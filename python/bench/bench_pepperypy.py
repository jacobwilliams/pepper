"""PepperyPy against SpiceyPy: the cost of a call from Python (one at a
time, vectorized, prepared queries in batches) and queries from threads;
then PepperyPy's vectorized functions (<name>_v) against SpiceyPy's cyice.

    pixi run -e python py-bench

The kernels are SpiceyPy's test kernels (DE405 subset, LSK, PCK), loaded in
both; PepperyPy reads the SPK record by record (as SpiceyPy does) and, in a
second instance, from memory. Prints a Markdown table (µs per epoch).
"""

import os
import sys
import time
from concurrent.futures import ThreadPoolExecutor

import numpy as np

import pepperypy
import spiceypy
from spiceypy.tests import gettestkernels as gtk

try:
    from spiceypy import cyice
except ImportError:     # SpiceyPy before 8
    cyice = None


def best(f, n, repeat=5):
    """The best time per item of f() (which handles n items), in µs."""
    t = []
    for _ in range(repeat):
        t0 = time.perf_counter()
        f()
        t.append(time.perf_counter() - t0)
    return min(t) / n * 1e6


def main():
    gtk.download_kernels()
    kernels = gtk.CoreKernels.standardKernelList
    spiceypy.furnsh(kernels)
    pepperypy.furnsh(kernels)
    mem = pepperypy.Spice()
    mem.furnsh(kernels, in_memory=True)

    rng = np.random.default_rng(1)
    n = 20000
    ets = np.sort(rng.uniform(1.0e8, 3.0e8, n))
    ctx = pepperypy.current_context()
    mctx = mem.context()

    rows = []

    def row(name, spy, ppy, pmem=None):
        rows.append((name, spy, ppy, pmem))

    # one call at a time, from Python
    row('spkezr Moon from Earth, J2000, NONE (one call per epoch)',
        best(lambda: [spiceypy.spkezr('MOON', e, 'J2000', 'NONE', 'EARTH') for e in ets], n),
        best(lambda: [pepperypy.spkezr('MOON', e, 'J2000', 'NONE', 'EARTH') for e in ets], n),
        best(lambda: [mctx.spkezr('MOON', e, 'J2000', 'NONE', 'EARTH') for e in ets], n))
    row('spkezr Mars barycenter from Earth, ECLIPJ2000, LT+S (one call per epoch)',
        best(lambda: [spiceypy.spkezr('MARS BARYCENTER', e, 'ECLIPJ2000', 'LT+S', 'EARTH') for e in ets], n),
        best(lambda: [pepperypy.spkezr('MARS BARYCENTER', e, 'ECLIPJ2000', 'LT+S', 'EARTH') for e in ets], n),
        best(lambda: [mctx.spkezr('MARS BARYCENTER', e, 'ECLIPJ2000', 'LT+S', 'EARTH') for e in ets], n))
    row('pxform J2000 to IAU_EARTH (one call per epoch)',
        best(lambda: [spiceypy.pxform('J2000', 'IAU_EARTH', e) for e in ets], n),
        best(lambda: [pepperypy.pxform('J2000', 'IAU_EARTH', e) for e in ets], n))
    row('str2et (one call)',
        best(lambda: [spiceypy.str2et('2026 NOV 8 12:00:00') for _ in range(2000)], 2000),
        best(lambda: [pepperypy.str2et('2026 NOV 8 12:00:00') for _ in range(2000)], 2000))
    row('vnorm (one call)',
        best(lambda: [spiceypy.vnorm([1.0, 2.0, 3.0]) for _ in range(20000)], 20000),
        best(lambda: [pepperypy.vnorm([1.0, 2.0, 3.0]) for _ in range(20000)], 20000))

    # vectorized: SpiceyPy loops in Python
    row('spkezr Moon from Earth, J2000, NONE (an array of epochs)',
        best(lambda: spiceypy.spkezr('MOON', ets, 'J2000', 'NONE', 'EARTH'), n),
        best(lambda: pepperypy.spkezr('MOON', ets, 'J2000', 'NONE', 'EARTH'), n),
        best(lambda: mctx.spkezr('MOON', ets, 'J2000', 'NONE', 'EARTH'), n))

    # prepared queries, one call for all the epochs
    q = ctx.prepare_state('MOON', 'EARTH', 'J2000', 'NONE')
    mq = mctx.prepare_state('MOON', 'EARTH', 'J2000', 'NONE')
    row('Moon from Earth, J2000, NONE: prepared query, a batch (PepperyPy only)', None,
        best(lambda: q(ets), n), best(lambda: mq(ets), n))
    q2 = ctx.prepare_state('MARS BARYCENTER', 'EARTH', 'ECLIPJ2000', 'LT+S')
    mq2 = mctx.prepare_state('MARS BARYCENTER', 'EARTH', 'ECLIPJ2000', 'LT+S')
    row('Mars barycenter from Earth, ECLIPJ2000, LT+S: prepared query, a batch (PepperyPy only)', None,
        best(lambda: q2(ets), n), best(lambda: mq2(ets), n))
    r = ctx.prepare_rotation('J2000', 'IAU_EARTH')
    row('J2000 to IAU_EARTH: prepared rotation, a batch (PepperyPy only)', None, best(lambda: r(ets), n))

    # threads: the batch split over threads, each with its own context
    ncpu = os.cpu_count() or 4
    for nt in (1, 2, 4, min(8, ncpu)):
        chunks = np.array_split(ets, nt)

        def work(chunk):
            c = mem.context()
            c.prepare_state('MARS BARYCENTER', 'EARTH', 'ECLIPJ2000', 'LT+S')(chunk)

        def one_call_each(chunk):
            c = mem.context()
            for e in chunk:
                c.spkezr('MARS BARYCENTER', e, 'ECLIPJ2000', 'LT+S', 'EARTH')

        with ThreadPoolExecutor(nt) as pool:
            tb = best(lambda: list(pool.map(work, chunks)), n)
            to = best(lambda: list(pool.map(one_call_each, chunks)), n)
        row(f'Mars LT+S from memory, {nt} thread(s): prepared batches / one call per epoch (PepperyPy only)',
            None, to, tb)

    gil = getattr(sys, '_is_gil_enabled', lambda: True)()
    print(f'PepperyPy {pepperypy.__version__}, SpiceyPy {spiceypy.__version__} ({spiceypy.tkvrsn("TOOLKIT")}), '
          f'Python {sys.version.split()[0]}{"" if gil else " (free-threaded)"}, {n} random epochs, '
          f'{ncpu} CPUs\n')
    print('| Operation | SpiceyPy (µs) | PepperyPy (µs) | PepperyPy, kernels in memory (µs) | Speedup |')
    print('|---|--:|--:|--:|--:|')
    for name, spy, ppy, pmem in rows:
        fmt = lambda x: '–' if x is None else f'{x:.3g}'
        fastest = min(x for x in (ppy, pmem) if x is not None)
        speed = '–' if spy is None else f'{spy / fastest:.1f}×'
        print(f'| {name} | {fmt(spy)} | {fmt(ppy)} | {fmt(pmem)} | {speed} |')
    if cyice is not None:
        vectorized(ets)


def vectorized(ets):
    """SpiceyPy's cyice against PepperyPy's <name>_v (C loops with the GIL
    released), the same arguments; and PepperyPy's in threads."""
    n = len(ets)
    utc = cyice.et2utc_v(ets, 'ISOC', 3)
    rect = np.random.default_rng(2).normal(size=(n, 3)) * 7000.0
    lat = [np.ascontiguousarray(c) for c in cyice.reclat_v(rect).T]   # (cyice: contiguous arrays)
    cases = [
        ('spkezr_v', ('MOON', ets, 'J2000', 'LT+S', 'EARTH')),
        ('spkpos_v', ('MOON', ets, 'J2000', 'LT+S', 'EARTH')),
        ('spkgeo_v', (301, ets, 'J2000', 399)),
        ('spkssb_v', (499, ets, 'J2000')),
        ('pxform_v', ('J2000', 'IAU_EARTH', ets)),
        ('sxform_v', ('J2000', 'IAU_EARTH', ets)),
        ('subpnt_v', ('NEAR POINT/ELLIPSOID', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH')),
        ('subslr_v', ('NEAR POINT/ELLIPSOID', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH')),
        ('et2utc_v', (ets, 'ISOC', 3)),
        ('timout_v', (ets, 'YYYY-MON-DD HR:MN:SC.### ::TDB')),
        ('str2et_v', (utc,)),
        ('utc2et_v', (utc,)),
        ('deltet_v', (ets, 'ET')),
        ('reclat_v', (rect,)),
        ('latrec_v', tuple(lat)),
        ('recgeo_v', (rect, 6378.1366, 0.00335)),
    ]
    print(f'\nVectorized, {n} epochs or points: µs per element\n')
    print("| Function | SpiceyPy's cyice (µs) | PepperyPy (µs) | Speedup |")
    print('|---|--:|--:|--:|')
    for name, args in cases:
        f_s, f_p = getattr(cyice, name, None), getattr(pepperypy, name)
        if f_s is None:
            continue
        ts, tp = best(lambda: f_s(*args), n), best(lambda: f_p(*args), n)
        print(f'| `{name}` | {ts:.3g} | {tp:.3g} | {ts / tp:.1f}× |')

    # PepperyPy's in threads (cyice cannot: CSPICE is not thread-safe)
    args = ('NEAR POINT/ELLIPSOID', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH')
    one = best(lambda: pepperypy.subpnt_v(*args), n)
    print(f'\n`subpnt_v` split over threads (PepperyPy only), µs per element\n')
    print('| Threads | PepperyPy (µs) | Speedup |')
    print('|--:|--:|--:|')
    for nt in (1, 2, 4, min(8, os.cpu_count() or 4)):
        chunks = np.array_split(ets, nt)

        def work(chunk):
            pepperypy.subpnt_v('NEAR POINT/ELLIPSOID', 'MOON', chunk, 'IAU_MOON', 'LT+S', 'EARTH')

        with ThreadPoolExecutor(nt) as pool:
            t = best(lambda: list(pool.map(work, chunks)), n)
        print(f'| {nt} | {t:.3g} | {one / t:.1f}× |')


if __name__ == '__main__':
    main()
