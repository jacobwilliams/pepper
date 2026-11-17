"""PepperyPy's vectorized functions (<name>_v, <name>_s and the dispatching
<name>) against SpiceyPy's cyice (CSPICE): code written for cyice runs with
`import pepperypy as cyice`. The C loops give the scalar functions' results,
element by element, and cyice's to the last bits; they run in parallel in
threads."""

import threading

import numpy as np
import pytest

import pepperypy
pc = pepperypy   # cyice's names are PepperyPy's

spiceypy = pytest.importorskip('spiceypy')
from spiceypy import cyice as sc  # noqa: E402


@pytest.fixture
def ets(core):
    et0 = pepperypy.str2et('2001 JAN 1')
    return et0 + np.linspace(0.0, 86400.0 * 300, 257)


def test_states_and_rotations(ets):
    for name, args in [('spkezr', ('MOON', ets, 'J2000', 'LT+S', 'EARTH')),
                       ('spkpos', ('MOON', ets, 'J2000', 'LT+S', 'EARTH')),
                       ('spkgeo', (301, ets, 'J2000', 399)),
                       ('pxform', ('J2000', 'IAU_MOON', ets)),
                       ('sxform', ('J2000', 'IAU_MOON', ets))]:
        pv = getattr(pc, name + '_v')(*args)
        sv = getattr(sc, name + '_v')(*args)
        for a, b in zip(pv if isinstance(pv, tuple) else (pv,), sv if isinstance(sv, tuple) else (sv,)):
            assert np.shape(a) == np.shape(b), name
            np.testing.assert_allclose(a, b, rtol=1e-11, atol=1e-8, err_msg=name)
        # each element is the scalar function's result, to the last bit
        k = 100
        one = getattr(pepperypy, name)(*[a[k] if a is ets else a for a in args])
        for a, b in zip(pv if isinstance(pv, tuple) else (pv,), one if isinstance(one, tuple) else (one,)):
            assert np.array_equal(a[k], b), name


def test_strings_and_time(ets):
    utc = pc.et2utc_v(ets, 'ISOC', 3)
    assert list(utc) == list(sc.et2utc_v(ets, 'ISOC', 3))
    assert np.array_equal(pc.str2et_v(utc), sc.str2et_v(utc))
    assert np.array_equal(pc.utc2et_v(utc), [pepperypy.utc2et(u) for u in utc])
    assert list(pc.timout_v(ets, 'YYYY-DOY')) == list(sc.timout_v(ets, 'YYYY-DOY'))
    # the dispatchers: a scalar to _s, an array to _v
    assert isinstance(pc.str2et(utc[0]), float) and pc.str2et(utc).shape == ets.shape
    assert pc.str2et_s(utc[0]) == pc.str2et(np.array(utc[0])) == pc.str2et(utc[0].encode())
    assert pc.spkezr('MOON', ets, 'J2000', 'NONE', 'EARTH')[0].shape == (len(ets), 6)
    assert pc.reclat([1.0, 0.0, 0.0])[0] == 1.0 and pc.reclat(np.eye(3)).shape == (3, 3)


def test_coordinates_broadcast():
    rect = np.random.default_rng(3).normal(size=(50, 3)) * 7000
    lat = pc.reclat_v(rect)
    assert lat.shape == (50, 3)
    np.testing.assert_allclose(lat, sc.reclat_v(rect), rtol=1e-14)
    back = pc.latrec_v(lat[:, 0], lat[:, 1], lat[:, 2])
    np.testing.assert_allclose(back, rect, rtol=1e-12, atol=1e-9)
    # a single value is used for every element
    ones = pc.latrec_v(1.0, lat[:, 1], lat[:, 2])
    np.testing.assert_allclose(np.linalg.norm(ones, axis=1), 1.0)
    with pytest.raises(ValueError):
        pc.latrec_v(np.ones(3), np.ones(4), np.ones(3))


def test_found_flags(ets):
    with pepperypy.no_found_check():
        spoint, trgepc, srfvec, found = pc.sincpt_v('Ellipsoid', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH',
                                                     'J2000', [0.0, 0.0, 1.0])
    assert found.dtype == bool and spoint.shape == (len(ets), 3)
    if not found.all():
        with pytest.raises(pepperypy.NotFoundError):
            pc.sincpt_v('Ellipsoid', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH', 'J2000', [0.0, 0.0, 1.0])


def test_error_stops_the_loop(ets):
    with pytest.raises(pepperypy.SpiceyError):
        pc.spkpos_v('NO SUCH BODY', ets, 'J2000', 'NONE', 'EARTH')
    pepperypy.reset()
    assert pc.spkpos_v('MOON', ets, 'J2000', 'NONE', 'EARTH')[0].shape == (len(ets), 3)


def test_threads(ets):
    expected = pc.subpnt_v('NEAR POINT/ELLIPSOID', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH')
    results = [None] * 4

    def work(i):
        results[i] = pc.subpnt_v('NEAR POINT/ELLIPSOID', 'MOON', ets, 'IAU_MOON', 'LT+S', 'EARTH')

    threads = [threading.Thread(target=work, args=(i,)) for i in range(4)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    for r in results:
        for a, b in zip(r, expected):
            assert np.array_equal(a, b)
