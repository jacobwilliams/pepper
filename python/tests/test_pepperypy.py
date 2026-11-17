"""What PepperyPy adds to SpiceyPy's API: instances and contexts, queries
from several threads, prepared queries, GF searches with Python functions,
programmatic bodies and frames from Python, results of any size."""

import threading
from concurrent.futures import ThreadPoolExecutor

import numpy as np
import pytest

import pepperypy
from pepperypy import Spice
import spiceypy

from conftest import same

ET = 2.0e8


@pytest.fixture
def spice(kernels):
    """An instance of its own with the core kernels."""
    s = Spice()
    s.furnsh(kernels.CoreKernels.standardKernelList)
    yield s
    s.close()


# ---- contexts and threads


def test_threads_match_serial(spice):
    """Queries from 8 threads (each its own context) give the serial results
    to the last bit."""
    ets = np.linspace(1.0e8, 3.0e8, 400)
    ctx = spice.context()
    serial = [ctx.spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH') for et in ets]
    rot = [ctx.pxform('J2000', 'IAU_MARS', et) for et in ets]

    def work(k):
        c = spice.context()          # this thread's
        assert c is not ctx
        out = []
        for i in range(k, len(ets), 8):
            out.append((i, c.spkezr('MOON', ets[i], 'J2000', 'LT+S', 'EARTH'), c.pxform('J2000', 'IAU_MARS', ets[i])))
        return out

    with ThreadPoolExecutor(8) as pool:
        for chunk in pool.map(work, range(8)):
            for i, (state, lt), r in chunk:
                assert np.array_equal(state, serial[i][0]) and lt == serial[i][1]
                assert np.array_equal(r, rot[i])


def test_module_functions_use_thread_contexts(core):
    """The module-level functions query through the calling thread's context:
    threads may use them at the same time."""
    ets = np.linspace(1.0e8, 3.0e8, 200)
    serial = [pepperypy.spkpos('SUN', et, 'J2000', 'CN+S', 'EARTH')[0] for et in ets]
    contexts = []
    barrier = threading.Barrier(4)     # the four workers run at the same time

    def work(k):
        barrier.wait()
        contexts.append(pepperypy.current_context())
        return [(i, pepperypy.spkpos('SUN', ets[i], 'J2000', 'CN+S', 'EARTH')[0]) for i in range(k, len(ets), 4)]

    with ThreadPoolExecutor(4) as pool:
        for chunk in pool.map(work, range(4)):
            for i, pos in chunk:
                assert np.array_equal(pos, serial[i])
    assert len({id(c) for c in contexts}) == 4


def test_errors_stay_in_their_thread(spice):
    """An error in one thread's context does not affect another's."""
    barrier = threading.Barrier(2)
    results = {}

    def bad():
        c = spice.context()
        barrier.wait()
        with pytest.raises(pepperypy.SpiceyError):
            c.spkezr('NOBODY', ET, 'J2000', 'NONE', 'EARTH')
        results['bad'] = True

    def good():
        c = spice.context()
        barrier.wait()
        results['good'] = [c.spkezr('MOON', ET + i, 'J2000', 'NONE', 'EARTH')[1] for i in range(200)]

    ts = [threading.Thread(target=bad), threading.Thread(target=good)]
    for t in ts:
        t.start()
    for t in ts:
        t.join()
    assert results['bad'] and len(results['good']) == 200


def test_context_in_use(spice):
    """A context used by a thread while another is in it: ContextInUseError
    (from inside a GF callback, which runs while the search is in the
    context)."""
    ctx = spice.new_context()
    errors = []

    def udfuns(et):
        t = threading.Thread(target=lambda: errors.append(_try(lambda: ctx.bodn2c('EARTH'))))
        t.start()
        t.join()
        return ctx.spkpos('MOON', et, 'J2000', 'NONE', 'EARTH')[0][0]   # the same thread: allowed

    ctx.gf_user_scalar(udfuns, 'LOCMAX', 0.0, 0.0, 86400.0, [[ET, ET + 3 * 86400.0]])
    assert errors and all(isinstance(e, pepperypy.ContextInUseError) for e in errors)


def _try(f):
    try:
        f()
    except Exception as e:  # noqa: BLE001
        return e
    return None


def test_instances_are_independent(kernels):
    """Two instances hold different kernels; as_current switches the
    module-level functions."""
    a = Spice()
    b = Spice()
    a.furnsh(kernels.CoreKernels.standardKernelList)
    b.furnsh(kernels.CoreKernels.lsk)
    assert a.context().ktotal('ALL') == 4 and b.context().ktotal('ALL') == 1
    with pytest.raises(pepperypy.SpiceyError):
        b.context().spkezr('MOON', ET, 'J2000', 'NONE', 'EARTH')
    with a.as_current():
        assert pepperypy.ktotal('ALL') == 4
        with b.as_current():
            assert pepperypy.ktotal('ALL') == 1
        assert pepperypy.current_instance() is a
    a.close()
    b.close()
    with pytest.raises(ValueError):
        a.context()


def test_kernel_pool_block(kernels, core):
    """KernelPool: a temporary instance; the current one is unchanged."""
    n = pepperypy.ktotal('ALL')
    with pepperypy.KernelPool([kernels.CoreKernels.lsk]):
        assert pepperypy.ktotal('ALL') == 1
    assert pepperypy.ktotal('ALL') == n


def test_reload_after_kclear(kernels):
    """A context that read a file, then the kernels cleared and loaded
    again: the context reopens the file (a PEPPER fix found here)."""
    s = Spice()
    s.furnsh(kernels.CoreKernels.standardKernelList)
    c = s.context()
    lt = c.spkezr('MOON', ET, 'J2000', 'NONE', 'EARTH')[1]
    s.kclear()
    s.furnsh(kernels.CoreKernels.standardKernelList)
    assert c.spkezr('MOON', ET, 'J2000', 'NONE', 'EARTH')[1] == lt
    s.close()


# ---- prepared queries


def test_prepared_state(spice):
    ctx = spice.context()
    q = ctx.prepare_state('MARS BARYCENTER', 'EARTH', 'ECLIPJ2000', 'LT+S')
    ets = np.linspace(1.0e8, 3.0e8, 50)
    states, lts = q(ets)
    assert states.shape == (50, 6) and lts.shape == (50,)
    for k, et in enumerate(ets):
        s, lt = ctx.spkezr('MARS BARYCENTER', et, 'ECLIPJ2000', 'LT+S', 'EARTH')
        assert np.array_equal(states[k], s) and lts[k] == lt
    s1, lt1 = q(ets[3])
    assert np.array_equal(s1, states[3]) and lt1 == lts[3]


def test_prepared_rotation(spice):
    ctx = spice.context()
    r = ctx.prepare_rotation('J2000', 'IAU_EARTH')
    ets = np.linspace(1.0e8, 3.0e8, 20)
    rots = r(ets)
    xfs = r.transform(ets)
    assert rots.shape == (20, 3, 3) and xfs.shape == (20, 6, 6)
    for k, et in enumerate(ets):
        assert np.array_equal(rots[k], ctx.pxform('J2000', 'IAU_EARTH', et))
        assert np.array_equal(xfs[k], ctx.sxform('J2000', 'IAU_EARTH', et))
    assert np.array_equal(r(ets[0]), rots[0])


def test_vectorized_matches_scalar(spice):
    ctx = spice.context()
    ets = [1.0e8, 2.0e8]
    states, lts = ctx.spkezr('MOON', ets, 'J2000', 'NONE', 'EARTH')
    assert states.shape == (2, 6)
    assert np.array_equal(states[1], ctx.spkezr('MOON', ets[1], 'J2000', 'NONE', 'EARTH')[0])


# ---- results of any size


def test_any_size_results(spice):
    ctx = spice.context()
    radii = ctx.body_values_by_name('EARTH', 'RADII')
    assert radii.shape == (3,)
    cover = ctx.spk_coverage(spiceypy.tests.gettestkernels.CoreKernels.spk, 301)
    assert cover.ndim == 2 and cover.shape[1] == 2
    ids = ctx.spk_objects(spiceypy.tests.gettestkernels.CoreKernels.spk)
    assert 301 in ids and 399 in ids
    w = ctx.gf_distance('MOON', 'NONE', 'EARTH', 'LOCMAX', 0.0, 0.0, 5 * 86400.0,
                        [[cover[0, 0] + 86400, cover[0, 0] + 90 * 86400]])
    assert w.shape[1] == 2 and 2 <= w.shape[0] <= 5
    names = ctx.pool_names('BODY399*')
    assert 'BODY399_RADII' in names
    assert ctx.body_name(301) == 'MOON'


# ---- time strings below STR2ET


def test_tpartv(spice):
    """TPARTV: the components, type, modifiers and picture of a time string,
    and a diagnostic (not an exception) for one it cannot parse."""
    ctx = spice.context()
    tvec, ntvec, type_, modify, mods, yabbrv, succes, pictur, error = ctx.tpartv('2000 JAN 01 12:00:00 TDB')
    assert succes and error == '' and not yabbrv
    assert type_ == 'YMD' and ntvec == 6 and list(tvec) == [2000, 1, 1, 12, 0, 0]
    assert mods and modify == ['', '', '', '', 'TDB']   # ERA, WDAY, ZONE, AMPM, SYSTEM
    assert pictur.startswith('YYYY MON DD HR:MN:SC')
    tvec, ntvec, type_, *_ = pepperypy.tpartv('JD 2451545.0')
    assert type_ == 'JD' and ntvec == 1 and tvec[0] == 2451545.0
    tvec, ntvec, *_, succes, pictur, error = ctx.tpartv('nonsense')
    assert not succes and ntvec == 0 and len(tvec) == 0 and 'unrecognizable' in error


def test_ttrans(spice):
    """TTRANS: a short time vector is padded to MXCOMP; YMD to TDB is
    STR2ET's epoch; an unknown representation raises."""
    ctx = spice.context()
    tvec = ctx.tpartv('2000 JAN 01 12:00:00')[0]
    out = ctx.ttrans('YMD', 'TDB', tvec)
    assert out.shape == (10,) and out[0] == ctx.str2et('2000 JAN 01 12:00:00')
    assert list(ctx.ttrans('YMD', 'YD', [2000, 3, 1, 0, 0, 0])[:5]) == [2000, 61, 0, 0, 0]
    with pytest.raises(ValueError):
        ctx.ttrans('YMD', 'TDB', np.zeros(11))
    with pytest.raises(pepperypy.exceptions.SpiceyError):
        ctx.ttrans('XYZ', 'TDB', [1.0])


# ---- GF searches with Python functions


def test_gfuds_against_gfdist(core):
    """A user-defined scalar (the Moon's distance) finds the extrema GFDIST
    finds."""
    cnfine = pepperypy.window([[pepperypy.str2et('2007 JAN 1'), pepperypy.str2et('2007 APR 1')]])

    def udfuns(et):
        return pepperypy.vnorm(pepperypy.spkpos('MOON', et, 'J2000', 'NONE', 'EARTH')[0])

    def udqdec(f, et):
        return pepperypy.uddc(f, et, 10.0)

    a = pepperypy.gfuds(udfuns, udqdec, 'LOCMAX', 0.0, 0.0, 5 * 86400.0, 1000, cnfine)
    b = pepperypy.gfdist('MOON', 'NONE', 'EARTH', 'LOCMAX', 0.0, 0.0, 5 * 86400.0, 1000, cnfine)
    same(list(a), list(b), rtol=0, atol=1e-3)
    c = pepperypy.current_context().gf_user_scalar(udfuns, 'LOCMAX', 0.0, 0.0, 5 * 86400.0,
                                                   cnfine.to_intervals())   # default decreasing test
    same(c.reshape(-1), np.array(list(b)), rtol=0, atol=1e-3)


def test_gfudb(core):
    """A user-defined boolean: the Moon's latitude (J2000) positive."""
    et0, et1 = pepperypy.str2et('2007 JAN 1'), pepperypy.str2et('2007 FEB 1')

    def udfuns(et):
        return pepperypy.reclat(pepperypy.spkpos('MOON', et, 'J2000', 'NONE', 'EARTH')[0])[2]

    def udfunb(f, et):
        return f(et) > 0.0

    r = pepperypy.gfudb(udfuns, udfunb, 3600.0, pepperypy.window([[et0, et1]]))
    p = pepperypy.gfposc('MOON', 'J2000', 'NONE', 'EARTH', 'LATITUDINAL', 'LATITUDE', '>', 0.0, 0.0,
                         3600.0, 1000, pepperypy.window([[et0, et1]]))
    same(list(r), list(p), rtol=0, atol=1e-3)


def test_callback_exception(core):
    """An exception in a callback stops the search; it is the cause of the
    CallbackError raised."""
    def udfuns(et):
        raise ZeroDivisionError('boom')

    with pytest.raises(pepperypy.CallbackError) as e:
        pepperypy.gfuds(udfuns, lambda f, et: False, '=', 0.0, 0.0, 3600.0, 100,
                        pepperypy.window([[ET, ET + 86400.0]]))
    assert isinstance(e.value.__cause__, ZeroDivisionError)
    assert pepperypy.bodn2c('EARTH') == 399     # the context works again


# ---- programmatic bodies and frames


def test_python_ephemeris(spice):
    """A body whose states come from Python; light time corrections and
    frames apply to it as to any body."""
    v = np.array([1.0, 2.0, 3.0])

    def orbit(ctx, body, et):
        return np.concatenate([[7000.0, 0.0, 0.0] + v * (et - ET), v])

    spice.add_ephemeris(-999, 399, 'J2000', ET - 1e6, ET + 1e6, orbit)
    ctx = spice.context()
    state, lt = ctx.spkezr('-999', ET + 10.0, 'J2000', 'NONE', 'EARTH')
    assert np.allclose(state, [7010.0, 20.0, 30.0, 1.0, 2.0, 3.0])
    s2, lt2 = ctx.spkezr('-999', ET + 10.0, 'IAU_EARTH', 'LT+S', 'MOON')
    assert lt2 > 1.0
    # from several threads
    with ThreadPoolExecutor(4) as pool:
        out = list(pool.map(lambda et: spice.context().spkezr('-999', et, 'J2000', 'NONE', 'EARTH')[0],
                            [ET + k for k in range(40)]))
    assert np.allclose(out[5], [7005.0, 10.0, 15.0, 1.0, 2.0, 3.0])
    # outside the coverage, or after removal: no data
    with pytest.raises(pepperypy.SpiceyError):
        ctx.spkezr('-999', ET + 2e6, 'J2000', 'NONE', 'EARTH')
    spice.remove_ephemeris(-999)
    with pytest.raises(pepperypy.SpiceyError):
        ctx.spkezr('-999', ET, 'J2000', 'NONE', 'EARTH')


def test_provider_queries(spice):
    """A provider makes queries through the context it is given: the
    Earth-Moon midpoint."""
    def midpoint(ctx, body, et):
        return ctx.spkezr('MOON', et, 'J2000', 'NONE', 'EARTH')[0] / 2.0

    spice.add_ephemeris(-998, 399, 'J2000', ET - 1e7, ET + 1e7, midpoint)
    ctx = spice.context()
    moon = ctx.spkezr('MOON', ET, 'J2000', 'NONE', 'EARTH')[0]
    mid = ctx.spkezr('-998', ET, 'J2000', 'NONE', 'EARTH')[0]
    assert np.allclose(mid, moon / 2.0)
    q = ctx.prepare_state('-998', 'EARTH', 'J2000', 'NONE')
    assert np.allclose(q(ET)[0], moon / 2.0)


def test_python_frame(spice):
    """A rotating frame from Python: 1 degree per day about Z relative to
    J2000."""
    w = np.radians(1.0) / 86400.0

    def rotating(ctx, frame, et):
        a = w * (et - ET)
        c, s = np.cos(a), np.sin(a)
        r = np.array([[c, -s, 0.0], [s, c, 0.0], [0.0, 0.0, 1.0]])     # frame -> J2000
        dr = w * np.array([[-s, -c, 0.0], [c, -s, 0.0], [0.0, 0.0, 0.0]])
        x = np.zeros((6, 6))
        x[:3, :3] = r
        x[3:, 3:] = r
        x[3:, :3] = dr
        return x, 'J2000'

    spice.add_frame('PY_ROTATING', 1400099, 399, rotating)
    ctx = spice.context()
    r = ctx.pxform('PY_ROTATING', 'J2000', ET + 86400.0)
    assert np.allclose(r, spiceypy.rotate(-np.radians(1.0), 3))
    assert ctx.namfrm('PY_ROTATING') == 1400099
    x = ctx.sxform('J2000', 'PY_ROTATING', ET)
    assert np.allclose(x[:3, :3], np.eye(3))
    s = ctx.spkezr('MOON', ET, 'PY_ROTATING', 'NONE', 'EARTH')[0]
    assert np.allclose(s[:3], ctx.spkezr('MOON', ET, 'J2000', 'NONE', 'EARTH')[0][:3])
    spice.kclear()
    with pytest.raises(pepperypy.SpiceyError):
        ctx.namfrm('PY_ROTATING') or ctx.pxform('PY_ROTATING', 'J2000', ET)


def test_provider_exception(spice):
    def broken(ctx, body, et):
        raise RuntimeError('no orbit today')

    spice.add_ephemeris(-997, 399, 'J2000', ET - 1e6, ET + 1e6, broken)
    with pytest.raises(pepperypy.CallbackError) as e:
        spice.context().spkezr('-997', ET, 'J2000', 'NONE', 'EARTH')
    assert isinstance(e.value.__cause__, RuntimeError)
