"""PepperyPy's vectorized functions beyond SpiceyPy's cyice (propagation,
frames, aberration-corrected states, SCLK, ellipsoid geometry, Jacobians,
rotations, matrices): each element of `<name>_v` is `<name>_s`'s result to
the last bit, and `<name>` dispatches to either by its arguments."""

import numpy as np
import pytest

import pepperypy as p

N = 7
RNG = np.random.default_rng(5)
A, B, C = 3.0, 2.0, 1.0     # an ellipsoid's semi-axes
RE, F = 3396.19, 0.00589    # Mars


def check(name, args, looped):
    """`name_v(*args)` against `name_s` at each element (the arguments at
    the positions `looped` are arrays of N elements), and the dispatch."""
    v = getattr(p, name + '_v')(*args)
    vt = v if isinstance(v, tuple) else (v,)
    for i in range(N):
        one = getattr(p, name + '_s')(*[a[i] if k in looped else a for k, a in enumerate(args)])
        ot = one if isinstance(one, tuple) else (one,)
        assert len(vt) == len(ot), name
        for j, (x, y) in enumerate(zip(vt, ot)):
            assert np.shape(x)[0] == N, (name, j)
            assert np.array_equal(x[i], y), (name, i, j, x[i], y)
    d = getattr(p, name)(*args)
    for x, y in zip(d if isinstance(d, tuple) else (d,), vt):
        assert np.array_equal(x, y), name
    return v


def vectors(k=3, scale=1.0):
    return RNG.normal(size=(N, k)) * scale


@pytest.fixture
def ets(core):
    return p.str2et('2001 JAN 1') + np.linspace(0.0, 86400.0 * 200, N)


def test_propagation(core):
    pv = np.column_stack([7000.0 + vectors(3, 100.0)[:, 0], vectors(2, 100.0), vectors(3, 0.1) + [0, 7.5, 0.5]])
    check('prop2b', [398600.4418, pv, np.linspace(-3000.0, 3000.0, N)], {1, 2})
    # one state at several times, and several states at one time (broadcast)
    assert p.prop2b_v(398600.4418, pv[0], np.linspace(0, 100, N)).shape == (N, 6)
    assert np.array_equal(p.prop2b_v(398600.4418, pv, 60.0)[3], p.prop2b(398600.4418, pv[3], 60.0))
    # equinoctial elements
    gm, ecc, pp = 398600.436, 0.1, 10000.0
    a = pp / (1.0 - ecc)
    argp, node, inc, m0 = (x * p.rpd() for x in (30.0, 15.0, 10.0, 45.0))
    eqel = [a, ecc * np.sin(argp + node), ecc * np.cos(argp + node), m0 + argp + node,
            np.tan(inc / 2) * np.sin(node), np.tan(inc / 2) * np.cos(node), 0.0, np.sqrt(gm / a) / a, 0.0]
    t0 = -1.0e8
    check('eqncpv', [t0 + np.linspace(0.0, 1.0e5, N), t0, eqel, -p.halfpi(), p.halfpi()], {0})


def test_deep_space_sgp4(core):
    geophs = [1.082616e-3, -2.53881e-6, -1.65597e-6, 7.43669161e-2, 120.0, 78.0, 6378.135, 1.0]
    tle = ['1 26038U 00004A   06120.80873521 -.00000277  00000-0  10000-3 0  9998',
           '2 26038   0.0322 274.4469 0003033 168.1009 196.5432  1.00271186 22916']
    epoch, elems = p.getelm(1957, tle)
    check('dpspce', [epoch + np.linspace(0.0, 86400.0, N), geophs, elems], {0})


@pytest.mark.parametrize('tle', [
    # geosynchronous (one-day resonance)
    ['1 26038U 00004A   06120.80873521 -.00000277  00000-0  10000-3 0  9998',
     '2 26038   0.0322 274.4469 0003033 168.1009 196.5432  1.00271186 22916'],
    # 11801: deep space without resonance (zzdpsec leaves the mean motion
    # unchanged; it was intent(out), so optimized builds returned NaN)
    ['1 11801U          80230.29629788  .01431103  00000-0  14311-1      13',
     '2 11801  46.7916 230.4354 7318036  47.4722  10.4117  2.28537848    13']], ids=['26038', '11801'])
def test_dpspce_against_cspice(core, tle):
    """DPSPCE against CSPICE's (its f2c translation, dpspce_, in SpiceyPy's
    library: SpiceyPy has no wrapper)."""
    import ctypes
    from spiceypy.utils.libspicehelper import libspice
    geophs = np.array([1.082616e-3, -2.53881e-6, -1.65597e-6, 7.43669161e-2, 120.0, 78.0, 6378.135, 1.0])
    epoch, elems = p.getelm(1957, tle)
    dp = ctypes.POINTER(ctypes.c_double)
    for dt in (0.0, 600.0, 86400.0, -86400.0 * 3):
        et = ctypes.c_double(epoch + dt)
        expected = np.zeros(6)
        libspice.dpspce_(ctypes.byref(et), geophs.ctypes.data_as(dp), np.asarray(elems).ctypes.data_as(dp),
                         expected.ctypes.data_as(dp))
        np.testing.assert_allclose(p.dpspce(epoch + dt, geophs, elems), expected, rtol=1e-9, atol=1e-6)


def test_frames(ets):
    check('tipbod', ['J2000', 399, ets], {2})
    check('tisbod', ['J2000', 301, ets], {2})
    check('bodeul', [399, ets], {1})
    check('pxfrm2', ['IAU_EARTH', 'IAU_MOON', ets, ets + 3600.0], {2, 3})


def test_ck_and_sclk(cassini):
    et = p.str2et('2013-FEB-26 00:01:08.828')
    ets = et + np.linspace(0.0, 0.5, N)
    check('ckfrot', [-82000, ets], {1})
    check('ckfxfm', [-82000, ets], {1})
    check('sce2t', [-82, ets], {1})


def test_aberration_corrected_states(ets):
    stobs = p.spkssb_v(399, ets, 'J2000')
    check('spkltc', [301, ets, 'J2000', 'LT+S', stobs], {1, 4})
    check('spkacs', [301, ets, 'J2000', 'LT+S', 399], {1})
    check('spkaps', [301, ets, 'J2000', 'LT+S', stobs, vectors(3, 1e-6)], {1, 4, 5})
    check('spkapp', [301, ets, 'J2000', stobs, 'LT+S'], {1, 3})
    check('stelab', [vectors(3, 1e5), vectors(3, 30.0)], {0, 1})
    check('stlabx', [vectors(3, 1e5), vectors(3, 30.0)], {0, 1})


def test_ellipsoid_geometry(core):
    pos = vectors(3, 10.0)
    check('nearpt', [pos, A, B, C], {0})
    # rays from outside toward the center: all hit
    check('surfpt', [pos, -pos, A, B, C], {0, 1})
    surface = check('edpnt', [pos, A, B, C], {0})
    check('surfnm', [A, B, C, surface], {3})
    check('npedln', [A, B, C, pos, vectors()], {3, 4})
    # a ray that misses: NotFoundError, or the flags with no_found_check
    away = pos.copy()
    with pytest.raises(p.NotFoundError):
        p.surfpt_v(away, away, A, B, C)
    with p.no_found_check():
        found = p.surfpt_v(away, away, A, B, C)[-1]
    assert found.dtype == bool and not found.any()


def test_jacobians(core):
    r, lon, lat = RNG.uniform(1, 10, N), RNG.uniform(-3, 3, N), RNG.uniform(-1.5, 1.5, N)
    alt = RNG.uniform(0, 100, N)
    check('drdlat', [r, lon, lat], {0, 1, 2})
    check('drdcyl', [r, lon, lat], {0, 1, 2})
    check('drdsph', [r, lat + 1.6, lon], {0, 1, 2})
    check('drdgeo', [lon, lat, alt, RE, F], {0, 1, 2})
    check('drdpgr', ['MARS', lon + 3.2, lat, alt, RE, F], {1, 2, 3})
    check('drdazl', [r, lon, lat, False, True], {0, 1, 2})
    x, y, z = (vectors(3, 3000.0) + 100.0).T
    check('dlatdr', [x, y, z], {0, 1, 2})
    check('dcyldr', [x, y, z], {0, 1, 2})
    check('dsphdr', [x, y, z], {0, 1, 2})
    check('dgeodr', [x, y, z, RE, F], {0, 1, 2})
    check('dpgrdr', ['MARS', x, y, z, RE, F], {1, 2, 3})
    check('dazldr', [x, y, z, False, True], {0, 1, 2})
    # one radius for every point
    assert np.array_equal(p.drdlat_v(2.0, lon, lat)[4], p.drdlat(2.0, lon[4], lat[4]))


def test_rotations_and_matrices(core):
    ang = RNG.uniform(-3, 3, (3, N))
    axes = vectors()
    check('axisar', [axes, ang[0]], {0, 1})
    check('rotate', [ang[0], 3], {0})
    check('rotvec', [vectors(), ang[0], 1], {0, 1})
    r = check('eul2m', [ang[0], ang[1], ang[2], 3, 1, 3], {0, 1, 2})
    xf = check('eul2xf', [np.column_stack([ang.T, vectors(3, 1e-3)]), 3, 1, 3], {0})
    # over matrices: (n, 3, 3), (n, 6, 6)
    check('m2eul', [r, 3, 1, 3], {0})
    check('xf2eul', [xf, 3, 1, 3], {0})
    q = check('m2q', [r], {0})
    check('q2m', [q], {0})
    rot, av = check('xf2rav', [xf], {0})
    check('rav2xf', [rot, av], {0, 1})
    check('raxisa', [r], {0})
    check('invstm', [xf], {0})
    # one matrix (an array, or lists) is the scalar form
    assert np.array_equal(p.m2q(r[2]), p.m2q_s(r[2]))
    assert np.array_equal(p.m2q(r[2].tolist()), p.m2q_s(r[2]))
    assert p.m2q(r.tolist()).shape == (N, 4)
    with pytest.raises(ValueError):
        p.m2q_v(np.zeros((N, 3, 4)))
