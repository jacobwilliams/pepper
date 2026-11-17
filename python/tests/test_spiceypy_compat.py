"""PepperyPy as a replacement of SpiceyPy: the functions, types and checks
added for it (guides/spiceypy_porting.md), compared with SpiceyPy where it
has the same function."""

import os
import tempfile

import numpy as np
import pytest

import pepperypy
import spiceypy
from pepperypy.exceptions import SpiceyError


# ---- planes and ellipses (SpiceyPy's Plane and Ellipse)


def test_planes_and_ellipses():
    for m in (pepperypy, spiceypy):
        m.reset()
    normal, point = [1.0, 2.0, 3.0], [4.0, -1.0, 2.0]
    pp = pepperypy.nvp2pl(normal, point)
    sp = spiceypy.nvp2pl(normal, point)
    assert isinstance(pp, pepperypy.Plane)
    assert np.allclose(pp.normal, sp.normal, atol=1e-15) and pp.constant == pytest.approx(sp.constant)
    n, c = pepperypy.pl2nvc(pp)
    assert np.allclose(n, spiceypy.pl2nvc(sp)[0]) and c == pytest.approx(spiceypy.pl2nvc(sp)[1])
    # an ellipse: the intersection of a plane and an ellipsoid
    plane = pepperypy.nvc2pl([0.0, 0.0, 1.0], 0.5)
    ell = pepperypy.inedpl(1.0, 2.0, 3.0, plane)
    sell = spiceypy.inedpl(1.0, 2.0, 3.0, spiceypy.nvc2pl([0.0, 0.0, 1.0], 0.5))
    assert isinstance(ell, pepperypy.Ellipse)
    for a, b in ((ell.center, sell.center), (ell.semi_major, sell.semi_major), (ell.semi_minor, sell.semi_minor)):
        assert np.allclose(a, b, atol=1e-15)
    # the ellipse back to vectors, and to an ellipse again
    center, smajor, sminor = pepperypy.el2cgv(ell)
    again = pepperypy.cgv2el(center, smajor, sminor)
    assert np.allclose(np.asarray(again), np.asarray(ell))
    # a plain array is a plane too
    assert np.allclose(pepperypy.vprjp([1.0, 1.0, 1.0], np.asarray(plane)),
                       spiceypy.vprjp([1.0, 1.0, 1.0], spiceypy.nvc2pl([0.0, 0.0, 1.0], 0.5)))


# ---- CSPICE's checks of string arguments


def test_empty_strings():
    with pytest.raises(SpiceyError) as e:
        pepperypy.unload('')
    assert e.value.short == 'SPICE(EMPTYSTRING)'
    with pytest.raises(SpiceyError):
        pepperypy.bodn2c('')
    with pytest.raises(SpiceyError):
        pepperypy.furnsh('')          # a hand-written function
    with pytest.raises(SpiceyError):
        pepperypy.gdpool('', 0, 1)    # a hand-written one with SpiceyPy's signature
    # a blank string is not empty; and erract checks its action only for SET
    with pytest.raises(SpiceyError) as e:
        pepperypy.bodn2c(' ')
    assert getattr(e.value, 'short', '') != 'SPICE(EMPTYSTRING)'
    assert pepperypy.erract('GET', 10, '')


# ---- GF searches with Python callbacks (gfocce, gfevnt), uddf


def _occultation_search(**hooks):
    et0 = pepperypy.str2et('2001 DEC 01 00:00:00 TDB')
    et1 = pepperypy.str2et('2002 JAN 01 00:00:00 TDB')
    cnfine = pepperypy.cell_double(2)
    pepperypy.wninsd(et0, et1, cnfine)
    result = pepperypy.cell_double(1000)
    pepperypy.gfsstp(20.0)
    args = dict(udstep=pepperypy.gfstep, udrefn=pepperypy.gfrefn, rpt=False, udrepi=pepperypy.gfrepi,
                udrepu=pepperypy.gfrepu, udrepf=pepperypy.gfrepf, bail=False, udbail=pepperypy.gfbail)
    args.update(hooks)
    pepperypy.gfocce('Any', 'moon', 'ellipsoid', 'iau_moon', 'sun', 'ellipsoid', 'iau_sun', 'lt', 'earth',
                     1.0e-6, args['udstep'], args['udrefn'], args['rpt'], args['udrepi'], args['udrepu'],
                     args['udrepf'], args['bail'], args['udbail'], cnfine, result)
    return [pepperypy.wnfetd(result, i) for i in range(pepperypy.wncard(result))]


def test_gf_python_callbacks(core):
    default = _occultation_search()
    assert len(default) == 1
    calls = {'step': 0, 'refn': 0, 'repi': 0, 'repu': 0, 'repf': 0, 'bail': 0}

    def step(t):
        calls['step'] += 1
        return 20.0

    def refn(t1, t2, s1, s2):
        calls['refn'] += 1
        return 0.5 * (t1 + t2)

    def repi(window, begmss, endmss):
        calls['repi'] += 1
        assert pepperypy.wncard(window) == 1

    def repu(ivbeg, ivend, time):
        calls['repu'] += 1

    def repf():
        calls['repf'] += 1

    def bail():
        calls['bail'] += 1
        return False

    found = _occultation_search(udstep=step, udrefn=refn, rpt=True, udrepi=repi, udrepu=repu, udrepf=repf,
                                bail=True, udbail=bail)
    assert np.allclose(found, default, atol=1e-5)
    assert all(calls[k] > 0 for k in calls), calls
    # the hooks are cleared after the search: PEPPER's routines again
    assert np.allclose(_occultation_search(), default)


def test_gf_callback_error(core):
    def step(t):
        raise RuntimeError('no step')
    with pytest.raises(SpiceyError):
        _occultation_search(udstep=step)
    pepperypy.reset()
    assert len(_occultation_search()) == 1


def test_gf_interrupt(core):
    pepperypy.gfclrh()
    assert not pepperypy.gfbail()
    with pytest.raises(SpiceyError):
        pepperypy.gfinth(0)
    pepperypy.reset()
    pepperypy.gfinth(2)
    assert pepperypy.gfbail()
    try:
        # an interrupted search stops at once (with or without an error)
        try:
            found = _occultation_search(bail=True)
        except SpiceyError:
            pepperypy.reset()
            found = []
        assert found == []
    finally:
        pepperypy.gfclrh()
    assert not pepperypy.gfbail()


def test_uddf(core):
    et = pepperypy.str2et('JAN 1 2009')

    def lt(t):
        return pepperypy.spkpos('MERCURY', t, 'J2000', 'LT+S', 'MOON')[1]

    def slt(t):
        return spiceypy.spkpos('MERCURY', t, 'J2000', 'LT+S', 'MOON')[1]

    d = pepperypy.uddf(lt, et, 1.0)
    assert d == pytest.approx(spiceypy.uddf(spiceypy.utils.callbacks.SpiceUDFUNS(slt), et, 1.0), rel=1e-9)


# ---- results of any size, EK fast load


def test_limbpt_all_points(core):
    npts, points, epochs, tangts = pepperypy.limbpt(
        'TANGENT/ELLIPSOID', 'MOON', 0.0, 'IAU_MOON', 'NONE', 'CENTER', 'EARTH', [0.0, 0.0, 1.0],
        np.pi / 3, 6, 1.0e-4, 1.0e-7, 10000)
    assert len(npts) == 6 and points.shape == (int(np.sum(npts)), 3)
    assert epochs.shape == (int(np.sum(npts)),) and tangts.shape == points.shape


def test_ek_fast_load_short_arrays():
    # SpiceyPy's test of EKDELR passes per-row arrays shorter than the
    # segment's rows; PepperyPy pads them (SPICELIB reads them for each row)
    d = tempfile.mkdtemp()
    ek = os.path.join(d, 'short.ek')
    try:
        for _ in range(20):
            handle = pepperypy.ekopn(ek, ek, 0)
            segno, rcptrs = pepperypy.ekifld(handle, 'test_table', 1, 10, 200, ['c1'], 200,
                                             ['DATATYPE = INTEGER, NULLS_OK = TRUE'])
            pepperypy.ekacli(handle, segno, 'c1', [1, 2], [1], [False, False], rcptrs, [1])
            pepperypy.ekffld(handle, segno, rcptrs)
            pepperypy.ekdelr(handle, segno, 2)
            pepperypy.ekcls(handle)
            os.remove(ek)
    finally:
        pepperypy.kclear()


# ---- the structures of SpiceyPy's support_types


def test_descriptor_types():
    d = pepperypy.SpiceDLADescr(list(range(1, 9)))
    assert (d.bwdptr, d.fwdptr, d.csize) == (1, 2, 8)
    assert np.asarray(d).dtype == np.int32
    k = pepperypy.SpiceDSKDescr(np.arange(24.0))
    assert k.surfce == 0 and k.stop == 23.0 and list(k.corpar) == list(range(6, 16))
    a = pepperypy.SpiceEKAttDsc._from_array([1, 3, -1, 1, -1, 1])
    assert (a.dtype, a.indexd, a.nullok) == (2, False, True)
