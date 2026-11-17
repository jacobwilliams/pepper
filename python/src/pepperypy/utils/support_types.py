"""SpiceyPy's cell types (``spiceypy.utils.support_types``), without ctypes."""
from ..cells import (SpiceCell, SPICEDOUBLE_CELL, SPICEINT_CELL, SPICECHAR_CELL, SPICEBOOL_CELL,  # noqa: F401
                     SPICETIME_CELL, Cell_Double, Cell_Int, Cell_Char, Cell_Bool, Cell_Time, CTRLBLOCK)


def is_iterable(i):
    """Whether i is iterable (and not a string)."""
    return hasattr(i, '__iter__') and not isinstance(i, (str, bytes))

from ..exceptions import SpiceyError, NotFoundError  # noqa: F401,E402  (SpiceyPy has them here too)


class SpiceSPK18Subtype:
    """SPK type 18 subtypes."""
    S18TP0 = 0
    S18TP1 = 1


class SpiceEKDataType:
    """EK column data types."""
    SPICE_CHR = 0
    SPICE_DP = 1
    SPICE_INT = 2
    SPICE_TIME = 3
    SPICE_BOOL = 4


class SpiceEKExprClass:
    """EK query expression classes."""
    SPICE_EK_EXP_COL = 0
    SPICE_EK_EXP_FUNC = 1
    SPICE_EK_EXP_EXPR = 2


from ..exceptions import (SpiceyPyError, SpiceyPyIOError, SpiceyPyMemoryError,  # noqa: F401,E402
                          SpiceyPyTypeError, SpiceyPyKeyError, SpiceyPyIndexError,
                          SpiceyPyRuntimeError, SpiceyPyValueError, SpiceyPyZeroDivisionError)
import numpy as _np  # noqa: E402

#: SpiceyPy's name of a cell (PepperyPy passes cells themselves).
SpiceCellPointer = SpiceCell


class DataType:
    """The data types of cells."""
    SPICE_CHR = 0
    SPICE_DP = 1
    SPICE_INT = 2
    SPICE_TIME = 3
    SPICE_BOOL = 4


class _Record:
    """A fixed set of numbers with names (SpiceyPy's ctypes structures):
    `__array__` gives them in SPICELIB's order, so a record is passed where
    SPICELIB takes the array."""
    _dtype = _np.float64
    _size = 0

    def __init__(self, values=None):
        a = _np.zeros(self._size, dtype=self._dtype)
        if values is not None:
            v = _np.asarray(values, dtype=self._dtype).reshape(-1)
            a[:v.shape[0]] = v[:self._size]
        self._a = a

    @classmethod
    def _from_array(cls, a):
        return cls(a)

    def __array__(self, dtype=None, copy=None):
        return self._a.astype(dtype) if dtype is not None else self._a.copy()

    def __eq__(self, other):
        return isinstance(other, type(self)) and bool(_np.array_equal(self._a, other._a))

    def __repr__(self):
        return f'{type(self).__name__}({self._a.tolist()})'


class Plane(_Record):
    """A SPICE plane: a unit normal vector and a constant (SPICELIB's
    4-element array)."""
    _size = 4

    def __init__(self, values=None, normal=None, constant=None):
        super().__init__(values)
        if normal is not None:
            self._a[0:3] = normal
        if constant is not None:
            self._a[3] = constant

    @property
    def normal(self):
        return self._a[0:3].copy()

    @property
    def constant(self):
        return float(self._a[3])

    def __str__(self):
        return f"<SpicePlane: normal={', '.join(str(x) for x in self._a[0:3])}; constant={self._a[3]}>"


class Ellipse(_Record):
    """A SPICE ellipse: a center and semi-major and semi-minor axes
    (SPICELIB's 9-element array)."""
    _size = 9

    @property
    def center(self):
        return self._a[0:3].copy()

    @property
    def semi_major(self):
        return self._a[3:6].copy()

    @property
    def semi_minor(self):
        return self._a[6:9].copy()

    def __str__(self):
        return (f'<SpiceEllipse: center = {self.center}, semi_major = {self.semi_major}, '
                f'semi_minor = {self.semi_minor}>')


class SpiceDLADescr(_Record):
    """A DLA segment descriptor (SPICELIB's 8 integers)."""
    _dtype = _np.int32
    _size = 8
    bwdptr = property(lambda self: int(self._a[0]))
    fwdptr = property(lambda self: int(self._a[1]))
    ibase = property(lambda self: int(self._a[2]))
    isize = property(lambda self: int(self._a[3]))
    dbase = property(lambda self: int(self._a[4]))
    dsize = property(lambda self: int(self._a[5]))
    cbase = property(lambda self: int(self._a[6]))
    csize = property(lambda self: int(self._a[7]))


class SpiceDSKDescr(_Record):
    """A DSK segment descriptor (SPICELIB's 24 numbers)."""
    _size = 24
    surfce = property(lambda self: int(self._a[0]))
    center = property(lambda self: int(self._a[1]))
    dclass = property(lambda self: int(self._a[2]))
    dtype = property(lambda self: int(self._a[3]))
    frmcde = property(lambda self: int(self._a[4]))
    corsys = property(lambda self: int(self._a[5]))
    corpar = property(lambda self: self._a[6:16].copy())
    co1min = property(lambda self: float(self._a[16]))
    co1max = property(lambda self: float(self._a[17]))
    co2min = property(lambda self: float(self._a[18]))
    co2max = property(lambda self: float(self._a[19]))
    co3min = property(lambda self: float(self._a[20]))
    co3max = property(lambda self: float(self._a[21]))
    start = property(lambda self: float(self._a[22]))
    stop = property(lambda self: float(self._a[23]))


class SpiceEKAttDsc:
    """The attributes of an EK column (CSPICE's SpiceEKAttDsc)."""

    def __init__(self, cclass=0, dtype=0, strlen=0, size=0, indexd=False, nullok=False):
        self.cclass = int(cclass)
        self.dtype = int(dtype)
        self.strlen = int(strlen)
        self.size = int(size)
        self.indexd = bool(indexd)
        self.nullok = bool(nullok)

    @classmethod
    def _from_array(cls, a):
        """From SPICELIB's attribute descriptor (class, type code from 1,
        string length, size, index flag, null flag), as CSPICE converts it."""
        a = [int(x) for x in a]
        return cls(a[0], a[1] - 1, a[2], a[3], a[4] >= 0, a[5] >= 0)

    def __str__(self):
        return (f'<SpiceEKAttDsc cclass = {self.cclass}, dtype = {self.dtype}, strlen = {self.strlen}, '
                f'size = {self.size}, indexd = {self.indexd}, nullok = {self.nullok} >')


class SpiceEKSegSum:
    """The summary of an EK segment (CSPICE's SpiceEKSegSum)."""

    def __init__(self, tabnam='', nrows=0, ncols=0, cnames=(), cdescrs=()):
        self.tabnam = tabnam
        self.nrows = int(nrows)
        self.ncols = int(ncols)
        self.cnames = list(cnames)
        self.cdescrs = list(cdescrs)

    def __str__(self):
        return (f'<SpiceEKSegSum tabnam = {self.tabnam}, nrows = {self.nrows}, ncols = {self.ncols}, '
                f'cnames = {self.cnames}, cdescrs = {[str(d) for d in self.cdescrs]} >')
