"""PepperyPy: a Python interface to PEPPER, with SpiceyPy's API.

    import pepperypy as spice

    spice.furnsh('kernels.tm')
    et = spice.str2et('2026 NOV 8 12:00:00')
    state, lt = spice.spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH')

The module-level functions use the current instance (one is made at import)
and the calling thread's context of it, so threads may query at the same
time. For several kernel sets, explicit contexts and prepared queries, see
`Spice` and `Context`.
"""

from ._version import __version__, toolkit_version
from . import config
from .exceptions import (SpiceyError, SpiceyPyError, NotFoundError,  # noqa: F401
                         dynamically_instantiate_spiceyerror, SpiceyPyIOError, SpiceyPyMemoryError,
                         SpiceyPyTypeError, SpiceyPyKeyError, SpiceyPyIndexError, SpiceyPyRuntimeError,
                         SpiceyPyValueError, SpiceyPyZeroDivisionError)
from .cells import (SpiceCell, SPICEDOUBLE_CELL, SPICEINT_CELL, SPICECHAR_CELL,  # noqa: F401
                    SPICEBOOL_CELL, SPICETIME_CELL, Cell_Double, Cell_Int, Cell_Char, Cell_Bool,
                    Cell_Time, window)
from ._core import *  # noqa: F401,F403  (the generated functions)
from ._core import (Spice, Context, StateQuery, PositionQuery, RotationQuery, ContextInUseError,  # noqa: F401
                    CallbackError, current_instance, current_context, default_instance,
                    KernelBuilder, KernelLoader, load_kernel, add_loader)
from ._compat import *  # noqa: F401,F403
from ._compat import KernelPool  # noqa: F401
from .utils import support_types as stypes  # noqa: F401  (SpiceyPy's name)
from .utils.support_types import (Plane, Ellipse, SpiceCellPointer, SpiceDLADescr, SpiceDSKDescr,  # noqa: F401
                                  SpiceEKAttDsc, SpiceEKSegSum)
from .utils.callbacks import (SpiceUDFUNS, SpiceUDFUNB, UDFUNS, UDFUNB, UDFUNC, UDSTEP, UDREFN,  # noqa: F401
                              UDREPI, UDREPU, UDREPF, UDBAIL)
from .found_catcher import (found_check, no_found_check, found_check_off, found_check_on,  # noqa: F401
                            get_found_catch_state, spice_found_exception_thrower)
