# Moving from SpiceyPy to PepperyPy

PepperyPy is PEPPER's Python interface, with the API of
[SpiceyPy](https://github.com/AndrewAnnex/SpiceyPy). Most code written for
SpiceyPy runs unchanged after one change:

```python
import pepperypy as spice      # was: import spiceypy as spice
```

This guide lists what else a SpiceyPy user needs to know: what is the same,
what differs and why, and what to change in code that relies on a
difference. It is kept up to date as PepperyPy's interfaces change (see
[Changes](#changes)). For what PepperyPy adds (threads, several kernel sets,
prepared queries, kernels built in memory), see
[python/README.md](../python/README.md); for how fast it is, [Speed](#speed).

Compared with SpiceyPy 8.3.0 (CSPICE N0067).

## What is the same

- **Functions.** 650 of SpiceyPy's 651 functions, with its names,
  parameters and results ([python/COVERAGE.md](../python/COVERAGE.md)). The
  one missing, `fromisoformat`, is Python's `datetime.fromisoformat`, which
  SpiceyPy's module happens to import.
- **Results.** NumPy arrays and Python scalars, with SpiceyPy's shapes;
  lists of epochs where SpiceyPy takes them.
- **Errors.** `SpiceyError` and its subclasses (`SpiceNOSUCHFILE`, ...),
  `NotFoundError`, and the `SpiceyPy*Error` classes, also at the top level.
  Strings that CSPICE checks raise `SPICE(EMPTYSTRING)` when they are empty,
  as in SpiceyPy (from N0067's own list of checks).
- **Found flags.** `found_check`, `no_found_check`, `found_check_off`,
  `found_check_on`.
- **Cells and windows.** `cell_double`, `cell_int`, `cell_char`, `SpiceCell`
  and the window routines, as in SpiceyPy.
- **Types.** `stypes` (`spiceypy.utils.support_types`): `Plane`,
  `Ellipse`, `SpiceDLADescr`, `SpiceDSKDescr`, `SpiceEKAttDsc`,
  `SpiceEKSegSum`, `SpiceCell`, `DataType`, `SpiceEKDataType`,
  `SpiceEKExprClass`, with SpiceyPy's attributes.
- **Callbacks.** The decorators of `spiceypy.utils.callbacks`
  (`SpiceUDFUNS`, `SpiceUDSTEP`, ..., `UDFUNS`, ...) are accepted.

SpiceyPy's own tests run against PepperyPy
(`pixi run -e python py-test-spiceypy`: its `test_wrapper.py`, and the
tests and benchmarks of cyice): 1110 of the 1114 pass and one is skipped.
The other three are intentional differences: `test_szpool` (no limits),
`test_tkvrsn` (the version string) and `test_sce2c` (text kernel numbers,
see [Numbers](#numbers-the-last-bits)).

## Differences

### No fixed limits: size arguments are ignored

CSPICE's functions take sizes (`room`, `maxn`, `lenout`, `nelts`, ...) for
their output buffers, and SpiceyPy passes them on. PEPPER has no such
limits, so PepperyPy accepts these arguments and ignores them, and returns
all the data:

| Function | SpiceyPy | PepperyPy |
|---|---|---|
| `limbpt`, `termpt` | arrays of `maxn` points, the unused ones left over | only the points found (`sum(npts)` rows) |
| `dskv02` | `room` rows, the unused ones left over | the `n` vertices read |
| `getfov`, `getfvn` | at most `room` boundary vectors | all of them |
| `bodvar` | `dim` values | all the values |
| `ekrced`, `ekrcei`, `ekrcec` | at most `nelts` values | all the values |
| `gfevnt` | work space for `nintvls` intervals | sized from the step (see [GF searches](#gf-searches-with-your-own-routines)) |
| `szpool` | CSPICE's pool limits (26003 variables, ...) | `2147483647`: no limit |

Arguments that select data keep their meaning: `start` and `room` of
`gdpool`, `gipool`, `gcpool`, `gnpool`, `dskd02`, `dski02`, `dskp02` and
`dskv02` still give the part returned; `bufsiz` of `dafec` and `dasec`
still gives the number of lines per call.

**What to do:** nothing for most code. Code that relied on the unused rows
of `limbpt`'s, `termpt`'s or `dskv02`'s arrays (their length) should use
the number of points returned instead.

### Numbers: the last bits

PEPPER computes what SPICELIB computes, in the same order, so results agree
with CSPICE's to within a few units in the last place, and usually exactly.
Where they differ:

- **Text kernel numbers** are read correctly rounded. CSPICE's reader
  (NPARSD) is not: many decimal values come out one or a few ulps away
  (`2879.99424` → `2879.9942400000004`). Everything computed from such a
  value can differ in its last bits; through a spacecraft clock's rate the
  difference can grow, e.g. `sce2c` for Voyager 2 gives `985327949.9999729`
  where CSPICE gives `985327949.9999709` (2·10⁻⁶ ticks).
  ([spicelib_bugs.md](../spicelib_bugs.md), NPARSD.)
- **Fused multiply-adds.** PEPPER is built without them
  (`-ffp-contract=off`; see `fpm.toml`): its arithmetic is SPICELIB's as
  written, the same on every platform, as NAIF's x86-64 builds compute it.
  NAIF's CSPICE for Apple silicon (arm64), which SpiceyPy uses there, is
  built by clang, which fuses a multiplication and an addition within an
  expression: such results can differ from PEPPER's by an ulp or two
  (`reclat`'s radius, for example). gfortran cannot fuse the same way.

**What to do:** compare floating-point results with a tolerance, not for
equality (good practice with SpiceyPy too).

### The toolkit version

`tkvrsn('TOOLKIT')` returns PEPPER's version, such as
`PEPPER 0.1.0 (from SPICELIB N0067)`, not `CSPICE_N0067`.

### Threads, instances and contexts

The module-level functions use the current instance and the calling
thread's context, so single-threaded SpiceyPy code works as is, and
several threads may query at once (SpiceyPy is not thread-safe). A context
is for one thread at a time: passing a `Context` object to another thread
while it is in use raises `ContextInUseError`. See
[python/README.md](../python/README.md); for how fast it is, [Speed](#speed).

Kernel changes (`furnsh`, `unload`, `kclear`, the pool's `p*pool`) are not
to be made while another thread queries, as with any SPICE library.

### Callbacks: plain Python functions

PepperyPy calls Python functions directly, without ctypes. SpiceyPy's
decorators (`SpiceUDFUNS(f)`, `UDSTEP(f)`, ...) return the function
unchanged, so code that uses them works; so does code that passes plain
functions. An exception raised in a callback stops the search and is the
cause (`__cause__`) of the `CallbackError` raised.

### GF searches with your own routines

`gfevnt`, `gffove` and `gfocce` take the step, refinement, progress report
and interrupt routines, as in SpiceyPy:

- PepperyPy's own `gfstep`, `gfrefn`, `gfrepi`, `gfrepu` and `gfrepf`
  (decorated or not) select PEPPER's routines directly, with no call into
  Python.
- Any other Python function is called: `step(et) -> float`,
  `refn(t1, t2, s1, s2) -> float`, `repi(window, begmss, endmss)`,
  `repu(ivbeg, ivend, et)`, `repf()`, `bail() -> bool`.
- `gfevnt` ignores `nintvls` and `lenvals`: its work windows are sized from
  the step (`gfsstp`'s, with `gfstep`), assuming at most one transition per
  step, as PEPPER's other searches do. With your own step routine, the
  smallest step it gives at the starts of the confinement window's
  intervals is used.

### Interrupts (gfinth, gfbail, gfclrh)

CSPICE's `gfinth` is a signal handler: SpiceyPy users install it for
SIGINT so that Ctrl-C stops a search through `gfbail`. PepperyPy's
`gfinth(2)` records the interrupt, `gfbail()` reports it and `gfclrh()`
clears it, as CSPICE's do, but in Python: install it yourself if you want
Ctrl-C to stop searches,

```python
import signal
signal.signal(signal.SIGINT, lambda sig, frame: spice.gfinth(sig))
```

and pass `bail=True, udbail=spice.gfbail` to the search.

### SpiceyPy's types: Python classes, not ctypes structures

`Plane`, `Ellipse`, `SpiceDLADescr`, `SpiceDSKDescr`, `SpiceEKAttDsc` and
`SpiceEKSegSum` have SpiceyPy's attributes (`plane.normal`,
`ellipse.semi_major`, `descr.fwdptr`, ...), but they are not ctypes
structures. Planes, ellipses and descriptors convert to SPICELIB's arrays
(`np.asarray(plane)`: normal and constant; an ellipse's 9 numbers), and the
functions accept those arrays where they take the type.

- `SpiceCellPointer` is `SpiceCell` (cells are passed themselves).
- `ekssum`'s column descriptors have `cclass` 0: SPICELIB's EKSSUM does not
  give the column class.
- `ekpsel` returns positions from 0 and the types and classes as
  `SpiceEKDataType` and `SpiceEKExprClass` values, as SpiceyPy does; after
  a parse error its arrays are empty.

### cyice: `import pepperypy as cyice`

SpiceyPy's `spiceypy.cyice` (its Cython functions over CSPICE) is part of
PepperyPy itself: for each of its routines PepperyPy has `<name>_s` (one
element), `<name>_v` (arrays) and `<name>` (either, by the arguments), with
cyice's names and results. Change the import and nothing else:

```python
from spiceypy import cyice      # before
import pepperypy as cyice       # after
```

SpiceyPy's own tests and benchmarks of cyice pass this way.

- **`<name>` dispatches.** With an array where cyice takes one (`et` of
  `spkezr`, the points of `reclat`, ...) `<name>` calls `<name>_v`, as
  cyice's dispatchers do; a scalar (also a 0-d array, NumPy scalars,
  `bytes`) calls the scalar function. SpiceyPy's own `spkezr` and the
  others that accept lists of epochs return the same arrays as before.
- **The `_v` functions** are C loops over PEPPER's C interface with the GIL
  released, generated like the rest (`tools/gen_capi.py`): cyice's 71, and
  [46 more](#vectorized-functions-beyond-cyice);
  `spkezr_v`, `pxform_v` and `sxform_v` use prepared queries, which give
  the same results. They are also methods of a `Context`
  (`ctx.subpnt_v(...)`). They are as fast as cyice's or faster, but for
  the simplest coordinate conversions; see [Speed](#speed).
- **Found flags.** `ckgp_v`, `ckgpav_v` and `sincpt_v` raise
  `NotFoundError` unless every element was found, then return the results
  without the flags, as cyice does; under `no_found_check()` the flags (a
  `bool` array) come last.
- **Threads.** cyice releases the GIL but CSPICE is not thread-safe, so
  cyice cannot be called from several threads at once. PepperyPy's
  functions can.
- **Broadcasting.** Where a `_v` function loops over several arguments
  together (`latrec_v(radius, lon, lat)`, `conics_v(elts, ets)`), a single
  value is used for every element, as NumPy broadcasts; cyice needs arrays
  of the same length. `azlcpo_v` and the illumination functions (`illumf_v`,
  `illumg_v`, `ilumin_v`) evaluate every epoch with every point, as cyice
  does: results (epochs, points, ...).
- `limbpt_v` and `termpt_v` (loops in Python over `limbpt` and `termpt`)
  pad their arrays to the largest number of points found at an epoch
  (cyice: to `maxn`); zeros past an epoch's points. `getelm_v` takes pairs
  of lines ((n, 2)).
- String results are NumPy string arrays (of up to 255 characters).
- Each element of a `_v` function's results is the scalar function's
  result, to the last bit.
- cyice's `cyice_found_exception_thrower` decorator does not exist (the
  functions handle found flags themselves).

### Vectorized functions beyond cyice

PepperyPy has `_v` functions (and `_s` and dispatching `<name>`, as for
cyice's) for 46 routines that cyice does not vectorize:

| Group | Routines | Arrays of |
|---|---|---|
| Propagation | `prop2b`, `eqncpv`, `dpspce` | states and times (`prop2b`), epochs |
| Frames | `tipbod`, `tisbod`, `bodeul`, `pxfrm2`, `ckfrot`, `ckfxfm` | epochs (`pxfrm2`: both) |
| States with aberration corrections | `spkltc`, `spkacs`, `spkaps`, `spkapp`, `stelab`, `stlabx` | epochs, observer states and accelerations; vectors |
| SCLK | `sce2t` | epochs |
| Ellipsoids | `nearpt`, `surfpt`, `surfnm`, `npedln`, `edpnt` | points, rays, lines |
| Jacobians | `drdlat`, `drdcyl`, `drdsph`, `drdgeo`, `drdpgr`, `drdazl`, `dlatdr`, `dcyldr`, `dsphdr`, `dgeodr`, `dpgrdr`, `dazldr` | coordinates |
| Rotations | `axisar`, `rotate`, `rotvec`, `eul2m`, `eul2xf` | axes, angles, vectors, Euler angles |
| Matrices | `m2eul`, `xf2eul`, `m2q`, `q2m`, `xf2rav`, `rav2xf`, `raxisa`, `invstm` | matrices (n, 3, 3) or (n, 6, 6); quaternions |

The looped arguments broadcast together (`prop2b_v(gm, state, dts)`
propagates one state to many times, `prop2b_v(gm, states, dt)` many states
by one time). Matrices are NumPy's (n, rows, columns), as the scalar
functions return them. `ckfrot_v`, `ckfxfm_v` and `surfpt_v` handle found
flags as `sincpt_v` does. One matrix (a 3x3 array or nested lists) is a
scalar call: `m2q(r)` is `m2q_s(r)`, `m2q(rs)` with `rs` (n, 3, 3) is
`m2q_v(rs)`.

### No ctypes access to CSPICE

There is no CSPICE underneath: `spiceypy.utils.libspicehelper` (the
`libspice` ctypes library) and SpiceyPy's ctypes conversion helpers
(`stypes.to_double_vector`, `stypes.empty_double_matrix`, ...) do not
exist. Code that calls CSPICE through them must call the PepperyPy
function instead.

### EK fast load: short per-row arrays

`ekacli`, `ekacld` and `ekaclc` read `entszs` and `nlflgs`, and write
`wkindx`, for every row of the segment. Arrays shorter than the number of
rows (that of `rcptrs`) are padded with zeros (`False`) instead of being
read and written past their end, which in CSPICE corrupts memory. Pass
arrays of the segment's size.

### The traceback

`chkin`, `chkout`, `trcdep`, `trcnam` and `qcktrc` keep a traceback per
thread for the program's own modules. `trcoff()` turns it off for good, as
in CSPICE: later check-ins are ignored.

## Speed

PepperyPy is faster than SpiceyPy from Python: about 2–3× for one call,
and far more for arrays of epochs, which PepperyPy evaluates in one C loop
where SpiceyPy loops in Python. Its vectorized functions are as fast as
SpiceyPy's cyice, or faster, and unlike cyice they can run in several
threads at once.

Measured with `pixi run -e python py-bench`
([python/bench/bench_pepperypy.py](../python/bench/bench_pepperypy.py))
on an Apple M5 (10 cores), Python 3.13, SpiceyPy 8.3.0 (CSPICE N0067),
SpiceyPy's test kernels (DE405 subset), 20,000 random epochs; the best of
five runs, in µs per call or per element. Rerun it on your machine: the
ratios depend on the CPU and the compilers.

**One call at a time:**

| Call | SpiceyPy (µs) | PepperyPy (µs) | Speedup |
|---|--:|--:|--:|
| `spkezr` Moon from Earth, J2000, NONE | 2.60 | 1.15 | 2.3× |
| `spkezr` Mars barycenter from Earth, ECLIPJ2000, LT+S | 4.30 | 2.31 | 1.9× |
| `pxform` J2000 to IAU_EARTH | 1.79 | 0.70 | 2.6× |
| `str2et` | 5.58 | 3.82 | 1.5× |
| `vnorm` | 0.52 | 0.18 | 2.8× |

With the kernels in memory (`furnsh(..., in_memory=True)` on a `Spice`
instance) `spkezr` takes 0.82 and 1.84 µs.

**An array of epochs** (`spkezr('MOON', ets, 'J2000', 'NONE', 'EARTH')`):
SpiceyPy 1.88 µs per epoch, PepperyPy 0.33 (5.6×). PepperyPy's `spkezr`
with an array calls `spkezr_v`, a prepared query.

**Vectorized: SpiceyPy's cyice against PepperyPy's `_v` functions**, the
same arguments:

| Function | cyice (µs) | PepperyPy (µs) | Speedup |
|---|--:|--:|--:|
| `spkezr_v` | 2.43 | 1.19 | 2.0× |
| `spkpos_v` | 1.52 | 1.52 | 1.0× |
| `spkgeo_v` | 0.672 | 0.692 | 1.0× |
| `spkssb_v` | 0.491 | 0.341 | 1.4× |
| `pxform_v` | 0.420 | 0.296 | 1.4× |
| `sxform_v` | 0.459 | 0.326 | 1.4× |
| `subpnt_v` | 5.94 | 5.26 | 1.1× |
| `subslr_v` | 16.0 | 14.4 | 1.1× |
| `et2utc_v` | 1.49 | 0.679 | 2.2× |
| `timout_v` | 2.45 | 2.19 | 1.1× |
| `str2et_v` | 4.89 | 2.14 | 2.3× |
| `utc2et_v` | 4.62 | 1.82 | 2.5× |
| `deltet_v` | 0.691 | 0.052 | 13× |
| `recgeo_v` | 1.47 | 1.18 | 1.2× |
| `reclat_v` | 0.028 | 0.031 | 0.9× |
| `latrec_v` | 0.010 | 0.017 | 0.6× |

The simplest coordinate conversions take a few nanoseconds per element
either way (`reclat_v` varies between 0.9× and 1.2× from run to run);
`latrec_v` takes 7 ns more: PepperyPy's loop calls the Fortran routine
through its C interface, cyice calls CSPICE's C function directly. Either
is far below the cost of anything else in a program.

A call of a vectorized function with a scalar (`spkezr(..., et, ...)`)
checks its arguments before the scalar routine: about 10 ns.

**Threads** (PepperyPy only: CSPICE, and so cyice, is not thread-safe).
The 20,000 epochs of `subpnt_v` split over threads:

| Threads | µs per element | Speedup |
|--:|--:|--:|
| 1 | 5.07 | 1.0× |
| 2 | 2.64 | 1.9× |
| 4 | 1.40 | 3.6× |
| 8 | 1.01 | 5.0× |

Prepared state queries from memory scale the same way: 0.97 µs per epoch
on one thread, 0.22 on eight.

## Running SpiceyPy's tests against PepperyPy

```
pixi run -e python py-test-spiceypy
```

copies SpiceyPy's `test_wrapper.py`, `tests/test_cyice.py` and
`benchmarks/test_cyice.py` with `spiceypy` replaced by `pepperypy` (and
`from spiceypy import cyice` by `import pepperypy as cyice`) and runs
each (the benchmarks once, as tests); the three expected failures are
listed in [What is the same](#what-is-the-same).

## Changes

PepperyPy's interfaces that differ from SpiceyPy's, by date. Each is also
described above.

- **2026-11-16**
  - Added SpiceyPy's remaining functions: planes and ellipses (`nvc2pl`,
    `nvp2pl`, `psv2pl`, `pl2nvc`, `pl2nvp`, `pl2psv`, `inedpl`, `inelpl`,
    `inrypl`, `npelpt`, `pjelpl`, `vprjp`, `vprjpi`, `cgv2el`, `el2cgv`,
    `edlimb`), DSK and DLA (`dlabbs`, `dlabfs`, `dlafns`, `dlafps`,
    `dskgd`, `dskd02`, `dski02`, `dskp02`, `dskv02`, `dskxsi`, `dskxv`,
    `dskmi2`, `latsrf`, `srfnrm`), EK (`ekaclc`, `ekacld`, `ekacli`,
    `ekcii`, `ekffld`, `ekifld`, `ekpsel`, `ekrcec`, `ekrced`, `ekrcei`,
    `ekssum`), GF (`gfevnt`, `gffove`, `gfocce`, `gfinth`, `gfclrh`,
    `uddf`), CK writers (`ckw01`, `ckw02`, `ckw03`), comments (`dafec`,
    `dasec`), interpolation (`hrmint`, `hrmesp`, `lgrint`, `lgresp`,
    `chbder`, `polyds`), and `bodvar`, `ev2lin`, `ftncls`, `getelm`,
    `getfvn`, `kxtrct`, `limbpt`, `oscltx`, `ordc`, `scpart`, `spkpds`,
    `spksfs`, `termpt`, `timdef`, `zzdynrot`.
  - Size arguments ignored, all the data returned: `limbpt`, `termpt`,
    `dskv02`, `getfvn`, `bodvar`, `ekrced`, `ekrcei`, `ekrcec`, `gfevnt`
    (see [No fixed limits](#no-fixed-limits-size-arguments-are-ignored)).
  - `lgrind` takes SpiceyPy's arguments (`xvals, yvals, x`); the work
    space is made by PepperyPy.
  - Empty strings raise `SPICE(EMPTYSTRING)` where CSPICE checks them.
  - `trcoff` turns the traceback off, as in CSPICE (it did nothing).
  - `gfinth`, `gfclrh`, `gfbail`: the interrupt status kept in Python.
  - Built without fused multiply-adds (`-ffp-contract=off`): SPICELIB's
    arithmetic as written, the same on every platform.
  - [Speed](#speed): PepperyPy against SpiceyPy and cyice.
  - SpiceyPy's cyice functions in PepperyPy itself (`import pepperypy as
    cyice`): `<name>_s`, `<name>_v` and `<name>` dispatching to either by
    its arguments; the `_v` functions (71 of them) are C loops over PEPPER
    with the GIL released, also `Context` methods; `ckgp_v`, `ckgpav_v`
    and `sincpt_v` handle found flags as cyice does.
  - `_v` functions for 46 more routines (propagation, frames, aberration
    corrections, ellipsoids, Jacobians, rotations, matrices; see
    [Vectorized functions beyond cyice](#vectorized-functions-beyond-cyice)).
    `<name>` of these routines dispatches to `<name>_v` when given arrays.
