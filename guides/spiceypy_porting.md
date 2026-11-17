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
[python/README.md](../python/README.md).

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

SpiceyPy's own test suite runs against PepperyPy
(`pixi run -e python py-test-spiceypy`): 634 of its 638 tests pass and one
is skipped. The other three are intentional differences: `test_szpool` (no limits),
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
with CSPICE's to the last bit in nearly all cases. Where they differ:

- **Text kernel numbers** are read correctly rounded. CSPICE's reader
  (NPARSD) is not: many decimal values come out one or a few ulps away
  (`2879.99424` → `2879.9942400000004`). Everything computed from such a
  value can differ in its last bits; through a spacecraft clock's rate the
  difference can grow, e.g. `sce2c` for Voyager 2 gives `985327949.9999729`
  where CSPICE gives `985327949.9999709` (2·10⁻⁶ ticks).
  ([spicelib_bugs.md](../spicelib_bugs.md), NPARSD.)
- **Compilers.** PEPPER is built so that its arithmetic is SPICELIB's
  (`-ffp-contract=off`: no fused multiply-adds; see `fpm.toml`). A build
  with other options can differ in the last bits.

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
[python/README.md](../python/README.md).

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

## Running SpiceyPy's tests against PepperyPy

```
pixi run -e python py-test-spiceypy
```

copies SpiceyPy's `test_wrapper.py` with `spiceypy` replaced by
`pepperypy` and runs it; the three expected failures are listed in
[What is the same](#what-is-the-same).

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
  - Built without fused multiply-adds (`-ffp-contract=off`), so results
    agree with CSPICE's to the last bit (they could differ by an ulp).
