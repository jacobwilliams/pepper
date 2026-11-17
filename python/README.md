# PepperyPy

A Python interface to [PEPPER](https://github.com/jacobwilliams/pepper), the
thread-safe modern Fortran reimagining of NAIF's SPICELIB, with
[SpiceyPy](https://github.com/AndrewAnnex/SpiceyPy)'s API.

```python
import pepperypy as spice

spice.furnsh('kernels.tm')
et = spice.str2et('2026 NOV 8 12:00:00')
state, lt = spice.spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH')
```

Code written for SpiceyPy runs with `import pepperypy as spice`: the same
function names, arguments, results (NumPy arrays), exceptions
(`SpiceyError` and its subclasses, `NotFoundError`), found handling
(`found_check`, `no_found_check`), cells and windows. What PEPPER adds shows
through:

- **Threads.** Queries from several threads at once, each through its own
  context, are correct, and the long ones (batches, GF searches, geometry)
  run in parallel: the extension releases the GIL for them (see Speed).
  SpiceyPy (CSPICE) is not thread-safe.
- **Several kernel sets in one process**: instances (`Spice`).
- **Prepared queries and batches**: a state or a frame transformation
  resolved once and evaluated at an array of epochs in one call.
- **Results of any size**: coverage windows, GF results, pool values, as
  arrays of the size of the data, without `room` or cell sizes.
- **Programmatic ephemerides and frames** from Python functions.

PepperyPy is Cython extension modules over PEPPER's C interface (no
ctypes): a call costs a fraction of a microsecond, 3 to 5 times less than
SpiceyPy's (see Speed).

Moving code from SpiceyPy: see the [porting guide](../guides/spiceypy_porting.md)
(what differs, and why).

PepperyPy is not SpiceyPy, and is not supported by SpiceyPy's maintainers,
nor by NAIF. See [LICENSE](LICENSE).

## Installing

From the PEPPER repository, with [pixi](https://pixi.sh) (gfortran, a C
compiler, Cython, meson-python):

```bash
pixi run -e python py-build    # build and install (editable: rebuilt on import after a change)
pixi run -e python py-test     # the tests (SpiceyPy as the reference)
pixi run -e python py-bench    # speed against SpiceyPy
pixi run -e python py-test-spiceypy   # SpiceyPy's own test suite, against PepperyPy
pixi run -e python py-coverage  # COVERAGE.md
```

or with pip in an environment that has gfortran, a C compiler and NumPy:

```bash
pip install .                  # from the repository's root (pyproject.toml, meson.build)
```

## Instances and contexts

PEPPER's rule is: an *instance* holds the kernels; each thread queries
through its own *context*; the kernels change only while no thread queries.
PepperyPy has two levels.

**SpiceyPy's functions (module level).** Kernel changes (`furnsh`,
`unload`, `kclear`, `pdpool`, `boddef`, ...) go to the *current instance*,
one made at import. Queries go to the *calling thread's context* of it,
made on the thread's first query. Single-threaded SpiceyPy code is
unchanged, and code that queries from several threads is correct:

```python
from concurrent.futures import ThreadPoolExecutor

def positions(ets):
    return [spice.spkpos('SUN', et, 'J2000', 'CN+S', 'EARTH')[0] for et in ets]

with ThreadPoolExecutor(8) as pool:
    results = list(pool.map(positions, np.array_split(ets, 8)))
```

**Explicit objects.** For programs that manage threads, or need several
kernel sets:

```python
from pepperypy import Spice

de440 = Spice()
de440.furnsh('de440.tm', in_memory=True)    # binary kernels read whole into memory

ctx = de440.context()                       # this thread's context
state, lt = ctx.spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH')

with de440.as_current():                    # the module-level functions use de440 here
    state, lt = spice.spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH')
```

- `Spice`: the kernel changes (methods with SpiceyPy's names), `add_ephemeris`,
  `add_frame`, `set_in_memory`, `kernel_memory`, `set_max_open_files`,
  `context()` (the calling thread's context, the one the module-level
  functions use), `new_context()` (a context the caller owns), `as_current()`,
  `close()`.
- `Context`: every query, with SpiceyPy's names, arguments and results, as
  methods (`ctx.spkezr`, `ctx.pxform`, `ctx.gfdist`, ...), and PEPPER's own
  (`prepare_state`, `spk_coverage`, `gf_distance`, ...).
- `current_instance()`, `current_context()`, `default_instance()`.
- `KernelPool(paths)`: a block in which the module-level functions use a
  new instance with only these kernels; the current one is unchanged.

**The rules, enforced where it is cheap.**

- A context is used by one thread at a time: a call from a thread while
  another thread is in the context raises `ContextInUseError` (a C atomic
  in the context, a few nanoseconds). The thread that is in it may enter it
  again (a callback that makes queries). Handing a context to another
  thread between calls is allowed.
- Prepared queries belong to their context.
- Kernels must not be changed while other threads query: not checked (as in
  PEPPER).
- Errors are per context: an exception is raised in the thread that caused
  it. `found_check()` and `no_found_check()` apply to the calling thread (and
  asyncio task); `found_check_on()`/`found_check_off()` set the default for
  all.
- An instance is freed (`close()`, or when unreferenced) after its
  contexts. `Spice` objects cannot be pickled: with multiprocessing, each
  worker loads its kernels (threads share them).

## Prepared queries and batches

```python
q = ctx.prepare_state('MARS BARYCENTER', 'EARTH', 'ECLIPJ2000', 'LT+S')
state, lt = q(et)                 # one epoch
states, lts = q(ets)              # (n, 6), (n,): one call for all the epochs

r = ctx.prepare_rotation('J2000', 'IAU_EARTH')
m = r(et)                         # PXFORM's 3x3
ms = r(ets)                       # (n, 3, 3)
xs = r.transform(ets)             # SXFORM's (n, 6, 6)
```

The results are SPKEZR's, PXFORM's and SXFORM's to the last bit. Where
SpiceyPy accepts a list of epochs (`spkezr`, `spkpos`, `et2utc`, `str2et`,
...), so does PepperyPy, with the same result shapes.

For 117 routines there are also `<name>_v` functions: the routine at each
element of arrays, in a C loop with the GIL released
(`spice.subpnt_v(method, target, ets, ...)`, `ctx.reclat_v(points)`,
`spice.m2q_v(matrices)`). 71 are SpiceyPy's cyice's (the SPK and CK
queries, time and SCLK conversions, geometry, coordinate conversions);
46 are PepperyPy's (propagation, frames, aberration corrections,
ellipsoids, Jacobians, rotations and matrices; see the
[list](../guides/spiceypy_porting.md#vectorized-functions-beyond-cyice)). For each, `<name>_s` is the scalar function and
`<name>` dispatches to either by its arguments, as cyice does: code
written for cyice runs with `import pepperypy as cyice` (see the
[porting guide](../guides/spiceypy_porting.md#cyice-import-pepperypy-as-cyice),
and its [speed comparisons](../guides/spiceypy_porting.md#speed)).

## Results of any size

PEPPER's procedures that return results of any size are methods and
functions too, with windows as `(n, 2)` arrays:

```python
radii = spice.body_values_by_name('EARTH', 'RADII')
cover = spice.spk_coverage('de440.bsp', 301)                    # (n, 2)
ids = spice.spk_objects('de440.bsp')
maxima = spice.gf_distance('MOON', 'NONE', 'EARTH', 'LOCMAX', 0.0, 0.0, 5 * 86400.0, cover)
names = spice.pool_names('BODY399*')                            # also pool_values, pool_strings
```

SpiceyPy's cell forms (`spkcov`, `gfdist`, `wnunid`, ...) work as in
SpiceyPy: a cell keeps SPICELIB's layout in a NumPy array, so PEPPER's
cell, set and window routines work on it directly. `pepperypy.window(arr)`
makes a window from an `(n, 2)` array; `cell.to_intervals()` the reverse.

## GF searches with Python functions

```python
def distance(et):
    return spice.vnorm(spice.spkpos('MOON', et, 'J2000', 'NONE', 'EARTH')[0])

def decreasing(f, et):
    return spice.uddc(f, et, 10.0)

result = spice.gfuds(distance, decreasing, 'LOCMAX', 0.0, 0.0, 5 * 86400.0, 1000, cnfine)
maxima = ctx.gf_user_scalar(distance, 'LOCMAX', 0.0, 0.0, 5 * 86400.0, intervals)   # (n, 2)
```

The functions are plain Python callables: SpiceyPy's decorators
(`pepperypy.utils.callbacks.SpiceUDFUNS`, ...) are accepted and return the
function unchanged. An exception raised in a callback stops the search and
is the cause (`__cause__`) of the `CallbackError` raised.

## Programmatic ephemerides and frames

Bodies and frames whose states and orientations come from Python,
usable everywhere a body or a frame from a kernel is (states, light time
and aberration corrections, frames, GF searches, prepared queries):

```python
def orbit(ctx, body, et):                   # ctx: the Context of the query
    return state_or_None                    # 6 values relative to the center, in the frame

spice.default_instance().add_ephemeris(-999, 399, 'J2000', t0, t1, orbit)
state, lt = spice.spkezr('-999', et, 'J2000', 'LT+S', 'MOON')

def rotating(ctx, frame, et):
    return xform_6x6, 'J2000'               # from this frame to J2000

spice.default_instance().add_frame('MY_FRAME', 1400000, 399, rotating)
```

A provider may make queries through the context it is given (another
body's state, a frame). Providers are called from the querying threads,
under the GIL; `remove_ephemeris`, `remove_frame` and `kclear` remove them.

## Kernels built in memory

An SPK made from NumPy arrays, with no file, loaded like an SPK file
(states, `spkcov` and `spkobj` by its name, `kdata`, `unload`), and written
to a real SPK file if wanted (see the [main README](../README.md#kernels-built-in-memory-and-kernel-loaders)):

```python
from pepperypy import KernelBuilder

kb = KernelBuilder()
kb.spk_hermite(-10001, 399, 'J2000', ets, states, 7)    # SPK type 13; states: (n, 6)
kb.spk_lagrange(-10002, 399, 'J2000', ets, states, 8)   # SPK type 9
spice.load_kernel('my_trajectory', kb)                  # or pepperypy.load_kernel(...)
spice.write_kernel('my_trajectory', 'my_trajectory.bsp')
ctx.kernel_format('my_trajectory')                      # ('', '(programmatic)')
```

`kb.define_body('SPACECRAFT1', -10001)` defines a body name while the
kernel is loaded (removed with it). `tbeg`/`tend` set coverage bounds inside the epochs, `adapt=True` lowers
the degree of a segment too short for it, and a kernel loaded under a name
in use replaces it (`keep=True` keeps the builder's segments). A builder
belongs to no instance: threads can fill builders while others query.
`kb.add_ephemeris` and `kb.add_frame` add Python providers to the kernel,
and `kb.frame_rotations(name, code, center, base, epochs, rotations, degree)`
a frame from rotations at epochs ((n, 4) quaternions or (n, 3, 3) matrices).
Kernel loaders can be written in Python: a `KernelLoader` subclass with
`load(ctx, kb, file)` (and optionally `detect`, `unload`), registered with
`spice.add_loader(name, loader, extensions=[...])`; `spice.furnsh(path,
loader='NAME')` names one. See the [guide](../guides/ephemeris_data.md#from-c-and-python).

## Speed

`pixi run -e python py-bench` (Apple M5, 10 CPUs, Python 3.13 with the GIL,
SpiceyPy 8.3.0 with CSPICE N0067; DE405 subset, 20000 random epochs; µs per
epoch, the best of five runs):

| Operation | SpiceyPy | PepperyPy | Speedup | PepperyPy, kernels in memory | Speedup |
|---|--:|--:|--:|--:|--:|
| `spkezr` Moon from Earth, J2000, NONE, one call per epoch | 2.69 | 0.89 | 3.0× | 0.60 | 4.5× |
| `spkezr` Mars barycenter from Earth, ECLIPJ2000, LT+S, one call per epoch | 4.45 | 1.69 | 2.6× | 1.26 | 3.5× |
| `pxform` J2000 to IAU_EARTH, one call per epoch | 1.83 | 0.71 | 2.6× | | |
| `str2et` | 5.67 | 3.75 | 1.5× | | |
| `vnorm` | 0.53 | 0.19 | 2.8× | | |
| `spkezr` Moon, an array of epochs (PepperyPy: `spkezr_v`) | 1.97 | 0.19 | 10× | 0.13 | 16× |
| Moon from Earth: prepared query, one batch (SpiceyPy: an array of epochs) | 1.97 | 0.18 | 11× | 0.13 | 15× |
| Mars barycenter, LT+S: prepared query, one batch (SpiceyPy: one call per epoch) | 4.45 | 0.62 | 7.2× | 0.60 | 7.4× |
| J2000 to IAU_EARTH: prepared rotation, one batch (SpiceyPy: one call per epoch) | 1.83 | 0.31 | 5.9× | | |

SpiceyPy has no prepared queries: their rows compare with its fastest way
to compute the same results. For PepperyPy's vectorized functions against
SpiceyPy's cyice, see the
[porting guide](../guides/spiceypy_porting.md#speed).

Threads (Mars barycenter, LT+S, kernels in memory; the epochs split among
the threads, each with its own context). The speedups are over SpiceyPy's
4.45 µs, one call per epoch, from one thread (CSPICE is not thread-safe):

| Threads | one call per epoch | Speedup | prepared batches | Speedup |
|--:|--:|--:|--:|--:|
| 1 | 1.18 | 3.8× | 0.60 | 7.4× |
| 2 | 1.18 | 3.8× | 0.31 | 14× |
| 4 | 1.18 | 3.8× | 0.16 | 28× |
| 8 | 1.18 | 3.8× | 0.13 | 33× |

- A call keeps the GIL unless it is a long one (GF searches, geometry,
  DSK, EK, loading and coverage, batches of a prepared query): releasing
  and taking back the GIL around a call of a fraction of a microsecond
  costs more than the call when threads contend for it. So one call per
  epoch does not speed up with threads (nor slow down); batches and long
  calls run in parallel. In free-threaded Python (3.13t, 3.14t) every call
  does.
- About 0.4 µs of a short call is Python's: the call, finding the thread's
  context, encoding the strings, making the result arrays. A prepared query
  pays it once per batch.
- PEPPER is built without fused multiply-adds (`-ffp-contract=off`), for
  SPICELIB's roundings on every platform. On arm64 this costs 6–10% in
  batches of SPK states (Chebyshev evaluations); calls one at a time hardly
  notice it.

## Differences from SpiceyPy

- `tkvrsn` reports PEPPER's version. Tracebacks are empty (`qcktrc`):
  PEPPER keeps none.
- `erract`, `errdev`, `errprt` are accepted and change nothing: errors are
  always raised as exceptions.
- `KernelPool` uses a temporary instance: the current one, its kernels and
  its kernel pool, are untouched (SpiceyPy's clears and reloads them).
- `furnsh` also loads JSON kernels: a file named `*.json` is a text kernel
  in [jsonspice](https://github.com/jacobwilliams/jsonspice)'s format,
  read by PEPPER itself (see the [main README](../README.md#json-kernels)).
  Unlike jsonspice, a bad `"@"` time is an error, and dicts are not
  accepted (write the JSON to a file).
- Limits PEPPER removed are gone: string lengths, the sizes of output cells
  (`lenout`, `room` and the like are accepted and ignored).
- SpiceyPy's functions that PepperyPy does not have are listed, with the
  reason, in [COVERAGE.md](COVERAGE.md): PepperyPy has 574 of SpiceyPy's 651
  (88%); most of the others take or return SpiceyPy's ctypes structures
  (planes, ellipses, DLA and DSK descriptors, EK queries) or work arrays.
- SpiceyPy's own test suite, run against PepperyPy
  (`pixi run -e python py-test-spiceypy`): 523 of its 639 tests pass; of the
  others, 108 use a function PepperyPy does not have, and 5 differ:
  `tkvrsn` (PEPPER's version), `szpool` (PEPPER has no pool size limits),
  `gfposc` (a time agrees to 1 µs, not to its last printed digit), `lgrind`
  (a work array) and `dafopw` (a file still loaded by an earlier test).
- Results agree with SpiceyPy's (CSPICE N0067) to 1e-12 relative, those of
  iterations (`sincpt`) to 1e-10 (the tests compare ~130 calls).

## How it is built

```
Python      pepperypy  (SpiceyPy's API; Spice, Context, cells, _compat.py)
              │ Cython extension module (_core.pyx; cdef extern from "pepper.h")
C ABI       PEPPER's C interface (src/capi, include/pepper.h)
              │
Fortran     PEPPER (pepper_instance, pepper_context)
```

- The C interface and the Cython wrappers are generated from PEPPER's
  Fortran interfaces by `tools/gen_capi.py` (`pixi run -e python gen-capi`):
  every context procedure whose arguments it knows (scalars, strings,
  arrays of fixed size or sized by an argument, cells, results of any size)
  — about 830 of them, with SpiceyPy's parameter names where SpiceyPy has the
  function. `python/src/pepperypy/_compat.py` has the functions whose
  SpiceyPy signature differs from PEPPER's (`gdpool`, `bodvrd`, `gfdist`
  with cells, ...).
- meson-python builds PEPPER's Fortran sources into a static library linked
  into the extension (`meson.build` at the repository's root).
- The design: [plans/pepperypy_plan.md](../plans/pepperypy_plan.md).
