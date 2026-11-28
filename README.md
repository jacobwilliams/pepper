<p align="center">
  <img src="media/logo.svg" alt="PEPPER" width="500">
</p>

**PEPPER**: Planetary Ephemerides, Positions, Pointing, Events & Rotations.

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![CI](https://github.com/jacobwilliams/pepper/actions/workflows/ci.yml/badge.svg)](https://github.com/jacobwilliams/pepper/actions/workflows/ci.yml)

## Overview

PEPPER provides tools for computing planetary ephemerides, positions, pointing, events, and rotations, leveraging modern Fortran features for high performance and maintainability. It can be thought of as a modern, thread-safe reimagining and replacement for [SPICELIB](https://naif.jpl.nasa.gov/naif/toolkit.html), the Fortran 77 SPICE library that has been developed by NASA's [NAIF](https://naif.jpl.nasa.gov/naif/) for decades. PEPPER aims not to rewrite the SPICELIB code from scratch or convert it into another language, but to modernize, improve, and add new features to it while maintaining compatibility with existing SPICELIB applications and tools.

PEPPER can be used from three languages:

- **Fortran:** the library itself, with SPICELIB's routines as methods of an instance (`spice%furnsh`) and of its per-thread contexts (`ctx%spkezr`).
- **C and C++:** a C interface (`include/pepper.h`), generated from the Fortran interfaces (see [The C interface](#the-c-interface)).
- **Python:** [PepperyPy](python/README.md), with [SpiceyPy](https://github.com/AndrewAnnex/SpiceyPy)'s API, so SpiceyPy programs run with `import pepperypy as spice`, 3 to 5 times faster and safe to call from several threads (see [Python: PepperyPy](#python-pepperypy)).

PEPPER is fast:

- **Against SPICELIB**, both reading kernels from their files, it is 1.4 to 5× faster than NAIF's build for most queries, and about as fast as or faster than SPICELIB compiled with `-O3`. Where SPICELIB searched linear lists, the gains are larger: unloading a kernel is 1,800× faster, and a frame among 1,000 TK frames 39×.
- **With kernels read into memory** (`in_memory=.true.`), a state takes about 0.25 µs instead of about 10 µs from the files.
- **Prepared queries** resolve names and frames once, then cost 0.1 to 0.5 µs per state.
- **Against [ANISE](https://github.com/nyx-space/anise)**, the Rust library, PEPPER's in-memory queries are generally faster, and its prepared queries are 1.1 to 19× faster.
- **Threads:** queries scale with cores (5.4× on 8 threads). SPICELIB can't be used from several threads at all.
- **From Python**, PepperyPy is 3 to 5× faster than SpiceyPy for single calls, and up to 30× with prepared queries in batches across threads.

The measurements are in [pepper_vs_spicelib.md](pepper_vs_spicelib.md) and [python/README.md](python/README.md).

> PEPPER is new and experimental. Its interfaces (Fortran, C and Python) may still change. It passes NAIF's test suite and agrees with SPICELIB and SpiceyPy in the comparisons described here, but it has not yet been used for any mission-critical applications. PEPPER is not developed or supported by NAIF, JPL, or NASA.

## Features

- Compute planetary ephemerides with high precision.
- Determine positions and orientations of celestial bodies.
- Calculate pointing directions for observational instruments.
- Identify and analyze astronomical events.
- Handle rotations and transformations efficiently.

How PEPPER differs from SPICELIB (thread safety, the limits it removes, speed
comparisons): [pepper_vs_spicelib.md](pepper_vs_spicelib.md).

## SPICE documentation

PEPPER keeps SPICELIB's routines, their names, arguments and behavior
(`call spkezr(...)` becomes `call ctx%spkezr(...)`; see "Usage"), so NAIF's
documentation of the SPICE Toolkit for Fortran (N0067) applies. What
PEPPER changes is in [pepper_vs_spicelib.md](pepper_vs_spicelib.md).

- [NAIF](https://naif.jpl.nasa.gov/naif/), the Navigation and Ancillary
  Information Facility of NASA's Jet Propulsion Laboratory, which develops
  SPICE.
- [The SPICE Toolkit for Fortran](https://naif.jpl.nasa.gov/naif/toolkit_FORTRAN.html):
  the packages of SPICELIB N0067 that PEPPER started from.
- [Toolkit documentation](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/index.html),
  and the [SPICELIB routines](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/index.html)
  (each routine's header; PEPPER's FORD documentation, `pixi run docs`, has
  them too).
- [Required reading](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/index.html),
  the documents on each subsystem, among them:
  [kernels](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/kernel.html),
  [SPK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/spk.html) (ephemerides),
  [frames](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/frames.html),
  [time](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/time.html),
  [aberration corrections](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/abcorr.html),
  [NAIF IDs](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/naif_ids.html),
  [PCK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/pck.html),
  [CK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/ck.html),
  [SCLK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/sclk.html),
  [DSK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/dsk.html),
  [GF](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/gf.html) (geometry finder),
  [EK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/ek.html),
  [cells](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/cells.html),
  [windows](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/windows.html),
  [error handling](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/error.html)
  (PEPPER: per context; see "Usage").
- [Tutorials](https://naif.jpl.nasa.gov/naif/tutorials.html): NAIF's
  introductions to SPICE and each subsystem.
- [Data](https://naif.jpl.nasa.gov/naif/data.html) and the
  [generic kernels](https://naif.jpl.nasa.gov/pub/naif/generic_kernels/)
  (planetary ephemerides, leap seconds, planetary constants, frames).

## Building and testing

PEPPER builds with [fpm](https://fpm.fortran-lang.org); the development
environment (gfortran, fpm, Python, FORD) is managed with [pixi](https://pixi.sh):

```bash
pixi run build          # the library (debug profile)
pixi run test           # the unit tests (test/core), with bounds checks
pixi run tspice         # NAIF's test suite (the tspice package), in parallel, with bounds checks
pixi run tspice-release # NAIF's test suite against the optimized (-O3) library
pixi run test-parallel  # queries from several OpenMP threads
pixi run check-threads  # no thread-unsafe constructs (also run by `pixi run test`)
pixi run bench-parallel # timings from 1, 2, 4 and 8 threads
pixi run docs           # FORD documentation in doc/
pixi run -e coverage coverage      # test coverage of src/ (unit tests): build/coverage/html/index.html
pixi run -e coverage coverage-all  # the same with NAIF's test suite (tspice) too
```

The tasks name their fpm profile: `--profile debug` (bounds and
array-temporary checks, backtraces) or `--profile release` (`-O3`). With no
profile, fpm builds neither optimized nor checked: `fpm.toml`'s
`[profiles] default` replaces fpm's built-in flags.

To use PEPPER as an fpm dependency, give your own package its compiler
options: fpm compiles a dependency with the root package's flags, not with
the dependency's own. For gfortran, `-frecursive` (locals on the stack:
without it the library is not thread-safe) and `-ffp-contract=off` (no fused
multiply-adds: SPICELIB's arithmetic, as NAIF's tests expect); for Intel
Fortran, `-recursive -fp-model precise`:

```toml
[dependencies]
pepper = { git = "https://github.com/jacobwilliams/pepper" }

[features]
platform.gfortran.flags = "-frecursive -ffp-contract=off"
platform.ifx.flags = "-recursive -fp-model precise"

[profiles]
default = ["platform"]
```

The same tests with Intel Fortran (ifx), as the CI runs them, on any machine
with Docker: `tools/ifx_docker/ifx.sh` (see `tools/ifx_docker/README.md`).

PepperyPy, the Python interface, and the C interface build with
meson-python in the `python` environment:

```bash
pixi run -e python py-build     # build and install PepperyPy (editable)
pixi run -e python py-test      # its tests (SpiceyPy as the reference)
pixi run -e python test-capi    # the C test of the C interface
pixi run -e python gen-capi     # regenerate the C interface and the wrappers
pixi run -e python py-bench     # speed against SpiceyPy
pixi run -e python py-test-spiceypy  # SpiceyPy's own test suite, against PepperyPy
```

## Usage

**Guides:** [Getting ephemeris data into PEPPER](guides/ephemeris_data.md):
SPICE kernels, providers (your code computes the states), programmatic
kernels (your states, PEPPER interpolates), and kernel loaders (files in
other formats), with examples.

All state belongs to an instance (`type(pepper_instance)`): loaded kernels, the
kernel pool, file tables, the names defined with `boddef`. The kernels are
changed through the instance: `spice%furnsh`, `unload`, `kclear`, `ldpool`,
`clpool`, the pool's setters (`pcpool`, `pdpool`, `pipool`, `lmpool`,
`dvpool`), `boddef` and the low-level loaders (`spklef`, `pcklof`, `cklpf`,
`eklef` and their unloaders). Their errors are reported through the
instance's own context (`spice%context()`; `spice%failed()`).

Each thread queries the instance through a context (`type(pepper_context)`,
from `spice%new_context()`); the procedures are type-bound procedures of the
context with SPICELIB's names and arguments, and errors are reported through
the context. A context finds the changes of the kernels on its next call. A
serial program can also query through the instance's own context.

```fortran
use pepper_module  ! PEPPER's public interface

type(pepper_instance), target :: spice
type(pepper_context) :: ctx
real(wp) :: et, state(6), lt

call spice%furnsh('kernels.tm')
if (spice%failed()) ...
ctx = spice%new_context()
call ctx%str2et('2026 NOV 8 12:00:00 UTC', et)
call ctx%spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH', state, lt)
if (ctx%failed()) ...
```

Load kernels first, then query from any number of threads, one context per
thread (see `test/core/test_parallel.f90`). The kernels must not be changed
while other threads use contexts of the instance (nothing checks this).
Declare each thread's context as a local variable of a procedure that the
threads call (with OpenMP: a procedure called in the parallel region), not
in the main program or in a `BLOCK` of the main program: such variables are
shared by the threads (ifx gives a main program's `BLOCK` variables the
main program's implicit SAVE), and threads sharing a context crash.

A context is released when it goes out of scope, and an instance likewise.
Variables of a main program or a module live until the program ends, so to
release them sooner call `ctx%destroy()`, which closes the context's files and
frees its memory. Then call `spice%destroy()`, which unloads the kernels,
removes programmatic ephemerides and frames, and frees the instance's memory.
Destroy an instance's contexts before the instance itself. Both can be used
again afterward: `ctx = spice%new_context()`, `spice%furnsh(...)`.

Binary kernels (SPK, CK, binary PCK) are read record by record as queries
need them, as in SPICELIB. They can instead be read whole into memory when
they are loaded: `call spice%furnsh('de440.bsp', in_memory=.true.)` (for a
meta-kernel: every binary kernel it lists), or `call
spice%set_in_memory(.true.)` for all later loads. Queries at scattered epochs
then never read the file (DE421 at random epochs: 9.6 → 0.22 µs per state),
at the cost of memory equal to the file sizes, once per instance
(`spice%kernel_memory()`).

gfortran (at least up to 16.2) keeps the length of a deferred-length
character function result (a `character(:), allocatable` result) in a static
variable at each call site, which threads share. The library therefore has no
such functions: they return strings of a length computed from their
arguments, or are subroutines with an allocatable argument
(`tools/check_threads.py` checks the sources; `pixi run check-threads-ir`
checks the generated code for any static variable). The same applies to
programs that call PEPPER from several threads.

Each context keeps up to 23 files open at a time, as SPICELIB does, and
reopens a file when it needs it again. A program that queries many kernels in
turn can raise this before creating its contexts, for example
`call spice%set_max_open_files(500)`; each context may then use that many file
descriptors, within the operating system's limit (`ulimit -n`).

For programs written for SPICELIB, the module `pepper_legacy`
(`src/legacy/`) has SPICELIB's routines with SPICELIB's names
and arguments (`call spkezr(...)`): thin wrappers that call the library with a
default instance and context, whose error handling starts with SPICELIB's
settings (`erract`, `errprt` and `errdev` work as in SPICELIB). It is close to
a drop-in replacement: a program adds `use pepper_legacy` to each program
unit and drops its own declarations of SPICELIB's functions (`LOGICAL
FAILED`, `EXTERNAL ...`), which the module declares. Its interfaces are
explicit, so the compiler checks the calls: F77 code that passes a scalar
where SPICELIB declares an array (or the reverse), or one type for another,
is fixed as NAIF's test suite was (`[x]` for a value the routine reads; an
array equivalenced with the scalar, or the array's first element, otherwise).
Like SPICELIB, it is not thread-safe; threads use the module API. NAIF's
test suite (the `tspice` package) runs through it.

```fortran
program legacy_example
   use pepper_legacy
   implicit none
   character(80) :: action = 'RETURN'
   double precision :: et, state(6), lt
   call erract('SET', action)       ! errors return (SPICELIB's default stops)
   call furnsh('kernels.tm')
   call str2et('2026 NOV 8 12:00:00', et)
   call spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH', state, lt)
   if (failed()) call reset()
end program legacy_example
```

`example/legacy_example.f90` is a complete one (`fpm run --example legacy_example`).

Bugs and inconsistencies found in SPICELIB N0067 are listed in
`spicelib_bugs.md`; the design and its history are in `plans/plan.md`.

## JSON kernels

A text kernel can be written in JSON instead of SPICE's PCK syntax: any file
named `*.json` is read as one. The format is the one
[jsonspice](https://github.com/jacobwilliams/jsonspice) defined for
SpiceyPy. Here is a text kernel:

```text
KPL/PCK
\begindata
BODY399_RADII   = ( 6378.1366 6378.1366 6356.7519 )
NAIF_BODY_NAME += 'SPOCK'
NAIF_BODY_CODE += -1000
EPOCH           = @2026-NOV-11/12:00
\begintext
```

And the same kernel in JSON:

```json5
// a JSON kernel
{
    "BODY399_RADII": [6378.1366, 6378.1366, 6356.7519],
    "+NAIF_BODY_NAME": "SPOCK",
    "+NAIF_BODY_CODE": -1000,
    "EPOCH": "@2026-NOV-11 12:00",
}
```

It is loaded like any kernel: `spice%furnsh('masses.json')` in Fortran, and
`furnsh` from C or Python. FURNSH, UNLOAD, `kdata`/`ktotal` (where it is a
`TEXT` kernel) and the pool's watches treat it as a text kernel.

| JSON | Kernel pool |
|---|---|
| number | a numeric value, correctly rounded (beyond double precision's range: SPICE(NUMBEREXPECTED), as in text kernels) |
| `true`, `false` | 1, 0 |
| string | a character value (escapes resolved, `\u` as UTF-8; a NUL or a lone surrogate is an error) |
| `"@..."` | a time: UTC seconds past J2000 (as `@` in text kernels; a bad time is SPICE(BADTIMESPEC)) |
| array of the above | the values: all strings, or all numbers and times |
| `"+NAME": ...` | appends to NAME (`+=`) |
| `null`, a nested object or array, an empty array | an error (SPICE(BADVARASSIGN)) |

The syntax is standard JSON plus `//` comments and trailing commas.

- **Atomic loads:** a kernel with an error (a SPICE(JSONSYNTAXERROR), with
  its line) loads nothing.
- **Duplicate keys:** they apply in order, as repeated assignments do in a
  text kernel.
- **Meta-kernels:** a JSON kernel that sets `KERNELS_TO_LOAD` is a
  meta-kernel, with `PATH_VALUES` and `PATH_SYMBOLS` as usual, and it may
  list both kinds of kernel:

```json5
{
    "PATH_VALUES": ["/data/kernels"],
    "PATH_SYMBOLS": ["K"],
    "KERNELS_TO_LOAD": ["$K/naif0012.tls", "$K/de440.bsp", "$K/masses.json"],
}
```

`tools/tk2json.py` converts a text kernel to JSON, keeping its comments as
`//` lines. `--reverse` converts JSON back to a text kernel, for SPICELIB and
other toolkits.

Numbers are correctly rounded in both formats, by the one conversion of
decimal text to double precision that the library uses everywhere
(`decimal_value` in `src/pepper_strings.f90`: text and JSON kernels, time
strings, DAF text transfer files), so a `.tpc` and its `.json` give the same
pool with any compiler. The parser is `src/pepper_json.f90`; the design is in
`plans/json_kernels_plan.md`.

### Where the pool's values came from

The kernel pool remembers the source of each assignment in effect: the
kernel file and the line where the assignment begins, or a tag for the
values a program set (`(PDPOOL)`, `(PCPOOL)`, `(PIPOOL)`, `(LMPOOL)` with its
buffer's line, `(add_frame)`). An `=` replaces a variable's sources, a `+=`
adds one, so when several kernels define a variable, its sources say which
one won (or which ones it combines).

```fortran
type(string), allocatable :: sources(:), names(:)
integer(ip), allocatable :: lines(:)

call ctx%pool_sources('BODY399_RADII', sources, lines, found)  ! which kernels, which lines
call ctx%pool_defined_by('pck00011.tpc', names)                ! what that kernel defined
call ctx%pool_write_json(unit)                                 ! the whole pool, as a JSON kernel
```

`pool_write_json` writes one `"NAME": value` per variable with a `//`
comment naming its sources; the file loads back as a JSON kernel with the
same values (the numbers are written with the digits they need to read back
exactly).

## Prepared queries

A state, position or frame query can be resolved once (names, aberration
correction, frame) and then evaluated at any number of epochs, one at a
time or as a batch, with no string handling per epoch. The results are
SPKEZR's, SPKPOS's, PXFORM's and SXFORM's, to the last bit, and states are
1.5 to 4 times faster (`pepper_vs_spicelib.md`, "Prepared queries"). A
position query evaluates no velocities (only the position series of
Chebyshev segments, 3x3 frame rotations, no observer acceleration for
stellar aberration): about 1.3 to 2 times faster than a state query.

```fortran
use pepper_module, only: state_query, position_query, rotation_query

type(state_query) :: q
type(position_query) :: p
type(rotation_query) :: r
real(wp) :: state(6), pos(3), lt, states(6, 1000), positions(3, 1000), lts(1000), ets(1000), rot(3, 3)

call ctx%prepare_state(q, 'MOON', 'EARTH', 'J2000', 'LT+S')
call ctx%query_state(q, et, state, lt)         ! one epoch
call ctx%query_state(q, ets, states, lts)      ! a batch (lts is optional)

call ctx%prepare_position(p, 'MOON', 'EARTH', 'J2000', 'LT+S')
call ctx%query_position(p, et, pos, lt)        ! SPKPOS
call ctx%query_position(p, ets, positions, lts)

call ctx%prepare_rotation(r, 'J2000', 'ITRF93')
call ctx%query_rotation(r, et, rot)            ! PXFORM; query_transform: SXFORM
```

A query keeps the segments it used and their current records, so a thread
uses its own queries (as its own context).

## Programmatic ephemerides and frames

Bodies and frames can come from the program instead of kernel files: a
provider computes the states or the orientation, and the body or frame is
then used everywhere one from a kernel is (states with any correction,
frame transformations, GF searches, prepared queries). See
`plans/programmatic_plan.md`, and the guide
[Getting ephemeris data into PEPPER](guides/ephemeris_data.md).

```fortran
use pepper_module, only: ephemeris_provider, frame_provider, pepper_context

type, extends(ephemeris_provider) :: my_orbit
   !! your provider type: its components hold the orbit data
   real(wp) :: r0(3), v(3)
contains
   procedure :: state => my_orbit_state  ! (self, ctx, body, et, state, found)
end type

call spice%add_ephemeris(-999, 399, 'J2000', t0, t1, my_orbit(r0, v))
call ctx%spkezr('-999', et, 'J2000', 'LT+S', 'MOON', state, lt)

call spice%add_frame('MY_FRAME', 1400000, 399, my_frame_provider)  ! no frame kernel
call ctx%pxform('MY_FRAME', 'J2000', et, rot)
```

- A provider's `state` gives the body's state relative to its registered
  center in the registered frame. A frame provider's `transform` gives the
  state transformation from the frame to the frame it is relative to (any
  frame, programmatic ones included), as a TK frame's rotation does.
- `ephemeris_procedure` and `frame_procedure` wrap plain procedures.
- A programmatic body takes part in the segment search as a file loaded when
  it was added: it overrides files loaded before and is overridden by files
  loaded after, within its coverage.
- `remove_ephemeris` and `remove_frame` remove one; `kclear` removes all
  (`clpool` keeps the frames, as it keeps the loaded files). A kernel can
  own programmatic ephemerides too (a file loader's, for a file that holds a
  model: "Kernels built in memory and kernel loaders" below); those go
  with their kernel, and `remove_ephemeris` does not remove them.
- Providers are called from any context, so from several threads at once:
  they must not change shared data.
- A provider may make queries through the context it is given (states,
  frame transformations, names, kernel variables), which may call other
  providers: a frame defined by bodies' states (a two-body frame), a body
  defined from other bodies. It must not change the kernels or run a GF
  search. A provider whose result depends on itself (more than 50 nested
  provider calls) gives SPICE(RECURSIONTOODEEP). A provider procedure that
  can be nested (its queries reach bodies or frames of the same provider
  type) must be `recursive`.

## Kernels built in memory and kernel loaders

A program can make an SPK from its own arrays, with no file, and load it
like an SPK file: states and GF searches, SPKCOV and SPKOBJ by its name,
KDATA (type SPK), UNLOAD and KCLEAR. A **kernel loader** does the same for a
file format SPICE does not know: FURNSH hands it the file, it fills a
builder, and the file is then an SPK in memory. PEPPER has no loader for
any real format; loaders are separate libraries. The guide
[Getting ephemeris data into PEPPER](guides/ephemeris_data.md) explains
all of it with examples; the design is in `plans/external_loaders_plan.md`.

```fortran
use pepper_module, only: kernel_builder, kernel_loader

type(kernel_builder) :: kb
call kb%spk_hermite(ctx, -10001, 399, 'J2000', epochs, states, 7)   ! SPK type 13
call kb%spk_lagrange(ctx, -10002, 399, 'J2000', epochs, states, 8)  ! SPK type 9
call spice%load_kernel('my_trajectory', kb)       ! a kernel with no file
call spice%write_kernel('my_trajectory', 'my_trajectory.bsp')       ! to a real SPK

call spice%add_loader('MYFORMAT', my_loader, extensions=['.myeph'])
call spice%furnsh('sc.myeph')                      ! by extension
call spice%furnsh('sc_ephem.txt', loader='MYFORMAT')               ! by name
```

- **The builder** takes states at epochs and uses SPICE's interpolation:
  Lagrange (SPK type 9) or Hermite (type 13, odd degrees), with SPKW09's and
  SPKW13's checks and short error messages. `tbeg` and `tend` set coverage
  bounds inside the epochs. With `adapt`, a segment too short for its degree
  has its degree lowered (two states make a Hermite cubic, one state is
  dropped). It does not depend on an instance: threads can fill builders
  while others query. Frames are resolved when the builder is loaded.
- **`load_kernel(name, kb, keep)`** loads a builder under a name. A name in
  use is replaced (the old kernel is unloaded once the new one has loaded;
  the new kernel has the highest priority). The builder is emptied, unless
  `keep`. An error, such as an unknown frame, loads nothing and keeps the
  old kernel.
- **A loader** extends `kernel_loader` with `load(self, ctx, kb, file)`:
  read the file, fill the builder, signal errors in `ctx`. Optional:
  `detect(self, file, head)` (accept a file by its first 4 KB) and
  `unload(self, file)` (free what the loader kept). The instance keeps its
  own copy of the loader; `spice%loader(name, ptr)` gives access to it.
- **How FURNSH chooses** (in full in `spice_furnsh`'s documentation):
  1. `loader=` (or the meta-kernel variable `KERNEL_LOADERS`, pairs of a
     file of `KERNELS_TO_LOAD` and a loader's name): the registered loader
     with that name reads the file, overriding everything else. FURNSH
     makes no check of the file (that it exists, what it is, its
     extension); the loader reports its own errors.
  2. Else SPICE's binary kernels (DAF, DAS), by their ID word: PEPPER
     loads them; they are never given to a loader by extension or
     detection.
  3. Else the longest registered extension that ends the file name.
  4. Else the loaders' `detect`, the most recently registered first.
  5. Else the text and JSON readers, as before.

  `loader='PEPPER'` is PEPPER's own loading (steps 2 and 5, registered
  loaders not asked): for a SPICE text kernel or meta-kernel whose
  extension a loader also has. PEPPER cannot be a loader's name; any other
  name no loader has is SPICE(UNKNOWNLOADER).
- **`kernel_format(file, loader, format, found)`** tells which loader read a
  kernel, and the builder's description (`set_format`).
- **A file that holds a model,** not states (elements and a propagator, say):
  the loader reads it into a provider of its own (an `ephemeris_provider`,
  as `add_ephemeris` takes) and adds it with `kb%add_ephemeris(ctx, body,
  center, frame, tbeg, tend, provider)`. Queries call the provider's
  `state` at any epoch. It belongs to the kernel: SPKCOV and SPKOBJ report
  it by the kernel's name, UNLOAD and KCLEAR remove it, and kernels of the
  same body coexist (the last loaded first; unloading it restores the
  earlier), where `add_ephemeris` replaces the body's earlier registration.
  `write_kernel` refuses such a kernel (there are no states to write).
- **Frames a kernel defines:** `kb%frame_rotations(ctx, name, code,
  center, base, epochs, quats, degree)` (or 3x3 matrices) gives a new
  frame from rotations at ET epochs relative to a base frame (e.g. J2000),
  interpolated by SPICE's CK type 5 routines on a table held in memory (no
  clock); `kb%add_frame(ctx, name, code, center, provider)` a frame
  computed by code. Both are defined while the kernel is loaded; loaders use
  them for files of frame data. CKCOV and CKOBJ (`ck_coverage`,
  `ck_objects`) report the tables by the kernel's name, and
  `write_kernel(name, file, ck_file, text_file)` writes them as a CK with a
  text kernel of their clocks and frame definitions, which SPICE loads. See
  the guide's "Reference frames".
- **Body names a kernel defines:** `kb%define_body(ctx, name, code)`. While
  the kernel is loaded, the name works everywhere a body name does
  (`spkezr('SPACECRAFT1', ...)`, BODN2C, BODC2N); when it is unloaded
  (UNLOAD, KCLEAR, replaced) the definition goes with it, and an earlier one
  of the same name (BODDEF's, another kernel's) is in effect again. The
  latest definition wins, and names from the kernel pool
  (`NAIF_BODY_NAME`/`CODE`) take precedence, as over BODDEF's. (KCLEAR keeps
  the program's own BODDEF names, as in SPICELIB.)
- **Only SPICE's interpolations.** A loader maps the file's method to type 9
  or 13, or signals an error; it does not substitute another method.

## Results of any size

SPICELIB makes the caller size every output array, and fails (or truncates)
when the result is larger. For these routines PEPPER has variants that return
allocatable results of the size of the data. The SPICELIB forms stay, with
their arguments unchanged.

| SPICELIB | PEPPER |
|---|---|
| `gdpool`, `gipool`, `gcpool` | `pool_values(name, values, found)`, `pool_integers`, `pool_strings` |
| `bodvrd`, `bodvcd` | `body_values(body, item, values)` (body by name or code) |
| `getfov`, `getfvn` | `fov(inst, shape, frame, bsight, bounds)` (instrument by code or name) |
| `bodc2n`, `frmnam`, `srfc2s` | `body_name(code, name, found)`, `frame_name(code, name, found)`, `surface_name(code, body, name, found)` |
| `kdata`, `kinfo` | `kernel_data(which, kind, file, filtyp, source, handle, found)`, `kernel_info(file, filtyp, source, handle, found)` |
| `spkcov`, `ckcov`, `pckcov` | `spk_coverage(file, body, intervals)`, `ck_coverage(file, inst, needav, level, tol, timsys, intervals)`, `pck_coverage(file, frame, intervals)` |
| `spkobj`, `ckobj`, `pckfrm`, `dskobj`, `dsksrf` | `spk_objects(file, ids)`, `ck_objects`, `pck_frames`, `dsk_objects`, `dsk_surfaces(file, body, ids)` |
| `dskp02`, `dskv02` | `dsk_plates(handle, dladsc, plates)`, `dsk_vertices(handle, dladsc, vertices)` |
| `dafec`, `dasec` | `daf_comments(handle, lines)`, `das_comments(handle, lines)` |
| `ekrced`, `ekrcei`, `ekrcec` | `ek_values(handle, segno, recno, column, values, isnull)` (real, integer or string values) |
| `limbpt`, `termpt` | `limb_points(...)`, `terminator_points(...)`: the inputs without `maxn` |
| `gfdist`, `gfposc`, `gfsubc`, `gfsntc`, `gfsep`, `gfoclt`, `gfilum`, `gfpa`, `gfrr`, `gftfov`, `gfrfov`, `gfuds`, `gfudb`, `gfevnt` | `gf_distance`, `gf_position`, `gf_subpoint`, `gf_intercept`, `gf_separation`, `gf_occultation`, `gf_illumination`, `gf_phase`, `gf_range_rate`, `gf_target_in_fov`, `gf_ray_in_fov`, `gf_user_scalar`, `gf_user_boolean`, `gf_event`: the inputs without `mw`, `nw` and `work` |

Strings are `character(:), allocatable`, lists of strings `type(string)`
arrays (`pepper_containers`). A window (a coverage, a GF result or
confinement window) is an array `intervals(2, n)`, each column an interval
[start, stop], sorted and disjoint; a set of IDs is a sorted integer array.
`pepper_windows` converts them to and from SPICELIB's cells
(`window_to_cell`, `cell_to_window`, `set_to_cell`, `cell_to_set`) for the
cell, set and window routines. After an error the results are empty (size 0).

```fortran
real(wp), allocatable :: radii(:), cover(:,:), result(:,:)

call ctx%body_values('EARTH', 'RADII', radii)
call ctx%spk_coverage('de440.bsp', 301, cover)
call ctx%gf_distance('MOON', 'NONE', 'EARTH', '<', 4.0e5_wp, 0.0_wp, 3600.0_wp, cover, result)
do i = 1, size(result, 2)
   print *, result(:, i)
end do
```

## Python: PepperyPy

[![Language](https://img.shields.io/badge/-Python-3776AB?logo=python&logoColor=white)](https://github.com/topics/python)

[PepperyPy](python/README.md) is PEPPER's Python interface, with
[SpiceyPy](https://github.com/AndrewAnnex/SpiceyPy)'s API: SpiceyPy programs
run with `import pepperypy as spice`. Queries from several threads run in
parallel, through a context per thread; instances hold several kernel sets;
prepared queries evaluate arrays of epochs in one call; GF searches and
programmatic bodies and frames take Python functions.

```python
import pepperypy as spice

spice.furnsh('kernels.tm')
state, lt = spice.spkezr('MOON', spice.str2et('2026 NOV 8'), 'J2000', 'LT+S', 'EARTH')

ctx = spice.current_context()                       # this thread's
states, lts = ctx.prepare_state('MOON', 'EARTH', 'J2000', 'LT+S')(ets)   # (n, 6), (n,)
positions, lts = ctx.prepare_position('MOON', 'EARTH', 'J2000', 'LT+S')(ets)   # (n, 3), (n,)
```

It is Cython extension modules over PEPPER's C interface, generated from
the Fortran interfaces; a call costs 0.2 to 0.8 µs, 3 to 5 times less than
SpiceyPy's. It has 88% of SpiceyPy's functions, and passes 523 of the 639
tests of SpiceyPy's own suite. See [python/README.md](python/README.md), the coverage of
SpiceyPy's functions in [python/COVERAGE.md](python/COVERAGE.md), and the
design in [plans/pepperypy_plan.md](plans/pepperypy_plan.md).

## The C interface

[![Language](https://img.shields.io/badge/-C-A8B9CC?logo=c&logoColor=white)](https://github.com/topics/c)
[![Language](https://img.shields.io/badge/-C%2B%2B-00599C?logo=cplusplus&logoColor=white)](https://github.com/topics/cpp)

`include/pepper.h` declares PEPPER's C interface (`src/capi`): an instance
and its contexts as opaque pointers, and every routine of a context whose
arguments C can pass (about 830) as `pepper_<name>(ctx, ...)` with
SPICELIB's arguments. Every routine returns 1 if an error was signaled
(`pepper_getmsg`, `pepper_reset`); strings are NUL-terminated; results of
any size stay in the context's buffers; matrices are column-major. A
program whose main is not Fortran calls `pepper_runtime_init()` first
(with gfortran, `-DPEPPER_GFORTRAN`). The shims, the header and PepperyPy's
wrappers are generated by `tools/gen_capi.py`; `capi/test_capi.c` is a C
example and test. Kernels built in memory have a builder of their own
(`pepper_builder_new`, `pepper_builder_spk`, `_add_ephemeris`,
`_frame_rotations`, `_add_frame`, `_define_body`,
`pepper_spice_load_kernel`), and kernel loaders written in C are registered
with `pepper_spice_add_loader` (`pepper_spice_furnsh_loader` names one).

```c
#include "pepper.h"

pepper_runtime_init();
void *spice = pepper_spice_new();
void *ctx = pepper_context_new(spice);          /* one per thread */
pepper_spice_furnsh(spice, "kernels.tm", -1);
double et, state[6], lt;
pepper_str2et(ctx, "2026 NOV 8 12:00", &et);
if (pepper_spkezr(ctx, "MOON", et, "J2000", "LT+S", "EARTH", state, &lt)) {
    char msg[1024];
    pepper_getmsg(ctx, "LONG", sizeof msg, msg);
    pepper_reset(ctx);
}
pepper_context_free(ctx);
pepper_spice_free(spice);
```

### Other projects

* [NAIF SPICE Toolkit](https://naif.jpl.nasa.gov/naif/toolkit.html) -- The original SPICE toolkit from NASA's NAIF
* [rust-spice](https://github.com/GregoireHENRY/rust-spice) -- CSPICE interface from Rust
* [SpiceyPy](https://github.com/AndrewAnnex/SpiceyPy) -- CSPICE interface from Python
* [ANISE](https://github.com/nyx-space/anise) -- A thread-safe alternative to NASA SPICE toolkit (Rust)

