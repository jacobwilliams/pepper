# Accelerations from the ephemeris data

Get the acceleration of a body relative to another body from the stored ephemeris
data (the derivative of the interpolating polynomials, or of the analytic model
the segment stores), not by differencing velocities. SPICELIB has nothing like
it: every SPK evaluator stops at the state.

Status: design. No code yet.

## Goal

```fortran
call ctx%spkacc ( 'MOON', et, 'J2000', 'NONE', 'EARTH', state, lt )
! state(1:3) position (km), state(4:6) velocity (km/s), state(7:9) acceleration (km/s^2)
```

- The acceleration comes from the same record, the same interpolant and the same
  frame chain as the state. It is consistent with the returned velocity, and it
  costs about one evaluation, not three.
- Where the data cannot give an acceleration (SGP4 elements, frames without a
  second derivative), it signals an error. It never silently falls back to
  finite differences.
- A query that does not ask for an acceleration costs what it costs today.

## How states are computed now

- **SPKGEO** ([pepper_spk_api.f90](../src/pepper_spk_api.f90)) finds a segment for
  each link of the chain from the target to a common center, and from the
  observer to it. It does this with SPKSFS, then SPKPVN.
  - It converts each link's state to the next link's frame. For inertial frames
    it uses IRFROT and MXV (rotation only). Otherwise it uses FRMCHG and MXVG
    (the 6×6 state transformation).
  - It adds the states.
- **SPKPVN** ([pepper_orientation.f90](../src/pepper_orientation.f90)) unpacks the
  descriptor and calls the type's reader, `spkrNN`, then the type's evaluator,
  `spkeNN`, in [pepper_records.f90](../src/pepper_records.f90).
  - Type 100 is the programmatic provider (`provided_state`,
    [pepper_programmatic.f90](../src/pepper_programmatic.f90)).
- **SPKEZR, SPKEZ and SPKACS** add light time and stellar aberration (SPKLTC).
- **Prepared queries** (`prepare_state`, `query_state_one`, `query_state_batch`)
  cache the segments of a chain and repeat the same steps.
- **FRMCHG** ([pepper_transforms.f90](../src/pepper_transforms.f90)) composes
  6×6 transformations (ZZMSXF) along the frame tree. FRMGET supplies each
  link, by frame class: inertial, PCK, CK, TK, dynamic, switch or programmatic.

## Design

### Representation: the 9×9 transformation

To change the frame of a position, velocity and acceleration (r, v, a) with
r′ = R r:

```
v′ = Ṙ r + R v
a′ = R̈ r + 2 Ṙ v + R a
```

So (r′, v′, a′) = T (r, v, a), with the block lower-triangular matrix

```
     | R    0    0 |
T =  | Ṙ    R    0 |
     | R̈   2Ṙ    R |
```

This extends SPICELIB's 6×6 state transformation the same way, with two
properties that make it easy to adopt:

- **Composition is the matrix product.** The product of two such matrices has
  the same form: (R₁R₂)¨ = R̈₁R₂ + 2Ṙ₁Ṙ₂ + R₁R̈₂. FRMCHG's chain logic carries
  over unchanged, with 9×9 matrices instead of 6×6 (ZZMSXF already takes `n`).
- **The inverse is the blockwise transpose** (Rᵀ, Ṙᵀ, R̈ᵀ in the same places),
  just as INVSTM inverts a 6×6. No general inversion is needed.

It is stored as `real(wp) :: xform(9,9)`. The leading 6×6 block is exactly
FRMCHG's result.

### Plumbing: optional arguments, a new public routine

| Level | Change |
|---|---|
| `spkeNN` | new `real(wp), intent(out), optional :: acc(3)`; the extra work only `if (present(acc))` |
| SPKPVN | the same optional `acc(3)`, passed through; types without an acceleration signal an error only when `acc` is present |
| `provided_state` | the same, through the provider's new `acceleration` method (below) |
| SPKGEO | optional `acc(3)`; when present, the frame links use the 9×9 transformation (FRMCHG2) |
| new **SPKACC** (names, like SPKEZR) and **SPKACCI**, or an `acc` optional on SPKEZ (codes) | `state(9)`, `lt`; ABCORR `'NONE'` in phase 3, the light-time corrections in phase 4 |
| prepared queries | `query_state_one` / `query_state_batch` gain an optional `accs` (shape `(3, n)`) |
| C API, PepperyPy | `spkacc` (generated; gen_capi skips optional arguments, so a plain routine is the way in), vectorized over `et` like `spkezr` |

**Why optional arguments internally:**

- The SPICELIB signatures and the `src/legacy` wrappers stay as they are.
- There is one evaluator per type, not two copies that could drift apart.
- When the argument is absent, all it costs is a `present()` test.

Check this with `py-bench` before and after
([[no-runtime-size-results-hot-paths]]: SPK evaluation is a hot path).

**Why a separate public routine:**

- SPKEZR's results stay exactly SPICELIB's.
- The C API and PepperyPy get a plain signature.

`acc` is `intent(out)`. It is set on every successful return.

### Errors

The new short message is `SPICE(NOACCELERATION)`. Its long message names the
reason:

- the SPK type and segment (types 10, 15 and 17 until they are done, and
  providers without an acceleration method);
- the frame and its class (frame classes without a second derivative);
- the aberration correction (any correction other than `NONE` before phase 4).

Add it to the PepperyPy exception table.

## Per SPK type

Notation:
- t: the epoch;
- for a Chebyshev record, s = (t − mid)/radius is the scaled time, so
  d/dt = (1/radius)·d/ds;
- primes are derivatives in s.

| Type | Stored | Acceleration | New code |
|---|---|---|---|
| **1, 21** (MDA: modified difference arrays) | the acceleration's difference table `dt`, integrated twice in [spke01](../src/pepper_records.f90) | one more pass of the `w` recurrence, after the velocity's: `acc(i) = Σ_j dt(j,i)·w(j+ks)` at the zeroth integration level. Exact: it is the integrator's own acceleration. Derive the form from Shampine & Gordon, then check it against a central difference | small |
| **2** (Chebyshev, position) | position coefficients | `p''(s)/radius²`: [chbder](../src/pepper_math.f90) with `nderiv = 2` instead of CHBINT | small |
| **3, 14** (Chebyshev, position and velocity) | two independent series | the derivative of the velocity series: CHBINT on the velocity coefficients (`dpdx/radius`) instead of CHBVAL | small |
| **20** (Chebyshev, velocity; position by integration) | velocity coefficients, scaled by DSCALE/TSCALE | the derivative of the velocity series, with the same scaling as the velocity. CHBIGR has no derivative: add a CHBINT call | small |
| **5** (two conics blended by a cosine weight) | two states and GM | aᵢ = −GM rᵢ/\|rᵢ\|³ for each conic, then `w a₁ + (1−w) a₂ + 2ẇ (v₁−v₂) + ẅ (r₁−r₂)`, where ẅ = −½ cos(arg)·(dargdt)² (`arg` is linear in t; `w` and `dwdt` are already there) | small |
| **8** (Lagrange, equal spacing) | positions and velocities | the derivative of the velocity interpolant: LGRIND on components 4–6, with the x values `begin + (j−1)·step` built in a work array, or a new LGRDSP (LGRESP with a derivative) | small |
| **9, 18 subtype 1, 19 subtype 1** (Lagrange, unequal) | positions and velocities | LGRIND on components 4–6 (instead of LGRINT) | small |
| **12** (Hermite, equal spacing) | positions and velocities; velocity = the derivative of the position Hermite | the second derivative of the position Hermite: new **HRMESP2** | medium |
| **13, 19 subtype 2** (Hermite) | the same | new **HRMINT2** | medium |
| **18 subtype 0, 19 subtype 0** (Hermite, 12-element packets) | position, velocity; velocity, acceleration | the derivative of the velocity Hermite. It is already computed and discarded (`vbuff(4:6)` in SPKE18/SPKE19) | trivial |
| **10** (SGP4/TLE) | mean elements | none: SGP4 has no usable analytic derivative here | error |
| **15** (precessing conic) | elements, J2 precession rates | analytic: the two-body acceleration rotated by the two precessions, plus the product-rule terms of the rotation rates | phase 5 |
| **17** (equinoctial elements) | elements and secular rates | analytic (EQNCPV's second derivative) | phase 5 |
| **100** (programmatic provider) | callback | the provider's `acceleration` method (below) | small |

**HRMINT2 and HRMESP2.** HRMINT computes the value and first derivative of the
Hermite interpolant with a Neville-like recurrence over a work array of size
2n×2. HRMINT2 adds a third set of work columns: it carries the second
derivative through the same recurrence (each level's update is a linear
combination with coefficients linear in x, so its derivative is one more term).

**Dimensions.** Check every new work array against MAXREC, as the existing
evaluators do.

**Programmatic providers.**
- `ephemeris_provider` gains a non-deferred type-bound procedure
  `acceleration(self, ctx, body, et, state, acc, found)`. Its default sets
  `found = .false.`, which gives the error.
- Plain-procedure providers (`ephemeris_function`) can register an optional
  second procedure.
- PepperyPy's provider class gets an optional `acceleration` method.

### Meaning of the result, to document

- **Types 2, 12, 13 and 19 subtype 2:** the acceleration is the second
  derivative of the position fit. It is less accurate than the position (each
  derivative loses accuracy, Chebyshev series most near the ends of the
  interval).
- **Types 3, 8, 9, 14, 18 and 19 subtypes 0 and 1:** it is the derivative of
  the velocity fit. It is consistent with the returned velocity, not
  necessarily with d²(position).
- **Types 1 and 21:** it is the integrator's acceleration at the fit points,
  interpolated: the best case.
- **The acceleration is discontinuous at record and segment boundaries.** Fits
  are generally made continuous in position, and sometimes in velocity, not in
  acceleration. The record chosen is the state's record, by the same selection
  rules.

## Frames: FRMCHG2

`frmchg2(ctx, frame1, frame2, et, xform9)`, and `sxform2` with frame names, are
FRMCHG and SXFORM with 9×9 transformations. FRMGET's per-class links become:

| Class | Link | Phase |
|---|---|---|
| Inertial | R from IRFROT; Ṙ = R̈ = 0 | 2 |
| TK (fixed offset) | R constant; Ṙ = R̈ = 0 | 2 |
| PCK binary (types 2, 3, 20: Euler angles) | angles, rates and second derivatives from the record (type 2: the angle series to order 2 with CHBDER; types 3 and 20: the derivative of the rate series), then **EUL2XF2** | 2 |
| PCK text (TISBOD: RA, DEC, W polynomials and nutation-precession terms) | the second derivatives of the polynomial and trigonometric terms (TISBOD already differentiates them once), then EUL2XF2 | 2 |
| Switch | the active base frame's link | 2 |
| CK | type 2 (constant angular velocity over each interval): R̈ = Ω²R, with Ω the cross-product matrix of the rate. Types 3–6: the derivative of the interpolated angular velocity, or the second derivative of the quaternion interpolant | 5 |
| Dynamic (ZZDYNFRM) | of-date frames (precession and nutation models) and Euler frames: analytic second derivatives. Two-vector frames: need the jerk of the defining vectors (the third derivative) when a defining vector is a velocity | 5 |
| Programmatic frames | the frame provider's optional `transform2` (9×9); otherwise an error | 2 (error), 5 |

**EUL2XF2.** This is EUL2XF with second derivatives. R = A(θ₁) B(θ₂) C(θ₃),
where each factor is a rotation about one axis:

- d/dt Rₖ(θ) = θ̇·Rₖ′(θ)
- d²/dt² Rₖ(θ) = θ̈·Rₖ′(θ) + θ̇²·Rₖ″(θ)

where Rₖ′ and Rₖ″ are the derivatives of the axis rotation in its angle
(sines and cosines). Then apply the product rule to the three factors.

**Dynamic frames (ZZDYNFRM).** A dynamic frame's orientation is computed at
run time from a definition in a frame kernel. There are three styles, and they
differ in how hard R̈ is:

- **Of-date frames** (mean or true equator and equinox of date, mean ecliptic
  of date). R is built from model angles that are explicit functions of time:
  the IAU 1976 precession angles (ZZEPRC76, polynomials in t), the IAU 1980
  nutation (ZZENUT80, trigonometric series) and the mean obliquity (ZZMOBLIQ,
  a polynomial). These routines already differentiate their angles once to
  give the 6×6. Differentiating once more is mechanical (polynomial and
  sine/cosine terms), and then EUL2XF2 (or the product rule on the precession,
  nutation and obliquity rotations) gives the 9×9. Nothing outside the frame
  is needed.
- **Euler frames.** The three angles are polynomials in (t − epoch) whose
  coefficients are in the kernel. ZZDYNFRM evaluates them with POLYDS to order
  1. Evaluating to order 2 gives θ̈, and EUL2XF2 does the rest.
- **Two-vector frames.** R is built from two defining vectors v₁ (primary) and
  v₂ (secondary): one axis is along v₁, another is along v₁ × v₂, and the third
  completes the right-handed set (ZZTWOVXF). Ṙ depends on v₁, v₂ and their
  first derivatives, and R̈ also needs their second derivatives. What those
  derivatives are depends on the kind of each vector:
  - *Position* (observer-to-target): the first derivative is a velocity and the
    second is an acceleration, which phases 1 and 3 provide. This kind is
    straightforward.
  - *Velocity* (the target's velocity relative to the observer, for example
    the secondary vector of a radial/along-track frame): the first derivative
    is an acceleration and the second is the **jerk**, the third derivative of
    position. No SPK evaluator provides jerk, and the fits are not accurate
    enough to give a usable third derivative. SPICE already uses a numerical
    derivative here: ZZDYNFRM gets the derivative of the unit velocity vector
    by a central difference (QDERIV) of SPKEZ velocities at t ± δ, so even the
    6×6 is not analytic for this kind. With accelerations available, the first
    derivative could be computed analytically from v and a, but the second
    would still need a difference of accelerations. That conflicts with the
    rule that numerical differentiation is never the default.
  - *Near point* (the point on the target body's surface nearest the
    observer): R̈ needs the second derivative of the near point, which needs
    the observer's acceleration and the second derivative of the near-point
    geometry (DNEARP gives only the first).
  - *Constant* (fixed in some frame): its derivatives come from the R̈ of that
    frame, recursively.

  Defining vectors can also carry aberration corrections, which need phase 4.
  For these reasons two-vector frames come last.

**SPKGEO.**
- Inertial links still use IRFROT, because R̈ = 0: the acceleration rotates
  like the position.
- Other links use FRMCHG2 when `acc` is present.

**Not in this plan:** a numerical fallback for frames without a second
derivative (a central difference of the exact Ṙ). If wanted later, it must be
an explicit opt-in (an argument or a context setting), never the default. See
the open questions.

## Aberration corrections (phase 4)

The goal is to accept the same ABCORR values as SPKEZR (`LT`, `LT+S`, `CN`,
`CN+S` and the transmission `X` forms).

Light time, reception case. τ = t − lt(t), and the apparent position is
p(t) = r_t(τ) − r_o(t). Then:

```
ṗ = v_t(τ)(1 − l̇t) − v_o
p̈ = a_t(τ)(1 − l̇t)² − v_t(τ) l̈t − a_o
```

l̇t comes from SPKLTC (`dlt`). l̈t comes from differentiating
c·lt = |r_t(τ) − r_o(t)| twice. That needs the target's acceleration at τ,
which this feature provides.

Stellar aberration: the second derivative of STELAB/STLABX's correction. It
needs the observer's acceleration and is the most involved part. If it gets
too complicated, accept `LT` and `CN` and reject `+S` with
`SPICE(NOACCELERATION)`.

## Phases

1. **Evaluators.**
   - The `acc` optional on every `spkeNN` listed above except 10, 15 and 17.
   - HRMINT2 and HRMESP2.
   - SPKPVN pass-through and errors.
   - The provider's `acceleration` method.
   - Tests per type (below).
2. **Frames.**
   - The 9×9 helpers: multiply (ZZMSXF already takes `n`), invert by blockwise
     transpose, build from R (inertial or TK).
   - EUL2XF2.
   - FRMGET2 and FRMCHG2 for the inertial, TK, PCK (binary and text) and
     switch classes; an error for CK, dynamic and programmatic frames.
   - SXFORM2.
   - Tests against a central difference of FRMCHG's 6×6.
3. **Chain and API (geometric).**
   - `acc` on SPKGEO.
   - SPKACC/SPKACCI (ABCORR `'NONE'` only).
   - `query_state_one` / `query_state_batch` `accs`.
   - gen_capi (the C API) and PepperyPy (`spkacc`, vectorized).
   - `py-bench` before and after.
   - Documentation (fdoc), the README feature list, the PepperyPy README.
4. **Light time**, then stellar aberration.
5. **Later, if wanted:**
   - types 15 and 17;
   - CK frames;
   - dynamic frames (of-date and Euler first, two-vector frames last);
   - programmatic frames' `transform2`;
   - an opt-in numerical fallback.

Each phase ends with `pixi run test`, `pixi run tspice`, `pixi run tspice-release`,
`pixi run test-parallel` and (phases 3–5) `pixi run -e python py-test`.

## Tests

The core check uses trajectories that the fits reproduce exactly.

- **Exact trajectories.** For each type with a writer, write a segment
  (`spkw01`, `spkw02`, `spkw03`, `spkw05`, `spkw08`, `spkw09`, `spkw12`,
  `spkw13`, `spkw14*`, `spkw18`, `spkw19`, `spkw20`, `spkw21`) from a trajectory
  the type represents exactly:
  - a polynomial in t of degree below the interpolation degree, for the
    Lagrange, Hermite and Chebyshev types;
  - a two-body orbit, for type 5.

  The acceleration must then match the analytic acceleration to rounding.
- **Types 1 and 21:** difference lines made from a known polynomial
  acceleration, and comparison with that acceleration.
- **Real kernels:** for each type present in the tspice and test kernels (DE
  files: type 2; spacecraft: types 13, 19, 21), compare with a central
  difference of the velocity, `(v(t+h) − v(t−h))/2h`, with h well inside one
  record. Agreement within the difference's truncation error is expected,
  not equality.
- **Physical check:** the Moon relative to the Earth from a DE file. The
  acceleration ≈ −μ_E r/r³ plus the solar term, within the sum of the omitted
  terms.
- **Boundaries:** epochs at record and segment boundaries pick the state's
  record. The acceleration's jump there is reported, not hidden.
- **Frames:**
  - the 9×9 transformation of FRMCHG2 against a central difference of FRMCHG's
    6×6 (ITRF93 from a binary PCK, IAU_EARTH and IAU_MOON from a text PCK, a TK
    frame on top of each);
  - T·T⁻¹ = I;
  - the composition of the chain matches the direct transformation.
- **End to end:** SPKACC in a rotating frame (Moon from Earth in ITRF93)
  against a central difference of SPKEZR's velocity.
- **Errors:** types 10, 15 and 17, a CK frame, a dynamic frame, ABCORR `'LT'`
  (before phase 4) and a provider without an acceleration all signal
  `SPICE(NOACCELERATION)`.
- **Threads:** add SPKACC to the test-parallel queries.
- **PepperyPy:** `spkacc` scalar and vectorized, its errors, and agreement with
  the Fortran.

## Risks

- **The MDA second derivative (types 1 and 21).** The recurrence is subtle.
  Derive it from the source formulas, and check it against an exactly known
  case before trusting it.
- **Hot-path cost.** Optional arguments in SPKPVN and the evaluators must not
  slow SPKEZR. Measure with `py-bench`. If they do, move the acceleration
  versions into separate routines that share the record reading.
- **FRMCHG2 duplicates FRMCHG's chain search.** Prefer one implementation
  parameterized by the dimension (6 or 9), not a copy, so the two cannot
  diverge.
- **Users may expect smooth accelerations.** The documentation must say that
  the acceleration of a fit is only as good as the fit, and jumps at record
  boundaries.

## Open questions

1. **Names:** SPKACC/SPKACCI (or `spkezr_acc`)? An optional `acc` on SPKEZR
   itself, in addition?
2. **Legacy interface:** add `SPKACC` to `src/legacy` (a non-SPICELIB routine
   with a SPICELIB-style interface), or keep it a context method only?
3. **Numerical fallback:** never, or as an explicit opt-in for CK and dynamic
   frames and for types 10, 15 and 17?
4. **Stellar aberration:** support it (phase 4), or support light time only?
5. **Should the 9×9 transformation be public** (`sxform2`, like SXFORM): the
   frame's angular acceleration is useful on its own?
