# Bugs and inconsistencies found in SPICELIB (N0067)

Found while moving SPICELIB into modules (explicit interfaces, intents, per-context
state). Each entry says what is wrong in N0067, how it shows, and what PEPPER does.
Unless noted, the behavior of a correct program is unchanged, and the tspice tests
pass as with SPICELIB.

Kinds:
- **code**: the code does something wrong or relies on undefined behavior;
- **interface**: an argument is passed against its declaration (works with implicit
  interfaces and sequence association);
- **docs**: the header (Brief_I/O table, argument names) disagrees with the code.

## Code

| Routine | Problem | PEPPER |
|---|---|---|
| GETFVN | `INSTID` is declared DOUBLE PRECISION, but passed to integer arguments (ZZBODS2C, GETFOV). It works only because a little-endian machine keeps the integer in the double's first bytes. | Declared INTEGER. |
| ZZNRDDP (ZZDPPER) | `PE0`, `PINC0`, `PL0`, `PGH0`, `PH0` are never assigned (since the 2011 removal of DOPERT) and are subtracted, relying on saved storage being zero. `PREEP` is compared before it is first set. | All saved state is set to zero (blank, false) when it is created (`tools/zero_state.py`), as static storage was in practice. |
| SCS2E (and SCTK01's copy) | Finds the partition with `DO WHILE ( PART .LE. NPARTS .AND. ( TICKS .LT. PSTART(PART) .OR. ... ) )`: Fortran's .AND. does not short-circuit, so when no partition holds TICKS it reads PSTART(NPARTS+1) and PSTOP(NPARTS+1), past the partitions (inside the MXPART-sized arrays, so unnoticed). Found by bounds checking (tspice F_SCLK0). | A counted loop that stops at NPARTS (PEPPER's arrays have exactly NPARTS elements). |
| WNINSD (and ZZWNINSD's copy), WNCOMD, LSTCLD, LSTCLI | The same non-short-circuit guards: `DO WHILE ( J .LE. CARD .AND. WINDOW(J) .LE. WINDOW(I) )`, `IF ( J .LE. CARD .AND. WINDOW(I) .GE. WINDOW(J-1) )` (WNINSD's merge), `WINDOW(I-1)`, `WINDOW(I)` and `WINDOW(I+1)` past CARD in WNCOMD, `ARRAY(N+1)` in LSTCLD and LSTCLI. With a full window (cardinality = size) they read past the cell; the results do not depend on the values read. The windows are assumed-size, so bounds checking cannot see it; test_window_bounds puts signaling NaNs past full windows. | The index is tested before the element is read. |
| BODEUL | Resets NTHETA but not NPHASE when the body has no NUT_PREC_ANGLES: NPHASE keeps the previous body's count (saved), and the loop over the phase angles reads TCOEF past its NTHETA values. The results do not change (NA = ND = NW = 0), but the reads are past the data; TISBOD resets both. Found by bounds checking (tspice F_RDPCK, F_RDPCKD, F_PCKBUF). | NPHASE reset with NTHETA. |
| DAFPS | Packs the integers through an EQUIVALENCE with a local array and copies `(NI-1)/2+1` words: with NI odd, the second half of the last word is whatever the local array held (undefined); with NI = 0 it still copies one (undefined) word, past the summary's ND + (NI+1)/2 words. | Packs with TRANSFER: the half word is zero, and nothing is written for NI = 0. DAFUS unpacks with TRANSFER too. |
| ZZDSKSEL | A LOGICAL FUNCTION whose ENTRY points (e.g. ZZDSKSBD) are invoked with CALL (in ZZDSKSPH). Invalid Fortran that F77 compilers tolerated. | The value is referenced in an IF (`if ( zzdsksbd(...) ) continue`). |
| (many: REPMC, REPMI, REPMD, UCASE, ZZINSSUB, NEXTWD, CMPRSS, KXTRCT's SHIFTL, ...) | About 500 calls pass one variable as both the input and the output string, e.g. `CALL REPMC ( MSG, '#', X, MSG )`, and the routines are written to survive it (ZZINSSUB compares `OUT` with `IN`; REPSUB, ZZREPSUB and RJUST move characters right to left). Fortran forbids it: an argument that is changed may only be reached through its own dummy, and the compiler may optimize on that (gfortran treats such dummies as not overlapping; with module procedures it can inline and reorder or vectorize the copies). gfortran's `-Wall` reports it ("Same actual argument associated with INTENT(IN) argument ... and INTENT(OUT) argument"). | The library calls in-place variants with one `intent(inout)` string (`repmc_inplace`, `repmi_inplace`, `ucase_inplace`, ... in pepper_strings); the few others use substring assignment (`str = adjustr(str)`, `wordsq(berase:) = wordsq(eerase+1:)`) or pass the message template directly. `test/core/test_strings_inplace.f90` checks the variants against the originals. The original routines are unchanged, so code using the legacy interface (tspice) still relies on it. |
| ZZREPSUB | With `RIGHT > LEN(IN)` the count of tail characters, `MIN(REMAIN, INLEN-RIGHT)`, is negative (the clamped `MYRGHT` is computed but not used), so `END` lands before position 1 and `OUT(END+1:) = ' '` writes blanks before `OUT`; with `LEFT < 1` and `RIGHT >= LEFT-1` it reads `IN` before position 1. Callers pass valid ranges. | Unchanged in ZZREPSUB; `zzrepsub_inplace` clamps both ends. |
| LIMBPT | Its room check compares each cut's number of limb points with MAXN, never with the room left (`ROOM` is set to MAXN and not decremented): when no single cut has more than MAXN points but all of them together do, it writes past the end of POINTS, EPOCHS and TANGTS. Only DSK shapes have several points in a cut. | `limbpt` checks the running total (SPICE(OUTOFROOM)); `limb_points` has no MAXN (phase 7, `plans/limits.md`). Test: `test_outputs` (two octahedra). |
| TERMPT | Subtracts a cut's count from `ROOM` once per point of the cut (the statement is inside the loop over the points), so a cut with k points uses k² of the room: a DSK search with several points in a cut signals SPICE(OUTOFROOM) with room to spare. | `termpt` checks the running total; `terminator_points` has no MAXN. |
| ZZGFCOU (ZZGFCOCD, ZZGFCOSD) | The error message on an unreachable path uses `CRDSYS`, an argument of another entry point (ZZGFCOIN), not of these. | Uses the saved `SVCSYS`. |
| ZZGFOCU (ZZGFOCIN) | The back body's shape/method string is parsed (ZZPRSMET) with the front body's ID `IDFRNT`, not `IDBACK`. Affects only the parsing and checking of surface names in the back body's DSK method string. | Unchanged (faithful). |
| ZZGFRPWK | `LS` (length of the report's begin message) is computed only by ZZGFTSWK; ZZGFWKAD replaces the begin message without updating it, so ZZGFWKIN's messages can be cut or padded. | Unchanged (faithful). |
| ZZGFREL, ZZGFRELX | `CSTEP` is saved but never assigned (it keeps its DATA value .FALSE.). Harmless. | Unchanged. |
| GFEVNT | `DREF` is set only when a DREF parameter is supplied; otherwise it keeps the value from an earlier call. | Unchanged. |
| NAMFRM (FRAMEX) | Kernel frame names are added to the name cache as `FRAME_<NAME>` (after PREFIX) but looked up as `<NAME>`, so the cache never hits and every lookup reads the pool. Results are correct. | Fixed: cached under the name (`plans/limits.md`, phase 4). |
| CCIFRM (FRAMEX) | In the loop over a batch of `FRAME_*_CLASS` names from GNPOOL, GIPOOL overwrites `N`, the batch size, with 1. The next batch then starts at `START + 1`, not `START + N`, so each name is examined up to 100 times, and each GNPOOL call scans the whole pool. Results are correct. | Fixed: the names are fetched once (`plans/limits.md`, phase 4). |
| CCIFRM (FRAMEX) | The name variable is built with `REPMC ( KVBUFF(I), '_CLASS', '_NAME', LOOK2 )`, which replaces the first `_CLASS` in `FRAME_<ID or name>_CLASS`. A frame defined by a name that contains `_CLASS` gets the wrong variable. | Fixed: the trailing `_CLASS` is replaced. |
| UDF | Assigns `X = X + 0` and `VALUE = VALUE + 0` to its inputs (to use them). | Removed. |
| GFSTEP | Assigns `TIME = TIME` to its input. | Removed. |
| ZZCKCV05 | Passes the DP word `BUFFER(1)` to ERRINT. | Passes the integer subtype. |
| SGBWFS | Passes the array `PKTSIZ` to ERRINT. | Passes `PKTSIZ(1)`. |
| STMP03 | Truncates its series at 10 terms with Intel Fortran on Windows (`#if`), 11 elsewhere. | 11 everywhere. |
| POOL | Message text "an entry which performs performs no run-time function". | Unchanged (faithful). |
| ET2LST | Signals `SPICE(CANTGETROTATIONTYPE)`, 26 characters; short messages hold 25, so it is stored as `SPICE(CANTGETROTATIONTYPE` (without the closing parenthesis). | Fixed: messages have no length limit (`plans/limits.md`, phase 1). |
| ZZDDHOPN (ZZDDHMAN) | Copies the file name into a `CHARACTER*(FILEN)` (255) local without a length check: a longer path is silently truncated, and the wrong file is opened or inquired. FURNSH checks the length, but DAFOPR, DASOPR, etc. called directly do not. | Fixed: file names have no length limit (`plans/limits.md`, phase 2). |
| RDKER, RDTEXT | RDKER keeps the name of the text kernel being read in `CHARACTER*(FILSIZ)` (255) and reads the following lines through RDTEXT with that name; RDTEXT recognizes the file by comparing it with its own 255-character copy. A longer path works only because both copies are cut the same way; two paths that agree in their first 255 characters would be confused. | Fixed: both names have no length limit. |
| FRAMEX | The cache of kernel-defined frames (`MAXKFR` = 5209 entries) is reset only when the pool changes, and entries are added with ZZHSCADD/ZZHSIADD without checking for room (ZZHSCAVL), so looking up the 5210th distinct kernel frame between pool changes signals `SPICE(HASHISFULL)` instead of evicting. | Fixed: the caches grow (`plans/limits.md`, phase 4). |
| ZZRVBF (LMPOOL) | Copies the characters of a string value into the pool's 80-character slot without the bounds check ZZRVAR has: a string value longer than 80 characters in an LMPOOL buffer is written past the end of its slot, into the next value. | Fixed: string values have no length limit (`plans/limits.md`, phase 3). |
| ZZBODDEF (BODDEF) | The documented limit is `MAXP` = 150 new names, but the check is against `MAXE` − `NPERM` = 161; `MAXP` is not used in code. | Unchanged. |
| ZZEKECMP | The right-hand entry of an integer or double precision comparison is read with element index `ELTS(1)`, the left-hand one's; the error message reports `ELTS(2)`. Harmless, as only scalar entries are compared (both 1). | Reads `ELTS(2)`. |
| ZZEKJOIN | Its check of the number of tables of the first join row set tests `NT2` (the second's) against the upper bound. | Moot: no upper bound (`plans/limits.md`, phase 6). |
| ZZEKJSRT | Its sort keys are the first INISUB (32) characters of each string, read with ZZEKRSC into `CHARACTER*(INISUB)`. For a fixed-count column (class 9, `FIXED_COUNT = TRUE`) the reader signals SPICE(STRINGTRUNCATED) on a buffer shorter than the column's strings: a query that orders by a fixed-count string column longer than 32 characters fails. | Reads the whole string and keeps its first 32 characters (`plans/limits.md`, phase 6, EK step C3). |
| ZZEKRSC | Leaves `CVLEN` unset for class 6 columns (arrays of strings). Harmless in practice: only scalar entries are compared and EKGC ignores `CVLEN`. | Sets `CVLEN` to the column's declared string length. |
| ZZEKNRML | The normalized WHERE clause (an OR of ANDs) is limited only by the relation pool (10*MAXTOK = 5000 relations), but its conjunction sizes are stored in `SIZES(MAXCON)` (1000) without a check, and EKSRCH's per-constraint arrays have MAXCON elements: a WHERE clause whose normal form has more than 1000 relations (e.g. 8 parenthesized ORs joined by AND: 1024) is written past those arrays. | Fixed: the constraint arrays are sized for the query (`plans/limits.md`, phase 6, EK steps C1, C2). |
| EKQMGR (EKLEF, EKUEF) | The list of the files of each table (`TBFILS`, `TBFLSZ`): EKLEF stores the handle of every new file of a table in element 1 (overwriting the previous one) while counting it; EKUEF looks for the file in that list, but its flag `FND`, still set by the search of the file table, keeps the search from running, so every table is examined (and its count decremented); EKLEF's clean-up after a failed load starts from the saved `FND` of an earlier call. The files are unloaded correctly only because the segments are removed by handle in every table examined. | The list is gone; every table is examined (`plans/limits.md`, phase 6). |
| NPARSD (ZZRVAR, text kernels) | Text kernel numbers are parsed by NPARSD, which accumulates the digits in double precision and is not correctly rounded: many decimal values come out a few ulps from the nearest double, e.g. `1.8200000000000001E+00` → 1.8199999999999998 (1 ulp), `-1.4D-12` → -1.3999999999999999e-12, and `1.3271244004193938E+11` (the Sun's GM) → 132712440041.9393, 5 ulps below 132712440041.93938 (NAIF's `gm_de431.tpc` and `pck00010.tpc`; CSPICE gives the same). | Fixed: NPARSD keeps SPICELIB's scan (the forms it accepts, its errors) and reads the value with Fortran's READ, correctly rounded. Text kernel values can therefore differ from SPICELIB's in their last bits. Test: `test_nparsd`. |
| TTRANS | Reads the leapseconds (`DELTET/DELTA_AT`) with GDPOOL into a table of `MAXLP` = 140, which returns only the first 140 without an error: with more leapseconds, UTC strings (STR2ET, ET2UTC, TIMOUT) silently use the 140th offset for later epochs, while DELTET (a buffer of 200, an error past it) uses the right one. | Fixed: the table holds all the leapseconds (`plans/limits.md`, phase 6). |

## EK

Found while documenting the EK state. "Checked" entries were confirmed against the
N0067 source; the others are from reading the code and are likely but not tested.
PEPPER keeps SPICELIB's behavior in all of them.

| Routine | Problem | Status |
|---|---|---|
| EKQMGR (EKLEF) | When a segment of an already loaded table is found, the file is stored in `TBFILS(1, TBCURR)` (overwriting the first file) instead of `TBFILS(TBFLSZ+1, TBCURR)`, and the count is incremented; the handle is also added again for each segment of that table in the file. | Checked |
| EKQMGR (EKUEF) | `FND` is still .TRUE. from finding the file and is not reset for each table, so the per-table search of `TBFILS` never runs: every table is treated as containing the file, and `TBFLSZ` is decremented for all of them (it can become negative). Segments are still removed correctly (that test uses `STHAN`), and `TBFILS` is read only here, which mostly hides the EKLEF bug. | Checked |
| EKQMGR (EKLEF, abort cleanup) | The cleanup loop tests `.NOT. FND` without EKLEF setting `FND` (the value is left by the previous entry point); same missing per-table reset as EKUEF. | Reported |
| EKQMGR (EKNELT, EKGC, EKGD, EKGI) | The SELECT index is checked against `NSEL`, but the "valid range 1:#" message is filled with `NTAB`. | Reported |
| ZZEKJTST (ZZEKJNXT) | `LELTS` and `RELTS` are never assigned (ZZEKJPRP saves only the key constraint's elements); they are passed to ZZEKVMCH, so the other join constraints are compared with element index 0 (zero storage). | Checked |
| ZZEKJTST (ZZEKJNXT) | The test `SVCP1(CNSTR) <= SVNT1` that places the two halves of a row vector uses the width of the join row set holding the key's left table, which is NT2 when that table is in the second set. | Reported |
| ZZEKVADR (ZZEKVSET, ZZEKVCAL) | `MAXRWV = BEGIDX(NJRS) + J` is one more than the last row vector index, so ZZEKVCAL accepts one index past the end; some messages give ranges that differ from the checks, and one passes an extra ERRINT. | Likely (checked the code) |
| ZZEKAD06 | The "Column index = #; valid range is 1:#" error passes `NREC` instead of `NCOLS`; the comment "Make sure the record exists" has no check. | Reported |
| STCC01 | Every early return with an error message skips EKCLS, so the catalog file stays open; NCOLS and the column names come from the last segment only. | Reported |
| ZZEKTRLK | After a failed lookup the page buffer `PAGE` is overwritten but the cached `OLD*` values and `LEAF` are not, so a following lookup in the same tree could use the wrong page (an error was signaled first). | Reported |
| ZZEKPAGE | The fill buffers `CFILL`, `DFILL`, `IFILL` are set only by ZZEKPGIN but used by ZZEKPGAN and ZZEKPGAL for any file (zero storage; `CFILL` would not be blanks). | Reported (harmless in PEPPER: state starts blank/zero) |

Note: the per-context query results (EKSRCH) point into the instance's tables of
loaded EK files; a load or unload after a query invalidates them, in SPICELIB too. In
PEPPER this is covered by the rule that kernels are not loaded while other contexts
query.

## Interface (passed against the declaration)

| Routine | Problem | PEPPER |
|---|---|---|
| ZZEKUE04, ZZEKUE05, ZZEKUE06 | `IVALS`, `DVALS`, `CVALS` are declared as scalars but are arrays: they are passed on to ZZEKAD04/05/06, which use `NVALS` of them. | Declared as arrays `( * )`. |
| Callers of TTRANS (STR2ET, SGP4 routines, ET2UTC, TIMOUT, ZZGETELM) | Pass 8-element time vectors; TTRANS declares 10 (`MXCOMP`; it uses at most 7). | 10 elements (ET2UTC's, TIMOUT's and ZZGETELM's in their state types were missed at first: ifx found them, error #7983). |
| ZZSCUP01 | The pool counter `POLCTR` is declared scalar; it is a 2-element counter. | Declared as the counter array. |
| DNEARP | Passes an array (`DALT`) for NEARPT's scalar altitude. | Passes `DALT(1)`. |
| ZZDAFGFR | Passes scalars to ZZXLATEI's array argument. | One-element arrays. |
| Many (EK, DAS, DSK, GF, ...) | Scalars passed to array arguments and arrays to scalar arguments (sequence association), e.g. a DAS address range of one word read into a scalar. | One-element arrays, first elements (`tools/fix_scalar_args.py`, `tools/fix_rank_args.py`). |

## Arguments changed although documented as inputs

These routines modify arguments their Brief_I/O table calls inputs (the code is
what SPICELIB does; PEPPER declares them `intent(inout)`):

- ZZCK4D2I: `DPCOEF` (overwritten while unpacking).
- SPKE18, SPKE19: `RECORD` (packets transposed in place).
- CKW04A: `PKTSIZ`, `PKTDAT` (packed in place).
- ZZGFSPIN: `SHAPE` (normalized in place).
- ZZDSIN (SGP4): `EMSQ`.
- ZZDPSEC, ZZDPPER (SDP4): the perturbed elements (`XLL`, `OMGASM`, `XNODES`, `EM`, `XINC`).
- EK: the segment descriptor `SEGDSC` in the column writers and updaters (ZZEKAD01-06, ZZEKDE01-06, ZZEKUE01-06), `COLDSC` in ZZEKCIX1, `CDSCRS` in ZZEKBS01, `RCPTRS` in EKFFLD; `SEGNO` in EKACEI, EKRCEC, EKRCED, EKRCEI, EKUCEC, EKUCED, EKUCEI (reassigned from the descriptor); `RECNO` in EKDELR.
- ZZGFREL, ZZGFRELX: `WORK` (workspace).
- DAFRA: the order vector.
- Cells documented as outputs are inputs too (their size); declared `intent(inout)`.

## Documentation

| Routine | Problem |
|---|---|
| SCTE01 | Brief_I/O says ET is an input; it is the output. |
| ZZDDHNFC | `NATBFF` is an output. |
| ZZASCII | TERMIN marked `0` (zero) instead of `O`; LINE (a work buffer) marked I. |
| ZZDPPER | XLL marked `0` (zero) instead of `O`. |
| ZZDPSEC | OMGDOT missing from the table. |
| ZZDPSEC | XN is marked O (the umbrella ZZNRDDP's table: I); it is both. For an orbit without resonance the entry returns before setting it, and DPSPCE uses the value it set before the call (XNODP). PEPPER had declared it `intent(out)` from the table: optimized builds then dropped DPSPCE's assignment, and DPSPCE returned NaN for deep-space orbits without resonance (TLE 11801). Now `intent(inout)`. |
| ZZDYNOAC | MAXN missing from the table. |
| ZZDSKBSR (ZZDSKSNS) | `DLSDSC` for `DLADSC`. |
| ZZSEGBOX | The table and Detailed_Output name the arguments CENTER and RADIUS; they are BOXCTR and MAXR. |
| ZZDSKBUX | DC and IC missing from the table. |
| EKLEF | The Exceptions section names SPICE(EKSEGMENTTABLEFULL), SPICE(EKCOLDESCTABLEFULL) and SPICE(EKIDTABLEFULL); the code signals SPICE(EKSEGTABLEFULL) and SPICE(COLDESCTABLEFULL), never EKIDTABLEFULL, and also SPICE(EKTABLELISTFULL), not documented. (Moot in PEPPER: no limits.) |
| DELTET | The Exceptions section says SPICE(BUFFEROVERFLOW) for too many leapseconds; the code signals SPICE(BUFFERTOOSMALL). (Moot in PEPPER: no limit.) |
| ZZGFRPWK (ZZGFWKAD) | The table lists TOTAL (an argument of ZZGFWKIN) instead of TCHECK. |
| ZZEKQORD, ZZEKQSEL | TABIDX listed twice; the second is COLIDX. |
| ZZEKRP2N | `RECTPR` for `RECPTR`. |
| ZZEKRBCK | `RECPTR` for `RECNO`. |
| ZZEKVMCH | `RHAN` for `RHANS`. |
| ZZEKAD05, ZZEKAD06 | The table describes an older argument list (SEGNO, RECNO, COLUMN for SEGDSC, COLDSC, RECPTR). |
| ZZEKJTST (ZZEKJPRP) | ELTS1 and ELTS2 missing from the table. |
| ZZEKCCHK, ZZEKTRES, ZZEKNRES | QUERY missing from the table. |
| ZZDSCM, ZZDSIN, ZZDSPC, ZZINIL (SGP4) | No Brief_I/O table; some arguments documented in neither Detailed_Input nor Detailed_Output. |

The fixed headers keep NAIF's text otherwise.
