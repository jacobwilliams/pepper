!> The legacy interface's routines of the subsystem gf (80 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_gf

   use pepper_instance_module, only: pepper_context, pepper_zzgfcoq => zzgfcoq, &
      pepper_zzgfcprx => zzgfcprx, pepper_zzgfdiq => zzgfdiq, pepper_zzgfdsps => zzgfdsps, &
      pepper_zzgffvin => zzgffvin, pepper_zzgffvst => zzgffvst, pepper_zzgfilin => zzgfilin, &
      pepper_zzgfildc => zzgfildc, pepper_zzgfilgq => zzgfilgq, pepper_zzgfocin => zzgfocin, &
      pepper_zzgfocst => zzgfocst, pepper_zzgfpaq => zzgfpaq, pepper_zzgfref => zzgfref, &
      pepper_zzgfrrq => zzgfrrq, pepper_zzgfsolv => zzgfsolv, pepper_zzgfsolvx => zzgfsolvx, &
      pepper_zzgfspq => zzgfspq, pepper_zzgfspin => zzgfspin, pepper_zzgfspdc => zzgfspdc, &
      pepper_zzgfspgq => zzgfspgq, pepper_zzgfspx => zzgfspx, pepper_zzgfssin => zzgfssin, &
      pepper_zzgfssob => zzgfssob, pepper_zzgfudlt => zzgfudlt, pepper_zzgfwsts => zzgfwsts, &
      pepper_zzgfcost => zzgfcost, pepper_zzgfdiin => zzgfdiin, pepper_zzgfdidc => zzgfdidc, &
      pepper_zzgfdigq => zzgfdigq, pepper_zzgfpain => zzgfpain, pepper_zzgfpadc => zzgfpadc, &
      pepper_zzgfpagq => zzgfpagq, pepper_zzgfpax => zzgfpax, pepper_zzgfrel => zzgfrel, &
      pepper_zzgfrelx => zzgfrelx, pepper_zzgftswk => zzgftswk, pepper_zzgfwkin => zzgfwkin, &
      pepper_zzgfwkad => zzgfwkad, pepper_zzgfwkun => zzgfwkun, pepper_zzgfwkmo => zzgfwkmo, &
      pepper_zzgfrrin => zzgfrrin, pepper_zzgfrrdc => zzgfrrdc, pepper_zzgfrrgq => zzgfrrgq, &
      pepper_zzgfrrx => zzgfrrx, pepper_zzgfudb => zzgfudb, pepper_zzgfcoin => zzgfcoin, &
      pepper_zzgfcog => zzgfcog, pepper_zzgfcodc => zzgfcodc, pepper_zzgfcoex => zzgfcoex, &
      pepper_zzgfcocg => zzgfcocg, pepper_zzgfcosg => zzgfcosg, pepper_zzgfcocd => zzgfcocd, &
      pepper_zzgfcosd => zzgfcosd, pepper_zzgflong => zzgflong, pepper_zzgfcslv => zzgfcslv
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_legacy_callbacks
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: gfbail, gfstol, zzgfcoq, zzgfcprx, zzgfdiq, zzgfdsps, zzgffvin, zzgffvst, zzgfilin, &
      zzgfildc, zzgfilgq, zzgfocin, zzgfocst, zzgfpaq, zzgfref, zzgfrrq, zzgfsolv, zzgfsolvx, &
      zzgfspq, zzgfspin, zzgfspdc, zzgfspgq, zzgfspx, zzgfssin, zzgfssob, zzgfudlt, zzgfwsts, &
      fovray, fovtrg, gffove, gfocce, occult, phaseq, zzgfcost, zzgfdiin, zzgfdidc, zzgfdigq, &
      zzgfpain, zzgfpadc, zzgfpagq, zzgfpax, zzgfrel, zzgfrelx, zzgftswk, zzgfwkin, zzgfwkad, &
      zzgfwkun, zzgfwkmo, zzgfrrin, zzgfrrdc, zzgfrrgq, zzgfrrx, zzgfudb, gfoclt, gfrfov, gfrepi, &
      gfrepu, gfrepf, gftfov, gfudb, gfuds, zzgfcoin, zzgfcog, zzgfcodc, zzgfcoex, zzgfcocg, &
      zzgfcosg, zzgfcocd, zzgfcosd, zzgflong, zzgfcslv, gfevnt, gfdist, gfilum, gfpa, gfposc, &
      gfrr, gfsep, gfsntc, gfsubc

contains

      !> Serve as a placeholder for an interrupt detection function.
      !>
      !> Legacy interface: calls ctx%gfbail with the legacy context.
      logical function gfbail()
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      gfbail = ctx%gfbail (  )
      end function gfbail

      !> Override the default GF convergence value used in the high
      !> level GF routines.
      !>
      !> Legacy interface: calls ctx%gfstol with the legacy context.
      subroutine gfstol ( value )
      use pepper_kinds, only: wp
      use pepper_inc_zzholdd
      implicit none
      real(wp) :: value  !! Double precision value returned or to store.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfstol ( value )
      end subroutine gfstol

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfcoq with the legacy context.
      subroutine zzgfcoq ( vecdef, method, trgid, et, ref, abcorr, obsid, dref, dvec, crdsys, ctrid, re, f, crdnam, value, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      implicit none
      character(*) :: vecdef  !! Vector definition.
      character(*) :: method  !! Computation method.
      integer(ip) :: trgid  !! Target ID code.
      real(wp) :: et  !! Computation epoch.
      character(*) :: ref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      integer(ip) :: obsid  !! Observer ID code.
      character(*) :: dref  !! Reference frame of ray's direction vector.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      character(*) :: crdsys  !! Coordinate system name.
      integer(ip) :: ctrid  !! Frame center ID code.
      real(wp) :: re  !! Equatorial radius of central body.
      real(wp) :: f  !! Flattening coefficient of central body.
      character(*) :: crdnam  !! Coordinate name.
      real(wp) :: value  !! Coordinate value.
      logical :: found  !! Flag indicating if coordinate was computed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgfcoq ( ctx, vecdef, method, trgid, et, ref, abcorr, obsid, dref, (dvec), &
         crdsys, ctrid, re, f, crdnam, value, found )
      end subroutine zzgfcoq

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfcprx with the legacy context.
      subroutine zzgfcprx ( state, corsys, re, f, sense, cdsign )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      real(wp) :: state  ( 6 )  !! A 6-dimensional Cartesian state vector.
      character(*) :: corsys  !! A coordinate system name parameter.
      real(wp) :: re  !! Ellipsoid equatorial radius.
      real(wp) :: f  !! Ellipsoid flattening coefficient.
      integer(ip) :: sense  !! Reference body longitude sense.
      integer(ip) :: cdsign ( 3 )  !! Velocity sign vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfcprx ( ctx, state, corsys, re, f, sense, cdsign )
      end subroutine zzgfcprx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfdiq with the legacy context.
      subroutine zzgfdiq ( targid, et, abcorr, obsid, dist )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: targid  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obsid  !! Observing body.
      real(wp) :: dist  !! Distance between target and observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfdiq ( ctx, targid, et, abcorr, obsid, dist )
      end subroutine zzgfdiq

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfdsps with the legacy context.
      subroutine zzgfdsps ( nlead, string, fmt, ntrail )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nlead  !! Number of leading blank lines to write.
      character(*) :: string  !! The string to display.
      character(*) :: fmt  !! Format in which the string is to be written.
      integer(ip) :: ntrail  !! Number of trailing blank lines to write.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfdsps ( ctx, nlead, string, fmt, ntrail )
      end subroutine zzgfdsps

      !> Perform initialization functions for visibility state
      !> determination.
      !>
      !> Legacy interface: calls zz routine zzgffvin with the legacy context.
      subroutine zzgffvin ( inst, tshape, raydir, target, tframe, abcorr, obsrvr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      use pepper_inc_zzocced
      use pepper_inc_gf
      implicit none
      character(*) :: inst  !! Name of the instrument.
      character(*) :: tshape  !! Type of shape model used for target body.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      character(*) :: target  !! Name of the target body.
      character(*) :: tframe  !! Body-fixed, body-centered frame for target body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgffvin ( ctx, inst, tshape, raydir, target, tframe, abcorr, obsrvr )
      end subroutine zzgffvin

      !> Indicate whether the target is currently in the instrument FOV.
      !>
      !> Legacy interface: calls zz routine zzgffvst with the legacy context.
      subroutine zzgffvst ( time, vistat )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      use pepper_inc_zzocced
      use pepper_inc_gf
      implicit none
      real(wp) :: time  !! TDB epoch (in seconds past J2000)
      logical :: vistat  !! .TRUE. if the object is visible, .FALSE. otherwise.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgffvst ( ctx, time, vistat )
      end subroutine zzgffvst

      !> Initialize the GF illumination angle constraint search utilities.
      !>
      !> Legacy interface: calls zz routine zzgfilin with the legacy context.
      subroutine zzgfilin ( method, angtyp, target, illum, fixref, abcorr, obsrvr, spoint )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: angtyp  !! Type of illumination angle.
      character(*) :: target  !! Target body name.
      character(*) :: illum  !! Illumination source name.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction specifier.
      character(*) :: obsrvr  !! Observer name.
      real(wp) :: spoint ( 3 )  !! Body-fixed coordinates of a target surface point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfilin ( ctx, method, angtyp, target, illum, fixref, abcorr, obsrvr, spoint )
      end subroutine zzgfilin

      !> Indicate whether a specified illumination angle is decreasing at
      !> a specified time.
      !>
      !> Legacy interface: calls zz routine zzgfildc with the legacy context.
      subroutine zzgfildc ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      logical :: decres  !! Flag indicating whether illumination angle is decreasing.
      procedure(legacy_gf_scalar) :: udfunc  !! Placeholder external routine argument.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfildc ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfildc

      !> Return the specified illumination angle at a specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfilgq with the legacy context.
      subroutine zzgfilgq ( et, angle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      real(wp) :: angle  !! Illumination angle at time ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfilgq ( ctx, et, angle )
      end subroutine zzgfilgq

      !> Perform initialization functions for occultation state
      !> determination.
      !>
      !> Legacy interface: calls zz routine zzgfocin with the legacy context.
      subroutine zzgfocin ( occtyp, front, fshape, fframe, back, bshape, bframe, obsrvr, abcorr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzdsk
      use pepper_inc_zzocced
      implicit none
      character(*) :: occtyp  !! Type of occultation.
      character(*) :: front  !! Name of body occulting the other.
      character(*) :: fshape  !! Type of shape model used for front body.
      character(*) :: fframe  !! Body-fixed, body-centered frame for front body.
      character(*) :: back  !! Name of body occulted by the other.
      character(*) :: bshape  !! Type of shape model used for back body.
      character(*) :: bframe  !! Body-fixed, body-centered frame for back body.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: abcorr  !! Aberration correction flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfocin ( ctx, occtyp, front, fshape, fframe, back, bshape, bframe, obsrvr, abcorr )
      end subroutine zzgfocin

      !> See if the object is currently occulted.
      !>
      !> Legacy interface: calls zz routine zzgfocst with the legacy context.
      subroutine zzgfocst ( time, ocstat )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzdsk
      use pepper_inc_zzocced
      implicit none
      real(wp) :: time  !! TDB epoch (in seconds past J2000)
      logical :: ocstat  !! .TRUE. if the object is occulted, .FALSE. otherwise.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfocst ( ctx, time, ocstat )
      end subroutine zzgfocst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfpaq with the legacy context.
      subroutine zzgfpaq ( et, targ, illmn, obs, abcorr, value )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      integer(ip) :: targ  !! Target body ID.
      integer(ip) :: illmn  !! Illuminating body ID.
      integer(ip) :: obs  !! Observer body ID.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: value  !! Value of phase angle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfpaq ( ctx, et, targ, illmn, obs, abcorr, value )
      end subroutine zzgfpaq

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfref with the legacy context.
      subroutine zzgfref( refval )
      use pepper_kinds, only: wp
      use pepper_inc_zzholdd
      implicit none
      real(wp) :: refval  !! The value to set as the reference value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfref ( ctx, refval )
      end subroutine zzgfref

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfrrq with the legacy context.
      subroutine zzgfrrq ( et, targ, obs, abcorr, value )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB
      integer(ip) :: targ  !! Target body ID
      integer(ip) :: obs  !! Observer body ID
      character(*) :: abcorr  !! Aberration correction flag
      real(wp) :: value  !! Value of range rate between objects
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfrrq ( ctx, et, targ, obs, abcorr, value )
      end subroutine zzgfrrq

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfsolv with the legacy context.
      subroutine zzgfsolv ( udcond,   udstep,  udrefn,  bail, &
                            udbail,   cstep,   step,    start, &
                            finish,   tol,     rpt,     udrepu, &
                            result )
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      logical :: cstep  !! Logical indicating constant step size.
      real(wp) :: step  !! Constant step size in seconds for finding geometric events.
      logical :: rpt  !! Progress reporter on ( .TRUE.) or off ( .FALSE. )
      real(wp) :: start  !! Beginning of the search interval.
      real(wp) :: finish  !! End of the search interval.
      real(wp) :: tol  !! Maximum error in detection of state transitions.
      real(wp) :: result  ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_cond) :: udcond  !! Name of the routine that compares the current state condition with-respect-to a constraint.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that computes a time step
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_cond), pointer :: outer_udcond  !! the outer call's routine
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      ctx => legacy_context()
      outer_udcond => current_gf_cond
      current_gf_cond => udcond
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      call pepper_zzgfsolv ( ctx, bridge_gf_cond, bridge_gf_step, bridge_gf_refine, bail, bridge_gf_bail, cstep, step, start, finish, tol, rpt, bridge_gf_rep_update, result )
      current_gf_cond => outer_udcond
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_bail => outer_udbail
      current_gf_rep_update => outer_udrepu
      end subroutine zzgfsolv

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfsolvx with the legacy context.
      subroutine zzgfsolvx ( udfuns, udfunb, udstep,  udrefn,  bail, &
                             udbail, cstep,  step,    start, &
                             finish, tol,    rpt,     udrepu, &
                             result )
      use pepper_kinds, only: ip, wp
      implicit none
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      logical :: cstep  !! Logical indicating constant step size.
      real(wp) :: step  !! Constant step size in seconds for finding geometric events.
      logical :: rpt  !! Progress reporter on ( .TRUE.) or off ( .FALSE. )
      real(wp) :: start  !! Beginning of the search interval.
      real(wp) :: finish  !! End of the search interval.
      real(wp) :: tol  !! Maximum error in detection of state transitions.
      real(wp) :: result  ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_scalar) :: udfuns  !! The routine that computes the scalar quantity of interest.
      procedure(legacy_gf_boolean) :: udfunb  !! Name of the routine that compares the current state condition with-respect-to a constraint.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that computes a time step
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfuns  !! the outer call's routine
      procedure(legacy_gf_boolean), pointer :: outer_udfunb  !! the outer call's routine
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      ctx => legacy_context()
      outer_udfuns => current_gf_scalar
      current_gf_scalar => udfuns
      outer_udfunb => current_gf_boolean
      current_gf_boolean => udfunb
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      call pepper_zzgfsolvx ( ctx, bridge_gf_scalar, bridge_gf_boolean, bridge_gf_step, bridge_gf_refine, bail, bridge_gf_bail, cstep, step, start, finish, tol, rpt, bridge_gf_rep_update, result )
      current_gf_scalar => outer_udfuns
      current_gf_boolean => outer_udfunb
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_bail => outer_udbail
      current_gf_rep_update => outer_udrepu
      end subroutine zzgfsolvx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfspq with the legacy context.
      subroutine zzgfspq ( et, targ1, targ2, r1, r2, &
                           obs, abcorr, ref, value )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB
      integer(ip) :: targ1  !! NAIF ID for first target
      integer(ip) :: targ2  !! NAIF ID for second target
      real(wp) :: r1  !! Radius of a spherical model for TARG1
      real(wp) :: r2  !! Radius of a spherical model for TARG2
      integer(ip) :: obs  !! NAIF ID of observer
      character(*) :: abcorr  !! Aberration correction flag
      character(*) :: ref  !! Reference frame of the angular separation
      real(wp) :: value  !! Value of angular separation between objects
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfspq ( ctx, et, targ1, targ2, r1, r2, obs, abcorr, ref, value )
      end subroutine zzgfspq

      !> This routine initializes variables that describe an angular
      !> separation event of interest for solution by ZZGFSOLV.
      !>
      !> Legacy interface: calls zz routine zzgfspin with the legacy context.
      subroutine zzgfspin ( of, from, shape, frame, abcorr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: of    (2)  !! Body id's of the angular separation objects
      character(*) :: from  !! Observer name
      character(*) :: shape (2)  !! Array of shape IDs corresponding to OF
      character(*) :: frame (2)  !! Array of frame names corresponding to OF
      character(*) :: abcorr  !! Aberration correction flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: of, from, frame, abcorr are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgfspin ( ctx, (of), (from), shape, (frame), (abcorr) )
      end subroutine zzgfspin

      !> Computes whether or not the angular separation between SVBOD1 and
      !> SVBOD2 is decreasing at time ET.
      !>
      !> Legacy interface: calls zz routine zzgfspdc with the legacy context.
      subroutine zzgfspdc ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      logical :: decres  !! .TRUE. if angular separation is decreasing .FALSE. otherwise.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfspdc ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfspdc

      !> Determine the angular separation between the limbs of the two
      !> bodies.
      !>
      !> Legacy interface: calls zz routine zzgfspgq with the legacy context.
      subroutine zzgfspgq ( et, sep )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      real(wp) :: sep  !! Separation at time ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfspgq ( ctx, et, sep )
      end subroutine zzgfspgq

      !> Retrieve values set in ZZGFSPIN.
      !>
      !> Legacy interface: calls zz routine zzgfspx with the legacy context.
      subroutine zzgfspx ( xabcr, xbod, yref, xref, xobs, xrad, xshp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: xabcr  !! Saved value SVABCR.
      integer(ip) :: xbod  (2)  !! Saved values SVBOD1 and SVBOD2.
      character(*) :: yref  !! Saved value SVREF.
      character(*) :: xref  (2)  !! Saved values SVREF1 and SVREF2.
      integer(ip) :: xobs  !! Saved value SVOBS.
      real(wp) :: xrad  (2)  !! Saved values SVRAD1 and SVRAD2.
      integer(ip) :: xshp  (2)  !! Saved values SVSHP1 and SVSHP2.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfspx ( ctx, xabcr, xbod, yref, xref, xobs, xrad, xshp )
      end subroutine zzgfspx

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfssin with the legacy context.
      subroutine zzgfssin ( method, trgid, et, fixref, abcorr, obsid, dref, dctr, dvec, radii, state, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: method  !! Computation method.
      integer(ip) :: trgid  !! Target ID code.
      real(wp) :: et  !! Computation epoch.
      character(*) :: fixref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      integer(ip) :: obsid  !! Observer ID code.
      character(*) :: dref  !! Reference frame of ray's direction vector.
      integer(ip) :: dctr  !! DREF's center ID code.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      real(wp) :: radii  ( 3 )  !! Target radii.
      real(wp) :: state  ( 6 )  !! State used to define coordinates.
      logical :: found  !! Flag indicating whether state was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec, radii are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgfssin ( ctx, method, trgid, et, fixref, abcorr, obsid, dref, dctr, (dvec), &
         (radii), state, found )
      end subroutine zzgfssin

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfssob with the legacy context.
      subroutine zzgfssob ( method, trgid, et, fixref, abcorr, obsid, radii, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: method  !! Computation method.
      integer(ip) :: trgid  !! Target ID code.
      real(wp) :: et  !! Computation epoch.
      character(*) :: fixref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      integer(ip) :: obsid  !! Observer ID code.
      real(wp) :: radii  ( 3 )  !! Target radii.
      real(wp) :: state  ( 6 )  !! State used to define coordinates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: radii is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgfssob ( ctx, method, trgid, et, fixref, abcorr, obsid, (radii), state )
      end subroutine zzgfssob

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfudlt with the legacy context.
      subroutine zzgfudlt ( udfunc, et, isless )
      use pepper_kinds, only: wp
      use pepper_inc_zzholdd
      implicit none
      real(wp) :: et  !! Time in TDB seconds for which to evaluate UDFUNC.
      logical :: isless  !! Boolean indicating if the scalar value is less than reference value.
      procedure(legacy_gf_scalar) :: udfunc  !! Name of the routine that computes the scalar value of interest at ET.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfudlt ( ctx, bridge_gf_scalar, et, isless )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfudlt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfwsts with the legacy context.
      subroutine zzgfwsts ( wndw1, wndw2, inclsn, wndw3 )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: wndw1 ( lbcell : * )  !! Input window 1.
      real(wp) :: wndw2 ( lbcell : * )  !! Input window 2.
      character(*) :: inclsn  !! Flag indicating inclusion desired.
      real(wp) :: wndw3 ( lbcell : * )  !! Result of sifting WNDW1 through WNDW2.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfwsts ( ctx, wndw1, wndw2, inclsn, wndw3 )
      end subroutine zzgfwsts

      !> Determine if a specified ray is within the field-of-view (FOV) of
      !> a specified instrument at a given time.
      !>
      !> Legacy interface: calls ctx%fovray with the legacy context.
      subroutine fovray ( inst,   raydir, rframe, abcorr, &
                          obsrvr, et,     visibl        )
      use pepper_kinds, only: wp
      use pepper_inc_gf
      implicit none
      character(*) :: inst  !! Name or ID code string of the instrument.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      character(*) :: rframe  !! Reference frame of ray's direction vector.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name or ID code string of the observer.
      real(wp) :: et  !! Time of the observation (seconds past J2000).
      logical :: visibl  !! Visibility flag (.TRUE./.FALSE.).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%fovray ( inst, raydir, rframe, abcorr, obsrvr, et, visibl )
      end subroutine fovray

      !> Determine if a specified ephemeris object is within the
      !> field-of-view (FOV) of a specified instrument at a given time.
      !>
      !> Legacy interface: calls ctx%fovtrg with the legacy context.
      subroutine fovtrg ( inst, target, tshape, tframe, abcorr, obsrvr, et, visibl )
      use pepper_kinds, only: wp
      use pepper_inc_gf
      implicit none
      character(*) :: inst  !! Name or ID code string of the instrument.
      character(*) :: target  !! Name or ID code string of the target.
      character(*) :: tshape  !! Type of shape model used for the target.
      character(*) :: tframe  !! Body-fixed, body-centered frame for target body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name or ID code string of the observer.
      real(wp) :: et  !! Time of the observation (seconds past J2000).
      logical :: visibl  !! Visibility flag (.TRUE./.FALSE.).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%fovtrg ( inst, target, tshape, tframe, abcorr, obsrvr, et, visibl )
      end subroutine fovtrg

      !> Determine time intervals when a specified target body or ray
      !> intersects the space bounded by the field-of-view (FOV) of a
      !> specified instrument. Report progress and handle interrupts if so
      !> commanded.
      !>
      !> Legacy interface: calls ctx%gffove with the legacy context.
      subroutine gffove ( inst,    tshape,  raydir, &
                          target,  tframe,  abcorr,  obsrvr,  tol, &
                          udstep,  udrefn,  rpt,     udrepi,  udrepu, &
                          udrepf,  bail,    udbail,  cnfine,  result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      implicit none
      character(*) :: inst  !! Name of the instrument.
      character(*) :: tshape  !! Type of shape model used for target body.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      character(*) :: target  !! Name of the target body.
      character(*) :: tframe  !! Body-fixed, body-centered frame for target body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      real(wp) :: tol  !! Convergence tolerance in seconds.
      logical :: rpt  !! Progress report flag.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_step) :: udstep  !! Name of routine that returns a time step.
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_rep_init) :: udrepi  !! Function that initializes progress reporting.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      procedure(legacy_gf_rep_final) :: udrepf  !! Function that finalizes progress reporting.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      ! PEPPER: raydir is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%gffove ( inst, tshape, (raydir), target, tframe, abcorr, obsrvr, tol, &
         bridge_gf_step, bridge_gf_refine, rpt, bridge_gf_rep_init, bridge_gf_rep_update, &
         bridge_gf_rep_final, bail, bridge_gf_bail, cnfine, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine gffove

      !> Determine time intervals when an observer sees one target
      !> occulted by another. Report progress and handle interrupts
      !> if so commanded.
      !>
      !> Legacy interface: calls ctx%gfocce with the legacy context.
      subroutine gfocce ( occtyp,  front,   fshape,  fframe, &
                          back,    bshape,  bframe,  abcorr, &
                          obsrvr,  tol,     udstep,  udrefn, &
                          rpt,     udrepi,  udrepu,  udrepf, &
                          bail,    udbail,  cnfine,  result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzdsk
      implicit none
      character(*) :: occtyp  !! Type of occultation.
      character(*) :: front  !! Name of body occulting the other.
      character(*) :: fshape  !! Type of shape model used for front body.
      character(*) :: fframe  !! Body-fixed, body-centered frame for front body.
      character(*) :: back  !! Name of body occulted by the other.
      character(*) :: bshape  !! Type of shape model used for back body.
      character(*) :: bframe  !! Body-fixed, body-centered frame for back body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      real(wp) :: tol  !! Convergence tolerance in seconds.
      logical :: rpt  !! Progress report flag.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that returns a time step.
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_rep_init) :: udrepi  !! Function that initializes progress reporting.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      procedure(legacy_gf_rep_final) :: udrepf  !! Function that finalizes progress reporting.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      call ctx%gfocce ( occtyp, front, fshape, fframe, back, bshape, bframe, abcorr, obsrvr, tol, bridge_gf_step, bridge_gf_refine, rpt, bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, bail, bridge_gf_bail, cnfine, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine gfocce

      !> Determine the occultation condition (not occulted, partially
      !> occulted, etc.) of one target relative to another target as seen
      !> by an observer at a given time.
      !>
      !> Legacy interface: calls ctx%occult with the legacy context.
      subroutine occult ( targ1,  shape1, frame1, &
                          targ2,  shape2, frame2, &
                          abcorr, obsrvr, et,     ocltid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_occult
      use pepper_inc_zzdsk
      implicit none
      character(*) :: targ1  !! Name or ID of first target.
      character(*) :: shape1  !! Type of shape model used for first target.
      character(*) :: frame1  !! Body-fixed, body-centered frame for first body.
      character(*) :: targ2  !! Name or ID of second target.
      character(*) :: shape2  !! Type of shape model used for second target.
      character(*) :: frame2  !! Body-fixed, body-centered frame for second body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name or ID of the observer.
      real(wp) :: et  !! Time of the observation (seconds past J2000).
      integer(ip) :: ocltid  !! Occultation identification code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%occult ( targ1, shape1, frame1, targ2, shape2, frame2, abcorr, obsrvr, et, ocltid )
      end subroutine occult

      !> Compute the apparent phase angle for a target, observer,
      !> illuminator set of ephemeris objects.
      !>
      !> Legacy interface: calls ctx%phaseq with the legacy context.
      real(wp) function phaseq ( et, target, illmn, obsrvr, abcorr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      character(*) :: target  !! Target body name.
      character(*) :: illmn  !! Illuminating body name.
      character(*) :: obsrvr  !! Observer body.
      character(*) :: abcorr  !! Aberration correction flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      phaseq = ctx%phaseq ( et, target, illmn, obsrvr, abcorr )
      end function phaseq

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfcost with the legacy context.
      subroutine zzgfcost ( vecdef, method, trgid, et, &
                            ref,    abcorr, obsid, dref, &
                            dctr,   dvec,   radii, state, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      implicit none
      character(*) :: vecdef  !! Vector definition.
      character(*) :: method  !! Computation method.
      integer(ip) :: trgid  !! Target ID code.
      real(wp) :: et  !! Computation epoch.
      character(*) :: ref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      integer(ip) :: obsid  !! Observer ID code.
      character(*) :: dref  !! Reference frame of ray's direction vector.
      integer(ip) :: dctr  !! ID code of ray frame's center.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      real(wp) :: radii  ( 3 )  !! Radii of reference ellipsoid.
      real(wp) :: state  ( 6 )  !! State used to define coordinates.
      logical :: found  !! Flag indicating if state was computed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec, radii are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgfcost ( ctx, vecdef, method, trgid, et, ref, abcorr, obsid, dref, dctr, &
         (dvec), (radii), state, found )
      end subroutine zzgfcost

      !> Initialize the GF distance constraint search utilities.
      !>
      !> Legacy interface: calls zz routine zzgfdiin with the legacy context.
      subroutine zzgfdiin ( target, abcorr, obsrvr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: target  !! Target body name.
      character(*) :: abcorr  !! Aberration correction specifier.
      character(*) :: obsrvr  !! Observer name.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfdiin ( ctx, target, abcorr, obsrvr )
      end subroutine zzgfdiin

      !> Indicate whether the observer-target distance is decreasing at a
      !> specified time.
      !>
      !> Legacy interface: calls zz routine zzgfdidc with the legacy context.
      subroutine zzgfdidc ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      logical :: decres  !! Flag indicating whether distance is decreasing.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfdidc ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfdidc

      !> Return the distance between the target and observer
      !> at a specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfdigq with the legacy context.
      subroutine zzgfdigq ( et, dist )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      real(wp) :: dist  !! Distance at time ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfdigq ( ctx, et, dist )
      end subroutine zzgfdigq

      !> This is the initialization entry point used for describing
      !> the event that is to be solved for by ZZGFSOLV.
      !>
      !> Legacy interface: calls zz routine zzgfpain with the legacy context.
      subroutine zzgfpain ( target, illmn, abcorr, obsrvr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: illmn  !! Name of the illuminating body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfpain ( ctx, target, illmn, abcorr, obsrvr )
      end subroutine zzgfpain

      !> Computes whether or not the phase angle between the observer
      !> and the target is decreasing at time ET.
      !>
      !> Legacy interface: calls zz routine zzgfpadc with the legacy context.
      subroutine zzgfpadc ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      logical :: decres  !! .TRUE. if phase angle is decreasing, .FALSE. otherwise.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfpadc ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfpadc

      !> Determine the phase angle between the centers of the two
      !> bodies.
      !>
      !> Legacy interface: calls zz routine zzgfpagq with the legacy context.
      subroutine zzgfpagq ( et, rvl )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      real(wp) :: rvl  !! Phase angle at time ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfpagq ( ctx, et, rvl )
      end subroutine zzgfpagq

      !> Retrieve values set in ZZGFPAIN.
      !>
      !> Legacy interface: calls zz routine zzgfpax with the legacy context.
      subroutine zzgfpax ( xtarg, xillmn, xabcor, xobs, xablk )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      integer(ip) :: xtarg  !! Saved value for the target body.
      integer(ip) :: xillmn  !! Saved value for the illuminator body.
      character(*) :: xabcor  !! Saved value for the aberration correction.
      integer(ip) :: xobs  !! Saved value for the observing body.
      logical :: xablk  ( nabcor )  !! Saved value for the aberration correction attribute block.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfpax ( ctx, xtarg, xillmn, xabcor, xobs, xablk )
      end subroutine zzgfpax

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfrel with the legacy context.
      subroutine zzgfrel ( udstep, udrefn, udqdec, udcond, udfunc, udqref, relate, refval, tol, adjust, cnfine, mw, nw, work, rpt, udrepi, udrepu, udrepf, rptpre, rptsuf, bail, udbail, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      implicit none
      character(*) :: relate  !! Operator that either looks for an extreme value (max, min, local, absolute) or compares the geometric quantity value and a number.
      real(wp) :: refval  !! Value used as reference for geometric quantity condition.
      real(wp) :: tol  !! Convergence tolerance in seconds.
      real(wp) :: adjust  !! Allowed variation for absolute extremal geometric conditions.
      real(wp) :: cnfine ( lbcell : * )  !! Confinement schedule
      integer(ip) :: mw  !! Size of workspace windows.
      integer(ip) :: nw  !! Number of workspace windows.
      real(wp) :: work ( lbcell : mw, nw )  !! Array containing workspace windows
      logical :: rpt  !! Progress reporter on ( .TRUE.) or off ( .FALSE. )
      character(*) :: rptpre ( * )  !! Progress reporter beginning message.
      character(*) :: rptsuf ( * )  !! Progress reporter ending message.
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that computes and returns a time step.
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_decr) :: udqdec  !! Name of the routine that computes whether the geometric quantity is decreasing.
      procedure(legacy_gf_cond) :: udcond  !! Name of the routine that computes the geometric condition with-respect-to the constraint.
      procedure(legacy_gf_scalar) :: udfunc  !! The routine that computes the geometric quantity of interest.
      procedure(legacy_gf_ref) :: udqref  !! Name of the routine that resets the current value of REFVAL.
      procedure(legacy_gf_rep_init) :: udrepi  !! Function that initializes progress reporting.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      procedure(legacy_gf_rep_final) :: udrepf  !! Function that finalizes progress reporting.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_decr), pointer :: outer_udqdec  !! the outer call's routine
      procedure(legacy_gf_cond), pointer :: outer_udcond  !! the outer call's routine
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      procedure(legacy_gf_ref), pointer :: outer_udqref  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udqdec => current_gf_decr
      current_gf_decr => udqdec
      outer_udcond => current_gf_cond
      current_gf_cond => udcond
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      outer_udqref => current_gf_ref
      current_gf_ref => udqref
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      call pepper_zzgfrel ( ctx, bridge_gf_step, bridge_gf_refine, bridge_gf_decr, bridge_gf_cond, bridge_gf_scalar, bridge_gf_ref, relate, refval, tol, adjust, cnfine, mw, nw, work, rpt, bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, rptpre, rptsuf, bail, bridge_gf_bail, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_decr => outer_udqdec
      current_gf_cond => outer_udcond
      current_gf_scalar => outer_udfunc
      current_gf_ref => outer_udqref
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine zzgfrel

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfrelx with the legacy context.
      subroutine zzgfrelx ( udstep, udrefn, udqdec, udcond, udfunc, relate, refval, tol, adjust, cnfine, mw, nw, work, rpt, udrepi, udrepu, udrepf, rptpre, rptsuf, bail, udbail, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      implicit none
      character(*) :: relate  !! Operator that either looks for an extreme value (max, min, local, absolute) or compares the scalar quantity value and a number.
      real(wp) :: refval  !! Value used as reference for scalar quantity condition.
      real(wp) :: tol  !! Convergence tolerance in seconds.
      real(wp) :: adjust  !! Allowed variation for absolute extremal scalar conditions.
      real(wp) :: cnfine ( lbcell : * )  !! Confinement window
      integer(ip) :: mw  !! Size of workspace windows.
      integer(ip) :: nw  !! Number of workspace windows.
      real(wp) :: work ( lbcell : mw, nw )  !! Array containing workspace windows
      logical :: rpt  !! Progress reporter on ( .TRUE.) or off ( .FALSE. )
      character(*) :: rptpre ( * )  !! Progress reporter beginning message.
      character(*) :: rptsuf ( * )  !! Progress reporter ending message.
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that computes and returns a time step.
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_decr_x) :: udqdec  !! Name of the routine that computes whether the scalar quantity is decreasing.
      procedure(legacy_gf_boolean) :: udcond  !! Name of the routine that computes the scalar quantity condition with-respect-to the constraint.
      procedure(legacy_gf_scalar) :: udfunc  !! The routine that computes the scalar quantity of interest.
      procedure(legacy_gf_rep_init) :: udrepi  !! Function that initializes progress reporting.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      procedure(legacy_gf_rep_final) :: udrepf  !! Function that finalizes progress reporting.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_decr_x), pointer :: outer_udqdec  !! the outer call's routine
      procedure(legacy_gf_boolean), pointer :: outer_udcond  !! the outer call's routine
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udqdec => current_gf_decr_x
      current_gf_decr_x => udqdec
      outer_udcond => current_gf_boolean
      current_gf_boolean => udcond
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      call pepper_zzgfrelx ( ctx, bridge_gf_step, bridge_gf_refine, bridge_gf_decr_x, bridge_gf_boolean, bridge_gf_scalar, relate, refval, tol, adjust, cnfine, mw, nw, work, rpt, bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, rptpre, rptsuf, bail, bridge_gf_bail, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_decr_x => outer_udqdec
      current_gf_boolean => outer_udcond
      current_gf_scalar => outer_udfunc
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine zzgfrelx

      !> Initialize the work progress utility. This is required prior to
      !> use of the routine that performs the actual reporting.
      !>
      !> Legacy interface: calls zz routine zzgftswk with the legacy context.
      subroutine zzgftswk ( total, wait, tcheck, begin, end )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      real(wp) :: total  !! A measure of the total amount of work to be done.
      real(wp) :: wait  !! Minimum wait time between reports.
      integer(ip) :: tcheck  !! How often to sample the system clock.
      character(*) :: begin  !! First part of the output message.
      character(*) :: end  !! Last part of the output message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgftswk ( ctx, total, wait, tcheck, begin, end )
      end subroutine zzgftswk

      !> Let the work reporter know that an increment of work has just
      !> been completed.
      !>
      !> Legacy interface: calls zz routine zzgfwkin with the legacy context.
      subroutine zzgfwkin ( incr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      real(wp) :: incr  !! An amount of work just completed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfwkin ( ctx, incr )
      end subroutine zzgfwkin

      !> Adjust the frequency with which work progress is reported.
      !>
      !> Legacy interface: calls zz routine zzgfwkad with the legacy context.
      subroutine zzgfwkad ( wait, tcheck, begin, end )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      real(wp) :: wait  !! Minimum wait time between reports.
      integer(ip) :: tcheck  !! Number of calls between system time checks.
      character(*) :: begin  !! First part of the output message.
      character(*) :: end  !! Last part of the output message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfwkad ( ctx, wait, tcheck, begin, end )
      end subroutine zzgfwkad

      !> Set the output unit for the progress report.
      !>
      !> Legacy interface: calls zz routine zzgfwkun with the legacy context.
      subroutine zzgfwkun ( unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      integer(ip) :: unit  !! Output logical unit.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfwkun ( ctx, unit )
      end subroutine zzgfwkun

      !> Return saved progress report parameters.
      !>
      !> Legacy interface: calls zz routine zzgfwkmo with the legacy context.
      subroutine zzgfwkmo ( unit, total, wait, tcheck, begin, end, incr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      integer(ip) :: unit  !! Output logical unit.
      real(wp) :: total  !! A measure of the total amount of work to be done.
      real(wp) :: wait  !! Minimum wait time between reports.
      integer(ip) :: tcheck  !! Number of calls between system time check.
      character(*) :: begin  !! First part of the output message.
      character(*) :: end  !! Last part of the output message.
      real(wp) :: incr  !! Last progress increment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfwkmo ( ctx, unit, total, wait, tcheck, begin, end, incr )
      end subroutine zzgfwkmo

      !> This is the initialization entry point used for describing
      !> the event that is to be solved for by ZZGFSOLV.
      !>
      !> Legacy interface: calls zz routine zzgfrrin with the legacy context.
      subroutine zzgfrrin ( target, abcorr, obsrvr, dt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: target  !! Name of the target body
      character(*) :: abcorr  !! Aberration correction flag
      character(*) :: obsrvr  !! Name of the observing body
      real(wp) :: dt  !! Interval from ET for derivative calculation.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfrrin ( ctx, target, abcorr, obsrvr, dt )
      end subroutine zzgfrrin

      !> Computes whether or not the range rate between the observer
      !> and the target is decreasing at time ET.
      !>
      !> Legacy interface: calls zz routine zzgfrrdc with the legacy context.
      subroutine zzgfrrdc ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      logical :: decres  !! true if range rate is decreasing, false otherwise.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfrrdc ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfrrdc

      !> Determine the range rate between the centers of the two
      !> bodies.
      !>
      !> Legacy interface: calls zz routine zzgfrrgq with the legacy context.
      subroutine zzgfrrgq ( et, rvl )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      real(wp) :: rvl  !! Range rate at time ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfrrgq ( ctx, et, rvl )
      end subroutine zzgfrrgq

      !> Retrieve values set in ZZGFRRIN.
      !>
      !> Legacy interface: calls zz routine zzgfrrx with the legacy context.
      subroutine zzgfrrx ( xtarg, xabcor, xobs, xdt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      integer(ip) :: xtarg  !! Saved value for the target body.
      character(*) :: xabcor  !! Saved value for the aberration correction.
      integer(ip) :: xobs  !! Saved value for the observing body.
      real(wp) :: xdt  !! Saved value for DT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfrrx ( ctx, xtarg, xabcor, xobs, xdt )
      end subroutine zzgfrrx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgfudb with the legacy context.
      subroutine zzgfudb ( udfuns, udfunb, tol,     udstep,  udrefn, &
                           rpt,    udrepi, udrepu,  udrepf, &
                           bail,   udbail, cnfine,  result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      implicit none
      real(wp) :: tol  !! Convergence tolerance in seconds.
      logical :: rpt  !! Progress report flag.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_scalar) :: udfuns  !! Name of the routine that computes a scalar quantity of interest.
      procedure(legacy_gf_boolean) :: udfunb  !! Name of the routine that computes the boolean value of interest.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that returns a time step.
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_rep_init) :: udrepi  !! Function that initializes progress reporting.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      procedure(legacy_gf_rep_final) :: udrepf  !! Function that finalizes progress reporting.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfuns  !! the outer call's routine
      procedure(legacy_gf_boolean), pointer :: outer_udfunb  !! the outer call's routine
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udfuns => current_gf_scalar
      current_gf_scalar => udfuns
      outer_udfunb => current_gf_boolean
      current_gf_boolean => udfunb
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      call pepper_zzgfudb ( ctx, bridge_gf_scalar, bridge_gf_boolean, tol, bridge_gf_step, bridge_gf_refine, rpt, bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, bail, bridge_gf_bail, cnfine, result )
      current_gf_scalar => outer_udfuns
      current_gf_boolean => outer_udfunb
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine zzgfudb

      !> Determine time intervals when an observer sees one target body
      !> occulted by, or in transit across, another.
      !>
      !> Legacy interface: calls ctx%gfoclt with the legacy context.
      subroutine gfoclt ( occtyp,  front,   fshape,  fframe, &
                          back,    bshape,  bframe,  abcorr, &
                          obsrvr,  step,    cnfine,  result  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: occtyp  !! Type of occultation.
      character(*) :: front  !! Name of body occulting the other.
      character(*) :: fshape  !! Type of shape model used for front body.
      character(*) :: fframe  !! Body-fixed, body-centered frame for front body.
      character(*) :: back  !! Name of body occulted by the other.
      character(*) :: bshape  !! Type of shape model used for back body.
      character(*) :: bframe  !! Body-fixed, body-centered frame for back body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      real(wp) :: step  !! Step size in seconds for finding occultation events.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfoclt ( occtyp, front, fshape, fframe, back, bshape, bframe, abcorr, obsrvr, step, cnfine, result )
      end subroutine gfoclt

      !> Determine time intervals when a specified ray intersects the
      !> space bounded by the field-of-view (FOV) of a specified
      !> instrument.
      !>
      !> Legacy interface: calls ctx%gfrfov with the legacy context.
      subroutine gfrfov ( inst,   raydir, rframe, abcorr, &
                          obsrvr, step,   cnfine, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: inst  !! Name of the instrument.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      character(*) :: rframe  !! Reference frame of ray's direction vector.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      real(wp) :: step  !! Step size in seconds for finding FOV events.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: raydir is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%gfrfov ( inst, (raydir), rframe, abcorr, obsrvr, step, cnfine, result )
      end subroutine gfrfov

      !> Initialize a search progress report.
      !>
      !> Legacy interface: calls ctx%gfrepi with the legacy context.
      subroutine gfrepi ( window, begmss, endmss )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      real(wp) :: window ( lbcell : * )  !! A window over which a job is to be performed.
      character(*) :: begmss  !! Beginning of the text portion of output message.
      character(*) :: endmss  !! End of the text portion of output message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfrepi ( window, begmss, endmss )
      end subroutine gfrepi

      !> Tell the progress reporting system how far a search has
      !> progressed.
      !>
      !> Legacy interface: calls ctx%gfrepu with the legacy context.
      subroutine gfrepu ( ivbeg, ivend, time )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      real(wp) :: ivbeg  !! Start time of work interval.
      real(wp) :: ivend  !! End time of work interval.
      real(wp) :: time  !! Current time being examined in the search process
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfrepu ( ivbeg, ivend, time )
      end subroutine gfrepu

      !> Finish a progress report.
      !>
      !> Legacy interface: calls ctx%gfrepf with the legacy context.
      subroutine gfrepf (  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzgf
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfrepf (  )
      end subroutine gfrepf

      !> Determine time intervals when a specified ephemeris object
      !> intersects the space bounded by the field-of-view (FOV) of a
      !> specified instrument.
      !>
      !> Legacy interface: calls ctx%gftfov with the legacy context.
      subroutine gftfov ( inst, target, tshape, tframe, abcorr, obsrvr, step, cnfine, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: inst  !! Name of the instrument.
      character(*) :: target  !! Name of the target body.
      character(*) :: tshape  !! Type of shape model used for target body.
      character(*) :: tframe  !! Body-fixed, body-centered frame for target body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      real(wp) :: step  !! Step size in seconds for finding FOV events.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gftfov ( inst, target, tshape, tframe, abcorr, obsrvr, step, cnfine, result )
      end subroutine gftfov

      !> Perform a GF search on a user defined boolean quantity.
      !>
      !> Legacy interface: calls ctx%gfudb with the legacy context.
      subroutine gfudb ( udfuns, udfunb, step, cnfine, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzholdd
      implicit none
      real(wp) :: step  !! Constant step size in seconds for finding geometric events.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_scalar) :: udfuns  !! Name of the routine that computes a scalar quantity corresponding to an ET.
      procedure(legacy_gf_boolean) :: udfunb  !! Name of the routine returning the boolean value corresponding to an ET.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfuns  !! the outer call's routine
      procedure(legacy_gf_boolean), pointer :: outer_udfunb  !! the outer call's routine
      ctx => legacy_context()
      outer_udfuns => current_gf_scalar
      current_gf_scalar => udfuns
      outer_udfunb => current_gf_boolean
      current_gf_boolean => udfunb
      call ctx%gfudb ( bridge_gf_scalar, bridge_gf_boolean, step, cnfine, result )
      current_gf_scalar => outer_udfuns
      current_gf_boolean => outer_udfunb
      end subroutine gfudb

      !> Perform a GF search on a user defined scalar quantity.
      !>
      !> Legacy interface: calls ctx%gfuds with the legacy context.
      subroutine gfuds ( udfuns, udqdec, relate, refval, &
                         adjust, step,   cnfine, &
                         mw,     nw,     work,   result  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: relate  !! Operator that either looks for an extreme value (max, min, local, absolute) or compares the geometric quantity value and a number.
      real(wp) :: refval  !! Value used as reference for scalar quantity condition.
      real(wp) :: adjust  !! Allowed variation for absolute extremal geometric conditions.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Size of workspace windows.
      integer(ip) :: nw  !! Number of workspace windows.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array containing workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_scalar) :: udfuns  !! Name of the routine that computes a scalar quantity corresponding to an ET.
      procedure(legacy_gf_decr_x) :: udqdec  !! Name of the routine that computes whether the scalar quantity is decreasing.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfuns  !! the outer call's routine
      procedure(legacy_gf_decr_x), pointer :: outer_udqdec  !! the outer call's routine
      ctx => legacy_context()
      outer_udfuns => current_gf_scalar
      current_gf_scalar => udfuns
      outer_udqdec => current_gf_decr_x
      current_gf_decr_x => udqdec
      call ctx%gfuds ( bridge_gf_scalar, bridge_gf_decr_x, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      current_gf_scalar => outer_udfuns
      current_gf_decr_x => outer_udqdec
      end subroutine gfuds

      !> Initialize a coordinate search.
      !>
      !> Legacy interface: calls zz routine zzgfcoin with the legacy context.
      subroutine zzgfcoin ( vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, crdsys, crdnam )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: vecdef  !! Vector definition.
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Target name.
      character(*) :: ref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Observer name.
      character(*) :: dref  !! Ray's direction vector frame.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      character(*) :: crdsys  !! Coordinate system name.
      character(*) :: crdnam  !! Coordinate name.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfcoin ( ctx, vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, crdsys, crdnam )
      end subroutine zzgfcoin

      !> Compute the coordinate defined by the last call to ZZGFCOIN is at
      !> the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcog with the legacy context.
      subroutine zzgfcog ( et, crdval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      real(wp) :: crdval  !! Coordinate at epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfcog ( ctx, et, crdval )
      end subroutine zzgfcog

      !> Indicate whether the coordinate defined by the last call to
      !> ZZGFCOIN is decreasing at the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcodc with the legacy context.
      subroutine zzgfcodc ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      logical :: decres  !! Flag indicating if coordinate is decreasing.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfcodc ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfcodc

      !> Indicate whether the state of coordinate defined by the last call
      !> to ZZGFCOIN is computable at the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcoex with the legacy context.
      subroutine zzgfcoex ( udfunc, et, crdfnd )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      logical :: crdfnd  !! Flag indicating if coordinate state is computable.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfcoex ( ctx, bridge_gf_scalar, et, crdfnd )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfcoex

      !> Compute the cosine of the coordinate defined by the last call to
      !> ZZGFCOIN is at the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcocg with the legacy context.
      subroutine zzgfcocg ( et, crdval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      real(wp) :: crdval  !! Cosine of coordinate at epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfcocg ( ctx, et, crdval )
      end subroutine zzgfcocg

      !> Compute the sine of the coordinate defined by the last call to
      !> ZZGFCOIN is at the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcosg with the legacy context.
      subroutine zzgfcosg ( et, crdval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      real(wp) :: crdval  !! Sine of coordinate at epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgfcosg ( ctx, et, crdval )
      end subroutine zzgfcosg

      !> Indicate whether the cosine of the coordinate defined by the
      !> last call to ZZGFCOIN is decreasing at the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcocd with the legacy context.
      subroutine zzgfcocd ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      logical :: decres  !! Flag indicating if cos of coordinate is decreasing.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfcocd ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfcocd

      !> Indicate whether the sine of the coordinate defined by the
      !> last call to ZZGFCOIN is decreasing at the specified epoch.
      !>
      !> Legacy interface: calls zz routine zzgfcosd with the legacy context.
      subroutine zzgfcosd ( udfunc, et, decres )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: et  !! Computation epoch.
      logical :: decres  !! Flag indicating if sine of coordinate is decreasing.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call pepper_zzgfcosd ( ctx, bridge_gf_scalar, et, decres )
      current_gf_scalar => outer_udfunc
      end subroutine zzgfcosd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgflong with the legacy context.
      subroutine zzgflong ( vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, crdsys, crdnam, relate, refval, tol, adjust, udstep, udrefn, rpt, udrepi, udrepu, udrepf, bail, udbail, mw, nw, work, cnfine, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      implicit none
      character(*) :: vecdef  !! Vector definition.
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Target name.
      character(*) :: ref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Observer name.
      character(*) :: dref  !! Ray's direction vector frame.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      character(*) :: crdsys  !! Coordinate system name.
      character(*) :: crdnam  !! Coordinate name.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: tol  !! Convergence tolerance.
      real(wp) :: adjust  !! Absolute extremum adjustment value.
      logical :: rpt  !! Progress report flag.
      logical :: bail  !! Bail-out flag.
      procedure(legacy_gf_bail) :: udbail  !! Bail-out status function.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! Workspace window count.
      real(wp) :: work   ( lbcell : mw, nw )  !! Workspace window array.
      real(wp) :: cnfine ( lbcell : * )  !! Confinement window.
      real(wp) :: result ( lbcell : * )  !! Result window.
      procedure(legacy_gf_step) :: udstep  !! Step size routine.
      procedure(legacy_gf_refine) :: udrefn  !! Search refinement routine.
      procedure(legacy_gf_rep_init) :: udrepi  !! Progress report initialization routine.
      procedure(legacy_gf_rep_update) :: udrepu  !! Progress report update routine.
      procedure(legacy_gf_rep_final) :: udrepf  !! Progress report termination routine.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgflong ( ctx, vecdef, method, target, ref, abcorr, obsrvr, dref, (dvec), &
         crdsys, crdnam, relate, refval, tol, adjust, bridge_gf_step, bridge_gf_refine, rpt, &
         bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, bail, bridge_gf_bail, mw, &
         nw, work, cnfine, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine zzgflong

      !> Perform a coordinate search.
      !>
      !> Legacy interface: calls zz routine zzgfcslv with the legacy context.
      subroutine zzgfcslv ( vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, crdsys, crdnam, relate, refval, tol, adjust, udstep, udrefn, rpt, udrepi, udrepu, udrepf, bail, udbail, mw, nw, work, cnfine, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      implicit none
      character(*) :: vecdef  !! Vector definition.
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Target name.
      character(*) :: ref  !! Reference frame name.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Observer name.
      character(*) :: dref  !! Ray's direction vector frame.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      character(*) :: crdsys  !! Coordinate system name.
      character(*) :: crdnam  !! Coordinate name.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: tol  !! Convergence tolerance.
      real(wp) :: adjust  !! Absolute extremum adjustment value.
      logical :: rpt  !! Progress report flag.
      logical :: bail  !! Bail-out flag.
      procedure(legacy_gf_bail) :: udbail  !! Bail-out status function.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! Workspace window count.
      real(wp) :: work   ( lbcell : mw, nw )  !! Workspace window array.
      real(wp) :: cnfine ( lbcell : * )  !! Confinement window.
      real(wp) :: result ( lbcell : * )  !! Result window.
      procedure(legacy_gf_step) :: udstep  !! Step size routine.
      procedure(legacy_gf_refine) :: udrefn  !! Search refinement routine.
      procedure(legacy_gf_rep_init) :: udrepi  !! Progress report initialization routine.
      procedure(legacy_gf_rep_update) :: udrepu  !! Progress report update routine.
      procedure(legacy_gf_rep_final) :: udrepf  !! Progress report termination routine.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgfcslv ( ctx, vecdef, method, target, ref, abcorr, obsrvr, dref, (dvec), &
         crdsys, crdnam, relate, refval, tol, adjust, bridge_gf_step, bridge_gf_refine, rpt, &
         bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, bail, bridge_gf_bail, mw, &
         nw, work, cnfine, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine zzgfcslv

      !> Determine time intervals when a specified geometric quantity
      !> satisfies a specified mathematical condition.
      !>
      !> Legacy interface: calls ctx%gfevnt with the legacy context.
      subroutine gfevnt ( udstep, udrefn, gquant, qnpars, qpnams, qcpars, qdpars, qipars, qlpars, op, refval, tol, adjust, cnfine, rpt, udrepi, udrepu, udrepf, mw, nw, work, bail, udbail, result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: gquant  !! Type of geometric quantity.
      integer(ip) :: qnpars  !! Number of quantity definition parameters.
      character(*) :: qpnams ( * )  !! Names of quantity definition parameters.
      character(*) :: qcpars ( * )  !! Array of character quantity definition parameters.
      real(wp) :: qdpars ( * )  !! Array of double precision quantity definition parameters.
      integer(ip) :: qipars ( * )  !! Array of integer quantity definition parameters.
      logical :: qlpars ( * )  !! Array of logical quantity definition parameters.
      character(*) :: op  !! Operator that either looks for an extreme value (max, min, local, absolute) or compares the geometric quantity value and a number.
      real(wp) :: refval  !! Reference value.
      real(wp) :: tol  !! Convergence tolerance in seconds
      real(wp) :: adjust  !! Absolute extremum adjustment value.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      logical :: rpt  !! Progress reporter on (.TRUE.) or off (.FALSE.).
      integer(ip) :: mw  !! Size of workspace windows.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array containing workspace windows.
      logical :: bail  !! Logical indicating program interrupt monitoring.
      procedure(legacy_gf_bail) :: udbail  !! Name of a routine that signals a program interrupt.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      procedure(legacy_gf_step) :: udstep  !! Name of the routine that computes and returns a time step.
      procedure(legacy_gf_refine) :: udrefn  !! Name of the routine that computes a refined time.
      procedure(legacy_gf_rep_init) :: udrepi  !! Function that initializes progress reporting.
      procedure(legacy_gf_rep_update) :: udrepu  !! Function that updates the progress report.
      procedure(legacy_gf_rep_final) :: udrepf  !! Function that finalizes progress reporting.
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      procedure(legacy_gf_rep_init), pointer :: outer_udrepi  !! the outer call's routine
      procedure(legacy_gf_rep_update), pointer :: outer_udrepu  !! the outer call's routine
      procedure(legacy_gf_rep_final), pointer :: outer_udrepf  !! the outer call's routine
      procedure(legacy_gf_bail), pointer :: outer_udbail  !! the outer call's routine
      ctx => legacy_context()
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      outer_udrepi => current_gf_rep_init
      current_gf_rep_init => udrepi
      outer_udrepu => current_gf_rep_update
      current_gf_rep_update => udrepu
      outer_udrepf => current_gf_rep_final
      current_gf_rep_final => udrepf
      outer_udbail => current_gf_bail
      current_gf_bail => udbail
      call ctx%gfevnt ( bridge_gf_step, bridge_gf_refine, gquant, qnpars, qpnams, qcpars, qdpars, qipars, qlpars, op, refval, tol, adjust, cnfine, rpt, bridge_gf_rep_init, bridge_gf_rep_update, bridge_gf_rep_final, mw, nw, work, bail, bridge_gf_bail, result )
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      current_gf_rep_init => outer_udrepi
      current_gf_rep_update => outer_udrepu
      current_gf_rep_final => outer_udrepf
      current_gf_bail => outer_udbail
      end subroutine gfevnt

      !> Determine time intervals over which a specified constraint on
      !> observer-target distance is met.
      !>
      !> Legacy interface: calls ctx%gfdist with the legacy context.
      subroutine gfdist ( target, abcorr, obsrvr, relate, &
                          refval, adjust, step,   cnfine, &
                          mw,     nw,     work,   result  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! Workspace window count.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfdist ( target, abcorr, obsrvr, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfdist

      !> Determine time intervals over which a specified constraint on
      !> the observed phase, solar incidence, or emission angle at
      !> a specified target body surface point is met.
      !>
      !> Legacy interface: calls ctx%gfilum with the legacy context.
      subroutine gfilum ( method, angtyp, target, illmn, &
                          fixref, abcorr, obsrvr, spoint, &
                          relate, refval, adjust, step, &
                          cnfine, mw,     nw,     work,  result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzholdd
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: angtyp  !! Type of illumination angle.
      character(*) :: target  !! Name of the target body.
      character(*) :: illmn  !! Name of the illumination source.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      real(wp) :: spoint ( 3 )  !! Body-fixed coordinates of a target surface point.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! Workspace window count.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: spoint is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%gfilum ( method, angtyp, target, illmn, fixref, abcorr, obsrvr, (spoint), relate, &
         refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfilum

      !> Determine time intervals for which a specified constraint
      !> on the phase angle between an illumination source, a target,
      !> and observer body centers is met.
      !>
      !> Legacy interface: calls ctx%gfpa with the legacy context.
      subroutine gfpa ( target, illmn,  abcorr, obsrvr, &
                        relate, refval, adjust, step, &
                        cnfine, mw,     nw,     work, &
                        result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: illmn  !! Name of the illuminating body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfpa ( target, illmn, abcorr, obsrvr, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfpa

      !> Determine time intervals for which a coordinate of an
      !> observer-target position vector satisfies a numerical constraint.
      !>
      !> Legacy interface: calls ctx%gfposc with the legacy context.
      subroutine gfposc ( target, frame,  abcorr, obsrvr, &
                          crdsys, coord,  relate, refval, &
                          adjust, step,   cnfine, mw, &
                          nw,     work,   result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      use pepper_inc_zzholdd
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: frame  !! Name of the reference frame for coordinate calculations.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: crdsys  !! Name of the coordinate system containing COORD.
      character(*) :: coord  !! Name of the coordinate of interest.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfposc ( target, frame, abcorr, obsrvr, crdsys, coord, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfposc

      !> Determine time intervals for which a specified constraint
      !> on the observer-target range rate is met.
      !>
      !> Legacy interface: calls ctx%gfrr with the legacy context.
      subroutine gfrr ( target, abcorr, obsrvr, relate, &
                        refval, adjust, step,   cnfine, &
                        mw,     nw,     work,   result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfrr ( target, abcorr, obsrvr, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfrr

      !> Determine time intervals when the angular separation between
      !> the position vectors of two target bodies relative to an observer
      !> satisfies a numerical relationship.
      !>
      !> Legacy interface: calls ctx%gfsep with the legacy context.
      subroutine gfsep ( targ1,  shape1, frame1, &
                         targ2,  shape2, frame2, &
                         abcorr, obsrvr, relate, &
                         refval, adjust, step, &
                         cnfine, mw,     nw, &
                         work,   result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzholdd
      implicit none
      character(*) :: targ1  !! Name of first body.
      character(*) :: shape1  !! Name of shape model describing the first body.
      character(*) :: frame1  !! The body-fixed reference frame of the first body.
      character(*) :: targ2  !! Name of second body.
      character(*) :: shape2  !! Name of the shape model describing the second body.
      character(*) :: frame2  !! The body-fixed reference frame of the second body.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: relate  !! Operator that either looks for an extreme value (max, min, local, absolute) or compares the angular separation value and REFVAL.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Absolute extremum adjustment value.
      real(wp) :: step  !! Step size in seconds for finding angular separation events.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is restricted.
      integer(ip) :: mw  !! Size of workspace windows.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array containing workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfsep ( targ1, shape1, frame1, targ2, shape2, frame2, abcorr, obsrvr, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfsep

      !> Determine time intervals for which a coordinate of a
      !> surface intercept position vector satisfies a numerical
      !> constraint.
      !>
      !> Legacy interface: calls ctx%gfsntc with the legacy context.
      subroutine gfsntc (  target,  fixref,  method, &
                           abcorr,  obsrvr,  dref, &
                           dvec,    crdsys,  coord, &
                           relate,  refval,  adjust, &
                           step,    cnfine,  mw, &
                           nw,      work,    result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzholdd
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: fixref  !! Body fixed frame associated with TARGET.
      character(*) :: method  !! Name of method type for surface intercept calculation.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: dref  !! Reference frame of direction vector DVEC.
      real(wp) :: dvec   (3)  !! Pointing direction vector from OBSRVR.
      character(*) :: crdsys  !! Name of the coordinate system containing COORD.
      character(*) :: coord  !! Name of the coordinate of interest.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%gfsntc ( target, fixref, method, abcorr, obsrvr, dref, (dvec), crdsys, coord, &
         relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfsntc

      !> Determine time intervals for which a coordinate of an
      !> subpoint position vector satisfies a numerical constraint.
      !>
      !> Legacy interface: calls ctx%gfsubc with the legacy context.
      subroutine gfsubc (  target, fixref, method, &
                           abcorr, obsrvr, crdsys, &
                           coord,  relate, refval, &
                           adjust, step,   cnfine, &
                           mw,     nw,     work,   result )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr
      use pepper_inc_zzholdd
      implicit none
      character(*) :: target  !! Name of the target body.
      character(*) :: fixref  !! Body fixed frame associated with TARGET.
      character(*) :: method  !! Name of method type for subpoint calculation.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obsrvr  !! Name of the observing body.
      character(*) :: crdsys  !! Name of the coordinate system containing COORD.
      character(*) :: coord  !! Name of the coordinate of interest.
      character(*) :: relate  !! Relational operator.
      real(wp) :: refval  !! Reference value.
      real(wp) :: adjust  !! Adjustment value for absolute extrema searches.
      real(wp) :: step  !! Step size used for locating extrema and roots.
      real(wp) :: cnfine ( lbcell : * )  !! SPICE window to which the search is confined.
      integer(ip) :: mw  !! Workspace window size.
      integer(ip) :: nw  !! The number of workspace windows needed for the search.
      real(wp) :: work   ( lbcell : mw, nw )  !! Array of workspace windows.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfsubc ( target, fixref, method, abcorr, obsrvr, crdsys, coord, relate, refval, adjust, step, cnfine, mw, nw, work, result )
      end subroutine gfsubc

end module pepper_legacy_gf
