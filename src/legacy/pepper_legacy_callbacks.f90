!> The callbacks of the legacy interface ([[pepper_legacy]]): the
!> interfaces of the routines a SPICELIB program passes (legacy_*, without
!> the context), and the bridges (bridge_*), with the module API's
!> interfaces, that call the routine a legacy routine was given (current_*).
module pepper_legacy_callbacks

   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_context
   use pepper_instance_module, only: zzdsk_compare, zzspk_state, zzsfx_near, zzsfx_maxrad, zzsfx_rayx, &
      zztan_cond, gf_step, gf_refine, gf_rep_init, gf_rep_update, gf_rep_final, gf_bail, gf_cond, &
      gf_ref, gf_boolean, gf_decr, gf_decr_x, gf_scalar
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: legacy_zzdsk_compare, legacy_zzspk_state, legacy_zzsfx_near, legacy_zzsfx_maxrad, &
      legacy_zzsfx_rayx, legacy_zztan_cond, legacy_gf_step, legacy_gf_refine, legacy_gf_rep_init, &
      legacy_gf_rep_update, legacy_gf_rep_final, legacy_gf_bail, legacy_gf_cond, legacy_gf_ref, &
      legacy_gf_scalar, legacy_gf_boolean, legacy_gf_decr, legacy_gf_decr_x
   public :: current_zzdsk_compare, current_zzspk_state, current_zzsfx_near, &
      current_zzsfx_maxrad, current_zzsfx_rayx, current_zztan_cond, current_gf_step, &
      current_gf_refine, current_gf_rep_init, current_gf_rep_update, current_gf_rep_final, &
      current_gf_bail, current_gf_cond, current_gf_ref, current_gf_boolean, current_gf_decr, &
      current_gf_decr_x, current_gf_scalar
   public :: bridge_zzdsk_compare, bridge_zzspk_state, bridge_zzsfx_near, bridge_zzsfx_maxrad, &
      bridge_zzsfx_rayx, bridge_zztan_cond, bridge_gf_step, bridge_gf_refine, bridge_gf_rep_init, &
      bridge_gf_rep_update, bridge_gf_rep_final, bridge_gf_bail, bridge_gf_cond, bridge_gf_ref, &
      bridge_gf_boolean, bridge_gf_decr, bridge_gf_decr_x, bridge_gf_scalar

   abstract interface
      !> ZZDSK_COMPARE in SPICELIB's form (without the context).
      logical function legacy_zzdsk_compare ( handle, dladsc, dskdsc )
         import :: ip, wp
         integer(ip) :: handle  !! handle of the DSK file of the segment
         integer(ip) :: dladsc ( * )  !! DLA descriptor of the segment
         real(wp) :: dskdsc ( * )  !! DSK descriptor of the segment
      end function legacy_zzdsk_compare

      !> ZZSPK_STATE in SPICELIB's form (without the context).
      subroutine legacy_zzspk_state ( et, ref, center, state )
         import :: ip, wp
         real(wp) :: et  !! epoch (TDB seconds past J2000)
         character(*) :: ref  !! inertial reference frame of the state
         integer(ip) :: center  !! ID code of the center of motion
         real(wp) :: state ( 6 )  !! state of the object
      end subroutine legacy_zzspk_state

      !> ZZSFX_NEAR in SPICELIB's form (without the context).
      subroutine legacy_zzsfx_near ( vertex, raydir, et, pnear, dist )
         import :: ip, wp
         real(wp) :: vertex ( 3 )  !! the ray's vertex
         real(wp) :: raydir ( 3 )  !! the ray's direction vector
         real(wp) :: et  !! evaluation epoch (TDB seconds past J2000)
         real(wp) :: pnear ( 3 )  !! near point to the ray on the surface
         real(wp) :: dist  !! distance between the near point and the ray
      end subroutine legacy_zzsfx_near

      !> ZZSFX_MAXRAD in SPICELIB's form (without the context).
      subroutine legacy_zzsfx_maxrad ( maxrad )
         import :: ip, wp
         real(wp) :: maxrad  !! radius of the outer bounding sphere
      end subroutine legacy_zzsfx_maxrad

      !> ZZSFX_RAYX in SPICELIB's form (without the context).
      subroutine legacy_zzsfx_rayx ( vertex, raydir, et, spoint, found )
         import :: ip, wp
         real(wp) :: vertex ( 3 )  !! the ray's vertex
         real(wp) :: raydir ( 3 )  !! the ray's direction vector
         real(wp) :: et  !! evaluation epoch (TDB seconds past J2000)
         real(wp) :: spoint ( 3 )  !! surface intercept point
         logical :: found  !! the intercept exists
      end subroutine legacy_zzsfx_rayx

      !> ZZTAN_COND in SPICELIB's form (without the context).
      subroutine legacy_zztan_cond ( angle, ocultd, point )
         import :: ip, wp
         real(wp) :: angle  !! angle of the ray
         logical :: ocultd  !! the ray is occulted
         real(wp) :: point ( 3 )  !! tangent point on the target
      end subroutine legacy_zztan_cond

      !> GF_STEP in SPICELIB's form (without the context).
      subroutine legacy_gf_step ( time, step )
         import :: ip, wp
         real(wp) :: time  !! the time (TDB seconds past J2000)
         real(wp) :: step  !! the time step (seconds)
      end subroutine legacy_gf_step

      !> GF_REFINE in SPICELIB's form (without the context).
      subroutine legacy_gf_refine ( t1, t2, s1, s2, t )
         import :: ip, wp
         real(wp) :: t1  !! one of the times
         real(wp) :: t2  !! the other time
         logical :: s1  !! the condition at T1
         logical :: s2  !! the condition at T2
         real(wp) :: t  !! the time between T1 and T2
      end subroutine legacy_gf_refine

      !> GF_REP_INIT in SPICELIB's form (without the context).
      subroutine legacy_gf_rep_init ( window, begmss, endmss )
         import :: ip, wp, lbcell
         real(wp) :: window ( lbcell : * )  !! the window over which the search runs
         character(*) :: begmss  !! beginning of the text of the report
         character(*) :: endmss  !! end of the text of the report
      end subroutine legacy_gf_rep_init

      !> GF_REP_UPDATE in SPICELIB's form (without the context).
      subroutine legacy_gf_rep_update ( ivbeg, ivend, time )
         import :: ip, wp
         real(wp) :: ivbeg  !! start time of the work interval
         real(wp) :: ivend  !! end time of the work interval
         real(wp) :: time  !! current time of the search
      end subroutine legacy_gf_rep_update

      !> GF_REP_FINAL in SPICELIB's form (without the context).
      subroutine legacy_gf_rep_final (  )
         import :: ip, wp
      end subroutine legacy_gf_rep_final

      !> GF_BAIL in SPICELIB's form (without the context).
      logical function legacy_gf_bail (  )
         import :: ip, wp
      end function legacy_gf_bail

      !> GF_COND in SPICELIB's form (without the context).
      subroutine legacy_gf_cond ( et, xbool )
         import :: ip, wp
         real(wp) :: et  !! the time (TDB seconds past J2000)
         logical :: xbool  !! the condition at ET
      end subroutine legacy_gf_cond

      !> GF_REF in SPICELIB's form (without the context).
      subroutine legacy_gf_ref ( refval )
         import :: ip, wp
         real(wp) :: refval  !! the reference value
      end subroutine legacy_gf_ref

      !> GF_SCALAR in SPICELIB's form (without the context).
      subroutine legacy_gf_scalar ( et, value )
         import :: ip, wp
         real(wp) :: et  !! the time (TDB seconds past J2000)
         real(wp) :: value  !! the value of the function
      end subroutine legacy_gf_scalar

      !> GF_BOOLEAN in SPICELIB's form (without the context).
      subroutine legacy_gf_boolean ( udfunc, et, xbool )
         import :: ip, wp, legacy_gf_scalar
         procedure(legacy_gf_scalar) :: udfunc  !! the scalar function
         real(wp) :: et  !! the time (TDB seconds past J2000)
         logical :: xbool  !! the condition at ET
      end subroutine legacy_gf_boolean

      !> GF_DECR in SPICELIB's form (without the context).
      subroutine legacy_gf_decr ( et, xbool )
         import :: ip, wp
         real(wp) :: et  !! the time (TDB seconds past J2000)
         logical :: xbool  !! the quantity is decreasing at ET
      end subroutine legacy_gf_decr

      !> GF_DECR_X in SPICELIB's form (without the context).
      subroutine legacy_gf_decr_x ( udfunc, et, xbool )
         import :: ip, wp, legacy_gf_scalar
         procedure(legacy_gf_scalar) :: udfunc  !! the scalar function
         real(wp) :: et  !! the time (TDB seconds past J2000)
         logical :: xbool  !! the function is decreasing at ET
      end subroutine legacy_gf_decr_x
   end interface

   !> The legacy caller's ZZDSK_COMPARE routine (during a call).
   procedure(legacy_zzdsk_compare), pointer :: current_zzdsk_compare => null()
   !> The legacy caller's ZZSPK_STATE routine (during a call).
   procedure(legacy_zzspk_state), pointer :: current_zzspk_state => null()
   !> The legacy caller's ZZSFX_NEAR routine (during a call).
   procedure(legacy_zzsfx_near), pointer :: current_zzsfx_near => null()
   !> The legacy caller's ZZSFX_MAXRAD routine (during a call).
   procedure(legacy_zzsfx_maxrad), pointer :: current_zzsfx_maxrad => null()
   !> The legacy caller's ZZSFX_RAYX routine (during a call).
   procedure(legacy_zzsfx_rayx), pointer :: current_zzsfx_rayx => null()
   !> The legacy caller's ZZTAN_COND routine (during a call).
   procedure(legacy_zztan_cond), pointer :: current_zztan_cond => null()
   !> The legacy caller's GF_STEP routine (during a call).
   procedure(legacy_gf_step), pointer :: current_gf_step => null()
   !> The legacy caller's GF_REFINE routine (during a call).
   procedure(legacy_gf_refine), pointer :: current_gf_refine => null()
   !> The legacy caller's GF_REP_INIT routine (during a call).
   procedure(legacy_gf_rep_init), pointer :: current_gf_rep_init => null()
   !> The legacy caller's GF_REP_UPDATE routine (during a call).
   procedure(legacy_gf_rep_update), pointer :: current_gf_rep_update => null()
   !> The legacy caller's GF_REP_FINAL routine (during a call).
   procedure(legacy_gf_rep_final), pointer :: current_gf_rep_final => null()
   !> The legacy caller's GF_BAIL routine (during a call).
   procedure(legacy_gf_bail), pointer :: current_gf_bail => null()
   !> The legacy caller's GF_COND routine (during a call).
   procedure(legacy_gf_cond), pointer :: current_gf_cond => null()
   !> The legacy caller's GF_REF routine (during a call).
   procedure(legacy_gf_ref), pointer :: current_gf_ref => null()
   !> The legacy caller's GF_BOOLEAN routine (during a call).
   procedure(legacy_gf_boolean), pointer :: current_gf_boolean => null()
   !> The legacy caller's GF_DECR routine (during a call).
   procedure(legacy_gf_decr), pointer :: current_gf_decr => null()
   !> The legacy caller's GF_DECR_X routine (during a call).
   procedure(legacy_gf_decr_x), pointer :: current_gf_decr_x => null()
   !> The legacy caller's GF_SCALAR routine (during a call).
   procedure(legacy_gf_scalar), pointer :: current_gf_scalar => null()

contains

   !> A zzdsk_compare that calls the legacy caller's routine (current_zzdsk_compare).
   logical function bridge_zzdsk_compare ( ctx, handle, dladsc, dskdsc )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      integer(ip), intent(in) :: handle  !! handle of the DSK file of the segment
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor of the segment
      real(wp), intent(in) :: dskdsc ( * )  !! DSK descriptor of the segment
      bridge_zzdsk_compare = current_zzdsk_compare ( handle, dladsc, dskdsc )
   end function bridge_zzdsk_compare

   !> A zzspk_state that calls the legacy caller's routine (current_zzspk_state).
   subroutine bridge_zzspk_state ( ctx, et, ref, center, state )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: et  !! epoch (TDB seconds past J2000)
      character(*), intent(in) :: ref  !! inertial reference frame of the state
      integer(ip), intent(out) :: center  !! ID code of the center of motion
      real(wp), intent(out) :: state ( 6 )  !! state of the object
      call current_zzspk_state ( et, ref, center, state )
   end subroutine bridge_zzspk_state

   !> A zzsfx_near that calls the legacy caller's routine (current_zzsfx_near).
   subroutine bridge_zzsfx_near ( ctx, vertex, raydir, et, pnear, dist )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: vertex ( 3 )  !! the ray's vertex
      real(wp), intent(in) :: raydir ( 3 )  !! the ray's direction vector
      real(wp), intent(in) :: et  !! evaluation epoch (TDB seconds past J2000)
      real(wp), intent(out) :: pnear ( 3 )  !! near point to the ray on the surface
      real(wp), intent(out) :: dist  !! distance between the near point and the ray
      call current_zzsfx_near ( vertex, raydir, et, pnear, dist )
   end subroutine bridge_zzsfx_near

   !> A zzsfx_maxrad that calls the legacy caller's routine (current_zzsfx_maxrad).
   subroutine bridge_zzsfx_maxrad ( ctx, maxrad )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(out) :: maxrad  !! radius of the outer bounding sphere
      call current_zzsfx_maxrad ( maxrad )
   end subroutine bridge_zzsfx_maxrad

   !> A zzsfx_rayx that calls the legacy caller's routine (current_zzsfx_rayx).
   subroutine bridge_zzsfx_rayx ( ctx, vertex, raydir, et, spoint, found )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: vertex ( 3 )  !! the ray's vertex
      real(wp), intent(in) :: raydir ( 3 )  !! the ray's direction vector
      real(wp), intent(in) :: et  !! evaluation epoch (TDB seconds past J2000)
      real(wp), intent(out) :: spoint ( 3 )  !! surface intercept point
      logical, intent(out) :: found  !! the intercept exists
      call current_zzsfx_rayx ( vertex, raydir, et, spoint, found )
   end subroutine bridge_zzsfx_rayx

   !> A zztan_cond that calls the legacy caller's routine (current_zztan_cond).
   subroutine bridge_zztan_cond ( ctx, angle, ocultd, point )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: angle  !! angle of the ray
      logical, intent(out) :: ocultd  !! the ray is occulted
      real(wp), intent(out) :: point ( 3 )  !! tangent point on the target
      call current_zztan_cond ( angle, ocultd, point )
   end subroutine bridge_zztan_cond

   !> A gf_step that calls the legacy caller's routine (current_gf_step).
   subroutine bridge_gf_step ( ctx, time, step )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: time  !! the time (TDB seconds past J2000)
      real(wp), intent(out) :: step  !! the time step (seconds)
      call current_gf_step ( time, step )
   end subroutine bridge_gf_step

   !> A gf_refine that calls the legacy caller's routine (current_gf_refine).
   subroutine bridge_gf_refine ( ctx, t1, t2, s1, s2, t )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: t1  !! one of the times
      real(wp), intent(in) :: t2  !! the other time
      logical, intent(in) :: s1  !! the condition at T1
      logical, intent(in) :: s2  !! the condition at T2
      real(wp), intent(out) :: t  !! the time between T1 and T2
      call current_gf_refine ( t1, t2, s1, s2, t )
   end subroutine bridge_gf_refine

   !> A gf_rep_init that calls the legacy caller's routine (current_gf_rep_init).
   subroutine bridge_gf_rep_init ( ctx, window, begmss, endmss )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: window ( lbcell : * )  !! the window over which the search runs
      character(*), intent(in) :: begmss  !! beginning of the text of the report
      character(*), intent(in) :: endmss  !! end of the text of the report
      call current_gf_rep_init ( window, begmss, endmss )
   end subroutine bridge_gf_rep_init

   !> A gf_rep_update that calls the legacy caller's routine (current_gf_rep_update).
   subroutine bridge_gf_rep_update ( ctx, ivbeg, ivend, time )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: ivbeg  !! start time of the work interval
      real(wp), intent(in) :: ivend  !! end time of the work interval
      real(wp), intent(in) :: time  !! current time of the search
      call current_gf_rep_update ( ivbeg, ivend, time )
   end subroutine bridge_gf_rep_update

   !> A gf_rep_final that calls the legacy caller's routine (current_gf_rep_final).
   subroutine bridge_gf_rep_final ( ctx )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      call current_gf_rep_final (  )
   end subroutine bridge_gf_rep_final

   !> A gf_bail that calls the legacy caller's routine (current_gf_bail).
   logical function bridge_gf_bail ( ctx )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      bridge_gf_bail = current_gf_bail (  )
   end function bridge_gf_bail

   !> A gf_cond that calls the legacy caller's routine (current_gf_cond).
   subroutine bridge_gf_cond ( ctx, et, xbool )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: et  !! the time (TDB seconds past J2000)
      logical, intent(out) :: xbool  !! the condition at ET
      call current_gf_cond ( et, xbool )
   end subroutine bridge_gf_cond

   !> A gf_ref that calls the legacy caller's routine (current_gf_ref).
   subroutine bridge_gf_ref ( ctx, refval )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: refval  !! the reference value
      call current_gf_ref ( refval )
   end subroutine bridge_gf_ref

   !> A gf_boolean that calls the legacy caller's routine (current_gf_boolean).
   subroutine bridge_gf_boolean ( ctx, udfunc, et, xbool )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      procedure(gf_scalar) :: udfunc  !! the scalar function
      real(wp), intent(in) :: et  !! the time (TDB seconds past J2000)
      logical, intent(out) :: xbool  !! the condition at ET
      call current_gf_boolean ( current_gf_scalar, et, xbool )
   end subroutine bridge_gf_boolean

   !> A gf_decr that calls the legacy caller's routine (current_gf_decr).
   subroutine bridge_gf_decr ( ctx, et, xbool )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: et  !! the time (TDB seconds past J2000)
      logical, intent(out) :: xbool  !! the quantity is decreasing at ET
      call current_gf_decr ( et, xbool )
   end subroutine bridge_gf_decr

   !> A gf_decr_x that calls the legacy caller's routine (current_gf_decr_x).
   subroutine bridge_gf_decr_x ( ctx, udfunc, et, xbool )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      procedure(gf_scalar) :: udfunc  !! the scalar function
      real(wp), intent(in) :: et  !! the time (TDB seconds past J2000)
      logical, intent(out) :: xbool  !! the function is decreasing at ET
      call current_gf_decr_x ( current_gf_scalar, et, xbool )
   end subroutine bridge_gf_decr_x

   !> A gf_scalar that calls the legacy caller's routine (current_gf_scalar).
   subroutine bridge_gf_scalar ( ctx, et, value )
      class(pepper_context), intent(inout) :: ctx  !! the context (not used)
      real(wp), intent(in) :: et  !! the time (TDB seconds past J2000)
      real(wp), intent(out) :: value  !! the value of the function
      call current_gf_scalar ( et, value )
   end subroutine bridge_gf_scalar

end module pepper_legacy_callbacks
