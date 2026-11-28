!> PEPPER's public interface in one module: `use pepper_module` is all a
!> program needs.
!>
!> It re-exports:
!>
!> * the instance and the per-thread context, type([[pepper_instance]]) and
!>   type([[pepper_context]]) ([[pepper_instance_module]]);
!> * the kinds of PEPPER's integers and reals, `ip` and `wp` ([[pepper_kinds]]);
!> * the named constants ([[pepper_constants]]);
!> * strings of any length, type([[string]]) ([[pepper_containers]]);
!> * windows and sets as plain arrays and their conversions to and from
!>   SPICELIB's cells ([[pepper_windows]]);
!> * what programmatic ephemerides and frames are built from: the provider
!>   types, and the queries passed to them;
!> * kernels built in memory and kernel loaders: type([[kernel_builder]])
!>   and the abstract type([[kernel_loader]]);
!> * the interfaces of the procedures a program passes to the GF searches
!>   (step, refinement, progress report, interrupt check, user-defined
!>   functions).
!>
!>```fortran
!>     use pepper_module
!>     type(pepper_instance), target :: spice
!>     type(pepper_context) :: ctx
!>     real(wp) :: et
!>     call spice%furnsh('kernels.tm')
!>     ctx = spice%new_context()
!>     call ctx%str2et('2026 NOV 8 12:00:00 UTC', et)
!>```
module pepper_module

   use pepper_kinds, only: ip, wp
   use pepper_constants
   use pepper_containers, only: string
   use pepper_windows, only: interval_list, window_to_cell, cell_to_window, &
                             set_to_cell, cell_to_set
   use pepper_instance_module, only: pepper_instance, pepper_context, &
      ! programmatic ephemerides and frames
      ephemeris_provider, frame_provider, ephemeris_procedure, frame_procedure, &
      state_query, position_query, rotation_query, &
      ! kernels built in memory and kernel loaders
      kernel_builder, kernel_loader, &
      ! the procedures passed to the GF searches
      gf_step, gf_refine, gf_rep_init, gf_rep_update, gf_rep_final, gf_bail, &
      gf_scalar, gf_decr_x, gf_boolean

   implicit none

   public

end module pepper_module
