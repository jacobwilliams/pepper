!> Reads past the end of a cell or array (SPICELIB's non-short-circuit
!> loops, such as `do while (i <= card .and. window(i) < left)`; Fortran's
!> `.and.` may evaluate both operands). The windows are assumed-size
!> arguments, which bounds checking cannot see, so the elements just past
!> each full window (and past the array of [[lstcld]]) are signaling NaNs:
!> reading and comparing one raises IEEE's invalid flag, which each case
!> checks along with its result.
program test_window_bounds

   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_signaling_nan, ieee_set_flag, &
                                           ieee_get_flag, ieee_invalid
   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_instance, pepper_context
   use pepper_arrays, only: lstcld, lstcli

   implicit none

   integer(ip), parameter :: lbcell = -5
   integer :: failures = 0
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: ctx
   real(wp) :: snan

   ctx => spice%context()
   snan = ieee_value(1.0_wp, ieee_signaling_nan)

   call test_wninsd()
   call test_wncomd()
   call test_lstcl()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_window_bounds: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
      end if
   end subroutine check

   !> A full window of the intervals [1,2], [3,4], [5,6] (size = cardinality
   !> = 6), followed by two signaling NaNs past the cell.
   subroutine full_window(w)
      real(wp), intent(out) :: w(lbcell:8)  !! the window and the two elements past it
      w = 0.0_wp
      call ctx%ssized(6_ip, w)
      w(1:6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
      call ctx%scardd(6_ip, w)
      w(7:8) = snan
   end subroutine full_window

   !> Whether the invalid flag was raised since it was cleared (and clear it).
   logical function read_past()
      call ieee_get_flag(ieee_invalid, read_past)
      call ieee_set_flag(ieee_invalid, .false.)
   end function read_past

   !> WNINSD: an interval after every interval of a full window (no room:
   !> an error, after the search), and one merging with the last interval.
   subroutine test_wninsd()
      real(wp) :: w(lbcell:8)

      call full_window(w)
      call ieee_set_flag(ieee_invalid, .false.)
      call ctx%wninsd(10.0_wp, 11.0_wp, w)
      call check(.not. read_past(), 'wninsd after the last interval: no read past the window')
      call check(ctx%failed(), 'wninsd into a full window: an error')
      call ctx%reset()

      call full_window(w)
      call ieee_set_flag(ieee_invalid, .false.)
      call ctx%wninsd(5.5_wp, 9.0_wp, w)
      call check(.not. read_past(), 'wninsd merging with the last interval: no read past the window')
      call check(.not. ctx%failed(), 'wninsd merging with the last interval: no error')
      call check(ctx%cardd(w) == 6 .and. w(5) == 5.0_wp .and. w(6) == 9.0_wp, &
                 'wninsd merging with the last interval: [5,9]')
      call ctx%reset()
   end subroutine test_wninsd

   !> WNCOMD: complements over an interval after the window, one that
   !> covers it, and one ending inside its last interval.
   subroutine test_wncomd()
      real(wp) :: w(lbcell:8), res(lbcell:20)

      call full_window(w)
      call ctx%ssized(20_ip, res)
      call ieee_set_flag(ieee_invalid, .false.)
      call ctx%wncomd(7.0_wp, 9.0_wp, w, res)
      call check(.not. read_past(), 'wncomd after the window: no read past it')
      call check(ctx%cardd(res) == 2 .and. res(1) == 7.0_wp .and. res(2) == 9.0_wp, &
                 'wncomd after the window: [7,9]')

      call full_window(w)
      call ieee_set_flag(ieee_invalid, .false.)
      call ctx%wncomd(0.0_wp, 10.0_wp, w, res)
      call check(.not. read_past(), 'wncomd covering the window: no read past it')
      call check(ctx%cardd(res) == 8 .and. all(res(1:8) == [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                 6.0_wp, 10.0_wp]), 'wncomd covering the window: [0,1] [2,3] [4,5] [6,10]')

      call full_window(w)
      call ieee_set_flag(ieee_invalid, .false.)
      call ctx%wncomd(0.0_wp, 5.5_wp, w, res)
      call check(.not. read_past(), 'wncomd ending in the last interval: no read past it')
      call check(ctx%cardd(res) == 6 .and. all(res(1:6) == [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]), &
                 'wncomd ending in the last interval: [0,1] [2,3] [4,5]')
      call check(.not. ctx%failed(), 'wncomd: no error')
      call ctx%reset()
   end subroutine test_wncomd

   !> LSTCLD and LSTCLI: the closest value equal to the array's last
   !> elements (the search for the last equal element stops at N).
   subroutine test_lstcl()
      real(wp) :: a(5)
      integer(ip) :: ia(5)
      a = [1.0_wp, 2.0_wp, 3.0_wp, 3.0_wp, snan]
      call ieee_set_flag(ieee_invalid, .false.)
      call check(lstcld(3.5_wp, 4_ip, a) == 4, 'lstcld: the last of the equal closest elements')
      call check(.not. read_past(), 'lstcld: no read past the array')
      ia = [1, 2, 3, 3, -huge(1_ip)]
      call check(lstcli(4_ip, 4_ip, ia) == 4, 'lstcli: the last of the equal closest elements')
   end subroutine test_lstcl

end program test_window_bounds
