!> Frame transformations through the module API with two contexts: a
!> text-kernel (TK) frame loaded through one context is known to the other
!> (frame names and IDs), PXFORM gives the same rotation in both, and a
!> change of the frame's definition in the pool is seen by both (TKFRAM
!> watches its variables with a watch of its own in each context).
program test_frames

   use pepper_kinds, only: ip, wp
   use pepper_constants, only: rpd
   use pepper_instance_module, only: pepper_instance, pepper_context

   implicit none

   character(*), parameter :: fk = 'test_frames.tf'
   integer(ip), parameter :: tkid = 1500001
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: a  !! the instance's context (it loads the kernels)
   type(pepper_context) :: b
   integer :: failures = 0
   real(wp) :: ra(3, 3), rb(3, 3)
   integer(ip) :: code
   character(32) :: name

   a => spice%context()
   b = spice%new_context()

   ! A built-in frame needs no kernels: ECLIPJ2000 is J2000 rotated about
   ! X by the obliquity of the ecliptic (84381.448 arcseconds).
   call b%pxform('J2000', 'ECLIPJ2000', 0.0_wp, rb)
   call check(.not. b%failed() .and. abs(rb(1, 1) - 1.0_wp) < 1.0e-14_wp .and. &
              abs(rb(2, 2) - cos(84381.448_wp / 3600.0_wp * rpd)) < 1.0e-14_wp, &
              'pxform J2000 to ECLIPJ2000')

   ! b looks for the TK frame before it is defined
   call b%namfrm('PEPPER_TK', code)
   call check(.not. b%failed() .and. code == 0, 'b: PEPPER_TK is unknown before the kernel')

   call delete_file()
   call write_fk(30.0_wp)
   call spice%furnsh(fk)
   call check(.not. a%failed(), 'a loads the frame kernel')

   call b%namfrm('PEPPER_TK', code)
   call check(.not. b%failed() .and. code == tkid, 'b knows PEPPER_TK (loaded by a)')
   call b%frmnam(tkid, name)
   call check(name == 'PEPPER_TK', 'b: frmnam of the TK frame')

   call a%pxform('J2000', 'PEPPER_TK', 0.0_wp, ra)
   call b%pxform('J2000', 'PEPPER_TK', 0.0_wp, rb)
   call check(.not. a%failed() .and. .not. b%failed(), 'pxform to PEPPER_TK in a and b')
   call check(all(ra == rb), 'a and b give the same rotation')
   call check(abs(rb(3, 3) - 1.0_wp) < 1.0e-14_wp .and. &
              abs(abs(rb(1, 2)) - sin(30.0_wp * rpd)) < 1.0e-14_wp, 'the rotation is 30 degrees about Z')

   ! a changes the frame's angle: both see it
   call spice%pdpool('TKFRAME_1500001_ANGLES', 3_ip, [0.0_wp, 0.0_wp, 60.0_wp])
   call b%pxform('J2000', 'PEPPER_TK', 0.0_wp, rb)
   call check(abs(abs(rb(1, 2)) - sin(60.0_wp * rpd)) < 1.0e-14_wp, 'b sees the new angle')
   call a%pxform('J2000', 'PEPPER_TK', 0.0_wp, ra)
   call check(all(ra == rb), 'a sees the new angle too')

   ! after kclear the frame is gone in both
   call spice%kclear()
   call b%namfrm('PEPPER_TK', code)
   call check(.not. b%failed() .and. code == 0, 'b: PEPPER_TK is unknown after kclear')
   call b%pxform('J2000', 'PEPPER_TK', 0.0_wp, rb)
   call check(b%failed(), 'b: pxform to an unknown frame fails')
   call b%reset()

   call delete_file()

   call test_cubic_phase_angles()

   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_frames: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      character(1840) :: msg
      if (.not. ok) then
         failures = failures + 1
         print '(2a)', 'FAILED: ', what
         if (a%failed()) then
            call a%getmsg('LONG', msg)
            print '(2a)', '   a: ', trim(msg)
         end if
         if (b%failed()) then
            call b%getmsg('LONG', msg)
            print '(2a)', '   b: ', trim(msg)
         end if
      end if
   end subroutine check

   !> Remove the test file if it exists.
   subroutine delete_file()
      logical :: exists
      integer :: unit
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      inquire (file=fk, exist=exists)
      if (exists) then
         inquire (file=fk, opened=still_open)
         if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//fk
         open (newunit=unit, file=fk, status='old')
         close (unit, status='delete')
      end if
   end subroutine delete_file

   !> Write a frame kernel defining PEPPER_TK: J2000 rotated about Z.
   subroutine write_fk(angle)
      real(wp), intent(in) :: angle  !! the rotation angle (degrees)
      integer :: unit
      open (newunit=unit, file=fk, status='new', action='write')
      write (unit, '(a)') 'KPL/FK'
      write (unit, '(a)') '\begindata'
      write (unit, '(a)') 'FRAME_PEPPER_TK = 1500001'
      write (unit, '(a)') 'FRAME_1500001_NAME = ''PEPPER_TK'''
      write (unit, '(a)') 'FRAME_1500001_CLASS = 4'
      write (unit, '(a)') 'FRAME_1500001_CLASS_ID = 1500001'
      write (unit, '(a)') 'FRAME_1500001_CENTER = 399'
      write (unit, '(a)') 'TKFRAME_1500001_RELATIVE = ''J2000'''
      write (unit, '(a)') 'TKFRAME_1500001_SPEC = ''ANGLES'''
      write (unit, '(a)') 'TKFRAME_1500001_UNITS = ''DEGREES'''
      write (unit, '(a)') 'TKFRAME_1500001_AXES = ( 3, 1, 3 )'
      write (unit, '(a, f5.1, a)') 'TKFRAME_1500001_ANGLES = ( 0.0, 0.0, ', angle, ' )'
      write (unit, '(a)') '\begintext'
      close (unit)
   end subroutine write_fk

   !> TISBOD's derivative block for a body whose nutation/precession angle
   !> is a cubic (MAX_PHASE_DEGREE = 3) matches a central difference of its
   !> rotation. SPICELIB N0067 divides the cubic term's derivative by T**2
   !> instead of T (seconds per century), so its rate was about 40% off here.
   !> The pole and prime meridian are fixed, so the angle drives all the
   !> motion.
   subroutine test_cubic_phase_angles()
      integer(ip), parameter :: body = 9001
      real(wp), parameter :: et = 1.0e9_wp  !! about 0.32 Julian centuries past J2000
      real(wp), parameter :: h = 1.0e5_wp  !! the difference step (seconds)
      real(wp) :: tsipm(6, 6), tplus(6, 6), tminus(6, 6), drot(3, 3)
      call spice%pdpool('BODY9001_POLE_RA', 3_ip, [10.0_wp, 0.0_wp, 0.0_wp])
      call spice%pdpool('BODY9001_POLE_DEC', 3_ip, [60.0_wp, 0.0_wp, 0.0_wp])
      call spice%pdpool('BODY9001_PM', 3_ip, [20.0_wp, 0.0_wp, 0.0_wp])
      call spice%pdpool('BODY9001_MAX_PHASE_DEGREE', 1_ip, [3.0_wp])
      call spice%pdpool('BODY9001_NUT_PREC_ANGLES', 4_ip, [10.0_wp, 1000.0_wp, 2000.0_wp, 5000.0_wp])
      call spice%pdpool('BODY9001_NUT_PREC_RA', 1_ip, [0.5_wp])
      call spice%pdpool('BODY9001_NUT_PREC_DEC', 1_ip, [0.5_wp])
      call spice%pdpool('BODY9001_NUT_PREC_PM', 1_ip, [0.5_wp])
      call a%tisbod('J2000', body, et, tsipm)
      call a%tisbod('J2000', body, et + h, tplus)
      call a%tisbod('J2000', body, et - h, tminus)
      call check(.not. a%failed(), 'tisbod with cubic phase angles')
      drot = (tplus(1:3, 1:3) - tminus(1:3, 1:3)) / (2*h)
      call check(maxval(abs(tsipm(4:6, 1:3) - drot)) < 1.0e-5_wp * maxval(abs(drot)), &
                 'tisbod: the rate with cubic phase angles matches a central difference')
      call spice%clpool()
   end subroutine test_cubic_phase_angles

end program test_frames
