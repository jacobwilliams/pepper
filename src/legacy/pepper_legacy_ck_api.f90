!> The legacy interface's routines of the subsystem ck_api (10 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_ck_api

   use pepper_instance_module, only: pepper_context, pepper_zzckcv01 => zzckcv01, &
      pepper_zzckcv02 => zzckcv02, pepper_zzckcv03 => zzckcv03, pepper_zzckcv04 => zzckcv04, &
      pepper_zzckcv05 => zzckcv05, pepper_zzckcv06 => zzckcv06
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: ckobj, ckgp, ckgpav, zzckcv01, zzckcv02, zzckcv03, zzckcv04, zzckcv05, zzckcv06, &
      ckcov

contains

      !> Find the set of ID codes of all objects in a specified CK file.
      !>
      !> Legacy interface: calls ctx%ckobj with the legacy context.
      subroutine ckobj ( ckfnm, ids )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: ckfnm  !! Name of CK file.
      integer(ip) :: ids ( lbcell : * )  !! Set of ID codes of objects in CK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckobj ( ckfnm, ids )
      end subroutine ckobj

      !> Get pointing (attitude) for a specified spacecraft clock time.
      !>
      !> Legacy interface: calls ctx%ckgp with the legacy context.
      subroutine ckgp ( inst, sclkdp, tol, ref, cmat, clkout, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: inst  !! NAIF ID of instrument, spacecraft, or structure.
      real(wp) :: sclkdp  !! Encoded spacecraft clock time.
      real(wp) :: tol  !! Time tolerance.
      character(*) :: ref  !! Reference frame.
      real(wp) :: cmat   ( 3, 3 )  !! C-matrix pointing data.
      real(wp) :: clkout  !! Output encoded spacecraft clock time.
      logical :: found  !! .TRUE. when requested pointing is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgp ( inst, sclkdp, tol, ref, cmat, clkout, found )
      end subroutine ckgp

      !> Get pointing (attitude) and angular velocity for a specified
      !> spacecraft clock time.
      !>
      !> Legacy interface: calls ctx%ckgpav with the legacy context.
      subroutine ckgpav ( inst, sclkdp, tol, ref, cmat, av, clkout, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: inst  !! NAIF ID of instrument, spacecraft, or structure.
      real(wp) :: sclkdp  !! Encoded spacecraft clock time.
      real(wp) :: tol  !! Time tolerance.
      character(*) :: ref  !! Reference frame.
      real(wp) :: cmat   ( 3, 3 )  !! C-matrix pointing data.
      real(wp) :: av     ( 3    )  !! Angular velocity vector.
      real(wp) :: clkout  !! Output encoded spacecraft clock time.
      logical :: found  !! .TRUE. when requested pointing is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgpav ( inst, sclkdp, tol, ref, cmat, av, clkout, found )
      end subroutine ckgpav

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckcv01 with the legacy context.
      subroutine zzckcv01 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_windows, only: interval_list
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a C-kernel open for read access
      integer(ip) :: arrbeg  !! Beginning DAF address
      integer(ip) :: arrend  !! Ending DAF address
      integer(ip) :: sclkid  !! ID of SCLK associated with segment.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: schedl ( lbcell:* )  !! An initialized window/schedule of interval
      type(interval_list) :: list  ! the coverage of the segment
      integer(ip) :: k
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the routine adds the coverage to an interval list; here it
      ! is added to the window cell SCHEDL.
      call pepper_zzckcv01 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, list )
      do k = 1, list%n
         call ctx%wninsd ( list%intervals(1,k), list%intervals(2,k), schedl )
      end do
      end subroutine zzckcv01

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckcv02 with the legacy context.
      subroutine zzckcv02 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_windows, only: interval_list
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a C-kernel open for read access
      integer(ip) :: arrbeg  !! Beginning DAF address
      integer(ip) :: arrend  !! Ending DAF address
      integer(ip) :: sclkid  !! ID of SCLK associated with segment.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: schedl ( lbcell:* )  !! An initialized window/schedule of interval
      type(interval_list) :: list  ! the coverage of the segment
      integer(ip) :: k
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the routine adds the coverage to an interval list; here it
      ! is added to the window cell SCHEDL.
      call pepper_zzckcv02 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, list )
      do k = 1, list%n
         call ctx%wninsd ( list%intervals(1,k), list%intervals(2,k), schedl )
      end do
      end subroutine zzckcv02

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckcv03 with the legacy context.
      subroutine zzckcv03 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_windows, only: interval_list
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a C-kernel open for read access
      integer(ip) :: arrbeg  !! Beginning DAF address
      integer(ip) :: arrend  !! Ending DAF address
      integer(ip) :: sclkid  !! ID of SCLK associated with segment.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: schedl ( lbcell:* )  !! An initialized window/schedule of interval
      type(interval_list) :: list  ! the coverage of the segment
      integer(ip) :: k
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the routine adds the coverage to an interval list; here it
      ! is added to the window cell SCHEDL.
      call pepper_zzckcv03 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, list )
      do k = 1, list%n
         call ctx%wninsd ( list%intervals(1,k), list%intervals(2,k), schedl )
      end do
      end subroutine zzckcv03

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckcv04 with the legacy context.
      subroutine zzckcv04 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_windows, only: interval_list
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! Handle of a C-kernel open for read access
      integer(ip) :: arrbeg  !! Beginning DAF address
      integer(ip) :: arrend  !! Ending DAF address
      integer(ip) :: sclkid  !! ID of SCLK associated with segment.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: schedl ( lbcell:* )  !! An initialized window/schedule of interval
      type(interval_list) :: list  ! the coverage of the segment
      integer(ip) :: k
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the routine adds the coverage to an interval list; here it
      ! is added to the window cell SCHEDL.
      call pepper_zzckcv04 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, list )
      do k = 1, list%n
         call ctx%wninsd ( list%intervals(1,k), list%intervals(2,k), schedl )
      end do
      end subroutine zzckcv04

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckcv05 with the legacy context.
      subroutine zzckcv05 ( handle,  arrbeg,  arrend,  sclkid, &
                            dc,      tol,     timsys,  schedl  )
      use pepper_windows, only: interval_list
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck05
      implicit none
      integer(ip) :: handle  !! Handle of a C-kernel open for read access
      integer(ip) :: arrbeg  !! Beginning DAF address
      integer(ip) :: arrend  !! Ending DAF address
      integer(ip) :: sclkid  !! ID of SCLK associated with segment.
      real(wp) :: dc     ( 2 )  !! D.p. component of CK segment descriptor.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: schedl ( lbcell:* )  !! An initialized window/schedule of interval
      type(interval_list) :: list  ! the coverage of the segment
      integer(ip) :: k
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the routine adds the coverage to an interval list; here it
      ! is added to the window cell SCHEDL.
      call pepper_zzckcv05 ( ctx, handle, arrbeg, arrend, sclkid, dc, tol, timsys, list )
      do k = 1, list%n
         call ctx%wninsd ( list%intervals(1,k), list%intervals(2,k), schedl )
      end do
      end subroutine zzckcv05

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckcv06 with the legacy context.
      subroutine zzckcv06 ( handle, arrbeg, arrend, sclkid, &
                            dc,     tol,    timsys, schedl )
      use pepper_windows, only: interval_list
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck06
      implicit none
      integer(ip) :: handle  !! Handle of a C-kernel open for read access.
      integer(ip) :: arrbeg  !! Beginning DAF address.
      integer(ip) :: arrend  !! Ending DAF address.
      integer(ip) :: sclkid  !! ID of SCLK associated with segment.
      real(wp) :: dc     ( 2 )  !! D.p. component of CK segment descriptor.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: schedl ( lbcell:* )  !! An initialized window/schedule of interval
      type(interval_list) :: list  ! the coverage of the segment
      integer(ip) :: k
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the routine adds the coverage to an interval list; here it
      ! is added to the window cell SCHEDL.
      call pepper_zzckcv06 ( ctx, handle, arrbeg, arrend, sclkid, dc, tol, timsys, list )
      do k = 1, list%n
         call ctx%wninsd ( list%intervals(1,k), list%intervals(2,k), schedl )
      end do
      end subroutine zzckcv06

      !> Find the coverage window for a specified object in a specified CK
      !> file.
      !>
      !> Legacy interface: calls ctx%ckcov with the legacy context.
      subroutine ckcov ( ckfnm, idcode, needav, level, &
                         tol,   timsys, cover          )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: ckfnm  !! Name of CK file.
      integer(ip) :: idcode  !! ID code of object.
      logical :: needav  !! Flag indicating whether angular velocity is needed.
      character(*) :: level  !! Coverage level: 'SEGMENT' OR 'INTERVAL'.
      real(wp) :: tol  !! Tolerance in ticks.
      character(*) :: timsys  !! Time system used to represent coverage.
      real(wp) :: cover ( lbcell : * )  !! Window giving coverage for IDCODE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckcov ( ckfnm, idcode, needav, level, tol, timsys, cover )
      end subroutine ckcov

end module pepper_legacy_ck_api
