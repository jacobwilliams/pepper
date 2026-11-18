!> The legacy interface's routines of the subsystem pck_api (4 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_pck_api

   use pepper_instance_module, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: pckcov, pckeul, pckfrm, bodeul

contains

      !> Find the coverage window for a specified reference frame in a
      !> specified binary PCK file.
      !>
      !> Legacy interface: calls ctx%pckcov with the legacy context.
      subroutine pckcov ( pckfnm, idcode, cover )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: pckfnm  !! Name of PCK file.
      integer(ip) :: idcode  !! Class ID code of PCK reference frame.
      real(wp) :: cover  ( lbcell : * )  !! Window giving coverage in PCKFNM for IDCODE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckcov ( pckfnm, idcode, cover )
      end subroutine pckcov

      !> This routine is obsolete. It supports only the type 02 binary
      !> PCK format. It is maintained only for backward compatibility
      !>
      !> Legacy interface: calls ctx%pckeul with the legacy context.
      subroutine pckeul ( body, et, found, ref, eulang )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! ID code of body
      real(wp) :: et  !! Epoch of transformation
      logical :: found  !! .TRUE. if ET, BODY found in a PCK file
      character(*) :: ref  !! Name of inertial ref. frame of state
      real(wp) :: eulang   ( 6 )  !! Euler angles and their derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckeul ( body, et, found, ref, eulang )
      end subroutine pckeul

      !> Find the set of reference frame class ID codes of all frames
      !> in a specified binary PCK file.
      !>
      !> Legacy interface: calls ctx%pckfrm with the legacy context.
      subroutine pckfrm ( pckfnm, ids )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: pckfnm  !! Name of PCK file.
      integer(ip) :: ids    ( lbcell : * )  !! Set of frame class ID codes of frames in PCK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckfrm ( pckfnm, ids )
      end subroutine pckfrm

      !> Return the Euler angles needed to compute the transformation from
      !> inertial to body-fixed coordinates for any body in the kernel
      !> pool.
      !>
      !> Legacy interface: calls ctx%bodeul with the legacy context.
      subroutine bodeul ( body, et, ra, dec, w, lambda )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! ID code of body.
      real(wp) :: et  !! Epoch of transformation.
      real(wp) :: ra  !! Right ascension of the (IAU) north pole.
      real(wp) :: dec  !! Declination of the (IAU) north pole of the body.
      real(wp) :: w  !! Prime meridian rotation angle.
      real(wp) :: lambda  !! Angle between the prime meridian and longitude of longest axis.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodeul ( body, et, ra, dec, w, lambda )
      end subroutine bodeul

end module pepper_legacy_pck_api
