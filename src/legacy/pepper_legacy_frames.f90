!> The legacy interface's routines of the subsystem frames (16 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_frames

   use pepper_instance_module, only: pepper_context, pepper_zzdynbid => zzdynbid, &
      pepper_zzdynvai => zzdynvai, pepper_zzdynoac => zzdynoac, pepper_zzdynoad => zzdynoad, &
      pepper_zzdynvac => zzdynvac, pepper_zzdynvad => zzdynvad, pepper_zzswfcln => zzswfcln, &
      pepper_zzswfini => zzswfini, pepper_zzgftreb => zzgftreb, pepper_zzfdat => zzfdat
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbpool

   implicit none

   private
   public :: zzdynbid, zzdynvai, zzdynoac, zzdynoad, zzdynvac, zzdynvad, zzswfcln, zzswfini, &
      zzgftreb, zzfdat, namfrm, frmnam, frinfo, cidfrm, cnmfrm, ccifrm

contains

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynbid with the legacy context.
      subroutine zzdynbid ( frname, frcode, item, idcode )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: idcode  !! Body ID code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdynbid ( ctx, frname, frcode, item, idcode )
      end subroutine zzdynbid

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynvai with the legacy context.
      subroutine zzdynvai ( frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: maxn  !! Maximum number of values to return.
      integer(ip) :: n  !! Number of returned values.
      integer(ip) :: values  ( * )  !! Output kernel variable.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdynvai ( ctx, frname, frcode, item, maxn, n, values )
      end subroutine zzdynvai

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynoac with the legacy context.
      subroutine zzdynoac ( frname, frcode, item, &
                            maxn,   n,      values, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: maxn  !! Maximum number of values to return.
      integer(ip) :: n  !! Number of returned values.
      character(*) :: values  ( * )  !! Output kernel variable.
      logical :: found  !! "Found" flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: frname, item are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzdynoac ( ctx, (frname), frcode, (item), maxn, n, values, found )
      end subroutine zzdynoac

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynoad with the legacy context.
      subroutine zzdynoad ( frname, frcode, item, &
                            maxn,   n,      values, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzbodtrn
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: maxn  !! Maximum number of values to return.
      integer(ip) :: n  !! Number of returned values.
      real(wp) :: values  ( * )  !! Output kernel variable.
      logical :: found  !! "Found" flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdynoad ( ctx, frname, frcode, item, maxn, n, values, found )
      end subroutine zzdynoad

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynvac with the legacy context.
      subroutine zzdynvac ( frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: maxn  !! Maximum number of values to return.
      integer(ip) :: n  !! Number of returned values.
      character(*) :: values  ( * )  !! Output kernel variable.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: frname, item are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzdynvac ( ctx, (frname), frcode, (item), maxn, n, values )
      end subroutine zzdynvac

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdynvad with the legacy context.
      subroutine zzdynvad ( frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzdyn
      implicit none
      character(*) :: frname  !! Frame name.
      integer(ip) :: frcode  !! Frame ID code.
      character(*) :: item  !! Item associated with frame definition.
      integer(ip) :: maxn  !! Maximum number of values to return.
      integer(ip) :: n  !! Number of returned values.
      real(wp) :: values  ( * )  !! Output kernel variable.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdynvad ( ctx, frname, frcode, item, maxn, n, values )
      end subroutine zzdynvad

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzswfcln with the legacy context.
      subroutine zzswfcln ( hdfram, frpool, basbeg, framat )
      use pepper_kinds, only: ip
      use pepper_inc_zzswtchf
      implicit none
      integer(ip) :: hdfram ( * )  !! Array of hash collision list head nodes.
      integer(ip) :: frpool ( lbsngl : * )  !! Hash collision list pool.
      integer(ip) :: basbeg ( * )  !! Start indices of base lists of switch frames.
      integer(ip) :: framat  !! Switch frame index in switch frame ID array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzswfcln ( ctx, hdfram, frpool, basbeg, framat )
      end subroutine zzswfcln

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzswfini with the legacy context.
      subroutine zzswfini ( hdfram, frpool, fidlst, basbeg, &
                            free,   prvat,  prvfrm, samfrm  )
      use pepper_kinds, only: ip
      use pepper_inc_zzswtchf
      implicit none
      integer(ip) :: hdfram ( * )  !! Array of hash collision list head nodes.
      integer(ip) :: frpool ( lbsngl : * )  !! Hash collision list pool.
      integer(ip) :: fidlst ( * )  !! Frame IDs corresponding to list nodes.
      integer(ip) :: basbeg ( * )  !! Start indices of base lists of switch frames.
      integer(ip) :: free  !! First free index in base and time lists.
      integer(ip) :: prvat  !! Index of previous input frame in frame list.
      integer(ip) :: prvfrm  !! Previous input frame ID.
      logical :: samfrm  !! Flag indicating input frame matches previous one.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, free, prvat, prvfrm, samfrm )
      end subroutine zzswfini

      !> Return the values of the triaxial radii for any body in the
      !> kernel pool.
      !>
      !> Legacy interface: calls zz routine zzgftreb with the legacy context.
      subroutine zzgftreb ( body, axes )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! NAIF ID code of body.
      real(wp) :: axes ( 3 )  !! Length of axes of body (1,2,3, as defined below).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgftreb ( ctx, body, axes )
      end subroutine zzgftreb

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzfdat with the legacy context.
      subroutine zzfdat ( ncount, maxbfr, name,   idcode, &
                          center, type,   typid,  centrd, &
                          bnmlst, bnmpol, bnmnms, bnmidx, &
                          bidlst, bidpol, bidids, bididx )
      use pepper_kinds, only: ip
      use pepper_inc_frmtyp
      use pepper_inc_ninert
      use pepper_inc_nninrt
      implicit none
      integer(ip) :: ncount  !! Input checking variable.
      integer(ip) :: maxbfr  !! build-in frame hash size.
      character(*) :: name   (          * )  !! array containing the names of all built-in frames
      integer(ip) :: idcode (          * )  !! array containing the IDs of all built-in frames
      integer(ip) :: center (          * )  !! array containing the centers of the built-in frames
      integer(ip) :: type   (          * )  !! array containing the types of the built-in frames
      integer(ip) :: typid  (          * )  !! array containing the subtype id
      integer(ip) :: centrd (          * )  !! an order vector for CENTER
      integer(ip) :: bnmlst (          * )  !! frame name-based hash head node pointer list
      integer(ip) :: bnmpol ( lbpool : * )  !! frame name-based hash node collision list
      character(*) :: bnmnms (          * )  !! frame name-based hash item list
      integer(ip) :: bnmidx (          * )  !! frame name-based hash index storage array
      integer(ip) :: bidlst (          * )  !! frame ID-based hash head node pointer list
      integer(ip) :: bidpol ( lbpool : * )  !! frame ID-based hash node collision list
      integer(ip) :: bidids (          * )  !! frame ID-based hash item list
      integer(ip) :: bididx (          * )  !! frame ID-based hash index storage array
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzfdat ( ctx, ncount, maxbfr, name, idcode, center, type, typid, centrd, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, bidids, bididx )
      end subroutine zzfdat

      !> Look up the frame ID code associated with a string.
      !>
      !> Legacy interface: calls ctx%namfrm with the legacy context.
      subroutine namfrm ( frname, frcode )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: frname  !! The name of some reference frame
      integer(ip) :: frcode  !! The SPICE ID code of the frame.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%namfrm ( frname, frcode )
      end subroutine namfrm

      !> Retrieve the name of a reference frame associated with a SPICE ID
      !> code.
      !>
      !> Legacy interface: calls ctx%frmnam with the legacy context.
      subroutine frmnam ( frcode, frname )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: frname  !! the name associated with the reference frame.
      integer(ip) :: frcode  !! an integer code for a reference frame
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%frmnam ( frcode, frname )
      end subroutine frmnam

      !> Retrieve the minimal attributes associated with a frame
      !> needed for converting transformations to and from it.
      !>
      !> Legacy interface: calls ctx%frinfo with the legacy context.
      subroutine frinfo ( frcode, cent, frclss, clssid, found )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: frcode  !! the ID code for some frame
      integer(ip) :: cent  !! the center of the frame
      integer(ip) :: frclss  !! the class (type) of the frame
      integer(ip) :: clssid  !! the ID code for the frame within its class.
      logical :: found  !! .TRUE. if the requested information is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%frinfo ( frcode, cent, frclss, clssid, found )
      end subroutine frinfo

      !> Retrieve frame ID code and name to associate with a frame center.
      !>
      !> Legacy interface: calls ctx%cidfrm with the legacy context.
      subroutine cidfrm ( cent, frcode, frname, found )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: frname  !! the name of the frame with ID FRCODE
      integer(ip) :: frcode  !! the ID code of the frame associated with CENT
      integer(ip) :: cent  !! an object to associate a frame with.
      logical :: found  !! .TRUE. if the requested information is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cidfrm ( cent, frcode, frname, found )
      end subroutine cidfrm

      !> Retrieve frame ID code and name to associate with an object.
      !>
      !> Legacy interface: calls ctx%cnmfrm with the legacy context.
      subroutine cnmfrm ( cname, frcode, frname, found )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: cname  !! name of the object to find a frame for
      character(*) :: frname  !! the name of the frame with ID FRCODE
      integer(ip) :: frcode  !! the ID code of the frame associated with CNAME
      logical :: found  !! .TRUE. if the requested information is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: cname is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%cnmfrm ( (cname), frcode, frname, found )
      end subroutine cnmfrm

      !> Return the frame name, frame ID, and center associated with
      !> a given frame class and class ID.
      !>
      !> Legacy interface: calls ctx%ccifrm with the legacy context.
      subroutine ccifrm ( frclss, clssid, frcode, frname, cent, found )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: frname  !! Name of the frame identified by FRCLSS, CLSSID.
      integer(ip) :: frcode  !! ID code of the frame identified by FRCLSS, CLSSID.
      integer(ip) :: cent  !! Center of the frame identified by FRCLSS, CLSSID.
      integer(ip) :: frclss  !! Class of frame.
      integer(ip) :: clssid  !! Class ID of frame.
      logical :: found  !! .TRUE. if the requested information is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ccifrm ( frclss, clssid, frcode, frname, cent, found )
      end subroutine ccifrm

end module pepper_legacy_frames
