!> The legacy interface's routines of the subsystem spk_api (49 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_spk_api

   use pepper_instance_module, only: pepper_context, pepper_zzcorsxf => zzcorsxf, &
      pepper_zzcvxsta => zzcvxsta, pepper_zzcvssta => zzcvssta
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: sgfref, sgbwfs, sgbwvs, sgwfpk, sgwvpk, sgwes, spkcov, spkgeo, spkgps, spkobj, &
      spkpds, spks01, spks02, spks03, spks05, spks08, spks09, spks15, spks17, spks18, spks19, &
      spks20, spks21, spkuds, zzcorsxf, zzcvxsta, zzcvssta, spk14a, spk14b, spk14e, spkapo, &
      spkltc, spks10, spks12, spks13, spkssb, spkapp, spkaps, spkezp, spks14, spkacs, spkpos, &
      spksub, spkez, spkcvo, spkezr, spkcpo, spkcvt, spkcpt

contains

      !> Fetch from the references partition of a generic segment
      !> the double precision numbers from FIRST to LAST. The
      !> segment is identified by a DAF file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%sgfref with the legacy context.
      subroutine sgfref ( handle, descr, first, last, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF open for reading.
      real(wp) :: descr  ( * )  !! Descriptor for a generic segment in the DAF.
      integer(ip) :: first  !! The index of the first reference value to fetch.
      integer(ip) :: last  !! The index of the last reference value to fetch.
      real(wp) :: values ( * )  !! The reference values that were requested.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgfref ( handle, descr, first, last, values )
      end subroutine sgfref

      !> Begin writing a generic segment that will contain fixed size data
      !> packets.
      !>
      !> Legacy interface: calls ctx%sgbwfs with the legacy context.
      subroutine sgbwfs ( handle, descr, segid, nconst, const, pktsiz, idxtyp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file opened with write access.
      real(wp) :: descr  ( * )  !! Descriptor for a generic segment.
      character(*) :: segid  !! Identifier for a generic segment.
      integer(ip) :: nconst  !! Number of constant values in a generic segment.
      real(wp) :: const  ( * )  !! Array of constant values for a generic segment.
      integer(ip) :: pktsiz ( * )  !! Size of the data packets.
      integer(ip) :: idxtyp  !! Index type for the reference values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgbwfs ( handle, descr, segid, nconst, const, pktsiz, idxtyp )
      end subroutine sgbwfs

      !> Begin writing a generic segment that will contain variable size
      !> data packets.
      !>
      !> Legacy interface: calls ctx%sgbwvs with the legacy context.
      subroutine sgbwvs ( handle, descr, segid, nconst, const, idxtyp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file opened with write access.
      real(wp) :: descr  ( * )  !! Descriptor for a segment.
      character(*) :: segid  !! Identifier for a segment.
      integer(ip) :: nconst  !! Number of constant values in a segment.
      real(wp) :: const  ( * )  !! Array of constant values for a segment.
      integer(ip) :: idxtyp  !! Index type for the reference values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgbwvs ( handle, descr, segid, nconst, const, idxtyp )
      end subroutine sgbwvs

      !> Write one or more fixed size data packets to the generic segment
      !> currently being written to the DAF file associated with HANDLE.
      !>
      !> Legacy interface: calls ctx%sgwfpk with the legacy context.
      subroutine sgwfpk ( handle, npkts, pktdat, nrefs, refdat )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file opened with write access.
      integer(ip) :: npkts  !! Number of data packets to write to a segment.
      real(wp) :: pktdat ( * )  !! Array of packet data.
      integer(ip) :: nrefs  !! Number of reference values.
      real(wp) :: refdat ( * )  !! Reference data.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgwfpk ( handle, npkts, pktdat, nrefs, refdat )
      end subroutine sgwfpk

      !> Write one or more variable size data packets to the generic
      !> segment currently being written to the DAF file associated with
      !> HANDLE.
      !>
      !> Legacy interface: calls ctx%sgwvpk with the legacy context.
      subroutine sgwvpk ( handle, npkts, pktsiz, pktdat, nrefs, refdat )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file opened with write access.
      integer(ip) :: npkts  !! Number of data packets to write to a segment.
      integer(ip) :: pktsiz ( * )  !! Array of sizes of variable size packets.
      real(wp) :: pktdat ( * )  !! Array of packet data.
      integer(ip) :: nrefs  !! Number of reference values.
      real(wp) :: refdat ( * )  !! Reference data.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgwvpk ( handle, npkts, pktsiz, pktdat, nrefs, refdat )
      end subroutine sgwvpk

      !> End the generic segment in the DAF file associated with HANDLE.
      !>
      !> Legacy interface: calls ctx%sgwes with the legacy context.
      subroutine sgwes ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file opened with write access.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgwes ( handle )
      end subroutine sgwes

      !> Find the coverage window for a specified ephemeris object in a
      !> specified SPK file.
      !>
      !> Legacy interface: calls ctx%spkcov with the legacy context.
      subroutine spkcov ( spkfnm, idcode, cover )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: spkfnm  !! Name of SPK file.
      integer(ip) :: idcode  !! ID code of ephemeris object.
      real(wp) :: cover ( lbcell : * )  !! Window giving coverage in SPKFNM for IDCODE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkcov ( spkfnm, idcode, cover )
      end subroutine spkcov

      !> Compute the geometric state (position and velocity) of a target
      !> body relative to an observing body.
      !>
      !> Legacy interface: calls ctx%spkgeo with the legacy context.
      subroutine spkgeo ( targ, et, ref, obs, state, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ninert
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Target epoch.
      character(*) :: ref  !! Target reference frame.
      integer(ip) :: obs  !! Observing body.
      real(wp) :: state    ( 6 )  !! State of target.
      real(wp) :: lt  !! Light time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkgeo ( targ, et, ref, obs, state, lt )
      end subroutine spkgeo

      !> Compute the geometric position of a target body relative to an
      !> observing body.
      !>
      !> Legacy interface: calls ctx%spkgps with the legacy context.
      subroutine spkgps ( targ, et, ref, obs, pos, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ninert
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Target epoch.
      character(*) :: ref  !! Target reference frame.
      integer(ip) :: obs  !! Observing body.
      real(wp) :: pos ( 3 )  !! Position of target.
      real(wp) :: lt  !! Light time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkgps ( targ, et, ref, obs, pos, lt )
      end subroutine spkgps

      !> Find the set of ID codes of all objects in a specified SPK file.
      !>
      !> Legacy interface: calls ctx%spkobj with the legacy context.
      subroutine spkobj ( spkfnm, ids )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: spkfnm  !! Name of SPK file.
      integer(ip) :: ids    ( lbcell : * )  !! Set of ID codes of objects in SPK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkobj ( spkfnm, ids )
      end subroutine spkobj

      !> Perform routine error checks and if all check pass, pack the
      !> descriptor for an SPK segment
      !>
      !> Legacy interface: calls ctx%spkpds with the legacy context.
      subroutine spkpds ( body, center, frame, &
                          type, first,  last, descr )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      integer(ip) :: center  !! The center of motion for BODY.
      character(*) :: frame  !! The frame for this segment.
      integer(ip) :: type  !! The type of SPK segment to create.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last  epoch for which the segment is valid.
      real(wp) :: descr ( * )  !! An SPK segment descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkpds ( body, center, frame, type, first, last, descr )
      end subroutine spkpds

      !> Extract a subset of the data in a SPK segment of type 1
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks01 with the legacy context.
      subroutine spks01 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of source segment.
      integer(ip) :: baddr  !! Beginning address of source segment.
      integer(ip) :: eaddr  !! Ending address of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks01 ( handle, baddr, eaddr, begin, end )
      end subroutine spks01

      !> Extract a subset of the data in a SPK segment of type 2
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks02 with the legacy context.
      subroutine spks02 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of source segment.
      integer(ip) :: baddr  !! Beginning address of source segment.
      integer(ip) :: eaddr  !! Ending address of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks02 ( handle, baddr, eaddr, begin, end )
      end subroutine spks02

      !> Extract a subset of the data in a SPK segment of type 3 (Chebyshev
      !> polynomials, position and velocity) into a new segment.
      !>
      !> Legacy interface: calls ctx%spks03 with the legacy context.
      subroutine spks03 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of source segment.
      integer(ip) :: baddr  !! Beginning address of source segment.
      integer(ip) :: eaddr  !! Ending address of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks03 ( handle, baddr, eaddr, begin, end )
      end subroutine spks03

      !> Extract a subset of the data in an SPK segment of type 5
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks05 with the legacy context.
      subroutine spks05 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks05 ( handle, baddr, eaddr, begin, end )
      end subroutine spks05

      !> Extract a subset of the data in an SPK segment of type 8
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks08 with the legacy context.
      subroutine spks08 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks08 ( handle, baddr, eaddr, begin, end )
      end subroutine spks08

      !> Extract a subset of the data in an SPK segment of type 9
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks09 with the legacy context.
      subroutine spks09 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks09 ( handle, baddr, eaddr, begin, end )
      end subroutine spks09

      !> Extract a subset of the data in an SPK segment of type 15
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks15 with the legacy context.
      subroutine spks15 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks15 ( handle, baddr, eaddr, begin, end )
      end subroutine spks15

      !> Extract a subset of the data in an SPK segment of type 17
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks17 with the legacy context.
      subroutine spks17 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks17 ( handle, baddr, eaddr, begin, end )
      end subroutine spks17

      !> Extract a subset of the data in an SPK segment of type 18
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks18 with the legacy context.
      subroutine spks18 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk18
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks18 ( handle, baddr, eaddr, begin, end )
      end subroutine spks18

      !> Extract a subset of the data in an SPK segment of type 19
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks19 with the legacy context.
      subroutine spks19 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk19
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks19 ( handle, baddr, eaddr, begin, end )
      end subroutine spks19

      !> Extract a subset of the data in a SPK segment of type 20
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks20 with the legacy context.
      subroutine spks20 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of source segment.
      integer(ip) :: baddr  !! Beginning address of source segment.
      integer(ip) :: eaddr  !! Ending address of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks20 ( handle, baddr, eaddr, begin, end )
      end subroutine spks20

      !> Extract a subset of the data in a SPK segment of type 21
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks21 with the legacy context.
      subroutine spks21 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk21
      implicit none
      integer(ip) :: handle  !! Handle of source segment.
      integer(ip) :: baddr  !! Beginning address of source segment.
      integer(ip) :: eaddr  !! Ending address of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks21 ( handle, baddr, eaddr, begin, end )
      end subroutine spks21

      !> Unpack the contents of an SPK segment descriptor.
      !>
      !> Legacy interface: calls ctx%spkuds with the legacy context.
      subroutine spkuds ( descr, &
                          body,  center, frame,  type, &
                          first, last,   baddrs, eaddrs )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: descr ( * )  !! An SPK segment descriptor.
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      integer(ip) :: center  !! The center of motion for BODY.
      integer(ip) :: frame  !! The code for the frame of this segment.
      integer(ip) :: type  !! The type of SPK segment.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last  epoch for which the segment is valid.
      integer(ip) :: baddrs  !! Beginning DAF address of the segment.
      integer(ip) :: eaddrs  !! Ending DAF address of the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkuds ( descr, body, center, frame, type, first, last, baddrs, eaddrs )
      end subroutine spkuds

      !> Correct a state transformation matrix for the rate of change of
      !> light time.
      !>
      !> Legacy interface: calls zz routine zzcorsxf with the legacy context.
      subroutine zzcorsxf ( xmit, dlt, xform, corxfm )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: xmit  !! Radiation direction flag.
      real(wp) :: dlt  !! Light time derivative with respect to TDB.
      real(wp) :: xform  ( 6, 6 )  !! State transformation matrix.
      real(wp) :: corxfm ( 6, 6 )  !! Corrected state transformation matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: xform is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzcorsxf ( ctx, xmit, dlt, (xform), corxfm )
      end subroutine zzcorsxf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcvxsta with the legacy context.
      subroutine zzcvxsta ( et, ref, center, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      integer(ip) :: center  !! ID code of center of motion.
      real(wp) :: state ( 6 )  !! State vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzcvxsta ( ctx, et, ref, center, state )
      end subroutine zzcvxsta

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcvssta with the legacy context.
      subroutine zzcvssta ( state, center, et, ref )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      integer(ip) :: center  !! ID code of center of motion.
      real(wp) :: state ( 6 )  !! State vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzcvssta ( ctx, state, center, et, ref )
      end subroutine zzcvssta

      !> Add data to a type 14 SPK segment associated with HANDLE.
      !>
      !> Legacy interface: calls ctx%spk14a with the legacy context.
      subroutine spk14a ( handle, ncsets, coeffs, epochs )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of an SPK file open for writing.
      integer(ip) :: ncsets  !! The number of coefficient sets and epochs.
      real(wp) :: coeffs ( * )  !! The collection of coefficient sets.
      real(wp) :: epochs ( * )  !! The epochs associated with the coefficient sets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spk14a ( handle, ncsets, coeffs, epochs )
      end subroutine spk14a

      !> Begin a type 14 SPK segment in the SPK file associated with
      !> HANDLE.
      !>
      !> Legacy interface: calls ctx%spk14b with the legacy context.
      subroutine spk14b ( handle, segid, &
                          body,   center, frame, &
                          first,  last,   chbdeg  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! The handle of an SPK file open for writing.
      character(*) :: segid  !! The string to use for segment identifier.
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      integer(ip) :: center  !! The center of motion for BODY.
      character(*) :: frame  !! The reference frame for this segment.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last epoch for which the segment is valid.
      integer(ip) :: chbdeg  !! The degree of the Chebyshev Polynomial used.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spk14b ( handle, segid, body, center, frame, first, last, chbdeg )
      end subroutine spk14b

      !> End the type 14 SPK segment currently being written to the SPK
      !> file associated with HANDLE.
      !>
      !> Legacy interface: calls ctx%spk14e with the legacy context.
      subroutine spk14e ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! The handle of an SPK file open for writing.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spk14e ( handle )
      end subroutine spk14e

      !> Return the position of a target body relative to an observer,
      !> optionally corrected for light time and stellar aberration.
      !>
      !> Legacy interface: calls ctx%spkapo with the legacy context.
      subroutine spkapo ( targ, et, ref, sobs, abcorr, ptarg, lt )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of observer's state.
      real(wp) :: sobs     ( 6 )  !! State of observer wrt. solar system barycenter.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: ptarg    ( 3 )  !! Position of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: sobs is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkapo ( targ, et, ref, (sobs), abcorr, ptarg, lt )
      end subroutine spkapo

      !> Return the state (position and velocity) of a target body
      !> relative to an observer, optionally corrected for light time,
      !> expressed relative to an inertial reference frame.
      !>
      !> Legacy interface: calls ctx%spkltc with the legacy context.
      subroutine spkltc ( targ, et, ref, abcorr, stobs, starg, lt, dlt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: stobs  ( 6 )  !! State of the observer relative to the SSB.
      real(wp) :: starg  ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: stobs is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkltc ( targ, et, ref, abcorr, (stobs), starg, lt, dlt )
      end subroutine spkltc

      !> Extract a subset of the data in a type 10 SPK segment into a new
      !> type 10 segment.
      !>
      !> Legacy interface: calls ctx%spks10 with the legacy context.
      subroutine spks10 ( srchan, srcdsc, dsthan, dstdsc, dstsid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: srchan  !! Handle of the SPK file with the source segment.
      real(wp) :: srcdsc(*)  !! Descriptor for the source segment.
      integer(ip) :: dsthan  !! Handle of the SPK file for the destination segment.
      real(wp) :: dstdsc(*)  !! Descriptor for the destination segment.
      character(*) :: dstsid  !! Segment identifier for the new segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks10 ( srchan, srcdsc, dsthan, dstdsc, dstsid )
      end subroutine spks10

      !> Extract a subset of the data in an SPK segment of type 12
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks12 with the legacy context.
      subroutine spks12 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks12 ( handle, baddr, eaddr, begin, end )
      end subroutine spks12

      !> Extract a subset of the data in an SPK segment of type 13
      !> into a new segment.
      !>
      !> Legacy interface: calls ctx%spks13 with the legacy context.
      subroutine spks13 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing source segment.
      integer(ip) :: baddr  !! Beginning address in file of source segment.
      integer(ip) :: eaddr  !! Ending address in file of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks13 ( handle, baddr, eaddr, begin, end )
      end subroutine spks13

      !> Return the state (position and velocity) of a target body
      !> relative to the solar system barycenter.
      !>
      !> Legacy interface: calls ctx%spkssb with the legacy context.
      subroutine spkssb ( targ, et, ref, starg )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Target epoch.
      character(*) :: ref  !! Target reference frame.
      real(wp) :: starg    ( 6 )  !! State of target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkssb ( targ, et, ref, starg )
      end subroutine spkssb

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine SPKAPS. This routine is supported for purposes of
      !> backward compatibility only.
      !>
      !> Legacy interface: calls ctx%spkapp with the legacy context.
      subroutine spkapp ( targ, et, ref, sobs, abcorr, starg, lt )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of observer's state.
      real(wp) :: sobs     ( 6 )  !! State of observer wrt. solar system barycenter.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: sobs is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkapp ( targ, et, ref, (sobs), abcorr, starg, lt )
      end subroutine spkapp

      !> Return the state (position and velocity) of a target body
      !> relative to an observer specified by its state and
      !> acceleration relative to the solar system barycenter. The
      !> returned state may be optionally corrected for light time
      !> and stellar aberration. All input and output vectors are
      !> expressed relative to an inertial reference frame.
      !>
      !> Legacy interface: calls ctx%spkaps with the legacy context.
      subroutine spkaps ( targ, et, ref, abcorr, stobs, accobs, starg, lt, dlt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: stobs  ( 6 )  !! State of the observer relative to the SSB.
      real(wp) :: accobs ( 3 )  !! Acceleration of the observer relative to the SSB.
      real(wp) :: starg  ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: stobs, accobs are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkaps ( targ, et, ref, abcorr, (stobs), (accobs), starg, lt, dlt )
      end subroutine spkaps

      !> Return the position of a target body relative to an observing
      !> body, optionally corrected for light time (planetary aberration)
      !> and stellar aberration.
      !>
      !> Legacy interface: calls ctx%spkezp with the legacy context.
      subroutine spkezp ( targ, et, ref, abcorr, obs, ptarg, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: targ  !! Target body NAIF ID code.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output position vector.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observing body NAIF ID code.
      real(wp) :: ptarg    ( 3 )  !! Position of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkezp ( targ, et, ref, abcorr, obs, ptarg, lt )
      end subroutine spkezp

      !> Extract a subset of the data in a type 14 SPK segment into a new
      !> type 14 segment.
      !>
      !> Legacy interface: calls ctx%spks14 with the legacy context.
      subroutine spks14 ( srchan, srcdsc, dsthan, dstdsc, dstsid )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: srchan  !! Handle of the SPK file with the source segment.
      real(wp) :: srcdsc(*)  !! Descriptor for the source segment.
      integer(ip) :: dsthan  !! Handle of the SPK file for the destination segment.
      real(wp) :: dstdsc(*)  !! Descriptor for the destination segment.
      character(*) :: dstsid  !! Segment identifier for the new segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spks14 ( srchan, srcdsc, dsthan, dstdsc, dstsid )
      end subroutine spks14

      !> Return the state (position and velocity) of a target body
      !> relative to an observer, optionally corrected for light time
      !> and stellar aberration, expressed relative to an inertial
      !> reference frame.
      !>
      !> Legacy interface: calls ctx%spkacs with the legacy context.
      subroutine spkacs ( targ, et, ref, abcorr, obs, starg, lt, dlt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of output state.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observer.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      real(wp) :: dlt  !! Derivative of light time with respect to time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkacs ( targ, et, ref, abcorr, obs, starg, lt, dlt )
      end subroutine spkacs

      !> Return the position of a target body relative to an observing
      !> body, optionally corrected for light time (planetary aberration)
      !> and stellar aberration.
      !>
      !> Legacy interface: calls ctx%spkpos with the legacy context.
      subroutine spkpos ( targ, et, ref, abcorr, obs, ptarg, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: targ  !! Target body name.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output position vector.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obs  !! Observing body name.
      real(wp) :: ptarg    ( 3 )  !! Position of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkpos ( targ, et, ref, abcorr, obs, ptarg, lt )
      end subroutine spkpos

      !> Extract a subset of the data in an SPK segment into a
      !> separate segment.
      !>
      !> Legacy interface: calls ctx%spksub with the legacy context.
      subroutine spksub ( handle, descr, ident, begin, end, newh )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of source segment.
      real(wp) :: descr    ( 5 )  !! Descriptor of source segment.
      character(*) :: ident  !! Identifier of source segment.
      real(wp) :: begin  !! Beginning (initial epoch) of subset.
      real(wp) :: end  !! End (final epoch) of subset.
      integer(ip) :: newh  !! Handle of new segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spksub ( handle, descr, ident, begin, end, newh )
      end subroutine spksub

      !> Return the state (position and velocity) of a target body
      !> relative to an observing body, optionally corrected for light
      !> time (planetary aberration) and stellar aberration.
      !>
      !> Legacy interface: calls ctx%spkez with the legacy context.
      subroutine spkez ( targ, et, ref, abcorr, obs, starg, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output state vector.
      character(*) :: abcorr  !! Aberration correction flag.
      integer(ip) :: obs  !! Observing body.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkez ( targ, et, ref, abcorr, obs, starg, lt )
      end subroutine spkez

      !> Return the state of a specified target relative to an "observer,"
      !> where the observer has constant velocity in a specified reference
      !> frame. The observer's state is provided by the calling program
      !> rather than by loaded SPK files.
      !>
      !> Legacy interface: calls ctx%spkcvo with the legacy context.
      subroutine spkcvo ( target, et, outref, refloc, abcorr, obssta, obsepc, obsctr, obsref, state, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      character(*) :: target  !! Name of target ephemeris object.
      real(wp) :: et  !! Observation epoch.
      character(*) :: outref  !! Reference frame of output state.
      character(*) :: refloc  !! Output reference frame evaluation locus.
      character(*) :: abcorr  !! Aberration correction.
      real(wp) :: obssta ( 6 )  !! Observer state relative to center of motion.
      real(wp) :: obsepc  !! Epoch of observer state.
      character(*) :: obsctr  !! Center of motion of observer.
      character(*) :: obsref  !! Frame of observer state.
      real(wp) :: state  ( 6 )  !! State of target with respect to observer.
      real(wp) :: lt  !! One way light time between target and observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: obssta is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkcvo ( target, et, outref, refloc, abcorr, (obssta), obsepc, obsctr, obsref, &
         state, lt )
      end subroutine spkcvo

      !> Return the state (position and velocity) of a target body
      !> relative to an observing body, optionally corrected for light
      !> time (planetary aberration) and stellar aberration.
      !>
      !> Legacy interface: calls ctx%spkezr with the legacy context.
      subroutine spkezr ( targ, et, ref, abcorr, obs, starg, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: targ  !! Target body name.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Reference frame of output state vector.
      character(*) :: abcorr  !! Aberration correction flag.
      character(*) :: obs  !! Observing body name.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkezr ( targ, et, ref, abcorr, obs, starg, lt )
      end subroutine spkezr

      !> Return the state of a specified target relative to an "observer,"
      !> where the observer has constant position in a specified reference
      !> frame. The observer's position is provided by the calling program
      !> rather than by loaded SPK files.
      !>
      !> Legacy interface: calls ctx%spkcpo with the legacy context.
      subroutine spkcpo ( target, et,     outref, refloc, abcorr, &
                          obspos, obsctr, obsref, state,  lt     )
      use pepper_kinds, only: wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: target  !! Name of target ephemeris object.
      real(wp) :: et  !! Observation epoch.
      character(*) :: outref  !! Reference frame of output state.
      character(*) :: refloc  !! Output reference frame evaluation locus.
      character(*) :: abcorr  !! Aberration correction.
      real(wp) :: obspos ( 3 )  !! Observer position relative to center of motion.
      character(*) :: obsctr  !! Center of motion of observer.
      character(*) :: obsref  !! Frame of observer position.
      real(wp) :: state  ( 6 )  !! State of target with respect to observer.
      real(wp) :: lt  !! One way light time between target and observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: obspos is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkcpo ( target, et, outref, refloc, abcorr, (obspos), obsctr, obsref, state, lt )
      end subroutine spkcpo

      !> Return the state, relative to a specified observer, of a target
      !> having constant velocity in a specified reference frame. The
      !> target's state is provided by the calling program rather than by
      !> loaded SPK files.
      !>
      !> Legacy interface: calls ctx%spkcvt with the legacy context.
      subroutine spkcvt ( trgsta, trgepc, trgctr, trgref, et, outref, refloc, abcorr, obsrvr, state, lt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      real(wp) :: trgsta ( 6 )  !! Target state relative to center of motion.
      real(wp) :: trgepc  !! Epoch of target state.
      character(*) :: trgctr  !! Center of motion of target.
      character(*) :: trgref  !! Frame of target state.
      real(wp) :: et  !! Observation epoch.
      character(*) :: outref  !! Reference frame of output state.
      character(*) :: refloc  !! Output reference frame evaluation locus.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing ephemeris object.
      real(wp) :: state  ( 6 )  !! State of target with respect to observer.
      real(wp) :: lt  !! One way light time between target and observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: trgsta is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkcvt ( (trgsta), trgepc, trgctr, trgref, et, outref, refloc, abcorr, obsrvr, &
         state, lt )
      end subroutine spkcvt

      !> Return the state, relative to a specified observer, of a target
      !> having constant position in a specified reference frame. The
      !> target's position is provided by the calling program rather than
      !> by loaded SPK files.
      !>
      !> Legacy interface: calls ctx%spkcpt with the legacy context.
      subroutine spkcpt ( trgpos, trgctr, trgref, et,    outref, &
                          refloc, abcorr, obsrvr, state, lt     )
      use pepper_kinds, only: wp
      use pepper_inc_zzabcorr
      implicit none
      real(wp) :: trgpos ( 3 )  !! Target position relative to center of motion.
      character(*) :: trgctr  !! Center of motion of target.
      character(*) :: trgref  !! Frame of target position.
      real(wp) :: et  !! Observation epoch.
      character(*) :: outref  !! Reference frame of output state.
      character(*) :: refloc  !! Output reference frame evaluation locus.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing ephemeris object.
      real(wp) :: state  ( 6 )  !! State of target with respect to observer.
      real(wp) :: lt  !! One way light time between target and observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: trgpos is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkcpt ( (trgpos), trgctr, trgref, et, outref, refloc, abcorr, obsrvr, state, lt )
      end subroutine spkcpt

end module pepper_legacy_spk_api
