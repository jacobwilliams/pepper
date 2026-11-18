!> The legacy interface's routines of the subsystem writers (60 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_writers

   use pepper_instance_module, only: pepper_context, pepper_zzck4i2d => zzck4i2d
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: dlsize

   implicit none

   private
   public :: ckcls, ckgr01, ckgr03, ckgr04, ckgr05, ckgr06, ckmp06, cknm06, cknr01, cknr02, &
      cknr03, cknr05, ckopn, ckw01, ckw02, ckw03, ckw04b, ckw04e, ckw05, ckw06, pck03a, pck03e, &
      pckcls, pckopn, pckpds, pckuds, pckw02, pckw20, spcac, spcdc, spcec, spcopn, spcrfl, &
      spcrnl, spkcls, spkopa, spkopn, spkw01, spkw02, spkw03, spkw05, spkw08, spkw09, spkw10, &
      spkw12, spkw13, spkw15, spkw17, spkw18, spkw19, spkw20, spkw21, zzck4i2d, ckgr02, ckw04a, &
      pck03b, spcb2t, spct2b, spca2b, spcb2a

contains

      !> Close an open CK file.
      !>
      !> Legacy interface: calls ctx%ckcls with the legacy context.
      subroutine ckcls ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of the CK file to be closed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckcls ( handle )
      end subroutine ckcls

      !> Return a specified pointing instance from a CK type 01 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%ckgr01 with the legacy context.
      subroutine ckgr01 ( handle, descr, recno, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The segment descriptor.
      integer(ip) :: recno  !! The number of the pointing record to be returned.
      real(wp) :: record  ( * )  !! The pointing record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgr01 ( handle, descr, recno, record )
      end subroutine ckgr01

      !> Return a specified pointing instance from a CK type 03 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%ckgr03 with the legacy context.
      subroutine ckgr03 ( handle, descr, recno, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the CK file containing the segment.
      real(wp) :: descr   ( * )  !! The segment descriptor.
      integer(ip) :: recno  !! The number of the pointing instance to be returned.
      real(wp) :: record  ( * )  !! The pointing record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgr03 ( handle, descr, recno, record )
      end subroutine ckgr03

      !> Return a specified pointing record from a CK type 04 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%ckgr04 with the legacy context.
      subroutine ckgr04 ( handle, descr, recno, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The segment descriptor.
      integer(ip) :: recno  !! The number of the pointing record to be returned.
      real(wp) :: record  ( * )  !! The pointing record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgr04 ( handle, descr, recno, record )
      end subroutine ckgr04

      !> Return a specified pointing instance from a CK type 05 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%ckgr05 with the legacy context.
      subroutine ckgr05 ( handle, descr, recno, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck05
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The segment descriptor.
      integer(ip) :: recno  !! The number of the pointing instance to be returned.
      real(wp) :: record  ( * )  !! The pointing record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgr05 ( handle, descr, recno, record )
      end subroutine ckgr05

      !> Return a specified pointing record from a type 6 CK segment, given
      !> the CK file's handle and the segment's descriptor.
      !>
      !> Legacy interface: calls ctx%ckgr06 with the legacy context.
      subroutine ckgr06 ( handle, descr, msno, recno, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck06
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr  ( * )  !! The segment descriptor.
      integer(ip) :: msno  !! Index of the mini-segment containing the record.
      integer(ip) :: recno  !! Index of the pointing record to be returned.
      real(wp) :: record ( * )  !! The pointing record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgr06 ( handle, descr, msno, recno, record )
      end subroutine ckgr06

      !> Return the mini-segment control parameters, mini-segment interval
      !> bounds, and last epoch for a specified mini-segment in a type 6
      !> CK segment.
      !>
      !> Legacy interface: calls ctx%ckmp06 with the legacy context.
      subroutine ckmp06 ( handle, descr,  msno, rate, &
                          subtyp, winsiz, nrec, ivlbds, lstepc )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr  ( * )  !! The descriptor of the type 6 segment.
      integer(ip) :: msno  !! Mini-segment index.
      real(wp) :: rate  !! SCLK rate in seconds/tick.
      integer(ip) :: subtyp  !! Subtype code.
      integer(ip) :: winsiz  !! Interpolation window size.
      integer(ip) :: nrec  !! Number of records in mini-segment.
      real(wp) :: ivlbds ( 2 )  !! Mini-segment interval bounds of mini-segment.
      real(wp) :: lstepc  !! Last epoch of mini-segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckmp06 ( handle, descr, msno, rate, subtyp, winsiz, nrec, ivlbds, lstepc )
      end subroutine ckmp06

      !> Return the number of mini-segments in a type 6 CK segment, given
      !> the handle of the type 6 CK file and the segment's descriptor.
      !>
      !> Legacy interface: calls ctx%cknm06 with the legacy context.
      subroutine cknm06 ( handle, descr, nmini )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The descriptor of the type 6 segment.
      integer(ip) :: nmini  !! The number of pointing instances in the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cknm06 ( handle, descr, nmini )
      end subroutine cknm06

      !> Return the number of pointing instances in a CK type 01 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%cknr01 with the legacy context.
      subroutine cknr01 ( handle, descr, nrec )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The descriptor of the type 1 segment.
      integer(ip) :: nrec  !! The number of records in the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cknr01 ( handle, descr, nrec )
      end subroutine cknr01

      !> Return the number of pointing records in a CK type 02 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%cknr02 with the legacy context.
      subroutine cknr02 ( handle, descr, nrec )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the CK file containing the segment.
      real(wp) :: descr   ( * )  !! The descriptor of the type 2 segment.
      integer(ip) :: nrec  !! The number of records in the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cknr02 ( handle, descr, nrec )
      end subroutine cknr02

      !> Return the number of pointing instances in a CK type 03 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%cknr03 with the legacy context.
      subroutine cknr03 ( handle, descr, nrec )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the CK file containing the segment.
      real(wp) :: descr   ( * )  !! The descriptor of the type 3 segment.
      integer(ip) :: nrec  !! The number of pointing instances in the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cknr03 ( handle, descr, nrec )
      end subroutine cknr03

      !> Return the number of pointing instances in a CK type 05 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%cknr05 with the legacy context.
      subroutine cknr05 ( handle, descr, nrec )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The descriptor of the type 5 segment.
      integer(ip) :: nrec  !! The number of pointing instances in the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cknr05 ( handle, descr, nrec )
      end subroutine cknr05

      !> Open a new CK file, returning the handle of the opened file.
      !>
      !> Legacy interface: calls ctx%ckopn with the legacy context.
      subroutine ckopn ( fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! The name of the CK file to be opened.
      character(*) :: ifname  !! The internal filename for the CK.
      integer(ip) :: ncomch  !! The number of characters to reserve for comments.
      integer(ip) :: handle  !! The handle of the opened CK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckopn ( fname, ifname, ncomch, handle )
      end subroutine ckopn

      !> Add a type 1 segment to a C-kernel.
      !>
      !> Legacy interface: calls ctx%ckw01 with the legacy context.
      subroutine ckw01 ( handle, begtim, endtim, inst,  ref, avflag, &
                         segid,  nrec,   sclkdp, quats, avvs        )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an open CK file.
      real(wp) :: begtim  !! The beginning encoded SCLK of the segment.
      real(wp) :: endtim  !! The ending encoded SCLK of the segment.
      integer(ip) :: inst  !! The NAIF instrument ID code.
      character(*) :: ref  !! The reference frame of the segment.
      logical :: avflag  !! .TRUE. if the segment will contain angular velocity.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: nrec  !! Number of pointing records.
      real(wp) :: sclkdp (      * )  !! Encoded SCLK times.
      real(wp) :: quats  ( 0:3, * )  !! SPICE quaternions representing instrument pointing.
      real(wp) :: avvs   (   3, * )  !! Angular velocity vectors.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw01 ( handle, begtim, endtim, inst, ref, avflag, segid, nrec, sclkdp, quats, avvs )
      end subroutine ckw01

      !> Write a type 2 segment to a C-kernel.
      !>
      !> Legacy interface: calls ctx%ckw02 with the legacy context.
      subroutine ckw02 ( handle, begtim, endtim, inst,  ref,  segid, &
                         nrec,   start,  stop,   quats, avvs, rates )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an open CK file.
      real(wp) :: begtim  !! The beginning encoded SCLK of the segment.
      real(wp) :: endtim  !! The ending encoded SCLK of the segment.
      integer(ip) :: inst  !! The NAIF instrument ID code.
      character(*) :: ref  !! The reference frame of the segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: nrec  !! Number of pointing records.
      real(wp) :: start  (      * )  !! Encoded SCLK interval start times.
      real(wp) :: stop   (      * )  !! Encoded SCLK interval stop times.
      real(wp) :: quats  ( 0:3, * )  !! SPICE quaternions representing instrument pointing.
      real(wp) :: avvs   (   3, * )  !! Angular velocity vectors.
      real(wp) :: rates  (      * )  !! Number of seconds per tick for each interval.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw02 ( handle, begtim, endtim, inst, ref, segid, nrec, start, stop, quats, avvs, rates )
      end subroutine ckw02

      !> Add a type 3 segment to a C-kernel.
      !>
      !> Legacy interface: calls ctx%ckw03 with the legacy context.
      subroutine ckw03 ( handle, begtim, endtim, inst,  ref,  avflag, &
                         segid,  nrec,   sclkdp, quats, avvs, nints, &
                         starts                                      )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an open CK file.
      real(wp) :: begtim  !! Beginning encoded SCLK of the segment.
      real(wp) :: endtim  !! Ending encoded SCLK of the segment.
      integer(ip) :: inst  !! NAIF instrument ID code.
      character(*) :: ref  !! Reference frame of the segment.
      logical :: avflag  !! .TRUE. if the segment will contain angular velocity.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: nrec  !! Number of pointing records.
      real(wp) :: sclkdp (      * )  !! Encoded SCLK times.
      real(wp) :: quats  ( 0:3, * )  !! SPICE quaternions representing instrument pointing.
      real(wp) :: avvs   (   3, * )  !! Angular velocity vectors.
      integer(ip) :: nints  !! Number of intervals.
      real(wp) :: starts (      * )  !! Encoded SCLK interval start times.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw03 ( handle, begtim, endtim, inst, ref, avflag, segid, nrec, sclkdp, quats, avvs, nints, starts )
      end subroutine ckw03

      !> Begin a type CK04 segment in the DAF file associated with
      !> HANDLE. See also CKW04A and CKW04E.
      !>
      !> Legacy interface: calls ctx%ckw04b with the legacy context.
      subroutine ckw04b ( handle, begtim, inst, ref, avflag, segid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! The handle of an DAF file open for writing.
      character(*) :: segid  !! The string to use for segment identifier.
      integer(ip) :: inst  !! The NAIF ID code for the SC or instrument.
      logical :: avflag  !! The angular rates flag.
      character(*) :: ref  !! The reference frame for this segment.
      real(wp) :: begtim  !! The segment coverage start encoded SCLK time
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw04b ( handle, begtim, inst, ref, avflag, segid )
      end subroutine ckw04b

      !> End the type 04 CK segment currently being written to the DAF
      !> file associated with HANDLE. See also CKW04B and CKW04E.
      !>
      !> Legacy interface: calls ctx%ckw04e with the legacy context.
      subroutine ckw04e ( handle, endtim )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of an CK file open for writing.
      real(wp) :: endtim  !! The segment coverage end encoded SCLK time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw04e ( handle, endtim )
      end subroutine ckw04e

      !> Write a type 5 segment to a CK file.
      !>
      !> Legacy interface: calls ctx%ckw05 with the legacy context.
      subroutine ckw05 ( handle,  subtyp,  degree,  begtim,  endtim, &
                         inst,    ref,     avflag,  segid,   n, &
                         sclkdp,  packts,  rate,    nints,   starts  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck05
      implicit none
      integer(ip) :: handle  !! Handle of an CK file open for writing.
      integer(ip) :: subtyp  !! CK type 5 subtype code.
      integer(ip) :: degree  !! Degree of interpolating polynomials.
      real(wp) :: begtim  !! Start time of interval covered by segment.
      real(wp) :: endtim  !! End time of interval covered by segment.
      integer(ip) :: inst  !! NAIF code for a s/c instrument or structure.
      character(*) :: ref  !! Reference frame name.
      logical :: avflag  !! Flag indicating if the segment will contain angular velocity.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: n  !! Number of packets.
      real(wp) :: sclkdp ( * )  !! Encoded SCLK times.
      real(wp) :: packts ( * )  !! Array of packets.
      real(wp) :: rate  !! Nominal SCLK rate in seconds per tick.
      integer(ip) :: nints  !! Number of intervals.
      real(wp) :: starts ( * )  !! Encoded SCLK interval start times.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw05 ( handle, subtyp, degree, begtim, endtim, inst, ref, avflag, segid, n, sclkdp, packts, rate, nints, starts )
      end subroutine ckw05

      !> Write a type 6 segment to a CK file.
      !>
      !> Legacy interface: calls ctx%ckw06 with the legacy context.
      subroutine ckw06 ( handle,  inst,    ref,     avflag, &
                         first,   last,    segid,   nmini, &
                         npkts,   subtps,  degres,  packts, &
                         rates,   sclkdp,  ivlbds,  sellst )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck06
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! Handle of a CK file open for writing.
      integer(ip) :: inst  !! NAIF instrument ID code.
      character(*) :: ref  !! Reference frame name.
      logical :: avflag  !! Flag indicating if the segment will contain angular velocity.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: nmini  !! Number of mini-segments.
      integer(ip) :: npkts  ( * )  !! Array of packet counts of mini-segments.
      integer(ip) :: subtps ( * )  !! Array of segment subtypes of mini-segments.
      integer(ip) :: degres ( * )  !! Array of polynomial degrees of mini-segments.
      real(wp) :: packts ( * )  !! Array of data packets of mini-segments.
      real(wp) :: rates  ( * )  !! Nominal SCLK rates in seconds per tick.
      real(wp) :: sclkdp ( * )  !! Array of epochs of mini-segments.
      real(wp) :: ivlbds ( * )  !! Mini-segment interval bounds.
      logical :: sellst  !! Interval selection flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw06 ( handle, inst, ref, avflag, first, last, segid, nmini, npkts, subtps, degres, packts, rates, sclkdp, ivlbds, sellst )
      end subroutine ckw06

      !> Add data to a type 03 PCK segment in the binary PCK file
      !> associated with HANDLE. See also PCK03B and PCK03E.
      !>
      !> Legacy interface: calls ctx%pck03a with the legacy context.
      subroutine pck03a ( handle, ncsets, coeffs, epochs )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of a DAF file open for writing.
      integer(ip) :: ncsets  !! The number of Cheby coefficient sets and epochs.
      real(wp) :: coeffs ( * )  !! The collection of Cheby coefficient sets.
      real(wp) :: epochs ( * )  !! The epochs associated with the element sets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pck03a ( handle, ncsets, coeffs, epochs )
      end subroutine pck03a

      !> End the type 03 PCK segment currently being written to the binary
      !> PCK file associated with HANDLE. See also PCK03B and PCK03A.
      !>
      !> Legacy interface: calls ctx%pck03e with the legacy context.
      subroutine pck03e ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! The handle of a binary PCK file open for writing.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pck03e ( handle )
      end subroutine pck03e

      !> Close an open PCK file.
      !>
      !> Legacy interface: calls ctx%pckcls with the legacy context.
      subroutine pckcls ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of the PCK file to be closed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckcls ( handle )
      end subroutine pckcls

      !> Create a new PCK file, returning the handle of the opened file.
      !>
      !> Legacy interface: calls ctx%pckopn with the legacy context.
      subroutine pckopn ( name, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! The name of the PCK file to be opened.
      character(*) :: ifname  !! The internal filename for the PCK.
      integer(ip) :: ncomch  !! The number of characters to reserve for comments.
      integer(ip) :: handle  !! The handle of the opened PCK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckopn ( name, ifname, ncomch, handle )
      end subroutine pckopn

      !> Perform routine error checks and if all checks pass, pack the
      !> descriptor for a PCK segment
      !>
      !> Legacy interface: calls ctx%pckpds with the legacy context.
      subroutine pckpds ( body, frame, type, first, last, descr )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      character(*) :: frame  !! The inertial frame for this segment.
      integer(ip) :: type  !! The type of PCK segment to create.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last  epoch for which the segment is valid.
      real(wp) :: descr ( * )  !! A PCK segment descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckpds ( body, frame, type, first, last, descr )
      end subroutine pckpds

      !> Unpack the contents of a PCK segment descriptor
      !>
      !> Legacy interface: calls ctx%pckuds with the legacy context.
      subroutine pckuds ( descr, body, frame, type, &
                          first, last, begin, end   )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: descr ( * )  !! A PCK segment descriptor.
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      integer(ip) :: frame  !! The code for the inertial frame of this segment.
      integer(ip) :: type  !! The type of PCK segment.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last  epoch for which the segment is valid.
      integer(ip) :: begin  !! Beginning DAF address of the segment.
      integer(ip) :: end  !! Ending DAF address of the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckuds ( descr, body, frame, type, first, last, begin, end )
      end subroutine pckuds

      !> Write a type 2 segment to a PCK binary file given the file
      !> handle, body-fixed frame class ID, frame, time range covered by
      !> the segment, and the Chebyshev polynomial coefficients.
      !>
      !> Legacy interface: calls ctx%pckw02 with the legacy context.
      subroutine pckw02 (  handle,  clssid,  frame,   first, &
                           last,    segid,   intlen,  n, &
                           polydg,  cdata,   btime          )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of binary PCK file open for writing.
      integer(ip) :: clssid  !! Frame class ID of body-fixed frame.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: intlen  !! Length of time covered by logical record.
      integer(ip) :: n  !! Number of logical records in segment.
      integer(ip) :: polydg  !! Chebyshev polynomial degree.
      real(wp) :: cdata (*)  !! Array of Chebyshev coefficients.
      real(wp) :: btime  !! Begin time of first logical record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckw02 ( handle, clssid, frame, first, last, segid, intlen, n, polydg, cdata, btime )
      end subroutine pckw02

      !> Write a type 20 segment to a PCK file.
      !>
      !> Legacy interface: calls ctx%pckw20 with the legacy context.
      subroutine pckw20 (  handle,  clssid,  frame,   first, &
                           last,    segid,   intlen,  n, &
                           polydg,  cdata,   ascale,  tscale, &
                           initjd,  initfr                   )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk20
      implicit none
      integer(ip) :: handle  !! Handle of PCK file open for writing.
      integer(ip) :: clssid  !! NAIF PCK frame class ID.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: intlen  !! Length of time covered by logical record (days).
      integer(ip) :: n  !! Number of logical records in segment.
      integer(ip) :: polydg  !! Chebyshev polynomial degree.
      real(wp) :: cdata  ( * )  !! Array of Chebyshev coefficients and angles.
      real(wp) :: ascale  !! Angular scale of data.
      real(wp) :: tscale  !! Time scale of data.
      real(wp) :: initjd  !! Integer part of begin time (TDB Julian date) of first record.
      real(wp) :: initfr  !! Fractional part of begin time (TDB Julian date) of first record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckw20 ( handle, clssid, frame, first, last, segid, intlen, n, polydg, cdata, ascale, tscale, initjd, initfr )
      end subroutine pckw20

      !> Store text from a text file in the comment area of a binary SPK
      !> or CK file, appending it to whatever text may already have
      !> been stored there.
      !>
      !> Legacy interface: calls ctx%spcac with the legacy context.
      subroutine spcac ( handle, unit, bmark, emark )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle assigned to binary SPK or CK file.
      integer(ip) :: unit  !! Logical unit connected to comment file.
      character(*) :: bmark  !! Beginning marker.
      character(*) :: emark  !! Ending marker.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcac ( handle, unit, bmark, emark )
      end subroutine spcac

      !> Empty the comment area of a binary SPK or CK file.
      !>
      !> Legacy interface: calls ctx%spcdc with the legacy context.
      subroutine spcdc ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle assigned to binary SPK or CK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcdc ( handle )
      end subroutine spcdc

      !> Extract the text from the comment area of a binary SPK or CK file
      !> and write it to a text file.
      !>
      !> Legacy interface: calls ctx%spcec with the legacy context.
      subroutine spcec ( handle, unit )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle assigned to binary SPK or CK file.
      integer(ip) :: unit  !! Logical unit connected to text file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcec ( handle, unit )
      end subroutine spcec

      !> Open a new SPK or CK file for subsequent write requests.
      !>
      !> Legacy interface: calls ctx%spcopn with the legacy context.
      subroutine spcopn ( fname, ifname, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! Name of SPK or CK file to be created.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: handle  !! Handle of new SPK or CK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcopn ( fname, ifname, handle )
      end subroutine spcopn

      !> Read the first line of text from the comment area
      !> of a binary SPK or CK file.
      !>
      !> Legacy interface: calls ctx%spcrfl with the legacy context.
      subroutine spcrfl ( handle, line, eoc )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle assigned to binary SPK or CK file.
      character(*) :: line  !! First line of text from the comment area.
      logical :: eoc  !! End of comments?
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcrfl ( handle, line, eoc )
      end subroutine spcrfl

      !> Continue reading lines from the comment area of a binary
      !> SPK or CK file specified by the most recent call to
      !> the routine SPCRFL.
      !>
      !> Legacy interface: calls ctx%spcrnl with the legacy context.
      subroutine spcrnl ( line, eoc )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: line  !! Next line of text from the comment area.
      logical :: eoc  !! End of comments?
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcrnl ( line, eoc )
      end subroutine spcrnl

      !> Close an open SPK file.
      !>
      !> Legacy interface: calls ctx%spkcls with the legacy context.
      subroutine spkcls ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of the SPK file to be closed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkcls ( handle )
      end subroutine spkcls

      !> Open an existing SPK file for subsequent write.
      !>
      !> Legacy interface: calls ctx%spkopa with the legacy context.
      subroutine spkopa ( file, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: file  !! The name of an existing SPK file.
      integer(ip) :: handle  !! Handle attached to the SPK file opened to append.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkopa ( file, handle )
      end subroutine spkopa

      !> Create a new SPK file, returning the handle of the opened file.
      !>
      !> Legacy interface: calls ctx%spkopn with the legacy context.
      subroutine spkopn ( fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! The name of the new SPK file to be created.
      character(*) :: ifname  !! The internal filename for the SPK file.
      integer(ip) :: ncomch  !! The number of characters to reserve for comments.
      integer(ip) :: handle  !! The handle of the opened SPK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkopn ( fname, ifname, ncomch, handle )
      end subroutine spkopn

      !> Write a type 1 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw01 with the legacy context.
      subroutine spkw01 (  handle,  body,    center,  frame,   first, &
                           last,    segid,   n,       dlines,  epochs )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: n  !! Number of difference lines in segment.
      real(wp) :: dlines ( dlsize, * )  !! Array of difference lines.
      real(wp) :: epochs (         * )  !! Coverage end times of difference lines.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw01 ( handle, body, center, frame, first, last, segid, n, dlines, epochs )
      end subroutine spkw01

      !> Write a type 2 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw02 with the legacy context.
      subroutine spkw02 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   intlen, &
                           n,       polydg,  cdata,   btime  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk02
      implicit none
      integer(ip) :: handle  !! Handle of SPK file open for writing.
      integer(ip) :: body  !! NAIF code for ephemeris object.
      integer(ip) :: center  !! NAIF code for the center of motion of the body.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: intlen  !! Length of time covered by logical record.
      integer(ip) :: n  !! Number of logical records in segment.
      integer(ip) :: polydg  !! Chebyshev polynomial degree.
      real(wp) :: cdata (*)  !! Array of Chebyshev coefficients.
      real(wp) :: btime  !! Begin time of first logical record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw02 ( handle, body, center, frame, first, last, segid, intlen, n, polydg, cdata, btime )
      end subroutine spkw02

      !> Write a type 3 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw03 with the legacy context.
      subroutine spkw03 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   intlen, &
                           n,       polydg,  cdata,   btime )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk03
      implicit none
      integer(ip) :: handle  !! Handle of SPK file open for writing.
      integer(ip) :: body  !! NAIF code for ephemeris object.
      integer(ip) :: center  !! NAIF code for the center of motion of the body.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: intlen  !! Length of time covered by record.
      integer(ip) :: n  !! Number of records in segment.
      integer(ip) :: polydg  !! Chebyshev polynomial degree.
      real(wp) :: cdata (*)  !! Array of Chebyshev coefficients.
      real(wp) :: btime  !! Begin time of first record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw03 ( handle, body, center, frame, first, last, segid, intlen, n, polydg, cdata, btime )
      end subroutine spkw03

      !> Write an SPK segment of type 5 given a time-ordered set of
      !> discrete states and epochs, and the gravitational parameter
      !> of a central body.
      !>
      !> Legacy interface: calls ctx%spkw05 with the legacy context.
      subroutine spkw05 ( handle, body,  center, frame,  first, last, &
                          segid,  gm,    n,      states, epochs      )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! Body code for ephemeris object.
      integer(ip) :: center  !! Body code for the center of motion of the body.
      character(*) :: frame  !! The reference frame of the states.
      real(wp) :: first  !! First valid time for which states can be computed.
      real(wp) :: last  !! Last valid time for which states can be computed.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: gm  !! Gravitational parameter of central body.
      integer(ip) :: n  !! Number of states and epochs.
      real(wp) :: states   ( 6, * )  !! States.
      real(wp) :: epochs   (    * )  !! Epochs.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw05 ( handle, body, center, frame, first, last, segid, gm, n, states, epochs )
      end subroutine spkw05

      !> Write a type 8 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw08 with the legacy context.
      subroutine spkw08 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   degree, &
                           n,       states,  begtim,  step     )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk08
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: degree  !! Degree of interpolating polynomials.
      integer(ip) :: n  !! Number of states.
      real(wp) :: states ( 6, * )  !! Array of states.
      real(wp) :: begtim  !! Epoch of first state in STATES array.
      real(wp) :: step  !! Time step separating epochs of states.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw08 ( handle, body, center, frame, first, last, segid, degree, n, states, begtim, step )
      end subroutine spkw08

      !> Write a type 9 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw09 with the legacy context.
      subroutine spkw09 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   degree, &
                           n,       states,  epochs           )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: degree  !! Degree of interpolating polynomials.
      integer(ip) :: n  !! Number of states.
      real(wp) :: states ( 6, * )  !! Array of states.
      real(wp) :: epochs (    * )  !! Array of epochs corresponding to states.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw09 ( handle, body, center, frame, first, last, segid, degree, n, states, epochs )
      end subroutine spkw09

      !> Write an SPK type 10 segment to the file specified by
      !> the input HANDLE.
      !>
      !> Legacy interface: calls ctx%spkw10 with the legacy context.
      subroutine spkw10 ( handle, body,   center, frame,  first, last, &
                          segid,  consts, n,      elems,  epochs      )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! The handle of a DAF file open for writing.
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      integer(ip) :: center  !! The center of motion for BODY.
      character(*) :: frame  !! The reference frame for this segment.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last  epoch for which the segment is valid.
      character(*) :: segid  !! The string to use for segment identifier.
      real(wp) :: consts ( * )  !! Array of geophysical constants for the segment.
      integer(ip) :: n  !! The number of element/epoch pairs to be stored
      real(wp) :: elems  ( * )  !! The collection of "two-line" element sets.
      real(wp) :: epochs ( * )  !! The epochs associated with the element sets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw10 ( handle, body, center, frame, first, last, segid, consts, n, elems, epochs )
      end subroutine spkw10

      !> Write a type 12 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw12 with the legacy context.
      subroutine spkw12 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   degree, &
                           n,       states,  begtim,  step     )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk12
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: degree  !! Degree of interpolating polynomials.
      integer(ip) :: n  !! Number of states.
      real(wp) :: states ( 6, * )  !! Array of states.
      real(wp) :: begtim  !! Epoch of first state in STATES array.
      real(wp) :: step  !! Time step separating epochs of states.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw12 ( handle, body, center, frame, first, last, segid, degree, n, states, begtim, step )
      end subroutine spkw12

      !> Write a type 13 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw13 with the legacy context.
      subroutine spkw13 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   degree, &
                           n,       states,  epochs           )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: degree  !! Degree of interpolating polynomials.
      integer(ip) :: n  !! Number of states.
      real(wp) :: states ( 6, * )  !! Array of states.
      real(wp) :: epochs (    * )  !! Array of epochs corresponding to states.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw13 ( handle, body, center, frame, first, last, segid, degree, n, states, epochs )
      end subroutine spkw13

      !> Write an SPK segment of type 15 given a type 15 data record.
      !>
      !> Legacy interface: calls ctx%spkw15 with the legacy context.
      subroutine spkw15 ( handle, body,  center, frame,  first, last, &
                          segid,  epoch, tp,     pa,     p,     ecc, &
                          j2flg,  pv,    gm,     j2,     radius      )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! Body code for ephemeris object.
      integer(ip) :: center  !! Body code for the center of motion of the body.
      character(*) :: frame  !! The reference frame of the states.
      real(wp) :: first  !! First valid time for which states can be computed.
      real(wp) :: last  !! Last valid time for which states can be computed.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: epoch  !! Epoch of the periapse.
      real(wp) :: tp     ( 3 )  !! Trajectory pole vector.
      real(wp) :: pa     ( 3 )  !! Periapsis vector.
      real(wp) :: p  !! Semi-latus rectum.
      real(wp) :: ecc  !! Eccentricity.
      real(wp) :: j2flg  !! J2 processing flag.
      real(wp) :: pv     ( 3 )  !! Central body pole vector.
      real(wp) :: gm  !! Central body GM.
      real(wp) :: j2  !! Central body J2.
      real(wp) :: radius  !! Equatorial radius of central body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw15 ( handle, body, center, frame, first, last, segid, epoch, tp, pa, p, ecc, j2flg, pv, gm, j2, radius )
      end subroutine spkw15

      !> Write an SPK segment of type 17 given a type 17 data record.
      !>
      !> Legacy interface: calls ctx%spkw17 with the legacy context.
      subroutine spkw17 ( handle, body,  center, frame, first, last, &
                          segid,  epoch, eqel,   rapol, decpol  )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! Body code for ephemeris object.
      integer(ip) :: center  !! Body code for the center of motion of the body.
      character(*) :: frame  !! The reference frame of the states.
      real(wp) :: first  !! First valid time for which states can be computed.
      real(wp) :: last  !! Last valid time for which states can be computed.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: epoch  !! Epoch of elements in seconds past J2000.
      real(wp) :: eqel  ( 9 )  !! Array of equinoctial elements.
      real(wp) :: rapol  !! Right Ascension of the reference plane's pole.
      real(wp) :: decpol  !! Declination of the reference plane's pole.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw17 ( handle, body, center, frame, first, last, segid, epoch, eqel, rapol, decpol )
      end subroutine spkw17

      !> Write a type 18 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw18 with the legacy context.
      subroutine spkw18 (  handle,  subtyp,  body,    center, &
                           frame,   first,   last,    segid, &
                           degree,  n,       packts,  epochs  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk18
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: subtyp  !! SPK type 18 subtype code.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: degree  !! Degree of interpolating polynomials.
      integer(ip) :: n  !! Number of packets.
      real(wp) :: packts ( * )  !! Array of packets.
      real(wp) :: epochs ( * )  !! Array of epochs corresponding to packets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw18 ( handle, subtyp, body, center, frame, first, last, segid, degree, n, packts, epochs )
      end subroutine spkw18

      !> Write a type 19 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw19 with the legacy context.
      subroutine spkw19 ( handle,  body,    center,  frame, &
                          first,   last,    segid,   nintvl, &
                          npkts,   subtps,  degres,  packts, &
                          epochs,  ivlbds,  sellst          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk19
      use pepper_inc_spkrec
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF ID code for an ephemeris object.
      integer(ip) :: center  !! NAIF ID code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: nintvl  !! Number of mini-segments and interpolation intervals.
      integer(ip) :: npkts  ( * )  !! Array of packet counts of mini-segments.
      integer(ip) :: subtps ( * )  !! Array of segment subtypes of mini-segments.
      integer(ip) :: degres ( * )  !! Array of polynomial degrees of mini-segments.
      real(wp) :: packts ( * )  !! Array of data packets of mini-segments.
      real(wp) :: epochs ( * )  !! Array of epochs of mini-segments.
      real(wp) :: ivlbds ( * )  !! Interpolation interval bounds.
      logical :: sellst  !! Interval selection flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw19 ( handle, body, center, frame, first, last, segid, nintvl, npkts, subtps, degres, packts, epochs, ivlbds, sellst )
      end subroutine spkw19

      !> Write a type 20 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw20 with the legacy context.
      subroutine spkw20 (  handle,  body,    center,  frame, &
                           first,   last,    segid,   intlen, &
                           n,       polydg,  cdata,   dscale, &
                           tscale,  initjd,  initfr          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk20
      implicit none
      integer(ip) :: handle  !! Handle of SPK file open for writing.
      integer(ip) :: body  !! NAIF code for ephemeris object.
      integer(ip) :: center  !! NAIF code for the center of motion of the body.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      real(wp) :: intlen  !! Length of time covered by logical record (days).
      integer(ip) :: n  !! Number of logical records in segment.
      integer(ip) :: polydg  !! Chebyshev polynomial degree.
      real(wp) :: cdata  ( * )  !! Array of Chebyshev coefficients and positions.
      real(wp) :: dscale  !! Distance scale of data.
      real(wp) :: tscale  !! Time scale of data.
      real(wp) :: initjd  !! Integer part of begin time (TDB Julian date) of first record.
      real(wp) :: initfr  !! Fractional part of begin time of first record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw20 ( handle, body, center, frame, first, last, segid, intlen, n, polydg, cdata, dscale, tscale, initjd, initfr )
      end subroutine spkw20

      !> Write a type 21 segment to an SPK file.
      !>
      !> Legacy interface: calls ctx%spkw21 with the legacy context.
      subroutine spkw21 ( handle, body,   center, frame, &
                          first,  last,   segid,  n, &
                          dlsize, dlines, epochs        )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk21
      implicit none
      integer(ip) :: handle  !! Handle of an SPK file open for writing.
      integer(ip) :: body  !! NAIF code for an ephemeris object.
      integer(ip) :: center  !! NAIF code for center of motion of BODY.
      character(*) :: frame  !! Reference frame name.
      real(wp) :: first  !! Start time of interval covered by segment.
      real(wp) :: last  !! End time of interval covered by segment.
      character(*) :: segid  !! Segment identifier.
      integer(ip) :: n  !! Number of difference lines in segment.
      integer(ip) :: dlsize  !! Difference line size.
      real(wp) :: dlines ( dlsize, * )  !! Array of difference lines.
      real(wp) :: epochs (         * )  !! Coverage end times of difference lines.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkw21 ( handle, body, center, frame, first, last, segid, n, dlsize, dlines, epochs )
      end subroutine spkw21

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzck4i2d with the legacy context.
      subroutine zzck4i2d ( i, nsets, parcod, dpcoef )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: i      ( * )  !! Array of NSETS integer components.
      integer(ip) :: nsets  !! Number of integer components in input array I.
      real(wp) :: parcod  !! Packing base.
      real(wp) :: dpcoef  !! DP number containing NSETS packed integer numbers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzck4i2d ( ctx, i, nsets, parcod, dpcoef )
      end subroutine zzck4i2d

      !> Return a specified pointing instance from a CK type 02 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%ckgr02 with the legacy context.
      subroutine ckgr02 ( handle, descr, recno, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! The handle of the CK file containing the segment.
      real(wp) :: descr   ( * )  !! The segment descriptor.
      integer(ip) :: recno  !! The number of the pointing record to be returned.
      real(wp) :: record  ( * )  !! The pointing record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckgr02 ( handle, descr, recno, record )
      end subroutine ckgr02

      !> Add data to a type 4 CK segment currently being written to
      !> the file associated with HANDLE. See also CKW04B and CKW04E.
      !>
      !> Legacy interface: calls ctx%ckw04a with the legacy context.
      subroutine ckw04a ( handle, npkts, pktsiz, pktdat, sclkdp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! The handle of an DAF file opened for writing.
      integer(ip) :: npkts  !! Number of data packets to write to a segment.
      integer(ip) :: pktsiz( * )  !! The numbers of values in the data packets
      real(wp) :: pktdat( * )  !! The data packets.
      real(wp) :: sclkdp( * )  !! The SCLK times associated with the data packets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckw04a ( handle, npkts, pktsiz, pktdat, sclkdp )
      end subroutine ckw04a

      !> Begin a type 03 PCK segment in the binary PCK file associated with
      !> HANDLE. See also PCK03A and PCK03E.
      !>
      !> Legacy interface: calls ctx%pck03b with the legacy context.
      subroutine pck03b ( handle, &
                          segid,  body,  frame, &
                          first,  last,  chbdeg  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! The handle of a DAF file open for writing.
      character(*) :: segid  !! The string to use for segment identifier.
      integer(ip) :: body  !! The NAIF ID code for the body of the segment.
      character(*) :: frame  !! The inertial frame for this segment.
      real(wp) :: first  !! The first epoch for which the segment is valid.
      real(wp) :: last  !! The last epoch for which the segment is valid.
      integer(ip) :: chbdeg  !! The degree of the Chebyshev Polynomial used.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pck03b ( handle, segid, body, frame, first, last, chbdeg )
      end subroutine pck03b

      !> Convert the contents of a binary SPK or CK file to text,
      !> including comments if present, and write them to a text file
      !> opened by the calling program.
      !>
      !> Legacy interface: calls ctx%spcb2t with the legacy context.
      subroutine spcb2t ( binary, unit )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: binary  !! Name of an existing binary SPK or CK file.
      integer(ip) :: unit  !! Logical unit connected to a text file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcb2t ( binary, unit )
      end subroutine spcb2t

      !> Reconstruct a binary SPK or CK file including comments
      !> from a text file opened by the calling program.
      !>
      !> Legacy interface: calls ctx%spct2b with the legacy context.
      subroutine spct2b ( unit, binary )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Logical unit connected to the text format file.
      character(*) :: binary  !! Name of a binary SPK or CK file to be created.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spct2b ( unit, binary )
      end subroutine spct2b

      !> Convert a text (ASCII) format SPK or CK file to an equivalent
      !> binary file, including comments.
      !>
      !> Legacy interface: calls ctx%spca2b with the legacy context.
      subroutine spca2b ( text, binary )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: text  !! Name of an existing text format SPK or CK file.
      character(*) :: binary  !! Name of a binary SPK or CK file to be created.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spca2b ( text, binary )
      end subroutine spca2b

      !> Convert a binary SPK or CK file to an equivalent text (ASCII)
      !> file, including the comment area.
      !>
      !> Legacy interface: calls ctx%spcb2a with the legacy context.
      subroutine spcb2a ( binary, text )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: binary  !! Name of an existing binary SPK or CK file.
      character(*) :: text  !! Name of a text file to be created.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spcb2a ( binary, text )
      end subroutine spcb2a

end module pepper_legacy_writers
