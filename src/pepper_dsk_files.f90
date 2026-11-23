!> DLA (DAS linked array) files and DSK files: segment lists, opening and closing, reading and
!> writing type 2 segments (plates, vertices, voxels), their descriptors, and the DSK tolerances.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## DSKTOL (DSK, tolerance umbrella)
!>
!> PEPPER: SPICELIB's umbrella routine DSKTOL is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_dsktol. Its documentation follows.
!>
!>### Abstract
!>
!>     Umbrella routine for DSK tolerance and margin parameter access
!>     routines.
!>
!>### Detailed Input
!>
!>     See the entry points for descriptions of their input arguments.
!>
!>### Detailed Output
!>
!>     See the entry points for descriptions of their output arguments.
!>
!>### Parameters
!>
!>     See the include file
!>
!>        dsktol.inc
!>
!>     for descriptions and values of the tolerance or margin parameters
!>     accessed by the entry points of this routine, and of the keyword
!>     parameters used to refer to them.
!>
!>### Exceptions
!>
!>     1)  If this routine is called directly, the error
!>         SPICE(BOGUSENTRY) is signaled.
!>
!>     2)  See the entry points for descriptions of exceptions specific
!>         to those entry points.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     The routines in this package serve to centralize numeric
!>     tolerance and margin values used by the DSK subsystem. The
!>     subsystem retrieves values from this package to use at run time.
!>
!>     The entry points of this routine are
!>
!>        DSKGTL {DSK, get tolerance value}
!>        DSKSTL {DSK, set tolerance value}
!>
!>     To minimize run time overhead, the "keywords" used by these
!>     routines to identify parameters are actually integer codes.
!>
!>     SPICE users may override certain values maintained by this
!>     package; others values are fixed.
!>
!>     One use of this system would be to disable the "greedy"
!>     algorithms used to prevent "false miss" ray-surface intercepts
!>     results. Setting to zero the margins used for this purpose would
!>     accomplish this.
!>
!>     It is recommended that any change to the tolerance values made at
!>     run time be programmed only by SPICE experts.
!>
!>### Examples
!>
!>     See the entry points.
!>
!>### Restrictions
!>
!>     1)  The default settings used by the DSK subsystem should
!>         be overridden only by expert SPICE users.
!>
!>     2)  The entry points of this routine do not check the
!>         validity of new parameter values supplied by the
!>         calling application.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>
!>### Version
!>
!>-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
!>
!>        Edited the entry points and umbrella headers to comply with
!>        NAIF standard.
!>
!>-    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB)
!>
!>### Implementation notes
!>
!>     Saved values
!>
!>     Caution: this array must be declared in
!>     the order defined by the keywords declared
!>     in dsktol.inc.
submodule (pepper_instance_module) pepper_dsk_files

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: ibfsiz = 10  !! size of a buffer of integers
   integer(ip), parameter :: namlen = 6  !! length of a DSK parameter name
   integer(ip), parameter :: ndspar = 6  !! number of DSK parameters
   integer(ip), parameter :: toklen = 4  !! length of a token

contains

      !> This routine exists to determine whether or not the type of
      !> a DAF segment is compatible with the sizes allowed for SPK
      !> type 01 segments or CK type 02 segments.  However, more generally
      !> it determines whether or not the integer equation:
      !>
      !> SIZE = PSIZE*N + (N-OFFSET)/DSIZE
      !>
      !> can be satisfied for some value of N.  Moreover, if such
      !> an N exists (there can be only one) it returns that value.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     SIZE       Constant terms in the equation given in the abstract.
      !>     PSIZE
      !>     DSIZE
      !>
      !>     OFFSET     Constant term in the equation above.  It should be
      !>                1 or 0.
      !>
      !>## Detailed Output
      !>
      !>     OK         is TRUE if an integer solution for N exists.  Otherwise
      !>                it is returned FALSE.
      !>
      !>     N          is the solution to the equation in the abstract
      !>                if such a solution exists.  Otherwise it is returned
      !>                with the value zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If SIZE, PSIZE, or DSIZE is less than 1, OK is set to FALSE
      !>         N is set to zero and no attempt is made at finding a
      !>         solution.
      !>
      !>## Particulars
      !>
      !>     This routine determines whether or not the integer arithmetic
      !>     equation
      !>
      !>         SIZE = PSIZE*N + (N-1)/DSIZE
      !>
      !>     has a solution for N and if so returns the value of N.
      !>
      !>     The routine is intended for checking the sizes of segments
      !>     for SPK type 01 and CK type 02.  For SPK type 01,
      !>
      !>        SIZE   = segment size - 1
      !>        PSIZE  = 72
      !>        DSIZE  = 100
      !>        OFFSET = 0
      !>
      !>     for CK type 02,
      !>
      !>        SIZE   = segment size
      !>        PSIZE  = 10
      !>        DSIZE  = 100
      !>        OFFSET = 1
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 02-DEC-1999 (WLT)
      module subroutine zzsizeok ( ctx, size, psize, dsize, offset, ok, n )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: size  !! Left hand side of the equation in the abstract
      integer(ip), intent(in) :: psize  !! Coefficient of N (packet size).
      integer(ip), intent(in) :: dsize  !! Divisor of N-OFFSET (directory size).
      integer(ip), intent(in) :: offset  !! Offset used in computation of number of directories
      logical, intent(out) :: ok  !! TRUE if a solution for N exists.
      integer(ip), intent(out) :: n  !! Value of N if there is a solution, 0 otherwise.

!
!     Here's the scoop.
!
!     Suppose N is a solution to SIZE = PSIZE*N + (N-OFFSET)/DSIZE
!     N can be represented uniquely as
!
!         N = q*DSIZE + r
!
!     where OFFSET <= r <= DSIZE+OFFSET-1.  Therefore there must
!     be values q and r such that
!
!        SIZE = PSIZE*(q*DSIZE + r ) + ( q*DSIZE + r - 1 ) / DSIZE
!
!             = PSIZE*DSIZE*q + q + PSIZE*r
!
!             = (PSIZE*DSIZE+1)*q + PSIZE*r
!
!     But SIZE can be represented uniquely as
!
!           SIZE = (PSIZE*DSIZE+1)*k + a
!
!     where  0 <= a < (PSIZE*DSIZE+1).
!
!     But   PSIZE*OFFSET < PSIZE*r < (PSIZE*DSIZE+OFFSET-1),
!    therefore  it must be that
!
!              SIZE mod(PSIZE*DSIZE+1) = PSIZE*r
!     and                            q = k
!
!     Hence, there is a solution to our equation if and only if
!
!          PSIZE divides SIZE mod(PSIZE*DSIZE+1)
!     and  OFFSET*PSIZE <= SIZE mod(PSIZE*DSIZE+1)
!

      integer(ip) :: q
      integer(ip) :: pd1
      integer(ip) :: r
      integer(ip) :: a

!
!     Handle the exceptional case first.
!
      if (       size <= 0 &
           .or. dsize <= 0 &
           .or. psize <= 0 ) then
         n  = 0
         ok = .false.
         return
      end if

      pd1 = psize*dsize + 1

      call rmaini ( ctx, size, pd1, q, a )

      if ( offset*psize > a ) then
         n  = 0
         ok = .false.
         return
      end if

      if ( a == (a/psize)*psize ) then

         r  = a/psize
         n  = dsize*q + r
         ok = .true.

      else

         ok = .false.
         n  =  0

      end if

      return
      end subroutine zzsizeok

      !> This routine determines whether or not a DAF file attached to
      !> the supplied handle is an SPK, CK or unknown file.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of a DAF file open for read.
      !>
      !>## Detailed Output
      !>
      !>     CKSPK      is a string containing one of the following 3 values
      !>                'SPK', 'CK' or '?'
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine examines the first segment of a DAF that is
      !>     a candidate for being an SPK or CK and returns a diagnosis
      !>     of the type of the file.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.0, 03-DEC-1999 (WLT)
      module subroutine zzckspk ( ctx, handle, ckspk )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! the handle of a DAF file open for read access.
      character(*), intent(out) :: ckspk  !! the type of the DAF file (SPK,CK or ?)
      real(wp) :: chcktm_1(1)  !! one-element copy of chcktm (array argument)
      real(wp) :: frsttm_1(1)  !! one-element copy of frsttm (array argument)
      real(wp) :: lastdp_1(1)  !! one-element copy of lastdp (array argument)

!     The following parameters point to the various slots in the
!     integer portion of the DAF descriptor where the values are
!     located.
!
      integer(ip), parameter :: bodid = 1

      integer(ip), parameter :: cenid = bodid  + 1

      integer(ip), parameter :: spkfrm = cenid  + 1

      integer(ip), parameter :: spktyp = spkfrm + 1

      integer(ip), parameter :: start = spktyp + 1

      integer(ip), parameter :: finish = start  + 1

      integer(ip), parameter :: ckrate = spktyp

      integer(ip), parameter :: cktype = spkfrm

!
!     These parameters give the number of integer and double precision
!     components of the descriptor for SPK and CK files.
!
      integer(ip), parameter :: nd = 2

      integer(ip), parameter :: ni = 6
!
!     The size of a summary.
!
      integer(ip), parameter :: sumsiz = nd + ((ni+1)/2)

      real(wp) :: chcktm
      real(wp) :: dc    ( nd )
      real(wp) :: lastdp
      real(wp) :: frsttm
      real(wp) :: sum   ( sumsiz )
      real(wp) :: times ( 2 )

      integer(ip) :: angvel
      integer(ip) :: first
      integer(ip) :: from
      integer(ip) :: ic    ( ni )
      integer(ip) :: last
      integer(ip) :: nck2
      integer(ip) :: nspk
      integer(ip) :: size
      integer(ip) :: thisnd
      integer(ip) :: thisni
      integer(ip) :: to
      integer(ip) :: type

      logical :: ck2ok
      logical :: found
      logical :: spkok

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Make sure the values of ND and NI associated with this file
!     have the correct values.
!
      call dafhsf ( ctx, handle, thisnd, thisni )

      if (     thisnd /= nd &
          .or. thisni /= ni ) then
         ckspk = '?'
         return
      end if

!
!     We've got the correct values for ND and NI, examine the descriptor
!     for the first array.
!
      call dafbfs ( ctx, handle )
      call daffna ( ctx, found  )

      if ( ctx%failed() ) then
         ckspk = '?'
         return
      end if

!
!     If we don't find any segments, we don't have a clue about
!     the file type.
!
      if ( .not. found ) then

         ckspk = '?'
         return

      end if
!
!     Unpack the summary record.
!
      call dafgs ( ctx, sum                 )
      call dafus ( ctx, sum, nd, ni, dc, ic )

!
!     Look at the slot where the angular velocity flag would
!     be located if this is a CK file.
!
      angvel = ic(ckrate)
      type   = ic(cktype)

!
!     Test 1. The value of ANGVEL may do the trick
!     right at the start.
!
      if ( angvel == 0 ) then
         ckspk   = 'CK'
         return
      end if

      if ( angvel > 1 ) then
         ckspk   = 'SPK'
         return
      end if

!
!     Test 2. If this is an SPK file, it has a type 01 segment.
!     See if this is something orbiting the solar system
!     barycenter.
!
      if ( ic(cenid) == 0 ) then
         ckspk   = 'SPK'
         return
      end if

!
!     Test 3. This is the super test.  Compute the size of the
!     segment and fetch the last d.p. from the segment.
!

      first  = ic(start)
      last   = ic(finish)
      size   = last  - first +  1
!
!     Check the size of the array to see if it has any chance
!     of being an SPK and if it does get the number of MDA records.
!
      call zzsizeok ( ctx, size-1, 72, 100, 0, spkok, nspk )

      if ( .not. spkok ) then
         ckspk = 'CK'
         return
      end if

      call dafgda ( ctx, handle, last, last, lastdp_1 )
      lastdp = lastdp_1(1)
!
!     See if the last number in the file is the allowed number of
!     MDA records.  If not, this must be a CK segment.
!
      if ( lastdp /= real(nspk, wp) ) then
         ckspk   = 'CK'
         return
      end if
!
!     If we are still here, the last d.p. in the segment matches the
!     expected number of MDA records.  If the potential CK type is
!     not 2, we must have an SPK file.
!
      if ( type /= 2 ) then
         ckspk = 'SPK'
         return
      end if
!
!     We are getting down to the nitty gritty here. See if the
!     size is compatible with a type 02 C-kernel.
!
      call zzsizeok ( ctx, size, 10, 100, 1, ck2ok, nck2 )

      if ( .not. ck2ok ) then
         ckspk = 'SPK'
         return
      end if

!
!     So much for being nice. We need to examine the structure of the
!     actual data in the segment.  There are two cases to consider:
!     when there is 1 or fewer type 02 CK directory records and when
!     there is more than 1.  Note that to get to this point there must
!     be at least 1 directory value if this is a CK type 02 segment.
!     (To see this check the sizes when ZZSIZEOK returns TRUE for
!     both type 1 SPK and type 02 CK. The only such sizes in which
!     there the number CK type 02 directory values is one or fewer
!     are SIZE = 1081, 1441, and 1801 which correspond to (NSPK,NCK2) =
!     (15,108), (20,144), (25, 180).  In all of these cases there is
!     exactly 1 ck type 02 directory value.)
!
      if ( nck2 < 201 ) then
!
!        Recall that MDA record contains its stop time as the first
!        entry of the record.  These epochs show up duplicated in the
!        epochs portion of the segment.
!
!        If this is a type 01 SPK segment, there are no directory
!        records and the first epoch shows up in the slot NSPK before
!        the last slot of the segment.  If it is a type 02 CK segment
!        the last stop tick shows up in this slot.  We need to look
!        at this value to see what's up.
!
         call dafgda ( ctx, handle, last-nspk, last-nspk, frsttm_1 )
         frsttm = frsttm_1(1)

!
!        Now (under the assumption that we have an SPK segment) look
!        up the epoch from the last MDA record--- the NSPK'th
!        record.  This epoch must be greater than the first epoch
!        in the array of epochs.
         from = first + (nspk-1)*71
         to   = from
         call dafgda ( ctx, handle, from, to, chcktm_1 )
         chcktm = chcktm_1(1)

!
!        If this is a type 02 segment.  The value we just picked out
!        will come from the array of stop ticks.  The array of stop
!        ticks is non-decreasing so:
!
         if ( chcktm > frsttm ) then
            ckspk = 'SPK'
         else
            ckspk = 'CK'
         end if

      else
!
!        In this case there are at least 2 directory records if we
!        have a CK.  We read the last potential tick value and the
!        first potential directory value..  Note that the last potential
!        stop tick must be greater than the first potential directory
!        record.
!
         from = last - (nck2-1)/100
         to   = from + 1

         call dafgda ( ctx, handle, from, to, times )
!
!        If we happen to have a TYPE 01 SPK segment we've just
!        read two consecutive values from the epochs sub-array of the
!        segment. Here's a sketch of why this is so:
!
!          The number of directory records for a CK type 02 segment is
!          (NCK2-1)/100  which is the same as SIZE/1001.
!
!          The number of directory records for an SPK type 01 segment is
!          (NSPK-1)/100  which is the same as SIZE/7201.
!
!          The number of stop ticks for type 02 CK is NCK2 ~ SIZE/10
!
!          The number of epochs for a type 01 SPK is  NSPK ~ SIZE/72
!
!        so NSPK directories < NCK2 directories < NCK2 directories + 1
!        < NSPK + NSPK directories < NCK2.  Consequently, the
!        two values just read are either the last stop tick and the
!        first CK directory value or two consecutive epochs.
!        In the first case TIMES(1) > TIMES(2), in the later case
!        we have TIMES(1) < TIMES(2)
!
         if ( times(1) > times(2) ) then
            ckspk = 'CK'
         else
            ckspk = 'SPK'
         end if

      end if

      return

      end subroutine zzckspk

      !> Determine the architecture and type of SPICE kernels.
      !>
      !>## Detailed Input
      !>
      !>     FILE     is the name of a SPICE kernel file whose architecture
      !>              and type are desired.
      !>
      !>## Detailed Output
      !>
      !>     ARCH     is the file architecture of the SPICE kernel file
      !>              specified by FILE. If the architecture cannot be
      !>              determined or is not recognized the value '?' is
      !>              returned.
      !>
      !>              Architectures currently recognized are:
      !>
      !>                 DAF -- The file is based on the DAF architecture.
      !>                 DAS -- The file is based on the DAS architecture.
      !>                 XFR -- The file is in a SPICE transfer file format.
      !>                 DEC -- The file is an old SPICE decimal text file.
      !>                 ASC -- An ASCII text file.
      !>                 KPL -- Kernel Pool File (i.e., a text kernel)
      !>                 TXT -- An ASCII text file.
      !>                 TE1 -- Text E-Kernel type 1.
      !>                 ?   -- The architecture could not be determined.
      !>
      !>              This variable must be at least 3 characters long.
      !>
      !>     KERTYP   is the type of the SPICE kernel file. If the type
      !>              can not be determined the value '?' is returned.
      !>
      !>              Kernel file types may be any sequence of at most four
      !>              printing characters. NAIF has reserved for its use
      !>              types which contain all upper case letters.
      !>
      !>              A file type of 'PRE' means that the file is a
      !>              pre-release file.
      !>
      !>              This variable may be at most 4 characters long.
      !>
      !>## Parameters
      !>
      !>     RECL     is the record length of a binary kernel file. Each
      !>              record must be large enough to hold 128 double
      !>              precision numbers. The units in which the record
      !>              length must be specified vary from environment to
      !>              environment. For example, VAX Fortran requires
      !>              record lengths to be specified in longwords,
      !>              where two longwords equal one double precision
      !>              number.
      !>
      !>## Exceptions
      !>
      !>     1)  If the filename specified is blank, the error
      !>         SPICE(BLANKFILENAME) is signaled.
      !>
      !>     2)  If any inquire on the filename specified by FILE, required to
      !>         obtain information about the physical file, fails for some
      !>         reason, the error SPICE(INQUIREERROR) is signaled.
      !>
      !>     3)  If the file specified by FILE does not exist, the error
      !>         SPICE(FILENOTFOUND) is signaled.
      !>
      !>     4)  If the file specified by FILE is already open but not through
      !>         SPICE interfaces, the error SPICE(EXTERNALOPEN) is signaled.
      !>
      !>     5)  If an attempt to open the file specified by FILE fails when
      !>         this routine requires that it succeed, the error
      !>         SPICE(FILEOPENFAILED) is signaled.
      !>
      !>     6)  If an attempt to read the file specified by FILE fails when
      !>         this routine requires that it succeed, the error
      !>         SPICE(FILEREADFAILED) is signaled.
      !>
      !>     7)  If an issue is detected during the opening the input file or
      !>         the process to determine its architecture and type, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     8)  If the ID word in a DAF based kernel is 'NAIF/DAF', then the
      !>         algorithm GETFAT uses to distinguish between CK and SPK
      !>         kernels may result in an indeterminate KERTYP if the SPK or
      !>         CK files have invalid first segments.
      !>
      !>## Files
      !>
      !>     The SPICE kernel file specified by FILE is opened and then
      !>     closed by this routine to determine its file architecture and
      !>     type. Filenames of open files should not be passed to this
      !>     routine.
      !>
      !>## Particulars
      !>
      !>     This subroutine is a support utility routine that determines the
      !>     architecture and type of a SPICE kernel file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Determine the file architecture and file type of all the
      !>        kernels loaded through a meta-kernel, and of a kernel in
      !>        transfer format.
      !>
      !>        Use the SPK kernel below to provide an example of a kernel in
      !>        transfer format.
      !>
      !>           earthstns_itrf93_050714.xsp
      !>
      !>        Use the meta-kernel shown below to load the other types of
      !>        SPICE kernels.
      !>
      !>           KPL/MK
      !>
      !>           File: getfat_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                        Contents
      !>              ---------                        --------
      !>              de430.bsp                        Planetary ephemeris
      !>              mar097.bsp                       Mars satellite ephemeris
      !>              pck00010.tpc                     Planet orientation and
      !>                                               radii
      !>              naif0011.tls                     Leapseconds
      !>              mgs_moc_v20.ti                   MGS MOC instrument
      !>                                               parameters
      !>              mgs_sclkscet_00061.tsc           MGS SCLK coefficients
      !>              mgs_sc_ext12.bc                  MGS s/c bus attitude
      !>              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris
      !>              megr90n000cb_plate.bds           Plate model based on
      !>                                               MEGDR DEM, resolution
      !>                                               4 pixels/degree.
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de430.bsp',
      !>                                  'mar097.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0011.tls',
      !>                                  'mgs_moc_v20.ti',
      !>                                  'mgs_sclkscet_00061.tsc',
      !>                                  'mgs_sc_ext12.bc',
      !>                                  'mgs_ext12_ipng_mgs95j.bsp',
      !>                                  'megr90n000cb_plate.bds'      )
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM GETFAT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               ARCHLN
      !>      PARAMETER           ( ARCHLN = 4 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 256 )
      !>
      !>      INTEGER               KTYPLN
      !>      PARAMETER           ( KTYPLN = 5 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(ARCHLN)    ARCH
      !>      CHARACTER*(FILSIZ)    FNAME
      !>      CHARACTER*(28)        FNAME1
      !>      CHARACTER*(KTYPLN)    KTYPE
      !>      CHARACTER*(FILSIZ)    SOURCE
      !>
      !>      INTEGER               COUNT
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Check the file architecture and type of an SPK
      !>C     in transfer format.
      !>C
      !>      FNAME1 = 'earthstns_itrf93_050714.xsp'
      !>
      !>      CALL GETFAT ( FNAME1, ARCH, KTYPE )
      !>
      !>      WRITE(*,*) 'File name     : ', FNAME1
      !>      WRITE(*,*) '  Architecture: ', ARCH
      !>      WRITE(*,*) '  Kernel type : ', KTYPE
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Load the kernels.
      !>C
      !>      CALL FURNSH ( 'getfat_ex1.tm' )
      !>
      !>C
      !>C     Get the file architecture and kernel type for each of
      !>C     the kernels in the kernel pool.
      !>C
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      DO I= 1, COUNT
      !>
      !>         CALL KDATA ( I,     'ALL', FNAME, KTYPE, SOURCE,
      !>     .                HANDLE, FOUND                      )
      !>
      !>         CALL GETFAT ( FNAME, ARCH, KTYPE )
      !>
      !>         WRITE(*,*) 'File name     : ', FNAME
      !>         WRITE(*,*) '  Source      : ', SOURCE
      !>         WRITE(*,*) '  Architecture: ', ARCH
      !>         WRITE(*,*) '  Kernel type : ', KTYPE
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         File name     : earthstns_itrf93_050714.xsp
      !>           Architecture: XFR
      !>           Kernel type : DAF
      !>
      !>         File name     : getfat_ex1.tm
      !>           Source      :
      !>           Architecture: KPL
      !>           Kernel type : MK
      !>
      !>         File name     : de430.bsp
      !>           Source      : getfat_ex1.tm
      !>           Architecture: DAF
      !>           Kernel type : SPK
      !>
      !>         File name     : mar097.bsp
      !>           Source      : getfat_ex1.tm
      !>           Architecture: DAF
      !>           Kernel type : SPK
      !>
      !>         File name     : pck00010.tpc
      !>           Source      : getfat_ex1.tm
      !>           Architecture: KPL
      !>           Kernel type : PCK
      !>
      !>         File name     : naif0011.tls
      !>           Source      : getfat_ex1.tm
      !>           Architecture: KPL
      !>           Kernel type : LSK
      !>
      !>         File name     : mgs_moc_v20.ti
      !>           Source      : getfat_ex1.tm
      !>           Architecture: KPL
      !>           Kernel type : IK
      !>
      !>         File name     : mgs_sclkscet_00061.tsc
      !>           Source      : getfat_ex1.tm
      !>           Architecture: KPL
      !>           Kernel type : SCLK
      !>
      !>         File name     : mgs_sc_ext12.bc
      !>           Source      : getfat_ex1.tm
      !>           Architecture: DAF
      !>           Kernel type : CK
      !>
      !>         File name     : mgs_ext12_ipng_mgs95j.bsp
      !>           Source      : getfat_ex1.tm
      !>           Architecture: DAF
      !>           Kernel type : SPK
      !>
      !>         File name     : megr90n000cb_plate.bds
      !>           Source      : getfat_ex1.tm
      !>           Architecture: DAS
      !>           Kernel type : DSK
      !>
      !>## Restrictions
      !>
      !>     1)  In order to properly determine the type of DAF based binary
      !>         kernels, the routine requires that their first segments and
      !>         the meta data necessary to address them are valid.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.1.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 5.0.1, 07-AUG-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example to $Examples section.
      !>
      !>-    SPICELIB Version 5.0.0, 05-FEB-2015 (NJB)
      !>
      !>        Updated to support integration of DAS into the
      !>        handle manager subsystem. Now opened DAS files
      !>        must be known to that subsystem; if this routine
      !>        encounters an open, unrecognized DAS file, an
      !>        error is signaled.
      !>
      !>        Corrected various typos in comments.
      !>
      !>-    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 4.21.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 4.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 4.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 4.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 4.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 4.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 4.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 4.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 4.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 4.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 4.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 4.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 4.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 4.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 4.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 4.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 4.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 4.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 4.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 4.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 4.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 4.0.2, 24-APR-2003 (EDW)
      !>
      !>        Added MAC-OSX-F77 to the list of platforms
      !>        that require READONLY to read write protected
      !>        kernels.
      !>
      !>-    SPICELIB Version 4.0.1, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW)
      !>
      !>        Added code so that the architecture and type of open binary
      !>        SPICE kernels can be determined.
      !>
      !>        Added exception for MACPPC_C (CodeWarrior Mac classic).
      !>        Reduced RECL value to 12 to prevent expression of
      !>        the fseek bug.
      !>
      !>-    SPICELIB Version 3.2.0, 06-DEC-1999 (WLT)
      !>
      !>        The heuristics for distinguishing between CK and SPK have
      !>        been enhanced so that the routine is no longer requires
      !>        that TICKS in C-kernels be positive or integral.
      !>
      !>-    SPICELIB Version 3.1.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 3.1.3, 22-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 3.1.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 3.1.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 3.1.0, 11-FEB-1999 (FST)
      !>
      !>        Added an integrality check to Test 3. If LASTDP is not
      !>        an integral value, then GETFAT simply returns KERTYP = '?',
      !>        since it is of an indeterminate type.
      !>
      !>-    SPICELIB Version 3.0.0, 07-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 2.0.0, 19-DEC-1995 (KRG)
      !>
      !>        Added several new features to the subroutine:
      !>
      !>         - Error handling has been enhanced.
      !>         - Several new file architectures have been added.
      !>
      !>        Removed the mention of 1000 characters as a candidate for the
      !>        record length of a file.
      !>
      !>        Added the exception for a blank filename to the header. The
      !>        error is signaled, but it was not listed in the header.
      !>
      !>        Added IOSTAT values to the appropriate error messages.
      !>
      !>        Non-printing characters are replaced with blanks in the ID
      !>        word when it is read. This deals with the case where a
      !>        platform allows a text file to be opened as an unformatted
      !>        file and the ID word does not completely fill 8 characters.
      !>
      !>-    SPICELIB Version 1.4.0, 05-JAN-1995 (HAN)
      !>
      !>        Removed ENV11 since it is now the same as ENV2.
      !>        Removed ENV10 since it is the same as the VAX environment.
      !>
      !>-    SPICELIB Version 1.3.0, 30-AUG-1994 (HAN)
      !>
      !>        Added two new environments, DEC Alpha/OpenVMS and
      !>        Sun/Solaris, to the source master file.
      !>
      !>-    SPICELIB Version 1.2.0, 25-MAR-1994 (HAN)
      !>
      !>        Added two new environments, DEC Alpha/OpenVMS and
      !>        Sun/Solaris, to the source master file.
      !>
      !>-    SPICELIB Version 1.1.0, 25-MAR-1994 (HAN)
      !>
      !>        Modified master source code file to use READONLY on platforms
      !>        that support it. Also, changed some local declaration comment
      !>        lines to match the standard NAIF template.
      !>
      !>-    SPICELIB Version 1.0.0, 24-JUL-1993 (WLT) (HAN) (KRG)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW)
      !>
      !>        Added code so that the architecture and type of open binary
      !>        SPICE kernels can be determined. This uses the new DAF/DAS
      !>        handle manager as well as examination of handles of open DAS
      !>        files. Currently the handle manager deals only with DAF
      !>        files. This routine should be updated again when the DAS
      !>        system is integrated with the handle manager.
      !>
      !>        Some slight changes were required to support ZZDDHFNH on
      !>        the VAX environment. This resulted in the addition of
      !>        the logical USEFNH that is set to true in most
      !>        environments, and never used again other than to allow
      !>        the invocation of the ZZDDHFNH module.
      !>
      !>-    SPICELIB Version 2.0.0, 19-DEC-1995 (KRG)
      !>
      !>        Added several new features to the subroutine:
      !>
      !>         - Error handling has been enhanced.
      !>         - Several new file architectures have been added.
      !>
      !>        Removed the mention of 1000 characters as a candidate for the
      !>        record length of a file. It seems unlikely that we will
      !>        encounter an environment where 1000 characters of storage is
      !>        larger than the storage necessary for 128 double precision
      !>        numbers; typically there are 8 characters per double precision
      !>        number, yielding 1024 characters.
      !>
      !>        Added the exception for a blank filename to the header. The
      !>        error is signaled, but it was not listed in the header.
      !>
      !>        Added IOSTAT values to the appropriate error messages.
      !>
      !>        Non-printing characters are replaced with blanks in the ID
      !>        word when it is read. This deals with the case where a
      !>        platform allows a text file to be opened as an unformatted
      !>        file and the ID word does not completely fill 8 characters.
      module subroutine getfat ( ctx, file, arch, kertyp )
      use pepper_kinds, only: ip
      use pepper_strings, only: nextwd_inplace, ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! The name of a file to be examined.
      character(*), intent(out) :: arch  !! The architecture of the kernel file.
      character(*), intent(out) :: kertyp  !! The type of the kernel file.


!
!     Set the length of a SPICE kernel file ID word.
!
      integer(ip), parameter :: idlen = 12

      integer(ip), parameter :: filsiz = 255

      integer(ip), parameter :: wdsize = 32

!
!     Set minimum and maximum values for the range of ASCII printing
!     characters.
!
      integer(ip), parameter :: minpch = 32

      integer(ip), parameter :: maxpch = 126

      character(wdsize) :: filarc
      character(filsiz) :: fname
      character(idlen) :: idword
      character(idlen) :: tmpwrd

      integer(ip) :: handle
      integer(ip) :: i
      integer(ip) :: intamn
      integer(ip) :: intarc
      integer(ip) :: intbff
      integer(ip) :: iostat
      integer(ip) :: number

      logical :: diropn
      logical :: exist
      logical :: found
      logical :: kept
      logical :: opened

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialize the temporary storage variables that we use.
!
      idword = ' '

!
!     If the filename we have is blank, signal an error and return.
!
      if ( file == ' ' ) then

         call ctx%setmsg ( 'The file name is blank.' )
         call ctx%sigerr ( 'SPICE(BLANKFILENAME)'    )
         return

      end if

!
!     PEPPER: a kernel built in memory (it has no file) is a DAF, of the
!     type its ID word names.
!
      i = synthetic_named ( ctx, file )
      if ( i > 0 ) then
         associate ( rec => daf_image_chars ( ctx%spice%zzddhman%files(i)%image, 1_ip ) )
            arch   = 'DAF'
            kertyp = rec(5:8)
         end associate
         return
      end if

!
!     See if this is a binary file that is currently open
!     within the SPICE binary file management subsystem.  At
!     the moment, as far as we know, the file is not opened.
!
      opened = .false.
      kept   = .false.

      call zzddhfnh ( ctx, file, handle, found )

      if ( found ) then

         i = daf_raw_slot ( ctx, handle )
         if ( i > 0 ) then
            kept   = .true.
            tmpwrd = ctx%spice%zzddhman%files(i)%raw_records(1)(1:idlen)
         else
!
!        If the file was recognized, we need to get the unit number
!        associated with it.
!
            call zzddhnfo ( ctx, handle, fname, intarc, intbff, intamn, found )

!
!        Translate the architecture ID to a string and retrieve the
!        logical unit to use with this file.
!
            call zzddhgsd ( ctx, 'ARCH', intarc, filarc )
            call zzddhhlu ( ctx, handle, filarc, .false., number )

            opened = .true.
         end if

      else

         if ( .not. ctx%spice%zzddhman%files_only ) then
            do i = 1, ctx%spice%keeper%loaded
               if ( allocated ( ctx%spice%keeper%kernels(i)%memory_text ) ) then
                  if ( ctx%spice%keeper%kernels(i)%name == trim ( adjustl ( file ) ) ) then
                     kept   = .true.
                     tmpwrd = ' '
                     associate ( mtxt => ctx%spice%keeper%kernels(i)%memory_text )
                        number = min ( idlen, len ( mtxt ) )
                        do intamn = 1, number
                           if ( mtxt(intamn:intamn) == achar(10) .or. &
                                mtxt(intamn:intamn) == achar(13) ) exit
                           tmpwrd(intamn:intamn) = mtxt(intamn:intamn)
                        end do
                     end associate
                     exit
                  end if
               end if
            end do
         end if

         if ( .not. kept ) then
!
!        We'll do a bit of inquiring before we try opening anything.
!
            inquire ( file   = file, &
                      exist  = exist, &
                      iostat = iostat, &
                      opened = opened )

!
!        Not too likely, but if the INQUIRE statement fails...
!
            if ( iostat /= 0 ) then
               call ctx%setmsg ( 'IOSTAT error in INQUIRE statement. IOSTAT ' &
               //              '= #.'                                    )
               call ctx%errint ( '#', iostat                                 )
               call ctx%sigerr ( 'SPICE(INQUIREERROR)'                       )
               return
            end if

!
!        Note: the following two tests MUST be performed in the order
!        in which they appear, since in some environments files that do
!        not exist are considered to be open.
!
            if ( .not. exist ) then

               call ctx%setmsg ( 'The kernel file ''#'' does not exist.'   )
               call ctx%errch  ( '#', file                                 )
               call ctx%sigerr ( 'SPICE(FILENOTFOUND)'                     )
               return

            end if

!
!        Reject open files not known to the handle manager subsystem.
!
!        PEPPER: a unit of PEPPER may be connected to the file: another
!        context's, still open on a file closed through the instance (it
!        is closed when the context next uses its unit table), or another
!        instance's. That is not an external open: the file is opened
!        again below, on a unit of its own (see opened_by_pepper).
!        (Loading a file again after UNLOAD or KCLEAR, while a context that
!        had read it was not used since, failed; so did loading a file
!        another instance had loaded.)
!
            if ( opened ) then
               if ( opened_by_pepper ( ctx%spice, file ) ) opened = .false.
            end if

            if ( opened ) then
!
!           Open files that are not opened within the SPICE
!           binary file management subsystem are forbidden fruit.
!           All we can do is signal an error letting the caller
!           know that we are helpless in this case.
!
               call ctx%setmsg ( 'The file ''#'' is already open.' )
               call ctx%errch  ( '#', file                         )
               call ctx%sigerr ( 'SPICE(EXTERNALOPEN)'             )
               return

            end if
         end if

      end if

!
!     PEPPER: during a load, ZZDDHF2H (through ZZDDHFNH above) has
!     opened the file and read the start of its first record (direct
!     access): use it instead of opening the file again (see
!     zzddhman_state).
!
      if ( .not. kept ) then
         if ( .not. opened ) then
            if ( ctx%spice%zzddhman%kept_f2h ( file ) ) then
               kept = ctx%spice%zzddhman%f2h_word_ok
               if ( kept ) tmpwrd = ctx%spice%zzddhman%f2h_word
            end if
         end if
      end if

      if ( kept ) then
         continue
      else
!
!     Open the file with a record length of RECL (the length of the
!     DAF and DAS records). We assume, for now, that opening the file as
!     a direct access file will work.
!
      diropn = .true.

!
!     If the file is not already open (probably the case that
!     happens most frequently) we try opening it for direct access
!     and see if we can locate the idword.
!
      if ( .not. opened ) then

!     PEPPER: the unit comes from NEWUNIT= in the OPEN below.

         open ( newunit               = number, &
                file               = file, &
                access             = 'DIRECT', &
                recl               = record_length(), &
                status             = 'OLD', &
                iostat             = iostat    )

!
!     If we had trouble opening the file, try opening it as a
!     sequential file.
!
         if ( iostat /= 0 ) then

            diropn = .false.

            open ( unit            = number, &
                   file            = file, &
                   access          = 'SEQUENTIAL', &
                   status          = 'OLD', &
                   iostat          = iostat    )

!
!        If we still have problems opening the file, we don't have a
!        clue about the file architecture and type.
!
            if ( iostat /= 0 ) then

               arch   = '?'
               kertyp = '?'
               call ctx%setmsg ( 'Attempt to open the file ''#'' failed.' &
               //           ' IOSTAT = #.'                          )
               call ctx%errch  ( '#', file                              )
               call ctx%errint ( '#', iostat                            )
               call ctx%sigerr ( 'SPICE(FILEOPENFAILED)'                )
               return

            end if

         end if
      end if

!
!     We opened the file successfully, so let's try to read from the
!     file. We need to be sure to use the correct form of the read
!     statement, depending on whether the file was opened with direct
!     access or sequential access.
!
      if ( diropn ) then

         read ( number, rec=1, iostat=iostat ) tmpwrd

!
!        If we couldn't read from the file as a direct access file with
!        a fixed record length, then try to open the file as a
!        sequential file and read from it.
!
         if ( iostat /= 0 ) then

            if ( opened ) then

!
!              Something has gone wrong here.  The file was opened
!              as either a DAF or DAS prior to the call to GETFAT.
!              We retrieved the unit number maintained by the
!              underlying binary file management system, but we
!              were unable to read the file as direct access.
!              There's nothing we can do but abandon our quest to
!              determine the type of the file.
!
               call ctx%setmsg ( 'The file ''#'' is opened as a binary ' &
               //            'SPICE kernel.  But it cannot be ' &
               //            'read using a direct access read. ' &
               //            'The value of IOSTAT returned by the ' &
               //            'attempted READ is #. '                 )
               call ctx%errch  ( '#', file                               )
               call ctx%errint ( '#', iostat                             )
               call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                 )
               return

            end if

!
!           If we reach this point, the file was opened locally
!           as a direct access file.  We could not read it that
!           way, so we'll try using a sequential read.   However,
!           we first need to close the file and then reopen it
!           for sequential reading.
!
            close ( number )

            open ( unit   = number, &
                   file   = file, &
                   access = 'SEQUENTIAL', &
                   status = 'OLD', &
                   iostat = iostat    )

!
!           If we could not open the file, we don't have a clue about
!           the file architecture and type.
!
            if ( iostat /= 0 ) then

               arch   = '?'
               kertyp = '?'
               call ctx%setmsg ( 'Attempt to open the file ''#''' &
               //            ' failed. IOSTAT = #.'           )
               call ctx%errch  ( '#', file                        )
               call ctx%errint ( '#', iostat                      )
               call ctx%sigerr ( 'SPICE(FILEOPENFAILED)'          )
               return

            end if

!
!           Try to read from the file.
!
            read ( number, fmt='(A)', iostat=iostat ) tmpwrd

         end if

      else

         read ( number, fmt='(A)', iostat=iostat ) tmpwrd

      end if

!
!     If we had an error while reading, we don't recognize this file.
!
      if ( iostat /= 0 ) then

         arch   = '?'
         kertyp = '?'
         close       ( number                                      )
         call ctx%setmsg ( 'Attempt to read from file ''#'' failed.'  // &
                       ' IOSTAT = #.'                              )
         call ctx%errch  ( '#', file                                   )
         call ctx%errint ( '#', iostat                                 )
         call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                     )
         return

      end if

!
!     Close the file (if we opened it here), as we do not need it
!     to be open any more.
!
      if ( .not. opened ) then
         close ( number )
      end if

      end if

!
!     At this point, we have a candidate for an ID word. To avoid
!     difficulties with Fortran I/O and other things, we will now
!     replace any non printing ASCII characters with blanks.
!
      do i = 1, idlen
         if (      ( ichar(tmpwrd(i:i)) < minpch ) &
              .or. ( ichar(tmpwrd(i:i)) > maxpch ) ) then
            tmpwrd(i:i) = ' '
         end if
      end do

!
!     Identify the architecture and type, if we can.
!
      tmpwrd = adjustl(tmpwrd)
      call ucase_inplace ( tmpwrd )
      call nextwd_inplace ( tmpwrd, idword )

      if ( idword == 'DAFETF' ) then

!
!        We have a DAF encoded transfer file.
!
         arch   = 'XFR'
         kertyp = 'DAF'

      else if ( idword == 'DASETF') then

!
!        We have a DAS encoded transfer file.
!
         arch   = 'XFR'
         kertyp = 'DAS'

      else if ( idword(1:10) == '''NAIF/DAF''' ) then

!
!        We have an old DAF decimal text file.
!
         arch   = 'DEC'
         kertyp = 'DAF'

      else if ( idword(1:8) == 'NAIF/DAS' ) then

!
!        We have a pre release DAS binary file.
!
         arch   = 'DAS'
         kertyp = 'PRE'

      else

!
!        Get the architecture and type from the ID word, if we can.
!
         call idw2at ( ctx, idword(1:8), arch, kertyp )

      end if

!
!     If the architecture is DAF and the type is unknown, '?', then we
!     have either an SPK file, a CK file, or something we don't
!     understand. Let's check it out.
!
      if ( ( arch == 'DAF' ) .and. ( kertyp == '?' ) ) then

!
!        We have a DAF file and we do not know what the type is. This
!        situation can occur for older SPK and CK files, before the ID
!        word was used to store type information.
!
!        We use Bill's (WLT'S) magic heuristics to determine the type
!        of the file.
!
!        Open the file and pass the handle to the private routine
!        that deals with the dirty work.
!
         call dafopr ( ctx, file, handle )
         call zzckspk( ctx, handle, kertyp )
         call dafcls ( ctx, handle )

      end if

      return

      end subroutine getfat

      !> Compute the outward-pointing, unit normal vector at a point on
      !> the surface of an ellipsoid.
      !>
      !>## Detailed Input
      !>
      !>     A        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the X-axis of the body-fixed reference frame.
      !>
      !>     B        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the Y-axis of the body-fixed reference frame.
      !>
      !>     C        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the Z-axis of the body-fixed reference frame.
      !>
      !>     POINT    is a 3-vector giving the bodyfixed coordinates of a point
      !>              on the ellipsoid. In bodyfixed coordinates, the semi-axes
      !>              of the ellipsoid are aligned with the X, Y, and Z-axes of
      !>              the reference frame.
      !>
      !>## Detailed Output
      !>
      !>     NORMAL   is the unit vector pointing away from the ellipsoid and
      !>              normal to the ellipsoid at POINT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the axes are non-positive, the error
      !>         SPICE(BADAXISLENGTH) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the outward pointing unit normal vector to
      !>     the ellipsoid having semi-axes of length A, B, and C from the
      !>     point POINT.
      !>
      !>## Examples
      !>
      !>     A typical use of SURFNM would be to find the angle of incidence
      !>     of the light from the sun at a point on the surface of an
      !>     ellipsoid.
      !>
      !>     Let Q be a 3-vector representing the rectangular body-fixed
      !>     coordinates of a point on the ellipsoid (we are assuming that
      !>     the axes of the ellipsoid are aligned with the axes of the
      !>     body fixed frame.) Let V be the vector from Q to the sun in
      !>     bodyfixed coordinates. Then the following code fragment could
      !>     be used to compute angle of incidence of sunlight at Q.
      !>
      !>        CALL SURFNM   ( A, B, C, Q, NRML )
      !>
      !>        INCIDN = VSEP ( V,          NRML )
      !>
      !>## Restrictions
      !>
      !>     1)  It is assumed that the input POINT is indeed on the ellipsoid.
      !>         No checking for this is done.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Updated the documentation to refer to "reference frame" instead
      !>        of "coordinate system" as per NAIF conventions.
      !>
      !>-    SPICELIB Version 1.3.2, 23-FEB-2016 (NJB)
      !>
      !>        Corrected some typos in the header.
      !>
      !>-    SPICELIB Version 1.3.1, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.3.0, 02-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VHAT call.
      !>
      !>-    SPICELIB Version 1.2.0, 07-AUG-1996 (WLT)
      !>
      !>        Added a SAVE statement so that the error message will
      !>        not be lost between separate invocations of the routine.
      !>
      !>-    SPICELIB Version 1.1.0, 21-JUL-1995 (WLT)
      !>
      !>        A typo in the $Examples section was corrected
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      module subroutine surfnm ( ctx, a, b, c, point, normal )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: vhatip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: a  !! Length of the ellipsoid semi-axis along the X-axis.
      real(wp), intent(in) :: b  !! Length of the ellipsoid semi-axis along the Y-axis.
      real(wp), intent(in) :: c  !! Length of the ellipsoid semi-axis along the Z-axis.
      real(wp), intent(in) :: point  ( 3 )  !! Body-fixed coordinates of a point on the
                                            !! ellipsoid.
      real(wp), intent(out) :: normal ( 3 )  !! Outward pointing unit normal to ellipsoid at
                                             !! POINT.

      real(wp) :: m
      real(wp) :: a1
      real(wp) :: b1
      real(wp) :: c1

      character(32), parameter :: mssg ( 7 ) = &
         [ character(32) :: 'Axis A was nonpositive.', 'Axis B was nonpositive.', &
           'Axes A and B were nonpositive.', 'Axis C was nonpositive.', &
           'Axes A and C were nonpositive.', &
           'Axes B and C were nonpositive.', &
           'All three axes were nonpositive.' ]
      integer(ip) :: bad

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check the axes to make sure that none of them is less than or
!     equal to zero. If one is, signal an error and return.
!
      bad = 0

      if ( a <= 0 ) &
         bad = bad + 1

      if ( b <= 0 ) &
         bad = bad + 2

      if ( c <= 0 ) &
         bad = bad + 4

      if ( bad > 0 ) then
         call ctx%setmsg ( mssg (bad)                                     // &
                       ' ? '                                           )
         call ctx%errch  ( ' ? ', 'The A,B, and C axes were '             // &
                              '#, #, and # respectively.'              )

         call ctx%errdp  ( '#', a )
         call ctx%errdp  ( '#', b )
         call ctx%errdp  ( '#', c )

         call ctx%sigerr ( 'SPICE(BADAXISLENGTH)' )
         return
      end if

!
!     Mathematically we want to compute (Px/a**2, Py/b**2, Pz/c**2)
!     and then convert this to a unit vector. However, computationally
!     this can blow up in our faces.  But note that only the ratios
!     a/b, b/c and a/c are important in computing the unit normal.
!     We can use the trick below to avoid the unpleasantness of
!     multiplication and division overflows.
!
      m         = min ( a, b, c )

!
!     M can be divided by A,B or C without fear of an overflow
!     occurring.
!
      a1        = m/a
      b1        = m/b
      c1        = m/c

!
!     All of the terms A1,B1,C1 are less than 1. Thus no overflows
!     can occur.
!
      normal(1) = point(1) * (a1*a1)
      normal(2) = point(2) * (b1*b1)
      normal(3) = point(3) * (c1*c1)

      call vhatip ( normal )

      return
      end subroutine surfnm

      !> Locate the point on the surface of an ellipsoid that is nearest
      !> to a specified position. Also return the altitude of the position
      !> above the ellipsoid.
      !>
      !>## Detailed Input
      !>
      !>     POSITN   is a 3-vector giving the position of a point with respect
      !>              to the center of an ellipsoid. The vector is expressed
      !>              in a body-fixed reference frame. The semi-axes of the
      !>              ellipsoid are aligned with the x, y, and z-axes of the
      !>              body-fixed frame.
      !>
      !>     A        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the x-axis of the body-fixed reference frame.
      !>
      !>     B        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the y-axis of the body-fixed reference frame.
      !>
      !>     C        is the length of the semi-axis of the ellipsoid that is
      !>              parallel to the z-axis of the body-fixed reference frame.
      !>
      !>## Detailed Output
      !>
      !>     NPOINT   is the nearest point on the ellipsoid to POSITN.
      !>              NPOINT is a 3-vector expressed in the body-fixed
      !>              reference frame.
      !>
      !>     ALT      is the altitude of POSITN above the ellipsoid. If
      !>              POSITN is inside the ellipsoid, ALT will be negative
      !>              and have magnitude equal to the distance between
      !>              NPOINT and POSITN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the axis lengths A, B or C are non-positive, the
      !>         error SPICE(BADAXISLENGTH) is signaled.
      !>
      !>     2)  If the ratio of the longest to the shortest ellipsoid axis
      !>         is large enough so that arithmetic expressions involving its
      !>         squared value may overflow, the error SPICE(BADAXISLENGTH)
      !>         is signaled.
      !>
      !>     3)  If any of the expressions
      !>
      !>            A * ABS( POSITN(1) ) / m**2
      !>            B * ABS( POSITN(2) ) / m**2
      !>            C * ABS( POSITN(3) ) / m**2
      !>
      !>         where `m' is the minimum of { A, B, C }, is large enough so
      !>         that arithmetic expressions involving these sub-expressions
      !>         may overflow, the error SPICE(INPUTSTOOLARGE) is signaled.
      !>
      !>     4)  If the axes of the ellipsoid have radically different
      !>         magnitudes, for example if the ratios of the axis lengths vary
      !>         by 10 orders of magnitude, the results may have poor
      !>         precision. No error checks are done to identify this problem.
      !>
      !>     5)  If the axes of the ellipsoid and the input point POSITN have
      !>         radically different magnitudes, for example if the ratio of
      !>         the magnitude of POSITN to the length of the shortest axis is
      !>         1.E25, the results may have poor precision. No error checks
      !>         are done to identify this problem.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Many applications of this routine are more easily performed
      !>     using the higher-level SPICELIB routine SUBPNT. This routine
      !>     is the mathematical workhorse on which SUBPNT relies.
      !>
      !>     This routine solves for the location, N, on the surface of an
      !>     ellipsoid nearest to an arbitrary location, P, relative to that
      !>     ellipsoid.
      !>
      !>## Examples
      !>
      !>     Example 1.
      !>
      !>     The code fragment below illustrates how you can use SPICELIB to
      !>     compute the apparent sub-earth point on the moon.
      !>
      !>     C
      !>     C     Load the ephemeris, leapseconds and physical constants
      !>     C     files first. We assume the names of these files are
      !>     C     stored in the character variables SPK, LSK and
      !>     C     PCK.
      !>     C
      !>           CALL FURNSH ( SPK )
      !>           CALL FURNSH ( LSK )
      !>           CALL FURNSH ( PCK )
      !>
      !>     C
      !>     C     Get the apparent position of the moon as seen from the
      !>     C     earth. Look up this position vector in the moon
      !>     C     body-fixed frame IAU_MOON. The orientation of the
      !>     C     IAU_MOON frame will be computed at epoch ET-LT.
      !>     C
      !>           CALL SPKPOS ( 'moon',  ET,    'IAU_MOON', 'LT+S',
      !>          .              'earth', TRGPOS, LT                 )
      !>
      !>     C
      !>     C     Negate the moon's apparent position to obtain the
      !>     C     position of the earth in the moon's body-fixed frame.
      !>     C
      !>           CALL VMINUS ( TRGPOS, EVEC )
      !>
      !>     C
      !>     C     Get the lengths of the principal axes of the moon.
      !>     C     Transfer the elements of the array RADII to the
      !>     C     variables A, B, C to enhance readability.
      !>     C
      !>           CALL BODVRD (  'MOON',    'RADII', DIM, RADII )
      !>           CALL VUPACK (  RADII,  A,       B,   C     )
      !>
      !>     C
      !>     C     Finally get the point SUBPNT on the surface of the
      !>     C     moon closest to the earth --- the sub-earth point.
      !>     C     SUBPNT is expressed in the IAU_MOON reference frame.
      !>     C
      !>           CALL NEARPT ( EVEC, A, B, C, SUBPNT, ALT )
      !>
      !>     Example 2.
      !>
      !>     One can use this routine to define a generalization of GEODETIC
      !>     coordinates called GAUSSIAN coordinates of a triaxial body. (The
      !>     name is derived from the famous Gauss-map of classical
      !>     differential geometry).  The coordinates are longitude,
      !>     latitude, and altitude.
      !>
      !>     We let the x-axis of the body fixed coordinate system point
      !>     along the longest axis of the triaxial body. The y-axis points
      !>     along the middle axis and the z-axis points along the shortest
      !>     axis.
      !>
      !>     Given a point P, there is a point on the ellipsoid that is
      !>     closest to P, call it Q. The latitude and longitude of P
      !>     are determined by constructing the outward pointing unit normal
      !>     to the ellipsoid at Q. Latitude of P is the latitude that the
      !>     normal points toward in the body-fixed frame. Longitude is
      !>     the longitude the normal points to in the body-fixed frame.
      !>     The altitude is the signed distance from P to Q, positive if P
      !>     is outside the ellipsoid, negative if P is inside.
      !>     (the mapping of the point Q to the unit normal at Q is the
      !>     Gauss-map of Q).
      !>
      !>     To obtain the Gaussian coordinates of a point whose position
      !>     in body-fixed rectangular coordinates is given by a vector P,
      !>     the code fragment below will suffice.
      !>
      !>        CALL NEARPT ( P,    A, B, C, Q, ALT  )
      !>        CALL SURFNM (       A, B, C  Q, NRML )
      !>
      !>        CALL RECLAT ( NRML, R, LONG, LAT     )
      !>
      !>     The Gaussian coordinates are LONG, LAT, and ALT.
      !>
      !>## Restrictions
      !>
      !>     See $Exceptions section.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 26-OCT-2021 (NJB) (JDR) (EDW)
      !>
      !>        Edit to logic to reduce unneeded operations when
      !>        error or projection vectors equal zero. Addition
      !>        of details concerning the "ellipsoid near point"
      !>        problem and solution.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.4.0, 27-JUN-2013 (NJB)
      !>
      !>        Updated in-line comments.
      !>
      !>     Last update was 04-MAR-2013 (NJB)
      !>
      !>        Bug fix: now correctly computes off-axis solution for
      !>        the case of a prolate ellipsoid and a viewing point
      !>        on the interior long axis.
      !>
      !>-    SPICELIB Version 1.3.1, 07-FEB-2008 (NJB)
      !>
      !>        Header update: header now refers to SUBPNT rather
      !>        than deprecated routine SUBPT.
      !>
      !>-    SPICELIB Version 1.3.0, 07-AUG-2006 (NJB)
      !>
      !>        Bug fix: added initialization of variable SNGLPT to support
      !>                  operation under the Macintosh Intel Fortran
      !>                  compiler. Note that this bug did not affect
      !>                  operation of this routine on other platforms.
      !>
      !>-    SPICELIB Version 1.2.0, 15-NOV-2005 (EDW) (NJB)
      !>
      !>        Various changes were made to ensure that all loops terminate.
      !>
      !>        Bug fix: scale of transverse component of error vector
      !>        was corrected for the exterior point case.
      !>
      !>        Bug fix: non-standard use of duplicate arguments in VSCL
      !>        calls was corrected.
      !>
      !>        Error checking was added to screen out inputs that might
      !>        cause numeric overflow.
      !>
      !>        Replaced BODVAR call in examples to BODVRD.
      !>
      !>-    SPICELIB Version 1.1.1, 28-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header corrections were made.
      !>
      !>-    SPICELIB Version 1.1.0, 27-NOV-1990 (WLT)
      !>
      !>        The routine was substantially rewritten to achieve
      !>        more robust behavior and document the mathematics
      !>        of the routine.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 27-NOV-1990 (NJB)
      !>
      !>        Bug fix: added initialization of variable SNGLPT to support
      !>                  operation under the Macintosh Intel Fortran
      !>                  compiler. Note that this bug did not affect
      !>                  operation of this routine on other platforms. The
      !>                  statement referencing the uninitialized variable
      !>                  was:
      !>
      !>           IF ( INSIDE  .AND. (      SNGLPT .EQ. 2
      !>     .                          .OR. SNGLPT .EQ. 3 ) ) THEN
      !>
      !>        SNGLPT is uninitialized only if INSIDE is .FALSE.,
      !>        so the  value of the logical expression is not affected by
      !>        the uninitialized value of SNGLPT.
      !>
      !>        However, the Intel Fortran compiler for the Mac flags a runtime
      !>        error when the above code is exercised. So SNGLPT is now
      !>        initialized prior to the above IF statement.
      !>
      !>-    SPICELIB Version 1.2.0, 15-NOV-2005 (EDW) (NJB)
      !>
      !>        Bug fix: scale of transverse component of error vector
      !>        was corrected for the exterior point case.
      !>        Replaced BODVAR call in examples to BODVRD.
      !>
      !>        Bug fix: non-standard use of duplicate arguments in VSCL
      !>        calls was corrected.
      !>
      !>        Various changes were made to ensure that all loops terminate.
      !>
      !>        Error checking was added to screen out inputs that might
      !>        cause numeric overflow.
      !>
      !>        Removed secant solution branch from root-finding loop.
      !>        Although the secant solution sped up some root searches,
      !>        it caused large numbers of unnecessary iterations in others.
      !>
      !>        Changed the expression:
      !>
      !>           IF (       LAMBDA .EQ. LOWER
      !>     .          .OR.  LAMBDA .EQ. UPPER ) THEN
      !>
      !>        to
      !>
      !>           IF (      APPROX( LAMBDA, LOWER, CNVTOL )
      !>     .          .OR. APPROX( LAMBDA, UPPER, CNVTOL )  ) THEN
      !>
      !>        Use of APPROX eliminates the possibility of an infinite loop
      !>        when LAMBDA approaches to within epsilon of, but does not
      !>        equate to UPPER or LOWER. Infinite loops occurred under some
      !>        compiler's optimizations.
      !>
      !>        The loop also includes a check on number of iterations,
      !>        signaling an error if the bisection loop uses more than
      !>        MAXITR passes.
      !>
      !>        TOUCHD is now used to defeat extended-register usage in
      !>        cases where such usage may cause logic problems.
      !>
      !>        Some minor code changes were made to ensure that various
      !>        variables remain in their expected ranges.
      !>
      !>        A few code changes were made to enhance clarity.
      !>
      !>-    SPICELIB Version 1.1.0, 27-NOV-1990 (WLT)
      !>
      !>        The routine was nearly rewritten so that points
      !>        near the coordinate planes in the interior of the ellipsoid
      !>        could be handled without fear of floating point overflow
      !>        or divide by zero.
      !>
      !>        While the mathematical ideas involved in the original routine
      !>        are retained, the code is for the most part new. In addition,
      !>        the new code has been documented far more extensively than was
      !>        NEARPT 1.0.0.
      !>
      !>-    Beta Version 2.0.0, 9-JAN-1989 (WLT)
      !>
      !>        Error handling added has been added for bad axes values.
      !>
      !>        The algorithm did not work correctly for some points inside
      !>        the ellipsoid lying on the plane orthogonal to the shortest
      !>        axis of the ellipsoid. The problem was corrected.
      !>
      !>        Finally the algorithm was made slightly more robust and clearer
      !>        by use of SPICELIB routines and by normalizing the inputs.
      !>
      !>        Add an example to the header section.
      module subroutine nearpt ( ctx, positn, a, b, c, npoint, alt )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_arrays, only: orderd, reordd
      use pepper_math, only: approx, vdist, vnorm, vperp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: positn ( 3 )  !! Position of a point in body-fixed frame.
      real(wp), intent(in) :: a  !! Length of semi-axis parallel to x-axis.
      real(wp), intent(in) :: b  !! Length of semi-axis parallel to y-axis.
      real(wp), intent(in) :: c  !! Length on semi-axis parallel to z-axis.
      real(wp), intent(out) :: npoint ( 3 )  !! Point on the ellipsoid closest to POSITN.
      real(wp), intent(out) :: alt  !! Altitude of POSITN above the ellipsoid.

!
!     Parameters
!
!     The convergence tolerance CNVTOL is used to terminate the
!     bisection loop when the solution interval is very small but
!     hasn't converged to length zero.  This situation can occur when
!     the root is extremely close to zero.
!
      real(wp), parameter :: cnvtol = 1.0e-16_wp

!
!     Various potentially large numbers we'll compute must not
!     exceed DPMAX()/MARGIN:
!
      real(wp), parameter :: margin = 1.0e2_wp

!
!     The parameter MAXSOL determines the maximum number of
!     iterations that will be performed in locating the
!     near point.  This must be at least 3.  To get strong
!     robustness in the routine, MAXSOL should be at least 4.
!
      integer(ip), parameter :: maxsol = 6

!
!     MAXITR defines the maximum number of iterations allowed in
!     the bisection loop used to find LAMBDA.  If this loop requires
!     more than MAXITR iterations to achieve convergence, NEARPT
!     will signal an error.
!
!     On a PC/Linux/g77 platform, it has been observed that each
!     bisection loop normally completes in fewer than 70 iterations.
!     MAXITR is used as a "backstop" to prevent infinite looping in
!     case the normal loop termination conditions don't take effect.
!     The value selected is based on the range of exponents for IEEE
!     double precision floating point numbers.
!
      integer(ip), parameter :: maxitr = 2048

!
!     Length of lines in message buffer.
!
      integer(ip), parameter :: msglen = 80

      character(msglen), parameter :: mssg   ( 7 ) = &
         [ character(msglen) :: 'Axis A was nonpositive. ?', 'Axis B was nonpositive. ?', &
           'Axes A and B were nonpositive. ?', &
           'Axis C was nonpositive. ?', &
           'Axes A and C were nonpositive. ?', &
           'Axes B and C were nonpositive. ?', &
           'All three axes were nonpositive. ?' ]

      real(wp) :: axis   ( 3 )
      real(wp) :: axisqr ( 3 )
      real(wp) :: bestht
      real(wp) :: bestpt ( 3 )
      real(wp) :: copy   ( 3 )
      real(wp) :: denom
      real(wp) :: denom2
      real(wp) :: denom3
      real(wp) :: epoint ( 3 )
      real(wp) :: err    ( 3 )
      real(wp) :: errp   ( 3 )
      real(wp) :: factor
      real(wp) :: height
      real(wp) :: lambda
      real(wp) :: lower
      real(wp) :: newerr
      real(wp) :: normal ( 3 )
      real(wp) :: olderr
      real(wp) :: orignl ( 3 )
      real(wp) :: pnorm
      real(wp) :: point  ( 3 )
      real(wp) :: prodct
      real(wp) :: q
      real(wp) :: qlower
      real(wp) :: qupper
      real(wp) :: scale
      real(wp) :: sign
      real(wp) :: spoint ( 3 )
      real(wp) :: temp
      real(wp) :: term   ( 3 )
      real(wp) :: toobig
      real(wp) :: tlambd ( 3 )
      real(wp) :: upper

      integer(ip) :: bad
      integer(ip) :: i
      integer(ip) :: iorder ( 3 )
      integer(ip) :: itr
      integer(ip) :: snglpt
      integer(ip) :: solutn

      logical :: extra
      logical :: inside
      logical :: solvng
      logical :: trim

!
!     Here's what you can expect to find in the routine below.
!
!        Chapter 1.  Error and Exception Handling.
!
!        Chapter 2.  Mathematical background for the solution---the
!                    lambda equation.
!
!        Chapter 3.  Initializations for the main processing loop.
!
!        Chapter 4.  Mathematical Solution of the lambda equation.
!
!                    Section 4.1  Avoiding numerical difficulties.
!                    Section 4.2  Bracketing the root of the lambda
!                                 equation.
!                    Section 4.3  Refining the estimate of lambda.
!                    Section 4.4  Handling points on the central plane.
!
!        Chapter 5.  Decisions and initializations for sharpening
!                    the solution.
!
!        Chapter 6.  Clean up.
!

!
!     Chapter 1
!
!     Error and Exception Handling.
!     ================================================================
!     ----------------------------------------------------------------
!
      if ( ctx%failed() ) return

!
!     Check the axes to make sure that none of them is less than or
!     equal to zero. If one is, signal an error and return.
!
      bad = 0

      if ( a <= 0.0_wp ) then
         bad = bad + 1
      end if

      if ( b <= 0.0_wp ) then
         bad = bad + 2
      end if

      if ( c <= 0.0_wp ) then
         bad = bad + 4
      end if

      if ( bad > 0 ) then

         call ctx%setmsg ( mssg (bad)                       )
         call ctx%errch  ( '?', 'The A,B, and C axes were ' // &
                            '#, #, and # respectively.' )

         call ctx%errdp  ( '#', a )
         call ctx%errdp  ( '#', b )
         call ctx%errdp  ( '#', c )

         call ctx%sigerr ( 'SPICE(BADAXISLENGTH)' )
         return

      end if

!
!     Chapter 2
!
!     Mathematical background for the solution---the lambda equation.
!     ================================================================
!     ----------------------------------------------------------------
!
!     You can describe this problem in two ways, either using a
!     geometric description of the required conditions for the
!     solution, or a functional description based on minimization.
!
!     Geometric description:
!
!     Here is the background and general outline of how this problem is
!     going to be solved.
!
!     We want to find a point on the ellipsoid
!
!           X**2       Y**2       Z**2
!          ------  +  ------  +  ------  =  1                       [1]
!           A**2       B**2       C**2
!
!     that is closest to the input point POSITN.
!
!         If one cares about the gory details, we know that such a
!         point must exist because the ellipsoid is a compact subset of
!         Euclidean 3-space and the distance function between the input
!         point and the ellipsoid is continuous. Since any continuous
!         function on a compact set actually achieves its minimum at
!         some point of the compact set, we are guaranteed that a
!         closest point exists. The closest point may not be unique if
!         the input point is inside the ellipsoid.
!
!     If we let NPOINT be a closest point to POSITN, then the line
!     segment joining POSITN to NPOINT is parallel to the normal to the
!     ellipsoid at NPOINT.  Moreover, suppose we let SEGMENT(P) be the
!     line segment that connects an arbitrary point P with POSITN.  It
!     can be shown that there is only one point P on the ellipsoid in
!     the same octant at POSITN such that the normal at P is parallel
!     to SEGMENT(P).
!
!         More gory details: A normal to a point (X,Y,Z)
!         on the ellipsoid is given by
!
!               X      Y      Z
!           ( -----, -----, ----- )                                 [2]
!              A**2   B**2   C**2
!
!         Given a fixed lambda, and allowing (X,Y,Z) to
!         range over all points on the ellipsoid, the set
!         of points
!
!                 lambda*X      lambda*Y      lambda*Z
!           ( X + --------, Y + --------, Z + -------- )            [3]
!                   A**2          B**2          C**2
!
!         describes another ellipsoid with axes having lengths
!
!                  lambda         lambda        lambda
!              A + ------ ,   B + ------ ,  C + ------  .           [4]
!                    A              B             C
!
!         Moreover, as long as lambda > - MIN( A**2, B**2, C**2 )
!         none of these ellipsoids intersect.  Thus, as long as
!         the normal lines are not allowed to cross the coordinate plane
!         orthogonal to the smallest axis (called the central plane)
!         they do not intersect.
!
!         Finally every point that does not lie on the central plane
!         lies on one of the "lambda" ellipsoids described above.
!
!         Consequently, for each point, P, not on the central plane
!         there is a unique point, NPOINT, on the ellipsoid, such that
!         the normal line at NPOINT also contains P and does not cross
!         the central plane.
!
!     From the above discussion we see that we can mathematically
!     solve the near point problem by finding a point NPOINT
!     on the ellipsoid given by the equation:
!
!           X**2       Y**2       Z**2
!          ------  +  ------  +  ------  =  1                       [5]
!           A**2       B**2       C**2
!
!     such that for some value of lambda
!
!          POSITN = NPOINT + LAMBDA * NORMAL(NPOINT).
!
!     Moreover, if POSITN = (Px,Py,Pz) then lambda must satisfy
!     the equation:
!
!              2                 2                2
!           Px                 Py               Pz
!      -------------   +  -------------  +  ------------  = 1       [6]
!                  2                2               2
!      (A + lambda)       (B + lambda)      (C + lambda)
!           ------             ------            ------
!              A                 B                 C
!
!     and lambda must be greater than -MIN(A**2,B**2,C**2)
!
!     Once lambda is known, NPOINT can be computed from the equation:
!
!          POSITN = NPOINT + LAMBDA*NORMAL(NPOINT).
!
!     The process of solving for lambda can be viewed as selecting
!     that ellipsoid
!
!                2                 2               2
!               x                 y               z
!      --------------- + ---------------- + ---------------  = 1    [7]
!                     2                  2                 2
!       (a + lambda/a)    ( b + lambda/b)    (c + lambda/c)
!
!     that contains the input point POSITN.  For lambda = 0, this
!     ellipsoid is just the input ellipsoid.  When we increase
!     lambda we get a larger "inflated" ellipsoid.  When we
!     decrease lambda we get a smaller "deflated" ellipsoid.  Thus,
!     the search for lambda can be viewed as inflating or deflating
!     the input ellipsoid (in a specially prescribed manner) until
!     the resulting ellipsoid contains the input point POSITN.
!
!     The mathematical solution laid out above, has some numeric
!     flaws.  However, it is robust enough so that if it is applied
!     repeatedly, we can converge to a good solution of the near point
!     problem.
!
!     In the code that follows, we will first solve the lambda equation
!     using the original input point.  However, for points near the
!     central plane the solution we obtain may not lie on the
!     ellipsoid.  But, it should lie along the correct normal line.
!
!     Using this first candidate solution, we find the closest point
!     to it on the ellipsoid.  This second iteration always produces
!     a point that is as close as you can get to the ellipsoid.
!     However, the normal at this second solution may not come as close
!     as desired to pointing toward the input position.  To overcome
!     this deficiency we sharpen the second solution.
!
!     To sharpen a solution we use the computed near point, the
!     computed altitude of POSITN and the normal at the near point to
!     approximate POSITN. The difference between the approximated
!     position of POSITN and the input value of POSITN is called the
!     error vector. To get a sharpened solution we translate the
!     computed near point in the direction of the component of the
!     error vector orthogonal to the normal. We then find the
!     mathematical near point to our translated solution.
!
!     The sharpening process is repeated until it no longer produces
!     an "improved" near point.
!
!     At each step of this procedure, we must compute a solution to
!     the "lambda" equation in order to produce our next estimate of
!     the near point.  If it were possible to create a "private"
!     routine in FORTRAN that only this routine could access, we
!     would do it.  However, things being what they are, we have to
!     compute the lambda solution in a loop.  We keep track of which
!     refinement we are working on by counting the number of
!     lambda solutions that are computed.
!
!     Functional description:
!
!     The problem also defines a minimization condition solvable with
!     the technique of Lagrange Multipliers. In this case,
!     minimize the distance between the ellipsoid surface (set) and the
!     location P subject to the ellipsoid constraint, the solution,
!     NPOINT, an element of the ellipsoid set.
!
!     Define the Lagrange expression, L, as
!
!        L = f - lambda * g                                         [8]
!
!     with f the function to minimize, and g the constraint on f.
!
!     Solve
!
!        D*L = 0                                                    [9]
!
!     with
!
!        g = 0                                                     [10]
!
!     which gives us the expressions
!
!        D*f = lambda * D*g                                        [11]
!        g = 0
!
!     where D = (Dx, Dy, Dz).
!                                     2
!      Choose f(x,y,z) as ||P-NPOINT|| rather than ||P-NPOINT||
!      to simplify the resulting derivatives. Both functions have the
!      same minimum.
!
!        f(x,y,z) = (Px - X)**2 + (Py-Y)**2 + (Pz-Z)**2            [12]
!
!       The constrain equation
!
!                    X**2       Y**2       Z**2
!        g(x,y,z) = ------  +  ------  +  ------  - 1              [13]
!                    A**2       B**2       C**2
!
!      The resulting equations from [11]
!
!         (Px-X) = lambda *   X                                    [14]
!                            ---
!                            A**2
!
!         (Py-Y) = lambda *   Y                                    [15]
!                            ---
!                            B**2
!
!         (Pz-Z) = lambda *   Z                                    [16]
!                            ---
!                            C**2
!
!          X**2       Y**2       Z**2
!          -----   +  -----  +   -----  - 1 = 0                    [17]
!          A**2       B**2       C**2
!
!     Solve [14], [15], [16] for X, Y, Z, then substitute into [5]
!
!              2                 2                2
!           Px                 Py               Pz
!      -------------   +  -------------  +  ------------  - 1 = 0  [18]
!                  2                  2                 2
!      (A + lambda)       (B + lambda)      (C + lambda)
!           ------             ------            ------
!              A                 B                 C
!
!     We see expression [18] identical to expression [6].
!

!
!     Chapter 3
!
!     Initializations for the main processing loop
!     ================================================================
!     ----------------------------------------------------------------
!
!     The solution as implemented uses the geometric description
!     of the problem.
!
!     Let the game begin!
!
!     First order the axes of the ellipsoid and corresponding
!     component of POSITN by the size of lengths of axes.  Knowing
!     which axes are smallest will simplify our task of computing
!     lambda when the time comes.
!
      axis(1)  = a
      axis(2)  = b
      axis(3)  = c

      point = positn

      call orderd ( axis,   3, iorder )

      call reordd ( iorder, 3, axis   )
      call reordd ( iorder, 3, point  )

!
!     Rescale everything so as to avoid underflows when squaring
!     quantities and copy the original starting point.
!
!     Be sure that this is UNDONE at the end of the routine.
!
      scale = 1.0_wp/axis(1)

      axis = scale*axis
      point = scale*point
      orignl = point

!
!     Save the norm of the scaled input point.
!
      pnorm = vnorm ( point )

!
!     The scaled axis lengths must be small enough so they can
!     be squared.
!
      toobig = sqrt ( dpmax / margin )

!
!     Note the first axis has length 1.D0, so we don't check it.
!
      do i = 2, 3

         if ( axis(i) > toobig ) then

            call ctx%setmsg ( 'Ratio of length of axis #* to length of ' // &
                          'axis #* is *; this value may cause '      // &
                          'numeric overflow.'                        )
            call ctx%errint ( '*', iorder(i)                             )
            call ctx%errint ( '*', iorder(1)                             )
            call ctx%errdp  ( '*', axis(i)                               )
            call ctx%sigerr ( 'SPICE(BADAXISLENGTH)'                     )
            return

         end if

      end do

!
!     We also must limit the size of the products
!
!        AXIS(I)*POINT(I), I = 1, 3
!
!     We can safely check these by comparing the products of
!     the square roots of the factors to TOOBIG.
!
      do i = 1, 3

         prodct = sqrt( axis(i) ) * sqrt ( abs(point(i)) )

         if ( prodct > toobig ) then

            call ctx%setmsg ( 'Product of length of scaled axis #* and ' // &
                          'size of corresponding scaled component '  // &
                          'of POSITN is > *; these values '          // &
                          'may cause numeric overflow.'              )
            call ctx%errint ( '*', iorder(i)                             )
            call ctx%errdp  ( '*', toobig**2.0_wp                          )
            call ctx%sigerr ( 'SPICE(INPUTSTOOLARGE)'                    )
            return

         end if

      end do

!
!     Compute the squared lengths of the scaled axes.
!
      axisqr(1) = axis(1)*axis(1)
      axisqr(2) = axis(2)*axis(2)
      axisqr(3) = axis(3)*axis(3)

!
!     We will need to "solve" for the NEARPT at least 3 times.
!     SOLUTN is the counter that keeps track of how many times
!     we have actually solved for a near point.  SOLVNG indicates
!     whether we should continue solving for NEARPT. The logic relies
!     on the initial value of SOLVNG as TRUE, maintaining that
!     value unless explicitly changed.
!
      snglpt =  4
      solutn =  1
      solvng = .true.

      do while ( solvng )

!
!       Chapter 4
!
!       Mathematical solution of the lambda equation.
!       ================================================================
!       ----------------------------------------------------------------
!
!        Make a stab at solving the mathematical problem of finding the
!        near point.  In other words, solve the lambda equation.
!
!        Avoiding Numerical difficulties
!        -------------------------------
!
!        First make a copy of POINT, then to avoid numerical
!        difficulties later on, we will assume that any component of
!        POINT that is not sufficiently different from zero to
!        contribute to an addition to the corresponding component
!        of AXIS, is in fact zero.
!
         copy = point

         do i = 1, 3

            if (      ( (0.5_wp*point(i) + axis(i)) ==  axis(i) ) &
                 .or. ( (0.5_wp*point(i) - axis(i)) == -axis(i) ) ) &
            then
               point(i) = 0.0_wp
            end if

         end do

!
!        OK. Next we set up the logical that indicates whether
!        the current point is inside the ellipsoid.
!
         inside = .false.

!
!        Bracketing the root of the lambda equation.
!        -------------------------------------------
!
!        Let (x,y,z) stand for (POINT(1), POINT(2), POINT(3)) and
!        let (a,b,c) stand for (AXIS(1),  AXIS(2),   AXIS(3)).
!
!        The main step in finding the near point is to find the
!        root of the lambda equation:
!
!                 2                     2               2
!                x                     y               z
!       0 = --------------- + ---------------- + ---------------  - 1
!                         2                  2                 2
!           (a + lambda/a)    ( b + lambda/b)    (c + lambda/c)
!
!        We let Q(lambda) be the right hand side of this equation.
!        To find the roots of the equation we determine
!        values of lambda that make Q greater than 0 and less than 0.
!        An obvious value to check is lambda = 0.
!
         q = (point(1)/axis(1))**2 &
           + (point(2)/axis(2))**2 &
           + (point(3)/axis(3))**2 &
           - 1.0_wp

!
!        On the first solution pass, we will determine the sign of
!        the altitude of the input POSITN
!
         if ( solutn == 1 ) then

            if       ( q >= 0 ) then
               sign =  1.0_wp
            else
               sign = -1.0_wp
            end if

         end if

!
!        OK. Now for the stuff we will have to do on every solution
!        pass.
!
!        Below, LOWER and UPPER are the bounds on our independent
!        variable LAMBDA.  QLOWER and QUPPER are the values of Q
!        evaluated at LOWER and UPPER, respectively.  The root we
!        seek lies in the interval
!
!           [ LOWER, UPPER ]
!
!        At all points in the algorithm, we have, since Q is a
!        decreasing function to the right of the first non-removable
!        singularity,
!
!           QLOWER > 0
!                  -
!
!           QUPPER < 0
!                  -
!
!        We'll use bracketing to ensure that round-off errors don't
!        violate these inequalities.
!
!        The logical flag INSIDE indicates whether the point is
!        strictly inside the interior of the ellipsoid.  Points on the
!        surface are not considered to be inside.
!
         if ( q == 0.0_wp ) then
!
!           In this case the point is already on the ellipsoid
!           (pretty lucky eh?)  We simply set our bracketing values,
!           QLOWER and QUPPER, to zero so that that bisection
!           loop won't ever get executed.
!
            qlower = 0.0_wp
            qupper = 0.0_wp
            lower  = 0.0_wp
            upper  = 0.0_wp
            lambda = 0.0_wp

            inside = .false.

         else if ( q > 0.0_wp ) then
!
!           The input point is outside the ellipsoid (we expect that
!           this is the usual case).  We want to choose our lower
!           bracketing value so that the bracketing values for lambda
!           aren't too far apart.  So we just make sure that the largest
!           term of the expression for Q isn't bigger than 4.
!
            do i = 1, 3
               tlambd(i) = ( 0.5_wp*dabs(point(i)) - axis(i) ) * axis(i)
            end do

            lower = max ( 0.0_wp, tlambd(1), tlambd(2), tlambd(3) )

!
!           Choose the next value of lambda so that the largest term
!           of Q will be no more than 1/4. (?)
!
            upper  = 2.0_wp * max ( dabs( axis(1)*point(1) ), &
                                   dabs( axis(2)*point(2) ), &
                                   dabs( axis(3)*point(3) ) )
            lambda = upper

            inside = .false.

         else
!
!           In this case the point POSITN is inside the ellipsoid.
!
            inside = .true.

!
!           This case is a bit of a nuisance. To solve the lambda
!           equation we have to find upper and lower bounds on
!           lambda such that one makes Q greater than 0, the other
!           makes Q less than 0.  Once the root has been bracketed
!           in this way it is a straightforward problem to find
!           the value of LAMBDA that is closest to the root we
!           seek.  We already know that for LAMBDA = 0, Q is negative.
!           So we only need to find a value of LAMBDA that makes
!           Q positive.  But... the expression for Q has singularities
!           at LAMBDA = -a**2, -b**2, and -c**2.
!
!           These singularities are not necessarily to be avoided.
!           If the numerator of one of the terms for Q is zero, we
!           can simply compute Q ignoring that particular term.  We
!           say that a singularity corresponding to a term whose
!           numerator is zero is a viable singularity.  By being
!           careful in our computation of Q, we can assign LAMBDA to
!           the value of the singularity. A singularity that is not
!           viable is called a true singularity.
!
!           By choosing LAMBDA just slightly greater than the largest
!           true singularity, we can bracket the root we seek.
!
!           First we must decide which singularity is the first true
!           one.
!
            snglpt = 4

            do i = 3, 1, -1

               if ( point(i) /= 0 ) then
                  snglpt = i
               end if

            end do

!
!           If there is a singular point, compute LAMBDA so that the
!           largest term of Q is equal to 4.
!
            if ( snglpt <= 3 ) then

               do i = 1, 3

                  if ( point(i) == 0 ) then
                     tlambd(i) = -axisqr(3)
                  else
                     tlambd(i) = axis(i) * (   0.5_wp*dabs(point(i)) &
                                             - axis(i)              )
                  end if

               end do

               lambda = max ( tlambd(1), tlambd(2), tlambd(3) )
               lower  = lambda
               upper  = max ( lower, 0.0_wp )

            else
!
!              The point must be at the origin.  In this case
!              we know where the closest point is. WE DON'T have
!              to compute anything. It's just at the end of the
!              shortest semi-major axis.  However, since we
!              may have done some rounding off, we will make
!              sure that we pick the side of the shortest axis
!              that has the same sign as COPY(1).
!
!              We are going to be a bit sneaky here.  We know
!              where the closest point is so we are going to
!              simply make POINT and COPY equal to that point
!              and set the upper and lower bracketing bounds
!              to zero so that we won't have to deal with any
!              special cases later on.
!
               if ( copy(1) < 0 ) then
                  point(1) = -axis(1)
                  copy(1)  = -axis(1)
               else
                  point(1) =  axis(1)
                  copy(1)  =  axis(1)
               end if

               copy(2) = 0.0_wp
               copy(3) = 0.0_wp

               upper   = 0.0_wp
               lower   = 0.0_wp
               lambda  = 0.0_wp
               q       = 0.0_wp

               inside  = .false.

            end if

         end if

!
!        OK. Now compute the value of Q at the two bracketing
!        values of LAMBDA.
!
         do i = 1, 3

            if ( point(i) == 0 ) then

               term(i) = 0.0_wp

            else
!
!              We have to be a bit careful for points inside the
!              ellipsoid. The denominator of the factor we are going to
!              compute is ( AXIS + LAMBDA/AXIS ). Numerically this may
!              be too close to zero for us to actually divide POINT by
!              it.  However, since our solution algorithm for lambda
!              does not depend upon the differentiability of Q---in
!              fact it depends only on Q having the correct sign---we
!              can simply truncate its individual terms when we are in
!              danger of division overflows.

               denom = axis(i) + lambda/axis(i)

               trim  = 0.5_wp * dabs( point(i) )   >   denom

               if ( inside .and. trim ) then

                 factor = 2.0_wp

               else
!
!                 We don't expect DENOM to be zero here, but we'll
!                 check anyway.
!
                  if ( denom == 0.0_wp ) then

                     call ctx%setmsg ( 'AXIS(#) + LAMBDA/AXIS(#) is zero.' )
                     call ctx%errint ( '#',  i                             )
                     call ctx%errint ( '#',  i                             )
                     call ctx%sigerr ( 'SPICE(BUG)'                        )
                     return

                  end if

                  factor =  point(i) / denom

               end if

               term(i) = factor*factor

            end if

         end do

         if ( .not. inside ) then
            qlower = q
            qupper = term(1) + term(2) + term(3) - 1.0_wp
         else
            qupper = q
            qlower = term(1) + term(2) + term(3) - 1.0_wp
         end if

!
!        Bracket QLOWER and QUPPER.
!
         qlower = max ( 0.0_wp,  qlower )
         qupper = min ( 0.0_wp,  qupper )

         lambda = upper
         q      = qupper

!
!        Refining the estimate of lambda
!        -------------------------------
!
!        Now find the root of Q by bisection.
!
         itr = 0
!
!        Throughout this loop we'll use TOUCHD to avoid logic problems
!        that may be caused by extended precision register usage by
!        some compilers.
!
         do while ( (upper-lower) > 0.0_wp )

            itr = itr + 1

            if ( itr > maxitr ) then

               call ctx%setmsg ('Iteration limit # exceeded in NEARPT. ' // &
                            'A, B, C = # # #; POSITN = ( #, #, # ). '// &
                            'LOWER = #; UPPER = #; UPPER-LOWER = #. '// &
                            'Solution pass number = #.  This event ' // &
                            'should never occur. Contact NAIF.'      )
               call ctx%errint ( '#',  maxitr                            )
               call ctx%errdp  ( '#',  a                                 )
               call ctx%errdp  ( '#',  b                                 )
               call ctx%errdp  ( '#',  c                                 )
               call ctx%errdp  ( '#',  positn(1)                         )
               call ctx%errdp  ( '#',  positn(2)                         )
               call ctx%errdp  ( '#',  positn(3)                         )
               call ctx%errdp  ( '#',  lower                             )
               call ctx%errdp  ( '#',  upper                             )
               call ctx%errdp  ( '#',  upper - lower                     )
               call ctx%sigerr ( 'SPICE(BUG)'                            )
               return

            end if

!
!           Bracket LOWER, QLOWER, and QUPPER.
!
            lower  = min ( lower, upper  )
            qlower = max ( 0.0_wp,  qlower )
            qupper = min ( 0.0_wp,  qupper )

!
!           Depending upon how Q compares with Q at the
!           bracketing endpoints we adjust the endpoints
!           of the bracketing interval
!
            if ( q == 0 ) then
!
!              We've found the root.
!
               lower = lambda
               upper = lambda

            else

               if ( q < 0.0_wp ) then

                  upper  = lambda
                  qupper = q

               else
!
!                 We have Q > 0
!
                  lower  = lambda
                  qlower = q

               end if
!
!              Update LAMBDA.
!
               lambda = 0.5_wp*lower + 0.5_wp*upper

!
!              It's quite possible as we get close to the root for Q
!              that round off errors in the computation of the next
!              value of LAMBDA will push it outside of the current
!              bracketing interval.  Force it back in to the current
!              interval.
!
               lambda = max(min(lower, upper), min(max(lower, upper), lambda))

            end if
!
!           At this point, it's guaranteed that
!
!              LOWER  <  LAMBDA  <  UPPER
!                     -          -
!
!           If we didn't find a root, we've set LAMBDA to the midpoint
!           of the previous values of LOWER and UPPER, and we've moved
!           either LOWER or UPPER to the old value of LAMBDA, thereby
!           halving the distance between LOWER and UPPER.
!
!           If we are still at an endpoint, we might as well cash in
!           our chips.  We aren't going to be able to get away from the
!           endpoints.  Set LOWER and UPPER equal so that the loop will
!           finally terminate.
!
            if (      approx( lambda, lower, cnvtol ) &
                 .or. approx( lambda, upper, cnvtol )  ) then
!
!              Make the decision as to which way to push
!              the boundaries, by selecting that endpoint
!              at which Q is closest to zero.

               if ( dabs(qlower) < dabs(qupper) ) then
                  upper = lower
               else
                  lower = upper
               end if
!
!              Since LOWER is equal to UPPER, the loop will terminate.
!
            end if

!
!           If LOWER and UPPER aren't the same, we compute the
!           value of Q at our new guess for LAMBDA.
!
            if ( (upper-lower) > 0 ) then

               do i = 1, 3

                  if ( point(i) == 0 ) then

                     term(i) = 0.0_wp

                  else

                     denom =  axis(i) + lambda/axis(i)

                     trim  =  0.5_wp * dabs( point(i) )   >   denom

                     if ( inside .and. trim ) then

                        factor = 2.0_wp

                     else
!
!                       We don't expect DENOM to be zero here, but we'll
!                       check anyway.
!
                        if ( denom == 0.0_wp ) then

                           call ctx%setmsg ( 'AXIS(#) + LAMBDA/AXIS(#) ' // &
                                         'is zero.'                  )
                           call ctx%errint ( '#',  i                     )
                           call ctx%errint ( '#',  i                     )
                           call ctx%sigerr ( 'SPICE(BUG)'                )
                           return

                        end if

                        factor =  point(i) / denom

                     end if

                     term(i) = factor*factor

                  end if

               end do

               q  =  (term(1) + term(2) + term(3) - 1.0_wp)

            end if
!
!           Q(LAMBDA) has been set unless we've already found
!           a solution.
!
!           Loop back through the bracketing refinement code.
!
        end do

!
!       Now we have LAMBDA, compute the nearest point based upon
!       this value.
!
        lambda = lower

        do i = 1, 3

           if ( point(i) == 0.0_wp ) then

              spoint(i) = 0.0_wp

           else

              denom = 1.0_wp +  lambda/axisqr(i)
!
!             We don't expect that DENOM will be non-positive, but we
!             check for this case anyway.
!
              if ( denom <= 0.0_wp ) then

                 call ctx%setmsg ( 'Denominator in expression for ' // &
                               'SPOINT(#) is #.'                )
                 call ctx%errint ( '#', i                           )
                 call ctx%errdp  ( '#', denom                       )
                 call ctx%sigerr ( 'SPICE(BUG)'                     )
                 return

              end if

              spoint(i) = copy(i) / denom

           end if

        end do

!
!       Handling points on the central plane.
!       -------------------------------------
!
!       I suppose you thought you were done at this point. Not
!       necessarily.  If POINT is INSIDE the ellipsoid and happens to
!       lie in the Y-Z plane, there is a possibility (perhaps even
!       likelihood) that the nearest point on the ellipsoid is NOT in
!       the Y-Z plane. we must consider this possibility next.
!
        if ( inside  .and. (      (snglpt == 2) &
                             .or. (snglpt == 3) ) ) then
!
!          There are two ways to get here. SNGLPT = 2 or SNGLPT = 3.
!          Fortunately these two cases can be handled simultaneously by
!          code. However, they are most easily understood if explained
!          separately.
!
!          Case 1.  SNGLPT = 2
!
!          The input to the lambda solution POINT lies in the Y-Z plane.
!          We have already detected one critical point of the
!          distance function to POINT restricted to the ellipsoid.
!          This point also lies in the Y-Z plane.  However, when
!          POINT lies on the Y-Z plane close to the center of the
!          ellipsoid, there may be a point that is nearest that does
!          not lie in the Y-Z plane. Assuming the existence of such a
!          point, (x,y,z) it must satisfy the equations
!
!                   lambda*x
!             x  +  --------  = POINT(1)  = 0
!                      a*a
!
!                    lambda*y
!             y  +  --------  = POINT(2)
!                      b*b
!
!                   lambda*z
!             z  +  --------  = POINT(3)
!                      c*c
!
!          Since we are assuming that this undetected solution (x,y,z)
!          does not have x equal to 0, it must be the case that
!
!          lambda = -a*a.
!
!          Because of this, we must have
!
!             y    =  POINT(2) / ( 1 - (a**2/b**2) )
!             z    =  POINT(3) / ( 1 - (a**2/c**2) )
!
!          The value of x is obtained by forcing
!
!             (x/a)**2 + (y/b)**2 + (z/c)**2 = 1.
!
!          This assumes of course that a and b are not equal. If a and
!          b are the same, then since POINT(2) is not zero, the
!          solution we found above by deflating the original ellipsoid
!          will find the near point.
!
!             (If a and b are equal, the ellipsoid deflates to a
!              segment on the z-axis when lambda = -a**2. Since
!              POINT(2) is not zero, the deflating ellipsoid must pass
!              through POINT before it collapses to a segment.)
!
!          Case 2.  SNGLPT = 3
!
!          The input to the lambda solution POINT lies on the Z-axis.
!          The solution obtained in the generic case above will
!          locate the critical point of the distance function
!          that lies on the Z.  However, there will also be
!          critical points in the X-Z plane and Y-Z plane.  The point
!          in the X-Z plane is the one to examine.  Why?  We are looking
!          for the point on the ellipsoid closest to POINT.  It must
!          lie in one of these two planes. But the ellipse of
!          intersection with the X-Z plane fits inside the ellipse
!          of intersection with the Y-Z plane.  Therefore the closest
!          point on the Y-Z ellipse must be at a greater distance than
!          the closest point on the X-Z ellipse.  Thus, in solving
!          the equations
!
!                   lambda*x
!             x  +  --------  = POINT(1)  = 0
!                      a*a
!
!                    lambda*y
!             y  +  --------  = POINT(2)  = 0
!                      b*b
!
!                   lambda*z
!             z  +  --------  = POINT(3)
!                      c*c
!
!          We have
!
!             lambda = -a*a,
!
!             y      =  0,
!
!             z      =  POINT(3) / ( 1 - (a**2/c**2) )
!
!          The value of x is obtained by forcing
!
!             (x/a)**2 + (y/b)**2 + (z/c)**2 = 1.
!
!          This assumes that a and c are not equal.  If
!          a and c are the same, then the solution we found above
!          by deflating the original ellipsoid, will find the
!          near point.
!
!             ( If a = c, then the input ellipsoid is a sphere.
!               The ellipsoid will deflate to the center of the
!               sphere.  Since our point is NOT at the center,
!               the deflating sphere will cross through
!               (x,y,z) before it collapses to a point )
!
!          We begin by assuming this extra point doesn't exist.
!
           extra = .false.

!
!          Next let's note a few simple tests we can apply to
!          eliminate searching for an extra point.
!
!          First of all the smallest axis must be different from
!          the axis associated with the first true singularity.
!
!          Next, whatever point we find, it must be true that
!
!               |y| < b, |z| < c
!
!          because of the condition on the absolute values, we must
!          have:
!
!               | POINT(2) | <= b - a*(a/b)
!
!               | POINT(3) | <= c - a*(a/c)
!

           if (     (     axis (1)  /= axis(snglpt)                ) &
               .and.(dabs(point(2)) <= axis(2) - axisqr(1)/axis(2) ) &
               .and.(dabs(point(3)) <= axis(3) - axisqr(1)/axis(3) )) &
           then
!
!             What happens next depends on whether the ellipsoid is
!             prolate or triaxial.
!
              if ( axis(1) == axis(2) ) then
!
!                This is the prolate case; we need to compute the
!                z component of the solution.
!
                 denom3 = 1.0_wp - axisqr(1)/axisqr(3)

                 if ( denom3 > 0.0_wp ) then

                    epoint(2) = 0.0_wp
!
!                   Concerning the safety of the following division:
!
!                      - A divide-by-zero check has been done above.
!
!                      - The numerator can be squared without exceeding
!                        DPMAX(). This was checked near the start of the
!                        routine.
!
!                      - The denominator was computed as the difference
!                        of 1.D0 and another number. The smallest
!                        possible magnitude of a non-zero value of the
!                        denominator is roughly 1.D-16, assuming IEEE
!                        double precision numeric representation.
!
                    epoint(3) = point(3) / denom3

!
!                   See if these components can even be on the
!                   ellipsoid...
!
                    temp      = 1.0_wp  - ( epoint(2) / axis(2) )**2 &
                                       - ( epoint(3) / axis(3) )**2

                    if ( temp > 0 ) then
!
!                      ...and compute the x component of the point.
!
                       epoint(1) = axis(1) * dsqrt( max(0.0_wp,temp) )
                       extra     = .true.

                    end if

                 end if

              else
!
!                This is the triaxial case.
!
!                Compute the y and z components (2 and 3) of the extra
!                point.
!
                 denom2 = 1.0_wp - (axisqr(1)/axisqr(2))
                 denom3 = 1.0_wp - (axisqr(1)/axisqr(3))

!
!                We expect DENOM2 and DENOM3 will always be positive.
!                Nonetheless, we check to make sure this is the case.
!                If not, we don't compute the extra point.
!
                 if (       ( denom2 > 0.0_wp ) &
                      .and. ( denom3 > 0.0_wp ) ) then

!
!                   Concerning the safety of the following divisions:
!
!                      - Divide-by-zero checks have been done above.
!
!                      - The numerators can be squared without exceeding
!                        DPMAX(). This was checked near the start of the
!                        routine.
!
!                      - Each denominator was computed as the difference
!                        of 1.D0 and another number. The smallest
!                        possible magnitude of a non-zero value of
!                        either denominator is roughly 1.D-16, assuming
!                        IEEE double precision numeric representation.
!

                    epoint(2) = point(2) / denom2
                    epoint(3) = point(3) / denom3
!
!                   See if these components can even be on the
!                   ellipsoid...
!
                    temp      = 1.0_wp  - ( epoint(2) / axis(2) )**2 &
                                       - ( epoint(3) / axis(3) )**2

                    if ( temp > 0 ) then
!
!                      ...and compute the x component of the point.
!
                       epoint(1) = axis(1) * dsqrt(temp)
                       extra     = .true.

                    end if

                 end if

              end if

           end if

!
!          Ok.  If an extra point is possible, check and see if it
!          is the one we are searching for.
!
           if ( extra ) then

              if (      vdist(epoint, point) &
                   < vdist(spoint, point) ) then

                 spoint = epoint

              end if

           end if

        end if

!
!       Chapter 5
!
!       Decisions and initializations for sharpening the solution.
!       ================================================================
!       ----------------------------------------------------------------
!
        if ( solutn == 1 ) then

!
!           The first solution for the nearest point may not be
!           very close to being on the ellipsoid.  To
!           take care of this case, we next find the point on the
!           ellipsoid, closest to our first solution point.
!           (Ideally the normal line at this second point should
!           contain both the current solution point and the
!           original point).
!
            point = spoint
            bestpt = spoint

            bestht = vdist ( bestpt, orignl )

         else if ( solutn == 2 ) then

!
!           The current solution point will be very close to lying
!           on the ellipsoid.  However, the normal at this solution
!           may not actually point toward the input point.
!
!           With the current solution we can predict
!           the location of the input point.  The difference between
!           this predicted point and the actual point can be used
!           to sharpen our estimate of the solution.
!
!           The sharpening is performed by
!
!              1) Compute the vector ERR that gives the difference
!                 between the input point (POSITN) and the point
!                 computed using the solution point, normal and
!                 altitude.
!
!              2) Find the component of ERR that is orthogonal to the
!                 normal at the current solution point. If the point
!                 is outside the ellipsoid, scale this component to
!                 the approximate scale of the near point.  We use
!                 the scale factor
!
!                     ||near point|| / ||input point||
!
!                  Call this scaled component ERRP.
!
!              3) Translate the solution point by ERRP to get POINT.
!
!              4) Find the point on the ellipsoid closest to POINT.
!                 (step 4 is handled by the solution loop above).
!
!           First we need to compute the altitude
!
            height  = sign * vdist( spoint, orignl )

!
!           Next compute the difference between the input point and
!           the one we get by moving out along the normal at our
!           solution point by the computed altitude.
!
            call surfnm ( ctx, axis(1), axis(2), axis(3), spoint, normal )

            do i=1, 3

               err(i) = orignl(i) - (spoint(i) + height*normal(i))

            end do

!
!           Find the component of the error vector that is
!           perpendicular to the normal, and shift our solution
!           point by this component.
!
            call vperp ( err, normal, errp )

!
!           Check for a zero projection. If so, set iteration flag to
!           prevent an iteration pass after the current finishes.
!
            if ( all(errp == 0.0_wp) ) then
               solvng = .false.
            end if

!
!           The sign of the original point's altitude tells
!           us whether the point is outside the ellipsoid.
!
            if ( sign >= 0.0_wp ) then
!
!              Scale the transverse component down to the local radius
!              of the surface point.
!
               if ( pnorm == 0.0_wp ) then
!
!                 Since the point is outside of the scaled ellipsoid,
!                 we don't expect to arrive here. This is a backstop
!                 check.
!
                  call ctx%setmsg ( 'Norm of scaled point is 0. ' // &
                                'POSITN = ( #, #, # )'        )
                  call ctx%errdp  ( '#',  positn(1)               )
                  call ctx%errdp  ( '#',  positn(2)               )
                  call ctx%errdp  ( '#',  positn(3)               )
                  call ctx%sigerr ( 'SPICE(BUG)'                  )
                  return

               end if

               if ( solvng ) then
                  errp = (vnorm(spoint)/pnorm)*errp
               end if

            end if

            point = spoint + errp

            if (  (spoint(1) == point(1)) .and. &
                  (spoint(2) == point(2)) .and. &
                  (spoint(3) == point(3))       ) then
               solvng = .false.
            end if

            olderr = vnorm ( err )
            bestht = height

!
!           Finally store the current solution point, so that if
!           this sharpening doesn't improve our estimate of the
!           near point, we can just return our current best guess.
!
            bestpt = spoint

         else if ( solutn > 2 ) then

!
!           This branch exists for the purpose of testing our
!           "sharpened" solution and setting up for another sharpening
!           pass.
!
!           We have already stored our best guess so far in BESTPT and
!           the vector ERR is the difference
!
!              ORIGNL - ( BESTPT + BESTHT*NORMAL )
!
!           We have just computed a new candidate "best" near point.
!           SPOINT.
!
!           If the error vector
!
!              ORIGNL - ( SPOINT + HEIGHT*NORMAL)
!
!           is shorter than our previous error, we will make SPOINT
!           our best guess and try to sharpen our estimate again.
!
!           If our sharpening method hasn't improved things, we just
!           call it quits and go with our current best guess.
!

!
!           First compute the altitude,
!
            height  = sign * vdist( spoint, orignl )

!
!           ... compute the difference
!
!              ORIGNL - SPOINT - HEIGHT*NORMAL,
!
            call surfnm ( ctx, axis(1), axis(2), axis(3), spoint, normal )

            do i = 1, 3

               err(i) = orignl(i) - (spoint(i) + height*normal(i))

            end do

!
!           Check for a zero difference. If so, set iteration flag to
!           prevent an iteration pass after the current finishes.
!
            if ( all(err == 0.0_wp) ) then
               solvng = .false.
               newerr = 0.0_wp
            else

!              Determine the magnitude of the error due to our
!              sharpened estimate, if error non zero.
!
               newerr = vnorm ( err )
            end if

!
!           If the sharpened estimate yields a smaller error ...
!
            if ( newerr < olderr ) then
!
!              ...our current value of SPOINT becomes our new
!              best point and the current altitude becomes our
!              new altitude.
!
               olderr  =  newerr
               bestht  =  height

               bestpt = spoint

!
!              Next, if we haven't passed the limit on the number of
!              iterations allowed we prepare the initial point for our
!              "sharpening" estimate.
!
               if ( solutn <= maxsol ) then

!                 Find the component of the error vector that is
!                 perpendicular to the normal, and shift our solution
!                 point by this component.

                  call vperp ( err, normal, errp )

!
!                 Check for a zero projection. If so, set iteration
!                 flag to prevent an iteration pass after the current
!                 finishes.
!
                  if ( all(errp == 0.0_wp) ) then
                     solvng = .false.
                  end if

!
!                 The sign of the original point's altitude tells
!                 us whether the point is outside the ellipsoid.
!
                  if ( sign >= 0.0_wp ) then
!
!                    Scale the transverse component down to the local
!                    radius of the surface point.
!
                     if ( pnorm == 0.0_wp ) then
!
!                       Since the point is outside of the scaled
!                       ellipsoid, we don't expect to arrive here.
!                       This is a backstop check.
!
                        call ctx%setmsg ( 'Norm of scaled point is 0. ' // &
                                      'POSITN = ( #, #, # )'        )
                        call ctx%errdp  ( '#',  positn(1)               )
                        call ctx%errdp  ( '#',  positn(2)               )
                        call ctx%errdp  ( '#',  positn(3)               )
                        call ctx%sigerr ( 'SPICE(BUG)'                  )
                        return

                     end if

                     if ( solvng ) then
                        errp = (vnorm(spoint)/pnorm)*errp
                     end if

                  end if

                  point = spoint + errp

                  if (  (spoint(1) == point(1)) .and. &
                        (spoint(2) == point(2)) .and. &
                        (spoint(3) == point(3))       ) then
                     solvng = .false.
                  end if

               end if

            else
!
!              If things didn't get better, there is no point in
!              going on.  Just set the SOLVNG flag to .FALSE. to
!              terminate the outer loop.
!
               solvng = .false.

            end if

         end if

!
!        Increment the solution counter so that eventually this
!        loop will terminate.
!
         solutn = solutn + 1

         solvng = solvng .and. ( solutn <= maxsol )

      end do

!
!     Chapter 6
!
!     Clean up.
!     ==================================================================
!     ------------------------------------------------------------------
!
!     Re-scale and re-order the components of our solution point. Scale
!     and copy the value of BESTHT into the output argument.
!
      bestpt = (1.0_wp/scale)*bestpt

      do i = 1,3
         npoint( iorder(i) ) = bestpt(i)
      end do

      alt = bestht/scale

      return
      end subroutine nearpt

      !> Convert from rectangular coordinates to geodetic coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RECTAN   are the rectangular coordinates of a point. RECTAN
      !>              must be in the same units as RE.
      !>
      !>     RE       is the equatorial radius of a reference spheroid. This
      !>              spheroid is a volume of revolution: its horizontal cross
      !>              sections are circular. The shape of the spheroid is
      !>              defined by an equatorial radius RE and a polar radius RP.
      !>              RE must be in the same units as RECTAN.
      !>
      !>     F        is the flattening coefficient = (RE-RP) / RE, where RP is
      !>              the polar radius of the spheroid.
      !>
      !>## Detailed Output
      !>
      !>     LON      is the geodetic longitude of the input point. This is the
      !>              angle between the prime meridian and the meridian
      !>              containing RECTAN. The direction of increasing longitude
      !>              is from the +X axis towards the +Y axis.
      !>
      !>              LON is output in radians. The range of LON is [-pi, pi].
      !>
      !>     LAT      is the geodetic latitude of the input point. For a point
      !>              P on the reference spheroid, this is the angle between
      !>              the XY plane and the outward normal vector at P. For a
      !>              point P not on the reference spheroid, the geodetic
      !>              latitude is that of the closest point to P on the
      !>              spheroid.
      !>
      !>              LAT is output in radians. The range of LAT is
      !>              [-pi/2, pi/2].
      !>
      !>     ALT      is the altitude of point above the reference spheroid.
      !>
      !>              The units associated with ALT are those associated with
      !>              the inputs RECTAN and RE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the equatorial radius is non-positive, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     2)  If the flattening coefficient is greater than or equal to
      !>         one, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     3)  For points inside the reference ellipsoid, the nearest
      !>         point on the ellipsoid to RECTAN may not be unique, so
      !>         latitude may not be well-defined.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given the body-fixed rectangular coordinates of a point, and the
      !>     constants describing the reference spheroid,  this routine
      !>     returns the geodetic coordinates of the point. The body-fixed
      !>     rectangular frame is that having the x-axis pass through the
      !>     0 degree latitude 0 degree longitude point. The y-axis passes
      !>     through the 0 degree latitude 90 degree longitude. The z-axis
      !>     passes through the 90 degree latitude point. For some bodies
      !>     this coordinate system may not be a right-handed coordinate
      !>     system.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the geodetic coordinates of the point having Earth
      !>        rectangular coordinates:
      !>
      !>           X (km) =  -2541.748162
      !>           Y (km) =   4780.333036
      !>           Z (km) =   3360.428190
      !>
      !>        Use the PCK kernel below to load the required triaxial
      !>        ellipsoidal shape model and orientation data for the Earth.
      !>
      !>           pck00010.tpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECGEO_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ALT
      !>      DOUBLE PRECISION      F
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      RADII  ( 3 )
      !>      DOUBLE PRECISION      RE
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      RP
      !>
      !>      INTEGER               N
      !>
      !>C
      !>C     Load a PCK file containing a triaxial
      !>C     ellipsoidal shape model and orientation
      !>C     data for the Earth.
      !>C
      !>      CALL FURNSH ( 'pck00010.tpc' )
      !>
      !>C
      !>C     Retrieve the triaxial radii of the Earth
      !>C
      !>      CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII )
      !>
      !>C
      !>C     Compute flattening coefficient.
      !>C
      !>      RE  =  RADII(1)
      !>      RP  =  RADII(3)
      !>      F   =  ( RE - RP ) / RE
      !>
      !>C
      !>C     Set a body-fixed position.
      !>C
      !>      RECTAN(1) =  -2541.748162D0
      !>      RECTAN(2) =   4780.333036D0
      !>      RECTAN(3) =   3360.428190D0
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      CALL RECGEO( RECTAN, RADII(1), F, LON, LAT, ALT )
      !>
      !>      WRITE (*,*) 'Rectangular coordinates in km (x, y, z)'
      !>      WRITE (*,'(A,3F14.6)') ' ', RECTAN
      !>      WRITE (*,*) 'Geodetic coordinates in deg and '
      !>     . //         'km (lon, lat, alt)'
      !>      WRITE (*,'(A,3F14.6)') ' ', LON * DPR(), LAT * DPR(), ALT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Rectangular coordinates in km (x, y, z)
      !>           -2541.748162   4780.333036   3360.428190
      !>         Geodetic coordinates in deg and km (lon, lat, alt)
      !>             118.000000     31.999957      0.001916
      !>
      !>     2) Create a table showing a variety of rectangular coordinates
      !>        and the corresponding Earth geodetic coordinates. The
      !>        values are computed using the equatorial radius of the Clark
      !>        66 spheroid and the Clark 66 flattening factor:
      !>
      !>           radius: 6378.2064
      !>           flattening factor: 1./294.9787
      !>
      !>        Note: the values shown above may not be current or suitable
      !>              for your application.
      !>
      !>        Corresponding rectangular and geodetic coordinates are
      !>        listed to three decimal places. Output angles are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECGEO_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      ALT
      !>      DOUBLE PRECISION      CLARKR
      !>      DOUBLE PRECISION      CLARKF
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      RECTAN ( 3, NREC )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the input rectangular coordinates.
      !>C
      !>      DATA                 RECTAN /
      !>     .                  0.D0,         0.D0,         0.D0,
      !>     .               6378.2064D0,     0.D0,         0.D0,
      !>     .                  0.D0,      6378.2064D0,     0.D0,
      !>     .                  0.D0,         0.D0,      6378.2064D0,
      !>     .              -6378.2064D0,     0.D0,         0.D0,
      !>     .                  0.D0,     -6378.2064D0,     0.D0,
      !>     .                  0.D0,         0.D0,     -6378.2064D0,
      !>     .               6378.2064D0,  6378.2064D0,     0.D0,
      !>     .               6378.2064D0,     0.D0,      6378.2064D0,
      !>     .                  0.D0,      6378.2064D0,  6378.2064D0,
      !>     .               6378.2064D0,  6378.2064D0,  6378.2064D0 /
      !>
      !>C
      !>C     Using the equatorial radius of the Clark66 spheroid
      !>C     (CLARKR = 6378.2064 km) and the Clark 66 flattening
      !>C     factor (CLARKF = 1.0 / 294.9787 ) convert from
      !>C     body fixed rectangular coordinates.
      !>C
      !>      CLARKR = 6378.2064D0
      !>      CLARKF = 1.0D0 / 294.9787D0
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) ' RECTAN(1)  RECTAN(2)  RECTAN(3) '
      !>     . //        '   LON      LAT       ALT'
      !>      WRITE(*,*) ' ---------  ---------  --------- '
      !>     . //        ' -------  -------  ---------'
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         CALL RECGEO( RECTAN(1,I), CLARKR, CLARKF,
      !>     .                LON,         LAT,    ALT    )
      !>
      !>         WRITE (*,'(3F11.3,2F9.3,F11.3)')
      !>     .             ( RECTAN(J,I), J=1,3 ), LON * DPR(),
      !>     .             LAT * DPR(), ALT
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>          RECTAN(1)  RECTAN(2)  RECTAN(3)    LON      LAT       ALT
      !>          ---------  ---------  ---------  -------  -------  ---------
      !>              0.000      0.000      0.000    0.000   90.000  -6356.584
      !>           6378.206      0.000      0.000    0.000    0.000      0.000
      !>              0.000   6378.206      0.000   90.000    0.000      0.000
      !>              0.000      0.000   6378.206    0.000   90.000     21.623
      !>          -6378.206      0.000      0.000  180.000    0.000      0.000
      !>              0.000  -6378.206      0.000  -90.000    0.000      0.000
      !>              0.000      0.000  -6378.206    0.000  -90.000     21.623
      !>           6378.206   6378.206      0.000   45.000    0.000   2641.940
      !>           6378.206      0.000   6378.206    0.000   45.137   2652.768
      !>              0.000   6378.206   6378.206   90.000   45.137   2652.768
      !>           6378.206   6378.206   6378.206   45.000   35.370   4676.389
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
      !>          Astrodynamics," Dover Publications Inc., 1971.
      !>
      !>## Author and Institution
      !>
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR) (NJB)
      !>
      !>        Changed the output argument name LONG to LON for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples.
      !>
      !>-    SPICELIB Version 1.1.0, 03-AUG-2016 (BVS) (NJB)
      !>
      !>        Re-implemented derivation of longitude to improve
      !>        accuracy.
      !>
      !>        Minor header edits.
      !>
      !>-    SPICELIB Version 1.0.3, 02-JUL-2007 (NJB)
      !>
      !>        In $Examples section of header, description of right-hand
      !>        table was updated to use correct names of columns. Term
      !>        "bodyfixed" is now hyphenated.
      !>
      !>-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header changes were made to improve clarity. Some
      !>        minor header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 3.0.1, 9-JUN-1989 (HAN)
      !>
      !>        Error handling was added to detect and equatorial radius
      !>        whose value is less than or equal to zero.
      !>
      !>-    Beta Version 2.0.0, 21-DEC-1988 (HAN)
      !>
      !>        Error handling to detect invalid flattening coefficients
      !>        was added. Because the flattening coefficient is used to
      !>        compute the length of an axis, it must be checked so that
      !>        the length is greater than zero.
      module subroutine recgeo ( ctx, rectan, re, f, lon, lat, alt )
      use pepper_kinds, only: wp
      use pepper_math, only: reclat
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp), intent(in) :: re  !! Equatorial radius of the reference spheroid.
      real(wp), intent(in) :: f  !! Flattening coefficient.
      real(wp), intent(out) :: lon  !! Geodetic longitude of the point (radians).
      real(wp), intent(out) :: lat  !! Geodetic latitude  of the point (radians).
      real(wp), intent(out) :: alt  !! Altitude of the point above reference spheroid.

      real(wp) :: a
      real(wp) :: b
      real(wp) :: c
      real(wp) :: base   (3)
      real(wp) :: normal (3)
      real(wp) :: radius

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The equatorial radius must be positive. If not, signal an error.
!
      if ( re<= 0.0_wp ) then
          call ctx%setmsg ( 'Equatorial radius was *.' )
          call ctx%errdp  ( '*', re                    )
          call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'   )
          return
      end if

!
!     If the flattening coefficient is greater than one, the length
!     of the 'C' axis computed below is negative. If it's equal to one,
!     the length of the axis is zero. Either case is a problem, so
!     signal an error.
!
      if ( f >= 1 ) then
          call ctx%setmsg ( 'Flattening coefficient was *.'  )
          call ctx%errdp  ( '*', f                           )
          call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
          return
      end if

!
!     Determine the lengths of the axes of the reference ellipsoid.
!
      a = re
      b = re
      c = re - f*re

!
!     Find the point on the reference spheroid closest to the input
!     point. From this closest point determine the surface normal.
!
      call nearpt ( ctx, rectan, a, b, c, base, alt    )
      call surfnm ( ctx,         a, b, c, base, normal )
!
!     Using the surface normal, determine the latitude and longitude
!     of the input point.
!
      call reclat ( normal, radius, lon, lat )

!
!     Compute longitude directly rather than from the normal vector.
!
      if (       ( rectan(1) ==0.0_wp ) &
           .and. ( rectan(2) ==0.0_wp ) ) then

         lon = 0.0_wp
      else
         lon = datan2 ( rectan(2), rectan(1) )
      end if

      return
      end subroutine recgeo

      !> Generate a data structure that represents the mapping from
      !> vertices to the plates containing them.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NV         Number of vertices.
      !>
      !>     NP         Number of plates.
      !>
      !>     PLATES     Plate array: this is an array of 3-tuples of
      !>                vertex indices. The elements
      !>
      !>                   PLATES(1:3,I)
      !>
      !>                are the indices of the vertices of the Ith plate.
      !>
      !>     CELLSZ     Size of cell array: explicit workspace dimension used
      !>                for error checking. This value should be
      !>
      !>                   3*NP
      !>
      !>     MAXLST     Maximum size of output plate list. This size should be
      !>
      !>                   3*NP  +  NV
      !>
      !>     CELLS      workspace array used to construct the vertex-plate
      !>                mapping.
      !>
      !>## Detailed Output
      !>
      !>     CELLS      Workspace array used to construct the vertex-plate
      !>                mapping.
      !>
      !>     VRTPTR     Array of pointers associating plate lists with
      !>                vertices. The Ith element of VRTPTR contains
      !>                the start index in PLTLST of the list associated with
      !>                the Ith vertex.
      !>
      !>                The size of this array must be at least NV.
      !>
      !>     NLIST      Length of the output plate data list PLTLST.
      !>
      !>     PLTLST     Plate data list: for each vertex, there is a count
      !>                of associated plates, followed by the IDs of those
      !>                plates. The count for vertex I is located at
      !>
      !>                   PLTLST( VRTPTR(I) )
      !>
      !>                The size of this array must be at least MAXLST.
      !>
      !>## Parameters
      !>
      !>     See parameter declarations in
      !>
      !>        dsk02.inc
      !>
      !>## Exceptions
      !>
      !>     1)  If the input plate count NP is non-positive, the
      !>         error SPICE(BADPLATECOUNT) is signaled.
      !>
      !>     2)  If the input vertex count NV is non-positive, the
      !>         error SPICE(BADVERTEXCOUNT) is signaled.
      !>
      !>     3)  If the cell array size CELLSZ is less than
      !>
      !>            3 * NP
      !>
      !>         the error SPICE(CELLARRAYTOOSMALL) is signaled.
      !>
      !>     4)  If the plate list size MAXPLT is less than
      !>
      !>            ( 3 * NP ) + NV
      !>
      !>         the error SPICE(PLATELISTTOOSMALL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine supports the DSK type 2 spatial index
      !>     routines
      !>
      !>        DSKMI2
      !>        DSKSI2
      !>
      !>## Examples
      !>
      !>     See usage in DSKMI2.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-NOV-2020 (NJB)
      !>
      !>        Bug fix: in call to ZZADDLNK, size of pointer array was
      !>        changed from NP to NV.
      !>
      !>-    SPICELIB Version 1.0.0, 20-MAY-2016 (NJB)
      !>
      !>        Updated error handling. Now ZZVRTPLT performs precise checks
      !>        on cell and plate array size inputs.
      !>
      !>        22-DEC-2015 (NJB)
      !>
      !>           Renamed routine from VRTCOM to ZZVRTPLT.
      !>
      !>           CAUTION: argument list change! Removed input workspace
      !>           array for pointer construction. Array was named PNTRS.
      !>
      !>           Added error checks for input counts and sizes.
      !>
      !>           Now calls updated 3.0.0 version of ZZUNTNGL.
      !>
      !>        03-MAY-2014 (NJB)
      !>
      !>           Now calls ZZ* versions of supporting routines.
      !>
      !>           Changed argument list to include sizes of arrays.
      !>           Changed error handling to make use of array sizes.
      !>           Changed call to UNTNGL to accommodate argument
      !>           list change in that routine. Updated header I/O
      !>           descriptions.
      !>
      !>        12-JUL-2011 (NJB)
      !>
      !>           Argument list change: the input arrays V1, V2, V3 were
      !>           replaced with the input array PLATES.
      !>
      !>        04-MAY-2010 (NJB)
      !>
      !>           Now accepts input workspace arrays PNTRS and CELLS.
      !>           Changed INCLUDE file to dsk02.inc.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Re-ordered header sections.
      !>
      !>        04-FEB-1999 (JAB)
      module subroutine zzvrtplt ( ctx, nv, np, plates, cellsz, maxlst, cells, vrtptr, nlist, pltlst )
      use pepper_kinds, only: ip
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nv  !! Number of vertices.
      integer(ip), intent(in) :: np  !! Number of plates.
      integer(ip), intent(in) :: plates ( 3, * )  !! Plate array.
      integer(ip), intent(in) :: cellsz  !! Cell array size.
      integer(ip), intent(in) :: maxlst  !! Maximum size of output plate list.
      integer(ip), intent(inout) :: cells  ( 2, cellsz )  !! Workspace array for vertex list
                                                          !! construction.
      integer(ip), intent(out) :: vrtptr ( * )  !! Array of pointers, by vertex, into plate data
                                                !! list.
      integer(ip), intent(out) :: nlist  !! Length of the plate data list.
      integer(ip), intent(out) :: pltlst ( * )  !! Plate data list; first element is the number
                                                !! of plate IDs to follow.
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: ncell

!
!     Standard SPICELIB error handling.
!

      if ( ctx%failed() ) return

      if ( nv < 1 ) then

         call ctx%setmsg ( 'Vertex count NV = #; count must be positive.' &
         //            'be positive.'                                )
         call ctx%errint ( '#',  nv                                      )
         call ctx%sigerr ( 'SPICE(BADVERTEXCOUNT)'                       )
         return

      end if

      if ( np < 1 ) then

         call ctx%setmsg ( 'Plate count NP = #; count must be positive.' &
         //            'be positive.'                                )
         call ctx%errint ( '#',  np                                      )
         call ctx%sigerr ( 'SPICE(BADPLATECOUNT)'                        )
         return

      end if

      if ( cellsz < 3*np ) then

         call ctx%setmsg ( 'Cell array size CELLSZ = #; size must be ' &
         //            '>= 3*NP. NP is the plate count #.'         )
         call ctx%errint ( '#',  cellsz                                )
         call ctx%errint ( '#',  np                                    )
         call ctx%sigerr ( 'SPICE(CELLARRAYTOOSMALL)'                  )
         return

      end if

      if ( maxlst < (nv+3*np) ) then

         call ctx%setmsg ( 'Plate list array size MAXPLT = #; size ' &
         //            'must be >= 3*NP + NV, which is #. (NV = ' &
         //            'vertex count, NP = plate count.)'        )
         call ctx%errint ( '#',  maxlst                              )
         call ctx%errint ( '#',  3*np + nv                           )
         call ctx%sigerr ( 'SPICE(PLATELISTTOOSMALL)'                )
         return

      end if

!
!     Initialize pointer and cell structure.
!
      call zzinilnk ( ctx, nv, cellsz, ncell, vrtptr, cells )

      if ( ctx%failed() ) return

!
!     Loop over all plate IDS. Add each plate/vertex
!     combination to the linked list.
!
      do i = 1, np

         do j = 1, 3
!
!           AVAL = PLATES(J,I), vertex J of plate ID I.
!           BVAL = I, plate ID value I.
!
            call zzaddlnk ( ctx, plates(j,i), i,     nv,   cellsz, &
                            vrtptr,      ncell, cells        )

            if ( ctx%failed() ) return

         end do

      end do

!
!     Generate two linked lists mapping vertex ID to the plates
!     including that vertex as a member.
!
!     VRTPTR: An array, indexed by vertex ID. For an array element,
!             VRTPTR(VERT_ID), greater than zero, the value identifies
!             an index in PLTLST, the value of that PLTLST array
!             element  equaling the number of plates that include
!             the vertex specified by the ID as a member. The
!             condition VRTPTR(VERT_ID) = -1 indicates a bug.
!
!     PLTLST: An array, indexed by the positive entries in VRTPTR.
!             The element, N, identified by a VRTPTR value describes
!             the number of plates of which the vertex is a member.
!
!                 N = PLTLST( VRTPTR(VERT_ID) )
!
!             The N elements following PLTLST( VRTPTR(VERT_ID) ),
!             contain the IDs of those plates which have the vertex
!             as a member.
!
      call zzuntngl ( ctx, nv,     cellsz, cells, &
                      maxlst, vrtptr, nlist, pltlst )

!
!     Standard SPICE error handling.
!
      return
      end subroutine zzvrtplt

      !> Map voxel coordinates to coarse voxel coordinates and offset
      !> relative to coarse voxel. Offset coordinates are 1-based.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     VIXYZ      is an array containing integer Cartesian coordinates
      !>                of a voxel in the fine voxel grid. Coordinates are
      !>                1-based.
      !>
      !>     NVOX       is an array containing the integer extents of the fine
      !>                voxel grid. Units are voxels.
      !>
      !>     CGRSCL     is the coarse voxel scale. This is the integer ratio
      !>                of the coarse voxel edge length to the fine voxel
      !>                edge length.
      !>
      !>## Detailed Output
      !>
      !>     CGXYZ      is an array containing integer Cartesian coordinates
      !>                of the coarse voxel that contains the fine voxel
      !>                indexed by VIXYZ. Coordinates are 1-based.
      !>
      !>     CGOFF      is an array containing the integer Cartesian
      !>                coordinates of the fine voxel relative to the coarse
      !>                voxel that contains it. Coordinates are 1-based.
      !>
      !>     CGOF1D     is the 1-dimensional offset of the input fine voxel
      !>                relative to the coarse voxel that contains it. The
      !>                offset is 1-based.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If any input fine voxel grid dimension is non-positive,
      !>        the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     2) If the coarse voxel scale is non-positive, the error
      !>        SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine encapsulates a frequently used type 2 DSK address
      !>     calculation. The output offset CGOF1D is useful for locating
      !>     a fine voxel's pointer into the voxel-plate list.
      !>
      !>## Examples
      !>
      !>     See usage in the routine ZZMKSPIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB)
      !>
      !>        $Exceptions section was updated.
      !>
      !>     Based on DSKLIB Version 1.0.0, 20-MAR-2015 (NJB)
      module subroutine zzvoxcvo ( ctx, vixyz, nvox, cgrscl, cgxyz, cgoff, cgof1d )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: vixyz  ( 3 )  !! Fine voxel integer coordinates.
      integer(ip), intent(in) :: nvox   ( 3 )  !! Fine voxel grid dimensions.
      integer(ip), intent(in) :: cgrscl  !! Coarse voxel scale.
      integer(ip), intent(out) :: cgxyz  ( 3 )  !! Coarse voxel coordinates.
      integer(ip), intent(out) :: cgoff  ( 3 )  !! 3-D voxel offset relative to coarse voxel.
      integer(ip), intent(out) :: cgof1d  !! 1-D voxel offset relative to coarse voxel.

      integer(ip) :: i

      if ( ctx%failed() ) return

      do i = 1, 3

         if ( nvox(i) < 1 ) then

            call ctx%setmsg ( 'Voxel grid dimensions must be positive ' &
            //            'but were # # #.'                        )
            call ctx%errint ( '#', nvox(1)                             )
            call ctx%errint ( '#', nvox(2)                             )
            call ctx%errint ( '#', nvox(3)                             )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )

            return

         end if

      end do

      do i = 1, 3

         if ( ( vixyz(i) < 1 ) .or. ( vixyz(i) > nvox(i) ) ) then

            call ctx%setmsg ( 'Voxel grid coordinates must be inside ' &
            //            'grid having dimensions # x # x # ' &
            //            'but were # # #.'                        )
            call ctx%errint ( '#', nvox(1)                             )
            call ctx%errint ( '#', nvox(2)                             )
            call ctx%errint ( '#', nvox(3)                             )
            call ctx%errint ( '#', vixyz(1)                            )
            call ctx%errint ( '#', vixyz(2)                            )
            call ctx%errint ( '#', vixyz(3)                            )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )

            return

         end if

      end do

      if ( cgrscl < 1 ) then

         call ctx%setmsg ( 'Coarse voxel grid scale must be positive ' &
         //            'but was #.'                               )
         call ctx%errint ( '#', nvox(1)                               )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                   )

         return

      end if

      do i = 1, 3
!
!        Set the Ith coarse grid coordinate. Recall these coordinates
!        are 1-based.
!
         cgxyz(i) = ( ( vixyz(i) - 1 ) /  cgrscl  )  +  1

!
!        Set the Ith coarse grid coordinate offset. These offsets
!        are 1-based as well.
!
         cgoff(i) =  vixyz(i)  -  cgrscl*( cgxyz(i) - 1 )

      end do

!
!     Convert the coarse grid-relative offset to a relative
!     ID. The ID is a one-dimensional offset.
!
      cgof1d =    (cgoff(3)-1) * cgrscl * cgrscl &
                + (cgoff(2)-1) * cgrscl &
                +  cgoff(1)

      end subroutine zzvoxcvo

      !> The function returns the voxel index.
      !>
      !> Given a voxel's grid coordinates and the voxel grid
      !> dimensions, return the voxel's 1-dimensional index.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     VIXYZ      An integer 3-vector storing the voxel grid coordinates
      !>                for the point of interest.
      !>
      !>     NVOX       An array of three positive integers defining a voxel
      !>                grid's extents in the X, Y, and Z directions.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value of the voxel index.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     See usage in ZZMKSPIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>     E.D. Wright     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        17-JAN-2016 (NJB)
      !>
      !>           Re-wrote portions of header.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Re-ordered header sections.
      !>
      !>        23-SEP-2004 (EDW)
      !>
      !>           Recast equation to improve runtime performance.
      !>           Edited comments for clarity.
      !>
      !>           Subroutine renamed from GETXYZ.
      !>
      !>        19-FEB-2000 (JAB)
      !>
      !>           Removed SAVED variables.
      !>
      !>        04-FEB-1999 (JAB)
      module integer(ip) function zzvox2id ( ctx, vixyz, nvox )
      use pepper_kinds, only: ip
      implicit              none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nvox   ( 3 )  !! Number of voxels in each coordinate direction.
      integer(ip), intent(in) :: vixyz  ( 3 )  !! Voxel's grid coordinates stored in a three-
                                               !! dimensional vector.

!
!     Convert from voxel coordinates to voxel index. A more
!     readable form for the following function:
!
!        NX   = NVOX(1)
!        NY   = NVOX(2)
!        NXNY = NX * NY
!
!        ZZVOX2ID = VIXYZ(1) + (VIXYZ(2)-1)*NX + (VIXYZ(3)-1)*NXNY
!
!     Expressing the function in this format improves runtime
!     performance as per Horner's Rule.
!
      zzvox2id = vixyz(1) + &
                 nvox(1)*(  vixyz(2) - 1 + (vixyz(3)-1)*nvox(2)  )

      return
      end function zzvox2id

      !> Traverse an AB pool linked list, searching for all cells that are
      !> associated with a specified A-value. Store the B-values of these
      !> cells in an array.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     AVAL       is an index associated with a pool entry. AVAL
      !>                is the index of the Ith value of a finite set "A."
      !>                AVAL is an index into the PNTRS array.
      !>
      !>     MAXA       Size of PNTRS array.
      !>
      !>     PNTRS      is an array of integers acting as pointers into the
      !>                cell array. The element at index I of PNTRS points to
      !>                the head of a linked list in CELLS that associates
      !>                B-values with the index I. Here I is the "A value."
      !>
      !>                If there are no values associated with the Ith A value,
      !>                PNTRS(I) is null (-1).
      !>
      !>     CELLSZ     is the number of cells in the cell array. The array
      !>                has dimensions
      !>
      !>                   (2, CELLSZ)
      !>
      !>     CELLS      is the input cell array. The element
      !>
      !>                   CELLS ( 1, PNTRS(AVAL) )
      !>
      !>                contains a B-value associated with AVAL. The element
      !>
      !>                   CELLS ( 2, PNTRS(AVAL) )
      !>
      !>                is a pointer to the next node of the list associated
      !>                with AVAL, or is null (-1) if the list contains only
      !>                one element. The pointer element of the last list node
      !>                for AVAL is null.
      !>
      !>     MAXB       is the size of the output B-value list.
      !>
      !>## Detailed Output
      !>
      !>     NB         is the number of nodes in the list associated with
      !>                AVAL.
      !>
      !>     BLIST      is an array containing the B-values of the nodes
      !>                associated with AVAL. There is one entry in BLIST for
      !>                each node in the list associated with AVAL; the list
      !>                may contain duplicates.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the array index AVAL is less than 1 or exceeds the
      !>         declared size of the PNTR array, the error
      !>         SPICE(INDEXOUTOFRANGE) is signaled. Occurrence of this error
      !>         may result from the fine voxel scale being too small for the
      !>         data.
      !>
      !>     2)  If the size of the output BLIST array is non-positive, the
      !>         error SPICE(INVALIDSIZE) is signaled.
      !>
      !>     3)  If an element of the PNTRS array is larger than the length
      !>         CELLSZ of the cell array, the error SPICE(POINTEROUTOFRANGE)
      !>         is signaled.
      !>
      !>     4)  If the number of output list entries exceeds the size MAXB of
      !>         the output list array, the error SPICE(BARRAYTOOSMALL) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The AB list routines support manipulation of a data structure that
      !>     represents a one-to-many mapping from a set A to a set B. Each
      !>     element of set A is mapped to a set of values from B, where the
      !>     range values are represented by a linked list of entries in the
      !>     cell array.
      !>
      !>     The elements of set B associated with the Ith element of A are
      !>     stored in a linked list of cells starting with the cell
      !>     consisting of elements
      !>
      !>        CELLS(*,PNTRS(I))
      !>
      !>     The set A is really an abstraction: it's just some finite set
      !>     with size in the range 1:MAXA. The input AVAL is not a member of
      !>     A but rather just an index into A. For consistency with existing
      !>     code, the name AVAL has been retained.
      !>
      !>     The fact that B values are stored in linked lists enables a
      !>     program to store entries for A-B associations in random order.
      !>     For example, in the process of constructing DSK type 2 segments,
      !>     the of non-empty fine voxels intersected by a given plate can be
      !>     computed; then an entry representing a voxel-plate association
      !>     can be made for each fine voxel in the set. In this case, the
      !>     set "A" is the set of fine voxels belonging to non-empty coarse
      !>     voxels.
      !>
      !>     This routine supports creation of DSK type 2 segments. It is used
      !>     for creation of both voxel-plate association data structures and
      !>     of vertex-plate association data structures.
      !>
      !>## Examples
      !>
      !>     See usage in the routine ZZUNTNGL.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB)
      !>
      !>      07-JAN-2016 (NJB)
      !>
      !>        Strengthened check on invalid pointer. Renamed argument MAXCEL
      !>        to CELLSZ. Updated header comments.
      !>
      !>      26-MAR-2015 (NJB)
      !>
      !>        Updated long error messages to improve accuracy.
      !>
      !>        Header update: added description of MAXB input
      !>        argument. Updated description of MAXA input
      !>        argument. Filled out Exceptions section.
      !>
      !>      30-APR-2014 (NJB)
      !>
      !>        Changed argument list: each array argument now
      !>        has an associated argument giving its size.
      !>        Updated error checking to check array indices
      !>        against array sizes.
      !>
      !>      08-OCT-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>      11-JUN-2004 (EDW)
      !>
      !>        Added check on AVAL to ensure it's smaller
      !>        than the size of MAXA.
      !>
      !>      26-AUG-2002 (BVS)
      !>
      !>        Replaced WRITE with normal error reporting calls.
      !>
      !>      03-FEB-1999 (JAB)
      module subroutine zztrvlnk ( ctx, aval, maxa, pntrs, cellsz, cells, maxb, nb, blist )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: aval  !! A-value index associated with pool entry.
      integer(ip), intent(in) :: maxa  !! Size of PNTRS array.
      integer(ip), intent(in) :: pntrs  ( * )  !! A-value pointer array.
      integer(ip), intent(in) :: cellsz  !! Number of cells in cell array.
      integer(ip), intent(in) :: cells  ( 2, * )  !! Cell array.
      integer(ip), intent(in) :: maxb  !! Size of BLIST array.
      integer(ip), intent(out) :: nb  !! Number of B values found.
      integer(ip), intent(out) :: blist  ( * )  !! List of B values.
      integer(ip) :: i
      integer(ip) :: ptr

!
!     Standard SPICE error handling.
!

      if ( ctx%failed() ) return

      if ( ( aval < 1 ) .or. ( aval > maxa ) ) then

         call ctx%setmsg ( 'Index AVAL is out of range. ' &
         //            'Index = #1. Valid range = 1:#2.' )
         call ctx%errint ( '#1', aval                        )
         call ctx%errint ( '#2', maxa                        )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'          )
         return

      end if

      if ( maxb < 1 ) then

         call ctx%setmsg ( 'Maximum output list size MAXB is invalid. ' &
         //            'MAXB = #1.'                                )
         call ctx%errint ( '#1', maxb                                  )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'                        )
         return

      end if

      nb       = 0
      blist(1) = 0

      ptr = pntrs(aval)

      do while ( ptr /= -1 )

         if (      ( ptr < -1     ) &
              .or. ( ptr == 0      ) &
              .or. ( ptr > cellsz )  ) then

            call ctx%setmsg ( 'Value in PNTRS array is not a ' &
            //            'valid index in the cell array.' &
            //            'Value = #1. Array size = #2.'    )
            call ctx%errint ( '#1', ptr                         )
            call ctx%errint ( '#2', cellsz                      )
            call ctx%sigerr ( 'SPICE(POINTEROUTOFRANGE)'        )
            return

         end if

         nb = nb + 1

         if ( nb > maxb ) then

            call ctx%setmsg ( 'Output value count is larger than ' &
            //            'B-list array room. Count = #1. Output ' &
            //            'array room = #2. Input pointer index ' &
            //            'was #3. Input pointer list size was #4. ' &
            //            'Last pointer was #5. Cell size was #6.'  )
            call ctx%errint ( '#1', nb                                  )
            call ctx%errint ( '#2', maxb                                )
            call ctx%errint ( '#3', aval                                )
            call ctx%errint ( '#4', maxa                                )
            call ctx%errint ( '#5', ptr                                 )
            call ctx%errint ( '#6', cellsz                              )
            call ctx%sigerr ( 'SPICE(BARRAYTOOSMALL)'                   )
            return

         end if

         blist(nb) = cells(1,ptr)
         i         = cells(2,ptr)
         ptr       = i

      end do

!
!     Standard SPICE error handling.
!
      return
      end subroutine zztrvlnk

      !> Untangle an unsorted AB linked-cell list into an A-index and
      !> associated B-list.
      !>
      !>## Detailed Input
      !>
      !>     NPTR       is the size of the input pointer array INPTR.
      !>
      !>     MAXCEL     is the number of cells in the CELLS array. That
      !>                array has dimensions
      !>
      !>                   (2, MAXCEL)
      !>
      !>     CELLS      is an array containing linked lists of "B" values.
      !>                The elements at indices
      !>
      !>                   (1,K)
      !>                   (2,K)
      !>
      !>                are, respectively, a "B" value and a pointer to
      !>                another element of the array. Null pointers are
      !>                indicated by the value -1.
      !>
      !>                Each linked list in CELLS contains a set of "B"
      !>                values associated with a particular "A" value.
      !>                The input pointer list maps "A" values to the head
      !>                of the associated list in CELLS.
      !>
      !>     MAXB       is the maximum number of elements in the output
      !>                array OUTLST.
      !>
      !>     PNTRS      is an array of pointers from A values to
      !>                linked lists of "B" entries in the CELLS array.
      !>
      !>## Detailed Output
      !>
      !>     PNTRS      is an array of pointers that map "A" values to
      !>                their associated "B" values in the array OUTLST.
      !>                Null pointers are indicated by the value -1.
      !>
      !>                PNTRS must be declared with size at least NPTR.
      !>
      !>     NOUT       is the number of elements in the output list OUTLST.
      !>
      !>     OUTLST     is an array containing lists of "B" values. The Kth
      !>                element of PNTRS is the index of the start of a list
      !>                in OUTLST of "B" values associated with the "A" value
      !>                having index K. The list of values associated with an
      !>                "A" value starts with a count and is followed by
      !>                the values themselves.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The routine signals the error SPICE(BARRAYTOOSMALL) if the
      !>         entire set of B-value lists from the input cell array,
      !>         including the counts for each list, array cannot be stored in
      !>         the OUTLST array.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The AB list routines support manipulation of a data structure that
      !>     represents a one-to-many mapping from a set A to a set B. Each
      !>     element of set A is mapped to a set of values from B, where the
      !>     range values are represented by a linked list of entries in the
      !>     cell array.
      !>
      !>     The elements of set B associated with the Ith element of A are
      !>     stored in a linked list of cells starting with the cell
      !>     consisting of elements
      !>
      !>        CELLS(*,PNTRS(I))
      !>
      !>     The set A is really an abstraction: it's just some finite set
      !>     with size in the range 1:MAXA. The input AVAL is not a member of
      !>     A but rather just an index into A. For consistency with existing
      !>     code, the name AVAL has been retained.
      !>
      !>     The fact that B values are stored in linked lists enables a
      !>     program to store entries for A-B associations in random order.
      !>     For example, in the process of constructing DSK type 2 segments,
      !>     the of non-empty fine voxels intersected by a given plate can be
      !>     computed; then an entry representing a voxel-plate association
      !>     can be made for each fine voxel in the set. In this case, the
      !>     set "A" is the set of fine voxels belonging to non-empty coarse
      !>     voxels.
      !>
      !>     This routine supports creation of DSK type 2 segments. It is used
      !>     for creation of both voxel-plate association data structures and
      !>     of vertex-plate association data structures.
      !>
      !>## Examples
      !>
      !>     See usage in ZZMKSPIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB)
      !>
      !>      07-JAN-2016 (NJB)
      !>
      !>        Argument list change: removed INPTR; renamed
      !>        OUTPTR to PNTRS. PNTRS is an in-out argument.
      !>        Updated header.
      !>
      !>      30-APR-2014 (NJB)
      !>
      !>        Changed argument list to accommodate
      !>        better error checking. Added error checks
      !>        for array overflows. Changed calls to
      !>        ADDLNK and TRVLNK. Updated header I/O
      !>        descriptions.
      !>
      !>      08-OCT-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>      16-JUL-2004 (EDW)
      !>
      !>        Added check on NPTR to ensure it's smaller
      !>        than the size of the pointer arrays.
      !>
      !>        Removed use of BVAL array, TRVLNK now directly
      !>        writes to OUTLST array.
      !>
      !>      03-FEB-1999 (JAB)
      module subroutine zzuntngl ( ctx, nptr, maxcel, cells, maxb, pntrs, nout, outlst )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nptr  !! Length of A-list.
      integer(ip), intent(in) :: maxcel  !! Size of cell list.
      integer(ip), intent(in) :: cells  ( 2, * )  !! AB cell linked list.
      integer(ip), intent(in) :: maxb  !! Maximum allowed dimension of B list.
      integer(ip), intent(inout) :: pntrs  ( * )  !! Pointer array.
      integer(ip), intent(out) :: nout  !! Length of B-list.
      integer(ip), intent(out) :: outlst ( * )  !! B-list.
      integer(ip) :: aval
      integer(ip) :: ptrdex
      integer(ip) :: nfound
      integer(ip) :: room

!
!     Standard SPICE error handling.
!

      if ( ctx%failed() ) return

      if ( nptr > maxcel ) then

         call ctx%setmsg ( 'Input pointer array is larger than ' &
         //            'cell array. Pointer array size = ' &
         //            '#1. Cell array size = #2.'          )
         call ctx%errint ( '#1', nptr                           )
         call ctx%errint ( '#2', maxcel                         )
         call ctx%sigerr ( 'SPICE(BARRAYTOOSMALL)'              )
         return

      end if

!
!     ROOM is the remaining room in the output list.
!
      room = maxb

!
!     Initialize pointer index.
!
      ptrdex = 0

!
!     Loop over all A-values.
!
      do aval = 1, nptr
!
!        Traverse the chained list for a particular A-value
!        and collect associated B-values. If B-values exists,
!        return the number of B-vals to the OUTLST array
!        at element PTRDEX + 1; return the list of B-vals
!        starting at element PTRDEX + 2.
!
!        Make sure the output pointers below are in range.
!
         if ( ptrdex+2 > maxb ) then

            call ctx%setmsg ( 'Index larger than output array. ' &
            //            'Index = #1. Array size = #2.'     )
            call ctx%errint ( '#1', ptrdex+2                     )
            call ctx%errint ( '#2', maxb                         )
            call ctx%sigerr ( 'SPICE(BARRAYTOOSMALL)'            )
            return

         end if

         if ( room <= 0 ) then

            call ctx%setmsg ( 'Remaining room in output array is #1. ' &
            //            'Current input pointer index = #2. Output ' &
            //            'array size = #3. Output pointer index ' &
            //            'is #4.'                                  )
            call ctx%errint ( '#1', room                                )
            call ctx%errint ( '#2', aval                                )
            call ctx%errint ( '#3', maxb                                )
            call ctx%errint ( '#4', ptrdex                              )
            call ctx%sigerr ( 'SPICE(BARRAYTOOSMALL)'                   )
            return

         end if

         call zztrvlnk ( ctx, aval,    nptr,  pntrs, &
                         maxcel,  cells, room, &
                         outlst(ptrdex + 1), &
                         outlst(ptrdex + 2)     )

         if ( ctx%failed() ) return

!
!        Increment pointer PTRDEX if we found any B-vals.
!
         nfound = outlst(ptrdex + 1)

         if ( nfound > 0 ) then
!
!           Store in PNTRS the pointer to the returned list.
!           This assignment overwrites the input pointer from
!           AVAL to the head of the associated list in CELLS.
!
            pntrs(aval) = ptrdex + 1
!
!           Update the count of available spaces in the
!           output list. Account for the list size stored
!           at the front of the list.
!
            room = room - ( 1 + nfound )
!
!           Increment PTRDEX to mark the position of the final
!           B-val.
!
            ptrdex = ptrdex + 1 + nfound

         else
!
!           If no associated B-values exist, set the
!           PNTRS element to -1, indicating no B-value.
!
            pntrs(aval) = -1

         end if

      end do

!
!     Return the current pointer value.
!
      nout = ptrdex

!
!     Standard SPICE error handling.
!
      return
      end subroutine zzuntngl

      !> Initialize an AB cell linked-list structure.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     MAXP       is the length of the pointer array PNTRS.
      !>
      !>     MAXC       is the length of the cell array CELLS. The array has
      !>                dimensions
      !>
      !>                   ( 2, MAXC )
      !>
      !>                Elements
      !>
      !>                   ( 1, * )
      !>
      !>                normally contain "A-values"; elements
      !>
      !>                   ( 2, * )
      !>
      !>                normally contain pointers.
      !>
      !>## Detailed Output
      !>
      !>     NCELL      is the number of cells in use; this argument is
      !>                initialized to zero. This is done as a convenience
      !>                for the calling routine, which usually must keep
      !>                track of the number of cells in use.
      !>
      !>     PNTRS      is the pointer array, initialized so that each
      !>                element contains -1, which represents a null
      !>                pointer.
      !>
      !>     CELLS      is a cell array, initialized so that each pointer
      !>                element contains -1, and so that each value
      !>                element contains 0.
      !>
      !>## Parameters
      !>
      !>     See the DSK type 2 include file
      !>
      !>        dsk02.inc
      !>
      !>## Exceptions
      !>
      !>     1)  If the pointer array size MAXP is non-positive, the
      !>         error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     2)  If the cell array size MAXC is non-positive, the
      !>         error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine supports creation of DSK type 2 segments.
      !>     It is used for creation of both voxel-plate association
      !>     data structures and of vertex-plate association data
      !>     structures.
      !>
      !>## Examples
      !>
      !>     See usage in ZZMKSPIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        05-JAN-2016 (NJB)
      !>
      !>           Added error checks on input dimension arguments. Re-wrote
      !>           some of the argument descriptions. Added private routine
      !>           warning to header abstract.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Re-ordered header sections.
      !>
      !>        03-FEB-1999 (JAB)
      module subroutine zzinilnk ( ctx, maxp, maxc, ncell, pntrs, cells )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: maxp  !! Length of A-value array.
      integer(ip), intent(in) :: maxc  !! Number of cells.
      integer(ip), intent(out) :: ncell  !! Number of cells in use; initialized to zero.
      integer(ip), intent(out) :: pntrs  ( * )  !! Initialized pointer array.
      integer(ip), intent(out) :: cells  ( 2, * )  !! Initialized cell array.

      integer(ip) :: i

      if ( ctx%failed() ) return

      if ( maxp < 1 ) then

         call ctx%setmsg ( 'Pointer array size MAXP = #; size must ' &
         //            'be positive.'                           )
         call ctx%errint ( '#',  maxp                               )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         return

      end if

      if ( maxc < maxp ) then

         call ctx%setmsg ( 'Cell array size MAXC = #; size must ' &
         //            'be at least as large as pointer array ' &
         //            'size #.'                                )
         call ctx%errint ( '#',  maxc                               )
         call ctx%errint ( '#',  maxp                               )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         return

      end if

!
!     Initialize pointer array and cells.
!
      do i = 1, maxp
         pntrs(i)   = -1
      end do

      do i = 1, maxc
         cells(1,i) =  0
         cells(2,i) = -1
      end do

!
!     Set count of cells in use to 0, for the convenience
!     of the calling routine.
!
      ncell = 0

      return
      end subroutine zzinilnk

      !> This subroutine returns the 1-based voxel coordinates of the
      !> voxel that contains a given point.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     VOXSIZ     is a double precision scalar value giving the size of
      !>                each cubical voxel in model units.
      !>
      !>     VOXORI     is a double precision 3-vector indicating the location
      !>                of the voxel grid origin in model units.
      !>
      !>     NVOX       is an integer 3-vector giving the length of each
      !>                side of the voxel grid in voxel units.
      !>
      !>     XYZ        is a double precision 3-vector containing the location
      !>                of the point in question in model units.
      !>
      !>## Detailed Output
      !>
      !>     INBOX      is a logical scalar indicating whether or not the
      !>                point XYZ lies within the extent of the voxel grid.
      !>
      !>     VOXCOR     is an integer 3-vector containing the x, y, and z
      !>                voxel indices of the voxel containing the point.
      !>
      !>                VOXCOR is valid only if INBOX is .TRUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If VOXSIZ is less than or equal to zero, the error
      !>         SPICE(NONPOSITIVEVALUE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is not a general coordinate conversion routine. This routine
      !>     returns valid voxel coordinates only for voxels inside the voxel
      !>     grid.
      !>
      !>     Points on the outer surface of the grid, as defined by the voxel
      !>     origin and the grid dimensions, are considered to be inside the
      !>     grid.
      !>
      !>     Points outside of the grid are not mapped to voxel coordinates.
      !>
      !>## Examples
      !>
      !>   C
      !>   C     Some position in model coordinates, i.e. the body fixed
      !>   C     units and frame coordinates.
      !>   C
      !>         DOUBLE PRECISION       POS(3)
      !>
      !>               ...
      !>
      !>   C
      !>   C     Retrieve the voxel grid geometry description.
      !>   C
      !>         CALL VOXDIM ( NVOX, VOXSIZ, VOXORI)
      !>
      !>   C
      !>   C     Convert the coordinate POS to voxel grid indices.
      !>   C
      !>         CALL [[zzgetvox]] ( VOXSIZ, VOXORI, NVOX, POS, INBOX, VOXCOR )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>     E.D. Wright     (JPL
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 18-MAR-2016 (NJB)
      !>
      !>        Added error check for non-positive voxel size.
      !>        Made small changes to header.
      !>
      !>        17-JAN-2016 (NJB)
      !>
      !>           Functional change: routine returns valid voxel coordinates
      !>           only for voxels inside the voxel grid. Points on the outer
      !>           surface of the grid are considered to be inside the grid.
      !>
      !>           Changed check-in to discovery style.
      !>
      !>           Corrected rounding for voxels on outer edge of grid.
      !>           Updated header.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Re-ordered header sections.
      !>
      !>        14-SEP-2004 (EDW)
      !>
      !>           Corrected VOXID(I) functions to allow for
      !>           negative voxel coordinates. Original functions
      !>           returned values GE 1.
      !>
      !>           Added Examples section.
      !>
      !>        28-JAN-1999 (JAB)
      !>
      !>           Original version.
      module subroutine zzgetvox ( ctx, voxsiz, voxori, nvox, xyz, inbox, voxcor )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: voxsiz  !! Voxel size in model units.
      real(wp), intent(in) :: voxori    ( 3 )  !! 3-Vector location of the voxel grid origin in
                                               !! model units.
      integer(ip), intent(in) :: nvox      ( 3 )  !! Number of voxel in each dimension.
      real(wp), intent(in) :: xyz       ( 3 )  !! Model grid coordinates of point.
      logical, intent(out) :: inbox  !! Logical value is true if point is inside voxel grid,
                                     !! false if not.
      integer(ip), intent(out) :: voxcor     ( 3 )  !! 3-Index of voxel location with the voxel
                                                    !! grid.

!
!     Local variables.
!
      real(wp) :: term
      integer(ip) :: i

      if ( ctx%failed() ) return

      if ( voxsiz <= 0.0_wp ) then

         call ctx%setmsg ( 'Voxel size was #; must be positive.' )
         call ctx%errdp  ( '#',  voxsiz                          )
         call ctx%sigerr ( 'SPICE(NONPOSITIVEVALUE)'             )
         return

      end if

!
!     Initialize 'point in box' flag and voxel coordinates. The
!     coordinates are assigned out-of-range values.
!
      inbox     = .false.

      voxcor(1) = 0
      voxcor(2) = 0
      voxcor(3) = 0

!
!     Scale the point's coordinates to voxel grid space
!     and determine the indices of the voxel that contains it.
!
      do i = 1, 3
!
!        A Galilean transform. Calculate the voxel coordinate
!        corresponding to the body centered coordinate. This
!        operation performs the same task as TOGRID, but
!        including the operation here improves ZZGETVOX's
!        runtime performance.
!
         term = ( xyz(i) - voxori(i) ) / voxsiz

!
!        Calculate the voxel index for each degree of freedom
!        corresponding to the voxel coordinate.
!
!        If the point is outside of the grid, return now.
!
         if ( ( term < 0.0_wp ) .or. ( term > nvox(i) ) ) return

!
!        Assign a 1-based value to the Ith component of the voxel's
!        coordinates. The outer surface of the grid is considered part
!        of the grid.
!
!        Note that TERM is non-negative at this point.
!
         if ( int(term) < nvox(i) ) then

            voxcor(i) =  1 + int(term)

         else
!
!           TERM is NVOX(I), since the cases
!
!              TERM > NVOX(I)
!              TERM < NVOX(I)
!
!           have been ruled out.
!
            voxcor(i) = nvox(i)

         end if

      end do

      inbox = .true.

      return
      end subroutine zzgetvox

      !> Add a new AB cell to an AB cell pool and update the A-value
      !> pointer array.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     AVAL       is an index associated with a new pool entry. AVAL
      !>                is the index of the Ith value of a finite set "A."
      !>                AVAL is an index into the PNTRS array.
      !>
      !>     BVAL       is a B-value associated with AVAL. A new cell is to be
      !>                allocated to associate AVAL and BVAL.
      !>
      !>     MAXA       Size of pointer array. AVAL must not exceed
      !>                this value.
      !>
      !>     CELLSZ     Number of cells in the cell array CELLS. The array
      !>                has dimension (2, CELLSZ).
      !>
      !>     PNTRS      is an array of integers acting as pointers into the
      !>                cell array. The element at index I of PNTRS points to
      !>                the head of a linked list in CELLS that associates
      !>                B-values with the index I. Here I is the "A value."
      !>
      !>     NCELL      Number of cells in use on input. NCELL is incremented
      !>                by one on output.
      !>
      !>     CELLS      Cell array, updated on output.
      !>
      !>## Detailed Output
      !>
      !>     PNTRS      is the input pointer array, modified to reflect the
      !>                addition of a new cell entry. The element
      !>
      !>                   PNTRS(AVAL)
      !>
      !>                contains the second subscript in CELLS of the new
      !>                entry.
      !>
      !>     NCELL      Number of cells in use on output. NCELL is incremented
      !>                by one relative to its input value.
      !>
      !>     CELLS      is the input cell array, with a new entry. The element
      !>
      !>                   CELLS ( 1, PNTRS(AVAL) )
      !>
      !>                contains BVAL. The element
      !>
      !>                   CELLS ( 2, PNTRS(AVAL) )
      !>
      !>                is a pointer to the previous head of the list
      !>                associated with AVAL, or is null (-1) if the list was
      !>                previously empty.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the array index AVAL exceeds the size MAXA of the
      !>         A-value pointer array, the error SPICE(AVALOUTOFRANGE) is
      !>         signaled.
      !>
      !>         Occurrence of this error in the context of DSK type 2 spatial
      !>         index creation can result from the fine voxel scale having
      !>         been set to a value too small for the plate set.
      !>
      !>     2)  If the required cell count exceeds the size CELLSZ of the
      !>         cell array, the error SPICE(CELLARRAYTOOSMALL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The AB list routines support manipulation of a data structure that
      !>     represents a one-to-many mapping from a set A to a set B. Each
      !>     element of set A is mapped to a set of values from B, where the
      !>     range values are represented by a linked list of entries in the
      !>     cell array.
      !>
      !>     The elements of set B associated with the Ith element of A are
      !>     stored in a linked list of cells starting with the cell
      !>     consisting of elements
      !>
      !>        CELLS(*,PNTRS(I))
      !>
      !>     The set A is really an abstraction: it's just some finite set
      !>     with size in the range 1:MAXA. The input AVAL is not a member of
      !>     A but rather just an index into A. For consistency with existing
      !>     code, the name AVAL has been retained.
      !>
      !>     The fact that B values are stored in linked lists enables a
      !>     program to store entries for A-B associations in random order.
      !>     For example, in the process of constructing DSK type 2 segments,
      !>     the of non-empty fine voxels intersected by a given plate can be
      !>     computed; then an entry representing a voxel-plate association
      !>     can be made for each fine voxel in the set. In this case, the
      !>     set "A" is the set of fine voxels belonging to non-empty coarse
      !>     voxels.
      !>
      !>     This routine supports creation of DSK type 2 segments. It is used
      !>     for creation of both voxel-plate association data structures and
      !>     of vertex-plate association data structures.
      !>
      !>## Examples
      !>
      !>     See usage in ZZMKSPIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     J.A. Bytof      (JPL)
      !>     E.D. Wright     (JPL)
      !>
      !>## Version
      !>
      !>-    MKDSK Version 2.1.0, 15-JAN-2016 (NJB)
      !>
      !>        Updated error check on AVAL.
      !>
      !>        Filled in or update various header entries.
      !>
      !>-    MKDSK Version 2.0.0, 30-APR-2014 (NJB)
      !>
      !>        Argument list change: now accepts separate inputs for pointer
      !>        list size and cell size.
      !>
      !>-    MKDSK Version 1.2.0, 04-MAY-2010 (NJB)
      !>
      !>        Changed INCLUDE file to dsk02.inc.
      !>
      !>-    MKDSK Version 1.1.1, 08-OCT-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    MKDSK Version 1.1.0, 11-JUN-2004 (EDW)
      !>
      !>        Added check on AVAL to ensure it's smaller
      !>        than the size of CELLSZ.
      !>
      !>-    MKDSK Version 1.0.0, 03-FEB-1999 (JAB)
      module subroutine zzaddlnk ( ctx, aval, bval, maxa, cellsz, pntrs, ncell, cells )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: aval  !! A-value index associated with new pool entry.
      integer(ip), intent(in) :: bval  !! B-value associated with input A value.
      integer(ip), intent(in) :: maxa  !! Size of pointer array.
      integer(ip), intent(in) :: cellsz  !! Number of cells in cell array.
      integer(ip), intent(inout) :: pntrs  ( * )  !! A-value pointer array to update.
      integer(ip), intent(inout) :: ncell  !! Number of cells in AB cell linked-list.
      integer(ip), intent(inout) :: cells  ( 2, * )  !! Cell array.
      if ( ctx%failed() ) return

!
!     Test the pointer array index AVAL.
!
      if ( ( aval < 1 ) .or. ( aval > maxa ) ) then

         call ctx%setmsg ( 'Index AVAL is out of range. AVAL = #1; ' &
         //            'valid range is 1:#2.'                   )
         call ctx%errint ( '#1', aval                               )
         call ctx%errint ( '#2', maxa                               )
         call ctx%sigerr ( 'SPICE(AVALOUTOFRANGE)'                  )
         return

      end if

!
!     Increment the cell counter.
!
      ncell = ncell + 1

      if ( ncell > cellsz ) then

         call ctx%setmsg ( 'NCELL larger than cell array. ' &
         //            'Cell index = #1. Array size = #2.' )
         call ctx%errint ( '#1', ncell                         )
         call ctx%errint ( '#2', cellsz                        )
         call ctx%sigerr ( 'SPICE(CELLARRAYTOOSMALL)'          )
         return

      end if

!
!     Update the cell address of the last occurrence of the A-value,
!     if any. If none, PNTRS(AVAL) has value -1.
!
      cells(1,ncell) = bval
      cells(2,ncell) = pntrs(aval)

      pntrs(aval)    = ncell

      return
      end subroutine zzaddlnk

      !> Create voxel grid data structure and voxel-plate mapping.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NP         Total number of plates in model.
      !>
      !>     PLATES     Array containing indices in the array VRTCES of
      !>                the vertices corresponding to each plate. The
      !>                elements (1:3,I) of PLATES are the vertex indices
      !>                of the Ith plate.
      !>
      !>     VRTCES     Array containing cartesian coordinates for all
      !>                vertices in the model reference frame. Elements
      !>                (1:3,I) of VRTCES are the coordinates of the Ith
      !>                vertex.
      !>
      !>     VOXSCL     The voxel size is determined by the average extent of
      !>                each plate in XYZ. VOXSCL is a scaling factor that
      !>                may be used to adjust final voxel size.
      !>
      !>     CGSCAL     The coarse voxel grid has voxel edge length
      !>                larger by factor CGSCAL than the fine voxel
      !>                edge length.
      !>
      !>     MAXPTR     is the maximum number of elements in the output
      !>                list of voxel pointers.
      !>
      !>     MXCELL     is the number of cells in the input cell array.
      !>                This is the second dimension of the array.
      !>
      !>     MAXVXL     is the maximum number of elements in the output
      !>                voxel-plate list.
      !>
      !>     CELLS      workspace array used to construct the voxel-plate
      !>                mapping.
      !>
      !>## Detailed Output
      !>
      !>     CELLS      workspace array used to construct the voxel-plate
      !>                mapping.
      !>
      !>     NVOX       Dimensions of the voxel grid in voxel units.
      !>
      !>     VOXSIZ     Size of each voxel in model units (km).
      !>
      !>     VOXORI     Origin of voxel grid in model units (km).
      !>
      !>     NVXTOT     Total number of voxels in grid.
      !>
      !>     NVXPTR     Number of pointers in VXPTR array.
      !>
      !>     VXPTR      Array of pointers that map voxels to their associated
      !>                plated IDs in the voxel to plates list. The Nth
      !>                element of this array contains a pointer to the Mth
      !>                element of VXLIST, or -1. If VXPTR(N) is -1, the Nth
      !>                voxel is empty.
      !>
      !>     NVXLST     The total number of elements in the plate list.
      !>
      !>     VXLIST     The plates list. For the Nth voxel, VXPTR(N) points to
      !>                VXLIST(M). VXLIST(M) contains the number plates that
      !>                intersect voxel N. The IDs of those plates are stored
      !>                in the following elements.
      !>
      !>     EXTENT     is an array of extents of vertex coordinates. Elements
      !>                indexed (2*I)-1 and 2*I are the minimum and maximum
      !>                values of the Ith coordinate, taken over all vertices.
      !>                Units are km.
      !>
      !>     CGRPTR     is an of array coarse voxel grid pointers. Null
      !>                pointers have the value -1. Non-null pointers indicate
      !>                locations in the voxel pointer array: each non-empty
      !>                coarse voxel points to a list of pointers for each of
      !>                the fine voxels it contains.
      !>
      !>## Parameters
      !>
      !>     See the include file dsk02.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  If NP is less than 1 or greater than MAXPLT, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     2)  If the number of coarse voxels exceeds the grid size MAXCGR,
      !>         the error SPICE(COARSEGRIDOVERFLOW) is signaled.
      !>
      !>     3)  If the voxel count is greater than MAXVOX, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     4)  If the coarse voxel count is less than 1 or greater than
      !>         MAXCGR, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     5)  If the coarse voxel scale is less than 1 or more than
      !>         the cube root of the fine voxel count, the error
      !>         SPICE(VALUEOUTOFRANGE) will be signaled.
      !>
      !>     6)  If the cube of the coarse voxel scale does not divide the
      !>         fine voxel count evenly, the error SPICE(INCOMPATIBLESCALE)
      !>         will be signaled.
      !>
      !>     7)  If the workspace overflows while this routine accumulates
      !>         voxel-plate associations, the error will be signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>     8)  If the voxel-plate association list array overflows while
      !>         this routine accumulates voxel-plate associations, the error
      !>         will be signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine supports creation of spatial indexes for DSK type 2
      !>     segments. This routine determines the fine and coarse voxel
      !>     grid dimensions and parameters. It also builds the voxel-plate
      !>     association data structures.
      !>
      !>## Examples
      !>
      !>     See usage in DSKMI2.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman  (JPL)
      !>     J.A. Bytof    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 17-FEB-2017 (NJB)
      !>
      !>        Added new error checks.
      !>
      !>        29-JAN-2016 (NJB)
      !>
      !>           Removed reference to ZZVOXPAD. Renamed argument MAXCEL to
      !>           MXCELL to accommodate declaration of MAXCEL in dsk02.inc.
      !>
      !>           Updated long error messages and header comments.
      !>
      !>        26-MAR-2015 (NJB)
      !>
      !>           Functional change: now fills in the coarse voxel grid
      !>           pointer array with valid pointers; this is done in one
      !>           pass. The old version of the routine only marked the
      !>           elements of this array as null or non-null.
      !>
      !>           The algorithm for computing the set of voxels containing
      !>           the bounding box of each plate was re-written to improve
      !>           efficiency.
      !>
      !>        30-JUN-2014 (NJB)
      !>
      !>           Argument list change: removed work space array PNTRS. The
      !>           pointer array for the voxel-to-plate list mapping is now
      !>           constructed in place.
      !>
      !>           Changed argument list to include sizes of arrays. Changed
      !>           error handling to make use of array sizes. Changed call to
      !>           UNTNGL to accommodate argument list change in that routine.
      !>           Updated header I/O descriptions.
      !>
      !>        13-MAY-2010 (NJB)
      !>
      !>           Now accepts input workspace arrays PNTRS and CELLS.
      !>           Replaced argument VERT with arguments VRTCES and PLATES;
      !>           this was done to greatly reduce the memory needed by this
      !>           program.
      !>
      !>           Changed INCLUDE file to dsk02.inc.
      !>
      !>           Bug fix: determination of empty voxels is now based on
      !>           voxel pointers, not their target values.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Re-ordered header sections.
      !>
      !>        12-SEP-2004 (EDW)
      !>
      !>           Improve/expand comments and descriptions.
      !>
      !>        03-FEB-1999 (JAB)
      !>
      !>           Original version.
      !>
      !>## Revisions
      !>
      !>        20-MAR-2015 (NJB)
      !>
      !>        Functional change: now fills in the coarse voxel grid pointer
      !>        array with valid pointers; this is done in one pass. The old
      !>        version of the routine only marked the elements of this array
      !>        as null or non-null.
      !>
      !>        The maximum size of the voxel-plate pointer array has been
      !>        reduced to MAXVOX/2.
      !>
      !>        The array mapping voxels to the voxel-plate pointer array is
      !>        no longer used. Previously, voxel IDs were used as the
      !>        "A list" elements with which plate lists were associated.
      !>        Now, indices in the voxel-plate pointer array are used as
      !>        "A" values.
      module subroutine zzmkspin ( ctx, np, plates, vrtces, voxscl, cgscal, maxptr, mxcell, maxvxl, cells, nvox, voxsiz, voxori, nvxtot, nvxptr, vxptr, nvxlst, vxlist, extent, cgrptr )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, dpmin
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: np  !! Number of plates.
      integer(ip), intent(in) :: plates ( 3, * )  !! Array of plates.
      real(wp), intent(in) :: vrtces ( 3, * )  !! Array of vertices.
      real(wp), intent(in) :: voxscl  !! Voxel scaling factor.
      integer(ip), intent(in) :: cgscal  !! Coarse voxel grid scaling factor.
      integer(ip), intent(in) :: maxptr  !! Maximum voxel pointer list size.
      integer(ip), intent(in) :: mxcell  !! Cell array column dimension.
      integer(ip), intent(in) :: maxvxl  !! Maximum voxel-plate list size.
      integer(ip), intent(inout) :: cells   ( 2, mxcell )  !! Workspace array for vertex list
                                                           !! construction.
      integer(ip), intent(out) :: nvox   ( 3 )  !! Dimensions of voxel grid.
      real(wp), intent(out) :: voxsiz  !! Size of a voxel in model units.
      real(wp), intent(out) :: voxori ( 3 )  !! Origin of voxel grid in model units.
      integer(ip), intent(out) :: nvxtot  !! Total number of voxels in grid.
      integer(ip), intent(out) :: nvxptr  !! Number of pointers in VXPTR array.
      integer(ip), intent(out) :: vxptr  ( * )  !! Pointer index to the voxel to plates list.
      integer(ip), intent(out) :: nvxlst  !! Length of voxel to plates list.
      integer(ip), intent(out) :: vxlist ( * )  !! Voxel to plates list.
      real(wp), intent(out) :: extent ( 6 )  !! Vertex coordinate extents in km.
      integer(ip), intent(out) :: cgrptr ( * )  !! Coarse voxel grid pointer array.
!
!     Fraction of voxel edge length used a tolerance for plate
!     inclusion in voxels:
!
      real(wp), parameter :: tol = 1.0e-3_wp

      real(wp) :: avext
      real(wp) :: bxmax
      real(wp) :: bxmin
      real(wp) :: bymax
      real(wp) :: bymin
      real(wp) :: bzmax
      real(wp) :: bzmin
      real(wp) :: cvxsiz
      real(wp) :: mdltol
      real(wp) :: vmod   ( 3 )
      real(wp) :: xextnt ( 6 )
      real(wp) :: xmax
      real(wp) :: xmin
      real(wp) :: xp     ( 3 )
      real(wp) :: xvmax
      real(wp) :: xvmin
      real(wp) :: ymax
      real(wp) :: ymin
      real(wp) :: yp     ( 3 )
      real(wp) :: yvmax
      real(wp) :: yvmin
      real(wp) :: zmax
      real(wp) :: zmin
      real(wp) :: zp     ( 3 )
      real(wp) :: zvmax
      real(wp) :: zvmin

      integer(ip) :: cgof1d
      integer(ip) :: cgoff  ( 3 )
      integer(ip) :: cgrdim ( 3 )
      integer(ip) :: cgxyz  ( 3 )
      integer(ip) :: cvid
      integer(ip) :: gxmax
      integer(ip) :: gxmin
      integer(ip) :: gymax
      integer(ip) :: gymin
      integer(ip) :: gzmax
      integer(ip) :: gzmin
      integer(ip) :: i
      integer(ip) :: ix
      integer(ip) :: ixptr
      integer(ip) :: iy
      integer(ip) :: iz
      integer(ip) :: j
      integer(ip) :: ncell
      integer(ip) :: ncgflg
      integer(ip) :: npcg
      integer(ip) :: nx
      integer(ip) :: ny
      integer(ip) :: nz
      integer(ip) :: q
      integer(ip) :: r
      integer(ip) :: to
      integer(ip) :: vixyz  ( 3 )
      integer(ip) :: vcoord ( 3 )

      logical :: inbox

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check NP.
!
      if (  ( np < 1 ) .or. ( np > maxplt )  ) then

         call ctx%setmsg ( 'Plate count NP = #; count must ' &
         //            'be in the range 1:#.'            )
         call ctx%errint ( '#',  np                          )
         call ctx%errint ( '#',  maxplt                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'          )
         return

      end if

!
!     Make sure the coarse voxel scale is positive. We'll
!     perform additional checks later on. Those checks
!     require computations that can't be done if the coarse
!     scale is zero.
!
      if ( cgscal < 1 ) then

         call ctx%setmsg ( 'Coarse voxel scale = #; scale must ' &
         //            'be positive.'                       )
         call ctx%errint ( '#',  cgscal                         )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'             )
         return

      end if

!
!     Get the average extents of all plates and the
!     overall model extent. The extents have model units; in
!     other words km.
!
      avext = 0.0_wp

      xmin =  dpmax
      xmax =  dpmin
      ymin =  dpmax
      ymax =  dpmin
      zmin =  dpmax
      zmax =  dpmin

      do i = 1, np

         bxmin = dpmax
         bxmax = dpmin
         bymin = dpmax
         bymax = dpmin
         bzmin = dpmax
         bzmax = dpmin

         xp(1) = vrtces( 1, plates(1,i) )
         xp(2) = vrtces( 1, plates(2,i) )
         xp(3) = vrtces( 1, plates(3,i) )

         yp(1) = vrtces( 2, plates(1,i) )
         yp(2) = vrtces( 2, plates(2,i) )
         yp(3) = vrtces( 2, plates(3,i) )

         zp(1) = vrtces( 3, plates(1,i) )
         zp(2) = vrtces( 3, plates(2,i) )
         zp(3) = vrtces( 3, plates(3,i) )

         do j = 1, 3
!
!           Determine plate extents.
!
            bxmin = dmin1( bxmin, xp(j) )
            bxmax = dmax1( bxmax, xp(j) )

            bymin = dmin1( bymin, yp(j) )
            bymax = dmax1( bymax, yp(j) )

            bzmin = dmin1( bzmin, zp(j) )
            bzmax = dmax1( bzmax, zp(j) )

!
!           Determine model extent.
!
            xmin = dmin1 ( xmin, bxmin )
            xmax = dmax1 ( xmax, bxmax )

            ymin = dmin1 ( ymin, bymin )
            ymax = dmax1 ( ymax, bymax )

            zmin = dmin1 ( zmin, bzmin )
            zmax = dmax1 ( zmax, bzmax )

         end do

         extent(1) = xmin
         extent(2) = xmax
         extent(3) = ymin
         extent(4) = ymax
         extent(5) = zmin
         extent(6) = zmax

!
!        Calculate the cumulative extent of all plates for
!        each degree of freedom.
!
         avext = avext + dabs(bxmax-bxmin) + &
                         dabs(bymax-bymin) + &
                         dabs(bzmax-bzmin)

      end do

!
!     Calculate the average extent of all plates for
!     and the voxel size, i.e the length of one side
!     of a voxel cube.
!
      avext  = avext / real(3*np, wp)
      voxsiz = voxscl * avext
      mdltol = voxsiz * tol

!
!     Produce a set of vertex extents, extended by MDLTOL,
!     to be used later.
!
      do i = 1, 5, 2

         xextnt(i)   = extent(i)   - mdltol
         xextnt(i+1) = extent(i+1) + mdltol

      end do

!
!     Determine the size of the coarse voxels.
!
      cvxsiz = voxsiz * cgscal

!
!     Determine the minima and maxima of the body centered
!     vertex coordinates expressed in coarse voxel units. Scale the
!     vertices coord values by CVXSIZ: this scales the
!     axis in the voxel model space producing cubic voxels
!     with length 1 along each edge in voxel space,
!     CVXSIZ along an edge in model space.
!
      xvmin = xmin / cvxsiz
      yvmin = ymin / cvxsiz
      zvmin = zmin / cvxsiz
      xvmax = xmax / cvxsiz
      yvmax = ymax / cvxsiz
      zvmax = zmax / cvxsiz

!
!     Extend the coarse voxel grid by at least 1/2
!     coarse voxel length along each degree of freedom.
!
      xvmin = dnint ( xvmin - 1.0_wp )
      yvmin = dnint ( yvmin - 1.0_wp )
      zvmin = dnint ( zvmin - 1.0_wp )
      xvmax = dnint ( xvmax + 1.0_wp )
      yvmax = dnint ( yvmax + 1.0_wp )
      zvmax = dnint ( zvmax + 1.0_wp )

!
!     Calculate the coarse voxel grid origin in model units.
!
      voxori(1) = xvmin * cvxsiz
      voxori(2) = yvmin * cvxsiz
      voxori(3) = zvmin * cvxsiz

!
!     Calculate the dimension of the voxel grid in
!     units of (regular) voxels.
!
      nx = nint( xvmax - xvmin ) * cgscal
      ny = nint( yvmax - yvmin ) * cgscal
      nz = nint( zvmax - zvmin ) * cgscal

      nvox(1) = nx
      nvox(2) = ny
      nvox(3) = nz

      nvxtot = nx*ny*nz
!
!     Make sure the number of voxels NVXTOT is within range.
!
      if ( nvxtot > maxvox ) then

         call ctx%setmsg ( 'Fine voxel count NVXTOT = #; count must ' &
         //            'be in the range 1:#.'                    )
         call ctx%errint ( '#',  nvxtot                              )
         call ctx%errint ( '#',  maxvox                              )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         return

      end if

!
!     Check the coarse voxel scale. It must be at least 1, and its
!     cube must not exceed the fine voxel count.
!
      if (      ( cgscal < 1                ) &
           .or. ( cgscal > nvxtot**(1.0_wp/3) )  ) then

         call ctx%setmsg ( 'Coarse voxel scale = #; scale must ' &
         //            'be in the range 1:NVXTOT**3, where ' &
         //            'NVXTOT is the total fine voxel count. ' &
         //            'In this case, NVXTOT = #.'               )
         call ctx%errint ( '#',  cgscal                              )
         call ctx%errint ( '#',  nvxtot                              )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         return

      end if

!
!     The cube of the coarse scale must divide the total voxel count
!     evenly. This is a consistency check: the code that derives the
!     voxel grid dimensions should ensure this condition is met.
!
      q = nvxtot /   (cgscal**3)
      r = nvxtot - q*(cgscal**3)

      if ( r /= 0 ) then

         call ctx%setmsg ( 'Coarse voxel scale = #; the cube of ' &
         //            'the scale must divide NVXTOT evenly, ' &
         //            'where NVXTOT is the total  fine voxel ' &
         //            'count. In this case, NVXTOT = #.'        )
         call ctx%errint ( '#',  cgscal                              )
         call ctx%errint ( '#',  nvxtot                              )
         call ctx%sigerr ( 'SPICE(INCOMPATIBLESCALE)'                )
         return

      end if

!
!     Check the number of coarse voxels.
!
      npcg   = cgscal ** 3

      ncgflg = nvxtot / npcg

      if ( ncgflg > maxcgr ) then

         call ctx%setmsg ( 'Number of coarse voxels # exceeds ' &
         //            'limit #. Increase coarse voxel scale, ' &
         //            'fine voxel scale, or both.'            )
         call ctx%errint ( '#', ncgflg                             )
         call ctx%errint ( '#', maxcgr                             )
         call ctx%sigerr ( 'SPICE(COARSEGRIDOVERFLOW)'             )
         return

      end if

!
!     Enumerate all voxels that each plate might intersect.
!
      call zzinilnk ( ctx, maxptr, mxcell, ncell, vxptr, cells )

!
!     Set the dimensions of the coarse grid.
!
      cgrdim(1) = nx / cgscal
      cgrdim(2) = ny / cgscal
      cgrdim(3) = nz / cgscal

      cgrptr(1:ncgflg) = 0

!
!     TO points to the first free location in the VXPTR array.
!
      to = 1

      do i = 1, np
!
!        Find the extents of the Ith plate, where the extents
!        are expanded by TOL in each direction. We truncate
!        the expanded box at a distance of MDLTOL beyond the
!        extents of the vertex set, if necessary.
!
         xp(1) = vrtces( 1, plates(1,i) )
         xp(2) = vrtces( 1, plates(2,i) )
         xp(3) = vrtces( 1, plates(3,i) )

         yp(1) = vrtces( 2, plates(1,i) )
         yp(2) = vrtces( 2, plates(2,i) )
         yp(3) = vrtces( 2, plates(3,i) )

         zp(1) = vrtces( 3, plates(1,i) )
         zp(2) = vrtces( 3, plates(2,i) )
         zp(3) = vrtces( 3, plates(3,i) )

         bxmin = max(min(xextnt(1), xextnt(2)), min(max(xextnt(1), xextnt(2)), min( xp(1), xp(2), xp(3) ) - mdltol))

         bxmax = max(min(xextnt(1), xextnt(2)), min(max(xextnt(1), xextnt(2)), max( xp(1), xp(2), xp(3) ) + mdltol))

         bymin = max(min(xextnt(3), xextnt(4)), min(max(xextnt(3), xextnt(4)), min( yp(1), yp(2), yp(3) ) - mdltol))

         bymax = max(min(xextnt(3), xextnt(4)), min(max(xextnt(3), xextnt(4)), max( yp(1), yp(2), yp(3) ) + mdltol))

         bzmin = max(min(xextnt(5), xextnt(6)), min(max(xextnt(5), xextnt(6)), min( zp(1), zp(2), zp(3) ) - mdltol))

         bzmax = max(min(xextnt(5), xextnt(6)), min(max(xextnt(5), xextnt(6)), max( zp(1), zp(2), zp(3) ) + mdltol))

!
!        Find the range of voxel coordinates that contain the bounding
!        box of the plate. All we need look at are the coordinates
!        of the two corners having minimum and maximum coordinates.
!
!        Start with the corner having minimum coordinates:
!
         vmod = [bxmin, bymin, bzmin]

         call zzgetvox ( ctx, voxsiz, voxori, nvox, vmod, inbox, vcoord )

         if ( .not. inbox ) then
!
!           A corner of the bounding box lies outside the voxel grid.
!           This should never occur.
!
            call ctx%setmsg ( 'BUG: bounding box of plate is outside of ' &
            //            'voxel grid. Input coordinates were ' &
            //            '(#, #, #). Plate ID = #.'                  )
            call ctx%errdp  ( '#', vmod(1)                                )
            call ctx%errdp  ( '#', vmod(2)                                )
            call ctx%errdp  ( '#', vmod(3)                                )
            call ctx%errint ( '#', i                                      )
            call ctx%sigerr ( 'SPICE(BUG)'                                )
            return

         end if

!
!        Unpack minimum voxel coordinates from VCOORD.
!
         gxmin = vcoord(1)
         gymin = vcoord(2)
         gzmin = vcoord(3)

!
!        Now handle the corner having maximum coordinates:
!
         vmod = [bxmax, bymax, bzmax]

         call zzgetvox ( ctx, voxsiz, voxori, nvox, vmod, inbox, vcoord )

         if ( .not. inbox ) then
!
!           A corner of the bounding box lies outside the voxel grid.
!           This should never occur.
!
            call ctx%setmsg ( 'BUG: bounding box of plate is outside of ' &
            //            'voxel grid. Input coordinates were ' &
            //            '(#, #, #). Plate ID = #.'                  )
            call ctx%errdp  ( '#', vmod(1)                                )
            call ctx%errdp  ( '#', vmod(2)                                )
            call ctx%errdp  ( '#', vmod(3)                                )
            call ctx%errint ( '#', i                                      )
            call ctx%sigerr ( 'SPICE(BUG)'                                )
            return

         end if

!
!        Unpack maximum voxel coordinates from VCOORD.
!
         gxmax = vcoord(1)
         gymax = vcoord(2)
         gzmax = vcoord(3)

!
!        Determine voxels that the bounding box of the plate
!        intersects.
!
         do iz = gzmin, gzmax

             do iy = gymin, gymax

                 do ix = gxmin, gxmax

                    vixyz(1) = ix
                    vixyz(2) = iy
                    vixyz(3) = iz
!
!                   Find the coarse voxel containing this voxel, and
!                   compute the offset of this voxel within the coarse
!                   voxel. The output CGXYZ contains the 3-dimensional
!                   coordinates of the coarse voxel within the coarse
!                   grid. The output CGOF1D is the 1-based,
!                   1-dimensional offset of the current voxel (having
!                   coordinates VIXYZ) from the start of the coarse
!                   voxel.
!
                    call zzvoxcvo ( ctx, vixyz, nvox,  cgscal, &
                                    cgxyz, cgoff, cgof1d  )

                    if ( ctx%failed() ) return

                    cvid = zzvox2id ( ctx, cgxyz, cgrdim )

                    if ( cgrptr(cvid) == 0 ) then
!
!                      The coarse voxel at index CVID is empty so far.
!                      Allocate CGSCAL pointers for it in the VXPTR
!                      array; make the coarse voxel point to the first
!                      element of this sub-array.
!
                       cgrptr(cvid) = to
                       to           = to + npcg

                    end if
!
!                   Let IXPTR be the index in the VXPTR array of the
!                   pointer for the current voxel.
!
                    ixptr = ( cgrptr(cvid) - 1 )  +  cgof1d

                    call zzaddlnk ( ctx, ixptr,  i,     maxptr, &
                                    mxcell, vxptr, ncell, cells )

                    if ( ctx%failed() ) return

                 end do

              end do

          end do

      end do

      nvxptr = to - 1

!
!     Generate two linked lists mapping voxel ID to the plates enclosed
!     within that voxel (if any).
!
!     VXPTR : An array, indexed by voxel ID. For an array element,
!             VXPTR(VOX_ID), greater than zero, the value identifies an
!             index in VXLIST, the value of that VXLIST array element
!             equaling the number of plates contained in the voxel
!             specified by the ID. The condition VXPTR(VOX_ID) = -1
!             indicates the voxel contains no plates.
!
!     VXLIST: An array, indexed by the positive entries in VXPTR. The
!             element, N, identified by a VXPTR value describes the
!             number of plates contained in the corresponding voxel.
!
!                 N = VXLIST( VXPTR(VOX_ID) )
!
!             The N elements following VXLIST( VXPTR(VOX_ID) ),
!             contain the IDs of those plates within the voxel.
!
      call zzuntngl ( ctx, nvxptr, mxcell, cells, &
                      maxvxl, vxptr,  nvxlst, vxlist )

      return
      end subroutine zzmkspin

      !> Open a new DLA file and set the file type.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a new DLA file to be created. The file
      !>              will be left opened for write access.
      !>
      !>     FTYPE    is a code for type of data placed into a DLA file. The
      !>              non-blank part of FTYPE is used as the "file type"
      !>              portion of the ID word in the DLA file.
      !>
      !>              The first nonblank character and the three, or fewer,
      !>              characters immediately following it, giving four
      !>              characters, are used to represent the type of the data
      !>              placed in the DLA file. This is provided as a convenience
      !>              for higher level software. It is an error if this string
      !>              is blank. Also, the file type may not contain any
      !>              nonprinting characters. When written to the DLA file, the
      !>              value for the type IS case sensitive.
      !>
      !>              NAIF has reserved for its own use file types consisting
      !>              of the upper case letters (A-Z) and the digits 0-9. NAIF
      !>              recommends lower case or mixed case file types be used by
      !>              all others in order to avoid any conflicts with NAIF file
      !>              types.
      !>
      !>     IFNAME   is the internal file name for the new file. The name may
      !>              contain as many as 60 characters. This name should
      !>              uniquely identify the file.
      !>
      !>     NCOMCH   is the number of comment characters to allocate.
      !>
      !>              NCOMCH is used to establish the number of comment records
      !>              that will be allocated to the new DLA file. The number of
      !>              comment records allocated is the minimum required to
      !>              store the specified number of comment characters.
      !>
      !>              Allocating comment records at file creation time may
      !>              reduce the likelihood of having to expand the
      !>              comment area later.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the file handle associated with the file. This handle
      !>              is used to identify the file in subsequent calls to other
      !>              DLA routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input filename is blank, an error is signaled by a
      !>         routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     2)  If the specified file cannot be opened without exceeding the
      !>         maximum allowed number of open DAS files, an error is signaled
      !>         by a routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     3)  If the file cannot be opened properly, an error is signaled by
      !>         a routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     4)  If the initial records in the file cannot be written, an error
      !>         is signaled by a routine in the call tree of this routine. No
      !>         file will be created.
      !>
      !>     5)  If no logical units are available, an error is signaled by a
      !>         routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     6)  If the file type is blank, an error is signaled by a routine
      !>         in the call tree of this routine. No file will be created.
      !>
      !>     7)  If the file type contains nonprinting characters, decimal 0-31
      !>         and 127-255, an error is signaled by a routine in the call
      !>         tree of this routine. No file will be created.
      !>
      !>     8)  If the number of comment characters allocated to be allocated,
      !>         NCOMCH, is negative, the error SPICE(BADRECORDCOUNT) is
      !>         signaled. No file will be created.
      !>
      !>## Files
      !>
      !>     See argument FNAME.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine creates a new DLA file and sets the type of the
      !>     file to the mnemonic code passed to it.
      !>
      !>     DLA files created by this routine have initialized file records.
      !>     The ID word in a DLA file record has the form
      !>
      !>        DAS/xxxx
      !>
      !>     where the characters following the slash are supplied by the
      !>     caller of this routine.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a DLA file containing one segment; the segment
      !>        contains character, double precision, and integer data.
      !>        After writing and closing the file, open the file for
      !>        read access; dump the data to standard output.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLAOPN_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         DLA
      !>      PARAMETER           ( DLA    = 'dlaopn_ex1.dla' )
      !>
      !>      INTEGER               IFNLEN
      !>      PARAMETER           ( IFNLEN =  60 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE =  61 )
      !>
      !>      INTEGER               MAXC
      !>      PARAMETER           ( MAXC   =  5 )
      !>
      !>      INTEGER               MAXD
      !>      PARAMETER           ( MAXD   =  50 )
      !>
      !>      INTEGER               MAXI
      !>      PARAMETER           ( MAXI   =  100 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(LNSIZE)    CVALS   ( MAXC )
      !>      CHARACTER*(LNSIZE)    CVALS2  ( MAXC )
      !>      CHARACTER*(IFNLEN)    IFNAME
      !>
      !>      DOUBLE PRECISION      DVALS   ( MAXD )
      !>      DOUBLE PRECISION      DVALS2  ( MAXD )
      !>
      !>      INTEGER               BASE
      !>      INTEGER               DESCR   ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               IVALS   ( MAXI )
      !>      INTEGER               IVALS2  ( MAXI )
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>      INTEGER               NCOMCH
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Set the internal file name.  Don't reserve characters in
      !>C     the DAS comment area.
      !>C
      !>      IFNAME = 'Example DLA file for testing'
      !>      NCOMCH = 0
      !>
      !>C
      !>C     Open a new DLA file.
      !>C
      !>      CALL DLAOPN ( DLA, 'DLA', IFNAME, NCOMCH, HANDLE )
      !>
      !>C
      !>C     Begin a new segment.
      !>C
      !>      CALL DLABNS ( HANDLE )
      !>
      !>C
      !>C     Add character data to the segment.
      !>C
      !>      DO I = 1, MAXC
      !>
      !>         DO J = 1, LNSIZE
      !>
      !>            K = MOD( J+I-1, 10 )
      !>
      !>            CALL INTSTR ( K,  CVALS(I)(J:J) )
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      CALL DASADC ( HANDLE, MAXC*LNSIZE, 1, LNSIZE, CVALS )
      !>
      !>C
      !>C     Add integer and double precision data to the segment.
      !>C
      !>      DO I = 1, MAXI
      !>         IVALS(I) = I
      !>      END DO
      !>
      !>      CALL DASADI ( HANDLE, MAXI, IVALS )
      !>
      !>      DO I = 1, MAXD
      !>         DVALS(I) = I
      !>      END DO
      !>
      !>      CALL DASADD ( HANDLE, MAXD, DVALS )
      !>
      !>C
      !>C     End the segment.
      !>C
      !>      CALL DLAENS ( HANDLE )
      !>
      !>C
      !>C     Close the file.  The routine DASCLS flushes the DAS
      !>C     buffers and segregates the file before closing it.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>C
      !>C     Now read the file and check the data.
      !>C
      !>      CALL DASOPR ( DLA, HANDLE )
      !>
      !>C
      !>C     Obtain the segment descriptor for the sole segment
      !>C     in the file. We need not check the found flag
      !>C     in this case because we know there is one segment
      !>C     in the file.
      !>C
      !>      CALL DLABFS ( HANDLE, DESCR, FOUND )
      !>
      !>C
      !>C     Fetch character data from the segment.  Obtain the
      !>C     base address of the character data and the
      !>C     character count from the descriptor.
      !>C
      !>      BASE = DESCR(CBSIDX)
      !>      N    = DESCR(CSZIDX)
      !>
      !>      CALL DASRDC ( HANDLE, BASE+1, BASE+N, 1, LNSIZE, CVALS2 )
      !>
      !>C
      !>C     Display the character data.
      !>C
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Character array:'
      !>
      !>      DO I = 1, N/LNSIZE
      !>         WRITE (*,*) CVALS2(I)
      !>      END DO
      !>
      !>C
      !>C     Fetch and display the integer and double precision data.
      !>C
      !>      BASE = DESCR(IBSIDX)
      !>      N    = DESCR(ISZIDX)
      !>
      !>      CALL DASRDI( HANDLE, BASE+1, BASE+N, IVALS2 )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Integer array:'
      !>      DO I = 1, N/10
      !>         WRITE (*,'(10I6)') (IVALS2((I-1)*10 + J), J=1, 10)
      !>      END DO
      !>
      !>      BASE = DESCR(DBSIDX)
      !>      N    = DESCR(DSZIDX)
      !>
      !>      CALL DASRDD( HANDLE, BASE+1, BASE+N, DVALS2 )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Double precision array:'
      !>      DO I = 1, N/10
      !>         WRITE (*,'(10F6.1)') (DVALS2((I-1)*10 + J), J=1, 10)
      !>      END DO
      !>
      !>C
      !>C     Close the file.  This step is unnecessary in this
      !>C     program, but is a good practice in general
      !>C     because closing the file frees resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Character array:
      !>         1234567890123456789012345678901234567890123456789012345678901
      !>         2345678901234567890123456789012345678901234567890123456789012
      !>         3456789012345678901234567890123456789012345678901234567890123
      !>         4567890123456789012345678901234567890123456789012345678901234
      !>         5678901234567890123456789012345678901234567890123456789012345
      !>
      !>         Integer array:
      !>             1     2     3     4     5     6     7     8     9    10
      !>            11    12    13    14    15    16    17    18    19    20
      !>            21    22    23    24    25    26    27    28    29    30
      !>            31    32    33    34    35    36    37    38    39    40
      !>            41    42    43    44    45    46    47    48    49    50
      !>            51    52    53    54    55    56    57    58    59    60
      !>            61    62    63    64    65    66    67    68    69    70
      !>            71    72    73    74    75    76    77    78    79    80
      !>            81    82    83    84    85    86    87    88    89    90
      !>            91    92    93    94    95    96    97    98    99   100
      !>
      !>         Double precision array:
      !>           1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0  10.0
      !>          11.0  12.0  13.0  14.0  15.0  16.0  17.0  18.0  19.0  20.0
      !>          21.0  22.0  23.0  24.0  25.0  26.0  27.0  28.0  29.0  30.0
      !>          31.0  32.0  33.0  34.0  35.0  36.0  37.0  38.0  39.0  40.0
      !>          41.0  42.0  43.0  44.0  45.0  46.0  47.0  48.0  49.0  50.0
      !>
      !>        Note that after run completion, a new DLA file exists in the
      !>        output directory.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 14-SEP-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated the header to describe the usage of input argument
      !>        NCOMCH instead of the previously documented NCOMR.
      !>
      !>        Added complete code example based on that provided for DLABNS
      !>        and DLAENS.
      !>
      !>        Replaced sequence of asterisks with string 'xxxx'
      !>        in the comment line illustrating the DAS ID word syntax.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        01-APR-2016 (NJB)
      !>
      !>           Changed short error message for invalid comment
      !>           count. Corrected reference to "DASCLU" in comments.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        09-FEB-2005 (NJB) (KRG)
      module subroutine dlaopn ( ctx, fname, ftype, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of a DLA file to be opened.
      character(*), intent(in) :: ftype  !! Mnemonic code for type of data in the DLA file.
      character(*), intent(in) :: ifname  !! Internal file name.
      integer(ip), intent(in) :: ncomch  !! Number of comment characters to allocate.
      integer(ip), intent(out) :: handle  !! Handle assigned to the opened DLA file.

      integer(ip) :: ncomr

      if ( ctx%failed() ) return

!
!     Compute the number of comment records required.
!
      if ( ncomch > 0 ) then

         ncomr = ( (ncomch-1) / nchrec )  +  1

      else if ( ncomch == 0 ) then

         ncomr = 0

      else

         call ctx%setmsg ( 'Requested number of comment characters ' &
         //            'must be non-negative but was #.'         )
         call ctx%errint ( '#',  ncomch                              )
         call ctx%sigerr ( 'SPICE(BADRECORDCOUNT)'                   )
         return

      end if

!
!     Let the DAS "open new" routine do the work.
!
      call dasonw ( ctx, fname, ftype, ifname, ncomr, handle )

!
!     Write the format version.
!
      call dasadi ( ctx, handle, 1, [ fmtver ] )

!
!     Initialize the forward and backward segment list pointers.
!
      call dasadi ( ctx, handle, 1, [ nulptr ] )
      call dasadi ( ctx, handle, 1, [ nulptr ] )

!
!     We leave the file open, since further writes to the file
!     should occur next.  The file will eventually be closed
!     by a call to DASCLS or DASLLC, if all goes well.
!
      return
      end subroutine dlaopn

      !> Begin a new segment in a DLA file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle associated with the DLA file to
      !>              be updated. This handle is used to identify the file
      !>              in subsequent calls to other DLA or DAS routines.
      !>
      !>              The DLA file must be open for write access. A new DLA
      !>              segment is started in the indicated file. The file
      !>              is left open, since normally data will be written to
      !>              the file following a call to this routine.
      !>
      !>## Detailed Output
      !>
      !>     None. See the $Particulars and $Examples header sections for
      !>     a description of the actions performed by this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file handle does not refer to a DAS file that is
      !>         open for write access, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If an error occurs while reading or writing to the DLA file,
      !>         the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See description of input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports creation of a DLA segment. DLA segments
      !>     are created by appending data to the DAS integer, double
      !>     precision, and character address spaces of a DLA file. The new
      !>     segment's descriptor is located immediately before the integer
      !>     component of the segment's data.
      !>
      !>     When a new segment is added to a DLA file, the segment is
      !>     inserted into the file's doubly linked segment list. If the new
      !>     segment is the first, the DLA file's first and last list entry
      !>     pointers are updated to point to the new segment; specifically,
      !>     these pointers point to the first integer of the new segment's
      !>     descriptor. The backward pointer of the new segment is set to
      !>     null in this case.
      !>
      !>     If the new segment is not the first, the DLA file's list end
      !>     pointer is updated to point to the new segment, and the forward
      !>     pointer of the previous segment also is updated to point to the
      !>     first integer of the new segment's descriptor. The backward
      !>     pointer of the new segment points to the first integer of the
      !>     previous segment's descriptor.
      !>
      !>     The normal sequence of operations required to create a DLA
      !>     segment is as follows:
      !>
      !>        Call DLAOPN to create a new, empty DLA file.
      !>
      !>        For each segment to be created,
      !>
      !>           Call DLABNS to begin a segment.
      !>
      !>           Use the DAS "add" and "update" routines to populate
      !>           the segment with data.
      !>
      !>           Call DLAENS to end the segment.
      !>
      !>        Call DASCLS to segregate and close the DLA file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a DLA file containing one segment; the segment
      !>        contains character, double precision, and integer data.
      !>        After writing and closing the file, open the file for
      !>        read access; dump the data to standard output.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLABNS_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         DLA
      !>      PARAMETER           ( DLA    = 'dlabns_ex1.dla' )
      !>
      !>      INTEGER               IFNLEN
      !>      PARAMETER           ( IFNLEN =  60 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE =  61 )
      !>
      !>      INTEGER               MAXC
      !>      PARAMETER           ( MAXC   =  5 )
      !>
      !>      INTEGER               MAXD
      !>      PARAMETER           ( MAXD   =  50 )
      !>
      !>      INTEGER               MAXI
      !>      PARAMETER           ( MAXI   =  100 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(LNSIZE)    CVALS   ( MAXC )
      !>      CHARACTER*(LNSIZE)    CVALS2  ( MAXC )
      !>      CHARACTER*(IFNLEN)    IFNAME
      !>
      !>      DOUBLE PRECISION      DVALS   ( MAXD )
      !>      DOUBLE PRECISION      DVALS2  ( MAXD )
      !>
      !>      INTEGER               BASE
      !>      INTEGER               DESCR   ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               IVALS   ( MAXI )
      !>      INTEGER               IVALS2  ( MAXI )
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>      INTEGER               NCOMCH
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Set the internal file name.  Don't reserve characters in
      !>C     the DAS comment area.
      !>C
      !>      IFNAME = 'Example DLA file for testing'
      !>      NCOMCH = 0
      !>
      !>C
      !>C     Open a new DLA file.
      !>C
      !>      CALL DLAOPN ( DLA, 'DLA', IFNAME, NCOMCH, HANDLE )
      !>
      !>C
      !>C     Begin a new segment.
      !>C
      !>      CALL DLABNS ( HANDLE )
      !>
      !>C
      !>C     Add character data to the segment.
      !>C
      !>      DO I = 1, MAXC
      !>
      !>         DO J = 1, LNSIZE
      !>
      !>            K = MOD( J+I-1, 10 )
      !>
      !>            CALL INTSTR ( K,  CVALS(I)(J:J) )
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      CALL DASADC ( HANDLE, MAXC*LNSIZE, 1, LNSIZE, CVALS )
      !>
      !>C
      !>C     Add integer and double precision data to the segment.
      !>C
      !>      DO I = 1, MAXI
      !>         IVALS(I) = I
      !>      END DO
      !>
      !>      CALL DASADI ( HANDLE, MAXI, IVALS )
      !>
      !>      DO I = 1, MAXD
      !>         DVALS(I) = I
      !>      END DO
      !>
      !>      CALL DASADD ( HANDLE, MAXD, DVALS )
      !>
      !>C
      !>C     End the segment.
      !>C
      !>      CALL DLAENS ( HANDLE )
      !>
      !>C
      !>C     Close the file.  The routine DASCLS flushes the DAS
      !>C     buffers and segregates the file before closing it.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>C
      !>C     Now read the file and check the data.
      !>C
      !>      CALL DASOPR ( DLA, HANDLE )
      !>
      !>C
      !>C     Obtain the segment descriptor for the sole segment
      !>C     in the file. We need not check the found flag
      !>C     in this case because we know there is one segment
      !>C     in the file.
      !>C
      !>      CALL DLABFS ( HANDLE, DESCR, FOUND )
      !>
      !>C
      !>C     Fetch character data from the segment.  Obtain the
      !>C     base address of the character data and the
      !>C     character count from the descriptor.
      !>C
      !>      BASE = DESCR(CBSIDX)
      !>      N    = DESCR(CSZIDX)
      !>
      !>      CALL DASRDC ( HANDLE, BASE+1, BASE+N, 1, LNSIZE, CVALS2 )
      !>
      !>C
      !>C     Display the character data.
      !>C
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Character array:'
      !>
      !>      DO I = 1, N/LNSIZE
      !>         WRITE (*,*) CVALS2(I)
      !>      END DO
      !>
      !>C
      !>C     Fetch and display the integer and double precision data.
      !>C
      !>      BASE = DESCR(IBSIDX)
      !>      N    = DESCR(ISZIDX)
      !>
      !>      CALL DASRDI( HANDLE, BASE+1, BASE+N, IVALS2 )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Integer array:'
      !>      DO I = 1, N/10
      !>         WRITE (*,'(10I6)') (IVALS2((I-1)*10 + J), J=1, 10)
      !>      END DO
      !>
      !>      BASE = DESCR(DBSIDX)
      !>      N    = DESCR(DSZIDX)
      !>
      !>      CALL DASRDD( HANDLE, BASE+1, BASE+N, DVALS2 )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Double precision array:'
      !>      DO I = 1, N/10
      !>         WRITE (*,'(10F6.1)') (DVALS2((I-1)*10 + J), J=1, 10)
      !>      END DO
      !>
      !>C
      !>C     Close the file.  This step is unnecessary in this
      !>C     program, but is a good practice in general
      !>C     because closing the file frees resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Character array:
      !>         1234567890123456789012345678901234567890123456789012345678901
      !>         2345678901234567890123456789012345678901234567890123456789012
      !>         3456789012345678901234567890123456789012345678901234567890123
      !>         4567890123456789012345678901234567890123456789012345678901234
      !>         5678901234567890123456789012345678901234567890123456789012345
      !>
      !>         Integer array:
      !>             1     2     3     4     5     6     7     8     9    10
      !>            11    12    13    14    15    16    17    18    19    20
      !>            21    22    23    24    25    26    27    28    29    30
      !>            31    32    33    34    35    36    37    38    39    40
      !>            41    42    43    44    45    46    47    48    49    50
      !>            51    52    53    54    55    56    57    58    59    60
      !>            61    62    63    64    65    66    67    68    69    70
      !>            71    72    73    74    75    76    77    78    79    80
      !>            81    82    83    84    85    86    87    88    89    90
      !>            91    92    93    94    95    96    97    98    99   100
      !>
      !>         Double precision array:
      !>           1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0  10.0
      !>          11.0  12.0  13.0  14.0  15.0  16.0  17.0  18.0  19.0  20.0
      !>          21.0  22.0  23.0  24.0  25.0  26.0  27.0  28.0  29.0  30.0
      !>          31.0  32.0  33.0  34.0  35.0  36.0  37.0  38.0  39.0  40.0
      !>          41.0  42.0  43.0  44.0  45.0  46.0  47.0  48.0  49.0  50.0
      !>
      !>        Note that after run completion, a new DLA file exists in the
      !>        output directory.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 14-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Modified
      !>        the presentation of the output in the code example to comply
      !>        with the maximum line length for the header comments.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        13-JUL-2012 (NJB)
      !>
      !>           Bug fix: deleted unused line of code.
      !>           Fixed some header typos.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        11-FEB-2005 (NJB)
      module subroutine dlabns ( ctx, handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of open DLA file.

      integer(ip), parameter :: fstidx = 1

      integer(ip), parameter :: lstidx = 2

      integer(ip) :: addr
      integer(ip) :: sgptrs ( 2 )
      integer(ip) :: descr  ( dladsz )
      integer(ip) :: lastc
      integer(ip) :: lastd
      integer(ip) :: lasti
      integer(ip) :: this

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Make sure the input handle refers to a DAS file that
!     is open for write access.
!
      call dassih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Look up the pointers to the first and last DLA segment
!     descriptors in the file.  If no segments are present,
!     the pointers will contain the value NULPTR.
!
      call dasrdi ( ctx, handle, llbidx, lleidx, sgptrs )

!
!     Find the last DAS logical addresses in use for each data type.
!
      call daslla ( ctx, handle, lastc, lastd, lasti )

!
!     Initialize a DLA descriptor with null values.  If this
!     is not the first segment in the file, the backward pointer
!     must be set to point to the previous descriptor.  It's
!     valid to overwrite the descriptor pointer in either case
!     due to the initialization of the file's last segment pointer
!     to NULPTR.
!
      descr = nulptr

      descr(bwdidx) = sgptrs(lstidx)

!
!     Set the descriptor's component base addresses now.  The
!     base addresses are *predecessors* of the first address of
!     each data type.  This choice slightly simplifies arithmetic
!     needed to express the address range occupied by a segment
!     component:  the range is
!
!        base + 1  :  base + size
!
!     For the integer address, add on the size of the descriptor
!     we're about to write.
!
      descr(ibsidx) = lasti + dladsz
      descr(dbsidx) = lastd
      descr(cbsidx) = lastc

!
!     Append the descriptor to the file.
!
      call dasadi ( ctx, handle, dladsz, descr )

!
!     THIS is the pointer to the current descriptor.
!
      this = lasti + 1

!
!     If this is not the first segment, the forward pointer of the
!     previous descriptor must be updated to point to this descriptor.
!
      if ( sgptrs(lstidx) /= nulptr ) then

         addr = sgptrs(lstidx) + fwdidx - 1

         call dasudi ( ctx, handle, addr, addr, [ this ] )

      end if

!
!     Update the segment list pointers in the file. The begin pointer
!     must be updated only if it's null.  The end pointer will point to
!     this segment.
!
      if ( sgptrs(fstidx) == nulptr ) then

         call dasudi ( ctx, handle, llbidx, llbidx, [ this ] )

      end if

      call dasudi ( ctx, handle, lleidx, lleidx, [ this ] )

!
!     Leave the file open.  The segment is now ready to be
!     populated with data.  The routines DASADC, DASADD, and
!     DASADI should be used to append data to the segment.
!

      return
      end subroutine dlabns

      !> End a new segment in a DLA file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle associated with the DLA file to
      !>              be updated. This handle is used to identify the file
      !>              in subsequent calls to other DLA or DAS routines.
      !>
      !>              The DLA file must be open for write access. A new DLA
      !>              segment is completed in the indicated file. The file
      !>              is left open, since data may be written to the file
      !>              following a call to this routine.
      !>
      !>## Detailed Output
      !>
      !>     None. See the $Particulars and $Examples header sections for
      !>     a description of the actions performed by this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file handle does not refer to a DAS file that is
      !>         open for write access, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If an error occurs while reading or writing to the DLA file,
      !>         the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See description of input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports creation of a DLA segment. DLA segments
      !>     are created by appending data to the DAS integer, double
      !>     precision, and character address spaces of a DLA file. The new
      !>     segment's descriptor is located immediately before the integer
      !>     component of the segment's data.
      !>
      !>     When a new segment is added to a DLA file, the segment is
      !>     inserted into the file's doubly linked segment list. If the new
      !>     segment is the first, the DLA file's first and last list entry
      !>     pointers are updated to point to the new segment; specifically,
      !>     these pointers point to the first integer of the new segment's
      !>     descriptor. The backward pointer of the new segment is set to
      !>     null in this case.
      !>
      !>     If the new segment is not the first, the DLA file's list end
      !>     pointer is updated to point to the new segment, and the forward
      !>     pointer of the previous segment also is updated to point to the
      !>     first integer of the new segment's descriptor. The backward
      !>     pointer of the new segment points to to point to the first
      !>     integer of the previous segment's descriptor.
      !>
      !>     The normal sequence of operations required to create a DLA
      !>     segment is as follows:
      !>
      !>        Call DLAOPN to create a new, empty DLA file.
      !>
      !>        For each segment to be created,
      !>
      !>           Call DLABNS to begin a segment.
      !>
      !>           Use the DAS "add" and "update" routines to populate
      !>           the segment with data.
      !>
      !>           Call DLAENS to end the segment.
      !>
      !>        Call DASCLS to segregate and close the DLA file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a DLA file containing one segment; the segment
      !>        contains character, double precision, and integer data.
      !>        After writing and closing the file, open the file for
      !>        read access; dump the data to standard output.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLAENS_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         DLA
      !>      PARAMETER           ( DLA    = 'dlaens_ex1.dla' )
      !>
      !>      INTEGER               IFNLEN
      !>      PARAMETER           ( IFNLEN =  60 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE =  61 )
      !>
      !>      INTEGER               MAXC
      !>      PARAMETER           ( MAXC   =  5 )
      !>
      !>      INTEGER               MAXD
      !>      PARAMETER           ( MAXD   =  50 )
      !>
      !>      INTEGER               MAXI
      !>      PARAMETER           ( MAXI   =  100 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(LNSIZE)    CVALS   ( MAXC )
      !>      CHARACTER*(LNSIZE)    CVALS2  ( MAXC )
      !>      CHARACTER*(IFNLEN)    IFNAME
      !>
      !>      DOUBLE PRECISION      DVALS   ( MAXD )
      !>      DOUBLE PRECISION      DVALS2  ( MAXD )
      !>
      !>      INTEGER               BASE
      !>      INTEGER               DESCR   ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               IVALS   ( MAXI )
      !>      INTEGER               IVALS2  ( MAXI )
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>      INTEGER               NCOMCH
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Set the internal file name.  Don't reserve characters in
      !>C     the DAS comment area.
      !>C
      !>      IFNAME = 'Example DLA file for testing'
      !>      NCOMCH = 0
      !>
      !>C
      !>C     Open a new DLA file.
      !>C
      !>      CALL DLAOPN ( DLA, 'DLA', IFNAME, NCOMCH, HANDLE )
      !>
      !>C
      !>C     Begin a new segment.
      !>C
      !>      CALL DLABNS ( HANDLE )
      !>
      !>C
      !>C     Add character data to the segment.
      !>C
      !>      DO I = 1, MAXC
      !>
      !>         DO J = 1, LNSIZE
      !>
      !>            K = MOD( J+I-1, 10 )
      !>
      !>            CALL INTSTR ( K,  CVALS(I)(J:J) )
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      CALL DASADC ( HANDLE, MAXC*LNSIZE, 1, LNSIZE, CVALS )
      !>
      !>C
      !>C     Add integer and double precision data to the segment.
      !>C
      !>      DO I = 1, MAXI
      !>         IVALS(I) = I
      !>      END DO
      !>
      !>      CALL DASADI ( HANDLE, MAXI, IVALS )
      !>
      !>      DO I = 1, MAXD
      !>         DVALS(I) = I
      !>      END DO
      !>
      !>      CALL DASADD ( HANDLE, MAXD, DVALS )
      !>
      !>C
      !>C     End the segment.
      !>C
      !>      CALL DLAENS ( HANDLE )
      !>
      !>C
      !>C     Close the file.  The routine DASCLS flushes the DAS
      !>C     buffers and segregates the file before closing it.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>C
      !>C     Now read the file and check the data.
      !>C
      !>      CALL DASOPR ( DLA, HANDLE )
      !>
      !>C
      !>C     Obtain the segment descriptor for the sole segment
      !>C     in the file. We need not check the found flag
      !>C     in this case because we know there is one segment
      !>C     in the file.
      !>C
      !>      CALL DLABFS ( HANDLE, DESCR, FOUND )
      !>
      !>C
      !>C     Fetch character data from the segment.  Obtain the
      !>C     base address of the character data and the
      !>C     character count from the descriptor.
      !>C
      !>      BASE = DESCR(CBSIDX)
      !>      N    = DESCR(CSZIDX)
      !>
      !>      CALL DASRDC ( HANDLE, BASE+1, BASE+N, 1, LNSIZE, CVALS2 )
      !>
      !>C
      !>C     Display the character data.
      !>C
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Character array:'
      !>
      !>      DO I = 1, N/LNSIZE
      !>         WRITE (*,*) CVALS2(I)
      !>      END DO
      !>
      !>C
      !>C     Fetch and display the integer and double precision data.
      !>C
      !>      BASE = DESCR(IBSIDX)
      !>      N    = DESCR(ISZIDX)
      !>
      !>      CALL DASRDI( HANDLE, BASE+1, BASE+N, IVALS2 )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Integer array:'
      !>      DO I = 1, N/10
      !>         WRITE (*,'(10I6)') (IVALS2((I-1)*10 + J), J=1, 10)
      !>      END DO
      !>
      !>      BASE = DESCR(DBSIDX)
      !>      N    = DESCR(DSZIDX)
      !>
      !>      CALL DASRDD( HANDLE, BASE+1, BASE+N, DVALS2 )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Double precision array:'
      !>      DO I = 1, N/10
      !>         WRITE (*,'(10F6.1)') (DVALS2((I-1)*10 + J), J=1, 10)
      !>      END DO
      !>
      !>C
      !>C     Close the file.  This step is unnecessary in this
      !>C     program, but is a good practice in general
      !>C     because closing the file frees resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Character array:
      !>         1234567890123456789012345678901234567890123456789012345678901
      !>         2345678901234567890123456789012345678901234567890123456789012
      !>         3456789012345678901234567890123456789012345678901234567890123
      !>         4567890123456789012345678901234567890123456789012345678901234
      !>         5678901234567890123456789012345678901234567890123456789012345
      !>
      !>         Integer array:
      !>             1     2     3     4     5     6     7     8     9    10
      !>            11    12    13    14    15    16    17    18    19    20
      !>            21    22    23    24    25    26    27    28    29    30
      !>            31    32    33    34    35    36    37    38    39    40
      !>            41    42    43    44    45    46    47    48    49    50
      !>            51    52    53    54    55    56    57    58    59    60
      !>            61    62    63    64    65    66    67    68    69    70
      !>            71    72    73    74    75    76    77    78    79    80
      !>            81    82    83    84    85    86    87    88    89    90
      !>            91    92    93    94    95    96    97    98    99   100
      !>
      !>         Double precision array:
      !>           1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0  10.0
      !>          11.0  12.0  13.0  14.0  15.0  16.0  17.0  18.0  19.0  20.0
      !>          21.0  22.0  23.0  24.0  25.0  26.0  27.0  28.0  29.0  30.0
      !>          31.0  32.0  33.0  34.0  35.0  36.0  37.0  38.0  39.0  40.0
      !>          41.0  42.0  43.0  44.0  45.0  46.0  47.0  48.0  49.0  50.0
      !>
      !>        Note that after run completion, a new DLA file exists in the
      !>        output directory.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 14-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Modified
      !>        the presentation of the output in the code example to comply
      !>        with the maximum line length for the header comments.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        11-FEB-2005 (NJB)
      module subroutine dlaens ( ctx, handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      integer(ip) :: this_1(1)  !! one-element copy of this (array argument)

      integer(ip) :: descr  ( dladsz )
      integer(ip) :: lastc
      integer(ip) :: lastd
      integer(ip) :: lasti
      integer(ip) :: this

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Now that the segment has been written, our only task is
!     to update the corresponding segment descriptor to reflect
!     the sizes of each component.
!
!     Look up the pointer to the last DLA segment descriptor in the
!     file.  Then look up the segment descriptor itself.
!
      call dasrdi ( ctx, handle, lleidx, lleidx, this_1 )
      this = this_1(1)
      call dasrdi ( ctx, handle, this,   this+dladsz-1, descr )

!
!     Find the last DAS logical addresses in use for each data type.
!
      call daslla ( ctx, handle, lastc, lastd, lasti )

!
!     Set the component sizes in the descriptor. The sizes are easily
!     computed from the last addresses in use and the component base
!     addresses already stored in the segment descriptor.
!
      descr ( iszidx ) = lasti - descr(ibsidx )
      descr ( dszidx ) = lastd - descr(dbsidx )
      descr ( cszidx ) = lastc - descr(cbsidx )

!
!     Update the descriptor in the file.
!
      call dasudi ( ctx, handle, this, this+dladsz-1, descr )

!
!     Leave the file open.  The file is now ready for the
!     addition of a new segment.
!
      return
      end subroutine dlaens

      !> Begin a forward segment search in a DLA file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle associated with the file to be
      !>              searched. This handle is used to identify the file in
      !>              subsequent calls to other DLA or DAS routines.
      !>
      !>## Detailed Output
      !>
      !>     DLADSC   is the descriptor of the first DLA segment in the
      !>              file associated with HANDLE.
      !>
      !>              The segment descriptor layout is:
      !>
      !>                 +---------------+
      !>                 | BACKWARD PTR  | Linked list backward pointer
      !>                 +---------------+
      !>                 | FORWARD PTR   | Linked list forward pointer
      !>                 +---------------+
      !>                 | BASE INT ADDR | Base DAS integer address
      !>                 +---------------+
      !>                 | INT COMP SIZE | Size of integer segment component
      !>                 +---------------+
      !>                 | BASE DP ADDR  | Base DAS d.p. address
      !>                 +---------------+
      !>                 | DP COMP SIZE  | Size of d.p. segment component
      !>                 +---------------+
      !>                 | BASE CHR ADDR | Base DAS character address
      !>                 +---------------+
      !>                 | CHR COMP SIZE | Size of character segment component
      !>                 +---------------+
      !>
      !>              DLADSC is valid only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     FOUND    is a logical flag indicating whether a segment was
      !>              found. FOUND has the value .TRUE. if the file
      !>              contains at least one segment; otherwise FOUND is
      !>              .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If an error occurs while reading the DLA file, the error
      !>         is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See description of input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports forward traversal of a DLA file's segment
      !>     list. Note that it is not necessary to call this routine to
      !>     conduct a forward traversal; all that is necessary is to have
      !>     access to the first descriptor in the file, which this routine
      !>     provides.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Open a DLA file for read access, traverse the segment
      !>        list from front to back, and display segment address
      !>        and size attributes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLABFS_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>
      !>      INTEGER               CURRNT ( DLADSZ )
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               SEGNO
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the file to search.
      !>C
      !>      CALL PROMPT ( 'Name of DLA file > ', FNAME )
      !>
      !>C
      !>C     Open the DLA file for read access.  Since DLA
      !>C     files use the DAS architecture, we can use DAS
      !>C     routines to open and close the file.
      !>C
      !>      CALL DASOPR ( FNAME, HANDLE )
      !>
      !>C
      !>C     Begin a forward search.  Let DLADSC contain
      !>C     the descriptor of the first segment.
      !>C
      !>      SEGNO = 0
      !>
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>C
      !>C        Display the contents of the current segment
      !>C        descriptor.
      !>C
      !>         SEGNO = SEGNO + 1
      !>
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Segment number = ', SEGNO
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Backward segment pointer         = ',
      !>     .               DLADSC(BWDIDX)
      !>         WRITE (*,*) 'Forward segment pointer          = ',
      !>     .               DLADSC(FWDIDX)
      !>         WRITE (*,*) 'Character component base address = ',
      !>     .               DLADSC(CBSIDX)
      !>         WRITE (*,*) 'Character component size         = ',
      !>     .               DLADSC(CSZIDX)
      !>         WRITE (*,*) 'D.p. base address                = ',
      !>     .               DLADSC(DBSIDX)
      !>         WRITE (*,*) 'D.p. component size              = ',
      !>     .               DLADSC(DSZIDX)
      !>         WRITE (*,*) 'Integer base address             = ',
      !>     .               DLADSC(IBSIDX)
      !>         WRITE (*,*) 'Integer component size           = ',
      !>     .               DLADSC(ISZIDX)
      !>         WRITE (*,*) ' '
      !>
      !>C
      !>C        Find the next segment.
      !>C
      !>C        To avoid using DLADSC as both input and output
      !>C        in the following call (this use is not allowed
      !>C        by the ANSI Fortran 77 standard), we copy DLADSC
      !>C        into the variable CURRNT.  We then find the
      !>C        segment following CURRNT.
      !>C
      !>         CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
      !>         CALL DLAFNS ( HANDLE, CURRNT, DLADSC, FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file using the DAS close routine.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Name of DLA file > phobos512.bds
      !>
      !>         Segment number =            1
      !>
      !>         Backward segment pointer         =           -1
      !>         Forward segment pointer          =           -1
      !>         Character component base address =            0
      !>         Character component size         =            0
      !>         D.p. base address                =            0
      !>         D.p. component size              =      4737076
      !>         Integer base address             =           11
      !>         Integer component size           =     29692614
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR)
      !>
      !>        Changed output argument name DESCR to DLADSC for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        23-JAN-2013 (NJB)
      !>
      !>           Deleted third exception description from the $Exceptions
      !>           header section.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        10-FEB-2005 (NJB)
      module subroutine dlabfs ( ctx, handle, dladsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      integer(ip), intent(out) :: dladsc  ( * )  !! Descriptor of first segment in DLA file.
      logical, intent(out) :: found  !! Flag indicating whether a segment was found.
      integer(ip) :: this_1(1)  !! one-element copy of this (array argument)

      integer(ip) :: this

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     Look up the pointer to the first DLA segment descriptor in the
!     file.  Then look up the segment descriptor itself.
!
      call dasrdi ( ctx, handle, llbidx, llbidx, this_1 )
      this = this_1(1)

      if (  ctx%failed()  .or.  ( this == nulptr )  ) then
!
!        If the pointer THIS is null, there are no segments in the
!        file.
!
         return

      end if

!
!     Return the first descriptor.
!
      call dasrdi ( ctx, handle, this, this+dladsz-1, dladsc )

      found = .true.

      return
      end subroutine dlabfs

      !> Begin a backward segment search in a DLA file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle associated with the file to be
      !>              searched. This handle is used to identify the file in
      !>              subsequent calls to other DLA or DAS routines.
      !>
      !>## Detailed Output
      !>
      !>     DLADSC   is the descriptor of the last DLA segment in the
      !>              file associated with HANDLE.
      !>
      !>              The segment descriptor layout is:
      !>
      !>                 +---------------+
      !>                 | BACKWARD PTR  | Linked list backward pointer
      !>                 +---------------+
      !>                 | FORWARD PTR   | Linked list forward pointer
      !>                 +---------------+
      !>                 | BASE INT ADDR | Base DAS integer address
      !>                 +---------------+
      !>                 | INT COMP SIZE | Size of integer segment component
      !>                 +---------------+
      !>                 | BASE DP ADDR  | Base DAS d.p. address
      !>                 +---------------+
      !>                 | DP COMP SIZE  | Size of d.p. segment component
      !>                 +---------------+
      !>                 | BASE CHR ADDR | Base DAS character address
      !>                 +---------------+
      !>                 | CHR COMP SIZE | Size of character segment component
      !>                 +---------------+
      !>
      !>              DLADSC is valid only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     FOUND    is a logical flag indicating whether a segment was
      !>              found. FOUND has the value .TRUE. if the file
      !>              contains at least one segment; otherwise FOUND is
      !>              .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file handle is invalid, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If an error occurs while reading the DLA file, the error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the input descriptor is invalid, this routine will
      !>         fail in an unpredictable manner.
      !>
      !>## Files
      !>
      !>     See description of input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports backward traversal of a DLA file's segment
      !>     list. Note that it is not necessary to call this routine to
      !>     conduct a backward traversal; all that is necessary is to have
      !>     access to the last descriptor in the file, which this routine
      !>     provides.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Open a DLA file for read access, traverse the segment
      !>        list from back to front, and display segment address
      !>        and size attributes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLABBS_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>
      !>      INTEGER               CURRNT ( DLADSZ )
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               NSEGS
      !>      INTEGER               SEGNO
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the file to search.
      !>C
      !>      CALL PROMPT ( 'Name of DLA file > ', FNAME )
      !>
      !>C
      !>C     Open the DLA file for read access.  Since DLA
      !>C     files use the DAS architecture, we can use DAS
      !>C     routines to open and close the file.
      !>C
      !>      CALL DASOPR ( FNAME, HANDLE )
      !>
      !>C
      !>C     Count the segments in the file; this allows us
      !>C     to label the segments in our display.
      !>C
      !>      NSEGS = 0
      !>      CALL DLABBS ( HANDLE, DLADSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>
      !>         NSEGS = NSEGS + 1
      !>         CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
      !>         CALL DLAFPS ( HANDLE, CURRNT, DLADSC, FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Begin a backward search.  Let DLADSC contain
      !>C     the descriptor of the last segment.
      !>C
      !>      SEGNO = NSEGS + 1
      !>
      !>      CALL DLABBS ( HANDLE, DLADSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>C
      !>C        Display the contents of the current segment
      !>C        descriptor.
      !>C
      !>         SEGNO = SEGNO - 1
      !>
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Segment number = ', SEGNO
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Backward segment pointer         = ',
      !>     .               DLADSC(BWDIDX)
      !>         WRITE (*,*) 'Forward segment pointer          = ',
      !>     .               DLADSC(FWDIDX)
      !>         WRITE (*,*) 'Character component base address = ',
      !>     .               DLADSC(CBSIDX)
      !>         WRITE (*,*) 'Character component size         = ',
      !>     .               DLADSC(CSZIDX)
      !>         WRITE (*,*) 'D.p. base address                = ',
      !>     .               DLADSC(DBSIDX)
      !>         WRITE (*,*) 'D.p. component size              = ',
      !>     .               DLADSC(DSZIDX)
      !>         WRITE (*,*) 'Integer base address             = ',
      !>     .               DLADSC(IBSIDX)
      !>         WRITE (*,*) 'Integer component size           = ',
      !>     .               DLADSC(ISZIDX)
      !>         WRITE (*,*) ' '
      !>
      !>C
      !>C        Find the previous segment.
      !>C
      !>C        To avoid using DLADSC as both input and output
      !>C        in the following call (this use is not allowed
      !>C        by the ANSI Fortran 77 standard), we copy DLADSC
      !>C        into the variable CURRNT.  We then find the
      !>C        segment preceding CURRNT.
      !>C
      !>         CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
      !>         CALL DLAFPS ( HANDLE, CURRNT, DLADSC, FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file using the DAS close routine.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Name of DLA file > phobos512.bds
      !>
      !>         Segment number =            1
      !>
      !>         Backward segment pointer         =           -1
      !>         Forward segment pointer          =           -1
      !>         Character component base address =            0
      !>         Character component size         =            0
      !>         D.p. base address                =            0
      !>         D.p. component size              =      4737076
      !>         Integer base address             =           11
      !>         Integer component size           =     29692614
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed output argument name DESCR to DLADSC for consistency
      !>        with other routines.
      !>
      !>        Edits the header comments to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 21-APR-2010 (NJB)
      module subroutine dlabbs ( ctx, handle, dladsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      integer(ip), intent(out) :: dladsc  ( * )  !! Descriptor of last segment in DLA file.
      logical, intent(out) :: found  !! Flag indicating whether a segment was found.
      integer(ip) :: this_1(1)  !! one-element copy of this (array argument)

      integer(ip) :: this

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     Look up the pointer to the last DLA segment descriptor in the
!     file.  Then look up the segment descriptor itself.
!
      call dasrdi ( ctx, handle, lleidx, lleidx, this_1 )
      this = this_1(1)

      if (  ctx%failed()  .or.  ( this == nulptr )  ) then
!
!        If the pointer THIS is null, there are no segments in the
!        file.
!
         return

      end if

!
!     Return the last descriptor.
!
      call dasrdi ( ctx, handle, this, this+dladsz-1, dladsc )

      found = .true.

      return
      end subroutine dlabbs

      !> Find the segment following a specified segment in a DLA file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle associated with the file to be
      !>              searched. This handle is used to identify the file in
      !>              subsequent calls to other DLA or DAS routines.
      !>
      !>     DLADSC   is the descriptor of a DLA segment in the file
      !>              associated with HANDLE. The descriptor of the
      !>              segment following DLADSC is sought.
      !>
      !>              The segment descriptor layout is:
      !>
      !>                 +---------------+
      !>                 | BACKWARD PTR  | Linked list backward pointer
      !>                 +---------------+
      !>                 | FORWARD PTR   | Linked list forward pointer
      !>                 +---------------+
      !>                 | BASE INT ADDR | Base DAS integer address
      !>                 +---------------+
      !>                 | INT COMP SIZE | Size of integer segment component
      !>                 +---------------+
      !>                 | BASE DP ADDR  | Base DAS d.p. address
      !>                 +---------------+
      !>                 | DP COMP SIZE  | Size of d.p. segment component
      !>                 +---------------+
      !>                 | BASE CHR ADDR | Base DAS character address
      !>                 +---------------+
      !>                 | CHR COMP SIZE | Size of character segment component
      !>                 +---------------+
      !>
      !>## Detailed Output
      !>
      !>     NXTDSC   is the descriptor of the next DLA segment following
      !>              the segment associated with the input argument DLADSC.
      !>
      !>              NXTDSC is valid only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     FOUND    is a logical flag indicating whether the next segment
      !>              was found. FOUND has the value .TRUE. if the segment
      !>              was found; otherwise FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If an error occurs while reading the DLA file, the error
      !>         is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     3)  If the input descriptor is invalid, this routine will
      !>         fail in an unpredictable manner.
      !>
      !>## Files
      !>
      !>     See description of input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports forward traversal of a DLA file's segment
      !>     list. A forward traversal may be started from any segment in
      !>     the file; it is not necessary to call DLABFS first. The role
      !>     of DLABFS is simply to return the descriptor of the first
      !>     segment in the file.
      !>
      !>## Examples
      !>
      !>     1) Open a DLA file for read access, traverse the segment
      !>        list from front to back, and display segment address
      !>        and size attributes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLAFNS_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>
      !>      INTEGER               CURRNT ( DLADSZ )
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               SEGNO
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the file to search.
      !>C
      !>      CALL PROMPT ( 'Name of DLA file > ', FNAME )
      !>
      !>C
      !>C     Open the DLA file for read access.  Since DLA
      !>C     files use the DAS architecture, we can use DAS
      !>C     routines to open and close the file.
      !>C
      !>      CALL DASOPR ( FNAME, HANDLE )
      !>
      !>C
      !>C     Begin a forward search.  Let DLADSC contain
      !>C     the descriptor of the first segment.
      !>C
      !>      SEGNO = 0
      !>
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>C
      !>C        Display the contents of the current segment
      !>C        descriptor.
      !>C
      !>         SEGNO = SEGNO + 1
      !>
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Segment number = ', SEGNO
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Backward segment pointer         = ',
      !>     .               DLADSC(BWDIDX)
      !>         WRITE (*,*) 'Forward segment pointer          = ',
      !>     .               DLADSC(FWDIDX)
      !>         WRITE (*,*) 'Character component base address = ',
      !>     .               DLADSC(CBSIDX)
      !>         WRITE (*,*) 'Character component size         = ',
      !>     .               DLADSC(CSZIDX)
      !>         WRITE (*,*) 'D.p. base address                = ',
      !>     .               DLADSC(DBSIDX)
      !>         WRITE (*,*) 'D.p. component size              = ',
      !>     .               DLADSC(DSZIDX)
      !>         WRITE (*,*) 'Integer base address             = ',
      !>     .               DLADSC(IBSIDX)
      !>         WRITE (*,*) 'Integer component size           = ',
      !>     .               DLADSC(ISZIDX)
      !>         WRITE (*,*) ' '
      !>
      !>C
      !>C        Find the next segment.
      !>C
      !>C        To avoid using DLADSC as both input and output
      !>C        in the following call (this use is not allowed
      !>C        by the ANSI Fortran 77 standard), we copy DLADSC
      !>C        into the variable CURRNT.  We then find the
      !>C        segment following CURRNT.
      !>C
      !>         CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
      !>         CALL DLAFNS ( HANDLE, CURRNT, DLADSC, FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file using the DAS close routine.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Name of DLA file > phobos512.bds
      !>
      !>         Segment number =            1
      !>
      !>         Backward segment pointer         =           -1
      !>         Forward segment pointer          =           -1
      !>         Character component base address =            0
      !>         Character component size         =            0
      !>         D.p. base address                =            0
      !>         D.p. component size              =      4737076
      !>         Integer base address             =           11
      !>         Integer component size           =     29692614
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR)
      !>
      !>        Changed input argument name DESCR to DLADSC for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        23-JAN-2013 (NJB)
      !>
      !>           Added to $Exceptions header section a mention
      !>           of the invalid input descriptor error case.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        10-FEB-2005 (NJB)
      module subroutine dlafns ( ctx, handle, dladsc, nxtdsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      integer(ip), intent(in) :: dladsc ( * )  !! Descriptor of a segment in DLA file.
      integer(ip), intent(out) :: nxtdsc ( * )  !! Descriptor of next segment in DLA file.
      logical, intent(out) :: found  !! Flag indicating whether a segment was found.

      integer(ip) :: fwd

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     Extract the forward pointer from the segment descriptor.
!
      fwd = dladsc( fwdidx )

      if ( fwd == nulptr ) then
!
!        There is no segment following the input segment.
!
         return

      end if

!
!     Look up the next descriptor
!
      call dasrdi ( ctx, handle, fwd, fwd+dladsz-1, nxtdsc )

      found = .true.

      return
      end subroutine dlafns

      !> Find the segment preceding a specified segment in a DLA file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle associated with the file to be
      !>              searched. This handle is used to identify the file in
      !>              subsequent calls to other DLA or DAS routines.
      !>
      !>     DLADSC   is the descriptor of a DLA segment in the file
      !>              associated with HANDLE. The descriptor of the
      !>              segment preceding DLADSC is sought.
      !>
      !>              The segment descriptor layout is:
      !>
      !>                 +---------------+
      !>                 | BACKWARD PTR  | Linked list backward pointer
      !>                 +---------------+
      !>                 | FORWARD PTR   | Linked list forward pointer
      !>                 +---------------+
      !>                 | BASE INT ADDR | Base DAS integer address
      !>                 +---------------+
      !>                 | INT COMP SIZE | Size of integer segment component
      !>                 +---------------+
      !>                 | BASE DP ADDR  | Base DAS d.p. address
      !>                 +---------------+
      !>                 | DP COMP SIZE  | Size of d.p. segment component
      !>                 +---------------+
      !>                 | BASE CHR ADDR | Base DAS character address
      !>                 +---------------+
      !>                 | CHR COMP SIZE | Size of character segment component
      !>                 +---------------+
      !>
      !>## Detailed Output
      !>
      !>     PRVDSC   is the descriptor of the DLA segment preceding the
      !>              segment associated with the input argument DLADSC.
      !>
      !>              PRVDSC is valid only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     FOUND    is a logical flag indicating whether the previous
      !>              segment was found. FOUND has the value .TRUE. if the
      !>              segment was found; otherwise FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file handle is invalid, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If a failure occurs while reading the DLA file, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See description of input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports backward traversal of a DLA file's segment
      !>     list. A backward traversal may be started from any segment in
      !>     the file; it is not necessary to call DLABBS first. The role of
      !>     DLABBS is simply to return the descriptor of the last segment in
      !>     the file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Open a DLA file for read access, traverse the segment
      !>        list from back to front, and display segment address
      !>        and size attributes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLAFPS_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>
      !>      INTEGER               CURRNT ( DLADSZ )
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               NSEGS
      !>      INTEGER               SEGNO
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the file to search.
      !>C
      !>      CALL PROMPT ( 'Name of DLA file > ', FNAME )
      !>
      !>C
      !>C     Open the DLA file for read access.  Since DLA
      !>C     files use the DAS architecture, we can use DAS
      !>C     routines to open and close the file.
      !>C
      !>      CALL DASOPR ( FNAME, HANDLE )
      !>
      !>C
      !>C     Count the segments in the file; this allows us
      !>C     to label the segments in our display.
      !>C
      !>      NSEGS = 0
      !>      CALL DLABBS ( HANDLE, DLADSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>
      !>         NSEGS = NSEGS + 1
      !>         CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
      !>         CALL DLAFPS ( HANDLE, CURRNT, DLADSC, FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Begin a backward search.  Let DLADSC contain
      !>C     the descriptor of the last segment.
      !>C
      !>      SEGNO = NSEGS + 1
      !>
      !>      CALL DLABBS ( HANDLE, DLADSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>C
      !>C        Display the contents of the current segment
      !>C        descriptor.
      !>C
      !>         SEGNO = SEGNO - 1
      !>
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Segment number = ', SEGNO
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Backward segment pointer         = ',
      !>     .               DLADSC(BWDIDX)
      !>         WRITE (*,*) 'Forward segment pointer          = ',
      !>     .               DLADSC(FWDIDX)
      !>         WRITE (*,*) 'Character component base address = ',
      !>     .               DLADSC(CBSIDX)
      !>         WRITE (*,*) 'Character component size         = ',
      !>     .               DLADSC(CSZIDX)
      !>         WRITE (*,*) 'D.p. base address                = ',
      !>     .               DLADSC(DBSIDX)
      !>         WRITE (*,*) 'D.p. component size              = ',
      !>     .               DLADSC(DSZIDX)
      !>         WRITE (*,*) 'Integer base address             = ',
      !>     .               DLADSC(IBSIDX)
      !>         WRITE (*,*) 'Integer component size           = ',
      !>     .               DLADSC(ISZIDX)
      !>         WRITE (*,*) ' '
      !>
      !>C
      !>C        Find the previous segment.
      !>C
      !>C        To avoid using DLADSC as both input and output
      !>C        in the following call (this use is not allowed
      !>C        by the ANSI Fortran 77 standard), we copy DLADSC
      !>C        into the variable CURRNT.  We then find the
      !>C        segment preceding CURRNT.
      !>C
      !>         CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
      !>         CALL DLAFPS ( HANDLE, CURRNT, DLADSC, FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file using the DAS close routine.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Name of DLA file > phobos512.bds
      !>
      !>         Segment number =            1
      !>
      !>         Backward segment pointer         =           -1
      !>         Forward segment pointer          =           -1
      !>         Character component base address =            0
      !>         Character component size         =            0
      !>         D.p. base address                =            0
      !>         D.p. component size              =      4737076
      !>         Integer base address             =           11
      !>         Integer component size           =     29692614
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed input argument name DESCR to DLADSC for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added solution for code example.
      !>
      !>-    SPICELIB Version 1.0.0, 06-JAN-2017 (NJB)
      !>
      !>     Original version 21-APR-2010 (NJB)
      module subroutine dlafps ( ctx, handle, dladsc, prvdsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      integer(ip), intent(in) :: dladsc ( * )  !! Descriptor of a segment in DLA file.
      integer(ip), intent(out) :: prvdsc ( * )  !! Descriptor of previous segment in DLA file.
      logical, intent(out) :: found  !! Flag indicating whether a segment was found.

      integer(ip) :: bwd

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Nothing found yet.
!
      found = .false.

!
!     Extract the backward pointer from the segment descriptor.
!
      bwd = dladsc( bwdidx )

      if ( bwd == nulptr ) then
!
!        There is no segment preceding the input segment.
!
         return

      end if

!
!     Look up the previous descriptor
!
      call dasrdi ( ctx, handle, bwd, bwd+dladsz-1, prvdsc )

      found = .true.

      return
      end subroutine dlafps

      !> Return a logical value indicating whether a two DLA
      !> segments, each identified by DAS handle and DLA descriptor,
      !> are in fact the same segment.
      !>
      !> The function returns .TRUE. if and only if the DLA segments
      !> match.
      !>
      !>## Detailed Input
      !>
      !>     HAN1     is the integer handle associated with a DLA file.
      !>              The file is open for read access.
      !>
      !>     HAN2     is the integer handle associated with a second DLA
      !>              file. The file is open for read access.
      !>
      !>     DSC1     is the DLA descriptor of a segment in the file
      !>              associated with HAN1.
      !>
      !>     DSC2     is the DLA descriptor of a segment in the file
      !>              associated with HAN2.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if and only if the DLA segments
      !>     match. The segments are considered to match if and only if the
      !>     input handles match and all elements of the DLA descriptors
      !>     match.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the inputs are invalid, this routine will
      !>         fail in an unspecified manner.
      !>
      !>## Files
      !>
      !>     See description of input arguments HAN1 and HAN2.
      !>
      !>## Particulars
      !>
      !>     DLA files are built using the DAS low-level format; DLA files are
      !>     a specialized type of DAS file in which data are organized as a
      !>     doubly linked list of segments. Each segment's data belong to
      !>     contiguous components of character, double precision, and integer
      !>     type.
      !>
      !>     This routine supports DLA and DSK routines by enabling
      !>     them to determine whether a given DLA segment matches one
      !>     they've previously examined. This may allow such routines
      !>     to avoid buffering information redundantly.
      !>
      !>## Examples
      !>
      !>     1)  A typical use of this routine is to enable a subroutine
      !>         to determine whether a DLA segment identified by a
      !>         handle and DLA descriptor matches one seen previously.
      !>         The logic of such a test can be implemented as follows:
      !>
      !>                   SUBROUTINE SUBA ( HANDLE, DLADSC )
      !>                   IMPLICIT NONE
      !>
      !>                   INCLUDE 'dla.inc'
      !>
      !>                   INTEGER               HANDLE
      !>                   INTEGER               DLADSC ( * )
      !>
      !>             C
      !>             C     SPICELIB functions
      !>             C
      !>                   LOGICAL               DLASSG
      !>                   LOGICAL               FAILED
      !>             C
      !>             C     Local variables
      !>             C
      !>                   INTEGER               PRVDSC ( DLADSZ )
      !>                   INTEGER               PRVHAN
      !>
      !>             C
      !>             C     Saved variables
      !>             C
      !>                   SAVE                  PRVDSC
      !>                   SAVE                  PRVHAN
      !>
      !>             C
      !>             C     Initial values
      !>             C
      !>                   DATA                  PRVHAN / 0 /
      !>
      !>                   ...
      !>
      !>                   IF ( .NOT. DLASSG( HANDLE, PRVHAN,
      !>                  .                   DLADSC, PRVDSC ) ) THEN
      !>
      !>                      [Examine segment]
      !>
      !>                      IF ( .NOT. FAILED() ) THEN
      !>             C
      !>             C           Save values only if no error occurred.
      !>             C
      !>                         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC )
      !>                         PRVHAN = HANDLE
      !>
      !>                      END IF
      !>
      !>                   END IF
      !>
      !>                   [Normal case]
      !>
      !>                   ...
      !>
      !>                   END
      !>
      !>## Restrictions
      !>
      !>     1)  This routine relies on uniqueness of DAS file handles.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 22-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-MAY-2016 (NJB)
      module logical function dlassg ( ctx, han1, han2, dsc1, dsc2 )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: han1  !! Handle of an open DLA file.
      integer(ip), intent(in) :: han2  !! Handle of a second open DLA file.
      integer(ip), intent(in) :: dsc1 ( dladsz )  !! DLA descriptor of a segment in the first
                                                  !! file.
      integer(ip), intent(in) :: dsc2 ( dladsz )  !! DLA descriptor of a segment in the second
                                                  !! file.

      integer(ip) :: i

!
!     Give the function an initial value.
!
      dlassg = .false.

!
!     If the handles don't match, we're done.
!
      if ( han1 /= han2 ) return
!
!     Compare the DLA descriptors. All elements, including pointers,
!     must match in order to have a matching result.
!
      do i = 1, dladsz

         if ( dsc1(i) /= dsc2(i) ) return

      end do
!
!     At this point, everything's a match.
!
      dlassg = .true.

      return
      end function dlassg

      !> Open a new DSK file for subsequent write operations.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a new DSK file to be created. The
      !>              file will be left opened for write access.
      !>
      !>     IFNAME   is the internal file name for the new file. The name
      !>              may contain as many as 60 characters. All characters
      !>              of IFNAME should be printing characters (ASCII codes
      !>              32-126 decimal). This name should uniquely identify
      !>              the file.
      !>
      !>     NCOMCH   is the number of comment characters to allocate.
      !>              Allocating comment characters at file creation time
      !>              may reduce the likelihood of having to expand the
      !>              comment area later.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the file handle associated with the file. This
      !>              handle is used to identify the file in subsequent
      !>              calls to other DSK routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input filename is blank, an error is signaled by a
      !>         routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     2)  If the specified file cannot be opened without exceeding the
      !>         maximum allowed number of open DAS files, an error is signaled
      !>         by a routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     3)  If the file cannot be opened properly, an error is signaled by
      !>         a routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     4)  If the initial records in the file cannot be written, an error
      !>         is signaled by a routine in the call tree of this routine. No
      !>         file will be created.
      !>
      !>     5)  If no logical units are available, an error is signaled by a
      !>         routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     6)  If the internal file name contains nonprinting characters
      !>         (ASCII codes decimal 0-31 and 127-255), an error is signaled
      !>         by a routine in the call tree of this routine. No file will be
      !>         created.
      !>
      !>     7)  If the number of comment characters allocated NCOMCH is
      !>         negative, an error is signaled by a routine in the call
      !>         tree of this routine. No file will be created.
      !>
      !>## Files
      !>
      !>     See argument FNAME.
      !>
      !>## Particulars
      !>
      !>     DSK files are built using the DLA low-level format and
      !>     the DAS architecture; DLA files are a specialized type of DAS
      !>     file in which data are organized as a doubly linked list of
      !>     segments. Each segment's data belong to contiguous components of
      !>     character, double precision, and integer type.
      !>
      !>     This routine creates a new DSK file and sets the type of the
      !>     file to the mnemonic code passed to it.
      !>
      !>     DSK files created by this routine have initialized file records.
      !>     The ID word in a DSK file record has the form
      !>
      !>        DAS/DSK
      !>
      !>     where the characters following the slash are supplied by the
      !>     caller of this routine.
      !>
      !>## Examples
      !>
      !>     1)  Create a new DSK file, using an internal file name that
      !>         attempts to serve as an unique identifier. No room for
      !>         comments will be reserved.
      !>
      !>            FNAME  =  'TEST.DSK'
      !>            IFNAME =  'TEST.DSK/NAIF/NJB/20-OCT-2006/14:37:00'
      !>            NCOMCH =   0
      !>
      !>            CALL DSKOPN ( FNAME, IFNAME, NCOMCH, HANDLE )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Corrected a few header typos.
      !>
      !>        29-APR-2010 (NJB)
      !>
      !>           Now passes NCOMCH to DLAOPN.
      !>
      !>        08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        20-OCT-2006 (NJB)
      module subroutine dskopn ( ctx, fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of a DSK file to be opened.
      character(*), intent(in) :: ifname  !! Internal file name.
      integer(ip), intent(in) :: ncomch  !! Number of comment characters to allocate.
      integer(ip), intent(out) :: handle  !! Handle assigned to the opened DSK file.

      if ( ctx%failed() ) return

      call dlaopn ( ctx, fname, 'DSK', ifname, ncomch, handle )

      return
      end subroutine dskopn

      !> Close a DSK file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the DAS file handle associated with the file.
      !>              The file may be open for read or write access.
      !>
      !>     OPTMIZ   is a logical flag indicating whether the DSK
      !>              should be segregated before it is closed. This
      !>              option applies only to files open for write
      !>              access. The value of OPTMIZ has no effect for
      !>              files opened for read access.
      !>
      !>              See the DAS Required Reading das.req for a
      !>              discussion of segregation of DAS files.
      !>
      !>## Detailed Output
      !>
      !>     None. This routine operates by side effects.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs when the file is closed, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine provides a DSK-level interface for closing DSK files.
      !>
      !>     In cases where DSKs opened for write access are to be closed
      !>     without segregation, this interface is slightly simpler than that
      !>     available at the DAS level.
      !>
      !>## Examples
      !>
      !>     1) Close a new DSK file using DAS segregation. HANDLE
      !>        is the DAS file handle of the DSK.
      !>
      !>        This is the normal choice for DSK creation.
      !>
      !>           CALL DSKCLS ( HANDLE, .TRUE. )
      !>
      !>     2) Close a new DSK file without using DAS segregation. The
      !>        close operation will be fast, but reading the file will be
      !>        less efficient than if the file had been segregated.
      !>
      !>           CALL DSKCLS ( HANDLE, .TRUE. )
      !>
      !>     3) Close an existing DSK file that had been opened
      !>        for read access. In this case OPTMIZ is ignored:
      !>
      !>           CALL DSKCLS ( HANDLE, .FALSE. )
      !>
      !>        or
      !>
      !>           CALL DSKCLS ( HANDLE, .TRUE. )
      !>
      !>## Restrictions
      !>
      !>     1)  This routine should not be called by user applications
      !>         that have loaded a DSK file via FURNSH. Such applications
      !>         should call the KEEPER entry points UNLOAD or KCLEAR instead.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-OCT-2021 (JDR) (NJB)
      !>
      !>        Bug fix: now calls FAILED after call to DASHAM.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        09-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        20-OCT-2006 (NJB)
      !>
      !>           Original DSKLIB version.
      module subroutine dskcls ( ctx, handle, optmiz )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle assigned to the opened DSK file.
      logical, intent(in) :: optmiz  !! Flag indicating whether to segregate the DSK.

      integer(ip), parameter :: methln = 10

      character(methln) :: method

      if ( ctx%failed() ) return

      if ( optmiz ) then
!
!        Segregate the file to enable fast read access.  This is
!        the "normal" way to close a DSK.  Segregating a large file
!        can be slow, however.
!
         call dascls ( ctx, handle )

      else
!
!        Close the file without first segregating it; this allows
!        the caller to close the file quickly, but results in a
!        file that will be read more slowly.
!
!        Any buffered data to be written must be explicitly flushed
!        to the file, if the file is open for write access.
!
         call dasham ( ctx, handle, method )

         if ( ctx%failed() ) return

         if ( method == 'WRITE ' ) then
!
!           Write out any buffered records belonging to the
!           indicated file.
!
            call daswbr ( ctx, handle )

         end if

!
!        Close the file without segregating records.
!
         call dasllc ( ctx, handle )

      end if

      return
      end subroutine dskcls

      !> Retrieve the value of a specified DSK tolerance or margin
      !> parameter.
      !>
      !>## Detailed Input
      !>
      !>     KEYWRD   is an integer code specifying the parameter to
      !>              retrieve. See the include file dsktol.inc for
      !>              a description of the possible keywords.
      !>
      !>## Detailed Output
      !>
      !>     DPVAL    is the value of the parameter specified by KEYWRD.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dsktol.inc
      !>
      !>     for descriptions and values of the tolerance or margin parameters
      !>     accessed by this routine, and of the keyword parameters used to
      !>     refer to them.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input keyword is not recognized, the error
      !>         SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The routines in this package serve to centralize numeric
      !>     tolerance and margin values used by the DSK subsystem.
      !>     The subsystem retrieves values from this package to use
      !>     at run time.
      !>
      !>     The entry points of this routine are
      !>
      !>        DSKGTL {DSK, get tolerance value}
      !>        DSKSTL {DSK, set tolerance value}
      !>
      !>     To minimize run time overhead, the "keywords" used by
      !>     these routines to identify parameters are actually
      !>     integer codes.
      !>
      !>     SPICE users may override certain values maintained by
      !>     this package; others values are fixed. It is recommended
      !>     that any change to the tolerance values made at run
      !>     time be performed only by expert SPICE users.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Obtain and display the DSK type 2 plate expansion fraction.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKGTL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include files
      !>C
      !>      INCLUDE 'dsktol.inc'
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DPVAL
      !>
      !>      CALL DSKGTL ( KEYXFR, DPVAL )
      !>
      !>      WRITE(*,*) 'Plate expansion fraction = ', DPVAL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Plate expansion fraction =    1.0000000000000000E-010
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        complete example code.
      !>
      !>-    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB)
      module subroutine dskgtl ( ctx, keywrd, dpval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: keywrd  !! Code specifying parameter to retrieve.
      real(wp), intent(out) :: dpval  !! Value of parameter.
      logical :: return
      character(namlen), parameter :: names  ( ndspar ) = &
         [ character(namlen) :: 'XFRACT', 'SGREED', 'SGPADM', 'PTMEMM', 'ANGMRG', 'LONALI' ]
      logical, parameter :: isfixd ( ndspar ) = &
         [ .false., .false., .false., .false., .true., .true. ]
      associate ( dppars => ctx%spice%dsktol%dppars )
      if (  ( keywrd < 1 ) .or. ( keywrd > ndspar )  ) then

         call ctx%setmsg ( 'Valid keyword range is 1:#; keyword was #.' )
         call ctx%errint ( '#', ndspar                                  )
         call ctx%errint ( '#', keywrd                                  )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                     )
         return

      end if

      dpval = dppars( keywrd )

      return
      end associate
      end subroutine dskgtl

      !> Set the value of a specified DSK tolerance or margin parameter.
      !>
      !>## Detailed Input
      !>
      !>     KEYWRD   is an integer code specifying the parameter to
      !>              set. See the include file dsktol.inc for
      !>              a description of the possible keywords.
      !>
      !>     DPVAL    is the new value of the parameter specified by KEYWRD.
      !>              This value will be retrieved by future calls to
      !>              to DSKGTL made to retrieve the specified parameter.
      !>
      !>              <<< Use extreme caution. This routine performs no
      !>              checks on DPVAL. >>>
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>     This routine operates by side effects.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dsktol.inc
      !>
      !>     for descriptions and values of the tolerance or margin parameters
      !>     accessed by this routine, and of the keyword parameters used to
      !>     refer to them.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input keyword is not recognized, the error
      !>         SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>     2)  If an attempt is made to modify a fixed parameter,
      !>         the error SPICE(IMMUTABLEVALUE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The routines in this package serve to centralize numeric
      !>     tolerance and margin values used by the DSK subsystem.
      !>     The subsystem retrieves values from this package to use
      !>     at run time.
      !>
      !>     The entry points of this routine are
      !>
      !>        DSKGTL {DSK, get tolerance value}
      !>        DSKSTL {DSK, set tolerance value}
      !>
      !>     To minimize run time overhead, the "keywords" used by
      !>     these routines to identify parameters are actually
      !>     integer codes.
      !>
      !>     SPICE users may override certain values maintained by
      !>     this package; others values are fixed. It is recommended
      !>     that any change to the tolerance values made at run
      !>     time be performed only by expert SPICE users.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Obtain, display, and update the DSK type 2 plate expansion
      !>        fraction.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKSTL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include files
      !>C
      !>      INCLUDE 'dsktol.inc'
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DPVAL
      !>
      !>      CALL DSKGTL ( KEYXFR, DPVAL )
      !>
      !>      WRITE(*,*) 'Default plate expansion fraction = ', DPVAL
      !>
      !>C
      !>C     Update the parameter.
      !>C
      !>      CALL DSKSTL ( KEYXFR, 1.D-8 )
      !>
      !>C
      !>C     Verify the update.
      !>C
      !>      CALL DSKGTL ( KEYXFR, DPVAL )
      !>
      !>      WRITE(*,*) 'New plate expansion fraction     = ', DPVAL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Default plate expansion fraction =    1.0000000000000000E-010
      !>         New plate expansion fraction     =    1.0000000000000000E-008
      !>
      !>## Restrictions
      !>
      !>     1)  The default settings used by the DSK subsystem should
      !>         be overridden only by expert SPICE users.
      !>
      !>     2)  This routine does not check the  validity of new parameter
      !>         values supplied by the calling application.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        complete example code.
      !>
      !>-    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB)
      module subroutine dskstl ( ctx, keywrd, dpval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: keywrd  !! Code specifying parameter to modify.
      real(wp), intent(in) :: dpval  !! New value of parameter.
      character(namlen), parameter :: names  ( ndspar ) = &
         [ character(namlen) :: 'XFRACT', 'SGREED', 'SGPADM', 'PTMEMM', 'ANGMRG', 'LONALI' ]
      logical, parameter :: isfixd ( ndspar ) = &
         [ .false., .false., .false., .false., .true., .true. ]
      associate ( dppars => ctx%spice%dsktol%dppars )
      if ( ctx%failed() ) return

      if (  ( keywrd < 1 ) .or. ( keywrd > ndspar )  ) then

         call ctx%setmsg ( 'Valid keyword range is 1:#; keyword was #.' )
         call ctx%errint ( '#', ndspar                                  )
         call ctx%errint ( '#', keywrd                                  )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                     )
         return

      end if

      if ( isfixd(keywrd) ) then

         call ctx%setmsg ( 'The parameter # cannot be modified.' )
         call ctx%errch  ( '#', names(keywrd)                    )
         call ctx%sigerr ( 'SPICE(IMMUTABLEVALUE)'               )
         return

      end if

!
!     We have a valid parameter index. We don't check
!     the new parameter value; the user presumably knows
!     the reason for change.
!
      dppars(keywrd) = dpval

      return
      end associate
      end subroutine dskstl

      !> Return the DSK descriptor from a DSK segment identified
      !> by a DAS handle and DLA descriptor.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file that is open for
      !>              read access.
      !>
      !>     DLADSC   is the DLA segment descriptor corresponding to
      !>              a DSK segment.
      !>
      !>## Detailed Output
      !>
      !>     DSKDSC   is the DSK segment descriptor of the segment
      !>              designated by the input handle and DLA descriptor.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>## Exceptions
      !>
      !>     1)  If the size of the double precision component of the
      !>         segment is smaller than that of a DSK descriptor, the
      !>         error SPICE(INVALIDFORMAT) is signaled.
      !>
      !>     2)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>     4)  If any DAS read error is detected, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This is a convenience routine intended for use by low-level
      !>     routines that read DSK segments. This routine may also be called
      !>     by user applications that must access DSK files at the segment
      !>     level.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Dump the DSK descriptors of a DSK file.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKGD_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               NXTDSC ( DLADSZ )
      !>
      !>      LOGICAL               FOUND
      !>
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>C
      !>C     Open the DSK file and begin a forward search
      !>C     for segments.
      !>C
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>      CALL DLABFS ( HANDLE, NXTDSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>C
      !>C        Make the DLA descriptor we just fetched
      !>C        the current one.
      !>C
      !>         CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )
      !>
      !>         CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      !>
      !>         WRITE (*,*) 'DSK descriptor contents: '
      !>
      !>         DO I = 1, DSKDSZ
      !>            WRITE (*,*) DSKDSC(I)
      !>         END DO
      !>C
      !>C        Find the next segment, if it exists.
      !>C
      !>         CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>         DSK descriptor contents:
      !>           401.00000000000000
      !>           401.00000000000000
      !>           1.0000000000000000
      !>           2.0000000000000000
      !>           10021.000000000000
      !>           1.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>           0.0000000000000000
      !>          -3.1415926535897931
      !>           3.1415926535897931
      !>          -1.5707963267948966
      !>           1.5707963267948966
      !>           8.0496322487215526
      !>           13.940939832123945
      !>          -1577879958.8160586
      !>           1577880066.1839132
      !>
      !>     2) Again, dump the DSK descriptors of a DSK file, this time
      !>        interpreting the descriptor information and displaying
      !>        it in a user-friendly form. This display is a simplified
      !>        version of that created by the utility DSKBRIEF.
      !>
      !>        This program requests the name of an optional meta-kernel.
      !>        The meta-kernel can be used to define surface name-ID
      !>        associations. If no meta-kernel is needed, the user can
      !>        enter a carriage return at the prompt for this file.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKGD_EX2
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>      INCLUDE 'srftrn.inc'
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,2(F19.12))' )
      !>
      !>      CHARACTER*(*)         FMT2
      !>      PARAMETER           ( FMT2 = '(A,I3)' )
      !>
      !>      INTEGER               BDNMLN
      !>      PARAMETER           ( BDNMLN = 36 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               FRNMLN
      !>      PARAMETER           ( FRNMLN = 32 )
      !>
      !>      INTEGER               NAMLEN
      !>      PARAMETER           ( NAMLEN = 30 )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 40 )
      !>
      !>      INTEGER               NSYS
      !>      PARAMETER           ( NSYS   = 4 )
      !>
      !>      INTEGER               NCLASS
      !>      PARAMETER           ( NCLASS = 2 )
      !>
      !>      INTEGER               CLNMLN
      !>      PARAMETER           ( CLNMLN = 25 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(BDNMLN)    BODNAM
      !>      CHARACTER*(TIMLEN)    BTIME
      !>      CHARACTER*(CLNMLN)    CLSNMS ( NCLASS )
      !>      CHARACTER*(FILSIZ)    DSK
      !>      CHARACTER*(TIMLEN)    ETIME
      !>      CHARACTER*(FRNMLN)    FRAME
      !>      CHARACTER*(FILSIZ)    META
      !>      CHARACTER*(SFNMLN)    SRFNAM
      !>      CHARACTER*(NAMLEN)    SYSNAM
      !>      CHARACTER*(NAMLEN)    SYSNMS ( NSYS )
      !>
      !>      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      !>      DOUBLE PRECISION      F
      !>      DOUBLE PRECISION      RE
      !>      DOUBLE PRECISION      RP
      !>
      !>      INTEGER               BODYID
      !>      INTEGER               CORSYS
      !>      INTEGER               DCLASS
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               DTYPE
      !>      INTEGER               FRAMID
      !>      INTEGER               HANDLE
      !>      INTEGER               NXTDSC ( DLADSZ )
      !>      INTEGER               SEGNO
      !>      INTEGER               SURFID
      !>
      !>      LOGICAL               FOUND
      !>      LOGICAL               ISNAME
      !>
      !>C
      !>C     Initial values
      !>C
      !>      DATA                  CLSNMS / 'Single-valued surface',
      !>     .                               'General surface'       /
      !>
      !>      DATA                  SYSNMS / 'Latitudinal',
      !>     .                               'Cylindrical',
      !>     .                               'Rectangular',
      !>     .                               'Planetodetic' /
      !>
      !>      CALL PROMPT ( 'Enter DSK name         > ', DSK  )
      !>      CALL PROMPT ( 'Enter meta-kernel name > ', META )
      !>
      !>      IF ( META .NE. ' ' ) THEN
      !>         CALL FURNSH ( META )
      !>      END IF
      !>
      !>C
      !>C     Open the DLA file and begin a forward search
      !>C     for segments.
      !>C
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>      SEGNO = 0
      !>
      !>      CALL DLABFS ( HANDLE, NXTDSC, FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>
      !>         SEGNO = SEGNO + 1
      !>C
      !>C        Make the DLA descriptor we just fetched
      !>C        the current one.
      !>C
      !>         CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )
      !>
      !>         CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      !>
      !>         BODYID = NINT( DSKDSC(CTRIDX) )
      !>         SURFID = NINT( DSKDSC(SRFIDX) )
      !>         FRAMID = NINT( DSKDSC(FRMIDX) )
      !>         DTYPE  = NINT( DSKDSC(TYPIDX) )
      !>         DCLASS = NINT( DSKDSC(CLSIDX) )
      !>
      !>         CALL BODC2S ( BODYID, BODNAM )
      !>         CALL SRFC2S ( SURFID, BODYID, SRFNAM, ISNAME )
      !>         CALL FRMNAM ( FRAMID, FRAME  )
      !>
      !>         IF ( FRAME .EQ. ' ' ) THEN
      !>            CALL INTSTR ( FRAMID, FRAME )
      !>         END IF
      !>
      !>         CALL ETCAL ( DSKDSC(BTMIDX), BTIME )
      !>         CALL ETCAL ( DSKDSC(ETMIDX), ETIME )
      !>
      !>         CORSYS = NINT( DSKDSC(SYSIDX) )
      !>
      !>         SYSNAM = SYSNMS( CORSYS )
      !>
      !>         WRITE (*,*)    '===================================='
      !>         WRITE (*,FMT2) ' DSK descriptor for segment ',
      !>     .                  SEGNO
      !>         WRITE (*,*)    '  Body:              ', BODNAM
      !>         WRITE (*,*)    '  Surface:           ', SRFNAM
      !>         WRITE (*,*)    '  Frame:             ', FRAME
      !>         WRITE (*,*)    '  Start time (TDB):  ', BTIME
      !>         WRITE (*,*)    '  Stop time  (TDB):  ', ETIME
      !>         WRITE (*,*)    '  Data type:         ', DTYPE
      !>         WRITE (*,*)    '  Data class:        ', DCLASS, ' ',
      !>     .                                        CLSNMS(DCLASS)
      !>         WRITE (*,*)    '  Coordinate system: ', SYSNAM
      !>
      !>         IF ( CORSYS .EQ. PDTSYS ) THEN
      !>
      !>            RE = DSKDSC(PARIDX  )
      !>            F  = DSKDSC(PARIDX+1)
      !>            RP = RE * ( 1.D0 - F )
      !>
      !>            WRITE (*,*) '     Equatorial radius (km): ', RE
      !>            WRITE (*,*) '     Polar radius      (km): ', RP
      !>
      !>         END IF
      !>
      !>         WRITE (*,*) '  Segment boundaries:'
      !>
      !>         IF ( CORSYS .EQ. LATSYS ) THEN
      !>
      !>            WRITE (*,FMT1) '    Longitude (deg):   ',
      !>     .                  DPR() * DSKDSC(MN1IDX),
      !>     .                  DPR() * DSKDSC(MX1IDX)
      !>            WRITE (*,FMT1) '    Latitude  (deg):   ',
      !>     .                  DPR() * DSKDSC(MN2IDX),
      !>     .                  DPR() * DSKDSC(MX2IDX)
      !>            WRITE (*,FMT1) '    Radius     (km):   ',
      !>     .                          DSKDSC(MN3IDX),
      !>     .                          DSKDSC(MX3IDX)
      !>
      !>         ELSE IF ( CORSYS .EQ. CYLSYS ) THEN
      !>
      !>            CALL SETMSG ( 'Coordinate system was '
      !>     .      //            'Cylindrical'           )
      !>            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'   )
      !>
      !>         ELSE IF ( CORSYS .EQ. RECSYS ) THEN
      !>
      !>            WRITE (*,FMT1) '    X-coordinate (km): ',
      !>     .                          DSKDSC(MN1IDX),
      !>     .                          DSKDSC(MX1IDX)
      !>            WRITE (*,FMT1) '    Y-coordinate (km): ',
      !>     .                          DSKDSC(MN2IDX),
      !>     .                          DSKDSC(MX2IDX)
      !>            WRITE (*,FMT1) '    Z-coordinate (km): ',
      !>     .                          DSKDSC(MN3IDX),
      !>     .                          DSKDSC(MX3IDX)
      !>
      !>         ELSE IF ( CORSYS .EQ. PDTSYS ) THEN
      !>
      !>            WRITE (*,FMT1) '    Longitude (deg):   ',
      !>     .                  DPR() * DSKDSC(MN1IDX),
      !>     .                  DPR() * DSKDSC(MX1IDX)
      !>            WRITE (*,FMT1) '    Latitude  (deg):   ',
      !>     .                  DPR() * DSKDSC(MN2IDX),
      !>     .                  DPR() * DSKDSC(MX2IDX)
      !>            WRITE (*,FMT1) '    Altitude   (km):   ',
      !>     .                          DSKDSC(MN3IDX),
      !>     .                          DSKDSC(MX3IDX)
      !>         END IF
      !>C
      !>C        Find the next segment, if it exists.
      !>C
      !>         CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds and an empty
      !>        string instead of the meta-kernel name, the output was:
      !>
      !>        Enter DSK name         > phobos512.bds
      !>        Enter meta-kernel name >
      !>         ====================================
      !>         DSK descriptor for segment   1
      !>           Body:              PHOBOS
      !>           Surface:           401
      !>           Frame:             IAU_PHOBOS
      !>           Start time (TDB):  1950 JAN 01 00:00:41.183
      !>           Stop time  (TDB):  2050 JAN 01 00:01:06.183
      !>           Data type:                    2
      !>           Data class:                   1  Single-valued surface
      !>           Coordinate system: Latitudinal
      !>           Segment boundaries:
      !>            Longitude (deg):     -180.000000000000   180.000000000000
      !>            Latitude  (deg):      -90.000000000000    90.000000000000
      !>            Radius     (km):        8.049632248722    13.940939832124
      !>
      !>     3) Again, dump the DSK descriptors of a DSK file, using the
      !>        program from example 2, but this time reading the DSK file
      !>
      !>           phobos_3_3_3seg.bds
      !>
      !>        which can be created by running an example program from
      !>        DSKW02. Use the meta-kernel shown below to demonstrate surface
      !>        name-ID mapping.
      !>
      !>           KPL/MK
      !>
      !>           File: dskgd_ex3.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The file contents shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           \begindata
      !>
      !>           NAIF_SURFACE_NAME += ( 'Phobos example surface 1',
      !>                                  'Phobos example surface 2',
      !>                                  'Phobos example surface 3' )
      !>           NAIF_SURFACE_CODE += (   1,   2,   3 )
      !>           NAIF_SURFACE_BODY += ( 401, 401, 401 )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        When Example #2 was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos_3_3_3seg.bds and the
      !>        meta-kernel dskgd_ex3.tm, the output was:
      !>
      !>        Enter DSK name         > phobos_3_3_3seg.bds
      !>        Enter meta-kernel name > dskgd_ex3.tm
      !>         ====================================
      !>         DSK descriptor for segment   1
      !>           Body:              PHOBOS
      !>           Surface:           Phobos example surface 1
      !>           Frame:             IAU_PHOBOS
      !>           Start time (TDB):  1950 JAN 01 00:00:00.000
      !>           Stop time  (TDB):  2050 JAN 01 00:00:00.000
      !>           Data type:                    2
      !>           Data class:                   2  General surface
      !>           Coordinate system: Latitudinal
      !>           Segment boundaries:
      !>            Longitude (deg):     -180.000000000000   180.000000000000
      !>            Latitude  (deg):      -90.000000000000    90.000000000000
      !>            Radius     (km):        8.225298075974    14.011768145626
      !>         ====================================
      !>         DSK descriptor for segment   2
      !>           Body:              PHOBOS
      !>           Surface:           Phobos example surface 2
      !>           Frame:             IAU_PHOBOS
      !>           Start time (TDB):  1950 JAN 01 00:00:00.000
      !>           Stop time  (TDB):  2050 JAN 01 00:00:00.000
      !>           Data type:                    2
      !>           Data class:                   2  General surface
      !>           Coordinate system: Rectangular
      !>           Segment boundaries:
      !>            X-coordinate (km):     -1.300000000000     1.310000000000
      !>            Y-coordinate (km):     -1.210000000000     1.200000000000
      !>            Z-coordinate (km):     -9.452932357788     9.638179779053
      !>         ====================================
      !>         DSK descriptor for segment   3
      !>           Body:              PHOBOS
      !>           Surface:           Phobos example surface 3
      !>           Frame:             IAU_PHOBOS
      !>           Start time (TDB):  1950 JAN 01 00:00:00.000
      !>           Stop time  (TDB):  2050 JAN 01 00:00:00.000
      !>           Data type:                    2
      !>           Data class:                   2  General surface
      !>           Coordinate system: Planetodetic
      !>              Equatorial radius (km):    13.000000000000000
      !>              Polar radius      (km):    9.0999999999999996
      !>           Segment boundaries:
      !>            Longitude (deg):     -180.000000000000   180.000000000000
      !>            Latitude  (deg):      -90.000000000000    90.000000000000
      !>            Altitude   (km):       -3.728668683604     1.372015791081
      !>
      !>## Restrictions
      !>
      !>     1)  See Exception #3.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Extended the
      !>        $Exceptions section and updated the $Restrictions.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        22-JAN-2016 (NJB)
      !>
      !>           Added new header example programs and updated existing
      !>           example program. Made minor changes to code to enhance
      !>           readability. Corrected header typo.
      !>
      !>        09-OCT-2009 (NJB)
      module subroutine dskgd ( ctx, handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DSK file.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA segment descriptor.
      real(wp), intent(out) :: dskdsc ( * )  !! DSK segment descriptor.

      integer(ip) :: dpbase
      integer(ip) :: dpsize

      if ( ctx%failed() ) return

!
!     Fetch the base address and size of the DP component of the
!     indicated segment.
!
      dpbase = dladsc ( dbsidx )
      dpsize = dladsc ( dszidx )

!
!     If we don't have enough d.p. elements to hold a descriptor,
!     something's wrong.
!
      if ( dpsize < dskdsz ) then

         call ctx%setmsg ( 'Size of d.p. component of segment is #; ' &
         //            'cannot extract descriptor.  This is a ' &
         //            'file format error which may be indicative ' &
         //             'of a corrupted file.'                      )
         call ctx%errint ( '#', dpsize                                  )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                       )
         return

      end if

!
!     Extract the descriptor.
!
      call dasrdd ( ctx, handle, dpbase+1, dpbase+dskdsz, dskdsc )

      return
      end subroutine dskgd

      !> Return bookkeeping data from a DSK type 2 segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     NV       is the number of vertices in model.
      !>
      !>     NP       is the number of plates in model.
      !>
      !>     NVXTOT   is the total number of voxels in fine grid.
      !>
      !>     VTXBDS   are the vertex bounds. This is an array of six values
      !>              giving the minimum and maximum values of each component
      !>              of the vertex set. VTXBDS has dimensions ( 2, 3 ).
      !>              Units are km.
      !>
      !>     VOXSIZ   is the fine grid voxel size. DSK voxels are cubes; the
      !>              edge length of each cube is given by the voxel size.
      !>              This size applies to the fine voxel grid. Units are km.
      !>
      !>     VOXORI   is the voxel grid origin. This is the location of the
      !>              voxel grid origin in the body-fixed frame associated
      !>              with the target body. Units are km.
      !>
      !>     VGREXT   is the voxel grid extent. This extent is an array of
      !>              three integers indicating the number of voxels in the
      !>              X, Y, and Z directions in the fine voxel grid.
      !>
      !>     CGSCAL   is the coarse voxel grid scale. The extent of the fine
      !>              voxel grid is related to the extent of the coarse voxel
      !>              grid by this scale factor.
      !>
      !>     VTXNPL   is the vertex-plate correspondence list size.
      !>
      !>     VOXNPT   is the size of the voxel-to-plate pointer list.
      !>
      !>     VOXNPL   is the voxel-plate correspondence list size.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine supports computations involving bookkeeping
      !>     information stored in DSK type 2 segments. User applications
      !>     typically will not need to call this routine.
      !>
      !>     DSK files are built using the DLA low-level format and
      !>     the DAS architecture; DLA files are a specialized type of DAS
      !>     file in which data are organized as a doubly linked list of
      !>     segments. Each segment's data belong to contiguous components of
      !>     character, double precision, and integer type.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Dump several parameters from the first DLA segment of
      !>        a DSK file. The segment is assumed to be of type 2.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKB02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE = 80 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    DSK
      !>      CHARACTER*(LNSIZE)    OUTLIN
      !>
      !>      DOUBLE PRECISION      VOXORI ( 3 )
      !>      DOUBLE PRECISION      VOXSIZ
      !>      DOUBLE PRECISION      VTXBDS ( 2, 3 )
      !>
      !>      INTEGER               CGSCAL
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               NP
      !>      INTEGER               NV
      !>      INTEGER               NVXTOT
      !>      INTEGER               VGREXT ( 3 )
      !>      INTEGER               VOXNPL
      !>      INTEGER               VOXNPT
      !>      INTEGER               VTXNPL
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the DSK to read.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>C
      !>C     Open the DSK file for read access.
      !>C     We use the DAS-level interface for
      !>C     this function.
      !>C
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>C
      !>C     Begin a forward search through the
      !>C     kernel, treating the file as a DLA.
      !>C     In this example, it's a very short
      !>C     search.
      !>C
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>C
      !>C        We arrive here only if the kernel
      !>C        contains no segments.  This is
      !>C        unexpected, but we're prepared for it.
      !>C
      !>         CALL SETMSG ( 'No segments found '
      !>     .   //            'in DSK file #.'    )
      !>         CALL ERRCH  ( '#',  DSK           )
      !>         CALL SIGERR ( 'SPICE(NODATA)'     )
      !>
      !>      END IF
      !>
      !>C
      !>C     If we made it this far, DLADSC is the
      !>C     DLA descriptor of the first segment.
      !>C
      !>C     Read and display type 2 bookkeeping data.
      !>C
      !>      CALL DSKB02 ( HANDLE, DLADSC, NV,     NP,     NVXTOT,
      !>     .              VTXBDS, VOXSIZ, VOXORI, VGREXT, CGSCAL,
      !>     .              VTXNPL, VOXNPT, VOXNPL                 )
      !>
      !>C
      !>C     Show vertex and plate counts.
      !>C
      !>      OUTLIN = 'Number of vertices:                 #'
      !>      CALL REPMI  ( OUTLIN, '#', NV, OUTLIN )
      !>      CALL TOSTDO ( OUTLIN )
      !>
      !>      OUTLIN = 'Number of plates:                   #'
      !>      CALL REPMI  ( OUTLIN, '#', NP, OUTLIN )
      !>      CALL TOSTDO ( OUTLIN )
      !>
      !>      OUTLIN = 'Voxel edge length (km):             #'
      !>      CALL REPMF  ( OUTLIN, '#', VOXSIZ, 6, 'E', OUTLIN )
      !>      CALL TOSTDO ( OUTLIN )
      !>
      !>      OUTLIN = 'Number of voxels:                   #'
      !>      CALL REPMI  ( OUTLIN, '#', NVXTOT, OUTLIN )
      !>      CALL TOSTDO ( OUTLIN )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>        Number of vertices:                 1579014
      !>        Number of plates:                   3145728
      !>        Voxel edge length (km):             1.04248E-01
      !>        Number of voxels:                   11914500
      !>
      !>## Restrictions
      !>
      !>     1)  The caller must verify that the segment associated with
      !>         the input DLA descriptor is a DSK type 2 segment.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        solution for code example.
      !>
      !>-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
      !>
      !>        Updated version info.
      !>
      !>        23-JAN-2016 (NJB)
      !>
      !>           Removed references to unneeded variables.
      !>           Updated header comments.
      !>
      !>        DSKLIB Version 2.0.0, 05-MAY-2010 (NJB)
      !>
      !>           Renamed routine from DSKP02 to DSKB02.
      !>
      !>        DSKLIB Version 1.0.1, 08-OCT-2009 (NJB)
      !>
      !>           Updated header.
      !>
      !>        Beta Version 1.0.0, 30-OCT-2006 (NJB)
      module subroutine dskb02 ( ctx, handle, dladsc, nv, np, nvxtot, vtxbds, voxsiz, voxori, vgrext, cgscal, vtxnpl, voxnpt, voxnpl )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(out) :: nv  !! Number of vertices in model.
      integer(ip), intent(out) :: np  !! Number of plates in model.
      integer(ip), intent(out) :: nvxtot  !! Number of voxels in fine grid.
      real(wp), intent(out) :: vtxbds ( 2, 3 )  !! Vertex bounds.
      real(wp), intent(out) :: voxsiz  !! Fine voxel edge length.
      real(wp), intent(out) :: voxori ( 3 )  !! Fine voxel grid origin.
      integer(ip), intent(out) :: vgrext ( 3 )  !! Fine voxel grid extent.
      integer(ip), intent(out) :: cgscal  !! Coarse voxel grid scale.
      integer(ip), intent(out) :: vtxnpl  !! Size of vertex-plate correspondence list.
      integer(ip), intent(out) :: voxnpt  !! Size of voxel-plate pointer list.
      integer(ip), intent(out) :: voxnpl  !! Size of voxel-plate correspondence list.

      integer(ip), parameter :: dbfsiz = 10


      integer(ip), parameter :: vtbsiz = 6

      real(wp) :: dbuff  ( dbfsiz )

      integer(ip) :: b
      integer(ip) :: e
      integer(ip) :: dpbase
      integer(ip) :: ibase
      integer(ip) :: ibuff  ( ibfsiz )

      if ( ctx%failed() ) return

      dpbase = dladsc ( dbsidx )
      ibase  = dladsc ( ibsidx )

!
!     Read the integer parameters first.  These are located at the
!     beginning of the integer component of the segment.
!
      call dasrdi ( ctx, handle, ibase+1, ibase+ibfsiz, ibuff )

      nv     = ibuff ( ixnv   )
      np     = ibuff ( ixnp   )
      nvxtot = ibuff ( ixnvxt )
      cgscal = ibuff ( ixcgsc )
      voxnpt = ibuff ( ixvxps )
      voxnpl = ibuff ( ixvxls )
      vtxnpl = ibuff ( ixvtls )

      vgrext = ibuff(ixvgrx:ixvgrx+3-1)

!
!     Read the d.p. parameters.
!
      b = dpbase + ixvtbd
      e = dpbase + ixvtbd + dbfsiz - 1

      call dasrdd( ctx, handle, b, e, dbuff )

      vtxbds = reshape(dbuff(1:vtbsiz), [2, 3])
      voxori = dbuff(vtbsiz+1:vtbsiz+1+3-1)

      voxsiz = dbuff ( ixvxsz - dskdsz )

      return
      end subroutine dskb02

      !> Fetch double precision data from a type 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>     ITEM     is an integer "keyword" parameter designating the
      !>              item to fetch. In the descriptions below, note
      !>              that "model" refers to the model represented by
      !>              the designated segment. This model may be a
      !>              subset of a larger model.
      !>
      !>              Names and meanings of parameters supported by this
      !>              routine are:
      !>
      !>                 KWDSC      DSK descriptor of segment. See the
      !>                            INCLUDE file dskdsc.inc for a
      !>                            discussion of the contents of DSK
      !>                            descriptors. Note that DSK
      !>                            descriptors are not to be confused
      !>                            with DLA descriptors, which contain
      !>                            segment component base address and
      !>                            size information.
      !>
      !>                 KWVTBD     Vertex bounds. This is an array of
      !>                            six values giving the minimum and
      !>                            maximum values of each component of
      !>                            the vertex set.
      !>
      !>                 KWVXOR     Voxel grid origin. This is the
      !>                            location of the voxel grid origin in
      !>                            the body-fixed frame associated with
      !>                            the target body.
      !>
      !>                 KWVXSZ     Voxel size. DSK voxels are cubes;
      !>                            the edge length of each cube is
      !>                            given by the voxel size. This
      !>                            size applies to the fine voxel grid.
      !>                            Units are km.
      !>
      !>     START    is the start index within specified data item from
      !>              which data are to be fetched. The index of the
      !>              first element of each data item is 1.
      !>
      !>     ROOM     is the amount of room in the output array. It
      !>              is permissible to provide an output array
      !>              that has too little room to fetch an item in
      !>              one call.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of elements fetched to the output
      !>              array VALUES. N is normally in the range
      !>              1:ROOM; if an error occurs on the call, N is
      !>              undefined.
      !>
      !>     VALUES   is a contiguous set of elements of the item
      !>              designated by ITEM. The correspondence of
      !>              VALUES with the elements of the data item is:
      !>
      !>                 VALUES(1)      ITEM(START)
      !>                   ...             ...
      !>                 VALUES(N)      ITEM(START+N-1)
      !>
      !>              If an error occurs on the call, VALUES is
      !>              undefined.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>     4)  If ROOM is non-positive, the error SPICE(VALUEOUTOFRANGE)
      !>         is signaled.
      !>
      !>     5)  If the coarse voxel scale read from the designated segment
      !>         is less than 1, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     6)  If the input keyword parameter is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     7)  If START is less than 1 or greater than the size of the
      !>         item to be fetched, the error SPICE(INDEXOUTOFRANGE) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     Most SPICE applications will not need to call this routine. The
      !>     routines DSKV02, DSKP02, and DSKZ02 provide a higher-level
      !>     interface for fetching DSK type 2 vertex and plate data.
      !>
      !>     DSK files are built using the DLA low-level format and
      !>     the DAS architecture; DLA files are a specialized type of DAS
      !>     file in which data are organized as a doubly linked list of
      !>     segments. Each segment's data belong to contiguous components of
      !>     character, double precision, and integer type.
      !>
      !>     Note that the DSK descriptor for the segment is not needed by
      !>     this routine; the DLA descriptor contains the base address and
      !>     size information for the integer, double precision, and character
      !>     components of the segment, and these suffice for the purpose of
      !>     fetching data.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Look up all the vertices associated with each plate
      !>        of the model contained in a specified type 2 segment.
      !>        For this example, we'll show the context of this look-up:
      !>        opening the DSK file for read access, traversing a trivial,
      !>        one-segment list to obtain the segment of interest.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKD02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      VRTCES ( 3, 3 )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>      INTEGER               NP
      !>      INTEGER               START
      !>      INTEGER               VRTIDS ( 3 )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the DSK to read.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>C
      !>C     Open the DSK file for read access.
      !>C     We use the DAS-level interface for
      !>C     this function.
      !>C
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>C
      !>C     Begin a forward search through the
      !>C     kernel, treating the file as a DLA.
      !>C     In this example, it's a very short
      !>C     search.
      !>C
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>C
      !>C        We arrive here only if the kernel
      !>C        contains no segments.  This is
      !>C        unexpected, but we're prepared for it.
      !>C
      !>         CALL SETMSG ( 'No segments found '
      !>     .   //            'in DSK file #.'    )
      !>         CALL ERRCH  ( '#',  DSK           )
      !>         CALL SIGERR ( 'SPICE(NODATA)'     )
      !>
      !>      END IF
      !>
      !>C
      !>C     If we made it this far, DLADSC is the
      !>C     DLA descriptor of the first segment.
      !>C
      !>C     Find the number of plates in the model.
      !>C
      !>      CALL DSKI02 ( HANDLE, DLADSC, KWNP, 1, 1, N, NP )
      !>      WRITE (*,*) 'Number of plates: ', NP
      !>
      !>C
      !>C     For the first 5 plates, look up the desired data.
      !>C
      !>      K = MIN(5, NP)
      !>      DO I = 1, K
      !>C
      !>C        For the Ith plate, find the associated
      !>C        vertex IDs.  We must take into account
      !>C        the fact that each plate has three
      !>C        vertices when we compute the start
      !>C        index.
      !>C
      !>         START = 3*(I-1)+1
      !>
      !>         CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, START,
      !>     .                 3,      N,      VRTIDS        )
      !>
      !>         DO J = 1, 3
      !>C
      !>C            Fetch the vertex associated with
      !>C            the Jth vertex ID.  Again, each
      !>C            vertex is a 3-vector.  Note that
      !>C            the vertices are double-precision
      !>C            data, so we fetch them using
      !>C            DSKD02.
      !>C
      !>             START = 3*( VRTIDS(J) - 1 ) + 1
      !>
      !>             CALL DSKD02 ( HANDLE, DLADSC, KWVERT,  START,
      !>     .                     3,      N,      VRTCES(1,J)    )
      !>         END DO
      !>
      !>C
      !>C        Display the vertices of the Ith plate:
      !>C
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Plate number: ', I
      !>         WRITE (*,FMT) '   Vertex 1: ', (VRTCES(J,1), J=1,3)
      !>         WRITE (*,FMT) '   Vertex 2: ', (VRTCES(J,2), J=1,3)
      !>         WRITE (*,FMT) '   Vertex 3: ', (VRTCES(J,3), J=1,3)
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the kernel.  This isn't necessary in a stand-
      !>C     alone program, but it's good practice in subroutines
      !>C     because it frees program and system resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>         Number of plates:      3145728
      !>
      !>         Plate number:            1
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
      !>
      !>         Plate number:            2
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>
      !>         Plate number:            3
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>
      !>         Plate number:            4
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>
      !>         Plate number:            5
      !>            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>
      !>        Note that only the vertex information for first 5 plates is
      !>        provided.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine uses discovery check-in to boost
      !>         execution speed. However, this routine is in
      !>         violation of NAIF standards for use of discovery
      !>         check-in:  routines called from this routine may
      !>         signal errors. If errors are signaled in called
      !>         routines, this routine's name will be missing
      !>         from the traceback message.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 13-AUG-2021 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Modified code
      !>        example to reduce the output.
      !>
      !>-    SPICELIB Version 1.0.0, 04-FEB-2017 (NJB)
      !>
      !>        Fixed typo in version description.
      !>
      !>        23-AUG-2016
      !>
      !>           Now saves NV and updates it only when the current
      !>           segment changes.
      !>
      !>        15-JAN-2016 (NJB)
      !>
      !>           Removed code involving parameter NP.
      !>           Updated header $Examples section.
      !>
      !>        DSKLIB Version 3.0.0, 13-MAY-2010 (NJB)
      !>
      !>           Updated for compatibility with new DSK type 2
      !>           segment design.
      !>
      !>        DSKLIB Version 2.1.0, 20-APR-2010 (NJB)
      !>
      !>           Bug fix: changed declaration of output argument
      !>           VALUES to double precision.
      !>
      !>        DSKLIB Version 2.0.0, 27-DEC-2006 (NJB)
      !>
      !>           Updated to remove support for min, max radius
      !>           lookup. These values are now stored in DSK
      !>           descriptors.
      !>
      !>        DSKLIB Version 1.0.0, 27-OCT-2006 (NJB)
      module subroutine dskd02 ( ctx, handle, dladsc, item, start, room, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(in) :: item  !! Keyword identifying item to fetch.
      integer(ip), intent(in) :: start  !! Start index.
      integer(ip), intent(in) :: room  !! Amount of room in output array.
      integer(ip), intent(out) :: n  !! Number of values returned.
      real(wp), intent(out) :: values ( * )  !! Array containing requested item.
      integer(ip) :: nv_1(1)  !! one-element copy of nv (array argument)
      integer(ip) :: b
      integer(ip) :: dbase
      integer(ip) :: e
      integer(ip) :: ibase
      integer(ip) :: size
      if ( .not. allocated ( ctx%dskd02_state ) ) allocate ( ctx%dskd02_state )
      associate ( first => ctx%dskd02_state%first, &
                  prvbas => ctx%dskd02_state%prvbas, &
                  prvhan => ctx%dskd02_state%prvhan, &
                  nv => ctx%dskd02_state%nv )
      if ( first ) then
!
!        Make sure we treat the input handle as new on the first pass.
!        Set PRVHAN to an invalid handle value.
!
         prvhan = 0

!
!        Set previous segment base integer address to an invalid value
!        as well.
!
         prvbas = -1

         first  = .false.

      end if

      if ( room <= 0 ) then

         call ctx%setmsg ( 'ROOM was #; must be positive.' )
         call ctx%errint ( '#',  room                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'        )
         return

      end if

      ibase  = dladsc ( ibsidx )
      dbase  = dladsc ( dbsidx )

!
!     Either a new file or new segment in the same file will require
!     looking up the segment parameters. To determine whether the
!     segment is new, we don't need to compare the entire DLA
!     descriptor: just comparing the integer base address of the
!     descriptor against the saved integer base address is sufficient.
!
!     DSK type 2 segments always have a non-empty integer component, so
!     each type 2 segment in a given file will have a distinct integer
!     base address. Segments of other types might not contain integers,
!     but they can't share an integer base address with a type 2
!     segment.
!
      if (      ( handle /= prvhan ) &
           .or. ( ibase  /= prvbas )  ) then
!
!        Treat the input file and segment as new.
!
!        Read NV.
!
         call dasrdi ( ctx, handle, ibase+ixnv, ibase+ixnv, nv_1 )
         nv = nv_1(1)

         if ( ctx%failed() ) return
!
!        Update the saved handle value.
!
         prvhan = handle

!
!        Update the saved integer base address.
!
         prvbas = ibase

      end if

!
!     Branch based on the item to be returned.
!
!     Note that we haven't checked the validity of START; we'll do this
!     after the IF block.
!
      if ( item == kwvert ) then
!
!        Return vertices.  There are 3*NV values in all.
!        First locate the data.
!
         size =   3*nv
         b    = ( dbase + ixvert ) + start - 1

      else if ( item == kwdsc ) then
!
!        Return DSK descriptor.
!
         size =   dskdsz
         b    = ( dbase + ixdscr ) + start - 1

      else if ( item == kwvtbd ) then
!
!        Return vertex bounds.  There are 6 elements.
!
         size =   6
         b    = ( dbase + ixvtbd ) + start - 1

      else if ( item == kwvxor ) then
!
!        Return voxel grid origin.  There are 3 elements.
!
         size =   3
         b    = ( dbase + ixvxor ) + start - 1

      else if ( item == kwvxsz ) then
!
!        Return voxel size.  This is a scalar.
!
         size =   1
         b    = ( dbase + ixvxsz ) + start - 1

      else

         call ctx%setmsg ( 'Keyword parameter # was not recognized.' )
         call ctx%errint ( '#',  item                                )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                     )
         return

      end if

!
!     The valid range for START is 1:SIZE.
!
      if (  ( start < 1 ) .or. ( start > size )  ) then

         call ctx%setmsg ( 'START must be in the range defined ' // &
                       'by the size of the data associated ' // &
                       'with the keyword parameter #, '      // &
                       'namely 1:#.  Actual value of START ' // &
                       'was #.'                              )
         call ctx%errint ( '#',  item                            )
         call ctx%errint ( '#',  size                            )
         call ctx%errint ( '#',  start                           )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         return

      end if

!
!     Read the requested data.  We already have the start address B.
!
      n  =   min ( room, size - start + 1 )
      e  =   b + n - 1

      call dasrdd ( ctx, handle, b, e, values )

      return
      end associate
      end subroutine dskd02

      !> Fetch integer data from a type 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>     ITEM     is an integer "keyword" parameter designating the
      !>              item to fetch. In the descriptions below, note
      !>              that "model" refers to the model represented by
      !>              the designated segment. This model may be a
      !>              subset of a larger model.
      !>
      !>              Names and meanings of parameters supported by this
      !>              routine are:
      !>
      !>                 KWNV       Number of vertices in model.
      !>
      !>                 KWNP       Number of plates in model.
      !>
      !>                 KWNVXT     Total number of voxels in fine grid.
      !>
      !>                 KWVGRX     Voxel grid extent. This extent is
      !>                            an array of three integers
      !>                            indicating the number of voxels in
      !>                            the X, Y, and Z directions in the
      !>                            fine voxel grid.
      !>
      !>                 KWCGSC     Coarse voxel grid scale. The extent
      !>                            of the fine voxel grid is related to
      !>                            the extent of the coarse voxel grid
      !>                            by this scale factor.
      !>
      !>                 KWVXPS     Size of the voxel-to-plate pointer
      !>                            list.
      !>
      !>                 KWVXLS     Voxel-plate correspondence list size.
      !>
      !>                 KWVTLS     Vertex-plate correspondence list
      !>                            size.
      !>
      !>                 KWPLAT     Plate array. For each plate, this
      !>                            array contains the indices of the
      !>                            plate's three vertices. The ordering
      !>                            of the array members is:
      !>
      !>                               Plate 1 vertex index 1
      !>                               Plate 1 vertex index 2
      !>                               Plate 1 vertex index 3
      !>                               Plate 2 vertex index 1
      !>                                       ...
      !>
      !>                 KWVXPT     Voxel-plate pointer list. This list
      !>                            contains pointers that map fine
      !>                            voxels to lists of plates that
      !>                            intersect those voxels. Note that
      !>                            only fine voxels belonging to
      !>                            non-empty coarse voxels are in the
      !>                            domain of this mapping.
      !>
      !>                 KWVXPL     Voxel-plate correspondence list.
      !>                            This list contains lists of plates
      !>                            that intersect fine voxels. (This
      !>                            list is the data structure into
      !>                            which the voxel-to-plate pointers
      !>                            point.) This list can contain
      !>                            empty lists.
      !>
      !>                 KWVTPT     Vertex-plate pointer list. This list
      !>                            contains pointers that map vertices
      !>                            to lists of plates to which those
      !>                            vertices belong.
      !>
      !>                            Note that the size of this list is
      !>                            always NV, the number of vertices.
      !>                            Hence there's no need for a separate
      !>                            keyword for the size of this list.
      !>
      !>                 KWVTPL     Vertex-plate correspondence list.
      !>                            This list contains, for each vertex,
      !>                            the indices of the plates to which
      !>                            that vertex belongs.
      !>
      !>                 KWCGPT     Coarse voxel grid pointers. This is
      !>                            an array of pointers mapping coarse
      !>                            voxels to lists of pointers in the
      !>                            voxel-plate pointer list. Each
      !>                            non-empty coarse voxel maps to a
      !>                            list of pointers; every fine voxel
      !>                            contained in a non-empty coarse voxel
      !>                            has its own pointers. Grid elements
      !>                            corresponding to empty coarse voxels
      !>                            have null (non-positive) pointers.
      !>
      !>              See the INCLUDE file dsk.inc for values
      !>              associated with the keyword parameters.
      !>
      !>     START    is the start index within the specified data item
      !>              from which data are to be fetched. The index of
      !>              the first element of each data item is 1. START
      !>              has units of integers; for example, the start
      !>              index of the second plate is 4, since each plate
      !>              occupies three integers.
      !>
      !>     ROOM     is the amount of room in the output array. It is
      !>              permissible to provide an output array that has
      !>              too little room to fetch an item in one call. ROOM
      !>              has units of integers: for example, the room
      !>              required to fetch one plate is 3.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of elements fetched to the output
      !>              array VALUES. N is normally in the range
      !>              1:ROOM; if an error occurs on the call, N is
      !>              undefined.
      !>
      !>     VALUES   is a contiguous set of elements of the item
      !>              designated by ITEM. The correspondence of
      !>              VALUES with the elements of the data item is:
      !>
      !>                 VALUES(1)      ITEM(START)
      !>                   ...             ...
      !>                 VALUES(N)      ITEM(START+N-1)
      !>
      !>              If an error occurs on the call, VALUES is
      !>              undefined.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>     4)  If ROOM is non-positive, the error SPICE(VALUEOUTOFRANGE)
      !>         is signaled.
      !>
      !>     5)  If the coarse voxel scale read from the designated segment
      !>         is less than 1, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     6)  If the input keyword parameter is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     7)  If START is less than 1 or greater than the size of the
      !>         item to be fetched, the error SPICE(INDEXOUTOFRANGE) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     Most SPICE applications will not need to call this routine. The
      !>     routines DSKV02, DSKP02, and DSKZ02 provide a higher-level
      !>     interface for fetching DSK type 2 vertex and plate data.
      !>
      !>     DSK files are built using the DLA low-level format and
      !>     the DAS architecture; DLA files are a specialized type of DAS
      !>     file in which data are organized as a doubly linked list of
      !>     segments. Each segment's data belong to contiguous components of
      !>     character, double precision, and integer type.
      !>
      !>     Note that the DSK descriptor for the segment is not needed by
      !>     this routine; the DLA descriptor contains the base address and
      !>     size information for the integer, double precision, and character
      !>     components of the segment, and these suffice for the purpose of
      !>     fetching data.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Look up all the vertices associated with each plate
      !>        of the model contained in a specified type 2 segment.
      !>        For this example, we'll show the context of this look-up:
      !>        opening the DSK file for read access, traversing a trivial,
      !>        one-segment list to obtain the segment of interest.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKI02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      VRTCES ( 3, 3 )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>      INTEGER               NP
      !>      INTEGER               START
      !>      INTEGER               VRTIDS ( 3 )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of the DSK to read.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>C
      !>C     Open the DSK file for read access.
      !>C     We use the DAS-level interface for
      !>C     this function.
      !>C
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>C
      !>C     Begin a forward search through the
      !>C     kernel, treating the file as a DLA.
      !>C     In this example, it's a very short
      !>C     search.
      !>C
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>C
      !>C        We arrive here only if the kernel
      !>C        contains no segments.  This is
      !>C        unexpected, but we're prepared for it.
      !>C
      !>         CALL SETMSG ( 'No segments found '
      !>     .   //            'in DSK file #.'    )
      !>         CALL ERRCH  ( '#',  DSK           )
      !>         CALL SIGERR ( 'SPICE(NODATA)'     )
      !>
      !>      END IF
      !>
      !>C
      !>C     If we made it this far, DLADSC is the
      !>C     DLA descriptor of the first segment.
      !>C
      !>C     Find the number of plates in the model.
      !>C
      !>      CALL DSKI02 ( HANDLE, DLADSC, KWNP, 1, 1, N, NP )
      !>      WRITE (*,*) 'Number of plates: ', NP
      !>
      !>C
      !>C     For the first 5 plates, look up the desired data.
      !>C
      !>      K = MIN(5, NP)
      !>      DO I = 1, K
      !>C
      !>C        For the Ith plate, find the associated
      !>C        vertex IDs.  We must take into account
      !>C        the fact that each plate has three
      !>C        vertices when we compute the start
      !>C        index.
      !>C
      !>         START = 3*(I-1)+1
      !>
      !>         CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, START,
      !>     .                 3,      N,      VRTIDS        )
      !>
      !>         DO J = 1, 3
      !>C
      !>C            Fetch the vertex associated with
      !>C            the Jth vertex ID.  Again, each
      !>C            vertex is a 3-vector.  Note that
      !>C            the vertices are double-precision
      !>C            data, so we fetch them using
      !>C            DSKD02.
      !>C
      !>             START = 3*( VRTIDS(J) - 1 ) + 1
      !>
      !>             CALL DSKD02 ( HANDLE, DLADSC, KWVERT,  START,
      !>     .                     3,      N,      VRTCES(1,J)    )
      !>         END DO
      !>
      !>C
      !>C        Display the vertices of the Ith plate:
      !>C
      !>         WRITE (*,*)   ' '
      !>         WRITE (*,*)   'Plate number: ', I
      !>         WRITE (*,FMT) '   Vertex 1: ', (VRTCES(J,1), J=1,3)
      !>         WRITE (*,FMT) '   Vertex 2: ', (VRTCES(J,2), J=1,3)
      !>         WRITE (*,FMT) '   Vertex 3: ', (VRTCES(J,3), J=1,3)
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the kernel.  This isn't necessary in a stand-
      !>C     alone program, but it's good practice in subroutines
      !>C     because it frees program and system resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>         Number of plates:      3145728
      !>
      !>         Plate number:            1
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
      !>
      !>         Plate number:            2
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>
      !>         Plate number:            3
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>
      !>         Plate number:            4
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>
      !>         Plate number:            5
      !>            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>
      !>        Note that only the vertex information for first 5 plates is
      !>        provided.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine uses discovery check-in to boost
      !>         execution speed. However, this routine is in
      !>         violation of NAIF standards for use of discovery
      !>         check-in:  routines called from this routine may
      !>         signal errors. If errors are signaled in called
      !>         routines, this routine's name will be missing
      !>         from the traceback message.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Extended the
      !>        $Keywords section. Modified code example to reduce the output.
      !>
      !>-    SPICELIB Version 1.0.0, 22-NOV-2016 (NJB)
      !>
      !>        Added FAILED check after segment attribute fetch calls.
      !>        Re-ordered code so that values are saved only after
      !>        all error checks have passed. Simplified base address
      !>        comparisons.
      !>
      !>        15-JAN-2016 (NJB)
      !>
      !>           Updated header $Examples and $Particulars sections.
      !>
      !>        DSKLIB Version 1.0.2, 11-JUL-2014 (NJB)
      !>
      !>           Fixed a trivial header comment typo.
      !>
      !>        DSKLIB Version 1.0.1, 13-MAY-2010 (NJB)
      !>
      !>           Updated header.
      !>
      !>        DSKLIB Version 1.0.0, 27-OCT-2006 (NJB)
      module subroutine dski02 ( ctx, handle, dladsc, item, start, room, n, values )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(in) :: item  !! Keyword identifying item to fetch.
      integer(ip), intent(in) :: start  !! Start index.
      integer(ip), intent(in) :: room  !! Amount of room in output array.
      integer(ip), intent(out) :: n  !! Number of values returned.
      integer(ip), intent(out) :: values ( * )  !! Array containing requested item.
      integer(ip) :: b
      integer(ip) :: e
      integer(ip) :: ibase
      integer(ip) :: ibuff  ( ibfsiz )
      integer(ip) :: ncgr
      integer(ip) :: size
      if ( .not. allocated ( ctx%dski02_state ) ) allocate ( ctx%dski02_state )
      associate ( cgscal => ctx%dski02_state%cgscal, &
                  first => ctx%dski02_state%first, &
                  np => ctx%dski02_state%np, &
                  nv => ctx%dski02_state%nv, &
                  nvxtot => ctx%dski02_state%nvxtot, &
                  prvbas => ctx%dski02_state%prvbas, &
                  prvhan => ctx%dski02_state%prvhan, &
                  voxnpl => ctx%dski02_state%voxnpl, &
                  voxnpt => ctx%dski02_state%voxnpt, &
                  vtxnpl => ctx%dski02_state%vtxnpl )
      if ( first ) then
!
!        Make sure we treat the input handle as new on the first pass.
!        Set PRVHAN to an invalid handle value.
!
         prvhan = 0

!
!        Set the previous segment base integer address to an invalid
!        value as well.
!
         prvbas = -1

         first  = .false.

      end if

      if ( room <= 0 ) then

         call ctx%setmsg ( 'ROOM was #; must be positive.' )
         call ctx%errint ( '#',  room                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'        )
         return

      end if

      ibase  = dladsc ( ibsidx )
!
!     Either a new file or new segment in the same file will require
!     looking up the segment parameters. To determine whether the
!     segment is new, we don't need to compare the entire DLA
!     descriptor: just comparing the integer base address of the
!     descriptor against the saved integer base address is sufficient.
!
!     DSK type 2 segments always have a non-empty integer component, so
!     each type 2 segment in a given file will have a distinct integer
!     base address. Segments of other types might not contain integers,
!     but they can't share an integer base address with a type 2
!     segment.
!
      if (      ( handle /= prvhan ) &
           .or. ( ibase  /= prvbas )  ) then
!
!        Treat the input file and segment as new.
!
!        Read the integer parameters first.  These are located at the
!        beginning of the integer component of the segment.
!
         call dasrdi ( ctx, handle, ibase+1, ibase+ibfsiz, ibuff )

         if ( ctx%failed() ) return

!
!        Check the coarse voxel scale.
!
         if ( ibuff(ixcgsc) < 1 ) then

            call ctx%setmsg ( 'Coarse voxel grid scale is #; ' // &
                          'this scale should be an '       // &
                          'integer > 1'                    )
            call ctx%errint ( '#',  cgscal                     )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
            return

         end if

!
!        All checks have passed. We can safely store the segment
!        parameters.
!
         nv     = ibuff ( ixnv   )
         np     = ibuff ( ixnp   )
         nvxtot = ibuff ( ixnvxt )
         cgscal = ibuff ( ixcgsc )
         vtxnpl = ibuff ( ixvtls )
         voxnpt = ibuff ( ixvxps )
         voxnpl = ibuff ( ixvxls )

!
!        Update the saved handle value.
!
         prvhan = handle

!
!        Update the saved base integer address.
!
         prvbas = ibase

      end if

!
!     Branch based on the item to be returned.
!
!     Note that we haven't checked the validity of START; we'll do this
!     after the IF block.
!
      if ( item == kwnv ) then
!
!        Return the number of vertices.
!
         n         = 1
         values(1) = nv

!
!        As long as START is valid, we can return. Otherwise,
!        let control pass to the error handling block near
!        the end of this routine.
!
         if ( start == 1 ) return

      else if ( item == kwnp ) then
!
!        Return the number of plates.
!
         n         = 1
         values(1) = np

         if ( start == 1 ) return

      else if ( item == kwnvxt ) then
!
!        Return the total number of voxels.
!
         n         = 1
         values(1) = nvxtot

         if ( start == 1 ) return

      else if ( item == kwvgrx ) then
!
!        Return the voxel grid extents.
!
         size = 3
         b    = ibase + ixvgrx + start - 1

      else if ( item == kwcgsc ) then
!
!        Return the coarse voxel grid scale.
!
         n         = 1
         values(1) = cgscal

         if ( start == 1 ) return

      else if ( item == kwvxps ) then
!
!        Return the voxel-plate pointer list size.
!
         n         = 1
         values(1) = voxnpt

         if ( start == 1 ) return

      else if ( item == kwvxls ) then
!
!        Return the voxel-plate list size.
!
         n         = 1
         values(1) = voxnpl

         if ( start == 1 ) return

      else if ( item == kwvtls ) then
!
!        Return the vertex-plate list size.
!
         n         = 1
         values(1) = vtxnpl

         if ( start == 1 ) return

      else if ( item == kwplat ) then
!
!        Return plate data.  There are 3*NP values in all.  First
!        locate the data.
!
         size = 3*np
         b    = ( ibase + ixplat ) + start - 1

      else if ( item == kwvxpt )  then
!
!        Return voxel pointer data.  There are VOXNPT values in all.
!        First locate the data.
!
         size = voxnpt
         b    = ( ibase + ixplat + 3*np ) + start - 1

      else if ( item == kwvxpl )  then
!
!        Return voxel-plate list data.  There are VOXNPL values in all.
!        First locate the data.
!
         size = voxnpl
         b    = ( ibase + ixplat + 3*np + voxnpt ) + start - 1

      else if ( item == kwvtpt )  then
!
!        Return vertex-plate pointer data.  There are NV values in all.
!        First locate the data.
!
         size =    nv
         b    =  ( ibase + ixplat + 3*np + voxnpt + voxnpl ) &
                 + start - 1

      else if ( item == kwvtpl )  then
!
!        Return vertex-plate list data.  There are VTXNPL values in
!        all. First locate the data.
!
         size = vtxnpl

         b =  ( ibase + ixplat + 3*np + voxnpt + voxnpl + nv ) &
              + start - 1

      else if ( item == kwcgpt )  then
!
!        Compute the coarse grid size.
!
         ncgr = nvxtot / (cgscal**3)

!
!        Return the coarse voxel grid occupancy pointers.  There are
!
!           NCGR
!
!        values in all. First locate the data.
!
         size = ncgr

         b =  (   ibase  + ixplat + 3*np   + voxnpt &
                + voxnpl + nv     + vtxnpl          ) &
                + start  - 1

      else

         call ctx%setmsg ( 'Keyword parameter # was not recognized.' )
         call ctx%errint ( '#',  item                                )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                     )
         return

      end if

!
!     The valid range for START is 1:SIZE.
!
      if (  ( start < 1 ) .or. ( start > size )  ) then

         call ctx%setmsg ( 'START must be in the range defined ' // &
                       'by the size of the data associated ' // &
                       'with the keyword parameter #, '      // &
                       'namely 1:#.  Actual value of START ' // &
                       'was #.'                              )
         call ctx%errint ( '#',  item                            )
         call ctx%errint ( '#',  size                            )
         call ctx%errint ( '#',  start                           )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         return

      end if

!
!     Read the requested data.  We already have the start address B.
!
      n  =  min ( room,  size - start + 1 )
      e  =  b + n - 1

      call dasrdi ( ctx, handle, b, e, values )

      return
      end associate
      end subroutine dski02

      !> Compute the unit normal vector for a specified plate from a type
      !> 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>     PLID     is the plate ID. Plate IDs range from 1 to NP
      !>              (the number of plates).
      !>
      !>## Detailed Output
      !>
      !>     NORMAL   is the normal vector associated with the plate
      !>              designated by PLID. The direction of NORMAL is
      !>              determined by the order of the plate's vertices;
      !>              the vertices are presumed to be ordered in the
      !>              right-handed (counterclockwise) sense about the
      !>              normal direction. If the plate's vertices are
      !>              V1, V2, V3, then NORMAL points in the direction
      !>
      !>                 (V2 - V1) x ( V3 - V2 )
      !>
      !>              where "x" represents the cross product operator.
      !>
      !>              The vector NORMAL is expressed in the body-fixed
      !>              reference frame of the segment designated by DLADSC.
      !>              The center of this frame is the origin of the cartesian
      !>              coordinate system in which the vertices are expressed.
      !>              Note that the frame center need not coincide with the
      !>              central body of the segment. Units are km.
      !>
      !>              The vector has magnitude 1.
      !>
      !>              If an error occurs on the call, NORMAL is undefined.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>     4)  If PLID is less than 1 or greater than the number of plates
      !>         in the segment, the error SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>     5)  This routine does not check for linear independence of the
      !>         plate's edges. The plate model is assumed to be geometrically
      !>         valid.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Look up all the vertices associated with each plate
      !>        of the model contained in a specified type 2 segment. For each
      !>        of the first 5 plates, display the plate's vertices and normal
      !>        vector.
      !>
      !>        For this example, we'll show the context of this look-up:
      !>        opening the DSK file for read access, traversing a trivial,
      !>        one-segment list to obtain the segment of interest.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKN02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
      !>
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 10000 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      VERTS  ( 3, BUFSIZ )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               N
      !>      INTEGER               NNORM
      !>      INTEGER               NP
      !>      INTEGER               NREAD
      !>      INTEGER               NV
      !>      INTEGER               NVTX
      !>      INTEGER               PLATES  ( 3, BUFSIZ )
      !>      INTEGER               PLIX
      !>      INTEGER               REMAIN
      !>      INTEGER               START
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for name of DSK and open file for reading.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>
      !>         CALL SETMSG ( 'No segment found in file #.' )
      !>         CALL ERRCH  ( '#',  DSK                     )
      !>         CALL SIGERR ( 'SPICE(NOSEGMENT)'            )
      !>
      !>      END IF
      !>
      !>C
      !>C     Get segment vertex and plate counts.
      !>C
      !>      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Number of vertices: ', NV
      !>      WRITE (*,*) 'Number of plates:   ', NP
      !>C
      !>C     Display the vertices of the first 5 plates.
      !>C
      !>      REMAIN = MIN(5, NP)
      !>      START  = 1
      !>
      !>      DO WHILE ( REMAIN .GT. 0 )
      !>C
      !>C        NREAD is the number of plates we'll read on this
      !>C        loop pass.
      !>C
      !>         NREAD  = MIN ( BUFSIZ, REMAIN )
      !>
      !>         CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N,
      !>     .                 PLATES                          )
      !>
      !>         DO I = 1, N
      !>
      !>            PLIX = START + I - 1
      !>C
      !>C           Read the vertices of the current plate.
      !>C
      !>            DO J = 1, 3
      !>               CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I),
      !>     .                       1,      NVTX,   VERTS (1,J)  )
      !>            END DO
      !>C
      !>C           Display the vertices of the current plate:
      !>C
      !>            WRITE (*,*  ) ' '
      !>            WRITE (*,*  ) 'Plate number: ', PLIX
      !>            WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3)
      !>
      !>C
      !>C           Display the normal vector of the current plate:
      !>C
      !>            CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL )
      !>
      !>            WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3)
      !>
      !>         END DO
      !>
      !>         START  = START  + NREAD
      !>         REMAIN = REMAIN - NREAD
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the kernel.  This isn't necessary in a stand-
      !>C     alone program, but it's good practice in subroutines
      !>C     because it frees program and system resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>
      !>         Number of vertices:      1579014
      !>         Number of plates:        3145728
      !>
      !>         Plate number:            1
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
      !>            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00
      !>
      !>         Plate number:            2
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00
      !>
      !>         Plate number:            3
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00
      !>
      !>         Plate number:            4
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00
      !>
      !>         Plate number:            5
      !>            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00
      !>
      !>        Note that only the vertex information for first 5 plates is
      !>        provided.
      !>
      !>## Restrictions
      !>
      !>     See $Exceptions section.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Modified code
      !>        example to reduce the output.
      !>
      !>        Extended detailed description of NORMAL output argument and
      !>        added an additional record to $Index_Entries.
      !>
      !>-    SPICELIB Version 1.0.0, 17-MAR-2016 (NJB)
      !>
      !>        Now calls ZZDDHHLU.
      !>
      !>        Deleted references to unused parameter. Updated
      !>        $Examples section.
      !>
      !>        DSKLIB Version 1.0.0, 02-JUN-2010 (NJB)
      module subroutine dskn02 ( ctx, handle, dladsc, plid, normal )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: ucrss
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(in) :: plid  !! Plate ID.
      real(wp), intent(out) :: normal ( 3 )  !! Plate's unit normal vector.

!     Local variables
!
      real(wp) :: dskdsc ( dskdsz )
      real(wp) :: edge1  ( 3 )
      real(wp) :: edge2  ( 3 )
      real(wp) :: verts  ( 3, 3 )

      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: np
      integer(ip) :: nv
      integer(ip) :: plate  ( 3 )
      integer(ip) :: start
      integer(ip) :: unit

      if ( ctx%failed() ) return

!
!     Look up the DSK descriptor for this segment.
!
      call dskgd ( ctx, handle, dladsc, dskdsc )

!
!     Get the plate model size parameters for this segment.
!     Note that we get a segment data type check for free from
!     DSKZ02.
!
      call dskz02 ( ctx, handle, dladsc, nv, np )

!
!     Check START.
!
      if (  ( plid < 1 ) .or. ( plid > np )  ) then

         call zzddhhlu ( ctx, handle, 'DAS', .false., unit )

         call ctx%setmsg ( 'Segment in DSK file # with DAS base ' &
         //            'addresses INT = #, DP = #, CHR = # ' &
         //            'contains # plates, so PLID must ' &
         //            'be in the range 1:#; actual value ' &
         //            'was #.'                              )
         call ctx%errfnm ( '#', unit                             )
         call ctx%errint ( '#', dladsc(ibsidx)                   )
         call ctx%errint ( '#', dladsc(dbsidx)                   )
         call ctx%errint ( '#', dladsc(cbsidx)                   )
         call ctx%errint ( '#', np                               )
         call ctx%errint ( '#', np                               )
         call ctx%errint ( '#', plid                             )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         return

      end if

!
!     Look up the plate and its vertices.
!
      start = (plid-1)*3  +  1

      call dski02 ( ctx, handle, dladsc, kwplat, start, 3, n, plate )

      do i = 1, 3

         start = ( plate(i) - 1 )*3  + 1

         call dskd02 ( ctx, handle, dladsc, kwvert, start, 3, n, verts(1,i) )

      end do

      if ( ctx%failed() ) return

!
!     Use the right-handed order of the vertices to determine the
!     correct choice of normal direction.
!
      edge1 = verts(:,2) - verts(:,1)
      edge2 = verts(:,3) - verts(:,1)

      call ucrss ( edge1, edge2, normal )

      return
      end subroutine dskn02

      !> Fetch triangular plates from a type 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>     START    is the ID of the first plate to be fetched from the
      !>              segment designated by HANDLE and DLADSC. The ID
      !>              of a plate is its ordinal position within the
      !>              segment. Plate IDs range from 1 to NP, where NP is
      !>              the number of plates in the segment.
      !>
      !>     ROOM     is the number of plates that can fit in the output
      !>              PLATES array: the output array must be large enough
      !>              to hold at least 3*ROOM integer values.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of plates fetched to the output
      !>              array PLATES. N is normally in the range
      !>
      !>                 1 : MIN( NP, ROOM )
      !>
      !>              If an error occurs on the call, N is undefined.
      !>
      !>     PLATES   is a contiguous set of plates. The returned
      !>              plates are arranged in order of increasing plate
      !>              ID. The IDs of the returned plates range from
      !>
      !>                 START
      !>
      !>              to
      !>
      !>                 START + N - 1
      !>
      !>              Each plate consists of three vertex indices. The
      !>              correspondence of elements of PLATES with the
      !>              elements of the set of plates contained in the
      !>              segment is:
      !>
      !>                 PLATES(1,1)      plate_set(1, START)
      !>                 PLATES(2,1)      plate_set(2, START)
      !>                 PLATES(3,1)      plate_set(3, START)
      !>                   ...             ...
      !>                 PLATES(1,N)      plate_set(1, START+N-1)
      !>                 PLATES(2,N)      plate_set(2, START+N-1)
      !>                 PLATES(3,N)      plate_set(3, START+N-1)
      !>
      !>              If an error occurs on the call, PLATES is
      !>              undefined.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>     4)  If ROOM is non-positive, the error SPICE(VALUEOUTOFRANGE)
      !>         is signaled.
      !>
      !>     5)  If START is less than 1 or greater than the number of plates
      !>         in the segment, the error SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine enables SPICE-based user applications to rapidly
      !>     fetch the plate data from a specified type 2 DSK segment. Using
      !>     a large output array generally improves efficiency.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Look up all the vertices associated with each plate
      !>        of the model contained in a specified type 2 segment. For each
      !>        plate, display the plate's vertices and normal vector.
      !>
      !>        For this example, we'll show the context of this look-up:
      !>        opening the DSK file for read access, traversing a trivial,
      !>        one-segment list to obtain the segment of interest.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKP02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
      !>
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 10000 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      VERTS  ( 3, BUFSIZ )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               N
      !>      INTEGER               NNORM
      !>      INTEGER               NP
      !>      INTEGER               NREAD
      !>      INTEGER               NV
      !>      INTEGER               NVTX
      !>      INTEGER               PLATES  ( 3, BUFSIZ )
      !>      INTEGER               PLIX
      !>      INTEGER               REMAIN
      !>      INTEGER               START
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for name of DSK and open file for reading.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>
      !>         CALL SETMSG ( 'No segment found in file #.' )
      !>         CALL ERRCH  ( '#',  DSK                     )
      !>         CALL SIGERR ( 'SPICE(NOSEGMENT)'            )
      !>
      !>      END IF
      !>
      !>C
      !>C     Get segment vertex and plate counts.
      !>C
      !>      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Number of vertices: ', NV
      !>      WRITE (*,*) 'Number of plates:   ', NP
      !>C
      !>C     Display the vertices of the first 5 plates.
      !>C
      !>      REMAIN = MIN(5, NP)
      !>      START  = 1
      !>
      !>      DO WHILE ( REMAIN .GT. 0 )
      !>C
      !>C        NREAD is the number of plates we'll read on this
      !>C        loop pass.
      !>C
      !>         NREAD  = MIN ( BUFSIZ, REMAIN )
      !>
      !>         CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N,
      !>     .                 PLATES                          )
      !>
      !>         DO I = 1, N
      !>
      !>            PLIX = START + I - 1
      !>C
      !>C           Read the vertices of the current plate.
      !>C
      !>            DO J = 1, 3
      !>               CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I),
      !>     .                       1,      NVTX,   VERTS (1,J)  )
      !>            END DO
      !>C
      !>C           Display the vertices of the current plate:
      !>C
      !>            WRITE (*,*  ) ' '
      !>            WRITE (*,*  ) 'Plate number: ', PLIX
      !>            WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3)
      !>
      !>C
      !>C           Display the normal vector of the current plate:
      !>C
      !>            CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL )
      !>
      !>            WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3)
      !>
      !>         END DO
      !>
      !>         START  = START  + NREAD
      !>         REMAIN = REMAIN - NREAD
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the kernel.  This isn't necessary in a stand-
      !>C     alone program, but it's good practice in subroutines
      !>C     because it frees program and system resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>
      !>         Number of vertices:      1579014
      !>         Number of plates:        3145728
      !>
      !>         Plate number:            1
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
      !>            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00
      !>
      !>         Plate number:            2
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00
      !>
      !>         Plate number:            3
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00
      !>
      !>         Plate number:            4
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00
      !>
      !>         Plate number:            5
      !>            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00
      !>
      !>        Note that only the vertex information for first 5 plates is
      !>        provided.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Modified code
      !>        example to reduce the output.
      !>
      !>-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
      !>
      !>        Now calls ZZDDHHLU.
      !>
      !>        Made trivial change to code example.
      !>
      !>        DSKLIB Version 1.0.1, 22-APR-2014 (NJB)
      !>
      !>           The diagram in the $Detailed_Output header section showing
      !>           the contents of the output PLATES array has been corrected.
      !>
      !>        DSKLIB Version 1.0.0, 02-JUN-2010 (NJB)
      module subroutine dskp02 ( ctx, handle, dladsc, start, room, n, plates )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(in) :: start  !! Start index.
      integer(ip), intent(in) :: room  !! Amount of room in output array.
      integer(ip), intent(out) :: n  !! Number of plates returned.
      integer(ip), intent(out) :: plates ( 3, * )  !! Array containing plates.

      real(wp) :: dskdsc ( dskdsz )

      integer(ip) :: lcroom
      integer(ip) :: lcstrt
      integer(ip) :: np
      integer(ip) :: nv
      integer(ip) :: unit

      if ( ctx%failed() ) return

!
!     Look up the DSK descriptor for this segment.
!
      call dskgd ( ctx, handle, dladsc, dskdsc )

!
!     Get the plate model size parameters for this segment.
!     Note that we get a segment data type check for free from
!     DSKZ02.
!
      call dskz02 ( ctx, handle, dladsc, nv, np )

!
!     Check START.
!
      if (  ( start < 1 ) .or. ( start > np )  ) then

         call zzddhhlu ( ctx, handle, 'DAS', .false., unit )

         call ctx%setmsg ( 'Segment in DSK file # with DAS base ' &
         //            'addresses INT = #, DP = #, CHR = # ' &
         //            'contains # plates, so START must ' &
         //            'be in the range 1:#; actual value ' &
         //            'was #.'                              )
         call ctx%errfnm ( '#', unit                             )
         call ctx%errint ( '#', dladsc(ibsidx)                   )
         call ctx%errint ( '#', dladsc(dbsidx)                   )
         call ctx%errint ( '#', dladsc(cbsidx)                   )
         call ctx%errint ( '#', np                               )
         call ctx%errint ( '#', np                               )
         call ctx%errint ( '#', start                            )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         return

      end if

!
!     Note that the start and room arguments of DSKI02 apply
!     to individual integers, not objects. So we adjust the
!     inputs to make them compatible with this lower-level
!     interface.
!
      lcstrt = 3 * ( start - 1 ) + 1
      lcroom = 3 * room

      call dski02 ( ctx, handle, dladsc, kwplat, &
                    lcstrt, lcroom, n,      plates )

!
!     Change the output count from one of integers to one of plates.
!
      n = n / 3

      return
      end subroutine dskp02

      !> Fetch vertices from a type 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>     START    is the ID of the first vertex to be fetched from
      !>              the segment designated by HANDLE and DLADSC. The
      !>              ID of a vertex is its ordinal position within the
      !>              segment. Vertex IDs range from 1 to NV, where NV
      !>              is the number of vertices in the segment.
      !>
      !>     ROOM     is the number of vertices that can fit in the
      !>              output VRTCES array: the output array must be
      !>              large enough to hold at least 3*ROOM double
      !>              precision values.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of vertices fetched to the output
      !>              array VRTCES. N is normally in the range
      !>
      !>                  1 : MIN( NV, ROOM )
      !>
      !>              If an error occurs on the call, N is undefined.
      !>
      !>     VRTCES   is a contiguous set of vertices. The returned
      !>              vertices are arranged in order of increasing vertex
      !>              ID. The IDs of the returned vertices range from
      !>
      !>                 START
      !>
      !>              to
      !>
      !>                 START + N - 1
      !>
      !>              Each vertex is a 3-vector. The correspondence of
      !>              elements of VRTCES with the elements of the set of
      !>              vertices contained in the segment is:
      !>
      !>                 VRTCES(1,1)      vertex_set(1, START)
      !>                 VRTCES(2,1)      vertex_set(2, START)
      !>                 VRTCES(3,1)      vertex_set(3, START)
      !>                   ...             ...
      !>                 VRTCES(1,N)      vertex_set(1, START+N-1)
      !>                 VRTCES(2,N)      vertex_set(2, START+N-1)
      !>                 VRTCES(3,N)      vertex_set(3, START+N-1)
      !>
      !>              The vertices are expressed in the body-fixed
      !>              reference frame of the segment designated by
      !>              HANDLE and DLADSC. The center of this frame is the
      !>              origin of the Cartesian coordinate system in which
      !>              the vertices are expressed. Note that the frame
      !>              center need not coincide with the central body of
      !>              the segment. Units are km.
      !>
      !>              If an error occurs on the call, VRTCES is
      !>              undefined.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>     4)  If ROOM is non-positive, the error SPICE(VALUEOUTOFRANGE)
      !>         is signaled.
      !>
      !>     5)  If START is less than 1 or greater than the number of
      !>         vertices in the segment, the error SPICE(INDEXOUTOFRANGE) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine enables SPICE-based user applications to rapidly
      !>     fetch the vertex data from a specified type 2 DSK segment. Using
      !>     a large output array generally improves efficiency.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Look up all the vertices associated with each plate
      !>        of the model contained in a specified type 2 segment. For each
      !>        plate, display the plate's vertices and normal vector.
      !>
      !>        For this example, we'll show the context of this look-up:
      !>        opening the DSK file for read access, traversing a trivial,
      !>        one-segment list to obtain the segment of interest.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKV02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
      !>
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 10000 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      VERTS  ( 3, BUFSIZ )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               N
      !>      INTEGER               NNORM
      !>      INTEGER               NP
      !>      INTEGER               NREAD
      !>      INTEGER               NV
      !>      INTEGER               NVTX
      !>      INTEGER               PLATES  ( 3, BUFSIZ )
      !>      INTEGER               PLIX
      !>      INTEGER               REMAIN
      !>      INTEGER               START
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for name of DSK and open file for reading.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>
      !>         CALL SETMSG ( 'No segment found in file #.' )
      !>         CALL ERRCH  ( '#',  DSK                     )
      !>         CALL SIGERR ( 'SPICE(NOSEGMENT)'            )
      !>
      !>      END IF
      !>
      !>C
      !>C     Get segment vertex and plate counts.
      !>C
      !>      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Number of vertices: ', NV
      !>      WRITE (*,*) 'Number of plates:   ', NP
      !>C
      !>C     Display the vertices of the first 5 plates.
      !>C
      !>      REMAIN = MIN(5, NP)
      !>      START  = 1
      !>
      !>      DO WHILE ( REMAIN .GT. 0 )
      !>C
      !>C        NREAD is the number of plates we'll read on this
      !>C        loop pass.
      !>C
      !>         NREAD  = MIN ( BUFSIZ, REMAIN )
      !>
      !>         CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N,
      !>     .                 PLATES                          )
      !>
      !>         DO I = 1, N
      !>
      !>            PLIX = START + I - 1
      !>C
      !>C           Read the vertices of the current plate.
      !>C
      !>            DO J = 1, 3
      !>               CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I),
      !>     .                       1,      NVTX,   VERTS (1,J)  )
      !>            END DO
      !>C
      !>C           Display the vertices of the current plate:
      !>C
      !>            WRITE (*,*  ) ' '
      !>            WRITE (*,*  ) 'Plate number: ', PLIX
      !>            WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3)
      !>
      !>C
      !>C           Display the normal vector of the current plate:
      !>C
      !>            CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL )
      !>
      !>            WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3)
      !>
      !>         END DO
      !>
      !>         START  = START  + NREAD
      !>         REMAIN = REMAIN - NREAD
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the kernel.  This isn't necessary in a stand-
      !>C     alone program, but it's good practice in subroutines
      !>C     because it frees program and system resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>
      !>         Number of vertices:      1579014
      !>         Number of plates:        3145728
      !>
      !>         Plate number:            1
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
      !>            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00
      !>
      !>         Plate number:            2
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00
      !>
      !>         Plate number:            3
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00
      !>
      !>         Plate number:            4
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00
      !>
      !>         Plate number:            5
      !>            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00
      !>
      !>        Note that only the vertex information for first 5 plates is
      !>        provided.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Modified code
      !>        example to reduce the output.
      !>
      !>-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
      !>
      !>        Now calls ZZDDHHLU.
      !>
      !>        Updated detailed description of VRTCES. Updated comments
      !>        in $Examples.
      !>
      !>        DSKLIB Version 1.0.1, 21-APR-2014 (NJB)
      !>
      !>           The diagram in the $Detailed_Output header section showing
      !>           the contents of the output VRTCES array has been corrected.
      !>
      !>        DSKLIB Version 1.0.0, 02-JUN-2010 (NJB)
      module subroutine dskv02 ( ctx, handle, dladsc, start, room, n, vrtces )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(in) :: start  !! Start index.
      integer(ip), intent(in) :: room  !! Amount of room in output array.
      integer(ip), intent(out) :: n  !! Number of vertices returned.
      real(wp), intent(out) :: vrtces ( 3, * )  !! Array containing vertices.

      real(wp) :: dskdsc ( dskdsz )

      integer(ip) :: lcroom
      integer(ip) :: lcstrt
      integer(ip) :: np
      integer(ip) :: nv
      integer(ip) :: unit

      if ( ctx%failed() ) return

!
!     Look up the DSK descriptor for this segment.
!
      call dskgd ( ctx, handle, dladsc, dskdsc )

!
!     Get the plate model size parameters for this segment.
!     Note that we get a segment data type check for free from
!     DSKZ02.
!
      call dskz02 ( ctx, handle, dladsc, nv, np )

!
!     Check START.
!
      if (  ( start < 1 ) .or. ( start > nv )  ) then

         call zzddhhlu ( ctx, handle, 'DAS', .false., unit )

         call ctx%setmsg ( 'Segment in DSK file # with DAS base ' &
         //            'addresses INT = #, DP = #, CHR = # ' &
         //            'contains # vertices, so START must ' &
         //            'be in the range 1:#; actual value ' &
         //            'was #.'                              )
         call ctx%errfnm ( '#', unit                             )
         call ctx%errint ( '#', dladsc(ibsidx)                   )
         call ctx%errint ( '#', dladsc(dbsidx)                   )
         call ctx%errint ( '#', dladsc(cbsidx)                   )
         call ctx%errint ( '#', nv                               )
         call ctx%errint ( '#', nv                               )
         call ctx%errint ( '#', start                            )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         return

      end if

!
!     Note that the start and room arguments of DSKD02 apply
!     to individual d.p. numbers, not objects. So we adjust the
!     inputs to make them compatible with this lower-level
!     interface.
!
      lcstrt = 3 * ( start - 1 ) + 1
      lcroom = 3 * room

      call dskd02 ( ctx, handle, dladsc, kwvert, &
                    lcstrt, lcroom, n,      vrtces )

!
!     Change the output count from one of d.p. numbers to one of
!     vertices.
!
      n = n / 3

      return
      end subroutine dskv02

      !> All the plates of a type 2 DSK segment (PEPPER; [[dskp02]] with the
      !> size from [[dskz02]]).
      module subroutine dsk_plates ( ctx, handle, dladsc, plates )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! the handle of the DSK file
      integer(ip), intent(in) :: dladsc(*)  !! the DLA descriptor of the segment
      integer(ip), allocatable, intent(out) :: plates(:,:)  !! the plates, shape (3, n): vertex indices (n = 0 after an error)
      integer(ip) :: n
      integer(ip) :: np
      integer(ip) :: nv
      allocate ( plates(3,0) )
      if ( ctx%failed() ) return
      call dskz02 ( ctx, handle, dladsc, nv, np )
      if ( ctx%failed() .or. np < 1 ) return
      deallocate ( plates )
      allocate ( plates(3,np) )
      call dskp02 ( ctx, handle, dladsc, 1, np, n, plates )
      if ( ctx%failed() ) then
         deallocate ( plates )
         allocate ( plates(3,0) )
      end if
      end subroutine dsk_plates

      !> All the vertices of a type 2 DSK segment (PEPPER; [[dskv02]] with
      !> the size from [[dskz02]]).
      module subroutine dsk_vertices ( ctx, handle, dladsc, vertices )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! the handle of the DSK file
      integer(ip), intent(in) :: dladsc(*)  !! the DLA descriptor of the segment
      real(wp), allocatable, intent(out) :: vertices(:,:)  !! the vertices, shape (3, n) (n = 0 after an error)
      integer(ip) :: n
      integer(ip) :: np
      integer(ip) :: nv
      allocate ( vertices(3,0) )
      if ( ctx%failed() ) return
      call dskz02 ( ctx, handle, dladsc, nv, np )
      if ( ctx%failed() .or. nv < 1 ) return
      deallocate ( vertices )
      allocate ( vertices(3,nv) )
      call dskv02 ( ctx, handle, dladsc, 1, nv, n, vertices )
      if ( ctx%failed() ) then
         deallocate ( vertices )
         allocate ( vertices(3,0) )
      end if
      end subroutine dsk_vertices

      !> Return plate model size parameters---plate count and
      !> vertex count---for a type 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DSK file containing a type 2
      !>              segment from which data are to be fetched.
      !>
      !>     DLADSC   is the DLA descriptor associated with the segment
      !>              from which data are to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     NV       is the number of vertices belonging to
      !>              the specified plate model.
      !>
      !>     NP       is the number of plates belonging to the
      !>              specified plate model.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If a file read error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the input DLA descriptor is invalid, the effect of this
      !>         routine is undefined. The error *may* be diagnosed by
      !>         routines in the call tree of this routine, but there are no
      !>         guarantees.
      !>
      !>## Files
      !>
      !>     See input argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine enables SPICE-based user applications to
      !>     conveniently fetch the plate and vertex counts of a type 2 DSK
      !>     segment.
      !>
      !>     See the routine DSKB02 (DSK, fetch type 2 bookkeeping data)
      !>     for an interface that returns all type 2 DSK segment
      !>     bookkeeping data in a single call.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Look up all the vertices associated with each plate
      !>        of the model contained in a specified type 2 segment. For each
      !>        plate, display the plate's vertices and normal vector.
      !>
      !>        For this example, we'll show the context of this look-up:
      !>        opening the DSK file for read access, traversing a trivial,
      !>        one-segment list to obtain the segment of interest.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSKZ02_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(1X,A,3(1XE15.8))' )
      !>
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 10000 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      CHARACTER*(FILSIZ)    DSK
      !>
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      VERTS  ( 3, BUFSIZ )
      !>
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               N
      !>      INTEGER               NNORM
      !>      INTEGER               NP
      !>      INTEGER               NREAD
      !>      INTEGER               NV
      !>      INTEGER               NVTX
      !>      INTEGER               PLATES  ( 3, BUFSIZ )
      !>      INTEGER               PLIX
      !>      INTEGER               REMAIN
      !>      INTEGER               START
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for name of DSK and open file for reading.
      !>C
      !>      CALL PROMPT ( 'Enter DSK name > ', DSK )
      !>
      !>      CALL DASOPR ( DSK, HANDLE )
      !>
      !>      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>
      !>         CALL SETMSG ( 'No segment found in file #.' )
      !>         CALL ERRCH  ( '#',  DSK                     )
      !>         CALL SIGERR ( 'SPICE(NOSEGMENT)'            )
      !>
      !>      END IF
      !>
      !>C
      !>C     Get segment vertex and plate counts.
      !>C
      !>      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Number of vertices: ', NV
      !>      WRITE (*,*) 'Number of plates:   ', NP
      !>C
      !>C     Display the vertices of the first 5 plates.
      !>C
      !>      REMAIN = MIN(5, NP)
      !>      START  = 1
      !>
      !>      DO WHILE ( REMAIN .GT. 0 )
      !>C
      !>C        NREAD is the number of plates we'll read on this
      !>C        loop pass.
      !>C
      !>         NREAD  = MIN ( BUFSIZ, REMAIN )
      !>
      !>         CALL DSKP02 ( HANDLE, DLADSC, START, NREAD, N,
      !>     .                 PLATES                          )
      !>
      !>         DO I = 1, N
      !>
      !>            PLIX = START + I - 1
      !>C
      !>C           Read the vertices of the current plate.
      !>C
      !>            DO J = 1, 3
      !>               CALL DSKV02 ( HANDLE, DLADSC, PLATES(J,I),
      !>     .                       1,      NVTX,   VERTS (1,J)  )
      !>            END DO
      !>C
      !>C           Display the vertices of the current plate:
      !>C
      !>            WRITE (*,*  ) ' '
      !>            WRITE (*,*  ) 'Plate number: ', PLIX
      !>            WRITE (*,FMT) '   Vertex 1: ', (VERTS(J,1), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 2: ', (VERTS(J,2), J=1,3)
      !>            WRITE (*,FMT) '   Vertex 3: ', (VERTS(J,3), J=1,3)
      !>
      !>C
      !>C           Display the normal vector of the current plate:
      !>C
      !>            CALL DSKN02 ( HANDLE, DLADSC, PLIX, NORMAL )
      !>
      !>            WRITE (*,FMT) '   Normal:   ', (NORMAL(J), J=1,3)
      !>
      !>         END DO
      !>
      !>         START  = START  + NREAD
      !>         REMAIN = REMAIN - NREAD
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the kernel.  This isn't necessary in a stand-
      !>C     alone program, but it's good practice in subroutines
      !>C     because it frees program and system resources.
      !>C
      !>      CALL DASCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter DSK name > phobos512.bds
      !>
      !>         Number of vertices:      1579014
      !>         Number of plates:        3145728
      !>
      !>         Plate number:            1
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Vertex 3:  -0.67571000E+01  0.62775400E+01  0.60209600E+01
      !>            Normal:    -0.58197377E+00  0.32128561E+00  0.74704892E+00
      !>
      !>         Plate number:            2
      !>            Vertex 1:  -0.67744400E+01  0.62681500E+01  0.60114900E+01
      !>            Vertex 2:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58145695E+00  0.32198831E+00  0.74714881E+00
      !>
      !>         Plate number:            3
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Vertex 3:  -0.67623800E+01  0.62572800E+01  0.60255600E+01
      !>            Normal:    -0.58159707E+00  0.32264196E+00  0.74675767E+00
      !>
      !>         Plate number:            4
      !>            Vertex 1:  -0.67797300E+01  0.62479000E+01  0.60161000E+01
      !>            Vertex 2:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58312901E+00  0.32056070E+00  0.74645924E+00
      !>
      !>         Plate number:            5
      !>            Vertex 1:  -0.67849900E+01  0.62276200E+01  0.60207000E+01
      !>            Vertex 2:  -0.67729900E+01  0.62167400E+01  0.60348200E+01
      !>            Vertex 3:  -0.67676800E+01  0.62370100E+01  0.60301900E+01
      !>            Normal:    -0.58366405E+00  0.32306020E+00  0.74496200E+00
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Modified code
      !>        example to reduce the output.
      !>
      !>-    SPICELIB Version 1.0.0, 02-JUN-2010 (NJB)
      module subroutine dskz02 ( ctx, handle, dladsc, nv, np )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DSK file handle.
      integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
      integer(ip), intent(out) :: nv  !! Number of vertices.
      integer(ip), intent(out) :: np  !! Number of plates.
      integer(ip) :: np_1(1)  !! one-element copy of np (array argument)
      integer(ip) :: nv_1(1)  !! one-element copy of nv (array argument)

      integer(ip) :: n

      if ( ctx%failed() ) return

      call dski02 ( ctx, handle, dladsc, kwnv, 1, 1, n, nv_1 )
      nv = nv_1(1)
      call dski02 ( ctx, handle, dladsc, kwnp, 1, 1, n, np_1 )
      np = np_1(1)

      return
      end subroutine dskz02

      !> Make spatial index for a DSK type 2 segment. The index is
      !> returned as a pair of arrays, one of type INTEGER and one of type
      !> DOUBLE PRECISION. These arrays are suitable for use with the DSK
      !> type 2 writer [[dskw02]].
      !>
      !>## Detailed Input
      !>
      !>     NV       is the number of vertices belonging to the input
      !>              set of plates.
      !>
      !>     VRTCES   is an array of coordinates of the vertices. The Ith
      !>              vertex occupies elements (1:3,I) of this array.
      !>
      !>     NP       is the number of plates in the input plate set.
      !>
      !>     PLATES   is an array representing the triangular plates of a
      !>              shape model. The elements of PLATES are vertex
      !>              indices; vertex indices are 1-based. The vertex
      !>              indices of the Ith plate occupy elements (1:3,I) of
      !>              this array.
      !>
      !>     FINSCL   is the fine voxel scale. This scale determines the
      !>              edge length of the cubical voxels comprising the fine
      !>              voxel grid: the edge length VOXSIZ is approximately
      !>
      !>                  FINSCL * {average plate extent}
      !>
      !>              where the extents of a plate are the respective
      !>              differences between the maximum and minimum
      !>              coordinate values of the plate's vertices.
      !>
      !>              The relationship between VOXSIZ and the average plate
      !>              extent is approximate because the VOXSIZ is adjusted
      !>              so that each dimension of the fine voxel grid is an
      !>              integer multiple of the coarse voxel scale.
      !>
      !>              See the $Particulars section below for further
      !>              information on voxel scales.
      !>
      !>     CORSCL   is the coarse voxel scale. This integer scale is the
      !>              ratio of the edge length of coarse voxels to
      !>              that of fine voxels. The coarse scale must be
      !>              large enough so that the total number of coarse
      !>              voxels does not exceed MAXCGR (see the $Parameters
      !>              section below).
      !>
      !>     WORKSZ   is the second dimension of the workspace array WORK.
      !>              WORKSZ must be at least as large as the greater of
      !>
      !>                 - the number of fine voxel-plate associations
      !>
      !>                   This number is equal to
      !>
      !>                      NP * {average number of fine voxels
      !>                            intersected by each plate}
      !>
      !>                 - the number of vertex-plate associations, if
      !>                   the vertex-plate mapping is constructed.
      !>
      !>                   This number is equal to
      !>
      !>                      NV + ( 3 * NP )
      !>
      !>     VOXPSZ   is the size of the fine voxel-plate pointer array.
      !>              This array maps fine voxels to lists of plates that
      !>              intersect those voxels. VOXPSZ must be at least as
      !>              large as
      !>
      !>                       3
      !>                 CORSCL  * {number of non-empty coarse voxels}
      !>
      !>     VOXLSZ   is the size of the fine voxel-plate list array. This
      !>              array contains, for each non-empty fine voxel, the
      !>              count of plates that intersect that voxel and the
      !>              IDs of those plates. VOXLSZ must be at least as large
      !>              as
      !>
      !>                      NP * {average number of fine voxels
      !>                            intersected by each plate}
      !>
      !>                  +   {number of non-empty fine voxels}
      !>
      !>     MAKVTL   is a logical flag that, when set to .TRUE., indicates
      !>              that a  vertex-plate association list is to be
      !>              constructed.
      !>
      !>              The amount of workspace that is needed may depend on
      !>              whether a vertex-plate association list is
      !>              constructed. When this list is constructed, the size
      !>              of the integer component of the spatial index is
      !>              increased by the size of the list and the size of a
      !>              vertex-plate pointer array; the total of these sizes
      !>              is
      !>
      !>                 ( 2 * NV ) + ( 3 * NP )
      !>
      !>     SPXISZ   is the declared size of the output array SPAIXI. This
      !>              size must be at least as large as the sum of
      !>
      !>                 - the fixed-size part of the integer component of
      !>                   the index, which includes the coarse voxel grid;
      !>                   this value is
      !>
      !>                      IXIFIX
      !>
      !>                 - the size VOXPSZ of the voxel-plate pointer array
      !>
      !>                 - the size VOXLSZ of the voxel-plate association
      !>                   list
      !>
      !>              plus, if the vertex-plate association list is
      !>              constructed,
      !>
      !>                 - the size NV of the vertex-plate pointer array
      !>
      !>                 - the size of the vertex-plate association list;
      !>                   this size is
      !>
      !>                      NV + ( 3 * NP )
      !>
      !>     WORK     is the workspace array. The array should be declared
      !>              with dimensions
      !>
      !>                 (2, WORKSZ)
      !>
      !>              See the description of WORKSZ above.
      !>
      !>## Detailed Output
      !>
      !>     WORK     is the workspace array, modified by the operations
      !>              performed by this routine.
      !>
      !>     SPAIXD,
      !>     SPAIXI   are, respectively, the double precision and integer
      !>              components of the spatial index of the segment.
      !>
      !>              SPAIXD must be declared with size at least IXDFIX.
      !>              SPAIXI must be declared with size at least SPXISZ.
      !>
      !>## Parameters
      !>
      !>     IXDFIX   is the size of the double precision component of
      !>              the spatial index.
      !>
      !>     IXIFIX   is the size of the fixed-size portion of the integer
      !>              component of the spatial index.
      !>
      !>     See the include file
      !>
      !>        dsk02.inc
      !>
      !>     for declarations of DSK data type 2 (plate model) parameters.
      !>
      !>     See the include file
      !>
      !>        dla.inc
      !>
      !>     for declarations of DLA descriptor sizes and documentation of the
      !>     contents of DLA descriptors.
      !>
      !>     See the include file
      !>
      !>        dskdsc.inc
      !>
      !>     for declarations of DSK descriptor sizes and documentation of the
      !>     contents of DSK descriptors.
      !>
      !>## Exceptions
      !>
      !>     1)  If the fine voxel scale is non-positive, the error
      !>         SPICE(BADFINEVOXELSCALE) is signaled.
      !>
      !>     2)  If the coarse voxel scale is less than 1, the error
      !>         SPICE(BADCOARSEVOXSCALE) is signaled.
      !>
      !>     3)  If NV is less than 3 or greater than MAXVRT, the error
      !>         SPICE(BADVERTEXCOUNT) is signaled.
      !>
      !>     4)  If NP is less than 1 or greater than MAXPLT, the error
      !>         SPICE(BADPLATECOUNT) is signaled.
      !>
      !>     5)  If the workspace size WORKSZ is less than NP+1, the error
      !>         SPICE(WORKSPACETOOSMALL) is signaled. This is merely a
      !>         sanity check; normally the workspace will need to be
      !>         substantially larger than this reference value. See the
      !>         description of WORKSZ in the header section $Detailed_Input
      !>         above.
      !>
      !>     6)  If the voxel-plate pointer array size VOXPSZ is less than 1,
      !>         the error SPICE(PTRARRAYTOOSMALL) is signaled. This is merely
      !>         a sanity check; normally this pointer array will need to be
      !>         substantially larger than this reference value. See the
      !>         description of VOXPSZ in the header section $Detailed_Input
      !>         above.
      !>
      !>     7)  If the voxel-plate list array size VOXLSZ is less than NP+1,
      !>         the error SPICE(PLATELISTTOOSMALL) is signaled. This is
      !>         merely a sanity check; normally this array will need to be
      !>         substantially larger than this reference value. See the
      !>         description of VOXLSZ in the header section $Detailed_Input
      !>         above.
      !>
      !>     8)  If the size SPXISZ of the integer array SPAIXI is too small
      !>         to contain its constituent structures, where the sizes
      !>         of these structures are derived from the inputs
      !>
      !>             NV, NP, VOXPSZ, VOXLSZ
      !>
      !>         the error SPICE(INTINDEXTOOSMALL) is signaled.
      !>
      !>     9)  If there is insufficient room to create any of the data
      !>         structures contained in the spatial index, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Users planning to create DSK files should consider whether the
      !>     SPICE DSK creation utility MKDSK may be suitable for their needs.
      !>
      !>     This routine supports use of the DSK type 2 segment writer DSKW02
      !>     by creating the "spatial index" arrays required as inputs to that
      !>     routine.
      !>
      !>     A spatial index is a group of data structures that facilitates
      !>     rapid high-level computations involving sets of plates. The data
      !>     structures created by this routine are aggregated into arrays
      !>     of type INTEGER and type DOUBLE PRECISION.
      !>
      !>     Voxel grids
      !>     ===========
      !>
      !>     A key geometric computation---probably the most important, as it
      !>     serves as a foundation for other high-level computations---is
      !>     finding the intersection of a ray with the plate set. DSK type 2
      !>     segments use data structures called "voxel grids" as part of
      !>     their indexing mechanism. There is a "coarse grid": a box that
      !>     completely encloses a DSK type 2 segment's plate set, and which
      !>     is composed of identically-sized cubes called "coarse voxels."
      !>     Each coarse voxel in composed of smaller cubes called "fine
      !>     voxels." When the term "voxel" is used without qualification, it
      !>     refers to fine voxels.
      !>
      !>     Type 2 DSK segments contain data structures that associate plates
      !>     with the fine voxels intersected by those plates. These
      !>     structures enable the type 2 DSK software to rapidly find plates
      !>     in a given region of space.
      !>
      !>     Voxel scales
      !>     ============
      !>
      !>     There are two voxel scales:
      !>
      !>     -  The coarse voxel scale is the integer ratio of the
      !>        edge length of a coarse voxel to the edge length of
      !>        a fine voxel
      !>
      !>     -  The fine voxel scale is the double precision ratio
      !>        of the edge length of a fine voxel to the average
      !>        extent of the plates in the input plate set. "Extents"
      !>        of a plate are the absolute values of the differences
      !>        between the respective maximum and minimum X, Y, and Z
      !>        coordinates of the plate's vertices.
      !>
      !>     Voxel scales determine the resolution of the voxel grid.
      !>     Voxel scales must be chosen to satisfy size constraints and
      !>     provide reasonable plate lookup performance.
      !>
      !>     The following considerations apply to spatial indexes of
      !>     type 2 DSK segments:
      !>
      !>        1)  The maximum number of coarse voxels is fixed at MAXCGR
      !>            (declared in dsk02.inc).
      !>
      !>        2)  If there are too few fine voxels, the average number of
      !>            plates per fine voxel will be very large. This largely
      !>            negates the performance improvement afforded by having an
      !>            index. Also, the number of plates per voxel may exceed
      !>            limits imposed by DSK subroutines that use static arrays.
      !>
      !>        3)  If there are too many fine voxels, the average number of
      !>            voxels intersected by a given plate may be too large for
      !>            all the plate-voxel associations to be stored. In
      !>            addition, the time needed to examine the plate lists for
      !>            each voxel (including the empty ones) may become quite
      !>            large, again negating the value of the index.
      !>
      !>     In many cases, voxel scales yielding optimum performance must be
      !>     determined by experiment. However, the following heuristics can
      !>     provide reasonable starting values:
      !>
      !>        Let NP be the number of plates. Let FS be the fine voxel
      !>        scale. Then a reasonable value of FS may be
      !>
      !>                   (0.25D0)
      !>           FS =  NP       / 8.D0
      !>
      !>        In general, FS should not smaller than 1.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a three-segment DSK file using plate model data for
      !>        Phobos. Use latitudinal, rectangular, and planetodetic
      !>        coordinates in the respective segments. This is not a
      !>        realistic example, but it serves to demonstrate use of
      !>        the supported coordinate systems.
      !>
      !>        Use the DSK kernel below to provide, for simplicity, the
      !>        input plate and vertex data. The selected input file has one
      !>        segment.
      !>
      !>           phobos_3_3.bds
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>C
      !>C     Example program for DSKW02, DSKMI2, and DSKRB2
      !>C
      !>C        Create a three-segment DSK file using plate model
      !>C        data for Phobos. Use latitudinal, rectangular, and
      !>C        planetodetic coordinates in the respective segments.
      !>C
      !>C        For simplicity, use an existing DSK file to provide
      !>C        the input plate and vertex data. The selected input
      !>C        file has one segment.
      !>C
      !>C           Version 1.0.0 22-JAN-2016 (NJB)
      !>C
      !>      PROGRAM DSKMI2_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      JYEAR
      !>      DOUBLE PRECISION      PI
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FRNMLN
      !>      PARAMETER           ( FRNMLN = 32 )
      !>
      !>      INTEGER               NSEG
      !>      PARAMETER           ( NSEG   = 3 )
      !>
      !>      INTEGER               NAMLEN
      !>      PARAMETER           ( NAMLEN = 20 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE = 80 )
      !>
      !>      INTEGER               NCOR
      !>      PARAMETER           ( NCOR   = 4 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(NAMLEN)    CORNAM ( NCOR )
      !>      CHARACTER*(FILSIZ)    DSK
      !>      CHARACTER*(FRNMLN)    FRAME
      !>      CHARACTER*(FILSIZ)    INDSK
      !>      CHARACTER*(LNSIZE)    LINE
      !>C
      !>C     Note: the values of MAXVRT and MAXPLT declared
      !>C     in dsk02.inc, and the integer spatial index
      !>C     dimension SPAISZ are very large. Smaller buffers
      !>C     can be used for most applications.
      !>C
      !>      DOUBLE PRECISION      CORPAR ( NSYPAR )
      !>      DOUBLE PRECISION      F
      !>      DOUBLE PRECISION      FINSCL
      !>      DOUBLE PRECISION      FIRST
      !>      DOUBLE PRECISION      LAST
      !>      DOUBLE PRECISION      MNCOR1
      !>      DOUBLE PRECISION      MNCOR2
      !>      DOUBLE PRECISION      MNCOR3
      !>      DOUBLE PRECISION      MXCOR1
      !>      DOUBLE PRECISION      MXCOR2
      !>      DOUBLE PRECISION      MXCOR3
      !>      DOUBLE PRECISION      RE
      !>      DOUBLE PRECISION      RP
      !>      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      !>      DOUBLE PRECISION      VRTCES ( 3, MAXVRT )
      !>
      !>      INTEGER               CENTER
      !>      INTEGER               CORSCL
      !>      INTEGER               CORSYS
      !>      INTEGER               DCLASS
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               INHAN
      !>      INTEGER               NP
      !>      INTEGER               NV
      !>      INTEGER               PLATES ( 3, MAXPLT )
      !>      INTEGER               SEGNO
      !>      INTEGER               SPAIXI ( SPAISZ )
      !>      INTEGER               SURFID
      !>      INTEGER               VOXPSZ
      !>      INTEGER               VOXLSZ
      !>      INTEGER               WORK   ( 2, MAXCEL )
      !>      INTEGER               WORKSZ
      !>
      !>      LOGICAL               FOUND
      !>C
      !>C     Saved variables
      !>C
      !>C     Save all large arrays to avoid stack problems.
      !>C
      !>      SAVE
      !>C
      !>C     Initial values
      !>C
      !>      DATA                  CORNAM / 'radius',
      !>     .                               'Z-coordinate',
      !>     .                               'Z-coordinate',
      !>     .                               'altitude'     /
      !>
      !>C
      !>C     Assign names of input and output DSK files.
      !>C
      !>      INDSK = 'phobos_3_3.bds'
      !>      DSK   = 'phobos_3_3_3seg.bds'
      !>C
      !>C     Open input DSK for read access; find first segment.
      !>C
      !>      CALL DASOPR ( INDSK, INHAN )
      !>      CALL DLABFS ( INHAN, DLADSC, FOUND )
      !>C
      !>C     Fetch vertices and plates from input DSK file.
      !>C
      !>      WRITE (*,*) 'Reading input data...'
      !>
      !>      CALL DSKV02 ( INHAN, DLADSC, 1, MAXVRT, NV, VRTCES )
      !>      CALL DSKP02 ( INHAN, DLADSC, 1, MAXPLT, NP, PLATES )
      !>
      !>      WRITE (*,*) 'Done.'
      !>C
      !>C     Set input array sizes required by DSKMI2.
      !>C
      !>      VOXPSZ = MAXVXP
      !>      VOXLSZ = MXNVLS
      !>      WORKSZ = MAXCEL
      !>C
      !>C     Set fine and coarse voxel scales. (These usually
      !>C     need to determined by experimentation.)
      !>C
      !>      FINSCL = 5.D0
      !>      CORSCL = 4
      !>C
      !>C     Open a new DSK file.
      !>C
      !>      CALL DSKOPN ( DSK, DSK, 0, HANDLE )
      !>C
      !>C     Create three segments and add them to the file.
      !>C
      !>      DO SEGNO = 1, NSEG
      !>C
      !>C        Create spatial index.
      !>C
      !>         WRITE (*,*) 'Creating segment ', SEGNO
      !>         WRITE (*,*) 'Creating spatial index...'
      !>
      !>         CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
      !>     .                 CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE.,
      !>     .                 SPAISZ, WORK,   SPAIXD, SPAIXI        )
      !>
      !>         WRITE (*,*) 'Done.'
      !>C
      !>C        Set up inputs describing segment attributes:
      !>C
      !>C        - Central body: Phobos
      !>C        - Surface ID code: user's choice.
      !>C          We use the segment number here.
      !>C        - Data class: general (arbitrary) shape
      !>C        - Body-fixed reference frame
      !>C        - Time coverage bounds (TBD)
      !>C
      !>         CENTER = 401
      !>         SURFID = SEGNO
      !>         DCLASS = GENCLS
      !>         FRAME  = 'IAU_PHOBOS'
      !>
      !>         FIRST = -50 * JYEAR()
      !>         LAST  =  50 * JYEAR()
      !>C
      !>C        Set the coordinate system and coordinate system
      !>C        bounds based on the segment index.
      !>C
      !>C        Zero out the coordinate parameters to start.
      !>C
      !>         CALL CLEARD ( NSYPAR, CORPAR )
      !>
      !>         IF ( SEGNO .EQ. 1 ) THEN
      !>C
      !>C           Use planetocentric latitudinal coordinates. Set
      !>C           the longitude and latitude bounds.
      !>C
      !>            CORSYS = LATSYS
      !>
      !>            MNCOR1 = -PI()
      !>            MXCOR1 =  PI()
      !>            MNCOR2 = -PI()/2
      !>            MXCOR2 =  PI()/2
      !>
      !>         ELSE IF ( SEGNO .EQ. 2 ) THEN
      !>C
      !>C           Use rectangular coordinates. Set the
      !>C           X and Y bounds.
      !>C
      !>C           The bounds shown here were derived from
      !>C           the plate data. They lie slightly outside
      !>C           of the range spanned by the plates.
      !>C
      !>            CORSYS = RECSYS
      !>
      !>            MNCOR1 = -1.3D0
      !>            MXCOR1 =  1.31D0
      !>            MNCOR2 = -1.21D0
      !>            MXCOR2 =  1.2D0
      !>
      !>         ELSE
      !>C
      !>C           Set the coordinate system to planetodetic.
      !>C
      !>            CORSYS    = PDTSYS
      !>
      !>            MNCOR1    = -PI()
      !>            MXCOR1    =  PI()
      !>            MNCOR2    = -PI()/2
      !>            MXCOR2    =  PI()/2
      !>C
      !>C           We'll use equatorial and polar radii from
      !>C           pck00010.tpc. These normally would be fetched
      !>C           at run time, but for simplicity, we'll use
      !>C           hard-coded values.
      !>
      !>            RE        = 13.0D0
      !>            RP        =  9.1D0
      !>            F         = ( RE - RP ) / RE
      !>
      !>            CORPAR(1) = RE
      !>            CORPAR(2) = F
      !>
      !>         END IF
      !>C
      !>C        Compute plate model radius bounds.
      !>C
      !>         LINE = 'Computing # bounds of plate set...'
      !>
      !>         CALL REPMC ( LINE, '#', CORNAM(CORSYS), LINE )
      !>         WRITE (*,*) LINE
      !>
      !>         CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES,
      !>     .                 CORSYS, CORPAR, MNCOR3, MXCOR3 )
      !>
      !>         WRITE (*,*) 'Done.'
      !>C
      !>C        Write the segment to the file.
      !>C
      !>         WRITE (*,*) 'Writing segment...'
      !>
      !>         CALL DSKW02 ( HANDLE,
      !>     .                 CENTER, SURFID, DCLASS, FRAME,  CORSYS,
      !>     .                 CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
      !>     .                 MNCOR3, MXCOR3, FIRST,  LAST,   NV,
      !>     .                 VRTCES, NP,     PLATES, SPAIXD, SPAIXI )
      !>
      !>         WRITE (*,*) 'Done.'
      !>
      !>      END DO
      !>C
      !>C     Segregate the data records in the DSK file and
      !>C     close the file.
      !>C
      !>      WRITE (*,*) 'Segregating and closing DSK file...'
      !>
      !>      CALL DSKCLS ( HANDLE, .TRUE. )
      !>
      !>      WRITE (*,*) 'Done.'
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Reading input data...
      !>         Done.
      !>         Creating segment            1
      !>         Creating spatial index...
      !>         Done.
      !>         Computing radius bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Creating segment            2
      !>         Creating spatial index...
      !>         Done.
      !>         Computing Z-coordinate bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Creating segment            3
      !>         Creating spatial index...
      !>         Done.
      !>         Computing altitude bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Segregating and closing DSK file...
      !>         Done.
      !>
      !>        Note that after run completion, a new DSK exists in the output
      !>        directory.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of arguments WORK, SPAIXD and SPAIXI in $Brief_I/O table.
      !>
      !>        Added solution to code example.
      !>
      !>-    SPICELIB Version 1.0.0, 13-DEC-2016 (NJB)
      !>
      !>        Updated check on NV.
      !>
      !>        16-MAR-2016 (NJB)
      !>
      !>           Now zeros out the size of the vertex-plate list
      !>           when the list is not created.
      !>
      !>        23-JAN-2016 (NJB)
      !>
      !>           Original version.
      module subroutine dskmi2 ( ctx, nv, vrtces, np, plates, finscl, corscl, worksz, voxpsz, voxlsz, makvtl, spxisz, work, spaixd, spaixi )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nv  !! Number of vertices.
      real(wp), intent(in) :: vrtces ( 3, * )  !! Vertices.
      integer(ip), intent(in) :: np  !! Number of plates.
      integer(ip), intent(in) :: plates ( 3, * )  !! Plates.
      real(wp), intent(in) :: finscl  !! Fine voxel scale.
      integer(ip), intent(in) :: corscl  !! Coarse voxel scale.
      integer(ip), intent(in) :: worksz  !! Workspace size.
      integer(ip), intent(in) :: voxpsz  !! Voxel-plate pointer array size.
      integer(ip), intent(in) :: voxlsz  !! Voxel-plate list array size.
      logical, intent(in) :: makvtl  !! Vertex-plate list flag.
      integer(ip), intent(in) :: spxisz  !! Spatial index integer component size.
      integer(ip), intent(inout) :: work   ( 2, worksz )  !! Workspace.
      real(wp), intent(out) :: spaixd ( ixdfix )  !! Double precision component of spatial index.
      integer(ip), intent(out) :: spaixi ( spxisz )  !! Integer component of spatial index.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: nshift
      integer(ip) :: nvxtot
      integer(ip) :: reqsiz
      integer(ip) :: vtlidx
      integer(ip) :: vtpidx
      integer(ip) :: vtxlsz
      integer(ip) :: vxlidx
      integer(ip) :: vxpidx

      if ( ctx%failed() ) return

!
!     Perform error checks on inputs.
!
      if (  finscl <= 0.0_wp ) then

         call ctx%setmsg ( 'Fine voxel scale = #; scale must ' &
         //            'be positive. Usually scale should ' &
         //            'be > 1.0.'                         )
         call ctx%errdp  ( '#',  finscl                        )
         call ctx%sigerr ( 'SPICE(BADFINEVOXELSCALE)'          )
         return

      end if

      if (  corscl < 1 ) then

         call ctx%setmsg ( 'Coarse voxel scale = #; scale must ' &
         //            'be >= 1.'                           )
         call ctx%errint ( '#',  corscl                         )
         call ctx%sigerr ( 'SPICE(BADCOARSEVOXSCALE)'           )
         return

      end if

      if (  ( nv < 3 ) .or. ( nv > maxvrt )  ) then

         call ctx%setmsg ( 'Vertex count NV = #; count must ' &
         //            'be in the range 3:#.'            )
         call ctx%errint ( '#',  nv                          )
         call ctx%errint ( '#',  maxvrt                      )
         call ctx%sigerr ( 'SPICE(BADVERTEXCOUNT)'           )
         return

      end if

      if (  ( np < 1 ) .or. ( np > maxplt )  ) then

         call ctx%setmsg ( 'Plate count NP = #; count must ' &
         //            'be in the range 1:#.'            )
         call ctx%errint ( '#',  np                          )
         call ctx%errint ( '#',  maxplt                      )
         call ctx%sigerr ( 'SPICE(BADPLATECOUNT)'            )
         return

      end if

      if ( worksz < (np+1)  ) then

         call ctx%setmsg ( 'Workspace size = #; size is too ' &
         //            'small to hold all voxel-plate ' &
         //            'associations. Size should be at ' &
         //            'least # * (average number of ' &
         //            'voxels intersected by each plate).' )
         call ctx%errint ( '#',  worksz                         )
         call ctx%errint ( '#',  np                             )
         call ctx%sigerr ( 'SPICE(WORKSPACETOOSMALL)'           )
         return

      end if

      if ( voxpsz < 1 ) then

         call ctx%setmsg ( 'Voxel-pointer array size = #; size is ' &
         //            'too small to hold all voxel-plate ' &
         //            'list pointers. Size should be at ' &
         //            'least # * (number of non-empty ' &
         //            'coarse voxels).'                      )
         call ctx%errint ( '#',  voxpsz                           )
         call ctx%errint ( '#',  corscl**3                        )
         call ctx%sigerr ( 'SPICE(PTRARRAYTOOSMALL)'              )
         return

      end if

      if ( voxlsz < (np+1)  ) then

         call ctx%setmsg ( 'Voxel-plate list array size = #; size ' &
         //            'is too small to hold all voxel-plate ' &
         //            'associations. Size should be at ' &
         //            'least # * (average number of ' &
         //            'voxels intersected by each plate).'   )
         call ctx%errint ( '#',  voxlsz                           )
         call ctx%errint ( '#',  np                               )
         call ctx%sigerr ( 'SPICE(PLATELISTTOOSMALL)'             )
         return

      end if

!
!     Check the size of the integer spatial index array. The
!     declared size must be large enough to hold:
!
!        - the fixed-size part of the index, which includes
!          the coarse voxel grid
!
!        - the voxel-plate pointer array
!
!        - the voxel-plate association list
!
!     plus, if the vertex-plate association list is constructed,
!
!        - the vertex-plate pointer array
!
!        - the vertex-plate association list
!
      reqsiz = ixifix + voxpsz + voxlsz

      if ( makvtl ) then
!
!        Add on the sizes of the vertex-plate pointer array (NV)
!        and the vertex-plate list array (NV + 3*NP).
!
         vtxlsz = nv  +  3*np

         reqsiz = reqsiz + nv + vtxlsz

      else
         vtxlsz = 0
      end if

      if ( spxisz < reqsiz ) then

         call ctx%setmsg ( 'Integer spatial index size = #; size ' &
         //            'must be at least #.'                   )
         call ctx%errint ( '#',  spxisz                            )
         call ctx%errint ( '#',  reqsiz                            )
         call ctx%sigerr ( 'SPICE(INTINDEXTOOSMALL)'               )
         return

      end if

!
!     Set known values in spatial index arrays.
!
      spaixi(sicgsc) = corscl

!
!     Prepare indices in the spatial index arrays.
!
!        VXPIDX is the start index of the voxel pointer array.
!
      vxpidx = sicgrd + maxcgr
!
!        VXLIDX is the start index of the voxel-plate list. This
!        list is offset from the start of the pointer array by
!        the input size given for that array. The size is the
!        total room available, not the room actually used.
!
      vxlidx = vxpidx + voxpsz

!
!     Create spatial index for plates.
!
      call zzmkspin ( ctx, np,      plates,  vrtces, &
                      finscl,  corscl,  voxpsz, &
                      worksz,  voxlsz,  work, &
                      spaixi(sivgrx), &
                      spaixd(sivxsz), &
                      spaixd(sivxor), &
                      nvxtot, &
                      spaixi(sivxnp), &
                      spaixi(vxpidx), &
                      spaixi(sivxnl), &
                      spaixi(vxlidx), &
                      spaixd(sivtbd), &
                      spaixi(sicgrd)           )

      if ( ctx%failed() ) return

!
!     At this point the voxel plate list is offset from the
!     start of the voxel pointer array by the allocated size
!     of the array. We need to shift the plate list so that
!     it starts right after the end of the pointer array.
!
      nshift = voxpsz - spaixi(sivxnp)

      do i = 1, spaixi(sivxnl)

         j                = vxlidx - 1 + i

         spaixi(j-nshift) = spaixi(j)

      end do

!
!     Update the voxel list start index to reflect the shift.
!
      vxlidx = vxlidx - nshift

!
!     Create vertex-plate mapping, if requested, as indicated
!     by the vertex-plate list size.
!
      if ( makvtl ) then
!
!           VTPIDX is the start index of the vertex pointer array.
!
         vtpidx = vxlidx + spaixi(sivxnl)
!
!           VXLIDX is the start index of the vertex-plate list. The
!           list start is offset from the vertex pointer array by
!           the size of the array, which is always NV.
!
         vtlidx = vtpidx + nv

         call zzvrtplt ( ctx, nv,     np,     plates, &
                         worksz, vtxlsz, work, &
                         spaixi(vtpidx), &
                         spaixi(sivtnl), &
                         spaixi(vtlidx)         )

      else
!
!        Zero out the size of the vertex-plate list.
!
         spaixi(sivtnl) = 0

      end if

      return
      end subroutine dskmi2

      !> Determine range bounds for a set of triangular plates to
      !> be stored in a type 2 DSK segment.
      !>
      !>## Detailed Input
      !>
      !>     NV       is the number of vertices belonging to the input
      !>              set of plates.
      !>
      !>     VRTCES   is an array of coordinates of the vertices. The Ith
      !>              vertex occupies elements (1:3,I) of this array.
      !>
      !>     NP       is the number of plates in the input plate set.
      !>
      !>     PLATES   is an array representing the triangular plates of a
      !>              shape model. The elements of PLATES are vertex
      !>              indices; vertex indices are 1-based. The vertex
      !>              indices of the Ith plate occupy elements (1:3,I) of
      !>              this array.
      !>
      !>     CORSYS   is an integer parameter identifying the coordinate
      !>              system in which the bounds are to be computed. The
      !>              bounds apply to the third coordinate in each system:
      !>
      !>                 Latitudinal:           radius
      !>                 Planetodetic:          altitude
      !>                 Rectangular:           Z
      !>
      !>     CORPAR   is an array of parameters associated with the
      !>                coordinate system. Currently the only supported system
      !>                that has associated parameters is the planetodetic
      !>                system. For planetodetic coordinates,
      !>
      !>                CORPAR(1) is the equatorial radius
      !>
      !>                CORPAR(2) is the flattening coefficient. Let RE and
      !>                RP represent, respectively, the equatorial and
      !>                polar radii of the reference ellipsoid of the
      !>                system. Then
      !>
      !>                    CORPAR(2) = ( RE - RP ) / RE
      !>
      !>## Detailed Output
      !>
      !>     MNCOR3   is a lower bound on the range of the third coordinate
      !>              of the system identified by CORSYS and CORPAR, taken
      !>              over all plates.
      !>
      !>              For latitudinal and rectangular coordinates, MNCOR3
      !>              is the greatest lower bound of the third coordinate.
      !>
      !>              For planetodetic coordinates, MNCOR3 is an
      !>              approximation: it is less than or equal to the greatest
      !>              lower bound.
      !>
      !>     MXCOR3   is the least upper bound on the range of the third
      !>              coordinate of the system identified by CORSYS and
      !>              CORPAR, taken over all plates.
      !>
      !>## Parameters
      !>
      !>     See the include file dskdsc.inc for declarations of the public DSK
      !>     type 2 parameters used by this routine.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input coordinate system is not recognized, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     2)  If a conversion from rectangular to planetodetic coordinates
      !>         fails, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Users planning to create DSK files should consider whether the
      !>     SPICE DSK creation utility MKDSK may be suitable for their needs.
      !>
      !>     This routine supports use of the DSK type 2 segment writer DSKW02
      !>     by computing bounds on the range of the third coordinates of
      !>     the input plate set.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a three-segment DSK file using plate model data for
      !>        Phobos. Use latitudinal, rectangular, and planetodetic
      !>        coordinates in the respective segments. This is not a
      !>        realistic example, but it serves to demonstrate use of
      !>        the supported coordinate systems.
      !>
      !>        Use the DSK kernel below to provide, for simplicity, the
      !>        input plate and vertex data. The selected input file has one
      !>        segment.
      !>
      !>           phobos_3_3.bds
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>C
      !>C     Example program for DSKW02, DSKMI2, and DSKRB2
      !>C
      !>C        Create a three-segment DSK file using plate model
      !>C        data for Phobos. Use latitudinal, rectangular, and
      !>C        planetodetic coordinates in the respective segments.
      !>C
      !>C        For simplicity, use an existing DSK file to provide
      !>C        the input plate and vertex data. The selected input
      !>C        file has one segment.
      !>C
      !>C           Version 1.0.0 22-JAN-2016 (NJB)
      !>C
      !>      PROGRAM DSKRB2_EX1
      !>      IMPLICIT NONE
      !>
      !>      INCLUDE 'dla.inc'
      !>      INCLUDE 'dskdsc.inc'
      !>      INCLUDE 'dsk02.inc'
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      JYEAR
      !>      DOUBLE PRECISION      PI
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               FRNMLN
      !>      PARAMETER           ( FRNMLN = 32 )
      !>
      !>      INTEGER               NSEG
      !>      PARAMETER           ( NSEG   = 3 )
      !>
      !>      INTEGER               NAMLEN
      !>      PARAMETER           ( NAMLEN = 20 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE = 80 )
      !>
      !>      INTEGER               NCOR
      !>      PARAMETER           ( NCOR   = 4 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(NAMLEN)    CORNAM ( NCOR )
      !>      CHARACTER*(FILSIZ)    DSK
      !>      CHARACTER*(FRNMLN)    FRAME
      !>      CHARACTER*(FILSIZ)    INDSK
      !>      CHARACTER*(LNSIZE)    LINE
      !>C
      !>C     Note: the values of MAXVRT and MAXPLT declared
      !>C     in dsk02.inc, and the integer spatial index
      !>C     dimension SPAISZ are very large. Smaller buffers
      !>C     can be used for most applications.
      !>C
      !>      DOUBLE PRECISION      CORPAR ( NSYPAR )
      !>      DOUBLE PRECISION      F
      !>      DOUBLE PRECISION      FINSCL
      !>      DOUBLE PRECISION      FIRST
      !>      DOUBLE PRECISION      LAST
      !>      DOUBLE PRECISION      MNCOR1
      !>      DOUBLE PRECISION      MNCOR2
      !>      DOUBLE PRECISION      MNCOR3
      !>      DOUBLE PRECISION      MXCOR1
      !>      DOUBLE PRECISION      MXCOR2
      !>      DOUBLE PRECISION      MXCOR3
      !>      DOUBLE PRECISION      RE
      !>      DOUBLE PRECISION      RP
      !>      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      !>      DOUBLE PRECISION      VRTCES ( 3, MAXVRT )
      !>
      !>      INTEGER               CENTER
      !>      INTEGER               CORSCL
      !>      INTEGER               CORSYS
      !>      INTEGER               DCLASS
      !>      INTEGER               DLADSC ( DLADSZ )
      !>      INTEGER               HANDLE
      !>      INTEGER               INHAN
      !>      INTEGER               NP
      !>      INTEGER               NV
      !>      INTEGER               PLATES ( 3, MAXPLT )
      !>      INTEGER               SEGNO
      !>      INTEGER               SPAIXI ( SPAISZ )
      !>      INTEGER               SURFID
      !>      INTEGER               VOXPSZ
      !>      INTEGER               VOXLSZ
      !>      INTEGER               WORK   ( 2, MAXCEL )
      !>      INTEGER               WORKSZ
      !>
      !>      LOGICAL               FOUND
      !>C
      !>C     Saved variables
      !>C
      !>C     Save all large arrays to avoid stack problems.
      !>C
      !>      SAVE
      !>C
      !>C     Initial values
      !>C
      !>      DATA                  CORNAM / 'radius',
      !>     .                               'Z-coordinate',
      !>     .                               'Z-coordinate',
      !>     .                               'altitude'     /
      !>
      !>C
      !>C     Assign names of input and output DSK files.
      !>C
      !>      INDSK = 'phobos_3_3.bds'
      !>      DSK   = 'phobos_3_3_3seg.bds'
      !>C
      !>C     Open input DSK for read access; find first segment.
      !>C
      !>      CALL DASOPR ( INDSK, INHAN )
      !>      CALL DLABFS ( INHAN, DLADSC, FOUND )
      !>C
      !>C     Fetch vertices and plates from input DSK file.
      !>C
      !>      WRITE (*,*) 'Reading input data...'
      !>
      !>      CALL DSKV02 ( INHAN, DLADSC, 1, MAXVRT, NV, VRTCES )
      !>      CALL DSKP02 ( INHAN, DLADSC, 1, MAXPLT, NP, PLATES )
      !>
      !>      WRITE (*,*) 'Done.'
      !>C
      !>C     Set input array sizes required by DSKMI2.
      !>C
      !>      VOXPSZ = MAXVXP
      !>      VOXLSZ = MXNVLS
      !>      WORKSZ = MAXCEL
      !>C
      !>C     Set fine and coarse voxel scales. (These usually
      !>C     need to determined by experimentation.)
      !>C
      !>      FINSCL = 5.D0
      !>      CORSCL = 4
      !>C
      !>C     Open a new DSK file.
      !>C
      !>      CALL DSKOPN ( DSK, DSK, 0, HANDLE )
      !>C
      !>C     Create three segments and add them to the file.
      !>C
      !>      DO SEGNO = 1, NSEG
      !>C
      !>C        Create spatial index.
      !>C
      !>         WRITE (*,*) 'Creating segment ', SEGNO
      !>         WRITE (*,*) 'Creating spatial index...'
      !>
      !>         CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
      !>     .                 CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE.,
      !>     .                 SPAISZ, WORK,   SPAIXD, SPAIXI        )
      !>
      !>         WRITE (*,*) 'Done.'
      !>C
      !>C        Set up inputs describing segment attributes:
      !>C
      !>C        - Central body: Phobos
      !>C        - Surface ID code: user's choice.
      !>C          We use the segment number here.
      !>C        - Data class: general (arbitrary) shape
      !>C        - Body-fixed reference frame
      !>C        - Time coverage bounds (TBD)
      !>C
      !>         CENTER = 401
      !>         SURFID = SEGNO
      !>         DCLASS = GENCLS
      !>         FRAME  = 'IAU_PHOBOS'
      !>
      !>         FIRST = -50 * JYEAR()
      !>         LAST  =  50 * JYEAR()
      !>C
      !>C        Set the coordinate system and coordinate system
      !>C        bounds based on the segment index.
      !>C
      !>C        Zero out the coordinate parameters to start.
      !>C
      !>         CALL CLEARD ( NSYPAR, CORPAR )
      !>
      !>         IF ( SEGNO .EQ. 1 ) THEN
      !>C
      !>C           Use planetocentric latitudinal coordinates. Set
      !>C           the longitude and latitude bounds.
      !>C
      !>            CORSYS = LATSYS
      !>
      !>            MNCOR1 = -PI()
      !>            MXCOR1 =  PI()
      !>            MNCOR2 = -PI()/2
      !>            MXCOR2 =  PI()/2
      !>
      !>         ELSE IF ( SEGNO .EQ. 2 ) THEN
      !>C
      !>C           Use rectangular coordinates. Set the
      !>C           X and Y bounds.
      !>C
      !>C           The bounds shown here were derived from
      !>C           the plate data. They lie slightly outside
      !>C           of the range spanned by the plates.
      !>C
      !>            CORSYS = RECSYS
      !>
      !>            MNCOR1 = -1.3D0
      !>            MXCOR1 =  1.31D0
      !>            MNCOR2 = -1.21D0
      !>            MXCOR2 =  1.2D0
      !>
      !>         ELSE
      !>C
      !>C           Set the coordinate system to planetodetic.
      !>C
      !>            CORSYS    = PDTSYS
      !>
      !>            MNCOR1    = -PI()
      !>            MXCOR1    =  PI()
      !>            MNCOR2    = -PI()/2
      !>            MXCOR2    =  PI()/2
      !>C
      !>C           We'll use equatorial and polar radii from
      !>C           pck00010.tpc. These normally would be fetched
      !>C           at run time, but for simplicity, we'll use
      !>C           hard-coded values.
      !>
      !>            RE        = 13.0D0
      !>            RP        =  9.1D0
      !>            F         = ( RE - RP ) / RE
      !>
      !>            CORPAR(1) = RE
      !>            CORPAR(2) = F
      !>
      !>         END IF
      !>C
      !>C        Compute plate model radius bounds.
      !>C
      !>         LINE = 'Computing # bounds of plate set...'
      !>
      !>         CALL REPMC ( LINE, '#', CORNAM(CORSYS), LINE )
      !>         WRITE (*,*) LINE
      !>
      !>         CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES,
      !>     .                 CORSYS, CORPAR, MNCOR3, MXCOR3 )
      !>
      !>         WRITE (*,*) 'Done.'
      !>C
      !>C        Write the segment to the file.
      !>C
      !>         WRITE (*,*) 'Writing segment...'
      !>
      !>         CALL DSKW02 ( HANDLE,
      !>     .                 CENTER, SURFID, DCLASS, FRAME,  CORSYS,
      !>     .                 CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
      !>     .                 MNCOR3, MXCOR3, FIRST,  LAST,   NV,
      !>     .                 VRTCES, NP,     PLATES, SPAIXD, SPAIXI )
      !>
      !>         WRITE (*,*) 'Done.'
      !>
      !>      END DO
      !>C
      !>C     Segregate the data records in the DSK file and
      !>C     close the file.
      !>C
      !>      WRITE (*,*) 'Segregating and closing DSK file...'
      !>
      !>      CALL DSKCLS ( HANDLE, .TRUE. )
      !>
      !>      WRITE (*,*) 'Done.'
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Reading input data...
      !>         Done.
      !>         Creating segment            1
      !>         Creating spatial index...
      !>         Done.
      !>         Computing radius bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Creating segment            2
      !>         Creating spatial index...
      !>         Done.
      !>         Computing Z-coordinate bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Creating segment            3
      !>         Creating spatial index...
      !>         Done.
      !>         Computing altitude bounds of plate set...
      !>         Done.
      !>         Writing segment...
      !>         Done.
      !>         Segregating and closing DSK file...
      !>         Done.
      !>
      !>        Note that after run completion, a new DSK exists in the output
      !>        directory.
      !>
      !>## Restrictions
      !>
      !>     1)  For planetodetic coordinates, the computation of the lower
      !>         altitude bound requires that the surface at altitude MNCOR3 be
      !>         convex. This is the case for realistic geometries, but can
      !>         be false if a plate is very large compared to the overall
      !>         shape model.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added solution to code example.
      !>
      !>-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
      !>
      !>        22-JAN-2016 (NJB)
      !>
      !>         Original version.
      module subroutine dskrb2 ( ctx, nv, vrtces, np, plates, corsys, corpar, mncor3, mxcor3 )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, dpmin
      use pepper_math, only: vdist, vnorm
      use pepper_inc_dskdsc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nv  !! Number of vertices.
      real(wp), intent(in) :: vrtces ( 3, * )  !! Vertices.
      integer(ip), intent(in) :: np  !! Number of plates.
      integer(ip), intent(in) :: plates ( 3, * )  !! Plates.
      integer(ip), intent(in) :: corsys  !! DSK coordinate system code.
      real(wp), intent(in) :: corpar ( * )  !! DSK coordinate system parameters.
      real(wp), intent(out) :: mncor3  !! Lower bound on range of third coordinate.
      real(wp), intent(out) :: mxcor3  !! Upper bound on range of third coordinate.

      real(wp), parameter :: third = 1.0_wp/3.0_wp
      real(wp) :: alt
      real(wp) :: center ( 3 )
      real(wp) :: dist
      real(wp) :: f
      real(wp) :: lat
      real(wp) :: lon
      real(wp) :: maxd
      real(wp), parameter :: origin ( 3 ) = &
         [ 0.0_wp, 0.0_wp, 0.0_wp ]
      real(wp) :: pnear  ( 3 )
      real(wp) :: re

      integer(ip) :: i

      if ( ctx%failed() ) return

      if ( corsys == latsys ) then
!
!        The coordinate system is latitudinal.
!
!        Compute radius bounds. Start with the maximum radius.
!        This is simply the maximum norm of the vertices.
!
         mxcor3 = 0.0_wp

         do i = 1, nv
            mxcor3 = max (  vnorm( vrtces(1,i) ),  mxcor3  )
         end do

!
!        Compute the minimum radius of the plate set.
!
         mncor3 = dpmax

         do i = 1, np

            call pltnp ( ctx, origin, &
                         vrtces( 1, plates(1,i) ), &
                         vrtces( 1, plates(2,i) ), &
                         vrtces( 1, plates(3,i) ), &
                         pnear, &
                         dist                     )

            mncor3 = min ( dist, mncor3 )

         end do

      else if ( corsys == recsys ) then
!
!        The coordinate system is rectangular. Compute the range
!        of Z-coordinates of the plates.
!
         mncor3 = dpmax
         mxcor3 = dpmin

         do i = 1, nv

            mncor3 = min( mncor3, vrtces(3,i) )
            mxcor3 = max( mxcor3, vrtces(3,i) )

         end do

      else if ( corsys == pdtsys ) then
!
!        The coordinate system is planetodetic. Compute the range
!        of altitudes of the plates.
!
         re     = corpar(1)
         f      = corpar(2)

         mxcor3 = dpmin
         mncor3 = dpmax

!
!        The maximum altitude is attained at a plate vertex.
!
         do i = 1, nv

            call recgeo ( ctx, vrtces(1,i), re, f, lon, lat, alt )

            if ( ctx%failed() ) return

            mxcor3 = max( mxcor3, alt )

         end do

!
!        For the Ith plate, let DMAX(I) be the maximum distance between
!        the plate's center and any of the plate's vertices.
!
!        The minimum altitude is greater than or equal to
!        the minimum of
!
!           {altitude of the Ith plate's center - DMAX(I)}
!
!        taken over all plates.
!
         do i = 1, np

            center = third*vrtces(:,plates(1,i)) + third*vrtces(:,plates(2,i)) + third*vrtces(:,plates(3,i))

            maxd = max (  vdist(  vrtces( 1, plates(1,i) ), center  ), &
                          vdist(  vrtces( 1, plates(2,i) ), center  ), &
                          vdist(  vrtces( 1, plates(3,i) ), center  )  )

            call recgeo ( ctx, center, re, f, lon, lat, alt )

            if ( ctx%failed() ) return

            mncor3 = min (  mncor3,  ( alt - maxd )  )

         end do

      else

         call ctx%setmsg ( 'Coordinate system # is not supported.' )
         call ctx%errint ( '#', corsys                             )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                   )
         return

      end if

      return
      end subroutine dskrb2

      !> Find the set of body ID codes of all objects for which
      !> topographic data are provided in a specified DSK file.
      !>
      !>## Detailed Input
      !>
      !>     DSKFNM   is the name of a DSK file. This file will be opened for
      !>              read access by this routine.
      !>
      !>     BODIDS   is an initialized SPICE set data structure.
      !>
      !>              BODIDS optionally may contain a set of body ID codes on
      !>              input; on output, the data already present in BODIDS will
      !>              be combined with ID code set found for the file DSKFNM.
      !>
      !>              If BODIDS contains no data on input, its size and
      !>              cardinality still must be initialized.
      !>
      !>## Detailed Output
      !>
      !>     BODIDS   is a SPICE set data structure that contains the union
      !>              of its contents upon input with the set of body ID codes
      !>              of segments in the indicated DSK file.
      !>
      !>              The elements of SPICE sets are unique; each ID code in
      !>              BODIDS appears only once, even if the DSK file contains
      !>              multiple segments for that ID code.
      !>
      !>              See the $Examples section below for a complete example
      !>              program showing how to retrieve the body and surface ID
      !>              codes from a DSK file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file has transfer format, the error
      !>         SPICE(INVALIDFORMAT) is signaled.
      !>
      !>     2)  If the input file is not a transfer file but has architecture
      !>         other than DAS, the error SPICE(INVALIDARCHTYPE) is signaled.
      !>
      !>     3)  If the input file is a binary DAS file of type other than DSK,
      !>         the error SPICE(INVALIDFILETYPE) is signaled.
      !>
      !>     4)  If the DSK file cannot be opened or read, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     5)  If the size of the output set argument BODIDS is insufficient
      !>         to contain the actual number of ID codes of objects covered by
      !>         the indicated DSK file, the error SPICE(CELLTOOSMALL) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See the description of the argument DSKFNM above.
      !>
      !>## Particulars
      !>
      !>     This routine provides an API via which applications can determine
      !>     the set of objects for which there are topographic data in a
      !>     specified DSK file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Display the coverage for each object in a specified DSK file.
      !>        Find the set of objects in the file. Loop over the contents
      !>        of the ID code set: find the surface ID for each item in the
      !>        set and display the surface ID.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>C
      !>C     Examine a DSK file and identify the set of
      !>C     central bodies associated with the segments
      !>C     in the file. For each body, find the
      !>C     set of surfaces associated with that body.
      !>C
      !>      PROGRAM DSKOBJ_EX1
      !>      IMPLICIT NONE
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               CARDI
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               MAXID
      !>      PARAMETER           ( MAXID  = 10000 )
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    DSKFNM
      !>
      !>      INTEGER               BODIDS ( LBCELL : MAXID )
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               SRFIDS ( LBCELL : MAXID )
      !>
      !>C
      !>C     Initialize body ID and surface ID cells.
      !>C
      !>      CALL SSIZEI ( MAXID, BODIDS )
      !>      CALL SSIZEI ( MAXID, SRFIDS )
      !>
      !>C
      !>C     Prompt for the name of a DSK file.
      !>C
      !>      CALL PROMPT ( 'Enter name of DSK file > ', DSKFNM )
      !>
      !>C
      !>C     Obtain body ID set for the DSK.
      !>C
      !>      CALL DSKOBJ ( DSKFNM, BODIDS )
      !>
      !>      DO I = 1, CARDI( BODIDS )
      !>
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Body ID:     ', BODIDS(I)
      !>C
      !>C        Get the surface IDs for the Ith body.
      !>C
      !>         CALL DSKSRF ( DSKFNM, BODIDS(I), SRFIDS )
      !>
      !>         DO J = 1, CARDI( SRFIDS )
      !>            WRITE (*,*) '   Surface ID: ', SRFIDS(J)
      !>         END DO
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter name of DSK file > phobos512.bds
      !>
      !>         Body ID:              401
      !>            Surface ID:          401
      !>
      !>## Restrictions
      !>
      !>     1)  If an error occurs while this routine is updating the set
      !>         BODIDS, the set may be corrupted.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Changed input argument name "DSK" to "DSKFNM" for consistency
      !>        with other routines.
      !>
      !>        Bug fix: added call to FAILED after call to GETFAT.
      !>
      !>        Edited the header comments to comply with NAIF standard.
      !>        Changed the DKS file used in the code example's solution.
      !>        Added NAIF_IDS to the $Required_Reading section.
      !>
      !>        Corrected short error message in entries #2, #3 and #5 in
      !>        $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.0, 22-AUG-2016 (NJB)
      module subroutine dskobj ( ctx, dskfnm, bodids )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: dskfnm  !! Name of DSK file.
      integer(ip), intent(inout) :: bodids ( lbcell : * )  !! Set of ID codes of objects in DSK
                                                           !! file.
      integer(ip), allocatable :: list(:)
      integer(ip) :: k
!
!     PEPPER: the IDs are found by DSK_OBJECTS; here they are added to
!     the set BODIDS.
!
      if ( ctx%failed() ) return
      call dsk_objects ( ctx, dskfnm, list )
      do k = 1, size ( list )
         if ( ctx%failed() ) return
         if ( elemi ( ctx, list(k), bodids ) ) cycle
         if ( cardi( ctx,bodids) == sizei( ctx,bodids) ) then
            call ctx%setmsg ( 'Cannot append body ID # to cell ' &
            //            'while reading DSK file #. ' &
            //            'Cell size is #.'                 )
            call ctx%errint ( '#', list(k)                      )
            call ctx%errch  ( '#', dskfnm                       )
            call ctx%errint ( '#', sizei( ctx, bodids )              )
            call ctx%sigerr ( 'SPICE(CELLTOOSMALL)'             )
            return
         end if
         call insrti ( ctx, list(k), bodids )
      end do
      end subroutine dskobj

      !> The bodies of a DSK file (PEPPER; [[dskobj]] without a set cell):
      !> their ID codes, sorted, without duplicates.
      module subroutine dsk_objects ( ctx, file, ids )
      use pepper_kinds, only: ip, wp
      use pepper_windows, only: append_id, sort_unique
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the DSK file
      integer(ip), allocatable, intent(out) :: ids(:)  !! the body ID codes (size 0 after an error)
      integer(ip), allocatable :: list(:)  ! the IDs found (unsorted, with duplicates)
      integer(ip) :: nlist  ! the number of IDs in LIST



      character(toklen) :: arch
      character(toklen) :: kertyp

      real(wp) :: dskdsc ( dskdsz )

      integer(ip) :: bid
      integer(ip) :: dladsc ( dladsz )
      integer(ip) :: handle
      integer(ip) :: nxtdsc ( dladsz )

      logical :: found

      allocate ( ids(0) )
      nlist = 0
      if ( ctx%failed() ) return

!
!     See whether GETFAT thinks we've got a DSK file.
!
      call getfat ( ctx, file, arch, kertyp )

      if ( ctx%failed() ) return

      if ( arch == 'XFR' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If the input file is an ' &
         //            'DSK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                      )
         return

      else if ( arch /= 'DAS' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. Binary DSK files have ' &
         //            'DAS architecture. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDARCHTYPE)'                    )
         return

      else if ( kertyp /= 'DSK' ) then

         call ctx%setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  kertyp                                )
         call ctx%sigerr ( 'SPICE(INVALIDFILETYPE)'                    )
         return

      end if

!
!     Open the DSK for read access; start a forward search.
!
      call dasopr ( ctx, file, handle )
      call dlabfs ( ctx, handle, nxtdsc, found )

      if ( ctx%failed() ) return

      do while (  found  .and.  ( .not. ctx%failed() )  )
!
!        Get the DSK descriptor of the current segment.
!        This is where we'll find the body ID code.
!
         dladsc = nxtdsc

         call dskgd ( ctx, handle, dladsc, dskdsc )
!
!        The body ID is at location CTRIDX ("center index")
!        of the DSK descriptor.
!
!        Append, rather than insert, the new ID (PEPPER: to an array
!        that grows; it is sorted at the end).
!
         bid = nint( dskdsc(ctridx) )

         call append_id ( list, nlist, bid )
!
!        Fetch the DLA descriptor of the next segment.
!
         call dlafns ( ctx, handle, dladsc, nxtdsc, found )

      end do

      call dascls ( ctx, handle )

      if ( .not. ctx%failed() ) then
         ids = list(1:nlist)
         call sort_unique ( ids )
      end if
      return
      end subroutine dsk_objects

      !> Find the set of surface ID codes for all surfaces associated with
      !> a given body in a specified DSK file.
      !>
      !>## Detailed Input
      !>
      !>     DSKFNM   is the name of a DSK file. This file will be opened for
      !>              read access by this routine.
      !>
      !>     BODYID   is the integer ID code of a body for which topographic
      !>              data are present in the specified DSK file.
      !>
      !>     SRFIDS   is an initialized SPICE set data structure.
      !>
      !>              SRFIDS optionally may contain a set of surface ID codes
      !>              on input; on output, the ID codes already present in
      !>              SRFIDS will be combined with surface ID code set found
      !>              for the body designated by BODYID in the file DSKFNM.
      !>
      !>              If SRFIDS contains no data on input, its size and
      !>              cardinality still must be initialized.
      !>
      !>## Detailed Output
      !>
      !>     SRFIDS   is a SPICE set data structure that contains the union
      !>              of its contents upon input with the set of ID codes of
      !>              the surfaces associated with the body designated by
      !>              BODYID, for which segments were found in the indicated
      !>              DSK file.
      !>
      !>              The elements of SPICE sets are unique; each ID code in
      !>              SRFIDS appears only once, even if the DSK file contains
      !>              multiple segments for that ID code.
      !>
      !>              See the $Examples section below for a complete example
      !>              program showing how to retrieve body and surface ID codes
      !>              from a DSK file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input file has transfer format, the error
      !>         SPICE(INVALIDFORMAT) is signaled.
      !>
      !>     2)  If the input file is not a transfer file but has architecture
      !>         other than DAS, the error SPICE(INVALIDARCHTYPE) is signaled.
      !>
      !>     3)  If the input file is a binary DAS file of type other than DSK,
      !>         the error SPICE(INVALIDFILETYPE) is signaled.
      !>
      !>     4)  If the DSK file cannot be opened or read, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     5)  If the size of the output set argument SRFIDS is insufficient
      !>         to contain the actual number of ID codes of surfaces covered
      !>         by the indicated DSK file, the error SPICE(CELLTOOSMALL) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See the description of the argument DSKFNM above.
      !>
      !>## Particulars
      !>
      !>     This routine provides an API via which applications can determine
      !>     the set of surfaces associated with a given body in a specified
      !>     DSK file. This routine is normally used together with DSKOBJ.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Examine a DSK file and identify the set of central bodies
      !>        associated with the segments in the file. For each body, find
      !>        the set of surfaces associated with that body.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>C
      !>C     Examine a DSK file and identify the set of
      !>C     central bodies associated with the segments
      !>C     in the file. For each body, find the
      !>C     set of surfaces associated with that body.
      !>C
      !>      PROGRAM DSKSRF_EX1
      !>      IMPLICIT NONE
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               CARDI
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               MAXID
      !>      PARAMETER           ( MAXID  = 10000 )
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    DSKFNM
      !>
      !>      INTEGER               BODIDS ( LBCELL : MAXID )
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               SRFIDS ( LBCELL : MAXID )
      !>
      !>C
      !>C     Initialize body ID and surface ID cells.
      !>C
      !>      CALL SSIZEI ( MAXID, BODIDS )
      !>      CALL SSIZEI ( MAXID, SRFIDS )
      !>
      !>C
      !>C     Prompt for the name of a DSK file.
      !>C
      !>      CALL PROMPT ( 'Enter name of DSK file > ', DSKFNM )
      !>
      !>C
      !>C     Obtain body ID set for the DSK.
      !>C
      !>      CALL DSKOBJ ( DSKFNM, BODIDS )
      !>
      !>      DO I = 1, CARDI( BODIDS )
      !>
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Body ID:     ', BODIDS(I)
      !>C
      !>C        Get the surface IDs for the Ith body.
      !>C
      !>         CALL DSKSRF ( DSKFNM, BODIDS(I), SRFIDS )
      !>
      !>         DO J = 1, CARDI( SRFIDS )
      !>            WRITE (*,*) '   Surface ID: ', SRFIDS(J)
      !>         END DO
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the DSK file named phobos512.bds, the output
      !>        was:
      !>
      !>        Enter name of DSK file > phobos512.bds
      !>
      !>         Body ID:              401
      !>            Surface ID:          401
      !>
      !>## Restrictions
      !>
      !>     1)  If an error occurs while this routine is updating the set
      !>         SRFIDS, the set may be corrupted.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Changed input argument name "DSK" to "DSKFNM" for consistency
      !>        with other routines.
      !>
      !>        Bug fix: added call to FAILED after call to GETFAT.
      !>
      !>        Edited the header to comply with NAIF standard. Update problem
      !>        statement in $Examples section. Corrected short error message
      !>        in entries #2, #3 and #5 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.0, 22-AUG-2016 (NJB)
      module subroutine dsksrf ( ctx, dskfnm, bodyid, srfids )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: dskfnm  !! Name of DSK file.
      integer(ip), intent(in) :: bodyid  !! Integer body ID code.
      integer(ip), intent(inout) :: srfids ( lbcell : * )  !! Set of ID codes of surfaces in DSK
                                                           !! file.
      integer(ip), allocatable :: list(:)
      integer(ip) :: k
!
!     PEPPER: the IDs are found by DSK_SURFACES; here they are added to
!     the set SRFIDS.
!
      if ( ctx%failed() ) return
      call dsk_surfaces ( ctx, dskfnm, bodyid, list )
      do k = 1, size ( list )
         if ( ctx%failed() ) return
         if ( elemi ( ctx, list(k), srfids ) ) cycle
         if ( cardi( ctx,srfids) == sizei( ctx,srfids) ) then
            call ctx%setmsg ( 'Cannot append surface ID # to cell ' &
            //            'while reading DSK file #. ' &
            //            'Cell size is #.'                 )
            call ctx%errint ( '#', list(k)                      )
            call ctx%errch  ( '#', dskfnm                       )
            call ctx%errint ( '#', sizei( ctx, srfids )              )
            call ctx%sigerr ( 'SPICE(CELLTOOSMALL)'             )
            return
         end if
         call insrti ( ctx, list(k), srfids )
      end do
      end subroutine dsksrf

      !> The surfaces of a body in a DSK file (PEPPER; [[dsksrf]] without a
      !> set cell): their ID codes, sorted, without duplicates.
      module subroutine dsk_surfaces ( ctx, file, body, ids )
      use pepper_kinds, only: ip, wp
      use pepper_windows, only: append_id, sort_unique
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the DSK file
      integer(ip), intent(in) :: body  !! the body ID code
      integer(ip), allocatable, intent(out) :: ids(:)  !! the surface ID codes (size 0 after an error)
      integer(ip), allocatable :: list(:)  ! the IDs found (unsorted, with duplicates)
      integer(ip) :: nlist  ! the number of IDs in LIST



      character(toklen) :: arch
      character(toklen) :: kertyp

      real(wp) :: dskdsc ( dskdsz )

      integer(ip) :: bid
      integer(ip) :: dladsc ( dladsz )
      integer(ip) :: handle
      integer(ip) :: nxtdsc ( dladsz )
      integer(ip) :: sid

      logical :: found

      allocate ( ids(0) )
      nlist = 0
      if ( ctx%failed() ) return

!
!     See whether GETFAT thinks we've got a DSK file.
!
      call getfat ( ctx, file, arch, kertyp )

      if ( ctx%failed() ) return

      if ( arch == 'XFR' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If the input file is an ' &
         //            'DSK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                      )
         return

      else if ( arch /= 'DAS' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. Binary DSK files have ' &
         //            'DAS architecture. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDARCHTYPE)'                    )
         return

      else if ( kertyp /= 'DSK' ) then

         call ctx%setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  kertyp                                )
         call ctx%sigerr ( 'SPICE(INVALIDFILETYPE)'                    )
         return

      end if

!
!     Open the DSK for read access; start a forward search.
!
      call dasopr ( ctx, file, handle )
      call dlabfs ( ctx, handle, nxtdsc, found )

      if ( ctx%failed() ) return

      do while (  found  .and.  ( .not. ctx%failed() )  )
!
!        Get the DSK descriptor of the current segment.
!        This is where we'll find the body ID code.
!
         dladsc = nxtdsc

         call dskgd ( ctx, handle, dladsc, dskdsc )
!
!        The body ID is at location CTRIDX ("center index")
!        of the DSK descriptor.
!
         bid = nint( dskdsc(ctridx) )

         if ( bid == body ) then

            sid = nint( dskdsc(srfidx) )
!
!           Append, rather than insert, the new ID (PEPPER: to an array that
!           grows; it is sorted at the end).
!

            call append_id ( list, nlist, sid )

         end if
!
!        Fetch the DLA descriptor of the next segment.
!
         call dlafns ( ctx, handle, dladsc, nxtdsc, found )

      end do

      call dascls ( ctx, handle )

      if ( .not. ctx%failed() ) then
         ids = list(1:nlist)
         call sort_unique ( ids )
      end if
      return
      end subroutine dsk_surfaces

end submodule pepper_dsk_files
