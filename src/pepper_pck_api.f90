!> PCK coverage and frames ([[pckcov]], [[pckfrm]]), and Euler angles of bodies ([[bodeul]], [[pckeul]]).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_pck_api

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: lnsize = 80  !! length of a line of text (messages, kernel lines)
   integer(ip), parameter :: nd = 2  !! number of double precision components of a DAF array summary (SPK, CK, PCK: 2)
   integer(ip), parameter :: ni = 6  !! number of integer components of a DAF array summary (SPK, CK: 6)

contains

      !> Find the coverage window for a specified reference frame in a
      !> specified binary PCK file.
      !>
      !>## Detailed Input
      !>
      !>     PCKFNM   is the name of a binary PCK file.
      !>
      !>     IDCODE   is the integer frame class ID code of a PCK reference
      !>              frame for which data are expected to exist in the
      !>              specified PCK file.
      !>
      !>     COVER    is an initialized SPICE window data structure. COVER
      !>              optionally may contain coverage data on input; on output,
      !>              the data already present in COVER will be combined with
      !>              coverage found for the reference frame designated by
      !>              IDCODE in the file PCKFNM.
      !>
      !>              If COVER contains no data on input, its size and
      !>              cardinality still must be initialized.
      !>
      !>## Detailed Output
      !>
      !>     COVER    is a SPICE window data structure which represents the
      !>              merged coverage for the reference frame having frame
      !>              class ID IDCODE. This is the set of time intervals for
      !>              which data for IDCODE are present in the file PCKFNM,
      !>              merged with the set of time intervals present in COVER on
      !>              input. The merged coverage is represented as the union of
      !>              one or more disjoint time intervals. The window COVER
      !>              contains the pairs of endpoints of these intervals.
      !>
      !>              The interval endpoints contained in COVER are ephemeris
      !>              times, expressed as seconds past J2000 TDB.
      !>
      !>              See the $Examples section below for a complete example
      !>              program showing how to retrieve the endpoints from COVER.
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
      !>         other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled.
      !>
      !>     3)  If the input file is a binary DAF file of type other than PCK,
      !>         the error SPICE(INVALIDFILETYPE) is signaled.
      !>
      !>     4)  If the PCK file cannot be opened or read, an error is signaled
      !>         by a routine in the call tree of this routine. The output
      !>         window will not be modified.
      !>
      !>     5)  If the size of the output window argument COVER is
      !>         insufficient to contain the actual number of intervals in the
      !>         coverage window for IDCODE, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     This routine reads a PCK file.
      !>
      !>## Particulars
      !>
      !>     This routine provides an API via which applications can determine
      !>     the coverage a specified PCK file provides for a specified PCK
      !>     class reference frame.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) This example demonstrates combined usage of PCKCOV and the
      !>        related PCK utility PCKFRM.
      !>
      !>        Display the coverage for each object in a specified PCK file.
      !>        Find the set of objects in the file; for each object, find
      !>        and display the coverage.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PCKCOV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               WNCARD
      !>      INTEGER               CARDI
      !>C
      !>C     Local parameters
      !>C
      !>C
      !>C     Declare the coverage window.  Make enough room
      !>C     for MAXIV intervals.
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               MAXIV
      !>      PARAMETER           ( MAXIV  = 1000 )
      !>
      !>      INTEGER               WINSIZ
      !>      PARAMETER           ( WINSIZ = 2 * MAXIV )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 50 )
      !>
      !>      INTEGER               MAXFRM
      !>      PARAMETER           ( MAXFRM = 1000 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    LSK
      !>      CHARACTER*(FILSIZ)    PCKFNM
      !>      CHARACTER*(TIMLEN)    TIMSTR
      !>
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
      !>      DOUBLE PRECISION      E
      !>
      !>      INTEGER               I
      !>      INTEGER               IDS   ( LBCELL : MAXFRM )
      !>      INTEGER               J
      !>      INTEGER               NIV
      !>
      !>C
      !>C     Load a leapseconds kernel for output time conversion.
      !>C     PCKCOV itself does not require a leapseconds kernel.
      !>C
      !>      CALL PROMPT ( 'Name of leapseconds kernel > ', LSK )
      !>      CALL FURNSH ( LSK )
      !>
      !>C
      !>C     Get name of PCK file.
      !>C
      !>      CALL PROMPT ( 'Name of PCK file           > ', PCKFNM )
      !>
      !>C
      !>C     Initialize the set IDS.
      !>C
      !>      CALL SSIZEI ( MAXFRM, IDS )
      !>
      !>C
      !>C     Initialize the window COVER.
      !>C
      !>      CALL SSIZED ( WINSIZ, COVER )
      !>
      !>C
      !>C     Find the set of frames in the PCK file.
      !>C
      !>      CALL PCKFRM ( PCKFNM, IDS )
      !>
      !>C
      !>C     We want to display the coverage for each frame.  Loop
      !>C     over the contents of the ID code set, find the coverage
      !>C     for each item in the set, and display the coverage.
      !>C
      !>      DO I = 1, CARDI( IDS )
      !>
      !>C
      !>C        Find the coverage window for the current frame.
      !>C        Empty the coverage window each time so
      !>C        we don't include data for the previous frame.
      !>C
      !>         CALL SCARDD ( 0,      COVER )
      !>         CALL PCKCOV ( PCKFNM, IDS(I), COVER )
      !>
      !>C
      !>C        Get the number of intervals in the coverage
      !>C        window.
      !>C
      !>         NIV = WNCARD( COVER )
      !>
      !>C
      !>C        Display a simple banner.
      !>C
      !>         WRITE (*,*) '========================================'
      !>         WRITE (*,*) 'Coverage for reference frame ', IDS(I)
      !>
      !>C
      !>C        Convert the coverage interval start and stop
      !>C        times to TDB calendar strings.
      !>C
      !>         DO J = 1, NIV
      !>
      !>C
      !>C           Get the endpoints of the Jth interval.
      !>C
      !>            CALL WNFETD ( COVER, J, B, E )
      !>
      !>C
      !>C           Convert the endpoints to TDB calendar
      !>C           format time strings and display them.
      !>C
      !>            CALL TIMOUT ( B,
      !>     .                    'YYYY MON DD HR:MN:SC.### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                        )
      !>            WRITE (*,*) ' '
      !>            WRITE (*,*) 'Interval: ', J
      !>            WRITE (*,*) 'Start:    ', TIMSTR
      !>
      !>            CALL TIMOUT ( E,
      !>     .                    'YYYY MON DD HR:MN:SC.### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                        )
      !>            WRITE (*,*) 'Stop:     ', TIMSTR
      !>            WRITE (*,*) ' '
      !>
      !>         END DO
      !>
      !>         WRITE (*,*) '========================================'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the LSK file named naif0012.tls, and the PCK
      !>        file named earth_720101_070426.bpc, the output was:
      !>
      !>        Name of leapseconds kernel > naif0012.tls
      !>        Name of PCK file           > earth_720101_070426.bpc
      !>         ========================================
      !>         Coverage for reference frame         3000
      !>
      !>         Interval:            1
      !>         Start:    1962 JAN 20 00:00:41.184 (TDB)
      !>         Stop:     2007 APR 26 00:01:05.185 (TDB)
      !>
      !>         ========================================
      !>
      !>     2) Find the coverage for the frame designated by IDCODE
      !>        provided by the set of PCK files loaded via a metakernel.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: pckcov_ex2.tm
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
      !>              earth_720101_070426.bpc          Earth historical
      !>                                               binary PCK
      !>              earth_070425_370426_predict.bpc  Earth predicted
      !>                                               binary PCK
      !>              naif0012.tls                     Leapseconds
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = ( 'earth_070425_370426_predict.bpc',
      !>                               'earth_720101_070426.bpc',
      !>                               'naif0012.tls'                    )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PCKCOV_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               LNSIZE
      !>      PARAMETER           ( LNSIZE = 80 )
      !>
      !>      INTEGER               MAXCOV
      !>      PARAMETER           ( MAXCOV = 100000 )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 50 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FILE
      !>      CHARACTER*(LNSIZE)    IDCH
      !>      CHARACTER*(FILSIZ)    META
      !>      CHARACTER*(FILSIZ)    SOURCE
      !>      CHARACTER*(TIMLEN)    TIMSTR
      !>      CHARACTER*(LNSIZE)    TYPE
      !>
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      COVER  ( LBCELL : 2*MAXCOV )
      !>      DOUBLE PRECISION      E
      !>
      !>      INTEGER               COUNT
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               IDCODE
      !>      INTEGER               NIV
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the metakernel name; load the metakernel.
      !>C     The metakernel lists the PCK files whose coverage
      !>C     for IDCODE we'd like to determine.  The metakernel
      !>C     must also specify a leapseconds kernel.
      !>C
      !>      CALL PROMPT ( 'Enter name of metakernel > ', META )
      !>
      !>      CALL FURNSH ( META )
      !>
      !>C
      !>C     Get the ID code of interest.
      !>C
      !>      CALL PROMPT ( 'Enter ID code            > ', IDCH )
      !>
      !>      CALL PRSINT ( IDCH,  IDCODE )
      !>
      !>C
      !>C     Initialize the coverage window.
      !>C
      !>      CALL SSIZED ( MAXCOV, COVER )
      !>
      !>C
      !>C     Find out how many kernels are loaded.  Loop over the
      !>C     kernels:  for each loaded PCK file, add its coverage
      !>C     for IDCODE, if any, to the coverage window.
      !>C
      !>      CALL KTOTAL ( 'PCK', COUNT )
      !>
      !>      DO I = 1, COUNT
      !>
      !>         CALL KDATA  ( I,       'PCK',   FILE,  TYPE,
      !>     .                 SOURCE,  HANDLE,  FOUND       )
      !>
      !>         CALL PCKCOV ( FILE,    IDCODE,  COVER )
      !>
      !>      END DO
      !>
      !>C
      !>C     Display results.
      !>C
      !>C     Get the number of intervals in the coverage
      !>C     window.
      !>C
      !>      NIV = WNCARD( COVER )
      !>
      !>C
      !>C     Display a simple banner.
      !>C
      !>      WRITE (*,*) ' '
      !>      WRITE (*,*) 'Coverage for frame ', IDCODE
      !>
      !>C
      !>C     Convert the coverage interval start and stop
      !>C     times to TDB calendar strings.
      !>C
      !>      DO I = 1, NIV
      !>
      !>C
      !>C        Get the endpoints of the Ith interval.
      !>C
      !>         CALL WNFETD ( COVER, I, B, E )
      !>
      !>C
      !>C        Convert the endpoints to TDB calendar
      !>C        format time strings and display them.
      !>C
      !>         CALL TIMOUT ( B,
      !>     .                 'YYYY MON DD HR:MN:SC.### ' //
      !>     .                 '(TDB) ::TDB',
      !>     .                 TIMSTR                        )
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Interval: ', I
      !>         WRITE (*,*) 'Start:    ', TIMSTR
      !>
      !>         CALL TIMOUT ( E,
      !>     .                 'YYYY MON DD HR:MN:SC.### ' //
      !>     .                 '(TDB) ::TDB',
      !>     .                 TIMSTR                        )
      !>         WRITE (*,*) 'Stop:     ', TIMSTR
      !>         WRITE (*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the meta-kernel named pckcov_ex2.tm provided
      !>        above to find the coverage window for the ITRF93 frame using
      !>        its ID code, '3000', the output was:
      !>
      !>        Enter name of metakernel > pckcov_ex2.tm
      !>        Enter ID code            > 3000
      !>
      !>         Coverage for frame         3000
      !>
      !>         Interval:            1
      !>         Start:    1962 JAN 20 00:00:41.184 (TDB)
      !>         Stop:     2037 JUL 17 00:01:05.183 (TDB)
      !>
      !>## Restrictions
      !>
      !>     1)  If an error occurs while this routine is updating the window
      !>         COVER, the window may be corrupted.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Changed input argument name "PCK" to "PCKFNM" for consistency
      !>        with other routines.
      !>
      !>        Bug fix: added call to FAILED after call to GETFAT.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added examples' solution.
      !>
      !>        Corrected short error message in entries #2 and #3 in
      !>        $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.1, 03-JAN-2014 (NJB) (EDW)
      !>
      !>        Updated index entries.
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>
      !>-    SPICELIB Version 1.0.0, 30-NOV-2007 (NJB)
      module subroutine pckcov ( ctx, pckfnm, idcode, cover )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: pckfnm  !! Name of PCK file.
      integer(ip), intent(in) :: idcode  !! Class ID code of PCK reference frame.
      real(wp), intent(inout) :: cover  ( lbcell : * )  !! Window giving coverage in PCKFNM for IDCODE.
      real(wp), allocatable :: intervals(:,:)
      integer(ip) :: k
!
!     PEPPER: the coverage is found by PCK_COVERAGE; here it is added
!     to COVER (the union of the two).
!
      if ( ctx%failed() ) return
      call pck_coverage ( ctx, pckfnm, idcode, intervals )
      do k = 1, size ( intervals, 2 )
         if ( ctx%failed() ) return
         call wninsd ( ctx, intervals(1,k), intervals(2,k), cover )
      end do
      end subroutine pckcov

      !> The coverage of a frame in a binary PCK file (PEPPER; [[pckcov]]
      !> without a window cell).
      !>
      !> The result is a window as an array: each column an interval
      !> [start, stop] (TDB seconds past J2000), sorted and disjoint
      !> ([[pepper_windows]]). The errors are those of [[pckcov]],
      !> without SPICE(WINDOWEXCESS).
      module subroutine pck_coverage ( ctx, file, frame, intervals )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the binary PCK file
      integer(ip), intent(in) :: frame  !! the frame class ID code
      real(wp), allocatable, intent(out) :: intervals(:,:)  !! the coverage, shape (2, n) (n = 0 after an error)
      type(interval_list) :: window  ! the coverage being built





      character(lnsize) :: arch
      character(lnsize) :: kertyp

      real(wp) :: dc     ( nd )
      real(wp) :: descr  ( nd + ni/2 )

      integer(ip) :: handle
      integer(ip) :: ic     ( ni )

      logical :: found

!
!     Standard SPICE error handling.
!
      allocate ( intervals(2,0) )
      if ( ctx%failed() ) return

!
!     See whether GETFAT thinks we've got a binary PCK file.
!     If not, indicate the specific problem.
!
      call getfat ( ctx, file, arch, kertyp )

      if ( ctx%failed() ) return

      if ( arch == 'XFR' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary PCK file to be readable ' &
         //            'by this routine.  If the input file is an ' &
         //            'PCK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                      )
         return

      else if ( arch /= 'DAF' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary PCK file to be readable ' &
         //            'by this routine.  Binary PCK files have ' &
         //            'DAF architecture.  If you expected the ' &
         //            'file to be a binary PCK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDARCHTYPE)'                    )
         return

      else if ( kertyp /= 'PCK' ) then

         call ctx%setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary PCK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary PCK file, the problem ' &
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
!     Open the file for reading.
!
      call dafopr ( ctx, file, handle )

      if ( ctx%failed() ) return

!
!     We will examine each segment descriptor in the file, and
!     we'll update our coverage bounds according to the data found
!     in these descriptors.
!
!     Start a forward search.
!
      call dafbfs ( ctx, handle )

!
!     Find the next DAF array.
!
      call daffna ( ctx, found )

      do while (  found  .and.  ( .not. ctx%failed() )  )
!
!        Fetch and unpack the segment descriptor.
!
         call dafgs ( ctx, descr )
         call dafus ( ctx, descr, nd, ni, dc, ic )

         if ( ic(1) == frame ) then
!
!           This segment is for the body of interest.  Insert the
!           coverage bounds into the coverage window.
!
            call insert_interval ( ctx, dc(1), dc(2), window )

         end if

         call daffna ( ctx, found )

      end do

!
!     Release the file.
!
      call dafcls ( ctx, handle )

      if ( .not. ctx%failed() ) call window%window ( intervals )
      return
      end subroutine pck_coverage

      !> This routine is obsolete. It supports only the type 02 binary
      !> PCK format. It is maintained only for backward compatibility
      !>
      !> Return Euler angles and their derivatives and their reference
      !> frame, given an input time and body and reference frame from
      !> a PCK binary file.
      !>
      !>## Detailed Input
      !>
      !>     BODY     is the integer ID code of the body for which the
      !>              state transformation matrix is requested. Bodies
      !>              are numbered according to the standard NAIF
      !>              numbering scheme. The numbering scheme is
      !>              explained in the NAIF_IDS required reading file.
      !>
      !>     ET       is the epoch at which the state transformation
      !>              matrix is requested.
      !>
      !>## Detailed Output
      !>
      !>     FOUND    if the Euler angles for the requested time
      !>              and body are found in a PCK binary file,
      !>              FOUND is .TRUE. Otherwise, it's false.
      !>
      !>     REF      is the name of an inertial ref. frame.
      !>              (See the routine CHGIRF for a full list of names.)
      !>
      !>     EULANG   is the Euler angles and their derivatives at
      !>              time ET. The rotation matrix is
      !>              [ EULANG(3) ]  [EULANG(2)] [EULANG(1)]
      !>                           3            1           3
      !>
      !>              and   dEULANG(1)/dt = EULANG(4)
      !>                    dEULANG(2)/dt = EULANG(5)
      !>                    dEULANG(3)/dt = EULANG(6)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     None.
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
      !>     Here we load a binary PCK files and use PCKEUL to get the
      !>     Euler angles.
      !>
      !>     C
      !>     C  Load binary PCK file.
      !>     C
      !>        CALL PCKLOF ('example.pck', HANDLE)
      !>
      !>     C  Call routine to get Euler angles phi, delta, w.
      !>
      !>        CALL PCKEUL ( BODY, ET, FOUND, REF, EULANG )
      !>
      !>     The Euler angles and their derivatives are returned
      !>     in EULANG.
      !>
      !>## Restrictions
      !>
      !>     1)  A binary PCK kernel must be loaded with PCKLOF before
      !>         calling this routine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 2.0.0, 21-MAR-1995 (KSZ)
      !>
      !>        PCKEUL modified to check in. PCKMAT takes
      !>        over for PCKEUL in many cases. REF now a character.
      !>
      !>-    SPICELIB Version 1.1.0, 18-OCT-1994 (KSZ)
      !>
      !>        Fixed bug which incorrectly modded DW by two pi.
      !>
      !>-    SPICELIB Version 1.0.0, 11-MAR-1994 (KSZ)
      module subroutine pckeul ( ctx, body, et, found, ref, eulang )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! ID code of body
      real(wp), intent(in) :: et  !! Epoch of transformation
      logical, intent(out) :: found  !! .TRUE. if ET, BODY found in a PCK file
      character(*), intent(out) :: ref  !! Name of inertial ref. frame of state
      real(wp), intent(out) :: eulang   ( 6 )  !! Euler angles and their derivatives.

!
!     Parameters
!
!     ND    number of double precision components of descriptor
!     NI    number of integer components of descriptor
!     NR    component number of reference frame in integer
!           portion of descriptor
!     NS    size of a packed PCK segment descriptor
!     NT    component number of data type in integer portion
!           of descriptor
!

      integer(ip), parameter :: ni = 5

      integer(ip), parameter :: nr = 2

      integer(ip), parameter :: ns = 5

      integer(ip), parameter :: nt = 3


      character(40) :: ident

      real(wp) :: dcd     ( nd )
      real(wp) :: descr   ( ns )
      real(wp) :: record  ( 130 )

      integer(ip) :: type
      integer(ip) :: handle
      integer(ip) :: icd ( ni )
      integer(ip) :: iref

!
!     Standard SPICE Error handling.
!
      if ( ctx%failed() ) return

!
!     Get a segment applicable to a specified body and epoch.
!
      call pcksfs ( ctx, body, et, handle, descr, ident, found )

      if ( found ) then

!
!        Look at parts of the descriptor.
!
         call dafus ( ctx, descr, nd, ni, dcd, icd )
         type   = icd( nt )
         iref   = icd( nr )
         call irfnam ( ctx, iref, ref )

         if ( type == 2 ) then

!
!           Read in Chebyshev coefficients from segment.
!
            call pckr02 ( ctx, handle, descr, et, record )

!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            call pcke02 ( ctx, et, record, eulang )

         else
!
!           If appropriate data was not found, found is false.
!
            found = .false.

         end if

      end if

      return
      end subroutine pckeul

      !> Find the set of reference frame class ID codes of all frames
      !> in a specified binary PCK file.
      !>
      !>## Detailed Input
      !>
      !>     PCKFNM   is the name of a binary PCK file.
      !>
      !>     IDS      is an initialized SPICE set data structure. IDS
      !>              optionally may contain a set of ID codes on input; on
      !>              output, the data already present in IDS will be combined
      !>              with ID code set found for the file PCKFNM.
      !>
      !>              If IDS contains no data on input, its size and
      !>              cardinality still must be initialized.
      !>
      !>## Detailed Output
      !>
      !>     IDS      is a SPICE set data structure which contains the union
      !>              of its contents upon input with the set of reference
      !>              frame class ID codes of each frame for which data are
      !>              present in the indicated PCK file. The elements of
      !>              SPICE sets are unique; hence each ID code in IDS
      !>              appears only once, even if the PCK file contains multiple
      !>              segments for that ID code.
      !>
      !>              See the $Examples section below for a complete example
      !>              program showing how to retrieve the ID codes from IDS.
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
      !>         other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled.
      !>
      !>     3)  If the input file is a binary DAF file of type other than PCK,
      !>         the error SPICE(INVALIDFILETYPE) is signaled.
      !>
      !>     4)  If the PCK file cannot be opened or read, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     5)  If the size of the output set argument IDS is insufficient to
      !>         contain the actual number of ID codes of frames covered by the
      !>         indicated PCK file, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides an API via which applications can determine
      !>     the set of reference frames for which there are data in a
      !>     specified PCK file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Display the coverage for each frame in a specified PCK file.
      !>        Find the set of frames in the file. Loop over the contents
      !>        of the ID code set: find the coverage for each item in the
      !>        set and display the coverage.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PCKFRM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               WNCARD
      !>      INTEGER               CARDI
      !>C
      !>C     Local parameters
      !>C
      !>C
      !>C     Declare the coverage window.  Make enough room
      !>C     for MAXIV intervals.
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               MAXIV
      !>      PARAMETER           ( MAXIV  = 1000 )
      !>
      !>      INTEGER               WINSIZ
      !>      PARAMETER           ( WINSIZ = 2 * MAXIV )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 50 )
      !>
      !>      INTEGER               MAXFRM
      !>      PARAMETER           ( MAXFRM = 1000 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    LSK
      !>      CHARACTER*(FILSIZ)    PCKFNM
      !>      CHARACTER*(TIMLEN)    TIMSTR
      !>
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
      !>      DOUBLE PRECISION      E
      !>
      !>      INTEGER               I
      !>      INTEGER               IDS   ( LBCELL : MAXFRM )
      !>      INTEGER               J
      !>      INTEGER               NIV
      !>
      !>C
      !>C     Load a leapseconds kernel for output time conversion.
      !>C     PCKCOV itself does not require a leapseconds kernel.
      !>C
      !>      CALL PROMPT ( 'Name of leapseconds kernel > ', LSK )
      !>      CALL FURNSH ( LSK )
      !>
      !>C
      !>C     Get name of PCK file.
      !>C
      !>      CALL PROMPT ( 'Name of PCK file           > ', PCKFNM )
      !>
      !>C
      !>C     Initialize the set IDS.
      !>C
      !>      CALL SSIZEI ( MAXFRM, IDS )
      !>
      !>C
      !>C     Initialize the window COVER.
      !>C
      !>      CALL SSIZED ( WINSIZ, COVER )
      !>
      !>C
      !>C     Find the set of frames in the PCK file.
      !>C
      !>      CALL PCKFRM ( PCKFNM, IDS )
      !>
      !>C
      !>C     We want to display the coverage for each frame.  Loop
      !>C     over the contents of the ID code set, find the coverage
      !>C     for each item in the set, and display the coverage.
      !>C
      !>      DO I = 1, CARDI( IDS )
      !>C
      !>C        Find the coverage window for the current frame.
      !>C        Empty the coverage window each time so
      !>C        we don't include data for the previous frame.
      !>C
      !>         CALL SCARDD ( 0,      COVER )
      !>         CALL PCKCOV ( PCKFNM, IDS(I), COVER )
      !>
      !>C
      !>C        Get the number of intervals in the coverage
      !>C        window.
      !>C
      !>         NIV = WNCARD( COVER )
      !>
      !>C
      !>C        Display a simple banner.
      !>C
      !>         WRITE (*,*) '========================================'
      !>         WRITE (*,*) 'Coverage for reference frame ', IDS(I)
      !>
      !>C
      !>C        Convert the coverage interval start and stop
      !>C        times to TDB calendar strings.
      !>C
      !>         DO J = 1, NIV
      !>C
      !>C           Get the endpoints of the Jth interval.
      !>C
      !>            CALL WNFETD ( COVER, J, B, E )
      !>C
      !>C           Convert the endpoints to TDB calendar
      !>C           format time strings and display them.
      !>C
      !>            CALL TIMOUT ( B,
      !>     .                    'YYYY MON DD HR:MN:SC.### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                        )
      !>            WRITE (*,*) ' '
      !>            WRITE (*,*) 'Interval: ', J
      !>            WRITE (*,*) 'Start:    ', TIMSTR
      !>
      !>            CALL TIMOUT ( E,
      !>     .                    'YYYY MON DD HR:MN:SC.### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                        )
      !>            WRITE (*,*) 'Stop:     ', TIMSTR
      !>            WRITE (*,*) ' '
      !>
      !>         END DO
      !>
      !>         WRITE (*,*) '========================================'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the LSK file named naif0012.tls, and the PCK
      !>        file named earth_720101_070426.bpc, the output was:
      !>
      !>        Name of leapseconds kernel > naif0012.tls
      !>        Name of PCK file           > earth_720101_070426.bpc
      !>         ========================================
      !>         Coverage for reference frame         3000
      !>
      !>         Interval:            1
      !>         Start:    1962 JAN 20 00:00:41.184 (TDB)
      !>         Stop:     2007 APR 26 00:01:05.185 (TDB)
      !>
      !>         ========================================
      !>
      !>## Restrictions
      !>
      !>     1)  If an error occurs while this routine is updating the set
      !>         IDS, the set may be corrupted.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Changed input argument name "PCK" to "PCKFNM" for consistency
      !>        with other routines.
      !>
      !>        Bug fix: added call to FAILED after call to GETFAT.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added example's solution.
      !>
      !>        Corrected short error message in entries #2 and #3 in
      !>        $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-2007 (NJB)
      module subroutine pckfrm ( ctx, pckfnm, ids )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: pckfnm  !! Name of PCK file.
      integer(ip), intent(inout) :: ids    ( lbcell : * )  !! Set of frame class ID codes of frames in PCK file.
      integer(ip), allocatable :: list(:)
      integer(ip) :: k
!
!     PEPPER: the IDs are found by PCK_FRAMES; here they are added to
!     the set IDS.
!
      if ( ctx%failed() ) return
      call pck_frames ( ctx, pckfnm, list )
      do k = 1, size ( list )
         if ( ctx%failed() ) return
         call insrti ( ctx, list(k), ids )
      end do
      end subroutine pckfrm

      !> The frames of a binary PCK file (PEPPER; [[pckfrm]] without a set
      !> cell): their class ID codes, sorted, without duplicates.
      module subroutine pck_frames ( ctx, file, ids )
      use pepper_kinds, only: ip, wp
      use pepper_windows, only: append_id, sort_unique
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the binary PCK file
      integer(ip), allocatable, intent(out) :: ids(:)  !! the frame class ID codes (size 0 after an error)
      integer(ip), allocatable :: list(:)  ! the IDs found (unsorted, with duplicates)
      integer(ip) :: nlist  ! the number of IDs in LIST





      character(lnsize) :: arch
      character(lnsize) :: kertyp

      real(wp) :: dc     ( nd )
      real(wp) :: descr  ( nd + ni/2 )

      integer(ip) :: handle
      integer(ip) :: ic     ( ni )

      logical :: found

!
!     Standard SPICE error handling.
!
      allocate ( ids(0) )
      nlist = 0
      if ( ctx%failed() ) return

!
!     See whether GETFAT thinks we've got a PCK file.
!
      call getfat ( ctx, file, arch, kertyp )

      if ( ctx%failed() ) return

      if ( arch == 'XFR' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary PCK file to be readable ' &
         //            'by this routine.  If the input file is an ' &
         //            'PCK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                      )
         return

      else if ( arch /= 'DAF' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary PCK file to be readable ' &
         //            'by this routine.  Binary PCK files have ' &
         //            'DAF architecture.  If you expected the ' &
         //            'file to be a binary PCK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDARCHTYPE)'                    )
         return

      else if ( kertyp /= 'PCK' ) then

         call ctx%setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary PCK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary PCK file, the problem ' &
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
!     Open the file for reading.
!
      call dafopr ( ctx, file, handle )

      if ( ctx%failed() ) return

!
!     We will examine each segment descriptor in the file, and
!     we'll update our ID code set according to the data found
!     in these descriptors.
!
!     Start a forward search.
!
      call dafbfs ( ctx, handle )

!
!     Find the next DAF array.
!
      call daffna ( ctx, found )

      do while (  found  .and.  ( .not. ctx%failed() )  )
!
!        Fetch and unpack the segment descriptor.
!
         call dafgs ( ctx, descr )
         call dafus ( ctx, descr, nd, ni, dc, ic )

!
!        Insert the current ID code into the output set.
!        The insertion algorithm will handle duplicates; no special
!        action is required here.
!
         call append_id ( list, nlist, ic(1) )

         call daffna ( ctx, found )

      end do

!
!     Release the file.
!
      call dafcls ( ctx, handle )

      if ( .not. ctx%failed() ) then
         ids = list(1:nlist)
         call sort_unique ( ids )
      end if
      return
      end subroutine pck_frames

      !> Return the Euler angles needed to compute the transformation from
      !> inertial to body-fixed coordinates for any body in the kernel
      !> pool.
      !>
      !>## Detailed Input
      !>
      !>     BODY     is the integer ID code of the body for which the
      !>              transformation is requested. Bodies are numbered
      !>              according to the standard NAIF numbering scheme.
      !>
      !>     ET       is the epoch at which the transformation is
      !>              requested.
      !>
      !>## Detailed Output
      !>
      !>     RA,
      !>     DEC      are the right ascension and declination of the
      !>              (IAU) north pole of the body at the epoch of
      !>              transformation. RA and DEC are given in radians.
      !>
      !>     W        is the angle between the ascending node of the
      !>              body-fixed equatorial plane on the inertial
      !>              equatorial plane and the prime meridian of the body.
      !>              The node is the cross product of the inertial
      !>              frame's Z-axis with the Z-axis of the body-fixed
      !>              frame. The angle is measured in the positive
      !>              (counterclockwise) sense about the body-fixed
      !>              Z-axis, from the node to the prime meridian. W is
      !>              given in radians.
      !>
      !>     LAMBDA   is the angle between the prime meridian and the
      !>              longest axis of the tri-axial ellipsoid which
      !>              models the body. LAMBDA is given in radians.
      !>              See the $Particulars section below for further
      !>              discussion.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the PCK keywords required to compute the angles are
      !>         not available in the kernel pool, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If the number of phase terms is insufficient, the error
      !>         SPICE(INSUFFICIENTANGLES) is signaled.
      !>
      !>     3)  If, for a given body, both forms of the kernel variable names
      !>
      !>            BODY<body ID>_CONSTANTS_JED_EPOCH
      !>            BODY<body ID>_CONSTS_JED_EPOCH
      !>
      !>         are found in the kernel pool, the error
      !>         SPICE(COMPETINGEPOCHSPEC) is signaled. This is done
      !>         regardless of whether the values assigned to the kernel
      !>         variable names match.
      !>
      !>     4)  If, for a given body, both forms of the kernel variable names
      !>
      !>            BODY<body ID>_CONSTANTS_REF_FRAME
      !>            BODY<body ID>_CONSTS_REF_FRAME
      !>
      !>         are found in the kernel pool, the error
      !>         SPICE(COMPETINGFRAMESPEC) is signaled. This is done
      !>         regardless of whether the values assigned to the kernel
      !>         variable names match.
      !>
      !>     5)  If the central body associated with the input BODY, whether
      !>         a system barycenter or BODY itself, has associated phase
      !>         angles (aka nutation precession angles), and the kernel
      !>         variable BODY<body ID>_MAX_PHASE_DEGREE for the central
      !>         body is present but has a value outside the range 1:3,
      !>         the error SPICE(DEGREEOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     A text or binary PCK containing orientation data for the
      !>     body designated by BODY must be loaded at the time this
      !>     routine is called.
      !>
      !>     Normally PCK files are loaded during program initialization;
      !>     they need not be re-loaded prior to each call to this routine.
      !>
      !>## Particulars
      !>
      !>     Applications that need to compute the transformation between
      !>     body-fixed and inertial frames usually can call the higher-level
      !>     routine PXFORM instead of this routine.
      !>
      !>     If there exists high-precision binary PCK kernel information for
      !>     the body at the requested time, the angles, W, DELTA and PHI are
      !>     computed directly from that file. These angles are then used to
      !>     compute RA, DEC and W. The most recently loaded binary PCK file
      !>     has first priority followed by previously loaded binary PCK files
      !>     in backward time order. If no binary PCK file has been loaded,
      !>     the text P_constants kernel file (PCK) is used.
      !>
      !>     If there is only text PCK kernel information, it is expressed in
      !>     terms of RA, DEC and W (same W as above), where
      !>
      !>        RA    = PHI - HALFPI()
      !>        DEC   = HALFPI() - DELTA
      !>
      !>     RA, DEC, and W are defined as follows in the text PCK file:
      !>
      !>        RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta
      !>                                           i          i
      !>
      !>        DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta
      !>                                           i          i
      !>
      !>        W   = W0   + W1*d   + W2*d*d    + w  sin theta
      !>                                           i          i
      !>
      !>     where:
      !>
      !>        d = days past J2000.
      !>
      !>        T = Julian centuries past J2000.
      !>
      !>        a , d , and w  arrays apply to satellites only.
      !>         i   i       i
      !>
      !>        theta  = THETA0 * THETA1*T are specific to each planet.
      !>             i
      !>
      !>       These angles -- typically nodal rates -- vary in number and
      !>       definition from one planetary system to the next.
      !>
      !>     The prime meridian offset LAMBDA
      !>     ================================
      !>
      !>     The offset LAMBDA is the value specified by the kernel variable
      !>
      !>        BODYnnn_LONG_AXIS
      !>
      !>     if such a variable is defined.
      !>
      !>     The offset LAMBDA is a constant for a given body. LAMBDA serves
      !>     to distinguish between the planetocentric prime meridian, which
      !>     is provided in the PCK file, and the meridian that passes through
      !>     the +X axis of a reference frame aligned with the axes of the
      !>     body's reference ellipsoid.
      !>
      !>     However, SPICE Toolkit makes no use of LAMBDA. In order to
      !>     perform geometry computations using a reference ellipsoid not
      !>     aligned with a body's planetocentric reference frame, a
      !>     fixed-offset (aka "TK") reference frame aligned with the
      !>     ellipsoid's axes should be specified in a frames kernel. Note
      !>     that a fixed-offset frame may be rotated from the planetocentric
      !>     frame about an arbitrary axis, not just the polar axis.
      !>
      !>     See the Frames Required Reading frames.req for details on
      !>     constructing a fixed-offset frame specification.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, BODEUL is used to get the unit
      !>     vector (POLE) parallel to the north pole of a target body (BODY)
      !>     at a specific epoch (ET).
      !>
      !>        CALL BODEUL ( BODY, ET, RA, DEC, W, LAMBDA )
      !>        CALL RADREC ( 1.D0, RA,  DEC, POLE )
      !>
      !>     Note that the items necessary to compute the Euler angles
      !>     must have been loaded into the kernel pool (by one or more
      !>     previous calls to LDPOOL).
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 14-APR-2021 (NJB) (JDR)
      !>
      !>        The routine was updated to support user-defined maximum phase
      !>        angle degrees. The additional text kernel kernel variable name
      !>        BODYnnn_MAX_PHASE_DEGREE must be used when the phase angle
      !>        polynomials have degree higher than 1. The maximum allowed
      !>        degree is 3.
      !>
      !>        The kernel variable names
      !>
      !>           BODY#_CONSTS_REF_FRAME
      !>           BODY#_CONSTS_JED_EPOCH
      !>
      !>        are now recognized.
      !>
      !>        Error handling was upgraded to check FAILED() between kernel
      !>        data lookups and computations.
      !>
      !>        Now SAVEs all local variables.
      !>
      !>        Edited the header to comply with NAIF standard. Moved NAIF_IDS
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 4.2.0, 02-MAR-2016 (BVS)
      !>
      !>        BUG FIX: changed available room in the BODVCD call
      !>        fetching 'NUT_PREC_ANGLES' from MAXANG to MAXANG*2.
      !>
      !>        Fixed indention in some header sections.
      !>
      !>        Removed BODEUL: prefix from the text of the long
      !>        error for insufficient angles.
      !>
      !>     Last update was 24-APR-2014 (NJB)
      !>
      !>        Corrected the brief and detailed descriptions of W.
      !>
      !>-    SPICELIB Version 4.1.0, 24-OCT-2005 (NJB)
      !>
      !>        Calls to ZZBODVCD have been replaced with calls to
      !>        BODVCD.
      !>
      !>-    SPICELIB Version 4.0.0, 13-FEB-2004 (NJB)
      !>
      !>        Code has been updated to support satellite ID codes in the
      !>        range 10000 to 99999 and to allow nutation precession angles
      !>        to be associated with any object.
      !>
      !>        Implementation changes were made to improve robustness
      !>        of the code.
      !>
      !>-    SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ)
      !>
      !>        REF frame is now passed correctly as a character string.
      !>
      !>-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
      !>
      !>        Ability to get Euler angles from binary PCK file added.
      !>        This uses the new routine PCKEUL.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
      !>
      !>        Updated to handle P_constants referenced to different epochs
      !>        and inertial reference frames.
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
      !>
      !>        Allowed number of nutation precession angles increased to
      !>        100.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.1.0, 24-OCT-2005 (NJB)
      !>
      !>        Calls to ZZBODVCD have been replaced with calls to
      !>        BODVCD.
      !>
      !>-    SPICELIB Version 4.0.0, 13-FEB-2004 (NJB)
      !>
      !>        Code has been updated to support satellite ID codes in the
      !>        range 10000 to 99999 and to allow nutation precession angles
      !>        to be associated with any object.
      !>
      !>        Calls to deprecated kernel pool access routine RTPOOL
      !>        were replaced by calls to GDPOOL.
      !>
      !>        Calls to BODVAR have been replaced with calls to
      !>        ZZBODVCD.
      !>
      !>-    SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ)
      !>
      !>        REF frame is now passed correctly as a character string.
      !>
      !>-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
      !>
      !>        BODEUL now uses new software to check for the
      !>        existence of binary PCK files, search the for
      !>        data corresponding to the requested body and time,
      !>        and return the appropriate Euler angles, using the
      !>        new routine PCKEUL. Otherwise the code calculates
      !>        the Euler angles from the P_constants kernel file.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
      !>
      !>        Updated to handle P_constants referenced to different epochs
      !>        and inertial reference frames.
      !>
      !>        Updated to handle P_constants referenced to different epochs
      !>        and inertial reference frames.
      !>
      !>        BODEUL now checks the kernel pool for presence of the
      !>        variables
      !>
      !>           BODY#_CONSTANTS_REF_FRAME
      !>
      !>        and
      !>
      !>           BODY#_CONSTANTS_JED_EPOCH
      !>
      !>        where # is the NAIF integer code of the barycenter of a
      !>        planetary system or of a body other than a planet or
      !>        satellite. If either or both of these variables are
      !>        present, the P_constants for BODY are presumed to be
      !>        referenced to the specified inertial frame or epoch.
      !>        If the epoch of the constants is not J2000, the input
      !>        time ET is converted to seconds past the reference epoch.
      !>        If the frame of the constants is not J2000, the Euler angles
      !>        defining the rotation from the P_constants' frame to
      !>        body-fixed coordinates are transformed so that they define
      !>        the rotation from J2000 coordinates to body-fixed
      !>        coordinates.
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990  (NJB)
      !>
      !>        Allowed number of nutation precession angles increased to
      !>        100.
      !>
      !>-    Beta Version 2.0.0, 23-JUN-1989 (HAN)
      !>
      !>        Mod angles by two pi. Check to see that right ascension and
      !>        prime meridian angles are within the range 0 to two pi.
      !>
      !>        LAMBDA used to be returned in degrees. It has been corrected
      !>        to return LAMBDA in radians.
      !>
      !>-    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
      !>
      !>        $Examples section completed. Declarations of unused variables
      !>        HALFPI and N removed.
      module subroutine bodeul ( ctx, body, et, ra, dec, w, lambda )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: halfpi, j2000, rpd, spd, twopi
      use pepper_math, only: mxm, vdotg
      use pepper_strings, only: repmi_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! ID code of body.
      real(wp), intent(in) :: et  !! Epoch of transformation.
      real(wp), intent(out) :: ra  !! Right ascension of the (IAU) north pole.
      real(wp), intent(out) :: dec  !! Declination of the (IAU) north pole of the body.
      real(wp), intent(out) :: w  !! Prime meridian rotation angle.
      real(wp), intent(out) :: lambda  !! Angle between the prime meridian and longitude of longest axis.
      ! PEPPER: work arrays of the kernel variables' sizes (SPICELIB: saved
      ! arrays of MAXANG, 200, values)
!
!     Local variables
!
      real(wp), allocatable :: ac(:), dc(:), wc(:), tcoef(:), sinth(:), costh(:)
      real(wp) :: dpval_1(1)  !! one-element copy of dpval (array argument)
      integer(ip) :: ref_1(1)  !! one-element copy of ref (array argument)
      real(wp) :: conepc_1(1)  !! one-element copy of conepc (array argument)
      real(wp) :: lambda_1(1)  !! one-element copy of lambda (array argument)
!
!     Maximum number of coefficients per phase angle polynomial.
!
      integer(ip), parameter :: mxphas = 4
      integer(ip), parameter :: mxpoly = 3
      integer(ip), parameter :: namlen = 32
      real(wp) :: eulang (   6    )
      real(wp) :: dcoef  ( 0:mxpoly-1 )
      real(wp) :: phi
      integer(ip) :: nl
      real(wp) :: rf2bfx ( 3, 3     )
      integer(ip) :: na
      integer(ip) :: refid
      character(1) :: dtype
      real(wp) :: t
      character(namlen) :: item2
      integer(ip) :: ntheta
      character(namlen) :: bref
      integer(ip) :: ref
      integer(ip) :: j
      logical :: found
      real(wp) :: epoch
      integer(ip) :: k
      logical :: found2
      integer(ip) :: m
      integer(ip) :: i
      real(wp) :: theta
      integer(ip) :: nw
      real(wp) :: d
      integer(ip) :: deg
      real(wp) :: j2bfx  ( 3, 3   )
      integer(ip) :: dim
      character(namlen) :: item
      real(wp) :: j2ref  ( 3, 3   )
      real(wp) :: delta
      integer(ip) :: nd
      real(wp) :: wcoef  ( 0:mxpoly-1 )
      real(wp) :: dpval
      real(wp) :: conepc
      real(wp) :: rcoef  ( 0:mxpoly-1 )
      if ( .not. allocated ( ctx%bodeul_state ) ) allocate ( ctx%bodeul_state )
      associate ( j2code => ctx%bodeul_state%j2code, &
                  nphase => ctx%bodeul_state%nphase, &
                  nphsco => ctx%bodeul_state%nphsco, &
                  first => ctx%bodeul_state%first )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the code for the J2000 frame, if we don't have it yet.
!
      if ( first ) then

         call irfnum ( ctx, 'J2000', j2code )
         first = .false.

      end if

!
!     Get Euler angles from high precision data file.
!
      call pckeul ( ctx, body, et, found, bref, eulang )

      if ( ctx%failed() ) return

      if ( found ) then

         phi    = eulang(1)
         delta  = eulang(2)
         w      = eulang(3)

         call irfnum ( ctx, bref, ref )

         if ( ctx%failed() ) return

!
!        The offset of the prime meridian is optional.
!
         item = 'LONG_AXIS'

         if ( bodfnd ( ctx, body, item ) ) then

            call bodvcd ( ctx, body, item, 1, nl, lambda_1 )
            lambda = lambda_1(1)
            lambda = lambda * rpd
            lambda = mod ( lambda, twopi )

         else
            lambda = 0
         end if

      else
!
!        Find the body code used to label the reference frame and epoch
!        specifiers for the orientation constants for BODY.
!
!        For planetary systems, the reference frame and epoch for the
!        orientation constants is associated with the system
!        barycenter, not with individual bodies in the system.  For any
!        other bodies, (the Sun or asteroids, for example) the body's
!        own code is used as the label.
!
         refid = zzbodbry ( ctx, body )

!
!        Look up the epoch of the constants. The epoch is specified
!        as a Julian ephemeris date. The epoch defaults to J2000.
!
!        Look for both forms of the JED epoch kernel variable. At
!        most one is allowed to be present.
!
         item   =  'BODY#_CONSTANTS_JED_EPOCH'
         call repmi_inplace ( item, '#', refid )

         item2  =  'BODY#_CONSTS_JED_EPOCH'
         call repmi_inplace ( item2, '#', refid )

         call gdpool ( ctx, item, 1, 1, dim, conepc_1, found )
         conepc = conepc_1(1)

         if ( .not. found ) then

            call gdpool ( ctx, item2, 1, 1, dim, conepc_1, found2 )
            conepc = conepc_1(1)

            if ( .not. found2 ) then
               conepc = j2000
            end if

         else
!
!           Check for presence of both forms of the variable names.
!
            call dtpool ( ctx, item2, found2, dim, dtype )

            if ( found2 ) then

               call ctx%setmsg ( 'Both kernel variables # and # are ' &
               //            'present in the kernel pool. At most ' &
               //            'one form of the kernel variable name ' &
               //            'may be present.'           )
               call ctx%errch  ( '#', item                   )
               call ctx%errch  ( '#', item2                  )
               call ctx%sigerr ( 'SPICE(COMPETINGEPOCHSPEC)' )
               return

            end if

         end if

         if ( ctx%failed() ) return

         if ( found .or. found2 ) then
!
!           The reference epoch is returned as a JED.  Convert to
!           ephemeris seconds past J2000.  Then convert the input ET to
!           seconds past the reference epoch.
!
            conepc  =  spd * ( conepc - j2000 )
            epoch   =  et    -   conepc

         else
            epoch   =  et
         end if

!
!        Look up the reference frame of the constants.  The reference
!        frame is specified by a code recognized by CHGIRF.  The
!        default frame is J2000, symbolized by the code J2CODE.
!
         call irfnum ( ctx, 'J2000', j2code )

         item   =  'BODY#_CONSTANTS_REF_FRAME'
         call repmi_inplace ( item, '#', refid )

         item2  =  'BODY#_CONSTS_REF_FRAME'
         call repmi_inplace ( item2, '#', refid )

         call repmi_inplace ( item, '#', refid )
         call gipool ( ctx, item, 1, 1, dim, ref_1, found )
         ref = ref_1(1)

         if ( .not. found ) then

            call gipool ( ctx, item2, 1, 1, dim, ref_1, found2 )
            ref = ref_1(1)

            if ( .not. found2 ) then
               ref  =  j2code
            end if

         else
!
!           Check for presence of both forms of the variable names.
!
            call dtpool ( ctx, item2, found2, dim, dtype )

            if ( found2 ) then

               call ctx%setmsg ( 'Both kernel variables # and # are ' &
               //            'present in the kernel pool. At most ' &
               //            'one form of the kernel variable name ' &
               //            'may be present.'           )
               call ctx%errch  ( '#', item                   )
               call ctx%errch  ( '#', item2                  )
               call ctx%sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return

            end if

         end if

!
!        Whatever the body, it has quadratic time polynomials for
!        the RA and Dec of the pole, and for the rotation of the
!        Prime Meridian.
!
         item = 'POLE_RA'
         rcoef(0:0+mxpoly-1) = 0.0_wp
         call bodvcd ( ctx, body, item, mxpoly, na, rcoef )

         item = 'POLE_DEC'
         dcoef(0:0+mxpoly-1) = 0.0_wp
         call bodvcd ( ctx, body, item, mxpoly, nd, dcoef )

         item = 'PM'
         wcoef(0:0+mxpoly-1) = 0.0_wp
         call bodvcd ( ctx, body, item, mxpoly, nw, wcoef )

!
!        The offset of the prime meridian is optional.
!
         item = 'LONG_AXIS'

         if ( bodfnd ( ctx, body, item ) ) then
            call bodvcd ( ctx, body, item, 1, nl, lambda_1 )
            lambda = lambda_1(1)
         else
            lambda = 0
         end if

         if ( ctx%failed() ) return

!
!        There may be additional nutation and libration (THETA) terms.
!
         ntheta = 0
         nphase = 0  ! PEPPER: SPICELIB keeps the last body's count (as TISBOD does not)
         na     = 0
         nd     = 0
         nw     = 0

         allocate ( tcoef(max(1_ip, body_item_size(ctx, refid, 'NUT_PREC_ANGLES'))), source=0.0_wp )
         allocate ( ac(max(1_ip, body_item_size(ctx, body, 'NUT_PREC_RA'))), source=0.0_wp )
         allocate ( dc(max(1_ip, body_item_size(ctx, body, 'NUT_PREC_DEC'))), source=0.0_wp )
         allocate ( wc(max(1_ip, body_item_size(ctx, body, 'NUT_PREC_PM'))), source=0.0_wp )
         item = 'NUT_PREC_ANGLES'

         if ( bodfnd ( ctx, refid, item ) ) then
!
!           Find out whether the maximum phase angle degree has been
!           explicitly set.
!
            item2 = 'MAX_PHASE_DEGREE'

            if ( bodfnd( ctx,refid, item2) ) then

               call bodvcd ( ctx, refid, item2, 1, dim, dpval_1 )
               dpval = dpval_1(1)

               deg = nint( dpval )

               if ( ( deg < 1 ) .or. ( deg > mxphas-1 ) ) then

                  call ctx%setmsg ( 'Maximum phase angle degree for ' &
                  //            'body # must be in the range 1:# ' &
                  //            'but was #.'                      )
                  call ctx%errint ( '#', refid                        )
                  call ctx%errint ( '#', mxphas-1                     )
                  call ctx%errint ( '#', deg                          )
                  call ctx%sigerr ( 'SPICE(DEGREEOUTOFRANGE)'         )
                  return

               end if

               nphsco = deg + 1

            else
!
!              The default degree is 1, yielding two coefficients.
!
               nphsco = 2

            end if
!
!           There is something a bit obscure going on below. BODVCD
!           loads the array TCOEF in the following order
!
!              TCOEF(1,1), TCOEF(2,1), ... TCOEF(NPHSCO),
!              TCOEF(1,2), TCOEF(2,2), ...
!
!           The NTHETA that comes back is the total number of items
!           loaded, but we will need the actual limit on the second
!           dimension. That is --- NTHETA / NPHSCO.
!
            call bodvcd ( ctx, refid, item, size(tcoef, kind=ip), ntheta, tcoef )

            if ( ctx%failed() ) return
!
!           NPHSCO is at least 1; this division is safe.
!
            nphase = ntheta / nphsco

         end if

         item = 'NUT_PREC_RA'

         if ( bodfnd ( ctx, body, item ) ) then
            call bodvcd ( ctx, body, item, size(ac, kind=ip), na, ac )
         end if

         item = 'NUT_PREC_DEC'

         if ( bodfnd ( ctx, body, item ) ) then
            call bodvcd ( ctx, body, item, size(dc, kind=ip), nd, dc )
         end if

         item = 'NUT_PREC_PM'

         if ( bodfnd ( ctx, body, item ) ) then
            call bodvcd ( ctx, body, item, size(wc, kind=ip), nw, wc )
         end if

         if ( ctx%failed() ) return

         if ( max ( na, nd, nw ) > ntheta ) then
            call ctx%setmsg ( 'Insufficient number of nutation/'// &
                          'precession angles for body * at time #.' )
            call ctx%errint ( '*', body )
            call ctx%errdp  ( '#', et   )
            call ctx%sigerr ( 'SPICE(INSUFFICIENTANGLES)' )
            return
         end if

!
!        Evaluate the time polynomials at EPOCH.
!
         d   = epoch / spd
         t   = d     / 36525.0_wp

         ra  = rcoef(0)  +  t * ( rcoef(1) + t * rcoef(2) )
         dec = dcoef(0)  +  t * ( dcoef(1) + t * dcoef(2) )
         w   = wcoef(0)  +  d * ( wcoef(1) + d * wcoef(2) )

!
!        Add nutation and libration as appropriate.
!
         allocate ( sinth(max(1_ip, nphase)), costh(max(1_ip, nphase)), source=0.0_wp )
         do i = 1, nphase

            if ( nphsco == 2 ) then
!
!              This case is so common that we'll deal with it
!              separately. We'll avoid unnecessary arithmetic
!              operations.
!
               k = 1 + 2*(i-1)
               m = 1 + k

               theta  = ( tcoef(k) + t * tcoef(m) ) * rpd

            else
!
!              THETA and DTHETA have higher-order terms; there are
!              NPHSCO coefficients for each angle.
!
               theta = 0.0_wp

               do j = 1, nphsco
!
!                 K is the start index for the coefficients of the
!                 Ith angle.
!
                  k     = j + nphsco*(i-1)

                  theta = theta  + ( t**(j-1) * tcoef(k) )

               end do

               theta  = theta * rpd

            end if

            sinth(i)  =  sin ( theta )
            costh(i)  =  cos ( theta )

         end do

!
!        Adjust RA, DEC, W by their librations and nutations.
!
         ra  = ra  + vdotg ( ac, sinth, na )
         dec = dec + vdotg ( dc, costh, nd )
         w   = w   + vdotg ( wc, sinth, nw )

!
!        Convert from degrees to radians and mod by two pi.
!
         ra     = ra     * rpd
         dec    = dec    * rpd
         w      = w      * rpd
         lambda = lambda * rpd

         ra     = mod ( ra,     twopi )
         dec    = mod ( dec,    twopi )
         w      = mod ( w,      twopi )
         lambda = mod ( lambda, twopi )

!
!        Convert to Euler angles.
!
         phi     = ra + halfpi
         delta   = halfpi - dec

      end if

!
!     Convert the angles to the J2000 frame if they are not already
!     referenced to J2000.
!
      if ( ref /= j2code ) then
!
!        Find the transformation from the J2000 frame to the frame
!        designated by REF.  Form the transformation from `REF'
!        coordinates to body-fixed coordinates, using our Euler angles.
!        Compose the transformations to obtain the J2000-to-body-fixed
!        transformation.  Decompose this transformation into Euler
!        angles.
!
         call irfrot ( ctx, j2code, ref, j2ref )

         call eul2m  ( ctx, w, &
                       delta, &
                       phi, &
                       3, 1, 3, &
                       rf2bfx               )

         call mxm    ( rf2bfx, j2ref, j2bfx )

         call m2eul  ( ctx, j2bfx,  3,  1,  3,  w,  delta,  phi )

         if ( ctx%failed() ) return

      end if

!
!     The Euler angles now give the transformation from J2000 to
!     body-fixed coordinates at epoch ET seconds past J2000,
!     regardless of the epoch and frame of the orientation constants
!     for the specified body.
!
      ra   =  phi       -  halfpi
      dec  =  halfpi  -  delta

!
!     Make sure that the prime meridian and right ascension are in
!     the correct range.
!
      if ( w < 0 ) then
         w = w + twopi
      end if

      if ( ra < 0 ) then
         ra = ra + twopi
      end if

      return
      end associate
      end subroutine bodeul

end submodule pepper_pck_api
