!> C-kernel pointing ([[ckgp]], [[ckgpav]]) and coverage ([[ckcov]], [[ckobj]]).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_ck_api

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: ndc = 2  !! number of double precision components of a segment descriptor
   integer(ip), parameter :: nic = 6  !! number of integer components of a segment descriptor
   integer(ip), parameter :: nc = ndc + ( nic + 1 )/2  !! number of characters (double precision words) a packed descriptor takes
   integer(ip), parameter :: idlen = nc * 8  !! length of a segment identifier (NC double precision words of 8 characters)
   integer(ip), parameter :: lnsize = 80  !! length of a line of text (messages, kernel lines)
   integer(ip), parameter :: nd = 2  !! number of double precision components of a DAF array summary (SPK, CK, PCK: 2)
   integer(ip), parameter :: ni = 6  !! number of integer components of a DAF array summary (SPK, CK: 6)

contains

      !> Find the set of ID codes of all objects in a specified CK file.
      !>
      !>## Detailed Input
      !>
      !>     CKFNM    is the name of a C-kernel.
      !>
      !>     IDS      is an initialized SPICE set data structure. IDS
      !>              optionally may contain a set of ID codes on input; on
      !>              output, the data already present in IDS will be combined
      !>              with ID code set found for the file CKFNM.
      !>
      !>              If IDS contains no data on input, its size and
      !>              cardinality still must be initialized.
      !>
      !>## Detailed Output
      !>
      !>     IDS      is a SPICE set data structure which contains the union
      !>              of its contents upon input with the set of ID codes of
      !>              each object for which pointing data are present in the
      !>              indicated CK file. The elements of SPICE sets are
      !>              unique; hence each ID code in IDS appears only once, even
      !>              if the CK file contains multiple segments for that ID
      !>              code.
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
      !>     3)  If the input file is a binary DAF file of type other than
      !>         CK, the error SPICE(INVALIDFILETYPE) is signaled.
      !>
      !>     4)  If the CK file cannot be opened or read, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     5)  If the size of the output set argument IDS is insufficient to
      !>         contain the actual number of ID codes of objects covered by
      !>         the indicated CK file, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>## Files
      !>
      !>     This routine reads a C-kernel.
      !>
      !>## Particulars
      !>
      !>     This routine provides an API via which applications can determine
      !>     the set of objects for which there are pointing data in a
      !>     specified CK file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Display the interval-level coverage for each object in a
      !>        specified CK file. Use tolerance of zero ticks. Do not
      !>        request angular velocity. Express the results in the TDB time
      !>        system.
      !>
      !>        Find the set of objects in the file. Loop over the contents
      !>        of the ID code set: find the coverage for each item in the
      !>        set and display the coverage.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKOBJ_EX1
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
      !>      PARAMETER           ( MAXIV  = 100000 )
      !>
      !>      INTEGER               WINSIZ
      !>      PARAMETER           ( WINSIZ = 2 * MAXIV )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 50 )
      !>
      !>      INTEGER               MAXOBJ
      !>      PARAMETER           ( MAXOBJ = 1000 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    CKFNM
      !>      CHARACTER*(FILSIZ)    LSK
      !>      CHARACTER*(FILSIZ)    SCLK
      !>      CHARACTER*(TIMLEN)    TIMSTR
      !>
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
      !>      DOUBLE PRECISION      E
      !>
      !>      INTEGER               I
      !>      INTEGER               IDS   ( LBCELL : MAXOBJ )
      !>      INTEGER               J
      !>      INTEGER               NIV
      !>
      !>C
      !>C     Load a leapseconds kernel and SCLK kernel for output
      !>C     time conversion.  Note that we assume a single
      !>C     spacecraft clock is associated with all of the objects
      !>C     in the CK.
      !>C
      !>      CALL PROMPT ( 'Name of leapseconds kernel > ', LSK  )
      !>      CALL FURNSH ( LSK )
      !>
      !>      CALL PROMPT ( 'Name of SCLK kernel        > ', SCLK )
      !>      CALL FURNSH ( SCLK )
      !>
      !>C
      !>C     Get name of CK file.
      !>C
      !>      CALL PROMPT ( 'Name of CK file            > ', CKFNM )
      !>
      !>C
      !>C     Initialize the set IDS.
      !>C
      !>      CALL SSIZEI ( MAXOBJ, IDS )
      !>
      !>C
      !>C     Initialize the window COVER.
      !>C
      !>      CALL SSIZED ( WINSIZ, COVER )
      !>
      !>C
      !>C     Find the set of objects in the CK file.
      !>C
      !>      CALL CKOBJ ( CKFNM, IDS )
      !>
      !>C
      !>C     We want to display the coverage for each object.  Loop
      !>C     over the contents of the ID code set, find the coverage
      !>C     for each item in the set, and display the coverage.
      !>C
      !>      DO I = 1, CARDI( IDS )
      !>C
      !>C        Find the coverage window for the current
      !>C        object. Empty the coverage window each time
      !>C        so we don't include data for the previous object.
      !>C
      !>         CALL SCARDD ( 0,   COVER )
      !>         CALL CKCOV  ( CKFNM,       IDS(I),  .FALSE.,
      !>     .                 'INTERVAL',  0.D0,    'TDB',    COVER )
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
      !>         WRITE (*,*) '======================================='
      !>         WRITE (*,*) 'Coverage for object ', IDS(I)
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
      !>     .                    'YYYY MON DD HR:MN:SC.###### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                           )
      !>            WRITE (*,*) ' '
      !>            WRITE (*,*) 'Interval: ', J
      !>            WRITE (*,*) 'Start:    ', TIMSTR
      !>
      !>            CALL TIMOUT ( E,
      !>     .                    'YYYY MON DD HR:MN:SC.###### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                          )
      !>            WRITE (*,*) 'Stop:     ', TIMSTR
      !>            WRITE (*,*) ' '
      !>
      !>         END DO
      !>
      !>         WRITE (*,*) '======================================='
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the LSK file named naif0010.tls, the SCLK file
      !>        named cas00145.tsc and the CK file named 08052_08057ra.bc, the
      !>        output was:
      !>
      !>        Name of leapseconds kernel > naif0010.tls
      !>        Name of SCLK kernel        > cas00145.tsc
      !>        Name of CK file            > 08052_08057ra.bc
      !>         =======================================
      !>         Coverage for object       -82000
      !>
      !>         Interval:            1
      !>         Start:    2008 FEB 21 00:01:07.771186 (TDB)
      !>         Stop:     2008 FEB 23 22:53:30.001738 (TDB)
      !>
      !>         Interval:            2
      !>         Start:    2008 FEB 23 22:58:13.999732 (TDB)
      !>         Stop:     2008 FEB 24 02:22:25.913175 (TDB)
      !>
      !>         Interval:            3
      !>         Start:    2008 FEB 24 02:27:49.910886 (TDB)
      !>         Stop:     2008 FEB 24 19:46:33.470587 (TDB)
      !>
      !>         Interval:            4
      !>         Start:    2008 FEB 24 19:49:33.469315 (TDB)
      !>         Stop:     2008 FEB 25 04:25:21.250677 (TDB)
      !>
      !>         Interval:            5
      !>         Start:    2008 FEB 25 04:29:33.248897 (TDB)
      !>         Stop:     2008 FEB 25 15:23:44.971594 (TDB)
      !>
      !>         Interval:            6
      !>         Start:    2008 FEB 25 15:24:12.971396 (TDB)
      !>         Stop:     2008 FEB 25 20:25:04.843864 (TDB)
      !>
      !>         Interval:            7
      !>         Start:    2008 FEB 25 20:25:48.843553 (TDB)
      !>         Stop:     2008 FEB 26 00:01:04.752306 (TDB)
      !>
      !>         =======================================
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB)
      !>
      !>        Changed input argument name "CK" to "CKFNM" for consistency
      !>        with other routines.
      !>
      !>        Bug fix: added call to FAILED after call to GETFAT.
      !>
      !>        Edited the header comments to comply with NAIF standard. Added
      !>        solution using CASSINI data. Corrected short error message in
      !>        entries #2 and #3 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB)
      !>
      !>        Corrected bug in program in header $Examples section: program
      !>        now empties the coverage window prior to collecting data for
      !>        the current object. Deleted declaration of unused parameter
      !>        NAMLEN in example program. Updated example to use WNCARD
      !>        rather than CARDD.
      !>
      !>-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB)
      module subroutine ckobj ( ctx, ckfnm, ids )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: ckfnm  !! Name of CK file.
      integer(ip), intent(inout) :: ids ( lbcell : * )  !! Set of ID codes of objects in CK file.
      integer(ip), allocatable :: list(:)
      integer(ip) :: k
!
!     PEPPER: the IDs are found by CK_OBJECTS; here they are added to
!     the set IDS.
!
      if ( ctx%failed() ) return
      call ck_objects ( ctx, ckfnm, list )
      do k = 1, size ( list )
         if ( ctx%failed() ) return
         call insrti ( ctx, list(k), ids )
      end do
      end subroutine ckobj

      !> The objects of a CK file (PEPPER; [[ckobj]] without a set cell):
      !> their ID codes, sorted, without duplicates.
      module subroutine ck_objects ( ctx, file, ids )
      use pepper_kinds, only: ip, wp
      use pepper_windows, only: append_id, sort_unique
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the CK file
      integer(ip), allocatable, intent(out) :: ids(:)  !! the ID codes (size 0 after an error)
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
!     See whether GETFAT thinks we've got a CK file.
!
!
!     PEPPER: a kernel built in memory: the frames of its rotation tables
!     (their CK ID codes are the frames' codes).
!
      block
         integer(ip), allocatable :: codes(:)
         logical :: is_kernel
         call kernel_frame_codes ( ctx, file, is_kernel, codes )
         if ( ctx%failed() ) return
         if ( is_kernel ) then
            ids = codes
            call sort_unique ( ids )
            return
         end if
      end block
      call getfat ( ctx, file, arch, kertyp )

      if ( ctx%failed() ) return

      if ( arch == 'XFR' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine.  If the input file is an ' &
         //            'CK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'   )
         call ctx%errch  ( '#',  file                                  )
         call ctx%errch  ( '#',  arch                                   )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                       )
         return

      else if ( arch /= 'DAF' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine.  Binary CK files have ' &
         //            'DAF architecture.  If you expected the ' &
         //            'file to be a binary CK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                        )
         call ctx%errch  ( '#',  file                                  )
         call ctx%errch  ( '#',  arch                                   )
         call ctx%sigerr ( 'SPICE(INVALIDARCHTYPE)'                     )
         return

      else if ( kertyp /= 'CK' ) then

         call ctx%setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary CK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                        )
         call ctx%errch  ( '#',  file                                  )
         call ctx%errch  ( '#',  kertyp                                 )
         call ctx%sigerr ( 'SPICE(INVALIDFILETYPE)'                     )
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
      end subroutine ck_objects

      !> Get pointing (attitude) for a specified spacecraft clock time.
      !>
      !>## Detailed Input
      !>
      !>     INST     is the NAIF integer ID for the instrument, spacecraft, or
      !>              other structure for which pointing is requested. For
      !>              brevity we will refer to this object as the "instrument,"
      !>              and the frame fixed to this object as the "instrument
      !>              frame" or "instrument-fixed" frame.
      !>
      !>     SCLKDP   is the encoded spacecraft clock time for which pointing
      !>              is requested.
      !>
      !>              The SPICELIB routines SCENCD and SCE2C respectively
      !>              convert spacecraft clock strings and ephemeris time to
      !>              encoded spacecraft clock. The inverse conversions are
      !>              performed by SCDECD and SCT2E.
      !>
      !>     TOL      is a time tolerance in ticks, the units of encoded
      !>              spacecraft clock time.
      !>
      !>              The SPICELIB routine SCTIKS converts a spacecraft clock
      !>              tolerance duration from its character string
      !>              representation to ticks. SCFMT performs the inverse
      !>              conversion.
      !>
      !>              The C-matrix returned by CKGP is the one whose time tag
      !>              is closest to SCLKDP and within TOL units of SCLKDP.
      !>              (More in $Particulars, below.)
      !>
      !>              In general, because using a non-zero tolerance affects
      !>              selection of the segment from which the data is obtained,
      !>              users are strongly discouraged from using a non-zero
      !>              tolerance when reading CKs with continuous data. Using a
      !>              non-zero tolerance should be reserved exclusively to
      !>              reading CKs with discrete data because in practice
      !>              obtaining data from such CKs using a zero tolerance is
      !>              often not possible due to time round off.
      !>
      !>     REF      is the desired reference frame for the returned pointing.
      !>              The returned C-matrix CMAT gives the orientation of the
      !>              instrument designated by INST relative to the frame
      !>              designated by REF. When a vector specified relative to
      !>              frame REF is left- multiplied by CMAT, the vector is
      !>              rotated to the frame associated with INST. See the
      !>              discussion of CMAT below for details.
      !>
      !>              Consult the SPICE document "Frames" for a discussion
      !>              of supported reference frames.
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components of a
      !>              vector expressed in the reference frame specified by REF
      !>              to components expressed in the frame tied to the
      !>              instrument, spacecraft, or other structure at time CLKOUT
      !>              (see below).
      !>
      !>              Thus, if a vector v has components x,y,z in the REF
      !>              reference frame, then v has components x',y',z' in the
      !>              instrument fixed frame at time CLKOUT:
      !>
      !>                   .-   -.     .-        -. .-   -.
      !>                   |  x' |     |          | |  x  |
      !>                   |  y' |  =  |   CMAT   | |  y  |
      !>                   |  z' |     |          | |  z  |
      !>                   `-   -'     `-        -' `-   -'
      !>
      !>              If you know x', y', z', use the transpose of the
      !>              C-matrix to determine x, y, z as follows:
      !>
      !>                   .-   -.      .-        -.T  .-   -.
      !>                   |  x  |      |          |   |  x' |
      !>                   |  y  |  =   |   CMAT   |   |  y' |
      !>                   |  z  |      |          |   |  z' |
      !>                   `-   -'      `-        -'   `-   -'
      !>
      !>                            (Transpose of CMAT)
      !>
      !>     CLKOUT   is the encoded spacecraft clock time associated with the
      !>              returned C-matrix. This value may differ from the
      !>              requested time, but never by more than the input
      !>              tolerance TOL.
      !>
      !>              The $Particulars section below describes the search
      !>              algorithm used by CKGP to satisfy a pointing request.
      !>              This algorithm determines the pointing instance (and
      !>              therefore the associated time value) that is returned.
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. FOUND will be .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If a C-kernel file has not been loaded using FURNSH prior to
      !>         a call to this routine, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     2)  If TOL is negative, found is set to .FALSE.
      !>
      !>     3)  If REF is not a supported reference frame, an error is
      !>         signaled by a routine in the call tree of this routine and
      !>         FOUND is set to .FALSE.
      !>
      !>## Files
      !>
      !>     CKGP searches through files loaded by FURNSH to locate a
      !>     segment that can satisfy the request for pointing for instrument
      !>     INST at time SCLKDP. You must load a C-kernel file using FURNSH
      !>     prior to calling this routine.
      !>
      !>## Particulars
      !>
      !>     How the tolerance argument is used
      !>     ==================================
      !>
      !>     Reading a type 1 CK segment (discrete pointing instances)
      !>     ---------------------------------------------------------
      !>
      !>     In the diagram below
      !>
      !>        - "0" is used to represent discrete pointing instances
      !>          (quaternions and associated time tags).
      !>
      !>        - "( )" are used to represent the end points of the time
      !>          interval covered by a segment in a CK file.
      !>
      !>        - SCLKDP is the time at which you requested pointing.
      !>          The location of SCLKDP relative to the time tags of the
      !>          pointing instances is indicated by the "+" sign.
      !>
      !>        - TOL is the time tolerance specified in the pointing
      !>          request. The square brackets "[ ]" represent the
      !>          endpoints of the time interval
      !>
      !>             SCLKDP-TOL : SCLKDP+TOL
      !>
      !>        - The quaternions occurring in the segment need not be
      !>          evenly spaced in time.
      !>
      !>     Case 1: pointing is available
      !>     ------------------------------
      !>
      !>                              SCLKDP
      !>                                   \   TOL
      !>                                    | /
      !>                                    |/\
      !>     Your request                [--+--]
      !>                                 .  .  .
      !>     Segment      (0-----0--0--0--0--0--0---0--0------------0--0--0--0)
      !>                                     ^
      !>                                     |
      !>                         CKGP returns this instance.
      !>
      !>     Case 2: pointing is not available
      !>     ----------------------------------
      !>
      !>                                                   SCLKDP
      !>                                                      \   TOL
      !>                                                       | /
      !>                                                       |/\
      !>     Your request                                   [--+--]
      !>                                                    .  .  .
      !>     Segment      (0-----0--0--0--0--0--0---0--0--0---------0--0--0--0)
      !>
      !>                         CKGP returns no pointing; the output
      !>                         FOUND flag is set to .FALSE.
      !>
      !>     Reading a type 2, 3, 4, or 5 CK segment (continuous pointing)
      !>     -------------------------------------------------------------
      !>
      !>     In the diagrams below
      !>
      !>        - "==" is used to represent periods of continuous pointing.
      !>
      !>        - "--" is used to represent gaps in the pointing coverage.
      !>
      !>        - "( )" are used to represent the end points of the time
      !>          interval covered by a segment in a CK file.
      !>
      !>        - SCLKDP is the time at which you requested pointing.
      !>          The location of SCLKDP relative to the time tags of the
      !>          pointing instances is indicated by the "+" sign.
      !>
      !>        - TOL is the time tolerance specified in the pointing
      !>          request. The square brackets "[ ]" represent the
      !>          endpoints of the time interval
      !>
      !>             SCLKDP-TOL : SCLKDP+TOL
      !>
      !>        - The quaternions occurring in the periods of continuous
      !>          pointing need not be evenly spaced in time.
      !>
      !>     Case 1: pointing is available at the request time
      !>     --------------------------------------------------
      !>
      !>                             SCLKDP
      !>                                   \   TOL
      !>                                    | /
      !>                                    |/\
      !>     Your request                [--+--]
      !>                                 .  .  .
      !>                                 .  .  .
      !>                                 .  .  .
      !>     Segment            (==---===========---=======----------===--)
      !>                                    ^
      !>                                    |
      !>
      !>                   The request time lies within an interval where
      !>                   continuous pointing is available. CKGP returns
      !>                   pointing at the requested epoch.
      !>
      !>     Case 2: pointing is available "near" the request time
      !>     ------------------------------------------------------
      !>
      !>                                    SCLKDP
      !>                                          \   TOL
      !>                                           | /
      !>                                           |/\
      !>     Your request                       [--+--]
      !>                                        .  .  .
      !>     Segment            (==---===========----=======---------===--)
      !>                                             ^
      !>                                             |
      !>
      !>                   The request time lies in a gap: an interval where
      !>                   continuous pointing is *not* available.  CKGP
      !>                   returns pointing for the epoch closest to the
      !>                   request time SCLKDP.
      !>
      !>     Case 3: pointing is not available
      !>     ----------------------------------
      !>
      !>                                                 SCLKDP
      !>                                                       \   TOL
      !>                                                        | /
      !>                                                        |/\
      !>     Your request                                    [--+--]
      !>                                                     .  .  .
      !>     Segment            (==---===========----=======---------===--)
      !>
      !>                         CKGP returns no pointing; the output
      !>                         FOUND flag is set to .FALSE.
      !>
      !>     Tolerance and segment priority
      !>     ==============================
      !>
      !>     CKGP searches through loaded C-kernels to satisfy a pointing
      !>     request. Last-loaded files are searched first. Individual files
      !>     are searched in backwards order, so that between competing
      !>     segments (segments containing data for the same object, for
      !>     overlapping time ranges), the one closest to the end of the file
      !>     has highest priority.
      !>
      !>     The search ends when a segment is found that can provide pointing
      !>     for the specified instrument at a time falling within the
      !>     specified tolerance on either side of the request time. Within
      !>     that segment, the instance closest to the input time is located
      !>     and returned.
      !>
      !>     The following four cases illustrate this search procedure.
      !>     Segments A and B are in the same file, with segment A located
      !>     further towards the end of the file than segment B. Both segments
      !>     A and B contain discrete pointing data, indicated by the number
      !>     0.
      !>
      !>     Case 1: Pointing is available in the first segment searched.
      !>              Because segment A has the highest priority and can
      !>              satisfy the request, segment B is not searched.
      !>
      !>                                  SCLKDP
      !>                                        \  TOL
      !>                                         | /
      !>                                         |/\
      !>     Your request                     [--+--]
      !>                                      .  .  .
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                           ^
      !>                                           |
      !>                                           |
      !>                               CKGP returns this instance
      !>
      !>     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)
      !>
      !>     Case 2: Pointing is not available in the first segment searched.
      !>              Because segment A cannot satisfy the request, segment B
      !>              is searched.
      !>
      !>                             SCLKDP
      !>                                  \   TOL
      !>                                   | /
      !>                                   |/\
      !>     Your request               [--+--]
      !>                                .  .  .
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                .  .  .
      !>     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)
      !>                                   ^
      !>                                   |
      !>                       CKGP returns this instance
      !>
      !>     Segments that contain continuous pointing data are searched in
      !>     the same manner as segments containing discrete pointing data.
      !>     For request times that fall within the bounds of continuous
      !>     intervals, CKGP will return pointing at the request time. When
      !>     the request time does not fall within an interval, then a time at
      !>     an endpoint of an interval may be returned if it is the closest
      !>     time in the segment to the user request time and is also within
      !>     the tolerance.
      !>
      !>     In the following examples, segment A is located further towards
      !>     the end of the file than segment C. Segment A contains discrete
      !>     pointing data and segment C contains continuous data, indicated
      !>     by the "=" character.
      !>
      !>     Case 3: Pointing is not available in the first segment searched.
      !>              Because segment A cannot satisfy the request, segment C
      !>              is searched.
      !>
      !>                             SCLKDP
      !>                                   \  TOL
      !>                                    | /
      !>                                    |/\
      !>     Your request                [--+--]
      !>                                 .  .  .
      !>                                 .  .  .
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                 .  .  .
      !>                                 .  .  .
      !>     Segment C          (---=============-----====--------==--)
      !>                                    ^
      !>                                    |
      !>                                    |
      !>                         CKGP returns this instance
      !>
      !>     In the next case, assume that the order of segments A and C in the
      !>     file is reversed: A is now closer to the front, so data from
      !>     segment C are considered first.
      !>
      !>     Case 4: Pointing is available in the first segment searched.
      !>              Because segment C has the highest priority and can
      !>              satisfy the request, segment A is not searched.
      !>
      !>                                             SCLKDP
      !>                                            /
      !>                                           |  TOL
      !>                                           | /
      !>                                           |/\
      !>     Your request                       [--+--]
      !>                                        .  .  .
      !>                                        .  .  .
      !>     Segment C          (---=============-----====--------==--)
      !>                                             ^
      !>                                             |
      !>                                CKGP returns this instance
      !>
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                           ^
      !>                                           |
      !>                                     "Best" answer
      !>
      !>     The next case illustrates an unfortunate side effect of using
      !>     a non-zero tolerance when reading multi-segment CKs with
      !>     continuous data. In all cases when the look-up interval
      !>     formed using tolerance overlaps a segment boundary and
      !>     the request time falls within the coverage of the lower
      !>     priority segment, the data at the end of the higher priority
      !>     segment will be picked instead of the data from the lower
      !>     priority segment.
      !>
      !>     Case 5: Pointing is available in the first segment searched.
      !>              Because segment C has the highest priority and can
      !>              satisfy the request, segment A is not searched.
      !>
      !>                                             SCLKDP
      !>                                            /
      !>                                           |  TOL
      !>                                           | /
      !>                                           |/\
      !>     Your request                       [--+--]
      !>                                        .  .  .
      !>                                        .  .  .
      !>     Segment C                                (===============)
      !>                                              ^
      !>                                              |
      !>                                CKGP returns this instance
      !>
      !>     Segment A          (=====================)
      !>                                           ^
      !>                                           |
      !>                                     "Best" answer
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) The following example program uses CKGP to get C-matrices
      !>        for a set of images whose SCLK counts (un-encoded character
      !>        string versions) are contained in the array SCLKCH.
      !>
      !>        If available, the program will get the corrected pointing
      !>        values.
      !>
      !>        For each C-matrix, a unit pointing vector is constructed and
      !>        printed.
      !>
      !>        Use the CK kernel below to load the CASSINI image navigated
      !>        spacecraft pointing and orientation data.
      !>
      !>           04153_04182ca_ISS.bc
      !>
      !>        Use the SCLK kernel below to load the CASSINI spacecraft clock
      !>        time correlation data required for the conversion between
      !>        spacecraft clock string representation and double precision
      !>        encoding of spacecraft clock counts.
      !>
      !>           cas00071.tsc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKGP_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Constants for this program.
      !>C
      !>C     -- The code for the CASSINI spacecraft clock is -82.
      !>C
      !>C     -- The code for CASSINI spacecraft reference frame
      !>C        is -82000.
      !>C
      !>C    --  Spacecraft clock times for successive CASSINI
      !>C        navigation images always differ by more than 1.0.
      !>C        This is an acceptable tolerance, and must be
      !>C        converted to "ticks" (units of encoded SCLK) for
      !>C        input to CKGP.
      !>C
      !>C     -- The reference frame we want is J2000.
      !>C
      !>C     -- The CASSINI ISS camera boresight
      !>C        in the spacecraft frame is
      !>C        (0.0005760, -0.99999982, -0.0001710).
      !>C
      !>      INTEGER               FILEN
      !>      PARAMETER           ( FILEN  = 255 )
      !>
      !>      INTEGER               NPICS
      !>      PARAMETER           ( NPICS  = 2 )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 30 )
      !>
      !>      INTEGER               REFLEN
      !>      PARAMETER           ( REFLEN = 32 )
      !>
      !>      CHARACTER*(TIMLEN)    CLKCH
      !>      CHARACTER*(FILEN)     CK
      !>      CHARACTER*(REFLEN)    REF
      !>      CHARACTER*(FILEN)     SCLK
      !>      CHARACTER*(TIMLEN)    SCLKCH ( NPICS )
      !>      CHARACTER*(TIMLEN)    TOL
      !>
      !>      DOUBLE PRECISION      CLKOUT
      !>      DOUBLE PRECISION      CMAT   ( 3, 3 )
      !>      DOUBLE PRECISION      SCLKDP
      !>      DOUBLE PRECISION      TOLTIK
      !>      DOUBLE PRECISION      ISSFIX ( 3 )
      !>      DOUBLE PRECISION      VINERT ( 3 )
      !>
      !>      INTEGER               SC
      !>      INTEGER               I
      !>      INTEGER               INST
      !>
      !>      LOGICAL               FOUND
      !>
      !>      DATA                  SCLKCH /  '1465644281.0',
      !>     .                                '1465644351.0' /
      !>
      !>      DATA                  ISSFIX /  0.00057600D0,
      !>     .                               -0.99999982D0,
      !>     .                               -0.00017100D0  /
      !>
      !>      CK         = '04153_04182ca_ISS.bc'
      !>      SCLK       = 'cas00071.tsc'
      !>      SC         = -82
      !>      INST       = -82000
      !>      TOL        = '1.0'
      !>      REF        = 'J2000'
      !>
      !>C
      !>C     Load the CK file.
      !>C
      !>      CALL FURNSH ( CK )
      !>
      !>C
      !>C     Need to load a CASSINI SCLK kernel to convert from
      !>C     clock string to ticks.  Although not required for
      !>C     the CASSINI spacecraft clock, most modern spacecraft
      !>C     clocks require a leapseconds kernel to be loaded in
      !>C     addition to an SCLK kernel.
      !>C
      !>      CALL FURNSH ( SCLK )
      !>
      !>C
      !>C     Convert tolerance from CASSINI formatted character
      !>C     string SCLK to ticks, which are units of encoded SCLK.
      !>C
      !>      CALL SCTIKS ( SC, TOL, TOLTIK )
      !>
      !>      DO I = 1, NPICS
      !>C
      !>C        CKGP requires encoded spacecraft clock.
      !>C
      !>         CALL SCENCD ( SC, SCLKCH( I ), SCLKDP )
      !>
      !>         CALL CKGP ( INST,   SCLKDP, TOLTIK, REF, CMAT,
      !>     .               CLKOUT, FOUND                      )
      !>
      !>         IF ( FOUND ) THEN
      !>
      !>C
      !>C           Use the transpose of the C-matrix to transform the
      !>C           boresight vector from camera-fixed to reference
      !>C           coordinates.
      !>C
      !>            CALL MTXV   ( CMAT, ISSFIX, VINERT )
      !>            CALL SCDECD ( SC,   CLKOUT, CLKCH  )
      !>
      !>            WRITE(*,*) 'Requested SCLK time : ', SCLKCH(I)
      !>            WRITE(*,*) '   CASSINI SCLK time: ', CLKCH
      !>            WRITE(*,'(A,3F11.7)')
      !>     .             '    CASSINI ISS boresight:', VINERT
      !>            WRITE(*,*) ' '
      !>
      !>         ELSE
      !>
      !>            WRITE (*,*) 'Pointing not found for time ',
      !>     .                                            SCLKCH(I)
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Requested SCLK time : 1465644281.0
      !>            CASSINI SCLK time: 1/1465644281.171
      !>            CASSINI ISS boresight:  0.9376789  0.3444125  0.0462419
      !>
      !>         Requested SCLK time : 1465644351.0
      !>            CASSINI SCLK time: 1/1465644351.071
      !>            CASSINI ISS boresight:  0.9376657  0.3444504  0.0462266
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
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.4.1, 26-MAY-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Updated the code, input times and kernel set to work with
      !>        PDS archived CASSINI data.
      !>
      !>-    SPICELIB Version 5.4.0, 23-SEP-2013 (BVS)
      !>
      !>        Updated to save the input frame name and POOL state counter
      !>        and to do frame name-ID conversion only if the counter has
      !>        changed.
      !>
      !>-    SPICELIB Version 5.3.1, 09-JUN-2010 (BVS)
      !>
      !>        Header update: description of the tolerance and $Particulars
      !>        section were expanded to address some problems arising from
      !>        using a non-zero tolerance.
      !>
      !>-    SPICELIB Version 5.3.0, 23-APR-2010 (NJB)
      !>
      !>        Bug fix: this routine now obtains the rotation
      !>        from the request frame to the applicable CK segment's
      !>        base frame via a call to REFCHG. Formerly the routine
      !>        used FRMCHG, which required that angular velocity data
      !>        be available for this transformation.
      !>
      !>-    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MXM call.
      !>
      !>-    SPICELIB Version 5.1.2, 29-JAN-2004 (NJB)
      !>
      !>        Header update: description of input argument REF was
      !>        expanded.
      !>
      !>-    SPICELIB Version 5.1.1, 27-JUL-2003 (CHA) (NJB)
      !>
      !>        Various header corrections were made.
      !>
      !>-    SPICELIB Version 3.2.0, 23-FEB-1999 (WLT)
      !>
      !>        The previous editions of this routine did not properly handle
      !>        the case when TOL was negative. The routine now returns a
      !>        value of .FALSE. for FOUND as is advertised above.
      !>
      !>-    SPICELIB Version 3.1.0, 13-APR-1998 (WLT)
      !>
      !>        The call to CHKOUT in the case when FAILED returned the
      !>        value .TRUE. used to check out with the name 'CKGPAV'. This
      !>        has been changed to a CKGP.
      !>
      !>-    SPICELIB Version 3.0.0, 19-SEP-1994 (WLT)
      !>
      !>        The routine was upgraded to support non-inertial frames.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
      !>
      !>        The $Particulars section was updated to show how the
      !>        search algorithm processes segments with continuous
      !>        pointing data.
      !>
      !>        The example program now loads an SCLK kernel.
      !>
      !>        FAILED is checked after the call to IRFROT to handle the
      !>        case where the reference frame is invalid and the error
      !>        handling is not set to abort.
      !>
      !>        FAILED is checked in the DO WHILE loop to handle the case
      !>        where an error is detected by a SPICELIB routine inside the
      !>        loop and the error handling is not set to abort.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        The restriction that a C-kernel file must be loaded
      !>        was explicitly stated.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MXM call.
      !>
      !>-    SPICELIB Version 3.1.0, 13-APR-1998 (WLT)
      !>
      !>        A call to FRINFO did not have enough arguments and
      !>        went undetected until Howard Taylor of ACT. Many
      !>        thanks go out to Howard for tracking down this error.
      !>
      !>-    SPICELIB Version 3.0.0, 19-SEP-1994 (WLT)
      !>
      !>        The routine was upgraded to support non-inertial frames.
      !>
      !>        Calls to NAMIRF and IRFROT were replaced with calls to
      !>        NAMFRM and FRMCHG respectively.
      !>
      !>-    SPICELIB Version 1.0.2, 30-AUG-1991 (JML)
      !>
      !>        1) The $Particulars section was updated to show how the
      !>           search algorithm processes segments with continuous
      !>           pointing data.
      !>
      !>        2) The example program now loads an SCLK kernel.
      !>
      !>        3) FAILED is checked after the call to IRFROT to handle the
      !>           case where the reference frame is invalid and the error
      !>           handling is not set to abort.
      !>
      !>        4) FAILED is checked in the DO WHILE loop to handle the case
      !>           where an error is detected by a SPICELIB routine inside the
      !>           loop and the error handling is not set to abort.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        1) The restriction that a C-kernel file must be loaded
      !>           was explicitly stated.
      !>        2) Minor changes were made to the wording of the header.
      !>
      !>-    Beta Version 1.1.0, 29-AUG-1990 (MJS)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>        1) The variable SCLK was changed to SCLKDP.
      !>        2) The variable INSTR was changed to INST.
      !>        3) The variable IDENT was changed to SEGID.
      !>        4) The declarations for the parameters NDC, NIC, NC, and
      !>           IDLEN were moved from the "Declarations" section of the
      !>           header to the "Local parameters" section of the code below
      !>           the header. These parameters are not meant to modified by
      !>           users.
      !>        5) The header was updated to reflect the changes.
      !>
      !>-    Beta Version 1.0.0, 04-MAY-1990 (RET) (IMU)
      module subroutine ckgp ( ctx, inst, sclkdp, tol, ref, cmat, clkout, found )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mxm
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inst  !! NAIF ID of instrument, spacecraft, or structure.
      real(wp), intent(in) :: sclkdp  !! Encoded spacecraft clock time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      character(*), intent(in) :: ref  !! Reference frame.
      real(wp), intent(out) :: cmat   ( 3, 3 )  !! C-matrix pointing data.
      real(wp), intent(out) :: clkout  !! Output encoded spacecraft clock time.
      logical, intent(out) :: found  !! .TRUE. when requested pointing is available.
!
!     Local variables
!
      character(idlen) :: segid
      real(wp) :: av     ( 3 )
      real(wp) :: dcd    ( ndc  )
      real(wp) :: descr  ( nc   )
      real(wp) :: et
      real(wp) :: rot    ( 3, 3 )
      real(wp) :: tmpmat ( 3, 3 )
      integer(ip) :: center
      integer(ip) :: handle
      integer(ip) :: icd    ( nic  )
      integer(ip) :: refreq
      integer(ip) :: refseg
      integer(ip) :: sclk
      integer(ip) :: type1
      integer(ip) :: type2
      integer(ip) :: typeid
      logical :: needav
      logical :: pfnd
      logical :: sfnd
      logical :: gotit
      if ( .not. allocated ( ctx%ckgp_state ) ) allocate ( ctx%ckgp_state )
      associate ( svctr1 => ctx%ckgp_state%svctr1, &
                  svref => ctx%ckgp_state%svref, &
                  svrefr => ctx%ckgp_state%svrefr, &
                  first => ctx%ckgp_state%first )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     Don't need angular velocity data.
!     Assume the segment won't be found until it really is.
!
      needav = .false.
      found  = .false.

!
!     If the tolerance is less than zero, we go no further.
!
      if ( tol < 0.0_wp ) return

!
!     Begin a search for this instrument and time, and get the first
!     applicable segment.
!
      call ckbss ( ctx, inst,   sclkdp, tol,   needav )
      call cksns ( ctx, handle, descr,  segid, sfnd   )

!
!     Keep trying candidate segments until a segment can produce a
!     pointing instance within the specified time tolerance of the
!     input time.
!
!     Check FAILED to prevent an infinite loop if an error is detected
!     by a SPICELIB routine and the error handling is not set to abort.
!

      do while ( ( sfnd ) .and. ( .not. ctx%failed() ) )

         call ckpfs ( ctx, handle, descr, sclkdp, tol, needav, &
                      cmat,   av,    clkout, pfnd        )

         if ( pfnd ) then

!
!           Found one. If the C-matrix doesn't already rotate from the
!           requested frame, convert it to one that does.
!
            call dafus  ( ctx, descr, ndc, nic, dcd, icd )
            refseg = icd( 2 )
!
!           Look up the id code for the requested reference frame.
!
            call zznamfrm ( ctx, svctr1, svref, svrefr, ref, refreq )

            if ( refreq /= refseg ) then
!
!              We may need to convert the output ticks CLKOUT to ET
!              so that we can get the needed state transformation
!              matrix.  This is the case if either of the frames
!              is non-inertial.
!
               call frinfo ( ctx, refreq, center, type1, typeid, gotit )
               call frinfo ( ctx, refseg, center, type2, typeid, gotit )

               if (       type1 == inertl &
                    .and. type2 == inertl ) then
!
!                 Any old value of ET will do in this case.  We'll
!                 use zero.
!
                  et = 0.0_wp

               else
!
!                 Look up the spacecraft clock id to use to convert
!                 the output CLKOUT to ET.
!
                  call ckmeta ( ctx, inst, 'SCLK', sclk )
                  call sct2e  ( ctx, sclk, clkout, et   )

               end if
!
!              Get the transformation from the requested frame to
!              the segment frame at ET.
!
               call refchg ( ctx, refreq, refseg, et, rot )
!
!              If REFCHG detects that the reference frame is invalid
!              then return from this routine with FOUND equal to false.
!
               if ( ctx%failed() ) return
!
!              Transform the attitude information: convert CMAT so that
!              it maps from request frame to C-matrix frame.
!
               call mxm   ( cmat,   rot, tmpmat )
               cmat = tmpmat

            end if

            found = .true.

            return

         end if

         call cksns ( ctx, handle, descr, segid, sfnd )

      end do

      return
      end associate
      end subroutine ckgp

      !> Get pointing (attitude) and angular velocity for a specified
      !> spacecraft clock time.
      !>
      !>## Detailed Input
      !>
      !>     INST     is the NAIF integer ID for the instrument, spacecraft,
      !>              or other structure for which pointing and angular
      !>              velocity are requested. For brevity we will refer to
      !>              this object as the "instrument," and the frame fixed
      !>              to this object as the "instrument frame" or
      !>              "instrument-fixed" frame.
      !>
      !>     SCLKDP   is the encoded spacecraft clock time for which
      !>              pointing and angular velocity are requested.
      !>
      !>              The SPICELIB routines SCENCD and SCE2C respectively
      !>              convert spacecraft clock strings and ephemeris time to
      !>              encoded spacecraft clock. The inverse conversions are
      !>              performed by SCDECD and SCT2E.
      !>
      !>     TOL      is a time tolerance in ticks, the units of encoded
      !>              spacecraft clock time.
      !>
      !>              The SPICELIB routine SCTIKS converts a spacecraft
      !>              clock tolerance duration from its character string
      !>              representation to ticks. SCFMT performs the inverse
      !>              conversion.
      !>
      !>              The C-matrix - angular velocity vector pair returned by
      !>              CKGPAV is the one whose time tag is closest to SCLKDP
      !>              and within TOL units of SCLKDP. (More in $Particulars,
      !>              below.)
      !>
      !>              In general, because using a non-zero tolerance
      !>              affects selection of the segment from which the
      !>              data is obtained, users are strongly discouraged
      !>              from using a non-zero tolerance when reading CKs
      !>              with continuous data. Using a non-zero tolerance
      !>              should be reserved exclusively to reading CKs with
      !>              discrete data because in practice obtaining data
      !>              from such CKs using a zero tolerance is often not
      !>              possible due to time round off.
      !>
      !>     REF      is the desired reference frame for the returned
      !>              pointing and angular velocity. The returned C-matrix
      !>              CMAT gives the orientation of the instrument
      !>              designated by INST relative to the frame designated by
      !>              REF. When a vector specified relative to frame REF is
      !>              left-multiplied by CMAT, the vector is rotated to the
      !>              frame associated with INST. The returned angular
      !>              velocity vector AV expresses the angular velocity of
      !>              the instrument designated by INST relative to the
      !>              frame designated by REF. See the discussion of CMAT
      !>              and AV below for details.
      !>
      !>              Consult the SPICE document "Frames" for a discussion
      !>              of supported reference frames.
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components of
      !>              a vector expressed in the reference frame specified by
      !>              REF to components expressed in the frame tied to the
      !>              instrument, spacecraft, or other structure at time
      !>              CLKOUT (see below).
      !>
      !>              Thus, if a vector v has components x,y,z in the REF
      !>              reference frame, then v has components x',y',z' in the
      !>              instrument fixed frame at time CLKOUT:
      !>
      !>                 .-   -.     .-        -. .-   -.
      !>                 |  x' |     |          | |  x  |
      !>                 |  y' |  =  |   CMAT   | |  y  |
      !>                 |  z' |     |          | |  z  |
      !>                 '-   -'     '-        -' '-   -'
      !>
      !>              If you know x', y', z', use the transpose of the
      !>              C-matrix to determine x, y, z as follows:
      !>
      !>                 .-   -.      .-        -.T  .-   -.
      !>                 |  x  |      |          |   |  x' |
      !>                 |  y  |  =   |   CMAT   |   |  y' |
      !>                 |  z  |      |          |   |  z' |
      !>                 '-   -'      '-        -'   '-   -'
      !>
      !>                         (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector. This is the axis about
      !>              which the reference frame tied to the instrument is
      !>              rotating in the right-handed sense at time CLKOUT. The
      !>              magnitude of AV is the magnitude of the instantaneous
      !>              velocity of the rotation, in radians per second.  AV
      !>              is expressed relative to the frame designated by REF.
      !>
      !>     CLKOUT   is the encoded spacecraft clock time associated with
      !>              the returned C-matrix and the returned angular
      !>              velocity vector. This value may differ from the
      !>              requested time, but never by more than the input
      !>              tolerance TOL.
      !>
      !>              The $Particulars section below describes the search
      !>              algorithm used by CKGPAV to satisfy a pointing
      !>              request. This algorithm determines the pointing
      !>              instance (and therefore the associated time value)
      !>              that is returned.
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. FOUND will be .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If a C-kernel file has not been loaded using FURNSH prior to
      !>         a call to this routine, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     2)  If TOL is negative, found is set to .FALSE.
      !>
      !>     3)  If REF is not a supported reference frame, an error is
      !>         signaled by a routine in the call tree of this routine and
      !>         FOUND is set to .FALSE.
      !>
      !>## Files
      !>
      !>     CKGPAV searches through files loaded by FURNSH to locate a
      !>     segment that can satisfy the request for pointing and angular
      !>     velocity for instrument INST at time SCLKDP. You must load a
      !>     C-kernel file using FURNSH prior to calling this routine.
      !>
      !>## Particulars
      !>
      !>     How the tolerance argument is used
      !>     ==================================
      !>
      !>     Reading a type 1 CK segment (discrete pointing instances)
      !>     ---------------------------------------------------------
      !>
      !>     In the diagram below
      !>
      !>        - "0" is used to represent discrete pointing instances
      !>          (quaternions, angular velocity vectors, and associated
      !>          time tags).
      !>
      !>        - "( )" are used to represent the end points of the time
      !>          interval covered by a segment in a CK file.
      !>
      !>        - SCLKDP is the time at which you requested pointing.
      !>          The location of SCLKDP relative to the time tags of the
      !>          pointing instances is indicated by the "+" sign.
      !>
      !>        - TOL is the time tolerance specified in the pointing
      !>          request. The square brackets "[ ]" represent the
      !>          endpoints of the time interval
      !>
      !>             SCLKDP-TOL : SCLKDP+TOL
      !>
      !>        - The quaternions occurring in the segment need not be
      !>          evenly spaced in time.
      !>
      !>     Case 1:  pointing is available
      !>     ------------------------------
      !>
      !>                              SCLKDP
      !>                                   \   TOL
      !>                                    | /
      !>                                    |/\
      !>     Your request                [--+--]
      !>                                 .  .  .
      !>     Segment      (0-----0--0--0--0--0--0---0--0------------0--0--0--0)
      !>                                     ^
      !>                                     |
      !>                         CKGPAV returns this instance.
      !>
      !>     Case 2:  pointing is not available
      !>     ----------------------------------
      !>
      !>                                                   SCLKDP
      !>                                                      \   TOL
      !>                                                       | /
      !>                                                       |/\
      !>     Your request                                   [--+--]
      !>                                                    .  .  .
      !>     Segment      (0-----0--0--0--0--0--0---0--0--0---------0--0--0--0)
      !>
      !>                         CKGPAV returns no pointing; the output
      !>                         FOUND flag is set to .FALSE.
      !>
      !>     Reading a type 2, 3, 4, or 5 CK segment (continuous pointing)
      !>     -------------------------------------------------------------
      !>
      !>     In the diagrams below
      !>
      !>        - "==" is used to represent periods of continuous pointing.
      !>
      !>        - "--" is used to represent gaps in the pointing coverage.
      !>
      !>        - "( )" are used to represent the end points of the time
      !>          interval covered by a segment in a CK file.
      !>
      !>        - SCLKDP is the time at which you requested pointing.
      !>          The location of SCLKDP relative to the time tags of the
      !>          pointing instances is indicated by the "+" sign.
      !>
      !>        - TOL is the time tolerance specified in the pointing
      !>          request. The square brackets "[ ]" represent the
      !>          endpoints of the time interval
      !>
      !>             SCLKDP-TOL : SCLKDP+TOL
      !>
      !>        - The quaternions occurring in the periods of continuous
      !>          pointing need not be evenly spaced in time.
      !>
      !>     Case 1:  pointing is available at the request time
      !>     --------------------------------------------------
      !>
      !>                             SCLKDP
      !>                                   \   TOL
      !>                                    | /
      !>                                    |/\
      !>     Your request                [--+--]
      !>                                 .  .  .
      !>                                 .  .  .
      !>                                 .  .  .
      !>     Segment            (==---===========---=======----------===--)
      !>                                    ^
      !>                                    |
      !>
      !>                   The request time lies within an interval where
      !>                   continuous pointing is available. CKGPAV returns
      !>                   pointing at the requested epoch.
      !>
      !>     Case 2:  pointing is available "near" the request time
      !>     ------------------------------------------------------
      !>
      !>                                    SCLKDP
      !>                                          \   TOL
      !>                                           | /
      !>                                           |/\
      !>     Your request                       [--+--]
      !>                                        .  .  .
      !>     Segment            (==---===========----=======---------===--)
      !>                                             ^
      !>                                             |
      !>
      !>                   The request time lies in a gap: an interval where
      !>                   continuous pointing is *not* available.  CKGPAV
      !>                   returns pointing for the epoch closest to the
      !>                   request time SCLKDP.
      !>
      !>     Case 3:  pointing is not available
      !>     ----------------------------------
      !>
      !>                                                 SCLKDP
      !>                                                       \   TOL
      !>                                                        | /
      !>                                                        |/\
      !>     Your request                                    [--+--]
      !>                                                     .  .  .
      !>     Segment            (==---===========----=======---------===--)
      !>
      !>                         CKGPAV returns no pointing; the output
      !>                         FOUND flag is set to .FALSE.
      !>
      !>     Tolerance and segment priority
      !>     ==============================
      !>
      !>     CKGPAV searches through loaded C-kernels to satisfy a pointing
      !>     request. Last-loaded files are searched first. Individual files
      !>     are searched in backwards order, so that between competing
      !>     segments (segments containing data for the same object, for
      !>     overlapping time ranges), the one closest to the end of the file
      !>     has highest priority. CKGPAV considers only those segments that
      !>     contain both pointing and angular velocity data, as indicated by
      !>     the segment descriptor.
      !>
      !>     The search ends when a segment is found that can provide pointing
      !>     and angular velocity for the specified instrument at a time
      !>     falling within the specified tolerance on either side of the
      !>     request time. Within that segment, the instance closest to the
      !>     input time is located and returned.
      !>
      !>     The following four cases illustrate this search procedure.
      !>     Segments A and B are in the same file, with segment A located
      !>     further towards the end of the file than segment B. Both segments
      !>     A and B contain discrete pointing data, indicated by the number
      !>     0.
      !>
      !>     Case 1:  Pointing is available in the first segment searched.
      !>              Because segment A has the highest priority and can
      !>              satisfy the request, segment B is not searched.
      !>
      !>                                  SCLKDP
      !>                                        \  TOL
      !>                                         | /
      !>                                         |/\
      !>     Your request                     [--+--]
      !>                                      .  .  .
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                           ^
      !>                                           |
      !>                                           |
      !>                               CKGPAV returns this instance
      !>
      !>     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)
      !>
      !>     Case 2:  Pointing is not available in the first segment searched.
      !>              Because segment A cannot satisfy the request, segment B
      !>              is searched.
      !>
      !>                             SCLKDP
      !>                                  \   TOL
      !>                                   | /
      !>                                   |/\
      !>     Your request               [--+--]
      !>                                .  .  .
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                .  .  .
      !>     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0)
      !>                                   ^
      !>                                   |
      !>                       CKGPAV returns this instance
      !>
      !>     Segments that contain continuous pointing data are searched in
      !>     the same manner as segments containing discrete pointing data.
      !>     For request times that fall within the bounds of continuous
      !>     intervals, CKGPAV will return pointing at the request time. When
      !>     the request time does not fall within an interval, then a time at
      !>     an endpoint of an interval may be returned if it is the closest
      !>     time in the segment to the user request time and is also within
      !>     the tolerance.
      !>
      !>     In the following examples, segment A is located further towards
      !>     the end of the file than segment C. Segment A contains discrete
      !>     pointing data and segment C contains continuous data, indicated
      !>     by the "=" character.
      !>
      !>     Case 3:  Pointing is not available in the first segment searched.
      !>              Because segment A cannot satisfy the request, segment C
      !>              is searched.
      !>
      !>                             SCLKDP
      !>                                   \  TOL
      !>                                    | /
      !>                                    |/\
      !>     Your request                [--+--]
      !>                                 .  .  .
      !>                                 .  .  .
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                 .  .  .
      !>                                 .  .  .
      !>     Segment C          (---=============-----====--------==--)
      !>                                    ^
      !>                                    |
      !>                                    |
      !>                         CKGPAV returns this instance
      !>
      !>     In the next case, assume that the order of segments A and C in the
      !>     file is reversed: A is now closer to the front, so data from
      !>     segment C are considered first.
      !>
      !>     Case 4:  Pointing is available in the first segment searched.
      !>              Because segment C has the highest priority and can
      !>              satisfy the request, segment A is not searched.
      !>
      !>                                             SCLKDP
      !>                                            /
      !>                                           |  TOL
      !>                                           | /
      !>                                           |/\
      !>     Your request                       [--+--]
      !>                                        .  .  .
      !>                                        .  .  .
      !>     Segment C          (---=============-----====--------==--)
      !>                                             ^
      !>                                             |
      !>                                CKGPAV returns this instance
      !>
      !>     Segment A          (0-----------------0--------0--0-----0)
      !>                                           ^
      !>                                           |
      !>                                     "Best" answer
      !>
      !>     The next case illustrates an unfortunate side effect of using
      !>     a non-zero tolerance when reading multi-segment CKs with
      !>     continuous data. In all cases when the look-up interval
      !>     formed using tolerance overlaps a segment boundary and
      !>     the request time falls within the coverage of the lower
      !>     priority segment, the data at the end of the higher priority
      !>     segment will be picked instead of the data from the lower
      !>     priority segment.
      !>
      !>     Case 5:  Pointing is available in the first segment searched.
      !>              Because segment C has the highest priority and can
      !>              satisfy the request, segment A is not searched.
      !>
      !>                                             SCLKDP
      !>                                            /
      !>                                           |  TOL
      !>                                           | /
      !>                                           |/\
      !>     Your request                       [--+--]
      !>                                        .  .  .
      !>                                        .  .  .
      !>     Segment C                                (===============)
      !>                                              ^
      !>                                              |
      !>                                CKGPAV returns this instance
      !>
      !>     Segment A          (=====================)
      !>                                           ^
      !>                                           |
      !>                                     "Best" answer
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following example program uses CKGPAV to get C-matrices
      !>        and associated angular velocity vectors for a set of images
      !>        whose SCLK counts (un-encoded character string versions) are
      !>        known.
      !>
      !>        For each C-matrix, a unit pointing vector is constructed and
      !>        printed along with the angular velocity vector.
      !>
      !>        Note: if the C-kernels of interest do not contain angular
      !>        velocity data, then the SPICELIB routine CKGP should be used
      !>        to read the pointing data. An example program in the header
      !>        of the SPICELIB routine CKGP demonstrates this.
      !>
      !>        We need to load also an SCLK kernel to convert from clock
      !>        string to 'ticks.' Although not required for older spacecraft
      !>        clocks, most modern spacecraft ones require a leapseconds
      !>        kernel to be loaded in addition to an SCLK kernel.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ckgpav_ex1.tm
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
      !>              File name              Contents
      !>              --------------------   -----------------------
      !>              cas00071.tsc           CASSINI SCLK
      !>              04153_04182ca_ISS.bc   CASSINI image navigated
      !>                                     spacecraft CK
      !>
      !>           \begindata
      !>
      !>             KERNELS_TO_LOAD = ( 'cas00071.tsc'
      !>                                 '04153_04182ca_ISS.bc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKGPAV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Constants for this program.
      !>C
      !>C     -- The code for the CASSINI spacecraft clock is -82.
      !>C
      !>C     -- The code for CASSINI spacecraft reference frame is
      !>C        -82000.
      !>C
      !>C    --  Spacecraft clock times for successive CASSINI
      !>C        navigation images always differ by more than 1.0
      !>C        seconds. This is an acceptable tolerance, and must
      !>C        be converted to "ticks" (units of encoded SCLK) for
      !>C        input to CKGPAV.
      !>C
      !>C     -- The reference frame we want is J2000.
      !>C
      !>C     -- The CASSINI ISS camera boresight in the spacecraft
      !>C        frame is (0.0005760, -0.99999982, -0.0001710).
      !>C
      !>      INTEGER               NPICS
      !>      PARAMETER           ( NPICS  = 2 )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 30 )
      !>
      !>      INTEGER               REFLEN
      !>      PARAMETER           ( REFLEN = 32 )
      !>
      !>      CHARACTER*(TIMLEN)    CLKCH
      !>      CHARACTER*(REFLEN)    REF
      !>      CHARACTER*(TIMLEN)    SCLKCH ( NPICS )
      !>      CHARACTER*(TIMLEN)    TOL
      !>
      !>      DOUBLE PRECISION      AV     ( 3 )
      !>      DOUBLE PRECISION      CLKOUT
      !>      DOUBLE PRECISION      CMAT   ( 3, 3 )
      !>      DOUBLE PRECISION      SCLKDP
      !>      DOUBLE PRECISION      TOLTIK
      !>      DOUBLE PRECISION      ISSFIX ( 3 )
      !>      DOUBLE PRECISION      VINERT ( 3 )
      !>
      !>      INTEGER               SC
      !>      INTEGER               I
      !>      INTEGER               INST
      !>
      !>      LOGICAL               FOUND
      !>
      !>      DATA                  SCLKCH /  '1465644281.0',
      !>     .                                '1465644351.0' /
      !>
      !>      DATA                  ISSFIX /  0.00057600D0,
      !>     .                               -0.99999982D0,
      !>     .                               -0.00017100D0  /
      !>
      !>      SC   = -82
      !>      INST = -82000
      !>      TOL  = '1.0'
      !>      REF  = 'J2000'
      !>
      !>C
      !>C     Load kernels.
      !>C
      !>      CALL FURNSH ( 'ckgpav_ex1.tm' )
      !>
      !>C
      !>C     Convert tolerance from CASSINI formatted character
      !>C     string SCLK to ticks which are units of encoded SCLK.
      !>C
      !>      CALL SCTIKS ( SC, TOL, TOLTIK )
      !>
      !>      DO I = 1, NPICS
      !>C
      !>C        CKGPAV requires encoded spacecraft clock.
      !>C
      !>         CALL SCENCD ( SC, SCLKCH( I ), SCLKDP )
      !>
      !>         CALL CKGPAV ( INST,   SCLKDP, TOLTIK, REF, CMAT, AV,
      !>     .                 CLKOUT, FOUND                        )
      !>
      !>         IF ( FOUND ) THEN
      !>
      !>C
      !>C           Use the transpose of the C-matrix to transform the
      !>C           boresight vector from camera-fixed to reference
      !>C           coordinates.
      !>C
      !>            CALL MTXV   ( CMAT, ISSFIX, VINERT )
      !>            CALL SCDECD ( SC,   CLKOUT, CLKCH  )
      !>
      !>            WRITE(*,*) 'Requested SCLK time : ', SCLKCH(I)
      !>            WRITE(*,*) '   CASSINI SCLK time: ', CLKCH
      !>            WRITE(*,'(A,3F11.7)')
      !>     .             '    CASSINI ISS boresight  :', VINERT
      !>            WRITE(*,'(A,3F11.7)')
      !>     .             '    Angular velocity vector:', AV
      !>            WRITE(*,*) ' '
      !>
      !>         ELSE
      !>
      !>            WRITE (*,*) 'Pointing not found for time ',
      !>     .                   SCLKCH(I)
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Requested SCLK time : 1465644281.0
      !>            CASSINI SCLK time: 1/1465644281.171
      !>            CASSINI ISS boresight  :  0.9376789  0.3444125  0.0462419
      !>            Angular velocity vector:  0.0000000  0.0000000  0.0000000
      !>
      !>         Requested SCLK time : 1465644351.0
      !>            CASSINI SCLK time: 1/1465644351.071
      !>            CASSINI ISS boresight  :  0.9376657  0.3444504  0.0462266
      !>            Angular velocity vector:  0.0000000  0.0000000  0.0000000
      !>
      !>## Restrictions
      !>
      !>     1)  Only loaded C-kernel segments containing both pointing and
      !>         angular velocity data will be searched by this reader.
      !>         Segments containing only pointing data will be skipped over.
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
      !>     J.M. Lynch         (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.3.1, 02-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated the
      !>        example code, input times and kernel set to work with
      !>        PDS archived CASSINI data.
      !>
      !>        Added missing entry in $Versions section.
      !>
      !>-    SPICELIB Version 5.3.0, 23-SEP-2013 (BVS)
      !>
      !>        Updated to save the input frame name and POOL state counter
      !>        and to do frame name-ID conversion only if the counter has
      !>        changed.
      !>
      !>-    SPICELIB Version 5.2.1, 03-JUN-2010 (BVS)
      !>
      !>        Header update: description of the tolerance and $Particulars
      !>        section were expanded to address some problems arising from
      !>        using a non-zero tolerance.
      !>
      !>-    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MTXV, MXM and VADD calls.
      !>
      !>-    SPICELIB Version 5.1.2, 29-JAN-2004 (NJB)
      !>
      !>        Header update: descriptions of input arguments REF and
      !>        AV were expanded.
      !>
      !>-    SPICELIB Version 5.1.1, 27-JUL-2003 (CHA) (NJB)
      !>
      !>        Various header corrections were made.
      !>
      !>-    SPICELIB Version 5.1.0, 23-FEB-1999 (WLT)
      !>
      !>        The previous editions of this routine did not properly handle
      !>        the case when TOL was negative. The routine now returns a
      !>        value of .FALSE. for FOUND as is advertised above.
      !>
      !>-    SPICELIB Version 5.0.0, 28-JUL-1997 (WLT)
      !>
      !>        The previous routine incorrectly computed the angular
      !>        velocity of the transformation from the request frame
      !>        to the platform frame of the C-matrix for non-inertial
      !>        reference frames.
      !>
      !>-    SPICELIB Version 4.1.0, 20-DEC-1995 (WLT)
      !>
      !>        A call to FRINFO did not have enough arguments and
      !>        went undetected until Howard Taylor of ACT. Many
      !>        thanks go out to Howard for tracking down this error.
      !>
      !>-    SPICELIB Version 4.0.0, 19-SEP-1995 (WLT)
      !>
      !>        The routine was upgraded so that the reference frame may
      !>        be non-inertial.
      !>
      !>-    SPICELIB Version 3.0.0, 05-OCT-1994 (WLT)
      !>
      !>        The previous versions all computed an incorrect
      !>        value for the angular velocity if the frame specified by
      !>        REF was different from the reference frame of the segment
      !>        from which the angular velocity was extracted. This has
      !>        now been corrected.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
      !>
      !>        1) The $Particulars section was updated to show how the
      !>           search algorithm processes segments with continuous
      !>           pointing data.
      !>
      !>        2) It was specified that the angular velocity vector
      !>           gives the right-handed axis about which the instrument
      !>           frame rotates.
      !>
      !>        3) The example program now loads an SCLK kernel.
      !>
      !>        4) FAILED is checked after the call to IRFROT to handle the
      !>           case where the reference frame is invalid and the error
      !>           handling is not set to abort.
      !>
      !>        5) FAILED is checked in the DO WHILE loop to handle the case
      !>           where an error is detected by a SPICELIB routine inside the
      !>           loop and the error handling is not set to abort.
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (JML)
      !>
      !>        1) The variable NEEDAV is no longer being saved.
      !>        2) In the example program, the calling sequences
      !>           for SCENCD and CKGPAV were corrected.
      !>        3) The restriction that a C-kernel file must be loaded
      !>           was explicitly stated.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 5.3.0, 23-SEP-2013 (BVS)
      !>
      !>        Updated to save the input frame name and POOL state counter
      !>        and to do frame name-ID conversion only if the counter has
      !>        changed.
      !>
      !>-    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MTXV, MXM and VADD calls.
      !>
      !>-    SPICELIB Version 5.1.0, 23-FEB-1999 (WLT)
      !>
      !>        The previous editions of this routine did not properly handle
      !>        the case when TOL was negative. The routine now returns a
      !>        value of .FALSE. for FOUND as is advertised above.
      !>
      !>-    SPICELIB Version 5.0.0, 28-JUL-1997 (WLT)
      !>
      !>        The previous routine incorrectly computed the angular
      !>        velocity of the transformation from the request frame
      !>        to the platform frame of the C-matrix for non-inertial
      !>        reference frames.
      !>
      !>-    SPICELIB Version 4.1.0, 20-DEC-1995 (WLT)
      !>
      !>        A call to FRINFO did not have enough arguments and
      !>        went undetected until Howard Taylor of ACT. Many
      !>        thanks go out to Howard for tracking down this error.
      !>
      !>-    SPICELIB Version 4.0.0, 19-SEP-1995 (WLT)
      !>
      !>        The routine was upgraded so that the reference frame may
      !>        be non-inertial.
      !>
      !>-    SPICELIB Version 3.0.0, 5-OCT-1994 (WLT)
      !>
      !>        The previous versions all computed an incorrect
      !>        value for the angular velocity if the frame specified by
      !>        REF was different from the reference frame of the segment
      !>        from which the angular velocity was extracted. This has
      !>        now been corrected.
      !>
      !>        Previously we were multiplying by the inverse of the
      !>        rotation that transforms frames.
      !>
      !>-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
      !>
      !>        1) The $Particulars section was updated to show how the
      !>           search algorithm processes segments with continuous
      !>           pointing data.
      !>
      !>        2) It was specified that the angular velocity vector
      !>           gives the right-handed axis about which the instrument
      !>           frame rotates.
      !>
      !>        3) The example program now loads an SCLK kernel.
      !>
      !>        4) FAILED is checked after the call to IRFROT to handle the
      !>           case where the reference frame is invalid and the error
      !>           handling is not set to abort.
      !>
      !>        5) FAILED is checked in the DO WHILE loop to handle the case
      !>           where an error is detected by a SPICELIB routine inside the
      !>           loop and the error handling is not set to abort.
      !>
      !>-    SPICELIB Version 1.1.0, 02-NOV-1990 (JML)
      !>
      !>        1) The variable NEEDAV is no longer being saved.
      !>        2) In the example program, the calling sequences
      !>           for SCENCD and CKGPAV were corrected.
      !>        3) The restriction that a C-kernel file must be loaded
      !>           was explicitly stated.
      !>
      !>-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>        1) The variable SCLK was changed to SCLKDP.
      !>        2) The variable INSTR was changed to INST.
      !>        3) The variable IDENT was changed to SEGID.
      !>        4) The declarations for the parameters NDC, NIC, NC, and
      !>           IDLEN were moved from the "Declarations" section of the
      !>           header to the "Local parameters" section of the code below
      !>           the header. These parameters are not meant to modified by
      !>           users.
      !>        5) The header was updated to reflect the changes.
      !>
      !>-    Beta Version 1.0.0, 04-JUN-1990 (RET) (IMU)
      module subroutine ckgpav ( ctx, inst, sclkdp, tol, ref, cmat, av, clkout, found )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: mtxv, mxm, xf2rav
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inst  !! NAIF ID of instrument, spacecraft, or structure.
      real(wp), intent(in) :: sclkdp  !! Encoded spacecraft clock time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      character(*), intent(in) :: ref  !! Reference frame.
      real(wp), intent(out) :: cmat   ( 3, 3 )  !! C-matrix pointing data.
      real(wp), intent(out) :: av     ( 3    )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! Output encoded spacecraft clock time.
      logical, intent(out) :: found  !! .TRUE. when requested pointing is available.
!
!     Local variables
!
      character(idlen) :: segid
      real(wp) :: dcd      ( ndc  )
      real(wp) :: descr    ( nc   )
      real(wp) :: et
      real(wp) :: omega    ( 3    )
      real(wp) :: rot      ( 3, 3 )
      real(wp) :: tmpmat   ( 3, 3 )
      real(wp) :: tmpv     ( 3 )
      real(wp) :: xform    ( 6, 6 )
      integer(ip) :: center
      integer(ip) :: handle
      integer(ip) :: icd      ( nic  )
      integer(ip) :: refreq
      integer(ip) :: refseg
      integer(ip) :: sclk
      integer(ip) :: type1
      integer(ip) :: type2
      integer(ip) :: typeid
      logical :: needav
      logical :: pfnd
      logical :: sfnd
      logical :: gotit
      if ( .not. allocated ( ctx%ckgpav_state ) ) allocate ( ctx%ckgpav_state )
      associate ( svctr1 => ctx%ckgpav_state%svctr1, &
                  svref => ctx%ckgpav_state%svref, &
                  svrefr => ctx%ckgpav_state%svrefr, &
                  first => ctx%ckgpav_state%first )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     Need angular velocity data.
!     Assume the segment won't be found until it really is.
!
      needav = .true.
      found  = .false.

!
!     If the tolerance is less than zero, we go no further.
!
      if ( tol < 0.0_wp ) return

!
!     Begin a search for this instrument and time, and get the first
!     applicable segment.
!
      call ckbss ( ctx, inst,   sclkdp, tol,   needav )
      call cksns ( ctx, handle, descr,  segid, sfnd   )

!
!     Keep trying candidate segments until a segment can produce a
!     pointing instance within the specified time tolerance of the
!     input time.
!
!     Check FAILED to prevent an infinite loop if an error is detected
!     by a SPICELIB routine and the error handling is not set to abort.
!
      do while ( ( sfnd ) .and. ( .not. ctx%failed() ) )

         call ckpfs ( ctx, handle, descr, sclkdp, tol, needav, &
                      cmat,   av,    clkout, pfnd          )

         if ( pfnd ) then
!
!           Found one. If the data aren't already referenced to the
!           requested frame, rotate them.
!
            call dafus  ( ctx, descr, ndc, nic, dcd, icd )
            refseg = icd( 2 )
!
!           Look up the id code for the requested reference frame.
!
            call zznamfrm ( ctx, svctr1, svref, svrefr, ref, refreq )

            if ( refreq /= refseg ) then
!
!              We may need to convert the output ticks CLKOUT to ET
!              so that we can get the needed state transformation
!              matrix.  This is the case if either of the frames
!              is non-inertial.
!
               call frinfo ( ctx, refreq, center, type1, typeid, gotit )
               call frinfo ( ctx, refseg, center, type2, typeid, gotit )

               if (       type1 == inertl &
                    .and. type2 == inertl ) then
!
!                 Any old value of ET will do in this case.  We'll
!                 use zero.
!
                  et = 0.0_wp

               else
!
!                 Look up the spacecraft clock id to use to convert
!                 the output CLKOUT to ET.
!
                  call ckmeta ( ctx, inst, 'SCLK', sclk )
                  call sct2e  ( ctx, sclk, clkout, et   )

               end if
!
!              Get the transformation from the requested frame to
!              the segment frame at ET.
!
               call frmchg ( ctx, refreq, refseg, et, xform )
!
!              If FRMCHG detects that the reference frame is invalid
!              then return from this routine with FOUND equal to false.
!
               if ( ctx%failed() ) return
!
!              First transform the attitude information. Get the
!              rotation and angular velocity associated with the
!              transformation from request frame to segment frame.
!              Then convert CMAT so that it maps from request frame
!              to C-matrix frame.
!
               call xf2rav ( xform,   rot,  omega  )
               call mxm    ( cmat,    rot,  tmpmat )
               cmat = tmpmat
!
!              Now transform the angular velocity information.
!              Currently we have OMEGA (the angular velocity of
!              the transformation from REF frame to the base
!              frame of the C-matrix), and AV the angular velocity
!              of the transformation from the C-MATRIX reference
!              system to the platform of the C-matrix.
!
!              The angular velocity of the C-MATRIX relative to
!              requested frame is given by
!
!                            T
!                 OMEGA + ROT * AV
!
!              Here's why.
!
!              The transformation from the request frame to the frame
!              of the C-kernel looks like this:
!
!                     [                 ]
!                     [   ROT   :   0   ]
!                     [................ ]
!                     [  dROT   :       ]
!                     [  ----   : ROT   ]
!                     [    dt   :       ]
!
!              The transformation from the C-kernel reference frame to
!              the C-kernel platform frame looks like:
!
!                     [                 ]
!                     [  CMAT   :   0   ]
!                     [ ............... ]
!                     [  dCMAT  :       ]
!                     [  ----   : CMAT  ]
!                     [    dt   :       ]
!
!              The transformation from the request frame to the platform
!              frame is the product shown below
!
!               [                 ][                 ]
!               [  CMAT   :   0   ][   ROT   :   0   ]
!               [ ............... ][................ ]
!               [  dCMAT  :       ][  dROT   :       ]
!               [  ----   : CMAT  ][  ----   : ROT   ]
!               [    dt   :       ][    dt   :       ]
!
!                     [                            :             ]
!                     [  CMAT * ROT                :   0         ]
!              =      [ ........................................ ]
!                     [  dCMAT                dROT :             ]
!                     [  ----  * ROT + CMAT * ---- : CMAT * ROT  ]
!                     [    dt                  dt  :             ]
!
!              In general, the angular velocity matrix of a
!              transformation R is given by
!
!                   T
!                 dR
!                 --  * R
!                 dt
!
!              Substituting the appropriate components of the matrix
!              in for R we have:
!
!                                  T        T
!              OMEGA         =  ROT  * dCMAT  * CMAT * ROT
!                   CMAT*ROT           -----
!                                        dt
!
!                                        T
!                                    dROT       T
!                               +    ---- * CMAT  * CMAT * ROT
!                                     dt
!
!                                  T
!                            =  ROT  * OMEGA      * ROT   + OMEGA
!                                            CMAT                ROT
!
!              Consider the first term of the final expression. If we
!              let "x" stand for the cross product operation, then by
!              definition for any vector V:
!
!                  T
!               ROT  * OMEGA     * ROT  * V
!                           CMAT
!
!                             T
!                        = ROT  * (AV     x  ROT*V )
!                                    CMAT
!
!                          (since rotations distribute across cross
!                           products)
!
!                              T                  T
!                        = (ROT * AV    ) x  ( ROT * ROT*V )
!                                   CMAT
!
!                              T
!                        = (ROT * AV    ) x  V
!                                   CMAT
!
!              Thus OMEGA         is the matrix form of the cross
!                        CMAT*ROT
!
!                                      T
!              product operation {( ROT *AV    )   +  AV   } x  .
!                                          CMAT         ROT
!
               call mtxv ( rot,    av,    tmpv )
               av = omega + tmpv

            end if

            found = .true.

            return

         end if

         call cksns ( ctx, handle, descr, segid, sfnd )

      end do

      return
      end associate
      end subroutine ckgpav

      !> Determine the "window" of coverage of a type 01 C-kernel segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of some DAF that is open for reading.
      !>
      !>     ARRBEG     is the beginning address of a type 01 segment
      !>
      !>     ARREND     is the ending address of a type 01 segment.
      !>
      !>     SCLKID     is the ID code of the spacecraft clock associated with
      !>                the object for which the segment contains pointing.
      !>                This is the ID code used by the SCLK conversion
      !>                routines.
      !>
      !>     TOL        is a tolerance value expressed in ticks of the
      !>                spacecraft clock associated with the segment. Before
      !>                each interval is inserted into the coverage window,
      !>                the intervals are expanded by TOL:  the left endpoint
      !>                of each interval is reduced by TOL and the right
      !>                endpoint is increased by TOL.  Any intervals that
      !>                overlap as a result of the expansion are merged.
      !>
      !>                The coverage window returned when TOL > 0 indicates
      !>                the coverage provided by the file to the CK readers
      !>                CKGPAV and CKGP when that value of TOL is passed to
      !>                them as an input.
      !>
      !>     TIMSYS     is a string indicating the time system used in the
      !>                output coverage window.  TIMSYS may have the values:
      !>
      !>                   'SCLK'    Elements of SCHEDL are expressed in
      !>                             encoded SCLK ("ticks"), where the clock
      !>                             is associated with the object designated
      !>                             by IDCODE.
      !>
      !>                   'TDB'     Elements of SCHEDL are expressed as
      !>                             seconds past J2000 TDB.
      !>
      !>                TIMSYS must be consistent with the system used for
      !>                the contents of SCHEDL on input, if any.
      !>
      !>     SCHEDL     is a schedule (window) of intervals, to which the
      !>                intervals of coverage for this segment will be added.
      !>
      !>## Detailed Output
      !>
      !>     SCHEDL     the input schedule updated to include the intervals
      !>                of coverage for this segment. Since type 01 segments,
      !>                don't have interpolation intervals, each epoch
      !>                associated with a pointing instance is treated as a
      !>                singleton interval.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     This routine reads the contents of the file associated with
      !>     HANDLE to locate coverage intervals.
      !>
      !>## Exceptions
      !>
      !>     1) The error SPICE(BADCK1SEGMENT) is signaled if the derived
      !>        segment length from ARRBEG and ARREND does not match
      !>        the possible lengths computed from the segment metadata.
      !>
      !>     2) Routines in the call tree of this routine may signal errors
      !>        if insufficient room in SCHEDL exists or other error
      !>        conditions relating to file access arise.
      !>
      !>     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
      !>         is signaled.
      !>
      !>     5) If a time conversion error occurs, the error will be
      !>         diagnosed by a routine in the call tree of this routine.
      !>C
      !>## Particulars
      !>
      !>     This is a utility routine that determines the intervals of
      !>     coverage for a type 01 C-kernel segment. Since type 01 segments,
      !>     don't have interpolation intervals, each epoch associated with a
      !>     pointing instance is treated as a singleton interval.
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
      !>     N.J. Bachman    (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SUPPORT Version 1.0.0, 03-JAN-2005 (WLT)(NJB)(BVS)
      !>
      !>        Initial version.
      module subroutine zzckcv01 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, schedl )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a C-kernel open for read access
      integer(ip), intent(in) :: arrbeg  !! Beginning DAF address
      integer(ip), intent(in) :: arrend  !! Ending DAF address
      integer(ip), intent(in) :: sclkid  !! ID of SCLK associated with segment.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      type(interval_list), intent(inout) :: schedl  !! The window/schedule to which the segment's coverage is added (PEPPER: an [[interval_list]], not a cell)


      integer(ip), parameter :: bufsiz = 100

      integer(ip), parameter :: qavsiz = 7

      integer(ip), parameter :: qsiz = 4

      real(wp) :: begin
      real(wp) :: buffer ( bufsiz )
      real(wp) :: et
      real(wp) :: finish

      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: nrec
      integer(ip) :: offset
      integer(ip) :: psiz
      integer(ip) :: remain
      integer(ip) :: tbase
      integer(ip) :: navsln
      integer(ip) :: avsln
      integer(ip) :: seglen

      logical :: istdb

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     The coverage window is the set of singleton intervals consisting
!     of the epochs of the pointing records. We'll need to find the
!     epochs.
!
!     First, get the number of records in this segment.
!
      call dafgda ( ctx, handle, arrend, arrend, buffer )

      nrec = int( buffer(1) )

!
!     Determine the size of the pointing packets. This is dependent
!     on whether angular rate data is present in the segment or not.
!     We can determine this with the following computation:
!
!     Assume a record size of 4, i.e. no angular rate data.
!
      navsln = 5 * nrec + ( nrec - 1 ) / 100 + 1

!
!     Assume a record size of 7, i.e. angular rate data.
!
      avsln  = 8 * nrec + ( nrec - 1 ) / 100 + 1

!
!     Compute the actual length of the segment.
!
      seglen = arrend - arrbeg + 1

      if ( seglen == navsln ) then
         psiz = qsiz
      else if ( seglen == avsln ) then
         psiz = qavsiz
      else
         call ctx%setmsg ( 'The requested segment in file # reports a ' &
         //            'length of # d.p. numbers, but the metadata ' &
         //            'in the segment indicates the length must ' &
         //            'either be # (no angular rate data) or # ' &
         //            '(angular rate data). Perhaps the segment ' &
         //            'is not type 1?'                              )
         call ctx%errhan ( '#', handle                                   )
         call ctx%errint ( '#', seglen                                   )
         call ctx%errint ( '#', navsln                                   )
         call ctx%errint ( '#', avsln                                    )
         call ctx%sigerr ( 'SPICE(BADCK1SEGMENT)'                        )
         return
      end if

!
!     The epochs start right after the pointing data. Let TBASE be the
!     address preceding the first epoch.
!
      tbase  = arrbeg  +  nrec * psiz  -  1

!
!     Grab the epochs. Make a singleton interval out of each one; add
!     the interval to the coverage window.
!
!     For efficiency, we'll read the epochs into a buffer of length
!     BUFSIZ.
!
      remain = nrec
      offset = 0

      do while ( remain > 0 )

!
!        Buffer the next set of epochs.
!
         n  =  min ( bufsiz, remain )

         call dafgda ( ctx, handle, &
                       tbase + offset + 1, &
                       tbase + offset + n, &
                       buffer              )

!
!        Insert the current batch of N singleton intervals.
!
         do i = 1, n

            begin   =  buffer(i)
            finish  =  buffer(i)

            if ( tol > 0.0_wp ) then
!
!              Adjust the interval using the tolerance.
!
               begin   =  max( begin  - tol, 0.0_wp )
               finish  =       finish + tol

            end if

!
!           Convert the time to TDB if necessary.
!
            if ( istdb ) then

               call sct2e ( ctx, sclkid, begin,  et )
               begin  = et

               call sct2e ( ctx, sclkid, finish, et )
               finish = et

            end if

            call insert_interval ( ctx, begin, finish, schedl )

         end do

         offset = offset + n
         remain = remain - n

      end do

      return
      end subroutine zzckcv01

      !> Determine the "window" of coverage of a type 02 C-kernel segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of some DAF that is open for reading.
      !>
      !>     ARRBEG     is the beginning address of a type 02 segment
      !>
      !>     ARREND     is the ending address of a type 02 segment.
      !>
      !>     SCLKID     is the ID code of the spacecraft clock associated with
      !>                the object for which the segment contains pointing.
      !>                This is the ID code used by the SCLK conversion
      !>                routines.
      !>
      !>     TOL        is a tolerance value expressed in ticks of the
      !>                spacecraft clock associated with the segment. Before
      !>                each interval is inserted into the coverage window,
      !>                the intervals are expanded by TOL:  the left endpoint
      !>                of each interval is reduced by TOL and the right
      !>                endpoint is increased by TOL.  Any intervals that
      !>                overlap as a result of the expansion are merged.
      !>
      !>                The coverage window returned when TOL > 0 indicates
      !>                the coverage provided by the file to the CK readers
      !>                CKGPAV and CKGP when that value of TOL is passed to
      !>                them as an input.
      !>
      !>     TIMSYS     is a string indicating the time system used in the
      !>                output coverage window.  TIMSYS may have the values:
      !>
      !>                   'SCLK'    Elements of SCHEDL are expressed in
      !>                             encoded SCLK ("ticks"), where the clock
      !>                             is associated with the object designated
      !>                             by IDCODE.
      !>
      !>                   'TDB'     Elements of SCHEDL are expressed as
      !>                             seconds past J2000 TDB.
      !>
      !>                TIMSYS must be consistent with the system used for
      !>                the contents of SCHEDL on input, if any.
      !>
      !>     SCHEDL     is a schedule (window) of intervals, to which the
      !>                intervals of coverage for this segment will be added.
      !>
      !>## Detailed Output
      !>
      !>     SCHEDL     the input schedule updated to include the intervals
      !>                of coverage for this segment.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     This routine reads the contents of the file associated with
      !>     HANDLE to locate coverage intervals.
      !>
      !>## Exceptions
      !>
      !>     1) Routines in the call tree of this routine may signal errors
      !>        if insufficient room in SCHEDL exists or other error
      !>        conditions relating to file access arise.
      !>
      !>     2) If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>        signaled.
      !>
      !>     3) If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
      !>        is signaled.
      !>
      !>     4) If a time conversion error occurs, the error will be
      !>        diagnosed by a routine in the call tree of this routine
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that determines the intervals
      !>     of coverage for a type 02 C-kernel segment.
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
      !>     N.J. Bachman    (JPL)
      !>     W.L. Taber      (JPL)
      !>     F.S. Turner     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 03-JAN-2005 (NJB) (FST) (WLT) (BVS)
      module subroutine zzckcv02 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, schedl )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a C-kernel open for read access
      integer(ip), intent(in) :: arrbeg  !! Beginning DAF address
      integer(ip), intent(in) :: arrend  !! Ending DAF address
      integer(ip), intent(in) :: sclkid  !! ID of SCLK associated with segment.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      type(interval_list), intent(inout) :: schedl  !! The window/schedule to which the segment's coverage is added (PEPPER: an [[interval_list]], not a cell)


      integer(ip), parameter :: psize = 8

      integer(ip), parameter :: bsize = 100

      real(wp) :: begin
      real(wp) :: et
      real(wp) :: finish
      real(wp) :: first ( bsize )
      real(wp) :: last  ( bsize )

      integer(ip) :: arrsiz
      integer(ip) :: begat
      integer(ip) :: endat
      integer(ip) :: get
      integer(ip) :: got
      integer(ip) :: i
      integer(ip) :: nrec

      logical :: istdb

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     Determine the size of the array and the number of records
!     in it.
!
      arrsiz = arrend - arrbeg + 1
      nrec   = nint (  ( 100.0_wp * (real(arrsiz, wp)) + 1.0_wp ) / 1001.0_wp  )

!
!     The variable GOT tells us how many time endpoints we've
!     gotten so far.
!
      got = 0

      do while ( got < nrec )

         get = min ( bsize, nrec - got )

         begat = arrbeg + nrec*psize + got
         endat = arrbeg + nrec*psize + nrec + got

!
!        Retrieve the list next list of windows.
!
         call dafgda ( ctx, handle, begat, begat + get - 1, first )
         call dafgda ( ctx, handle, endat, endat + get - 1, last  )

!
!        Insert the coverage intervals into the schedule.
!
         do i = 1, get
!
!           Adjust the interval using the tolerance.
!
            begin  = first(i)
            finish = last(i)

            if ( tol > 0.0_wp ) then

               begin   =  max( begin  - tol, 0.0_wp )
               finish  =       finish + tol

            end if

!
!           Convert the time to TDB if necessary.
!
            if ( istdb ) then

               call sct2e ( ctx, sclkid, begin,  et )
               begin  = et

               call sct2e ( ctx, sclkid, finish, et )
               finish = et

            end if

            call insert_interval ( ctx, begin, finish, schedl )

         end do

         got = got + get

      end do

      return
      end subroutine zzckcv02

      !> Determine the "window" of coverage of a type 03 C-kernel segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of some DAF that is open for reading.
      !>
      !>     ARRBEG     is the beginning address of a type 03 segment
      !>
      !>     ARREND     is the ending address of a type 03 segment.
      !>
      !>     SCLKID     is the ID code of the spacecraft clock associated with
      !>                the object for which the segment contains pointing.
      !>                This is the ID code used by the SCLK conversion
      !>                routines.
      !>
      !>     TOL        is a tolerance value expressed in ticks of the
      !>                spacecraft clock associated with the segment. Before
      !>                each interval is inserted into the coverage window,
      !>                the intervals are expanded by TOL:  the left endpoint
      !>                of each interval is reduced by TOL and the right
      !>                endpoint is increased by TOL.  Any intervals that
      !>                overlap as a result of the expansion are merged.
      !>
      !>                The coverage window returned when TOL > 0 indicates
      !>                the coverage provided by the file to the CK readers
      !>                CKGPAV and CKGP when that value of TOL is passed to
      !>                them as an input.
      !>
      !>     TIMSYS     is a string indicating the time system used in the
      !>                output coverage window.  TIMSYS may have the values:
      !>
      !>                   'SCLK'    Elements of SCHEDL are expressed in
      !>                             encoded SCLK ("ticks"), where the clock
      !>                             is associated with the object designated
      !>                             by IDCODE.
      !>
      !>                   'TDB'     Elements of SCHEDL are expressed as
      !>                             seconds past J2000 TDB.
      !>
      !>                TIMSYS must be consistent with the system used for
      !>                the contents of SCHEDL on input, if any.
      !>
      !>     SCHEDL     is a schedule (window) of intervals, to which the
      !>                intervals of coverage for this segment will be added.
      !>
      !>## Detailed Output
      !>
      !>     SCHEDL     the input schedule updated to include the intervals
      !>                of coverage for this segment.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     This routine reads the contents of the file associated with
      !>     HANDLE to locate coverage intervals.
      !>
      !>## Exceptions
      !>
      !>     1) The error SPICE(BADCK3SEGMENT) is signaled if the derived
      !>        segment length from ARRBEG and ARREND does not match
      !>        the possible lengths computed from the segment metadata.
      !>
      !>     2) Routines in the call tree of this routine may signal errors
      !>        if insufficient room in SCHEDL exists or other error
      !>        conditions relating to file access arise.
      !>
      !>     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
      !>         is signaled.
      !>
      !>     5) If a time conversion error occurs, the error will be
      !>         diagnosed by a routine in the call tree of this routine.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that determines the intervals
      !>     of coverage for a type 03 C-kernel segment.
      !>
      !>## Examples
      !>
      !>     See CKBRIEF's main driver.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     W.L. Taber      (JPL)
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 03-JAN-2005 (NJB) (FST) (WLT)
      module subroutine zzckcv03 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, schedl )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a C-kernel open for read access
      integer(ip), intent(in) :: arrbeg  !! Beginning DAF address
      integer(ip), intent(in) :: arrend  !! Ending DAF address
      integer(ip), intent(in) :: sclkid  !! ID of SCLK associated with segment.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      type(interval_list), intent(inout) :: schedl  !! The window/schedule to which the segment's coverage is added (PEPPER: an [[interval_list]], not a cell)
      real(wp) :: finish_1(1)  !! one-element copy of finish (array argument)
      real(wp) :: tick_1(1)  !! one-element copy of tick (array argument)
      real(wp) :: start_1(1)  !! one-element copy of start (array argument)


      real(wp) :: begin
      real(wp) :: buffer ( 2 )
      real(wp) :: et
      real(wp) :: finish
      real(wp) :: start
      real(wp) :: tick

      integer(ip) :: intat
      integer(ip) :: intbeg
      integer(ip) :: invls
      integer(ip) :: lstint
      integer(ip) :: lsttik
      integer(ip) :: ndir
      integer(ip) :: nrec
      integer(ip) :: rsize
      integer(ip) :: tickat

      integer(ip) :: navsln
      integer(ip) :: avsln
      integer(ip) :: seglen

      logical :: bail
      logical :: istdb

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     Get the number of intervals and pointing instances ( records )
!     in this segment, and from that determine the number of respective
!     directory epochs.
!
      call dafgda ( ctx, handle, arrend - 1, arrend, buffer )

      invls  = nint ( buffer ( 1 ) )
      nrec   = nint ( buffer ( 2 ) )
      ndir   = (nrec-1)/ 100

!
!     Determine the size of the pointing packets.  This is dependent
!     on whether angular rate data is present in the segment or not.
!     We can determine this with the following computation:
!
!     Assume a record size of 4, i.e. no angular rate data.
!
      navsln = 5*nrec + ndir + invls + (invls-1)/100 + 2

!
!     Assume a record size of 7, i.e. angular rate data.
!
      avsln = 8*nrec + ndir + invls + (invls-1)/100 + 2

!
!     Compute the actual length of the segment.
!
      seglen = arrend - arrbeg + 1

      if ( seglen == navsln ) then
         rsize = 4
      else if ( seglen == avsln ) then
         rsize = 7
      else
         call ctx%setmsg ( 'The requested segment in file # reports a ' &
         //            'length of # d.p. numbers, but the metadata ' &
         //            'in the segment indicates the length must ' &
         //            'either be # (no angular rate data) or # ' &
         //            '(angular rate data). Perhaps the segment ' &
         //            'is not type 3?'                              )
         call ctx%errhan ( '#', handle                                   )
         call ctx%errint ( '#', seglen                                   )
         call ctx%errint ( '#', navsln                                   )
         call ctx%errint ( '#', avsln                                    )
         call ctx%sigerr ( 'SPICE(BADCK3SEGMENT)'                        )
         return
      end if

!
!     Recall that the segment is layed out as:
!
!       +------------------------------+
!       |                              |
!       |  Pointing                    |
!       |                              |
!       +------------------------------+
!       |                        |
!       |  SCLK times            |
!       |                        |
!       +------------------------+
!       |                        |
!       |  SCLK directory        |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Interval start times  |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Start times directory |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Number of intervals   |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Number of pointing    |
!       |      instances         |
!       |                        |
!       +------------------------+
!

      tickat = arrbeg + rsize*nrec
      lsttik = tickat + nrec - 1
      intbeg = arrbeg + rsize*nrec + nrec + ndir
      intat  = intbeg
      lstint = intbeg + invls - 1

      call dafgda ( ctx, handle, intat, intat, start_1 )
      start = start_1(1)
      call dafgda ( ctx, handle, tickat, tickat, tick_1 )
      tick = tick_1(1)

      do while ( tick < start .and. tickat < lsttik )
         tickat = tickat + 1
         call dafgda ( ctx, handle, tickat, tickat, tick_1 )
         tick = tick_1(1)
      end do

!
!     If we did not find a TICK at least as big as START, we can
!     just return now.
!
      if ( tick < start ) return

      bail = .false.

      do while (       ( intat  <=  lstint ) &
                 .and. ( tickat <=  lsttik ) &
                 .and. (        .not. bail   )  )
!
!        At this point, we have an interval that begins at START
!        and ends at FINISH (unless of course we never found a "good"
!        TICK to start with.)
!
         begin  = start

!
!        If the start of the interval was the start of the LAST
!        interval available, we can short cut the remainder of the
!        reads.
!
         if ( intat == lstint ) then

            call dafgda ( ctx, handle, lsttik, lsttik, finish_1 )
            finish = finish_1(1)

            bail = .true.
!
!           The routine will return at the end of this loop
!           iteration.  But first, we may have to update BEGIN
!           and FINISH, depending on the values of TOL and TIMSYS,
!           and we have to insert these values into SCHEDL.
!           We'll carry out these tasks at the end of this IF block.
!

         else

!
!           This is the expected case.  Get the start of the next
!           interval.
!
            intat  = intat  + 1
            call dafgda ( ctx, handle, intat, intat, start_1 )
            start = start_1(1)

!
!           Read forward from the last tick until we reach the
!           START of the next interval or until we run out of TICKS.
!
            do while ( tick < start .and. tickat < lsttik )
               finish = tick
               tickat = tickat + 1
               call dafgda ( ctx, handle, tickat, tickat, tick_1 )
               tick = tick_1(1)
            end do

!
!           A structurally correct CK-3 segment should never allow the
!           next test to pass, but it's just easier to check than
!           police the writers of C-kernels.  The only way to get into
!           the block below is if TICKAT .EQ. LSTTIK
!
            if ( tick < start ) then
               finish = tick
               tickat = tickat + 1
            end if

         end if

!
!        Adjust the interval using the tolerance.
!
         if ( tol > 0.0_wp ) then

            begin   =  max( begin  - tol, 0.0_wp )
            finish  =       finish + tol

         end if

!
!        Convert the time to TDB if necessary.
!
         if ( istdb ) then

            call sct2e ( ctx, sclkid, begin,  et )
            begin  = et

            call sct2e ( ctx, sclkid, finish, et )
            finish = et

         end if

!
!        Insert the interval into the window.
!
         call insert_interval ( ctx, begin, finish, schedl )

      end do

      return
      end subroutine zzckcv03

      !> Determine the "window" of coverage of a type 04 C-kernel segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of some DAF that is open for reading.
      !>
      !>     ARRBEG     is the beginning address of a type 04 segment
      !>
      !>     ARREND     is the ending address of a type 04 segment.
      !>
      !>     SCLKID     is the ID code of the spacecraft clock associated with
      !>                the object for which the segment contains pointing.
      !>                This is the ID code used by the SCLK conversion
      !>                routines.
      !>
      !>     TOL        is a tolerance value expressed in ticks of the
      !>                spacecraft clock associated with the segment. Before
      !>                each interval is inserted into the coverage window,
      !>                the intervals are expanded by TOL:  the left endpoint
      !>                of each interval is reduced by TOL and the right
      !>                endpoint is increased by TOL.  Any intervals that
      !>                overlap as a result of the expansion are merged.
      !>
      !>                The coverage window returned when TOL > 0 indicates
      !>                the coverage provided by the file to the CK readers
      !>                CKGPAV and CKGP when that value of TOL is passed to
      !>                them as an input.
      !>
      !>     TIMSYS     is a string indicating the time system used in the
      !>                output coverage window.  TIMSYS may have the values:
      !>
      !>                   'SCLK'    Elements of SCHEDL are expressed in
      !>                             encoded SCLK ("ticks"), where the clock
      !>                             is associated with the object designated
      !>                             by IDCODE.
      !>
      !>                   'TDB'     Elements of SCHEDL are expressed as
      !>                             seconds past J2000 TDB.
      !>
      !>                TIMSYS must be consistent with the system used for
      !>                the contents of SCHEDL on input, if any.
      !>
      !>     SCHEDL     is a schedule (window) of intervals, to which the
      !>                intervals of coverage for this segment will be added.
      !>
      !>## Detailed Output
      !>
      !>     SCHEDL     the input schedule updated to include the intervals
      !>                of coverage for this segment.
      !>
      !>## Parameters
      !>
      !>     CK4RSZ     is the maximum length of a CK4 record (with angular
      !>                velocity). Defined in the include file 'ckparam.inc'.
      !>
      !>## Files
      !>
      !>     This routine reads the contents of the file associated with
      !>     HANDLE to locate coverage intervals.
      !>
      !>## Exceptions
      !>
      !>     1) Routines in the call tree of this routine may signal errors
      !>        if insufficient room in SCHEDL exists or other error
      !>        conditions relating to file access arise.
      !>
      !>     2) If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>        signaled.
      !>
      !>     3) If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
      !>        is signaled.
      !>
      !>     4) If a time conversion error occurs, the error will be
      !>        diagnosed by a routine in the call tree of this routine.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that determines the intervals
      !>     of coverage for a type 04 C-kernel segment.
      !>
      !>## Examples
      !>
      !>     See CKCOV.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) (FST)
      module subroutine zzckcv04 ( ctx, handle, arrbeg, arrend, sclkid, tol, timsys, schedl )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      use pepper_strings, only: eqstr
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a C-kernel open for read access
      integer(ip), intent(in) :: arrbeg  !! Beginning DAF address
      integer(ip), intent(in) :: arrend  !! Ending DAF address
      integer(ip), intent(in) :: sclkid  !! ID of SCLK associated with segment.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      type(interval_list), intent(inout) :: schedl  !! The window/schedule to which the segment's coverage is added (PEPPER: an [[interval_list]], not a cell)




      real(wp) :: dc     ( nd )
      real(wp) :: descr  ( 5 )
      real(wp) :: et
      real(wp) :: left
      real(wp) :: right
      real(wp) :: values ( ck4rsz )

      integer(ip) :: ends   ( 2 )
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: nrec

      logical :: istdb

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     Build a descriptor record that satisfies the requirements
!     of CKNR04 and SGFPKT.
!
!     Note: This is a hack dependent on the implementation of
!     the generic segments routines.  But for C-kernels it
!     should always work, as ND and NI aren't changing any
!     time soon.
!
      ic ( 1 ) = intmax
      ic ( 2 ) = intmax
      ic ( 3 ) = 4
      ic ( 4 ) = intmax
      ic ( 5 ) = arrbeg
      ic ( 6 ) = arrend

      dc ( 1 ) = 0.0_wp
      dc ( 2 ) = 0.0_wp

      call dafps ( ctx, nd, ni, dc, ic, descr )

!
!     Determine the number of records in the array.
!
      call cknr04 ( ctx, handle, descr, nrec )

      do i = 1, nrec

!
!        Extract each packet of pointing coefficients.
!
         call sgfpkt ( ctx, handle, descr, i, i, values, ends )

!
!        Compute the left and right end points of the interval
!        of coverage related to this packet.
!
         left  = values ( 1 ) - values ( 2 )
         right = values ( 1 ) + values ( 2 )

!
!        Adjust the interval using the tolerance.
!
         if ( tol > 0.0_wp ) then

            left   =  max( left  - tol, 0.0_wp )
            right  =       right + tol

         end if

!
!        Convert the time to TDB if necessary.
!
         if ( istdb ) then

            call sct2e ( ctx, sclkid, left,  et )
            left  = et

            call sct2e ( ctx, sclkid, right, et )
            right = et

         end if

!
!        Store the results in the schedule.
!
         call insert_interval ( ctx, left, right, schedl )

      end do

      return
      end subroutine zzckcv04

      !> Determine the "window" of coverage of a type 05 C-kernel segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of some DAF that is open for reading.
      !>
      !>     ARRBEG     is the beginning address of a type 05 segment
      !>
      !>     ARREND     is the ending address of a type 05 segment.
      !>
      !>     SCLKID     is the ID code of the spacecraft clock associated with
      !>                the object for which the segment contains pointing.
      !>                This is the ID code used by the SCLK conversion
      !>                routines.
      !>
      !>     DC         is the double precision component of the descriptor of
      !>                the CK segment.  The components are the segment start
      !>                and stop times.
      !>
      !>                Each interpolation interval is replaced with its
      !>                intersection with the segment coverage interval
      !>
      !>                   [ DC(1), DC(2) ]
      !>
      !>                before being expanded by TOL. Interpolation intervals
      !>                that don't intersect the segment coverage interval are
      !>                discarded, even if after expansion by TOL they would
      !>                have non-empty intersection with the segment coverage
      !>                interval.
      !>
      !>     TOL        is a tolerance value expressed in ticks of the
      !>                spacecraft clock associated with the segment. After
      !>                truncation by the segment coverage interval, and
      !>                before insertion into the coverage window, each
      !>                non-empty truncated interpolation interval is expanded
      !>                by TOL:  the left endpoint of each interval is reduced
      !>                by TOL and the right endpoint is increased by TOL.
      !>                Any intervals that overlap as a result of the
      !>                expansion are merged.
      !>
      !>                The coverage window returned when TOL > 0 indicates
      !>                the coverage provided by the file to the CK readers
      !>                CKGPAV and CKGP when that value of TOL is passed to
      !>                them as an input.
      !>
      !>     TIMSYS     is a string indicating the time system used in the
      !>                output coverage window.  TIMSYS may have the values:
      !>
      !>                   'SCLK'    Elements of SCHEDL are expressed in
      !>                             encoded SCLK ("ticks"), where the clock
      !>                             is associated with the object designated
      !>                             by IDCODE.
      !>
      !>                   'TDB'     Elements of SCHEDL are expressed as
      !>                             seconds past J2000 TDB.
      !>
      !>                TIMSYS must be consistent with the system used for
      !>                the contents of SCHEDL on input, if any.
      !>
      !>     SCHEDL     is a schedule (window) of intervals, to which the
      !>                intervals of coverage for this segment will be added.
      !>
      !>## Detailed Output
      !>
      !>     SCHEDL     the input schedule updated to include the intervals
      !>                of coverage for this segment.  The schedule has
      !>                been adjusted to account for the provided tolerance
      !>                value.  Coverage lying outside the interval
      !>
      !>                   DC(1) - TOL : DC(2) + TOL
      !>
      !>                is excluded.
      !>
      !>                The elements of SCHEDL are given in the time system
      !>                indicated by TIMSYS.
      !>
      !>## Parameters
      !>
      !>     Several parameters associated with the type 05 C-kernel
      !>     are utilized to compute the packet size of each subtype.
      !>     See the include file 'ck05.inc' for details.
      !>
      !>## Files
      !>
      !>     This routine reads the contents of the file associated with
      !>     HANDLE to locate coverage intervals.
      !>
      !>## Exceptions
      !>
      !>     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
      !>         the CK type 05 segment is not recognized.
      !>
      !>     2)  Routines in the call tree of this routine may signal errors
      !>         if insufficient room in SCHEDL exists or other error
      !>         conditions relating to file access arise.
      !>
      !>     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
      !>         is signaled.
      !>
      !>     5)  If a time conversion error occurs, the error will be
      !>         diagnosed by a routine in the call tree of this routine.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that determines the intervals
      !>     of coverage for a type 05 C-kernel segment.
      !>
      !>## Examples
      !>
      !>     See CKCOV.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     W.L. Taber      (JPL)
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB) (FST) (WLT)
      module subroutine zzckcv05 ( ctx, handle, arrbeg, arrend, sclkid, dc, tol, timsys, schedl )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      use pepper_inc_ck05
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a C-kernel open for read access
      integer(ip), intent(in) :: arrbeg  !! Beginning DAF address
      integer(ip), intent(in) :: arrend  !! Ending DAF address
      integer(ip), intent(in) :: sclkid  !! ID of SCLK associated with segment.
      real(wp), intent(in) :: dc     ( 2 )  !! D.p. component of CK segment descriptor.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      type(interval_list), intent(inout) :: schedl  !! The window/schedule to which the segment's coverage is added (PEPPER: an [[interval_list]], not a cell)
      real(wp) :: finish_1(1)  !! one-element copy of finish (array argument)
      real(wp) :: tick_1(1)  !! one-element copy of tick (array argument)
      real(wp) :: start_1(1)  !! one-element copy of start (array argument)


      real(wp) :: begin
      real(wp) :: buffer ( 4 )
      real(wp) :: et
      real(wp) :: finish
      real(wp) :: start
      real(wp) :: tick

      integer(ip) :: intat
      integer(ip) :: intbeg
      integer(ip) :: invls
      integer(ip) :: lstint
      integer(ip) :: lsttik
      integer(ip) :: ndir
      integer(ip) :: nrec
      integer(ip) :: rsize
      integer(ip) :: subtyp
      integer(ip) :: tickat

      logical :: bail
      logical :: istdb

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time systm is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     Get the meta-data associated with this segment that we
!     require to produce the schedule.
!
!     BUFFER(1) = Subtype Code
!     BUFFER(2) = Window Size
!     BUFFER(3) = Number of Interpolation Intervals
!     BUFFER(4) = Number of Packets
!
      call dafgda ( ctx, handle, arrend - 3, arrend, buffer )

      subtyp = nint ( buffer ( 1 ) )
      invls  = nint ( buffer ( 3 ) )
      nrec   = nint ( buffer ( 4 ) )
      ndir   = (nrec-1)/ 100

!
!     Compute the packet size.  This requires parameters listed
!     in the include file 'ck05.inc' and is based on the subtype.
!
      if      ( subtyp == c05tp0 ) then
         rsize = c05ps0
      else if ( subtyp == c05tp1 ) then
         rsize = c05ps1
      else if ( subtyp == c05tp2 ) then
         rsize = c05ps2
      else if ( subtyp == c05tp3 ) then
         rsize = c05ps3
      else
         call ctx%setmsg ( 'CK type 5 subtype <#> is not supported.' )
!        (PEPPER: SPICELIB passed the DP word BUFFER(1) to ERRINT.)
         call ctx%errint ( '#', subtyp                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                     )
         return
      end if

!
!     Recall that the segment is layed out as:
!
!       +------------------------------+
!       |                              |
!       |  Pointing                    |
!       |                              |
!       +------------------------------+
!       |                        |
!       |  SCLK times            |
!       |                        |
!       +------------------------+
!       |                        |
!       |  SCLK directory        |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Interval start times  |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Start times directory |
!       |                        |
!       +------------------------+
!       |    Seconds per tick    |
!       +------------------------+
!       |      Subtype code      |
!       +------------------------+
!       |      Window size       |
!       +------------------------+
!       |                        |
!       |  Number of intervals   |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Number of pointing    |
!       |      instances         |
!       |                        |
!       +------------------------+
!

      tickat = arrbeg + rsize*nrec
      lsttik = tickat + nrec - 1
      intbeg = arrbeg + rsize*nrec + nrec + ndir
      intat  = intbeg
      lstint = intbeg + invls - 1

      call dafgda ( ctx, handle, intat, intat, start_1 )
      start = start_1(1)
      call dafgda ( ctx, handle, tickat, tickat, tick_1 )
      tick = tick_1(1)

      do while ( tick < start .and. tickat < lsttik )
         tickat = tickat + 1
         call dafgda ( ctx, handle, tickat, tickat, tick_1 )
         tick = tick_1(1)
      end do

!
!     If we did not find a TICK at least as big as START, we can
!     just return now.
!
      if ( tick < start ) return

      bail = .false.

      do while (       ( intat  <=  lstint ) &
                 .and. ( tickat <=  lsttik ) &
                 .and. (        .not. bail   )  )
!
!        At this point, we have an interval that begins at START
!        and ends at FINISH (unless of course we never found a "good"
!        TICK to start with.)
!
         begin = start

!
!        If the start of the interval was the start of the LAST
!        interval available, we can short cut the remainder of the
!        reads.
!
         if ( intat == lstint ) then

            call dafgda ( ctx, handle, lsttik, lsttik, finish_1 )
            finish = finish_1(1)

            bail = .true.
!
!           The routine will return at the end of this loop
!           iteration.  But first, we may have to update BEGIN
!           and FINISH, depending on the values of TOL and TIMSYS,
!           and we have to insert these values into SCHEDL.
!           We'll carry out these tasks at the end of this IF block.

         else
!
!           This is the expected case.  Get the start of the next
!           interval.
!
            intat  = intat  + 1
            call dafgda ( ctx, handle, intat, intat, start_1 )
            start = start_1(1)

!
!           Read forward from the last tick until we reach the
!           START of the next interval or until we run out of TICKS.
!
            do while ( tick < start .and. tickat < lsttik )
               finish = tick
               tickat = tickat + 1
               call dafgda ( ctx, handle, tickat, tickat, tick_1 )
               tick = tick_1(1)
            end do

!
!           A structurally correct CK-5 segment should never allow the
!           next test to pass, but it's just easier to check than
!           police the writers of C-kernels.  The only way to get into
!           the block below is if TICKAT .EQ. LSTTIK
!
            if ( tick < start ) then
               finish = tick
               tickat = tickat + 1
            end if

         end if

!
!        Truncate the interval using the segment bounds.
!
         begin   =  max ( begin,  dc(1) )
         finish  =  min ( finish, dc(2) )

!
!        Adjust the interval using the tolerance.  Empty
!        intervals *do not get expanded*; this choice is
!        consistent with the type 5 reading algorithm.
!
         if ( begin <= finish ) then

            if ( tol > 0.0_wp ) then

               begin   =  max( begin  - tol, 0.0_wp )
               finish  =       finish + tol

            end if

         end if

!
!        Convert the time to TDB if necessary.
!
         if ( istdb ) then

            call sct2e ( ctx, sclkid, begin,  et )
            begin  = et

            call sct2e ( ctx, sclkid, finish, et )
            finish = et

         end if

!
!        Insert the interval into the window.
!
         if ( begin <= finish ) then
            call insert_interval ( ctx, begin, finish, schedl )
         end if

      end do

      return
      end subroutine zzckcv05

      !> Determine the "window" of coverage of a type 06 C-kernel segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle of some DAF that is open for reading.
      !>
      !>     ARRBEG     is the beginning address of the type 06 segment
      !>                to be examined.
      !>
      !>     ARREND     is the ending address of the type 06 segment.
      !>
      !>     SCLKID     is the ID code of the spacecraft clock associated with
      !>                the object for which the segment contains pointing.
      !>                This is the ID code used by the SCLK conversion
      !>                routines.
      !>
      !>     DC         is the double precision component of the descriptor of
      !>                the CK segment. The components are the segment start
      !>                and stop times.
      !>
      !>                Each mini-segment interval is replaced with its
      !>                intersection with the segment coverage interval
      !>
      !>                   [ DC(1), DC(2) ]
      !>
      !>                before being expanded by TOL. Mini-segment intervals
      !>                that don't intersect the segment coverage interval are
      !>                discarded, even if after expansion by TOL they would
      !>                have non-empty intersection with the segment coverage
      !>                interval.
      !>
      !>     TOL        is a tolerance value expressed in ticks of the
      !>                spacecraft clock associated with the segment. After
      !>                truncation by the segment coverage interval, and
      !>                before insertion into the coverage window, each
      !>                non-empty truncated mini-segment interval is expanded
      !>                by TOL:  the left endpoint of each interval is reduced
      !>                by TOL and the right endpoint is increased by TOL.
      !>                Any intervals that overlap as a result of the
      !>                expansion are merged.
      !>
      !>                The coverage window returned when TOL > 0 indicates
      !>                the coverage provided by the file to the CK readers
      !>                CKGPAV and CKGP when that value of TOL is passed to
      !>                them as an input.
      !>
      !>     TIMSYS     is a string indicating the time system used in the
      !>                output coverage window.TIMSYS may have the values:
      !>
      !>                   'SCLK'    Elements of SCHEDL are expressed in
      !>                             encoded SCLK ("ticks"), where the clock
      !>                             is associated with the object designated
      !>                             by IDCODE.
      !>
      !>                   'TDB'     Elements of SCHEDL are expressed as
      !>                             seconds past J2000 TDB.
      !>
      !>                TIMSYS must be consistent with the system used for
      !>                the contents of SCHEDL on input, if any.
      !>
      !>     SCHEDL     is a schedule (window) of intervals, to which the
      !>                intervals of coverage for this segment will be added.
      !>
      !>## Detailed Output
      !>
      !>     SCHEDL     the input schedule updated to include the intervals
      !>                of coverage for this segment. The schedule has
      !>                been adjusted to account for the provided tolerance
      !>                value. Coverage lying outside the interval
      !>
      !>                   DC(1) - TOL : DC(2) + TOL
      !>
      !>                is excluded.
      !>
      !>                The elements of SCHEDL are given in the time system
      !>                indicated by TIMSYS.
      !>
      !>## Parameters
      !>
      !>     Several parameters associated with the type 06 C-kernel
      !>     are utilized to compute the packet size of each subtype.
      !>     See the include file 'ck06.inc' for details.
      !>
      !>## Exceptions
      !>
      !>     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
      !>         the CK type 06 segment is not recognized.
      !>
      !>     2)  Routines in the call tree of this routine may signal errors
      !>         if insufficient room in SCHEDL exists or other error
      !>         conditions relating to file access arise.
      !>
      !>     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
      !>         is signaled.
      !>
      !>     5)  If a time conversion error occurs, the error will be
      !>         diagnosed by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     This routine reads the contents of the file associated with
      !>     HANDLE to locate coverage intervals.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that determines the intervals
      !>     of coverage for a type 06 C-kernel segment.
      !>
      !>## Examples
      !>
      !>     See CKCOV.
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
      !>     B.V. Semenov    (JPL)
      !>     W.L. Taber      (JPL)
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) (FST) (WLT)
      module subroutine zzckcv06 ( ctx, handle, arrbeg, arrend, sclkid, dc, tol, timsys, schedl )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      use pepper_inc_ck06
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a C-kernel open for read access.
      integer(ip), intent(in) :: arrbeg  !! Beginning DAF address.
      integer(ip), intent(in) :: arrend  !! Ending DAF address.
      integer(ip), intent(in) :: sclkid  !! ID of SCLK associated with segment.
      real(wp), intent(in) :: dc     ( 2 )  !! D.p. component of CK segment descriptor.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      type(interval_list), intent(inout) :: schedl  !! The window/schedule to which the segment's coverage is added (PEPPER: an [[interval_list]], not a cell)
      real(wp) :: lstepc_1(1)  !! one-element copy of lstepc (array argument)


!
!     Number of elements in a type 6 mini-segment
!     interval directory:
!
      integer(ip), parameter :: nvdsiz = 100

!
!     Mini-segment epoch directory size:
!
      integer(ip), parameter :: dirsiz = 100

!
!     Type 6 control area size:
!
      integer(ip), parameter :: nsgpar = 2

!
!     Type 6 mini-segment control area size:
!
      integer(ip), parameter :: mctlsz = 4

      real(wp) :: begin
      real(wp) :: buffer ( mctlsz )
      real(wp) :: et
      real(wp) :: finish
      real(wp) :: ivlbds ( 2 )
      real(wp) :: lstepc

      integer(ip) :: epaddr
      integer(ip) :: i
      integer(ip) :: ivlbas
      integer(ip) :: minie
      integer(ip) :: ndir
      integer(ip) :: nintvl
      integer(ip) :: nivdir
      integer(ip) :: nrec
      integer(ip) :: ptrbas

      logical :: istdb

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time system is SCLK.
!
      istdb = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     Fetch the mini-segment count from the segment.
!
      call dafgda ( ctx, handle, arrend, arrend, buffer )

      nintvl = nint( buffer(1) )

!
!     Each mini-segment contributes a coverage interval to the
!     total coverage of the segment. Since mini-segments can
!     contain gaps, we need to examine not only the mini-segment
!     interval bounds but the final epochs of the mini-segments.
!
!     Let IVLBAS be the base address of the mini-segment interval
!     bounds. Let PTRBAS be the base address of the mini-segment
!     pointers.
!
!     First compute PTRBAS. There are NINTVL+1 pointers.
!
      ptrbas = arrend - nsgpar - ( nintvl + 1 )

!
!     Compute the number of mini-segment interval directories.
!     There are NINTVL + 1 interval boundaries, so the directory
!     count is
!
!        (  ( NINTVL + 1 ) - 1  )  /  NVDSIZ
!
      nivdir = nintvl / nvdsiz

!
!     The interval bounds and their directories precede the
!     mini-segment pointers.
!
      ivlbas = ptrbas - nivdir - ( nintvl + 1 )

!
!     Now loop over the mini-segments and find the contribution
!     from each one.
!
      do i = 1,  nintvl
!
!        Find the interval bounds for this mini-segment.
!
         call dafgda ( ctx, handle, ivlbas+i, ivlbas+i+1, ivlbds )

         if ( ctx%failed() ) return

!
!        Now find the last epoch of this mini-segment, since
!        there could be a gap at the end.
!
!        Find the begin and end pointers for the current
!        mini-segment. Convert these from relative to
!        absolute DAF addresses.
!
         call dafgda ( ctx, handle, ptrbas+i, ptrbas+i+1, buffer )

         if ( ctx%failed() ) return

         minie = ( arrbeg - 1 )  +  nint( buffer(2) )  -  1

!
!        Fetch the mini-segment's record count NREC.
!
         call dafgda ( ctx, handle, minie, minie, buffer )

         if ( ctx%failed() ) return

         nrec = nint( buffer(1) )

!
!        Compute the number of epoch directories for this
!        mini-segment.
!
         ndir = ( nrec - 1 ) / dirsiz

!
!        The last epoch precedes the mini-segment control
!        area and the epoch directories.
!
         epaddr = minie - mctlsz - ndir

         call dafgda ( ctx, handle, epaddr, epaddr, lstepc_1 )
         lstepc = lstepc_1(1)

         if ( ctx%failed() ) return

         begin  = ivlbds(1)
!
!        The smaller of LSTEPC and IVLBDS(2) is the
!        end of the mini-segment's coverage.
!
         finish = min ( lstepc, ivlbds(2) )
!
!        Truncate the interval using the segment bounds.
!
         begin   =  max ( begin,  dc(1) )
         finish  =  min ( finish, dc(2) )

!
!        Adjust the interval using the tolerance. Empty
!        intervals *do not get expanded*; this choice is
!        consistent with the type 6 reading algorithm.
!
         if ( begin <= finish ) then

            if ( tol > 0.0_wp ) then

               begin   =  max( begin  - tol, 0.0_wp )
               finish  =       finish + tol

            end if

         end if

!
!        Convert the time to TDB if necessary.
!
         if ( istdb ) then

            call sct2e ( ctx, sclkid, begin,  et )
            begin  = et

            call sct2e ( ctx, sclkid, finish, et )
            finish = et

            if ( ctx%failed() ) return

         end if

!
!        Insert the interval into the window.
!
         if ( begin <= finish ) then

            call insert_interval ( ctx, begin, finish, schedl )

            if ( ctx%failed() ) return

         end if

      end do

      return
      end subroutine zzckcv06

      !> Find the coverage window for a specified object in a specified CK
      !> file.
      !>
      !>## Detailed Input
      !>
      !>     CKFNM    is the name of a C-kernel.
      !>
      !>     IDCODE   is the integer ID code of an object, normally a
      !>              spacecraft structure or instrument, for which pointing
      !>              data are expected to exist in the specified CK file.
      !>
      !>     NEEDAV   is a logical variable indicating whether only segments
      !>              having angular velocity are to be considered when
      !>              determining coverage. When NEEDAV is .TRUE., segments
      !>              without angular velocity don't contribute to the coverage
      !>              window; when NEEDAV is .FALSE., all segments for IDCODE
      !>              may contribute to the coverage window.
      !>
      !>     LEVEL    is the level (granularity) at which the coverage is
      !>              examined. Allowed values and corresponding meanings are:
      !>
      !>                 'SEGMENT'    The output coverage window contains
      !>                              intervals defined by the start and stop
      !>                              times of segments for the object
      !>                              designated by IDCODE.
      !>
      !>                 'INTERVAL'   The output coverage window contains
      !>                              interpolation intervals of segments for
      !>                              the object designated by IDCODE. For type
      !>                              1 segments, which don't have
      !>                              interpolation intervals, each epoch
      !>                              associated with a pointing instance is
      !>                              treated as a singleton interval; these
      !>                              intervals are added to the coverage
      !>                              window.
      !>
      !>                              All interpolation intervals are
      !>                              considered to lie within the segment
      !>                              bounds for the purpose of this summary:
      !>                              if an interpolation interval extends
      !>                              beyond the segment coverage interval,
      !>                              only its intersection with the segment
      !>                              coverage interval is considered to
      !>                              contribute to the total coverage.
      !>
      !>     TOL      is a tolerance value expressed in ticks of the spacecraft
      !>              clock associated with IDCODE. Before each interval is
      !>              inserted into the coverage window, the interval is
      !>              intersected with the segment coverage interval, then if
      !>              the intersection is non-empty, it is expanded by TOL: the
      !>              left endpoint of the intersection interval is reduced by
      !>              TOL and the right endpoint is increased by TOL. Adjusted
      !>              interval endpoints, when expressed as encoded SCLK, never
      !>              are less than zero ticks. Any intervals that overlap as a
      !>              result of the expansion are merged.
      !>
      !>              The coverage window returned when TOL > 0 indicates the
      !>              coverage provided by the file to the CK readers CKGPAV
      !>              and CKGP when that value of TOL is passed to them as an
      !>              input.
      !>
      !>     TIMSYS   is a string indicating the time system used in the output
      !>              coverage window. TIMSYS may have the values:
      !>
      !>                  'SCLK'    Elements of COVER are expressed in encoded
      !>                            SCLK ("ticks"), where the clock is
      !>                            associated with the object designated by
      !>                            IDCODE.
      !>
      !>                  'TDB'     Elements of COVER are expressed as seconds
      !>                            past J2000 TDB.
      !>
      !>     COVER    is an initialized SPICE window data structure. COVER
      !>              optionally may contain coverage data on input; on output,
      !>              the data already present in COVER will be combined with
      !>              coverage found for the object designated by IDCODE in the
      !>              file CKFNM.
      !>
      !>              If COVER contains no data on input, its size and
      !>              cardinality still must be initialized.
      !>
      !>## Detailed Output
      !>
      !>     COVER    is a SPICE window data structure which represents the
      !>              merged coverage for IDCODE. When the coverage level is
      !>              'INTERVAL', this is the set of time intervals for which
      !>              data for IDCODE are present in the file CKFNM, merged
      !>              with the set of time intervals present in COVER on input.
      !>              The merged coverage is represented as the union of one or
      !>              more disjoint time intervals. The window COVER contains
      !>              the pairs of endpoints of these intervals.
      !>
      !>              When the coverage level is 'SEGMENT', COVER is computed
      !>              in a manner similar to that described above, but the
      !>              coverage intervals used in the computation are those of
      !>              segments rather than interpolation intervals within
      !>              segments.
      !>
      !>              When TOL is > 0, the intervals comprising the coverage
      !>              window for IDCODE are expanded by TOL and any intervals
      !>              overlapping as a result are merged. The resulting window
      !>              is returned in COVER. The expanded window in no case
      !>              extends beyond the segment bounds in either direction by
      !>              more than TOL.
      !>
      !>              The interval endpoints contained in COVER are encoded
      !>              spacecraft clock times if TIMSYS is 'SCLK'; otherwise the
      !>              times are converted from encoded spacecraft clock to
      !>              seconds past J2000 TDB.
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
      !>     3)  If the input file is a binary DAF file of type other than CK,
      !>         the error SPICE(INVALIDFILETYPE) is signaled.
      !>
      !>     4)  If the CK file cannot be opened or read, an error is signaled
      !>         by a routine in the call tree of this routine. The output
      !>         window will not be modified.
      !>
      !>     5)  If the size of the output window argument COVER is
      !>         insufficient to contain the actual number of intervals in the
      !>         coverage window for IDCODE, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     6)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>     7)  If LEVEL is not recognized, the error SPICE(INVALIDOPTION)
      !>         is signaled.
      !>
      !>     8)  If TIMSYS is not recognized, the error SPICE(NOTSUPPORTED)
      !>         is signaled.
      !>
      !>     9)  If a time conversion error occurs, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     10) If the output time system is TDB, the CK subsystem must be
      !>         able to map IDCODE to the ID code of the associated spacecraft
      !>         clock. If this mapping cannot be performed, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     11) If the input CK type is not one of the supported CK types, the
      !>         error SPICE(NOTSUPPORTED) is signaled. This problem may
      !>         indicate the version of the SPICE Toolkit being used is
      !>         outdated and a new version is required.
      !>
      !>## Files
      !>
      !>     This routine reads a C-kernel.
      !>
      !>     If the output time system is 'TDB', then a leapseconds kernel
      !>     and an SCLK kernel for the spacecraft clock associated with
      !>     IDCODE must be loaded before this routine is called.
      !>
      !>     If the ID code of the clock associated with IDCODE is not
      !>     equal to
      !>
      !>        IDCODE / 1000
      !>
      !>     then the kernel variable
      !>
      !>        CK_<IDCODE>_SCLK
      !>
      !>     must be present in the kernel pool to identify the clock
      !>     associated with IDCODE. This variable must contain the ID code
      !>     to be used for conversion between SCLK and TDB. Normally this
      !>     variable is provided in a text kernel loaded via FURNSH.
      !>
      !>## Particulars
      !>
      !>     This routine provides an API via which applications can determine
      !>     the coverage a specified CK file provides for a specified
      !>     object.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Display the interval-level coverage for each object in a
      !>        specified CK file. Use tolerance of zero ticks. Do not
      !>        request angular velocity. Express the results in the TDB time
      !>        system.
      !>
      !>        Find the set of objects in the file. Loop over the contents
      !>        of the ID code set: find the coverage for each item in the
      !>        set and display the coverage.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKCOV_EX1
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
      !>      PARAMETER           ( MAXIV  = 100000 )
      !>
      !>      INTEGER               WINSIZ
      !>      PARAMETER           ( WINSIZ = 2 * MAXIV )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 50 )
      !>
      !>      INTEGER               MAXOBJ
      !>      PARAMETER           ( MAXOBJ = 1000 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    CKFNM
      !>      CHARACTER*(FILSIZ)    LSK
      !>      CHARACTER*(FILSIZ)    SCLK
      !>      CHARACTER*(TIMLEN)    TIMSTR
      !>
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
      !>      DOUBLE PRECISION      E
      !>
      !>      INTEGER               I
      !>      INTEGER               IDS   ( LBCELL : MAXOBJ )
      !>      INTEGER               J
      !>      INTEGER               NIV
      !>
      !>C
      !>C     Load a leapseconds kernel and SCLK kernel for output
      !>C     time conversion.  Note that we assume a single spacecraft
      !>C     clock is associated with all of the objects in the CK.
      !>C
      !>      CALL PROMPT ( 'Name of leapseconds kernel > ', LSK  )
      !>      CALL FURNSH ( LSK )
      !>
      !>      CALL PROMPT ( 'Name of SCLK kernel        > ', SCLK )
      !>      CALL FURNSH ( SCLK )
      !>
      !>C
      !>C     Get name of CK file.
      !>C
      !>      CALL PROMPT ( 'Name of CK file            > ', CKFNM )
      !>
      !>C
      !>C     Initialize the set IDS.
      !>C
      !>      CALL SSIZEI ( MAXOBJ, IDS )
      !>
      !>C
      !>C     Initialize the window COVER.
      !>C
      !>      CALL SSIZED ( WINSIZ, COVER )
      !>
      !>C
      !>C     Find the set of objects in the CK file.
      !>C
      !>      CALL CKOBJ ( CKFNM, IDS )
      !>
      !>C
      !>C     We want to display the coverage for each object.  Loop
      !>C     over the contents of the ID code set, find the coverage
      !>C     for each item in the set, and display the coverage.
      !>C
      !>      DO I = 1, CARDI( IDS )
      !>C
      !>C        Find the coverage window for the current
      !>C        object. Empty the coverage window each time
      !>C        so we don't include data for the previous object.
      !>C
      !>         CALL SCARDD ( 0,   COVER )
      !>         CALL CKCOV  ( CKFNM,       IDS(I),  .FALSE.,
      !>     .                 'INTERVAL',  0.D0,    'TDB',    COVER )
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
      !>         WRITE (*,*) 'Coverage for object ', IDS(I)
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
      !>     .                    'YYYY MON DD HR:MN:SC.###### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                           )
      !>            WRITE (*,*) ' '
      !>            WRITE (*,*) 'Interval: ', J
      !>            WRITE (*,*) 'Start:    ', TIMSTR
      !>
      !>            CALL TIMOUT ( E,
      !>     .                    'YYYY MON DD HR:MN:SC.###### ' //
      !>     .                    '(TDB) ::TDB',
      !>     .                    TIMSTR                          )
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
      !>        platform, using the LSK file named naif0010.tls, the SCLK file
      !>        named cas00145.tsc and the CK file named 08052_08057ra.bc, the
      !>        output was:
      !>
      !>        Name of leapseconds kernel > naif0010.tls
      !>        Name of SCLK kernel        > cas00145.tsc
      !>        Name of CK file            > 08052_08057ra.bc
      !>         ========================================
      !>         Coverage for object       -82000
      !>
      !>         Interval:            1
      !>         Start:    2008 FEB 21 00:01:07.771186 (TDB)
      !>         Stop:     2008 FEB 23 22:53:30.001738 (TDB)
      !>
      !>         Interval:            2
      !>         Start:    2008 FEB 23 22:58:13.999732 (TDB)
      !>         Stop:     2008 FEB 24 02:22:25.913175 (TDB)
      !>
      !>         Interval:            3
      !>         Start:    2008 FEB 24 02:27:49.910886 (TDB)
      !>         Stop:     2008 FEB 24 19:46:33.470587 (TDB)
      !>
      !>         Interval:            4
      !>         Start:    2008 FEB 24 19:49:33.469315 (TDB)
      !>         Stop:     2008 FEB 25 04:25:21.250677 (TDB)
      !>
      !>         Interval:            5
      !>         Start:    2008 FEB 25 04:29:33.248897 (TDB)
      !>         Stop:     2008 FEB 25 15:23:44.971594 (TDB)
      !>
      !>         Interval:            6
      !>         Start:    2008 FEB 25 15:24:12.971396 (TDB)
      !>         Stop:     2008 FEB 25 20:25:04.843864 (TDB)
      !>
      !>         Interval:            7
      !>         Start:    2008 FEB 25 20:25:48.843553 (TDB)
      !>         Stop:     2008 FEB 26 00:01:04.752306 (TDB)
      !>
      !>         ========================================
      !>
      !>     2) Find the segment-level coverage for the object designated by
      !>        IDCODE provided by the set of CK files loaded via a
      !>        metakernel. (The metakernel must also specify leapseconds and
      !>        SCLK kernels.) Use tolerance of zero ticks. Do not request
      !>        angular velocity. Express the results in the TDB time system.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ckcov_ex2.tm
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
      !>             File name                      Contents
      !>             ---------                      --------
      !>             naif0010.tls                   Leapseconds
      !>             cas00145.tsc                   Cassini SCLK
      !>             08052_08057ra.bc               Orientation for Cassini
      !>
      !>           \begindata
      !>
      !>             KERNELS_TO_LOAD = ( 'naif0010.tls'
      !>                                 'cas00145.tsc'
      !>                                 '08052_08057ra.bc')
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKCOV_EX2
      !>      IMPLICIT NONE
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
      !>C     The metakernel lists the CK files whose coverage
      !>C     for IDCODE we'd like to determine.  The metakernel
      !>C     must also specify a leapseconds kernel and an SCLK
      !>C     kernel for the clock associated with IDCODE.
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
      !>C     kernels:  for each loaded CK file, add its coverage
      !>C     for IDCODE, if any, to the coverage window.
      !>C
      !>      CALL KTOTAL ( 'CK', COUNT )
      !>
      !>      DO I = 1, COUNT
      !>
      !>         CALL KDATA ( I,       'CK',    FILE,  TYPE,
      !>     .                SOURCE,  HANDLE,  FOUND       )
      !>
      !>         CALL CKCOV ( FILE,      IDCODE,  .FALSE.,
      !>     .                'SEGMENT', 0.D0,    'TDB',    COVER )
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
      !>      WRITE (*,*) 'Coverage for object ', IDCODE
      !>
      !>C
      !>C     Convert the coverage interval start and stop
      !>C     times to TDB calendar strings.
      !>C
      !>      DO I = 1, NIV
      !>C
      !>C        Get the endpoints of the Ith interval.
      !>C
      !>         CALL WNFETD ( COVER, I, B, E )
      !>C
      !>C        Convert the endpoints to TDB calendar
      !>C        format time strings and display them.
      !>C
      !>         CALL TIMOUT ( B,
      !>     .                 'YYYY MON DD HR:MN:SC.###### ' //
      !>     .                 '(TDB) ::TDB',
      !>     .                 TIMSTR                           )
      !>         WRITE (*,*) ' '
      !>         WRITE (*,*) 'Interval: ', I
      !>         WRITE (*,*) 'Start:    ', TIMSTR
      !>
      !>         CALL TIMOUT ( E,
      !>     .                 'YYYY MON DD HR:MN:SC.###### ' //
      !>     .                 '(TDB) ::TDB',
      !>     .                 TIMSTR                           )
      !>         WRITE (*,*) 'Stop:     ', TIMSTR
      !>         WRITE (*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the meta-kernel file named ckcov_ex2.tm and
      !>        the NAIF ID "-82000" (Cassini spacecraft bus), the output was:
      !>
      !>        Enter name of metakernel > ckcov_ex2.tm
      !>        Enter ID code            > -82000
      !>
      !>         Coverage for object       -82000
      !>
      !>         Interval:            1
      !>         Start:    2008 FEB 21 00:01:07.771186 (TDB)
      !>         Stop:     2008 FEB 26 00:01:04.752306 (TDB)
      !>
      !>## Restrictions
      !>
      !>     1)  When this routine is used to accumulate coverage for IDCODE
      !>         provided by multiple CK files, the inputs NEEDAV, LEVEL, TOL,
      !>         and TIMSYS  must have the same values for all files in order
      !>         for the result to be meaningful.
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
      !>-    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR)
      !>
      !>        Bug fix: added call to FAILED after call to GETFAT.
      !>
      !>        Changed input argument name "CK" to "CKFNM" for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard. Added solutions
      !>        using CASSINI data. Fixed a bug on Example #2. Added entry #11
      !>        in $Exceptions section and corrected short error messages in
      !>        entry #2 and #3.
      !>
      !>-    SPICELIB Version 2.0.0, 05-JAN-2014 (NJB) (BVS)
      !>
      !>        Updated index entries.
      !>
      !>        Last update was 05-JAN-2014 (NJB) (BVS)
      !>
      !>           Updated to support type 6.
      !>
      !>-    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB)
      !>
      !>        Corrected bug in first program in header $Examples section:
      !>        program now empties the coverage window prior to collecting
      !>        data for the current object. Updated examples to use WNCARD
      !>        rather than CARDD.
      !>
      !>-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB)
      module subroutine ckcov ( ctx, ckfnm, idcode, needav, level, tol, timsys, cover )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: ckfnm  !! Name of CK file.
      integer(ip), intent(in) :: idcode  !! ID code of object.
      logical, intent(in) :: needav  !! Flag indicating whether angular velocity is needed.
      character(*), intent(in) :: level  !! Coverage level: 'SEGMENT' OR 'INTERVAL'.
      real(wp), intent(in) :: tol  !! Tolerance in ticks.
      character(*), intent(in) :: timsys  !! Time system used to represent coverage.
      real(wp), intent(inout) :: cover ( lbcell : * )  !! Window giving coverage for IDCODE.
      real(wp), allocatable :: intervals(:,:)
      integer(ip) :: k
!
!     PEPPER: the coverage is found by CK_COVERAGE; here it is added
!     to COVER (the union of the two).
!
      if ( ctx%failed() ) return
      call ck_coverage ( ctx, ckfnm, idcode, needav, level, tol, timsys, intervals )
      do k = 1, size ( intervals, 2 )
         if ( ctx%failed() ) return
         call wninsd ( ctx, intervals(1,k), intervals(2,k), cover )
      end do
      end subroutine ckcov

      !> The coverage of an object in a CK file (PEPPER; [[ckcov]] without
      !> a window cell).
      !>
      !> The result is a window as an array: each column an interval
      !> [start, stop] (TDB seconds past J2000), sorted and disjoint
      !> ([[pepper_windows]]). The errors are those of [[ckcov]],
      !> without SPICE(WINDOWEXCESS). With TIMSYS = 'SCLK' the times are
      !> encoded spacecraft clock ticks.
      module subroutine ck_coverage ( ctx, file, inst, needav, level, tol, timsys, intervals )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the CK file
      integer(ip), intent(in) :: inst  !! the ID code of the object (instrument or structure)
      logical, intent(in) :: needav  !! only the coverage with angular velocity
      character(*), intent(in) :: level  !! 'SEGMENT' or 'INTERVAL'
      real(wp), intent(in) :: tol  !! the tolerance in ticks by which intervals are expanded
      character(*), intent(in) :: timsys  !! the time system of the result: 'TDB' or 'SCLK'
      real(wp), allocatable, intent(out) :: intervals(:,:)  !! the coverage, shape (2, n) (n = 0 after an error)
      type(interval_list) :: window  ! the coverage being built


      character(*), parameter :: kwintv = 'INTERVAL'

      character(*), parameter :: kwseg = 'SEGMENT'




      character(lnsize) :: arch
      character(lnsize) :: kertyp

      real(wp) :: dc     ( nd )
      real(wp) :: dctol  ( nd )
      real(wp) :: descr  ( nd + (ni+1)/2 )
      real(wp) :: et

      integer(ip) :: clkid
      integer(ip) :: dtype
      integer(ip) :: handle
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: segbeg
      integer(ip) :: segend

      logical :: avok
      logical :: found
      logical :: istdb
      logical :: seglvl

!
!     Standard SPICE error handling.
!
      allocate ( intervals(2,0) )
      if ( ctx%failed() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call ctx%errdp  ( '#',  tol                                      )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Use a logical flag to indicate whether this is a segment-level
!     coverage description.
!
      seglvl = eqstr( level, kwseg )

!
!     Check coverage level keyword.
!
      if (  .not. ( seglvl .or. eqstr( level, kwintv ) )  ) then

         call ctx%setmsg ( 'Allowed values of LEVEL are # and #; actual '// &
                       'value was #.'                                )
         call ctx%errch  ( '#',  kwseg                                   )
         call ctx%errch  ( '#',  kwintv                                  )
         call ctx%errch  ( '#',  level                                   )
         call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                        )
         return

      end if

!
!     See whether GETFAT thinks we've got a CK file.
!
!
!     PEPPER: a kernel built in memory: the coverage of its rotation tables
!     (their time tags are ET, which are also their ticks: TDB and SCLK
!     give the same values).
!
      block
         real(wp), allocatable :: tb(:), te(:)
         logical :: is_kernel
         call kernel_frame_tables ( ctx, file, inst, is_kernel, tb, te )
         if ( ctx%failed() ) return
         if ( is_kernel ) then
            do i = 1, size ( tb )
               call insert_interval ( ctx, tb(i) - tol, te(i) + tol, window )
            end do
            if ( .not. ctx%failed() ) call window%window ( intervals )
            return
         end if
      end block
      call getfat ( ctx, file, arch, kertyp )

      if ( ctx%failed() ) return

      if ( arch == 'XFR' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine.  If the input file is an ' &
         //            'CK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         call ctx%errch  ( '#',  file                                 )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                      )
         return

      else if ( arch /= 'DAF' ) then

         call ctx%setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine.  Binary CK files have ' &
         //            'DAF architecture.  If you expected the ' &
         //            'file to be a binary CK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                 )
         call ctx%errch  ( '#',  arch                                  )
         call ctx%sigerr ( 'SPICE(INVALIDARCHTYPE)'                    )
         return

      else if ( kertyp /= 'CK' ) then

         call ctx%setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary CK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary CK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call ctx%errch  ( '#',  file                                 )
         call ctx%errch  ( '#',  kertyp                                )
         call ctx%sigerr ( 'SPICE(INVALIDFILETYPE)'                    )
         return

      end if

!
!     Set a logical flag indicating whether the time system is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call ctx%setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call ctx%errch  ( '#',  timsys                              )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                     )
            return

         end if

      end if

!
!     If the output time system is TDB, find the clock ID associated
!     with IDCODE.
!
      if ( istdb ) then

         call ckmeta ( ctx, inst, 'SCLK', clkid )

         if ( ctx%failed() ) return

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
!     If TOL > 0, we'll apply TOL after we've found the coverage
!     for the zero-tolerance case.
!
!     If the time system is TDB, we'll convert the times to TDB
!     at the end of this routine.

!
!     Start a forward search.
!
      call dafbfs ( ctx, handle )

!
!     Find the next DAF array.
!
      call daffna ( ctx, found )

      do while ( found )
!
!        Note:  we check FAILED() at the bottom of this loop; this
!        routine returns if FAILED() returns .TRUE. at that point.
!
!        Fetch and unpack the segment descriptor.
!
         call dafgs ( ctx, descr )
         call dafus ( ctx, descr, nd, ni, dc, ic )

!
!        Let AVOK indicate whether the segment satisfies the
!        angular velocity restriction.
!
         avok = ( ic(4) == 1 ) .or. ( .not. needav )

         if (  ( ic(1) == inst )  .and.  avok  ) then
!
!           This segment is for the body of interest.  If angular
!           velocity is needed, this segment has it.
!
            if ( seglvl ) then
!
!              This is a segment-level summary.
!
!              Insert the coverage bounds into the coverage window.
!              Adjust the interval using the tolerance.
!
               dctol(1)  =  max (  dc(1) - tol,  0.0_wp  )
               dctol(2)  =         dc(2) + tol

!
!              Convert the time to TDB if necessary.
!
               if ( istdb ) then
!
!                 Convert the time bounds to TDB before inserting
!                 into the window.
!
                  do i = 1, 2
                     call sct2e ( ctx, clkid, dctol(i), et )
                     dctol(i)  =  et
                  end do

               end if

               if ( dctol(1) <= dctol(2) ) then
                  call insert_interval ( ctx,  dctol(1),  dctol(2), window )
               end if

            else
!
!              We're looking for an interval-level coverage window.
!              This information must be retrieved in a
!              data-type-dependent fashion.  The coverage routines
!              we'll call will, if necessary, adjust intervals by TOL
!              and convert interval times to TDB.
!
               dtype  = ic(3)
               segbeg = ic(5)
               segend = ic(6)

               if ( dtype == 1 ) then

                  call zzckcv01 ( ctx, handle, segbeg, segend, &
                                  clkid,  tol,    timsys, window )

               else if ( dtype == 2 ) then

                  call zzckcv02 ( ctx, handle, segbeg, segend, &
                                  clkid,  tol,    timsys, window )

               else if ( dtype == 3 ) then

                  call zzckcv03 ( ctx, handle, segbeg, segend, &
                                  clkid,  tol,    timsys, window )

               else if ( dtype == 4 ) then

                  call zzckcv04 ( ctx, handle, segbeg, segend, &
                                  clkid,  tol,    timsys, window )

               else if ( dtype == 5 ) then

                  call zzckcv05 ( ctx, handle, segbeg, segend, clkid, &
                                  dc,     tol,    timsys, window )

               else if ( dtype == 6 ) then

                  call zzckcv06 ( ctx, handle, segbeg, segend, clkid, &
                                  dc,     tol,    timsys, window )

               else

                  call ctx%setmsg ( 'Supported CK data types are 1, 2, ' // &
                                '3, 4, 5.  Data type of segment: #. '// &
                                'This problem may indicate that you '// &
                                'need to update your SPICE Toolkit.' )
                  call ctx%errint ( '#',  dtype                          )
                  call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                )
                  return

               end if

            end if

         end if

         call daffna ( ctx, found )

         if ( ctx%failed() ) return

      end do

!
!     COVER now represents the coverage of the entire file at the
!     granularity indicated by LEVEL, combined with the coverage
!     contained in COVER on input.
!
!     Release the file.
!
      call dafcls ( ctx, handle )

      if ( .not. ctx%failed() ) call window%window ( intervals )
      return
      end subroutine ck_coverage

end submodule pepper_ck_api
