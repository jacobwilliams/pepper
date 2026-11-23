!> The kernel manager: loading (FURNSH), tracking ([[ktotal]], [[kdata]], [[kinfo]]) and unloading (UNLOAD, KCLEAR) of SPICE kernels, including meta-kernels, and the routines it needs.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## KEEPER (Keeps track of SPICE kernels)
!>
!> PEPPER: SPICELIB's umbrella routine KEEPER is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_keeper. Its documentation follows.
!>
!>### Abstract
!>
!>     This routine is an umbrella for a collection of entry points
!>     that manage the loading and unloading of SPICE kernels from
!>     an application program.
!>
!>### Detailed Input
!>
!>     See Individual Entry points.
!>
!>### Detailed Output
!>
!>     See Individual Entry points.
!>
!>### Parameters
!>
!>     FILSIZ   is the maximum file name length that can be
!>              accommodated by this set of routines.
!>
!>     MAXFIL   is the number of entries that can be stored in KEEPER's
!>              kernel database. In this version of the toolkit MAXFIL
!>              is set to 5300. Each time a kernel is loaded via
!>              FURNSH, a database entry is created for that kernel.
!>              If a meta-kernel is loaded, a database entry is created
!>              for the meta-kernel itself and for all files referenced
!>              in the meta-kernel's KERNELS_TO_LOAD specification.
!>              Unloading a kernel or meta-kernel deletes database
!>              entries created when the file was loaded.
!>
!>              The parameter MAXFIL is an upper bound on number of
!>              SPICE kernels that can be loaded at any time via the
!>              KEEPER interface, but the number of kernels that can be
!>              loaded may be smaller, since re-loading a loaded kernel
!>              or meta-kernel results in creation of additional
!>              database entries.
!>
!>              Kernels loaded into the KEEPER system are subject to
!>              constraints imposed by lower-level subsystems. The
!>              binary kernel systems (SPK, CK, binary PCK, and EK)
!>              have their own limits on the maximum number of kernels
!>              that may be loaded.
!>
!>              The total number of DAF-based files (this set includes
!>              SPKs, CKs, and binary PCKs) and DAS-based files (this
!>              set includes EKs and DSKs) that may be loaded at any
!>              time may not exceed 5000. This limit applies whether
!>              the files are loaded via FURNSH or lower-level loaders
!>              such as SPKLEF or DAFOPR. File access performance
!>              normally will degrade as the number of loaded kernels
!>              increases.
!>
!>### Exceptions
!>
!>     1)  If the main routine KEEPER is called, the error
!>         SPICE(BOGUSENTRY) is signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     This routine serves as an umbrella for a collection of
!>     entry points that unify the task of loading, tracking,
!>     and unloading SPICE kernels. A description of each entry
!>     point is given below:
!>
!>     FURNSH    Furnish a kernel to a program. This entry point
!>               provides a single interface for loading kernels into
!>               your application program. All SPICE kernels (Text
!>               kernels, SPK, CK, Binary PCK, and EK) can be loaded
!>               through this entry point. In addition, special text
!>               kernels, called meta-Text kernels, that contain a list
!>               of other kernels to load can be processed by FURNSH.
!>
!>               Meta-text kernels allow you to easily control which
!>               kernels will be loaded by your program without having
!>               to write your own kernel managing routines.
!>
!>     KTOTAL    returns the number of kernels that are currently
!>               available to your program as a result of previous calls
!>               to FURNSH and UNLOAD.
!>
!>     KDATA     provides an interface for retrieving (in order of their
!>               specification through FURNSH) kernels that are active in
!>               your application.
!>
!>     KINFO     allows you to retrieve information about a loaded
!>               kernel using the name of that kernel.
!>
!>     KCLEAR    Unloads all kernels that were loaded via the KEEPER
!>               system, clears the kernel pool, and re-initializes the
!>               KEEPER system.
!>
!>     UNLOAD    provides an interface for unloading kernels that have
!>               been loaded via the routine FURNSH.
!>
!>     For more details concerning any particular entry point, see the
!>     header for that entry point.
!>
!>### Examples
!>
!>     The numerical results shown for this example may differ across
!>     platforms. The results depend on the SPICE kernels used as
!>     input, the compiler and supporting libraries, and the machine
!>     specific arithmetic implementation.
!>
!>     1) Load a meta-kernel with a PCK, an LSK and an SPK, and
!>        separately, a text kernel and a binary PCK. Loop over the
!>        loaded kernels, outputting file information for each of
!>        them.
!>
!>        Then unload the text kernels, check that they have been
!>        unloaded, and finally unload all remaining kernels
!>        and clear the kernel pool using KCLEAR.
!>
!>        Use the meta-kernel shown below to load the required SPICE
!>        kernels.
!>
!>           KPL/MK
!>
!>           File name: keeper_ex1.tm
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
!>              File name                     Contents
!>              ---------                     --------
!>              de421.bsp                     Planetary ephemeris
!>              pck00009.tpc                  Planet orientation and
!>                                            radii
!>              naif0012.tls                  Leapseconds
!>
!>           \begindata
!>
!>              KERNELS_TO_LOAD = ( 'de421.bsp',
!>                                  'naif0012.tls',
!>                                  'pck00009.tpc' )
!>
!>           \begintext
!>
!>           End of meta-kernel
!>
!>        Use the PCK kernel below as the binary PCK required for the
!>        example.
!>
!>           earth_latest_high_prec.bpc
!>
!>        Use the FK kernel below as the text kernel required for the
!>        example.
!>
!>           RSSD0002.TF
!>
!>        Example code begins here.
!>
!>```fortran
!>      PROGRAM KEEPER_EX1
!>      IMPLICIT NONE
!>
!>C
!>C     Local constants.
!>C
!>      INTEGER               FNAMLN
!>      PARAMETER           ( FNAMLN = 256 )
!>
!>      INTEGER               FTYPLN
!>      PARAMETER           ( FTYPLN = 33 )
!>
!>      INTEGER               SRCLEN
!>      PARAMETER           ( SRCLEN = 256 )
!>
!>C
!>C     Local variables.
!>C
!>      CHARACTER*(FNAMLN)    FILE
!>      CHARACTER*(FTYPLN)    FILTYP
!>      CHARACTER*(SRCLEN)    SRCFIL
!>
!>      INTEGER               COUNT
!>      INTEGER               HANDLE
!>      INTEGER               WHICH
!>
!>      LOGICAL               FOUND
!>
!>C
!>C     Load several kernel files.
!>C
!>      CALL FURNSH ( 'keeper_ex1.tm'              )
!>      CALL FURNSH ( 'RSSD0002.TF'                )
!>      CALL FURNSH ( 'earth_latest_high_prec.bpc' )
!>
!>C
!>C     Count the number of loaded kernel files.
!>C
!>      CALL KTOTAL ( 'ALL', COUNT )
!>
!>      WRITE(*,'(A,I2)') 'The total number of kernels after '
!>     . //               'final FURNSH: ', COUNT
!>      WRITE(*,*) ' '
!>
!>C
!>C     Retrieve the data for all the loaded kernels and
!>C     test an index for which there should be no kernel.
!>C
!>      WRITE(*,'(A)') 'Overview of loaded kernels, by index:'
!>      WRITE(*,*) ' '
!>
!>      DO WHICH= 1, COUNT + 1
!>
!>         CALL KDATA ( WHICH, 'ALL',   FILE, FILTYP,
!>     .                SRCFIL, HANDLE, FOUND        )
!>
!>         IF ( FOUND ) THEN
!>
!>            WRITE(*,*) '  Index : ', WHICH
!>            WRITE(*,*) '  File  : ', FILE
!>            WRITE(*,*) '  Type  : ', FILTYP
!>            WRITE(*,*) '  Source: ', SRCFIL
!>            WRITE(*,*) '  Handle: ', HANDLE
!>            WRITE(*,*) ' '
!>
!>         ELSE
!>
!>            WRITE(*,*) '  No kernel found with index: ', WHICH
!>
!>         END IF
!>
!>      END DO
!>
!>C
!>C     Unload the text kernels.
!>C
!>      CALL KTOTAL ( 'TEXT', COUNT )
!>
!>      WRITE(*,*) ' '
!>      WRITE(*,'(A,I2,A)') 'Unloading', COUNT,
!>     .                    ' text kernels...'
!>      WRITE(*,*) ' '
!>
!>      DO WHILE ( COUNT .NE. 0 )
!>
!>         CALL KDATA (      1, 'TEXT',  FILE, FILTYP,
!>     .                SRCFIL, HANDLE, FOUND        )
!>
!>C
!>C        If the kernel is found in the pool, unload it.
!>C
!>         IF ( FOUND ) THEN
!>
!>            CALL UNLOAD ( FILE )
!>
!>C
!>C           Check if the file has been unloaded.
!>C
!>            CALL KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )
!>
!>            IF ( FOUND ) THEN
!>
!>               WRITE(*,'(A)') '  Error unloading ' // FILE
!>
!>            ELSE
!>
!>               WRITE(*,'(A)') '  Success unloading ' // FILE
!>
!>            END IF
!>
!>C
!>C        Something is not working. Inform NAIF.
!>C
!>         ELSE
!>
!>            WRITE(*,*) ' ERROR: No kernel found with index: ',
!>     .                 WHICH
!>
!>         END IF
!>
!>C
!>C        Check if we have more text kernels to unload from
!>C        the kernel pool. Note that unloading a text kernel
!>C        or meta-kernel implies that the kernel pool is
!>C        cleared, and any kernel(s) that were not to be
!>C        unloaded are re-loaded. Therefore the COUNT value
!>C        changes, and the indexing of the files within the
!>C        kernel pool too.
!>C
!>         CALL KTOTAL ( 'TEXT', COUNT )
!>
!>      END DO
!>
!>      CALL KTOTAL ( 'ALL', COUNT )
!>
!>      WRITE(*,*) ' '
!>      WRITE(*,'(A,I2)') 'The total number of kernels after '
!>     . //               'UNLOAD calls: ', COUNT
!>
!>C
!>C     Clear the KEEPER system, retrieve the number of loaded
!>C     after the clear.
!>C
!>      CALL KCLEAR()
!>
!>      CALL KTOTAL ( 'ALL', COUNT )
!>
!>      WRITE(*,*) ' '
!>      WRITE(*,'(A,I2)') 'The total number of kernels after '
!>     . //               'KCLEAR      : ', COUNT
!>
!>      END
!>```
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, the output was:
!>
!>        The total number of kernels after final FURNSH:  6
!>
!>        Overview of loaded kernels, by index:
!>
!>           Index :            1
!>           File  : keeper_ex1.tm
!>           Type  : META
!>           Source:
!>           Handle:            0
!>
!>           Index :            2
!>           File  : de421.bsp
!>           Type  : SPK
!>           Source: keeper_ex1.tm
!>           Handle:            1
!>
!>           Index :            3
!>           File  : naif0012.tls
!>           Type  : TEXT
!>           Source: keeper_ex1.tm
!>           Handle:            0
!>
!>           Index :            4
!>           File  : pck00009.tpc
!>           Type  : TEXT
!>           Source: keeper_ex1.tm
!>           Handle:            0
!>
!>           Index :            5
!>           File  : RSSD0002.TF
!>           Type  : TEXT
!>           Source:
!>           Handle:            0
!>
!>           Index :            6
!>           File  : earth_latest_high_prec.bpc
!>           Type  : PCK
!>           Source:
!>           Handle:            2
!>
!>           No kernel found with index:            7
!>
!>        Unloading 3 text kernels...
!>
!>          Success unloading naif0012.tls
!>          Success unloading pck00009.tpc
!>          Success unloading RSSD0002.TF
!>
!>        The total number of kernels after UNLOAD calls:  3
!>
!>        The total number of kernels after KCLEAR      :  0
!>
!>### Restrictions
!>
!>     None.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     C.H. Acton         (JPL)
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     B.V. Semenov       (JPL)
!>     W.L. Taber         (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 5.1.0, 29-DEC-2021 (JDR) (NJB)
!>
!>        Changed argument name SOURCE to SRCFIL for consistency with
!>        other routines.
!>
!>        Updated KEEPER umbrella routine and all entry points' headers
!>        to comply with NAIF standard.
!>
!>        Updated description of input argument KIND in headers of entry
!>        points KDATA and KTOTAL. Updated $Brief_I/O table to include
!>        WHICH short description and sort arguments in the order they
!>        are declared.
!>
!>        Added a restriction about specifying kernels using relative
!>        paths to the FURNSH entry point header $Restrictions section.
!>
!>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
!>
!>        Updated to support use of DSKs.
!>
!>        Bug fix: now unloads binary kernels via low-level
!>        unload routines only when those kernels have just
!>        one entry in the KEEPER database.
!>
!>        Updated description of MAXFIL in the header.
!>
!>        Updated the $Author_and_Institution section.
!>
!>-    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS)
!>
!>        Updated the discussion of kernel variable watches in entry
!>        points KCLEAR and UNLOAD. Added to the FURNSH header mention
!>        of the effects of failure during text kernel or meta-kernel
!>        loading.
!>
!>        Last update was 12-APR-2012 (BVS)
!>
!>           Increased FTSIZE (from 1000 to 5000).
!>
!>           Changed to use SEPOOL instead of STPOOL to reduce loading
!>           time for large meta-kernels due to n^2 delay in STPOOL.
!>
!>-    SPICELIB Version 4.0.2, 13-APR-2011 (EDW)
!>
!>        Trivial edit to KCLEAR $Restrictions, replaced P*POOL with
!>        PXPOOL. The "*" character causes the HTML documentation
!>        script to create a link for the "POOL" substring.
!>
!>-    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW)
!>
!>        Added mention of the restriction on kernel pool variable
!>        names to MAXLEN (defined in pool.f) characters or less.
!>
!>-    SPICELIB Version 4.0.0, 02-APR-2009 (NJB)
!>
!>        Continued path values are now supported. FURNSH now rejects
!>        file names longer than FILSIZ characters.
!>
!>        Deleted references to unneeded variable DOALL. Made
!>        THSTYP declaration compatible with TYPES array.
!>
!>-    SPICELIB Version 3.0.1, 27-APR-2007 (NJB)
!>
!>        Fixed header typo: added quotes to literal string
!>        input arguments in example FURNSH calls.
!>
!>-    SPICELIB Version 3.0.0, 15-NOV-2006 (NJB)
!>
!>        Added entry point KCLEAR. Bug fix: meta-kernel unloading bug
!>        in UNLOAD was corrected. Some header updates were made.
!>
!>-    SPICELIB Version 2.0.2, 29-JUL-2003 (NJB) (CHA)
!>
!>        Only the header of the entry point FURNSH was modified.
!>        Numerous updates were made to improve clarity. Some
!>        corrections were made.
!>
!>-    SPICELIB Version 2.0.1, 06-DEC-2002 (NJB)
!>
!>        Typo in header example was corrected.
!>
!>-    SPICELIB Version 2.0.0, 07-JAN-2002 (WLT)
!>
!>        Added a call to CVPOOL in FURNSH so that watches that are
!>        triggered are triggered by loading Meta-kernels and not by
!>        some external interaction with the kernel pool.
!>
!>        Added code to make sure that UNLOAD has the effect of
!>        loading all remaining kernels in the order they were first
!>        introduced.
!>
!>-    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT)
!>
!>        Corrected the error message template used
!>        by ZZLDKER
!>
!>-    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB)
!>
!>        Documentation fix: corrected second code example in the
!>        header of the entry point FURNSH. The example previously used
!>        the kernel variable PATH_NAMES; that name has been replaced
!>        with the correct name PATH_VALUES.
!>
!>-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
!>
!>### Implementation notes
!>
!>     Here we set up the database of loaded kernels
!>
!>     The name of every file loaded through this interface will
!>     be stored in the array FILES.
!>
!>     The handle of every loaded file will be stored in the array
!>     HANDLS.  If the file is a text kernel it will be assigned the
!>     handle 0.
!>
!>     The source of each file specified will be stored in the integer
!>     array SRCFIL.  If the file is loaded directly, its source
!>     will be zero.  If it is loaded as the result of meta-information
!>     in a text kernel, the index of the source file in FILES will
!>     be stored in SRCES.
!>
!>     The file type of every loaded kernel will be stored in the array
!>     TYPES.
!>
!>     The number of files loaded through this interfaces is kept in the
!>     integer LOADED.
submodule (pepper_instance_module) pepper_keeper

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: lnsize = 80  !! length of a line of text (messages, kernel lines)
   integer(ip), parameter :: msgsiz = 500  !! length of an error message
   integer(ip), parameter :: nknown = 3  !! number of known kernel types
   integer(ip), parameter :: typlen = 8  !! length of a kernel type (SPK, TEXT, META ...)

contains

      !> Convert an integer to an equivalent written phrase.
      !> For example, convert 121 to 'ONE HUNDRED TWENTY-ONE'.
      !>
      !>## Detailed Input
      !>
      !>     N        is any integer (less than 10**12 in absolute value).
      !>              If N is less than 0, -N must be a legitimate number.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is the English cardinal equivalent of N. STRING will
      !>              contain only upper case letters.
      !>
      !>              The longest possible output string contains 145
      !>              characters. One such string is:
      !>
      !>                 'NEGATIVE '                                  //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN'
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the resulting text is longer than the output string,
      !>         it will be truncated on the right, leaving only the most
      !>         significant portion of the number.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is used primarily for constructing error messages.
      !>     For example, an overflow message might look like the following:
      !>
      !>        'An excess of seventy-four parameters was detected.'
      !>
      !>     A second use might be to write dollar amounts: it's much harder
      !>     to tamper with a string like
      !>
      !>        'Two thousand four hundred seventy-one dollars'
      !>
      !>     than with the equivalent string
      !>
      !>        '$ 2471.00'
      !>
      !>## Examples
      !>
      !>     N           STRING
      !>     ------      ------------------------------------------
      !>     -43         NEGATIVE FORTY-THREE
      !>      1          ONE
      !>      2          TWO
      !>      3          THREE
      !>      4          FOUR
      !>      20         TWENTY
      !>      21         TWENTY-ONE
      !>      99         NINETY-NINE
      !>      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-ONE
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that N will always be less than
      !>         a trillion (10**12) in absolute value.
      !>
      !>     2)  In the event that N is less than zero, this routine assumes
      !>         that -N is a legitimate integer on the host machine.
      !>
      !>     3)  This routine assumes that an integer as large as 10**9
      !>         (one billion) is representable on the host machine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
      module subroutine inttxt ( ctx, n, string )
      use pepper_kinds, only: ip
      use pepper_strings, only: suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! An integer (less than 10**12 in absolute value).
      character(*), intent(out) :: string  !! An English string representing the cardinal of N.

      integer(ip), parameter :: lngest = 9

      character(lngest), parameter :: number (19) = &
         [ character(lngest) :: 'ONE', 'TWO', 'THREE', 'FOUR', 'FIVE', 'SIX', 'SEVEN', &
           'EIGHT', 'NINE', 'TEN', 'ELEVEN', 'TWELVE', 'THIRTEEN', &
           'FOURTEEN', 'FIFTEEN', 'SIXTEEN', 'SEVENTEEN', 'EIGHTEEN', &
           'NINETEEN' ]
      character(lngest) :: suff
      character(lngest), parameter :: tens   ( 9) = &
         [ character(lngest) :: 'TEN', 'TWENTY', 'THIRTY', 'FORTY', 'FIFTY', 'SIXTY', &
           'SEVENTY', 'EIGHTY', 'NINETY' ]

      integer(ip) :: num
      integer(ip) :: pad
      integer(ip) :: space
      integer(ip) :: x
      integer(ip) :: y

!
!     Zero is easy.
!
      if ( n == 0 ) then

         string = 'ZERO'
         return

      end if

!
!     If the number is negative, the string begins with the word
!     `NEGATIVE', and otherwise the number can be treated as though
!     it were positive.
!
      if ( n < 0 ) then

         num    = - n
         string = 'NEGATIVE'

      else

         num    =   n
         string = ' '

      end if

!
!     Construct the number portion, from left to right: billions,
!     then millions, and so on. In case of overflow, SUFFIX simply
!     leaves the output string unchanged, so there is no need to
!     check explicitly for truncation.
!
      do while ( num > 0 )

!
!        Find the right unit (billion, million, or whatever),
!        and the number (X) of those units. X should always
!        be between zero and 999, regardless of the units.
!
         if ( num >= 1000000000 ) then

            x    = num / 1000000000
            suff = 'BILLION'
            num  = num - x * 1000000000

         else if  ( num >= 1000000 ) then

            x    = num / 1000000
            suff = 'MILLION'
            num  = num - x * 1000000

         else if  ( num >= 1000 )    then

            x    = num / 1000
            suff = 'THOUSAND'
            num  = num - x * 1000

         else

            x    = num
            suff = ' '
            num  = 0

         end if

!
!        Convert X to text, ...
!
         space = 1

         do while ( x > 0 )

            if ( string == ' ' ) then
               pad = 0
            else
               pad = 1
            end if

            if ( x >= 100 ) then

               y = x / 100
               x = x - y*100

               call suffix ( number(y), pad, string )
               call suffix ( 'HUNDRED', 1,   string )

            else if ( x >= 20 ) then

               y = x / 10
               x = x - y*10

               call suffix ( tens(y), pad, string )

               if ( x /= 0 ) then
                  call suffix ( '-', 0, string )
                  space = 0
               end if

            else

               y = x
               x = 0

               if ( string == ' ' ) then
                  space = 0
               end if

               call suffix ( number(y), space, string )

            end if

         end do

!
!        ... then add the units. Repeat as necessary.
!
         call suffix ( suff, 1, string )

      end do

      return
      end subroutine inttxt

      !> Convert an integer to an equivalent written ordinal phrase.
      !> For example, convert 121 to 'ONE HUNDRED TWENTY-FIRST'.
      !>
      !>## Detailed Input
      !>
      !>     N        is an integer (less than 10**12 in absolute value).
      !>              Moreover, if N is less than zero, -N must be a
      !>              a legitimate number on the host machine.
      !>
      !>              In the context of this routine N represents the
      !>              ranking of some item within a group.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is the English ordinal equivalent of N. STRING will
      !>              contain only upper case letters.
      !>
      !>## Parameters
      !>
      !>     MAXORD   is one more than the length of the longest ordinal
      !>              string that can be produced by a call to this routine:
      !>              One string of maximum length is:
      !>
      !>                 'NEGATIVE '                                  //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
      !>                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
      !>                 'SEVEN HUNDRED SEVENTY-SEVENTH'
      !>
      !>              It has 147 characters.
      !>
      !>              The parameter MAXORD is used to declare a local string
      !>              of sufficient length to allow the construction of
      !>              any ordinal string.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the resulting ordinal is longer than the output string,
      !>         it will be truncated on the right, leaving only the most
      !>         significant portion of the ordinal.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is used primarily for generating error messages. For example,
      !>     if the third letter or token in a string is in error, it might
      !>     be desirable to supply a message like the following:
      !>
      !>        'The third token of 31-JAN-198$ is not a valid year.'
      !>
      !>## Examples
      !>
      !>     N           STRING
      !>     ------      -------------------------------------------
      !>     -6          NEGATIVE SIXTH
      !>      1          FIRST
      !>      2          SECOND
      !>      3          THIRD
      !>      4          FOURTH
      !>      20         TWENTIETH
      !>      21         TWENTY-FIRST
      !>      99         NINETY-NINTH
      !>      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-FIRST
      !>
      !>## Restrictions
      !>
      !>     1)  Whatever restrictions apply to INTTXT apply to this routine
      !>         as well.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
      module subroutine intord ( ctx, n, string )
      use pepper_kinds, only: ip
      use pepper_strings, only: suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! An integer (less than 10**12 in absolute value).
      character(*), intent(out) :: string  !! An English string representing the ordinal of N.

      integer(ip), parameter :: maxord = 148

      integer(ip) :: i
      integer(ip) :: last
      character(maxord) :: mystr

!
!     First get the English equivalent of the cardinal N.
!
      mystr = ' '

      call inttxt ( ctx, n, mystr )

      last  = len_trim(mystr)
      i     = last

!
!     Find the beginning of the last number of MYSTR.
!
      do while (       ( mystr(i:i) /= '-' ) &
                 .and. ( mystr(i:i) /= ' ' ) &
                 .and. ( i          >  1  )  )

         i = i - 1

      end do

      if (      ( mystr(i:i) == ' ' ) &
           .or. ( mystr(i:i) == '-' )  ) then

         i = i + 1

      end if

!
!     Now convert the last cardinal to an ordinal.
!
      if ( mystr(i:last) == 'ONE' ) then

         mystr(i:) = 'FIRST'

      else if ( mystr(i:last) == 'TWO' ) then

         mystr(i:) = 'SECOND'

      else if ( mystr(i:last) == 'THREE' ) then

         mystr(i:) = 'THIRD'

      else if ( mystr(i:last) == 'FIVE' ) then

         mystr(i:) = 'FIFTH'

      else if ( mystr(i:last) == 'EIGHT' ) then

         mystr(i:) = 'EIGHTH'

      else if ( mystr(i:last) == 'NINE' ) then

         mystr(i:) = 'NINTH'

      else if ( mystr (i:last) == 'TWELVE' ) then

         mystr(i:) = 'TWELFTH'

      else if ( mystr(last:last) == 'Y' ) then

         mystr(last:) = 'IETH'

      else

         call suffix ( 'TH', 0, mystr )

      end if

!
!     Now simply put MYSTR into STRING and return.
!
      string = mystr

      return
      end subroutine intord

      !> Replace a marker with the text representation of an ordinal
      !> number.
      !>
      !>## Detailed Input
      !>
      !>     IN       is an arbitrary character string.
      !>
      !>     MARKER   is an arbitrary character string. The first occurrence of
      !>              MARKER in the input string is to be replaced by the text
      !>              representation of the ordinal number VALUE.
      !>
      !>              Leading and trailing blanks in MARKER are NOT
      !>              significant. In particular, no substitution is performed
      !>              if MARKER is blank.
      !>
      !>     VALUE    is an arbitrary integer.
      !>
      !>     RTCASE   indicates the case of the replacement text. RTCASE may be
      !>              any of the following:
      !>
      !>                 RTCASE   Meaning        Example
      !>                 ------   -----------    -----------------------
      !>                 U, u     Uppercase      ONE HUNDRED FIFTY-THIRD
      !>
      !>                 L, l     Lowercase      one hundred fifty-third
      !>
      !>                 C, c     Capitalized    One hundred fifty-third
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the string obtained by substituting the text
      !>              representation of the ordinal number VALUE for the first
      !>              occurrence of MARKER in the input string.
      !>
      !>              OUT and IN must be identical or disjoint.
      !>
      !>## Parameters
      !>
      !>     MAXLON   is the maximum expected length of any ordinal text.
      !>              147 characters are sufficient to hold the text
      !>              representing any ordinal value whose corresponding
      !>              ordinal value is in the range
      !>
      !>                ( -10**12, 10**12 )
      !>
      !>              An example of a number whose ordinal text representation
      !>              is of maximum length is
      !>
      !>                 - 777 777 777 777
      !>
      !>## Exceptions
      !>
      !>     1)  If OUT does not have sufficient length to accommodate the
      !>         result of the substitution, the result will be truncated on
      !>         the right.
      !>
      !>     2)  If MARKER is blank, or if MARKER is not a substring of IN,
      !>         no substitution is performed. (OUT and IN are identical.)
      !>
      !>     3)  If the value of RTCASE is not recognized, the error
      !>         SPICE(INVALIDCASE) is signaled. OUT is not changed.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is one of a family of related routines for inserting values
      !>     into strings. They are typically to construct messages that
      !>     are partly fixed, and partly determined at run time. For example,
      !>     a message like
      !>
      !>        'The fifty-first picture was found in directory [USER.DATA].'
      !>
      !>     might be constructed from the fixed string
      !>
      !>        'The #1 picture was found in directory #2.'
      !>
      !>     by the calls
      !>
      !>        CALL REPMOT ( STRING, '#1',  51,           'L', STRING )
      !>        CALL REPMC  ( STRING, '#2', '[USER.DATA]',      STRING )
      !>
      !>     which substitute the ordinal text 'fifty-first' and the character
      !>     string '[USER.DATA]' for the markers '#1' and '#2' respectively.
      !>
      !>     The complete list of routines is shown below.
      !>
      !>        REPMC    ( Replace marker with character string value )
      !>        REPMD    ( Replace marker with double precision value )
      !>        REPMF    ( Replace marker with formatted d.p. value   )
      !>        REPMI    ( Replace marker with integer value          )
      !>        REPML    ( Replace marker with logical value          )
      !>        REPMCT   ( Replace marker with cardinal text          )
      !>        REPMOT   ( Replace marker with ordinal text           )
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following example illustrate the use of REPMOT to
      !>        replace a marker within a string with the ordinal text
      !>        corresponding to an integer.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REPMOT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 80 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      INSTR
      !>      CHARACTER*(STRLEN)      MARKER
      !>      CHARACTER*(STRLEN)      OUTSTR
      !>
      !>C
      !>C     1. Uppercase
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'INVALID COMMAND. # WORD NOT RECOGNIZED.'
      !>
      !>      CALL REPMOT ( INSTR, MARKER, 5, 'U', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 1: Replacement text in uppercase.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     2. Lowercase
      !>C
      !>      MARKER = ' XX '
      !>      INSTR  = 'The XX word of the XX sentence was ...'
      !>
      !>      CALL REPMOT ( INSTR, MARKER, 5, 'L', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 2: Replacement text in lowercase.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     2. Capitalized
      !>C
      !>      MARKER = ' XX '
      !>      INSTR  = 'Name:  YY.  Rank:  XX.'
      !>
      !>      CALL REPMC  ( INSTR,  'YY',  'Moriarty', OUTSTR )
      !>      CALL REPMOT ( OUTSTR, MARKER, 5,    'C', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 3: Replacement text capitalized.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Case 1: Replacement text in uppercase.
      !>            Input : INVALID COMMAND. # WORD NOT RECOGNIZED.
      !>            Output: INVALID COMMAND. FIFTH WORD NOT RECOGNIZED.
      !>
      !>         Case 2: Replacement text in lowercase.
      !>            Input : The XX word of the XX sentence was ...
      !>            Output: The fifth word of the XX sentence was ...
      !>
      !>         Case 3: Replacement text capitalized.
      !>            Input : Name:  YY.  Rank:  XX.
      !>            Output: Name:  Moriarty.  Rank:  Fifth.
      !>
      !>## Restrictions
      !>
      !>     1)  VALUE must be in the range accepted by the SPICELIB routine
      !>         INTORD. This range is currently
      !>
      !>            ( -10**12, 10**12 )
      !>
      !>         Note that the endpoints of the interval are excluded.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed input argument name CASE to RTCASE for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated code to remove unnecessary lines of code in the
      !>        Standard SPICE error handling CHKIN statements.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example from existing fragments. Improved example in
      !>        $Particulars section.
      !>
      !>        Added REPML to the list of available replace marker routines.
      !>
      !>-    SPICELIB Version 1.2.0, 21-SEP-2013 (BVS)
      !>
      !>        Minor efficiency update: the routine now looks up the first
      !>        and last non-blank characters only once.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) (IMU)
      module subroutine repmot ( ctx, in, marker, value, rtcase, out )
      use pepper_kinds, only: ip
      use pepper_strings, only: lcase_inplace, ucase_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      integer(ip), intent(in) :: value  !! Replacement value.
      character(1), intent(in) :: rtcase  !! Case of replacement text.
      character(*), intent(out) :: out  !! Output string.

      integer(ip), parameter :: maxlon = 147

      character(maxlon) :: ord
      character(1) :: tmpcas

      integer(ip) :: mrknbf
      integer(ip) :: mrknbl

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Bail out if RTCASE is not recognized.
!
      tmpcas = adjustl(rtcase)
      call ucase_inplace ( tmpcas )

      if (        ( tmpcas /= 'U' ) &
            .and. ( tmpcas /= 'L' ) &
            .and. ( tmpcas /= 'C' )  ) then

         call ctx%setmsg ( 'Case (#) must be U, L, or C.' )
         call ctx%errch  ( '#', rtcase                    )
         call ctx%sigerr ( 'SPICE(INVALIDCASE)'           )
         return

      end if

!
!     If MARKER is blank, no substitution is possible.
!
      if ( marker == ' ' ) then

         out = in

         return

      end if

!
!     Locate the leftmost occurrence of MARKER, if there is one
!     (ignoring leading and trailing blanks). If MARKER is not
!     a substring of IN, no substitution can be performed.
!
      mrknbf = verify(marker, ' ')
      mrknbl = len_trim(marker)

      mrkpsb = index ( in, marker ( mrknbf : mrknbl )  )

      if  ( mrkpsb == 0 ) then

         out = in

         return

      end if

      mrkpse = mrkpsb + mrknbl - mrknbf

!
!     Okay, RTCASE is recognized and MARKER has been found.
!     Generate the ordinal text corresponding to VALUE.
!
      call intord ( ctx, value, ord )

!
!     CARD is always returned in upper case; change to the specified
!     case, if required.
!
      if ( tmpcas == 'L' ) then

         call lcase_inplace ( ord )

      else if ( tmpcas == 'C' ) then

         call lcase_inplace ( ord(2:) )

      end if

!
!     Replace MARKER with CARD.
!
      call repsub ( ctx, in,  mrkpsb,  mrkpse,  ord ( : len_trim(ord) ),  out )

      return
      end subroutine repmot

      !> Retrieve the string starting at the FIDX element of the kernel
      !> pool variable, where the string may be continued across several
      !> components of the kernel pool variable.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is the name of a kernel pool variable for which
      !>              the caller wants to retrieve a full (potentially
      !>              continued) string.
      !>
      !>     FIDX     is the index of the first component (the start) of
      !>              the string in ITEM.
      !>
      !>     CONTIN   is a sequence of characters which (if they appear as
      !>              the last non-blank sequence of characters in a
      !>              component of a value of a kernel pool variable)
      !>              indicate that the string associated with the
      !>              component is continued into the next literal
      !>              component of the kernel pool variable.
      !>
      !>              If CONTIN is blank, all of the components of ITEM
      !>              will be retrieved as a single string.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is the full string starting at the FIDX element of the
      !>              kernel pool variable specified by ITEM.
      !>
      !>              Note that if STRING is not sufficiently long to hold
      !>              the fully continued string, the value will be
      !>              truncated. You can determine if STRING has been
      !>              truncated by examining the variable SIZE.
      !>
      !>     SIZE     is the index of last non-blank character of
      !>              continued string as it is represented in the
      !>              kernel pool. This is the actual number of characters
      !>              needed to hold the requested string. If STRING
      !>              contains a truncated portion of the full string,
      !>              RTRIM(STRING) will be less than SIZE.
      !>
      !>              If the value of STRING should be a blank, then
      !>              SIZE will be set to 1.
      !>
      !>     LIDX     is the index of the last component (the end) of
      !>              the retrieved string in ITEM.
      !>
      !>     FOUND    is a logical variable indicating success of the
      !>              request to retrieve the string.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the variable specified by ITEM is not present in the
      !>         kernel pool or is present but is not character valued, STRING
      !>         will be returned as a blank, SIZE will be returned with the
      !>         value 0 and FOUND will be set to .FALSE. In particular if NTH
      !>         is less than 1, STRING will be returned as a blank, SIZE will
      !>         be zero and FOUND will be .FALSE.
      !>
      !>     2)  If the variable specified has a blank string associated
      !>         with its full string starting at FIDX, STRING will be blank,
      !>         SIZE will be 1 and FOUND will be set to .TRUE.
      !>
      !>     3)  If STRING is not long enough to hold all of the characters
      !>         associated with the NTH string, it will be truncated on the
      !>         right.
      !>
      !>     4)  If the continuation character is a blank, every component
      !>         of the variable specified by ITEM will be inserted into
      !>         the output string.
      !>
      !>     5)  If the continuation character is blank, then a blank component
      !>         of a variable is treated as a component with no letters.
      !>         For example:
      !>
      !>            STRINGS = ( 'This is a variable'
      !>                        'with a blank'
      !>                        ' '
      !>                        'component.' )
      !>
      !>         Is equivalent to
      !>
      !>            STRINGS = ( 'This is a variable'
      !>                        'with a blank'
      !>                        'component.' )
      !>
      !>         from the point of view of SEPOOL if CONTIN is set to the
      !>         blank character.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>      The SPICE Kernel Pool provides a very convenient interface
      !>      for supplying both numeric and textual data to user application
      !>      programs. However, any particular component of a character
      !>      valued component of a kernel pool variable is limited to 80
      !>      or fewer characters in length.
      !>
      !>      This routine allows you to overcome this limitation by
      !>      "continuing" a character component of a kernel pool variable.
      !>      To do this you need to select a continuation sequence
      !>      of characters and then insert this sequence as the last non-blank
      !>      set of characters that make up the portion of the component
      !>      that should be continued.
      !>
      !>      For example, you may decide to use the sequence '//' to indicate
      !>      that a string should be continued to the next component of
      !>      a kernel pool variable. Then set up the
      !>      kernel pool variable as shown below
      !>
      !>      LONG_STRINGS = ( 'This is part of the first component //'
      !>                       'that needs more than one line when //'
      !>                       'inserting it into the kernel pool.'
      !>                       'This is the second string that is split //'
      !>                       'up as several components of a kernel pool //'
      !>                       'variable.' )
      !>
      !>      When loaded into the kernel pool, the variable LONG_STRINGS
      !>      will have six literal components:
      !>
      !>         COMPONENT (1) = 'This is part of the first component //'
      !>         COMPONENT (2) = 'that needs more than one line when //'
      !>         COMPONENT (3) = 'inserting it into the kernel pool.'
      !>         COMPONENT (4) = 'This is the second string that is split //'
      !>         COMPONENT (5) = 'up as several components of a kernel pool //'
      !>         COMPONENT (6) = 'variable.'
      !>
      !>      These are the components that would be retrieved by the call
      !>
      !>         CALL GCPOOL ( 'LONG_STRINGS', 1, 6, N, COMPONENT, FOUND )
      !>
      !>      However, using the routine SEPOOL you can view the variable
      !>      LONG_STRINGS as having two long components.
      !>
      !>         STRING (1) = 'This is part of the first component that '
      !>     .   //           'needs more than one line when inserting '
      !>     .   //           'it into the kernel pool. '
      !>
      !>         STRING (2) = 'This is the second string that is split '
      !>     .   //           'up as several components of a kernel pool '
      !>     .   //           'variable. '
      !>
      !>      These string components would be retrieved by the following two
      !>      calls.
      !>
      !>         FIDX = 1
      !>         CALL SEPOOL ( 'LONG_STRINGS', FIDX, '//',
      !>        .                              STRING(1), SIZE, LIDX, FOUND )
      !>         FIDX = LIDX+1
      !>         CALL SEPOOL ( 'LONG_STRINGS', FIDX, '//',
      !>        .                              STRING(2), SIZE, LIDX, FOUND )
      !>
      !>## Examples
      !>
      !>     Example 1. Retrieving file names.
      !>
      !>      Suppose a you have used the kernel pool as a mechanism for
      !>      specifying SPK files to load at startup but that the full
      !>      names of the files are too long to be contained in a single
      !>      text line of a kernel pool assignment.
      !>
      !>      By selecting an appropriate continuation character ('*' for
      !>      example)  you can insert the full names of the SPK files
      !>      into the kernel pool and then retrieve them using this
      !>      routine.
      !>
      !>      First set up the kernel pool specification of the strings
      !>      as shown here:
      !>
      !>            SPK_FILES = ( 'this_is_the_full_path_specification_*'
      !>                          'of_a_file_with_a_long_name'
      !>                          'this_is_the_full_path_specification_*'
      !>                          'of_a_second_file_with_a_very_long_*'
      !>                          'name' )
      !>
      !>      Now to retrieve and load the SPK_FILES one at a time,
      !>      exercise the following loop.
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 255 )
      !>
      !>      CHARACTER*(FILSIZ)    FILE
      !>      INTEGER               I
      !>      INTEGER               LIDX
      !>
      !>      I = 1
      !>
      !>      CALL SEPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, LIDX, FOUND )
      !>
      !>      DO WHILE ( FOUND .AND. RTRIM(FILE) .EQ. SIZE )
      !>
      !>         CALL SPKLEF ( FILE, HANDLE )
      !>         I = LIDX + 1
      !>         CALL SEPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, LIDX, FOUND )
      !>      END DO
      !>
      !>      IF ( FOUND .AND. RTRIM(FILE) .NE. SIZE ) THEN
      !>         WRITE (*,*) 'The ', I, '''th file name was too long.'
      !>      END IF
      !>
      !>      Example 2. Retrieving all components as a string.
      !>
      !>      Occasionally, it may be useful to retrieve the entire
      !>      contents of a kernel pool variable as a single string. To
      !>      do this you can use the blank character as the
      !>      continuation character. For example if you place the
      !>      following assignment in a text kernel
      !>
      !>          COMMENT = (  'This is a long note '
      !>                       ' about the intended '
      !>                       ' use of this text kernel that '
      !>                       ' can be retrieved at run time.' )
      !>
      !>      you can retrieve COMMENT as single string via the call below.
      !>
      !>         CALL SEPOOL ( 'COMMENT', 1, ' ', COMMNT, SIZE, LIDX, FOUND )
      !>
      !>      The result will be that COMMNT will have the following value.
      !>
      !>         COMMNT = 'This is a long note about the intended use of '
      !>     .   //       'this text kernel that can be retrieved at run '
      !>     .   //       'time. '
      !>
      !>      Note that the leading blanks of each component of COMMENT are
      !>      significant, trailing blanks are not significant.
      !>
      !>      If COMMENT had been set as
      !>
      !>          COMMENT = (  'This is a long note '
      !>                       'about the intended '
      !>                       'use of this text kernel that '
      !>                       'can be retrieved at run time.' )
      !>
      !>      Then the call to SEPOOL above would have resulted in several
      !>      words being run together as shown below.
      !>
      !>         COMMNT = 'This is a long noteabout the intendeduse of '
      !>     .   //       'this text kernel thatcan be retrieved at run '
      !>     .   //       'time. '
      !>
      !>      resulted in several words being run together as shown below.
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 12-APR-2012 (WLT) (BVS)
      module subroutine sepool ( ctx, item, fidx, contin, string, size, lidx, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: item  !! name of the kernel pool variable
      integer(ip), intent(in) :: fidx  !! index of the first component of the string
      character(*), intent(in) :: contin  !! character sequence used to indicate continuation
      character(*), intent(out) :: string  !! a full string concatenated across continuations
      integer(ip), intent(out) :: size  !! the number of character in the full string value
      integer(ip), intent(out) :: lidx  !! index of the last component of the string
      logical, intent(out) :: found  !! flag indicating success or failure of request

!     SPICELIB Variables
!

      ! PEPPER: the components are read whole (pool strings have any
      ! length; SPICELIB's are at most 80 characters, the length of PART).
      character(:), allocatable :: part

      integer(ip) :: cfirst
      integer(ip) :: clast
      integer(ip) :: comp
      integer(ip) :: csize
      integer(ip) :: n
      integer(ip) :: putat
      integer(ip) :: room

      logical :: gotit
      logical :: more

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Return empty output if the input index is bad.
!
      if ( fidx < 1 ) then
         found  = .false.
         string = ' '
         size   = 0
         lidx   = 0
         return
      end if

!
!     Check in.
!

!
!     Check if the first component exists. Return empty output if not.
!
      call get_part ( fidx, gotit )

      if ( .not. gotit ) then
         found  = .false.
         string = ' '
         size   = 0
         lidx   = 0
         return
      end if

!
!     Fetch the string using Bill's algorithm from STPOOL 'as is'.
!
      room   = len(string)
      csize  = rtrim(contin)
      putat  = 1

      comp   = fidx
      more   = .true.
      string = ' '
      n      =  0

      do while ( more )

         call get_part ( comp, more )

         if ( more ) then

            found  = .true.

            clast  = rtrim(part)
            cfirst = clast - csize + 1

            if ( cfirst < 0 ) then

               if ( putat <= room ) then
                  string(putat:) = part(1:clast)
               end if

               putat          = putat + clast
               more           = .false.

            else if ( part(cfirst:clast) /= contin ) then

               if ( putat <= room ) then
                  string(putat:) = part(1:clast)
               end if
               putat          = putat + clast
               more           = .false.

            else if ( cfirst > 1 ) then

               if ( putat <= room ) then
                  string(putat:) = part(1:cfirst-1)
               end if
               putat          = putat + cfirst - 1

            end if

         end if

         comp = comp + 1

      end do

!
!     We are done. Get the size of the full string and the index of its
!     last component and checkout.
!
      size = putat - 1
      lidx = comp  - 1

      return
      contains
      !> The component K of the variable (a blank for an empty string).
      subroutine get_part ( k, got )
         integer(ip), intent(in) :: k  !! the component
         logical, intent(out) :: got  !! it exists (a character variable)
         call pool_string ( ctx, item, k, part, got )
         if ( got .and. len ( part ) == 0 ) part = ' '
      end subroutine get_part
      end subroutine sepool

      !> Retrieve the Nth string from a kernel pool variable, where the
      !> string may be continued across several components of the kernel
      !> pool variable.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is the name of a kernel pool variable for which
      !>              the caller wants to retrieve a full (potentially
      !>              continued) string component.
      !>
      !>     NTH      is the number of the string to retrieve from the kernel
      !>              pool. The range of NTH is 1 to the number of full strings
      !>              that are present.
      !>
      !>     CONTIN   is a sequence of characters which (if they appear as the
      !>              last non-blank sequence of characters in a component of a
      !>              value of a kernel pool variable) act as a continuation
      !>              marker: the marker indicates that the string associated
      !>              with the component is continued into the next literal
      !>              component of the kernel pool variable.
      !>
      !>              If CONTIN is blank, all of the components of ITEM will be
      !>              retrieved as a single string.
      !>
      !>## Detailed Output
      !>
      !>     NTHSTR   is the NTH full string associated with the kernel pool
      !>              variable specified by ITEM.
      !>
      !>              Note that if NTHSTR is not sufficiently long to hold the
      !>              fully continued string, the value will be truncated. You
      !>              can determine if NTHSTR has been truncated by examining
      !>              the variable SIZE.
      !>
      !>     SIZE     is the index of last non-blank character of the continued
      !>              string as it is represented in the kernel pool. This is
      !>              the actual number of characters needed to hold the
      !>              requested string. If NTHSTR contains a truncated portion
      !>              of the full string, RTRIM(NTHSTR) will be less than SIZE.
      !>
      !>              If the value of NTHSTR should be a blank, then SIZE will
      !>              be set to 1.
      !>
      !>     FOUND    is a logical variable indicating success of the request
      !>              to retrieve the NTH string associated with ITEM. If an
      !>              Nth string exists, FOUND will be set to .TRUE.;
      !>              otherwise FOUND will be set to .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the variable specified by ITEM is not present in the
      !>         kernel pool or is present but is not character valued,
      !>         NTHSTR will be returned as a blank, SIZE will be
      !>         returned with the value 0 and FOUND will be set to .FALSE. In
      !>         particular if NTH is less than 1, NTHSTR will be returned as a
      !>         blank, SIZE will be zero and FOUND will be .FALSE.
      !>
      !>     2)  If the variable specified has a blank string associated
      !>         with its NTH full string, NTHSTR will be blank, SIZE
      !>         will be 1 and FOUND will be set to .TRUE.
      !>
      !>     3)  If NTHSTR is not long enough to hold all of the characters
      !>         associated with the NTH string, it will be truncated on the
      !>         right.
      !>
      !>     4)  If the continuation character is a blank, every component
      !>         of the variable specified by ITEM will be inserted into
      !>         the output string.
      !>
      !>     5)  If the continuation character is blank, then a blank component
      !>         of a variable is treated as a component with no letters.
      !>         For example:
      !>
      !>            STRINGS = ( 'This is a variable'
      !>                        'with a blank'
      !>                        ' '
      !>                        'component.' )
      !>
      !>         Is equivalent to
      !>
      !>            STRINGS = ( 'This is a variable'
      !>                        'with a blank'
      !>                        'component.' )
      !>
      !>         from the point of view of STPOOL if CONTIN is set to the
      !>         blank character.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The SPICE Kernel Pool provides a very convenient interface
      !>     for supplying both numeric and textual data to user application
      !>     programs. However, any particular component of a character
      !>     valued component of a kernel pool variable is limited to 80
      !>     or fewer characters in length.
      !>
      !>     This routine allows you to overcome this limitation by
      !>     "continuing" a character component of a kernel pool variable.
      !>     To do this you need to select a continuation sequence
      !>     of characters and then insert this sequence as the last non-blank
      !>     set of characters that make up the portion of the component
      !>     that should be continued.
      !>
      !>     For example, you may decide to use the sequence '//' to indicate
      !>     that a string should be continued to the next component of
      !>     a kernel pool variable. Then set up the
      !>     kernel pool variable as shown below
      !>
      !>        LONG_STRINGS = ( 'This is part of the first component //'
      !>                         'that needs more than one line when //'
      !>                         'inserting it into the kernel pool.'
      !>                         'This is the second string that is split //'
      !>                         'up as several components of a kernel pool //'
      !>                         'variable.' )
      !>
      !>     When loaded into the kernel pool, the variable LONG_STRINGS
      !>     will have six literal components:
      !>
      !>        COMPONENT (1) = 'This is part of the first component //'
      !>        COMPONENT (2) = 'that needs more than one line when //'
      !>        COMPONENT (3) = 'inserting it into the kernel pool.'
      !>        COMPONENT (4) = 'This is the second string that is split //'
      !>        COMPONENT (5) = 'up as several components of a kernel pool //'
      !>        COMPONENT (6) = 'variable.'
      !>
      !>     These are the components that would be retrieved by the call
      !>
      !>        CALL GCPOOL ( 'LONG_STRINGS', 1, 6, N, COMPONENT, FOUND )
      !>
      !>     However, using the routine STPOOL you can view the variable
      !>     LONG_STRINGS as having two long components.
      !>
      !>        STRGNA = 'This is part of the first component that '
      !>       . //      'needs more than one line when inserting '
      !>       . //      'it into the kernel pool. '
      !>
      !>        STRGNB = 'This is the second string that is split '
      !>       . //      'up as several components of a kernel pool '
      !>       . //      'variable. '
      !>
      !>     These string components would be retrieved by the following two
      !>     calls.
      !>
      !>        CALL STPOOL ( 'LONG_STRINGS', 1, '//', STRGNA, SIZE, FOUND )
      !>        CALL STPOOL ( 'LONG_STRINGS', 2, '//', STRGNB, SIZE, FOUND )
      !>
      !>## Examples
      !>
      !>     Example 1. Retrieving file names.
      !>
      !>     Suppose a you have used the kernel pool as a mechanism for
      !>     specifying SPK files to load at startup but that the full
      !>     names of the files are too long to be contained in a single
      !>     text line of a kernel pool assignment.
      !>
      !>     By selecting an appropriate continuation character ('*' for
      !>     example)  you can insert the full names of the SPK files
      !>     into the kernel pool and then retrieve them using this
      !>     routine.
      !>
      !>     First set up the kernel pool specification of the strings
      !>     as shown here:
      !>
      !>           SPK_FILES = ( 'this_is_the_full_path_specification_*'
      !>                         'of_a_file_with_a_long_name'
      !>                         'this_is_the_full_path_specification_*'
      !>                         'of_a_second_file_with_a_very_long_*'
      !>                         'name' )
      !>
      !>     Now to retrieve and load the SPK_FILES one at a time,
      !>     exercise the following loop.
      !>
      !>     INTEGER               FILSIZ
      !>     PARAMETER           ( FILSIZ = 255 )
      !>
      !>     CHARACTER*(FILSIZ)    FILE
      !>     INTEGER               I
      !>
      !>     I = 1
      !>
      !>     CALL STPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, FOUND )
      !>
      !>     DO WHILE ( FOUND .AND. RTRIM(FILE) .EQ. SIZE )
      !>
      !>        CALL SPKLEF ( FILE, HANDLE )
      !>        I = I + 1
      !>        CALL STPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, FOUND )
      !>
      !>     END DO
      !>
      !>     IF ( FOUND .AND. RTRIM(FILE) .NE. SIZE ) THEN
      !>
      !>        WRITE (*,*) 'The ', I, '''th file name was too long.'
      !>
      !>     END IF
      !>
      !>     Example 2. Retrieving all components as a string.
      !>
      !>     Occasionally, it may be useful to retrieve the entire
      !>     contents of a kernel pool variable as a single string. To
      !>     do this you can use the blank character as the
      !>     continuation character. For example if you place the
      !>     following assignment in a text kernel
      !>
      !>         COMMENT = (  'This is a long note '
      !>                      ' about the intended '
      !>                      ' use of this text kernel that '
      !>                      ' can be retrieved at run time.' )
      !>
      !>     you can retrieve COMMENT as single string via the call below.
      !>
      !>        CALL STPOOL ( 'COMMENT', 1, ' ', COMMNT, SIZE, FOUND )
      !>
      !>     The result will be that COMMNT will have the following value.
      !>
      !>        COMMNT = 'This is a long note about the intended use of '
      !>       . //      'this text kernel that can be retrieved at run '
      !>       . //      'time. '
      !>
      !>     Note that the leading blanks of each component of COMMENT are
      !>     significant, trailing blanks are not significant.
      !>
      !>     If COMMENT had been set as
      !>
      !>         COMMENT = (  'This is a long note '
      !>                      'about the intended '
      !>                      'use of this text kernel that '
      !>                      'can be retrieved at run time.' )
      !>
      !>     Then the call to STPOOL above would have resulted in several
      !>     words being run together as shown below.
      !>
      !>        COMMNT = 'This is a long noteabout the intendeduse of '
      !>       . //      'this text kernel thatcan be retrieved at run '
      !>       . //      'time. '
      !>
      !>     resulted in several words being run together as shown below.
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Changed the output argument name "STRING" to "NTHSTR" for
      !>        consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 11-JUL-1997 (WLT)
      module subroutine stpool ( ctx, item, nth, contin, nthstr, size, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: item  !! Name of the kernel pool variable.
      integer(ip), intent(in) :: nth  !! Index of the full string to retrieve.
      character(*), intent(in) :: contin  !! Character sequence used to indicate continuation.
      character(*), intent(out) :: nthstr  !! A full string concatenated across continuations.
      integer(ip), intent(out) :: size  !! The number of characters in the full string value.
      logical, intent(out) :: found  !! Flag indicating success or failure of request.

!     SPICELIB Variables
!

      ! PEPPER: the components are read whole (pool strings have any
      ! length; SPICELIB's are at most 80 characters, the length of PART).
      character(:), allocatable :: part

      integer(ip) :: cfirst
      integer(ip) :: clast
      integer(ip) :: comp
      integer(ip) :: csize
      integer(ip) :: n
      integer(ip) :: putat
      integer(ip) :: room
      integer(ip) :: strno

      logical :: gotit
      logical :: more

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      if ( nth < 1 ) then
         found  = .false.
         nthstr = ' '
         size   = 0
         return
      end if

      room  = len(nthstr)
      csize = rtrim(contin)
      putat = 1

!
!     Retrieve components until we've gone past the first NTH-1
!     strings.
!
      strno = 1
      comp  = 1
      found = .false.

      do while ( strno < nth )

         call get_part ( comp, gotit )

         if ( .not. gotit ) then
            nthstr = ' '
            size   = 0
            found  = .false.
            return
         end if

         clast  = rtrim(part)
         cfirst = clast - csize + 1

         if ( cfirst < 0 ) then
            strno = strno + 1
         else if ( part(cfirst:clast) /= contin ) then
            strno = strno + 1
         end if

         comp = comp + 1

      end do

!
!     Once we've reached this point, COMP points to the component
!     of the kernel pool variable that is the beginning of the NTH
!     string.  Now just retrieve components until we run out or
!     one is not continued.
!
      more   = .true.
      nthstr = ' '
      n      =  0

      do while ( more )

         call get_part ( comp, more )

         if ( more ) then

            found  = .true.

            clast  = rtrim(part)
            cfirst = clast - csize + 1

            if ( cfirst < 0 ) then

               if ( putat <= room ) then
                  nthstr(putat:) = part(1:clast)
               end if

               putat          = putat + clast
               more           = .false.

            else if ( part(cfirst:clast) /= contin ) then

               if ( putat <= room ) then
                  nthstr(putat:) = part(1:clast)
               end if
               putat          = putat + clast
               more           = .false.

            else if ( cfirst > 1 ) then

               if ( putat <= room ) then
                  nthstr(putat:) = part(1:cfirst-1)
               end if
               putat          = putat + cfirst - 1

            end if

         end if

         comp = comp + 1

      end do

!
!     We are done.  Get the size of the full string and checkout.
!
      size = putat - 1

      return
      contains
      !> The component K of the variable (a blank for an empty string).
      subroutine get_part ( k, got )
         integer(ip), intent(in) :: k  !! the component
         logical, intent(out) :: got  !! it exists (a character variable)
         call pool_string ( ctx, item, k, part, got )
         if ( got .and. len ( part ) == 0 ) part = ' '
      end subroutine get_part
      end subroutine stpool

      !> Return the latest version string of a given item such as the
      !> Toolkit or a routine name.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is the item for which a version string is to be returned.
      !>              ITEM may be 'TOOLKIT', entry point names, or program
      !>              names. ITEM is case insensitive.
      !>
      !>              Currently, the only ITEM supported is 'TOOLKIT' and it
      !>              will return the toolkit version number.
      !>
      !>              Any other ITEM will return 'No version found.'
      !>
      !>## Detailed Output
      !>
      !>     VERSTR   is the latest version string for the specified ITEM.
      !>
      !>              If ITEM is not one of the items having a version, the
      !>              value 'No version found.' will be returned.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the ITEM whose version string is requested is not
      !>         recognized, the string 'No version found.' is returned.
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
      !>     Suppose you want to find out the recent Toolkit configuration
      !>     version number. Using the code fragment below:
      !>
      !>     CHARACTER*(80)        VERSN
      !>
      !>     CALL TKVRSN ( 'TOOLKIT', VERSN )
      !>
      !>     The variable VERSN would contain a string similar to the one
      !>     shown below:
      !>
      !>        'N0035'
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.16.0, 31-DEC-2021 (WLT)
      !>
      !>        Version update, N0067
      !>
      !>-    SPICELIB Version 3.15.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.14.0, 05-APR-2017 (BVS)
      !>
      !>        Version update, N0066
      !>
      !>-    SPICELIB Version 3.13.0, 15-JUL-2014 (BVS)
      !>
      !>        Version update, N0065
      !>
      !>-    SPICELIB Version 3.12.0, 09-JUN-2010 (BVS)
      !>
      !>        Version update, N0064
      !>
      !>-    SPICELIB Version 3.11.0, 15-APR-2009 (BVS)
      !>
      !>        Version update, N0063
      !>
      !>-    SPICELIB Version 3.10.0, 04-MAR-2008 (BVS)
      !>
      !>        Version update, N0062
      !>
      !>-    SPICELIB Version 3.9.0, 27-NOV-2006 (BVS)
      !>
      !>        Version update, N0061
      !>
      !>-    SPICELIB Version 3.8.0, 16-DEC-2005 (BVS)
      !>
      !>        Version update, N0060
      !>
      !>-    SPICELIB Version 3.7.0, 17-NOV-2005 (BVS)
      !>
      !>        Version update, N0059
      !>
      !>-    SPICELIB Version 3.6.0, 11-JAN-2005 (BVS)
      !>
      !>        Version update, N0058
      !>
      !>-    SPICELIB Version 3.5.0, 02-MAR-2004 (BVS)
      !>
      !>        Version update, N0057
      !>
      !>-    SPICELIB Version 3.4.0, 30-JUL-2003 (BVS)
      !>
      !>        Version update, N0056
      !>
      !>-    SPICELIB Version 3.3.0, 26-FEB-2003 (BVS)
      !>
      !>        Version update, N0055
      !>
      !>-    SPICELIB Version 3.2.0, 13-DEC-2002 (BVS)
      !>
      !>        Version update, N0054
      !>
      !>-    SPICELIB Version 3.1.0, 05-SEP-2002 (BVS)
      !>
      !>        Version update, N0053
      !>
      !>-    SPICELIB Version 3.0.0, 06-FEB-2002 (FST)
      !>
      !>        Version update, N0052a
      !>
      !>-    SPICELIB Version 2.9.0, 17-JAN-2002 (WLT)
      !>
      !>        Version update, N0052
      !>
      !>-    SPICELIB Version 2.8.0, 07-APR-2000 (WLT)
      !>
      !>        Version update, N0051
      !>
      !>-    SPICELIB Version 2.7.0, 06-OCT-1999 (WLT)
      !>
      !>        Version update, N0050
      !>
      !>-    SPICELIB Version 2.6.0, 04-SEP-1998 (WLT)
      !>
      !>        Version update, N0049
      !>
      !>-    SPICELIB Version 2.5.0, 01-MAY-1998 (WLT)
      !>
      !>        Version update, N0048
      !>
      !>-    SPICELIB Version 2.4.0, 31-JUL-1997 (WLT)
      !>
      !>        Version update, N0047
      !>
      !>-    SPICELIB Version 2.3.0, 27-JAN-1997 (WLT)
      !>
      !>        Version update, N0046
      !>
      !>-    SPICELIB Version 2.2.0, 15-OCT-1996 (WLT)
      !>
      !>        Version update, N0045
      !>
      !>-    SPICELIB Version 2.1.0, 26-AUG-1996 (WLT)
      !>
      !>        Version update, N0044
      !>
      !>-    SPICELIB Version 2.0.0, 09-MAY-1996 (KRG)
      !>
      !>        Removed the check of the SPICELIB function RETURN. This
      !>        routine is called by the error handling after an error
      !>        has been signaled to get the toolkit version, so it
      !>        cannot return on entry after an error.
      !>
      !>        The calls to CHKIN and CHKOUT have also been removed to
      !>        completely isolate this subroutine from the error handling.
      !>
      !>        Version update, N0043.
      !>
      !>-    SPICELIB Version 1.7.0, 02-JAN-1996 (WLT)
      !>
      !>        Version update, N0042.
      !>
      !>-    SPICELIB Version 1.6.0, 28-SEP-1995 (HAN)
      !>
      !>        Version update, N0041.
      !>
      !>-    SPICELIB Version 1.5.0, 19-AUG-1995 (HAN)
      !>
      !>        Version update, N0040.
      !>
      !>-    SPICELIB Version 1.4.0, 05-JUN-1995 (HAN)
      !>
      !>        Version update, N0039.
      !>
      !>-    SPICELIB Version 1.3.0, 28-MAR-1995 (HAN)
      !>
      !>        Version update, N0038.
      !>
      !>-    SPICELIB Version 1.2.0, 23-DEC-1994 (HAN)
      !>
      !>        Version update, N0037.
      !>
      !>-    SPICELIB Version 1.1.0, 31-OCT-1994 (HAN)
      !>
      !>        Version update, N0036.
      !>
      !>-    SPICELIB Version 1.0.0, 23-AUG-1994 (HAN)
      module subroutine tkvrsn ( ctx, item, verstr )
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: item  !! Item for which a version string is desired.
      character(*), intent(out) :: verstr  !! Version string.

!
!     At the current time only the TOOLKIT version number is
!     defined.
!
      if ( eqstr( item, 'TOOLKIT' ) ) then
         verstr = 'N0067'
      else
         verstr = 'No version found.'
      end if

      return
      end subroutine tkvrsn

      !> Returns a string indicating the line terminators of an ASCII file
      !> and, if requested, stops execution if the terminator does match
      !> the one that is native to the platform on which the toolkit was
      !> compiled.
      !>
      !>## Detailed Input
      !>
      !>     FILE       the name of the ASCII file to scan for a line
      !>                terminator
      !>
      !>     LINE       a character string of sufficient length to perform the
      !>                line reads from FILE.
      !>
      !>     CHECK      a logical flag that, if set to .TRUE., instructs this
      !>                routine to check terminator that has been determined
      !>                against the one that is native to the platform, on
      !>                which the toolkit was compiled, and to generate error
      !>                if it was not the case. If set to .FALSE., instructs
      !>                the routine to bypass the check.
      !>
      !>## Detailed Output
      !>
      !>     TERMIN     the terminator ID extracted from FILE. The possible
      !>                values:
      !>
      !>                'CR'    - carriage return (Mac classic)
      !>                'LF'    - line feed (Unix)
      !>                'CR-LF' - carriage return and line feed (DOS)
      !>                '?'     - unable to determine, possibly
      !>                          due to an error event
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) A SPICE(STRINGTOOSHORT) error signals if LINE has length less
      !>        than 3.
      !>
      !>     2) A SPICE(FILEOPENFAILED) error signals if the file of interest
      !>        fails to open, i.e. IOSTAT < 0.
      !>
      !>     3) A text kernel found to contain non-native line terminators
      !>        and abort of the run was requested by causes this routine to
      !>        signal the error SPICE(INCOMPATIBLEEOL).
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The function scans a string read from a text file to determine
      !>     the native platform of the file. The functions response is
      !>     unpredictable if it scans a binary file.
      !>
      !>## Examples
      !>
      !>     To return EOL terminator for a given file:
      !>
      !>         CHARACTER*(5)    TERMIN
      !>         CHARACTER*(64)   LINE
      !>
      !>          ... given a file name
      !>          ... and a line long enough to hold a text string
      !>              from FILE
      !>
      !>         CALL ZZASCII( FILE, LINE, .FALSE., TERMIN )
      !>
      !>         CALL TOSTDO( 'FOUND FILE TERMINATOR '//TERMIN )
      !>
      !>     To stop if EOL terminator for a given file, if detected
      !>     successfully, is not native to this platform:
      !>
      !>         CHARACTER*(5)    TERMIN
      !>         CHARACTER*(64)   LINE
      !>
      !>          ... given a file name
      !>          ... and a line long enough to hold a text string
      !>              from FILE
      !>
      !>         CALL ZZASCII( FILE, LINE, .TRUE., TERMIN )
      !>
      !>     If the EOL terminator was not native, the call will generate
      !>     SPICE(INCOMPATIBLEEOL) error.
      !>
      !>## Restrictions
      !>
      !>     1) The terminator detection is not performed if the read from
      !>        the file fails because the file is smaller than the allocated
      !>        LINE size or for any other reason.
      !>
      !>     2) The terminator detection is not possible if the length of the
      !>        first text line in the file exceeds the length of the LINE
      !>        work space.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     E.D. Wright      (JPL)
      !>     B.V. Semenov     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.26.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 1.3.1, 26-OCT-2006 (EDW)
      !>
      !>        Expanded error message explanation the
      !>        routine outputs when the file-of-interest
      !>        includes non-native text line terminators.
      !>
      !>-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 1.0.0, 17-FEB-2004 (EDW) (BVS)
      module subroutine zzascii ( ctx, file, line, check, termin )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqstr, rtrim

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! Name of the text file to scan.
      character(*), intent(inout) :: line  !! The work string for file reads.
      logical, intent(in) :: check  !! Flag directing to check for mismatched EOL.
      character(*), intent(out) :: termin  !! The deduced terminator ID.
      integer(ip), parameter :: eoltsz = 5

!
!     Local variables.
!
      character(eoltsz) :: native

      integer(ip) :: doscnt
      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: maccnt
      integer(ip) :: number
      integer(ip) :: reclen
      integer :: iolen
      integer(ip) :: unxcnt

!
! Can't determine the terminator in RETURN
!     mode.
!
      if ( ctx%failed() ) then
         termin = '?'
         return
      end if

!
!     Check-in to the error system.
!

!
!     Retrieve the native line terminator.
!
      call zzplatfm ( ctx, 'TEXT_FORMAT', native )

!
!     If it is VAX, return immediately with undefined terminator.
!
      if ( eqstr( native, 'VAX' ) ) then

         termin = '?'

         return

      end if

!
!     Set the record length that will be used to read data from
!     the file.
!
      ! PEPPER: the record of LEN(LINE) characters, in the units RECL=
      ! counts (bytes for gfortran; 4-byte words for Intel Fortran without
      ! -assume byterecl): INQUIRE (IOLENGTH=), no compiler option needed
      inquire ( iolength = iolen ) line
      reclen  = iolen

!
!     Check the length of the work string is sufficient to perform the
!     operations. Less than 3 is a no-op.
!
      if ( len(line) < 3 ) then

         termin = '?'

         call ctx%setmsg ( 'Work string lacks sufficient length' &
                    // ' to perform operation.')
         call ctx%sigerr ( 'SPICE(STRINGTOOSHORT)' )
         return

      end if

!
!     Find a free logical unit for file access.
!
!     PEPPER: the unit comes from NEWUNIT= in the OPEN below.

!
!     Open the file for DIRECT access.
!
      open ( newunit               = number  , &
             file               = file( :rtrim(file) ), &
             access             = 'DIRECT', &
             status             = 'OLD'   , &
             recl               = reclen  , &
             action             = 'READ', &
             iostat             = iostat  )

      if ( iostat /= 0 ) then

!
!        The open failed, can't determine the terminator if the routine
!        can't open the file.
!
         termin = '?'

!
!        Execute a close, J.I.C.
!
         close( number )

         call ctx%setmsg ( 'File open failed for file ''$1''. IOSTAT ' &
                     //' value $2.' )
         call ctx%errch  ( '$1', file            )
         call ctx%errint ( '$2', iostat          )
         call ctx%sigerr ( 'SPICE(FILEOPENFAIL)' )
         return

      end if

!
!     Read a line into the LINE variable assigned by the user.
!
      line = ' '

      read ( number, rec=1, iostat=iostat ) line

      if ( iostat /= 0 ) then

!
!        If something went wrong during this read, a part or the whole
!        returned line may contain garbage. Instead of examining it and
!        making wrong determination based on it, set terminator to
!        undefined and return.
!
         termin = '?'

!
!        Execute a close, J.I.C.
!
         close( number )

         return

      end if

!
!     We have a line of text data. Use ICHAR to scan for carriage
!     returns and line feeds and count how may of various recognized
!     line termination sequences are in this line.
!
      doscnt = 0
      unxcnt = 0
      maccnt = 0

      i      = 1

      do while ( i < len(line) )

!
!        Check for ICHAR values of 10 (LF) and 13 (CR).
!
         if      ( ichar(line(i:i)) == 10 ) then

!
!           Found a UNIX line terminator LF.
!
            unxcnt = unxcnt + 1

         else if ( ichar(line(i:i)) == 13 ) then

!
!           Found CR, increment character counter and check
!           the next character.
!
            i = i + 1

            if ( ichar(line(i:i)) == 10 ) then

!
!              Found a DOS line terminator CR+LF.
!
               doscnt = doscnt + 1

            else

!
!              Found a Classic Mac line terminator CR.
!
               maccnt = maccnt + 1

            end if

         end if

         i = i + 1

      end do

!
!     Examine the counters.
!
      if      ( doscnt > 0 .and. &
                unxcnt == 0 .and. &
                maccnt == 0       ) then

!
!        Only DOS terminator counter is non-zero. ID the file as DOS.
!
         termin = 'CR-LF'

      else if ( doscnt == 0 .and. &
                unxcnt > 0 .and. &
                maccnt == 0       ) then

!
!        Only Unix terminator counter is non-zero. ID the file as UNIX.
!
         termin =  'LF'

      else if ( doscnt == 0 .and. &
                unxcnt == 0 .and. &
                maccnt > 0       ) then

!
!        Only Mac terminator counter is non-zero. ID the file as Mac
!        Classic.
!
         termin =  'CR'

      else

!
!        We can get here in two cases. First if the line did not
!        contain any CRs or LFs. Second if the line contained more than
!        one kind of terminators. In either case the format of the file
!        is unclear.
!
         termin = '?'

      end if

!
!     Close the file.
!
      close ( number )

!
!     If we were told check the terminator against the native one, do
!     it.
!
      if ( check ) then

!
!        If the terminator was identified and does not match the native
!        one, error out.
!
         if ( .not. eqstr( termin, native ) .and. &
              .not. eqstr( termin, '?'    )       ) then

            call ctx%setmsg ( 'Text file ''$1'' contains lines ' &
                     //   'terminated with ''$2'' while ' &
                     //   'the expected terminator for ' &
                     //   'this platform is ''$3''. SPICE ' &
                     //   'cannot process the file in the ' &
                     //   'current form. This problem ' &
                     //   'likely occurred because the ' &
                     //   'file was copied in binary mode ' &
                     //   'between operating systems where ' &
                     //   'the operating systems use different ' &
                     //   'text line terminators. Try ' &
                     //   'converting the file to native text ' &
                     //   'form using a utility such as ' &
                     //   'dos2unix or unix2dos.'     )

            call ctx%errch  ( '$1', file   )
            call ctx%errch  ( '$2', termin )
            call ctx%errch  ( '$3', native )
            call ctx%sigerr ( 'SPICE(INCOMPATIBLEEOL)' )
            return

         end if

      end if

      return
      end subroutine zzascii

      !> Determine whether a file exists.
      !>
      !> The function returns the value .TRUE. if the file exists, .FALSE.
      !> otherwise.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of the file in question. This may be any
      !>              unambiguous file name valid on the user's computer, for
      !>              example
      !>
      !>                 '/usr/dir1/dir2/DATA.DAT'
      !>                 './DATA.DAT'
      !>                 'c:\usr\dir1\dir2\data.dat'
      !>
      !>              Environment or shell variables may not be used.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value .TRUE. if the file exists, .FALSE.
      !>     otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the filename is blank, the error SPICE(BLANKFILENAME) is
      !>         signaled.
      !>
      !>     2)  If an I/O error occurs while checking the existence of the
      !>         indicated file, the error SPICE(INQUIREFAILED) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Use the Fortran INQUIRE statement to determine the existence
      !>     of FNAME.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given two arbitrary files (one of them the actual code example
      !>        source file), determine if they exists.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EXISTS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL                 EXISTS
      !>      INTEGER                 RTRIM
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 FILEN
      !>      PARAMETER             ( FILEN = 14 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FILEN)       FNAME  ( 2 )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Define an array of file names.
      !>C
      !>      DATA                    FNAME / 'exists_ex1.txt',
      !>     .                                'exists_ex1.pgm' /
      !>
      !>      DO I = 1, 2
      !>
      !>         IF ( EXISTS ( FNAME(I) ) ) THEN
      !>
      !>            WRITE(*,*) 'The file ', FNAME(I)(:RTRIM(FNAME(I))),
      !>     .                 ' exists.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'Cannot find the file ', FNAME(I)
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
      !>         Cannot find the file exists_ex1.txt
      !>         The file exists_ex1.pgm exists.
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
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.3.0, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>        Changed input argument name FILE to FNAME for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>-    SPICELIB Version 2.2.1, 01-JUL-2014 (NJB)
      !>
      !>        VAX examples were deleted from the header.
      !>
      !>-    SPICELIB Version 2.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The input file name is now "trimmed" of trailing blanks
      !>        before checking its existence.
      !>
      !>-    SPICELIB Version 2.1.0, 04-MAR-1996 (KRG)
      !>
      !>        Added a local logical variable that is used as temporary
      !>        storage for the results from the INQUIRE statement rather
      !>        than using the function name. This solved a problem on the
      !>        macintosh.
      !>
      !>-    SPICELIB Version 2.0.0, 04-AUG-1994 (KRG)
      !>
      !>        Added a test to see if the filename was blank before the
      !>        INQUIRE statement. This allows a meaningful error message to
      !>        be presented.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of
      !>        the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 29-DEC-1988 (HAN)
      !>
      !>        The IOSTAT specifier was added to the INQUIRE statement.
      !>        If the value of IOSTAT is not equal to zero, an error
      !>        occurred during the execution of the INQUIRE statement.
      !>        In this case, a SPICELIB error is signaled and the routine
      !>        checks out.
      module logical function exists ( ctx, fname )
      use pepper_kinds, only: ip
      use pepper_strings, only: rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the file in question.

      integer(ip) :: iostat
      integer(ip) :: r

      logical :: myexst

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         exists = .false.
         return
      end if
!
!     Initialize the local variable MYEXST to be .FALSE.
!
      myexst = .false.

!
!     First we test to see if the filename is blank.
!
      if ( fname == ' ' ) then

         exists = .false.
         call ctx%setmsg ( 'The file name is blank. ' )
         call ctx%sigerr ( 'SPICE(BLANKFILENAME)'     )
         return

      end if

      r = rtrim(fname)
!
!     So simple, it defies explanation.
!
      inquire ( file = fname(1:r), exist = myexst, iostat = iostat )

      if ( iostat /= 0 ) then

         exists = .false.
         call ctx%setmsg ( 'Value of IOSTAT was *.' )
         call ctx%errint ( '*', iostat              )
         call ctx%sigerr ( 'SPICE(INQUIREFAILED)'   )
         return

      end if
!
!     Set the value of the function, return.
!
      exists = myexst

      return
      end function exists

      !> PEPPER: chooses the loader of FILE (the rules: [[spice_furnsh]]): a
      !> registered kernel loader named by `loader`, before any check of the
      !> file; else SPICE's binary kernels by their ID word; else a
      !> registered loader by extension or `detect`; else the text readers.
      !> A file a kernel loader reads is loaded as an SPK built in memory
      !> (FILTYP 'SPK').
      !>
      !> Determine the architecture and type of a file and load
      !> the file into the appropriate SPICE subsystem
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FILE       is the name of a file that is anticipated to
      !>                be a SPICE kernel.
      !>
      !>     NOFILE     is a template for the message that should be created
      !>                with SETMSG if a problem is identified with FILE. The
      !>                message should have the form: "[text] '#' [text] #" The
      !>                first octothorpe ('#') will be replaced by the name of
      !>                the file. The second by a descriptive message.
      !>
      !>## Detailed Output
      !>
      !>     FILTYP     is the type of the kernel as determined by the
      !>                SPICE file record of the file or by various
      !>                heuristics.  Possible return values are:
      !>
      !>                  TEXT   ---  if FILE is interpreted as a text kernel
      !>                              suitable for loading via LDPOOL.  No
      !>                              attempt is made to distinguish between
      !>                              different types of text kernels.
      !>                  SPK   |
      !>                  CK    |
      !>                  PCK   |---  if FILE is a binary PCK file.
      !>                  DSK   |
      !>                  EK    |
      !>
      !>                If a failure occurs during the attempt to load
      !>                the FILE, FILTYP will be returned as the blank string.
      !>
      !>     HANDLE     is the DAF or DAS handle that is associated with the
      !>                file.  If the FILTYP of the file is 'TEXT', HANDLE
      !>                will be set to zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If the specified file does not exist, the error
      !>        SPICE(NOSUCHFILE) will be signaled.
      !>
      !>     2) If the specified file can be identified as unloadable
      !>        because it is a transfer format file, the error
      !>        SPICE(TRANSFERFILE) will be signaled.
      !>
      !>     3) If the specified file can be identified as unloadable
      !>        because it is an obsolete text E-kernel, the error
      !>        SPICE(TYPE1TEXTEK) will be signaled.
      !>
      !>     4) If the specified file can be recognized as a DAF/DAS file
      !>        but is not one of the currently recognized binary kernel
      !>        types, the error SPICE(UNKNOWNKERNELTYPE) will be signaled.
      !>
      !>     5) FILTYP is not sufficiently long to hold the full text of the
      !>        type of the kernel, the value returned will be the truncation
      !>        of the value.  As currently implemented this truncated type is
      !>        sufficient to distinguish between the various types of
      !>        kernels.
      !>
      !>     6) If the FILE cannot be loaded, HANDLE will be set to zero.
      !>
      !>     7) All other problems associated with the loading of FILE
      !>        are diagnosed by the routines called by this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is intended as a supporting routine for the
      !>     SPICE routine FURNSH.  It handles the task of loading
      !>     an arbitrary kernel without the caller having to specify
      !>     the type of the kernel.
      !>
      !>## Examples
      !>
      !>     None.  (After all it's a private routine)
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
      !>     W.L. Taber      (JPL)
      !>     E.D. Wright     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 17-OCT-2021 (BVS)
      !>
      !>        Wrapped KERLIN and TERMIN declarations in FORTRAN environment
      !>        block to silence declared-not-used complaints from f2c.
      !>
      !>-    SPICELIB Version 2.0.0, 20-JAN-2016 (NJB)
      !>
      !>        Added ability to load DSK files.
      !>
      !>-    SPICELIB Version 1.17.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 1.16.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.15.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.14.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.10.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.9.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.8.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.7.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.6.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 1.3.0, 03-OCT-2005 (EDW)
      !>
      !>        Source file zzldker.f converted to master file.
      !>        Modification occurred to prevent f2c's versions
      !>        from making the zzascii test. CSPICE now
      !>        includes coed to allow reading of non native text files.
      !>
      !>-    SPICELIB Version 1.2.0, 17-FEB-2004 (EDW) (BVS)
      !>
      !>        Added the ZZASCII terminator test for text files. Used a
      !>        working line length of 132 characters (maximum text kernel
      !>        line size.)
      !>
      !>-    SPICELIB Version 1.1.0, 24-JUN-2002 (EDW)
      !>
      !>        Added a call to ZZBODKIK to run the
      !>        NAIF_BODY_NAME/CODE read/check routine
      !>        whenever a text kernel loads.
      !>
      !>-    SPICELIB Version 1.0.0, 04-JUN-1999 (WLT)
      module subroutine zzldker ( ctx, file, nofile, filtyp, handle, loader, used, format )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! The name of a file to be loaded.
      character(*), intent(in) :: nofile  !! A message to issue if FILE cannot be located
      character(*), intent(out) :: filtyp  !! The type of kernel.
      integer(ip), intent(out) :: handle  !! The handle associated with the loaded kernel.
      character(*), intent(in), optional :: loader  !! PEPPER: the registered loader that reads FILE, before anything else
                                                    !! (PEPPER: PEPPER's own loading; blank: chosen by the file)
      character(:), allocatable, intent(out), optional :: used  !! PEPPER: the kernel loader that read the file (blank: none)
      character(:), allocatable, intent(out), optional :: format  !! PEPPER: the description of a kernel built by a loader

!
!     Local Variables.
!
      integer(ip), parameter :: wdsize = 32

      character(wdsize) :: versn
      character(wdsize) :: arch
      character(wdsize) :: mytype
      character(:), allocatable :: fmt  ! PEPPER: the description of a kernel made by a loader
      integer(ip) :: k  ! PEPPER: the kernel loader (0: none; -1: PEPPER's own loading, loader='PEPPER')

      if ( ctx%failed() ) return

!
!     PEPPER: a registered kernel loader named by FURNSH's LOADER argument
!     (or the meta-kernel's KERNEL_LOADERS) reads the file, before and
!     instead of anything else: no check that the file exists, of its
!     architecture (a SPICE binary kernel is given to it too), of its
!     extension, or of the other loaders' detect. The loader alone
!     decides, and reports its own errors. The name PEPPER (k = -1) means
!     PEPPER's own loading: the file is loaded as without kernel loaders
!     (SPICE's kernels by their ID word, JSON kernels by their extension,
!     else text kernels and meta-kernels), and registered loaders are not
!     asked. Any other name no loader has: SPICE(UNKNOWNLOADER).
!
      if ( present ( used   ) ) used   = ' '
      if ( present ( format ) ) format = ' '
      k = 0
      if ( present ( loader ) ) then
         if ( loader /= ' ' ) then
            call find_kernel_loader ( ctx, file, loader, k )
            if ( ctx%failed() ) return
         end if
      end if
      if ( k > 0 ) then
         call load_with_loader ( )
         return
      end if

      if ( .not. exists( ctx, file ) ) then

         call ctx%setmsg ( nofile )
         call ctx%errch  ( '#', file )
         call ctx%errch  ( '#', 'could not be located.' )
         call ctx%sigerr ( 'SPICE(NOSUCHFILE)'  )
         return

      end if

!
!     PEPPER: a file a kernel loader read is loaded again (GETFAT would
!     see the kernel built from it): the loader reads it again.
!
      if ( k == 0 .and. synthetic_named ( ctx, file ) > 0 ) then
         call match_kernel_loader ( ctx, file, k )
         if ( ctx%failed() ) return
         if ( k > 0 ) then
            call load_with_loader ( )
            return
         end if
      end if

!
!     PEPPER: from here the file is read from disk: a kernel built in
!     memory with its name (a loader's file loaded again with
!     loader='PEPPER' or no loader matching, an SPK written from a kernel
!     under the kernel's name) is not what GETFAT and DAFOPR see. FURNSH
!     clears the flag after ZZLDKER returns.
!
      ctx%spice%zzddhman%files_only = .true.
      call getfat ( ctx, file, arch, mytype )

!
!     Possible values for the architecture are:
!
!        DAF -- The file is based on the DAF architecture.
!        DAS -- The file is based on the DAS architecture.
!        XFR -- The file is in a SPICE transfer file format.
!        DEC -- The file is an old SPICE decimal text file.
!        ASC -- An ASCII text file.
!        KPL -- Kernel Pool File (i.e., a text kernel)
!        TXT -- An ASCII text file.
!        TE1 -- Text E-Kernel type 1.
!         ?  -- The architecture could not be determined.
!
!     Some of these are obviously losers.
!
      if (      arch == 'XFR' &
           .or. arch == 'DEC' ) then

         call ctx%setmsg ( nofile )
         call ctx%errch  ( '#', file )
         call ctx%errch  ( '#', 'is a transfer format file. ' &
         //                 'Transfer format files cannot be ' &
         //                 'loaded. ' )
         call ctx%sigerr ( 'SPICE(TRANSFERFILE)'  )
         return

      else if ( arch == 'TE1' ) then

         call ctx%setmsg ( nofile )
         call ctx%errch  ( '#', file )
         call ctx%errch  ( '#', 'is a type 1 text E-kernel.  These ' &
         //                 'files are obsolete and cannot be ' &
         //                 'loaded. ' )
         call ctx%sigerr ( 'SPICE(TYPE1TEXTEK)' )
         return

      end if

!
!     That takes care of the obvious errors.  Try loading the
!     kernel.
!
      handle = 0
      filtyp = ' '

      if ( arch == 'DAF' )      then

         if      ( mytype == 'SPK' ) then

            call spklef ( ctx, file, handle )

         else if ( mytype == 'CK' ) then

            call cklpf ( ctx, file, handle )

         else if ( mytype == 'PCK' ) then

            call pcklof ( ctx, file, handle )

         else

            call tkvrsn ( ctx, 'TOOLKIT', versn )
            call ctx%setmsg ( nofile )
            call ctx%errch  ( '#', file )
            call ctx%errch  ( '#', 'is a "#" DAF file. This kind of ' &
            //                 'binary file is not supported in ' &
            //                 'version # of the SPICE toolkit. ' &
            //                 'Check with NAIF to see if your ' &
            //                 'toolkit version is up to date. ' )
            call ctx%errch  ( '#', mytype  )
            call ctx%errch  ( '#', versn )
            call ctx%sigerr ( 'SPICE(UNKNOWNKERNELTYPE)' )
            return

         end if

         filtyp = mytype

!
!        PEPPER: FURNSH's in_memory (or the instance's choice): read the
!        file whole into memory. If that fails, the file is unloaded again.
!
         if ( ctx%spice%in_memory .and. .not. ctx%failed() ) then
            call daf_load_image ( ctx, handle )
            if ( ctx%failed() ) then
               call unload_daf ( mytype, handle )
               return
            end if
         end if

      else if ( arch == 'DAS' ) then

         if ( mytype == 'EK' ) then

            call eklef ( ctx, file, handle )

         else if ( mytype == 'DSK' ) then

            call zzdsklsf ( ctx, file, handle )

         else

            call tkvrsn ( ctx, 'TOOLKIT', versn )
            call ctx%setmsg ( nofile )
            call ctx%errch  ( '#', file )
            call ctx%errch  ( '#', 'is a "#" DAS file.  This kind of ' &
            //                 'binary file is not supported in ' &
            //                 'version # of the SPICE toolkit. ' &
            //                 'Check with NAIF to see if your ' &
            //                 'toolkit version is up to date. ' )
            call ctx%errch  ( '#', mytype  )
            call ctx%errch  ( '#', versn )
            call ctx%sigerr ( 'SPICE(UNKNOWNKERNELTYPE)' )
            return

         end if

         filtyp = mytype

      else

!
!        PEPPER: a file that is not a SPICE binary kernel may be one a
!        registered kernel loader reads (its extension, or its detect).
!
         if ( k == 0 ) then
            call match_kernel_loader ( ctx, file, k )
            if ( ctx%failed() ) return
            if ( k > 0 ) then
               call load_with_loader ( )
               return
            end if
         end if

!
!        PEPPER: SPICELIB checked here (ZZASCII) that the file's line
!        terminators were the platform's, and signaled
!        SPICE(INCOMPATIBLEEOL) otherwise. The text reader (RDTEXT) reads
!        LF and CR-LF files on any platform, as CSPICE always did, so the
!        check is gone.
!
!        Load the file using the text file loader.
!
         call ldpool ( ctx, file )

         if ( .not. ctx%failed() ) then
            filtyp = 'TEXT'
!
!           Cause the kernel pool mechanism to perform
!           the standard error checks on the pool
!           data.
!
            call zzbodkik ( ctx )
         end if

      end if

      return

      contains

      !> PEPPER: load the file with registered kernel loader K: an SPK
      !> built in memory, named FILE.
      subroutine load_with_loader ( )
         ! the loader's kernel is registered under the file's name and
         ! found by it (SPKLEF)
         ctx%spice%zzddhman%files_only = .false.
         call run_kernel_loader ( ctx, k, file, handle, fmt )
         if ( ctx%failed() ) return
         filtyp = 'SPK'
         if ( present ( used   ) ) used   = ctx%spice%loaders(k)%name
         if ( present ( format ) ) format = fmt
      end subroutine load_with_loader

      !> Unload a DAF that was just loaded but could not be read into
      !> memory, keeping the error that was signaled.
      subroutine unload_daf ( kind, han )
         character(*), intent(in) :: kind  !! SPK, CK or PCK
         integer(ip), intent(in) :: han  !! the handle
         character(:), allocatable :: short, long
         short = short_text ( ctx )
         long = long_text ( ctx )
         call ctx%reset()
         select case ( kind )
         case ( 'SPK' )
            call spkuef ( ctx, han )
         case ( 'CK' )
            call ckupf ( ctx, han )
         case ( 'PCK' )
            call pckuof ( ctx, han )
         end select
         call ctx%reset()
         call ctx%setmsg ( long )
         call ctx%sigerr ( short )
      end subroutine unload_daf

      end subroutine zzldker

      !> Load one or more SPICE kernels into a program. Lines of text
      !> kernels and meta-kernels have at most 132 characters; a longer
      !> line is cut without an error (see Restrictions).
      !>
      !> PEPPER: a file named `*.json` (in any case) is a JSON kernel (see
      !> [[pepper_json]]), a text kernel or meta-kernel.
      !>
      !> PEPPER: kernel loaders ([[spice_add_loader]]) read other file
      !> formats: `loader` names the one that reads FILE, overriding every
      !> other choice, or is PEPPER for PEPPER's own loading (registered
      !> loaders not asked); without it FURNSH chooses (SPICE's binary
      !> kernels, a loader's extension, a loader's `detect`, the text
      !> readers). The rules are in [[spice_furnsh]], the method programs
      !> call.
      !>
      !>## Detailed Input
      !>
      !>     FILE     is a SPICE kernel file. The file may be either binary
      !>              or text. If the file is a binary SPICE kernel it will
      !>              be loaded into the appropriate SPICE subsystem. If
      !>              FILE is a SPICE text kernel it will be loaded into the
      !>              kernel pool. If FILE is a SPICE meta-kernel containing
      !>              initialization instructions (through use of the
      !>              correct kernel pool variables), the files specified in
      !>              those variables will be loaded into the appropriate
      !>              SPICE subsystem.
      !>
      !>              The SPICE text kernel format supports association of
      !>              names and data values using a "keyword = value"
      !>              format. The keyword-value pairs thus defined are
      !>              called "kernel variables."
      !>
      !>              While any information can be placed in a text kernel
      !>              file, the following string valued kernel variables are
      !>              recognized by SPICE as meta-kernel keywords:
      !>
      !>                   KERNELS_TO_LOAD
      !>                   PATH_SYMBOLS
      !>                   PATH_VALUES
      !>
      !>              Each kernel variable is discussed below.
      !>
      !>              KERNELS_TO_LOAD   is a list of SPICE kernels to be
      !>                                loaded into a program. If file
      !>                                names do not fit within the kernel
      !>                                pool 80 character limit, they may be
      !>                                continued to subsequent array
      !>                                elements by placing the continuation
      !>                                character ('+') at the end of an
      !>                                element and then placing the
      !>                                remainder of the file name in the
      !>                                next array element. (See the
      !>                                examples below for an illustration
      !>                                of this technique or consult the
      !>                                routine STPOOL for further details.)
      !>
      !>                                You may use one or more PATH_SYMBOL
      !>                                assignments (see below) to specify
      !>                                strings to be substituted for some
      !>                                part of a file name.
      !>
      !>              PATH_SYMBOLS      is a list of strings (without
      !>                                embedded blanks) which if
      !>                                encountered following the '$'
      !>                                character will be replaced with the
      !>                                corresponding PATH_VALUES string.
      !>                                Note that PATH_SYMBOLS are
      !>                                interpreted only in values
      !>                                associated with the KERNELS_TO_LOAD
      !>                                variable. There must be a one-to-one
      !>                                correspondence between the values
      !>                                supplied for PATH_SYMBOLS and
      !>                                PATH_VALUES. For the purpose of
      !>                                determining this correspondence, any
      !>                                path value that is continued over
      !>                                multiple array elements counts as a
      !>                                single value.
      !>
      !>              PATH_VALUES       is a list of expansions to use when
      !>                                PATH_SYMBOLS are encountered. If
      !>                                path values do not fit within the
      !>                                kernel pool 80 character limit, they
      !>                                may be continued in the same way as
      !>                                file names (see the KERNELS_TO_LOAD
      !>                                description above).
      !>
      !>               These kernel pool variables persist within the kernel
      !>               pool only until all kernels associated with the
      !>               variable KERNELS_TO_LOAD have been loaded. Once all
      !>               specified kernels have been loaded, the variables
      !>               KERNELS_TO_LOAD, PATH_SYMBOLS and PATH_VALUES are
      !>               removed from the kernel pool.
      !>
      !>## Detailed Output
      !>
      !>     None. The routine loads various SPICE kernels for use by your
      !>     application.
      !>
      !>## Parameters
      !>
      !>     FILSIZ   is the maximum file name length that can be accommodated
      !>              by the kernel pool.
      !>
      !>     MAXFIL   is the number of entries that can be stored in KEEPER's
      !>              kernel database. In this version of the toolkit MAXFIL
      !>              is set to 5300. Each time a kernel is loaded via
      !>              FURNSH, a database entry is created for that kernel.
      !>              If a meta-kernel is loaded, a database entry is created
      !>              for the meta-kernel itself and for all files referenced
      !>              in the meta-kernel's KERNELS_TO_LOAD specification.
      !>              Unloading a kernel or meta-kernel deletes database
      !>              entries created when the file was loaded.
      !>
      !>              The parameter MAXFIL is an upper bound on number of
      !>              SPICE kernels that can be loaded at any time via the
      !>              KEEPER interface, but the number of kernels that can be
      !>              loaded may be smaller, since re-loading a loaded kernel
      !>              or meta-kernel results in creation of additional
      !>              database entries.
      !>
      !>              Kernels loaded into the KEEPER system are subject to
      !>              constraints imposed by lower-level subsystems. The
      !>              binary kernel systems (SPK, CK, binary PCK, and EK)
      !>              have their own limits on the maximum number of kernels
      !>              that may be loaded.
      !>
      !>              The total number of DAF-based kt%kernels(this set includes
      !>              SPKs, CKs, and binary PCKs)%name and DAS-based kt%kernels(this
      !>              set includes EKs and DSKs)%name that may be loaded at any
      !>              time may not exceed 5000. This limit applies whether
      !>              the files are loaded via FURNSH or lower-level loaders
      !>              such as SPKLEF or DAFOPR. File access performance
      !>              normally will degrade as the number of loaded kernels
      !>              increases.
      !>
      !>## Exceptions
      !>
      !>     1)  If a problem is encountered while trying to load FILE, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the input FILE is a meta-kernel and some file in the
      !>         KERNELS_TO_LOAD assignment cannot be found, or if an error
      !>         occurs while trying to load a file specified by this
      !>         assignment, the error is signaled by a routine in the call
      !>         tree of this routine, and this routine will return. Any files
      !>         loaded prior to encountering the failure, including those
      !>         referenced by the KERNELS_TO_LOAD assignment, will remain
      !>         loaded.
      !>
      !>     3)  If an attempt to load a text kernel fails while the kernel is
      !>         being parsed, any kernel variable assignments made before
      !>         the failure occurred will be retained in the kernel pool.
      !>
      !>     4)  If a PATH_SYMBOLS assignment is specified without a
      !>         corresponding PATH_VALUES assignment, the error
      !>         SPICE(NOPATHVALUE) is signaled.
      !>
      !>     5)  If a meta-text kernel is supplied to FURNSH that contains
      !>         instructions specifying that another meta-text kernel be
      !>         loaded, the error SPICE(RECURSIVELOADING) is signaled.
      !>
      !>     6)  If the input file name has non-blank length exceeding FILSIZ
      !>         characters, the error SPICE(FILENAMETOOLONG) is signaled.
      !>
      !>     7)  If the input file is a meta-kernel and some file in the
      !>         KERNELS_TO_LOAD assignment has name length exceeding FILSIZ
      !>         characters, the error SPICE(FILENAMETOOLONG) is signaled.
      !>
      !>     8)  If the input file is a meta-kernel and some value in the
      !>         PATH_VALUES assignment has length exceeding FILSIZ
      !>         characters, the error SPICE(PATHTOOLONG) is signaled.
      !>
      !>     9)  If the input file is a meta-kernel and some file in the
      !>         KERNELS_TO_LOAD assignment has, after symbol substitution,
      !>         combined name and path length exceeding FILSIZ characters,
      !>         the error SPICE(FILENAMETOOLONG) is signaled.
      !>
      !>     10) If a kernel pool variable name length exceeds its maximum
      !>         allowed length (see Kernel Required Reading, kernel.req), an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     The input FILE is examined and loaded into the appropriate SPICE
      !>     subsystem. If the file is a meta-kernel, any kernels specified
      !>     by the KERNELS_TO_LOAD keyword (and if present, the PATH_SYMBOLS
      !>     and PATH_VALUES keywords) are loaded as well.
      !>
      !>## Particulars
      !>
      !>     This routine provides a uniform interface to the SPICE kernel
      !>     loading systems. It allows you to easily assemble a list of
      !>     SPICE kernels required by your application and to modify that set
      !>     without modifying the source code of programs that make use of
      !>     these kernels.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Load the leapseconds kernel naif0007.tls and the planetary
      !>        ephemeris SPK file de405s.bsp.
      !>
      !>           CALL FURNSH ( 'naif0007.tls' )
      !>           CALL FURNSH ( 'de405s.bsp'   )
      !>
      !>     2) This example illustrates how you could create a meta-kernel
      !>        file for a program that requires several text and binary
      !>        kernels.
      !>
      !>        First create a list of the kernels you need in a text file as
      !>        shown below.
      !>
      !>           KPL/MK
      !>
      !>           File name: furnsh_ex2.tm
      !>
      !>           Here are the SPICE kernels required for my application
      !>           program.
      !>
      !>           Note that kernels are loaded in the order listed. Thus
      !>           we need to list the highest priority kernel last.
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = (
      !>
      !>              '/home/mydir/kernels/spk/lowest_priority.bsp',
      !>              '/home/mydir/kernels/spk/next_priority.bsp',
      !>              '/home/mydir/kernels/spk/highest_priority.bsp',
      !>              '/home/mydir/kernels/text/leapsecond.ker',
      !>              '/home/mydir/kernels+',
      !>              '/custom+',
      !>              '/kernel_data/constants.ker',
      !>              '/home/mydir/kernels/text/sclk.tsc',
      !>              '/home/mydir/kernels/ck/c-kernel.bc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Note that the file name
      !>
      !>           /home/mydir/kernels/custom/kernel_data/constants.ker
      !>
      !>        is continued across several lines in the right hand side of
      !>        the assignment of the kernel variable KERNELS_TO_LOAD.
      !>
      !>        Once you've created your list of kernels, call FURNSH near the
      !>        beginning of your application program to load the meta-kernel
      !>        automatically at program start up.
      !>
      !>           CALL FURNSH ( 'furnsh_ex2.tm' )
      !>
      !>        This will cause each of the kernels listed in your meta-kernel
      !>        to be loaded.
      !>
      !>     3) This example illustrates how you can simplify the previous
      !>        kernel list by using PATH_SYMBOLS.
      !>
      !>           KPL/MK
      !>
      !>           File name: furnsh_ex3.tm
      !>
      !>           Here are the SPICE kernels required for my application
      !>           program.
      !>
      !>           We are going to let A substitute for the directory that
      !>           contains SPK files; B substitute for the directory that
      !>           contains C-kernels; and C substitute for the directory that
      !>           contains text kernels. And we'll let D substitute for
      !>           a "custom" directory that contains a special planetary
      !>           constants kernel made just for our mission.
      !>
      !>           Note that our PATH_VALUES and the corresponding
      !>           PATH_SYMBOLS must be listed in the same order.
      !>
      !>           \begindata
      !>
      !>           PATH_VALUES  = ( '/home/mydir/kernels/spk',
      !>                            '/home/mydir/kernels/ck',
      !>                            '/home/mydir/kernels/text',
      !>                            '/home/mydir/kernels/custom/kernel_data' )
      !>
      !>           PATH_SYMBOLS = ( 'A',
      !>                            'B',
      !>                            'C',
      !>                            'D'  )
      !>
      !>           KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
      !>                                '$A/next_priority.bsp',
      !>                                '$A/highest_priority.bsp',
      !>                                '$C/leapsecond.ker',
      !>                                '$D/constants.ker',
      !>                                '$C/sclk.tsc',
      !>                                '$B/c-kernel.bc'         )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>     4) This example illustrates continuation of path values. The
      !>        meta-kernel shown here is a modified version of that from
      !>        example 3.
      !>
      !>           KPL/MK
      !>
      !>           File name: furnsh_ex4.tm
      !>
      !>           Here are the SPICE kernels required for my application
      !>           program.
      !>
      !>           We are going to let A substitute for the directory that
      !>           contains SPK files; B substitute for the directory that
      !>           contains C-kernels; and C substitute for the directory that
      !>           contains text kernels. And we'll let D substitute for
      !>           a "custom" directory that contains a special planetary
      !>           constants kernel made just for our mission.
      !>
      !>           Note that our PATH_VALUES and the corresponding
      !>           PATH_SYMBOLS must be listed in the same order.
      !>
      !>           The values for path symbols A and D are continued over
      !>           multiple lines.
      !>
      !>           \begindata
      !>
      !>           PATH_VALUES  = ( '/very_long_top_level_path_name/mydir/+',
      !>                            'kernels/spk',
      !>                            '/home/mydir/kernels/ck',
      !>                            '/home/mydir/kernels/text',
      !>                            '/very_long_top_level_path_name+',
      !>                            '/mydir/kernels/custom+',
      !>                            '/kernel_data'                )
      !>
      !>           PATH_SYMBOLS = ( 'A',
      !>                            'B',
      !>                            'C',
      !>                            'D'  )
      !>
      !>           KERNELS_TO_LOAD = (  '$A/lowest_priority.bsp',
      !>                                '$A/next_priority.bsp',
      !>                                '$A/highest_priority.bsp',
      !>                                '$C/leapsecond.ker',
      !>                                '$D/constants.ker',
      !>                                '$C/sclk.tsc',
      !>                                '$B/c-kernel.bc'         )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>     5) Load a meta-kernel containing three kernels, and separately,
      !>        a text kernel and a binary PCK. Count the number of loaded
      !>        files before and after calling KCLEAR.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: furnsh_ex5.tm
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
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00009.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls',
      !>                                  'pck00009.tpc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Use the PCK kernel below as the binary PCK required for the
      !>        example.
      !>
      !>           earth_latest_high_prec.bpc
      !>
      !>        Use the FK kernel below as the text kernel required for the
      !>        example.
      !>
      !>           RSSD0002.TF
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM FURNSH_EX5
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               COUNT
      !>
      !>C
      !>C     Load several kernel files.
      !>C
      !>      CALL FURNSH ( 'furnsh_ex5.tm'              )
      !>      CALL FURNSH ( 'RSSD0002.TF'                )
      !>      CALL FURNSH ( 'earth_latest_high_prec.bpc' )
      !>
      !>C
      !>C     Count the number of loaded kernel files.
      !>C
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'final FURNSH: ', COUNT
      !>
      !>C
      !>C     Clear the KEEPER system, retrieve the number of loaded
      !>C     after the clear.
      !>C
      !>      CALL KCLEAR()
      !>
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'KCLEAR      : ', COUNT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        The total number of kernels after final FURNSH:  6
      !>        The total number of kernels after KCLEAR      :  0
      !>
      !>## Restrictions
      !>
      !>     PEPPER: a line of a text kernel or meta-kernel may have at most
      !>     132 characters (NAIF's text kernel rule). A longer line is cut to
      !>     132 characters without an error, as in SPICELIB: a value or name
      !>     that continues past column 132 is lost or misread. Split long
      !>     values over several lines.
      !>
      !>     1)  A meta-kernel cannot reference another meta-kernel.
      !>
      !>     2)  Failure during an attempt to load a text kernel or a
      !>         meta-kernel can result in a subset of the intended kernel
      !>         variables being set or a subset of the intended files
      !>         being loaded. FURNSH does not "clean up" so as to undo the
      !>         effects of a failed load operation.
      !>
      !>     3)  When a kernel is specified with a relative path, this path
      !>         should be valid at the time when FURNSH is called and stay
      !>         valid for the rest of the application run. This is required
      !>         because SPICE stores kernel names as provided by the caller
      !>         and uses them to open and close binary kernels as needed
      !>         by the DAF/DAS handle manager subsystem (behind the scenes,
      !>         to allow reading many more binary kernels than available
      !>         logical units), and to automatically reload into the POOL
      !>         the rest of text kernels that should stay loaded when a
      !>         particular text kernel is unloaded.
      !>
      !>         Changing the working directory from within an application
      !>         during an application run after calling FURNSH to load
      !>         kernels specified using relative paths is likely to
      !>         invalidate stored paths and prevent open/close and unload
      !>         operations mentioned above. A simple workaround when this
      !>         is needed is to specify kernels using absolute paths.
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.1, 08-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Created complete code example from existing code fragments.
      !>
      !>        Added KERNEL to $Required_Reading section.
      !>
      !>        Added FILSIZ to the $Declarations section.
      !>
      !>        Added a restriction about specifying kernels using relative
      !>        paths to the $Restrictions section.
      !>
      !>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
      !>
      !>        Updated to support use of DSKs.
      !>
      !>        Updated description of MAXFIL in the header.
      !>
      !>        Updated the $Author_and_Institution section.
      !>
      !>-    SPICELIB Version 4.1.0, 01-JUL-2014 (NJB) (BVS)
      !>
      !>        Updated discussion of partially completed kernel loading.
      !>
      !>     Last update was 12-APR-2012 (BVS)
      !>
      !>        Changed to use SEPOOL instead of STPOOL to reduce loading time
      !>        for large meta-kernels due to n^2 delay in STPOOL.
      !>
      !>-    SPICELIB Version 4.0.1, 10-FEB-2010 (EDW)
      !>
      !>        Added mention of the restriction on kernel pool variable
      !>        names to MAXLEN (defined in pool.f) characters or less.
      !>
      !>-    SPICELIB Version 4.0.0, 02-APR-2009 (NJB)
      !>
      !>        Continued path values are now supported. FURNSH now rejects
      !>        file names longer than FILSIZ characters.
      !>
      !>-    SPICELIB Version 2.0.3, 27-APR-2007 (NJB)
      !>
      !>        Fixed header typo: added quotes to literal string
      !>        input arguments in example FURNSH calls.
      !>
      !>-    SPICELIB Version 2.0.2, 15-NOV-2006 (NJB)
      !>
      !>        Added description of parameter MAXFIL to header.
      !>
      !>-    SPICELIB Version 2.0.1, 29-JUL-2003 (NJB) (CHA)
      !>
      !>        Numerous updates to improve clarity. Some corrections were
      !>        made.
      !>
      !>-    SPICELIB Version 2.0.0, 23-AUG-2001 (WLT)
      !>
      !>        Added a call to CVPOOL in FURNSH so that watches that are
      !>        triggered are triggered by loading Meta-kernels and not by
      !>        some external interaction with the kernel pool.
      !>
      !>-    SPICELIB Version 1.1.0, 19-SEP-2000 (WLT)
      !>
      !>        Corrected the error message template used
      !>        by ZZLDKER
      !>
      !>-    SPICELIB Version 1.0.1, 16-DEC-1999 (NJB)
      !>
      !>        Documentation fix: corrected second code example in the
      !>        header of this entry point. The example previously used the
      !>        kernel variable PATH_NAMES; that name has been replaced with
      !>        the correct name PATH_VALUES.
      !>
      !>-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
      module subroutine furnsh ( ctx, file, loader )
      use pepper_kinds, only: ip
      use pepper_strings, only: pos, rtrim, samsub, repmc_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! SPICE kernel file (text or binary).
      character(*), intent(in), optional :: loader  !! PEPPER: the registered kernel loader that reads FILE, overriding
                                                    !! every other choice ([[spice_furnsh]]; blank or absent: FURNSH chooses)
      character(1) :: norc
      character(:), allocatable :: fil2ld
      character(:), allocatable :: pvalue
      character(1) :: probe
      character(lnsize) :: symbol
      character(msgsiz) :: nofile
      character(typlen) :: thstyp
      integer(ip) :: b
      integer(ip) :: cursrc
      integer(ip) :: d
      integer(ip) :: dollar
      integer(ip) :: e
      integer(ip) :: fidx
      integer(ip) :: filnum
      integer(ip) :: fnmlen
      integer(ip) :: hits
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lidx
      integer(ip) :: myhand
      integer(ip) :: n
      integer(ip) :: n1
      integer(ip) :: n2
      integer(ip) :: n3
      integer(ip) :: nmult
      integer(ip) :: npaths
      integer(ip) :: npvals
      integer(ip) :: r
      integer(ip) :: size
      integer(ip) :: src
      integer(ip) :: start
      integer(ip) :: use
      logical :: add
      logical :: didck
      logical :: diddsk
      logical :: didek
      logical :: didpck
      logical :: didspk
      logical :: didtxt
      logical :: dock
      logical :: dodsk
      logical :: doek
      logical :: dometa
      logical :: dopck
      logical :: dospk
      logical :: dotext
      logical :: fnd
      logical :: gotit
      logical :: ok
      logical :: paths
      logical :: single
      logical :: update
      type(string), allocatable :: kload(:)  ! PEPPER: the meta-kernel's KERNEL_LOADERS
      character(:), allocatable :: raw  ! PEPPER: the file as written in KERNELS_TO_LOAD
      character(:), allocatable :: lname  ! PEPPER: the loader KERNEL_LOADERS names for it
      character(:), allocatable :: used  ! PEPPER: the kernel loader that read a file
      character(:), allocatable :: fmt  ! PEPPER: the description of a kernel made by a loader
      associate ( kt => ctx%spice%keeper, &
                  loaded => ctx%spice%keeper%loaded, &
                  known => ctx%spice%keeper%known, &
                  first => ctx%spice%keeper%first )
      if ( ctx%failed() ) return

      if ( first ) then

         first    = .false.
         known(1) = 'KERNELS_TO_LOAD'
         known(2) = 'PATH_SYMBOLS'
         known(3) = 'PATH_VALUES'
         loaded   =  0

         call swpool ( ctx, 'FURNSH', nknown, known )
         call cvpool ( ctx, 'FURNSH', update )

      end if

      ! PEPPER: file names have no length limit (SPICELIB signaled
      ! SPICE(FILENAMETOOLONG) for more than FILSIZ, 255, characters).

!
!     We don't want external interactions with the kernel pool to
!     have any affect on FURNSH's watch so we check the watcher
!     here prior to the call to ZZLDKER.
!
      call cvpool ( ctx, 'FURNSH', update )

!
!     Set a preliminary value for the error message in case the
!     call to ZZLDKER doesn't succeed.
!
      nofile = 'The attempt to load "#" by the routine FURNSH ' &
      //       'failed. It #'

      !
      !     PEPPER: ZZDDHF2H may keep its result for the file during the
      !     load (GETFAT and ZZDDHOPN check the same file; see zzddhman_state).
      !
      ctx%spice%zzddhman%reuse_f2h = .true.
      call zzldker ( ctx, file, nofile, thstyp, myhand, loader, used, fmt )
      ctx%spice%zzddhman%reuse_f2h = .false.
      ctx%spice%zzddhman%files_only = .false.

      if ( ctx%failed() ) return

      call kt%add ( keeper_kernel ( name = trim ( file ), type = thstyp, handle = myhand, source = 0, &
                                    loader = used, format = fmt ) )
      cursrc = loaded

       call cvpool ( ctx, 'FURNSH', update )

      if ( .not. update ) then
!
!        Nothing to do.  None of the control variables
!        were set in FILE.
!
         return

      end if

!
!     See what is present in the kernel pool: Are any path symbols
!     defined?
!
      call dtpool ( ctx, 'PATH_SYMBOLS', paths, npaths, norc )

      if (  paths .and. ( norc == 'C' )  ) then
!
!        Make sure that the values are equal in number. We need to
!        use STPOOL to count the path values, since some of them
!        might span multiple array elements.
!
         i = 1
         call stpool( ctx, 'PATH_VALUES', i, '+', probe, size, ok )

         do while (  ok  .and.  ( .not. ctx%failed() )  )
!
!           Reject excessively long path names.
!
            ! PEPPER: path values have no length limit (SPICELIB signaled
            ! SPICE(PATHTOOLONG) for more than FILSIZ, 255, characters).

            i = i + 1
            call stpool( ctx, 'PATH_VALUES', i, '+', probe, size, ok )

         end do

         if ( ctx%failed() ) return

         npvals = i - 1

         if ( npvals /= npaths ) then

            call ctx%setmsg ( 'Number of path symbols is #; number of ' &
            //            'path values is #; counts must match.'  )
            call ctx%errint ( '#', npaths                             )
            call ctx%errint ( '#', npvals                             )
            call ctx%sigerr ( 'SPICE(PATHMISMATCH)'                   )
            return

         end if

      else

         paths = .false.

      end if

!
!     This kernel appears to be a legitimate meta-text kernel. Mark
!     it as such and then process its contents.
!
      kt%kernels(loaded)%type = 'META'

!
!     PEPPER: the kernel loaders the meta-kernel names for its files
!     (KERNEL_LOADERS: pairs of a file, as written in KERNELS_TO_LOAD,
!     and a loader's name; see spice_add_loader).
!
      call meta_kernel_loaders ( ctx, file, kload )
      if ( ctx%failed() ) return

!
!     Now load all kernels specified in the KERNELS_TO_LOAD variable.
!
      filnum = 1
      fidx   = 1

      call get_file_name ( )

      do while (  ok  .and.  ( .not. ctx%failed() )  )
!
!        Reject excessively long file names.
!
         ! PEPPER: file names have no length limit (SPICELIB signaled
         ! SPICE(FILENAMETOOLONG) for more than FILSIZ, 255, characters).

!
!        Make sure we have room to load at least one more file.
!

!
!        Resolve any path symbols that may be present.
!        Make sure we have room to load at least one more file.
!
         raw = trim ( fil2ld )
         if ( paths ) then

            start  = 1
            dollar = pos( fil2ld, '$', start )

            do while ( dollar > 0 )
!
!              Determine the longest path symbol that fits into the
!              current file name.  We fetch path symbols one at a
!              time and see if they match the portion of the
!              string that follows the '$'.  The longest match
!              is the one we use as a symbol.
!
               size = 0
               use  = 0
               d    = dollar

               do i = 1, npaths

                  call gcpool( ctx, 'PATH_SYMBOLS', i, 1, n, symbol, fnd )

                  r =  rtrim ( symbol )

                  if (       r  > size &
                       .and. samsub( symbol,1,r, fil2ld,d+1,d+r ) ) then
                     use  = i
                     size = r
                  end if

               end do
!
!              If we found a matching path symbol, get the corresponding
!              value and put it into the file name.
!
               if ( use > 0 ) then
!
!                 Get the path value having index USE in the set of
!                 path values. Note that we've already checked that
!                 the path value will fit in PVALUE.
!
                  call stpool( ctx, 'PATH_VALUES', use, '+', probe, n, fnd )
                  if ( allocated ( pvalue ) ) deallocate ( pvalue )
                  allocate ( character(max(1_ip, n)) :: pvalue )
                  call stpool( ctx, 'PATH_VALUES', use, '+', pvalue, n, fnd )

!
!                 When the path is substituted for the symbol, the
!                 total length of the path and file name must fit in
!                 the name buffer.
!
                  ! PEPPER: the combined path and file name has no length limit
                  ! (SPICELIB signaled SPICE(FILENAMETOOLONG) for more than FILSIZ,
                  ! 255, characters).

                  fil2ld = fil2ld(1:d-1) // pvalue(1:n) // fil2ld(d+size+1:)

               end if
!
!              Look for the next occurrence of a '$' after the last
!              place we found one.
!
               start  = dollar + 1
               dollar = pos( fil2ld, '$', start )

            end do

         end if
!
!        If any path symbols were present, they have now been
!        resolved.  Let ZZLDKER handle the task of loading this
!        kernel.  Make up a message template for use if ZZLDKER
!        runs into a problem.
!
         call repmot ( ctx, 'The @ file ''#'' specified by KERNELS_TO_LOAD ' &
                       //   'in the file @ #', '@', filnum, 'L', nofile )
         call repmc_inplace ( nofile, '@', file )

         !
         !     PEPPER: ZZDDHF2H may keep its result for the file during the
         !     load (GETFAT and ZZDDHOPN check the same file; see zzddhman_state).
         !
         ctx%spice%zzddhman%reuse_f2h = .true.
         call meta_kernel_loader ( kload, raw, trim ( fil2ld ), lname )
         call zzldker( ctx, fil2ld, nofile, thstyp, myhand, lname, used, fmt )
         ctx%spice%zzddhman%reuse_f2h = .false.
         ctx%spice%zzddhman%files_only = .false.

         if ( ctx%failed() ) return

         if ( thstyp == 'TEXT' ) then
!
!           See if we stepped on any of the recognized variables.  If
!           we did, there's no point in trying to continue.
!
            call cvpool ( ctx, 'FURNSH', update )

            if ( update ) then
!
!              First clean up the debris created by this attempt
!              at recursion.
!
               do i = 1, nknown
                  call dvpool ( ctx, known(i) )
               end do
!
!              Take care of any watcher activation caused by the
!              mop-up of the preceding loop.
!
               call cvpool ( ctx, 'FURNSH', update )

               call ctx%setmsg ( 'Hmmm.  This is interesting. In the ' &
               //            'meta-text kernel ''#'' you''ve ' &
               //            'requested that the text kernel ' &
               //            '''#'' be loaded. This second file ' &
               //            'is also a "meta-text" kernel and ' &
               //            'specifies new kernel loading ' &
               //            'instructions. Although you receive ' &
               //            'high marks for creativity, this ' &
               //            'path is fraught with peril and can ' &
               //            'not be supported by FURNSH. ' )

               call ctx%errch  ( '#', file   )
               call ctx%errch  ( '#', fil2ld )
               call ctx%sigerr ( 'SPICE(RECURSIVELOADING)' )
               return
            end if

         end if
!
!        Add the latest file loaded to our database of loaded
!        files.
!
         call kt%add ( keeper_kernel ( name = fil2ld, type = thstyp, handle = myhand, source = cursrc, &
                                       loader = used, format = fmt ) )

!
!        Get the name of the next file to load.
!
         filnum = filnum + 1
         fidx   = lidx   + 1

         call get_file_name ( )

      end do

!
!     Last Step.  Remove the special variables from the kernel pool.
!
      do i = 1, nknown
         call dvpool ( ctx, known(i) )
      end do
      call dvpool ( ctx, 'KERNEL_LOADERS' )

      call cvpool ( ctx, 'FURNSH', update )

      return
      end associate

      contains

!
!        PEPPER: get the file name starting at element FIDX of
!        KERNELS_TO_LOAD (SEPOOL) at its full length: FIL2LD has FNMLEN
!        characters. LIDX and OK are as returned by SEPOOL.
!
         subroutine get_file_name ( )
            call sepool ( ctx, 'KERNELS_TO_LOAD', fidx, '+', probe, fnmlen, lidx, ok )
            if ( allocated ( fil2ld ) ) deallocate ( fil2ld )
            allocate ( character(max(1_ip, fnmlen)) :: fil2ld )
            if ( ok ) then
               call sepool ( ctx, 'KERNELS_TO_LOAD', fidx, '+', fil2ld, fnmlen, lidx, ok )
            else
               fil2ld = ' '
            end if
         end subroutine get_file_name
      end subroutine furnsh

      !> Return the number of kernels of a specified type that are
      !> currently loaded via the FURNSH interface.
      !>
      !>## Detailed Input
      !>
      !>     KIND     is a list of types of kernels to count when computing
      !>              loaded kernels. KIND should consist of a list of words of
      !>              kernels to examine. Recognized types are
      !>
      !>                 SPK  --- All SPK files are counted in the total.
      !>                 CK   --- All CK files are counted in the total.
      !>                 PCK  --- All binary PCK files are counted in the
      !>                          total.
      !>                 DSK  --- All DSK files are counted in the total.
      !>                 EK   --- All EK files are counted in the total.
      !>                 TEXT --- All text kernels that are not meta-text
      !>                          kernels are included in the total.
      !>                 META --- All meta-text kernels are counted in the
      !>                          total.
      !>                 ALL  --- Every type of kernel is counted in the
      !>                          total.
      !>
      !>              KIND is case insensitive. If a word appears in KIND
      !>              that is not one of those listed above, it is ignored.
      !>
      !>              When KIND consists of multiple words, the words must
      !>              be separated by blanks. Examples of valid lists are the
      !>              strings
      !>
      !>                 'SPK CK TEXT'
      !>                 'SPK CK text'
      !>                 'PCK DSK'
      !>                 'CK'
      !>                 'ALL'
      !>
      !>              See the $Examples section for illustrations of the
      !>              use of KIND.
      !>
      !>## Detailed Output
      !>
      !>     COUNT    is the number of kernels loaded through FURNSH that
      !>              belong to the list specified by KIND.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If a word on the list specified by KIND is not recognized,
      !>         it is ignored.
      !>
      !>     2)  If KIND is blank, or none of the words in KIND is on the
      !>         list specified above, COUNT will be returned as zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     KTOTAL allows you to easily determine the number of kernels
      !>     loaded via the interface FURNSH that are of a type of interest.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Load a meta-kernel with a PCK, an LSK and an SPK, and
      !>        separately, a text kernel and a binary PCK. Show the
      !>        total number of kernels and meta-kernels loaded. Determine the
      !>        number of text kernels loaded, and the number of binary
      !>        kernels.
      !>
      !>        Unload all kernels and clear the kernel pool using
      !>        KCLEAR, and check that no kernels are loaded.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ktotal_ex1.tm
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
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00009.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls',
      !>                                  'pck00009.tpc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Use the PCK kernel below as the binary PCK required for the
      !>        example.
      !>
      !>           earth_latest_high_prec.bpc
      !>
      !>        Use the FK kernel below as the text kernel required for the
      !>        example.
      !>
      !>           RSSD0002.TF
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM KTOTAL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               COUNT
      !>
      !>C
      !>C     Load several kernel files.
      !>C
      !>      CALL FURNSH ( 'ktotal_ex1.tm'              )
      !>      CALL FURNSH ( 'RSSD0002.TF'                )
      !>      CALL FURNSH ( 'earth_latest_high_prec.bpc' )
      !>
      !>C
      !>C     Count the number of loaded kernel files.
      !>C
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'final FURNSH: ', COUNT
      !>
      !>C
      !>C     Count the number of meta-kernels.
      !>C
      !>      CALL KTOTAL ( 'META', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of meta-kernels  '
      !>     . //               '            : ', COUNT
      !>
      !>C
      !>C     Count the number of text kernels.
      !>C
      !>      CALL KTOTAL ( 'TEXT', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of text kernels  '
      !>     . //               '            : ', COUNT
      !>
      !>C
      !>C     Count the number of binary kernels. These kernels
      !>C     are of type CK, DSK, EK, PCK or SPK.
      !>C
      !>      CALL KTOTAL ( 'CK DSK EK PCK SPK', COUNT )
      !>      WRITE(*,'(A,I2)') 'The total number of binary kernels'
      !>     . //               '            : ', COUNT
      !>
      !>C
      !>C     Clear the KEEPER system, retrieve the number of loaded
      !>C     after the clear.
      !>C
      !>      CALL KCLEAR()
      !>
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'KCLEAR      : ', COUNT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        The total number of kernels after final FURNSH:  6
      !>        The total number of meta-kernels              :  1
      !>        The total number of text kernels              :  3
      !>        The total number of binary kernels            :  2
      !>        The total number of kernels after KCLEAR      :  0
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.1, 25-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Created complete code example from existing code fragments.
      !>
      !>        Updated $Detailed_Input description of input argument KIND to
      !>        illustrate use of multi-word lists. Added KERNEL to the list
      !>        of required readings.
      !>
      !>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
      !>
      !>        Updated to support use of DSKs.
      !>
      !>        Updated the $Author_and_Institution section.
      !>
      !>-    SPICELIB Version 1.1.0, 02-APR-2009 (NJB)
      !>
      !>        Deleted reference to unneeded variable DOALL.
      !>
      !>-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
      module subroutine ktotal ( ctx, kind, count )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqstr, fndnwd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: kind  !! A list of kinds of kernels to count.
      integer(ip), intent(out) :: count  !! The number of kernels of type KIND.
      logical :: return
      character(1) :: norc
      character(lnsize) :: symbol
      character(msgsiz) :: nofile
      character(typlen) :: thstyp
      integer(ip) :: b
      integer(ip) :: cursrc
      integer(ip) :: d
      integer(ip) :: dollar
      integer(ip) :: e
      integer(ip) :: fidx
      integer(ip) :: filnum
      integer(ip) :: fnmlen
      integer(ip) :: hits
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lidx
      integer(ip) :: myhand
      integer(ip) :: n
      integer(ip) :: n1
      integer(ip) :: n2
      integer(ip) :: n3
      integer(ip) :: nmult
      integer(ip) :: npaths
      integer(ip) :: npvals
      integer(ip) :: r
      integer(ip) :: size
      integer(ip) :: src
      integer(ip) :: start
      integer(ip) :: use
      logical :: add
      logical :: didck
      logical :: diddsk
      logical :: didek
      logical :: didpck
      logical :: didspk
      logical :: didtxt
      logical :: dock
      logical :: dodsk
      logical :: doek
      logical :: dometa
      logical :: dopck
      logical :: dospk
      logical :: dotext
      logical :: fnd
      logical :: gotit
      logical :: ok
      logical :: paths
      logical :: single
      logical :: update
      associate ( kt => ctx%spice%keeper, &
                  loaded => ctx%spice%keeper%loaded )
      if ( loaded == 0 ) then
         count = 0
         return
      end if

!
!     Parse KIND to see which kernels are of interest.
!
      dospk  = .false.
      dock   = .false.
      dodsk  = .false.
      dotext = .false.
      dometa = .false.
      doek   = .false.
      dopck  = .false.
      start  = 1

      call fndnwd ( kind, start, b, e )

      do while ( b > 0 )

         if ( eqstr( kind(b:e), 'ALL' ) )then
            count = loaded
            return
         else
            dock   = dock   .or. eqstr ( kind(b:e), 'CK' )
            dodsk  = dodsk  .or. eqstr ( kind(b:e), 'DSK' )
            doek   = doek   .or. eqstr ( kind(b:e), 'EK' )
            dometa = dometa .or. eqstr ( kind(b:e), 'META' )
            dopck  = dopck  .or. eqstr ( kind(b:e), 'PCK' )
            dospk  = dospk  .or. eqstr ( kind(b:e), 'SPK' )
            dotext = dotext .or. eqstr ( kind(b:e), 'TEXT' )
         end if

         start = e + 1
         call fndnwd ( kind, start, b, e )

      end do

      count = 0

      do i = 1, loaded

         add =      ( kt%kernels(i)%type == 'CK'   .and. dock   ) &
               .or. ( kt%kernels(i)%type == 'DSK'  .and. dodsk  ) &
               .or. ( kt%kernels(i)%type == 'EK'   .and. doek   ) &
               .or. ( kt%kernels(i)%type == 'META' .and. dometa ) &
               .or. ( kt%kernels(i)%type == 'PCK'  .and. dopck  ) &
               .or. ( kt%kernels(i)%type == 'SPK'  .and. dospk  ) &
               .or. ( kt%kernels(i)%type == 'TEXT' .and. dotext )

         if ( add ) then
            count = count + 1
         end if

      end do

      return
      end associate
      end subroutine ktotal

      !> Return data for the nth kernel that is among a list of specified
      !> kernel types.
      !>
      !>## Detailed Input
      !>
      !>     WHICH    is the number of the kernel to fetch (matching the type
      !>              specified by KIND) from the list of kernels that have
      !>              been loaded through the routine FURNSH but that have not
      !>              been unloaded through the routine UNLOAD.
      !>
      !>              The range of WHICH is 1 to COUNT, where COUNT is the
      !>              number of kernels loaded via FURNSH of type KIND. This
      !>              count may be obtained by calling KTOTAL. See the
      !>              $Examples section for an illustrative example.
      !>
      !>     KIND     is a list of types of kernels to be considered when
      !>              fetching kernels from the list of loaded kernels. KIND
      !>              should consist of words from list of kernel types
      !>              given below.
      !>
      !>                 SPK  --- All SPK files are counted in the total.
      !>                 CK   --- All CK files are counted in the total.
      !>                 DSK  --- All DSK files are counted in the total.
      !>                 PCK  --- All binary PCK files are counted in the
      !>                          total.
      !>                 EK   --- All EK files are counted in the total.
      !>                 TEXT --- All text kernels that are not meta-text
      !>                          kernels are included in the total.
      !>                 META --- All meta-text kernels are counted in the
      !>                          total.
      !>                 ALL  --- Every type of kernel is counted in the
      !>                          total.
      !>
      !>              KIND is case insensitive. If a word appears in KIND
      !>              that is not one of those listed above, it is ignored.
      !>
      !>              When KIND consists of multiple words, the words must
      !>              be separated by blanks. Examples of valid lists are the
      !>              strings
      !>
      !>                 'SPK CK TEXT'
      !>                 'SPK CK text'
      !>                 'PCK DSK'
      !>                 'CK'
      !>                 'ALL'
      !>
      !>              See the routine KTOTAL for examples of the use of KIND.
      !>
      !>## Detailed Output
      !>
      !>     FILE     is the name of the file having index WHICH in the
      !>              sequence of files of type KIND that is currently loaded
      !>              via FURNSH. FILE will be blank if there is not such
      !>              kernel loaded.
      !>
      !>     FILTYP   is the type of the kernel specified by FILE. FILE
      !>              will be blank if there is no file matching the
      !>              specification of WHICH and KIND.
      !>
      !>     SRCFIL   is the name of the source file that was used to
      !>              specify FILE as one to load. If FILE was loaded
      !>              directly via a call to FURNSH, SRCFIL will be blank.
      !>              If there is no file matching the specification of
      !>              WHICH and KIND, SRCFIL will be blank.
      !>
      !>     HANDLE   is the handle attached to FILE if it is a binary
      !>              kernel. If FILE is a text kernel or meta-text kernel
      !>              HANDLE will be zero. If there is no file matching
      !>              the specification of WHICH and KIND, HANDLE will be
      !>              set to zero.
      !>
      !>     FOUND    is returned .TRUE. if a FILE matching the specification
      !>              of WHICH and KIND exists. If there is no such file,
      !>              FOUND will be set to .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If a file is not loaded matching the specification of WHICH
      !>         and KIND, FOUND will be .FALSE., FILE, FILTYP, and SRCFIL will
      !>         be blank and HANDLE will be set to zero.
      !>
      !>     2)  If any of FILE, FILTYP or SRCFIL output strings has length
      !>         too short to contain the corresponding output string, the
      !>         string is truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to determine which kernels have been
      !>     loaded via FURNSH and to obtain information sufficient to directly
      !>     query those files.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Load a meta-kernel with a PCK, an LSK and an SPK and loop over
      !>        the loaded kernels, outputting file information for each of
      !>        them.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: kdata_ex1.tm
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
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00009.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00009.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM KDATA_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               FNAMLN
      !>      PARAMETER           ( FNAMLN = 256 )
      !>
      !>      INTEGER               FTYPLN
      !>      PARAMETER           ( FTYPLN = 33 )
      !>
      !>      INTEGER               SRCLEN
      !>      PARAMETER           ( SRCLEN = 256 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FNAMLN)    FILE
      !>      CHARACTER*(FTYPLN)    FILTYP
      !>      CHARACTER*(SRCLEN)    SRCFIL
      !>
      !>      INTEGER               COUNT
      !>      INTEGER               HANDLE
      !>      INTEGER               WHICH
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load several kernel files.
      !>C
      !>      CALL FURNSH ( 'kdata_ex1.tm' )
      !>
      !>C
      !>C     Count the number of loaded kernel files.
      !>C
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      DO WHICH= 1, COUNT + 1
      !>
      !>         CALL KDATA ( WHICH, 'ALL',   FILE, FILTYP,
      !>     .                SRCFIL, HANDLE, FOUND        )
      !>
      !>         IF ( FOUND ) THEN
      !>
      !>            WRITE(*,*) 'Index : ', WHICH
      !>            WRITE(*,*) 'File  : ', FILE
      !>            WRITE(*,*) 'Type  : ', FILTYP
      !>            WRITE(*,*) 'Source: ', SRCFIL
      !>            WRITE(*,*) 'Handle: ', HANDLE
      !>            WRITE(*,*) ' '
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'No kernel found with index: ', WHICH
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
      !>         Index :            1
      !>         File  : kdata_ex1.tm
      !>         Type  : META
      !>         Source:
      !>         Handle:            0
      !>
      !>         Index :            2
      !>         File  : de421.bsp
      !>         Type  : SPK
      !>         Source: kdata_ex1.tm
      !>         Handle:            1
      !>
      !>         Index :            3
      !>         File  : pck00009.tpc
      !>         Type  : TEXT
      !>         Source: kdata_ex1.tm
      !>         Handle:            0
      !>
      !>         Index :            4
      !>         File  : naif0009.tls
      !>         Type  : TEXT
      !>         Source: kdata_ex1.tm
      !>         Handle:            0
      !>
      !>         No kernel found with index:            5
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.1.0, 08-AUG-2021 (JDR) (NJB)
      !>
      !>        Changed argument name SOURCE to SRCFIL for consistency with
      !>        other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Created complete code example from existing code fragments.
      !>
      !>        Updated $Detailed_Input description of input arguments KIND, to
      !>        illustrate use of multi-word lists, and WHICH, to describe its
      !>        range.
      !>
      !>        Added entry #2 to $Exceptions section. Added KERNEL to the list
      !>        of required readings.
      !>
      !>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
      !>
      !>        Updated to support use of DSKs.
      !>
      !>        Updated the $Author_and_Institution section.
      !>
      !>-    SPICELIB Version 1.1.0, 02-APR-2009 (NJB)
      !>
      !>        Deleted reference to unneeded variable DOALL.
      !>
      !>-    SPICELIB Version 1.0.1, 06-DEC-2002 (NJB)
      !>
      !>        Typo in header example was corrected.
      !>
      !>-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
      module subroutine kdata ( ctx, which, kind, file, filtyp, srcfil, handle, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqstr, fndnwd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: which  !! Index of kernel to fetch from the list of kernels.
      character(*), intent(in) :: kind  !! The kind of kernel to which fetches are limited.
      character(*), intent(out) :: file  !! The name of the kernel file.
      character(*), intent(out) :: filtyp  !! The type of the kernel.
      integer(ip), intent(out) :: handle  !! The handle attached to FILE.
      character(*), intent(out) :: srcfil  !! Name of the source file used to load FILE.
      logical, intent(out) :: found  !! .TRUE. if the specified file could be located.
      logical :: return
      character(1) :: norc
      character(lnsize) :: symbol
      character(msgsiz) :: nofile
      character(typlen) :: thstyp
      integer(ip) :: b
      integer(ip) :: cursrc
      integer(ip) :: d
      integer(ip) :: dollar
      integer(ip) :: e
      integer(ip) :: fidx
      integer(ip) :: filnum
      integer(ip) :: fnmlen
      integer(ip) :: hits
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lidx
      integer(ip) :: myhand
      integer(ip) :: n
      integer(ip) :: n1
      integer(ip) :: n2
      integer(ip) :: n3
      integer(ip) :: nmult
      integer(ip) :: npaths
      integer(ip) :: npvals
      integer(ip) :: r
      integer(ip) :: size
      integer(ip) :: src
      integer(ip) :: start
      integer(ip) :: use
      logical :: add
      logical :: didck
      logical :: diddsk
      logical :: didek
      logical :: didpck
      logical :: didspk
      logical :: didtxt
      logical :: dock
      logical :: dodsk
      logical :: doek
      logical :: dometa
      logical :: dopck
      logical :: dospk
      logical :: dotext
      logical :: fnd
      logical :: gotit
      logical :: ok
      logical :: paths
      logical :: single
      logical :: update
      associate ( kt => ctx%spice%keeper, &
                  loaded => ctx%spice%keeper%loaded )
      file   = ' '
      filtyp = ' '
      srcfil = ' '
      handle =  0
      found  = .false.

      if ( which < 1 .or. which > loaded ) return
!
!     Parse KIND to see which kernels are of interest.
!
      dospk  = .false.
      dock   = .false.
      dodsk  = .false.
      dotext = .false.
      dometa = .false.
      doek   = .false.
      dopck  = .false.
      start  = 1
      call fndnwd ( kind, start, b, e )

      do while ( b > 0 )

         if ( eqstr( kind(b:e), 'ALL' ) )then
!
!           There's no point in going on, we can fill in the output
!           variables right now.
!
            found  = .true.
            file   = kt%kernels(which)%name
            filtyp = kt%kernels(which)%type
            handle = kt%kernels(which)%handle

            if ( kt%kernels(which)%source /= 0 ) then
               srcfil = kt%kernels( kt%kernels(which)%source )%name
            end if
            return

         else
            dock   = dock   .or. eqstr ( kind(b:e), 'CK' )
            dodsk  = dodsk  .or. eqstr ( kind(b:e), 'DSK' )
            doek   = doek   .or. eqstr ( kind(b:e), 'EK' )
            dometa = dometa .or. eqstr ( kind(b:e), 'META' )
            dopck  = dopck  .or. eqstr ( kind(b:e), 'PCK' )
            dospk  = dospk  .or. eqstr ( kind(b:e), 'SPK' )
            dotext = dotext .or. eqstr ( kind(b:e), 'TEXT' )
         end if

         start = e + 1
         call fndnwd ( kind, start, b, e )

      end do

!
!     Examine the loaded kernels one at a time until we match
!     WHICH files of the specified KIND.
!
      hits = 0

      do i = 1, loaded

         add =      ( kt%kernels(i)%type == 'CK'   .and. dock   ) &
               .or. ( kt%kernels(i)%type == 'DSK'  .and. dodsk  ) &
               .or. ( kt%kernels(i)%type == 'EK'   .and. doek   ) &
               .or. ( kt%kernels(i)%type == 'META' .and. dometa ) &
               .or. ( kt%kernels(i)%type == 'PCK'  .and. dopck  ) &
               .or. ( kt%kernels(i)%type == 'SPK'  .and. dospk  ) &
               .or. ( kt%kernels(i)%type == 'TEXT' .and. dotext )

         if ( add ) then

            hits = hits + 1
!
!           If we've reached the specified number, fill in the
!           requested information and return.
!
            if ( hits == which ) then

               found  = .true.
               file   = kt%kernels(i)%name
               filtyp = kt%kernels(i)%type
               handle = kt%kernels(i)%handle

               if ( kt%kernels(i)%source /= 0 ) then
                  srcfil = kt%kernels( kt%kernels(i)%source )%name
               end if
               return
            end if
         end if

      end do

      return
      end associate
      end subroutine kdata

      !> Return information about a specific kernel
      !>
      !>## Detailed Input
      !>
      !>     FILE     is the name of a kernel file for which KEEPER
      !>              information is desired.
      !>
      !>## Detailed Output
      !>
      !>     FILTYP   is the type of the kernel specified by FILE.  FILE
      !>              will be blank if FILE is not on the list of loaded
      !>              kernels.
      !>
      !>     SRCFIL   is the name of the source file that was used to
      !>              specify FILE as one to load. If FILE was loaded
      !>              directly via a call to FURNSH, SRCFIL will be blank.
      !>              If FILE is not on the list of loaded kernels, SRCFIL
      !>              will be blank
      !>
      !>     HANDLE   is the handle attached to FILE if it is a binary
      !>              kernel. If FILE is a text kernel or meta-text kernel
      !>              HANDLE will be zero. If FILE is not on the list of
      !>              loaded kernels, HANDLE will be set to zero.
      !>
      !>     FOUND    is returned .TRUE. if FILE is on the KEEPER list of
      !>              loaded kernels. If there is no such file, FOUND will
      !>              be set to .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the specified file is not on the list of files that are
      !>         currently loaded via the interface FURNSH, FOUND will be
      !>         .FALSE., HANDLE will be set to zero and FILTYP and SRCFIL will
      !>         be set to blanks.
      !>
      !>     2)  If any of  FILTYP or SRCFIL output strings has length too
      !>         short to contain the corresponding output string, the string
      !>         is truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This entry point allows you to request information directly
      !>     for a specific SPICE kernel.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you wish to determine the types of kernels loaded
      !>        by a given meta-kernel. The following code example shows
      !>        how you might use this routine to do this.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: kinfo_ex1.tm
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
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00008.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00008.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM KINFO_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      LOGICAL               EQSTR
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               FILLEN
      !>      PARAMETER           ( FILLEN = 32 )
      !>
      !>      INTEGER               TYPLEN
      !>      PARAMETER           ( TYPLEN = 33 )
      !>
      !>      INTEGER               SRCLEN
      !>      PARAMETER           ( SRCLEN = 256 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FILLEN)    FILE
      !>      CHARACTER*(TYPLEN)    FILTYP
      !>      CHARACTER*(SRCLEN)    SRCFIL
      !>
      !>      INTEGER               COUNT
      !>      INTEGER               HANDLE
      !>      INTEGER               WHICH
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load the meta-kernel.
      !>C
      !>      CALL FURNSH ( 'kinfo_ex1.tm' )
      !>
      !>C
      !>C     Find out the total number of kernels in the kernel pool.
      !>C
      !>      CALL KTOTAL ( 'all', COUNT )
      !>
      !>      IF ( COUNT .EQ. 0 ) THEN
      !>
      !>         WRITE(*,*) 'No files loaded at this time.'
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) 'The loaded files files are: '
      !>         WRITE(*,*) ' '
      !>
      !>      END IF
      !>
      !>C
      !>C     Find the file name, type and source for each of the
      !>C     kernels in the kernel pool and print its type.
      !>C
      !>      DO WHICH= 1, COUNT
      !>
      !>         CALL KDATA ( WHICH,  'all',  FILE, FILTYP,
      !>     .                SRCFIL, HANDLE, FOUND        )
      !>
      !>         CALL KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )
      !>
      !>         IF ( EQSTR( FILTYP, "SPK" ) ) THEN
      !>
      !>            WRITE(*,*) FILE, ' is an SPK file.'
      !>
      !>         ELSE IF ( EQSTR( FILTYP, "CK" ) ) THEN
      !>
      !>            WRITE(*,*) FILE, ' is a CK file.'
      !>
      !>         ELSE IF ( EQSTR( FILTYP, "PCK" ) ) THEN
      !>
      !>            WRITE(*,*) FILE, ' is a PCK file.'
      !>
      !>         ELSE IF ( EQSTR( FILTYP, "DSK" ) ) THEN
      !>
      !>            WRITE(*,*) FILE, ' is a DSK file.'
      !>
      !>         ELSE IF ( EQSTR( FILTYP, "EK" ) ) THEN
      !>
      !>            WRITE(*,*) FILE, ' is an EK file.'
      !>
      !>         ELSE IF ( EQSTR( FILTYP, "META" ) ) THEN
      !>
      !>            WRITE(*,*) FILE, ' is a meta-kernel file.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) FILE, ' is a text kernel.'
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
      !>         The loaded files files are:
      !>
      !>         kinfo_ex1.tm                     is a meta-kernel file.
      !>         de421.bsp                        is an SPK file.
      !>         pck00008.tpc                     is a text kernel.
      !>         naif0009.tls                     is a text kernel.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.1.0, 08-AUG-2021 (JDR)
      !>
      !>        Changed argument name SOURCE to SRCFIL for consistency with
      !>        other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Created complete code example from existing code fragments.
      !>
      !>        Added entry #2 to $Exceptions section.
      !>
      !>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
      !>
      !>        Updated to support use of DSKs.
      !>
      !>        Updated the $Author_and_Institution section.
      !>
      !>-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
      module subroutine kinfo ( ctx, file, filtyp, srcfil, handle, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! Name of a kernel to fetch information for
      character(*), intent(out) :: filtyp  !! The type of the kernel
      integer(ip), intent(out) :: handle  !! The handle attached to FILE.
      character(*), intent(out) :: srcfil  !! Name of the source file used to load FILE.
      logical, intent(out) :: found  !! .TRUE. if the specified file could be located.
      logical :: return
      character(1) :: norc
      character(lnsize) :: symbol
      character(msgsiz) :: nofile
      character(typlen) :: thstyp
      integer(ip) :: b
      integer(ip) :: cursrc
      integer(ip) :: d
      integer(ip) :: dollar
      integer(ip) :: e
      integer(ip) :: fidx
      integer(ip) :: filnum
      integer(ip) :: fnmlen
      integer(ip) :: hits
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lidx
      integer(ip) :: myhand
      integer(ip) :: n
      integer(ip) :: n1
      integer(ip) :: n2
      integer(ip) :: n3
      integer(ip) :: nmult
      integer(ip) :: npaths
      integer(ip) :: npvals
      integer(ip) :: r
      integer(ip) :: size
      integer(ip) :: src
      integer(ip) :: start
      integer(ip) :: use
      logical :: add
      logical :: didck
      logical :: diddsk
      logical :: didek
      logical :: didpck
      logical :: didspk
      logical :: didtxt
      logical :: dock
      logical :: dodsk
      logical :: doek
      logical :: dometa
      logical :: dopck
      logical :: dospk
      logical :: dotext
      logical :: fnd
      logical :: gotit
      logical :: ok
      logical :: paths
      logical :: single
      logical :: update
      associate ( kt => ctx%spice%keeper, &
                  loaded => ctx%spice%keeper%loaded )
      filtyp = ' '
      srcfil = ' '
      handle =  0
      found  = .false.

      i = 0
      do j = 1, loaded
         if ( kt%kernels(j)%name == file ) then
            i = j
            exit
         end if
      end do

      if ( i > 0 ) then

         found  = .true.
         filtyp = kt%kernels(i)%type
         handle = kt%kernels(i)%handle

         if ( kt%kernels(i)%source /= 0 ) then
            srcfil = kt%kernels( kt%kernels(i)%source )%name
         end if

      end if

      return
      end associate
      end subroutine kinfo

      !> The length of the longest file name among the kernels loaded
      !> through FURNSH (at least 1).
      pure integer(ip) function longest_kernel_name ( ctx ) result ( n )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(in) :: ctx  !! the context
      integer(ip) :: j
      n = 1
      associate ( kt => ctx%spice%keeper )
      do j = 1, kt%loaded
         n = max ( n, len ( kt%kernels(j)%name, ip ) )
      end do
      end associate
      end function longest_kernel_name

      !> The data of the nth kernel of a kind loaded through FURNSH, with
      !> file names of any length (PEPPER; [[kdata]], which truncates to its
      !> arguments' lengths).
      module subroutine kernel_data ( ctx, which, kind, file, filtyp, source, handle, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: which  !! the index of the kernel among those of KIND
      character(*), intent(in) :: kind  !! the kinds of kernel ('ALL', 'SPK', 'CK TEXT', ..., as [[kdata]])
      character(:), allocatable, intent(out) :: file  !! the file name (empty if not found)
      character(:), allocatable, intent(out) :: filtyp  !! the kernel type: SPK, CK, PCK, DSK, EK, TEXT or META
      character(:), allocatable, intent(out) :: source  !! the meta-kernel it was loaded from (empty if loaded directly)
      integer(ip), intent(out) :: handle  !! its handle (0 for a text or meta-kernel)
      logical, intent(out) :: found  !! a kernel was found
      character(len=longest_kernel_name(ctx)) :: f
      character(len=longest_kernel_name(ctx)) :: s
      character(8) :: t
      call kdata ( ctx, which, kind, f, t, s, handle, found )
      file   = trim ( f )
      filtyp = trim ( t )
      source = trim ( s )
      end subroutine kernel_data

      !> The data of a kernel loaded through FURNSH, with file names of any
      !> length (PEPPER; [[kinfo]], which truncates to its arguments'
      !> lengths).
      module subroutine kernel_info ( ctx, file, filtyp, source, handle, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the file name, as loaded
      character(:), allocatable, intent(out) :: filtyp  !! the kernel type: SPK, CK, PCK, DSK, EK, TEXT or META
      character(:), allocatable, intent(out) :: source  !! the meta-kernel it was loaded from (empty if loaded directly)
      integer(ip), intent(out) :: handle  !! its handle (0 for a text or meta-kernel)
      logical, intent(out) :: found  !! the file is loaded
      character(len=longest_kernel_name(ctx)) :: s
      character(8) :: t
      call kinfo ( ctx, file, t, s, handle, found )
      filtyp = trim ( t )
      source = trim ( s )
      end subroutine kernel_info

      !> Clear the KEEPER subsystem: unload all kernels, clear the kernel
      !> pool, and re-initialize the subsystem. Existing watches on kernel
      !> variables are retained.
      !>
      !>## Detailed Input
      !>
      !>     None. This routine operates by side effects. See $Particulars
      !>     below.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs when setting a kernel pool watch or
      !>         checking watched variables, the error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See $Particulars.
      !>
      !>## Particulars
      !>
      !>     This entry point allows you re-initialize the KEEPER system with
      !>     a single call.
      !>
      !>     This routine unloads all kernels from their kernel-type-specific
      !>     kernel management subsystems (SPKBSR, CKBSR, etc.), clears the
      !>     kernel pool, clears KEEPER's internal file database, and re-sets
      !>     the watch status for the kernel variables used to load kernels
      !>     via meta-kernels. As a side effect of clearing the kernel pool,
      !>     all watched variables are marked as updated. Note that clearing
      !>     the kernel pool does not delete watches (aka "watchers"). Watches
      !>     can be deleted by calling the POOL entry point DWPOOL.
      !>
      !>     This capability, though implemented in Fortran, is particularly
      !>     relevant to SPICE implementations such as Icy, for which the
      !>     state of the KEEPER system persists after any Icy-based IDL
      !>     script is run. Successive runs of Icy-based scripts may perform
      !>     in unexpected ways when scripts access data loaded during runs of
      !>     previous scripts.
      !>
      !>     Cleaning up after such programs using explicit UNLOAD commands is
      !>     tedious and error-prone. One call to this routine sets the
      !>     KEEPER system to its initial state, preventing unintentional
      !>     interaction between scripts via KEEPER's state.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Load a meta-kernel containing three kernels, and count the
      !>        number of files in the kernel pool before and after calling
      !>        KCLEAR.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: kclear_ex1.tm
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
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00009.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls',
      !>                                  'pck00009.tpc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM KCLEAR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               COUNT
      !>
      !>C
      !>C     Load several kernel files.
      !>C
      !>      CALL FURNSH ( 'kclear_ex1.tm' )
      !>
      !>C
      !>C     Count the number of loaded kernel files.
      !>C
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'FURNSH: ', COUNT
      !>
      !>C
      !>C     Clear the KEEPER system, retrieve the number of loaded
      !>C     after the clear.
      !>C
      !>      CALL KCLEAR()
      !>
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'KCLEAR: ', COUNT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        The total number of kernels after FURNSH:  4
      !>        The total number of kernels after KCLEAR:  0
      !>
      !>## Restrictions
      !>
      !>     1)  Calling this routine will wipe out any kernel pool data
      !>         inserted via the SPICELIB API routines to put data into the
      !>         kernel pool (PCPOOL, PDPOOL and PIPOOL).
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.1, 08-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example from existing code fragments.
      !>
      !>        Improved $Restrictions section.
      !>
      !>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
      !>
      !>        Updated to support use of DSKs.
      !>
      !>        Updated the $Author_and_Institution section.
      !>
      !>-    SPICELIB Version 1.0.1, 01-JUL-2014 (NJB) (EDW)
      !>
      !>        Updated the discussion of kernel variable watchers.
      !>
      !>     Last update was 13-APR-2011 (EDW)
      !>
      !>        Trivial edit to $Restrictions, replaced P*POOL with
      !>        PXPOOL. The "*" character causes the HTML documentation
      !>        script to create a link for the "POOL" substring.
      !>
      !>-    SPICELIB Version 1.0.0, 15-NOV-2006 (NJB)
      module subroutine kclear ( ctx )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(1) :: norc
      character(lnsize) :: symbol
      character(msgsiz) :: nofile
      character(typlen) :: thstyp
      integer(ip) :: b
      integer(ip) :: cursrc
      integer(ip) :: d
      integer(ip) :: dollar
      integer(ip) :: e
      integer(ip) :: fidx
      integer(ip) :: filnum
      integer(ip) :: fnmlen
      integer(ip) :: hits
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lidx
      integer(ip) :: myhand
      integer(ip) :: n
      integer(ip) :: n1
      integer(ip) :: n2
      integer(ip) :: n3
      integer(ip) :: nmult
      integer(ip) :: npaths
      integer(ip) :: npvals
      integer(ip) :: r
      integer(ip) :: size
      integer(ip) :: src
      integer(ip) :: start
      integer(ip) :: use
      logical :: add
      logical :: didck
      logical :: diddsk
      logical :: didek
      logical :: didpck
      logical :: didspk
      logical :: didtxt
      logical :: dock
      logical :: dodsk
      logical :: doek
      logical :: dometa
      logical :: dopck
      logical :: dospk
      logical :: dotext
      logical :: fnd
      logical :: gotit
      logical :: ok
      logical :: paths
      logical :: single
      logical :: update
      associate ( kt => ctx%spice%keeper, &
                  loaded => ctx%spice%keeper%loaded )
      if ( ctx%failed() ) return

!
!     Unloading all kernels is actually much less work than
!     unloading just a few of them.  We unload all of the
!     binary kernels via the "unload" routines for their
!     respective subsystems, then clear the kernel pool.
!
      do i = 1, loaded

         if (      kt%kernels(i)%type == 'SPK' ) then

            call spkuef ( ctx, kt%kernels(i)%handle )

         else if ( kt%kernels(i)%type == 'CK' ) then

            call ckupf ( ctx, kt%kernels(i)%handle )

         else if ( kt%kernels(i)%type == 'PCK' ) then

            call pckuof ( ctx, kt%kernels(i)%handle )

         else if ( kt%kernels(i)%type == 'EK' ) then

            call ekuef ( ctx, kt%kernels(i)%handle )

         else if ( kt%kernels(i)%type == 'DSK' ) then

            call zzdskusf ( ctx, kt%kernels(i)%handle )

         end if
         call notify_kernel_unloaded ( ctx, i, clearing=.true. )  ! PEPPER: a kernel loader's callback

      end do

!
!     PEPPER: the programmatic ephemerides and frames go too (the frames
!     before CLPOOL, which writes the definitions of those it knows).
!
      call clear_provided_ephemerides ( ctx )
      call clear_provided_frames ( ctx )
      call clpool ( ctx )

!
!     Although it's not strictly necessary, we initialize
!     KEEPER's database arrays.  This step may occasionally
!     be helpful for debugging.
!
      call kt%clear ( )

!
!     There's just one counter that indicates the number of
!     database entries:  LOADED.  Set this counter to
!     its initial state.
!
      loaded = 0

!
!     Calling CLPOOL doesn't remove watches, but it does send a message
!     to each agent indicating that its variables have been touched.
!     Clear this indication by calling CVPOOL.  (This is done for
!     safety; the current implementation of FURNSH doesn't require it.)
!
      call cvpool ( ctx, 'FURNSH', update )

      return
      end associate
      end subroutine kclear

      !> Unload a SPICE kernel.
      !>
      !>## Detailed Input
      !>
      !>     FILE     is the name of a file to unload. This file
      !>              should be one loaded through the interface FURNSH.
      !>              If the file is not on the list of loaded kernels
      !>              no action is taken.
      !>
      !>              Note that if FILE is a meta-text kernel, all of
      !>              the files loaded as a result of loading the meta-text
      !>              kernel will be unloaded.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the specified kernel is not on the list of loaded kernels
      !>         no action is taken.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The call
      !>
      !>        CALL UNLOAD ( FILE )
      !>
      !>     has the effect of "erasing" the last previous call:
      !>
      !>        CALL FURNSH ( FILE )
      !>
      !>     This interface allows you to unload binary and text kernels.
      !>     Moreover, if you used a meta-text kernel to set up your
      !>     working environment, you can unload all of the kernels loaded
      !>     through the meta-kernel by unloading the meta-kernel.
      !>
      !>     The usual usage of FURNSH is to load each file needed by your
      !>     program exactly one time. However, it is possible to load a
      !>     kernel more than one time. (Usually, this is a result of loading
      !>     meta-kernels without taking the care needed to ensure that the
      !>     meta-kernels do not specify the same file more than once.) The
      !>     effect of unloading a kernel that has been loaded more than once
      !>     is to "undo" the last loading of the kernel. Depending upon the
      !>     kernel and its relationship to other loaded kernels, this may
      !>     have no visible effect on the working of your program. To
      !>     illustrate this behavior suppose that you have a collection of
      !>     files FILE1, FILE2, FILE3, FILE4, FILE5, FILE6, FILE7, FILE8,
      !>     META1, META2  where FILE1 ... FILE8 are SPICE kernels and META1
      !>     and META2 are meta-kernels with the specified kernels to load as
      !>     shown below.
      !>
      !>         META1:
      !>            KERNELS_TO_LOAD = ( FILE2,
      !>                                FILE3,
      !>                                FILE4,
      !>                                FILE5 )
      !>
      !>         META2:
      !>            KERNELS_TO_LOAD = ( FILE2,
      !>                                FILE3,
      !>                                FILE7,
      !>                                FILE8 )
      !>
      !>      The following sequence of calls
      !>
      !>          CALL FURNSH ( FILE1 )
      !>          CALL FURNSH ( FILE2 )
      !>          CALL FURNSH ( FILE3 )
      !>          CALL FURNSH ( META1 )
      !>          CALL FURNSH ( FILE6 )
      !>          CALL FURNSH ( META2 )
      !>
      !>      has the effect:
      !>
      !>          "Load" FILE1
      !>          "Load" FILE2
      !>          "Load" FILE3
      !>          "Load" META1 as a text kernel and then...
      !>                "Load" FILE2 (note that it was loaded from META1)
      !>                "Load" FILE3 (note that it was loaded from META1)
      !>                "Load" FILE4 (note that it was loaded from META1)
      !>                "Load" FILE5 (note that it was loaded from META1)
      !>          "Load" FILE6
      !>          "Load" META2 as a text kernel and then...
      !>                "Load" FILE2 (note that it was loaded from META2)
      !>                "Load" FILE3 (note that it was loaded from META2) *
      !>                "Load" FILE7 (note that it was loaded from META2)
      !>                "Load" FILE8 (note that it was loaded from META2)
      !>
      !>      If we  UNLOAD FILE3
      !>
      !>         CALL UNLOAD ( FILE3 )
      !>
      !>      we locate the last time FILE3 was loaded (* above) and modify the
      !>      state of loaded kernels so that it looks as if we had made the
      !>      following sequence of "load" operations.
      !>
      !>          "Load" FILE1
      !>          "Load" FILE2
      !>          "Load" FILE3
      !>          "Load" META1 as a text kernel and then...
      !>                "Load" FILE2 (note that it was loaded from META1)
      !>                "Load" FILE3 (note that it was loaded from META1)
      !>                "Load" FILE4 (note that it was loaded from META1)
      !>                "Load" FILE5 (note that it was loaded from META1)
      !>          "Load" FILE6
      !>          "Load" META2 as a text kernel and then...
      !>                "Load" FILE2 (note that it was loaded from META2)
      !>                "Load" FILE7 (note that it was loaded from META2)
      !>                "Load" FILE8 (note that it was loaded from META2)
      !>
      !>      As you can see, the data from FILE3 is still available to the
      !>      program. All that may have changed is the usage priority
      !>      associated with that data.
      !>
      !>      If we unload META2 (or META1) we remove all remaining files that
      !>      are noted as being loaded from META2 (or META1)
      !>
      !>          CALL UNLOAD ( META2 )
      !>
      !>      produces the following load state for the program:
      !>
      !>          "Load" FILE1
      !>          "Load" FILE2
      !>          "Load" FILE3
      !>          "Load" META1 as a text kernel and then...
      !>                "Load" FILE2 (note that it was loaded from META1)
      !>                "Load" FILE3 (note that it was loaded from META1)
      !>                "Load" FILE4 (note that it was loaded from META1)
      !>                "Load" FILE5 (note that it was loaded from META1)
      !>          "Load" FILE6
      !>
      !>      If we had unloaded META1 instead, we would have this load state.
      !>
      !>          "Load" FILE1
      !>          "Load" FILE2
      !>          "Load" FILE3
      !>          "Load" FILE6
      !>          "Load" META2 as a text kernel and then...
      !>                "Load" FILE2 (note that it was loaded from META2)
      !>                "Load" FILE7 (note that it was loaded from META2)
      !>                "Load" FILE8 (note that it was loaded from META2)
      !>
      !>      So we see that unloading a file does not necessarily make its
      !>      data unavailable to your program. Unloading modifies the
      !>      precedence of the files loaded in your program. The data
      !>      associated with an unloaded file becomes unavailable only when
      !>      the file has been unloaded as many times as it was loaded.
      !>
      !>      When would you encounter such a scenario? The situation of
      !>      loading a file more than once might appear if you were trying to
      !>      contrast the results of computations performed with two
      !>      different meta-kernels. In such a scenario you might load a
      !>      "baseline" set of kernels early in your program and then load
      !>      and unload meta-kernels to compare results between the two
      !>      different sets of data.
      !>
      !>     Unloading Text Kernels or Meta-Kernels
      !>     --------------------------------------
      !>
      !>     Part of the action of unloading text (or meta-kernels) is
      !>     the clearing of the kernel pool and re-loading any kernels that
      !>     were not in the specified set of kernels to unload. Since
      !>     loading of text kernels is not a very fast process, unloading
      !>     text kernels takes considerably longer than unloading binary
      !>     kernels. Moreover, since the kernel pool is cleared, any kernel
      !>     pool variables you have set from your program by using one of the
      !>     interfaces PCPOOL, PDPOOL, PIPOOL, or LMPOOL will be removed from
      !>     the kernel pool. For this reason, if you plan to use this
      !>     feature in your program, together with one of the routines
      !>     specified above, you will need to take special precautions to
      !>     make sure kernel pool variables required by your program do not
      !>     inadvertently disappear.
      !>
      !>     As a side effect of unloading a text kernel, all watched kernel
      !>     variables are marked as updated. Note that unloading a text
      !>     kernel does not delete watchers. Watchers can be deleted by
      !>     calling the POOL entry point DWPOOL.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Load a meta-kernel with a PCK, an LSK and an SPK, and
      !>        separately, a text kernel and a binary PCK. Loop over the
      !>        loaded kernels, outputting file information for each of
      !>        them.
      !>
      !>        Then unload the text kernels, check that they have been
      !>        unloaded, and finally unload the meta-kernel.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: unload_ex1.tm
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
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00009.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls',
      !>                                  'pck00009.tpc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Use the PCK kernel below as the binary PCK required for the
      !>        example.
      !>
      !>           earth_latest_high_prec.bpc
      !>
      !>        Use the FK kernel below as the text kernel required for the
      !>        example.
      !>
      !>           RSSD0002.TF
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM UNLOAD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               FNAMLN
      !>      PARAMETER           ( FNAMLN = 256 )
      !>
      !>      INTEGER               FTYPLN
      !>      PARAMETER           ( FTYPLN = 33 )
      !>
      !>      INTEGER               SRCLEN
      !>      PARAMETER           ( SRCLEN = 256 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FNAMLN)    FILE
      !>      CHARACTER*(FTYPLN)    FILTYP
      !>      CHARACTER*(SRCLEN)    SRCFIL
      !>
      !>      INTEGER               COUNT
      !>      INTEGER               HANDLE
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load several kernel files.
      !>C
      !>      CALL FURNSH ( 'unload_ex1.tm'              )
      !>      CALL FURNSH ( 'RSSD0002.TF'                )
      !>      CALL FURNSH ( 'earth_latest_high_prec.bpc' )
      !>
      !>C
      !>C     Count the number of loaded kernel files.
      !>C
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'final FURNSH: ', COUNT
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Unload the text kernels.
      !>C
      !>      CALL KTOTAL ( 'TEXT', COUNT )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,'(A,I2,A)') 'Unloading', COUNT,
      !>     .                    ' text kernels...'
      !>      WRITE(*,*) ' '
      !>
      !>      DO WHILE ( COUNT .NE. 0 )
      !>
      !>         CALL KDATA (      1, 'TEXT',  FILE, FILTYP,
      !>     .                SRCFIL, HANDLE, FOUND        )
      !>
      !>C
      !>C        If the kernel is found in the pool, unload it.
      !>C
      !>         IF ( FOUND ) THEN
      !>
      !>            CALL UNLOAD ( FILE )
      !>
      !>C
      !>C           Check if the file has been unloaded.
      !>C
      !>            CALL KINFO ( FILE, FILTYP, SRCFIL, HANDLE, FOUND )
      !>
      !>            IF ( FOUND ) THEN
      !>
      !>               WRITE(*,'(A)') '  Error unloading ' // FILE
      !>
      !>            ELSE
      !>
      !>               WRITE(*,'(A)') '  Success unloading ' // FILE
      !>
      !>            END IF
      !>
      !>C
      !>C        Something is not working. Inform NAIF.
      !>C
      !>         ELSE
      !>
      !>            WRITE(*,*) ' ERROR: No kernel found but KTOTAL '
      !>     .      //         'returns ', COUNT
      !>
      !>         END IF
      !>
      !>C
      !>C        Check if we have more text kernels to unload from
      !>C        the kernel pool. Note that unloading a text kernel
      !>C        or meta-kernel implies that the kernel pool is
      !>C        cleared, and any kernel(s) that were not to be
      !>C        unloaded are re-loaded. Therefore the COUNT value
      !>C        changes, and the indexing of the files within the
      !>C        kernel pool too.
      !>C
      !>         CALL KTOTAL ( 'TEXT', COUNT )
      !>
      !>      END DO
      !>
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'UNLOAD calls: ', COUNT
      !>
      !>C
      !>C     Unload the meta-kernel and retrieve the number of loaded
      !>C     after the clear.
      !>C
      !>      CALL UNLOAD ( 'unload_ex1.tm' )
      !>
      !>      CALL KTOTAL ( 'ALL', COUNT )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,'(A,I2)') 'The total number of kernels after '
      !>     . //               'final UNLOAD: ', COUNT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        The total number of kernels after final FURNSH:  6
      !>
      !>        Unloading 3 text kernels...
      !>
      !>          Success unloading naif0012.tls
      !>          Success unloading pck00009.tpc
      !>          Success unloading RSSD0002.TF
      !>
      !>        The total number of kernels after UNLOAD calls:  3
      !>
      !>        The total number of kernels after final UNLOAD:  1
      !>
      !>## Restrictions
      !>
      !>     1)  See the note regarding the unloading of Text and meta-text
      !>         Kernels.
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
      !>-    SPICELIB Version 5.0.1, 08-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Created complete code example from existing code fragments.
      !>
      !>-    SPICELIB Version 5.0.0, 01-FEB-2017 (NJB) (BVS)
      !>
      !>        Updated to support use of DSKs.
      !>
      !>        Bug fix: now unloads binary kernels via low-level
      !>        unload routines only when those kernels have just
      !>        one entry in the KEEPER database.
      !>
      !>        Updated the $Author_and_Institution section.
      !>
      !>-    SPICELIB Version 3.0.1, 01-JUL-2014 (NJB)
      !>
      !>        Updated discussion of kernel variable watchers.
      !>
      !>-    SPICELIB Version 3.0.0, 15-NOV-2006 (NJB)
      !>
      !>        Bug fix: corrected update of source pointers when a
      !>        meta-kernel is unloaded. Previously source pointers
      !>        having higher indices than those of the files referenced
      !>        by the meta kernel were not adjusted when the database
      !>        was compressed.
      !>
      !>-    SPICELIB Version 2.0.0, 23-AUG-2001 (WLT)
      !>
      !>        Added code to make sure that UNLOAD has the effect of
      !>        loading all remaining kernels in the order they were first
      !>        introduced.
      !>
      !>-    SPICELIB Version 1.0.0, 01-JUL-1999 (WLT)
      module subroutine unload ( ctx, file )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! The name of a kernel to unload.
      character(1) :: norc
      character(lnsize) :: symbol
      character(msgsiz) :: nofile
      character(typlen) :: thstyp
      integer(ip) :: b
      integer(ip) :: cursrc
      integer(ip) :: d
      integer(ip) :: dollar
      integer(ip) :: e
      integer(ip) :: fidx
      integer(ip) :: filnum
      integer(ip) :: fnmlen
      integer(ip) :: hits
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lidx
      integer(ip) :: myhand
      integer(ip) :: n
      integer(ip) :: n1
      integer(ip) :: n2
      integer(ip) :: n3
      integer(ip) :: nmult
      integer(ip) :: npaths
      integer(ip) :: npvals
      integer(ip) :: r
      integer(ip) :: size
      integer(ip) :: src
      integer(ip) :: start
      integer(ip) :: use
      logical :: add
      logical :: didck
      logical :: diddsk
      logical :: didek
      logical :: didpck
      logical :: didspk
      logical :: didtxt
      logical :: dock
      logical :: dodsk
      logical :: doek
      logical :: dometa
      logical :: dopck
      logical :: dospk
      logical :: dotext
      logical :: fnd
      logical :: gotit
      logical :: ok
      logical :: paths
      logical :: single
      logical :: update
      associate ( kt => ctx%spice%keeper, &
                  loaded => ctx%spice%keeper%loaded, &
                  known => ctx%spice%keeper%known )
      if ( ctx%failed() ) return

      didspk = .false.
      didpck = .false.
      didck  = .false.
      didek  = .false.
      diddsk = .false.
      didtxt = .false.

!
!     First locate the file we need to unload, we search backward
!     through the list of loaded files so that we unload in the right
!     order.
!
      gotit = .false.
      i     = loaded

      do while ( .not. gotit .and. i > 0 )

         if ( kt%kernels(i)%name == file ) then
            gotit = .true.
         else
            i     = i - 1
         end if

      end do

!
!     If we didn't locate the requested file, there is nothing to do.
!
      if ( .not. gotit ) return

!
!     We need to know what type of file we've got so that we
!     can take the correct "unload" action.
!
!     If the kernel to be unloaded is binary, found out how
!     many instances of it are present in the database.
!
!     We take advantage of the fact that all binary kernels
!     use the handle manager subsystem: handles are unique
!     across all file types. We don't need to rely on file
!     names.
!
      if (    ( kt%kernels(i)%type == 'SPK' ) &
          .or.( kt%kernels(i)%type == 'CK'  ) &
          .or.( kt%kernels(i)%type == 'DSK' ) &
          .or.( kt%kernels(i)%type == 'EK'  ) &
          .or.( kt%kernels(i)%type == 'PCK' ) ) then
!
!        Count the occurrences of the file in the database.
!        Stop if we reach two occurrences.
!
         nmult = 0
         j     = 1

         do while ( ( j <= loaded ) .and. ( nmult < 2 ) )

            if ( kt%kernels(j)%handle == kt%kernels(i)%handle ) then
!
!              To be safe, make sure we're not looking at
!              a text kernel with a random, matching handle
!              value.
!
               if (      ( kt%kernels(j)%type /= 'TEXT' ) &
                   .and. ( kt%kernels(j)%type /= 'META' ) ) then

                  nmult = nmult + 1

               end if

            end if

            j = j + 1

         end do

         single = nmult == 1

      end if

      if (      kt%kernels(i)%type == 'SPK' ) then

         if ( single ) then
            call spkuef ( ctx, kt%kernels(i)%handle )
         else
            didspk = .true.
         end if

      else if ( kt%kernels(i)%type == 'CK' ) then

         if ( single ) then
            call ckupf ( ctx, kt%kernels(i)%handle )
         else
            didck = .true.
         end if

      else if ( kt%kernels(i)%type == 'DSK' ) then

         if ( single ) then
            call zzdskusf ( ctx, kt%kernels(i)%handle )
         else
            diddsk = .true.
         end if

      else if ( kt%kernels(i)%type == 'PCK' ) then

         if ( single ) then
            call pckuof ( ctx, kt%kernels(i)%handle )
         else
            didpck = .true.
         end if

      else if ( kt%kernels(i)%type == 'EK' ) then

         if ( single ) then
            call ekuef ( ctx, kt%kernels(i)%handle )
         else
            didek = .true.
         end if

      else if ( kt%kernels(i)%type == 'TEXT' ) then

         call clpool ( ctx )
         didtxt = .true.

      else if ( kt%kernels(i)%type == 'META' ) then
!
!        This is a special case, we need to undo the effect of loading
!        the meta-kernel.  This means we need to unload all kernels
!        that were loaded using this meta-kernel.
!
         didtxt = .true.
         src    = i

         do j = loaded, src+1, -1

            if ( kt%kernels(j)%source == src ) then
!
!              This file was loaded by the meta-kernel of interest.
!              We only need to unload the binary kernels as we
!              will get rid of all text kernels by clearing the
!              kernel pool.
!
!              See whether the file we're about to process is
!              binary, and if so, count the number of times
!              it appears in the database. We have to repeat
!              this test on each loop pass, since the count
!              may have changed since the last pass.
!
               if (     ( kt%kernels(j)%type == 'SPK' ) &
                   .or. ( kt%kernels(j)%type == 'CK'  ) &
                   .or. ( kt%kernels(j)%type == 'DSK' ) &
                   .or. ( kt%kernels(j)%type == 'EK'  ) &
                   .or. ( kt%kernels(j)%type == 'PCK' ) ) then
!
!                 Count the occurrences of the file in the database.
!                 Stop if we reach two occurrences.
!
                  nmult = 0
                  k     = 1

                  do while (      ( k     <= loaded ) &
                            .and. ( nmult < 2      ) )

                     if ( kt%kernels(k)%handle == kt%kernels(j)%handle ) then
!
!                       To be safe, make sure we're not looking at a
!                       text kernel with a random, matching handle
!                       value.
!
                        if (     ( kt%kernels(k)%type /= 'TEXT' ) &
                           .and. ( kt%kernels(k)%type /= 'META' ) ) then

                           nmult = nmult + 1

                        end if

                     end if

                     k = k + 1

                  end do

                  single = nmult == 1

               end if

               if ( kt%kernels(j)%type == 'SPK' ) then

                  if ( single ) then
                     call spkuef ( ctx, kt%kernels(j)%handle )
                  else
                     didspk = .true.
                  end if

               else if ( kt%kernels(j)%type == 'CK' ) then

                  if ( single ) then
                     call ckupf ( ctx, kt%kernels(j)%handle )
                  else
                     didck = .true.
                  end if

               else if ( kt%kernels(j)%type == 'DSK' ) then

                  if ( single ) then
                     call zzdskusf ( ctx, kt%kernels(j)%handle )
                  else
                     diddsk = .true.
                  end if

               else if ( kt%kernels(j)%type == 'PCK' ) then

                  if ( single ) then
                     call pckuof ( ctx, kt%kernels(j)%handle )
                  else
                     didpck = .true.
                  end if

               else if ( kt%kernels(j)%type == 'EK' ) then

                  if ( single ) then
                     call ekuef ( ctx, kt%kernels(j)%handle )
                  else
                     didek = .true.
                  end if

               end if

               call notify_kernel_unloaded ( ctx, j )  ! PEPPER: a kernel loader's callback
               call kt%remove ( j )

!
!              Each time we delete an item from the database, any
!              pointer to a location past the deletion point must be
!              updated to reflect the compression of the database.
!              Files loaded from meta kernels are always recorded
!              in the database *after* their sources, so each pointer
!              value is less than the index at which it occurs.
!              So, we need examine only those entries from index J
!              upwards.
!
               do k = j, loaded

                  if ( kt%kernels(k)%source > j ) then
!
!                    This pointer is affected by the deletion of
!                    the Jth database entry.
!
                     kt%kernels(k)%source = kt%kernels(k)%source-1

                  end if

               end do

            end if

         end do
!
!        Now clear the kernel pool.
!
         call clpool ( ctx )

      end if
!
!     Remove the I'th kernel from our local database.
!
      call notify_kernel_unloaded ( ctx, i )  ! PEPPER: a kernel loader's callback
      call kt%remove ( i )

!
!     Update any source pointers affected by the deletion of the Ith
!     database entry.
!
      do j = i, loaded

         if ( kt%kernels(j)%source > i ) then
!
!           This pointer is affected by the deletion of the Ith
!           database entry.
!
            kt%kernels(j)%source = kt%kernels(j)%source-1

         end if

      end do

!
!     If we unloaded a text kernel, we now need to reload all
!     of the text kernels that were not unloaded.
!
      if ( didtxt ) then

         do i = 1, loaded

            if (      kt%kernels(i)%type == 'TEXT' &
                 .or. kt%kernels(i)%type == 'META' ) then

               if ( allocated ( kt%kernels(i)%memory_text ) ) then
                  call ldpool_text ( ctx, kt%kernels(i)%name, kt%kernels(i)%memory_text )
               else
                  call ldpool ( ctx, kt%kernels(i)%name )
               end if

               if ( kt%kernels(i)%type == 'META' ) then
!
!                 Clean up any debris that may have been left lying
!                 around because we reloaded a meta-text kernel.
!
                  do j = 1, nknown
                     call dvpool ( ctx, known(j) )
                  end do
                  call dvpool ( ctx, 'KERNEL_LOADERS' )

                  call cvpool ( ctx, 'FURNSH', update )

               end if

            end if

         end do

      end if

!
!     If any SPK files were unloaded, we need to reload everything
!     to establish the right priority sequence for segments.
!
!     PEPPER: only when an unloaded entry was a second load of a file
!     still loaded (SINGLE false): DIDSPK, DIDCK, DIDDSK, DIDPCK and
!     DIDEK are set only then. A file loaded once is removed from its
!     subsystem's file table (SPKUEF, ...), which keeps the order of
!     the other files, and that is the order the reload would restore.
!     SPICELIB reloaded every remaining file of the type, which made
!     unloading N files take O(N**2) file opens (and also moved the
!     files loaded through FURNSH above those loaded directly, e.g. with
!     SPKLEF; PEPPER keeps the existing order).
!
      if ( didspk ) then

         do i = 1, loaded
            if ( kt%kernels(i)%type == 'SPK' ) then
               call spklef ( ctx, kt%kernels(i)%name, kt%kernels(i)%handle )
            end if
         end do

      end if
!
!     If any CK files were unloaded, we need to reload all of the
!     C-kernels to make sure that we have the correct priorities
!     for the remaining C-kernels.
!
      if ( didck ) then

         do i = 1, loaded
            if ( kt%kernels(i)%type == 'CK' ) then
               call cklpf ( ctx, kt%kernels(i)%name, kt%kernels(i)%handle )
            end if
         end do

      end if

!
!     If any DSK files were unloaded, we need to reload the remaining
!     ones to make sure that we have the correct priorities for the
!     remaining DSKs.
!
      if ( diddsk ) then

         do i = 1, loaded
            if ( kt%kernels(i)%type == 'DSK' ) then
               call zzdsklsf ( ctx, kt%kernels(i)%name, kt%kernels(i)%handle )
            end if
         end do

      end if

!
!     If any binary PCK files were unloaded, we need to reload any
!     remaining ones to re-establish the correct priorities for
!     kernels.
!
      if ( didpck ) then

         do i = 1, loaded
            if ( kt%kernels(i)%type == 'PCK' ) then
               call pcklof ( ctx, kt%kernels(i)%name, kt%kernels(i)%handle )
            end if
         end do

      end if
!
!     Finally, if any E-kernels were unloaded, we reload the remaining
!     kernels to make sure the state is restored to the correct set
!     of loaded kernels.
!
      if ( didek ) then

         do i = 1, loaded
            if ( kt%kernels(i)%type == 'EK' ) then
               call eklef ( ctx, kt%kernels(i)%name, kt%kernels(i)%handle )
            end if
         end do

      end if

      return
      end associate
      end subroutine unload

      !> Load a SPICE kernel (binary DAF SPK/CK/PCK, ASCII text kernel, or
      !> JSON kernel) directly from an in-memory byte buffer without any
      !> filesystem access (PEPPER).
      module subroutine furnsh_memory ( ctx, name, bytes )
      use, intrinsic :: iso_fortran_env, only: int8, int64
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: nextwd_inplace, pos, ucase_inplace
      use pepper_inc_zzddhman
      use pepper_inc_zzftprms
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Virtual kernel name (e.g. 'de440s.bsp', 'naif0012.tls').
      integer(int8), intent(in) :: bytes(:)  !! Raw kernel bytes.
      integer(ip), parameter :: hdrlen = 12
      integer(ip), parameter :: recl = 1024
      integer(ip), parameter :: minpch = 32
      integer(ip), parameter :: maxpch = 126
      character(:), allocatable :: locfnm, text, short_err, long_err
      character(hdrlen) :: tmpwrd, idword
      character(typlen) :: arch, mytype
      character(strsiz) :: strbff(numbff)
      character(8) :: bffidw, null8
      character(sizstr) :: ftpmem
      character(sizdlm) :: ftpdlm
      character(sizend) :: ftplft
      character(sizend) :: ftprgt
      character(len=recl), allocatable :: raw_records(:)
      real(wp), allocatable :: image(:)
      type(ddh_file) :: fentry
      type(keeper_kernel) :: kentry
      integer(int64) :: nbytes, nrec, base
      integer(ip) :: bff, natbff, handle, tmp_han, i, b, r, ch, ddh_idx
      integer :: stat
      logical :: update, ftperr, found, added_ddh

      if ( ctx%failed() ) return

      locfnm = trim ( adjustl ( name ) )
      if ( locfnm == ' ' ) then
         call ctx%setmsg ( 'The attempt to load a kernel from memory failed ' &
         //                'because the kernel name is blank.' )
         call ctx%sigerr ( 'SPICE(BLANKFILENAME)' )
         return
      end if

      associate ( kt => ctx%spice%keeper, &
                  known => ctx%spice%keeper%known, &
                  first => ctx%spice%keeper%first )

      if ( first ) then
         first    = .false.
         known(1) = 'KERNELS_TO_LOAD'
         known(2) = 'PATH_SYMBOLS'
         known(3) = 'PATH_VALUES'
         kt%loaded = 0
         call swpool ( ctx, 'FURNSH', nknown, known )
         call cvpool ( ctx, 'FURNSH', update )
      end if

      call cvpool ( ctx, 'FURNSH', update )

      nbytes = size ( bytes, kind=int64 )
      tmpwrd = ' '
      do i = 1, int ( min ( int(hdrlen, int64), nbytes ), ip )
         if ( bytes(i) == 10_int8 .or. bytes(i) == 13_int8 ) exit
         ch = modulo ( int(bytes(i), ip), 256 )
         if ( ch >= minpch .and. ch <= maxpch ) then
            tmpwrd(i:i) = achar(ch)
         end if
      end do

      tmpwrd = adjustl ( tmpwrd )
      call ucase_inplace ( tmpwrd )
      idword = ' '
      call nextwd_inplace ( tmpwrd, idword )

      if ( idword == 'DAFETF' ) then
         arch   = 'XFR'
         mytype = 'DAF'
      else if ( idword == 'DASETF' ) then
         arch   = 'XFR'
         mytype = 'DAS'
      else if ( idword(1:10) == '''NAIF/DAF''' ) then
         arch   = 'DEC'
         mytype = 'DAF'
      else if ( idword(1:8) == 'NAIF/DAS' ) then
         arch   = 'DAS'
         mytype = 'PRE'
      else
         call idw2at ( ctx, idword(1:8), arch, mytype )
      end if

      if ( arch == 'XFR' .or. arch == 'DEC' ) then
         call ctx%setmsg ( 'The attempt to load "#" from memory failed: ' &
         //                'transfer format files cannot be loaded.' )
         call ctx%errch  ( '#', locfnm )
         call ctx%sigerr ( 'SPICE(TRANSFERFILE)' )
         return
      else if ( arch == 'TE1' ) then
         call ctx%setmsg ( 'The attempt to load "#" from memory failed: ' &
         //                'type 1 text E-kernels are obsolete.' )
         call ctx%errch  ( '#', locfnm )
         call ctx%sigerr ( 'SPICE(TYPE1TEXTEK)' )
         return
      else if ( arch == 'DAS' ) then
         call ctx%setmsg ( 'The attempt to load "#" from memory failed: ' &
         //                'DAS binary kernels are not supported by furnsh_memory.' )
         call ctx%errch  ( '#', locfnm )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDARCH)' )
         return
      end if

      if ( arch == 'DAF' ) then
         if ( nbytes < int(recl, int64) .or. mod(nbytes, int(recl, int64)) /= 0_int64 ) then
            call ctx%setmsg ( 'In-memory DAF kernel ''#'' has invalid byte length # ' &
            //                '(must be a positive multiple of 1024).' )
            call ctx%errch  ( '#', locfnm )
            call ctx%errint ( '#', int(min(nbytes, int(huge(1_ip), int64)), ip) )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)' )
            return
         end if

         nrec = nbytes / int(recl, int64)
         allocate ( raw_records(nrec), stat=stat )
         if ( stat /= 0 ) then
            call ctx%setmsg ( 'Not enough memory to allocate raw records for ''#''.' )
            call ctx%errch  ( '#', locfnm )
            call ctx%sigerr ( 'SPICE(MALLOCFAILED)' )
            return
         end if

         do r = 1, int(nrec, ip)
            base = int(r - 1, int64) * int(recl, int64)
            do b = 1, recl
               raw_records(r)(b:b) = achar ( modulo ( int(bytes(base + b), ip), 256 ) )
            end do
         end do

         call zzftpchk ( ctx, raw_records(1)(500:1000), ftperr )
         if ( ftperr ) then
            call ctx%setmsg ( 'FTP transfer error detected in binary DAF ''#''.' )
            call ctx%errch  ( '#', locfnm )
            call ctx%sigerr ( 'SPICE(FTPXFERERROR)' )
            return
         end if

         call zzddhnfc ( ctx, natbff )
         do i = 1, numbff
            call zzddhgsd ( ctx, 'BFF', i, strbff(i) )
         end do
         call zzftpstr ( ctx, ftpmem, ftplft, ftprgt, ftpdlm )
         if ( ctx%failed() ) return

         null8 = repeat ( achar(0), 8 )
         bff   = 0_ip
         if ( pos ( raw_records(1)(500:1000), ftplft, 1 ) /= 0 ) then
            bffidw = raw_records(1)(89:96)
            if ( bffidw /= null8 ) then
               bff = findloc ( strbff(1:numbff), bffidw, dim=1 )
               if ( bff == 0_ip ) then
                  call ctx%setmsg ( 'The DAF ''#'' uses unrecognized binary file format ''#''.' )
                  call ctx%errch  ( '#', locfnm )
                  call ctx%errch  ( '#', bffidw )
                  call ctx%sigerr ( 'SPICE(UNKNOWNBFF)' )
                  return
               end if
            end if
         end if

         if ( bff == 0_ip ) then
            if ( raw_records(1)(13:13) == achar(0) .and. &
                 raw_records(1)(14:14) == achar(0) .and. &
                 raw_records(1)(15:15) == achar(0) .and. &
                 raw_records(1)(16:16) /= achar(0) ) then
               bff = bigi3e
            else if ( raw_records(1)(13:13) /= achar(0) .and. &
                      raw_records(1)(14:14) == achar(0) .and. &
                      raw_records(1)(15:15) == achar(0) .and. &
                      raw_records(1)(16:16) == achar(0) ) then
               bff = ltli3e
            else
               call ctx%setmsg ( 'Unable to determine binary file format of DAF ''#''.' )
               call ctx%errch  ( '#', locfnm )
               call ctx%sigerr ( 'SPICE(UNKNOWNBFF)' )
               return
            end if
         end if

         if ( bff /= ltli3e .and. bff /= bigi3e ) then
            call ctx%setmsg ( 'Unsupported binary file format in DAF ''#''.' )
            call ctx%errch  ( '#', locfnm )
            call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)' )
            return
         end if

         allocate ( image(128_int64 * nrec), stat=stat )
         if ( stat /= 0 ) then
            call ctx%setmsg ( 'Not enough memory to allocate DAF image for ''#''.' )
            call ctx%errch  ( '#', locfnm )
            call ctx%sigerr ( 'SPICE(MALLOCFAILED)' )
            return
         end if

         do r = 1, int(nrec, ip)
            if ( bff == natbff ) then
               base = int(r - 1, int64) * int(recl, int64)
               image(128*(r - 1) + 1 : 128*r) = transfer ( bytes(base + 1_int64 : base + int(recl, int64)), 0.0_wp, 128 )
            else
               call zzxlated ( ctx, bff, raw_records(r), 128_ip, image(128*(r - 1) + 1 : 128*r) )
               if ( ctx%failed() ) return
            end if
         end do

         call zzddhfnh ( ctx, locfnm, handle, found )
         added_ddh = .false.
         if ( .not. found ) then
            ctx%spice%zzddhman%next = ctx%spice%zzddhman%next + 1_ip
            handle        = ctx%spice%zzddhman%next
            fentry%handle = handle
            fentry%method = read
            fentry%arch   = daf
            fentry%bff    = bff
            fentry%mnm    = 0.0_wp
            fentry%name   = locfnm
            call move_alloc ( image, fentry%image )
            call move_alloc ( raw_records, fentry%raw_records )
            call ctx%spice%zzddhman%add ( fentry )
            added_ddh = .true.
         end if

         tmp_han = 0_ip
         if ( mytype == '?' ) then
            call dafopr ( ctx, locfnm, tmp_han )
            if ( .not. ctx%failed() ) call zzckspk ( ctx, tmp_han, mytype )
         end if

         if ( .not. ctx%failed() ) then
            if ( mytype == 'SPK' ) then
               call spklef ( ctx, locfnm, handle )
            else if ( mytype == 'CK' ) then
               call cklpf ( ctx, locfnm, handle )
            else if ( mytype == 'PCK' ) then
               call pcklof ( ctx, locfnm, handle )
            else
               call ctx%setmsg ( 'Unsupported DAF kernel type ''#'' in ''#''.' )
               call ctx%errch  ( '#', mytype )
               call ctx%errch  ( '#', locfnm )
               call ctx%sigerr ( 'SPICE(UNKNOWNKERNELTYPE)' )
            end if
         end if

         ! Keep the discovery reference alive while the typed loader opens
         ! the memory image. Closing its last reference removes the bytes.
         if ( tmp_han /= 0_ip ) then
            if ( ctx%failed() ) then
               short_err = short_text ( ctx )
               long_err  = long_text ( ctx )
               call ctx%reset()
               call dafcls ( ctx, tmp_han )
               call ctx%setmsg ( long_err )
               call ctx%sigerr ( short_err )
            else
               call dafcls ( ctx, tmp_han )
            end if
         end if

         if ( ctx%failed() ) then
            if ( added_ddh ) then
               short_err = short_text ( ctx )
               long_err  = long_text ( ctx )
               call ctx%reset()
               ddh_idx = ctx%spice%zzddhman%find ( handle )
               if ( ddh_idx > 0 ) call ctx%spice%zzddhman%remove ( ddh_idx )
               call ctx%setmsg ( long_err )
               call ctx%sigerr ( short_err )
            end if
            return
         end if

         call kt%add ( keeper_kernel ( name = locfnm, type = mytype, handle = handle, source = 0 ) )
      else
         allocate ( character(nbytes) :: text )
         do base = 1_int64, nbytes
            text(base:base) = achar ( modulo ( int(bytes(base), ip), 256 ) )
         end do
         call ldpool_text ( ctx, locfnm, text )
         if ( ctx%failed() ) return
         call zzbodkik ( ctx )
         if ( ctx%failed() ) return
         kentry%name   = locfnm
         kentry%type   = 'TEXT'
         kentry%handle = 0_ip
         kentry%source = 0_ip
         call move_alloc ( text, kentry%memory_text )
         call kt%add ( kentry )
         call cvpool ( ctx, 'FURNSH', update )
      end if

      end associate
      end subroutine furnsh_memory

end submodule pepper_keeper
