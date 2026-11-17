!> Cells, sets and windows: SPICELIB's arrays with a control area (size and cardinality), sets
!> (ordered cells without duplicates) and windows (sets of intervals).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_cells

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: lbcell = -5  !! lower bound of a SPICE cell (its control area: elements -5 to 0)
   integer(ip), parameter :: minlen = 5  !! minimum length of the strings of a character cell

contains

      !> Return the base value used to encode unsigned integer values
      !> in character strings.
      !>
      !> The function returns the base value used to encode unsigned
      !> integer values in character strings.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     CHBASE is the base used by ENCHAR and DECHAR to encode and decode
      !>     non-negative integers to and from character strings. Its value is
      !>     determined by the size of the character set available for a given
      !>     machine and compiler. Strictly speaking, CHBASE is one more than
      !>     the biggest positive integer which can be handled by both the
      !>     CHAR and ICHAR intrinsic functions (which are used by ENCHAR and
      !>     DECHAR). That is, CHBASE is the first positive integer for which
      !>     the logical expression
      !>
      !>           ( ICHAR ( CHAR ( CHBASE ) ) .EQ. CHBASE )
      !>
      !>     is .FALSE.
      !>
      !>     Note that CHBASE can be (and probably is) different from the
      !>     number of characters in the character set used by the processor.
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
      !>     The function always returns a constant value, set by the user
      !>     prior to compilation.
      !>
      !>     CHBASE should always be at least 128 (the size of the ASCII
      !>     character set), and will usually be 256 for machines which use
      !>     eight bits to represent a single character. The following list
      !>     contains the values of CHBASE for a range of environments.
      !>
      !>     Environment: VAX/VMS, VAX FORTRAN
      !>     Value:       256
      !>
      !>     Environment: Sun, Sun FORTRAN
      !>     Value:       256
      !>
      !>     Environment: PC, MS FORTRAN
      !>     Value:       256
      !>
      !>     Environment: Macintosh, Language Systems FORTRAN
      !>     Value:       256
      !>
      !>     Environment: PC, Lahey F77 EM/32 Version 4.0
      !>     Value:       256
      !>
      !>     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
      !>     Value:       256
      !>
      !>     Environment: Silicon Graphics IRIX OS, SGI FORTRAN 77
      !>     Value:       256
      !>
      !>     Environment: DEC Alpha 3000/4000, OSF/1, DEC FORTRAN-77
      !>     Value:       256
      !>
      !>     Environment: NeXT/Mach OS, Absoft Fortran
      !>     Value:       256
      !>
      !>     Environment: PC/Linux, Fort77
      !>     Value:       128
      !>
      !>     For other machines, the value can be determined by running
      !>     the following simple program:
      !>
      !>            INTEGER          CHBASE
      !>            DATA             CHBASE    / 0 /
      !>
      !>            DO WHILE ( .TRUE. )
      !>
      !>               IF ( ICHAR (CHAR ( CHBASE ) ) .EQ. CHBASE ) THEN
      !>                  CHBASE = CHBASE + 1
      !>               ELSE
      !>                  WRITE (6,*) 'CHBASE for this machine is : ', CHBASE
      !>                  STOP
      !>               END IF
      !>
      !>            END DO
      !>            END
      !>
      !>## Examples
      !>
      !>     See ENCHAR, DECHAR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  "Programming in VAX FORTRAN", Digital Equipment Corporation,
      !>          Section 8.3, pp.8-6, September 1984.
      !>
      !>     [2]  "Microsoft FORTRAN Reference", Microsoft Corporation,
      !>          Section 5.1.1, p.241, 1989.
      !>
      !>     [3]  "Language Systems FORTRAN Reference Manual", Language Systems
      !>          Corporation, version 1.2.1, pp.3-20.
      !>
      !>     [4]  "Lahey F77L EM/32 FORTRAN Language Reference Manual", p.222,
      !>          Note 20.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 2.26.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.26.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 2.21.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.11.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.9.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 2.8.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 2.4.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 2.3.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 2.1.1, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 2.1.0, 05-DEC-2001 (FST)
      !>
      !>        Updated the value for PC-LINUX environment.
      !>
      !>-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 2.0.3, 24-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
      !>
      !>        Added reference to the PC-LINUX environment.
      !>
      !>-    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN)
      !>
      !>        Module was updated to include the character base
      !>        value for the Silicon Graphics, DEC Alpha-OSF/1, and
      !>        NeXT platforms.
      !>
      !>-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
      !>
      !>        Module was updated to include the character base
      !>        value for the Hewlett Packard UX 9000/750 environment,
      !>        and the value for the Sun was changed from 128 to 256.
      !>        Both changes are the result of running the program in
      !>        the $Particulars section of the header on both machines.
      !>
      !>-    SPICELIB Version 1.3.1, 10-MAR-1999 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
      !>
      !>        Module was updated to include the character base
      !>        value for the Lahey FORTRAN EM/32 environment (PC).
      !>
      !>-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
      !>
      !>        Module was updated to include the character base
      !>        value for the Macintosh.
      !>
      !>-    SPICELIB Version 1.1.0, 09-MAR-1990 (HAN)
      !>
      !>        Module was updated to include the character base
      !>        value for the Sun. Sources for the values contained
      !>        in this module are now specified in the $Literature_References
      !>        section.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 05-DEC-2001 (FST)
      !>
      !>        It was discovered that linux distributions shipping
      !>        versions of g77 derived off of gcc versions 2.96-3.00
      !>        suffer from in implementation change in ICHAR that
      !>        requires CHBASE to change to 128. Since restricting
      !>        CHBASE to 128 has little impact on other linux
      !>        environments utilizing other versions of g77 or fort77,
      !>        we elected to make the change to all environments
      !>        rather than complicate this issue by forking a new one.
      !>
      !>-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
      !>
      !>        Module was updated to include the character base
      !>        value for the Hewlett Packard UX 9000/750 environment,
      !>        and the value for the Sun was changed from 128 to 256.
      !>        Both changes are the result of running the program in
      !>        the $Particulars section of the header on both machines.
      !>
      !>        The previous Sun value was computed on the Sun3 and was
      !>        not updated when we moved to the Sun4. Everything passed
      !>        the suite of test programs that would have indicated a bug.
      !>
      !>        The code was also reformatted so that a utility program can
      !>        create the file for each environment.
      !>
      !>-    Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
      !>
      !>        Contents of the $Exceptions section was changed
      !>        to "error free" to reflect the decision that the
      !>        module will never participate in error handling.
      !>
      !>        Missing parentheses added to CHBASE declaration.
      module integer(ip) function chbase ( ctx )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context

!
!     PEPPER: one value for every platform. CHBASE is used by ENCHAR and
!     DECHAR, which encode integers in characters held in memory only;
!     any value up to 256 works with every compiler (SPICELIB used 256
!     on Windows with Intel Fortran, 128 elsewhere).
!
      chbase = 128

      return
      end function chbase

      !> Encode a nonnegative integer number into a character string
      !> as the expansion of the number in base [[chbase]] (a function of
      !> the size of the available character set).
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is an arbitrary nonnegative integer.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is the character string implied by the ASCII
      !>              interpretation of NUMBER when converted to its
      !>              base CHBASE representation.
      !>
      !>              Let L be the declared length of STRING, and let
      !>              NUMBER be given by
      !>
      !>                                  0           1                 L-1
      !>                 NUMBER = a CHBASE  + a CHBASE  + ... + a CHBASE
      !>                           1           2                 L
      !>
      !>              Then
      !>
      !>                 STRING(i:i) = CHAR(a )   for i = 1, L
      !>                                     i
      !>
      !>              Note that, just as for any other "numbers",
      !>              the "digits" in STRING are arranged from right
      !>              to left in order of increasing significance.
      !>              The string is, in effect, "padded with nulls"
      !>              on the left.
      !>
      !>## Parameters
      !>
      !>     MINLEN   is the minimum length of a string into which a
      !>              number may be encoded. In order to avoid padding
      !>              long strings with hundreds, possibly thousands
      !>              of null characters, only the first MINLEN characters
      !>              of the string are actually used. Note that this
      !>              also allows the encoded number to be preserved
      !>              during assignments,
      !>
      !>                 STR1 = STR2
      !>
      !>              so long as both strings are of length MINLEN or
      !>              greater.
      !>
      !>## Exceptions
      !>
      !>     1)  If the length of the output string is less than MINLEN,
      !>         the error SPICE(INSUFFLEN) is signaled.
      !>
      !>     2)  If the number to be encoded is negative, the error
      !>         SPICE(OUTOFRANGE) is signaled.
      !>
      !>                                                          MINLEN
      !>     3)  If the number to be encoded is larger than CHBASE       - 1,
      !>         the error SPICE(OUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The value of CHBASE, which varies from machine to machine, is
      !>     returned by a constant function of the same name.
      !>
      !>## Examples
      !>
      !>     See: SCARDC, SSIZEC.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
      !>
      !>        Changed header section title '$C Revision' to '$C Revisions'.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-JAN-1989 (IMU)
      !>
      !>        Only the first MINLEN characters of the string are now
      !>        used to encode the value. Also, negative values are now
      !>        treated as errors.
      module subroutine enchar ( ctx, number, string )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: number  !! Number to be encoded.
      character(*), intent(out) :: string  !! Encoded string.


      integer(ip) :: base
      integer(ip) :: num
      integer(ip) :: remain
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         return

      else if ( len ( string ) < minlen ) then
         call ctx%sigerr ( 'SPICE(INSUFFLEN)' )
         return

      else if ( number < 0 ) then
         call ctx%sigerr ( 'SPICE(OUTOFRANGE)' )
         return
      end if

!
!     Generate the digits from right to left.
!
      base = chbase( ctx )
      num  = number

      do i = minlen, 1, -1

         remain      = mod ( num, base )
         string(i:i) = char ( remain )
         num         = num / base

      end do

!
!     More error handling.
!
      if ( num > 0 ) then
         call ctx%sigerr ( 'SPICE(OUTOFRANGE)' )
      end if

      return
      end subroutine enchar

      !> Decode a character string encoded by [[enchar]].
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string previously encoded by ENCHAR.
      !>              This contains an integer in base CHBASE notation,
      !>              where CHBASE is a function of the size of the
      !>              available character set. See ENCHAR for details
      !>              about the format of STRING.
      !>
      !>## Detailed Output
      !>
      !>     NUMBER   is the integer encoded in the input string.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the length of the input string is less than MINLEN,
      !>         the error SPICE(INSUFFLEN) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DECHAR is the inverse of ENCHAR. In the example below,
      !>
      !>           CALL ENCHAR (      I, STRING )
      !>           CALL DECHAR ( STRING,      J )
      !>
      !>           IF ( I .EQ. J ) THEN
      !>            .
      !>            .
      !>           END IF
      !>
      !>     the logical test (I .EQ. J) is always true.
      !>
      !>## Examples
      !>
      !>     See: CARDC, SIZEC.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
      !>
      !>        Changed header section title '$C Revision' to '$C Revisions'.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-JAN-1989 (IMU)
      !>
      !>        Changed to reflect changes in ENCHAR. In particular,
      !>        it now checks the length of the input string. It is
      !>        also an entry point of ENCHAR, to make sure they always
      !>        have the same value of MINLEN. (Also, if CHBASE is
      !>        changed, ENCHAR and DECHAR will always be recompiled
      !>        simultaneously.)
      module subroutine dechar ( ctx, string, number )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: number  !! Decoded number.
      character(*), intent(in) :: string  !! Encoded character string.
      integer(ip) :: base
      integer(ip) :: num
      integer(ip) :: remain
      integer(ip) :: i

      if ( ctx%failed() ) then
         return

!
!     Standard SPICE error handling.
!
      else if ( len ( string ) < minlen ) then
         call ctx%sigerr ( 'SPICE(INSUFFLEN)' )
         return
      end if

!
!     Sum the products of the 'digits' and the corresponding powers
!     of NDCHAR, just like any other base conversion.
!
      base   = chbase( ctx )
      number = 0

      do i = 1, minlen
         number = base*number + ichar ( string(i:i) )
      end do

      return
      end subroutine dechar

      !> Set the long error message so as to indicate the number of excess
      !> elements encountered by a routine operating on cells or on data
      !> structures based on cells.
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is the number of excess elements encountered.
      !>              This may be zero or negative, which indicates
      !>              no excess.
      !>
      !>     STRUCT   is the name of the data structure being manipulated.
      !>              Typically, this is one of the strings: 'cell', 'set',
      !>              or 'symbol table'. However, it may be any character
      !>              string. STRUCT should NOT end in a period.
      !>              The period at the end of the message is supplied
      !>              automatically.
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
      !>     1)  This routine does not detect any errors.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is part of the SPICELIB error handling mechanism.
      !>
      !>     EXCESS sets the long error message. The message has the form:
      !>
      !>                 An excess of <NUMBER> element(s) could
      !>                 not be accommodated in the output <STRUCT>.
      !>
      !>     Leading and trailing blanks in STRUCT are removed. If there is
      !>     no excess (NUMBER is zero or negative), then is blank.
      !>
      !>## Examples
      !>
      !>     The response of EXCESS to a variety of inputs is illustrated
      !>     below.
      !>
      !>           NUMBER = 1
      !>           STRUCT = 'set'
      !>           ERROR  = 'An excess of 1 element could not
      !>                     be accommodated in the output set.'
      !>
      !>           NUMBER = 5
      !>           STRUCT = 'stack'
      !>           ERROR  =  An excess of 5 elements could not
      !>                     be accommodated in the output stack.'
      !>
      !>           NUMBER = 0
      !>           STRUCT =
      !>           ERROR  = ' '
      !>
      !>           NUMBER = -6
      !>           STRUCT =
      !>           ERROR  = ' '
      !>
      !>     In particular, note that EXCESS does not set the long error
      !>     message when the number of excess elements is not positive. Also,
      !>     the singular 'element' is used for an excess of one, while
      !>     the plural 'elements' is used for all other positive excesses.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 11-JAN-1989 (NJB)
      !>
      !>        Sets the long error message directly. No longer returns
      !>        an error message. Message no longer contains name of
      !>        routine which detected the error.
      module subroutine excess ( ctx, number, struct )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, prefix, suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: number  !! Number of excess elements.
      character(*), intent(in) :: struct  !! Name of the data structure.

      integer(ip), parameter :: longln = 320
      character(longln) :: error
!
!     Set up the error processing.
!
      if ( ctx%failed() ) return
!
!     If there is no excess, don't report one.
!
      if ( number > 0 ) then

!
!        Begin with the number. We will build the rest of the
!        message around it.
!
         call intstr ( number, error )

!
!        A short blurb goes in front of the number.
!
         call prefix ( 'An excess of', 1, error )

!
!        Singular or plural?
!
         if ( number == 1 ) then
            call suffix ( 'element', 1, error )
         else
            call suffix ( 'elements', 1, error )
         end if

!
!        Another short blurb.
!
         call suffix ( 'could not be accommodated in the output', &
                       1, &
                       error                                      )

!
!        And the name of the structure.
!
         call suffix ( struct, 1, error   )

!
!        And a period at the end, to complete the sentence.
!
         call suffix ( '.', 0, error )

!
!        Set the long error message:
!
         call ctx%setmsg ( error )

      else

          error = ' '

      end if

      return
      end subroutine excess

      !> Return the cardinality (number of elements) of a character cell.
      !>
      !> The function returns the cardinality of the input cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the cardinality of (number of elements in)
      !>     the input cell.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input array has invalid cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled. CARDC returns
      !>         an unspecified value in this case.
      !>
      !>     2)  If the input array has invalid size, the error
      !>         SPICE(INVALIDSIZE) is signaled. CARDC returns
      !>         an unspecified value in this case.
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
      !>     The cardinality (CARD) functions are typically used to process
      !>     each of the elements of a cell. In the following example, CARDC
      !>     is used to step through the individual elements of the character
      !>     cell NAMES.
      !>
      !>           DO I = 1, CARDC ( NAMES )
      !>            .
      !>            .
      !>           END DO
      !>
      !>     In conjunction with the size (SIZE) functions, they may be used
      !>     to predict (and subsequently avoid) overflows when manipulating
      !>     cells. In the following example, SIZEC is used to determine
      !>     whether the character cell ORIGINAL can be safely copied into
      !>     the character cell SAVE before actually attempting the operation.
      !>     If ORIGINAL contains more elements than SAVE can hold, then
      !>     the operation would fail.
      !>
      !>           IF ( CARDC ( ORIGINAL ) .LE. SIZEC ( SAVE ) ) THEN
      !>              CALL COPYC ( ORIGINAL, SAVE )
      !>
      !>           ELSE
      !>            .
      !>            .
      !>           END IF
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 29-JUL-2002 (NJB)
      !>
      !>        Errors in code fragments in the $Examples section of
      !>        the header were corrected.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>       If the value of the function RETURN is .TRUE. upon execution of
      !>       this module, this function is assigned a default value of
      !>       either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>       function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for valid input cell added. The input cell must
      !>        have valid size and cardinality values.
      module integer(ip) function cardc ( ctx, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: cell  ( lbcell:* )  !! Input cell.


      integer(ip) :: card
      integer(ip) :: size

      if ( ctx%failed() ) then
         cardc = 0
         return
      end if
!
!     Set return value, regardless of validity.
!
      call dechar ( ctx, cell(0), card )
      cardc = card

!
!     Squeal if something is awry.
!

      call dechar ( ctx, cell(-1), size )

      if (  size < 0  )  then

         call ctx%setmsg ( 'Invalid cell size.  The size was #.')
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return

      else if ( card < 0  ) then

         call ctx%setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call ctx%errint ( '#', card )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      else if ( card > size ) then

         call ctx%setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call ctx%errint ( '#', card )
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

      return
      end function cardc

      !> Return the cardinality (number of elements) of a double
      !> precision cell.
      !>
      !> The function returns the cardinality of the input cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the cardinality of (number of elements in)
      !>     the input cell.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input array has invalid cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled. CARDD returns
      !>         an unspecified value in this case.
      !>
      !>     2)  If the input array has invalid size, the error
      !>         SPICE(INVALIDSIZE) is signaled. CARDD returns
      !>         an unspecified value in this case.
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
      !>     The cardinality (CARD) functions are typically used to process
      !>     each of the elements of a cell. In the following example, CARDC
      !>     is used to step through the individual elements of the character
      !>     cell NAMES.
      !>
      !>           DO I = 1, CARDC ( NAMES )
      !>            .
      !>            .
      !>           END DO
      !>
      !>     In conjunction with the size (SIZE) functions, they may be used
      !>     to predict (and subsequently avoid) overflows when manipulating
      !>     cells. In the following example, SIZED is used to determine
      !>     whether the d.p. cell ORIGINAL can be safely copied into
      !>     the d.p. cell SAVE before actually attempting the operation.
      !>     If ORIGINAL contains more elements than SAVE can hold, then
      !>     the operation would fail.
      !>
      !>           IF ( CARDD ( ORIGINAL ) .LE. SIZED ( SAVE ) ) THEN
      !>              CALL COPYD ( ORIGINAL, SAVE )
      !>
      !>           ELSE
      !>            .
      !>            .
      !>           END IF
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 29-JUL-2002 (NJB)
      !>
      !>        Errors in code fragments in the $Examples section of
      !>        the header were corrected.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>       If the value of the function RETURN is .TRUE. upon execution of
      !>       this module, this function is assigned a default value of
      !>       either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>       function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for valid input cell added. The input cell must
      !>        have valid size and cardinality values.
      module integer(ip) function cardd ( ctx, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: cell  ( lbcell:* )  !! Input cell.


      if ( ctx%failed() ) then
         cardd = 0
         return
      end if

!
!     Set return value, regardless of validity.
!
      cardd = int ( cell(0) )

!
!     Squeal if something is awry.
!

      if (  int( cell(-1) ) < 0  )  then

         call ctx%setmsg ( 'Invalid cell size.  The size was #.')
         call ctx%errint ( '#', int( cell(-1) ) )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return

      else if ( int( cell(0) ) < 0  ) then

         call ctx%setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call ctx%errint ( '#', int( cell(0) )  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      else if ( int(cell(0)) > int(cell(-1))  ) then

         call ctx%setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call ctx%errint ( '#', int(cell( 0))  )
         call ctx%errint ( '#', int(cell(-1))  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

      return
      end function cardd

      !> Return the cardinality (number of elements) of an integer cell.
      !>
      !> The function returns the cardinality of the input cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the cardinality of (number of elements in)
      !>     the input cell.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input array has invalid cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled. CARDI returns
      !>         an unspecified value in this case.
      !>
      !>     2)  If the input array has invalid size, the error
      !>         SPICE(INVALIDSIZE) is signaled. CARDI returns
      !>         an unspecified value in this case.
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
      !>     The cardinality (CARD) functions are typically used to process
      !>     each of the elements of a cell. In the following example, CARDC
      !>     is used to step through the individual elements of the character
      !>     cell NAMES.
      !>
      !>           DO I = 1, CARDC ( NAMES )
      !>            .
      !>            .
      !>           END DO
      !>
      !>     In conjunction with the size (SIZE) functions, they may be used
      !>     to predict (and subsequently avoid) overflows when manipulating
      !>     cells. In the following example, SIZEI is used to determine
      !>     whether the integer cell ORIGINAL can be safely copied into
      !>     the integer cell SAVE before actually attempting the operation.
      !>     If ORIGINAL contains more elements than SAVE can hold, then
      !>     the operation would fail.
      !>
      !>           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
      !>              CALL COPYI ( ORIGINAL, SAVE )
      !>
      !>           ELSE
      !>            .
      !>            .
      !>           END IF
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 29-JUL-2002 (NJB)
      !>
      !>        Errors in code fragments in the $Examples section of
      !>        the header were corrected.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>       If the value of the function RETURN is .TRUE. upon execution of
      !>       this module, this function is assigned a default value of
      !>       either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>       function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for valid input cell added. The input cell must
      !>        have valid size and cardinality values.
      module integer(ip) function cardi ( ctx, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: cell  ( lbcell:* )  !! Input cell.


      if ( ctx%failed() ) then
         cardi = 0
         return
      end if

!
!     Set return value, regardless of validity.
!
      cardi = cell(0)

!
!     Squeal if something is awry.
!

      if (  cell(-1) < 0  )  then

         call ctx%setmsg ( 'Invalid cell size.  The size was #.')
         call ctx%errint ( '#', cell(-1) )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return

      else if ( cell(0) < 0  ) then

         call ctx%setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call ctx%errint ( '#', cell(0)  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      else if ( cell(0) > cell(-1)  ) then

         call ctx%setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call ctx%errint ( '#', cell( 0)  )
         call ctx%errint ( '#', cell(-1)  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

      return
      end function cardi

      !> Return the size (maximum cardinality) of a character cell.
      !>
      !> The function returns the size of the input cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the size of (maximum number of elements in)
      !>     the input cell.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input array has invalid cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled. SIZEC returns
      !>         an unspecified value in this case.
      !>
      !>     2)  If the input array has invalid size, the error
      !>         SPICE(INVALIDSIZE) is signaled. SIZEC returns
      !>         an unspecified value in this case.
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
      !>     The size (SIZE) functions are typically used in conjunction
      !>     with the cardinality functions to predict (and subsequently
      !>     avoid) overflows when manipulating cells. In the following
      !>     example, SIZEI is used to determine whether the integer cell
      !>     ORIGINAL can be safely copied into the integer cell SAVE before
      !>     actually attempting the operation. (If ORIGINAL contains more
      !>     elements than SAVE is capable of holding, then the operation
      !>     will fail.)
      !>
      !>           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
      !>              CALL COPYI ( ORIGINAL, SAVE, ERROR )
      !>
      !>           ELSE
      !>            .
      !>            .
      !>           END DO
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for valid input cell added. The input cell must
      !>        have valid size and cardinality values.
      module integer(ip) function sizec ( ctx, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: cell   ( lbcell:* )  !! Input cell.


      integer(ip) :: size
      integer(ip) :: card

      if ( ctx%failed() ) then
         sizec = 0
         return
      end if
!
!     Set return value, regardless of validity.
!
      call dechar ( ctx, cell(-1), size )
      sizec = size

!
!     Squeal if something is awry.
!

      call dechar ( ctx, cell(0), card )

      if (  size < 0  )  then

         call ctx%setmsg ( 'Invalid cell size.  The size was #.')
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return

      else if ( card < 0  ) then

         call ctx%setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call ctx%errint ( '#', card )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      else if ( card > size ) then

         call ctx%setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call ctx%errint ( '#', card )
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

      return
      end function sizec

      !> Return the size (maximum cardinality) of a double precision
      !> cell.
      !>
      !> The function returns the size of the input cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the size of (maximum number of elements in)
      !>     the input cell.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input array has invalid cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled. SIZEI returns
      !>         an unspecified value in this case.
      !>
      !>     2)  If the input array has invalid size, the error
      !>         SPICE(INVALIDSIZE) is signaled. SIZEI returns
      !>         an unspecified value in this case.
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
      !>     The size (SIZE) functions are typically used in conjunction
      !>     with the cardinality functions to predict (and subsequently
      !>     avoid) overflows when manipulating cells. In the following
      !>     example, SIZEI is used to determine whether the integer cell
      !>     ORIGINAL can be safely copied into the integer cell SAVE before
      !>     actually attempting the operation. (If ORIGINAL contains more
      !>     elements than SAVE is capable of holding, then the operation
      !>     will fail.)
      !>
      !>           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
      !>              CALL COPYI ( ORIGINAL, SAVE, ERROR )
      !>
      !>           ELSE
      !>            .
      !>            .
      !>           END DO
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for valid input cell added. The input cell must
      !>        have valid size and cardinality values.
      module integer(ip) function sized ( ctx, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: cell   ( lbcell:* )  !! Input cell.


      if ( ctx%failed() ) then
         sized = 0
         return
      end if

!
!     Set return value, regardless of validity.
!
      sized = int ( cell(-1) )

!
!     Squeal if something is awry.
!

      if (  int( cell(-1) )  <  0  )  then

         call ctx%setmsg ( 'Invalid cell size.  The size was #.')
         call ctx%errint ( '#', int( cell(-1) )  )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'  )
         return

      else if ( int( cell(0) )  <  0  ) then

         call ctx%setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call ctx%errint ( '#', int ( cell(0) )  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)'  )
         return

      else if ( int(cell(0)) > int(cell(-1))  ) then

         call ctx%setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call ctx%errint ( '#', int(cell( 0))  )
         call ctx%errint ( '#', int(cell(-1))  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

      return
      end function sized

      !> Return the size (maximum cardinality) of an integer cell.
      !>
      !> The function returns the size of the input cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the size of (maximum number of elements in)
      !>     the input cell.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input array has invalid cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled. SIZEI returns
      !>         an unspecified value in this case.
      !>
      !>     2)  If the input array has invalid size, the error
      !>         SPICE(INVALIDSIZE) is signaled. SIZEI returns
      !>         an unspecified value in this case.
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
      !>     The size (SIZE) functions are typically used in conjunction
      !>     with the cardinality functions to predict (and subsequently
      !>     avoid) overflows when manipulating cells. In the following
      !>     example, SIZEI is used to determine whether the integer cell
      !>     ORIGINAL can be safely copied into the integer cell SAVE before
      !>     actually attempting the operation. (If ORIGINAL contains more
      !>     elements than SAVE is capable of holding, then the operation
      !>     will fail.)
      !>
      !>           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN
      !>              CALL COPYI ( ORIGINAL, SAVE, ERROR )
      !>
      !>           ELSE
      !>            .
      !>            .
      !>           END DO
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for valid input cell added. The input cell must
      !>        have valid size and cardinality values.
      module integer(ip) function sizei ( ctx, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: cell  ( lbcell: * )  !! Input cell.


      if ( ctx%failed() ) then
         sizei = 0
         return
      end if

!
!     Set return value, regardless of validity.
!
      sizei = cell(-1)

!
!     Squeal if something is awry.
!

      if (  cell(-1) < 0  )  then

         call ctx%setmsg ( 'Invalid cell size.  The size was #.')
         call ctx%errint ( '#', cell(-1)  )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return

      else if ( cell(0) < 0  ) then

         call ctx%setmsg ( 'Invalid cell cardinality.  The cardinality '  // &
                       'was #.'      )
         call ctx%errint ( '#', cell(0)  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      else if ( cell(0) > cell(-1)  ) then

         call ctx%setmsg ( 'Invalid cell cardinality; cardinality exceeds'// &
                       ' cell size.  The cardinality was #.  The size'// &
                       ' was #.'  )
         call ctx%errint ( '#', cell( 0)  )
         call ctx%errint ( '#', cell(-1)  )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

      return
      end function sizei

      !> Set the cardinality of a character cell.
      !>
      !>## Detailed Input
      !>
      !>     CARD     is the cardinality of (number of elements in) the
      !>              cell.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is a cell.
      !>
      !>              On output, the cardinality of the cell is CARD.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the cardinality value supplied is less than 0 or greater
      !>         than the cell size, the error SPICE(INVALIDCARDINALITY) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
      !>     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
      !>     initialize cells for subsequent use. Since all cell routines
      !>     expect to find the size and cardinality of a cell in place,
      !>     no cell can be used until both have been set.
      !>
      !>     The set cardinality routines are also used by library routines
      !>     which manipulate cells (including set and window routines) to
      !>     reset the cardinalities of cells as they gain or lose elements.
      !>
      !>## Examples
      !>
      !>     In the example below, the size and cardinality of the character
      !>     cell FRED are set in the main module of the program FLNSTN.
      !>     Both are subsequently retrieved, and the cardinality changed,
      !>     in one of its subroutines, WILMA.
      !>
      !>           PROGRAM FLNSTN
      !>
      !>           CHARACTER*30     FRED ( LBCELL:100 )
      !>            .
      !>            .
      !>           CALL SSIZEC ( 100, FRED )
      !>            .
      !>            .
      !>           CALL WILMA ( FRED )
      !>            .
      !>            .
      !>           STOP
      !>           END
      !>
      !>           SUBROUTINE WILMA ( FRED )
      !>
      !>           CHARACTER*(*)      FRED  ( LBCELL:* )
      !>           INTEGER            SIZE
      !>           INTEGER            CARD
      !>
      !>           INTEGER            CARDC
      !>           INTEGER            SIZEC
      !>            .
      !>            .
      !>           SIZE = SIZEC ( FRED )
      !>           CARD = CARDC ( FRED )
      !>            .
      !>            .
      !>           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
      !>           CALL EXCESS ( CARD-SIZE, 'cell' )
      !>            .
      !>            .
      !>           RETURN
      !>           END
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for invalid cardinality value added. An error
      !>        is signaled if the value is out of range. Examples
      !>        updated so as not to refer to the EMPTYx routines, and
      !>        to show the correct calling protocol for EXCESS.
      module subroutine scardc ( ctx, card, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
      character(*), intent(inout) :: cell   ( lbcell:* )  !! The cell.


      integer(ip) :: size

      if ( ctx%failed() ) return

!
!     The cardinality may range from 0 to the size of the cell,
!     inclusive.  Other values will be snubbed.
!

      call dechar ( ctx, cell(-1), size )

      if (  ( card < 0 )  .or.  ( card > size )  )  then
         call ctx%setmsg ( 'Attempt to set cardinality of cell '          // &
                       'to invalid value.  The value was #.'  )
         call ctx%errint ( '#', card )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return
      end if

!
!     Not much to this.
!
      call enchar ( ctx, card, cell(0) )

      return
      end subroutine scardc

      !> Set the cardinality of a double precision cell.
      !>
      !>## Detailed Input
      !>
      !>     CARD     is the cardinality of (number of elements in) the
      !>              cell.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is a cell.
      !>
      !>                On output, the cardinality of the cell is CARD.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the cardinality value supplied is less than 0 or greater
      !>         than the cell size, the error SPICE(INVALIDCARDINALITY) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
      !>     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
      !>     initialize cells for subsequent use. Since all cell routines
      !>     expect to find the size and cardinality of a cell in place,
      !>     no cell can be used until both have been set.
      !>
      !>     The set cardinality routines are also used by library routines
      !>     which manipulate cells (including set and window routines) to
      !>     reset the cardinalities of cells as they gain or lose elements.
      !>
      !>## Examples
      !>
      !>     In the example below, the size and cardinality of the character
      !>     cell FRED are set in the main module of the program FLNSTN.
      !>     Both are subsequently retrieved, and the cardinality changed,
      !>     in one of its subroutines, WILMA.
      !>
      !>           PROGRAM FLNSTN
      !>
      !>           CHARACTER*30     FRED ( LBCELL:100 )
      !>            .
      !>            .
      !>           CALL SSIZEC ( 100, FRED )
      !>            .
      !>            .
      !>           CALL WILMA ( FRED )
      !>            .
      !>            .
      !>           STOP
      !>           END
      !>
      !>           SUBROUTINE WILMA ( FRED )
      !>
      !>           CHARACTER*(*)      FRED  ( LBCELL:* )
      !>           INTEGER            SIZE
      !>           INTEGER            CARD
      !>
      !>           INTEGER            CARDC
      !>           INTEGER            SIZEC
      !>            .
      !>            .
      !>           SIZE = SIZEC ( FRED )
      !>           CARD = CARDC ( FRED )
      !>            .
      !>            .
      !>           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
      !>           CALL EXCESS ( CARD-SIZE, 'cell' )
      !>            .
      !>            .
      !>           RETURN
      !>           END
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for invalid cardinality value added. An error
      !>        is signaled if the value is out of range. Examples
      !>        updated so as not to refer to the EMPTYx routines, and
      !>        to show the correct calling protocol for EXCESS.
      module subroutine scardd ( ctx, card, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
      real(wp), intent(inout) :: cell   ( lbcell:* )  !! The cell.


      if ( ctx%failed() ) return

!
!     The cardinality may range from 0 to the size of the cell,
!     inclusive.  Other values will be snubbed.
!
      if (  ( card < 0 ) .or. ( card > int( cell(-1) ) )  )  then
         call ctx%setmsg ( 'Attempt to set cardinality of cell '          // &
                       'to invalid value.  The value was #.'  )
         call ctx%errint ( '#', card )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return
      end if

!
!     Not much to this.
!
      cell(0) = real( card, wp )

      return
      end subroutine scardd

      !> Set the cardinality of an integer cell.
      !>
      !>## Detailed Input
      !>
      !>     CARD     is the cardinality of (number of elements in) the
      !>              cell.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is a cell.
      !>
      !>                On output, the cardinality of the cell is CARD.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the cardinality value supplied is less than 0 or greater
      !>         than the cell size, the error SPICE(INVALIDCARDINALITY) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
      !>     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
      !>     initialize cells for subsequent use. Since all cell routines
      !>     expect to find the size and cardinality of a cell in place,
      !>     no cell can be used until both have been set.
      !>
      !>     The set cardinality routines are also used by library routines
      !>     which manipulate cells (including set and window routines) to
      !>     reset the cardinalities of cells as they gain or lose elements.
      !>
      !>## Examples
      !>
      !>     In the example below, the size and cardinality of the character
      !>     cell FRED are set in the main module of the program FLNSTN.
      !>     Both are subsequently retrieved, and the cardinality changed,
      !>     in one of its subroutines, WILMA.
      !>
      !>           PROGRAM FLNSTN
      !>
      !>           CHARACTER*30     FRED ( LBCELL:100 )
      !>            .
      !>            .
      !>           CALL SSIZEC ( 100, FRED )
      !>            .
      !>            .
      !>           CALL WILMA ( FRED )
      !>            .
      !>            .
      !>           STOP
      !>           END
      !>
      !>           SUBROUTINE WILMA ( FRED )
      !>
      !>           CHARACTER*(*)      FRED  ( LBCELL:* )
      !>           INTEGER            SIZE
      !>           INTEGER            CARD
      !>
      !>           INTEGER            CARDC
      !>           INTEGER            SIZEC
      !>            .
      !>            .
      !>           SIZE = SIZEC ( FRED )
      !>           CARD = CARDC ( FRED )
      !>            .
      !>            .
      !>           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
      !>           CALL EXCESS ( CARD-SIZE, 'cell' )
      !>            .
      !>            .
      !>           RETURN
      !>           END
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for invalid cardinality value added. An error
      !>        is signaled if the value is out of range. Examples
      !>        updated so as not to refer to the EMPTYx routines, and
      !>        to show the correct calling protocol for EXCESS.
      module subroutine scardi ( ctx, card, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
      integer(ip), intent(inout) :: cell   ( lbcell:* )  !! The cell.


      if ( ctx%failed() ) return

!
!     The cardinality may range from 0 to the size of the cell,
!     inclusive.  Other values will be snubbed.
!
      if (  ( card < 0 )  .or.  ( card > cell(-1) )  )  then
         call ctx%setmsg ( 'Attempt to set cardinality of cell '          // &
                       'to invalid value.  The value was #.'  )
         call ctx%errint ( '#', card )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return
      end if

!
!     Not much to this.
!
      cell(0) = card

      return
      end subroutine scardi

      !> Set the size (maximum cardinality) of a character cell.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the size (maximum number of elements) of the cell.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is a cell.
      !>
      !>                On output, the size of the cell is SIZE. The
      !>                cardinality of the cell is 0. The rest of the
      !>                control area is zeroed out.
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
      !>     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
      !>     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
      !>     initialize cells for subsequent use. Since all cell routines
      !>     expect to find the size and cardinality of a cell in place,
      !>     no cell can be used until both have been set.
      !>
      !>## Examples
      !>
      !>     In the example below, the size and cardinality of the character
      !>     cell FRED are set in the main module of the program FLNSTN.
      !>     Both are subsequently retrieved, and the cardinality changed,
      !>     in one of its subroutines, WILMA.
      !>
      !>           PROGRAM FLNSTN
      !>
      !>           CHARACTER*30     FRED ( LBCELL:100 )
      !>            .
      !>            .
      !>           CALL SSIZEC ( 100, FRED )
      !>            .
      !>            .
      !>           CALL WILMA ( FRED )
      !>            .
      !>            .
      !>           STOP
      !>           END
      !>
      !>           SUBROUTINE WILMA ( FRED )
      !>
      !>           CHARACTER*(*)      FRED  ( LBCELL:* )
      !>           INTEGER            SIZE
      !>           INTEGER            CARD
      !>
      !>           INTEGER            CARDC
      !>           INTEGER            SIZEC
      !>            .
      !>            .
      !>           SIZE = SIZEC ( FRED )
      !>           CARD = CARDC ( FRED )
      !>            .
      !>            .
      !>           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
      !>           CALL EXCESS ( CARD-SIZE, 'cell' )
      !>            .
      !>            .
      !>           RETURN
      !>           END
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for invalid size value added. An error
      !>        is signaled if the value is out of range. The cardinality
      !>        is now automatically reset to 0. The rest of the control
      !>        area is now zeroed out.
      !>
      !>        The examples have been updated to illustrate set initialization
      !>        without the use of the EMPTYx routines, which have been
      !>        removed from the library. Errors in the examples have been
      !>        removed, also.
      module subroutine ssizec ( ctx, size, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
      character(*), intent(inout) :: cell   ( lbcell:* )  !! The cell.


      integer(ip) :: i

      if ( ctx%failed() ) return

!
!     The size must be non-negative.  Other values will be snubbed.
!
      if (  size < 0  )  then
         call ctx%setmsg ( 'Attempt to set size of cell to invalid '      // &
                       'value.  The value was #.'  )
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if

!
!     Not much to this.
!
      call enchar ( ctx, size, cell(-1) )
      call enchar ( ctx, 0,    cell( 0) )

      do i = lbcell, -2
         call enchar ( ctx, 0, cell(i) )
      end do

      return
      end subroutine ssizec

      !> Set the size (maximum cardinality) of a double precision cell.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the size (maximum number of elements) of the cell.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is a cell.
      !>
      !>                On output, the size of the cell is SIZE. The
      !>                cardinality of the cell is 0. The rest of the
      !>                control area is zeroed out.
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
      !>     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
      !>     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
      !>     initialize cells for subsequent use. Since all cell routines
      !>     expect to find the size and cardinality of a cell in place,
      !>     no cell can be used until both have been set.
      !>
      !>## Examples
      !>
      !>     In the example below, the size and cardinality of the character
      !>     cell FRED are set in the main module of the program FLNSTN.
      !>     Both are subsequently retrieved, and the cardinality changed,
      !>     in one of its subroutines, WILMA.
      !>
      !>           PROGRAM FLNSTN
      !>
      !>           CHARACTER*30     FRED ( LBCELL:100 )
      !>            .
      !>            .
      !>           CALL SSIZEC ( 100, FRED )
      !>            .
      !>            .
      !>           CALL WILMA ( FRED )
      !>            .
      !>            .
      !>           STOP
      !>           END
      !>
      !>           SUBROUTINE WILMA ( FRED )
      !>
      !>           CHARACTER*(*)      FRED  ( LBCELL:* )
      !>           INTEGER            SIZE
      !>           INTEGER            CARD
      !>
      !>           INTEGER            CARDC
      !>           INTEGER            SIZEC
      !>            .
      !>            .
      !>           SIZE = SIZEC ( FRED )
      !>           CARD = CARDC ( FRED )
      !>            .
      !>            .
      !>           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
      !>           CALL EXCESS ( CARD-SIZE, 'cell' )
      !>            .
      !>            .
      !>           RETURN
      !>           END
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for invalid size value added. An error
      !>        is signaled if the value is out of range. The cardinality
      !>        is now automatically reset to 0. The rest of the control
      !>        area is now zeroed out.
      !>
      !>        The examples have been updated to illustrate set initialization
      !>        without the use of the EMPTYx routines, which have been
      !>        removed from the library. Errors in the examples have been
      !>        removed, also.
      module subroutine ssized ( ctx, size, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
      real(wp), intent(inout) :: cell   ( lbcell:* )  !! The cell.


      integer(ip) :: i

      if ( ctx%failed() ) return

!
!     The size must be non-negative.  Other values will be snubbed.
!
      if (  size < 0  )  then
         call ctx%setmsg ( 'Attempt to set size of cell to invalid '      // &
                       'value.  The value was #.'  )
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if

!
!     Not much to this.
!
      cell(-1) = real( size, wp )
      cell( 0) = 0.0_wp

      do i = lbcell, -2
         cell(i) = 0.0_wp
      end do

      return
      end subroutine ssized

      !> Set the size (maximum cardinality) of an integer cell.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the size (maximum number of elements) of the cell.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is a cell.
      !>
      !>                On output, the size of the cell is SIZE. The
      !>                cardinality of the cell is 0. The rest of the
      !>                control area is zeroed out.
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
      !>     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size
      !>     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to
      !>     initialize cells for subsequent use. Since all cell routines
      !>     expect to find the size and cardinality of a cell in place,
      !>     no cell can be used until both have been set.
      !>
      !>## Examples
      !>
      !>     In the example below, the size and cardinality of the character
      !>     cell FRED are set in the main module of the program FLNSTN.
      !>     Both are subsequently retrieved, and the cardinality changed,
      !>     in one of its subroutines, WILMA.
      !>
      !>           PROGRAM FLNSTN
      !>
      !>           CHARACTER*30     FRED ( LBCELL:100 )
      !>            .
      !>            .
      !>           CALL SSIZEC ( 100, FRED )
      !>            .
      !>            .
      !>           CALL WILMA ( FRED )
      !>            .
      !>            .
      !>           STOP
      !>           END
      !>
      !>           SUBROUTINE WILMA ( FRED )
      !>
      !>           CHARACTER*(*)      FRED  ( LBCELL:* )
      !>           INTEGER            SIZE
      !>           INTEGER            CARD
      !>
      !>           INTEGER            CARDC
      !>           INTEGER            SIZEC
      !>            .
      !>            .
      !>           SIZE = SIZEC ( FRED )
      !>           CARD = CARDC ( FRED )
      !>            .
      !>            .
      !>           CALL SCARDC ( MIN ( SIZE, CARD ), FRED )
      !>           CALL EXCESS ( CARD-SIZE, 'cell' )
      !>            .
      !>            .
      !>           RETURN
      !>           END
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Check for invalid size value added. An error
      !>        is signaled if the value is out of range. The cardinality
      !>        is now automatically reset to 0. The rest of the control
      !>        area is now zeroed out.
      !>
      !>        The examples have been updated to illustrate set initialization
      !>        without the use of the EMPTYx routines, which have been
      !>        removed from the library. Errors in the examples have been
      !>        removed, also.
      module subroutine ssizei ( ctx, size, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
      integer(ip), intent(inout) :: cell   ( lbcell:* )  !! The cell.


      integer(ip) :: i

      if ( ctx%failed() ) return

!
!     The size must be non-negative.  Other values will be snubbed.
!
      if (  size < 0  )  then
         call ctx%setmsg ( 'Attempt to set size of cell to invalid '      // &
                       'value.  The value was #.'  )
         call ctx%errint ( '#', size )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if

!
!     Not much to this.
!
      cell(-1) = size
      cell( 0) = 0

      do i = lbcell, -2
         cell(i) = 0
      end do

      return
      end subroutine ssizei

      !> Create a valid set from a character set array.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the maximum cardinality (number of elements)
      !>              of the set.
      !>
      !>     N        is the number of (possibly non-distinct) elements
      !>              initially contained in the array used to maintain
      !>              the set. N cannot be greater than the size of the
      !>              set.
      !>
      !>     A        is a set.
      !>
      !>              On input, A contains N elements beginning at A(1).
      !>              To create a valid set, the elements are ordered,
      !>              and duplicate elements are removed. The contents
      !>              of A(LBCELL) through A(0) are lost during validation.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, is the set containing the ordered,
      !>              distinct values in the input array, ready for
      !>              use with other set routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the size of the set is too small to hold the set BEFORE
      !>         validation, the error SPICE(INVALIDSIZE) is signaled. The set
      !>         A is not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is typically used to turn an array which has been
      !>     initialized through DATA or I/O statements into a set, which
      !>     can then be used with the other set routines.
      !>
      !>     Because a set is ordered and contains distinct values, to
      !>     create a set from an array, it is necessary to sort the array
      !>     into the set and remove duplicates. Once the array has been
      !>     sorted, duplicate elements (adjacent after sorting) are removed.
      !>     The size and cardinality of the set are initialized, and the
      !>     set is ready to go.
      !>
      !>     Because validation is done in place, there is no chance of
      !>     overflow.
      !>
      !>## Examples
      !>
      !>     Empty sets may be initialized with the cell routines SSIZEx.
      !>     Sets may also be initialized from nonempty set arrays.
      !>     This process, called validation, is done by the set routines
      !>     VALIDC and VALIDI. In the following example,
      !>
      !>           INTEGER      BODIES  ( LBCELL:100 )
      !>
      !>           DATA       ( BODIES(I), I=1,8)       /  3, 301,
      !>          .                                        3, 399,
      !>          .                                        5, 501,
      !>          .                                        6, 601,  /
      !>
      !>           CALL VALIDI ( 100, 8, BODIES )
      !>
      !>     the integer set BODIES is validated. The size of BODIES set to
      !>     100. The eight elements of the array (stored in elements 1-8)
      !>     are sorted, and duplicate elements (in this case, the number 3,
      !>     which appears twice) are removed, and the cardinality of the set
      !>     is set to the number of distinct elements, now seven. The set is
      !>     now ready for use with the rest of the set routines.
      !>
      !>     The previous contents of elements LBCELL through 0 are lost
      !>     during the process of validation.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Now participates in error handling. References to RETURN,
      !>        CHKIN, and CHKOUT added. Check for adequate set size added.
      !>
      !>        The examples have been updated to illustrate set initialization
      !>        without the use of the EMPTYx routines, which have been
      !>        removed from the library. Errors in the examples have been
      !>        removed, also.
      module subroutine validc ( ctx, size, n, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: rmdupc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the set.
      integer(ip), intent(in) :: n  !! Initial no. of (possibly non-distinct) elements.
      character(*), intent(inout) :: a      ( lbcell:* )  !! Set to be validated.


      integer(ip) :: card

      if ( ctx%failed() ) return

!
!     Is the set size big enough?
!
      if ( n > size ) then

         call ctx%setmsg ( 'Size of un-validated set is too small.  '     // &
                       'Size is #, size required is #. ' )

         call ctx%errint ( '#', size )
         call ctx%errint ( '#', n    )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if

!
!     Just like it says above. Order the array, and remove duplicates.
!
      card = n
      call rmdupc ( card, a(1) )

!
!     Set the size and cardinality of the input set.
!
      call ssizec ( ctx, size, a )
      call scardc ( ctx, card, a )

      return
      end subroutine validc

      !> Create a valid set from a double precision set array.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the maximum cardinality (number of elements)
      !>              of the set.
      !>
      !>     N        is the number of (possibly non-distinct) elements
      !>              initially contained in the array used to maintain
      !>              the set. N cannot be greater than the size of the
      !>              set.
      !>
      !>     A        is a set.
      !>
      !>              On input, A contains N elements beginning at A(1).
      !>              To create a valid set, the elements are ordered,
      !>              and duplicate elements are removed. The contents
      !>              of A(LBCELL) through A(0) are lost during validation.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, is the set containing the ordered,
      !>              distinct values in the input array, ready for
      !>              use with other set routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the size of the set is too small to hold the set BEFORE
      !>         validation, the error SPICE(INVALIDSIZE) is signaled. The set
      !>         A is not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is typically used to turn an array which has been
      !>     initialized through DATA or I/O statements into a set, which
      !>     can then be used with the other set routines.
      !>
      !>     Because a set is ordered and contains distinct values, to
      !>     create a set from an array, it is necessary to sort the array
      !>     into the set and remove duplicates. Once the array has been
      !>     sorted, duplicate elements (adjacent after sorting) are removed.
      !>     The size and cardinality of the set are initialized, and the
      !>     set is ready to go.
      !>
      !>     Because validation is done in place, there is no chance of
      !>     overflow.
      !>
      !>## Examples
      !>
      !>     Empty sets may be initialized with the cell routines SSIZEx.
      !>     Sets may also be initialized from nonempty set arrays.
      !>     This process, called validation, is done by the set routines
      !>     VALIDC and VALIDI. In the following example,
      !>
      !>           INTEGER      BODIES  ( LBCELL:100 )
      !>
      !>           DATA       ( BODIES(I), I=1,8)       /  3, 301,
      !>          .                                        3, 399,
      !>          .                                        5, 501,
      !>          .                                        6, 601,  /
      !>
      !>           CALL VALIDI ( 100, 8, BODIES )
      !>
      !>     the integer set BODIES is validated. The size of BODIES set to
      !>     100. The eight elements of the array (stored in elements 1-8)
      !>     are sorted, and duplicate elements (in this case, the number 3,
      !>     which appears twice) are removed, and the cardinality of the set
      !>     is set to the number of distinct elements, now seven. The set is
      !>     now ready for use with the rest of the set routines.
      !>
      !>     The previous contents of elements LBCELL through 0 are lost
      !>     during the process of validation.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Now participates in error handling. References to RETURN,
      !>        CHKIN, and CHKOUT added. Check for adequate set size added.
      !>
      !>        The examples have been updated to illustrate set initialization
      !>        without the use of the EMPTYx routines, which have been
      !>        removed from the library. Errors in the examples have been
      !>        removed, also.
      module subroutine validd ( ctx, size, n, a )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: rmdupd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the set.
      integer(ip), intent(in) :: n  !! Initial no. of (possibly non-distinct) elements.
      real(wp), intent(inout) :: a       ( lbcell:* )  !! Set to be validated.


      integer(ip) :: card

!
!     Standard error handling:
!
      if ( ctx%failed() ) return

!
!     Is the set size big enough?
!
      if ( n > size ) then

         call ctx%setmsg ( 'Size of un-validated set is too small.  '     // &
                       'Size is #, size required is #. ' )

         call ctx%errint ( '#', size )
         call ctx%errint ( '#', n    )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if

!
!     Just like it says above. Order the array, and remove duplicates.
!
      card = n
      call rmdupd ( card, a(1) )

!
!     Set the size and cardinality of the input set.
!
      call ssized ( ctx, size, a )
      call scardd ( ctx, card, a )

      return
      end subroutine validd

      !> Create a valid set from an integer set array.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the maximum cardinality (number of elements)
      !>              of the set.
      !>
      !>     N        is the number of (possibly non-distinct) elements
      !>              initially contained in the array used to maintain
      !>              the set. N cannot be greater than the size of the
      !>              set.
      !>
      !>     A        is a set.
      !>
      !>              On input, A contains N elements beginning at A(1).
      !>              To create a valid set, the elements are ordered,
      !>              and duplicate elements are removed. The contents
      !>              of A(LBCELL) through A(0) are lost during validation.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, is the set containing the ordered,
      !>              distinct values in the input array, ready for
      !>              use with other set routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the size of the set is too small to hold the set BEFORE
      !>         validation, the error SPICE(INVALIDSIZE) is signaled. The set
      !>         A is not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is typically used to turn an array which has been
      !>     initialized through DATA or I/O statements into a set, which
      !>     can then be used with the other set routines.
      !>
      !>     Because a set is ordered and contains distinct values, to
      !>     create a set from an array, it is necessary to sort the array
      !>     into the set and remove duplicates. Once the array has been
      !>     sorted, duplicate elements (adjacent after sorting) are removed.
      !>     The size and cardinality of the set are initialized, and the
      !>     set is ready to go.
      !>
      !>     Because validation is done in place, there is no chance of
      !>     overflow.
      !>
      !>## Examples
      !>
      !>     Empty sets may be initialized with the cell routines SSIZEx.
      !>     Sets may also be initialized from nonempty set arrays.
      !>     This process, called validation, is done by the set routines
      !>     VALIDC, VALIDD, and VALIDI. In the following example,
      !>
      !>           INTEGER      BODIES  ( LBCELL:100 )
      !>
      !>           DATA       ( BODIES(I), I=1,8)       /  3, 301,
      !>          .                                        3, 399,
      !>          .                                        5, 501,
      !>          .                                        6, 601,  /
      !>
      !>           CALL VALIDI ( 100, 8, BODIES )
      !>
      !>     the integer set BODIES is validated. The size of BODIES set to
      !>     100. The eight elements of the array (stored in elements 1-8)
      !>     are sorted, and duplicate elements (in this case, the number 3,
      !>     which appears twice) are removed, and the cardinality of the set
      !>     is set to the number of distinct elements, now seven. The set is
      !>     now ready for use with the rest of the set routines.
      !>
      !>     The previous contents of elements LBCELL through 0 are lost
      !>     during the process of validation.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 13-MAR-1989 (NJB)
      !>
      !>        Now participates in error handling. References to RETURN,
      !>        CHKIN, and CHKOUT added. Check for adequate set size added.
      !>
      !>        The examples have been updated to illustrate set initialization
      !>        without the use of the EMPTYx routines, which have been
      !>        removed from the library. Errors in the examples have been
      !>        removed, also.
      module subroutine validi ( ctx, size, n, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: rmdupi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the set.
      integer(ip), intent(in) :: n  !! Initial no. of (possibly non-distinct) elements.
      integer(ip), intent(inout) :: a       ( lbcell:* )  !! Set to be validated.


      integer(ip) :: card

      if ( ctx%failed() ) return

!
!     Is the set size big enough?
!
      if ( n > size ) then

         call ctx%setmsg ( 'Size of un-validated set is too small.  '     // &
                       'Size is #, size required is #. ' )

         call ctx%errint ( '#', size )
         call ctx%errint ( '#', n    )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if

!
!     Just like it says above. Order the array, and remove duplicates.
!
      card = n
      call rmdupi ( card, a(1) )

!
!     Set the size and cardinality of the input set.
!
      call ssizei ( ctx, size, a )
      call scardi ( ctx, card, a )

      return
      end subroutine validi

      !> Append an item to a character cell.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is a character string which is to be appended to CELL.
      !>
      !>     CELL     is a character SPICE cell to which ITEM will be
      !>              appended.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is the input cell with ITEM appended. ITEM is the last
      !>              member of CELL.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input cell has invalid cardinality, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the input cell has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the cell is not large enough to accommodate the addition
      !>         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
      !>
      !>     4)  If the length of the item is longer than the length of the
      !>         cell, ITEM is truncated on the right.
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
      !>     In the following example, the item 'PLUTO' is appended to
      !>     the character cell PLANETS.
      !>
      !>     Before appending 'PLUTO', the cell contains:
      !>
      !>     PLANETS (1) = 'MERCURY'
      !>     PLANETS (2) = 'VENUS'
      !>     PLANETS (3) = 'EARTH'
      !>     PLANTES (4) = 'MARS'
      !>     PLANETS (5) = 'JUPITER'
      !>     PLANETS (6) = 'SATURN'
      !>     PLANETS (7) = 'URANUS'
      !>     PLANETS (8) = 'NEPTUNE'
      !>
      !>     The call
      !>
      !>        CALL APPNDC ( 'PLUTO', PLANETS )
      !>
      !>     appends the element 'PLUTO' at the location PLANETS (9), and the
      !>     cardinality is updated.
      !>
      !>     If the cell is not big enough to accommodate the addition of
      !>     the item, an error is signaled. In this case, the cell is not
      !>     altered.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Improved the documentation of CELL in $Detailed_Input and
      !>        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
      module subroutine appndc ( ctx, item, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: item  !! The item to append.
      character(*), intent(inout) :: cell ( lbcell:* )  !! The cell to which ITEM will be
                                                        !! appended.


      integer(ip) :: nwcard

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the cell can accommodate the addition of a
!     new item. If there is room, append the item to the cell and
!     reset the cardinality. If the cell cannot accommodate the
!     addition of a new item, signal an error.
!

      nwcard = cardc ( ctx,cell) + 1

      if (  ( nwcard )  <=  ( sizec ( ctx,cell) )  ) then

         cell (nwcard) = item
         call scardc ( ctx, nwcard, cell )

      else

        call ctx%setmsg ( 'The cell cannot accommodate the '     // &
                      'addition of the item *.'                 )

        call ctx%errch  ( '*', item )
        call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )

      end if

      return
      end subroutine appndc

      !> Append an item to a double precision cell.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is a double precision value which is to be appended to
      !>              CELL.
      !>
      !>     CELL     is a double precision SPICE cell to which ITEM will be
      !>              appended.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is the input cell with ITEM appended. ITEM is the last
      !>              member of CELL.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input cell has invalid cardinality, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the input cell has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the cell is not big enough to accommodate the addition
      !>         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a cell for fifteen elements, add a first element to
      !>        it and then append several more double precision numbers.
      !>        Validate the cell into a set and print the result.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM APPNDD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDD
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ = 9  )
      !>
      !>      INTEGER                 SIZE
      !>      PARAMETER             ( SIZE   = 15 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        A      ( LBCELL:SIZE )
      !>      DOUBLE PRECISION        ITEMS  ( LISTSZ      )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Set the list of double precision numbers to be appended
      !>C     to the cell.
      !>C
      !>      DATA                    ITEMS /  3.D0,  1.D0,  1.D0,
      !>     .                                 2.D0,  5.D0,  8.D0,
      !>     .                                21.D0, 13.D0, 34.D0  /
      !>
      !>C
      !>C     Initialize the cell.
      !>C
      !>      CALL SSIZED ( SIZE, A )
      !>
      !>C
      !>C     Add a single item to the new cell.
      !>C
      !>      CALL APPNDD ( 0.D0, A )
      !>
      !>C
      !>C     Now insert a list of items.
      !>C
      !>      DO I= 1, LISTSZ
      !>
      !>         CALL APPNDD ( ITEMS(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the original contents of cell A.
      !>C
      !>      WRITE(*,*) 'Items in original cell A:'
      !>      WRITE(*,'(15F6.1)') ( A(I), I = 1, CARDD ( A ) )
      !>
      !>C
      !>C     Validate the set: remove duplicates and sort the
      !>C     elements.
      !>C
      !>      CALL VALIDD ( SIZE, CARDD( A ), A )
      !>
      !>C
      !>C     Output the contents of the set A.
      !>C
      !>      WRITE(*,*) 'Items in cell A after VALIDD (now a set):'
      !>      WRITE(*,'(15F6.1)') ( A(I), I = 1, CARDD ( A ) )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Items in original cell A:
      !>           0.0   3.0   1.0   1.0   2.0   5.0   8.0  21.0  13.0  34.0
      !>         Items in cell A after VALIDD (now a set):
      !>           0.0   1.0   2.0   3.0   5.0   8.0  13.0  21.0  34.0
      !>
      !>        Note that if the cell is not big enough to accommodate the
      !>        addition of an item, an error is signaled. In this case, the
      !>        cell is not altered.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>        Improved the documentation of CELL in $Detailed_Input and
      !>        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 09-NOV-2006 (WLT)
      !>
      !>        Corrected typo in $Examples section describing the cell as
      !>        character instead of d.p.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
      module subroutine appndd ( ctx, item, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: item  !! The item to append.
      real(wp), intent(inout) :: cell ( lbcell:* )  !! The cell to which ITEM will be appended.


      integer(ip) :: nwcard

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the cell can accommodate the addition of a
!     new item. If there is room, append the item to the cell and
!     reset the cardinality. If the cell cannot accommodate the
!     addition of a new item, signal an error.
!

      nwcard = cardd ( ctx,cell) + 1

      if (  ( nwcard )  <=  ( sized ( ctx,cell) )  ) then

         cell (nwcard) = item
         call scardd ( ctx, nwcard, cell )

      else

        call ctx%setmsg ( 'The cell cannot accommodate the '          // &
                      'addition of the element *. '                )

        call ctx%errdp  ( '*', item )
        call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )

      end if

      return
      end subroutine appndd

      !> Append an item to an integer cell.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an integer value which is to be appended to CELL.
      !>
      !>     CELL     is an integer SPICE cell to which ITEM will be
      !>              appended.
      !>
      !>## Detailed Output
      !>
      !>     CELL     is the input cell with ITEM appended. ITEM is the last
      !>              member of CELL.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input cell has invalid cardinality, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the input cell has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If the cell is not big enough to accommodate the addition
      !>         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a cell for fifteen elements, add a first element to
      !>        it and then append several more integer numbers. Validate
      !>        the cell into a set and print the result.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM APPNDI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDI
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ = 9  )
      !>
      !>      INTEGER                 SIZE
      !>      PARAMETER             ( SIZE   = 15 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER                 A      ( LBCELL:SIZE )
      !>
      !>      INTEGER                 I
      !>      INTEGER                 ITEMS  ( LISTSZ      )
      !>
      !>C
      !>C     Set the list of integers to be appended to the cell.
      !>C
      !>      DATA                    ITEMS /  3,  1,  1 , 2, 5,
      !>     .                                 8, 21, 13, 34    /
      !>
      !>C
      !>C     Initialize the cell.
      !>C
      !>      CALL SSIZEI ( SIZE, A )
      !>
      !>C
      !>C     Add a single item to the new cell.
      !>C
      !>      CALL APPNDI ( 0, A )
      !>
      !>C
      !>C     Now insert a list of items.
      !>C
      !>      DO I= 1, LISTSZ
      !>
      !>         CALL APPNDI ( ITEMS(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the original contents of cell A.
      !>C
      !>      WRITE(*,*) 'Items in original cell A:'
      !>      WRITE(*,'(15I6)') ( A(I), I = 1, CARDI ( A ) )
      !>
      !>C
      !>C     Validate the set: remove duplicates and sort the
      !>C     elements.
      !>C
      !>      CALL VALIDI ( SIZE, CARDI( A ), A )
      !>
      !>C
      !>C     Output the contents of the set A.
      !>C
      !>      WRITE(*,*) 'Items in cell A after VALIDI (now a set):'
      !>      WRITE(*,'(15I6)') ( A(I), I = 1, CARDI ( A ) )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Items in original cell A:
      !>             0     3     1     1     2     5     8    21    13    34
      !>         Items in cell A after VALIDI (now a set):
      !>             0     1     2     3     5     8    13    21    34
      !>
      !>        Note that if the cell is not big enough to accommodate the
      !>        addition of an item, an error is signaled. In this case, the
      !>        cell is not altered.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Improved the documentation of CELL in $Detailed_Input and
      !>        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 31-JUL-2002 (NJB)
      !>
      !>        Corrected miscellaneous typos in header and in the long
      !>        error message text.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
      module subroutine appndi ( ctx, item, cell )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: item  !! The item to append.
      integer(ip), intent(inout) :: cell ( lbcell:* )  !! The cell to which ITEM will be
                                                       !! appended.


      integer(ip) :: nwcard

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the cell can accommodate the addition of a
!     new item. If there is room, append the item to the cell and
!     reset the cardinality. If the cell cannot accommodate the
!     addition of a new item, signal an error.
!

      nwcard = cardi ( ctx,cell) + 1

      if (  ( nwcard )  <=  ( sizei ( ctx,cell) )  ) then

         cell (nwcard) = item
         call scardi ( ctx, nwcard, cell )

      else

        call ctx%setmsg ( 'The cell cannot accommodate the '          // &
                      'addition of the element *. '                )

        call ctx%errint ( '*', item )
        call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )

      end if

      return
      end subroutine appndi

      !> Copy the contents of a character cell to another cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     COPY     is a cell which contains the same elements as the
      !>              input cell, in the same order. If the size (maximum
      !>              cardinality) of the output cell is smaller than
      !>              the cardinality of the input cell, then only as many
      !>              items as will fit in the output cell are copied,
      !>              and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the output cell in not large enough to hold the elements
      !>         of the input cell, the error SPICE(CELLTOOSMALL) is signaled.
      !>
      !>     2)  If length of the elements of the output cell is less than the
      !>         length of the elements of the input cell, the error
      !>         SPICE(ELEMENTSTOOSHORT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The copy routines (COPYC, COPYD, and COPYI) are used primarily
      !>     to manipulate working cells, since many routines that use cells
      !>     (binary set routines, for instance) do not allow cells to be
      !>     combined or manipulated in place.
      !>
      !>## Examples
      !>
      !>     In the following example, COPYC is used to copy the result
      !>     of the union of two sets (ordered cells) from a temporary
      !>     working set back into the one of the original set.
      !>
      !>           CALL UNIONC ( BODIES, PLANETS, TEMP )
      !>           CALL COPYC  ( TEMP,   BODIES       )
      !>
      !>     If the size of the temporary cell is greater than the size
      !>     of the original set, the function FAILED should be checked to be
      !>     sure that no overflow occurred. If BODIES is at least as
      !>     large as TEMP, no such check is necessary.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 09-JAN-1989 (NJB)
      !>
      !>        Error signaled if output set elements are not long enough.
      !>        Length must be at least max of lengths of input elements.
      !>        Also, calling protocol for EXCESS has been changed. And,
      !>        elements LBCELL through -2 of control area are now copied to
      !>        the output cell.
      module subroutine copyc ( ctx, cell, copy )
      use pepper_kinds, only: ip
      use pepper_strings, only: lastpc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
      character(*), intent(inout) :: copy     ( lbcell:* )  !! New cell.


!     Local variables
!
      integer(ip) :: size
      integer(ip) :: card
      integer(ip) :: moved
      integer(ip) :: i
      logical :: trunc
      integer(ip) :: reqlen

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     We need the cardinality of the input cell, and the size of
!     the output cell.
!
      card = cardc ( ctx, cell )
      size = sizec ( ctx, copy )

!
!     Start moving the elements, one by one. Stop if the output
!     cell fills up.  Copy the control area too, except for the
!     the size and cardinality values.  Truncation indicator
!     starts at .FALSE.
!
      trunc  = .false.
      reqlen = 0

      moved  =  min ( card, size )

      do i = 1, moved
         copy(i) = cell(i)
!
!        Test for truncation:
!
         if ( copy(i) /= cell(i) ) then
            trunc   = .true.
            reqlen  = max ( reqlen, lastpc( cell(i) ) )
         end if

      end do

      do i = lbcell, -2
         copy(i) = cell(i)
!
!        Test for truncation:
!
         if ( copy(i) /= cell(i) ) then
            trunc   = .true.
            reqlen  = max ( reqlen, lastpc( cell(i) ) )
         end if

      end do

!
!     Set the cardinality of the output cell.
!
      call scardc ( ctx, moved, copy )

!
!     We've got an error if the output cell was too small.
!
      if ( size < card ) then
         call excess ( ctx,  card - size,  'cell' )
         call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )
         return
      end if

!
!     We also have an error if the output set elements are not long
!     enough.
!
      if (  trunc  ) then

         call ctx%setmsg ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         call ctx%errint ( '#', len( copy(lbcell) )  )
         call ctx%errint ( '#', reqlen  )
         call ctx%sigerr ( 'SPICE(ELEMENTSTOOSHORT)' )

         return

      end if
      return
      end subroutine copyc

      !> Copy the contents of a double precision cell to another cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     COPY     is a cell which contains the same elements as the
      !>              input cell, in the same order. If the size (maximum
      !>              cardinality) of the output cell is smaller than
      !>              the cardinality of the input cell, then only as many
      !>              items as will fit in the output cell are copied,
      !>              and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the output cell in not large enough to hold the elements
      !>         of the input cell, the error SPICE(CELLTOOSMALL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The copy routines (COPYC, COPYD, and COPYI) are used primarily
      !>     to manipulate working cells, since many routines that use cells
      !>     (binary set routines, for instance) do not allow cells to be
      !>     combined or manipulated in place.
      !>
      !>## Examples
      !>
      !>     In the following example, COPYC is used to copy the result
      !>     of the union of two sets (ordered cells) from a temporary
      !>     working set back into the one of the original set.
      !>
      !>           CALL UNIONC ( BODIES, PLANETS, TEMP )
      !>           CALL COPYC  ( TEMP,   BODIES        )
      !>
      !>     If the size of the temporary cell is greater than the size
      !>     of the original set, the function FAILED should be checked to be
      !>     sure that no overflow occurred. If BODIES is at least as
      !>     large as TEMP, no such check is necessary.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 09-JAN-1989 (NJB)
      !>
      !>        Calling protocol for EXCESS has been changed. Call to SETMSG
      !>        has been removed.
      module subroutine copyd ( ctx, cell, copy )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
      real(wp), intent(inout) :: copy     ( lbcell:* )  !! New cell.


      integer(ip) :: size
      integer(ip) :: card
      integer(ip) :: moved
      integer(ip) :: i

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     We need the cardinality of the input cell, and the size of
!     the output cell.
!
      card = cardd ( ctx, cell )
      size = sized ( ctx, copy )

!
!     Start moving the elements, one by one. Stop if the output
!     cell fills up.
!
      moved = min ( card, size )

      do i = 1, moved
         copy(i) = cell(i)
      end do

!
!     Set the cardinality of the output cell. Report any excess.
!
      call scardd ( ctx, moved, copy )

      if ( card > size ) then

         call excess ( ctx, card - size, 'cell' )
         call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )
         return

      end if

      return
      end subroutine copyd

      !> Copy the contents of an integer cell to another cell.
      !>
      !>## Detailed Input
      !>
      !>     CELL     is a cell.
      !>
      !>## Detailed Output
      !>
      !>     COPY     is a cell which contains the same elements as the
      !>              input cell, in the same order. If the size (maximum
      !>              cardinality) of the output cell is smaller than
      !>              the cardinality of the input cell, then only as many
      !>              items as will fit in the output cell are copied,
      !>              and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the output cell in not large enough to hold the elements
      !>         of the input cell, the error SPICE(CELLTOOSMALL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The copy routines (COPYC, COPYD, and COPYI) are used primarily
      !>     to manipulate working cells, since many routines that use cells
      !>     (binary set routines, for instance) do not allow cells to be
      !>     combined or manipulated in place.
      !>
      !>## Examples
      !>
      !>     In the following example, COPYC is used to copy the result
      !>     of the union of two sets (ordered cells) from a temporary
      !>     working set back into the one of the original set.
      !>
      !>           CALL UNIONC ( BODIES, PLANETS, TEMP )
      !>           CALL COPYC  ( TEMP,   BODIES        )
      !>
      !>     If the size of the temporary cell is greater than the size
      !>     of the original set, the function FAILED should be checked to be
      !>     sure that no overflow occurred. If BODIES is at least as
      !>     large as TEMP, no such check is necessary.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 09-JAN-1989 (NJB)
      !>
      !>        Calling protocol for EXCESS has been changed. Call to SETMSG
      !>        has been removed.
      module subroutine copyi ( ctx, cell, copy )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
      integer(ip), intent(inout) :: copy     ( lbcell:* )  !! New cell.


      integer(ip) :: size
      integer(ip) :: card
      integer(ip) :: moved
      integer(ip) :: i

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     We need the cardinality of the input cell, and the size of
!     the output cell.
!
      card = cardi ( ctx, cell )
      size = sizei ( ctx, copy )

!
!     Start moving the elements, one by one. Stop if the output
!     cell fills up.
!
      moved = min ( card, size )

      do i = 1, moved
         copy(i) = cell(i)
      end do

!
!     Set the cardinality of the output cell. Report any excess.
!
      call scardi ( ctx, moved, copy )

      if ( card > size ) then

         call excess ( ctx, card - size, 'cell' )
         call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )
         return

      end if

      return
      end subroutine copyi

      !> Insert an item into a character set.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which is to be inserted into the specified
      !>              set. ITEM may or may not already be an element of the
      !>              set.
      !>
      !>              If ITEM is longer than the declared maximum length of the
      !>              set's elements, the string will be truncated on the right
      !>              when it is inserted. Trailing blanks in ITEM are not
      !>              significant.
      !>
      !>     A        is a SPICE set.
      !>
      !>              On input, A may or may not contain the input item as an
      !>              element.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, contains the union of the input set and the
      !>              singleton set containing the input item, unless there was
      !>              not sufficient room in the set for the item to be
      !>              included, in which case the set is not changed and an
      !>              error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the insertion of the item into the set causes an excess
      !>         of elements, the error SPICE(SETEXCESS) is signaled.
      !>
      !>     2)  If the item to be inserted has greater length than the string
      !>         length of the elements of the set, the item will be truncated
      !>         on the right when it is inserted. The insertion point of the
      !>         element will be determined by the comparison of the truncated
      !>         item to members of the set. If, after truncation, the item to
      !>         be inserted matches an element already present in the set, no
      !>         insertion occurs.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a set with all the original planets of the Solar
      !>        System and then remove Pluto from that set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INSRTC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDC
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5  )
      !>
      !>      INTEGER                 PNAMSZ
      !>      PARAMETER             ( PNAMSZ   = 7 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 9 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(PNAMSZ)      LIST   ( SETDIM        )
      !>      CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Create the original planets list.
      !>C
      !>      DATA                    LIST  /
      !>     .                'MERCURY', 'VENUS',   'EARTH',
      !>     .                'MARS',    'JUPITER', 'SATURN',
      !>     .                'URANUS',  'NEPTUNE', 'PLUTO'   /
      !>
      !>C
      !>C     Initialize the empty set.
      !>C
      !>      CALL VALIDC ( SETDIM, 0, PLNETS )
      !>
      !>C
      !>C     Insert the list of planets into the set. If the item is
      !>C     an element of the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL INSRTC ( LIST(I), PLNETS )
      !>
      !>      END DO
      !>
      !>C
      !>C     Remove the Pluto from the set. If the Pluto is not an
      !>C     element of the set, the set is not changed.
      !>C
      !>      CALL REMOVC ( 'PLUTO', PLNETS )
      !>
      !>C
      !>C     Output the contents of PLNETS.
      !>C
      !>      WRITE(*,*) 'Planets of the Solar System:'
      !>
      !>      DO I = 1, CARDC ( PLNETS )
      !>
      !>         WRITE(*,*) '   ', PLNETS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planets of the Solar System:
      !>            EARTH
      !>            JUPITER
      !>            MARS
      !>            MERCURY
      !>            NEPTUNE
      !>            SATURN
      !>            URANUS
      !>            VENUS
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Improved the argument ITEM description in $Detailed_Input.
      !>
      !>-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
      !>
      !>        Bug fix: when the item to be inserted would, after
      !>        truncation to the set's string length, match an item
      !>        already in the set, no insertion is performed. Previously
      !>        the truncated string was inserted, corrupting the set.
      !>
      !>        Long error message was updated to include size of
      !>        set into which insertion was attempted.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      module subroutine insrtc ( ctx, item, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: item  !! Item to be inserted.
      character(*), intent(inout) :: a        ( lbcell:* )  !! Insertion set.


      integer(ip) :: card
      integer(ip) :: i
      integer(ip) :: last
      integer(ip) :: size
      integer(ip) :: slen

      logical :: in

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     What are the size and cardinality of the set?
!
      size = sizec ( ctx, a )
      card = cardc ( ctx, a )

!
!     When we insert an item into the set, any trailing characters
!     that don't fit are truncated.  So in deciding where to insert
!     the item, we ignore any characters that won't remain after
!     insertion.
!
!     We're going to consider only the initial substring of ITEM
!     whose length doesn't exceed the string length of the set's
!     members.
!
      slen = min (  len(item),  len( a(1) )  )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      last = lstlec ( item(:slen),  card,  a(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      if ( last > 0 ) then
         in  =  a(last) == item(:slen)
      else
         in  =  .false.
      end if

      if ( .not. in ) then
!
!        If there is room in the set for the new element, then move
!        the succeeding elements back to make room. And update the
!        cardinality for future reference.
!
         if ( card < size ) then

            do i = card, last+1, -1
               a(i+1) = a(i)
            end do

            a(last+1) = item(:slen)

            call scardc ( ctx, card+1, a )

         else

            call ctx%setmsg ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            call ctx%errint ( '#', size                             )
            call ctx%sigerr ( 'SPICE(SETEXCESS)'                    )

         end if

      end if

      return
      end subroutine insrtc

      !> Insert an item into a double precision set.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which is to be inserted into the specified
      !>              set. ITEM may or may not already be an element of the
      !>              set.
      !>
      !>     A        is a SPICE set.
      !>
      !>              On input, A may or may not contain the input item as an
      !>              element.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, contains the union of the input set and the
      !>              singleton set containing the input item, unless there was
      !>              not sufficient room in the set for the item to be
      !>              included, in which case the set is not changed and an
      !>              error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the insertion of the element into the set causes an excess
      !>         of elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create an double precision set for ten elements, insert items
      !>        to it and then remove the even values.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INSRTD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDD
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 10  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        A      ( LBCELL:SETDIM )
      !>      DOUBLE PRECISION        EVEN   ( SETDIM        )
      !>      DOUBLE PRECISION        ITEMS  ( SETDIM        )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Create a list of items and even numbers.
      !>C
      !>      DATA                    EVEN  /
      !>     .                      0.D0,  2.D0,  4.D0,  6.D0,  8.D0,
      !>     .                     10.D0, 12.D0, 14.D0, 16.D0, 18.D0  /
      !>
      !>      DATA                    ITEMS /
      !>     .                      0.D0,  1.D0,  1.D0,  2.D0,  3.D0,
      !>     .                      5.D0,  8.D0, 10.D0, 13.D0, 21.D0  /
      !>
      !>C
      !>C     Initialize the empty set.
      !>C
      !>      CALL VALIDD ( SETDIM, 0, A )
      !>
      !>C
      !>C     Insert the list of double precision numbers into the
      !>C     set. If the item is an element of the set, the set is
      !>C     not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL INSRTD ( ITEMS(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the original contents of set A.
      !>C
      !>      WRITE(*,*) 'Items in original set A:'
      !>      WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Remove the even values. If the item is not an element of
      !>C     the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL REMOVD ( EVEN(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the contents of A.
      !>C
      !>      WRITE(*,*) 'Odd numbers in set A:'
      !>      WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Items in original set A:
      !>           0.0   1.0   2.0   3.0   5.0   8.0  10.0  13.0  21.0
      !>
      !>         Odd numbers in set A:
      !>           1.0   3.0   5.0  13.0  21.0
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
      !>
      !>        Code was modified slightly to keep logical structure parallel
      !>        to that of INSRTC.
      !>
      !>        Long error message was updated to include size of
      !>        set into which insertion was attempted.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module subroutine insrtd ( ctx, item, a )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstled
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: item  !! Item to be inserted.
      real(wp), intent(inout) :: a     ( lbcell:* )  !! Insertion set.


      integer(ip) :: card
      integer(ip) :: i
      integer(ip) :: last
      integer(ip) :: size

      logical :: in

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     What are the size and cardinality of the set?
!
      size = sized ( ctx, a )
      card = cardd ( ctx, a )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      last = lstled ( item, card, a(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      if ( last > 0 ) then
         in  =  a(last) == item
      else
         in  =  .false.
      end if

      if ( .not. in ) then
!
!        If there is room in the set for the new element, then move
!        the succeeding elements back to make room. And update the
!        cardinality for future reference.
!
         if ( card < size ) then

            do i = card,  last+1,  -1
               a(i+1) = a(i)
            end do

            a(last+1) = item

            call scardd ( ctx, card+1,  a )

         else

            call ctx%setmsg ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            call ctx%errint ( '#', size                             )
            call ctx%sigerr ( 'SPICE(SETEXCESS)'                    )

         end if

      end if

      return
      end subroutine insrtd

      !> Insert an item into an integer set.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which is to be inserted into the specified
      !>              set. ITEM may or may not already be an element of the
      !>              set.
      !>
      !>     A        is a SPICE set.
      !>
      !>              On input, A may or may not contain the input item as an
      !>              element.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, contains the union of the input set and the
      !>              singleton set containing the input item, unless there was
      !>              not sufficient room in the set for the item to be
      !>              included, in which case the set is not changed and an
      !>              error is returned.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the insertion of the element into the set causes an excess
      !>         of elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create an integer set for ten elements, insert items
      !>        to it and then remove the even values.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INSRTI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDI
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 10  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER                 A      ( LBCELL:SETDIM )
      !>      INTEGER                 EVEN   ( SETDIM        )
      !>      INTEGER                 I
      !>      INTEGER                 ITEMS  ( SETDIM        )
      !>
      !>C
      !>C     Create a list of items and even numbers.
      !>C
      !>      DATA                    EVEN  /  0,  2,  4,  6,  8,
      !>     .                                10, 12, 14, 16, 18  /
      !>
      !>      DATA                    ITEMS /  0,  1,  1,  2,  3,
      !>     .                                 5,  8, 10, 13, 21  /
      !>
      !>C
      !>C     Initialize the empty set.
      !>C
      !>      CALL VALIDI ( SETDIM, 0, A )
      !>
      !>C
      !>C     Insert the list of integers into the set. If the item is
      !>C     an element of the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL INSRTI ( ITEMS(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the original contents of set A.
      !>C
      !>      WRITE(*,*) 'Items in original set A:'
      !>      WRITE(*,'(10I6)') ( A(I), I = 1, CARDI ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Remove the even values. If the item is not an element of
      !>C     the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL REMOVI ( EVEN(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the contents of A.
      !>C
      !>      WRITE(*,*) 'Odd numbers in set A:'
      !>      WRITE(*,'(10I6)') ( A(I), I = 1, CARDI ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Items in original set A:
      !>             0     1     2     3     5     8    10    13    21
      !>
      !>         Odd numbers in set A:
      !>             1     3     5    13    21
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
      !>
      !>        Code was modified slightly to keep logical structure parallel
      !>        to that of INSRTC.
      !>
      !>        Long error message was updated to include size of
      !>        set into which insertion was attempted.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      module subroutine insrti ( ctx, item, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlei
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: item  !! Item to be inserted.
      integer(ip), intent(inout) :: a        ( lbcell:* )  !! Insertion set.


      integer(ip) :: size
      integer(ip) :: card
      integer(ip) :: last
      logical :: in
      integer(ip) :: i

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     What are the size and cardinality of the set?
!
      size = sizei ( ctx, a )
      card = cardi ( ctx, a )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      last = lstlei ( item, card, a(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      if ( last > 0 ) then
         in  =  a(last) == item
      else
         in  =  .false.
      end if

      if ( .not. in ) then
!
!        If there is room in the set for the new element, then move
!        the succeeding elements back to make room. And update the
!        cardinality for future reference.
!
         if ( card < size ) then

            do i = card, last+1, -1
               a(i+1) = a(i)
            end do

            a(last+1) = item

            call scardi ( ctx, card+1, a )

         else

            call ctx%setmsg ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            call ctx%errint ( '#', size                             )
            call ctx%sigerr ( 'SPICE(SETEXCESS)'                    )

         end if

      end if

      return
      end subroutine insrti

      !> Remove an item from a character set.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which is to be removed from the specified set.
      !>              ITEM may or may not already be an element of the set.
      !>              Trailing blanks in ITEM are not significant.
      !>
      !>     A        is a SPICE set.
      !>
      !>              On input, A may or may not contain the input item as an
      !>              element.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, contains the difference of the input set and
      !>              the input item. If the item is not an element of the set,
      !>              the set is not changed.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input set A has invalid cardinality, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the input set A has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  The data values in set A must be monotone strictly increasing.
      !>         This is not checked. If this condition is not met, the results
      !>         are unpredictable.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create a set with all the original planets of the Solar
      !>        System and then remove Pluto from that set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REMOVC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDC
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5  )
      !>
      !>      INTEGER                 PNAMSZ
      !>      PARAMETER             ( PNAMSZ   = 7 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 9 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(PNAMSZ)      LIST   ( SETDIM        )
      !>      CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Create the original planets list.
      !>C
      !>      DATA                    LIST  /
      !>     .                'MERCURY', 'VENUS',   'EARTH',
      !>     .                'MARS',    'JUPITER', 'SATURN',
      !>     .                'URANUS',  'NEPTUNE', 'PLUTO'   /
      !>
      !>C
      !>C     Initialize the empty set.
      !>C
      !>      CALL VALIDC ( SETDIM, 0, PLNETS )
      !>
      !>C
      !>C     Insert the list of planets into the set. If the item is
      !>C     an element of the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL INSRTC ( LIST(I), PLNETS )
      !>
      !>      END DO
      !>
      !>C
      !>C     Remove the Pluto from the set. If the Pluto is not an
      !>C     element of the set, the set is not changed.
      !>C
      !>      CALL REMOVC ( 'PLUTO', PLNETS )
      !>
      !>C
      !>C     Output the contents of PLNETS.
      !>C
      !>      WRITE(*,*) 'Planets of the Solar System:'
      !>
      !>      DO I = 1, CARDC ( PLNETS )
      !>
      !>         WRITE(*,*) '   ', PLNETS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planets of the Solar System:
      !>            EARTH
      !>            JUPITER
      !>            MARS
      !>            MERCURY
      !>            NEPTUNE
      !>            SATURN
      !>            URANUS
      !>            VENUS
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Extended the $Exceptions section.
      !>
      !>        Updated description of argument ITEM to indicate that trailing
      !>        blanks are not significant
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
      module subroutine removc ( ctx, item, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: item  !! Item to be removed.
      character(*), intent(inout) :: a       ( lbcell:* )  !! Removal set.


!     Local variables
!
      integer(ip) :: card
      integer(ip) :: loc
      logical :: in
      integer(ip) :: i

!
!     Standard error handling:
!
      if ( ctx%failed() ) return
!
!     What is the cardinality of the set?
!
      card = cardc ( ctx, a )

!
!     Determine the location (if any) of the item within the set.
!
      loc = bsrchc ( item, card, a(1) )

!
!     Is the item in the set? If so, it needs to be removed.
!
      in = ( loc > 0 )

      if ( in ) then

!
!        Move succeeding elements forward to take up the slack left
!        by the departing element. And update the cardinality for
!        future reference.
!
         do i = loc, card-1
            a(i) = a(i+1)
         end do

         call scardc ( ctx, card-1, a )

      end if

      return
      end subroutine removc

      !> Remove an item from a double precision set.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which is to be removed from the specified set.
      !>              ITEM may or may not already be an element of the set.
      !>
      !>     A        is a set.
      !>
      !>              On input, A may or may not contain the input item as an
      !>              element.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, contains the difference of the input set and
      !>              the input item. If the item is not an element of the set,
      !>              the set is not changed.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input set A has invalid cardinality, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the input set A has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  The data values in set A must be monotone strictly increasing.
      !>         This is not checked. If this condition is not met, the results
      !>         are unpredictable.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create an double precision set for ten elements, insert items
      !>        to it and then remove the even values.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REMOVD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDD
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 10  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        A      ( LBCELL:SETDIM )
      !>      DOUBLE PRECISION        EVEN   ( SETDIM        )
      !>      DOUBLE PRECISION        ITEMS  ( SETDIM        )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Create a list of items and even numbers.
      !>C
      !>      DATA                    EVEN  /
      !>     .                      0.D0,  2.D0,  4.D0,  6.D0,  8.D0,
      !>     .                     10.D0, 12.D0, 14.D0, 16.D0, 18.D0  /
      !>
      !>      DATA                    ITEMS /
      !>     .                      0.D0,  1.D0,  1.D0,  2.D0,  3.D0,
      !>     .                      5.D0,  8.D0, 10.D0, 13.D0, 21.D0  /
      !>
      !>C
      !>C     Initialize the empty set.
      !>C
      !>      CALL VALIDD ( SETDIM, 0, A )
      !>
      !>C
      !>C     Insert the list of double precision numbers into the
      !>C     set. If the item is an element of the set, the set is
      !>C     not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL INSRTD ( ITEMS(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the original contents of set A.
      !>C
      !>      WRITE(*,*) 'Items in original set A:'
      !>      WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Remove the even values. If the item is not an element of
      !>C     the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL REMOVD ( EVEN(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the contents of A.
      !>C
      !>      WRITE(*,*) 'Odd numbers in set A:'
      !>      WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
      !>      WRITE(*,*)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Items in original set A:
      !>           0.0   1.0   2.0   3.0   5.0   8.0  10.0  13.0  21.0
      !>
      !>         Odd numbers in set A:
      !>           1.0   3.0   5.0  13.0  21.0
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-MAR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Extended the $Exceptions section.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module subroutine removd ( ctx, item, a )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: item  !! Item to be removed.
      real(wp), intent(inout) :: a        ( lbcell:* )  !! Removal set.


!     Local variables
!
      integer(ip) :: card
      integer(ip) :: loc
      logical :: in
      integer(ip) :: i

!
!     Standard error handling:
!
      if ( ctx%failed() ) return
!
!     What is the cardinality of the set?
!
      card = cardd ( ctx, a )

!
!     Determine the location (if any) of the item within the set.
!
      loc = bsrchd ( item, card, a(1) )

!
!     Is the item in the set? If so, it needs to be removed.
!
      in = ( loc > 0 )

      if ( in ) then

!
!        Move succeeding elements forward to take up the slack left
!        by the departing element. And update the cardinality for
!        future reference.
!
         do i = loc, card-1
            a(i) = a(i+1)
         end do

         call scardd ( ctx, card-1, a )

      end if

      return
      end subroutine removd

      !> Remove an item from an integer set.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which is to be removed from the specified set.
      !>              ITEM may or may not already be an element of the set.
      !>
      !>     A        is a SPICE set.
      !>
      !>              On input, A may or may not contain the input item as an
      !>              element.
      !>
      !>## Detailed Output
      !>
      !>     A        on output, contains the difference of the input set and
      !>              the input item. If the item is not an element of the set,
      !>              the set is not changed.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input set A has invalid cardinality, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the input set A has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  The data values in set A must be monotone strictly increasing.
      !>         This is not checked. If this condition is not met, the results
      !>         are unpredictable.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create an integer set for ten elements, insert items
      !>        to it and then remove the even values.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REMOVI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 CARDI
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 10  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER                 A      ( LBCELL:SETDIM )
      !>      INTEGER                 EVEN   ( SETDIM        )
      !>      INTEGER                 I
      !>      INTEGER                 ITEMS  ( SETDIM        )
      !>
      !>C
      !>C     Create a list of items and even numbers.
      !>C
      !>      DATA                    EVEN  /  0,  2,  4,  6,  8,
      !>     .                                10, 12, 14, 16, 18  /
      !>
      !>      DATA                    ITEMS /  0,  1,  1,  2,  3,
      !>     .                                 5,  8, 10, 13, 21  /
      !>
      !>C
      !>C     Initialize the empty set.
      !>C
      !>      CALL VALIDI ( SETDIM, 0, A )
      !>
      !>C
      !>C     Insert the list of integers into the set. If the item is
      !>C     an element of the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL INSRTI ( ITEMS(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the original contents of set A.
      !>C
      !>      WRITE(*,*) 'Items in original set A:'
      !>      WRITE(*,'(10I6)') ( A(I), I=1, CARDI ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Remove the even values. If the item is not an element of
      !>C     the set, the set is not changed.
      !>C
      !>      DO I = 1, SETDIM
      !>
      !>         CALL REMOVI ( EVEN(I), A )
      !>
      !>      END DO
      !>
      !>C
      !>C     Output the contents of A.
      !>C
      !>      WRITE(*,*) 'Odd numbers in set A:'
      !>      WRITE(*,'(10I6)') ( A(I), I=1, CARDI ( A ) )
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Items in original set A:
      !>             0     1     2     3     5     8    10    13    21
      !>
      !>         Odd numbers in set A:
      !>             1     3     5    13    21
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Extended the $Exceptions section.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module subroutine removi ( ctx, item, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: item  !! Item to be removed.
      integer(ip), intent(inout) :: a        ( lbcell:* )  !! Removal set.


      integer(ip) :: card
      integer(ip) :: loc
      logical :: in
      integer(ip) :: i

!
!     Standard error handling:
!
      if ( ctx%failed() ) return
!
!     What is the cardinality of the set?
!
      card = cardi ( ctx, a )

!
!     Determine the location (if any) of the item within the set.
!
      loc = bsrchi ( item, card, a(1) )

!
!     Is the item in the set? If so, it needs to be removed.
!
      in = ( loc > 0 )

      if ( in ) then

!
!        Move succeeding elements forward to take up the slack left
!        by the departing element. And update the cardinality for
!        future reference.
!
         do i = loc, card-1
            a(i) = a(i+1)
         end do

         call scardi ( ctx, card-1, a )

      end if

      return
      end subroutine removi

      !> Determine whether an item is an element of a character set.
      !>
      !> The function returns .TRUE. if ITEM is an element of set A.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which may or may not be an element of the
      !>              input set. Trailing blanks in ITEM are not significant.
      !>
      !>     A        is a SPICE set. Trailing blanks in the members of A
      !>              are not significant.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if ITEM is a member of the set A,
      !>     and returns .FALSE. otherwise.
      !>
      !>     The comparison between ITEM and members of A is case-sensitive.
      !>     Trailing blanks are ignored.
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
      !>     The LOGICAL functions ELEMC, ELEMD and ELEMI correspond to the
      !>     set operator IN in the Pascal language.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Check if the elements of a list of body names belong to the
      !>        Solar System planets set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM ELEMC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL                 ELEMC
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ   = 6   )
      !>
      !>      INTEGER                 PNAMSZ
      !>      PARAMETER             ( PNAMSZ   = 7 )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 9   )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(PNAMSZ)      ITEMS  ( LISTSZ        )
      !>      CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Create the original planets set and a list of body
      !>C     names.
      !>C
      !>      DATA                  ( PLNETS(I), I=1,SETDIM)  /
      !>     .                       'MERCURY', 'VENUS',   'EARTH',
      !>     .                       'MARS',    'JUPITER', 'SATURN',
      !>     .                       'URANUS',  'NEPTUNE', 'PLUTO'  /
      !>
      !>      DATA                    ITEMS /
      !>     .                            'EARTH', 'APOLLO', 'MARS',
      !>     .                            'PLUTO', 'VENUS', 'CERES' /
      !>
      !>C
      !>C     Validate the set: Initialize the non-empty set, remove
      !>C     duplicates and sort the elements.
      !>C
      !>      CALL VALIDC ( SETDIM, SETDIM, PLNETS )
      !>
      !>C
      !>C     Check if the items in the list belong to the set.
      !>C
      !>      DO I = 1, LISTSZ
      !>
      !>         IF ( ELEMC ( ITEMS(I), PLNETS ) ) THEN
      !>
      !>            WRITE(*,*) 'Item ', ITEMS(I), ' is in the set.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'Item ', ITEMS(I),
      !>     .                 ' is NOT in the set.'
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
      !>         Item EARTH   is in the set.
      !>         Item APOLLO  is NOT in the set.
      !>         Item MARS    is in the set.
      !>         Item PLUTO   is in the set.
      !>         Item VENUS   is in the set.
      !>         Item CERES   is NOT in the set.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Updated description of arguments ITEM and A to indicate that
      !>        trailing blanks are not significant and extended
      !>        $Detailed_Output to indicate that comparison is case-sensitive.
      !>
      !>        Removed unnecessary $Revisions section.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module logical function elemc ( ctx, item, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: item  !! Item to be tested.
      character(*), intent(in) :: a      ( lbcell:* )  !! Set to be tested.


!
!     Standard error handling:
!
      if ( ctx%failed() ) then
         elemc = .false.
         return
      end if
!
!     Just a binary search.
!
      elemc = ( bsrchc ( item, cardc ( ctx, a ), a(1) ) /= 0 )

      return
      end function elemc

      !> Determine whether an item is an element of a double
      !> precision set.
      !>
      !> The function returns .TRUE. if ITEM is an element of set A.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which may or may not be an element of the
      !>              input set.
      !>
      !>     A        is a SPICE set.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if ITEM is a member of the set A, and
      !>     returns .FALSE. otherwise.
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
      !>     The LOGICAL functions ELEMC, ELEMD and ELEMI correspond to the
      !>     set operator IN in the Pascal language.
      !>
      !>     This routine uses a binary search to check for the presence in
      !>     the set of the specified ITEM.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Check if the elements of a list of double precision numbers
      !>        belong to a given double precision set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM ELEMD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL                 ELEMD
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ   = 6   )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 8   )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        A      ( LBCELL:SETDIM )
      !>      DOUBLE PRECISION        ITEMS  ( LISTSZ        )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Set the values of the set and the list of double
      !>C     precision numbers.
      !>C
      !>      DATA                  ( A(I), I=1,SETDIM)  /
      !>     .                            -1.D0, 0.D0, 1.D0,  1.D0,
      !>     .                             3.D0, 5.D0, 0.D0, -3.D0 /
      !>
      !>      DATA                    ITEMS /    6.D0, -1.D0, 0.D0,
      !>     .                                   2.D0, 3.D0, -2.D0 /
      !>
      !>C
      !>C     Validate the set: Initialize the non-empty set, remove
      !>C     duplicates and sort the elements.
      !>C
      !>      CALL VALIDD ( SETDIM, SETDIM, A )
      !>
      !>C
      !>C     Check if the items in the list belong to the set.
      !>C
      !>      DO I = 1, LISTSZ
      !>
      !>         IF ( ELEMD ( ITEMS(I), A ) ) THEN
      !>
      !>            WRITE(*,'(A,F5.1,A)') 'Item ', ITEMS(I),
      !>     .                          ' is in the set.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A,F5.1,A)') 'Item ', ITEMS(I),
      !>     .                            ' is NOT in the set.'
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
      !>        Item   6.0 is NOT in the set.
      !>        Item  -1.0 is in the set.
      !>        Item   0.0 is in the set.
      !>        Item   2.0 is NOT in the set.
      !>        Item   3.0 is in the set.
      !>        Item  -2.0 is NOT in the set.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Removed unnecessary $Revisions section.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module logical function elemd ( ctx, item, a )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: item  !! Item to be tested.
      real(wp), intent(in) :: a      ( lbcell:* )  !! Set to be tested.


!
!     Standard error handling:
!
      if ( ctx%failed() ) then
         elemd = .false.
         return
      end if

!
!     Just a binary search.
!
      elemd = ( bsrchd ( item, cardd ( ctx, a ), a(1) ) /= 0 )

      return
      end function elemd

      !> Determine whether an item is an element of an integer set.
      !>
      !> The function returns .TRUE. if ITEM is an element of set A.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an item which may or may not be an element of
      !>              the input set.
      !>
      !>     A        is a SPICE set.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if ITEM is a member of the set A,
      !>     and returns .FALSE. otherwise.
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
      !>     The LOGICAL functions ELEMC, ELEMD and ELEMI correspond to the
      !>     set operator IN in the Pascal language.
      !>
      !>     This routine uses a binary search to check for the presence in
      !>     the set of the specified ITEM.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Check if the elements of a list of integers belong to a given
      !>        integer set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM ELEMI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL                 ELEMI
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 LBCELL
      !>      PARAMETER             ( LBCELL = -5 )
      !>
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ   = 6   )
      !>
      !>      INTEGER                 SETDIM
      !>      PARAMETER             ( SETDIM   = 8   )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER                 A      ( LBCELL:SETDIM )
      !>
      !>      INTEGER                 I
      !>      INTEGER                 ITEMS  ( LISTSZ        )
      !>
      !>C
      !>C     Set the values of the set and the list of integers.
      !>C
      !>      DATA                  ( A(I), I=1,SETDIM)  /
      !>     .                           -1, 0, 1, 1, 3, 5, 0, -3 /
      !>
      !>      DATA                    ITEMS /  6, -1, 0, 2, 3, -2 /
      !>
      !>C
      !>C     Validate the set: Initialize the non-empty set, remove
      !>C     duplicates and sort the elements.
      !>C
      !>      CALL VALIDI ( SETDIM, SETDIM, A )
      !>
      !>C
      !>C     Check if the items in the list belong to the set.
      !>C
      !>      DO I = 1, LISTSZ
      !>
      !>         IF ( ELEMI ( ITEMS(I), A ) ) THEN
      !>
      !>            WRITE(*,'(A,I4,A)') 'Item ', ITEMS(I),
      !>     .                          ' is in the set.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A,I4,A)') 'Item ', ITEMS(I),
      !>     .                          ' is NOT in the set.'
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
      !>        Item    6 is NOT in the set.
      !>        Item   -1 is in the set.
      !>        Item    0 is in the set.
      !>        Item    2 is NOT in the set.
      !>        Item    3 is in the set.
      !>        Item   -2 is NOT in the set.
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Removed unnecessary $Revisions section.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module logical function elemi ( ctx, item, a )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: item  !! Item to be tested.
      integer(ip), intent(in) :: a      ( lbcell:* )  !! Set to be tested.


!
!     Standard error handling:
!
      if ( ctx%failed() ) then
         elemi = .false.
         return
      end if
!
!     Just a binary search.
!
      elemi = ( bsrchi ( item, cardi ( ctx, a ), a(1) ) /= 0 )

      return
      end function elemi

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> The function returns the ordinal position of ITEM within the SET.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is a string to be located within a character set.
      !>              Trailing blanks are not significant in the comparison.
      !>
      !>     SET      is a properly validated SPICE set that is to be
      !>              searched for the occurrence of ITEM. Trailing blanks are
      !>              not significant in the comparison.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the ordinal position of ITEM within SET.
      !>     Ordinal positions range from 1 to N, where N is the cardinality
      !>     of the set.
      !>
      !>     If ITEM is not an element of SET, the function returns 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input set has invalid cardinality, an error is signaled
      !>         by a routine in the call tree of this routine. ORDC returns an
      !>         unspecified value in this case.
      !>
      !>     2)  If the input set has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine. ORDC returns an
      !>         unspecified value in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A natural ordering can be imposed upon the elements of any
      !>     SPICE set, be it INTEGER, CHARACTER or DOUBLE PRECISION. For
      !>     character strings the ASCII collating sequence serves as the
      !>     ordering relation, for DOUBLE PRECISION and INTEGER variables
      !>     the arithmetic ordering is used.
      !>
      !>     Given any element of a set, its location within this ordered
      !>     sequence of elements is called its ordinal position within
      !>     the set.
      !>
      !>     For illustrative purposes suppose that SET represents the set
      !>
      !>              { 8, 1, 2, 9, 7, 4, 10 }
      !>
      !>     The ordinal position of:     8 is 5
      !>                                  1 is 1
      !>                                  2 is 2
      !>                                  9 is 6
      !>                                  7 is 4
      !>                                  4 is 3
      !>                                 10 is 7
      !>
      !>     Given an item of the SET, this routine returns its ordinal
      !>     position. If the item is not in the set, this function returns
      !>     a value of 0.
      !>
      !>## Examples
      !>
      !>     Suppose that you wished to find the relative position of a value
      !>     in a large list of values stored within an array. Say we want
      !>     to know the relative position of item I of ARRAY withing the
      !>     set of values represented in ARRAY.
      !>
      !>     The following sequence of subroutine calls would allow you
      !>     determine the relative position of the value ARRAY(I).
      !>
      !>        INTEGER               N
      !>        PARAMETER           ( N = something useful )
      !>
      !>        CHARACTER*(*)         ARRAY (         N )
      !>        CHARACTER*(*)         SET   ( LBCELL: N )
      !>        INTEGER               I
      !>
      !>        INTEGER               NVALID
      !>        INTEGER               POSITION
      !>
      !>     set the value of NVALID to be the number of valid elements in the
      !>     array ARRAY
      !>
      !>        CALL MOVEC  ( ARRAY, N,      SET(1) )
      !>        CALL VALIDC ( N,     NVALID, SET    )
      !>
      !>        POSITION = ORDC ( ARRAY(I), SET )
      !>
      !>     POSITION now contains the ordinal position of ARRAY(I) within the
      !>     values represented in the array.
      !>
      !>## Restrictions
      !>
      !>     1)  SET must be a validated or empty set.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Extended
      !>        $Detailed_Input, $Detailed_Output and $Exceptions sections.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module integer(ip) function ordc ( ctx, item, set )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: item  !! An item to locate within a set.
      character(*), intent(in) :: set ( lbcell:* )  !! A set to search for a given item.


!
!     Standard error handling:
!
      if ( ctx%failed() ) then
         ordc = 0
         return
      end if

!
!     Given the structure of sets, there's not much to do.
!
      ordc = bsrchc ( item, cardc( ctx,set), set(1) )

      return
      end function ordc

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> The function returns the ordinal position of ITEM within the SET.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is a double precision value to be located within a set.
      !>
      !>     SET      is a properly validated SPICE set that is to be
      !>              searched for the occurrence of ITEM.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the ordinal position of ITEM within SET.
      !>     Ordinal positions range from 1 to N, where N is the cardinality of
      !>     the set.
      !>
      !>     If ITEM is not an element of SET, the function returns 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input set has invalid cardinality, an error is signaled
      !>         by a routine in the call tree of this routine. ORDD returns an
      !>         unspecified value in this case.
      !>
      !>     2)  If the input set has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine. ORDD returns an
      !>         unspecified value in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A natural ordering can be imposed upon the elements of any
      !>     SPICE set, be it INTEGER, CHARACTER or DOUBLE PRECISION. For
      !>     character strings the ASCII collating sequence serves as the
      !>     ordering relation, for DOUBLE PRECISION and INTEGER variables
      !>     the arithmetic ordering is used.
      !>
      !>     Given any element of a set, its location within this ordered
      !>     sequence of elements is called its ordinal position within
      !>     the set.
      !>
      !>     For illustrative purposes suppose that SET represents the set
      !>
      !>              { 8, 1, 2, 9, 7, 4, 10 }
      !>
      !>     The ordinal position of:     8 is 5
      !>                                  1 is 1
      !>                                  2 is 2
      !>                                  9 is 6
      !>                                  7 is 4
      !>                                  4 is 3
      !>                                 10 is 7
      !>
      !>     Given an item of the SET, this routine returns its ordinal
      !>     position. If the item is not in the set, this function returns
      !>     a value of 0.
      !>
      !>## Examples
      !>
      !>     Suppose that you wished to find the relative position of a value
      !>     in a large list of values stored within an array. Say we want
      !>     to know the relative position of item I of ARRAY withing the
      !>     set of values represented in ARRAY.
      !>
      !>     The following sequence of subroutine calls would allow you
      !>     determine the relative position of the value ARRAY(I).
      !>
      !>        INTEGER               N
      !>        PARAMETER           ( N = something useful )
      !>
      !>        DOUBLE PRECISION      ARRAY (         N )
      !>        DOUBLE PRECISION      SET   ( LBCELL: N )
      !>        INTEGER               I
      !>
      !>        INTEGER               NVALID
      !>        INTEGER               POSITION
      !>
      !>     set the value of NVALID to be the number of valid elements in the
      !>     array ARRAY
      !>
      !>        CALL MOVED  ( ARRAY, N,      SET(1) )
      !>        CALL VALIDD ( N,     NVALID, SET    )
      !>
      !>        POSITION = ORDD ( ARRAY(I), SET )
      !>
      !>     POSITION now contains the ordinal position of ARRAY(I) within the
      !>     values represented in the array.
      !>
      !>## Restrictions
      !>
      !>     1)  SET must be a validated or empty set.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Extended
      !>        $Detailed_Output and $Exceptions section.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module integer(ip) function ordd ( ctx, item, set )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: item  !! An item to locate within a set.
      real(wp), intent(in) :: set ( lbcell:* )  !! A set to search for a given item.


!
!     Standard error handling:
!
      if ( ctx%failed() ) then
         ordd = 0
         return
      end if

!
!     Given the structure of sets, there's not much to do.
!
      ordd = bsrchd ( item, cardd( ctx,set), set(1) )

      return
      end function ordd

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> The function returns the ordinal position of ITEM within the SET.
      !>
      !>## Detailed Input
      !>
      !>     ITEM     is an integer value to be located within a set.
      !>
      !>     SET      is a properly validated SPICE set that is to be
      !>              searched for the occurrence of ITEM.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the ordinal position of ITEM within SET.
      !>     Ordinal positions range from 1 to N, where N is the cardinality of
      !>     the set.
      !>
      !>     If ITEM is not an element of SET, the function return 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input set has invalid cardinality, an error is signaled
      !>         by a routine in the call tree of this routine. ORDI returns an
      !>         unspecified value in this case.
      !>
      !>     2)  If the input set has invalid size, an error is signaled by a
      !>         routine in the call tree of this routine. ORDI returns an
      !>         unspecified value in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A natural ordering can be imposed upon the elements of any
      !>     SPICE set, be it INTEGER, CHARACTER or DOUBLE PRECISION. For
      !>     character strings the ASCII collating sequence serves as the
      !>     ordering relation, for DOUBLE PRECISION and INTEGER variables
      !>     the arithmetic ordering is used.
      !>
      !>     Given any element of a set, its location within this ordered
      !>     sequence of elements is called its ordinal position within
      !>     the set.
      !>
      !>     For illustrative purposes suppose that SET represents the set
      !>
      !>              { 8, 1, 2, 9, 7, 4, 10 }
      !>
      !>     The ordinal position of:     8 is 5
      !>                                  1 is 1
      !>                                  2 is 2
      !>                                  9 is 6
      !>                                  7 is 4
      !>                                  4 is 3
      !>                                 10 is 7
      !>
      !>     Given an item of the SET, this routine returns its ordinal
      !>     position. If the item is not in the set, this function returns
      !>     a value of 0.
      !>
      !>## Examples
      !>
      !>     Suppose that you wished to find the relative position of a value
      !>     in a large list of values stored within an array. Say we want
      !>     to know the relative position of item I of ARRAY withing the
      !>     set of values represented in ARRAY.
      !>
      !>     The following sequence of subroutine calls would allow you
      !>     determine the relative position of the value ARRAY(I).
      !>
      !>        INTEGER               N
      !>        PARAMETER           ( N = something useful )
      !>
      !>        INTEGER               ARRAY (         N )
      !>        INTEGER               SET   ( LBCELL: N )
      !>        INTEGER               I
      !>
      !>        INTEGER               NVALID
      !>        INTEGER               POSITION
      !>
      !>     set the value of NVALID to be the number of valid elements in the
      !>     array ARRAY
      !>
      !>        CALL MOVEI  ( ARRAY, N,      SET(1) )
      !>        CALL VALIDI ( N,     NVALID, SET    )
      !>
      !>        POSITION = ORDI ( ARRAY(I), SET )
      !>
      !>     POSITION now contains the ordinal position of ARRAY(I) within the
      !>     values represented in the array.
      !>
      !>## Restrictions
      !>
      !>     1)  SET must be a validated or empty set.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Extended
      !>        $Detailed_Output and $Exceptions section.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      module integer(ip) function ordi ( ctx, item, set )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: item  !! An item to locate within a set.
      integer(ip), intent(in) :: set ( lbcell:* )  !! A set to search for a given item.


!     Standard error handling:
!
      if ( ctx%failed() ) then
         ordi = 0
         return
      end if
!
!     Given the structure of sets, there's not much to do.
!
      ordi = bsrchi ( item, cardi( ctx,set), set(1) )

      return
      end function ordi

      !> Compare two character sets, as indicated by a relational operator.
      !>
      !> The function returns the result of the comparison: A (OP) B.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     OP       is a comparison operator, indicating the way in
      !>              which the input sets are to be compared. OP may
      !>              be any of the following:
      !>
      !>                  Operator             Meaning
      !>                  --------  -------------------------------------
      !>                    '='     A = B is .TRUE. if A and B are equal
      !>                            (contain the same elements).
      !>
      !>                    '<>'    A <> B is .TRUE. if A and B are not
      !>                            equal.
      !>
      !>                    '<='    A <= B is .TRUE. if A is a subset of B.
      !>
      !>                    '<'     A < B is .TRUE. if A is a proper subset
      !>                            of B.
      !>
      !>                    '>='    A >= B is .TRUE. if B is a subset of A.
      !>
      !>                    '>'     A > B is .TRUE. if B is a proper subset
      !>                            of A.
      !>
      !>                    '&'     A & B is .TRUE. if A and B have one or
      !>                            more elements in common. (The
      !>                            intersection of the two sets in
      !>                            non-empty.)
      !>
      !>                    '~'     A ~ B is .TRUE. if A and B are disjoint
      !>                            sets.
      !>
      !>     B        is a set.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the result of the comparison: A (OP) B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the set relational operator is not recognized, the error
      !>         SPICE(INVALIDOPERATION) is signaled.
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
      !>     1) In the following example, SETx is used to repeat an operation
      !>        for as long as the integer set FINISHED remains a proper
      !>        subset of the integer set PLANNED.
      !>
      !>           DO WHILE ( SETx ( FINISHED, '<', PLANNED ) )
      !>              .
      !>              .
      !>           END DO
      !>
      !>     2) In the following example, let the integer sets A, B, and C
      !>        contain the elements listed below. Let E be an empty integer
      !>        set.
      !>
      !>             A        B        C
      !>           ---      ---      ---
      !>             1        1        1
      !>             2        3        3
      !>             3
      !>             4
      !>
      !>     Then all of the following expressions are true.
      !>
      !>           SETI ( B, '=',  C )      "B is equal to C"
      !>           SETI ( A, '<>', C )      "A is not equal to C"
      !>           SETI ( A, '>',  B )      "A is a proper superset of B"
      !>           SETI ( B, '<=', C )      "B is a subset of C"
      !>           SETI ( C, '<=', B )      "C is a subset of B"
      !>           SETI ( A, '<=', A )      "A is a subset of A"
      !>           SETI ( E, '<=', B )      "E is a subset of B"
      !>           SETI ( E, '<',  B )      "E is a proper subset of B"
      !>           SETI ( E, '<=', E )      "E is a subset of E"
      !>           SETI ( A, '&',  B )      "A has elements in common with B."
      !>           SETI ( B, '&',  C )      "B has elements in common with C."
      !>
      !>     And all of the following are false.
      !>
      !>           SETI ( B, '<>',  C )      "B is not equal to C"
      !>           SETI ( A, '=',   C )      "A is equal to C"
      !>           SETI ( A, '<',   B )      "A is a proper subset of B"
      !>           SETI ( B, '<',   C )      "B is a proper subset of C"
      !>           SETI ( B, '>=',  A )      "B is a superset of A"
      !>           SETI ( A, '>',   A )      "A is a proper superset of A"
      !>           SETI ( E, '>=',  A )      "E is a superset of A"
      !>           SETI ( E, '<',   E )      "E is a proper subset of E"
      !>           SETI ( A, '~',   B )      "A and B are disjoint sets."
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        Set the default function value to either 0, 0.0D0, .FALSE.,
      !>        or blank depending on the type of the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 11-JAN-1989 (WLT) (HAN)
      !>
      !>       The old version was not compatible with the error handling
      !>       mechanism. Taking the difference of sets A and B caused an
      !>       overflow of the set DIFF, whose dimension was one. The method of
      !>       determining the function value has been redesigned, and the
      !>       difference of the sets is no longer computed.
      !>
      !>       The new routine recognizes two new operators, '~' and '&'.
      !>       If the operator is not recognized, an error is now signaled.
      module logical function setc ( ctx, a, op, b )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: a      ( lbcell:* )  !! First set.
      character(*), intent(in) :: op  !! Comparison operator.
      character(*), intent(in) :: b      ( lbcell:* )  !! Second set.


      integer(ip) :: carda
      integer(ip) :: cardb
      integer(ip) :: condlt
      integer(ip) :: condeq
      integer(ip) :: condgt
      integer(ip) :: condoa
      integer(ip) :: condob
      integer(ip) :: condab
      integer(ip) :: indexa
      integer(ip) :: indexb
      integer(ip) :: cond

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         setc = .false.
         return
      else
         setc = .false.
      end if

!
!     Obtain the cardinality of the sets.
!
      carda = cardc ( ctx, a )
      cardb = cardc ( ctx, b )

!
!     The easiest way to compare two sets is to list them side by side
!     as shown below:
!
!     Set A  Set B
!     -----  -----
!       1      1
!              2
!       3      3
!       4      4
!       5
!              6
!       7      7
!
!     When listed this way, one can easily determine intersections,
!     differences, and unions.  Moreover, to determine if one set
!     is a subset of another, if they are equal, etc, one can just
!     inspect the two lists.
!
!     We can mimic this in an algorithm.  The main trick is to figure
!     out how to list the sets in this way.  Once we know how to
!     list them, we can simply adapt the listing algorithm to get
!     a comparison algorithm.
!
!     By the time we get this far, we know that our sets have distinct
!     elements and they are ordered. To write out the list above,
!     we start at the beginning of both sets (they're ordered,
!     remember?).  Look at the next element of A and the next element
!     of B ( to start out ``next'' means ``first'' ).  If the item
!     from A is smaller it should be written and space should be left
!     in the B column. If they are the same write them both.  Otherwise,
!     the item from B is smaller, so  leave space in the A column and
!     write the item from B.  Continue until you run out of items in
!     one of the sets.  Then just write down all those remaining in the
!     other set in the appropriate column.  This is what the loop
!     below does.
!
!           NEXTA = 1
!           NEXTB = 1
!
!           DO WHILE (       ( NEXTA .LT. CARD(A) )
!          .           .AND. ( NEXTB .LT. CARD(B) ) )
!
!              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
!
!                 WRITE (UNIT,*) A(NEXTA),   SPACES
!                 NEXTA = NEXTA + 1
!
!              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
!
!                 WRITE (UNIT,*) A(NEXTA),   B(NEXTB)
!                 NEXTA = NEXTA + 1
!                 NEXTB = NEXTB + 1
!
!              ELSE
!
!                 WRITE (UNIT,*) SPACES, B(NEXTB)
!                 NEXTB = NEXTB + 1
!
!              END IF
!           END DO
!
!           DO NEXTA = 1, CARD(A)
!              WRITE (UNIT,*) A(NEXTA),SPACES
!           END DO
!
!           DO NEXTB = 1, CARD(B)
!              WRITE (UNIT,*) B(NEXTB),SPACES
!           END DO
!
!     This also gives us a way to compare the elements of the two
!     sets one item at a time.  Instead of writing the items, we
!     can make a decision as to whether or not the sets have the
!     relationship we are interested in.
!
!     At the beginning of the loop we assume that the two sets are
!     related in the way we want.  Once the comparison has been made
!     we can decide if they are still related in that way.  If not,
!     we can RETURN .FALSE.  Using psuedo-code the loop is modified
!     as shown below.
!
!           NEXTA = 1
!           NEXTB = 1
!
!           DO WHILE (       ( NEXTA .LT. CARD(A) )
!          .           .AND. ( NEXTB .LT. CARD(B) ) )
!
!              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A<B)
!                 NEXTA   = NEXTA + 1
!
!              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A=B)
!                 NEXTA   = NEXTA + 1
!                 NEXTB   = NEXTB + 1
!
!              ELSE
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A>B)
!                 NEXTB   = NEXTB + 1
!
!              END IF
!
!              IF ( SURE_NOW(RELATED) ) THEN
!                 RETURN with the correct value.
!              ELSE
!                 Keep going.
!              END IF
!
!           END DO
!

!
!     Using the cardinality of the two sets, some function
!     values can be determined right away. If the cardinality
!     is not enough, we need to set up some conditions for the
!     loop which compares the individual elements of the sets.
!

!
!     A cannot be a proper subset of B if the cardinality of A is
!     greater than or equal to the cardinality of B.
!
      if      ( op == '<' ) then

              if ( carda >= cardb ) then
                 setc = .false.
                 return
              else
                 condlt = 0
                 condeq = 1
                 condgt = 1
                 condoa = 0
                 condob = 1
                 condab = 1
              end if

!
!     A cannot be a subset of B if A contains more elements than B.
!
      else if ( op == '<=' ) then

              if ( carda > cardb ) then
                 setc = .false.
                 return
               else
                 condlt = 0
                 condeq = 1
                 condgt = 1
                 condoa = 0
                 condob = 1
                 condab = 1
               end if

!
!     If the cardinality of the two sets is not equal, there's no way
!     that the two sets could be equal.
!
      else if ( op == '=' ) then

               if ( carda /= cardb ) then
                  setc = .false.
                  return
               else
                  condlt = 0
                  condeq = 1
                  condgt = 0
                  condoa = 0
                  condob = 0
                  condab = 1
               end if

!
!     If the cardinality of the two sets is not equal, the sets
!     are not equal.
!
      else if ( op == '<>' ) then

              if ( carda /= cardb ) then
                 setc = .true.
                 return
              else
                 condlt = 2
                 condeq = 1
                 condgt = 2
                 condoa = 0
                 condob = 0
                 condab = 0
              end if

!
!     B cannot be a proper subset of A if the cardinality of A is less
!     than or equal to the cardinality of B.
!
      else if ( op == '>' ) then

              if ( carda <= cardb ) then
                 setc = .false.
                 return
              else
                 condlt = 1
                 condeq = 1
                 condgt = 0
                 condoa = 1
                 condob = 0
                 condab = 1
              end if
!
!     B cannot be a subset of A if B contains more elements than A.
!
      else if ( op == '>=' ) then

              if ( carda < cardb ) then
                 setc = .false.
                 return
              else
                 condlt = 1
                 condeq = 1
                 condgt = 0
                 condoa = 1
                 condob = 0
                 condab = 1
              end if
!
!     If the cardinality of one of the sets is zero, they can't
!     possibly have any elements in common.
!
      else if ( op == '&' ) then

              if ( ( carda == 0 ) .or. ( cardb == 0 ) ) then
                 setc = .false.
                 return
              else
                 condlt = 1
                 condeq = 2
                 condgt = 1
                 condoa = 0
                 condob = 0
              end if

!
!     If either A or B is the null set, the two sets are disjoint.
!
      else if ( op == '~' ) then

              if ( ( carda == 0 ) .or. ( cardb == 0 ) ) then
                 setc = .true.
                 return
              else
                 condlt = 1
                 condeq = 0
                 condgt = 1
                 condoa = 1
                 condob = 1
              end if

!
!     If the relational operator is not recognized, signal an
!     error.
!

      else
             call ctx%setmsg ( 'Relational operator, *, is not '         // &
                           'recognized.'                              )
             call ctx%errch  ( '*', op                   )
             call ctx%sigerr ( 'SPICE(INVALIDOPERATION)' )
             return
      end if

!
!     Initialize counters used for checking the elements of the sets.
!
      indexa    = 1
      indexb    = 1
      cond      = 0

!
!     If we've come this far we need to check the elements of the
!     sets to determine the function value.
!
      do while ( ( indexa <= carda ) .and. ( indexb <= cardb ) )

         if      ( a (indexa) < b (indexb) )  then

                   cond   = condlt
                   indexa = indexa + 1

         else if ( a (indexa) == b (indexb) )  then

                   cond   = condeq
                   indexa = indexa + 1
                   indexb = indexb + 1

         else

                   cond   = condgt
                   indexb = indexb + 1

         end if

!
!        At this point, there are several cases which allow us to
!        determine the function value without continuing to compare
!        the elements of the sets:
!
!        1. If the operator is '~' and a common element was found,
!           the sets are not disjoint ( COND = 0 ).
!
!        2. If the operator is '&' and a common element was found,
!           the sets have at least one common element ( COND = 2 ).
!
!        3. If the sets are being compared for containment, and the
!           first element of the "contained" set is less than the first
!           element of the "containing" set, the "contained" set
!           cannot be a subset of the "containing" set ( COND = 0 ).
!
!        4. If the operator is '=' and the elements being compared are
!           not equal, the sets are not equal ( COND = 0 ).
!
!        5. If the operator is '<>' and the elements being compared are
!           not equal, the sets are not equal ( COND = 2 ).
!
         if      ( cond == 0 ) then

                  setc = .false.
                  return

         else if ( cond == 2 ) then

                  setc = .true.
                  return

         end if

      end do

!
!     We've exited the loop, so now we need to make a decision based on
!     what's left over.
!

!
!     We've gone through all of set B and there are elements left in
!     A.
!
      if      ( indexa <= carda ) then
                cond = condoa
!
!     We've gone through all of set A and there are elements left in
!     B.
!
      else if ( indexb <= cardb ) then
                cond = condob
!
!     We've gone through both the sets.
!
      else
                cond = condab
      end if

!
!     Determine the value of SETC from the results.
!
      setc = ( cond == 1 )

      return
      end function setc

      !> Compare two double precision sets, as indicated by a relational
      !> operator.
      !>
      !> The function returns the result of the comparison: A (OP) B.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     OP       is a comparison operator, indicating the way in
      !>              which the input sets are to be compared. OP may
      !>              be any of the following:
      !>
      !>                  Operator             Meaning
      !>                  --------  -------------------------------------
      !>                    '='     A = B is .TRUE. if A and B are equal
      !>                            (contain the same elements).
      !>
      !>                    '<>'    A <> B is .TRUE. if A and B are not
      !>                            equal.
      !>
      !>                    '<='    A <= B is .TRUE. if A is a subset of B.
      !>
      !>                    '<'     A < B is .TRUE. if A is a proper subset
      !>                            of B.
      !>
      !>                    '>='    A >= B is .TRUE. if B is a subset of A.
      !>
      !>                    '>'     A > B is .TRUE. if B is a proper subset
      !>                            of A.
      !>
      !>                    '&'     A & B is .TRUE. if A and B have one or
      !>                            more elements in common. (The
      !>                            intersection of the two sets in
      !>                            non-empty.)
      !>
      !>                    '~'     A ~ B is .TRUE. if A and B are disjoint
      !>                            sets.
      !>
      !>     B        is a set.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the result of the comparison: A (OP) B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the set relational operator is not recognized, the error
      !>         SPICE(INVALIDOPERATION) is signaled.
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
      !>     1) In the following example, SETx is used to repeat an operation
      !>        for as long as the integer set FINISHED remains a proper
      !>        subset of the integer set PLANNED.
      !>
      !>           DO WHILE ( SETx ( FINISHED, '<', PLANNED ) )
      !>              .
      !>              .
      !>           END DO
      !>
      !>     2) In the following example, let the integer sets A, B, and C
      !>        contain the elements listed below. Let E be an empty integer
      !>        set.
      !>
      !>             A        B        C
      !>           ---      ---      ---
      !>             1        1        1
      !>             2        3        3
      !>             3
      !>             4
      !>
      !>     Then all of the following expressions are true.
      !>
      !>           SETI ( B, '=',  C )      "B is equal to C"
      !>           SETI ( A, '<>', C )      "A is not equal to C"
      !>           SETI ( A, '>',  B )      "A is a proper superset of B"
      !>           SETI ( B, '<=', C )      "B is a subset of C"
      !>           SETI ( C, '<=', B )      "C is a subset of B"
      !>           SETI ( A, '<=', A )      "A is a subset of A"
      !>           SETI ( E, '<=', B )      "E is a subset of B"
      !>           SETI ( E, '<',  B )      "E is a proper subset of B"
      !>           SETI ( E, '<=', E )      "E is a subset of E"
      !>           SETI ( A, '&',  B )      "A has elements in common with B."
      !>           SETI ( B, '&',  C )      "B has elements in common with C."
      !>
      !>     And all of the following are false.
      !>
      !>           SETI ( B, '<>',  C )      "B is not equal to C"
      !>           SETI ( A, '=',   C )      "A is equal to C"
      !>           SETI ( A, '<',   B )      "A is a proper subset of B"
      !>           SETI ( B, '<',   C )      "B is a proper subset of C"
      !>           SETI ( B, '>=',  A )      "B is a superset of A"
      !>           SETI ( A, '>',   A )      "A is a proper superset of A"
      !>           SETI ( E, '>=',  A )      "E is a superset of A"
      !>           SETI ( E, '<',   E )      "E is a proper subset of E"
      !>           SETI ( A, '~',   B )      "A and B are disjoint sets."
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        Set the default function value to either 0, 0.0D0, .FALSE.,
      !>        or blank depending on the type of the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 11-JAN-1989 (WLT) (HAN)
      !>
      !>       The old version was not compatible with the error handling
      !>       mechanism. Taking the difference of sets A and B caused an
      !>       overflow of the set DIFF, whose dimension was one. The method of
      !>       determining the function value has been redesigned, and the
      !>       difference of the sets is no longer computed.
      !>
      !>       The new routine recognizes two new operators, '~' and '&'.
      !>       If the operator is not recognized, an error is now signaled.
      module logical function setd ( ctx, a, op, b )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a      ( lbcell:* )  !! First set.
      character(*), intent(in) :: op  !! Comparison operator.
      real(wp), intent(in) :: b      ( lbcell:* )  !! Second set.


      integer(ip) :: carda
      integer(ip) :: cardb
      integer(ip) :: condlt
      integer(ip) :: condeq
      integer(ip) :: condgt
      integer(ip) :: condoa
      integer(ip) :: condob
      integer(ip) :: condab
      integer(ip) :: indexa
      integer(ip) :: indexb
      integer(ip) :: cond

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         setd = .false.
         return
      else
         setd = .false.
      end if

!
!     Obtain the cardinality of the sets.
!
      carda = cardd ( ctx, a )
      cardb = cardd ( ctx, b )

!
!     The easiest way to compare two sets is to list them side by side
!     as shown below:
!
!     Set A  Set B
!     -----  -----
!       1      1
!              2
!       3      3
!       4      4
!       5
!              6
!       7      7
!
!     When listed this way, one can easily determine intersections,
!     differences, and unions.  Moreover, to determine if one set
!     is a subset of another, if they are equal, etc, one can just
!     inspect the two lists.
!
!     We can mimic this in an algorithm.  The main trick is to figure
!     out how to list the sets in this way.  Once we know how to
!     list them, we can simply adapt the listing algorithm to get
!     a comparison algorithm.
!
!     By the time we get this far, we know that our sets have distinct
!     elements and they are ordered. To write out the list above,
!     we start at the beginning of both sets (they're ordered,
!     remember?).  Look at the next element of A and the next element
!     of B ( to start out ``next'' means ``first'' ).  If the item
!     from A is smaller it should be written and space should be left
!     in the B column. If they are the same write them both.  Otherwise,
!     the item from B is smaller, so  leave space in the A column and
!     write the item from B.  Continue until you run out of items in
!     one of the sets.  Then just write down all those remaining in the
!     other set in the appropriate column.  This is what the loop
!     below does.
!
!           NEXTA = 1
!           NEXTB = 1
!
!           DO WHILE (       ( NEXTA .LT. CARD(A) )
!          .           .AND. ( NEXTB .LT. CARD(B) ) )
!
!              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
!
!                 WRITE (UNIT,*) A(NEXTA),   SPACES
!                 NEXTA = NEXTA + 1
!
!              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
!
!                 WRITE (UNIT,*) A(NEXTA),   B(NEXTB)
!                 NEXTA = NEXTA + 1
!                 NEXTB = NEXTB + 1
!
!              ELSE
!
!                 WRITE (UNIT,*) SPACES, B(NEXTB)
!                 NEXTB = NEXTB + 1
!
!              END IF
!           END DO
!
!           DO NEXTA = 1, CARD(A)
!              WRITE (UNIT,*) A(NEXTA),SPACES
!           END DO
!
!           DO NEXTB = 1, CARD(B)
!              WRITE (UNIT,*) B(NEXTB),SPACES
!           END DO
!
!     This also gives us a way to compare the elements of the two
!     sets one item at a time.  Instead of writing the items, we
!     can make a decision as to whether or not the sets have the
!     relationship we are interested in.
!
!     At the beginning of the loop we assume that the two sets are
!     related in the way we want.  Once the comparison has been made
!     we can decide if they are still related in that way.  If not,
!     we can RETURN .FALSE.  Using psuedo-code the loop is modified
!     as shown below.
!
!           NEXTA = 1
!           NEXTB = 1
!
!           DO WHILE (       ( NEXTA .LT. CARD(A) )
!          .           .AND. ( NEXTB .LT. CARD(B) ) )
!
!              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A<B)
!                 NEXTA   = NEXTA + 1
!
!              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A=B)
!                 NEXTA   = NEXTA + 1
!                 NEXTB   = NEXTB + 1
!
!              ELSE
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A>B)
!                 NEXTB   = NEXTB + 1
!
!              END IF
!
!              IF ( SURE_NOW(RELATED) ) THEN
!                 RETURN with the correct value.
!              ELSE
!                 Keep going.
!              END IF
!
!           END DO
!

!
!     Using the cardinality of the two sets, some function
!     values can be determined right away. If the cardinality
!     is not enough, we need to set up some conditions for the
!     loop which compares the individual elements of the sets.
!

!
!     A cannot be a proper subset of B if the cardinality of A is
!     greater than or equal to the cardinality of B.
!
      if      ( op == '<' ) then

              if ( carda >= cardb ) then
                 setd = .false.
                 return
              else
                 condlt = 0
                 condeq = 1
                 condgt = 1
                 condoa = 0
                 condob = 1
                 condab = 1
              end if

!
!     A cannot be a subset of B if A contains more elements than B.
!
      else if ( op == '<=' ) then

              if ( carda > cardb ) then
                 setd = .false.
                 return
               else
                 condlt = 0
                 condeq = 1
                 condgt = 1
                 condoa = 0
                 condob = 1
                 condab = 1
               end if

!
!     If the cardinality of the two sets is not equal, there's no way
!     that the two sets could be equal.
!
      else if ( op == '=' ) then

               if ( carda /= cardb ) then
                  setd = .false.
                  return
               else
                  condlt = 0
                  condeq = 1
                  condgt = 0
                  condoa = 0
                  condob = 0
                  condab = 1
               end if

!
!     If the cardinality of the two sets is not equal, the sets
!     are not equal.
!
      else if ( op == '<>' ) then

              if ( carda /= cardb ) then
                 setd = .true.
                 return
              else
                 condlt = 2
                 condeq = 1
                 condgt = 2
                 condoa = 0
                 condob = 0
                 condab = 0
              end if

!
!     B cannot be a proper subset of A if the cardinality of A is less
!     than or equal to the cardinality of B.
!
      else if ( op == '>' ) then

              if ( carda <= cardb ) then
                 setd = .false.
                 return
              else
                 condlt = 1
                 condeq = 1
                 condgt = 0
                 condoa = 1
                 condob = 0
                 condab = 1
              end if
!
!     B cannot be a subset of A if B contains more elements than A.
!
      else if ( op == '>=' ) then

              if ( carda < cardb ) then
                 setd = .false.
                 return
              else
                 condlt = 1
                 condeq = 1
                 condgt = 0
                 condoa = 1
                 condob = 0
                 condab = 1
              end if
!
!     If the cardinality of one of the sets is zero, they can't
!     possibly have any elements in common.
!
      else if ( op == '&' ) then

              if ( ( carda == 0 ) .or. ( cardb == 0 ) ) then
                 setd = .false.
                 return
              else
                 condlt = 1
                 condeq = 2
                 condgt = 1
                 condoa = 0
                 condob = 0
              end if

!
!     If either A or B is the null set, the two sets are disjoint.
!
      else if ( op == '~' ) then

              if ( ( carda == 0 ) .or. ( cardb == 0 ) ) then
                 setd = .true.
                 return
              else
                 condlt = 1
                 condeq = 0
                 condgt = 1
                 condoa = 1
                 condob = 1
              end if

!
!     If the relational operator is not recognized, signal an
!     error.
!

      else
             call ctx%setmsg ( 'Relational operator, *, is not '         // &
                           'recognized.'                              )
             call ctx%errch  ( '*', op                   )
             call ctx%sigerr ( 'SPICE(INVALIDOPERATION)' )
             return
      end if

!
!     Initialize counters used for checking the elements of the sets.
!
      indexa    = 1
      indexb    = 1
      cond      = 0

!
!     If we've come this far we need to check the elements of the
!     sets to determine the function value.
!
      do while ( ( indexa <= carda ) .and. ( indexb <= cardb ) )

         if      ( a (indexa) < b (indexb) )  then

                   cond   = condlt
                   indexa = indexa + 1

         else if ( a (indexa) == b (indexb) )  then

                   cond   = condeq
                   indexa = indexa + 1
                   indexb = indexb + 1

         else

                   cond   = condgt
                   indexb = indexb + 1

         end if

!
!        At this point, there are several cases which allow us to
!        determine the function value without continuing to compare
!        the elements of the sets:
!
!        1. If the operator is '~' and a common element was found,
!           the sets are not disjoint ( COND = 0 ).
!
!        2. If the operator is '&' and a common element was found,
!           the sets have at least one common element ( COND = 2 ).
!
!        3. If the sets are being compared for containment, and the
!           first element of the "contained" set is less than the first
!           element of the "containing" set, the "contained" set
!           cannot be a subset of the "containing" set ( COND = 0 ).
!
!        4. If the operator is '=' and the elements being compared are
!           not equal, the sets are not equal ( COND = 0 ).
!
!        5. If the operator is '<>' and the elements being compared are
!           not equal, the sets are not equal ( COND = 2 ).
!
         if      ( cond == 0 ) then

                  setd = .false.
                  return

         else if ( cond == 2 ) then

                  setd = .true.
                  return

         end if

      end do

!
!     We've exited the loop, so now we need to make a decision based on
!     what's left over.
!

!
!     We've gone through all of set B and there are elements left in
!     A.
!
      if      ( indexa <= carda ) then
                cond = condoa
!
!     We've gone through all of set A and there are elements left in
!     B.
!
      else if ( indexb <= cardb ) then
                cond = condob
!
!     We've gone through both the sets.
!
      else
                cond = condab
      end if

!
!     Determine the value of SETD from the results.
!
      setd = ( cond == 1 )

      return
      end function setd

      !> Compare two integer sets, as indicated by a relational operator.
      !>
      !> The function returns the result of the comparison: A (OP) B.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     OP       is a comparison operator, indicating the way in
      !>              which the input sets are to be compared. OP may
      !>              be any of the following:
      !>
      !>                  Operator             Meaning
      !>                  --------  -------------------------------------
      !>                    '='     A = B is .TRUE. if A and B are equal
      !>                            (contain the same elements).
      !>
      !>                    '<>'    A <> B is .TRUE. if A and B are not
      !>                            equal.
      !>
      !>                    '<='    A <= B is .TRUE. if A is a subset of B.
      !>
      !>                    '<'     A < B is .TRUE. if A is a proper subset
      !>                            of B.
      !>
      !>                    '>='    A >= B is .TRUE. if B is a subset of A.
      !>
      !>                    '>'     A > B is .TRUE. if B is a proper subset
      !>                            of A.
      !>
      !>                    '&'     A & B is .TRUE. if A and B have one or
      !>                            more elements in common. (The
      !>                            intersection of the two sets in
      !>                            non-empty.)
      !>
      !>                    '~'     A ~ B is .TRUE. if A and B are disjoint
      !>                            sets.
      !>
      !>     B        is a set.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the result of the comparison: A (OP) B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the set relational operator is not recognized, the error
      !>         SPICE(INVALIDOPERATION) is signaled.
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
      !>     1) In the following example, SETx is used to repeat an operation
      !>        for as long as the integer set FINISHED remains a proper
      !>        subset of the integer set PLANNED.
      !>
      !>           DO WHILE ( SETx ( FINISHED, '<', PLANNED ) )
      !>              .
      !>              .
      !>           END DO
      !>
      !>     2) In the following example, let the integer sets A, B, and C
      !>        contain the elements listed below. Let E be an empty integer
      !>        set.
      !>
      !>             A        B        C
      !>           ---      ---      ---
      !>             1        1        1
      !>             2        3        3
      !>             3
      !>             4
      !>
      !>     Then all of the following expressions are true.
      !>
      !>           SETI ( B, '=',  C )      "B is equal to C"
      !>           SETI ( A, '<>', C )      "A is not equal to C"
      !>           SETI ( A, '>',  B )      "A is a proper superset of B"
      !>           SETI ( B, '<=', C )      "B is a subset of C"
      !>           SETI ( C, '<=', B )      "C is a subset of B"
      !>           SETI ( A, '<=', A )      "A is a subset of A"
      !>           SETI ( E, '<=', B )      "E is a subset of B"
      !>           SETI ( E, '<',  B )      "E is a proper subset of B"
      !>           SETI ( E, '<=', E )      "E is a subset of E"
      !>           SETI ( A, '&',  B )      "A has elements in common with B."
      !>           SETI ( B, '&',  C )      "B has elements in common with C."
      !>
      !>     And all of the following are false.
      !>
      !>           SETI ( B, '<>',  C )      "B is not equal to C"
      !>           SETI ( A, '=',   C )      "A is equal to C"
      !>           SETI ( A, '<',   B )      "A is a proper subset of B"
      !>           SETI ( B, '<',   C )      "B is a proper subset of C"
      !>           SETI ( B, '>=',  A )      "B is a superset of A"
      !>           SETI ( A, '>',   A )      "A is a proper superset of A"
      !>           SETI ( E, '>=',  A )      "E is a superset of A"
      !>           SETI ( E, '<',   E )      "E is a proper subset of E"
      !>           SETI ( A, '~',   B )      "A and B are disjoint sets."
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        Set the default function value to either 0, 0.0D0, .FALSE.,
      !>        or blank depending on the type of the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 11-JAN-1989 (WLT) (HAN)
      !>
      !>       The old version was not compatible with the error handling
      !>       mechanism. Taking the difference of sets A and B caused an
      !>       overflow of the set DIFF, whose dimension was one. The method of
      !>       determining the function value has been redesigned, and the
      !>       difference of the sets is no longer computed.
      !>
      !>       The new routine recognizes two new operators, '~' and '&'.
      !>       If the operator is not recognized, an error is now signaled.
      module logical function seti ( ctx, a, op, b )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: a      ( lbcell:* )  !! First set.
      character(*), intent(in) :: op  !! Comparison operator.
      integer(ip), intent(in) :: b      ( lbcell:* )  !! Second set.


      integer(ip) :: carda
      integer(ip) :: cardb
      integer(ip) :: condlt
      integer(ip) :: condeq
      integer(ip) :: condgt
      integer(ip) :: condoa
      integer(ip) :: condob
      integer(ip) :: condab
      integer(ip) :: indexa
      integer(ip) :: indexb
      integer(ip) :: cond

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         seti = .false.
         return
      else
         seti = .false.
      end if

!
!     Obtain the cardinality of the sets.
!
      carda = cardi ( ctx, a )
      cardb = cardi ( ctx, b )

!
!     The easiest way to compare two sets is to list them side by side
!     as shown below:
!
!     Set A  Set B
!     -----  -----
!       1      1
!              2
!       3      3
!       4      4
!       5
!              6
!       7      7
!
!     When listed this way, one can easily determine intersections,
!     differences, and unions.  Moreover, to determine if one set
!     is a subset of another, if they are equal, etc, one can just
!     inspect the two lists.
!
!     We can mimic this in an algorithm.  The main trick is to figure
!     out how to list the sets in this way.  Once we know how to
!     list them, we can simply adapt the listing algorithm to get
!     a comparison algorithm.
!
!     By the time we get this far, we know that our sets have distinct
!     elements and they are ordered. To write out the list above,
!     we start at the beginning of both sets (they're ordered,
!     remember?).  Look at the next element of A and the next element
!     of B ( to start out ``next'' means ``first'' ).  If the item
!     from A is smaller it should be written and space should be left
!     in the B column. If they are the same write them both.  Otherwise,
!     the item from B is smaller, so  leave space in the A column and
!     write the item from B.  Continue until you run out of items in
!     one of the sets.  Then just write down all those remaining in the
!     other set in the appropriate column.  This is what the loop
!     below does.
!
!           NEXTA = 1
!           NEXTB = 1
!
!           DO WHILE (       ( NEXTA .LT. CARD(A) )
!          .           .AND. ( NEXTB .LT. CARD(B) ) )
!
!              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
!
!                 WRITE (UNIT,*) A(NEXTA),   SPACES
!                 NEXTA = NEXTA + 1
!
!              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
!
!                 WRITE (UNIT,*) A(NEXTA),   B(NEXTB)
!                 NEXTA = NEXTA + 1
!                 NEXTB = NEXTB + 1
!
!              ELSE
!
!                 WRITE (UNIT,*) SPACES, B(NEXTB)
!                 NEXTB = NEXTB + 1
!
!              END IF
!           END DO
!
!           DO NEXTA = 1, CARD(A)
!              WRITE (UNIT,*) A(NEXTA),SPACES
!           END DO
!
!           DO NEXTB = 1, CARD(B)
!              WRITE (UNIT,*) B(NEXTB),SPACES
!           END DO
!
!     This also gives us a way to compare the elements of the two
!     sets one item at a time.  Instead of writing the items, we
!     can make a decision as to whether or not the sets have the
!     relationship we are interested in.
!
!     At the beginning of the loop we assume that the two sets are
!     related in the way we want.  Once the comparison has been made
!     we can decide if they are still related in that way.  If not,
!     we can RETURN .FALSE.  Using psuedo-code the loop is modified
!     as shown below.
!
!           NEXTA = 1
!           NEXTB = 1
!
!           DO WHILE (       ( NEXTA .LT. CARD(A) )
!          .           .AND. ( NEXTB .LT. CARD(B) ) )
!
!              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A<B)
!                 NEXTA   = NEXTA + 1
!
!              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A=B)
!                 NEXTA   = NEXTA + 1
!                 NEXTB   = NEXTB + 1
!
!              ELSE
!
!                 RELATED = RELATIONSHIP_OF_INTEREST(A>B)
!                 NEXTB   = NEXTB + 1
!
!              END IF
!
!              IF ( SURE_NOW(RELATED) ) THEN
!                 RETURN with the correct value.
!              ELSE
!                 Keep going.
!              END IF
!
!           END DO
!

!
!     Using the cardinality of the two sets, some function
!     values can be determined right away. If the cardinality
!     is not enough, we need to set up some conditions for the
!     loop which compares the individual elements of the sets.
!

!
!     A cannot be a proper subset of B if the cardinality of A is
!     greater than or equal to the cardinality of B.
!
      if      ( op == '<' ) then

              if ( carda >= cardb ) then
                 seti = .false.
                 return
              else
                 condlt = 0
                 condeq = 1
                 condgt = 1
                 condoa = 0
                 condob = 1
                 condab = 1
              end if

!
!     A cannot be a subset of B if A contains more elements than B.
!
      else if ( op == '<=' ) then

              if ( carda > cardb ) then
                 seti = .false.
                 return
               else
                 condlt = 0
                 condeq = 1
                 condgt = 1
                 condoa = 0
                 condob = 1
                 condab = 1
               end if

!
!     If the cardinality of the two sets is not equal, there's no way
!     that the two sets could be equal.
!
      else if ( op == '=' ) then

               if ( carda /= cardb ) then
                  seti = .false.
                  return
               else
                  condlt = 0
                  condeq = 1
                  condgt = 0
                  condoa = 0
                  condob = 0
                  condab = 1
               end if

!
!     If the cardinality of the two sets is not equal, the sets
!     are not equal.
!
      else if ( op == '<>' ) then

              if ( carda /= cardb ) then
                 seti = .true.
                 return
              else
                 condlt = 2
                 condeq = 1
                 condgt = 2
                 condoa = 0
                 condob = 0
                 condab = 0
              end if

!
!     B cannot be a proper subset of A if the cardinality of A is less
!     than or equal to the cardinality of B.
!
      else if ( op == '>' ) then

              if ( carda <= cardb ) then
                 seti = .false.
                 return
              else
                 condlt = 1
                 condeq = 1
                 condgt = 0
                 condoa = 1
                 condob = 0
                 condab = 1
              end if
!
!     B cannot be a subset of A if B contains more elements than A.
!
      else if ( op == '>=' ) then

              if ( carda < cardb ) then
                 seti = .false.
                 return
              else
                 condlt = 1
                 condeq = 1
                 condgt = 0
                 condoa = 1
                 condob = 0
                 condab = 1
              end if
!
!     If the cardinality of one of the sets is zero, they can't
!     possibly have any elements in common.
!
      else if ( op == '&' ) then

              if ( ( carda == 0 ) .or. ( cardb == 0 ) ) then
                 seti = .false.
                 return
              else
                 condlt = 1
                 condeq = 2
                 condgt = 1
                 condoa = 0
                 condob = 0
              end if

!
!     If either A or B is the null set, the two sets are disjoint.
!
      else if ( op == '~' ) then

              if ( ( carda == 0 ) .or. ( cardb == 0 ) ) then
                 seti = .true.
                 return
              else
                 condlt = 1
                 condeq = 0
                 condgt = 1
                 condoa = 1
                 condob = 1
              end if

!
!     If the relational operator is not recognized, signal an
!     error.
!

      else
             call ctx%setmsg ( 'Relational operator, *, is not '         // &
                           'recognized.'                              )
             call ctx%errch  ( '*', op                   )
             call ctx%sigerr ( 'SPICE(INVALIDOPERATION)' )
             return
      end if

!
!     Initialize counters used for checking the elements of the sets.
!
      indexa    = 1
      indexb    = 1
      cond      = 0

!
!     If we've come this far we need to check the elements of the
!     sets to determine the function value.
!
      do while ( ( indexa <= carda ) .and. ( indexb <= cardb ) )

         if      ( a (indexa) < b (indexb) )  then

                   cond   = condlt
                   indexa = indexa + 1

         else if ( a (indexa) == b (indexb) )  then

                   cond   = condeq
                   indexa = indexa + 1
                   indexb = indexb + 1

         else

                   cond   = condgt
                   indexb = indexb + 1

         end if

!
!        At this point, there are several cases which allow us to
!        determine the function value without continuing to compare
!        the elements of the sets:
!
!        1. If the operator is '~' and a common element was found,
!           the sets are not disjoint ( COND = 0 ).
!
!        2. If the operator is '&' and a common element was found,
!           the sets have at least one common element ( COND = 2 ).
!
!        3. If the sets are being compared for containment, and the
!           first element of the "contained" set is less than the first
!           element of the "containing" set, the "contained" set
!           cannot be a subset of the "containing" set ( COND = 0 ).
!
!        4. If the operator is '=' and the elements being compared are
!           not equal, the sets are not equal ( COND = 0 ).
!
!        5. If the operator is '<>' and the elements being compared are
!           not equal, the sets are not equal ( COND = 2 ).
!
         if      ( cond == 0 ) then

                  seti = .false.
                  return

         else if ( cond == 2 ) then

                  seti = .true.
                  return

         end if

      end do

!
!     We've exited the loop, so now we need to make a decision based on
!     what's left over.
!

!
!     We've gone through all of set B and there are elements left in
!     A.
!
      if      ( indexa <= carda ) then
                cond = condoa
!
!     We've gone through all of set A and there are elements left in
!     B.
!
      else if ( indexb <= cardb ) then
                cond = condob
!
!     We've gone through both the sets.
!
      else
                cond = condab
      end if

!
!     Determine the value of SETI from the results.
!
      seti = ( cond == 1 )

      return
      end function seti

      !> Compute the union of two character sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the union of A and B (that is, all of
      !>              the elements which are in A or B or both).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the union of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the union of the two sets causes an excess of elements, the
      !>         error SPICE(SETEXCESS) is signaled.
      !>
      !>     2)  If length of the elements of the output set is < the
      !>         maximum of the lengths of the elements of the input
      !>         sets, the error SPICE(ELEMENTSTOOSHORT) is signaled.
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
      !>     The UNION of two sets contains every element which is
      !>     in the first set, or in the second set, or in both sets.
      !>
      !>           {a,b}      union  {c,d}     =  {a,b,c,d}
      !>           {a,b,c}           {b,c,d}      {a,b,c,d}
      !>           {a,b,c,d}         {}           {a,b,c,d}
      !>           {}                {a,b,c,d}    {a,b,c,d}
      !>           {}                {}           {}
      !>
      !>     The following call
      !>
      !>           CALL UNIONC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the union of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL UNIONI  ( CURRENT,     NEW, CURRENT )
      !>           CALL UNIONI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL UNIONI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Made CHKOUT calls consistent with CHKIN.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 05-JAN-1989 (NJB)
      !>
      !>        Error signaled if output set elements are not long enough.
      !>        Length must be at least max of lengths of input elements.
      !>        Also, calling protocol for EXCESS has been changed.
      module subroutine unionc ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
      character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
      character(*), intent(inout) :: c      ( lbcell:* )  !! Union of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Make sure output set elements are long enough.
!
      if (               len( c(lbcell) ) &
            <  max (  len( a(lbcell) ), len( b(lbcell) )  )   ) then

         call ctx%setmsg ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         call ctx%errint ( '#', len( c(lbcell) )  )
         call ctx%errint ( '#', max (  len(a(lbcell)), len(b(lbcell)) )  )

         call ctx%sigerr ( 'SPICE(ELEMENTSTOOSHORT)' )

         return

      end if

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardc ( ctx, a )
      bcard = cardc ( ctx, b )
      csize = sizec ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the ends of both input sets are reached, we're done.
!
      do while ( apoint <= acard .or. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( apoint > acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint > bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( lgt ( a(apoint), b(bpoint) ) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint > acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( lgt ( a(apoint), b(bpoint) ) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardc ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine unionc

      !> Compute the union of two double precision sets to form a third
      !> set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the union of A and B (that is, all of
      !>              the elements which are in A or B or both).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the union of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the union of the two sets causes an excess of elements, the
      !>         error SPICE(SETEXCESS) is signaled.
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
      !>     The UNION of two sets contains every element which is
      !>     in the first set, or in the second set, or in both sets.
      !>
      !>           {a,b}      union  {c,d}     =  {a,b,c,d}
      !>           {a,b,c}           {b,c,d}      {a,b,c,d}
      !>           {a,b,c,d}         {}           {a,b,c,d}
      !>           {}                {a,b,c,d}    {a,b,c,d}
      !>           {}                {}           {}
      !>
      !>     The following call
      !>
      !>           CALL UNIONC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the union of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL UNIONI  ( CURRENT,     NEW, CURRENT )
      !>           CALL UNIONI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL UNIONI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 05-JAN-1989 (NJB)
      !>
      !>        Calling protocol for EXCESS updated. Call to SETMSG
      !>        deleted.
      module subroutine uniond ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
      real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
      real(wp), intent(inout) :: c      ( lbcell:* )  !! Union of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the ends of both input sets are reached, we're done.
!
      do while ( apoint <= acard .or. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( apoint > acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint > bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) > b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint > acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) > b(bpoint) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardd ( ctx, ccard, c )

!
!     Report any excess.
!

      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine uniond

      !> Compute the union of two integer sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the union of A and B (that is, all of
      !>              the elements which are in A or B or both).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the union of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the union of the two sets causes an excess of elements, the
      !>         error SPICE(SETEXCESS) is signaled.
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
      !>     The UNION of two sets contains every element which is
      !>     in the first set, or in the second set, or in both sets.
      !>
      !>           {a,b}      union  {c,d}     =  {a,b,c,d}
      !>           {a,b,c}           {b,c,d}      {a,b,c,d}
      !>           {a,b,c,d}         {}           {a,b,c,d}
      !>           {}                {a,b,c,d}    {a,b,c,d}
      !>           {}                {}           {}
      !>
      !>     The following call
      !>
      !>           CALL UNIONC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the union of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL UNIONI  ( CURRENT,     NEW, CURRENT )
      !>           CALL UNIONI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL UNIONI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 05-JAN-1989 (NJB)
      !>
      !>        Calling protocol for EXCESS updated. Call to SETMSG
      !>        deleted.
      module subroutine unioni ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
      integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
      integer(ip), intent(inout) :: c      ( lbcell:* )  !! Union of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardi ( ctx, a )
      bcard = cardi ( ctx, b )
      csize = sizei ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the ends of both input sets are reached, we're done.
!
      do while ( apoint <= acard .or. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( apoint > acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint > bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) > b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint > acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) > b(bpoint) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardi ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine unioni

      !> Intersect two character sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the intersection of A and B (that is,
      !>              all of the elements which are in A, AND in B).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the intersection of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the intersection of the two sets causes an excess of
      !>         elements, the error SPICE(SETEXCESS) is signaled.
      !>
      !>     2)  If length of the elements of the output set is < the
      !>         maximum of the lengths of the elements of the input
      !>         sets, the error SPICE(ELEMENTSTOOSHORT) is signaled.
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
      !>     The INTERSECTION of two sets contains every element
      !>     which is in the first set AND in the second set.
      !>
      !>           {a,b}      intersect  {c,d}     =  {}
      !>           {a,b,c}               {b,c,d}      {b,c}
      !>           {a,b,c,d}             {}           {}
      !>           {}                    {a,b,c,d}    {}
      !>           {}                    {}           {}
      !>
      !>     The following call
      !>
      !>           CALL INTERC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the intersection of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL INTERI  ( CURRENT,     NEW, CURRENT )
      !>           CALL INTERI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL INTERI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW        )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Fixed call to CHKOUT to be consistent with CHKIN.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 05-JAN-1989 (NJB)
      !>
      !>        Error signaled if output set elements are not long enough.
      !>        Length must be at least max of lengths of input elements.
      !>        Also, calling protocol for EXCESS has been changed.
      module subroutine interc ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
      character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
      character(*), intent(inout) :: c      ( lbcell:* )  !! Intersection of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Make sure output set elements are long enough.
!
      if (               len( c(lbcell) ) &
            <  max (  len( a(lbcell) ), len( b(lbcell) )  )   ) then

         call ctx%setmsg ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         call ctx%errint ( '#', len( c(lbcell) )  )
         call ctx%errint ( '#', max (  len(a(lbcell)), len(b(lbcell)) )  )

         call ctx%sigerr ( 'SPICE(ELEMENTSTOOSHORT)' )

         return

      end if

!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardc ( ctx, a )
      bcard = cardc ( ctx, b )
      csize = sizec ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of either input set is reached, we're done.
!
      do while ( apoint <= acard .and. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)

               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               apoint = apoint + 1

            else if ( llt ( b(bpoint), a(apoint) ) ) then

               bpoint = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( a(apoint) == b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               apoint = apoint + 1

            else if ( llt ( b(bpoint), a(apoint) ) ) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardc ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine interc

      !> Intersect two double precision sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the intersection of A and B (that is,
      !>              all of the elements which are in A, AND in B).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the intersection of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the intersection of the two sets causes an excess of
      !>         elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The INTERSECTION of two sets contains every element
      !>     which is in the first set AND in the second set.
      !>
      !>           {a,b}      intersect  {c,d}     =  {}
      !>           {a,b,c}               {b,c,d}      {b,c}
      !>           {a,b,c,d}             {}           {}
      !>           {}                    {a,b,c,d}    {}
      !>           {}                    {}           {}
      !>
      !>     The following call
      !>
      !>           CALL INTERC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the intersection of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL INTERI  ( CURRENT,     NEW, CURRENT )
      !>           CALL INTERI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL INTERI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW        )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      module subroutine interd ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
      real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
      real(wp), intent(inout) :: c      ( lbcell:* )  !! Intersection of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of either input set is reached, we're done.
!
      do while ( apoint <= acard .and. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)

               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               apoint = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if

!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if (a(apoint) == b(bpoint)) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if (a(apoint) < b(bpoint)) then

               apoint = apoint + 1

            else if (b(bpoint) < a(apoint)) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardd ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine interd

      !> Intersect two integer sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the intersection of A and B (that is,
      !>              all of the elements which are in A, AND in B).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the intersection of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the intersection of the two sets causes an excess of
      !>         elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The INTERSECTION of two sets contains every element
      !>     which is in the first set AND in the second set.
      !>
      !>           {a,b}      intersect  {c,d}     =  {}
      !>           {a,b,c}               {b,c,d}      {b,c}
      !>           {a,b,c,d}             {}           {}
      !>           {}                    {a,b,c,d}    {}
      !>           {}                    {}           {}
      !>
      !>     The following call
      !>
      !>           CALL INTERC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the intersection of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL INTERI  ( CURRENT,     NEW, CURRENT )
      !>           CALL INTERI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL INTERI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW        )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      module subroutine interi ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
      integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
      integer(ip), intent(inout) :: c      ( lbcell:* )  !! Intersection of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardi ( ctx, a )
      bcard = cardi ( ctx, b )
      csize = sizei ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of either input set is reached, we're done.
!
      do while ( apoint <= acard .and. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)

               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               apoint = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if

!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if (a(apoint) == b(bpoint)) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if (a(apoint) < b(bpoint)) then

               apoint = apoint + 1

            else if (b(bpoint) < a(apoint)) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardi ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine interi

      !> Take the difference of two character sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the difference of A and B (that is,
      !>              all of the elements which are in A, but NOT
      !>              in B).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the difference of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is returned.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the difference of the two sets causes an excess of
      !>         elements, the error SPICE(SETEXCESS) is signaled.
      !>
      !>     2)  If length of the elements of the output set is less than
      !>         the length of the elements of the FIRST input set, the
      !>         error SPICE(ELEMENTSTOOSHORT) is signaled.
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
      !>     The DIFFERENCE of two sets contains every element which is
      !>     in the first set, but NOT in the second.
      !>
      !>           {a,b}      difference  {c,d}     =  {a,b}
      !>           {a,b,c}                {b,c,d}      {a}
      !>           {a,b,c,d}              {}           {a,b,c,d}
      !>           {}                     {a,b,c,d}    {}
      !>           {}                     {}           {}
      !>
      !>     The following call
      !>
      !>           CALL DIFFC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the difference of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL DIFFI  ( CURRENT,     NEW, CURRENT )
      !>           CALL DIFFI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL DIFFI  ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW        )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 21-DEC-1988 (NJB)
      !>
      !>        Error signaled if output set elements are not long enough.
      !>        Length must be at least max of lengths of input elements.
      !>        Also, calling protocol for EXCESS has been changed. Call to
      !>        SETMSG removed.
      !>
      !>        Also, in the overflow case, the number of excess elements was
      !>        computed incorrectly; this has been fixed. The problem was
      !>        that OVER was incremented in all cases of the overflow IF
      !>        block, rather than only in the cases where the cardinality of
      !>        the output cell would have been incremented if there were
      !>        enough room.
      module subroutine diffc ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
      character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
      character(*), intent(inout) :: c      ( lbcell:* )  !! Difference of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Make sure output set elements are long enough.
!
      if (   len( c(lbcell) )  <   len( a(lbcell) )  ) then

         call ctx%setmsg ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         call ctx%errint ( '#', len( c(lbcell) )  )
         call ctx%errint ( '#', max (  len(a(lbcell)), len(b(lbcell)) )  )

         call ctx%sigerr ( 'SPICE(ELEMENTSTOOSHORT)' )

         return

      end if

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardc ( ctx, a )
      bcard = cardc ( ctx, b )
      csize = sizec ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of the first input set is reached, we're done.
!
      do while ( apoint <= acard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( bpoint > bcard) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint)) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( llt ( b(bpoint), a(apoint) ) ) then

               bpoint = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( llt ( b(bpoint), a(apoint) ) ) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardc ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then

         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )

      end if

      return
      end subroutine diffc

      !> Take the difference of two double precision sets to form
      !> a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the difference of A and B (that is,
      !>              all of the elements which are in A, but NOT
      !>              in B).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the difference of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is returned.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the difference of the two sets causes an excess of
      !>         elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The DIFFERENCE of two sets contains every element which is
      !>     in the first set, but NOT in the second.
      !>
      !>           {a,b}      difference  {c,d}     =  {a,b}
      !>           {a,b,c}                {b,c,d}      {a}
      !>           {a,b,c,d}              {}           {a,b,c,d}
      !>           {}                     {a,b,c,d}    {}
      !>           {}                     {}           {}
      !>
      !>     The following call
      !>
      !>           CALL DIFFC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the difference of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL DIFFI  ( CURRENT,     NEW, CURRENT )
      !>           CALL DIFFI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL DIFFI  ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW       )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      !>
      !>        Also, in the overflow case, the number of excess elements was
      !>        computed incorrectly; this has been fixed. The problem was
      !>        that OVER was incremented in all cases of the overflow IF
      !>        block, rather than only in the cases where the cardinality of
      !>        the output cell would have been incremented if there were
      !>        enough room.
      module subroutine diffd ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
      real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
      real(wp), intent(inout) :: c      ( lbcell:* )  !! Difference of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of the first input set is reached, we're done.
!
      do while ( apoint <= acard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( bpoint > bcard) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint)) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               over   = over   + 1
               apoint   = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardd ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine diffd

      !> Take the difference of two integer sets to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the difference of A and B (that is,
      !>              all of the elements which are in A, but NOT
      !>              in B).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the difference of A and B,
      !>              then only as many items as will fit in C are
      !>              included, and an error is returned.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the difference of the two sets causes an excess of
      !>         elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The DIFFERENCE of two sets contains every element which is
      !>     in the first set, but NOT in the second.
      !>
      !>           {a,b}      difference  {c,d}     =  {a,b}
      !>           {a,b,c}                {b,c,d}      {a}
      !>           {a,b,c,d}              {}           {a,b,c,d}
      !>           {}                     {a,b,c,d}    {}
      !>           {}                     {}           {}
      !>
      !>     The following call
      !>
      !>           CALL DIFFC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the difference of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL DIFFI  ( CURRENT,     NEW, CURRENT )
      !>           CALL DIFFI  (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL DIFFI  ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW        )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      !>
      !>        Also, in the overflow case, the number of excess elements was
      !>        computed incorrectly; this has been fixed. The problem was
      !>        that OVER was incremented in all cases of the overflow IF
      !>        block, rather than only in the cases where the cardinality of
      !>        the output cell would have been incremented if there were
      !>        enough room.
      module subroutine diffi ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
      integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
      integer(ip), intent(inout) :: c      ( lbcell:* )  !! Difference of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardi ( ctx, a )
      bcard = cardi ( ctx, b )
      csize = sizei ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of the first input set is reached, we're done.
!
      do while ( apoint <= acard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( bpoint > bcard) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint)) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if

!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               over   = over   + 1
               apoint   = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardi ( ctx, ccard, c )

!
!     Report any excess.
!

      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine diffi

      !> Take the symmetric difference of two character sets to form
      !> a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the symmetric difference of A and B
      !>              (that is, all of the elements which are in A
      !>              OR in B, but NOT in both).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the symmetric difference of
      !>              A and B, then only as many items as will fit in C
      !>              are included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symmetric difference of the two sets causes an excess
      !>         of elements, the error SPICE(SETEXCESS) is signaled.
      !>
      !>     2)  If length of the elements of the output set is < the
      !>         maximum of the lengths of the elements of the input
      !>         sets, the error SPICE(ELEMENTSTOOSHORT) is signaled.
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
      !>     The SYMMETRIC DIFFERENCE of two sets contains every
      !>     element which is in the first set OR in the second set,
      !>     but NOT in both sets.
      !>
      !>           {a,b}      sym. difference {c,d}     =  {a,b,c,d}
      !>           {a,b,c}                    {b,c,d}      {a,d}
      !>           {a,b,c,d}                  {}           {a,b,c,d}
      !>           {}                         {a,b,c,d}    {a,b,c,d}
      !>           {}                         {}           {}
      !>
      !>     The following call
      !>
      !>           CALL SDIFFC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the symmetric difference of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL SDIFFI ( CURRENT,     NEW, CURRENT )
      !>           CALL SDIFFI (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL SDIFFI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW        )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Modified call to CHKOUT to be consistent with CHKIN.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 05-JAN-1989 (NJB)
      !>
      !>        Error signaled if output set elements are not long enough.
      !>        Length must be at least max of lengths of input elements.
      !>        Also, calling protocol for EXCESS has been changed.
      module subroutine sdiffc ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
      character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
      character(*), intent(inout) :: c      ( lbcell:* )  !! Symmetric difference of A and B.


!     Local variables
!

      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Make sure output set elements are long enough.
!
      if (               len( c(lbcell) ) &
            <  max (  len( a(lbcell) ), len( b(lbcell) )  )   ) then

         call ctx%setmsg ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         call ctx%errint ( '#', len( c(lbcell) )  )
         call ctx%errint ( '#', max (  len(a(lbcell)), len(b(lbcell)) )  )

         call ctx%sigerr ( 'SPICE(ELEMENTSTOOSHORT)' )

         return

      end if
!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardc ( ctx, a )
      bcard = cardc ( ctx, b )
      csize = sizec ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of both input sets are reached, we're done.
!
      do while ( apoint <= acard .or. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( apoint > acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint > bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( llt ( b(bpoint), a(apoint) ) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint > acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( llt ( a(apoint), b(bpoint) ) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( llt ( b(bpoint), a(apoint) ) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardc ( ctx, ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine sdiffc

      !> Take the symmetric difference of two double precision sets
      !> to form a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the symmetric difference of A and B
      !>              (that is, all of the elements which are in A
      !>              OR in B, but NOT in both).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the symmetric difference of
      !>              A and B, then only as many items as will fit in C
      !>              are included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symmetric difference of the two sets causes an excess
      !>         of elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The SYMMETRIC DIFFERENCE of two sets contains every
      !>     element which is in the first set OR in the second set,
      !>     but NOT in both sets.
      !>
      !>           {a,b}      sym. difference {c,d}     =  {a,b,c,d}
      !>           {a,b,c}                    {b,c,d}      {a,d}
      !>           {a,b,c,d}                  {}           {a,b,c,d}
      !>           {}                         {a,b,c,d}    {a,b,c,d}
      !>           {}                         {}           {}
      !>
      !>     The following call
      !>
      !>           CALL SDIFFC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the symmetric difference of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL SDIFFI ( CURRENT,     NEW, CURRENT )
      !>           CALL SDIFFI (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL SDIFFI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      module subroutine sdiffd ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
      real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
      real(wp), intent(inout) :: c      ( lbcell:* )  !! Symmetric difference of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of both input sets are reached, we're done.
!
      do while ( apoint <= acard .or. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( apoint > acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint > bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint > acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardd ( ctx, ccard, c )

!
!     Report any excess.
!

      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine sdiffd

      !> Take the symmetric difference of two integer sets to form
      !> a third set.
      !>
      !>## Detailed Input
      !>
      !>     A        is a set.
      !>
      !>     B        is a set, distinct from A.
      !>
      !>## Detailed Output
      !>
      !>     C        is a set, distinct from sets A and B, which
      !>              contains the symmetric difference of A and B
      !>              (that is, all of the elements which are in A
      !>              OR in B, but NOT in both).
      !>
      !>              If the size (maximum cardinality) of C is smaller
      !>              than the cardinality of the symmetric difference of
      !>              A and B, then only as many items as will fit in C
      !>              are included, and an error is signaled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symmetric difference of the two sets causes an excess
      !>         of elements, the error SPICE(SETEXCESS) is signaled.
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
      !>     The SYMMETRIC DIFFERENCE of two sets contains every
      !>     element which is in the first set OR in the second set,
      !>     but NOT in both sets.
      !>
      !>           {a,b}      sym. difference {c,d}     =  {a,b,c,d}
      !>           {a,b,c}                    {b,c,d}      {a,d}
      !>           {a,b,c,d}                  {}           {a,b,c,d}
      !>           {}                         {a,b,c,d}    {a,b,c,d}
      !>           {}                         {}           {}
      !>
      !>     The following call
      !>
      !>           CALL SDIFFC  ( PLANETS, ASTEROIDS, RESULT )
      !>
      !>     places the symmetric difference of the character sets PLANETS and
      !>     ASTEROIDS into the character set RESULT.
      !>
      !>     The output set must be distinct from both of the input sets.
      !>     For example, the following calls are invalid.
      !>
      !>           CALL SDIFFI ( CURRENT,     NEW, CURRENT )
      !>           CALL SDIFFI (     NEW, CURRENT, CURRENT )
      !>
      !>     In each of the examples above, whether or not the subroutine
      !>     signals an error, the results will almost certainly be wrong.
      !>     Nearly the same effect can be achieved, however, by placing the
      !>     result into a temporary set, which is immediately copied back
      !>     into one of the input sets, as shown below.
      !>
      !>           CALL SDIFFI ( CURRENT, NEW,  TEMP )
      !>           CALL COPYI  ( TEMP,    NEW )
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
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
      !>
      !>        Calling protocol of EXCESS changed. Call to SETMSG removed.
      module subroutine sdiffi ( ctx, a, b, c )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
      integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
      integer(ip), intent(inout) :: c      ( lbcell:* )  !! Symmetric difference of A and B.


      integer(ip) :: apoint
      integer(ip) :: bpoint

      integer(ip) :: csize
      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: ccard

      integer(ip) :: over

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardi ( ctx, a )
      bcard = cardi ( ctx, b )
      csize = sizei ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the end of both input sets are reached, we're done.
!
      do while ( apoint <= acard .or. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( apoint > acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint > bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint > acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint > bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) == b(bpoint) ) then

               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardi ( ctx, ccard, c )

!
!     Report any excess.
!

      if ( over > 0 ) then
         call excess ( ctx, over, 'set' )
         call ctx%sigerr ( 'SPICE(SETEXCESS)' )
      end if

      return
      end subroutine sdiffi

      !> Insert an interval into a double precision window.
      !>
      !>## Detailed Input
      !>
      !>      LEFT,
      !>      RIGHT       are the left and right endpoints of the interval
      !>                  to be inserted.
      !>
      !>      CONTEXT     a context/explanation string to append to the
      !>                  long error message if an error signals. The caller
      !>                  need not include a message. A single blank, ' ',
      !>                  represents no message.
      !>
      !>      WINDOW      on input, is a window containing zero or more
      !>                  intervals.
      !>
      !>## Detailed Output
      !>
      !>      WINDOW      on output, is the original window following the
      !>                  insertion of the interval from LEFT to RIGHT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is
      !>        signaled.
      !>
      !>     2) If the insertion of the interval causes an excess of elements,
      !>        the error SPICE(WINDOWEXCESS) is signaled.
      !>
      !>## Files
      !>
      !>      None.
      !>
      !>## Particulars
      !>
      !>      This routine inserts the interval from LEFT to RIGHT into the
      !>      input window. If the new interval overlaps any of the intervals
      !>      in the window, the intervals are merged. Thus, the cardinality
      !>      of the input window can actually decrease as the result of an
      !>      insertion. However, because inserting an interval that is
      !>      disjoint from the other intervals in the window can increase the
      !>      cardinality of the window, the routine signals an error.
      !>
      !>      This is the only unary routine to signal an error. No
      !>      other unary routine can increase the number of intervals in
      !>      the input window.
      !>
      !>      If a non-blank CONTEXT string passes from the caller, any error
      !>      signal will return the long error message with the CONTEXT
      !>      string appended to that message.
      !>
      !>## Examples
      !>
      !>      Let WINDOW contain the intervals
      !>
      !>            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>      Then the following series of calls
      !>
      !>            CALL ZZWNINSD ( 5,  5, CONTEXT, WINDOW)       (1)
      !>            CALL ZZWNINSD ( 4,  8, CONTEXT, WINDOW)       (2)
      !>            CALL ZZWNINSD ( 0, 30, CONTEXT, WINDOW)       (3)
      !>
      !>      produces the following series of windows
      !>
      !>            [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]   (1)
      !>            [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]               (2)
      !>            [ 0, 30 ]                                      (3)
      !>
      !>## Restrictions
      !>
      !>      None.
      !>
      !>## Literature References
      !>
      !>      None.
      !>
      !>## Author and Institution
      !>
      !>      K.R. Gehringer  (JPL)
      !>      N.J. Bachman    (JPL)
      !>      H.A. Neilan     (JPL)
      !>      W.L. Taber      (JPL)
      !>      I.M. Underwood  (JPL)
      !>
      !>## Version
      !>
      !>-     SPICELIB Version 1.1.1, 05-OCT-2021 (NJB)
      !>
      !>         Corrected typos in long error messages and in comments.
      !>
      !>-     SPICELIB Version 1.0.0, 03-MAR-2009 (EDW)
      !>
      !>         This routine is a copy of the SPICELIB WNINSD routine
      !>         changed only by the addition of the CONTEXT string.
      module subroutine zzwninsd ( ctx, left, right, context, window )
      use pepper_kinds, only: ip, wp
      use pepper_inc_errhnd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: left  !! Left, right endpoints of new interval.
      real(wp), intent(in) :: right  !! Left, right endpoints of new interval.
      character(*), intent(in) :: context  !! A call explanation string.
      real(wp), intent(inout) :: window    ( lbcell:* )  !! Input, output window.


      integer(ip) :: size
      integer(ip) :: card

      integer(ip) :: i
      integer(ip) :: j

      character(lmsgln) :: msg

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the size and cardinality of the window.
!
      size = sized ( ctx, window )
      card = cardd ( ctx, window )

!
!     Let's try the easy cases first. No input interval? No change.
!     Signal that an error has occurred and set the error message.
!
      if ( left > right ) then

         msg = 'Left endpoint greater-than right. Left endpoint ' &
            // 'was #1. Right endpoint was #2.'

         msg = msg(1: len_trim(msg) ) //' '// context(1: len_trim(context) )

         call ctx%setmsg ( msg)
         call ctx%errdp  ( '#1', left  )
         call ctx%errdp  ( '#2', right )
         call ctx%sigerr ( 'SPICE(BADENDPOINTS)' )

         return

      else if ( card == 0  .or.  left > window(card) ) then

!
!        Empty window? Input interval later than the end of the window?
!        Just insert the interval, if there's room.
!
         if ( size >= card+2 ) then

            call scardd ( ctx, card+2, window )
            window(card+1) = left
            window(card+2) = right

         else

            msg = 'Window has size, #1, cardinality #2. Cannot ' &
               // 'insert an additional interval into the ' &
               // 'window.'

            msg = msg(1: len_trim(msg) ) //' '//  context(1: len_trim(context) )

            call ctx%setmsg ( msg)
            call ctx%errint ( '#1', size  )
            call ctx%errint ( '#2', card )
            call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )

         end if

         return

      end if

!
!     Now on to the tougher cases.
!
!     Skip intervals which lie completely to the left of the input
!     interval. (The index I will always point to the right endpoint
!     of an interval).
!
      i = 2

      do while ( i <= card )
         if ( .not. ( window(i) < left ) ) exit
         i = i + 2
      end do

!
!     There are three ways this can go. The new interval can:
!
!        1) lie entirely between the previous interval and the next.
!
!        2) overlap the next interval, but no others.
!
!        3) overlap more than one interval.
!
!     Only the first case can possibly cause an overflow, since the
!     other two cases require existing intervals to be merged.
!

!
!     Case (1). If there's room, move succeeding intervals back and
!     insert the new one. If there isn't room, signal an error.
!
      if ( right < window(i-1) ) then

         if ( size >= card+2 ) then

            do j = card, i-1, -1
               window(j+2) = window(j)
            end do

            call scardd ( ctx, card+2, window )
            window(i-1) = left
            window(i  ) = right

         else

            msg = 'Window has size, #1, cardinality #2. Cannot ' &
               // 'insert an additional interval into the ' &
               // 'window. The new interval lies entirely ' &
               // 'between the previous interval and the' &
               // 'next.'

            msg = msg(1: len_trim(msg) ) //' '//  context(1: len_trim(context) )

            call ctx%setmsg ( msg)
            call ctx%errint ( '#1', size  )
            call ctx%errint ( '#2', card )
            call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )

            return

         end if

!
!     Cases (2) and (3).
!
      else

!
!        The left and right endpoints of the new interval may or
!        may not replace the left and right endpoints of the existing
!        interval.
!
         window(i-1) = min ( left,  window(i-1) )
         window(i  ) = max ( right, window(i  ) )

!
!        Skip any intervals contained in the one we modified.
!        (Like I, J always points to the right endpoint of an
!        interval.)
!
         j = i + 2

         do while ( j <= card )
            if ( .not. ( window(j) <= window(i) ) ) exit
            j = j + 2
         end do

!
!        If the modified interval extends into the next interval,
!        merge the two. (The modified interval grows to the right.)
!
         if ( j <= card ) then
            if ( window(i) >= window(j-1) ) then
               window(i) = window(j)
               j         = j + 2
            end if
         end if

!
!        Move the rest of the intervals forward to take up the
!        spaces left by the absorbed intervals.
!
         do while ( j <= card )
            i           = i + 2
            window(i-1) = window(j-1)
            window(i  ) = window(j  )
            j           = j + 2
         end do

         call scardd ( ctx, i, window )

      end if

      return
      end subroutine zzwninsd

      !> Return the cardinality (number of intervals) of a double
      !> precision window.
      !>
      !> The function returns the cardinality of the input window.
      !>
      !>## Detailed Input
      !>
      !>     WINDOW   is a window containing zero or more intervals.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the cardinality of (number of intervals in)
      !>     the input window.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the number of elements in WINDOW is not even,
      !>         the error SPICE(INVALIDSIZE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The window cardinality (WNCARD) function simply wraps a CARD call
      !>     then divides the result by 2. A common error when using the SPICE
      !>     windows function is to use the CARDD value as the number of
      !>     window intervals rather than the CARDD/2 value.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) The following code example demonstrates how to insert an
      !>        interval into an existing double precision SPICE window, and
      !>        how to loop over all its intervals to extract their left and
      !>        right points.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNCARD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WNSIZE
      !>      PARAMETER           ( WNSIZE = 10 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
      !>      DOUBLE PRECISION      LEFT
      !>      DOUBLE PRECISION      RIGHT
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Validate the window with size WNSIZE and zero elements.
      !>C
      !>      CALL WNVALD( WNSIZE, 0, WINDOW )
      !>
      !>C
      !>C     Insert the intervals
      !>C
      !>C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>C
      !>C     into WINDOW.
      !>C
      !>      CALL WNINSD(  1.D0,  3.D0, WINDOW )
      !>      CALL WNINSD(  7.D0, 11.D0, WINDOW )
      !>      CALL WNINSD( 23.D0, 27.D0, WINDOW )
      !>
      !>C
      !>C     Loop over the number of intervals in WINDOW, output
      !>C     the LEFT and RIGHT endpoints for each interval.
      !>C
      !>      DO I=1, WNCARD(WINDOW)
      !>
      !>         CALL WNFETD( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I,
      !>     .                  ' [', LEFT,',',  RIGHT, ']'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Interval 1 [   1.000,   3.000]
      !>        Interval 2 [   7.000,  11.000]
      !>        Interval 3 [  23.000,  27.000]
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated to remove unnecessary lines of code in the
      !>        Standard SPICE error handling CHKIN statements.
      !>
      !>        Edited the header to comply to NAIF standard. Created complete
      !>        code example from code fragment and added example's problem
      !>        statement.
      !>
      !>-    SPICELIB Version 1.0.1, 24-APR-2010 (EDW)
      !>
      !>        Minor edit to code comments eliminating typo.
      !>
      !>-    SPICELIB Version 1.0.0, 10-AUG-2007 (EDW)
      module integer(ip) function wncard ( ctx, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: window  ( lbcell:* )  !! Input window.


!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         wncard = 0
         return
      end if

      wncard = cardd( ctx,window)

!
!     Confirm the cardinality as an even integer.
!
      if( .not. (mod(wncard, 2) == 0) ) then

         call ctx%setmsg ( 'Invalid window size, a window should ' &
              // 'have an even number of elements. The size was #.')
         call ctx%errint ( '#', wncard )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         wncard = 0
         return

      end if

!
!     Set return value. Cardinality in a SPICE window sense
!     means the number of intervals, half the cell
!     cardinality value.
!
      wncard = wncard/2

      return
      end function wncard

      !> Determine the complement of a double precision window with
      !> respect to the interval [LEFT,RIGHT].
      !>
      !>## Detailed Input
      !>
      !>     LEFT,
      !>     RIGHT    are the left and right endpoints of the complement
      !>              interval.
      !>
      !>     WINDOW   is the window to be complemented.
      !>
      !>## Detailed Output
      !>
      !>     RESULT   is the output window, containing the complement
      !>              of WINDOW with respect to the interval from LEFT
      !>              to RIGHT. If the output window is not large enough
      !>              to contain the result, as many intervals as will
      !>              fit are returned.
      !>
      !>              RESULT must be distinct from WINDOW.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS)
      !>         is signaled.
      !>
      !>     2)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Mathematically, the complement of a window contains those
      !>     points that are not contained in the window. That is, the
      !>     complement of the set of closed intervals
      !>
      !>          [ a(1), b(1) ], [ a(2), b(2) ], ..., [ a(n), b(n) ]
      !>
      !>     is the set of open intervals
      !>
      !>          ( -inf, a(1) ), ( b(1), a(2) ), ..., ( b(n), +inf )
      !>
      !>     Because Fortran offers no satisfactory representation of
      !>     infinity, we must take the complement with respect to a
      !>     finite interval.
      !>
      !>     In addition, Fortran offers no satisfactory floating point
      !>     representation of open intervals. Therefore, the complement
      !>     of a floating point window is closure of the set theoretical
      !>     complement. In short, the floating point complement of the
      !>     window
      !>
      !>          [ a(1), b(1) ], [ a(2), b(2) ], ..., [ a(n), b(n) ]
      !>
      !>     with respect to the interval from LEFT to RIGHT is the
      !>     intersection of the windows
      !>
      !>          ( -inf, a(1) ], [ b(1), a(2) ], ..., [ b(n), +inf )
      !>
      !>     and
      !>
      !>          [ LEFT, RIGHT ]
      !>
      !>     Note that floating point intervals of measure zero (singleton
      !>     intervals) in the original window are replaced by gaps of
      !>     measure zero, which are filled. Thus, complementing a floating
      !>     point window twice does not necessarily yield the original
      !>     window.
      !>
      !>## Examples
      !>
      !>     Let WINDOW contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     Then the floating point complement of WINDOW with respect
      !>     to [2,20] contains the intervals
      !>
      !>           [ 3, 7 ]  [ 11, 20 ]
      !>
      !>     and the complement with respect to [ 0, 100 ] contains
      !>
      !>           [ 0, 1 ]  [ 3, 7 ]  [ 11, 23 ]  [ 27, 100 ]
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added entry #2 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) (NJB)
      module subroutine wncomd ( ctx, left, right, window, result )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: left  !! Left, right endpoints of complement interval.
      real(wp), intent(in) :: right  !! Left, right endpoints of complement interval.
      real(wp), intent(in) :: window   ( lbcell:* )  !! Input window.
      real(wp), intent(inout) :: result   ( lbcell:* )  !! Complement of WINDOW with respect to
                                                      !! [LEFT,RIGHT].


      integer(ip) :: card
      integer(ip) :: i

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     Get the cardinality of the input window.
!
      card = cardd ( ctx, window )

!
!     Empty out the result window before proceeding.
!
      call scardd ( ctx, 0, result )

!
!     Check to see if the input interval is valid. If it is not, signal
!     an error and return.
!
      if ( left > right ) then
         call ctx%setmsg ( 'WNCOMD: Left endpoint may not exceed right' // &
                       ' endpoint.' )
         call ctx%sigerr ( 'SPICE(BADENDPOINTS)' )
         return
      end if

!
!     There are two trivial cases: the window is empty, or it does not
!     intersect the input interval. In either case, the complement is
!     the entire interval.
!
      if (       card         == 0 &
           .or.  window(1)    >= right &
           .or.  window(card) <= left  ) then

         call wninsd ( ctx, left, right, result )

         return

      end if

!
!     Let WINDOW represent the set of intervals
!
!            [a1,b1], [a2,b2], ..., [aN,bN]
!
!     Then the closure of the complement of WINDOW in the reals is
!
!            (-infinity,a1], [b1,a2], [b2,a3], ..., [bN, infinity)
!
!     Thus the sequence of endpoints of WINDOW is also the sequence
!     of finite endpoints of its complement. Moreover, these endpoints
!     are simply "shifted" from their original positions in WINDOW.
!     This makes finding the complement of WINDOW with respect to
!     a given interval almost trivial.
!

!
!     Find the first right not less than the beginning of the input
!     interval.
!
      i = 2

      do while ( i <= card )
         if ( .not. ( window(i) < left ) ) exit
         i = i + 2
      end do

!
!     If the beginning of the input interval doesn't split an interval
!     in the input window, the complement begins with LEFT.
!
      if ( i <= card ) then
         if ( window(i-1) > left ) call wninsd ( ctx, left, window(i-1), result )
      end if

!
!     Start schlepping endpoints [b(i),a(i+1)] from the input window
!     to the output window. Stop when we find one of our new right
!     endpoints exceeds the end of the input interval.
!
      do while (  ( .not. ctx%failed() )  .and.  i < card )

         if ( .not. ( window(i+1) < right ) ) exit
         call wninsd ( ctx, window(i), window(i+1), result )
         i = i + 2

      end do

!
!     If the end of the input interval doesn't split an interval
!     in the input window, the complement ends with RIGHT.
!
      if ( i <= card ) then
         if ( window(i) < right ) call wninsd ( ctx, window(i), right, result )
      end if

      return
      end subroutine wncomd

      !> Contract each of the intervals of a double precision window.
      !>
      !>## Detailed Input
      !>
      !>     LEFT     is the amount to be added to the left endpoint of each
      !>              interval in the input window. The amount LEFT is signed.
      !>
      !>     RIGHT    is the amount to be subtracted from the right endpoint of
      !>              each interval in the window. The amount RIGHT is signed.
      !>
      !>     WINDOW   on input, is a window containing zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output, is the original window with each of its
      !>              intervals contracted by LEFT units on the left and
      !>              RIGHT units on the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine contracts (shortens) each of the intervals in
      !>     the input window. The adjustments are not necessarily symmetric.
      !>     That is, LEFT units are added to the left endpoint of each
      !>     interval, and RIGHT units are subtracted from the right endpoint
      !>     of each interval, where LEFT and RIGHT may be different.
      !>
      !>     Intervals are dropped when they are contracted by amounts
      !>     greater than their measures.
      !>
      !>## Examples
      !>
      !>     Let WINDOW contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
      !>
      !>     Then the following series of calls
      !>
      !>           CALL WNCOND (  2,  1, WINDOW )              (1)
      !>           CALL WNCOND ( -2,  2, WINDOW )              (2)
      !>           CALL WNCOND ( -2, -1, WINDOW )              (3)
      !>
      !>     produces the following series of windows
      !>
      !>           [ 9, 10 ]  [ 25, 26 ]                       (1)
      !>           [ 7,  8 ]  [ 23, 24 ]                       (2)
      !>           [ 5,  9 ]  [ 21, 25 ]                       (3)
      !>
      !>     Note that intervals may be "contracted" by negative amounts.
      !>     In the example above, the second call shifts each interval to
      !>     the left, while the third call undoes the effect of the first
      !>     call (without restoring the destroyed intervals).
      !>
      !>     Note also that the third call is exactly equivalent to the
      !>     call
      !>
      !>           CALL WNEXPD ( 2, 1, WINDOW )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added entry #1 in $Exceptions section. Extended LEFT and RIGHT
      !>        description in $Detailed_Input.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      module subroutine wncond ( ctx, left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: left  !! Amount added to each left endpoint.
      real(wp), intent(in) :: right  !! Amount subtracted from each right endpoint.
      real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be contracted.


!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     This is just negative expansion.
!
      call wnexpd ( ctx, -left, -right, window )

      return
      end subroutine wncond

      !> Place the difference of two double precision windows into
      !> a third window.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are SPICE windows, each of which contains zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     C        is the output SPICE window, containing the difference
      !>              of A and B --- every point contained in A, but not
      !>              contained in B.
      !>
      !>              C must be distinct from both A and B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the difference of the two windows results in an excess of
      !>         elements, the error SPICE(WINDOWEXCESS) is signaled.
      !>
      !>     2)  The cardinality of the input windows must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Mathematically, the difference of two windows contains every
      !>     point contained in the first window but not contained in the
      !>     second window.
      !>
      !>     Fortran offers no satisfactory floating point representation
      !>     of open intervals. Thus, for floating point windows we must
      !>     return the closure of the set theoretical difference: that is,
      !>     the difference plus the endpoints of the first window that are
      !>     contained in the second window.
      !>
      !>## Examples
      !>
      !>     Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     and B contain the intervals
      !>
      !>           [ 2, 4 ]  [ 8, 10 ]  [ 16, 18 ]
      !>
      !>     Then the difference of A and B contains the intervals
      !>
      !>           [ 1, 2 ]  [ 7, 8 ]  [ 10, 11 ]  [ 23, 27 ]
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of argument C in $Brief_I/O table.
      !>
      !>        Added entry #2 in $Exceptions section.
      !>
      !>-    SPICELIB Version 2.0.0, 16-SEP-1998 (WLT)
      !>
      !>        The previous version did not work when removing
      !>        singletons. This has been corrected.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 27-FEB-1989  (HAN)
      !>
      !>        Due to the calling sequence and functionality changes
      !>        in the routine EXCESS, the method of signaling an
      !>        excess of elements needed to be changed.
      module subroutine wndifd ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a        ( lbcell:* )  !! Input windows.
      real(wp), intent(in) :: b        ( lbcell:* )  !! Input windows.
      real(wp), intent(inout) :: c        ( lbcell:* )  !! Difference of A and B.


      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: csize

      integer(ip) :: apb
      integer(ip) :: ape
      integer(ip) :: bpb
      integer(ip) :: bpe
      integer(ip) :: put
      real(wp) :: f
      real(wp) :: l
      integer(ip) :: needed

      integer(ip) :: over

      logical :: keep
      logical :: unrslv

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window. Also, save the size of the second window.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )
      over  = 0

!
!     Empty out the output window.
!
      call ssized ( ctx, csize, c )

!
!     Let's handle the pathological cases first.
!
      if ( bcard == 0 ) then
         call copyd ( ctx, a, c )
         return
      else if ( acard == 0 ) then
         return
      end if

!
!     Now get pointers to the first intervals of A and B.
!
      apb = 1
      ape = 2
      bpb = 1
      bpe = 2
      put = 1
!
!     As long as the end pointer for A is less than the cardinality
!     of A we need to examine intervals and decide how much of
!     them to keep in C.
!
      do while ( ape <= acard )
!
!        We will work with the interval [F,L] which starts out
!        as the next interval of A.  We modify it below as required
!        when subtracting out intervals of B.
!
         f      = a(apb)
         l      = a(ape)
!
!        Right now we have not resolved whether to keep the interval
!        [F,L], but until we know better we assume it is a keeper.
!
         unrslv =  bpe <= bcard
         keep   = .true.

         do while ( unrslv )

            if ( l < b(bpb) ) then
!
!              The interval [F,L] is before the next interval of B, we
!              have resolved what to do with this one.   It is a
!              keeper.
!
               unrslv = .false.

            else if ( f > b(bpe) ) then
!
!              [F,L] is after the end of the current interval in B,
!              we need to look at the next interval of B
!
               bpb    = bpb + 2
               bpe    = bpe + 2
               unrslv = bpe <= bcard

            else
!
!              There is some overlap between the current interval
!              of B and the current interval of A. There are
!              several possibilities
!
!              1) The current interval of A is contained in the
!                 current interval of B (This includes singleton
!                 intervals in A). We just mark [F,L] so that it
!                 won't be kept.  We have fully resolved what to
!                 do with [F,L].
!
!              2) The interval from B overlaps at the beginning
!                 of the interval of A
!
!                 B interval [......]
!                 A interval     [............]
!                 result of A-B     [.........]
!
!                 In this case we need to shrink the interval [F,L]
!                 but we have not resolved how much of the result
!                 to keep.
!
!              3) The interval from B falls inside the current
!                 interval [F,L]
!
!                 B interval        [......]
!                 A interval     [............]
!                 result of A-B  [..]      [..]
!
!                 If the interval from B is not a singleton, we store
!                 the first part of [F,L] in C and then set [F,L] to
!                 be the right interval which is still not resolved.
!
!                 If the B interval is a singleton we can ignore ignore
!                 it.  But we have not resolved what to do about
!                 [F,L], we need to look at the next interval of B.
!

!
!              4) The interval from B overlaps at the ending
!                 of the interval of A
!
!                 B interval          [......]
!                 A interval     [......]
!                 result of A-B  [....]
!
!                 We need to shrink [F,L]. In this case we know we can
!                 keep all of what's left because all other intervals
!                 of B are to the right of [F,L]
!
               if (       b(bpb) <= f &
                    .and. l      <= b(bpe) ) then
!
!                 Case 1 above
!
                  keep   = .false.
                  unrslv = .false.

               else if ( b(bpb) <= f ) then
!
!                 Case 2 above
!
                  f      = b(bpe)
                  bpb    = bpb+2
                  bpe    = bpe+2
                  unrslv = bpe <= bcard

               else if (      f      <= b(bpb) &
                        .and. l      >= b(bpe) &
                        .and. b(bpb) < b(bpe) ) then
!
!                 Case 3 above (non-singleton interval of B).
!

                  if ( put < csize ) then
                     c(put)   = f
                     c(put+1) = b(bpb)

                     call scardd ( ctx, put+1, c )

                     put      = put + 2
                  else
                     over = over + 2
                  end if

                  f      = b(bpe)
!
!                 If the interval from B contained L, we will not
!                 want to be keeping the singleton [F,L].
!
                  if ( f == l ) then
                     keep   = .false.
                     unrslv = .false.
                  end if

                  bpb    = bpb + 2
                  bpe    = bpe + 2
                  unrslv = unrslv .and. bpe <= bcard

               else if (      f      <= b(bpb) &
                        .and. l      >= b(bpe) &
                        .and. b(bpb) == b(bpe) ) then
!
!                 Case 3 above (singleton interval of B).
!
                  bpb    = bpb + 2
                  bpe    = bpe + 2
                  unrslv = bpe <= bcard

               else
!
!                 Case 4 above
!
                  l      =  b(bpb)
                  unrslv = .false.

               end if

            end if

         end do

!
!        If there is anything to keep in C, put it there.
!
         if ( keep ) then
!
!           Make sure there is sufficient room to do the putting.
!
            if ( put < csize ) then
               c(put)   = f
               c(put+1) = l
               call scardd ( ctx, put+1, c )
               put      = put + 2
            else
               over = over + 2
            end if

         end if
!
!        Move the pointers in A to the next interval.
!
         apb = apb + 2
         ape = ape + 2

      end do

!
!     We've examined all of the intervals of A and B, but if we
!     didn't actually store all of the difference, signal an error.
!
      if ( over > 0 ) then

         needed = over + csize

         call ctx%setmsg ( 'The output window did not have ' &
         //            'sufficient room to contain the result of ' &
         //            'the window difference.  It has room for ' &
         //            '# endpoints, but # were needed to ' &
         //            'describe the difference. ' )

         call ctx%errint ( '#', csize  )
         call ctx%errint ( '#', needed )
         call ctx%sigerr ( 'SPICE(WINDOWEXCESS)'  )

      end if

      return
      end subroutine wndifd

      !> Determine whether a point is an element of a double precision
      !> window.
      !>
      !> The function returns .TRUE. if POINT is an element of WINDOW.
      !>
      !>## Detailed Input
      !>
      !>     POINT    is a point, which may or may not be contained in one of
      !>              the intervals in WINDOW.
      !>
      !>     WINDOW   is a SPICE window containing zero or more intervals.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the input point is an element of
      !>     the input window --- that is, if
      !>
      !>        a(i)  <  POINT  <  b(i)
      !>              -         -
      !>
      !>     for some interval [ a(i), b(i) ] in WINDOW --- and returns .FALSE.
      !>     otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
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
      !>     Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     Then the following expressions are true
      !>
      !>           WNELMD ( 1, WINDOW )
      !>           WNELMD ( 9, WINDOW )
      !>
      !>     and the following expressions are false.
      !>
      !>           WNELMD (  0, WINDOW )
      !>           WNELMD ( 13, WINDOW )
      !>           WNELMD ( 29, WINDOW )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 25-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>        Removed unnecessary $Revisions section.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      module logical function wnelmd ( ctx, point, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: point  !! Input point.
      real(wp), intent(in) :: window      ( lbcell:* )  !! Input window.


      integer(ip) :: card
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         wnelmd = .false.
         return
      end if

!
!     How many endpoints in the window?
!
      card = cardd ( ctx, window )

!
!     Check the point against every interval in the window. Quit if
!     we find an interval that contains it. Inefficient, but it works.
!
      do i = 1, card, 2

         if ( point >= window(i)  .and. point <= window(i+1) ) then
            wnelmd = .true.
            return
         end if

      end do

      wnelmd = .false.

      return
      end function wnelmd

      !> Expand each of the intervals of a double precision window.
      !>
      !>## Detailed Input
      !>
      !>     LEFT     is the amount to be subtracted from the left endpoint of
      !>              each interval in the input window. The amount LEFT is
      !>              signed.
      !>
      !>     RIGHT    is the amount to be added to the right endpoint of each
      !>              interval in the window. The amount RIGHT is signed.
      !>
      !>     WINDOW   on input, is a window containing zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output, is the original window with each of its
      !>              intervals expanded by LEFT units on the left and
      !>              RIGHT units on the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine expands (lengthens) each of the intervals in
      !>     the input window. The adjustments are not necessarily symmetric.
      !>     That is, LEFT units are subtracted from the left endpoint of
      !>     each interval, and RIGHT units are added to the right endpoint
      !>     of each interval, where LEFT and RIGHT may be different.
      !>
      !>     Intervals are merged when expansion causes them to overlap.
      !>
      !>## Examples
      !>
      !>     Let WINDOW contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
      !>
      !>     Then the following series of calls
      !>
      !>           CALL WNEXPD (  2,  1, WINDOW )              (1)
      !>           CALL WNEXPD ( -2,  2, WINDOW )              (2)
      !>           CALL WNEXPD ( -2, -1, WINDOW )              (3)
      !>
      !>     produces the following series of windows
      !>
      !>           [ -1, 4 ]  [ 5, 12 ]  [ 21, 30 ]            (1)
      !>           [  1, 6 ]  [ 7, 14 ]  [ 23, 32 ]            (2)
      !>           [  3, 5 ]  [ 9, 13 ]  [ 25, 31 ]            (3)
      !>
      !>     Note that intervals may be "expanded" by negative amounts.
      !>     In the example above, the second call shifts each interval to
      !>     the right, while the third call undoes the effect of the first
      !>     call (without restoring the merged intervals).
      !>
      !>     Note also that the third call is exactly equivalent to the
      !>     call
      !>
      !>           CALL WNCOND ( 2, 1, WINDOW )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added entry #1 in $Exceptions section. Extended LEFT and RIGHT
      !>        description in $Detailed_Input.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      module subroutine wnexpd ( ctx, left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: left  !! Amount subtracted from each left endpoint.
      real(wp), intent(in) :: right  !! Amount added to each right endpoint.
      real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be expanded.


      integer(ip) :: card
      integer(ip) :: gone
      integer(ip) :: i
      integer(ip) :: j

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the cardinality of the window. (The size is not important;
!     this routine can't create any new intervals.)
!
      card = cardd ( ctx, window )

!
!     Expand the intervals individually. We'll take care of
!     overlaps later on. Negative expansion may cause some
!     intervals to disappear.
!
      gone = 0

      do i = 1, card, 2
         window(i-gone  ) = window(i)   - left
         window(i-gone+1) = window(i+1) + right

         if ( window(i-gone) > window(i-gone+1) ) then
            gone = gone + 2
         end if
      end do

!
!     Proceed only if at least one interval remains. (If there were
!     no intervals to begin with, we skip the previous loop and come
!     here without delay. Do not pass GO, do not collect $200.)
!
      card = card - gone

      if ( card == 0 ) then
         call scardd ( ctx, 0, window )
         return
      end if

!
!     None of the intervals can have extended to completely contain
!     any of the other intervals. (They were all expanded by the
!     same amount. Convince yourself that this is true.) So the first
!     endpoint is still the first endpoint (so to speak).
!
!     Step through the window, looking for the next right endpoint
!     less than the following left endpoint. This marks the end of
!     the new first interval, and the beginning of the new second
!     interval. Keep this up until the last right endpoint has been
!     reached. This remains the last right endpoint.
!
      i = 2
      j = 2

      do while ( j < card )

         if ( window(j) < window(j+1) ) then
            window(i  ) = window(j  )
            window(i+1) = window(j+1)
            i           = i + 2
         end if

         j = j + 2

      end do

      window(i) = window(j)
      call scardd ( ctx, i, window )

      return
      end subroutine wnexpd

      !> Extract the left or right endpoints from a double precision
      !> window.
      !>
      !>## Detailed Input
      !>
      !>     SIDE     indicates whether the left or right endpoints of the
      !>              intervals in the window are to be extracted.
      !>
      !>                 'L', 'l'       Left endpoints.
      !>                 'R', 'r'       Right endpoints.
      !>
      !>              If SIDE is not recognized, the input window is not
      !>              changed.
      !>
      !>     WINDOW   on input, is a window containing zero or more intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output, is the collection of singleton intervals
      !>              containing either the left or the right endpoints of the
      !>              intervals in the original window.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the endpoint specification, SIDE, is not recognized, the
      !>         error SPICE(INVALIDENDPNTSPEC) is signaled.
      !>
      !>     2)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine replaces every interval in the input window with
      !>     the singleton interval containing one of the endpoints of the
      !>     interval.
      !>
      !>## Examples
      !>
      !>     Let WINDOW contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
      !>
      !>     Then the call
      !>
      !>           CALL WNEXTD (  'L', WINDOW )
      !>
      !>     produces the window
      !>
      !>           [ 1, 1 ]  [ 7, 7 ]  [ 23, 23 ]  [ 29, 29 ]
      !>
      !>     And the call
      !>
      !>           CALL WNEXTD ( 'R', WINDOW )
      !>
      !>     produces the window
      !>
      !>           [ 3, 3 ]  [ 11, 11 ]  [ 27, 27 ]  [ 29, 29 ]
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-MAR-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #2
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.2.0, 24-FEB-1989  (HAN)
      !>
      !>        Added calls to CHKIN and CHKOUT. Error handling was added to
      !>        detect invalid endpoint specification. The previous version
      !>        did not signal an error if SIDE was not 'R', 'r', 'L', or 'l'.
      module subroutine wnextd ( ctx, side, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      character(1), intent(in) :: side  !! Extract left ('L') or right ('R') endpoints.
      real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be extracted.


      integer(ip) :: card
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the cardinality of the window. (The size is not important;
!     this routine can't create any new intervals.)
!
      card = cardd ( ctx, window )

!
!     Step through the window, keeping one endpoint from each interval.
!     For the sake of efficiency, we have separate loops for the two
!     possible values of SIDE.
!
      if ( side == 'L'  .or.  side == 'l' ) then

         do i = 1, card, 2
            window(i+1) = window(i)
         end do

      else if ( side == 'R'  .or.  side == 'r' ) then

         do i = 1, card, 2
            window(i) = window(i+1)
         end do

      else

         call ctx%setmsg ( 'SIDE was *.' )
         call ctx%errch  ( '*', side     )
         call ctx%sigerr ( 'SPICE(INVALIDENDPNTSPEC)' )

      end if

      return
      end subroutine wnextd

      !> Fetch a particular interval from a double precision window.
      !>
      !>## Detailed Input
      !>
      !>     WINDOW   is a window containing zero or more intervals.
      !>
      !>     N        is the index of a particular interval within the window.
      !>              Indices range from 1 to NINT, where NINT is the number of
      !>              intervals in the window (CARDD(WINDOW)/2).
      !>
      !>## Detailed Output
      !>
      !>     LEFT,
      !>     RIGHT    are the left and right endpoints of the N'th interval in
      !>              the input window. If the interval is not found, LEFT and
      !>              RIGHT are not defined.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If N is less than one, the error SPICE(NOINTERVAL) is
      !>         signaled.
      !>
      !>     2)  If the interval does not exist, i.e. N > CARDD(WINDOW)/2, the
      !>         error SPICE(NOINTERVAL) is signaled.
      !>
      !>     3)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
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
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) The following code example demonstrates how to insert an
      !>        interval into an existing double precision SPICE window, and
      !>        how to loop over all its intervals to extract their left and
      !>        right points.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNFETD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WNSIZE
      !>      PARAMETER           ( WNSIZE = 10 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
      !>      DOUBLE PRECISION      LEFT
      !>      DOUBLE PRECISION      RIGHT
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Validate the window with size WNSIZE and zero elements.
      !>C
      !>      CALL WNVALD( WNSIZE, 0, WINDOW )
      !>
      !>C
      !>C     Insert the intervals
      !>C
      !>C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>C
      !>C     into WINDOW.
      !>C
      !>      CALL WNINSD(  1.D0,  3.D0, WINDOW )
      !>      CALL WNINSD(  7.D0, 11.D0, WINDOW )
      !>      CALL WNINSD( 23.D0, 27.D0, WINDOW )
      !>
      !>C
      !>C     Loop over the number of intervals in WINDOW, output
      !>C     the LEFT and RIGHT endpoints for each interval.
      !>C
      !>      DO I=1, WNCARD(WINDOW)
      !>
      !>         CALL WNFETD( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I,
      !>     .                  ' [', LEFT,',',  RIGHT, ']'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Interval 1 [   1.000,   3.000]
      !>        Interval 2 [   7.000,  11.000]
      !>        Interval 3 [  23.000,  27.000]
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 15-MAR-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example, problem statement and solution. Added entry #3 in
      !>        $Exceptions section.
      !>
      !>        Improved description of argument N in $Detailed_Input.
      !>
      !>-    SPICELIB Version 1.0.2, 30-JUL-2007 (EDW)
      !>
      !>        Removed erroneous description in the $Examples section
      !>        indicating "Undefined" as a return state after an error
      !>        event caused by an invalid value of N.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module subroutine wnfetd ( ctx, window, n, left, right )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: window      ( lbcell:* )  !! Input window.
      integer(ip), intent(in) :: n  !! Index of interval to be fetched.
      real(wp), intent(out) :: left  !! Left, right endpoints of the Nth interval.
      real(wp), intent(out) :: right  !! Left, right endpoints of the Nth interval.


      integer(ip) :: card
      integer(ip) :: end

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     How many endpoints in the window? Enough? Normally, endpoints
!     of the Nth interval are stored in elements 2N and 2N-1.
!
      card = cardd ( ctx, window )
      end  = 2*n

      if ( n < 1  .or.  card < end ) then
         call ctx%setmsg ( 'WNFETD: No such interval.' )
         call ctx%sigerr ( 'SPICE(NOINTERVAL)' )
      else
         left  = window(end-1)
         right = window(end  )
      end if

      return
      end subroutine wnfetd

      !> Fill small gaps between adjacent intervals of a double precision
      !> window.
      !>
      !>## Detailed Input
      !>
      !>     SMLGAP   is the limiting measure of the small gaps to be filled.
      !>              Adjacent intervals separated by gaps of measure less than
      !>              or equal to SMLGAP are merged. The measure SMLGAP is
      !>              signed, and is used as is---the absolute value of SMLGAP
      !>              is not used for in place of negative input values.
      !>
      !>     WINDOW   on input, is a window containing zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output, is the original window, after adjacent
      !>              intervals separated by small gaps have been merged.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>     2)  If SMLGAP is less than or equal to zero, this routine has
      !>         no effect on the window.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine removes small gaps between adjacent intervals
      !>     by merging intervals separated by gaps of measure less than
      !>     or equal to the limiting measure SMLGAP.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Given a double precision window, containing the following four
      !>        intervals:
      !>
      !>           [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]
      !>
      !>        merge any adjacent intervals separated by a gap equal to or
      !>        less than 3.0.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNFILD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(A,I2,A,2(F7.3,A))' )
      !>
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WNSIZE
      !>      PARAMETER           ( WNSIZE = 10 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      LEFT
      !>      DOUBLE PRECISION      RIGHT
      !>      DOUBLE PRECISION      WINDOW ( LBCELL:WNSIZE )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Validate the WINDOW with size WNSIZE and zero elements.
      !>C
      !>      CALL WNVALD ( WNSIZE, 0, WINDOW )
      !>
      !>C
      !>C     Insert the intervals
      !>C
      !>C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
      !>C
      !>C     into WINDOW.
      !>C
      !>      CALL WNINSD ( 1.0D0,  3.0D0,  WINDOW )
      !>      CALL WNINSD ( 7.0D0,  11.0D0, WINDOW )
      !>      CALL WNINSD ( 23.0D0, 27.0D0, WINDOW )
      !>      CALL WNINSD ( 29.0D0, 29.0D0, WINDOW )
      !>
      !>C
      !>C     Loop over the number of intervals in WINDOW, output
      !>C     the LEFT and RIGHT endpoints for each interval.
      !>C
      !>      WRITE(*,*) 'Initial WINDOW:'
      !>
      !>      DO I= 1, WNCARD( WINDOW )
      !>
      !>         CALL WNFETD ( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,FMT) '   Interval ', I, ': [',
      !>     .                LEFT, ',', RIGHT, ' ]'
      !>
      !>      END DO
      !>
      !>C
      !>C     Fill the gaps smaller than or equal to 3.0
      !>C
      !>      CALL WNFILD ( 3.0D0, WINDOW )
      !>
      !>C
      !>C     Output the intervals.
      !>C
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Window after filling gaps <= 3.0:'
      !>
      !>      DO I= 1, WNCARD( WINDOW )
      !>
      !>         CALL WNFETD ( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,FMT) '   Interval ', I, ': [',
      !>     .                LEFT, ',', RIGHT, ' ]'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Initial WINDOW:
      !>           Interval  1: [  1.000,  3.000 ]
      !>           Interval  2: [  7.000, 11.000 ]
      !>           Interval  3: [ 23.000, 27.000 ]
      !>           Interval  4: [ 29.000, 29.000 ]
      !>
      !>         Window after filling gaps <= 3.0:
      !>           Interval  1: [  1.000,  3.000 ]
      !>           Interval  2: [  7.000, 11.000 ]
      !>           Interval  3: [ 23.000, 29.000 ]
      !>
      !>     2) Using the same window from the first example:
      !>
      !>           [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]
      !>
      !>        Then the following series of calls
      !>
      !>           CALL WNFILD (  1.D0, WINDOW )                           (1)
      !>           CALL WNFILD (  2.D0, WINDOW )                           (2)
      !>           CALL WNFILD (  3.D0, WINDOW )                           (3)
      !>           CALL WNFILD ( 12.D0, WINDOW )                           (4)
      !>
      !>        produces the following series of windows
      !>
      !>           [1.0,  3.0]  [7.0, 11.0]  [23.0, 27.0]  [29.0, 29.0]    (1)
      !>           [1.0,  3.0]  [7.0, 11.0]  [23.0, 29.0]                  (2)
      !>           [1.0,  3.0]  [7.0, 11.0]  [23.0, 29.0]                  (3)
      !>           [1.0, 29.0]                                             (4)
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) (NJB)
      !>
      !>        Changed the argument name SMALL to SMLGAP for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example. Added entry #1 and #2 in
      !>        $Exceptions section. Extended SMLGAP description in
      !>        $Detailed_Input.
      !>
      !>        Updated code to remove unnecessary lines of code in the
      !>        Standard SPICE error handling CHKIN statements.
      !>
      !>-    SPICELIB Version 1.0.3, 29-JUL-2007 (NJB)
      !>
      !>        Corrected typo in the previous Version line date string,
      !>        "29-JUL-20022" to "29-JUL-2002."
      !>
      !>-    SPICELIB Version 1.0.2, 29-JUL-2002 (NJB)
      !>
      !>        Changed gap size from 10 to 12 to correct erroneous example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      module subroutine wnfild ( ctx, smlgap, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: smlgap  !! Limiting measure of small gaps.
      real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be filled.


      integer(ip) :: card
      integer(ip) :: i
      integer(ip) :: j

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the cardinality of the window. (The size is not important;
!     this routine can't create any new intervals.)
!
      card = cardd ( ctx, window )

!
!     Step through the window, looking for the next right endpoint
!     more than SMLGAP away from the following left endpoint. This marks
!     the end of the new first interval, and the beginning of the new
!     second interval. Keep this up until the last right endpoint has
!     been reached. This remains the last right endpoint.
!
      if ( card > 0 ) then

         i = 2
         j = 2

         do while ( j < card )

            if ( ( window(j) + smlgap )  < window(j+1) ) then
               window(i  ) = window(j  )
               window(i+1) = window(j+1)
               i           = i + 2
            end if

            j = j + 2

         end do

         window(i) = window(j)
         call scardd ( ctx, i, window )

      end if

      return
      end subroutine wnfild

      !> Filter (remove) small intervals from a double precision window.
      !>
      !>## Detailed Input
      !>
      !>     SMLINT   is the limiting measure of the small intervals to be
      !>              filtered. Intervals of measure less than or equal to
      !>              SMLINT are removed from the window. The measure SMLINT is
      !>              signed, and is used as is---the absolute value of SMLINT
      !>              is not used for in place of negative input values.
      !>
      !>     WINDOW   on input, is a window containing zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output, is the original window, after small
      !>              intervals have been removed.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>     2)  If SMLINT is less than or equal to zero, this routine has
      !>         no effect on the window.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine removes from the input window every interval with
      !>     measure less than or equal to the limiting measure (SMLINT).
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Given a double precision window, containing the following four
      !>        intervals:
      !>
      !>           [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]
      !>
      !>        remove any intervals equal to or less than 3.0.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNFLTD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT    = '(A,I2,A,2(F7.3,A))' )
      !>
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WNSIZE
      !>      PARAMETER           ( WNSIZE = 10 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      LEFT
      !>      DOUBLE PRECISION      RIGHT
      !>      DOUBLE PRECISION      WINDOW ( LBCELL:WNSIZE )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Validate the WINDOW with size WNSIZE and zero elements.
      !>C
      !>      CALL WNVALD ( WNSIZE, 0, WINDOW )
      !>
      !>C
      !>C     Insert the intervals
      !>C
      !>C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
      !>C
      !>C     into WINDOW.
      !>C
      !>      CALL WNINSD (  1.0D0,  3.0D0, WINDOW )
      !>      CALL WNINSD (  7.0D0, 11.0D0, WINDOW )
      !>      CALL WNINSD ( 23.0D0, 27.0D0, WINDOW )
      !>      CALL WNINSD ( 29.0D0, 29.0D0, WINDOW )
      !>
      !>C
      !>C     Loop over the number of intervals in WINDOW, output
      !>C     the LEFT and RIGHT endpoints for each interval.
      !>C
      !>      WRITE(*,*) 'Initial WINDOW:'
      !>
      !>      DO I= 1, WNCARD( WINDOW )
      !>
      !>         CALL WNFETD ( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,FMT) '   Interval ', I, ': [',
      !>     .                LEFT, ',', RIGHT, ' ]'
      !>
      !>      END DO
      !>
      !>C
      !>C     Filter the intervals smaller than or equal to 3.0
      !>C
      !>      CALL WNFLTD ( 3.0D0, WINDOW )
      !>
      !>C
      !>C     Output the intervals.
      !>C
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Window after filtering intervals <= 3.0:'
      !>
      !>      DO I= 1, WNCARD( WINDOW )
      !>
      !>         CALL WNFETD ( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,FMT) '   Interval ', I, ': [',
      !>     .                LEFT, ',', RIGHT, ' ]'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Initial WINDOW:
      !>           Interval  1: [  1.000,  3.000 ]
      !>           Interval  2: [  7.000, 11.000 ]
      !>           Interval  3: [ 23.000, 27.000 ]
      !>           Interval  4: [ 29.000, 29.000 ]
      !>
      !>         Window after filtering intervals <= 3.0:
      !>           Interval  1: [  7.000, 11.000 ]
      !>           Interval  2: [ 23.000, 27.000 ]
      !>
      !>     2) Using the same window from the first example:
      !>
      !>           [ 1.0, 3.0 ], [ 7.0, 11.0 ], [ 23.0, 27.0 ], [ 29.0, 29.0 ]
      !>
      !>        Then the following series of calls
      !>
      !>           CALL WNFLTD (  0.D0, WINDOW )                           (1)
      !>           CALL WNFLTD (  2.D0, WINDOW )                           (2)
      !>           CALL WNFLTD (  3.D0, WINDOW )                           (3)
      !>
      !>        produces the following series of windows
      !>
      !>           [ 1.0, 3.0 ]   [ 7.0, 11.0 ]  [ 23.0, 27.0 ]            (1)
      !>                          [ 7.0, 11.0 ]  [ 23.0, 27.0 ]            (2)
      !>                          [ 7.0, 11.0 ]  [ 23.0, 27.0 ]            (3)
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) (NJB)
      !>
      !>        Changed the argument name SMALL to SMLINT for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example. Added entry #1 and #2 in
      !>        $Exceptions section. Extended SMLINT description in
      !>        $Detailed_Input.
      !>
      !>        Updated code to remove unnecessary lines of code in the
      !>        Standard SPICE error handling CHKIN statements.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      module subroutine wnfltd ( ctx, smlint, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: smlint  !! Limiting measure of small intervals.
      real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be filtered.


      integer(ip) :: card
      integer(ip) :: i
      integer(ip) :: j

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the cardinality of the window. (The size is not important;
!     this routine can't create any new intervals.)
!
      card = cardd ( ctx, window )

!
!     Step through the window, looking for the next interval big
!     enough to get stuck in the filter. Keep this up until the last
!     interval has been checked.
!
      i = 0
      j = 2

      do while ( j <= card )

         if ( ( window(j) - window(j-1) ) > smlint ) then

            i           = i + 2
            window(i-1) = window(j-1)
            window(i  ) = window(j  )

         end if

         j = j + 2

      end do

      call scardd ( ctx, i, window )

      return
      end subroutine wnfltd

      !> Determine whether an interval is included in a double precision
      !> window.
      !>
      !> The function returns .TRUE. if POINT is an element of WINDOW.
      !>
      !>## Detailed Input
      !>
      !>     LEFT,
      !>     RIGHT    are the endpoints of an interval, which may or may not be
      !>              contained in one of the intervals in WINDOW.
      !>
      !>     WINDOW   is a window containing zero or more intervals.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the input interval is included in
      !>     the input window --- that is, if
      !>
      !>        a(i)  <  LEFT  <  RIGHT  <  b(i)
      !>              -        -         -
      !>
      !>     for some interval [ a(i), b(i) ] in WINDOW --- and is .FALSE.
      !>     otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>     2)  The order of the input interval's endpoints, LEFT and RIGHT,
      !>         is not checked, and that this does not affect the result.
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
      !>     Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     Then the following expressions are true
      !>
      !>           WNINCD ( 1.D0,  3.D0, WINDOW )
      !>           WNINCD ( 9.D0, 10.D0, WINDOW )
      !>
      !>     and the following expressions are false.
      !>
      !>           WNINCD (  0,  2, WINDOW )
      !>           WNINCD ( 13, 15, WINDOW )
      !>           WNINCD ( 29, 30, WINDOW )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 14-MAR-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entries
      !>        #1 and #2 in $Exceptions section.
      !>
      !>        Removed unnecessary $Revisions section.
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
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      module logical function wnincd ( ctx, left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: left  !! Input interval.
      real(wp), intent(in) :: right  !! Input interval.
      real(wp), intent(in) :: window      ( lbcell:* )  !! Input window.


      integer(ip) :: card
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         wnincd = .false.
         return
      end if

!
!     How many endpoints in the window?
!
      card = cardd ( ctx, window )

!
!     Check this interval agains every interval in the window.
!     Inefficient, but foolproof.
!
      do i = 1, card, 2

         if ( left >= window(i)  .and.  right <= window(i+1) ) then
            wnincd = .true.
            return
         end if

      end do

      wnincd = .false.

      return
      end function wnincd

      !> Insert an interval into a double precision window.
      !>
      !>## Detailed Input
      !>
      !>     LEFT,
      !>     RIGHT    are the left and right endpoints of the interval to be
      !>              inserted.
      !>
      !>     WINDOW   on input, is a SPICE window containing zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output, is the original window following the insertion
      !>              of the interval from LEFT to RIGHT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS)
      !>         is signaled.
      !>
      !>     2)  If the insertion of the interval causes an excess of elements,
      !>         the error SPICE(WINDOWEXCESS) is signaled.
      !>
      !>     3)  The cardinality of the input WINDOW must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine inserts the interval from LEFT to RIGHT into the
      !>     input window. If the new interval overlaps any of the intervals
      !>     in the window, the intervals are merged. Thus, the cardinality
      !>     of the input window can actually decrease as the result of an
      !>     insertion. However, because inserting an interval that is
      !>     disjoint from the other intervals in the window can increase the
      !>     cardinality of the window, the routine signals an error.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) The following code example demonstrates how to insert an
      !>        interval into an existing double precision SPICE window, and
      !>        how to loop over all its intervals to extract their left and
      !>        right points.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNINSD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WNSIZE
      !>      PARAMETER           ( WNSIZE = 10 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE )
      !>      DOUBLE PRECISION      LEFT
      !>      DOUBLE PRECISION      RIGHT
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Validate the window with size WNSIZE and zero elements.
      !>C
      !>      CALL WNVALD( WNSIZE, 0, WINDOW )
      !>
      !>C
      !>C     Insert the intervals
      !>C
      !>C        [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>C
      !>C     into WINDOW.
      !>C
      !>      CALL WNINSD(  1.D0,  3.D0, WINDOW )
      !>      CALL WNINSD(  7.D0, 11.D0, WINDOW )
      !>      CALL WNINSD( 23.D0, 27.D0, WINDOW )
      !>
      !>C
      !>C     Loop over the number of intervals in WINDOW, output
      !>C     the LEFT and RIGHT endpoints for each interval.
      !>C
      !>      DO I=1, WNCARD(WINDOW)
      !>
      !>         CALL WNFETD( WINDOW, I, LEFT, RIGHT )
      !>
      !>         WRITE(*,'(A,I2,2(A,F8.3),A)') 'Interval', I,
      !>     .                  ' [', LEFT,',',  RIGHT, ']'
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Interval 1 [   1.000,   3.000]
      !>        Interval 2 [   7.000,  11.000]
      !>        Interval 3 [  23.000,  27.000]
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.0, 25-AUG-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated to remove unnecessary lines of code in the
      !>        Standard SPICE error handling CHKIN statements.
      !>
      !>        Edited the header to comply to NAIF standard. Added complete
      !>        code example, problem statement and solution. Added entry #3 in
      !>        $Exceptions section.
      !>
      !>        Removed irrelevant information related to other unary window
      !>        routines from $Particulars section.
      !>
      !>-    SPICELIB Version 1.3.0, 04-MAR-1993 (KRG)
      !>
      !>        There was a bug when moving the intervals in the cell
      !>        to the right when inserting a new interval to the left
      !>        of the left most interval. The incrementing in the DO
      !>        loop was incorrect.
      !>
      !>        The loop used to read:
      !>
      !>           DO J = I-1, CARD
      !>              WINDOW(J+2) = WINDOW(J)
      !>           END DO
      !>
      !>        which squashed everything to the right of the first interval
      !>        with the values of the first interval.
      !>
      !>        The loop now reads:
      !>
      !>           DO J = CARD, I-1, -1
      !>              WINDOW(J+2) = WINDOW(J)
      !>           END DO
      !>
      !>        which correctly scoots the elements in reverse order,
      !>        preserving their values.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.3.0, 04-MAR-1993 (KRG)
      !>
      !>        There was a bug when moving the intervals in the cell
      !>        to the right when inserting a new interval to the left
      !>        of the left most interval. the incrementing in the DO
      !>        loop was incorrect.
      !>
      !>        The loop used to read:
      !>
      !>           DO J = I-1, CARD
      !>              WINDOW(J+2) = WINDOW(J)
      !>           END DO
      !>
      !>        which squashed everything to the right of the first interval
      !>        with the values of the first interval.
      !>
      !>        The loop now reads:
      !>
      !>           DO J = CARD, I-1, -1
      !>              WINDOW(J+2) = WINDOW(J)
      !>           END DO
      !>
      !>        which correctly scoots the elements in reverse order,
      !>        preserving their values.
      !>
      !>-    Beta Version 1.2.0, 27-FEB-1989 (HAN)
      !>
      !>        Due to the calling sequence and functionality changes
      !>        in the routine EXCESS, the method of signaling an
      !>        excess of elements needed to be changed.
      !>
      !>-    Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
      !>
      !>        Contents of the $Required_Reading section was
      !>        changed from "None." to "WINDOWS".  Also, the
      !>        declaration of the unused variable K was removed.
      module subroutine wninsd ( ctx, left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: left  !! Left, right endpoints of new interval.
      real(wp), intent(in) :: right  !! Left, right endpoints of new interval.
      real(wp), intent(inout) :: window    ( lbcell:* )  !! Input, output window.


      integer(ip) :: size
      integer(ip) :: card

      integer(ip) :: i
      integer(ip) :: j

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the size and cardinality of the window.
!
      size = sized ( ctx, window )
      card = cardd ( ctx, window )

!
!     Let's try the easy cases first. No input interval? No change.
!     Signal that an error has occurred and set the error message.
!
      if ( left > right ) then
         call ctx%setmsg ( 'Left endpoint was *. Right endpoint was *.' )
         call ctx%errdp  ( '*', left  )
         call ctx%errdp  ( '*', right )
         call ctx%sigerr ( 'SPICE(BADENDPOINTS)' )
         return

!
!     Empty window? Input interval later than the end of the window?
!     Just insert the interval, if there's room.
!
      else if ( card == 0  .or.  left > window(card) ) then

         if ( size >= card+2 ) then

            call scardd ( ctx, card+2, window )
            window(card+1) = left
            window(card+2) = right

         else

            call excess ( ctx, 2, 'window' )
            call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )

         end if

         return

      end if

!
!     Now on to the tougher cases.
!
!     Skip intervals which lie completely to the left of the input
!     interval. (The index I will always point to the right endpoint
!     of an interval).
!
      i = 2

      do while ( i <= card )
         if ( .not. ( window(i) < left ) ) exit
         i = i + 2
      end do

!
!     There are three ways this can go. The new interval can:
!
!        1) lie entirely between the previous interval and the next.
!
!        2) overlap the next interval, but no others.
!
!        3) overlap more than one interval.
!
!     Only the first case can possibly cause an overflow, since the
!     other two cases require existing intervals to be merged.
!

!
!     Case (1). If there's room, move succeeding intervals back and
!     insert the new one. If there isn't room, signal an error.
!
      if ( right < window(i-1) ) then

         if ( size >= card+2 ) then

            do j = card, i-1, -1
               window(j+2) = window(j)
            end do

            call scardd ( ctx, card+2, window )
            window(i-1) = left
            window(i  ) = right

         else

            call excess ( ctx, 2, 'window' )
            call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )
            return

         end if

!
!     Cases (2) and (3).
!
      else

!
!        The left and right endpoints of the new interval may or
!        may not replace the left and right endpoints of the existing
!        interval.
!
         window(i-1) = min ( left,  window(i-1) )
         window(i  ) = max ( right, window(i  ) )

!
!        Skip any intervals contained in the one we modified.
!        (Like I, J always points to the right endpoint of an
!        interval.)
!
         j = i + 2

         do while ( j <= card )
            if ( .not. ( window(j) <= window(i) ) ) exit
            j = j + 2
         end do

!
!        If the modified interval extends into the next interval,
!        merge the two. (The modified interval grows to the right.)
!
         if ( j <= card ) then
            if ( window(i) >= window(j-1) ) then
               window(i) = window(j)
               j         = j + 2
            end if
         end if

!
!        Move the rest of the intervals forward to take up the
!        spaces left by the absorbed intervals.
!
         do while ( j <= card )
            i           = i + 2
            window(i-1) = window(j-1)
            window(i  ) = window(j  )
            j           = j + 2
         end do

         call scardd ( ctx, i, window )

      end if

      return
      end subroutine wninsd

      !> Insert an interval into a window being built (PEPPER): [[wninsd]]
      !> for an [[interval_list]], which grows as needed. Signals
      !> SPICE(BADENDPOINTS), as [[wninsd]], if LEFT > RIGHT.
      module subroutine insert_interval ( ctx, left, right, window )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: left  !! the start of the interval
      real(wp), intent(in) :: right  !! the end of the interval
      type(interval_list), intent(inout) :: window  !! the window
      if ( ctx%failed() ) return
      if ( left > right ) then
         call ctx%setmsg ( 'Left endpoint was *. Right endpoint was *.' )
         call ctx%errdp  ( '*', left  )
         call ctx%errdp  ( '*', right )
         call ctx%sigerr ( 'SPICE(BADENDPOINTS)' )
         return
      end if
      call window%insert ( left, right )
      end subroutine insert_interval

      !> Place the intersection of two double precision windows into
      !> a third window.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are SPICE windows, each of which contains zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     C        is the output SPICE window, containing the
      !>              intersection of A and B --- every point contained in both
      !>              A and B.
      !>
      !>              C must be distinct from both A and B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the intersection of the two windows results in an excess of
      !>         elements, the error SPICE(WINDOWEXCESS) is signaled.
      !>
      !>     2)  The cardinality of the input windows must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The intersection of two windows contains every point contained
      !>     both in the first window and in the second window.
      !>
      !>## Examples
      !>
      !>     Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     and B contain the intervals
      !>
      !>           [ 2, 4 ]  [ 8, 10 ]  [ 16, 18 ]
      !>
      !>     Then the intersection of A and B contains the intervals
      !>
      !>           [ 2, 3 ]  [ 8, 10 ]
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of argument C in $Brief_I/O table.
      !>
      !>        Added entry #2 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 27-FEB-1989  (HAN)
      !>
      !>        Due to the calling sequence and functionality changes
      !>        in the routine EXCESS, the method of signaling an
      !>        excess of elements needed to be changed.
      module subroutine wnintd ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a        ( lbcell:* )  !! Input windows.
      real(wp), intent(in) :: b        ( lbcell:* )  !! Input windows.
      real(wp), intent(inout) :: c        ( lbcell:* )  !! Intersection of A and B.


      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: csize

      integer(ip) :: ap
      integer(ip) :: bp
      integer(ip) :: cp
      character(1) :: use
      integer(ip) :: over

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input windows. The initial cardinality of the output window
!     is zero. And there is no overflow so far.
!
!     (Note that AP and BP point to the LEFT endpoints of intervals
!     in A and B, while CP points to the RIGHT endpoint of the latest
!     interval in C.)
!
      ap   = 1
      bp   = 1
      cp   = 0
      over = 0

!
!     When the end of either input window is reached, we're done.
!
      do while ( ap < acard  .and.  bp < bcard )

!
!        Let's see what we can do with the earlier of the next
!        intervals from A and B.
!
         if ( a(ap+1) < b(bp+1) ) then
            use = 'A'

         else if ( b(bp+1) <= a(ap+1) ) then
            use = 'B'
         end if

!
!        If there is still space in the output window, fill it
!        as necessary. Otherwise, stop filling the array, but continue
!        to count the number of elements in excess of the size of the
!        output window.
!
!        The general idea is this: if the next interval of A overlaps
!        the next interval of B, save the area of overlap. (Similarly
!        for B.)
!
         if ( use == 'A' ) then

            if ( a(ap+1) >= b(bp) ) then

               if ( cp < csize ) then
                  cp      = cp + 2
                  c(cp-1) = max ( b(bp), a(ap) )
                  c(cp  ) = a(ap+1)
               else
                  over    = over + 2
               end if

            end if

            ap = ap + 2

!
!        This is the same as the last clause, with B replacing A.
!
         else if ( use == 'B' ) then

            if ( b(bp+1) >= a(ap) ) then

               if ( cp < csize ) then
                  cp      = cp + 2
                  c(cp-1) = max ( a(ap), b(bp) )
                  c(cp  ) = b(bp+1)
               else
                  over    = over + 2
               end if

            end if

            bp = bp + 2

         end if

      end do

!
!     Set the cardinality of the output window.
!
      call scardd ( ctx, cp, c )

!
!     If there are any excess elements, signal an error and check out
!     as usual.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'window' )
         call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )
      end if

      return
      end subroutine wnintd

      !> Compare two double precision windows.
      !>
      !> The function returns the result of comparison: A (OP) B.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are SPICE windows, each of which contains zero or more
      !>              intervals.
      !>
      !>     OP       is a comparison operator, indicating the way in which the
      !>              input sets are to be compared. OP may be any of the
      !>              following:
      !>
      !>                 Operator             Meaning
      !>                 --------  -------------------------------------------
      !>                   '='     A = B is .TRUE. if A and B are equal
      !>                           (contain the same intervals).
      !>
      !>                   '<>'    A <> B is .TRUE. if A and B are not equal.
      !>
      !>                   '<='    A <= B is .TRUE. if A is a subset of B.
      !>
      !>                   '<'     A < B is .TRUE. if A is a proper subset
      !>                           of B.
      !>
      !>                   '>='    A >= B is .TRUE. if B is a subset of A.
      !>
      !>                   '>'     A > B is .TRUE. if B is a proper subset
      !>                           of A.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the result of the comparison.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the relational operator is not recognized, the error
      !>         SPICE(INVALIDOPERATION) is signaled.
      !>
      !>     2)  The cardinality of the input windows must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function returns .TRUE. whenever the specified relationship
      !>     between the input windows, A and B, is satisfied. For example,
      !>     the expression
      !>
      !>        WNRELD ( NEEDED, '<=', AVAIL )
      !>
      !>     is .TRUE. whenever the window NEEDED is a subset of the window
      !>     AVAIL. One window is a subset of another window if each of
      !>     the intervals in the first window is included in one of the
      !>     intervals in the second window. In addition, the first window
      !>     is a proper subset of the second if the second window contains
      !>     at least one point not contained in the first window. (Thus,
      !>     '<' implies '<=', and '>' implies '>='.)
      !>
      !>     The following pairs of expressions are equivalent.
      !>
      !>        WNRELD ( A, '>', B )
      !>        WNRELD ( B, '<', A )
      !>
      !>        WNRELD ( A, '>=', B )
      !>        WNRELD ( B, '<=', A )
      !>
      !>## Examples
      !>
      !>     Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     Let B and C contain the intervals
      !>
      !>           [ 1, 2 ]  [ 9, 9 ]  [ 24, 27 ]
      !>
      !>     Let D contain the intervals
      !>
      !>           [ 5, 10 ]  [ 15, 25 ]
      !>
      !>     Finally, let E and F be empty windows (containing no intervals).
      !>
      !>     Because B and C contain the same intervals,
      !>
      !>           WNRELD ( B, '=',  C )
      !>           WNRELD ( B, '<=', C )
      !>           WNRELD ( B, '>=', C )
      !>
      !>     are all true, while
      !>
      !>           WNRELD ( B, '<>', C )
      !>
      !>     is .FALSE. Because neither B nor C contains any points not also
      !>     contained by the other, neither is a proper subset of the other.
      !>     Thus,
      !>
      !>           WNRELD ( B, '<', C )
      !>           WNRELD ( B, '>', C )
      !>
      !>     are both false.
      !>
      !>     Every point contained in B and C is also contained in A. Thus,
      !>
      !>           WNRELD ( B, '<=', A )
      !>           WNRELD ( A, '>=', C )
      !>
      !>     are both true. In addition, A contains points not contained in
      !>     B and C. (That is, the differences A-B and A-C are not empty.)
      !>     Thus, B and C are proper subsets of A as well, and
      !>
      !>           WNRELD ( B, '<', A )
      !>           WNRELD ( A, '>', B )
      !>
      !>     are both true.
      !>
      !>     Although A and D have points in common, neither contains the
      !>     other. Thus
      !>
      !>           WNRELD ( A, '=',  D )
      !>           WNRELD ( A, '<=', D )
      !>           WNRELD ( A, '>=', D )
      !>
      !>     are all false.
      !>
      !>     In addition, any window is equal to itself, a subset of itself,
      !>     and a superset of itself. Thus,
      !>
      !>           WNRELD ( A, '=',  A )
      !>           WNRELD ( A, '<=', A )
      !>           WNRELD ( A, '>=', A )
      !>
      !>     are always true. However, no window is a proper subset or a
      !>     proper superset of itself. Thus,
      !>
      !>           WNRELD ( A, '<', A )
      !>           WNRELD ( A, '>', A )
      !>
      !>     are always false.
      !>
      !>     Finally, an empty window is a a proper subset of any window
      !>     except another empty window. Thus,
      !>
      !>           WNRELD ( E, '<', A )
      !>
      !>     is .TRUE., but
      !>
      !>           WNRELD ( E, '<', F )
      !>
      !>     is .FALSE.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #2
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        Set the default function value to either 0, 0.0D0, .FALSE.,
      !>        or blank depending on the type of the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 02-FEB-1989 (HAN)
      !>
      !>        If the relational operator is not recognized, an error is
      !>        signaled. The previous version returned .FALSE. as the
      !>        function value, and no error was signaled.
      !>
      !>        Also, the $Required_Reading section has been changed to
      !>        include WINDOWS as the required reading for the module.
      module logical function wnreld ( ctx, a, op, b )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a        ( lbcell:* )  !! First window.
      character(*), intent(in) :: op  !! Comparison operator.
      real(wp), intent(in) :: b        ( lbcell:* )  !! Second window.


      integer(ip) :: acard
      integer(ip) :: bcard
      logical :: equal
      logical :: subset
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         wnreld = .false.
         return
      else
         wnreld = .false.
      end if

!
!     Find the cardinality of the input windows.
!
      acard  = cardd ( ctx, a )
      bcard  = cardd ( ctx, b )

!
!     A and B are equal if they contain exactly the same intervals.
!     We need to know this for nearly every relationship, so find out
!     before going any further.
!
         if ( acard /= bcard ) then
            equal = .false.

         else
            equal = .true.

            do i = 1, acard
               equal = equal .and. ( a(i) == b(i) )
            end do
         end if

!
!     Simple equality and inequality are trivial at this point.
!
      if ( op == '=' ) then

         wnreld = equal

      else if ( op == '<>' ) then

         wnreld = .not. equal

!
!     Subsets are a little trickier. A is a subset of B if every
!     interval in A is included in B. In addition, A is a proper
!     subset if A and B are not equal.
!
      else if ( op == '<='  .or.  op == '<' ) then

         subset = .true.

         do i = 1, acard, 2
            subset = subset .and. ( wnincd ( ctx, a(i), a(i+1), b ) )
         end do

         if ( op == '<=' ) then
            wnreld = subset

         else
            wnreld = subset .and. ( .not. equal )
         end if

!
!     A and B change places here...
!
      else if ( op == '>='  .or.  op == '>' ) then

         subset = .true.

         do i = 1, bcard, 2
            subset = subset .and. ( wnincd ( ctx, b(i), b(i+1), a ) )
         end do

         if ( op == '>=' ) then
            wnreld = subset

         else
            wnreld = subset .and. ( .not. equal )
         end if

!
!     An unrecognized operator always fails.
!
      else

         call ctx%setmsg ( 'Relational operator, *, is not '         // &
                       'recognized.'                              )
         call ctx%errch  ( '*', op                   )
         call ctx%sigerr ( 'SPICE(INVALIDOPERATION)' )
         return

      end if

      return
      end function wnreld

      !> Summarize the contents of a double precision window.
      !>
      !>## Detailed Input
      !>
      !>     WINDOW   is a window containing zero or more intervals.
      !>
      !>## Detailed Output
      !>
      !>     MEAS     is the total measure of the intervals in the input
      !>              window. This is just the sum of the measures of the
      !>              individual intervals.
      !>
      !>     AVG      is the average of the measures of the intervals in the
      !>              input window.
      !>
      !>     STDDEV   is the standard deviation of the measures of the
      !>              intervals in the input window.
      !>
      !>     IDXSML,
      !>     IDXLON   are the locations of the shortest and longest intervals
      !>              in the input window. The shortest interval is
      !>
      !>                 [ WINDOW(IDXSML), WINDOW(IDXSML+1) ]
      !>
      !>              and the longest is
      !>
      !>                 [ WINDOW(IDXLON), WINDOW(IDXLON+1) ]
      !>
      !>              IDXSML and IDXLON are both zero if the input window
      !>              contains no intervals.
      !>
      !>              If WINDOW contains multiple intervals having the shortest
      !>              length, IDXSML is the index of the first such interval.
      !>              Likewise for the longest length.
      !>
      !>              Indices range from 1 to 2*N-1, where N is the number of
      !>              intervals in the window.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If WINDOW has odd cardinality, the error
      !>         SPICE(INVALIDCARDINALITY) is signaled.
      !>
      !>     2)  Left endpoints of stored intervals must be strictly greater
      !>         than preceding right endpoints. Right endpoints must be
      !>         greater than or equal to corresponding left endpoints.
      !>         Invalid window data are not diagnosed by this routine and may
      !>         lead to unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides a summary of the input window, consisting
      !>     of the following items:
      !>
      !>     -  The measure of the window.
      !>
      !>     -  The average and standard deviation of the measures
      !>        of the individual intervals in the window.
      !>
      !>     -  The indices of the left endpoints of the shortest
      !>        and longest intervals in the window.
      !>
      !>     All of these quantities are zero if the window contains no
      !>     intervals.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Define a window with six intervals, and calculate the
      !>        summary for that window.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNSUMD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               WNCARD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1   = '(A,F11.6)' )
      !>
      !>      CHARACTER*(*)         FMT2
      !>      PARAMETER           ( FMT2   = '(A,I4)' )
      !>
      !>      CHARACTER*(*)         FMT3
      !>      PARAMETER           ( FMT3   = '(A,2(F6.3,A))' )
      !>
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WNSIZE
      !>      PARAMETER           ( WNSIZE = 12 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AVG
      !>      DOUBLE PRECISION      LEFT
      !>      DOUBLE PRECISION      MEAS
      !>      DOUBLE PRECISION      RIGHT
      !>      DOUBLE PRECISION      STDDEV
      !>      DOUBLE PRECISION      WINDOW ( LBCELL:WNSIZE )
      !>
      !>      INTEGER               IDXLON
      !>      INTEGER               IDXSML
      !>      INTEGER               INTLON
      !>      INTEGER               INTSML
      !>
      !>C
      !>C     Validate the WINDOW with size WNSIZE and zero elements.
      !>C
      !>      CALL WNVALD ( WNSIZE, 0, WINDOW )
      !>
      !>C
      !>C     Insert the intervals
      !>C
      !>C        [  1,  3 ] [  7, 11 ] [ 18, 18 ]
      !>C        [ 23, 27 ] [ 30, 69 ] [ 72, 80 ]
      !>C
      !>C     into WINDOW.
      !>C
      !>      CALL WNINSD (  1.0D0,  3.0D0, WINDOW )
      !>      CALL WNINSD (  7.0D0, 11.0D0, WINDOW )
      !>      CALL WNINSD ( 18.0D0, 18.0D0, WINDOW )
      !>      CALL WNINSD ( 23.0D0, 27.0D0, WINDOW )
      !>      CALL WNINSD ( 30.0D0, 69.0D0, WINDOW )
      !>      CALL WNINSD ( 72.0D0, 80.0D0, WINDOW )
      !>
      !>C
      !>C     Calculate the summary for WINDOW.
      !>C
      !>      CALL WNSUMD ( WINDOW, MEAS,   AVG,
      !>     .              STDDEV, IDXSML, IDXLON )
      !>
      !>C
      !>C     IDXSML and IDXLON refer to the indices of
      !>C     the SPICE Cell data array.
      !>C
      !>      INTSML = (IDXSML+1)/2
      !>      INTLON = (IDXLON+1)/2
      !>
      !>      WRITE(*,FMT1) 'Measure           : ', MEAS
      !>      WRITE(*,FMT1) 'Average           : ', AVG
      !>      WRITE(*,FMT1) 'Standard Dev      : ', STDDEV
      !>      WRITE(*,FMT2) 'Index shortest    : ', IDXSML
      !>      WRITE(*,FMT2) 'Index longest     : ', IDXLON
      !>      WRITE(*,FMT2) 'Interval shortest : ', INTSML
      !>      WRITE(*,FMT2) 'Interval longest  : ', INTLON
      !>
      !>C
      !>C     Output the shortest and longest intervals.
      !>C
      !>      CALL WNFETD ( WINDOW, INTSML, LEFT, RIGHT )
      !>      WRITE(*,FMT3) 'Shortest interval : [ ', LEFT, ', ',
      !>     .                                        RIGHT, ' ]'
      !>      CALL WNFETD ( WINDOW, INTLON, LEFT, RIGHT )
      !>      WRITE(*,FMT3) 'Longest interval  : [ ', LEFT, ', ',
      !>     .                                        RIGHT, ' ]'
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Measure           :   57.000000
      !>        Average           :    9.500000
      !>        Standard Dev      :   13.413302
      !>        Index shortest    :    5
      !>        Index longest     :    9
      !>        Interval shortest :    3
      !>        Interval longest  :    5
      !>        Shortest interval : [ 18.000, 18.000 ]
      !>        Longest interval  : [ 30.000, 69.000 ]
      !>
      !>     2) Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>        Let B contain the singleton intervals
      !>
      !>           [ 2, 2 ]  [ 9, 9 ]  [ 27, 27 ]
      !>
      !>        The measures of A and B are
      !>
      !>           (3-1) + (11-7) + (27-23) = 10
      !>
      !>        and
      !>
      !>           (2-2) + (9-9) + (27-27) = 0
      !>
      !>        respectively. Each window has three intervals; thus, the
      !>        average measures of the windows are 10/3 and 0. The standard
      !>        deviations are
      !>
      !>             .-----------------------------------------
      !>             |       2         2          2
      !>             |  (3-1)  + (11-7)  + (27-23)           2           1/2
      !>             |  ---------------------------  - (10/3)     = (8/9)
      !>             |             3
      !>           \ |
      !>            \|
      !>
      !>        and 0. Neither window has one "shortest" interval or "longest"
      !>        interval; so the first ones found are returned: IDXSML and
      !>        IDXLON are 1 and 3 for A, 1 and 1 for B.
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR) (NJB)
      !>
      !>        Changed output argument names SMALL and LONG to IDXSML and
      !>        IDXLON for consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Added entry #2 in $Exceptions section.
      !>
      !>        Improved description of arguments IDXSML and IDXLON in
      !>        $Detailed_Output.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 25-FEB-2009 (EDW)
      !>
      !>        Added error test to confirm input window has even cardinality.
      !>        Corrected section order to match NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 29-JUL-2002 (NJB)
      !>
      !>        Corrected error in example section: changed claimed value
      !>        of longest interval for window A from 2 to 3.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module subroutine wnsumd ( ctx, window, meas, avg, stddev, idxsml, idxlon )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: window    ( lbcell:* )  !! Window to be summarized.
      real(wp), intent(out) :: meas  !! Total measure of intervals in WINDOW.
      real(wp), intent(out) :: avg  !! Average measure.
      real(wp), intent(out) :: stddev  !! Standard deviation.
      integer(ip), intent(out) :: idxsml  !! Locations of shortest, longest intervals.
      integer(ip), intent(out) :: idxlon  !! Locations of shortest, longest intervals.


      real(wp) :: m
      real(wp) :: sum
      real(wp) :: sumsqr
      real(wp) :: mshort
      real(wp) :: mlong

      integer(ip) :: i
      integer(ip) :: card

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the cardinality (number of endpoints) of the window.
!
      card = cardd ( ctx, window )

!
!     Confirm evenness of CARD.
!
      if ( .not. (mod(card, 2) == 0) ) then

         call ctx%setmsg ( 'Input window has odd cardinality. A valid ' &
                  //   'SPICE window must have even element ' &
                  //   'cardinality.' )
         call ctx%sigerr ( 'SPICE(INVALIDCARDINALITY)' )
         return

      end if

!
!     Trivial case: no intervals. Return all zeros.
!
      if ( card == 0 ) then

         meas   = 0.0_wp
         avg    = 0.0_wp
         stddev = 0.0_wp
         idxsml  = 0
         idxlon   = 0

!
!     Collect the sum of the measures and the squares of the measures
!     for each of the intervals in the window. At the same time, keep
!     track of the shortest and longest intervals encountered.
!
      else

         sum    = 0.0_wp
         sumsqr = 0.0_wp

         idxsml  = 1
         mshort = window(2) - window(1)

         idxlon   = 1
         mlong  = window(2) - window(1)

         do i = 1, card, 2

            m       = window(i+1) - window(i)
            sum     = sum         + m
            sumsqr  = sumsqr      + m*m

            if ( m < mshort ) then
               idxsml  = i
               mshort = m
            end if

            if ( m > mlong ) then
               idxlon  = i
               mlong = m
            end if

         end do

!
!        The envelope please?
!
         meas   = sum
         avg    = meas * 2.0_wp / real( card, wp )
         stddev = dsqrt ( ( sumsqr * 2.0_wp / real( card, wp ) ) - avg*avg )

      end if

      return
      end subroutine wnsumd

      !> Place the union of two double precision windows into a third
      !> window.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are SPICE windows, each of which contains zero or more
      !>              intervals.
      !>
      !>## Detailed Output
      !>
      !>     C        is the output SPICE window, containing the union of A
      !>              and B --- every point contained in A, or in B, or in
      !>              both.
      !>
      !>              C must be distinct from both A and B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the union of the two windows results in an excess of
      !>         elements, the error SPICE(WINDOWEXCESS) is signaled.
      !>
      !>     2)  The cardinality of the input windows must be even. Left
      !>         endpoints of stored intervals must be strictly greater than
      !>         preceding right endpoints. Right endpoints must be greater
      !>         than or equal to corresponding left endpoints. Invalid window
      !>         data are not diagnosed by this routine and may lead to
      !>         unpredictable results.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The union of two windows contains every point contained in the
      !>     first window, or the second window, or both.
      !>
      !>## Examples
      !>
      !>     Let A contain the intervals
      !>
      !>           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
      !>
      !>     and B contain the intervals
      !>
      !>           [ 2, 6 ]  [ 8, 10 ]  [ 16, 18 ]
      !>
      !>     Then the union of A and B contains the intervals
      !>
      !>           [ 1, 6 ]  [ 7, 11 ]  [ 16, 18 ]  [ 23, 27 ]
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of argument C in $Brief_I/O table.
      !>
      !>        Added entry #2 in $Exceptions section. Removed unnecessary
      !>        entries in $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT)
      !>
      !>        The variable END was not initialized in the previous
      !>        edition. It is now initialized to be the minimum of
      !>        A(1) and B(1).
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 27-FEB-1989  (HAN)
      !>
      !>        Due to the calling sequence and functionality changes
      !>        in the routine EXCESS, the method of signaling an
      !>        excess of elements needed to be changed.
      module subroutine wnunid ( ctx, a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      real(wp), intent(in) :: a        ( lbcell:* )  !! Input windows.
      real(wp), intent(in) :: b        ( lbcell:* )  !! Input windows.
      real(wp), intent(inout) :: c        ( lbcell:* )  !! Union of A and B.


      integer(ip) :: acard
      integer(ip) :: bcard
      integer(ip) :: csize

      integer(ip) :: ap
      integer(ip) :: bp
      integer(ip) :: cp
      real(wp) :: end
      character(1) :: use
      integer(ip) :: over

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window.
!
      acard = cardd ( ctx, a )
      bcard = cardd ( ctx, b )
      csize = sized ( ctx, c )

!
!     Begin with the input pointers at the first elements of the
!     input windows. The initial cardinality of the output window
!     is zero. And there is no overflow so far.
!
!     (Note that AP and BP point to the LEFT endpoints of intervals
!     in A and B, while CP points to the RIGHT endpoint of the latest
!     interval in C.)
!
      ap   = 1
      bp   = 1
      cp   = 0
      end  = min( a(1), b(1) )
      over = 0

!
!     When the ends of both input windows are reached, we're done.
!
      do while ( ap < acard  .or.  bp < bcard )

!
!        If the end of one window has been reached, copy (or merge)
!        the next interval from the other window.
!
         if ( ap > acard ) then
            use = 'B'

         else if ( bp > bcard ) then
            use = 'A'

!
!        Otherwise, let's see what we can do with the earlier of
!        the next intervals from A and B.
!
         else if ( a(ap) < b(bp) ) then
            use = 'A'

         else if ( b(bp) <= a(ap) ) then
            use = 'B'
         end if

!
!        If there is still space in the output window, fill it
!        as necessary. Otherwise, stop filling the array, but continue
!        to count the number of elements in excess of the size of the
!        output window.
!
!        The general idea is this: if the next interval overlaps the
!        latest output interval, merge the two (extending the output
!        interval to the right). Otherwise, insert the next interval
!        intact.
!
         if ( use == 'A' ) then

            if ( cp < csize ) then

               if ( a(ap) <= end .and. cp > 0 ) then
                  c(cp) = max ( c(cp), a(ap+1) )
               else
                  cp      = cp + 2
                  c(cp-1) = a(ap  )
                  c(cp  ) = a(ap+1)
               end if

               end = c(cp)

            else

               if ( a(ap) <= end ) then
                  end  = max ( end, a(ap+1) )
               else
                  over = over + 2
                  end  = a(ap+1)
               end if

            end if

            ap = ap + 2

!
!        This is the same as the last clause, with B replacing A.
!
         else if ( use == 'B' ) then

            if ( cp < csize ) then

               if ( b(bp) <= end .and. cp > 0 ) then
                  c(cp)   = max ( c(cp), b(bp+1) )
               else
                  cp      = cp + 2
                  c(cp-1) = b(bp  )
                  c(cp  ) = b(bp+1)
               end if

               end = c(cp)

            else

               if ( b(bp) <= end ) then
                  end  = max ( end, b(bp+1) )
               else
                  over = over + 2
                  end  = b(bp+1)
               end if
            end if

            bp = bp + 2

         end if

      end do

!
!     Set the cardinality of the output window.
!
      call scardd ( ctx, cp, c )

!
!     If there is an excess of elements, signal an error and check out
!     as usual.
!
      if ( over > 0 ) then
         call excess ( ctx, over, 'window' )
         call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )
      end if

      return
      end subroutine wnunid

      !> Form a valid double precision window from the contents
      !> of a window array.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the size of the window to be validated. This is the
      !>              maximum number of endpoints that the cell used to
      !>              implement the window is capable of holding at any one
      !>              time.
      !>
      !>     N        is the original number of endpoints in the input cell.
      !>
      !>     WINDOW   on input is a (possibly uninitialized) cell array of
      !>              maximum size SIZE containing N endpoints of (possibly
      !>              unordered and non-disjoint) intervals.
      !>
      !>## Detailed Output
      !>
      !>     WINDOW   on output is a validated window, in which any overlapping
      !>              input intervals have been merged and the resulting set of
      !>              intervals is arranged in increasing order.
      !>
      !>              WINDOW is ready for use with any of the window routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the original number of endpoints N is odd, the error
      !>         SPICE(UNMATCHENDPTS) is signaled.
      !>
      !>     2)  If the original number of endpoints of the window exceeds its
      !>         size, the error SPICE(WINDOWTOOSMALL) is signaled.
      !>
      !>     3)  If the left endpoint is greater than the right endpoint, the
      !>         error SPICE(BADENDPOINTS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine takes as input a cell array containing pairs of
      !>     endpoints and validates it to form a window.
      !>
      !>     On input, WINDOW is a cell of size SIZE containing N endpoints.
      !>     During validation, the intervals are ordered, and overlapping
      !>     intervals are merged. On output, the cardinality of WINDOW is
      !>     the number of endpoints remaining, and it is ready for use with
      !>     any of the window routines.
      !>
      !>     Because validation is done in place, there is no chance of
      !>     overflow.
      !>
      !>     Validation is primarily useful for ordering and merging
      !>     intervals read from input files or initialized in DATA
      !>     statements.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Define an array containing a set of unordered and possibly
      !>        overlapping intervals, and validate the array as a SPICE
      !>        window.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM WNVALD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER               CARDD
      !>      INTEGER               SIZED
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5 )
      !>
      !>      INTEGER               WINSIZ
      !>      PARAMETER           ( WINSIZ = 20 )
      !>
      !>      INTEGER               DATSIZ
      !>      PARAMETER           ( DATSIZ = 16 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      WINDOW  ( LBCELL : WINSIZ )
      !>      DOUBLE PRECISION      WINDAT  ( DATSIZ )
      !>
      !>      INTEGER               I
      !>
      !>      DATA                  WINDAT  /  0,  0,
      !>     .                                10, 12,
      !>     .                                 2,  7,
      !>     .                                13, 15,
      !>     .                                 1,  5,
      !>     .                                23, 29,  4*0 /
      !>
      !>C
      !>C     Insert the data into the SPICE cell array.
      !>C
      !>      CALL MOVED ( WINDAT, WINSIZ, WINDOW(1) )
      !>
      !>C
      !>C     Validate the input WINDOW array as a SPICE window.
      !>C
      !>      CALL WNVALD ( WINSIZ, DATSIZ, WINDOW )
      !>
      !>      WRITE (*,*) 'Current intervals: ', CARDD ( WINDOW ) / 2
      !>      WRITE (*,*) 'Maximum intervals: ', SIZED ( WINDOW ) / 2
      !>      WRITE (*,*)
      !>      WRITE (*,*) 'Intervals:'
      !>      WRITE (*,*)
      !>
      !>      DO I = 1, CARDD ( WINDOW ), 2
      !>         WRITE (*,*) WINDOW(I), WINDOW(I+1)
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Current intervals:            5
      !>         Maximum intervals:           10
      !>
      !>         Intervals:
      !>
      !>           0.0000000000000000        0.0000000000000000
      !>           1.0000000000000000        7.0000000000000000
      !>           10.000000000000000        12.000000000000000
      !>           13.000000000000000        15.000000000000000
      !>           23.000000000000000        29.000000000000000
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 16-MAR-2021 (JDR)
      !>
      !>        Changed argument name A to WINDOW for consistency with other
      !>        routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply to NAIF standard. Created complete
      !>        code example from code fragment and added example's problem
      !>        statement.
      !>
      !>        Improved description of argument WINDOW in $Detailed_Output.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.1, 30-JUL-2002 (NJB)
      !>
      !>        Fixed bugs in example program.
      !>
      !>-    SPICELIB Version 1.1.0, 14-AUG-1995 (HAN)
      !>
      !>        Fixed a character string that continued over two lines.
      !>        The "//" characters were missing. The Alpha/OpenVMS compiler
      !>        issued a warning regarding this incorrect statement syntax.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module subroutine wnvald ( ctx, size, n, window )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: odd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: lbcell = -5  !! lower bound of a cell (its control area)
      integer(ip), intent(in) :: size  !! Size of window.
      integer(ip), intent(in) :: n  !! Original number of endpoints.
      real(wp), intent(inout) :: window ( lbcell:* )  !! Input, output window.


      real(wp) :: left
      real(wp) :: right
      integer(ip) :: i

!
!     Setting up error processing.
!
      if ( ctx%failed() ) return

!
!     First, some error checks. The number of endpoints must be even,
!     and smaller than the reported size of the window.
!
      if ( odd ( n ) ) then

         call ctx%setmsg ( 'WNVALD: Unmatched endpoints' )
         call ctx%sigerr ( 'SPICE(UNMATCHENDPTS)' )
         return

      else if ( n > size ) then

         call ctx%setmsg ( 'WNVALD: Inconsistent value for SIZE.' )
         call ctx%sigerr ( 'SPICE(WINDOWTOOSMALL)' )
         return

      end if

!
!     Taking the easy way out, we will simply insert each new interval
!     as we happen upon it. We can do this safely in place. The output
!     window can't possibly contain more intervals than the input array.
!
!     What can go wrong is this: a left endpoint might be greater than
!     the corresponding left endpoint. This is a boo-boo, and should be
!     reported.
!
      call ssized ( ctx, size, window )
      call scardd ( ctx,    0, window )

      i = 1

      do while  ( i < n )

         left  = window(i)
         right = window(i+1)

         if ( left > right ) then
           call ctx%setmsg ( 'WNVALD: Left endpoint may not exceed ' // &
                         'right endpoint.'                        )
           call ctx%sigerr ( 'SPICE(BADENDPOINTS)'                    )
           return
         end if

         call wninsd ( ctx, left, right, window )
         i = i + 2

      end do

      return
      end subroutine wnvald

end submodule pepper_cells
