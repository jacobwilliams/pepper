!> Character string operations.
!>
!> The procedures keep SPICELIB's names, arguments and documentation. They
!> have no saved state and do not signal SPICE errors.
!>
!> The `_inplace` variants ([[ucase_inplace]], [[repmc_inplace]], ...) change
!> one `intent(inout)` string. The library calls them where SPICELIB passes
!> one variable as both the input and the output string, which Fortran does
!> not allow.
module pepper_strings

   use pepper_kinds, only: ip, wp

   implicit none

   private

   ! (PEPPER) the one conversion of decimal text to double precision
   public :: decimal_value
   public :: astrip, bedec, beint, benum, beuns, cmprss, cpos, cposr, dpstr,&
      eqchr, eqstr, esrchc, fndnwd, frstnb, frstnp, frstpc, intstr, kxtrct,&
      lastnb, lastpc, lbuild, lcase, ljucrs, ljust, lparse, lparsm, ltrim,&
      lx4uns, lxqstr, matchi, matchw, nblen, nbwid, ncpos, ncposr, nechr,&
      nextwd, nparsd, nparsi, nthwd, parsqs, pcwid, pos, posr, prefix,&
      quote, replch, replwd, repmc, rjust, rtrim, samch, samchi, samsbi,&
      samsub, scanrj, shiftl, shiftr, sigdgt, suffix, ucase, wdcnt, wdindx,&
      zzinssub, zzrepsub

   ! In-place variants (one INTENT(INOUT) string instead of the input and
   ! output strings; see the comment before ucase_inplace).
   public :: astrip_inplace, cmprss_inplace, lcase_inplace, nextwd_inplace,&
      repmc_inplace, repmd_inplace, repmi_inplace, replch_inplace,&
      ucase_inplace, zzinssub_inplace, zzrepsub_inplace


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   character(1), parameter :: blank = ' '  !! a blank
   integer(ip), parameter :: ispace = 32  !! character code of a blank
   integer(ip), parameter :: lowa = ichar ('a')  !! character code of "a"
   integer(ip), parameter :: lowz = ichar ('z')  !! character code of "z"
   integer(ip), parameter :: lstchr = 255  !! last character code
   integer(ip), parameter :: shift = ichar ('A') - lowa  !! from a lower case character code to its upper case
   integer(ip), parameter :: uppera = ichar ('A')  !! character code of "A"
   integer(ip), parameter :: upperz = ichar ('Z')  !! character code of "Z"

contains

      !> Remove from a character string all characters which fall
      !> between specified starting and ending characters, inclusive.
      !>
      !>## Detailed Input
      !>
      !>     INSTR    is a character string from which all characters
      !>              between ASCIIB and ASCIIE, inclusive, are to be
      !>              removed.
      !>
      !>     ASCIIB   is the first ASCII character in the range of
      !>              characters to be removed from the input string.
      !>              ASCIIB is itself removed from the string, if
      !>              it occurs.
      !>
      !>     ASCIIE   is the last ASCII character in the range of
      !>              characters to be removed from the input string.
      !>              ASCIIE is itself removed from the string, if
      !>              it occurs.
      !>
      !>## Detailed Output
      !>
      !>     OUTSTR   is the input string after all the character
      !>              between ASCIIB and ASCIIE, inclusive, have
      !>              been removed.
      !>
      !>              If OUTSTR is not large enough to hold the output
      !>              string, it is truncated on the right.
      !>
      !>              OUTSTR may overwrite INSTR.
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
      !>     ASTRIP checks each character
      !>     in INSTR to determine if it falls between the characters ASCIIB
      !>     and ASCIIE. If so this character is removed from the string
      !>     (and the string is shortened). Remaining characters are copied
      !>     to the output string.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of ASTRIP.
      !>
      !>           ASCIIB = 'b'
      !>           ASCIIE = 'k'
      !>           INSTR  = 'Now is the time for all good men to come quick.'
      !>           OUTSTR = 'Now s t tm or all oo mn to om qu.'
      !>
      !>           ASCIIB = 'a'
      !>           ASCIIE = 'z'
      !>           INSTR  = 'SELECT column TIME FROM table TEST'
      !>           OUTSTR = 'SELECT TIME FROM TEST'
      !>
      !>           ASCIIB = 'a'
      !>           ASCIIE = 'z'
      !>           INSTR  = 'this is going to be an empty string'
      !>           OUTSTR = ' '
      !>
      !>           ASCIIB = '!'
      !>           ASCIIE = '!'
      !>           INSTR  = 'Only 32 more shopping days until Christmas!'
      !>           OUTSTR = 'Only 32 more shopping days until Christmas'
      !>
      !>     ASTRIP may also be used to strip ASCII control characters
      !>     (line feeds, tab stops, and so on), as shown in the example
      !>     below.
      !>
      !>           ASCIIB = CHAR ( 0  )
      !>           ASCIIE = CHAR ( 31 )
      !>           CALL ASTRIP ( STRING, ASCIIB, ASCIIE, STRING )
      !>
      !>## Restrictions
      !>
      !>     1)  If ASCIIB and ASCIIE are not properly ordered (that is,
      !>         if ICHAR(ASCIIB) is not less than or equal to ICHAR(ASCIIE))
      !>         then ASTRIP will not function as described. (In fact, it will
      !>         copy the input string to the output string without change.)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      elemental subroutine astrip ( instr, asciib, asciie, outstr )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: instr  !! Input string.
      character(1), intent(in) :: asciib  !! First ASCII character in range to be stripped.
      character(1), intent(in) :: asciie  !! Last ASCII character in range to be stripped.
      character(*), intent(out) :: outstr  !! Output (stripped) string.

      integer(ip) :: outlen
      integer(ip) :: last
      integer(ip) :: lwrbnd
      integer(ip) :: uprbnd
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k

!
!     Find the length of the output string. We don't want to
!     exceed it.
!
      outlen = len ( outstr )

!
!     Find the last non-blank character of the input string.
!
      last   = len_trim(instr)

!
!     Get the numeric representation of ASCIIB and ASCIIE.
!
      lwrbnd = ichar ( asciib )
      uprbnd = ichar ( asciie )

!
!     Step through INSTR (I) a character at a time, transferring
!     characters to OUTSTR (J) whenever they fall outside the range
!     [ASCIIB, ASCIIE].
!
!     If the end of OUTSTR is reached, stop transferring characters
!     and return.
!
      j    = 0

      do i = 1, last

         k = ichar ( instr(i:i) )

         if ( ( k < lwrbnd ) .or. ( k > uprbnd ) ) then
!
!           The character is kept.  Note that if the user inputs
!           ASCIIB and ASCIIE in the wrong order this test will
!           always succeed so that the output string will be
!           the same as the input string.
!
            j           = j + 1
            outstr(j:j) = instr(i:i)

            if ( j == outlen ) then
               return
            end if

         end if

      end do

!
!     Pad the output string with blanks.
!
      if ( j < outlen ) then
         outstr(j+1: ) = ' '
      end if

      return
      end subroutine astrip

      !> Determine whether a string represents a decimal number.
      !>
      !> The function returns .TRUE. if the string represents a decimal
      !> number. Otherwise, it returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is any string.
      !>
      !>## Detailed Output
      !>
      !>     If the input string contains a decimal number (as defined
      !>     in $Particulars below), the function returns .TRUE. Otherwise,
      !>     the functions returns .FALSE.
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
      !>     A decimal number may be constructed by concatenating
      !>     the following components in the order shown.
      !>
      !>        1) A sign ('+' or '-'), or the null string.
      !>
      !>        2) An unsigned integer (as defined by function BEUNS),
      !>           or the null string.
      !>
      !>        3) A decimal point, or the null string.
      !>
      !>        4) An unsigned integer, or the null string.
      !>
      !>## Examples
      !>
      !>     Four classes of numbers recognized by the various BE functions.
      !>
      !>        UNS      unsigned integer
      !>        INT      integer                (includes INT)
      !>        DEC      decimal number         (includes UNS, INT)
      !>        NUM      number                 (includes UNS, INT, NUM)
      !>
      !>     The following table illustrates the differences between
      !>     the classes. (Any number of leading and trailing blanks
      !>     are acceptable.)
      !>
      !>        String                  Accepted by
      !>        ------------------      ------------------
      !>        0                       UNS, INT, DEC, NUM
      !>        21
      !>        21994217453648
      !>
      !>        +0                      INT, DEC, NUM
      !>        -13
      !>        +21946
      !>
      !>        1.23                    DEC, NUM
      !>        12.
      !>        .17
      !>        +4.1
      !>        -.25
      !>
      !>        2.3e17                  NUM
      !>        17.D-13275849
      !>        -.194265E+0004
      !>
      !>     Note that the functions don't take the magnitudes of the numbers
      !>     into account. They may accept numbers that cannot be represented
      !>     in Fortran variables. (For example, '2.19E999999999999' probably
      !>     exceeds the maximum floating point number on any machine, but
      !>     is perfectly acceptable to BENUM.)
      !>
      !>     The following strings are not accepted by any of the functions.
      !>
      !>        String             Reason
      !>        ---------------    ----------------------------------------
      !>        3/4                No implied operations (rational numbers)
      !>        37+14              No explicit operations
      !>        E12                Must have mantissa
      !>        217,346.91         No commas
      !>        3.14 159 264       No embedded spaces
      !>        PI                 No special numbers
      !>        FIVE               No textual numbers
      !>        CXIV               No roman numerals
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
      !>-    SPICELIB Version 1.1.0, 24-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      elemental logical function bedec  ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Character string.

      integer(ip) :: c
      integer(ip) :: d
      integer(ip) :: e
      integer(ip) :: l

!
!     First determine whether or not a decimal point is present.
!
      d = pos ( string, '.', 1 )
      c = d-1
      e = d+1

      if ( d == 0 ) then

!
!        If there is no decimal point just apply the integer test.
!
         bedec = beint ( string )

      else

!
!        A decimal point is present, get the length of the string
!        and see where the decimal point is relative to the last
!        character.
!
         l = len(string)

         if ( l == 1 ) then

!
!           The string is one character long and a decimal point.
!           Sorry, this is not a decimal number.
!
            bedec = .false.

         else if ( d == 1 ) then

!
!           The decimal point occurs as the first character of the
!           string.  The string following it must begin with
!           a non-blank character and be an unsigned integer.
!
            bedec =       string(e:e) /= ' ' &
                     .and. beuns  ( string(e:) )

         else if ( d == l ) then

!
!           The decimal point is the last character of the string.
!           The character that precedes it must be non-blank and
!           the substring to the left must be an integer.
!
            bedec =       string(c:c) /= ' ' &
                     .and. beint ( string(1:c) )

         else if ( string(c:c) == ' ' ) then

!
!           The decimal point occurs somewhere in the middle of the
!           string and the character preceding it is blank.
!
            bedec =        string(e:e) /= ' ' &
                     .and. string(1:c) == ' ' &
                     .and. beuns  ( string(e:) )

         else if ( string(e:e) == ' ' ) then

!
!           Again the decimal point occurs somewhere in the middle of
!           the string and the character following it is blank.
!
            bedec =        string(e:l) == ' ' &
                     .and. string(c:c) /= ' ' &
                     .and. beint ( string(1:c) )

         else if ( string(c:c) == '-' .or. string(c:c) == '+' ) then

!
!           The decimal point is in the middle of the string and
!           is preceded by a '+' or '-'.  There should be nothing
!           preceding the sign and what follows the decimal point
!           should be an unsigned integer. (we already know that the
!           character following the decimal point is not a blank)
!
            if ( c == 1 ) then
               bedec = beuns (string(e:l))
            else
               bedec =        beuns ( string(e:l) ) &
                        .and. string(1:c-1) == ' '
            end if

         else

!
!            Last chance, the decimal point is in the middle of the
!            string.  The characters to the right and left of the
!            point are non-blank and we know the character to the
!            left of the point is not a sign.  The string left must
!            be an integer, the string to the right must be an
!            unsigned integer.
!
             bedec =       beint  ( string(1:c) ) &
                     .and. beuns  ( string(e:l) )

         end if

      end if

      return
      end function bedec

      !> Determine whether a string represents an integer.
      !>
      !> The function returns .TRUE. if the string represents an integer.
      !> Otherwise, it returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is any string.
      !>
      !>## Detailed Output
      !>
      !>     If the input string contains an integer (as defined in
      !>     $Particulars below), the function returns .TRUE. Otherwise,
      !>     the function returns .FALSE.
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
      !>     An integer may be either of the following:
      !>
      !>        1) An unsigned integer (as defined by function BEUNS).
      !>
      !>        2) A sign ('+' or '-') followed by an unsigned
      !>           integer.
      !>
      !>## Examples
      !>
      !>     Four classes of numbers recognized by the various BE functions.
      !>
      !>        UNS      unsigned integer
      !>        INT      integer                (includes INT)
      !>        DEC      decimal number         (includes UNS, INT)
      !>        NUM      number                 (includes UNS, INT, NUM)
      !>
      !>     The following table illustrates the differences between
      !>     the classes. (Any number of leading and trailing blanks
      !>     are acceptable.)
      !>
      !>        String                  Accepted by
      !>        ------------------      ------------------
      !>        0                       UNS, INT, DEC, NUM
      !>        21
      !>        21994217453648
      !>
      !>        +0                      INT, DEC, NUM
      !>        -13
      !>        +21946
      !>
      !>        1.23                    DEC, NUM
      !>        12.
      !>        .17
      !>        +4.1
      !>        -.25
      !>
      !>        2.3e17                  NUM
      !>        17.D-13275849
      !>        -.194265E+0004
      !>
      !>     Note that the functions don't take the magnitudes of the numbers
      !>     into account. They may accept numbers that cannot be represented
      !>     in Fortran variables. (For example, '2.19E999999999999' probably
      !>     exceeds the maximum floating point number on any machine, but
      !>     is perfectly acceptable to BENUM.)
      !>
      !>     The following strings are not accepted by any of the functions.
      !>
      !>        String             Reason
      !>        ---------------    ----------------------------------------
      !>        3/4                No implied operations (rational numbers)
      !>        37+14              No explicit operations
      !>        E12                Must have mantissa
      !>        217,346.91         No commas
      !>        3.14 159 264       No embedded spaces
      !>        PI                 No special numbers
      !>        FIVE               No textual numbers
      !>        CXIV               No roman numerals
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
      !>-    SPICELIB Version 1.1.0, 24-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      elemental logical function beint  ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Character string.

      integer(ip) :: i
      integer(ip) :: l

      character(1) :: letter

!
!     Find the first non-blank character and the length of the
!     string.
!
      l  =  len    ( string )
      i  =  verify(string, ' ')

!
!     If there isn't a non-blank character, this isn't an
!     integer.
!
      if ( i == 0 ) then
         beint  = .false.
         return
      end if

!
!     Copy the first non-blank letter in the string.
!
      letter = string(i:i)

      if      ( i < l ) then

!
!        The first character is not the last, so we might start with
!        a plus or minus.  If so the rest must be an unsigned integer.
!
         if (      ( letter == '+' ) &
              .or. ( letter == '-' ) ) then

            i    = i + 1

            if ( string(i:i) /= ' ' ) then
               beint = beuns ( string(i:) )
            else
               beint = .false.
            end if

         else

!
!           If the first character isn't plus (+) or minus (-)
!           the string must be an unsigned integer if its going
!           to be an integer.
!
            beint = beuns ( string(i:) )

         end if

      else

!
!        If the first (non-blank) character is the last one, then
!        it must be an unsigned integer, for the string to
!        represent an integer.
!
         beint = beuns ( letter )

      end if

      return
      end function beint

      !> Determine whether a string represents a number.
      !>
      !> The function returns .TRUE. if the string is a number.
      !> Otherwise, it returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is any string.
      !>
      !>## Detailed Output
      !>
      !>     If the input string contains a number (as defined in
      !>     $Particulars below) the function returns .TRUE. Otherwise,
      !>     the function returns .FALSE.
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
      !>     A number may be either of the following:
      !>
      !>        1) A decimal number (as defined by function BEDEC).
      !>
      !>        2) A decimal number followed by an exponent character
      !>           ('E', 'e', 'D', or 'd') and an integer (as defined
      !>           by function BEINT).
      !>
      !>## Examples
      !>
      !>     Four classes of numbers recognized by the various BE functions.
      !>
      !>        UNS      unsigned integer
      !>        INT      integer                (includes INT)
      !>        DEC      decimal number         (includes UNS, INT)
      !>        NUM      number                 (includes UNS, INT, NUM)
      !>
      !>     The following table illustrates the differences between
      !>     the classes. (Any number of leading and trailing blanks
      !>     are acceptable.)
      !>
      !>        String                  Accepted by
      !>        ------------------      ------------------
      !>        0                       UNS, INT, DEC, NUM
      !>        21
      !>        21994217453648
      !>
      !>        +0                      INT, DEC, NUM
      !>        -13
      !>        +21946
      !>
      !>        1.23                    DEC, NUM
      !>        12.
      !>        .17
      !>        +4.1
      !>        -.25
      !>
      !>        2.3e17                  NUM
      !>        17.D-13275849
      !>        -.194265E+0004
      !>
      !>     Note that the functions don't take the magnitudes of the numbers
      !>     into account. They may accept numbers that cannot be represented
      !>     in Fortran variables. (For example, '2.19E999999999999' probably
      !>     exceeds the maximum floating point number on any machine, but
      !>     is perfectly acceptable to BENUM.)
      !>
      !>     The following strings are not accepted by any of the functions.
      !>
      !>        String             Reason
      !>        ---------------    ----------------------------------------
      !>        3/4                No implied operations (rational numbers)
      !>        37+14              No explicit operations
      !>        E12                Must have mantissa
      !>        217,346.91         No commas
      !>        3.14 159 264       No embedded spaces
      !>        PI                 No special numbers
      !>        FIVE               No textual numbers
      !>        CXIV               No roman numerals
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
      !>-    SPICELIB Version 1.1.0, 24-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      elemental logical function benum  ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Character string.

      integer(ip) :: d
      integer(ip) :: e
      integer(ip) :: f
      integer(ip) :: l

!
!     Determine whether or not there is an exponent character in the
!     string.
!
      l = len  ( string            )
      e = cpos ( string, 'EeDd', 1 )
      d = e-1
      f = e+1

      if ( e == 0 ) then

!
!        There is no exponent character, this is a number if it
!        is a decimal number.
!
         benum = bedec ( string )

      else if (     ( e == 1 ) &
               .or. ( e == l )  ) then

         benum = .false.

      else if (      ( string(d:d) == ' ' ) &
                .or. ( string(f:f) == ' ' ) ) then

         benum = .false.

      else

         benum =      bedec ( string(1:d) ) &
                .and. beint ( string(f:l) )

      end if

      return
      end function benum

      !> Determine whether a string represents an unsigned integer.
      !>
      !> The function returns .TRUE. if the string represents an unsigned
      !> integer. Otherwise, it returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is any string.
      !>
      !>## Detailed Output
      !>
      !>     If STRING contains a single word made entirely from the
      !>     characters '0' through '9', then the function returns .TRUE.
      !>     Otherwise, it returns .FALSE.
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
      !>     By definition an unsigned integer is a word made exclusively
      !>     from the characters '0', '1', '2', '3', '4', '5', '6', '7', '8',
      !>     and '9'.
      !>
      !>## Examples
      !>
      !>     Four classes of numbers recognized by the various BE functions.
      !>
      !>        UNS      unsigned integer
      !>        INT      integer                (includes INT)
      !>        DEC      decimal number         (includes UNS, INT)
      !>        NUM      number                 (includes UNS, INT, NUM)
      !>
      !>     The following table illustrates the differences between
      !>     the classes. (Any number of leading and trailing blanks
      !>     are acceptable.)
      !>
      !>        String                  Accepted by
      !>        ------------------      ------------------
      !>        0                       UNS, INT, DEC, NUM
      !>        21
      !>        21994217453648
      !>
      !>        +0                      INT, DEC, NUM
      !>        -13
      !>        +21946
      !>
      !>        1.23                    DEC, NUM
      !>        12.
      !>        .17
      !>        +4.1
      !>        -.25
      !>
      !>        2.3e17                  NUM
      !>        17.D-13275849
      !>        -.194265E+0004
      !>
      !>     Note that the functions don't take the magnitudes of the numbers
      !>     into account. They may accept numbers that cannot be represented
      !>     in Fortran variables. (For example, '2.19E999999999999' probably
      !>     exceeds the maximum floating point number on any machine, but
      !>     is perfectly acceptable to BENUM.)
      !>
      !>     The following strings are not accepted by any of the functions.
      !>
      !>        String             Reason
      !>        ---------------    ----------------------------------------
      !>        3/4                No implied operations (rational numbers)
      !>        37+14              No explicit operations
      !>        E12                Must have mantissa
      !>        217,346.91         No commas
      !>        3.14 159 264       No embedded spaces
      !>        PI                 No special numbers
      !>        FIVE               No textual numbers
      !>        CXIV               No roman numerals
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
      !>-    SPICELIB Version 1.1.0, 24-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      elemental logical function beuns  ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Character string.

!
!     SPICE functions
!

      integer(ip) :: i
      integer(ip) :: l

      logical :: ok

!
!     Get the length of the string and the position of its
!     first non-blank character.
!
      l     = len    ( string )
      i     = verify(string, ' ')

!
!     If there isn't a non-blank character, this isn't an
!     unsigned integer.
!
      if ( i == 0 ) then
         beuns  = .false.
         return
      end if

!
!     As far as we know right now, everything is ok.  Examine
!     characters until we run out of string or until we
!     hit a non-digit character.
!
      ok = .true.

      do while ( ok .and. ( i <= l ) )

         if ( index('0123456789', string(i:i) ) > 0  ) then
            i = i + 1
         else
            ok = .false.
         end if

      end do

!
!     If the string still is ok as an unsigned integer, it must be
!     one...
!
      if ( ok ) then
         beuns  = .true.
      else

!
!     ... otherwise, it's an unsigned integer if the remainder is blank.
!
         beuns  = string(i:) == ' '
      end if

      return
      end function beuns

      !> Compress a character string by removing occurrences of
      !> more than N consecutive occurrences of a specified
      !> character.
      !>
      !>## Detailed Input
      !>
      !>     DELIM    is the delimiter to be compressed out of the string.
      !>              This may be any ASCII character.
      !>
      !>     N        is the maximum number of consecutive occurrences
      !>              of DELIM that will be allowed to remain in the
      !>              output string.
      !>
      !>     INPUT    is the input string.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT   is the output string. This is the input string
      !>              with all occurrences of more than N consecutive
      !>              delimiters removed.
      !>
      !>              If OUTPUT is not large enough to hold the
      !>              compressed string, it is truncated on the right.
      !>
      !>              OUTPUT may overwrite INPUT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the output string length is too short to contain the result
      !>         of compressing the input string, the result is truncated on
      !>         the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Occurrences of more than N consecutive delimiters are removed
      !>     from the input string as it is copied to the output string.
      !>     If the output string is not large enough to hold the compressed
      !>     string, it is truncated on the right.
      !>
      !>## Examples
      !>
      !>     Let DELIM = '.', and N = 2. Then
      !>
      !>        'ABC...DE.F...',           becomes   'ABC..DE.F..'
      !>        ' ...........'                       ' ..'
      !>        '.. ..AB....CD'                      '.. ..AB..CD'
      !>
      !>     Let DELIM = ' ', and N = 0. Then
      !>
      !>        ' DISK:[USER.  SUB  ]'     becomes   'DISK:[USER.SUB]'
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 09-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added $Exceptions entry #1.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      elemental subroutine cmprss ( delim, n, input, output )
      use pepper_kinds, only: ip
      implicit none

      character(1), intent(in) :: delim  !! Delimiter to be compressed.
      integer(ip), intent(in) :: n  !! Maximum consecutive occurrences of DELIM.
      character(*), intent(in) :: input  !! Input string.
      character(*), intent(out) :: output  !! Compressed string.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: count

      integer(ip) :: inlen
      integer(ip) :: outlen

!
!     Find out how much space there is in the INPUT and OUTPUT strings
!     and initialize the delimiter counter and output place holder.
!
      inlen  = len(input)
      outlen = len(output)

      count  = 0
      j      = 0

      do i = 1, inlen

!
!        Check each character to see if it is a delimiter or not.
!
         if ( input(i:i) == delim ) then

            count = count + 1

!
!           Copy delimiters until enough consecutive delimiters
!           have been accumulated.  When enough consecutive delimiters
!           have accumulated, we no longer copy them.
!
            if ( count <= n ) then
               j           = j + 1
               output(j:j) = input(i:i)
            end if

         else

!
!           We don't have a delimiter here so we just copy the
!           character and set the delimiter counter to zero.
!
            count       = 0
            j           = j + 1
            output(j:j) = input(i:i)

         end if

         if ( j == outlen ) then
            return
         end if

      end do

!
!     Pad any left over space in the output string with blanks.
!
      if ( j < outlen ) then
         output(j+1:) = ' '
      end if

      return
      end subroutine cmprss

      !> Find the first occurrence in a string of a character belonging
      !> to a collection of characters, starting at a specified location,
      !> searching forward.
      !>
      !> The function returns the index of the first character of STR
      !> at or following index START that is in the collection CHARS.
      !>
      !>## Detailed Input
      !>
      !>     STR      is any character string.
      !>
      !>     CHARS    is a character string containing a collection
      !>              of characters. Spaces in CHARS are significant.
      !>
      !>     START    is the position in STR to begin looking for one of
      !>              the characters in CHARS.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first character of STR
      !>     (at or following index START) that is one of the characters in
      !>     the string CHARS. If none of the characters is found, the
      !>     function returns zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If START is less than 1, the search begins at the first
      !>         character of the string.
      !>
      !>     2)  If START is greater than the length of the string, CPOS
      !>         returns zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     CPOS is case sensitive.
      !>
      !>     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
      !>     POSR, CPOSR, NCPOSR) is described in the Required Reading.
      !>
      !>     Those familiar with the True BASIC language should note that
      !>     these functions are equivalent to the True BASIC intrinsic
      !>     functions with the same names.
      !>
      !>## Examples
      !>
      !>     Let STRING = 'BOB, JOHN, TED, AND MARTIN....'
      !>                   123456789012345678901234567890
      !>
      !>     Normal (sequential) searching
      !>     -----------------------------
      !>
      !>           CPOS( STRING, ' ,',    1  ) =  4
      !>
      !>           CPOS( STRING, ' ,',    5  ) =  5
      !>
      !>           CPOS( STRING, ' ,',    6  ) = 10
      !>
      !>           CPOS( STRING, ' ,',    11 ) = 11
      !>
      !>           CPOS( STRING, ' ,',    12 ) = 15
      !>
      !>           CPOS( STRING, ' ,',    16 ) = 16
      !>
      !>           CPOS( STRING, ' ,',    17 ) = 20
      !>
      !>           CPOS( STRING, ' ,',    21 ) = 0
      !>
      !>     START out of bounds
      !>     -------------------
      !>
      !>           CPOS( STRING, ' ,',  -113 ) = 4
      !>
      !>           CPOS( STRING, ' ,',    -1 ) = 4
      !>
      !>           CPOS( STRING, ' ,',    31 ) = 0
      !>
      !>           CPOS( STRING, ' ,',  1231 ) = 0
      !>
      !>     Order within CHARS
      !>     ------------------
      !>
      !>           CPOS( STRING, ',. ',   22 ) = 27
      !>
      !>           CPOS( STRING, ' ,.',   22 ) = 27
      !>
      !>           CPOS( STRING, ', .',   22 ) = 27
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
      !>
      !>        The Required Reading file POSITION was renamed to SCANNING.
      !>        This header was updated to reflect the change.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function cpos ( str, chars, start )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str  !! Any character string.
      character(*), intent(in) :: chars  !! A collection of characters.
      integer(ip), intent(in) :: start  !! Position to begin looking for one of CHARS

      logical :: found
      integer(ip) :: lenstr
      integer(ip) :: b

      lenstr = len ( str )

      b      = max(1,start)
      found  = .false.
      cpos   = 0

      do while ( .not. found )

         if ( b > lenstr ) then
            return
         else if ( index( chars, str(b:b) ) /= 0 ) then
            cpos = b
            return
         else
            b    = b + 1
         end if

      end do

      return
      end function cpos

      !> Find the first occurrence in a string of a character belonging
      !> to a collection of characters, starting at a specified location,
      !> searching in reverse.
      !>
      !> The function returns the index of the last character of STR
      !> at or before index START that is in the collection CHARS.
      !>
      !>## Detailed Input
      !>
      !>     STR      is any character string.
      !>
      !>     CHARS    is a character string containing a collection of
      !>              characters. Spaces in CHARS are significant.
      !>
      !>     START    is the position in STR to begin looking for one of the
      !>              characters in CHARS.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the last character of STR (at
      !>     or before index START) that is one of the characters in the
      !>     string CHARS. If none of the characters is found, the function
      !>     returns zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If START is less than 1, CPOSR returns zero.
      !>
      !>     2)  If START is greater than LEN(STRING), the search begins
      !>         at the last character of the string.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     CPOSR is case sensitive.
      !>
      !>     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
      !>     POSR, CPOSR, NCPOSR) is described in the Required Reading.
      !>
      !>     Those familiar with the True BASIC language should note that
      !>     these functions are equivalent to the True BASIC intrinsic
      !>     functions with the same name.
      !>
      !>## Examples
      !>
      !>     Let STRING = 'BOB, JOHN, TED, AND MARTIN    '
      !>                   123456789012345678901234567890
      !>
      !>     Normal (sequential) searching:
      !>     ------------------------------
      !>
      !>           CPOSR( STRING, ' ,',    30 ) = 30
      !>
      !>           CPOSR( STRING, ' ,',    29 ) = 29
      !>
      !>           CPOSR( STRING, ' ,',    28 ) = 28
      !>
      !>           CPOSR( STRING, ' ,',    27 ) = 27
      !>
      !>           CPOSR( STRING, ' ,',    26 ) = 20
      !>
      !>           CPOSR( STRING, ' ,',    19 ) = 16
      !>
      !>           CPOSR( STRING, ' ,',    15 ) = 15
      !>
      !>           CPOSR( STRING, ' ,',    14 ) = 11
      !>
      !>           CPOSR( STRING, ' ,',    10 ) = 10
      !>
      !>           CPOSR( STRING, ' ,',     9 ) =  5
      !>
      !>           CPOSR( STRING, ' ,',     4 ) =  4
      !>
      !>           CPOSR( STRING, ' ,',     3 ) =  0
      !>
      !>     START out of bounds:
      !>     --------------------
      !>
      !>           CPOSR( STRING, ' ,',   231 ) = 30
      !>
      !>           CPOSR( STRING, ' ,',    31 ) = 30
      !>
      !>           CPOSR( STRING, ' ,',     0 ) =  0
      !>
      !>           CPOSR( STRING, ' ,',   -10 ) =  0
      !>
      !>     Order within CHARS
      !>     ------------------
      !>
      !>           CPOSR( STRING, 'JOHN',  23 ) =  18
      !>
      !>           CPOSR( STRING, 'OHNJ',  23 ) =  18
      !>
      !>           CPOSR( STRING, 'HNJO',  23 ) =  18
      !>
      !>           CPOSR( STRING, 'NJOH',  23 ) =  18
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
      !>
      !>        The Required Reading file POSITION was renamed to SCANNING.
      !>        This header was updated to reflect the change.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function cposr ( str, chars, start )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str  !! Any character string.
      character(*), intent(in) :: chars  !! A collection of characters.
      integer(ip), intent(in) :: start  !! Position to begin looking for one of CHARS

      logical :: found
      integer(ip) :: lenstr
      integer(ip) :: b

      lenstr = len ( str )

      b      = min(lenstr,start)
      found  = .false.
      cposr  = 0

      do while ( .not. found )

         if ( b <= 0 ) then
            return
         else if ( index( chars, str(b:b) ) /= 0 ) then
            cposr = b
            return
         else
            b     = b - 1
         end if

      end do

      return
      end function cposr

      !> Determine whether two strings are equivalent.
      !>
      !> The function returns .TRUE. if A and B are equivalent.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are arbitrary character strings.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if A and B are equivalent: that is,
      !>     if A and B contain  the same characters in the same order,
      !>     when blanks are ignored and uppercase and lowercase characters
      !>     are considered equal.
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
      !>     This routine is provided for those cases in which two strings
      !>     must be compared, and in which allowances are to be made for
      !>     extra (leading, trailing, and embedded) blanks and differences
      !>     in case. For the most part,
      !>
      !>        EQSTR ( A, B )
      !>
      !>     is .TRUE. whenever
      !>
      !>        CALL CMPRSS ( ' ', 0, A, TEMPA )
      !>        CALL UCASE  (            TEMPA, TEMPA )
      !>
      !>        CALL CMPRSS ( ' ', 0, B, TEMPB )
      !>        CALL UCASE  (            TEMPB, TEMPB )
      !>
      !>        EQVLNT = TEMPA .EQ. TEMPB
      !>
      !>     is .TRUE. There are two important differences, however.
      !>
      !>        1) The single reference to EQSTR is much simpler to
      !>           write, and simpler to understand.
      !>
      !>        2) The reference to EQSTR does not require any temporary
      !>           storage, nor does it require that the strings A and B
      !>           be changed. This feature is especially useful when
      !>           comparing strings received as subprogram arguments
      !>           against strings stored internally within the subprogram.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) This code provides examples of equivalent and non-equivalent
      !>        strings according to the algorithm implemented in EQSTR.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EQSTR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL               EQSTR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 9  )
      !>
      !>      INTEGER               STRLEN
      !>      PARAMETER           ( STRLEN = 22 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)    STR1   ( SETSIZ )
      !>      CHARACTER*(STRLEN)    STR2   ( SETSIZ )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Initialize the two arrays of strings.
      !>C
      !>      DATA                  STR1   / 'A short string   ',
      !>     .                               'Embedded        blanks',
      !>     .                               'Embedded        blanks',
      !>     .                               ' ',
      !>     .                               'One word left out',
      !>     .                               'Extra [] delimiters',
      !>     .                               'Testing 1, 2, 3',
      !>     .                               'Case insensitive',
      !>     .                               'Steve'  /
      !>
      !>      DATA                  STR2   / 'ashortstring',
      !>     .                               'Em be dd ed bl an ks',
      !>     .                               '   Embeddedblanks',
      !>     .                               '          ',
      !>     .                               'WORD LEFT OUT',
      !>     .                               'extradelimiters',
      !>     .                               'TESTING123',
      !>     .                               'Case Insensitive',
      !>     .                               '  S t E v E  '  /
      !>
      !>C
      !>C     Compare the two arrays.
      !>C
      !>      DO I = 1, SETSIZ
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,*) 'STR1 : ', STR1(I)
      !>         WRITE(*,*) 'STR2 : ', STR2(I)
      !>
      !>         IF ( EQSTR( STR1(I), STR2(I) ) ) THEN
      !>
      !>            WRITE(*,*) 'EQSTR: equivalent.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'EQSTR: NOT equivalent.'
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
      !>         STR1 : A short string
      !>         STR2 : ashortstring
      !>         EQSTR: equivalent.
      !>
      !>         STR1 : Embedded        blanks
      !>         STR2 : Em be dd ed bl an ks
      !>         EQSTR: equivalent.
      !>
      !>         STR1 : Embedded        blanks
      !>         STR2 :    Embeddedblanks
      !>         EQSTR: equivalent.
      !>
      !>         STR1 :
      !>         STR2 :
      !>         EQSTR: equivalent.
      !>
      !>         STR1 : One word left out
      !>         STR2 : WORD LEFT OUT
      !>         EQSTR: NOT equivalent.
      !>
      !>         STR1 : Extra [] delimiters
      !>         STR2 : extradelimiters
      !>         EQSTR: NOT equivalent.
      !>
      !>         STR1 : Testing 1, 2, 3
      !>         STR2 : TESTING123
      !>         EQSTR: NOT equivalent.
      !>
      !>         STR1 : Case insensitive
      !>         STR2 : Case Insensitive
      !>         EQSTR: equivalent.
      !>
      !>         STR1 : Steve
      !>         STR2 :   S t E v E
      !>         EQSTR: equivalent.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example fragments.
      !>
      !>-    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)
      !>
      !>        Code changed to eliminate DO WHILE ( .TRUE. ) construct.
      !>        The purpose of the change was to eliminate compilation
      !>        diagnostics relating to unreachable statements. The code
      !>        ran just fine before this change.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB)
      !>
      !>        Loop termination condition fixed.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)
      !>
      !>        Code changed to eliminate DO WHILE ( .TRUE. ) construct.
      !>        The purpose of the change was to eliminate compilation
      !>        diagnostics relating to unreachable statements.
      !>
      !>        Changed some statements of form
      !>
      !>           IF <condition> <statement>
      !>
      !>        to
      !>
      !>           IF <condition> THEN
      !>
      !>             <statement>
      !>
      !>           END IF
      !>
      !>-    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB)
      !>
      !>        Loop termination condition fixed. The routine now checks
      !>        the termination case where both string pointers are pointing
      !>        to blanks, and at least one pointer has a value greater than
      !>        the length of the string it corresponds to. Internal comments
      !>        were updated accordingly.
      elemental logical function eqstr ( a, b )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: a  !! Arbitrary character strings.
      character(*), intent(in) :: b  !! Arbitrary character strings.

      integer(ip) :: pa
      integer(ip) :: pb

      integer(ip) :: lena
      integer(ip) :: lenb

      integer(ip) :: ca
      integer(ip) :: cb

      integer(ip) :: lbound
      integer(ip) :: ubound
      integer(ip) :: delta

      logical :: done

!
!     The general plan is to move a pair of pointers (PA, PB)
!     through strings A and B, skipping blank characters and
!     comparing others one-for-one.
!
!        Repeat:
!
!           If (A is blank) then
!              Increment A
!
!           Else if (B is blank) then
!              Increment B
!
!           Else
!              If (A and B are equivalent) then
!                 Increment A and B
!              Else
!                 Return .FALSE.
!
!           If (A and B are past end) then
!              Return .TRUE.
!
!           Else if (A or B is past end and other is non-blank) then
!              Return .FALSE.
!
!           Else if (A or B is past end and other is blank) then
!              Return .TRUE.
!
!     Note that no pointer gets incremented more than once on each
!     pass through the loop.
!
!     On the other hand, in many cases the strings will be exactly
!     equal. If so, why knock ourselves out?
!
      if ( a == b ) then

         eqstr  = .true.
         return

      else

         pa = 1
         pb = 1

         lena = len ( a )
         lenb = len ( b )

         lbound = ichar ( 'a' )
         ubound = ichar ( 'z' )
         delta  = ichar ( 'A' ) - ichar ( 'a' )

         done   = .false.

         do while ( .not. done )

!
!           At this point, we're guaranteed that
!
!             ( PA .LE. LENA )   and   ( PB .LE. LENB )
!

            if ( a(pa:pa) == ' ' ) then
               pa = pa + 1

            else if ( b(pb:pb) == ' ' ) then
               pb = pb + 1

            else

               ca = ichar ( a(pa:pa) )
               cb = ichar ( b(pb:pb) )

               if ( ca >= lbound  .and.  ca <= ubound ) then
                  ca = ca + delta
               end if

               if ( cb >= lbound  .and.  cb <= ubound ) then
                  cb = cb + delta
               end if

               if ( ca == cb ) then

                  pa = pa + 1
                  pb = pb + 1

               else

                  eqstr  = .false.
                  done   = .true.
!
!                 We'll return from this point, having taken no further
!                 action.
!
               end if

            end if

            if ( .not. done ) then

               if ( pa > lena ) then
!
!                 Whichever of the following tests passes, we're going
!                 to have a verdict at the end of the IF block below.
!
                  if ( pb > lenb ) then
                     eqstr  = .true.

                  else if ( b(pb: ) /= ' ' ) then
                     eqstr  = .false.

                  else
                     eqstr  = .true.

                  end if

                  done  =  .true.
!
!                 We'll return from this point, having taken no further
!                 action.
!

               else if ( pb > lenb ) then
!
!                 Whichever of the following tests passes, we're going
!                 to have a verdict at the end of the IF block below.
!
                  if ( a(pa: ) /= ' ' ) then
                     eqstr  = .false.
                  else
                     eqstr  = .true.
                  end if

                  done  =  .true.
!
!                 We'll return from this point, having taken no further
!                 action.
!
               end if

            end if

         end do

      end if

      end function eqstr

      !> Search for a given value within a character string array.
      !> Return the index of the first equivalent array entry, or zero
      !> if no equivalent element is found.
      !>
      !> The function returns the index of the first array entry equivalent
      !> to VALUE, or zero if none is found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is an arbitrary character string.
      !>
      !>     NDIM     is the dimension of (number of elements in) an array of
      !>              character strings.
      !>
      !>     ARRAY    is the array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first element of the input
      !>     array equivalent to the input value, or zero if the array contains
      !>     no such elements.
      !>
      !>     Two strings are equivalent if they contain the same characters in
      !>     the same order, when blanks are ignored and uppercase and
      !>     lowercase characters are considered equal.
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
      !>     ESRCHC is identical to ISRCHC, except that it looks for the first
      !>     equivalent string (as defined by EQSTR) instead of the first
      !>     identical one.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements:
      !>
      !>        ARRAY(1) = 'This'
      !>        ARRAY(2) = 'little'
      !>        ARRAY(3) = 'piggy'
      !>        ARRAY(4) = 'went'
      !>        ARRAY(5) = 'to'
      !>        ARRAY(6) = 'market'
      !>
      !>     Then
      !>
      !>        ESRCHC ( 'PIGGY',      6, ARRAY )  =  3
      !>        ESRCHC ( ' LiTtLe  ',  6, ARRAY )  =  2
      !>        ESRCHC ( 'W e n t',    6, ARRAY )  =  4
      !>        ESRCHC ( 'mall',       6, ARRAY )  =  0
      !>
      !>## Restrictions
      !>
      !>     1)  ESRCHC assumes that the function EQSTR does not participate
      !>         in normal SPICELIB error handling.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure integer(ip) function esrchc ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: value  !! Key value to be found in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      character(*), intent(in) :: array ( * )  !! Character string array to search.

      integer(ip) :: i

!
!     Just like ISRCHC.
!
      esrchc = 0

      do i = 1, ndim

         if ( eqstr ( array(i), value ) ) then
            esrchc = i
            return
         end if

      end do

      return
      end function esrchc

      !> Find the beginning and end of the first word starting at
      !> or after a specified character.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string that potentially consists of
      !>              words of text.
      !>
      !>     START    is the index of a letter within the string from which
      !>              to start looking for the next word.
      !>
      !>## Detailed Output
      !>
      !>     B        is the index of the first letter of the word substring
      !>              of STRING that begins at or after position START. If
      !>              there are no such substrings I is returned as 0.
      !>
      !>     E        is the index of the last letter of the word substring
      !>              of STRING that begins at or after position START. If
      !>              there are no such substrings J is returned as 0.
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
      !>     Given a character string and location of a character within that
      !>     string, this routine finds the first full word of the string
      !>     that starts on or after the specified location.
      !>
      !>## Examples
      !>
      !>     1         2         3         4         5
      !>              12345678901234567890123456789012345678901234567890
      !>     STRING: 'Now is the time for all good men to go home to bed'
      !>
      !>     START    I      J
      !>     -----   ---    ---
      !>     1        1      3
      !>     2        5      6
      !>     3        5      6
      !>     4        5      6
      !>     5        5      6
      !>     6        8      10
      !>     7        8      10
      !>     8        8      10
      !>     9        12     15
      !>
      !>     48       48     50
      !>     49       0      0
      !>     111      0      0
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
      !>-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 15-OCT-1993 (WLT)
      !>
      !>        The routine was completely rewritten with a resulting
      !>        increase in execution speed of between 2000% and 6000%.
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
      !>-     SPICELIB Version 2.0.0, 15-OCT-1993 (WLT)
      !>
      !>         The routine was completely rewritten with a resulting
      !>         increase in execution speed of between 2000% and 6000%.
      !>         It was tested against the old version of the routine to
      !>         ensure that the functionality was exactly duplicated.
      elemental subroutine fndnwd ( string, start, b, e )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! A string to examine for words.
      integer(ip), intent(in) :: start  !! Position in the string to start looking for words.
      integer(ip), intent(out) :: b  !! String position of first character of the word.
      integer(ip), intent(out) :: e  !! String position of last character of the word.

      integer(ip) :: blank
      integer(ip) :: l
      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: size

      logical :: lastn
      logical :: thisb

!
!     Set up neede parameters and check obvious out-of-bound cases.
!
      blank = ichar(' ')
      size  = len(string)

      if ( start > size ) then
         b = 0
         e = 0
         return
      end if

      n = max(1,start)
      l = n - 1

      if ( l <= 0 ) then
         lastn = .false.
      else
         lastn = ichar( string(l:l) ) /= blank
      end if

      thisb = ichar( string(n:n) ) == blank

!
!     Search for the beginning of a word (the last character
!     blank and the current non-blank).
!
      do while ( thisb .or. lastn )

         n = n + 1

         if ( n > size ) then
            b = 0
            e = 0
            return
         end if

         lastn = .not.   thisb
         thisb =  ichar( string(n:n) ) == blank

      end do

!
!     If we get this far, we found the beginning of the
!     string.  To find the end look for the next blank and
!     back up one.
!
      b     =  n

      do i = n+1, size

         if ( ichar(string(i:i)) == blank ) then
            e = i - 1
            return
         end if

      end do

!
!     If we get this far, the word ends at the end of the
!     string.
!
      e = size
      return

      end subroutine fndnwd

      !> Return the index of the first non-blank character in
      !> a character string.
      !>
      !> The function returns the index of the first non-blank character in
      !> STRING.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (VERIFY instead).
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index if the first non-blank character in
      !>     the input string. If there are no non-blank characters in the
      !>     string, FRSTNB is zero.
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
      !>     If the string is blank, return zero. Otherwise, step through
      !>     the string one character at a time until something other than
      !>     a blank is found. Return the index of that something within
      !>     the string.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of FRSTNB.
      !>
      !>           FRSTNB ( 'ABCDE'         )   = 1
      !>           FRSTNB ( 'AN EXAMPLE'    )   = 1
      !>           FRSTNB ( '   AN EXAMPLE' )   = 4
      !>           FRSTNB ( '             ' )   = 0
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
      !>
      !>        Modified the comparison to use integer values and the ICHAR()
      !>        function. This improves the performance of the subroutine.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental integer(ip) function frstnb ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

!
!     Just like it says in the header.
!
      frstnb = verify(string, ' ')
      end function frstnb

      !> Return the index of the first non-printable character in a
      !> character string. ASCII characters 32-126 are considered
      !> printable by this routine. (Blanks are considered printable.)
      !>
      !> The function returns the index of first non-printable character in
      !> STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first non-printable
      !>     character in the input string. Characters having integer codes
      !>     outside the range 32-126 are considered to be non-printable
      !>     characters. Blanks are considered to be printable characters. If
      !>     the input string contains no non-printable characters, FRSTNP is
      !>     zero.
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
      !>     This routine may be used to assist in validating strings that
      !>     are intended to be free of non-printable characters.
      !>
      !>     This routine treats blanks as printable characters. This choice
      !>     prevents embedded blanks from causing false positive results in
      !>     tests of strings for invalid characters. Note that the routine
      !>     FRSTPC and LASTPC treat blanks as non-printable.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the first non-printable character in a given string,
      !>        considering blanks as printable characters.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM FRSTNP_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               FRSTNP
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*10    S
      !>
      !>C
      !>C     Add some random characters to the string, some of them
      !>C     non-printable.
      !>C
      !>      S( 1: 1) = 'A'
      !>      S( 2: 2) = ' '
      !>      S( 3: 3) = CHAR ( 3 )
      !>      S( 4: 4) = 'A'
      !>      S( 5: 5) = 'B'
      !>      S( 6: 6) = 'C'
      !>      S( 7: 7) = CHAR ( 7 )
      !>      S( 8: 8) = CHAR ( 8 )
      !>      S( 9: 9) = CHAR ( 9 )
      !>      S(10:10) = ' '
      !>
      !>      WRITE (*,'(A,I2)') 'First non-printable character '
      !>     .//                 'found at index', FRSTNP(S)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        First non-printable character found at index 3
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 07-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Created
      !>        complete code example from existing fragment.
      !>
      !>-    SPICELIB Version 1.0.0, 16-JUN-1995 (NJB) (IMU)
      elemental integer(ip) function frstnp ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

      integer(ip) :: i

!
!     Look for the first character outside the range [32,126], and
!     return its index.
!
      do i = 1, len ( string )

         if (       (ichar ( string(i:i) ) <  32) &
              .or.  (ichar ( string(i:i) ) > 126) ) then

            frstnp = i
            return
         end if

      end do

!
!     Still here? All characters are printable. Return zero.
!
      frstnp = 0

      return
      end function frstnp

      !> Return the index of the first printable character in a character
      !> string. ASCII characters 33-126 are printable. (Blanks are not
      !> considered printable.)
      !>
      !> The function returns the index of the first non-blank printable
      !> character in STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first printable character in
      !>     the input string.
      !>
      !>     ASCII characters 33-126 are considered to be printable characters.
      !>     Blanks are not considered printable characters. If the input
      !>     string contains no printable characters, FRSTPC is zero.
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
      !>     This works exactly like FRSTNB, except that it skips non-printable
      !>     characters (ASCII control characters) as well as blanks.
      !>
      !>## Examples
      !>
      !>     The program
      !>
      !>        INTEGER         FRSTNB
      !>        INTEGER         FRSTPC
      !>        INTEGER         LASTNB
      !>        INTEGER         LASTPC
      !>
      !>        CHARACTER*10    S
      !>
      !>        S( 1: 1) = ' '
      !>        S( 2: 2) = CHAR (  2 )
      !>        S( 3: 3) = CHAR (  3 )
      !>        S( 4: 4) = 'A'
      !>        S( 5: 5) = 'B'
      !>        S( 6: 6) = 'C'
      !>        S( 7: 7) = CHAR (  7 )
      !>        S( 8: 8) = CHAR (  8 )
      !>        S( 9: 9) = CHAR (  9 )
      !>        S(10:10) = ' '
      !>
      !>        WRITE (*,*) 'Non-blank from ', FRSTNB(S), ' to ', LASTNB(S)
      !>        WRITE (*,*) 'Printable from ', FRSTPC(S), ' to ', LASTPC(S)
      !>
      !>        END
      !>
      !>     produces the following output:
      !>
      !>        Non-blank from 2 to 9.
      !>        Printable from 4 to 6.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental integer(ip) function frstpc ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

      integer(ip) :: i

!
!     Look for the first character in the range [33,126], and return
!     its index.
!
      do i = 1, len ( string )

         if (       (ichar ( string(i:i) ) >=  33) &
              .and. (ichar ( string(i:i) ) <= 126) ) then

            frstpc = i
            return
         end if

      end do

!
!     Still here? No printable characters. Return zero.
!
      frstpc = 0

      return
      end function frstpc

      !> Locate a keyword in a string and extract the substring from
      !> the beginning of the first word following the keyword to the
      !> beginning of the first subsequent recognized terminator of a list.
      !>
      !>## Detailed Input
      !>
      !>     KEYWD    is a word used to mark the start of text of interest.
      !>
      !>     TERMS    is a set of words, any one of which may signal the end of
      !>              text of interest.
      !>
      !>     NTERMS   is the number of TERMS.
      !>
      !>     WORDSQ   is a character string made up of words, that may contain
      !>              the keyword in KEYWD.
      !>
      !>## Detailed Output
      !>
      !>     WORDSQ   is the input string stripped of all words from the
      !>              beginning of the keyword KEYWD to the end of the last
      !>              word preceding one of the words in TERMS (or the end of
      !>              the string if none of the TERMS follows KEYWD in the
      !>              string).
      !>
      !>     FOUND    is .TRUE. if KEYWD is present in the input WORDSQ.
      !>
      !>     SUBSTR   is the substring that begins with the first word
      !>              following KEYWD up to the beginning of any of the words
      !>              in TERM or the end of the string.
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
      !>     Definitions:
      !>
      !>     A WORD        is a set of consecutive non-blank characters
      !>                   delimited by blanks or either end of the string
      !>                   that contains them.
      !>
      !>     Given a string and a keyword this routine locates the first
      !>     occurrence of the keyword in the string and returns the
      !>     substring between the end of the keyword and the first occurrence
      !>     of any of the words in a list of terminating words. If none
      !>     of the terminating words follows the keyword in the string,
      !>     the routine returns all of the string following the keyword.
      !>
      !>     If the next word following the keyword is a terminating word,
      !>     the substring returned will be a blank.
      !>
      !>     If the keyword can not be located in the string, the variable
      !>     FOUND will be returned as .FALSE. and the input string will be
      !>     unchanged. The substring will be returned as a blank.
      !>
      !>     In all other cases, the part of the input string from the
      !>     beginning of the keyword to the start of the first terminating
      !>     word will be removed. If no terminating word follows the keyword
      !>     the portion of the string from the keyword to the last non-blank
      !>     character of the string will be removed.
      !>
      !>## Examples
      !>
      !>     Example 1.
      !>     ----------
      !>       Input:  WORDSQ  'FROM 1 October 1984 12:00:00 TO 1 January 1987'
      !>               KEYWD   'TO'
      !>               TERMS   'FROM'
      !>                       'TO'
      !>                       'BEGINNING'
      !>                       'ENDING'
      !>
      !>       Output: WORDSQ  'FROM 1 October 1984 12:00:00 '
      !>               FOUND   .TRUE.
      !>               SUBSTR  '1 January 1987'
      !>
      !>     Example 2.
      !>     ----------
      !>       Input:  WORDSQ  'FROM 1 October 1984 12:00:00 TO 1 January 1987'
      !>               KEYWD   'FROM'
      !>               TERMS   'FROM'
      !>                       'TO'
      !>                       'BEGINNING'
      !>                       'ENDING'
      !>
      !>       Output: WORDSQ  ' TO 1 January 1987'
      !>               FOUND   .TRUE.
      !>               SUBSTR  '1 October 1984 12:00:00'
      !>
      !>     Example 3.
      !>     ----------
      !>       Input:  WORDSQ  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 '
      !>               KEYWD   'ADDRESS:'
      !>               TERMS   'ADDRESS:'
      !>                       'PHONE:'
      !>                       'NAME:'
      !>
      !>       Output: WORDSQ  ' PHONE: 354-4321 '
      !>               FOUND   .TRUE.
      !>               SUBSTR  '4800 OAK GROVE DRIVE'
      !>
      !>     Example 4.
      !>     ----------
      !>       Input:  WORDSQ  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 '
      !>               KEYWD   'NAME:'
      !>               TERMS   'ADDRESS:'
      !>                       'PHONE:'
      !>                       'NAME:'
      !>
      !>       Output: WORDSQ  'ADDRESS: 4800 OAK GROVE DRIVE PHONE: 354-4321 '
      !>               FOUND   .FALSE.
      !>               SUBSTR  ' '
      !>
      !>## Restrictions
      !>
      !>     1)  It is the user's responsibility to make sure there is adequate
      !>         room in SUBSTR to contain the substring.
      !>
      !>     2)  SUBSTR cannot overwrite WORDSQ.
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
      !>-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement. Changed the input argument
      !>        STRING to WORDSQ for consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 28-FEB-1989 (WLT)
      !>
      !>         Reference to REMSUB replaced by SHIFTL.
      !>
      !>-     Beta Version 1.0.1, 10-FEB-1989 (HAN)
      !>
      !>         Contents of the $Exceptions section was changed
      !>         to "error free" to reflect the decision that the
      !>         module will never participate in error handling.
      pure subroutine kxtrct ( keywd,  terms, nterms, &
                          wordsq, found, substr  )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: keywd  !! Word that marks the beginning of text of interest.
      character(*), intent(in) :: terms(*)  !! Set of words, any of which marks the end of text.
      integer(ip), intent(in) :: nterms  !! Number of TERMS.
      character(*), intent(inout) :: wordsq  !! String containing a sequence of words.
      logical, intent(out) :: found  !! .TRUE. if the keyword is found in the string.
      character(*), intent(out) :: substr  !! String from end of KEYWD to beginning of first TERMS
                                           !! item found.

      integer(ip) :: positn

      integer(ip) :: berase
      integer(ip) :: eerase
      integer(ip) :: begstr
      integer(ip) :: endstr

      integer(ip) :: start
      integer(ip) :: b
      integer(ip) :: e
      integer(ip) :: delims

!
!     Locate the keyword within the string.
!
      positn  =  wdindx ( wordsq, keywd )

!
!     If the keyword wasn't found, set the outputs and head for home.
!
      if ( positn == 0 ) then
         found  = .false.
         substr = ' '
         return
      else
         found  = .true.
      end if

!
!     Set the begin erase marker to the start of the current word
!     Set the end   erase marker to the end   of the current word
!
      berase  = positn
      eerase  = positn + nblen(keywd) - 1
      start   = eerase + 1

!
!     Find the begin and end of the next word.
!
      call fndnwd ( wordsq, start, b, e )

!
!     If there is a next word ( E came back non-zero ) see if its a
!     terminator.
!
      if ( e /= 0 ) then
         delims = findloc(terms(1:nterms), wordsq(b:e), dim=1)
      end if

!
!     If we found a terminator, or were already at the end of the
!     string, we are done.  Remove the keyword and put a blank in
!     SUBSTR
!
      if (      ( e      == 0 ) &
           .or. ( delims /= 0 ) ) then

         wordsq(berase:) = wordsq(eerase+1:)
         substr = ' '
         return

      end if

!
!     Ok. If we made it this far,  we have at least one legitimate word
!     following the keyword,  set the pointer for the start of the
!     substring (to return) to the beginning of this word.
!
      begstr = b

!
!     Now we just examine each word until we run out of string or we
!     run into a terminator.
!
      do while ( (e /= 0) .and. (delims == 0) )

         endstr = e
         eerase = e
         start  = e + 1

         call fndnwd ( wordsq, start, b, e )

         if ( e /= 0 ) then
            delims = findloc(terms(1:nterms), wordsq(b:e), dim=1)
         end if

      end do

!
!     That's it, load the substring variable and remove the keyword
!     and words up to the terminator or end of the string --- whichever
!     came first.
!
      substr = wordsq ( begstr : endstr )

      wordsq(berase:) = wordsq(eerase+1:)

      return
      end subroutine kxtrct

      !> Return the index of the last non-blank character in
      !> a character string.
      !>
      !> The function returns the index of the last non-blank character in
      !> STRING.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (LEN_TRIM instead).
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the last non-blank character in
      !>     the input string. If there are no non-blank characters in the
      !>     string, LASTNB is zero.
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
      !>     If the string is blank, return zero. Otherwise, step through
      !>     the string one character at a time until something other than
      !>     a blank is found. Return the index of that something within
      !>     the string.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of LASTNB.
      !>
      !>           LASTNB ( 'ABCDE'              )   = 5
      !>           LASTNB ( 'AN EXAMPLE'         )   = 10
      !>           LASTNB ( 'AN EXAMPLE        ' )   = 10
      !>           LASTNB ( '                  ' )   = 0
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
      !>
      !>        Modified the comparison to use integer values and the ICHAR()
      !>        function. This improves the performance of the subroutine.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 07-DEC-1990 (IMU)
      !>
      !>        Corrected a misprint in the description of LASTNB.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental integer(ip) function  lastnb ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

!
!     Just like it says in the header.
!
      lastnb = len_trim(string)
      end function lastnb

      !> Return the index of the last printable character in a character
      !> string. ASCII characters 33-126 are printable. (Blanks are not
      !> considered printable.)
      !>
      !> The function returns the index of the last non-blank printable
      !> character in STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the last printable character in
      !>     the input string.
      !>
      !>     ASCII characters 33-126 are considered to be printable characters.
      !>     Blanks are not considered printable characters. If the input
      !>     string contains no printable characters, LASTPC is zero.
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
      !>     This works exactly like LASTNB, except that it skips non-printable
      !>     characters (ASCII control characters) as well as blanks.
      !>
      !>## Examples
      !>
      !>     The program
      !>
      !>        INTEGER         FRSTNB
      !>        INTEGER         FRSTPC
      !>        INTEGER         LASTNB
      !>        INTEGER         LASTPC
      !>
      !>        CHARACTER*10    S
      !>
      !>        S( 1: 1) = ' '
      !>        S( 2: 2) = CHAR (  2 )
      !>        S( 3: 3) = CHAR (  3 )
      !>        S( 4: 4) = 'A'
      !>        S( 5: 5) = 'B'
      !>        S( 6: 6) = 'C'
      !>        S( 7: 7) = CHAR (  7 )
      !>        S( 8: 8) = CHAR (  8 )
      !>        S( 9: 9) = CHAR (  9 )
      !>        S(10:10) = ' '
      !>
      !>        WRITE (*,*) 'Non-blank from ', FRSTNB(S), ' to ', LASTNB(S)
      !>        WRITE (*,*) 'Printable from ', FRSTPC(S), ' to ', LASTPC(S)
      !>
      !>        END
      !>
      !>     produces te following output:
      !>
      !>        Non-blank from 2 to 9.
      !>        Printable from 4 to 6.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental integer(ip) function  lastpc ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

      integer(ip) :: i

!
!     Look for the last character in the range [33,126], and return
!     its index.
!
      do i = len ( string ), 1, -1

         if (       (ichar ( string(i:i) ) >=  33) &
              .and. (ichar ( string(i:i) ) <= 126) ) then

            lastpc = i
            return
         end if

      end do

!
!     Still here? No printable characters. Return zero.
!
      lastpc = 0

      return
      end function lastpc

      !> Build a list of items delimited by a character.
      !>
      !>## Detailed Input
      !>
      !>     ITEMS    are the items to be combined to make the output
      !>              list. Leading and trailing blanks are ignored.
      !>              (Only the non-blank parts of the items are used.)
      !>
      !>     N        is the number of items.
      !>
      !>     DELIM    is the string used to delimit the items in the
      !>              output list. DELIM may contain any number of
      !>              characters, including blanks.
      !>
      !>## Detailed Output
      !>
      !>     LIST     is the output list, containing the N elements of
      !>              ITEMS delimited by DELIM. If LIST is not long enough
      !>              to contain the output list, it is truncated on the
      !>              right.
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
      !>     The non-blank parts of the elements of the ITEMS array are
      !>     appended to the list, one at a time, separated by DELIM.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the operation of LBUILD.
      !>
      !>     1) Let
      !>              DELIM    = ' '
      !>
      !>              ITEMS(1) = 'A'
      !>              ITEMS(2) = '  number'
      !>              ITEMS(3) = 'of'
      !>              ITEMS(4) = ' words'
      !>              ITEMS(5) = 'separated'
      !>              ITEMS(6) = '  by'
      !>              ITEMS(7) = 'spaces'
      !>
      !>        Then
      !>              LIST  = 'A number of words separated by spaces'
      !>
      !>     2) Let
      !>              DELIM    = '/'
      !>
      !>              ITEMS(1) = ' '
      !>              ITEMS(2) = ' '
      !>              ITEMS(3) = 'option1'
      !>              ITEMS(4) = ' '
      !>              ITEMS(5) = 'option2'
      !>              ITEMS(6) = ' '
      !>              ITEMS(7) = ' '
      !>              ITEMS(8) = ' '
      !>
      !>        Then
      !>              LIST  = '//option1//option2///'
      !>
      !>     3) Let
      !>              DELIM    = ' and '
      !>
      !>              ITEMS(1) = 'Bob'
      !>              ITEMS(2) = 'Carol'
      !>              ITEMS(3) = 'Ted'
      !>              ITEMS(4) = 'Alice'
      !>
      !>        Then
      !>              LIST  = 'Bob and Carol and Ted and Alice'
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine lbuild ( items, n, delim, list )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: items ( * )  !! Items in the list.
      integer(ip), intent(in) :: n  !! Number of items in the list.
      character(*), intent(in) :: delim  !! String used to delimit items.
      character(*), intent(out) :: list  !! List of items delimited by DELIM.

      integer(ip) :: lpos
      integer(ip) :: first
      integer(ip) :: last
      integer(ip) :: dlen
      integer(ip) :: llen
      integer(ip) :: ilen
      integer(ip) :: i

!
!     Find the non-blank part of each item. Move it to the
!     end of the list, followed by a delimiter. If the item is
!     blank, don't move anything but the delimiter.
!
!     LPOS is the next position in the output list to be filled.
!     LLEN is the length of the output list.
!     DLEN is the length of DELIM.
!     ILEN is the length of the next item in the list.
!
      list = ' '
      lpos = 1
      llen = len ( list  )
      dlen = len ( delim )

      if ( n > 0 ) then

         do i = 1, n

            if ( lpos <= llen ) then

               if ( items(i) == ' ' ) then
                  list(lpos: ) = delim
                  lpos         = lpos + dlen

               else
                  first = verify(items(i), ' ')
                  last  = len_trim(items(i))
                  ilen  = last - first + 1

                  list(lpos: ) = items(i)(first:last)
                  call suffix ( delim, 0, list )

                  lpos = lpos + ilen + dlen
               end if

            end if

         end do

!
!     We're at the end of the list. Right now, the list ends in
!     a delimiter. Drop it.
!
         if ( lpos-dlen <= llen ) then
            list(lpos-dlen: ) = ' '
         end if

      end if

      return
      end subroutine lbuild

      !> Convert the characters in a string to lowercase.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input string.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is the input string
      !>              with all uppercase letters converted to lowercase.
      !>              Non-letters are not affected.
      !>
      !>              OUT may overwrite IN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the output string length is less than the input string
      !>         length, the result will be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Convert each uppercase character in IN to lowercase.
      !>
      !>## Examples
      !>
      !>     'This is an EXAMPLE'     becomes  'this is an example'
      !>     '12345 +-=? > * $ &'              '12345 +-=? > * $ &'
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 04-AUG-2021 (JDR)
      !>
      !>        Added IMPLICT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
      !>
      !>        Removed the calls to the string lexicographic functions.
      !>
      !>        Modified the algorithm to use the ICHAR() intrinsic function
      !>        and some local integer storage for the bases of the lower and
      !>        upper case letters.
      !>
      !>        Added a "FIRST" clause to the code so that the lower and
      !>        upper case bases and the separation between them are only
      !>        initialized the first time the subroutine is called rather
      !>        than every time.
      !>
      !>        These changes were made to improve the execution speed of
      !>        the subroutine
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine lcase ( in, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input string.
      character(*), intent(out) :: out  !! Output string, all lowercase.

      integer(ip) :: i
      integer(ip) :: ich
!
!     Character codes used for the case conversion.
!
      integer(ip), parameter :: shift = ichar ('a') - uppera

!
!     Move the string from IN to OUT. Step through OUT one character
!     at a time, translating letters between 'A' and 'Z' to lowercase.
!
      out   = in

      do i = 1, len (out)

         ich = ichar( out(i:i) )

         if ( ( ich >= uppera ) .and. ( ich <= upperz ) ) then
            out(i:i) = char ( ich + shift )
         end if

      end do

      return
      end subroutine lcase

      !> Left-justify, uppercase, and space-compress a character string.
      !>
      !>## Detailed Input
      !>
      !>     N        is the maximum number of consecutive occurrences
      !>              of space that will be allowed to remain in the
      !>              output string.
      !>
      !>     INPUT    is the input string.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT   is the output string. This is the input string that
      !>              left-justified and with all occurrences of more than
      !>              N consecutive spaces removed.
      !>
      !>              If OUTPUT is not large enough to hold the
      !>              compressed string, it is truncated on the right.
      !>
      !>              OUTPUT may overwrite INPUT.
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
      !>     The string is left-justified and uppercased. Occurrences of more
      !>     than N consecutive spaces are removed from the input string as it
      !>     is copied to the output string. If the output string is not large
      !>     enough to hold the compressed string, it is truncated on the
      !>     right.
      !>
      !>## Examples
      !>
      !>     Let N = 1. Then
      !>
      !>         ' Abc  DE F  ',           becomes    'ABC DE F',
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
      elemental subroutine ljucrs ( n, input, output )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: n  !! Maximum consecutive occurrences of space.
      character(*), intent(in) :: input  !! Input string.
      character(*), intent(out) :: output  !! Output string.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: count

      integer(ip) :: inlen
      integer(ip) :: outlen

      integer(ip) :: ich

!
!     Character codes used for the case conversion.
!

!
!     Find out how much space there is in the INPUT and OUTPUT strings
!     and initialize the space counter and output place holder.
!
      inlen  = len(input)
      outlen = len(output)

      count  = 0
      j      = 0

      do i = 1, inlen

!
!        Skip leading spaces.
!
         if ( j == 0 .and. input(i:i) == ' ' ) then

!
!           Another leading space. Skip it.
!

         else

!
!           Check this character to see if it is a space or not.
!
            if ( input(i:i) == ' ' ) then

               count = count + 1

!
!              Copy spaces until enough consecutive spaces
!              have been accumulated. When enough consecutive spaces
!              have accumulated, we no longer copy them.
!
               if ( count <= n ) then
                  j           = j + 1
                  output(j:j) = input(i:i)
               end if

            else

!
!              We don't have a space here. Set the space counter to
!              zero.
!
               count       = 0

!
!              Copy this character while swapping lowercase with upper
!              case along the way.
!
               j           = j + 1

               ich = ichar( input(i:i) )

               if ( ( ich >= lowa ) .and. ( ich <= lowz ) ) then
                  output(j:j) = char ( ich + shift )
               else
                  output(j:j) = input(i:i)
               end if

            end if

            if ( j == outlen ) then
               return
            end if

         end if

      end do

!
!     Pad any left over space in the output string with blanks. Note
!     that if the input string was blank, J will be zero at this
!     point and the case below will set the whole output string to
!     blank.
!
      if ( j < outlen ) then
         output(j+1:) = ' '
      end if

      return
      end subroutine ljucrs

      !> Left-justify a character string.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (ADJUSTL instead).
      !>
      !>## Detailed Input
      !>
      !>     INPUT    is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT   is the output character string, left justified.
      !>
      !>              OUTPUT may overwrite INPUT.
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
      !>     Leading blanks are removed from the input character string.
      !>     If the output string is not large enough to hold the left
      !>     justified string, it is truncated on the right.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of LJUST.
      !>
      !>            'ABCDE'             becomes   'ABCDE'
      !>            'AN EXAMPLE'                  'AN EXAMPLE'
      !>            '   AN EXAMPLE  '             'AN EXAMPLE'
      !>            '               '             ' '
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
      !>-    SPICELIB Version 1.2.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 29-JUL-2013 (BVS)
      !>
      !>        Added the quick return branch for input strings that are
      !>        already left-justified. Removed the initial check for blank
      !>        input and changed logic to return an empty string after
      !>        scanning the input. Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine ljust ( input, output )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: input  !! Input character string.
      character(*), intent(out) :: output  !! Output character string, left justified.

!
!     Is the first character of the input string non-blank? If yes, the
!     input string is already left-justified. There is nothing to do
!     but to assign the input string to the output string.
!
      output = adjustl(input)
      end subroutine ljust

      !> Parse a list of items delimited by a single character.
      !>
      !>## Detailed Input
      !>
      !>     LIST     is a list of items delimited by the single character
      !>              DELIM. Consecutive delimiters, and delimiters at the
      !>              beginning and end of the list, are considered to
      !>              delimit blank items. A blank list is considered to
      !>              contain a single (blank) item.
      !>
      !>     DELIM    is the character delimiting the items in the list.
      !>              This may be any ASCII character, including a blank.
      !>              However, by definition, consecutive blanks are NOT
      !>              considered to be consecutive delimiters. In addition,
      !>              leading and trailing blanks are ignored.
      !>
      !>     NMAX     is the maximum number of items to be returned from
      !>              the list. This allows the user to guard against
      !>              overflow from a list containing more items than
      !>              expected.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of items in the list. N may be
      !>              any number between one and NMAX. N is always the
      !>              number of delimiters plus one.
      !>
      !>     ITEMS    are the items in the list, left justified. Any item
      !>              in the list too long to fit into an element of ITEMS
      !>              is truncated on the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the string length of ITEMS is too short to accommodate
      !>         an item, the item will be truncated on the right.
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
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Parse a character string to retrieve the words contained
      !>        within.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LPARSE_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NMAX
      !>      PARAMETER             ( NMAX   = 25  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 255 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(1)           DELIM
      !>      CHARACTER*(STRLEN)      ITEMS  ( NMAX )
      !>      CHARACTER*(STRLEN)      LIST
      !>
      !>      INTEGER                 I
      !>      INTEGER                 N
      !>
      !>C
      !>C     Define the list of delimited items.
      !>C
      !>C     Think of a sentence as a list delimited by a space.
      !>C     DELIM is assigned to a space.
      !>C
      !>      LIST  = 'Run and find out.'
      !>      DELIM = ' '
      !>
      !>C
      !>C     Parse the items from LIST.
      !>C
      !>      CALL LPARSE ( LIST, DELIM, NMAX, N, ITEMS )
      !>
      !>C
      !>C     Output the ITEMS.
      !>C
      !>      DO I = 1, N
      !>
      !>         WRITE(*,'(A,I3,2A)') 'Item', I, ': ', ITEMS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Item  1: Run
      !>        Item  2: and
      !>        Item  3: find
      !>        Item  4: out.
      !>
      !>     2) Repeat the previous example with different character
      !>        delimiting the items in the list and different maximum number
      !>        of items to return.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LPARSE_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 RTRIM
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NCASES
      !>      PARAMETER             ( NCASES = 2   )
      !>
      !>      INTEGER                 NMAXT
      !>      PARAMETER             ( NMAXT  = 25  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 255 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(1)           DELIM  ( NCASES )
      !>      CHARACTER*(STRLEN)      ITEMS  ( NMAXT  )
      !>      CHARACTER*(STRLEN)      LIST   ( NCASES )
      !>
      !>      INTEGER                 I
      !>      INTEGER                 J
      !>      INTEGER                 N
      !>      INTEGER                 NMAX   ( NCASES )
      !>
      !>C
      !>C     Define the lists of delimited items, the delimiting
      !>C     character and the maximum number of items to return.
      !>C
      !>      LIST(1)  = '//option1//option2/ //'
      !>      DELIM(1) = '/'
      !>      NMAX(1)  = 20
      !>
      !>      LIST(2)  = ' ,bob,   carol,, ted,  alice'
      !>      DELIM(2) = ','
      !>      NMAX(2)  = 4
      !>
      !>      DO I = 1, NCASES
      !>
      !>         WRITE(*,'(A,I2,A)') 'Case', I, ':'
      !>         WRITE(*,'(3A)')   '   String: ''',
      !>     .                     LIST(I)(:RTRIM(LIST(I))), ''''
      !>         WRITE(*,'(3A)')   '   DELIM : ''', DELIM(I), ''''
      !>         WRITE(*,'(A,I3)') '   NMAX  :', NMAX(I)
      !>         WRITE(*,'(A)')    '   Output items:'
      !>
      !>C
      !>C        Parse the items from LIST.
      !>C
      !>         CALL LPARSE ( LIST(I), DELIM(I), NMAX(I), N, ITEMS )
      !>
      !>C
      !>C        Output the ITEMS.
      !>C
      !>         DO J = 1, N
      !>
      !>            WRITE(*,'(A,I3,3A)') '      Item', J, ': ''',
      !>     .                  ITEMS(J)(:RTRIM(ITEMS(J))), ''''
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Case 1:
      !>           String: '//option1//option2/ //'
      !>           DELIM : '/'
      !>           NMAX  : 20
      !>           Output items:
      !>              Item  1: ' '
      !>              Item  2: ' '
      !>              Item  3: 'option1'
      !>              Item  4: ' '
      !>              Item  5: 'option2'
      !>              Item  6: ' '
      !>              Item  7: ' '
      !>              Item  8: ' '
      !>        Case 2:
      !>           String: ' ,bob,   carol,, ted,  alice'
      !>           DELIM : ','
      !>           NMAX  :  4
      !>           Output items:
      !>              Item  1: ' '
      !>              Item  2: 'bob'
      !>              Item  3: 'carol'
      !>              Item  4: ' '
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
      !>-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries from $Revisions section.
      !>
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
      !>
      !>        Bug fix: code was modified to avoid out-of-range
      !>        substring bound conditions.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
      !>
      !>        Bug fix: code was modified to avoid out-of-range
      !>        substring bound conditions. The previous version
      !>        of this routine used DO WHILE statements of the form
      !>
      !>                  DO WHILE (      ( B         .LE. EOL   )
      !>           .                .AND. ( LIST(B:B) .EQ. BLANK ) )
      !>
      !>        Such statements can cause index range violations when the
      !>        index B is greater than the length of the string LIST.
      !>        Whether or not such violations occur is platform-dependent.
      pure subroutine lparse ( list, delim, nmax, n, items )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: list  !! List of items delimited by DELIM.
      character(1), intent(in) :: delim  !! Single character used to delimit items.
      integer(ip), intent(in) :: nmax  !! Maximum number of items to return.
      integer(ip), intent(out) :: n  !! Number of items in the list.
      character(*), intent(out) :: items ( * )  !! Items in the list, left justified.


      character(1) :: bchr
      character(1) :: echr

      integer(ip) :: b
      integer(ip) :: e
      integer(ip) :: eol

!
!     Because speed is essential in many list parsing applications,
!     LPARSE parses the input list in a single pass.
!

!
!     Nothing yet.
!
      n = 0

!
!     Blank list contains a blank item.
!
      if ( list == blank ) then

         n        = 1
         items(1) = blank

      else
!
!        Eliminate trailing blanks. EOL is the last non-blank
!        character in the list.
!
         eol = len ( list )

         do while ( list(eol:eol) == blank )

            eol = eol - 1

         end do

!
!        As the king said to Alice: 'Begin at the beginning.
!        Continue until you reach the end. Then stop.'
!
!        When searching for items, B is the beginning of the current
!        item; E is the end.  E points to the next non-blank delimiter,
!        if any; otherwise E points to either the last character
!        preceding the next item, or to the last character of the list.
!
         b = 1

         do while ( b <= eol )
!
!           Skip any blanks before the next item or delimiter.
!
!           At this point in the loop, we know
!
!              B <= EOL
!
            bchr = list(b:b)

            do while (       ( b           <= eol    ) &
                       .and. ( ichar(bchr) == ispace ) )

               b = b + 1

               if ( b <= eol ) then
                  bchr = list(b:b)
               end if

            end do

!
!           At this point B is the index of the next non-blank
!           character BCHR, or else
!
!              B == EOL + 1
!
!           The item ends at the next delimiter.
!
            e = b

            if ( e <= eol ) then
               echr = list(e:e)
            else
               echr = blank
            end if

            do while (       (  e    <= eol   ) &
                       .and. (  echr /= delim )  )

               e = e + 1

               if ( e <= eol ) then
                  echr = list(e:e)
               end if

            end do

!
!           The item now lies between B and E. Unless, of course, B and
!           E are the same character; this can happen if the list
!           starts or ends with a non-blank delimiter, or if we have
!           stumbled upon consecutive delimiters.
!
            n = n + 1

            if ( e > b ) then
               items(n) = list(b:e-1)
            else
               items(n) = blank
            end if

!
!           If there are more items to be found, continue with
!           character following E (which is a delimiter).
!
            if ( n < nmax ) then
               b = e + 1
            else
               return
            end if

         end do

!
!        If the list ended with a (non-blank) delimiter, add a blank
!        item to the end.
!
         if ( ( list(eol:eol) == delim ) .and. ( n < nmax ) ) then

            n        = n + 1
            items(n) = blank

         end if

      end if

      return
      end subroutine lparse

      !> Parse a list of items separated by multiple delimiters.
      !>
      !>## Detailed Input
      !>
      !>     LIST     is a list of items delimited by any one of the
      !>              characters in the string DELIMS. Consecutive
      !>              delimiters, and delimiters at the beginning and
      !>              end of the list, are considered to delimit blank
      !>              items. A blank list is considered to contain
      !>              a single (blank) item.
      !>
      !>     DELIMS   contains the individual characters which delimit
      !>              the items in the list. These may be any ASCII
      !>              characters, including blanks.
      !>
      !>              However, by definition, consecutive blanks are NOT
      !>              considered to be consecutive delimiters. Nor are
      !>              a blank and any other delimiter considered to be
      !>              consecutive delimiters. In addition, leading and
      !>              trailing blanks are ignored.
      !>
      !>     NMAX     is the maximum number of items to be returned from
      !>              the list. This allows the user to guard against
      !>              overflow from a list containing more items than
      !>              expected.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of items in the list. N may be
      !>              any number between one and NMAX. N is always the
      !>              number of delimiters plus one.
      !>
      !>     ITEMS    are the items in the list, left justified. Any item
      !>              in the list to long to fit into an element of ITEMS
      !>              is truncated on the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the string length of ITEMS is too short to accommodate
      !>         an item, the item will be truncated on the right.
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
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Parse a character string to retrieve the words contained
      !>        within.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LPARSM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 DELMLN
      !>      PARAMETER             ( DELMLN = 1   )
      !>
      !>      INTEGER                 NMAX
      !>      PARAMETER             ( NMAX   = 25  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 255 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(DELMLN)      DELIMS
      !>      CHARACTER*(STRLEN)      ITEMS  ( NMAX )
      !>      CHARACTER*(STRLEN)      LIST
      !>
      !>      INTEGER                 I
      !>      INTEGER                 N
      !>
      !>C
      !>C     Define the list of delimited items.
      !>C
      !>C     Think of a sentence as a list delimited by a space.
      !>C     DELIMS is assigned to a space.
      !>C
      !>      LIST   = 'Run and find out.'
      !>      DELIMS = ' '
      !>
      !>C
      !>C     Parse the items from LIST.
      !>C
      !>      CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
      !>
      !>C
      !>C     Output the ITEMS.
      !>C
      !>      DO I = 1, N
      !>
      !>         WRITE(*,'(A,I3,2A)') 'Item', I, ': ', ITEMS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Item  1: Run
      !>        Item  2: and
      !>        Item  3: find
      !>        Item  4: out.
      !>
      !>     2) Parse a character string to retrieve the items contained
      !>        within, when then items are separated by multiple delimiters.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LPARSM_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 RTRIM
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 DELMLN
      !>      PARAMETER             ( DELMLN = 5   )
      !>
      !>      INTEGER                 NMAX
      !>      PARAMETER             ( NMAX   = 25  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 255 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(DELMLN)      DELIMS
      !>      CHARACTER*(STRLEN)      ITEMS  ( NMAX )
      !>      CHARACTER*(STRLEN)      LIST
      !>
      !>      INTEGER                 I
      !>      INTEGER                 N
      !>
      !>C
      !>C     Define the list of delimited items.
      !>C
      !>C     Think of a sentence as a list delimited by a space.
      !>C     DELIMS is assigned to a space.
      !>C
      !>      LIST   = '  1986-187// 13:15:12.184 '
      !>      DELIMS = ' ,/-:'
      !>
      !>C
      !>C     Parse the items from LIST.
      !>C
      !>      CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
      !>
      !>C
      !>C     Output the ITEMS.
      !>C
      !>      DO I = 1, N
      !>
      !>         WRITE(*,'(A,I3,3A)') 'Item', I, ': ''',
      !>     .                        ITEMS(I)(:RTRIM(ITEMS(I))), ''''
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Item  1: '1986'
      !>        Item  2: '187'
      !>        Item  3: ' '
      !>        Item  4: '13'
      !>        Item  5: '15'
      !>        Item  6: '12.184'
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 13-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
      !>
      !>        Bug fix: code was modified to avoid out-of-range
      !>        substring bound conditions.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
      !>
      !>        Bug fix: code was modified to avoid out-of-range
      !>        substring bound conditions. The previous version
      !>        of this routine used DO WHILE statements of the form
      !>
      !>                  DO WHILE (      ( B         .LE. EOL   )
      !>           .                .AND. ( LIST(B:B) .EQ. BLANK ) )
      !>
      !>        Such statements can cause index range violations when the
      !>        index B is greater than the length of the string LIST.
      !>        Whether or not such violations occur is platform-dependent.
      pure subroutine lparsm ( list, delims, nmax, n, items )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: list  !! List of items delimited by DELIMS.
      character(*), intent(in) :: delims  !! Single characters which delimit items.
      integer(ip), intent(in) :: nmax  !! Maximum number of items to return.
      integer(ip), intent(out) :: n  !! Number of items in the list.
      character(*), intent(out) :: items ( * )  !! Items in the list, left justified.


      character(1) :: bchr
      character(1) :: echr

      integer(ip) :: b
      integer(ip) :: e
      integer(ip) :: eol

!
!     Because speed is essential in many list parsing applications,
!     LPARSM parses the input list in a single pass. What follows
!     is nearly identical to LPARSE, except the Fortran INDEX function
!     is used to test for delimiters, instead of testing each character
!     for simple equality.
!

!
!     Nothing yet.
!
      n = 0

!
!     Blank list contains a blank item.
!
      if ( list == blank ) then

         n        = 1
         items(1) = blank

      else
!
!        Eliminate trailing blanks. EOL is the last non-blank
!        character in the list.
!
         eol = len ( list )

         do while ( ichar(list(eol:eol)) == ispace )
            eol = eol - 1
         end do

!
!        As the King said to Alice: 'Begin at the beginning.
!        Continue until you reach the end. Then stop.'
!
!        When searching for items, B is the beginning of the current
!        item; E is the end.  E points to the next non-blank delimiter,
!        if any; otherwise E points to either the last character
!        preceding the next item, or to the last character of the list.
!
         b = 1

         do while ( b <= eol )
!
!           Skip any blanks before the next item or delimiter.
!
!           At this point in the loop, we know
!
!              B <= EOL
!
            bchr = list(b:b)

            do while (       ( b           <= eol    ) &
                       .and. ( ichar(bchr) == ispace ) )

               b = b + 1

               if ( b <= eol ) then
                  bchr = list(b:b)
               end if

            end do

!
!           At this point B is the index of the next non-blank
!           character BCHR, or else
!
!              B == EOL + 1
!
!           The item ends at the next delimiter.
!
            e = b

            if ( e <= eol ) then
               echr = list(e:e)
            else
               echr = blank
            end if

            do while (       (  e                     <= eol ) &
                       .and. (  index( delims, echr ) == 0   )  )

               e = e + 1

               if ( e <= eol ) then
                  echr = list(e:e)
               end if

            end do

!
!           (This is different from LPARSE. If the delimiter was
!           a blank, find the next non-blank character. If it's not
!           a delimiter, back up. This prevents constructions
!           like 'a , b', where the delimiters are blank and comma,
!           from being interpreted as three items instead of two.
!           By definition, consecutive blanks, or a blank and any
!           other delimiter, do not count as consecutive delimiters.)
!
            if (       ( e           <= eol    ) &
                 .and. ( ichar(echr) == ispace ) ) then
!
!              Find the next non-blank character.
!
               do while (       ( e           <= eol    ) &
                          .and. ( ichar(echr) == ispace )  )

                  e = e + 1

                  if ( e <= eol ) then
                     echr = list(e:e)
                  end if

               end do

               if ( e <= eol ) then

                  if (  index( delims, echr ) == 0  ) then
!
!                    We're looking at a non-delimiter character.
!
!                    E is guaranteed to be > 1 if we're here, so the
!                    following subtraction is valid.
!
                     e = e - 1

                  end if

               end if

            end if

!
!           The item now lies between B and E. Unless, of course, B and
!           E are the same character; this can happen if the list
!           starts or ends with a non-blank delimiter, or if we have
!           stumbled upon consecutive delimiters.
!
            n = n + 1

            if ( e > b ) then
               items(n) = list(b:e-1)
            else
               items(n) = blank
            end if

!
!           If there are more items to be found, continue with
!           character following E (which is a delimiter).
!
            if ( n < nmax ) then
               b = e + 1
            else
               return
            end if

         end do

!
!        If the list ended with a (non-blank) delimiter, add a
!        blank item to the end.
!
         if (       ( index( delims, list(eol:eol) ) /= 0    ) &
              .and. (  n                             < nmax )  ) then

            n        = n + 1
            items(n) = blank

         end if

      end if

      return
      end subroutine lparsm

      !> Return the maximum of 1 and the location of the first non-blank
      !> character in the string.
      !>
      !> The function returns the maximum of 1 and the location of the
      !> first non-blank character in STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a string to be trimmed: the location of the
      !>              first non-blank character is desired.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the maximum of 1 and the location of the
      !>     first non-blank character in STRING.
      !>
      !>     In particular, when STRING is blank, the function returns the
      !>     value 1.
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
      !>     When writing a character string to a file, we may wish to omit
      !>     leading blanks. We'd like to use FRSTNB as a lower substring
      !>     bound, but we have to handle the case where FRSTNB returns 0,
      !>     so we write:
      !>
      !>        WRITE ( UNIT, '(A)' ),  STRING ( MAX (1, FRSTNB (STRING)) : )
      !>
      !>     This can be simplified using LTRIM:
      !>
      !>        WRITE ( UNIT, '(A)' ),  STRING ( LTRIM (STRING) : )
      !>
      !>     This routine has a counterpart, RTRIM, which finds the maximum of
      !>     1 and the position of the last non-blank character of a string.
      !>
      !>## Examples
      !>
      !>     1)  Write the non-blank portion of each element of a character
      !>         cell to file SPUD.DAT:
      !>
      !>            DO I = 1,  CARDC (CELL)
      !>
      !>               CALL WRLINE ( 'SPUD.DAT',
      !>           .                  CELL(I) ( LTRIM (CELL) : RTRIM (CELL) ) )
      !>
      !>            END DO
      !>
      !>         When CELL(I) is blank, the string ' ' will be written.
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
      !>-    SPICELIB Version 1.0.0, 02-MAY-1990 (NJB)
      elemental integer(ip) function ltrim ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String to be trimmed.

!
!     `Just do it'.
!
      ltrim = max ( 1, verify(string, ' ') )

      end function ltrim

      !> Scan (lex) a quoted string.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string that may contain a `string
      !>              token' starting at the character position
      !>              indicated by the input argument FIRST (see below).
      !>              String tokens are sequences of characters that
      !>              represent literal strings. Syntactically, a string
      !>              token is a sequence of characters that begins and
      !>              ends with a designated `quote character'. Within
      !>              the token, any occurrence of the quote character
      !>              is indicated by an adjacent pair of quote
      !>              characters: for example, if the quote character is
      !>
      !>                 "
      !>
      !>              then the token representing one instance of this
      !>              character is
      !>
      !>                 """"
      !>
      !>              Here the first quote indicates the beginning of the
      !>              token, the next two quotes together indicate a
      !>              single quote character that constitutes the
      !>              `contents' of the token, and the final quote
      !>              indicates the end of the token.
      !>
      !>     QCHAR    is the quote character. This is always a single
      !>              character. The characters
      !>
      !>                 "  and '
      !>
      !>              are common choices, but any non-blank character is
      !>              accepted. Case *is* significant in QCHAR.
      !>
      !>     FIRST    is the character position at which the routine
      !>              is to start scanning a quoted string token. Note
      !>              that the character STRING(FIRST:FIRST) must equal
      !>              QCHAR if a string token is to be found; this
      !>              routine does *not* attempt to locate the first
      !>              quoted string following the position FIRST.
      !>
      !>## Detailed Output
      !>
      !>     LAST     is the last character position such that the
      !>              substring STRING(FIRST:LAST) is a quoted string
      !>              token, if such a substring exists. Otherwise, the
      !>              returned value of LAST is FIRST-1.
      !>
      !>     NCHAR    is the length of the string token found by this
      !>              routine, if such a token exists. This length
      !>              includes the starting and ending bracketing quotes.
      !>              If a string token is not found, the returned value
      !>              of NCHAR is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the input argument FIRST is less than 1 or greater than
      !>         LEN(STRING)-1, the returned value of LAST is FIRST-1, and the
      !>         returned value of NCHAR is zero.
      !>
      !>     2)  It is not an error for a quoted string token to consist of
      !>         two consecutive quote characters with no intervening
      !>         characters. Calling routines that require special treatment
      !>         of null tokens must handle this case.
      !>
      !>     3)  If the input argument QCHAR is blank, the returned value of
      !>         LAST is FIRST-1, and the returned value of NCHAR is zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Quote characters may be ANY non-blank character. For example, the
      !>     ampersand
      !>
      !>        &
      !>
      !>     is a perfectly valid quote character. If we were using the
      !>     ampersand as the quote character, then the term `doubled quote'
      !>     in the following discussion would refer to the sequence
      !>
      !>        &&
      !>
      !>     not the character
      !>
      !>        "
      !>
      !>     The string tokens identified by this routine are Fortran-style
      !>     quoted strings: they start and end with quote characters. In the
      !>     interior of any such token, any quote characters are represented
      !>     by doubled quote characters. These rules imply that the number of
      !>     quote characters in a quoted string token is always even. The end
      !>     of a quoted string token is located at the first even-numbered
      !>     quote character, counting from the initial quote character, that
      !>     is  not the first member of a pair of quotes indicating an
      !>     embedded quote character.
      !>
      !>     To map the token to the string of characters it represents, use
      !>     the SPICELIB subroutine PARSQS (String parse, quoted).  PARSQS
      !>     removes the bracketing quotes from a quoted string token and
      !>     converts each doubled quote between the bracketing quotes to a
      !>     single quote. For example, the token
      !>
      !>        """"
      !>
      !>     identified by this routine would be mapped by PARSQS to a string
      !>     variable containing the single character
      !>
      !>        "
      !>
      !>## Examples
      !>
      !>     1)  The table below illustrates the action of this routine.
      !>
      !>         STRING CONTENTS               QCHAR   FIRST   LAST   NCHAR
      !>         ==========================================================
      !>         The "SPICE" system            "       5       11     7
      !>         The "SPICE" system            "       1       0      0
      !>         The "SPICE" system            '       5       4      0
      !>         The """SPICE"" system"        "       5       22     18
      !>         The """SPICE"""" system       "       5       15     11
      !>         The &&&SPICE system           &       5       6      2
      !>         ' '                           '       1       3      3
      !>         ''                            '       1       2      2
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
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 25-FEB-2002 (NJB)
      !>
      !>        Corrected references to other SPICELIB routines in header.
      !>
      !>-    SPICELIB Version 1.0.0, 20-OCT-1994 (NJB)
      elemental subroutine lxqstr ( string, qchar, first, last, nchar )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String to be scanned.
      character(1), intent(in) :: qchar  !! Quote delimiter character.
      integer(ip), intent(in) :: first  !! Character position at which to start scanning.
      integer(ip), intent(out) :: last  !! Character position of end of token.
      integer(ip), intent(out) :: nchar  !! Number of characters in token.

      integer(ip) :: l
      integer(ip) :: loc
      integer(ip) :: pos

      logical :: even
      logical :: none  !! no token can start at FIRST

!
!     Error free, no check-in required.
!
      l    =   len(string)

!
!     Handle the cases in which we can tell right away that
!     no token can be found.
!
      none = first < 1  .or.  first > l-1  .or.  qchar == ' '
      if ( .not. none ) none = string(first:first) /= qchar

      if ( none ) then

         last  = first - 1
         nchar = 0
         return

      end if

!
!     We started out with a quote character, if we got this far.  Now
!     we have to see whether a quoted string token exists.  Note that
!     we can safely assume FIRST+1 does not exceed L.
!
      loc = index (  string( first+1: l ),  qchar  )

      if ( loc == 0 ) then

         last  = first - 1
         nchar = 0
         return

      end if

!
!     At this point, we have a candidate ending point for the token.
!     We must search for the actual end of the token.  The token ends
!     at the first even-numbered quote character that is not part of
!     an embedded pair of quotes.
!
!     Our method of looking for the end of the token will be to search
!     from left to right, keeping track of the rightmost character
!     position that could be the end of the string token, until we find
!     a definitive answer as to the status of our candidate.
!     The variable LAST will be used for this candidate character
!     position.  The variable EVEN will tell us whether we've seen an
!     even number of quotes.  The variable POS will point to the current
!     character to examine.
!
      last  =  first + loc
      even  = .true.
      pos   =  last  + 1

      do while (  pos  <=  l  )

         if ( string(pos:pos) == qchar ) then
!
!           Each quote character we see toggles the quote parity.
!
            even  =  .not. even
!
!           If the current parity is even, the current quote character
!           becomes the candidate for the final quote.  This character
!           can lose out only to a quote that is further to the right.
!
            if ( even ) then
               last = pos
            end if

         else

            if ( even ) then
!
!              The last even-numbered quote was followed by a non-quote
!              character.  We're done.
!
               nchar  =  last - first + 1
               return

            end if

         end if

         pos  =  pos + 1

!
!        At this point in the loop,
!
!           EVEN indicates whether we've seen an even number of quote
!           characters.
!
!           LAST is the index, relative to the start of the string,
!           of the last even-numbered quote we've seen.  This is the
!           current candidate for the closing quote.
!
!           POS is the index of the next character to examine.
!
      end do

!
!     Since there are no further characters to examine, the value of
!     LAST that we already have is the largest value we can get.
!
      nchar  =  last - first + 1

      return
      end subroutine lxqstr

      !> Determine whether a string is matched by a template containing
      !> wild cards.
      !>
      !> The function returns .TRUE. if STRING matches TEMPL and otherwise
      !> returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string to be tested for a match
      !>              against the input template. Leading and trailing blanks
      !>              are ignored.
      !>
      !>     TEMPL    is the input template to be tested for a match against
      !>              the input string. TEMPL may contain wild cards. Leading
      !>              and trailing blanks are ignored.
      !>
      !>     WSTR     is the wild string token used in the input template. The
      !>              wild string token may represent from zero to any number
      !>              of characters.
      !>
      !>     WCHR     is the wild character token used in the input template.
      !>              The wild character token represents exactly one
      !>              character.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. when the input string matches the
      !>     input template, and .FALSE. otherwise. The string and template
      !>     match whenever the template can expand (through replacement of its
      !>     wild cards) to become the input string.
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
      !>     MATCHW ignores leading and trailing blanks in both the string
      !>     and the template. All of the following are equivalent (they
      !>     all return .TRUE.).
      !>
      !>        MATCHW ( 'ALCATRAZ',     'A*Z',      '*', '%' )
      !>        MATCHW ( '  ALCATRAZ  ', 'A*Z',      '*', '%' )
      !>        MATCHW ( 'ALCATRAZ',     '  A*Z  ',  '*', '%' )
      !>        MATCHW ( '  ALCATRAZ  ', '  A*Z  ',  '*', '%' )
      !>
      !>     MATCHW is case-sensitive:  uppercase characters do not match
      !>     lowercase characters, and vice versa. Wild characters match
      !>     characters of both cases.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>        STRING  = '  ABCDEFGHIJKLMNOPQRSTUVWXYZ  '
      !>        WSTR    = '*'
      !>        WCHR    = '%'
      !>
      !>     Then
      !>
      !>        if TEMPL is  '*A*'        MATCHW is   T
      !>                     'A%D*'                     F
      !>                     'A%C*'                   T
      !>                     '%A*'                      F
      !>                         '%%CD*Z'                 T
      !>                         '%%CD'                     F
      !>                         'A*MN*Y*Z'               T
      !>                         'A*MN*Y*%Z'                F
      !>                         '*BCD*Z*'                T
      !>                         '*bcd*z*'                  F
      !>                         ' *BCD*Z*  '             T
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
      !>-    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries from $Revisions section.
      !>
      !>-    SPICELIB Version 1.3.1, 11-NOV-2005 (NJB)
      !>
      !>        Corrected example calls in header; made other minor
      !>        edits to header.
      !>
      !>-    SPICELIB Version 1.3.0, 08-JUN-1999 (WLT)
      !>
      !>        Fixed comments in detailed output and example sections.
      !>
      !>-    SPICELIB Version 1.2.0, 15-MAY-1995 (WLT)
      !>
      !>        Direct substring comparisons were replaced with the logical
      !>        function SAMCH in several cases so as to avoid out of range
      !>        errors when examining substrings.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 06-OCT-1988 (WLT)
      !>
      !>        The old algorithm just did not work. Strings with wild
      !>        characters at the beginning or end of the string were not
      !>        matched correctly. For example, A% matched APPROX, if the
      !>        wild character token was % and the wild string token was *.
      !>        Needless to say, a new algorithm was developed.
      elemental logical function matchw ( string, templ, wstr, wchr )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String to be tested.
      character(*), intent(in) :: templ  !! Template (with wild cards) to test against STRING.
      character(1), intent(in) :: wstr  !! Wild string token.
      character(1), intent(in) :: wchr  !! Wild character token.

      integer(ip) :: sfirst
      integer(ip) :: tfirst
      integer(ip) :: slast
      integer(ip) :: tlast

      integer(ip) :: slen
      integer(ip) :: tlen

      integer(ip) :: scur
      integer(ip) :: tcur

      integer(ip) :: left
      integer(ip) :: right

      integer(ip) :: i
      integer(ip) :: j

      logical :: nosubm

!
!     Set the default function value to be FALSE.
!
      matchw = .false.

!
!     First let's get everybody's measurements.
!
      sfirst = verify(string, ' ')
      slast  = len_trim(string)
      tfirst = verify(templ, ' ')
      tlast  = len_trim(templ)

      tlen   = tlast - tfirst + 1
      slen   = slast - sfirst + 1

      scur   = max ( 1, sfirst )
      tcur   = tfirst

!
!     A blank template matches a blank string, and nothing else.
!
      if ( ( tlast == 0 ) .and. ( slast == 0 ) ) then

         matchw = .true.
         return

      else if ( tlast == 0 ) then

         matchw = .false.
         return

      end if

!
!     The beginning of the string and template must be identical
!     up to the first occurrence of a wild string.
!

      do while (       ( tcur             <= tlast ) &
                 .and. ( scur             <= slast ) &
                 .and. ( .not. samch( templ,tcur, wstr,1) ) )

         if (       ( templ(tcur:tcur) /= string(scur:scur) ) &
              .and. ( templ(tcur:tcur) /= wchr              ) ) then

            matchw = .false.
            return

         else

            tcur = tcur + 1
            scur = scur + 1

         end if

      end do

!
!     There are a three ways we could have finished the loop above
!     without hitting a wild string.
!
!     Case 1.  Both the string and template ran out of characters at
!     the same time without running into a wild string in the template.
!
      if (       ( tcur > tlast ) &
           .and. ( scur > slast ) ) then

         matchw = .true.
         return

      end if

!
!     Case 2. The template ran out of characters while there were still
!     characters remaining in the in the string.  No match.
!
      if (       ( tcur > tlast ) &
           .and. ( scur <= slast ) ) then

         matchw = .false.
         return

      end if

!
!     Case 3. The string ran out of characters while non-wild characters
!     remain in the template.
!
!     We have to check to see if any non-wild-string characters
!     remain.  If so, we DO NOT have a match.  On the other hand if
!     only wild string characters remain we DO have a match.
!
      if (       ( tcur <= tlast ) &
           .and. ( scur > slast ) ) then

         matchw = .true.

         do i = tcur, tlast
            matchw = matchw .and. ( templ(i:i) == wstr )
         end do

         return

      end if

!
!     OK. There is only one way that you can get to this point.
!     It must be the case that characters remain in both the template
!     (TCUR .LE. TLAST) and the string (SCUR .LE. SLAST).  Moreover,
!     to get out of the first loop you had to hit a wild string
!     character.  Find the first non-wild-string character in the
!     template. (If there isn't one, we have a match.)
!

      do while (       ( tcur             <= tlast ) &
                 .and. ( samch ( templ,tcur, wstr,1 )  ) )

         tcur = tcur + 1

      end do

      if ( tcur > tlast ) then

         matchw = .true.
         return

      end if

!
!     Still here? Ok. We have a non-wild-string character at TCUR. Call
!     this position left and look for the right end of the maximum
!     length substring of TEMPL (starting at left) that does not have
!     a wild string character.
!
      left = tcur

      do while (       ( tcur             <=   tlast   ) &
                 .and. ( .not. samch(templ,tcur, wstr,1) ) )

         tcur = tcur + 1

      end do

      right = tcur - 1

      do while ( left <= tlast )

!
!        First see if there is enough room left in the string to
!        match TEMPL(LEFT:RIGHT)
!
         if ( slast - scur < right - left ) then

            matchw = .false.
            return

         end if

!
!        The substring TEMPL(LEFT:RIGHT) might be the end of the
!        string.  In such a case the ends of STRING must match
!        exactly with the end of TEMPL.
!
         if ( right == tlast ) then

            i = slast
            j = tlast

            do while ( j >= left )

               if (      samch (templ,j, wchr,  1 ) &
                    .or. samch (templ,j, string,i )  ) then

                  j = j - 1
                  i = i - 1

               else

                  matchw = .false.
                  return

               end if

            end do

!
!           If we made it through the loop, we've got a match.
!
            matchw = .true.
            return
         else

!
!           In this case TEMPL(LEFT:RIGHT) is in between wild string
!           characters.  Try to find a substring at or to the right
!           of SCUR in STRING that matches TEMPL(LEFT:RIGHT)
!
            nosubm = .true.

            do while ( nosubm )

               i = scur
               j = left

               do while (     ( j           <= right           ) &
                         .and.(      samch ( string,i, templ,j ) &
                                .or. samch ( wchr,  1, templ,j ) ) )

                  i = i + 1
                  j = j + 1

               end do

!
!              If J made it past RIGHT, we have a substring match
!
               if ( j > right ) then

                  scur   = i
                  nosubm = .false.

!
!              Otherwise, try the substring starting 1 to the right
!              of where our last try began.
!
               else

                  scur = scur + 1

!
!                 Make sure there's room to even attempt a match.
!
                  if ( slast - scur < right - left ) then

                     matchw = .false.
                     return

                  end if

               end if
            end do

         end if

!
!        If you have reached this point there must be something left
!        in the template and that something must begin with a wild
!        string character.  Hunt for the next substring that doesn't
!        contain a wild string character.
!
         do while (       ( tcur             <= tlast ) &
                    .and.   samch ( templ,tcur,  wstr,1) )

            tcur = tcur + 1

         end do

         if ( tcur > tlast ) then

!
!           All that was left was wild string characters.  We've
!           got a match.
!
            matchw = .true.
            return

         end if

!
!        Still here? Ok. We have a non-wild-string character at TCUR.
!        Call this position left and look for the right end of the
!        maximum length substring of TEMPL (starting at left) that
!        does not have a wild string character.
!
         left = tcur

         do while (       ( tcur             <= tlast ) &
                    .and. ( .not. samch( templ,tcur, wstr,1 ) ) )

            tcur = tcur + 1

         end do

         right = tcur - 1

      end do

      end function matchw

      !> Return the non-blank length of a character string. (That is,
      !> the index of the last non-blank character when the string is
      !> left-justified.)
      !>
      !> The function returns the non-blank length of STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the non-blank length of STRING. This is the
      !>     same as the index of the last non-blank character in the left
      !>     justified string. If STRING is blank, NBLEN is zero.
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
      !>     Locate the first and last non-blank characters in the string.
      !>     Subtract to get the non-blank length.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of NBLEN.
      !>
      !>           NBLEN ( 'ABCDE' )             = 5
      !>           NBLEN ( 'AN EXAMPLE' )        = 10
      !>           NBLEN ( '   AN EXAMPLE  ' )   = 10
      !>           NBLEN ( '               ' )   = 0
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental integer(ip) function  nblen ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

!
!     Blank string is easy.
!
      if ( string == ' ' ) then
         nblen = 0
      else
         nblen = len_trim(string) - verify(string, ' ') + 1
      end if

      return
      end function nblen

      !> Determine the non-blank width of a character array---that is,
      !> the largest value of [[lastnb]] for any element in the array.
      !>
      !> The function returns the index of the rightmost non-blank
      !> character in the entire array of strings ARRAY.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MAXVAL and LEN_TRIM instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the rightmost non-blank
      !>     character in the entire array. This is equivalent to the maximum
      !>     value of LASTNB for the array, but somewhat more efficient to
      !>     compute. If NELT is not greater than zero, NBWID is zero.
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
      !>     Find the last non-blank character in the first element of the
      !>     array. Search the rest of the elements, starting at the end of
      !>     each string and moving back just far enough to determine if the
      !>     current string is wider than any of the previous ones. (This
      !>     makes NBWID somewhat more efficient than LASTNB.)
      !>
      !>     If any of the strings is found to contain no trailing blanks,
      !>     NBWID is just the length of the individual elements of the array,
      !>     and the search is terminated immediately.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following strings.
      !>
      !>           ARRAY(1) = 'A string of medium length                      '
      !>           ARRAY(2) = 'A very long string, much longer than the rest  '
      !>           ARRAY(3) = 'Shorter                                        '
      !>           ARRAY(4) = 'Short                                          '
      !>
      !>     Then the value returned by
      !>
      !>           WIDEST = NBWID ( ARRAY, 4 )
      !>
      !>     is 45.
      !>
      !>     If the word 'rest' in the second element is changed to 'others',
      !>     the value returned is 47, and the search is terminated after the
      !>     second element.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure integer(ip) function nbwid ( array, nelt )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: array   ( * )  !! Input array.
      integer(ip), intent(in) :: nelt  !! Number of elements in the array.

!
!     Nonsense case: no elements.
!
      nbwid = max(0, maxval(len_trim(array(1:nelt))))
      end function nbwid

      !> Find the first occurrence in a string of a character NOT belonging
      !> to a collection of characters, starting at a specified location,
      !> searching forwards.
      !>
      !> The function returns the index of the first character of STR
      !> at or following index START that is not in the collection CHARS.
      !>
      !>## Detailed Input
      !>
      !>     STR      is any character string.
      !>
      !>     CHARS    is a character string containing a collection of
      !>              characters. Spaces in CHARS are significant.
      !>
      !>     START    is the position in STR to begin looking for
      !>              characters not in CHARS.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first character of STR (at
      !>     or following index START) that is not one of the characters in the
      !>     string CHARS. If no such character is found, the function returns
      !>     zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If START is less than 1, the search begins at the first
      !>         character of the string.
      !>
      !>     2)  If START is greater than the length of the string, NCPOS
      !>         returns zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     NCPOS is case sensitive.
      !>
      !>     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
      !>     POSR, CPOSR, NCPOSR) is described in the Required Reading.
      !>
      !>     Those familiar with the True BASIC language should note that
      !>     these functions are equivalent to the True BASIC intrinsic
      !>     functions with the same names.
      !>
      !>## Examples
      !>
      !>     Let STRING = 'BOB, JOHN, TED, AND MARTIN    '
      !>                   123456789012345678901234567890
      !>
      !>     Let CHAR   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      !>
      !>     Normal (Sequential) Searching:
      !>     ------------------------------
      !>
      !>           NCPOS( STRING, CHAR,  1 ) = 4
      !>           NCPOS( STRING, CHAR,  5 ) = 5
      !>           NCPOS( STRING, CHAR,  6 ) = 10
      !>           NCPOS( STRING, CHAR, 11 ) = 11
      !>           NCPOS( STRING, CHAR, 12 ) = 15
      !>           NCPOS( STRING, CHAR, 16 ) = 16
      !>           NCPOS( STRING, CHAR, 17 ) = 20
      !>           NCPOS( STRING, CHAR, 21 ) = 27
      !>           NCPOS( STRING, CHAR, 28 ) = 28
      !>           NCPOS( STRING, CHAR, 29 ) = 29
      !>           NCPOS( STRING, CHAR, 30 ) = 30
      !>           NCPOS( STRING, CHAR, 31 ) =  0
      !>
      !>     START out of bounds:
      !>     --------------------
      !>
      !>           NCPOS( STRING, CHAR, -12 ) = 4
      !>           NCPOS( STRING, CHAR,   0 ) = 4
      !>           NCPOS( STRING, CHAR,  31 ) = 0
      !>           NCPOS( STRING, CHAR, 123 ) = 0
      !>
      !>     Order within CHARS:
      !>     -------------------
      !>
      !>           NCPOS( STRING, 'JOHN', 7 ) = 10
      !>           NCPOS( STRING, 'OHJN', 7 ) = 10
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
      !>
      !>        The Required Reading file POSITION was renamed to SCANNING.
      !>        This header was updated to reflect the change.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function ncpos ( str, chars, start )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str  !! Any character string.
      character(*), intent(in) :: chars  !! A collection of characters.
      integer(ip), intent(in) :: start  !! Position to begin looking for one not in CHARS

      logical :: found
      integer(ip) :: lenstr
      integer(ip) :: b

      lenstr = len ( str )

      b      = max(1,start)
      found  = .false.
      ncpos   = 0

      do while ( .not. found )

         if ( b > lenstr ) then
            return
         else if ( index( chars, str(b:b) ) == 0 ) then
            ncpos = b
            return
         else
            b = b + 1
         end if

      end do

      return
      end function ncpos

      !> Find the first occurrence in a string of a character NOT
      !> belonging to a collection of characters, starting at a
      !> specified location, searching in reverse.
      !>
      !> The function returns the index of the last character of STR
      !> at or before index START that is not in the collection CHARS.
      !>
      !>## Detailed Input
      !>
      !>     STR      is any character string.
      !>
      !>     CHARS    is a character string containing a collection of
      !>              characters. Spaces in CHARS are significant.
      !>
      !>     START    is the position in STR to begin looking for
      !>              characters not in CHARS.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the last character of STR (at
      !>     or before index START) that is not one of the characters in the
      !>     string CHARS. No such character is found, the function returns
      !>     zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If START is less than 1, NCPOSR returns zero.
      !>
      !>     2)  If START is greater than LEN(STRING), the search begins
      !>         at the last character of the string.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     NCPOSR is case sensitive.
      !>
      !>     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
      !>     POSR, CPOSR, NCPOSR) is described in the Required Reading.
      !>
      !>     Those familiar with the True BASIC language should note that
      !>     these functions are equivalent to the True BASIC intrinsic
      !>     functions with the same names.
      !>
      !>## Examples
      !>
      !>     Let STRING = 'BOB, JOHN, TED, AND MARTIN    '
      !>                   123456789012345678901234567890
      !>
      !>     Let CHAR   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      !>
      !>     Normal (Sequential) Searching:
      !>     ------------------------------
      !>
      !>           NCPOSR( STRING, CHAR, 31 ) = 30
      !>           NCPOSR( STRING, CHAR, 29 ) = 29
      !>           NCPOSR( STRING, CHAR, 28 ) = 28
      !>           NCPOSR( STRING, CHAR, 27 ) = 27
      !>           NCPOSR( STRING, CHAR, 26 ) = 20
      !>           NCPOSR( STRING, CHAR, 19 ) = 16
      !>           NCPOSR( STRING, CHAR, 15 ) = 15
      !>           NCPOSR( STRING, CHAR, 14 ) = 11
      !>           NCPOSR( STRING, CHAR, 10 ) = 10
      !>           NCPOSR( STRING, CHAR,  9 ) =  5
      !>           NCPOSR( STRING, CHAR,  4 ) =  4
      !>           NCPOSR( STRING, CHAR,  3 ) =  0
      !>
      !>     START out of bounds:
      !>     --------------------
      !>
      !>           NCPOSR( STRING, CHAR,   0 ) =  0
      !>           NCPOSR( STRING, CHAR,  -4 ) =  0
      !>           NCPOSR( STRING, CHAR,  31 ) = 30
      !>           NCPOSR( STRING, CHAR, 123 ) = 30
      !>
      !>     Order within CHARS:
      !>     -------------------
      !>
      !>           NCPOSR( STRING, 'JOHN', 7 ) =  5
      !>           NCPOSR( STRING, 'OHJN', 7 ) =  5
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
      !>
      !>        The Required Reading file POSITION was renamed to SCANNING.
      !>        This header was updated to reflect the change.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function ncposr ( str, chars, start )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str  !! Any character string.
      character(*), intent(in) :: chars  !! A collection of characters.
      integer(ip), intent(in) :: start  !! Position to begin looking for one not in CHARS

      logical :: found
      integer(ip) :: lenstr
      integer(ip) :: b

      lenstr = len ( str )

      b      = min(lenstr,start)
      found  = .false.
      ncposr = 0

      do while ( .not. found )

         if ( b <= 0 ) then
            return
         else if ( index( chars, str(b:b) ) == 0 ) then
            ncposr = b
            return
         else
            b = b - 1
         end if

      end do

      return
      end function ncposr

      !> Return the next word in a given character string, and
      !> left justify the rest of the string.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input string to be parsed. Each word of this
      !>              string is a maximal sequence of consecutive non-blank
      !>              characters.
      !>
      !>## Detailed Output
      !>
      !>     NEXT     is the first word in STRING. It is called the "next" word
      !>              because NEXTWD is typically called repeatedly to find the
      !>              words of the input string in left-to-right order. A word
      !>              is a maximal sequence of consecutive non-blank
      !>              characters. NEXT is always returned left-justified.
      !>
      !>              If STRING is blank, NEXT is blank.
      !>
      !>              NEXT may NOT overwrite STRING.
      !>
      !>     REST     is the remaining part of STRING, left-justified after the
      !>              removal of NEXT.
      !>
      !>              REST may overwrite STRING.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the declared lengths of NEXT and REST are not large enough
      !>         to hold the output strings, they are truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     NEXTWD is used primarily for parsing input commands consisting
      !>     of one or more words, where a word is defined to be any sequence
      !>     of consecutive non-blank characters. Successive calls to NEXTWD,
      !>     each using the previous value of REST as the input string, allow
      !>     the calling routine to neatly parse and process one word at a
      !>     time.
      !>
      !>     NEXTWD cuts the input string into two pieces, and returns them
      !>     separately. The first piece is the first word in the string.
      !>     (Leading blanks are ignored. The first word, which is returned in
      !>     the output argument NEXT, runs from the first non-blank character
      !>     in the string up to the first blank that follows it.) The second
      !>     piece is whatever is left after the first word is removed. The
      !>     second piece is left justified, to simplify later calls to NEXTWD.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a character string, get the sequence of words within.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM NEXTWD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL               EQSTR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               LINZS
      !>      PARAMETER           ( LINZS = 47 )
      !>
      !>      INTEGER               WRDSZ
      !>      PARAMETER           ( WRDSZ = 5  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(WRDSZ)     NEXT
      !>      CHARACTER*(LINZS)     REST
      !>      CHARACTER*(LINZS)     STRING
      !>
      !>      REST = '  Now is the time,  for all good men to come.'
      !>
      !>      WRITE(*,'(A)') 'Next   Rest of the string'
      !>      WRITE(*,'(A)') '-----  ---'
      !>     .            // '---------------------------------------'
      !>
      !>      DO WHILE ( .NOT. EQSTR ( REST, ' ' ) )
      !>
      !>         STRING = REST
      !>         CALL NEXTWD ( STRING, NEXT, REST )
      !>
      !>         WRITE(*,'(A5,2X,A)') NEXT, REST
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Next   Rest of the string
      !>        -----  ------------------------------------------
      !>        Now    is the time,  for all good men   to come.
      !>        is     the time,  for all good men   to come.
      !>        the    time,  for all good men   to come.
      !>        time,  for all good men   to come.
      !>        for    all good men   to come.
      !>        all    good men   to come.
      !>        good   men   to come.
      !>        men    to come.
      !>        to     come.
      !>        come.
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
      !>-    SPICELIB Version 1.3.0, 19-MAY-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on the existing fragment.
      !>
      !>        Updated header documentation. Added entry #1 in $Exceptions
      !>        section.
      !>
      !>-    SPICELIB Version 1.2.0, 04-APR-1996 (KRG)
      !>
      !>        Fixed a problem that could occur when STRING and REST are
      !>        the same character string. Simplified the algorithm a bit
      !>        while I was at it.
      !>
      !>        Single character comparisons now make use of ICHAR to
      !>        perform the comparisons as integers for speed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine nextwd ( string, next, rest )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.
      character(*), intent(out) :: next  !! The next word in the string.
      character(*), intent(out) :: rest  !! The remaining part of STRING, left-justified.


      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: i

      logical :: inword

!
!     The trivial case.
!
      if ( string == ' ' ) then

         next = ' '
         rest = ' '

!
!     The non-trivial case.
!
      else
!
!        Get the length of the string.
!
         end = len ( string )

!
!        Skip leading blanks and set flags indicating that we are
!        not in a word and that we do not have a word.
!
         begin  = 1
         inword = .false.
!
!        We know the string is not blank, so we will eventually
!        get to a word, thus no need to check against END here.
!
         do while ( .not. inword )
            if ( ichar( string(begin:begin) ) == ispace ) then
               begin = begin + 1
            else
               inword = .true.
            end if
         end do

!
!        We are now in a word. Step through the input string until the
!        next blank is encountered or until the end of the string is
!        found. We start at BEGIN even though we know from above that
!        STRING(BEGIN:BEGIN) is not blank; this allows us to deal
!        cleanly with the case where the string is a single character
!        long and not blank (because we're in that case).
!
         i = begin
         do while ( inword )
            if ( ichar ( string(i:i) ) /= ispace ) then
               i = i + 1
               if ( i > end ) then
                  i      = i - 1
                  inword = .false.
               end if
            else
               i      = i - 1
               inword = .false.
            end if
         end do

!
!        Our word is the substring between BEGIN and I. Note that I
!        might be equal to END, so we have to be careful about setting
!        the REST. We also left justify REST as we set it. LJUST does
!        the right thing if STRING and REST overlap. If we do not have
!        a word, the NEXT and REST are both blank.
!
         next = string(begin:i)

         if ( i < end ) then
            rest = adjustl(string(i+1:))
         else
            rest = ' '
         end if

      end if

      return
      end subroutine nextwd

      !> Return the Nth word in a character string, and its location
      !> in the string.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input string to be parsed. Each word of this
      !>              string is a maximal sequence of consecutive non-blank
      !>              characters.
      !>
      !>     NTH      is the index of the word to be returned. (One for the
      !>              first word, two for the second, and so on.)
      !>
      !>## Detailed Output
      !>
      !>     WORD     is the NTH word in STRING. If STRING is blank, or NTH is
      !>              non-positive or too large, WORD is blank.
      !>
      !>              WORD may overwrite STRING.
      !>
      !>     LOC      is the location of WORD in STRING. (That is, WORD begins
      !>              at STRING(LOC:LOC)). If STRING is blank, or NTH is
      !>              non-positive or too large, LOC is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the declared length of WORD is not large enough to contain
      !>         the NTH word in STRING, the word will be truncated on the
      !>         right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     NTHWD, like NEXTWD, is useful primarily for parsing input commands
      !>     consisting of one or more words, where a word is defined to be a
      !>     maximal sequence of consecutive non-blank characters. Each word is
      !>     bounded on both sides by a blank character, or by the start or end
      !>     of the input string. Successive calls to NEXTWD allow the calling
      !>     routine to neatly parse and process one word at a time.
      !>
      !>     The chief difference between the two routines is that
      !>     NTHWD allows the calling routine to access the words making
      !>     up the input string in random order. (NEXTWD allows only
      !>     sequential access.)
      !>
      !>     NTHWD may be more efficient than NEXTWD, since NTHWD doesn't
      !>     update an output string consisting of the remaining, unparsed
      !>     string.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a character string, get the N'th word within, and the
      !>        word's location.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM NTHWD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         STRING
      !>      PARAMETER           ( STRING = ' Now is the time,   '
      !>     .                  //  'for all good men     to come.' )
      !>
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT = '(A,I3,3A,I3)' )
      !>
      !>      INTEGER               WRDSZ
      !>      PARAMETER           ( WRDSZ = 5 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(WRDSZ)     WORD
      !>      INTEGER               LOC
      !>      INTEGER               NTH
      !>
      !>      DO NTH = -1, 11
      !>
      !>         CALL NTHWD ( STRING, NTH, WORD, LOC )
      !>         WRITE(*,FMT) 'Word #', NTH, '  is <',
      !>     .                WORD(:RTRIM(WORD)),
      !>     .                '>, starting at position', LOC
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Word # -1  is < >, starting at position  0
      !>        Word #  0  is < >, starting at position  0
      !>        Word #  1  is <Now>, starting at position  2
      !>        Word #  2  is <is>, starting at position  6
      !>        Word #  3  is <the>, starting at position  9
      !>        Word #  4  is <time,>, starting at position 13
      !>        Word #  5  is <for>, starting at position 21
      !>        Word #  6  is <all>, starting at position 25
      !>        Word #  7  is <good>, starting at position 29
      !>        Word #  8  is <men>, starting at position 34
      !>        Word #  9  is <to>, starting at position 42
      !>        Word # 10  is <come.>, starting at position 45
      !>        Word # 11  is < >, starting at position  0
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
      !>-    SPICELIB Version 1.2.0, 26-OCT-2021 (NJB) (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on the existing fragment.
      !>
      !>        Updated header documentation. Added entry #1 in $Exceptions
      !>        section.
      !>
      !>-    SPICELIB Version 1.1.0, 10-MAY-2006 (EDW)
      !>
      !>        Added logic to prevent the evaluation of STRING(I:I)
      !>        if I exceeds the length of STRING. Functionally, the
      !>        evaluation had no effect on NTHWD's output, but the ifort
      !>        F95 compiler flagged the evaluation as an array
      !>        overrun error. This occurred because given:
      !>
      !>           A .AND. B
      !>
      !>        ifort evaluates A then B then performs the logical
      !>        comparison.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine nthwd ( string, nth, word, loc )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.
      integer(ip), intent(in) :: nth  !! Index of the word to be returned.
      character(*), intent(out) :: word  !! The NTH word in STRING.
      integer(ip), intent(out) :: loc  !! Location of WORD in STRING.

      integer(ip) :: n
      integer(ip) :: i
      integer(ip) :: length

      logical :: loop

!
!     Trivial cases first. Blank STRING? Nonpositive NTH?
!
      if ( string == ' ' .or. nth < 1 ) then

         word = ' '
         loc  =  0
         return

      end if

!
!     Skip leading blanks.
!
      loc = 1

      do while ( string(loc:loc) == ' ' )
         loc = loc + 1
      end do

!
!     If we wanted the first word, we have the location. Otherwise,
!     keep stepping through STRING. Quit when the N'TH word is found,
!     or when the end of the string is reached. (The current word is
!     ended whenever a blank is encountered.)
!
!     N is the number of words found so far.
!     I is the current location in STRING.
!
      n      = 1
      i      = loc
      length = len ( string )

      do while ( i < length .and. n < nth )

         i = i + 1

!
!        Blank signals end of the current word.
!
         if ( string(i:i) == ' ' ) then

!
!           Skip ahead to the next one.  The logic ensures no
!           evaluation of STRING(I:I) if I > LEN(STRING).
!
            loop = i <= length
            if( loop ) then
               loop = loop .and. string(i:i) == ' '
            end if

            do while ( loop )
               i = i + 1

               if( i > length ) then
                  loop = .false.
               else if ( string(i:i) /= ' ' ) then
                  loop = .false.
               else
                  loop = .true.
               end if

            end do

!
!           If not at the end of the string, we have another word.
!
            if ( i <= length ) then
               n   = n + 1
               loc = i
            end if

         end if

      end do

!
!     Couldn't find enough words? Return blank and zero.
!
      if ( n < nth ) then
         word = ' '
         loc  =  0

!
!     Otherwise, find the rest of WORD (it continues until the next
!     blank), and return the current LOC.
!
      else
         i = index ( string(loc: ), ' ' )

         if ( i == 0 ) then
            word = string(loc: )
         else
            word = string(loc:loc+i-1)
         end if
      end if

      return
      end subroutine nthwd

      !> Parse a quoted string token.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string containing a `quoted string
      !>              token'. Quoted string tokens are sequences of
      !>              characters that represent literal strings.
      !>              Syntactically, a string token is a sequence of
      !>              characters that begins and ends with a designated
      !>              `quote character'. Within the token, any
      !>              occurrence of the quote character is indicated by
      !>              an adjacent pair of quote characters: for example,
      !>              if the quote character is
      !>
      !>                 "
      !>
      !>              then the token representing one instance of this
      !>              character is
      !>
      !>                 """"
      !>
      !>              Here the first quote indicates the beginning of the
      !>              token, the next two quotes together indicate a
      !>              single quote character that constitutes the
      !>              `contents' of the token, and the final quote
      !>              indicates the end of the token.
      !>
      !>              Leading and trailing blanks in STRING are ignored.
      !>              The input string may not contain any trailing,
      !>              non-blank characters after the final quote
      !>              character.
      !>
      !>              All blanks occurring between the bracketing
      !>              quote characters in STRING are significant.
      !>
      !>     QCHAR    is the quote character. This is always a single
      !>              character. The characters
      !>
      !>                 "  and '
      !>
      !>              are common choices, but any non-blank character is
      !>              accepted. Case *is* significant in QCHAR.
      !>
      !>## Detailed Output
      !>
      !>     VALUE    is the string resulting from parsing STRING.
      !>              VALUE is obtained from STRING by removing the
      !>              bracketing quote characters and replacing each pair
      !>              of quote characters in the interior of STRING with
      !>              a singleton quote character. The value resulting
      !>              from parsing STRING will occupy the leftmost
      !>              characters of VALUE, but will not be
      !>              `left-justified', since leading blanks within
      !>              the quoted string token in STRING are significant.
      !>
      !>     LENGTH   is the number of significant characters in VALUE.
      !>              This is the number of characters in the string
      !>              resulting from parsing the input string. Because
      !>              parsed strings containing embedded quote
      !>              characters are shorter than the unparsed tokens
      !>              that represent them, LENGTH may be less than the
      !>              number of characters between the bracketing quote
      !>              characters of the input string.
      !>
      !>     ERROR    is a logical flag indicating whether a parse error
      !>              occurred; if so, ERROR is returned with the value
      !>              .TRUE.
      !>
      !>     ERRMSG   is a message indicating that STRING could not be
      !>              parsed due to an error in its structure. If the
      !>              input string token was successfully parsed, ERRMSG
      !>              will be returned as a blank string.
      !>
      !>     PTR      indicates the character position at which an
      !>              error in STRING was detected. If STRING is
      !>              correctly formed, PTR is returned as 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the input argument QCHAR is blank, a parse error will be
      !>         indicated by ERROR; PTR will be set to 1. The contents of
      !>         VALUE and LENGTH are undefined in this case.
      !>
      !>     2)  If STRING is not a well-formed quoted string, a parse error
      !>         will be indicated by ERROR and PTR. The contents of VALUE
      !>          and LENGTH are undefined in this case.
      !>
      !>     3)  If the length of the output string VALUE is too short to
      !>         accommodate the parsed string token produced by this routine,
      !>         a parse error message to this effect is generated.  VALUE
      !>         will contain the as much as possible of the result, truncated
      !>         on the right.
      !>
      !>     4)  If STRING consists of a null string token, that is, two
      !>         adjacent quote characters with nothing but blanks on either
      !>         side, a parse error will be indicated. The contents of VALUE
      !>          and LENGTH are undefined in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Quote characters may be ANY non-blank character. For example, the
      !>     ampersand
      !>
      !>        &
      !>
      !>     is a perfectly valid quote character. If we were using the
      !>     ampersand as the quote character, then the term `doubled quote'
      !>     in the following discussion would refer to the sequence
      !>
      !>        &&
      !>
      !>     not the character
      !>
      !>        "
      !>
      !>     The string tokens that are expected inputs to this routine are
      !>     Fortran-style quoted strings: they start and end with quote
      !>     characters. In the interior of any such token, any quote
      !>     characters are represented by doubled quote characters. These
      !>     rules imply that the number of quote characters in a valid quoted
      !>     string token is always even. The end of a quoted string token is
      !>     located at the first even-numbered quote character, counting from
      !>     the initial quote character, that is  not the first member of a
      !>     pair of quotes indicating an embedded quote character.
      !>
      !>     This routine is meant to be used together with the SPICELIB
      !>     routine LXQSTR (Lex quoted string):  LXQSTR is used to identify
      !>     quoted string tokens, and this routine converts the tokens to
      !>     string values.
      !>
      !>## Examples
      !>
      !>     1)  The table below illustrates the action of this routine.
      !>
      !>     STRING               QCHAR   VALUE           LENGTH       ERROR
      !>     =================================================================
      !>     "SPICE"              "       SPICE           5            .FALSE.
      !>     "SPICE"              '       <undefined>     <undefined>  .TRUE.
      !>     """SPICE"" system"   "       "SPICE" system  14           .FALSE.
      !>     " "                  "       <single blank>  1            .FALSE.
      !>     ''                   '       <undefined>     <undefined>  .TRUE.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 03-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT)
      !>
      !>        Corrected the problem with an uninitialized variable
      !>        INLEN that was detected on the HP and reported by Steve
      !>        Schlaifer of MASL.
      !>
      !>-    SPICELIB Version 1.0.0, 21-NOV-1994 (NJB)
      elemental subroutine parsqs ( string, qchar, value, length, error, errmsg, &
                          ptr                                          )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Quoted string to be parsed.
      character(1), intent(in) :: qchar  !! Quote delimiter character.
      character(*), intent(out) :: value  !! Parsed string.
      integer(ip), intent(out) :: length  !! Number of significant characters in VALUE.
      logical, intent(out) :: error  !! Logical error flag.
      character(*), intent(out) :: errmsg  !! Message indicating whether errors have occurred.
      integer(ip), intent(out) :: ptr  !! Position in string where an error occurred.

      character(1) :: chr

      integer(ip) :: first
      integer(ip) :: ipos
      integer(ip) :: inlen
      integer(ip) :: last
      integer(ip) :: outlen
      integer(ip) :: opos

!
!     Error free, no check-in required.  No parse error to start with.
!     No characters in the parsed string to start with.
!
      error  = .false.
      errmsg = ' '
      ptr    = 0
      length = 0

!
!     Reject invalid quote characters.
!
      if ( qchar == ' ' ) then

         error  = .true.
         errmsg = 'The quote character must be non-blank, but isn''t'
         ptr    = 1

      end if

!
!     Grab the lengths of the string arguments.
!
      inlen  =  len(string)
      outlen =  len(value )

!
!     The token to be parsed extends from the first non-blank
!     character to the last non-blank character of STRING.
!
      first  =  verify(string, ' ')
      last   =  len_trim(string)

      if ( first == 0 ) then

         error  = .true.
         errmsg = 'Blank input string'
         ptr    =  inlen
         return

      end if

!
!     The input token must be bracketed by quote characters.
!
      if ( string(first:first) /= qchar ) then

         error  = .true.
         errmsg = 'String token does not start with quote character'
         ptr    =  first
         return

      else if ( string(last:last) /= qchar ) then

         error  = .true.
         errmsg = 'String token does not end with quote character'
         ptr    =  last
         return

      end if

!
!     Null strings are not accepted.
!
      if ( first == last-1 ) then

         error  = .true.
         errmsg = 'Null (zero length) string token'
         ptr    = last
         return

      end if

!
!     Transfer the interior characters of the input string to the output
!     string, replacing each doubled quote character with a single quote
!     character.  The interior of the string must not contain any
!     un-doubled quotes; we have a parse error if we find any such
!     stragglers.
!
      opos  =  1
      ipos  =  first + 1

      do while (  ( ipos <= last-1 ) .and. ( opos <= outlen )  )
!
!        At this point, IPOS points to the current input character to
!        examine; OPOS points to the currently available position to
!        write to in the output string.
!
         chr  =  string(ipos:ipos)

         if ( chr /= qchar ) then
!
!           This is the normal, non-quote case.  Transfer the
!           character to the output string and advance both the input
!           and output character positions.
!
            value(opos:opos)  =  chr
            ipos              =  ipos   + 1
            opos              =  opos   + 1
            length            =  length + 1

         else
!
!           We've encountered a quote character.  By construction, the
!           parity of this quote character must be odd.  The quote must
!           be followed immediately by a second, interior quote.
!
            if ( ipos == last-1 ) then
!
!              We're already looking at the last interior input
!              character.
!
               error  = .true.
               errmsg = 'Quote character is unmatched or else string '// &
                        'ends without final quote; take your pick'
               ptr    = ipos
               return

            else if ( string(ipos+1:ipos+1) /= qchar ) then

               error  =  .true.
               errmsg =  'Interior quote character is not doubled'
               ptr    =  ipos
               return

            else
!
!              This is the normal case; the quote character is doubled.
!              Transfer a single quote character to the output string,
!              and skip over the second quote in the input string.
!
               value(opos:opos)  =  chr
               opos              =  opos   + 1
               length            =  length + 1
               ipos              =  ipos   + 2

            end if

         end if

      end do

      if ( ipos < last-1 ) then
!
!        We must have stopped transferring characters to VALUE
!        because we ran out of room.
!
         error  =  .true.
         errmsg =  'Output string too short, truncated on right'
         ptr    =  ipos
         return

      end if

      if ( opos < outlen ) then
!
!        Blank-pad the trailing portion of the output string.
!
         value(opos:) = ' '

      end if

      return
      end subroutine parsqs

      !> Determine the printable width of a character array.
      !>
      !> The function returns the index of the rightmost printable
      !> character in the entire array of strings ARRAY.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the rightmost printable
      !>     character in the entire array. This is equivalent to the maximum
      !>     value of LASTPC for the array, but somewhat more efficient to
      !>     compute. If NELT is not greater than zero, PCWID is zero.
      !>
      !>     ASCII characters in the range [33,126] are considered printable.
      !>     Blanks are not considered printable. Thus, for character arrays
      !>     padded with blanks, PCWID is equivalent to NBWID.
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
      !>     Find the last printable character in the first element of the
      !>     array. Search the rest of the elements, starting at the end of
      !>     each string and moving back just far enough to determine if the
      !>     current string is wider than any of the previous ones. (This
      !>     makes PCWID somewhat more efficient than LASTPC.)
      !>
      !>     If any of the strings is found to end in a printable character,
      !>     PCWID is just the length of the individual elements of the array,
      !>     and the search is discontinued immediately.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following strings,
      !>
      !>           ARRAY(1) = 'A string of medium length'
      !>           ARRAY(2) = 'A very long string, much longer than the rest'
      !>           ARRAY(3) = 'Shorter'
      !>           ARRAY(4) = 'Short'
      !>
      !>     padded to length 47 with null characters instead of blanks.
      !>     Then the value returned by
      !>
      !>           WIDEST = PCWID ( ARRAY, 4 )
      !>
      !>     is 45.
      !>
      !>     If the word 'rest' in the second element is changed to 'others',
      !>     the value returned is 47, and the search is terminated after the
      !>     second element.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure integer(ip) function pcwid ( array, nelt )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: array   ( * )  !! Input array.
      integer(ip), intent(in) :: nelt  !! Number of elements in the array.

      integer(ip) :: strlen
      integer(ip) :: i
      integer(ip) :: j

!
!     Nonsense case: no elements.
!
      if ( nelt < 1 ) then

         pcwid = 0

!
!     Get the length of the individual elements of the string.
!     So far, we have no maximum width, because we haven't examined
!     any elements.
!
      else

         strlen = len ( array(1) )
         pcwid  = 0
         i      = 0

!
!        Continue until the end of the array is reached, or until
!        a string with no trailing non-printing characters is found.
!
         do while ( i < nelt  .and.  pcwid < strlen )

!
!           Search no further than the current value of PCWID.
!
            i = i + 1
            j = strlen

            do while (        j                       > pcwid &
                       .and.  ichar ( array(i)(j:j) ) <    33 &
                       .and.  ichar ( array(i)(j:j) ) >   126 )

               j = j - 1
            end do

!
!           PCWID only increases if this string was wider than all
!           previous strings.
!
            pcwid = max ( pcwid, j )

         end do

      end if

      return
      end function pcwid

      !> Find the first occurrence in a string of a substring, starting at
      !> a specified location, searching forward.
      !>
      !> The function returns the index of SUBSTR in STR following START
      !>
      !>## Detailed Input
      !>
      !>     STR      is any character string.
      !>
      !>     SUBSTR   is a substring to look for in STR. Spaces in
      !>              SUBSTR are significant.
      !>
      !>     START    is the position in STR to begin looking for SUBSTR.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the beginning of the first
      !>     substring of STR that begins on or after index START and is equal
      !>     to SUBSTR. If the substring cannot be found after START, the
      !>     function is returns 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If START is less than 1, the search begins at the first
      !>         character of the string.
      !>
      !>     2)  If START is greater than the length of the string, POS
      !>         returns zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     POS is case sensitive.
      !>
      !>     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
      !>     POSR, CPOSR, NCPOSR) is described in the Required Reading.
      !>
      !>     Those familiar with the .TRUE. BASIC language should note that
      !>     these functions are equivalent to the .TRUE. BASIC intrinsic
      !>     functions with the same names.
      !>
      !>## Examples
      !>
      !>     Let STRING = 'AN ANT AND AN ELEPHANT        '
      !>                   123456789012345678901234567890
      !>
      !>      Normal (Sequential) Searching:
      !>      ------------------------------
      !>
      !>            POS ( STRING, 'AN',  1 ) =  1
      !>            POS ( STRING, 'AN',  3 ) =  4
      !>            POS ( STRING, 'AN',  6 ) =  8
      !>            POS ( STRING, 'AN', 10 ) = 12
      !>            POS ( STRING, 'AN', 14 ) = 20
      !>            POS ( STRING, 'AN', 22 ) =  0
      !>
      !>      START out of bounds:
      !>      --------------------
      !>
      !>            POS ( STRING, 'AN', -5 ) =  1
      !>            POS ( STRING, 'AN',  0 ) =  1
      !>            POS ( STRING, 'AN', 31 ) =  0
      !>            POS ( STRING, 'AN', 44 ) =  0
      !>
      !>      Significance of Spaces:
      !>      -----------------------
      !>
      !>            POS ( STRING, 'AN',    1 ) =  1
      !>            POS ( STRING, ' AN',   1 ) =  3
      !>            POS ( STRING, ' AN ',  1 ) = 11
      !>            POS ( STRING, ' AN  ', 1 ) =  0
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
      !>
      !>        The Required Reading file POSITION was renamed to SCANNING.
      !>        This header was updated to reflect the change.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function pos ( str, substr, start )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str  !! A character string
      character(*), intent(in) :: substr  !! Substring to locate in the character string.
      integer(ip), intent(in) :: start  !! Where to start looking for SUBSTR in STR.

      logical :: found
      integer(ip) :: lenstr
      integer(ip) :: offset
      integer(ip) :: lchnce
      integer(ip) :: b

!
!     Let's find out how big every body is.
!
      lenstr =  len   ( str                    )
      offset =  max   ( 0,  len ( substr ) - 1 )
      lchnce =  lenstr - offset

      b      =  max ( 1, start )

!
!     Look for the string until we run find it or run out of room to
!     look.
!
      found  = .false.
      pos    =  0

      do while ( .not. found )

         if ( b > lchnce ) then
            return
         else if ( str(b:b+offset) == substr ) then
            pos = b
            return
         else
            b   = b + 1
         end if

      end do

      return
      end function pos

      !> Find the first occurrence in a string of a substring, starting at
      !> a specified location, searching in reverse.
      !>
      !> The function returns the index of SUBSTR in STR preceding START
      !>
      !>## Detailed Input
      !>
      !>     STR      is any character string.
      !>
      !>     SUBSTR   is a substring to look for in STR. Spaces in
      !>              SUBSTR are significant.
      !>
      !>     START    is the position in STR to begin looking for SUBSTR.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the beginning of the last
      !>     substring of STR that begins on or before index START and is
      !>     equal to SUBSTR. If the substring cannot be found starting at or
      !>     before START, the function is returns 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If START is less than 1, POSR returns zero.
      !>
      !>     2)  If START is greater than LEN(STRING), the search begins
      !>         at the last character of the string.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     POSR is case sensitive.
      !>
      !>     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
      !>     POSR, CPOSR, NCPOSR) is described in the Required Reading.
      !>
      !>     Those familiar with the .TRUE. BASIC language should note that
      !>     these functions are equivalent to the .TRUE. BASIC intrinsic
      !>     functions with the same name.
      !>
      !>## Examples
      !>
      !>     Let STRING = 'AN ANT AND AN ELEPHANT        '
      !>                   123456789012345678901234567890
      !>
      !>     Normal (Sequential) Searching:
      !>     ------------------------------
      !>
      !>           POSR ( STRING, 'AN',  31 ) = 20
      !>           POSR ( STRING, 'AN',  19 ) = 12
      !>           POSR ( STRING, 'AN',  11 ) =  8
      !>           POSR ( STRING, 'AN',   7 ) =  4
      !>           POSR ( STRING, 'AN',   3 ) =  1
      !>           POSR ( STRING, 'AN',   0 ) =  0
      !>
      !>     START out of bounds:
      !>     --------------------
      !>
      !>           POSR ( STRING, 'AN', -5 ) =  0
      !>           POSR ( STRING, 'AN',  0 ) =  0
      !>           POSR ( STRING, 'AN', 31 ) = 20
      !>           POSR ( STRING, 'AN', 44 ) = 20
      !>
      !>     Significance of Spaces:
      !>     -----------------------
      !>
      !>           POSR ( STRING, 'AN',    31 ) =  20
      !>           POSR ( STRING, ' AN',   31 ) =  11
      !>           POSR ( STRING, ' AN ',  31 ) =  11
      !>           POSR ( STRING, ' AN ',  10 ) =   0
      !>           POSR ( STRING, ' AN  ', 31 ) =   0
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.4, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.3, 25-AUG-1994 (HAN) (KSZ)
      !>
      !>        $Examples section of the header used POS instead of POSR.
      !>        Also, some examples were incorrect. They have been corrected.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
      !>
      !>        The Required Reading file POSITION was renamed to SCANNING.
      !>        This header was updated to reflect the change.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function posr ( str, substr, start )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str  !! A character string
      character(*), intent(in) :: substr  !! Substring to locate in the character string.
      integer(ip), intent(in) :: start  !! Where to start looking for SUBSTR in STR.

      logical :: found
      integer(ip) :: lenstr
      integer(ip) :: offset
      integer(ip) :: fchnce
      integer(ip) :: b

!
!     Let's find out how big every body is.
!
      lenstr = len   ( str                    )
      offset = max   ( 0,  len ( substr ) - 1 )
      fchnce = lenstr - offset

!
!     Look for the string until we run find it or run out of room to
!     look.
!
      b      = min(fchnce,start)
      found  = .false.
      posr   = 0

      do while ( .not. found )

         if ( b <= 0 ) then
            return
         else if ( str(b:b+offset) == substr ) then
            posr = b
            return
         else
            b    = b - 1
         end if

      end do

      return
      end function posr

      !> Add a prefix to a character string.
      !>
      !>## Detailed Input
      !>
      !>     PREF     is the prefix to be added to the string. Trailing
      !>              blanks are ignored. (A blank prefix is interpreted
      !>              as a null prefix.)
      !>
      !>     SPACES   is the number of spaces (blanks) in the output
      !>              string separating the last non-blank character
      !>              of the prefix from the first (blank or non-blank)
      !>              character of the suffix. Typically, this will be
      !>              zero or one. If not positive, SPACES defaults to
      !>              zero.
      !>
      !>     STRING   on input is the suffix to which the prefix is to
      !>              be added. Leading blanks are significant.
      !>
      !>## Detailed Output
      !>
      !>     STRING   on output is the is the prefixed string. If STRING
      !>              is not large enough to contain the output string,
      !>              the output string is truncated on the right.
      !>
      !>              STRING may NOT overwrite PREF.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If SPACES is negative it is treated as zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The suffix is shifted to the right to make room for the prefix
      !>     and required spaces, which are then added to the front of the
      !>     string. (The shift operation handles any necessary truncation.)
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of PREFIX.
      !>
      !>           PREF         STRING (input)   SPACES    STRING (output)
      !>           ----------   --------------   ------    ---------------
      !>           'abc     '   'def    '             0    'abcdef '
      !>           'abc     '   'def    '             1    'abc def'
      !>           'abc     '   ' def   '             0    'abc def'
      !>           'abc     '   ' def   '             1    'abc  de'
      !>           ' abc    '   'def    '             0    ' abcdef'
      !>           ' abc    '   'def    '             1    ' abc de'
      !>           ' abc    '   ' def   '            -1    ' abc de'
      !>           '        '   'def    '             0    'def    '
      !>           '        '   'def    '             1    ' def   '
      !>           ' abc    '   '       '             0    ' abc   '
      !>
      !>## Restrictions
      !>
      !>     1)  PREF and STRING must be distinct.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 18-MAR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 28-FEB-1989 (WLT)
      !>
      !>         Reference to SHIFT replaced by SHIFTL.
      elemental subroutine prefix ( pref, spaces, string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: pref  !! Prefix.
      integer(ip), intent(in) :: spaces  !! Number of spaces separating prefix and suffix.
      character(*), intent(inout) :: string  !! Suffix on input, string on output.

      integer(ip) :: plen
      integer(ip) :: slen
      integer(ip) :: shift

!
!     L is the location of the last non-blank character in the prefix.
!     PLEN is the length of the prefix. Remember that a blank (null)
!     prefix has zero length.
!
      plen = len_trim(pref)

!
!     SLEN is the allocated length of the string.
!
      slen = len ( string )

!
!     We can't just do a concatenation, because the input and output
!     strings are of indeterminate length. (This would be a violation
!     of the ANSI Fortran 77 standard.) Instead, we will shift the
!     suffix to the right in order to make room for the prefix and
!     the required number of spaces. If part of the string gets
!     truncated, well, that's life.
!
      shift = plen + max ( spaces, 0 )

!     PEPPER: pass a copy of STRING as the input (SHIFTR allows OUT to
!     overwrite IN, but Fortran does not allow the aliasing).
      call shiftr ( (string), shift, ' ', string )

!
!     Put the non-blank part of the prefix in the vacated part of
!     the string. The spaces will fill themselves in.
!
      if ( plen > 0 ) then

         if ( shift < slen ) then
            string(1:shift) = pref

         else
            string = pref
         end if

      end if

      return
      end subroutine prefix

      !> Enclose (quote) the non-blank part of a character string
      !> between delimiting symbols.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input string to be quoted.
      !>
      !>     LEFT,
      !>     RIGHT    are the left and right delimiters to be used in
      !>              quoting the input string. These may be the same
      !>              character (apostrophe, vertical bar), complementary
      !>              characters (left and right parentheses, brackets,
      !>              or braces), or two totally unrelated characters.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is the non-blank part
      !>              of the input string delimited by LEFT and RIGHT.
      !>              If the output string is not large enough to contain
      !>              the quoted string, it is truncated on the right.
      !>              (The right delimiter would be lost in this case.)
      !>
      !>              If the input string is blank, the output string is
      !>              a single quoted blank.
      !>
      !>              OUT may overwrite IN.
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
      !>     The first character of the output string is the left delimiter,
      !>     LEFT. This is followed immediately by the non-blank part of the
      !>     input string, which is in turn followed by the right delimiter,
      !>     RIGHT.
      !>
      !>     If the input string is blank (has no non-blank characters),
      !>     a single quoted blank is returned.
      !>
      !>## Examples
      !>
      !>     Let
      !>           IN    = '    This string has leading and trailing blanks  '
      !>           LEFT  = '('
      !>           RIGHT = ')'
      !>
      !>     Then
      !>           OUT   = '(This string has leading and trailing blanks)    '
      !>
      !>     Or, let IN = '         '. Then OUT = '( )'.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine quote ( in, left, right, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input string.
      character(1), intent(in) :: left  !! Left delimiter.
      character(1), intent(in) :: right  !! Right delimiter.
      character(*), intent(out) :: out  !! Output (quoted) string.

!
!     Check for blank string first.
!
      if ( in == ' ' ) then
         out = left
         call suffix ( right, 1, out )

      else
         out = in(verify(in, ' ') : len_trim(in))
         call prefix ( left,  0, out )
         call suffix ( right, 0, out )
      end if

      return
      end subroutine quote

      !> Replace all occurrences of a single character with a second
      !> character.
      !>
      !>## Detailed Input
      !>
      !>     INSTR    is the input character string, possibly containing
      !>              one or more occurrences of the character OLD.
      !>
      !>     OLD      is the character to be replaced wherever it occurs in
      !>              the input string.
      !>
      !>     NEW      is the character which is to replace each occurrence
      !>              of the character OLD in the output string.
      !>
      !>## Detailed Output
      !>
      !>     OUTSTR   is the output string. This is the input string
      !>              with every occurrence of the character OLD replaced
      !>              by the character NEW.
      !>
      !>              OUTSTR may overwrite INSTR.
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
      !>     Copy the contents of the input string to the output string
      !>     a character at a time, replacing each occurrence of OLD with NEW.
      !>     If the output string is not long enough to contain the input
      !>     string, it is truncated on the right.
      !>
      !>## Examples
      !>
      !>     Let
      !>           INSTR  = 'Woodsy is the Anti-Pollution Owl.'
      !>           OLD    = 'O'
      !>           NEW    = 'E'
      !>     then
      !>           OUTSTR = 'Woodsy is the Anti-Pollution Ewl.'
      !>
      !>     Note the case-sensitivity of REPLCH. The lowercase o's are
      !>     not affected.
      !>
      !>     REPLCH may similarly be used to replace control characters
      !>     (such as tab stops, line feeds, and nulls) with regular ASCII
      !>     characters (such as blanks).
      !>
      !>## Restrictions
      !>
      !>     1)  REPLCH is sensitive to case, as shown in the examples above.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      elemental subroutine replch ( instr, old, new, outstr )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: instr  !! Input string.
      character(1), intent(in) :: old  !! Character to be replaced.
      character(1), intent(in) :: new  !! Replacement character.
      character(*), intent(out) :: outstr  !! Output string.

      integer(ip) :: i

!
!     Move the input string to the output string. If it's too long,
!     this will truncate it.
!
      outstr = instr

!
!     Check each character of OUTSTR and replace as necessary.
!
      do i = 1, len ( outstr )

         if ( outstr(i:i) == old ) then
            outstr(i:i) = new
         end if

      end do

      return
      end subroutine replch

      !> Replace the Nth word in a string with a new word.
      !>
      !>## Detailed Input
      !>
      !>     INSTR    is the input character string, possibly containing
      !>              one or more words, where a word is any string of
      !>              consecutive non-blank characters delimited by a
      !>              blank or by either end of the string.
      !>
      !>     NTH      is the number of the word to be replaced. Words
      !>              are numbered from one. If NTH is less than one,
      !>              or greater than the number of words in the string,
      !>              no replacement is made.
      !>
      !>     NEW      is the word which is to replace the specified word
      !>              in the input string. Leading and trailing blanks
      !>              are ignored. If the replacement word is blank,
      !>              the original word is simply removed.
      !>
      !>## Detailed Output
      !>
      !>     OUTSTR   is the output string. This is the input string
      !>              with the N'th word replaced by the word NEW.
      !>              Any blanks originally surrounding the replaced
      !>              word are retained.
      !>
      !>              OUTSTR may overwrite INSTR.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NEW is blank, then the Nth word is replaced by a single
      !>         space.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The effect of this routine is to remove the old word with
      !>     REMSUB, and insert the replacement word with INSSUB.
      !>
      !>## Examples
      !>
      !>     Let
      !>           INSTR  = '  Woodsy is the Anti-Pollution  Owl.'
      !>
      !>     and
      !>           NEW    = '   an   '
      !>
      !>     then the following values of NTH yield the following strings.
      !>
      !>           NTH      OUTSTR
      !>           ---      ------------------------------------------
      !>            -1      '  Woodsy is the Anti-Pollution  Owl.'
      !>             0      '  Woodsy is the Anti-Pollution  Owl.'
      !>             1      '  an is the Anti-Pollution  Owl.'
      !>             3      '  Woodsy is an Anti-Pollution  Owl.'
      !>             4      '  Woodsy is the an  Owl.'
      !>             5      '  Woodsy is the Anti-Pollution  an'
      !>             6      '  Woodsy is the Anti-Pollution  Owl.'
      !>
      !>     Note that in the first, second, and last cases, the string
      !>     was not changed. Note also that in the next to last case,
      !>     the final period was treated as part of the fifth word in the
      !>     string.
      !>
      !>     If NEW is ' ', and NTH is 3, then
      !>
      !>           OUTSTR = '  Woodsy is Anti-Pollution  Owl.'
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
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN) (NJB)
      elemental subroutine replwd ( instr, nth, new, outstr )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: instr  !! Input string.
      integer(ip), intent(in) :: nth  !! Number of the word to be replaced.
      character(*), intent(in) :: new  !! Replacement word.
      character(*), intent(out) :: outstr  !! Output string.

      character(2) :: short
      integer(ip) :: begin
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: f
      integer(ip) :: l
      integer(ip) :: k
      integer(ip) :: n
      integer(ip) :: shift

!
!     First just shift the input string into the output string,
!     then do everything in place (for the case when the new
!     word is longer than the old one.  When its shorter we'll
!     need to change this scheme slightly.)
!
      outstr = instr
!
!     Where does the word to be replaced begin? If there is none,
!     just return the original string.
!
      call nthwd  ( outstr, nth, short, begin )

      if ( begin == 0 ) then

         return

      end if
!
!     Otherwise, find out where it ends as well.
!
      call fndnwd ( instr, begin, i, j )

!
!     Now insert only the non-blank part of the replacement string.
!     If the replacement string is blank, don't insert anything.
!
      if ( new /= ' ' ) then

         f = verify(new, ' ')
         l = len_trim(new)

!
!        Except in the lucky case that the word to insert is the
!        same length as the word it's replacing, we will have
!        to shift right or left by some amount.  Compute the
!        appropriate amount to shift right.
!
         shift = ( l - f ) - ( j - i )

      else

         f     = 1
         l     = 1
         shift = i - j

      end if

      if      ( shift > 0 ) then

!
!        To shift right in place start at the right most character
!        of the string and copy the character SHIFT spaces to the
!        left.
!
         k = len(outstr)
         n = k - shift

         do while ( n > j )
            outstr(k:k) = outstr(n:n)
            k           = k - 1
            n           = n - 1
         end do

!
!        Once the appropriate characters have been shifted out
!        of the way, replace the opened space with the new
!        word.
!
         do while (       ( f <= l           ) &
                    .and. ( i <= len(outstr) ) )
            outstr(i:i) = new(f:f)
            f           = f + 1
            i           = i + 1
         end do

      else

!
!        We have a left shift. Fill in the first part of the word
!        we are replacing with the new one.
!
         do while (       ( f <= l           ) &
                    .and. ( i <= len(outstr) ) )

            outstr(i:i) = new(f:f)
            f           = f + 1
            i           = i + 1

         end do

!
!        Now starting just past the end of the word we are replacing
!        shift the remainder of string left one character at a time.
!
         if ( shift < 0 ) then

            j = j + 1

            do while (       ( i <= len(outstr)  ) &
                       .and. ( j <= len(instr)   ) )

               outstr(i:i) = instr(j:j)
               i           = i + 1
               j           = j + 1
            end do

!
!           Finally pad the string with blanks.
!
            if ( i <= len(outstr) ) then
               outstr(i:) = ' '
            end if

         end if

      end if

      return
      end subroutine replwd

      !> Replace a marker with a character string.
      !>
      !>## Detailed Input
      !>
      !>     IN       is an arbitrary character string.
      !>
      !>     MARKER   is an arbitrary character string. The first occurrence of
      !>              MARKER in the input string is to be replaced by VALUE.
      !>
      !>              Leading and trailing blanks in MARKER are NOT
      !>              significant. In particular, no substitution is performed
      !>              if MARKER is blank.
      !>
      !>     VALUE    is the replacement character string.
      !>
      !>              Leading and trailing blanks in VALUE are NOT significant:
      !>              the portion of VALUE that is substituted for MARKER
      !>              extends from its first non-blank character to its last
      !>              non-blank character.
      !>
      !>              However, if VALUE is blank, a single blank is substituted
      !>              for the first occurrence of MARKER.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the string obtained by substituting VALUE (leading and
      !>              trailing blanks excepted) for the first occurrence of
      !>              MARKER in the input string.
      !>
      !>              OUT and IN must be identical or disjoint.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If OUT does not have sufficient length to accommodate the
      !>         result of the substitution, the result will be truncated on
      !>         the right.
      !>
      !>     2)  If MARKER is blank, or if MARKER is not a substring of IN,
      !>         no substitution is performed. (OUT and IN are identical.)
      !>
      !>     3)  If VALUE is blank, a single blank is substituted for the
      !>         first occurrence of MARKER.
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
      !>        'Fifty-one pictures were found in directory [USER.DATA].'
      !>
      !>     might be constructed from the fixed string
      !>
      !>        '#1 pictures were found in directory #2.'
      !>
      !>     by the calls
      !>
      !>        CALL REPMCT ( STRING, '#1', N_PICS,  'C', STRING )
      !>        CALL REPMC  ( STRING, '#2', DIR_NAME,     STRING )
      !>
      !>     which substitute the cardinal text 'Fifty-one' and the character
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
      !>     1) The following example illustrate the use of REPMC to
      !>        replace a marker within a string with a character string
      !>        value.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REPMC_EX1
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
      !>C     1. Single marker
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMC ( INSTR, MARKER, 'append', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 1: Single marker.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     2. Multiple markers
      !>C
      !>      MARKER = ' XX '
      !>      INSTR  = 'The token XX was not recognized. Was it XX?'
      !>
      !>      CALL REPMC ( INSTR, MARKER, '  FND  ', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 2: Multiple markers.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Case 1: Single marker.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  append
      !>
      !>         Case 2: Multiple markers.
      !>            Input : The token XX was not recognized. Was it XX?
      !>            Output: The token FND was not recognized. Was it XX?
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 21-AUG-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example, based on existing fragments.
      !>
      !>        Added REPML to the list of available replace marker routines.
      !>
      !>-    SPICELIB Version 1.2.0, 21-SEP-2013 (BVS)
      !>
      !>        Minor efficiency update: the routine now looks up the first
      !>        and last non-blank characters only once.
      !>
      !>-    SPICELIB Version 1.1.0, 15-AUG-2002 (WLT)
      !>
      !>        The routine is now error free.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) (IMU)
      elemental subroutine repmc ( in, marker, value, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      character(*), intent(in) :: value  !! Replacement string.
      character(*), intent(out) :: out  !! Output string.

      integer(ip) :: mrknbf
      integer(ip) :: mrknbl

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse

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
!     Okay, MARKER is non-blank and has been found. If VALUE is
!     blank, substitute a single blank. (This removes the marker.)
!     Otherwise substitute the non-blank portion.
!
      if ( value == ' ' ) then

         call zzrepsub ( in, &
                         mrkpsb, &
                         mrkpse, &
                         ' ', &
                         out  )

      else

         call zzrepsub ( in,  mrkpsb,  mrkpse,  value( verify(value, ' ') : len_trim(value) ),  out  )

      end if

      return
      end subroutine repmc

      !> Right justify a character string.
      !>
      !>## Detailed Input
      !>
      !>     INPUT    is the input character string.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT   is the output character string, right justified.
      !>              If INPUT is too large to fit into OUTPUT, it is
      !>              truncated on the left.
      !>
      !>              OUTPUT may overwrite INPUT.
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
      !>     Any trailing blanks in the input string are removed, and
      !>     the remaining string is copied to the output string.
      !>
      !>## Examples
      !>
      !>     The following examples should illustrate the use of RJUST.
      !>
      !>        'ABCDE          '   becomes  '          ABCDE'
      !>        'AN EXAMPLE     '            '     AN EXAMPLE'
      !>        '   AN EXAMPLE  '            '     AN EXAMPLE'
      !>        '               '            '               '
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 11-DEC-1989 (IMU)
      !>
      !>         Did not work on Sun when INPUT and OUTPUT were
      !>         the same string, and where the initial and final
      !>         locations of the non-blank part of the string
      !>         overlapped.
      !>
      !>         The solution is to move the characters one by one,
      !>         starting from the right side of the input string.
      !>         That way, nothing gets clobbered.
      elemental subroutine rjust ( input, output )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: input  !! Input character string.
      character(*), intent(out) :: output  !! Output character string, right justified.

      integer(ip) :: first
      integer(ip) :: i
      integer(ip) :: last
      integer(ip) :: loc
      integer(ip) :: start

!
!     Blank string? It's all the same.
!
      if ( input == ' ' ) then
         output = input

!
!     Get the first non-blank character. Start OUTPUT at that point.
!
      else
         first  = verify(input, ' ')
         last   = len_trim(input)
         start  = len ( output ) - (last-first)

!
!        If the input string is too long (START < 1), move FIRST
!        up a little to truncate on the left.
!
         if ( start < 1 ) then
            first = first + (1-start)
            start = 1
         end if

!
!        Move the characters in reverse order, to keep from stomping
!        anything if the operation is being done in place.
!
         loc = len ( output )

         do i = last, first, -1
            output(loc:loc) = input(i:i)
            loc = loc - 1
         end do

!
!        Clear the first part of OUTPUT, if necessary.
!
         if ( start > 1 ) then
            output(1:start-1) = ' '
         end if

      end if

      return
      end subroutine rjust

      !> Return the maximum of 1 and the location of the last non-blank
      !> character in the string.
      !>
      !> The function returns the maximum of 1 and the location of the
      !> last non-blank character in STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a string to be trimmed: the location of the
      !>              last non-blank character is desired.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the maximum of 1 and the location of the
      !>     last non-blank character in STRING.
      !>
      !>     In particular, when STRING is blank, the function returns the
      !>     value 1.
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
      !>     When writing a character string to a file, we usually are content
      !>     to omit the trailing blanks. We'd like to use LASTNB as an upper
      !>     substring bound, but we have to handle the case where LASTNB
      !>     returns 0, so we write:
      !>
      !>        WRITE ( UNIT, '(A)' ),  STRING ( : MAX (1, LASTNB (STRING)) )
      !>
      !>     This can be simplified using RTRIM:
      !>
      !>        WRITE ( UNIT, '(A)' ),  STRING ( : RTRIM (STRING) )  )
      !>
      !>     This routine has a counterpart, LTRIM, which finds the maximum of
      !>     1 and the position of the first non-blank character of a string.
      !>
      !>## Examples
      !>
      !>     1)  Write the non-blank portion of each element of a character
      !>         cell to file SPUD.DAT:
      !>
      !>            DO I = 1,  CARDC (CELL)
      !>
      !>               CALL WRLINE ( 'SPUD.DAT',
      !>           .                  CELL(I) ( LTRIM (CELL) : RTRIM (CELL) ) )
      !>
      !>            END DO
      !>
      !>         When CELL(I) is blank, the string ' ' will be written.
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
      !>-    SPICELIB Version 1.0.0, 02-MAY-1990 (NJB)
      elemental integer(ip) function rtrim ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String to be trimmed.

!
!     `Just do it'.
!
      rtrim = max ( 1, len_trim(string) )

      return
      end function rtrim

      !> Determine if two characters from different strings are the
      !> same.
      !>
      !> The function returns .TRUE. if the two characters are the
      !> same.
      !>
      !>## Detailed Input
      !>
      !>     STR1     is a character string
      !>
      !>     L1       is the location (index) of a character in STR1
      !>
      !>     STR2     is a character string
      !>
      !>     L2       is the location (index) of a character in STR2
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the characters STR1(L1:L1) and
      !>     STR2(L2:L2) are the same.
      !>
      !>     If the characters are different or L1 or L2 is out of range the
      !>     function returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If either L1 or L2 is out of range the function returns
      !>         .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility function for determining whether or not
      !>     two characters in different strings are the same. This
      !>     function is intended for situation in which you need to
      !>     search two strings for a match (or mismatch).
      !>
      !>## Examples
      !>
      !>     Often you need to scan through two string comparing character
      !>     by character until a mismatch occurs. The usual way to code
      !>     this is
      !>
      !>        DO WHILE (        L1 .LE. LEN(STR1)
      !>                    .AND. L2 .LE. LEN(STR2)
      !>                    .AND. STR1(L1:L1) .EQ. STR2(L2:L2) )
      !>
      !>           L1 = L1 + 1
      !>           L2 = L2 + 1
      !>
      !>        END DO
      !>
      !>        Check L1, L2 to make sure we are still in range, etc.
      !>
      !>     The problem with this loop is that even though the check to make
      !>     sure that L1 and L2 are in range is performed, FORTRAN may
      !>     go ahead and compute the equality condition even though one of the
      !>     first two steps failed. This can lead to out of range errors
      !>     and possible halting of your program depending upon how
      !>     the routine is compiled. An alternative way to code this is
      !>
      !>        IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
      !>           ALIKE = STR1(L1:L1) .EQ. STR2(L2:L2)
      !>        ELSE
      !>           ALIKE = .FALSE.
      !>        END IF
      !>
      !>        DO WHILE ( ALIKE )
      !>
      !>           L1 = L1 + 1
      !>           L2 = L2 + 1
      !>
      !>           IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
      !>              ALIKE = STR1(L1:L1) .EQ. STR2(L2:L2)
      !>           ELSE
      !>              ALIKE = .FALSE.
      !>           END IF
      !>        END DO
      !>
      !>     However this is a much more complicated section of code. This
      !>     routine allows you to code the above loops as:
      !>
      !>        DO WHILE ( SAMCH ( STR1,L1, STR2,L2 )  )
      !>           L1 = L1 + 1
      !>           L2 = L2 + 1
      !>        END DO
      !>
      !>     The boundary checks are automatically performed and out
      !>     of range errors are avoided.
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
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
      elemental logical function samch ( str1, l1, str2, l2 )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str1  !! A character string
      integer(ip), intent(in) :: l1  !! The location (index) of a character in STR1
      character(*), intent(in) :: str2  !! A character string
      integer(ip), intent(in) :: l2  !! The location (index) of a character in STR2

      if (      l1 < 1 &
           .or. l2 < 1 &
           .or. l1 > len(str1) &
           .or. l2 > len(str2)  ) then

         samch = .false.
         return
      end if

      samch = ichar(str1(l1:l1)) == ichar(str2(l2:l2))
      return

      end function samch

      !> Determine whether or not two substrings are the same
      !>
      !> The function returns .TRUE. if the substrings are identical
      !>
      !>## Detailed Input
      !>
      !>     STR1     is a character string
      !>
      !>     B1       are integers giving the beginning and ending of a
      !>     E1       substring in STR1
      !>
      !>     STR2     is a character string
      !>
      !>     B2       are integers giving the beginning and ending of a
      !>     E2       substring in STR2
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the two substrings STR(B1:E1) and
      !>     STR(B2:E2) have the same length and the same characters.
      !>
      !>     If any of the indices B1, E1, B2, E2 are out of range or out
      !>     of order the function returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If any of the B1, E1, B2, E2 are out of range or if an
      !>         ending substring index is before a beginning substring
      !>         index, the function returns false.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is a macro for comparing two substrings of
      !>     strings and handles all of the bounds checking to avoid
      !>     out of range errors with string indices.
      !>
      !>## Examples
      !>
      !>     Suppose a string contains a number of occurrences of some
      !>     particular substring in sequence and that you need to locate
      !>     the first character that is out of this sequence or the
      !>     end of the string.
      !>
      !>     If one ignores boundary constraints this can easily be
      !>     coded as shown here: We assume the particular substring is
      !>
      !>     '/beg'
      !>
      !>        B = 1
      !>        E = B + LEN('/beg' )
      !>
      !>        DO WHILE (       E           .LE. LEN(STR)
      !>                   .AND. STRING(B:E) .EQ. '/beg' )
      !>
      !>           B = B + LEN('/beg')
      !>           E = E + LEN('/beg')
      !>
      !>        END DO
      !>
      !>        IF ( B .LT. LEN(STR) ) THEN
      !>
      !>           we've found the start of a substring of interest
      !>
      !>        ELSE
      !>
      !>           there is no substring to find.
      !>
      !>        END IF
      !>
      !>     Unfortunately, you can't rely upon FORTRAN to check the boundary
      !>     condition: E .LE. LEN(STR) and skip the second test if the first
      !>     condition if false. As a result you can get an out of range
      !>     error.
      !>
      !>     Instead you could code:
      !>
      !>     B = 1
      !>     E = B + LEN('/beg')
      !>
      !>     IF ( E .LE. LEN(STR) ) THEN
      !>        ALIKE = STRINB(B:E) .EQ. '/beg'
      !>     ELSE
      !>        ALIKE = .FALSE.
      !>     END IF
      !>
      !>     DO WHILE ( ALIKE )
      !>
      !>           B = B + LEN('/beg')
      !>           E = E + LEN('/beg')
      !>
      !>        IF ( E .LE. LEN(STR) ) THEN
      !>           ALIKE = STRINB(B:E) .EQ. '/beg'
      !>        ELSE
      !>           ALIKE = .FALSE.
      !>        END IF
      !>
      !>     END DO
      !>
      !>     However, this is code is far more effort. Using this routine
      !>     you can make a much simpler block of code.
      !>
      !>     B = 1
      !>     E = B + LEN('/beg' )
      !>
      !>     DO WHILE ( SAMSUB(STR,B,E, '/beg',1,4 ) )
      !>
      !>        B = B + LEN('/beg')
      !>        E = E + LEN('/beg')
      !>
      !>     END DO
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
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
      elemental logical function samsub ( str1, b1, e1,  str2, b2, e2 )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str1  !! A string
      integer(ip), intent(in) :: b1  !! Beginning of a substring in STR1
      integer(ip), intent(in) :: e1  !! End of s substring in STR1
      character(*), intent(in) :: str2  !! A second string
      integer(ip), intent(in) :: b2  !! The beginning of a substring in STR2
      integer(ip), intent(in) :: e2  !! The end  of s substring in STR2

      if (      ( e1      < b1           ) &
           .or. ( e2      < b2           ) &
           .or. ( b1      < 1            ) &
           .or. ( b2      < 1            ) &
           .or. ( e1      > len(str1)    ) &
           .or. ( e2      > len(str2)    ) &
           .or. ( e1 - b1 /= e2 - b2      )  ) then

         samsub = .false.
         return

      end if

      samsub = str1(b1:e1) == str2(b2:e2)

      return
      end function samsub

      !> Reject those tokens descriptors whose identities are among those
      !> of a specific collection.
      !>
      !>## Detailed Input
      !>
      !>     IDS      is a list of the identity codes that we will want to
      !>              reject.
      !>
      !>     N        is the number of different cases.
      !>
      !>     NTOKNS   is the number of tokens to consider.
      !>
      !>     IDENT    holds the identities of each token that is up for
      !>              consideration.
      !>
      !>     BEG      holds the beginning indices of each token being
      !>              considered.
      !>
      !>     END      holds the ending indices of each token being
      !>              considered.
      !>
      !>## Detailed Output
      !>
      !>     NTOKNS   is the number of tokens remaining after the rejection
      !>              process has been completed.
      !>
      !>     IDENT    holds the identities of each token remaining.
      !>
      !>     BEG      holds the beginning indices of each token remaining.
      !>
      !>     END      holds the ending indices of each token remaining.
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
      !>     This routine serves as a macro for the rejection process that
      !>     is typically performed to remove tokens whose ID's fall into
      !>     some set.
      !>
      !>## Examples
      !>
      !>     Suppose you wished to scan a string to locate the beginning and
      !>      endings of words together with punctuation, but that you did not
      !>      want to keep white space. The following code fragment illustrates
      !>      how you could use this routine to accomplish this task.
      !>
      !>      Words will be delimited by spaces, periods, commas, colons,
      !>      question marks, exclamation marks, semicolons, parentheses,
      !>      m-dashes, and quotes.
      !>
      !>      MARKS(1)  = ' '
      !>      MARKS(2)  = '.'
      !>      MARKS(3)  = ','
      !>      MARKS(4)  = '?'
      !>      MARKS(5)  = '!'
      !>      MARKS(6)  = '---'
      !>      MARKS(7)  = ':'
      !>      MARKS(8)  = ';'
      !>      MARKS(9)  = '('
      !>      MARKS(10) = ')'
      !>      MARKS(11) = '"'
      !>
      !>      NMARKS    = 11
      !>
      !>      IDS(1)    = 0
      !>      N         = 1
      !>
      !>      CALL SCANPR ( NMARKS, MARKS,  MRKLEN, MRKPTR )
      !>
      !>      IDS(1)    = BSRCHC ( ' ', NMARKS, MARKS )
      !>      N         = 1
      !>
      !>      CALL SCAN   ( STRING, MARKS,  MRKLEN, MRKPTR,
      !>     .              ROOM,   NTOKNS, IDENT,  BEG,    END )
      !>
      !>      CALL SCANRJ ( IDS, N, NTOKNS, IDENT,  BEG,    END )
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
      !>-    SPICELIB Version 1.0.1, 02-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
      pure subroutine scanrj ( ids, n, ntokns, ident, beg, end )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ids  ( * )  !! value of id's of tokens that should be dumped.
      integer(ip), intent(in) :: n  !! number of id's.
      integer(ip), intent(inout) :: ntokns  !! number of tokens input. The number kept.
      integer(ip), intent(inout) :: ident  ( * )  !! identity of each of the tokens.
      integer(ip), intent(inout) :: beg    ( * )  !! indices of beginning of tokens.
      integer(ip), intent(inout) :: end    ( * )  !! indices of endings of tokens.

      integer(ip) :: i
      integer(ip) :: j

!
!     There's not much to do, shift forward the token attributes for
!     tokens whose identities don't belong to the rejection list.
!
      j = 0

      do i = 1, ntokns

         if ( findloc(ids(1:n), ident(i), dim=1) == 0 ) then

            j        = j + 1
            ident(j) = ident(i)
            beg  (j) = beg  (i)
            end  (j) = end  (i)

         end if

      end do

      ntokns = j

      return
      end subroutine scanrj

      !> Shift the contents of a character string to the left.
      !> Characters moved past the beginning of the input string are
      !> lost. Vacant spaces are filled with a specified character.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input character string.
      !>
      !>     NSHIFT   is the number of times the string is to be
      !>              shifted. If NSHIFT is negative, OUT will be
      !>              identical to IN.
      !>
      !>     FILLC    is the character with which spaces left vacant by
      !>              the shift are to be filled.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is the input string,
      !>              shifted N times, filled with FILLC.
      !>
      !>              OUT may overwrite IN.
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
      !>     As a string is shifted left or right, the leftmost or
      !>     rightmost characters of the string disappear (as if pushed
      !>     off the end of the string). This is .TRUE. regardless of
      !>     the length of the output string.
      !>
      !>     The remaining characters are shifted simultaneously, and
      !>     the spaces vacated by those characters are filled with a
      !>     replacement character.
      !>
      !>## Examples
      !>
      !>     If FILLC = ' '
      !>
      !>        'abcde'   shifted left twice becomes     'cde  '
      !>        'abcde'   shifted right once becomes     ' abcd'
      !>
      !>     If FILLC = '.'
      !>
      !>        '12345 '  shifted right once becomes     '.12345'
      !>        'Apple '  shifted left ten times becomes '......'
      !>
      !>     Given the declarations
      !>
      !>        CHARACTER*3         SHORT
      !>        CHARACTER*10        LONG
      !>
      !>     The calls
      !>
      !>        CALL SHIFTR ( 'abcde ', 2, '-', SHORT )
      !>        CALL SHIFTR ( 'abcde ', 2, '-', LONG  )
      !>
      !>     yield the strings
      !>
      !>        SHORT = '--a'
      !>        LONG  = '--abcd    '
      !>
      !>     while the calls
      !>
      !>        CALL SHIFTL ( 'abcde ', 2, '-', SHORT )
      !>        CALL SHIFTL ( 'abcde ', 2, '-', LONG  )
      !>
      !>     yield the strings
      !>
      !>        SHORT = 'cde'
      !>        LONG  = 'cde ..     '
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
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDDO to END DO.
      !>
      !>-    SPICELIB Version 2.0.0, 01-SEP-1994 (MJS)
      !>
      !>        This version correctly handles negative shifts.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine shiftl ( in, nshift, fillc, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input string.
      integer(ip), intent(in) :: nshift  !! Number of times to shift.
      character(1), intent(in) :: fillc  !! Character to fill spaces left vacant.
      character(*), intent(out) :: out  !! Shifted string.

      integer(ip) :: inlen
      integer(ip) :: outlen

      integer(ip) :: n
      integer(ip) :: nsave
      integer(ip) :: nfill
      integer(ip) :: i
      integer(ip) :: s

!
!     Get the length of the input, output strings.
!
      inlen  = len ( in  )
      outlen = len ( out )

!
!     If the shift is zero or negative, the string is not changed.
!     If longer than the input string, the entire string is shifted.
!
      s = max ( nshift, 0 )
      n = min ( inlen,  s )

!
!     Figure out how many characters in the input string will
!     be saved (will not be shifted off the end of the string,
!     and will fit in the output string), and how many fill
!     characters will be needed (no more than NSHIFT, no fewer
!     than zero).
!
      nsave =     min ( inlen - n, outlen         )
      nfill = n - max ( 0,         inlen - outlen )

!
!     Move the saved characters to output.
!
      do i = 1, nsave
         out(i:i) = in(i+s:i+s)
      end do

!
!     Add as many fill characters as appropriate.
!
      do i = nsave+1, nsave+nfill
         out(i:i) = fillc
      end do

!
!     Pad the output string with blanks (to cover any previous
!     ugliness there).
!
      if ( outlen > inlen ) then
         out(inlen+1: ) = ' '
      end if

      return
      end subroutine shiftl

      !> Shift the contents of a character string to the right.
      !> Characters moved past the end of the input string are
      !> lost. Vacant spaces are filled with a specified character.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input character string.
      !>
      !>     NSHIFT   is the number of times the string is to be
      !>              shifted. If NSHIFT is negative, OUT will be
      !>              identical to IN.
      !>
      !>     FILLC    is the character with which spaces left vacant by
      !>              the shift are to be filled.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is the input string,
      !>              shifted N times, filled with FILLC.
      !>
      !>              OUT may overwrite IN.
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
      !>     As a string is shifted left or right, the leftmost or
      !>     rightmost characters of the string disappear (as if pushed
      !>     off the end of the string). This is .TRUE. regardless of
      !>     the length of the output string.
      !>
      !>     The remaining characters are shifted simultaneously, and
      !>     the spaces vacated by those characters are filled with a
      !>     replacement character.
      !>
      !>## Examples
      !>
      !>     If FILLC = ' '
      !>
      !>        'abcde'   shifted left twice becomes     'cde  '
      !>        'abcde'   shifted right once becomes     ' abcd'
      !>
      !>     If FILLC = '.'
      !>
      !>        '12345 '  shifted right once becomes     '.12345'
      !>        'Apple '  shifted left ten times becomes '......'
      !>
      !>     Given the declarations
      !>
      !>        CHARACTER*3         SHORT
      !>        CHARACTER*10        LONG
      !>
      !>     The calls
      !>
      !>        CALL SHIFTR ( 'abcde ', 2, '-', SHORT )
      !>        CALL SHIFTR ( 'abcde ', 2, '-', LONG  )
      !>
      !>     yield the strings
      !>
      !>        SHORT = '--a'
      !>        LONG  = '--abcd    '
      !>
      !>     while the calls
      !>
      !>        CALL SHIFTL ( 'abcde ', 2, '-', SHORT )
      !>        CALL SHIFTL ( 'abcde ', 2, '-', LONG  )
      !>
      !>     yield the strings
      !>
      !>        SHORT = 'cde'
      !>        LONG  = 'cde ..     '
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
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDDO to END DO.
      !>
      !>-    SPICELIB Version 2.0.0, 01-SEP-1994 (MJS)
      !>
      !>        This version correctly handles negative shifts.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine shiftr ( in, nshift, fillc, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input string.
      integer(ip), intent(in) :: nshift  !! Number of times to shift.
      character(1), intent(in) :: fillc  !! Character to fill spaces left vacant.
      character(*), intent(out) :: out  !! Shifted string.

      integer(ip) :: inlen
      integer(ip) :: outlen
      integer(ip) :: n
      integer(ip) :: nsave
      integer(ip) :: nfill
      integer(ip) :: i
      integer(ip) :: s

!
!     Get the length of the input, output strings.
!
      inlen  = len ( in  )
      outlen = len ( out )

!
!     If the shift is zero or negative, the string is not changed.
!     If longer than the input string, the entire string is shifted.
!
      s = max ( nshift, 0 )
      n = min ( inlen,  s )

!
!     Figure out how many characters in the input string will
!     be saved (will not be shifted off the end of the string,
!     and will fit in the output string), and how many fill
!     characters will be needed (no more than NSHIFT, no fewer
!     than zero).
!
      nsave = ( inlen - n ) - max ( 0, inlen - outlen )
      nfill =                 min ( n, outlen         )

!
!     Move the saved characters to output.
!
      do i = nsave, 1, -1
         out(i+s:i+s) = in(i:i)
      end do

!
!     Add as many fill characters as appropriate.
!
      do i = 1, nfill
         out(i:i) = fillc
      end do

!
!     Pad the output string with blanks (to cover any previous
!     ugliness there).
!
      if ( outlen > inlen ) then
         out(inlen+1: ) = ' '
      end if

      return
      end subroutine shiftr

      !> Retain only the significant digits in a numeric string.
      !>
      !>## Detailed Input
      !>
      !>     IN       is a numeric string.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the same numeric string with insignificant
      !>              zeros and spaces removed. The special case '.000...'
      !>              becomes just '0'. OUT may overwrite IN. If the
      !>              output string is too long, it is truncated on the
      !>              right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If IN is a non-numeric string, the contents of OUT are
      !>         unpredictable.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     There are only two interesting cases:
      !>
      !>        1) There is a decimal point and an exponent immediately
      !>           preceded by zero ('...0E', '...0D', '...0e', '...0d')
      !>           or by a space ('... E', '... D', '... e', '... d').
      !>
      !>        2) There is a decimal point and no exponent, and the last non-
      !>           blank character is a zero ('...0').
      !>
      !>     In each of these cases, go to the zero in question, and step
      !>     backwards until you find something other than a blank or a zero.
      !>
      !>     Finally, remove all leading spaces, and all occurrences of more
      !>     than one consecutive space within the string.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of SIGDGT.
      !>
      !>     '0.123456000000D-04'        becomes     '0.123456D-04'
      !>     '  -9.2100000000000'                    '-9.21'
      !>     '       13'                             '13'
      !>     '    00013'                             '00013'
      !>     ' .314 159 265 300 000 e1'              '.314 159 265 3e1'
      !>     '   123    45     6'                    '123 45 6'
      !>     '  .000000000'                          '0'
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB) (WLT)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.3.0, 21-MAR-1989 (WLT)
      !>
      !>         Previous fix was unbelievably bad, very buggy. This
      !>         has been fixed along with other bugs and non-standard
      !>         code has been removed.
      !>
      !>-     Beta Version 1.2.0, 28-FEB-1989 (WLT)
      !>
      !>         Reference to INSSUB replaced by SUFFIX
      !>
      !>-     Beta Version 1.1.1, 17-FEB-1989 (HAN) (NJB)
      !>
      !>         Declaration of the unused function ISRCHC removed.
      elemental subroutine sigdgt ( in, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input numeric string.
      character(*), intent(out) :: out  !! Numeric string, with insignificant digits removed.

      integer(ip) :: begin
      integer(ip) :: end
      character(1) :: lchar
      integer(ip) :: zero
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l

!
!     Find the first and last non-blank characters in the string.
!
      begin  = max ( 1, verify(in, ' ') )
      end    = max ( 1, len_trim(in) )
      lchar  = ' '
!
!     Trivial case.
!
      if ( begin == end ) then

         out(1:1) = in(begin:begin)

         if ( len(out) > 1 ) out(2:) = ' '

!
!     If there is no decimal point, all zeros are significant.
!
      else if ( index (in, '.') == 0 ) then

         l      = 1
         k      = begin

         do while (       ( l <= len(out) ) &
                    .and. ( k <= end      ) )

            out(l:l) = in(k:k)

!
!           Don't increment L if the last item copied was a space
!           (we don't want to copy extra spaces).
!
            if ( in(k:k) /= ' ' .or. lchar /= ' ' ) &
            l        = l+1
            lchar    = in(k:k)
            k        = k+1

         end do

         if ( l <= len(out) ) out(l:) = ' '

      else

!
!        Is there is a decimal point and an exponent immediately
!        preceded by zero ('...0E', '...0D', '...0e', '...0d') or
!        by a space ('... E', '... D', '... e', '... d')?
!
         zero = index ( in, '0E' )

         if ( zero == 0 )    zero = index ( in, '0D' )
         if ( zero == 0 )    zero = index ( in, '0e' )
         if ( zero == 0 )    zero = index ( in, '0d' )
         if ( zero == 0 )    zero = index ( in, ' E' )
         if ( zero == 0 )    zero = index ( in, ' D' )
         if ( zero == 0 )    zero = index ( in, ' e' )
         if ( zero == 0 )    zero = index ( in, ' d' )

!
!        Begin there, and move toward the front of the string until
!        something other than a blank or a zero is encountered. Then
!        remove the superfluous characters.
!
         if ( zero > 0 ) then

            j = zero + 1
            i = zero

            do while ( in(i:i) == '0'  .or. in(i:i) == ' ' )
               i = i - 1
            end do

            l = 1
            k = begin

            do while (       ( l <= len(out) ) &
                       .and. ( k <= i        ) )

               out(l:l) = in(k:k)

!
!              Don't increment L if the last item copied was a space.
!
               if ( in(k:k) /= ' ' .or. lchar /= ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            k = j

            do while (       ( l <= len(out) ) &
                       .and. ( k <= end      ) )

               out(l:l) = in(k:k)

!
!              Increment L only if we don't have two consecutive
!              spaces.
!
               if ( in(k:k) /= ' ' .or. lchar /= ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            if ( l <= len(out) ) out(l:) = ' '
!
!        Is there is a decimal point and no exponent, and is the last
!        non-blank character a zero ('...0')? Then truncate the string
!        after the last character that is neither a blank nor a zero.
!
         else if (       in(end:end)            == '0' &
                   .and. cpos ( in, 'EeDd', 1 ) ==  0 ) then

            i = end

            do while ( in(i:i) == '0' .or. in(i:i) == ' ' )
               i = i - 1
            end do

            l = 1
            k = begin

            do while (       ( l <= len(out) ) &
                       .and. ( k <= i        ) )

               out(l:l) = in(k:k)

!
!              Increment L only if we don't have two consecutive
!              spaces.
!
               if ( in(k:k) /= ' ' .or. lchar /= ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            if ( l <= len(out) ) out(l:) = ' '

         else

            l = 1
            k = begin

            do while (       ( l <= len(out) ) &
                       .and. ( k <= end      ) )

               out(l:l) = in(k:k)

!
!              Increment L only if we don't have two consecutive spaces.
!
               if ( in(k:k) /= ' ' .or. lchar /= ' ' ) &
               l        = l+1
               lchar    = in(k:k)
               k        = k+1

            end do

            if ( l <= len(out) ) out(l:) = ' '

         end if

      end if

!
!     Special case. The string '.0000....' reduces to '.' after the
!     zeros are removed.
!
      if ( out == '.' ) then
         out = '0'
      end if

      return
      end subroutine sigdgt

      !> Add a suffix to a character string.
      !>
      !>## Detailed Input
      !>
      !>     SUFF     is the suffix to be added to the string.
      !>              Leading blanks are significant. (A blank
      !>              suffix is interpreted as a null suffix.)
      !>
      !>     SPACES   is the number of spaces (blanks) in the output
      !>              string separating the last non-blank character
      !>              of the prefix from the first (blank or non-blank)
      !>              character of the suffix. Typically, this will be
      !>              zero or one. If not positive, SPACES defaults to
      !>              zero.
      !>
      !>     STRING   on input is the prefix to which the suffix is
      !>              to be added. Leading blanks are significant.
      !>              Trailing blanks are ignored.
      !>
      !>## Detailed Output
      !>
      !>     STRING   on output is the suffixed string. If STRING
      !>              is not large enough to contain the output string,
      !>              the output string is truncated on the right.
      !>
      !>              STRING may NOT overwrite SUFF.
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
      !>     The suffix is added to the right of the last non-blank character
      !>     of the prefix. (Any necessary truncation is done automatically.)
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of SUFFIX.
      !>
      !>           SUFF         STRING (input)   SPACES    STRING (output)
      !>           ----------   --------------   ------    ---------------
      !>           'abc     '   'def    '             0    'defabc '
      !>           'abc     '   'def    '             1    'def abc'
      !>           'abc     '   ' def   '             0    ' defabc'
      !>           'abc     '   ' def   '             1    ' def ab'
      !>           ' abc    '   'def    '             0    'def abc'
      !>           ' abc    '   'def    '             1    'def  ab'
      !>           ' abc    '   ' def   '            -1    ' def ab'
      !>           '        '   'def    '             0    'def    '
      !>           '        '   'def    '             1    'def    '
      !>           ' abc    '   '       '             0    ' abc   '
      !>           ' abc    '   '       '             1    '  abc  '
      !>
      !>## Restrictions
      !>
      !>     1)  SUFF and STRING must be distinct.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 18-MAR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      elemental subroutine suffix ( suff, spaces, string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: suff  !! Suffix.
      integer(ip), intent(in) :: spaces  !! Number of spaces separating prefix and suffix.
      character(*), intent(inout) :: string  !! Prefix on input, string on output.

      integer(ip) :: l
      integer(ip) :: slen
      integer(ip) :: end

!
!     SLEN is the allocated length of the string. L is the location of
!     the last non-blank character of the prefix.
!
      slen = len    ( string )
      l    = len_trim(string)

!
!     Put the suffix at the end of the string. The spaces will fill
!     themselves in.
!
      end = l + max ( spaces, 0 )

      if ( end < slen ) then
         string(end+1: ) = suff
      end if

      return
      end subroutine suffix

      !> Convert the characters in a string to uppercase.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input string.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is the input string
      !>              with all lowercase letters converted to uppercase.
      !>              Non-letters are not affected.
      !>
      !>              OUT may overwrite IN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the output string length is less than the input string
      !>         length, the result will be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Convert each lowercase character in IN to uppercase.
      !>
      !>## Examples
      !>
      !>     'This is an example'   becomes   'THIS IS AN EXAMPLE'
      !>     '12345 +-=? > * $ &'             '12345 +-=? > * $ &'
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
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 05-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
      !>
      !>        Removed the calls to the string lexicographic functions.
      !>
      !>        Modified the algorithm to use the ICHAR() intrinsic function
      !>        and some local integer storage for the bases of the lower and
      !>        upper case letters.
      !>
      !>        Added a "FIRST" clause to the code so that the lower and
      !>        upper case bases and the separation between them are only
      !>        initialized the first time the subroutine is called rather
      !>        than every time.
      !>
      !>        These changes were made to improve the execution speed of
      !>        the subroutine
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine ucase ( in, out )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: in  !! Input string.
      character(*), intent(out) :: out  !! Output string, all uppercase.

      integer(ip) :: i
      integer(ip) :: ich
!
!     Character codes used for the case conversion.
!
!
!     Move the string from IN to OUT. Step through OUT one character
!     at a time, translating letters between 'a' and 'z' to uppercase.
!
      out   = in

      do i = 1, len (out)

         ich = ichar( out(i:i) )

         if ( ( ich >= lowa ) .and. ( ich <= lowz ) ) then
            out(i:i) = char ( ich + shift )
         end if

      end do

      return
      end subroutine ucase

      !> Return the number of words in a string.
      !>
      !> The function returns the number of words in the input string
      !> STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input string to be parsed. It contains some number
      !>              of words, where a word is any string of consecutive
      !>              non-blank characters delimited by a blank or by either
      !>              end of the string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of words in the input character
      !>     string STRING.
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
      !>     WDCNT, like NTHWD and NEXTWD, is useful primarily for parsing
      !>     input commands consisting of one or more words, where a word is
      !>     defined to be any sequence of consecutive non-blank characters
      !>     delimited by either a blank or by either end of the string.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of WDCNT.
      !>
      !>           WDCNT ( 'Now is the time'  )   = 4
      !>           WDCNT ( '  for all  '      )   = 2
      !>           WDCNT ( 'good,men.to_come' )   = 1
      !>           WDCNT ( ' '                )   = 0
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
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 10-JAN-2005 (EDW)
      !>
      !>        Added logic to prevent the evaluation of STRING(LOC:LOC)
      !>        if LOC exceeds the length of string. Functionally, the
      !>        evaluation had no effect on WDCNT's output, but the NAG
      !>        F95 compiler flagged the evaluation as an array
      !>        overrun error. This occurred because given:
      !>
      !>            A .AND. B
      !>
      !>        NAG evaluates A then B then performs the logical
      !>        comparison.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental integer(ip) function wdcnt ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Input character string.

      integer(ip) :: n
      integer(ip) :: loc
      integer(ip) :: length
      logical :: cont

!
!     This is just NTHWD, except that it keeps looking until
!     it finds the last word.
!

!
!     Trivial case first.
!
      if ( string == ' ' ) then

         wdcnt = 0
         return

      else

         length = len ( string )

      end if

!
!     Skip leading blanks.
!
      loc = 1

      do while ( string(loc:loc) == ' ' )
         loc = loc + 1
      end do

!
!     Keep stepping through STRING, counting words as we go.
!     (The current word is ended whenever a blank is encountered.)
!     Quit when the end of the string is reached.
!
!     N is the number of words found so far.
!     LOC is the current location in STRING.
!
      n = 1

      do while ( loc < length )

         loc = loc + 1

!
!        Blank signals end of the current word.
!
         if ( string(loc:loc) == ' ' ) then

!
!           Skip ahead to the next word. Ensure no
!           evaluation of STRING(LOC:LOC) when
!           LOC = LENGTH+1.
!
            cont = loc <= length
            if( cont ) then
               cont = cont .and. string(loc:loc) == ' '
            end if

            do while ( cont )
               loc = loc + 1

               cont = loc <= length
               if( cont ) then
                  cont = cont .and. string(loc:loc) == ' '
               end if

            end do

!
!           If not at the end of the string, we have another word.
!
            if ( loc <= length ) then
               n = n + 1
            end if

         end if

      end do

!
!     Return the number of words found.
!
      wdcnt = n

      return
      end function wdcnt

      !> Find the index of a word within a string. If the word does not
      !> exist as a word within the string, the value zero is returned.
      !>
      !> The function returns the location of WORD within STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a string of characters, potentially containing the
      !>              word.
      !>
      !>              Leading and trailing blanks are not significant in
      !>              STRING.
      !>
      !>     WORD     is a string of consecutive printable characters.
      !>
      !>              Leading and trailing blanks are not significant in
      !>              WORD.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the location of WORD within STRING, providing
      !>     the index of the first letter of WORD within the input STRING. If
      !>     WORD does not exist or WORD is blank, the function returns zero.
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
      !>     A word within a string is a substring beginning and ending with a
      !>     non-blank characters that is delimited by blanks on each end. (A
      !>     blank is assumed to precede and follow the first and last
      !>     characters of a string.)
      !>
      !>     Given a word, this routine returns the index of the first letter
      !>     of the first word of STRING that matches the word.
      !>
      !>## Examples
      !>
      !>     STRING:
      !>                      1         2         3         4
      !>     WORD    1234567890123456789012345678901234567890123456    WDINDX
      !>     ------  ----------------------------------------------    ------
      !>     'POT'   'PUT THE POTATOES IN THE POT'                     25
      !>     'TOES'                                                    0
      !>     'PUT'                                                     1
      !>     'THE'                                                     5
      !>     'IN THE'                                                  18
      !>     'THE PO'                                                  0
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
      !>-    SPICELIB Version 1.1.0, 03-OCT-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function wdindx ( string, word )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String of characters, potentially containing words
      character(*), intent(in) :: word  !! A string of consecutive printable letters.

      integer(ip) :: i
      integer(ip) :: j

      integer(ip) :: begwd
      integer(ip) :: endwd

      integer(ip) :: begstr
      integer(ip) :: endstr

      integer(ip) :: strlen
      integer(ip) :: wdlen

      integer(ip) :: bgtond

!
!     Find the ends of the word and input string.
!
      endstr = len_trim(string)
      begstr = verify(string, ' ')

      endwd  = len_trim(word)
      begwd  = verify(word, ' ')

!
!     Get the offset from the beginning of the word to the end of the
!     word, the word length and the string length.
!
      bgtond =     endwd  - begwd
      wdlen  = 1 + bgtond
      strlen = 1 + endstr - begstr

!
!     We deal with all of the pathologies first...
!
      if (      ( endwd  < 1      ) &
           .or. ( strlen < wdlen  ) ) then

!
!        ... If we got a blank word or a string that is too short, then
!        the index of the word is zero.
!
         wdindx =  0
         return

      else if ( strlen == wdlen ) then

!
!        ... the word and string have the same non-blank length.
!        Either they match up or they don't.  Find out and return.
!
         if ( string(begstr:endstr) == word(begwd:endwd) ) then
            wdindx = begstr
         else
            wdindx = 0
         end if

         return
      end if

!
!     Ok.  Now we've got a realistic case to deal with.  The string
!     length is longer than the word length.  Check to see if we have a
!     match at the beginning of the string.
!
      i      = begstr
      j      = i + bgtond

      if (       ( string(i:j)     == word(begwd:endwd) ) &
           .and. ( string(j+1:j+1) == ' '               )  ) then

         wdindx = i
         return

      end if

!
!     No luck yet?  Search the string until we find a word match or
!     we run out of string to check.
!
      i      = begstr + 1
      j      = i      + bgtond

      do while (       ( j                    < endstr            ) &
                 .and. &
                 .not. (     (string(i  :j  ) == word(begwd:endwd) ) &
                        .and.(string(i-1:i-1) == ' '               ) &
                        .and.(string(j+1:j+1) == ' '               ) &
                       ) &
               )
         i   = i + 1
         j   = j + 1
      end do

!
!     If J equals ENDSTR then no match was found in the interior of the
!     string.  We make a last check at the end.
!
      if ( j == endstr ) then

         if (       ( string(i-1:i-1) == ' '               ) &
              .and. ( string(i  :j  ) == word(begwd:endwd) ) ) then
            wdindx = i
         else
            wdindx = 0
         end if

      else
!
!        The only way to get here is if we exited the above loop before
!        running out of room --- that is we had a word match.  Set
!        the index to the value of "I" that got us out of the loop.
!
         wdindx    = i

      end if

      return

      end function wdindx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Insert a substring into a character string at a specified
      !>     location.
      !>
      !>## Detailed Input
      !>
      !>     IN          is an input character string, into which a substring
      !>                 is to be inserted.
      !>
      !>     SUB         is the substring to be inserted. Leading and trailing
      !>                 blanks are significant.
      !>
      !>     LOC         is the position in the input string at which the
      !>                 substring is to be inserted. To append to the
      !>                 string, set LOC equal to LEN(IN) + 1.
      !>
      !>## Detailed Output
      !>
      !>     OUT         is the output string. This is equivalent to the
      !>                 string that would be created by the concatenation
      !>
      !>                    OUT = IN(1:LOC-1) // SUB // IN(LOC: )
      !>
      !>                 If the output string is too long, it is truncated
      !>                 on the right.
      !>
      !>                 OUT may overwrite IN.  OUT may NOT overwrite SUB.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error Free.
      !>
      !>     1) If LOC is less than 1 it is treated as having value 1.
      !>
      !>     2) If LOC is greater than LEN(IN) + 1, it is treated as if
      !>        it had value LEN(IN) + 1.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Shift the end of the input string, beginning with LOC, to the
      !>     right, leaving space for the substring. Then insert the substring
      !>     into the vacated space in the middle of the string. This has
      !>     the same effect as the concatenation
      !>
      !>        OUT = IN(1:LOC-1) // SUB // IN(LOC: )
      !>
      !>     Because this operation is not standard for strings of length (*),
      !>     this routine does not use concatenation.
      !>
      !>     This private routine is just a copy of the SPICE routine INSSUB
      !>     with "reasonable" choices made for out of bounds errors.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of ZZINSSUB.
      !>
      !>        IN                SUB      LOC    OUT
      !>        ----------------- -------  ---    ------------------------
      !>        'ABCDEFGHIJ'      ' YXZ '    3    'AB XYZ CDEFGHIJ'
      !>        'The rabbit'      'best '    5    'The best rabbit'
      !>        ' other woman'    'The'      1    'The other woman'
      !>        'An Apple a day'  ' keeps'  15    'An Apple a day keeps'
      !>        'Apple a day'     'An '      0    'An Apple a day'
      !>        'Apple a day'     'An '     -3    'An Apple a day'
      !>        'An Apple a day'  ' keeps'  16    'An Apple a day keeps'
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
      !>     H.A. Neilan     (JPL)
      !>     I.M. Underwood  (JPL)
      !>     W.L. Taber      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Reordered header sections. Corrected typos in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 12-AUG-1996 (WLT)
      !>
      !>        Adapted from the SPICELIB routine INSSUB to be error free.
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB)
      !>
      !>        Bug fix:  case where insertion location follows end of
      !>        input string is now handled correctly.  Formerly, an
      !>        out-of-range substring bound violation was incurred in this
      !>        case.
      !>
      !>        Bug fix:  use of SHIFTC routine in old implementation
      !>        resulted in output string being truncated at length
      !>        LEN(IN), which is not consistent with the routine's
      !>        specification.
      !>
      !>        Now does discovery check-in.  Header sections re-arranged.
      !>        Some clean-up of header format done.
      !>
      !>-    Beta Version 2.0.0, 4-JAN-1989 (HAN)
      !>
      !>        If the location at which the substring is to be inserted is
      !>        not in the interval [1, LEN(IN)+1], an error is signaled.
      !>        Locations not within that interval refer to non-existent
      !>        characters positions. (To append to the string, set the
      !>        location equal to LEN(IN)+1.)
      elemental subroutine zzinssub ( in, sub, loc, out )
      use pepper_kinds, only: ip
      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: sub  !! Substring to be inserted.
      integer(ip), intent(in) :: loc  !! Position at which substring is to be inserted.
      character(*), intent(out) :: out  !! Output string.

      character(1) :: chr

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: inlen
      integer(ip) :: myloc
      integer(ip) :: nmove
      integer(ip) :: outlen
      integer(ip) :: subend
      integer(ip) :: sublen
      integer(ip) :: to

      logical :: same

!
!     Note to the careful reader:  in order to scrupulously avoid
!     non-standard assignments of characters from a substring of IN to
!     an overlapping substring of OUT, in the case where IN and OUT
!     refer to the same memory, we'll test whether the output and
!     input strings are the same.  If they're the same, we can avoid
!     various assignments that could cause trouble if IN and OUT
!     actually refer to the same memory.  This test has little effect on
!     performance, and allows the author to sleep more soundly at night.
!
!     Capture the lengths of the input, output, and substitution
!     strings.
!
      inlen   =  len ( in  )
      outlen  =  len ( out )
      sublen  =  len ( sub )
      myloc   =  min ( inlen + 1, max ( 1, loc ) )

!
!     If the insertion occurs after the end of the output string,
!     just return the original string.  Don't do the assignment if
!     the output and input strings have equal values; the assignment
!     is not needed in this case and could cause a run-time error if
!     OUT and IN refer to the same memory.
!
      same  =  out == in

      if ( myloc > outlen ) then

         if ( .not. same ) then
            out = in
         end if

         return

      end if

!
!     At this point, we're guaranteed that
!
!        MYLOC  <  OUTLEN
!             -
!
!        MYLOC  <  INLEN + 1
!             -
!
!        MYLOC  >  0
!
!     The first part of the input string is copied without change
!     to the output string, if this first part is non-empty.
!
      if ( myloc > 1 ) then
!
!        Again, do the assignment only if it's required.
!
         if ( .not. same ) then
            out(1 : myloc-1) = in
         end if

      end if

!
!     The part following the new substring is shifted into place, if
!     there's both something to move and a place to put it.  Move the
!     rightmost characters first.
!
      subend  =  myloc - 1 + sublen

      if (        ( myloc  <= inlen  ) &
            .and. ( subend < outlen )  ) then

         nmove  =  min (  outlen-subend, inlen-myloc+1  )

         do i   =  nmove,  1,  -1

            from         =  myloc  + i - 1
            to           =  subend + i
            chr          =  in(from:from)
            out (to:to)  =  chr

         end do

      end if

!
!     And the new word is dropped into the middle.
!
      out( myloc : min(subend,outlen) )  =  sub

!
!     Blank-pad the output string if necessary.
!
      if ( outlen > inlen+sublen ) then
         out( inlen+sublen+1 : ) = ' '
      end if

      return
      end subroutine zzinssub

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Replace the substring (LEFT:RIGHT) with a string of any length.
      !>
      !>## Detailed Input
      !>
      !>     IN         is an arbitrary character string.
      !>
      !>     LEFT,
      !>     RIGHT      are the ends of the substring to be replaced.
      !>                Legitimate substrings satisfy the following
      !>                conditions
      !>
      !>                    RIGHT > LEFT - 2
      !>                    LEFT  > 1
      !>                    RIGHT < LEN(STRING) + 1
      !>
      !>                This allows users to refer to zero-length substrings
      !>                (null substrings) of IN.
      !>
      !>     STRING     is the replacement string. Essentially, the
      !>                substring (LEFT:RIGHT) is removed from the
      !>                input string, and STRING is inserted at the
      !>                point of removal.
      !>
      !>## Detailed Output
      !>
      !>     OUT        is the resulting string. OUT may overwrite IN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If RIGHT is one less than LEFT, the substring to
      !>        replace will be the null substring.  In this case,
      !>        STRING will be inserted between IN(:RIGHT) and IN(LEFT:).
      !>
      !>     2) If LEFT is smaller than one, it's treated as 1.
      !>
      !>     3) If RIGHT is greater than the length of the input string,
      !>        it is treated as being the length of the string.
      !>
      !>     4) If RIGHT is less than LEFT-1, no substitution is made.
      !>
      !>     5) Whenever the output string is too small to hold the result,
      !>        the result is truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Ideally, replacement could be done with simple concatenation,
      !>
      !>        OUT = IN(1:LEFT-1) // STRING // IN(RIGHT+1: )
      !>
      !>     but the Fortran 77 standard makes this illegal for strings of
      !>     unknown length.
      !>
      !>     This private routine is basically just a copy of the SPICE
      !>     routine REPSUB with all error handling removed and "reasonable"
      !>     interpretations used for exceptional cases.
      !>
      !>## Examples
      !>
      !>     A typical use for this routine might be to replace all
      !>     occurrences of one word in a string with another word.
      !>     For example, the following code fragment replaces every
      !>     occurrence of the word 'AND' with the word 'OR' in the
      !>     character string LINE.
      !>
      !>        LEFT = WDINDX ( LINE, 'AND' )
      !>
      !>        DO WHILE ( LEFT .NE. 0 )
      !>           CALL REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
      !>           LEFT = WDINDX ( LINE, 'AND' )
      !>        END DO
      !>
      !>     This routine can also be used to insert substring between
      !>     two characters.  Consider the string:
      !>
      !>         IN   = 'The defendant,, was found innocent.'
      !>
      !>     to insert ' Emelda Marcos' between the first and second commas
      !>     determine the location of the pair ',,'
      !>
      !>        RIGHT = POS ( IN, ',,', 1 )
      !>        LEFT  = RIGHT + 1
      !>
      !>     then
      !>
      !>        CALL REPSUB ( IN, LEFT, RIGHT, ' Emelda Marcos', OUT )
      !>
      !>     The output (OUT) will have the value:
      !>
      !>        'The defendant, Emelda Marcos, was found innocent.'
      !>
      !>## Restrictions
      !>
      !>     The memory used by STRING and OUT must be disjoint. The memory
      !>     used by IN and OUT must be identical or disjoint.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber     (JPL)
      !>     I.M. Underwood (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 27-APR-1996 (WLT)
      elemental subroutine zzrepsub ( in, left, right, string, out )
      use pepper_kinds, only: ip
      implicit none
      character(*), intent(in) :: in  !! Input string.
      integer(ip), intent(in) :: left  !! Ends of substring to be replaced.
      integer(ip), intent(in) :: right  !! Ends of substring to be replaced.
      character(*), intent(in) :: string  !! Replacement string.
      character(*), intent(out) :: out  !! Resulting string.

      integer(ip) :: inlen
      integer(ip) :: strlen
      integer(ip) :: outlen

      integer(ip) :: remain
      integer(ip) :: use         ( 3 )

      integer(ip) :: end
      integer(ip) :: next
      integer(ip) :: i
      integer(ip) :: myleft
      integer(ip) :: myrght

!
!     Get the lengths of all the strings involved in this transaction.
!
      inlen  = len ( in     )
      strlen = len ( string )
      outlen = len ( out    )
      myleft = min( inlen+1, max(1, left ) )
      myrght = min( inlen,   max(0, right) )
!
!     Reject bad inputs.
!
      if ( myleft < 1 ) then

         myleft = 1

      else if ( myrght > inlen ) then

         myrght = inlen

      else if ( right < left-1 ) then

         return

      end if

!
!     Consider three separate sections:
!
!        1) The front of the original string.
!
!        2) The replacement string.
!
!        3) The end of the original string.
!
!     Determine how much of each section to use in the output string.
!     REMAIN is the number of characters that will fit in the output
!     string.
!
      remain = outlen
      use(1) = min ( remain, myleft - 1 )

      remain = remain - use(1)
      use(2) = min ( remain, strlen )

      remain = remain - use(2)
      use(3) = min ( remain, inlen - right )

!
!     Move the third section first. It gets moved back to front
!     or front to back, depending on whether the replacement string
!     is longer than the original substring. The main thing is to
!     avoid overwriting characters that have yet to be moved.
!
      end = sum(use)

      if ( myleft + strlen > right ) then

         next = end

         do i = use(3), 1, -1
            out(next:next) = in(right+i:right+i)
            next           = next - 1
         end do

      else

         next = myleft + strlen

         do i = 1, use(3)
            out(next:next) = in(right+i:right+i)
            next           = next + 1
         end do

      end if

!
!     The first two sections can be moved directly to the front of
!     the output string.
!
      next = 1

      do i = 1, use(1)
         out(next:next) = in(i:i)
         next           = next + 1
      end do

      do i = 1, use(2)
         out(next:next) = string(i:i)
         next           = next + 1
      end do

!
!     Pad with blanks, if the output string was not filled.
!
      if ( end < outlen ) then
         out(end+1: ) = ' '
      end if

      return
      end subroutine zzrepsub

      !> Convert an integer to an equivalent character string.
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is the integer to be converted into a character string.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is the character string representing the integer NUMBER.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the output character string is not large enough to
      !>         contain the entire character string produced, the output
      !>         character string will be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine will convert a signed integer into an equivalent
      !>     decimal character string representation. The decimal digits of
      !>     the integer NUMBER are found by repeated applications of
      !>     "modulus" and division operations.
      !>
      !>## Examples
      !>
      !>     The following argument values illustrate the use of INTSTR.
      !>
      !>         NUMBER        STRING
      !>         ------------  ---------------------
      !>          1            '-1'
      !>         -1            '-1'
      !>          223          '223'
      !>         -32           '-32'
      !>          0            '0'
      !>          2147483647   '2147483647'   ( Maximum 32 bit integer )
      !>         -2147483647   '-2147483647'  ( Minimum 32 bit integer + 1 )
      !>         -2147483647   '-2147483648'  ( Minimum 32 bit integer )
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that all signed integers will fit into a
      !>         character string with LINLEN or fewer digits. See the
      !>         parameter LINLEN below for the current value.
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
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.1.0, 11-MAY-1993 (HAN) (MJS)
      !>
      !>        DATA statement came before the SAVE statement. This is
      !>        a violation of the ANSI Standard. It is now the other way
      !>        around.
      !>
      !>-    SPICELIB Version 2.0.0, 14-OCT-1992 (KRG)
      !>
      !>        The routine was rewritten to fix a bug concerning the minimum
      !>        representable integer.
      !>
      !>        This routine used to negate a negative number before it began
      !>        generating its digits. This was a bad thing to do, because on
      !>        many machines the minimum representable integer and the
      !>        maximum representable integer have the following relationship:
      !>
      !>           ABS( minimum integer ) = 1 + ABS( maximum integer ).
      !>
      !>        Changing the sign of a negative number before converting it
      !>        to a character string would cause a program to crash if it
      !>        were attempting to convert the minimum representable integer
      !>        into a character string.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 07-DEC-1990 (WLT)
      !>
      !>        References to the old name INT2CH were removed and
      !>        an exception added to that section.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
      elemental subroutine intstr ( number, string )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: number  !! Integer to be converted.
      character(*), intent(out) :: string  !! Equivalent character string, left justified.

      integer(ip),parameter :: base = 10
      integer(ip),parameter :: strlen = 80
      character(1), parameter :: digits(0:9) = &
         ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

      character(strlen) :: tmpstr
      integer(ip) :: tmpnum
      integer(ip) :: result
      integer(ip) :: remndr
      integer(ip) :: i

!
!     The digits are generated in reverse order, so we fill the
!     character string in reverse order, from `right' to `left',
!     so that the digits are in the correct order when we are
!     done converting the integer. This is to avoid reversing the
!     character string before returning. The output character
!     string is then left justified upon exit.
!
!     Make a copy of the input so that it will not be modified.
!
      tmpnum = number
!
!     Initialize the temporary character buffer used to store the
!     character string as it is generated to blanks.
!
      tmpstr = ' '
!
!     We need to do different things for the cases where the number to
!     be converted is positive, negative, or zero. ( Actually, the
!     positive case and the zero case are the same, but since we can
!     test for integer zero exactly it will save a few arithmetic
!     operations if we treat it as a special case. ) The case for a
!     negative number is the only one which truly might cause problems,
!     because ABS(minimum integer) may equal ABS(maximum integer) + 1.
!     For 32 bit numbers, INTMIN = -214748368 and INTMAX = 214748367.
!     You should be able to see the repercussions of this.
!
      i = len( tmpstr ) + 1
      if ( tmpnum < 0 ) then
!
!        Collect all of the digits in the string.
!
         do while ( tmpnum /= 0 )

            i      = i - 1
            result = tmpnum / base
            remndr = result * base - tmpnum
            tmpnum = result

            tmpstr (i:i) = digits(remndr)

         end do
!
!        Put the minus sign in place.
!
         i           = i - 1
         tmpstr(i:i) = '-'

      else if ( tmpnum > 0 ) then
!
!        Collect all of the digits in the string.
!
         do while ( tmpnum /= 0 )

            i      = i - 1
            result = tmpnum / base
            remndr = tmpnum - result * base
            tmpnum = result

            tmpstr (i:i) = digits(remndr)

         end do

      else
!
!        Treat zero as a special case, because it's easier.
!
         i           = i - 1
         tmpstr(i:i) = digits(0)

      end if
!
!     Set the value of the output string before returning. Let the
!     Fortran string equals deal with the left justification, and the
!     truncation on the right if the string STRING is not long enough
!     to contain all of the characters necessary.
!
      string = tmpstr(i:len(tmpstr))

      return
      end subroutine intstr

      !> Take a double precision number and convert it to
      !> an equivalent character string representation (base 10).
      !>
      !>## Detailed Input
      !>
      !>     X        is a double precision number.
      !>
      !>     SIGDIG   is the number of significant digits that are desired
      !>              for the output string.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is a character representation of X to the number of
      !>              significant digits specified by SIGDIG. The number of
      !>              spaces required to return the requested character
      !>              string is SIGDIG + 6. If STRING is not declared to
      !>              have adequate length, the number returned will be
      !>              truncated on the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If SIGDIG is less than one, this routine returns one
      !>         significant digit in the output string.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes an approximate character representation
      !>     of the input string X. The maximum number of significant
      !>     digits returned is 14. The representation returned will be
      !>     the same as that given by the FORTRAN write statement
      !>
      !>         WRITE ( STRING, FMT=(P1E23.xx)
      !>
      !>     where xx is a two digit number that represents MIN(14,SIGDIG).
      !>     The last decimal place is rounded. The output string is left
      !>     justified.
      !>
      !>     This routine has the advantage that it does not use an internal
      !>     file and is about 2.3 times as fast as an internal write. It can
      !>     be used as part of character function without fear of introducing
      !>     recursive I/O conflicts. It is intended to be an approximate
      !>     inverse to the subroutine NPARSD.
      !>
      !>     There is of course no formatting of the output string. All
      !>     outputs are written in scientific notation.
      !>
      !>     IF you want the character string representation of a double
      !>     precision number to be the same as that produced by a formatted
      !>     write statement use a FORTRAN write statement.
      !>
      !>     For example the number represented by the string
      !>
      !>           1.245454545454545454545E+01
      !>
      !>     when read (via a FORTRAN READ statement) into the DP variable X
      !>     and converted back to a character string having 14 significant
      !>     digits by this routine yields
      !>
      !>           1.2454545454545E+01
      !>
      !>     The FORTRAN write statement
      !>
      !>           WRITE ( 6, FMT='(P1E)' ) X
      !>
      !>     yields
      !>
      !>           1.2454545454545454E+01
      !>
      !>     If this is too much error for your application DO NOT use this
      !>     routine. You should be aware however, that a character string
      !>     read into a double precision number may not WRITE out with an
      !>     equivalent character representation as was input.
      !>
      !>     For example on a VAX 11/780 if you
      !>
      !>           READ  (5,*)         X
      !>           WRITE (6,FMT='(E)') X
      !>
      !>     and enter a value of 7.00000001 for the read statement
      !>     the output written will be 0.7000000010000001E+01
      !>
      !>## Examples
      !>
      !>     This routine is intended for use by routines that manipulate
      !>     character strings. For example, it may be desirable for a
      !>     routine to be able to take a character string input such as
      !>
      !>           12 miles
      !>
      !>     and convert it to the string
      !>
      !>           1.932E+02 km
      !>
      !>     or to simply
      !>
      !>           1.932E+02
      !>
      !>     The arithmetic is of course most easily handled using numeric
      !>     variables. However, it may be that a string is required for
      !>     subsequent processing of the input.  A SPICELIB routine NPARSD
      !>     exists that will take a character representation of a number
      !>     and convert it to a DOUBLE PRECISION number. The 12 above
      !>     can be converted to double precision using NPARSD,  the d.p.
      !>     number can then be multiplied by the 1.61... and the result
      !>     converted back to a string using this routine.
      !>
      !>     Suppose the following declarations are made
      !>
      !>           CHARACTER*(80)     TEXT
      !>           CHARACTER*(80)     NUMBER
      !>           CHARACTER*(80)     SCRATCH
      !>
      !>           DOUBLE PRECISION   X
      !>           INTEGER            I
      !>
      !>     and that TEXT contains the string '12 mi'.  Then the following
      !>     code would produce a character string  '1.932E+01 KM'
      !>
      !>           CALL NEXTWD (  TEXT,   NUMBER, SCRATCH   )
      !>           CALL NPARSD (  NUMBER, X,      ERROR,  I )
      !>
      !>           IF ( ERROR .EQ. ' ' ) THEN
      !>
      !>              X    = X * 1.61D0
      !>              CALL DPSTR ( X, 5, NUMBER )
      !>              TEXT = NUMBER(1:10) // 'KM'
      !>
      !>           ELSE
      !>              .
      !>              .
      !>              create an error message, try again, etc.
      !>              .
      !>              .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  The format of the string returned by this routine is used in
      !>         DPSTRF which is in the call tree to DPFMT. Changes to the
      !>         format of the output string may have unexpected consequences
      !>         for these SPICE routines. Please check those routines before
      !>         modifying this routine.
      !>
      !>     2)  The maximum number of significant digits returned is 14.
      !>
      !>     3)  If the output string is not declared to be adequately large
      !>         (at least SIGDIG + 6), the numeric string will be truncated to
      !>         the side opposite its justification.
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
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 09-SEP-1996 (WLT)
      !>
      !>        Added a reference to the header concerning the dependency
      !>        of the SPICE routines DPSTRF and DPFMT on the format of
      !>        the string produced by this routine.
      !>
      !>-    SPICELIB Version 1.1.0, 11-JUN-1992 (WLT)
      !>
      !>        A bug that caused this routine to have a floating point
      !>        overflow for values of X close to zero was corrected. In
      !>        addition the restriction on range of exponents supported
      !>        has been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
      !>
      !>## Revisions
      !>
      !>-     SPICELIB Version 1.1.0, 14-OCT-1992 (WLT)
      !>
      !>         A bug that caused this routine to have a floating point
      !>         overflow for values of X close to zero was corrected. In
      !>         addition the restriction on range of exponents supported
      !>         has been removed.
      !>
      !>-     Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
      !>
      !>         Header was changed to reflect the "error free" status
      !>         of the module, and a comment was added stating what the
      !>         routine does if SIGIDG is less than one.
      !>
      !>         Declaration of the unused variable FRAC removed.
      elemental subroutine dpstr ( x, sigdig, string )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: x  !! A double precision number
      integer(ip), intent(in) :: sigdig  !! The number of significant digits placed in output
      character(*), intent(out) :: string  !! A character string representation of X

!
!     Maximum number of allowed significant digits.
!
      integer(ip), parameter :: mostdg = 14

      integer(ip), parameter :: maxpow = 17

      real(wp), parameter :: power (0:maxpow) = &
         [ 1.0_wp, 1.0e1_wp, 1.0e2_wp, 1.0e3_wp, 1.0e4_wp, 1.0e5_wp, &
           1.0e6_wp, 1.0e7_wp, 1.0e8_wp, 1.0e9_wp, 1.0e10_wp, 1.0e11_wp, &
           1.0e12_wp, 1.0e13_wp, 1.0e14_wp, 1.0e15_wp, 1.0e16_wp, &
           1.0e17_wp ]
      real(wp), parameter :: ipower(0:maxpow) = &
         [ 1.0_wp, 1.0e-1_wp, 1.0e-2_wp, 1.0e-3_wp, 1.0e-4_wp, &
           1.0e-5_wp, 1.0e-6_wp, 1.0e-7_wp, 1.0e-8_wp, 1.0e-9_wp, &
           1.0e-10_wp, 1.0e-11_wp, 1.0e-12_wp, 1.0e-13_wp, 1.0e-14_wp, &
           1.0e-15_wp, 1.0e-16_wp, 1.0e-17_wp ]

      character(1), parameter :: digits(0:9) = &
         [ character(1) :: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
      real(wp), parameter :: values(0:9) = &
         [ 0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
           7.0_wp, 8.0_wp, 9.0_wp ]


      character(2), parameter :: vaxexp(0:40) = &
         [ character(2) :: '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', &
           '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', &
           '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', &
           '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', &
           '40' ]

      real(wp) :: copy
      integer(ip) :: postn
      integer(ip) :: expont
      integer(ip) :: maxsig

      real(wp) :: exp10

      integer(ip) :: i
      integer(ip) :: k
      integer(ip) :: last

      character(20) :: expc
      character(28) :: zero

      character(32) :: numstr

!
!     Transfer X to the local variable COPY and leave X alone for the
!     rest of the routine.
!
      copy   = x

!
!     Wipe out anything sitting in NUMSTR
!
      numstr = ' '

!
!     At least 1 significant digit is required.  The most allowed is 14.
!     MAXSIG is the integer in this range that is closest to SIGDIG.
!
      maxsig = min ( mostdg, max( 1, sigdig ) )

!
!     Examine COPY to see if its positive, zero, or negative.
!     This determines whether we need a minus sign and where the
!     decimal point needs to go in the output string.
!
      if      ( copy < 0 ) then

         numstr(1:1) = '-'
         copy        =  - copy
         postn       =  2
         numstr(3:3) = '.'

      else if ( copy > 0 ) then

         numstr(1:1) = ' '
         postn       =  2
         numstr(3:3) = '.'

      else
         zero        = ' 0.0000000000000000000000000'
         numstr      = zero(1:maxsig+2) // 'E+00'

         string = numstr
         return

      end if

!
!     We need a first guess at the exponent string.  Compute the LOG
!     base 10 of COPY
!
      exp10  = dlog10(copy)

!
!     Scale our copy of the input into the range 1 to 10.
!
      if ( exp10 < 0 ) then

!
!        In this case the exponent will be negative.  We want the
!        largest integer exponent less than EXP10,  but the FORTRAN
!        INT function gives the INTEGER closest to EXP10 between EXP10
!        and zero.  As a result we have to subtract 1 from INT(EXP10).
!
         expont =  int(exp10) - 1
         k      = -expont

         do while ( k > 16 )
            copy = copy * 1.0e16_wp
            k    = k - 16
         end do

         if ( k /= 0 ) then
            copy = copy * power(k)
         end if

      else

         expont = int ( exp10 )
         k      = expont

         do while ( k > 16 )
            copy = copy * 1.0e-16_wp
            k    = k - 16
         end do

         if ( k /= 0 ) then
            copy = copy * ipower(k)
         end if

      end if

!
!     Round off the last significant digit.
!
      copy   = (dnint( copy*power(maxsig-1)) + 0.125_wp ) &
             * ipower(maxsig-1)

!
!     We might have accidentally made copy as big as 10 by the
!     round off process.  If we did we need to divide by 10 and add 1
!     to the exponent value.  (COPY must always remain between 0 and 10)
!
      if ( copy >= 10.0_wp ) then
         copy   = copy * 1.0e-1_wp
         expont = expont + 1
      end if
!
!     Get the first digit of the decimal expansion of X.
!
      i                   = int ( copy )
      numstr(postn:postn) = digits(i)

      copy                = ( copy - values(i) ) * 10.0_wp

!
!     Set the string pointer to the next position and compute the
!     position of the last significant digit
!
      postn               = postn + 2
      last                = postn + maxsig - 1

!
!     Fetch digits until we fill in the last available slot for
!     significant digits.
!
      do while ( postn  < last )

         i                   = int ( copy )
         numstr(postn:postn) = digits(i)
         copy                = ( copy - values(i) ) * 10.0_wp
         postn               = postn + 1

      end do

!
!     Tack on the exponent to the output. Note that the rather odd
!     if, else if, else construction below is done to maintain backward
!     compatibility of the "look" of the output.
!
!     First get the exponent symbol and sign of the exponent.
!
      if   ( expont >= 0  ) then

         numstr(postn:) = 'E+'

      else

         expont         = - expont
         numstr(postn:) = 'E-'

      end if

      postn = postn + 2

!
!     Now get the numeric representation.
!
      if ( expont <= 40 ) then
         expc  = vaxexp ( expont )
      else
         call intstr ( expont, expc )
      end if

      numstr(postn:) = expc
      string         = numstr

!
!     That's all folks.
!
      return

      end subroutine dpstr

      !> Return .TRUE. if two given characters are equivalent when the
      !> case of the characters is ignored.
      !>
      !> The function returns .TRUE. if the characters are equivalent
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are two characters that are to be compared to see
      !>              if they are the same letter (although possibly
      !>              having different case such as 'a' and 'A')
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value .TRUE. if the two input characters
      !>     are the same or can be made the same by converting both to
      !>     upper or lower case.
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
      !>     This is a utility routine for comparing two characters to
      !>     see if they are the same when converted to upper case. It
      !>     is particularly useful when writing string analysis routines
      !>     that should be case insensitive. Instead of writing the
      !>     expression
      !>
      !>        A .EQ. B
      !>
      !>     use the expression
      !>
      !>        EQCHR ( A, B )
      !>
      !>     in all tests of equivalence for characters.
      !>
      !>## Examples
      !>
      !>     Suppose you want to determine whether or not two strings
      !>     are the same if differences in the case of letters are ignored.
      !>     The following code fragment shows how you can use this routine
      !>     to check for the equivalence of character strings.
      !>
      !>        MORE  = .TRUE.
      !>        SAME  = .TRUE.
      !>        L1    =  LEN(STR1)
      !>        L2    =  LEN(STR2)
      !>        CHECK = MIN ( L1, L2 )
      !>
      !>        DO WHILE ( SAME .AND. MORE )
      !>
      !>           SAME = EQCHR( STR1(I:I), STR2(I:I) )
      !>           I    = I + 1
      !>           MORE = I .LT. CHECK
      !>
      !>        END DO
      !>
      !>        IF ( .NOT. SAME ) THEN
      !>
      !>           There's nothing to do, we already know the strings
      !>           are not the same.
      !>
      !>        ELSE IF ( L1 .LT. L2 ) THEN
      !>
      !>           The only way the strings can be regarded as being equal
      !>           is if the extra unchecked characters in STR2 are all blank.
      !>
      !>           SAME = STR2(I:) .EQ. ' '
      !>
      !>        ELSE
      !>
      !>           Same test as previous one but with STR1 this time.
      !>
      !>           SAME = STR1(I:) .EQ. ' '
      !>
      !>        END IF
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 17-SEP-1998 (EDW)
      !>
      !>        Replace the UVALUE data statement with a loop to fill
      !>        UVALUE. The Absoft Mac compiler failed to compile the
      !>        data statement correctly, and so this function failed
      !>        to work properly in all situations on the Mac. The
      !>        corrects the problem and functions on all platforms.
      !>
      !>-    SPICELIB Version 1.0.0, 16-MAY-1995 (WLT)
      elemental logical function eqchr ( a, b )
      use pepper_kinds, only: ip
      implicit none

      character(1), intent(in) :: a  !! one of the characters to check
      character(1), intent(in) :: b  !! the other character to check

!
!     Range of characters
!
!     The array UVALUE contains the ICHAR values for the upper case
!     version of each character.
!
      integer(ip) :: i
      integer(ip) :: j
!
!     Uppercase character codes (SPICELIB filled the table on the first
!     call).
!
      integer(ip), parameter :: uvalue ( 0 : lstchr ) = &
         [ ( merge( i - ( iachar('a') - iachar('A') ), i, &
                    i >= iachar('a') .and. i <= iachar('z') ), i = 0, lstchr ) ]

!
!     The first time through the loop we set the upper case values
!     for each of the lower case letters.
!

      i     = ichar(a)
      j     = ichar(b)

      if ( i > lstchr .or. j > lstchr ) then
         eqchr = i == j
      else
         eqchr = uvalue(i) == uvalue(j)
      end if

      return
      end function eqchr

      !> Return .TRUE. if two given characters are not equivalent if the
      !> case of the characters is ignored.
      !>
      !> The function returns .TRUE. if the characters are not equivalent
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are two characters that are to be compared to see
      !>              if they are different letters. Letters that have
      !>              the same value when converted to uppercase are
      !>              considered to be equivalent.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value .FALSE. if the two input characters
      !>     are the same or can be made the same by converting both to
      !>     upper or lower case. Otherwise it returns .TRUE.
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
      !>     This routine simply determines the truth value of .NOT. EQCHR.
      !>     See the entry point EQCHR for a discussion of that function.
      !>
      !>## Examples
      !>
      !>     Suppose you want to determine whether or not two strings
      !>     are the same up to differences in case. The following
      !>     code fragment shows how you can use this routine to check
      !>     for the equivalence of character strings.
      !>
      !>        MORE  = .TRUE.
      !>        SAME  = .TRUE.
      !>        L1    =  LEN(STR1)
      !>        L2    =  LEN(STR2)
      !>        CHECK = MIN ( L1, L2 )
      !>
      !>        DO WHILE ( SAME .AND. MORE )
      !>
      !>           IF ( NECHR(STR1(I:I),STR2(I:I) ) THEN
      !>              SAME = .FALSE.
      !>           END IF
      !>
      !>           I    = I + 1
      !>           MORE = I .LT. CHECK
      !>
      !>        END DO
      !>
      !>        IF ( .NOT. SAME ) THEN
      !>
      !>           There's nothing to do, we already know the strings
      !>           are not the same.
      !>
      !>        ELSE IF ( L1 .LT. L2 ) THEN
      !>
      !>           The only way the strings can be regarded as being equal
      !>           is if the extra unchecked characters in STR2 are all blank.
      !>
      !>           SAME = STR2(I:) .EQ. ' '
      !>
      !>        ELSE
      !>
      !>           Same test as previous one but with STR1 this time.
      !>
      !>           SAME = STR1(I:) .EQ. ' '
      !>
      !>        END IF
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 17-SEP-1998 (EDW)
      !>
      !>        Replace the UVALUE data statement with a loop to fill
      !>        UVALUE. The Absoft Mac compiler failed to compile the
      !>        data statement correctly, and so this function failed
      !>        to work properly in all situations on the Mac. The
      !>        corrects the problem and functions on all platforms.
      !>
      !>-    SPICELIB Version 1.0.0, 16-MAY-1995 (WLT)
      elemental logical function nechr ( a, b )
      use pepper_kinds, only: ip
      implicit none
      character(1), intent(in) :: a  !! one of the characters to check
      character(1), intent(in) :: b  !! the other character to check
      integer(ip) :: i
      integer(ip) :: j
      integer(ip), parameter :: uvalue ( 0 : lstchr ) = &
         [ ( merge( i - ( iachar('a') - iachar('A') ), i, &
                    i >= iachar('a') .and. i <= iachar('z') ), i = 0, lstchr ) ]

      i = ichar(a)
      j = ichar(b)

      if ( i > lstchr .or. j > lstchr ) then
         nechr = i /= j
      else
         nechr = uvalue(i) /= uvalue(j)
      end if

      return

      end function nechr

      !> Determine if two characters from different strings are the
      !> same when the case of the characters is ignored.
      !>
      !> The function returns .TRUE. if the two characters are the
      !> same up to case.
      !>
      !>## Detailed Input
      !>
      !>     STR1     is a character string
      !>
      !>     L1       is the location (index) of a character in STR1
      !>
      !>     STR2     is a character string
      !>
      !>     L2       is the location (index) of a character in STR2
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the characters STR1(L1:L1) and
      !>     STR2(L2:L2) are the same when the case of the characters is
      !>     ignored.
      !>
      !>     If the characters are different or L1 or L2 is out of range the
      !>     function returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If either L1 or L2 is out of range the function returns
      !>         .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility function for determining whether or not
      !>     two characters in different strings are the same up to case.
      !>     This function is intended for situation in which you need
      !>     to search two strings for a match (or mismatch).
      !>
      !>## Examples
      !>
      !>     Often you need to scan through two string comparing
      !>     character by character until a case insensitive mismatch
      !>     occurs. The usual way to code this is
      !>
      !>        DO WHILE (        L1 .LE. LEN(STR1)
      !>                    .AND. L2 .LE. LEN(STR2)
      !>                    .AND. EQCHR( STR1(L1:L1),STR2(L2:L2) ) )
      !>
      !>           L1 = L1 + 1
      !>           L2 = L2 + 1
      !>
      !>        END DO
      !>
      !>        Check L1, L2 to make sure we are still in range, etc.
      !>
      !>     The problem with this loop is that even though the check to make
      !>     sure that L1 and L2 are in range is performed, FORTRAN may
      !>     go ahead and compute the equality condition even though one of the
      !>     first two steps failed. This can lead to out of range errors
      !>     and possible halting of your program depending upon how
      !>     the routine is compiled. An alternative way to code this is
      !>
      !>        IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
      !>           ALIKE = EQCHR( STR1(L1:L1),STR2(L2:L2) )
      !>        ELSE
      !>           ALIKE = .FALSE.
      !>        END IF
      !>
      !>        DO WHILE ( ALIKE )
      !>
      !>           L1 = L1 + 1
      !>           L2 = L2 + 1
      !>
      !>           IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN
      !>              ALIKE = EQCHR( STR1(L1:L1), STR2(L2:L2) )
      !>           ELSE
      !>              ALIKE = .FALSE.
      !>           END IF
      !>        END DO
      !>
      !>     However this is a much more complicated section of code. This
      !>     routine allows you to code the above loops as:
      !>
      !>        DO WHILE ( SAMCHI ( STR1,L1, STR2,L2 )  )
      !>           L1 = L1 + 1
      !>           L2 = L2 + 1
      !>        END DO
      !>
      !>     The boundary checks are automatically performed and out
      !>     of range errors are avoided.
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
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
      elemental logical function samchi ( str1, l1, str2, l2 )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str1  !! A character string
      integer(ip), intent(in) :: l1  !! The location (index) of a character in STR1
      character(*), intent(in) :: str2  !! A character string
      integer(ip), intent(in) :: l2  !! The location (index) of a character in STR2

      if (      l1 < 1 &
           .or. l2 < 1 &
           .or. l1 > len(str1) &
           .or. l2 > len(str2)  ) then

         samchi = .false.
         return
      end if

      samchi = eqchr( str1(l1:l1), str2(l2:l2) )
      return

      end function samchi

      !> Determine whether or not two substrings are the same up to
      !> case.
      !>
      !> The function returns .TRUE. if the substrings are identical
      !> up to case.
      !>
      !>## Detailed Input
      !>
      !>     STR1     is a character string
      !>
      !>     B1       are integers giving the beginning and ending of a
      !>     E1       substring in STR1
      !>
      !>     STR2     is a character string
      !>
      !>     B2       are integers giving the beginning and ending of a
      !>     E2       substring in STR2
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the two substrings STR(B1:E1) and
      !>     STR(B2:E2) have the same length and the same characters up to
      !>     case.
      !>
      !>     If any of the indices B1, E1, B2, E2 are out of range or out
      !>     of order the function returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If any of the B1, E1, B2, E2 are out of range or if an
      !>         ending substring index is before a beginning substring
      !>         index, the function returns false.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is a macro for comparing two substrings of
      !>     strings and handles all of the bounds checking to avoid
      !>     out of range errors with string indices.
      !>
      !>## Examples
      !>
      !>     Suppose a string contains a number of occurrences of some
      !>     particular substring in sequence and that you need to locate
      !>     the first character that is out of this sequence or the
      !>     end of the string.
      !>
      !>     If one ignores boundary constraints this can easily be
      !>     coded as shown here: We assume the particular substring is
      !>
      !>     '/beg'
      !>
      !>        B = 1
      !>        E = B + LEN('/beg' )
      !>
      !>        DO WHILE (       E           .LE. LEN(STR)
      !>                   .AND. STRING(B:E) .EQ. '/beg' )
      !>
      !>           B = B + LEN('/beg')
      !>           E = E + LEN('/beg')
      !>
      !>        END DO
      !>
      !>        IF ( B .LT. LEN(STR) ) THEN
      !>
      !>           we've found the start of a substring of interest
      !>
      !>        ELSE
      !>
      !>           there is no substring to find.
      !>
      !>        END IF
      !>
      !>     Unfortunately, you can't rely upon FORTRAN to check the boundary
      !>     condition: E .LE. LEN(STR) and skip the second test if the first
      !>     condition if false. As a result you can get an out of range
      !>     error.
      !>
      !>     Instead you could code:
      !>
      !>     B = 1
      !>     E = B + LEN('/beg')
      !>
      !>     IF ( E .LE. LEN(STR) ) THEN
      !>        ALIKE = STRINB(B:E) .EQ. '/beg'
      !>     ELSE
      !>        ALIKE = .FALSE.
      !>     END IF
      !>
      !>     DO WHILE ( ALIKE )
      !>
      !>           B = B + LEN('/beg')
      !>           E = E + LEN('/beg')
      !>
      !>        IF ( E .LE. LEN(STR) ) THEN
      !>           ALIKE = STRINB(B:E) .EQ. '/beg'
      !>        ELSE
      !>           ALIKE = .FALSE.
      !>        END IF
      !>
      !>     END DO
      !>
      !>     However, this is code is far more effort. Using this routine
      !>     you can make a much simpler block of code.
      !>
      !>     B = 1
      !>     E = B + LEN('/beg' )
      !>
      !>     DO WHILE ( SAMSBI(STR,B,E, '/beg',1,4 ) )
      !>
      !>        B = B + LEN('/beg')
      !>        E = E + LEN('/beg')
      !>
      !>     END DO
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT)
      elemental logical function samsbi ( str1, b1, e1,  str2, b2, e2 )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: str1  !! A string
      integer(ip), intent(in) :: b1  !! Beginning of a substring in STR1
      integer(ip), intent(in) :: e1  !! End of s substring in STR1
      character(*), intent(in) :: str2  !! A second string
      integer(ip), intent(in) :: b2  !! The beginning of a substring in STR2
      integer(ip), intent(in) :: e2  !! The end  of s substring in STR2

      integer(ip) :: i
      integer(ip) :: j

      if (      ( e1      < b1           ) &
           .or. ( e2      < b2           ) &
           .or. ( b1      < 1            ) &
           .or. ( b2      < 1            ) &
           .or. ( e1      > len(str1)    ) &
           .or. ( e2      > len(str2)    ) &
           .or. ( e1 - b1 /= e2 - b2      )  ) then

         samsbi = .false.
         return

      end if

      j      =  b2
      samsbi = .false.

      do i = b1, e1
         if ( nechr( str1(i:i), str2(j:j) ) ) return
         j = j + 1
      end do

      samsbi = .true.

      return
      end function samsbi

      !> Determine whether a string is matched by a template containing
      !> wild cards. This routine is case-insensitive.
      !>
      !> The function returns .TRUE. if STRING matches TEMPL and otherwise
      !> returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the input character string to be tested for a match
      !>              against the input template. Leading and trailing blanks
      !>              are ignored.
      !>
      !>     TEMPL    is the input template to be tested for a match against
      !>              the input string. TEMPL may contain wild cards. Leading
      !>              and trailing blanks are ignored.
      !>
      !>     WSTR     is the wild string token used in the input template. The
      !>              wild string token may represent from zero to any number
      !>              of characters.
      !>
      !>     WCHR     is the wild character token used in the input template.
      !>              The wild character token represents exactly one
      !>              character.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. when the input string matches the
      !>     input template, and .FALSE. otherwise. The string and template
      !>     match whenever the template can expand (through replacement of its
      !>     wild cards) to become the input string.
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
      !>     MATCHI ignores leading and trailing blanks in both the string
      !>     and the template. All of the following are equivalent (they
      !>     all return .TRUE.).
      !>
      !>        MATCHI ( 'ALCATRAZ',     'A*Z',      '*', '%' )
      !>        MATCHI ( '  ALCATRAZ  ', 'A*Z',      '*', '%' )
      !>        MATCHI ( 'ALCATRAZ',     '  A*Z  ',  '*', '%' )
      !>        MATCHI ( '  ALCATRAZ  ', '  A*Z  ',  '*', '%' )
      !>
      !>     MATCHI is case-insensitive:  uppercase characters match
      !>     lowercase characters, and vice versa. Wild characters match
      !>     characters of both cases.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>        STRING  = '  ABCDEFGHIJKLMNOPQRSTUVWXYZ  '
      !>        WSTR    = '*'
      !>        WCHR    = '%'
      !>
      !>     Then
      !>
      !>        if TEMPL is  '*A*'        MATCHI is   T
      !>                     'A%D*'                     F
      !>                     'A%C*'                   T
      !>                     '%A*'                      F
      !>                     '%%CD*Z'                 T
      !>                     '%%CD'                     F
      !>                     'A*MN*Y*Z'               T
      !>                     'A*MN*Y*%Z'                F
      !>                     '*BCD*Z*'                T
      !>                     '*bdc*z*'                  F
      !>                     ' *bcD*Z*  '             T
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.3.1, 11-NOV-2005 (NJB)
      !>
      !>        Corrected example calls in header; made other minor
      !>        edits to header.
      !>
      !>-    SPICELIB Version 1.1.0, 08-JUN-1999 (WLT)
      !>
      !>         Fixed comments in detailed output and example sections.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      elemental logical function  matchi ( string, templ, wstr, wchr )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String to be tested.
      character(*), intent(in) :: templ  !! Template (with wild cards) to test against STRING.
      character(1), intent(in) :: wstr  !! Wild string token.
      character(1), intent(in) :: wchr  !! Wild character token.

      integer(ip) :: sfirst
      integer(ip) :: tfirst
      integer(ip) :: slast
      integer(ip) :: tlast

      integer(ip) :: slen
      integer(ip) :: tlen

      integer(ip) :: scur
      integer(ip) :: tcur

      integer(ip) :: left
      integer(ip) :: right

      integer(ip) :: i
      integer(ip) :: j

      logical :: nosubm

!
!     Give the function an initial value of .FALSE.
!
      matchi = .false.

!
!     First let's get everybody's measurements.
!
      sfirst = verify(string, ' ')
      slast  = len_trim(string)
      tfirst = verify(templ, ' ')
      tlast  = len_trim(templ)

      tlen   = tlast - tfirst + 1
      slen   = slast - sfirst + 1

      scur   = max ( 1, sfirst )
      tcur   = tfirst

!
!     A blank template matches a blank string, and nothing else.
!
      if ( ( tlast == 0 ) .and. ( slast == 0 ) ) then

         matchi = .true.
         return

      else if ( tlast == 0 ) then

         matchi = .false.
         return

      end if

!
!     The beginning of the string and template must be identical
!     up to the first occurrence of a wild string.
!

      do while (       ( tcur             <= tlast ) &
                 .and. ( scur             <= slast ) &
                 .and. ( .not. samch( templ,tcur, wstr,1) ) )

         if ( nechr ( templ(tcur:tcur) ,    string(scur:scur) ) &
              .and. ( templ(tcur:tcur) /= wchr              ) ) then

            matchi = .false.
            return

         else

            tcur = tcur + 1
            scur = scur + 1

         end if

      end do

!
!     There are a three ways we could have finished the loop above
!     without hitting a wild string.
!
!     Case 1.  Both the string and template ran out of characters at
!     the same time without running into a wild string in the template.
!
      if (       ( tcur > tlast ) &
           .and. ( scur > slast ) ) then

         matchi = .true.
         return

      end if

!
!     Case 2. The template ran out of characters while there were still
!     characters remaining in the in the string.  No match.
!
      if (       ( tcur > tlast ) &
           .and. ( scur <= slast ) ) then

         matchi = .false.
         return

      end if

!
!     Case 3. The string ran out of characters while non-wild characters
!     remain in the template.
!
!     We have to check to see if any non-wild-string characters
!     remain.  If so, we DO NOT have a match.  On the other hand if
!     only wild string characters remain we DO have a match.
!
      if (       ( tcur <= tlast ) &
           .and. ( scur > slast ) ) then

         matchi = .true.

         do i = tcur, tlast
            matchi = matchi .and. ( templ(i:i) == wstr )
         end do

         return

      end if

!
!     OK. There is only one way that you can get to this point.
!     It must be the case that characters remain in both the template
!     (TCUR .LE. TLAST) and the string (SCUR .LE. SLAST).  Moreover,
!     to get out of the first loop you had to hit a wild string
!     character.  Find the first non-wild-string character in the
!     template. (If there isn't one, we have a match.)
!

      do while (       ( tcur             <= tlast ) &
                 .and. ( samch ( templ,tcur, wstr,1 )  ) )

         tcur = tcur + 1

      end do

      if ( tcur > tlast ) then

         matchi = .true.
         return

      end if

!
!     Still here? Ok. We have a non-wild-string character at TCUR. Call
!     this position left and look for the right end of the maximum
!     length substring of TEMPL (starting at left) that does not have
!     a wild string character.
!
      left = tcur

      do while (       ( tcur             <=   tlast   ) &
                 .and. ( .not. samch(templ,tcur, wstr,1) ) )

         tcur = tcur + 1

      end do

      right = tcur - 1

      do while ( left <= tlast )

!
!        First see if there is enough room left in the string to
!        match TEMPL(LEFT:RIGHT)
!
         if ( slast - scur < right - left ) then

            matchi = .false.
            return

         end if

!
!        The substring TEMPL(LEFT:RIGHT) might be the end of the
!        string.  In such a case the ends of STRING must match
!        exactly with the end of TEMPL.
!
         if ( right == tlast ) then

            i = slast
            j = tlast

            do while ( j >= left )

               if (      samch (templ,j, wchr,  1 ) &
                    .or. samchi(templ,j, string,i )  ) then

                  j = j - 1
                  i = i - 1

               else

                  matchi = .false.
                  return

               end if

            end do

!
!           If we made it through the loop, we've got a match.
!
            matchi = .true.
            return
         else

!
!           In this case TEMPL(LEFT:RIGHT) is in between wild string
!           characters.  Try to find a substring at or to the right
!           of SCUR in STRING that matches TEMPL(LEFT:RIGHT)
!
            nosubm = .true.

            do while ( nosubm )

               i = scur
               j = left

               do while (     ( j           <= right           ) &
                         .and.(      samchi( string,i, templ,j ) &
                                .or. samch ( wchr,  1, templ,j ) ) )

                  i = i + 1
                  j = j + 1

               end do

!
!              If J made it past RIGHT, we have a substring match
!
               if ( j > right ) then

                  scur   = i
                  nosubm = .false.

!
!              Otherwise, try the substring starting 1 to the right
!              of where our last try began.
!
               else

                  scur = scur + 1

!
!                 Make sure there's room to even attempt a match.
!
                  if ( slast - scur < right - left ) then

                     matchi = .false.
                     return

                  end if

               end if
            end do

         end if

!
!        If you have reached this point there must be something left
!        in the template and that something must begin with a wild
!        string character.  Hunt for the next substring that doesn't
!        contain a wild string character.
!
         do while (       ( tcur             <= tlast ) &
                    .and.   samch ( templ,tcur,  wstr,1) )

            tcur = tcur + 1

         end do

         if ( tcur > tlast ) then

!
!           All that was left was wild string characters.  We've
!           got a match.
!
            matchi = .true.
            return

         end if

!
!        Still here? Ok. We have a non-wild-string character at TCUR.
!        Call this position left and look for the right end of the
!        maximum length substring of TEMPL (starting at left) that
!        does not have a wild string character.
!
         left = tcur

         do while (       ( tcur             <= tlast ) &
                    .and. ( .not. samch( templ,tcur, wstr,1 ) ) )

            tcur = tcur + 1

         end do

         right = tcur - 1

      end do

      end function matchi

      !> Parse a character string that represents a number and return
      !> a double precision value.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string that represents a numeric value.
      !>              Commas and spaces may be used in this string for
      !>              ease of reading and writing the number. They
      !>              are treated as insignificant but non-error-producing
      !>              characters.
      !>
      !>              For exponential representation the characters
      !>              'E','D','e','d' may be used.
      !>
      !>              The following are legitimate numeric expressions
      !>
      !>               +12.2 e-1
      !>               -3. 1415 9276
      !>               1e12
      !>               E10
      !>
      !>              The program also recognizes the following  mnemonics
      !>              'PI', 'pi', 'Pi', 'pI'
      !>              '+PI', '+pi', '+Pi', '+pI'
      !>              '-PI', '-pi', '-Pi', '-pI'
      !>              and returns the value
      !>              ( + OR - ) 3.1415 9265 3589 7932 3846 2600 D0 as
      !>              appropriate.
      !>
      !>## Detailed Output
      !>
      !>     X        double precision parsed value of input string. If an
      !>              error is encountered, X is not changed.
      !>
      !>     ERROR    is a message indicating that the string could
      !>              not be parsed due to use of an unexpected or misplaced
      !>              character or due to a string representing a number
      !>              too large for double precision. If the number was
      !>              successfully parsed, ERROR will be returned as a blank.
      !>
      !>              In particular, blank strings, or strings that do not
      !>              contain either a digit or exponent character will
      !>              be regarded as errors.
      !>
      !>     PTR      this indicates which character was being used when
      !>              the error occurred. If no error occurs, PTR is
      !>              returned as 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the string is non-numeric, PTR indicates the location in
      !>         the string where the error occurred, and ERROR contains a
      !>         descriptive error message.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine parses an input character string that represents a
      !>     number, checks for overflow, unexpected or misplaced
      !>     characters. It returns the double precision number or an error
      !>     message.
      !>
      !>## Examples
      !>
      !>     Let   LINE = 'DELTA_T_A       =   32.184'
      !>
      !>     The following code fragment parses the line and obtains the
      !>     double precision value.
      !>
      !>        CALL NEXTWD ( LINE,  FIRST,  REST )
      !>        CALL NEXTWD ( REST, SECOND,  REST )
      !>        CALL NEXTWD ( REST,  THIRD,  REST )
      !>
      !>        CALL NPARSD (  THIRD,  VALUE, ERROR, PTR    )
      !>
      !>## Restrictions
      !>
      !>     1)  Due to rounding errors this routine may not be able to parse
      !>         the decimal character string representation of the largest
      !>         and smallest double precision numbers.
      !>
      !>     PEPPER: the value is read with Fortran's READ from the scanned
      !>     characters, so it is the double nearest the decimal number
      !>     (SPICELIB's arithmetic could be a few ulps off; see
      !>     spicelib_bugs.md). What is accepted, and the errors, are
      !>     SPICELIB's.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.6.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 3.5.0, 15-AUG-2002 (WLT)
      !>
      !>        Replaced the call to INSSUB with a call to ZZINSSUB so
      !>        that this routine can legitimately call itself Error Free
      !>
      !>-    SPICELIB Version 3.4.0, 03-DEC-2001 (NJB)
      !>
      !>        Added an extra check to make sure that ICHAR of any character
      !>        of the input string is positive.
      !>
      !>-    SPICELIB Version 3.3.0, 29-FEB-1996 (KRG)
      !>
      !>        The declaration for the SPICELIB function PI is now
      !>        preceded by an EXTERNAL statement declaring PI to be an
      !>        external function. This removes a conflict with any
      !>        compilers that have a PI intrinsic function.
      !>
      !>        Removed the error message and storage for the unexpected
      !>        comma error message. This variable was set but never used,
      !>        and according to the spec for this routine a comma is a valid
      !>        delimiter, treated like a space, within numbers.
      !>
      !>-    SPICELIB Version 3.2.0, 10-JAN-1995 (WLT)
      !>
      !>        Changed error strings from parameters to assignments to
      !>        compensate for shortcomings of the Absoft FORTRAN compiler
      !>        on the NeXT.
      !>
      !>-    SPICELIB Version 3.1.0, 12-JUL-1994 (WLT)
      !>
      !>        The previous version of the routine assumed that the range
      !>        of values of ICHAR was 0 to 128. That turns out not to be
      !>        true on some machines. If a character whose ICHAR value is
      !>        outside this range is detected, it is now handled properly
      !>        as an unexpected character.
      !>
      !>-    SPICELIB Version 3.0.0, 24-FEB-1993 (WLT)
      !>
      !>        The previous version of the algorithm interpreted P or p as 1.
      !>        This was not the intent of the routine and was corrected.
      !>
      !>-    SPICELIB Version 2.0.0, 28-AUG-1992 (WLT) (KRG)
      !>
      !>        The basic algorithm was completely re-written. As a result
      !>        the routine now runs an order of magnitude faster than
      !>        it did before. In addition, strings that do not contain
      !>        enough information to assign a value to the string are now
      !>        regarded as errors. These include blank strings or strings
      !>        that contain only a sign characters, blanks and commas.
      !>
      !>        In addition the error diagnosis and checking for overflow
      !>        was greatly enhanced.
      !>
      !>        Note: strings may now parse with slightly different values
      !>        from the previous version of NPARSD. The current
      !>        implementation is more accurate in converting strings to
      !>        double precision numbers.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 17-APR-1990 (WLT)
      !>
      !>        Bug fix. The subscript used to reference individual characters
      !>        of the input string could sometimes step out of bounds. This
      !>        went unnoticed until NAIF began compiling with the CHECK=BOUNDS
      !>        option of the DEC Fortran compiler.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
      elemental subroutine nparsd ( string, x, error, ptr )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, pi
      implicit none

      character(*), intent(in) :: string  !! Character string representing a numeric value.
      real(wp), intent(out) :: x  !! Double precision value parsed from STRING.
      character(*), intent(out) :: error  !! Message indicating whether errors have occurred.
      integer(ip), intent(out) :: ptr  !! Position in string where an error occurred.

      real(wp),parameter :: base = 10.0_wp
      integer(ip), parameter :: digit = 1
      integer(ip), parameter :: dpoint = 2
      integer(ip),parameter :: expont = 3
      integer(ip), parameter :: ignore = 4
      integer(ip), parameter :: bpistr = 5
      integer(ip), parameter :: epistr = 6
      integer(ip), parameter :: sign = 7
      integer(ip), parameter :: nknown = 128
      integer(ip), parameter :: test = 16
      integer(ip), parameter :: errsiz = 160

      character(errsiz) :: blnkst
      character(errsiz) :: toobig
      character(errsiz) :: unrcst
      character(errsiz) :: unxpch
      character(errsiz) :: unxppt
      character(errsiz) :: unxpsn

      real(wp) :: decval
      real(wp) :: divisr
      real(wp) :: dpsign ( 2 )
      real(wp) :: ecount
      real(wp) :: expval
      real(wp) :: factor
      real(wp) :: intbnd
      real(wp) :: intval
      real(wp) :: maxexp
      real(wp) :: minexp
      real(wp) :: next
      real(wp) :: value
      real(wp) :: values (     nknown )

      integer(ip) :: class  ( 0 : nknown )
      integer(ip) :: i
      integer(ip) :: id
      integer(ip) :: signdx

      integer(ip) :: b
      integer(ip) :: blank
      integer(ip) :: l
      integer(ip) :: m
      integer(ip) :: nl
      integer(ip) :: nexti
      integer(ip) :: thisi

      logical :: dodec
      logical :: expok
      logical :: doexp
      logical :: doint
      logical :: mantsa
      logical :: bpiok
      logical :: epiok
      logical :: pntok
      logical :: roundi
      logical :: roundd
      logical :: sigchr
      logical :: signok
      logical :: zeroi
      logical :: ispi
      character(len(string) + 2) :: buf
      integer(ip) :: nb
      integer(ip) :: epos
      integer :: ios

!
!     PEPPER: SPICELIB computed these tables and limits on the first call
!     only (saved); they are computed on every call now.
!
      values = 0.0_wp
      class  = 0

!
!        Set up the error messages
!
      toobig = 'The number represented by the input string is ' &
      //       'too large to be stored as a double precision ' &
      //       'number. '

      unxpch = 'An unexpected character was found while ' &
      //       'attempting to parse the input string. '

      unxppt = 'An unexpected decimal point was found in the ' &
      //       'input string. '

      unxpsn = 'An unexpected sign character was found in the ' &
      //       'input string. '

      blnkst = 'The input string is blank. Blank strings are ' &
      //       'not considered to be numbers. '

      unrcst = 'The input string could not be recognized as a ' &
      //       'number. '

      blank = ichar ( ' ' )

      values( ichar ( '0' ) ) =  0.0_wp
      values( ichar ( '1' ) ) =  1.0_wp
      values( ichar ( '2' ) ) =  2.0_wp
      values( ichar ( '3' ) ) =  3.0_wp
      values( ichar ( '4' ) ) =  4.0_wp
      values( ichar ( '5' ) ) =  5.0_wp
      values( ichar ( '6' ) ) =  6.0_wp
      values( ichar ( '7' ) ) =  7.0_wp
      values( ichar ( '8' ) ) =  8.0_wp
      values( ichar ( '9' ) ) =  9.0_wp
      values( ichar ( '-' ) ) = -1.0_wp
      values( ichar ( '+' ) ) =  1.0_wp

      class( ichar(' ') ) = ignore
      class( ichar(',') ) = ignore

      class( ichar('.') ) = dpoint

      class( ichar('E') ) = expont
      class( ichar('D') ) = expont
      class( ichar('e') ) = expont
      class( ichar('d') ) = expont

      class( ichar('+') ) = sign
      class( ichar('-') ) = sign

      class( ichar('1') ) = digit
      class( ichar('2') ) = digit
      class( ichar('3') ) = digit
      class( ichar('4') ) = digit
      class( ichar('5') ) = digit
      class( ichar('6') ) = digit
      class( ichar('7') ) = digit
      class( ichar('8') ) = digit
      class( ichar('9') ) = digit
      class( ichar('0') ) = digit

      class( ichar('p') ) = bpistr
      class( ichar('P') ) = bpistr
      class( ichar('i') ) = epistr
      class( ichar('I') ) = epistr
!
!        Finally create the numbers that will be used for checking
!        for floating point overflow.
!
!        NOTE: The value for MINEXP may be too small by one, but it
!              really doesn't make any difference, as you're going to
!              underflow anyway, and dividing zero by a number (BASE)
!              still gives you zero.
!
      maxexp = dint ( dlog10( dpmax ) )
      minexp =-( maxexp + 1 )

      intbnd = base
      next   = intbnd + 1.0_wp

      do while ( intbnd /= next )

         intbnd = intbnd *  base
         next   = intbnd +  1.0_wp

      end do

      intbnd = intbnd / base
!
!        That takes care of the first pass initializations.
!
!     Here's what's true right now.
!
!     There are no errors.
!     The error pointer doesn't need to point anywhere.
!     It's ok for the next token to be a decimal point.
!     It's ok for the next token to be a sign character.
!     It's ok for the next token to be an exponent marker.
!     It's ok for the next character to be the start of pi.
!
!     We expect to be constructing the integer part of the
!     numeric string.
!
      error     = ' '
      ptr       = 0

      pntok     = .true.
      signok    = .true.
      expok     = .true.
      bpiok     = .true.
      doint     = .true.
      roundd    = .true.
      roundi    = .true.
      ispi      = .false.
!
!     Here's some other facts.
!
!     We are not parsing the decimal part of the string.
!     We are not parsing the exponent part of the string.
!     We have not encountered any digits in the mantissa.
!     We have not encountered any significant characters.
!     It's not ok for the next character to be the end of pi (i).
!
      dodec     = .false.
      doexp     = .false.
      mantsa    = .false.
      sigchr    = .false.
      epiok     = .false.
!
!     So far there is no integer, decimal or exponent part to this
!     string.
!
      intval    = 0.0_wp
      decval    = 0.0_wp
      expval    = 0.0_wp
      divisr    = 1.0_wp
      factor    = 1.0_wp
      ecount    = 0.0_wp
!
!     Right now if we encounter a sign, it's part of the mantissa.
!     And until we know better the sign of both the mantissa and
!     exponent are +1 (as opposed to -1).
!
      signdx    = 1
      dpsign(1) = 1.0_wp
      dpsign(2) = 1.0_wp
!
!     Before doing anything else we determine whether or not
!     the input string is empty.
!
      if ( string == ' ' ) then

         error = blnkst
         ptr   = 1
         return

      end if

!
!     We need to find the last non-blank character of the input
!     string.  We shall use the idea of binary searching to locate
!     this character.  At first this may appear to be a bit convoluted
!     when compared to the obvious thing to do (start at the end of
!     the string and step backward until a non-blank character is
!     located).  However, on every machine we've looked at this method
!     locates the last non-blank character much more quickly on average
!     than the obvious method.
!
!     L and B denote the last and beginning characters
!     of the substring we are searching.  NL is the next to last
!     character that we are concerned with and M is the middle of
!     the current search interval ( from B to NL ).
!
      l     =  len(string)
      b     =  1
      nl    =  l - 1

!
!     We want M to be ( B + NL ) / 2   but right now that's L/2
!
      m     =  l / 2

      do while ( l-b > test )
!
!        What is true right now?  The string from L+1 on out
!        is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2;
!        and M >= B,  B is at least one and if greater than 1
!        there must be a non-blank character between B and the
!        end of the string.
!
         if ( ichar(string(l:l)) /= blank ) then

            b = l

         else if ( string(m:nl) == ' ' ) then

!
!           If you got here, the STRING(L:L) is a blank.
!           The string from L+1 on out is blank.
!           The string from M to NL (=L-1) is blank.  Thus the
!           string from M out is blank.
!
!           M is greater than or equal to B.
!           If M  is less than B + 2, then L will become
!           B or less and there will not be a
!           next pass through the loop.  That means that
!           we will never get to this point again and don't
!           have to worry about the reference STRING(M:NL)
!           giving us an access violation.
!
            l  = m - 1
!
!           With the new value of L, we now know that STRING(L+1:)
!           is blank.
!
         else
!
!           If you get to this point all of the string from
!           L out is blank and L is greater than M.
!           There is a non-blank character between M and NL.
!           If L should get within 16 of B, then the loop
!           will not be executed again.  That means again that
!           we don't have to worry about STRING(M:NL) being
!           an ill formed string.
!
            l  =  nl
            b  =  m
!
!           With the new value of L, we now know that STRING(L+1:)
!           is blank.
!
         end if

!
!        Finally compute NL,the index of the character that precedes
!        L and the new midpoint of the stuff from B to NL.
!
         nl =  l - 1
         m  = (b + nl) / 2

!
!        What's true now?  The string from L+1 on out is blank.
!
      end do

!
!     L is now within 16 characters of the last non-blank character
!     of the input string.  We simply search backward from L to
!     locate this last non-blank.
!
      do while ( ichar(string(l:l)) == blank )
         l = l - 1
      end do

!
!     Begin to collect the number in its various parts: an integer
!     portion, a fractional portion, and an exponent.
!
      do i = 1, l

         id    = ichar(string(i:i))

         if ( id > nknown .or. id < 0 ) then

!
!           This is definitely not expected.  Set the error message
!           and return.
!
            nexti = i + 1
            thisi = i

            call zzinssub ( string, ']', nexti, error )
            call zzinssub_inplace ( error,  '[', thisi )
            call prefix ( unxpch,  1,         error )

            ptr   = i
            return

!
!        The action taken depends upon the class of the token.
!
         else if ( class(id) == digit ) then
!
!           Once a digit has been encountered, we can no longer
!           allow the string 'PI' or a sign until an exponent
!           character is hit and resets the SIGNOK flag.
!
            bpiok  = .false.
            epiok  = .false.
            signok = .false.
            sigchr = .true.
!
!           If we are constructing the integer part ...
!
            if ( doint  ) then

               mantsa = .true.
!
!              Check the current value of the integer part to
!              make sure we don't overflow.
!
               if ( intval < intbnd ) then

                  intval = intval * base + values(id)

               else
!
!                 Once the integer exceeds a given bound,
!                 we add the rest on as fractional part and
!                 keep track of the factor we will need to
!                 multiply the decimal part by to scale things
!                 appropriately.  We also keep track of the number
!                 we will need to add to the exponent part.
!
                  ecount = ecount + 1
                  factor = factor / base

                  if ( roundi ) then

                     roundi = .false.

                     if ( values(id) > 0.5_wp*base ) then
                        intval = intval + 1.0_wp
                     end if

                  end if

               end if
!
!           ... or the decimal part ...
!
            else if ( dodec ) then

               mantsa = .true.
!
!              There are two cases to consider.  The case in which
!              the integer portion of the string has value 0...
!
               if ( zeroi ) then
!
!                 We can just keep accumulating the decimal part
!                 as an integer.  But we keep track of how many
!                 places past the decimal point the first non-zero
!                 digit occurs.  Note that once the decimal part
!                 exceeds the integer bound, we don't need to do
!                 anything.  The remaining digits cannot contribute
!                 to the value of the decimal part.
!
                  if  ( decval < intbnd ) then

                     decval = decval * base + values(id)
                     ecount = ecount - 1

                  else if ( roundd ) then

                     roundd = .false.

                     if ( values(id) >= 0.5_wp*base ) then
                        decval =  decval + 1.0_wp
                     end if

                  end if
!
!              ...and the case in which the integer portion is not
!              zero.
!
               else
!
!                 In this case, we know there is at least _something_
!                 to the integer part of this string.  We can
!                 stop accumulating the decimal part when the divisor
!                 portion exceeds the integer barrier.  After that
!                 the extra digits can't make any contribution to
!                 the double precision value given to the string.
!
                  if ( divisr < intbnd ) then

                     decval = decval * base + values(id)
                     divisr = divisr * base

                  end if

               end if
!
!           ...or the exponent part of the string.
!
            else if ( doexp  ) then

               if ( expval + ecount > maxexp ) then
!
!                 This number is too big to put into a double
!                 precision number. The marginal case where
!                 EXPVAL + ECOUNT .EQ. MAXEXP will be dealt
!                 with when the integer and fractional parts
!                 of the double precision number are built
!                 at the end of this routine.
!
                  error = toobig
                  ptr   = i
                  return

               else if ( expval + ecount < minexp ) then
!
!                 This number is going to underflow, we can
!                 just stop accumulating exponent. But we don't
!                 stop parsing the string yet. There might be
!                 a bad character lurking somewhere later in the
!                 string.
!
!                 NOTE: It is also possible to underflow when the
!                       value of EXPVAL + ECOUNT is equal to MINEXP,
!                       since an entire 'BASE' scale is not supported
!                       for this particular exponent.
!
               else
!
!                 This is the case we expect.  Just add on the
!                 next part of the exponent.
!
                  expval = expval*base + dpsign(2)*values(id)

               end if
!
!           Even though this character is a digit, its not expected
!           for some reason.  Set the error flag and return.
!
            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub_inplace ( error,  '[', thisi )
               call prefix ( unxpch,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == dpoint ) then

            if ( pntok ) then

               bpiok  = .false.
               epiok  = .false.
               pntok  = .false.
               signok = .false.
               dodec  = .true.
               doint  = .false.
               doexp  = .false.
               zeroi  = intval == 0.0_wp

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub_inplace ( error,  '[', thisi )
               call prefix ( unxppt,  1,         error )

               ptr    = i
               return

            end if

         else if ( class(id) == expont ) then

            sigchr = .true.

            if ( expok ) then

               bpiok  = .false.
               epiok  = .false.
               expok  = .false.
               pntok  = .false.
               dodec  = .false.
               doint  = .false.
               doexp  = .true.
               signok = .true.
               signdx =  2

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub_inplace ( error,  '[', thisi )
               call prefix ( unxpch,  1,         error )

               ptr   =  i
               return

            end if

         else if ( class(id) == sign ) then

            if ( signok ) then

               dpsign(signdx) = values(id)
               signok         = .false.

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub_inplace ( error,  '[', thisi )
               call prefix ( unxpsn,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == bpistr ) then

            sigchr = .true.

            if ( bpiok ) then

               doint  = .false.
               dodec  = .false.
               doexp  = .false.
               expok  = .false.
               pntok  = .false.
               bpiok  = .false.
               signok = .false.
               epiok  = .true.

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub_inplace ( error,  '[', thisi )
               call prefix ( unxpch,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == epistr ) then

            if ( epiok ) then

               doint  = .false.
               dodec  = .false.
               doexp  = .false.
               expok  = .false.
               pntok  = .false.
               bpiok  = .false.
               signok = .false.
               epiok  = .false.
               mantsa = .true.
               ispi   = .true.

               intval = pi

            else

               nexti = i + 1
               thisi = i

               call zzinssub ( string, ']', nexti, error )
               call zzinssub_inplace ( error,  '[', thisi )
               call prefix ( unxpch,  1,         error )

               ptr   = i
               return

            end if

         else if ( class(id) == ignore ) then

!
!           We don't do anything.
!

         else

!
!           This is definitely not expected.  Set the error message
!           and return.
!
            nexti = i + 1
            thisi = i

            call zzinssub ( string, ']', nexti, error )
            call zzinssub_inplace ( error,  '[', thisi )
            call prefix ( unxpch,  1,         error )

            ptr   = i
            return

         end if

      end do

!
!     If we got through the loop and it's OK to end PI, then we started
!     it but never finished.  This is an error.
!
      if ( epiok ) then
         error = unrcst
         ptr   = l
         return
      end if

!
!     PEPPER: the value is read with Fortran's READ, which is correctly
!     rounded (IEEE round to nearest), from the characters scanned above:
!     the blanks and commas left out, the exponent marker made 'E', a
!     mantissa of 1 when there is none ('E10'), an exponent of 0 when the
!     marker has no digits ('1E'). SPICELIB accumulated the digits and
!     scaled them by powers of ten in double precision, which can be a few
!     ulps from the nearest double (1.3271244004193938E+11 was 5 ulps low;
!     spicelib_bugs.md). The scan above still decides what is a number and
!     which error is reported.
!
      if ( .not. sigchr ) then
!
!        No significant characters in the input string (for example
!        '+  ,,.,,'): not a number.
!
         error = unrcst
         ptr   = len(string) + 1
         return
      end if

      if ( ispi ) then
         x = dpsign(1)*pi
         return
      end if

      buf  = ' '
      nb   = 0
      epos = 0
      do i = 1, l
         id = ichar(string(i:i))
         select case ( class(id) )
         case ( digit, sign )
            nb = nb + 1
            buf(nb:nb) = string(i:i)
         case ( dpoint )
!           (a point without digits, as in '-.E3', is part of the
!           implicit mantissa 1)
            if ( mantsa ) then
               nb = nb + 1
               buf(nb:nb) = '.'
            end if
         case ( expont )
            if ( .not. mantsa ) then
               nb = nb + 1
               buf(nb:nb) = '1'
            end if
            nb   = nb + 1
            buf(nb:nb) = 'E'
            epos = nb
         end select
      end do
      if ( epos > 0 ) then
         if ( verify ( buf(epos+1:nb), '+-' ) == 0 ) then
            nb = nb + 1
            buf(nb:nb) = '0'
         end if
      end if

!
!     PEPPER: the value, correctly rounded, by the library's one
!     conversion of decimal text to double precision (DECIMAL_VALUE), so
!     that a number gives the same double wherever it is read (text and
!     JSON kernels, time strings, ...).
!
      call decimal_value ( buf(1:nb), value, ios )
      if ( ios /= 0 ) then
         error = toobig
         ptr   = len(string) + 1
         return
      end if

      x = value

      return
      end subroutine nparsd

      !> The value of a decimal number ([+-]digits[.digits][E[+-]digits],
      !> the exponent letter E or D),
      !> correctly rounded, when it can be computed exactly without I/O
      !> (PEPPER; [[nparsd]]): Clinger's fast path. If the digits, without
      !> the decimal point, are an integer M of at most 2**53 and the power
      !> of ten is 10**K with |K| <= 22, both M and 10**|K| are doubles
      !> exactly, and one multiplication or division (IEEE arithmetic:
      !> correctly rounded) gives the correctly rounded value. EXACT is
      !> false (VALUE undefined) otherwise, or for text of another form.
      !> The exact path of [[decimal_value]].
      pure subroutine exact_decimal ( text, value, exact )
      use pepper_kinds, only: wp
      use, intrinsic :: iso_fortran_env, only: int64
      character(*), intent(in) :: text  !! the number
      real(wp), intent(out) :: value  !! its value, if EXACT
      logical, intent(out) :: exact  !! VALUE is the number, correctly rounded
      integer(int64), parameter :: max_exact = 2_int64**53  !! integers exact as doubles
      real(wp), parameter :: tens(0:22) = [ 1.0e0_wp, 1.0e1_wp, 1.0e2_wp, 1.0e3_wp, 1.0e4_wp, &
         1.0e5_wp, 1.0e6_wp, 1.0e7_wp, 1.0e8_wp, 1.0e9_wp, 1.0e10_wp, 1.0e11_wp, 1.0e12_wp, &
         1.0e13_wp, 1.0e14_wp, 1.0e15_wp, 1.0e16_wp, 1.0e17_wp, 1.0e18_wp, 1.0e19_wp, &
         1.0e20_wp, 1.0e21_wp, 1.0e22_wp ]  !! the powers of ten that are doubles exactly
      integer(int64) :: m
      integer :: i, k, n, d, scale, expo, esign
      logical :: point, digits, edigits
      real(wp) :: sgn
      exact = .false.
      value = 0.0_wp
      n = len(text)
      i = 1
      sgn = 1.0_wp
      if ( n == 0 ) return
      if ( text(1:1) == '+' .or. text(1:1) == '-' ) then
         if ( text(1:1) == '-' ) sgn = -1.0_wp
         i = 2
      end if
!
!     The digits: M, and the power of ten SCALE they are shifted by (the
!     digits after the point, less the trailing zeros dropped from M).
!
      m = 0
      scale = 0
      point = .false.
      digits = .false.
      do while ( i <= n )
         if ( text(i:i) == '.' ) then
            if ( point ) return
            point = .true.
         else if ( text(i:i) >= '0' .and. text(i:i) <= '9' ) then
            d = ichar(text(i:i)) - ichar('0')
            digits = .true.
            if ( m <= ( max_exact - d ) / 10 ) then
               m = 10*m + d
               if ( point ) scale = scale - 1
            else if ( d == 0 ) then
               if ( .not. point ) scale = scale + 1  ! a zero M cannot hold
            else
               return  ! more significant digits than a double holds exactly
            end if
         else
            exit
         end if
         i = i + 1
      end do
      if ( .not. digits ) return
!
!     The exponent.
!
      expo = 0
      if ( i <= n ) then
         if ( index ( 'EeDd', text(i:i) ) == 0 ) return
         i = i + 1
         esign = 1
         if ( i <= n ) then
            if ( text(i:i) == '+' .or. text(i:i) == '-' ) then
               if ( text(i:i) == '-' ) esign = -1
               i = i + 1
            end if
         end if
         edigits = .false.
         do while ( i <= n )
            if ( text(i:i) < '0' .or. text(i:i) > '9' ) return
            if ( expo > 9999 ) return  ! far out of range: READ decides
            expo = 10*expo + ( ichar(text(i:i)) - ichar('0') )
            edigits = .true.
            i = i + 1
         end do
         if ( .not. edigits ) return
         expo = esign*expo
      end if
      k = scale + expo
      if ( m == 0 ) then
         value = sgn*0.0_wp
         exact = .true.
      else if ( k >= 0 .and. k <= 22 ) then
         value = sgn*( real(m, wp)*tens(k) )
         exact = .true.
      else if ( k < 0 .and. k >= -22 ) then
         value = sgn*( real(m, wp)/tens(-k) )
         exact = .true.
      end if
      end subroutine exact_decimal

      !> The value of a decimal number, correctly rounded (PEPPER): the
      !> library's one conversion of decimal text to double precision, so a
      !> number gives the same double wherever it is read (text kernels and
      !> time strings through [[nparsd]], JSON kernels, DAF text transfer
      !> files). Exactly, without I/O, when [[exact_decimal]] can (most
      !> numbers: no lock, so threads do not wait on each other); otherwise
      !> with Fortran's READ (list-directed: any form READ accepts). An exact
      !> tie (a value halfway between two doubles) rounds to even on the
      !> exact path, as READ does with gfortran (ifx's READ rounds it away
      !> from zero; the Fortran standard leaves ties to the compiler).
      pure subroutine decimal_value ( text, value, iostat )
      use pepper_kinds, only: wp
      character(*), intent(in) :: text  !! the number
      real(wp), intent(out) :: value  !! its value (0 if IOSTAT is not 0)
      integer, intent(out) :: iostat  !! 0, or nonzero: not a number, or beyond the range of double precision
      logical :: exact
      call exact_decimal ( text, value, exact )
      iostat = 0
      if ( exact ) return
      read ( text, *, iostat = iostat ) value
      if ( iostat == 0 .and. abs(value) > huge(value) ) iostat = 1  ! gfortran: an infinity, no error
      if ( iostat /= 0 ) value = 0.0_wp
      end subroutine decimal_value

      !> Parse a character string that represents a number and return
      !> the FORTRAN-truncated integer value.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string that represents a numeric value.
      !>              Commas and spaces may be used in this string for
      !>              ease of reading and writing the number. They
      !>              are treated as insignificant but non-error-producing
      !>              characters.
      !>
      !>              For exponential representation and of the characters
      !>              'E','D','e','d' may be used.
      !>
      !>              The following are legitimate numeric expressions
      !>
      !>               +12.2 e-1
      !>               -3. 1415 9276
      !>               1e12
      !>               E10
      !>
      !>              The program also recognizes the following  mnemonics
      !>              'PI', 'pi', 'Pi', 'pI'
      !>              '+PI', '+pi', '+Pi', '+pI'
      !>              '-PI', '-pi', '-Pi', '-pI'
      !>              and returns the value ( + OR - ) 3 as appropriate.
      !>
      !>## Detailed Output
      !>
      !>     N        integer parsed value of input string  ( with
      !>              the implied limits on precision).  If an error is
      !>              encountered, N is not changed from whatever the
      !>              input value was. If the input string has a fractional
      !>              part, the fractional part will be truncated. Thus
      !>              3.18 is interpreted as 3.  -4.98 is interpreted as -4.
      !>
      !>     ERROR    this is a message indicating that the string could
      !>              not be parsed due to ambiguous use of symbols or
      !>              due to a string representing a number too large for
      !>              VAX double precision or integer variables. If no
      !>              error occurred, ERROR is blank.
      !>
      !>              In particular, blank strings, or strings that do not
      !>              contain either a digit or exponent character will
      !>              be regarded as errors.
      !>
      !>     PNTER    this indicates which character was being used when
      !>              the error occurred. If no error occurred, PNTER is 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the string is non-numeric, PNTER indicates the location in
      !>         the string where the error occurred, and ERROR contains a
      !>         descriptive error message.
      !>
      !>     2)  If the string is blank, ERROR is returned with a message
      !>         indicating the problem and PNTER will have a non-zero value.
      !>
      !>     3)  If the string represents a number that is outside the range of
      !>         representable integers, as defined by INTMIN() and INTMAX(),
      !>         ERROR is returned with a message and PNTER is set to the value
      !>         1, as the entire numeric string is at fault.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Basically, all this routine does is pass the input string to
      !>     NPARSD which does the parsing in double precision. If nothing
      !>     goes wrong in the double precision parsing of the number, the
      !>     returned value is checked to determine whether or not it will fit
      !>     into a VAX integer. If it doesn't, an error message is returned.
      !>
      !>## Examples
      !>
      !>     Let   LINE = 'DELTA_T_A       =   32'
      !>
      !>     The following code fragment parses the line and obtains the
      !>     integer value.
      !>
      !>        CALL NEXTWD ( LINE,  FIRST,  REST )
      !>        CALL NEXTWD ( REST, SECOND,  REST )
      !>        CALL NEXTWD ( REST,  THIRD,  REST )
      !>
      !>        CALL NPARSI (  THIRD,  VALUE, ERROR, POINTR )
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
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 2.1.0, 29-APR-1996 (KRG)
      !>
      !>        This subroutine was modified to return a non-zero value of
      !>        PNTER when the value returned by NPARSD is not a representable
      !>        integer, as defined by INTMIN() and INTMAX(). The value
      !>        returned is one (1), since the entire input string was not
      !>        correct.
      !>
      !>        The test for an error from NPARSD was also changed. It now
      !>        uses the integer PNTER returned from NPARSD rather then the
      !>        character string ERROR. This should pose no problems because
      !>        PNTER is non-zero if and only if there was an error and an
      !>        error message was assigned to ERROR.
      !>
      !>        Some extra, and unnecessary, assignments were deleted. The
      !>        assignments were:
      !>
      !>           X = DBLE ( N )
      !>
      !>           ERROR = ' '
      !>
      !>        which converted the input argument into a double before
      !>        calling NPARSD with X and initialized the error message
      !>        to be blank. NPARSD sets the value for X, ERROR, and PNTER
      !>        unless an error occurs, in which case X is not changed.
      !>        So, it is not necessary to initialize ERROR, PNTER, or X.
      !>
      !>        Finally, the values of INTMIN and INTMAX are only set on the
      !>        first call to the routine. They are now SAVEd.
      !>
      !>-    SPICELIB Version 2.0.0, 15-OCT-1992 (WLT)
      !>
      !>        The abstract of this routine was modified to reflect what
      !>        the routine actually does---truncate the value to an
      !>        integer.
      !>
      !>        In addition, a blank string is no longer considered to be
      !>        valid input.
      !>
      !>        Finally the instances of DFLOAT in the previous version were
      !>        replaced by the standard intrinsic function DBLE and the
      !>        function DINT was replaced by IDINT in one place to make types
      !>        match up on both sides of an assignment.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
      elemental subroutine nparsi (  string,  n, error, pnter)
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax, intmin
      implicit none

      character(*), intent(in) :: string  !! Character string representing a numeric value.
      integer(ip), intent(out) :: n  !! Translated integer value of STRING.
      character(*), intent(out) :: error  !! Message indicating what errors have occurred.
      integer(ip), intent(out) :: pnter  !! Position in character string where an error occurred.

      real(wp) :: x

!     Initial values
!
!     Constants (SPICELIB computed them on the first call).
!
      real(wp), parameter :: xmxint = real( intmax, wp )
      real(wp), parameter :: xmnint = real( intmin, wp )
!
!     If this is the first time NPARSI has been called, initialize
!     bounds for the range of integers.
!
!     NPARSD will define ERROR and PNTER if there is an error,
!     so we do not need to initialize them here.
!
      call nparsd ( string, x, error, pnter )

      if ( pnter == 0 ) then

         if (     ( dint(x) < xmnint ) &
             .or. ( dint(x) > xmxint ) ) then

            pnter = 1
            error = 'NPARSI: Value entered is beyond the bounds of '// &
                    'representable integers.'

         else

            n = idint(x)

         end if

      end if

      return
      end subroutine nparsi

      !> Scan a string from a specified starting position for the
      !> end of an unsigned integer.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is any character string.
      !>
      !>     FIRST    is the location in the string to beginning scanning
      !>              for an unsigned integer. It is assumed that the
      !>              unsigned integer begins at FIRST.
      !>
      !>## Detailed Output
      !>
      !>     LAST     is the last character at or after FIRST such that
      !>              the substring STRING(FIRST:LAST) is an unsigned
      !>              integer. If there is no such substring, LAST
      !>              will be returned with the value FIRST-1.
      !>
      !>     NCHAR    is the number of characters in the unsigned integer
      !>              that begins at FIRST and ends at last. If there
      !>              is no such string NCHAR will be given the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If FIRST is beyond either end of the string, then LAST will be
      !>         returned with the value FIRST-1 and NCHAR will be returned
      !>         with the value 0.
      !>
      !>     2)  If STRING(FIRST:FIRST) is not part of an unsigned integer then
      !>         LAST will be returned with the value FIRST-1 and NCHAR will be
      !>         returned with the value 0.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to scan forward in a string to locate
      !>     an unsigned integer that begins on the input character FIRST.
      !>
      !>## Examples
      !>
      !>     Suppose you believe that a string has the form
      !>
      !>        X%Y%Z
      !>
      !>     where X, Y, and Z are unsigned integers of some unknown
      !>     length and % stands for some non-digit character. You could
      !>     use this routine to locate the unsigned integers in the
      !>     string as shown below. We'll keep track of the beginning and
      !>     ending of the unsigned integers in the integer arrays B and E.
      !>
      !>     FIRST = 1
      !>     I     = 0
      !>
      !>     DO WHILE ( FIRST .LT. LEN(STRING) )
      !>
      !>        CALL LX4UNS ( STRING, FIRST, LAST, NCHAR )
      !>
      !>        IF ( NCHAR .GT. 0 ) THEN
      !>
      !>           I     = I    + 1
      !>           B(I)  = FIRST
      !>           E(I)  = LAST
      !>           FIRST = LAST + 2
      !>
      !>        ELSE
      !>
      !>           FIRST = FIRST + 1
      !>
      !>        END IF
      !>
      !>     END DO
      !>
      !>## Restrictions
      !>
      !>     1)  Assumes ICHAR returns values in the range [-128, 255].
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 04-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Fixed return value for LAST in $Exceptions section entry #1.
      !>
      !>-    SPICELIB Version 1.1.0, 03-DEC-2001 (NJB)
      !>
      !>        Updated to work if non-printing characters are present in
      !>        the input string. Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT)
      elemental subroutine lx4uns ( string, first, last, nchar )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! any character string
      integer(ip), intent(in) :: first  !! first character to scan from in STRING
      integer(ip), intent(out) :: last  !! last character that is part of an unsigned integer
      integer(ip), intent(out) :: nchar  !! number of characters in the unsigned integer.

      integer(ip), parameter :: maxc = 255

      integer(ip), parameter :: minc = -128

      integer(ip) :: i
      integer(ip) :: l
!
!     The digits (SPICELIB filled this table on the first call).
!
      logical, parameter :: digit ( minc : maxc ) = &
         [ ( i >= iachar('0') .and. i <= iachar('9'), i = minc, maxc ) ]

!
!     First we perform some initializations that are needed on
!     each pass through this routine.
!

      last  = first - 1
      l     = len ( string )

!
!     If start is beyond the ends of the string, we  can quit now.
!
      if ( first < 1 .or. first > l ) then
         nchar = 0
         return
      end if

!
!     Now for the real work of the routine. Examine characters one
!     at a time...
!
      do i = first, l

!
!        If this character is a digit, move the LAST pointer one
!        further down on the string.  Otherwise set NCHAR and return.
!
         if ( digit(ichar(string(i:i))) ) then
            last  = last + 1
         else
            nchar = last + 1 - first
            return
         end if
      end do

      nchar = last + 1 - first

      return
      end subroutine lx4uns

!=======================================================================
!     In-place variants.
!
!     SPICELIB code often passes one variable as both the input and
!     the output string of a routine, e.g. CALL REPMC ( MSG, '#', X,
!     MSG ). Fortran does not allow that: a dummy argument that is
!     changed must not be reached through another dummy argument, and
!     the compiler may optimize on that assumption. The library calls
!     these variants instead; they have one INTENT(INOUT) string. The
!     results are the same as those of the original routines with the
!     output string equal to the input string.
!=======================================================================

      !> Uppercase a string in place: the in-place form of [[ucase]].
      elemental subroutine ucase_inplace ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String to convert to uppercase.


      integer(ip) :: i
      integer(ip) :: ich

      do i = 1, len (string)
         ich = ichar ( string(i:i) )
         if ( ( ich >= lowa ) .and. ( ich <= lowz ) ) then
            string(i:i) = char ( ich + shift )
         end if
      end do

      end subroutine ucase_inplace

      !> Lowercase a string in place: the in-place form of [[lcase]].
      elemental subroutine lcase_inplace ( string )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String to convert to lowercase.

      integer(ip), parameter :: shift = ichar ('a') - uppera

      integer(ip) :: i
      integer(ip) :: ich

      do i = 1, len (string)
         ich = ichar ( string(i:i) )
         if ( ( ich >= uppera ) .and. ( ich <= upperz ) ) then
            string(i:i) = char ( ich + shift )
         end if
      end do

      end subroutine lcase_inplace

      !> Replace every occurrence of a character in a string, in place:
      !> the in-place form of [[replch]].
      elemental subroutine replch_inplace ( string, old, new )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String in which to replace characters.
      character(1), intent(in) :: old  !! Character to be replaced.
      character(1), intent(in) :: new  !! Replacement character.

      integer(ip) :: i

      do i = 1, len (string)
         if ( string(i:i) == old ) then
            string(i:i) = new
         end if
      end do

      end subroutine replch_inplace

      !> Compress runs of a delimiter in a string, in place: the in-place
      !> form of [[cmprss]].
      !>
      !> Runs of more than N consecutive occurrences of DELIM are reduced
      !> to N occurrences; the string is padded on the right with blanks.
      elemental subroutine cmprss_inplace ( delim, n, string )
      use pepper_kinds, only: ip
      implicit none

      character(1), intent(in) :: delim  !! Delimiter to be compressed.
      integer(ip), intent(in) :: n  !! Maximum consecutive occurrences of DELIM.
      character(*), intent(inout) :: string  !! String to compress.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: count

!
!     The output position J never passes the input position I, so the
!     characters can be moved left within the string as they are read.
!
      count = 0
      j     = 0

      do i = 1, len (string)

         if ( string(i:i) == delim ) then
            count = count + 1
            if ( count > n ) cycle
         else
            count = 0
         end if

         j = j + 1
         if ( j < i ) string(j:j) = string(i:i)

      end do

      if ( j < len (string) ) then
         string(j+1:) = ' '
      end if

      end subroutine cmprss_inplace

      !> Remove the characters in an ASCII range from a string, in place:
      !> the in-place form of [[astrip]].
      elemental subroutine astrip_inplace ( string, asciib, asciie )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String to strip.
      character(1), intent(in) :: asciib  !! First ASCII character in range to be stripped.
      character(1), intent(in) :: asciie  !! Last ASCII character in range to be stripped.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lwrbnd
      integer(ip) :: uprbnd

      lwrbnd = ichar ( asciib )
      uprbnd = ichar ( asciie )

!
!     The output position J never passes the input position I.
!
      j = 0

      do i = 1, len_trim (string)
         k = ichar ( string(i:i) )
         if ( ( k < lwrbnd ) .or. ( k > uprbnd ) ) then
            j = j + 1
            if ( j < i ) string(j:j) = string(i:i)
         end if
      end do

      if ( j < len (string) ) then
         string(j+1:) = ' '
      end if

      end subroutine astrip_inplace

      !> Remove the next word from a string: the form of [[nextwd]] whose
      !> REST is the input string.
      !>
      !> NEXT receives the first word of STRING (blank if STRING is
      !> blank), and STRING is replaced by the remainder, left-justified.
      elemental subroutine nextwd_inplace ( string, next )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! On input, the string; on output, the remaining part, left-justified.
      character(*), intent(out) :: next  !! The next word in the string.

      integer(ip) :: begin
      integer(ip) :: last

      begin = verify ( string, ' ' )

      if ( begin == 0 ) then
         next = ' '
         return
      end if

      last = scan ( string(begin:), ' ' )

      if ( last == 0 ) then
         next   = string(begin:)
         string = ' '
      else
         last   = begin + last - 2
         next   = string(begin:last)
         string = adjustl ( string(last+1:) )
      end if

      end subroutine nextwd_inplace

      !> Insert a substring into a string at a given position, in place:
      !> the in-place form of [[zzinssub]].
      !>
      !> Characters shifted past the end of STRING are lost. If LOC is
      !> beyond the end of STRING, STRING is unchanged.
      elemental subroutine zzinssub_inplace ( string, sub, loc )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String into which SUB is inserted.
      character(*), intent(in) :: sub  !! Substring to be inserted.
      integer(ip), intent(in) :: loc  !! Position at which SUB is to be inserted.

      integer(ip) :: myloc
      integer(ip) :: subend

      myloc = max ( 1, loc )

      if ( myloc > len (string) ) return

      subend = myloc - 1 + len (sub)

!
!     Shift the tail right (Fortran evaluates the right side of a
!     character assignment before storing, so the overlap is allowed).
!
      if ( subend < len (string) ) then
         string(subend+1:) = string(myloc:)
      end if

      string( myloc : min(subend,len(string)) ) = sub

      end subroutine zzinssub_inplace

      !> Replace the substring (LEFT:RIGHT) of a string with another
      !> string, in place: the in-place form of [[zzrepsub]].
      !>
      !> LEFT is limited to the range 1 to LEN(STRING)+1. If RIGHT is
      !> less than LEFT-1, STRING is unchanged. Characters shifted past
      !> the end of STRING are lost; vacated characters become blanks.
      elemental subroutine zzrepsub_inplace ( string, left, right, sub )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String in which the substring is replaced.
      integer(ip), intent(in) :: left  !! Start of the substring to be replaced.
      integer(ip), intent(in) :: right  !! End of the substring to be replaced.
      character(*), intent(in) :: sub  !! Replacement string.

      integer(ip) :: n
      integer(ip) :: myleft
      integer(ip) :: myrght
      integer(ip) :: nsub
      integer(ip) :: ntail
      integer(ip) :: last

      if ( right < left-1 ) return

      n      = len ( string )
      myleft = min ( n+1, max(1, left) )
      myrght = max ( myleft-1, right )

!
!     NSUB characters of SUB fit after the first MYLEFT-1 characters;
!     NTAIL characters of the tail (MYRGHT+1:) fit after them.
!
      nsub  = min ( n-myleft+1, len(sub) )
      ntail = max ( 0, min(n-myleft+1-nsub, n-myrght) )
      last  = myleft - 1 + nsub + ntail

!
!     Move the tail first (the overlap is allowed; see
!     [[zzinssub_inplace]]), then store the replacement.
!
      if ( ntail > 0 ) then
         string(myleft+nsub : last) = string(myrght+1 : myrght+ntail)
      end if

      string(myleft : myleft+nsub-1) = sub

      if ( last < n ) then
         string(last+1:) = ' '
      end if

      end subroutine zzrepsub_inplace

      !> Replace a marker in a string with a character string, in place:
      !> the in-place form of [[repmc]].
      !>
      !> The first occurrence of MARKER (ignoring its leading and trailing
      !> blanks) is replaced by VALUE without its leading and trailing
      !> blanks, or by a single blank if VALUE is blank.
      elemental subroutine repmc_inplace ( string, marker, value )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String in which the marker is replaced.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      character(*), intent(in) :: value  !! Replacement string.

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse

      call find_marker ( string, marker, mrkpsb, mrkpse )

      if ( mrkpsb == 0 ) return

      if ( value == ' ' ) then
         call zzrepsub_inplace ( string, mrkpsb, mrkpse, ' ' )
      else
         call zzrepsub_inplace ( string, mrkpsb, mrkpse, &
                                 value( verify(value, ' ') : len_trim(value) ) )
      end if

      end subroutine repmc_inplace

      !> Replace a marker in a string with an integer, in place: the
      !> in-place form of [[repmi]].
      elemental subroutine repmi_inplace ( string, marker, value )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: string  !! String in which the marker is replaced.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      integer(ip), intent(in) :: value  !! Replacement value.

      integer(ip), parameter :: maxli = 11  ! Maximum length of an integer.

      character(maxli) :: substr

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse

      call find_marker ( string, marker, mrkpsb, mrkpse )

      if ( mrkpsb == 0 ) return

      call intstr ( value, substr )

      call zzrepsub_inplace ( string, mrkpsb, mrkpse, substr(:len_trim(substr)) )

      end subroutine repmi_inplace

      !> Replace a marker in a string with a double precision number, in
      !> place: the in-place form of [[repmd]].
      elemental subroutine repmd_inplace ( string, marker, value, sigdig )
      use pepper_kinds, only: ip, wp
      implicit none

      character(*), intent(inout) :: string  !! String in which the marker is replaced.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      real(wp), intent(in) :: value  !! Replacement value.
      integer(ip), intent(in) :: sigdig  !! Significant digits in replacement text.

      integer(ip), parameter :: maxldp = 23  ! Maximum length of a DP number.

      character(maxldp) :: substr

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse
      integer(ip) :: subnbf
      integer(ip) :: subnbl

      call find_marker ( string, marker, mrkpsb, mrkpse )

      if ( mrkpsb == 0 ) return

      call dpstr ( value, sigdig, substr )

      subnbf = verify ( substr, ' ' )
      subnbl = len_trim ( substr )

      if ( subnbf /= 0 ) then
         call zzrepsub_inplace ( string, mrkpsb, mrkpse, substr(subnbf:subnbl) )
      end if

      end subroutine repmd_inplace

      !> Locate the first occurrence of a marker in a string, ignoring
      !> the marker's leading and trailing blanks (as [[repmc]] does).
      !>
      !> FIRST is 0 if MARKER is blank or does not occur in STRING.
      pure subroutine find_marker ( string, marker, first, last )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! String to search.
      character(*), intent(in) :: marker  !! Marker to find.
      integer(ip), intent(out) :: first  !! Index of the first character of the marker in STRING.
      integer(ip), intent(out) :: last  !! Index of the last character of the marker in STRING.

      integer(ip) :: mrknbf
      integer(ip) :: mrknbl

      first = 0
      last  = 0

      if ( marker == ' ' ) return

      mrknbf = verify ( marker, ' ' )
      mrknbl = len_trim ( marker )

      first = index ( string, marker(mrknbf:mrknbl) )

      if ( first /= 0 ) then
         last = first + mrknbl - mrknbf
      end if

      end subroutine find_marker

end module pepper_strings
