!> Time conversion: calendars and time scales ([[ttrans]], [[unitim]], [[etcal]], [[jul2gr]]), parsing ([[str2et]]), the defaults ([[timdef]]), number formatting, unit conversion, and the aberration correction strings.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## ZZVSTRNG (Virtual String)
!>
!> PEPPER: SPICELIB's umbrella routine ZZVSTRNG is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzvstrng. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines.  Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     Maintain a virtual decimal string associated with a d.p. number X.
!>
!>### Detailed Input
!>
!>     X          is a double precision number for which we want to
!>                create a virtual decimal string.  This is supplied
!>                to the routine ZZVSTSTR which sets up the internal
!>                representation of the virtual decimal string.
!>
!>                X is assumed to be positive.
!>
!>     FILL       is the character to use for digits that precede the
!>                first significant digit in the virtual decimal string.
!>                Usually this will be a blank or zero ('0')
!>
!>     FROM       is the index in the virtual decimal string of the
!>                first character that will be returned by ZZVSBSTR.
!>
!>     TO         is the index in the virtual decimal string of the
!>                last character that will be returned by ZZVSBSTR.
!>
!>     RND        is a logical flag used to indicate that the output
!>                string should represent the virtual decimal string
!>                that results from rounding to the TO'th decimal
!>                location.
!>
!>### Detailed Output
!>
!>     EXPONT     is the exponent associated with X when represented
!>                in scientific notation.  It is returned by ZZVSTSTR.
!>
!>     SUBSTR     is the substring of the virtual decimal string from
!>                index FROM to TO returned by ZZVSBSTR
!>
!>     DID        is a logical flag that is used to indicate that
!>                the left most character returned by ZZVSBSTR became
!>                a zero as a result of rounding up from 9. (i.e. there
!>                are significant digits to the left of the first
!>                character returned in SUBSTR.
!>
!>### Parameters
!>
!>     None.
!>
!>### Files
!>
!>     None.
!>
!>### Exceptions
!>
!>     Error free.
!>
!>### Particulars
!>
!>     Given a character representation of a number such as
!>     '1.234567890123E+3' there is a corresponding "infinite"
!>     representation.  In this case
!>
!>           ...0000001234.56789012300000....
!>
!>     If we let the "index" of the decimal point be zero and number
!>     the other characters from left to right in sequence we can
!>     speak of the J'th character in the infinite representation.
!>
!>     We call the combination of the infinite representation and
!>     indexing scheme  the virtual decimal string associated with the
!>     input string.
!>
!>     The internal representation of the virtual decimal string is
!>     set using the entry point ZZVSTSTR.  This entry point returns
!>     the exponent associated with the string when it is written
!>     in scientific notation.
!>
!>     For any J the entry point ZZVSBSTR returns the J'th character
!>     of the virtual decimal string.
!>
!>     You may request that ZZVSBSTR return a string that is rounded
!>     to the right most digit returned.  If return to the example
!>     above
!>
!>           ...0000001234.56789012300000....
!>
!>     and the substring from -5 to 3 is requested with rounding,
!>     the virtual decimal string will be treated as virtual string
!>     rounded to the 3rd decimal point.
!>
!>           ...0000001234.56800000000000....
!>
!>     As a special convenience, you may specify any character to
!>     be used in place of the extra leading zeros in the representation.
!>     This leading character is specified via the input FILL in
!>     ZZVSTSTR.
!>
!>### Examples
!>
!>     Suppose you would like to create an output string associated
!>     with X and you would like to present it in decimal format.
!>
!>     Moreover, suppose you know that X is positive and less than
!>     100000.  The following would create the string and set the
!>     leading character to be a blank.
!>
!>        CALL ZZVSTSTR ( X, ' ', EXP )
!>
!>     Check the exponent returned.  If it's greater than 5, our basic
!>     assumptions were violated.
!>
!>        IF ( EXP .GT. 5 ) THEN
!>           WRITE (*,*) 'The exponent is too big. It is: ', EXP
!>        END IF
!>
!>     Now fill in the string.
!>
!>        CALL ZZVSBSTR ( -6, 5, RND, SUBSTR, DID )
!>
!>        WRITE (*,*) 'The value of X was: ', SUBSTR
!>
!>### Restrictions
!>
!>     None.
!>
!>### Author and Institution
!>
!>     W.L. Taber      (JPL)
!>
!>### Literature References
!>
!>     None.
!>
!>### Version
!>
!>-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
!>
!>### Implementation notes
!>
!>     Although we don't anticipate ever needing these values
!>     we set some initial values for EXP and STRING.
!>
!>     This routine doesn't do anything.
submodule (pepper_instance_module) pepper_time

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: apr = 30  !! days in April (common year)
   integer(ip), parameter :: mar = 31  !! days in March (common year)
   integer(ip), parameter :: jan0 = 0  !! days in the year before January (common year)
   integer(ip), parameter :: jan = 31  !! days in January (common year)
   integer(ip), parameter :: feb0 = jan + jan0  !! days in the year before February (common year)
   integer(ip), parameter :: feb = 28  !! days in February (common year)
   integer(ip), parameter :: mar0 = feb + feb0  !! days in the year before March (common year)
   integer(ip), parameter :: apr0 = mar + mar0  !! days in the year before April (common year)
   integer(ip), parameter :: aprl0 = apr0 + 1  !! days in the year before April (leap year)
   integer(ip), parameter :: aug = 31  !! days in August (common year)
   integer(ip), parameter :: may = 31  !! days in May (common year)
   integer(ip), parameter :: may0 = apr + apr0  !! days in the year before May (common year)
   integer(ip), parameter :: jun0 = may + may0  !! days in the year before June (common year)
   integer(ip), parameter :: jun = 30  !! days in June (common year)
   integer(ip), parameter :: jul0 = jun + jun0  !! days in the year before July (common year)
   integer(ip), parameter :: jul = 31  !! days in July (common year)
   integer(ip), parameter :: aug0 = jul + jul0  !! days in the year before August (common year)
   integer(ip), parameter :: augl0 = aug0 + 1  !! days in the year before August (leap year)
   integer(ip), parameter :: sep0 = aug + aug0  !! days in the year before September (common year)
   integer(ip), parameter :: sep = 30  !! days in September (common year)
   integer(ip), parameter :: oct0 = sep + sep0  !! days in the year before October (common year)
   integer(ip), parameter :: oct = 31  !! days in October (common year)
   integer(ip), parameter :: nov0 = oct + oct0  !! days in the year before November (common year)
   integer(ip), parameter :: nov = 30  !! days in November (common year)
   integer(ip), parameter :: dec0 = nov + nov0  !! days in the year before December (common year)
   integer(ip), parameter :: decl0 = dec0 + 1  !! days in the year before December (leap year)
   integer(ip), parameter :: dp100y = 365*100 + 24  !! days in 100 years
   integer(ip), parameter :: dp1y = 365  !! days in a year
   integer(ip), parameter :: dp400y = 365*400 + 97  !! days in 400 years
   integer(ip), parameter :: dp4y = 365*4   + 1  !! days in 4 years
   integer(ip), parameter :: febl0 = feb0  !! days in the year before February (leap year)
   integer(ip), parameter :: janl0 = jan0  !! days in the year before January (leap year)
   integer(ip), parameter :: jull0 = jul0 + 1  !! days in the year before July (leap year)
   integer(ip), parameter :: junl0 = jun0 + 1  !! days in the year before June (leap year)
   integer(ip), parameter :: lnsize = 80  !! length of a line of text (messages, kernel lines)
   integer(ip), parameter :: marl0 = mar0 + 1  !! days in the year before March (leap year)
   integer(ip), parameter :: maxsig = 14  !! maximum number of significant digits
   integer(ip), parameter :: mayl0 = may0 + 1  !! days in the year before May (leap year)
   logical, parameter :: no = .false.  !! false
   integer(ip), parameter :: novl0 = nov0 + 1  !! days in the year before November (leap year)
   integer(ip), parameter :: octl0 = oct0 + 1  !! days in the year before October (leap year)
   integer(ip), parameter :: sepl0 = sep0 + 1  !! days in the year before September (leap year)

contains

      !> Determine if a kernel pool variable is present and if so
      !> that it has the correct size and type.
      !>
      !> The function returns .FALSE. if the kernel pool variable is OK.
      !>
      !>## Detailed Input
      !>
      !>     CALLER   is the name of the routine calling this routine
      !>              to check correctness of kernel pool variables.
      !>
      !>     NAME     is the name of a kernel pool variable that the
      !>              calling program expects to be present in the
      !>              kernel pool.
      !>
      !>     COMP     is the comparison operator to use when comparing
      !>              the number of components of the kernel pool variable
      !>              specified by NAME with the integer SIZE. If DIM is
      !>              is the actual size of the kernel pool variable then
      !>              BADKPV will check that the sentence
      !>
      !>                 DIM COMP SIZE
      !>
      !>              is a true statement. If it is not a true statement
      !>              an error will be signaled.
      !>
      !>              Allowed values for COMP and their meanings are:
      !>
      !>                 '='      DIM .EQ. SIZE
      !>                 '<'      DIM .LT. SIZE
      !>                 '>'      DIM .GT. SIZE
      !>                 '=>'     DIM .GE. SIZE
      !>                 '<='     DIM .LE. SIZE
      !>
      !>     SIZE     is an integer to compare with the actual
      !>              number of components of the kernel pool variable
      !>              specified by NAME.
      !>
      !>     DIVBY    is an integer that is one of the factors of the
      !>              actual dimension of the specified kernel pool variable.
      !>              In other words, it is expected that DIVBY evenly
      !>              divides the actual dimension of NAME. In those
      !>              cases in which the factors of the dimension of NAME
      !>              are not important, set DIVBY to 1 in the calling
      !>              program.
      !>
      !>     TYPE     is the expected type of the kernel pool variable.
      !>              Recognized values are
      !>
      !>                 'C' for character type
      !>                 'N' for numeric type (integer and double precision)
      !>
      !>              The case of TYPE is insignificant. If the value
      !>              of TYPE is not one of the 2 values given above
      !>              no check for the type of the variable will be
      !>              performed.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value .FALSE. if the kernel pool
      !>     variable has the expected properties. Otherwise the routine
      !>     signals an error and returns the value .TRUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the kernel pool variable specified by NAME is not present
      !>         in the kernel pool, the error SPICE(VARIABLENOTFOUND) is
      !>         signaled and the routine will return the value .TRUE.
      !>
      !>     2)  If the comparison operator specified by COMP is unrecognized,
      !>         the error SPICE(UNKNOWNCOMPARE) is signaled and the routine
      !>         will return the value .TRUE.
      !>
      !>     3)  If the expected type of the kernel pool variable TYPE is not
      !>         one of the supported types, the error SPICE(INVALIDTYPE) is
      !>         signaled and the routine will return the value .TRUE.
      !>
      !>     4)  If the comparison of the actual size of the kernel pool
      !>         variable with SIZE is not satisfied, the error
      !>         SPICE(BADVARIABLESIZE) is signaled and the routine will
      !>         return the value .TRUE.
      !>
      !>     5)  If the variable does not have the expected type, the error
      !>         SPICE(BADVARIABLETYPE) is signaled and the routine will
      !>         return the value .TRUE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine takes care of routine checking that often needs
      !>     to be done by programs and routines that rely upon kernel
      !>     pool variables being present and having the correct attributes.
      !>
      !>     It checks for the presence of the kernel pool variable and
      !>     examines the type and dimension of the variable to make sure
      !>     they conform to the requirements of the calling routine.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that you need to fetch a number of variables
      !>        from the kernel pool and want to check that the requested
      !>        items are in fact available prior to performing further
      !>        computations. The code example shows how you might use
      !>        this routine to handle the details of checking of
      !>        the various items.
      !>
      !>        Although by default the SPICE error handling system will
      !>        report the error and halt the execution of the program, in
      !>        this example we have decided to change this behavior to
      !>        display the error messages and continue the execution of
      !>        the program.
      !>
      !>        Use the kernel shown below to define some variables related
      !>        to the Earth.
      !>
      !>           KPL/PCK
      !>
      !>           File name: badkpv_ex1.tpc
      !>
      !>           The contents of this kernel are not intended for
      !>           real applications. Use only with this example.
      !>
      !>           \begindata
      !>
      !>              BODY_399_DATA  = ( 3.1416, 2.71828, 0.5, 12.0 )
      !>              BODY_399_NAMES = ( 'PI', 'E', 'HALF', 'DOZEN' )
      !>
      !>           \begintext
      !>
      !>           End of constants kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BADKPV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL               BADKPV
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         CALLER
      !>      PARAMETER           ( CALLER  = 'BADKPV_EX1' )
      !>
      !>      INTEGER               KWDLEN
      !>      PARAMETER           ( KWDLEN = 32 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*2           COMP
      !>      CHARACTER*(KWDLEN)    NAME
      !>      CHARACTER*1           TYPE
      !>
      !>      INTEGER               DIVBY
      !>      INTEGER               SIZE
      !>
      !>C
      !>C     Load the test kernel.
      !>C
      !>      CALL FURNSH ( 'badkpv_ex1.tpc' )
      !>
      !>C
      !>C     Change the default behavior of the SPICE error handling
      !>C     system to print out all messages and continue the
      !>C     execution of the program. We do this for demonstration
      !>C     purposes. Please, refrain from changing the default
      !>C     behavior on real applications.
      !>C
      !>      CALL ERRACT ( 'SET', 'REPORT' )
      !>
      !>C
      !>C     Assume that we need some data for body 399 and we expect
      !>C     there to be an even number of items available and at
      !>C     least 4 such items. Moreover we expect these items to be
      !>C     numeric. Note that the variable assignments below are
      !>C     present only to assist in understanding the calls to
      !>C     BADKPV.
      !>C
      !>      NAME   = 'BODY_399_DATA'
      !>      COMP   = '=>'
      !>      SIZE   =  4
      !>      DIVBY  =  2
      !>      TYPE = 'N'
      !>
      !>      IF ( .NOT. BADKPV( CALLER, NAME,  COMP,
      !>     .                   SIZE,   DIVBY, TYPE ) ) THEN
      !>
      !>         WRITE(*,'(3A)') 'Expected form of variable ',
      !>     .                   NAME(:RTRIM(NAME)),
      !>     .                   ' found in kernel pool.'
      !>
      !>      END IF
      !>
      !>C
      !>C     In addition we need the names given to these items.
      !>C     Improperly indicate the array has type numeric.
      !>C
      !>      NAME   = 'BODY_399_NAMES'
      !>      COMP   = '=>'
      !>      SIZE   =  4
      !>      DIVBY  =  1
      !>      TYPE = 'N'
      !>
      !>      IF ( .NOT. BADKPV( CALLER, NAME,  COMP,
      !>     .                   SIZE,   DIVBY, TYPE ) ) THEN
      !>
      !>         WRITE(*,'(3A)') 'Expected form of variable ',
      !>     .                   NAME(:RTRIM(NAME)),
      !>     .                   ' found in kernel pool.'
      !>
      !>      END IF
      !>
      !>C
      !>C     Change the behavior of the SPICE error handling to
      !>C     its default.
      !>C
      !>      CALL ERRACT ( 'SET', 'DEFAULT' )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Expected form of variable BODY_399_DATA found in kernel pool.
      !>
      !>        ============================================================***
      !>
      !>        Toolkit version: N0066
      !>
      !>        SPICE(BADVARIABLETYPE) --
      !>
      !>        BADKPV_EX1: The kernel pool variable 'BODY_399_NAMES' must b***
      !>        "NUMERIC". However, the current type is character.
      !>
      !>        A traceback follows.  The name of the highest level module i***
      !>        BADKPV
      !>
      !>        ============================================================***
      !>
      !>        Warning: incomplete output. 4 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
      !>
      !>        Note that, as expected, the error SPICE(BADVARIABLETYPE) is
      !>        signaled by the second BADKPV call, since we have improperly
      !>        indicated that the requested array is numeric, when actually
      !>        it is of character type.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 05-SEP-2021 (JDR) (BVS)
      !>
      !>        Fixed typo in long message for the case "comparison not
      !>        favorable."
      !>
      !>        Added exception SPICE(INVALIDTYPE) for the case of unknown
      !>        expected kernel pool variable type.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing fragment. Added required
      !>        readings references.
      !>
      !>        Removed references to FURNSH and CLPOOL from the "variable
      !>        not present" long error message.
      !>
      !>-    SPICELIB Version 1.1.2, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAY-2000 (WLT)
      !>
      !>        Modified the example section so that it is consistent with
      !>        calling sequence for BADKPV.
      !>
      !>-    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT)
      !>
      !>        Moved the initial assignment of BADKPV to the lines
      !>        prior to the check of RETURN(). This avoids returning
      !>        without having assigned value to BADKPV.
      !>
      !>-    SPICELIB Version 1.0.0, 09-APR-1997 (WLT)
      module logical function badkpv ( ctx, caller, name, comp, size, divby, type )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqchr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: caller  !! Name of the routine calling this routine.
      character(*), intent(in) :: name  !! Name of a kernel pool variable.
      character(*), intent(in) :: comp  !! Comparison operator.
      integer(ip), intent(in) :: size  !! Expected size of the kernel pool variable.
      integer(ip), intent(in) :: divby  !! A divisor of the size of the kernel pool variable.
      character(*), intent(in) :: type  !! Expected type of the kernel pool variable.

      character(1) :: class

      integer(ip) :: dim
      integer(ip) :: ratio

      logical :: ok
      logical :: found

!
!     Until we know otherwise, we shall assume that we have
!     a bad kernel pool variable.
!
      badkpv = .true.

      if ( ctx%failed() ) return

!
!     Look up the attributes of this variable in the kernel pool.
!
      call dtpool ( ctx, name, found, dim, class )

      if ( .not. found ) then

         call ctx%setmsg ( '#: The kernel pool variable ''#'' is not ' &
         //            'currently present in the kernel pool. ' &
         //            'Possible reasons are that the ' &
         //            'appropriate text kernel file has not ' &
         //            'been loaded or that the kernel pool has ' &
         //            'been cleared after loading the appropriate ' &
         //            'text kernel file. ' )

         call ctx%errch  ( '#', caller )
         call ctx%errch  ( '#', name   )
         call ctx%sigerr ( 'SPICE(VARIABLENOTFOUND)' )
         return

      end if

!
!     Compare the dimension of the specified variable with the
!     input SIZE.
!
      if      ( comp == '='  ) then

         ok = dim == size

      else if ( comp == '<'  ) then

         ok = dim < size

      else if ( comp == '>'  ) then

         ok = dim > size

      else if ( comp == '<=' ) then

         ok = dim <= size

      else if ( comp == '=>' ) then

         ok = dim >= size

      else

         call ctx%setmsg ( '#: The comparison operator ''#'' is not ' &
         //            'a recognized value.  The recognized ' &
         //            'values are ''<'', ''<='', ''='', ''=>'', ' &
         //            '''>''. ' )
         call ctx%errch  ( '#', caller )
         call ctx%errch  ( '#', comp   )
         call ctx%sigerr ( 'SPICE(UNKNOWNCOMPARE)' )
         return

      end if

!
!     If the comparison was not favorable, signal an error
!     and return.
!
      if ( .not. ok ) then

         call ctx%setmsg ( '#: The kernel pool variable ''#'' is ' &
         //            'expected to have a number of components ' &
         //            'DIM such that the comparison DIM # # is ' &
         //            '.TRUE.  However, the current number of ' &
         //            'components for ''#'' is #. ' )

         call ctx%errch  ( '#', caller )
         call ctx%errch  ( '#', name   )
         call ctx%errch  ( '#', comp   )
         call ctx%errint ( '#', size   )
         call ctx%errch  ( '#', name   )
         call ctx%errint ( '#', dim    )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)' )
         return

      end if

!
!     Check to see that DIVBY evenly divides the dimension of
!     the variable.
!
      if ( divby /= 0 ) then
         ratio = dim / divby
      else
         ratio = 1
      end if

      if ( divby*ratio /= dim ) then

         call ctx%setmsg ( '#: The number of components of the ' &
         //            'kernel pool variable ''#'' is required ' &
         //            'to be divisible by #.  However, the ' &
         //            'actual number of components is # which ' &
         //            'is not evenly divisible by #. '  )

         call ctx%errch  ( '#', caller )
         call ctx%errch  ( '#', name   )
         call ctx%errint ( '#', divby  )
         call ctx%errint ( '#', dim    )
         call ctx%errint ( '#', divby  )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'  )
         return

      end if

!
!     Finally check the type of the variable.
!
      if      ( eqchr ( type, 'C' ) ) then

         if ( class /= 'C' ) then

            call ctx%setmsg ( '#: The kernel pool variable ''#'' ' &
            //            'must be of type "CHARACTER". ' &
            //            'However, the current type is numeric. ' )
            call ctx%errch  ( '#', caller )
            call ctx%errch  ( '#', name   )
            call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)' )
            return

         end if

      else if ( eqchr ( type, 'N' ) ) then

         if ( class /= 'N' ) then

            call ctx%setmsg ( '#: The kernel pool variable ''#'' ' &
            //            'must be of type "NUMERIC".  However, ' &
            //            'the current type is character. ' )
            call ctx%errch  ( '#', caller )
            call ctx%errch  ( '#', name   )
            call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)' )
            return

         end if

      else

         call ctx%setmsg ( '#: Unknown expected type of the kernel pool ' &
            //         'variable ''#''. The expected type of the ' &
            //         'kernel pool variable must be either ''C'' ' &
            //         'or ''N''.'  )
         call ctx%errch  ( '#', caller )
         call ctx%errch  ( '#', type                                       )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                            )
         return

      end if

      badkpv = .false.

      return
      end function badkpv

      !> Parse a string as an integer, encapsulating error handling.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a string representing a numeric value. Commas and
      !>              spaces may be used in this string for ease of reading
      !>              and writing the number. They are treated as
      !>              insignificant but non-error-producing characters.
      !>
      !>              For exponential representation any of the characters
      !>              'E','D','e','d' may be used.
      !>
      !>              The following are legitimate numeric expressions
      !>
      !>                 +12.2 e-1
      !>                 -3. 1415 9276
      !>                 1e6
      !>                 E8
      !>
      !>              The program also recognizes the following  mnemonics
      !>
      !>                 'PI',  'pi',  'Pi',  'pI'
      !>                 '+PI', '+pi', '+Pi', '+pI'
      !>                 '-PI', '-pi', '-Pi', '-pI'
      !>
      !>              and returns the value ( + OR - ) 3 as appropriate.
      !>
      !>## Detailed Output
      !>
      !>     INTVAL   is the integer obtained by parsing STRING. If an error is
      !>              encountered, INTVAL is not changed from whatever the
      !>              input value was. If the input string has a fractional
      !>              part, the fractional part will be truncated. Thus
      !>              3.18 is interpreted as 3. -4.98 is interpreted as -4.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input string cannot be parsed or if the string
      !>         represents a number that is outside the range of
      !>         representable integers, as defined by INTMIN and INTMAX, the
      !>         error SPICE(NOTANINTEGER) is signaled. The value of INTVAL is
      !>         not changed from whatever the input value was.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The purpose of this routine is to enable safe parsing of numeric
      !>     values into an INTEGER variable without the necessity of in-line
      !>     error checking.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Parse into an INTEGER variable a set of strings representing
      !>        numeric values.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PRSINT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 10 )
      !>
      !>      INTEGER               STRLEN
      !>      PARAMETER           ( STRLEN = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)    STRVAL ( SETSIZ )
      !>
      !>      INTEGER               I
      !>      INTEGER               INTVAL
      !>
      !>C
      !>C     Initialize the array of strings.
      !>C
      !>      DATA                  STRVAL / '100,000,000',
      !>     .                               ' -2 690 192',
      !>     .                               '  +12.2 e-1',
      !>     .                               '-3. 141 592',
      !>     .                               '      1.2e8',
      !>     .                               '         E6',
      !>     .                               '         Pi',
      !>     .                               '        -PI',
      !>     .                               '-2147483648',
      !>     .                               ' 2147483647' /
      !>
      !>C
      !>C     Parse each string into an INTEGER variable.
      !>C
      !>      WRITE(*,'(A)') '   STRVAL       INTVAL'
      !>      WRITE(*,'(A)') '-----------  ------------'
      !>      DO I = 1, SETSIZ
      !>
      !>         CALL PRSINT ( STRVAL(I), INTVAL )
      !>
      !>         WRITE(*,'(A11,2X,I12)') STRVAL(I), INTVAL
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>          STRVAL        INTVAL
      !>        -----------  ------------
      !>        100,000,000     100000000
      !>         -2 690 192      -2690192
      !>          +12.2 e-1             1
      !>        -3. 141 592            -3
      !>              1.2e8     120000000
      !>                 E6       1000000
      !>                 Pi             3
      !>                -PI            -3
      !>        -2147483648   -2147483648
      !>         2147483647    2147483647
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
      !>-    SPICELIB Version 1.0.1, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Updated the header to properly describe its input, output,
      !>        exceptions and particulars.
      !>
      !>-    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB)
      module subroutine prsint ( ctx, string, intval )
      use pepper_kinds, only: ip
      use pepper_strings, only: nparsi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! String representing a numeric value.
      integer(ip), intent(out) :: intval  !! Integer value obtained by parsing STRING.

      integer(ip), parameter :: msglen = 320

      character(msglen) :: errmsg
      integer(ip) :: ptr

      call nparsi ( string, intval, errmsg, ptr )

      if ( errmsg /= ' ' ) then

         call ctx%setmsg ( errmsg                 )
         call ctx%sigerr ( 'SPICE(NOTANINTEGER)'  )
         return

      end if

      return
      end subroutine prsint

      !> Take a measurement X, the units associated with
      !> X, and units to which X should be converted; return Y ---
      !> the value of the measurement in the output units.
      !>
      !>## Detailed Input
      !>
      !>     X        is a number representing a measurement in the units
      !>              specified by IN.
      !>
      !>     IN       is the identifier of the units associated with the
      !>              measurement X. Acceptable units are:
      !>
      !>                 Angles:                 'RADIANS'
      !>                                         'DEGREES'
      !>                                         'ARCMINUTES'
      !>                                         'ARCSECONDS'
      !>                                         'HOURANGLE'
      !>                                         'MINUTEANGLE'
      !>                                         'SECONDANGLE'
      !>
      !>                 Metric Distances:       'M'
      !>                                         'METERS'
      !>                                         'KM'
      !>                                         'KILOMETERS'
      !>                                         'CM'
      !>                                         'CENTIMETERS'
      !>                                         'MM'
      !>                                         'MILLIMETERS'
      !>
      !>                 English Distances:      'FEET'
      !>                                         'INCHES'
      !>                                         'YARDS'
      !>                                         'STATUTE_MILES'
      !>                                         'NAUTICAL_MILES'
      !>
      !>                 Astrometric Distances:  'AU'
      !>                                         'PARSECS'
      !>                                         'LIGHTSECS'
      !>                                         'LIGHTYEARS' julian lightyears
      !>
      !>                 Time:                   'SECONDS'
      !>                                         'MINUTES'
      !>                                         'HOURS'
      !>                                         'DAYS'
      !>                                         'JULIAN_YEARS'
      !>                                         'TROPICAL_YEARS'
      !>                                         'YEARS' (same as julian years)
      !>
      !>     OUT      is the identifier of the units desired for the
      !>              measurement X. See the description of IN.
      !>
      !>## Detailed Output
      !>
      !>     Y        is the input measurement converted to the desired
      !>              units.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input units, output units, or both input and
      !>         output units are not recognized, the error
      !>         SPICE(UNITSNOTREC) is signaled.
      !>
      !>     2)  If the units being converted between are incompatible, the
      !>         error SPICE(INCOMPATIBLEUNITS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine converts a measurement X given in units specified by
      !>     IN to the equivalent value Y in units specified by OUT.
      !>
      !>     If a unit is not recognized, an error message is produced that
      !>     indicates which one was not recognized.
      !>
      !>     If input and output units are incompatible (for example ANGLE
      !>     and DISTANCE units) and error message will be produced stating
      !>     the requested units and associated types.
      !>
      !>## Examples
      !>
      !>     To convert 1 meter to statute miles and feet you could
      !>
      !>        CALL CONVRT ( 1.0D0, 'METERS',        'STATUTE_MILES', MILES )
      !>        CALL CONVRT ( MILES, 'STATUTE_MILES', 'FEET',          FEET  )
      !>
      !>     or
      !>
      !>        CALL CONVRT ( 1.0D0, 'METERS', 'STATUTE_MILES', MILES )
      !>        CALL CONVRT ( 1.0D0, 'METERS', 'FEET',          FEET  )
      !>
      !>## Restrictions
      !>
      !>     1)  This routine does not do any checking for overflow. The caller
      !>         is required to make sure that the units used for the
      !>         measurement are such that no floating point overflow will
      !>         occur when the conversion is performed.
      !>
      !>     2)  Some of the units are not "defined" quantities. In such a case
      !>         a best estimate is provided as of the date of the current
      !>         version of this routine. Those estimated quantities are:
      !>
      !>            AU               The astronomical unit. The value was taken
      !>                             from the JPL ephemeris DE125. This value
      !>                             is an approximation and should not be used
      !>                             for high-accuracy work. It agrees with the
      !>                             value used in the JPL planetary ephemeris
      !>                             DE430 (149597870.700 km) at the 100m
      !>                             level.
      !>
      !>            TROPICAL_YEARS   The tropical year is the time from equinox
      !>                             to equinox. This varies slightly with
      !>                             time.
      !>
      !>            PARSECS          The parsec is the distance to an object
      !>                             whose parallax angle is one arcsecond. Its
      !>                             value is dependent upon the value of the
      !>                             astronomical unit.
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Corrected a typo in $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.0, 12-MAY-2015 (NJB)
      !>
      !>        Added support for full names of metric distance units. Added
      !>        support for the abbreviation 'M' indicating meters.
      !>
      !>-    SPICELIB Version 1.0.2, 01-JUL-2014 (NJB)
      !>
      !>        Updated the description of the AU in the $Restrictions
      !>        section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WMO) (WLT) (IMU)
      module subroutine convrt ( ctx, x, in, out, y )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpr
      use pepper_strings, only: ucase
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! Number representing a measurement in some units.
      character(*), intent(in) :: in  !! The units in which X is measured.
      character(*), intent(in) :: out  !! Desired units for the measurement.
      real(wp), intent(out) :: y  !! The measurement in the desired units.

      character(16) :: inu
      character(16) :: outu

      integer(ip) :: i
      integer(ip) :: j
      real(wp) :: temp

      real(wp), parameter :: secpjy = 86400.0_wp * 365.25_wp

      real(wp), parameter :: secpty = 86400.0_wp * 365.2421988_wp

      real(wp), parameter :: ltyear = 86400.0_wp * 365.25_wp * 299792458.0_wp

      real(wp), parameter :: dpm = 1.0_wp  /   60.0_wp

      real(wp), parameter :: dps = 1.0_wp  / 3600.0_wp

      real(wp), parameter :: dpsa = 15.0_wp / 3600.0_wp

      real(wp), parameter :: au = 1.4959787061368887e11_wp
!
!     1.0d0 divided by the sin of 1 arc second
!
      real(wp), parameter :: scale = 2.0626480624790438e05_wp

      real(wp), parameter :: parsec = au * scale

      integer(ip), parameter :: ang = 0

      integer(ip), parameter :: dist = ang  + 7

      integer(ip), parameter :: tme = dist + 17

      integer(ip), parameter :: total = tme + 7

      character(16), parameter :: units (total) = [ character(16) :: &
         'RADIANS', 'DEGREES', 'ARCMINUTES', 'ARCSECONDS',&
         'HOURANGLE', 'MINUTEANGLE', 'SECONDANGLE', 'METERS',&
         'KM', 'CM', 'MM', 'LIGHTSECS',&
         'AU', 'M', 'KILOMETERS', 'CENTIMETERS',&
         'MILLIMETERS', 'FEET', 'INCHES', 'STATUTE_MILES',&
         'NAUTICAL_MILES', 'YARDS', 'LIGHTYEARS', 'PARSECS',&
         'SECONDS', 'MINUTES', 'HOURS', 'DAYS',&
         'JULIAN_YEARS', 'TROPICAL_YEARS', 'YEARS' ]

      real(wp), parameter :: cnvrtn (total) = [ &
         dpr, 1.0_wp, dpm, dps,&
         15.000_wp, 0.250_wp, dpsa, 1.00_wp,&
         1000.0_wp, 0.01_wp, 0.001_wp, 299792458.0_wp,&
         au, 1.00_wp, 1000.0_wp, 0.01_wp,&
         0.001_wp, 0.3048_wp, 0.0254_wp, 1609.344_wp,&
         1852.0_wp, 0.9144_wp, ltyear, parsec,&
         1.0_wp, 60.0_wp, 3600.0_wp, 86400.0_wp,&
         secpjy, secpty, secpjy ]

      character(8), parameter :: type (total) = [ character(8) :: &
         'ANGLE', 'ANGLE', 'ANGLE', 'ANGLE',&
         'ANGLE', 'ANGLE', 'ANGLE', 'DISTANCE',&
         'DISTANCE', 'DISTANCE', 'DISTANCE', 'DISTANCE',&
         'DISTANCE', 'DISTANCE', 'DISTANCE', 'DISTANCE',&
         'DISTANCE', 'DISTANCE', 'DISTANCE', 'DISTANCE',&
         'DISTANCE', 'DISTANCE', 'DISTANCE', 'DISTANCE',&
         'TIME', 'TIME', 'TIME', 'TIME',&
         'TIME', 'TIME', 'TIME' ]

!
!     Angular Conversions:
!
!                 (1)  Degrees/Radians
!                 (2)  Degrees/Degrees
!                 (3)  Degrees/ARCMINUTES
!                 (4)  Degrees/ARCSECONDS
!
!                 ()   Degrees/HOURANGLE
!                 ()   Degrees/MINUTEANGLE
!                 ()   Degrees/SECONDANGLE
!

!
!     DATA CNVRTN (ANG + 1)  /      DPR()   /
!
!     This value will be loaded using the SPICELIB function DPR()
!     on the first execution of this routine.
!

!
!     Distance Conversions ( 8 through 17 )
!
!                 (+  1) Meters/Meter
!                 (+  2) Meters/Km
!                 (+  3) Meters/Cm
!                 (+  4) Meters/mm
!                 (+  5) Meters/Lightsecs
!                 (+  6) Meters/AU
!                 (+  7) Meters/Meter
!                 (+  8) Meters/Km
!                 (+  9) Meters/cm
!                 (+ 10) Meters/mm
!

!
!     Distance Conversions
!
!                 (+ 11) Meters/Foot
!                 (+ 12) Meters/inch
!                 (+ 13) Meters/Statute Mile
!                 (+ 14) Meters/Nautical Mile
!                 (+ 15) Meters/Yard
!

!
!     Distance Conversions
!
!                 (+ 16) Meters/LightYear
!                 (+ 17) Meters/Parsec
!

!
!     Time Conversions
!
!                 (+ 1 ) seconds / second
!                 (+ 2 ) seconds / minute
!                 (+ 3 ) seconds / hour
!                 (+ 4 ) seconds / day
!                 (+ 5 ) Seconds / Julian year
!                 (+ 6 ) Seconds / Tropical year
!                 (+ 7 ) Seconds / year          --- same as Julian year
!

!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

      call ucase ( in,  inu  )
      call ucase ( out, outu )

      i    = findloc(units, inu, dim=1)
      j    = findloc(units, outu, dim=1)

      if (      ( i == 0 ) &
           .or. ( j== 0 ) ) then

         if (       ( i == 0 ) &
              .and. ( j == 0 ) ) then

            call ctx%setmsg ('CONVRT: Neither the input units ' // inu  // &
                         'nor the output units '            // outu // &
                         'were recognized.' &
                        )

            call ctx%sigerr ('SPICE(UNITSNOTREC)')

            return

         else if ( i == 0 ) then

            call ctx%setmsg ('CONVRT: Input units '// inu // &
                         ' were not recognized' &
                        )

            call ctx%sigerr ('SPICE(UNITSNOTREC)')

            return

         else if ( j == 0 ) then

            call ctx%setmsg ('CONVRT: Output units '// outu // &
                         ' were not recognized' &
                        )

            call ctx%sigerr ('SPICE(UNITSNOTREC)')

            return

         end if

      end if

      if ( type(i) /= type(j) ) then

         call ctx%setmsg ('CONVRT: Incompatible units. '   // &
                      'You are attempting to convert ' // &
                      inu  // 'type: ' // type(i)      // &
                      ' to '                           // &
                      outu // 'type: ' // type(j)      // &
                      '.' &
                     )

         call ctx%sigerr ('SPICE(INCOMPATIBLEUNITS)')

         return

      end if

      temp = x    * cnvrtn ( i )
      y    = temp / cnvrtn ( j )

      return

      end subroutine convrt

      !> Determine whether or not any of the POOL variables that are to be
      !> watched and have AGENT on their distribution list have been
      !> updated, but do the full watcher check only if the POOL state
      !> counter has changed.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     AGENT       is the name of a subroutine, entry point, or
      !>                 significant portion of code that needs to access
      !>                 variables in the kernel pool. Generally this agent
      !>                 will buffer these variables internally and fetch them
      !>                 from the kernel pool only when they are updated.
      !>
      !>     USRCTR      is the value of the POOL state counter tracked by
      !>                 (saved in) the caller (user) routine specifically
      !>                 for this agent.
      !>
      !>## Detailed Output
      !>
      !>     USRCTR      is the current POOL state counter.
      !>
      !>     UPDATE      is a logical flag that will be set to true if the
      !>                 variables in the kernel pool that are required by
      !>                 AGENT have been updated since the last call to
      !>                 CVPOOL.
      !>
      !>## Parameters
      !>
      !>     CTRSIZ      is the dimension of the counter array used by
      !>                 various SPICE subsystems to uniquely identify
      !>                 changes in their states. This parameter is
      !>                 defined in the private include file 'zzctr.inc'.
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
      !>     This routine uses ZZPCTRCK to check the caller's saved POOL state
      !>     counter specific for the input AGENT against the current POOL
      !>     state counter to bypass the call to CVPOOL in cases when the POOL
      !>     has not changed.
      !>
      !>     If the POOL state counter specific for the input AGENT did not
      !>     change it bypasses the call to CVPOOL and returns the UPDATE flag
      !>     set to .FALSE.
      !>
      !>     If the POOL state counter specific for the input AGENT did change
      !>     it calls CVPOOL and returns the UPDATE flag set to whatever value
      !>     CVPOOL sets it to.
      !>
      !>     For details on ZZPCTRCK and CVPOOL see POOL the umbrella routine.
      !>
      !>## Examples
      !>
      !>     Suppose that you have an application subroutine, TASK, that
      !>     needs to access to two large independent data set in the kernel
      !>     pool. If this data could be kept in local storage and kernel pool
      !>     queries performed only when the data in the kernel pool has been
      !>     updated, the routine can perform much more efficiently.
      !>
      !>     The code fragment below illustrates how you might make use of this
      !>     feature.
      !>
      !>     C
      !>     C     On the first call to this routine set two separate
      !>     C     watchers for the two sets of POOL variables that we are
      !>     C     interested in and initialize the two POOL state counters,
      !>     C     one for each POOL agent.
      !>     C
      !>           IF ( FIRST ) THEN
      !>
      !>              FIRST = .FALSE.
      !>              HAVE1 = .FALSE.
      !>              HAVE2 = .FALSE.
      !>
      !>              CALL ZZCTRUIN ( USRCT1 )
      !>              CALL ZZCTRUIN ( USRCT2 )
      !>
      !>              CALL SWPOOL ( 'TASK1', NNAMS1, NAMES1 )
      !>              CALL SWPOOL ( 'TASK2', NNAMS2, NAMES2 )
      !>
      !>           END IF
      !>
      !>     C
      !>     C     If any of the variables has been updated, fetch
      !>     C     it from the kernel pool. (Note that this also
      !>     C     handles getting variables for the first time.)
      !>     C     We use HAVE1 to indicate the fetch succeeded. If it
      !>     C     didn't, we need to attempt the fetch on the next
      !>     C     pass into this routine. Do the first set of variable.
      !>     C
      !>           CALL CVPOOL ( 'TASK1', USRCT1, UPDATE )
      !>
      !>           IF (  UPDATE  .OR (.NOT. HAVE1 ) ) THEN
      !>
      !>              CALL GDPOOL ( 'TASK1_VAR_1', 1, M, N1, VALS1, FOUND(1) )
      !>              CALL GDPOOL ( 'TASK1_VAR_2', 1, M, N2, VALS2, FOUND(2) )
      !>                      .
      !>              CALL GDPOOL ( 'TASK1_VAR_N', 1, M, NN, VALSN, FOUND(N) )
      !>
      !>           END IF
      !>
      !>           IF ( FAILED() ) THEN
      !>                 .
      !>              do something about the failure
      !>                 .
      !>           ELSE
      !>              HAVE1 = .TRUE.
      !>           END IF
      !>
      !>     C
      !>     C     Do the second set of variable.
      !>     C
      !>           CALL CVPOOL ( 'TASK2', USRCT2, UPDATE )
      !>
      !>           IF (  UPDATE  .OR (.NOT. HAVE2 ) ) THEN
      !>
      !>              CALL GDPOOL ( 'TASK2_VAR_1', 1, M, N1, VALS1, FOUND(1) )
      !>              CALL GDPOOL ( 'TASK2_VAR_2', 1, M, N2, VALS2, FOUND(2) )
      !>                      .
      !>              CALL GDPOOL ( 'TASK2_VAR_N', 1, M, NN, VALSN, FOUND(N) )
      !>
      !>           END IF
      !>
      !>           IF ( FAILED() ) THEN
      !>                 .
      !>              do something about the failure
      !>                 .
      !>           ELSE
      !>              HAVE2 = .TRUE.
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     The POOL counter passed to the routine by the caller must be
      !>     specific to the POOL agent that's being checked.
      !>
      !>     The caller routines watching more than one agent must set up a
      !>     separate local POOL counter for each agent.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     B.V. Semenov   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-2013 (BVS)
      module subroutine zzcvpool ( ctx, agent, usrctr, update )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: agent  !! Name of the agent to check for notices
      integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! POOL state counter tracked by the caller
      logical, intent(out) :: update  !! .TRUE. if variables for AGENT have been updated

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check/update counter.
!
      call zzpctrck ( ctx, usrctr, update )

!
!     If counter was updated, check in and call CVPOOL.
!
      if ( update ) then

         call cvpool ( ctx, agent, update )

      end if

      return

      end subroutine zzcvpool

      !> Parse a substring of the form ::UTC[+/-]1-12:0-59
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a string that has an embedded substring of the
      !>                form ::UTC+HR[:MN] ( or ::UTC-HR[:MN] starting at
      !>                character start.
      !>
      !>     START      is the location in STRING where a time zone
      !>                specification is believed to begin.
      !>
      !>## Detailed Output
      !>
      !>     HOFF       is the double precision value associated with
      !>                HR in the picture above.  This value will be
      !>                between -12 and 12 inclusive.
      !>
      !>     MOFF       is the double precision value associated with MN
      !>                in the picture above.  This value will be between
      !>                0 and 59 inclusive (or -59 and 0 inclusive) depending
      !>                on the sign present in the UTC+/- substring.  The
      !>                sign of MOFF is the same as the sign present in the
      !>                string.
      !>
      !>     LAST       is the last character of the time zone specification.
      !>                If the string doesn't have a correct format and
      !>                range of values, LAST is returns as START - 1.
      !>
      !>     SUCCES     is a logical which if true, indicates that a time
      !>                zone was successfully parsed.
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
      !>     1) There are no exceptions.  Either the string matches
      !>        the template or it doesn't.  No case is regarded
      !>        as an error.
      !>
      !>## Particulars
      !>
      !>     This is a private routine for parsing time zones specified
      !>     as UTC+/-HR:MN  where HR is an unsigned integer between 0 and
      !>     11.  HR must have no more than 2 digits.  MN is expected
      !>     to be an unsigned integer between 0 and 59 inclusive.  It must
      !>     have no more than 2 digits.
      !>
      !>## Examples
      !>
      !>     See TIMOUT.
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
      !>-    SPICELIB Version 1.0.0, 27-SEP-1996 (WLT)
      module subroutine zzutcpm ( ctx, string, start, hoff, moff, last, succes )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: lx4uns, nparsd, samch
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! is a string containing a substring ::UTC+HR:MN
      integer(ip), intent(in) :: start  !! is the location in the string to start parsing
      real(wp), intent(out) :: hoff  !! is the d.p. value associated with HR.
      real(wp), intent(out) :: moff  !! is the d.p. value associated with MN
      integer(ip), intent(out) :: last  !! is the end of the time zone substring.
      logical, intent(out) :: succes  !! indicates that a time zone was parsed.


      character(lnsize) :: error

      integer(ip) :: need
      integer(ip) :: length
      integer(ip) :: signat
      integer(ip) :: unsat
      integer(ip) :: unsto
      integer(ip) :: nchar
      integer(ip) :: ptr

      real(wp) :: sign
      real(wp) :: x

!
!     This is a special purpose routine.  The input string must have
!     exactly the right format to be a time zone substring.  If anything
!     goes wrong, we just bail out and leave HOFF and MOFF right at
!     zero.
!
      hoff   =  0.0_wp
      moff   =  0.0_wp
      last   =  start - 1
      succes = .false.

!
!     Note that NEED   = START + LEN('::UTC+x') - 1
!               SIGNAT = START + LEN('::UTC+' ) - 1
!
      length = len(string)
      need   = start + 6
      signat = start + 5
      unsat  = need

      if ( length < need ) return

      if       ( ichar(string(signat:signat)) == ichar('+') ) then
         sign =  1.0_wp
      else if  ( ichar(string(signat:signat)) == ichar('-') ) then
         sign = -1.0_wp
      else
         return
      end if
!
!     So far everything looks fine, "lex" the string starting at
!     SIGNAT + 1 for an unsigned integer.
!
      call lx4uns ( string, unsat, unsto, nchar )

      if ( nchar > 0 .and. nchar < 3 ) then
         call nparsd ( string(unsat:unsto), x, error, ptr )

         if ( x >= 13.0_wp ) return

         last = unsto
         hoff = sign * x

      else
         return
      end if
!
!     If we're still in the game at this point, we have at least
!     an hour offset, see if there is a minutes portion to the
!     time zone.
!
      succes = .true.

      if ( samch( string, unsto+1, ':', 1 ) ) then
         unsat = unsto + 2
      else
         return
      end if

      call lx4uns ( string, unsat, unsto, nchar )

      if ( nchar > 0 .and. nchar < 3 ) then
         call nparsd ( string(unsat:unsto), x, error, ptr )

         if ( x > 59.0_wp ) return

         last = unsto
         moff = sign*x

      end if

      return
      end subroutine zzutcpm

      !> The function returns the exponent associated with X.
      !>
      !> Set up the virtual string associated with X and return the
      !> exponent associated with X when represented in scientific
      !> notation.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     X          is a double precision number that from which
      !>                a virtual decimal string should be created.
      !>
      !>     FILL       is the character to use for the leading character
      !>                in the virtual decimal string.
      !>
      !>## Detailed Output
      !>
      !>     EXPONT     is the value of the scientific notation
      !>                exponent associated with X.
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
      !>     Error free.
      !>
      !>## Particulars
      !>
      !>     This entry point is used to establish a virtual decimal string.
      !>     The companion entry point ZZVSBSTR is used to retrieve the
      !>     characters in the virtual string.
      !>
      !>## Examples
      !>
      !>     See the main entry point or the routine DPFMT.
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
      !>-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
      module subroutine zzvststr ( ctx, x, fill, expont )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: dpstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! double precision number to needing a virtual string
      character(1), intent(in) :: fill  !! leading character for virtual string.
      integer(ip), intent(out) :: expont  !! The exponent associated with X.
      integer(ip), parameter :: esgn = maxsig + 4
      integer(ip), parameter :: efst = esgn + 1
      integer(ip), parameter :: escd = efst + 1
      integer(ip) :: i
      integer(ip) :: blank
      logical :: minus
      integer(ip) :: code
      if ( .not. allocated ( ctx%zzvstrng_state ) ) allocate ( ctx%zzvstrng_state )
      associate ( string => ctx%zzvstrng_state%string, &
                  myfill => ctx%zzvstrng_state%myfill, &
                  code0 => ctx%zzvstrng_state%code0, &
                  exp => ctx%zzvstrng_state%exp )
      myfill = fill

      call dpstr ( x, maxsig, string )

!
!     Parse the exponent, string looks like the pattern presented
!     below:
!
!                    MAXSIG + 2
!                    |
!                    v
!     by.xxxxxxxxxxxxxEsxxx
!     1234567890123456789
!                      ^^
!                      ||
!                      |EFST = ESGN + 1
!                      |
!                      ESGN = MAXSIG + 4
!

      code0 = ichar ( '0' )
      blank = ichar ( ' ' )

      minus = ichar( string(esgn:esgn) )== ichar('-')
      code  = ichar( string(efst:efst) )
      exp   = code - code0

      i     = escd
      code  = ichar( string(i:i) )

      do while ( code /= blank )

         exp  = exp * 10 + ( code - code0 )
         i    = i + 1
         code = ichar( string(i:i) )

      end do

      if ( minus ) then
         exp = -exp
      end if

      expont = exp

      return
      end associate
      end subroutine zzvststr

      !> Return the character from the specified SLOT of a virtual
      !> decimal string.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FROM       is the index in the virtual decimal string of the
      !>                first character that will be returned in SUBSTR.
      !>
      !>     TO         is the index in the virtual decimal string of the
      !>                last character that will be returned in SUBSTR.
      !>
      !>     RND        is a logical flag used to indicate that the output
      !>                string should represent the virtual decimal string
      !>                that results from rounding to the TO'th decimal
      !>                location.
      !>
      !>## Detailed Output
      !>
      !>     SUBSTR     is we regard the virtual string as VIRTUL.  Then
      !>                in FORTRAN notation SUBSTR = VIRTUL(FROM:TO)
      !>
      !>     DID        is a logical flag that is used to indicate that
      !>                the left most character returned by ZZVSBSTR became
      !>                a zero as a result of rounding up from 9.
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
      !>     Error free.
      !>
      !>## Particulars
      !>
      !>     This entry point retrieves a specified character from the
      !>     virtual decimal string that was established by the last
      !>     call to the entry point ZZVSTSTR.
      !>
      !>## Examples
      !>
      !>     See the main entry point or the routine DPFMT.
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
      !>-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
      module subroutine zzvsbstr ( ctx, from, to, rnd, substr, did )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: from  !! the index of the first character to retrieve
      integer(ip), intent(in) :: to  !! the index of the last character to retrieve
      logical, intent(in) :: rnd  !! treat the virtual string as rounded string.
      character(*), intent(out) :: substr  !! Contents of virtual string from FROM to TO.
      logical, intent(out) :: did  !! is a leading zero a result of rounding.
      integer(ip) :: slot
      integer(ip) :: j
      logical :: incr
      integer(ip) :: i
      character(1) :: letter
      integer(ip) :: value
      integer(ip) :: lsub
      if ( .not. allocated ( ctx%zzvstrng_state ) ) allocate ( ctx%zzvstrng_state )
      associate ( string => ctx%zzvstrng_state%string, &
                  myfill => ctx%zzvstrng_state%myfill, &
                  code0 => ctx%zzvstrng_state%code0, &
                  exp => ctx%zzvstrng_state%exp )
!
!     The buffered numeric string has the form:
!
!       by.xxxxxxxxxxxxxEseee...
!       123456789012345678901234
!                1         2
!
!     Ignoring the exponent we can regard this as being the
!     decimal equivalent of the number with the decimal point
!     in the wrong position.  We'll need to remedy this.
!
!       by.xxxxxxxxxxxxx
!       1234567890123456
!                1
!
!      We can think of this decimal representation as being a
!      simplification of the "infinite string" representation
!      below.
!
!                     b    y   .   x   x       x
!        d-4 d-3 d-2 d-1  d00  p  d01 d02 ... d13  0   0   0   0
!        -2  -1   0   1    2   3   4   5       16
!
!     From this its clear that i'th digit can be easily computed
!     via following decision block.
!
!        if ( i .lt. 0 ) then
!           digit  = '0'
!        else if ( i .eq. 0 ) then
!           digit  = string(2:2)
!        else if ( i .lt. maxsig ) then
!           digit = string(i+3:i+3)
!        else
!           digit = '0'
!        end if
!
!     To have an accurate representation of the number (one that
!     accounts for the exponent) we shift the decimal point ('p')
!     "right" by EXP slots. (If EXP is negative we shift right a
!     negative number of slots).  In the sequence of characters the
!     decimal point will follow d_EXP.
!
!     IF we renumber the slots so that the decimal point is in
!     slot 0 then for S < 0 slot S contains digit d_EXP+1+S
!
!     For S > 0 slot S contains digit d_EXP+S
!
!     Combining these observations we can compute the SLOT'th character
!     of the virtual string as follows.
!
!     If the character requested is character zero of the virtual
!     string, we just get the decimal point.
!
!     If the character requested is in a slot whose index is
!     greater than zero it is to the
!     right of the decimal point so it must be D_exp+slot.
!
!     If the character requested is in a slot whose index is negative
!     it is to the left of the decimal point.  Since the slot
!     just to the left of the decimal point contains D_exp it follows
!     by induction that for any negative slot, the decimal is
!     D_exp+slot+1
!
!     Since we may need to round the output, we will work from right
!     to left.  First thing we do is get the index of the right most
!     significant portion of SUBSTR that we will manipulate.
!
      j    = to - from + 1
      lsub = len(substr)
!
!     Blank pad to the right of J (if there's anything to pad).
!
      if ( j < lsub ) then
         substr(j+1:) = ' '
      end if
!
!     If we need to round the output string, locate the first numeric
!     slot after TO.
!
      if ( rnd ) then

         slot = to + 1
!
!        If this points to the decimal point, move one more to the
!        right.
!
         if ( slot == 0 ) then
            slot = slot + 1
         end if
!
!        Determine which digit D_i corresponds to SLOT.
!
         if ( slot < 0 ) then
            i = exp + slot + 1
         else
            i = exp + slot
         end if
!
!        We will need to round in D_i is 5 or more.
!
         if ( i < 0 ) then
            letter = '0'
         else if ( i == 0 ) then
            letter = string(2:2)
         else if ( i < maxsig ) then
            letter = string(3+i:3+i)
         else
            letter = '0'
         end if

         incr = lge ( letter, '5' )

      else
         incr = no
      end if
!
!     Starting at the right most slot, we work left incrementing
!     digits as required.  Note that once we don't round up
!     some value, we are done incrementing.
!
      do slot = to, from, -1

         if ( slot == 0 ) then

            letter = '.'

         else

!
!           Otherwise we need to first see which digit, d_I, is being
!           requested.
!
            if ( slot < 0 ) then
               i = exp + slot + 1
            else
               i = exp + slot
            end if

!
!           Now just look up d_I according to the rule we established
!           earlier.
!
            if ( i < 0 ) then

!
!              If the SLOT is prior to the first significant character
!              or the virtual string, we use the fill character.
!              Otherwise we use a zero.
!

               if ( incr ) then
                  letter = '1'
                  incr   = no
               else

                  if ( slot < -1 ) then
                     letter = myfill
                  else
                     letter = '0'
                  end if

               end if

            else if ( i == 0 ) then

               letter = string(2:2)
!
!              If necessary, increment LETTER.
!
               if ( incr ) then

                  value = ichar(letter) - code0 + 1
!
!                 If value is 10 or more we will need to
!                 increment the next character too.  If VALUE
!                 is less than 10, we are done incrementing set
!                 INCR to NO.
!
                  if ( value == 10 ) then
                     letter = '0'
                  else
                     letter = char ( value + code0 )
                     incr   = no
                  end if

               end if

            else if ( i < maxsig ) then
!
!              This case is virtually identical to the previous
!              case, except that we need to pick off a different
!              letter from STRING.
!
               letter = string(i+3:i+3)

               if ( incr ) then

                  value = ichar(letter) - code0 + 1

                  if ( value == 10 ) then
                     letter = '0'
                  else
                     letter = char ( value + code0 )
                     incr   = no
                  end if
               end if

            else

               letter = '0'
               incr   = no

            end if

         end if

         if ( j <= lsub ) then
            substr(j:j) = letter
         end if

         j = j - 1

      end do

      did = incr

      return
      end associate
      end subroutine zzvsbstr

      !> Create a formatted string that represents a double precision
      !> number, using a format picture.
      !>
      !>## Detailed Input
      !>
      !>     X        is any double precision number.
      !>
      !>     PICTUR   is a string used to describe the format of the
      !>              output string. There are four special characters
      !>              recognized by DPFMT --- a leading + or -, a leading
      !>              zero ( '0' ) or a zero that follows a leading + or -,
      !>              and the first decimal point of the string.
      !>
      !>              All other non-blank characters are regarded as
      !>              equivalent. The picture ends at the first blank
      !>              character. The effects associated with the various
      !>              characters in a picture are spelled out in the
      !>              description of the output STRING.
      !>
      !>              The following pictures are treated as errors.
      !>
      !>              ' ', '+', '-', '.', '+.', '-.'
      !>
      !>## Detailed Output
      !>
      !>     STRING   is a string representing X that matches the input
      !>              picture. The format of STRING is governed by PICTUR.
      !>              It will represent X rounded to the level of precision
      !>              specified by PICTUR.
      !>
      !>              If the first character of the picture is a minus sign,
      !>              the first character in the output string will be
      !>              a blank if the number is non-negative, a minus sign
      !>              if the number is negative.
      !>
      !>              If the first character of the picture is a plus sign,
      !>              the first character of the output string will be a
      !>              plus if the number is positive, a blank if the number
      !>              is zero, and a minus sign if the number is negative.
      !>
      !>              If the first character of the string is NOT a sign
      !>              (plus or minus) the first character of the output
      !>              string will be a minus sign if the number is negative
      !>              and will be the first character of the integer part
      !>              of the number otherwise.
      !>
      !>              The integer portion of STRING will contain the same
      !>              number of characters as appear before the decimal
      !>              point (or last character if there is no decimal
      !>              point) but after a leading + or -.
      !>
      !>              If the picture begins with any of the following
      !>
      !>                 '+0', '-0', or '0'
      !>
      !>              it is said to have a leading zero. If a picture has
      !>              a leading zero and the integer portion is not large
      !>              enough to fill up the integer space specified by
      !>              PICTUR, STRING will be zero padded from the sign (if
      !>              one is required) up to the first character of the
      !>              integer part of the number.
      !>
      !>              If picture does NOT have a leading zero and the
      !>              integer portion is not large enough to fill up the
      !>              space specified by PICTUR, STRING will be blank
      !>              padded on the left between the sign (if one is
      !>              required) and the first character of the integer part
      !>              of the number.
      !>
      !>              If a decimal point ( '.' ) is present in PICTUR it
      !>              will be present following the integer portion of
      !>              STRING. Moreover, the decimal portion of STRING will
      !>              contain the same number of digits as there are
      !>              non-blank characters following the decimal point in
      !>              PICTUR. However, only the first 14 digits starting
      !>              with the first non-zero digit are meaningful.
      !>
      !>              If the format specified by PICTUR does not provide
      !>              enough room for the integer portion of X, the routine
      !>              determines whether or not the number of characters
      !>              present in the picture is sufficient to create a
      !>              representation for X using scientific notation. If
      !>              so, the output is displayed using scientific notation
      !>              (leading signs, if they are present in PICTUR, will
      !>              also appear in STRING).   If the format specified by
      !>              PICTUR is too short to accommodate scientific
      !>              notation, the output string is filled with '*' to the
      !>              same length as the length of PICTUR. Leading signs
      !>              are not preserved in this overflow case.
      !>
      !>              STRING may overwrite PICTUR.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If PICTUR begins with a blank, the error SPICE(NOPICTURE) is
      !>         signaled.
      !>
      !>     2)  If PICTUR consists only of '+', '-', '.', '+.' or '-.' are
      !>         regarded as invalid (there's no significant component to the
      !>         picture.) therefore, the error SPICE(BADPICTURE) is signaled.
      !>
      !>     3)  If the length of STR is less than the length of the first
      !>         non-blank portion of PICTUR, the error SPICE(OUTPUTTOOSHORT)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides a mechanism for producing numeric strings
      !>     formatted according to a user supplied picture. We expect that
      !>     the string produced by this routine will be used to assist in
      !>     the construction of a string that can be read by people.
      !>
      !>     Note that the process of converting a double precision number
      !>     to a string, in not precisely invertible even if the string
      !>     contains all of the significant figures allowed by this
      !>     routine. You should not anticipate that the string produced
      !>     by this routine can be "read" into a double precision number
      !>     to reproduce the double precision number X. To the level of
      !>     accuracy implied by the string representation, they will be
      !>     the same. But, they are unlikely to have the same internal
      !>     binary representation.
      !>
      !>## Examples
      !>
      !>     Suppose that X has the binary representation of PI. Then the
      !>     table below illustrates the strings that would be produced
      !>     by a variety of different pictures.
      !>
      !>     PICTUR         |    STRING
      !>     -------------------------------
      !>     '0x.xxx'       |  '03.142'
      !>     'xx.xxx'       |  ' 3.142'
      !>     '+xxx.yyyy'    |  '+  3.1416'
      !>     '-.yyyy'       |  '******'
      !>     'xxxxxxxx'     |  '       3'
      !>     '00xx'         |  '0003'
      !>     '-00.0000000'  |  ' 03.1415927'
      !>     '00'           |  '03'
      !>     'x.'           |  '3.'
      !>     '.mynumber'    |  '3.142E+00'
      !>     'my dog spot'  |  ' 3'
      !>     'my.dog spot'  |  ' 3.142'
      !>     '+my.dog,spot' |  '+ 3.14159265'
      !>
      !>     Suppose that X has the binary representation of 2/3. Then the
      !>     table below illustrates the strings that would be produced
      !>     by a variety of different pictures.
      !>
      !>     PICTUR         |    STRING
      !>     -------------------------------
      !>     '+x.xxx'       |  '+0.667'
      !>     '+xx.xxx'      |  '+ 0.667'
      !>     'xxx.yyyy'     |  '  0.6667'
      !>     '.yyyy'        |  '.6667'
      !>     'xxxxxxxx'     |  '       1'
      !>     '00xx'         |  '0001'
      !>     '-0.0000000'   |  ' 0.6666667'
      !>     '00'           |  '01'
      !>     'x.'           |  '1.'
      !>     'mynumber'     |  '       1'
      !>     'my dog spot'  |  ' 1'
      !>     'my.dog spot'  |  ' 0.667'
      !>     'my.dog,spot'  |  ' 0.66666667'
      !>
      !>     Suppose that X has the binary representation of -8/9. Then the
      !>     table below illustrates the strings that would be produced
      !>     by a variety of different pictures.
      !>
      !>     PICTUR         |    STRING
      !>     -------------------------------
      !>     '+x.xxx'       |  '-0.889'
      !>     '-00.xxxx'     |  '-00.8889'
      !>     'xxx.xxx'      |  ' -0.889'
      !>     '000.000'      |  '-00.889'
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.3, 27-OCT-2021 (JDR) (EDW) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.1, 22-JUN-1998 (WLT)
      !>
      !>        A number of typographical and grammatical errors
      !>        were corrected in the header.
      !>
      !>-    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT)
      module subroutine dpfmt ( ctx, x, pictur, str )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: dpstr, ncpos, pos
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! a double precision number.
      character(*), intent(in) :: pictur  !! a string describing the appearance of the output
      character(*), intent(out) :: str  !! a string representing X as prescribed by PICTUR

      integer(ip), parameter :: wdsize = 32

      character(1) :: fill
      character(1) :: sign
      character(wdsize) :: mystr

      real(wp) :: y

      integer(ip) :: declen
      integer(ip) :: dpat
      integer(ip) :: exp
      integer(ip) :: expsiz
      integer(ip) :: frstch
      integer(ip) :: i
      integer(ip) :: intlen
      integer(ip) :: lastch
      integer(ip) :: firstb
      integer(ip) :: sgnlen
      integer(ip) :: sigdig
      integer(ip) :: sprsiz
      integer(ip) :: start

      logical :: needsn
      logical :: ovflow
      logical :: shift

!
!     Determine where the picture ends.
!
      firstb  = pos( pictur, ' ',  1 )

      if ( firstb == 0 ) then
         lastch = len ( pictur )
      else
         lastch = firstb - 1
      end if

!
!     Make sure there is a picture to worry about.
!
      if ( lastch == 0 ) then

         call ctx%setmsg ( 'The format picture must begin with a '       // &
                       'non-blank character. The picture supplied '  // &
                       'was begun with a blank.'                    )
         call ctx%sigerr ( 'SPICE(NOPICTURE)'                           )
         return

      else if ( lastch == 1 ) then

         if (     pictur == '+' &
             .or. pictur == '-' &
             .or. pictur == '.' ) then

            call ctx%setmsg ( 'Format pictures must have at least ' &
            //            'one significant character. The ' &
            //            'picture provided ''#'' does not. ' )
            call ctx%errch  ( '#', pictur(1:1)                    )
            call ctx%sigerr ( 'SPICE(BADPICTURE)'                 )
            return

         end if

      else if ( lastch == 2 ) then

         if (     pictur == '+.' &
             .or. pictur == '-.' ) then

            call ctx%setmsg ( 'Format pictures must have at least ' &
            //            'one significant character. The ' &
            //            'picture provided ''#'' does not. ' )
            call ctx%errch  ( '#', pictur(1:2)                    )
            call ctx%sigerr ( 'SPICE(BADPICTURE)'                 )
            return
         end if

      else if ( lastch > len(str) ) then

         call ctx%setmsg ( 'The output string is not long enough to ' &
         //            'accommodate a number formatted according ' &
         //            'to the supplied format picture. The ' &
         //            'output string has length #. The output ' &
         //            'picture ''#'' requires # characters. ' )

         call ctx%errint ( '#', len(str) )
         call ctx%errch  ( '#', pictur(1:lastch) )
         call ctx%errint ( '#', lastch )
         call ctx%sigerr ( 'SPICE(OUTPUTTOOSHORT)' )
         return

      end if

!
!     If we get this far, the routine can go ahead and do its business.
!     Determine the sign of X.  Also, determine how many characters
!     are needed to represent the sign if leading sign is suppressed for
!     positive numbers.
!
      if ( x > 0 ) then
         sign   = '+'
         sprsiz =  0
      else if ( x < 0 ) then
         sign   = '-'
         sprsiz = 1
      else
         sign   = ' '
         sprsiz =  0
      end if
!
!     Look at the picture and see if a leading sign is required and
!     if so whether the sign just determined should use a different
!     character and how many characters are needed for the sign.
!
      if      ( pictur(1:1) == '+' ) then

         needsn = .true.
         sgnlen =  1

      else if ( pictur(1:1) == '-' ) then

         needsn = .true.
         sgnlen = 1

         if ( x > 0 ) then
            sign = ' '
         end if

      else

         if ( x > 0 ) then
            sign = ' '
         end if

         needsn = .false.
         sgnlen =  sprsiz

      end if

!
!     If we need a leading sign. The numeric part of the string
!     will start at character 2.  Otherwise it starts at character 1.
!
      if ( needsn ) then
         start    = 2
      else
         start = 1
      end if

!
!     We can set the sign portion of the string now.
!
      str = sign

!
!     Determine what character should be use for leading characters
!     before the first significant character of the output string.
!
      if ( pictur(start:start) == '0' ) then
         fill   = '0'
      else
         fill  = ' '
      end if

!
!     See if there is a decimal point.
!
      dpat   = pos( pictur, '.',  1 )
!
!     The integer part is the stuff to the left of the first
!     decimal point and that follows the sign (if there is one
!     that is explicitly required.  The length of the decimal
!     portion is the stuff to the right of the decimal point.
!
      if ( dpat > 0 ) then

         intlen = dpat - start
         declen = lastch - dpat

      else

         intlen =  lastch - start + 1
         declen = -1

      end if
!
!     If a sign was not explicitly requested by placing it in
!     the first digit of the picture START will be 1.  If in
!     addition X is less than zero ( SGNLEN will be 1 in this
!     case) we have one fewer digits available for the integer
!     portion of the string than is currently set in INTLEN.
!     Adjust INTLEN to reflect the actual number of digits
!     available.
!
!     Also set the SHIFT flag to .TRUE. so that we know to swap
!     the sign and any blanks that might lie between the sign
!     and the first significant character of the output string.
!
      if ( start == 1 .and. sgnlen == 1 ) then
         intlen = intlen - 1
         shift  = .true.
!
!        If INTLEN has become negative (i.e. -1) the picture
!        must be of the form .xxxxx and the input number must
!        be negative. Add 1 back onto the INTLEN but take one
!        away from the decimal length DECLEN.
!
         if ( intlen == -1 ) then
            intlen = 0
            declen = declen - 1

            if (       declen == 0 &
                 .and. intlen == 0 ) then
!
!              There is no room for anything other than a
!              decimal point.  We simply fill the output
!              string with the '*' character.
!
               do i = 1, lastch
                  str(i:i) = '*'
               end do
               return

            end if

         end if

      else
         shift  = .false.
      end if

!
!     Create the "virtual decimal string" associated with the
!     unsigned part of X.
!
      y   = dabs(x)

      call zzvststr ( ctx, y, fill, exp )

!
!     The actual number of digits required to print the unsigned integer
!     portion X is EXP + 1 (provided EXP is at least 0.) We have
!     INTLEN slots available.  So if EXP + 1 is more than INTLEN
!     ( which is equivalent to EXP being at least INTLEN) we don't
!     have enough room to print the unsigned integer portion of the
!     number.
!

      if ( exp >= intlen .and. y /= 0.0_wp ) then
!
!        See if we have room to print an exponential form.
!        First we need the number of characters for the
!        exponent which is always of the form 'E+dd...'
!
         expsiz = 4 + min(1, exp/1000 ) + min(1,exp/100)
!
!        The number of significant digits that can be printed is the
!        size of the picture minus:   the size of the sign
!                                     the size of the exponent
!                                     the size of the decimal point.
!
         sigdig = lastch - sgnlen - expsiz - 1
!
!        If we don't have room for at least one significant digit,
!        there's not much we can do.  Fill the string with '*'.
!

         if ( sigdig < 1 ) then
            do i = 1, lastch
               str(i:i) = '*'
            end do
         else

            call dpstr ( x, sigdig, mystr )
            mystr(1:1) = sign
            str = adjustl(mystr)
            str(1:lastch) = adjustr ( str(1:lastch) )

         end if

         return

      end if
!
!     One more check.  If -INTLEN is greater than DECLEN, or if
!     both are zero, we don't have room to create an output string.
!
      if (  (       intlen == 0 &
              .and. declen == 0 ) &
              .or. -intlen > declen ) then

         do i = 1, lastch
            str(i:i) = '*'
         end do

         return
      end if
!
!     We have a reasonable chance of successfully constructing
!     the string without overflow.
!
      start = sgnlen + 1
      call zzvsbstr ( ctx, -intlen, declen, .true., str(start:), ovflow )

!
!     We might be done at this point.  The IF-THEN block below
!     handles the one snag that could arise.
!
!     If the first digit is a zero as a result of rounding it up
!     OVFLOW will be true.  This means we don't have enough room
!     in the picture for the integer portion of the string.  We try
!     to make an exponential picture.
!
      if ( ovflow ) then
!
!        See if we have room to print an exponential form.
!
         expsiz = 4 + min(1, exp/1000 ) + min(1,exp/100)
!
!        The number of significant digits that can be printed is the
!        size of the picture minus:   the size of the sign
!                                     the size of the exponent
!                                     the size of the decimal point.
!
         sigdig = lastch - sgnlen - expsiz - 1

         if ( sigdig < 1 ) then
            do i = 1, lastch
               str(i:i) = '*'
            end do
         else

            call dpstr ( x, sigdig, mystr )
            mystr(1:1) = sign
            str = adjustl(mystr)
            str(1:lastch) = adjustr ( str(1:lastch) )
            return

         end if

      else if ( shift ) then
!
!        We need to move the sign right until, there are no
!        blanks between it and the next character.
!
         frstch = ncpos ( str, ' -', 1 )

         if ( frstch > 2 ) then
            str(frstch-1:frstch-1) = str(1:1)
            str(1:1)               = ' '
         end if

      end if

      return
      end subroutine dpfmt

      !> Take a double precision number and convert it to an
      !> equivalent formatted character string representation (base 10).
      !>
      !>## Detailed Input
      !>
      !>     X        is a double precision number.
      !>
      !>     SIGDIG   is the number of significant digits that are desired
      !>              for the output string.
      !>
      !>     FORMAT   is a character flag that indicates how the double
      !>              precision number should be represented. The two
      !>              acceptable inputs are 'E' and 'F'.  If the input
      !>              is 'E' then the number will be displayed with an
      !>              exponent in scientific notation. It will have the
      !>              form 'sx.xxx - - - xxxxxEsyy' where there are
      !>              SIGDIG x's and s is ' ' or '-' at its first occurrence
      !>              and '-' or '+' in the second.
      !>
      !>              If the input is 'F' then the number will be
      !>              displayed without an exponent --- the representation
      !>              will be strictly decimal. The first symbol will be
      !>              a sign ('-' or ' ').
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
      !>     digits returned is 14 (in F format there may be many extra
      !>     zeros returned but only a maximum of 14 digits will be
      !>     significant.
      !>
      !>     The output string is left justified.
      !>
      !>     This routine has the advantage that it does not use an internal
      !>     file and is about twice as fast as an internal write. It can
      !>     be used as part of character function without fear of introducing
      !>     recursive I/O conflicts. It is intended to be an approximate
      !>     inverse to the subroutine NPARSD.
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
      !>           1.2454545454545E+01  in E format
      !>           12.454545454545      in F format
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
      !>     Suppose that you wished to insert the character representation
      !>     of some DOUBLE PRECISION number into a line of text.
      !>
      !>     For example suppose X contains the double precision number
      !>     4.268176872928187 and you would like to insert the character
      !>     representation of this number to 2 places between the strings
      !>
      !>     'There are', 'meters between lamp posts'
      !>
      !>     You could perform the following sequence of steps
      !>
      !>           DOUBLE PRECISION  X
      !>           CHARACTER*5       DISTANCE
      !>           CHARACTER*80      MESSAGE
      !>
      !>           CALL DPSTRF ( X, 2, 'F', DISTANCE )
      !>
      !>           MESSAGE = 'There are '                //
      !>          .           DISTANCE                   //
      !>          .          'meters between lamp posts'
      !>          .
      !>
      !>     C
      !>     C     Squeeze any extra spaces out of the message string.
      !>     C
      !>           CALL CMPRSS ( ' ', 1, MESSAGE, MESSAGE )
      !>
      !>     The string MESSAGE would contain:
      !>
      !>          'There are 4.2 meters between lamp posts'
      !>
      !>## Restrictions
      !>
      !>     1)  The maximum number of significant digits returned is 14.
      !>
      !>     2)  If the output string is not declared to be adequately large
      !>         the numeric string will be truncated to the side opposite its
      !>         justification (At least SIGDIG + 6 characters are needed in E
      !>         format, in F format the size required is dependent upon the
      !>         input X and the number of significant digits requested. In
      !>         extreme cases up to 56 characters may be required.)
      !>
      !>     3)  This routine makes explicit use of the format of the string
      !>         returned by DPSTR, should that routine change, substantial
      !>         work may be required to bring this routine back up to snuff.
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
      !>-    SPICELIB Version 1.2.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 17-SEP-1996 (WLT)
      !>
      !>        Upgraded routine to handle arbitrary magnitude d.p. numbers.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUL-1990 (WLT)
      !>
      !>        The routine was repaired so that references to zero-length
      !>        strings ( for example STRING(4:3) ) are not made.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUL-1990 (WLT)
      !>
      !>        As previously implemented, one would occasionally reference
      !>        a zero length substring of the variable NUMSTR. This was
      !>        O.K. under VAX Fortran, because it allows such references.
      !>        However, most implementations of Fortran are not as forgiving.
      module subroutine dpstrf ( ctx, x, sigdig, format, string )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: dpstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! A double precision number
      integer(ip), intent(in) :: sigdig  !! The number of significant digits saved for output
      character(1), intent(in) :: format  !! 'E' for scientific, 'F' for floating point.
      character(*), intent(out) :: string  !! A character string representation of X

      real(wp) :: y

      integer(ip) :: exp
      integer(ip) :: first
      integer(ip) :: last
      integer(ip) :: lastch
      integer(ip) :: maxdig
      integer(ip) :: i
      integer(ip) :: j

      logical :: ovflow

      maxdig = min ( 14, max(1,sigdig)       )

!
!     If the format is 'E' we just let DPSTR handle the problem.
!
      if ( format == 'E' ) then

         call dpstr  ( x,  maxdig, string )
         return

      end if

!
!     If we're still here, we have a decimal format requested.  Set
!     the sign for the number.
!
      if ( x < 0.0_wp ) then
         string = '-'
      else
         string = ' '
      end if
!
!     If X is zero, we can handle this without any regard to the
!     exponent.
!
      if ( x == 0.0_wp ) then

         call zzvststr ( ctx,  x, ' ', exp )
         call zzvsbstr ( ctx, -1, maxdig, .false., string(2:), ovflow )
         return

      end if
!
!     We've already set the sign, now we deal with the unsigned
!     portion of X.
!
      y = dabs(x)

!
!     Create a virtual decimal string for Y.
!
      call zzvststr ( ctx, y, ' ', exp )

!
!     Now we can just fill in the string by reading the appropriate
!     substring from the virtual decimal string.  We need to compute
!     the first and last virtual digits to retrieve.  To do this
!     we look at EXP.
!
      if ( exp >= 0 ) then
         first  = -exp-1
      else
         first  = -exp
      end if

      last   =  first + maxdig - 1

      if ( first < 0 .and. last >= 0 ) then
         last = last + 1
      end if

      first = min ( -1, first )

      call zzvsbstr ( ctx, first, last, .true., string(2:), ovflow )

      if ( ovflow ) then

         first = first - 1

         call zzvsbstr ( ctx, first, last, .true., string(2:), ovflow )
!
!        We need to blank out the last digit of string.
!
         lastch = last - first + 2

         if (       last   > 0 &
              .and. lastch <= len(string) ) then
            string(lastch:) = ' '
         end if

      end if

      if ( last < 0 ) then

         j  = last - first + 3

         do i = last+1, -1

            if ( j <= len(string) ) then
               string(j:j) = '0'
            end if
            j = j + 1

         end do

         if ( j <= len(string) ) then
            string(j:j) = '.'
         end if

      end if

      return

      end subroutine dpstrf

      !> Replace a marker in a string with a formatted double precision
      !> value.
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
      !>     VALUE    is an arbitrary double precision number.
      !>
      !>     SIGDIG   is the number of significant digits with which VALUE is
      !>              to be represented. SIGDIG must be greater than zero and
      !>              less than 15.
      !>
      !>     FORMAT   is the format in which VALUE is to be represented. FORMAT
      !>              may be any of the following:
      !>
      !>                 FORMAT   Meaning                 Example
      !>                 ------   ---------------------   -----------
      !>                 E, e     Scientific (exponent)   3.14159E+03
      !>                          notation
      !>
      !>                 F, f     Fixed-point notation    3141.59
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the string obtained by substituting the text
      !>              representation of VALUE for the first occurrence of
      !>              MARKER in the input string.
      !>
      !>              The text representation of VALUE is in scientific
      !>              (exponent) or fixed-point notation, depending on having
      !>              the value of FORMAT, and having the number of significant
      !>              digits specified by SIGDIG. The representation of VALUE
      !>              is produced by the SPICELIB routine DPSTRF; see that
      !>              routine for details concerning the representation of
      !>              double precision numbers.
      !>
      !>              OUT and IN must be identical or disjoint.
      !>
      !>## Parameters
      !>
      !>     MAXLFD   is the maximum expected length of the text representation
      !>              of a formatted double precision number. 56 characters are
      !>              sufficient to hold any result returned by the SPICELIB
      !>              routine DPSTRF. (See $Restrictions)
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
      !>     3)  If FORMAT is anything other than 'E', this routine defaults
      !>         to 'F'. This is not considered an error.
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
      !>        CALL REPMCT ( STRING, '#1',  51,           'C', STRING )
      !>        CALL REPMC  ( STRING, '#2', '[USER.DATA]',      STRING )
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
      !>     1) The following example illustrate the use of REPMF to
      !>        replace a marker within a string with a formatted double
      !>        precision value.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REPMF_EX1
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
      !>C     1. Single marker, two significant digits, scientific.
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMF ( INSTR, MARKER, 5.0D1, 2, 'E', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 1: Single marker, two significant '
      !>     .        // 'digits, scientific.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     2. Multiple markers, three significant digits,
      !>C        scientific.
      !>C
      !>      MARKER = ' XX '
      !>      INSTR  = 'Left > Right endpoint. Left: XX; Right: XX'
      !>
      !>      CALL REPMF ( INSTR, MARKER, -5.2D-9, 3, 'e', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 2: Multiple markers, 3 significant '
      !>     .        // 'digits, scientific.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     3. Fixed-point notation.
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMF ( INSTR, MARKER, 5.0D1, 3, 'F', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 3: Fixed-point notation.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     4. Fixed-point notation, no decimals.
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMF ( INSTR, MARKER, 5.0D1, 2, 'f', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 4: Fixed-point notation, no decimals.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     5. Excessive significant digits.
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMF ( INSTR, MARKER, 5.0D1, 100, 'F', OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 5: Excessive significant digits.'
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
      !>         Case 1: Single marker, two significant digits, scientific.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  5.0E+01
      !>
      !>         Case 2: Multiple markers, 3 significant digits, scientific.
      !>            Input : Left > Right endpoint. Left: XX; Right: XX
      !>            Output: Left > Right endpoint. Left: -5.20E-09; Right: XX
      !>
      !>         Case 3: Fixed-point notation.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  50.0
      !>
      !>         Case 4: Fixed-point notation, no decimals.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  50.
      !>
      !>         Case 5: Excessive significant digits.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  50.000000000000
      !>
      !>        Note that, in Case #5 even though 100 digits of precision were
      !>        requested, only 14 were returned.
      !>
      !>## Restrictions
      !>
      !>     1)  The maximum number of significant digits returned is 14.
      !>
      !>     2)  This routine makes explicit use of the format of the string
      !>         returned by the SPICELIB routine DPSTRF; should that routine
      !>         change, substantial work may be required to bring this routine
      !>         back up to snuff.
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
      !>-    SPICELIB Version 1.3.0, 03-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing fragments.
      !>
      !>        Added REPML to the list of available replace marker routines in
      !>        $Particulars. Added entry #3 in $Exceptions.
      !>
      !>-    SPICELIB Version 1.2.0, 23-SEP-2013 (BVS)
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
      module subroutine repmf ( ctx, in, marker, value, sigdig, format, out )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: zzrepsub, ucase_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      real(wp), intent(in) :: value  !! Replacement value.
      integer(ip), intent(in) :: sigdig  !! Significant digits in replacement text.
      character(*), intent(in) :: format  !! Format: 'E' or 'F'.
      character(*), intent(out) :: out  !! Output string.

      integer(ip), parameter :: maxlfd = 56

      character(1) :: gdfmt
      character(maxlfd) :: substr

      integer(ip) :: mrknbf
      integer(ip) :: mrknbl

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse

      integer(ip) :: subnbf
      integer(ip) :: subnbl

!
!     If MARKER is blank, no substitution is possible.
!
      if  ( marker == ' ' ) then

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
!     Okay, MARKER is non-blank and has been found. Convert the
!     number to text, and substitute the text for the marker.
!
      gdfmt = adjustl(format)
      call ucase_inplace ( gdfmt )

      call dpstrf  ( ctx, value, sigdig, gdfmt, substr )

      subnbf = verify(substr, ' ')
      subnbl = len_trim(substr)

      if ( subnbf /= 0 .and. subnbl /= 0 ) then

         call zzrepsub ( in, &
                         mrkpsb, &
                         mrkpse, &
                         substr( subnbf:subnbl ), &
                         out  )

      end if

      return
      end subroutine repmf

      !> Convert Year Month and Day on the Julian Calendar
      !> to the Gregorian Calendar
      !>
      !>## Detailed Input
      !>
      !>     YEAR     is an integer representing the year of an epoch, E, in
      !>              the Julian proleptic calendar. Note that the year 0
      !>              and negative values are required to represent
      !>              years in the pre-Christian era (B.C.) A year, Y B.C.,
      !>              should be represented as -(Y-1).  For example the year
      !>              435 B.C. should be input as -434.
      !>
      !>     MONTH    is an integer representing the month of some epoch, E,
      !>              in the Julian proleptic calendar. Months
      !>              outside the usual range from 1 to 12 are converted
      !>              to the standard range using modular arithmetic and
      !>              the input year is adjusted appropriately.
      !>
      !>     DAY      is the day of the month of some epoch, E, in the Julian
      !>              proleptic calendar.
      !>
      !>              Note to input an epoch as the day of a year, set MONTH
      !>              to 1 and DAY to the day of the year.
      !>
      !>## Detailed Output
      !>
      !>     YEAR     is an integer representing the year of the epoch, E,
      !>              above in the Gregorian calendar. Note that the year
      !>              0 (zero) and negative values are used to represent
      !>              years in the pre-Christian era (B.C.) A year, Y B.C.,
      !>              is be represented as -(Y-1).  For example the year
      !>              435 B.C. will be returned as -434.
      !>
      !>     MONTH    is an integer representing the month of the epoch, E,
      !>              above in the Gregorian Calendar calendar.
      !>
      !>     DAY      is the day of the month of the epoch, E, above in the
      !>              Gregorian Calendar
      !>
      !>     DOY      is the day of the year of the epoch, E, above in the
      !>              Gregorian Calendar.
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
      !>     This is a mid-level utility routine to assist in the assignment
      !>     and presentation of ancient epochs.
      !>
      !>     The SPICE software deals primarily with epochs represented on
      !>     in the Gregorian Calendar. However, the Gregorian calendar
      !>     was not adopted until October 15, 1582. As a result, epochs
      !>     prior to that time are usually represented in the Julian
      !>     proleptic calendar.
      !>
      !>     Formally, both calendars can be extended indefinitely forward
      !>     and backward in time due the algorithmic nature of the
      !>     determination of calendar representation.
      !>
      !>     When converting "parsed" calendar epochs in the SPICE system,
      !>     you need to first convert to the Gregorian Calendar. From that
      !>     point the SPICE toolkit can easily convert the epoch to Julian
      !>     date or seconds past the J2000 epoch.
      !>
      !>     This routine allows you to take a numeric representation of
      !>     an epoch represented in the Julian proleptic calendar and
      !>     convert that to an epoch in the Gregorian calendar.
      !>
      !>     To convert from Gregorian Calendar to Julian proleptic
      !>     calendar, use the entry point GR2JUL.
      !>
      !>## Examples
      !>
      !>     Suppose you need to find the epoch (in seconds past the
      !>     J2000) of some ancient epoch that occurred at
      !>     3:00 on March 4 of the year 121 B.C. And that this epoch
      !>     is based on the Julian proleptic calendar. We first need
      !>     to convert the Julian Calendar date to the Gregorian Calendar.
      !>
      !>     Here's the declarations we'll need
      !>
      !>        INTEGER               YEAR
      !>        INTEGER               MONTH
      !>        INTEGER               DAY
      !>        INTEGER               DOY
      !>
      !>        DOUBLE PRECISION      TVEC ( 6 )
      !>        DOUBLE PRECISION      TDB
      !>
      !>     You first need to convert the calendar date of this epoch
      !>     integers. (We don't need to worry about the hours for a moment).
      !>
      !>        YEAR  = -120
      !>        MONTH =  3
      !>        DAY   =  4
      !>
      !>     Convert this Year, Month and Day to the Gregorian Calendar.
      !>
      !>        CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
      !>
      !>     Now construct a time vector for use in the routine TTRANS.
      !>     Note now we use the hour component of the epoch (the fourth
      !>     component of the time vector TVEC).
      !>
      !>        TVEC(1) = DBLE( YEAR )
      !>        TVEC(2) = DBLE( MONTH )
      !>        TVEC(3) = DBLE( DAY )
      !>        TVEC(4) = 3.0D0
      !>        TVEC(5) = 0.0D0
      !>        TVEC(6) = 0.0D0
      !>
      !>     Now the routine TTRANS can convert the time vector from
      !>     the input YMD format to barycentric dynamical time.
      !>
      !>        CALL TTRANS ( 'YDM', 'TDB', TVEC )
      !>
      !>        TDB = TVEC(1)
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
      !>-    SPICELIB Version 1.2.1, 02-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in RMAINI calls.
      !>
      !>-    SPICELIB Version 1.1.1, 23-SEP-1999 (WLT)
      !>
      !>        Removed the unused variable DPMON.
      !>
      !>-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
      !>
      !>        The routine was upgraded so that it will handle without
      !>        error months that are outside the range from 1 to 12.
      !>
      !>-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 23-FEB-1998 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in RMAINI calls.
      module subroutine jul2gr ( ctx, year, month, day, doy )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlti
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: year  !! Year  of Julian Calendar/Gregorian Calendar
      integer(ip), intent(inout) :: month  !! Month of Julian Calendar/Gregorian Calendar
      integer(ip), intent(inout) :: day  !! Day of Month in Julian Calendar/Gregorian Calendar
      integer(ip), intent(out) :: doy  !! Day of Year in Gregorian Calendar
!
!     Local (in-line) Functions
!
      integer(ip) :: divble
      integer(ip) :: gdoy
      integer(ip) :: gdp001
      integer(ip) :: gldays
!
!     Definitions of statement functions.
!
!     The number of days elapsed since Gregorian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      integer(ip) :: gydays
      integer(ip) :: jdoy
      integer(ip) :: jdp001
      integer(ip) :: jldays
      integer(ip) :: jydays
!
!     Local variables
!
      integer(ip) :: dayg
      integer(ip) :: dayj
      integer(ip) :: dofyr
      integer(ip) :: dy
      integer(ip) :: m
      integer(ip) :: m100
      integer(ip) :: m4
      integer(ip) :: m400
      integer(ip) :: mon
      integer(ip) :: offset
      integer(ip) :: tmpday
      integer(ip) :: tmpyr
      integer(ip) :: yr
      integer(ip) :: yroff
!
!     The array EXTRA contains the number of additional days that
!     appear before the first of a month during a leap year (as opposed
!     to a non-leap year).
!
!     Initial values
!
      integer(ip), parameter :: extra  ( 12 ) = &
         [ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
      integer(ip), parameter :: dpbegl ( 12 ) = &
         [ janl0, febl0, marl0, aprl0, mayl0, junl0, jull0, augl0, &
           sepl0, octl0, novl0, decl0 ]
!
!     DPJAN0(I) gives the number of days that occur before the I'th
!     month of a normal year.
!
      integer(ip), parameter :: dpjan0 ( 12 ) = &
         [ jan0, feb0, mar0, apr0, may0, jun0, jul0, aug0, sep0, oct0, &
           nov0, dec0 ]
      gydays(yr) = 365*( yr-1) &
                   +   ((yr-1)/4) &
                   -   ((yr-1)/100) &
                   +   ((yr-1)/400)

!
!     The number of days elapsed since Julian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      jydays(yr) = 365*(yr-1) &
                   +  ((yr-1)/4)

!
!     Return 1 if YR is divisible by M, otherwise return 0.
!
      divble(yr,m) = max ( 0, 1 + ((iabs(yr)/m)*m) - iabs(yr) )

!
!     The number of leap days in a Gregorian year is given by:
!
      gldays(yr) = divble( yr, 4   ) &
                 - divble( yr, 100 ) &
                 + divble( yr, 400 )

!
!     The number of leap days in a Julian year is given by:
!
      jldays(yr) = divble( yr, 4   )

!
!     To compute the day of the year we
!
!        look up the number of days to the beginning of the month,
!
!        add on the number leap days that occurred prior to that
!        time
!
!        add on the number of days into the month
!
      gdoy(yr,mon,dy) = dpjan0 ( mon ) &
                      + extra  ( mon ) * gldays(yr) &
                      + dy

      jdoy(yr,mon,dy) = dpjan0 ( mon ) &
                      + extra  ( mon ) * jldays(yr) &
                      + dy

!
!     The number of days since 1 Jan 1 A.D. (Gregorian) is given by:
!
      gdp001(yr,mon,dy) = gydays(yr) &
                        + gdoy  (yr, mon, dy) &
                        - 1

!
!     The number of days since 1 Jan 1 A.D. (Julianis given by:
!
      jdp001(yr,mon,dy) = jydays(yr) &
                        + jdoy  (yr, mon, dy) &
                        - 1
      if ( .not. allocated ( ctx%jul2gr_state ) ) allocate ( ctx%jul2gr_state )
      associate ( first => ctx%jul2gr_state%first, &
                  rdayg => ctx%jul2gr_state%rdayg, &
                  rdayj => ctx%jul2gr_state%rdayj, &
                  offstj => ctx%jul2gr_state%offstj, &
                  offstg => ctx%jul2gr_state%offstg )
!
!     If this is the first pass through this entry point (or the
!     companion entry point) we need to set up some reference points.
!
!     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
!             calendar of the date Oct 15, 1582
!
!     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
!             calendar of the date Oct 5, 1582.
!
!     OFFSTJ and OFFSTG are just the offset from one count of days
!     to the other.
!
      if ( first ) then

         first  = .false.

         rdayg  = gdp001 ( 1582, 10, 15 )
         rdayj  = jdp001 ( 1582, 10,  5 )
         offstj = rdayj - rdayg
         offstg = rdayg - rdayj

      end if

!
!     Make local copies of the year, month and day.  Then get the
!     YEARs into a positive range.
!
      call rmaini ( ctx, month-1, 12, yroff, mon )

      yr  = year + yroff
      mon = mon + 1
      dy  = day

      if ( yr <= 0 ) then

         call rmaini ( ctx, yr, 4, m4, tmpyr )
         yr = tmpyr

         if ( yr == 0 ) then
            yr = yr + 4
            m4   = m4   - 1
         end if

         offset = m4 * dp4y

      else

         offset = 0

      end if

!
!     First get the day number (Julian) for the input
!     year month and day.
!
      dayj = jdp001 ( yr, mon, dy  ) + offset

!
!     This day is DAYJ - RDAYJ days after 1582 Oct 5 on the
!     julian calendar.  But this is the same as the number
!     of days past 1582 Oct 15 on the Gregorian Calendar
!     So the Gregorian day number is DAYJ - RDAYJ + RDAYG
!     which is the same as DAYJ + OFFSTG.
!
      dayg = dayj + offstg
!
!     Now that we have the Gregorian day number it's a fairly
!     straight forward task to get the year, month and day
!     on the Gregorian calendar.
!

      call rmaini ( ctx, dayg, dp400y, m400, tmpday )
      dayg = tmpday

      m100 = min ( 3, dayg/dp100y )
      dayg = dayg -   m100*dp100y

      m4   = min ( 24, dayg/dp4y  )
      dayg = dayg  -     m4*dp4y

      m    = min (  3, dayg/dp1y )
      dayg = dayg   -     m*dp1y

      dofyr  = dayg + 1
      yr     = 400 * m400 &
             + 100 * m100 &
             +   4 * m4 &
             +       m &
             +       1

!
!     Now look up the month number and compute the day of the month.
!     How we do this depends on whether or not this is a leap year.
!
      if ( gldays(yr) == 0 ) then
         mon  = lstlti ( dofyr, 12, dpjan0 )
         dy   = dofyr  -            dpjan0(mon)
      else
         mon  = lstlti ( dofyr, 12, dpbegl )
         dy   = dofyr  -            dpbegl(mon)
      end if

      year  = yr
      month = mon
      day   = dy
      doy   = dofyr

      return
      end associate
      end subroutine jul2gr

      !> Convert Year Month and Day on the  Gregorian Calendar
      !> to the Julian Calendar
      !>
      !>## Detailed Input
      !>
      !>     YEAR     is an integer representing the year of an epoch, E, in
      !>              the Gregorian calendar. Note that the year 0 (zero)
      !>              and negative values are required to represent
      !>              years in the pre-Christian era (B.C.) A year, Y B.C.
      !>              should be represented as -(Y-1).  For example the year
      !>              435 B.C. should be input as -434.
      !>
      !>     MONTH    is an integer representing the month of some epoch, E,
      !>              in the Gregorian calendar. Months
      !>              outside the usual range from 1 to 12 are converted
      !>              to the standard range using modular arithmetic and
      !>              the input year is adjusted appropriately.
      !>
      !>     DAY      is the day of the month of some epoch, E, in the
      !>              Gregorian calendar.
      !>
      !>              Note to input an epoch as the day of a year, set MONTH
      !>              to 1 and DAY to the day of the year.
      !>
      !>## Detailed Output
      !>
      !>     YEAR     is an integer representing the year of the epoch, E,
      !>              above in the Julian calendar. Note that the year 0
      !>              (zero) and negative values are used to represent
      !>              years in the pre-Christian era (B.C.) A year, Y B.C.,
      !>              is be represented as -(Y-1).  For example the year
      !>              435 B.C. will be returned as -434.
      !>
      !>     MONTH    is an integer representing the month of the epoch, E,
      !>              above in the Julian Calendar calendar.
      !>
      !>     DAY      is the day of the month of the epoch, E, above in the
      !>              Julian Calendar
      !>
      !>     DOY      is the day of the year of the epoch, E, above in the
      !>              Julian Calendar.
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
      !>     This is a mid-level utility routine to assist in the assignment
      !>     and presentation of Ancient epochs.
      !>
      !>     The SPICE software deals primarily with epochs represented on
      !>     in the Gregorian Calendar. However, the Gregorian calendar
      !>     was not adopted until October 15, 1582. As a result, epochs
      !>     prior to that time are usually represented in the Julian
      !>     proleptic calendar.
      !>
      !>     Formally, both calendars can be extended indefinitely forward
      !>     and backward in time due the algorithmic nature of the
      !>     determination of calendar representation.
      !>
      !>     This routine allows you to take a numeric representation of
      !>     an epoch represented in the Gregorian calendar and
      !>     convert that to an epoch in the Julian calendar.
      !>
      !>     To convert from Julian Calendar to Gregorian
      !>     calendar, use the entry point JUL2GR.
      !>
      !>## Examples
      !>
      !>     Suppose you need to print an epoch (given in seconds past the
      !>     J2000 epoch) of some ancient epoch that occurred during
      !>     pre-Christian era, and that you want to represent this epoch
      !>     using the Julian proleptic calendar.
      !>
      !>     Here's the declarations we'll need
      !>
      !>        INTEGER               YEAR
      !>        INTEGER               MONTH
      !>        INTEGER               DAY
      !>        INTEGER               DOY
      !>
      !>        DOUBLE PRECISION      TVEC ( 6 )
      !>        DOUBLE PRECISION      TDB
      !>
      !>     You first need to convert TDB (the epoch in Seconds past J2000)
      !>     to a calendar representation.
      !>
      !>        TVEC(1) = TDB.
      !>
      !>        CALL TTRANS ( 'TDB', 'YMD', TVEC )
      !>
      !>     The output time vector will be relative to the Gregorian
      !>     Calendar. Collect the year, month and day from the time
      !>     vector.
      !>
      !>        YEAR    = INT ( TVEC(1) )
      !>        MONTH   = INT ( TVEC(2) )
      !>        DAY     = INT ( TVEC(3) )
      !>
      !>     The hours, minutes and seconds appear in components 4 through 6
      !>     of the time vector. We can ignore them in the conversion
      !>     of the calendar from Gregorian to Julian.
      !>
      !>        CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
      !>
      !>     Now create a string from the YEAR, MONTH, DAY and TVEC(4)
      !>     through TVEC(6).
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
      !>-    SPICELIB Version 1.1.1, 02-OCT-2021 (JDR)
      !>
      !>        Reordered header sections. Edited the header to comply with
      !>        NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
      !>
      !>        The routine was upgraded so that it will handle without
      !>        error months that are outside the range from 1 to 12.
      !>
      !>-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
      module subroutine gr2jul ( ctx, year, month, day, doy )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlti
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: year  !! Year  of Gregorian Calendar/Julian Calendar
      integer(ip), intent(inout) :: month  !! Month of Gregorian Calendar/Julian Calendar
      integer(ip), intent(inout) :: day  !! Day of Month in Gregorian Calendar/Julian Calendar
      integer(ip), intent(out) :: doy  !! Day of Year in Julian Calendar
      integer(ip) :: divble
      integer(ip) :: gdoy
      integer(ip) :: gdp001
      integer(ip) :: gldays
      integer(ip) :: gydays
      integer(ip) :: jdoy
      integer(ip) :: jdp001
      integer(ip) :: jldays
      integer(ip) :: jydays
      integer(ip) :: dayg
      integer(ip) :: dayj
      integer(ip) :: dofyr
      integer(ip) :: dy
      integer(ip) :: m
      integer(ip) :: m100
      integer(ip) :: m4
      integer(ip) :: m400
      integer(ip) :: mon
      integer(ip) :: offset
      integer(ip) :: tmpday
      integer(ip) :: tmpyr
      integer(ip) :: yr
      integer(ip) :: yroff
      integer(ip), parameter :: extra  ( 12 ) = &
         [ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
      integer(ip), parameter :: dpbegl ( 12 ) = &
         [ janl0, febl0, marl0, aprl0, mayl0, junl0, jull0, augl0, &
           sepl0, octl0, novl0, decl0 ]
      integer(ip), parameter :: dpjan0 ( 12 ) = &
         [ jan0, feb0, mar0, apr0, may0, jun0, jul0, aug0, sep0, oct0, &
           nov0, dec0 ]
      gydays(yr) = 365*( yr-1) &
                   +   ((yr-1)/4) &
                   -   ((yr-1)/100) &
                   +   ((yr-1)/400)

!
!     The number of days elapsed since Julian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      jydays(yr) = 365*(yr-1) &
                   +  ((yr-1)/4)

!
!     Return 1 if YR is divisible by M, otherwise return 0.
!
      divble(yr,m) = max ( 0, 1 + ((iabs(yr)/m)*m) - iabs(yr) )

!
!     The number of leap days in a Gregorian year is given by:
!
      gldays(yr) = divble( yr, 4   ) &
                 - divble( yr, 100 ) &
                 + divble( yr, 400 )

!
!     The number of leap days in a Julian year is given by:
!
      jldays(yr) = divble( yr, 4   )

!
!     To compute the day of the year we
!
!        look up the number of days to the beginning of the month,
!
!        add on the number leap days that occurred prior to that
!        time
!
!        add on the number of days into the month
!
      gdoy(yr,mon,dy) = dpjan0 ( mon ) &
                      + extra  ( mon ) * gldays(yr) &
                      + dy

      jdoy(yr,mon,dy) = dpjan0 ( mon ) &
                      + extra  ( mon ) * jldays(yr) &
                      + dy

!
!     The number of days since 1 Jan 1 A.D. (Gregorian) is given by:
!
      gdp001(yr,mon,dy) = gydays(yr) &
                        + gdoy  (yr, mon, dy) &
                        - 1

!
!     The number of days since 1 Jan 1 A.D. (Julianis given by:
!
      jdp001(yr,mon,dy) = jydays(yr) &
                        + jdoy  (yr, mon, dy) &
                        - 1
      if ( .not. allocated ( ctx%jul2gr_state ) ) allocate ( ctx%jul2gr_state )
      associate ( first => ctx%jul2gr_state%first, &
                  rdayg => ctx%jul2gr_state%rdayg, &
                  rdayj => ctx%jul2gr_state%rdayj, &
                  offstj => ctx%jul2gr_state%offstj, &
                  offstg => ctx%jul2gr_state%offstg )
      if ( first ) then

         first  = .false.

         rdayg  = gdp001 ( 1582, 10, 15 )
         rdayj  = jdp001 ( 1582, 10,  5 )
         offstj = rdayj - rdayg
         offstg = rdayg - rdayj

      end if
!
!     Make Local Copies of YEAR, MONTH and DAY and get YEAR into
!     a positive range.
!
      call rmaini ( ctx, month-1, 12, yroff, mon )

      yr  = year + yroff
      mon = mon  + 1
      dy  = day

      if ( yr <= 0 ) then

         call rmaini ( ctx, yr, 400, m400, tmpyr )
         yr = tmpyr

         if ( yr == 0 ) then
            yr   = yr + 400
            m400 = m400 - 1
         end if

         offset = m400 * dp400y

      else

         offset = 0

      end if

!
!     First get the day number (Gregorian) for the input
!     year month and day.
!
      dayg = gdp001 ( yr, mon, dy  ) + offset

!
!     This day is DAYG - RDAYG days after 1582 Oct 15 on the
!     Gregorian calendar.  But this is the same as the number
!     of days past 1582 Oct 5 on the Julian Calendar
!     So the Julian day number is DAYG - RDAYG + RDAYJ
!     which is the same as DAYG + OFFSTJ.
!
      dayj = dayg + offstj
!
!     Now that we have the Julian day number it's a fairly
!     straight forward task to get the year, month and day
!     on the Julian calendar.
!

      call rmaini ( ctx, dayj, dp4y,  m4,  tmpday )
      dayj = tmpday

      m    = min ( 3, dayj/dp1y )
      dayj = dayj -      m*dp1y

      dofyr  = dayj + 1
      yr     = 4 * m4 &
             +     m &
             +     1

!
!     Now look up the month number and compute the day of the month.
!     How we do this depends on whether or not this is a leap year.
!
      if ( jldays(yr) == 0 ) then
         mon  = lstlti ( dofyr, 12, dpjan0 )
         dy   = dofyr  -            dpjan0(mon)
      else
         mon  = lstlti ( dofyr, 12, dpbegl )
         dy   = dofyr  -            dpbegl(mon)
      end if

      year    = yr
      month   = mon
      day     = dy
      doy     = dofyr

      return
      end associate
      end subroutine gr2jul

      !> Convert from an ephemeris epoch measured in seconds past
      !> the epoch of J2000 to a calendar string format using a
      !> formal calendar free of leapseconds.
      !>
      !>## Detailed Input
      !>
      !>     ET       is an epoch measured in ephemeris seconds
      !>              past the epoch of J2000.
      !>
      !>## Detailed Output
      !>
      !>     CALSTR   is a calendar string representing the input ephemeris
      !>              epoch. This string is based upon extending the
      !>              Gregorian Calendar backward and forward indefinitely
      !>              keeping the same rules for determining leap years.
      !>              Moreover, there is no accounting for leapseconds.
      !>
      !>              To be sure that all of the date can be stored in
      !>              CALSTR, it should be declared to have length at
      !>              least 48 characters.
      !>
      !>              The string will have the following format
      !>
      !>                 year (era) mon day hr:mn:sc.sss
      !>
      !>              Where:
      !>
      !>                 year --- is the year
      !>                 era  --- is the chronological era associated with
      !>                          the date. For years after 999 A.D.
      !>                          the era is omitted. For years
      !>                          between 1 A.D. and 999 A.D. (inclusive)
      !>                          era is the string 'A.D.' For epochs
      !>                          before 1 A.D. Jan 1 00:00:00, era is
      !>                          given as 'B.C.' and the year is converted
      !>                          to years before the "Christian Era".
      !>                          The last B.C. epoch is
      !>
      !>                            1 B.C. DEC 31 23:59:59.999
      !>
      !>                          The first A.D. epoch (which occurs .001
      !>                          seconds after the last B.C. epoch) is:
      !>
      !>                             1 A.D. JAN 1 00:00:00.000
      !>
      !>                          Note: there is no year 0 A.D. or 0 B.C.
      !>                 mon  --- is a 3-letter abbreviation for the month
      !>                          in all capital letters.
      !>                 day  --- is the day of the month
      !>                 hr   --- is the hour of the day (between 0 and 23)
      !>                          leading zeros are added to hr if the
      !>                          numeric value is less than 10.
      !>                 mn   --- is the minute of the hour (0 to 59)
      !>                          leading zeros are added to mn if the
      !>                          numeric value is less than 10.
      !>                 sc.sss   is the second of the minute to 3 decimal
      !>                          places ( 0 to 59.999). Leading zeros
      !>                          are added if the numeric value is less
      !>                          than 10. Seconds are truncated, not
      !>                          rounded.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the input ET is so large that the corresponding
      !>         number of days since 1 A.D. Jan 1, 00:00:00 is
      !>         within 1 of overflowing or underflowing an integer,
      !>         ET will not be converted to the correct string
      !>         representation rather, the string returned will
      !>         state that the epoch was before or after the day
      !>         that is INTMIN +1 or INTMAX - 1 days after
      !>         1 A.D. Jan 1, 00:00:00.
      !>
      !>     2)  If the output string is not sufficiently long to hold
      !>         the full date, it will be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is an error free routine for converting ephemeris epochs
      !>     represented as seconds past the J2000 epoch to formal
      !>     calendar strings based upon the Gregorian Calendar. This formal
      !>     time is often useful when one needs a human recognizable
      !>     form of an ephemeris epoch. There is no accounting for leap
      !>     seconds in the output times produced.
      !>
      !>     Note: The calendar epochs produced are not the same as the
      !>           UTC calendar epochs that correspond to ET. The strings
      !>           produced by this routine may vary from the corresponding
      !>           UTC epochs by more than 1 minute.
      !>
      !>     This routine can be used in creating error messages or
      !>     in routines and programs in which one prefers to report
      !>     times without employing leapseconds to produce exact UTC
      !>     epochs.
      !>
      !>## Examples
      !>
      !>     Suppose you wish to  report that no data is
      !>     available at a particular ephemeris epoch ET. The following
      !>     code shows how you might accomplish this task:
      !>
      !>           CALL DPSTRF ( ET,  6, 'F', ETSTR  )
      !>           CALL ETCAL  ( ET,          CALSTR )
      !>
      !>           E1 = RTRIM   (             CALSTR )
      !>           E2 = RTRIM   (             ETSTR  )
      !>
      !>           WRITE (*,*) 'There is no data available for the body '
      !>           WRITE (*,*) 'at requested time: '
      !>           WRITE (*,*) '   ', CALSTR(1:E1), ' (', ETSTR(1:E2), ')'
      !>
      !>## Restrictions
      !>
      !>     1)  One must keep in mind when using this routine that
      !>         ancient times are not based upon the Gregorian
      !>         calendar. For example the 0 point of the Julian
      !>         Date system is 4713 B.C. Jan 1, 12:00:00 on the Julian
      !>         Calendar. If one formalized the Gregorian calendar
      !>         and extended it indefinitely, the zero point of the Julian
      !>         date system corresponds to 4714 B.C. NOV 24 12:00:00 on
      !>         the Gregorian calendar. There are several reasons for this.
      !>         Leap years in the Julian calendar occur every
      !>         4 years (including *all* centuries). Moreover,  the
      !>         Gregorian calendar "effectively" begins on 15 Oct, 1582 A.D.
      !>         which is 5 Oct, 1582 A.D. in the Julian Calendar.
      !>
      !>         Therefore you must be careful in your interpretation
      !>         of ancient dates produced by this routine.
      !>
      !>## Literature References
      !>
      !>     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
      !>          Clocks, Understanding Time and Frequency," Dover
      !>          Publications, Inc. New York, 1982.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.3.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement. Changed output argument name
      !>        STRING to CALSTR for consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added TIME required reading.
      !>
      !>-    SPICELIB Version 2.2.0, 05-MAR-1998 (WLT)
      !>
      !>        The documentation concerning the appearance of the output
      !>        time string was corrected so that it does not suggest
      !>        a comma is inserted after the day of the month. The
      !>        comma was removed from the output string in Version 2.0.0
      !>        (see the note below) but the documentation was not upgraded
      !>        accordingly.
      !>
      !>-    SPICELIB Version 2.1.0, 20-MAY-1996 (WLT)
      !>
      !>        Two arrays that were initialized but never used were
      !>        removed.
      !>
      !>-    SPICELIB Version 2.0.0, 16-AUG-1995 (KRG)
      !>
      !>        If the day number was less than 10, the spacing was off for
      !>        the rest of the time by one space, that for the "tens" digit.
      !>        This has been fixed by using a leading zero when the number of
      !>        days is < 10.
      !>
      !>        Also, the comma that appeared between the month/day/year
      !>        and the hour:minute:seconds tokens has been removed. This was
      !>        done in order to make the calendar date format of ETCAL
      !>        consistent with the calendar date format of ET2UTC.
      !>
      !>-    SPICELIB Version 1.0.0, 14-DEC-1993 (WLT)
      module subroutine etcal ( ctx, et, calstr )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax, intmin, spd
      use pepper_arrays, only: lstlti
      use pepper_strings, only: intstr, cmprss_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Ephemeris time measured in seconds past J2000.
      character(*), intent(out) :: calstr  !! A standard calendar representation of ET.
      integer(ip), parameter :: strsiz = 16
      integer(ip), parameter :: lngsiz = 180
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: day
      integer(ip) :: doy
      integer(ip) :: dp0001
      integer(ip) :: ldays
      integer(ip) :: month
      integer(ip) :: ydays
      integer(ip) :: year
      integer(ip), parameter :: extra  ( 12 ) = &
         [ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
      integer(ip), parameter :: dpbegl ( 12 ) = &
         [ janl0, febl0, marl0, aprl0, mayl0, junl0, jull0, augl0, &
           sepl0, octl0, novl0, decl0 ]
      integer(ip), parameter :: dpjan0 ( 12 ) = &
         [ jan0, feb0, mar0, apr0, may0, jun0, jul0, aug0, sep0, oct0, &
           nov0, dec0 ]
      ydays(year) = 365*(year-1) &
                    +  ((year-1)/4) &
                    -  ((year-1)/100) &
                    +  ((year-1)/400)

!
!     The number of leap days in a year is given by:
!
      ldays(year) =   ( (year/4  ) * 4   ) / year &
                    - ( (year/100) * 100 ) / year &
                    + ( (year/400) * 400 ) / year

!
!     To compute the day of the year we
!
!        look up the number of days to the beginning of the month,
!
!        add on the number leap days that occurred prior to that
!        time
!
!        add on the number of days into the month
!
      doy(year,month,day) =   dpjan0 ( month ) &
                            + extra  ( month ) * ldays(year) &
                            + day

!
!     The number of days since 1 Jan 1 A.D. is given by:
!
      dp0001(year,month,day) = ydays(year) &
                             + doy  (year, month, day) &
                             - 1
      character(strsiz) :: messge
      integer(ip) :: tsecs
      character(strsiz) :: ystr
      character(strsiz) :: mstr
      character(strsiz) :: era
      integer(ip) :: daynum
      integer(ip) :: yr400
      real(wp) :: frac
      integer(ip) :: bm
      integer(ip) :: bh
      real(wp) :: mynum
      character(strsiz) :: sstr
      real(wp) :: secs
      integer(ip) :: yr100
      character(lngsiz) :: date
      real(wp) :: remd
      integer(ip) :: dofyr
      character(strsiz) :: dstr
      real(wp) :: dp2000
      integer(ip) :: rem
      integer(ip) :: yr4
      integer(ip) :: iq
      integer(ip) :: hours
      real(wp) :: q
      character(strsiz) :: hstr
      integer(ip) :: mins
      logical :: adjust
      real(wp) :: mydnom
      integer(ip) :: yr1
      if ( .not. allocated ( ctx%etcal_state ) ) allocate ( ctx%etcal_state )
      associate ( dmnint => ctx%etcal_state%dmnint, &
                  dmxint => ctx%etcal_state%dmxint, &
                  halfd => ctx%etcal_state%halfd, &
                  secspd => ctx%etcal_state%secspd, &
                  dn2000 => ctx%etcal_state%dn2000, &
                  offset => ctx%etcal_state%offset, &
                  first => ctx%etcal_state%first )
      if ( first ) then

         first  = .false.
         halfd  = spd/2.0_wp
         secspd = spd
         dn2000 = dp0001 ( 2000, 1, 1 )
         dmxint = intmax
         dmnint = intmin

      end if
!
!     Now we "in-line" compute the following call.
!
!        call rmaind ( et + halfd, secspd, dp2000, secs )
!
!     because we can't make a call to rmaind.
!
!     The reader may wonder why we use et + halfd.  The value
!     et is seconds past the ephemeris epoch of J2000 which
!     is at 2000 Jan 1, 12:00:00.  We want to compute days past
!     2000 Jan 1, 00:00:00.  The seconds past THAT epoch is et + halfd.
!     We add on 0.0005 seconds so that the string produced will be
!     rounded to the nearest millisecond.
!
      mydnom = secspd
      mynum  = et + halfd

      q    = dint ( mynum / mydnom )
      remd = mynum -    q * mydnom

      if ( remd < 0.0_wp ) then
         q    = q   - 1.0_wp
         remd = remd + mydnom
      end if

      secs   = remd
      dp2000 = q

!
!     Do something about the problem when ET is vastly
!     out of range.  (Day number outside MAX and MIN integer).
!
      if      ( dp2000 + dn2000 < dmnint + 1) then

         dp2000 = dmnint - dn2000 + 1
         messge = 'Epoch before '
         secs   = 0.0_wp

      else if ( dp2000 + dn2000 > dmxint - 1 ) then

         dp2000 = dmxint - dn2000 - 1
         messge = 'Epoch after '
         secs   = 0.0_wp

      else

         messge = ' '

      end if

!
!     Compute the number of days since 1 .A.D. Jan 1, 00:00:00.
!     From the tests in the previous IF-ELSE IF-ELSE block this
!     addition is guaranteed not to overflow.
!
      daynum = int( dp2000 + real(dn2000, wp) )

!
!     If the number of days is negative, we need to do a little
!     work so that we can represent the date in the B.C. era.
!     We add enough multiples of 400 years so that the year will
!     be positive and then we subtract off the appropriate multiple
!     of 400 years later.
!
      if ( daynum < 0 ) then

!
!        Since we can't make the call below and remain
!        error free, we compute it ourselves.
!
!        call rmaini ( daynum, dp400y, offset, daynum )
!
         iq   = daynum / dp400y
         rem  = daynum - dp400y*iq

         if ( rem < 0 ) then
            iq   = iq   - 1
            rem  = rem  + dp400y
         end if

         offset = iq
         daynum = rem

         adjust = .true.

      else

         adjust = .false.

      end if

!
!     Next we compute the year.  Divide out multiples of 400, 100
!     4 and 1 year.  Finally combine these to get the correct
!     value for year.  (Note this is all integer arithmetic.)
!
!     Recall that DP1Y   =    365
!                 DP4Y   =  4*DPY    + 1
!                 DP100Y = 25*DP4Y   - 1
!                 DP400Y =  4*DP100Y + 1
!
      yr400  =  daynum / dp400y
      rem    =  daynum - dp400y*yr400

      yr100  =  min ( 3, rem   / dp100y )
      rem    =  rem    - yr100 * dp100y

      yr4    =  min (24, rem / dp4y )
      rem    =  rem    - yr4 * dp4y

      yr1    =  min (3,  rem / dp1y )
      rem    =  rem    - yr1 * dp1y

      dofyr  =  rem    + 1

      year   = yr400*400 + yr100*100 + yr4*4 + yr1 + 1
!
!     Get the month, and day of month (depending upon whether
!     we have a leap year or not).
!
      if ( ldays(year) == 0 ) then
         month  = lstlti ( dofyr, 12, dpjan0 )
         day    = dofyr - dpjan0(month)
      else
         month  = lstlti ( dofyr, 12, dpbegl )
         day    = dofyr - dpbegl(month)
      end if
!
!     If we had to adjust the year to make it positive, we now
!     need to correct it and then convert it to a B.C. year.
!
      if ( adjust ) then

         year   =  year + offset*400
         year   = -year + 1
         era    = ' B.C. '

      else
!
!        If the year is less than 1000, we can't just write it
!        out.  We need to add the era.  If we don't do this
!        the dates look very confusing.
!
         if ( year < 1000 ) then
            era   = ' A.D. '
         else
            era   = ' '
         end if

      end if
!
!     Convert Seconds to Hours, Minute and Seconds.
!     We work with thousandths of a second in integer arithmetic
!     so that all of the truncation work with seconds will already
!     be done.  (Note that we already know that SECS is greater than
!     or equal to zero so we'll have no problems with HOURS, MINS
!     or SECS becoming negative.)
!
      tsecs  = int(secs*1000.0_wp)
      frac   = secs - real(tsecs, wp)

      hours  = tsecs / 3600000
      tsecs  = tsecs - 3600000 * hours

      mins   = tsecs / 60000
      tsecs  = tsecs - 60000 * mins

      secs   = real( tsecs, wp ) / 1000.0_wp
!
!     We round seconds if we can do so without getting seconds to be
!     bigger than 60.
!
      if ( secs + 0.0005_wp < 60.0_wp ) then
         secs = secs + 0.0005_wp
      end if

!
!     Finally, get the components of our date string.
!
      call intstr ( year,   ystr )
      if ( day >= 10 ) then
         call intstr ( day, dstr )
      else
         dstr = '0'
         call intstr( day, dstr(2:) )
      end if

!
!     We want to zero pad the hours minutes and seconds.
!
      if ( hours < 10 ) then
         bh = 2
      else
         bh = 1
      end if

      if ( mins < 10 ) then
         bm = 2
      else
         bm = 1
      end if

      mstr = '00'
      hstr = '00'
      sstr = ' '

!
!     Now construct the string components for hours, minutes and
!     seconds.
!
      secs = int(secs*1000.0_wp) / 1000.0_wp

      call intstr ( hours,         hstr(bh:) )
      call intstr ( mins,          mstr(bm:) )
      call dpstrf ( ctx, secs,  6, 'F', sstr      )
!
!     The form of the output for SSTR has a leading blank followed by
!     the first significant digit.  If a decimal point is in the
!     third slot, then SSTR is of the form ' x.xxxxx'  and we need
!     to insert a leading zero.
!
      if ( sstr(3:3) == '.' ) then
         sstr(1:1) = '0'
      end if
!
!     We don't want any leading spaces in SSTR, (HSTR and MSTR don't
!     have leading spaces by construction.
!
      sstr = adjustl(sstr)
!
!     Now form the date string, squeeze out extra spaces and
!     left justify the whole thing.
!
      date = messge &
      //     ystr &
      //     era &
      //     months(month) &
      //     ' ' &
      //     dstr(1:3) &
      //     ' ' &
      //     hstr(1:2) &
      //     ':' &
      //     mstr(1:2) &
      //     ':' &
      //     sstr(1:6)

      call cmprss_inplace ( ' ', 1, date )
      date = adjustl(date)

      calstr = date

      return
      end associate
      end subroutine etcal

      !> Transform a time vector from one representation and system
      !> to another.
      !>
      !>## Detailed Input
      !>
      !>     TVEC     is called a time vector. It is an array of double
      !>              precision numbers that represent some epoch. To
      !>              determine its meaning you must examine the string
      !>              FROM. Note that the number of significant entries
      !>              in TVEC is implied by FROM.
      !>
      !>     FROM,
      !>     TO       are two strings used to describe the type of time vector
      !>              TVEC. FROM is the type of the input vector TVEC and
      !>              TO is the type of the output TVEC
      !>
      !>              The interpretation of TVEC  is as follows:
      !>
      !>                 TYPE      Interpretation of TVEC
      !>                 ------    -------------------------------------------
      !>                 YMD(F)  - year, month, day,   hour, minutes, seconds
      !>                 YD(F)   - year,  day-of-year, hour, minutes, seconds
      !>                 YD.D(F) - year, number of days past beginning of year
      !>                 DAYSEC  - calendar days past 1 jan 1 AD,
      !>                           seconds past beg day
      !>                 DP2000  - calendar days past 1 jan 2000,
      !>                           seconds past beg day
      !>                 JDUTC   - julian date UTC.
      !>                 FORMAL  - seconds in the formal calendar since J2000.
      !>                 YWD(F)  - year, week, day, hour, minutes, seconds
      !>                 YMWD(F) - year, month, week, day, hour, minutes,
      !>                           seconds
      !>                 TAI     - atomic seconds past Atomic J2000.
      !>                 TDT     - Terrestrial Dynamical Time
      !>                 TDB     - Barycentric Dynamical Time
      !>                 JED     - Julian Ephemeris Date (based on TDB)
      !>                 ET      - Ephemeris time (same as TDB)
      !>                 JDTDB   - Julian Date based on TDB (same as JED)
      !>                 JDTDT   - Julian Date based on TDT
      !>
      !>              The number of components of TVEC implied by TYPE is
      !>              as follows:
      !>
      !>                 YMD     - 6
      !>                 YD      - 5
      !>                 JDUTC   - 1
      !>                 FORMAL  - 1
      !>                 YD.D    - 2
      !>                 DAYSEC  - 2
      !>                 DP2000  - 2
      !>                 YWD     - 6
      !>                 YMWD    - 7
      !>                 TAI     - 1
      !>                 TDT     - 1
      !>                 TDB     - 1
      !>                 JED     - 1
      !>                 ET      - 1
      !>                 JDTDB   - 1
      !>                 JDTDT   - 1
      !>
      !>              For all types, only the last component of the
      !>              time vector may be non-integer. If other components
      !>              have fractional parts only their truncated integer
      !>              components will be recognized.
      !>
      !>              YMD and YD
      !>
      !>                 These types are assumed to be different
      !>                 representations on UTC time markers. Thus
      !>                 the hour, minutes and seconds portions all
      !>                 represent time elapsed
      !>                 since the beginning of a day. As such the
      !>                 seconds portion of HMS may range up to (but
      !>                 not include) 61 on days when positive leap
      !>                 seconds occur and may range up to (but not
      !>                 include) 59 on days during which negative
      !>                 leapseconds occur.
      !>
      !>              YD.D type.
      !>
      !>                 Y is the calendar year used in civil time keeping
      !>                 D is the day of the calendar year --- for any time
      !>                   during the first of January, the integer portion
      !>                   of the day will be 1.
      !>
      !>                   The fractional portion is the fractional part of
      !>                   the specific day. Thus the amount of time
      !>                   specified by the fractional portion of the day
      !>                   depends upon whether or not the day has a leap
      !>                   second.  ".D" can be computed from the formula
      !>
      !>                         number of seconds past beginning of day
      !>                   .D = ---------------------------------------
      !>                            number of UTC seconds in the day.
      !>
      !>              FORMAL type.
      !>
      !>                 The FORMAL type for TVEC gives the number of
      !>                 seconds past the epoch J2000 (noon Jan 1 2000)
      !>                 on the formal calendar (no leap seconds ---
      !>                 all days contain 86400 seconds)  The formal clock
      !>                 is simply held still for one second during
      !>                 positive leap seconds. Times during leap seconds
      !>                 cannot be represented in this system.
      !>
      !>                 This system is converted internally to a
      !>                 calendar days past epoch and seconds
      !>                 past beginning of day form. For this reason,
      !>                 times that occur during a positive leap second
      !>                 can never be represented. Moreover, if a negative
      !>                 leapsecond occurs, times that occur during the
      !>                 ``missing'' leapsecond will simply be placed
      !>                 at the beginning of the next day. Thus two
      !>                 different FORMAL times can represent the
      !>                 same time around a negative leap second.
      !>
      !>                 FORMAL time is equivalent to somewhat parochial
      !>                 ``UTC seconds past J2000'' that is produced
      !>                 by the SPICE routine TPARSE.
      !>
      !>              JDUTC type.
      !>
      !>                 This system is similar to the FORMAL system
      !>                 described above. All days are assumed to have
      !>                 86400 seconds. All numbers of the form
      !>
      !>                    integer + 0.5
      !>
      !>                 fall at the beginning of calendar UTC days.
      !>
      !>                 There is no way to represent times during a
      !>                 positive leapsecond. Times during missing
      !>                 negative leap seconds are represented in two ways.
      !>
      !>              DAYSEC type.
      !>
      !>                 This time vector has the form of calendar
      !>                 days since January 1, of the year 1 A.D.
      !>                 and number of seconds past the beginning of the
      !>                 calendar day.
      !>                 (January 2 of the year 1 A.D. is 1 calendar
      !>                 day past January 1, 1 A.D.)
      !>
      !>              DP2000 type.
      !>
      !>                 This time vector has the same form as DAYSEC
      !>                 time vectors. The only difference is that
      !>                 the reference epoch is JAN 1, 2000.
      !>
      !>              YWD and YMWD types.
      !>
      !>                 These time vectors are used to specify a time
      !>                 that are most conveniently expressed by phrases
      !>                 such as "the third Monday of every month" or
      !>                 "Beginning with the second Wednesday of the new
      !>                 year and every 4th Wednesday thereafter."
      !>
      !>                 The hours, minutes and seconds components of
      !>                 these time vectors are the
      !>                 same as for the Year-Month-Day and Year-Day UTC
      !>                 time vectors.
      !>
      !>                 The Y component refers to the calendar year, and
      !>                 in the YMWD vector, the M component refers to
      !>                 the calendar month.
      !>
      !>                 The W component refers to the week of the
      !>                 Year (YWD) or Month (YMWD).  The first week
      !>                 begins on the first day of the year or the first
      !>                 day of the month. The D component is the day of the
      !>                 week with 1 corresponding to Sunday, 2 to Monday,
      !>                 and so on with 7 corresponding to Saturday.
      !>
      !>                 Thus the YMWD time vector
      !>
      !>                    1991
      !>                      11
      !>                       3
      !>                       5
      !>                      12
      !>                       0
      !>                       0
      !>
      !>                 refers to 12:00:00 on the third Thursday of
      !>                 November of 1991.
      !>
      !>                 The YWD time vector
      !>
      !>                    1997
      !>                      11
      !>                       4
      !>                      13
      !>                       5
      !>                      11
      !>
      !>                 refers to 12:05:11 on the eleventh Wednesday
      !>                 of 1997.
      !>
      !>              Formal Calendar Time Vectors
      !>              ============================
      !>              The types YMDF, YDF, YD.D(F), YWDF, YMWDF are similar
      !>              to the corresponding base types: YMD, YD, YD.D, YWD
      !>              and YMWD. However, these types represent formal
      !>              time vectors. Each day contains exactly 86400 seconds.
      !>              The difference between formal and non-formal systems
      !>              can only be seen during a positive leapsecond or
      !>              during the second following a negative leapsecond.
      !>
      !>              Epochs during a positive leapsecond on input are
      !>              placed in the first second of the next day. Epochs
      !>              during a positive leapsecond on output are held
      !>              at 00:00:00 of the next day.
      !>
      !>              Epochs during the first second following a negative
      !>              leapsecond are counted as belonging to the previous
      !>              day if both the input and output types are formal
      !>              types.
      !>
      !>              Calendars
      !>              =====================
      !>              In all time vectors for which a year is specified,
      !>              the year is assumed to belong to the Gregorian
      !>              Calendar---every 4th year is a leapyear except
      !>              for centuries (such as 1900) that are not divisible
      !>              by 400. This calendar is formally extended
      !>              indefinitely backward and forward in time.
      !>
      !>              Note that the Gregorian Calendar did not
      !>              formally exist prior to October 15, 1582. Prior to
      !>              that time the Julian Calendar was used (in the
      !>              Julian Calendar every 4th year is a leapyear, including
      !>              all centuries).
      !>
      !>              If you have epochs relative to the Julian calendar,
      !>              the SPICE routine JUL2GR is available for converting
      !>              to the formal Gregorian Calendar.
      !>
      !>              Epochs Prior to 1972
      !>              =====================
      !>              UTC as it exists today, was adopted in 1972. For
      !>              epochs prior to 1972, it is assumed that the difference
      !>              between TAI and UTC is a constant value.
      !>
      !>              Years prior to 1 A.D.
      !>              =====================
      !>              A year belonging to the B.C. era,  may be
      !>              represented by subtracting the year from 1.
      !>              Thus to specify 27 B.C (Gregorian) set the
      !>              year component of the time vector to -26.
      !>
      !>              Notes:
      !>              ======
      !>              The FORMAL and JDUTC types should not be used
      !>              for times near a leap second. However, for times
      !>              removed from leap seconds they pose no problems.
      !>
      !>              The DAYSEC and DP2000 are useful for representing
      !>              times that are given in atomic seconds past some
      !>              reference epoch other than J2000.
      !>
      !>## Detailed Output
      !>
      !>     TVEC     is the time vector corresponding to the input
      !>              time vector but with components consistent with
      !>              the type specified by input variable TO.
      !>
      !>## Parameters
      !>
      !>     MXCOMP   is the maximum number of components that can appear in
      !>              TVEC.
      !>
      !>## Exceptions
      !>
      !>     1)  If the type of either FROM or TO is not recognized, the
      !>         error SPICE(UNKNONWNTIMESYSTEM) is signaled.
      !>
      !>     2)  If a leapseconds kernel has not been loaded prior a call
      !>         to TTRANS, the error  SPICE(NOLEAPSECONDS) is signaled.
      !>
      !>     3)  If epochs associated with leapseconds in the leapseconds
      !>         kernel are not in increasing order, the error
      !>         SPICE(BADLEAPSECONDS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is the fundamental translator between various
      !>     representations of time in the SPICE system. However, it
      !>     is intended to be a mid-level routine that few user's should
      !>     have need of calling.
      !>
      !>     In addition to translating between time systems, this routine
      !>     can be used to normalize the components of a time string
      !>     so that they are in the normal range for a particular
      !>     representation. This allows you to easily do arithmetic
      !>     with epochs.
      !>
      !>## Examples
      !>
      !>     Suppose you need to convert a time expressed as seconds
      !>     past J2000 (TDB) to Pacific Daylight time. The following
      !>     example shows how you might use TTRANS to accomplish this
      !>     task.
      !>
      !>      TVEC(1) = ET
      !>
      !>      CALL TTRANS ( 'TDB', 'YMD', TVEC )
      !>
      !>      The seconds component of PDT is the same as the seconds
      !>      component of UTC. We save and add the UTC-PDT offset
      !>      to the hours and minutes component of the time vector.
      !>
      !>      SECNDS  = TVEC(6)
      !>      TVEC(6) = 0.0D0
      !>
      !>      TVEC(4) = TVEC(4) - 7.0D0
      !>      TVEC(5) = TVEC(5) + 0.0D0
      !>
      !>      CALL TTRANS ( 'YMDF', 'YMDF', TVEC )
      !>
      !>      Now reset the seconds component to the original value
      !>      and pass the time vector to some formatting routine.
      !>
      !>      TVEC(6) = SECNDS
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.6.0, 05-SEP-2021 (EDW) (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Removed INT casts in HMSSEC calls. The casts prevent
      !>        correct calculation of TDB time for non integer hour
      !>        and minute values in time strings from STR2ET.
      !>
      !>        Removed reference to FURNSH from the "LSK variable
      !>        not present" long error message.
      !>
      !>-    SPICELIB Version 1.5.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 1.4.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 1.3.0, 15-NOV-2006 (NJB)
      !>
      !>        A reference to RTPOOL was replaced by a reference
      !>        to GDPOOL.
      !>
      !>-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in RMAIND and RMAINI calls. Changed reference to LDPOOL to
      !>        reference to FURNSH in an error message.
      !>
      !>-    SPICELIB Version 1.1.0, 09-JUN-1999 (WLT)
      !>
      !>        The routine was modified so that uniform time system
      !>        transformations (see UNITIM) are handled without
      !>        performing intermediate computations. This gives a slight
      !>        improvement in the accuracy of some computations.
      !>
      !>        In addition, two unused variables were removed.
      !>
      !>-    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in RMAIND and RMAINI calls. Changed reference to LDPOOL to
      !>        reference to FURNSH in an error message.
      module subroutine ttrans ( ctx, from, to, tvec )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: j2000, spd
      use pepper_arrays, only: bsrchc, lstled, lstlei, lstlti, orderc, reordc, reordi, reordl
      use pepper_math, only: odd
      use pepper_strings, only: nextwd, ucase_inplace
      use pepper_inc_zzctr
      use pepper_containers, only: grow
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: from  !! description of a time vector.
      character(*), intent(in) :: to  !! description of a time vector.
      real(wp), intent(inout) :: tvec ( mxcomp )  !! time vector representing an epoch.
!
!     Local (in-line) functions
!
      integer(ip) :: divble
      integer(ip) :: doy
      integer(ip) :: dp0001
      integer(ip) :: ldays
!
!     Definitions of statement functions.
!
!     The number of days elapsed since Jan 1, of year 1 A.D. to
!     Jan 1 of YEAR is given by:
!
      integer(ip) :: ydays
      real(wp) :: hmssec
!
!     The following gives us an "enumeration" for all of the
!     various types of time vectors that are recognized.
!
!     DAYSEC
!     DAYP2
!     ET
!     FRML
!     JDTDB
!     JDTDT
!     JDUTC
!     JED
!     TAI
!     TDB
!     TDT
!     YD
!     YDD
!     YDDF
!     YDF
!     YMD
!     YMDF
!     YMWD
!     YMWDF
!     YWD
!     YWDF
!
      integer(ip), parameter :: daysec = 1
      integer(ip), parameter :: dayp2 = daysec + 1
      integer(ip), parameter :: et = dayp2 + 1
      integer(ip), parameter :: frml = et     + 1
      integer(ip), parameter :: jdtdb = frml + 1
      integer(ip), parameter :: jdtdt = jdtdb  + 1
      integer(ip), parameter :: jdutc = jdtdt  + 1
      integer(ip), parameter :: jed = jdutc  + 1
      integer(ip), parameter :: ttai = jed    + 1
      integer(ip), parameter :: tdb = ttai   + 1
      integer(ip), parameter :: tdt = tdb    + 1
      integer(ip), parameter :: yd = tdt    + 1
      integer(ip), parameter :: ydd = yd     + 1
      integer(ip), parameter :: yddf = ydd    + 1
      integer(ip), parameter :: ydf = yddf   + 1
      integer(ip), parameter :: ymd = ydf    + 1
      integer(ip), parameter :: ymdf = ymd    + 1
      integer(ip), parameter :: ymwd = ymdf   + 1
      integer(ip), parameter :: ymwdf = ymwd   + 1
      integer(ip), parameter :: ywd = ymwdf  + 1
      integer(ip), parameter :: ywdf = ywd    + 1
      integer(ip), parameter :: nrec = ywdf
!
!     The following parameters just make the code seem a bit
!     more natural.
!
      logical, parameter :: yes = .true.
!
!     Local variables
!
      real(wp) :: hours
      real(wp) :: mins
      real(wp) :: secs
      integer(ip) :: day
      integer(ip) :: i
      integer(ip) :: month
      integer(ip) :: year
!
!     The array EXTRA contains the number of many additional days that
!     appear before the first of a month during a leap year (as opposed
!     to a non-leap year).
!
!     Initial values
!
      integer(ip), parameter :: extra  ( 12 ) = &
         [ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
      integer(ip), parameter :: dpbegl ( 12 ) = &
         [ janl0, febl0, marl0, aprl0, mayl0, junl0, jull0, augl0, &
           sepl0, octl0, novl0, decl0 ]
!
!     DPJAN0(I) gives the number of days that occur before the I'th
!     month of a normal year.
!
      integer(ip), parameter :: dpjan0 ( 12 ) = &
         [ jan0, feb0, mar0, apr0, may0, jun0, jul0, aug0, sep0, oct0, &
           nov0, dec0 ]
      ydays(year) = 365*(year-1) &
                  +    ((year-1)/4) &
                  -    ((year-1)/100) &
                  +    ((year-1)/400)

!
!     Return 1 if YEAR is divisible by N, otherwise return 0.
!
      divble(year,i) = max ( 0, 1 + ((iabs(year)/i)*i) - iabs(year) )

!
!     The number of leap days in a year is given by:
!
      ldays(year) = divble( year, 4   ) &
                  - divble( year, 100 ) &
                  + divble( year, 400 )

!
!     To compute the day of the year we
!
!        look up the number of days to the beginning of the month,
!
!        add on the number leap days that occurred prior to that
!        time
!
!        add on the number of days into the month
!
      doy(year,month,day) = dpjan0 ( month ) &
                          + extra  ( month ) * ldays(year) &
                          + day

!
!     The number of days since 1 Jan 1 A.D. is given by:
!
      dp0001(year,month,day) = ydays(year) &
                             + doy  (year, month, day) &
                             - 1

!
!     The number of seconds represented by HOURS hours MINS minutes
!     and SECS seconds.
!
      hmssec ( hours, mins, secs ) = hours * 3600.0_wp &
                                   + mins  *   60.0_wp &
                                   + secs
      integer(ip) :: refptr
      character(32) :: myfrom
      real(wp) :: daylen
      integer(ip) :: yr1
      real(wp) :: formal
      real(wp) :: tempd
      integer(ip) :: offset
      integer(ip) :: dpsun
      real(wp) :: dt
      logical :: update
      integer(ip) :: qint
      real(wp) :: lastdt
      integer(ip) :: tempi
      integer(ip) :: yr4
      integer(ip) :: wkday
      integer(ip) :: pfrom
      character(32) :: rest
      integer(ip) :: fyrday
      integer(ip) :: doffst
      integer(ip) :: fmday
      real(wp) :: jdsecs
      integer(ip) :: dayptr
      integer(ip) :: daynum
      real(wp) :: daydp
      integer(ip) :: yr400
      real(wp) :: frac
      integer(ip) :: week
      real(wp) :: exsecs
      integer(ip) :: yr100
      logical :: found
      real(wp) :: dp2000
      integer(ip) :: ordvec  ( nrec )
      integer(ip) :: pto
      character(32) :: myto
      integer(ip) :: taiptr
      integer(ip) :: dyear
      real(wp) :: tai
      character(1) :: dtype
      integer(ip) :: rem
      if ( .not. allocated ( ctx%ttrans_state ) ) allocate ( ctx%ttrans_state )
      associate ( unifrm => ctx%ttrans_state%unifrm, &
                  recog => ctx%ttrans_state%recog, &
                  parsed => ctx%ttrans_state%parsed, &
                  needy => ctx%ttrans_state%needy, &
                  forml => ctx%ttrans_state%forml, &
                  vars => ctx%ttrans_state%vars, &
                  halfd => ctx%ttrans_state%halfd, &
                  jd1101 => ctx%ttrans_state%jd1101, &
                  secspd => ctx%ttrans_state%secspd, &
                  tsecs => ctx%ttrans_state%tsecs, &
                  dn2000 => ctx%ttrans_state%dn2000, &
                  dofyr => ctx%ttrans_state%dofyr, &
                  nref => ctx%ttrans_state%nref, &
                  sunday => ctx%ttrans_state%sunday, &
                  usrctr => ctx%ttrans_state%usrctr, &
                  first => ctx%ttrans_state%first, &
                  nodata => ctx%ttrans_state%nodata )
      if ( ctx%failed() ) return

!
!     The first time any of the entry points are called we
!     must set up the "watcher" for the kernel pool variables
!     that will be needed by this routine.
!
      if ( first ) then

         first   = .false.

         secspd  = spd
         halfd   = spd / 2.0_wp

         dn2000  = dp0001 ( 2000, 1, 1 )
         sunday  = dp0001 ( 1991, 1, 6 )

         jd1101  = j2000 - real(dn2000, wp) - 0.5_wp

!
!        Initialize the list of Uniform time systems.
!
         call ssizec ( ctx, nrec, unifrm )

!
!        Set up the set of recognized time vectors.
!
!        The following 4 parallel arrays are here
!        to assist in the task of classifying the
!        FROM and TO time representations. The arrays
!        contain:
!
!        RECOG   the strings that are recognized as legitimate
!                time representations
!
!        PARSED  a unique integer that can be used to stand
!                for each recognized format.  This is used
!                in the various IF THEN blocks to decide
!                how a time vector should be processed instead
!                of the name because integer compares are
!                much faster than string comparisons.
!
!        FORML   is a logical that indicates whether or not the
!                corresponding time system is a formal system
!                or UTC based system.  FORML(I) = YES implies
!                the time system is formal.  FORML(I) means it
!                isn't.
!
!        NEEDY   is a logical that indicates whether or not
!                there is a YEAR in the time system.  It should
!                be read "NEED Y" for "need year"  not "needy"
!                as when you are destitute.  NEEDY(I) = YES means
!                the time system has a year.  NEEDY(I) = NO means
!                it doesn't
!
         recog ( 01 ) = 'DAYSEC '
         parsed( 01 ) =  daysec
         forml ( 01 ) =  no
         needy ( 01 ) =  no

         recog ( 02 ) = 'DP2000 '
         parsed( 02 ) =  dayp2
         forml ( 02 ) =  no
         needy ( 02 ) =  no

         recog ( 03 ) = 'ET '
         parsed( 03 ) =  et
         forml ( 03 ) =  no
         needy ( 03 ) =  no

         call insrtc ( ctx, 'ET', unifrm )

         recog ( 04 ) = 'FORMAL '
         parsed( 04 ) =  frml
         forml ( 04 ) =  yes
         needy ( 04 ) =  no

         recog ( 05 ) = 'JDTDB '
         parsed( 05 ) =  jdtdb
         forml ( 05 ) =  no
         needy ( 05 ) =  no

         call insrtc ( ctx, 'JDTDB', unifrm )

         recog ( 06 ) = 'JDTDT '
         parsed( 06 ) =  jdtdt
         forml ( 06 ) =  no
         needy ( 06 ) =  no

         call insrtc ( ctx, 'JDTDT', unifrm )

         recog ( 07 ) = 'JDUTC '
         parsed( 07 ) =  jdutc
         forml ( 07 ) =  yes
         needy ( 07 ) =  no

         recog ( 08 ) = 'JED '
         parsed( 08 ) =  jed
         forml ( 08 ) =  no
         needy ( 08 ) =  no

         call insrtc ( ctx, 'JED', unifrm )

         recog ( 09 ) = 'TAI '
         parsed( 09 ) =  ttai
         forml ( 09 ) =  no
         needy ( 09 ) =  no

         call insrtc ( ctx, 'TAI', unifrm )

         recog ( 10 ) = 'TDB '
         parsed( 10 ) =  tdb
         forml ( 10 ) =  no
         needy ( 10 ) =  no

         call insrtc ( ctx, 'TDB', unifrm )

         recog ( 11 ) = 'TDT '
         parsed( 11 ) =  tdt
         forml ( 11 ) =  no
         needy ( 11 ) =  no

         call insrtc ( ctx, 'TDT', unifrm )

         recog ( 12 ) = 'YD '
         parsed( 12 ) =  yd
         forml ( 12 ) =  no
         needy ( 12 ) =  yes

         recog ( 13 ) = 'YD.D '
         parsed( 13 ) =  ydd
         forml ( 13 ) =  no
         needy ( 13 ) =  yes

         recog ( 14 ) = 'YD.DF '
         parsed( 14 ) =  yddf
         forml ( 14 ) =  yes
         needy ( 14 ) =  yes

         recog ( 15 ) = 'YDF '
         parsed( 15 ) =  ydf
         forml ( 15 ) =  yes
         needy ( 15 ) =  yes

         recog ( 16 ) = 'YMD '
         parsed( 16 ) =  ymd
         forml ( 16 ) =  no
         needy ( 16 ) =  yes

         recog ( 17 ) = 'YMDF '
         parsed( 17 ) =  ymdf
         forml ( 17 ) =  yes
         needy ( 17 ) =  yes

         recog ( 18 ) = 'YMWD '
         parsed( 18 ) =  ymwd
         forml ( 18 ) =  no
         needy ( 18 ) =  yes

         recog ( 19 ) = 'YMWDF '
         parsed( 19 ) =  ymwdf
         forml ( 19 ) =  yes
         needy ( 19 ) =  yes

         recog ( 20 ) = 'YWD '
         parsed( 20 ) =  ywd
         forml ( 20 ) =  no
         needy ( 20 ) =  yes

         recog ( 21 ) = 'YWDF '
         parsed( 21 ) =  ywdf
         forml ( 21 ) =  yes
         needy ( 21 ) =  yes

         call orderc ( recog,  nrec, ordvec )
         call reordc ( ordvec, nrec, recog  )
         call reordi ( ordvec, nrec, parsed )
         call reordl ( ordvec, nrec, forml  )
         call reordl ( ordvec, nrec, needy  )

!
!        Initialize the local POOL counter to user value.
!
         call zzctruin( ctx, usrctr )

!
!        Set up the kernel pool watchers
!
         vars(1) = 'DELTET/DELTA_AT'

         call swpool ( ctx, 'TTRANS', 1, vars )

      end if

!
!     Check to see if any of the kernel items required by this
!     routine have been updated since the last call to this
!     entry point.
!
      call zzcvpool ( ctx, 'TTRANS', usrctr, update )

      if ( update .or. nodata ) then
!
!        PEPPER: the tables hold all the leapseconds of the kernel
!        pool (SPICELIB: 140; GDPOOL silently dropped the others).
!
         call dtpool ( ctx, 'DELTET/DELTA_AT', found, nref, dtype )

         if ( .not. found ) nref = 0

         call grow ( ctx%ttrans_state%taitab, max ( nref, 1_ip ) )
         call grow ( ctx%ttrans_state%daytab, max ( nref, 1_ip ) )

         associate ( taitab => ctx%ttrans_state%taitab, &
                     daytab => ctx%ttrans_state%daytab )
!
!        We load the TAI-UTC offsets and formal leapsecond epochs
!        into the TAITAB.  (We will modify this array in a minute).
!
         if ( found ) then
            call gdpool ( ctx, 'DELTET/DELTA_AT',  1,  size( taitab, kind=ip ), &
                          nref,               taitab,  found    )
         end if

!
!        Make sure all of the requested data was there.
!
         if ( .not. found ) then

            nodata = .true.

            call ctx%setmsg ( 'The variable that points to the '          // &
                          'leapseconds (DELTET/DELTA_AT) could not '  // &
                          'be located in the kernel pool.  It is '    // &
                          'likely that the leapseconds kernel has '   // &
                          'not been loaded.'                          )
            call ctx%sigerr ( 'SPICE(NOLEAPSECONDS)'                      )
            return

         end if

!
!        Transform the TAITAB in place to give the TAI time tag
!        at the beginning of the UTC day in which a leap
!        second occurred and the TAI time tag at the beginning
!        of the next day.  Pictorially, the table is transformed
!
!               +----------------------+         +-------------------+
!               | DELTA_1 (TAI to UTC) |         | TAI at start of   |
!               |                      |         | day before TAI-UTC|
!               |                      |         | change occurred   |
!               +----------------------+         +-------------------+
!        from:  | First Formal time    |     to: | TAI time at start |
!               | associated with      |         | of next day UTC.  |
!               | DELTA_1              |         | after DELTA_1 jump|
!               +----------------------+         +-------------------+
!               | DELTA_2 (TAI to UTC) |         | TAI at start of   |
!               |                      |         | day before TAI-UTC|
!               |                      |         | jump occurred     |
!               +----------------------+         +-------------------+
!               | First Formal time    |         | TAI time at start |
!               | associated with      |         | of next day UTC.  |
!               | DELTA_2              |         | after DELTA_2 jump|
!               +----------------------+         +-------------------+
!                        .                                .
!                        .                                .
!                        .                                .
!
!        At the same time, load the table DAYTAB. It contains the
!        the day number past 1 Jan 1 AD for the beginning of the
!        days loaded in TAITAB.
!
         lastdt = taitab(1) - 1.0_wp

         do i = 1, nref, 2

            offset         = i
            refptr         = i + 1

            dt             = taitab(offset)
            formal         = taitab(refptr)
            taitab(offset) = formal - secspd + lastdt
            taitab(refptr) = formal          + dt

            daynum         = int ( ( formal + halfd ) / secspd ) &
                           + dn2000

            daytab(offset) = daynum - 1
            daytab(refptr) = daynum

            lastdt         = dt

         end do

!
!        Since we don't have to do it very often, make sure the
!        times in the TAI table are in increasing order.
!
         do i = 2, nref

            nodata = .true.

            if ( taitab(i-1) >= taitab(i) ) then
               call ctx%setmsg ( 'Either the leapsecond epochs taken '    // &
                             'from the kernel pool are not properly ' // &
                             'ordered or the UTC - TAI offsets '      // &
                             'are completely out of range. '          )
               call ctx%sigerr ( 'SPICE(BADLEAPSECONDS)'                  )
               return
            end if

         end do

!
!        At this point, we've completed all checks on kernel data.
!
         nodata = .false.

         end associate

      end if

      associate ( taitab => ctx%ttrans_state%taitab, &
                  daytab => ctx%ttrans_state%daytab )

!
!     Make local normalized copies of FROM and TO.
!
      call nextwd ( from,   myfrom, rest )
      call nextwd ( to,     myto,   rest )
      call ucase_inplace ( myfrom )
      call ucase_inplace ( myto )

!
!     Make sure that the FROM and TO are recognized time types.
!
      pto   = bsrchc ( myto,   nrec, recog )
      pfrom = bsrchc ( myfrom, nrec, recog )

!
!     Eventually, we need to handle SCLKs.  When that happens
!     we will do it here and in a similarly marked spot at
!     the end of this routine.  First see if we know how to
!     handle the FROM system.
!
!     IF ( PFROM .EQ. 0 ) THEN
!
!        CALL ISSCLK ( FROM,ERROR, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!           IF ( ERROR .NE. ' ' ) THEN
!              CALL SETMSG ( ERROR )
!              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' )
!              CALL CHKOUT ( 'TTRANS' )
!              RETURN
!           END IF
!        ELSE
!           CALL SCLKTV ( FROM, TVEC )
!           PFROM = TDB
!        END IF
!
!     END IF
!
!     Now check to see if we know how to handle the  TO system.
!
!     IF ( PTO .EQ. 0 ) THEN
!
!        CALL ISSCLK ( TO, ERROR, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!
!           IF ( ERROR .NE. ' ' ) THEN
!              CALL SETMSG ( ERROR )
!              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' )
!              CALL CHKOUT ( 'TTRANS' )
!           END IF
!
!        ELSE
!
!           MKSCLK = .TRUE.
!           PTO    =  TDB
!
!        END IF
!
!     END IF
!
!     For now we are NOT going to deal with SCLK so if something
!     isn't recognized, we can just signal an error and quit.
!
      if ( pfrom == 0 ) then

         call ctx%setmsg ( 'The FROM time representation ''#'' is ' &
         //            'not recognized. ' )
         call ctx%errch  ( '#', from                   )
         call ctx%sigerr ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         return

      else if ( pto == 0 ) then

         call ctx%setmsg ( 'The TO time representation ''#'' is ' &
         //            'not recognized. ' )
         call ctx%errch  ( '#', from                   )
         call ctx%sigerr ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         return

      end if

!
!     OK.  We have made our last attempt at diagnosing a user error.
!     From this point on we assume that the user input exactly what
!     was intended.
!
!     We convert the time vector to days past 1 jan 01 and seconds
!     past the beginning of the day.  None of the cases below
!     are particularly tricky.  There's just a lot of cases.
!
      if ( (pfrom == ymd) .or. (pfrom == ymdf) ) then

         year   = int( tvec(1) )
         month  = int( tvec(2) )
         day    = int( tvec(3) )

         call rmaini ( ctx, month-1, 12, dyear, month )

         year   = year  + dyear
         month  = month + 1
         doffst = 0

         if ( year <= 0 ) then

            call rmaini ( ctx, year, 400, yr400, tempi )
            year = tempi

            if ( year == 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year,          month,         day     ) &
                + doffst

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec( tvec(4), tvec(5), tvec(6) )

      else if ( pfrom == yd  .or. pfrom == ydf ) then

         year   = int( tvec(1) )
         day    = int( tvec(2) )
         month  = 1

         doffst = 0

         if ( year <= 0 ) then

            call rmaini ( ctx, year, 400, yr400, tempi )
            year = tempi

            if ( year == 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year,          month,         day     ) &
                + doffst

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec ( tvec(3), tvec(4), tvec(5) )

      else if ( pfrom == ydd .or. pfrom == yddf   ) then

         year   = int(tvec(1))
         day    = int(tvec(2))
         month  = 1

         doffst = 0

         if ( year <= 0 ) then

            call rmaini ( ctx, year, 400, yr400, tempi )
            year = tempi

            if ( year == 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         frac   = tvec(2) - real(day, wp)
         daynum = dp0001  ( year, month, day ) &
                + doffst

!
!        Normally the length of a day is 86400 seconds, but this day
!        might be a leapsecond day.  We will set DAYLEN to SECSPD and
!        change it if it turns out this is a day with a leapsecond.
!
         if ( pfrom == yddf ) then
            secs = frac * secspd
         else
            daylen = secspd
            dayptr = lstlei ( daynum, nref, daytab )

            if ( odd(dayptr) ) then
               daylen = taitab(dayptr+1) - taitab(dayptr)
            end if

            secs = frac * daylen
         end if

      else if ( pfrom == frml ) then

!
!        First lets get the number of days since 1-Jan-2000 00:00:00
!
         call rmaind ( ctx, tvec(1) + halfd, secspd, dp2000, secs )

         daynum = int(dp2000) + dn2000

      else if ( pfrom == jdutc  ) then

!
!        JD1101 is the julian date UTC of Jan 1, 1 AD.
!
         jdsecs = (tvec(1) - jd1101)*secspd

         call rmaind ( ctx, jdsecs, secspd, daydp, secs )

         daynum = int(daydp)

      else if ( pfrom == daysec ) then

         daynum = int(tvec(1))
         secs   = tvec(2)

      else if ( pfrom == dayp2 ) then

         daynum = int(tvec(1)) + dn2000
         secs   = tvec(2)

      else if ( (pfrom == ywd) .or. (pfrom == ywdf) ) then

         year   = int( tvec(1) )
         week   = int( tvec(2) ) - 1
         wkday  = int( tvec(3) )
         month  = 1

!
!        Compute the days past 1 jan 1 of the beginning of this
!        year and month.
!

         doffst = 0

         if ( year <= 0 ) then

            call rmaini ( ctx, year, 400, yr400, tempi )
            year = tempi

            if ( year == 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year, month, 1 ) &
                + doffst

         call rmaini ( ctx, daynum - sunday, 7, qint, dpsun )

         fyrday = dpsun + 1

         call rmaini ( ctx, wkday - fyrday, 7,  qint, offset )

         daynum = daynum + week*7 + offset

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec ( tvec(4), tvec(5), tvec(6) )

      else if ( pfrom == ymwd .or. pfrom == ymwdf ) then

         year   = int( tvec(1) )
         month  = int( tvec(2) )
         week   = int( tvec(3) ) - 1
         day    = int( tvec(4) )

         doffst = 0

         if ( year <= 0 ) then

            call rmaini ( ctx, year, 400, yr400, tempi )
            year = tempi

            if ( year == 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year, month, 1 ) &
                + doffst

         call rmaini ( ctx, daynum - sunday, 7, qint, dpsun  )
         fmday  = dpsun + 1

         call rmaini ( ctx, day - fmday, 7, qint, offset )

         daynum = daynum + week*7 + offset

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec ( tvec(5), tvec(6), tvec(7) )

!
!     If we get to this point the type must be one of the continuous
!     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
!
      else
!
!        If the output time is one of the continuous time systems
!        we can take a short cut and just perform the computation
!        directly.
!
         if ( elemc ( ctx, myto, unifrm ) ) then

            tvec(1) = unitim ( ctx, tvec(1), myfrom, myto )
            return

         end if

!
!        The output time system isn't one of the uniform time systems.
!        Convert what we have to TAI and then to the DAYNUM, SECOND
!        representation.
!
         tai    = unitim ( ctx, tvec(1), myfrom, 'TAI'   )
         taiptr = lstled ( tai,     nref,    taitab )

!
!        If the TAIPTR value is odd, then the TAI time falls during
!        a day with a leap second.  We can just look up the day
!        number and compute the number of seconds into that
!        day directly ...
!
         if ( odd(taiptr) ) then

            daynum = daytab (taiptr)
            secs   = tai &
                   - taitab (taiptr)

!
!        ...Otherwise, all days since the reference TAI time have
!        the same number of seconds (SECSPD).  (This statement applies
!        to days that precede the first reference TAI time too.)
!        Thus we can simply compute the number of days and seconds
!        that have elapsed since the reference TAI time.
!
         else

!
!           If TAI is before the first time in the table, we can
!           compute the number of days and seconds before the first
!           entry in the TAI table.
!
            taiptr = max ( taiptr, 1)

            call  rmaind ( ctx, tai     - taitab(taiptr), secspd, &
                           daydp,                    secs    )

            daynum = int ( daydp ) + daytab(taiptr)

         end if

      end if

      if ( forml ( pfrom )  ) then

         call rmaind ( ctx, secs, secspd, daydp, tsecs )
         daynum = daynum + int(daydp)
         secs   = tsecs

      end if

!     ==================================================================
!
!     Force the seconds into the range 0 to 86401 or 86400
!     depending upon whether or not the output system is a formal
!     time system or not.
!
      if ( forml(pto) .and. forml(pfrom) ) then
!
!        We don't have to do anything here.
!
      else

         if (      (secs > (secspd - 1.0_wp)) &
              .or. (secs < 0.0_wp)          ) then

!
!           First convert to TAI...
!
            dayptr = max( 1, lstlei ( daynum, nref, daytab ) )
            secs   = secs &
                   + real( daynum - daytab(dayptr), wp ) * secspd
            tai    = taitab(dayptr) + secs

!
!           ...then back to DAYNUM and SECS
!
            taiptr = lstled ( tai, nref, taitab )

            if ( odd(taiptr) ) then

               daynum = daytab ( taiptr )
               secs   = tai &
                      - taitab ( taiptr )

            else

               taiptr = max ( 1, taiptr )
               daynum = daytab ( taiptr )

               call rmaind ( ctx, tai - taitab(taiptr), secspd, daydp, secs )

               daynum = daynum + int ( daydp )

            end if
         end if

      end if
!
!     One last thing.  If we are going to a formal time vector,
!     we want to ignore positive leapseconds. (Negative ones
!     were handled above, the clock jumped ahead one second
!     when the second hand got to 59.)
!
!     The idea is that we want the clock
!     to stand still during the leapsecond.  Yeah this is bogus,
!     but people with analog clocks don't have any other choice.
!
!     We are in a positive leapsecond only if SECS is greater than
!     the number of seconds in a normal day.  In that case we
!     increment the day number by one and set SECS to zero.
!

      if ( forml(pto) .and. secs > secspd ) then
         daynum = daynum + 1
         secs   = 0
      end if
!
!     OK. Now we have DAYNUM and SECS,  convert this form to the
!     one requested.
!
!     If there is a 'Y' in the form we are to convert to, then we
!     will need some form of year, etc.  Do the work now and sort it
!     it all out at the appropriate time later on.
!
      if ( needy ( pto )  ) then

         yr400  =  daynum / dp400y
         rem    =  daynum - dp400y*yr400
!
!        We want to be able to deal with years prior to  1 Jan 1
!        So we make sure the remainder is positive.
!
         if ( rem < 0 ) then
            yr400 = yr400 - 1
            rem   = rem + dp400y
         end if

         yr100  =  min ( 3, rem   / dp100y )
         rem    =  rem    - yr100 * dp100y

         yr4    =  min (24, rem / dp4y )
         rem    =  rem    - yr4 * dp4y

         yr1    =  min (3,  rem / dp1y )
         rem    =  rem    - yr1 * dp1y

         dofyr  =  rem    + 1

         year   = yr400*400 + yr100*100 + yr4*4 + yr1 + 1

         if ( ldays(year) == 0 ) then
            month  = lstlti ( dofyr, 12, dpjan0 )
            day    = dofyr - dpjan0(month)
         else
            month  = lstlti ( dofyr, 12, dpbegl )
            day    = dofyr - dpbegl(month)
         end if

!
!        We only want to convert that portion of seconds less than
!        86399 to hours, minutes and seconds.  Take anything extra
!        and put it in EXSECS.
!
         exsecs = max ( 0.0_wp, secs - secspd + 1 )
         tsecs  = secs - exsecs

         call rmaind ( ctx, tsecs, 3600.0_wp, hours, tempd )
         call rmaind ( ctx, tempd,   60.0_wp, mins,  tsecs )

         tsecs  = tsecs + exsecs

      end if

!=====================================================================
!
!     Finally, we convert to the requested output.
!
      if      ( pto == ymd .or. pto == ymdf ) then

         tvec(1) = real( year, wp  )
         tvec(2) = real( month, wp )
         tvec(3) = real( day, wp   )
         tvec(4) = hours
         tvec(5) = mins
         tvec(6) = tsecs

      else if ( pto == yd .or. pto == ydf  ) then

         tvec(1) = real( year, wp  )
         tvec(2) = real( dofyr, wp )
         tvec(3) = hours
         tvec(4) = mins
         tvec(5) = tsecs

      else if ( pto == ydd .or. pto == yddf   ) then

         tvec(1) = real( year, wp  )

         if ( pto == ydd ) then
            dayptr  = lstlei ( daynum, nref, daytab )
            daylen  = secspd

            if ( odd(dayptr) ) then
               daylen = taitab(dayptr+1) - taitab(dayptr)
            end if

            tvec(2) = real(dofyr, wp) + secs/daylen
         else
            tvec(2) = real(dofyr, wp) + secs/secspd
         end if

      else if ( pto == frml ) then

         tvec(1) = real( daynum - dn2000, wp ) * secspd &
                 - halfd &
                 + secs

      else if ( pto == jdutc  ) then

         tvec(1) = jd1101 + real( daynum, wp ) + secs/secspd

      else if ( pto == daysec ) then

         tvec(1) = real(daynum, wp)
         tvec(2) = secs

      else if ( pto == dayp2 ) then

         tvec(1) = real(daynum - dn2000, wp)
         tvec(2) = secs

      else if ( pto  == ywd .or. pto == ywdf ) then
!
!        First compute the day of the week, and the week number
!
         call   rmaini  ( ctx, daynum - sunday, 7, qint, day )
         week    = 1 + (( dofyr  - 1 )   / 7)

!
!        Now just put everything where it belongs.
!
         tvec(1) = real(year, wp)
         tvec(2) = real(week, wp)
         tvec(3) = real(day, wp) + 1.0_wp
         tvec(4) = hours
         tvec(5) = mins
         tvec(6) = tsecs

      else if ( pto == ymwd .or. pto == ymwdf ) then

!
!        First compute how many weeks into the month DAYNUM is,
!        and compute the day of week number.
!
         tvec(1) = real(year, wp)
         doffst  = 0

         if ( year <= 0 ) then

            call rmaini ( ctx, year, 400, yr400, tempi )
            year = tempi

            if ( year == 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         week    = 1 + ( daynum - dp0001(year, month, 1) - doffst ) / 7
         call rmaini   ( ctx, daynum - sunday,   7, qint, day )

!
!        Now just move the remaining stuff into TVEC.
!
         tvec(2) = real(month, wp)
         tvec(3) = real(week, wp)
         tvec(4) = real(day, wp) + 1.0_wp
         tvec(5) = hours
         tvec(6) = mins
         tvec(7) = tsecs

!
!     If we get to this point the type must be one of the continuous
!     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
!
!     First convert to TAI and then to the appropriate output type.
!
      else

         dayptr  = max( 1, lstlei ( daynum, nref, daytab ) )
         secs    = secs &
                 + real( daynum - daytab(dayptr), wp ) * secspd
         tai     = taitab(dayptr) + secs

         tvec(1) = unitim ( ctx, tai, 'TAI', myto )

      end if

!
!     Here's where we will handle conversion to SCLK when
!     we get around to implementing that portion of TTRANS
!
!     IF ( MKSCLK ) THEN
!        CALL TVSCLK ( TO, TVEC )
!     END IF
!
!     END IF
!

      return
      end associate
      end associate
      end subroutine ttrans

      !> Set and retrieve the defaults associated with calendar
      !> input strings.
      !>
      !>## Detailed Input
      !>
      !>     ACTION   is a word that specifies whether TIMDEF sets the
      !>              value associated with ITEM or retrieves the value
      !>              associated with ITEM. The allowed values for
      !>              ACTION are 'SET' and 'GET'. The routine is not
      !>              sensitive to the case of the letters in ACTION.
      !>
      !>     ITEM     is the default items whose value should be set or
      !>              retrieved. The items that may be requested are:
      !>
      !>                 ITEM        Allowed Values
      !>                 ---------   --------------
      !>                 CALENDAR    GREGORIAN
      !>                             JULIAN
      !>                             MIXED
      !>
      !>                 SYSTEM      TDB
      !>                             TDT
      !>                             TT
      !>                             UTC
      !>
      !>                 ZONE        EST, EDT, CST, CDT, MST, MDT, PST, PDT
      !>                             UTC+HR
      !>                             UTC-HR       ( 0 <= HR < 13 )
      !>                             UTC+HR:MN    ( 0 <= MN < 60 )
      !>                             UTC-HR:MN
      !>
      !>              The case of ITEM is not significant.
      !>
      !>     VALUE    if the action is 'SET' then VALUE is an input and
      !>              is the value to be associated with ITEM. Note that
      !>              VALUE is checked to ensure it is within the range
      !>              of allowed values for ITEM. If it is not within
      !>              the expected range and appropriate error message
      !>              is signaled. The case of VALUE is not significant.
      !>
      !>## Detailed Output
      !>
      !>     VALUE    if the action is 'GET' then VALUE will be the
      !>              value associated with the requested ITEM. Note that
      !>              when time zones are set, they are translated to the
      !>              UTC offset form ( UTC(+/-)HR[:MN] ). When VALUE is
      !>              an output it will be in upper case.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the ACTION specified is not 'SET' or 'GET', the error
      !>         SPICE(BADACTION) is signaled.
      !>
      !>     2)  If the ITEM specified is not one the recognized items,
      !>         the error SPICE(BADTIMEITEM) is signaled.
      !>
      !>     3)  If the value associated with a 'SET' item input
      !>         is not one of the recognized items, the error
      !>         SPICE(BADDEFAULTVALUE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine exists to allow SPICE toolkit users to alter
      !>     the default interpretation of time strings made by the
      !>     routine STR2ET.
      !>
      !>     Normally, unlabelled time strings are assumed to belong to
      !>     the Gregorian Calendar and are UTC times. However, you
      !>     may alter the default behavior by calling TIMDEF.
      !>
      !>     Calendar
      !>     --------
      !>
      !>     You may set the calendar to be one of the following
      !>
      !>     Gregorian   --- This is the calendar used daily the
      !>                     Western Hemisphere. Leap years occur in this
      !>                     calendar every 4 years except on centuries
      !>                     such as 1900 that are not divisible by 400.
      !>
      !>     Julian      --- This is the calendar that was in use prior
      !>                     to October 15, 1582. Leap years occur every
      !>                     4 years on the Julian Calendar (including all
      !>                     centuries.) October 5, 1582 on the Julian
      !>                     calendar corresponds to October 15, 1582 of the
      !>                     Gregorian Calendar.
      !>
      !>     Mixed       --- This calendar uses the Julian calendar
      !>                     for days prior to October 15, 1582 and
      !>                     the Gregorian calendar for days on or after
      !>                     October 15, 1582.
      !>
      !>     To set the default calendar, select on of the above for VALUE
      !>     and make the following call.
      !>
      !>        CALL TIMDEF ( 'SET', 'CALENDAR', VALUE )
      !>
      !>     System
      !>     -------
      !>
      !>     You may set the system used for keeping time to be UTC (default)
      !>     TDB (barycentric Dynamical Time), TDT (Terrestrial Dynamical
      !>     Time), or TT (Terrestrial Time). TDT and TT represent the same
      !>     time system. Both TDB and TT (TDT) have no leapseconds. As such
      !>     the time elapsed between any two epochs on these calendars does
      !>     not depend upon when leapseconds occur.
      !>
      !>     To set the default time system, select TDT, TT, TDB or UTC for
      !>     VALUE and make the following call.
      !>
      !>        CALL TIMDEF ( 'SET', 'SYSTEM', VALUE )
      !>
      !>     Note that such a call has the side effect of setting the value
      !>     associated with ZONE to a blank.
      !>
      !>     Zone
      !>     -----
      !>
      !>     You may alter the UTC system by specifying a time zone (UTC
      !>     offset). For example you may specify that epochs are referred
      !>     to Pacific Standard Time (PST --- UTC-7). The standard
      !>     abbreviations for U.S. time zones are recognized:
      !>
      !>        EST   UTC-5
      !>        EDT   UTC-4
      !>        CST   UTC-6
      !>        CDT   UTC-5
      !>        MST   UTC-7
      !>        MDT   UTC-6
      !>        PST   UTC-8
      !>        PDT   UTC-7
      !>
      !>     In addition you may specify any commercial time zone by using
      !>     "offset" notation. This notation starts with the letters 'UTC'
      !>     followed by a + for time zones east of Greenwich and - for time
      !>     zones west of Greenwich. This is followed by the number of hours
      !>     to add or subtract from UTC. This is optionally followed by a
      !>     colon ':' and the number of minutes to add or subtract (based on
      !>     the sign that follows 'UTC') to get the local time zone. Thus to
      !>     specify the time zone of Calcutta you would specify the time zone
      !>     to be UTC+5:30. To specify the time zone of Newfoundland use the
      !>     time zone UTC-3:30.
      !>
      !>     To set a default time zone, select one of the "built-in" U.S.
      !>     zones or construct an offset as discussed above. Then make the
      !>     call
      !>
      !>        CALL TIMDEF ( 'SET', 'ZONE', VALUE )
      !>
      !>     If you 'GET' a 'ZONE' it will either be blank, or have the
      !>     form 'UTC+/-HR[:MN]'
      !>
      !>     Note that such a call has the side effect of setting the value
      !>     associated with SYSTEM to a blank.
      !>
      !>## Examples
      !>
      !>     Suppose you wish to modify the behavior of STR2ET so that
      !>     it interprets unlabeled time strings as being times in
      !>     Pacific Daylight Time and that you want the calendar to use
      !>     to be the "Mixed" calendar. The following two calls will
      !>     make the desired changes to the behavior of STR2ET
      !>
      !>         CALL TIMDEF ( 'SET', 'CALENDAR', 'MIXED' )
      !>         CALL TIMDEF ( 'SET', 'ZONE',     'PDT'   )
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
      !>-    SPICELIB Version 1.3.0, 14-OCT-2021 (EDW) (JDR)
      !>
      !>        UCASE and LJUST called on VALUE only in 'SET' block.
      !>
      !>        Add time system name 'TT' (Terrestrial Time) as alternate
      !>        assignment of 'TDT' (Terrestrial Dynamical Time).
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 26-MAY-1998 (WLT)
      !>
      !>        The previous version did not check out and return
      !>        when an error was detected in the if block that
      !>        begins with
      !>
      !>           ELSE IF ( MYITEM .EQ. 'ZONE' ) THEN
      !>
      !>        The routine did eventually check out and return so
      !>        that the trace stack was maintained correctly, but
      !>        the default time zone would be modified which was not
      !>        the desired behavior.
      !>
      !>-    SPICELIB Version 1.1.0, 27-JUN-1997 (WLT)
      !>
      !>        The previous version failed to check out when
      !>        the default value was set.
      !>
      !>-    SPICELIB Version 1.0.0, 13-NOV-1996 (WLT)
      module subroutine timdef ( ctx, action, item, value )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: prefix, ucase_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: action  !! Kind of action to take 'SET' or 'GET'.
      character(*), intent(in) :: item  !! Default item of interest.
      character(*), intent(inout) :: value  !! Value associated with the default item.
      integer(ip), parameter :: wdsize = 16
      integer(ip), parameter :: nzones = 8
      character(wdsize), parameter :: trnslt ( nzones ) = &
         [ character(wdsize) :: 'UTC-5', 'UTC-4', 'UTC-6', 'UTC-5', 'UTC-7', 'UTC-6', &
           'UTC-8', 'UTC-7' ]
      logical :: succes
      integer(ip) :: zone
      integer(ip) :: last
      real(wp) :: hoff
      character(wdsize) :: myval
      character(wdsize) :: myitem
      character(wdsize) :: myactn
      real(wp) :: moff
      associate ( defzon => ctx%spice%timdef_state%defzon, &
                  defsys => ctx%spice%timdef_state%defsys, &
                  defcal => ctx%spice%timdef_state%defcal, &
                  zones => ctx%spice%timdef_state%zones )
      if ( ctx%failed() ) return

!
!     Normalize the input.
!
      myactn = adjustl(action)
      call ucase_inplace ( myactn )

      myitem = adjustl(item)
      call ucase_inplace ( myitem )

!
!     Admittedly, the decision making below is not very elegant.
!     However, this works and is simpler than anything that comes
!     to mind at the moment and allows us to give quite specific
!     diagnostic messages easily.
!
      if ( myactn == 'SET' ) then

         myval = adjustl(value)
         call ucase_inplace ( myval )

         if      ( myitem == 'SYSTEM' ) then

!
!           Recognize only the know time systems. Note, 'UTC' not
!           actually a time system, more a time representation.
!
            if (       (myval == 'TDB') &
                  .or. (myval == 'TDT') &
                  .or. (myval == 'TT') &
                  .or. (myval == 'UTC') ) then

               defzon = ' '
               defsys = myval

            else
               call ctx%setmsg ( 'The default value assigned to the ' &
               //            'time system must be one of ''UTC'', ' &
               //            '''TDT'', ''TT'', or ''TDB''. The value ' &
               //            'supplied was ''#''. ' )

               call ctx%errch  ( '#', value )
               call ctx%sigerr ( 'SPICE(BADDEFAULTVALUE)'  )
               return
            end if

         else if ( myitem == 'ZONE' ) then

            zone = findloc(zones, myval, dim=1)

!
!           If MYVAL was one of the recognized time zones, we
!           translate it to the UTC offset form.
!
            if ( zone > 0 ) then
               myval = trnslt ( zone )
            end if

            call prefix  ( '::',  0, myval                    )
            call zzutcpm ( ctx, myval, 1, hoff, moff, last, succes )

            if ( .not. succes ) then

               call ctx%setmsg ( 'The input value for a time zone ' &
               //            '"#" was not recognized as known ' &
               //            'time zone and could not be parsed ' &
               //            'according to the pattern ' &
               //            'UTC(+/-)HR[:MN]. Known time zones ' &
               //            'are: ''EST'', ''EDT'', ''CST'', ' &
               //            '''CDT'', ''MST'', ''MDT'', ' &
               //            '''PST'', and ''PDT''. ' )

               call ctx%errch  ( '#', value               )
               call ctx%sigerr ( 'SPICE(BADDEFAULTVALUE)' )
               return

            end if

            defzon = myval(3:)
            defsys = ' '

         else if ( myitem == 'CALENDAR' )  then

            if (       myval == 'JULIAN' &
                  .or. myval == 'GREGORIAN' &
                  .or. myval == 'MIXED'   ) then

               defcal = myval

            else

               call ctx%setmsg ( 'The input value for ''#'' is not a ' &
               //            'recognized calendar type.  The ' &
               //            'recognized calendars are ' &
               //            '''GREGORIAN'', ''JULIAN'', and ' &
               //            '''MIXED''. ' )
               call ctx%errch  ( '#', value    )
               call ctx%sigerr ( 'SPICE(BADDEFAULTVALUE)' )
               return

            end if

         else

            call ctx%setmsg ( 'The specified item ''#'' is not a ' &
            //            'recognized time default item.  The ' &
            //            'items that you may "SET" via the ' &
            //            'routine TIMDEF are ''CALENDAR'', ' &
            //            '''SYSTEM'', or ''ZONE'' ' )

            call ctx%errch  ( '#', item            )
            call ctx%sigerr ( 'SPICE(BADTIMEITEM)' )
            return

         end if

         return

      else if ( myactn == 'GET' ) then

         if      ( myitem == 'CALENDAR' ) then
            value = defcal
         else if ( myitem == 'SYSTEM'   ) then
            value = defsys
         else if ( myitem == 'ZONE'     ) then
            value = defzon
         else

            call ctx%setmsg ( 'The specified item ''#'' is not a ' &
            //            'recognized time default item.  The ' &
            //            'items that you may "SET" via the ' &
            //            'routine TIMDEF are ''CALENDAR'', ' &
            //            '''SYSTEM'', or ''ZONE'' ' )
            call ctx%errch  ( '#', item )
            call ctx%sigerr ( 'SPICE(BADTIMEITEM)' )
            return

         end if

      else

         call ctx%setmsg ( 'The action specified to TIMDEF was ' &
         //            '''#''.  This is not a recognized action. ' &
         //            'The recognized actions are ''SET'' and ' &
         //            '''GET''. ' )

         call ctx%errch  ( '#', action )
         call ctx%sigerr ( 'SPICE(BADACTION)' )
         return

      end if

      return
      end associate
      end subroutine timdef

      !> Transform time from one uniform scale to another. The uniform
      !> time scales are TAI, GPS, TT, TDT, TDB, ET, JED, JDTDB, JDTDT.
      !>
      !> The function returns the d.p. in OUTSYS that is equivalent to the
      !> EPOCH on the INSYS time scale.
      !>
      !>## Detailed Input
      !>
      !>     EPOCH    is an epoch relative to the INSYS time scale.
      !>
      !>     INSYS    is a time scale. Acceptable values are:
      !>
      !>                 'TAI'     International Atomic Time.
      !>                 'TDB'     Barycentric Dynamical Time.
      !>                 'TDT'     Terrestrial Dynamical Time.
      !>                 'TT'      Terrestrial Time, identical to TDT.
      !>                 'ET'      Ephemeris time (in the SPICE system, this is
      !>                           equivalent to TDB).
      !>                 'JDTDB'   Julian Date relative to TDB.
      !>                 'JDTDT'   Julian Date relative to TDT.
      !>                 'JED'     Julian Ephemeris date (in the SPICE system
      !>                           this is equivalent to JDTDB).
      !>                 'GPS'     Global Positioning System Time.
      !>
      !>              The routine is not sensitive to the case of the
      !>              characters in INSYS; 'tai' 'Tai' and 'TAI' are all
      !>              equivalent from the point of view of this routine.
      !>
      !>     OUTSYS   is the time scale to which EPOCH should be converted.
      !>              Acceptable values are the same as for INSYS. The
      !>              routine is not sensitive to the case of OUTSYS.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the time in the system specified by OUTSYS
      !>     that is equivalent to the EPOCH in the INSYS time scale.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The kernel pool must contain the variables:
      !>
      !>            'DELTET/DELTA_T_A'
      !>            'DELTET/K'
      !>            'DELTET/EB'
      !>            'DELTET/M'
      !>
      !>         If these are not present, the error SPICE(MISSINGTIMEINFO) is
      !>         signaled. (These variables are typically inserted into the
      !>         kernel pool by loading a leapseconds kernel with the SPICE
      !>         routine FURNSH.)
      !>
      !>     2)  If the names of either the input or output time types are
      !>         unrecognized, the error SPICE(BADTIMETYPE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     We use the term uniform time scale to refer to those
      !>     representations of time that are numeric (each epoch is
      !>     represented by a number) and additive. A numeric time system is
      !>     additive if given the representations, E1 and E2, of any pair of
      !>     successive epochs, the time elapsed between the epochs is given by
      !>     E2 - E1.
      !>
      !>     Given an epoch in one of the uniform time scales specified by
      !>     INSYS, the function returns the equivalent representation in the
      !>     scale specified by OUTSYS. A list of the recognized uniform time
      !>     scales is given in the detailed input for INSYS.
      !>
      !>## Examples
      !>
      !>     To convert an epoch with respect to the International Atomic
      !>     Time (TAI) scale to ET (Barycentric Dynamical Time), make the
      !>     following assignment.
      !>
      !>           ET = UNITIM ( TAI, 'TAI', 'ET' )
      !>
      !>## Restrictions
      !>
      !>     1)  The appropriate variable must be loaded into the SPICE kernel
      !>         pool (normally by loading a leapseconds kernel with FURNSH)
      !>         prior to calling this routine.
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.5.0, 05-SEP-2020 (EDW) (JDR)
      !>
      !>        Added time system name 'TT' (Terrestrial Time) as alternate
      !>        assignment of 'TDT' (Terrestrial Dynamical Time).
      !>
      !>        Included GPS time system mapping.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Removed references to FURNSH, CLPOOL, KCLEAR, UNLOAD, and
      !>        Required Reading documents and tutorials from the "variables
      !>        not present" long error message.
      !>
      !>-    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB)
      !>
      !>        This routine now keeps track of whether its kernel pool
      !>        look-up failed. If so, a kernel pool lookup is attempted on
      !>        the next call to this routine. This change is an enhancement,
      !>        not a bug fix (unlike similar modifications in SCLK routines).
      !>
      !>-    SPICELIB Version 1.2.1, 15-NOV-2006 (EDW) (NJB)
      !>
      !>        Replaced references to LDPOOL with references to FURNSH.
      !>        Replaced references to RTPOOL with references to GDPOOL.
      !>        Enhanced long error message associated with missing kernel
      !>        variables.
      !>
      !>-    SPICELIB Version 1.2.0, 17-FEB-1999 (WLT)
      !>
      !>        Added a second call to SWPOOL in the event some required
      !>        kernel pool variable is not supplied.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of
      !>        the function.
      !>
      !>-    SPICELIB Version 1.0.0, 28-MAR-1992 (WLT)
      module real(wp) function unitim ( ctx, epoch, insys, outsys )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: j2000, spd
      use pepper_strings, only: ucase
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: epoch  !! An epoch.
      character(*), intent(in) :: insys  !! The time scale associated with the input EPOCH.
      character(*), intent(in) :: outsys  !! The time scale associated with the function value.
      real(wp) :: eb_1(1)  !! one-element copy of eb (array argument)
      real(wp) :: k_1(1)  !! one-element copy of k (array argument)
      real(wp) :: dta_1(1)  !! one-element copy of dta (array argument)
      integer(ip), parameter :: needed = 4
      integer(ip), parameter :: typlen = 8
      integer(ip), parameter :: ntdt = 5
      integer(ip), parameter :: ntdb = 4
      real(wp), parameter :: gpstai = 19.0_wp
      character(typlen) :: myin
      character(typlen) :: myout
      character(typlen) :: types  ( lbcell : 2         )
      real(wp) :: mytime
      real(wp) :: tdb
      real(wp) :: tdt
      integer(ip) :: i
      integer(ip) :: n
      logical :: found    ( needed )
      logical :: intdb
      logical :: intdt
      logical :: outtdb
      logical :: outtdt
      logical :: update
      if ( .not. allocated ( ctx%unitim_state ) ) allocate ( ctx%unitim_state )
      associate ( bslash => ctx%unitim_state%bslash, &
                  dta => ctx%unitim_state%dta, &
                  eb => ctx%unitim_state%eb, &
                  first => ctx%unitim_state%first, &
                  jd2000 => ctx%unitim_state%jd2000, &
                  k => ctx%unitim_state%k, &
                  m => ctx%unitim_state%m, &
                  missed => ctx%unitim_state%missed, &
                  nodata => ctx%unitim_state%nodata, &
                  recog => ctx%unitim_state%recog, &
                  secspd => ctx%unitim_state%secspd, &
                  typtdb => ctx%unitim_state%typtdb, &
                  typtdt => ctx%unitim_state%typtdt, &
                  vars => ctx%unitim_state%vars, &
                  usrctr => ctx%unitim_state%usrctr )
      if ( ctx%failed() ) then
         unitim = 0.0_wp
         return
      end if

      if ( first ) then

         first  = .false.
!
!        Initialize the backslash character.  We use this for error
!        message construction.
!
         bslash = char(92)

!
!        Set up the parameters that we are going to need often.
!
         secspd  = spd
         jd2000  = j2000

!
!        Initialize the sets that we will use.
!
         typtdt(1) = 'JDTDT'
         typtdt(2) = 'TAI'
         typtdt(3) = 'TDT'
         typtdt(4) = 'GPS'
         typtdt(5) = 'TT'

         typtdb(1) = 'ET'
         typtdb(2) = 'JDTDB'
         typtdb(3) = 'JED'
         typtdb(4) = 'TDB'

         call validc ( ctx, ntdt,    ntdt,   typtdt )
         call validc ( ctx, ntdb,    ntdb,   typtdb )

         call ssizec ( ctx, ntdt  +  ntdb,   recog  )
         call unionc ( ctx, typtdt,  typtdb, recog  )

!
!        Initialize the local POOL counter to user value.
!
         call zzctruin( ctx, usrctr )

!
!        Set up the kernel pool watchers
!
         call swpool ( ctx, 'UNITIM', needed, vars )

      end if

!
!     Check to see if any of the kernel items required by this
!     routine have been updated since the last call to this
!     entry point.
!
      call zzcvpool ( ctx, 'UNITIM', usrctr, update )

      if ( update .or. nodata ) then
!
!        Fetch all of the time parameters from the kernel pool.
!
         call gdpool ( ctx, 'DELTET/DELTA_T_A', 1, 1, n, dta_1, found(1) )
         dta = dta_1(1)
         call gdpool ( ctx, 'DELTET/K', 1, 1, n, k_1, found(2) )
         k = k_1(1)
         call gdpool ( ctx, 'DELTET/EB', 1, 1, n, eb_1, found(3) )
         eb = eb_1(1)
         call gdpool ( ctx, 'DELTET/M',         1, 2, n, m,    found(4) )

         if ( ctx%failed() ) then

            nodata = .true.
            unitim = 0.0_wp

            return

         end if

!
!        If anything wasn't found, it's an error dude.
!
         if ( (.not. all(found)) ) then

            nodata = .true.

!
!           If we didn't get all of the things we needed for time
!           conversion, we need to reset the watch.  Otherwise
!           subsequent calls to this routine will never have the
!           needed data.
!
            call swpool ( ctx, 'UNITIM', needed, vars )

            call ctx%setmsg ( 'The following variables, needed to convert'// &
                          ' between the input uniform time scales, '  // &
                          'were not found in the kernel pool: # '     // &
                          'Your program may have failed to load a '   // &
                          'leapseconds kernel. Other possible '       // &
                          'causes of this problem include loading an '// &
                          'invalid leapseconds kernel---one that '    // &
                          'lacks an initial '// bslash //'begindata ' // &
                          'marker or final newline character, or is ' // &
                          'otherwise corrupted---or deleting previous'// &
                          'ly loaded kernel pool variables via calls '// &
                          'to routines that clear the kernel pool. '  )

            do i = 1, needed
               if ( .not. found(i) ) then
                  call ctx%errch ( '#', missed(i) )
               end if
            end do

            call ctx%errch  ( ', #', '.' )
            call ctx%sigerr ( 'SPICE(MISSINGTIMEINFO)' )

            unitim = epoch

            return

         end if

!
!        At this point the kernel data checks are done.
!
         nodata = .false.

      end if

!
!     Normalize the IN and OUT scale variables
!
      call ucase  ( insys,  myin  )
      call ucase  ( outsys, myout )

      call ssizec ( ctx, 2,      types )
      call insrtc ( ctx, myin,   types )
      call insrtc ( ctx, myout,  types )

!
!     We will work with a local copy of EPOCH.
!
      mytime = epoch

!
!     First make sure both types are recognized.
!
      if      ( .not. setc ( ctx, types, '<', recog ) ) then

         call ctx%setmsg ( 'The time types recognized by UNITIM are: '   // &
                   'TAI, GPS, TT, TDT, JDTDT, TDB, ET, JED, JDTDB. ' // &
                   'At least one of the inputs (#, #) was not in '   // &
                   'the list of recognized types. '               )

         call ctx%errch  ( '#', myin            )
         call ctx%errch  ( '#', myout           )
         call ctx%sigerr ( 'SPICE(BADTIMETYPE)' )

         unitim = epoch
         return

      end if

!
!     If the input and output types are the same, just copy the input
!     epoch to the output and call it quits.
!
      if ( myin == myout ) then

         unitim = mytime

         return
      end if

!
!     Determine the base types of the input and output types.
!
      intdt  = elemc ( ctx, myin,  typtdt )
      outtdt = elemc ( ctx, myout, typtdt )
      intdb  = .not. intdt
      outtdb = .not. outtdt

!
!     The two types, TDT and TDB, will be used as the fundamental
!     base used in conversions.
!
!        TAI, GPS, and JDTDT will be converted to TDT
!        JED and JDTDB will be converted to TDB.
!        ET means TDB; TT means TDT.
!
      if      ( myin == 'TAI'   ) then

         mytime = mytime + dta

      else if ( myin == 'GPS' ) then

         mytime = mytime + (dta + gpstai)

      else if ( myin == 'JDTDT' ) then

         mytime = ( mytime - jd2000 ) * secspd

      else if ( myin == 'JED'   ) then

         mytime = ( mytime - jd2000 ) * secspd

      else if ( myin == 'JDTDB' ) then

         mytime = ( mytime - jd2000 ) * secspd

      end if

!
!     At this point, MYTIME has been converted from its input
!     to one of the base types.
!
!     Next change type from TDB to TDT or vice versa, if
!     required.  (The time is already in TDT or TDB).
!
      if      ( intdt .and. outtdb ) then

         tdt    = mytime
         tdb    = tdt + k*sin(           m(0) + m(1)*tdt &
                               + eb*sin( m(0) + m(1)*tdt ) )

         mytime = tdb

      else if ( intdb .and. outtdt ) then

!
!        What we have to do here is invert the formula used to get
!        TDB from TDT that was used above.
!
!        Of course solving the equation
!
!           TDB = TDT + K*SIN { M0 + M1*TDT + EB*SIN( MO + M1*TDT ) }
!
!        analytically for TDT if given TDB is no piece of cake.
!        However, we can get as close as we want to TDT if
!        we notice a few tricks.  First, let's let f(t) denote the
!        function
!
!           f(t) = SIN( M0 + M1*t + EB*SIN( M0 + M1*t ) )
!
!        With this simpler notation we can rewrite our problem
!        as that of solving the equation
!
!           y = t + K*f(t)
!
!        for t given y.  Whichever t satisfies this equation will be
!        unique. The uniqueness of the solution is ensured because the
!        expression on the right-hand side of the equation is
!        monotone increasing in t.
!
!        Let's suppose that t is the solution, then the following
!        is true.
!
!           t = y - K*f(t)
!
!        but we can also replace the t on the right hand side of the
!        equation by y - K*f(t).  Thus
!
!           t = y - K*f( y - K*f(t))
!
!             = y - K*f( y - K*f( y - K*f(t)))
!
!             = y - K*f( y - K*f( y - K*f( y - K*f(t))))
!
!             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(t)))))
!             .
!             .
!             .
!             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(y - ... )))
!
!        and so on, for as long as we have patience to perform the
!        substitutions.
!
!        The point of doing this recursive substitution is that we
!        hope to move t to an insignificant part of the computation.
!        This would seem to have a reasonable chance of success since
!        K is a small number and f is bounded by 1.
!
!        Following this idea, we will attempt to solve for t using
!        the recursive method outlined below.
!
!        We will make our first guess at t, call it t_0.
!
!         t_0 = y
!
!        Our next guess, t_1, is given by:
!
!         t_1 = y - K*f(t_0)
!
!        And so on:
!
!         t_2 = y - K*f(t_1)        [ = y - K*f(y - K*f(y))            ]
!         t_3 = y - K*f(t_2)        [ = y - K*f(y - K*f(y - K*f(y)))   ]
!             .
!             .
!             .
!         t_n = y - K*f(t_(n-1))    [ = y - K*f(y - K*f(y - K*f(y...)))]
!
!        The questions to ask at this point are:
!
!           1) Do the t_i's converge?
!           2) If they converge, do they converge to t?
!           3) If they converge to t, how fast do they get there?
!
!        1) The sequence of approximations converges.
!
!           | t_n - t_(n-1) | =    [ y - K*f( t_(n-1) ) ]
!                               -  [ y - K*f( t_(n-2) ) ]
!
!                             =  K*[ f( t_(n-2) ) - f( t_(n-1) ) ]
!
!           The function f has an important property. The absolute
!           value of its derivative is always less than M1*(1+EB).
!           This means that for any pair of real numbers s,t
!
!              | f(t) - f(s) |  < M1*(1+EB)*| t - s |.
!
!           From this observation, we can see that
!
!             | t_n - t_(n-1) | < K*M1*(1+EB)*| t_(n-1) - t_(n-2) |
!
!           With this fact available, we could (with a bit more work)
!           conclude that the sequence of t_i's converges and that
!           it converges at a rate that is at least as fast as the
!           sequence L, L**2, L**3, ....
!
!           Where L = K*M1*(1+EB) << 1.
!
!         2) If we let t be the limit of the t_i's then it follows
!            that
!
!               t = y - K*f(t).
!
!            or that
!
!               y = t + K*f(t).
!
!         3) As we already pointed out, the sequence of t_i's
!            converges at least as fast as the geometric series
!            L, L**2, ...
!
!        Since K*M1*(1+EB) is quite small (on the order of 10**-9)
!        3 iterations should get us as close as we can get to the
!        solution for TDT
!

         tdb = mytime
         tdt = tdb

         do i = 1, 3

            tdt = tdb - k*sin(           m(0) + m(1)*tdt &
                               + eb*sin( m(0) + m(1)*tdt ))
         end do

         mytime = tdt

      end if

!
!     Now MYTIME is in the base type of the requested output.
!     If further conversion is required, we do it here.
!
      if      ( myout == 'TAI'   ) then

         mytime = mytime - dta

       else if ( myout == 'GPS' ) then

         mytime = mytime - (dta + gpstai)

      else if ( myout == 'JDTDT' ) then

         mytime = mytime/secspd + jd2000

      else if ( myout == 'JED'   ) then

         mytime = mytime/secspd + jd2000

      else if ( myout == 'JDTDB' ) then

         mytime = mytime/secspd + jd2000

      end if

      unitim = mytime

      return
      end associate
      end function unitim

      !> Convert a string representing an epoch to a double precision
      !> value representing the number of TDB seconds past the J2000
      !> epoch corresponding to the input epoch.
      !>
      !>## Detailed Input
      !>
      !>     TIMSTR   is a string representing an epoch. Virtually all
      !>              common calendar representations are allowed. You may
      !>              specify a time string belonging to any of the
      !>              systems TDB, TDT, UTC. Moreover, you may specify a
      !>              time string relative to a specific UTC based time
      !>              zone.
      !>
      !>              The rules used in the parsing of TIMSTR are spelled out
      !>              in great detail in the reference document time.req. The
      !>              basics are given in the $Particulars section below.
      !>
      !>## Detailed Output
      !>
      !>     ET       is the double precision number of TDB seconds past the
      !>              J2000 epoch that corresponds to the input TIMSTR.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the TIMSTR input string cannot be recognized as a
      !>         legitimate time string, the error SPICE(UNPARSEDTIME) is
      !>         signaled.
      !>
      !>     2)  If more than one time system is specified as part of the
      !>         input time string, the error SPICE(TIMECONFLICT) is signaled.
      !>
      !>     3)  If any component of the input time string is outside the
      !>         normal range of usage, the error SPICE(BADTIMESTRING) is
      !>         signaled. For example, the day January 35 is outside the
      !>         normal range of days in January. The checks applied are
      !>         spelled out in the routine TCHECK.
      !>
      !>     4)  If a time zone is specified with hours or minutes components
      !>         that are outside of the normal range, the error
      !>         SPICE(TIMEZONEERROR) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the ephemeris epoch corresponding to an
      !>     input string. The ephemeris epoch is represented as seconds
      !>     past the J2000 epoch in the time system known as Barycentric
      !>     Dynamical Time (TDB). This time system is also referred to as
      !>     Ephemeris Time (ET) throughout the SPICE Toolkit.
      !>
      !>     The variety of ways people have developed for representing
      !>     times is enormous. It is unlikely that any single subroutine
      !>     can accommodate the wide variety of custom time formats that
      !>     have arisen in various computing contexts. However, we
      !>     believe that this routine will correctly interpret most time
      !>     formats used throughout the planetary science community.
      !>     For example this routine supports ISO time formats and UNIX
      !>     `date` output formats. One obvious omission from the strings
      !>     recognized by this routine are strings of the form
      !>
      !>          93234.1829  or 1993234.1829
      !>
      !>     Some readers may recognize this as the epoch that is 0.1829
      !>     days past the beginning of the 234'th day of 1993. However,
      !>     many other readers may regard this interpretation as a bit
      !>     obscure.
      !>
      !>     Below we outline some of the rules used in the interpretation
      !>     of strings. A more complete discussion of the interpretation
      !>     of strings is given in the reference document time.req.
      !>
      !>     Default Behavior
      !>     ----------------
      !>
      !>     Consider the string
      !>
      !>        1988 June 13, 3:29:48
      !>
      !>     There is nothing in this string to indicate what time system
      !>     the date and time belong to. Moreover, there is nothing to
      !>     indicate whether the time is based on a 24-hour clock or
      !>     twelve hour clock.
      !>
      !>     In the absence of such indicators, the default interpretation
      !>     of this string is to regard the time of day to be a time on
      !>     a 24-hour clock in the UTC time system. The date is a date
      !>     on the Gregorian Calendar (this is the calendar used in nearly
      !>     all western societies).
      !>
      !>     Labels
      !>     ------
      !>
      !>     If you add more information to the string, STR2ET can make a
      !>     more informed interpretation of the time string. For example:
      !>
      !>        1988 June 13, 3:29:48 P.M.
      !>
      !>     is still regarded as a UTC epoch. However, with the addition
      !>     of the 'P.M.' label it is now interpreted as the same epoch
      !>     as the unlabeled epoch 1988 June 13, 15:29:48. Similarly
      !>
      !>        1988 June 13, 12:29:48 A.M.
      !>
      !>     is interpreted as
      !>
      !>        1988 June 13, 00:29:48
      !>
      !>     For the record: 12:00 A.M. corresponds to Midnight (00:00 on the
      !>     24 hour clock.  12:00 P.M. corresponds to Noon. (12:00) on the
      !>     24 hour clock.
      !>
      !>     You may add still further indicators to the string. For example
      !>
      !>        1988 June 13, 3:29:48 P.M. PST
      !>
      !>     is interpreted as an epoch in the Pacific Standard Time system.
      !>     This is equivalent to
      !>
      !>        1988 June 13, 07:29:48 UTC
      !>
      !>     The following U.S. time zones are recognized.
      !>
      !>        EST   --- Eastern Standard Time  ( UTC-5:00 )
      !>        CST   --- Central Standard Time  ( UTC-6:00 )
      !>        MST   --- Mountain Standard Time ( UTC-7:00 )
      !>        PST   --- Pacific Standard Time  ( UTC-8:00 )
      !>
      !>        EDT   --- Eastern Daylight Time  ( UTC-4:00 )
      !>        CDT   --- Central Daylight Time  ( UTC-5:00 )
      !>        MDT   --- Mountain Daylight Time ( UTC-6:00 )
      !>        PDT   --- Pacific Daylight Time  ( UTC-7:00 )
      !>
      !>     In addition any other time zone may be specified by representing
      !>     its offset from UTC. This notation starts with the letters 'UTC'
      !>     followed by a '+' for time zones east of Greenwich and '-' for
      !>     time zones west of Greenwich. This is followed by the number of
      !>     hours to add or subtract from UTC. This is optionally followed
      !>     by a colon ':' and the number of minutes to add or subtract to
      !>     get the local time zone. Thus to specify the time zone of
      !>     Calcutta (which is 5 and 1/2 hours ahead of UTC) you would
      !>     specify the time zone to be UTC+5:30. To specify the time zone
      !>     of Newfoundland (which is 3 and 1/2 hours behind UTC) use the
      !>     offset notation UTC-3:30.
      !>
      !>     For the Record:  Leapseconds occur at the same time in all
      !>     time zones. In other words, the seconds component of a time
      !>     string is the same for any time zone as is the seconds
      !>     component of UTC. Thus the following are all legitimate
      !>     ways to represent an epoch of some event that occurred
      !>     in the leapsecond
      !>
      !>        1995 December 31  23:59:60.5  (UTC)
      !>
      !>        1996 January   1, 05:29:60.5  (UTC+5:30 --- Calcutta Time)
      !>        1995 December 31, 20:29:60.5  (UTC-3:30 --- Newfoundland)
      !>        1995 December 31  18:59:60.5  (EST)
      !>        1995 December 31  17:59:60.5  (CST)
      !>        1995 December 31  16:59:60.5  (MST)
      !>        1995 December 31  15:59:60.5  (PST)
      !>
      !>     In addition to specifying time zones, you may specify that the
      !>     string be interpreted as a formal calendar representation in
      !>     either the Barycentric Dynamical Time system (TDB) or the
      !>     Terrestrial Dynamical Time system (TDT).  In These systems there
      !>     are no leapseconds. Times in TDB are written as
      !>
      !>        1988 June 13, 12:29:48 TDB
      !>
      !>     TDT times are written as:
      !>
      !>        1988 June 13, 12:29:48 TDT
      !>
      !>     Finally, you may explicitly state that the time system is UTC
      !>
      !>        1988 June 13, 12:29:48 UTC.
      !>
      !>     Abbreviating Years
      !>     ------------------
      !>
      !>     Although it can lead to confusion, many people are in the
      !>     habit of abbreviating years when they write them in dates.
      !>     For example
      !>
      !>        99 Jan 13,  12:28:24
      !>
      !>     Upon seeing such a string, most of us would regard this
      !>     as being 1999 January 13, 12:28:24 and not January 13 of
      !>     the year 99. This routine interprets years that are less
      !>     than 100 as belonging either to the 1900's or 2000's. Years
      !>     greater than 68 ( 69 - 99 ) are regarded as being an
      !>     abbreviation with the '19' suppressed (1969 - 1999). Years
      !>     smaller than 69 ( 00 - 68 ) are regarded as being an
      !>     abbreviation with the '20' suppressed (2000 - 2068).
      !>
      !>     Note that in general it is usually a good idea to write
      !>     out the year. Or if you'd like to save some typing
      !>     abbreviate 1999 as '99.
      !>
      !>     If you need to specify an epoch whose year
      !>     is less than 1000, we recommend that you specify the era
      !>     along with the year. For example if you want to specify
      !>     the year 13 A.D. write it as
      !>
      !>        13 A.D. Jan 12
      !>
      !>     When specifying the era it should immediately follow the year.
      !>     Both the A.D. and B.C. eras are supported.
      !>
      !>     Changing Default Behavior
      !>     -------------------------
      !>
      !>     As discussed above, if a string is unlabeled, it is regarded
      !>     as representing a string in the UTC time system on the
      !>     Gregorian calendar. In addition abbreviated years are
      !>     regarded as abbreviations of the years from 1969 to 2068.
      !>
      !>     You may modify these defaults through the routines TIMDEF
      !>     and TSETYR.
      !>
      !>     You may:
      !>
      !>        Set the calendar to be Gregorian, Julian or a mixture of
      !>        two via the TIMDEF;
      !>
      !>        Set the time system to be UTC, TDB, TDT or any time zone
      !>        via the routine TIMDEF;
      !>
      !>        Set the range of year abbreviations to be any 100 year
      !>        interval via the routine TSETYR.
      !>
      !>     See the SPICELIB routine TEXPYR and TIMDEF for details on changing
      !>     defaults.
      !>
      !>     These alterations affect only the interpretation of unlabeled
      !>     strings. If an input string is labeled the specification
      !>     in the label is used.
      !>
      !>     If any component of a date or time is out of range, STR2ET
      !>     regards the string as erroneous. Below is a list of
      !>     erroneous strings and why they are regarded as such.
      !>
      !>        1997 Jan 32 12:29:29     --- there are only 31 days in January
      !>
      !>        '98 Jan 12 13:29:29 A.M. --- Hours must be between 1 and 12
      !>                                     inclusive when A.M. or P.M. is
      !>                                     specified.
      !>
      !>        1997 Feb 29, 12:29:20.0  --- February has only 29 days in
      !>                                     1997. This would be ok if the
      !>                                     year was 1996.
      !>
      !>        1992 Mar 12 12:62:20     --- Minutes must be between 0 and 59
      !>                                     inclusive.
      !>
      !>        1993 Mar 18 15:29:60.5   --- Seconds is out of range for this
      !>                                     date. It would not be out of
      !>                                     range for Dec 31 23:59:60.5 or
      !>                                     Jun 30 23:59:60.5 because these
      !>                                     can be leapseconds (UTC).
      !>
      !>     Specifics On Interpretation of the Input String
      !>     -----------------------------------------------
      !>
      !>     The process of examining the string to determine its meaning is
      !>     called "parsing" the string. The string is parsed by first
      !>     determining its recognizable substrings (integers, punctuation
      !>     marks, names of months, names of weekdays, time systems, time
      !>     zones, etc.) These recognizable substrings are called the tokens
      !>     of the input string. The meaning of some tokens are immediately
      !>     determined. For example named months, weekdays, time systems have
      !>     clear meanings. However, the meanings of numeric components must
      !>     be deciphered from their magnitudes and location in the string
      !>     relative to the immediately recognized components of the input
      !>     string.
      !>
      !>     To determine the meaning of the numeric tokens in the input
      !>     string, a set of "production rules" and transformations are
      !>     applied to the full set of tokens in the string. These
      !>     transformations are repeated until the meaning of every token
      !>     has been determined, or until further transformations yield
      !>     no new clues into the meaning of the numeric tokens.
      !>
      !>     1)  Unless the substring 'JD' or 'jd' is present, the string is
      !>         assumed to be a calendar format (day-month-year or year and
      !>         day of year). If the substring JD or jd is present, the
      !>         string is assumed to represent a Julian date.
      !>
      !>     2)  If the Julian date specifier is not present, any integer
      !>         greater than 999 is regarded as being a year specification.
      !>
      !>     3)  A dash '-' can represent a minus sign only if it precedes
      !>         the first digit in the string and the string contains
      !>         the Julian date specifier (JD). (No negative years,
      !>         months, days, etc. are allowed).
      !>
      !>     4)  Numeric components of a time string must be separated
      !>         by a character that is not a digit or decimal point.
      !>         Only one decimal component is allowed. For example
      !>         1994219.12819 is sometimes interpreted as the
      !>         219th day of 1994 + 0.12819 days. STR2ET does not
      !>         support such strings.
      !>
      !>     5)   No exponential components are allowed. For example you
      !>         can't specify the Julian date of J2000 as 2.451545E6.
      !>         You also can't input 1993 Jun 23 23:00:01.202E-4 and have
      !>         to explicitly list all zeros that follow the decimal
      !>         point: i.e. 1993 Jun 23 23:00:00.0001202.
      !>
      !>     6)  The single colon (:) when used to separate numeric
      !>         components of a string is interpreted as separating
      !>         Hours, Minutes, and Seconds of time.
      !>
      !>     7)  If a double slash (//) or double colon (::) follows
      !>         a pair of integers, those integers are assumed  to
      !>         represent the year and day of year.
      !>
      !>     8)  A quote followed by an integer less than 100 is regarded
      !>         as an abbreviated year. For example: '93 would be regarded
      !>         as the 93rd year of the reference century. See the SPICELIB
      !>         routine TEXPYR for further discussion of abbreviated years.
      !>
      !>     9)  An integer followed by 'B.C.' or 'A.D.' is regarded as
      !>         a year in the era associated with that abbreviation.
      !>
      !>     10) All dates are regarded as belonging to the extended
      !>         Gregorian Calendar (the Gregorian calendar is the calendar
      !>         currently used by western society). See the routine TIMDEF
      !>         to modify this behavior.
      !>
      !>     11) If the ISO date-time separator (T) is present in the string
      !>         ISO allowed token patterns are examined for a match
      !>         with the current token list. If no match is found the
      !>         search is abandoned and appropriate diagnostic messages
      !>         are generated. Historically the interpretation of ISO
      !>         formatted time strings deviates from the ISO standard in
      !>         allowing two digit years and expanding years in the 0 to 99
      !>         range the same way as is done for non ISO formatted strings.
      !>         Due to this interpretation it is impossible to specify
      !>         times in years in the 0 A.D. to 99 A.D. range using ISO
      !>         formatted strings on the input.
      !>
      !>     12) If two delimiters are found in succession in the time
      !>         string, the time string is diagnosed as an erroneous string.
      !>         (Delimiters are comma, white space, dash, slash, period, or
      !>         day of year mark. The day of year mark is a pair of forward
      !>         slashes or a pair of colons.)
      !>
      !>         Note the delimiters do not have to be the same. The pair
      !>         of characters ",-" counts as two successive delimiters.
      !>
      !>     13) White space and commas serve only to delimit tokens in the
      !>         input string. They do not affect the meaning of any
      !>         of the tokens.
      !>
      !>     14) If an integer is greater than 1000 (and the 'JD' label
      !>         is not present, the integer is regarded as a year.
      !>
      !>     15) When the size of the integer components does not clearly
      !>         specify a year the following patterns are assumed
      !>
      !>         Calendar Format
      !>
      !>            Year Month Day
      !>            Month Day Year
      !>            Year Day Month
      !>
      !>            where Month is the name of a month, not its numeric
      !>            value.
      !>
      !>            When integer components are separated by slashes (/)
      !>            as in 3/4/5. Month, Day, Year is assumed (2005 March 4)
      !>
      !>         Day of Year Format.
      !>
      !>            If a day of year marker is present (// or ::) the
      !>            pattern
      !>
      !>              I-I// or I-I:: (where I stands for an integer)
      !>
      !>            is interpreted as Year Day-of-Year. However, I-I/ is
      !>            regarded as ambiguous.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you would like to determine whether your favorite
      !>        time representation is supported by STR2ET. The small
      !>        program below gives you a simple way to experiment with
      !>        STR2ET. (Note that erroneous inputs will be flagged by
      !>        signaling an error.)
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM STR2ET_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(64)        TIMSTR
      !>      CHARACTER*(64)        CALDR
      !>      CHARACTER*(64)        DAYOFY
      !>      CHARACTER*(127)       FILNAM
      !>
      !>      DOUBLE PRECISION      ET
      !>
      !>C
      !>C     First get the name of a leapseconds kernel, and load it.
      !>C
      !>      CALL PROMPT ( 'Leapseconds kernel: ', FILNAM )
      !>      CALL FURNSH ( FILNAM )
      !>
      !>C
      !>C     Get the time string.
      !>C
      !>      CALL PROMPT ( 'Time string: ', TIMSTR )
      !>
      !>C
      !>C     Convert the string to ET and then back to UTC calendar
      !>C     and day-of-year formats.
      !>C
      !>      CALL STR2ET ( TIMSTR, ET )
      !>      CALL ET2UTC ( ET, 'C', 0, CALDR  )
      !>      CALL ET2UTC ( ET, 'D', 0, DAYOFY )
      !>
      !>C
      !>C     Print the results.
      !>C
      !>      WRITE (*,*)
      !>      WRITE (*,*) 'TBD seconds from J2000 epoch: ', ET
      !>      WRITE (*,*) 'Calendar    Format:           ', CALDR
      !>      WRITE (*,*) 'Day of year Format:           ', DAYOFY
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a PC/Linux/gfortran/64-bit
      !>        platform, using the LCK file named naif0012.tls and the time
      !>        string '2017-07-14T19:46:00', the output was:
      !>
      !>        Leapseconds kernel: naif0012.tls
      !>        Time string: 2017-07-14T19:46:00
      !>
      !>         TBD seconds from J2000 epoch:    553333629.18372738
      !>         Calendar    Format:           2017 JUL 14 19:46:00
      !>         Day of year Format:           2017-195 // 19:46:00
      !>
      !>     2) Below is a sampling of some of the time formats that are
      !>        acceptable as inputs to STR2ET. A complete discussion of
      !>        permissible formats is given in the reference document
      !>        time.req.
      !>
      !>        ISO (T) Formats.
      !>
      !>        String                        Year Mon  DOY DOM  HR Min Sec
      !>        ----------------------------  ---- ---  --- ---  -- --- ------
      !>        1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
      !>        1986-01-18T12                 1986 Jan   na  18  12  00 00
      !>        1986-01-18T12:19              1986 Jan   na  18  12  19 00
      !>        1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18
      !>        1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18
      !>        1995-08T18:28:12              1995  na  008  na  18  28 12
      !>        1995-08T18:28:12Z             1995  na  008  na  18  28 12
      !>        1995-18T                      1995  na  018  na  00  00 00
      !>        0000-01-01T                   1 BC Jan   na  01  00  00 00
      !>
      !>        Calendar Formats.
      !>
      !>        String                        Year   Mon DOM  HR Min  Sec
      !>        ----------------------------  ----   --- ---  -- ---  ------
      !>        Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57
      !>        1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192
      !>        2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002
      !>        Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287
      !>        1992 11:18:28  3 Jul          1992   Jul  03  11  18  28
      !>        June 12, 1989 01:21           1989   Jun  12  01  21  00
      !>        1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29
      !>        17JUN1982 18:28:28            1982   Jun  17  18  28  28
      !>        13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128
      !>        1972 27 jun 12:29             1972   Jun  27  12  29  00
      !>        '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289
      !>        27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182
      !>        23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29
      !>        18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291
      !>        29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298
      !>        29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298
      !>
      !>        Day of Year Formats
      !>
      !>        String                        Year  DOY HR Min Sec
      !>        ----------------------------  ----  --- -- --- ------
      !>        1997-162::12:18:28.827        1997  162 12  18 28.827
      !>        162-1996/12:28:28.287         1996  162 12  28 28.287
      !>        1993-321/12:28:28.287         1993  231 12  28 28.287
      !>        1992 183// 12:18:19           1992  183 12  18 19
      !>        17:28:01.287 1992-272//       1992  272 17  28 01.287
      !>        17:28:01.282 272-1994//       1994  272 17  28 01.282
      !>        '92-271/ 12:28:30.291         1992* 271 12  28 30.291
      !>        92-182/ 18:28:28.281          1992* 182 18  28 28.281
      !>        182-92/ 12:29:29.192          0182+ 092 12  29 29.192
      !>        182-'92/ 12:28:29.182         1992  182 12  28 29.182
      !>
      !>        Julian Date Strings
      !>
      !>        jd 28272.291                  Julian Date   28272.291
      !>        2451515.2981 (JD)             Julian Date 2451515.2981
      !>        2451515.2981 JD               Julian Date 2451515.2981
      !>
      !>                                      Abbreviations Used in Tables
      !>
      !>                                        na    --- Not Applicable
      !>                                        Mon   --- Month
      !>                                        DOY   --- Day of Year
      !>                                        DOM   --- Day of Month
      !>                                        Wkday --- Weekday
      !>                                        Hr    --- Hour
      !>                                        Min   --- Minutes
      !>                                        Sec   --- Seconds
      !>
      !>        *  The default interpretation of a year that has been
      !>           abbreviated to two digits with or without a leading quote
      !>           as in 'xy or xy (such as '92 or 92) is to treat the year as
      !>           19xy if xy > 68 and to treat it as 20xy otherwise. Thus '70
      !>           is interpreted as 1970 and '67 is treated as 2067. However,
      !>           you may change the "split point" and centuries through use
      !>           of the SPICE routine TSETYR. See that routine for a
      !>           discussion of how you may reset the split point.
      !>
      !>        ** All epochs are regarded as belonging to the Gregorian
      !>           calendar. We formally extend the Gregorian calendar backward
      !>           and forward in time for all epochs. If you have epochs
      !>           belonging to the Julian Calendar, consult the SPICELIB
      !>           routines TPARTV and JUL2GR for a discussion concerning
      !>           conversions to the Gregorian calendar and ET. The routines
      !>           TIMDEF and STR2ET, used together, also support conversions
      !>           from Julian Calendar epochs to ET.
      !>
      !>        +  When a day of year format or calendar format string is
      !>           input and neither of the integer components of the date is
      !>           greater than 1000, the first integer is regarded as being
      !>           the year.
      !>
      !>        Any integer greater than 1000 is regarded as a year
      !>        specification. Thus 1001-1821//12:28:28 is interpreted as
      !>        specifying two years and will be rejected as ambiguous.
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
      !>     M. Costa Sitja     (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.0, 23-DEC-2021 (JDR) (EDW) (MCS)
      !>
      !>        Changed the input argument name STRING to TIMSTR for
      !>        consistency with other routines.
      !>
      !>        Header edits to expand description of ISO format.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added comments and removed do-loop from code example.
      !>
      !>        Replaced references to TPARTV by time.req.
      !>
      !>-    SPICELIB Version 1.3.1, 02-NOV-2009 (CHA)
      !>
      !>        A few minor grammar errors were fixed in the header.
      !>        The header sections were reordered.
      !>
      !>-    SPICELIB Version 1.3.0, 31-AUG-2006 (NJB) (EDW)
      !>
      !>        Bug fix: routine formerly returned incorrect results
      !>        in some cases on calls following calls for which a time
      !>        zone was specified.
      !>
      !>        Replaced reference to LDPOOL in header $Examples section
      !>        with reference to FURNSH.
      !>
      !>-    SPICELIB Version 1.2.2, 29-JUL-2003 (NJB)
      !>
      !>        Various minor header corrections were made
      !>
      !>-    SPICELIB Version 1.2.1, 10-FEB-2003 (NJB)
      !>
      !>        Corrected header typo.
      !>
      !>-    SPICELIB Version 1.2.0, 11-NOV-1997 (WLT)
      !>
      !>        The previous versions of this routine did not correctly
      !>        convert day-of-year strings in the TDB or TDT systems.
      !>        They treated the day of year as year, month, day giving
      !>        spectacularly wrong answers.
      !>
      !>        In addition, comments concerning the default century for
      !>        abbreviated years were updated to reflect changes to TEXPYR
      !>
      !>-    SPICELIB Version 1.1.0, 10-FEB-1997 (WLT)
      !>
      !>        In the case that a time zone could not be parsed,
      !>        this routine signaled an error and checked out without
      !>        then returning. This error has been corrected.
      !>
      !>-    SPICELIB Version 1.0.0, 15-NOV-1996 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.3.0, 31-AUG-2006 (NJB)
      !>
      !>        Bug fix: routine formerly returned incorrect results
      !>        in some cases on calls following calls for which a time
      !>        zone was specified.
      !>
      !>        The problem was caused by the variable ZONED not being
      !>        properly set when a time system was specified
      !>        in the input string. In such cases, ZONED retained the
      !>        value from the previous call.
      module subroutine str2et ( ctx, timstr, et )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: prefix, repmc_inplace, repmi_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: timstr  !! A string representing an epoch.
      real(wp), intent(out) :: et  !! The equivalent value in seconds past J2000, TDB.
!
!     Local (in-line) Functions
!
      integer(ip), parameter :: smwdsz = 16
      integer(ip), parameter :: msgsiz = 400
      integer(ip), parameter :: era = 1
      integer(ip), parameter :: wkday = era    + 1
      integer(ip), parameter :: zone = wkday  + 1
      integer(ip), parameter :: ampm = zone   + 1
      integer(ip), parameter :: system = ampm   + 1
      character(smwdsz), parameter :: juln = 'JULIAN'
      character(smwdsz), parameter :: mixed = 'MIXED'
      character(smwdsz), parameter :: gregrn = 'GREGORIAN'
      character(lnsize) :: pictur
      character(smwdsz) :: type
      integer(ip) :: day
      integer(ip) :: year
!
!     The following integers are pointers to the
!     locations of various components in a time vector.
!
      logical :: mods
      integer(ip) :: hr
      integer(ip) :: yr
      integer(ip) :: doy
      real(wp) :: tvecm  ( 8 )
      real(wp) :: secs
      logical :: ok2
!     PEPPER: TTRANS declares its time vector with 10 elements.
      real(wp) :: tvec   ( 10 )
      logical :: succes
      character(smwdsz) :: forml
      logical :: ok1
      real(wp) :: minute
      integer(ip) :: dy
      character(smwdsz) :: modify ( system )
      real(wp) :: dhoff
      logical :: adjust
      character(2) :: mstr
      real(wp) :: extra
      integer(ip) :: cyear
      logical :: yabbrv
      integer(ip) :: sc
      character(smwdsz) :: check
      character(msgsiz) :: error
      logical :: dojul
      integer(ip) :: ntvec
      real(wp) :: frac
      integer(ip) :: orgnyr
      logical :: zoned
      integer(ip) :: month
      real(wp) :: dmoff
      logical :: ok
      integer(ip) :: last
      integer(ip) :: i
      integer(ip) :: mn
      character(2) :: hstr
      real(wp) :: hour
      integer(ip) :: gyear
      if ( .not. allocated ( ctx%str2et_state ) ) allocate ( ctx%str2et_state )
      associate ( calndr => ctx%str2et_state%calndr, &
                  defsys => ctx%str2et_state%defsys, &
                  defzon => ctx%str2et_state%defzon, &
                  mname => ctx%str2et_state%mname, &
                  hoff => ctx%str2et_state%hoff, &
                  mdy => ctx%str2et_state%mdy, &
                  moff => ctx%str2et_state%moff, &
                  mon => ctx%str2et_state%mon, &
                  mm => ctx%str2et_state%mm )
      if ( ctx%failed() ) return

!
!     Collect the current defaults.
!
      call timdef ( ctx, 'GET', 'SYSTEM',   defsys )
      call timdef ( ctx, 'GET', 'ZONE',     defzon )
      call timdef ( ctx, 'GET', 'CALENDAR', calndr )

      if ( defzon /= ' ' ) then
         call prefix  ( '::', 0, defzon )
         call zzutcpm ( ctx, defzon, 1, dhoff, dmoff, last, succes )
      else
         dhoff = 0.0_wp
         dmoff = 0.0_wp
      end if

!
!     See if TPARTV can recognize what the user has supplied.
!
      call tpartv ( ctx, timstr, &
                    tvec,   ntvec, type, &
                    modify, mods,  yabbrv, succes, &
                    pictur, error )

      if ( .not. succes ) then

         call ctx%setmsg ( error )
         call ctx%sigerr ( 'SPICE(UNPARSEDTIME)' )
         return

      end if

!
!     A system and time zone are incompatible components in a
!     time string.
!
      if (       modify(zone)   /= ' ' &
           .and. modify(system) /= ' ' ) then

         call ctx%setmsg ( 'Both a time system and time zone have ' &
         //            'been specified in the input string (# ' &
         //            'and #). These are inconsistent. A ' &
         //            'time zone is a fixed offset from UTC. ' )

         call ctx%errch  ( '#', modify(system)   )
         call ctx%errch  ( '#', modify(zone)     )
         call ctx%sigerr ( 'SPICE(TIMECONFLICT)' )
         return

      end if
!
!     If both the zone and system are empty, we can replace them
!     with the default zone and system values (only one of which
!     can be non-blank).
!
      zoned = .false.

      if ( modify(zone) == ' ' .and. modify(system) == ' ' ) then

         modify(zone)   = defzon
         modify(system) = defsys
         hoff           = dhoff
         moff           = dmoff
         zoned          = modify(zone) /= ' '

      else if ( modify(zone) /= ' ' ) then
!
!        Parse the time zone specification.  If we don't succeed
!        in the parsing, signal an error.
!
         zoned = .true.
         call prefix  ( '::', 0, modify(zone) )
         call zzutcpm ( ctx, modify(zone), 1, hoff, moff, last, succes )

         if ( .not. succes ) then
            call ctx%setmsg ( '# is not a legitimate time zone ' &
            //            'specification. '       )
            call ctx%errch  ( '#', modify(zone)(3:)   )
            call ctx%sigerr ( 'SPICE(TIMEZONEERROR)'  )
            return
         end if

      end if

!
!     We handle the julian date case now.  It doesn't have the
!     complications associated with it that the calendar strings
!     have.
!
      if ( type == 'JD' ) then

         if      ( modify(system) == 'UTC' ) then
            type = 'JDUTC'
         else if ( modify(system) == 'TDB' ) then
            type = 'JDTDB'
         else if ( modify(system) == 'TDT' ) then
            type = 'JDTDT'
         else
            type = 'JDUTC'
         end if

         call ttrans ( ctx, type, 'TDB', tvec )
         et = tvec(1)

         return

      end if

!
!     Set the indexes of the hours, minutes, seconds, etc. components
!     of the time vector.
!
      if ( type == 'YD' ) then
         yr    =  1
         dy    =  2
         hr    =  3
         mn    =  4
         sc    =  5
         forml = 'YDF'
      else
         yr    =  1
         mm    =  2
         dy    =  3
         hr    =  4
         mn    =  5
         sc    =  6
         forml = 'YMDF'
      end if

!
!     Check the components for reasonableness.
!
      call tchckd ( ctx, check )
      call tparch ( ctx, 'YES' )

!
!     If the calendar is NOT Gregorian, or if we have a time zone
!     present, we avoid the problem of checking for legitimate
!     leapseconds (at least we avoid this problem for the moment).
!
      adjust = .false.

      if ( zoned .or.  calndr /= gregrn ) then

         if (       tvec(sc) >= 60.0_wp &
              .and. tvec(sc) < 61.0_wp ) then

            adjust   = .true.
            tvec(sc) = tvec(sc) - 1.0_wp

         end if

      end if

      if ( calndr == mixed  ) then
!
!        This is a bit awkward, but here's what's going on.
!        If the input calendar is part of the Julian calendar
!        it might be Feb 29 on a century such as 1500.  These
!        are not legitimate dates on the Gregorian calendar.
!        But they are ok on the Julian calendar.
!
!        However, one of the year numbers YEAR or YEAR + 4 will
!        be a leap year on both the Julian and Gregorian calendar.
!        If we have just a century problem, it will be a problem
!        for only one of the years.  So in the range where we could
!        have a problem we call TCHECK twice and .OR. the results
!        of the checks to see if we have a legitimate time vector.
!
         if ( tvec(yr) < 1580.0_wp ) then

            tvecm(1:6) = tvec(1:6)

            tvecm(1) = tvecm(1) + 4.0_wp

            call tcheck ( ctx, tvecm, type, mods, modify, ok1, error )
            call tcheck ( ctx, tvec,  type, mods, modify, ok2, error )

            ok = ok1 .or. ok2

         else

            call tcheck ( ctx, tvec,  type, mods, modify, ok, error )

         end if

      else if ( calndr == juln  ) then
!
!        Basically, this is the same story as before, but there
!        are no bounds in the years where we might be on a century.
!        So we just check twice for each time vector.
!
         tvecm(1:6) = tvec(1:6)

         tvecm(1) = tvecm(1) + 4.0_wp

         call tcheck ( ctx, tvecm, type, mods, modify, ok1, error )
         call tcheck ( ctx, tvec,  type, mods, modify, ok2, error )

         ok = ok1 .or. ok2

      else
!
!        TCHECK was designed for the Gregorian Calendar,  So we
!        don't have much to do.
!
         call tcheck ( ctx, tvec, type, mods, modify, ok, error )

      end if
!
!     Reset the checking status.
!
      call tparch ( ctx, check  )

!
!     If we didn't get an OK from the inspection above,
!     say so and signal an error.
!
      if ( .not. ok ) then

         call ctx%setmsg ( error )
         call ctx%sigerr ( 'SPICE(BADTIMESTRING)' )
         return

      end if
!
!     Reset TVEC(SC) if it was adjusted earlier.
!
      if ( adjust ) then
         tvec(sc) = tvec(sc) + 1.0_wp
      end if

!
!     There are no leapseconds in the TDT and TDB time systems
!     This means that the seconds component must be less than 60.
!
      if (      modify(system) == 'TDT' &
           .or. modify(system) == 'TDB' ) then

         if ( tvec(sc) >= 60.0_wp ) then

            call ctx%setmsg ( 'The seconds component of time must ' &
            //            'be less than 60 for any ' &
            //            'calendar representation of #. ' )
            call ctx%errch  ( '#', modify(system) )
            call ctx%sigerr ( 'SPICE(BADTIMESTRING)' )
            return

         end if

      end if

!
!     If a B.C. era marker is present we can't have a year abbreviation
!
      if ( modify(era) == 'B.C.' .and. yabbrv ) then

         call ctx%setmsg ( 'The Year may be abbreviated only if the ' &
         //            'year belongs to the Christian Era (A.D.) ' )
         call ctx%sigerr ( 'SPICE(BADTIMESTRING)' )
         return

      end if
!
!     If the era is B.C. we need to reset the year.
!
      if ( modify(era) == 'B.C.' ) then
         tvec(yr) = 1.0_wp - tvec(yr)
      end if

!
!     If there is a A.M. or P.M. time string modifier, we need to adjust
!     the hours component of the time.
!
      if     ( modify(ampm) == 'P.M.'   ) then

         if ( tvec(hr) < 12.0_wp ) then
            tvec(hr) = tvec(hr) + 12.0_wp
         end if

      else if ( modify(ampm) == 'A.M.' ) then

         if ( tvec(hr) >= 12.0_wp ) then
            tvec(hr) = tvec(hr) - 12.0_wp
         end if

      end if

!
!     If the year has been abbreviated, we need to convert it
!     to the proper range.  In addition we assume a year less
!     than 100 that is not qualified with the B.C. or A.D. era
!     string is in fact an abbreviated year.
!
      year = nint( tvec(yr) )

      if ( yabbrv ) then

         call       texpyr ( ctx, year )
         tvec(yr) = real( year, wp )

      else if (       year        < 100 &
                .and. modify(era) == ' ' ) then

         call       texpyr ( ctx, year )
         tvec(yr) = real( year, wp )

      end if

!
!     We may need to convert to the Gregorian Calendar, now is
!     the time to do so.
!
      if ( calndr == mixed ) then
!
!        We need to check the components.
!
         if ( type == 'YD' ) then

            dojul =            tvec(yr) < 1582.0_wp &
                   .or. (      tvec(yr) == 1582.0_wp &
                         .and. tvec(dy) <  279.0_wp )

         else

            dojul =             tvec(yr) < 1582.0_wp &
                   .or. (       tvec(yr) <= 1582.0_wp &
                          .and. tvec(mm) <   10.0_wp ) &
                   .or. (       tvec(yr) <= 1582.0_wp &
                          .and. tvec(mm) <=   10.0_wp &
                          .and. tvec(dy) <    6.0_wp )

         end if

      else if ( calndr == juln ) then

         dojul = .true.

      else

         dojul = .false.

      end if
!
!     If the input string is from the julian calendar, we need
!     to convert it to Gregorian.  We also need to save the original
!     year value in the unlikely event it is needed for a later
!     diagnostic message.
!
      if ( dojul ) then

         if ( type == 'YD' ) then

            year   = dint( tvec(yr) )
            month  = 1
            day    = dint( tvec(dy) )
            frac   = tvec(dy) - real(day, wp)
            orgnyr = year

            call jul2gr ( ctx, year, month, day, doy )

            tvec(yr) = real(year, wp)
            tvec(dy) = real(doy, wp ) + frac

         else

            year   = dint ( tvec(yr) )
            month  = dint ( tvec(mm) )
            day    = dint ( tvec(dy) )
            frac   = tvec(dy) - real(day, wp)
            orgnyr = year

            call jul2gr ( ctx, year, month, day, doy )

            tvec(yr) = real(year, wp)
            tvec(mm) = real(month, wp)
            tvec(dy) = real(day, wp) + frac

         end if

      else

         orgnyr = dint( tvec(yr) )

      end if

!
!     The TDT and TDB calendars don't need to worry about time
!     zone adjustments.
!
      if      ( modify(system) == 'TDT' ) then

         call ttrans ( ctx, forml, 'FORMAL', tvec )
         call ttrans ( ctx, 'TDT',  'TDB',    tvec )
         et = tvec(1)
         return

      else if ( modify(system) == 'TDB' ) then

         call ttrans ( ctx, forml, 'FORMAL', tvec(1) )
         et = tvec(1)
         return

      end if

!
!     If a time zone has been specified, we need to convert
!     from the time zone components to UTC components.
!
      if ( zoned  ) then
!
!        A time zone was specified explicitly in the input
!        string.  We need to compute the hour and minute offsets
!        associated with the time zone.
!
         tvec(hr) = tvec(hr) - hoff
         tvec(mn) = tvec(mn) - moff
         secs     = tvec(sc)
         tvec(sc) = 0.0_wp

         call ttrans ( ctx, forml, forml, tvec )

         tvec(sc) = secs

      end if
!
!     If we decided to forgo the leapseconds check earlier
!     now is the time to do it.  We've now got Gregorian UTC
!     time components.
!

      if ( adjust ) then

         call tchckd ( ctx, check )
         call tparch ( ctx, 'YES' )

         mods         = .false.
         modify(ampm) = ' '

         call tcheck ( ctx, tvec,  type, mods, modify, ok, error )

      else

         ok = .true.

      end if

      if ( ok ) then
!
!        That's it we are ready to rumble.
!
         call ttrans ( ctx, type, 'TDB', tvec )
         et = tvec(1)
         return

      end if

!     ===============================================================
!     If you are still here, it is because OK was .FALSE. in the test
!     above.  The only way this can happen is if the seconds were
!     not in the expected range.  The rest of the code is a diagnosis
!     of this problem.  (This is a nuisance case that is
!     unlikely to occur very often.)
!

      if ( zoned .and. dojul ) then

         error  = 'The seconds component of ''#'' is out of range. ' &
         //       'On the Julian Calendar ' &
         //       'in the specified time zone  (#) leapseconds ' &
         //       'can occur during the year # only in the ' &
         //       'second that immediately follows the time #:#:59 ' &
         //       'on  # # and # #. '

         call repmc_inplace ( error,  '#', timstr )
         call repmc_inplace ( error , '#', modify(zone)(3:) )

      else if ( zoned ) then
!
!        If we had a time zone, we want to say what time zone
!        in the output string.
!
         error  = 'The seconds component of ''#'' is out of range. ' &
         //       'In the specified time zone  (#) leapseconds ' &
         //       'can occur during the year # only in the ' &
         //       'second that immediately follows the time #:#:59 ' &
         //       'on  # # and # #.'

         call repmc_inplace ( error,  '#', timstr )
         call repmc_inplace ( error , '#', modify(zone)(3:) )

      else
!
!        No time zone, this case can only occur if we interpreted
!        the input string as a date on the Julian Calendar
!
         error  = 'The seconds component of ''#'' is out of range. ' &
         //       'Leapseconds can occur during the year # of ' &
         //       'the Julian calendar only in the second that ' &
         //       'immediately follows the time #:#:59  on # # ' &
         //       'and # #.'' '

         call repmc_inplace ( error,  '#', timstr )

      end if

!
!     First fill in the year portion of the error message.
!
      call repmi_inplace ( error , '#', orgnyr )

      mon(1) =  6.0_wp
      mon(2) = 12.0_wp

      mdy(1) = 30.0_wp
      mdy(2) = 31.0_wp
!
!     Next Fill in the hours and minutes. Recall that leapseconds
!     occur during the last second of the 59'th minute of the 23'rd
!     hour UTC.  So in the new time zone, it occurs in the 59'th + MOFF
!     minute of the 23'rd + HOFF hour of the time zone.  We adjust
!     these to account for hour roll over and day roll over.
!
      minute = 59.0_wp + moff

      if ( minute > 59.0_wp ) then
         minute = minute - 60.0_wp
         extra  = 1.0_wp
      else if ( minute < 0.0_wp ) then
         minute = minute + 60.0_wp
         extra  = -1.0_wp
      else
         extra  = 0.0_wp
      end if

      hour = 23.0_wp + hoff + extra

      if ( hour > 23 ) then
         hour = hour - 24
      end if

!
!     Convert the hours and minutes to strings and place the
!     strings in the message.
!
      call dpfmt ( ctx, hour,   '0x', hstr )
      call dpfmt ( ctx, minute, '0x', mstr )

      call repmc_inplace ( error , '#', hstr )
      call repmc_inplace ( error , '#', mstr )

!
!     Last step we generate the month and day corresponding
!     to Dec 31, 23:59, and Jun 30, 23:59.  We only want the
!     dates that belong to the original year.  We could
!     probably figure out the right year to use, but with Julian
!     date possibly messing everything up, we just use the
!     current year and the one before.  If you keep in mind that
!     the Julian Year is always less than the Gregorian year and
!     that the offsets can only push you into the next year, you
!     can determine that we want to start with what ever current
!     UTC year we have and work backwards until we have the
!     year corresponding to the original year.  Since the current
!     UTC year was constructed from the input original year, we
!     only have to step back at most 1 year to get all the dates
!     that might have leapseconds in the user specified year
!     of whatever calendar happens to be in use.
!
      cyear = dint( tvec(yr) )

      do gyear = cyear, cyear-1, -1

         do i = 1, 2

            tvec(1) = real(gyear, wp)
            tvec(2) = mon(i)
            tvec(3) = mdy(i)
            tvec(4) = 23.0_wp + hoff
            tvec(5) = 59.0_wp + moff
            tvec(6) = 0.0_wp
!
!           Normalize the time vector.
!
            call ttrans ( ctx, 'YMDF', 'YMDF', tvec )

            year   = nint(tvec(1))
            month  = nint(tvec(2))
            day    = nint(tvec(3))

            if ( dojul ) then
               call gr2jul ( ctx, year, month, day, doy )
            end if

            if ( year == orgnyr ) then

               call repmc_inplace ( error, '#', mname(month) )
               call repmi_inplace ( error, '#', day )

            end if

         end do

      end do

      call ctx%setmsg ( error  )
      call ctx%sigerr ( 'SPICE(BADTIMESTRING)' )
      return
      end associate
      end subroutine str2et

      !> Parse an aberration correction string; return attributes.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ABCORR         is a string representing a aberration
      !>                    correction.  The supported values are:
      !>
      !>                       'CN'
      !>                       'CN+S'
      !>                       'LT'
      !>                       'LT+S'
      !>                       'NONE'
      !>                       'RL'
      !>                       'RL+S'
      !>                       'S'
      !>                       'XCN'
      !>                       'XCN+S'
      !>                       'XLT'
      !>                       'XLT+S'
      !>                       'XRL'
      !>                       'XRL+S'
      !>                       'XS'
      !>
      !>                    Note that some values not supported by the
      !>                    SPICELIB SPK subsystem are supported by
      !>                    this routine:
      !>
      !>                       - The letter 'R' indicates relativistic
      !>                         corrections.
      !>
      !>                       - Stellar aberration-only corrections are
      !>                         indicated by the strings
      !>
      !>                            'S'
      !>                            'XS'
      !>
      !>                    Case and embedded blanks are not significant in
      !>                    ABCORR.
      !>
      !>## Detailed Output
      !>
      !>     ATTBLK         is a block of logical flags indicating the
      !>                    attributes of the aberration correction
      !>                    specified by ABCORR.  The attributes are:
      !>
      !>                       - Is the correction "geometric"?
      !>
      !>                       - Is light time correction indicated?
      !>
      !>                       - Is stellar aberration correction indicated?
      !>
      !>                       - Is the light time correction of the
      !>                         "converged Newtonian" variety?
      !>
      !>                       - Is the correction for the transmission
      !>                         case?
      !>
      !>                       - Is the correction relativistic?
      !>
      !>                    The structure of ATTBLK is defined in the
      !>                    include file
      !>
      !>                       zzabcorr.inc
      !>
      !>                    The size of ATTBLK and the offsets of the
      !>                    component flags are defined there.
      !>
      !>## Parameters
      !>
      !>     See INCLUDE file zzabcorr.inc.
      !>
      !>## Exceptions
      !>
      !>     1) If the input aberration correction choice is not recognized,
      !>        the error SPICE(INVALIDOPTION) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Many SPICELIB routines have logic branches based on the
      !>     attributes of aberration corrections.  Much duplicated
      !>     parsing code can be avoided by using this routine.
      !>
      !>     In particular, the routine ZZCOREPC uses this routine
      !>     to combine an epoch and light time value to compute
      !>     a light-time-adjusted epoch.
      !>
      !>## Examples
      !>
      !>     See ZZCOREPC.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) This routine recognizes some aberration corrections not
      !>        handled by most SPICELIB routines.  Callers should do
      !>        their own checking to ensure the parsed correction is
      !>        acceptable.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS)
      !>
      !>        Efficiency updates: moved CHKIN inside the exception block;
      !>        replaced CMPRSS/UCASE with LJUCRS.
      !>
      !>-    SPICELIB Version 1.0.0, 13-DEC-2004 (NJB)
      module subroutine zzprscor ( ctx, abcorr, attblk )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      use pepper_strings, only: ljucrs
      use pepper_inc_zzabcorr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: abcorr  !! Aberration correction string.
      logical, intent(out) :: attblk ( * )  !! Aberration correction attribute block.
      character(corlen), parameter :: corlst ( nabcor ) = [ character(corlen) :: &
         'CN', 'CN+S', 'LT', 'LT+S', 'NONE', 'RL', 'RL+S', 'S',&
         'XCN', 'XCN+S', 'XLT', 'XLT+S', 'XRL', 'XRL+S', 'XS' ]
!
!     Local variables
!
!     PEPPER: the table (in increasing order of the strings, as the
!     binary search needs; SPICELIB sorted it on the first call).
!
      character(corlen) :: tmpcor

      integer(ip) :: i
      integer(ip) :: loc

      logical, parameter :: conv ( nabcor ) = [ &
         .true., .true., .false., .false., .false., .false., .false., .false.,&
         .true., .true., .false., .false., .false., .false., .false. ]
      logical, parameter :: geo  ( nabcor ) = [ &
         .false., .false., .false., .false., .true., .false., .false., .false.,&
         .false., .false., .false., .false., .false., .false., .false. ]
      logical, parameter :: lt   ( nabcor ) = [ &
         .true., .true., .true., .true., .false., .true., .true., .false.,&
         .true., .true., .true., .true., .true., .true., .false. ]
      logical, parameter :: rel  ( nabcor ) = [ &
         .false., .false., .false., .false., .false., .true., .true., .false.,&
         .false., .false., .false., .false., .true., .true., .false. ]
      logical, parameter :: stl  ( nabcor ) = [ &
         .false., .true., .false., .true., .false., .false., .true., .true.,&
         .false., .true., .false., .true., .false., .true., .true. ]
      logical, parameter :: xmit ( nabcor ) = [ &
         .false., .false., .false., .false., .false., .false., .false., .false.,&
         .true., .true., .true., .true., .true., .true., .true. ]

!     It is recommended that, for maintainability, the correction
!     strings be kept in increasing order in this list.  However,
!     this routine does not rely on the strings being ordered
!     in this data statement:  the strings and associated values
!     are ordered at run time.
!

      if ( ctx%failed() ) return

!
!     Obtain a blank-free, upper-case copy of the aberration
!     correction string.
!
      call ljucrs ( 0, abcorr, tmpcor )

!
!     Search the list for the aberration correction string.
!
      loc = bsrchc ( tmpcor, nabcor, corlst )

      if ( loc == 0 ) then

         call ctx%setmsg ( 'Aberration correction specification # is not '// &
                       'recognized.'                                  )
         call ctx%errch  ( '#',  abcorr                                   )
         call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                         )
         return

      end if

!
!     Set the output flags.
!
      attblk( geoidx )  =  geo   ( loc )
      attblk( ltidx  )  =  lt    ( loc )
      attblk( stlidx )  =  stl   ( loc )
      attblk( cnvidx )  =  conv  ( loc )
      attblk( xmtidx )  =  xmit  ( loc )
      attblk( relidx )  =  rel   ( loc )

      return
      end subroutine zzprscor

      !> Validate an aberration correction string suitable for use by
      !> the SPK system; return attributes.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ABCORR         is a string representing a aberration
      !>                    correction.  The supported values are:
      !>
      !>                       'CN'
      !>                       'CN+S'
      !>                       'LT'
      !>                       'LT+S'
      !>                       'NONE'
      !>                       'XCN'
      !>                       'XCN+S'
      !>                       'XLT'
      !>                       'XLT+S'
      !>
      !>                    Note that some values not supported by the
      !>                    SPICELIB SPK subsystem are supported by
      !>                    the underlying routine ZZPRSCOR:
      !>
      !>                       - The letter 'R' indicates relativistic
      !>                         corrections.
      !>
      !>                       - Stellar aberration-only corrections are
      !>                         indicated by the strings
      !>
      !>                            'S'
      !>                            'XS'
      !>
      !>                    This routine *does not* permit values that
      !>                    the SPK system doesn't handle.
      !>
      !>                    Case and embedded blanks are not significant in
      !>                    ABCORR.
      !>
      !>                    If ABCORR contains an unsupported value, this
      !>                    routine will signal an error.
      !>
      !>## Detailed Output
      !>
      !>     ATTBLK         is a block of logical flags indicating the
      !>                    attributes of the aberration correction
      !>                    specified by ABCORR.  The attributes are:
      !>
      !>                       - Is the correction "geometric"?
      !>
      !>                       - Is light time correction indicated?
      !>
      !>                       - Is stellar aberration correction indicated?
      !>
      !>                       - Is the light time correction of the
      !>                         "converged Newtonian" variety?
      !>
      !>                       - Is the correction for the transmission
      !>                         case?
      !>
      !>                       - Is the correction relativistic? (This
      !>                         value is always .FALSE. for aberration
      !>                         correction specifications allowed by
      !>                         this routine.)
      !>
      !>                    The structure of ATTBLK is defined in the
      !>                    include file
      !>
      !>                       zzabcorr.inc
      !>
      !>                    The size of ATTBLK and the offsets of the
      !>                    component flags are defined there.
      !>
      !>## Parameters
      !>
      !>     See INCLUDE file zzabcorr.inc.
      !>
      !>## Exceptions
      !>
      !>     1) If the input aberration correction choice is not allowed,
      !>        the error SPICE(INVALIDOPTION) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is similar to ZZPRSCOR, but stellar aberration-only
      !>     and relativistic corrections specifications are not allowed
      !>     by this routine. The allowed values are precisely those allowed
      !>     by SPKEZR.
      !>
      !>## Examples
      !>
      !>     See ZZGFOCIN.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) This routine recognizes some aberration corrections not
      !>        handled by most SPICELIB routines.  Callers should do
      !>        their own checking to ensure the parsed correction is
      !>        acceptable.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 11-APR-2008 (NJB)
      module subroutine zzvalcor ( ctx, abcorr, attblk )
      use pepper_inc_zzabcorr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: abcorr  !! Aberration correction string.
      logical, intent(out) :: attblk ( * )  !! Aberration correction attribute block.

      if ( ctx%failed() ) return

!
!     Parse the aberration correction string and obtain
!     an attribute block.
!
      call zzprscor ( ctx, abcorr, attblk )

      if ( ctx%failed() ) return

!
!     Check the attribute block. We don't allow relativistic
!     corrections.
!
      if ( attblk(relidx) ) then

         call ctx%setmsg ( 'Aberration correction specification # calls ' &
         //            'for relativistic corrections, which are ' &
         //            'not supported.'                               )
         call ctx%errch  ( '#',  abcorr                                   )
         call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                         )
         return

      end if

!
!     Stellar aberration corrections are allowed only if light
!     time corrections are specified as well.
!
      if (  attblk(stlidx)  .and.  ( .not. attblk(ltidx) )  ) then

         call ctx%setmsg ( 'Aberration correction specification # calls ' &
         //            'for stellar aberration correction without ' &
         //            'light time correction; this combination ' &
         //            'is not supported.'                            )
         call ctx%errch  ( '#',  abcorr                                   )
         call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                         )
         return

      end if

      return
      end subroutine zzvalcor

      !> Compute an aberration corrected epoch, given an aberration
      !> correction specification, an epoch, and a light time.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ABCORR         is a string representing a aberration
      !>                    correction.  The supported values are:
      !>
      !>                       'CN'
      !>                       'CN+S'
      !>                       'LT'
      !>                       'LT+S'
      !>                       'NONE'
      !>                       'RL'
      !>                       'RL+S'
      !>                       'S'
      !>                       'XCN'
      !>                       'XCN+S'
      !>                       'XLT'
      !>                       'XLT+S'
      !>                       'XRL'
      !>                       'XRL+S'
      !>                       'XS'
      !>
      !>                    Note that some values not supported by the
      !>                    SPICELIB SPK subsystem are supported by
      !>                    this routine:
      !>
      !>                       - The letter 'R' indicates relativistic
      !>                         corrections.
      !>
      !>                       - Stellar aberration-only corrections are
      !>                         indicated by the strings
      !>
      !>                            'S'
      !>                            'XS'
      !>
      !>                    Case and leading and trailing blanks are not
      !>                    significant in ABCORR.
      !>
      !>     ET             is an epoch, expressed as seconds past J2000 TDB.
      !>
      !>     LT             is a light time value, expressed as TDB seconds.
      !>
      !>## Detailed Output
      !>
      !>     ETCORR         is the input epoch ET, corrected for light time:
      !>
      !>                       If the specified aberration correction calls
      !>                       for some type of light time correction (normal,
      !>                       converged Newtonian, relativistic), LT will be
      !>                       added to or subtracted from ET.  If the
      !>                       correction is of the transmission type, then
      !>
      !>                          ETCORR = ET + LT
      !>
      !>                       If the correction is of the reception type,
      !>                       then
      !>
      !>                          ETCORR = ET - LT
      !>
      !>                       If no light time correction is specified, then
      !>
      !>                          ETCORR = ET
      !>
      !>## Parameters
      !>
      !>     See INCLUDE file zzabcorr.inc.
      !>
      !>## Exceptions
      !>
      !>     1) If the input aberration correction choice is not recognized,
      !>        the error SPICE(INVALIDOPTION) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Many SPICELIB routines have logic branches based on the
      !>     attributes of aberration corrections.  Much duplicated
      !>     parsing code can be avoided by using this routine.
      !>
      !>## Examples
      !>
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 24-NOV-2004 (NJB)
      module subroutine zzcorepc ( ctx, abcorr, et, lt, etcorr )
      use pepper_kinds, only: wp
      use pepper_inc_zzabcorr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: abcorr  !! Aberration correction string.
      real(wp), intent(in) :: et  !! Ephemeris time, seconds past J2000.
      real(wp), intent(in) :: lt  !! Light time.
      real(wp), intent(out) :: etcorr  !! Light time-corrected epoch.
!
!     Local variables
!
      logical :: corblk ( abatsz )

      if ( ctx%failed() ) return

!
!     Parse the aberration correction string.  Obtain a correction
!     attribute block.
!
      call zzprscor ( ctx, abcorr, corblk )

      if ( corblk(ltidx) ) then
!
!        Light time corrections are used.  The output epoch
!        must be adjusted according to whether the correction
!        is for received or transmitted radiation.
!
         if ( corblk(xmtidx) ) then
!
!           This is the transmission case.
!
            etcorr = et + lt

         else
!
!           This is the reception case.
!
            etcorr = et - lt

         end if

      else
!
!        Light time corrections are not used.
!
         etcorr = et

      end if

      return
      end subroutine zzcorepc

end submodule pepper_time
