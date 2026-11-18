!> String operations that check their inputs (signal errors): replacing markers with numbers,
!> inserting and removing substrings, conversions, ...
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_text

   implicit none

contains

      !> Cycle the contents of a character string to the left or right.
      !>
      !>## Detailed Input
      !>
      !>     DIR      is the direction in which the characters in the
      !>              string are to be cycled.
      !>
      !>                    'L' or 'l'  to cycle left.
      !>                    'R' or 'r'  to cycle right.
      !>
      !>     NCYCLE   is the number of times the characters in the string
      !>              are to be cycled.
      !>
      !>     INSTR    is the string to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     OUTSTR   is the input string after it has been cycled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the direction flag is not one of the acceptable values
      !>         'r', 'R', 'l', 'L',  the error SPICE(INVALIDDIRECTION) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A string is cycled when its contents are shifted to the left
      !>     or right by one place. A character pushed off one end of the
      !>     string is brought around to the other end of the string instead
      !>     of disappearing.
      !>
      !>     Leading and trailing blanks are treated just like any other
      !>     characters.
      !>
      !>     If the output string is not large enough to contain the input
      !>     string, the cycled string is truncated on the right.
      !>
      !>## Examples
      !>
      !>     'abcde'   cycled left twice becomes               'cdeab'
      !>     'abcde '  cycled left twice becomes               'cde ab'
      !>     'abcde'   cycled right once becomes               'eabcd'
      !>     'Apple '  cycled left six times becomes           'Apple '
      !>     'Apple '  cycled right twenty-four times becomes  'Apple '
      !>
      !>## Restrictions
      !>
      !>     1)  The memory used for the output string must be identical to
      !>         that used for the input string or be disjoint from the input
      !>         string memory.
      !>
      !>         That is:
      !>
      !>            CALL CYCLEN ( STRING, DIR, NCYCLE, STRING )
      !>
      !>         will produce correct results with output overwriting input.
      !>
      !>            CALL CYCLEN ( STRING(4:20), DIR, NCYCLE, STRING(2:18) )
      !>
      !>         will produce garbage results.
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
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Fixed problem with unbalanced CHKIN/CHKOUT calls.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 6-FEB-1989 (WLT)
      !>
      !>      Error handling for bad direction flag added.
      module subroutine cyclec ( ctx, instr, dir, ncycle, outstr )
      use pepper_kinds, only: ip
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: instr  !! String to be cycled.
      character(1), intent(in) :: dir  !! Direction to cycle.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      character(*), intent(out) :: outstr  !! Cycled string.

      character(1) :: last
      character(1) :: temp

      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m
      integer(ip) :: n
      integer(ip) :: limit

      if ( ctx%failed() ) return
!
!     Get the length of the input string.
!
      n     = len ( instr  )
      limit = len ( outstr )

!
!     A left cycle is the same as a right cycle by the opposite of
!     NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum positive right cycle that is equivalent to the inputs.
!
      if (      (dir == 'L') &
           .or. (dir == 'l') ) then

         k = mod ( - ncycle, n )

      else if (      ( dir == 'R') &
                .or. ( dir == 'r') ) then

         k = mod (   ncycle, n )

      else

         call ctx%setmsg ( 'The direction flag should be one of the '     // &
                       'following: ''r'', ''R'', ''l'', ''L''.  It'   // &
                       ' was #.'   )

         call ctx%errch  ( '#', dir )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)' )
         return

      end if

      if      ( k < 0 ) then
         k = k + n
      else if ( k == 0 ) then
         return
      end if

!
!     As to the method for performing the cycle in place, we need a
!     few preliminaries.
!
!        1.  Since we are performing a cycle on the input string we
!            can regard the letters of the string as being attached
!            to a circle at N equally spaced points.  Thus a cycle
!            by K has the effect of moving the position of each letter
!            to the K'th point from its current position along the
!            circle.  (The first point from its position is the
!            adjacent point.)
!
!        2.  If we start at some point on the circle and begin moves to
!            other points of the circle by always moving K points
!            at a time, how long will it take until we get back to
!            the starting point?  Answer: N/gcd(K,N)
!
!               Justification of the above answer.
!
!               a.  If we count all of the points that we move past or
!                   onto in such a trip (counting second, third, ...
!                   passes), we will find that we have
!                   moved past or onto i*K points after i steps.
!
!               b.  In order to get back to the starting point we will
!                   have to move past or onto a multiple of N points.
!
!               c.  The first time we will get back to the starting
!                   point is the smallest value of i such that i*K
!                   is a multiple of N.  That value is N/g.c.d.(K,N)
!                   where g.c.d stands for the greatest common divisor
!                   of K and N. Lets call this number M.
!
!                      i.  To see that this is the smallest number we
!                          first show that K*M is in fact a multiple of
!                          N.  The product K*M = K * ( N / gcd(K,N) )
!                                              = N * ( K / gcd(K,N) )
!
!                          Since gcd(K,N) evenly divides K, K/gcd(K,N)
!                          is an integer.  Thus K*M = N*I for some
!                          integer I ( = K / gcd(K,N) ).
!
!                      ii. The least common multiple of K and N is:
!                          K*N / gcd(K,N)  thus the first multiple
!                          of K that is also a multiple of N is the
!                          N/ gcd(K,N) 'th multiple of K.
!
!        3.  The closest stopping point on the circle will be gcd(K,N)
!            points away from our starting point.  To see this recall
!            that we make N/gcd(K,N) moves of size K inorder to get
!            back to the starting point.  The stopping points must
!            be equally spaced around the circle since the set of
!            points must look the same from any one of the points
!            visited --- after all we could get the same set by just
!            starting at one of those visited and making N/gcd(K,N)
!            moves.  But the set of N/gcd(K,N) equally space points
!            out of the original N must be gcd(K,N) points apart.
!
!        4.  To visit every point on the circle we could
!
!            a.  Pick a starting point
!            b.  Take N/gcd(K,N) steps of size K (bringing us back
!                to our starting point.
!            c.  move forward 1 point
!            d.  repeat steps a. b. and c. gcd(K,N) times.
!
!        5.  If in addition to moving around the circle by the
!            prescription of 4. above we:
!               a. pick up the letter at a position when we stop there
!                  (starting being the same as stopping)
!               b. put down the letter we had picked up at a previous
!                  point.
!            then we will cycle every letter by the prescribed value
!            of K.
!
!     In this case the code is much shorter than its explanation.
!

      g = gcd ( k, n )
      m = n / g

      do i = 1, g

         l    = i
         last = instr (l:l)

         do j = 1, m

            l = l + k

!
!           Compute L mod N.
!
            if ( l > n ) then
               l = l - n
            end if

            temp        = instr (l:l)

!
!           Make sure there is someplace to put the letter picked up
!           in the previous pass through the loop.
!
            if ( l <= limit ) then
               outstr(l:l) = last
            end if

            last        = temp

         end do

      end do

      return
      end subroutine cyclec

      !> Replace a marker with a double precision number.
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
      !>## Detailed Output
      !>
      !>     OUT      is the string obtained by substituting the text
      !>              representation of VALUE for the first occurrence of
      !>              MARKER in the input string.
      !>
      !>              The text representation of VALUE is in scientific
      !>              notation, having the number of significant digits
      !>              specified by SIGDIG. The representation of VALUE is
      !>              produced by the SPICELIB routine DPSTR; see that routine
      !>              for details concerning the representation of double
      !>              precision numbers.
      !>
      !>              OUT and IN must be identical or disjoint.
      !>
      !>## Parameters
      !>
      !>     MAXLDP   is the maximum expected length of the text representation
      !>              of a double precision number. 23 characters are
      !>              sufficient to hold any result returned by the SPICELIB
      !>              routine DPSTR. (See $Restrictions)
      !>
      !>              This routine assumes that the input d.p. value is such
      !>              that its string representation contains no more than
      !>              MAXLDP characters.
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
      !>     1) The following example illustrate the use of REPMD to
      !>        replace a marker within a string with the text representation
      !>        of a double precision value.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REPMD_EX1
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
      !>C     1. Single marker, two significant digits.
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMD ( INSTR, MARKER, 5.0D1, 2, OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 1: Single marker, two significant '
      !>     .        // 'digits.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     2. Multiple markers, three significant digits.
      !>C
      !>      MARKER = ' XX '
      !>      INSTR  = 'Left > Right endpoint. Left: XX; Right: XX'
      !>
      !>      CALL REPMD ( INSTR, MARKER, -5.2D-9, 3, OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 2: Multiple markers, three '
      !>     .        // 'significant digits.'
      !>      WRITE(*,*) '   Input : ', INSTR
      !>      WRITE(*,*) '   Output: ', OUTSTR
      !>      WRITE(*,*)
      !>
      !>C
      !>C     3. Excessive significant digits.
      !>C
      !>      MARKER = '#'
      !>      INSTR  = 'Invalid value. The value was:  #'
      !>
      !>      CALL REPMD ( INSTR, MARKER, 5.0D1, 100, OUTSTR )
      !>
      !>      WRITE(*,*) 'Case 3: Excessive significant digits.'
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
      !>         Case 1: Single marker, two significant digits.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  5.0E+01
      !>
      !>         Case 2: Multiple markers, three significant digits.
      !>            Input : Left > Right endpoint. Left: XX; Right: XX
      !>            Output: Left > Right endpoint. Left: -5.20E-09; Right: XX
      !>
      !>         Case 3: Excessive significant digits.
      !>            Input : Invalid value. The value was:  #
      !>            Output: Invalid value. The value was:  5.0000000000000E+01
      !>
      !>        Note that, in Case #3 even though 100 digits of precision were
      !>        requested, only 14 were returned.
      !>
      !>## Restrictions
      !>
      !>     1)  The maximum number of significant digits returned is 14.
      !>
      !>     2)  This routine makes explicit use of the format of the string
      !>         returned by the SPICELIB routine DPSTR; should that routine
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
      !>        $Particulars and extended the description of MAXLDP in
      !>        $Parameters.
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
      module subroutine repmd ( ctx, in, marker, value, sigdig, out )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: dpstr, zzrepsub
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      real(wp), intent(in) :: value  !! Replacement value.
      integer(ip), intent(in) :: sigdig  !! Significant digits in replacement text.
      character(*), intent(out) :: out  !! Output string.

      integer(ip), parameter :: maxldp = 23

      character(maxldp) :: substr

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
      call dpstr  ( value, sigdig, substr )

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
      end subroutine repmd

      !> Replace a marker with an integer.
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
      !>     VALUE    is an arbitrary integer.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the string obtained by substituting the text
      !>              representation of VALUE for the first occurrence of
      !>              MARKER in the input string.
      !>
      !>              OUT and IN must be identical or disjoint.
      !>
      !>## Parameters
      !>
      !>     MAXLI    is the maximum expected length of the text representation
      !>              of an integer. 11 characters are sufficient to hold any
      !>              integer whose absolute value is less than 10 billion.
      !>
      !>              This routine assumes that the input integer is such that
      !>              its string representation contains no more than MAXLI
      !>              characters.
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
      !>     1) The following example illustrate the use of REPMI to
      !>        replace a marker within a string with the text representation
      !>        of an integer value.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REPMI_EX1
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
      !>      CALL REPMI ( INSTR, MARKER, 75, OUTSTR )
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
      !>      INSTR  = 'Left > Right endpoint. Left: XX; Right: XX'
      !>
      !>      CALL REPMI ( INSTR, MARKER, 2035, OUTSTR )
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
      !>            Output: Invalid value. The value was:  75
      !>
      !>         Case 2: Multiple markers.
      !>            Input : Left > Right endpoint. Left: XX; Right: XX
      !>            Output: Left > Right endpoint. Left: 2035; Right: XX
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
      !>        code example based on existing fragments.
      !>
      !>        Added REPML to the list of available replace marker routines in
      !>        $Particulars and extended MAXLI parameter description in
      !>        $Parameters.
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
      module subroutine repmi ( ctx, in, marker, value, out )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, zzrepsub
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: marker  !! Marker to be replaced.
      integer(ip), intent(in) :: value  !! Replacement value.
      character(*), intent(out) :: out  !! Output string.

      integer(ip), parameter :: maxli = 11

      character(maxli) :: substr

      integer(ip) :: mrknbf
      integer(ip) :: mrknbl

      integer(ip) :: mrkpsb
      integer(ip) :: mrkpse

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
!     integer to text, and substitute the text for the marker.
!
      call intstr ( value, substr )

      call zzrepsub ( in,  mrkpsb,  mrkpse,  substr( :len_trim(substr) ),  out  )

      return
      end subroutine repmi

      !> Replace the substring (LEFT:RIGHT) with a string of any length.
      !>
      !>## Detailed Input
      !>
      !>     IN       is an arbitrary character string.
      !>
      !>     LEFT,
      !>     RIGHT    are the ends of the substring to be replaced.
      !>              Legitimate substrings satisfy the following
      !>              conditions
      !>
      !>                  RIGHT > LEFT - 2
      !>                  LEFT  > 1
      !>                  RIGHT < LEN(STRING) + 1
      !>
      !>              This allows users to refer to zero-length substrings
      !>              (null substrings) of IN.
      !>
      !>     STRING   is the replacement string. Essentially, the
      !>              substring (LEFT:RIGHT) is removed from the
      !>              input string, and STRING is inserted at the
      !>              point of removal.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the resulting string. OUT may overwrite IN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If RIGHT is one less than LEFT, the substring to
      !>         replace will be the null substring. In this case,
      !>         STRING will be inserted between IN(:RIGHT) and IN(LEFT:).
      !>
      !>     2)  If LEFT is smaller than one, the error SPICE(BEFOREBEGSTR)
      !>         is signaled.
      !>
      !>     3)  If RIGHT is greater than the length of the input string,
      !>         the error SPICE(PASTENDSTR) is signaled.
      !>
      !>     4)  If RIGHT is less than LEFT-1, the error SPICE(BADSUBSTR)
      !>         is signaled.
      !>
      !>     5)  Whenever the output string is too small to hold the result,
      !>         the result is truncated on the right.
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
      !>           CALL   REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
      !>           LEFT = WDINDX ( LINE, 'AND' )
      !>        END DO
      !>
      !>     This routine can also be used to insert substring between
      !>     two characters. Consider the string:
      !>
      !>         IN   = 'The defendant,, was found innocent.'
      !>
      !>     to insert ' Imelda Marcos' between the first and second commas
      !>     determine the location of the pair ',,'
      !>
      !>        RIGHT = POS ( IN, ',,', 1 )
      !>        LEFT  = RIGHT + 1
      !>
      !>     then
      !>
      !>        CALL REPSUB ( IN, LEFT, RIGHT, ' Imelda Marcos', OUT )
      !>
      !>     The output (OUT) will have the value:
      !>
      !>        'The defendant, Imelda Marcos, was found innocent.'
      !>
      !>## Restrictions
      !>
      !>     1)  The memory used by STRING and OUT must be disjoint. The memory
      !>         used by IN and OUT must be identical or disjoint.
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
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 17-JUN-1999 (WLT)
      !>
      !>        Fixed example code fragment.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 24-AUG-1990 (WLT) (IMU)
      module subroutine repsub ( ctx, in, left, right, string, out )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
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

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the lengths of all the strings involved in this transaction.
!
      inlen  = len ( in     )
      strlen = len ( string )
      outlen = len ( out    )

!
!     Reject bad inputs.
!
      if ( left < 1 ) then
         call ctx%setmsg ( 'REPSUB error: LEFT (#) must not be less '     // &
                       'than 1.' )

         call ctx%errint ( '#', left )
         call ctx%sigerr ( 'SPICE(BEFOREBEGSTR)' )
         return

      else if ( right > inlen ) then
         call ctx%setmsg ( 'REPSUB error: RIGHT (#) must not exceed '     // &
                       'length of IN (#).' )
         call ctx%errint ( '#', right )
         call ctx%errint ( '#', inlen )
         call ctx%sigerr ( 'SPICE(PASTENDSTR)' )
         return

      else if ( right < left-1 ) then
         call ctx%setmsg ( 'REPSUB error: LEFT (#) must not exceed '      // &
                       'RIGHT+1 (# + 1). ' )
         call ctx%errint ( '#', left  )
         call ctx%errint ( '#', right )
         call ctx%sigerr ( 'SPICE(BADSUBSTR)' )
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
      use(1) = min ( remain, left - 1 )

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

      if ( left + strlen > right ) then

         next = end

         do i = use(3), 1, -1
            out(next:next) = in(right+i:right+i)
            next           = next - 1
         end do

      else

         next = left + strlen

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
      end subroutine repsub

      !> Remove the substring (LEFT:RIGHT) from a character string.
      !>
      !>## Detailed Input
      !>
      !>     IN       is an input character string, from which a substring
      !>              is to be removed.
      !>
      !>     LEFT,
      !>     RIGHT    are the ends of the substring to be removed.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is equivalent to the
      !>              string that would be created by the concatenation
      !>
      !>                    OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : )
      !>
      !>              If the string is too long to fit into OUT, it is
      !>              truncated on the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LEFT > RIGHT, RIGHT < 1, LEFT < 1, RIGHT > LEN(IN), or
      !>         LEFT > LEN(IN), the error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Move the characters, beginning with RIGHT, one at a time to the
      !>     positions immediately following LEFT. This has the same effect
      !>     as the concatenation
      !>
      !>           OUT = IN(1 : LEFT-1) // IN(RIGHT+1 : )
      !>
      !>     Because this operation is not standard for strings of length (*),
      !>     this routine does not use concatenation.
      !>
      !>## Examples
      !>
      !>     The following examples illustrate the use of REMSUB.
      !>
      !>     IN                 LEFT  RIGHT        OUT
      !>     -----------------  ----  -----        ------------------------
      !>     'ABCDEFGHIJ'          3      5        'ABFGHIJ'
      !>     'The best rabbit'     5      8        'The  rabbit'
      !>     'The other woman'     1      4        'other woman'
      !>     'An Apple a day'      2      2        'A apple a day'
      !>     'An Apple a day'      5      2         An error is signaled.
      !>     'An Apple a day'      0      0         An error is signaled.
      !>     'An Apple a day'     -3      3         An error is signaled.
      !>
      !>     Whenever an error has been signaled, the contents of OUT are
      !>     unpredictable.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 05-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect invalid character
      !>         positions. If LEFT > RIGHT, RIGHT < 1, LEFT < 1,
      !>         RIGHT > LEN(IN), or LEFT > LEN(IN), an error is signaled.
      module subroutine remsub ( ctx, in, left, right, out )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      integer(ip), intent(in) :: left  !! Position of first character to be removed.
      integer(ip), intent(in) :: right  !! Position of last character to be removed.
      character(*), intent(out) :: out  !! Output string.

!
!     Other functions
!
      integer(ip) :: len

      integer(ip) :: inlen
      integer(ip) :: outlen

      integer(ip) :: l
      integer(ip) :: r

      integer(ip) :: i
      integer(ip) :: j

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If a character position is out of range, signal an error.
!
      if ( ( left  > right    )   .or. &
           ( right < 1        )   .or. &
           ( left  < 1        )   .or. &
           ( right > len (in) )   .or. &
           ( left  > len (in) ) )       then

         call ctx%setmsg ( 'Left location was *. Right location was *.' )
         call ctx%errint ( '*', left             )
         call ctx%errint ( '*', right            )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      else

         l = left
         r = right

      end if

!
!     How much of the input string will we use? And how big is the
!     output string?
!
      inlen  = len_trim(in)
      outlen = len    ( out )

!
!     Copy the first part of the input string. (One character at a
!     time, in case this is being done in place.)
!
      do i = 1, min ( l-1, outlen )

         out(i:i) = in(i:i)

      end do

!
!     Now move the rest of the string over.
!
      i = l
      j = r + 1

      do while ( ( i <= outlen ) .and. ( j <= inlen ) )

         out(i:i) = in(j:j)
         i        = i + 1
         j        = j + 1

      end do

!
!     Pad with blanks, if necessary.
!
      if ( i <= outlen ) then

         out(i: ) = ' '

      end if

      return
      end subroutine remsub

      !> Insert a substring into a character string at a specified
      !> location.
      !>
      !>## Detailed Input
      !>
      !>     IN       is an input character string, into which a substring
      !>              is to be inserted.
      !>
      !>     SUB      is the substring to be inserted. Leading and trailing
      !>              blanks are significant.
      !>
      !>     LOC      is the index in the input string at which the substring
      !>              is to be inserted. The range of LOC is 1:LEN(IN). To
      !>              append to the string, set LOC equal to LEN(IN)+1.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the output string. This is equivalent to the
      !>              string that would be created by the concatenation
      !>
      !>                 OUT = IN(1:LOC-1) // SUB // IN(LOC: )
      !>
      !>              If the output string is too long, it is truncated
      !>              on the right.
      !>
      !>              OUT may overwrite IN. OUT may NOT overwrite SUB.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LOC is not in the interval [1, LEN(IN)+1], the error
      !>         SPICE(INVALIDINDEX) is signaled.
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
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following table illustrates the use of INSSUB.
      !>
      !>           IN                SUB      LOC    OUT
      !>           ----------------- -------  ---    ------------------------
      !>           'ABCDEFGHIJ'      ' YXZ '    3    'AB XYZ CDEFGHIJ'
      !>           'The rabbit'      'best '    5    'The best rabbit'
      !>           ' other woman'    'The'      1    'The other woman'
      !>           'An Apple a day'  ' keeps'  15    'An Apple a day keeps'
      !>           'Apple a day'     'An '      0     An error is signaled.
      !>           'Apple a day'     'An '     -3     An error is signaled.
      !>           'An Apple a day'  ' keeps'  16     An error is signaled.
      !>
      !>     2) INSSUB could be used to insert substrings at any position
      !>        within the declared length of the input string. Use it to
      !>        add an ascii arrow (--->) at the end of the string.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INSSUB_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               STRSZ
      !>      PARAMETER           ( STRSZ = 20 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRSZ)     STRIN
      !>      CHARACTER*(STRSZ)     STROUT
      !>
      !>C
      !>C     Assign the input string.
      !>C
      !>      DATA                  STRIN / '0123456789         .' /
      !>
      !>C
      !>C     Insert a substring at index 17. This should leave 5
      !>C     blanks after the sequence of numbers. Note that the
      !>C     resulting string is truncated.
      !>C
      !>      CALL INSSUB ( STRIN, '--->', 17, STROUT )
      !>
      !>      WRITE(*,*) 'Input string:  ', STRIN
      !>      WRITE(*,*) 'Output string: ', STROUT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Input string:  0123456789         .
      !>         Output string: 0123456789      --->
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
      !>-    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated long error message for LOC out of range check,
      !>        providing valid range of values.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB)
      !>
      !>        Bug fixes made. Now does discovery check-in. Header sections
      !>        re-arranged. Some clean-up of header format done.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 24-OCT-1994 (NJB)
      !>
      !>        Bug fix: case where insertion location follows end of
      !>        input string is now handled correctly. Formerly, an
      !>        out-of-range substring bound violation was incurred in this
      !>        case.
      !>
      !>        Bug fix: use of SHIFTC routine in old implementation
      !>        resulted in output string being truncated at length
      !>        LEN(IN), which is not consistent with the routine's
      !>        specification.
      !>
      !>        Now does discovery check-in. Header sections re-arranged.
      !>        Some clean-up of header format done.
      !>
      !>-    Beta Version 2.0.0, 04-JAN-1989 (HAN)
      !>
      !>        If the location at which the substring is to be inserted is
      !>        not in the interval [1, LEN(IN)+1], an error is signaled.
      !>        Locations not within that interval refer to non-existent
      !>        characters positions. (To append to the string, set the
      !>        location equal to LEN(IN)+1.)
      module subroutine inssub ( ctx, in, sub, loc, out )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      character(*), intent(in) :: sub  !! Substring to be inserted.
      integer(ip), intent(in) :: loc  !! Position at which substring is to be inserted.
      character(*), intent(out) :: out  !! Output string.

      character(1) :: chr

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: inlen
      integer(ip) :: nmove
      integer(ip) :: outlen
      integer(ip) :: subend
      integer(ip) :: sublen
      integer(ip) :: to

      logical :: same

!
!     Discovery check-in is used in this routine.
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

!
!     If insertion occurs before the beginning of the string
!     or after INLEN + 1, signal an error.
!
      if (  ( loc < 1 )  .or.  ( loc > inlen+1 )  ) then

         call ctx%setmsg ( 'Index at which the substring is to be ' &
         //            'inserted is out of the valid range [1,#]. ' &
         //            'Requested index was *.'                    )
         call ctx%errint ( '#', inlen + 1                              )
         call ctx%errint ( '*', loc                                    )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

!
!     If the insertion occurs after the end of the output string,
!     just return the original string.  Don't do the assignment if
!     the output and input strings have equal values; the assignment
!     is not needed in this cause and could cause a run-time error if
!     OUT and IN refer to the same memory.
!
      same  =  out == in

      if ( loc > outlen ) then

         if ( .not. same ) then
            out = in
         end if

         return

      end if

!
!     At this point, we're guaranteed that
!
!        LOC  <  OUTLEN
!             -
!
!        LOC  <  INLEN + 1
!             -
!
!        LOC  >  0
!
!     The first part of the input string is copied without change
!     to the output string, if this first part is non-empty.
!
      if ( loc > 1 ) then
!
!        Again, do the assignment only if it's required.
!
         if ( .not. same ) then
            out(1 : loc-1) = in
         end if

      end if

!
!     The part following the new substring is shifted into place, if
!     there's both something to move and a place to put it.  Move the
!     rightmost characters first.
!
      subend  =  loc - 1 + sublen

      if (        ( loc    <= inlen  ) &
            .and. ( subend < outlen )  ) then

         nmove  =  min (  outlen-subend, inlen-loc+1  )

         do i   =  nmove,  1,  -1

            from         =  loc    + i - 1
            to           =  subend + i
            chr          =  in(from:from)
            out (to:to)  =  chr

         end do

      end if

!
!     And the new word is dropped into the middle.
!
      out( loc : min(subend,outlen) )  =  sub

!
!     Blank-pad the output string if necessary.
!
      if ( outlen > inlen+sublen ) then
         out( inlen+sublen+1 : ) = ' '
      end if

      return
      end subroutine inssub

      !> Shift the contents of a character string to the left or right.
      !> Characters moved past the beginning or end of the string are
      !> lost. Vacant spaces are filled with a specified character.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input character string.
      !>
      !>     DIR      is the direction in which the characters in the
      !>              string are to be shifted.
      !>
      !>                    'L' or 'l'  to shift left.
      !>                    'R' or 'r'  to shift right.
      !>
      !>     NSHIFT   is the number of times the string is to be
      !>              shifted.
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
      !>     1)  A negative shift in one direction is equal to a positive
      !>         shift in the other.
      !>
      !>     2)  If a legal direction ('L', 'l', 'R', 'r') is not supplied,
      !>         the error SPICE(ILLEGSHIFTDIR) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The first NSHIFT characters of the output string are filled
      !>     with the fill character, and the input string is appended.
      !>
      !>## Examples
      !>
      !>     If FILLC = ' '
      !>
      !>            'abcde'   shifted left twice becomes     'cde  '
      !>            'abcde'   shifted right once becomes     ' abcd'
      !>
      !>     If FILLC = '.'
      !>
      !>            '12345 '  shifted right once becomes     '.12345'
      !>            'Apple '  shifted left ten times becomes '......'
      !>
      !>## Restrictions
      !>
      !>     1)  SHIFTC is being maintained for historical reasons only.
      !>         To avoid the overhead imposed by the error handling in this
      !>         routine, use the equivalent routines SHIFTL and SHIFTR.
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
      !>-    Beta Version 1.1.0, 17-OCT-1988 (IMU)
      !>
      !>        Dick Simpson reported that the statement
      !>
      !>           OUT(N+1: ) = IN
      !>
      !>        which began the right-shift section failed on his Data
      !>        General, presumably because it requires temporary buffering
      !>        of characters. The new version seems to work for all cases.
      !>        It has been tested on the VAX and on the Sun (f77 compiler).
      module subroutine shiftc ( ctx, in, dir, nshift, fillc, out )
      use pepper_kinds, only: ip
      use pepper_strings, only: shiftl, shiftr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in  !! Input string.
      character(1), intent(in) :: dir  !! Direction to shift.
      integer(ip), intent(in) :: nshift  !! Number of times to shift.
      character(1), intent(in) :: fillc  !! Character to fill spaces left vacant.
      character(*), intent(out) :: out  !! Shifted string.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Hand off to one of the other routines.
!
      if ( dir == 'L' .or. dir == 'l' ) then

         if ( nshift >= 0 ) then
            call shiftl ( in, nshift, fillc, out )

         else
            call shiftr ( in, -nshift, fillc, out )
         end if

      else if ( dir == 'R' .or. dir == 'r' ) then

         if ( nshift >= 0 ) then
            call shiftr ( in, nshift, fillc, out )

         else
            call shiftl ( in, -nshift, fillc, out )
         end if

      else
         call ctx%setmsg ( 'Shift direction (#) must be L, l, R, or r.' )
         call ctx%errch  ( '#', dir )

         call ctx%sigerr ( 'SPICE(ILLEGSHIFTDIR)' )
      end if

      return
      end subroutine shiftc

      !> Validate an ID string: check for non-printing characters
      !> or excessive non-blank length.
      !>
      !>## Detailed Input
      !>
      !>     CLASS    is a descriptive string indicating the type of
      !>              object represented by ID. Examples are
      !>              'SPK segment identifier', 'DAF internal file name',
      !>              or 'EK table name'.
      !>
      !>              If the input ID is found to be invalid, CLASS is
      !>              used in the error message generated by this
      !>              routine.
      !>
      !>     MAXLEN   is the maximum allowed non-blank length of the
      !>              input ID string. If ID has any non-blank
      !>              characters at positions greater than MAXLEN,
      !>              an error will be signaled.
      !>
      !>     ID       is the input ID string to be checked. In order
      !>              to be considered valid, ID must contain only
      !>              printing characters and must satisfy the condition
      !>
      !>                 LASTNB( ID )  <  MAXLEN
      !>                               -
      !>
      !>## Detailed Output
      !>
      !>     None. See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If ID contains any nonprintable characters, the error
      !>         SPICE(NONPRINTABLECHARS) is signaled.
      !>
      !>     2)  If MAXLEN is non-positive, the error SPICE(INVALIDCOUNT) is
      !>          signaled.
      !>
      !>     3)  If ID contains any non-blank characters past position
      !>         MAXLEN, the error SPICE(IDSTRINGTOOLONG) is signaled.
      !>
      !>     4)  If CLASS contains any non-printing characters, the error
      !>         SPICE(NONPRINTABLECHARS) is signaled.
      !>
      !>     5)  CLASS is allowed to be blank. The word 'ID' is used in
      !>         place of the class string in any error messages in this
      !>         case.
      !>
      !>     6)  The error messages output by this routine have a maximum
      !>         length of 320 characters. If substitution of CLASS and
      !>         ID into the long messages causes overflow, the messages
      !>         will be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects: it validates an ID string
      !>     and signals an error if the ID has either of the following
      !>     problems:
      !>
      !>     -  There are non-printing characters in the ID string.
      !>
      !>     -  The last non-blank character in the string occurs at a
      !>        location having index higher than a specified value.
      !>
      !>     The error message signaled by this routine contains the offending
      !>     ID string and indicates the class of item to which ID belongs.
      !>     The form of the message is:
      !>
      !>         The <CLASS> <'ID'> is invalid; <reason>
      !>
      !>## Examples
      !>
      !>     1)  If
      !>
      !>            CLASS  = 'segment identifier'
      !>            MAXLEN =  40
      !>
      !>         and
      !>
      !>            ID = 'Example EK created on March 28, 1995 by NJB/NAIF'
      !>
      !>         the error message
      !>
      !>            The segment identifier 'Example EK created on March 28,
      !>            1995 by NJB/NAIF' is invalid; the last non-blank character
      !>            is located at position 48.
      !>
      !>         will be signaled.
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
      !>-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 16-JUN-1995 (NJB)
      module subroutine chckid ( ctx, class, maxlen, id )
      use pepper_kinds, only: ip
      use pepper_strings, only: frstnp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: class  !! A description of the class to which ID belongs.
      integer(ip), intent(in) :: maxlen  !! Maximum allowed non-blank length of ID.
      character(*), intent(in) :: id  !! The ID string to be validated.

      integer(ip) :: chrcod
      integer(ip) :: i
      integer(ip) :: l

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check CLASS before trying to use it in an error message.
!
      i  =  frstnp( class )

      if ( i > 0 ) then

         chrcod = ichar( class(i:i) )

         call ctx%setmsg ( 'The class string ''#'' is invalid; '  // &
                       'this string contains a non-printing ' // &
                       'character (ICHAR = #) at position #.'  )
         call ctx%errch  ( '#', class                              )
         call ctx%errint ( '#', chrcod                             )
         call ctx%errint ( '#', i                                  )
         call ctx%sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
         return

      end if

!
!     MAXLEN must be a sensible value.
!
      if ( maxlen < 1 ) then

         call ctx%setmsg ( 'Non-blank length limit MAXLEN should be '  // &
                       'positive but was #.'                        )
         call ctx%errint ( '#',   maxlen                                )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

      l  =  len_trim(id)

!
!     The ID must not be too long.
!
      if ( l > maxlen ) then

         call ctx%setmsg ( 'The # ''#'' is invalid; the last non-blank '// &
                       'character is located at position #; the '   // &
                       'maximum allowed length is #.'               )

         if ( class /= ' ' ) then
            call ctx%errch ( '#', class )
         else
            call ctx%errch ( '#', 'ID' )
         end if

         call ctx%errch  ( '#', id                  )
         call ctx%errint ( '#', l                   )
         call ctx%errint ( '#', maxlen              )
         call ctx%sigerr ( 'SPICE(IDSTRINGTOOLONG)' )
         return

      end if

!
!     Look for non-printing characters in ID.
!
      i  = frstnp ( id )

      if ( i > 0 ) then

         chrcod = ichar( id(i:i) )

         call ctx%setmsg ( 'The # ''#'' is invalid; this string ' // &
                       'contains a non-printing character '   // &
                       '(ICHAR = #) at position #.'            )

         if ( class /= ' ' ) then
            call ctx%errch ( '#', class )
         else
            call ctx%errch ( '#', 'ID' )
         end if

         call ctx%errch  ( '#', id                    )
         call ctx%errint ( '#', chrcod                )
         call ctx%errint ( '#', i                     )
         call ctx%sigerr ( 'SPICE(NONPRINTABLECHARS)' )
         return

      end if

      return
      end subroutine chckid

      !> Count the characters in a group of lines in a text file.
      !>
      !> The function returns the number of characters in a group of lines
      !> in a text file.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is a logical unit that has been connected to a
      !>              text file by the calling program. Use the routine
      !>              TXTOPR to open the file for read access and get its
      !>              logical unit. A text file is a formatted,
      !>              sequential file that contains only printable
      !>              characters:  ASCII 32-126.
      !>
      !>     BLINE,
      !>     ELINE    are line numbers in the text file. BLINE is
      !>              the line where the count will begin, and ELINE
      !>              is the line where the count will end. The
      !>              number of characters in the beginning and ending
      !>              lines are included in the total count.
      !>
      !>              By convention, line 1 is the first line of the file.
      !>
      !>     LINE     on input, is an arbitrary character string whose
      !>              contents are ignored. LINE is used to read lines
      !>              from the file connected to UNIT; its function
      !>              is to determine the maximum length of the lines
      !>              that can be read from the file. Lines longer
      !>              than the declared length of LINE are truncated
      !>              as they are read.
      !>
      !>## Detailed Output
      !>
      !>     LINE     on output, is undefined.
      !>
      !>     The function returns the number of characters in the group of
      !>     lines in the text file beginning with BLINE and ending with ELINE.
      !>     Trailing blanks on a line are not included in the count.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while reading from the input file,
      !>         the error SPICE(FILEREADFAILED) is signaled.
      !>
      !>     2)  If a non-printing ASCII character is encountered during
      !>         the count, the error SPICE(INVALIDTEXT) is signaled.
      !>
      !>     3)  If BLINE is greater than ELINE or if the file does not
      !>         contain both of this lines, the error SPICE(CANNOTFINDGRP)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See argument UNIT. COUNTC rewinds the text file connected to
      !>     UNIT and then steps through the file. The next read statement
      !>     after calling COUNTC would return the line after ELINE.
      !>
      !>## Particulars
      !>
      !>     This routine counts characters in a group of lines in a text
      !>     file. Using COUNTC, you can determine in advance how much space
      !>     is required to store those characters.
      !>
      !>## Examples
      !>
      !>     The following code fragment opens an existing text file for
      !>     read access and counts the characters that it contains in
      !>     the first five lines. We'll assume that the longest line
      !>     in the file is 80 characters.
      !>
      !>        INTEGER               COUNTC
      !>        INTEGER               UNIT
      !>        INTEGER               N
      !>        CHARACTER*(80)        LINE
      !>
      !>        CALL TXTOPR ( 'DATA.TXT', UNIT )
      !>
      !>        N = COUNTC ( UNIT, 1, 5, LINE )
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
      !>     J.E. McLean        (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 18-MAR-2021 (JDR)
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
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
      module integer(ip) function countc ( ctx, unit, bline, eline, line )
      use pepper_kinds, only: ip
      use pepper_strings, only: astrip_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Logical unit connected to text file.
      integer(ip), intent(in) :: bline  !! Beginning line number.
      integer(ip), intent(in) :: eline  !! Ending line number.
      character(*), intent(inout) :: line  !! Workspace.

      integer(ip) :: chars
      integer(ip) :: iostat
      integer(ip) :: linect

      logical :: done

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         countc = 0
         return
      else
         countc = 0
      end if

!
!     First, see if the line numbers make sense.
!
      if ( ( bline > eline ) .or. ( bline <= 0 ) ) then

         call ctx%setmsg ( 'The line numbers do not make sense:  '        // &
                       'BLINE = # and  ELINE = #.'                    )
         call ctx%errint ( '#', bline                                     )
         call ctx%errint ( '#', eline                                     )
         call ctx%sigerr ( 'SPICE(CANNOTFINDGRP)'                         )
         return

      end if

!
!     Read through the file, line by line, beginning with the first
!     line in the file, checking for I/O errors, and counting
!     characters in the lines between and including BLINE and ELINE.
!
      rewind ( unit )

      linect = 0
      chars  = 0
      done   = .false.

      do while ( .not. done )

         read ( unit, fmt='(A)', iostat=iostat ) line
!
!        An end-of-file condition is indicated by a negative value
!        for IOSTAT. Any other non-zero value indicates some other
!        error.  If IOSTAT is zero, the read was successful.
!
         if ( iostat > 0 ) then

            call ctx%setmsg ( 'Error reading text file named FILENAME.'   // &
                          'The value of IOSTAT is #.'                 )
            call ctx%errint ( '#', iostat                                 )
            call ctx%errfnm ( 'FILENAME', unit                            )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                     )
            return

         else if ( iostat < 0 ) then

            call ctx%setmsg ( 'Reached end of file unexpectedly at line ' // &
                          '# in file FILE.  BLINE = # and ELINE = #.' )
            call ctx%errint ( '#', linect                                 )
            call ctx%errint ( '#', bline                                  )
            call ctx%errint ( '#', eline                                  )
            call ctx%errfnm ( 'FILE', unit                                )
            call ctx%sigerr ( 'SPICE(CANNOTFINDGRP)'                      )
            return

         else
!
!           We've read a line successfully, so add it to the line count.
!           If this line is in the group delimited by BLINE and ELINE,
!           count the characters in it, and if this line is ELINE, we're
!           done.
!
            linect = linect + 1

            if (( linect >= bline ) .and. ( linect <= eline )) then

!
!              Add the number of characters in this line to the count.
!              If LINE is blank, LASTNB will return 0 which is just
!              what we want.
!
               chars = chars + len_trim(line)

!
!              Remove the printable characters from the line.  If
!              any characters remain, signal an error.
!
               call astrip_inplace ( line, char( 32), char(126) )

               if ( line /= ' ' ) then

                  call ctx%setmsg ( 'Non-printing ASCII characters were ' // &
                                'found when counting characters on '  // &
                                'line number # in file FILENAME.'     )
                  call ctx%errint ( '#', linect                           )
                  call ctx%errfnm ( 'FILENAME', unit                      )
                  call ctx%sigerr ( 'SPICE(INVALIDTEXT)'                  )
                  return

               end if

            end if

            if ( linect == eline ) done = .true.

         end if

      end do

!
!     Assign the final character count.
!
      countc = chars

      return
      end function countc

      !> Convert a signed hexadecimal string representation of an integer
      !> to its equivalent integer.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is the hexadecimal string to be converted to an integer.
      !>
      !>              The following table describes the character set used
      !>              to represent the hexadecimal digits and their
      !>              corresponding values.
      !>
      !>              Character    Value           Character    Value
      !>              ---------    -----           ---------    -----
      !>                '0'          0                '8'          8
      !>                '1'          1                '9'          9
      !>                '2'          2              'A','a'       10
      !>                '3'          3              'B','b'       11
      !>                '4'          4              'C','c'       12
      !>                '5'          5              'D','d'       13
      !>                '6'          6              'E','e'       14
      !>                '7'          7              'F','f'       15
      !>
      !>             The plus sign, '+', and the minus sign, '-', are used as
      !>             well, and they have their usual meanings.
      !>
      !>             A hexadecimal character string parsed by this routine
      !>             should consist of a sign, '+' or '-' (the plus sign is
      !>             optional for nonnegative numbers), followed immediately
      !>             by a contiguous sequence of hexadecimal digits, e.g.:
      !>
      !>                (1)   +h h ... h
      !>                        1 2     n
      !>
      !>                (2)   -h h ... h
      !>                        1 2     n
      !>
      !>                (3)   h h ... h
      !>                       1 2     n
      !>
      !>             where h  represents an hexadecimal digit.
      !>                    i
      !>
      !>             STRING may have leading and trailing blanks, but blanks
      !>             embedded within the significant portion of the character
      !>             string are not allowed. This includes any blanks which
      !>             appear between the sign character and the first
      !>             hexadecimal digit.
      !>
      !>## Detailed Output
      !>
      !>     NUMBER   is the integer value to be returned. The value of this
      !>              variable is not changed if an error occurs while parsing
      !>              the hexadecimal character string.
      !>
      !>     ERROR    is a logical flag which indicates whether an error
      !>              occurred while attempting to parse NUMBER from the
      !>              hexadecimal character string STRING. ERROR will have the
      !>              value .TRUE. if an error occurs. It will have the value
      !>              .FALSE. otherwise.
      !>
      !>     ERRMSG   contains a descriptive error message if an error
      !>              occurs while attempting to parse NUMBER from the
      !>              hexadecimal character string STRING, blank otherwise.
      !>              The error message will be left justified.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If an unexpected character is encountered while parsing the
      !>         hexadecimal character string, an appropriate error message
      !>         will be set, and the routine will exit. The value of NUMBER
      !>         will be unchanged.
      !>
      !>     2)  If the string represents a number that is larger than
      !>         the maximum representable integer an appropriate error
      !>         message will be set, and the routine will exit. The value
      !>         of NUMBER will be unchanged.
      !>
      !>     3)  If the string represents a number that is smaller than
      !>         the minimum representable integer, an appropriate error
      !>         message will be set, and the routine will exit. The value
      !>         of NUMBER will be unchanged.
      !>
      !>     4)  If the input string is blank, an appropriate error message
      !>         will be set, and the routine will exit. The value of NUMBER
      !>         will be unchanged.
      !>
      !>     5)  If the error message string is not long enough to contain
      !>         the entire error message, the error message will be
      !>         truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine will convert a signed hexadecimal character string
      !>     representation of an integer into its equivalent integer. This
      !>     provides a machine independent mechanism for storing or porting
      !>     integer values. This routine is used by the routine HX2DP which
      !>     converts a character string representation of a double precision
      !>     into its equivalent double precision value.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     perform conversions between integers and equivalent signed
      !>     hexadecimal character strings:
      !>
      !>           INT2HX -- Convert an integer into a signed hexadecimal
      !>                     character string.
      !>
      !>           HX2INT -- Convert a signed hexadecimal character string
      !>                     into an integer.
      !>
      !>## Examples
      !>
      !>     All of the values shown are for a two's complement 32 bit
      !>     representation for signed integers.
      !>
      !>     The following argument values illustrate the action of HX2INT for
      !>     various input values.
      !>
      !>         STRING                 NUMBER        ERROR   ERRMSG
      !>         ---------------------  ------------  ------  ------
      !>          '1'                    1            .FALSE.   ' '
      !>          '-1'                  -1            .FALSE.   ' '
      !>          'DF'                   223          .FALSE.   ' '
      !>          'Df'                   223          .FALSE.   ' '
      !>          '+3ABC'                15036        .FALSE.   ' '
      !>          'ff'                   255          .FALSE.   ' '
      !>          '-20'                 -32           .FALSE.   ' '
      !>          '0'                    0            .FALSE.   ' '
      !>
      !>          '7FFFFFFF'             2147483647   .FALSE.   ' '
      !>          (Maximum 32 bit integer)
      !>
      !>          '-7FFFFFFF'           -2147483647   .FALSE.   ' '
      !>          (Minimum 32 bit integer + 1)
      !>
      !>          '-80000000'           -2147483648   .FALSE.   ' '
      !>          (Minimum 32 bit integer)
      !>
      !>          STRING = ' '
      !>          NUMBER = ( Not defined )
      !>          ERROR  = .TRUE.
      !>          ERRMSG = 'ERROR: A blank input string is not allowed.'
      !>
      !>          STRING = '-AB238Q'
      !>          NUMBER = ( Not defined )
      !>          ERROR  = .TRUE.
      !>          ERRMSG = 'ERROR: Illegal character ''Q'' encountered.'
      !>
      !>          STRING = '- AAA'
      !>          NUMBER = ( Not defined )
      !>          ERROR  = .TRUE.
      !>          ERRMSG = 'ERROR: Illegal character '' '' encountered.'
      !>
      !>          STRING = '80000000'
      !>          NUMBER = ( Not defined )
      !>          ERROR  = .TRUE.
      !>          ERRMSG = 'ERROR: Integer too large to be represented.'
      !>
      !>          STRING = '-800F0000'
      !>          NUMBER = ( Not defined )
      !>          ERROR  = .TRUE.
      !>          ERRMSG = 'ERROR: Integer too small to be represented.'
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 20-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 10-MAR-1994 (KRG)
      !>
      !>        Changed an IF test operand from .LE. to .LT. so that
      !>        the ELSE IF clause could be reached. This change has
      !>        NO effect on the execution of the routine because it
      !>        makes use of a base that is a power of 2 (16), so the
      !>        ELSE IF clause never needs to be reached. The algorithm
      !>        was meant to be as general as possible, however, so that
      !>        only the base and digits would need to be changed in order to
      !>        implement a different number base.
      !>
      !>-    SPICELIB Version 1.0.0, 22-OCT-1992 (KRG)
      !>
      !>## Revisions
      !>
      !>-     SPICELIB Version 1.1.0, 10-MAR-1994 (KRG)
      !>
      !>         Changed an IF test operand from .LE. to .LT. so that
      !>         the ELSE IF clause could be reached. This change has
      !>         NO effect on the execution of the routine because it
      !>         makes use of a base that is a power of 2 (16), so the
      !>         ELSE IF clause never needs to be reached. The algorithm
      !>         was meant to be as general as possible, however, so that
      !>         only the base and digits would need to be changed in order to
      !>         implement a different number base.
      !>
      !>         Old code was:
      !>
      !>            IF ( TMPNUM .LE. MAXI ) THEN
      !>
      !>               TMPNUM = TMPNUM * BASE + IDIGIT
      !>               POS    = POS + 1
      !>
      !>            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
      !>     .                ( IDIGIT .LE. MAXMOD ) ) THEN
      !>
      !>               TMPNUM = TMPNUM * BASE + IDIGIT
      !>               POS    = POS + 1
      !>
      !>            ELSE ...
      !>
      !>         New code:
      !>
      !>            IF ( TMPNUM .LT. MAXI ) THEN
      !>
      !>               TMPNUM = TMPNUM * BASE + IDIGIT
      !>               POS    = POS + 1
      !>
      !>            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
      !>     .                ( IDIGIT .LE. MAXMOD ) ) THEN
      !>
      !>               TMPNUM = TMPNUM * BASE + IDIGIT
      !>               POS    = POS + 1
      !>
      !>            ELSE ...
      module subroutine hx2int ( ctx, string, number, error, errmsg )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_strings, only: repmc_inplace
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! Hexadecimal string to be converted to an integer.
      integer(ip), intent(out) :: number  !! Integer value to be returned.
      logical, intent(out) :: error  !! A logical flag which is .TRUE. on error.
      character(*), intent(out) :: errmsg  !! A descriptive error message.
!
!     Local Parameters
!
      integer(ip), parameter :: base = 16
!
!     Character codes and limits (SPICELIB computed them on the first
!     call).
!
      integer(ip), parameter :: digbeg = ichar( '0' )
      integer(ip), parameter :: digend = ichar( '9' )
      integer(ip), parameter :: uccbeg = ichar( 'A' )
      integer(ip), parameter :: uccend = ichar( 'F' )
      integer(ip), parameter :: lccbeg = ichar( 'a' )
      integer(ip), parameter :: lccend = ichar( 'f' )
      integer(ip), parameter :: iminus = ichar( '-' )
      integer(ip), parameter :: iplus  = ichar( '+' )
      integer(ip), parameter :: ispace = ichar( ' ' )
      integer(ip), parameter :: mini   = intmin / base
      integer(ip), parameter :: minmod = base * mini - intmin
      integer(ip), parameter :: maxi   = intmax / base
      integer(ip), parameter :: maxmod = intmax - base * maxi
      integer(ip) :: idigit
      integer(ip) :: letter
      integer(ip) :: pos
      integer(ip) :: strbeg
      integer(ip) :: strend
      integer(ip) :: tmpnum

      logical :: more
      logical :: negtiv

!
!     The input hexadecimal string is scanned from left to right, and
!     the integer is generated by repeated multiplications and additions
!     or subtractions.
!
!     If this is the first time that this routine has been called,
!     we need to do some setup stuff.
!
!     There are no errors initially, so set the error flag to
!     .FALSE.
!
      error = .false.
!
!     If the string is blank, set the error flag and return immediately.
!
      if ( string == ' ' ) then

         error  = .true.
         errmsg = 'ERROR: A blank input string is not allowed.'
         return

      end if
!
!     Initialize a few other things.
!
      errmsg = ' '
      tmpnum = 0
!
!     Assume that the number is nonnegative.
!
      negtiv = .false.
!
!     Skip any leading white space. We know that there is at least
!     one nonblank character at this point, so we will not loop
!     off the end of the string.
!
      strbeg = 1

      do while ( ichar( string(strbeg:strbeg) ) == ispace )

         strbeg = strbeg + 1

      end do
!
!     Now, we want to find the end of the significant portion of
!     the input string.
!
      strend = strbeg + 1
      more   = .true.

      do while ( more )

         if ( strend <= len( string ) ) then

            if ( string(strend:) /= ' ' ) then

               strend = strend + 1

            else

               more = .false.

            end if

         else

            more = .false.

         end if

      end do
!
!     At this point, STREND is one larger than the length of the
!     significant portion of the string because we incremented
!     its value after the test. We will subtract one from the
!     value of STREND so that it exactly represents the position
!     of the last significant character in the string.
!
      strend = strend - 1
!
!     Set the position pointer to the beginning of the significant
!     part, i.e., the nonblank part, of the string, because we are
!     now ready to try and parse the number.
!
      pos = strbeg
!
!     The first character should be a plus sign, '+', a minus sign,
!     '-', or a digit, '0' - '9', 'A' - 'F', or 'a' - 'f'. Anything
!     else is bogus, and we will catch it in the main loop below.
!
!     If the character is a minus sign, we want to set the value of
!     NEGTIV to .TRUE. and increment the position.
!
!     If the character is a plus sign, we want to increment the
!     position.
!
      if ( ichar( string(pos:pos) ) == iminus ) then

         negtiv = .true.
         pos    = pos + 1

      else if ( ichar( string(pos:pos) ) == iplus ) then

         pos = pos + 1

      end if
!
!     When we build up the number from the hexadecimal string we
!     need to treat nonnegative numbers differently from negative
!     numbers. This is because on many computers the minimum
!     integer is one less than the negation of the maximum integer.
!     Negative numbers are the ones which truly might cause
!     problems, because ABS(minimum integer) may equal ABS(maximum
!     integer) + 1, on some machines. For example, on many machines
!     with 32 bit numbers, INTMIN = -2147483648 and INTMAX =
!     2147483647.
!
!     Build up the number from the hexadecimal character string.
!
      if ( negtiv ) then

         do while ( pos <= strend )

            letter = ichar( string(pos:pos) )

            if ( ( letter >= digbeg ) .and. &
                 ( letter <= digend ) ) then

               idigit = letter - digbeg

            else if ( ( letter >= uccbeg ) .and. &
                      ( letter <= uccend ) ) then

               idigit = 10 + letter - uccbeg

            else if ( ( letter >= lccbeg ) .and. &
                      ( letter <= lccend ) ) then

               idigit = 10 + letter - lccbeg

            else

               error  = .true.
               errmsg = 'ERROR: Illegal character ''#'''  // &
                        ' encountered.'
               call repmc_inplace ( errmsg, '#', char( letter ) )
               return

            end if

            if ( tmpnum > mini ) then

               tmpnum = tmpnum * base - idigit
               pos    = pos + 1

            else if ( ( tmpnum == mini   ) .and. &
                      ( idigit <= minmod ) ) then

               tmpnum = tmpnum * base - idigit
               pos    = pos + 1

            else

               error  = .true.
               errmsg = 'ERROR: Integer too small'  // &
                        ' to be represented.'
               return

            end if

         end do

      else

         do while ( pos <= strend )

            letter = ichar( string(pos:pos) )

            if ( ( letter >= digbeg ) .and. &
                 ( letter <= digend ) ) then

               idigit = letter - digbeg

            else if ( ( letter >= uccbeg ) .and. &
                      ( letter <= uccend ) ) then

               idigit = 10 + letter - uccbeg

            else if ( ( letter >= lccbeg) .and. &
                      ( letter <= lccend) ) then

               idigit = 10 + letter - lccbeg

            else

               error  = .true.
               errmsg = 'ERROR: Illegal character ''#'''  // &
                        ' encountered.'
               call repmc_inplace ( errmsg, '#', char( letter ) )
               return

            end if

            if ( tmpnum < maxi ) then

               tmpnum = tmpnum * base + idigit
               pos    = pos + 1

            else if ( ( tmpnum == maxi   ) .and. &
                      ( idigit <= maxmod ) ) then

               tmpnum = tmpnum * base + idigit
               pos    = pos + 1

            else

               error  = .true.
               errmsg = 'ERROR: Integer too large'  // &
                        ' to be represented.'
               return

            end if

         end do

      end if
!
!     If we got to here, we have successfully parsed the hexadecimal
!     string into an integer. Set the value and return.
!
      number = tmpnum

      return
      end subroutine hx2int

      !> Find a given identifier, which consists of an integer array,
      !> within a list of such identifiers, or insert the identifier
      !> into the list. Return the location of the identifier and a flag
      !> indicating whether or not the identifier was already present.
      !>
      !>## Detailed Input
      !>
      !>     ID       is an integer array that serves as an identifier
      !>              for some object. For example it might be a SPICE
      !>              id code for a planet or satellite; it might be the
      !>              instrument id and mode of operation of an instrument.
      !>              See the $Examples section for more details.
      !>
      !>     IDSZ     is the number of components in the array ID.
      !>
      !>     LIST     is an array containing several ID's. The array
      !>              should be declared so as to have the same upper
      !>              bound at least as large as the upper bound used
      !>              in the declaration of POOL.
      !>
      !>     POOL     is a linked list pool that gives the search order
      !>              for examining LIST to locate ID's. The declaration
      !>              of POOL and LIST need to be compatible. Normally,
      !>              the declaration should look like this:
      !>
      !>                 INTEGER   LIST (IDSZ,         LSTSIZ )
      !>                 INTEGER   POOL (   2, LBPOOL: LSTSIZ )
      !>
      !>              If POOL is declared with the statement
      !>
      !>                 INTEGER   POOL (   2, LBPOOL: PSIZE  )
      !>
      !>              then you must make sure that PSIZE is less than
      !>              or equal to LSTSIZ.
      !>
      !>              POOL should be initialized before the first
      !>              call to this routine with the SPICE routine
      !>              LNKINI.
      !>
      !>     AT       is a value that is set by this routine and that
      !>              you should never reset yourself. It points
      !>              to the head of the linked list used for
      !>              searching LIST. Changing AT will destroy the
      !>              link between POOL and LIST.
      !>
      !>              There is one exception to these restrictions.
      !>              The first call to this routine that occurs after
      !>              initializing POOL, AT may have any value. It will
      !>              be set upon output and from that time on, you should
      !>              not alter its value except by calling this routine
      !>              to do so.
      !>
      !>## Detailed Output
      !>
      !>     AT       on output AT points to the location in LIST
      !>              of ID.
      !>
      !>     PRESNT   is a logical flag. It indicates whether or not
      !>              ID was already present in the LIST when this
      !>              routine was called. If ID was already in LIST
      !>              PRESNT is returned with the value .TRUE. Otherwise
      !>              it is returned with the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of AT is less than zero or greater than
      !>         the declared size of POOL (except immediately after
      !>         initializing or re-initializing POOL), the
      !>         error SPICE(ADDRESSOUTOFBOUNDS) is signaled.
      !>
      !>     2)  If the linked list pool POOL is corrupted by a higher
      !>         level routine, a diagnosis of the problem will be
      !>         made by a routine called by this one.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine serves as a utility for managing the bookkeeping
      !>     needed when using a local buffering scheme which removes
      !>     the last used item when the local buffer becomes full.
      !>
      !>     It is primarily a programming utility. Unless you are dealing
      !>     with a problem very similar to the one just described, you
      !>     probably shouldn't be using this routine.
      !>
      !>     The example below illustrates the intended use of this
      !>     routine.
      !>
      !>## Examples
      !>
      !>     Consider the following programming situation.
      !>
      !>     Suppose that a routine is being written that will
      !>     access large amounts of data stored in the SPICE
      !>     kernel pool. Kernel pool access requires overhead that
      !>     may be prohibitive under some circumstances. Buffering
      !>     data locally and only fetching data from the kernel pool
      !>     when it has not been buffered locally, may substantially
      !>     improve the performance of the routine being written.
      !>
      !>     However, since FORTRAN does not allow dynamic memory allocation
      !>     the local data storage must be set at compile time. As
      !>     a result the local data buffer might become full during
      !>     an execution of your program. If data for an item needs
      !>     to be fetched from the kernel pool once the buffer has become
      !>     full, you must either repeatedly call the kernel pool to fetch
      !>     the new data or overwrite some of the data in your local buffer.
      !>
      !>     This routine helps with the decisions of which items to
      !>     overwrite. In addition it always moves the last requested
      !>     item to the head of the index used for searching the buffered
      !>     ID's. In this way if the same item is needed many times
      !>     in succession, there will be very little overhead associated
      !>     with finding the item. Thus the routine spends its time
      !>     in computing the desired quantities, not in looking up the
      !>     parameters needed for the computation.
      !>
      !>     Below is a fragment of code that illustrates how this routine
      !>     should be used. In the situation outlined above. We'll suppose
      !>     that we are fetching MDLSIZ double precision numbers from the
      !>     kernel pool that are associated with the item
      !>
      !>         'BODYid_MAGMODEL'
      !>
      !>     And that we are computing something with this model data.
      !>
      !>        INTEGER               MDLSIZ
      !>        PARAMETER           ( MDLSIZ = xxxxxx )
      !>
      !>        We'll create room to buffer this data for 8 bodies.
      !>
      !>        INTEGER               PSIZE
      !>        PARAMETER           ( PSIZE = 8 )
      !>
      !>        The ID's we shall be using are 1-dimensional. They are body
      !>        ID's for planets or and their satellites.
      !>
      !>        INTEGER               IDSZ
      !>        PARAMETER           ( IDSZ = 1 )
      !>
      !>        INTEGER               AT
      !>        INTEGER               DIM
      !>        INTEGER               LIST   (   IDSZ,  PSIZE        )
      !>        INTEGER               POOL   (      2,  LBPOOL:PSIZE )
      !>
      !>        DOUBLE PRECISION      MAGMDL ( MDLSIZ,  PSIZE        )
      !>        DOUBLE PRECISION      MODEL  ( MDLSIZ                )
      !>
      !>        LOGICAL               FIRST
      !>        LOGICAL               PRESNT
      !>
      !>        SAVE
      !>
      !>        DATA                  FIRST / .TRUE. /
      !>
      !>        The block below handles initializing the linked list pool.
      !>
      !>        IF ( FIRST ) THEN
      !>
      !>           FIRST = .FALSE.
      !>
      !>           CALL LNKINI ( PSIZE, POOL )
      !>
      !>        END IF
      !>
      !>        See if the data associated with ID has already been
      !>        buffered.
      !>
      !>        CALL LOCATI ( ID, IDSZ, LIST, POOL, AT, PRESNT )
      !>
      !>        IF ( .NOT. PRESNT ) THEN
      !>
      !>           The data has not yet been buffered, look it up. Normally
      !>           you might want to check to see if the data exists and
      !>           handle things appropriately if it doesn't but this is just
      !>           to give you the idea...
      !>
      !>           CALL BODVCD ( ID, 'MAGMODEL', 3, DIM, MAGMDL ( 1, AT ) )
      !>
      !>        END IF
      !>
      !>        Put the model data into the array MODEL for ease of
      !>        reading the rest of the code.
      !>
      !>        CALL MOVED ( MAGMDL(1,AT), MDLSIZ, MODEL )
      !>
      !>        Now do whatever processing is needed ....
      !>
      !>     There are a few things to note about the code fragment above.
      !>     First the handling of the buffering of data was very easy.
      !>     Second, if this routine is called again using the same ID,
      !>     the buffer will already contain the needed model. Moreover
      !>     the routine LOCATI will return very quickly because the
      !>     ID will already be at the head of the list indexed by POOL.
      !>
      !>     You can also easily add an entry point to this routine that
      !>     will force it to look up data from the kernel pool on the
      !>     next call. All that needs to be done is re-initialize the
      !>     linked list pool.
      !>
      !>        ENTRY DOLOOK
      !>
      !>        CALL LNKINI ( PSIZE, POOL )
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
      !>-    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        PRESNT short description in $Brief_I/O section.
      !>
      !>-    SPICELIB Version 1.0.1, 24-OCT-2005 (NJB)
      !>
      !>        Header update: changed reference to BODVAR to reference
      !>        to BODVCD.
      !>
      !>-    SPICELIB Version 1.0.0, 09-APR-1997 (WLT)
      module subroutine locati ( ctx, id, idsz, list, pool, at, presnt )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: id   (            * )  !! An array of integers that comprise an
                                                        !! identifier.
      integer(ip), intent(in) :: idsz  !! The number of integer components per identifier.
      integer(ip), intent(inout) :: list ( idsz,      * )  !! A list of known identifiers.
      integer(ip), intent(inout) :: pool ( 2, lbpool: * )  !! A doubly linked list used for
                                                           !! search LIST.
      integer(ip), intent(inout) :: at  !! Location of the ID in the LIST.
      logical, intent(out) :: presnt  !! Flag indicating if ID was already in LIST.


!
!     Linked list parameters
!
      integer(ip), parameter :: next = 1  !! row of a node's forward pointer in the linked list

      integer(ip), parameter :: prev = 2

!
!     Local Variables.
!

      integer(ip) :: last
      integer(ip) :: nfree
      integer(ip) :: psize
      integer(ip) :: i
      integer(ip) :: head
      integer(ip) :: new

      logical :: more
      logical :: same

!
!     We begin by looking through the list of items at our disposal.
!     One way or the other we will need the number of free nodes
!     in the linked list.
!
      nfree = lnknfn( ctx,pool)
      psize = lnksiz( ctx,pool)

      if ( nfree == psize ) then
!
!        There's nothing in the list so far. Allocate a
!        node and begin a list.
!
         call lnkan ( ctx, pool, at )

         do i = 1, idsz
            list(i,at) = id(i)
         end do

         presnt = .false.

         return

      end if

      if ( at <= 0 .or. at > psize ) then

         call ctx%setmsg ( 'The input value for the head of the ID ' &
         //            'address linked list is out of bounds. ' &
         //            'It should be between 0 and #. The value ' &
         //            'supplied was #.' )
         call ctx%errint( '#', psize )
         call ctx%errint( '#', at    )
         call ctx%sigerr ( 'SPICE(ADDRESSOUTOFBOUNDS)'  )
         return

      end if

!
!     If we are still here then there is actually something in
!     the list.  We begin at start and traverse the list.
!     Since we are unlikely to ever have large ID's (their purpose
!     after all is to be a label for something more complex) we
!     will handle the cases where IDSZ is 1 or 2 as special
!     cases since the tests for equality are a lot easier.
!
      same = .false.

      head =  at

      if ( idsz == 1 ) then

         same = id(1) == list(1,at)
         more = at > 0 .and. .not. same

         do while ( more )

            at   =  pool ( next, at )

            if ( at > 0 ) then
               same =  id(1) == list(1,at)
               more = .not. same
            else
               more = .false.
            end if

         end do

      else if ( idsz == 2 ) then

         same =       id(1) == list(1,at) &
                .and. id(2) == list(2,at)

         more = at > 0 .and. .not. same

         do while ( more )

            at  = pool ( next, at )

            if ( at > 0 ) then
               if (      id(1) == list(1,at) ) then
                  same = id(2) == list(2,at)
                  more = .not. same
               end if
            else
               more = .false.
            end if

         end do

      else

         i      =  1
         same   = .true.

         do while ( same .and. i < idsz )
            same = same .and. id(i) == list(i,at)
            i    = i + 1
         end do

         more = at > 0 .and. .not. same

         do while ( more )

            at = pool ( next, at )

            if ( at > 0 ) then
               i      =  1
               same   = .true.

               do while ( same .and. i < idsz )
                  same = same .and. id(i) == list(i,at)
                  i    = i + 1
               end do

               more = .not. same
            else
               more = .false.
            end if

         end do

      end if

!
!     The hunting is over either we found it or we need to
!     allocate space to put this ID into storage.
!
      if ( same ) then

         presnt = .true.
         last   =  pool(prev,at)

!
!        If AT is not already at the head of the list, we
!        move this node to the front of the list.
!
         if ( last > 0 ) then
            call lnkxsl ( ctx, at, at,   pool )
            call lnkilb ( ctx, at, head, pool )
         end if

         return

      end if

!
!     If we got to this point, we traversed the entire linked
!     list and did not find a matching ID.  AT is negative
!     and -AT points to the head of the list.
!
      presnt =  .false.
!
!     We'll put it in the list. First see if there are any free nodes.
!
      if ( nfree > 0 ) then
!
!        Allocate a free node and put our ID at the NEW address.
!
         call lnkan ( ctx, pool, new )
         do i = 1, idsz
            list(i,new) = id(i)
         end do
!
!        Put the new node at the head of the linked list.
!
         call lnkilb ( ctx, new, head, pool )
         at =  new

      else

!
!        The last item in the list is pointed to as being the
!        previous item to the head of the list. But we have to
!        change the sign to get a legitimate address.  Overwrite
!        the ID information in this last slot of the list.
!
         last = -pool(prev,head)

         do i = 1, idsz
            list(i,last) = id(i)
         end do
!
!        Extract the last item as a sublist and insert it before
!        the current head of the list.
!
         call lnkxsl ( ctx, last,   last,   pool )
         call lnkilb ( ctx, last,   head,   pool )

         at    = last

      end if

      return

      end subroutine locati

      !> Locate a group of lines in a text file delimited by markers.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is a logical unit that has been connected to a
      !>              text file by the calling program. Use the routine
      !>              TXTOPR to open the file for read access and get its
      !>              logical unit. The file pointer may be pointing to
      !>              any line in the file due to previous read statements,
      !>              for example, or due to previous calls to LOCLN.
      !>
      !>     BMARK,
      !>     EMARK    are markers that delimit some group of lines in
      !>              the part of the file following the current position
      !>              of the file pointer. The group begins with the
      !>              first line equivalent to BMARK and ends with the
      !>              next line equivalent to EMARK, ignoring leading
      !>              and trailing blanks.
      !>
      !>              If BMARK is blank, the group of lines begins with
      !>              the first line following the current position of the
      !>              file pointer; if EMARK is blank, the group of lines
      !>              ends with the last line in the file.
      !>
      !>      LINE       on input, is an arbitrary character string whose
      !>              contents are ignored. LINE is used to read lines
      !>              from the file connected to UNIT; its function
      !>              is to determine the maximum length of the lines
      !>              that can be read from the file. Lines longer
      !>              than the declared length of LINE are truncated
      !>              as they are read.
      !>
      !>## Detailed Output
      !>
      !>     LINE     on output, is undefined.
      !>
      !>     BLINE,
      !>     ELINE    are the line numbers of the first and last lines
      !>              in the group delimited by BMARK and EMARK.
      !>
      !>              By convention, the first line read by the routine
      !>              is line 1; the second line is line 2; and so on.
      !>              If BMARK is blank, BLINE will be 1.
      !>
      !>     FOUND    is .TRUE. if a group of lines delimited by BMARK and
      !>              EMARK is found, and is .FALSE. otherwise. ELINE is
      !>              the last line read by LOCLN, so if FOUND is .TRUE.,
      !>              the file pointer will be positioned on the line
      !>              after ELINE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If FOUND is .FALSE., the values of BLINE and ELINE are not
      !>         changed.
      !>
      !>     2)  If an error occurs while reading from the input file,
      !>         the error SPICE(FILEREADFAILED) is signaled.
      !>
      !>     3)  Lines in the file that are longer than the declared length of
      !>         LINE are truncated as they are read. If the truncation of
      !>         line containing a marker causes truncation of that marker,
      !>         it will not match the input value for that marker, so
      !>         FOUND will be .FALSE.
      !>
      !>## Files
      !>
      !>     See argument UNIT.
      !>
      !>## Particulars
      !>
      !>     This routine locates delimited groups of lines in a text file.
      !>     This allows files to be partitioned into sub-files; it also
      !>     allows related inputs to be grouped together in a relatively
      !>     free-format way.
      !>
      !>## Examples
      !>
      !>     1) Let FILE.TXT be a text file that contains the following lines.
      !>        (The lines are numbered for reference, but these numbers do
      !>        not appear in the file).
      !>
      !>           1    BEGIN POEM
      !>           2       Oh snail,
      !>           3       Climb Mount Fuji,
      !>           4       But slowly, slowly!
      !>           5    END POEM
      !>           6
      !>           7    BEGIN PROSE
      !>           8       Lady, one of us has this book open
      !>           9       to the wrong page.
      !>           10   END PROSE
      !>           11
      !>           12   BEGIN POEM
      !>           13      John Keats, John Keats,
      !>           14      John,
      !>           15      Put your scarf on.
      !>           16   END POEM
      !>           17
      !>           18   BEGIN QUOTE
      !>           19      That's not writing. That's typing.
      !>           20
      !>           21               (Truman Capote on Jack Kerouac)
      !>           22   END QUOTE
      !>           23
      !>           24   BEGIN POEM
      !>           25      Twice five syllables
      !>           26      Plus seven isn't much, but
      !>           27      That's haiku for you.
      !>           28   BEGIN POEM
      !>           29
      !>           30   BEGIN EQUATION
      !>           31            2
      !>           32      e = mc
      !>           33   END EQUATION
      !>
      !>     Then the code fragment
      !>
      !>           CALL TXTOPR ( 'FILE.TXT', UNIT )
      !>
      !>           BMARK = 'BEGIN POEM'
      !>           EMARK = 'END POEM'
      !>
      !>           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
      !>
      !>           DO WHILE ( FOUND )
      !>              WRITE (*,*) 'Found poem between lines ', B, ' and ', E
      !>
      !>              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
      !>           END DO
      !>
      !>     produces the following report:
      !>
      !>           Found poem between lines   1 and   5
      !>           Found poem between lines   7 and  11
      !>           Found poem between lines   8 and  12
      !>
      !>     Note that line numbers are returned relative to the position
      !>     of the file pointer when LOCLN is called. The following code
      !>     fragment generates the numbers relative to the start of the
      !>     file.
      !>
      !>           REWIND ( UNIT )
      !>
      !>           OFFSET = 0
      !>           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
      !>
      !>           DO WHILE ( FOUND )
      !>              WRITE (*,*) 'Found poem between lines ',
      !>          .                OFFSET + B,
      !>          .                ' and ',
      !>          .                OFFSET + E
      !>
      !>              OFFSET = OFFSET + E
      !>              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
      !>           END DO
      !>
      !>           CLOSE ( UNIT )
      !>
      !>     The following report is produced:
      !>
      !>           Found poem between lines   1 and   5
      !>           Found poem between lines  12 and  16
      !>           Found poem between lines  24 and  28
      !>
      !>     2) Given the same file, the code fragment
      !>
      !>           CALL TXTOPR ( 'FILE.TXT', UNIT )
      !>
      !>           CALL LOCLN ( UNIT,
      !>          .             'begin poem',
      !>          .             'end poem',
      !>          .             LINE,
      !>          .             B,
      !>          .             E,
      !>          .             FOUND )
      !>
      !>           CLOSE ( UNIT )
      !>
      !>     finds nothing because case is significant: FOUND is false,
      !>     and B and E are unchanged.
      !>
      !>     3) This code fragment
      !>
      !>           CALL TXTOPR ( 'FILE.TXT', UNIT )
      !>
      !>           CALL LOCLN ( UNIT,
      !>          .             ' ',
      !>          .             'BEGIN PROSE',
      !>          .             LINE,
      !>          .             B,
      !>          .             E,
      !>          .             FOUND )
      !>
      !>           CLOSE ( UNIT )
      !>
      !>     when executed on the same file returns B = 1 and E = 7.
      !>     In effect, a blank begin marker "matches" the first line
      !>     that is read.
      !>
      !>     Similarly, a blank end marker "matches" the last line of
      !>     the file, the code fragment
      !>
      !>           CALL TXTOPR ( 'FILE.TXT', UNIT )
      !>
      !>           CALL LOCLN ( UNIT,
      !>          .             'BEGIN QUOTE',
      !>          .             ' ',
      !>          .             LINE,
      !>          .             B,
      !>          .             E,
      !>          .             FOUND )
      !>
      !>           CLOSE ( UNIT )
      !>
      !>     when executed on the same file returns B = 18 and E = 33.
      !>     If both markers are blank, LOCLN basically counts the lines
      !>     in the file.
      !>
      !>     4) The code fragment
      !>
      !>           CALL TXTOPR ( 'FILE.TXT', UNIT )
      !>
      !>           MARK = 'BEGIN POEM'
      !>
      !>           CALL LOCLN ( UNIT, MARK, MARK, LINE, FIRST, SECOND, FOUND )
      !>
      !>           CLOSE ( UNIT )
      !>
      !>     returns FIRST = 1 and SECOND = 12 -- the first two lines that
      !>     are equivalent to MARK.
      !>
      !>     5) Nesting is not supported. That is, if UNIT is connected to
      !>     a file containing the following lines (ignoring line numbers),
      !>
      !>           1   Begin Object
      !>           2     Begin Object
      !>           3       Begin Object
      !>           4         Just kidding!
      !>           5       End Object
      !>           6     End Object
      !>           7   End Object
      !>
      !>           REWIND ( UNIT )
      !>
      !>           CALL LOCLN ( UNIT,
      !>          .             'Begin Object'
      !>          .             'End Object',
      !>          .             LINE,
      !>          .             B,
      !>          .             E,
      !>          .             FOUND )
      !>
      !>     returns B = 1 and E = 5, not E = 7.
      !>
      !>     6) Let UNIT be connected to a text file containing the
      !>     following lines, again ignoring line numbers which are
      !>     listed for easy reference.
      !>
      !>           1    The first case tests the capability of ...
      !>           2
      !>           3    NEW CASE
      !>           4       TARGET = JUPITER
      !>           5       EPOCH  = 21 JUN 1992 13:04
      !>           6    END CASE
      !>           7
      !>           8    The next case uses a different target and a slightly
      !>           9    longer exposure time...
      !>           10
      !>           11   NEW CASE
      !>           12      TARGET   = IO
      !>           13      EPOCH    = 21 JUN 1992 13:04
      !>           14      EXPOSURE = 2.44 SECONDS
      !>           15   END CASE
      !>           16
      !>           17   The next case changes targets in order to...
      !>           18
      !>           19   NEW CASE
      !>           20      TARGET   = EUROPA
      !>           21      EPOCH    = 21 JUN 1992 13:04
      !>           22      EXPOSURE = 2.44 SECONDS
      !>           23   END CASE
      !>
      !>     Then the code fragment
      !>
      !>           REWIND ( UNIT )
      !>
      !>           BMARK = 'NEW CASE'
      !>           EMARK = 'END CASE'
      !>
      !>           CASES  = 0
      !>           OFFSET = 0
      !>           CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
      !>
      !>           DO WHILE ( FOUND )
      !>              CASES      = CASES  + 1
      !>              BEG(CASES) = OFFSET + B
      !>              END(CASES) = OFFSET + E
      !>
      !>              OFFSET = OFFSET + E
      !>              CALL LOCLN ( UNIT, BMARK, EMARK, LINE, B, E, FOUND )
      !>           END DO
      !>
      !>     saves the locations of the various input cases (skipping past
      !>     the intervening commentary) in the arrays BEG and END. After
      !>     running the code, CASES, BEG, and END have the following values:
      !>
      !>           CASES = 3
      !>           BEG   = 3,  11,  19
      !>           END   = 6,  15,  23
      !>
      !>     The following code fragment retrieves the i'th case.
      !>
      !>           REWIND ( UNIT )
      !>
      !>           DO J = 1, BEG(I) - 1
      !>              READ (UNIT,FMT='(A)') LINE
      !>           END DO
      !>
      !>           DO J = BEG(I), END(I)
      !>              READ (UNIT,FMT='(A)') LINE
      !>               .
      !>               .  Process the line
      !>               .
      !>           END DO
      !>
      !>     While this isn't an incredibly efficient way to process
      !>     large files, it can be effective for smaller files.
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
      !>     J.E. McLean        (JPL)
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
      !>-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
      module subroutine locln ( ctx, unit, bmark, emark, line, bline, eline, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: ltrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Logical unit connected to text file.
      character(*), intent(in) :: bmark  !! Begin marker.
      character(*), intent(in) :: emark  !! End marker.
      character(*), intent(inout) :: line  !! Workspace.
      integer(ip), intent(out) :: bline  !! Beginning line.
      integer(ip), intent(out) :: eline  !! Ending line.
      logical, intent(out) :: found  !! Markers found?

      integer(ip) :: bltemp
      integer(ip) :: eltemp
      integer(ip) :: iostat

      logical :: bfound
      logical :: efound
      logical :: eof

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     We'll use temporary variables BLTEMP and ELTEMP for BLINE and
!     ELINE until we know that both markers have been found.  We'll
!     use BFOUND to indicate whether or not BMARK was found, and
!     EFOUND to indicate whether or not EMARK was found.  EOF
!     indicates end of file.
!
      bltemp =  0
      bfound = .false.
      efound = .false.
      eof    = .false.

!
!     Read through the file, line by line, searching for the first
!     occurrence of BMARK and counting lines as we go.  Once we
!     find BMARK, we'll start searching for EMARK.  After each read
!     we'll check for I/O errors.
!

      do while (  ( .not. bfound )  .and.  ( .not. eof )  )

         read ( unit, fmt='(A)', iostat=iostat ) line
!
!        An end-of-file condition is indicated by a negative value
!        for IOSTAT. Any other non-zero value indicates some other
!        error.
!
         if ( iostat > 0 ) then

            call ctx%setmsg ( 'While searching for BMARK = #, an attempt '// &
                          'to read the file named FILENAME failed.  ' // &
                          'The value of IOSTAT is #.'                 )
            call ctx%errch  ( '#', bmark                                  )
            call ctx%errint ( '#', iostat                                 )
            call ctx%errfnm ( 'FILENAME', unit                            )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                     )
            return

         else if ( iostat < 0 ) then

            eof = .true.

         else
!
!           The read was successful, so count the line then
!           check for a match.
!
            bltemp = bltemp + 1

            line = adjustl(line)

!
!           By convention, if BMARK is blank, it matches the
!           first line that we read.  If it is not blank, we
!           compare it to the line just read, ignoring leading
!           and trailing blanks.
!
            if ( bmark == ' ' ) then
               bfound = .true.

            else
               if (  bmark ( ltrim(bmark): ) == line  ) then
                  bfound = .true.
               end if

            end if

         end if

      end do

!
!     Start the search for EMARK starting from where we left off.
!
      eltemp = bltemp

      do while (  ( .not. efound )  .and.  ( .not. eof )  )

         read ( unit, fmt='(A)', iostat=iostat ) line
!
!        An end-of-file condition is indicated by a negative value
!        for IOSTAT. Any other non-zero value indicates some other
!        error.
!
         if ( iostat > 0 ) then

            call ctx%setmsg ( 'While searching for EMARK = #, an attempt '// &
                          'to read the file named FILENAME failed.  ' // &
                          'The value of IOSTAT is #.'                 )
            call ctx%errch  ( '#', emark                                  )
            call ctx%errint ( '#', iostat                                 )
            call ctx%errfnm ( 'FILENAME', unit                            )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                     )
            return

         else if ( iostat < 0 ) then

            eof = .true.
!
!           By convention, if EMARK is blank, it matches the
!           last line in the file.
!
            if ( emark == ' ' ) then
               efound = .true.
            end if

         else
!
!           The read was successful, so count the line and check for
!           a match.
!
            eltemp = eltemp + 1

            line = adjustl(line)

            if ( emark /= ' ' ) then
               if ( emark ( ltrim(emark): ) == line  ) then
                  efound = .true.
               end if
            end if

         end if

      end do

!
!     Assign the line numbers to BLINE and ELINE only if both markers
!     were found.
!
      found = bfound .and. efound

      if ( found ) then

         bline = bltemp
         eline = eltemp

      end if

      return
      end subroutine locln

end submodule pepper_text
