!> Searching, sorting and other operations on arrays.
!>
!> The procedures keep SPICELIB's names, arguments and documentation. They
!> have no saved state and do not signal SPICE errors.
module pepper_arrays

   implicit none

   private

   public :: alltru, bschoc, bschoi, bsrchc, bsrchd, bsrchi, clearc, cleard,&
      cleari, fillc, filld, filli, isordv, isrchc, isrchd, isrchi, lstcld,&
      lstcli, lstlec, lstled, lstlei, lstltc, lstltd, lstlti, maxac, maxad,&
      maxai, minac, minad, minai, movec, moved, movei, notru, orderc,&
      orderd, orderi, prodad, prodai, reordc, reordd, reordi, reordl,&
      rmdupc, rmdupd, rmdupi, sameai, shellc, shelld, shelli, somfls,&
      somtru, sumad, sumai, swapc, swapd, swapi, touchc, touchd, touchi,&
      touchl

contains

      !> Determine if all the entries in an array of logicals are .TRUE.
      !>
      !> The function returns .TRUE. if all of the values in the array
      !> LOGCLS are true.
      !>
      !>## Detailed Input
      !>
      !>     LOGCLS   is an array of logicals.
      !>
      !>     N        is the number of elements in the array LOGCLS
      !>
      !>## Detailed Output
      !>
      !>     The function returns true if the value of every entry of LOGCLS
      !>     is .TRUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than 1, the function returns a value of .TRUE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function examines each element of LOGCLS until
      !>     a .FALSE. value is found or until all values have been
      !>     examined.
      !>
      !>## Examples
      !>
      !>     Suppose you needed to confirm that each a character set
      !>     WORDS contained all of the words in the phrase
      !>
      !>       'EVERY GOOD BOY DOES FINE'
      !>
      !>     You might execute the following block of code.
      !>
      !>           FOUND(1) = ELEMC  ( 'EVERY', WORDS )
      !>           FOUND(2) = ELEMC  ( 'GOOD',  WORDS )
      !>           FOUND(3) = ELEMC  ( 'BOY',   WORDS )
      !>           FOUND(4) = ELEMC  ( 'DOES',  WORDS )
      !>           FOUND(5) = ELEMC  ( 'FINE',  WORDS )
      !>
      !>           OK       = ALLTRU ( FOUND,   5     )
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
      !>-    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT)
      pure logical function alltru ( logcls, n )
      use pepper_kinds, only: ip
      implicit none

      logical, intent(in) :: logcls ( * )  !! An array of logicals.
      integer(ip), intent(in) :: n  !! Number of elements in the array LOGCLS.

      integer(ip) :: i

!
!     Just do it.
!

      do i = 1, n

         if ( .not. logcls(i) ) then

            alltru = .false.
            return

         end if

      end do

      alltru = .true.
      return

      end function alltru

      !> Do a binary search for a given value within an array of character
      !> strings, accompanied by an order vector. Return the index of the
      !> matching array entry, or zero if the key value is not found.
      !>
      !> The function returns the index of the first matching array element
      !> or zero if the value is not found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the key value to be found in the array. Trailing
      !>              blanks in this key are not significant: string matches
      !>              found by this routine do not require trailing blanks in
      !>              value to match those in the corresponding element of
      !>              array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    is the array of character strings to be searched.
      !>              Trailing blanks in the strings in this array are not
      !>              significant.
      !>
      !>     ORDER    is an order array that can be used to access the elements
      !>              of ARRAY in order (according to the ASCII collating
      !>              sequence). The contents of ORDER are a permutation of
      !>              the sequence of integers ranging from 1 to NDIM.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the specified value in the input
      !>     array. Indices range from 1 to NDIM.
      !>
      !>     If the input array does not contain the specified value, the
      !>     function returns zero.
      !>
      !>     If the input array contains more than one occurrence of the
      !>     specified value, the returned index may point to any of the
      !>     occurrences.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the value of the function is zero. This is not
      !>         considered an error.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A binary search is performed on the input array, whose order is
      !>     given by an associated order vector. If an element of the array is
      !>     found to match the input value, the index of that element is
      !>     returned. If no matching element is found, zero is returned.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Search for different character strings in an array that
      !>        is sorted following a given criteria, not necessarily
      !>        alphabetically.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BSCHOC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 BSCHOC
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NDIM
      !>      PARAMETER             ( NDIM   = 5  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 8  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      ARRAY  ( NDIM )
      !>      CHARACTER*(STRLEN)      NAMES  ( NDIM )
      !>
      !>      INTEGER                 I
      !>      INTEGER                 IDX
      !>      INTEGER                 ORDER  ( NDIM )
      !>
      !>C
      !>C     Let ARRAY and ORDER contain the following elements:
      !>C
      !>      DATA                    ARRAY  / 'FEYNMAN', 'BOHR',
      !>     .                     'EINSTEIN', 'NEWTON',  'GALILEO' /
      !>
      !>      DATA                    ORDER  / 2, 3, 1, 5, 4 /
      !>
      !>C
      !>C     Set the list of NAMES to be searched.
      !>C
      !>      DATA                    NAMES /  'NEWTON',  'EINSTEIN',
      !>     .                      'GALILEO', 'Galileo', 'BETHE'    /
      !>
      !>C
      !>C     Search for the NAMES.
      !>C
      !>      DO I = 1, NDIM
      !>
      !>         IDX = BSCHOC ( NAMES(I), NDIM, ARRAY, ORDER )
      !>
      !>         IF ( IDX .EQ. 0 ) THEN
      !>
      !>            WRITE(*,*) 'Name ', NAMES(I),
      !>     .                 ' not found in ARRAY.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'Name ', NAMES(I),
      !>     .                 ' found in position', IDX
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
      !>         Name NEWTON   found in position           4
      !>         Name EINSTEIN found in position           3
      !>         Name GALILEO  found in position           5
      !>         Name Galileo  not found in ARRAY.
      !>         Name BETHE    not found in ARRAY.
      !>
      !>        Note that these results indicate that:
      !>
      !>            ARRAY(4) = 'NEWTON'
      !>            ARRAY(3) = 'EINSTEIN'
      !>            ARRAY(5) = 'GALILEO'
      !>
      !>## Restrictions
      !>
      !>     1)  ORDER is assumed to give the order of the elements of ARRAY in
      !>         increasing order according to the ASCII collating sequence. If
      !>         this condition is not met, the results of BSCHOC are
      !>         unpredictable.
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
      !>-    SPICELIB Version 1.0.1, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Updated $Brief_I/O, $Detailed_Input and $Detailed_Output
      !>        sections to improve the description of the arguments and
      !>        returned values of the function.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT)
      pure integer(ip) function bschoc ( value, ndim, array, order )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: value  !! Key value to be found in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      character(*), intent(in) :: array ( * )  !! Character string array to search.
      integer(ip), intent(in) :: order ( * )  !! Order vector.

      integer(ip) :: left
      integer(ip) :: right
      integer(ip) :: i

!
!     Set the initial bounds for the search area.
!
      left  = 1
      right = ndim

      do while ( left <= right )

!
!        Check the middle element.
!
         i = (left+right)/2

!
!        If the middle element matches, return its location.
!
         if ( value == array(order(i)) ) then
            bschoc = order(i)
            return

!
!        Otherwise narrow the search area.
!
         else if ( llt (value, array(order(i))) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bschoc = 0

      return
      end function bschoc

      !> Do a binary search for a given value within an integer array,
      !> accompanied by an order vector. Return the index of the
      !> matching array entry, or zero if the key value is not found.
      !>
      !> The function returns the index of the first matching array element
      !> or zero if the value is not found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the value to be found in the input array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    is the array to be searched.
      !>
      !>     ORDER    is an order vector which can be used to access the
      !>              elements of ARRAY in order. The contents of order are a
      !>              permutation of the sequence of integers ranging from 1 to
      !>              NDIM.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the specified value in the input
      !>     array. Indices range from 1 to NDIM.
      !>
      !>     If the input array does not contain the specified value, the
      !>     function returns zero.
      !>
      !>     If the input array contains more than one occurrence of the
      !>     specified value, the returned index may point to any of the
      !>     occurrences.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the value of the function is zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A binary search is performed on the input array, whose order is
      !>     given by an associated order vector. If an element of the array is
      !>     found to match the input value, the index of that element is
      !>     returned. If no matching element is found, zero is returned.
      !>
      !>## Examples
      !>
      !>     Let ARRAY and ORDER contain the following elements:
      !>
      !>           ARRAY         ORDER
      !>           -----------   -----
      !>             100             2
      !>               1             3
      !>              10             1
      !>           10000             5
      !>            1000             4
      !>
      !>     Then
      !>
      !>           BSCHOI (  1000, 5, ARRAY, ORDER )  = 5
      !>           BSCHOI (     1, 5, ARRAY, ORDER )  = 2
      !>           BSCHOI ( 10000, 5, ARRAY, ORDER )  = 4
      !>           BSCHOI (    -1, 5, ARRAY, ORDER )  = 0
      !>           BSCHOI (    17, 5, ARRAY, ORDER )  = 0
      !>
      !>     That is,
      !>
      !>           ARRAY(5) =  1000
      !>           ARRAY(2) =     1
      !>           ARRAY(4) = 10000
      !>
      !>## Restrictions
      !>
      !>     1)  ORDER is assumed to give the order of the elements of ARRAY
      !>         in increasing order. If this condition is not met, the results
      !>         of BSCHOI are unpredictable.
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
      !>-    SPICELIB Version 1.0.1, 09-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT)
      pure integer(ip) function bschoi ( value, ndim, array, order )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: value  !! Value to find in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(in) :: array ( * )  !! Array to be searched.
      integer(ip), intent(in) :: order ( * )  !! Order vector.

      integer(ip) :: left
      integer(ip) :: right
      integer(ip) :: i

!
!     Set the initial bounds for the search area.
!
      left  = 1
      right = ndim

      do while ( left <= right )

!
!        Check the middle element.
!
         i = (left+right)/2

!
!        If the middle element matches, return its location.
!
         if ( value == array(order(i)) ) then
            bschoi = order(i)
            return

!
!        Otherwise narrow the search area.
!
         else if ( value < array(order(i)) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bschoi = 0

      return
      end function bschoi

      !> Do a binary search for a given value within a character array,
      !> assumed to be in nondecreasing order. Return the index of the
      !> matching array entry, or zero if the key value is not found.
      !>
      !> The function returns the index of the first matching array element
      !> or zero if the value is not found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the key value to be found in the array. Trailing
      !>              blanks in this key are not significant: string matches
      !>              found by this routine do not require trailing blanks in
      !>              value to match those in the corresponding element of
      !>              array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    is the array of character strings to be searched.
      !>              Trailing blanks in the strings in this array are not
      !>              significant. The elements in ARRAY are assumed to
      !>              sorted according to the ASCII collating sequence.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the specified value in the input
      !>     array. Indices range from 1 to NDIM.
      !>
      !>     If the input array does not contain the specified value, the
      !>     function returns zero.
      !>
      !>     If the input array contains more than one occurrence of the
      !>     specified value, the returned index may point to any of the
      !>     occurrences.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the value of the function is zero. This is not
      !>         considered an error.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A binary search is performed on the input array. If an element of
      !>     the array is found to match the input value, the index of that
      !>     element is returned. If no matching element is found, zero is
      !>     returned.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements:
      !>
      !>           'BOHR'
      !>           'EINSTEIN'
      !>           'FEYNMAN'
      !>           'GALILEO'
      !>           'NEWTON'
      !>
      !>     Then
      !>
      !>           BSRCHC ( 'NEWTON',   5, ARRAY )    = 5
      !>           BSRCHC ( 'EINSTEIN', 5, ARRAY )    = 2
      !>           BSRCHC ( 'GALILEO',  5, ARRAY )    = 4
      !>           BSRCHC ( 'Galileo',  5, ARRAY )    = 0
      !>           BSRCHC ( 'BETHE',    5, ARRAY )    = 0
      !>
      !>## Restrictions
      !>
      !>     1)  ARRAY is assumed to be sorted in increasing order according to
      !>         the ASCII collating sequence. If this condition is not met,
      !>         the results of BSRCHC are unpredictable.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Input and
      !>        $Detailed_Output section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure integer(ip) function bsrchc ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: value  !! Key value to be found in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      character(*), intent(in) :: array ( * )  !! Character string array to search.

      integer(ip) :: left
      integer(ip) :: right
      integer(ip) :: i

!
!     Set the initial bounds for the search area.
!
      left  = 1
      right = ndim

      do while ( left <= right )

!
!        Check the middle element.
!
         i = (left+right)/2

!
!        If the middle element matches, return its location.
!
         if ( value == array(i) ) then
            bsrchc = i
            return

!
!        Otherwise narrow the search area.
!
         else if ( llt (value, array(i)) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bsrchc = 0

      return
      end function bsrchc

      !> Do a binary search for a given value within a double precision
      !> array, assumed to be in nondecreasing order. Return the index of
      !> the matching array entry, or zero if the key value is not found.
      !>
      !> The function returns the index of VALUE in ARRAY, or zero if not
      !> found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the double precision value to be found in the input
      !>              array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    is the double precision array to be searched. The
      !>              elements in ARRAY are assumed to sorted in increasing
      !>              order.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the specified value in the input
      !>     array. Indices range from 1 to NDIM.
      !>
      !>     If the input array does not contain the specified value, the
      !>     function returns zero.
      !>
      !>     If the input array contains more than one occurrence of the
      !>     specified value, the returned index may point to any of the
      !>     occurrences.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the value of the function is zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A binary search is performed on the input array. If an element of
      !>     the array is found to match the input value, the index of that
      !>     element is returned. If no matching element is found, zero is
      !>     returned.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements:
      !>
      !>             -11.D0
      !>               0.D0
      !>              22.491D0
      !>             750.0D0
      !>
      !>     Then
      !>
      !>           BSRCHD ( -11.D0,    4, ARRAY )    = 1
      !>           BSRCHD (  22.491D0, 4, ARRAY )    = 3
      !>           BSRCHD ( 751.D0,    4, ARRAY )    = 0
      !>
      !>## Restrictions
      !>
      !>     1)  ARRAY is assumed to be sorted in increasing order. If this
      !>         condition is not met, the results of BSRCHD are unpredictable.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Output
      !>        section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure integer(ip) function bsrchd ( value, ndim, array )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: value  !! Value to find in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      real(wp), intent(in) :: array ( * )  !! Array to be searched.

      integer(ip) :: left
      integer(ip) :: right
      integer(ip) :: i

!
!     Set the initial bounds for the search area.
!
      left  = 1
      right = ndim

      do while ( left <= right )

!
!        Check the middle element.
!
         i = (left+right)/2

!
!        If the middle element matches, return its location.
!
         if ( value == array(i) ) then
            bsrchd = i
            return

!
!        Otherwise narrow the search area.
!
         else if ( value < array(i) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bsrchd = 0

      return
      end function bsrchd

      !> Do a binary search for a given value within an integer array,
      !> assumed to be in nondecreasing order. Return the index of the
      !> matching array entry, or zero if the key value is not found.
      !>
      !> The function returns the index of VALUE in ARRAY, or zero if not
      !> found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the value to be found in the input array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    is the integer array to be searched. The elements in
      !>              ARRAY are assumed to sorted in increasing order.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the specified value in the input
      !>     array. Indices range from 1 to NDIM.
      !>
      !>     If the input array does not contain the specified value, the
      !>     function returns zero.
      !>
      !>     If the input array contains more than one occurrence of the
      !>     specified value, the returned index may point to any of the
      !>     occurrences.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the value of the function is zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A binary search is performed on the input array. If an element of
      !>     the array is found to match the input value, the index of that
      !>     element is returned. If no matching element is found, zero is
      !>     returned.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements:
      !>
      !>             -11
      !>               0
      !>              22
      !>             750
      !>
      !>     Then
      !>
      !>           BSRCHI ( -11, 4, ARRAY )    = 1
      !>           BSRCHI (  22, 4, ARRAY )    = 3
      !>           BSRCHI ( 751, 4, ARRAY )    = 0
      !>
      !>## Restrictions
      !>
      !>     1)  ARRAY is assumed to be sorted in increasing order. If this
      !>         condition is not met, the results of BSRCHI are unpredictable.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Output
      !>        section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure integer(ip) function bsrchi ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: value  !! Value to find in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(in) :: array ( * )  !! Array to be searched.

      integer(ip) :: left
      integer(ip) :: right
      integer(ip) :: i

!
!     Set the initial bounds for the search area.
!
      left  = 1
      right = ndim

      do while ( left <= right )

!
!        Check the middle element.
!
         i = (left+right)/2

!
!        If the middle element matches, return its location.
!
         if ( value == array(i) ) then
            bsrchi = i
            return

!
!        Otherwise narrow the search area.
!
         else if ( value < array(i) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bsrchi = 0

      return
      end function bsrchi

      !> Fill an array of strings with blank strings.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the number of elements in ARRAY which are to be set to
      !>              blank.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is the array of strings with each of its NDIM elements
      !>              set to a blank string. If NDIM is smaller than the
      !>              declared dimension of ARRAY, only the first NDIM
      !>              elements of ARRAY are set to blank; all other elements
      !>              remain unchanged.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the array is not modified.
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
      !>     1) Initialize a one dimensional string array and then clear
      !>        the first two strings.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CLEARC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               STRSZ
      !>      PARAMETER           ( STRSZ = 21 )
      !>
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM = 4   )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRSZ)     ARRAY(NDIM)
      !>      INTEGER               I
      !>
      !>C
      !>C     Initialize ARRAY.
      !>C
      !>      ARRAY(1) = 'Element #1'
      !>      ARRAY(2) = 'Element #2'
      !>      ARRAY(3) = 'Element #3'
      !>      ARRAY(4) = 'Element #4'
      !>
      !>      WRITE(*,'(A)') 'Contents of ARRAY before CLEARC:'
      !>      WRITE(*,*)
      !>      DO I = 1, NDIM
      !>         WRITE(*,'(A,I1,2A)') 'Position #', I , ': ', ARRAY(I)
      !>      END DO
      !>
      !>C
      !>C     Clear the first 2 elements.
      !>C
      !>      CALL CLEARC ( 2, ARRAY )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Contents of ARRAY after CLEARC:'
      !>      WRITE(*,*)
      !>      DO I = 1, NDIM
      !>         WRITE(*,'(A,I1,2A)') 'Position #', I , ': ', ARRAY(I)
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Contents of ARRAY before CLEARC:
      !>
      !>        Position #1: Element #1
      !>        Position #2: Element #2
      !>        Position #3: Element #3
      !>        Position #4: Element #4
      !>
      !>        Contents of ARRAY after CLEARC:
      !>
      !>        Position #1:
      !>        Position #2:
      !>        Position #3: Element #3
      !>        Position #4: Element #4
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 19-MAY-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine clearc ( ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ndim  !! Number of elements of ARRAY to be set to blank.
      character(*), intent(out) :: array ( * )  !! Array of strings to be filled with blank strings.

      array(1:ndim) = ' '
      end subroutine clearc

      !> Fill a double precision array with zeros.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the number of elements in ARRAY which are to be
      !>              set to zero.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is the double precision array which is to be filled
      !>              with zeros.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the array is not modified.
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
      !>     1) Initialize all members of a double precision array to the
      !>        same value and clear it afterwards.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CLEARD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      ARRAY ( NDIM )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Initialize all member of the array ARRAY to 11.5, and
      !>C     print out its contents.
      !>C
      !>      CALL FILLD ( 11.5D0, NDIM, ARRAY )
      !>
      !>      WRITE(*,'(A)') 'Contents of ARRAY before CLEARD:'
      !>      WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM )
      !>
      !>C
      !>C     Clear the contents of ARRAY and print it.
      !>C
      !>      CALL CLEARD ( NDIM, ARRAY )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Contents of ARRAY after CLEARD:'
      !>      WRITE(*,'(4F6.1)') ( ARRAY(I), I=1, NDIM )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Contents of ARRAY before CLEARD:
      !>          11.5  11.5  11.5  11.5
      !>
      !>        Contents of ARRAY after CLEARD:
      !>           0.0   0.0   0.0   0.0
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
      !>     W.M. Owen          (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 29-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine cleard ( ndim, array )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! The number of elements of ARRAY which are to be set to
                                       !! zero.
      real(wp), intent(out) :: array ( * )  !! Double precision array to be filled.

      array(1:ndim) = 0.0_wp
      end subroutine cleard

      !> Fill an integer array with zeros.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the number of elements in ARRAY which are to be
      !>              set to zero.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is the integer array which is to be filled with
      !>              zeros.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the array is not modified.
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
      !>     1) Initialize all members of an integer array to the same value
      !>        and clear it afterwards.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CLEARI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               ARRAY ( NDIM )
      !>      INTEGER               I
      !>
      !>C
      !>C     Initialize all member of the array ARRAY to 11, and
      !>C     print out its contents.
      !>C
      !>      CALL FILLI ( 11, NDIM, ARRAY )
      !>
      !>      WRITE(*,'(A)') 'Contents of ARRAY before CLEARI:'
      !>      WRITE(*,'(4I4)') ( ARRAY(I), I=1, NDIM )
      !>
      !>C
      !>C     Clear the contents of ARRAY and print it.
      !>C
      !>      CALL CLEARI ( NDIM, ARRAY )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Contents of ARRAY after CLEARI:'
      !>      WRITE(*,'(4I4)') ( ARRAY(I), I=1, NDIM )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Contents of ARRAY before CLEARI:
      !>          11  11  11  11
      !>
      !>        Contents of ARRAY after CLEARI:
      !>           0   0   0   0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 19-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine cleari ( ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ndim  !! The number of elements of ARRAY which are to be set to
                                       !! zero.
      integer(ip), intent(out) :: array ( * )  !! Integer array to be filled.

      array(1:ndim) = 0
      end subroutine cleari

      !> Fill a character string array with a specified string.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the value to be assigned to the array elements
      !>              1 through NDIM.
      !>
      !>     NDIM     is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is a character string array whose elements are to be
      !>              set to VALUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the array is not modified.
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
      !>     Let  VALUE = '*'
      !>          NDIM  =  4
      !>
      !>     then the contents of ARRAY are:
      !>
      !>          ARRAY (1) = '*'
      !>          ARRAY (2) = '*'
      !>          ARRAY (3) = '*'
      !>          ARRAY (4) = '*'
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
      !>     W.M. Owen          (JPL)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine fillc ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: value  !! Character string value to be placed in ARRAY.
      integer(ip), intent(in) :: ndim  !! The number of elements in ARRAY.
      character(*), intent(out) :: array ( * )  !! Character string array which is to be filled.

      array(1:ndim) = value
      end subroutine fillc

      !> Fill a double precision array with a specified value.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the value to be assigned to the array elements
      !>              1 through NDIM.
      !>
      !>     NDIM     is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is a double precision array whose elements are to be
      !>              set to VALUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the array is not modified.
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
      !>     1) Initialize all members of a double precision array to the same
      !>        value.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM FILLD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      ARRAY ( NDIM )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Initialize all members of the array ARRAY to 1, and
      !>C     print out its contents.
      !>C
      !>      CALL FILLD ( 1.D0, NDIM, ARRAY )
      !>
      !>      WRITE(*,'(A)') 'Contents of ARRAY:'
      !>      DO I=1, NDIM
      !>
      !>         WRITE(*,'(A,I2,A,F4.1)') '   Index:', I,
      !>     .                            '; value:',  ARRAY(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Contents of ARRAY:
      !>           Index: 1; value: 1.0
      !>           Index: 2; value: 1.0
      !>           Index: 3; value: 1.0
      !>           Index: 4; value: 1.0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 19-FEB-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine filld ( value, ndim, array )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: value  !! Double precision value to be placed in all the elements of
                                     !! ARRAY.
      integer(ip), intent(in) :: ndim  !! The number of elements in ARRAY.
      real(wp), intent(out) :: array ( * )  !! Double precision array which is to be filled.

      array(1:ndim) = value
      end subroutine filld

      !> Fill an integer array with a specified value.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the value to be assigned to the array elements
      !>              1 through NDIM.
      !>
      !>     NDIM     is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is a integer array whose elements are to be set
      !>              to VALUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the array is not modified.
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
      !>     1) Initialize all members of an integer array to the same value.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM FILLI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               ARRAY ( NDIM )
      !>      INTEGER               I
      !>
      !>C
      !>C     Initialize all members of the array ARRAY to 11, and
      !>C     print out its contents.
      !>C
      !>      CALL FILLI ( 11, NDIM, ARRAY )
      !>
      !>      WRITE(*,'(A)') 'Contents of ARRAY:'
      !>      DO I=1, NDIM
      !>
      !>         WRITE(*,'(A,I2,A,I3)') '   Index:', I,
      !>     .                          '; value:', ARRAY(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Contents of ARRAY:
      !>           Index: 1; value: 11
      !>           Index: 2; value: 11
      !>           Index: 3; value: 11
      !>           Index: 4; value: 11
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 19-FEB-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine filli ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: value  !! Integer value to be placed in all the elements of ARRAY.
      integer(ip), intent(in) :: ndim  !! The number of elements in ARRAY.
      integer(ip), intent(out) :: array ( * )  !! Integer array which is to be filled.

      array(1:ndim) = value
      end subroutine filli

      !> Determine whether an array of N items contains the integers
      !> 1 through N.
      !>
      !> The function returns .TRUE. if the array contains the integers
      !> 1 through N, otherwise it returns .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an array of integers. Often this will be an array
      !>              that is a candidate order vector to be passed to
      !>              a routine for re-ordering some parallel array.
      !>
      !>     N        is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if the array contains the integers
      !>     1 through N. Otherwise it returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N < 1, the function returns .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function provides a simple means of determining whether
      !>     or not an array of N integers contains exactly the integers
      !>     1 through N.
      !>
      !>## Examples
      !>
      !>     1) Suppose you wished to reorder an array of strings based upon
      !>        a ranking array supplied by a user. If the ranking array
      !>        contains any duplicates or refers to indices that are out
      !>        of the range of valid indices for the array of strings,
      !>        the attempt to reorder the array of strings cannot succeed.
      !>        Its usually better to detect such a possibility before
      !>        you begin trying to reorder the array of strings. This routine
      !>        will detect the error.
      !>
      !>        The block of code below illustrates this idea.
      !>
      !>           IF ( ISORDV ( ORDVEC, N ) ) THEN
      !>
      !>              ...reorder the input array of strings
      !>
      !>              CALL REORDC ( ORDVEC, N, STRNGS )
      !>
      !>           ELSE
      !>
      !>              ...state the problem and let the user decide what
      !>              to do about it.
      !>                    .
      !>                    .
      !>                    .
      !>
      !>           END IF
      !>
      !>     2) This routine can also be used to determine whether or not an
      !>        array contains every integer between K and N (where K < N ).
      !>
      !>           First subtract K-1 from each integer
      !>
      !>           DO I = 1, N-K+1
      !>              ARRAY(I) = ARRAY(I) - K + 1
      !>           END DO
      !>
      !>           See if the modified array is an order vector
      !>
      !>           OK = ISORDV ( ARRAY, N-K )
      !>
      !>           Return the array to its original state.
      !>
      !>           DO I = 1, N-K+1
      !>              ARRAY(I) = ARRAY(I) + K - 1
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 03-JUL-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 06-MAR-1991 (NJB) (WLT) (IMU)
      logical function isordv ( array, n )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: array (*)  !! Array of integers.
      integer(ip), intent(in) :: n  !! Number of integers in ARRAY.

      integer(ip) :: i
      integer(ip) :: j

!
!     Let's take care of the goofy case first.
!
      if      ( n < 1 ) then
         isordv = .false.
         return
      else if ( n == 1 ) then
         isordv = array(1) == 1
         return
      end if

!
!     Make an initial pass through the array to be sure we
!     have legitimate values.
!
      do i = 1, n

         if (      ( array(i) < 1 ) &
              .or. ( array(i) > n )  ) then
            isordv = .false.
            return
         end if

      end do

!
!     Ok. All of the values are in range.  We just need to check
!     that this array could actually be used as an order vector.
!
!     For each I between 1 and N,  ARRAY(I) is some integer between 1
!     and N.  The only question remaining is whether the set
!     { ARRAY(I), I=1,N } contains every integer between 1 and N.
!
!     Suppose for a moment we could allocate a logical array called HITS
!
!           LOGICAL               HITS(N)
!
!     Then the following scheme could be used to determine whether or
!     not { ARRAY(I), I=1,N } contains every integer between 1 and N.
!
!        Initialize every entry of HITS to .FALSE.
!
!           DO I = 1, N
!              HITS(I) = .FALSE.
!           END DO
!
!        Then for each I set HITS(ARRAY(I)) to .TRUE.
!
!           DO I = 1, N
!              HITS(ARRAY(I)) = .TRUE.
!           END DO
!
!     What can be said about HITS at this point? If for any entry J,
!     HITS(J) is true then some ARRAY(I) is equal to J.
!
!     If all HITS are .TRUE. then {ARRAY(I), I=1,N} is in fact the
!     set of integers 1 to N.  Otherwise those J such that
!     HITS(J) = .FALSE. are the integers between 1 and N that are
!     missed by ARRAY.
!
!     It turns out we don't need to allocate an array of logicals;
!     we can use just use part of the input array, ARRAY.
!
!     The storage locations ARRAY(1) through ARRAY(N) can be viewed
!     as two parallel arrays:  SIGN_BIT and UNSIGNED
!
!          SIGN
!          BIT  UNSIGNED PORTION
!         +----+-----------------+
!      1  |    |                 |
!         +----+-----------------+
!      2  |    |                 |
!         +----+-----------------+
!      3  |    |                 |
!         +----+-----------------+
!
!                 .
!                 .
!                 .
!
!         +----+-----------------+
!     N-1 |    |                 |
!         +----+-----------------+
!     N   |    |                 |
!         +----+-----------------+
!
!     Since we know the value of all of the sign bits (it's '+') we can
!     alter them and then reset them once we are done.
!
!     We will choose for our array of HITS the SIGN_BITS of ARRAY.
!     We regard '+' as FALSE and '-' as TRUE.
!
!        DO I = 1, N
!           SIGN_BIT ( UNSIGNED(I) ) = '-'
!        END DO
!
!     Then check to make sure that all of the sign bits are '-'.
!
      do i = 1, n
         j        = abs( array(i) )
         array(j) = -    array(j)
      end do

!
!     Check each item to see if it's been hit.
!
      isordv = .true.

      do i = 1, n
         isordv   =  isordv .and. ( array(i) < 0 )
         array(i) =          abs  ( array(i)        )
      end do

      return
      end function isordv

      !> Search for a given value within a character string array. Return
      !> the index of the first matching array entry, or zero if the key
      !> value was not found.
      !>
      !> The function returns the index of the first matching array
      !> element or zero if the value is not found.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (FINDLOC instead).
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the key value to be found in the array. Trailing
      !>              blanks in this key are not significant: string matches
      !>              found by this routine do not require trailing blanks in
      !>              value to match those in the corresponding element of
      !>              array.
      !>
      !>     NDIM     is the dimension of the array.
      !>
      !>     ARRAY    is the character array to be searched. Trailing
      !>              blanks in the strings in this array are not significant.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first matching array
      !>     element in ARRAY. If VALUE is not found, ISRCHC is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the function value is zero.
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
      !>     The following table shows the value of ISRCHC given the contents
      !>     of ARRAY and VALUE:
      !>
      !>       ARRAY                 VALUE     ISRCHC
      !>     -----------------       -----     ------
      !>     '1', '0', '4', '2'       '4'        3
      !>     '1', '0', '4', '2'       '2'        4
      !>     '1', '0', '4', '2'       '3'        0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 03-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Extended
      !>        description of input arguments.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure integer(ip) function  isrchc ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: value  !! Key value to be found in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      character(*), intent(in) :: array ( * )  !! Character string array to search.

      isrchc = findloc(array(1:ndim), value, dim=1)
      end function isrchc

      !> Search for a given value within a double precision array. Return
      !> the index of the first matching array entry, or zero if the key
      !> value was not found.
      !>
      !> The function returns the index of the first matching array
      !> element or zero if the value is not found.
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the key value to be found in the array.
      !>
      !>     NDIM     is the dimension of the array.
      !>
      !>     ARRAY    is the double precision array to be searched.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first matching array
      !>     element in ARRAY. If VALUE is not found, ISRCHD is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the function value is zero.
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
      !>     The following table shows the value of ISRCHD given the contents
      !>     of ARRAY and VALUE:
      !>
      !>       ARRAY                         VALUE   ISRCHD
      !>     ---------------------------     -----   ------
      !>     1.0D0, 0.0D0, 4.0D0, 2.0D0      4.0D0     3
      !>     1.0D0, 0.0D0, 4.0D0, 2.0D0      2.OD0     4
      !>     1.0D0, 0.0D0, 4.0D0, 2.0D0      3.0D0     0
      !>
      !>## Restrictions
      !>
      !>     1)  CAUTION must be exercised when comparing floating point
      !>         numbers for equality. If the numbers in ARRAY or the number in
      !>         VALUE are the result of computations, then it is likely that
      !>         strict equality between VALUE and some element of ARRAY will
      !>         NOT hold (even if the two numbers are very close) unless the
      !>         numbers are the result of exactly the same computations.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 03-JUL-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure integer(ip) function  isrchd ( value, ndim, array )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: value  !! Key value to be found in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      real(wp), intent(in) :: array ( * )  !! Double precision array to search.

      integer(ip) :: i

      isrchd = 0

      do i = 1, ndim

         if ( array(i) == value ) then
            isrchd = i
            return
         end if

      end do

      return
      end function isrchd

      !> Search for a given value within a integer array. Return
      !> the index of the first matching array entry, or zero if
      !> the key value was not found.
      !>
      !> The function returns the index of the first matching array
      !> element or zero if the value is not found.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (FINDLOC instead).
      !>
      !>## Detailed Input
      !>
      !>     VALUE    is the key value to be found in the array.
      !>
      !>     NDIM     is the dimension of the array.
      !>
      !>     ARRAY    is the integer array to be searched.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the first matching array
      !>     element in ARRAY. If VALUE is not found, ISRCHI is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the function value is zero.
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
      !>     The following table shows the value of ISRCHI given the contents
      !>     of ARRAY and VALUE:
      !>
      !>       ARRAY        VALUE   ISRCHI
      !>     ----------     -----   ------
      !>     1, 0, 4, 2       4        3
      !>     1, 0, 4, 2       2        4
      !>     1, 0, 4, 2       3        0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 03-JUL-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure integer(ip) function  isrchi ( value, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: value  !! Key value to be found in ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(in) :: array ( * )  !! Integer array to search.

      isrchi = findloc(array(1:ndim), value, dim=1)
      end function isrchi

      !> Find the index of the array element closest to a given number X
      !> in an array of non-decreasing numbers.
      !>
      !> The function returns the index of the element of ARRAY
      !> whose value is closest to X.
      !>
      !>## Detailed Input
      !>
      !>     X        is the value to be compared with the elements of ARRAY.
      !>
      !>     N        is the number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of double precision numbers such that
      !>
      !>                         ARRAY( I ) <= ARRAY( J )
      !>
      !>              for all I < J.
      !>
      !>## Detailed Output
      !>
      !>     LSTCLD   is the index of the element of the non-decreasing
      !>              sequence: {ARRAY(I) : 1 <= I <= N} that is closest
      !>              to X. In other words, ARRAY( LSTCLD( X, N, ARRAY ) )
      !>              is the element of ARRAY whose value is closest to X.
      !>
      !>              If X falls precisely on the midpoint of consecutive array
      !>              elements, the index of the larger of the two values is
      !>              returned.
      !>
      !>              If X is closest to a value which appears more than
      !>              once in the array (since the array is ordered, these
      !>              elements would have to be consecutive), the highest index
      !>              for that value will be returned.
      !>
      !>              LSTCLD = I for some I in the range 1 to N, unless N is
      !>              less than or equal to zero, in which case LSTCLD is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the value of N is non-positive, LSTCLD returns the value
      !>         zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     LSTCLD uses a binary search algorithm to locate the value closest
      !>     to X in the non-decreasing sequence of double precision numbers
      !>     represented by the elements of ARRAY.
      !>
      !>## Examples
      !>
      !>     Suppose ARRAY contains the following double precision elements:
      !>
      !>     ARRAY: -1    0    1    1.5   1.5    2    3    9    9.5   100
      !>
      !>     index:  1    2    3     4     5     6    7    8     9     10
      !>
      !>     The following table shows the values of LSTCLD that would be
      !>     returned for various values of X, and the corresponding closest
      !>     array element values.
      !>
      !>            X      LSTCLD( X,10,ARRAY )   ARRAY( LSTCLD( X,10,ARRAY ))
      !>          -----    --------------------   ---------------------------
      !>           0.12             2                          0
      !>          -0.12             2                          0
      !>          -2.0              1                         -1
      !>           2.5              7                          3
      !>           1.3              5                        1.5
      !>         100.0             10                        100
      !>         100.1             10                        100
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence is not non-decreasing, the routine will run
      !>         to completion but the index found will not mean anything.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
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
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>        1) The name of this routine was changed from CLOSTD to
      !>           LSTCLD because it was a more descriptive name.
      !>        2) All references (comments and code) were changed to reflect
      !>           the name change.
      !>
      !>-    Beta Version 1.0.0, 15-MAY-1990 (RET)
      pure integer(ip) function  lstcld ( x, n, array )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: x  !! Search value.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      real(wp), intent(in) :: array ( * )  !! Array to be searched.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

!
!     Save the size of the array and point to the beginning and ending
!     positions. The pointers delimit the current search interval.
!
      items = n
      begin = 1
      end   = n

      if      ( n <= 0 ) then

!
!        There is nothing in the array to compare against. Zero is the
!        only sensible thing to return.
!
         lstcld = 0
         return

      else if ( x <= array( begin ) ) then

!
!        All elements of the array are at least as big as X. So the
!        first element is the closest to X.
!
         lstcld = 1

      else if ( array( end ) <= x ) then

!
!        X is at least as big as all elements of the array.  So the last
!        element is the closest to X.
!
         lstcld = end

      else

!
!        X lies between some pair of elements of the array.
!
         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( array( middle ) < x ) then
               begin  = middle
            else
               end    = middle
            end if

            items     = 1 + ( end - begin )

         end do

!
!        Which of the two is closest?
!
         if ( x - array( begin ) < array( end ) - x ) then
            lstcld = begin
         else
            lstcld = end
         end if

      end if

!
!     March down the array to find the last element equal to the
!     closet value.
!
      do while ( lstcld < n )
         if ( .not. ( array( lstcld ) == array( lstcld + 1 ) ) ) exit

         lstcld = lstcld + 1

      end do

      return
      end function lstcld

      !> Find the index of the array element closest to a given integer X
      !> in an array of non-decreasing integers.
      !>
      !> The function returns the index of the element of ARRAY
      !> whose value is closest to X.
      !>
      !>## Detailed Input
      !>
      !>     X        is the value to be compared with the elements of ARRAY.
      !>
      !>     N        is the number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of integers such that
      !>
      !>                         ARRAY( I ) <= ARRAY( J )
      !>
      !>              for all I < J.
      !>
      !>## Detailed Output
      !>
      !>     LSTCLI   is the index of the element of the non-decreasing
      !>              sequence: {ARRAY(I) : 1 <= I <= N} that is closest to
      !>              X. In other words, ARRAY( LSTCLI( X, N, ARRAY ) ) is the
      !>              closest element of ARRAY to X.
      !>
      !>              If X falls precisely on the midpoint of consecutive array
      !>              elements, the index of the larger of the two values is
      !>              returned.
      !>
      !>              If X is closest to a value which appears more than
      !>              once in the array (since the array is ordered, these
      !>              elements would have to be consecutive), the highest index
      !>              for that value will be returned.
      !>
      !>              LSTCLI = I for some I in the range 1 to N, unless N is
      !>              less than or equal to zero, in which case LSTCLI is zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the value of N is non-positive, LSTCLI returns the value
      !>         zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     LSTCLI uses a binary search algorithm to locate the value closest
      !>     to X in the non-decreasing sequence of integers represented by
      !>     the elements of ARRAY.
      !>
      !>## Examples
      !>
      !>     Suppose ARRAY contains the following integer elements:
      !>
      !>     ARRAY: -1    0    10  15    15    20    30   39  40  10
      !>
      !>     index:  1    2    3    4     5     6     7    8   9   10
      !>
      !>     The following table shows the values of LSTCLI that would be
      !>     returned for various values of X, and the corresponding closest
      !>     array element values.
      !>
      !>            X      LSTCLI( X,10,ARRAY )   ARRAY( LSTCLI( X,10,ARRAY ))
      !>          -----    --------------------   ---------------------------
      !>           -2               1                         -1
      !>           -1               1                         -1
      !>            1               2                          0
      !>           14               5                         15
      !>           17               5                         15
      !>           18               6                         20
      !>           60               9                         40
      !>          110              10                        100
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence is not non-decreasing, the routine will run
      !>         to completion but the index found will not mean anything.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
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
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET)
      pure integer(ip) function  lstcli ( x, n, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: x  !! Search value.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      integer(ip), intent(in) :: array ( * )  !! Array to be searched.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

!
!     Save the size of the array and point to the beginning and ending
!     positions. The pointers delimit the current search interval.
!
      items = n
      begin = 1
      end   = n

      if      ( n <= 0 ) then

!
!        There is nothing in the array to compare against. Zero is the
!        only sensible thing to return.
!
         lstcli = 0
         return

      else if ( x <= array( begin ) ) then

!
!        All elements of the array are at least as big as X. So the
!        first element is the closest to X.
!
         lstcli = 1

      else if ( array( end ) <= x ) then

!
!        X is at least as big as all elements of the array.  So the last
!        element is the closest to X.
!
         lstcli = end

      else

!
!        X lies between some pair of elements of the array.
!
         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( array( middle ) <= x ) then
               begin  = middle
            else
               end    = middle
            end if

            items     = 1 + ( end - begin )

         end do

!
!        Which of the two is closest?
!
         if ( x - array( begin ) < array( end ) - x ) then
            lstcli = begin
         else
            lstcli = end
         end if

      end if

!
!     March down the array to find the last element equal to the
!     closet value.
!
      do while ( lstcli < n )
         if ( .not. ( array( lstcli ) == array( lstcli + 1 ) ) ) exit

         lstcli = lstcli + 1

      end do

      return
      end function lstcli

      !> Find the index of the largest array element less than or equal to
      !> a given character string in an ordered array of character strings.
      !>
      !> The function returns the index of the last element of ARRAY that
      !> is lexically less than or equal to STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a string acting as an upper bound: the element of
      !>              ARRAY that is lexically the greatest element less than or
      !>              equal to STRING is to be found. Trailing blanks in this
      !>              bound value are not significant.
      !>
      !>     N        is the total number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of character strings to be searched. Trailing
      !>              blanks in the strings in this array are not significant.
      !>              The strings in ARRAY must be sorted in non-decreasing
      !>              order. The elements of ARRAY need not be distinct.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the highest-indexed element in
      !>     the input array that is lexically less than or equal to STRING.
      !>     The routine assumes the array elements are sorted in
      !>     non-decreasing order.
      !>
      !>     Indices range from 1 to N.
      !>
      !>     If all elements of ARRAY are lexically greater than STRING, the
      !>     routine returns the value 0. If N is less than or equal to zero,
      !>     the routine returns the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than or equal to zero, the function returns 0.
      !>         This case is not treated as an error.
      !>
      !>     2)  If the input array is not sorted in non-decreasing order, the
      !>         output of this routine is undefined. No error is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses a binary search algorithm and so requires
      !>     at most on the order of
      !>
      !>        log (N)
      !>           2
      !>
      !>     steps to compute the value of LSTLEC.
      !>
      !>     Note: If you need to find the first element of the array that is
      !>     lexically greater than STRING, simply add 1 to the result returned
      !>     by this function and check to see if the result is within the
      !>     array bounds given by N.
      !>
      !>## Examples
      !>
      !>     Suppose that you have a long list of words, sorted alphabetically
      !>     and entirely in upper case. Furthermore suppose you wished to
      !>     find all words that begin the sequence of letters PLA,  then
      !>     you could execute the following code.
      !>
      !>           START = 0
      !>           I     = 1
      !>
      !>           DO I = 1, NWORDS
      !>
      !>              IF ( WORD(I)(1:3) .EQ. 'PLA' ) THEN
      !>
      !>                 IF ( START .EQ. 0 ) THEN
      !>                    START = I
      !>                 END IF
      !>
      !>                 END = I
      !>              END IF
      !>
      !>           END DO
      !>
      !>     This can of course be improved by stopping the loop once START
      !>     is non-zero and END remains unchanged after a pass through the
      !>     loop. However, this is a linear search  and on average can be
      !>     expected to take NWORDS/2 comparisons. The above algorithm
      !>     fails to take advantage of the structure of the list of words
      !>     (they are sorted).
      !>
      !>     The code below is much simpler to code, simpler to check, and
      !>     much faster than the code above.
      !>
      !>           START = LSTLEC( 'PL ', NWORDS, WORDS ) + 1
      !>           END   = LSTLEC( 'PLA', NWORDS, WORDS )
      !>
      !>           do something in case there are no such words.
      !>
      !>           IF ( START .GT. END ) THEN
      !>              START = 0
      !>              END   = 0
      !>           END IF
      !>
      !>     This code will never exceed 2 * LOG_2 ( NWORDS ) comparisons.
      !>     For a large list of words (say 4096) the second method will
      !>     take 24 comparisons  the first method requires on average
      !>     2048 comparisons. About 200 times as much time. Its clear
      !>     that if searches such as this must be performed often, that
      !>     the second approach could make the difference between being
      !>     able to perform the task in a few minutes as opposed to
      !>     several hours.
      !>
      !>     For more ideas regarding the use of this routine see LSTLEI
      !>     and LSTLTI.
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence of character strings in the input array ARRAY
      !>         is not non-decreasing, the program will run to completion but
      !>         the index found will not mean anything.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Input,
      !>        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
      !>        sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
      pure integer(ip) function  lstlec ( string, n, array )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Upper bound value to search against.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      character(*), intent(in) :: array ( * )  !! Array of possible lower bounds.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

      items = n

      begin = 1
      end   = n

      if      ( n <= 0            ) then

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         lstlec = 0

      else if ( llt (string, array(begin)) ) then

!
!        None of the array elements are less than or equal to STRING
!
         lstlec = 0

      else if ( lge (string, array(end))   ) then

!
!        STRING is greater than or equal to all elements of the array.
!        Thus the last element of the array is the last item less than
!        or equal to STRING.
!
         lstlec = end

      else

!
!        STRING lies between some pair of elements of the array
!

         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( lle (array(middle), string) ) then
               begin = middle
            else
               end   = middle
            end if

            items    = 1 + (end - begin)
         end do

         lstlec = begin

      end if

      return
      end function lstlec

      !> Find the index of the largest array element less than or equal
      !> to a given number X in an array of non-decreasing numbers.
      !>
      !> The function returns the index of the last element of ARRAY that
      !> is less than or equal to X.
      !>
      !>## Detailed Input
      !>
      !>     X        is a double precision value acting as an upper bound: the
      !>              element of ARRAY that is the greatest element less than
      !>              or equal to X is to be found.
      !>
      !>     N        is the total number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of double precision numbers that forms a
      !>              non-decreasing sequence. The elements of array need not
      !>              be distinct.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the highest-indexed element in
      !>     the input array that is less than or equal to X. The routine
      !>     assumes the array elements are sorted in non-decreasing order.
      !>
      !>     Indices range from 1 to N.
      !>
      !>     If all elements of ARRAY are greater than X, the routine returns
      !>     the value 0. If N is less than or equal to zero, the routine
      !>     returns the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than or equal to zero, the function returns 0.
      !>         This case is not treated as an error.
      !>
      !>     2)  If the input array is not sorted in non-decreasing order, the
      !>         output of this routine is undefined. No error is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses a binary search algorithm and so requires
      !>     at most on the order of
      !>
      !>        log (N)
      !>           2
      !>
      !>     steps to compute the value of LSTLED.
      !>
      !>     Note: If you need to find the first element of the array that is
      !>     greater than X, simply add 1 to the result returned by this
      !>     function and check to see if the result is within the array bounds
      !>     given by N.
      !>
      !>## Examples
      !>
      !>     If ARRAY(I) = -1 + 4*I/3 (real arithmetic implied here)
      !>
      !>     N        = 10
      !>     X        = 7.12
      !>
      !>     then
      !>
      !>     LSTLED will be I where
      !>             (4*I/3) - 1       < or = 7.12
      !>     but
      !>             (4*(I+1)/3) - 1   >      7.12 .
      !>
      !>     In this case our subsequence is:
      !>            1/3, 5/3, 9/3, 13/3, 17/3, 21/3, 25/3, .... 37/3
      !>
      !>     index:  1    2    3    4     5     6     7    ....  10
      !>
      !>     Thus LSTLED will be returned as 6
      !>
      !>     The following table shows the values of LSTLED that would be
      !>     returned for various values of X
      !>
      !>            X       LSTLED
      !>          -----     -------
      !>           0.12        0
      !>           1.34        1
      !>           5.13        4
      !>           8.00        6
      !>          15.10       10
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence of double precision numbers in the input array
      !>         ARRAY is not non-decreasing, the program will run to
      !>         completion but the index found will not mean anything.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Input,
      !>        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
      !>        sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB)
      pure integer(ip) function  lstled ( x, n, array )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: x  !! Upper bound value to search against.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      real(wp), intent(in) :: array ( * )  !! Array of possible lower bounds.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

      items = n

      begin = 1
      end   = n

      if      ( n <= 0            ) then

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         lstled = 0

      else if ( x < array(begin) ) then

!
!        None of the array elements are less than or equal to X
!
         lstled = 0

      else if ( x >= array(end)   ) then

!
!        X is greater than or equal to all elements of the array.  Thus
!        the last element of the array is the last item less than or
!        equal to X.
!
         lstled = end

      else

!
!        X lies between some pair of elements of the array
!

         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( array(middle) <= x ) then
               begin = middle
            else
               end   = middle
            end if

            items    = 1 + (end - begin)
         end do

         lstled = begin

      end if

      return
      end function lstled

      !> Find the index of the largest array element less than or equal
      !> to a given integer X in an array of non-decreasing integers.
      !>
      !> The function returns the index of the last element of ARRAY that
      !> is less than or equal to X.
      !>
      !>## Detailed Input
      !>
      !>     X        is an integer value acting as an upper bound: the element
      !>              of ARRAY that is the greatest element less than or equal
      !>              to X is to be found.
      !>
      !>     N        is the total number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of integers that forms a non-decreasing
      !>              sequence. The elements of array need not be distinct.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the highest-indexed element in
      !>     the input array that is less than or equal to X. The routine
      !>     assumes the array elements are sorted in non-decreasing order.
      !>
      !>     Indices range from 1 to N.
      !>
      !>     If all elements of ARRAY are greater than X, the routine returns
      !>     the value 0. If N is less than or equal to zero, the routine
      !>     returns the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than or equal to zero, the function returns 0.
      !>         This case is not treated as an error.
      !>
      !>     2)  If the input array is not sorted in non-decreasing order, the
      !>         output of this routine is undefined. No error is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses a binary search algorithm and so requires
      !>     at most on the order of
      !>
      !>        log (N)
      !>           2
      !>
      !>     steps to compute the value of LSTLEI.
      !>
      !>     Note: If you need to find the first element of the array that is
      !>     greater than X, simply add 1 to the result returned by this
      !>     function and check to see if the result is within the array bounds
      !>     given by N.
      !>
      !>## Examples
      !>
      !>     Suppose that you have an reasonably large ordered array of
      !>     integers, into which you want to insert a few more without
      !>     destroying the ordering.
      !>
      !>     Depending upon your application, it may be desirable to
      !>     not insert duplicates, to insert duplicates before
      !>     existing entries or to insert them after existing entries.
      !>
      !>     The code fragment below, illustrates an insertion scheme
      !>     that will insert duplicate items after existing items
      !>     and simultaneously update a second parallel array of
      !>     double precision numbers.
      !>
      !>           get the pair to insert
      !>
      !>           READ (*,*) KEY, VALUE
      !>
      !>           locate the place to insert the new KEY into the sorted
      !>           array of keys.
      !>
      !>           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1
      !>
      !>           insert the key and its associated value into the
      !>           KEYS and  VALUES arrays at location LOC
      !>
      !>           CALL INSLAI ( KEY,   1, LOC, NKEYS, KEYS   )
      !>           CALL INSLAD ( VALUE, 1, LOC, NVALS, VALUES )
      !>
      !>     If at the READ statement the arrays KEYS and VALUES looked like:
      !>
      !>           KEYS     VALUES     NKEYS = 6, NVALS = 6
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7       3.14D0
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     and 9 and 33.33D3 were read into KEY and VALUE respectively
      !>     then LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would be 4.
      !>     After the calls to the routines INSLAI and INSLAD we would have
      !>
      !>           KEYS     VALUES     NKEYS = 7, NVALS = 7
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7       3.14D0
      !>             9      33.33D3     <===== inserted items.
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     If 7 and 33.33D3 were read into KEY and VALUE respectively
      !>     then again LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would
      !>     be 4. After the calls to the routines INSLAI and INSLAD we
      !>     would have:
      !>
      !>           KEYS     VALUES     NKEYS = 7, NVALS = 7
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7       3.14D0
      !>             7      33.33D3     <===== inserted items.
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     If we replaced the line of code
      !>
      !>           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1
      !>     by
      !>
      !>           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1
      !>
      !>     we would obtain a routine that inserted duplicates before
      !>     existing entries. (LSTLTI is similar to LSTLEI except it finds
      !>     the last occurrence of an integer strictly less than a value.)
      !>     Using 7 and 33.33D3 for KEY and VALUE again, the modified code
      !>     fragment would yield the results shown below.
      !>
      !>           KEYS     VALUES     NKEYS = 7, NVALS = 7
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7      33.33D3     <===== inserted items.
      !>             7       3.14D0
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     (Note: you should NOT use the
      !>     code outlined above as the basis of a sorting algorithm.
      !>     The NAIF routines SHELLI, SHELLD, SHELLC, ORDERI, ORDERD, ORDERC,
      !>     REORDI, REORDD and REORDC are much more efficient routines for
      !>     sorting arrays or sorting a set of parallel arrays using
      !>     one of the set as a key. The fragment presented here is useful
      !>     for performing update insertions into previously ordered arrays.)
      !>
      !>     For more ideas regarding the use of this routine, see LSTLEC
      !>     and LSTLTC.
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence of integer numbers in the input array ARRAY is
      !>         not non-decreasing, the program will run to completion but the
      !>         index found will not mean anything.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Input,
      !>        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
      !>        sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
      pure integer(ip) function  lstlei ( x, n, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: x  !! Upper bound value to search against.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      integer(ip), intent(in) :: array ( * )  !! Array of possible lower bounds.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

      items = n

      begin = 1
      end   = n

      if      ( n <= 0            ) then

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         lstlei = 0

      else if ( x < array(begin) ) then

!
!        None of the array elements are less than or equal to X
!
         lstlei = 0

      else if ( x >= array(end)   ) then

!
!        X is greater than or equal to all elements of the array.  Thus
!        the last element of the array is the last item less than or
!        equal to X.
!
         lstlei = end

      else

!
!        X lies between some pair of elements of the array
!

         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( array(middle) <= x ) then
               begin = middle
            else
               end   = middle
            end if

            items    = 1 + (end - begin)
         end do

         lstlei = begin

      end if

      return
      end function lstlei

      !> Find the index of the largest array element less than a given
      !> character string in an ordered array of character strings.
      !>
      !> The function returns the index of the last element of ARRAY that
      !> is lexically less than STRING.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a string acting as an upper bound: the element of
      !>              ARRAY that is lexically the greatest element less than
      !>              STRING is to be found. Trailing blanks in this bound
      !>              value are not significant.
      !>
      !>     N        is the total number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of character strings to be searched. Trailing
      !>              blanks in the strings in this array are not significant.
      !>              The strings in ARRAY must be sorted in non-decreasing
      !>              order. The elements of ARRAY need not be distinct.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the highest-indexed element in
      !>     the input array that is lexically less than STRING. The routine
      !>     assumes the array elements are sorted in non-decreasing order.
      !>
      !>     Indices range from 1 to N.
      !>
      !>     If all elements of ARRAY are lexically greater than or equal to
      !>     STRING, the routine returns the value 0. If N is less than or
      !>     equal to zero, the routine returns the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than or equal to zero, the function returns 0.
      !>         This case is not treated as an error.
      !>
      !>     2)  If the input array is not sorted in non-decreasing order, the
      !>         output of this routine is undefined. No error is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses a binary search algorithm and so requires
      !>     at most on the order of
      !>
      !>        log (N)
      !>           2
      !>
      !>     steps to compute the value of LSTLTC.
      !>
      !>     Note: If you need to find the first element of the array that is
      !>     lexically greater than or equal to STRING, simply add 1 to the
      !>     result returned by this function and check to see if the result is
      !>     within the array bounds given by N.
      !>
      !>## Examples
      !>
      !>     Suppose that you have a long list of words, sorted alphabetically
      !>     and entirely in upper case. Furthermore suppose you wished to
      !>     find all words that begin the sequence of letters PLA,  then
      !>     you could execute the following code.
      !>
      !>           START = 0
      !>           I     = 1
      !>
      !>           DO I = 1, NWORDS
      !>
      !>              IF ( WORD(I)(1:3) .EQ. 'PLA' ) THEN
      !>
      !>                 IF ( START .EQ. 0 ) THEN
      !>                    START = I
      !>                 END IF
      !>
      !>                 END = I
      !>              END IF
      !>
      !>           END DO
      !>
      !>     This can of course be improved by stopping the loop once START
      !>     is non-zero and END remains unchanged after a pass through the
      !>     loop. However, this is a linear search  and on average can be
      !>     expected to take NWORDS/2 comparisons. The above algorithm
      !>     fails to take advantage of the structure of the list of words
      !>     (they are sorted).
      !>
      !>     The code below is much simpler to code, simpler to check, and
      !>     much faster than the code above.
      !>
      !>           START = LSTLTC( 'PLA', NWORDS, WORDS ) + 1
      !>           END   = LSTLTC( 'PLB', NWORDS, WORDS )
      !>
      !>           do something in case there are no such words.
      !>
      !>           IF ( START .GT. END ) THEN
      !>              START = 0
      !>              END   = 0
      !>           END IF
      !>
      !>     This code will never exceed 2 * LOG_2 ( NWORDS ) comparisons.
      !>     For a large list of words (say 4096) the second method will
      !>     take 24 comparisons  the first method requires on average
      !>     2048 comparisons. About 200 times as much time. Its clear
      !>     that if searches such as this must be performed often, that
      !>     the second approach could make the difference between being
      !>     able to perform the task in a few minutes as opposed to
      !>     several hours.
      !>
      !>     For more ideas regarding the use of this routine see LSTLEI
      !>     and LSTLTI.
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence of character strings in the input array ARRAY
      !>         is not non-decreasing, the program will run to completion but
      !>         the index found will not mean anything.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Input,
      !>        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
      !>        sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
      pure integer(ip) function  lstltc ( string, n, array )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: string  !! Upper bound value to search against.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      character(*), intent(in) :: array ( * )  !! Array of possible lower bounds.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

      items = n

      begin = 1
      end   = n

      if      ( n <= 0            ) then

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return
!
         lstltc = 0

      else if ( lle(string, array(begin)) ) then

!
!        None of the array elements are less than STRING
!
         lstltc = 0

      else if ( llt(array(end), string) ) then

!
!        STRING is greater than all elements of the array.  Thus the las
!        element of the array is the last item less than STRING.
!
         lstltc = end

      else

!
!        STRING lies between some pair of elements of the array
!

         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( llt(array(middle), string) ) then
               begin = middle
            else
               end   = middle
            end if

            items    = 1 + (end - begin)
         end do

         lstltc = begin

      end if

      return
      end function lstltc

       !> Find the index of the largest array element less than
       !> a given number X in an array of non-decreasing numbers.
       !>
       !> The function returns the index of the last element of ARRAY that
       !> is less than X.
       !>
       !>## Detailed Input
       !>
       !>     X        is a double precision value acting as an upper bound: the
       !>              element of ARRAY that is the greatest element less than X
       !>              is to be found.
       !>
       !>     N        is the total number of elements in ARRAY.
       !>
       !>     ARRAY    is an array of double precision numbers that forms a
       !>              non-decreasing sequence. The elements of array need not
       !>              be distinct.
       !>
       !>## Detailed Output
       !>
       !>     The function returns the index of the highest-indexed element in
       !>     the input array that is less than X. The routine assumes the array
       !>     elements are sorted in non-decreasing order.
       !>
       !>     Indices range from 1 to N.
       !>
       !>     If all elements of ARRAY are greater than or equal to X, the
       !>     routine returns the value 0. If N is less than or equal to zero,
       !>     the routine returns the value 0.
       !>
       !>## Parameters
       !>
       !>     None.
       !>
       !>## Exceptions
       !>
       !>     Error free.
       !>
       !>     1)  If N is less than or equal to zero, the function returns 0.
       !>         This case is not treated as an error.
       !>
       !>     2)  If the input array is not sorted in non-decreasing order, the
       !>         output of this routine is undefined. No error is signaled.
       !>
       !>## Files
       !>
       !>     None.
       !>
       !>## Particulars
       !>
       !>     This routine uses a binary search algorithm and so requires
       !>     at most on the order of
       !>
       !>        log (N)
       !>           2
       !>
       !>     steps to compute the value of LSTLTD.
       !>
       !>     Note: If you need to find the first element of the array that is
       !>     greater than or equal to X, simply add 1 to the result returned by
       !>     this function and check to see if the result is within the array
       !>     bounds given by N.
       !>
       !>## Examples
       !>
       !>     If ARRAY(I) = -1 + 4*I/3 (real arithmetic implied here)
       !>
       !>        N        = 10
       !>        X        = 7.12
       !>
       !>     then
       !>
       !>        LSTLTD will be I where
       !>
       !>             (4*I/3) - 1       <      7.12
       !>
       !>     but
       !>
       !>             (4*(I+1)/3) - 1   > or = 7.12 .
       !>
       !>     In this case our subsequence is:
       !>
       !>            1/3, 5/3, 9/3, 13/3, 17/3, 21/3, 25/3, .... 37/3
       !>
       !>     index:  1    2    3    4     5     6     7    ....  10
       !>
       !>     Thus LSTLTD will be returned as 6
       !>
       !>     The following table shows the values of LSTLTD that would be
       !>     returned for various values of X
       !>
       !>            X       LSTLTD
       !>          -----     -------
       !>           0.12        0
       !>           1.34        1
       !>           5.13        4
       !>           8.00        6
       !>          15.10       10
       !>
       !>## Restrictions
       !>
       !>     1)  If the sequence of double precision numbers in the input array
       !>         ARRAY is not non-decreasing, the program will run to
       !>         completion but the index found will not mean anything.
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
       !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
       !>
       !>        Added IMPLICIT NONE statement.
       !>
       !>        Edited the header to comply with NAIF standard. Removed
       !>        unnecessary $Revisions section. Improved $Detailed_Input,
       !>        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
       !>        sections.
       !>
       !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
       !>
       !>        Comment section for permuted index source lines was added
       !>        following the header.
       !>
       !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB)
       pure integer(ip) function  lstltd ( x, n, array )
       use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: x  !! Upper bound value to search against.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      real(wp), intent(in) :: array ( * )  !! Array of possible lower bounds.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

      items = n

      begin = 1
      end   = n

      if      ( n <= 0            ) then

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return
!
         lstltd = 0

      else if ( x <= array(begin) ) then

!
!        None of the array elements are less than X
!
         lstltd = 0

      else if ( array(end) < x ) then

!
!        X is greater than all elements of the array.  Thus the last
!        element of the array is the last item less than X.
!
         lstltd = end

      else

!
!        X lies between some pair of elements of the array
!

         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( array(middle) < x ) then
               begin = middle
            else
               end   = middle
            end if

            items    = 1 + (end - begin)
         end do

         lstltd = begin

      end if

      return
      end function lstltd

      !> Find the index of the largest array element less than
      !> a given integer X in an array of non-decreasing integers.
      !>
      !> The function returns the index of the last element of ARRAY that
      !> is less than X.
      !>
      !>## Detailed Input
      !>
      !>     X        is an integer value acting as an upper bound: the element
      !>              of ARRAY that is the greatest element less than X is to
      !>              be found.
      !>
      !>     N        is the total number of elements in ARRAY.
      !>
      !>     ARRAY    is an array of integers that forms a non-decreasing
      !>              sequence. The elements of array need not be distinct.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the index of the highest-indexed element in
      !>     the input array that is less than X. The routine assumes the array
      !>     elements are sorted in non-decreasing order.
      !>
      !>     Indices range from 1 to N.
      !>
      !>     If all elements of ARRAY are greater than or equal to X, the
      !>     routine returns the value 0. If N is less than or equal to zero,
      !>     the routine returns the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than or equal to zero, the function returns 0.
      !>         This case is not treated as an error.
      !>
      !>     2)  If the input array is not sorted in non-decreasing order, the
      !>         output of this routine is undefined. No error is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses a binary search algorithm and so requires
      !>     at most on the order of
      !>
      !>        log (N)
      !>           2
      !>
      !>     steps to compute the value of LSTLTI.
      !>
      !>     Note: If you need to find the first element of the array that is
      !>     greater than or equal to X, simply add 1 to the result returned by
      !>     this function and check to see if the result is within the array
      !>     bounds given by N.
      !>
      !>## Examples
      !>
      !>     Suppose that you have an reasonably large ordered array of
      !>     integers, into which you want to insert a few more without
      !>     destroying the ordering.
      !>
      !>     Depending upon your application, it may be desirable to
      !>     not insert duplicates, to insert duplicates before
      !>     existing entries or to insert them after existing entries.
      !>
      !>     The code fragment below, illustrates an insertion scheme
      !>     that will insert duplicate items before existing items
      !>     and simultaneously update a second parallel array of
      !>     double precision numbers.
      !>
      !>           get the pair to insert
      !>
      !>           READ (*,*) KEY, VALUE
      !>
      !>           locate the place to insert the new KEY into the sorted
      !>           array of keys.
      !>
      !>           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1
      !>
      !>           insert the key and its associated value into the
      !>           KEYS and  VALUES arrays at location LOC
      !>
      !>           CALL INSLAI ( KEY,   1, LOC, NKEYS, KEYS   )
      !>           CALL INSLAD ( VALUE, 1, LOC, NVALS, VALUES )
      !>
      !>     If at the READ statement the arrays KEYS and VALUES looked like:
      !>
      !>           KEYS     VALUES     NKEYS = 6, NVALS = 6
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7       3.14D0
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     and 9 and 33.33D3 were read into KEY and VALUE respectively
      !>     then LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would be 4.
      !>     After the calls to the routines INSLAI and INSLAD we would have
      !>
      !>           KEYS     VALUES     NKEYS = 7, NVALS = 7
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7       3.14D0
      !>             9      33.33D3     <===== inserted items.
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     If 7 and 33.33D3 were read into KEY and VALUE respectively
      !>     then again LSTLEI (KEY, NKEYS, KEYS ) would be 2 and LOC would
      !>     be 3. After the calls to the routines INSLAI and INSLAD we
      !>     would have:
      !>
      !>           KEYS     VALUES     NKEYS = 7, NVALS = 7
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7      33.33D3     <===== inserted items.
      !>             7       3.14D0
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     If we replaced the line of code
      !>
      !>           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1
      !>     by
      !>
      !>           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1
      !>
      !>     we would obtain a routine that inserted duplicates before
      !>     existing entries. (LSTLEI is similar to LSTLTI except it finds
      !>     the last occurrence of an integer less than or equal to a value.)
      !>     Using 7 and 33.33D3 for KEY and VALUE again, the modified code
      !>     fragment would yield the results shown below.
      !>
      !>           KEYS     VALUES     NKEYS = 7, NVALS = 7
      !>           ----     -------
      !>             2       3.00D0
      !>             5       1.00D0
      !>             7       3.14D0
      !>             7      33.33D3     <===== inserted items.
      !>            16       7.11D0
      !>            18       2.14D0
      !>            23      12.12D0
      !>
      !>     Note: you should NOT use the code outlined above as the basis of
      !>     a sorting algorithm. The SPICELIB routines SHELLI, SHELLD, SHELLC,
      !>     ORDERI, ORDERD, ORDERC, REORDI, REORDD and REORDC are much more
      !>     efficient routines for sorting arrays or sorting a set of
      !>     parallel arrays using one of the set as a key. The fragment
      !>     presented here is useful for performing update insertions into
      !>     previously ordered arrays.
      !>
      !>     For more ideas regarding the use of this routine, see LSTLTC
      !>     and LSTLTC.
      !>
      !>## Restrictions
      !>
      !>     1)  If the sequence of integer numbers in the input array ARRAY is
      !>         not non-decreasing, the program will run to completion but the
      !>         index found will not mean anything.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Detailed_Input,
      !>        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
      !>        sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
      pure integer(ip) function  lstlti ( x, n, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: x  !! Upper bound value to search against.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.
      integer(ip), intent(in) :: array ( * )  !! Array of possible lower bounds.

      integer(ip) :: j

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: middle
      integer(ip) :: items

      items = n

      begin = 1
      end   = n

      if      ( n <= 0            ) then

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return
!
         lstlti = 0

      else if ( x <= array(begin) ) then

!
!        None of the array elements are less than X
!
         lstlti = 0

      else if ( array(end) < x ) then

!
!        X is greater than all elements of the array.  Thus the last
!        element of the array is the last item less than X.
!
         lstlti = end

      else

!
!        X lies between some pair of elements of the array
!

         do while ( items > 2 )

            j        = items/2
            middle   = begin + j

            if ( array(middle) < x ) then
               begin = middle
            else
               end   = middle
            end if

            items    = 1 + (end - begin)
         end do

         lstlti = begin

      end if

      return
      end function lstlti

      !> Locate the maximum element of a character array.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an arbitrary array.
      !>
      !>     NDIM     is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     MAXVAL   is the value in array that is greater than or equal
      !>              to all other values in the array. If the array
      !>              contains more than one element with this value,
      !>              the first one is returned.
      !>
      !>              Elements in character arrays are compared according
      !>              to the ASCII collating sequence.
      !>
      !>     LOC      is the location of the maximum element. That is,
      !>              MAXVAL contains element ARRAY(LOC).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the array is empty (NDIM is less than one), LOC is zero,
      !>         and MAXVAL is not changed.
      !>
      !>     2)  If the declared length of MAXVAL is too short to contain the
      !>         entire element, the element is truncated. (The original value
      !>         can be accessed via LOC.)
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
      !>     Let array A contain the following elements.
      !>
      !>        A(1) = 'Einstein'
      !>        A(2) = 'Bohr'
      !>        A(3) = 'Feynman'
      !>        A(4) = 'Pauli'
      !>        A(5) = 'Bardeen'
      !>        A(6) = 'Dirac'
      !>
      !>     Then following the call
      !>
      !>        CALL MAXAC ( A, 6, MAXVAL, LOC )
      !>
      !>     the values of MAXVAL and LOC are 'Pauli' and 4 respectively.
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
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
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
      pure subroutine maxac ( array, ndim, maxval, loc )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: array ( * )  !! Array.
      integer(ip), intent(in) :: ndim  !! Number of elements in ARRAY.
      character(*), intent(out) :: maxval  !! Maximum value in ARRAY.
      integer(ip), intent(out) :: loc  !! Location of MAXVAL in ARRAY.

      integer(ip) :: i

      if (ndim <= 0) then
         loc = 0
         return
      end if

      maxval = array(1)
      loc = 1

      do i = 2, ndim
         if (lgt (array(i), maxval)) then
            maxval = array(i)
            loc = i
         end if
      end do

      return
      end subroutine maxac

      !> Locate the maximum element of a DP array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MAXVAL and MAXLOC instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an arbitrary array.
      !>
      !>     NDIM     is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     MAXVAL   is the value in array that is greater than or equal
      !>              to all other values in the array. If the array
      !>              contains more than one element with this value,
      !>              the first one is returned.
      !>
      !>              Elements in character arrays are compared according
      !>              to the ASCII collating sequence.
      !>
      !>     LOC      is the location of the maximum element. That is,
      !>              MAXVAL contains element ARRAY(LOC).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the array is empty (NDIM is less than one), LOC is zero,
      !>         and MAXVAL is not changed.
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
      !>     Let array A contain the following elements.
      !>
      !>        A(1) = 16.D0
      !>        A(2) =  4.D0
      !>        A(3) = 32.D0
      !>        A(4) = 64.D0
      !>        A(5) =  2.D0
      !>        A(6) =  8.D0
      !>
      !>     Then following the call
      !>
      !>        CALL MAXAD ( A, 6, MAXVAL, LOC )
      !>
      !>     the values of MAXVAL and LOC are 64.D0 and 4 respectively.
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
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
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
      pure subroutine maxad ( array, ndim, maxval, loc )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: array ( * )  !! Array.
      integer(ip), intent(in) :: ndim  !! Number of elements in ARRAY.
      real(wp), intent(out) :: maxval  !! Maximum value in ARRAY.
      integer(ip), intent(out) :: loc  !! Location of MAXVAL in ARRAY.

      if ( ndim <= 0 ) then
         loc = 0
         return
      end if
      loc = maxloc(array(1:ndim), dim=1)
      maxval = array(loc)
      end subroutine maxad

      !> Locate the maximum element of an integer array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MAXVAL and MAXLOC instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an arbitrary array.
      !>
      !>     NDIM     is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     MAXVAL   is the value in array that is greater than or equal
      !>              to all other values in the array. If the array
      !>              contains more than one element with this value,
      !>              the first one is returned.
      !>
      !>              Elements in character arrays are compared according
      !>              to the ASCII collating sequence.
      !>
      !>     LOC      is the location of the maximum element. That is,
      !>              MAXVAL contains element ARRAY(LOC).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the array is empty (NDIM is less than one), LOC is zero,
      !>         and MAXVAL is not changed.
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
      !>     Let array A contain the following elements.
      !>
      !>        A(1) = 16
      !>        A(2) =  4
      !>        A(3) = 32
      !>        A(4) = 64
      !>        A(5) =  2
      !>        A(6) =  8
      !>
      !>     Then following the call
      !>
      !>        CALL MAXAI ( A, 6, MAXVAL, LOC )
      !>
      !>     the values of MAXVAL and LOC are 64 and 4 respectively.
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
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
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
      pure subroutine maxai ( array, ndim, maxval, loc )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: array ( * )  !! Array.
      integer(ip), intent(in) :: ndim  !! Number of elements in ARRAY.
      integer(ip), intent(out) :: maxval  !! Maximum value in ARRAY.
      integer(ip), intent(out) :: loc  !! Location of MAXVAL in ARRAY.

      if ( ndim <= 0 ) then
         loc = 0
         return
      end if
      loc = maxloc(array(1:ndim), dim=1)
      maxval = array(loc)
      end subroutine maxai

      !> Locate the minimum element of a character array.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an arbitrary array.
      !>
      !>     NDIM     is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     MINVAL   is the value in array that is less than or equal
      !>              to all other values in the array. If the array
      !>              contains more than one element with this value,
      !>              the first one is returned.
      !>
      !>              Elements in character arrays are compared according
      !>              to the ASCII collating sequence.
      !>
      !>     LOC      is the location of the minimum element. That is,
      !>              MINVAL contains element ARRAY(LOC).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the array is empty (NDIM is less than one), LOC is zero,
      !>         and MINVAL is not changed.
      !>
      !>     2)  If the declared length of MINVAL is too short to contain the
      !>         entire element, the element is truncated. (The original value
      !>         can be accessed via LOC.)
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
      !>     Let array A contain the following elements.
      !>
      !>        A(1) = 'Einstein'
      !>        A(2) = 'Bohr'
      !>        A(3) = 'Feynman'
      !>        A(4) = 'Pauli'
      !>        A(5) = 'Bardeen'
      !>        A(6) = 'Dirac'
      !>
      !>     Then following the call
      !>
      !>        CALL MINAC ( A, 6, MINVAL, LOC )
      !>
      !>     the values of MINVAL and LOC are 'Bardeen' and 5 respectively.
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
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
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
      pure subroutine minac ( array, ndim, minval, loc )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: array ( * )  !! Array.
      integer(ip), intent(in) :: ndim  !! Number of elements in ARRAY.
      character(*), intent(out) :: minval  !! Minimum value in ARRAY.
      integer(ip), intent(out) :: loc  !! Location of MINVAL in ARRAY.

      integer(ip) :: i

      if (ndim <= 0) then
         loc = 0
         return
      end if

      minval = array(1)
      loc = 1

      do i = 2, ndim
         if (llt (array(i), minval)) then
            minval = array(i)
            loc = i
         end if
      end do

      return
      end subroutine minac

      !> Locate the minimum element of a DP array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MINVAL and MINLOC instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an arbitrary array.
      !>
      !>     NDIM     is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     MINVAL   is the value in array that is less than or equal
      !>              to all other values in the array. If the array
      !>              contains more than one element with this value,
      !>              the first one is returned.
      !>
      !>     LOC      is the location of the minimum element. That is,
      !>              MINVAL contains element ARRAY(LOC).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the array is empty (NDIM is less than one), LOC is zero,
      !>         and MINVAL is not changed.
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
      !>     Let array A contain the following elements.
      !>
      !>        A(1) = 16.D0
      !>        A(2) =  4.D0
      !>        A(3) = 32.D0
      !>        A(4) = 64.D0
      !>        A(5) =  2.D0
      !>        A(6) =  8.D0
      !>
      !>     Then following the call
      !>
      !>        CALL MINAD ( A, 6, MINVAL, LOC )
      !>
      !>     the values of MINVAL and LOC are 2.D0 and 5 respectively.
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
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
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
      pure subroutine minad ( array, ndim, minval, loc )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: array ( * )  !! Array.
      integer(ip), intent(in) :: ndim  !! Number of elements in ARRAY.
      real(wp), intent(out) :: minval  !! Minimum value in ARRAY.
      integer(ip), intent(out) :: loc  !! Location of MINVAL in ARRAY.

      if ( ndim <= 0 ) then
         loc = 0
         return
      end if
      loc = minloc(array(1:ndim), dim=1)
      minval = array(loc)
      end subroutine minad

      !> Locate the minimum element of an integer array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MINVAL and MINLOC instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is an arbitrary array.
      !>
      !>     NDIM     is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     MINVAL   is the value in array that is less than or equal
      !>              to all other values in the array. If the array
      !>              contains more than one element with this value,
      !>              the first one is returned.
      !>
      !>     LOC      is the location of the minimum element. That is,
      !>              MINVAL contains element ARRAY(LOC).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the array is empty (NDIM is less than one), LOC is zero,
      !>         and MINVAL is not changed.
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
      !>     Let array A contain the following elements.
      !>
      !>        A(1) = 16
      !>        A(2) =  4
      !>        A(3) = 32
      !>        A(4) = 64
      !>        A(5) =  2
      !>        A(6) =  8
      !>
      !>     Then following the call
      !>
      !>        CALL MINAI ( A, 6, MINVAL, LOC )
      !>
      !>     the values of MINVAL and LOC are 2 and 5 respectively.
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
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
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
      pure subroutine minai ( array, ndim, minval, loc )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: array ( * )  !! Array.
      integer(ip), intent(in) :: ndim  !! Number of elements in ARRAY.
      integer(ip), intent(out) :: minval  !! Minimum value in ARRAY.
      integer(ip), intent(out) :: loc  !! Location of MINVAL in ARRAY.

      if ( ndim <= 0 ) then
         loc = 0
         return
      end if
      loc = minloc(array(1:ndim), dim=1)
      minval = array(loc)
      end subroutine minai

      !> Copy the elements of one character array into another
      !> array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRFRM   is an array from which to copy items.
      !>
      !>     NDIM     is the number of items to copy.
      !>
      !>## Detailed Output
      !>
      !>     ARRTO    is the array to which items should be copied.
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
      !>     This routine is simply shorthand for the following 3 lines of
      !>     code.
      !>
      !>             DO I = 1, NDIM
      !>                ARRTO(I) = ARRFRM(I)
      !>             END DO
      !>
      !>## Examples
      !>
      !>     Often one needs to make a temporary copy of an array so that
      !>     it can be manipulated without altering the original array.
      !>     As pointed out in particulars, you could just do this within
      !>     the code that needs the copy. However, if you have several
      !>     arrays to copy, you can cut the number of lines of code that
      !>     are needed by a third.
      !>
      !>     For example:
      !>
      !>          DO I = 1, 19
      !>             TEMPA(I) = A(I)
      !>          END DO
      !>
      !>          DO I = 1, 38
      !>             TEMPB(I) = B(I)
      !>          END DO
      !>
      !>     Can be rewritten as
      !>
      !>          CALL MOVEC ( A, 19, TEMPA )
      !>          CALL MOVEC ( B, 38, TEMPB )
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
      !>     W.M. Owen          (JPL)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.0.1, 4-FEB-1989 (WLT)
      !>
      !>      Header fully filled out.
      pure subroutine movec ( arrfrm, ndim, arrto )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: arrfrm ( * )  !! Character array to be moved.
      integer(ip), intent(in) :: ndim  !! Number of elements to copy, i.e. the dimension of ARRFRM
                                       !! and ARRTO.
      character(*), intent(out) :: arrto  ( * )  !! Destination array.

      arrto(1:ndim) = arrfrm(1:ndim)
      end subroutine movec

      !> Copy the elements of one double precision array into another
      !> array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRFRM   is an array from which to copy items.
      !>
      !>     NDIM     is the number of items to copy. i.e. the dimension of
      !>              ARRFRM and ARRTO.
      !>
      !>## Detailed Output
      !>
      !>     ARRTO    is the array to which items should be copied.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 1, the output array is returned unchanged.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is simply shorthand for the following 3 lines of
      !>     code.
      !>
      !>        DO I = 1, NDIM
      !>           ARRTO(I) = ARRFRM(I)
      !>        END DO
      !>
      !>## Examples
      !>
      !>     Often one needs to make a temporary copy of an array so that
      !>     it can be manipulated without altering the original array.
      !>     As pointed out in particulars, you could just do this within
      !>     the code that needs the copy. However, if you have several
      !>     arrays to copy, you can cut the number of lines of code that
      !>     are needed by a third.
      !>
      !>     For example:
      !>
      !>        DO I = 1, 19
      !>           TEMPA(I) = A(I)
      !>        END DO
      !>
      !>        DO I = 1, 38
      !>           TEMPB(I) = B(I)
      !>        END DO
      !>
      !>     Can be rewritten as
      !>
      !>        CALL MOVED ( A, 19, TEMPA )
      !>        CALL MOVED ( B, 38, TEMPB )
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
      !>     W.M. Owen          (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.17.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 2.16.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.7.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.5.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 2.1.1, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 2.1.0, 09-NOV-2001 (NJB)
      !>
      !>        Bug fix: code for PC-LINUX and PC-DIGITAL environments
      !>        was changed to prevent corruption of bit patterns inserted
      !>        into double precision numbers via EQUIVALENCE statements.
      !>        Now the routine memmove is called to effect the double
      !>        precision assignments performed by this routine.
      !>
      !>-    SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
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
      !>-    SPICELIB Version 2.0.0, 08-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform. Added
      !>        Copyright section to header.
      !>
      !>-    SPICELIB Version 1.1.0, 21-FEB-1996 (KRG)
      !>
      !>        This subroutine was turned into a machine dependent master
      !>        file due to problems with its use with arrays containing
      !>        INTEGERs that had been EQUIVALENCEd to DOUBLE PRECISION
      !>        numbers on the NeXT with the Absoft Fortran compiler, V3.2.
      !>
      !>        The DO loop has been replaced with a call to the C memmove
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine moved ( arrfrm, ndim, arrto )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: arrfrm ( * )  !! Double precision array to be moved.
      integer(ip), intent(in) :: ndim  !! Number of elements to copy.
      real(wp), intent(out) :: arrto  ( * )  !! Destination array.

!
!     PEPPER: copy directly. SPICELIB called MOVEI with 2*NDIM integers,
!     which needs an implicit interface (type mismatch).
!
      arrto(1:ndim) = arrfrm(1:ndim)
      end subroutine moved

      !> Copy the elements of one integer array into another
      !> array.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRFRM   is an array from which to copy items.
      !>
      !>     NDIM     is the number of items to copy.
      !>
      !>## Detailed Output
      !>
      !>     ARRTO    is the array to which items should be copied.
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
      !>     This routine is simply shorthand for the following 3 lines of
      !>     code.
      !>
      !>             DO I = 1, NDIM
      !>                ARRTO(I) = ARRFRM(I)
      !>             END DO
      !>
      !>## Examples
      !>
      !>     Often one needs to make a temporary copy of an array so that
      !>     it can be manipulated without altering the original array.
      !>     As pointed out in particulars, you could just do this within
      !>     the code that needs the copy. However, if you have several
      !>     arrays to copy, you can cut the number of lines of code that
      !>     are needed by a third.
      !>
      !>     For example:
      !>
      !>          DO I = 1, 19
      !>             TEMPA(I) = A(I)
      !>          END DO
      !>
      !>          DO I = 1, 38
      !>             TEMPB(I) = B(I)
      !>          END DO
      !>
      !>     Can be rewritten as
      !>
      !>          CALL MOVEI ( A, 19, TEMPA )
      !>          CALL MOVEI ( B, 38, TEMPB )
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
      !>     W.M. Owen          (JPL)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.0.1, 4-FEB-1989 (WLT)
      !>
      !>      Header fully filled out.
      pure subroutine movei ( arrfrm, ndim, arrto )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: arrfrm ( * )  !! Integer array to be moved.
      integer(ip), intent(in) :: ndim  !! Number of elements to copy, i.e. the dimension of ARRFRM
                                       !! and ARRTO.
      integer(ip), intent(out) :: arrto  ( * )  !! Destination array.

      arrto(1:ndim) = arrfrm(1:ndim)
      end subroutine movei

      !> Determine if none the entries in an array of logicals are .TRUE.
      !>
      !> The function returns .TRUE. if no entry has a value of .TRUE.
      !>
      !>## Detailed Input
      !>
      !>     LOGCLS   is an array of logicals.
      !>
      !>     N        is the number of elements in the array LOGCLS
      !>
      !>## Detailed Output
      !>
      !>     The function returns true if no entry of LOGCLS is .TRUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than 1, the function returns a value of .TRUE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function examines each element of LOGCLS until
      !>     a .TRUE. value is found or until all values have been
      !>     examined.
      !>
      !>## Examples
      !>
      !>     Suppose you needed to confirm that no entry of a character set
      !>     WORDS was one of the words in the phrase
      !>
      !>       'EVERY GOOD BOY DOES FINE'
      !>
      !>     You might execute the following block of code.
      !>
      !>           FOUND(1) = ELEMC  ( 'EVERY', WORDS )
      !>           FOUND(2) = ELEMC  ( 'GOOD',  WORDS )
      !>           FOUND(3) = ELEMC  ( 'BOY',   WORDS )
      !>           FOUND(4) = ELEMC  ( 'DOES',  WORDS )
      !>           FOUND(5) = ELEMC  ( 'FINE',  WORDS )
      !>
      !>           OK       = NOTRU  ( FOUND,   5     )
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
      !>-    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT)
      pure logical function notru ( logcls, n )
      use pepper_kinds, only: ip
      implicit none

      logical, intent(in) :: logcls ( * )  !! An array of logicals.
      integer(ip), intent(in) :: n  !! Number of elements in the array LOGCLS.

      integer(ip) :: i

!
!     Just do it.
!

      do i = 1, n

         if ( logcls(i) ) then

            notru = .false.
            return

         end if

      end do

      notru = .true.
      return

      end function notru

      !> Determine the order of elements in an array of character strings.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     IORDER   is the order vector for the input array.
      !>              IORDER(1) is the index of the smallest element
      !>              of ARRAY; IORDER(2) is the index of the next
      !>              smallest; and so on. Strings are ordered according
      !>              to the ASCII collating sequence.
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
      !>     ORDERC finds the index of the smallest element of the input
      !>     array. This becomes the first element of the order vector.
      !>     The process is repeated for the rest of the elements.
      !>
      !>     The order vector returned by ORDERC may be used by any of
      !>     the REORD routines to sort sets of related arrays, as shown
      !>     in the example below.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Sort four related arrays (containing the names, masses,
      !>        integer ID codes, and flags indicating whether they have
      !>        a ring system, for a group of planets).
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM ORDERC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NDIM
      !>      PARAMETER             ( NDIM   = 8  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 7  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      NAMES  ( NDIM )
      !>
      !>      DOUBLE PRECISION        MASSES ( NDIM )
      !>
      !>      INTEGER                 CODES  ( NDIM )
      !>      INTEGER                 I
      !>      INTEGER                 IORDER ( NDIM )
      !>
      !>      LOGICAL                 RINGS  ( NDIM )
      !>
      !>C
      !>C     Set the arrays containing the names, masses (given as
      !>C     ratios to of Solar GM to barycenter GM), integer ID
      !>C     codes, and flags indicating whether they have a ring
      !>C     system.
      !>C
      !>      DATA                    NAMES  /
      !>     .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
      !>     .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
      !>
      !>      DATA                    MASSES /
      !>     .                       22032.080D0,   324858.599D0,
      !>     .                      398600.436D0,    42828.314D0,
      !>     .                   126712767.881D0, 37940626.068D0,
      !>     .                     5794559.128D0,  6836534.065D0 /
      !>
      !>      DATA                    CODES  / 199, 299, 399, 499,
      !>     .                                 599, 699, 799, 899 /
      !>
      !>      DATA                    RINGS  /
      !>     .                   .FALSE., .FALSE., .FALSE., .FALSE.,
      !>     .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
      !>
      !>C
      !>C     Sort the object arrays by name.
      !>C
      !>      CALL ORDERC ( NAMES,  NDIM, IORDER )
      !>
      !>      CALL REORDC ( IORDER, NDIM, NAMES  )
      !>      CALL REORDD ( IORDER, NDIM, MASSES )
      !>      CALL REORDI ( IORDER, NDIM, CODES  )
      !>      CALL REORDL ( IORDER, NDIM, RINGS  )
      !>
      !>C
      !>C     Output the resulting table.
      !>C
      !>      WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
      !>      WRITE(*,'(A)') '-------  -------------  -------  ------'
      !>
      !>      DO I = 1, NDIM
      !>
      !>         WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
      !>     .                              CODES(I), RINGS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planet   Mass(GMS/GM)  ID Code  Rings?
      !>        -------  -------------  -------  ------
      !>        EARTH       398600.436      399    F
      !>        JUPITER  126712767.881      599    T
      !>        MARS         42828.314      499    F
      !>        MERCURY      22032.080      199    F
      !>        NEPTUNE    6836534.065      899    T
      !>        SATURN    37940626.068      699    T
      !>        URANUS     5794559.128      799    T
      !>        VENUS       324858.599      299    F
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
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine orderc ( array, ndim, iorder )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(in) :: array  ( * )  !! Input array.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(out) :: iorder ( * )  !! Order vector for ARRAY.

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

!
!     Begin with the initial ordering.
!
      do i = 1, ndim
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = ndim / 2

      do while ( gap > 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j > 0 )
               jg = j + gap

               if ( lle ( array(iorder(j)), array(iorder(jg)) ) ) then
                  j = 0
               else
                  call swapi ( iorder(j), iorder(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine orderc

      !> Determine the order of elements in a double precision array.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     IORDER   is the order vector for the input array.
      !>              IORDER(1) is the index of the smallest element
      !>              of ARRAY; IORDER(2) is the index of the next
      !>              smallest; and so on.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  A negative input dimension causes this routine to leave the
      !>         output order vector unchanged.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ORDERD finds the index of the smallest element of the input
      !>     array. This becomes the first element of the order vector.
      !>     The process is repeated for the rest of the elements.
      !>
      !>     The order vector returned by ORDERD may be used by any of
      !>     the REORD routines to sort sets of related arrays, as shown
      !>     in the example below.
      !>
      !>## Examples
      !>
      !>     In the following example, the ORDER and REORD routines are
      !>     used to sort four related arrays (containing the names,
      !>     masses, integer ID codes, and visual magnitudes for a group
      !>     of satellites). This is representative of the typical use of
      !>     these routines.
      !>
      !>            C
      !>            C     Sort the object arrays by visual magnitude.
      !>            C
      !>                  CALL ORDERD ( VMAGS,  N, IORDER )
      !>
      !>                  CALL REORDC ( IORDER, N, NAMES )
      !>                  CALL REORDD ( IORDER, N, MASSES )
      !>                  CALL REORDI ( IORDER, N, CODES )
      !>                  CALL REORDR ( IORDER, N, VMAGS )
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
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 23-MAR-2010 (NJB)
      !>
      !>        Header example was updated to show use of this routine.
      !>        $Exceptions section was updated. Header sections were
      !>        re-ordered.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine orderd ( array, ndim, iorder )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: array  ( * )  !! Input array.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(out) :: iorder ( * )  !! Order vector for ARRAY.

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

!
!     Begin with the initial ordering.
!
      do i = 1, ndim
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = ndim / 2

      do while ( gap > 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j > 0 )
               jg = j + gap

               if ( array(iorder(j)) <= array(iorder(jg)) ) then
                  j = 0
               else
                  call swapi ( iorder(j), iorder(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine orderd

      !> Determine the order of elements in an integer array.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     IORDER   is the order vector for the input array.
      !>              IORDER(1) is the index of the smallest element
      !>              of ARRAY; IORDER(2) is the index of the next
      !>              smallest; and so on.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  A negative input dimension causes this routine to leave the
      !>         output order vector unchanged.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ORDERI finds the index of the smallest element of the input
      !>     array. This becomes the first element of the order vector.
      !>     The process is repeated for the rest of the elements.
      !>
      !>     The order vector returned by ORDERI may be used by any of
      !>     the REORD routines to sort sets of related arrays, as shown
      !>     in the example below.
      !>
      !>## Examples
      !>
      !>     In the following example, the ORDER and REORD routines are
      !>     used to sort four related arrays (containing the names,
      !>     masses, integer ID codes, and visual magnitudes for a group
      !>     of satellites). This is representative of the typical use of
      !>     these routines.
      !>
      !>        C
      !>        C     Sort the object arrays by ID code.
      !>        C
      !>              CALL ORDERI ( CODES,  N, IORDER )
      !>
      !>              CALL REORDC ( IORDER, N, NAMES  )
      !>              CALL REORDD ( IORDER, N, MASSES )
      !>              CALL REORDI ( IORDER, N, CODES  )
      !>              CALL REORDR ( IORDER, N, VMAGS  )
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
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 23-MAR-2010 (NJB)
      !>
      !>        Header example was updated to show use of this routine.
      !>        $Exceptions section was updated. Header sections were
      !>        re-ordered.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine orderi ( array, ndim, iorder )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: array  ( * )  !! Input array.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(out) :: iorder ( * )  !! Order vector for ARRAY.

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

!
!     Begin with the initial ordering.
!
      do i = 1, ndim
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = ndim / 2

      do while ( gap > 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j > 0 )
               jg = j + gap

               if ( array(iorder(j)) <= array(iorder(jg)) ) then
                  j = 0
               else
                  call swapi ( iorder(j), iorder(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine orderi

      !> Return the product of the elements of a double precision array.
      !>
      !> The function returns the product of the elements of ARRAY.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     N        is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the product of the elements of the input
      !>     array. That is,
      !>
      !>        PRODAD = ARRAY(1) * ARRAY(2) * ... * ARRAY(N)
      !>
      !>     If N is zero or negative, PRODAD is one.
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
      !>     The value of the function is initially set to one. The elements
      !>     of the array are then multiplied. If the number of elements is
      !>     zero or negative, PRODAD is one.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           ARRAY(1) = 12.D0
      !>           ARRAY(2) =  2.D0
      !>           ARRAY(3) =  4.D0
      !>           ARRAY(4) = 75.D0
      !>           ARRAY(5) = 18.D0
      !>
      !>     Then
      !>
      !>           PRODAD ( ARRAY,   -3 )       =      1.D0
      !>           PRODAD ( ARRAY,    0 )       =      1.D0
      !>           PRODAD ( ARRAY,    1 )       =     12.D0
      !>           PRODAD ( ARRAY,    2 )       =     24.D0
      !>           PRODAD ( ARRAY,    5 )       = 129600.D0
      !>           PRODAD ( ARRAY(3), 3 )       =   5400.D0
      !>
      !>## Restrictions
      !>
      !>     1)  PRODAD does not check for overflow.
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
      !>-    SPICELIB Version 1.1.0, 07-APR-2021 (JDR)
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
      pure real(wp) function  prodad ( array, n )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: array ( * )  !! Input array.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.

      real(wp) :: prod
      integer(ip) :: i

!
!     Begin at one.
!
      prod = 1.0_wp

!
!     Multiply the elements. If N is zero or negative, nothing happens.
!
      do i = 1, n
         prod = prod * array(i)
      end do

!
!     Return the product.
!
      prodad = prod

      return
      end function prodad

      !> Return the product of the elements of an integer array.
      !>
      !> The function returns the product of the elements of ARRAY.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input array.
      !>
      !>     N        is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the product of the elements of the input
      !>     array. That is,
      !>
      !>        PRODAI = ARRAY(1) * ARRAY(2) * ... * ARRAY(N)
      !>
      !>     If N is zero or negative, PRODAI is one.
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
      !>     The value of the function is initially set to one. The elements
      !>     of the array are then multiplied. If the number of elements is
      !>     zero or negative, PRODAI is one.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           ARRAY(1) = 12
      !>           ARRAY(2) =  2
      !>           ARRAY(3) =  4
      !>           ARRAY(4) = 75
      !>           ARRAY(5) = 18
      !>
      !>     Then
      !>
      !>           PRODAI ( ARRAY,   -3 )       =      1
      !>           PRODAI ( ARRAY,    0 )       =      1
      !>           PRODAI ( ARRAY,    1 )       =     12
      !>           PRODAI ( ARRAY,    2 )       =     24
      !>           PRODAI ( ARRAY,    5 )       = 129600
      !>           PRODAI ( ARRAY(3), 3 )       =   5400
      !>
      !>## Restrictions
      !>
      !>     1)  PRODAI does not check for overflow. (For integers, this can
      !>         occur relatively quickly.)
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
      pure integer(ip) function  prodai ( array, n )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: array  ( * )  !! Input array.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.

      integer(ip) :: prod
      integer(ip) :: i

!
!     Begin at one.
!
      prod = 1

!
!     Multiply the elements. If N is zero or negative, nothing happens.
!
      do i = 1, n
         prod = prod * array(i)
      end do

!
!     Return the product.
!
      prodai = prod

      return
      end function prodai

      !> Reorder the elements of an array of character strings according to
      !> a given order vector.
      !>
      !>## Detailed Input
      !>
      !>     IORDER   is the order vector to be used to re-order the input
      !>              array. The first element of IORDER is the index of
      !>              the first item of the re-ordered array, and so on.
      !>
      !>              Note that the order imposed by REORDC is not the
      !>              same order that would be imposed by a sorting
      !>              routine. In general, the order vector will have
      !>              been created (by one of the ORDER routines) for
      !>              a related array, as illustrated in the example below.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    on input, is an array containing some number of
      !>              elements in unspecified order.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the same array, with the elements
      !>              re-ordered as specified by IORDER.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine executes a no-op. This case is
      !>         not an error.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     REORDC uses a cyclical algorithm to re-order the elements of
      !>     the array in place. After re-ordering, element IORDER(1) of
      !>     the input array is the first element of the output array,
      !>     element IORDER(2) is the input array is the second element of
      !>     the output array, and so on.
      !>
      !>     The order vector used by REORDC is typically created for
      !>     a related array by one of the ORDER routines, as shown in
      !>     the example below.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Sort four related arrays (containing the names, masses,
      !>        integer ID codes, and flags indicating whether they have
      !>        a ring system, for a group of planets).
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REORDC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NDIM
      !>      PARAMETER             ( NDIM   = 8  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 7  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      NAMES  ( NDIM )
      !>
      !>      DOUBLE PRECISION        MASSES ( NDIM )
      !>
      !>      INTEGER                 CODES  ( NDIM )
      !>      INTEGER                 I
      !>      INTEGER                 IORDER ( NDIM )
      !>
      !>      LOGICAL                 RINGS  ( NDIM )
      !>
      !>C
      !>C     Set the arrays containing the names, masses (given as
      !>C     ratios to of Solar GM to barycenter GM), integer ID
      !>C     codes, and flags indicating whether they have a ring
      !>C     system.
      !>C
      !>      DATA                    NAMES  /
      !>     .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
      !>     .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
      !>
      !>      DATA                    MASSES /
      !>     .                       22032.080D0,   324858.599D0,
      !>     .                      398600.436D0,    42828.314D0,
      !>     .                   126712767.881D0, 37940626.068D0,
      !>     .                     5794559.128D0,  6836534.065D0 /
      !>
      !>      DATA                    CODES  / 199, 299, 399, 499,
      !>     .                                 599, 699, 799, 899 /
      !>
      !>      DATA                    RINGS  /
      !>     .                   .FALSE., .FALSE., .FALSE., .FALSE.,
      !>     .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
      !>
      !>C
      !>C     Sort the object arrays by name.
      !>C
      !>      CALL ORDERC ( NAMES,  NDIM, IORDER )
      !>
      !>      CALL REORDC ( IORDER, NDIM, NAMES  )
      !>      CALL REORDD ( IORDER, NDIM, MASSES )
      !>      CALL REORDI ( IORDER, NDIM, CODES  )
      !>      CALL REORDL ( IORDER, NDIM, RINGS  )
      !>
      !>C
      !>C     Output the resulting table.
      !>C
      !>      WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
      !>      WRITE(*,'(A)') '-------  -------------  -------  ------'
      !>
      !>      DO I = 1, NDIM
      !>
      !>         WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
      !>     .                              CODES(I), RINGS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planet   Mass(GMS/GM)  ID Code  Rings?
      !>        -------  -------------  -------  ------
      !>        EARTH       398600.436      399    F
      !>        JUPITER  126712767.881      599    T
      !>        MARS         42828.314      499    F
      !>        MERCURY      22032.080      199    F
      !>        NEPTUNE    6836534.065      899    T
      !>        SATURN    37940626.068      699    T
      !>        URANUS     5794559.128      799    T
      !>        VENUS       324858.599      299    F
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
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      pure subroutine reordc ( iorder, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: iorder ( * )  !! Order vector to be used to re-order ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      character(*), intent(inout) :: array  ( * )  !! Array to be re-ordered.

      integer(ip) :: start
      integer(ip) :: index
      integer(ip) :: hold

      integer(ip) :: c
      character(1) :: temp

!
!     If the array doesn't have at least two elements, don't bother.
!
      if ( ndim < 2 ) then
           return
      end if

!
!     What follows is pretty much the same as for the other REORD
!     routines. The character routine is somewhat special in that
!     the use of a temporary variable would cause strings longer
!     than the variable to be truncated. Rather than just declare
!     a giant character string, the entire algorithm will be repeated
!     for each character in each string. That is, the first characters
!     will be ordered, then the second characters, and so on. This
!     looks messy as hell, but the same number of operations are
!     involved (more or less).
!
      do c = 1, len ( array(1) )

!
!        START is the position in the order vector that begins the
!        current cycle. When all the switches have been made, START
!        will point to the end of the order vector.
!
         start = 1

         do while ( start < ndim )

!
!           Begin with the element of input vector specified by
!           IORDER(START). Move it to the correct position in the
!           array, after saving the element it replaces to TEMP.
!           HOLD indicates the position of the array element to
!           be moved to its new position. After the element has
!           been moved, HOLD indicates the position of an available
!           space within the array.
!
            index = start
            temp  = array(index)(c:c)
            hold  = iorder(index)

!
!           As each slot in the output array is filled in, the sign
!           of the corresponding element in the order vector is changed
!           from positive to negative. This way, we know which elements
!           have already been ordered when looking for the beginning of
!           the next cycle.
!
!           Keep going until HOLD points to the first array element
!           moved during the current cycle. This ends the cycle.
!
            do while ( hold /= start )
               array(index)(c:c)  =  array(hold)(c:c)
               index              =  hold
               hold               =  iorder(hold)
               iorder(index)      = -iorder(index)
            end do

!
!           The last element in the cycle is restored from TEMP.
!
            array(index)(c:c) =  temp
            iorder(hold)      = -iorder(hold)

!
!           Begin the next cycle at the next element in the order
!           vector with a positive sign. (That is, the next one
!           that hasn't been moved.)
!
            do while ( iorder(start) < 0  .and.  start < ndim )
               start = start + 1
            end do

         end do

!
!        Restore the original signs of the elements of the order
!        vector, for the next go around.
!
         do index = 1, ndim
           iorder(index) = iabs ( iorder(index) )
         end do

      end do

      return
      end subroutine reordc

      !> Reorder the elements of a double precision array according to a
      !> given order vector.
      !>
      !>## Detailed Input
      !>
      !>     IORDER   is the order vector to be used to re-order the input
      !>              array. The first element of IORDER is the index of
      !>              the first item of the re-ordered array, and so on.
      !>
      !>              Note that the order imposed by REORDD is not the
      !>              same order that would be imposed by a sorting
      !>              routine. In general, the order vector will have
      !>              been created (by one of the ORDER routines) for
      !>              a related array, as illustrated in the example below.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    on input, is an array containing some number of
      !>              elements in unspecified order.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the same array, with the elements
      !>              re-ordered as specified by IORDER.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine executes a no-op. This case is
      !>         not an error.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     REORDD uses a cyclical algorithm to re-order the elements of
      !>     the array in place. After re-ordering, element IORDER(1) of
      !>     the input array is the first element of the output array,
      !>     element IORDER(2) is the input array is the second element of
      !>     the output array, and so on.
      !>
      !>     The order vector used by REORDD is typically created for
      !>     a related array by one of the ORDER routines, as shown in
      !>     the example below.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Sort four related arrays (containing the names, masses,
      !>        integer ID codes, and flags indicating whether they have
      !>        a ring system, for a group of planets).
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REORDD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NDIM
      !>      PARAMETER             ( NDIM   = 8  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 7  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      NAMES  ( NDIM )
      !>
      !>      DOUBLE PRECISION        MASSES ( NDIM )
      !>
      !>      INTEGER                 CODES  ( NDIM )
      !>      INTEGER                 I
      !>      INTEGER                 IORDER ( NDIM )
      !>
      !>      LOGICAL                 RINGS  ( NDIM )
      !>
      !>C
      !>C     Set the arrays containing the names, masses (given as
      !>C     ratios to of Solar GM to barycenter GM), integer ID
      !>C     codes, and flags indicating whether they have a ring
      !>C     system.
      !>C
      !>      DATA                    NAMES  /
      !>     .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
      !>     .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
      !>
      !>      DATA                    MASSES /
      !>     .                       22032.080D0,   324858.599D0,
      !>     .                      398600.436D0,    42828.314D0,
      !>     .                   126712767.881D0, 37940626.068D0,
      !>     .                     5794559.128D0,  6836534.065D0 /
      !>
      !>      DATA                    CODES  / 199, 299, 399, 499,
      !>     .                                 599, 699, 799, 899 /
      !>
      !>      DATA                    RINGS  /
      !>     .                   .FALSE., .FALSE., .FALSE., .FALSE.,
      !>     .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
      !>
      !>C
      !>C     Sort the object arrays by name.
      !>C
      !>      CALL ORDERC ( NAMES,  NDIM, IORDER )
      !>
      !>      CALL REORDC ( IORDER, NDIM, NAMES  )
      !>      CALL REORDD ( IORDER, NDIM, MASSES )
      !>      CALL REORDI ( IORDER, NDIM, CODES  )
      !>      CALL REORDL ( IORDER, NDIM, RINGS  )
      !>
      !>C
      !>C     Output the resulting table.
      !>C
      !>      WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
      !>      WRITE(*,'(A)') '-------  -------------  -------  ------'
      !>
      !>      DO I = 1, NDIM
      !>
      !>         WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
      !>     .                              CODES(I), RINGS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planet   Mass(GMS/GM)  ID Code  Rings?
      !>        -------  -------------  -------  ------
      !>        EARTH       398600.436      399    F
      !>        JUPITER  126712767.881      599    T
      !>        MARS         42828.314      499    F
      !>        MERCURY      22032.080      199    F
      !>        NEPTUNE    6836534.065      899    T
      !>        SATURN    37940626.068      699    T
      !>        URANUS     5794559.128      799    T
      !>        VENUS       324858.599      299    F
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
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      pure subroutine reordd ( iorder, ndim, array )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(inout) :: iorder ( * )  !! Order vector to be used to re-order ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      real(wp), intent(inout) :: array  ( * )  !! Array to be re-ordered.

      integer(ip) :: start
      integer(ip) :: index
      integer(ip) :: hold

      real(wp) :: temp

!
!     If the array doesn't have at least two elements, don't bother.
!
      if ( ndim < 2 ) then
           return
      end if

!
!     START is the position in the order vector that begins the
!     current cycle. When all the switches have been made, START
!     will point to the end of the order vector.
!
      start = 1

      do while ( start < ndim )

!
!        Begin with the element of input vector specified by
!        IORDER(START). Move it to the correct position in the
!        array, after saving the element it replaces to TEMP.
!        HOLD indicates the position of the array element to
!        be moved to its new position. After the element has
!        been moved, HOLD indicates the position of an available
!        space within the array.
!
         index = start
         temp  = array(index)
         hold  = iorder(index)

!
!        As each slot in the output array is filled in, the sign
!        of the corresponding element in the order vector is changed
!        from positive to negative. This way, we know which elements
!        have already been ordered when looking for the beginning of
!        the next cycle.
!
!        Keep going until HOLD points to the first array element
!        moved during the current cycle. This ends the cycle.
!
         do while ( hold /= start )
            array(index)  =  array(hold)
            index         =  hold
            hold          =  iorder(hold)
            iorder(index) = -iorder(index)
         end do

!
!        The last element in the cycle is restored from TEMP.
!
         array(index) =  temp
         iorder(hold) = -iorder(hold)

!
!        Begin the next cycle at the next element in the order
!        vector with a positive sign. (That is, the next one
!        that hasn't been moved.)
!
         do while ( iorder(start) < 0  .and.  start < ndim )
            start = start + 1
         end do

      end do

!
!     Restore the original signs of the elements of the order vector,
!     in case the vector is to be used again with another array.
!
      do index = 1, ndim
        iorder(index) = iabs ( iorder(index) )
      end do

      return
      end subroutine reordd

      !> Reorder the elements of an integer array according to a given
      !> order vector.
      !>
      !>## Detailed Input
      !>
      !>     IORDER   is the order vector to be used to re-order the input
      !>              array. The first element of IORDER is the index of
      !>              the first item of the re-ordered array, and so on.
      !>
      !>              Note that the order imposed by REORDI is not the
      !>              same order that would be imposed by a sorting
      !>              routine. In general, the order vector will have
      !>              been created (by one of the ORDER routines) for
      !>              a related array, as illustrated in the example below.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    on input, is an array containing some number of
      !>              elements in unspecified order.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the same array, with the elements
      !>              re-ordered as specified by IORDER.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine executes a no-op. This case is
      !>         not an error.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     REORDI uses a cyclical algorithm to re-order the elements of
      !>     the array in place. After re-ordering, element IORDER(1) of
      !>     the input array is the first element of the output array,
      !>     element IORDER(2) is the input array is the second element of
      !>     the output array, and so on.
      !>
      !>     The order vector used by REORDI is typically created for
      !>     a related array by one of the ORDER routines, as shown in
      !>     the example below.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Sort four related arrays (containing the names, masses,
      !>        integer ID codes, and flags indicating whether they have
      !>        a ring system, for a group of planets).
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REORDI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NDIM
      !>      PARAMETER             ( NDIM   = 8  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 7  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      NAMES  ( NDIM )
      !>
      !>      DOUBLE PRECISION        MASSES ( NDIM )
      !>
      !>      INTEGER                 CODES  ( NDIM )
      !>      INTEGER                 I
      !>      INTEGER                 IORDER ( NDIM )
      !>
      !>      LOGICAL                 RINGS  ( NDIM )
      !>
      !>C
      !>C     Set the arrays containing the names, masses (given as
      !>C     ratios to of Solar GM to barycenter GM), integer ID
      !>C     codes, and flags indicating whether they have a ring
      !>C     system.
      !>C
      !>      DATA                    NAMES  /
      !>     .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
      !>     .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
      !>
      !>      DATA                    MASSES /
      !>     .                       22032.080D0,   324858.599D0,
      !>     .                      398600.436D0,    42828.314D0,
      !>     .                   126712767.881D0, 37940626.068D0,
      !>     .                     5794559.128D0,  6836534.065D0 /
      !>
      !>      DATA                    CODES  / 199, 299, 399, 499,
      !>     .                                 599, 699, 799, 899 /
      !>
      !>      DATA                    RINGS  /
      !>     .                   .FALSE., .FALSE., .FALSE., .FALSE.,
      !>     .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
      !>
      !>C
      !>C     Sort the object arrays by name.
      !>C
      !>      CALL ORDERC ( NAMES,  NDIM, IORDER )
      !>
      !>      CALL REORDC ( IORDER, NDIM, NAMES  )
      !>      CALL REORDD ( IORDER, NDIM, MASSES )
      !>      CALL REORDI ( IORDER, NDIM, CODES  )
      !>      CALL REORDL ( IORDER, NDIM, RINGS  )
      !>
      !>C
      !>C     Output the resulting table.
      !>C
      !>      WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
      !>      WRITE(*,'(A)') '-------  -------------  -------  ------'
      !>
      !>      DO I = 1, NDIM
      !>
      !>         WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
      !>     .                              CODES(I), RINGS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planet   Mass(GMS/GM)  ID Code  Rings?
      !>        -------  -------------  -------  ------
      !>        EARTH       398600.436      399    F
      !>        JUPITER  126712767.881      599    T
      !>        MARS         42828.314      499    F
      !>        MERCURY      22032.080      199    F
      !>        NEPTUNE    6836534.065      899    T
      !>        SATURN    37940626.068      699    T
      !>        URANUS     5794559.128      799    T
      !>        VENUS       324858.599      299    F
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
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      pure subroutine reordi ( iorder, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: iorder ( * )  !! Order vector to be used to re-order ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      integer(ip), intent(inout) :: array  ( * )  !! Array to be re-ordered.

      integer(ip) :: start
      integer(ip) :: index
      integer(ip) :: hold

      integer(ip) :: temp

!
!     If the array doesn't have at least two elements, don't bother.
!
      if ( ndim < 2 ) then
           return
      end if

!
!     START is the position in the order vector that begins the
!     current cycle. When all the switches have been made, START
!     will point to the end of the order vector.
!
      start = 1

      do while ( start < ndim )

!
!        Begin with the element of input vector specified by
!        IORDER(START). Move it to the correct position in the
!        array, after saving the element it replaces to TEMP.
!        HOLD indicates the position of the array element to
!        be moved to its new position. After the element has
!        been moved, HOLD indicates the position of an available
!        space within the array.
!
         index = start
         temp  = array(index)
         hold  = iorder(index)

!
!        As each slot in the output array is filled in, the sign
!        of the corresponding element in the order vector is changed
!        from positive to negative. This way, we know which elements
!        have already been ordered when looking for the beginning of
!        the next cycle.
!
!        Keep going until HOLD points to the first array element
!        moved during the current cycle. This ends the cycle.
!
         do while ( hold /= start )
            array(index)  =  array(hold)
            index         =  hold
            hold          =  iorder(hold)
            iorder(index) = -iorder(index)
         end do

!
!        The last element in the cycle is restored from TEMP.
!
         array(index) =  temp
         iorder(hold) = -iorder(hold)

!
!        Begin the next cycle at the next element in the order
!        vector with a positive sign. (That is, the next one
!        that hasn't been moved.)
!
         do while ( iorder(start) < 0  .and.  start < ndim )
            start = start + 1
         end do

      end do

!
!     Restore the original signs of the elements of the order vector,
!     in case the vector is to be used again with another array.
!
      do index = 1, ndim
        iorder(index) = iabs ( iorder(index) )
      end do

      return
      end subroutine reordi

      !> Reorder the elements of a logical array according to a given order
      !> vector.
      !>
      !>## Detailed Input
      !>
      !>     IORDER   is the order vector to be used to re-order the input
      !>              array. The first element of IORDER is the index of
      !>              the first item of the re-ordered array, and so on.
      !>
      !>              Note that the order imposed by REORDL is not the
      !>              same order that would be imposed by a sorting
      !>              routine. In general, the order vector will have
      !>              been created (by one of the ORDER routines) for
      !>              a related array, as illustrated in the example below.
      !>
      !>     NDIM     is the number of elements in the input array.
      !>
      !>     ARRAY    on input, is an array containing some number of
      !>              logicals in unspecified order.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the same array, with the logicals
      !>              re-ordered as specified by IORDER.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine executes a no-op. This case is
      !>         not an error.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     REORDL uses a cyclical algorithm to re-order the elements of
      !>     the array in place. After re-ordering, element IORDER(1) of
      !>     the input array is the first element of the output array,
      !>     element IORDER(2) is the input array is the second element of
      !>     the output array, and so on.
      !>
      !>     The order vector used by REORDL is typically created for
      !>     a related array by one of the ORDER routines, as shown in
      !>     the example below.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Sort four related arrays (containing the names, masses,
      !>        integer ID codes, and flags indicating whether they have
      !>        a ring system, for a group of planets).
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM REORDL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 NDIM
      !>      PARAMETER             ( NDIM   = 8  )
      !>
      !>      INTEGER                 STRLEN
      !>      PARAMETER             ( STRLEN = 7  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(STRLEN)      NAMES  ( NDIM )
      !>
      !>      DOUBLE PRECISION        MASSES ( NDIM )
      !>
      !>      INTEGER                 CODES  ( NDIM )
      !>      INTEGER                 I
      !>      INTEGER                 IORDER ( NDIM )
      !>
      !>      LOGICAL                 RINGS  ( NDIM )
      !>
      !>C
      !>C     Set the arrays containing the names, masses (given as
      !>C     ratios to of Solar GM to barycenter GM), integer ID
      !>C     codes, and flags indicating whether they have a ring
      !>C     system.
      !>C
      !>      DATA                    NAMES  /
      !>     .            'MERCURY', 'VENUS',  'EARTH',  'MARS',
      !>     .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' /
      !>
      !>      DATA                    MASSES /
      !>     .                       22032.080D0,   324858.599D0,
      !>     .                      398600.436D0,    42828.314D0,
      !>     .                   126712767.881D0, 37940626.068D0,
      !>     .                     5794559.128D0,  6836534.065D0 /
      !>
      !>      DATA                    CODES  / 199, 299, 399, 499,
      !>     .                                 599, 699, 799, 899 /
      !>
      !>      DATA                    RINGS  /
      !>     .                   .FALSE., .FALSE., .FALSE., .FALSE.,
      !>     .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   /
      !>
      !>C
      !>C     Sort the object arrays by name.
      !>C
      !>      CALL ORDERC ( NAMES,  NDIM, IORDER )
      !>
      !>      CALL REORDC ( IORDER, NDIM, NAMES  )
      !>      CALL REORDD ( IORDER, NDIM, MASSES )
      !>      CALL REORDI ( IORDER, NDIM, CODES  )
      !>      CALL REORDL ( IORDER, NDIM, RINGS  )
      !>
      !>C
      !>C     Output the resulting table.
      !>C
      !>      WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?'
      !>      WRITE(*,'(A)') '-------  -------------  -------  ------'
      !>
      !>      DO I = 1, NDIM
      !>
      !>         WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I),
      !>     .                              CODES(I), RINGS(I)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Planet   Mass(GMS/GM)  ID Code  Rings?
      !>        -------  -------------  -------  ------
      !>        EARTH       398600.436      399    F
      !>        JUPITER  126712767.881      599    T
      !>        MARS         42828.314      499    F
      !>        MERCURY      22032.080      199    F
      !>        NEPTUNE    6836534.065      899    T
      !>        SATURN    37940626.068      699    T
      !>        URANUS     5794559.128      799    T
      !>        VENUS       324858.599      299    F
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
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.0, 06-MAR-1996 (WLT) (IMU)
      pure subroutine reordl ( iorder, ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: iorder ( * )  !! Order vector to be used to re-order ARRAY.
      integer(ip), intent(in) :: ndim  !! Dimension of ARRAY.
      logical, intent(inout) :: array  ( * )  !! Array to be re-ordered.

      integer(ip) :: start
      integer(ip) :: index
      integer(ip) :: hold

      logical :: temp

!
!     If the array doesn't have at least two elements, don't bother.
!
      if ( ndim < 2 ) then
           return
      end if

!
!     START is the position in the order vector that begins the
!     current cycle. When all the switches have been made, START
!     will point to the end of the order vector.
!
      start = 1

      do while ( start < ndim )

!
!        Begin with the element of input vector specified by
!        IORDER(START). Move it to the correct position in the
!        array, after saving the element it replaces to TEMP.
!        HOLD indicates the position of the array element to
!        be moved to its new position. After the element has
!        been moved, HOLD indicates the position of an available
!        space within the array.
!
         index = start
         temp  = array(index)
         hold  = iorder(index)

!
!        As each slot in the output array is filled in, the sign
!        of the corresponding element in the order vector is changed
!        from positive to negative. This way, we know which elements
!        have already been ordered when looking for the beginning of
!        the next cycle.
!
!        Keep going until HOLD points to the first array element
!        moved during the current cycle. This ends the cycle.
!
         do while ( hold /= start )
            array(index)  =  array(hold)
            index         =  hold
            hold          =  iorder(hold)
            iorder(index) = -iorder(index)
         end do

!
!        The last element in the cycle is restored from TEMP.
!
         array(index) =  temp
         iorder(hold) = -iorder(hold)

!
!        Begin the next cycle at the next element in the order
!        vector with a positive sign. (That is, the next one
!        that hasn't been moved.)
!
         do while ( iorder(start) < 0  .and.  start < ndim )
            start = start + 1
         end do

      end do

!
!     Restore the original signs of the elements of the order vector,
!     in case the vector is to be used again with another array.
!
      do index = 1, ndim
        iorder(index) = iabs ( iorder(index) )
      end do

      return
      end subroutine reordl

      !> Remove duplicate elements from a character array.
      !>
      !>## Detailed Input
      !>
      !>     NELT     on input is the number of elements in the input
      !>              array.
      !>
      !>     ARRAY    on input contains zero or more elements, from which
      !>              all duplicate elements are to be removed.
      !>
      !>## Detailed Output
      !>
      !>     NELT     on output is the number of elements in the output
      !>              array.
      !>
      !>     ARRAY    on output contains the distinct elements of the
      !>              input array, sorted in increasing order. (Character
      !>              arrays are sorted according to the ASCII collating
      !>              sequence).
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
      !>     Let the arrays C and I contain the following elements.
      !>
      !>           NC   = 7                NI   =   5
      !>           C(1) = 'Miranda'        I(1) =  13
      !>           C(2) = 'Ariel'          I(2) = -13
      !>           C(3) = 'Umbriel'        I(3) =   0
      !>           C(4) = 'Titania'        I(4) =   1
      !>           C(5) = 'Miranda'        I(5) =   0
      !>           C(6) = 'Oberon'
      !>           C(7) = 'Umbriel'
      !>
      !>     Then following the calls
      !>
      !>           CALL RMDUPC ( NC, C )
      !>           CALL RMDUPI ( NI, I )
      !>
      !>     C and I contain the following.
      !>
      !>           NC   = 5                NI   =   4
      !>           C(1) = 'Ariel'          I(1) = -13
      !>           C(2) = 'Miranda'        I(2) =   0
      !>           C(3) = 'Oberon'         I(3) =   1
      !>           C(4) = 'Titania'        I(4) =  13
      !>           C(5) = 'Umbriel'
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
      !>-    SPICELIB Version 1.1.0, 05-JUN-2021 (JDR)
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
      pure subroutine rmdupc ( nelt, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: nelt  !! Number of elements in the array.
      character(*), intent(inout) :: array   (*)  !! Input/output array.

      integer(ip) :: i
      integer(ip) :: j

!
!     Proceed only if the array actually contains more than one element.
!
      if ( nelt > 1 ) then

!
!        Sort the array in place.
!
         call shellc ( nelt, array )

!
!        Drop duplicate entries. Compare adjacent entries, and move
!        duplicates forward. (Duplicates are now adjacent, because of
!        sorting.)
!
         j = 1

         do i = 2, nelt

            if ( array(i) /= array(i-1) ) then
               j        = j + 1
               array(j) = array(i)
            end if

        end do

        nelt = j

      end if

      return
      end subroutine rmdupc

      !> Remove duplicate elements from a double precision array.
      !>
      !>## Detailed Input
      !>
      !>     NELT     on input is the number of elements in the input
      !>              array.
      !>
      !>     ARRAY    on input contains zero or more elements, from which
      !>              all duplicate elements are to be removed.
      !>
      !>## Detailed Output
      !>
      !>     NELT     on output is the number of elements in the output
      !>              array.
      !>
      !>     ARRAY    on output contains the distinct elements of the
      !>              input array, sorted in increasing order. (Character
      !>              arrays are sorted according to the ASCII collating
      !>              sequence).
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
      !>     Let the arrays C and I contain the following elements.
      !>
      !>           NC   = 7                NI   =   5
      !>           C(1) = 'Miranda'        I(1) =  13
      !>           C(2) = 'Ariel'          I(2) = -13
      !>           C(3) = 'Umbriel'        I(3) =   0
      !>           C(4) = 'Titania'        I(4) =   1
      !>           C(5) = 'Miranda'        I(5) =   0
      !>           C(6) = 'Oberon'
      !>           C(7) = 'Umbriel'
      !>
      !>     Then following the calls
      !>
      !>           CALL RMDUPC ( NC, C )
      !>           CALL RMDUPI ( NI, I )
      !>
      !>     C and I contain the following.
      !>
      !>           NC   = 5                NI   =   4
      !>           C(1) = 'Ariel'          I(1) = -13
      !>           C(2) = 'Miranda'        I(2) =   0
      !>           C(3) = 'Oberon'         I(3) =   1
      !>           C(4) = 'Titania'        I(4) =  13
      !>           C(5) = 'Umbriel'
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine rmdupd ( nelt, array )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(inout) :: nelt  !! Number of elements in the array.
      real(wp), intent(inout) :: array   (*)  !! Input/output array.

      integer(ip) :: i
      integer(ip) :: j

!
!     Proceed only if the array actually contains more than one element.
!
      if ( nelt > 1 ) then

!
!        Sort the array in place.
!
         call shelld ( nelt, array )

!
!        Drop duplicate entries. Compare adjacent entries, and move
!        duplicates forward. (Duplicates are now adjacent, because of
!        sorting.)
!
         j = 1

         do i = 2, nelt

            if ( array(i) /= array(i-1) ) then
               j        = j + 1
               array(j) = array(i)
            end if

        end do

        nelt = j

      end if

      return
      end subroutine rmdupd

      !> Remove duplicate elements from an integer array.
      !>
      !>## Detailed Input
      !>
      !>     NELT     on input is the number of elements in the input
      !>              array.
      !>
      !>     ARRAY    on input contains zero or more elements, from which
      !>              all duplicate elements are to be removed.
      !>
      !>## Detailed Output
      !>
      !>     NELT     on output is the number of elements in the output
      !>              array.
      !>
      !>     ARRAY    on output contains the distinct elements of the
      !>              input array, sorted in increasing order. (Character
      !>              arrays are sorted according to the ASCII collating
      !>              sequence).
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
      !>     Let the arrays C and I contain the following elements.
      !>
      !>           NC   = 7                NI   =   5
      !>           C(1) = 'Miranda'        I(1) =  13
      !>           C(2) = 'Ariel'          I(2) = -13
      !>           C(3) = 'Umbriel'        I(3) =   0
      !>           C(4) = 'Titania'        I(4) =   1
      !>           C(5) = 'Miranda'        I(5) =   0
      !>           C(6) = 'Oberon'
      !>           C(7) = 'Umbriel'
      !>
      !>     Then following the calls
      !>
      !>           CALL RMDUPC ( NC, C )
      !>           CALL RMDUPI ( NI, I )
      !>
      !>     C and I contain the following.
      !>
      !>           NC   = 5                NI   =   4
      !>           C(1) = 'Ariel'          I(1) = -13
      !>           C(2) = 'Miranda'        I(2) =   0
      !>           C(3) = 'Oberon'         I(3) =   1
      !>           C(4) = 'Titania'        I(4) =  13
      !>           C(5) = 'Umbriel'
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine rmdupi ( nelt, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: nelt  !! Number of elements in the array.
      integer(ip), intent(inout) :: array   (*)  !! Input/output array.

      integer(ip) :: i
      integer(ip) :: j

!
!     Proceed only if the array actually contains more than one element.
!
      if ( nelt > 1 ) then

!
!        Sort the array in place.
!
         call shelli ( nelt, array )

!
!        Drop duplicate entries. Compare adjacent entries, and move
!        duplicates forward. (Duplicates are now adjacent, because of
!        sorting.)
!
         j = 1

         do i = 2, nelt

            if ( array(i) /= array(i-1) ) then
               j        = j + 1
               array(j) = array(i)
            end if

        end do

        nelt = j

      end if

      return
      end subroutine rmdupi

      !> Indicate whether two integer arrays are equal.
      !>
      !> The function returns the value .TRUE. if and only if A1 = A2.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (ALL instead).
      !>
      !>## Detailed Input
      !>
      !>     A1,
      !>     A2       are two integer arrays to be compared.  A1 and
      !>              A2 must have the same dimension.
      !>
      !>     NDIM     is the common dimension of A1 and A2.
      !>
      !>## Detailed Output
      !>
      !>     The function takes the value .TRUE. if and only if A1 equals A2.
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
      !>     This function can be thought of as a macro. It replaces the
      !>     loop
      !>
      !>        SAME  = .TRUE.
      !>        I     =  1
      !>
      !>        DO WHILE (  ( I .LE. NDIM )  .AND.  SAME  )
      !>
      !>           IF ( A1(I) .NE.  A2(I)  )
      !>              SAME  = .FALSE.
      !>           ELSE
      !>              I     =  I + 1
      !>           END IF
      !>
      !>        END DO
      !>
      !>## Examples
      !>
      !>     1)  Test two integer arrays A1 and A2 for equality, where both
      !>         arrays have declared length 10:
      !>
      !>            SAME  =  SAMEAI ( A1, A2, 10 )
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
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
      pure logical function sameai ( a1, a2, ndim )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: a1 ( * )  !! First array to be compared.
      integer(ip), intent(in) :: a2 ( * )  !! Second array to be compared.
      integer(ip), intent(in) :: ndim  !! Dimension of A1 and A2.

!
!     Executable code
!
      sameai = all(a1(1:ndim) == a2(1:ndim))
      end function sameai

      !> Sort an array of character strings according to the ASCII
      !> collating sequence using the Shell Sort algorithm.
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the number of elements in the array to be sorted.
      !>
      !>     ARRAY    on input, is the array to be sorted.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, contains the same elements, sorted
      !>              according to the ASCII collating sequence.
      !>              The actual sorting is done in place in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine does not modify the array.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The Shell Sort Algorithm is well known.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a list of words, sort it according to the ASCII
      !>        collating sequence using the Shell Sort algorithm.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SHELLC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM  = 6 )
      !>
      !>      INTEGER               WRDSZ
      !>      PARAMETER           ( WRDSZ = 8 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(WRDSZ)     ARRAY  ( NDIM )
      !>      INTEGER               I
      !>
      !>C
      !>C     Let ARRAY contain the following elements:
      !>C
      !>      ARRAY(1) = 'FEYNMAN'
      !>      ARRAY(2) = 'NEWTON'
      !>      ARRAY(3) = 'EINSTEIN'
      !>      ARRAY(4) = 'GALILEO'
      !>      ARRAY(5) = 'EUCLID'
      !>      ARRAY(6) = 'Galileo'
      !>
      !>C
      !>C     Print ARRAY before calling SHELLC.
      !>C
      !>      WRITE(*,*) 'Array before calling SHELLC:'
      !>      WRITE(*,*)
      !>      DO I = 1, NDIM
      !>         WRITE(*,*) '   ', ARRAY(I)
      !>      END DO
      !>      WRITE(*,*)
      !>
      !>C
      !>C     Call SHELLC and print ARRAY again.
      !>C
      !>      CALL SHELLC ( NDIM, ARRAY )
      !>
      !>      WRITE(*,*) 'Array after calling SHELLC:'
      !>      WRITE(*,*)
      !>      DO I = 1, NDIM
      !>         WRITE(*,*) '   ', ARRAY(I)
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Array before calling SHELLC:
      !>
      !>            FEYNMAN
      !>            NEWTON
      !>            EINSTEIN
      !>            GALILEO
      !>            EUCLID
      !>            Galileo
      !>
      !>         Array after calling SHELLC:
      !>
      !>            EINSTEIN
      !>            EUCLID
      !>            FEYNMAN
      !>            GALILEO
      !>            Galileo
      !>            NEWTON
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example. Extended $Exceptions
      !>        section to explain what happens if NDIM < 2.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine shellc ( ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of the array.
      character(*), intent(inout) :: array ( * )  !! The array.

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

!
!     This is a straightforward implementation of the Shell Sort
!     algorithm.
!
      gap = ndim / 2

      do while ( gap > 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j > 0 )
               jg = j + gap

               if ( lle (array(j), array(jg)) ) then
                  j = 0
               else
                  call swapc ( array(j), array(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine shellc

      !> Sort a double precision array using the Shell Sort algorithm.
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the number of elements in the array to be sorted.
      !>
      !>     ARRAY    on input, is the array to be sorted.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, contains the same elements, sorted
      !>              in increasing order. The actual sorting is done
      !>              in place in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine does not modify the array.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The Shell Sort Algorithm is well known.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements:
      !>
      !>           99.D0
      !>           33.D0
      !>           55.D0
      !>           44.D0
      !>          -77.D0
      !>           66.D0
      !>
      !>     Then after a call to SHELLD, the array would be ordered as
      !>     follows:
      !>
      !>          -77.D0
      !>           33.D0
      !>           44.D0
      !>           55.D0
      !>           66.D0
      !>           99.D0
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
      !>-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine shelld ( ndim, array )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of the array.
      real(wp), intent(inout) :: array ( * )  !! The array.

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

!
!     This is a straightforward implementation of the Shell Sort
!     algorithm.
!
      gap = ndim / 2

      do while ( gap > 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j > 0 )
               jg = j + gap

               if ( array(j) <= array(jg) ) then
                  j = 0
               else
                  call swapd ( array(j), array(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine shelld

      !> Sort an integer array using the Shell Sort algorithm.
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the number of elements in the array to be sorted.
      !>
      !>     ARRAY    on input, is the array to be sorted.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, contains the same elements, sorted
      !>              in increasing order. The actual sorting is done
      !>              in place in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDIM < 2, this routine does not modify the array.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The Shell Sort Algorithm is well known.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements:
      !>
      !>           99
      !>           33
      !>           55
      !>           44
      !>          -77
      !>           66
      !>
      !>     Then after a call to SHELLI, the array would be ordered as
      !>     follows:
      !>
      !>          -77
      !>           33
      !>           44
      !>           55
      !>           66
      !>           99
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
      !>-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added entry #1 in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine shelli ( ndim, array )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of the array.
      integer(ip), intent(inout) :: array ( * )  !! The array.

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

!
!     This is a straightforward implementation of the Shell Sort
!     algorithm.
!
      gap = ndim / 2

      do while ( gap > 0 )

         do i = gap+1, ndim

            j = i - gap
            do while ( j > 0 )
               jg = j + gap

               if ( array(j) <= array(jg) ) then
                  j = 0
               else
                  call swapi ( array(j), array(jg) )
               end if

               j = j - gap
            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine shelli

      !> Determine if some of the entries in an array of logicals are
      !> .FALSE.
      !>
      !> The function returns .TRUE. if some of the values in the array
      !> LOGCLS are .FALSE.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (ALL instead).
      !>
      !>## Detailed Input
      !>
      !>     LOGCLS   is an array of logicals.
      !>
      !>     N        is the number of elements in the array LOGCLS
      !>
      !>## Detailed Output
      !>
      !>     The function returns true if the value of some entry of LOGCLS
      !>     is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than 1, the function returns a value of .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function examines each element of LOGCLS until
      !>     a .FALSE. value is found or until all values have been
      !>     examined.
      !>
      !>## Examples
      !>
      !>     Suppose you need to confirm that a character set
      !>     WORDS does not contain at least one of the words in the phrase
      !>
      !>       'EVERY GOOD BOY DOES FINE'
      !>
      !>     You might execute the following block of code.
      !>
      !>           FOUND(1) = ELEMC  ( 'EVERY', WORDS )
      !>           FOUND(2) = ELEMC  ( 'GOOD',  WORDS )
      !>           FOUND(3) = ELEMC  ( 'BOY',   WORDS )
      !>           FOUND(4) = ELEMC  ( 'DOES',  WORDS )
      !>           FOUND(5) = ELEMC  ( 'FINE',  WORDS )
      !>
      !>           OK       = SOMFLS ( FOUND,   5     )
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
      !>-    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT)
      pure logical function somfls ( logcls, n )
      use pepper_kinds, only: ip
      implicit none

      logical, intent(in) :: logcls ( * )  !! An array of logicals.
      integer(ip), intent(in) :: n  !! Number of elements in the array LOGCLS.

!
!     Just do it.
!
      somfls = .not. all(logcls(1:n))
      end function somfls

      !> Determine if some of the entries in an array of logicals are
      !> .TRUE.
      !>
      !> The function returns .TRUE. if some of the values in the array
      !> LOGCLS are .TRUE.
      !>
      !>## Detailed Input
      !>
      !>     LOGCLS   is an array of logicals.
      !>
      !>     N        is the number of elements in the array LOGCLS
      !>
      !>## Detailed Output
      !>
      !>     The function returns true if the value of some entry of LOGCLS
      !>     is .TRUE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If N is less than 1, the function returns a value of .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function examines each element of LOGCLS until
      !>     a .TRUE. value is found or until all values have been
      !>     examined.
      !>
      !>## Examples
      !>
      !>     Suppose you needed to confirm that a character set
      !>     WORDS contained at least one of the words in the phrase
      !>
      !>       'EVERY GOOD BOY DOES FINE'
      !>
      !>     You might execute the following block of code.
      !>
      !>           FOUND(1) = ELEMC  ( 'EVERY', WORDS )
      !>           FOUND(2) = ELEMC  ( 'GOOD',  WORDS )
      !>           FOUND(3) = ELEMC  ( 'BOY',   WORDS )
      !>           FOUND(4) = ELEMC  ( 'DOES',  WORDS )
      !>           FOUND(5) = ELEMC  ( 'FINE',  WORDS )
      !>
      !>           OK       = SOMTRU ( FOUND,   5     )
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
      !>-    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT)
      pure logical function somtru ( logcls, n )
      use pepper_kinds, only: ip
      implicit none

      logical, intent(in) :: logcls ( * )  !! An array of logicals.
      integer(ip), intent(in) :: n  !! Number of elements in the array LOGCLS.

      integer(ip) :: i

!
!     Just do it.
!
      do i = 1, n

         if ( logcls(i) ) then

            somtru = .true.
            return

         end if

      end do

      somtru = .false.
      return

      end function somtru

      !> Return the sum of the elements of a double precision array.
      !>
      !> The function returns the sum of the elements of ARRAY.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input double precision array.
      !>
      !>     N        is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the sum of the elements of the input array.
      !>     That is,
      !>
      !>        SUMAD( ARRAY, N ) = ARRAY(1) + ARRAY(2) + ... + ARRAY(N)
      !>
      !>     If N is zero or negative, SUMAD is zero.
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
      !>     The value of the function is initially set to zero. The elements
      !>     of the array are then added. If the number of elements is zero or
      !>     negative, SUMAD is zero.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           ARRAY(1) = 12.D0
      !>           ARRAY(2) =  1.D0
      !>           ARRAY(3) =  4.D0
      !>           ARRAY(4) = 75.D0
      !>           ARRAY(5) = 18.D0
      !>
      !>     Then
      !>
      !>           SUMAD ( ARRAY,   -3 )       =   0.D0
      !>           SUMAD ( ARRAY,    0 )       =   0.D0
      !>           SUMAD ( ARRAY,    1 )       =  12.D0
      !>           SUMAD ( ARRAY,    2 )       =  13.D0
      !>           SUMAD ( ARRAY,    5 )       = 110.D0
      !>           SUMAD ( ARRAY(3), 3 )       =  97.D0
      !>
      !>## Restrictions
      !>
      !>     1)  SUMAD does not check for overflow.
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
      !>-    SPICELIB Version 1.1.0, 09-APR-2021 (JDR)
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
      pure real(wp) function sumad ( array, n )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: array  ( * )  !! Input array.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.

      real(wp) :: sum
      integer(ip) :: i

!
!     Begin at zero.
!
      sum = 0.0_wp

!
!     Sum the elements. If N is zero or negative, nothing happens.
!
      do i = 1, n
         sum = sum + array(i)
      end do

!
!     Return the sum.
!
      sumad = sum

      return
      end function sumad

      !> Return the sum of the elements of an integer array.
      !>
      !> The function returns the sum of the elements of ARRAY.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (SUM instead).
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the input integer array.
      !>
      !>     N        is the number of elements in the array.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the sum of the elements of the input array.
      !>     That is,
      !>
      !>        SUMAI( ARRAY, N ) = ARRAY(1) + ARRAY(2) + ... + ARRAY(N)
      !>
      !>     If N is zero or negative, SUMAI is zero.
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
      !>     The value of the function is initially set to zero. The elements
      !>     of the array are then added. If the number of elements is zero or
      !>     negative, SUMAI is zero.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           ARRAY(1) = 12
      !>           ARRAY(2) =  1
      !>           ARRAY(3) =  4
      !>           ARRAY(4) = 75
      !>           ARRAY(5) = 18
      !>
      !>     Then
      !>
      !>           SUMAI ( ARRAY,   -3 )       =   0
      !>           SUMAI ( ARRAY,    0 )       =   0
      !>           SUMAI ( ARRAY,    1 )       =  12
      !>           SUMAI ( ARRAY,    2 )       =  13
      !>           SUMAI ( ARRAY,    5 )       = 110
      !>           SUMAI ( ARRAY(3), 3 )       =  97
      !>
      !>## Restrictions
      !>
      !>     1)  SUMAI does not check for overflow.
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
      !>-    SPICELIB Version 1.1.0, 09-APR-2021 (JDR)
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
      pure integer(ip) function sumai ( array, n )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: array  ( * )  !! Input array.
      integer(ip), intent(in) :: n  !! Number of elements in ARRAY.

!
!     Begin at zero.
!
      sumai = sum(array(1:n))
      end function sumai

      !> Swap the contents of two character strings.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are two character strings, the contents of which
      !>              are to be swapped (exchanged).
      !>
      !>## Detailed Output
      !>
      !>     A,
      !>     B        are the same two character strings, after their
      !>              contents have been exchanged.
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
      !>     This is just shorthand notation for the code fragment
      !>
      !>           TEMP = A
      !>           A    = B
      !>           B    = TEMP
      !>
      !>     The characters in the string are swapped one at a time, so
      !>     no intermediate string (TEMP) is needed. This means that the
      !>     strings may be of any length.
      !>
      !>## Examples
      !>
      !>     Let
      !>           A = 11.D0
      !>           B = 22.D0
      !>
      !>     Then after calling SWAPD (A,B),
      !>
      !>           A = 22.D0
      !>           B = 11.D0
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental subroutine swapc ( a, b )
      use pepper_kinds, only: ip
      implicit none

      character(*), intent(inout) :: a  !! First string.
      character(*), intent(inout) :: b  !! Second string.

      integer(ip) :: alen
      integer(ip) :: blen
      integer(ip) :: short

      character(1) :: temp
      integer(ip) :: i

!
!     Get the lengths of the strings.
!
      alen  = len ( a )
      blen  = len ( b )
      short = min ( alen, blen )

!
!     Keep going until the end of the shorter string is reached.
!
      do i = 1, short

         temp   = a(i:i)
         a(i:i) = b(i:i)
         b(i:i) = temp

      end do

!
!     If either string is longer than the shortest one, pad it
!     with blanks.
!
      if ( alen > short ) then
         a(short+1: ) = ' '

      else if ( blen > short ) then
         b(short+1: ) = ' '
      end if

      return
      end subroutine swapc

      !> Swap the contents of two double precision variables.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are two variables, the contents of which are to
      !>              be swapped (exchanged).
      !>
      !>## Detailed Output
      !>
      !>     A,
      !>     B        are the same two variables, after their contents
      !>              have been exchanged.
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
      !>     This is just shorthand notation for the code fragment
      !>
      !>           TEMP = A
      !>           A    = B
      !>           B    = TEMP
      !>
      !>## Examples
      !>
      !>     Let
      !>           A = 11.D0
      !>           B = 22.D0
      !>
      !>     Then after calling SWAPD (A,B),
      !>
      !>           A = 22.D0
      !>           B = 11.D0
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
      !>-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
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
      elemental subroutine swapd ( a, b )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(inout) :: a  !! First variable.
      real(wp), intent(inout) :: b  !! Second variable.

      real(wp) :: temp

!
!     What is there to say?
!
      temp = a
      a    = b
      b    = temp

      return
      end subroutine swapd

      !> Swap the contents of two integer variables.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B        are two variables, the contents of which are to
      !>              be swapped (exchanged).
      !>
      !>## Detailed Output
      !>
      !>     A,
      !>     B        are the same two variables, after their contents
      !>              have been exchanged.
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
      !>     This is just shorthand notation for the code fragment
      !>
      !>           TEMP = A
      !>           A    = B
      !>           B    = TEMP
      !>
      !>## Examples
      !>
      !>     Let
      !>           A = 11
      !>           B = 22
      !>
      !>     Then after calling SWAPI (A,B),
      !>
      !>           A = 22
      !>           B = 11
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
      !>-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
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
      elemental subroutine swapi ( a, b )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(inout) :: a  !! First variable.
      integer(ip), intent(inout) :: b  !! Second variable.

      integer(ip) :: temp

!
!     What is there to say?
!
      temp = a
      a    = b
      b    = temp

      return
      end subroutine swapi

      !> Return the first character of a string
      !>
      !> The function returns the first character of the string.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is any character string
      !>
      !>## Detailed Output
      !>
      !>     The function returns the first character of the string
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
      !>     This is a utility routine so that formal arguments to a routine
      !>     that are never used can be given the appearance of being used
      !>     to a compiler. In this way it is possible to leave "hooks" in
      !>     a calling sequence even if those variables are for the moment
      !>     unused. Similarly, variables declared for future use can be left
      !>     in place so that they don't need to be commented out
      !>
      !>## Examples
      !>
      !>     Suppose that a routine takes as an argument a
      !>     fortran structure implemented as a set of parallel arrays.
      !>     But that one of the arrays is not needed for the purposes of
      !>     the routine. This routine allows you to touch that array
      !>     without changing it.
      !>
      !>        SUBROUTINE INCPTR ( N, PTR, VALUES )
      !>
      !>        This routine increments the current pointer into a circular
      !>        array of character strings.
      !>
      !>        INTEGER               N
      !>        INTEGER               PTR
      !>        CHARACTER*(*)         VALUES ( * )
      !>
      !>        Even though we don't need to do anything with the values
      !>        array, it's passed for the sake of uniformity in calling
      !>        sequences. Touch the VALUES array so that the compiler
      !>        will think it's been used.
      !>
      !>        VALUES(1)(1:1) = TOUCHC ( VALUES(1)(1:1) )
      !>
      !>        PTR = PTR + 1
      !>
      !>        IF ( PTR .GT. N ) THEN
      !>           PTR = 1
      !>        END IF
      !>        RETURN
      !>
      !>## Restrictions
      !>
      !>     1)  If you use this routine, it would be a very good idea to
      !>         write down why you are using it in the calling routine.
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
      !>-    SPICELIB Version 1.0.1, 16-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 06-MAY-1994 (WLT)
      elemental character function touchc ( string )
      implicit none

      character(*), intent(in) :: string  !! any character string

      touchc = string(1:1)
      return
      end function touchc

      !> Return the value of a double precision number.
      !>
      !> The function returns the value of DP.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (the value itself instead).
      !>
      !>## Detailed Input
      !>
      !>     DP       is any double precision number
      !>
      !>## Detailed Output
      !>
      !>     The function returns the input d.p.
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
      !>     This is a utility routine so that formal arguments to a routine
      !>     that are never used can be given the appearance of being used
      !>     to a compiler. In this way it is possible to leave "hooks" in
      !>     a calling sequence even if those variables are for the moment
      !>     unused. Similarly, variables declared for future use can be left
      !>     in place so that they don't need to be commented out
      !>
      !>## Examples
      !>
      !>     Suppose that a routine takes as an argument a
      !>     fortran structure implemented as a set of parallel arrays.
      !>     But that one of the arrays is not needed for the purposes of
      !>     the routine. This routine allows you to touch that array
      !>     without changing it.
      !>
      !>        SUBROUTINE INCPTR ( N, PTR, VALUES )
      !>
      !>        This routine increments the current pointer into a circular
      !>        array of double precision numbers.
      !>
      !>        INTEGER               N
      !>        INTEGER               PTR
      !>        DOUBLE PRECISION      VALUES ( * )
      !>
      !>        Even though we don't need to do anything with the values
      !>        array, it's passed for the sake of uniformity in calling
      !>        sequences. Touch the VALUES array so that the compiler
      !>        will think it's been used.
      !>
      !>        VALUES(1) = TOUCHD ( VALUES(1) )
      !>
      !>        PTR = PTR + 1
      !>
      !>        IF ( PTR .GT. N ) THEN
      !>           PTR = 1
      !>        END IF
      !>        RETURN
      !>
      !>## Restrictions
      !>
      !>     1)  If you use this routine, it would be a very good idea to
      !>         write down why you are using it in the calling routine.
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
      !>-    SPICELIB Version 1.0.1, 16-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 06-MAY-1994 (WLT)
      elemental real(wp) function touchd ( dp )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: dp  !! any double precision number

      touchd = dp
      end function touchd

      !> Return the value of the input integer
      !>
      !> The function returns the value of INT.
      !>
      !>## Detailed Input
      !>
      !>     INT      is any integer
      !>
      !>## Detailed Output
      !>
      !>     The function returns the input integer
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
      !>     This is a utility routine so that formal arguments to a routine
      !>     that are never used can be given the appearance of being used
      !>     to a compiler. In this way it is possible to leave "hooks" in
      !>     a calling sequence even if those variables are for the moment
      !>     unused. Similarly, variables declared for future use can be left
      !>     in place so that they don't need to be commented out
      !>
      !>## Examples
      !>
      !>     Suppose that a routine takes as an argument a
      !>     fortran structure implemented as a set of parallel arrays.
      !>     But that one of the arrays is not needed for the purposes of
      !>     the routine. This routine allows you to touch that array
      !>     without changing it.
      !>
      !>        SUBROUTINE INCPTR ( N, PTR, VALUES )
      !>
      !>        This routine increments the current pointer into a circular
      !>        array of  integers.
      !>
      !>        INTEGER               N
      !>        INTEGER               PTR
      !>        INTEGER               VALUES ( * )
      !>
      !>        Even though we don't need to do anything with the values
      !>        array, it's passed for the sake of uniformity in calling
      !>        sequences. Touch the VALUES array so that the compiler
      !>        will think it's been used.
      !>
      !>        VALUES(1) = TOUCHI ( VALUES(1) )
      !>
      !>        PTR = PTR + 1
      !>
      !>        IF ( PTR .GT. N ) THEN
      !>           PTR = 1
      !>        END IF
      !>        RETURN
      !>
      !>## Restrictions
      !>
      !>     1)  If you use this routine, it would be a very good idea to
      !>         write down why you are using it in the calling routine.
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
      !>-    SPICELIB Version 1.0.1, 16-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 06-MAY-1994 (WLT)
      elemental integer(ip) function touchi ( int )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: int  !! any integer

      touchi = int
      return
      end function touchi

      !> Return the value of the input logical
      !>
      !> The function returns the value of LOG.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (the value itself instead).
      !>
      !>## Detailed Input
      !>
      !>     LOG      is any logical
      !>
      !>## Detailed Output
      !>
      !>     The function returns the input logical
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
      !>     This is a utility routine so that formal arguments to a routine
      !>     that are never used can be given the appearance of being used
      !>     to a compiler. In this way it is possible to leave "hooks" in
      !>     a calling sequence even if those variables are for the moment
      !>     unused. Similarly, variables declared for future use can be left
      !>     in place so that they don't need to be commented out
      !>
      !>## Examples
      !>
      !>     Suppose that a routine takes as an argument a
      !>     fortran structure implemented as a set of parallel arrays.
      !>     But that one of the arrays is not needed for the purposes of
      !>     the routine. This routine allows you to touch that array
      !>     without changing it.
      !>
      !>        SUBROUTINE INCPTR ( N, PTR, VALUES )
      !>
      !>        This routine increments the current pointer into a circular
      !>        array of logicals.
      !>
      !>        INTEGER               N
      !>        INTEGER               PTR
      !>        LOGICAL               VALUES ( * )
      !>
      !>        Even though we don't need to do anything with the values
      !>        array, it's passed for the sake of uniformity in calling
      !>        sequences. Touch the VALUES array so that the compiler
      !>        will think it's been used.
      !>
      !>        VALUES(1) = TOUCHL ( VALUES(1) )
      !>
      !>        PTR = PTR + 1
      !>
      !>        IF ( PTR .GT. N ) THEN
      !>           PTR = 1
      !>        END IF
      !>        RETURN
      !>
      !>## Restrictions
      !>
      !>     1)  If you use this routine, it would be a very good idea to
      !>         write down why you are using it in the calling routine.
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
      !>-    SPICELIB Version 1.0.1, 16-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 06-MAY-1994 (WLT)
      elemental logical function touchl ( log )
      implicit none

      logical, intent(in) :: log  !! any logical

      touchl = log
      end function touchl

end module pepper_arrays
