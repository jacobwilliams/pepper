!> Array operations that check their inputs (signal errors): cycling, inserting, removing, swapping
!> and packing elements; doubly linked lists in an integer pool (lnk*).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_array_ops

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   !
   ! The control area contains 3 elements.  They are:
   !
   !    The "size" of the pool, that is, the number
   !    of nodes in the pool.
   !
   !    The number of free nodes in the pool.
   !
   !    The "free pointer," which is the column index of the first free
   !    node.
   !
   ! Parameters defining the row and column indices of these control
   ! elements are given below.
   !
   ! Each assigned node consists of a backward pointer and a forward
   ! pointer.
   !
   !    +-------------+       +-------------+       +-------------+
   !    |  forward--> |       |  forward--> |       |  forward--> |
   !    +-------------+  ...  +-------------+  ...  +-------------+
   !    | <--backward |       | <--backward |       | <--backward |
   !    +-------------+       +-------------+       +-------------+
   !
   !        node 1                 node I              node SIZE
   integer(ip), parameter :: bckwrd = 2  !! row of the backward pointers of a linked list pool
   integer(ip), parameter :: forwrd = 1  !! row of the forward pointers of a linked list pool
   integer(ip), parameter :: frecol = -1  !! column of the head of the free list (in row FREROW)
   ! Free nodes say that that's what they are.  The way they say it
   ! is by containing the value FREE in their backward pointers.
   ! Needless to say, FREE is a value that cannot be a valid pointer.
   integer(ip), parameter :: free = 0  !! backward pointer of a node that is free (not in a list)
   integer(ip), parameter :: frerow = 1  !! row of the head of the free list (in column FRECOL)
   integer(ip), parameter :: nfrcol = 0  !! column of the number of free nodes (in row NFRROW)
   integer(ip), parameter :: nfrrow = 2  !! row of the number of free nodes (in column NFRCOL)
   integer(ip), parameter :: sizcol = 0  !! column of the pool's size (in row SIZROW)
   integer(ip), parameter :: sizrow = 1  !! row of the pool's size (in column SIZCOL)

contains

      !> Cycle the elements of a character array forward or backward
      !> in place.
      !>
      !>## Detailed Input
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>     DIR      is the direction in which the elements in the
      !>              array are to be cycled.
      !>
      !>                 'F' or 'f'  to cycle forward.
      !>                 'B' or 'b'  to cycle backward.
      !>
      !>     NCYCLE   is the number of times the elements in the array
      !>              are to be cycled.
      !>
      !>     ARRAY    is the array to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is the input array after it has been cycled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of DIR is not recognized, the error
      !>         SPICE(INVALIDDIRECTION) is signaled.
      !>
      !>     2)  If NELT is less than 1, the output array is not modified.
      !>
      !>     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
      !>         the opposite direction of DIR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine cycles a character array in place. To cycle
      !>     an array and store the result in a new array, use CYCLAC.
      !>
      !>     An array is cycled when its contents are shifted forward or
      !>     backward by one place. An element pushed off one end of the
      !>     array is brought around to the other end of the array instead
      !>     of disappearing.
      !>
      !>## Examples
      !>
      !>     Let the integer array A contain the following elements.
      !>
      !>        A(1) = 'apple'
      !>        A(2) = 'bear'
      !>        A(3) = 'cake'
      !>        A(4) = 'dragon'
      !>
      !>     Cycling A forward once yields the array
      !>
      !>        A(1) = 'dragon'
      !>        A(2) = 'apple'
      !>        A(3) = 'bear'
      !>        A(4) = 'cake'
      !>
      !>     Cycling A backward once yields the array
      !>
      !>        A(1) = 'bear'
      !>        A(2) = 'cake'
      !>        A(3) = 'dragon'
      !>        A(4) = 'apple'
      !>
      !>     Cycling by any multiple of the number of elements in the array
      !>     yields the same array.
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
      !>-    SPICELIB Version 1.0.0, 09-SEP-2005 (NJB) (HAN) (WLT) (IMU)
      module subroutine cyacip ( ctx, nelt, dir, ncycle, array )
      use pepper_kinds, only: ip
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nelt  !! Number of elements.
      character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      character(*), intent(inout) :: array    ( * )  !! Array to be cycled/cycled array.

      integer(ip) :: outlen
      integer(ip) :: widest

      character(1) :: last
      character(1) :: temp

      integer(ip) :: c
      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!     If the cycling direction is not recognized, signal an error.
!
      if (          ( dir == 'B' ) &
               .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'f' ) ) then

         k = mod (   ncycle, nelt )

      else

         call ctx%setmsg ( 'Cycling direction was *.' )
         call ctx%errch  ( '*', dir )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if

      if ( k < 0 ) then

         k = k + nelt

      else if ( k == 0 ) then

         return

      end if

!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
!     The character version of CYCLAx differs from the other
!     versions in that a single character is cycled at a time. That
!     is, the first trip through the outermost loop cycles the first
!     characters of the array elements; the second trip cycles the
!     second characters; and so on. This allows the same algorithm to
!     be used for all the routines. The local storage required is just
!     a couple of characters.
!
!     Don't swap the ends of strings if they're just blank padded.
!     And don't overwrite the elements of the output array, if they
!     happen to be shorter than those in the input array.
!
      outlen = len   ( array(1)       )
      widest = max(0, maxval(len_trim(array(1:nelt))))

!
!     The greatest common divisor need only be computed once.
!
      g = gcd ( k, nelt )
      m = nelt / g

!
!     To make this a non-character routine, remove all references to C.
!
      do c = 1, widest

         do i = 1, g

            l    = i
            last = array(l)(c:c)

            do j = 1, m

               l = l + k

               if ( l > nelt ) then
                  l = l - nelt
               end if

               temp          = array(l)(c:c)
               array(l)(c:c) = last
               last          = temp

            end do

         end do

      end do

!
!     If needed, pad the output array with blanks.
!
      if ( outlen > widest ) then

         do i = 1, nelt
            array(i)(widest+1: ) = ' '
         end do

      end if

      return
      end subroutine cyacip

      !> Cycle the elements of a double precision array forward
      !> or backward in place.
      !>
      !>## Detailed Input
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>     DIR      is the direction in which the elements in the
      !>              array are to be cycled.
      !>
      !>                 'F' or 'f'  to cycle forward.
      !>                 'B' or 'b'  to cycle backward.
      !>
      !>     NCYCLE   is the number of times the elements in the array
      !>              are to be cycled.
      !>
      !>     ARRAY    is the array to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is the input array after it has been cycled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of DIR is not recognized, the error
      !>         SPICE(INVALIDDIRECTION) is signaled.
      !>
      !>     2)  If NELT is less than 1, the output array is not modified.
      !>
      !>     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
      !>         the opposite direction of DIR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine cycles a double precision array in place. To cycle
      !>     an array and store the result in a new array, use CYCLAD.
      !>
      !>     An array is cycled when its contents are shifted forward or
      !>     backward by one place. An element pushed off one end of the array
      !>     is brought around to the other end of the array instead of
      !>     disappearing.
      !>
      !>## Examples
      !>
      !>     Let the double precision A contain the following elements.
      !>
      !>        A(1) = 1.D0
      !>        A(2) = 2.D0
      !>        A(3) = 3.D0
      !>        A(4) = 4.D0
      !>
      !>     Cycling A forward once yields the array
      !>
      !>        A(1) = 4.D0
      !>        A(2) = 1.D0
      !>        A(3) = 2.D0
      !>        A(4) = 3.D0
      !>
      !>     Cycling A backward once yields the array
      !>
      !>        A(1) = 2.D0
      !>        A(2) = 3.D0
      !>        A(3) = 4.D0
      !>        A(4) = 1.D0
      !>
      !>     Cycling by any multiple of the number of elements in the array
      !>     yields the same array.
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
      !>-    SPICELIB Version 1.0.0, 09-SEP-2005 (NJB) (HAN) (WLT) (IMU)
      module subroutine cyadip ( ctx, nelt, dir, ncycle, array )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nelt  !! Number of elements.
      character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      real(wp), intent(inout) :: array    ( * )  !! Array to be cycled/cycled array.

      real(wp) :: last
      real(wp) :: temp

      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!
      if (          ( dir == 'B' ) &
               .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'F' ) ) then

         k = mod (   ncycle, nelt )

      else

         call ctx%setmsg ( 'Cycling direction was *.' )
         call ctx%errch  ( '*', dir                   )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if

      if ( k < 0 ) then

         k = k + nelt

      else if ( k == 0 ) then

         return

      end if

!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
      g = gcd ( k, nelt )
      m = nelt / g

      do i = 1, g

         l    = i
         last = array(l)

         do j = 1, m

            l = l + k

            if ( l > nelt ) then
               l = l - nelt
            end if

            temp      = array(l)
            array(l)  = last
            last      = temp

         end do

      end do

      return
      end subroutine cyadip

      !> Cycle the elements of an integer array forward or backward
      !> in place.
      !>
      !>## Detailed Input
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>     DIR      is the direction in which the elements in the
      !>              array are to be cycled.
      !>
      !>                 'F' or 'f'  to cycle forward.
      !>                 'B' or 'b'  to cycle backward.
      !>
      !>     NCYCLE   is the number of times the elements in the array
      !>              are to be cycled.
      !>
      !>     ARRAY    is the array to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    is the input array after it has been cycled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of DIR is not recognized, the error
      !>         SPICE(INVALIDDIRECTION) is signaled.
      !>
      !>     2)  If NELT is less than 1, the output array is not modified.
      !>
      !>     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
      !>         the opposite direction of DIR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine cycles an integer array in place. To cycle
      !>     an array and store the result in a new array, use CYCLAI.
      !>
      !>     An array is cycled when its contents are shifted forward or
      !>     backward by one place. An element pushed off one end of the
      !>     array is brought around to the other end of the array instead
      !>     of disappearing.
      !>
      !>## Examples
      !>
      !>     Let the integer array A contain the following elements.
      !>
      !>        A(1) = 1
      !>        A(2) = 2
      !>        A(3) = 3
      !>        A(4) = 4
      !>
      !>     Cycling A forward once yields the array
      !>
      !>        A(1) = 4
      !>        A(2) = 1
      !>        A(3) = 2
      !>        A(4) = 3
      !>
      !>     Cycling A backward once yields the array
      !>
      !>        A(1) = 2
      !>        A(2) = 3
      !>        A(3) = 4
      !>        A(4) = 1
      !>
      !>     Cycling by any multiple of the number of elements in the array
      !>     yields the same array.
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
      !>-    SPICELIB Version 1.0.0, 09-SEP-2005 (NJB) (HAN) (WLT) (IMU)
      module subroutine cyaiip ( ctx, nelt, dir, ncycle, array )
      use pepper_kinds, only: ip
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nelt  !! Number of elements.
      character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      integer(ip), intent(inout) :: array    ( * )  !! Array to be cycled/cycled array.

      integer(ip) :: last
      integer(ip) :: temp

      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!
      if (      ( dir == 'B' ) &
           .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'F' ) ) then

         k = mod (   ncycle, nelt )

      else

         call ctx%setmsg ( 'Cycling direction was *.' )
         call ctx%errch  ( '*', dir                   )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if

      if ( k < 0 ) then

         k = k + nelt

      else if ( k == 0 ) then

         return

      end if

!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
      g = gcd ( k, nelt )
      m = nelt / g

      do i = 1, g

         l    = i
         last = array(l)

         do j = 1, m

            l = l + k

            if ( l > nelt ) then
               l = l - nelt
            end if

            temp     = array(l)
            array(l) = last
            last     = temp

         end do

      end do

      return
      end subroutine cyaiip

      !> Cycle the elements of a character array forward or backward.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the array to be cycled.
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>     DIR      is the direction in which the elements in the
      !>              array are to be cycled.
      !>
      !>                    'F' or 'f'  to cycle forward.
      !>                    'B' or 'b'  to cycle backward.
      !>
      !>     NCYCLE   is the number of times the elements in the array
      !>              are to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the input array after it has been cycled.
      !>              OUT may overwrite ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of DIR is not recognized, the error
      !>         SPICE(INVALIDDIRECTION) is signaled.
      !>
      !>     2)  If NELT is less than 1, the output array is not modified.
      !>
      !>     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
      !>         the opposite direction of DIR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     An array is cycled when its contents are shifted forward or
      !>     backward by one place. An element pushed off one end of the
      !>     array is brought around to the other end of the array instead
      !>     of disappearing.
      !>
      !>## Examples
      !>
      !>     Let the integer array A contain the following elements.
      !>
      !>           A(1) = 'apple'
      !>           A(2) = 'bear'
      !>           A(3) = 'cake'
      !>           A(4) = 'dragon'
      !>
      !>     Cycling A forward once yields the array
      !>
      !>           A(1) = 'dragon'
      !>           A(2) = 'apple'
      !>           A(3) = 'bear'
      !>           A(4) = 'cake'
      !>
      !>     Cycling A backward once yields the array
      !>
      !>           A(1) = 'bear'
      !>           A(2) = 'cake'
      !>           A(3) = 'dragon'
      !>           A(4) = 'apple'
      !>
      !>     Cycling by any multiple of the number of elements in the array
      !>     yields the same array.
      !>
      !>## Restrictions
      !>
      !>     1)  The memory used for the output array must be identical to or
      !>         disjoint from the memory used for the input array.
      !>
      !>         That is:
      !>
      !>            CALL CYCLAC ( ARRAY, NELT, DIR, NCYCLE, ARRAY )
      !>
      !>         will produce correct results, while
      !>
      !>            CALL CYCLAC ( ARRAY, NELT-3, DIR, NCYCLE, ARRAY(4) )
      !>
      !>         will produce garbage.
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
      !>-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect an invalid value for
      !>         the cycling direction. If the direction is not recognized
      !>         the error SPICE(INVALIDDIRECTION) is signaled and the
      !>         output array is not modified. (The routine used to copy the
      !>         input array into the output array if the direction was not
      !>         recognized.)
      !>
      !>         The "Exceptions" section was filled out in more detail.
      module subroutine cyclac ( ctx, array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: array    ( * )  !! Input array.
      integer(ip), intent(in) :: nelt  !! Number of elements.
      character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      character(*), intent(out) :: out      ( * )  !! Cycled array.

      integer(ip) :: outlen
      integer(ip) :: widest
      integer(ip) :: limit

      character(1) :: last
      character(1) :: temp

      integer(ip) :: c
      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!     If the cycling direction is not recognized, signal an error.
!
      if (          ( dir == 'B' ) &
               .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'f' ) ) then

         k = mod (   ncycle, nelt )

      else

         call ctx%setmsg ( 'Cycling direction was *.' )
         call ctx%errch  ( '*', dir )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if

      if ( k < 0 ) then

         k = k + nelt

      else if ( k == 0 ) then

         out(1:nelt) = array(1:nelt)
         return

      end if

!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
!     The character version of CYCLAx differs from the other
!     versions in that a single character is cycled at a time. That
!     is, the first trip through the outermost loop cycles the first
!     characters of the array elements; the second trip cycles the
!     second characters; and so on. This allows the same algorithm to
!     be used for all the routines. The local storage required is just
!     a couple of characters.
!

!
!     Don't swap the ends of strings if they're just blank padded.
!     And don't overwrite the elements of the output array, if they
!     happen to be shorter thAn those in the input array.
!
      outlen = len   ( out(1)         )
      widest = max(0, maxval(len_trim(array(1:nelt))))
      limit  = min   ( outlen, widest )

!
!     The greatest common divisor need only be computed once.
!
      g = gcd ( k, nelt )
      m = nelt / g

!
!     To make this a non-character routine, remove all references to C.
!
      do c = 1, limit

         do i = 1, g

            l    = i
            last = array(l)(c:c)

            do j = 1, m

               l = l + k

               if ( l > nelt ) then
                  l = l - nelt
               end if

               temp        = array(l)(c:c)
               out(l)(c:c) = last
               last        = temp

            end do

         end do

      end do

!
!     If needed, pad the output array with blanks.
!
      if ( outlen > limit ) then

         do i = 1, nelt
            out(i)(limit+1: ) = ' '
         end do

      end if

      return
      end subroutine cyclac

      !> Cycle the elements of a double precision array forward
      !> or backward.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the array to be cycled.
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>     DIR      is the direction in which the elements in the
      !>              array are to be cycled.
      !>
      !>                 'F' or 'f'  to cycle forward.
      !>                 'B' or 'b'  to cycle backward.
      !>
      !>     NCYCLE   is the number of times the elements in the array
      !>              are to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the input array after it has been cycled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of DIR is not recognized, the error
      !>         SPICE(INVALIDDIRECTION) is signaled.
      !>
      !>     2)  If NELT is less than 1, the output array is not modified.
      !>
      !>     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
      !>         the opposite direction of DIR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     An array is cycled when its contents are shifted forward or
      !>     backward by one place. An element pushed off one end of the
      !>     array is brought around to the other end of the array instead
      !>     of disappearing.
      !>
      !>## Examples
      !>
      !>     Let the double precision A contain the following elements.
      !>
      !>        A(1) = 1.D0
      !>        A(2) = 2.D0
      !>        A(3) = 3.D0
      !>        A(4) = 4.D0
      !>
      !>     Cycling A forward once yields the array
      !>
      !>        A(1) = 4.D0
      !>        A(2) = 1.D0
      !>        A(3) = 2.D0
      !>        A(4) = 3.D0
      !>
      !>     Cycling A backward once yields the array
      !>
      !>        A(1) = 2.D0
      !>        A(2) = 3.D0
      !>        A(3) = 4.D0
      !>        A(4) = 1.D0
      !>
      !>     Cycling by any multiple of the number of elements in the array
      !>     yields the same array.
      !>
      !>## Restrictions
      !>
      !>     1)  The memory used for the output array must be disjoint from the
      !>         memory used for the input array.
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
      !>-    SPICELIB Version 1.0.3, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect an invalid value for
      !>         the cycling direction. If the direction is not recognized
      !>         the error SPICE(INVALIDDIRECTION) is signaled and the
      !>         output array is not modified. (The routine used to copy the
      !>         input array into the output array if the direction was not
      !>         recognized.)
      !>
      !>         The "Exceptions" section was filled out in more detail.
      module subroutine cyclad ( ctx, array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: array    ( * )  !! Input array.
      integer(ip), intent(in) :: nelt  !! Number of elements.
      character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      real(wp), intent(out) :: out      ( * )  !! Cycled array.

      real(wp) :: last
      real(wp) :: temp

      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!
      if (          ( dir == 'B' ) &
               .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'F' ) ) then

         k = mod (   ncycle, nelt )

      else

         call ctx%setmsg ( 'Cycling direction was *.' )
         call ctx%errch  ( '*', dir )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if

      if ( k < 0 ) then
         k = k + nelt

      else if ( k == 0 ) then

         out(1:nelt) = array(1:nelt)
         return

      end if

!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
      g = gcd ( k, nelt )
      m = nelt / g

      do i = 1, g

         l    = i
         last = array(l)

         do j = 1, m

            l = l + k

            if ( l > nelt ) then
               l = l - nelt
            end if

            temp    = array(l)
            out(l)  = last
            last    = temp

         end do

      end do

      return
      end subroutine cyclad

      !> Cycle the elements of an integer array forward or backward.
      !>
      !>## Detailed Input
      !>
      !>     ARRAY    is the array to be cycled.
      !>
      !>     NELT     is the number of elements in the input array.
      !>
      !>     DIR      is the direction in which the elements in the
      !>              array are to be cycled.
      !>
      !>                 'F' or 'f'  to cycle forward.
      !>                 'B' or 'b'  to cycle backward.
      !>
      !>     NCYCLE   is the number of times the elements in the array
      !>              are to be cycled.
      !>
      !>## Detailed Output
      !>
      !>     OUT      is the input array after it has been cycled.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of DIR is not recognized, the error
      !>         SPICE(INVALIDDIRECTION) is signaled.
      !>
      !>     2)  If NELT is less than 1, the output array is not modified.
      !>
      !>     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
      !>         the opposite direction of DIR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     An array is cycled when its contents are shifted forward or
      !>     backward by one place. An element pushed off one end of the
      !>     array is brought around to the other end of the array instead
      !>     of disappearing.
      !>
      !>## Examples
      !>
      !>     Let the integer array A contain the following elements.
      !>
      !>        A(1) = 1
      !>        A(2) = 2
      !>        A(3) = 3
      !>        A(4) = 4
      !>
      !>     Cycling A forward once yields the array
      !>
      !>        A(1) = 4
      !>        A(2) = 1
      !>        A(3) = 2
      !>        A(4) = 3
      !>
      !>     Cycling A backward once yields the array
      !>
      !>        A(1) = 2
      !>        A(2) = 3
      !>        A(3) = 4
      !>        A(4) = 1
      !>
      !>     Cycling by any multiple of the number of elements in the array
      !>     yields the same array.
      !>
      !>## Restrictions
      !>
      !>     1)  The memory used for the output array must be disjoint from the
      !>         memory used for the input array.
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
      !>-    SPICELIB Version 1.0.3, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect an invalid value for
      !>         the cycling direction. If the direction is not recognized
      !>         the error SPICE(INVALIDDIRECTION) is signaled and the
      !>         output array is not modified. (The routine used to copy the
      !>         input array into the output array if the direction was not
      !>         recognized.)
      !>
      !>         The "Exceptions" section was filled out in more detail.
      module subroutine cyclai ( ctx, array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip
      use pepper_math, only: gcd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: array    ( * )  !! Input array.
      integer(ip), intent(in) :: nelt  !! Number of elements.
      character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
      integer(ip), intent(out) :: out      ( * )  !! Cycled array.

      integer(ip) :: last
      integer(ip) :: temp

      integer(ip) :: g
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!
      if (      ( dir == 'B' ) &
           .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'F' ) ) then

         k = mod (   ncycle, nelt )

      else

         call ctx%setmsg ( 'Cycling direction was *.' )
         call ctx%errch  ( '*', dir )
         call ctx%sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if

      if ( k < 0 ) then
         k = k + nelt

      else if ( k == 0 ) then

         out(1:nelt) = array(1:nelt)
         return

      end if

!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
      g = gcd ( k, nelt )
      m = nelt / g

      do i = 1, g

         l    = i
         last = array(l)

         do j = 1, m

            l = l + k

            if ( l > nelt ) then
               l = l - nelt
            end if

            temp    = array(l)
            out(l)  = last
            last    = temp

         end do

      end do

      return
      end subroutine cyclai

      !> Insert one or more elements into a character array at the
      !> indicated location.
      !>
      !>## Detailed Input
      !>
      !>     ELTS     contains one or more elements which are to be
      !>              inserted into the input array.
      !>
      !>     NE       is the number of elements to be inserted.
      !>
      !>     LOC      is the location in the array at which the first
      !>              element of ELTS is to be inserted. LOC must be
      !>              within the interval [1, NA+1]. To append to
      !>              ARRAY, set LOC equal to NA+1.
      !>
      !>     ARRAY    on input, is the original array.
      !>
      !>     NA       on input, is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the original array with the elements
      !>              of ELT inserted into positions LOC through LOC+NE-1.
      !>              The original elements in these positions are moved
      !>              back to make room for the inserted elements. If the
      !>              new elements are longer than the declared lengths
      !>              of the elements of ARRAY, the new elements are
      !>              truncated on the right.
      !>
      !>     NA       on output, is the number of elements in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The dimension of the array is set equal to zero if its
      !>         input value is less than one.
      !>
      !>     2)  If LOC is not in the interval [1, NA+1], the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the number of elements to be inserted is less than one,
      !>         the array is not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The elements in positions LOC through LOC+NE-1 are moved back
      !>     by NE spaces to make room for the new elements, which are then
      !>     inserted into the vacated spaces.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>           ELTS(1) = 'very'       NA = 4      ARRAY(1) =  'I'
      !>           ELTS(2) = 'big'                    ARRAY(2) =  'saw'
      !>           ELTS(3) = 'brown'                  ARRAY(3) =  'a'
      !>                                              ARRAY(4) =  'dog'
      !>
      !>     Then the call
      !>
      !>           CALL INSLAC ( ELTS, 3, 4, ARRAY, NA )
      !>
      !>     yields the following result:
      !>
      !>           NA = 7      ARRAY(1) = 'I'
      !>                       ARRAY(2) = 'saw'
      !>                       ARRAY(3) = 'a'
      !>                       ARRAY(4) = 'very'
      !>                       ARRAY(5) = 'big'
      !>                       ARRAY(6) = 'brown'
      !>                       ARRAY(7) = 'dog'
      !>
      !>     The following calls to INSLAC signal errors.
      !>
      !>     CALL INSLAC ( ELTS, 3, -1, ARRAY, NA )
      !>     CALL INSLAC ( ELTS, 3,  6, ARRAY, NA )
      !>     CALL INSLAC ( ELTS, 3,  2, ARRAY, -1 )
      !>     CALL INSLAC ( ELTS, 3, -1, ARRAY, -1 )
      !>
      !>## Restrictions
      !>
      !>     1)  The array must be large enough to contain both the original
      !>         and the inserted elements.
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
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 30-DEC-1988 (HAN)
      !>
      !>         If the location at which the elements are to be inserted is
      !>         not in the interval [1, NA+1], an error is signaled.
      !>         Locations not within that interval refer to non-existent
      !>         array elements. (To append to the array, the location
      !>         should be equal to NA+1.)
      !>
      !>         A negative dimension bug was fixed. The results of the
      !>         old version were unpredictable if the input array dimension
      !>         was negative. To avoid this problem the maximum of zero and
      !>         the input dimension becomes the dimension used by the
      !>         the routine. In this case, the only valid location at which
      !>         to insert is 1. If it is not 1, an error is signaled
      !>         when the location is checked.
      module subroutine inslac ( ctx, elts, ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: elts  ( * )  !! Elements to be inserted.
      integer(ip), intent(in) :: ne  !! Number of elements to be inserted.
      integer(ip), intent(in) :: loc  !! Location of the first inserted element.
      character(*), intent(inout) :: array ( * )  !! Input/output array.
      integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.

!
!     Other functions
!
      integer(ip) :: max

      integer(ip) :: size
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check the dimension of the array.
!
      size = max ( 0, na )

!
!     Make sure the location at which the elements are to be inserted
!     is not out of range. If it is, signal an error and bail out.
!

      if ( ( loc < 1 ) .or. ( loc > size+1 ) ) then

         call ctx%setmsg ( 'Location was *.'     )
         call ctx%errint ( '*', loc              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      end if

!
!     If the number of elements to be inserted is greater than zero,
!     insert them. If not, do not modify the array.
!
      if ( ne > 0 ) then

!
!        Move the trailing elements back to make room for the new ones.
!
         do i = size, loc, -1
            array(i+ne) = array(i)
         end do

!
!        Now put the new elements in the vacated spaces.
!
         do i = 1, ne
            array(loc+i-1) = elts(i)
         end do

!
!        Update the number of elements in the array.
!
         na = size + ne

      end if

      return
      end subroutine inslac

      !> Insert one or more elements into a double precision array at
      !> the indicated location.
      !>
      !>## Detailed Input
      !>
      !>     ELTS     contains one or more elements which are to be
      !>              inserted into the input array.
      !>
      !>     NE       is the number of elements to be inserted.
      !>
      !>     LOC      is the location in the array at which the first
      !>              element of ELTS is to be inserted. LOC must be
      !>              within the interval [1, NA+1]. To append to
      !>              ARRAY, set LOC equal to NA+1.
      !>
      !>     ARRAY    on input, is the original array.
      !>
      !>     NA       on input, is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the original array with the elements
      !>              of ELT inserted into positions LOC through LOC+NE-1.
      !>              The original elements in these positions are moved
      !>              back to make room for the inserted elements.
      !>
      !>     NA       on output, is the number of elements in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The dimension of the array is set equal to zero if its
      !>         input value is less than one.
      !>
      !>     2)  If LOC is not in the interval [1, NA+1], the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the number of elements to be inserted is less than one,
      !>         the array is not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The elements in positions LOC through LOC+NE-1 are moved back
      !>     by NE spaces to make room for the new elements, which are then
      !>     inserted into the vacated spaces.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>           ELTS(1) = 5.0D0        NA = 4      ARRAY(1) =  1.0D0
      !>           ELTS(2) = 6.0D0                    ARRAY(2) =  2.0D0
      !>           ELTS(3) = 7.0D0                    ARRAY(3) =  3.0D0
      !>                                              ARRAY(4) =  4.0D0
      !>
      !>     Then the call
      !>
      !>           CALL INSLAD ( ELTS, 3, 3, ARRAY, NA )
      !>
      !>     yields the following result:
      !>
      !>           NA = 7      ARRAY(1) = 1.0D0
      !>                       ARRAY(2) = 2.0D0
      !>                       ARRAY(3) = 5.0D0
      !>                       ARRAY(4) = 6.0D0
      !>                       ARRAY(5) = 7.0D0
      !>                       ARRAY(6) = 3.0D0
      !>                       ARRAY(7) = 4.0D0
      !>
      !>     The following calls to INSLAD signal errors.
      !>
      !>     CALL INSLAD ( ELTS, 3, -1, ARRAY, NA )
      !>     CALL INSLAD ( ELTS, 3,  6, ARRAY, NA )
      !>     CALL INSLAD ( ELTS, 3,  2, ARRAY, -1 )
      !>     CALL INSLAD ( ELTS, 3, -1, ARRAY, -1 )
      !>
      !>## Restrictions
      !>
      !>     1)  The array must be large enough to contain both the original
      !>         and the inserted elements.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 30-DEC-1988 (HAN)
      !>
      !>         If the location at which the elements are to be inserted is
      !>         not in the interval [1, NA+1], an error is signaled.
      !>         Locations not within that interval refer to non-existent
      !>         array elements. (To append to the array, the location
      !>         should be equal to NA+1.)
      !>
      !>         A negative dimension bug was fixed. The results of the
      !>         old version were unpredictable if the input array dimension
      !>         was negative. To avoid this problem the maximum of zero and
      !>         the input dimension becomes the dimension used by the
      !>         the routine. In this case, the only valid location at which
      !>         to insert is 1. If it is not 1, an error is signaled
      !>         when the location is checked.
      module subroutine inslad ( ctx, elts, ne, loc, array, na )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: elts  ( * )  !! Elements to be inserted.
      integer(ip), intent(in) :: ne  !! Number of elements to be inserted.
      integer(ip), intent(in) :: loc  !! Location of the first inserted element.
      real(wp), intent(inout) :: array ( * )  !! Input/output array.
      integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.

!
!     Other functions
!
      integer(ip) :: max

      integer(ip) :: size
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check the dimension of the array.
!
      size = max ( 0, na )

!
!     Make sure the location at which the elements are to be inserted
!     is not out of range. If it is, signal an error and bail out.
!

      if ( ( loc < 1 ) .or. ( loc > size+1 ) ) then

         call ctx%setmsg ( 'Location was *.'     )
         call ctx%errint ( '*', loc              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      end if

!
!     If the number of elements to be inserted is greater than zero,
!     insert them. If not, do not modify the array.
!
      if ( ne > 0 ) then

!
!        Move the trailing elements back to make room for the new ones.
!
         do i = size, loc, -1
            array(i+ne) = array(i)
         end do

!
!        Now put the new elements in the vacated spaces.
!
         do i = 1, ne
            array(loc+i-1) = elts(i)
         end do

!
!        Update the number of elements in the array.
!
         na = size + ne

      end if

      return
      end subroutine inslad

      !> Insert one or more elements into an integer array at
      !> the indicated location.
      !>
      !>## Detailed Input
      !>
      !>     ELTS     contains one or more elements which are to be
      !>              inserted into the input array.
      !>
      !>     NE       is the number of elements to be inserted.
      !>
      !>     LOC      is the location in the array at which the first
      !>              element of ELTS is to be inserted. LOC must be
      !>              within the interval [1, NA+1]. To append to
      !>              ARRAY, set LOC equal to NA+1.
      !>
      !>     ARRAY    on input, is the original array.
      !>
      !>     NA       on input, is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the original array with the elements
      !>              of ELT inserted into positions LOC through LOC+NE-1.
      !>              The original elements in these positions are moved
      !>              back to make room for the inserted elements.
      !>
      !>     NA       on output, is the number of elements in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The dimension of the array is set equal to zero if its
      !>         input value is less than one.
      !>
      !>     2)  If LOC is not in the interval [1, NA+1], the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the number of elements to be inserted is less than one,
      !>         the array is not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The elements in positions LOC through LOC+NE-1 are moved back
      !>     by NE spaces to make room for the new elements, which are then
      !>     inserted into the vacated spaces.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>           ELTS(1) = 5        NA = 4      ARRAY(1) =  1
      !>           ELTS(2) = 6                    ARRAY(2) =  2
      !>           ELTS(3) = 7                    ARRAY(3) =  3
      !>                                          ARRAY(4) =  4
      !>
      !>     Then the call
      !>
      !>           CALL INSLAI ( ELTS, 3, 3, ARRAY, NA )
      !>
      !>     yields the following result:
      !>
      !>           NA = 7      ARRAY(1) = 1
      !>                       ARRAY(2) = 2
      !>                       ARRAY(3) = 5
      !>                       ARRAY(4) = 6
      !>                       ARRAY(5) = 7
      !>                       ARRAY(6) = 3
      !>                       ARRAY(7) = 4
      !>
      !>     The following calls to INSLAI signal errors.
      !>
      !>     CALL INSLAI ( ELTS, 3, -1, ARRAY, NA )
      !>     CALL INSLAI ( ELTS, 3,  6, ARRAY, NA )
      !>     CALL INSLAI ( ELTS, 3,  2, ARRAY, -1 )
      !>     CALL INSLAI ( ELTS, 3, -1, ARRAY, -1 )
      !>
      !>## Restrictions
      !>
      !>     1)  The array must be large enough to contain both the original
      !>         and the inserted elements.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 30-DEC-1988 (HAN)
      !>
      !>         If the location at which the elements are to be inserted is
      !>         not in the interval [1, NA+1], an error is signaled.
      !>         Locations not within that interval refer to non-existent
      !>         array elements. (To append to the array, the location
      !>         should be equal to NA+1.)
      !>
      !>         A negative dimension bug was fixed. The results of the
      !>         old version were unpredictable if the input array dimension
      !>         was negative. To avoid this problem the maximum of zero and
      !>         the input dimension becomes the dimension used by the
      !>         the routine. In this case, the only valid location at which
      !>         to insert is 1. If it is not 1, an error is signaled
      !>         when the location is checked.
      module subroutine inslai ( ctx, elts, ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: elts  ( * )  !! Elements to be inserted.
      integer(ip), intent(in) :: ne  !! Number of elements to be inserted.
      integer(ip), intent(in) :: loc  !! Location of the first inserted element.
      integer(ip), intent(inout) :: array ( * )  !! Input/output array.
      integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.

!
!     Other functions
!
      integer(ip) :: max

      integer(ip) :: size
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check the dimension of the array.
!
      size = max ( 0, na )

!
!     Make sure the location at which the elements are to be inserted
!     is not out of range. If it is, signal an error and bail out.
!

      if ( ( loc < 1 ) .or. ( loc > size+1 ) ) then

         call ctx%setmsg ( 'Location was *.'     )
         call ctx%errint ( '*', loc              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      end if

!
!     If the number of elements to be inserted is greater than zero,
!     insert them. If not, do not modify the array.
!
      if ( ne > 0 ) then

!
!        Move the trailing elements back to make room for the new ones.
!
         do i = size, loc, -1
            array(i+ne) = array(i)
         end do

!
!        Now put the new elements in the vacated spaces.
!
         do i = 1, ne
            array(loc+i-1) = elts(i)
         end do

!
!        Update the number of elements in the array.
!
         na = size + ne

      end if

      return
      end subroutine inslai

      !> Remove one or more elements from a character array at the
      !> indicated location.
      !>
      !>## Detailed Input
      !>
      !>     NE       is the number of elements to be removed.
      !>
      !>     LOC      is the location in the array at which the first
      !>              element is to be removed.
      !>
      !>     ARRAY    on input, is the original array.
      !>
      !>     NA       on input, is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the original array with elements
      !>              LOC through LOC+NE-1 removed. Succeeding elements
      !>              are moved forward to fill the vacated spaces.
      !>
      !>     NA       on output, is the number of elements in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LOC is not in the interval [1, NA], the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     2)  If the number of elements to be removed is greater than the
      !>         number of elements that can be removed, the error
      !>         SPICE(NONEXISTELEMENTS) is signaled.
      !>
      !>     3)  If NE is less than one, the array is not modified.
      !>
      !>     4)  If NA is less than one, any location is invalid, and, the
      !>         error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The elements in positions LOC through LOC+NE-1 are overwritten
      !>     as the elements beginning at LOC+NE are moved back.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>           NA = 7      ARRAY(1) = 'The'
      !>                       ARRAY(2) = 'boy'
      !>                       ARRAY(3) = 'in'
      !>                       ARRAY(4) = 'the'
      !>                       ARRAY(5) = 'park'
      !>                       ARRAY(6) = 'fell'
      !>                       ARRAY(7) = 'down'
      !>
      !>     Then the call
      !>
      !>           CALL REMLAC ( 3, 3, ARRAY, NA )
      !>
      !>     yields the following result:
      !>
      !>           NA = 4      ARRAY(1) = 'The'
      !>                       ARRAY(2) = 'boy'
      !>                       ARRAY(3) = 'fell'
      !>                       ARRAY(4) = 'down'
      !>
      !>     The following calls would signal errors:
      !>
      !>     CALL REMLAC ( 3,  1, ARRAY, -1 )
      !>     CALL REMLAC ( 3, -1, ARRAY,  7 )
      !>     CALL REMLAC ( 3,  6, ARRAY,  7 )
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
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 1-JAN-1989 (HAN)
      !>
      !>         Code was added to handle the following exceptional
      !>         inputs.
      !>
      !>         If the dimension of the array is less than one, any
      !>         value of LOC is invalid. The old version did not check
      !>         the dimension of the array, and as a result, its output
      !>         was unpredictable.
      !>
      !>         If the location at which the elements are to be removed is
      !>         not in the interval [1, NA], an error is signaled.
      !>         Locations not within that interval refer to non-existent
      !>         array elements. The old routine did not signal an error.
      !>         It just returned the original array.
      !>
      !>         If the number of elements to be removed is greater than the
      !>         number of elements can be removed, an error is signaled.
      !>         In the old version, only those elements that could be
      !>         removed were removed, and no error was signaled.
      module subroutine remlac ( ctx, ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ne  !! Number of elements to be removed.
      integer(ip), intent(in) :: loc  !! Location of the first removed element.
      character(*), intent(inout) :: array ( * )  !! Input/output array.
      integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If LOC does not point to an actual element, signal an error and
!     check out. If the dimension of the array is less than one, any
!     value of LOC is invalid, and an error is signaled.
!

      if ( ( loc < 1 ) .or. ( loc > na ) ) then

         call ctx%setmsg ( 'Location was *.'     )
         call ctx%errint ( '*', loc              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

!
!     Don't try to remove non-existent elements.
!

      else if ( ne > na-loc+1 ) then

         call ctx%setmsg ( 'Trying to remove non-existent elements.' )
         call ctx%sigerr ( 'SPICE(NONEXISTELEMENTS)' )
         return

!
!     If there are elements to be removed, remove them. Otherwise,
!     do not modify the array.
!

      else if ( ne > 0 ) then

!
!        Move the elements forward.
!
         do i = loc, na - ne
            array(i) = array(i+ne)
         end do

!
!        Update the number of elements in the array.
!
         na = na - ne

      end if

      return
      end subroutine remlac

      !> Remove one or more elements from a double precision array at the
      !> indicated location.
      !>
      !>## Detailed Input
      !>
      !>     NE       is the number of elements to be removed.
      !>
      !>     LOC      is the location in the array at which the first
      !>              element is to be removed.
      !>
      !>     ARRAY    on input, is the original array.
      !>
      !>     NA       on input, is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the original array with elements
      !>              LOC through LOC+NE-1 removed. Succeeding elements
      !>              are moved forward to fill the vacated spaces.
      !>
      !>     NA       on output, is the number of elements in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LOC is not in the interval [1, NA], the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     2)  If the number of elements to be removed is greater than the
      !>         number of elements that can be removed, the error
      !>         SPICE(NONEXISTELEMENTS) is signaled.
      !>
      !>     3)  If NE is less than one, the array is not modified.
      !>
      !>     4)  If NA is less than one, any location is invalid, and, the
      !>         error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The elements in positions LOC through LOC+NE-1 are overwritten
      !>     as the elements beginning at LOC+NE are moved back.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>           NA = 7      ARRAY(1) = 1.0D0
      !>                       ARRAY(2) = 2.0D0
      !>                       ARRAY(3) = 3.0D0
      !>                       ARRAY(4) = 4.0D0
      !>                       ARRAY(5) = 5.0D0
      !>                       ARRAY(6) = 6.0D0
      !>                       ARRAY(7) = 7.0D0
      !>
      !>     Then the call
      !>
      !>           CALL REMLAD ( 3, 3, ARRAY, NA )
      !>
      !>     yields the following result:
      !>
      !>           NA = 4      ARRAY(1) = 1.0D0
      !>                       ARRAY(2) = 2.0D0
      !>                       ARRAY(3) = 6.0D0
      !>                       ARRAY(4) = 7.0D0
      !>
      !>     The following calls would signal errors:
      !>
      !>     CALL REMLAD ( 3,  1, ARRAY, -1 )
      !>     CALL REMLAD ( 3, -1, ARRAY,  7 )
      !>     CALL REMLAD ( 3,  6, ARRAY,  7 )
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 1-JAN-1989 (HAN)
      !>
      !>         Code was added to handle the following exceptional
      !>         inputs.
      !>
      !>         If the dimension of the array is less than one, any
      !>         value of LOC is invalid. The old version did not check
      !>         the dimension of the array, and as a result, its output
      !>         was unpredictable.
      !>
      !>         If the location at which the elements are to be removed is
      !>         not in the interval [1, NA], an error is signaled.
      !>         Locations not within that interval refer to non-existent
      !>         array elements. The old routine did not signal an error.
      !>         It just returned the original array.
      !>
      !>         If the number of elements to be removed is greater than the
      !>         number of elements can be removed, an error is signaled.
      !>         In the old version, only those elements that could be
      !>         removed were removed, and no error was signaled.
      module subroutine remlad ( ctx, ne, loc, array, na )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ne  !! Number of elements to be removed.
      integer(ip), intent(in) :: loc  !! Location of the first removed element.
      real(wp), intent(inout) :: array ( * )  !! Input/output array.
      integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If LOC does not point to an actual element, signal an error and
!     check out. If the dimension of the array is less than one, any
!     value of LOC is invalid, and an error is signaled.
!

      if ( ( loc < 1 ) .or. ( loc > na ) ) then

         call ctx%setmsg ( 'Location was *.'     )
         call ctx%errint ( '*', loc              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

!
!     Don't try to remove non-existent elements.
!

      else if ( ne > na-loc+1 ) then

         call ctx%setmsg ( 'Trying to remove non-existent elements.' )
         call ctx%sigerr ( 'SPICE(NONEXISTELEMENTS)' )
         return

!
!     If there are elements to be removed, remove them. Otherwise,
!     do not modify the array.
!

      else if ( ne > 0 ) then

!
!        Move the elements forward.
!
         do i = loc, na - ne
            array(i) = array(i+ne)
         end do

!
!        Update the number of elements in the array.
!
         na = na - ne

      end if

      return
      end subroutine remlad

      !> Remove one or more elements from an integer array at the
      !> indicated location.
      !>
      !>## Detailed Input
      !>
      !>     NE       is the number of elements to be removed.
      !>
      !>     LOC      is the location in the array at which the first
      !>              element is to be removed.
      !>
      !>     ARRAY    on input, is the original array.
      !>
      !>     NA       on input, is the number of elements in ARRAY.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output, is the original array with elements
      !>              LOC through LOC+NE-1 removed. Succeeding elements
      !>              are moved forward to fill the vacated spaces.
      !>
      !>     NA       on output, is the number of elements in ARRAY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If LOC is not in the interval [1, NA], the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     2)  If the number of elements to be removed is greater than the
      !>         number of elements that can be removed, the error
      !>         SPICE(NONEXISTELEMENTS) is signaled.
      !>
      !>     3)  If NE is less than one, the array is not modified.
      !>
      !>     4)  If NA is less than one, any location is invalid, and, the
      !>         error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The elements in positions LOC through LOC+NE-1 are overwritten
      !>     as the elements beginning at LOC+NE are moved back.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>           NA = 7      ARRAY(1) = 1
      !>                       ARRAY(2) = 2
      !>                       ARRAY(3) = 3
      !>                       ARRAY(4) = 4
      !>                       ARRAY(5) = 5
      !>                       ARRAY(6) = 6
      !>                       ARRAY(7) = 7
      !>
      !>     Then the call
      !>
      !>           CALL REMLAI ( 3, 3, ARRAY, NA )
      !>
      !>     yields the following result:
      !>
      !>           NA = 4      ARRAY(1) = 1
      !>                       ARRAY(2) = 2
      !>                       ARRAY(3) = 6
      !>                       ARRAY(4) = 7
      !>
      !>     The following calls would signal errors:
      !>
      !>     CALL REMLAI ( 3,  1, ARRAY, -1 )
      !>     CALL REMLAI ( 3, -1, ARRAY,  7 )
      !>     CALL REMLAI ( 3,  6, ARRAY,  7 )
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 01-JAN-1989 (HAN)
      !>
      !>         Code was added to handle the following exceptional
      !>         inputs.
      !>
      !>         If the dimension of the array is less than one, any
      !>         value of LOC is invalid. The old version did not check
      !>         the dimension of the array, and as a result, its output
      !>         was unpredictable.
      !>
      !>         If the location at which the elements are to be removed is
      !>         not in the interval [1, NA], an error is signaled.
      !>         Locations not within that interval refer to non-existent
      !>         array elements. The old routine did not signal an error.
      !>         It just returned the original array.
      !>
      !>         If the number of elements to be removed is greater than the
      !>         number of elements can be removed, an error is signaled.
      !>         In the old version, only those elements that could be
      !>         removed were removed, and no error was signaled.
      module subroutine remlai ( ctx, ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ne  !! Number of elements to be removed.
      integer(ip), intent(in) :: loc  !! Location of the first removed element.
      integer(ip), intent(inout) :: array ( * )  !! Input/output array.
      integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If LOC does not point to an actual element, signal an error and
!     check out. If the dimension of the array is less than one, any
!     value of LOC is invalid, and an error is signaled.
!

      if ( ( loc < 1 ) .or. ( loc > na ) ) then

         call ctx%setmsg ( 'Location was *.'     )
         call ctx%errint ( '*', loc              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

!
!     Don't try to remove non-existent elements.
!

      else if ( ne > na-loc+1 ) then

         call ctx%setmsg ( 'Trying to remove non-existent elements.' )
         call ctx%sigerr ( 'SPICE(NONEXISTELEMENTS)' )
         return

!
!     If there are elements to be removed, remove them. Otherwise,
!     do not modify the array.
!

      else if ( ne > 0 ) then

!
!        Move the elements forward.
!
         do i = loc, na - ne
            array(i) = array(i+ne)
         end do

!
!        Update the number of elements in the array.
!
         na = na - ne

      end if

      return
      end subroutine remlai

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of a character array.
      !>
      !>## Detailed Input
      !>
      !>     N,
      !>     LOCN     define the first group of elements to be exchanged:
      !>              ARRAY(LOCN) through ARRAY(LOCN+N-1).
      !>
      !>     M,
      !>     LOCM     define the second group of elements to be exchanged:
      !>              ARRAY(LOCM) through ARRAY(LOCM+M-1). These must be
      !>              distinct from the first group.
      !>
      !>     ARRAY    on input contains both groups of elements in their
      !>              original locations.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output contains the input array with the indicated
      !>              groups of elements exchanged.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the elements to be swapped are not distinct, the error
      !>         SPICE(NOTDISTINCT) is signaled.
      !>
      !>     2)  If LOCN or LOCM is less than one, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the number of elements to be swapped is less than zero,
      !>         the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If N [M] is zero, the second [first] group is removed from
      !>     its current location and inserted in front of ARRAY(LOCN)
      !>     [ARRAY(LOCM)]. Thus, to move the second [first] group to the
      !>     front of the list, set N [M] and LOCN [LOCM] to zero and one
      !>     respectively. To move the group to the end of the list, set
      !>     N [M] and LOCN [LOCM] to zero and one more than the number of
      !>     elements in the array.
      !>
      !>     All of the elements to be swapped must be distinct.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           Roosevelt
      !>           Truman
      !>           Eisenhower
      !>           Kennedy
      !>           Johnson
      !>           Nixon
      !>           Ford
      !>           Carter
      !>           Reagan
      !>           Cuomo
      !>
      !>     Then the following calls
      !>
      !>           CALL SWAPAC (  1,  2,  2,  7,  ARRAY )
      !>           CALL SWAPAC (  3,  1,  3,  8,  ARRAY )
      !>           CALL SWAPAC (  3,  4,  0,  1,  ARRAY )
      !>           CALL SWAPAC (  2,  4,  0,  11, ARRAY )
      !>
      !>     yield the following arrays respectively.
      !>
      !>           [1]          [2]          [3]          [4]
      !>
      !>           Roosevelt    Carter       Kennedy      Roosevelt
      !>           Ford         Reagan       Johnson      Truman
      !>           Carter       Cuomo        Nixon        Eisenhower
      !>           Eisenhower   Kennedy      Roosevelt    Nixon
      !>           Kennedy      Johnson      Truman       Ford
      !>           Johnson      Nixon        Eisenhower   Carter
      !>           Nixon        Ford         Ford         Reagan
      !>           Truman       Roosevelt    Carter       Cuomo
      !>           Reagan       Truman       Reagan       Kennedy
      !>           Cuomo        Eisenhower   Cuomo        Johnson
      !>
      !>     The following calls
      !>
      !>           CALL SWAPAC ( 3, 2, 4, 5, ARRAY )
      !>           CALL SWAPAC ( 4, 5, 3, 2, ARRAY )
      !>
      !>     yield the following arrays. Note that the resulting arrays
      !>     are equivalent.
      !>
      !>           [1]          [2]
      !>
      !>           Roosevelt    Roosevelt
      !>           Johnson      Johnson
      !>           Nixon        Nixon
      !>           Ford         Ford
      !>           Carter       Carter
      !>           Truman       Truman
      !>           Eisenhower   Eisenhower
      !>           Kennedy      Kennedy
      !>           Reagan       Reagan
      !>           Cuomo        Cuomo
      !>
      !>     The calls
      !>
      !>           CALL SWAPAC ( 3,  5, 4,  6, ARRAY )
      !>           CALL SWAPAC ( 3, -3, 3, 10, ARRAY )
      !>
      !>     signal the errors
      !>
      !>           SPICE(NOTDISTINCT)
      !>           SPICE(INVALIDINDEX)
      !>
      !>     respectively.
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
      !>-    SPICELIB Version 1.2.0, 05-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CYCLAC call.
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
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CYCLAC call.
      !>
      !>-    Beta Version 2.0.0, 3-JAN-1989 (HAN)
      !>
      !>        The "Particulars" section stated that by setting N [M]
      !>        to zero, the second [first] group is removed from its current
      !>        location and inserted in front of ARRAY(LOCM) [ARRAY(LOCN)].
      !>        That statement was incorrect. Insertion occurs in front of
      !>        ARRAY(LOCN) [ARRAY(LOCM)]. The section has been corrected.
      !>
      !>        New checks for locations were added. LOCN and LOCM must be
      !>        greater than one, not zero as specified before. If they are
      !>        not, and error is signaled.
      !>
      !>        More examples were added to the "Examples" section, and
      !>        the long error messages were revised.
      module subroutine swapac ( ctx, n, locn, m, locm, array )
      use pepper_kinds, only: ip
      use pepper_arrays, only: swapc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of elements in the first group.
      integer(ip), intent(in) :: locn  !! Location of the first group.
      integer(ip), intent(in) :: m  !! Number of elements in the second group.
      integer(ip), intent(in) :: locm  !! Location of the second group.
      character(*), intent(inout) :: array  ( * )  !! The array.

      integer(ip) :: nn
      integer(ip) :: ln
      integer(ip) :: nm
      integer(ip) :: lm

      integer(ip) :: direct
      integer(ip) :: extra

      character(1) :: dir
      integer(ip) :: begsub
      integer(ip) :: nsub

      integer(ip) :: i

!
!     We will assume that LOCN and N refer to the earlier group of
!     elements, LOCM and M to the later group. (We can always make
!     this true by exchanging their values.) We also assume that
!     all the elements to be swapped are distinct. (That is, LOCM
!     is greater than or equal to LOCN+N.)
!
!     It's easy enough to swap elements on a one-to-one basis, but
!     what about the ones left over? Without extra storage, they can
!     be moved one at a time; but each such move requires moving every
!     element between the origin and destination as well. For large
!     arrays, this is clearly unacceptable.
!
!     In the figure below, the array on the left contains two groups
!     of elements which are to be swapped. We can begin by swapping the
!     leading elements of each group one-for-one.
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Adam         |        | Barney       |
!        +--------------+        +--------------+
!        | Alvin        |        | Betty        |
!        +--------------+        +--------------+
!        |              |        |              |  <---+
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Barney       |        | Adam         |      |
!        +--------------+        +--------------+      |
!        | Betty        |        | Alvin        |      |
!        +--------------+        +--------------+      |
!        | Bill         |        | Bill         |      |
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     Notice that cycling the indicated sub-array forward twice brings
!     the remaining elements to their proper locations. This is most
!     fortunate, because cycling the elements of an array is a linear
!     operation. (See CYCLAx for details.)
!
!     And what if the extra elements are in the first group?
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Barney       |        | Adam         |
!        +--------------+        +--------------+
!        | Betty        |        | Alvin        |
!        +--------------+        +--------------+
!        | Bill         |        | Bill         |  <---+
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Adam         |        | Barney       |      |
!        +--------------+        +--------------+      |
!        | Alvin        |        | Betty        |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     In this case, the indicated sub-array must be cycled backward
!     in order to bring the extra elements to their proper places.
!
!     The algorithm is:
!
!        1) Let DIRECT be the smaller of N and M, and let EXTRA
!           be the absolute value of the difference (N-M).
!
!        2) Exchange DIRECT elements directly.
!
!        3) Determine the direction of the cycle: forward when N < M,
!           backward when N > M.
!
!        4) Determine the sub-array to be cycled. It begins at element
!           (LOCN+DIRECT) and contains (LOCM-LOCN) + (M-DIRECT) elements
!
!        5) Cycle the sub-array EXTRA times in the indicated direction.
!

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the inputs are valid.
!
      if ( n < 0 ) then

         call ctx%setmsg ( 'Number of elements in the first group is *.' )
         call ctx%errint ( '*', n                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return

      else if ( m < 0 ) then

         call ctx%setmsg ( 'Number of elements in the second group is *.' )
         call ctx%errint ( '*', m                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return

      else if ( locn < 1 ) then

         call ctx%setmsg ( 'Location of the first group is *.' )
         call ctx%errint ( '*', locn             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      else if ( locm < 1 ) then

         call ctx%setmsg ( 'Location of the second group is *.' )
         call ctx%errint ( '*', locm             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      end if

!
!     Make sure we have the groups in the right order.
!
      if ( locn < locm ) then
         ln = locn
         lm = locm
         nn = n
         nm = m
      else
         ln = locm
         lm = locn
         nn = m
         nm = n
      end if

!
!     The elements must be distinct.
!
      if ( lm < ln + nn ) then
         call ctx%setmsg ( 'Elements to be swapped are not distinct.' )
         call ctx%sigerr ( 'SPICE(NOTDISTINCT)' )
         return
      end if

!
!     Direct exchange.
!
      direct = min ( nn, nm )

      do i = 0, direct-1
         call swapc ( array(ln+i), array(lm+i) )
      end do

!
!     Cycle.
!
      extra = abs ( nn - nm )

      if ( extra > 0 ) then

         if ( nn < nm ) then
            dir = 'F'
         else
            dir = 'B'
         end if

         begsub = ln + direct
         nsub   = (lm - ln) + (nm - direct)

         call cyacip ( ctx, nsub, dir, extra, array(begsub) )

      end if

      return
      end subroutine swapac

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of a double precision array.
      !>
      !>## Detailed Input
      !>
      !>     N,
      !>     LOCN     define the first group of elements to be exchanged:
      !>              ARRAY(LOCN) through ARRAY(LOCN+N-1).
      !>
      !>     M,
      !>     LOCM     define the second group of elements to be exchanged:
      !>              ARRAY(LOCM) through ARRAY(LOCM+M-1). These must be
      !>              distinct from the first group.
      !>
      !>     ARRAY    on input contains both groups of elements in their
      !>              original locations.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output contains the input array with the indicated
      !>              groups of elements exchanged.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the elements to be swapped are not distinct, the error
      !>         SPICE(NOTDISTINCT) is signaled.
      !>
      !>     2)  If LOCN or LOCM is less than one, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the number of elements to be swapped is less than zero,
      !>         the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If N [M] is zero, the second [first] group is removed from
      !>     its current location and inserted in front of ARRAY(LOCN)
      !>     [ARRAY(LOCM)]. Thus, to move the second [first] group to the
      !>     front of the list, set N [M] and LOCN [LOCM] to zero and one
      !>     respectively. To move the group to the end of the list, set
      !>     N [M] and LOCN [LOCM] to zero and one more than the number of
      !>     elements in the array.
      !>
      !>     All of the elements to be swapped must be distinct.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           Roosevelt
      !>           Truman
      !>           Eisenhower
      !>           Kennedy
      !>           Johnson
      !>           Nixon
      !>           Ford
      !>           Carter
      !>           Reagan
      !>           Cuomo
      !>
      !>     Then the following calls
      !>
      !>           CALL SWAPAC (  1,  2,  2,  7,  ARRAY )
      !>           CALL SWAPAC (  3,  1,  3,  8,  ARRAY )
      !>           CALL SWAPAC (  3,  4,  0,  1,  ARRAY )
      !>           CALL SWAPAC (  2,  4,  0,  11, ARRAY )
      !>
      !>     yield the following arrays respectively.
      !>
      !>           [1]          [2]          [3]          [4]
      !>
      !>           Roosevelt    Carter       Kennedy      Roosevelt
      !>           Ford         Reagan       Johnson      Truman
      !>           Carter       Cuomo        Nixon        Eisenhower
      !>           Eisenhower   Kennedy      Roosevelt    Nixon
      !>           Kennedy      Johnson      Truman       Ford
      !>           Johnson      Nixon        Eisenhower   Carter
      !>           Nixon        Ford         Ford         Reagan
      !>           Truman       Roosevelt    Carter       Cuomo
      !>           Reagan       Truman       Reagan       Kennedy
      !>           Cuomo        Eisenhower   Cuomo        Johnson
      !>
      !>     The following calls
      !>
      !>           CALL SWAPAC ( 3, 2, 4, 5, ARRAY )
      !>           CALL SWAPAC ( 4, 5, 3, 2, ARRAY )
      !>
      !>     yield the following arrays. Note that the resulting arrays
      !>     are equivalent.
      !>
      !>           [1]          [2]
      !>
      !>           Roosevelt    Roosevelt
      !>           Johnson      Johnson
      !>           Nixon        Nixon
      !>           Ford         Ford
      !>           Carter       Carter
      !>           Truman       Truman
      !>           Eisenhower   Eisenhower
      !>           Kennedy      Kennedy
      !>           Reagan       Reagan
      !>           Cuomo        Cuomo
      !>
      !>     The calls
      !>
      !>           CALL SWAPAC ( 3,  5, 4,  6, ARRAY )
      !>           CALL SWAPAC ( 3, -3, 3, 10, ARRAY )
      !>
      !>     signal the errors
      !>
      !>           SPICE(NOTDISTINCT)
      !>           SPICE(INVALIDINDEX)
      !>
      !>     respectively.
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" markers from text in the header.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CYCLAD call.
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
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CYCLAD call.
      !>
      !>-    Beta Version 2.0.0, 03-JAN-1989 (HAN)
      !>
      !>        The "Particulars" section stated that by setting N [M]
      !>        to zero, the second [first] group is removed from its current
      !>        location and inserted in front of ARRAY(LOCM) [ARRAY(LOCN)].
      !>        That statement was incorrect. Insertion occurs in front of
      !>        ARRAY(LOCN) [ARRAY(LOCM)]. The section has been corrected.
      !>
      !>        New checks for locations were added. LOCN and LOCM must be
      !>        greater than one, not zero as specified before. If they are
      !>        not, and error is signaled.
      !>
      !>        More examples were added to the "Examples" section, and
      !>        the long error messages were revised.
      module subroutine swapad ( ctx, n, locn, m, locm, array )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: swapd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of elements in the first group.
      integer(ip), intent(in) :: locn  !! Location of the first group.
      integer(ip), intent(in) :: m  !! Number of elements in the second group.
      integer(ip), intent(in) :: locm  !! Location of the second group.
      real(wp), intent(inout) :: array  ( * )  !! The array.

      integer(ip) :: nn
      integer(ip) :: ln
      integer(ip) :: nm
      integer(ip) :: lm

      integer(ip) :: direct
      integer(ip) :: extra

      character(1) :: dir
      integer(ip) :: begsub
      integer(ip) :: nsub

      integer(ip) :: i

!
!     We will assume that LOCN and N refer to the earlier group of
!     elements, LOCM and M to the later group. (We can always make
!     this true by exchanging their values.) We also assume that
!     all the elements to be swapped are distinct. (That is, LOCM
!     is greater than or equal to LOCN+N.)
!
!     It's easy enough to swap elements on a one-to-one basis, but
!     what about the ones left over? Without extra storage, they can
!     be moved one at a time; but each such move requires moving every
!     element between the origin and destination as well. For large
!     arrays, this is clearly unacceptable.
!
!     In the figure below, the array on the left contains two groups
!     of elements which are to be swapped. We can begin by swapping the
!     leading elements of each group one-for-one.
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Adam         |        | Barney       |
!        +--------------+        +--------------+
!        | Alvin        |        | Betty        |
!        +--------------+        +--------------+
!        |              |        |              |  <---+
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Barney       |        | Adam         |      |
!        +--------------+        +--------------+      |
!        | Betty        |        | Alvin        |      |
!        +--------------+        +--------------+      |
!        | Bill         |        | Bill         |      |
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     Notice that cycling the indicated sub-array forward twice brings
!     the remaining elements to their proper locations. This is most
!     fortunate, because cycling the elements of an array is a linear
!     operation. (See CYCLAx for details.)
!
!     And what if the extra elements are in the first group?
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Barney       |        | Adam         |
!        +--------------+        +--------------+
!        | Betty        |        | Alvin        |
!        +--------------+        +--------------+
!        | Bill         |        | Bill         |  <---+
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Adam         |        | Barney       |      |
!        +--------------+        +--------------+      |
!        | Alvin        |        | Betty        |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     In this case, the indicated sub-array must be cycled backward
!     in order to bring the extra elements to their proper places.
!
!     The algorithm is:
!
!        1) Let DIRECT be the smaller of N and M, and let EXTRA
!           be the absolute value of the difference (N-M).
!
!        2) Exchange DIRECT elements directly.
!
!        3) Determine the direction of the cycle: forward when N < M,
!           backward when N > M.
!
!        4) Determine the sub-array to be cycled. It begins at element
!           (LOCN+DIRECT) and contains (LOCM-LOCN) + (M-DIRECT) elements
!
!        5) Cycle the sub-array EXTRA times in the indicated direction.
!

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the inputs are valid.
!

      if ( n < 0 ) then

         call ctx%setmsg ( 'Number of elements in the first group is *.' )
         call ctx%errint ( '*', n                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return

      else if ( m < 0 ) then

         call ctx%setmsg ( 'Number of elements in the second group is *.' )
         call ctx%errint ( '*', m                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return

      else if ( locn < 1 ) then

         call ctx%setmsg ( 'Location of the first group is *.' )
         call ctx%errint ( '*', locn             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      else if ( locm < 1 ) then

         call ctx%setmsg ( 'Location of the second group is *.' )
         call ctx%errint ( '*', locm             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      end if

!
!     Make sure we have the groups in the right order.
!
      if ( locn < locm ) then
         ln = locn
         lm = locm
         nn = n
         nm = m
      else
         ln = locm
         lm = locn
         nn = m
         nm = n
      end if

!
!     The elements must be distinct.
!
      if ( lm < ln + nn ) then
         call ctx%setmsg ( 'Elements to be swapped are not distinct.' )
         call ctx%sigerr ( 'SPICE(NOTDISTINCT)' )
         return
      end if

!
!     Direct exchange.
!
      direct = min ( nn, nm )

      do i = 0, direct-1
         call swapd ( array(ln+i), array(lm+i) )
      end do

!
!     Cycle.
!
      extra = abs ( nn - nm )

      if ( extra > 0 ) then

         if ( nn < nm ) then
            dir = 'F'
         else
            dir = 'B'
         end if

         begsub = ln + direct
         nsub   = (lm - ln) + (nm - direct)

         call cyadip ( ctx, nsub, dir, extra, array(begsub) )

      end if

      return
      end subroutine swapad

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of an integer array.
      !>
      !>## Detailed Input
      !>
      !>     N,
      !>     LOCN     define the first group of elements to be exchanged:
      !>              ARRAY(LOCN) through ARRAY(LOCN+N-1).
      !>
      !>     M,
      !>     LOCM     define the second group of elements to be exchanged:
      !>              ARRAY(LOCM) through ARRAY(LOCM+M-1). These must be
      !>              distinct from the first group.
      !>
      !>     ARRAY    on input contains both groups of elements in their
      !>              original locations.
      !>
      !>## Detailed Output
      !>
      !>     ARRAY    on output contains the input array with the indicated
      !>              groups of elements exchanged.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the elements to be swapped are not distinct, the error
      !>         SPICE(NOTDISTINCT) is signaled.
      !>
      !>     2)  If LOCN or LOCM is less than one, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the number of elements to be swapped is less than zero,
      !>         the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If N [M] is zero, the second [first] group is removed from
      !>     its current location and inserted in front of ARRAY(LOCN)
      !>     [ARRAY(LOCM)]. Thus, to move the second [first] group to the
      !>     front of the list, set N [M] and LOCN [LOCM] to zero and one
      !>     respectively. To move the group to the end of the list, set
      !>     N [M] and LOCN [LOCM] to zero and one more than the number of
      !>     elements in the array.
      !>
      !>     All of the elements to be swapped must be distinct.
      !>
      !>## Examples
      !>
      !>     Let ARRAY contain the following elements.
      !>
      !>           Roosevelt
      !>           Truman
      !>           Eisenhower
      !>           Kennedy
      !>           Johnson
      !>           Nixon
      !>           Ford
      !>           Carter
      !>           Reagan
      !>           Cuomo
      !>
      !>     Then the following calls
      !>
      !>           CALL SWAPAC (  1,  2,  2,  7,  ARRAY )
      !>           CALL SWAPAC (  3,  1,  3,  8,  ARRAY )
      !>           CALL SWAPAC (  3,  4,  0,  1,  ARRAY )
      !>           CALL SWAPAC (  2,  4,  0,  11, ARRAY )
      !>
      !>     yield the following arrays respectively.
      !>
      !>           [1]          [2]          [3]          [4]
      !>
      !>           Roosevelt    Carter       Kennedy      Roosevelt
      !>           Ford         Reagan       Johnson      Truman
      !>           Carter       Cuomo        Nixon        Eisenhower
      !>           Eisenhower   Kennedy      Roosevelt    Nixon
      !>           Kennedy      Johnson      Truman       Ford
      !>           Johnson      Nixon        Eisenhower   Carter
      !>           Nixon        Ford         Ford         Reagan
      !>           Truman       Roosevelt    Carter       Cuomo
      !>           Reagan       Truman       Reagan       Kennedy
      !>           Cuomo        Eisenhower   Cuomo        Johnson
      !>
      !>     The following calls
      !>
      !>           CALL SWAPAC ( 3, 2, 4, 5, ARRAY )
      !>           CALL SWAPAC ( 4, 5, 3, 2, ARRAY )
      !>
      !>     yield the following arrays. Note that the resulting arrays
      !>     are equivalent.
      !>
      !>           [1]          [2]
      !>
      !>           Roosevelt    Roosevelt
      !>           Johnson      Johnson
      !>           Nixon        Nixon
      !>           Ford         Ford
      !>           Carter       Carter
      !>           Truman       Truman
      !>           Eisenhower   Eisenhower
      !>           Kennedy      Kennedy
      !>           Reagan       Reagan
      !>           Cuomo        Cuomo
      !>
      !>     The calls
      !>
      !>           CALL SWAPAC ( 3,  5, 4,  6, ARRAY )
      !>           CALL SWAPAC ( 3, -3, 3, 10, ARRAY )
      !>
      !>     signal the errors
      !>
      !>           SPICE(NOTDISTINCT)
      !>           SPICE(INVALIDINDEX)
      !>
      !>     respectively.
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CYCLAI call.
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
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CYCLAI call.
      !>
      !>-    Beta Version 2.0.0, 03-JAN-1989 (HAN)
      !>
      !>        The "Particulars" section stated that by setting N [M]
      !>        to zero, the second [first] group is removed from its current
      !>        location and inserted in front of ARRAY(LOCM) [ARRAY(LOCN)].
      !>        That statement was incorrect. Insertion occurs in front of
      !>        ARRAY(LOCN) [ARRAY(LOCM)]. The section has been corrected.
      !>
      !>        New checks for locations were added. LOCN and LOCM must be
      !>        greater than one, not zero as specified before. If they are
      !>        not, and error is signaled.
      !>
      !>        More examples were added to the "Examples" section, and
      !>        the long error messages were revised.
      module subroutine swapai ( ctx, n, locn, m, locm, array )
      use pepper_kinds, only: ip
      use pepper_arrays, only: swapi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of elements in the first group.
      integer(ip), intent(in) :: locn  !! Location of the first group.
      integer(ip), intent(in) :: m  !! Number of elements in the second group.
      integer(ip), intent(in) :: locm  !! Location of the second group.
      integer(ip), intent(inout) :: array  ( * )  !! The array.

      integer(ip) :: nn
      integer(ip) :: ln
      integer(ip) :: nm
      integer(ip) :: lm

      integer(ip) :: direct
      integer(ip) :: extra

      character(1) :: dir
      integer(ip) :: begsub
      integer(ip) :: nsub

      integer(ip) :: i

!
!     We will assume that LOCN and N refer to the earlier group of
!     elements, LOCM and M to the later group. (We can always make
!     this true by exchanging their values.) We also assume that
!     all the elements to be swapped are distinct. (That is, LOCM
!     is greater than or equal to LOCN+N.)
!
!     It's easy enough to swap elements on a one-to-one basis, but
!     what about the ones left over? Without extra storage, they can
!     be moved one at a time; but each such move requires moving every
!     element between the origin and destination as well. For large
!     arrays, this is clearly unacceptable.
!
!     In the figure below, the array on the left contains two groups
!     of elements which are to be swapped. We can begin by swapping the
!     leading elements of each group one-for-one.
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Adam         |        | Barney       |
!        +--------------+        +--------------+
!        | Alvin        |        | Betty        |
!        +--------------+        +--------------+
!        |              |        |              |  <---+
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Barney       |        | Adam         |      |
!        +--------------+        +--------------+      |
!        | Betty        |        | Alvin        |      |
!        +--------------+        +--------------+      |
!        | Bill         |        | Bill         |      |
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     Notice that cycling the indicated sub-array forward twice brings
!     the remaining elements to their proper locations. This is most
!     fortunate, because cycling the elements of an array is a linear
!     operation. (See CYCLAx for details.)
!
!     And what if the extra elements are in the first group?
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Barney       |        | Adam         |
!        +--------------+        +--------------+
!        | Betty        |        | Alvin        |
!        +--------------+        +--------------+
!        | Bill         |        | Bill         |  <---+
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Adam         |        | Barney       |      |
!        +--------------+        +--------------+      |
!        | Alvin        |        | Betty        |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     In this case, the indicated sub-array must be cycled backward
!     in order to bring the extra elements to their proper places.
!
!     The algorithm is:
!
!        1) Let DIRECT be the smaller of N and M, and let EXTRA
!           be the absolute value of the difference (N-M).
!
!        2) Exchange DIRECT elements directly.
!
!        3) Determine the direction of the cycle: forward when N < M,
!           backward when N > M.
!
!        4) Determine the sub-array to be cycled. It begins at element
!           (LOCN+DIRECT) and contains (LOCM-LOCN) + (M-DIRECT) elements
!
!        5) Cycle the sub-array EXTRA times in the indicated direction.
!

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the inputs are valid.
!
      if ( n < 0 ) then

         call ctx%setmsg ( 'Number of elements in the first group is *.' )
         call ctx%errint ( '*', n                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return

      else if ( m < 0 ) then

         call ctx%setmsg ( 'Number of elements in the second group is *.' )
         call ctx%errint ( '*', m                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return

      else if ( locn < 1 ) then

         call ctx%setmsg ( 'Location of the first group is *.' )
         call ctx%errint ( '*', locn             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      else if ( locm < 1 ) then

         call ctx%setmsg ( 'Location of the second group is *.' )
         call ctx%errint ( '*', locm             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
         return

      end if

!
!     Make sure we have the groups in the right order.
!
      if ( locn < locm ) then
         ln = locn
         lm = locm
         nn = n
         nm = m
      else
         ln = locm
         lm = locn
         nn = m
         nm = n
      end if

!
!     The elements must be distinct.
!
      if ( lm < ln + nn ) then
         call ctx%setmsg ( 'Elements to be swapped are not distinct.' )
         call ctx%sigerr ( 'SPICE(NOTDISTINCT)' )
         return
      end if

!
!     Direct exchange.
!
      direct = min ( nn, nm )

      do i = 0, direct-1
         call swapi ( array(ln+i), array(lm+i) )
      end do

!
!     Cycle.
!
      extra = abs ( nn - nm )

      if ( extra > 0 ) then

         if ( nn < nm ) then
            dir = 'F'
         else
            dir = 'B'
         end if

         begsub = ln + direct
         nsub   = (lm - ln) + (nm - direct)

         call cyaiip ( ctx, nsub, dir, extra, array(begsub) )

      end if

      return
      end subroutine swapai

      !> Pack the contents of a CHARACTER array. That is, take
      !> a set of arbitrarily spaced elements from an input array,
      !> and make them adjacent elements in an output array.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input array.
      !>
      !>     PACK     is the set of elements to be packed into the output
      !>              array. PACK(i) is the index of the element in the
      !>              input array that is to become the i'th element of
      !>              the output array.
      !>
      !>     NPACK    is the number of elements to be packed into the
      !>              output array.
      !>
      !>     MAXOUT   is the maximum number of elements to be packed
      !>              into the output array. If NPACK is larger than
      !>              MAXOUT, the extra items are ignored.
      !>
      !>## Detailed Output
      !>
      !>     NOUT     is the number of elements in the output array.
      !>
      !>     OUT      is the output array. This array contains up to
      !>              MAXOUT elements from the input array, located
      !>              in the first NOUT elements of the array.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an element in the PACK array is less than 1, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The indicated elements are moved from their current locations
      !>     in the input array to consecutive positions in the output array.
      !>
      !>        OUT(   1) = IN(PACK(   1))
      !>        OUT(   2) = IN(PACK(   2))
      !>             .
      !>             .
      !>        OUT(NOUT) = IN(PACK(NOUT))
      !>
      !>     NOUT is either NPACK or MAXOUT, whichever is smaller.
      !>
      !>## Examples
      !>
      !>     The most common use for this routine is to remove unwanted items
      !>     from an array or set of arrays. For example, suppose that the
      !>     arrays NAME, CODE, RADIUS and MASS contain the names, NAIF
      !>     integer ID codes, radii, and masses of a set of NSAT satellites.
      !>     Suppose further that the user selects a subset of the original
      !>     set of satellites from a menu of some sort. Let the indices of
      !>     these satellites be the NSEL elements of the array SEL. The
      !>     following sequence would remove the names, codes, etc., of the
      !>     unselected satellites from the arrays.
      !>
      !>        CALL PACKAC ( NAME,   SEL, NSEL, NSAT, NOUT, NAME2   )
      !>        CALL PACKAI ( CODE,   SEL, NSEL, NSAT, NOUT, CODE2   )
      !>        CALL PACKAD ( RADIUS, SEL, NSEL, NSAT, NOUT, RADIUS2 )
      !>        CALL PACKAD ( MASS,   SEL, NSEL, NSAT, NOUT, MASS2   )
      !>
      !>     In the example above, suppose that NAME and PACK contain
      !>     the following:
      !>
      !>        NAME = 'MIMAS'          PACK = 2, 4, 6, 7
      !>               'ENCELADUS'
      !>               'TETHYS'
      !>               'DIONE'
      !>               'RHEA'
      !>               'TITAN'
      !>               'HYPERION'
      !>               'IAPETUS'
      !>               'PHOEBE'
      !>
      !>     Then, following the call to PACKAC, NOUT and NAME2 contain
      !>     the following:
      !>
      !>        NOUT = 4                 NAME2 = 'ENCELADUS'
      !>                                         'DIONE'
      !>                                         'TITAN'
      !>                                         'HYPERION'
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
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
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
      !>-     Beta Version 2.0.0, 4-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect array indices that are
      !>         out of bound. If any element contained in the PACK array is
      !>         less than one, an error is signaled, and the output array is
      !>         not packed.
      module subroutine packac ( ctx, in, pack, npack, maxout, nout, out )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: in      ( * )  !! Input array.
      integer(ip), intent(in) :: pack    ( * )  !! Indices of elements to be packed.
      integer(ip), intent(in) :: npack  !! Number of indices.
      integer(ip), intent(in) :: maxout  !! Maximum number of elements in the output array.
      integer(ip), intent(out) :: nout  !! Number of elements in the output array.
      character(*), intent(out) :: out     ( * )  !! Output array.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     First, determine how many items to transfer.
!
      nout = min ( npack, maxout )

!
!     Check to see if PACK contains valid array indices.
!
      do i = 1, nout

         if ( pack (i) < 1 ) then
            call ctx%setmsg ( 'Element number * contains index *.' )
            call ctx%errint ( '*', i                )
            call ctx%errint ( '*', pack(i)          )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
            return
         end if

      end do

!
!     Transfer them. Just like it says in the header.
!
      do i = 1, nout
         out(i) = in(pack(i))
      end do

      return
      end subroutine packac

      !> Pack the contents of a double precision array. That is,
      !> take a set of arbitrarily spaced elements from an input
      !> array, and make them adjacent elements in an output array.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input array.
      !>
      !>     PACK     is the set of elements to be packed into the output
      !>              array. PACK(i) is the index of the element in the
      !>              input array that is to become the i'th element of the
      !>              output array.
      !>
      !>     NPACK    is the number of elements to be packed into the
      !>              output array.
      !>
      !>     MAXOUT   is the maximum number of elements to be packed into
      !>              the output array. If NPACK is larger than MAXOUT, the
      !>              extra items are ignored.
      !>
      !>## Detailed Output
      !>
      !>     NOUT     is the number of elements in the output array.
      !>
      !>     OUT      is the output array. This array contains up to
      !>              MAXOUT elements from the input array, located
      !>              in the first NOUT elements of the array.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an element in the PACK array is less than 1, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The indicated elements are moved from their current locations
      !>     in the input array to consecutive positions in the output array.
      !>
      !>        OUT(   1) = IN(PACK(   1))
      !>        OUT(   2) = IN(PACK(   2))
      !>             .
      !>             .
      !>       OUT(NOUT) = IN(PACK(NOUT))
      !>
      !>     NOUT is either NPACK or MAXOUT, whichever is smaller.
      !>
      !>## Examples
      !>
      !>     The most common use for this routine is to remove unwanted items
      !>     from an array or set of arrays. For example, suppose that the
      !>     arrays NAME, CODE, RADIUS and MASS contain the names, NAIF
      !>     integer ID codes, radii, and masses of a set of NSAT satellites.
      !>     Suppose further that the user selects a subset of the original
      !>     set of satellites from a menu of some sort. Let the indices of
      !>     these satellites be the NSEL elements of the array SEL. The
      !>     following sequence would remove the names, codes, etc., of the
      !>     unselected satellites from the arrays.
      !>
      !>        CALL PACKAC ( NAME,   SEL, NSEL, NSAT, NOUT, NAME2   )
      !>        CALL PACKAI ( CODE,   SEL, NSEL, NSAT, NOUT, CODE2   )
      !>        CALL PACKAD ( RADIUS, SEL, NSEL, NSAT, NOUT, RADIUS2 )
      !>        CALL PACKAD ( MASS,   SEL, NSEL, NSAT, NOUT, MASS2   )
      !>
      !>     In the example above, suppose that NAME and PACK contain
      !>     the following:
      !>
      !>        NAME = 'MIMAS'          PACK = 2, 4, 6, 7
      !>               'ENCELADUS'
      !>               'TETHYS'
      !>               'DIONE'
      !>               'RHEA'
      !>               'TITAN'
      !>               'HYPERION'
      !>               'IAPETUS'
      !>               'PHOEBE'
      !>
      !>     Then, following the call to PACKAC, NOUT and NAME2 contain
      !>     the following:
      !>
      !>        NOUT = 4                 NAME2 = 'ENCELADUS'
      !>                                         'DIONE'
      !>                                         'TITAN'
      !>                                         'HYPERION'
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
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
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
      !>-     Beta Version 2.0.0, 4-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect array indices that are
      !>         out of bound. If any element contained in the PACK array is
      !>         less than one, an error is signaled, and the output array is
      !>         not packed.
      module subroutine packad ( ctx, in, pack, npack, maxout, nout, out )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: in      ( * )  !! Input array.
      integer(ip), intent(in) :: pack    ( * )  !! Indices of elements to be packed.
      integer(ip), intent(in) :: npack  !! Number of indices.
      integer(ip), intent(in) :: maxout  !! Maximum number of elements in the output array.
      integer(ip), intent(out) :: nout  !! Number of elements in the output array.
      real(wp), intent(out) :: out     ( * )  !! Output array.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     First, determine how many items to transfer.
!
      nout = min ( npack, maxout )

!
!     Check to see if PACK contains valid array indices.
!
      do i = 1, nout

         if ( pack (i) < 1 ) then
            call ctx%setmsg ( 'Element number * contains index *.' )
            call ctx%errint ( '*', i                )
            call ctx%errint ( '*', pack(i)          )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
            return
         end if

      end do

!
!     Transfer them. Just like it says in the header.
!
      do i = 1, nout
         out(i) = in(pack(i))
      end do

      return
      end subroutine packad

      !> Pack the contents of an integer array. That is,
      !> take a set of arbitrarily spaced elements from an input
      !> array, and make them adjacent elements in an output array.
      !>
      !>## Detailed Input
      !>
      !>     IN       is the input array.
      !>
      !>     PACK     is the set of elements to be packed into the output
      !>              array. PACK(i) is the index of the element in the
      !>              input array that is to become the i'th element of
      !>              the output array.
      !>
      !>     NPACK    is the number of elements to be packed into the
      !>              output array.
      !>
      !>     MAXOUT   is the maximum number of elements to be packed
      !>              into the output array. If NPACK is larger than
      !>              MAXOUT, the extra items are ignored.
      !>
      !>## Detailed Output
      !>
      !>     NOUT     is the number of elements in the output array.
      !>
      !>     OUT      is the output array. This array contains up to
      !>              MAXOUT elements from the input array, located
      !>              in the first NOUT elements of the array.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an element in the PACK array is less than 1, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The indicated elements are moved from their current locations
      !>     in the input array to consecutive positions in the output array.
      !>
      !>        OUT(   1) = IN(PACK(   1))
      !>        OUT(   2) = IN(PACK(   2))
      !>             .
      !>             .
      !>        OUT(NOUT) = IN(PACK(NOUT))
      !>
      !>     NOUT is either NPACK or MAXOUT, whichever is smaller.
      !>
      !>## Examples
      !>
      !>     The most common use for this routine is to remove unwanted items
      !>     from an array or set of arrays. For example, suppose that the
      !>     arrays NAME, CODE, RADIUS and MASS contain the names, NAIF
      !>     integer ID codes, radii, and masses of a set of NSAT satellites.
      !>     Suppose further that the user selects a subset of the original
      !>     set of satellites from a menu of some sort. Let the indices of
      !>     these satellites be the NSEL elements of the array SEL. The
      !>     following sequence would remove the names, codes, etc., of the
      !>     unselected satellites from the arrays.
      !>
      !>        CALL PACKAC ( NAME,   SEL, NSEL, NSAT, NOUT, NAME2   )
      !>        CALL PACKAI ( CODE,   SEL, NSEL, NSAT, NOUT, CODE2   )
      !>        CALL PACKAD ( RADIUS, SEL, NSEL, NSAT, NOUT, RADIUS2 )
      !>        CALL PACKAD ( MASS,   SEL, NSEL, NSAT, NOUT, MASS2   )
      !>
      !>     In the example above, suppose that NAME and PACK contain
      !>     the following:
      !>
      !>        NAME = 'MIMAS'          PACK = 2, 4, 6, 7
      !>               'ENCELADUS'
      !>               'TETHYS'
      !>               'DIONE'
      !>               'RHEA'
      !>               'TITAN'
      !>               'HYPERION'
      !>               'IAPETUS'
      !>               'PHOEBE'
      !>
      !>     Then, following the call to PACKAC, NOUT and NAME2 contain
      !>     the following:
      !>
      !>        NOUT = 4                 NAME2 = 'ENCELADUS'
      !>                                         'DIONE'
      !>                                         'TITAN'
      !>                                         'HYPERION'
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
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
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
      !>-     Beta Version 2.0.0, 4-JAN-1989 (HAN)
      !>
      !>         Error handling was added to detect array indices that are
      !>         out of bound. If any element contained in the PACK array is
      !>         less than one, an error is signaled, and the output array is
      !>         not packed.
      module subroutine packai ( ctx, in, pack, npack, maxout, nout, out )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: in      ( * )  !! Input array.
      integer(ip), intent(in) :: pack    ( * )  !! Indices of elements to be packed.
      integer(ip), intent(in) :: npack  !! Number of indices.
      integer(ip), intent(in) :: maxout  !! Maximum number of elements in the output array.
      integer(ip), intent(out) :: nout  !! Number of elements in the output array.
      integer(ip), intent(out) :: out     ( * )  !! Output array.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     First, determine how many items to transfer.
!
      nout = min ( npack, maxout )

!
!     Check to see if PACK contains valid array indices.
!
      do i = 1, nout

         if ( pack (i) < 1 ) then
            call ctx%setmsg ( 'Element number * contains index *.' )
            call ctx%errint ( '*', i                )
            call ctx%errint ( '*', pack(i)          )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)' )
            return
         end if

      end do

!
!     Transfer them. Just like it says in the header.
!
      do i = 1, nout
         out(i) = in(pack(i))
      end do

      return
      end subroutine packai

      !> Allocate a node in a doubly linked list pool.
      !>
      !>## Detailed Input
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     POOL     is the input pool, with the following
      !>              modifications:
      !>
      !>                 -- NEW is an allocated node: both the forward
      !>                    and backward pointers of NEW are -NEW.
      !>
      !>                 -- The node that was the successor of NEW on
      !>                    input is the head of the free list on output.
      !>
      !>     NEW      is the number of the newly allocated node.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If no free nodes are available for allocation, the error
      !>         SPICE(NOFREENODES) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     In a doubly linked list pool, an `allocated node' is one that has
      !>     been removed from the free list. An allocated node may be linked
      !>     to other nodes or may be unlinked; in the latter case, both the
      !>     forward and backward pointers of the node will be the negative of
      !>     the node number.
      !>
      !>     A node must be allocated before it can be linked to another
      !>     node.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool. To build a new list
      !>         of ten nodes, the code fragment below can be used:
      !>
      !>            C
      !>            C     We'll use LNKILA ( LNK, insert list after
      !>            C     a specified node ) to add nodes to the tail of the
      !>            C     list.
      !>            C
      !>                  PREV = 0
      !>
      !>                  DO I = 1, 10
      !>
      !>                     CALL LNKAN  ( POOL, NODE       )
      !>                     CALL LNKILA ( PREV, NODE, POOL )
      !>                     PREV = NODE
      !>
      !>                  END DO
      !>
      !>     2)  In this version of example (1), we check that a sufficient
      !>         number of free nodes are available before building the list:
      !>
      !>            C
      !>            C     Make sure we have 10 free nodes available.
      !>            C     Signal an error if not. Use LNKNFN to obtain
      !>            C     the number of free nodes.
      !>            C
      !>                  IF ( LNKNFN(POOL) .LT. 10 ) THEN
      !>
      !>                     CALL SETMSG ( 'Only # free nodes are available '//
      !>                 .                 'but 10 are required.'            )
      !>                     CALL ERRINT ( '#', LNKNFN(POOL)                 )
      !>                     CALL SIGERR ( 'POOL_TOO_SMALL'                  )
      !>                     RETURN
      !>
      !>                  END IF
      !>
      !>                     [ Build list ]
      !>                           .
      !>                           .
      !>                           .
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module subroutine lnkan ( ctx, pool, new )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      integer(ip), intent(out) :: new  !! Number of new node that was allocated.


!
!     Discovery check-in is used in place of standard SPICE error
!     handling.
!
      if (  pool( nfrrow, nfrcol )  ==  0  ) then

         call ctx%setmsg ( 'There are no free nodes left for ' &
         //            'allocating in the supplied linked list ' &
         //            'pool. ' )
         call ctx%sigerr ( 'SPICE(NOFREENODES)'  )
         return

      end if

!
!     The caller gets the first free node.  The forward pointer of
!     this node indicates the next free node.  After this, there's one
!     less free node.
!
      new                      =   pool( frerow, frecol )

      pool( frerow, frecol )   =   pool( forwrd, new    )
      pool( nfrrow, nfrcol )   =   pool( nfrrow, nfrcol ) - 1

!
!     The forward and backward pointers of the allocated node become
!     the negatives of the node numbers of the head and tail nodes
!     of the list containing NEW.  Since this is a singleton list,
!     both pointers are -NEW.
!
      pool( forwrd, new )   =   -new
      pool( bckwrd, new )   =   -new

      return
      end subroutine lnkan

      !> Free a specified sublist in a list.
      !>
      !>## Detailed Input
      !>
      !>     HEAD,
      !>     TAIL     are, respectively, the head and tail nodes of a
      !>              sublist to be extracted.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     POOL     is the input pool, with the following
      !>              modifications:
      !>
      !>                 -- All of the nodes of the sublist bounded by
      !>                    HEAD and by TAIL have now been returned to
      !>                    the free list.
      !>
      !>                 If on input, HEAD was preceded by the node
      !>                 PREV, and tail was followed by the node
      !>                 NEXT, then on output
      !>
      !>                 -- The successor of PREV is NEXT.
      !>                 -- The predecessor of NEXT is PREV.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If either of HEAD or TAIL are not valid node numbers, the
      !>         error SPICE(INVALIDNODE) is signaled. POOL will not be
      !>         modified.
      !>
      !>     2)  If either of HEAD or TAIL are valid node numbers but are not
      !>         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL
      !>         will not be modified.
      !>
      !>     3)  If TAIL cannot be reached by forward traversal of the list
      !>         containing HEAD, the error SPICE(INVALIDSUBLIST) is signaled.
      !>         POOL will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Deleting a sublist from a list returns all of the nodes in
      !>     the sublist to the free list.
      !>
      !>     To remove a sublist from a list and retain the sublist
      !>     as a second list, use the routine LNKXSL ( LNK, extract
      !>     sublist ).
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool containing the list
      !>
      !>            1002 <--> 3 <--> 7 <--> 88 <--> 2
      !>
      !>         To delete the sublist
      !>
      !>            3 <--> 7 <--> 88
      !>
      !>         the call
      !>
      !>            CALL LNKFSL ( 3, 88, POOL )
      !>
      !>         can be used. The resulting list will be:
      !>
      !>            1002 <--> 2
      !>
      !>         The nodes 3, 7, and 88 will now be on the free list.
      !>
      !>     2)  Let POOL be a doubly linked list pool containing the list
      !>
      !>            1002 <--> 3 <--> 7 <--> 88 <--> 2
      !>
      !>         To free the entire list, the call
      !>
      !>            CALL LNKFSL ( 1002, 2, POOL )
      !>
      !>         should be used.
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module subroutine lnkfsl ( ctx, head, tail, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: head  !! Head and tail nodes of a sublist to be freed.
      integer(ip), intent(in) :: tail  !! Head and tail nodes of a sublist to be freed.
      integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


      integer(ip) :: count
      integer(ip) :: next
      integer(ip) :: node
      integer(ip) :: prev

!
!     HEAD and TAIL must be valid node numbers.  These nodes
!     must be allocated as well.
!
      if (      ( head < 1                   ) &
           .or. ( head > pool(sizrow,sizcol) ) &
           .or. ( tail < 1                   ) &
           .or. ( tail > pool(sizrow,sizcol) )  )  then

         call ctx%setmsg ( 'HEAD was #.  TAIL was #. '                    // &
                       'Valid range is 1 to #.'                        )
         call ctx%errint ( '#', head                                       )
         call ctx%errint ( '#', tail                                       )
         call ctx%errint ( '#', pool(sizrow, sizcol)                       )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                            )
         return

      else if (       ( pool(bckwrd,head)  ==  free ) &
                .or.  ( pool(bckwrd,tail)  ==  free )  ) then

         call ctx%setmsg ( 'Node HEAD: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       'Node TAIL: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       '("FREE" is #)'                                 )
         call ctx%errint ( '#', head                                       )
         call ctx%errint ( '#', pool(bckwrd,head)                          )
         call ctx%errint ( '#', pool(forwrd,head)                          )
         call ctx%errint ( '#', tail                                       )
         call ctx%errint ( '#', pool(bckwrd,tail)                          )
         call ctx%errint ( '#', pool(forwrd,tail)                          )
         call ctx%errint ( '#', free                                       )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Starting at HEAD, search forward, looking for TAIL (apologies to
!     ZZ Top).  Count the nodes in the sublist, while we're at it.
!
      count = 1
      node  = head

      do while (  ( node /= tail ) .and. ( node > 0 )  )
         count  =  count + 1
         node   =  pool ( forwrd, node )
      end do

!
!     If we didn't find TAIL, that's an error.
!
      if ( node /= tail ) then

         call ctx%setmsg ( 'Node # cannot be found by forward '           // &
                       'traversal, starting at node #.'                )
         call ctx%errint ( '#', tail                                       )
         call ctx%errint ( '#', head                                       )
         call ctx%sigerr ( 'SPICE(INVALIDSUBLIST)'                         )
         return

      end if
!
!     We reached TAIL.  Extract the sublist between HEAD and TAIL
!     inclusive.
!

!
!     Find the predecessor of HEAD and the successor of TAIL.
!
      prev = pool ( bckwrd, head )
      next = pool ( forwrd, tail )

!
!     If the input list did not start with HEAD, then we must update
!     the forward pointer of the tail node, as well as the backward
!     pointer of the head node, of the sublist that preceded HEAD.
!
      if ( prev > 0 ) then
!
!        Update the forward pointer of PREV with the forward pointer of
!        TAIL.
!
!        If TAIL had a successor, the predecessor of HEAD will now
!        point forward to it.  If TAIL was the tail of the input list,
!        the forward pointer of TAIL was the negative of the head of
!        the input list---this is the correct forward pointer for the
!        predecessor of HEAD in this case, since the predecessor of
!        HEAD will become the tail of the main list after the sublist
!        ranging from HEAD to TAIL is removed.
!
         pool ( forwrd, prev )   =   next

!
!        If TAIL is the tail of the input list, we must update the
!        backward pointer of the head of the input list to point to
!        the negative of the new tail of the list, which now is PREV.
!
         if ( next <= 0 ) then
!
!           In this case, we can read off the number of the head
!           node from NEXT:  it is just -NEXT.
!
            pool ( bckwrd, -next )  =  -prev

         end if

      end if

!
!     The portion of the input list that preceded HEAD (if such
!     portion existed) has now been taken care of.
!
!     We now must perform the analogous updates to the portion of
!     the input list that followed TAIL.
!
!     If the input list did not end with TAIL, then we must update
!     the backward pointer of the head node, as well as the forward
!     pointer of the tail node, of the sublist that followed TAIL.
!
      if ( next > 0 ) then
!
!        Update the backward pointer of NEXT with the backward pointer
!        of HEAD.
!
!        If HEAD had a predecessor, the successor of TAIL will now
!        point backward to it.  If HEAD was the head of the input list,
!        the backward pointer of HEAD was the negative of the tail of
!        the input list---this is the correct backward pointer for the
!        successor of TAIL in this case, since the successor of TAIL
!        will become the head of the main list after the sublist
!        ranging from HEAD to TAIL is removed.
!
         pool ( bckwrd, next )   =  prev

!
!        If HEAD is the head of the input list, we must update the
!        forward pointer of the tail of the input list to point to
!        the negative of the new head of the list, which now is NEXT.
!
         if ( prev <= 0 ) then
!
!           In this case, we can read off the number of the tail
!           node from PREV:  it is just -PREV.
!
            pool ( forwrd, -prev )  =  -next

         end if

      end if
!
!     The portion of the input list that followed TAIL (if such
!     portion existed) has now been taken care of.
!

!
!     Set the backward pointers of the freed nodes to FREE.
!
      node = head

      do while ( node /= next )
         pool(bckwrd,node) = free
         node              = pool(forwrd,node)
      end do

!
!     Return the sublist to the free list.  Update the free node
!     count.
!
      pool ( forwrd, tail   )  =  pool ( frerow, frecol )
      pool ( frerow, frecol )  =  head
      pool ( nfrrow, nfrcol )  =  pool ( nfrrow, nfrcol )  +  count

      return
      end subroutine lnkfsl

      !> Return the head node of the list containing a specified node.
      !>
      !> The function returns the number of the head node of the list
      !> containing NODE.
      !>
      !>## Detailed Input
      !>
      !>     NODE     is the number of a node in POOL. Normally,
      !>              NODE will designate an allocated node, but NODE
      !>              is permitted to be less than or equal to zero.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of the head node of the list
      !>     containing NODE. If NODE is non-positive, the function returns
      !>     zero.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If the NODE is less than or equal to zero, NODE is not
      !>         considered to be erroneous. The value 0 is returned.
      !>
      !>     2)  If NODE is greater than the size of the pool, the error
      !>         SPICE(INVALIDNODE) is signaled. The value 0 is returned.
      !>
      !>     3)  If NODE is not the number of an allocated node, the error
      !>         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides a convenient way to find the head of a list
      !>     in a doubly linked list pool. The need to find the head of a
      !>     list arises in applications such as buffer management. For
      !>     example, in a system using a "least recently used" buffer
      !>     replacement policy, the head of a list may point to the most
      !>     recently accessed buffer element.
      !>
      !>## Examples
      !>
      !>     1)  If POOL is a doubly linked list pool that contains the list
      !>
      !>            3 <--> 7 <--> 1 <--> 44
      !>
      !>         any of function references
      !>
      !>            HEAD = LNKHL ( 3,  POOL )
      !>            HEAD = LNKHL ( 7,  POOL )
      !>            HEAD = LNKHL ( 44, POOL )
      !>
      !>         will assign the value 3 to HEAD.
      !>
      !>     2)  If POOL is a doubly linked list pool that contains the
      !>         singleton list consisting of the allocated node
      !>
      !>            44
      !>
      !>         the function reference
      !>
      !>            HEAD = LNKHL ( 44, POOL )
      !>
      !>         will assign the value 44 to HEAD.
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module integer(ip) function lnkhl ( ctx, node, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: node  !! Number of a node.
      integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


      integer(ip) :: prev

!
!     If the node is non-positive, we regard it as the nil node.
!
      if ( node < 1 ) then

         lnkhl = 0
         return

!
!     If the node is out of range, something's very wrong.
!
      else if  ( node > pool( sizrow, sizcol ) ) then

         lnkhl = 0

         call ctx%setmsg ( 'NODE was #; valid range is 1 to #.' )
         call ctx%errint ( '#', node                            )
         call ctx%errint ( '#', pool(sizrow,sizcol)             )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                 )
         return

!
!     We don't do free nodes.
!
      else if (  pool( bckwrd, node )  ==  free  ) then

         lnkhl = 0

         call ctx%setmsg ( 'NODE was #; backward pointer = #; '   // &
                       'forward pointer = #. "FREE" is #)'    )
         call ctx%errint ( '#', node                              )
         call ctx%errint ( '#', pool(bckwrd,node)                 )
         call ctx%errint ( '#', pool(forwrd,node)                 )
         call ctx%errint ( '#', free                              )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'               )
         return

      end if

!
!     Find the head of the list.
!
      lnkhl = node
      prev  = pool ( bckwrd, node )

      do while ( prev > 0 )

         lnkhl = prev
         prev  = pool ( bckwrd, lnkhl )

      end do

!
!     LNKHL is now the head of the list.
!
      return
      end function lnkhl

      !> Insert the list containing a specified node into a another list,
      !> following a specified node.
      !>
      !>## Detailed Input
      !>
      !>     PREV     is a node in a list. PREV is permitted to be
      !>              nil, in which case POOL is not modified.
      !>
      !>     LIST     is a node in the list to be inserted. The entire
      !>              list containing the node LIST is to be inserted
      !>              into the list containing PREV. The inserted list
      !>              will be located between PREV and its successor,
      !>              if any.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     POOL     is the input pool, with the following
      !>              modifications:
      !>
      !>                 Let HEAD and TAIL be the head and tail nodes of
      !>                 the list containing LIST. Then on output
      !>
      !>                    -- The successor of PREV is HEAD.
      !>                    -- The predecessor of HEAD is PREV.
      !>
      !>                 Let NEXT be the node that on input was the
      !>                 successor of PREV; if NEXT exists, then on
      !>                 output
      !>
      !>                    -- The successor of TAIL is NEXT.
      !>                    -- The predecessor of NEXT is TAIL.
      !>
      !>                 If NEXT is nil, the forward pointer of the
      !>                 inserted sublist is set to the negative of
      !>                 the head of the list containing PREV.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If LIST is not a valid node number, the error
      !>         SPICE(INVALIDNODE) is signaled. POOL will not be
      !>         modified.
      !>
      !>     2)  If PREV is positive but is not a valid node number, the error
      !>         SPICE(INVALIDNODE) is signaled. POOL will not be
      !>         modified.
      !>
      !>     3)  It is not an error for PREV to be non-positive; if it is,
      !>         the call to this routine does not affect the pool.
      !>
      !>     4)  If either of PREV or LIST are valid node numbers but are not
      !>         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL
      !>         will not be modified.
      !>
      !>     5)  If LIST belongs to the same list as does PREV, this routine
      !>         may fail in mysterious ways. For efficiency, this error
      !>         condition is not checked.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is used for augmenting lists by inserting other
      !>     lists into them. The case of insertion of a single allocated
      !>     node is not special: this is insertion of a singleton list.
      !>
      !>     To insert a list into a list BEFORE a specified element, use the
      !>     routine LNKILB.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool that contains the lists
      !>
      !>             3 <--> 7 <--> 1    and    500 <--> 2 <--> 80
      !>
      !>         To insert the second list into the first after node 7, use the
      !>         call
      !>
      !>             CALL LNKILA ( 7, 500, POOL )
      !>
      !>         The resulting list will be:
      !>
      !>             3 <--> 7 <--> 500 <--> 2 <--> 80 <--> 1
      !>
      !>     2)  Let POOL be a doubly linked list pool that contains 5 nodes.
      !>         The sequence of calls
      !>
      !>            TAIL = 0
      !>
      !>            DO I = 1, 5
      !>               CALL LNKAN  ( POOL, NODE       )
      !>               CALL LNKILA ( TAIL, NODE, POOL )
      !>               TAIL = NODE
      !>            END DO
      !>
      !>         builds the list
      !>
      !>             1 <--> 2 <--> 3 <--> 4 <--> 5
      !>
      !>         Note that the first call to LNKILA does not cause an error
      !>         to be signaled, even though TAIL is 0 at that point.
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
      !>
      !>     2)  For efficiency, discovery check-in is used in this routine.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added note about efficiency in $Restrictions section.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module subroutine lnkila ( ctx, prev, list, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: prev  !! Node after which a new list is to be inserted.
      integer(ip), intent(in) :: list  !! Node in the list to be inserted.
      integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


      integer(ip) :: next
      integer(ip) :: head
      integer(ip) :: tail

!
!     If PREV is non-positive, return now.
!
      if ( prev <= 0 ) return

!
!     At this point, PREV and LIST must be a valid node numbers, and
!     both PREV and LIST must be allocated as well.
!
      if (      ( prev > pool(sizrow,sizcol) ) &
           .or. ( list < 1                   ) &
           .or. ( list > pool(sizrow,sizcol) )  )  then

         call ctx%setmsg ( 'PREV was #.  LIST was #. '                    // &
                       'Valid range is 1 to #.'                        )
         call ctx%errint ( '#', prev                                       )
         call ctx%errint ( '#', list                                       )
         call ctx%errint ( '#', pool(sizrow, sizcol)                       )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                            )
         return

      else if (       ( pool(bckwrd,prev)  ==  free ) &
                .or.  ( pool(bckwrd,list)  ==  free )  ) then

         call ctx%setmsg ( 'Node PREV: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       'Node LIST: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       '("FREE" is #)'                                 )
         call ctx%errint ( '#', prev                                       )
         call ctx%errint ( '#', pool(bckwrd,prev)                          )
         call ctx%errint ( '#', pool(forwrd,prev)                          )
         call ctx%errint ( '#', list                                       )
         call ctx%errint ( '#', pool(bckwrd,list)                          )
         call ctx%errint ( '#', pool(forwrd,list)                          )
         call ctx%errint ( '#', free                                       )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Find the head and tail of the list containing LIST.
!
      head = list

      do while ( pool(bckwrd,head) > 0 )
         head = pool(bckwrd,head)
      end do

      tail = -pool(bckwrd,head)

!
!     Let NEXT be the forward pointer of PREV.
!
!     Insert HEAD after PREV.
!
!     If PREV has a successor, TAIL precedes it.
!
!     If PREV has no successor, TAIL is the new tail of the list.
!     The backward pointer of the head of the merged list should
!     be set to -TAIL.
!
!     In either case, the forward pointer of TAIL should be set
!     to the forward pointer of PREV.
!
      next                  =  pool( forwrd, prev )
      pool( forwrd, prev )  =  head
      pool( bckwrd, head )  =  prev

      if ( next > 0 ) then
         pool( bckwrd,  next )  =   tail
      else
         pool( bckwrd, -next )  =  -tail
      end if

      pool( forwrd, tail )  =  next

      return
      end subroutine lnkila

      !> Insert the list containing a specified node into a another list,
      !> preceding a specified node.
      !>
      !>## Detailed Input
      !>
      !>     LIST     is a node in the list to be inserted. The entire
      !>              list containing LIST is to be inserted into the
      !>              list containing NEXT. The inserted list will be
      !>              located between NEXT and its predecessor, if any.
      !>
      !>     NEXT     is a node in a list. NEXT is permitted to be
      !>              nil, in which case POOL is not modified.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     POOL     is the input pool, with the following
      !>              modifications:
      !>
      !>                 Let HEAD and TAIL be the head and tail nodes of
      !>                 the list containing LIST. Then on output
      !>
      !>                    -- The successor of TAIL is NEXT.
      !>
      !>                    -- The predecessor of NEXT is TAIL.
      !>
      !>                 Let PREV be the node that on input was the
      !>                 predecessor of NEXT; if PREV exists, then on
      !>                 output
      !>
      !>                    -- The successor of PREV is HEAD.
      !>
      !>                    -- The predecessor of HEAD is PREV.
      !>
      !>                 If PREV is nil, the backward pointer of the
      !>                 inserted sublist is set to the negative of
      !>                 the tail of the list containing NEXT.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     For efficiency, discovery check-in is used in this routine.
      !>
      !>     1)  If LIST is not a valid node number, the error
      !>         SPICE(INVALIDNODE) is signaled. POOL will not be
      !>         modified.
      !>
      !>     2)  If NEXT is positive but is not a valid node number, the error
      !>         SPICE(INVALIDNODE) is signaled. POOL will not be
      !>         modified.
      !>
      !>     3)  It is not an error for NEXT to be non-positive; if it is,
      !>         the call to this routine does not affect the pool.
      !>
      !>     4)  If either of LIST or NEXT are valid node numbers but are not
      !>         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL
      !>         will not be modified.
      !>
      !>     5)  If LIST belongs to the same list as does NEXT, this routine
      !>         may fail in mysterious ways. For efficiency, this error
      !>         condition is not checked.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is used for augmenting lists by inserting other
      !>     lists into them. The case of insertion of a single allocated
      !>     node is not special: this is insertion of a singleton list.
      !>
      !>     To insert a list into a list AFTER a specified element, use the
      !>     routine LNKILA.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool that contains the lists
      !>
      !>             3 <--> 7 <--> 1    and    500 <--> 2 <--> 80
      !>
      !>         To insert the second list into the first before node 7, use
      !>         the call
      !>
      !>             CALL LNKILB ( 500, 7, POOL )
      !>
      !>         The resulting list will be:
      !>
      !>             3 <--> 500 <--> 2 <--> 80 <--> 7 <--> 1
      !>
      !>     2)  Let POOL be a doubly linked list pool that contains 5 nodes.
      !>         The sequence of calls
      !>
      !>            HEAD = 0
      !>
      !>            DO I = 1, 5
      !>               CALL LNKAN  ( POOL, NODE       )
      !>               CALL LNKILB ( NODE, HEAD, POOL )
      !>               HEAD = NODE
      !>            END DO
      !>
      !>         builds the list
      !>
      !>             5 <--> 4 <--> 3 <--> 2 <--> 1
      !>
      !>         Note that the first call to LNKILB does not cause an error
      !>         to be signaled, even though HEAD is 0 at that point.
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
      !>
      !>     2)  For efficiency, discovery check-in is used in this routine.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Added note about efficiency in $Restrictions section.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module subroutine lnkilb ( ctx, list, next, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: list  !! Node in the list to be inserted.
      integer(ip), intent(in) :: next  !! Node before which a new list is to be inserted.
      integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


      integer(ip) :: head
      integer(ip) :: prev
      integer(ip) :: tail

!
!     If NEXT is non-positive, return now.
!
      if ( next <= 0 ) return

!
!     If we arrived here, NEXT and LIST must be valid node numbers.
!     These nodes must be allocated as well.
!
      if (      ( next > pool(sizrow,sizcol) ) &
           .or. ( list < 1                   ) &
           .or. ( list > pool(sizrow,sizcol) )  )  then

         call ctx%setmsg ( 'NEXT was #.  LIST was #. '                    // &
                       'Valid range is 1 to #.'                        )
         call ctx%errint ( '#', next                                       )
         call ctx%errint ( '#', list                                       )
         call ctx%errint ( '#', pool(sizrow, sizcol)                       )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                            )
         return

      else if (       ( pool(bckwrd,next)  ==  free ) &
                .or.  ( pool(bckwrd,list)  ==  free )  ) then

         call ctx%setmsg ( 'Node NEXT: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       'Node LIST: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       '("FREE" is #)'                                 )
         call ctx%errint ( '#', next                                       )
         call ctx%errint ( '#', pool(bckwrd,next)                          )
         call ctx%errint ( '#', pool(forwrd,next)                          )
         call ctx%errint ( '#', list                                       )
         call ctx%errint ( '#', pool(bckwrd,list)                          )
         call ctx%errint ( '#', pool(forwrd,list)                          )
         call ctx%errint ( '#', free                                       )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Find the head and tail of the list containing LIST.
!
      head = list

      do while ( pool(bckwrd,head) > 0 )
         head = pool(bckwrd,head)
      end do

      tail = -pool(bckwrd,head)

!
!     Let PREV be the backward pointer of NEXT.
!
!     Insert TAIL before NEXT.
!
!     If NEXT has a predecessor, HEAD follows it.
!
!     If NEXT has no predecessor, HEAD is the new head of the list.
!     The forward pointer of the tail of the merged list should
!     be set to -HEAD.
!
!     In either case, the backward pointer of HEAD should be set
!     to the backward pointer of NEXT.
!
      prev                  =  pool( bckwrd, next )
      pool( forwrd, tail )  =  next
      pool( bckwrd, next )  =  tail

      if ( prev > 0 ) then
         pool( forwrd,  prev )  =   head
      else
         pool( forwrd, -prev )  =  -head
      end if

      pool( bckwrd, head )  =  prev

      return
      end subroutine lnkilb

      !> Initialize a doubly linked list pool.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the number of nodes in the pool.
      !>
      !>     POOL     is an integer array that will contain the linked
      !>              pool on output.
      !>
      !>## Detailed Output
      !>
      !>     POOL     is an initialized doubly linked list pool.
      !>              The status of the pool is as follows:
      !>
      !>                --  All nodes in the pool are on the free list.
      !>
      !>                --  The free pointer indicates the first node.
      !>
      !>                --  The total node count is set to the input
      !>                    value, SIZE.
      !>
      !>                --  The free node count is the input value, SIZE.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested number of nodes is nonpositive, the error
      !>         SPICE(INVALIDCOUNT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     LNKINI must be called once to initialize a doubly linked list
      !>     pool before the pool is used. LNKINI can be called at any time
      !>     to re-initialize a doubly linked list pool.
      !>
      !>     The functions
      !>
      !>        LNKNFN ( LNK, number of free nodes ) and
      !>        LNKSIZ ( LNK, size of pool )
      !>
      !>     will both return the value PLSIZE if called immediately after a
      !>     call to LNKINI.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool with a maximum of
      !>         100 nodes. POOL should be declared as follows:
      !>
      !>            INTEGER               LBPOOL
      !>            PARAMETER           ( LBPOOL = -5 )
      !>
      !>            INTEGER               PLSIZE
      !>            PARAMETER           ( PLSIZE = 100 )
      !>
      !>            INTEGER               POOL ( 2, LBPOOL : PLSIZE )
      !>
      !>         To initialize POOL, use the call
      !>
      !>            CALL LNKINI ( PLSIZE, POOL )
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module subroutine lnkini ( ctx, size, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: size  !! Number of nodes in the pool.
      integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! An array that is a linked pool on
                                                             !! output.


      integer(ip) :: i

!
!     The requested number of nodes must be valid.
!
      if ( size < 1 ) then

         call ctx%setmsg ( 'A linked list cannot have # nodes.' )
         call ctx%errint ( '#', size                            )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                )
         return

      end if

!
!     Initialize the pool.  The free list occupies the whole pool at
!     this point.
!

!
!     POOL( SIZROW, SIZCOL ) is the pool size.
!
      pool( sizrow, sizcol )  =  size

!
!     POOL( NFRROW, NFRCOL ) is the number of free nodes.
!
      pool( nfrrow, nfrcol )  =  size

!
!     POOL( FREROW, FRECOL) is the "free" pointer.  It points to the
!     first free node, which is node 1.
!
      pool( frerow, frecol)   =  1

!
!     Initialize the backward and forward pointers.  The last forward
!     pointer is zero.  All of the backward pointers contain the value
!     FREE.
!
      do i = 1, size - 1
         pool ( forwrd, i )   =  i + 1
         pool ( bckwrd, i )   =  free
      end do

      pool ( forwrd, size )   =  0
      pool ( bckwrd, size )   =  free

      return
      end subroutine lnkini

      !> Return the number of free nodes in a doubly linked list pool.
      !>
      !> The function returns the number of free nodes in the pool.
      !>
      !>## Detailed Input
      !>
      !>     SIZE     is the number of nodes in the pool.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of free nodes in the pool.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
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
      !>     This routine allows the caller to find the number of free nodes
      !>     available in a doubly linked list pool, without having to make
      !>     use of knowledge of the internal structure of the pool.
      !>
      !>     Routines that allocate nodes can use this routine to determine
      !>     how many nodes can be allocated safely---an attempt to allocate
      !>     a node when no free nodes are available causes a SPICELIB error
      !>     to be signaled.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool containing 5 nodes.
      !>         If POOL contains the list
      !>
      !>            4 <--> 5 <--> 1 <--> 2
      !>
      !>         and the node 3 is unallocated, then the function reference
      !>
      !>            NFREE  =  LNKNFN ( POOL )
      !>
      !>         will assign the value 1 to NFREE.
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module integer(ip) function lnknfn ( ctx, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


!
!     Grab the number of free nodes from the control area.
!
      lnknfn  =  pool ( nfrrow, nfrcol )

      return
      end function lnknfn

      !> Find the node following a specified node in a doubly linked list
      !> pool.
      !>
      !> The function returns the number of the successor of the node
      !> indicated by NODE.
      !>
      !>## Detailed Input
      !>
      !>     NODE     is the number of an allocated node in POOL.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of the successor of the node
      !>     indicated by NODE. If NODE is the tail node of a list, the
      !>     function returns the negative of the node number of the head
      !>     of the list.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If NODE is the tail node of a list, the function returns the
      !>         negative of the node number of the head of the list.
      !>
      !>     2)  If NODE is not a valid node number, the error
      !>         SPICE(INVALIDNODE) is signaled. The value 0 is returned.
      !>
      !>     3)  If NODE is not the number of an allocated node, the error
      !>         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The raison d'etre of this routine is to allow forward traversal
      !>     of lists in a doubly linked list pool.
      !>
      !>     Traversing a list is often performed in cases where the list is
      !>     used to index elements of a data structure, and the elements
      !>     indexed by the list must be searched.
      !>
      !>     To traverse a list in backward order, use LNKPRV.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be doubly linked list pool, and let
      !>
      !>           3 <--> 7 <--> 1
      !>
      !>         be a list in the pool. The table below shows the effects
      !>         of function references to LNKNXT, where nodes in this list
      !>         are used as inputs:
      !>
      !>            Function reference               Value Returned
      !>            ------------------               --------------
      !>
      !>            LNKNXT ( 3, POOL )                     7
      !>            LNKNXT ( 7, POOL )                     1
      !>            LNKNXT ( 1, POOL )                    -3
      !>
      !>     2)  Forward traversal of a list: Let POOL be a doubly linked
      !>         list pool, and let NODE be an allocated node in the pool.
      !>         To traverse the list containing NODE in forward order
      !>         and print out the nodes of the list, we can use the
      !>         following code fragment:
      !>
      !>            C
      !>            C     Find the head of the list containing NODE.
      !>            C
      !>                  NEXT = LNKHL ( NODE, POOL )
      !>
      !>            C
      !>            C     Traverse the list, printing out node numbers
      !>            C     as we go.
      !>            C
      !>                  WRITE (*,*) 'The list, in forward order, is: '
      !>
      !>                  DO WHILE ( NEXT .GT. 0 )
      !>
      !>                     WRITE (*,*) NEXT
      !>                     NEXT = LNKNXT ( NEXT, POOL )
      !>
      !>                  END DO
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module integer(ip) function lnknxt ( ctx, node, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: pool ( 2,  lbpool :  *  )  !! A doubly linked list pool.
      integer(ip), intent(in) :: node  !! Number of an allocated node.


!
!     If the node is out of range, something's very wrong.
!
      if (      ( node < 1                       ) &
           .or. ( node > pool( sizrow, sizcol )  )  ) then

         lnknxt = 0

         call ctx%setmsg ( 'NODE was #; valid range is 1 to #.' )
         call ctx%errint ( '#', node                            )
         call ctx%errint ( '#', pool(sizrow,sizcol)             )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                 )
         return

!
!     We don't do free nodes.
!
      else if (  pool( bckwrd, node )  ==  free  ) then

         lnknxt = 0

         call ctx%setmsg ( 'NODE was #; backward pointer = #; '           // &
                       'forward pointer = #. "FREE" is #)'             )
         call ctx%errint ( '#', node                                       )
         call ctx%errint ( '#', pool(bckwrd,node)                          )
         call ctx%errint ( '#', pool(forwrd,node)                          )
         call ctx%errint ( '#', free                                       )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Just return the forward pointer of NODE.
!
      lnknxt = pool ( forwrd, node )

      return
      end function lnknxt

      !> Find the node preceding a specified node in a doubly linked list
      !> pool.
      !>
      !> The function returns the number of the predecessor of the node
      !> indicated by NODE.
      !>
      !>## Detailed Input
      !>
      !>     NODE     is the number of an allocated node in POOL.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of the predecessor of the node
      !>     indicated by NODE. If NODE is the head node of a list, the
      !>     function returns the negative of the node number of the tail
      !>     of the list.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If NODE is the head node of a list, the function returns the
      !>         negative of the node number of the tail of the list.
      !>
      !>     2)  If NODE is not a valid node number, the error
      !>         SPICE(INVALIDNODE) is signaled. The value 0 is returned.
      !>
      !>     3)  If NODE is not the number of an allocated node, the error
      !>         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The raison d'etre of this routine is to allow backward traversal
      !>     of lists in a doubly linked list pool.
      !>
      !>     Traversing a list is often performed in cases where the list is
      !>     used to index elements of a data structure, and the elements
      !>     indexed by the list must be searched.
      !>
      !>     To traverse a list in forward order, use LNKNXT.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be doubly linked list pool, and let
      !>
      !>           3 <--> 7 <--> 1
      !>
      !>         be a list in the pool. The table below shows the effects
      !>         of function references to LNKPRV, where nodes in this list
      !>         are used as inputs:
      !>
      !>            Function reference               Value Returned
      !>            ------------------               --------------
      !>
      !>            LNKPRV ( 1, POOL )                     7
      !>            LNKPRV ( 7, POOL )                     3
      !>            LNKPRV ( 3, POOL )                    -1
      !>
      !>     2)  Backward traversal of a list: Let POOL be a doubly linked
      !>         list pool, and let NODE be an allocated node in the pool.
      !>         To traverse the list containing NODE in backward order
      !>         and print out the nodes of the list, we can use the
      !>         following code fragment:
      !>
      !>            C
      !>            C     Find the tail of the list containing NODE.
      !>            C
      !>                  PREV = LNKTL ( NODE, POOL )
      !>
      !>            C
      !>            C     Traverse the list, printing out node numbers
      !>            C     as we go.
      !>            C
      !>                  WRITE (*,*) 'The list, in backward order, is: '
      !>
      !>                  DO WHILE ( PREV .GT. 0 )
      !>
      !>                     WRITE (*,*) PREV
      !>                     PREV = LNKPRV ( PREV, POOL )
      !>
      !>                  END DO
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module integer(ip) function lnkprv ( ctx, node, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      integer(ip), intent(in) :: node  !! Number of an allocated node.


!
!     If the node is out of range, something's very wrong.
!
      if (      ( node < 1                       ) &
           .or. ( node > pool( sizrow, sizcol )  )  ) then

         lnkprv = 0

         call ctx%setmsg ( 'NODE was #; valid range is 1 to #.' )
         call ctx%errint ( '#', node                            )
         call ctx%errint ( '#', pool(sizrow,sizcol)             )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                 )
         return

!
!     We don't do free nodes.
!
      else if (  pool( bckwrd, node )  ==  free  ) then

         lnkprv = 0

         call ctx%setmsg ( 'NODE was #; backward pointer = #; '           // &
                       'forward pointer = #. "FREE" is #)'             )
         call ctx%errint ( '#', node                                       )
         call ctx%errint ( '#', pool(bckwrd,node)                          )
         call ctx%errint ( '#', pool(forwrd,node)                          )
         call ctx%errint ( '#', free                                       )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Just return the backward pointer of NODE.
!
      lnkprv = pool ( bckwrd, node )

      return
      end function lnkprv

      !> Return the size of a doubly linked list pool.
      !>
      !> The function returns the size of the pool.
      !>
      !>## Detailed Input
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the size (total number of nodes) of the pool.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
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
      !>     This routine allows an application program to determine the size
      !>     of a doubly linked list pool at run-time, without having to rely
      !>     on knowledge of the internals of the doubly linked list pool
      !>     structure.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool. The total number of
      !>         nodes in a doubly linked list pool is set when the pool is
      !>         initialized by LNKINI. This number is returned by LNKSIZ:
      !>
      !>            C
      !>            C     This sequence of calls will assign the value 100
      !>            C     to the variable SIZE:
      !>            C
      !>                  CALL LNKINI ( 100, POOL )
      !>                  SIZE = LNKSIZ ( POOL )
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module integer(ip) function lnksiz ( ctx, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


!
!     Grab the pool size from the control area.
!
      lnksiz  =  pool ( sizrow, sizcol )

      return
      end function lnksiz

      !> Return the tail node of the list containing a specified node.
      !>
      !> The function returns the number of the tail node of the list
      !> containing NODE.
      !>
      !>## Detailed Input
      !>
      !>     NODE     is the number of a node in POOL. Normally,
      !>              NODE will designate an allocated node, but NODE
      !>              is permitted to be less than or equal to zero.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of the tail node of the list
      !>     containing NODE. If NODE is non-positive, the function returns
      !>     zero.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If the NODE is less than or equal to zero, NODE is not
      !>         considered to be erroneous. The value 0 is returned.
      !>
      !>     2)  If NODE is greater than the size of the pool, the error
      !>         SPICE(INVALIDNODE) is signaled. The value 0 is returned.
      !>
      !>     3)  If NODE is not the number of an allocated node, the error
      !>         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides a convenient way to find the tail of a list
      !>     in a doubly linked list pool. The need to find the tail of a
      !>     list arises in applications such as buffer management. For
      !>     example, in a system using a "least recently used" buffer
      !>     replacement policy, the tail of a list may point to the least
      !>     recently accessed buffer element.
      !>
      !>## Examples
      !>
      !>     1)  If POOL is a doubly linked list pool that contains the list
      !>
      !>            3 <--> 7 <--> 1 <--> 44
      !>
      !>         any of function references
      !>
      !>            TAIL = LNKTL ( 3,  POOL )
      !>            TAIL = LNKTL ( 7,  POOL )
      !>            TAIL = LNKTL ( 44, POOL )
      !>
      !>         will assign the value 44 to TAIL.
      !>
      !>     2)  If POOL is a doubly linked list pool that contains the
      !>         singleton list consisting of the allocated node
      !>
      !>            44
      !>
      !>         the function reference
      !>
      !>            TAIL = LNKTL ( 44, POOL )
      !>
      !>         will assign the value 44 to TAIL.
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool,
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.1.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 09-JAN-1997 (NJB)
      !>
      !>        Corrected module name in one pair of CHKIN/CHKOUT calls.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 09-JAN-1997 (NJB)
      !>
      !>        Corrected module name in one pair of CHKIN/CHKOUT calls. The
      !>        affected error case was the check for a node number being out
      !>        of range.
      module integer(ip) function lnktl ( ctx, node, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: node  !! Number of a node.
      integer(ip), intent(in) :: pool (  2,  lbpool : *  )  !! A doubly linked list pool.


      integer(ip) :: next

!
!     If the node is non-positive, we regard it as the nil node.
!
      if ( node < 1 ) then

         lnktl = 0
         return

!
!     If the node is out of range, something's very wrong.
!
      else if  ( node > pool( sizrow, sizcol ) ) then

         lnktl = 0

         call ctx%setmsg ( 'NODE was #; valid range is 1 to #.' )
         call ctx%errint ( '#', node                            )
         call ctx%errint ( '#', pool(sizrow,sizcol)             )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                 )
         return

!
!     We don't do free nodes.
!
      else if (  pool( bckwrd, node )  ==  free  ) then

         lnktl = 0

         call ctx%setmsg ( 'NODE was #; backward pointer = #; '  // &
                       'forward pointer = #. "FREE" is #)'   )
         call ctx%errint ( '#', node                             )
         call ctx%errint ( '#', pool(bckwrd,node)                )
         call ctx%errint ( '#', pool(forwrd,node)                )
         call ctx%errint ( '#', free                             )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'              )
         return

      end if

!
!     Find the tail of the list.
!
      lnktl = node
      next  = pool ( forwrd, node )

      do while ( next > 0 )

         lnktl = next
         next  = pool ( forwrd, lnktl )

      end do

!
!     LNKTL is now the tail of the list.
!
      return
      end function lnktl

      !> Extract a specified sublist from a list.
      !>
      !>## Detailed Input
      !>
      !>     HEAD,
      !>     TAIL     are, respectively, the head and tail nodes of a
      !>              sublist to be extracted.
      !>
      !>     POOL     is a doubly linked list pool.
      !>
      !>## Detailed Output
      !>
      !>     POOL     is the input pool, with the following
      !>              modifications:
      !>
      !>                 -- The sublist bounded by HEAD and
      !>                    by TAIL is now a separate list from
      !>                    the list that originally contained it.
      !>
      !>                 If on input, HEAD was preceded by the node
      !>                 PREV, and tail was followed by the node
      !>                 NEXT, then on output
      !>
      !>                 -- The successor of PREV is NEXT.
      !>                 -- The predecessor of NEXT is PREV.
      !>
      !>## Parameters
      !>
      !>     LBPOOL   is the lower bound of the column indices of the POOL
      !>              array. The columns indexed LBPOOL to 0 are reserved
      !>              as a control area for the pool.
      !>
      !>## Exceptions
      !>
      !>     1)  If either of HEAD or TAIL are not valid node numbers, the
      !>         error SPICE(INVALIDNODE) is signaled. POOL will not be
      !>         modified.
      !>
      !>     2)  If either of HEAD or TAIL are valid node numbers but are not
      !>         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL
      !>         will not be modified.
      !>
      !>     3)  If TAIL cannot be reached by forward traversal of the list
      !>         containing HEAD, the error SPICE(INVALIDSUBLIST) is signaled.
      !>         POOL will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Extracting a sublist from a list is necessary when a list is
      !>     to be re-arranged in some way. For example, to move a node
      !>     in a list to the head of the list, the node (which is a
      !>     singleton sublist) is first extracted from the list containing
      !>     it, then inserted before the head of the list.
      !>
      !>## Examples
      !>
      !>     1)  Let POOL be a doubly linked list pool, and let
      !>
      !>            9 <--> 8 <--> 4 <--> 2000 <--> 1
      !>
      !>         be a list in POOL. To extract the sublist
      !>
      !>            4 <--> 2000
      !>
      !>         the call
      !>
      !>            CALL LNKXSL ( 4, 2000, POOL )
      !>
      !>         can be used. After the call is made, POOL will contain the
      !>         separate lists
      !>
      !>            9 <--> 8 <--> 1
      !>
      !>         and
      !>
      !>            4 <--> 2000
      !>
      !>     2)  Let POOL be a doubly linked list pool, and let
      !>
      !>            9 <--> 8 <--> 4 <--> 2000 <--> 1
      !>
      !>         be a list in POOL. To move the node 2000 to the
      !>         head of the list, the code fragment
      !>
      !>            CALL LNKXSL ( 2000, 2000, POOL )
      !>            CALL LNKILB ( 2000, 9,    POOL )
      !>
      !>         can be used. The resulting list will be
      !>
      !>            2000 <--> 9 <--> 8 <--> 4 <--> 1
      !>
      !>## Restrictions
      !>
      !>     1)  Linked list pools must be initialized via the routine
      !>         LNKINI. Failure to initialize a linked list pool
      !>         will almost certainly lead to confusing results.
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
      !>-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
      module subroutine lnkxsl ( ctx, head, tail, pool )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: head  !! Head and tail nodes of a sublist to be extracted.
      integer(ip), intent(in) :: tail  !! Head and tail nodes of a sublist to be extracted.
      integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.


      integer(ip) :: next
      integer(ip) :: node
      integer(ip) :: prev

!
!     HEAD and TAIL must be valid node numbers.  These nodes
!     must be allocated as well.
!
      if (      ( head < 1                   ) &
           .or. ( head > pool(sizrow,sizcol) ) &
           .or. ( tail < 1                   ) &
           .or. ( tail > pool(sizrow,sizcol) )  )  then

         call ctx%setmsg ( 'HEAD was #.  TAIL was #. '                    // &
                       'Valid range is 1 to #.'                        )
         call ctx%errint ( '#', head                                       )
         call ctx%errint ( '#', tail                                       )
         call ctx%errint ( '#', pool(sizrow, sizcol)                       )
         call ctx%sigerr ( 'SPICE(INVALIDNODE)'                            )
         return

      else if (       ( pool(bckwrd,head)  ==  free ) &
                .or.  ( pool(bckwrd,tail)  ==  free )  ) then

         call ctx%setmsg ( 'Node HEAD: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       'Node TAIL: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       '("FREE" is #)'                                 )
         call ctx%errint ( '#', head                                       )
         call ctx%errint ( '#', pool(bckwrd,head)                          )
         call ctx%errint ( '#', pool(forwrd,head)                          )
         call ctx%errint ( '#', tail                                       )
         call ctx%errint ( '#', pool(bckwrd,tail)                          )
         call ctx%errint ( '#', pool(forwrd,tail)                          )
         call ctx%errint ( '#', free                                       )
         call ctx%sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Starting at HEAD, search forward, looking for TAIL (apologies to
!     ZZ Top).
!
      node = head

      do while (  ( node /= tail ) .and. ( node > 0 )  )
         node = pool ( forwrd, node )
      end do

!
!     If we didn't find TAIL, that's an error.
!
      if ( node /= tail ) then

         call ctx%setmsg ( 'Node # cannot be found by forward '           // &
                       'traversal, starting at node #.'                )
         call ctx%errint ( '#', tail                                       )
         call ctx%errint ( '#', head                                       )
         call ctx%sigerr ( 'SPICE(INVALIDSUBLIST)'                         )
         return

      end if

!
!     We reached TAIL.  Extract the sublist between HEAD and TAIL
!     inclusive.
!
!     Find the predecessor of HEAD and the successor of TAIL.
!
      prev = pool ( bckwrd, head )
      next = pool ( forwrd, tail )

!
!     If the input list did not start with HEAD, then we must update
!     the forward pointer of the tail node, as well as the backward
!     pointer of the head node, of the sublist that preceded HEAD.
!
      if ( prev > 0 ) then
!
!        Update the forward pointer of PREV with the forward pointer of
!        TAIL.
!
!        If TAIL had a successor, the predecessor of HEAD will now
!        point forward to it.  If TAIL was the tail of the input list,
!        the forward pointer of TAIL was the negative of the head of
!        the input list---this is the correct forward pointer for the
!        predecessor of HEAD in this case, since the predecessor of
!        HEAD will become the tail of the main list after the sublist
!        ranging from HEAD to TAIL is removed.
!
         pool ( forwrd, prev )   =   next

!
!        If TAIL is the tail of the input list, we must update the
!        backward pointer of the head of the input list to point to
!        the negative of the new tail of the list, which now is PREV.
!
         if ( next <= 0 ) then
!
!           In this case, we can read off the number of the head
!           node from NEXT:  it is just -NEXT.
!
            pool ( bckwrd, -next )  =  -prev

         end if

      end if

!
!     The portion of the input list that preceded HEAD (if such
!     portion existed) has now been taken care of.
!
!     We now must perform the analogous updates to the portion of
!     the input list that followed TAIL.
!
!     If the input list did not end with TAIL, then we must update
!     the backward pointer of the head node, as well as the forward
!     pointer of the tail node, of the sublist that followed TAIL.
!
      if ( next > 0 ) then
!
!        Update the backward pointer of NEXT with the backward pointer
!        of HEAD.
!
!        If HEAD had a predecessor, the successor of TAIL will now
!        point backward to it.  If HEAD was the head of the input list,
!        the backward pointer of HEAD was the negative of the tail of
!        the input list---this is the correct backward pointer for the
!        successor of TAIL in this case, since the successor of TAIL
!        will become the head of the main list after the sublist
!        ranging from HEAD to TAIL is removed.
!
         pool ( bckwrd, next )   =  prev

!
!        If HEAD is the head of the input list, we must update the
!        forward pointer of the tail of the input list to point to
!        the negative of the new head of the list, which now is NEXT.
!
         if ( prev <= 0 ) then
!
!           In this case, we can read off the number of the tail
!           node from PREV:  it is just -PREV.
!
            pool ( forwrd, -prev )  =  -next

         end if

      end if
!
!     The portion of the input list that followed TAIL (if such
!     portion existed) has now been taken care of.
!

!
!     Cauterize the sublist.
!
      pool ( bckwrd, head )  =  -tail
      pool ( forwrd, tail )  =  -head

      return
      end subroutine lnkxsl

end submodule pepper_array_ops
