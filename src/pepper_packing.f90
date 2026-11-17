!> Integers packed into double precision words (PEPPER): the one place in
!> the library where the bits of doubles are read as integers, or the bits
!> of integers stored as doubles.
!>
!> DAF summaries (segment descriptors) and DAF summary records hold their
!> integer components packed two to a double precision word, as SPICELIB
!> wrote them through EQUIVALENCE statements. Such a word is not a number:
!> as a real it is typically a denormal or a NaN, so it must only be
!> copied, never compared or used in arithmetic (denormals compare equal
!> to zero when they are flushed to zero, as is ifx's default; a NaN is not
!> equal even to itself). These subroutines convert between the two
!> views with TRANSFER, bit for bit.
module pepper_packing

   use pepper_kinds, only: ip, wp

   implicit none

   private

   public :: pack_integers, unpack_integers

contains

   !> The double precision words holding integers packed two to a word, in
   !> order (the first integer in the first half of the first word, in
   !> memory order); the second half of the last word is zero when the
   !> number of integers is odd.
   !>
   !> A subroutine converting one word at a time, so that a call makes no
   !> temporary arrays: [[dafus]] and [[dafps]] run for every segment
   !> summary a search examines.
   pure subroutine pack_integers(ints, n, words)
      integer(ip), intent(in) :: n  !! the number of integers
      integer(ip), intent(in) :: ints(n)  !! the integers
      real(wp), intent(out) :: words((n + 1)/2)  !! the words
      integer(ip) :: i
      do i = 1, n/2
         words(i) = transfer([ints(2*i - 1), ints(2*i)], 0.0_wp)
      end do
      if (mod(n, 2) == 1) words((n + 1)/2) = transfer([ints(n), 0_ip], 0.0_wp)
   end subroutine pack_integers

   !> The first N integers packed in double precision words, as
   !> [[pack_integers]] packs them (one word at a time, no temporary
   !> arrays).
   pure subroutine unpack_integers(words, n, ints)
      integer(ip), intent(in) :: n  !! the number of integers
      real(wp), intent(in) :: words((n + 1)/2)  !! the words
      integer(ip), intent(out) :: ints(n)  !! the integers
      integer(ip) :: i
      integer(ip) :: two(2)
      do i = 1, n/2
         two = transfer(words(i), two)
         ints(2*i - 1) = two(1)
         ints(2*i) = two(2)
      end do
      if (mod(n, 2) == 1) then
         two = transfer(words((n + 1)/2), two)
         ints(n) = two(1)
      end if
   end subroutine unpack_integers

end module pepper_packing
