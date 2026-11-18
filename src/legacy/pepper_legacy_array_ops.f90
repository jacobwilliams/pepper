!> The legacy interface's routines of the subsystem array_ops (30 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_array_ops

   use pepper_instance_module, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbpool

   implicit none

   private
   public :: cyacip, cyadip, cyaiip, cyclac, cyclad, cyclai, inslac, inslad, inslai, remlac, &
      remlad, remlai, swapac, swapad, swapai, packac, packad, packai, lnkan, lnkfsl, lnkhl, &
      lnkila, lnkilb, lnkini, lnknfn, lnknxt, lnkprv, lnksiz, lnktl, lnkxsl

contains

      !> Cycle the elements of a character array forward or backward
      !> in place.
      !>
      !> Legacy interface: calls ctx%cyacip with the legacy context.
      subroutine cyacip ( nelt, dir, ncycle, array )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nelt  !! Number of elements.
      character(1) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip) :: ncycle  !! Number of times to cycle.
      character(*) :: array    ( * )  !! Array to be cycled/cycled array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dir is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%cyacip ( nelt, (dir), ncycle, array )
      end subroutine cyacip

      !> Cycle the elements of a double precision array forward
      !> or backward in place.
      !>
      !> Legacy interface: calls ctx%cyadip with the legacy context.
      subroutine cyadip ( nelt, dir, ncycle, array )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nelt  !! Number of elements.
      character(1) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip) :: ncycle  !! Number of times to cycle.
      real(wp) :: array    ( * )  !! Array to be cycled/cycled array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cyadip ( nelt, dir, ncycle, array )
      end subroutine cyadip

      !> Cycle the elements of an integer array forward or backward
      !> in place.
      !>
      !> Legacy interface: calls ctx%cyaiip with the legacy context.
      subroutine cyaiip ( nelt, dir, ncycle, array )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nelt  !! Number of elements.
      character(1) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip) :: ncycle  !! Number of times to cycle.
      integer(ip) :: array    ( * )  !! Array to be cycled/cycled array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cyaiip ( nelt, dir, ncycle, array )
      end subroutine cyaiip

      !> Cycle the elements of a character array forward or backward.
      !>
      !> Legacy interface: calls ctx%cyclac with the legacy context.
      subroutine cyclac ( array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: array    ( * )  !! Input array.
      integer(ip) :: nelt  !! Number of elements.
      character(1) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip) :: ncycle  !! Number of times to cycle.
      character(*) :: out      ( * )  !! Cycled array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dir is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%cyclac ( array, nelt, (dir), ncycle, out )
      end subroutine cyclac

      !> Cycle the elements of a double precision array forward
      !> or backward.
      !>
      !> Legacy interface: calls ctx%cyclad with the legacy context.
      subroutine cyclad ( array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: array    ( * )  !! Input array.
      integer(ip) :: nelt  !! Number of elements.
      character(1) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip) :: ncycle  !! Number of times to cycle.
      real(wp) :: out      ( * )  !! Cycled array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cyclad ( array, nelt, dir, ncycle, out )
      end subroutine cyclad

      !> Cycle the elements of an integer array forward or backward.
      !>
      !> Legacy interface: calls ctx%cyclai with the legacy context.
      subroutine cyclai ( array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: array    ( * )  !! Input array.
      integer(ip) :: nelt  !! Number of elements.
      character(1) :: dir  !! Direction to cycle: 'F' or 'B'.
      integer(ip) :: ncycle  !! Number of times to cycle.
      integer(ip) :: out      ( * )  !! Cycled array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cyclai ( array, nelt, dir, ncycle, out )
      end subroutine cyclai

      !> Insert one or more elements into a character array at the
      !> indicated location.
      !>
      !> Legacy interface: calls ctx%inslac with the legacy context.
      subroutine inslac ( elts, ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: elts  ( * )  !! Elements to be inserted.
      integer(ip) :: ne  !! Number of elements to be inserted.
      integer(ip) :: loc  !! Location of the first inserted element.
      character(*) :: array ( * )  !! Input/output array.
      integer(ip) :: na  !! Number of elements in the input/output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%inslac ( elts, ne, loc, array, na )
      end subroutine inslac

      !> Insert one or more elements into a double precision array at
      !> the indicated location.
      !>
      !> Legacy interface: calls ctx%inslad with the legacy context.
      subroutine inslad ( elts, ne, loc, array, na )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: elts  ( * )  !! Elements to be inserted.
      integer(ip) :: ne  !! Number of elements to be inserted.
      integer(ip) :: loc  !! Location of the first inserted element.
      real(wp) :: array ( * )  !! Input/output array.
      integer(ip) :: na  !! Number of elements in the input/output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%inslad ( elts, ne, loc, array, na )
      end subroutine inslad

      !> Insert one or more elements into an integer array at
      !> the indicated location.
      !>
      !> Legacy interface: calls ctx%inslai with the legacy context.
      subroutine inslai ( elts, ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: elts  ( * )  !! Elements to be inserted.
      integer(ip) :: ne  !! Number of elements to be inserted.
      integer(ip) :: loc  !! Location of the first inserted element.
      integer(ip) :: array ( * )  !! Input/output array.
      integer(ip) :: na  !! Number of elements in the input/output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%inslai ( elts, ne, loc, array, na )
      end subroutine inslai

      !> Remove one or more elements from a character array at the
      !> indicated location.
      !>
      !> Legacy interface: calls ctx%remlac with the legacy context.
      subroutine remlac ( ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: ne  !! Number of elements to be removed.
      integer(ip) :: loc  !! Location of the first removed element.
      character(*) :: array ( * )  !! Input/output array.
      integer(ip) :: na  !! Number of elements in the input/output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%remlac ( ne, loc, array, na )
      end subroutine remlac

      !> Remove one or more elements from a double precision array at the
      !> indicated location.
      !>
      !> Legacy interface: calls ctx%remlad with the legacy context.
      subroutine remlad ( ne, loc, array, na )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ne  !! Number of elements to be removed.
      integer(ip) :: loc  !! Location of the first removed element.
      real(wp) :: array ( * )  !! Input/output array.
      integer(ip) :: na  !! Number of elements in the input/output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%remlad ( ne, loc, array, na )
      end subroutine remlad

      !> Remove one or more elements from an integer array at the
      !> indicated location.
      !>
      !> Legacy interface: calls ctx%remlai with the legacy context.
      subroutine remlai ( ne, loc, array, na )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: ne  !! Number of elements to be removed.
      integer(ip) :: loc  !! Location of the first removed element.
      integer(ip) :: array ( * )  !! Input/output array.
      integer(ip) :: na  !! Number of elements in the input/output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%remlai ( ne, loc, array, na )
      end subroutine remlai

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of a character array.
      !>
      !> Legacy interface: calls ctx%swapac with the legacy context.
      subroutine swapac ( n, locn, m, locm, array )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: n  !! Number of elements in the first group.
      integer(ip) :: locn  !! Location of the first group.
      integer(ip) :: m  !! Number of elements in the second group.
      integer(ip) :: locm  !! Location of the second group.
      character(*) :: array  ( * )  !! The array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%swapac ( n, locn, m, locm, array )
      end subroutine swapac

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of a double precision array.
      !>
      !> Legacy interface: calls ctx%swapad with the legacy context.
      subroutine swapad ( n, locn, m, locm, array )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of elements in the first group.
      integer(ip) :: locn  !! Location of the first group.
      integer(ip) :: m  !! Number of elements in the second group.
      integer(ip) :: locm  !! Location of the second group.
      real(wp) :: array  ( * )  !! The array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%swapad ( n, locn, m, locm, array )
      end subroutine swapad

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of an integer array.
      !>
      !> Legacy interface: calls ctx%swapai with the legacy context.
      subroutine swapai ( n, locn, m, locm, array )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: n  !! Number of elements in the first group.
      integer(ip) :: locn  !! Location of the first group.
      integer(ip) :: m  !! Number of elements in the second group.
      integer(ip) :: locm  !! Location of the second group.
      integer(ip) :: array  ( * )  !! The array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%swapai ( n, locn, m, locm, array )
      end subroutine swapai

      !> Pack the contents of a CHARACTER array. That is, take
      !> a set of arbitrarily spaced elements from an input array,
      !> and make them adjacent elements in an output array.
      !>
      !> Legacy interface: calls ctx%packac with the legacy context.
      subroutine packac ( in, &
                          pack,   npack, &
                          maxout, nout,   out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in      ( * )  !! Input array.
      integer(ip) :: pack    ( * )  !! Indices of elements to be packed.
      integer(ip) :: npack  !! Number of indices.
      integer(ip) :: maxout  !! Maximum number of elements in the output array.
      integer(ip) :: nout  !! Number of elements in the output array.
      character(*) :: out     ( * )  !! Output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%packac ( in, pack, npack, maxout, nout, out )
      end subroutine packac

      !> Pack the contents of a double precision array. That is,
      !> take a set of arbitrarily spaced elements from an input
      !> array, and make them adjacent elements in an output array.
      !>
      !> Legacy interface: calls ctx%packad with the legacy context.
      subroutine packad ( in, &
                          pack,   npack, &
                          maxout, nout,   out )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: in      ( * )  !! Input array.
      integer(ip) :: pack    ( * )  !! Indices of elements to be packed.
      integer(ip) :: npack  !! Number of indices.
      integer(ip) :: maxout  !! Maximum number of elements in the output array.
      integer(ip) :: nout  !! Number of elements in the output array.
      real(wp) :: out     ( * )  !! Output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%packad ( in, pack, npack, maxout, nout, out )
      end subroutine packad

      !> Pack the contents of an integer array. That is,
      !> take a set of arbitrarily spaced elements from an input
      !> array, and make them adjacent elements in an output array.
      !>
      !> Legacy interface: calls ctx%packai with the legacy context.
      subroutine packai ( in, &
                          pack,   npack, &
                          maxout, nout,   out )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: in      ( * )  !! Input array.
      integer(ip) :: pack    ( * )  !! Indices of elements to be packed.
      integer(ip) :: npack  !! Number of indices.
      integer(ip) :: maxout  !! Maximum number of elements in the output array.
      integer(ip) :: nout  !! Number of elements in the output array.
      integer(ip) :: out     ( * )  !! Output array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%packai ( in, pack, npack, maxout, nout, out )
      end subroutine packai

      !> Allocate a node in a doubly linked list pool.
      !>
      !> Legacy interface: calls ctx%lnkan with the legacy context.
      subroutine lnkan ( pool, new )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      integer(ip) :: new  !! Number of new node that was allocated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lnkan ( pool, new )
      end subroutine lnkan

      !> Free a specified sublist in a list.
      !>
      !> Legacy interface: calls ctx%lnkfsl with the legacy context.
      subroutine lnkfsl ( head, tail, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: head  !! Head and tail nodes of a sublist to be freed.
      integer(ip) :: tail  !! Head and tail nodes of a sublist to be freed.
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lnkfsl ( head, tail, pool )
      end subroutine lnkfsl

      !> Return the head node of the list containing a specified node.
      !>
      !> Legacy interface: calls ctx%lnkhl with the legacy context.
      integer(ip) function lnkhl ( node, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: node  !! Number of a node.
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lnkhl = ctx%lnkhl ( node, pool )
      end function lnkhl

      !> Insert the list containing a specified node into a another list,
      !> following a specified node.
      !>
      !> Legacy interface: calls ctx%lnkila with the legacy context.
      subroutine lnkila ( prev, list, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: prev  !! Node after which a new list is to be inserted.
      integer(ip) :: list  !! Node in the list to be inserted.
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lnkila ( prev, list, pool )
      end subroutine lnkila

      !> Insert the list containing a specified node into a another list,
      !> preceding a specified node.
      !>
      !> Legacy interface: calls ctx%lnkilb with the legacy context.
      subroutine lnkilb ( list, next, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: list  !! Node in the list to be inserted.
      integer(ip) :: next  !! Node before which a new list is to be inserted.
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lnkilb ( list, next, pool )
      end subroutine lnkilb

      !> Initialize a doubly linked list pool.
      !>
      !> Legacy interface: calls ctx%lnkini with the legacy context.
      subroutine lnkini ( size, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: size  !! Number of nodes in the pool.
      integer(ip) :: pool ( 2,  lbpool : * )  !! An array that is a linked pool on output.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lnkini ( size, pool )
      end subroutine lnkini

      !> Return the number of free nodes in a doubly linked list pool.
      !>
      !> Legacy interface: calls ctx%lnknfn with the legacy context.
      integer(ip) function lnknfn ( pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lnknfn = ctx%lnknfn ( pool )
      end function lnknfn

      !> Find the node following a specified node in a doubly linked list
      !> pool.
      !>
      !> Legacy interface: calls ctx%lnknxt with the legacy context.
      integer(ip) function lnknxt ( node, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: pool ( 2,  lbpool :  *  )  !! A doubly linked list pool.
      integer(ip) :: node  !! Number of an allocated node.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lnknxt = ctx%lnknxt ( node, pool )
      end function lnknxt

      !> Find the node preceding a specified node in a doubly linked list
      !> pool.
      !>
      !> Legacy interface: calls ctx%lnkprv with the legacy context.
      integer(ip) function lnkprv ( node, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      integer(ip) :: node  !! Number of an allocated node.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lnkprv = ctx%lnkprv ( node, pool )
      end function lnkprv

      !> Return the size of a doubly linked list pool.
      !>
      !> Legacy interface: calls ctx%lnksiz with the legacy context.
      integer(ip) function lnksiz ( pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lnksiz = ctx%lnksiz ( pool )
      end function lnksiz

      !> Return the tail node of the list containing a specified node.
      !>
      !> Legacy interface: calls ctx%lnktl with the legacy context.
      integer(ip) function lnktl ( node, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: node  !! Number of a node.
      integer(ip) :: pool (  2,  lbpool : *  )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lnktl = ctx%lnktl ( node, pool )
      end function lnktl

      !> Extract a specified sublist from a list.
      !>
      !> Legacy interface: calls ctx%lnkxsl with the legacy context.
      subroutine lnkxsl ( head, tail, pool )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: head  !! Head and tail nodes of a sublist to be extracted.
      integer(ip) :: tail  !! Head and tail nodes of a sublist to be extracted.
      integer(ip) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lnkxsl ( head, tail, pool )
      end subroutine lnkxsl

end module pepper_legacy_array_ops
