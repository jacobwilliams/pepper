!> The legacy interface's routines of the subsystem cells (70 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_cells

   use pepper_instance_module, only: pepper_context, pepper_zzwninsd => zzwninsd
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: chbase, enchar, dechar, excess, cardc, cardd, cardi, sizec, sized, sizei, scardc, &
      scardd, scardi, ssizec, ssized, ssizei, validc, validd, validi, appndc, appndd, appndi, &
      copyc, copyd, copyi, insrtc, insrtd, insrti, removc, removd, removi, elemc, elemd, elemi, &
      ordc, ordd, ordi, setc, setd, seti, unionc, uniond, unioni, interc, interd, interi, diffc, &
      diffd, diffi, sdiffc, sdiffd, sdiffi, zzwninsd, wncard, wncomd, wncond, wndifd, wnelmd, &
      wnexpd, wnextd, wnfetd, wnfild, wnfltd, wnincd, wninsd, wnintd, wnreld, wnsumd, wnunid, &
      wnvald

contains

      !> Return the base value used to encode unsigned integer values
      !> in character strings.
      !>
      !> Legacy interface: calls ctx%chbase with the legacy context.
      integer(ip) function chbase ()
      use pepper_kinds, only: ip
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      chbase = ctx%chbase (  )
      end function chbase

      !> Encode a nonnegative integer number into a character string
      !> as the expansion of the number in base CHBASE (a function of
      !> the size of the available character set).
      !>
      !> Legacy interface: calls ctx%enchar with the legacy context.
      subroutine enchar ( number, string )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Number to be encoded.
      character(*) :: string  !! Encoded string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%enchar ( number, string )
      end subroutine enchar

      !> Decode a character string encoded by ENCHAR.
      !>
      !> Legacy interface: calls ctx%dechar with the legacy context.
      subroutine dechar ( string, number )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Decoded number.
      character(*) :: string  !! Encoded character string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dechar ( string, number )
      end subroutine dechar

      !> Set the long error message so as to indicate the number of excess
      !> elements encountered by a routine operating on cells or on data
      !> structures based on cells.
      !>
      !> Legacy interface: calls ctx%excess with the legacy context.
      subroutine excess ( number, struct )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Number of excess elements.
      character(*) :: struct  !! Name of the data structure.
      integer(ip), parameter :: longln = 320
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%excess ( number, struct )
      end subroutine excess

      !> Return the cardinality (number of elements) of a character cell.
      !>
      !> Legacy interface: calls ctx%cardc with the legacy context.
      integer(ip) function cardc ( cell )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: cell  ( lbcell:* )  !! Input cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      cardc = ctx%cardc ( cell )
      end function cardc

      !> Return the cardinality (number of elements) of a double
      !> precision cell.
      !>
      !> Legacy interface: calls ctx%cardd with the legacy context.
      integer(ip) function cardd ( cell )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: cell  ( lbcell:* )  !! Input cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      cardd = ctx%cardd ( cell )
      end function cardd

      !> Return the cardinality (number of elements) of an integer cell.
      !>
      !> Legacy interface: calls ctx%cardi with the legacy context.
      integer(ip) function cardi ( cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: cell  ( lbcell:* )  !! Input cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      cardi = ctx%cardi ( cell )
      end function cardi

      !> Return the size (maximum cardinality) of a character cell.
      !>
      !> Legacy interface: calls ctx%sizec with the legacy context.
      integer(ip) function sizec ( cell )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: cell   ( lbcell:* )  !! Input cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sizec = ctx%sizec ( cell )
      end function sizec

      !> Return the size (maximum cardinality) of a double precision
      !> cell.
      !>
      !> Legacy interface: calls ctx%sized with the legacy context.
      integer(ip) function sized ( cell )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: cell   ( lbcell:* )  !! Input cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sized = ctx%sized ( cell )
      end function sized

      !> Return the size (maximum cardinality) of an integer cell.
      !>
      !> Legacy interface: calls ctx%sizei with the legacy context.
      integer(ip) function sizei ( cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: cell  ( lbcell: * )  !! Input cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sizei = ctx%sizei ( cell )
      end function sizei

      !> Set the cardinality of a character cell.
      !>
      !> Legacy interface: calls ctx%scardc with the legacy context.
      subroutine scardc ( card, cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: card  !! Cardinality of (number of elements in) the cell.
      character(*) :: cell   ( lbcell:* )  !! The cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scardc ( card, cell )
      end subroutine scardc

      !> Set the cardinality of a double precision cell.
      !>
      !> Legacy interface: calls ctx%scardd with the legacy context.
      subroutine scardd ( card, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: card  !! Cardinality of (number of elements in) the cell.
      real(wp) :: cell   ( lbcell:* )  !! The cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scardd ( card, cell )
      end subroutine scardd

      !> Set the cardinality of an integer cell.
      !>
      !> Legacy interface: calls ctx%scardi with the legacy context.
      subroutine scardi ( card, cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: card  !! Cardinality of (number of elements in) the cell.
      integer(ip) :: cell   ( lbcell:* )  !! The cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scardi ( card, cell )
      end subroutine scardi

      !> Set the size (maximum cardinality) of a character cell.
      !>
      !> Legacy interface: calls ctx%ssizec with the legacy context.
      subroutine ssizec ( size, cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: size  !! Size (maximum cardinality) of the cell.
      character(*) :: cell   ( lbcell:* )  !! The cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ssizec ( size, cell )
      end subroutine ssizec

      !> Set the size (maximum cardinality) of a double precision cell.
      !>
      !> Legacy interface: calls ctx%ssized with the legacy context.
      subroutine ssized ( size, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: size  !! Size (maximum cardinality) of the cell.
      real(wp) :: cell   ( lbcell:* )  !! The cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ssized ( size, cell )
      end subroutine ssized

      !> Set the size (maximum cardinality) of an integer cell.
      !>
      !> Legacy interface: calls ctx%ssizei with the legacy context.
      subroutine ssizei ( size, cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: size  !! Size (maximum cardinality) of the cell.
      integer(ip) :: cell   ( lbcell:* )  !! The cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ssizei ( size, cell )
      end subroutine ssizei

      !> Create a valid set from a character set array.
      !>
      !> Legacy interface: calls ctx%validc with the legacy context.
      subroutine validc ( size, n, a )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: size  !! Size (maximum cardinality) of the set.
      integer(ip) :: n  !! Initial no. of (possibly non-distinct) elements.
      character(*) :: a      ( lbcell:* )  !! Set to be validated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%validc ( size, n, a )
      end subroutine validc

      !> Create a valid set from a double precision set array.
      !>
      !> Legacy interface: calls ctx%validd with the legacy context.
      subroutine validd ( size, n, a )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: size  !! Size (maximum cardinality) of the set.
      integer(ip) :: n  !! Initial no. of (possibly non-distinct) elements.
      real(wp) :: a       ( lbcell:* )  !! Set to be validated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%validd ( size, n, a )
      end subroutine validd

      !> Create a valid set from an integer set array.
      !>
      !> Legacy interface: calls ctx%validi with the legacy context.
      subroutine validi ( size, n, a )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: size  !! Size (maximum cardinality) of the set.
      integer(ip) :: n  !! Initial no. of (possibly non-distinct) elements.
      integer(ip) :: a       ( lbcell:* )  !! Set to be validated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%validi ( size, n, a )
      end subroutine validi

      !> Append an item to a character cell.
      !>
      !> Legacy interface: calls ctx%appndc with the legacy context.
      subroutine appndc ( item, cell )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: item  !! The item to append.
      character(*) :: cell ( lbcell:* )  !! The cell to which ITEM will be appended.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: item is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%appndc ( (item), cell )
      end subroutine appndc

      !> Append an item to a double precision cell.
      !>
      !> Legacy interface: calls ctx%appndd with the legacy context.
      subroutine appndd ( item, cell )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: item  !! The item to append.
      real(wp) :: cell ( lbcell:* )  !! The cell to which ITEM will be appended.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%appndd ( item, cell )
      end subroutine appndd

      !> Append an item to an integer cell.
      !>
      !> Legacy interface: calls ctx%appndi with the legacy context.
      subroutine appndi ( item, cell )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: item  !! The item to append.
      integer(ip) :: cell ( lbcell:* )  !! The cell to which ITEM will be appended.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%appndi ( item, cell )
      end subroutine appndi

      !> Copy the contents of a character cell to another cell.
      !>
      !> Legacy interface: calls ctx%copyc with the legacy context.
      subroutine copyc ( cell, copy )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: cell     ( lbcell:* )  !! Cell to be copied.
      character(*) :: copy     ( lbcell:* )  !! New cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%copyc ( cell, copy )
      end subroutine copyc

      !> Copy the contents of a double precision cell to another cell.
      !>
      !> Legacy interface: calls ctx%copyd with the legacy context.
      subroutine copyd ( cell, copy )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: cell     ( lbcell:* )  !! Cell to be copied.
      real(wp) :: copy     ( lbcell:* )  !! New cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%copyd ( cell, copy )
      end subroutine copyd

      !> Copy the contents of an integer cell to another cell.
      !>
      !> Legacy interface: calls ctx%copyi with the legacy context.
      subroutine copyi ( cell, copy )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: cell     ( lbcell:* )  !! Cell to be copied.
      integer(ip) :: copy     ( lbcell:* )  !! New cell.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%copyi ( cell, copy )
      end subroutine copyi

      !> Insert an item into a character set.
      !>
      !> Legacy interface: calls ctx%insrtc with the legacy context.
      subroutine insrtc ( item, a )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: item  !! Item to be inserted.
      character(*) :: a        ( lbcell:* )  !! Insertion set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: item is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%insrtc ( (item), a )
      end subroutine insrtc

      !> Insert an item into a double precision set.
      !>
      !> Legacy interface: calls ctx%insrtd with the legacy context.
      subroutine insrtd ( item, a )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: item  !! Item to be inserted.
      real(wp) :: a     ( lbcell:* )  !! Insertion set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%insrtd ( item, a )
      end subroutine insrtd

      !> Insert an item into an integer set.
      !>
      !> Legacy interface: calls ctx%insrti with the legacy context.
      subroutine insrti ( item, a )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: item  !! Item to be inserted.
      integer(ip) :: a        ( lbcell:* )  !! Insertion set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%insrti ( item, a )
      end subroutine insrti

      !> Remove an item from a character set.
      !>
      !> Legacy interface: calls ctx%removc with the legacy context.
      subroutine removc ( item, a )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: item  !! Item to be removed.
      character(*) :: a       ( lbcell:* )  !! Removal set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: item is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%removc ( (item), a )
      end subroutine removc

      !> Remove an item from a double precision set.
      !>
      !> Legacy interface: calls ctx%removd with the legacy context.
      subroutine removd ( item, a )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: item  !! Item to be removed.
      real(wp) :: a        ( lbcell:* )  !! Removal set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%removd ( item, a )
      end subroutine removd

      !> Remove an item from an integer set.
      !>
      !> Legacy interface: calls ctx%removi with the legacy context.
      subroutine removi ( item, a )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: item  !! Item to be removed.
      integer(ip) :: a        ( lbcell:* )  !! Removal set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%removi ( item, a )
      end subroutine removi

      !> Determine whether an item is an element of a character set.
      !>
      !> Legacy interface: calls ctx%elemc with the legacy context.
      logical function elemc ( item, a )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: item  !! Item to be tested.
      character(*) :: a      ( lbcell:* )  !! Set to be tested.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      elemc = ctx%elemc ( item, a )
      end function elemc

      !> Determine whether an item is an element of a double
      !> precision set.
      !>
      !> Legacy interface: calls ctx%elemd with the legacy context.
      logical function elemd ( item, a )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: item  !! Item to be tested.
      real(wp) :: a      ( lbcell:* )  !! Set to be tested.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      elemd = ctx%elemd ( item, a )
      end function elemd

      !> Determine whether an item is an element of an integer set.
      !>
      !> Legacy interface: calls ctx%elemi with the legacy context.
      logical function elemi ( item, a )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: item  !! Item to be tested.
      integer(ip) :: a      ( lbcell:* )  !! Set to be tested.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      elemi = ctx%elemi ( item, a )
      end function elemi

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> Legacy interface: calls ctx%ordc with the legacy context.
      integer(ip) function ordc ( item, set )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: item  !! An item to locate within a set.
      character(*) :: set ( lbcell:* )  !! A set to search for a given item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ordc = ctx%ordc ( item, set )
      end function ordc

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> Legacy interface: calls ctx%ordd with the legacy context.
      integer(ip) function ordd ( item, set )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: item  !! An item to locate within a set.
      real(wp) :: set ( lbcell:* )  !! A set to search for a given item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ordd = ctx%ordd ( item, set )
      end function ordd

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> Legacy interface: calls ctx%ordi with the legacy context.
      integer(ip) function ordi ( item, set )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: item  !! An item to locate within a set.
      integer(ip) :: set ( lbcell:* )  !! A set to search for a given item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ordi = ctx%ordi ( item, set )
      end function ordi

      !> Compare two character sets, as indicated by a relational operator.
      !>
      !> Legacy interface: calls ctx%setc with the legacy context.
      logical function setc ( a, op, b )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: a      ( lbcell:* )  !! First set.
      character(*) :: op  !! Comparison operator.
      character(*) :: b      ( lbcell:* )  !! Second set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      setc = ctx%setc ( a, op, b )
      end function setc

      !> Compare two double precision sets, as indicated by a relational
      !> operator.
      !>
      !> Legacy interface: calls ctx%setd with the legacy context.
      logical function setd ( a, op, b )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a      ( lbcell:* )  !! First set.
      character(*) :: op  !! Comparison operator.
      real(wp) :: b      ( lbcell:* )  !! Second set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      setd = ctx%setd ( a, op, b )
      end function setd

      !> Compare two integer sets, as indicated by a relational operator.
      !>
      !> Legacy interface: calls ctx%seti with the legacy context.
      logical function seti ( a, op, b )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: a      ( lbcell:* )  !! First set.
      character(*) :: op  !! Comparison operator.
      integer(ip) :: b      ( lbcell:* )  !! Second set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      seti = ctx%seti ( a, op, b )
      end function seti

      !> Compute the union of two character sets to form a third set.
      !>
      !> Legacy interface: calls ctx%unionc with the legacy context.
      subroutine unionc ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: a      ( lbcell:* )  !! First input set.
      character(*) :: b      ( lbcell:* )  !! Second input set.
      character(*) :: c      ( lbcell:* )  !! Union of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%unionc ( a, b, c )
      end subroutine unionc

      !> Compute the union of two double precision sets to form a third
      !> set.
      !>
      !> Legacy interface: calls ctx%uniond with the legacy context.
      subroutine uniond ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a      ( lbcell:* )  !! First input set.
      real(wp) :: b      ( lbcell:* )  !! Second input set.
      real(wp) :: c      ( lbcell:* )  !! Union of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%uniond ( a, b, c )
      end subroutine uniond

      !> Compute the union of two integer sets to form a third set.
      !>
      !> Legacy interface: calls ctx%unioni with the legacy context.
      subroutine unioni ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: a      ( lbcell:* )  !! First input set.
      integer(ip) :: b      ( lbcell:* )  !! Second input set.
      integer(ip) :: c      ( lbcell:* )  !! Union of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%unioni ( a, b, c )
      end subroutine unioni

      !> Intersect two character sets to form a third set.
      !>
      !> Legacy interface: calls ctx%interc with the legacy context.
      subroutine interc ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: a      ( lbcell:* )  !! First input set.
      character(*) :: b      ( lbcell:* )  !! Second input set.
      character(*) :: c      ( lbcell:* )  !! Intersection of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%interc ( a, b, c )
      end subroutine interc

      !> Intersect two double precision sets to form a third set.
      !>
      !> Legacy interface: calls ctx%interd with the legacy context.
      subroutine interd ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a      ( lbcell:* )  !! First input set.
      real(wp) :: b      ( lbcell:* )  !! Second input set.
      real(wp) :: c      ( lbcell:* )  !! Intersection of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%interd ( a, b, c )
      end subroutine interd

      !> Intersect two integer sets to form a third set.
      !>
      !> Legacy interface: calls ctx%interi with the legacy context.
      subroutine interi ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: a      ( lbcell:* )  !! First input set.
      integer(ip) :: b      ( lbcell:* )  !! Second input set.
      integer(ip) :: c      ( lbcell:* )  !! Intersection of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%interi ( a, b, c )
      end subroutine interi

      !> Take the difference of two character sets to form a third set.
      !>
      !> Legacy interface: calls ctx%diffc with the legacy context.
      subroutine diffc ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: a      ( lbcell:* )  !! First input set.
      character(*) :: b      ( lbcell:* )  !! Second input set.
      character(*) :: c      ( lbcell:* )  !! Difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%diffc ( a, b, c )
      end subroutine diffc

      !> Take the difference of two double precision sets to form
      !> a third set.
      !>
      !> Legacy interface: calls ctx%diffd with the legacy context.
      subroutine diffd ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a      ( lbcell:* )  !! First input set.
      real(wp) :: b      ( lbcell:* )  !! Second input set.
      real(wp) :: c      ( lbcell:* )  !! Difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%diffd ( a, b, c )
      end subroutine diffd

      !> Take the difference of two integer sets to form a third set.
      !>
      !> Legacy interface: calls ctx%diffi with the legacy context.
      subroutine diffi ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: a      ( lbcell:* )  !! First input set.
      integer(ip) :: b      ( lbcell:* )  !! Second input set.
      integer(ip) :: c      ( lbcell:* )  !! Difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%diffi ( a, b, c )
      end subroutine diffi

      !> Take the symmetric difference of two character sets to form
      !> a third set.
      !>
      !> Legacy interface: calls ctx%sdiffc with the legacy context.
      subroutine sdiffc ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: a      ( lbcell:* )  !! First input set.
      character(*) :: b      ( lbcell:* )  !! Second input set.
      character(*) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sdiffc ( a, b, c )
      end subroutine sdiffc

      !> Take the symmetric difference of two double precision sets
      !> to form a third set.
      !>
      !> Legacy interface: calls ctx%sdiffd with the legacy context.
      subroutine sdiffd ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a      ( lbcell:* )  !! First input set.
      real(wp) :: b      ( lbcell:* )  !! Second input set.
      real(wp) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sdiffd ( a, b, c )
      end subroutine sdiffd

      !> Take the symmetric difference of two integer sets to form
      !> a third set.
      !>
      !> Legacy interface: calls ctx%sdiffi with the legacy context.
      subroutine sdiffi ( a, b, c )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: a      ( lbcell:* )  !! First input set.
      integer(ip) :: b      ( lbcell:* )  !! Second input set.
      integer(ip) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sdiffi ( a, b, c )
      end subroutine sdiffi

      !> Insert an interval into a double precision window.
      !>
      !> Legacy interface: calls zz routine zzwninsd with the legacy context.
      subroutine zzwninsd ( left, right, context, window )
      use pepper_kinds, only: ip, wp
      use pepper_inc_errhnd
      implicit none
      real(wp) :: left  !! Left, right endpoints of new interval.
      real(wp) :: right  !! Left, right endpoints of new interval.
      character(*) :: context  !! A call explanation string.
      real(wp) :: window    ( lbcell:* )  !! Input, output window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzwninsd ( ctx, left, right, context, window )
      end subroutine zzwninsd

      !> Return the cardinality (number of intervals) of a double
      !> precision window.
      !>
      !> Legacy interface: calls ctx%wncard with the legacy context.
      integer(ip) function wncard ( window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: window  ( lbcell:* )  !! Input window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      wncard = ctx%wncard ( window )
      end function wncard

      !> Determine the complement of a double precision window with
      !> respect to the interval [LEFT,RIGHT].
      !>
      !> Legacy interface: calls ctx%wncomd with the legacy context.
      subroutine wncomd ( left, right, window, result )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: left  !! Left, right endpoints of complement interval.
      real(wp) :: right  !! Left, right endpoints of complement interval.
      real(wp) :: window   ( lbcell:* )  !! Input window.
      real(wp) :: result   ( lbcell:* )  !! Complement of WINDOW with respect to [LEFT,RIGHT].
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wncomd ( left, right, window, result )
      end subroutine wncomd

      !> Contract each of the intervals of a double precision window.
      !>
      !> Legacy interface: calls ctx%wncond with the legacy context.
      subroutine wncond ( left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: left  !! Amount added to each left endpoint.
      real(wp) :: right  !! Amount subtracted from each right endpoint.
      real(wp) :: window  ( lbcell:* )  !! Window to be contracted.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wncond ( left, right, window )
      end subroutine wncond

      !> Place the difference of two double precision windows into
      !> a third window.
      !>
      !> Legacy interface: calls ctx%wndifd with the legacy context.
      subroutine wndifd ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a        ( lbcell:* )  !! Input windows.
      real(wp) :: b        ( lbcell:* )  !! Input windows.
      real(wp) :: c        ( lbcell:* )  !! Difference of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wndifd ( a, b, c )
      end subroutine wndifd

      !> Determine whether a point is an element of a double precision
      !> window.
      !>
      !> Legacy interface: calls ctx%wnelmd with the legacy context.
      logical function wnelmd ( point, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: point  !! Input point.
      real(wp) :: window      ( lbcell:* )  !! Input window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      wnelmd = ctx%wnelmd ( point, window )
      end function wnelmd

      !> Expand each of the intervals of a double precision window.
      !>
      !> Legacy interface: calls ctx%wnexpd with the legacy context.
      subroutine wnexpd ( left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: left  !! Amount subtracted from each left endpoint.
      real(wp) :: right  !! Amount added to each right endpoint.
      real(wp) :: window  ( lbcell:* )  !! Window to be expanded.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnexpd ( left, right, window )
      end subroutine wnexpd

      !> Extract the left or right endpoints from a double precision
      !> window.
      !>
      !> Legacy interface: calls ctx%wnextd with the legacy context.
      subroutine wnextd ( side, window )
      use pepper_kinds, only: ip, wp
      implicit none
      character(1) :: side  !! Extract left ('L') or right ('R') endpoints.
      real(wp) :: window  ( lbcell:* )  !! Window to be extracted.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnextd ( side, window )
      end subroutine wnextd

      !> Fetch a particular interval from a double precision window.
      !>
      !> Legacy interface: calls ctx%wnfetd with the legacy context.
      subroutine wnfetd ( window, n, left, right )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: window      ( lbcell:* )  !! Input window.
      integer(ip) :: n  !! Index of interval to be fetched.
      real(wp) :: left  !! Left, right endpoints of the Nth interval.
      real(wp) :: right  !! Left, right endpoints of the Nth interval.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnfetd ( window, n, left, right )
      end subroutine wnfetd

      !> Fill small gaps between adjacent intervals of a double precision
      !> window.
      !>
      !> Legacy interface: calls ctx%wnfild with the legacy context.
      subroutine wnfild ( smlgap, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: smlgap  !! Limiting measure of small gaps.
      real(wp) :: window  ( lbcell:* )  !! Window to be filled.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnfild ( smlgap, window )
      end subroutine wnfild

      !> Filter (remove) small intervals from a double precision window.
      !>
      !> Legacy interface: calls ctx%wnfltd with the legacy context.
      subroutine wnfltd ( smlint, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: smlint  !! Limiting measure of small intervals.
      real(wp) :: window  ( lbcell:* )  !! Window to be filtered.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnfltd ( smlint, window )
      end subroutine wnfltd

      !> Determine whether an interval is included in a double precision
      !> window.
      !>
      !> Legacy interface: calls ctx%wnincd with the legacy context.
      logical function wnincd ( left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: left  !! Input interval.
      real(wp) :: right  !! Input interval.
      real(wp) :: window      ( lbcell:* )  !! Input window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      wnincd = ctx%wnincd ( left, right, window )
      end function wnincd

      !> Insert an interval into a double precision window.
      !>
      !> Legacy interface: calls ctx%wninsd with the legacy context.
      subroutine wninsd ( left, right, window )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: left  !! Left, right endpoints of new interval.
      real(wp) :: right  !! Left, right endpoints of new interval.
      real(wp) :: window    ( lbcell:* )  !! Input, output window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wninsd ( left, right, window )
      end subroutine wninsd

      !> Place the intersection of two double precision windows into
      !> a third window.
      !>
      !> Legacy interface: calls ctx%wnintd with the legacy context.
      subroutine wnintd ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a        ( lbcell:* )  !! Input windows.
      real(wp) :: b        ( lbcell:* )  !! Input windows.
      real(wp) :: c        ( lbcell:* )  !! Intersection of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnintd ( a, b, c )
      end subroutine wnintd

      !> Compare two double precision windows.
      !>
      !> Legacy interface: calls ctx%wnreld with the legacy context.
      logical function  wnreld ( a, op, b )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a        ( lbcell:* )  !! First window.
      character(*) :: op  !! Comparison operator.
      real(wp) :: b        ( lbcell:* )  !! Second window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      wnreld = ctx%wnreld ( a, op, b )
      end function wnreld

      !> Summarize the contents of a double precision window.
      !>
      !> Legacy interface: calls ctx%wnsumd with the legacy context.
      subroutine wnsumd ( window, &
                          meas,   avg,   stddev, &
                          idxsml, idxlon         )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: window    ( lbcell:* )  !! Window to be summarized.
      real(wp) :: meas  !! Total measure of intervals in WINDOW.
      real(wp) :: avg  !! Average measure.
      real(wp) :: stddev  !! Standard deviation.
      integer(ip) :: idxsml  !! Locations of shortest, longest intervals.
      integer(ip) :: idxlon  !! Locations of shortest, longest intervals.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnsumd ( window, meas, avg, stddev, idxsml, idxlon )
      end subroutine wnsumd

      !> Place the union of two double precision windows into a third
      !> window.
      !>
      !> Legacy interface: calls ctx%wnunid with the legacy context.
      subroutine wnunid ( a, b, c )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a        ( lbcell:* )  !! Input windows.
      real(wp) :: b        ( lbcell:* )  !! Input windows.
      real(wp) :: c        ( lbcell:* )  !! Union of A and B.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnunid ( a, b, c )
      end subroutine wnunid

      !> Form a valid double precision window from the contents
      !> of a window array.
      !>
      !> Legacy interface: calls ctx%wnvald with the legacy context.
      subroutine wnvald ( size, n, window )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: size  !! Size of window.
      integer(ip) :: n  !! Original number of endpoints.
      real(wp) :: window ( lbcell:* )  !! Input, output window.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wnvald ( size, n, window )
      end subroutine wnvald

end module pepper_legacy_cells
