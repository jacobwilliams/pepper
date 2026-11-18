!> The legacy interface's routines of the subsystem ek_storage (108 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_ek_storage

   use pepper_instance_module, only: pepper_context, pepper_zzekcdsc => zzekcdsc, &
      pepper_zzekcnam => zzekcnam, pepper_zzekgei => zzekgei, pepper_zzekgrs => zzekgrs, &
      pepper_zzekordc => zzekordc, pepper_zzekordd => zzekordd, pepper_zzekordi => zzekordi, &
      pepper_zzekpdec => zzekpdec, pepper_zzekpgch => zzekpgch, pepper_zzekqini => zzekqini, &
      pepper_zzekrbck => zzekrbck, pepper_zzekreqi => zzekreqi, pepper_zzekstop => zzekstop, &
      pepper_zzekspsh => zzekspsh, pepper_zzekspop => zzekspop, pepper_zzeksdec => zzeksdec, &
      pepper_zzeksupd => zzeksupd, pepper_zzeksrd => zzeksrd, pepper_zzekscln => zzekscln, &
      pepper_zzekscan => zzekscan, pepper_zzeksei => zzeksei, pepper_zzekstyp => zzekstyp, &
      pepper_zzeksz04 => zzeksz04, pepper_zzeksz05 => zzeksz05, pepper_zzektcnv => zzektcnv, &
      pepper_zzektloc => zzektloc, pepper_zzekweqi => zzekweqi, pepper_zzekgrcp => zzekgrcp, &
      pepper_zzekinqc => zzekinqc, pepper_zzekinqn => zzekinqn, pepper_zzekjsqz => zzekjsqz, &
      pepper_zzekpgin => zzekpgin, pepper_zzekpgan => zzekpgan, pepper_zzekpgal => zzekpgal, &
      pepper_zzekpgfr => zzekpgfr, pepper_zzekpgrc => zzekpgrc, pepper_zzekpgrd => zzekpgrd, &
      pepper_zzekpgri => zzekpgri, pepper_zzekpgwc => zzekpgwc, pepper_zzekpgwd => zzekpgwd, &
      pepper_zzekpgwi => zzekpgwi, pepper_zzekpgbs => zzekpgbs, pepper_zzekpgpg => zzekpgpg, &
      pepper_zzekpgst => zzekpgst, pepper_zzekqcnj => zzekqcnj, pepper_zzekqcon => zzekqcon, &
      pepper_zzekqord => zzekqord, pepper_zzekqsel => zzekqsel, pepper_zzekqtab => zzekqtab, &
      pepper_zzekrd09 => zzekrd09, pepper_zzeksrcp => zzeksrcp, pepper_zzeksrs => zzeksrs, &
      pepper_zzeksz06 => zzeksz06, pepper_zzekvset => zzekvset, pepper_zzekvcal => zzekvcal, &
      pepper_zzekgfwd => zzekgfwd, pepper_zzekglnk => zzekglnk, pepper_zzeknrml => zzeknrml, &
      pepper_zzeksfwd => zzeksfwd, pepper_zzekslnk => zzekslnk, pepper_zzektr13 => zzektr13, &
      pepper_zzektr31 => zzektr31, pepper_zzektrbs => zzektrbs, pepper_zzektrfr => zzektrfr, &
      pepper_zzektrit => zzektrit, pepper_zzektrlk => zzektrlk, pepper_zzektrpi => zzektrpi, &
      pepper_zzektrrk => zzektrrk, pepper_zzekweed => zzekweed, pepper_zzekcix1 => zzekcix1, &
      pepper_zzekpars => zzekpars, pepper_zzektr23 => zzektr23, pepper_zzektr32 => zzektr32, &
      pepper_zzektrdp => zzektrdp, pepper_zzektrnk => zzektrnk, pepper_zzektrsb => zzektrsb, &
      pepper_zzektrsz => zzektrsz, pepper_zzektrud => zzektrud, pepper_zzektrui => zzektrui, &
      pepper_zzekixlk => zzekixlk, pepper_zzekrplk => zzekrplk, pepper_zzektr1s => zzektr1s, &
      pepper_zzektrbn => zzektrbn, pepper_zzektrki => zzektrki, pepper_zzektrls => zzektrls, &
      pepper_zzekmloc => zzekmloc, pepper_zzektrdl => zzektrdl, pepper_zzektrin => zzektrin, &
      pepper_zzekdps => zzekdps, pepper_zzekff01 => zzekff01, pepper_zzeksdsc => zzeksdsc, &
      pepper_zzeksinf => zzeksinf, pepper_zzektrap => zzektrap, pepper_zzekacps => zzekacps, &
      pepper_zzekaps => zzekaps, pepper_zzekbs01 => zzekbs01, pepper_zzekbs02 => zzekbs02, &
      pepper_zzekde04 => zzekde04, pepper_zzekrp2n => zzekrp2n
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: ekcls, ekshdw, zzekcdsc, zzekcnam, zzekgei, zzekgrs, zzekordc, zzekordd, zzekordi, &
      zzekpdec, zzekpgch, zzekqini, zzekrbck, zzekreqi, zzekstop, zzekspsh, zzekspop, zzeksdec, &
      zzeksupd, zzeksrd, zzekscln, zzekscan, zzeksei, zzekstyp, zzeksz04, zzeksz05, zzektcnv, &
      zzektloc, zzekweqi, ekopr, ekopw, zzekgrcp, zzekinqc, zzekinqn, zzekjsqz, zzekpgin, &
      zzekpgan, zzekpgal, zzekpgfr, zzekpgrc, zzekpgrd, zzekpgri, zzekpgwc, zzekpgwd, zzekpgwi, &
      zzekpgbs, zzekpgpg, zzekpgst, zzekqcnj, zzekqcon, zzekqord, zzekqsel, zzekqtab, zzekrd09, &
      zzeksrcp, zzeksrs, zzeksz06, zzekvset, zzekvcal, zzekgfwd, zzekglnk, zzeknrml, zzeksfwd, &
      zzekslnk, zzektr13, zzektr31, zzektrbs, zzektrfr, zzektrit, zzektrlk, zzektrpi, zzektrrk, &
      zzekweed, ekopn, ekops, zzekcix1, zzekpars, zzektr23, zzektr32, zzektrdp, zzektrnk, &
      zzektrsb, zzektrsz, zzektrud, zzektrui, eknseg, zzekixlk, zzekrplk, zzektr1s, zzektrbn, &
      zzektrki, zzektrls, zzekmloc, zzektrdl, zzektrin, zzekdps, zzekff01, zzeksdsc, zzeksinf, &
      zzektrap, ekffld, ekssum, zzekacps, zzekaps, zzekbs01, zzekbs02, zzekde04, zzekrp2n

contains

      !> Close an E-kernel.
      !>
      !> Legacy interface: calls ctx%ekcls with the legacy context.
      subroutine ekcls ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! EK file handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekcls ( handle )
      end subroutine ekcls

      !> Return shadowing status of a specified EK file. THIS IS A
      !> STUB ROUTINE.
      !>
      !> Legacy interface: calls ctx%ekshdw with the legacy context.
      subroutine ekshdw ( handle, isshad )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      logical :: isshad  !! Logical flag indicating whether EK is shadowed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekshdw ( handle, isshad )
      end subroutine ekshdw

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekcdsc with the legacy context.
      subroutine zzekcdsc ( handle, segdsc, column, coldsc )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      integer(ip) :: handle  !! Handle attached to an EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      character(*) :: column  !! Name of column.
      integer(ip) :: coldsc ( cdscsz )  !! Descriptor for specified column.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekcdsc ( ctx, handle, (segdsc), column, coldsc )
      end subroutine zzekcdsc

      !> Look up the name of a column, given the column's descriptor.
      !>
      !> Legacy interface: calls zz routine zzekcnam with the legacy context.
      subroutine zzekcnam ( handle, coldsc, column )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*) :: column  !! Column name.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekcnam ( ctx, handle, coldsc, column )
      end subroutine zzekcnam

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekgei with the legacy context.
      subroutine zzekgei ( handle, addrss, ival )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: addrss  !! DAS character address to read encoded value from.
      integer(ip) :: ival  !! Decoded integer value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekgei ( ctx, handle, addrss, ival )
      end subroutine zzekgei

      !> Return the status of a specified EK record.
      !>
      !> Legacy interface: calls zz routine zzekgrs with the legacy context.
      subroutine zzekgrs ( handle, recptr, status )
      use pepper_kinds, only: ip
      use pepper_inc_ekrecptr
      integer(ip) :: handle  !! File handle.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: status  !! Status of specified EK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekgrs ( ctx, handle, recptr, status )
      end subroutine zzekgrs

      !> Determine the order of elements in a character EK column,
      !> using dictionary ordering on character data values and array
      !> indices.
      !>
      !> Legacy interface: calls zz routine zzekordc with the legacy context.
      subroutine zzekordc ( cvals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip
      character(*) :: cvals  ( * )  !! Array of character string column values.
      logical :: nullok  !! Logical flag indicating whether nulls are allowed.
      logical :: nlflgs ( * )  !! Flags indicating whether column entries are null.
      integer(ip) :: nvals  !! Dimension of CVALS.
      integer(ip) :: iorder ( * )  !! Order vector for CVALS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekordc ( ctx, cvals, nullok, nlflgs, nvals, iorder )
      end subroutine zzekordc

      !> Determine the order of elements in a double precision EK column,
      !> using dictionary ordering on d.p. data values and array
      !> indices.
      !>
      !> Legacy interface: calls zz routine zzekordd with the legacy context.
      subroutine zzekordd ( dvals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip, wp
      real(wp) :: dvals  ( * )  !! Array of d.p. column values.
      logical :: nullok  !! Logical flag indicating whether nulls are allowed.
      logical :: nlflgs ( * )  !! Flags indicating whether column entries are null.
      integer(ip) :: nvals  !! Dimension of DVALS.
      integer(ip) :: iorder ( * )  !! Order vector for DVALS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekordd ( ctx, dvals, nullok, nlflgs, nvals, iorder )
      end subroutine zzekordd

      !> Determine the order of elements in an integer EK column, using
      !> dictionary ordering on integer data values and array indices.
      !>
      !> Legacy interface: calls zz routine zzekordi with the legacy context.
      subroutine zzekordi ( ivals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip
      integer(ip) :: ivals  ( * )  !! Array of integer column values.
      logical :: nullok  !! Logical flag indicating whether nulls are allowed.
      logical :: nlflgs ( * )  !! Flags indicating whether column entries are null.
      integer(ip) :: nvals  !! Dimension of IVALS.
      integer(ip) :: iorder ( * )  !! Order vector for IVALS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekordi ( ctx, ivals, nullok, nlflgs, nvals, iorder )
      end subroutine zzekordi

      !> Parse a declaration of a new EK column.
      !>
      !> Legacy interface: calls zz routine zzekpdec with the legacy context.
      subroutine zzekpdec ( decl, pardsc )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ektype
      implicit none
      character(*) :: decl  !! String containing column declaration.
      integer(ip) :: pardsc ( * )  !! Partial column descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpdec ( ctx, decl, pardsc )
      end subroutine zzekpdec

      !> Check that an EK is valid for a specified type of access by the
      !> paging system.
      !>
      !> Legacy interface: calls zz routine zzekpgch with the legacy context.
      subroutine zzekpgch ( handle, access )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ekarch
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      character(*) :: access  !! Access type.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgch ( ctx, handle, access )
      end subroutine zzekpgch

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekqini with the legacy context.
      subroutine zzekqini ( isize, dsize, eqryi, eqryc, eqryd )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ekqlimit
      use pepper_inc_ekbool
      integer(ip) :: isize  !! Size of integer component of encoded query.
      integer(ip) :: dsize  !! Size of d.p. component of encoded query.
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      real(wp) :: eqryd ( * )  !! D.p. component of query.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekqini ( ctx, isize, dsize, eqryi, eqryc, eqryd )
      end subroutine zzekqini

      !> Back up a modified EK record belonging to a shadowed EK.
      !> This is a stub routine.
      !>
      !> Legacy interface: calls zz routine zzekrbck with the legacy context.
      subroutine zzekrbck ( action, handle, segdsc, coldsc, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekglimit
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      character(*) :: action  !! Action necessitating backup.
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recno  !! Record number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekrbck ( ctx, action, handle, segdsc, coldsc, recno )
      end subroutine zzekrbck

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekreqi with the legacy context.
      subroutine zzekreqi ( eqryi, name, value )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: name  !! Name of scalar item to read.
      integer(ip) :: value  !! Value of item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekreqi ( ctx, eqryi, name, value )
      end subroutine zzekreqi

      !> Obtain last address in use in EK scratch area.
      !>
      !> Legacy interface: calls zz routine zzekstop with the legacy context.
      subroutine zzekstop ( top )
      use pepper_kinds, only: ip
      integer(ip) :: top  !! EK scratch area stack top.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekstop ( ctx, top )
      end subroutine zzekstop

      !> Push the contents of an integer array onto the EK scratch area
      !> stack.
      !>
      !> Legacy interface: calls zz routine zzekspsh with the legacy context.
      subroutine zzekspsh ( n, idata )
      use pepper_kinds, only: ip
      integer(ip) :: n  !! Number of integers to push.
      integer(ip) :: idata ( * )  !! Integer data.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekspsh ( ctx, n, idata )
      end subroutine zzekspsh

      !> Pop a specified number of elements from the top of the EK scratch
      !> area stack, transferring this data to an integer array.
      !>
      !> Legacy interface: calls zz routine zzekspop with the legacy context.
      subroutine zzekspop ( n, idata )
      use pepper_kinds, only: ip
      integer(ip) :: n  !! Number of integers to pop.
      integer(ip) :: idata ( * )  !! Integer data.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekspop ( ctx, n, idata )
      end subroutine zzekspop

      !> Decrement the EK scratch area stack pointer by a specified count.
      !>
      !> Legacy interface: calls zz routine zzeksdec with the legacy context.
      subroutine zzeksdec ( n )
      use pepper_kinds, only: ip
      integer(ip) :: n  !! Decrement count.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksdec ( ctx, n )
      end subroutine zzeksdec

      !> Update the contents of a range of addresses already in use in the
      !> EK scratch area.
      !>
      !> Legacy interface: calls zz routine zzeksupd with the legacy context.
      subroutine zzeksupd ( beg, end, idata )
      use pepper_kinds, only: ip
      integer(ip) :: beg  !! Begin and end addresses of range to update.
      integer(ip) :: end  !! Begin and end addresses of range to update.
      integer(ip) :: idata ( * )  !! Integer data.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksupd ( ctx, beg, end, idata )
      end subroutine zzeksupd

      !> Read from a range of addresses in the EK scratch area.
      !>
      !> Legacy interface: calls zz routine zzeksrd with the legacy context.
      subroutine zzeksrd ( beg, end, idata )
      use pepper_kinds, only: ip
      integer(ip) :: beg  !! Begin and end addresses of range to read from.
      integer(ip) :: end  !! Begin and end addresses of range to read from.
      integer(ip) :: idata ( * )  !! Integer data.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksrd ( ctx, beg, end, idata )
      end subroutine zzeksrd

      !> Clean up:  re-initialize the EK scratch area; unload the
      !> scratch file.
      !>
      !> Legacy interface: calls zz routine zzekscln with the legacy context.
      subroutine zzekscln (  )
      use pepper_kinds, only: ip
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekscln ( ctx )
      end subroutine zzekscln

      !> Scan tokens in an EK query.
      !>
      !> Legacy interface: calls zz routine zzekscan with the legacy context.
      subroutine zzekscan ( query, maxntk, maxnum, ntoken, tokens, lxbegs, lxends, values, numvls, chrbuf, chbegs, chends, scnerr, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekkeyw
      use pepper_inc_ektokn
      use pepper_inc_ekqlimit
      character(*) :: query  !! Query specifying events to be found.
      integer(ip) :: maxntk  !! Maximum number of tokens to return.
      integer(ip) :: maxnum  !! Maximum number of numeric tokens allowed.
      integer(ip) :: ntoken  !! Number of tokens returned.
      integer(ip) :: tokens ( * )  !! Code numbers of identified tokens.
      integer(ip) :: lxbegs ( * )  !! Start and end locations of lexemes in query.
      integer(ip) :: lxends ( * )  !! Start and end locations of lexemes in query.
      integer(ip) :: values ( * )  !! Token values or value pointers, as needed.
      real(wp) :: numvls ( * )  !! Buffer containing values of numeric tokens.
      character(*) :: chrbuf  !! Buffer containing string tokens and identifiers.
      integer(ip) :: chbegs ( * )  !! Begin locations of string tokens in CHRBUF.
      integer(ip) :: chends ( * )  !! End locations of string tokens in CHRBUF.
      logical :: scnerr  !! Flag indicating whether query parsed correctly.
      character(*) :: errmsg  !! Scan error description.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekscan ( ctx, (query), maxntk, maxnum, ntoken, tokens, lxbegs, lxends, values, &
         numvls, chrbuf, chbegs, chends, scnerr, errmsg )
      end subroutine zzekscan

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzeksei with the legacy context.
      subroutine zzeksei ( handle, addrss, ival )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: addrss  !! DAS character address.
      integer(ip) :: ival  !! Integer value to write.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksei ( ctx, handle, addrss, ival )
      end subroutine zzeksei

      !> Determine the type of segment required to support a specified
      !> set of columns.
      !>
      !> Legacy interface: calls zz routine zzekstyp with the legacy context.
      integer(ip) function zzekstyp ( ncols, cdscrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekglimit
      integer(ip) :: ncols  !! Number of columns in the segment.
      integer(ip) :: cdscrs ( cdscsz, * )  !! Descriptors of columns.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekstyp = pepper_zzekstyp ( ctx, ncols, cdscrs )
      end function zzekstyp

      !> Return the size of a specified entry in a class 4 column.
      !>
      !> Legacy interface: calls zz routine zzeksz04 with the legacy context.
      integer(ip) function zzeksz04 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzeksz04 = pepper_zzeksz04 ( ctx, handle, segdsc, coldsc, recptr )
      end function zzeksz04

      !> Return the size of a specified entry in a class 5 column.
      !>
      !> Legacy interface: calls zz routine zzeksz05 with the legacy context.
      integer(ip) function zzeksz05 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzeksz05 = pepper_zzeksz05 ( ctx, handle, segdsc, coldsc, recptr )
      end function zzeksz05

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzektcnv with the legacy context.
      subroutine zzektcnv ( timstr, et, error, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ektype
      implicit none
      character(*) :: timstr  !! Time string.
      real(wp) :: et  !! Ephemeris time in seconds past J2000, TDB.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: timstr is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzektcnv ( ctx, (timstr), et, error, errmsg )
      end subroutine zzektcnv

      !> Locate the first occurrence of a specified token in a tokenized
      !> EK query.  The input may actually be any subset of token codes
      !> and corresponding keyword codes from a tokenized query.
      !>
      !> Legacy interface: calls zz routine zzektloc with the legacy context.
      subroutine zzektloc (  tokid,  kwcode,  ntoken,  tokens,  values, &
                             loc,    found                            )
      use pepper_kinds, only: ip
      use pepper_inc_ektokn
      use pepper_inc_ekkeyw
      integer(ip) :: tokid  !! Token ID.
      integer(ip) :: kwcode  !! Keyword code.
      integer(ip) :: ntoken  !! Number of tokens in query.
      integer(ip) :: tokens ( * )  !! Token codes.
      integer(ip) :: values ( * )  !! Pointers to numeric and string token values.
      integer(ip) :: loc  !! Location of first occurrence of token.
      logical :: found  !! Flag indicating whether token was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektloc ( ctx, tokid, kwcode, ntoken, tokens, values, loc, found )
      end subroutine zzektloc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekweqi with the legacy context.
      subroutine zzekweqi ( name, value, eqryi )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      character(*) :: name  !! Name of scalar item to write.
      integer(ip) :: value  !! Value to write.
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekweqi ( ctx, name, value, eqryi )
      end subroutine zzekweqi

      !> Open an existing E-kernel file for reading.
      !>
      !> Legacy interface: calls ctx%ekopr with the legacy context.
      subroutine ekopr ( fname, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! Name of EK file.
      integer(ip) :: handle  !! Handle attached to EK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekopr ( fname, handle )
      end subroutine ekopr

      !> Open an existing E-kernel file for writing.
      !>
      !> Legacy interface: calls ctx%ekopw with the legacy context.
      subroutine ekopw ( fname, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! Name of EK file.
      integer(ip) :: handle  !! Handle attached to EK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekopw ( fname, handle )
      end subroutine ekopw

      !> Get the companion pointer of a specified EK record.
      !>
      !> Legacy interface: calls zz routine zzekgrcp with the legacy context.
      subroutine zzekgrcp ( handle, recptr, ptr )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ekrecptr
      integer(ip) :: handle  !! File handle.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: ptr  !! Pointer to companion of specified EK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekgrcp ( ctx, handle, recptr, ptr )
      end subroutine zzekgrcp

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekinqc with the legacy context.
      subroutine zzekinqc ( value, length, lexbeg, lexend, &
                            eqryi, eqryc,  descr           )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ektype
      use pepper_inc_ekbool
      character(*) :: value  !! Character value.
      integer(ip) :: length  !! Length of item to insert.
      integer(ip) :: lexbeg  !! Begin and end positions of value's lexeme.
      integer(ip) :: lexend  !! Begin and end positions of value's lexeme.
      integer(ip) :: eqryi  ( lbcell : * )  !! Integer portion of encoded query.
      character(*) :: eqryc  !! Character portion of encoded query.
      integer(ip) :: descr  ( * )  !! Descriptor for value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: value is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekinqc ( ctx, (value), length, lexbeg, lexend, eqryi, eqryc, descr )
      end subroutine zzekinqc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekinqn with the legacy context.
      subroutine zzekinqn ( value, type,  lexbeg, lexend, &
                            eqryi, eqryd, descr           )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ektype
      use pepper_inc_ekbool
      real(wp) :: value  !! Numeric value.
      integer(ip) :: type  !! Data type.  May be INT, DP, or TIME.
      integer(ip) :: lexbeg  !! Begin and end positions of value's lexeme.
      integer(ip) :: lexend  !! Begin and end positions of value's lexeme.
      integer(ip) :: eqryi  ( lbcell : * )  !! Integer portion of encoded query.
      real(wp) :: eqryd  ( * )  !! Numeric portion of encoded query.
      integer(ip) :: descr  ( * )  !! Descriptor for value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekinqn ( ctx, value, type, lexbeg, lexend, eqryi, eqryd, descr )
      end subroutine zzekinqn

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekjsqz with the legacy context.
      subroutine zzekjsqz ( jrsbas )
      use pepper_kinds, only: ip
      use pepper_inc_ekjrs
      use pepper_inc_ekqlimit
      integer(ip) :: jrsbas  !! Scratch area base address of join row set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekjsqz ( ctx, jrsbas )
      end subroutine zzekjsqz

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgin with the legacy context.
      subroutine zzekpgin ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! DAS file handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgin ( ctx, handle )
      end subroutine zzekpgin

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgan with the legacy context.
      subroutine zzekpgan ( handle, type, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: type  !! Data type of page to allocate.
      integer(ip) :: p  !! Page number.
      integer(ip) :: base  !! DAS base address of page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgan ( ctx, handle, type, p, base )
      end subroutine zzekpgan

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgal with the legacy context.
      subroutine zzekpgal ( handle, type, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: type  !! Data type of page to allocate.
      integer(ip) :: p  !! Page number.
      integer(ip) :: base  !! DAS base address of page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgal ( ctx, handle, type, p, base )
      end subroutine zzekpgal

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgfr with the legacy context.
      subroutine zzekpgfr ( handle, type, p )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: type  !! Data type of page to allocate.
      integer(ip) :: p  !! Page number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgfr ( ctx, handle, type, p )
      end subroutine zzekpgfr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgrc with the legacy context.
      subroutine zzekpgrc ( handle, p, pagec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: p  !! Page number.
      character(*) :: pagec  !! Character page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgrc ( ctx, handle, p, pagec )
      end subroutine zzekpgrc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgrd with the legacy context.
      subroutine zzekpgrd ( handle, p, paged )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: p  !! Page number.
      real(wp) :: paged ( * )  !! Double precision page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgrd ( ctx, handle, p, paged )
      end subroutine zzekpgrd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgri with the legacy context.
      subroutine zzekpgri ( handle, p, pagei )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: p  !! Page number.
      integer(ip) :: pagei ( * )  !! Integer page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgri ( ctx, handle, p, pagei )
      end subroutine zzekpgri

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgwc with the legacy context.
      subroutine zzekpgwc ( handle, p, pagec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: p  !! Page number.
      character(*) :: pagec  !! Character page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgwc ( ctx, handle, p, pagec )
      end subroutine zzekpgwc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgwd with the legacy context.
      subroutine zzekpgwd ( handle, p, paged )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: p  !! Page number.
      real(wp) :: paged ( * )  !! Double precision page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgwd ( ctx, handle, p, paged )
      end subroutine zzekpgwd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgwi with the legacy context.
      subroutine zzekpgwi ( handle, p, pagei )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      integer(ip) :: p  !! Page number.
      integer(ip) :: pagei ( * )  !! Integer page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgwi ( ctx, handle, p, pagei )
      end subroutine zzekpgwi

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgbs with the legacy context.
      subroutine zzekpgbs ( type, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: p  !! Page number.
      integer(ip) :: base  !! DAS base address of page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgbs ( ctx, type, p, base )
      end subroutine zzekpgbs

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgpg with the legacy context.
      subroutine zzekpgpg ( type, addrss, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: type  !! Data type of address.
      integer(ip) :: addrss  !! DAS address to be mapped.
      integer(ip) :: p  !! Page number.
      integer(ip) :: base  !! DAS base address of page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgpg ( ctx, type, addrss, p, base )
      end subroutine zzekpgpg

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpgst with the legacy context.
      subroutine zzekpgst ( handle, stat, value )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! Paged EK file handle.
      character(*) :: stat  !! Name of requested statistic.
      integer(ip) :: value  !! Value of requested statistic.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekpgst ( ctx, handle, stat, value )
      end subroutine zzekpgst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekqcnj with the legacy context.
      subroutine zzekqcnj ( eqryi, n, size )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      integer(ip) :: n  !! Index within FROM clause of table name to read.
      integer(ip) :: size  !! Size of Nth conjunction in WHERE clause.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekqcnj ( ctx, eqryi, n, size )
      end subroutine zzekqcnj

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekqcon with the legacy context.
      subroutine zzekqcon ( eqryi,   eqryc,   eqryd,   n, &
                            cnstyp, &
                            ltname,  ltidx,   lcname,  lcidx, &
                            opcode, &
                            rtname,  rtidx,   rcname,  rcidx, &
                            dtype, &
                            cbeg,    cend,    dval,    ival   )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      use pepper_inc_ekopcd
      use pepper_inc_ektype
      implicit none
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      real(wp) :: eqryd ( * )  !! Numeric component of query.
      integer(ip) :: n  !! Index of constraint to read.
      integer(ip) :: cnstyp  !! Type of constraint (column or value comparison).
      character(*) :: ltname  !! LHS table name.
      integer(ip) :: ltidx  !! LHS table index in FROM clause.
      character(*) :: lcname  !! LHS column name.
      integer(ip) :: lcidx  !! LHS column index in virtual parent table.
      integer(ip) :: opcode  !! Operation code.
      character(*) :: rtname  !! RHS table name.
      integer(ip) :: rtidx  !! RHS table index in FROM clause.
      character(*) :: rcname  !! RHS column name.
      integer(ip) :: rcidx  !! RHS column index in virtual parent table.
      integer(ip) :: dtype  !! Data type of RHS value.
      integer(ip) :: cbeg  !! Character begin pointer for RHS value.
      integer(ip) :: cend  !! Character end pointer for RHS value.
      real(wp) :: dval  !! RHS double precision value.
      integer(ip) :: ival  !! RHS integer value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: eqryc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekqcon ( ctx, eqryi, (eqryc), eqryd, n, cnstyp, ltname, ltidx, lcname, lcidx, &
         opcode, rtname, rtidx, rcname, rcidx, dtype, cbeg, cend, dval, ival )
      end subroutine zzekqcon

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekqord with the legacy context.
      subroutine zzekqord ( eqryi,  eqryc, n, table, tabidx, column, &
                            colidx, sense                            )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      integer(ip) :: n  !! Index within FROM clause of table name to read.
      character(*) :: table  !! Name of table qualifying Nth ORDER BY column.
      integer(ip) :: tabidx  !! Index of TABLE in FROM clause, if known.
      character(*) :: column  !! Nth column in ORDER BY clause of query.
      integer(ip) :: colidx  !! Index of column in TABLE, if known.
      integer(ip) :: sense  !! Code giving order sense for Nth column.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: eqryc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekqord ( ctx, eqryi, (eqryc), n, table, tabidx, column, colidx, sense )
      end subroutine zzekqord

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekqsel with the legacy context.
      subroutine zzekqsel ( eqryi,  eqryc,   n,       lxbeg,  lxend, &
                            table,  tabidx,  column,  colidx        )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      integer(ip) :: n  !! Index within SELECT clause of table name to read.
      integer(ip) :: lxbeg  !! Begin index in query of selected item.
      integer(ip) :: lxend  !! End index in query of selected item.
      character(*) :: table  !! Name of table qualifying Nth SELECT column.
      integer(ip) :: tabidx  !! Index of TABLE in FROM clause, if known.
      character(*) :: column  !! Nth column in SELECT clause of query.
      integer(ip) :: colidx  !! Index of column in TABLE, if known.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: eqryc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekqsel ( ctx, eqryi, (eqryc), n, lxbeg, lxend, table, tabidx, column, colidx )
      end subroutine zzekqsel

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekqtab with the legacy context.
      subroutine zzekqtab ( eqryi, eqryc, n, table, alias )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      integer(ip) :: n  !! Index within FROM clause of table name to read.
      character(*) :: table  !! Name of Nth table in FROM clause.
      character(*) :: alias  !! Alias of Nth table in FROM clause.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: eqryc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekqtab ( ctx, eqryi, (eqryc), n, table, alias )
      end subroutine zzekqtab

      !> Read a column entry from a specified record in a class 9 column.
      !> Class 9 columns contain fixed record count, fixed-length,
      !> scalar character values.
      !>
      !> Legacy interface: calls zz routine zzekrd09 with the legacy context.
      subroutine zzekrd09 (  handle,  segdsc,  coldsc, &
                             recno,   cvlen,   cval,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekclas09
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: cvlen  !! Length of returned character value.
      character(*) :: cval  !! Character value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrd09 ( ctx, handle, (segdsc), (coldsc), recno, cvlen, cval, isnull )
      end subroutine zzekrd09

      !> Set the companion pointer of a specified EK record.
      !>
      !> Legacy interface: calls zz routine zzeksrcp with the legacy context.
      subroutine zzeksrcp ( handle, recptr, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekrecptr
      integer(ip) :: handle  !! File handle.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: recno  !! Record number of companion of specified EK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksrcp ( ctx, handle, recptr, recno )
      end subroutine zzeksrcp

      !> Set the status of a specified EK record.
      !>
      !> Legacy interface: calls zz routine zzeksrs with the legacy context.
      subroutine zzeksrs ( handle, recptr, status )
      use pepper_kinds, only: ip
      use pepper_inc_ekrecptr
      integer(ip) :: handle  !! File handle.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: status  !! Status of specified EK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksrs ( ctx, handle, recptr, status )
      end subroutine zzeksrs

      !> Return the size of a specified entry in a class 6 column.
      !>
      !> Legacy interface: calls zz routine zzeksz06 with the legacy context.
      integer(ip) function zzeksz06 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzeksz06 = pepper_zzeksz06 ( ctx, handle, segdsc, coldsc, recptr )
      end function zzeksz06

      !> Given a union of EK join row sets, prepare EKVCAL to
      !> compute addresses of row vectors in that union.
      !>
      !> Legacy interface: calls zz routine zzekvset with the legacy context.
      subroutine zzekvset ( njrs, bases )
      use pepper_kinds, only: ip
      integer(ip) :: njrs  !! Number of join row sets in union.
      integer(ip) :: bases  ( * )  !! EK scratch area base addresses of join row sets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekvset ( ctx, njrs, bases )
      end subroutine zzekvset

      !> Find the EK scratch area base address of a row vector and the
      !> corresponding segment vector, where the row vector has a
      !> specified index within a union of join row sets.
      !>
      !> Legacy interface: calls zz routine zzekvcal with the legacy context.
      subroutine zzekvcal ( rwvidx, rwvbas, sgvbas )
      use pepper_kinds, only: ip
      integer(ip) :: rwvidx  !! Index of row vector.
      integer(ip) :: rwvbas  !! EK scratch area base address of row vector.
      integer(ip) :: sgvbas  !! Base address of parent segment vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekvcal ( ctx, rwvidx, rwvbas, sgvbas )
      end subroutine zzekvcal

      !> Return the forward data pointer for a specified EK data page.
      !>
      !> Legacy interface: calls zz routine zzekgfwd with the legacy context.
      subroutine zzekgfwd ( handle, type, p, fward )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: p  !! Page number.
      integer(ip) :: fward  !! Forward data pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekgfwd ( ctx, handle, type, p, fward )
      end subroutine zzekgfwd

      !> Return the link count for a specified EK data page.
      !>
      !> Legacy interface: calls zz routine zzekglnk with the legacy context.
      subroutine zzekglnk ( handle, type, p, nlinks )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: p  !! Page number.
      integer(ip) :: nlinks  !! Number of links to page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekglnk ( ctx, handle, type, p, nlinks )
      end subroutine zzekglnk

      !> Convert the WHERE clause of an EK query to a normalized form.
      !>
      !> Legacy interface: calls zz routine zzeknrml with the legacy context.
      subroutine zzeknrml ( query, ntoken, lxbegs, lxends, tokens, values, numvls, chrbuf, chbegs, chends, eqryi, eqryc, eqryd, error, prserr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekkeyw
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektokn
      use pepper_inc_ektype
      character(*) :: query  !! Input EK query.
      integer(ip) :: ntoken  !! Number of tokens in query.
      integer(ip) :: lxbegs ( * )  !! Start positions of lexemes comprising WHERE clause.
      integer(ip) :: lxends ( * )  !! End positions of lexemes comprising WHERE clause.
      integer(ip) :: tokens ( * )  !! Tokens comprising query.
      integer(ip) :: values ( * )  !! Values associated with tokens.
      real(wp) :: numvls ( * )  !! Buffer containing numeric token values.
      character(*) :: chrbuf  !! Buffer containing string token values.
      integer(ip) :: chbegs ( * )  !! String token begin and end character positions.
      integer(ip) :: chends ( * )  !! String token begin and end character positions.
      integer(ip) :: eqryi  ( lbcell : * )  !! Parsed query and string and number value buffers.
      character(*) :: eqryc  !! Parsed query and string and number value buffers.
      real(wp) :: eqryd  ( * )  !! Parsed query and string and number value buffers.
      logical :: error  !! Parse error flag.
      character(*) :: prserr  !! Parse error message.
      integer(ip), allocatable :: qi(:)
      integer(ip) :: n
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the context routine grows the integer part of the encoded
      ! query (allocatable); this interface keeps SPICELIB's cell: it is
      ! copied in and out, and a query larger than the cell is an error.
      n = nint ( real ( eqryi(-1), wp ), ip )
      allocate ( qi(lbcell:n) )
      qi = eqryi(lbcell:n)
      ! PEPPER: query, chrbuf are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeknrml ( ctx, (query), ntoken, lxbegs, lxends, tokens, values, numvls, &
         (chrbuf), chbegs, chends, qi, eqryc, eqryd, error, prserr )
      if ( ubound ( qi, 1, kind=ip ) > n ) then
         if ( qi(0) > n .and. .not. ctx%failed() ) then
            call ctx%setmsg ( 'The encoded query has # elements; the size of the cell is #.' )
            call ctx%errint ( '#', qi(0) )
            call ctx%errint ( '#', n )
            call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )
         end if
         qi(-1) = n
      end if
      eqryi(lbcell:n) = qi(lbcell:n)
      end subroutine zzeknrml

      !> Set the forward data pointer for a specified EK data page.
      !>
      !> Legacy interface: calls zz routine zzeksfwd with the legacy context.
      subroutine zzeksfwd ( handle, type, p, fward )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: p  !! Page number.
      integer(ip) :: fward  !! Forward data pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksfwd ( ctx, handle, type, p, fward )
      end subroutine zzeksfwd

      !> Set the link count for a specified EK data page.
      !>
      !> Legacy interface: calls zz routine zzekslnk with the legacy context.
      subroutine zzekslnk ( handle, type, p, nlinks )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: p  !! Page number.
      integer(ip) :: nlinks  !! Number of links to page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekslnk ( ctx, handle, type, p, nlinks )
      end subroutine zzekslnk

      !> Execute a 1-3 split:  split the root node to create two new
      !> children, leaving a single key in the root.
      !>
      !> Legacy interface: calls zz routine zzektr13 with the legacy context.
      subroutine zzektr13 ( handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektr13 ( ctx, handle, tree )
      end subroutine zzektr13

      !> Execute a 3-1 merge:  move the contents of two children into
      !> the root node and delete the children.
      !>
      !> Legacy interface: calls zz routine zzektr31 with the legacy context.
      subroutine zzektr31 ( handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektr31 ( ctx, handle, tree )
      end subroutine zzektr31

      !> Map a node in a tree to its DAS base integer address.
      !>
      !> Legacy interface: calls zz routine zzektrbs with the legacy context.
      integer(ip) function zzektrbs ( node )
      use pepper_kinds, only: ip
      use pepper_inc_ektype
      integer(ip) :: node  !! Node number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzektrbs = pepper_zzektrbs ( ctx, node )
      end function zzektrbs

      !> Free a tree:  deallocate all pages belonging to the tree.
      !>
      !> Legacy interface: calls zz routine zzektrfr with the legacy context.
      subroutine zzektrfr ( handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrfr ( ctx, handle, tree )
      end subroutine zzektrfr

      !> Initialize an EK tree, returning the root of the tree.
      !>
      !> Legacy interface: calls zz routine zzektrit with the legacy context.
      subroutine zzektrit ( handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrit ( ctx, handle, tree )
      end subroutine zzektrit

      !> Locate a specified key.  Return metadata describing the node
      !> containing the key.
      !>
      !> Legacy interface: calls zz routine zzektrlk with the legacy context.
      subroutine zzektrlk ( handle, tree, key, idx, node, noffst, level, value )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key corresponding to value.
      integer(ip) :: idx  !! Node-relative index of KEY.
      integer(ip) :: node  !! Node containing key.
      integer(ip) :: noffst  !! Offset of NODE.
      integer(ip) :: level  !! Level of NODE.
      integer(ip) :: value  !! Value associated with KEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrlk ( ctx, handle, tree, key, idx, node, noffst, level, value )
      end subroutine zzektrlk

      !> Given a key, return general information pertaining to the key's
      !> parent node.
      !>
      !> Legacy interface: calls zz routine zzektrpi with the legacy context.
      subroutine zzektrpi ( handle, tree,  key,  parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib, rpidx,  rpkey, rsib   )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key belonging to node of interest.
      integer(ip) :: parent  !! Parent node of the node containing KEY.
      integer(ip) :: pkey  !! A key in the parent node.
      integer(ip) :: poffst  !! Key offset of the parent node.
      integer(ip) :: lpidx  !! Node-relative index of the left parent key.
      integer(ip) :: lpkey  !! Left parent key.
      integer(ip) :: lsib  !! Node number of left sibling.
      integer(ip) :: rpidx  !! Node-relative index of the right parent key.
      integer(ip) :: rpkey  !! Right parent key.
      integer(ip) :: rsib  !! Node number of right sibling.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrpi ( ctx, handle, tree, key, parent, pkey, poffst, lpidx, lpkey, lsib, rpidx, rpkey, rsib )
      end subroutine zzektrpi

      !> Rotate a specified number of keys from one node, through
      !> the parent, into a neighboring sibling node.
      !>
      !> Legacy interface: calls zz routine zzektrrk with the legacy context.
      subroutine zzektrrk ( handle,  tree,   left,  right, &
                            parent,  pkidx,  nrot         )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: left  !! Left node of pair to participate in rotation.
      integer(ip) :: right  !! Right node of pair to participate in rotation.
      integer(ip) :: parent  !! Parent node of pair to participate in rotation.
      integer(ip) :: pkidx  !! Parent key index.
      integer(ip) :: nrot  !! Number of keys to rotate.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrrk ( ctx, handle, tree, left, right, parent, pkidx, nrot )
      end subroutine zzektrrk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekweed with the legacy context.
      subroutine zzekweed ( njrs, bases, nrows )
      use pepper_kinds, only: ip
      use pepper_inc_ekjrs
      use pepper_inc_ekqlimit
      integer(ip) :: njrs  !! Number of join row sets in union.
      integer(ip) :: bases ( * )  !! Scratch area base addresses of join row sets.
      integer(ip) :: nrows  !! Total number of row vectors in join row set union.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekweed ( ctx, njrs, bases, nrows )
      end subroutine zzekweed

      !> Open a new E-kernel file and prepare the file for writing.
      !>
      !> Legacy interface: calls ctx%ekopn with the legacy context.
      subroutine ekopn ( fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      use pepper_inc_ektype
      use pepper_inc_ekfilpar
      implicit none
      character(*) :: fname  !! Name of EK file.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: ncomch  !! The number of characters to reserve for comments.
      integer(ip) :: handle  !! Handle attached to new EK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekopn ( fname, ifname, ncomch, handle )
      end subroutine ekopn

      !> Open a scratch (temporary) E-kernel file and prepare the file
      !> for writing.
      !>
      !> Legacy interface: calls ctx%ekops with the legacy context.
      subroutine ekops ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_ektype
      use pepper_inc_ekfilpar
      implicit none
      integer(ip) :: handle  !! File handle attached to new EK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekops ( handle )
      end subroutine ekops

      !> Create a new type 1 index for a specified EK column.
      !>
      !> Legacy interface: calls zz routine zzekcix1 with the legacy context.
      subroutine zzekcix1 ( handle, coldsc )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      integer(ip) :: handle  !! File handle.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekcix1 ( ctx, handle, coldsc )
      end subroutine zzekcix1

      !> Parse an EK query that has been scanned and tokenized.
      !> Represent the result as an encoded EK query.
      !>
      !> Legacy interface: calls zz routine zzekpars with the legacy context.
      subroutine zzekpars (  query,   ntoken,  lxbegs,  lxends, &
                             tokens,  values,  numvls,  chrbuf, &
                             chbegs,  chends,  eqryi,   eqryc, &
                             eqryd,   error,   prserr          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ektokn
      use pepper_inc_ekkeyw
      use pepper_inc_ekbool
      use pepper_inc_ektype
      use pepper_inc_ekopcd
      use pepper_inc_ekquery
      use pepper_inc_ekqlimit
      implicit none
      character(*) :: query  !! Query in string form.
      integer(ip) :: ntoken  !! Number of tokens in query.
      integer(ip) :: lxbegs ( * )  !! Lexeme begin and end positions in QUERY.
      integer(ip) :: lxends ( * )  !! Lexeme begin and end positions in QUERY.
      integer(ip) :: tokens ( * )  !! Token codes.
      integer(ip) :: values ( * )  !! Values associated with tokens.
      real(wp) :: numvls ( * )  !! Buffer containing numeric token values.
      character(*) :: chrbuf  !! Buffer containing string token values.
      integer(ip) :: chbegs ( * )  !! String token begin and end character positions.
      integer(ip) :: chends ( * )  !! String token begin and end character positions.
      integer(ip) :: eqryi  ( lbcell : * )  !! Parsed query and string and number value buffers.
      character(*) :: eqryc  !! Parsed query and string and number value buffers.
      real(wp) :: eqryd  ( * )  !! Parsed query and string and number value buffers.
      logical :: error  !! Flag indicating whether query parsed correctly.
      character(*) :: prserr  !! Parse error description.
      integer(ip), allocatable :: qi(:)
      integer(ip) :: n
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: the context routine grows the integer part of the encoded
      ! query (allocatable); this interface keeps SPICELIB's cell: it is
      ! copied in and out, and a query larger than the cell is an error.
      n = nint ( real ( eqryi(-1), wp ), ip )
      allocate ( qi(lbcell:n) )
      qi = eqryi(lbcell:n)
      ! PEPPER: query, chrbuf are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekpars ( ctx, (query), ntoken, lxbegs, lxends, tokens, values, numvls, &
         (chrbuf), chbegs, chends, qi, eqryc, eqryd, error, prserr )
      if ( ubound ( qi, 1, kind=ip ) > n ) then
         if ( qi(0) > n .and. .not. ctx%failed() ) then
            call ctx%setmsg ( 'The encoded query has # elements; the size of the cell is #.' )
            call ctx%errint ( '#', qi(0) )
            call ctx%errint ( '#', n )
            call ctx%sigerr ( 'SPICE(CELLTOOSMALL)' )
         end if
         qi(-1) = n
      end if
      eqryi(lbcell:n) = qi(lbcell:n)
      end subroutine zzekpars

      !> Execute a 2-3 split:  split two sibling nodes into three nodes,
      !> each one approximately 2/3 full.
      !>
      !> Legacy interface: calls zz routine zzektr23 with the legacy context.
      subroutine zzektr23 ( handle,  tree,   left,   right, &
                            parent,  pkidx,  overfl        )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: left  !! Left sibling node.
      integer(ip) :: right  !! Right sibling node.
      integer(ip) :: parent  !! Common parent node.
      integer(ip) :: pkidx  !! Node-relative index of parent key.
      logical :: overfl  !! Overflow flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektr23 ( ctx, handle, tree, left, right, parent, pkidx, overfl )
      end subroutine zzektr23

      !> Execute a 3-2 merge:  merge three neighboring sibling nodes, two
      !> of which contain the minimum number of keys and one of which
      !> has an underflow of one key, into two nodes, each one
      !> approximately full.
      !>
      !> Legacy interface: calls zz routine zzektr32 with the legacy context.
      subroutine zzektr32 ( handle,  tree,    left,    middle, &
                            right,   parent,  lpkidx,  undrfl )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: left  !! Left sibling node.
      integer(ip) :: middle  !! Middle sibling node.
      integer(ip) :: right  !! Right sibling node.
      integer(ip) :: parent  !! Common parent node.
      integer(ip) :: lpkidx  !! Node-relative index of left parent key of MIDDLE.
      logical :: undrfl  !! Underflow flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektr32 ( ctx, handle, tree, left, middle, right, parent, lpkidx, undrfl )
      end subroutine zzektr32

      !> Return the data pointer from a specified location in an EK tree.
      !>
      !> Legacy interface: calls zz routine zzektrdp with the legacy context.
      subroutine zzektrdp ( handle, tree, key, ptr )
      use pepper_kinds, only: ip
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key corresponding to pointer.
      integer(ip) :: ptr  !! Data pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrdp ( ctx, handle, tree, key, ptr )
      end subroutine zzektrdp

      !> Return the number of keys in a node.
      !>
      !> Legacy interface: calls zz routine zzektrnk with the legacy context.
      integer(ip) function zzektrnk ( handle, tree, node )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: node  !! Node of interest.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzektrnk = pepper_zzektrnk ( ctx, handle, tree, node )
      end function zzektrnk

      !> Identify the immediate siblings of a node:  return a key in each
      !> sibling and the siblings' node numbers.
      !>
      !> Legacy interface: calls zz routine zzektrsb with the legacy context.
      subroutine zzektrsb ( handle, tree, key, lsib, lkey, rsib, rkey )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key of interest.
      integer(ip) :: lsib  !! Left sibling node.
      integer(ip) :: lkey  !! Key in left sibling.
      integer(ip) :: rsib  !! Right sibling node.
      integer(ip) :: rkey  !! Key in right sibling.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrsb ( ctx, handle, tree, key, lsib, lkey, rsib, rkey )
      end subroutine zzektrsb

      !> Return the number of keys in a tree.
      !>
      !> Legacy interface: calls zz routine zzektrsz with the legacy context.
      integer(ip) function zzektrsz ( handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzektrsz = pepper_zzektrsz ( ctx, handle, tree )
      end function zzektrsz

      !> Delete a value from a tree at a specified location without
      !> balancing the tree.
      !>
      !> Legacy interface: calls zz routine zzektrud with the legacy context.
      subroutine zzektrud ( handle, tree, key, trgkey, undrfl )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key to delete.
      integer(ip) :: trgkey  !! Key identifying node from which deletion occurred.
      logical :: undrfl  !! Underflow flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrud ( ctx, handle, tree, key, trgkey, undrfl )
      end subroutine zzektrud

      !> Insert a value into a tree at a specified location without
      !> balancing the tree.
      !>
      !> Legacy interface: calls zz routine zzektrui with the legacy context.
      subroutine zzektrui ( handle, tree, key, value, overfl )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key to insert.
      integer(ip) :: value  !! Value to insert.
      logical :: overfl  !! Overflow flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrui ( ctx, handle, tree, key, value, overfl )
      end subroutine zzektrui

      !> Return the number of segments in a specified EK.
      !>
      !> Legacy interface: calls ctx%eknseg with the legacy context.
      integer(ip) function eknseg ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_ekfilpar
      implicit none
      integer(ip) :: handle  !! EK file handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      eknseg = ctx%eknseg ( handle )
      end function eknseg

      !> Look up a specified record pointer from an EK index.
      !>
      !> Legacy interface: calls zz routine zzekixlk with the legacy context.
      subroutine zzekixlk ( handle, coldsc, key, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      integer(ip) :: handle  !! File handle.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: key  !! Key.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekixlk ( ctx, handle, (coldsc), key, recptr )
      end subroutine zzekixlk

      !> Look up the record pointer of an EK record having a specified
      !> ordinal position in a specified EK segment.
      !>
      !> Legacy interface: calls zz routine zzekrplk with the legacy context.
      subroutine zzekrplk ( handle, segdsc, n, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: n  !! Ordinal position of record.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrplk ( ctx, handle, (segdsc), n, recptr )
      end subroutine zzekrplk

      !> One-shot tree load:  insert an entire array into an empty
      !> tree.
      !>
      !> Legacy interface: calls zz routine zzektr1s with the legacy context.
      subroutine zzektr1s ( handle, tree, size, values )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: size  !! Size of tree.
      integer(ip) :: values  ( * )  !! Values to insert.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektr1s ( ctx, handle, tree, size, values )
      end subroutine zzektr1s

      !> Solve overflow in a node by balancing the node
      !> with one of its sibling nodes.
      !>
      !> Legacy interface: calls zz routine zzektrbn with the legacy context.
      subroutine zzektrbn ( handle, tree, left, right, parent, pkidx )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: left  !! Left node of pair to be balanced.
      integer(ip) :: right  !! Right node of pair to be balanced.
      integer(ip) :: parent  !! Parent node of pair to be balanced.
      integer(ip) :: pkidx  !! Parent key index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrbn ( ctx, handle, tree, left, right, parent, pkidx )
      end subroutine zzektrbn

      !> Get a key from a node by index:  return the key having a specified
      !> index in a specified node.  The node of interest is identified
      !> by a key in the node.
      !>
      !> Legacy interface: calls zz routine zzektrki with the legacy context.
      subroutine zzektrki ( handle, tree, nodkey, n, key )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: nodkey  !! Key identifying node containing key of interest.
      integer(ip) :: n  !! Index of key of interest.
      integer(ip) :: key  !! Key located at index N.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrki ( ctx, handle, tree, nodkey, n, key )
      end subroutine zzektrki

      !> Search an EK tree linearly to find a specified data value.  The
      !> function returns the index at which the value is found.
      !>
      !> Legacy interface: calls zz routine zzektrls with the legacy context.
      integer(ip) function zzektrls ( handle, tree, ival )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Tree root.
      integer(ip) :: ival  !! Value to search for.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzektrls = pepper_zzektrls ( ctx, handle, tree, ival )
      end function zzektrls

      !> Return the integer metadata location of a specified segment.  The
      !> number and DAS integer base address of the first integer
      !> page of the metadata are returned.
      !>
      !> Legacy interface: calls zz routine zzekmloc with the legacy context.
      subroutine zzekmloc ( handle, segno, page, base )
      use pepper_kinds, only: ip
      use pepper_inc_ekfilpar
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: page  !! Integer metadata start page number.
      integer(ip) :: base  !! Page base.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekmloc ( ctx, handle, segno, page, base )
      end subroutine zzekmloc

      !> Delete a value from an EK tree at a specified location.
      !>
      !> Legacy interface: calls zz routine zzektrdl with the legacy context.
      subroutine zzektrdl ( handle, tree, key )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key at which to delete value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrdl ( ctx, handle, tree, key )
      end subroutine zzektrdl

      !> Insert a value into an EK tree at a specified location.
      !>
      !> Legacy interface: calls zz routine zzektrin with the legacy context.
      subroutine zzektrin ( handle, tree, key, value )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: key  !! Key at which to insert value.
      integer(ip) :: value  !! Value to insert.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrin ( ctx, handle, tree, key, value )
      end subroutine zzektrin

      !> Delete a specified data page for a specified EK segment.
      !>
      !> Legacy interface: calls zz routine zzekdps with the legacy context.
      subroutine zzekdps ( handle, segdsc, type, p )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: p  !! Page number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekdps ( ctx, handle, segdsc, type, p )
      end subroutine zzekdps

      !> Complete a fast load operation on a new type 1 E-kernel segment.
      !>
      !> Legacy interface: calls zz routine zzekff01 with the legacy context.
      subroutine zzekff01 ( handle, segno, rcptrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekglimit
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: rcptrs ( * )  !! Record pointers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekff01 ( ctx, handle, segno, rcptrs )
      end subroutine zzekff01

      !> Look up the descriptor of a specified EK segment.
      !>
      !> Legacy interface: calls zz routine zzeksdsc with the legacy context.
      subroutine zzeksdsc ( handle, segno, segdsc )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksdsc ( ctx, handle, segno, segdsc )
      end subroutine zzeksdsc

      !> Return general segment information for a specified segment in a
      !> specified EK.
      !>
      !> Legacy interface: calls zz routine zzeksinf with the legacy context.
      subroutine zzeksinf ( handle,  segno,   tabnam, &
                            segdsc,  cnames,  cdscrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      integer(ip) :: handle  !! Handle of EK.
      integer(ip) :: segno  !! Number of segment to be summarized.
      character(*) :: tabnam  !! Name of table containing segment.
      integer(ip) :: segdsc ( sdscsz    )  !! Segment descriptor.
      character(*) :: cnames (         * )  !! Names of columns in segment.
      integer(ip) :: cdscrs ( cdscsz, * )  !! Descriptors of columns in segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeksinf ( ctx, handle, segno, tabnam, segdsc, cnames, cdscrs )
      end subroutine zzeksinf

      !> Append an item to a tree.  The key indicating the location of
      !> the new item is returned.
      !>
      !> Legacy interface: calls zz routine zzektrap with the legacy context.
      subroutine zzektrap ( handle, tree, value, key )
      use pepper_kinds, only: ip
      integer(ip) :: handle  !! File handle.
      integer(ip) :: tree  !! Root of tree.
      integer(ip) :: value  !! Value to append.
      integer(ip) :: key  !! Key pointing to new value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzektrap ( ctx, handle, tree, value, key )
      end subroutine zzektrap

      !> Complete a fast write operation on a new E-kernel segment.
      !>
      !> Legacy interface: calls ctx%ekffld with the legacy context.
      subroutine ekffld ( handle, segno, rcptrs )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: rcptrs ( * )  !! Record pointers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekffld ( handle, segno, rcptrs )
      end subroutine ekffld

      !> Return summary information for a specified segment in a
      !> specified EK.
      !>
      !> Legacy interface: calls ctx%ekssum with the legacy context.
      subroutine ekssum (  handle,  segno,   tabnam,  nrows, &
                           ncols,   cnames,  dtypes,  sizes, &
                           strlns,  indexd,  nullok          )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekglimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle of EK.
      integer(ip) :: segno  !! Number of segment to be summarized.
      character(*) :: tabnam  !! Name of table containing segment.
      integer(ip) :: nrows  !! Number of rows in segment.
      integer(ip) :: ncols  !! Number of columns in segment.
      character(*) :: cnames ( * )  !! Names of columns in segment.
      character(*) :: dtypes ( * )  !! Data types of columns in segment.
      integer(ip) :: sizes  ( * )  !! Entry sizes of columns in segment.
      integer(ip) :: strlns ( * )  !! String lengths of columns in segment.
      logical :: indexd ( * )  !! Flags indicating whether columns are indexed.
      logical :: nullok ( * )  !! Flags indicating whether columns allow nulls.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekssum ( handle, segno, tabnam, nrows, ncols, cnames, dtypes, sizes, strlns, indexd, nullok )
      end subroutine ekssum

      !> Allocate a series of contiguous data pages for a specified EK
      !> segment.
      !>
      !> Legacy interface: calls zz routine zzekacps with the legacy context.
      subroutine zzekacps ( handle, segdsc, type, n, p, base  )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: type  !! Data type of page.
      integer(ip) :: n  !! Number of pages to allocate.
      integer(ip) :: p  !! Page number.
      integer(ip) :: base  !! DAS base address of page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekacps ( ctx, handle, (segdsc), type, n, p, base )
      end subroutine zzekacps

      !> Allocate a data page for a specified EK segment.
      !>
      !> Legacy interface: calls zz routine zzekaps with the legacy context.
      subroutine zzekaps ( handle, segdsc, type, new, p, base  )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: type  !! Data type of page.
      logical :: new  !! Flag indicating whether page is new.
      integer(ip) :: p  !! Page number.
      integer(ip) :: base  !! DAS base address of page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekaps ( ctx, handle, (segdsc), type, new, p, base )
      end subroutine zzekaps

      !> Start a new type 1 segment in an E-kernel.
      !>
      !> Legacy interface: calls zz routine zzekbs01 with the legacy context.
      subroutine zzekbs01 (  handle,  tabnam,  ncols, &
                             cnames,  cdscrs,  segno  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekfilpar
      use pepper_inc_ekglimit
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      character(*) :: tabnam  !! Table name.
      integer(ip) :: ncols  !! Number of columns in the segment.
      character(*) :: cnames ( * )  !! Names of columns.
      integer(ip) :: cdscrs ( cdscsz, * )  !! Descriptors of columns.
      integer(ip) :: segno  !! Segment number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekbs01 ( ctx, handle, tabnam, ncols, cnames, cdscrs, segno )
      end subroutine zzekbs01

      !> Start a new type 2 segment in an E-kernel.
      !>
      !> Legacy interface: calls zz routine zzekbs02 with the legacy context.
      subroutine zzekbs02 (  handle,  tabnam,  ncols, &
                             cnames,  cdscrs,  segno  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekfilpar
      use pepper_inc_ekglimit
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      character(*) :: tabnam  !! Table name.
      integer(ip) :: ncols  !! Number of columns in the segment.
      character(*) :: cnames ( * )  !! Names of columns.
      integer(ip) :: cdscrs ( cdscsz, * )  !! Descriptors of columns.
      integer(ip) :: segno  !! Segment number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekbs02 ( ctx, handle, tabnam, ncols, cnames, cdscrs, segno )
      end subroutine zzekbs02

      !> Delete a specified class 4 column entry from an EK record.
      !>
      !> Legacy interface: calls zz routine zzekde04 with the legacy context.
      subroutine zzekde04 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekde04 ( ctx, handle, segdsc, (coldsc), recptr )
      end subroutine zzekde04

      !> Find the EK record number corresponding to a specified record
      !> pointer.  Beware, for type 1 segments, this is done by linear
      !> searching.
      !>
      !> Legacy interface: calls zz routine zzekrp2n with the legacy context.
      integer(ip) function zzekrp2n ( handle, segno, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekrp2n = pepper_zzekrp2n ( ctx, handle, segno, recptr )
      end function zzekrp2n

end module pepper_legacy_ek_storage
