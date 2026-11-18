!> The legacy interface's routines of the subsystem ek (115 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_ek

   use pepper_instance_module, only: pepper_context, pepper_zzekac01 => zzekac01, &
      pepper_zzekac02 => zzekac02, pepper_zzekac03 => zzekac03, pepper_zzekac04 => zzekac04, &
      pepper_zzekac05 => zzekac05, pepper_zzekac06 => zzekac06, pepper_zzekad04 => zzekad04, &
      pepper_zzekad05 => zzekad05, pepper_zzekad06 => zzekad06, pepper_zzekde05 => zzekde05, &
      pepper_zzekde06 => zzekde06, pepper_zzekesiz => zzekesiz, pepper_zzekgcdp => zzekgcdp, &
      pepper_zzekif01 => zzekif01, pepper_zzekif02 => zzekif02, pepper_zzekrd01 => zzekrd01, &
      pepper_zzekrd02 => zzekrd02, pepper_zzekrd03 => zzekrd03, pepper_zzekrd04 => zzekrd04, &
      pepper_zzekrd05 => zzekrd05, pepper_zzekrd06 => zzekrd06, pepper_zzekrd07 => zzekrd07, &
      pepper_zzekrd08 => zzekrd08, pepper_zzekscdp => zzekscdp, pepper_zzekwpac => zzekwpac, &
      pepper_zzekwpai => zzekwpai, pepper_zzekwpal => zzekwpal, pepper_zzekac07 => zzekac07, &
      pepper_zzekac08 => zzekac08, pepper_zzekac09 => zzekac09, pepper_zzekrsc => zzekrsc, &
      pepper_zzekrsd => zzekrsd, pepper_zzekrsi => zzekrsi, pepper_zzekue04 => zzekue04, &
      pepper_zzekue05 => zzekue05, pepper_zzekue06 => zzekue06, pepper_zzekecmp => zzekecmp, &
      pepper_zzekscmp => zzekscmp, pepper_zzekerc1 => zzekerc1, pepper_zzekerd1 => zzekerd1, &
      pepper_zzekeri1 => zzekeri1, pepper_zzekllec => zzekllec, pepper_zzeklled => zzeklled, &
      pepper_zzekllei => zzekllei, pepper_zzeklltc => zzeklltc, pepper_zzeklltd => zzeklltd, &
      pepper_zzekllti => zzekllti, pepper_zzekrcmp => zzekrcmp, pepper_zzekrmch => zzekrmch, &
      pepper_zzekvcmp => zzekvcmp, pepper_zzekvmch => zzekvmch, pepper_zzekille => zzekille, &
      pepper_zzekillt => zzekillt, pepper_zzekjsrt => zzekjsrt, pepper_zzeklerc => zzeklerc, &
      pepper_zzeklerd => zzeklerd, pepper_zzekleri => zzekleri, pepper_zzekfrx => zzekfrx, &
      pepper_zzekiic1 => zzekiic1, pepper_zzekiid1 => zzekiid1, pepper_zzekiii1 => zzekiii1, &
      pepper_zzekjprp => zzekjprp, pepper_zzekjnxt => zzekjnxt, pepper_zzekkey => zzekkey, &
      pepper_zzekad01 => zzekad01, pepper_zzekad02 => zzekad02, pepper_zzekad03 => zzekad03, &
      pepper_zzekixdl => zzekixdl, pepper_zzekjoin => zzekjoin, pepper_instance, &
      pepper_zzekde01 => zzekde01, pepper_zzekde02 => zzekde02, pepper_zzekde03 => zzekde03, &
      pepper_zzekue01 => zzekue01, pepper_zzekue02 => zzekue02, pepper_zzekcchk => zzekcchk, &
      pepper_zzekpcol => zzekpcol, pepper_zzeksemc => zzeksemc, pepper_zzektres => zzektres, &
      pepper_zzekue03 => zzekue03, pepper_zzeknres => zzeknres, pepper_zzekencd => zzekencd
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell, lbpool

   implicit none

   private
   public :: ekbseg, ekinsr, stcc01, zzekac01, zzekac02, zzekac03, zzekac04, zzekac05, zzekac06, &
      zzekad04, zzekad05, zzekad06, zzekde05, zzekde06, zzekesiz, zzekgcdp, zzekif01, zzekif02, &
      zzekrd01, zzekrd02, zzekrd03, zzekrd04, zzekrd05, zzekrd06, zzekrd07, zzekrd08, zzekscdp, &
      zzekwpac, zzekwpai, zzekwpal, ekappr, ekifld, ekrcec, ekrced, ekrcei, zzekac07, zzekac08, &
      zzekac09, zzekrsc, zzekrsd, zzekrsi, zzekue04, zzekue05, zzekue06, ekaclc, ekacld, ekacli, &
      zzekecmp, zzekscmp, zzekerc1, zzekerd1, zzekeri1, zzekllec, zzeklled, zzekllei, zzeklltc, &
      zzeklltd, zzekllti, zzekrcmp, zzekrmch, zzekvcmp, zzekvmch, zzekille, zzekillt, zzekjsrt, &
      zzeklerc, zzeklerd, zzekleri, zzekfrx, zzekiic1, zzekiid1, zzekiii1, zzekjprp, zzekjnxt, &
      zzekkey, zzekad01, zzekad02, zzekad03, zzekixdl, zzekjoin, ekacec, ekaced, ekacei, eklef, &
      ekuef, ekntab, ektnam, ekccnt, ekcii, eksrch, eknelt, ekgc, ekgd, ekgi, zzekde01, zzekde02, &
      zzekde03, zzekue01, zzekue02, ekdelr, ekuced, ekucei, stcg01, stcl01, zzekcchk, zzekpcol, &
      zzeksemc, zzektres, zzekue03, ekucec, zzeknres, ekfind, zzekencd, ekpsel, stcf01

contains

      !> Start a new segment in an E-kernel.
      !>
      !> Legacy interface: calls ctx%ekbseg with the legacy context.
      subroutine ekbseg ( handle, tabnam, ncols, cnames, decls, segno )
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
      implicit none
      integer(ip) :: handle  !! File handle.
      character(*) :: tabnam  !! Table name.
      integer(ip) :: ncols  !! Number of columns in the segment.
      character(*) :: cnames ( * )  !! Names of columns.
      character(*) :: decls  ( * )  !! Declarations of columns.
      integer(ip) :: segno  !! Segment number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekbseg ( handle, tabnam, ncols, cnames, decls, segno )
      end subroutine ekbseg

      !> Add a new, empty record to a specified E-kernel segment at
      !> a specified index.
      !>
      !> Legacy interface: calls ctx%ekinsr with the legacy context.
      subroutine ekinsr ( handle, segno, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: recno  !! Record number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekinsr ( handle, segno, recno )
      end subroutine ekinsr

      !> Check whether a file is a type 1 star catalog and return the
      !> catalog's table name if it is.
      !>
      !> Legacy interface: calls ctx%stcc01 with the legacy context.
      subroutine stcc01 ( catfnm, tabnam, istyp1, errmsg )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ektnamsz
      use pepper_inc_ekglimit
      implicit none
      character(*) :: catfnm  !! Catalog file name.
      character(*) :: tabnam  !! Catalog table name.
      logical :: istyp1  !! .TRUE. when file is type 1 star catalog.
      character(*) :: errmsg  !! Error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: catfnm is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%stcc01 ( (catfnm), tabnam, istyp1, errmsg )
      end subroutine stcc01

      !> Add an entire class 1 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac01 with the legacy context.
      subroutine zzekac01 (  handle,  segdsc,  coldsc,  ivals, &
                             nlflgs,  rcptrs,  wkindx         )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: rcptrs ( * )  !! Array of record pointers for segment.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac01 ( ctx, handle, segdsc, coldsc, ivals, nlflgs, rcptrs, wkindx )
      end subroutine zzekac01

      !> Add an entire class 2 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac02 with the legacy context.
      subroutine zzekac02 (  handle,  segdsc,  coldsc,  dvals, &
                             nlflgs,  rcptrs,  wkindx         )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      real(wp) :: dvals  ( * )  !! D.p. values to add to column.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: rcptrs ( * )  !! Array of record pointers for segment.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac02 ( ctx, handle, segdsc, coldsc, dvals, nlflgs, rcptrs, wkindx )
      end subroutine zzekac02

      !> Add an entire class 3 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac03 with the legacy context.
      subroutine zzekac03 (  handle,  segdsc,  coldsc,  cvals, &
                             nlflgs,  rcptrs,  wkindx  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      character(*) :: cvals  ( * )  !! Character values to add to column.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: rcptrs ( * )  !! Array of record pointers for segment.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac03 ( ctx, handle, segdsc, coldsc, cvals, nlflgs, rcptrs, wkindx )
      end subroutine zzekac03

      !> Add an entire class 4 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac04 with the legacy context.
      subroutine zzekac04 (  handle,  segdsc,  coldsc, &
                             ivals,   entszs,  nlflgs  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      integer(ip) :: entszs ( * )  !! Array of sizes of column entries.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac04 ( ctx, handle, segdsc, coldsc, ivals, entszs, nlflgs )
      end subroutine zzekac04

      !> Add an entire class 5 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac05 with the legacy context.
      subroutine zzekac05 (  handle,  segdsc,  coldsc, &
                             dvals,   entszs,  nlflgs  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      real(wp) :: dvals  ( * )  !! D.p. values to add to column.
      integer(ip) :: entszs ( * )  !! Array of sizes of column entries.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac05 ( ctx, handle, segdsc, coldsc, dvals, entszs, nlflgs )
      end subroutine zzekac05

      !> Add an entire class 6 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac06 with the legacy context.
      subroutine zzekac06 (  handle,  segdsc,  coldsc, &
                             cvals,   entszs,  nlflgs )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      character(*) :: cvals  ( * )  !! Character values to add to column.
      integer(ip) :: entszs ( * )  !! Array of sizes of column entries.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac06 ( ctx, handle, segdsc, coldsc, cvals, entszs, nlflgs )
      end subroutine zzekac06

      !> Add a column entry to a specified record in a class 4 column.
      !> The entries of class 4 columns are arrays of integer values.
      !>
      !> Legacy interface: calls zz routine zzekad04 with the legacy context.
      subroutine zzekad04 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   ivals,  isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: nvals  !! Number of values to add to column.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekad04 ( ctx, handle, segdsc, coldsc, recptr, nvals, ivals, isnull )
      end subroutine zzekad04

      !> Add a column entry to a specified record in a class 5 column.
      !> The entries of class 5 columns are arrays of d.p. values.
      !>
      !> Legacy interface: calls zz routine zzekad05 with the legacy context.
      subroutine zzekad05 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   dvals,  isnull  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: recptr  !! Pointer to the record to which data is to be added.
      integer(ip) :: nvals  !! Number of values to add to column.
      real(wp) :: dvals  ( * )  !! Double precision values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekad05 ( ctx, handle, segdsc, coldsc, recptr, nvals, dvals, isnull )
      end subroutine zzekad05

      !> Add a column entry to a specified record in a class 6 column.
      !> The entries of class 6 columns are arrays of character string
      !> values.
      !>
      !> Legacy interface: calls zz routine zzekad06 with the legacy context.
      subroutine zzekad06 ( handle, segdsc, coldsc, recptr, nvals, cvals, isnull )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: recptr  !! Pointer to the record to which data is to be added.
      integer(ip) :: nvals  !! Number of values to add to column.
      character(*) :: cvals  ( * )  !! Character values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekad06 ( ctx, handle, segdsc, coldsc, recptr, nvals, cvals, isnull )
      end subroutine zzekad06

      !> Delete a specified class 5 column entry from an EK record.
      !>
      !> Legacy interface: calls zz routine zzekde05 with the legacy context.
      subroutine zzekde05 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
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
      call pepper_zzekde05 ( ctx, handle, segdsc, (coldsc), recptr )
      end subroutine zzekde05

      !> Delete a specified class 6 column entry from an EK record.
      !>
      !> Legacy interface: calls zz routine zzekde06 with the legacy context.
      subroutine zzekde06 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekde06 ( ctx, handle, segdsc, (coldsc), recptr )
      end subroutine zzekde06

      !> Return the size of a specified column entry.
      !>
      !> Legacy interface: calls zz routine zzekesiz with the legacy context.
      integer(ip) function zzekesiz ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_eksegdsc
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekesiz = pepper_zzekesiz ( ctx, handle, segdsc, coldsc, recptr )
      end function zzekesiz

      !> Return the data pointer for a specified EK column entry.
      !>
      !> Legacy interface: calls zz routine zzekgcdp with the legacy context.
      subroutine zzekgcdp ( handle, segdsc, coldsc, recptr, datptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ekrecptr
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: datptr  !! Data pointer of column entry.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekgcdp ( ctx, handle, (segdsc), (coldsc), recptr, datptr )
      end subroutine zzekgcdp

      !> Initialize a new type 1 EK segment to allow fast loading.
      !>
      !> Legacy interface: calls zz routine zzekif01 with the legacy context.
      subroutine zzekif01 ( handle, segno, rcptrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: rcptrs ( * )  !! Array of record pointers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekif01 ( ctx, handle, segno, rcptrs )
      end subroutine zzekif01

      !> Initialize a new type 2 EK segment to allow fast loading.
      !>
      !> Legacy interface: calls zz routine zzekif02 with the legacy context.
      subroutine zzekif02 ( handle, segno )
      use pepper_kinds, only: ip
      use pepper_inc_ekclas07
      use pepper_inc_ekclas08
      use pepper_inc_ekclas09
      use pepper_inc_ekcoldsc
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekif02 ( ctx, handle, segno )
      end subroutine zzekif02

      !> Read a column entry from a specified record in a class 1 column.
      !> Class 1 columns contain scalar integer values.
      !>
      !> Legacy interface: calls zz routine zzekrd01 with the legacy context.
      subroutine zzekrd01 (  handle,  segdsc,  coldsc, &
                             recptr,  ival,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: ival  !! Integer value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrd01 ( ctx, handle, (segdsc), (coldsc), recptr, ival, isnull )
      end subroutine zzekrd01

      !> Read a column entry from a specified record in a class 2 column.
      !> Class 2 columns contain scalar double precision values.
      !>
      !> Legacy interface: calls zz routine zzekrd02 with the legacy context.
      subroutine zzekrd02 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      real(wp) :: dval  !! Double precision value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekrd02 ( ctx, handle, segdsc, coldsc, recptr, dval, isnull )
      end subroutine zzekrd02

      !> Read a column entry from a specified record in a class 3 column.
      !> Class 3 columns contain scalar character values.
      !>
      !> Legacy interface: calls zz routine zzekrd03 with the legacy context.
      subroutine zzekrd03 (  handle,  segdsc,  coldsc, &
                             recptr,  cvlen,   cval,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: cvlen  !! Length of returned character value.
      character(*) :: cval  !! Character value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrd03 ( ctx, handle, (segdsc), (coldsc), recptr, cvlen, cval, isnull )
      end subroutine zzekrd03

      !> Read a specified element range from a column entry in a specified
      !> record in a class 4 column.  Class 4 columns have integer arrays
      !> as column entries.
      !>
      !> Legacy interface: calls zz routine zzekrd04 with the legacy context.
      subroutine zzekrd04 ( handle,  segdsc,  coldsc,  recptr, &
                            beg,     end,     ivals,   isnull,  found )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: beg  !! Start element index.
      integer(ip) :: end  !! End element index.
      integer(ip) :: ivals  ( * )  !! Integer values in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      logical :: found  !! Flag indicating whether elements were found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrd04 ( ctx, handle, (segdsc), (coldsc), recptr, beg, end, ivals, isnull, &
         found )
      end subroutine zzekrd04

      !> Read a specified element range from a column entry in a specified
      !> record in a class 5 column.  Class 5 columns have d.p. arrays
      !> as column entries.
      !>
      !> Legacy interface: calls zz routine zzekrd05 with the legacy context.
      subroutine zzekrd05 ( handle,  segdsc,  coldsc,  recptr, &
                            beg,     end,     dvals,   isnull,  found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: beg  !! Start element index.
      integer(ip) :: end  !! End element index.
      real(wp) :: dvals  ( * )  !! Double precision values in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      logical :: found  !! Flag indicating whether elements were found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekrd05 ( ctx, handle, segdsc, coldsc, recptr, beg, end, dvals, isnull, found )
      end subroutine zzekrd05

      !> Read a specified element range from a column entry in a specified
      !> record in a class 6 column.  Class 6 columns have character arrays
      !> as column entries.
      !>
      !> Legacy interface: calls zz routine zzekrd06 with the legacy context.
      subroutine zzekrd06 ( handle,  segdsc,  coldsc,  recptr, &
                            beg,     end,     cvals,   isnull,  found )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: beg  !! Start element index.
      integer(ip) :: end  !! End element index.
      character(*) :: cvals  ( * )  !! Character values in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      logical :: found  !! Flag indicating whether elements were found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekrd06 ( ctx, handle, segdsc, coldsc, recptr, beg, end, cvals, isnull, found )
      end subroutine zzekrd06

      !> Read a column entry from a specified record in a class 7 column.
      !> Class 7 columns contain fixed-count, scalar integer values.
      !>
      !> Legacy interface: calls zz routine zzekrd07 with the legacy context.
      subroutine zzekrd07 (  handle,  segdsc,  coldsc, &
                             recptr,  ival,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekclas07
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: ival  !! Integer value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrd07 ( ctx, handle, (segdsc), (coldsc), recptr, ival, isnull )
      end subroutine zzekrd07

      !> Read a column entry from a specified record in a class 8 column.
      !> Class 8 columns contain fixed-count, scalar, double precision
      !> values.
      !>
      !> Legacy interface: calls zz routine zzekrd08 with the legacy context.
      subroutine zzekrd08 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekclas08
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      real(wp) :: dval  !! Double precision value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekrd08 ( ctx, handle, segdsc, coldsc, recptr, dval, isnull )
      end subroutine zzekrd08

      !> Set the data pointer for a specified EK column entry.
      !>
      !> Legacy interface: calls zz routine zzekscdp with the legacy context.
      subroutine zzekscdp ( handle, segdsc, coldsc, recptr, datptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ekrecptr
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: datptr  !! Data pointer of column entry.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekscdp ( ctx, handle, segdsc, coldsc, recptr, datptr )
      end subroutine zzekscdp

      !> Write a character array out to a contiguous set of EK pages.
      !>
      !> Legacy interface: calls zz routine zzekwpac with the legacy context.
      subroutine zzekwpac ( handle, segdsc, nvals, l, cvals, p, base )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( * )  !! Descriptor of segment that owns the array.
      integer(ip) :: nvals  !! Number of values to write.
      integer(ip) :: l  !! String length.
      character(*) :: cvals  ( * )  !! Character values.
      integer(ip) :: p  !! Number of first page containing array.
      integer(ip) :: base  !! Base address of first page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekwpac ( ctx, handle, segdsc, nvals, l, cvals, p, base )
      end subroutine zzekwpac

      !> Write an integer array out to a contiguous set of EK pages.
      !>
      !> Legacy interface: calls zz routine zzekwpai with the legacy context.
      subroutine zzekwpai ( handle, segdsc, nvals, ivals, p, base )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( * )  !! Descriptor of segment that owns the array.
      integer(ip) :: nvals  !! Number of values to write.
      integer(ip) :: ivals  ( * )  !! Integer values.
      integer(ip) :: p  !! Number of first page containing array.
      integer(ip) :: base  !! Base address of first page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekwpai ( ctx, handle, segdsc, nvals, ivals, p, base )
      end subroutine zzekwpai

      !> Write a logical array out to a contiguous set of EK pages.
      !>
      !> Legacy interface: calls zz routine zzekwpal with the legacy context.
      subroutine zzekwpal ( handle, segdsc, nvals, lvals, p, base )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( * )  !! Descriptor of segment that owns the array.
      integer(ip) :: nvals  !! Number of values to write.
      logical :: lvals  ( * )  !! Logical values.
      integer(ip) :: p  !! Number of first page containing array.
      integer(ip) :: base  !! Base address of first page.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekwpal ( ctx, handle, segdsc, nvals, lvals, p, base )
      end subroutine zzekwpal

      !> Append a new, empty record at the end of a specified E-kernel
      !> segment.
      !>
      !> Legacy interface: calls ctx%ekappr with the legacy context.
      subroutine ekappr ( handle, segno, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: recno  !! Number of appended record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekappr ( handle, segno, recno )
      end subroutine ekappr

      !> Initialize a new E-kernel segment to allow fast writing.
      !>
      !> Legacy interface: calls ctx%ekifld with the legacy context.
      subroutine ekifld (  handle,  tabnam,  ncols,  nrows, &
                           cnames,  decls,   segno,  rcptrs  )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      character(*) :: tabnam  !! Table name.
      integer(ip) :: ncols  !! Number of columns in the segment.
      integer(ip) :: nrows  !! Number of rows in the segment.
      character(*) :: cnames ( * )  !! Names of columns.
      character(*) :: decls  ( * )  !! Declarations of columns.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: rcptrs ( * )  !! Array of record pointers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekifld ( handle, tabnam, ncols, nrows, cnames, decls, segno, rcptrs )
      end subroutine ekifld

      !> Read data from a character column in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekrcec with the legacy context.
      subroutine ekrcec ( handle,  segno,  recno,  column, &
                          nvals,   cvals,  isnull          )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record from which data is to be read.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values in column entry.
      character(*) :: cvals  ( * )  !! Character values in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: column is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%ekrcec ( handle, segno, recno, (column), nvals, cvals, isnull )
      end subroutine ekrcec

      !> Read data from a double precision column in a specified EK
      !> record.
      !>
      !> Legacy interface: calls ctx%ekrced with the legacy context.
      subroutine ekrced ( handle,  segno,  recno,  column, &
                          nvals,   dvals,  isnull          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record from which data is to be read.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values in column entry.
      real(wp) :: dvals  ( * )  !! D.p. values in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekrced ( handle, segno, recno, column, nvals, dvals, isnull )
      end subroutine ekrced

      !> Read data from an integer column in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekrcei with the legacy context.
      subroutine ekrcei ( handle,  segno,  recno,  column, &
                          nvals,   ivals,  isnull          )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record from which data is to be read.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values in column entry.
      integer(ip) :: ivals  ( * )  !! Integer values in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekrcei ( handle, segno, recno, column, nvals, ivals, isnull )
      end subroutine ekrcei

      !> Add an entire class 7 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac07 with the legacy context.
      subroutine zzekac07 (  handle,  segdsc,  coldsc, &
                             ivals,   nlflgs,  wkindx  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekclas07
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac07 ( ctx, handle, segdsc, coldsc, ivals, nlflgs, wkindx )
      end subroutine zzekac07

      !> Add an entire class 8 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac08 with the legacy context.
      subroutine zzekac08 (  handle,  segdsc,  coldsc, &
                             dvals,   nlflgs,  wkindx  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekclas08
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      real(wp) :: dvals  ( * )  !! D.p. values to add to column.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac08 ( ctx, handle, segdsc, coldsc, dvals, nlflgs, wkindx )
      end subroutine zzekac08

      !> Add an entire class 9 column to an EK segment.
      !>
      !> Legacy interface: calls zz routine zzekac09 with the legacy context.
      subroutine zzekac09 (  handle,  segdsc,  coldsc, &
                             cvals,   nlflgs,  wkindx  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekclas09
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to new EK file.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      character(*) :: cvals  ( * )  !! Character values to add to column.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekac09 ( ctx, handle, segdsc, coldsc, cvals, nlflgs, wkindx )
      end subroutine zzekac09

      !> Read scalar data from a character column in a specified EK
      !> record.
      !>
      !> Legacy interface: calls zz routine zzekrsc with the legacy context.
      subroutine zzekrsc (  handle,  segdsc,  coldsc,  recptr, &
                            eltidx,  cvlen,   cval,    isnull, found  )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Pointer to record from which data is to be read.
      integer(ip) :: eltidx  !! Index of column entry element to be read.
      integer(ip) :: cvlen  !! Length of stored string.
      character(*) :: cval  !! Character value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      logical :: found  !! Flag indicting whether entry element was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrsc ( ctx, handle, (segdsc), (coldsc), recptr, eltidx, cvlen, cval, isnull, &
         found )
      end subroutine zzekrsc

      !> Read scalar data from a double precision column in a specified EK
      !> record.
      !>
      !> Legacy interface: calls zz routine zzekrsd with the legacy context.
      subroutine zzekrsd (  handle,  segdsc,  coldsc,  recptr, &
                            eltidx,  dval,    isnull,  found  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Pointer to record from which data is to be read.
      integer(ip) :: eltidx  !! Index of column entry element to be read.
      real(wp) :: dval  !! D.p. value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      logical :: found  !! Flag indicting whether entry element was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekrsd ( ctx, handle, segdsc, coldsc, recptr, eltidx, dval, isnull, found )
      end subroutine zzekrsd

      !> Read scalar data from an integer column in a specified EK record.
      !>
      !> Legacy interface: calls zz routine zzekrsi with the legacy context.
      subroutine zzekrsi (  handle,  segdsc,  coldsc,  recptr, &
                            eltidx,  ival,    isnull,  found  )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Pointer to record from which data is to be read.
      integer(ip) :: eltidx  !! Index of column entry element to be read.
      integer(ip) :: ival  !! Integer value in column entry.
      logical :: isnull  !! Flag indicating whether column entry is null.
      logical :: found  !! Flag indicting whether entry element was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekrsi ( ctx, handle, (segdsc), (coldsc), recptr, eltidx, ival, isnull, found )
      end subroutine zzekrsi

      !> Update a specified class 4 column entry in an EK record.
      !>
      !> Legacy interface: calls zz routine zzekue04 with the legacy context.
      subroutine zzekue04 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   ivals,  isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: nvals  !! Number of values.
      integer(ip) :: ivals ( * )  !! Integer values.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekue04 ( ctx, handle, segdsc, (coldsc), recptr, nvals, ivals, isnull )
      end subroutine zzekue04

      !> Update a specified class 5 column entry in an EK record.
      !>
      !> Legacy interface: calls zz routine zzekue05 with the legacy context.
      subroutine zzekue05 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   dvals,  isnull  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: nvals  !! Number of values.
      real(wp) :: dvals ( * )  !! Double precision values.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekue05 ( ctx, handle, segdsc, (coldsc), recptr, nvals, dvals, isnull )
      end subroutine zzekue05

      !> Update a specified class 6 column entry in an EK record.
      !>
      !> Legacy interface: calls zz routine zzekue06 with the legacy context.
      subroutine zzekue06 (  handle,  segdsc,  coldsc, &
                             recptr,  nvals,   cvals,  isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: nvals  !! Number of values.
      character(*) :: cvals ( * )  !! Character string values.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekue06 ( ctx, handle, segdsc, (coldsc), recptr, nvals, cvals, isnull )
      end subroutine zzekue06

      !> Add an entire character column to an EK segment.
      !>
      !> Legacy interface: calls ctx%ekaclc with the legacy context.
      subroutine ekaclc (  handle,  segno,   column,  cvals, &
                           entszs,  nlflgs,  rcptrs,  wkindx  )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segno  !! Number of segment to add column to.
      character(*) :: column  !! Column name.
      character(*) :: cvals  ( * )  !! Character values to add to column.
      integer(ip) :: entszs ( * )  !! Array of sizes of column entries.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: rcptrs ( * )  !! Record pointers for segment.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekaclc ( handle, segno, column, cvals, entszs, nlflgs, rcptrs, wkindx )
      end subroutine ekaclc

      !> Add an entire double precision column to an EK segment.
      !>
      !> Legacy interface: calls ctx%ekacld with the legacy context.
      subroutine ekacld (  handle,  segno,   column,  dvals, &
                           entszs,  nlflgs,  rcptrs,  wkindx  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segno  !! Number of segment to add column to.
      character(*) :: column  !! Column name.
      real(wp) :: dvals  ( * )  !! Double precision values to add to column.
      integer(ip) :: entszs ( * )  !! Array of sizes of column entries.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: rcptrs ( * )  !! Record pointers for segment.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekacld ( handle, segno, column, dvals, entszs, nlflgs, rcptrs, wkindx )
      end subroutine ekacld

      !> Add an entire integer column to an EK segment.
      !>
      !> Legacy interface: calls ctx%ekacli with the legacy context.
      subroutine ekacli (  handle,  segno,   column,  ivals, &
                           entszs,  nlflgs,  rcptrs,  wkindx  )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segno  !! Number of segment to add column to.
      character(*) :: column  !! Column name.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      integer(ip) :: entszs ( * )  !! Array of sizes of column entries.
      logical :: nlflgs ( * )  !! Array of null flags for column entries.
      integer(ip) :: rcptrs ( * )  !! Record pointers for segment.
      integer(ip) :: wkindx ( * )  !! Work space for column index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekacli ( handle, segno, column, ivals, entszs, nlflgs, rcptrs, wkindx )
      end subroutine ekacli

      !> Compare two column entry elements, and return the relation of the
      !> first to the second:  LT, EQ, or GT.
      !>
      !> Legacy interface: calls zz routine zzekecmp with the legacy context.
      integer(ip) function zzekecmp ( hans,  sgdscs,  cldscs,  rows,  elts )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: hans   ( * )  !! EK handles.
      integer(ip) :: sgdscs ( sdscsz, * )  !! Segment descriptors.
      integer(ip) :: cldscs ( cdscsz, * )  !! Column descriptors.
      integer(ip) :: rows   ( * )  !! Row numbers.
      integer(ip) :: elts   ( * )  !! Element indices.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekecmp = pepper_zzekecmp ( ctx, hans, sgdscs, cldscs, rows, elts )
      end function zzekecmp

      !> Compare a specified scalar EK column entry with a scalar value.
      !>
      !> Legacy interface: calls zz routine zzekscmp with the legacy context.
      logical function zzekscmp ( op, &
                                  handle, segdsc, coldsc, row,   eltidx, &
                                  dtype,  cval,   dval,   ival,  null  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      use pepper_inc_ekwild
      implicit none
      integer(ip) :: op  !! Relational operator code.
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: row  !! ID of row containing column entry to compare.
      integer(ip) :: eltidx  !! Index of element in array-valued column entry.
      integer(ip) :: dtype  !! Data type of input value.
      character(*) :: cval  !! Character string to compare with column entry.
      real(wp) :: dval  !! D.p. value to compare with column entry.
      integer(ip) :: ival  !! Integer value to compare with column entry.
      logical :: null  !! Flag indicating whether scalar is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekscmp = pepper_zzekscmp ( ctx, op, handle, segdsc, coldsc, row, eltidx, dtype, cval, dval, ival, null )
      end function zzekscmp

      !> Find the last column value less than or equal to a specified key,
      !> for a specified character EK column having a type 1 index, using
      !> dictionary ordering on character data values and record pointers.
      !>
      !> Legacy interface: calls zz routine zzekerc1 with the legacy context.
      subroutine zzekerc1 (  handle,  segdsc,  coldsc,  ckey, &
                             recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*) :: ckey  !! Character key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      integer(ip) :: prvidx  !! Ordinal position of predecessor of CKEY.
      integer(ip) :: prvptr  !! Record pointer for predecessor of CKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekerc1 ( ctx, handle, (segdsc), (coldsc), ckey, recptr, null, prvidx, prvptr )
      end subroutine zzekerc1

      !> Find the last column value less than or equal to a specified key,
      !> for a specified d.p. EK column having a type 1 index, using
      !> dictionary ordering on d.p. data values and record pointers.
      !>
      !> Legacy interface: calls zz routine zzekerd1 with the legacy context.
      subroutine zzekerd1 (  handle,  segdsc,  coldsc,  dkey, &
                             recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      real(wp) :: dkey  !! Double precision key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      integer(ip) :: prvidx  !! Ordinal position of predecessor of DKEY.
      integer(ip) :: prvptr  !! Pointer to record containing predecessor of DKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekerd1 ( ctx, handle, (segdsc), (coldsc), dkey, recptr, null, prvidx, prvptr )
      end subroutine zzekerd1

      !> Find the last column value less than or equal to a specified key,
      !> for a specified integer EK column having a type 1 index, using
      !> dictionary ordering on integer data values and record pointers.
      !>
      !> Legacy interface: calls zz routine zzekeri1 with the legacy context.
      subroutine zzekeri1 (  handle,  segdsc,  coldsc,  ikey, &
                             recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: ikey  !! Integer key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      integer(ip) :: prvidx  !! Ordinal position of predecessor of IKEY.
      integer(ip) :: prvptr  !! Pointer to record containing predecessor of IKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekeri1 ( ctx, handle, (segdsc), (coldsc), ikey, recptr, null, prvidx, prvptr )
      end subroutine zzekeri1

      !> Find the last column value less than or equal to a specified key,
      !> for a specified, indexed character EK column.
      !>
      !> Legacy interface: calls zz routine zzekllec with the legacy context.
      subroutine zzekllec (  handle,  segdsc,  coldsc, &
                             ckey,    prvloc,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*) :: ckey  !! Character key.
      integer(ip) :: prvloc  !! Ordinal position of predecessor of CKEY.
      integer(ip) :: prvptr  !! Record pointer for predecessor of CKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekllec ( ctx, handle, (segdsc), (coldsc), ckey, prvloc, prvptr )
      end subroutine zzekllec

      !> Find the last column value less than or equal to a specified key,
      !> for a specified, indexed double precision EK column.
      !>
      !> Legacy interface: calls zz routine zzeklled with the legacy context.
      subroutine zzeklled (  handle,  segdsc,  coldsc, &
                             dkey,    prvloc,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      real(wp) :: dkey  !! Double precision key.
      integer(ip) :: prvloc  !! Ordinal position of predecessor of DKEY.
      integer(ip) :: prvptr  !! Record pointer for predecessor of DKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeklled ( ctx, handle, (segdsc), (coldsc), dkey, prvloc, prvptr )
      end subroutine zzeklled

      !> Find the last column value less than or equal to a specified key,
      !> for a specified, indexed integer EK column.
      !>
      !> Legacy interface: calls zz routine zzekllei with the legacy context.
      subroutine zzekllei (  handle,  segdsc,  coldsc, &
                             ikey,    prvloc,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: ikey  !! Integer key.
      integer(ip) :: prvloc  !! Ordinal position of predecessor of IKEY.
      integer(ip) :: prvptr  !! Pointer to a record containing predecessor of IKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekllei ( ctx, handle, (segdsc), (coldsc), ikey, prvloc, prvptr )
      end subroutine zzekllei

      !> Find the last column value less than a specified key,
      !> for a specified, indexed character EK column.
      !>
      !> Legacy interface: calls zz routine zzeklltc with the legacy context.
      subroutine zzeklltc (  handle,  segdsc,  coldsc, &
                             ckey,    prvloc,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*) :: ckey  !! Character key.
      integer(ip) :: prvloc  !! Ordinal position of predecessor of CKEY.
      integer(ip) :: prvptr  !! Pointer to record containing predecessor of CKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeklltc ( ctx, handle, (segdsc), (coldsc), ckey, prvloc, prvptr )
      end subroutine zzeklltc

      !> Find the last column value less than a specified key,
      !> for a specified, indexed double precision EK column.
      !>
      !> Legacy interface: calls zz routine zzeklltd with the legacy context.
      subroutine zzeklltd (  handle,  segdsc,  coldsc, &
                             dkey,    prvloc,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      real(wp) :: dkey  !! Double precision key.
      integer(ip) :: prvloc  !! Ordinal position of predecessor of DKEY.
      integer(ip) :: prvptr  !! Pointer to a record containing predecessor of DKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeklltd ( ctx, handle, (segdsc), (coldsc), dkey, prvloc, prvptr )
      end subroutine zzeklltd

      !> Find the last column value less than a specified key,
      !> for a specified, indexed integer EK column.
      !>
      !> Legacy interface: calls zz routine zzekllti with the legacy context.
      subroutine zzekllti (  handle,  segdsc,  coldsc, &
                             ikey,    prvloc,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: ikey  !! Integer key.
      integer(ip) :: prvloc  !! Ordinal position of predecessor of IKEY.
      integer(ip) :: prvptr  !! Pointer to record containing predecessor of IKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekllti ( ctx, handle, (segdsc), (coldsc), ikey, prvloc, prvptr )
      end subroutine zzekllti

      !> Compare two EK rows, using as the order relation dictionary
      !> ordering on a specified list of columns.
      !>
      !> Legacy interface: calls zz routine zzekrcmp with the legacy context.
      logical function zzekrcmp ( op, &
                                  ncols, &
                                  han1, sgdsc1, cdlst1, row1, elts1, &
                                  han2, sgdsc2, cdlst2, row2, elts2  )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: op  !! Code for relational operator.
      integer(ip) :: ncols  !! Number of columns used to define order relation.
      integer(ip) :: han1  !! Handle of EK containing first row to compare.
      integer(ip) :: sgdsc1 ( sdscsz )  !! Descriptor of segment containing first row.
      integer(ip) :: cdlst1 ( cdscsz, * )  !! List of column descriptors for first row.
      integer(ip) :: row1  !! Number of first row (relative to segment).
      integer(ip) :: elts1  ( * )  !! List of element indices for first row.
      integer(ip) :: han2  !! Handle of EK containing second row to compare.
      integer(ip) :: sgdsc2 ( sdscsz )  !! Descriptor of segment containing second row.
      integer(ip) :: cdlst2 ( cdscsz, * )  !! List of column descriptors for second row.
      integer(ip) :: row2  !! Number of second row (relative to segment).
      integer(ip) :: elts2  ( * )  !! List of element indices for second row.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekrcmp = pepper_zzekrcmp ( ctx, op, ncols, han1, sgdsc1, cdlst1, row1, elts1, han2, sgdsc2, cdlst2, row2, elts2 )
      end function zzekrcmp

      !> Determine whether a specified row in an EK file satisfies
      !> a specified set of constraints.
      !>
      !> Legacy interface: calls zz routine zzekrmch with the legacy context.
      logical function zzekrmch ( ncnstr, &
                                  active, &
                                  handle, &
                                  segdsc, &
                                  cdscrs, &
                                  row, &
                                  elts, &
                                  ops, &
                                  vtypes, &
                                  chrbuf, &
                                  cbegs, &
                                  cends, &
                                  dvals, &
                                  ivals  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ektype
      use pepper_inc_ekopcd
      use pepper_inc_ekwild
      use pepper_inc_ekqlimit
      integer(ip) :: ncnstr  !! Number of constraints present in query.
      logical :: active ( * )  !! Array of flags indicating applicable constraints.
      integer(ip) :: handle  !! Handle of EK file containing row.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: cdscrs ( cdscsz, * )  !! Descriptors of columns referenced in query.
      integer(ip) :: row  !! Index of row to match.
      integer(ip) :: elts   ( * )  !! Indices of column entry elements to match.
      integer(ip) :: ops    ( * )  !! Operators used in query constraints.
      integer(ip) :: vtypes ( * )  !! Data types of values on RHS of constraints.
      character(*) :: chrbuf  !! Buffer containing query tokens.
      integer(ip) :: cbegs  ( * )  !! Begin indices of character query tokens.
      integer(ip) :: cends  ( * )  !! End indices of character query tokens.
      real(wp) :: dvals  ( * )  !! D.p. values used in query constraints.
      integer(ip) :: ivals  ( * )  !! Integer values used in query constraints.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekrmch = pepper_zzekrmch ( ctx, ncnstr, active, handle, segdsc, cdscrs, row, elts, ops, vtypes, chrbuf, cbegs, cends, dvals, ivals )
      end function zzekrmch

      !> Compare two row vectors, using dictionary ordering on a
      !> specified list of columns as the order relation.
      !>
      !> Legacy interface: calls zz routine zzekvcmp with the legacy context.
      logical function zzekvcmp ( op,      ncols,   tabs,    cols, &
                                  elts,    senses,  sthan,   stsdsc, &
                                  stdtpt,  dtpool,  dtdscs,  sgvec1, &
                                  rwvec1,  sgvec2,  rwvec2           )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: op  !! Code for relational operator.
      integer(ip) :: ncols  !! Number of columns used to define order relation.
      integer(ip) :: tabs   ( * )  !! Indices of tables containing order-by columns.
      integer(ip) :: cols   ( * )  !! Indices of order-by columns within parent tables.
      integer(ip) :: elts   ( * )  !! Element indices.
      integer(ip) :: senses ( * )  !! Order senses.
      integer(ip) :: sthan  ( * )  !! Array of handles of loaded EKs.
      integer(ip) :: stsdsc ( sdscsz, * )  !! Array of segment descriptors.
      integer(ip) :: stdtpt ( * )  !! Array of pointers to column descriptors.
      integer(ip) :: dtpool ( 2, lbpool : * )  !! Column descriptor index pool.
      integer(ip) :: dtdscs ( cdscsz, * )  !! Array of column descriptors.
      integer(ip) :: sgvec1 ( * )  !! First segment vector.
      integer(ip) :: rwvec1 ( * )  !! First row vector.
      integer(ip) :: sgvec2 ( * )  !! Second segment vector.
      integer(ip) :: rwvec2 ( * )  !! Second row vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekvcmp = pepper_zzekvcmp ( ctx, op, ncols, tabs, cols, elts, senses, sthan, stsdsc, stdtpt, dtpool, dtdscs, sgvec1, rwvec1, sgvec2, rwvec2 )
      end function zzekvcmp

      !> Determine whether a vector of constraints involving comparisons of
      !> specified EK column elements is satisfied.
      !>
      !> Legacy interface: calls zz routine zzekvmch with the legacy context.
      logical function zzekvmch ( ncnstr, active, &
                                  lhans,  lsdscs, lcdscs, lrows, lelts, &
                                  ops, &
                                  rhans,  rsdscs, rcdscs, rrows, relts )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekopcd
      use pepper_inc_ektype
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ekwild
      use pepper_inc_ekqlimit
      integer(ip) :: ncnstr  !! Number of join constraints.
      logical :: active ( * )  !! Array of flags indicating applicable constraints.
      integer(ip) :: lhans  ( * )  !! Handles of EKs for columns on LHS's of constraints.
      integer(ip) :: lsdscs ( sdscsz, * )  !! Descriptors of segments on LHS's of constraints.
      integer(ip) :: lcdscs ( cdscsz, * )  !! Column descriptors for LHS's of constraints.
      integer(ip) :: lrows  ( * )  !! Row numbers for LHS's of constraints.
      integer(ip) :: lelts  ( * )  !! Column element indices for LHS's of constraints.
      integer(ip) :: ops    ( * )  !! Code for relational operator in constraints.
      integer(ip) :: rhans  ( * )  !! Handles of EKs for columns on RHS's of constraints.
      integer(ip) :: rsdscs ( sdscsz, * )  !! Descriptors of segments on RHS's of constraints.
      integer(ip) :: rcdscs ( cdscsz, * )  !! Column descriptors for RHS's of constraints.
      integer(ip) :: rrows  ( * )  !! Row numbers for RHS's of constraints.
      integer(ip) :: relts  ( * )  !! Column element indices for RHS's of constraints.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekvmch = pepper_zzekvmch ( ctx, ncnstr, active, lhans, lsdscs, lcdscs, lrows, lelts, ops, rhans, rsdscs, rcdscs, rrows, relts )
      end function zzekvmch

      !> Find the ordinal position of the row, in an specified EK segment,
      !> whose value in a specified column is the last last element less
      !> than or equal to a specified value, where the order relation is
      !> given by an order vector in a specified DAS file.
      !>
      !> Legacy interface: calls zz routine zzekille with the legacy context.
      integer(ip) function zzekille (  handle,  segdsc,  coldsc,  nrows, &
                                   dtype,   cval,    dval,    ival    )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! HANDLE of EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Descriptor of column to be searched.
      integer(ip) :: nrows  !! Number of rows in column.
      integer(ip) :: dtype  !! Data type of input value.
      character(*) :: cval  !! Character string value.
      real(wp) :: dval  !! Double precision value.
      integer(ip) :: ival  !! Integer value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekille = pepper_zzekille ( ctx, handle, segdsc, coldsc, nrows, dtype, cval, dval, ival )
      end function zzekille

      !> Find the ordinal position of the row, in an specified EK segment,
      !> whose value in a specified column is the last last element less
      !> than a specified value, where the order relation is given by an
      !> order vector in a specified DAS file.
      !>
      !> Legacy interface: calls zz routine zzekillt with the legacy context.
      integer(ip) function zzekillt (  handle,  segdsc,  coldsc,  nrows, &
                                   dtype,   cval,    dval,    ival  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! HANDLE of EK file.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Descriptor of column to be searched.
      integer(ip) :: nrows  !! Number of rows in column.
      integer(ip) :: dtype  !! Data type of input value.
      character(*) :: cval  !! Character string value.
      real(wp) :: dval  !! Double precision value.
      integer(ip) :: ival  !! Integer value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzekillt = pepper_zzekillt ( ctx, handle, segdsc, coldsc, nrows, dtype, cval, dval, ival )
      end function zzekillt

      !> Sort the row vectors of a join row set union, given an order
      !> relation defined by a set of qualified order-by columns.
      !>
      !> Legacy interface: calls zz routine zzekjsrt with the legacy context.
      subroutine zzekjsrt ( njrs, ubases, norder, otabs, ocols, oelts, senses, sthan, stsdsc, stdtpt, dtpool, dtdscs, ordbas )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: njrs  !! Number of join row sets in union.
      integer(ip) :: ubases ( * )  !! Base addresses of join row sets of union.
      integer(ip) :: norder  !! Number of order-by columns.
      integer(ip) :: otabs  ( * )  !! Order-by table indices relative to FROM clause.
      integer(ip) :: ocols  ( * )  !! Order-by column indices.
      integer(ip) :: oelts  ( * )  !! Order-by element indices.
      integer(ip) :: senses ( * )  !! Order directions.
      integer(ip) :: sthan  ( * )  !! Handles of loaded files from segment table.
      integer(ip) :: stsdsc ( sdscsz, * )  !! Array of descriptors of loaded segments.
      integer(ip) :: stdtpt ( * )  !! Array of pointers to column descriptors.
      integer(ip) :: dtpool ( 2,      lbpool : * )  !! Column descriptor table pool.
      integer(ip) :: dtdscs ( cdscsz,          * )  !! Column descriptor table.
      integer(ip) :: ordbas  !! Scratch area base address for order vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekjsrt ( ctx, njrs, ubases, norder, otabs, ocols, oelts, senses, sthan, stsdsc, stdtpt, dtpool, dtdscs, ordbas )
      end subroutine zzekjsrt

      !> Find the last column value less than or equal to a specified key,
      !> for a specified, indexed character EK column, using dictionary
      !> ordering on character data values and record pointers.
      !>
      !> Legacy interface: calls zz routine zzeklerc with the legacy context.
      subroutine zzeklerc  (  handle,  segdsc,  coldsc,  ckey, &
                              recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*) :: ckey  !! Double precision key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      integer(ip) :: prvidx  !! Ordinal position of predecessor of CKEY.
      integer(ip) :: prvptr  !! Record pointer for predecessor of CKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeklerc ( ctx, handle, (segdsc), (coldsc), ckey, recptr, null, prvidx, prvptr )
      end subroutine zzeklerc

      !> Find the last column value less than or equal to a specified key,
      !> for a specified, indexed d.p. EK column, using dictionary
      !> ordering on d.p. data values and record pointers.
      !>
      !> Legacy interface: calls zz routine zzeklerd with the legacy context.
      subroutine zzeklerd (  handle,  segdsc,  coldsc,  dkey, &
                             recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      real(wp) :: dkey  !! Double precision key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      integer(ip) :: prvidx  !! Ordinal position of predecessor of DKEY.
      integer(ip) :: prvptr  !! Pointer to record containing predecessor of DKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeklerd ( ctx, handle, (segdsc), (coldsc), dkey, recptr, null, prvidx, prvptr )
      end subroutine zzeklerd

      !> Find the last column value less than or equal to a specified key,
      !> for a specified, indexed integer EK column, using dictionary
      !> ordering on integer data values and record pointers.
      !>
      !> Legacy interface: calls zz routine zzekleri with the legacy context.
      subroutine zzekleri (  handle,  segdsc,  coldsc,  ikey, &
                             recptr,  null,    prvidx,  prvptr  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: ikey  !! Integer key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      integer(ip) :: prvidx  !! Ordinal position of predecessor of IKEY.
      integer(ip) :: prvptr  !! Pointer to record containing predecessor of IKEY.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekleri ( ctx, handle, (segdsc), (coldsc), ikey, recptr, null, prvidx, prvptr )
      end subroutine zzekleri

      !> Find the ordinal position of a specified record in a specified,
      !> indexed EK column.
      !>
      !> Legacy interface: calls zz routine zzekfrx with the legacy context.
      subroutine zzekfrx ( handle, segdsc, coldsc, recptr, pos )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_ekqlimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Pointer to record to locate.
      integer(ip) :: pos  !! Ordinal position of record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc, coldsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekfrx ( ctx, handle, (segdsc), (coldsc), recptr, pos )
      end subroutine zzekfrx

      !> Insert into a type 1 EK index a record pointer associated with a
      !> character key.  The key and record pointer determine the insertion
      !> point via dictionary ordering on (value, record pointer) pairs.
      !>
      !> Legacy interface: calls zz routine zzekiic1 with the legacy context.
      subroutine zzekiic1 ( handle, segdsc, coldsc, ckey, recptr, null )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*) :: ckey  !! Character key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekiic1 ( ctx, handle, segdsc, coldsc, ckey, recptr, null )
      end subroutine zzekiic1

      !> Insert into a type 1 EK index a record pointer associated with a
      !> d.p. key.  The key and record pointer determine the insertion
      !> point via dictionary ordering on (value, record pointer) pairs.
      !>
      !> Legacy interface: calls zz routine zzekiid1 with the legacy context.
      subroutine zzekiid1 ( handle, segdsc, coldsc, dkey, recptr, null )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      real(wp) :: dkey  !! Double precision key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekiid1 ( ctx, handle, segdsc, coldsc, dkey, recptr, null )
      end subroutine zzekiid1

      !> Insert into a type 1 EK index a record pointer associated with an
      !> integer key.  The key and record pointer determine the insertion
      !> point via dictionary ordering on (value, record pointer) pairs.
      !>
      !> Legacy interface: calls zz routine zzekiii1 with the legacy context.
      subroutine zzekiii1 ( handle, segdsc, coldsc, ikey, recptr, null )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: ikey  !! Integer key.
      integer(ip) :: recptr  !! Record pointer.
      logical :: null  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekiii1 ( ctx, handle, segdsc, coldsc, ikey, recptr, null )
      end subroutine zzekiii1

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekjprp with the legacy context.
      subroutine zzekjprp ( segvec, jbase1, nt1, rb1, nr1, jbase2, nt2, rb2, nr2, njcnst, active, cpidx1, clidx1, elts1, ops, cpidx2, clidx2, elts2, sthan, stsdsc, stdtpt, dtpool, dtdscs )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      integer(ip) :: segvec ( * )  !! Composite segment vector for joined table.
      integer(ip) :: jbase1  !! Scratch area base address of first join row set.
      integer(ip) :: nt1  !! Width of first table.
      integer(ip) :: rb1  !! Row vector base address from first join row set.
      integer(ip) :: nr1  !! Number of row vectors from first join row set.
      integer(ip) :: jbase2  !! Scratch area base address of second join row set.
      integer(ip) :: nt2  !! Width of second table.
      integer(ip) :: rb2  !! Row vector base address from second join row set.
      integer(ip) :: nr2  !! Number of row vectors from second join row set.
      integer(ip) :: njcnst  !! Number of join constraints.
      logical :: active ( * )  !! Array of flags indicating applicable constraints.
      integer(ip) :: cpidx1 ( * )  !! Cross product indices for LHS's of constraints.
      integer(ip) :: clidx1 ( * )  !! Column indices for LHS's of constraints.
      integer(ip) :: elts1  ( * )  !! Column element indices for LHS's of constraints.
      integer(ip) :: ops    ( * )  !! Operator codes for constraints.
      integer(ip) :: cpidx2 ( * )  !! Cross product indices for RHS's of constraints.
      integer(ip) :: clidx2 ( * )  !! Column indices for RHS's of constraints.
      integer(ip) :: elts2  ( * )  !! Column element indices for RHS's of constraints.
      integer(ip) :: sthan  ( * )  !! Array of EK handles corresponding to segments.
      integer(ip) :: stsdsc ( sdscsz,          * )  !! Array of segment descriptors.
      integer(ip) :: stdtpt ( * )  !! Array of set table column descriptor pointers.
      integer(ip) :: dtpool ( 2,      lbpool : * )  !! Linked list pool for column descriptors.
      integer(ip) :: dtdscs ( cdscsz,          * )  !! Array of column descriptors.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekjprp ( ctx, segvec, jbase1, nt1, rb1, nr1, jbase2, nt2, rb2, nr2, njcnst, active, cpidx1, clidx1, elts1, ops, cpidx2, clidx2, elts2, sthan, stsdsc, stdtpt, dtpool, dtdscs )
      end subroutine zzekjprp

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekjnxt with the legacy context.
      subroutine zzekjnxt ( found, rowvec )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      logical :: found  !! Flag indicating whether a row vector was found.
      integer(ip) :: rowvec ( * )  !! Row vector matching join constraints.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekjnxt ( ctx, found, rowvec )
      end subroutine zzekjnxt

      !> Determine the key column to use when searching an EK segment
      !> for rows matching query constraints.
      !>
      !> Legacy interface: calls zz routine zzekkey with the legacy context.
      subroutine zzekkey ( handle,  segdsc,  nrows,   ncnstr,  clidxs, &
                           dsclst,  ops,     dtypes,  chrbuf,  cbegs, &
                           cends,   dvals,   ivals,   active,  key, &
                           keydsc,  begidx,  endidx,  found          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! Handle of EK file containing segment.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: nrows  !! Number of rows in segment.
      integer(ip) :: ncnstr  !! Number of relational constraints in query.
      integer(ip) :: clidxs ( * )  !! Column attribute table indices for columns.
      integer(ip) :: dsclst ( cdscsz, * )  !! Array of column descriptors for constraints.
      integer(ip) :: ops    ( * )  !! Operations used in query constraints.
      integer(ip) :: dtypes ( * )  !! Data types of scalar values used in constraints.
      character(*) :: chrbuf  !! Buffer containing query tokens.
      integer(ip) :: cbegs  ( * )  !! Begin indices of character query tokens.
      integer(ip) :: cends  ( * )  !! End indices of character query tokens.
      real(wp) :: dvals  ( * )  !! D.p. values used in query constraints.
      integer(ip) :: ivals  ( * )  !! Integer values used in query constraints.
      logical :: active ( * )  !! Array of flags indicating applicable constraints.
      integer(ip) :: key  !! Index of key column.
      integer(ip) :: keydsc ( * )  !! Descriptor of key column.
      integer(ip) :: begidx  !! Begin index of candidate row set.
      integer(ip) :: endidx  !! End index of candidate row set.
      logical :: found  !! Flag indicating whether a key column was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: segdsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekkey ( ctx, handle, (segdsc), nrows, ncnstr, clidxs, dsclst, ops, dtypes, &
         chrbuf, cbegs, cends, dvals, ivals, active, key, keydsc, begidx, endidx, found )
      end subroutine zzekkey

      !> Add a column entry to a class 1 column in a specified EK record.
      !> Class 1 columns contain scalar, integer values.
      !>
      !> Legacy interface: calls zz routine zzekad01 with the legacy context.
      subroutine zzekad01 (  handle,  segdsc,  coldsc, &
                             recptr,  ival,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: ival  !! Integer value.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekad01 ( ctx, handle, segdsc, coldsc, recptr, ival, isnull )
      end subroutine zzekad01

      !> Add a column entry to a specified record in a class 2 column.
      !> Class 2 columns contain scalar double precision values.
      !>
      !> Legacy interface: calls zz routine zzekad02 with the legacy context.
      subroutine zzekad02 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      real(wp) :: dval  !! Double precision value.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekad02 ( ctx, handle, segdsc, coldsc, recptr, dval, isnull )
      end subroutine zzekad02

      !> Add a column entry to a class 3 column in a specified EK record.
      !> Class 3 columns contain scalar, character values.
      !>
      !> Legacy interface: calls zz routine zzekad03 with the legacy context.
      subroutine zzekad03 (  handle,  segdsc,  coldsc, &
                             recptr,  cval,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( * )  !! Segment descriptor.
      integer(ip) :: coldsc ( * )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      character(*) :: cval  !! Character string value.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekad03 ( ctx, handle, segdsc, coldsc, recptr, cval, isnull )
      end subroutine zzekad03

      !> Update an EK column index to reflect deletion of a record
      !> specified by a record pointer.
      !>
      !> Legacy interface: calls zz routine zzekixdl with the legacy context.
      subroutine zzekixdl ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer to locate.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekixdl ( ctx, handle, segdsc, coldsc, recptr )
      end subroutine zzekixdl

      !> Perform join of two EK join row sets, subject to a specified set
      !> of EK join constraints, yielding an EK join row set.
      !>
      !> Legacy interface: calls zz routine zzekjoin with the legacy context.
      subroutine zzekjoin ( jbase1,  jbase2,  njcnst,  active, &
                            cpidx1,  clidx1,  elts1,   ops, &
                            cpidx2,  clidx2,  elts2,   sthan, &
                            stsdsc,  stdtpt,  dtpool,  dtdscs, &
                            jbase3,  nrows                             )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekqlimit
      use pepper_inc_eksegdsc
      integer(ip) :: jbase1  !! Scratch area base address of first join row set.
      integer(ip) :: jbase2  !! Scratch area base address of second join row set.
      integer(ip) :: njcnst  !! Number of join constraints.
      logical :: active ( * )  !! Array of flags indicating applicable constraints.
      integer(ip) :: cpidx1 ( * )  !! Cross product indices for LHS's of constraints.
      integer(ip) :: clidx1 ( * )  !! Column indices for LHS's of constraints.
      integer(ip) :: elts1  ( * )  !! Column entry elt. indices for LHS'of constraints.
      integer(ip) :: ops    ( * )  !! Operator codes for constraints.
      integer(ip) :: cpidx2 ( * )  !! Cross product indices for RHS's of constraints.
      integer(ip) :: clidx2 ( * )  !! Column indices for RHS's of constraints.
      integer(ip) :: elts2  ( * )  !! Column entry elt. indices for RHS'of constraints.
      integer(ip) :: sthan  ( * )  !! Array of EK handles corresponding to segments.
      integer(ip) :: stsdsc ( sdscsz, * )  !! Array of segment descriptors.
      integer(ip) :: stdtpt ( * )  !! Array of set table column descriptor pointers.
      integer(ip) :: dtpool ( 2,      lbpool : * )  !! Linked list pool for column descriptors.
      integer(ip) :: dtdscs ( cdscsz,          * )  !! Array of column descriptors.
      integer(ip) :: jbase3  !! Scratch area base address of output join row set.
      integer(ip) :: nrows  !! Number of rows in output join row set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzekjoin ( ctx, jbase1, jbase2, njcnst, active, cpidx1, clidx1, elts1, ops, cpidx2, clidx2, elts2, sthan, stsdsc, stdtpt, dtpool, dtdscs, jbase3, nrows )
      end subroutine zzekjoin

      !> Add data to a character column in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekacec with the legacy context.
      subroutine ekacec (  handle,  segno,  recno,  column, &
                           nvals,   cvals,  isnull          )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record to which data is to be added.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values to add to column.
      character(*) :: cvals  ( * )  !! Character values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekacec ( handle, segno, recno, column, nvals, cvals, isnull )
      end subroutine ekacec

      !> Add data to an double precision column in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekaced with the legacy context.
      subroutine ekaced (  handle,  segno,  recno,  column, &
                           nvals,   dvals,  isnull          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record to which data is to be added.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values to add to column.
      real(wp) :: dvals  ( * )  !! Double precision values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekaced ( handle, segno, recno, column, nvals, dvals, isnull )
      end subroutine ekaced

      !> Add data to an integer column in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekacei with the legacy context.
      subroutine ekacei (  handle,  segno,  recno,  column, &
                           nvals,   ivals,  isnull          )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! EK file handle.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record to which data is to be added.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values to add to column.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekacei ( handle, segno, recno, column, nvals, ivals, isnull )
      end subroutine ekacei

      !> Load an EK file, making it accessible to the EK readers.
      !>
      !> Legacy interface: calls spice%eklef of the legacy instance.
      subroutine eklef ( fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      character(*) :: fname  !! Name of EK file to load.
      integer(ip) :: handle  !! File handle of loaded EK file.
      type(pepper_instance), pointer :: spice
      spice => legacy_spice()
      call spice%eklef ( fname, handle )
      end subroutine eklef

      !> Unload an EK file, making its contents inaccessible to the
      !> EK reader routines, and clearing space in order to allow other
      !> EK files to be loaded.
      !>
      !> Legacy interface: calls spice%ekuef of the legacy instance.
      subroutine ekuef ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle of EK file.
      type(pepper_instance), pointer :: spice
      spice => legacy_spice()
      call spice%ekuef ( handle )
      end subroutine ekuef

      !> Return the number of loaded EK tables.
      !>
      !> Legacy interface: calls ctx%ekntab with the legacy context.
      subroutine ekntab ( n )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: n  !! Number of loaded tables.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekntab ( n )
      end subroutine ekntab

      !> Return the name of a specified, loaded table.
      !>
      !> Legacy interface: calls ctx%ektnam with the legacy context.
      subroutine ektnam ( n, table )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: n  !! Index of table.
      character(*) :: table  !! Name of table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ektnam ( n, table )
      end subroutine ektnam

      !> Return the number of distinct columns in a specified, currently
      !> loaded table.
      !>
      !> Legacy interface: calls ctx%ekccnt with the legacy context.
      subroutine ekccnt ( table, ccount )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      character(*) :: table  !! Name of table.
      integer(ip) :: ccount  !! Count of distinct, currently loaded columns.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekccnt ( table, ccount )
      end subroutine ekccnt

      !> Return attribute information about a column belonging to a loaded
      !> EK table, specifying the column by table and index.
      !>
      !> Legacy interface: calls ctx%ekcii with the legacy context.
      subroutine ekcii ( table, cindex, column, attdsc )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: cindex  !! Index of column whose attributes are to be found.
      character(*) :: column  !! Name of column.
      character(*) :: table  !! Name of table containing column.
      integer(ip) :: attdsc ( adscsz )  !! Column attribute descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: table is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%ekcii ( (table), cindex, column, attdsc )
      end subroutine ekcii

      !> Search for EK events matching a specified set of constraints.
      !>
      !> Legacy interface: calls ctx%eksrch with the legacy context.
      subroutine eksrch ( eqryi, eqryc, eqryd, nmrows, semerr, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      character(*) :: eqryc  !! Character component of encoded query.
      real(wp) :: eqryd  ( * )  !! D.p. component of encoded query.
      integer(ip) :: eqryi  ( lbcell : * )  !! Integer component of encoded query.
      integer(ip) :: nmrows  !! Number of rows matching query constraints.
      logical :: semerr  !! Flag indicating whether semantic error occurred.
      character(*) :: errmsg  !! Message describing semantic error, if any.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: eqryc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%eksrch ( eqryi, (eqryc), eqryd, nmrows, semerr, errmsg )
      end subroutine eksrch

      !> Return the number of elements in a specified column entry in
      !> the current row.
      !>
      !> Legacy interface: calls ctx%eknelt with the legacy context.
      subroutine eknelt ( selidx, row, nelt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: row  !! Row containing element.
      integer(ip) :: selidx  !! Index of parent column in SELECT clause.
      integer(ip) :: nelt  !! Number of elements in entry in current row.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%eknelt ( selidx, row, nelt )
      end subroutine eknelt

      !> Return an element of an entry in a column of character
      !> type in a specified row.
      !>
      !> Legacy interface: calls ctx%ekgc with the legacy context.
      subroutine ekgc ( selidx, row, elment, cdata, null, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: elment  !! Index of element, within column entry, to fetch.
      integer(ip) :: row  !! Row to fetch from.
      integer(ip) :: selidx  !! Index of parent column in SELECT clause.
      logical :: found  !! Flag indicating whether column was present in row.
      character(*) :: cdata  !! Character string element of column entry.
      logical :: null  !! Flag indicating whether column entry was null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekgc ( selidx, row, elment, cdata, null, found )
      end subroutine ekgc

      !> Return an element of an entry in a column of double precision
      !> or `time' type in a specified row.
      !>
      !> Legacy interface: calls ctx%ekgd with the legacy context.
      subroutine ekgd ( selidx, row, elment, ddata, null, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: elment  !! Index of element, within column entry, to fetch.
      integer(ip) :: row  !! Row to fetch from.
      integer(ip) :: selidx  !! Index of parent column in SELECT clause.
      logical :: found  !! Flag indicating whether column was present in row.
      real(wp) :: ddata  !! D.p. element of column entry.
      logical :: null  !! Flag indicating whether column entry was null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekgd ( selidx, row, elment, ddata, null, found )
      end subroutine ekgd

      !> Return an element of an entry in a column of integer
      !> type in a specified row.
      !>
      !> Legacy interface: calls ctx%ekgi with the legacy context.
      subroutine ekgi ( selidx, row, elment, idata, null, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekjrs
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      integer(ip) :: elment  !! Index of element, within column entry, to fetch.
      integer(ip) :: row  !! Row to fetch from.
      integer(ip) :: selidx  !! Index of parent column in SELECT clause.
      logical :: found  !! Flag indicating whether column was present in row.
      integer(ip) :: idata  !! Integer element of column entry.
      logical :: null  !! Flag indicating whether column entry was null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekgi ( selidx, row, elment, idata, null, found )
      end subroutine ekgi

      !> Delete a specified class 1 column entry from an EK record.
      !>
      !> Legacy interface: calls zz routine zzekde01 with the legacy context.
      subroutine zzekde01 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekde01 ( ctx, handle, segdsc, (coldsc), recptr )
      end subroutine zzekde01

      !> Delete a specified class 2 column entry from an EK record.
      !>
      !> Legacy interface: calls zz routine zzekde02 with the legacy context.
      subroutine zzekde02 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekde02 ( ctx, handle, segdsc, (coldsc), recptr )
      end subroutine zzekde02

      !> Delete a specified class 3 column entry from an EK record.
      !>
      !> Legacy interface: calls zz routine zzekde03 with the legacy context.
      subroutine zzekde03 ( handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
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
      call pepper_zzekde03 ( ctx, handle, segdsc, (coldsc), recptr )
      end subroutine zzekde03

      !> Update a specified class 1 column entry in an EK record.
      !>
      !> Legacy interface: calls zz routine zzekue01 with the legacy context.
      subroutine zzekue01 (  handle,  segdsc,  coldsc, &
                             recptr,  ival,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      integer(ip) :: ival  !! Integer value.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekue01 ( ctx, handle, segdsc, (coldsc), recptr, ival, isnull )
      end subroutine zzekue01

      !> Update a specified class 2 column entry in an EK record.
      !>
      !> Legacy interface: calls zz routine zzekue02 with the legacy context.
      subroutine zzekue02 (  handle,  segdsc,  coldsc, &
                             recptr,  dval,    isnull  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      real(wp) :: dval  !! Double precision value.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekue02 ( ctx, handle, segdsc, (coldsc), recptr, dval, isnull )
      end subroutine zzekue02

      !> Delete a specified record from a specified E-kernel segment.
      !>
      !> Legacy interface: calls ctx%ekdelr with the legacy context.
      subroutine ekdelr ( handle, segno, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segno  !! Segment number.
      integer(ip) :: recno  !! Record number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekdelr ( handle, segno, recno )
      end subroutine ekdelr

      !> Update a double precision column entry in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekuced with the legacy context.
      subroutine ekuced (  handle,  segno,  recno,  column, &
                           nvals,   dvals,  isnull          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record in which entry is to be updated.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values in new column entry.
      real(wp) :: dvals  ( * )  !! Double precision values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekuced ( handle, segno, recno, column, nvals, dvals, isnull )
      end subroutine ekuced

      !> Update an integer column entry in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekucei with the legacy context.
      subroutine ekucei (  handle,  segno,  recno,  column, &
                           nvals,   ivals,  isnull          )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record in which entry is to be updated.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values in new column entry.
      integer(ip) :: ivals  ( * )  !! Integer values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekucei ( handle, segno, recno, column, nvals, ivals, isnull )
      end subroutine ekucei

      !> Get data for a single star from a SPICE type 1 star catalog.
      !>
      !> Legacy interface: calls ctx%stcg01 with the legacy context.
      subroutine stcg01 ( index,  ra,     dec,    rasig, &
                          decsig, catnum, sptype, vmag )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: index  !! Star index.
      real(wp) :: ra  !! Right ascension in radians.
      real(wp) :: dec  !! Declination in radians.
      real(wp) :: rasig  !! Right ascension uncertainty in radians.
      real(wp) :: decsig  !! Declination uncertainty in radians.
      integer(ip) :: catnum  !! Catalog number.
      character(*) :: sptype  !! Spectral type.
      real(wp) :: vmag  !! Visual magnitude.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%stcg01 ( index, ra, dec, rasig, decsig, catnum, sptype, vmag )
      end subroutine stcg01

      !> Load SPICE type 1 star catalog and return the catalog's
      !> table name.
      !>
      !> Legacy interface: calls ctx%stcl01 with the legacy context.
      subroutine stcl01 ( catfnm, tabnam, handle )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: catfnm  !! Catalog file name.
      character(*) :: tabnam  !! Catalog table name.
      integer(ip) :: handle  !! Catalog file handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: catfnm is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%stcl01 ( (catfnm), tabnam, handle )
      end subroutine stcl01

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekcchk with the legacy context.
      subroutine zzekcchk (  query,  eqryi,  eqryc,  ntab,    tablst, &
                             alslst, base,   error,  errmsg,  errptr  )
      use pepper_kinds, only: ip
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      character(*) :: query  !! Original query string.
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      integer(ip) :: ntab  !! Number of tables in FROM clause.
      character(*) :: tablst ( * )  !! List of table names.
      character(*) :: alslst ( * )  !! List of table aliases.
      integer(ip) :: base  !! Base address of table/column descriptor pair.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Error message.
      integer(ip) :: errptr  !! Position in query where error was detected.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekcchk ( ctx, (query), eqryi, eqryc, ntab, tablst, alslst, base, error, errmsg, &
         errptr )
      end subroutine zzekcchk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzekpcol with the legacy context.
      subroutine zzekpcol ( qcol,    eqryi,   eqryc,   table,  alias, &
                            tabidx,  column,  colidx,  error,  errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektnamsz
      use pepper_inc_ektokn
      character(*) :: qcol  !! Column name, possibly qualified.
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      character(*) :: table  !! Name of table qualifying column.
      character(*) :: alias  !! Alias of table, if present.
      integer(ip) :: tabidx  !! Index of TABLE in FROM clause, if known.
      character(*) :: column  !! Name of QCOL, unqualified.
      integer(ip) :: colidx  !! Index of QCOL within its parent virtual table.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Parse error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: qcol, eqryc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekpcol ( ctx, (qcol), eqryi, (eqryc), table, alias, tabidx, column, colidx, &
         error, errmsg )
      end subroutine zzekpcol

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzeksemc with the legacy context.
      subroutine zzeksemc ( query, eqryi, eqryc, error, errmsg, errptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekopcd
      use pepper_inc_ekquery
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      character(*) :: query  !! Original query.
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Semantic error message.
      integer(ip) :: errptr  !! Position in query where error was detected.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeksemc ( ctx, (query), eqryi, eqryc, error, errmsg, errptr )
      end subroutine zzeksemc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzektres with the legacy context.
      subroutine zzektres ( query,  eqryi,  eqryc,   eqryd, &
                                    error,  errmsg,  errptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      character(*) :: query  !! Original query string.
      integer(ip) :: eqryi  ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      real(wp) :: eqryd  ( * )  !! Numeric component of query.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Error message.
      integer(ip) :: errptr  !! Position in query where error was detected.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzektres ( ctx, (query), eqryi, eqryc, eqryd, error, errmsg, errptr )
      end subroutine zzektres

      !> Update a specified class 3 column entry in an EK record.
      !>
      !> Legacy interface: calls zz routine zzekue03 with the legacy context.
      subroutine zzekue03 (  handle,  segdsc,  coldsc, &
                             recptr,  cval,    isnull  )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      integer(ip) :: handle  !! File handle.
      integer(ip) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: recptr  !! Record pointer.
      character(*) :: cval  !! Character string value.
      logical :: isnull  !! Null flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: coldsc is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekue03 ( ctx, handle, segdsc, (coldsc), recptr, cval, isnull )
      end subroutine zzekue03

      !> Update a character column entry in a specified EK record.
      !>
      !> Legacy interface: calls ctx%ekucec with the legacy context.
      subroutine ekucec (  handle,  segno,  recno,  column, &
                           nvals,   cvals,  isnull          )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      integer(ip) :: handle  !! Handle attached to EK file.
      integer(ip) :: segno  !! Index of segment containing record.
      integer(ip) :: recno  !! Record in which entry is to be updated.
      character(*) :: column  !! Column name.
      integer(ip) :: nvals  !! Number of values in new column entry.
      character(*) :: cvals  ( * )  !! Character string values to add to column.
      logical :: isnull  !! Flag indicating whether column entry is null.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ekucec ( handle, segno, recno, column, nvals, cvals, isnull )
      end subroutine ekucec

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzeknres with the legacy context.
      subroutine zzeknres ( query, eqryi, eqryc, error, errmsg, errptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektnamsz
      character(*) :: query  !! Original query string.
      integer(ip) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*) :: eqryc  !! Character component of query.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Error message.
      integer(ip) :: errptr  !! Position in query where error was detected.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzeknres ( ctx, (query), eqryi, eqryc, error, errmsg, errptr )
      end subroutine zzeknres

      !> Find E-kernel data that satisfy a set of constraints.
      !>
      !> Legacy interface: calls ctx%ekfind with the legacy context.
      subroutine ekfind ( query, nmrows, error, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ekqlimit
      use pepper_inc_ektype
      use pepper_inc_ekopcd
      use pepper_inc_ekcnamsz
      implicit none
      character(*) :: query  !! Query specifying data to be found.
      integer(ip) :: nmrows  !! Number of matching rows.
      logical :: error  !! Flag indicating whether query parsed correctly.
      character(*) :: errmsg  !! Parse error description.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%ekfind ( (query), nmrows, error, errmsg )
      end subroutine ekfind

      !> Convert an EK query to encoded form.
      !>
      !> Legacy interface: calls zz routine zzekencd with the legacy context.
      subroutine zzekencd (  query, &
                             eqryi,  eqryc,   eqryd, &
                             error,  errmsg,  errptr  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektype
      character(*) :: query  !! Query specifying events to be found.
      integer(ip) :: eqryi  ( lbcell : * )  !! Integer component of encoded query.
      character(*) :: eqryc  !! Character component of encoded query.
      real(wp) :: eqryd  ( * )  !! Numeric component of encoded query.
      logical :: error  !! Flag indicating whether query parsed correctly.
      character(*) :: errmsg  !! Parse error description.
      integer(ip) :: errptr  !! Error pointer.
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
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzekencd ( ctx, (query), qi, eqryc, eqryd, error, errmsg, errptr )
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
      end subroutine zzekencd

      !> Parse the SELECT clause of an EK query, returning full particulars
      !> concerning each selected item.
      !>
      !> Legacy interface: calls ctx%ekpsel with the legacy context.
      subroutine ekpsel (  query,   n,     xbegs,  xends,  xtypes, &
                           xclass,  tabs,  cols,   error,  errmsg  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekattdsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      implicit none
      character(*) :: query  !! EK query.
      integer(ip) :: n  !! Number of items in SELECT clause of QUERY.
      integer(ip) :: xbegs  ( * )  !! Begin positions of expressions in SELECT clause.
      integer(ip) :: xends  ( * )  !! End positions of expressions in SELECT clause.
      character(*) :: xtypes ( * )  !! Data types of expressions.
      character(*) :: xclass ( * )  !! Classes of expressions.
      character(*) :: tabs   ( * )  !! Names of tables qualifying SELECT columns.
      character(*) :: cols   ( * )  !! Names of columns in SELECT clause of QUERY.
      logical :: error  !! Error flag.
      character(*) :: errmsg  !! Parse error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: query is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%ekpsel ( (query), n, xbegs, xends, xtypes, xclass, tabs, cols, error, errmsg )
      end subroutine ekpsel

      !> Search through a type 1 star catalog and return the number of
      !> stars within a specified RA - DEC rectangle.
      !>
      !> Legacy interface: calls ctx%stcf01 with the legacy context.
      subroutine stcf01 ( catnam, westra, eastra, sthdec, nthdec, &
                          nstars)
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: catnam  !! Catalog table name.
      real(wp) :: westra  !! Western most right ascension in radians.
      real(wp) :: eastra  !! Eastern most right ascension in radians.
      real(wp) :: sthdec  !! Southern most declination in radians.
      real(wp) :: nthdec  !! Northern most declination in radians.
      integer(ip) :: nstars  !! Number of stars found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%stcf01 ( catnam, westra, eastra, sthdec, nthdec, nstars )
      end subroutine stcf01

end module pepper_legacy_ek
