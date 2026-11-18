!> The legacy interface's routines of the subsystem daf (69 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_daf

   use pepper_instance_module, only: pepper_context, pepper_zzdafgfr => zzdafgfr, &
      pepper_zzdafnfr => zzdafnfr, pepper_zzdafgdr => zzdafgdr, pepper_zzdafgsr => zzdafgsr, &
      pepper_zzxlated => zzxlated
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: rdenci, hx2dp, rdencd, txtopn, txtopr, wrenci, int2hx, dp2hx, wrencd, zzdafgfr, &
      zzdafnfr, zzdafgdr, zzdafgsr, dafopr, dafopw, dafonw, dafopn, dafcls, dafhsf, dafhlu, &
      dafluh, dafhfn, daffnh, dafhof, dafsih, dafrwa, dafarw, dafps, dafus, dafgdr, dafgsr, &
      dafrdr, dafwdr, dafnrr, dafgda, zzxlated, dafrda, dafrfr, dafwfr, dafwda, dafarr, dafrcr, &
      dafwcr, dafra, dafrrr, dafac, dafdc, dafec, dafbna, dafada, dafena, dafcad, dafbfs, daffna, &
      dafbbs, daffpa, dafgs, dafgn, dafgh, dafrs, dafrn, dafws, dafcs, dafbt, dafa2b, dafb2a, &
      dafb2t, daft2b, daftb

contains

      !> Read N encoded integers from a text file, decoding them into
      !> their equivalent integers.
      !>
      !> Legacy interface: calls ctx%rdenci with the legacy context.
      subroutine rdenci ( unit, n, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Fortran unit number of input text file.
      integer(ip) :: n  !! Number of integers to read and decode.
      integer(ip) :: data(*)  !! List of decoded integers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdenci ( unit, n, data )
      end subroutine rdenci

      !> Convert a string representing a double precision number in a
      !> base 16 "scientific notation" into its equivalent double
      !> precision number.
      !>
      !> Legacy interface: calls ctx%hx2dp with the legacy context.
      subroutine hx2dp ( string, number, error, errmsg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! Hex form string to convert to double precision.
      real(wp) :: number  !! Double precision value to be returned.
      logical :: error  !! A logical flag which is .TRUE. on error.
      character(*) :: errmsg  !! A descriptive error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: string is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%hx2dp ( (string), number, error, errmsg )
      end subroutine hx2dp

      !> Read N encoded d.p. numbers from a text file, decoding them
      !> into their equivalent d.p. numbers.
      !>
      !> Legacy interface: calls ctx%rdencd with the legacy context.
      subroutine rdencd ( unit, n, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! Fortran unit number of input text file.
      integer(ip) :: n  !! Number of d.p. numbers to read and decode.
      real(wp) :: data(*)  !! List of decoded d.p. numbers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdencd ( unit, n, data )
      end subroutine rdencd

      !> Open a new text file for subsequent write access.
      !>
      !> Legacy interface: calls ctx%txtopn with the legacy context.
      subroutine txtopn ( fname, unit )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! Name of file.
      integer(ip) :: unit  !! Logical unit.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%txtopn ( fname, unit )
      end subroutine txtopn

      !> Open a text file for read access.
      !>
      !> Legacy interface: calls ctx%txtopr with the legacy context.
      subroutine txtopr ( fname, unit )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: fname  !! Name of file.
      integer(ip) :: unit  !! Logical unit.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%txtopr ( fname, unit )
      end subroutine txtopr

      !> Encode and write integers to a text file.
      !>
      !> Legacy interface: calls ctx%wrenci with the legacy context.
      subroutine wrenci ( unit, n, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Fortran unit number of output text file.
      integer(ip) :: n  !! Number of integers to encode and write.
      integer(ip) :: data(*)  !! List of integers to be encoded and written.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wrenci ( unit, n, data )
      end subroutine wrenci

      !> Convert an integer to an equivalent signed hexadecimal string.
      !>
      !> Legacy interface: calls ctx%int2hx with the legacy context.
      subroutine int2hx ( number, string, length )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Integer to be converted.
      character(*) :: string  !! Equivalent hexadecimal string, left justified.
      integer(ip) :: length  !! The length of the hexadecimal string produced.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%int2hx ( number, string, length )
      end subroutine int2hx

      !> Convert a double precision number to an equivalent character
      !> string using a base 16 "scientific notation."
      !>
      !> Legacy interface: calls ctx%dp2hx with the legacy context.
      subroutine dp2hx ( number, hxstr, hxssiz )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: number  !! D.p. number to be converted.
      character(*) :: hxstr  !! Equivalent character string, left justified.
      integer(ip) :: hxssiz  !! Length of the character string produced.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dp2hx ( number, hxstr, hxssiz )
      end subroutine dp2hx

      !> Encode and write d.p. numbers to a text file.
      !>
      !> Legacy interface: calls ctx%wrencd with the legacy context.
      subroutine wrencd ( unit, n, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! Fortran unit number of output text file.
      integer(ip) :: n  !! Number of d.p. numbers to encode and write.
      real(wp) :: data(*)  !! List of d.p. numbers to encode and write.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wrencd ( unit, n, data )
      end subroutine wrencd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdafgfr with the legacy context.
      subroutine zzdafgfr ( handle, idword, nd, ni, ifname, fward, bward, free, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of the DAF.
      character(*) :: idword  !! DAF ID Word that indicates file type.
      integer(ip) :: nd  !! Number of double precision components in summaries.
      integer(ip) :: ni  !! Number of integer components in summaries.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: fward  !! Forward list pointer.
      integer(ip) :: bward  !! Backward list pointer.
      integer(ip) :: free  !! Free address pointer.
      logical :: found  !! Logical indicating whether the record was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdafgfr ( ctx, handle, idword, nd, ni, ifname, fward, bward, free, found )
      end subroutine zzdafgfr

      !> Write the file record to a new DAF file.
      !>
      !> Legacy interface: calls zz routine zzdafnfr with the legacy context.
      subroutine zzdafnfr ( lun, idword, nd, ni, ifname, fward, bward, free, format )
      use pepper_kinds, only: ip
      use pepper_inc_zzftprms
      implicit none
      integer(ip) :: lun  !! Logical unit number of an open DAF file.
      character(*) :: idword  !! DAF ID word.
      integer(ip) :: nd  !! Number of double precision components in a summary.
      integer(ip) :: ni  !! Number of integer components in a summary.
      character(*) :: ifname  !! Internal filename.
      integer(ip) :: fward  !! First descriptor record.
      integer(ip) :: bward  !! Last descriptor record.
      integer(ip) :: free  !! First free address.
      character(*) :: format  !! File binary format identifier string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdafnfr ( ctx, lun, idword, nd, ni, ifname, fward, bward, free, format )
      end subroutine zzdafnfr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdafgdr with the legacy context.
      subroutine zzdafgdr ( handle, recno, dprec, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of the DAF.
      integer(ip) :: recno  !! Record number.
      real(wp) :: dprec  ( * )  !! Contents of the record.
      logical :: found  !! Logical indicating whether the record was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdafgdr ( ctx, handle, recno, dprec, found )
      end subroutine zzdafgdr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdafgsr with the legacy context.
      subroutine zzdafgsr ( handle, recno, nd, ni, dprec, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of the DAF.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: nd  !! Number of double precision components in a summary.
      integer(ip) :: ni  !! Number of integer components in a summary.
      real(wp) :: dprec  ( * )  !! Contents of the record.
      logical :: found  !! Logical indicating whether the record was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdafgsr ( ctx, handle, recno, nd, ni, dprec, found )
      end subroutine zzdafgsr

      !> Open a DAF for subsequent read requests.
      !>
      !> Legacy interface: calls ctx%dafopr with the legacy context.
      subroutine dafopr ( fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of DAF to be opened.
      integer(ip) :: handle  !! Handle assigned to DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafopr ( fname, handle )
      end subroutine dafopr

      !> Open a DAF for subsequent write requests.
      !>
      !> Legacy interface: calls ctx%dafopw with the legacy context.
      subroutine dafopw ( fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of DAF to be opened.
      integer(ip) :: handle  !! Handle assigned to DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafopw ( fname, handle )
      end subroutine dafopw

      !> Open a new DAF for subsequent write requests.
      !>
      !> Legacy interface: calls ctx%dafonw with the legacy context.
      subroutine dafonw ( fname, ftype, nd, ni, ifname, resv, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of DAF to be opened.
      character(*) :: ftype  !! Mnemonic code for type of data in the DAF file.
      integer(ip) :: nd  !! Number of double precision components in summaries.
      integer(ip) :: ni  !! Number of integer components in summaries.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: resv  !! Number of records to reserve.
      integer(ip) :: handle  !! Handle assigned to DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafonw ( fname, ftype, nd, ni, ifname, resv, handle )
      end subroutine dafonw

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine DAFONW. NAIF supports this routine only to provide
      !> backward compatibility.
      !>
      !> Legacy interface: calls ctx%dafopn with the legacy context.
      subroutine dafopn ( fname, nd, ni, ifname, resv, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of DAF to be opened.
      integer(ip) :: nd  !! Number of double precision components in summaries.
      integer(ip) :: ni  !! Number of integer components in summaries.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: resv  !! Number of records to reserve.
      integer(ip) :: handle  !! Handle assigned to DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafopn ( fname, nd, ni, ifname, resv, handle )
      end subroutine dafopn

      !> Close the DAF associated with a given handle.
      !>
      !> Legacy interface: calls ctx%dafcls with the legacy context.
      subroutine dafcls ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of DAF to be closed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafcls ( handle )
      end subroutine dafcls

      !> Return the summary format associated with a handle.
      !>
      !> Legacy interface: calls ctx%dafhsf with the legacy context.
      subroutine dafhsf ( handle, nd, ni )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: nd  !! Number of double precision components in summaries.
      integer(ip) :: ni  !! Number of integer components in summaries.
      integer(ip) :: handle  !! Handle of a DAF file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafhsf ( handle, nd, ni )
      end subroutine dafhsf

      !> Return the logical unit associated with a handle.
      !>
      !> Legacy interface: calls ctx%dafhlu with the legacy context.
      subroutine dafhlu ( handle, unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file.
      integer(ip) :: unit  !! Corresponding logical unit.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafhlu ( handle, unit )
      end subroutine dafhlu

      !> Return the handle associated with a logical unit.
      !>
      !> Legacy interface: calls ctx%dafluh with the legacy context.
      subroutine dafluh ( unit, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Corresponding DAF file handle.
      integer(ip) :: unit  !! Logical unit connected to a DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafluh ( unit, handle )
      end subroutine dafluh

      !> Return the name of the file associated with a handle.
      !>
      !> Legacy interface: calls ctx%dafhfn with the legacy context.
      subroutine dafhfn ( handle, fname )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Corresponding file name.
      integer(ip) :: handle  !! Handle of a DAF file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafhfn ( handle, fname )
      end subroutine dafhfn

      !> Return handle associated with a file name.
      !>
      !> Legacy interface: calls ctx%daffnh with the legacy context.
      subroutine daffnh ( fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of a DAF file.
      integer(ip) :: handle  !! Corresponding DAF file handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daffnh ( fname, handle )
      end subroutine daffnh

      !> Return a SPICE set containing the handles of all currently
      !> open DAFS.
      !>
      !> Legacy interface: calls ctx%dafhof with the legacy context.
      subroutine dafhof ( fhset )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: fhset ( lbcell : * )  !! A set containing handles of currently open DAFS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafhof ( fhset )
      end subroutine dafhof

      !> Signal an error if a DAF file handle does not designate a DAF
      !> that is open for a specified type of access.
      !>
      !> Legacy interface: calls ctx%dafsih with the legacy context.
      subroutine dafsih ( handle, access )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! HANDLE to be validated.
      character(*) :: access  !! String indicating access type.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafsih ( handle, access )
      end subroutine dafsih

      !> Convert a record/word pair to its equivalent address within
      !> a DAF.
      !>
      !> Legacy interface: calls ctx%dafrwa with the legacy context.
      subroutine dafrwa ( recno, wordno, addr )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: recno  !! Record, word numbers of a location within DAF.
      integer(ip) :: wordno  !! Record, word numbers of a location within DAF.
      integer(ip) :: addr  !! Corresponding address.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrwa ( recno, wordno, addr )
      end subroutine dafrwa

      !> Convert an address within a DAF to its equivalent
      !> record/word representation.
      !>
      !> Legacy interface: calls ctx%dafarw with the legacy context.
      subroutine dafarw ( addr, recno, wordno )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: recno  !! Corresponding record, word numbers.
      integer(ip) :: wordno  !! Corresponding record, word numbers.
      integer(ip) :: addr  !! Address within DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafarw ( addr, recno, wordno )
      end subroutine dafarw

      !> Pack (assemble) an array summary from its double precision and
      !> integer components.
      !>
      !> Legacy interface: calls ctx%dafps with the legacy context.
      subroutine dafps ( nd, ni, dc, ic, sum )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nd  !! Number of double precision components.
      integer(ip) :: ni  !! Number of integer components.
      real(wp) :: dc       ( * )  !! Double precision components.
      integer(ip) :: ic       ( * )  !! Integer components.
      real(wp) :: sum      ( * )  !! Array summary.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafps ( nd, ni, dc, ic, sum )
      end subroutine dafps

      !> Unpack an array summary into its double precision and integer
      !> components.
      !>
      !> Legacy interface: calls ctx%dafus with the legacy context.
      subroutine dafus ( sum, nd, ni, dc, ic )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nd  !! Number of double precision components.
      integer(ip) :: ni  !! Number of integer components.
      real(wp) :: dc       ( * )  !! Double precision components.
      integer(ip) :: ic       ( * )  !! Integer components.
      real(wp) :: sum      ( * )  !! Array summary.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafus ( sum, nd, ni, dc, ic )
      end subroutine dafus

      !> Read a portion of the contents of a double precision record in a
      !> DAF file.
      !>
      !> Legacy interface: calls ctx%dafgdr with the legacy context.
      subroutine dafgdr ( handle, recno, begin, end, data, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: begin  !! First word to read from record.
      integer(ip) :: end  !! Last word to read from record.
      real(wp) :: data     (  *  )  !! Contents of record.
      logical :: found  !! .TRUE. if record is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafgdr ( handle, recno, begin, end, data, found )
      end subroutine dafgdr

      !> Read a portion of the contents of a summary record in a DAF file.
      !>
      !> Legacy interface: calls ctx%dafgsr with the legacy context.
      subroutine dafgsr ( handle, recno, begin, end, data, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: begin  !! First word to read from record.
      integer(ip) :: end  !! Last word to read from record.
      real(wp) :: data     (  *  )  !! Contents of record.
      logical :: found  !! .TRUE. if record is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafgsr ( handle, recno, begin, end, data, found )
      end subroutine dafgsr

      !> Read a portion of the contents of a double precision record in a
      !> DAF file.
      !> **Obsolete:** This routine has been superseded by DAFGDR, and it is
      !> supported for purposes of backwards compatibility only.
      !>
      !> Legacy interface: calls ctx%dafrdr with the legacy context.
      subroutine dafrdr ( handle, recno, begin, end, data, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: begin  !! First word to read from record.
      integer(ip) :: end  !! Last word to read from record.
      real(wp) :: data     (  *  )  !! Contents of record.
      logical :: found  !! .TRUE. if record is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrdr ( handle, recno, begin, end, data, found )
      end subroutine dafrdr

      !> Write or rewrite the contents of a double precision record in
      !> a DAF.
      !>
      !> Legacy interface: calls ctx%dafwdr with the legacy context.
      subroutine dafwdr ( handle, recno, drec )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: recno  !! Record number.
      real(wp) :: drec     ( 128 )  !! Contents of record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafwdr ( handle, recno, drec )
      end subroutine dafwdr

      !> Return the number of reads and requests fielded by DAFRDR.
      !>
      !> Legacy interface: calls ctx%dafnrr with the legacy context.
      subroutine dafnrr ( reads, reqs )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: reads  !! Reads, requests in this execution.
      integer(ip) :: reqs  !! Reads, requests in this execution.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafnrr ( reads, reqs )
      end subroutine dafnrr

      !> Read the double precision data bounded by two addresses within
      !> a DAF.
      !>
      !> Legacy interface: calls ctx%dafgda with the legacy context.
      subroutine dafgda ( handle, baddr, eaddr, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a DAF.
      integer(ip) :: baddr  !! Initial, final address within file.
      integer(ip) :: eaddr  !! Initial, final address within file.
      real(wp) :: data     ( * )  !! Data contained between BADDR and EADDR.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafgda ( handle, baddr, eaddr, data )
      end subroutine dafgda

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzxlated with the legacy context.
      subroutine zzxlated ( inbff, input, space, output )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: inbff  !! Binary file format of d.p. values in INPUT.
      character(*) :: input  !! String containing d.p. values read as characters.
      integer(ip) :: space  !! Number of d.p. values that can be placed in OUTPUT.
      real(wp) :: output ( * )  !! Translated d.p. values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzxlated ( ctx, inbff, input, space, output )
      end subroutine zzxlated

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routines DAFGDA and DAFGSR. This routine is supported for purposes
      !> of backward compatibility only.
      !>
      !> Legacy interface: calls ctx%dafrda with the legacy context.
      subroutine dafrda ( handle, begin, end, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a DAF.
      integer(ip) :: begin  !! Initial, final address within file.
      integer(ip) :: end  !! Initial, final address within file.
      real(wp) :: data     ( * )  !! Data contained between BEGIN and END.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrda ( handle, begin, end, data )
      end subroutine dafrda

      !> Read the contents of the file record of a DAF.
      !>
      !> Legacy interface: calls ctx%dafrfr with the legacy context.
      subroutine dafrfr ( handle, nd, ni, ifname, fward, bward, free )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of an open DAF file.
      integer(ip) :: nd  !! Number of double precision components in summaries.
      integer(ip) :: ni  !! Number of integer components in summaries.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: fward  !! Forward list pointer.
      integer(ip) :: bward  !! Backward list pointer.
      integer(ip) :: free  !! Free address pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrfr ( handle, nd, ni, ifname, fward, bward, free )
      end subroutine dafrfr

      !> Write or rewrite the contents of the file record of a DAF.
      !>
      !> Legacy interface: calls ctx%dafwfr with the legacy context.
      subroutine dafwfr ( handle, nd, ni, ifname, fward, bward, free )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of an open DAF file.
      integer(ip) :: nd  !! Number of double precision components in summaries.
      integer(ip) :: ni  !! Number of integer components in summaries.
      character(*) :: ifname  !! Internal filename.
      integer(ip) :: fward  !! Forward list pointer.
      integer(ip) :: bward  !! Backward list pointer.
      integer(ip) :: free  !! Free address pointer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafwfr ( handle, nd, ni, ifname, fward, bward, free )
      end subroutine dafwfr

      !> Write or rewrite the double precision data bounded by two
      !> addresses within a DAF.
      !>
      !> Legacy interface: calls ctx%dafwda with the legacy context.
      subroutine dafwda ( handle, begin, end, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a DAF.
      integer(ip) :: begin  !! Initial, final address within file.
      integer(ip) :: end  !! Initial, final address within file.
      real(wp) :: data     ( * )  !! Data to be stored between BEGIN and END.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafwda ( handle, begin, end, data )
      end subroutine dafwda

      !> Add a specified number of reserved records to a Double Precision
      !> Array File (DAF).
      !>
      !> Legacy interface: calls ctx%dafarr with the legacy context.
      subroutine dafarr ( handle, resv )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of a DAF file opened for writing.
      integer(ip) :: resv  !! Number of records to reserve.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafarr ( handle, resv )
      end subroutine dafarr

      !> Read the contents of a character record from a DAF.
      !>
      !> Legacy interface: calls ctx%dafrcr with the legacy context.
      subroutine dafrcr ( handle, recno, crec )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: recno  !! Record number of character record.
      character(*) :: crec  !! Character record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrcr ( handle, recno, crec )
      end subroutine dafrcr

      !> Write or rewrite the contents of a character record to
      !> a DAF.
      !>
      !> Legacy interface: calls ctx%dafwcr with the legacy context.
      subroutine dafwcr ( handle, recno, crec )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: recno  !! Record number of character record.
      character(*) :: crec  !! Character record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafwcr ( handle, recno, crec )
      end subroutine dafwcr

      !> Reorder the arrays in a DAF according to a given order vector.
      !>
      !> Legacy interface: calls ctx%dafra with the legacy context.
      subroutine dafra ( handle, iorder, n )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      integer(ip) :: iorder ( * )  !! Order vector.
      integer(ip) :: n  !! Dimension of IORDER.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafra ( handle, iorder, n )
      end subroutine dafra

      !> Remove a specified number of reserved records from a Double
      !> Precision Array File (DAF).
      !>
      !> Legacy interface: calls ctx%dafrrr with the legacy context.
      subroutine dafrrr ( handle, resv )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! DAF, opened for writing.
      integer(ip) :: resv  !! Number of records to remove.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrrr ( handle, resv )
      end subroutine dafrrr

      !> Add comments from a buffer of character strings to the comment
      !> area of a binary DAF file, appending them to any comments which
      !> are already present in the file's comment area.
      !>
      !> Legacy interface: calls ctx%dafac with the legacy context.
      subroutine dafac ( handle, n, buffer )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of a DAF opened with write access.
      integer(ip) :: n  !! Number of comments to put into the comment area.
      character(*) :: buffer(*)  !! Buffer of comments to put into the comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafac ( handle, n, buffer )
      end subroutine dafac

      !> Delete the entire comment area of a previously opened binary
      !> DAF attached to HANDLE.
      !>
      !> Legacy interface: calls ctx%dafdc with the legacy context.
      subroutine dafdc ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! The handle of a binary DAF opened for writing.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafdc ( handle )
      end subroutine dafdc

      !> Extract comments from the comment area of a binary DAF.
      !>
      !> Legacy interface: calls ctx%dafec with the legacy context.
      subroutine dafec ( handle, bufsiz, n, buffer, done )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of binary DAF opened with read access.
      integer(ip) :: bufsiz  !! Maximum size, in lines, of BUFFER.
      integer(ip) :: n  !! Number of extracted comment lines.
      character(*) :: buffer(*)  !! Buffer where extracted comment lines are placed.
      logical :: done  !! Indicates whether all comments have been extracted.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafec ( handle, bufsiz, n, buffer, done )
      end subroutine dafec

      !> Begin a new array in a DAF.
      !>
      !> Legacy interface: calls ctx%dafbna with the legacy context.
      subroutine dafbna ( handle, sum, name )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of DAF.
      real(wp) :: sum      ( * )  !! Summary of new array.
      character(*) :: name  !! Name of new array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafbna ( handle, sum, name )
      end subroutine dafbna

      !> Add one or more double precision words of data to the newest
      !> array in the current DAF.
      !>
      !> Legacy interface: calls ctx%dafada with the legacy context.
      subroutine dafada ( data, n )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      real(wp) :: data     ( * )  !! Elements of the new array.
      integer(ip) :: n  !! Number of elements in DATA.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafada ( data, n )
      end subroutine dafada

      !> End the addition of data to the newest array in the current DAF.
      !>
      !> Legacy interface: calls ctx%dafena with the legacy context.
      subroutine dafena (  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafena (  )
      end subroutine dafena

      !> Select a DAF that already has a new array in progress as the
      !> one to continue adding data to.
      !>
      !> Legacy interface: calls ctx%dafcad with the legacy context.
      subroutine dafcad ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of DAF to continue adding data to.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafcad ( handle )
      end subroutine dafcad

      !> Begin a forward search for arrays in a DAF.
      !>
      !> Legacy interface: calls ctx%dafbfs with the legacy context.
      subroutine dafbfs ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of file to be searched.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafbfs ( handle )
      end subroutine dafbfs

      !> Find the next (forward) array in the current DAF.
      !>
      !> Legacy interface: calls ctx%daffna with the legacy context.
      subroutine daffna ( found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      logical :: found  !! .TRUE. if an array was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daffna ( found )
      end subroutine daffna

      !> Begin a backward search for arrays in a DAF.
      !>
      !> Legacy interface: calls ctx%dafbbs with the legacy context.
      subroutine dafbbs ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of DAF to be searched.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafbbs ( handle )
      end subroutine dafbbs

      !> Find the previous (backward) array in the current DAF.
      !>
      !> Legacy interface: calls ctx%daffpa with the legacy context.
      subroutine daffpa ( found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      logical :: found  !! .TRUE. if an array was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daffpa ( found )
      end subroutine daffpa

      !> Return (get) the summary for the current array in the current
      !> DAF.
      !>
      !> Legacy interface: calls ctx%dafgs with the legacy context.
      subroutine dafgs ( sum )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      real(wp) :: sum    ( * )  !! Summary for current array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafgs ( sum )
      end subroutine dafgs

      !> Return (get) the name for the current array in the current DAF.
      !>
      !> Legacy interface: calls ctx%dafgn with the legacy context.
      subroutine dafgn ( name )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: name  !! Name of current array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafgn ( name )
      end subroutine dafgn

      !> Return (get) the handle of the DAF currently being searched.
      !>
      !> Legacy interface: calls ctx%dafgh with the legacy context.
      subroutine dafgh ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle for current DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafgh ( handle )
      end subroutine dafgh

      !> Change the summary for the current array in the current DAF.
      !>
      !> Legacy interface: calls ctx%dafrs with the legacy context.
      subroutine dafrs ( sum )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      real(wp) :: sum    ( * )  !! New summary for current array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrs ( sum )
      end subroutine dafrs

      !> Replace the name for the current array in the current DAF.
      !>
      !> Legacy interface: calls ctx%dafrn with the legacy context.
      subroutine dafrn ( name )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: name  !! New name for current array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafrn ( name )
      end subroutine dafrn

      !> Write a new summary for the current array in the current DAF.
      !>
      !> Legacy interface: calls ctx%dafws with the legacy context.
      subroutine dafws ( sum )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      real(wp) :: sum    ( * )  !! New summary for current array in the current DAF.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafws ( sum )
      end subroutine dafws

      !> Select a DAF that already has a search in progress as the
      !> one to continue searching.
      !>
      !> Legacy interface: calls ctx%dafcs with the legacy context.
      subroutine dafcs ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Handle of DAF to continue searching.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafcs ( handle )
      end subroutine dafcs

      !> Convert the contents of a binary DAF file to an equivalent DAF
      !> transfer file.
      !>
      !> Legacy interface: calls ctx%dafbt with the legacy context.
      subroutine dafbt ( binfil, xfrlun )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: binfil  !! The name of a binary DAF file to be converted.
      integer(ip) :: xfrlun  !! Logical unit of a previously opened file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafbt ( binfil, xfrlun )
      end subroutine dafbt

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine DAFTB. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Legacy interface: calls ctx%dafa2b with the legacy context.
      subroutine dafa2b ( ascii, binary, resv )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: ascii  !! Name of an existing ASCII (text) DAF.
      character(*) :: binary  !! Name of a binary DAF to be created.
      integer(ip) :: resv  !! Number of records to reserve.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafa2b ( ascii, binary, resv )
      end subroutine dafa2b

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine DAFBT. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Legacy interface: calls ctx%dafb2a with the legacy context.
      subroutine dafb2a ( binary, ascii )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: binary  !! Name of an existing binary DAF.
      character(*) :: ascii  !! Name of an ASCII (text) DAF to be created.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafb2a ( binary, ascii )
      end subroutine dafb2a

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine DAFBT. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Legacy interface: calls ctx%dafb2t with the legacy context.
      subroutine dafb2t ( binary, text )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: binary  !! Name of an existing binary DAF.
      integer(ip) :: text  !! Logical unit connected to text file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dafb2t ( binary, text )
      end subroutine dafb2t

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine DAFTB. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Legacy interface: calls ctx%daft2b with the legacy context.
      subroutine daft2b ( text, binary, resv )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: text  !! Logical unit connected to text file.
      character(*) :: binary  !! Name of a binary DAF to be created.
      integer(ip) :: resv  !! Number of records to reserve.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daft2b ( text, binary, resv )
      end subroutine daft2b

      !> Convert the contents of an DAF transfer file into an equivalent
      !> binary DAF file.
      !>
      !> Legacy interface: calls ctx%daftb with the legacy context.
      subroutine daftb ( xfrlun, binfil )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: xfrlun  !! Logical unit of an open DAF transfer file.
      character(*) :: binfil  !! Name of a binary DAF file to be created.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daftb ( xfrlun, binfil )
      end subroutine daftb

end module pepper_legacy_daf
