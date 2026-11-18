!> The legacy interface's routines of the subsystem das (63 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_das

   use pepper_instance_module, only: pepper_context, pepper_zzdasgrd => zzdasgrd, &
      pepper_zzdasgri => zzdasgri, pepper_zzdasnfr => zzdasnfr, pepper_zzdasrfr => zzdasrfr
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell
   use pepper_inc_das, only: nwd, nwi

   implicit none

   private
   public :: rdencc, wrencc, writln, writla, readln, readla, zzdasgrd, zzdasgri, zzdasnfr, &
      zzdasrfr, dasopr, dasopw, dasonw, dasopn, dasops, dasllc, dashfs, dasufs, dashlu, dasluh, &
      dashfn, dasfnh, dashof, dassih, dasham, dasrrd, dasrri, dasrrc, daswrd, daswri, daswrc, &
      dasurd, dasuri, dasurc, daswbr, dasioc, dasiod, dasioi, dasa2l, dascud, dasacr, dasrcr, &
      dassdr, dasadc, dasadd, dasadi, dasrdc, dasrdd, dasrdi, dasudc, dasudd, dasudi, dascls, &
      dasrfr, daswfr, dasac, dasacu, dasdc, dasec, dasecu, dasbt, dastb, daslla

contains

      !> Read and decode encoded characters from a text file.
      !>
      !> Legacy interface: calls ctx%rdencc with the legacy context.
      subroutine rdencc ( unit, n, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Fortran unit number of input text file.
      integer(ip) :: n  !! Number of characters to be read and decoded.
      character(*) :: data(*)  !! List of decoded characters to be returned.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdencc ( unit, n, data )
      end subroutine rdencc

      !> Encode and write characters to a text file.
      !>
      !> Legacy interface: calls ctx%wrencc with the legacy context.
      subroutine wrencc ( unit, n, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Fortran unit number of output text file.
      integer(ip) :: n  !! Number of characters to encode and write.
      character(*) :: data ( * )  !! List of characters to encode and write.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wrencc ( unit, n, data )
      end subroutine wrencc

      !> Write a single line of text to the Fortran logical unit UNIT.
      !>
      !> Legacy interface: calls ctx%writln with the legacy context.
      subroutine writln ( line, unit )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: line  !! The line which is to be written to UNIT.
      integer(ip) :: unit  !! The Fortran unit number to use for output.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%writln ( line, unit )
      end subroutine writln

      !> Write an array of text lines to a Fortran logical unit.
      !>
      !> Legacy interface: calls ctx%writla with the legacy context.
      subroutine writla ( numlin, array, unit )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: numlin  !! Number of lines to be written to the file.
      character(*) :: array(*)  !! Array containing the lines to be written.
      integer(ip) :: unit  !! Fortran unit number to use for output.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%writla ( numlin, array, unit )
      end subroutine writla

      !> Read a single line of text from the Fortran logical unit UNIT,
      !> reporting the end of file if it occurs.
      !>
      !> Legacy interface: calls ctx%readln with the legacy context.
      subroutine readln ( unit, line, eof )
      use pepper_kinds, only: ip
      integer(ip) :: unit  !! The Fortran unit number to use for input.
      character(*) :: line  !! The line read from the file.
      logical :: eof  !! A logical flag indicating the end of file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%readln ( unit, line, eof )
      end subroutine readln

      !> Read lines from a Fortran logical unit and place them in a
      !> character string array.
      !>
      !> Legacy interface: calls ctx%readla with the legacy context.
      subroutine readla ( unit, maxlin, numlin, array, eof )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Fortran unit number to use for input.
      integer(ip) :: maxlin  !! Maximum number of lines ARRAY can hold.
      integer(ip) :: numlin  !! Number of lines read from the file.
      character(*) :: array(*)  !! Array containing the lines read from the file.
      logical :: eof  !! Logical flag indicating the end of file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%readla ( unit, maxlin, numlin, array, eof )
      end subroutine readla

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdasgrd with the legacy context.
      subroutine zzdasgrd ( handle, recno, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_das
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      real(wp) :: record ( nwd )  !! Double precision data read from record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdasgrd ( ctx, handle, recno, record )
      end subroutine zzdasgrd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdasgri with the legacy context.
      subroutine zzdasgri ( handle, recno, record )
      use pepper_kinds, only: ip
      use pepper_inc_das
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: record ( nwi )  !! Integer data read from record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdasgri ( ctx, handle, recno, record )
      end subroutine zzdasgri

      !> Write the file record to a new DAS file.
      !>
      !> Legacy interface: calls zz routine zzdasnfr with the legacy context.
      subroutine zzdasnfr ( lun, idword, ifname, nresvr, nresvc, ncomr, ncomc, format )
      use pepper_kinds, only: ip
      use pepper_inc_zzftprms
      implicit none
      integer(ip) :: lun  !! Logical unit number of an open DAS file.
      character(*) :: idword  !! DAS File ID word.
      character(*) :: ifname  !! DAS internal file name.
      integer(ip) :: nresvr  !! Number of reserved records in file.
      integer(ip) :: nresvc  !! Number of characters in use in reserved rec. area.
      integer(ip) :: ncomr  !! Number of comment records in file.
      integer(ip) :: ncomc  !! Number of characters in use in comment area.
      character(*) :: format  !! File binary format identifier string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdasnfr ( ctx, lun, idword, ifname, nresvr, nresvc, ncomr, ncomc, format )
      end subroutine zzdasnfr

      !> Return the contents of the file record of a specified DAS file.
      !>
      !> Legacy interface: calls zz routine zzdasrfr with the legacy context.
      subroutine zzdasrfr ( handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      character(*) :: idword  !! ID word.
      character(*) :: ifname  !! DAS internal file name.
      integer(ip) :: nresvr  !! Number of reserved records in file.
      integer(ip) :: nresvc  !! Number of characters in use in reserved rec. area.
      integer(ip) :: ncomr  !! Number of comment records in file.
      integer(ip) :: ncomc  !! Number of characters in use in comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdasrfr ( ctx, handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
      end subroutine zzdasrfr

      !> Open a DAS file for reading.
      !>
      !> Legacy interface: calls ctx%dasopr with the legacy context.
      subroutine dasopr ( fname, handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      character(*) :: fname  !! Name of a DAS file to be opened.
      integer(ip) :: handle  !! Handle assigned to the opened DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasopr ( fname, handle )
      end subroutine dasopr

      !> Open a DAS file for writing.
      !>
      !> Legacy interface: calls ctx%dasopw with the legacy context.
      subroutine dasopw ( fname, handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      character(*) :: fname  !! Name of a DAS file to be opened.
      integer(ip) :: handle  !! Handle assigned to the opened DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasopw ( fname, handle )
      end subroutine dasopw

      !> Open a new DAS file and set the file type.
      !>
      !> Legacy interface: calls ctx%dasonw with the legacy context.
      subroutine dasonw ( fname, ftype, ifname, ncomr, handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      character(*) :: fname  !! Name of a DAS file to be opened.
      character(*) :: ftype  !! Mnemonic code for type of data in the DAS file.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: handle  !! Handle assigned to the opened DAS file.
      integer(ip) :: ncomr  !! Number of comment records to allocate.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasonw ( fname, ftype, ifname, ncomr, handle )
      end subroutine dasonw

      !> **Obsolete:** This routine has been superseded by DASONW, and it is
      !> supported for purposes of backward compatibility only.
      !>
      !> Legacy interface: calls ctx%dasopn with the legacy context.
      subroutine dasopn ( fname, ifname, handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      character(*) :: fname  !! Name of a DAS file to be opened.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: handle  !! Handle assigned to the opened DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasopn ( fname, ifname, handle )
      end subroutine dasopn

      !> Open a scratch DAS file for writing.
      !>
      !> Legacy interface: calls ctx%dasops with the legacy context.
      subroutine dasops ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! Handle assigned to a scratch DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasops ( handle )
      end subroutine dasops

      !> Close the DAS file associated with a given handle, without
      !> flushing buffered data or segregating the file.
      !>
      !> Legacy interface: calls ctx%dasllc with the legacy context.
      subroutine dasllc ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! Handle of a DAS file to be closed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasllc ( handle )
      end subroutine dasllc

      !> Return a file summary for a specified DAS file.
      !>
      !> Legacy interface: calls ctx%dashfs with the legacy context.
      subroutine dashfs ( handle, nresvr, nresvc, ncomr, ncomc, free, lastla, lastrc, lastwd )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! Handle of a DAS file.
      integer(ip) :: free  !! Number of first free record.
      integer(ip) :: lastla ( 3 )  !! Array of last logical addresses for each data type.
      integer(ip) :: lastrc ( 3 )  !! Record number of last descriptor of each data type.
      integer(ip) :: lastwd ( 3 )  !! Word number of last descriptor of each data type.
      integer(ip) :: nresvr  !! Number of reserved records in file.
      integer(ip) :: nresvc  !! Number of characters in use in reserved rec. area.
      integer(ip) :: ncomr  !! Number of comment records in file.
      integer(ip) :: ncomc  !! Number of characters in use in comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dashfs ( handle, nresvr, nresvc, ncomr, ncomc, free, lastla, lastrc, lastwd )
      end subroutine dashfs

      !> Update the file summary in a specified DAS file.
      !>
      !> Legacy interface: calls ctx%dasufs with the legacy context.
      subroutine dasufs ( handle, nresvr, nresvc, ncomr, ncomc, free, lastla, lastrc, lastwd )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! Handle of an open DAS file.
      integer(ip) :: free  !! Number of first free record.
      integer(ip) :: lastla ( 3 )  !! Array of last logical addresses for each data type.
      integer(ip) :: lastrc ( 3 )  !! Record number of last descriptor of each data type.
      integer(ip) :: lastwd ( 3 )  !! Word number of last descriptor of each data type.
      integer(ip) :: nresvr  !! Number of reserved records in file.
      integer(ip) :: nresvc  !! Number of characters in use in reserved rec. area.
      integer(ip) :: ncomr  !! Number of comment records in file.
      integer(ip) :: ncomc  !! Number of characters in use in comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasufs ( handle, nresvr, nresvc, ncomr, ncomc, free, lastla, lastrc, lastwd )
      end subroutine dasufs

      !> Return the logical unit associated with a handle. The unit
      !> is "locked" to the handle by the DAF/DAS handle manager
      !> subsystem.
      !>
      !> Legacy interface: calls ctx%dashlu with the legacy context.
      subroutine dashlu ( handle, unit )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! Handle of a DAS file.
      integer(ip) :: unit  !! Corresponding logical unit.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dashlu ( handle, unit )
      end subroutine dashlu

      !> Return the handle associated with a logical unit.
      !>
      !> Legacy interface: calls ctx%dasluh with the legacy context.
      subroutine dasluh ( unit, handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! Corresponding handle.
      integer(ip) :: unit  !! Logical unit connected to a DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasluh ( unit, handle )
      end subroutine dasluh

      !> Return the name of the DAS file associated with a handle.
      !>
      !> Legacy interface: calls ctx%dashfn with the legacy context.
      subroutine dashfn ( handle, fname )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      character(*) :: fname  !! Corresponding file name.
      integer(ip) :: handle  !! Handle of a DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dashfn ( handle, fname )
      end subroutine dashfn

      !> Return handle associated with a file name.
      !>
      !> Legacy interface: calls ctx%dasfnh with the legacy context.
      subroutine dasfnh ( fname, handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      character(*) :: fname  !! Name of a DAS file.
      integer(ip) :: handle  !! Corresponding handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasfnh ( fname, handle )
      end subroutine dasfnh

      !> Return a SPICE set containing the handles of all currently
      !> open DAS files.
      !>
      !> Legacy interface: calls ctx%dashof with the legacy context.
      subroutine dashof ( fhset )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: fhset  ( lbcell : * )  !! A set containing handles of currently open DAS files.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dashof ( fhset )
      end subroutine dashof

      !> Signal an error if a DAS file file handle does not designate a
      !> DAS file that is open for a specified type of access.
      !>
      !> Legacy interface: calls ctx%dassih with the legacy context.
      subroutine dassih ( handle, access )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! HANDLE to be validated.
      character(*) :: access  !! String indicating access type.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dassih ( handle, access )
      end subroutine dassih

      !> Return the allowed access method for a specified DAS file.
      !>
      !> Legacy interface: calls ctx%dasham with the legacy context.
      subroutine dasham ( handle, access )
      use pepper_kinds, only: ip
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip) :: handle  !! HANDLE of a DAS file.
      character(*) :: access  !! String indicating allowed access method.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasham ( handle, access )
      end subroutine dasham

      !> Read DAS double precision physical records.
      !>
      !> Legacy interface: calls ctx%dasrrd with the legacy context.
      subroutine dasrrd ( handle, recno, first, last, datad )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: first  !! First and last indices of range within record.
      integer(ip) :: last  !! First and last indices of range within record.
      real(wp) :: datad   ( * )  !! Double precision data read from record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrrd ( handle, recno, first, last, datad )
      end subroutine dasrrd

      !> Read DAS integer physical records.
      !>
      !> Legacy interface: calls ctx%dasrri with the legacy context.
      subroutine dasrri ( handle, recno, first, last, datai )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: first  !! First and last indices of range within record.
      integer(ip) :: last  !! First and last indices of range within record.
      integer(ip) :: datai   ( * )  !! Integer data read from record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrri ( handle, recno, first, last, datai )
      end subroutine dasrri

      !> Read DAS character physical records.
      !>
      !> Legacy interface: calls ctx%dasrrc with the legacy context.
      subroutine dasrrc ( handle, recno, first, last, datac )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: first  !! First and last indices of range within record.
      integer(ip) :: last  !! First and last indices of range within record.
      character(*) :: datac  !! Character data read from record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrrc ( handle, recno, first, last, datac )
      end subroutine dasrrc

      !> Write DAS double precision physical records.
      !>
      !> Legacy interface: calls ctx%daswrd with the legacy context.
      subroutine daswrd ( handle, recno, recd )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      real(wp) :: recd    ( nwd )  !! Double precision data to be written to record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daswrd ( handle, recno, recd )
      end subroutine daswrd

      !> Write DAS integer physical records.
      !>
      !> Legacy interface: calls ctx%daswri with the legacy context.
      subroutine daswri ( handle, recno, reci )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: reci    ( nwi )  !! Integer data to be written to record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daswri ( handle, recno, reci )
      end subroutine daswri

      !> Write DAS character physical records.
      !>
      !> Legacy interface: calls ctx%daswrc with the legacy context.
      subroutine daswrc ( handle, recno, recc )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      character(*) :: recc  !! Character data to be written to record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daswrc ( handle, recno, recc )
      end subroutine daswrc

      !> Update DAS double precision physical records.
      !>
      !> Legacy interface: calls ctx%dasurd with the legacy context.
      subroutine dasurd ( handle, recno, first, last, datad )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: first  !! First and last indices of range within record.
      integer(ip) :: last  !! First and last indices of range within record.
      real(wp) :: datad   ( * )  !! Double precision data to write to record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasurd ( handle, recno, first, last, datad )
      end subroutine dasurd

      !> Update DAS integer physical records.
      !>
      !> Legacy interface: calls ctx%dasuri with the legacy context.
      subroutine dasuri ( handle, recno, first, last, datai )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: first  !! First and last indices of range within record.
      integer(ip) :: last  !! First and last indices of range within record.
      integer(ip) :: datai   ( * )  !! Integer data to write to record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasuri ( handle, recno, first, last, datai )
      end subroutine dasuri

      !> Update DAS character physical records.
      !>
      !> Legacy interface: calls ctx%dasurc with the legacy context.
      subroutine dasurc ( handle, recno, first, last, datac )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      integer(ip) :: recno  !! Record number.
      integer(ip) :: first  !! First and last indices of range within record.
      integer(ip) :: last  !! First and last indices of range within record.
      character(*) :: datac  !! Character data to write to record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasurc ( handle, recno, first, last, datac )
      end subroutine dasurc

      !> Write out all buffered records of a specified DAS file.
      !>
      !> Legacy interface: calls ctx%daswbr with the legacy context.
      subroutine daswbr ( handle )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daswbr ( handle )
      end subroutine daswbr

      !> Perform Fortran reads and writes of DAS character records.
      !>
      !> Legacy interface: calls ctx%dasioc with the legacy context.
      subroutine dasioc ( action, unit, recno, record )
      use pepper_kinds, only: ip
      implicit none
      integer(ip), parameter :: nwc = 1024
      character(*) :: action  !! Action to take (read or write).
      integer(ip) :: unit  !! Fortran unit connected to DAS file.
      integer(ip) :: recno  !! Number of record to read or write.
      character(nwc) :: record  !! DAS character record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: action is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dasioc ( (action), unit, recno, record )
      end subroutine dasioc

      !> Perform Fortran reads and writes of DAS double precision records.
      !> This routine operates on DAS files having native binary format.
      !>
      !> Legacy interface: calls ctx%dasiod with the legacy context.
      subroutine dasiod ( action, unit, recno, record )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: action  !! Action to take (read or write).
      integer(ip) :: unit  !! Fortran unit connected to DAS file.
      integer(ip) :: recno  !! Number of record to read or write.
      real(wp) :: record ( nwd )  !! DAS double precision record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasiod ( action, unit, recno, record )
      end subroutine dasiod

      !> Perform Fortran reads and writes of DAS integer records.
      !> This routine operates on DAS files having native binary
      !> format.
      !>
      !> Legacy interface: calls ctx%dasioi with the legacy context.
      subroutine dasioi ( action, unit, recno, record )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: action  !! Action to take (read or write).
      integer(ip) :: unit  !! Fortran unit connected to DAS file.
      integer(ip) :: recno  !! Number of record to read or write.
      integer(ip) :: record ( nwi )  !! DAS integer record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasioi ( action, unit, recno, record )
      end subroutine dasioi

      !> Map a DAS address to a physical location in a specified DAS file.
      !>
      !> Legacy interface: calls ctx%dasa2l with the legacy context.
      subroutine dasa2l ( handle, type, addrss, clbase, clsize, recno, wordno )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: type  !! Data type specifier.
      integer(ip) :: addrss  !! DAS address of a word of data type TYPE.
      integer(ip) :: clbase  !! Cluster base record number and size.
      integer(ip) :: clsize  !! Cluster base record number and size.
      integer(ip) :: recno  !! Record/word pair corresponding to ADDRSS.
      integer(ip) :: wordno  !! Record/word pair corresponding to ADDRSS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasa2l ( handle, type, addrss, clbase, clsize, recno, wordno )
      end subroutine dasa2l

      !> Create or update directories in a DAS file to reflect addition
      !> of a specified number of words of a specified data type.
      !>
      !> Legacy interface: calls ctx%dascud with the legacy context.
      subroutine dascud ( handle, type, nwords )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: type  !! Data type specifier.
      integer(ip) :: nwords  !! Number of words of data being added.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dascud ( handle, type, nwords )
      end subroutine dascud

      !> Increase the size of the comment area in a DAS file to accommodate
      !> a specified number of additional comment records.
      !>
      !> Legacy interface: calls ctx%dasacr with the legacy context.
      subroutine dasacr ( handle, n )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! A DAS file handle.
      integer(ip) :: n  !! Number of comment records to append to the comment area of the specified
                        !! file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasacr ( handle, n )
      end subroutine dasacr

      !> Decrease the size of the comment area in a DAS file to reclaim
      !> space freed by the removal of a specified number of comment
      !> records.
      !>
      !> Legacy interface: calls ctx%dasrcr with the legacy context.
      subroutine dasrcr ( handle, n )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! A DAS file handle.
      integer(ip) :: n  !! Number of comment records to remove.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrcr ( handle, n )
      end subroutine dasrcr

      !> Segregate the data records in a DAS file into clusters, using
      !> one cluster per data type present in the file.
      !>
      !> Legacy interface: calls ctx%dassdr with the legacy context.
      subroutine dassdr ( handle )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dassdr ( handle )
      end subroutine dassdr

      !> Add character data to a DAS file.
      !>
      !> Legacy interface: calls ctx%dasadc with the legacy context.
      subroutine dasadc ( handle, n, bpos, epos, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: n  !! Number of characters to add to file.
      integer(ip) :: bpos  !! Begin and end positions of substrings.
      integer(ip) :: epos  !! Begin and end positions of substrings.
      character(*) :: data   ( * )  !! Array providing the set of substrings to be added to the
                                    !! character data in the DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasadc ( handle, n, bpos, epos, data )
      end subroutine dasadc

      !> Add an array of double precision numbers to a DAS file.
      !>
      !> Legacy interface: calls ctx%dasadd with the legacy context.
      subroutine dasadd ( handle, n, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: n  !! Number of d.p. numbers to add to DAS file.
      real(wp) :: data   ( * )  !! Array of d.p. numbers to add.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasadd ( handle, n, data )
      end subroutine dasadd

      !> Add an array of integers to a DAS file.
      !>
      !> Legacy interface: calls ctx%dasadi with the legacy context.
      subroutine dasadi ( handle, n, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: n  !! Number of integers to add to DAS file.
      integer(ip) :: data   ( * )  !! Array of integers to add.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasadi ( handle, n, data )
      end subroutine dasadi

      !> Read character data from a range of DAS logical addresses.
      !>
      !> Legacy interface: calls ctx%dasrdc with the legacy context.
      subroutine dasrdc ( handle, first, last, bpos, epos, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: first  !! Range of DAS character logical addresses.
      integer(ip) :: last  !! Range of DAS character logical addresses.
      integer(ip) :: bpos  !! Begin and end positions of substrings.
      integer(ip) :: epos  !! Begin and end positions of substrings.
      character(*) :: data   ( * )  !! Data having addresses FIRST through LAST.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrdc ( handle, first, last, bpos, epos, data )
      end subroutine dasrdc

      !> Read double precision data from a range of DAS logical addresses.
      !>
      !> Legacy interface: calls ctx%dasrdd with the legacy context.
      subroutine dasrdd ( handle, first, last, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: first  !! Bounds of range of DAS double precision logical addresses.
      integer(ip) :: last  !! Bounds of range of DAS double precision logical addresses.
      real(wp) :: data   ( * )  !! Data having addresses FIRST through LAST.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrdd ( handle, first, last, data )
      end subroutine dasrdd

      !> Read integer data from a range of DAS logical addresses.
      !>
      !> Legacy interface: calls ctx%dasrdi with the legacy context.
      subroutine dasrdi ( handle, first, last, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: first  !! Bounds of range of DAS integer logical addresses.
      integer(ip) :: last  !! Bounds of range of DAS integer logical addresses.
      integer(ip) :: data   ( * )  !! Data having addresses FIRST through LAST.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrdi ( handle, first, last, data )
      end subroutine dasrdi

      !> Update character data in a specified range of DAS logical
      !> addresses with substrings of a character array.
      !>
      !> Legacy interface: calls ctx%dasudc with the legacy context.
      subroutine dasudc ( handle, first, last, bpos, epos, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: first  !! Range of DAS character logical addresses.
      integer(ip) :: last  !! Range of DAS character logical addresses.
      integer(ip) :: bpos  !! Begin and end positions of substrings.
      integer(ip) :: epos  !! Begin and end positions of substrings.
      character(*) :: data   ( * )  !! Data having addresses FIRST through LAST.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasudc ( handle, first, last, bpos, epos, data )
      end subroutine dasudc

      !> Update data in a specified range of double precision addresses
      !> in a DAS file.
      !>
      !> Legacy interface: calls ctx%dasudd with the legacy context.
      subroutine dasudd ( handle, first, last, data )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: first  !! Range of d.p. addresses to write to.
      integer(ip) :: last  !! Range of d.p. addresses to write to.
      real(wp) :: data   ( * )  !! An array of d.p. numbers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasudd ( handle, first, last, data )
      end subroutine dasudd

      !> Update data in a specified range of integer addresses in a DAS
      !> file.
      !>
      !> Legacy interface: calls ctx%dasudi with the legacy context.
      subroutine dasudi ( handle, first, last, data )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: first  !! Range of integer addresses to write to.
      integer(ip) :: last  !! Range of integer addresses to write to.
      integer(ip) :: data   ( * )  !! An array of integers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasudi ( handle, first, last, data )
      end subroutine dasudi

      !> Close a DAS file.
      !>
      !> Legacy interface: calls ctx%dascls with the legacy context.
      subroutine dascls ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_das
      implicit none
      integer(ip) :: handle  !! Handle of an open DAS file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dascls ( handle )
      end subroutine dascls

      !> Return the contents of the file record of a specified DAS
      !> file.
      !>
      !> Legacy interface: calls ctx%dasrfr with the legacy context.
      subroutine dasrfr ( handle, idword, ifname, nresvr, &
                          nresvc, ncomr,  ncomc          )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      character(*) :: idword  !! ID word.
      character(*) :: ifname  !! DAS internal file name.
      integer(ip) :: nresvr  !! Number of reserved records in file.
      integer(ip) :: nresvc  !! Number of characters in use in reserved rec. area.
      integer(ip) :: ncomr  !! Number of comment records in file.
      integer(ip) :: ncomc  !! Number of characters in use in comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasrfr ( handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
      end subroutine dasrfr

      !> Update the contents of the file record of a specified DAS file.
      !>
      !> Legacy interface: calls ctx%daswfr with the legacy context.
      subroutine daswfr ( handle, &
                          idword, &
                          ifname, &
                          nresvr, &
                          nresvc, &
                          ncomr, &
                          ncomc  )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      character(*) :: idword  !! ID word.
      character(*) :: ifname  !! DAS internal file name.
      integer(ip) :: nresvr  !! Number of reserved records in file.
      integer(ip) :: nresvc  !! Number of characters in use in reserved rec. area.
      integer(ip) :: ncomr  !! Number of comment records in file.
      integer(ip) :: ncomc  !! Number of characters in use in comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daswfr ( handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
      end subroutine daswfr

      !> Add comments from a buffer of character strings to the comment
      !> area of a binary DAS file, appending them to any comments which
      !> are already present in the file's comment area.
      !>
      !> Legacy interface: calls ctx%dasac with the legacy context.
      subroutine dasac ( handle, n, buffer )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS handle of a file opened with write access.
      integer(ip) :: n  !! Number of comments to put into the comment area.
      character(*) :: buffer(*)  !! Buffer of lines to be put into the comment area.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasac ( handle, n, buffer )
      end subroutine dasac

      !> Add comments to a previously opened binary DAS file from a
      !> previously opened text file attached to a Fortran logical unit.
      !>
      !> Legacy interface: calls ctx%dasacu with the legacy context.
      subroutine dasacu ( comlun, begmrk, endmrk, insbln, handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: comlun  !! Logical unit of the open comment text file.
      character(*) :: begmrk  !! The begin comments marker in the comment text file.
      character(*) :: endmrk  !! The end comments marker in the comment text file.
      logical :: insbln  !! A flag indicating whether to insert a blank line.
      integer(ip) :: handle  !! Handle of a DAS file opened with write access.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasacu ( comlun, begmrk, endmrk, insbln, handle )
      end subroutine dasacu

      !> Delete the entire comment area of a previously opened binary
      !> DAS file.
      !>
      !> Legacy interface: calls ctx%dasdc with the legacy context.
      subroutine dasdc ( handle )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! The handle of a binary DAS file opened for writing.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasdc ( handle )
      end subroutine dasdc

      !> Extract comments from the comment area of a binary DAS file.
      !>
      !> Legacy interface: calls ctx%dasec with the legacy context.
      subroutine dasec ( handle, bufsiz, n, buffer, done )
      use pepper_kinds, only: ip
      use pepper_inc_das
      implicit none
      integer(ip) :: handle  !! Handle of binary DAS file open with read access.
      integer(ip) :: bufsiz  !! Maximum size, in lines, of BUFFER.
      integer(ip) :: n  !! Number of comments extracted from the DAS file.
      character(*) :: buffer(*)  !! Buffer in which extracted comments are placed.
      logical :: done  !! Indicates whether all comments have been extracted.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasec ( handle, bufsiz, n, buffer, done )
      end subroutine dasec

      !> Extract comments from a previously opened binary DAS file to a
      !> previously opened text file attached to a Fortran logical unit.
      !>
      !> Legacy interface: calls ctx%dasecu with the legacy context.
      subroutine dasecu ( handle, comlun, comnts )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! Handle of a DAS file opened with read access.
      integer(ip) :: comlun  !! Logical unit of an opened text file.
      logical :: comnts  !! Logical flag, indicating comments were found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasecu ( handle, comlun, comnts )
      end subroutine dasecu

      !> Convert the contents of a binary DAS file to an equivalent DAS
      !> transfer file.
      !>
      !> Legacy interface: calls ctx%dasbt with the legacy context.
      subroutine dasbt ( binfil, xfrlun )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: binfil  !! Name of the binary DAS file to be converted.
      integer(ip) :: xfrlun  !! Logical unit of a previously opened file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dasbt ( binfil, xfrlun )
      end subroutine dasbt

      !> Convert the contents of a DAS transfer file into an equivalent
      !> binary DAS file.
      !>
      !> Legacy interface: calls ctx%dastb with the legacy context.
      subroutine dastb ( xfrlun, binfil )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: xfrlun  !! Logical unit of an open DAS transfer file.
      character(*) :: binfil  !! Name of the binary DAS file to be created.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dastb ( xfrlun, binfil )
      end subroutine dastb

      !> Return last DAS logical addresses of character, double precision
      !> and integer type that are currently in use in a specified DAS
      !> file.
      !>
      !> Legacy interface: calls ctx%daslla with the legacy context.
      subroutine daslla ( handle, lastc, lastd, lasti )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: handle  !! DAS file handle.
      integer(ip) :: lastc  !! Last character address in use.
      integer(ip) :: lastd  !! Last double precision address in use.
      integer(ip) :: lasti  !! Last integer address in use.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%daslla ( handle, lastc, lastd, lasti )
      end subroutine daslla

end module pepper_legacy_das
