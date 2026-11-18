!> The legacy interface's routines of the subsystem dsk_files (39 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_dsk_files

   use pepper_instance_module, only: pepper_context, pepper_zzsizeok => zzsizeok, &
      pepper_zzckspk => zzckspk, pepper_zzvrtplt => zzvrtplt, pepper_zzvoxcvo => zzvoxcvo, &
      pepper_zzvox2id => zzvox2id, pepper_zztrvlnk => zztrvlnk, pepper_zzuntngl => zzuntngl, &
      pepper_zzinilnk => zzinilnk, pepper_zzgetvox => zzgetvox, pepper_zzaddlnk => zzaddlnk, &
      pepper_zzmkspin => zzmkspin
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: zzsizeok, zzckspk, getfat, surfnm, nearpt, recgeo, zzvrtplt, zzvoxcvo, zzvox2id, &
      zztrvlnk, zzuntngl, zzinilnk, zzgetvox, zzaddlnk, zzmkspin, dlaopn, dlabns, dlaens, dlabfs, &
      dlabbs, dlafns, dlafps, dlassg, dskopn, dskcls, dskgtl, dskstl, dskgd, dskb02, dskd02, &
      dski02, dskn02, dskp02, dskv02, dskz02, dskmi2, dskrb2, dskobj, dsksrf

contains

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsizeok with the legacy context.
      subroutine zzsizeok ( size, psize, dsize, offset, ok, n )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: size  !! Left hand side of the equation in the abstract
      integer(ip) :: psize  !! Coefficient of N (packet size).
      integer(ip) :: dsize  !! Divisor of N-OFFSET (directory size).
      integer(ip) :: offset  !! Offset used in computation of number of directories
      logical :: ok  !! TRUE if a solution for N exists.
      integer(ip) :: n  !! Value of N if there is a solution, 0 otherwise.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsizeok ( ctx, size, psize, dsize, offset, ok, n )
      end subroutine zzsizeok

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzckspk with the legacy context.
      subroutine zzckspk ( handle, ckspk )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! the handle of a DAF file open for read access.
      character(*) :: ckspk  !! the type of the DAF file (SPK,CK or ?)
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzckspk ( ctx, handle, ckspk )
      end subroutine zzckspk

      !> Determine the architecture and type of SPICE kernels.
      !>
      !> Legacy interface: calls ctx%getfat with the legacy context.
      subroutine getfat ( file, arch, kertyp )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      character(*) :: file  !! The name of a file to be examined.
      character(*) :: arch  !! The architecture of the kernel file.
      character(*) :: kertyp  !! The type of the kernel file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: file is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%getfat ( (file), arch, kertyp )
      end subroutine getfat

      !> Compute the outward-pointing, unit normal vector at a point on
      !> the surface of an ellipsoid.
      !>
      !> Legacy interface: calls ctx%surfnm with the legacy context.
      subroutine surfnm ( a, b, c, point, normal )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a  !! Length of the ellipsoid semi-axis along the X-axis.
      real(wp) :: b  !! Length of the ellipsoid semi-axis along the Y-axis.
      real(wp) :: c  !! Length of the ellipsoid semi-axis along the Z-axis.
      real(wp) :: point  ( 3 )  !! Body-fixed coordinates of a point on the ellipsoid.
      real(wp) :: normal ( 3 )  !! Outward pointing unit normal to ellipsoid at POINT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%surfnm ( a, b, c, (point), normal )
      end subroutine surfnm

      !> Locate the point on the surface of an ellipsoid that is nearest
      !> to a specified position. Also return the altitude of the position
      !> above the ellipsoid.
      !>
      !> Legacy interface: calls ctx%nearpt with the legacy context.
      subroutine nearpt ( positn, a, b, c, npoint, alt )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: positn ( 3 )  !! Position of a point in body-fixed frame.
      real(wp) :: a  !! Length of semi-axis parallel to x-axis.
      real(wp) :: b  !! Length of semi-axis parallel to y-axis.
      real(wp) :: c  !! Length on semi-axis parallel to z-axis.
      real(wp) :: npoint ( 3 )  !! Point on the ellipsoid closest to POSITN.
      real(wp) :: alt  !! Altitude of POSITN above the ellipsoid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: positn is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%nearpt ( (positn), a, b, c, npoint, alt )
      end subroutine nearpt

      !> Convert from rectangular coordinates to geodetic coordinates.
      !>
      !> Legacy interface: calls ctx%recgeo with the legacy context.
      subroutine recgeo ( rectan, re, f,  lon, lat, alt )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: lon  !! Geodetic longitude of the point (radians).
      real(wp) :: lat  !! Geodetic latitude  of the point (radians).
      real(wp) :: alt  !! Altitude of the point above reference spheroid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%recgeo ( (rectan), re, f, lon, lat, alt )
      end subroutine recgeo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvrtplt with the legacy context.
      subroutine zzvrtplt ( nv,    np,     plates, cellsz, maxlst, &
                            cells, vrtptr, nlist,  pltlst          )
      use pepper_kinds, only: ip
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: nv  !! Number of vertices.
      integer(ip) :: np  !! Number of plates.
      integer(ip) :: plates ( 3, * )  !! Plate array.
      integer(ip) :: cellsz  !! Cell array size.
      integer(ip) :: maxlst  !! Maximum size of output plate list.
      integer(ip) :: cells  ( 2, cellsz )  !! Workspace array for vertex list construction.
      integer(ip) :: vrtptr ( * )  !! Array of pointers, by vertex, into plate data list.
      integer(ip) :: nlist  !! Length of the plate data list.
      integer(ip) :: pltlst ( * )  !! Plate data list; first element is the number of plate IDs to
                                   !! follow.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzvrtplt ( ctx, nv, np, plates, cellsz, maxlst, cells, vrtptr, nlist, pltlst )
      end subroutine zzvrtplt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvoxcvo with the legacy context.
      subroutine zzvoxcvo ( vixyz, nvox, cgrscl, cgxyz, cgoff, cgof1d )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: vixyz  ( 3 )  !! Fine voxel integer coordinates.
      integer(ip) :: nvox   ( 3 )  !! Fine voxel grid dimensions.
      integer(ip) :: cgrscl  !! Coarse voxel scale.
      integer(ip) :: cgxyz  ( 3 )  !! Coarse voxel coordinates.
      integer(ip) :: cgoff  ( 3 )  !! 3-D voxel offset relative to coarse voxel.
      integer(ip) :: cgof1d  !! 1-D voxel offset relative to coarse voxel.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vixyz, nvox are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzvoxcvo ( ctx, (vixyz), (nvox), cgrscl, cgxyz, cgoff, cgof1d )
      end subroutine zzvoxcvo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvox2id with the legacy context.
      integer(ip) function zzvox2id ( vixyz, nvox )
      use pepper_kinds, only: ip
      implicit              none
      integer(ip) :: nvox   ( 3 )  !! Number of voxels in each coordinate direction.
      integer(ip) :: vixyz  ( 3 )  !! Voxel's grid coordinates stored in a three-dimensional vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzvox2id = pepper_zzvox2id ( ctx, vixyz, nvox )
      end function zzvox2id

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztrvlnk with the legacy context.
      subroutine zztrvlnk ( aval,  maxa, pntrs, cellsz, &
                            cells, maxb, nb,    blist  )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: aval  !! A-value index associated with pool entry.
      integer(ip) :: maxa  !! Size of PNTRS array.
      integer(ip) :: pntrs  ( * )  !! A-value pointer array.
      integer(ip) :: cellsz  !! Number of cells in cell array.
      integer(ip) :: cells  ( 2, * )  !! Cell array.
      integer(ip) :: maxb  !! Size of BLIST array.
      integer(ip) :: nb  !! Number of B values found.
      integer(ip) :: blist  ( * )  !! List of B values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zztrvlnk ( ctx, aval, maxa, pntrs, cellsz, cells, maxb, nb, blist )
      end subroutine zztrvlnk

      !> Untangle an unsorted AB linked-cell list into an A-index and
      !> associated B-list.
      !>
      !> Legacy interface: calls zz routine zzuntngl with the legacy context.
      subroutine zzuntngl ( nptr, maxcel, cells, &
                            maxb, pntrs,  nout,  outlst )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nptr  !! Length of A-list.
      integer(ip) :: maxcel  !! Size of cell list.
      integer(ip) :: cells  ( 2, * )  !! AB cell linked list.
      integer(ip) :: maxb  !! Maximum allowed dimension of B list.
      integer(ip) :: pntrs  ( * )  !! Pointer array.
      integer(ip) :: nout  !! Length of B-list.
      integer(ip) :: outlst ( * )  !! B-list.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzuntngl ( ctx, nptr, maxcel, cells, maxb, pntrs, nout, outlst )
      end subroutine zzuntngl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinilnk with the legacy context.
      subroutine zzinilnk ( maxp, maxc, ncell, pntrs, cells )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: maxp  !! Length of A-value array.
      integer(ip) :: maxc  !! Number of cells.
      integer(ip) :: ncell  !! Number of cells in use; initialized to zero.
      integer(ip) :: pntrs  ( * )  !! Initialized pointer array.
      integer(ip) :: cells  ( 2, * )  !! Initialized cell array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinilnk ( ctx, maxp, maxc, ncell, pntrs, cells )
      end subroutine zzinilnk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgetvox with the legacy context.
      subroutine zzgetvox ( voxsiz, voxori, nvox, xyz, inbox, voxcor )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: voxsiz  !! Voxel size in model units.
      real(wp) :: voxori    ( 3 )  !! 3-Vector location of the voxel grid origin in model units.
      integer(ip) :: nvox      ( 3 )  !! Number of voxel in each dimension.
      real(wp) :: xyz       ( 3 )  !! Model grid coordinates of point.
      logical :: inbox  !! Logical value is true if point is inside voxel grid, false if not.
      integer(ip) :: voxcor     ( 3 )  !! 3-Index of voxel location with the voxel grid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: nvox is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgetvox ( ctx, voxsiz, voxori, (nvox), xyz, inbox, voxcor )
      end subroutine zzgetvox

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzaddlnk with the legacy context.
      subroutine zzaddlnk ( aval,   bval,  maxa, &
                            cellsz, pntrs, ncell, cells )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: aval  !! A-value index associated with new pool entry.
      integer(ip) :: bval  !! B-value associated with input A value.
      integer(ip) :: maxa  !! Size of pointer array.
      integer(ip) :: cellsz  !! Number of cells in cell array.
      integer(ip) :: pntrs  ( * )  !! A-value pointer array to update.
      integer(ip) :: ncell  !! Number of cells in AB cell linked-list.
      integer(ip) :: cells  ( 2, * )  !! Cell array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzaddlnk ( ctx, aval, bval, maxa, cellsz, pntrs, ncell, cells )
      end subroutine zzaddlnk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzmkspin with the legacy context.
      subroutine zzmkspin ( np,     plates, vrtces, voxscl, cgscal, &
                            maxptr, mxcell, maxvxl, cells,  nvox, &
                            voxsiz, voxori, nvxtot, nvxptr, vxptr, &
                            nvxlst, vxlist, extent, cgrptr         )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: np  !! Number of plates.
      integer(ip) :: plates ( 3, * )  !! Array of plates.
      real(wp) :: vrtces ( 3, * )  !! Array of vertices.
      real(wp) :: voxscl  !! Voxel scaling factor.
      integer(ip) :: cgscal  !! Coarse voxel grid scaling factor.
      integer(ip) :: maxptr  !! Maximum voxel pointer list size.
      integer(ip) :: mxcell  !! Cell array column dimension.
      integer(ip) :: maxvxl  !! Maximum voxel-plate list size.
      integer(ip) :: cells   ( 2, mxcell )  !! Workspace array for vertex list construction.
      integer(ip) :: nvox   ( 3 )  !! Dimensions of voxel grid.
      real(wp) :: voxsiz  !! Size of a voxel in model units.
      real(wp) :: voxori ( 3 )  !! Origin of voxel grid in model units.
      integer(ip) :: nvxtot  !! Total number of voxels in grid.
      integer(ip) :: nvxptr  !! Number of pointers in VXPTR array.
      integer(ip) :: vxptr  ( * )  !! Pointer index to the voxel to plates list.
      integer(ip) :: nvxlst  !! Length of voxel to plates list.
      integer(ip) :: vxlist ( * )  !! Voxel to plates list.
      real(wp) :: extent ( 6 )  !! Vertex coordinate extents in km.
      integer(ip) :: cgrptr ( * )  !! Coarse voxel grid pointer array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzmkspin ( ctx, np, plates, vrtces, voxscl, cgscal, maxptr, mxcell, maxvxl, cells, nvox, voxsiz, voxori, nvxtot, nvxptr, vxptr, nvxlst, vxlist, extent, cgrptr )
      end subroutine zzmkspin

      !> Open a new DLA file and set the file type.
      !>
      !> Legacy interface: calls ctx%dlaopn with the legacy context.
      subroutine dlaopn ( fname, ftype, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      character(*) :: fname  !! Name of a DLA file to be opened.
      character(*) :: ftype  !! Mnemonic code for type of data in the DLA file.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: ncomch  !! Number of comment characters to allocate.
      integer(ip) :: handle  !! Handle assigned to the opened DLA file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlaopn ( fname, ftype, ifname, ncomch, handle )
      end subroutine dlaopn

      !> Begin a new segment in a DLA file.
      !>
      !> Legacy interface: calls ctx%dlabns with the legacy context.
      subroutine dlabns ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle of open DLA file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlabns ( handle )
      end subroutine dlabns

      !> End a new segment in a DLA file.
      !>
      !> Legacy interface: calls ctx%dlaens with the legacy context.
      subroutine dlaens ( handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle of open DLA file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlaens ( handle )
      end subroutine dlaens

      !> Begin a forward segment search in a DLA file.
      !>
      !> Legacy interface: calls ctx%dlabfs with the legacy context.
      subroutine dlabfs ( handle, dladsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle of open DLA file.
      integer(ip) :: dladsc  ( * )  !! Descriptor of first segment in DLA file.
      logical :: found  !! Flag indicating whether a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlabfs ( handle, dladsc, found )
      end subroutine dlabfs

      !> Begin a backward segment search in a DLA file.
      !>
      !> Legacy interface: calls ctx%dlabbs with the legacy context.
      subroutine dlabbs ( handle, dladsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle of open DLA file.
      integer(ip) :: dladsc  ( * )  !! Descriptor of last segment in DLA file.
      logical :: found  !! Flag indicating whether a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlabbs ( handle, dladsc, found )
      end subroutine dlabbs

      !> Find the segment following a specified segment in a DLA file.
      !>
      !> Legacy interface: calls ctx%dlafns with the legacy context.
      subroutine dlafns ( handle, dladsc, nxtdsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle of open DLA file.
      integer(ip) :: dladsc ( * )  !! Descriptor of a segment in DLA file.
      integer(ip) :: nxtdsc ( * )  !! Descriptor of next segment in DLA file.
      logical :: found  !! Flag indicating whether a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlafns ( handle, dladsc, nxtdsc, found )
      end subroutine dlafns

      !> Find the segment preceding a specified segment in a DLA file.
      !>
      !> Legacy interface: calls ctx%dlafps with the legacy context.
      subroutine dlafps ( handle, dladsc, prvdsc, found )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle of open DLA file.
      integer(ip) :: dladsc ( * )  !! Descriptor of a segment in DLA file.
      integer(ip) :: prvdsc ( * )  !! Descriptor of previous segment in DLA file.
      logical :: found  !! Flag indicating whether a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlafps ( handle, dladsc, prvdsc, found )
      end subroutine dlafps

      !> Return a logical value indicating whether a two DLA
      !> segments, each identified by DAS handle and DLA descriptor,
      !> are in fact the same segment.
      !>
      !> Legacy interface: calls ctx%dlassg with the legacy context.
      logical function dlassg ( han1, han2, dsc1, dsc2 )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: han1  !! Handle of an open DLA file.
      integer(ip) :: han2  !! Handle of a second open DLA file.
      integer(ip) :: dsc1 ( dladsz )  !! DLA descriptor of a segment in the first file.
      integer(ip) :: dsc2 ( dladsz )  !! DLA descriptor of a segment in the second file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      dlassg = ctx%dlassg ( han1, han2, dsc1, dsc2 )
      end function dlassg

      !> Open a new DSK file for subsequent write operations.
      !>
      !> Legacy interface: calls ctx%dskopn with the legacy context.
      subroutine dskopn ( fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      character(*) :: fname  !! Name of a DSK file to be opened.
      character(*) :: ifname  !! Internal file name.
      integer(ip) :: ncomch  !! Number of comment characters to allocate.
      integer(ip) :: handle  !! Handle assigned to the opened DSK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskopn ( fname, ifname, ncomch, handle )
      end subroutine dskopn

      !> Close a DSK file.
      !>
      !> Legacy interface: calls ctx%dskcls with the legacy context.
      subroutine dskcls ( handle, optmiz )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      implicit none
      integer(ip) :: handle  !! Handle assigned to the opened DSK file.
      logical :: optmiz  !! Flag indicating whether to segregate the DSK.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskcls ( handle, optmiz )
      end subroutine dskcls

      !> Retrieve the value of a specified DSK tolerance or margin
      !> parameter.
      !>
      !> Legacy interface: calls ctx%dskgtl with the legacy context.
      subroutine dskgtl ( keywrd, dpval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      integer(ip) :: keywrd  !! Code specifying parameter to retrieve.
      real(wp) :: dpval  !! Value of parameter.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskgtl ( keywrd, dpval )
      end subroutine dskgtl

      !> Set the value of a specified DSK tolerance or margin parameter.
      !>
      !> Legacy interface: calls ctx%dskstl with the legacy context.
      subroutine dskstl ( keywrd, dpval )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      integer(ip) :: keywrd  !! Code specifying parameter to modify.
      real(wp) :: dpval  !! New value of parameter.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskstl ( keywrd, dpval )
      end subroutine dskstl

      !> Return the DSK descriptor from a DSK segment identified
      !> by a DAS handle and DLA descriptor.
      !>
      !> Legacy interface: calls ctx%dskgd with the legacy context.
      subroutine dskgd ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of a DSK file.
      integer(ip) :: dladsc ( * )  !! DLA segment descriptor.
      real(wp) :: dskdsc ( * )  !! DSK segment descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskgd ( handle, dladsc, dskdsc )
      end subroutine dskgd

      !> Return bookkeeping data from a DSK type 2 segment.
      !>
      !> Legacy interface: calls ctx%dskb02 with the legacy context.
      subroutine dskb02 ( handle, dladsc, nv,     np,     nvxtot, &
                          vtxbds, voxsiz, voxori, vgrext, &
                          cgscal, vtxnpl, voxnpt, voxnpl          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: nv  !! Number of vertices in model.
      integer(ip) :: np  !! Number of plates in model.
      integer(ip) :: nvxtot  !! Number of voxels in fine grid.
      real(wp) :: vtxbds ( 2, 3 )  !! Vertex bounds.
      real(wp) :: voxsiz  !! Fine voxel edge length.
      real(wp) :: voxori ( 3 )  !! Fine voxel grid origin.
      integer(ip) :: vgrext ( 3 )  !! Fine voxel grid extent.
      integer(ip) :: cgscal  !! Coarse voxel grid scale.
      integer(ip) :: vtxnpl  !! Size of vertex-plate correspondence list.
      integer(ip) :: voxnpt  !! Size of voxel-plate pointer list.
      integer(ip) :: voxnpl  !! Size of voxel-plate correspondence list.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskb02 ( handle, dladsc, nv, np, nvxtot, vtxbds, voxsiz, voxori, vgrext, cgscal, vtxnpl, voxnpt, voxnpl )
      end subroutine dskb02

      !> Fetch double precision data from a type 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dskd02 with the legacy context.
      subroutine dskd02 ( handle, dladsc, item, start, room, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: item  !! Keyword identifying item to fetch.
      integer(ip) :: start  !! Start index.
      integer(ip) :: room  !! Amount of room in output array.
      integer(ip) :: n  !! Number of values returned.
      real(wp) :: values ( * )  !! Array containing requested item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskd02 ( handle, dladsc, item, start, room, n, values )
      end subroutine dskd02

      !> Fetch integer data from a type 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dski02 with the legacy context.
      subroutine dski02 ( handle, dladsc, item, start, room, n, values )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: item  !! Keyword identifying item to fetch.
      integer(ip) :: start  !! Start index.
      integer(ip) :: room  !! Amount of room in output array.
      integer(ip) :: n  !! Number of values returned.
      integer(ip) :: values ( * )  !! Array containing requested item.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dski02 ( handle, dladsc, item, start, room, n, values )
      end subroutine dski02

      !> Compute the unit normal vector for a specified plate from a type
      !> 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dskn02 with the legacy context.
      subroutine dskn02 ( handle, dladsc, plid, normal )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: plid  !! Plate ID.
      real(wp) :: normal ( 3 )  !! Plate's unit normal vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskn02 ( handle, dladsc, plid, normal )
      end subroutine dskn02

      !> Fetch triangular plates from a type 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dskp02 with the legacy context.
      subroutine dskp02 ( handle, dladsc, start, room, n, plates )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: start  !! Start index.
      integer(ip) :: room  !! Amount of room in output array.
      integer(ip) :: n  !! Number of plates returned.
      integer(ip) :: plates ( 3, * )  !! Array containing plates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskp02 ( handle, dladsc, start, room, n, plates )
      end subroutine dskp02

      !> Fetch vertices from a type 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dskv02 with the legacy context.
      subroutine dskv02 ( handle, dladsc, start, room, n, vrtces )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: start  !! Start index.
      integer(ip) :: room  !! Amount of room in output array.
      integer(ip) :: n  !! Number of vertices returned.
      real(wp) :: vrtces ( 3, * )  !! Array containing vertices.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskv02 ( handle, dladsc, start, room, n, vrtces )
      end subroutine dskv02

      !> Return plate model size parameters---plate count and
      !> vertex count---for a type 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dskz02 with the legacy context.
      subroutine dskz02 ( handle, dladsc, nv, np )
      use pepper_kinds, only: ip
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor.
      integer(ip) :: nv  !! Number of vertices.
      integer(ip) :: np  !! Number of plates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskz02 ( handle, dladsc, nv, np )
      end subroutine dskz02

      !> Make spatial index for a DSK type 2 segment. The index is
      !> returned as a pair of arrays, one of type INTEGER and one of type
      !> DOUBLE PRECISION. These arrays are suitable for use with the DSK
      !> type 2 writer DSKW02.
      !>
      !> Legacy interface: calls ctx%dskmi2 with the legacy context.
      subroutine dskmi2 ( nv,     vrtces, np,     plates, finscl, &
                          corscl, worksz, voxpsz, voxlsz, makvtl, &
                          spxisz, work,   spaixd, spaixi         )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: nv  !! Number of vertices.
      real(wp) :: vrtces ( 3, * )  !! Vertices.
      integer(ip) :: np  !! Number of plates.
      integer(ip) :: plates ( 3, * )  !! Plates.
      real(wp) :: finscl  !! Fine voxel scale.
      integer(ip) :: corscl  !! Coarse voxel scale.
      integer(ip) :: worksz  !! Workspace size.
      integer(ip) :: voxpsz  !! Voxel-plate pointer array size.
      integer(ip) :: voxlsz  !! Voxel-plate list array size.
      logical :: makvtl  !! Vertex-plate list flag.
      integer(ip) :: spxisz  !! Spatial index integer component size.
      integer(ip) :: work   ( 2, worksz )  !! Workspace.
      real(wp) :: spaixd ( ixdfix )  !! Double precision component of spatial index.
      integer(ip) :: spaixi ( spxisz )  !! Integer component of spatial index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskmi2 ( nv, vrtces, np, plates, finscl, corscl, worksz, voxpsz, voxlsz, makvtl, spxisz, work, spaixd, spaixi )
      end subroutine dskmi2

      !> Determine range bounds for a set of triangular plates to
      !> be stored in a type 2 DSK segment.
      !>
      !> Legacy interface: calls ctx%dskrb2 with the legacy context.
      subroutine dskrb2 ( nv,     vrtces, np,     plates, &
                          corsys, corpar, mncor3, mxcor3 )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: nv  !! Number of vertices.
      real(wp) :: vrtces ( 3, * )  !! Vertices.
      integer(ip) :: np  !! Number of plates.
      integer(ip) :: plates ( 3, * )  !! Plates.
      integer(ip) :: corsys  !! DSK coordinate system code.
      real(wp) :: corpar ( * )  !! DSK coordinate system parameters.
      real(wp) :: mncor3  !! Lower bound on range of third coordinate.
      real(wp) :: mxcor3  !! Upper bound on range of third coordinate.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskrb2 ( nv, vrtces, np, plates, corsys, corpar, mncor3, mxcor3 )
      end subroutine dskrb2

      !> Find the set of body ID codes of all objects for which
      !> topographic data are provided in a specified DSK file.
      !>
      !> Legacy interface: calls ctx%dskobj with the legacy context.
      subroutine dskobj ( dskfnm, bodids )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      character(*) :: dskfnm  !! Name of DSK file.
      integer(ip) :: bodids ( lbcell : * )  !! Set of ID codes of objects in DSK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskobj ( dskfnm, bodids )
      end subroutine dskobj

      !> Find the set of surface ID codes for all surfaces associated with
      !> a given body in a specified DSK file.
      !>
      !> Legacy interface: calls ctx%dsksrf with the legacy context.
      subroutine dsksrf ( dskfnm, bodyid, srfids )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      character(*) :: dskfnm  !! Name of DSK file.
      integer(ip) :: bodyid  !! Integer body ID code.
      integer(ip) :: srfids ( lbcell : * )  !! Set of ID codes of surfaces in DSK file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dsksrf ( dskfnm, bodyid, srfids )
      end subroutine dsksrf

end module pepper_legacy_dsk_files
