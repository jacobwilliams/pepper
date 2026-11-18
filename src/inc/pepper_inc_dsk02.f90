!> This include file declares parameters for DSK data type 2 (plate model).
!>
!>     Each type 2 DSK segment has integer, d.p., and character
!>     components.  The segment layout in DAS address space is as
!>     follows:
!>
!>## Version
!>
!>-       SPICELIB Version 1.0.0 08-FEB-2017 (NJB)
!>
!>          Updated version info.
!>
!>           22-JAN-2016 (NJB)
!>
!>              Now includes spatial index parameters.
!>
!>           26-MAR-2015 (NJB)
!>
!>              Updated to increase MAXVRT to 16000002. MAXNPV
!>              has been changed to (3/2)*MAXPLT. Set MAXVOX
!>              to 100000000.
!>
!>           13-MAY-2010 (NJB)
!>
!>              Updated to reflect new no-record design.
!>
!>           04-MAY-2010 (NJB)
!>
!>              Updated for new type 2 segment design. Now uses
!>              a local parameter to represent DSK descriptor
!>              size (NB).
!>
!>           13-SEP-2008 (NJB)
!>
!>              Updated to remove albedo information.
!>              Updated to use parameter for DSK descriptor size.
!>
!>           27-DEC-2006 (NJB)
!>
!>              Updated to remove minimum and maximum radius information
!>              from segment layout.  These bounds are now included
!>              in the segment descriptor.
!>
!>           26-OCT-2006 (NJB)
!>
!>              Updated to remove normal, center, longest side, albedo,
!>              and area keyword parameters.
!>
!>           04-AUG-2006 (NJB)
!>
!>              Updated to support coarse voxel grid.  Area data
!>              have now been removed.
!>
!>           10-JUL-2006 (NJB)
!>
!>        Integer layout:
module pepper_inc_dsk02

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: ixnv = 1  !! Integer layout:
                                       !!
                                       !! ```
                                       !! +-----------------+
                                       !! | NV              |  (# of vertices)
                                       !! +-----------------+
                                       !! | NP              |  (# of plates )
                                       !! +-----------------+
                                       !! | NVXTOT          |  (total number of voxels)
                                       !! +-----------------+
                                       !! | VGREXT          |  (voxel grid extents, 3 integers)
                                       !! +-----------------+
                                       !! | CGRSCL          |  (coarse voxel grid scale, 1 integer)
                                       !! +-----------------+
                                       !! | VOXNPT          |  (size of voxel-plate pointer list)
                                       !! +-----------------+
                                       !! | VOXNPL          |  (size of voxel-plate list)
                                       !! +-----------------+
                                       !! | VTXNPL          |  (size of vertex-plate list)
                                       !! +-----------------+
                                       !! | PLATES          |  (NP 3-tuples of vertex IDs)
                                       !! +-----------------+
                                       !! | VOXPTR          |  (voxel-plate pointer array)
                                       !! +-----------------+
                                       !! | VOXPLT          |  (voxel-plate list)
                                       !! +-----------------+
                                       !! | VTXPTR          |  (vertex-plate pointer array)
                                       !! +-----------------+
                                       !! | VTXPLT          |  (vertex-plate list)
                                       !! +-----------------+
                                       !! | CGRPTR          |  (coarse grid occupancy pointers)
                                       !! +-----------------+
                                       !! ```

   integer(ip), parameter :: ixnp = ixnv + 1

   integer(ip), parameter :: ixnvxt = ixnp + 1

   integer(ip), parameter :: ixvgrx = ixnvxt + 1

   integer(ip), parameter :: ixcgsc = ixvgrx + 3

   integer(ip), parameter :: ixvxps = ixcgsc + 1

   integer(ip), parameter :: ixvxls = ixvxps + 1

   integer(ip), parameter :: ixvtls = ixvxls + 1

   integer(ip), parameter :: ixplat = ixvtls + 1

   integer(ip), parameter :: ixdscr = 1  !! D.p. layout:
                                         !!
                                         !! ```
                                         !! +-----------------+
                                         !! | DSK descriptor  |  DSKDSZ elements
                                         !! +-----------------+
                                         !! | Vertex bounds   |  6 values (min/max for each component)
                                         !! +-----------------+
                                         !! | Voxel origin    |  3 elements
                                         !! +-----------------+
                                         !! | Voxel size      |  1 element
                                         !! +-----------------+
                                         !! | Vertices        |  3*NV elements
                                         !! +-----------------+
                                         !! ```

   integer(ip), parameter :: dscsz2 = 24  !! This local parameter MUST be kept consistent with
                                          !! the parameter DSKDSZ which is declared in dskdsc.inc.

   integer(ip), parameter :: ixvtbd = ixdscr + dscsz2

   integer(ip), parameter :: ixvxor = ixvtbd + 6

   integer(ip), parameter :: ixvxsz = ixvxor + 3

   integer(ip), parameter :: ixvert = ixvxsz + 1

   integer(ip), parameter :: kwnv = 1  !! Integer item keyword parameters used by fetch routines:

   integer(ip), parameter :: kwnp = kwnv + 1

   integer(ip), parameter :: kwnvxt = kwnp + 1

   integer(ip), parameter :: kwvgrx = kwnvxt + 1

   integer(ip), parameter :: kwcgsc = kwvgrx + 1

   integer(ip), parameter :: kwvxps = kwcgsc + 1

   integer(ip), parameter :: kwvxls = kwvxps + 1

   integer(ip), parameter :: kwvtls = kwvxls + 1

   integer(ip), parameter :: kwplat = kwvtls + 1

   integer(ip), parameter :: kwvxpt = kwplat + 1

   integer(ip), parameter :: kwvxpl = kwvxpt + 1

   integer(ip), parameter :: kwvtpt = kwvxpl + 1

   integer(ip), parameter :: kwvtpl = kwvtpt + 1

   integer(ip), parameter :: kwcgpt = kwvtpl + 1

   integer(ip), parameter :: kwdsc = kwcgpt + 1  !! Double precision item keyword parameters used by fetch routines:

   integer(ip), parameter :: kwvtbd = kwdsc + 1

   integer(ip), parameter :: kwvxor = kwvtbd + 1

   integer(ip), parameter :: kwvxsz = kwvxor + 1

   integer(ip), parameter :: kwvert = kwvxsz + 1

   integer(ip), parameter :: maxvrt = 16000002  !! The parameters below formerly were declared in pltmax.inc.
                                                !!
                                                !! Limits on plate model capacity:
                                                !!
                                                !! The maximum number of bodies, vertices and
                                                !! plates in a plate model or collective thereof are
                                                !! provided here.
                                                !!
                                                !! These values can be used to dimension arrays, or to
                                                !! use as limit checks.
                                                !!
                                                !! The value of MAXPLT is determined from MAXVRT via
                                                !! Euler's Formula for simple polyhedra having triangular
                                                !! faces.
                                                !!
                                                !! MAXVRT is the maximum number of vertices the triangular
                                                !! plate model software will support.

   integer(ip), parameter :: maxplt = 2*(maxvrt - 2)  !! MAXPLT is the maximum number of plates that the triangular
                                                      !! plate model software will support.

   integer(ip), parameter :: maxnpv = 3*maxplt/2 + 1  !! MAXNPV is the maximum allowed number of vertices, not taking into
                                                      !! account shared vertices.
                                                      !!
                                                      !! Note that this value is not sufficient to create a vertex-plate
                                                      !! mapping for a model of maximum plate count.

   integer(ip), parameter :: maxvox = 100000000  !! MAXVOX is the maximum number of voxels.

   integer(ip), parameter :: maxcgr = 100000  !! MAXCGR is the maximum size of the coarse voxel grid.

   integer(ip), parameter :: maxedg = 120  !! MAXEDG is the maximum allowed number of vertex or plate
                                           !! neighbors a vertex may have.

   integer(ip), parameter :: sivgrx = 1  !! DSK type 2 spatial index parameters
                                         !! ===================================
                                         !!
                                         !!     DSK type 2 spatial index integer component
                                         !!     ------------------------------------------
                                         !!
                                         !!        +-----------------+
                                         !!        | VGREXT          |  (voxel grid extents, 3 integers)
                                         !!        +-----------------+
                                         !!        | CGRSCL          |  (coarse voxel grid scale, 1 integer)
                                         !!        +-----------------+
                                         !!        | VOXNPT          |  (size of voxel-plate pointer list)
                                         !!        +-----------------+
                                         !!        | VOXNPL          |  (size of voxel-plate list)
                                         !!        +-----------------+
                                         !!        | VTXNPL          |  (size of vertex-plate list)
                                         !!        +-----------------+
                                         !!        | CGRPTR          |  (coarse grid occupancy pointers)
                                         !!        +-----------------+
                                         !!        | VOXPTR          |  (voxel-plate pointer array)
                                         !!        +-----------------+
                                         !!        | VOXPLT          |  (voxel-plate list)
                                         !!        +-----------------+
                                         !!        | VTXPTR          |  (vertex-plate pointer array)
                                         !!        +-----------------+
                                         !!        | VTXPLT          |  (vertex-plate list)
                                         !!        +-----------------+
                                         !!
                                         !!     Index parameters
                                         !!
                                         !! Grid extent:
   integer(ip), parameter :: sicgsc = sivgrx + 3  !! Coarse grid scale:
   integer(ip), parameter :: sivxnp = sicgsc + 1  !! Voxel pointer count:
   integer(ip), parameter :: sivxnl = sivxnp + 1  !! Voxel-plate list count:
   integer(ip), parameter :: sivtnl = sivxnl + 1  !! Vertex-plate list count:
   integer(ip), parameter :: sicgrd = sivtnl + 1  !! Coarse grid pointers:

   integer(ip), parameter :: ixifix = maxcgr + 7  !! Size of fixed-size portion of integer component:

   integer(ip), parameter :: sivtbd = 1  !!    DSK type 2 spatial index double precision component
                                         !!     ---------------------------------------------------
                                         !!
                                         !!        +-----------------+
                                         !!        | Vertex bounds   |  6 values (min/max for each component)
                                         !!        +-----------------+
                                         !!        | Voxel origin    |  3 elements
                                         !!        +-----------------+
                                         !!        | Voxel size      |  1 element
                                         !!        +-----------------+
                                         !!
                                         !!     Index parameters
                                         !!
                                         !! Vertex bounds:
   integer(ip), parameter :: sivxor = sivtbd + 6  !! Voxel grid origin:
   integer(ip), parameter :: sivxsz = sivxor + 3  !! Voxel size:

   integer(ip), parameter :: ixdfix = 10  !! Size of fixed-size portion of double precision component:

!
!     The limits below are used to define a suggested maximum
!     size for the integer component of the spatial index.
!

   integer(ip), parameter :: maxvxp = maxplt / 2  !! Maximum number of entries in voxel-plate pointer array:

   integer(ip), parameter :: maxcel = 60000000  !! Maximum cell size:

   integer(ip), parameter :: mxnvls = maxcel + maxvxp/2  !! Maximum number of entries in voxel-plate list:

   integer(ip), parameter :: spaisz = ixifix + maxvxp &
      + mxnvls + maxvrt &
      + maxnpv
      !! Spatial index integer component size:

end module pepper_inc_dsk02
