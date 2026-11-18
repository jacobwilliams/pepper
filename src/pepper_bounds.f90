!> The bounds and sizes of SPICELIB's arrays of fixed layout, which size
!> the arguments of the routines that take them: cells and windows,
!> linked-list pools, planes, ellipses, TTRANS's time vectors and SPK type 1
!> difference lines. (The sizes of DAS records, NWD and NWI, are in
!> [[pepper_inc_das]].)
module pepper_bounds

   use pepper_kinds, only: ip

   implicit none

   private

   integer(ip), parameter, public :: lbcell = -5  !! lower bound of a cell or window: elements `lbcell:0` are its
                                                  !! control area (size and cardinality), the data start at 1
   integer(ip), parameter, public :: lbpool = -5  !! lower bound of a doubly linked list pool, `pool(2, lbpool:size)`:
                                                  !! nodes `lbpool:0` are its control area ([[lnkini]])
   integer(ip), parameter, public :: ubpl = 4  !! number of elements of a plane: the unit normal vector and the
                                               !! constant
   integer(ip), parameter, public :: ubel = 9  !! number of elements of an ellipse: the center and the semi-major
                                               !! and semi-minor axes
   integer(ip), parameter, public :: mxcomp = 10  !! number of components of a time vector of [[ttrans]] (the
                                                  !! most any representation uses)
   integer(ip), parameter, public :: dlsize = 71  !! number of elements of a difference line of an SPK type 1
                                                  !! segment ([[spkw01]])

end module pepper_bounds
