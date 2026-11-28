!> Prepared queries (PEPPER): a state or a frame transformation resolved
!> once (names to ID codes, the aberration correction to its flags), then
!> evaluated at any number of epochs.
!>
!> ```fortran
!> type(state_query) :: q
!> call ctx%prepare_state(q, 'MOON', 'EARTH', 'J2000', 'LT+S')
!> call ctx%query_state(q, et, state, lt)          ! one epoch
!> call ctx%query_state(q, ets, states, lts)       ! many: states(6, n)
!>
!> type(position_query) :: p
!> call ctx%prepare_position(p, 'MOON', 'EARTH', 'J2000', 'LT+S')
!> call ctx%query_position(p, et, pos, lt)         ! pos(3)
!> call ctx%query_position(p, ets, positions, lts) ! positions(3, n)
!> ```
!>
!> A query also keeps what makes repeated evaluations fast (the SPK
!> segments it used and their current Chebyshev records), so it is used by
!> one thread at a time, like a context; each thread prepares its own (or
!> copies a prepared one). The results are the same, to the last bit, as
!> those of SPKEZR, SPKPOS, PXFORM and SXFORM with the same arguments.
!>
!> The components are internal: set them with the context's `prepare_*`
!> procedures only.
module pepper_query_types

   use pepper_kinds, only: ip, wp

   implicit none

   private

   public :: state_query, position_query, rotation_query, spk_segment_cache

   !> An SPK segment used by a prepared state query, with its current
   !> record (segment types 2 and 3: Chebyshev polynomials).
   type :: spk_segment_cache
      integer(ip) :: handle = 0  !! the file's handle
      real(wp) :: descr(5) = 0.0_wp  !! the segment descriptor
      integer(ip) :: type = 0  !! the segment type
      integer(ip) :: center = 0  !! the center of motion
      integer(ip) :: frame = 0  !! the frame of the states
      integer(ip) :: begin = 0  !! address of the segment's first word (types 2 and 3: of the first record)
      integer(ip) :: last = 0  !! address of the segment's last word
      real(wp) :: init = 0.0_wp  !! start of the first record's interval (types 2 and 3)
      real(wp) :: intlen = 0.0_wp  !! length of a record's interval (types 2 and 3)
      real(wp) :: rsize = 0.0_wp  !! record size, as stored in the segment (types 2 and 3)
      integer(ip) :: recsiz = 0  !! record size (types 2 and 3)
      integer(ip) :: nrec = 0  !! number of records (types 2 and 3)
      integer(ip) :: recno = 0  !! the record in RECORD (0: none)
      real(wp), allocatable :: record(:)  !! the record, as SPKR02/SPKR03 return it
   end type spk_segment_cache

   !> A prepared state query: the state of a target relative to an observer
   !> in a frame, with an aberration correction (SPKEZR's arguments but the
   !> epoch).
   type :: state_query
      logical :: prepared = .false.  !! set by prepare_state
      integer(ip) :: target = 0  !! the target's ID code
      integer(ip) :: observer = 0  !! the observer's ID code
      integer(ip) :: frame = 0  !! the frame's ID code
      integer(ip) :: frame_center = 0  !! the frame's center
      logical :: inertial = .false.  !! the frame is inertial
      character(:), allocatable :: frame_name  !! the frame's name, as given
      character(:), allocatable :: abcorr  !! the aberration correction, as given
      logical :: geometric = .true.  !! no aberration correction
      logical :: xmit = .false.  !! transmission (not reception) correction
      logical :: usestl = .false.  !! stellar aberration correction
      logical :: uselt = .false.  !! light time correction
      logical :: usecn = .false.  !! converged Newtonian light time
      logical :: fallback = .false.  !! evaluate with SPKEZ (corrections the fast path does not cover)
      type(spk_segment_cache), allocatable :: segs(:)  !! the segments used so far
      integer(ip) :: nsegs = 0  !! number of segments in SEGS
   end type state_query

   !> A prepared position query: the position of a target relative to an
   !> observer in a frame, with an aberration correction (SPKPOS's
   !> arguments but the epoch). It is evaluated as SPKPOS evaluates it
   !> (SPKGPS, SPKAPO), which differs from SPKEZR's light time and stellar
   !> aberration corrections: its positions are not always the first three
   !> components of SPKEZR's state.
   type :: position_query
      !> the target, observer, frame and correction (flags with SPKAPO's
      !> meaning: USELT one light time iteration, USECN three), and the
      !> segment cache
      type(state_query) :: s
   end type position_query

   !> A prepared frame transformation query (PXFORM's and SXFORM's
   !> arguments but the epoch).
   type :: rotation_query
      logical :: prepared = .false.  !! set by prepare_rotation
      integer(ip) :: from = 0  !! the ID code of the frame transformed from
      integer(ip) :: to = 0  !! the ID code of the frame transformed to
      logical :: constant = .false.  !! the transformation does not depend on time (inertial and fixed TK frames only)
      logical :: have_rotation = .false.  !! ROTATION holds the constant rotation
      real(wp) :: rotation(3, 3) = 0.0_wp  !! the constant rotation, once computed
      integer(ip) :: poolctr(2) = 0  !! the kernel pool's state when ROTATION was computed
   end type rotation_query

end module pepper_query_types
