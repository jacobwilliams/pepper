!> Prepared queries (PEPPER): states and frame transformations resolved once
!> and evaluated at any number of epochs ([[pepper_query_types]]).
!>
!> A state query gives the result of SPKEZR, to the last bit, with less
!> work per epoch:
!>
!> - the names, the aberration correction and the frame are resolved when
!>   the query is prepared, not at every call;
!> - the chain of segments from the target is found from the segments'
!>   descriptors before any is evaluated, so only the links that the
!>   result needs are evaluated (SPKGEO evaluates the target's whole chain
!>   to the solar system barycenter);
!> - for SPK types 2 and 3 (Chebyshev polynomials: the planetary
!>   ephemerides) the query keeps each segment's current record, and reads
!>   a record only when the epoch moves to another;
!> - the light time and stellar aberration corrections are SPKLTC, SPKAPS,
!>   SPKACS and SPKEZ, statement for statement, on these states.
!>
!> The arithmetic is SPICELIB's, in the same order, so the results are
!> identical. The routines are recursive: a programmatic provider evaluated
!> by a query may make queries of its own (pepper_programmatic). Cases outside the fast path (a chain longer than SPKGEO's 20
!> links, missing data, the relativistic correction) are evaluated by the
!> SPICELIB routines, with their errors.
!>
!> A position query gives the result of SPKPOS the same way, with SPKPOS's
!> arithmetic (SPKGPS, SPKAPO, SPKEZP's branch for non-inertial frames).
!> It evaluates no velocities that the result does not use: for types 2
!> and 3 only the position series, and position-only frame rotations
!> (REFCHG, not FRMCHG).
submodule (pepper_instance_module) pepper_queries

   implicit none

   integer(ip), parameter :: chlen = 20  !! SPKGEO's chain length
   integer(ip), parameter :: ssb = 0  !! the solar system barycenter
   integer(ip), parameter :: max_cached_segments = 64  !! a query forgets its segments past this number

contains

   !> Prepare a state query (PEPPER; [[pepper_query_types]]): the state of
   !> a target relative to an observer in a frame, with an aberration
   !> correction, as [[spkezr]], resolved once for evaluation at any number
   !> of epochs with `query_state`.
   !>
   !> The errors are those of [[spkezr]] for these arguments (unknown
   !> names, an invalid correction, an unknown frame).
   module subroutine prepare_state ( ctx, q, target, observer, frame, abcorr )
      use pepper_inc_frmtyp, only: inertl
      use pepper_inc_zzabcorr, only: nabcor, geoidx, ltidx, stlidx, cnvidx, xmtidx, relidx
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(out) :: q  !! the query
      character(*), intent(in) :: target  !! the target's name or ID code
      character(*), intent(in) :: observer  !! the observer's name or ID code
      character(*), intent(in) :: frame  !! the frame of the state
      character(*), intent(in) :: abcorr  !! the aberration correction, as for [[spkezr]]
      logical :: attblk(nabcor)
      logical :: found
      integer(ip) :: cent
      integer(ip) :: class
      integer(ip) :: clssid
      q = state_query()
      if ( ctx%failed() ) return
      call bods2c ( ctx, target, q%target, found )
      if ( ctx%failed() ) return
      if ( .not. found ) then
         call ctx%setmsg ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. ' &
         //            'Alternatively you may call SPKEZ ' &
         //            'directly if you know the SPICE ID codes ' &
         //            'for both ''#'' and ''#'' '                )
         call ctx%errch  ( '#', target                                )
         call ctx%errch  ( '#', target                                )
         call ctx%errch  ( '#', observer                              )
         call ctx%sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         return
      end if
      call bods2c ( ctx, observer, q%observer, found )
      if ( ctx%failed() ) return
      if ( .not. found ) then
         call ctx%setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. ' &
         //            'Alternatively you may call SPKEZ ' &
         //            'directly if you know the SPICE ID codes ' &
         //            'for both ''#'' and ''#'' '                )
         call ctx%errch  ( '#', observer                              )
         call ctx%errch  ( '#', target                                )
         call ctx%errch  ( '#', observer                              )
         call ctx%sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         return
      end if
      call zzvalcor ( ctx, abcorr, attblk )
      if ( ctx%failed() ) return
      q%geometric = attblk(geoidx)
      q%uselt     = attblk(ltidx)
      q%usestl    = attblk(stlidx)
      q%usecn     = attblk(cnvidx)
      q%xmit      = attblk(xmtidx)
      q%fallback  = attblk(relidx)
      call namfrm ( ctx, frame, q%frame )
      if ( ctx%failed() ) return
      if ( q%frame == 0 ) then
         call ctx%setmsg ( 'The requested output frame ''#'' is ' &
         //            'not recognized by the reference frame ' &
         //            'subsystem. Please check that the ' &
         //            'appropriate kernels have been loaded ' &
         //            'and that you have correctly entered ' &
         //            'the name of the output frame. ' )
         call ctx%errch  ( '#', frame )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return
      end if
      call frinfo ( ctx, q%frame, cent, class, clssid, found )
      if ( ctx%failed() ) return
      if ( .not. found ) then
         call ctx%setmsg ( 'The requested output frame ''#'' is ' &
         //            'not recognized by the reference frame ' &
         //            'subsystem. Please check that the ' &
         //            'appropriate kernels have been loaded ' &
         //            'and that you have correctly entered ' &
         //            'the name of the output frame. ' )
         call ctx%errch  ( '#', frame )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME2)' )
         return
      end if
      q%frame_center = cent
      q%inertial     = class == inertl
      q%frame_name   = frame
      q%abcorr       = abcorr
      q%prepared     = .true.
   end subroutine prepare_state

   !> The state of a prepared query at one epoch (PEPPER): [[spkezr]]'s
   !> result, to the last bit. Bound as the generic `query_state`.
   recursive module subroutine query_state_one ( ctx, q, et, state, lt )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query (its segment cache is updated)
      real(wp), intent(in) :: et  !! the epoch (TDB seconds past J2000)
      real(wp), intent(out) :: state(6)  !! the state of the target relative to the observer
      real(wp), intent(out) :: lt  !! the one-way light time between them
      real(wp) :: dlt
      if ( ctx%failed() ) return
      if ( .not. q%prepared ) then
         call ctx%setmsg ( 'The state query has not been prepared ' &
         //            '(PREPARE_STATE).' )
         call ctx%sigerr ( 'SPICE(NOTINITIALIZED)' )
         return
      end if
      if ( q%nsegs > max_cached_segments ) q%nsegs = 0
      if ( q%fallback ) then
         call spkez ( ctx, q%target, et, q%frame_name, q%abcorr, q%observer, state, lt )
      else if ( q%geometric ) then
         call fast_geo ( ctx, q, q%target, et, q%frame, q%frame_name, q%observer, state, lt )
      else if ( q%inertial ) then
         call fast_acs ( ctx, q, q%target, et, q%frame, q%frame_name, q%observer, state, lt, dlt )
      else
         call fast_noninertial ( ctx, q, et, state, lt )
      end if
   end subroutine query_state_one

   !> The states of a prepared query at many epochs (PEPPER). Bound as the
   !> generic `query_state`.
   recursive module subroutine query_state_batch ( ctx, q, ets, states, lts )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query (its segment cache is updated)
      real(wp), intent(in) :: ets(:)  !! the epochs (TDB seconds past J2000)
      real(wp), intent(out) :: states(:,:)  !! the states, shape (6, size(ets)) at least
      real(wp), intent(out), optional :: lts(:)  !! the light times
      real(wp) :: lt
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( size(states, 1) /= 6 .or. size(states, 2) < size(ets) ) then
         call ctx%setmsg ( 'The states array has shape (#, #); ' &
         //            '(6, #) is needed.' )
         call ctx%errint ( '#', size(states, 1, ip) )
         call ctx%errint ( '#', size(states, 2, ip) )
         call ctx%errint ( '#', size(ets, kind=ip) )
         call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)' )
         return
      end if
      if ( present(lts) ) then
         if ( size(lts) < size(ets) ) then
            call ctx%setmsg ( 'The light time array has # elements; ' &
            //            '# are needed.' )
            call ctx%errint ( '#', size(lts, kind=ip) )
            call ctx%errint ( '#', size(ets, kind=ip) )
            call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)' )
            return
         end if
      end if
      do k = 1, size(ets, kind=ip)
         call query_state_one ( ctx, q, ets(k), states(:, k), lt )
         if ( ctx%failed() ) return
         if ( present(lts) ) lts(k) = lt
      end do
   end subroutine query_state_batch

   !> Prepare a position query (PEPPER; [[pepper_query_types]]): the
   !> position of a target relative to an observer in a frame, with an
   !> aberration correction, as [[spkpos]], resolved once for evaluation at
   !> any number of epochs with `query_position`.
   !>
   !> The corrections are SPKPOS's ('NONE', 'LT', 'LT+S', 'CN', 'CN+S' and
   !> the transmission forms 'XLT', ...); others are SPKAPO's error,
   !> SPICE(SPKINVALIDOPTION). The other errors are those of
   !> [[prepare_state]] (unknown names, an unknown frame).
   module subroutine prepare_position ( ctx, q, target, observer, frame, abcorr )
      use pepper_math, only: odd
      use pepper_strings, only: eqstr, ljucrs
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(position_query), intent(out) :: q  !! the query
      character(*), intent(in) :: target  !! the target's name or ID code
      character(*), intent(in) :: observer  !! the observer's name or ID code
      character(*), intent(in) :: frame  !! the frame of the position
      character(*), intent(in) :: abcorr  !! the aberration correction, as for [[spkpos]]
      integer(ip), parameter :: corlen = 5  !! SPKAPO's length of a correction
      integer(ip), parameter :: ixcns = 5  !! SPKAPO's index of CN+S (the last reception correction)
      character(corlen), parameter :: flags(9) = [ character(corlen) :: 'NONE', 'LT', 'LT+S', 'CN', &
         'CN+S', 'XLT', 'XLT+S', 'XCN', 'XCN+S' ]  !! SPKAPO's corrections
      character(corlen) :: corr
      integer(ip) :: i
      if ( ctx%failed() ) return
!
!     The names and the frame, as for a state (SPKPOS and SPKEZP check
!     them first).
!
      call prepare_state ( ctx, q%s, target, observer, frame, 'NONE' )
      if ( ctx%failed() ) return
      q%s%prepared = .false.
      q%s%abcorr   = abcorr
!
!     SPKEZP evaluates 'NONE' with SPKGPS; any other correction with
!     SPKAPO, which parses it (left justified, upper case, blanks removed).
!
      q%s%geometric = eqstr ( abcorr, 'NONE' )
      q%s%xmit   = .false.
      q%s%uselt  = .false.
      q%s%usecn  = .false.
      q%s%usestl = .false.
      if ( .not. q%s%geometric ) then
         call ljucrs ( 0, abcorr, corr )
         i = findloc(flags, corr, dim=1)
         if ( i == 0 ) then
            call ctx%setmsg ( 'Requested aberration correction was #.' )
            call ctx%errch  ( '#', abcorr                              )
            call ctx%sigerr ( 'SPICE(SPKINVALIDOPTION)'                )
            return
         end if
         q%s%xmit   = i > ixcns
         q%s%uselt  = any ( i == [2, 3, 6, 7] )
         q%s%usecn  = any ( i == [4, 5, 8, 9] )
         q%s%usestl = ( i > 1 ) .and. odd(i)
      end if
      q%s%prepared = .true.
   end subroutine prepare_position

   !> The position of a prepared query at one epoch (PEPPER): [[spkpos]]'s
   !> result, to the last bit. Bound as the generic `query_position`.
   recursive module subroutine query_position_one ( ctx, q, et, pos, lt )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(position_query), intent(inout) :: q  !! the query (its segment cache is updated)
      real(wp), intent(in) :: et  !! the epoch (TDB seconds past J2000)
      real(wp), intent(out) :: pos(3)  !! the position of the target relative to the observer
      real(wp), intent(out) :: lt  !! the one-way light time between them
      real(wp) :: sobs(6)
      real(wp) :: ltobs
      if ( ctx%failed() ) return
      if ( .not. q%s%prepared ) then
         call ctx%setmsg ( 'The position query has not been prepared ' &
         //            '(PREPARE_POSITION).' )
         call ctx%sigerr ( 'SPICE(NOTINITIALIZED)' )
         return
      end if
      if ( q%s%nsegs > max_cached_segments ) q%s%nsegs = 0
      if ( q%s%geometric ) then
         call fast_gps ( ctx, q%s, q%s%target, et, q%s%frame, q%s%frame_name, q%s%observer, pos, lt )
      else if ( q%s%inertial ) then
!
!        SPKEZP: the observer's state relative to the barycenter (SPKSSB),
!        then SPKAPO.
!
         call fast_geo ( ctx, q%s, q%s%observer, et, q%s%frame, q%s%frame_name, ssb, sobs, ltobs )
         call fast_apo ( ctx, q%s, q%s%target, et, q%s%frame, q%s%frame_name, sobs, pos, lt )
      else
         call fast_position_noninertial ( ctx, q%s, et, pos, lt )
      end if
   end subroutine query_position_one

   !> The positions of a prepared query at many epochs (PEPPER). Bound as
   !> the generic `query_position`.
   recursive module subroutine query_position_batch ( ctx, q, ets, positions, lts )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(position_query), intent(inout) :: q  !! the query (its segment cache is updated)
      real(wp), intent(in) :: ets(:)  !! the epochs (TDB seconds past J2000)
      real(wp), intent(out) :: positions(:,:)  !! the positions, shape (3, size(ets)) at least
      real(wp), intent(out), optional :: lts(:)  !! the light times
      real(wp) :: lt
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( size(positions, 1) /= 3 .or. size(positions, 2) < size(ets) ) then
         call ctx%setmsg ( 'The positions array has shape (#, #); ' &
         //            '(3, #) is needed.' )
         call ctx%errint ( '#', size(positions, 1, ip) )
         call ctx%errint ( '#', size(positions, 2, ip) )
         call ctx%errint ( '#', size(ets, kind=ip) )
         call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)' )
         return
      end if
      if ( present(lts) ) then
         if ( size(lts) < size(ets) ) then
            call ctx%setmsg ( 'The light time array has # elements; ' &
            //            '# are needed.' )
            call ctx%errint ( '#', size(lts, kind=ip) )
            call ctx%errint ( '#', size(ets, kind=ip) )
            call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)' )
            return
         end if
      end if
      do k = 1, size(ets, kind=ip)
         call query_position_one ( ctx, q, ets(k), positions(:, k), lt )
         if ( ctx%failed() ) return
         if ( present(lts) ) lts(k) = lt
      end do
   end subroutine query_position_batch

   !> Prepare a frame transformation query (PEPPER;
   !> [[pepper_query_types]]): the transformation from one frame to
   !> another, as [[pxform]] and [[sxform]], resolved once for evaluation
   !> with `query_rotation` and `query_transform`. A rotation between
   !> inertial and fixed TK frames is computed once.
   module subroutine prepare_rotation ( ctx, q, from, to )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(rotation_query), intent(out) :: q  !! the query
      character(*), intent(in) :: from  !! the frame transformed from
      character(*), intent(in) :: to  !! the frame transformed to
      q = rotation_query()
      if ( ctx%failed() ) return
      call namfrm ( ctx, from, q%from )
      call namfrm ( ctx, to, q%to )
      if ( ctx%failed() ) return
      if ( q%from == 0 .and. q%to == 0 ) then
         call ctx%setmsg ( 'Neither of the frames # or # was ' &
         //            'recognized as a known reference frame. ' )
         call ctx%errch  ( '#', from )
         call ctx%errch  ( '#', to   )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return
      else if ( q%from == 0 ) then
         call ctx%setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call ctx%errch  ( '#', from )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return
      else if ( q%to == 0 ) then
         call ctx%setmsg ( 'The frame # was not recognized as a ' &
         //            'known reference frame. ' )
         call ctx%errch  ( '#', to   )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)'  )
         return
      end if
!
!     The kernel pool's state: a constant rotation is recomputed when the
!     pool changes (TK frames are defined in it).
!
      call zzctruin ( ctx, q%poolctr )
      q%prepared = .true.
   end subroutine prepare_rotation

   !> The rotation of a prepared frame query at one epoch (PEPPER):
   !> [[pxform]]'s result. Bound as the generic `query_rotation`.
   recursive module subroutine query_rotation_one ( ctx, q, et, rotate )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(rotation_query), intent(inout) :: q  !! the query
      real(wp), intent(in) :: et  !! the epoch (TDB seconds past J2000)
      real(wp), intent(out) :: rotate(3, 3)  !! the rotation
      logical :: update
      if ( ctx%failed() ) return
      if ( .not. q%prepared ) then
         call ctx%setmsg ( 'The frame query has not been prepared ' &
         //            '(PREPARE_ROTATION).' )
         call ctx%sigerr ( 'SPICE(NOTINITIALIZED)' )
         return
      end if
!
!     Whether the rotation depends on time: found on the first call and
!     after each change of the kernel pool.
!
      call zzpctrck ( ctx, q%poolctr, update )
      if ( update ) then
         q%have_rotation = .false.
         q%constant = frame_is_constant ( ctx, q%from ) .and. frame_is_constant ( ctx, q%to )
         if ( ctx%failed() ) return
      end if
      if ( q%constant .and. q%have_rotation ) then
         rotate = q%rotation
         return
      end if
      call refchg ( ctx, q%from, q%to, et, rotate )
      if ( ctx%failed() ) return
      if ( q%constant ) then
         q%rotation      = rotate
         q%have_rotation = .true.
      end if
   end subroutine query_rotation_one

   !> The rotations of a prepared frame query at many epochs (PEPPER).
   !> Bound as the generic `query_rotation`.
   recursive module subroutine query_rotation_batch ( ctx, q, ets, rotations )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(rotation_query), intent(inout) :: q  !! the query
      real(wp), intent(in) :: ets(:)  !! the epochs (TDB seconds past J2000)
      real(wp), intent(out) :: rotations(:,:,:)  !! the rotations, shape (3, 3, size(ets)) at least
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( size(rotations, 1) /= 3 .or. size(rotations, 2) /= 3 .or. size(rotations, 3) < size(ets) ) then
         call ctx%setmsg ( 'The rotations array has shape (#, #, #); ' &
         //            '(3, 3, #) is needed.' )
         call ctx%errint ( '#', size(rotations, 1, ip) )
         call ctx%errint ( '#', size(rotations, 2, ip) )
         call ctx%errint ( '#', size(rotations, 3, ip) )
         call ctx%errint ( '#', size(ets, kind=ip) )
         call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)' )
         return
      end if
      do k = 1, size(ets, kind=ip)
         call query_rotation_one ( ctx, q, ets(k), rotations(:, :, k) )
         if ( ctx%failed() ) return
      end do
   end subroutine query_rotation_batch

   !> The state transformation of a prepared frame query at one epoch
   !> (PEPPER): [[sxform]]'s result. Bound as the generic
   !> `query_transform`.
   recursive module subroutine query_transform_one ( ctx, q, et, xform )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(rotation_query), intent(inout) :: q  !! the query
      real(wp), intent(in) :: et  !! the epoch (TDB seconds past J2000)
      real(wp), intent(out) :: xform(6, 6)  !! the state transformation
      if ( ctx%failed() ) return
      if ( .not. q%prepared ) then
         call ctx%setmsg ( 'The frame query has not been prepared ' &
         //            '(PREPARE_ROTATION).' )
         call ctx%sigerr ( 'SPICE(NOTINITIALIZED)' )
         return
      end if
      call frmchg ( ctx, q%from, q%to, et, xform )
   end subroutine query_transform_one

   !> The state transformations of a prepared frame query at many epochs
   !> (PEPPER). Bound as the generic `query_transform`.
   recursive module subroutine query_transform_batch ( ctx, q, ets, xforms )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(rotation_query), intent(inout) :: q  !! the query
      real(wp), intent(in) :: ets(:)  !! the epochs (TDB seconds past J2000)
      real(wp), intent(out) :: xforms(:,:,:)  !! the transformations, shape (6, 6, size(ets)) at least
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( size(xforms, 1) /= 6 .or. size(xforms, 2) /= 6 .or. size(xforms, 3) < size(ets) ) then
         call ctx%setmsg ( 'The transformations array has shape (#, #, #); ' &
         //            '(6, 6, #) is needed.' )
         call ctx%errint ( '#', size(xforms, 1, ip) )
         call ctx%errint ( '#', size(xforms, 2, ip) )
         call ctx%errint ( '#', size(xforms, 3, ip) )
         call ctx%errint ( '#', size(ets, kind=ip) )
         call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)' )
         return
      end if
      do k = 1, size(ets, kind=ip)
         call query_transform_one ( ctx, q, ets(k), xforms(:, :, k) )
         if ( ctx%failed() ) return
      end do
   end subroutine query_transform_batch

   ! ----------------------------------------------------------------------
   ! The fast path of state queries. Each routine is its SPICELIB
   ! counterpart, statement for statement, on the query's segments.
   ! ----------------------------------------------------------------------

   !> Whether a frame's orientation relative to the inertial frames is
   !> constant: an inertial frame, or a chain of TK frames ending at one.
   recursive logical function frame_is_constant ( ctx, frame ) result ( constant )
      use pepper_inc_frmtyp, only: inertl, tk
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp) :: rot(3, 3)
      integer(ip) :: f
      integer(ip) :: cent
      integer(ip) :: class
      integer(ip) :: clssid
      integer(ip) :: hop
      logical :: found
      constant = .false.
      f = frame
      do hop = 1, chlen
         call frinfo ( ctx, f, cent, class, clssid, found )
         if ( ctx%failed() .or. .not. found ) return
         if ( class == inertl ) then
            constant = .true.
            return
         else if ( class == tk ) then
            call tkfram ( ctx, clssid, rot, f, found )
            if ( ctx%failed() .or. .not. found ) return
         else
            return
         end if
      end do
   end function frame_is_constant

   !> The index in Q%SEGS of a segment (added if new): its center, frame
   !> and type, and for types 2 and 3 the record directory (SPKR02, SPKR03).
   !>
   !> A segment is identified by its file and its addresses in the file,
   !> from the unpacked descriptor. (The packed descriptor's integer
   !> components are not numbers: as reals they are denormals or NaNs, which
   !> do not compare as their bits do.) The addresses are the last word of
   !> an SPK descriptor (ND = 2, NI = 6): only that word is unpacked to look
   !> a segment up, as it is done at every epoch; the whole descriptor only
   !> for a new segment.
   recursive integer(ip) function segment_slot ( ctx, q, handle, descr ) result ( k )
      use pepper_packing, only: unpack_integers
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: handle  !! the segment's file
      real(wp), intent(in) :: descr(5)  !! the segment's descriptor
      type(spk_segment_cache), allocatable :: tmp(:)
      real(wp) :: dc(2)
      real(wp) :: trailer(4)
      integer(ip) :: ic(6)
      integer(ip) :: addr(2)
      call unpack_integers ( descr(5:5), 2, addr )
      do k = 1, q%nsegs
         if (       q%segs(k)%handle == handle &
              .and. q%segs(k)%begin  == addr(1) &
              .and. q%segs(k)%last   == addr(2) ) return
      end do
      call dafus ( ctx, descr, 2, 6, dc, ic )
      if ( .not. allocated ( q%segs ) ) allocate ( q%segs(8) )
      if ( q%nsegs == size(q%segs) ) then
         allocate ( tmp(2*size(q%segs)) )
         tmp(1:q%nsegs) = q%segs(1:q%nsegs)
         call move_alloc ( tmp, q%segs )
      end if
      k = q%nsegs + 1
      q%segs(k)%handle = handle
      q%segs(k)%descr  = descr
      q%segs(k)%begin  = ic(5)
      q%segs(k)%last   = ic(6)
      q%segs(k)%center = ic(2)
      q%segs(k)%frame  = ic(3)
      q%segs(k)%type   = ic(4)
      q%segs(k)%recno  = 0
      if ( ic(4) == 2 .or. ic(4) == 3 ) then
         call dafgda ( ctx, handle, ic(6)-3, ic(6), trailer )
         if ( ctx%failed() ) return
         q%segs(k)%init   = trailer(1)
         q%segs(k)%intlen = trailer(2)
         q%segs(k)%rsize  = trailer(3)
         q%segs(k)%recsiz = int ( trailer(3) )
         q%segs(k)%nrec   = int ( trailer(4) )
         if ( allocated ( q%segs(k)%record ) ) deallocate ( q%segs(k)%record )
         allocate ( q%segs(k)%record(q%segs(k)%recsiz + 1) )
      end if
      q%nsegs = k
   end function segment_slot

   !> Make the record for ET a type 2 or 3 segment's current record (in
   !> S%RECORD), chosen as SPKR02 and SPKR03 choose it, read only if it is
   !> another.
   recursive subroutine fetch_record ( ctx, s, et )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(spk_segment_cache), intent(inout) :: s  !! the segment
      real(wp), intent(in) :: et  !! the epoch
      integer(ip) :: recno
      integer(ip) :: recadr
      recno = int( (et - s%init) / s%intlen ) + 1
      recno = min( recno, s%nrec )
      if ( recno /= s%recno ) then
         recadr = ( recno - 1 )*s%recsiz + s%begin
         s%record(1) = s%rsize
         call dafgda ( ctx, s%handle, recadr, recadr + s%recsiz - 1, s%record(2) )
         if ( ctx%failed() ) then
            s%recno = 0
            return
         end if
         s%recno = recno
      end if
   end subroutine fetch_record

   !> The state of a segment's body relative to its center at ET, in the
   !> segment's frame: SPKPVN, with the current record kept for types 2
   !> and 3 (whose record is chosen as SPKR02 and SPKR03 choose it).
   recursive subroutine evaluate_segment ( ctx, q, k, et, state )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: k  !! the segment's index in Q%SEGS
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state
      integer(ip) :: ref
      integer(ip) :: center
      associate ( s => q%segs(k) )
      if ( s%type == 2 .or. s%type == 3 ) then
         call fetch_record ( ctx, s, et )
         if ( ctx%failed() ) return
         if ( s%type == 2 ) then
            call spke02 ( ctx, et, s%record, state )
         else
            call spke03 ( ctx, et, s%record, state )
         end if
      else
         call spkpvn ( ctx, s%handle, s%descr, et, ref, state, center )
      end if
      end associate
   end subroutine evaluate_segment

   !> The geometric state of TARG relative to OBS at ET in frame REFID:
   !> SPKGEO. The target's chain is found first from the segments'
   !> descriptors, and only its links up to the body where it meets the
   !> observer's chain are evaluated (SPKGEO evaluates all of them).
   recursive subroutine fast_geo ( ctx, q, targ, et, refid, refname, obs, state, lt )
      use pepper_constants, only: clight
      use pepper_math, only: mxv, mxvg, vnorm
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: targ  !! the target
      real(wp), intent(in) :: et  !! the epoch
      integer(ip), intent(in) :: refid  !! the frame's ID code
      character(*), intent(in) :: refname  !! the frame's name (for SPKGEO, when the fast path does not apply)
      integer(ip), intent(in) :: obs  !! the observer
      real(wp), intent(out) :: state(6)  !! the state
      real(wp), intent(out) :: lt  !! the one-way light time
      character(40) :: ident
      real(wp) :: descr(5)
      real(wp) :: sobs(6)
      real(wp) :: starg(6, chlen)
      real(wp) :: stemp(6)
      real(wp) :: stxfrm(6, 6)
      real(wp) :: rot(3, 3)
      real(wp) :: vtemp(6)
      integer(ip) :: cframe
      integer(ip) :: cobs
      integer(ip) :: ctarg(chlen)
      integer(ip) :: tframe(chlen)
      integer(ip) :: tseg(chlen)
      integer(ip) :: ctpos
      integer(ip) :: handle
      integer(ip) :: i
      integer(ip) :: k
      integer(ip) :: legs
      integer(ip) :: nct
      integer(ip) :: tmpfrm
      logical :: found
      logical :: nofrm

      if ( ctx%failed() ) return
      if ( targ == obs ) then
         lt    = 0.0_wp
         state = 0.0_wp
         return
      end if
!
!     The target's chain: the segments and their centers (SPKGEO's first
!     loop, without the evaluations).
!
      tframe   = 0
      i        = 1
      ctarg(i) = targ
      found    = .true.
      starg(:,i) = 0.0_wp
      do while (           found &
                  .and.  ( i        < chlen ) &
                  .and.  ( ctarg(i) /= obs   ) &
                  .and.  ( ctarg(i) /= ssb   )  )
         call spksfs ( ctx, ctarg(i), et, handle, descr, ident, found )
         if ( ctx%failed() ) return
         if ( found ) then
            k = segment_slot ( ctx, q, handle, descr )
            if ( ctx%failed() ) return
            tseg(i)   = k
            i         = i + 1
            ctarg(i)  = q%segs(k)%center
            tframe(i) = q%segs(k)%frame
         end if
      end do
      if ( i == chlen ) then
!
!        A chain longer than SPKGEO's table: SPKGEO handles it.
!
         call spkgeo ( ctx, targ, et, refname, obs, state, lt )
         return
      end if
      tframe(1) = tframe(2)
      nct = i
!
!     The observer's chain, evaluated, until it meets the target's
!     (SPKGEO's second loop).
!
      cobs = obs
      sobs = 0.0_wp
      if ( ctarg(nct) == cobs ) then
         ctpos  = nct
         cframe = tframe(ctpos)
      else
         ctpos  = 0
      end if
      found  = .true.
      nofrm  = .true.
      legs   =  0
      do while (           found &
                  .and.  ( cobs  /= ssb ) &
                  .and.  ( ctpos == 0   )  )
         call spksfs ( ctx, cobs, et, handle, descr, ident, found )
         if ( ctx%failed() ) return
         if ( found ) then
            k = segment_slot ( ctx, q, handle, descr )
            if ( ctx%failed() ) return
            tmpfrm = q%segs(k)%frame
            if ( legs == 0 ) then
               call evaluate_segment ( ctx, q, k, et, sobs )
            else
               call evaluate_segment ( ctx, q, k, et, stemp )
            end if
            cobs = q%segs(k)%center
            if ( ctx%failed() ) return
            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
            if ( cframe == tmpfrm ) then
               if ( legs > 0 ) then
                  vtemp = sobs + stemp
                  sobs = vtemp
               end if
            else if ( is_inertial_pair ( cframe, tmpfrm ) ) then
               call irfrot ( ctx, cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs(1),   vtemp(1)  )
               call mxv    ( rot,    sobs(4),   vtemp(4)  )
               sobs = vtemp + stemp
               cframe = tmpfrm
            else
               call frmchg ( ctx, cframe, tmpfrm, et,   stxfrm )
               if ( ctx%failed() ) return
               call mxvg   ( stxfrm, sobs,   6, 6, vtemp  )
               sobs = vtemp + stemp
               cframe = tmpfrm
            end if
            legs  = legs + 1
            ctpos = findloc(ctarg(1:nct), cobs, dim=1)
         end if
      end do
      if ( ctpos == 0 ) then
!
!        Not enough data: SPKGEO signals the error.
!
         call spkgeo ( ctx, targ, et, refname, obs, state, lt )
         return
      end if
!
!     The target's links up to the common body (SPKGEO evaluated them in
!     its first loop: the same states).
!
      do i = 1, ctpos - 1
         call evaluate_segment ( ctx, q, tseg(i), et, starg(:,i+1) )
         if ( ctx%failed() ) return
      end do
!
!     SPKGEO's combination of the two chains.
!
      if ( ctpos == 1 ) then
         tframe(1) = cframe
      end if
      do i = 2, ctpos-1
         if ( tframe(i) == tframe(i+1) ) then
            vtemp = starg(:,i) + starg(:,i+1)
            starg(:,i+1) = vtemp
         else if ( is_inertial_pair ( tframe(i), tframe(i+1) ) ) then
            call irfrot ( ctx, tframe(i),  tframe(i+1),     rot          )
            call mxv    ( rot,        starg(1,i),      stemp(1)     )
            call mxv    ( rot,        starg(4,i),      stemp(4)     )
            vtemp = stemp + starg(:,i+1)
            starg(:,i+1) = vtemp
         else
            call frmchg ( ctx, tframe(i),  tframe(i+1), et,  stxfrm       )
            if ( ctx%failed() ) return
            call mxvg   ( stxfrm,     starg(1,i),  6,6, stemp        )
            vtemp = stemp + starg(:,i+1)
            starg(:,i+1) = vtemp
         end if
      end do
      if ( tframe(ctpos) == cframe ) then
         state = starg(:,ctpos) - sobs
      else if ( tframe(ctpos) == refid ) then
         if ( is_inertial_pair ( cframe, refid ) ) then
            call irfrot ( ctx, cframe, refid,   rot      )
            call mxv    ( rot,    sobs(1), stemp(1) )
            call mxv    ( rot,    sobs(4), stemp(4) )
         else
            call frmchg ( ctx, cframe, refid, et,  stxfrm )
            if ( ctx%failed() ) return
            call mxvg   ( stxfrm, sobs,  6,6, stemp  )
         end if
         cframe = refid
         state = starg(:,ctpos) - stemp
      else if ( is_inertial_pair ( tframe(ctpos), cframe ) ) then
         call irfrot ( ctx, tframe(ctpos),  cframe,               rot      )
         call mxv    ( rot,            starg(1,ctpos),       stemp(1) )
         call mxv    ( rot,            starg(4,ctpos),       stemp(4) )
         state = stemp - sobs
      else
         call frmchg ( ctx, tframe(ctpos),  cframe,          et,  stxfrm )
         if ( ctx%failed() ) return
         call mxvg   ( stxfrm,         starg(1,ctpos),  6,6, stemp  )
         state = stemp - sobs
      end if
      if ( cframe == refid ) then
         continue
      else if ( is_inertial_pair ( cframe, refid ) ) then
         call irfrot ( ctx, cframe,  refid,      rot      )
         call mxv    ( rot,     state(1),   stemp(1) )
         call mxv    ( rot,     state(4),   stemp(4) )
         state = stemp
      else
         call frmchg ( ctx, cframe,  refid, et,  stxfrm )
         if ( ctx%failed() ) return
         call mxvg   ( stxfrm,  state, 6, 6, stemp )
         state = stemp
      end if
      lt = vnorm  ( state ) / clight
   end subroutine fast_geo

   !> Whether two frames are both inertial (SPKGEO's statement function
   !> ISINRT).
   pure logical function is_inertial_pair ( f1, f2 )
      use pepper_inc_ninert, only: ninert
      integer(ip), intent(in) :: f1  !! one frame
      integer(ip), intent(in) :: f2  !! the other
      is_inertial_pair = f2 > 0 .and. f2 <= ninert .and. f1 > 0 .and. f1 <= ninert
   end function is_inertial_pair

   !> The light-time corrected state of TARG relative to an observer whose
   !> state relative to the solar system barycenter is STOBS: SPKLTC.
   recursive subroutine fast_ltc ( ctx, q, targ, et, refid, refname, stobs, starg, lt, dlt )
      use pepper_constants, only: clight
      use pepper_math, only: vdot, vnorm
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: targ  !! the target
      real(wp), intent(in) :: et  !! the epoch
      integer(ip), intent(in) :: refid  !! the (inertial) frame's ID code
      character(*), intent(in) :: refname  !! its name
      real(wp), intent(in) :: stobs(6)  !! the observer's state relative to the barycenter
      real(wp), intent(out) :: starg(6)  !! the state of the target relative to the observer
      real(wp), intent(out) :: lt  !! the one-way light time
      real(wp), intent(out) :: dlt  !! its derivative
      real(wp), parameter :: tol = 1.0e-10_wp
      real(wp), parameter :: cnvlim = 1.0e-17_wp
      integer(ip), parameter :: maxitr = 5
      real(wp) :: a
      real(wp) :: b
      real(wp) :: c
      real(wp) :: dist
      real(wp) :: epoch
      real(wp) :: lterr
      real(wp) :: prvlt
      real(wp) :: ssblt
      real(wp) :: ssbtrg(6)
      integer(ip) :: i
      integer(ip) :: ltsign
      integer(ip) :: numitr
      if ( ctx%failed() ) return
      call fast_geo ( ctx, q, targ, et, refid, refname, ssb, ssbtrg, ssblt )
      if ( ctx%failed() ) return
      starg = ssbtrg - stobs
      dist = vnorm(starg)
      lt   = dist / clight
      if ( lt == 0.0_wp ) then
         dlt = 0
         return
      end if
      if ( .not. q%uselt ) then
         dlt = vdot( starg, starg(4) ) / ( dist * clight )
         return
      end if
      if ( q%xmit ) then
         ltsign =  1
      else
         ltsign = -1
      end if
      if ( q%usecn ) then
         numitr = maxitr
      else
         numitr = 1
      end if
      i     = 0
      lterr = 1.0_wp
      do while (  ( i < numitr ) .and. ( lterr > cnvlim )  )
         epoch  =  et + ( ltsign * lt )
         call fast_geo ( ctx, q, targ, epoch, refid, refname, ssb, ssbtrg, ssblt )
         if ( ctx%failed() ) return
         starg = ssbtrg - stobs
         prvlt = lt
         lt    = (vnorm( starg ) / clight)
         lterr = (abs( lt  - prvlt      )  / max( 1.0_wp, abs(epoch) ))
         i     = i + 1
      end do
      a   =  1.0_wp / ( clight*vnorm(starg) )
      b   =  vdot ( starg, starg(4)  )
      c   =  vdot ( starg, ssbtrg(4) )
      if (  ltsign*c*a  >  1.0_wp-tol ) then
         call ctx%setmsg ( 'Target range rate magnitude is ' &
      //               'approximately the speed of light. The ' &
      //               'light time derivative cannot be ' &
      //               'computed.'                            )
         call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                  )
         return
      end if
      dlt = (a*b)/( 1.0_wp - ltsign*c*a )
      starg(4:6) = (1.0_wp + ltsign*dlt)*ssbtrg(4:6) + (-1.0_wp)*stobs(4:6)
   end subroutine fast_ltc

   !> The apparent state of TARG relative to an observer whose state and
   !> acceleration relative to the barycenter are STOBS and ACCOBS: SPKAPS.
   recursive subroutine fast_aps ( ctx, q, targ, et, refid, refname, stobs, accobs, starg, lt, dlt )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: targ  !! the target
      real(wp), intent(in) :: et  !! the epoch
      integer(ip), intent(in) :: refid  !! the (inertial) frame's ID code
      character(*), intent(in) :: refname  !! its name
      real(wp), intent(in) :: stobs(6)  !! the observer's state relative to the barycenter
      real(wp), intent(in) :: accobs(3)  !! the observer's acceleration relative to the barycenter
      real(wp), intent(out) :: starg(6)  !! the apparent state of the target
      real(wp), intent(out) :: lt  !! the one-way light time
      real(wp), intent(out) :: dlt  !! its derivative
      real(wp) :: corpos(3)
      real(wp) :: corvel(3)
      real(wp) :: dpcorr(3)
      real(wp) :: pcorr(3)
      if ( ctx%failed() ) return
      call fast_ltc ( ctx, q, targ, et, refid, refname, stobs, starg, lt, dlt )
      if ( ctx%failed() ) return
      if ( .not. q%usestl ) return
      call zzstelab ( ctx, q%xmit, accobs, stobs(4), starg, pcorr, dpcorr )
      corpos = pcorr + starg(1:3)
      starg(1:3) = corpos
      corvel = dpcorr + starg(4:6)
      starg(4:6) = corvel
   end subroutine fast_aps

   !> The apparent state of TARG relative to OBS in an inertial frame:
   !> SPKACS.
   recursive subroutine fast_acs ( ctx, q, targ, et, refid, refname, obs, starg, lt, dlt )
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: targ  !! the target
      real(wp), intent(in) :: et  !! the epoch
      integer(ip), intent(in) :: refid  !! the (inertial) frame's ID code
      character(*), intent(in) :: refname  !! its name
      integer(ip), intent(in) :: obs  !! the observer
      real(wp), intent(out) :: starg(6)  !! the apparent state of the target
      real(wp), intent(out) :: lt  !! the one-way light time
      real(wp), intent(out) :: dlt  !! its derivative
      real(wp), parameter :: delta = 1.0_wp
      real(wp) :: acc(3)
      real(wp) :: ltssb
      real(wp) :: ssblt
      real(wp) :: ssbobs(6)
      real(wp) :: stobs(6, 2)
      real(wp) :: t
      integer(ip) :: i
      if ( ctx%failed() ) return
      call fast_geo ( ctx, q, obs, et, refid, refname, ssb, ssbobs, ssblt )
      if ( q%usestl ) then
         do i = 1, 2
            t = et + (2*i - 3)*delta
            call fast_geo ( ctx, q, obs, t, refid, refname, ssb, stobs(1,i), ltssb )
         end do
         call qderiv ( ctx, 3, stobs(4,1), stobs(4,2), delta, acc )
      else
         acc = 0.0_wp
      end if
      call fast_aps ( ctx, q, targ, et, refid, refname, ssbobs, acc, starg, lt, dlt )
   end subroutine fast_acs

   !> The apparent state in a non-inertial frame: SPKEZ's branch for those
   !> frames (the state in J2000, rotated at the epoch of the frame's
   !> center corrected for light time).
   recursive subroutine fast_noninertial ( ctx, q, et, starg, lt )
      use pepper_math, only: mxvg
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: starg(6)  !! the apparent state of the target
      real(wp), intent(out) :: lt  !! the one-way light time
      integer(ip), parameter :: fj2000 = 1
      real(wp) :: state(6)
      real(wp) :: temp(6)
      real(wp) :: xform(6, 6)
      real(wp) :: stobs(6)
      real(wp) :: ltcent
      real(wp) :: dltctr
      real(wp) :: dlt
      real(wp) :: ltobs
      integer(ip) :: ltsign
      integer(ip) :: i
      call fast_acs ( ctx, q, q%target, et, fj2000, 'J2000', q%observer, state, lt, dlt )
      if ( ctx%failed() ) return
      if ( q%frame_center == q%observer ) then
         ltcent = 0.0_wp
         dltctr = 0.0_wp
      else if ( q%frame_center == q%target ) then
         ltcent = lt
         dltctr = dlt
      else
         call fast_geo ( ctx, q, q%observer, et, fj2000, 'J2000', ssb, stobs, ltobs )
         call fast_ltc ( ctx, q, q%frame_center, et, fj2000, 'J2000', stobs, &
                         temp,   ltcent, dltctr                 )
      end if
      if ( ctx%failed() ) return
      if ( q%xmit ) then
         ltsign =  1
      else
         ltsign = -1
      end if
      call frmchg ( ctx, fj2000, q%frame, et + ltsign*ltcent, xform )
      if ( ctx%failed() ) return
      do i = 1, 3
         xform(4:6,i) = (1.0_wp + ltsign*dltctr)*xform(4:6,i)
      end do
      call mxvg ( xform, state, 6, 6, starg )
   end subroutine fast_noninertial

   ! ----------------------------------------------------------------------
   ! The fast path of position queries: SPKGPS, SPKAPO and SPKEZP, statement
   ! for statement, on the query's segments, evaluating positions only.
   ! ----------------------------------------------------------------------

   !> The position of a segment's body relative to its center at ET, in the
   !> segment's frame: the position of SPKPVN's state. For types 2 and 3
   !> only the three position series are evaluated, with the current record
   !> kept. (CHBVAL's value is CHBINT's, which SPKE02 uses: the same
   !> recurrence, the last sum's operands commuted.)
   recursive subroutine evaluate_position ( ctx, q, k, et, pos )
      use pepper_math, only: chbval
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: k  !! the segment's index in Q%SEGS
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: pos(3)  !! the position
      real(wp) :: state(6)
      integer(ip) :: ncof
      integer(ip) :: i
      integer(ip) :: ref
      integer(ip) :: center
      associate ( s => q%segs(k) )
      if ( s%type == 2 .or. s%type == 3 ) then
         call fetch_record ( ctx, s, et )
         if ( ctx%failed() ) return
         if ( s%type == 2 ) then
            ncof = ( int( s%record(1) ) - 2 ) / 3
         else
            ncof = ( int( s%record(1) ) - 2 ) / 6
         end if
         if ( ncof < 1 .or. s%record(3) <= 0.0_wp ) then
!
!           An invalid record: SPKE02 or SPKE03 signals the error.
!
            if ( s%type == 2 ) then
               call spke02 ( ctx, et, s%record, state )
            else
               call spke03 ( ctx, et, s%record, state )
            end if
            pos = state(1:3)
            return
         end if
         do i = 1, 3
            call chbval ( s%record( ncof*(i-1) + 4 ), ncof - 1, s%record(2), et, pos(i) )
         end do
      else
         call spkpvn ( ctx, s%handle, s%descr, et, ref, state, center )
         pos = state(1:3)
      end if
      end associate
   end subroutine evaluate_position

   !> The geometric position of TARG relative to OBS at ET in frame REFID:
   !> SPKGPS. As in [[fast_geo]], the target's chain is found first, and
   !> only its links up to the body where it meets the observer's chain are
   !> evaluated.
   recursive subroutine fast_gps ( ctx, q, targ, et, refid, refname, obs, pos, lt )
      use pepper_constants, only: clight
      use pepper_math, only: mxv, vnorm
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: targ  !! the target
      real(wp), intent(in) :: et  !! the epoch
      integer(ip), intent(in) :: refid  !! the frame's ID code
      character(*), intent(in) :: refname  !! the frame's name (for SPKGPS, when the fast path does not apply)
      integer(ip), intent(in) :: obs  !! the observer
      real(wp), intent(out) :: pos(3)  !! the position
      real(wp), intent(out) :: lt  !! the one-way light time
      character(40) :: ident
      real(wp) :: descr(5)
      real(wp) :: sobs(3)
      real(wp) :: starg(3, chlen)
      real(wp) :: stemp(3)
      real(wp) :: rot(3, 3)
      real(wp) :: vtemp(3)
      integer(ip) :: cframe
      integer(ip) :: cobs
      integer(ip) :: ctarg(chlen)
      integer(ip) :: tframe(chlen)
      integer(ip) :: tseg(chlen)
      integer(ip) :: ctpos
      integer(ip) :: handle
      integer(ip) :: i
      integer(ip) :: k
      integer(ip) :: legs
      integer(ip) :: nct
      integer(ip) :: tmpfrm
      logical :: found
      logical :: nofrm

      if ( ctx%failed() ) return
      if ( targ == obs ) then
         lt  = 0.0_wp
         pos = 0.0_wp
         return
      end if
!
!     The target's chain: the segments and their centers.
!
      tframe   = 0
      i        = 1
      ctarg(i) = targ
      found    = .true.
      starg(:,i) = 0.0_wp
      do while (           found &
                  .and.  ( i        < chlen ) &
                  .and.  ( ctarg(i) /= obs   ) &
                  .and.  ( ctarg(i) /= ssb   )  )
         call spksfs ( ctx, ctarg(i), et, handle, descr, ident, found )
         if ( ctx%failed() ) return
         if ( found ) then
            k = segment_slot ( ctx, q, handle, descr )
            if ( ctx%failed() ) return
            tseg(i)   = k
            i         = i + 1
            ctarg(i)  = q%segs(k)%center
            tframe(i) = q%segs(k)%frame
         end if
      end do
      if ( i == chlen ) then
!
!        A chain longer than SPKGPS's table: SPKGPS handles it.
!
         call spkgps ( ctx, targ, et, refname, obs, pos, lt )
         return
      end if
      tframe(1) = tframe(2)
      nct = i
!
!     The observer's chain, evaluated, until it meets the target's.
!
      cobs = obs
      sobs = 0.0_wp
      if ( ctarg(nct) == cobs ) then
         ctpos  = nct
         cframe = tframe(ctpos)
      else
         ctpos  = 0
      end if
      found  = .true.
      nofrm  = .true.
      legs   =  0
      do while (           found &
                  .and.  ( cobs  /= ssb ) &
                  .and.  ( ctpos == 0   )  )
         call spksfs ( ctx, cobs, et, handle, descr, ident, found )
         if ( ctx%failed() ) return
         if ( found ) then
            k = segment_slot ( ctx, q, handle, descr )
            if ( ctx%failed() ) return
            tmpfrm = q%segs(k)%frame
            if ( legs == 0 ) then
               call evaluate_position ( ctx, q, k, et, sobs )
            else
               call evaluate_position ( ctx, q, k, et, stemp )
            end if
            cobs = q%segs(k)%center
            if ( ctx%failed() ) return
            if ( nofrm ) then
               nofrm  = .false.
               cframe = tmpfrm
            end if
            if ( cframe == tmpfrm ) then
               if ( legs > 0 ) then
                  vtemp = sobs + stemp
                  sobs = vtemp
               end if
            else if ( is_inertial_pair ( cframe, tmpfrm ) ) then
               call irfrot ( ctx, cframe, tmpfrm, rot )
               call mxv    ( rot,    sobs,   vtemp  )
               sobs = vtemp + stemp
               cframe = tmpfrm
            else
               call refchg ( ctx, cframe, tmpfrm, et,   rot )
               if ( ctx%failed() ) return
               call mxv    ( rot,    sobs,   vtemp  )
               sobs = vtemp + stemp
               cframe = tmpfrm
            end if
            legs  = legs + 1
            ctpos = findloc(ctarg(1:nct), cobs, dim=1)
         end if
      end do
      if ( ctpos == 0 ) then
!
!        Not enough data: SPKGPS signals the error.
!
         call spkgps ( ctx, targ, et, refname, obs, pos, lt )
         return
      end if
!
!     The target's links up to the common body.
!
      do i = 1, ctpos - 1
         call evaluate_position ( ctx, q, tseg(i), et, starg(:,i+1) )
         if ( ctx%failed() ) return
      end do
!
!     SPKGPS's combination of the two chains.
!
      if ( ctpos == 1 ) then
         tframe(1) = cframe
      end if
      do i = 2, ctpos-1
         if ( tframe(i) == tframe(i+1) ) then
            stemp = starg(:,i) + starg(:,i+1)
            starg(:,i+1) = stemp
         else if ( is_inertial_pair ( tframe(i), tframe(i+1) ) ) then
            call irfrot ( ctx, tframe(i),  tframe(i+1),     rot   )
            call mxv    ( rot,        starg(:,i),      stemp )
            vtemp = stemp + starg(:,i+1)
            starg(:,i+1) = vtemp
         else
            call refchg ( ctx, tframe(i),  tframe(i+1), et,  rot  )
            if ( ctx%failed() ) return
            call mxv    ( rot,        starg(:,i),       stemp )
            vtemp = stemp + starg(:,i+1)
            starg(:,i+1) = vtemp
         end if
      end do
      if ( tframe(ctpos) == cframe ) then
         pos = starg(:,ctpos) - sobs
      else if ( tframe(ctpos) == refid ) then
         if ( is_inertial_pair ( cframe, refid ) ) then
            call irfrot ( ctx, cframe, refid,   rot   )
            call mxv    ( rot,    sobs,  stemp )
         else
            call refchg ( ctx, cframe, refid, et,  rot )
            if ( ctx%failed() ) return
            call mxv    ( rot,    sobs,       stemp  )
         end if
         cframe = refid
         pos = starg(:,ctpos) - stemp
      else if ( is_inertial_pair ( tframe(ctpos), cframe ) ) then
         call irfrot ( ctx, tframe(ctpos),  cframe,               rot   )
         call mxv    ( rot,            starg(:,ctpos),       stemp )
         pos = stemp - sobs
      else
         call refchg ( ctx, tframe(ctpos),  cframe,          et,  rot )
         if ( ctx%failed() ) return
         call mxv    ( rot,            starg(:,ctpos),       stemp  )
         pos = stemp - sobs
      end if
      if ( cframe == refid ) then
         continue
      else if ( is_inertial_pair ( cframe, refid ) ) then
         call irfrot ( ctx, cframe,  refid,    rot   )
         call mxv    ( rot,     pos,      stemp )
         pos = stemp
      else
         call refchg ( ctx, cframe,  refid, et,  rot  )
         if ( ctx%failed() ) return
         call mxv    ( rot,     pos,        stemp   )
         pos = stemp
      end if
      lt = vnorm  ( pos ) / clight
   end subroutine fast_gps

   !> The apparent position of TARG relative to an observer whose state
   !> relative to the barycenter is SOBS, in an inertial frame: SPKAPO (one
   !> light time iteration for LT, three for CN; STELAB or STLABX).
   recursive subroutine fast_apo ( ctx, q, targ, et, refid, refname, sobs, ptarg, lt )
      use pepper_constants, only: clight
      use pepper_math, only: vnorm
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      integer(ip), intent(in) :: targ  !! the target
      real(wp), intent(in) :: et  !! the epoch
      integer(ip), intent(in) :: refid  !! the (inertial) frame's ID code
      character(*), intent(in) :: refname  !! its name
      real(wp), intent(in) :: sobs(6)  !! the observer's state relative to the barycenter
      real(wp), intent(out) :: ptarg(3)  !! the apparent position of the target
      real(wp), intent(out) :: lt  !! the one-way light time
      real(wp) :: tpos(3)
      integer(ip) :: i
      integer(ip) :: ltsign
      integer(ip) :: maxitr
      if ( ctx%failed() ) return
      if ( q%xmit ) then
         ltsign = 1
      else
         ltsign = -1
      end if
      call fast_gps ( ctx, q, targ, et, refid, refname, ssb, ptarg, lt )
      if ( ctx%failed() ) return
      tpos = ptarg - sobs(1:3)
      ptarg = tpos
      lt = vnorm  ( ptarg ) / clight
      if ( q%uselt ) then
         maxitr = 1
      else if ( q%usecn ) then
         maxitr = 3
      else
         maxitr = 0
      end if
      do i = 1, maxitr
         call fast_gps ( ctx, q, targ, et + ltsign*lt, refid, refname, ssb, ptarg, lt )
         if ( ctx%failed() ) return
         tpos = ptarg - sobs(1:3)
         ptarg = tpos
         lt = vnorm  ( ptarg ) / clight
      end do
      if ( q%usestl ) then
         if ( q%xmit ) then
            call stlabx ( ctx, ptarg, sobs(4:6), tpos )
            ptarg = tpos
         else
            call stelab ( ctx, ptarg, sobs(4:6), tpos )
            ptarg = tpos
         end if
      end if
   end subroutine fast_apo

   !> The apparent position in a non-inertial frame: SPKEZP's branch for
   !> those frames (the position in J2000, rotated at the epoch of the
   !> frame's center corrected for light time).
   recursive subroutine fast_position_noninertial ( ctx, q, et, ptarg, lt )
      use pepper_math, only: mxv
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(state_query), intent(inout) :: q  !! the query
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: ptarg(3)  !! the apparent position of the target
      real(wp), intent(out) :: lt  !! the one-way light time
      integer(ip), parameter :: fj2000 = 1
      real(wp) :: postn(3)
      real(wp) :: temp(3)
      real(wp) :: xform(3, 3)
      real(wp) :: sobs(6)
      real(wp) :: ltcent
      real(wp) :: ltobs
      call fast_geo ( ctx, q, q%observer, et, fj2000, 'J2000', ssb, sobs, ltobs )
      call fast_apo ( ctx, q, q%target, et, fj2000, 'J2000', sobs, postn, lt )
      if ( ctx%failed() ) return
      if ( q%frame_center == q%observer ) then
         ltcent = 0.0_wp
      else if ( q%frame_center == q%target ) then
         ltcent = lt
      else
         call fast_apo ( ctx, q, q%frame_center, et, fj2000, 'J2000', sobs, temp, ltcent )
      end if
      if ( ctx%failed() ) return
      if ( q%xmit ) then
         ltcent = -ltcent
      end if
      call refchg ( ctx, fj2000, q%frame, et-ltcent, xform )
      if ( ctx%failed() ) return
      call mxv    ( xform,  postn,             ptarg )
   end subroutine fast_position_noninertial

end submodule pepper_queries
