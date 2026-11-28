!> Prepared queries (pepper_queries): the states of `query_state` must equal
!> SPKEZR's, and the positions of `query_position` SPKPOS's, to the last
!> bit, for every combination of target, observer, frame and aberration
!> correction, one epoch at a time and in batches, at random and at
!> sequential epochs; the rotations of `query_rotation` and
!> `query_transform` must equal PXFORM's and SXFORM's. The errors are
!> SPKEZR's, SPKPOS's and PXFORM's.
!>
!> The kernels are synthetic: a DE-like chain of type 2 segments (the
!> Earth-Moon barycenter and Mars relative to the barycenter, the Earth
!> relative to the EMB; Mars in ECLIPJ2000), the Moon in type 3, a Mars
!> satellite in type 9 (evaluated through SPKPVN), the Sun, and a body
!> covered only for part of the time. IAU_EARTH from a text PCK, and two TK
!> frames (one fixed relative to J2000, one relative to IAU_EARTH).
program test_queries

   use pepper_kinds, only: ip, wp
   use pepper_instance_module, only: pepper_instance, pepper_context, state_query, position_query, rotation_query

   implicit none

   character(*), parameter :: spk = 'test_queries.bsp', tk = 'test_queries.tk'
   real(wp), parameter :: pi = acos(-1.0_wp)
   real(wp), parameter :: t0 = 0.0_wp, t1 = 4.0e6_wp  !! coverage of the SPK
   type(pepper_instance), target :: spice
   type(pepper_context), pointer :: ctx
   integer :: failures = 0

   ctx => spice%context()
   call write_kernels()
   call spice%furnsh(spk)
   call spice%furnsh(tk)
   call check(no_error('load'), 'load the kernels')

   call test_states()
   call test_batches()
   call test_positions()
   call test_position_batches()
   call test_rotations()
   call test_errors()

   call spice%kclear()
   call delete(spk)
   call delete(tk)
   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_queries: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      if (.not. ok) then
         failures = failures + 1
         if (failures <= 30) print '(2a)', 'FAILED: ', what
      end if
   end subroutine check

   !> Print and clear the context's error, if any; true if there was none.
   logical function no_error(what)
      character(*), intent(in) :: what  !! what was called
      character(1840) :: msg
      no_error = .not. ctx%failed()
      if (.not. no_error) then
         call ctx%getmsg('LONG', msg)
         print '(3a)', what, ': ', trim(msg)
         call ctx%reset()
      end if
   end function no_error

   !> The short error message, and reset the error state.
   function short_error() result(msg)
      character(40) :: msg  !! the short message
      call ctx%getmsg('SHORT', msg)
      call ctx%reset()
   end function short_error

   !> Delete a file if it exists.
   subroutine delete(file)
      character(*), intent(in) :: file  !! the file name
      integer :: unit, iostat
      logical :: still_open  !! the file is connected to a unit (Windows can't delete it)
      inquire (file=file, opened=still_open)
      if (still_open) error stop 'a file to delete is still open (Windows cannot delete it): '//file
      open (newunit=unit, file=file, status='old', iostat=iostat)
      if (iostat == 0) close (unit, status='delete')
   end subroutine delete

   !> Chebyshev coefficients of one record of a circular motion of radius
   !> R and rate W (phase PH) about the record's midpoint TM, radius RAD
   !> (second-order Taylor expansion: positions; with VEL, also the
   !> velocities, for type 3).
   subroutine circle_record(r, w, ph, tm, rad, deg, coef, vel)
      real(wp), intent(in) :: r, w, ph, tm, rad  !! the motion and the record
      integer(ip), intent(in) :: deg  !! the polynomials' degree
      real(wp), intent(out) :: coef(:)  !! the coefficients: x, y, z (and vx, vy, vz)
      logical, intent(in) :: vel  !! include the velocity coefficients
      real(wp) :: a, ca, sa
      integer(ip) :: n
      n = deg + 1
      coef = 0
      a = w*tm + ph
      ca = cos(a)
      sa = sin(a)
      ! x = r cos(w t + ph), y = r sin(w t + ph), z = r/10 sin(...)
      coef(1) = r*ca - 0.25_wp*r*(w*rad)**2*ca
      coef(2) = -r*w*rad*sa
      coef(3) = -0.25_wp*r*(w*rad)**2*ca
      coef(n + 1) = r*sa - 0.25_wp*r*(w*rad)**2*sa
      coef(n + 2) = r*w*rad*ca
      coef(n + 3) = -0.25_wp*r*(w*rad)**2*sa
      coef(2*n + 1) = 0.1_wp*coef(n + 1)
      coef(2*n + 2) = 0.1_wp*coef(n + 2)
      coef(2*n + 3) = 0.1_wp*coef(n + 3)
      if (vel) then
         coef(3*n + 1) = -r*w*sa
         coef(3*n + 2) = -r*w*w*rad*ca
         coef(4*n + 1) = r*w*ca
         coef(4*n + 2) = -r*w*w*rad*sa
         coef(5*n + 1) = 0.1_wp*coef(4*n + 1)
         coef(5*n + 2) = 0.1_wp*coef(4*n + 2)
      end if
   end subroutine circle_record

   !> A segment of type 2 or 3 for BODY relative to CENTER over [A, B].
   subroutine write_cheb(h, type, body, center, frame, r, period, ph, a, b)
      integer(ip), intent(in) :: h, type, body, center  !! the file, the type, the bodies
      character(*), intent(in) :: frame  !! the segment's frame
      real(wp), intent(in) :: r, period, ph, a, b  !! the motion and the coverage
      integer(ip), parameter :: deg = 4, nrec = 20
      real(wp) :: cdata(6*(deg + 1)*nrec), intlen, tm
      integer(ip) :: k, m
      intlen = (b - a)/nrec
      m = 3*(deg + 1)
      if (type == 3) m = 6*(deg + 1)
      do k = 1, nrec
         tm = a + (k - 0.5_wp)*intlen
         call circle_record(r, 2*pi/period, ph, tm, intlen/2, deg, cdata((k - 1)*m + 1:k*m), type == 3)
      end do
      if (type == 2) then
         call ctx%spkw02(h, body, center, frame, a, b, 'TEST', intlen, nrec, deg, cdata, a)
      else
         call ctx%spkw03(h, body, center, frame, a, b, 'TEST', intlen, nrec, deg, cdata, a)
      end if
   end subroutine write_cheb

   !> The SPK and the text kernel of the test.
   subroutine write_kernels()
      integer(ip), parameter :: nst = 41
      real(wp) :: states(6, nst), epochs(nst), w
      integer(ip) :: h, k
      integer :: unit
      call delete(spk)
      call ctx%spkopn(spk, 'test_queries', 0_ip, h)
      call write_cheb(h, 2_ip, 3_ip, 0_ip, 'J2000', 1.5e8_wp, 3.156e7_wp, 0.3_wp, t0, t1)
      call write_cheb(h, 2_ip, 399_ip, 3_ip, 'J2000', 4.7e3_wp, 2.36e6_wp, 1.1_wp, t0, t1)
      call write_cheb(h, 3_ip, 301_ip, 3_ip, 'J2000', 3.8e5_wp, 2.36e6_wp, 4.2_wp, t0, t1)
      call write_cheb(h, 2_ip, 4_ip, 0_ip, 'ECLIPJ2000', 2.2e8_wp, 5.9e7_wp, 2.0_wp, t0, t1)
      call write_cheb(h, 2_ip, 10_ip, 0_ip, 'J2000', 7.0e5_wp, 3.7e8_wp, 0.7_wp, t0, t1)
      call write_cheb(h, 2_ip, 999_ip, 3_ip, 'J2000', 1.0e4_wp, 1.0e5_wp, 0.0_wp, t0, 1.0e6_wp)
      ! a Mars satellite in type 9 (through SPKPVN)
      w = 2*pi/1.0e5_wp
      do k = 1, nst
         epochs(k) = t0 + (t1 - t0)*(k - 1)/(nst - 1)
         states(:, k) = [1.0e4_wp*cos(w*epochs(k)), 1.0e4_wp*sin(w*epochs(k)), 0.0_wp, &
                         -1.0e4_wp*w*sin(w*epochs(k)), 1.0e4_wp*w*cos(w*epochs(k)), 0.0_wp]
      end do
      call ctx%spkw09(h, 401_ip, 4_ip, 'J2000', t0, t1, 'TEST', 3_ip, nst, states, epochs)
      call ctx%spkcls(h)
      call delete(tk)
      open (newunit=unit, file=tk, status='replace', action='write')
      write (unit, '(a)') '\begindata', &
         'BODY399_POLE_RA = ( 0.0 -0.641 0.0 )', &
         'BODY399_POLE_DEC = ( 90.0 -0.557 0.0 )', &
         'BODY399_PM = ( 190.147 360.9856235 0.0 )', &
         'BODY399_RADII = ( 6378.1 6378.1 6356.8 )', &
         'FRAME_P7_FIXED = 1400200', &
         "FRAME_1400200_NAME = 'P7_FIXED'", &
         'FRAME_1400200_CLASS = 4', &
         'FRAME_1400200_CLASS_ID = 1400200', &
         'FRAME_1400200_CENTER = 399', &
         "TKFRAME_1400200_RELATIVE = 'ECLIPJ2000'", &
         "TKFRAME_1400200_SPEC = 'ANGLES'", &
         "TKFRAME_1400200_UNITS = 'DEGREES'", &
         'TKFRAME_1400200_AXES = ( 3 1 3 )', &
         'TKFRAME_1400200_ANGLES = ( 10 20 30 )', &
         'FRAME_P7_TOPO = 1400201', &
         "FRAME_1400201_NAME = 'P7_TOPO'", &
         'FRAME_1400201_CLASS = 4', &
         'FRAME_1400201_CLASS_ID = 1400201', &
         'FRAME_1400201_CENTER = 399', &
         "TKFRAME_1400201_RELATIVE = 'IAU_EARTH'", &
         "TKFRAME_1400201_SPEC = 'ANGLES'", &
         "TKFRAME_1400201_UNITS = 'DEGREES'", &
         'TKFRAME_1400201_AXES = ( 3 2 3 )', &
         'TKFRAME_1400201_ANGLES = ( -40 -50 180 )', &
         '\begintext'
      close (unit)
   end subroutine write_kernels

   !> Every combination of target, observer, frame and correction, at
   !> random epochs and at sequential epochs: query_state = spkezr.
   subroutine test_states()
      character(*), parameter :: bodies(8) = [character(24) :: '301', 'EARTH', 'MARS BARYCENTER', '401', &
                                               'EARTH BARYCENTER', 'SUN', 'SOLAR SYSTEM BARYCENTER', '999']
      character(*), parameter :: frames(5) = [character(12) :: 'J2000', 'ECLIPJ2000', 'IAU_EARTH', &
                                               'P7_FIXED', 'P7_TOPO']
      character(*), parameter :: corrs(9) = [character(8) :: 'NONE', 'LT', 'LT+S', 'CN', 'CN+S', &
                                              'XLT', 'XLT+S', 'XCN', 'XCN+S']
      type(state_query) :: q
      real(wp) :: et, state(6), state2(6), lt, lt2, r(1)
      integer :: it, io, ifr, ic, k, ncases, nbad
      character(200) :: what
      ncases = 0
      nbad = 0
      do it = 1, size(bodies)
         do io = 1, size(bodies)
            if (it == io) cycle
            do ifr = 1, size(frames)
               do ic = 1, size(corrs)
                  call ctx%prepare_state(q, trim(bodies(it)), trim(bodies(io)), trim(frames(ifr)), &
                                         trim(corrs(ic)))
                  write (what, '(7a)') trim(bodies(it)), ' from ', trim(bodies(io)), ' in ', &
                     trim(frames(ifr)), ', ', trim(corrs(ic))
                  call check(no_error('prepare ' // trim(what)), 'prepare ' // trim(what))
                  do k = 1, 12
                     if (k <= 6) then
                        call random_number(r)
                        et = 5.0e5_wp + r(1)*3.0e6_wp
                     else
                        et = 2.0e6_wp + 50.0_wp*k  ! sequential: the same records
                     end if
                     if (bodies(it) == '999' .or. bodies(io) == '999') et = 5.0e5_wp + 10.0_wp*k
                     call ctx%query_state(q, et, state, lt)
                     call ctx%spkezr(trim(bodies(it)), et, trim(frames(ifr)), trim(corrs(ic)), &
                                     trim(bodies(io)), state2, lt2)
                     ncases = ncases + 1
                     if (.not. (no_error(trim(what)) .and. all(state == state2) .and. lt == lt2)) then
                        nbad = nbad + 1
                        if (nbad <= 10) print '(a, es24.16, 2(1x, es24.16))', 'mismatch: ' // trim(what) // &
                           ' et, dx, dlt =', et, maxval(abs(state - state2)), lt - lt2
                     end if
                  end do
               end do
            end do
         end do
      end do
      print '(a, i0, a)', '   ', ncases, ' states compared with SPKEZR'
      call check(nbad == 0, 'query_state = spkezr, to the last bit')
   end subroutine test_states

   !> A batch gives the states of single queries, at sequential and at
   !> random epochs.
   subroutine test_batches()
      integer, parameter :: n = 500
      type(state_query) :: q, q2
      real(wp) :: ets(n), states(6, n), lts(n), st(6), lt, r(n)
      integer :: k
      logical :: same
      call random_number(r)
      ets(1:n/2) = 1.0e6_wp + 37.0_wp*[(k, k = 1, n/2)]
      ets(n/2 + 1:) = 5.0e5_wp + 3.0e6_wp*r(n/2 + 1:)
      call ctx%prepare_state(q, 'MOON', 'EARTH', 'IAU_EARTH', 'CN+S')
      call ctx%prepare_state(q2, 'MOON', 'EARTH', 'IAU_EARTH', 'CN+S')
      call ctx%query_state(q, ets, states, lts)
      same = .true.
      do k = 1, n
         call ctx%query_state(q2, ets(k), st, lt)
         same = same .and. all(st == states(:, k)) .and. lt == lts(k)
      end do
      call check(no_error('batch') .and. same, 'a batch gives the single queries'' states')
      call ctx%spkezr('MOON', ets(n), 'IAU_EARTH', 'CN+S', 'EARTH', st, lt)
      call check(all(st == states(:, n)) .and. lt == lts(n), 'batch = spkezr')
      ! without the light times; a states array too small
      call ctx%query_state(q, ets, states)
      call check(no_error('batch without lts'), 'a batch without the light times')
      call ctx%query_state(q, ets, states(:, 1:n - 1))
      call check(short_error() == 'SPICE(INVALIDDIMENSION)', 'a batch with too small an array')
   end subroutine test_batches

   !> Every combination of target, observer, frame and correction (also
   !> spelled as SPKAPO accepts them: lower case, blanks), at random epochs
   !> and at sequential epochs: query_position = spkpos.
   subroutine test_positions()
      character(*), parameter :: bodies(8) = [character(24) :: '301', 'EARTH', 'MARS BARYCENTER', '401', &
                                               'EARTH BARYCENTER', 'SUN', 'SOLAR SYSTEM BARYCENTER', '999']
      character(*), parameter :: frames(5) = [character(12) :: 'J2000', 'ECLIPJ2000', 'IAU_EARTH', &
                                               'P7_FIXED', 'P7_TOPO']
      character(*), parameter :: corrs(11) = [character(8) :: 'NONE', 'LT', 'LT+S', 'CN', 'CN+S', &
                                               'XLT', 'XLT+S', 'XCN', 'XCN+S', ' none', 'cn + s']
      type(position_query) :: q
      real(wp) :: et, pos(3), pos2(3), lt, lt2, r(1)
      integer :: it, io, ifr, ic, k, ncases, nbad
      character(200) :: what
      ncases = 0
      nbad = 0
      do it = 1, size(bodies)
         do io = 1, size(bodies)
            if (it == io) cycle
            do ifr = 1, size(frames)
               do ic = 1, size(corrs)
                  call ctx%prepare_position(q, trim(bodies(it)), trim(bodies(io)), trim(frames(ifr)), &
                                            trim(corrs(ic)))
                  write (what, '(7a)') trim(bodies(it)), ' from ', trim(bodies(io)), ' in ', &
                     trim(frames(ifr)), ', ', trim(corrs(ic))
                  call check(no_error('prepare position ' // trim(what)), 'prepare position ' // trim(what))
                  do k = 1, 12
                     if (k <= 6) then
                        call random_number(r)
                        et = 5.0e5_wp + r(1)*3.0e6_wp
                     else
                        et = 2.0e6_wp + 50.0_wp*k  ! sequential: the same records
                     end if
                     if (bodies(it) == '999' .or. bodies(io) == '999') et = 5.0e5_wp + 10.0_wp*k
                     call ctx%query_position(q, et, pos, lt)
                     call ctx%spkpos(trim(bodies(it)), et, trim(frames(ifr)), trim(corrs(ic)), &
                                     trim(bodies(io)), pos2, lt2)
                     ncases = ncases + 1
                     if (.not. (no_error(trim(what)) .and. all(pos == pos2) .and. lt == lt2)) then
                        nbad = nbad + 1
                        if (nbad <= 10) print '(a, es24.16, 2(1x, es24.16))', 'mismatch: ' // trim(what) // &
                           ' et, dx, dlt =', et, maxval(abs(pos - pos2)), lt - lt2
                     end if
                  end do
               end do
            end do
         end do
      end do
      print '(a, i0, a)', '   ', ncases, ' positions compared with SPKPOS'
      call check(nbad == 0, 'query_position = spkpos, to the last bit')
   end subroutine test_positions

   !> A position batch gives the positions of single queries, at
   !> sequential and at random epochs.
   subroutine test_position_batches()
      integer, parameter :: n = 500
      type(position_query) :: q, q2
      real(wp) :: ets(n), positions(3, n), lts(n), p(3), lt, r(n)
      integer :: k
      logical :: same
      call random_number(r)
      ets(1:n/2) = 1.0e6_wp + 37.0_wp*[(k, k = 1, n/2)]
      ets(n/2 + 1:) = 5.0e5_wp + 3.0e6_wp*r(n/2 + 1:)
      call ctx%prepare_position(q, 'MOON', 'EARTH', 'IAU_EARTH', 'CN+S')
      call ctx%prepare_position(q2, 'MOON', 'EARTH', 'IAU_EARTH', 'CN+S')
      call ctx%query_position(q, ets, positions, lts)
      same = .true.
      do k = 1, n
         call ctx%query_position(q2, ets(k), p, lt)
         same = same .and. all(p == positions(:, k)) .and. lt == lts(k)
      end do
      call check(no_error('position batch') .and. same, 'a position batch gives the single queries'' positions')
      call ctx%spkpos('MOON', ets(n), 'IAU_EARTH', 'CN+S', 'EARTH', p, lt)
      call check(all(p == positions(:, n)) .and. lt == lts(n), 'position batch = spkpos')
      ! without the light times; a positions array too small, or of states
      call ctx%query_position(q, ets, positions)
      call check(no_error('position batch without lts'), 'a position batch without the light times')
      call ctx%query_position(q, ets, positions(:, 1:n - 1))
      call check(short_error() == 'SPICE(INVALIDDIMENSION)', 'a position batch with too small an array')
   end subroutine test_position_batches

   !> query_rotation = pxform and query_transform = sxform, for time
   !> dependent and constant rotations; a constant rotation is recomputed
   !> when the pool changes.
   subroutine test_rotations()
      character(*), parameter :: frames(5) = [character(12) :: 'J2000', 'ECLIPJ2000', 'IAU_EARTH', &
                                               'P7_FIXED', 'P7_TOPO']
      type(rotation_query) :: q
      real(wp) :: rot(3, 3), rot2(3, 3), xf(6, 6), xf2(6, 6), et, ets(50), rots(3, 3, 50), xfs(6, 6, 50)
      integer :: i, j, k
      logical :: same
      same = .true.
      do i = 1, size(frames)
         do j = 1, size(frames)
            call ctx%prepare_rotation(q, trim(frames(i)), trim(frames(j)))
            do k = 1, 5
               et = 1.0e6_wp*k + 123.456_wp
               call ctx%query_rotation(q, et, rot)
               call ctx%pxform(trim(frames(i)), trim(frames(j)), et, rot2)
               call ctx%query_transform(q, et, xf)
               call ctx%sxform(trim(frames(i)), trim(frames(j)), et, xf2)
               same = same .and. all(rot == rot2) .and. all(xf == xf2)
            end do
         end do
      end do
      call check(no_error('rotations') .and. same, 'query_rotation = pxform, query_transform = sxform')

      ! a constant rotation (ECLIPJ2000 to a TK frame on ECLIPJ2000)
      call ctx%prepare_rotation(q, 'J2000', 'P7_FIXED')
      call ctx%query_rotation(q, 1.0e6_wp, rot)
      call check(q%constant .and. q%have_rotation, 'J2000 to P7_FIXED is constant')
      call ctx%prepare_rotation(q, 'J2000', 'P7_TOPO')
      call ctx%query_rotation(q, 1.0e6_wp, rot)
      call check(.not. q%constant, 'J2000 to P7_TOPO depends on time')
      ! the pool changes: the constant rotation is recomputed
      call ctx%prepare_rotation(q, 'J2000', 'P7_FIXED')
      call ctx%query_rotation(q, 1.0e6_wp, rot)
      call spice%pdpool('TKFRAME_1400200_ANGLES', 3_ip, [11.0_wp, 20.0_wp, 30.0_wp])
      call ctx%query_rotation(q, 1.0e6_wp, rot)
      call ctx%pxform('J2000', 'P7_FIXED', 1.0e6_wp, rot2)
      call check(no_error('pool change') .and. all(rot == rot2), 'a constant rotation after a pool change')
      call spice%pdpool('TKFRAME_1400200_ANGLES', 3_ip, [10.0_wp, 20.0_wp, 30.0_wp])

      ! batches
      ets = [(2.0e4_wp*k, k = 1, 50)]
      call ctx%prepare_rotation(q, 'J2000', 'P7_TOPO')
      call ctx%query_rotation(q, ets, rots)
      call ctx%query_transform(q, ets, xfs)
      same = .true.
      do k = 1, 50
         call ctx%pxform('J2000', 'P7_TOPO', ets(k), rot2)
         call ctx%sxform('J2000', 'P7_TOPO', ets(k), xf2)
         same = same .and. all(rots(:, :, k) == rot2) .and. all(xfs(:, :, k) == xf2)
      end do
      call check(no_error('rotation batches') .and. same, 'rotation and transformation batches')
   end subroutine test_rotations

   !> The errors of SPKEZR and PXFORM.
   subroutine test_errors()
      type(state_query) :: q
      type(position_query) :: p
      type(rotation_query) :: r
      real(wp) :: state(6), pos(3), lt, rot(3, 3)
      call ctx%prepare_state(q, 'NO_SUCH_BODY', 'EARTH', 'J2000', 'NONE')
      call check(short_error() == 'SPICE(IDCODENOTFOUND)', 'prepare_state: unknown target')
      call ctx%prepare_state(q, 'MOON', 'NO_SUCH_BODY', 'J2000', 'NONE')
      call check(short_error() == 'SPICE(IDCODENOTFOUND)', 'prepare_state: unknown observer')
      call ctx%prepare_state(q, 'MOON', 'EARTH', 'NO_SUCH_FRAME', 'NONE')
      call check(short_error() == 'SPICE(UNKNOWNFRAME)', 'prepare_state: unknown frame')
      call ctx%prepare_state(q, 'MOON', 'EARTH', 'J2000', 'BAD')
      call check(short_error() /= ' ', 'prepare_state: invalid correction')
      call ctx%query_state(q, 1.0e6_wp, state, lt)
      call check(short_error() == 'SPICE(NOTINITIALIZED)', 'query_state: not prepared')
      ! no data: SPKEZR's error
      call ctx%prepare_state(q, '999', 'EARTH', 'J2000', 'LT+S')
      call ctx%query_state(q, 3.0e6_wp, state, lt)
      call check(short_error() == 'SPICE(SPKINSUFFDATA)', 'query_state: no data')
      ! positions: SPKPOS's errors; SPKPOS has no relativistic correction
      call ctx%prepare_position(p, 'NO_SUCH_BODY', 'EARTH', 'J2000', 'NONE')
      call check(short_error() == 'SPICE(IDCODENOTFOUND)', 'prepare_position: unknown target')
      call ctx%prepare_position(p, 'MOON', 'NO_SUCH_BODY', 'J2000', 'NONE')
      call check(short_error() == 'SPICE(IDCODENOTFOUND)', 'prepare_position: unknown observer')
      call ctx%prepare_position(p, 'MOON', 'EARTH', 'NO_SUCH_FRAME', 'NONE')
      call check(short_error() == 'SPICE(UNKNOWNFRAME)', 'prepare_position: unknown frame')
      call ctx%prepare_position(p, 'MOON', 'EARTH', 'J2000', 'RL')
      call check(short_error() == 'SPICE(SPKINVALIDOPTION)', 'prepare_position: invalid correction')
      call ctx%spkpos('MOON', 1.0e6_wp, 'J2000', 'RL', 'EARTH', pos, lt)
      call check(short_error() == 'SPICE(SPKINVALIDOPTION)', 'spkpos: the same error')
      call ctx%query_position(p, 1.0e6_wp, pos, lt)
      call check(short_error() == 'SPICE(NOTINITIALIZED)', 'query_position: not prepared')
      call ctx%prepare_position(p, '999', 'EARTH', 'J2000', 'LT+S')
      call ctx%query_position(p, 3.0e6_wp, pos, lt)
      call check(short_error() == 'SPICE(SPKINSUFFDATA)', 'query_position: no data')
      call ctx%prepare_position(p, '999', 'EARTH', 'IAU_EARTH', 'NONE')
      call ctx%query_position(p, 3.0e6_wp, pos, lt)
      call check(short_error() == 'SPICE(SPKINSUFFDATA)', 'query_position: no data (geometric)')
      call ctx%prepare_rotation(r, 'J2000', 'NO_SUCH_FRAME')
      call check(short_error() == 'SPICE(UNKNOWNFRAME)', 'prepare_rotation: unknown frame')
      call ctx%query_rotation(r, 1.0e6_wp, rot)
      call check(short_error() == 'SPICE(NOTINITIALIZED)', 'query_rotation: not prepared')
   end subroutine test_errors

end program test_queries
