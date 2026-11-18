!> The legacy interface's routines of the subsystem math (118 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_math

   use pepper_bounds, only: ubpl, ubel

   implicit none

   private
   public :: approx, axisar, azlrec, brcktd, brckti, chbder, chbint, chbval, cyllat, cylrec, &
      cylsph, dcbrt, det, drdcyl, drdlat, drdsph, ducrss, dvcrss, dvdot, dvhat, dvnorm, el2cgv, &
      even, exact, frame, gcd, ident, insang, invert, latcyl, latrec, latsph, mequ, mequg, mtxm, &
      mtxmg, mtxv, mtxvg, mxm, mxmg, mxmt, mxmtg, mxv, mxvg, odd, opsgnd, opsgni, pl2nvc, pl2nvp, &
      pl2psv, pltexp, pltnrm, polyds, q2m, qdq2av, qxq, radrec, rav2xf, recazl, reccyl, reclat, &
      recrad, recsph, sharpr, smsgnd, smsgni, sphcyl, sphlat, sphrec, trace, traceg, ucrss, &
      unorm, unormg, vadd, vaddg, vcrss, vdist, vdistg, vdot, vdotg, vequ, vequg, vhat, vhatg, &
      vhatip, vlcom, vlcom3, vlcomg, vminug, vminus, vnorm, vnormg, vpack, vperp, vproj, vprojg, &
      vrel, vrelg, vrotv, vscl, vsclg, vsclip, vsep, vsepg, vsub, vsubg, vtmv, vtmvg, vupack, &
      vzero, vzerog, xf2rav, xpose, xposeg, xpsgip, zzmsxf, zzrxr

contains

      !> Return .TRUE. if two double precision numbers are equal to
      !> within some tolerance.
      !>
      !> Legacy interface: calls approx of module pepper_math.
      logical function approx ( x, y, tol )
      use pepper_math, only: pepper_approx => approx
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Double precision numbers.
      real(wp) :: y  !! Double precision numbers.
      real(wp) :: tol  !! Tolerance.
      approx = pepper_approx ( x, y, tol )
      end function approx

      !> Construct a rotation matrix that rotates vectors by a specified
      !> angle about a specified axis.
      !>
      !> Legacy interface: calls axisar of module pepper_math.
      subroutine axisar ( axis, angle, r )
      use pepper_math, only: pepper_axisar => axisar
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: axis  ( 3 )  !! Rotation axis.
      real(wp) :: angle  !! Rotation angle, in radians.
      real(wp) :: r     ( 3, 3 )  !! Rotation matrix corresponding to AXIS and ANGLE.
      ! PEPPER: axis is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_axisar ( (axis), angle, r )
      end subroutine axisar

      !> Convert from range, azimuth and elevation of a point to
      !> rectangular coordinates.
      !>
      !> Legacy interface: calls azlrec of module pepper_math.
      subroutine azlrec ( range, az, el, azccw, elplsz, rectan )
      use pepper_math, only: pepper_azlrec => azlrec
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: range  !! Distance of the point from the origin.
      real(wp) :: az  !! Azimuth in radians.
      real(wp) :: el  !! Elevation in radians.
      logical :: azccw  !! Flag indicating how azimuth is measured.
      logical :: elplsz  !! Flag indicating how elevation is measured.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      call pepper_azlrec ( range, az, el, azccw, elplsz, rectan )
      end subroutine azlrec

      !> Bracket a double precision number. That is, given a number and an
      !> acceptable interval, make sure that the number is contained in the
      !> interval. (If the number is already in the interval, leave it
      !> alone. If not, set it to the nearest endpoint of the interval.)
      !>
      !> Legacy interface: calls brcktd of module pepper_math.
      real(wp) function brcktd ( number, end1, end2 )
      use pepper_math, only: pepper_brcktd => brcktd
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: number  !! Number to be bracketed.
      real(wp) :: end1  !! One of the bracketing endpoints for NUMBER.
      real(wp) :: end2  !! The other bracketing endpoint for NUMBER.
      brcktd = pepper_brcktd ( number, end1, end2 )
      end function brcktd

      !> Bracket an integer number. That is, given a number and an
      !> acceptable interval, make sure that the number is contained in the
      !> interval. (If the number is already in the interval, leave it
      !> alone. If not, set it to the nearest endpoint of the interval.)
      !>
      !> Legacy interface: calls brckti of module pepper_math.
      integer(ip)  function brckti ( number, end1, end2 )
      use pepper_math, only: pepper_brckti => brckti
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Number to be bracketed.
      integer(ip) :: end1  !! One of the bracketing endpoints for NUMBER.
      integer(ip) :: end2  !! The other bracketing endpoint for NUMBER.
      brckti = pepper_brckti ( number, end1, end2 )
      end function brckti

      !> Return the value of a polynomial and its first NDERIV
      !> derivatives, evaluated at the input X, using the coefficients of
      !> the Chebyshev expansion of the polynomial.
      !>
      !> Legacy interface: calls chbder of module pepper_math.
      subroutine chbder ( cp, degp, x2s, x, nderiv, partdp, dpdxs )
      use pepper_math, only: pepper_chbder => chbder
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: cp     (      * )  !! DEGP+1 Chebyshev polynomial coefficients.
      integer(ip) :: degp  !! Degree of polynomial.
      real(wp) :: x2s    (      2 )  !! Transformation parameters of polynomial.
      real(wp) :: x  !! Value for which the polynomial is to be evaluated.
      integer(ip) :: nderiv  !! The number of derivatives to compute.
      real(wp) :: partdp ( 3, 0:* )  !! Workspace provided for computing derivatives.
      real(wp) :: dpdxs  (    0:* )  !! Array of the derivatives of the polynomial.
      ! PEPPER: x2s is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_chbder ( cp, degp, (x2s), x, nderiv, partdp, dpdxs )
      end subroutine chbder

      !> Return the value of a polynomial and its derivative, evaluated at
      !> the input X, using the coefficients of the Chebyshev expansion of
      !> the polynomial.
      !>
      !> Legacy interface: calls chbint of module pepper_math.
      subroutine chbint ( cp, degp, x2s, x, p, dpdx )
      use pepper_math, only: pepper_chbint => chbint
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: cp  ( * )  !! DEGP+1 Chebyshev polynomial coefficients.
      integer(ip) :: degp  !! Degree of polynomial.
      real(wp) :: x2s ( 2 )  !! Transformation parameters of polynomial.
      real(wp) :: x  !! Value for which the polynomial is to be evaluated
      real(wp) :: p  !! Value of the polynomial at X
      real(wp) :: dpdx  !! Value of the derivative of the polynomial at X
      ! PEPPER: x2s is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_chbint ( cp, degp, (x2s), x, p, dpdx )
      end subroutine chbint

      !> Return the value of a polynomial evaluated at the input X using
      !> the coefficients for the Chebyshev expansion of the polynomial.
      !>
      !> Legacy interface: calls chbval of module pepper_math.
      subroutine chbval ( cp, degp, x2s, x, p )
      use pepper_math, only: pepper_chbval => chbval
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: cp  ( * )  !! DEGP+1 Chebyshev polynomial coefficients.
      integer(ip) :: degp  !! Degree of polynomial.
      real(wp) :: x2s ( 2 )  !! Transformation parameters of polynomial.
      real(wp) :: x  !! Value for which the polynomial is to be evaluated.
      real(wp) :: p  !! Value of the polynomial at X.
      ! PEPPER: x2s is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_chbval ( cp, degp, (x2s), x, p )
      end subroutine chbval

      !> Convert from cylindrical to latitudinal coordinates.
      !>
      !> Legacy interface: calls cyllat of module pepper_math.
      subroutine cyllat ( r, clon, z,  radius, lon, lat )
      use pepper_math, only: pepper_cyllat => cyllat
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: r  !! Distance of point from Z axis.
      real(wp) :: clon  !! Cylindrical angle of point from XZ plane(radians).
      real(wp) :: z  !! Height of point above XY plane.
      real(wp) :: radius  !! Distance of point from origin.
      real(wp) :: lon  !! Longitude of point (radians).
      real(wp) :: lat  !! Latitude of point (radians).
      call pepper_cyllat ( r, clon, z, radius, lon, lat )
      end subroutine cyllat

      !> Convert from cylindrical to rectangular coordinates.
      !>
      !> Legacy interface: calls cylrec of module pepper_math.
      subroutine cylrec ( r, clon, z,  rectan )
      use pepper_math, only: pepper_cylrec => cylrec
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: r  !! Distance of a point from Z axis.
      real(wp) :: clon  !! Angle (radians) of a point from XZ plane
      real(wp) :: z  !! Height of a point above XY plane.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      call pepper_cylrec ( r, clon, z, rectan )
      end subroutine cylrec

      !> Convert from cylindrical to spherical coordinates.
      !>
      !> Legacy interface: calls cylsph of module pepper_math.
      subroutine cylsph ( r, clon, z,  radius, colat, slon )
      use pepper_math, only: pepper_cylsph => cylsph
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: r  !! Distance of point from Z axis.
      real(wp) :: clon  !! Angle (radians) of point from XZ plane.
      real(wp) :: z  !! Height of point above XY plane.
      real(wp) :: radius  !! Distance of point from origin.
      real(wp) :: colat  !! Polar angle (co-latitude in radians) of point.
      real(wp) :: slon  !! Azimuthal angle (longitude) of point (radians).
      call pepper_cylsph ( r, clon, z, radius, colat, slon )
      end subroutine cylsph

      !> Return the cube root of a double precision number.
      !>
      !> Legacy interface: calls dcbrt of module pepper_math.
      real(wp) function  dcbrt ( x )
      use pepper_math, only: pepper_dcbrt => dcbrt
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Number whose cube root is desired.
      real(wp), parameter :: third = 1.0_wp/3.0_wp
      dcbrt = pepper_dcbrt ( x )
      end function dcbrt

      !> Compute the determinant of a double precision 3x3 matrix.
      !>
      !> Legacy interface: calls det of module pepper_math.
      real(wp) function  det ( m1 )
      use pepper_math, only: pepper_det => det
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: m1 ( 3,3 )  !! Matrix whose determinant is to be found.
      det = pepper_det ( m1 )
      end function det

      !> Compute the Jacobian matrix of the transformation from
      !> cylindrical to rectangular coordinates.
      !>
      !> Legacy interface: calls drdcyl of module pepper_math.
      subroutine drdcyl ( r, clon, z, jacobi )
      use pepper_math, only: pepper_drdcyl => drdcyl
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: r  !! Distance of a point from the origin.
      real(wp) :: clon  !! Angle of the point from the XZ plane in radians.
      real(wp) :: z  !! Height of the point above the XY plane.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      call pepper_drdcyl ( r, clon, z, jacobi )
      end subroutine drdcyl

      !> Compute the Jacobian matrix of the transformation from
      !> latitudinal to rectangular coordinates.
      !>
      !> Legacy interface: calls drdlat of module pepper_math.
      subroutine drdlat ( r, lon, lat, jacobi )
      use pepper_math, only: pepper_drdlat => drdlat
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: r  !! Distance of a point from the origin.
      real(wp) :: lon  !! Angle of the point from the XZ plane in radians.
      real(wp) :: lat  !! Angle of the point from the XY plane in radians.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      integer(ip), parameter :: dlat = 3
      call pepper_drdlat ( r, lon, lat, jacobi )
      end subroutine drdlat

      !> Compute the Jacobian matrix of the transformation from spherical
      !> to rectangular coordinates.
      !>
      !> Legacy interface: calls drdsph of module pepper_math.
      subroutine drdsph ( r, colat, slon, jacobi )
      use pepper_math, only: pepper_drdsph => drdsph
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: r  !! Distance of a point from the origin.
      real(wp) :: colat  !! Angle of the point from the positive Z-axis.
      real(wp) :: slon  !! Angle of the point from the XY plane.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      integer(ip), parameter :: dcolat = 2
      call pepper_drdsph ( r, colat, slon, jacobi )
      end subroutine drdsph

      !> Compute the unit vector parallel to the cross product of
      !> two 3-dimensional vectors and the derivative of this unit vector.
      !>
      !> Legacy interface: calls ducrss of module pepper_math.
      subroutine ducrss ( s1, s2, sout )
      use pepper_math, only: pepper_ducrss => ducrss
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: s1   ( 6 )  !! Left hand state for cross product and derivative.
      real(wp) :: s2   ( 6 )  !! Right hand state for cross product and derivative.
      real(wp) :: sout ( 6 )  !! Unit vector and derivative of the cross product.
      ! PEPPER: s1, s2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_ducrss ( (s1), (s2), sout )
      end subroutine ducrss

      !> Compute the cross product of two 3-dimensional vectors
      !> and the derivative of this cross product.
      !>
      !> Legacy interface: calls dvcrss of module pepper_math.
      subroutine dvcrss ( s1, s2, sout )
      use pepper_math, only: pepper_dvcrss => dvcrss
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: s1   ( 6 )  !! Left hand state for cross product and derivative.
      real(wp) :: s2   ( 6 )  !! Right hand state for cross product and derivative.
      real(wp) :: sout ( 6 )  !! State associated with cross product of positions.
      ! PEPPER: s1, s2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_dvcrss ( (s1), (s2), sout )
      end subroutine dvcrss

      !> Compute the derivative of the dot product of two double
      !> precision position vectors.
      !>
      !> Legacy interface: calls dvdot of module pepper_math.
      real(wp) function dvdot ( s1, s2 )
      use pepper_math, only: pepper_dvdot => dvdot
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: s1 ( 6 )  !! First state vector in the dot product.
      real(wp) :: s2 ( 6 )  !! Second state vector in the dot product.
      dvdot = pepper_dvdot ( s1, s2 )
      end function dvdot

      !> Find the unit vector corresponding to a state vector and the
      !> derivative of the unit vector.
      !>
      !> Legacy interface: calls dvhat of module pepper_math.
      subroutine dvhat ( s1, sout )
      use pepper_math, only: pepper_dvhat => dvhat
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: s1   ( 6 )  !! State to be normalized.
      real(wp) :: sout ( 6 )  !! Unit vector S1 / |S1|, and its time derivative.
      integer(ip), parameter :: pos = 1
      integer(ip), parameter :: vel = 4
      ! PEPPER: s1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_dvhat ( (s1), sout )
      end subroutine dvhat

       !> Calculate the derivative of the norm of a 3-vector.
       !>
       !> Legacy interface: calls dvnorm of module pepper_math.
       real(wp) function dvnorm( state )
      use pepper_math, only: pepper_dvnorm => dvnorm
       use pepper_kinds, only: wp
      implicit none
      real(wp) :: state ( 6 )  !! A 6-vector composed of three coordinates and their
                                         !! derivatives.
      dvnorm = pepper_dvnorm ( state )
      end function dvnorm

      !> Convert a SPICE ellipse to a center vector and two generating
      !> vectors. The selected generating vectors are semi-axes of the
      !> ellipse.
      !>
      !> Legacy interface: calls el2cgv of module pepper_math.
      subroutine el2cgv ( ellips, center, smajor, sminor )
      use pepper_math, only: pepper_el2cgv => el2cgv
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: ellips ( ubel )  !! A SPICE ellipse.
      real(wp) :: center (    3 )  !! Center and semi-axes of ELLIPS.
      real(wp) :: smajor (    3 )  !! Center and semi-axes of ELLIPS.
      real(wp) :: sminor (    3 )  !! Center and semi-axes of ELLIPS.
      integer(ip), parameter :: ctrpos = 1
      integer(ip), parameter :: majpos = 4
      integer(ip), parameter :: minpos = 7
      ! PEPPER: ellips is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_el2cgv ( (ellips), center, smajor, sminor )
      end subroutine el2cgv

      !> Determine whether an integer is even.
      !>
      !> Legacy interface: calls even of module pepper_math.
      logical function even ( ival )
      use pepper_math, only: pepper_even => even
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: ival  !! The integer in question.
      even = pepper_even ( ival )
      end function even

      !> Round an input double precision number to a specified exact value
      !> if the number and the value are equal to within some tolerance.
      !>
      !> Legacy interface: calls exact of module pepper_math.
      real(wp) function exact ( number, value, tol )
      use pepper_math, only: pepper_exact => exact
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: number  !! Double precision number.
      real(wp) :: value  !! Target value.
      real(wp) :: tol  !! Tolerance.
      exact = pepper_exact ( number, value, tol )
      end function exact

      !> Build a right handed orthonormal frame (x,y,z) from a
      !> 3-dimensional input vector, where the X-axis of the resulting
      !> frame is parallel to the original input vector.
      !>
      !> Legacy interface: calls frame of module pepper_math.
      subroutine frame ( x, y, z )
      use pepper_math, only: pepper_frame => frame
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x ( 3 )  !! Input vector. A parallel unit vector on output.
      real(wp) :: y ( 3 )  !! Unit vector in the plane orthogonal to X.
      real(wp) :: z ( 3 )  !! Unit vector given by the cross product <X,Y>.
      call pepper_frame ( x, y, z )
      end subroutine frame

      !> Return the greatest common divisor of two integers.
      !>
      !> Legacy interface: calls gcd of module pepper_math.
      integer(ip) function gcd ( a, b )
      use pepper_math, only: pepper_gcd => gcd
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: a  !! Any integer
      integer(ip) :: b  !! Any integer
      gcd = pepper_gcd ( a, b )
      end function gcd

      !> Return the 3x3 identity matrix.
      !>
      !> Legacy interface: calls ident of module pepper_math.
      subroutine ident ( matrix )
      use pepper_math, only: pepper_ident => ident
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: matrix ( 3, 3 )  !! The 3x3 identity matrix.
      call pepper_ident ( matrix )
      end subroutine ident

      !> Determine if a given vector lies inside the solid tetrahedral
      !> angle determined by 3 vectors. If it does, return the
      !> point where the scale factor such that SCALE*V lies in the
      !> plane spanned by E1, E2, and E3.
      !>
      !> Legacy interface: calls insang of module pepper_math.
      subroutine insang ( v, e1, e2, e3, found, scale )
      use pepper_math, only: pepper_insang => insang
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v     ( 3 )  !! Vector to test for "betweenness"
      real(wp) :: e1    ( 3 )  !! First edge of the tetrahedral angle
      real(wp) :: e2    ( 3 )  !! Second edge of the tetrahedral angle
      real(wp) :: e3    ( 3 )  !! Third edge of the tetrahedral angle
      logical :: found  !! Indicates whether V lies in the solid angle
      real(wp) :: scale  !! Scale times V is in the triangle E1,E2,E3
      ! PEPPER: v, e1, e2, e3 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_insang ( (v), (e1), (e2), (e3), found, scale )
      end subroutine insang

      !> Generate the inverse of a 3x3 matrix.
      !>
      !> Legacy interface: calls invert of module pepper_math.
      subroutine invert ( m, mout )
      use pepper_math, only: pepper_invert => invert
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: m    ( 3,3 )  !! Matrix to be inverted.
      real(wp) :: mout ( 3,3 )  !! Inverted matrix (M)**-1. If M is singular, then MOUT will
                                       !! be the zero matrix.
      ! PEPPER: m is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_invert ( (m), mout )
      end subroutine invert

      !> Convert from latitudinal coordinates to cylindrical coordinates.
      !>
      !> Legacy interface: calls latcyl of module pepper_math.
      subroutine latcyl ( radius, lon, lat, r, clon, z )
      use pepper_math, only: pepper_latcyl => latcyl
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: radius  !! Distance of a point from the origin.
      real(wp) :: lon  !! Angle of the point from the XZ plane in radians.
      real(wp) :: lat  !! Angle of the point from the XY plane in radians.
      real(wp) :: r  !! Distance of the point from the Z axis.
      real(wp) :: clon  !! Angle of the point from the XZ plane in radians.
      real(wp) :: z  !! Height of the point above the XY plane.
      call pepper_latcyl ( radius, lon, lat, r, clon, z )
      end subroutine latcyl

      !> Convert from latitudinal coordinates to rectangular coordinates.
      !>
      !> Legacy interface: calls latrec of module pepper_math.
      subroutine latrec ( radius, lon, lat, rectan )
      use pepper_math, only: pepper_latrec => latrec
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: radius  !! Distance of a point from the origin.
      real(wp) :: lon  !! Longitude of point in radians.
      real(wp) :: lat  !! Latitude of point in radians.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      call pepper_latrec ( radius, lon, lat, rectan )
      end subroutine latrec

      !> Convert from latitudinal coordinates to spherical coordinates.
      !>
      !> Legacy interface: calls latsph of module pepper_math.
      subroutine latsph ( radius, lon, lat,  rho, colat, slon )
      use pepper_math, only: pepper_latsph => latsph
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: radius  !! Distance of a point from the origin.
      real(wp) :: lon  !! Angle of the point from the XZ plane in radians.
      real(wp) :: lat  !! Angle of the point from the XY plane in radians.
      real(wp) :: rho  !! Distance of the point from the origin.
      real(wp) :: colat  !! Angle of the point from positive Z axis (radians).
      real(wp) :: slon  !! Angle of the point from the XZ plane (radians).
      call pepper_latsph ( radius, lon, lat, rho, colat, slon )
      end subroutine latsph

      !> Set one double precision 3x3 matrix equal to another.
      !>
      !> Legacy interface: calls mequ of module pepper_math.
      subroutine mequ ( m1, mout )
      use pepper_math, only: pepper_mequ => mequ
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: m1   ( 3, 3 )  !! Input matrix.
      real(wp) :: mout ( 3, 3 )  !! Output matrix equal to M1.
      ! PEPPER: m1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mequ ( (m1), mout )
      end subroutine mequ

      !> Set one double precision matrix of arbitrary size equal to
      !> another.
      !>
      !> Legacy interface: calls mequg of module pepper_math.
      subroutine mequg ( m1, nr, nc, mout )
      use pepper_math, only: pepper_mequg => mequg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nr  !! Row dimension of M1 (and also MOUT).
      integer(ip) :: nc  !! Column dimension of M1 (and also MOUT).
      real(wp) :: m1   ( nr,nc )  !! Input matrix.
      real(wp) :: mout ( nr,nc )  !! Output matrix equal to M1.
      ! PEPPER: m1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mequg ( (m1), nr, nc, mout )
      end subroutine mequg

      !> Multiply the transpose of a 3x3 matrix and a 3x3 matrix.
      !>
      !> Legacy interface: calls mtxm of module pepper_math.
      subroutine mtxm ( m1, m2, mout )
      use pepper_math, only: pepper_mtxm => mtxm
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: m1   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: m2   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: mout ( 3,3 )  !! 3x3 double precision matrix which is the product (M1**T) *
                                       !! M2.
      ! PEPPER: m1, m2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mtxm ( (m1), (m2), mout )
      end subroutine mtxm

      !> Multiply the transpose of a matrix with another matrix,
      !> both of arbitrary size. (The dimensions of the matrices must be
      !> compatible with this multiplication.)
      !>
      !> Legacy interface: calls mtxmg of module pepper_math.
      subroutine mtxmg ( m1, m2, nc1, nr1r2, nc2, mout )
      use pepper_math, only: pepper_mtxmg => mtxmg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nc1  !! Column dimension of M1 and row dimension of MOUT.
      integer(ip) :: nr1r2  !! Row dimension of both M1 and M2.
      integer(ip) :: nc2  !! Column dimension of both M2 and MOUT.
      real(wp) :: m1   ( nr1r2,nc1 )  !! Left-hand matrix whose transpose is to be
                                             !! multiplied.
      real(wp) :: m2   ( nr1r2,nc2 )  !! Right-hand matrix to be multiplied.
      real(wp) :: mout ( nc1,  nc2 )  !! Product matrix M1**T * M2.
      ! PEPPER: m1, m2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mtxmg ( (m1), (m2), nc1, nr1r2, nc2, mout )
      end subroutine mtxmg

      !> Multiply the transpose of a 3x3 matrix on the left with a vector
      !> on the right.
      !>
      !> Legacy interface: calls mtxv of module pepper_math.
      subroutine mtxv ( m, vin, vout )
      use pepper_math, only: pepper_mtxv => mtxv
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: m      ( 3,3 )  !! 3X3 double precision matrix.
      real(wp) :: vin    (   3 )  !! 3-dimensional double precision vector.
      real(wp) :: vout   (   3 )  !! 3-dimensional double precision vector. VOUT is the
                                        !! product M**T * VIN.
      ! PEPPER: m, vin are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mtxv ( (m), (vin), vout )
      end subroutine mtxv

      !> Multiply the transpose of a matrix and a vector of
      !> arbitrary size.
      !>
      !> Legacy interface: calls mtxvg of module pepper_math.
      subroutine mtxvg ( m1, v2, nc1, nr1r2, vout )
      use pepper_math, only: pepper_mtxvg => mtxvg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nc1  !! Column dimension of M1 and length of VOUT.
      integer(ip) :: nr1r2  !! Row dimension of M1 and length of V2.
      real(wp) :: m1   ( nr1r2,nc1 )  !! Left-hand matrix whose transpose is to be
                                             !! multiplied.
      real(wp) :: v2   ( nr1r2     )  !! Right-hand vector to be multiplied.
      real(wp) :: vout (       nc1 )  !! Product vector M1**T * V2.
      ! PEPPER: m1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mtxvg ( (m1), (v2), nc1, nr1r2, vout )
      end subroutine mtxvg

      !> Multiply two 3x3 matrices.
      !>
      !> Legacy interface: calls mxm of module pepper_math.
      subroutine mxm ( m1, m2, mout )
      use pepper_math, only: pepper_mxm => mxm
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: m1   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: m2   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: mout ( 3,3 )  !! The 3x3 double precision matrix product M1*M2.
      ! PEPPER: m1, m2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mxm ( (m1), (m2), mout )
      end subroutine mxm

      !> Multiply two double precision matrices of arbitrary size.
      !>
      !> Legacy interface: calls mxmg of module pepper_math.
      subroutine mxmg ( m1, m2, nr1, nc1r2, nc2, mout )
      use pepper_math, only: pepper_mxmg => mxmg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nr1  !! Row dimension of M1 (and also MOUT).
      integer(ip) :: nc1r2  !! Column dimension of M1 and row dimension of M2.
      integer(ip) :: nc2  !! Column dimension of M2 (and also MOUT).
      real(wp) :: m1   ( nr1,   nc1r2 )  !! NR1   x NC1R2 double precision matrix.
      real(wp) :: m2   ( nc1r2, nc2   )  !! NC1R2 x NC2   double precision matrix.
      real(wp) :: mout ( nr1,   nc2   )  !! NR1 x NC2 double precision matrix.
      ! PEPPER: m1, m2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mxmg ( (m1), (m2), nr1, nc1r2, nc2, mout )
      end subroutine mxmg

      !> Multiply a 3x3 matrix and the transpose of another 3x3 matrix.
      !>
      !> Legacy interface: calls mxmt of module pepper_math.
      subroutine mxmt ( m1, m2, mout )
      use pepper_math, only: pepper_mxmt => mxmt
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: m1   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: m2   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: mout ( 3,3 )  !! The product M1 times transpose of M2.
      ! PEPPER: m1, m2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mxmt ( (m1), (m2), mout )
      end subroutine mxmt

      !> Multiply a matrix and the transpose of a matrix, both of
      !> arbitrary size.
      !>
      !> Legacy interface: calls mxmtg of module pepper_math.
      subroutine mxmtg ( m1, m2, nr1, nc1c2, nr2, mout )
      use pepper_math, only: pepper_mxmtg => mxmtg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nr1  !! Row dimension of M1 and row dimension of MOUT.
      integer(ip) :: nc1c2  !! Column dimension of M1 and column dimension of M2.
      integer(ip) :: nr2  !! Row dimension of M2 and column dimension of MOUT.
      real(wp) :: m1   ( nr1,nc1c2 )  !! Left-hand matrix to be multiplied.
      real(wp) :: m2   ( nr2,nc1c2 )  !! Right-hand matrix whose transpose is to be
                                             !! multiplied.
      real(wp) :: mout ( nr1,nr2   )  !! Product matrix M1 * M2**T.
      ! PEPPER: m1, m2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mxmtg ( (m1), (m2), nr1, nc1c2, nr2, mout )
      end subroutine mxmtg

      !> Multiply a 3x3 double precision matrix with a 3-dimensional
      !> double precision vector.
      !>
      !> Legacy interface: calls mxv of module pepper_math.
      subroutine mxv ( m, vin, vout )
      use pepper_math, only: pepper_mxv => mxv
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: m      ( 3, 3 )  !! 3x3 double precision matrix.
      real(wp) :: vin    (    3 )  !! 3-dimensional double precision vector.
      real(wp) :: vout   (    3 )  !! 3-dimensional double precision vector. VOUT is the
                                          !! product M*VIN.
      ! PEPPER: m, vin are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mxv ( (m), (vin), vout )
      end subroutine mxv

      !> Multiply a matrix and a vector of arbitrary size.
      !>
      !> Legacy interface: calls mxvg of module pepper_math.
      subroutine mxvg ( m1, v2, nr1, nc1r2, vout )
      use pepper_math, only: pepper_mxvg => mxvg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nr1  !! Row dimension of M1 and length of VOUT.
      integer(ip) :: nc1r2  !! Column dimension of M1 and length of V2.
      real(wp) :: m1   ( nr1,nc1r2 )  !! Left-hand matrix to be multiplied.
      real(wp) :: v2   (     nc1r2 )  !! Right-hand vector to be multiplied.
      real(wp) :: vout ( nr1       )  !! Product vector M1*V2.
      ! PEPPER: m1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_mxvg ( (m1), (v2), nr1, nc1r2, vout )
      end subroutine mxvg

      !> Determine whether an integer is odd.
      !>
      !> Legacy interface: calls odd of module pepper_math.
      logical function  odd ( ival )
      use pepper_math, only: pepper_odd => odd
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: ival  !! The integer in question.
      odd = pepper_odd ( ival )
      end function odd

      !> Return .TRUE. if two given double precision numbers have opposite
      !> signs.
      !>
      !> Legacy interface: calls opsgnd of module pepper_math.
      logical function opsgnd  ( x, y )
      use pepper_math, only: pepper_opsgnd => opsgnd
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! A double precision number
      real(wp) :: y  !! A double precision number
      opsgnd = pepper_opsgnd ( x, y )
      end function opsgnd

      !> Return .TRUE. if two given integer numbers have opposite signs.
      !>
      !> Legacy interface: calls opsgni of module pepper_math.
      logical function opsgni  ( x, y )
      use pepper_math, only: pepper_opsgni => opsgni
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: x  !! An integer.
      integer(ip) :: y  !! An integer.
      opsgni = pepper_opsgni ( x, y )
      end function opsgni

      !> Return a unit normal vector and constant that define a specified
      !> plane.
      !>
      !> Legacy interface: calls pl2nvc of module pepper_math.
      subroutine pl2nvc ( plane, normal, konst )
      use pepper_math, only: pepper_pl2nvc => pl2nvc
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: plane  ( ubpl )  !! A SPICE plane.
      real(wp) :: normal (    3 )  !! A normal vector and constant defining the geometric
                                             !! plane represented by PLANE.
      real(wp) :: konst  !! A normal vector and constant defining the geometric plane
                                   !! represented by PLANE.
      integer(ip), parameter :: nmlpos = 1
      integer(ip), parameter :: conpos = 4
      ! PEPPER: plane is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_pl2nvc ( (plane), normal, konst )
      end subroutine pl2nvc

      !> Return a unit normal vector and point that define a specified
      !> plane.
      !>
      !> Legacy interface: calls pl2nvp of module pepper_math.
      subroutine pl2nvp ( plane, normal, point )
      use pepper_math, only: pepper_pl2nvp => pl2nvp
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: plane  ( ubpl )  !! A SPICE plane.
      real(wp) :: normal (    3 )  !! A unit normal vector and point that define PLANE.
      real(wp) :: point  (    3 )  !! A unit normal vector and point that define PLANE.
      ! PEPPER: plane is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_pl2nvp ( (plane), normal, point )
      end subroutine pl2nvp

      !> Return a point and two orthogonal spanning vectors that generate
      !> a specified plane.
      !>
      !> Legacy interface: calls pl2psv of module pepper_math.
      subroutine pl2psv ( plane, point, span1, span2 )
      use pepper_math, only: pepper_pl2psv => pl2psv
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: plane ( ubpl )  !! A SPICE plane.
      real(wp) :: point (    3 )  !! A point in the input plane and two vectors spanning
                                            !! the input plane.
      real(wp) :: span1 (    3 )  !! A point in the input plane and two vectors spanning
                                            !! the input plane.
      real(wp) :: span2 (    3 )  !! A point in the input plane and two vectors spanning
                                            !! the input plane.
      ! PEPPER: plane is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_pl2psv ( (plane), point, span1, span2 )
      end subroutine pl2psv

      !> Expand a triangular plate by a specified amount. The expanded
      !> plate is co-planar with, and has the same orientation as, the
      !> original. The centroids of the two plates coincide.
      !>
      !> Legacy interface: calls pltexp of module pepper_math.
      subroutine pltexp ( iverts, delta, overts )
      use pepper_math, only: pepper_pltexp => pltexp
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: iverts ( 3, 3 )  !! Vertices of the plate to be expanded.
      real(wp) :: delta  !! Fraction by which the plate is to be expanded.
      real(wp) :: overts ( 3, 3 )  !! Vertices of the expanded plate.
      ! PEPPER: iverts is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_pltexp ( (iverts), delta, overts )
      end subroutine pltexp

      !> Compute an outward normal vector of a triangular plate.
      !> The vector does not necessarily have unit length.
      !>
      !> Legacy interface: calls pltnrm of module pepper_math.
      subroutine pltnrm ( v1, v2, v3, normal )
      use pepper_math, only: pepper_pltnrm => pltnrm
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1     ( 3 )  !! Vertices of a plate.
      real(wp) :: v2     ( 3 )  !! Vertices of a plate.
      real(wp) :: v3     ( 3 )  !! Vertices of a plate.
      real(wp) :: normal ( 3 )  !! Plate's outward normal vector.
      ! PEPPER: v1, v2, v3 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_pltnrm ( (v1), (v2), (v3), normal )
      end subroutine pltnrm

      !> Compute the value of a polynomial and its first
      !> NDERIV derivatives at the value T.
      !>
      !> Legacy interface: calls polyds of module pepper_math.
      subroutine polyds ( coeffs, deg, nderiv, t, p )
      use pepper_math, only: pepper_polyds => polyds
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: coeffs ( 0:* )  !! Coefficients of the polynomial to be evaluated.
      integer(ip) :: deg  !! Degree of the polynomial to be evaluated.
      integer(ip) :: nderiv  !! Number of derivatives to compute.
      real(wp) :: t  !! Point to evaluate the polynomial and derivatives
      real(wp) :: p      ( 0:* )  !! Value of polynomial and derivatives.
      call pepper_polyds ( coeffs, deg, nderiv, t, p )
      end subroutine polyds

      !> Find the rotation matrix corresponding to a specified unit
      !> quaternion.
      !>
      !> Legacy interface: calls q2m of module pepper_math.
      subroutine q2m ( q, r )
      use pepper_math, only: pepper_q2m => q2m
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: q ( 0 : 3 )  !! A unit quaternion.
      real(wp) :: r ( 3,  3 )  !! A rotation matrix corresponding to Q.
      ! PEPPER: q is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_q2m ( (q), r )
      end subroutine q2m

      !> Derive angular velocity from a unit quaternion and its derivative
      !> with respect to time.
      !>
      !> Legacy interface: calls qdq2av of module pepper_math.
      subroutine qdq2av ( q, dq, av )
      use pepper_math, only: pepper_qdq2av => qdq2av
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: q    ( 0 : 3 )  !! Unit SPICE quaternion.
      real(wp) :: dq   ( 0 : 3 )  !! Derivative of Q with respect to time.
      real(wp) :: av   ( 3 )  !! Angular velocity defined by Q and DQ.
      ! PEPPER: q, dq are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_qdq2av ( (q), (dq), av )
      end subroutine qdq2av

      !> Multiply two quaternions.
      !>
      !> Legacy interface: calls qxq of module pepper_math.
      subroutine qxq ( q1, q2, qout )
      use pepper_math, only: pepper_qxq => qxq
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: q1   ( 0 : 3 )  !! First SPICE quaternion factor.
      real(wp) :: q2   ( 0 : 3 )  !! Second SPICE quaternion factor.
      real(wp) :: qout ( 0 : 3 )  !! Product of Q1 and Q2.
      ! PEPPER: q1, q2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_qxq ( (q1), (q2), qout )
      end subroutine qxq

      !> Convert from range, right ascension, and declination to
      !> rectangular coordinates.
      !>
      !> Legacy interface: calls radrec of module pepper_math.
      subroutine radrec ( range, ra, dec, rectan )
      use pepper_math, only: pepper_radrec => radrec
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: range  !! Distance of a point from the origin.
      real(wp) :: ra  !! Right ascension of point in radians.
      real(wp) :: dec  !! Declination of point in radians.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      call pepper_radrec ( range, ra, dec, rectan )
      end subroutine radrec

      !> Determine a state transformation matrix from a rotation matrix
      !> and the angular velocity of the rotation.
      !>
      !> Legacy interface: calls rav2xf of module pepper_math.
      subroutine rav2xf ( rot, av, xform )
      use pepper_math, only: pepper_rav2xf => rav2xf
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: rot    ( 3, 3 )  !! Rotation matrix.
      real(wp) :: av     ( 3    )  !! Angular velocity vector.
      real(wp) :: xform  ( 6, 6 )  !! State transformation associated with ROT and AV.
      ! PEPPER: rot, av are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_rav2xf ( (rot), (av), xform )
      end subroutine rav2xf

      !> Convert rectangular coordinates of a point to range, azimuth and
      !> elevation.
      !>
      !> Legacy interface: calls recazl of module pepper_math.
      subroutine recazl ( rectan, azccw, elplsz, range, az, el )
      use pepper_math, only: pepper_recazl => recazl
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      logical :: azccw  !! Flag indicating how Azimuth is measured.
      logical :: elplsz  !! Flag indicating how Elevation is measured.
      real(wp) :: range  !! Distance of the point from the origin.
      real(wp) :: az  !! Azimuth in radians.
      real(wp) :: el  !! Elevation in radians.
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_recazl ( (rectan), azccw, elplsz, range, az, el )
      end subroutine recazl

      !> Convert from rectangular to cylindrical coordinates.
      !>
      !> Legacy interface: calls reccyl of module pepper_math.
      subroutine reccyl ( rectan, r, clon, z )
      use pepper_math, only: pepper_reccyl => reccyl
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rectan(3)  !! Rectangular coordinates of a point.
      real(wp) :: r  !! Distance of the point from Z axis.
      real(wp) :: clon  !! Angle (radians) of the point from XZ plane
      real(wp) :: z  !! Height of the point above XY plane.
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_reccyl ( (rectan), r, clon, z )
      end subroutine reccyl

      !> Convert from rectangular coordinates to latitudinal coordinates.
      !>
      !> Legacy interface: calls reclat of module pepper_math.
      subroutine reclat ( rectan, radius, lon, lat )
      use pepper_math, only: pepper_reclat => reclat
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      real(wp) :: radius  !! Distance of a point from the origin.
      real(wp) :: lon  !! Longitude of point in radians.
      real(wp) :: lat  !! Latitude of point in radians.
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_reclat ( (rectan), radius, lon, lat )
      end subroutine reclat

      !> Convert rectangular coordinates to range, right ascension,
      !> and declination.
      !>
      !> Legacy interface: calls recrad of module pepper_math.
      subroutine recrad ( rectan, range, ra, dec )
      use pepper_math, only: pepper_recrad => recrad
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp) :: range  !! Distance of the point from the origin.
      real(wp) :: ra  !! Right ascension in radians.
      real(wp) :: dec  !! Declination in radians.
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_recrad ( (rectan), range, ra, dec )
      end subroutine recrad

      !> Convert from rectangular coordinates to spherical coordinates.
      !>
      !> Legacy interface: calls recsph of module pepper_math.
      subroutine recsph ( rectan, r, colat, slon  )
      use pepper_math, only: pepper_recsph => recsph
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp) :: r  !! Distance of the point from the origin.
      real(wp) :: colat  !! Angle of the point from the Z-axis (radians)
      real(wp) :: slon  !! Longitude of the point (radians).
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_recsph ( (rectan), r, colat, slon )
      end subroutine recsph

      !> Adjust the columns of a matrix that is "nearly" a rotation
      !> so that they are numerically unit length and orthogonal,
      !> going from left to right in the usual printed presentation
      !> of a matrix.
      !>
      !> Legacy interface: calls sharpr of module pepper_math.
      subroutine sharpr ( rot )
      use pepper_math, only: pepper_sharpr => sharpr
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: rot ( 3, 3 )  !! The rotation matrix to be sharpened.
      call pepper_sharpr ( rot )
      end subroutine sharpr

      !> Return .TRUE. if two given double precision numbers have the same
      !> sign.
      !>
      !> Legacy interface: calls smsgnd of module pepper_math.
      logical function smsgnd ( x, y )
      use pepper_math, only: pepper_smsgnd => smsgnd
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! A double precision number
      real(wp) :: y  !! A double precision number
      smsgnd = pepper_smsgnd ( x, y )
      end function smsgnd

      !> Return .TRUE. if two given integer numbers have the same sign.
      !>
      !> Legacy interface: calls smsgni of module pepper_math.
      logical function smsgni ( x, y )
      use pepper_math, only: pepper_smsgni => smsgni
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: x  !! An integer.
      integer(ip) :: y  !! An integer.
      smsgni = pepper_smsgni ( x, y )
      end function smsgni

      !> Convert from spherical coordinates to cylindrical coordinates.
      !>
      !> Legacy interface: calls sphcyl of module pepper_math.
      subroutine sphcyl ( radius, colat, slon, r, clon, z )
      use pepper_math, only: pepper_sphcyl => sphcyl
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: radius  !! Distance of point from origin.
      real(wp) :: colat  !! Polar angle (co-latitude in radians) of point.
      real(wp) :: slon  !! Azimuthal angle (longitude) of point (radians).
      real(wp) :: r  !! Distance of point from Z axis.
      real(wp) :: clon  !! Angle (radians) of point from XZ plane.
      real(wp) :: z  !! Height of point above XY plane.
      call pepper_sphcyl ( radius, colat, slon, r, clon, z )
      end subroutine sphcyl

      !> Convert from spherical coordinates to latitudinal coordinates.
      !>
      !> Legacy interface: calls sphlat of module pepper_math.
      subroutine sphlat ( r, colat, slon, radius, lon, lat )
      use pepper_math, only: pepper_sphlat => sphlat
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: r  !! Distance of the point from the origin.
      real(wp) :: colat  !! Angle of the point from positive Z axis (radians).
      real(wp) :: slon  !! Angle of the point from the XZ plane (radians).
      real(wp) :: radius  !! Distance of a point from the origin
      real(wp) :: lon  !! Angle of the point from the XZ plane in radians
      real(wp) :: lat  !! Angle of the point from the XY plane in radians
      call pepper_sphlat ( r, colat, slon, radius, lon, lat )
      end subroutine sphlat

      !> Convert from spherical coordinates to rectangular coordinates.
      !>
      !> Legacy interface: calls sphrec of module pepper_math.
      subroutine sphrec ( r, colat, slon, rectan  )
      use pepper_math, only: pepper_sphrec => sphrec
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: r  !! Distance of a point from the origin.
      real(wp) :: colat  !! Angle of the point from the Z-axis in radians.
      real(wp) :: slon  !! Angle of the point from the XZ plane in radians.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      call pepper_sphrec ( r, colat, slon, rectan )
      end subroutine sphrec

      !> Return the trace of a 3x3 matrix.
      !>
      !> Legacy interface: calls trace of module pepper_math.
      real(wp) function  trace ( matrix )
      use pepper_math, only: pepper_trace => trace
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: matrix ( 3, 3 )  !! 3x3 matrix of double precision numbers.
      trace = pepper_trace ( matrix )
      end function trace

      !> Return the trace of a square matrix of arbitrary dimension.
      !>
      !> Legacy interface: calls traceg of module pepper_math.
      real(wp) function  traceg ( matrix, ndim )
      use pepper_math, only: pepper_traceg => traceg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of the matrix.
      real(wp) :: matrix ( ndim,ndim )  !! NDIM x NDIM matrix of double precision numbers.
      traceg = pepper_traceg ( matrix, ndim )
      end function traceg

      !> Compute the normalized cross product of two 3-vectors.
      !>
      !> Legacy interface: calls ucrss of module pepper_math.
      subroutine ucrss ( v1, v2, vout )
      use pepper_math, only: pepper_ucrss => ucrss
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! Left vector for cross product.
      real(wp) :: v2   ( 3 )  !! Right vector for cross product.
      real(wp) :: vout ( 3 )  !! Normalized cross product of V1 and V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_ucrss ( (v1), (v2), vout )
      end subroutine ucrss

      !> Normalize a double precision 3-vector and return its magnitude.
      !>
      !> Legacy interface: calls unorm of module pepper_math.
      subroutine unorm ( v1, vout, vmag )
      use pepper_math, only: pepper_unorm => unorm
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! Vector to be normalized.
      real(wp) :: vout ( 3 )  !! Unit vector V1 / |V1|.
      real(wp) :: vmag  !! Magnitude of V1, i.e. |V1|.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_unorm ( (v1), vout, vmag )
      end subroutine unorm

      !> Normalize a double precision vector of arbitrary dimension and
      !> return its magnitude.
      !>
      !> Legacy interface: calls unormg of module pepper_math.
      subroutine unormg ( v1, ndim, vout, vmag )
      use pepper_math, only: pepper_unormg => unormg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of V1 (and also VOUT).
      real(wp) :: v1   ( ndim )  !! Vector to be normalized.
      real(wp) :: vout ( ndim )  !! Unit vector V1 / ||V1||.
      real(wp) :: vmag  !! Magnitude of V1, i.e. ||V1||.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_unormg ( (v1), ndim, vout, vmag )
      end subroutine unormg

      !> Add two double precision 3-dimensional vectors.
      !>
      !> Legacy interface: calls vadd of module pepper_math.
      subroutine vadd ( v1, v2, vout )
      use pepper_math, only: pepper_vadd => vadd
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! First vector to be added.
      real(wp) :: v2   ( 3 )  !! Second vector to be added.
      real(wp) :: vout ( 3 )  !! Sum vector, V1 + V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vadd ( (v1), (v2), vout )
      end subroutine vadd

      !> Add two vectors of arbitrary dimension.
      !>
      !> Legacy interface: calls vaddg of module pepper_math.
      subroutine vaddg ( v1, v2, ndim, vout )
      use pepper_math, only: pepper_vaddg => vaddg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of V1, V2, and VOUT.
      real(wp) :: v1   ( ndim )  !! First vector to be added.
      real(wp) :: v2   ( ndim )  !! Second vector to be added.
      real(wp) :: vout ( ndim )  !! Sum vector, V1 + V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vaddg ( (v1), (v2), ndim, vout )
      end subroutine vaddg

      !> Compute the cross product of two 3-dimensional vectors.
      !>
      !> Legacy interface: calls vcrss of module pepper_math.
      subroutine vcrss ( v1, v2, vout )
      use pepper_math, only: pepper_vcrss => vcrss
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! Left hand vector for cross product.
      real(wp) :: v2   ( 3 )  !! Right hand vector for cross product.
      real(wp) :: vout ( 3 )  !! Cross product V1 x V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vcrss ( (v1), (v2), vout )
      end subroutine vcrss

      !> Return the distance between two three-dimensional vectors.
      !>
      !> Legacy interface: calls vdist of module pepper_math.
      real(wp) function  vdist ( v1, v2 )
      use pepper_math, only: pepper_vdist => vdist
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1 ( 3 )  !! Two 3-vectors.
      real(wp) :: v2 ( 3 )  !! Two 3-vectors.
      vdist = pepper_vdist ( v1, v2 )
      end function vdist

      !> Return the distance between two vectors of arbitrary dimension.
      !>
      !> Legacy interface: calls vdistg of module pepper_math.
      real(wp) function vdistg ( v1, v2, ndim )
      use pepper_math, only: pepper_vdistg => vdistg
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v1   ( * )  !! Two vectors of arbitrary dimension.
      real(wp) :: v2   ( * )  !! Two vectors of arbitrary dimension.
      integer(ip) :: ndim  !! The common dimension of V1 and V2
      vdistg = pepper_vdistg ( v1, v2, ndim )
      end function vdistg

      !> Compute the dot product of two double precision, 3-dimensional
      !> vectors.
      !>
      !> Legacy interface: calls vdot of module pepper_math.
      real(wp) function vdot ( v1, v2 )
      use pepper_math, only: pepper_vdot => vdot
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1 ( 3 )  !! First vector in the dot product.
      real(wp) :: v2 ( 3 )  !! Second vector in the dot product.
      vdot = pepper_vdot ( v1, v2 )
      end function vdot

      !> Compute the dot product of two vectors of arbitrary dimension.
      !>
      !> Legacy interface: calls vdotg of module pepper_math.
      real(wp) function vdotg ( v1, v2, ndim )
      use pepper_math, only: pepper_vdotg => vdotg
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v1  ( * )  !! First vector in the dot product.
      real(wp) :: v2  ( * )  !! Second vector in the dot product.
      integer(ip) :: ndim  !! Dimension of V1 and V2.
      vdotg = pepper_vdotg ( v1, v2, ndim )
      end function vdotg

      !> Make one double precision 3-dimensional vector equal to
      !> another.
      !>
      !> Legacy interface: calls vequ of module pepper_math.
      subroutine vequ ( vin, vout )
      use pepper_math, only: pepper_vequ => vequ
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: vin   ( 3 )  !! Double precision 3-dimensional vector.
      real(wp) :: vout  ( 3 )  !! Double precision 3-dimensional vector set equal to VIN.
      ! PEPPER: vin is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vequ ( (vin), vout )
      end subroutine vequ

      !> Make one double precision vector of arbitrary dimension equal
      !> to another.
      !>
      !> Legacy interface: calls vequg of module pepper_math.
      subroutine vequg ( vin, ndim, vout )
      use pepper_math, only: pepper_vequg => vequg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of VIN (and also VOUT).
      real(wp) :: vin   ( ndim )  !! Double precision n-dimensional vector.
      real(wp) :: vout  ( ndim )  !! Double precision n-dimensional vector set equal to VIN.
      ! PEPPER: vin is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vequg ( (vin), ndim, vout )
      end subroutine vequg

      !> Find the unit vector along a double precision 3-dimensional
      !> vector.
      !>
      !> Legacy interface: calls vhat of module pepper_math.
      subroutine vhat ( v1, vout )
      use pepper_math, only: pepper_vhat => vhat
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! Vector to be normalized.
      real(wp) :: vout ( 3 )  !! Unit vector V1 / |V1|.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vhat ( (v1), vout )
      end subroutine vhat

      !> Find the unit vector along a double precision vector of
      !> arbitrary dimension.
      !>
      !> Legacy interface: calls vhatg of module pepper_math.
      subroutine vhatg ( v1, ndim, vout )
      use pepper_math, only: pepper_vhatg => vhatg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of V1 (and also VOUT).
      real(wp) :: v1   ( ndim )  !! Vector to be normalized.
      real(wp) :: vout ( ndim )  !! Unit vector along V1.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vhatg ( (v1), ndim, vout )
      end subroutine vhatg

      !> Scale a three-dimensional vector to unit length.
      !>
      !> Legacy interface: calls vhatip of module pepper_math.
      subroutine vhatip ( v )
      use pepper_math, only: pepper_vhatip => vhatip
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v ( 3 )  !! Vector to be normalized/unit vector.
      call pepper_vhatip ( v )
      end subroutine vhatip

      !> Compute a vector linear combination of two double precision,
      !> 3-dimensional vectors.
      !>
      !> Legacy interface: calls vlcom of module pepper_math.
      subroutine vlcom ( a, v1, b, v2, sum )
      use pepper_math, only: pepper_vlcom => vlcom
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Coefficient of V1.
      real(wp) :: v1  ( 3 )  !! Vector in 3-space.
      real(wp) :: b  !! Coefficient of V2.
      real(wp) :: v2  ( 3 )  !! Vector in 3-space.
      real(wp) :: sum ( 3 )  !! Linear vector combination A*V1 + B*V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vlcom ( a, (v1), b, (v2), sum )
      end subroutine vlcom

      !> Compute the vector linear combination of three double precision
      !> 3-dimensional vectors.
      !>
      !> Legacy interface: calls vlcom3 of module pepper_math.
      subroutine vlcom3 ( a, v1, b, v2, c, v3, sum )
      use pepper_math, only: pepper_vlcom3 => vlcom3
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Coefficient of V1.
      real(wp) :: v1  ( 3 )  !! Vector in 3-space.
      real(wp) :: b  !! Coefficient of V2.
      real(wp) :: v2  ( 3 )  !! Vector in 3-space.
      real(wp) :: c  !! Coefficient of V3.
      real(wp) :: v3  ( 3 )  !! Vector in 3-space.
      real(wp) :: sum ( 3 )  !! Linear vector combination A*V1 + B*V2 + C*V3.
      ! PEPPER: v1, v2, v3 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vlcom3 ( a, (v1), b, (v2), c, (v3), sum )
      end subroutine vlcom3

      !> Compute a vector linear combination of two double precision
      !> vectors of arbitrary dimension.
      !>
      !> Legacy interface: calls vlcomg of module pepper_math.
      subroutine vlcomg ( n, a, v1, b, v2, sum )
      use pepper_math, only: pepper_vlcomg => vlcomg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Dimension of vector space.
      real(wp) :: a  !! Coefficient of V1.
      real(wp) :: v1  ( n )  !! Vector in N-space.
      real(wp) :: b  !! Coefficient of V2.
      real(wp) :: v2  ( n )  !! Vector in N-space.
      real(wp) :: sum ( n )  !! Linear vector combination A*V1 + B*V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vlcomg ( n, a, (v1), b, (v2), sum )
      end subroutine vlcomg

      !> Negate a double precision vector of arbitrary dimension.
      !>
      !> Legacy interface: calls vminug of module pepper_math.
      subroutine vminug ( vin, ndim, vout )
      use pepper_math, only: pepper_vminug => vminug
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of VIN (and also VOUT).
      real(wp) :: vin   ( ndim )  !! n-dimensional vector to be negated.
      real(wp) :: vout  ( ndim )  !! Negated vector -V1.
      ! PEPPER: vin is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vminug ( (vin), ndim, vout )
      end subroutine vminug

      !> Negate a double precision 3-dimensional vector.
      !>
      !> Legacy interface: calls vminus of module pepper_math.
      subroutine vminus ( v1, vout )
      use pepper_math, only: pepper_vminus => vminus
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! Vector to be negated.
      real(wp) :: vout ( 3 )  !! Negated vector -V1.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vminus ( (v1), vout )
      end subroutine vminus

      !> Compute the magnitude of a double precision 3-dimensional
      !> vector.
      !>
      !> Legacy interface: calls vnorm of module pepper_math.
      real(wp) function vnorm ( v1 )
      use pepper_math, only: pepper_vnorm => vnorm
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1 ( 3 )  !! Vector whose magnitude is to be found.
      vnorm = pepper_vnorm ( v1 )
      end function vnorm

      !> Compute the magnitude of a double precision vector of arbitrary
      !> dimension.
      !>
      !> Legacy interface: calls vnormg of module pepper_math.
      real(wp) function vnormg ( v1, ndim )
      use pepper_math, only: pepper_vnormg => vnormg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of V1.
      real(wp) :: v1   ( ndim )  !! Vector whose magnitude is to be found.
      vnormg = pepper_vnormg ( v1, ndim )
      end function vnormg

      !> Pack three scalar components into a vector.
      !>
      !> Legacy interface: calls vpack of module pepper_math.
      subroutine vpack ( x, y, z, v )
      use pepper_math, only: pepper_vpack => vpack
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Scalar components of a vector.
      real(wp) :: y  !! Scalar components of a vector.
      real(wp) :: z  !! Scalar components of a vector.
      real(wp) :: v ( 3 )  !! Equivalent vector.
      call pepper_vpack ( x, y, z, v )
      end subroutine vpack

      !> Find the component of a vector that is perpendicular to a second
      !> vector. All vectors are 3-dimensional.
      !>
      !> Legacy interface: calls vperp of module pepper_math.
      subroutine vperp ( a, b, p )
      use pepper_math, only: pepper_vperp => vperp
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a ( 3 )  !! The vector whose orthogonal component is sought.
      real(wp) :: b ( 3 )  !! The vector used as the orthogonal reference.
      real(wp) :: p ( 3 )  !! The component of A orthogonal to B.
      ! PEPPER: a, b are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vperp ( (a), (b), p )
      end subroutine vperp

      !> Compute the projection of one 3-dimensional vector onto another
      !> 3-dimensional vector.
      !>
      !> Legacy interface: calls vproj of module pepper_math.
      subroutine vproj ( a, b, p )
      use pepper_math, only: pepper_vproj => vproj
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a ( 3 )  !! The vector to be projected.
      real(wp) :: b ( 3 )  !! The vector onto which A is to be projected.
      real(wp) :: p ( 3 )  !! The projection of A onto B.
      ! PEPPER: a, b are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vproj ( (a), (b), p )
      end subroutine vproj

      !> Compute the projection of one vector onto another vector. All
      !> vectors are of arbitrary dimension.
      !>
      !> Legacy interface: calls vprojg of module pepper_math.
      subroutine vprojg ( a, b, ndim, p )
      use pepper_math, only: pepper_vprojg => vprojg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of A, B, and P.
      real(wp) :: a ( ndim )  !! The vector to be projected.
      real(wp) :: b ( ndim )  !! The vector onto which A is to be projected.
      real(wp) :: p ( ndim )  !! The projection of A onto B.
      ! PEPPER: a, b are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vprojg ( (a), (b), ndim, p )
      end subroutine vprojg

      !> Return the relative difference between two 3-dimensional vectors.
      !>
      !> Legacy interface: calls vrel of module pepper_math.
      real(wp) function vrel ( v1, v2  )
      use pepper_math, only: pepper_vrel => vrel
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1 ( 3 )  !! Input vectors.
      real(wp) :: v2 ( 3 )  !! Input vectors.
      vrel = pepper_vrel ( v1, v2 )
      end function vrel

      !> Return the relative difference between two vectors of general
      !> dimension.
      !>
      !> Legacy interface: calls vrelg of module pepper_math.
      real(wp) function vrelg ( v1, v2, ndim )
      use pepper_math, only: pepper_vrelg => vrelg
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v1 ( * )  !! Input vectors.
      real(wp) :: v2 ( * )  !! Input vectors.
      integer(ip) :: ndim  !! Dimension of V1 and V2.
      vrelg = pepper_vrelg ( v1, v2, ndim )
      end function vrelg

      !> Rotate a vector about a specified axis vector by a specified
      !> angle and return the rotated vector.
      !>
      !> Legacy interface: calls vrotv of module pepper_math.
      subroutine vrotv ( v, axis, theta, r )
      use pepper_math, only: pepper_vrotv => vrotv
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v    ( 3 )  !! Vector to be rotated.
      real(wp) :: axis ( 3 )  !! Axis of the rotation.
      real(wp) :: theta  !! Angle of rotation (radians).
      real(wp) :: r    ( 3 )  !! Result of rotating V about AXIS by THETA.
      ! PEPPER: v, axis are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vrotv ( (v), (axis), theta, r )
      end subroutine vrotv

      !> Multiply a scalar and a double precision 3-dimensional vector.
      !>
      !> Legacy interface: calls vscl of module pepper_math.
      subroutine vscl ( s, v1, vout )
      use pepper_math, only: pepper_vscl => vscl
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: s  !! Scalar to multiply a vector.
      real(wp) :: v1   ( 3 )  !! Vector to be multiplied.
      real(wp) :: vout ( 3 )  !! Product vector, S * V1.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vscl ( s, (v1), vout )
      end subroutine vscl

      !> Multiply a scalar and a double precision vector of arbitrary
      !> dimension.
      !>
      !> Legacy interface: calls vsclg of module pepper_math.
      subroutine vsclg ( s, v1, ndim, vout )
      use pepper_math, only: pepper_vsclg => vsclg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of V1 (and also VOUT).
      real(wp) :: s  !! Scalar to multiply a vector.
      real(wp) :: v1   ( ndim )  !! Vector to be multiplied.
      real(wp) :: vout ( ndim )  !! Product vector, S * V1.
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vsclg ( s, (v1), ndim, vout )
      end subroutine vsclg

      !> Multiply a scalar and a 3-dimensional double precision vector,
      !> replacing the input vector with the result.
      !>
      !> Legacy interface: calls vsclip of module pepper_math.
      subroutine vsclip ( s, v )
      use pepper_math, only: pepper_vsclip => vsclip
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: s  !! Scalar by which to multiply a vector.
      real(wp) :: v ( 3 )  !! Vector to be multiplied/result of multiplication.
      call pepper_vsclip ( s, v )
      end subroutine vsclip

      !> Find the separation angle in radians between two double
      !> precision, 3-dimensional vectors. This angle is defined as zero
      !> if either vector is zero.
      !>
      !> Legacy interface: calls vsep of module pepper_math.
      real(wp) function vsep ( v1, v2 )
      use pepper_math, only: pepper_vsep => vsep
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1 ( 3 )  !! First vector.
      real(wp) :: v2 ( 3 )  !! Second vector.
      vsep = pepper_vsep ( v1, v2 )
      end function vsep

      !> Find the separation angle in radians between two double precision
      !> vectors of arbitrary dimension. This angle is defined as zero if
      !> either vector is zero.
      !>
      !> Legacy interface: calls vsepg of module pepper_math.
      real(wp) function vsepg ( v1, v2, ndim )
      use pepper_math, only: pepper_vsepg => vsepg
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v1 ( * )  !! First vector.
      real(wp) :: v2 ( * )  !! Second vector.
      integer(ip) :: ndim  !! The number of elements in V1 and V2.
      vsepg = pepper_vsepg ( v1, v2, ndim )
      end function vsepg

      !> Compute the difference between two double precision 3-dimensional
      !> vectors.
      !>
      !> Legacy interface: calls vsub of module pepper_math.
      subroutine vsub ( v1, v2, vout )
      use pepper_math, only: pepper_vsub => vsub
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v1   ( 3 )  !! First vector (minuend).
      real(wp) :: v2   ( 3 )  !! Second vector (subtrahend).
      real(wp) :: vout ( 3 )  !! Difference vector, V1 - V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vsub ( (v1), (v2), vout )
      end subroutine vsub

      !> Compute the difference between two double precision vectors of
      !> arbitrary dimension.
      !>
      !> Legacy interface: calls vsubg of module pepper_math.
      subroutine vsubg ( v1, v2, ndim, vout )
      use pepper_math, only: pepper_vsubg => vsubg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of V1, V2, and VOUT.
      real(wp) :: v1   ( ndim )  !! First vector (minuend).
      real(wp) :: v2   ( ndim )  !! Second vector (subtrahend).
      real(wp) :: vout ( ndim )  !! Difference vector, V1 - V2.
      ! PEPPER: v1, v2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vsubg ( (v1), (v2), ndim, vout )
      end subroutine vsubg

      !> Multiply the transpose of a 3-dimensional column vector,
      !> a 3x3 matrix, and a 3-dimensional column vector.
      !>
      !> Legacy interface: calls vtmv of module pepper_math.
      real(wp) function vtmv ( v1, matrix, v2 )
      use pepper_math, only: pepper_vtmv => vtmv
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v1     (   3 )  !! 3-dimensional double precision column vector.
      real(wp) :: matrix ( 3,3 )  !! 3x3 double precision matrix.
      real(wp) :: v2     (   3 )  !! 3-dimensional double precision column vector.
      vtmv = pepper_vtmv ( v1, matrix, v2 )
      end function vtmv

      !> Multiply the transpose of a n-dimensional column vector,
      !> a nxm matrix, and a m-dimensional column vector.
      !>
      !> Legacy interface: calls vtmvg of module pepper_math.
      real(wp) function vtmvg ( v1, matrix, v2, nrow, ncol )
      use pepper_math, only: pepper_vtmvg => vtmvg
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nrow  !! Number of rows in MATRIX (number of rows in V1.)
      integer(ip) :: ncol  !! Number of columns in MATRIX (number of rows in V2.)
      real(wp) :: v1      ( nrow       )  !! N-dimensional double precision column vector.
      real(wp) :: matrix  ( nrow, ncol )  !! NxM double precision matrix.
      real(wp) :: v2      (       ncol )  !! M-dimensional double precision column vector.
      vtmvg = pepper_vtmvg ( v1, matrix, v2, nrow, ncol )
      end function vtmvg

      !> Unpack three scalar components from a vector.
      !>
      !> Legacy interface: calls vupack of module pepper_math.
      subroutine vupack ( v, x, y, z )
      use pepper_math, only: pepper_vupack => vupack
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v ( 3 )  !! Input 3-dimensional vector.
      real(wp) :: x  !! Scalar components of the vector.
      real(wp) :: y  !! Scalar components of the vector.
      real(wp) :: z  !! Scalar components of the vector.
      ! PEPPER: v is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_vupack ( (v), x, y, z )
      end subroutine vupack

      !> Indicate whether a 3-vector is the zero vector.
      !>
      !> Legacy interface: calls vzero of module pepper_math.
      logical function vzero ( v )
      use pepper_math, only: pepper_vzero => vzero
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: v ( 3 )  !! Vector to be tested.
      vzero = pepper_vzero ( v )
      end function vzero

      !> Indicate whether an n-dimensional vector is the zero vector.
      !>
      !> Legacy interface: calls vzerog of module pepper_math.
      logical function vzerog ( v, ndim )
      use pepper_math, only: pepper_vzerog => vzerog
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v    ( * )  !! Vector to be tested.
      integer(ip) :: ndim  !! Dimension of V.
      vzerog = pepper_vzerog ( v, ndim )
      end function vzerog

      !> Determine the rotation matrix and angular velocity of the
      !> rotation from a state transformation matrix.
      !>
      !> Legacy interface: calls xf2rav of module pepper_math.
      subroutine xf2rav ( xform, rot, av )
      use pepper_math, only: pepper_xf2rav => xf2rav
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: xform  ( 6, 6 )  !! is a state transformation matrix.
      real(wp) :: rot    ( 3, 3 )  !! is the rotation associated with XFORM.
      real(wp) :: av     ( 3    )  !! is the angular velocity associated with XFORM.
      ! PEPPER: xform is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_xf2rav ( (xform), rot, av )
      end subroutine xf2rav

      !> Transpose a 3x3 matrix.
      !>
      !> Legacy interface: calls xpose of module pepper_math.
      subroutine xpose ( m1, mout )
      use pepper_math, only: pepper_xpose => xpose
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: m1   ( 3,3 )  !! 3x3 matrix to be transposed.
      real(wp) :: mout ( 3,3 )  !! Transpose of M1.
      ! PEPPER: m1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_xpose ( (m1), mout )
      end subroutine xpose

      !> Transpose a matrix of arbitrary size (the matrix
      !> need not be square).
      !>
      !> Legacy interface: calls xposeg of module pepper_math.
      subroutine xposeg ( matrix, nrow, ncol, xposem )
      use pepper_math, only: pepper_xposeg => xposeg
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: matrix ( 0:* )  !! Matrix to be transposed.
      integer(ip) :: nrow  !! Number of rows of input matrix MATRIX.
      integer(ip) :: ncol  !! Number of columns of input matrix MATRIX.
      real(wp) :: xposem ( 0:* )  !! Transposed matrix.
      call pepper_xposeg ( matrix, nrow, ncol, xposem )
      end subroutine xposeg

      !> Transpose a matrix of arbitrary size and shape in place.
      !>
      !> Legacy interface: calls xpsgip of module pepper_math.
      subroutine xpsgip ( nrow, ncol, matrix )
      use pepper_math, only: pepper_xpsgip => xpsgip
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nrow  !! Number of rows of input matrix.
      integer(ip) :: ncol  !! Number of columns of input matrix.
      real(wp) :: matrix ( 0 : * )  !! Matrix to be transposed/transposed matrix.
      call pepper_xpsgip ( nrow, ncol, matrix )
      end subroutine xpsgip

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zzmsxf of module pepper_math.
      subroutine zzmsxf ( matrix, n, output )
      use pepper_math, only: pepper_zzmsxf => zzmsxf
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: matrix ( 6, 6, * )  !! A sequence of state transformation matrices
      integer(ip) :: n  !! The number of state transformation matrices
      real(wp) :: output ( 6, 6    )  !! The product of the state transformations.
      call pepper_zzmsxf ( matrix, n, output )
      end subroutine zzmsxf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zzrxr of module pepper_math.
      subroutine zzrxr ( matrix, n, output )
      use pepper_math, only: pepper_zzrxr => zzrxr
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: matrix ( 3, 3, * )  !! A sequence of state transformation matrices
      integer(ip) :: n  !! The number of 3x3  matrices
      real(wp) :: output ( 3, 3    )  !! The product of the 3x3 matrices.
      call pepper_zzrxr ( matrix, n, output )
      end subroutine zzrxr

end module pepper_legacy_math
