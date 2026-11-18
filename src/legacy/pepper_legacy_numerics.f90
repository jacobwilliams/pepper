!> The legacy interface's routines of the subsystem numerics (46 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_numerics

   use pepper_instance_module, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: ubpl, ubel

   implicit none

   private
   public :: dacosh, dacosn, dasine, datanh, dazldr, dcyldr, dlatdr, drdazl, drdgeo, dsphdr, &
      eqncpv, edpnt, ednmpt, elltof, hrmesp, hrmint, lgresp, lgrind, lgrint, chbigr, inelpl, &
      inrypl, invort, invstm, isrot, kepleq, kpsolv, m2q, nplnpt, npsgpt, nvc2pl, nvp2pl, psv2pl, &
      pltar, pltnp, pltvol, qderiv, raxisa, rquad, rmaind, rmaini, vprjp, vprjpi, stelab, stlabx, &
      xposbl

contains

      !> Return the inverse hyperbolic cosine of a double precision
      !> argument.
      !>
      !> Legacy interface: calls ctx%dacosh with the legacy context.
      real(wp) function  dacosh ( x )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Number whose inverse hyperbolic cosine is desired.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      dacosh = ctx%dacosh ( x )
      end function dacosh

      !> Compute arc cosine of a bracketed argument.
      !>
      !> Legacy interface: calls ctx%dacosn with the legacy context.
      real(wp) function dacosn ( arg, tol )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: arg  !! Argument to be evaluated.
      real(wp) :: tol  !! Tolerance.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      dacosn = ctx%dacosn ( arg, tol )
      end function dacosn

      !> Compute arc sine of a bracketed argument.
      !>
      !> Legacy interface: calls ctx%dasine with the legacy context.
      real(wp) function dasine ( arg, tol )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: arg  !! Argument to be evaluated.
      real(wp) :: tol  !! Tolerance.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      dasine = ctx%dasine ( arg, tol )
      end function dasine

      !> Return the inverse hyperbolic tangent of a double precision
      !> argument.
      !>
      !> Legacy interface: calls ctx%datanh with the legacy context.
      real(wp) function  datanh ( x )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Number whose inverse hyperbolic tangent is desired.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      datanh = ctx%datanh ( x )
      end function datanh

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to azimuth/elevation coordinates.
      !>
      !> Legacy interface: calls ctx%dazldr with the legacy context.
      subroutine dazldr ( x, y, z, azccw, elplsz, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! X-coordinate of point.
      real(wp) :: y  !! Y-coordinate of point.
      real(wp) :: z  !! Z-coordinate of point.
      logical :: azccw  !! Flag indicating how azimuth is measured.
      logical :: elplsz  !! Flag indicating how elevation is measured.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dazldr ( x, y, z, azccw, elplsz, jacobi )
      end subroutine dazldr

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to cylindrical coordinates.
      !>
      !> Legacy interface: calls ctx%dcyldr with the legacy context.
      subroutine dcyldr ( x, y, z, jacobi )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! X-coordinate of point.
      real(wp) :: y  !! Y-coordinate of point.
      real(wp) :: z  !! Z-coordinate of point.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dcyldr ( x, y, z, jacobi )
      end subroutine dcyldr

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to latitudinal coordinates.
      !>
      !> Legacy interface: calls ctx%dlatdr with the legacy context.
      subroutine dlatdr ( x, y, z, jacobi )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! X-coordinate of point.
      real(wp) :: y  !! Y-coordinate of point.
      real(wp) :: z  !! Z-coordinate of point.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dlatdr ( x, y, z, jacobi )
      end subroutine dlatdr

      !> Compute the Jacobian matrix of the transformation from
      !> azimuth/elevation to rectangular coordinates.
      !>
      !> Legacy interface: calls ctx%drdazl with the legacy context.
      subroutine drdazl ( range, az, el, azccw, elplsz, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: range  !! Distance of a point from the origin.
      real(wp) :: az  !! Azimuth of input point in radians.
      real(wp) :: el  !! Elevation of input point in radians.
      logical :: azccw  !! Flag indicating how azimuth is measured.
      logical :: elplsz  !! Flag indicating how elevation is measured.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%drdazl ( range, az, el, azccw, elplsz, jacobi )
      end subroutine drdazl

      !> Compute the Jacobian matrix of the transformation from geodetic
      !> to rectangular coordinates.
      !>
      !> Legacy interface: calls ctx%drdgeo with the legacy context.
      subroutine drdgeo ( lon, lat, alt, re, f, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: lon  !! Geodetic longitude of point (radians).
      real(wp) :: lat  !! Geodetic latitude of point (radians).
      real(wp) :: alt  !! Altitude of point above the reference spheroid.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%drdgeo ( lon, lat, alt, re, f, jacobi )
      end subroutine drdgeo

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to spherical coordinates.
      !>
      !> Legacy interface: calls ctx%dsphdr with the legacy context.
      subroutine dsphdr ( x, y, z, jacobi )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! X-coordinate of point.
      real(wp) :: y  !! Y-coordinate of point.
      real(wp) :: z  !! Z-coordinate of point.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dsphdr ( x, y, z, jacobi )
      end subroutine dsphdr

      !> Compute the state (position and velocity) of an object whose
      !> trajectory is described via equinoctial elements relative to some
      !> fixed plane (usually the equatorial plane of some planet).
      !>
      !> Legacy interface: calls ctx%eqncpv with the legacy context.
      subroutine eqncpv ( et, epoch, eqel, rapol, decpol, state )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none
      real(wp) :: et  !! Epoch in seconds past J2000 to find state
      real(wp) :: epoch  !! Epoch of elements in seconds past J2000
      real(wp) :: eqel ( 9 )  !! Array of equinoctial elements
      real(wp) :: rapol  !! Right Ascension of the pole of the reference plane
      real(wp) :: decpol  !! Declination of the pole of the reference plane
      real(wp) :: state ( 6 )  !! State of the object described by EQEL.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: eqel is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%eqncpv ( et, epoch, (eqel), rapol, decpol, state )
      end subroutine eqncpv

      !> Scale a point so that it lies on the surface of a specified
      !> triaxial ellipsoid that is centered at the origin and aligned
      !> with the Cartesian coordinate axes.
      !>
      !> Legacy interface: calls ctx%edpnt with the legacy context.
      subroutine edpnt ( p, a, b, c, ep )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: p  ( 3 )  !! A point in three-dimensional space.
      real(wp) :: a  !! Semi-axis length in the X direction.
      real(wp) :: b  !! Semi-axis length in the Y direction.
      real(wp) :: c  !! Semi-axis length in the Z direction.
      real(wp) :: ep ( 3 )  !! Point on ellipsoid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: p is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%edpnt ( (p), a, b, c, ep )
      end subroutine edpnt

      !> Return the unique point on an ellipsoid's surface where the
      !> outward normal direction is a given vector.
      !>
      !> Legacy interface: calls ctx%ednmpt with the legacy context.
      subroutine ednmpt ( a, b, c, normal, point )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Length of the ellipsoid semi-axis along the X-axis.
      real(wp) :: b  !! Length of the ellipsoid semi-axis along the Y-axis.
      real(wp) :: c  !! Length of the ellipsoid semi-axis along the Z-axis.
      real(wp) :: normal ( 3 )  !! Outward normal direction.
      real(wp) :: point  ( 3 )  !! Point where outward normal is parallel to NORMAL.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: normal is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%ednmpt ( a, b, c, (normal), point )
      end subroutine ednmpt

      !> Solve the time of flight equation MA = E - e sin(E) for the
      !> elliptic eccentric anomaly E, given mean anomaly the MA and
      !> the eccentricity ECC.
      !>
      !> Legacy interface: calls ctx%elltof with the legacy context.
      subroutine elltof ( ma, ecc, e )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: ma  !! Mean anomaly at epoch.
      real(wp) :: ecc  !! Eccentricity.
      real(wp) :: e  !! Elliptic eccentric anomaly.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%elltof ( ma, ecc, e )
      end subroutine elltof

      !> Evaluate, at a specified point, a Hermite interpolating polynomial
      !> for a specified set of equally spaced abscissa values and
      !> corresponding pairs of function and function derivative values.
      !>
      !> Legacy interface: calls ctx%hrmesp with the legacy context.
      subroutine hrmesp ( n,  first,  step,  yvals,  x,  work,  f,  df )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of points defining the polynomial.
      real(wp) :: first  !! First abscissa value.
      real(wp) :: step  !! Step size.
      real(wp) :: yvals ( 2*n )  !! Ordinate and derivative values.
      real(wp) :: x  !! Point at which to interpolate the polynomial.
      real(wp) :: work  ( 2*n, 2 )  !! Work space array.
      real(wp) :: f  !! Interpolated function value at X.
      real(wp) :: df  !! Interpolated function's derivative at X.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%hrmesp ( n, first, step, yvals, x, work, f, df )
      end subroutine hrmesp

      !> Evaluate a Hermite interpolating polynomial at a specified
      !> abscissa value.
      !>
      !> Legacy interface: calls ctx%hrmint with the legacy context.
      subroutine hrmint ( n,  xvals,  yvals,  x,  work,  f,  df )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of points defining the polynomial.
      real(wp) :: xvals (   n )  !! Abscissa values.
      real(wp) :: yvals ( 2*n )  !! Ordinate and derivative values.
      real(wp) :: x  !! Point at which to interpolate the polynomial.
      real(wp) :: work  ( 2*n, 2 )  !! Work space array.
      real(wp) :: f  !! Interpolated function value at X.
      real(wp) :: df  !! Interpolated function's derivative at X.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: xvals is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%hrmint ( n, (xvals), yvals, x, work, f, df )
      end subroutine hrmint

      !> Evaluate a Lagrange interpolating polynomial for a specified
      !> set of coordinate pairs whose first components are equally
      !> spaced, at a specified abscissa value.
      !>
      !> Legacy interface: calls ctx%lgresp with the legacy context.
      real(wp) function lgresp (  n,      first,  step, &
                                          yvals,  work,   x      )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of points defining the polynomial.
      real(wp) :: first  !! First abscissa value.
      real(wp) :: step  !! Step size.
      real(wp) :: yvals ( * )  !! Ordinate values.
      real(wp) :: work  ( * )  !! Work space array.
      real(wp) :: x  !! Point at which to interpolate the polynomial.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lgresp = ctx%lgresp ( n, first, step, yvals, work, x )
      end function lgresp

      !> Evaluate a Lagrange interpolating polynomial, for a specified
      !> set of coordinate pairs, at a specified abscissa value. Return
      !> both the value of the polynomial and its derivative.
      !>
      !> Legacy interface: calls ctx%lgrind with the legacy context.
      subroutine lgrind ( n, xvals, yvals, work, x, p, dp )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of points defining the polynomial.
      real(wp) :: xvals ( n )  !! Abscissa values.
      real(wp) :: yvals ( n )  !! Ordinate values.
      real(wp) :: work  ( n, 2 )  !! Work space array.
      real(wp) :: x  !! Point at which to interpolate the polynomial.
      real(wp) :: p  !! Polynomial value at X.
      real(wp) :: dp  !! Polynomial derivative at X.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: xvals, yvals are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%lgrind ( n, (xvals), (yvals), work, x, p, dp )
      end subroutine lgrind

      !> Evaluate a Lagrange interpolating polynomial for a specified
      !> set of coordinate pairs, at a specified abscissa value.
      !>
      !> Legacy interface: calls ctx%lgrint with the legacy context.
      real(wp) function lgrint ( n, xvals, yvals, work, x )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of points defining the polynomial.
      real(wp) :: xvals ( * )  !! Abscissa values.
      real(wp) :: yvals ( * )  !! Ordinate values.
      real(wp) :: work  ( * )  !! Work space array.
      real(wp) :: x  !! Point at which to interpolate the polynomial.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lgrint = ctx%lgrint ( n, xvals, yvals, work, x )
      end function lgrint

      !> Evaluate an indefinite integral of a Chebyshev expansion at a
      !> specified point X and return the value of the input expansion at
      !> X as well. The constant of integration is selected to make the
      !> integral zero when X equals the abscissa value X2S(1).
      !>
      !> Legacy interface: calls ctx%chbigr with the legacy context.
      subroutine chbigr ( degp, cp, x2s, x, p, itgrlp )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: degp  !! Degree of input Chebyshev expansion.
      real(wp) :: cp     ( * )  !! Chebyshev coefficients of input expansion.
      real(wp) :: x2s    ( 2 )  !! Transformation parameters.
      real(wp) :: x  !! Abscissa value of evaluation.
      real(wp) :: p  !! Input expansion evaluated at X.
      real(wp) :: itgrlp  !! Integral evaluated at X.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: x2s is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%chbigr ( degp, cp, (x2s), x, p, itgrlp )
      end subroutine chbigr

      !> Find the intersection of an ellipse and a plane.
      !>
      !> Legacy interface: calls ctx%inelpl with the legacy context.
      subroutine inelpl ( ellips, plane, nxpts, xpt1, xpt2 )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: ellips ( ubel )  !! A SPICE ellipse.
      real(wp) :: plane  ( ubpl )  !! A SPICE plane.
      integer(ip) :: nxpts  !! Number of intersection points of plane and ellipse.
      real(wp) :: xpt1   ( 3 )  !! Intersection points.
      real(wp) :: xpt2   ( 3 )  !! Intersection points.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: ellips, plane are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%inelpl ( (ellips), (plane), nxpts, xpt1, xpt2 )
      end subroutine inelpl

      !> Find the intersection of a ray and a plane.
      !>
      !> Legacy interface: calls ctx%inrypl with the legacy context.
      subroutine inrypl ( vertex, dir, plane, nxpts, xpt )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vertex (    3 )  !! Vertex and direction vector of ray.
      real(wp) :: dir    (    3 )  !! Vertex and direction vector of ray.
      real(wp) :: plane  ( ubpl )  !! A SPICE plane.
      integer(ip) :: nxpts  !! Number of intersection points of ray and plane.
      real(wp) :: xpt    (    3 )  !! Intersection point, if NXPTS = 1.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, dir, plane are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%inrypl ( (vertex), (dir), (plane), nxpts, xpt )
      end subroutine inrypl

      !> Construct the inverse of a 3x3 matrix with orthogonal columns and
      !> non-zero column norms using a numerically stable algorithm. The
      !> rows of the output matrix are the columns of the input matrix
      !> divided by the length squared of the corresponding columns.
      !>
      !> Legacy interface: calls ctx%invort with the legacy context.
      subroutine invort ( m, mit )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      implicit none
      real(wp) :: m   ( 3, 3 )  !! A 3x3 matrix.
      real(wp) :: mit ( 3, 3 )  !! M after transposition and scaling of rows.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: m is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%invort ( (m), mit )
      end subroutine invort

      !> Return the inverse of a state transformation matrix.
      !>
      !> Legacy interface: calls ctx%invstm with the legacy context.
      subroutine invstm ( mat, invmat )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: mat    ( 6, 6 )  !! A state transformation matrix.
      real(wp) :: invmat ( 6, 6 )  !! The inverse of MAT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: mat is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%invstm ( (mat), invmat )
      end subroutine invstm

      !> Indicate whether a 3x3 matrix is a rotation matrix.
      !>
      !> Legacy interface: calls ctx%isrot with the legacy context.
      logical function isrot ( m, ntol, dtol )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: m    ( 3, 3 )  !! A matrix to be tested.
      real(wp) :: ntol  !! Tolerance for the norms of the columns of M.
      real(wp) :: dtol  !! Tolerance for the determinant of a matrix whose columns are the unitized
                        !! columns of M.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      isrot = ctx%isrot ( m, ntol, dtol )
      end function isrot

      !> Solve the equinoctial version of Kepler's equation.
      !>
      !> Legacy interface: calls ctx%kepleq with the legacy context.
      real(wp) function kepleq (ml,h,k)
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: ml  !! Mean longitude.
      real(wp) :: h  !! h component of equinoctial elements.
      real(wp) :: k  !! k component of equinoctial elements.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      kepleq = ctx%kepleq ( ml, h, k )
      end function kepleq

      !> Solve the equation X = < EVEC, U(X) > where U(X) is the unit
      !> vector [ COS(X), SIN(X) ] and  < , > denotes the two-dimensional
      !> dot product.
      !>
      !> Legacy interface: calls ctx%kpsolv with the legacy context.
      real(wp) function kpsolv ( evec )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: evec ( 2 )  !! A 2-vector whose magnitude is less than 1.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      kpsolv = ctx%kpsolv ( evec )
      end function kpsolv

      !> Find a unit quaternion corresponding to a specified rotation
      !> matrix.
      !>
      !> Legacy interface: calls ctx%m2q with the legacy context.
      subroutine m2q ( r, q )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: r ( 3,  3 )  !! A rotation matrix.
      real(wp) :: q ( 0 : 3 )  !! A unit quaternion representing R.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: r is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%m2q ( (r), q )
      end subroutine m2q

      !> Find the nearest point on a line to a specified point, and find
      !> the distance between the two points.
      !>
      !> Legacy interface: calls ctx%nplnpt with the legacy context.
      subroutine nplnpt ( linpt, lindir, point, pnear, dist )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: linpt  ( 3 )  !! Point on a line and the line's direction vector.
      real(wp) :: lindir ( 3 )  !! Point on a line and the line's direction vector.
      real(wp) :: point  ( 3 )  !! A second point.
      real(wp) :: pnear  ( 3 )  !! Nearest point on the line to POINT.
      real(wp) :: dist  !! Distance between POINT and PNEAR.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: linpt, lindir, point are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%nplnpt ( (linpt), (lindir), (point), pnear, dist )
      end subroutine nplnpt

      !> Find the nearest point on a line segment to a given point.
      !>
      !> Legacy interface: calls ctx%npsgpt with the legacy context.
      subroutine npsgpt ( ep1, ep2, point, pnear, dist )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: ep1   ( 3 )  !! Endpoints of a line segment.
      real(wp) :: ep2   ( 3 )  !! Endpoints of a line segment.
      real(wp) :: point ( 3 )  !! A point in 3-dimensional space.
      real(wp) :: pnear ( 3 )  !! Nearest point on the line segment to POINT.
      real(wp) :: dist  !! Distance between PNEAR and POINT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: ep1, ep2, point are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%npsgpt ( (ep1), (ep2), (point), pnear, dist )
      end subroutine npsgpt

      !> Make a SPICE plane from a normal vector and a constant.
      !>
      !> Legacy interface: calls ctx%nvc2pl with the legacy context.
      subroutine nvc2pl ( normal, konst, plane )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: normal (    3 )  !! A normal vector and constant defining a plane.
      real(wp) :: konst  !! A normal vector and constant defining a plane.
      real(wp) :: plane  ( ubpl )  !! An array representing the plane.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: normal is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%nvc2pl ( (normal), konst, plane )
      end subroutine nvc2pl

      !> Make a SPICE plane from a normal vector and a point.
      !>
      !> Legacy interface: calls ctx%nvp2pl with the legacy context.
      subroutine nvp2pl ( normal, point, plane )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: normal (    3 )  !! A normal vector and a point defining a plane.
      real(wp) :: point  (    3 )  !! A normal vector and a point defining a plane.
      real(wp) :: plane  ( ubpl )  !! An array representing the plane.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: normal, point are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%nvp2pl ( (normal), (point), plane )
      end subroutine nvp2pl

      !> Make a SPICE plane from a point and two spanning vectors.
      !>
      !> Legacy interface: calls ctx%psv2pl with the legacy context.
      subroutine psv2pl ( point, span1, span2, plane )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: point (    3 )  !! A point and two spanning vectors defining a plane.
      real(wp) :: span1 (    3 )  !! A point and two spanning vectors defining a plane.
      real(wp) :: span2 (    3 )  !! A point and two spanning vectors defining a plane.
      real(wp) :: plane ( ubpl )  !! An array representing the plane.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point, span1, span2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%psv2pl ( (point), (span1), (span2), plane )
      end subroutine psv2pl

      !> Compute the total area of a collection of triangular plates.
      !>
      !> Legacy interface: calls ctx%pltar with the legacy context.
      real(wp) function pltar ( nv, vrtces, np, plates )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nv  !! Number of vertices.
      real(wp) :: vrtces ( 3, nv )  !! Array of vertices.
      integer(ip) :: np  !! Number of triangular plates.
      integer(ip) :: plates ( 3, np )  !! Array of plates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      pltar = ctx%pltar ( nv, vrtces, np, plates )
      end function pltar

      !> Find the nearest point on a triangular plate to a given point.
      !>
      !> Legacy interface: calls ctx%pltnp with the legacy context.
      subroutine pltnp ( point, v1, v2, v3, pnear, dist )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: point ( 3 )  !! A point in 3-dimensional space.
      real(wp) :: v1    ( 3 )  !! Vertices of a triangular plate.
      real(wp) :: v2    ( 3 )  !! Vertices of a triangular plate.
      real(wp) :: v3    ( 3 )  !! Vertices of a triangular plate.
      real(wp) :: pnear ( 3 )  !! Nearest point on the plate to POINT.
      real(wp) :: dist  !! Distance between PNEAR and POINT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point, v1, v2, v3 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%pltnp ( (point), (v1), (v2), (v3), pnear, dist )
      end subroutine pltnp

      !> Compute the volume of a three-dimensional region bounded by a
      !> collection of triangular plates.
      !>
      !> Legacy interface: calls ctx%pltvol with the legacy context.
      real(wp) function pltvol ( nv, vrtces, np, plates )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nv  !! Number of vertices.
      real(wp) :: vrtces ( 3, nv )  !! Array of vertices.
      integer(ip) :: np  !! Number of triangular plates.
      integer(ip) :: plates ( 3, np )  !! Array of plates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      pltvol = ctx%pltvol ( nv, vrtces, np, plates )
      end function pltvol

      !> Estimate the derivative of a function by finding the derivative
      !> of a quadratic approximating function. This derivative estimate
      !> is equivalent to that found by computing the average of forward
      !> and backward differences.
      !>
      !> Legacy interface: calls ctx%qderiv with the legacy context.
      subroutine qderiv ( ndim, f0, f2, delta, dfdt )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: ndim  !! Dimension of function to be differentiated.
      real(wp) :: f0    ( ndim )  !! Function values at left endpoint.
      real(wp) :: f2    ( ndim )  !! Function values at right endpoint.
      real(wp) :: delta  !! Separation of abscissa points.
      real(wp) :: dfdt  ( ndim )  !! Derivative vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: f0, f2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%qderiv ( ndim, (f0), (f2), delta, dfdt )
      end subroutine qderiv

      !> Compute the axis of the rotation given by an input matrix
      !> and the angle of the rotation about that axis.
      !>
      !> Legacy interface: calls ctx%raxisa with the legacy context.
      subroutine raxisa ( matrix, axis, angle )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: matrix ( 3, 3 )  !! 3x3 rotation matrix in double precision.
      real(wp) :: axis   (    3 )  !! Axis of the rotation.
      real(wp) :: angle  !! Angle through which the rotation is performed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: matrix is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%raxisa ( (matrix), axis, angle )
      end subroutine raxisa

      !> Find the roots of a quadratic equation.
      !>
      !> Legacy interface: calls ctx%rquad with the legacy context.
      subroutine rquad (  a,  b,  c,  root1,  root2  )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Coefficient of quadratic term.
      real(wp) :: b  !! Coefficient of linear term.
      real(wp) :: c  !! Constant.
      real(wp) :: root1 ( 2 )  !! Root built from positive discriminant term.
      real(wp) :: root2 ( 2 )  !! Root built from negative discriminant term.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rquad ( a, b, c, root1, root2 )
      end subroutine rquad

      !> Compute the integer quotient and non-negative remainder
      !> of NUM and DENOM.
      !>
      !> Legacy interface: calls ctx%rmaind with the legacy context.
      subroutine rmaind ( num, denom, q, rem )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: num  !! Numerator used to compute quotient and remainder.
      real(wp) :: denom  !! Denominator used to compute quotient and remainder.
      real(wp) :: q  !! Integer portion of the quotient NUM/DENOM.
      real(wp) :: rem  !! Remainder of the quotient NUM/DENOM.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rmaind ( num, denom, q, rem )
      end subroutine rmaind

      !> Compute the integer quotient and non-negative remainder
      !> of NUM and DENOM.
      !>
      !> Legacy interface: calls ctx%rmaini with the legacy context.
      subroutine rmaini ( num, denom, q, rem )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: num  !! Numerator used to compute quotient and remainder.
      integer(ip) :: denom  !! Denominator used to compute quotient and remainder.
      integer(ip) :: q  !! Integer portion of the quotient NUM/DENOM.
      integer(ip) :: rem  !! Remainder of the quotient NUM/DENOM.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rmaini ( num, denom, q, rem )
      end subroutine rmaini

      !> Project a vector onto a specified plane, orthogonally.
      !>
      !> Legacy interface: calls ctx%vprjp with the legacy context.
      subroutine vprjp ( vin, plane, vout )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vin   (    3 )  !! Vector to be projected.
      real(wp) :: plane ( ubpl )  !! A SPICE plane onto which VIN is projected.
      real(wp) :: vout  (    3 )  !! Vector resulting from projection.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vin, plane are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%vprjp ( (vin), (plane), vout )
      end subroutine vprjp

      !> Find the vector in a specified plane that maps to a specified
      !> vector in another plane under orthogonal projection.
      !>
      !> Legacy interface: calls ctx%vprjpi with the legacy context.
      subroutine vprjpi ( vin, projpl, invpl, vout, found )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vin    (    3 )  !! The projected vector.
      real(wp) :: projpl ( ubpl )  !! Plane containing VIN.
      real(wp) :: invpl  ( ubpl )  !! Plane containing inverse image of VIN.
      real(wp) :: vout   (    3 )  !! Inverse projection of VIN.
      logical :: found  !! Flag indicating whether VOUT could be calculated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vin, projpl, invpl are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%vprjpi ( (vin), (projpl), (invpl), vout, found )
      end subroutine vprjpi

      !> Correct the apparent position of an object for stellar
      !> aberration.
      !>
      !> Legacy interface: calls ctx%stelab with the legacy context.
      subroutine stelab ( pobj, vobs, appobj )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: pobj   ( 3 )  !! Position of an object with respect to the observer.
      real(wp) :: vobs   ( 3 )  !! Velocity of the observer with respect to the Solar System
                                !! barycenter.
      real(wp) :: appobj ( 3 )  !! Apparent position of the object with respect to the observer,
                                !! corrected for stellar aberration.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: pobj, vobs are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%stelab ( (pobj), (vobs), appobj )
      end subroutine stelab

      !> Correct the position of a target for the stellar aberration
      !> effect on radiation transmitted from a specified observer to
      !> the target.
      !>
      !> Legacy interface: calls ctx%stlabx with the legacy context.
      subroutine stlabx ( pobj, vobs, corpos )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: pobj   ( 3 )  !! Position of an object with respect to the observer.
      real(wp) :: vobs   ( 3 )  !! Velocity of the observer with respect to the Solar System
                                !! barycenter.
      real(wp) :: corpos ( 3 )  !! Corrected position of the object.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: pobj, vobs are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%stlabx ( (pobj), (vobs), corpos )
      end subroutine stlabx

      !> Transpose the square blocks within a matrix.
      !>
      !> Legacy interface: calls ctx%xposbl with the legacy context.
      subroutine xposbl ( bmat, nrow, ncol, bsize, btmat )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nrow  !! the number of rows in the matrix BMAT
      integer(ip) :: ncol  !! the number of columns in the matrix BMAT
      integer(ip) :: bsize  !! the size of the square blocks in BMAT
      real(wp) :: bmat  ( nrow, ncol )  !! a matrix composed of square block submatrices
      real(wp) :: btmat ( nrow, ncol )  !! the result of transposing the blocks of BMAT
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bmat is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%xposbl ( (bmat), nrow, ncol, bsize, btmat )
      end subroutine xposbl

end module pepper_legacy_numerics
