!> Numerical, geometric and coordinate routines that check their inputs (signal errors): inverse
!> functions, Jacobians, planes, ellipses, interpolation, Kepler's equation, aberration, ...
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_numerics

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   !
   ! The contents of SPICE planes are as follows:
   !
   !    Elements NMLPOS through NMLPOS + 2 contain a unit normal
   !    vector for the plane.
   !
   !    Element CONPOS contains a constant for the plane;  every point
   !    X in the plane satisfies
   !
   !       < X, PLANE(NMLPOS) >  =  PLANE(CONPOS).
   !
   !    The plane constant is the distance of the plane from the
   !    origin; the normal vector, scaled by the constant, is the
   !    closest point in the plane to the origin.
   integer(ip), parameter :: conpos = 4  !! index of the constant in a plane array
   real(wp), parameter :: magtol = 1.0e-14_wp  !! tolerance of a vector magnitude
   integer(ip), parameter :: nmlpos = 1  !! index of the normal vector in a plane array

contains

      !> Return the inverse hyperbolic cosine of a double precision
      !> argument.
      !>
      !> The function returns the inverse hyperbolic cosine of a double
      !> precision number.
      !>
      !>## Detailed Input
      !>
      !>     X        is any double precision number greater than or equal to
      !>              1.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the inverse hyperbolic cosine of the double
      !>     precision number X.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If X is less than 1.0D0, the error SPICE(INVALIDARGUMENT) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function simply implements the definition of the inverse
      !>     hyperbolic cosine as follows:
      !>
      !>        DACOSH = DLOG (X + DSQRT (X*X-1.D0))
      !>
      !>     If the input value is not valid, an error is signaled.
      !>
      !>## Examples
      !>
      !>     The following table gives a few values for X and the resulting
      !>     value of DACOSH.
      !>
      !>     X                       DACOSH(X)
      !>     ----------------------------------------------
      !>     1.000000000000000      0.0000000000000000E+00
      !>     10.00000000000000       2.993222846126381
      !>     100.0000000000000       5.298292365610485
      !>     1000.000000000000       7.600902209541989
      !>
      !>## Restrictions
      !>
      !>     1)  The value of the input variable X must be greater than or
      !>         equal to 1.0d0.
      !>
      !>## Literature References
      !>
      !>     [1]  W.H. Beyer, "CRC Standard Mathematical Tables," CRC Press,
      !>          1987.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        Set the default function value to either 0, 0.0D0, .FALSE.,
      !>        or blank depending on the type of the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      module real(wp) function dacosh ( ctx, x )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! Number whose inverse hyperbolic cosine is desired.

!
!   Set up the error processing.
!
      if ( ctx%failed() ) then
         dacosh = 0.0_wp
         return
      else
         dacosh = 0.0_wp
      end if

!
!  Check that X >= 1.
!

      if ( x < 1.0_wp ) then

         call ctx%setmsg ( 'DACOSH: Invalid argument, X is less than one.' )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )

         return
      end if
!
!  Abiding by the order implied by the parentheses in the expression
!  (1.0D0/X)/X prevents floating point overflow that might occur for
!  large values of X if the equivalent expression, 1.0D0/(X*X), were
!  used.
!
      dacosh = dlog (x + x * dsqrt (1.0_wp - (1.0_wp/x)/x) )

      return
      end function dacosh

      !> Compute arc cosine of a bracketed argument.
      !>
      !> The function returns the arc cosine of ARG.
      !>
      !> This routine produces a SPICE error if the |argument| exceeds
      !> 1.D0 by more than TOL. If ARG exceeds 1.D0, the argument is
      !> evaluated as if it equaled 1.D0, if ARG is less than -1.,
      !> the argument is evaluated as if it equaled -1.D0.
      !>
      !>## Detailed Input
      !>
      !>     ARG      is the arc cosine argument that is to be evaluated such
      !>              that if it is less than -1.D0 by more than TOL or greater
      !>              than 1.D0 by more than TOL, an error results.
      !>
      !>     TOL      is a tolerance such that |ARG| is considered to be equal
      !>              to 1.D0 if |ARG| <= 1.D0 + TOL. TOL must be non-negative.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the arc cosine of ARG. If
      !>
      !>        |ARG| >= 1.D0,
      !>
      !>     it returns DACOS (1.D0) or DACOS (-1.D0) as appropriate. Values
      !>     range from 0 to PI.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If |ARG| > 1.D0 + TOL, the error SPICE(INPUTOUTOFBOUNDS) is
      !>         signaled.
      !>
      !>     2)  If TOL is less than zero, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine determines whether |ARG| > 1.D0 + TOL. If it is, an
      !>     error will be flagged. In addition, the values of ARG are
      !>     constrained to [-1.D0, 1.D0].
      !>
      !>## Examples
      !>
      !>     The following illustrate the operation of DACOSN.
      !>
      !>           DACOSN (  -1.D0,        1.D-7 )  =   PI
      !>           DACOSN (  -1.00001D0,   1.D-3 )  =   PI
      !>           DACOSN (  -1.00001D0,   1.D-7 )  =   PI   (error flagged)
      !>           DACOSN (   0.D0,        1.D-7 )  =   PI/2
      !>           DACOSN (   1.00001D0,   1.D-3 )  =   0.
      !>           DACOSN (   1.00001D0,   1.D-7 )  =   0. (error flagged)
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     L.S. Elson         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 28-FEB-2006 (LSE)
      module real(wp) function dacosn ( ctx, arg, tol )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: arg  !! Argument to be evaluated.
      real(wp), intent(in) :: tol  !! Tolerance.

!
!     Bracket ARG.
!

      dacosn      =  acos ( max ( - 1.0_wp, min ( 1.0_wp, arg ) ) )

!
!     Check that tolerance is non negative.
!

      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'TOL was #; must be non-negative.' )
         call ctx%errdp  ( '#',  tol                          )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'           )
         return

      end if

!
!     Check to see if |ARG| is within TOL of 1.D0. Signal error if
!     appropriate.
!

      if  ( ( abs ( arg ) - tol ) > 1.0_wp) then

         call ctx%setmsg ( 'The |argument| specified was greater '// &
                       'than 1.D0 by more than #. The value '// &
                       'of the argument is #. '           )
         call ctx%errdp  ( '#',  tol                          )
         call ctx%errdp  ( '#',  arg                          )
         call ctx%sigerr ( 'SPICE(INPUTOUTOFBOUNDS)'          )
         return

      end if

      return
      end function dacosn

      !> Compute arc sine of a bracketed argument.
      !>
      !> The function returns the arc sine of ARG.
      !>
      !> This routine produces a SPICE error if the |argument| exceeds
      !> 1.D0 by more than TOL. If ARG exceeds 1.D0, the argument is
      !> evaluated as if it equaled 1.D0, if ARG is less than -1.,
      !> the argument is evaluated as if it equaled -1.D0.
      !>
      !>## Detailed Input
      !>
      !>     ARG      is the arc sine argument that is to be evaluated such
      !>              that if it is less than -1.D0 by more than TOL or greater
      !>              than 1.D0 by more than TOL, an error results.
      !>
      !>     TOL      is a tolerance such that |ARG| is considered to be equal
      !>              to 1.D0 if |ARG| <= 1.D0 + TOL. TOL must be non-negative.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the arc sine of ARG. If
      !>
      !>        |ARG| >= 1.D0,
      !>
      !>     it returns DASIN (1.D0) or DASIN (-1.D0) as appropriate. Values
      !>     range from -PI/2 to PI/2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If TOL is less than zero, the error SPICE(VALUEOUTOFRANGE) is
      !>         signaled.
      !>
      !>     2)  If |ARG| > 1.D0 + TOL, the error SPICE(INPUTOUTOFBOUNDS) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine determines whether |ARG| > 1.D0 + TOL. If it is, an
      !>     error will be flagged. In addition, the values of ARG are
      !>     constrained to [-1.D0, 1.D0].
      !>
      !>## Examples
      !>
      !>     The following illustrate the operation of DASINE.
      !>
      !>           DASINE (  -1.D0,        1.D-7 )  =  -PI/2
      !>           DASINE (  -1.00001D0,   1.D-3 )  =  -PI/2
      !>           DASINE (  -1.00001D0,   1.D-7 )  =  -PI/2 (error flagged)
      !>           DASINE (   0.D0,        1.D-7 )  =   0.D0
      !>           DASINE (   1.00001D0,   1.D-3 )  =   PI/2
      !>           DASINE (   1.00001D0,   1.D-7 )  =   PI/2 (error flagged)
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     L.S. Elson         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 28-FEB-2006 (LSE)
      module real(wp) function dasine ( ctx, arg, tol )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: arg  !! Argument to be evaluated.
      real(wp), intent(in) :: tol  !! Tolerance.

!
!     Bracket ARG.
!

      dasine      =  asin ( max ( - 1.0_wp, min ( 1.0_wp, arg ) ) )

!
!     Check that tolerance is non negative.
!

      if ( tol < 0.0_wp ) then

         call ctx%setmsg ( 'TOL was #; must be non-negative.' )
         call ctx%errdp  ( '#',  tol                          )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'           )
         return

      end if

!
!     Check to see if |ARG| is within TOL of 1.D0. Signal error if
!     appropriate.
!

      if  ( ( abs ( arg ) - tol ) > 1.0_wp) then

         call ctx%setmsg ( 'The |argument| specified was greater '// &
                       'than 1.D0 by more than #. The value '// &
                       'of the argument is #. '           )
         call ctx%errdp  ( '#',  tol                          )
         call ctx%errdp  ( '#',  arg                          )
         call ctx%sigerr ( 'SPICE(INPUTOUTOFBOUNDS)'          )
         return

      end if

      return
      end function dasine

      !> Return the inverse hyperbolic tangent of a double precision
      !> argument.
      !>
      !> The function returns the inverse hyperbolic tangent of a double
      !> precision number.
      !>
      !>## Detailed Input
      !>
      !>     X        is any double precision.
      !>
      !>              X must be within the range -1 < X < +1.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the inverse hyperbolic tangent of the double
      !>     precision number X.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If X is not between -1.0 and 1.0, the error
      !>         SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function simply implements the definition of the inverse
      !>     hyperbolic tangent as follows:
      !>
      !>        DATANH = 0.5D0 * DLOG ( (1+X) / (1-X) )
      !>
      !>     If the input value is not valid, an error is signaled.
      !>
      !>## Examples
      !>
      !>     The following table gives a few values for X and the resulting
      !>     value of DATANH.
      !>
      !>         X                       DATANH(X)
      !>        ----------------------------------------------
      !>        -0.2000000000000000     -0.2027325540540822
      !>        -0.1000000000000000     -0.1003353477310756
      !>         0.0000000000000000E+00  0.0000000000000000E+00
      !>         0.1000000000000000      0.1003353477310756
      !>         0.2000000000000000      0.2027325540540822
      !>         0.4000000000000000      0.4236489301936018
      !>        0.8000000000000000       1.098612288668110
      !>
      !>## Restrictions
      !>
      !>     1)  The value of the input variable X must be between -1.0 and
      !>         1.0, otherwise an error is signaled.
      !>
      !>## Literature References
      !>
      !>     [1]  W.H. Beyer, "CRC Standard Mathematical Tables," CRC Press,
      !>          1987.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        Set the default function value to either 0, 0.0D0, .FALSE.,
      !>        or blank depending on the type of the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      module real(wp) function datanh ( ctx, x )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! Number whose inverse hyperbolic tangent is desired.

!
!     Set up the error processing.
!
      if ( ctx%failed() ) then
         datanh = 0.0_wp
         return
      else
         datanh = 0.0_wp
      end if

!
!     Check that -1 < X < +1.
!
      if ( dabs(x) >= 1.0_wp ) then

         call ctx%setmsg ( 'DATANH: Argument out of range.' )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )

         return
      end if

      datanh = 0.5_wp * dlog ( (1.0_wp+x) / (1.0_wp-x) )

      return
      end function datanh

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to azimuth/elevation coordinates.
      !>
      !>## Detailed Input
      !>
      !>     X,
      !>     Y,
      !>     Z        are the rectangular coordinates of the point at
      !>              which the Jacobian matrix of the map from rectangular
      !>              to azimuth/elevation coordinates is desired.
      !>
      !>     AZCCW    is a flag indicating how the azimuth is measured.
      !>
      !>              If AZCCW is .TRUE., the azimuth increases in the
      !>              counterclockwise direction; otherwise it increases
      !>              in the clockwise direction.
      !>
      !>     ELPLSZ   is a flag indicating how the elevation is measured.
      !>
      !>              If ELPLSZ is .TRUE., the elevation increases from the
      !>              XY plane toward +Z; otherwise toward -Z.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the
      !>              transformation from rectangular to azimuth/elevation
      !>              coordinates. It has the form
      !>
      !>                 .-                            -.
      !>                 |  dr/dx     dr/dy     dr/dz   |
      !>                 |  daz/dx    daz/dy    daz/dz  |
      !>                 |  del/dx    del/dy    del/dz  |
      !>                 `-                            -'
      !>
      !>               evaluated at the input values of X, Y, and Z.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input point is on the Z-axis ( X = 0 and Y = 0 ), the
      !>         Jacobian matrix is undefined and therefore, the error
      !>         SPICE(POINTONZAXIS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     When performing vector calculations with velocities it is
      !>     usually most convenient to work in rectangular coordinates.
      !>     However, once the vector manipulations have been performed
      !>     it is often desirable to convert the rectangular representations
      !>     into azimuth/elevation coordinates to gain insights about
      !>     phenomena in this coordinate system.
      !>
      !>     To transform rectangular velocities to derivatives of coordinates
      !>     in a azimuth/elevation coordinate system, one uses the Jacobian
      !>     matrix of the transformation between the two systems.
      !>
      !>     Given a state in rectangular coordinates
      !>
      !>        ( x, y, z, dx, dy, dz )
      !>
      !>     the corresponding azimuth/elevation coordinate derivatives are
      !>     given by the matrix equation:
      !>
      !>                      t          |                      t
      !>        (dr, daz, del)   = JACOBI|        * (dx, dy, dz)
      !>                                 |(x,y,z)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(x, y, z)
      !>
      !>     In the azimuth/elevation coordinate system, several conventions
      !>     exist on how azimuth and elevation are measured. Using the AZCCW
      !>     and ELPLSZ flags, users indicate which conventions shall be used.
      !>     See the descriptions of these input arguments for details.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the azimuth/elevation state of Venus as seen from the
      !>        DSS-14 station at a given epoch. Map this state back to
      !>        rectangular coordinates as a check.
      !>
      !>        Task description
      !>        ================
      !>
      !>        In this example, we will obtain the apparent state of Venus as
      !>        seen from the DSS-14 station in the DSS-14 topocentric
      !>        reference frame. We will use a station frames kernel and
      !>        transform the resulting rectangular coordinates to azimuth,
      !>        elevation and range and its derivatives using RECAZL and
      !>        DAZLDR.
      !>
      !>        We will map this state back to rectangular coordinates using
      !>        AZLREC and DRDAZL.
      !>
      !>        In order to introduce the usage of the logical flags AZCCW
      !>        and ELPLSZ, we will request the azimuth to be measured
      !>        clockwise and the elevation positive towards +Z
      !>        axis of the DSS-14_TOPO reference frame.
      !>
      !>        Kernels
      !>        =======
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: dazldr_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                        Contents
      !>              ---------                        --------
      !>              de430.bsp                        Planetary ephemeris
      !>              naif0011.tls                     Leapseconds
      !>              earth_720101_070426.bpc          Earth historical
      !>                                                  binary PCK
      !>              earthstns_itrf93_050714.bsp      DSN station SPK
      !>              earth_topo_050714.tf             DSN station FK
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = ( 'de430.bsp',
      !>                               'naif0011.tls',
      !>                               'earth_720101_070426.bpc',
      !>                               'earthstns_itrf93_050714.bsp',
      !>                               'earth_topo_050714.tf'         )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAZLDR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1   = '(A,F20.8)' )
      !>
      !>      CHARACTER*(*)         META
      !>      PARAMETER           ( META   = 'dazldr_ex1.tm' )
      !>
      !>      INTEGER               BDNMLN
      !>      PARAMETER           ( BDNMLN = 36 )
      !>
      !>      INTEGER               CORLEN
      !>      PARAMETER           ( CORLEN = 10 )
      !>
      !>      INTEGER               FRNMLN
      !>      PARAMETER           ( FRNMLN = 32 )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 40 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(CORLEN)    ABCORR
      !>      CHARACTER*(BDNMLN)    OBS
      !>      CHARACTER*(TIMLEN)    OBSTIM
      !>      CHARACTER*(FRNMLN)    REF
      !>      CHARACTER*(BDNMLN)    TARGET
      !>
      !>      DOUBLE PRECISION      AZ
      !>      DOUBLE PRECISION      AZLVEL ( 3    )
      !>      DOUBLE PRECISION      DRECTN ( 3    )
      !>      DOUBLE PRECISION      EL
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      JACOBI ( 3, 3 )
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      STATE  ( 6    )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RECTAN ( 3    )
      !>
      !>      LOGICAL               AZCCW
      !>      LOGICAL               ELPLSZ
      !>
      !>C
      !>C     Load SPICE kernels.
      !>C
      !>      CALL FURNSH ( META )
      !>
      !>C
      !>C     Convert the observation time to seconds past J2000 TDB.
      !>C
      !>      OBSTIM = '2003 OCT 13 06:00:00.000000 UTC'
      !>
      !>      CALL STR2ET ( OBSTIM, ET )
      !>
      !>C
      !>C     Set the target, observer, observer frame, and
      !>C     aberration corrections.
      !>C
      !>      TARGET = 'VENUS'
      !>      OBS    = 'DSS-14'
      !>      REF    = 'DSS-14_TOPO'
      !>      ABCORR = 'CN+S'
      !>
      !>C
      !>C     Compute the observer-target state.
      !>C
      !>      CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBS,
      !>     .              STATE,  LT                   )
      !>
      !>C
      !>C     Convert position to azimuth/elevation coordinates,
      !>C     with azimuth increasing clockwise and elevation
      !>C     positive towards +Z axis of the DSS-14_TOPO
      !>C     reference frame
      !>C
      !>      AZCCW  = .FALSE.
      !>      ELPLSZ = .TRUE.
      !>
      !>      CALL RECAZL ( STATE, AZCCW, ELPLSZ, R, AZ, EL )
      !>
      !>C
      !>C     Convert velocity to azimuth/elevation coordinates.
      !>C
      !>      CALL DAZLDR ( STATE(1), STATE(2), STATE(3),
      !>     .              AZCCW,    ELPLSZ,   JACOBI   )
      !>
      !>      CALL MXV ( JACOBI, STATE(4), AZLVEL )
      !>
      !>C
      !>C     As a check, convert the azimuth/elevation state back to
      !>C     rectangular coordinates.
      !>C
      !>      CALL AZLREC ( R, AZ, EL, AZCCW, ELPLSZ, RECTAN )
      !>
      !>      CALL DRDAZL ( R, AZ, EL, AZCCW, ELPLSZ, JACOBI )
      !>
      !>      CALL MXV ( JACOBI, AZLVEL, DRECTN )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'AZ/EL coordinates:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   Range      (km)        = ', R
      !>      WRITE(*,FMT1) '   Azimuth    (deg)       = ', AZ * DPR()
      !>      WRITE(*,FMT1) '   Elevation  (deg)       = ', EL * DPR()
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)')    'AZ/EL velocity:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   d Range/dt     (km/s)  = ', AZLVEL(1)
      !>      WRITE(*,FMT1) '   d Azimuth/dt   (deg/s) = ', AZLVEL(2)
      !>     .                                             * DPR()
      !>      WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLVEL(3)
      !>     .                                             * DPR()
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular coordinates:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   X (km)                 = ', STATE(1)
      !>      WRITE(*,FMT1) '   Y (km)                 = ', STATE(2)
      !>      WRITE(*,FMT1) '   Z (km)                 = ', STATE(3)
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular velocity:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   dX/dt (km/s)           = ', STATE(4)
      !>      WRITE(*,FMT1) '   dY/dt (km/s)           = ', STATE(5)
      !>      WRITE(*,FMT1) '   dZ/dt (km/s)           = ', STATE(6)
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular coordinates from inverse '
      !>     .    //         'mapping:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   X (km)                 = ', RECTAN(1)
      !>      WRITE(*,FMT1) '   Y (km)                 = ', RECTAN(2)
      !>      WRITE(*,FMT1) '   Z (km)                 = ', RECTAN(3)
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular velocity from inverse '
      !>     .    //         'mapping:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   dX/dt (km/s)           = ', DRECTN(1)
      !>      WRITE(*,FMT1) '   dY/dt (km/s)           = ', DRECTN(2)
      !>      WRITE(*,FMT1) '   dZ/dt (km/s)           = ', DRECTN(3)
      !>      WRITE(*,*)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        AZ/EL coordinates:
      !>
      !>           Range      (km)        =   245721478.99272084
      !>           Azimuth    (deg)       =         294.48543372
      !>           Elevation  (deg)       =         -48.94609726
      !>
      !>        AZ/EL velocity:
      !>
      !>           d Range/dt     (km/s)  =          -4.68189834
      !>           d Azimuth/dt   (deg/s) =           0.00402256
      !>           d Elevation/dt (deg/s) =          -0.00309156
      !>
      !>        Rectangular coordinates:
      !>
      !>           X (km)                 =    66886767.37916667
      !>           Y (km)                 =   146868551.77222887
      !>           Z (km)                 =  -185296611.10841590
      !>
      !>        Rectangular velocity:
      !>
      !>           dX/dt (km/s)           =        6166.04150307
      !>           dY/dt (km/s)           =      -13797.77164550
      !>           dZ/dt (km/s)           =       -8704.32385654
      !>
      !>        Rectangular coordinates from inverse mapping:
      !>
      !>           X (km)                 =    66886767.37916658
      !>           Y (km)                 =   146868551.77222890
      !>           Z (km)                 =  -185296611.10841590
      !>
      !>        Rectangular velocity from inverse mapping:
      !>
      !>           dX/dt (km/s)           =        6166.04150307
      !>           dY/dt (km/s)           =      -13797.77164550
      !>           dZ/dt (km/s)           =       -8704.32385654
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-2021 (JDR) (NJB)
      module subroutine dazldr ( ctx, x, y, z, azccw, elplsz, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! X-coordinate of point.
      real(wp), intent(in) :: y  !! Y-coordinate of point.
      real(wp), intent(in) :: z  !! Z-coordinate of point.
      logical, intent(in) :: azccw  !! Flag indicating how azimuth is measured.
      logical, intent(in) :: elplsz  !! Flag indicating how elevation is measured.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     There is a singularity of the Jacobian matrix for points on
!     the z-axis.
!
      if ( ( x == 0 ) .and. ( y == 0 ) ) then

         call ctx%setmsg ( 'The Jacobian matrix of the transformation ' &
         //            'from rectangular to azimuth/elevation ' &
         //            'coordinates is not defined for points on ' &
         //            'the z-axis.'                               )
         call ctx%sigerr ( 'SPICE(POINTONZAXIS)'                       )
         return

      end if

!
!     Get the Jacobian matrix of the transformation from latitudinal to
!     rectangular coordinates at the input point
!
      call dlatdr ( ctx, x, y, z, jacobi )

!
!     The matrix JACOBI is
!
!        .-                                 -.
!        |  DRANGE/DX  DRANGE/DY  DRANGE/DZ  |
!        |    DLON/DX    DLON/DY    LDON/DZ  |
!        |    DLAT/DX    DLAT/DY    DLAT/DZ  |
!        `-                                 -'
!
!     The relationship between lon/lat and az/el depends on
!     the conventions used to measure azimuth and elevation
!     and follows these formulae:
!
!        lon = azsnse * az
!        lat = direl  * el
!
!     where azsnse (direl) are +1 when AZCCW (ELPLSZ) are
!     .TRUE.
!
      if ( .not. azccw ) then

         do i = 1, 3
            jacobi(2,i) = -jacobi(2,i)
         end do

      end if

      if ( .not. elplsz ) then

         do i = 1, 3
            jacobi(3,i) = -jacobi(3,i)
         end do

      end if

      return
      end subroutine dazldr

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to cylindrical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     X,
      !>     Y,
      !>     Z        are the rectangular coordinates of the point at
      !>              which the Jacobian of the map from rectangular
      !>              to cylindrical coordinates is desired.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between rectangular and cylindrical coordinates. It
      !>              has the form
      !>
      !>                 .-                               -.
      !>                 |  dr   /dx   dr   /dy  dr   /dz  |
      !>                 |  dlong/dx   dlong/dy  dlong/dz  |
      !>                 |  dz   /dx   dz   /dy  dz   /dz  |
      !>                 `-                               -'
      !>
      !>              evaluated at the input values of X, Y, and Z.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input point is on the Z-axis (X = 0 and Y = 0), the
      !>         Jacobian is undefined, the error SPICE(POINTONZAXIS) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     When performing vector calculations with velocities it is
      !>     usually most convenient to work in rectangular coordinates.
      !>     However, once the vector manipulations have been performed,
      !>     it is often desirable to convert the rectangular representations
      !>     into cylindrical coordinates to gain insights about phenomena
      !>     in this coordinate frame.
      !>
      !>     To transform rectangular velocities to derivatives of coordinates
      !>     in a cylindrical system, one uses the Jacobian of the
      !>     transformation between the two systems.
      !>
      !>     Given a state in rectangular coordinates
      !>
      !>        ( x, y, z, dx, dy, dz )
      !>
      !>     the velocity in cylindrical coordinates is given by the matrix
      !>     equation:
      !>
      !>                       t          |                     t
      !>        (dr, dlong, dz)   = JACOBI|       * (dx, dy, dz)
      !>                                  |(x,y,z)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(x,y,z)
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the cylindrical state of the Earth as seen from
      !>        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
      !>        Map this state back to rectangular coordinates as a check.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: dcyldr_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DCYLDR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,E18.8)' )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      DRECTN ( 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      JACOBI ( 3, 3 )
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      CYLVEL ( 3 )
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      STATE  ( 6 )
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK, PCK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'dcyldr_ex1.tm' )
      !>
      !>C
      !>C     Look up the apparent state of earth as seen from Mars
      !>C     at January 1, 2005 TDB, relative to the IAU_MARS
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( 'January 1, 2005 TDB', ET )
      !>
      !>      CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S',
      !>     .              'Mars',  STATE, LT                )
      !>
      !>C
      !>C     Convert position to cylindrical coordinates.
      !>C
      !>      CALL RECCYL ( STATE, R, CLON, Z )
      !>
      !>C
      !>C     Convert velocity to cylindrical coordinates.
      !>C
      !>
      !>      CALL DCYLDR ( STATE(1), STATE(2), STATE(3), JACOBI )
      !>
      !>      CALL MXV ( JACOBI, STATE(4), CYLVEL )
      !>
      !>C
      !>C     As a check, convert the cylindrical state back to
      !>C     rectangular coordinates.
      !>C
      !>      CALL CYLREC ( R, CLON, Z, RECTAN )
      !>
      !>      CALL DRDCYL ( R, CLON, Z, JACOBI )
      !>
      !>      CALL MXV ( JACOBI, CYLVEL, DRECTN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                 = ', STATE(1)
      !>      WRITE(*,FMT1) '  Y (km)                 = ', STATE(2)
      !>      WRITE(*,FMT1) '  Z (km)                 = ', STATE(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)           = ', STATE(4)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)           = ', STATE(5)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)           = ', STATE(6)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius    (km)         = ', R
      !>      WRITE(*,FMT1) '  Longitude (deg)        = ', CLON/RPD()
      !>      WRITE(*,FMT1) '  Z         (km)         = ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  d Radius/dt    (km/s)  = ', CYLVEL(1)
      !>      WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ',
      !>     .                                         CYLVEL(2)/RPD()
      !>      WRITE(*,FMT1) '  d Z/dt         (km/s)  = ', CYLVEL(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from inverse ' //
      !>     .           'mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                 = ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y (km)                 = ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z (km)                 = ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity from inverse mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)           = ', DRECTN(1)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)           = ', DRECTN(2)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)           = ', DRECTN(3)
      !>      WRITE(*,*) ' '
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Rectangular coordinates:
      !>
      !>          X (km)                 =    -0.76096183E+08
      !>          Y (km)                 =     0.32436380E+09
      !>          Z (km)                 =     0.47470484E+08
      !>
      !>         Rectangular velocity:
      !>
      !>          dX/dt (km/s)           =     0.22952075E+05
      !>          dY/dt (km/s)           =     0.53760111E+04
      !>          dZ/dt (km/s)           =    -0.20881149E+02
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius    (km)         =     0.33317039E+09
      !>          Longitude (deg)        =     0.10320290E+03
      !>          Z         (km)         =     0.47470484E+08
      !>
      !>         Cylindrical velocity:
      !>
      !>          d Radius/dt    (km/s)  =    -0.83496628E+01
      !>          d Longitude/dt (deg/s) =    -0.40539288E-02
      !>          d Z/dt         (km/s)  =    -0.20881149E+02
      !>
      !>         Rectangular coordinates from inverse mapping:
      !>
      !>          X (km)                 =    -0.76096183E+08
      !>          Y (km)                 =     0.32436380E+09
      !>          Z (km)                 =     0.47470484E+08
      !>
      !>         Rectangular velocity from inverse mapping:
      !>
      !>          dX/dt (km/s)           =     0.22952075E+05
      !>          dY/dt (km/s)           =     0.53760111E+04
      !>          dZ/dt (km/s)           =    -0.20881149E+02
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT)
      module subroutine dcyldr ( ctx, x, y, z, jacobi )
      use pepper_kinds, only: wp
      use pepper_math, only: drdcyl, reccyl
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! X-coordinate of point.
      real(wp), intent(in) :: y  !! Y-coordinate of point.
      real(wp), intent(in) :: z  !! Z-coordinate of point.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.

      real(wp) :: rectan  ( 3 )
      real(wp) :: r
      real(wp) :: long
      real(wp) :: zz
      real(wp) :: injacb  ( 3, 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     There is a singularity of the Jacobian for points on the z-axis.
!
      if ( ( x == 0.0_wp ) .and. ( y == 0.0_wp ) ) then

         call ctx%setmsg ( 'The Jacobian of the transformation from '     // &
                       'rectangular to cylindrical coordinates '      // &
                       'is not defined for points on the z-axis.'      )
         call ctx%sigerr ( 'SPICE(POINTONZAXIS)'                           )
         return

      end if

!
!     We will get the Jacobian of rectangular to cylindrical by
!     implicit differentiation.
!
!     First move the X,Y and Z coordinates into a vector.
!
      rectan = [x, y, z]

!
!     Convert from rectangular to cylindrical coordinates.
!
      call reccyl ( rectan, r, long, zz )

!
!     Get the Jacobian from cylindrical to rectangular coordinates at
!     R, LONG, Z.
!
      call drdcyl ( r,  long, zz, injacb )

!
!     Now invert INJACB to get the Jacobian from rectangular to
!     cylindrical coordinates.
!
      call invort ( ctx, injacb, jacobi )

      return
      end subroutine dcyldr

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to latitudinal coordinates.
      !>
      !>## Detailed Input
      !>
      !>     X,
      !>     Y,
      !>     Z        are the rectangular coordinates of the point at
      !>              which the Jacobian of the map from rectangular
      !>              to latitudinal coordinates is desired.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between rectangular and latitudinal coordinates. It
      !>              has the form
      !>
      !>                  .-                              -.
      !>                  |  dr/dx     dr/dy     dr/dz     |
      !>                  |  dlong/dx  dlong/dy  dlong/dz  |
      !>                  |  dlat/dx   dlat/dy   dlat/dz   |
      !>                  `-                              -'
      !>
      !>               evaluated at the input values of X, Y, and Z.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input point is on the z-axis (X = 0 and Y = 0), the
      !>         Jacobian is undefined, the error SPICE(POINTONZAXIS) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     When performing vector calculations with velocities it is
      !>     usually most convenient to work in rectangular coordinates.
      !>     However, once the vector manipulations have been performed
      !>     it is often desirable to convert the rectangular representations
      !>     into latitudinal coordinates to gain insights about phenomena
      !>     in this coordinate frame.
      !>
      !>     To transform rectangular velocities to derivatives of coordinates
      !>     in a latitudinal system, one uses the Jacobian of the
      !>     transformation between the two systems.
      !>
      !>     Given a state in rectangular coordinates
      !>
      !>          ( x, y, z, dx, dy, dz )
      !>
      !>     the corresponding latitudinal coordinate derivatives are given by
      !>     the matrix equation:
      !>
      !>                         t          |                     t
      !>        (dr, dlong, dlat)   = JACOBI|        * (dx, dy, dz)
      !>                                    |(x,y,z)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(x, y, z)
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the latitudinal state of the Earth as seen from
      !>        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
      !>        Map this state back to rectangular coordinates as a check.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: dlatdr_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DLATDR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,E18.8)' )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DRECTN ( 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      JACOBI ( 3, 3 )
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      LATVEL ( 3 )
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      STATE  ( 6 )
      !>
      !>C
      !>C     Load SPK, PCK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'dlatdr_ex1.tm' )
      !>
      !>C
      !>C     Look up the apparent state of earth as seen from Mars
      !>C     at January 1, 2005 TDB, relative to the IAU_MARS
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( 'January 1, 2005 TDB', ET )
      !>
      !>      CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S',
      !>     .              'Mars',  STATE, LT                )
      !>
      !>C
      !>C     Convert position to latitudinal coordinates.
      !>C
      !>      CALL RECLAT ( STATE, R, LON, LAT )
      !>
      !>C
      !>C     Convert velocity to latitudinal coordinates.
      !>C
      !>
      !>      CALL DLATDR ( STATE(1), STATE(2), STATE(3), JACOBI )
      !>
      !>      CALL MXV ( JACOBI, STATE(4), LATVEL )
      !>
      !>C
      !>C     As a check, convert the latitudinal state back to
      !>C     rectangular coordinates.
      !>C
      !>      CALL LATREC ( R, LON, LAT, RECTAN )
      !>
      !>      CALL DRDLAT ( R, LON, LAT, JACOBI )
      !>
      !>      CALL MXV ( JACOBI, LATVEL, DRECTN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                 = ', STATE(1)
      !>      WRITE(*,FMT1) '  Y (km)                 = ', STATE(2)
      !>      WRITE(*,FMT1) '  Z (km)                 = ', STATE(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)           = ', STATE(4)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)           = ', STATE(5)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)           = ', STATE(6)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius    (km)         = ', R
      !>      WRITE(*,FMT1) '  Longitude (deg)        = ', LON/RPD()
      !>      WRITE(*,FMT1) '  Latitude  (deg)        = ', LAT/RPD()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  d Radius/dt    (km/s)  = ', LATVEL(1)
      !>      WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ',
      !>     .                                         LATVEL(2)/RPD()
      !>      WRITE(*,FMT1) '  d Latitude/dt  (deg/s) = ',
      !>     .                                         LATVEL(3)/RPD()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from inverse ' //
      !>     .           'mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                 = ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y (km)                 = ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z (km)                 = ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity from inverse mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)           = ', DRECTN(1)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)           = ', DRECTN(2)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)           = ', DRECTN(3)
      !>      WRITE(*,*) ' '
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Rectangular coordinates:
      !>
      !>          X (km)                 =    -0.76096183E+08
      !>          Y (km)                 =     0.32436380E+09
      !>          Z (km)                 =     0.47470484E+08
      !>
      !>         Rectangular velocity:
      !>
      !>          dX/dt (km/s)           =     0.22952075E+05
      !>          dY/dt (km/s)           =     0.53760111E+04
      !>          dZ/dt (km/s)           =    -0.20881149E+02
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius    (km)         =     0.33653522E+09
      !>          Longitude (deg)        =     0.10320290E+03
      !>          Latitude  (deg)        =     0.81089866E+01
      !>
      !>         Latitudinal velocity:
      !>
      !>          d Radius/dt    (km/s)  =    -0.11211601E+02
      !>          d Longitude/dt (deg/s) =    -0.40539288E-02
      !>          d Latitude/dt  (deg/s) =    -0.33189930E-05
      !>
      !>         Rectangular coordinates from inverse mapping:
      !>
      !>          X (km)                 =    -0.76096183E+08
      !>          Y (km)                 =     0.32436380E+09
      !>          Z (km)                 =     0.47470484E+08
      !>
      !>         Rectangular velocity from inverse mapping:
      !>
      !>          dX/dt (km/s)           =     0.22952075E+05
      !>          dY/dt (km/s)           =     0.53760111E+04
      !>          dZ/dt (km/s)           =    -0.20881149E+02
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.0, 16-JUL-2001 (WLT)
      module subroutine dlatdr ( ctx, x, y, z, jacobi )
      use pepper_kinds, only: wp
      use pepper_math, only: drdlat, reclat
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! X-coordinate of point.
      real(wp), intent(in) :: y  !! Y-coordinate of point.
      real(wp), intent(in) :: z  !! Z-coordinate of point.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.

      real(wp) :: rectan  ( 3 )
      real(wp) :: r
      real(wp) :: long
      real(wp) :: lat
      real(wp) :: injacb  ( 3, 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     There is a singularity of the Jacobian for points on the z-axis.
!
      if ( ( x == 0 ) .and. ( y == 0 ) ) then

         call ctx%setmsg ( 'The Jacobian of the transformation from '     // &
                       'rectangular to latitudinal coordinates '      // &
                       'is not defined for points on the z-axis.'      )
         call ctx%sigerr ( 'SPICE(POINTONZAXIS)'                           )
         return

      end if

!
!     We will get the Jacobian of the transformation from rectangular
!     to latitudinal coordinates by implicit differentiation.
!
!     First move the X,Y and Z coordinates into a vector.
!
      rectan = [x, y, z]

!
!     Convert from rectangular to latitudinal coordinates.
!
      call reclat ( rectan, r, long, lat  )

!
!     Get the Jacobian of the transformation from latitudinal to
!     rectangular coordinates at R, LONG, LAT.
!
      call drdlat ( r, long, lat, injacb )

!
!     Now invert INJACB to get the Jacobian of the transformation from
!     rectangular to latitudinal coordinates.
!
      call invort ( ctx, injacb, jacobi )

      return
      end subroutine dlatdr

      !> Compute the Jacobian matrix of the transformation from
      !> azimuth/elevation to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RANGE    is the distance from the origin of the input point
      !>              specified by RANGE, AZ, and EL.
      !>
      !>              Negative values for RANGE are not allowed.
      !>
      !>              Units are arbitrary and are considered to match those
      !>              of the rectangular coordinate system associated with the
      !>              output matrix JACOBI.
      !>
      !>     AZ       is the azimuth of the point. This is the angle between
      !>              the projection onto the XY plane of the vector from
      !>              the origin to the point and the +X axis of the
      !>              reference frame. AZ is zero at the +X axis.
      !>
      !>              The way azimuth is measured depends on the value of
      !>              the logical flag AZCCW. See the description of the
      !>              argument AZCCW for details.
      !>
      !>              The range (i.e., the set of allowed values) of AZ is
      !>              unrestricted. See the $Exceptions section for a
      !>              discussion on the AZ range.
      !>
      !>              Units are radians.
      !>
      !>     EL       is the elevation of the point. This is the angle
      !>              between the vector from the origin to the point and
      !>              the XY plane. EL is zero at the XY plane.
      !>
      !>              The way elevation is measured depends on the value of
      !>              the logical flag ELPLSZ. See the description of the
      !>              argument ELPLSZ for details.
      !>
      !>              The range (i.e., the set of allowed values) of EL is
      !>              [-pi/2, pi/2], but no error checking is done to ensure
      !>              that EL is within this range. See the $Exceptions
      !>              section for a discussion on the EL range.
      !>
      !>              Units are radians.
      !>
      !>     AZCCW    is a flag indicating how the azimuth is measured.
      !>
      !>              If AZCCW is .TRUE., the azimuth increases in the
      !>              counterclockwise direction; otherwise AZ increases
      !>              in the clockwise direction.
      !>
      !>     ELPLSZ   if a flag indicating how the elevation is measured.
      !>
      !>              If ELPLSZ is .TRUE., the elevation increases from
      !>              the XY plane toward +Z; otherwise toward -Z.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the
      !>              transformation from azimuth/elevation to rectangular
      !>              coordinates. It has the form
      !>
      !>                 .-                                  -.
      !>                 |  DX/DRANGE     DX/DAZ     DX/DEL   |
      !>                 |                                    |
      !>                 |  DY/DRANGE     DY/DAZ     DY/DEL   |
      !>                 |                                    |
      !>                 |  DZ/DRANGE     DZ/DAZ     DZ/DEL   |
      !>                 `-                                  -'
      !>
      !>              evaluated at the input values of RANGE, AZ and EL.
      !>
      !>              X, Y, and Z are given by the familiar formulae
      !>
      !>                 X = RANGE * COS( AZ )          * COS( EL )
      !>                 Y = RANGE * SIN( AZSNSE * AZ ) * COS( EL )
      !>                 Z = RANGE * SIN( ELDIR  * EL )
      !>
      !>              where AZSNSE is +1 when AZCCW is .TRUE. and -1
      !>              otherwise, and ELDIR is +1 when ELPLSZ is .TRUE.
      !>              and -1 otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of the input parameter RANGE is negative,
      !>         the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     2)  If the value of the input argument EL is outside the
      !>         range [-pi/2, pi/2], the results may not be as
      !>         expected.
      !>
      !>     3)  If the value of the input argument AZ is outside the
      !>         range [0, 2*pi], the value will be mapped to a value
      !>         inside the range that differs from the input value by an
      !>         integer multiple of 2*pi.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is often convenient to describe the motion of an object
      !>     in azimuth/elevation coordinates. It is also convenient to
      !>     manipulate vectors associated with the object in rectangular
      !>     coordinates.
      !>
      !>     The transformation of a azimuth/elevation state into an
      !>     equivalent rectangular state makes use of the Jacobian matrix
      !>     of the transformation between the two systems.
      !>
      !>     Given a state in latitudinal coordinates,
      !>
      !>        ( r, az, el, dr, daz, del )
      !>
      !>     the velocity in rectangular coordinates is given by the matrix
      !>     equation
      !>                    t          |                             t
      !>        (dx, dy, dz)   = JACOBI|             * (dr, daz, del)
      !>                               |(r,az,el)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(r,az,el)
      !>
      !>     In the azimuth/elevation coordinate system, several conventions
      !>     exist on how azimuth and elevation are measured. Using the AZCCW
      !>     and ELPLSZ flags, users indicate which conventions shall be used.
      !>     See the descriptions of these input arguments for details.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the azimuth/elevation state of Venus as seen from the
      !>        DSS-14 station at a given epoch. Map this state back to
      !>        rectangular coordinates as a check.
      !>
      !>        Task description
      !>        ================
      !>
      !>        In this example, we will obtain the apparent state of Venus as
      !>        seen from the DSS-14 station in the DSS-14 topocentric
      !>        reference frame. We will use a station frames kernel and
      !>        transform the resulting rectangular coordinates to azimuth,
      !>        elevation and range and its derivatives using RECAZL and
      !>        DAZLDR.
      !>
      !>        We will map this state back to rectangular coordinates using
      !>        AZLREC and DRDAZL.
      !>
      !>        In order to introduce the usage of the logical flags AZCCW
      !>        and ELPLSZ, we will request the azimuth to be measured
      !>        clockwise and the elevation positive towards +Z
      !>        axis of the DSS-14_TOPO reference frame.
      !>
      !>        Kernels
      !>        =======
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: drdazl_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                        Contents
      !>              ---------                        --------
      !>              de430.bsp                        Planetary ephemeris
      !>              naif0011.tls                     Leapseconds
      !>              earth_720101_070426.bpc          Earth historical
      !>                                                  binary PCK
      !>              earthstns_itrf93_050714.bsp      DSN station SPK
      !>              earth_topo_050714.tf             DSN station FK
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = ( 'de430.bsp',
      !>                               'naif0011.tls',
      !>                               'earth_720101_070426.bpc',
      !>                               'earthstns_itrf93_050714.bsp',
      !>                               'earth_topo_050714.tf'         )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DRDAZL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1   = '(A,F20.8)' )
      !>
      !>      CHARACTER*(*)         META
      !>      PARAMETER           ( META   = 'drdazl_ex1.tm' )
      !>
      !>      INTEGER               BDNMLN
      !>      PARAMETER           ( BDNMLN = 36 )
      !>
      !>      INTEGER               CORLEN
      !>      PARAMETER           ( CORLEN = 10 )
      !>
      !>      INTEGER               FRNMLN
      !>      PARAMETER           ( FRNMLN = 32 )
      !>
      !>      INTEGER               TIMLEN
      !>      PARAMETER           ( TIMLEN = 40 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(CORLEN)    ABCORR
      !>      CHARACTER*(BDNMLN)    OBS
      !>      CHARACTER*(TIMLEN)    OBSTIM
      !>      CHARACTER*(FRNMLN)    REF
      !>      CHARACTER*(BDNMLN)    TARGET
      !>
      !>      DOUBLE PRECISION      AZ
      !>      DOUBLE PRECISION      AZLVEL ( 3    )
      !>      DOUBLE PRECISION      DRECTN ( 3    )
      !>      DOUBLE PRECISION      EL
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      JACOBI ( 3, 3 )
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      STATE  ( 6    )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RECTAN ( 3    )
      !>
      !>      LOGICAL               AZCCW
      !>      LOGICAL               ELPLSZ
      !>
      !>C
      !>C     Load SPICE kernels.
      !>C
      !>      CALL FURNSH ( META )
      !>
      !>C
      !>C     Convert the observation time to seconds past J2000 TDB.
      !>C
      !>      OBSTIM = '2003 OCT 13 06:00:00.000000 UTC'
      !>
      !>      CALL STR2ET ( OBSTIM, ET )
      !>
      !>C
      !>C     Set the target, observer, observer frame, and
      !>C     aberration corrections.
      !>C
      !>      TARGET = 'VENUS'
      !>      OBS    = 'DSS-14'
      !>      REF    = 'DSS-14_TOPO'
      !>      ABCORR = 'CN+S'
      !>
      !>C
      !>C     Compute the observer-target state.
      !>C
      !>      CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBS,
      !>     .              STATE,  LT                   )
      !>
      !>C
      !>C     Convert position to azimuth/elevation coordinates,
      !>C     with azimuth increasing clockwise and elevation
      !>C     positive towards +Z axis of the DSS-14_TOPO
      !>C     reference frame
      !>C
      !>      AZCCW  = .FALSE.
      !>      ELPLSZ = .TRUE.
      !>
      !>      CALL RECAZL ( STATE, AZCCW, ELPLSZ, R, AZ, EL )
      !>
      !>C
      !>C     Convert velocity to azimuth/elevation coordinates.
      !>C
      !>      CALL DAZLDR ( STATE(1), STATE(2), STATE(3),
      !>     .              AZCCW,    ELPLSZ,   JACOBI   )
      !>
      !>      CALL MXV ( JACOBI, STATE(4), AZLVEL )
      !>
      !>C
      !>C     As a check, convert the azimuth/elevation state back to
      !>C     rectangular coordinates.
      !>C
      !>      CALL AZLREC ( R, AZ, EL, AZCCW, ELPLSZ, RECTAN )
      !>
      !>      CALL DRDAZL ( R, AZ, EL, AZCCW, ELPLSZ, JACOBI )
      !>
      !>      CALL MXV ( JACOBI, AZLVEL, DRECTN )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'AZ/EL coordinates:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   Range      (km)        = ', R
      !>      WRITE(*,FMT1) '   Azimuth    (deg)       = ', AZ * DPR()
      !>      WRITE(*,FMT1) '   Elevation  (deg)       = ', EL * DPR()
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)')    'AZ/EL velocity:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   d Range/dt     (km/s)  = ', AZLVEL(1)
      !>      WRITE(*,FMT1) '   d Azimuth/dt   (deg/s) = ', AZLVEL(2)
      !>     .                                             * DPR()
      !>      WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLVEL(3)
      !>     .                                             * DPR()
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular coordinates:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   X (km)                 = ', STATE(1)
      !>      WRITE(*,FMT1) '   Y (km)                 = ', STATE(2)
      !>      WRITE(*,FMT1) '   Z (km)                 = ', STATE(3)
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular velocity:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   dX/dt (km/s)           = ', STATE(4)
      !>      WRITE(*,FMT1) '   dY/dt (km/s)           = ', STATE(5)
      !>      WRITE(*,FMT1) '   dZ/dt (km/s)           = ', STATE(6)
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular coordinates from inverse '
      !>     .    //         'mapping:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   X (km)                 = ', RECTAN(1)
      !>      WRITE(*,FMT1) '   Y (km)                 = ', RECTAN(2)
      !>      WRITE(*,FMT1) '   Z (km)                 = ', RECTAN(3)
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Rectangular velocity from inverse '
      !>     .    //         'mapping:'
      !>      WRITE(*,*)
      !>      WRITE(*,FMT1) '   dX/dt (km/s)           = ', DRECTN(1)
      !>      WRITE(*,FMT1) '   dY/dt (km/s)           = ', DRECTN(2)
      !>      WRITE(*,FMT1) '   dZ/dt (km/s)           = ', DRECTN(3)
      !>      WRITE(*,*)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        AZ/EL coordinates:
      !>
      !>           Range      (km)        =   245721478.99272084
      !>           Azimuth    (deg)       =         294.48543372
      !>           Elevation  (deg)       =         -48.94609726
      !>
      !>        AZ/EL velocity:
      !>
      !>           d Range/dt     (km/s)  =          -4.68189834
      !>           d Azimuth/dt   (deg/s) =           0.00402256
      !>           d Elevation/dt (deg/s) =          -0.00309156
      !>
      !>        Rectangular coordinates:
      !>
      !>           X (km)                 =    66886767.37916667
      !>           Y (km)                 =   146868551.77222887
      !>           Z (km)                 =  -185296611.10841590
      !>
      !>        Rectangular velocity:
      !>
      !>           dX/dt (km/s)           =        6166.04150307
      !>           dY/dt (km/s)           =      -13797.77164550
      !>           dZ/dt (km/s)           =       -8704.32385654
      !>
      !>        Rectangular coordinates from inverse mapping:
      !>
      !>           X (km)                 =    66886767.37916658
      !>           Y (km)                 =   146868551.77222890
      !>           Z (km)                 =  -185296611.10841590
      !>
      !>        Rectangular velocity from inverse mapping:
      !>
      !>           dX/dt (km/s)           =        6166.04150307
      !>           dY/dt (km/s)           =      -13797.77164550
      !>           dZ/dt (km/s)           =       -8704.32385654
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 08-SEP-2021 (JDR) (NJB)
      module subroutine drdazl ( ctx, range, az, el, azccw, elplsz, jacobi )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: drdlat
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: range  !! Distance of a point from the origin.
      real(wp), intent(in) :: az  !! Azimuth of input point in radians.
      real(wp), intent(in) :: el  !! Elevation of input point in radians.
      logical, intent(in) :: azccw  !! Flag indicating how azimuth is measured.
      logical, intent(in) :: elplsz  !! Flag indicating how elevation is measured.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.

      real(wp) :: lat
      real(wp) :: lon

      integer(ip) :: azsnse
      integer(ip) :: eldir
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The input range must be non-negative. If not, signal an error.
!
      if ( range < 0.0_wp ) then

         call ctx%setmsg ( 'Input range was #. Negative ' &
         //            'values are not allowed.'  )
         call ctx%errdp  ( '#', range                 )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'   )
         return

      end if

!
!     Convert the input azimuth and elevation to the equivalent
!     latitudinal coordinates, and define the rotation sense for the
!     azimuth and direction for the elevation
!
      if ( azccw ) then

         lon    = az
         azsnse = 1

      else

         lon    = -az
         azsnse = -1

      end if

      if ( elplsz ) then

         lat    = el
         eldir  = 1

      else

         lat    = -el
         eldir  = -1

      end if

!
!     Now we have the latitudinal equivalent coordinates, use them to
!     find the Jacobian matrix of rectangular coordinates with respect
!     to latitudinal coordinates.
!
      call drdlat ( range, lon, lat, jacobi )

!
!     The matrix JACOBI is
!
!        .-                             -.
!        |  DX/DRANGE  DX/DLON  DX/DLAT  |
!        |  DY/DRANGE  DY/DLON  DY/DLAT  |
!        |  DZ/DRANGE  DZ/DLON  DZ/DLAT  |
!        `-                             -'
!
!     Given that
!
!        LON = AZSNSE * AZ
!        LAT = ELDIR  * EL
!
!     applying the chain rule to derivative of each Cartesian
!     component with respect to the latitude and longitude, the matrix
!     above is equivalent to
!
!        .-                                                      -.
!        |  DX/DRANGE   (1/AZSNSE) * DX/DAZ   (1/ELDIR) * DX/DEL  |
!        |  DY/DRANGE   (1/AZSNSE) * DY/DAZ   (1/ELDIR) * DY/DEL  |
!        |  DZ/DRANGE   (1/AZSNSE) * DZ/DAZ   (1/ELDIR) * DZ/DEL  |
!        `-                                                      -'
!
!     We have
!
!        AZSNSE = 1 / AZSNSE
!        ELDIR  = 1 / ELDIR
!
!     So, multiplying the second column of JACOBI by AZSNSE and the
!     third column of JACOBI by ELDIR gives us the matrix we actually
!     want to compute: the Jacobian matrix of rectangular
!     coordinates with respect to azimuth/elevation coordinates.
!
      do i = 1, 3

         jacobi(i,2) = azsnse * jacobi(i,2)
         jacobi(i,3) = eldir  * jacobi(i,3)

      end do

      return
      end subroutine drdazl

      !> Compute the Jacobian matrix of the transformation from geodetic
      !> to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     LON      is the geodetic longitude of point (radians).
      !>
      !>     LAT      is the geodetic latitude  of point (radians).
      !>
      !>     ALT      is the altitude of point above the reference spheroid.
      !>
      !>     RE       is the equatorial radius of the reference spheroid.
      !>
      !>     F        is the flattening coefficient = (RE-RP) / RE,  where
      !>              RP is the polar radius of the spheroid. (More
      !>              importantly RP = RE*(1-F).)
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between geodetic and rectangular coordinates. It
      !>              has the form
      !>
      !>                 .-                             -.
      !>                 |  DX/DLON   DX/DLAT  DX/DALT   |
      !>                 |  DY/DLON   DY/DLAT  DY/DALT   |
      !>                 |  DZ/DLON   DZ/DLAT  DZ/DALT   |
      !>                 `-                             -'
      !>
      !>              evaluated at the input values of LON, LAT and ALT.
      !>
      !>              The formulae for computing X, Y, and Z from
      !>              geodetic coordinates are given below.
      !>
      !>                 X = [ALT +          RE/G(LAT,F)]*COS(LON)*COS(LAT)
      !>                 Y = [ALT +          RE/G(LAT,F)]*SIN(LON)*COS(LAT)
      !>                 Z = [ALT + RE*(1-F)**2/G(LAT,F)]*         SIN(LAT)
      !>
      !>              where
      !>
      !>                 RE is the polar radius of the reference spheroid.
      !>
      !>                 F  is the flattening factor (the polar radius is
      !>                    obtained by multiplying the equatorial radius by
      !>                    1-F).
      !>
      !>                 G( LAT, F ) is given by
      !>
      !>                    sqrt ( cos(lat)**2 + (1-f)**2 * sin(lat)**2 )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the flattening coefficient is greater than or equal to
      !>         one, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     2)  If the equatorial radius is non-positive, the error
      !>         SPICE(BADRADIUS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is often convenient to describe the motion of an object in
      !>     the geodetic coordinate system. However, when performing
      !>     vector computations its hard to beat rectangular coordinates.
      !>
      !>     To transform states given with respect to geodetic coordinates
      !>     to states with respect to rectangular coordinates, one makes use
      !>     of the Jacobian of the transformation between the two systems.
      !>
      !>     Given a state in geodetic coordinates
      !>
      !>          ( lon, lat, alt, dlon, dlat, dalt )
      !>
      !>     the velocity in rectangular coordinates is given by the matrix
      !>     equation:
      !>
      !>                    t          |                                  t
      !>        (dx, dy, dz)   = JACOBI|              * (dlon, dlat, dalt)
      !>                               |(lon,lat,alt)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(lon,lat,alt)
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the geodetic state of the earth as seen from
      !>        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
      !>        Map this state back to rectangular coordinates as a check.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: drdgeo_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DRDGEO_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,E18.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ALT
      !>      DOUBLE PRECISION      DRECTN ( 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      F
      !>      DOUBLE PRECISION      JACOBI ( 3, 3 )
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      GEOVEL ( 3 )
      !>      DOUBLE PRECISION      RADII  ( 3 )
      !>      DOUBLE PRECISION      RE
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      RP
      !>      DOUBLE PRECISION      STATE  ( 6 )
      !>
      !>      INTEGER               N
      !>
      !>C
      !>C     Load SPK, PCK, and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'drdgeo_ex1.tm' )
      !>
      !>C
      !>C     Look up the radii for Mars.  Although we
      !>C     omit it here, we could first call BADKPV
      !>C     to make sure the variable BODY499_RADII
      !>C     has three elements and numeric data type.
      !>C     If the variable is not present in the kernel
      !>C     pool, BODVRD will signal an error.
      !>C
      !>      CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII )
      !>
      !>C
      !>C     Compute flattening coefficient.
      !>C
      !>      RE  =  RADII(1)
      !>      RP  =  RADII(3)
      !>      F   =  ( RE - RP ) / RE
      !>
      !>C
      !>C     Look up the apparent state of earth as seen from Mars
      !>C     at January 1, 2005 TDB, relative to the IAU_MARS
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( 'January 1, 2005 TDB', ET )
      !>
      !>      CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S',
      !>     .              'Mars',  STATE, LT                )
      !>
      !>C
      !>C     Convert position to geodetic coordinates.
      !>C
      !>      CALL RECGEO ( STATE, RE, F, LON, LAT, ALT )
      !>
      !>C
      !>C     Convert velocity to geodetic coordinates.
      !>C
      !>
      !>      CALL DGEODR (  STATE(1), STATE(2), STATE(3),
      !>     .               RE,       F,        JACOBI   )
      !>
      !>      CALL MXV ( JACOBI, STATE(4), GEOVEL )
      !>
      !>C
      !>C     As a check, convert the geodetic state back to
      !>C     rectangular coordinates.
      !>C
      !>      CALL GEOREC ( LON, LAT, ALT, RE, F, RECTAN )
      !>
      !>      CALL DRDGEO ( LON, LAT, ALT, RE, F, JACOBI )
      !>
      !>      CALL MXV ( JACOBI, GEOVEL, DRECTN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                 = ', STATE(1)
      !>      WRITE(*,FMT1) '  Y (km)                 = ', STATE(2)
      !>      WRITE(*,FMT1) '  Z (km)                 = ', STATE(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)           = ', STATE(4)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)           = ', STATE(5)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)           = ', STATE(6)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Ellipsoid shape parameters: '
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Equatorial radius (km) = ', RE
      !>      WRITE(*,FMT1) '  Polar radius      (km) = ', RP
      !>      WRITE(*,FMT1) '  Flattening coefficient = ', F
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Geodetic coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Longitude (deg)        = ', LON / RPD()
      !>      WRITE(*,FMT1) '  Latitude  (deg)        = ', LAT / RPD()
      !>      WRITE(*,FMT1) '  Altitude  (km)         = ', ALT
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Geodetic velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ',
      !>     .                                         GEOVEL(1)/RPD()
      !>      WRITE(*,FMT1) '  d Latitude/dt  (deg/s) = ',
      !>     .                                         GEOVEL(2)/RPD()
      !>      WRITE(*,FMT1) '  d Altitude/dt  (km/s)  = ', GEOVEL(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from inverse ' //
      !>     .           'mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                 = ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y (km)                 = ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z (km)                 = ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity from inverse mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)           = ', DRECTN(1)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)           = ', DRECTN(2)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)           = ', DRECTN(3)
      !>      WRITE(*,*) ' '
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Rectangular coordinates:
      !>
      !>          X (km)                 =    -0.76096183E+08
      !>          Y (km)                 =     0.32436380E+09
      !>          Z (km)                 =     0.47470484E+08
      !>
      !>         Rectangular velocity:
      !>
      !>          dX/dt (km/s)           =     0.22952075E+05
      !>          dY/dt (km/s)           =     0.53760111E+04
      !>          dZ/dt (km/s)           =    -0.20881149E+02
      !>
      !>         Ellipsoid shape parameters:
      !>
      !>          Equatorial radius (km) =     0.33961900E+04
      !>          Polar radius      (km) =     0.33762000E+04
      !>          Flattening coefficient =     0.58860076E-02
      !>
      !>         Geodetic coordinates:
      !>
      !>          Longitude (deg)        =     0.10320290E+03
      !>          Latitude  (deg)        =     0.81089876E+01
      !>          Altitude  (km)         =     0.33653182E+09
      !>
      !>         Geodetic velocity:
      !>
      !>          d Longitude/dt (deg/s) =    -0.40539288E-02
      !>          d Latitude/dt  (deg/s) =    -0.33189934E-05
      !>          d Altitude/dt  (km/s)  =    -0.11211601E+02
      !>
      !>         Rectangular coordinates from inverse mapping:
      !>
      !>          X (km)                 =    -0.76096183E+08
      !>          Y (km)                 =     0.32436380E+09
      !>          Z (km)                 =     0.47470484E+08
      !>
      !>         Rectangular velocity from inverse mapping:
      !>
      !>          dX/dt (km/s)           =     0.22952075E+05
      !>          dY/dt (km/s)           =     0.53760111E+04
      !>          dZ/dt (km/s)           =    -0.20881149E+02
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Changed the input argument name LONG to LON for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT)
      module subroutine drdgeo ( ctx, lon, lat, alt, re, f, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: lon  !! Geodetic longitude of point (radians).
      real(wp), intent(in) :: lat  !! Geodetic latitude of point (radians).
      real(wp), intent(in) :: alt  !! Altitude of point above the reference spheroid.
      real(wp), intent(in) :: re  !! Equatorial radius of the reference spheroid.
      real(wp), intent(in) :: f  !! Flattening coefficient.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.

      integer(ip), parameter :: dx = 1  !! row of the partial derivatives of X

      integer(ip), parameter :: dy = 2  !! row of the partial derivatives of Y

      integer(ip), parameter :: dz = 3

      integer(ip), parameter :: dlon = 1

      integer(ip), parameter :: dlat = 2

      integer(ip), parameter :: dalt = 3

      real(wp) :: clat
      real(wp) :: clon
      real(wp) :: dgdlat
      real(wp) :: g
      real(wp) :: slat
      real(wp) :: slon

      real(wp) :: flat
      real(wp) :: flat2
      real(wp) :: g2

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If the flattening coefficient is greater than one, the polar
!     radius computed below is negative. If it's equal to one, the
!     polar radius is zero. Either case is a problem, so signal an
!     error.
!
      if ( f >= 1.0_wp ) then

          call ctx%setmsg ( 'Flattening coefficient was *.'  )
          call ctx%errdp  ( '*', f                           )
          call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
          return

      end if

      if ( re <= 0.0_wp ) then

          call ctx%setmsg ( 'Equatorial Radius <= 0.0D0. RE = *' )
          call ctx%errdp  ( '*', re                              )
          call ctx%sigerr ( 'SPICE(BADRADIUS)'                   )
          return

      end if

!
!     For the record, here is a derivation of the formulae for the
!     values of x, y and z as a function of longitude, latitude and
!     altitude.
!
!     First, let's take the case where the longitude is 0. Moreover,
!     lets assume that the length of the equatorial axis is a and
!     that the polar axis is b:
!
!        a = re
!        b = re * (1-f)
!
!     For any point on the spheroid where y is zero we know that there
!     is a unique q in the range (-Pi, Pi] such that
!
!        x = a cos(q) and z = b sin(q).
!
!     The normal to the surface at such a point is given by
!
!           cos(q)     sin(q)
!        ( ------- ,  ------- )
!             a          b
!
!     The unit vector in the same direction is
!
!                 b cos(q)                         a sin(q)
!        ( --------------------------  ,  -------------------------- )
!             ______________________         ______________________
!            / 2   2        2   2           / 2   2        2   2
!          \/ b cos (q)  + a sin (q)      \/ b cos (q)  + a sin (q)
!
!     The first component of this term is by definition equal to the
!     cosine of the geodetic latitude, thus
!
!                                ______________________
!                               / 2   2        2   2
!        b cos(q) = cos(lat)  \/ b cos (q)  + a sin (q)
!
!     This can be transformed to the equation
!
!                                ______________________________
!                               /   2    2     2        2
!        b cos(q) = cos(lat)  \/ ( b  - a  )cos (q)  + a
!
!     Squaring both sides and rearranging terms gives:
!
!         2   2         2         2   2     2        2    2
!        b cos (q) + cos (lat) ( a - b ) cos (q) =  a  cos (lat)
!
!     Thus
!                           2    2
!           2              a  cos (lat)
!        cos (q)  =  --------------------------
!                     2    2         2   2
!                    b  sin (lat) + a cos (lat)
!
!                             cos (lat)
!                 =  ------------------------------
!                       _____________________________
!                      /      2    2           2
!                    \/  (b/a)  sin (lat) + cos (lat)
!
!                             cos (lat)
!                 =  ---------------------------------
!                       _____________________________
!                      /      2    2           2
!                    \/  (1-f)  sin (lat) + cos (lat)
!
!     From this one can also conclude that
!
!                           (1-f) sin (lat)
!        sin(q)   =  ----------------------------------
!                        _____________________________
!                       /      2    2           2
!                     \/  (1-f)  sin (lat) + cos (lat)
!
!     Thus the point on the surface of the spheroid is given by
!
!                            re * cos (lat)
!        x_0      =  ---------------------------------
!                        _____________________________
!                       /      2    2           2
!                     \/  (1-f)  sin (lat) + cos (lat)
!
!                                  2
!                        re * (1-f) sin (lat)
!        z_0      =  ----------------------------------
!                        _____________________________
!                       /      2    2           2
!                     \/  (1-f)  sin (lat) + cos (lat)
!
!     Thus given a point with the same latitude but a non-zero
!     longitude, one can conclude that
!
!                         re * cos (lon) *cos (lat)
!        x_0      =  ---------------------------------
!                        _____________________________
!                       /      2    2           2
!                     \/  (1-f)  sin (lat) + cos (lat)
!
!                         re * sin (lon) cos (lat)
!        y_0      =  ---------------------------------
!                        _____________________________
!                       /      2    2           2
!                     \/  (1-f)  sin (lat) + cos (lat)
!
!                                    2
!                          re * (1-f) sin (lat)
!        z_0      =  ----------------------------------
!                        _____________________________
!                       /      2    2           2
!                     \/  (1-f)  sin (lat) + cos (lat)
!
!     The unit normal, n, at this point is simply
!
!        ( cos(lon)cos(lat),  sin(lon)cos(lat),  sin(lat) )
!
!     Thus for a point at altitude alt, we simply add the vector
!
!        alt*n
!
!     to the vector ( x_0, y_0, z_0 ).  Hence we have
!
!        x = [ alt +          re/g(lat,f) ] * cos(lon) * cos(lat)
!        y = [ alt +          re/g(lat,f) ] * sin(lon) * cos(lat)
!        z = [ alt + re*(1-f)**2/g(lat,f) ] *            sin(lat)
!
!     We're going to need the sine and cosine of LAT and LON many
!     times.  We'll just compute them once.
!
      clat              = dcos ( lat )
      clon              = dcos ( lon )
      slat              = dsin ( lat )
      slon              = dsin ( lon )

!
!     Referring to the G given in the header we have...
!
      flat              = 1.0_wp - f
      flat2             = flat * flat

      g                 = dsqrt( clat*clat   +   flat2 * slat*slat )
      g2                = g * g
      dgdlat            = ( -1.0_wp  +  flat2 ) * slat * clat / g

!
!     Now simply take the partial derivatives of the x,y,z w.r.t.
!     lon, lat, alt.
!
      jacobi (dx,dlon)  =  - ( alt + re/g ) * slon * clat
      jacobi (dy,dlon)  =    ( alt + re/g ) * clon * clat
      jacobi (dz,dlon)  =    0.0_wp

      jacobi (dx,dlat)  =    (      -  re*dgdlat / g2 ) * clon * clat &
                           - ( alt  +  re        / g  ) * clon * slat

      jacobi (dy,dlat)  =    (      -  re*dgdlat / g2 ) * slon * clat &
                           - ( alt  +  re        / g  ) * slon * slat

      jacobi (dz,dlat)  =    (      -  flat2*re*dgdlat / g2 ) * slat &
                           + ( alt  +  flat2*re        / g  ) * clat

      jacobi (dx,dalt)  =  clon * clat
      jacobi (dy,dalt)  =  slon * clat
      jacobi (dz,dalt)  =         slat

      return
      end subroutine drdgeo

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to spherical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     X,
      !>     Y,
      !>     Z        are the rectangular coordinates of the point at
      !>              which the Jacobian of the map from rectangular
      !>              to spherical coordinates is desired.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between rectangular and spherical coordinates. It
      !>              has the form
      !>
      !>                 .-                                  -.
      !>                 |  DR/DX       DR/DY      DR/DZ      |
      !>                 |  DCOLAT/DX   DCOLAT/DY  DCOLAT/DZ  |
      !>                 |  DLONG/DX    DLONG/DY   DLONG/DZ   |
      !>                 `-                                  -'
      !>
      !>              evaluated at the input values of X, Y, and Z.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input point is on the Z-axis (X and Y = 0), the
      !>         Jacobian is undefined, the error SPICE(POINTONZAXIS) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     When performing vector calculations with velocities it is
      !>     usually most convenient to work in rectangular coordinates.
      !>     However, once the vector manipulations have been performed
      !>     it is often desirable to convert the rectangular representations
      !>     into spherical coordinates to gain insights about phenomena
      !>     in this coordinate frame.
      !>
      !>     To transform rectangular velocities to derivatives of coordinates
      !>     in a spherical system, one uses the Jacobian of the
      !>     transformation between the two systems.
      !>
      !>     Given a state in rectangular coordinates
      !>
      !>          ( x, y, z, dx, dy, dz )
      !>
      !>     the corresponding spherical coordinate derivatives are given by
      !>     the matrix equation:
      !>
      !>                           t          |                    t
      !>        (dr, dcolat, dlong)   = JACOBI|      * (dx, dy, dz)
      !>                                      |(x,y,z)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(x, y, z)
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the spherical state of the Earth as seen from
      !>        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
      !>        Map this state back to rectangular coordinates as a check.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: dsphdr_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de421.bsp                     Planetary ephemeris
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DSPHDR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,E18.8)' )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      DRECTN ( 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      JACOBI ( 3, 3 )
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      SPHVEL ( 3 )
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      STATE  ( 6 )
      !>
      !>C
      !>C     Load SPK, PCK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'dsphdr_ex1.tm' )
      !>
      !>C
      !>C     Look up the apparent state of earth as seen from Mars at
      !>C     January 1, 2005 TDB, relative to the IAU_MARS reference
      !>C     frame.
      !>C
      !>      CALL STR2ET ( 'January 1, 2005 TDB', ET )
      !>
      !>      CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S',
      !>     .              'Mars',  STATE, LT                )
      !>
      !>C
      !>C     Convert position to spherical coordinates.
      !>C
      !>      CALL RECSPH ( STATE, R, COLAT, SLON )
      !>
      !>C
      !>C     Convert velocity to spherical coordinates.
      !>C
      !>
      !>      CALL DSPHDR ( STATE(1), STATE(2), STATE(3), JACOBI )
      !>
      !>      CALL MXV ( JACOBI, STATE(4), SPHVEL )
      !>
      !>C
      !>C     As a check, convert the spherical state back to
      !>C     rectangular coordinates.
      !>C
      !>      CALL SPHREC ( R, COLAT, SLON, RECTAN )
      !>
      !>      CALL DRDSPH ( R, COLAT, SLON, JACOBI )
      !>
      !>      CALL MXV ( JACOBI, SPHVEL, DRECTN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                  = ', STATE(1)
      !>      WRITE(*,FMT1) '  Y (km)                  = ', STATE(2)
      !>      WRITE(*,FMT1) '  Z (km)                  = ', STATE(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)            = ', STATE(4)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)            = ', STATE(5)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)            = ', STATE(6)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km)         = ', R
      !>      WRITE(*,FMT1) '  Colatitude (deg)        = ',
      !>     .                                             COLAT/RPD()
      !>      WRITE(*,FMT1) '  Longitude  (deg)        = ', SLON/RPD()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical velocity:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  d Radius/dt     (km/s)  = ', SPHVEL(1)
      !>      WRITE(*,FMT1) '  d Colatitude/dt (deg/s) = ',
      !>     .                                        SPHVEL(2)/RPD()
      !>      WRITE(*,FMT1) '  d Longitude/dt  (deg/s) = ',
      !>     .                                        SPHVEL(3)/RPD()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from inverse ' //
      !>     .           'mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X (km)                  = ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y (km)                  = ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z (km)                  = ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular velocity from inverse mapping:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  dX/dt (km/s)            = ', DRECTN(1)
      !>      WRITE(*,FMT1) '  dY/dt (km/s)            = ', DRECTN(2)
      !>      WRITE(*,FMT1) '  dZ/dt (km/s)            = ', DRECTN(3)
      !>      WRITE(*,*) ' '
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Rectangular coordinates:
      !>
      !>          X (km)                  =    -0.76096183E+08
      !>          Y (km)                  =     0.32436380E+09
      !>          Z (km)                  =     0.47470484E+08
      !>
      !>         Rectangular velocity:
      !>
      !>          dX/dt (km/s)            =     0.22952075E+05
      !>          dY/dt (km/s)            =     0.53760111E+04
      !>          dZ/dt (km/s)            =    -0.20881149E+02
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius     (km)         =     0.33653522E+09
      !>          Colatitude (deg)        =     0.81891013E+02
      !>          Longitude  (deg)        =     0.10320290E+03
      !>
      !>         Spherical velocity:
      !>
      !>          d Radius/dt     (km/s)  =    -0.11211601E+02
      !>          d Colatitude/dt (deg/s) =     0.33189930E-05
      !>          d Longitude/dt  (deg/s) =    -0.40539288E-02
      !>
      !>         Rectangular coordinates from inverse mapping:
      !>
      !>          X (km)                  =    -0.76096183E+08
      !>          Y (km)                  =     0.32436380E+09
      !>          Z (km)                  =     0.47470484E+08
      !>
      !>         Rectangular velocity from inverse mapping:
      !>
      !>          dX/dt (km/s)            =     0.22952075E+05
      !>          dY/dt (km/s)            =     0.53760111E+04
      !>          dZ/dt (km/s)            =    -0.20881149E+02
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT)
      module subroutine dsphdr ( ctx, x, y, z, jacobi )
      use pepper_kinds, only: wp
      use pepper_math, only: drdsph, recsph
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! X-coordinate of point.
      real(wp), intent(in) :: y  !! Y-coordinate of point.
      real(wp), intent(in) :: z  !! Z-coordinate of point.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.

      real(wp) :: rectan  ( 3 )
      real(wp) :: r
      real(wp) :: colat
      real(wp) :: long
      real(wp) :: injacb  ( 3, 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     There is a singularity of the jacobian for points on the z-axis.
!
      if ( ( x == 0 ) .and. ( y == 0 ) ) then

         call ctx%setmsg ( 'The Jacobian of the transformation from '     // &
                       'rectangular to spherical coordinates '        // &
                       'is not defined for points on the z-axis.'      )
         call ctx%sigerr ( 'SPICE(POINTONZAXIS)'                           )
         return

      end if

!
!     We will get the Jacobian of the transformation from rectangular
!     to spherical coordinates by implicit differentiation.
!
!     First move the X,Y and Z coordinates into a vector.
!
      rectan = [x, y, z]

!
!     Convert from rectangular to spherical coordinates.
!
      call recsph ( rectan, r, colat, long  )

!
!     Get the Jacobian of the transformation from spherical to
!     rectangular coordinates at R, COLAT, LONG.
!
      call drdsph ( r, colat, long, injacb )

!
!     Now invert INJACB to get the Jacobian of the transformation from
!     rectangular to spherical coordinates.
!
      call invort ( ctx, injacb, jacobi )

      return
      end subroutine dsphdr

      !> Compute the state (position and velocity) of an object whose
      !> trajectory is described via equinoctial elements relative to some
      !> fixed plane (usually the equatorial plane of some planet).
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch (ephemeris time) at which the state
      !>              of the target body is to be computed. ET is measured
      !>              in seconds past the J2000 epoch.
      !>
      !>     EPOCH    is the epoch of the equinoctial elements in seconds
      !>              past the J2000 epoch.
      !>
      !>     EQEL     is an array of 9 double precision numbers that are the
      !>              equinoctial elements for some orbit expressed relative to
      !>              the equatorial frame of the central body defined as
      !>
      !>              -  The Z-axis of the equatorial frame is the direction
      !>                 of the pole of the central body relative to some
      !>                 inertial frame;
      !>
      !>              -  The X-axis is given by the cross product of the Z-axis
      !>                 of the inertial frame with the direction of the pole
      !>                 of the central body; and
      !>
      !>              -  The Y-axis completes a right handed frame.
      !>
      !>              If the X-axis of the equatorial frame is aligned with the
      !>              X-axis of the inertial frame, then the X-axis of the
      !>              equatorial frame will be located at 90 degrees + RAPOL in
      !>              the inertial frame.
      !>
      !>              The specific arrangement of the elements is spelled out
      !>              below:
      !>
      !>                 EQEL(1)   is the semi-major axis (A) of the orbit in
      !>                           km.
      !>
      !>                 EQEL(2)   is the value of H at the specified epoch.
      !>                           ( E*SIN(ARGP+NODE) ).
      !>
      !>                 EQEL(3)   is the value of K at the specified epoch
      !>                           ( E*COS(ARGP+NODE) ).
      !>
      !>                 EQEL(4)   is the mean longitude (MEAN0+ARGP+NODE) at
      !>                           the epoch of the elements measured in
      !>                           radians.
      !>
      !>                 EQEL(5)   is the value of P (TAN(INC/2)*SIN(NODE)) at
      !>                           the specified epoch.
      !>
      !>                 EQEL(6)   is the value of Q (TAN(INC/2)*COS(NODE)) at
      !>                           the specified epoch.
      !>
      !>                 EQEL(7)   is the rate of the longitude of periapse
      !>                           (dARGP/dt + dNODE/dt ) at the epoch of
      !>                           the elements. This rate is assumed to hold
      !>                           for all time. The rate is measured in
      !>                           radians per second.
      !>
      !>                 EQEL(8)   is the derivative of the mean longitude
      !>                           ( dM/dt + dARGP/dt + dNODE/dt ). This
      !>                           rate is assumed to be constant and is
      !>                           measured in radians/second.
      !>
      !>                 EQEL(9)   is the rate of the longitude of the
      !>                           ascending node ( dNODE/dt). This rate is
      !>                           measured in radians per second.
      !>
      !>              where
      !>
      !>                 INC       is the inclination of the orbit,
      !>
      !>                 ARGP      is the argument of periapse,
      !>
      !>                 NODE      is longitude of the ascending node, and
      !>
      !>                 E         is eccentricity of the orbit.
      !>
      !>     RAPOL    is the Right Ascension of the pole of the reference plane
      !>              with respect to some inertial frame (measured in
      !>              radians).
      !>
      !>     DECPOL   is the Declination of the pole of the reference plane
      !>              with respect to some inertial frame (measured in
      !>              radians).
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state of the object described by EQEL relative to
      !>              the inertial frame used to define RAPOL and DECPOL. Units
      !>              are in km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the eccentricity corresponding to the input elements is
      !>         greater than 0.9, the error SPICE(ECCOUTOFRANGE) is signaled.
      !>
      !>     2)  If the semi-major axis of the elements is non-positive, the
      !>         error SPICE(BADSEMIAXIS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine evaluates the input equinoctial elements for
      !>     the specified epoch and return the corresponding state.
      !>
      !>     This routine was adapted from a routine provided by
      !>     Bob Jacobson of the Planetary Dynamics Group of
      !>     the Navigation and Flight Mechanics Section at JPL.
      !>
      !>## Examples
      !>
      !>     Suppose you have classical elements and rates of
      !>     change of the ascending node and argument of periapse
      !>     for some satellite of the earth.
      !>
      !>     By transforming the classical elements
      !>     this routine can be used to compute the state of the
      !>     object at an arbitrary epoch. The code below illustrates
      !>     how you might do this.
      !>
      !>     The table below illustrates the meanings of the various
      !>     variables used in the discussion below.
      !>
      !>           Variable     Meaning
      !>           --------     ----------------------------------
      !>           A            Semi-major axis in km
      !>           ECC          Eccentricity of orbit
      !>           INC          Inclination of orbit
      !>           NODE         Longitude of the ascending node at epoch
      !>           OMEGA        Argument of periapse at epoch
      !>           M            Mean anomaly at epoch
      !>           DMDT         Mean anomaly rate in radians/second
      !>           DNODE        Rate of change of longitude of ascending node
      !>                        in radians/second
      !>           DARGP        Rate of change of argument of periapse in
      !>                        radians/second
      !>           EPOCH        is the epoch of the elements in seconds past
      !>                        the J2000 epoch.
      !>
      !>        EQEL(1) = A
      !>        EQEL(2) = ECC * DSIN ( OMEGA + NODE )
      !>        EQEL(3) = ECC * DCOS ( OMEGA + NODE )
      !>
      !>        EQEL(4) = M + OMEGA + NODE
      !>
      !>        EQEL(5) = TAN(INC/2.0D0) * DSIN(NODE)
      !>        EQEL(6) = TAN(INC/2.0D0) * DCOS(NODE)
      !>
      !>        EQEL(7) = DARGP
      !>        EQEL(8) = DARGP + DMDT + DNODE
      !>        EQEL(9) = DNODE
      !>
      !>        We shall compute the state of the satellite in the
      !>        pole and equator reference system.
      !>
      !>        RAPOL   = -HALFPI()
      !>        DECPOL  =  HALFPI()
      !>
      !>        Now compute the state at the desired epoch ET.
      !>
      !>        CALL EQNCPV ( ET, EPOCH, EQEL, RAPOL, DECPOL, STATE )
      !>
      !>## Restrictions
      !>
      !>     1)  The equinoctial elements used by this routine are taken
      !>         from  "Tangent" formulation of equinoctial elements
      !>
      !>            p = tan(inclination/2) * sin(R.A. of ascending node)
      !>            q = tan(inclination/2) * cos(R.A. of ascending node)
      !>
      !>         Other formulations use Sine instead of Tangent. We shall
      !>         call these the "Sine" formulations.
      !>
      !>            p = sin(inclination/2) * sin(R.A. of ascending node)
      !>            q = sin(inclination/2) * cos(R.A. of ascending node)
      !>
      !>         If you have equinoctial elements from this alternative
      !>         formulation you should replace p and q  by the
      !>         expressions below.
      !>
      !>            P = P / DSQRT ( 1.0D0 - P*P - Q*Q )
      !>            Q = Q / DSQRT ( 1.0D0 - P*P - Q*Q )
      !>
      !>         This will convert the Sine formulation to the Tangent
      !>         formulation.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Owen and R. Vaughan, "Optical Navigation Program
      !>          Mathematical Models," JPL Engineering Memorandum 314-513,
      !>          August 9, 1991.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     R.A. Jacobson      (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.3, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added SPK
      !>        required reading.
      !>
      !>-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 1.0.1, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard header section heading
      !>        'Declarations_of_external_functions'.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT) (RAJ)
      module subroutine eqncpv ( ctx, et, epoch, eqel, rapol, decpol, state )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      use pepper_math, only: mxv
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch in seconds past J2000 to find state
      real(wp), intent(in) :: epoch  !! Epoch of elements in seconds past J2000
      real(wp), intent(in) :: eqel ( 9 )  !! Array of equinoctial elements
      real(wp), intent(in) :: rapol  !! Right Ascension of the pole of the reference plane
      real(wp), intent(in) :: decpol  !! Declination of the pole of the reference plane
      real(wp), intent(out) :: state ( 6 )  !! State of the object described by EQEL.

      real(wp) :: a
      real(wp) :: b
      real(wp) :: can
      real(wp) :: cf
      real(wp) :: cn
      real(wp) :: dt
      real(wp) :: di
      real(wp) :: ecc
      real(wp) :: nodedt
      real(wp) :: node
      real(wp) :: dlpdt
      real(wp) :: dlp
      real(wp) :: mldt

      real(wp) :: trans(3,3)
      real(wp) :: sa
      real(wp) :: ca
      real(wp) :: sd
      real(wp) :: cd

      real(wp) :: dx
      real(wp) :: dx1
      real(wp) :: dy
      real(wp) :: dy1
      real(wp) :: p

      real(wp) :: q
      real(wp) :: r
      real(wp) :: ra
      real(wp) :: prate
      real(wp) :: rb
      real(wp) :: san
      real(wp) :: sf
      real(wp) :: sn
      real(wp) :: temp ( 3  )

      real(wp) :: x1,y1
      real(wp) :: h
      real(wp) :: k

      real(wp) :: l
      real(wp) :: ml
      real(wp) :: nfac
      real(wp) :: eecan
      real(wp) :: xhold(6)
      real(wp) :: vf   (3)
      real(wp) :: vg   (3)

!
!     Constants
!
      real(wp), parameter :: pi2 = twopi

!
!     Standard SPICE exception handling code.
!
      if ( ctx%failed() ) return

!
!     Take care of the various errors that can arise with the
!     input elements.
!
      if ( eqel(1) <= 0.0_wp ) then

         call ctx%setmsg ( 'The semi-major axis supplied to EQNCPV ' &
         //            'was non-positive. The value is required ' &
         //            'to be positive by this routine. The ' &
         //            'value supplied was #. ' )

         call ctx%errdp  ( '#', eqel(1) )
         call ctx%sigerr ( 'SPICE(BADSEMIAXIS)'  )
         return

      end if

      ecc = dsqrt ( eqel(2)*eqel(2) + eqel(3)*eqel(3) )

      if ( ecc > 0.9_wp ) then

         call ctx%setmsg ( 'The routine EQNCPV can reliably evaluate ' &
         //            'states from equinoctial elements if the ' &
         //            'eccentricity of the orbit associated ' &
         //            'with the elements is less than 0.9.  The ' &
         //            'eccentricity associated with the ' &
         //            'elements supplies is #.  The values of H ' &
         //            'and K are: # and # respectively. ' )

         call ctx%errdp  ( '#', ecc     )
         call ctx%errdp  ( '#', eqel(2) )
         call ctx%errdp  ( '#', eqel(3) )
         call ctx%sigerr ( 'SPICE(ECCOUTOFRANGE)'  )
         return

      end if

!
!     Form the transformation from planetary equator to the inertial
!     reference frame.
!
      sa          =  dsin(rapol)
      ca          =  dcos(rapol)

      sd          =  dsin(decpol)
      cd          =  dcos(decpol)

      trans(1,1)  =  -sa
      trans(1,2)  =  -ca*sd
      trans(1,3)  =   ca*cd

      trans(2,1)  =   ca
      trans(2,2)  =  -sa*sd
      trans(2,3)  =   sa*cd

      trans(3,1)  =   0.0_wp
      trans(3,2)  =   cd
      trans(3,3)  =   sd
!
!     Compute the offset of the input epoch (ET) from the
!     epoch of the elements.
!
      dt     =  et - epoch

!
!     Obtain the elements, rates, and other parameters. First get
!     the semi-major axis.
!
      a     = eqel(1)

!
!     Recall that H and K at the epoch of the elements are in
!     EQEL(2) and EQEL(3) respectively.
!
!        H_0 = E*Sin(ARGP_0 + NODE_0 )
!        K_0 = E*Cos(ARGP_0 + NODE_0 )
!
!     The values of H and K at the epoch of interest is
!
!        H_dt = E*Sin(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt )
!        K_dt = E*Cos(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt )
!
!     But using the identities Sin(A+B) = Sin(A)Cos(B) + Sin(B)Cos(A)
!                              Cos(A+B) = Cos(A)Cos(B) - Sin(A)Sin(B)
!
!     We can re-write the expression for H_dt and K_dt as
!
!        H_dt = E*Sin(ARGP_0 + NODE_0 )Cos(dt*d(ARGP+NODE)/dt )
!             + E*Cos(ARGP_0 + NODE_0 )Sin(dt*d(ARGP+NODE)/dt )
!
!             = H_0 * Cos(dt*d(ARGP+NODE)/dt )
!             + K_0 * Sin(dt*d(ARGP+NODE)/dt )
!     and
!
!        K_dt = E*Cos(ARGP_0 + NODE_0)Cos(dt*d(ARGP+NODE)/dt)
!             - E*Sin(ARGP_0 + NODE_0)Sin(dt*d(ARGP+NODE)/dt)
!
!             = K_0 * Cos(dt*d(ARGP+NODE)/dt)
!             - H_0 * Sin(dt*d(ARGP+NODE)/dt)
!
!     Thus we can easily compute H and K at the current epoch.
!     Recall that the derivative of the longitude of periapse is
!     in entry 7 of EQEL.
!
      dlpdt = eqel(7)
      dlp   = dt*dlpdt

      can   = dcos( dlp )
      san   = dsin( dlp )

      h     = eqel(2)*can + eqel(3)*san
      k     = eqel(3)*can - eqel(2)*san

!
!     The mean longitude at epoch is in the 4th element of EQEL.
!
      l     = eqel(4)

!
!     The values for P and Q at epoch are stored in entries 5 and 6
!     of the array EQEL.  Recall that
!
!        P_0 = TAN(INC/2)*SIN(NODE_0)
!        Q_0 = TAN(INC/2)*COS(NODE_0)
!
!     We need P and Q offset from the initial epoch by DT.
!
!        P   = TAN(INC/2)*SIN(NODE_0 + dt*dNODE/dt)
!        Q   = TAN(INC/2)*COS(NODE_0 + dt*dNODE/dt)
!
!     Applying the same identities as we did before we have
!
!        P    = P_0 * Cos( dt*dNODE/dt ) + Q_0 * Sin( dt*dNODE/dt )
!        Q    = Q_0 * Cos( dt*dNODE/dt ) - P_0 * Sin( dt*dNODE/dt )
!
      nodedt = eqel(9)
      node   = dt*nodedt

      cn     = dcos( node )
      sn     = dsin( node )

      p      = eqel(5)*cn + eqel(6)*sn
      q      = eqel(6)*cn - eqel(5)*sn

      mldt  = eqel(8)
!
!     We compute the rate of change of the argument of periapse
!     by taking the difference between the rate of the longitude
!     of periapse and the rate of the node.
!
      prate = dlpdt - nodedt

!
!     Form Broucke's beta parameter
!
      b     = dsqrt( 1.0_wp - h*h - k*k )
      b     = 1.0_wp/( 1.0_wp + b )

!
!     Construct the coordinate axes
!
      di     = 1.0_wp/(1.0_wp + p*p + q*q)

      vf(1)  = (1.0_wp - p*p + q*q) * di
      vf(2)  =  2.0_wp * p   * q    * di
      vf(3)  = -2.0_wp * p          * di

      vg(1)  =  2.0_wp * p * q      * di
      vg(2)  = (1.0_wp + p*p - q*q) * di
      vg(3)  =  2.0_wp * q          * di

!
!     Compute the mean longitude
!
      ml  =  l + dmod( mldt*dt, pi2 )

!
!     Obtain the eccentric longitude from Kepler's equation
!
      eecan  =  kepleq( ctx, ml, h, k )

!
!     Trigonometric functions of the eccentric longitude
!
      sf = dsin(eecan)
      cf = dcos(eecan)

!
!     Position in the orbit plane
!
      x1 = a*( (1.0_wp-(b*h**2))*cf + (h*k*b*sf - k) )
      y1 = a*( (1.0_wp-(b*k**2))*sf + (h*k*b*cf - h) )

!
!     Radial distance and functions of the radial distance
!
      rb = h*sf + k*cf
      r  = a*(1.0_wp-rb)
      ra = (mldt*a*a)/r
!
!     Velocity in the orbit plane
!
      dx1 = ra * ( -sf + h*b*rb )
      dy1 = ra * (  cf - k*b*rb )

!
!     Correction factor for periapsis rate
!
      nfac = 1.0_wp - ( dlpdt/mldt )

!
!     Include precession in velocity
!
      dx  = nfac*dx1 - prate*y1
      dy  = nfac*dy1 + prate*x1

!
!     Form the planetary mean equator position vector
!
      xhold(1:3) = x1*vf + y1*vg

!
!     Form the planetary mean equator velocity vector
!
      temp (1) = -nodedt*xhold(2)
      temp (2) =  nodedt*xhold(1)
      temp (3) =  0.0_wp

      xhold(4:6) = 1.0_wp*temp + dx*vf + dy*vg

!
!     Transform to an inertial state vector
!
      call mxv ( trans, xhold(1), state(1) )
      call mxv ( trans, xhold(4), state(4) )

      return
      end subroutine eqncpv

      !> Scale a point so that it lies on the surface of a specified
      !> triaxial ellipsoid that is centered at the origin and aligned
      !> with the Cartesian coordinate axes.
      !>
      !>## Detailed Input
      !>
      !>     P        is a non-zero point in three-dimensional space.
      !>
      !>     A,
      !>     B,
      !>     C        are, respectively, the semi-axis lengths of a triaxial
      !>              ellipsoid in the X, Y, and Z directions. The axes of
      !>              the ellipsoid are aligned with the axes of the
      !>              Cartesian coordinate system.
      !>
      !>## Detailed Output
      !>
      !>     EP       is the result of scaling the input point P so that
      !>              it lies on the surface of the triaxial ellipsoid
      !>              defined by the input semi-axis lengths.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the target ellipsoid's semi-axis lengths is
      !>         non-positive, the error SPICE(INVALIDAXES) is signaled.
      !>
      !>     2)  If P is the zero vector, the error SPICE(ZEROVECTOR) is
      !>         signaled.
      !>
      !>     3)  If the level surface parameter of the input point
      !>         underflows, the error SPICE(POINTTOOSMALL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine efficiently computes the ellipsoid surface point
      !>     corresponding to a specified ray emanating from the origin.
      !>     Practical examples of this computation occur in the SPICELIB
      !>     routines LATSRF and SRFREC.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the surface intercept point on an ellipsoid having radii
      !>
      !>            ( 3, 2, 1 )
      !>
      !>        of the ray emanating from the origin and having direction
      !>        vector
      !>
      !>            ( 1, 1, 1 )
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EDPNT_EX1
      !>      IMPLICIT NONE
      !>
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,F18.14)'  )
      !>
      !>      CHARACTER*(*)         FMT3
      !>      PARAMETER           ( FMT3 = '(A,3F18.14)' )
      !>
      !>      DOUBLE PRECISION      A
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      C
      !>      DOUBLE PRECISION      V      ( 3 )
      !>      DOUBLE PRECISION      EP     ( 3 )
      !>      DOUBLE PRECISION      LEVEL
      !>
      !>      A = 3.D0
      !>      B = 2.D0
      !>      C = 1.D0
      !>
      !>      CALL VPACK ( 1.D0, 1.D0, 1.D0, V )
      !>
      !>      CALL EDPNT ( V, A, B, C, EP )
      !>
      !>      WRITE (*,FMT3) 'EP    = ', EP
      !>
      !>C
      !>C     Verify that EP is on the ellipsoid.
      !>C
      !>      LEVEL = (EP(1)/A)**2 + (EP(2)/B)**2 + (EP(3)/C)**2
      !>
      !>      WRITE (*,FMT1) 'LEVEL = ', LEVEL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        EP    =   0.85714285714286  0.85714285714286  0.85714285714286
      !>        LEVEL =   1.00000000000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 09-JUL-2020 (JDR)
      !>
      !>        Minor edits to the header and code example.
      !>
      !>-    SPICELIB Version 2.0.0, 19-APR-2016 (NJB) (EDW)
      module subroutine edpnt ( ctx, p, a, b, c, ep )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: p  ( 3 )  !! A point in three-dimensional space.
      real(wp), intent(in) :: a  !! Semi-axis length in the X direction.
      real(wp), intent(in) :: b  !! Semi-axis length in the Y direction.
      real(wp), intent(in) :: c  !! Semi-axis length in the Z direction.
      real(wp), intent(out) :: ep ( 3 )  !! Point on ellipsoid.

      real(wp) :: level
      real(wp) :: sq

      if ( ctx%failed() ) return

      if (      ( a <= 0.0_wp ) &
           .or. ( b <= 0.0_wp ) &
           .or. ( c <= 0.0_wp ) ) then

         call ctx%setmsg ( 'Ellipsoid radii must be strictly ' &
         //            'positive but are (#, #, #).'      )
         call ctx%errdp  ( '#', a                             )
         call ctx%errdp  ( '#', b                             )
         call ctx%errdp  ( '#', c                             )
         call ctx%sigerr ( 'SPICE(INVALIDRADII)'              )
         return

      end if

!
!     The input point must be non-zero, or we can't scale it
!     to the ellipsoid.
!
      if ( all(p == 0.0_wp) ) then

         call ctx%setmsg ( 'Input point was the zero vector. ' &
         //            'A non-zero vector is required.'   )
         call ctx%sigerr ( 'SPICE(ZEROVECTOR)'                )
         return

      end if

!
!     Find the level surface parameter of the input point with respect
!     to the scaled ellipsoid.
!
      level = (( p(1)/a )**2 + ( p(2)/b )**2 + ( p(3)/c )**2)

      if ( level <= 0.0_wp ) then
!
!        We expect that LEVEL will be non-negative, but it could
!        be zero. We check for negative values as a precaution.
!
         call ctx%setmsg ( 'Input point''s level surface parameter ' &
         //            'was non-positive. The point is too close ' &
         //            'to the origin to be scaled to the ' &
         //            'ellipsoid. The point was (#, #, #).'      )
         call ctx%errdp  ( '#', p(1)                                  )
         call ctx%errdp  ( '#', p(2)                                  )
         call ctx%errdp  ( '#', p(3)                                  )
         call ctx%sigerr ( 'SPICE(POINTTOOSMALL)'                     )
         return

      end if

!
!     Scale the point to one for which the level surface parameter is 1.
!
      sq    = sqrt(level)

      ep(1) = p(1) / sq
      ep(2) = p(2) / sq
      ep(3) = p(3) / sq

      return
      end subroutine edpnt

      !> Return the unique point on an ellipsoid's surface where the
      !> outward normal direction is a given vector.
      !>
      !>## Detailed Input
      !>
      !>     A        is the length of the semi-axis of the ellipsoid
      !>              that is parallel to the X-axis of the body-fixed
      !>              coordinate system.
      !>
      !>     B        is the length of the semi-axis of the ellipsoid
      !>              that is parallel to the Y-axis of the body-fixed
      !>              coordinate system.
      !>
      !>     C        is the length of the semi-axis of the ellipsoid
      !>              that is parallel to the Z-axis of the body-fixed
      !>              coordinate system.
      !>
      !>     NORMAL   is a non-zero vector. The unique point on the
      !>              ellipsoid at which NORMAL is an outward normal vector
      !>              is sought.
      !>
      !>## Detailed Output
      !>
      !>     POINT    is the unique point on the ellipsoid at which NORMAL
      !>              is an outward normal vector.
      !>
      !>              POINT is a 3-vector giving the body-fixed coordinates
      !>              of a point on the ellipsoid. In body-fixed
      !>              coordinates, the semi-axes of the ellipsoid are
      !>              aligned with the X, Y, and Z-axes of the coordinate
      !>              system.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the semi-axis lengths is non-positive, the error
      !>         SPICE(BADAXISLENGTH) is signaled.
      !>
      !>     2)  If any of the semi-axis lengths underflows to zero when
      !>         divided by the largest semi-axis length, the error
      !>         SPICE(AXISUNDERFLOW) is signaled.
      !>
      !>     3)  If NORMAL is the zero vector, the error SPICE(ZEROVECTOR)
      !>         is signaled.
      !>
      !>     4)  If the input pass the above checks but lead to a
      !>         divide-by-zero error or to an computing an invalid argument
      !>         of a fractional exponential expression, the error
      !>         SPICE(DEGENERATECASE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine can be used to determine the distance between an
      !>     ellipsoid and a non-intersecting plane. This distance computation
      !>     supports computation of terminator points on an ellipsoid.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Choose a triaxial ellipsoid with three unequal semi-axis
      !>        lengths. Pick several vectors; find the points on the
      !>        ellipsoid where the respective outward normals are parallel to
      !>        those vectors.
      !>
      !>        Check the results: at each point, a computed outward normal
      !>        vector should have very small angular separation from the
      !>        input vector. Also, the point should be on the surface of the
      !>        ellipsoid. The ellipsoid can be thought of as a level surface
      !>        of the function
      !>
      !>                             2        2         2
      !>           f(x, y, z) = (x/A)  + (y/B)  +  (z/C)
      !>
      !>        where A, B, C are the semi-axis lengths of the ellipsoid.
      !>        Specifically, the ellipsoid is the set
      !>
      !>           { (x, y, z) : f(x, y, z)  =  1 }
      !>
      !>        We can evaluate F at a point to determine whether that point
      !>        is close to the ellipsoid's surface.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EDNMPT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      VSEP
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,F14.8)'  )
      !>
      !>      CHARACTER*(*)         FMT3
      !>      PARAMETER           ( FMT3 = '(A,3F14.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      A
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      C
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      POINT  ( 3 )
      !>      DOUBLE PRECISION      XNORML ( 3 )
      !>
      !>C
      !>C     Initialize the ellipsoid semi-axes.
      !>C
      !>      A = 10.D0
      !>      B =  5.D0
      !>      C =  2.D0
      !>
      !>C
      !>C     Pick several vectors; find the points
      !>C     on the ellipsoid where the respective
      !>C     outward normals are parallel to those
      !>C     vectors; check the results.
      !>C
      !>      CALL VPACK  ( 0.D0, 0.D0, 3.D0, XNORML )
      !>      CALL EDNMPT ( A,    B,    C,    XNORML, POINT  )
      !>      CALL SURFNM ( A,    B,    C,    POINT,  NORMAL )
      !>
      !>      WRITE (*,*   ) ' '
      !>      WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
      !>      WRITE (*,FMT3) 'Input vector:        ', XNORML
      !>      WRITE (*,FMT3) 'Point:               ', POINT
      !>      WRITE (*,FMT3) 'Outward normal:      ', NORMAL
      !>      WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
      !>     .                                          XNORML )
      !>      WRITE (*,FMT1) 'Off-surface error:   ',
      !>     .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
      !>     .               + (POINT(3)/C)**2 - 1
      !>      WRITE (*,*) ' '
      !>
      !>      CALL VPACK  ( 15.D0, -7.D0, 3.D0, XNORML )
      !>      CALL EDNMPT ( A,      B,    C,    XNORML, POINT  )
      !>      CALL SURFNM ( A,      B,    C,    POINT,  NORMAL )
      !>
      !>      WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
      !>      WRITE (*,FMT3) 'Input vector:        ', XNORML
      !>      WRITE (*,FMT3) 'Point:               ', POINT
      !>      WRITE (*,FMT3) 'Outward normal:      ', NORMAL
      !>      WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
      !>     .                                             XNORML )
      !>      WRITE (*,FMT1) 'Off-surface error:   ',
      !>     .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
      !>     .               + (POINT(3)/C)**2 - 1
      !>      WRITE (*,*) ' '
      !>
      !>      CALL VPACK  ( 15.D0, -7.D0, 3.D0, XNORML )
      !>      CALL EDNMPT ( A,      B,    C,    XNORML, POINT  )
      !>      CALL SURFNM ( A,      B,    C,    POINT,  NORMAL )
      !>
      !>      WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
      !>      WRITE (*,FMT3) 'Input vector:        ', XNORML
      !>      WRITE (*,FMT3) 'Point:               ', POINT
      !>      WRITE (*,FMT3) 'Outward normal:      ', NORMAL
      !>      WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
      !>     .                                             XNORML )
      !>      WRITE (*,FMT1) 'Off-surface error:   ',
      !>     .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
      !>     .               + (POINT(3)/C)**2 - 1
      !>      WRITE (*,*) ' '
      !>
      !>      CALL VPACK  ( A/2, B/2,  C/2, XNORML )
      !>      CALL EDNMPT ( A,   B,    C,   XNORML, POINT  )
      !>      CALL SURFNM ( A,   B,    C,   POINT,  NORMAL )
      !>
      !>      WRITE (*,FMT3) 'Semi-axis lengths:   ', A, B, C
      !>      WRITE (*,FMT3) 'Input vector:        ', XNORML
      !>      WRITE (*,FMT3) 'Point:               ', POINT
      !>      WRITE (*,FMT3) 'Outward normal:      ', NORMAL
      !>      WRITE (*,FMT1) 'Angular error (rad): ', VSEP(NORMAL,
      !>     .                                             XNORML )
      !>      WRITE (*,FMT1) 'Off-surface error:   ',
      !>     .                 (POINT(1)/A)**2 + (POINT(2)/B)**2
      !>     .               + (POINT(3)/C)**2 - 1
      !>      WRITE (*,*) ' '
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      !>        Input vector:            0.00000000    0.00000000    3.00000000
      !>        Point:                   0.00000000    0.00000000    2.00000000
      !>        Outward normal:          0.00000000    0.00000000    1.00000000
      !>        Angular error (rad):     0.00000000
      !>        Off-surface error:       0.00000000
      !>
      !>        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      !>        Input vector:           15.00000000   -7.00000000    3.00000000
      !>        Point:                   9.73103203   -1.13528707    0.07784826
      !>        Outward normal:          0.89165745   -0.41610681    0.17833149
      !>        Angular error (rad):     0.00000000
      !>        Off-surface error:       0.00000000
      !>
      !>        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      !>        Input vector:           15.00000000   -7.00000000    3.00000000
      !>        Point:                   9.73103203   -1.13528707    0.07784826
      !>        Outward normal:          0.89165745   -0.41610681    0.17833149
      !>        Angular error (rad):     0.00000000
      !>        Off-surface error:       0.00000000
      !>
      !>        Semi-axis lengths:      10.00000000    5.00000000    2.00000000
      !>        Input vector:            5.00000000    2.50000000    1.00000000
      !>        Point:                   9.69412864    1.21176608    0.07755303
      !>        Outward normal:          0.88045091    0.44022545    0.17609018
      !>        Angular error (rad):     0.00000000
      !>        Off-surface error:       0.00000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Modified the output format FMT1 in the code example for the
      !>        output to fit in the $Examples section without modifications.
      !>
      !>-    SPICELIB Version 1.0.0, 17-MAY-2016 (NJB) (WLT)
      module subroutine ednmpt ( ctx, a, b, c, normal, point )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: a  !! Length of the ellipsoid semi-axis along the X-axis.
      real(wp), intent(in) :: b  !! Length of the ellipsoid semi-axis along the Y-axis.
      real(wp), intent(in) :: c  !! Length of the ellipsoid semi-axis along the Z-axis.
      real(wp), intent(in) :: normal ( 3 )  !! Outward normal direction.
      real(wp), intent(out) :: point  ( 3 )  !! Point where outward normal is parallel to NORMAL.

      real(wp) :: arg
      real(wp) :: lambda
      real(wp) :: na2
      real(wp) :: nb2
      real(wp) :: nc2
      real(wp) :: sa
      real(wp) :: sb
      real(wp) :: sc
      real(wp) :: scale

!
! We will test RETURN though, since
!     we could have invalid inputs to our arithmetic computations
!     if a SPICE error condition exists upon entry.
!
      if ( ctx%failed() ) return

!
!     Perform some preliminary checks.
!
!     We need a non-degenerate ellipsoid to start with.
!
      if (      ( a <= 0.0_wp ) &
           .or. ( b <= 0.0_wp ) &
           .or. ( c <= 0.0_wp )  ) then

         call ctx%setmsg ( 'All ellipsoid semi-axis lengths must ' &
         //            'be strictly positive. Lengths were: ' &
         //            'A = #; B = #; C = #'                  )
         call ctx%errdp  ( '#',  a                                )
         call ctx%errdp  ( '#',  b                                )
         call ctx%errdp  ( '#',  c                                )
         call ctx%sigerr ( 'SPICE(BADAXISLENGTH)'                 )
         return

      end if

!
!     We'll work with scaled copies of the input semi-axis
!     lengths.
!
      scale = max ( a, b, c )

      sa    = (a / scale)
      sb    = (b / scale)
      sc    = (c / scale)
!
!     If any of the scaled-semi axes underflowed to zero,
!     we can't continue.
!
      if (      ( sa <= 0.0_wp ) &
           .or. ( sb <= 0.0_wp ) &
           .or. ( sc <= 0.0_wp )  ) then

         call ctx%setmsg ( 'Scaled semi-axis lengths must be ' &
         //            'strictly positive. Scaled lengths were: ' &
         //            'SA = #; SB = #; SC = #'                   )
         call ctx%errdp  ( '#',  sa                                   )
         call ctx%errdp  ( '#',  sb                                   )
         call ctx%errdp  ( '#',  sc                                   )
         call ctx%sigerr ( 'SPICE(AXISUNDERFLOW)'                     )
         return

      end if

!
!     The normal vector can't be the zero vector.
!
      if ( all(normal == 0.0_wp) ) then

         call ctx%setmsg ( 'The input normal vector was the zero vector. ' &
         //            'There is no solution.'                        )
         call ctx%sigerr ( 'SPICE(ZEROVECTOR)'                            )
         return

      end if

!
!     In general, given an ellipsoid E with axes a, b, c,
!     and given a normal vector
!
!        n = ( n1, n2, n3 )
!
!     this problem is equivalent to finding a point (x,y,z)
!     on E such that
!
!             2     2     2
!        ( x/a,  y/b,  z/c ) = lambda * n
!
!     which implies
!                                           2     2     2
!        ( x, y, z )         = lambda ( n1*a, n2*b, n3*c )
!
!     Then since the vector on the left side is on the surface
!     of E, we must have
!
!              2       2   2        2   2       2   2
!        lambda  ( ( n1 * a  ) + (n2 * b ) + (n3 + c ) ) = 1
!
!     Requiring lambda to be positive, we have
!
!                               2   2        2   2       2   2
!        lambda = 1 / sqrt( ( n1 * a  ) + (n2 * b ) + (n3 + c ) )
!
      na2 = normal(1) * sa * sa
      nb2 = normal(2) * sb * sb
      nc2 = normal(3) * sc * sc

      arg = (( na2 * normal(1) ) + ( nb2 * normal(2) )  + ( nc2 * normal(3) ))

      if ( arg <= 0.0_wp ) then

         call ctx%setmsg ( 'Scale factor LAMBDA must be positive, ' &
         //            'but reciprocal of square of LAMBDA is #.' )
         call ctx%errdp  ( '#', arg                                   )
         call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                    )
         return

      end if

!
!     Compute LAMBDA as above, and scale it too. This will place
!     POINT on the original ellipsoid.
!
      lambda   = ( arg ** (-0.5_wp) ) * scale

      point(1) = lambda * na2
      point(2) = lambda * nb2
      point(3) = lambda * nc2

      end subroutine ednmpt

      !> Solve the time of flight equation MA = E - e sin(E) for the
      !> elliptic eccentric anomaly E, given mean anomaly the MA and
      !> the eccentricity ECC.
      !>
      !>## Detailed Input
      !>
      !>     MA       is the elliptic mean anomaly of an orbiting body at
      !>              some epoch t,
      !>
      !>                                3 1/2
      !>                 MA = (t-T)(mu/a )
      !>
      !>              where T is the time of periapsis passage, a is
      !>              the semi-major axis of the orbit, and mu is the
      !>              gravitational parameter of the primary body.
      !>
      !>     ECC      is the eccentricity of the orbit.
      !>
      !>## Detailed Output
      !>
      !>     E        is the corresponding eccentric anomaly. This is the
      !>              solution to the time of flight equation
      !>
      !>                 MA = E - e sin(E)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the eccentricity (ECC) is outside the range [0,1),
      !>         the error SPICE(WRONGCONIC) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Iterate to solve
      !>
      !>        f(E,MA,e) = E - e sin(E) - MA = 0
      !>
      !>## Examples
      !>
      !>     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
      !>          Astrodynamics," Dover Publications Inc., 1971.
      !>
      !>     [2]  E. W. Ng, "A General Algorithm for the Solution of Kepler's
      !>          Equation for Elliptic Orbits", Cel. Mech. 20, pp.243-249,
      !>          1979.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
      !>
      !>        The declaration for the SPICELIB function PI is now
      !>        preceded by an EXTERNAL statement declaring PI to be an
      !>        external function. This removes a conflict with any
      !>        compilers that have a PI intrinsic function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
      !>
      !>        The routine now verifies that the eccentricity is in the
      !>        proper range---[0,1)---before proceeding.
      module subroutine elltof ( ctx, ma, ecc, e )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: halfpi, pi, twopi
      use pepper_math, only: dcbrt
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: ma  !! Mean anomaly at epoch.
      real(wp), intent(in) :: ecc  !! Eccentricity.
      real(wp), intent(out) :: e  !! Elliptic eccentric anomaly.

      real(wp), parameter :: tol = 1.0e-15_wp

      real(wp) :: m
      real(wp) :: mprime
      integer(ip) :: n

      real(wp) :: m0
      real(wp) :: a
      real(wp) :: b
      real(wp) :: q
      real(wp) :: r
      real(wp) :: qr
      real(wp) :: y

      real(wp) :: fn
      real(wp) :: deriv
      real(wp) :: deriv2
      real(wp) :: change

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      if ( ecc < 0.0_wp  .or.  ecc >= 1.0_wp ) then
         call ctx%sigerr ( 'SPICE(WRONGCONIC)' )
         return
      end if

!
!     For reasons of numerical stability, we would like to restrict
!     our solution to the interval [0,pi]. Because E, M, and sin E
!     are always positive or negative together, we can pretend that M
!     is positive and adjust the sign of the result. And for M, E > pi,
!     we can define
!
!           M = 2n pi + M'     and    E = 2n pi + E'
!
!     where M' and E' are in the interval [-pi,pi]. Solving for E'
!     gives us E.
!
!     So, we begin by reducing the input mean anomaly to [0,pi].
!
      m = abs ( ma )

      if ( m > pi ) then
         n      = int ( (m-pi) / twopi ) + 1
         mprime = m - n * twopi
      else
         n      = 0
         mprime = m
      end if

      m = abs ( mprime )

!
!     The convergence of the iterative scheme below depends on a good
!     initial estimate for E.
!
!     For small eccentricity, the initial estimate E = M is sufficient.
!     However, as the eccentricity increases, so does the number of
!     iterations required for convergence. For sufficiently large
!     eccentricity, this estimate leads to divergence.
!
!     Ng [2] notes that the function y(M,e)
!
!           E - M
!          -------  =  sin(e y + M)
!             e
!
!     increases and decreases monotonically when M is in the ranges
!     [0,M0] and [m0,pi], respectively.
!
!     When M0 < M < pi, where M0 = (pi/2) - e, the cubic
!             -   -
!
!                              pi - M  2        pi - M    pi - M
!           B(M,e) = 1 - (1 -  -------)  (1 + 2 ------- - -------)
!                              pi - M0          pi - M0    1 + e
!
!     provides a good initial estimate of y for all values of e.
!
      m0 = halfpi - ecc

      if ( m >= m0 ) then

         a = pi - m
         b = pi - m0

         y = 1.0_wp - (1.0_wp - a/b)**2 * (1.0_wp + 2.0_wp*a/b - a/(1.0_wp+ecc))
         e = ecc * sin(ecc*y + m) + m

!
!     The situation is a little more troublesome, however, when M < M0.
!     For small eccentricity, the cubic
!
!                                  2
!           A(M,e) = 1 - (1 - M/M0)  (1 + 2M/M0 - M/(1-e) )
!
!     gives a reasonable first estimate of y. However, as e -> 1,
!     successive approximations of the form
!
!                             k           k
!           C (M,e) = 1 - (-1)  (1 - M/M0)
!            k
!
!     are used, where k = 4 for e > 0.7, and k = 8 for e > 0.85.
!
!     For high eccentricity (e > 0.96) and low mean anomaly (M < 0.05),
!     these successive approximations eventually fail. Fortunately, in
!     just these cases, the cubic
!
!                           3    2  1/3           3    2  1/3
!           D(M,e) = [r + (q  + r )]     + [r - (q  + r )]
!
!     where
!
!           r = 3M/e,   q = (2/e)(1 - e)
!
!     provides a reasonable estimate of E directly.
!
      else if ( ecc <= 0.7_wp ) then

         y = 1.0_wp - (1.0_wp - m/m0)**2 * (1.0_wp + 2.0_wp*m/m0 - m/(1.0_wp-ecc))
         e = ecc * sin(ecc*y + m) + m

      else if ( ecc <= 0.85_wp ) then

         y = 1.0_wp - (1.0_wp - m/m0)**4
         e = ecc * sin(ecc*y + m) + m

      else if ( ecc <= 0.96_wp .or. m > 0.05_wp ) then

         y = 1.0_wp - (1.0_wp - m/m0)**8
         e = ecc * sin(ecc*y + m) + m

      else

         q  = (2.0_wp/ecc) * (1.0_wp - ecc)
         r  = 3.0_wp * (m/ecc)
         qr = sqrt ( q**3 + r**2 )

         e  = dcbrt ( r + qr ) + dcbrt ( r - qr )

      end if

!
!     Use the Newton second-order method,
!
!                                           2
!          E    = E  - (f/f')*(1 + f*f''/2f' )
!           i+1    i
!
!     where
!
!          f   = E - e sin(E) - M
!          f'  = 1 - e cos(E)
!          f'' =     e sin(E)
!
      change = 1.0_wp

      do while ( abs ( change ) > tol )

         fn     = e    - ecc * sin(e) - m
         deriv  = 1.0_wp - ecc * cos(e)
         deriv2 =        ecc * sin(e)

         change = (fn/deriv) * ( 1.0_wp + (fn*deriv2) / (2.0_wp*deriv**2) )
         e      = e - change

      end do

!
!     "Unwrap" E' into the actual value of E.
!
      if ( mprime < 0 ) then
         e = -e
      end if

      if ( n > 0 ) then
         e = e + n * twopi
      end if

      if ( ma < 0 ) then
         e = -e
      end if

      return
      end subroutine elltof

      !> Evaluate, at a specified point, a Hermite interpolating polynomial
      !> for a specified set of equally spaced abscissa values and
      !> corresponding pairs of function and function derivative values.
      !>
      !>## Detailed Input
      !>
      !>     N        is the number of points defining the polynomial.
      !>              The array YVALS contains 2*N elements.
      !>
      !>     FIRST,
      !>     STEP     are, respectively, a starting abscissa value and a
      !>              step size that define the set of abscissa values
      !>
      !>                 FIRST   +   (I-1) * STEP,     I = 1, ..., N
      !>
      !>              STEP must be non-zero.
      !>
      !>     YVALS    is an array of length 2*N containing ordinate and
      !>              derivative values for each point in the domain
      !>              defined by FIRST, STEP, and N. The elements
      !>
      !>                 YVALS( 2*I - 1 )
      !>                 YVALS( 2*I     )
      !>
      !>              give the value and first derivative of the output
      !>              polynomial at the abscissa value
      !>
      !>                 FIRST   +   (I-1) * STEP
      !>
      !>              where I ranges from 1 to N.
      !>
      !>     WORK     is a work space array. It is used by this routine
      !>              as a scratch area to hold intermediate results.
      !>
      !>     X        is the abscissa value at which the interpolating
      !>              polynomial and its derivative are to be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     F,
      !>     DF       are the value and derivative at X of the unique
      !>              polynomial of degree 2*N-1 that fits the points and
      !>              derivatives defined by FIRST, STEP, and YVALS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If STEP is zero, the error SPICE(INVALIDSTEPSIZE) is
      !>         signaled.
      !>
      !>     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
      !>         signaled.
      !>
      !>     3)  This routine does not attempt to ward off or diagnose
      !>         arithmetic overflows.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Users of this routine must choose the number of points to use
      !>     in their interpolation method. The authors of Reference [1] have
      !>     this to say on the topic:
      !>
      !>        Unless there is solid evidence that the interpolating function
      !>        is close in form to the true function f, it is a good idea to
      !>        be cautious about high-order interpolation. We
      !>        enthusiastically endorse interpolations with 3 or 4 points, we
      !>        are perhaps tolerant of 5 or 6; but we rarely go higher than
      !>        that unless there is quite rigorous monitoring of estimated
      !>        errors.
      !>
      !>     The same authors offer this warning on the use of the
      !>     interpolating function for extrapolation:
      !>
      !>        ...the dangers of extrapolation cannot be overemphasized:
      !>        An interpolating function, which is perforce an extrapolating
      !>        function, will typically go berserk when the argument x is
      !>        outside the range of tabulated values by more than the typical
      !>        spacing of tabulated points.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Fit a 7th degree polynomial through the points ( x, y, y' )
      !>
      !>           ( -1,      6,       3 )
      !>           (  1,      8,      11 )
      !>           (  3,   2210,    5115 )
      !>           (  5,  78180,  109395 )
      !>
      !>        and evaluate this polynomial at x = 2.
      !>
      !>        The returned value of ANSWER should be 141.D0, and the
      !>        returned derivative value should be 456.D0, since the unique
      !>        7th degree polynomial that fits these constraints is
      !>
      !>                     7       2
      !>           f(x)  =  x   +  2x  + 5
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>       PROGRAM HRMESP_EX1
      !>       IMPLICIT NONE
      !>
      !>       DOUBLE PRECISION      ANSWER
      !>       DOUBLE PRECISION      DERIV
      !>       DOUBLE PRECISION      FIRST
      !>       DOUBLE PRECISION      STEP
      !>       DOUBLE PRECISION      YVALS (8)
      !>       DOUBLE PRECISION      WORK  (8,2)
      !>       INTEGER               N
      !>
      !>       N         =       4
      !>
      !>       YVALS(1)  =       6.D0
      !>       YVALS(2)  =       3.D0
      !>       YVALS(3)  =       8.D0
      !>       YVALS(4)  =      11.D0
      !>       YVALS(5)  =    2210.D0
      !>       YVALS(6)  =    5115.D0
      !>       YVALS(7)  =   78180.D0
      !>       YVALS(8)  =  109395.D0
      !>
      !>       FIRST     =  -1.D0
      !>       STEP      =   2.D0
      !>
      !>       CALL HRMESP ( N,    FIRST, STEP,   YVALS,
      !>      .              2.D0, WORK,  ANSWER, DERIV )
      !>
      !>       WRITE (*,*) 'ANSWER = ', ANSWER
      !>       WRITE (*,*) 'DERIV  = ', DERIV
      !>       END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         ANSWER =    141.00000000000000
      !>         DERIV  =    456.00000000000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapters 3.0 and 3.1, Cambridge University Press, 1986.
      !>
      !>     [2]  S. Conte and C. de Boor, "Elementary Numerical Analysis -- An
      !>          Algorithmic Approach," 3rd Edition, p 64, McGraw-Hill, 1980.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.2, 01-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard. Added code
      !>        example's solution.
      !>
      !>        Fixed formula in the description of YVALS argument in
      !>        $Detailed_Input.
      !>
      !>-    SPICELIB Version 1.2.1, 28-JAN-2014 (NJB)
      !>
      !>        Fixed a few comment typos.
      !>
      !>-    SPICELIB Version 1.2.0, 31-JAN-2002 (EDW)
      !>
      !>        Added the use of DBLE to convert integer values
      !>        used in DOUBLE PRECISION calculations.
      !>
      !>-    SPICELIB Version 1.1.0, 28-DEC-2001 (NJB)
      !>
      !>        Blanks following final newline were truncated to
      !>        suppress compilation warnings on the SGI-N32 platform.
      !>
      !>-    SPICELIB Version 1.0.0, 01-MAR-2000 (NJB)
      module subroutine hrmesp ( ctx, n, first, step, yvals, x, work, f, df )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
      real(wp), intent(in) :: first  !! First abscissa value.
      real(wp), intent(in) :: step  !! Step size.
      real(wp), intent(in) :: yvals ( 2*n )  !! Ordinate and derivative values.
      real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
      real(wp), intent(inout) :: work  ( 2*n, 2 )  !! Work space array.
      real(wp), intent(out) :: f  !! Interpolated function value at X.
      real(wp), intent(out) :: df  !! Interpolated function's derivative at X.

      real(wp) :: c1
      real(wp) :: c2
      real(wp) :: denom
      real(wp) :: newx
      real(wp) :: temp
      real(wp) :: xi
      real(wp) :: xij

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: next
      integer(ip) :: prev
      integer(ip) :: this

      if ( ctx%failed() ) return

!
!     No data, no interpolation.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'Array size must be positive; was #.' )
         call ctx%errint ( '#',  n                               )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'                  )
         return

      end if

!
!     The step size must be non-zero.
!
      if ( step == 0.0_wp ) then

         call ctx%setmsg ( 'Step size was zero.'    )
         call ctx%sigerr ( 'SPICE(INVALIDSTEPSIZE)' )
         return

      end if

!
!     We can simplify the interpolation problem by shifting
!     and scaling the abscissa values so that they start at 1
!     and are separated by a unit step. All we need to do here is
!     shift and scale X.
!
      newx  =  (  ( x - first ) / step  )  +  1.0_wp

!
!     For consistency with our scaled horizontal axis, we'll have
!     scale our local derivative values by STEP, and scale our final
!     computed derivative by 1/STEP.
!
!     Copy the input array into WORK.  Scale the derivatives at this
!     step. After this, the first column of WORK represents the first
!     column of our triangular interpolation table.
!
      do i = 1, 2*n-1, 2
         work(i,1) = yvals(i)
      end do

      do i = 2, 2*n, 2
         work(i,1) = yvals(i) * step
      end do

!
!     Compute the second column of the interpolation table: this
!     consists of the N-1 values obtained by evaluating the
!     first-degree interpolants at NEWX. We'll also evaluate the
!     derivatives of these interpolants at NEWX and save the results in
!     the second column of WORK. Because the derivative computations
!     depend on the function computations from the previous column in
!     the interpolation table, and because the function interpolation
!     overwrites the previous column of interpolated function values,
!     we must evaluate the derivatives first.
!

      do i = 1, n-1

         c1     = real( (i+1), wp )  - newx
         c2     =  newx  - real(i, wp)

!
!        The second column of WORK contains interpolated derivative
!        values.
!
!        The odd-indexed interpolated derivatives are simply the input
!        derivatives, after scaling.
!
         prev = 2*i  - 1
         this = prev + 1
         next = this + 1

         work( prev,  2 )  =    work( this, 1 )

!
!        The even-indexed interpolated derivatives are the slopes of
!        the linear interpolating polynomials for adjacent input
!        abscissa/ordinate pairs. No scaling is needed here.
!
         work( this, 2 ) = ( work(next, 1) - work(prev, 1) )

!
!        The first column of WORK contains interpolated function values.
!        The odd-indexed entries are the linear Taylor polynomials,
!        each input abscissa value, evaluated at NEWX.
!
         temp  = work(this, 1) * ( newx - real(i, wp) )  +  work(prev, 1)

         work( this, 1 ) = (   c1 * work( prev, 1 ) &
                             + c2 * work( next, 1 )  )

         work( prev, 1 ) = temp

      end do

!
!     The last column entries were not computed by the preceding loop;
!     compute them now.
!
      work ( 2*n - 1,  2 )  =    work( 2*n,     1 )
      work ( 2*n - 1,  1 )  =    work( 2*n,     1 ) * ( newx - n ) &
                               + work( 2*n - 1, 1 )

!
!     Compute columns 3 through 2*N of the table.
!
      do j = 2, 2*n-1

         do i = 1, 2*n-j
!
!           In the theoretical construction of the interpolation table,
!           there are 2*N abscissa values, since each input abscissa
!           value occurs with multiplicity two. In this theoretical
!           construction, the Jth column of the interpolation table
!           contains results of evaluating interpolants that span J+1
!           consecutive abscissa values. The indices XI and XIJ below
!           are used to pick the correct abscissa values out of this
!           sequence of 2*N values.
!
            xi       =   real( ( i     + 1 ) / 2, wp )
            xij      =   real( ( i + j + 1 ) / 2, wp )

            c1       =     xij   -  newx
            c2       =     newx  -  xi

            denom    =     xij   -  xi

!
!           Compute the interpolated derivative at NEWX for the Ith
!           interpolant. This is the derivative with respect to NEWX of
!           the expression for the interpolated function value, which is
!           the second expression below. This derivative computation
!           is done first because it relies on the interpolated function
!           values from the previous column of the interpolation table.
!
!           The derivative expression here corresponds to equation
!           2.35 on page 64 in reference [2].
!
            work(i,2) = (              c1  * work(i,   2) &
                          +            c2  * work(i+1, 2) &
                          + ( work(i+1, 1) - work(i,   1) )  )  / denom
!
!           Compute the interpolated function value at NEWX for the Ith
!           interpolant.
!
            work(i,1) = ( c1 * work(i,1)  +  c2 * work(i+1,1) ) / denom

         end do

      end do

!
!     Our interpolated function value is sitting in WORK(1,1) at this
!     point.  The interpolated derivative is located in WORK(1,2).
!     We must undo the scaling of the derivative. We've already
!     checked that STEP is non-zero.
!
      f   =  work( 1, 1 )
      df  =  work( 1, 2 )  /  step

      return
      end subroutine hrmesp

      !> Evaluate a Hermite interpolating polynomial at a specified
      !> abscissa value.
      !>
      !>## Detailed Input
      !>
      !>     N        is the number of points defining the polynomial.
      !>              The arrays XVALS and YVALS contain N and 2*N
      !>              elements respectively.
      !>
      !>     XVALS    is an array of length N containing abscissa values.
      !>
      !>     YVALS    is an array of length 2*N containing ordinate and
      !>              derivative values for each point in the domain
      !>              defined by XVALS. The elements
      !>
      !>                 YVALS( 2*I - 1 )
      !>                 YVALS( 2*I     )
      !>
      !>              give the value and first derivative of the output
      !>              polynomial at the abscissa value
      !>
      !>                 XVALS(I)
      !>
      !>              where I ranges from 1 to N.
      !>
      !>     WORK     is a work space array. It is used by this routine
      !>              as a scratch area to hold intermediate results.
      !>
      !>     X        is the abscissa value at which the interpolating
      !>              polynomial and its derivative are to be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     F,
      !>     DF       are the value and derivative at X of the unique
      !>              polynomial of degree 2N-1 that fits the points and
      !>              derivatives defined by XVALS and YVALS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If two input abscissas are equal, the error
      !>         SPICE(DIVIDEBYZERO) is signaled.
      !>
      !>     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
      !>         signaled.
      !>
      !>     3)  This routine does not attempt to ward off or diagnose
      !>         arithmetic overflows.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Users of this routine must choose the number of points to use
      !>     in their interpolation method. The authors of Reference [1] have
      !>     this to say on the topic:
      !>
      !>        Unless there is solid evidence that the interpolating function
      !>        is close in form to the true function f, it is a good idea to
      !>        be cautious about high-order interpolation. We
      !>        enthusiastically endorse interpolations with 3 or 4 points, we
      !>        are perhaps tolerant of 5 or 6; but we rarely go higher than
      !>        that unless there is quite rigorous monitoring of estimated
      !>        errors.
      !>
      !>     The same authors offer this warning on the use of the
      !>     interpolating function for extrapolation:
      !>
      !>        ...the dangers of extrapolation cannot be overemphasized:
      !>        An interpolating function, which is perforce an extrapolating
      !>        function, will typically go berserk when the argument x is
      !>        outside the range of tabulated values by more than the typical
      !>        spacing of tabulated points.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Fit a 7th degree polynomial through the points ( x, y, y' )
      !>
      !>           ( -1,      6,       3 )
      !>           (  0,      5,       0 )
      !>           (  3,   2210,    5115 )
      !>           (  5,  78180,  109395 )
      !>
      !>        and evaluate this polynomial at x = 2.
      !>
      !>        The returned value should be 141.0, and the returned
      !>        derivative value should be 456.0, since the unique 7th degree
      !>        polynomial that fits these constraints is
      !>
      !>                     7       2
      !>           f(x)  =  x   +  2x  + 5
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>       PROGRAM HRMINT_EX1
      !>       IMPLICIT NONE
      !>
      !>       DOUBLE PRECISION      ANSWER
      !>       DOUBLE PRECISION      DERIV
      !>       DOUBLE PRECISION      XVALS (4)
      !>       DOUBLE PRECISION      YVALS (8)
      !>       DOUBLE PRECISION      WORK  (8,2)
      !>       INTEGER               N
      !>
      !>       N         =   4
      !>
      !>       XVALS(1)  =      -1.D0
      !>       XVALS(2)  =       0.D0
      !>       XVALS(3)  =       3.D0
      !>       XVALS(4)  =       5.D0
      !>
      !>       YVALS(1)  =       6.D0
      !>       YVALS(2)  =       3.D0
      !>       YVALS(3)  =       5.D0
      !>       YVALS(4)  =       0.D0
      !>       YVALS(5)  =    2210.D0
      !>       YVALS(6)  =    5115.D0
      !>       YVALS(7)  =   78180.D0
      !>       YVALS(8)  =  109395.D0
      !>
      !>       CALL HRMINT ( N, XVALS, YVALS, 2.D0, WORK, ANSWER,
      !>      .                                           DERIV )
      !>
      !>       WRITE (*,*) 'ANSWER = ', ANSWER
      !>       WRITE (*,*) 'DERIV  = ', DERIV
      !>
      !>       END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         ANSWER =    141.00000000000000
      !>         DERIV  =    456.00000000000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapters 3.0 and 3.1, Cambridge University Press, 1986.
      !>
      !>     [2]  S. Conte and C. de Boor, "Elementary Numerical Analysis -- An
      !>          Algorithmic Approach," 3rd Edition, p 64, McGraw-Hill, 1980.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.2, 01-OCT-2021 (NJB) (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Fixed
      !>        a few more comment typos. Added IMPLICIT NONE to code
      !>        example.
      !>
      !>-    SPICELIB Version 1.2.1, 28-JAN-2014 (NJB)
      !>
      !>        Fixed a few comment typos.
      !>
      !>-    SPICELIB Version 1.2.0, 01-FEB-2002 (NJB) (EDW)
      !>
      !>        Bug fix: declarations of local variables XI and XIJ
      !>        were changed from DOUBLE PRECISION to INTEGER.
      !>        Note: bug had no effect on behavior of this routine.
      !>
      !>-    SPICELIB Version 1.1.0, 28-DEC-2001 (NJB)
      !>
      !>        Blanks following final newline were truncated to
      !>        suppress compilation warnings on the SGI-N32 platform.
      !>
      !>-    SPICELIB Version 1.0.0, 01-MAR-2000 (NJB)
      module subroutine hrmint ( ctx, n, xvals, yvals, x, work, f, df )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
      real(wp), intent(in) :: xvals (   n )  !! Abscissa values.
      real(wp), intent(in) :: yvals ( 2*n )  !! Ordinate and derivative values.
      real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
      real(wp), intent(inout) :: work  ( 2*n, 2 )  !! Work space array.
      real(wp), intent(out) :: f  !! Interpolated function value at X.
      real(wp), intent(out) :: df  !! Interpolated function's derivative at X.

      real(wp) :: c1
      real(wp) :: c2
      real(wp) :: denom
      real(wp) :: temp

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: next
      integer(ip) :: prev
      integer(ip) :: this
      integer(ip) :: xi
      integer(ip) :: xij

      if ( ctx%failed() ) return

!
!     No data, no interpolation.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'Array size must be positive; was #.' )
         call ctx%errint ( '#',  n                               )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'                  )
         return

      end if

!
!     Copy the input array into WORK.  After this, the first column
!     of WORK represents the first column of our triangular
!     interpolation table.
!
      do i = 1, 2*n
         work(i,1) = yvals(i)
      end do

!
!     Compute the second column of the interpolation table: this
!     consists of the N-1 values obtained by evaluating the
!     first-degree interpolants at X. We'll also evaluate the
!     derivatives of these interpolants at X and save the results in
!     the second column of WORK. Because the derivative computations
!     depend on the function computations from the previous column in
!     the interpolation table, and because the function interpolation
!     overwrites the previous column of interpolated function values,
!     we must evaluate the derivatives first.
!

      do i = 1, n-1

         c1     =  xvals(i+1) - x
         c2     =  x          - xvals(i)
         denom  =  xvals(i+1) - xvals(i)

         if ( denom == 0.0_wp ) then

            call ctx%setmsg ( 'XVALS(#) = XVALS(#) = #' )
            call ctx%errint ( '#',   i                  )
            call ctx%errint ( '#',   i+1                )
            call ctx%errdp  ( '#',   xvals(i)           )
            call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'     )
            return

         end if

!
!        The second column of WORK contains interpolated derivative
!        values.
!
!        The odd-indexed interpolated derivatives are simply the input
!        derivatives.
!
         prev = 2*i  - 1
         this = prev + 1
         next = this + 1

         work( prev,  2 )  =    work( this, 1 )

!
!        The even-indexed interpolated derivatives are the slopes of
!        the linear interpolating polynomials for adjacent input
!        abscissa/ordinate pairs.
!
         work( this, 2 ) = ( work(next, 1) - work(prev, 1) ) / denom

!
!        The first column of WORK contains interpolated function values.
!        The odd-indexed entries are the linear Taylor polynomials,
!        for each input abscissa value, evaluated at X.
!
         temp  = work(this, 1) * ( x - xvals(i) )  +  work(prev, 1)

         work( this, 1 ) = (   c1 * work( prev, 1 ) &
                             + c2 * work( next, 1 )  )  /  denom

         work( prev, 1 ) =     temp

      end do

!
!     The last column entries were not computed by the preceding loop;
!     compute them now.
!
      work ( 2*n - 1,  2 )  =    work( 2*n,     1 )
      work ( 2*n - 1,  1 )  =    work( 2*n,     1 ) * ( x - xvals(n) ) &
                               + work( 2*n - 1, 1 )

!
!     Compute columns 3 through 2*N of the table.
!
      do j = 2, 2*n-1

         do i = 1, 2*n-j
!
!           In the theoretical construction of the interpolation table,
!           there are 2*N abscissa values, since each input abscissa
!           value occurs with multiplicity two. In this theoretical
!           construction, the Jth column of the interpolation table
!           contains results of evaluating interpolants that span J+1
!           consecutive abscissa values.  The indices XI and XIJ below
!           are used to pick the correct abscissa values out of the
!           physical XVALS array, in which the abscissa values are not
!           repeated.
!
            xi       =   ( i     + 1 ) / 2
            xij      =   ( i + j + 1 ) / 2

            c1       =     xvals(xij)   -   x
            c2       =     x            -   xvals(xi)

            denom    =     xvals(xij)   -   xvals(xi)

            if ( denom == 0.0_wp ) then

               call ctx%setmsg ( 'XVALS(#) = XVALS(#) = #' )
               call ctx%errint ( '#',   xi                 )
               call ctx%errint ( '#',   xij                )
               call ctx%errdp  ( '#',   xvals(xi)          )
               call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'     )
               return

            end if

!
!           Compute the interpolated derivative at X for the Ith
!           interpolant. This is the derivative with respect to X of
!           the expression for the interpolated function value, which
!           is the second expression below. This derivative computation
!           is done first because it relies on the interpolated
!           function values from the previous column of the
!           interpolation table.
!
!           The derivative expression here corresponds to equation
!           2.35 on page 64 in reference [2].
!
            work(i,2) = (              c1  * work(i,   2) &
                          +            c2  * work(i+1, 2) &
                          + ( work(i+1, 1) - work(i,   1) )  ) / denom
!
!           Compute the interpolated function value at X for the Ith
!           interpolant.
!
            work(i,1) = ( c1 * work(i,1)  +  c2 * work(i+1,1) ) / denom

         end do

      end do

!
!     Our interpolated function value is sitting in WORK(1,1) at this
!     point.  The interpolated derivative is located in WORK(1,2).
!
      f   =  work( 1, 1 )
      df  =  work( 1, 2 )

      return
      end subroutine hrmint

      !> Evaluate a Lagrange interpolating polynomial for a specified
      !> set of coordinate pairs whose first components are equally
      !> spaced, at a specified abscissa value.
      !>
      !> The function returns the value at X of the unique polynomial of
      !> degree N-1 that fits the points in the plane defined by FIRST,
      !> STEP, and YVALS.
      !>
      !>## Detailed Input
      !>
      !>     N        is the number of points defining the polynomial. The
      !>              array YVALS contains N elements.
      !>
      !>     FIRST,
      !>     STEP     are, respectively, a starting abscissa value and a
      !>              step size that define the set of abscissa values
      !>              at which a Lagrange interpolating polynomial is to
      !>              be defined. The set of abscissa values is
      !>
      !>                 FIRST   +   I * STEP,     I = 0, ..., N-1
      !>
      !>              STEP must be non-zero.
      !>
      !>     YVALS    is an array of ordinate values that, together with
      !>              the abscissa values defined by FIRST and STEP,
      !>              define N ordered pairs belonging to the graph of
      !>              a function. The set of points
      !>
      !>                 (  FIRST  +  (I-1)*STEP,   YVALS(I)  )
      !>
      !>              where I ranges from 1 to N, define the Lagrange
      !>              polynomial used for interpolation.
      !>
      !>     WORK     is a work space array of the same dimension as YVALS.
      !>              It is used by this routine as a scratch area to hold
      !>              intermediate results.
      !>
      !>     X        is the abscissa value at which the interpolating
      !>              polynomial is to be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value at X of the unique polynomial of
      !>     degree N-1 that fits the points in the plane defined by FIRST,
      !>     STEP, and YVALS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If STEP is zero, the error SPICE(INVALIDSTEPSIZE) is
      !>         signaled. The function will return the value 0.D0.
      !>
      !>     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
      !>         signaled. The function will return the value 0.D0.
      !>
      !>     3)  This routine does not attempt to ward off or diagnose
      !>         arithmetic overflows.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given a set of N distinct abscissa values and corresponding
      !>     ordinate values, there is a unique polynomial of degree N-1,
      !>     often called the "Lagrange polynomial", that fits the graph
      !>     defined by these values. The Lagrange polynomial can be used to
      !>     interpolate the value of a function at a specified point, given a
      !>     discrete set of values of the function.
      !>
      !>     Users of this routine must choose the number of points to use
      !>     in their interpolation method. The authors of Reference [1] have
      !>     this to say on the topic:
      !>
      !>        Unless there is solid evidence that the interpolating function
      !>        is close in form to the true function F, it is a good idea to
      !>        be cautious about high-order interpolation. We
      !>        enthusiastically endorse interpolations with 3 or 4 points, we
      !>        are perhaps tolerant of 5 or 6; but we rarely go higher than
      !>        that unless there is quite rigorous monitoring of estimated
      !>        errors.
      !>
      !>     The same authors offer this warning on the use of the
      !>     interpolating function for extrapolation:
      !>
      !>        ...the dangers of extrapolation cannot be overemphasized:
      !>        An interpolating function, which is perforce an extrapolating
      !>        function, will typically go berserk when the argument X is
      !>        outside the range of tabulated values by more than the typical
      !>        spacing of tabulated points.
      !>
      !>     For Lagrange interpolation on unequally spaced abscissa values,
      !>     see the SPICELIB routine LGRINT.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Fit a cubic polynomial through the points
      !>
      !>            ( -1,  -2 )
      !>            (  1,  -8 )
      !>            (  3,  26 )
      !>            (  5, 148 )
      !>
      !>        and evaluate this polynomial at x = 2.
      !>
      !>        The returned value of LGRESP should be 1.D0, since the
      !>        unique cubic polynomial that fits these points is
      !>
      !>                     3      2
      !>           F(X)  =  X  + 2*X  - 4*X - 7
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>       PROGRAM LGRESP_EX1
      !>       IMPLICIT NONE
      !>
      !>       DOUBLE PRECISION      LGRESP
      !>       DOUBLE PRECISION      ANSWER
      !>       DOUBLE PRECISION      FIRST
      !>       DOUBLE PRECISION      STEP
      !>       DOUBLE PRECISION      YVALS (4)
      !>       DOUBLE PRECISION      WORK  (4)
      !>       INTEGER               N
      !>
      !>       N         =   4
      !>       FIRST     =  -1.D0
      !>       STEP      =   2.D0
      !>
      !>       YVALS(1)  =  -2.D0
      !>       YVALS(2)  =  -8.D0
      !>       YVALS(3)  =  26.D0
      !>       YVALS(4)  = 148.D0
      !>
      !>       ANSWER    =   LGRESP ( N,     FIRST, STEP,
      !>      .                       YVALS, WORK,  2.D0 )
      !>
      !>       WRITE (*,*) 'ANSWER = ', ANSWER
      !>       END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         ANSWER =    1.0000000000000000
      !>
      !>     2) Solve the same problem using a negative step. In order to
      !>        find the solution, set the elements of YVALS in reverse order.
      !>
      !>        The returned value of LGRESP would still be 1.D0.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>       PROGRAM LGRESP_EX2
      !>       IMPLICIT NONE
      !>
      !>       DOUBLE PRECISION      LGRESP
      !>       DOUBLE PRECISION      ANSWER
      !>       DOUBLE PRECISION      FIRST
      !>       DOUBLE PRECISION      STEP
      !>       DOUBLE PRECISION      YVALS (4)
      !>       DOUBLE PRECISION      WORK  (4)
      !>       INTEGER               N
      !>
      !>       N         =   4
      !>       FIRST     =   5.D0
      !>       STEP      =  -2.D0
      !>
      !>       YVALS(1)  = 148.D0
      !>       YVALS(2)  =  26.D0
      !>       YVALS(3)  =  -8.D0
      !>       YVALS(4)  =  -2.D0
      !>
      !>       ANSWER    =   LGRESP ( N,     FIRST, STEP,
      !>      .                       YVALS, WORK,  2.D0 )
      !>
      !>       WRITE (*,*) 'ANSWER = ', ANSWER
      !>       END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         ANSWER =    1.0000000000000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapters 3.0 and 3.1, Cambridge University Press, 1986.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 19-FEB-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Update "N" and "WORK" detailed descriptions to remove
      !>        references to nonexistent arguments.
      !>
      !>        Updated the header to comply with NAIF standard.
      !>        Added IMPLICIT NONE to code example. Added second example
      !>        providing code to solve the problem of example #1 using a
      !>        negative step.
      !>
      !>-    SPICELIB Version 1.0.1, 10-JAN-2014 (NJB)
      !>
      !>        Updated description of the workspace array: now the array WORK
      !>        is not described as being allowed to coincide with the input
      !>        YVALS. Such overlap would be a violation of the ANSI Fortran
      !>        77 standard. Corrected several spelling errors in header
      !>        documentation.
      !>
      !>-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
      module real(wp) function lgresp ( ctx, n, first, step, yvals, work, x )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
      real(wp), intent(in) :: first  !! First abscissa value.
      real(wp), intent(in) :: step  !! Step size.
      real(wp), intent(in) :: yvals ( * )  !! Ordinate values.
      real(wp), intent(inout) :: work  ( * )  !! Work space array.
      real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.

      real(wp) :: c1
      real(wp) :: c2
      real(wp) :: newx

      integer(ip) :: i
      integer(ip) :: j

      if ( ctx%failed() ) then
         lgresp = 0.0_wp
         return
      end if

!
!     No data, no interpolation.
!
      if ( n < 1 ) then

         lgresp = 0.0_wp

         call ctx%setmsg ( 'Array size must be positive; was #.' )
         call ctx%errint ( '#',  n                               )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'                  )
         return

      end if

!
!     The step size must be non-zero.
!
      if ( step == 0.0_wp ) then

         lgresp = 0.0_wp

         call ctx%setmsg ( 'Step size was zero.'    )
         call ctx%sigerr ( 'SPICE(INVALIDSTEPSIZE)' )
         return

      end if

!
!     We can simplify the interpolation problem by shifting
!     and scaling the abscissa values so that they start at 1
!     and are separated by a unit step.  All we need to do is
!     shift and scale X.
!
      newx  =  (  ( x - first ) / step  )  +  1.0_wp

!
!     We're going to compute the value of our interpolating polynomial
!     at X by taking advantage of a recursion relation between
!     Lagrange polynomials of order n+1 and order n.  The method works
!     as follows for the case of abscissa values that are not
!     necessarily uniformly spaced:
!
!        Define
!
!           P               (x)
!            i(i+1)...(i+j)
!
!        to be the unique Lagrange polynomial that interpolates our
!        input function at the abscissa values
!
!           x ,  x   , ... x   .
!            i    i+1       i+j
!
!        Then we have the recursion relation
!
!           P              (x)  =
!            i(i+1)...(i+j)
!
!                                  x - x
!                                   i
!                                 -----------  *  P                (x)
!                                  x - x           (i+1)...(i+j)
!                                   i   i+j
!
!                                  x  -  x
!                                         i+j
!                               + -----------  *  P                (x)
!                                  x  -  x         i(i+1)...(i+j-1)
!                                   i     i+j
!
!        Repeated application of this relation allows us to build
!        successive columns, in left-to-right order, of the
!        triangular table
!
!           P (x)
!            1
!                    P  (x)
!                     12
!           P (x)             P   (x)
!            2                 123
!                    P  (x)
!                     23               .
!                             P   (x)
!           .                  234            .
!           .
!           .        .                               .
!                    .
!                    .        .                           P      (x)
!                             .                      .     12...N
!                             .
!                                             .
!
!                                      .
!
!                             P           (x)
!                              (N-2)(N-1)N
!                    P     (x)
!                     (N-1)N
!           P (x)
!            N
!
!        and after N-1 steps arrive at our desired result,
!
!           P       (x).
!            12...N
!
!     In the current case, we've arranged the problem so that
!
!       x    =   i.
!        i
!
!     We'll use the scratch array WORK to contain the current column of
!     our interpolation table.  To start out with, WORK(I) will contain
!
!        P (x).
!         I
!
      do i = 1, n
         work(i) = yvals(i)
      end do

!
!     Compute columns 2 through N of the table.
!
      do j = 1, n-1

         do i = 1, n-j

            c1       =   real(i+j, wp)   -   newx
            c2       =   newx        -   real(i, wp)

            work(i)  =  (  c1 * work(i)  +  c2 * work(i+1)  )  / real(j, wp)

         end do

      end do

!
!     Our result is sitting in WORK(1) at this point.
!
      lgresp = work(1)

      return
      end function lgresp

      !> Evaluate a Lagrange interpolating polynomial, for a specified
      !> set of coordinate pairs, at a specified abscissa value. Return
      !> both the value of the polynomial and its derivative.
      !>
      !>## Detailed Input
      !>
      !>     N        is the number of points defining the polynomial.
      !>              The arrays XVALS and YVALS contain N elements.
      !>
      !>     XVALS,
      !>     YVALS    are arrays of abscissa and ordinate values that
      !>              together define N ordered pairs. The set of points
      !>
      !>                 ( XVALS(I), YVALS(I) )
      !>
      !>              define the Lagrange polynomial used for
      !>              interpolation. The elements of XVALS must be
      !>              distinct and in increasing order.
      !>
      !>     WORK     is an N * 2 work space array, where N is the same
      !>              dimension as that of XVALS and YVALS. It is used
      !>              by this routine as a scratch area to hold
      !>              intermediate results.
      !>
      !>     X        is the abscissa value at which the interpolating
      !>              polynomial is to be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     P        is the value at X of the unique polynomial of
      !>              degree N-1 that fits the points in the plane
      !>              defined by XVALS and YVALS.
      !>
      !>     DP       is the derivative at X of the interpolating
      !>              polynomial described above.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any two elements of the array XVALS are equal, the error
      !>         SPICE(DIVIDEBYZERO) is signaled.
      !>
      !>     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
      !>         signaled.
      !>
      !>     3)  This routine does not attempt to ward off or diagnose
      !>         arithmetic overflows.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given a set of N distinct abscissa values and corresponding
      !>     ordinate values, there is a unique polynomial of degree N-1, often
      !>     called the "Lagrange polynomial", that fits the graph defined by
      !>     these values. The Lagrange polynomial can be used to interpolate
      !>     the value of a function at a specified point, given a discrete
      !>     set of values of the function.
      !>
      !>     Users of this routine must choose the number of points to use
      !>     in their interpolation method. The authors of Reference [1] have
      !>     this to say on the topic:
      !>
      !>        Unless there is solid evidence that the interpolating function
      !>        is close in form to the true function F, it is a good idea to
      !>        be cautious about high-order interpolation. We
      !>        enthusiastically endorse interpolations with 3 or 4 points, we
      !>        are perhaps tolerant of 5 or 6; but we rarely go higher than
      !>        that unless there is quite rigorous monitoring of estimated
      !>        errors.
      !>
      !>     The same authors offer this warning on the use of the
      !>     interpolating function for extrapolation:
      !>
      !>        ...the dangers of extrapolation cannot be overemphasized:
      !>        An interpolating function, which is perforce an extrapolating
      !>        function, will typically go berserk when the argument X is
      !>        outside the range of tabulated values by more than the typical
      !>        spacing of tabulated points.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Fit a cubic polynomial through the points
      !>
      !>            ( -1, -2 )
      !>            (  0, -7 )
      !>            (  1, -8 )
      !>            (  3, 26 )
      !>
      !>        and evaluate this polynomial at X = 2.
      !>
      !>        The returned value of P should be 1.0, since the
      !>        unique cubic polynomial that fits these points is
      !>
      !>                       3      2
      !>           F(X)   =   X  + 2*X  - 4*X - 7
      !>
      !>        The returned value of DP should be 16.0, since the
      !>        derivative of f(x) is
      !>
      !>            '           2
      !>           F (X)  =  3*X  + 4*X - 4
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LGRIND_EX1
      !>      IMPLICIT NONE
      !>
      !>      DOUBLE PRECISION      P
      !>      DOUBLE PRECISION      DP
      !>      DOUBLE PRECISION      XVALS (4)
      !>      DOUBLE PRECISION      YVALS (4)
      !>      DOUBLE PRECISION      WORK  (4,2)
      !>      INTEGER               N
      !>
      !>      N         =   4
      !>
      !>      XVALS(1)  =  -1
      !>      XVALS(2)  =   0
      !>      XVALS(3)  =   1
      !>      XVALS(4)  =   3
      !>
      !>      YVALS(1)  =  -2
      !>      YVALS(2)  =  -7
      !>      YVALS(3)  =  -8
      !>      YVALS(4)  =  26
      !>
      !>      CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
      !>
      !>      WRITE (*,*) 'P, DP = ', P, DP
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         P, DP =    1.0000000000000000        16.000000000000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapters 3.0 and 3.1, Cambridge University Press, 1986.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        "IMPLICIT NONE" to code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-JAN-2014 (NJB)
      !>
      !>        Updated description of the workspace array: now the array WORK
      !>        is not described as being allowed to coincide with the input
      !>        YVALS. Such overlap would be a violation of the ANSI Fortran
      !>        77 standard. Corrected a spelling error in header
      !>        documentation.
      !>
      !>-    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB)
      module subroutine lgrind ( ctx, n, xvals, yvals, work, x, p, dp )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
      real(wp), intent(in) :: xvals ( n )  !! Abscissa values.
      real(wp), intent(in) :: yvals ( n )  !! Ordinate values.
      real(wp), intent(inout) :: work  ( n, 2 )  !! Work space array.
      real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
      real(wp), intent(out) :: p  !! Polynomial value at X.
      real(wp), intent(out) :: dp  !! Polynomial derivative at X.

      real(wp) :: c1
      real(wp) :: c2
      real(wp) :: denom

      integer(ip) :: i
      integer(ip) :: j

      if ( ctx%failed() ) return

!
!     No data, no interpolation.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'Array size must be positive; was #.' )
         call ctx%errint ( '#',  n                               )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'                  )
         return

      end if

!
!     We're going to compute the value of our interpolating polynomial
!     at X by taking advantage of a recursion relation between
!     Lagrange polynomials of order n+1 and order n.  The method works
!     as follows:
!
!        Define
!
!           P               (x)
!            i(i+1)...(i+j)
!
!        to be the unique Lagrange polynomial that interpolates our
!        input function at the abscissa values
!
!           x ,  x   , ... x   .
!            i    i+1       i+j
!
!        Then we have the recursion relation
!
!           P              (x)  =
!            i(i+1)...(i+j)
!
!                                  x - x
!                                   i
!                                 -----------  *  P                (x)
!                                  x - x           (i+1)...(i+j)
!                                   i   i+j
!
!                                  x  -  x
!                                         i+j
!                               + -----------  *  P                (x)
!                                  x  -  x         i(i+1)...(i+j-1)
!                                   i     i+j
!
!        Repeated application of this relation allows us to build
!        successive columns, in left-to-right order, of the
!        triangular table
!
!           P (x)
!            1
!                    P  (x)
!                     12
!           P (x)             P   (x)
!            2                 123
!                    P  (x)
!                     23               .
!                             P   (x)
!           .                  234            .
!           .
!           .        .                               .
!                    .
!                    .        .                           P      (x)
!                             .                      .     12...N
!                             .
!                                             .
!
!                                      .
!
!                             P           (x)
!                              (N-2)(N-1)N
!                    P     (x)
!                     (N-1)N
!           P (x)
!            N
!
!        and after N-1 steps arrive at our desired result,
!
!           P       (x).
!            12...N
!
!     The computation is easier to do than to describe.
!
!     We'll use the scratch array WORK to contain the current column of
!     our interpolation table.  To start out with, WORK(I) will contain
!
!        P (x).
!         I
!
!     For columns 2...N of the table, we'll also carry along the
!     derivative at X of each interpolating polynomial.  This will
!     allow us to find the derivative of the Lagrange polynomial
!     at X.
!

      do i = 1, n

         work(i,1)  =  yvals(i)
         work(i,2)  =  0.0_wp

      end do

!
!     Compute columns 2 through N of the table.  Note that DENOM must
!     be non-zero, or else a divide-by-zero error will occur.
!
      do j = 1, n-1

         do i = 1, n-j

            denom    =     xvals(i)  -  xvals(i+j)

            if ( denom == 0.0_wp ) then

               call ctx%setmsg ( 'XVALS(#) = XVALS(#) = #' )
               call ctx%errint ( '#',   i                  )
               call ctx%errint ( '#',   i + j              )
               call ctx%errdp  ( '#',   xvals(i)           )
               call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'     )
               return

            end if

            c1       =     x         -  xvals(i+j)
            c2       =     xvals(i)  -  x

!
!           Use the chain rule to compute the derivatives.  Do this
!           before computing the function value, because the latter
!           computation will overwrite the first column of WORK.
!
            work(i,2) = (   ( c1*work(i,2) + c2*work(i+1,2) ) &
                          + (    work(i,1) -    work(i+1,1) )  ) / denom

!
!           Compute the Ith entry in the Jth column.
!
            work(i,1) =     ( c1*work(i,1) + c2*work(i+1,1) )    / denom

         end do

      end do

!
!     Our results are sitting in WORK(1,1) and WORK(1,2) at this point.
!
      p  =  work(1,1)
      dp =  work(1,2)

      return
      end subroutine lgrind

      !> Evaluate a Lagrange interpolating polynomial for a specified
      !> set of coordinate pairs, at a specified abscissa value.
      !>
      !> The function returns the value at X of the unique polynomial of
      !> degree N-1 that fits the points in the plane defined by XVALS and
      !> YVALS.
      !>
      !>## Detailed Input
      !>
      !>     N        is the number of points defining the polynomial.
      !>              The arrays XVALS and YVALS contain N elements.
      !>
      !>     XVALS,
      !>     YVALS    are arrays of abscissa and ordinate values that
      !>              together define N ordered pairs. The set of points
      !>
      !>                 ( XVALS(I), YVALS(I) )
      !>
      !>              define the Lagrange polynomial used for
      !>              interpolation. The elements of XVALS must be
      !>              distinct and in increasing order.
      !>
      !>     WORK     is a work space array of the same dimension as
      !>              XVALS and YVALS. It is used by this routine as a
      !>              scratch area to hold intermediate results.
      !>
      !>     X        is the abscissa value at which the interpolating
      !>              polynomial is to be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value at X of the unique polynomial of
      !>     degree N-1 that fits the points in the plane defined by XVALS and
      !>     YVALS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any two elements of the array XVALS are equal, the error
      !>         SPICE(DIVIDEBYZERO) is signaled. The function will return the
      !>         value 0.D0.
      !>
      !>     2)  If N is less than 1, the error SPICE(INVALIDSIZE) is
      !>         signaled. The function will return the value 0.D0.
      !>
      !>     3)  This routine does not attempt to ward off or diagnose
      !>         arithmetic overflows.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given a set of N distinct abscissa values and corresponding
      !>     ordinate values, there is a unique polynomial of degree N-1, often
      !>     called the "Lagrange polynomial", that fits the graph defined by
      !>     these values. The Lagrange polynomial can be used to interpolate
      !>     the value of a function at a specified point, given a discrete
      !>     set of values of the function.
      !>
      !>     Users of this routine must choose the number of points to use
      !>     in their interpolation method. The authors of Reference [1] have
      !>     this to say on the topic:
      !>
      !>        Unless there is solid evidence that the interpolating function
      !>        is close in form to the true function F, it is a good idea to
      !>        be cautious about high-order interpolation. We
      !>        enthusiastically endorse interpolations with 3 or 4 points, we
      !>        are perhaps tolerant of 5 or 6; but we rarely go higher than
      !>        that unless there is quite rigorous monitoring of estimated
      !>        errors.
      !>
      !>     The same authors offer this warning on the use of the
      !>     interpolating function for extrapolation:
      !>
      !>        ...the dangers of extrapolation cannot be overemphasized:
      !>        An interpolating function, which is perforce an extrapolating
      !>        function, will typically go berserk when the argument X is
      !>        outside the range of tabulated values by more than the typical
      !>        spacing of tabulated points.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Fit a cubic polynomial through the points
      !>
      !>            ( -1, -2 )
      !>            (  0, -7 )
      !>            (  1, -8 )
      !>            (  3, 26 )
      !>
      !>        and evaluate this polynomial at X = 2.
      !>
      !>        The returned value of LGRINT should be 1.D0, since the
      !>        unique cubic polynomial that fits these points is
      !>
      !>                     3      2
      !>           F(X)  =  X  + 2*X  - 4*X - 7
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LGRINT_EX1
      !>      IMPLICIT NONE
      !>
      !>      DOUBLE PRECISION      LGRINT
      !>      DOUBLE PRECISION      ANSWER
      !>      DOUBLE PRECISION      XVALS (4)
      !>      DOUBLE PRECISION      YVALS (4)
      !>      DOUBLE PRECISION      WORK  (4)
      !>      INTEGER               N
      !>
      !>      N         =   4
      !>
      !>      XVALS(1)  =  -1.D0
      !>      XVALS(2)  =   0.D0
      !>      XVALS(3)  =   1.D0
      !>      XVALS(4)  =   3.D0
      !>
      !>      YVALS(1)  =  -2.D0
      !>      YVALS(2)  =  -7.D0
      !>      YVALS(3)  =  -8.D0
      !>      YVALS(4)  =  26.D0
      !>
      !>      ANSWER    =   LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      !>
      !>      WRITE (*,*) 'ANSWER = ', ANSWER
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         ANSWER =    1.0000000000000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapters 3.0 and 3.1, Cambridge University Press, 1986.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 19-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard.
      !>        Added "IMPLICIT NONE" to code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-JAN-2014 (NJB)
      !>
      !>        Updated description of the workspace array: now the array WORK
      !>        is not described as being allowed to coincide with the input
      !>        YVALS. Such overlap would be a violation of the ANSI Fortran
      !>        77 standard. Corrected several spelling errors in header
      !>        documentation.
      !>
      !>-    SPICELIB Version 1.0.0, 16-AUG-1993 (NJB)
      module real(wp) function lgrint ( ctx, n, xvals, yvals, work, x )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
      real(wp), intent(in) :: xvals ( * )  !! Abscissa values.
      real(wp), intent(in) :: yvals ( * )  !! Ordinate values.
      real(wp), intent(inout) :: work  ( * )  !! Work space array.
      real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.

      real(wp) :: c1
      real(wp) :: c2
      real(wp) :: denom

      integer(ip) :: i
      integer(ip) :: j

      if ( ctx%failed() ) then
         lgrint = 0.0_wp
         return
      end if

!
!     No data, no interpolation.
!
      if ( n < 1 ) then

         lgrint = 0.0_wp

         call ctx%setmsg ( 'Array size must be positive; was #.' )
         call ctx%errint ( '#',  n                               )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)'                  )
         return

      end if

!
!     We're going to compute the value of our interpolating polynomial
!     at X by taking advantage of a recursion relation between
!     Lagrange polynomials of order n+1 and order n.  The method works
!     as follows:
!
!        Define
!
!           P               (x)
!            i(i+1)...(i+j)
!
!        to be the unique Lagrange polynomial that interpolates our
!        input function at the abscissa values
!
!           x ,  x   , ... x   .
!            i    i+1       i+j
!
!        Then we have the recursion relation
!
!           P              (x)  =
!            i(i+1)...(i+j)
!
!                                  x - x
!                                   i
!                                 -----------  *  P                (x)
!                                  x - x           (i+1)...(i+j)
!                                   i   i+j
!
!                                  x  -  x
!                                         i+j
!                               + -----------  *  P                (x)
!                                  x  -  x         i(i+1)...(i+j-1)
!                                   i     i+j
!
!        Repeated application of this relation allows us to build
!        successive columns, in left-to-right order, of the
!        triangular table
!
!           P (x)
!            1
!                    P  (x)
!                     12
!           P (x)             P   (x)
!            2                 123
!                    P  (x)
!                     23               .
!                             P   (x)
!           .                  234            .
!           .
!           .        .                               .
!                    .
!                    .        .                           P      (x)
!                             .                      .     12...N
!                             .
!                                             .
!
!                                      .
!
!                             P           (x)
!                              (N-2)(N-1)N
!                    P     (x)
!                     (N-1)N
!           P (x)
!            N
!
!        and after N-1 steps arrive at our desired result,
!
!           P       (x).
!            12...N
!
!     The computation is easier to do than to describe.
!
!     We'll use the scratch array WORK to contain the current column of
!     our interpolation table.  To start out with, WORK(I) will contain
!
!        P (x).
!         I
!
      do i = 1, n
         work(i) = yvals(i)
      end do

!
!     Compute columns 2 through N of the table.  Note that DENOM must
!     be non-zero, or else a divide-by-zero error will occur.
!
      do j = 1, n-1

         do i = 1, n-j

            denom    =     xvals(i)  -  xvals(i+j)

            if ( denom == 0.0_wp ) then

               lgrint = 0.0_wp
               call ctx%setmsg ( 'XVALS(#) = XVALS(#) = #' )
               call ctx%errint ( '#',   i                  )
               call ctx%errint ( '#',   i + j              )
               call ctx%errdp  ( '#',   xvals(i)           )
               call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'     )
               return

            end if

            c1       =     x         -  xvals(i+j)
            c2       =     xvals(i)  -  x

            work(i)  =  (  c1 * work(i)  +  c2 * work(i+1)  ) / denom

         end do

      end do

!
!     Our result is sitting in WORK(1) at this point.
!
      lgrint = work(1)

      return
      end function lgrint

      !> Evaluate an indefinite integral of a Chebyshev expansion at a
      !> specified point X and return the value of the input expansion at
      !> X as well. The constant of integration is selected to make the
      !> integral zero when X equals the abscissa value X2S(1).
      !>
      !>## Detailed Input
      !>
      !>     DEGP     is the degree of the input Chebyshev expansion.
      !>
      !>     CP       is an array containing the coefficients of the input
      !>              Chebyshev expansion. The coefficient of the I'th
      !>              Chebyshev polynomial is contained in element CP(I+1),
      !>              for I = 0 : DEGP.
      !>
      !>     X2S      is an array containing the "transformation parameters"
      !>              of the domain of the expansion. Element X2S(1)
      !>              contains the midpoint of the interval on which the
      !>              input expansion is defined; X2S(2) is one-half of the
      !>              length of this interval; this value is called the
      !>              interval's "radius."
      !>
      !>              The input expansion defines a function f(X) on the
      !>              interval
      !>
      !>                 [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
      !>
      !>              as follows:
      !>
      !>                 Define S = ( X - X2S(1) ) / X2S(2)
      !>
      !>                                   DEGP+1
      !>                                   __
      !>                                   \
      !>                    f(X) = g(S)  = /  CP(k)  T   (S)
      !>                                   --         k-1
      !>                                   k=1
      !>
      !>     X        is the abscissa value at which the function defined by
      !>              the input expansion and its integral are to be
      !>              evaluated. Normally X should lie in the closed
      !>              interval
      !>
      !>                 [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
      !>
      !>              See the $Restrictions section below.
      !>
      !>## Detailed Output
      !>
      !>     P,
      !>     ITGRLP   define S and f(X) as above in the description of the
      !>              input argument X2S. Then P is f(X), and ITGRLP is
      !>              an indefinite integral of f(X), evaluated at X.
      !>
      !>              The indefinite integral satisfies
      !>
      !>                 d(ITGRLP)/dX     = f(X)
      !>
      !>              and
      !>
      !>                 ITGRLP( X2S(1) ) = 0
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input degree is negative, the error
      !>         SPICE(INVALDDEGREE) is signaled.
      !>
      !>     2)  If the input interval radius is non-positive, the error
      !>         SPICE(INVALIDRADIUS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let
      !>
      !>        T ,  n = 0, ...
      !>         n
      !>
      !>     represent the nth Chebyshev polynomial of the first kind:
      !>
      !>        T (x) = cos( n*arccos(x) )
      !>         n
      !>
      !>     The input coefficients represent the Chebyshev expansion
      !>
      !>                       DEGP+1
      !>                       __
      !>                       \
      !>        f(X) = g(S)  = /  CP(k)  T   (S)
      !>                       --         k-1
      !>                       k=1
      !>
      !>     where
      !>
      !>        S = ( X - X2S(1) ) / X2S(2)
      !>
      !>     This routine evaluates and returns the value at X of an
      !>     indefinite integral F(X), where
      !>
      !>        dF(X)/dX    = f(X)  for all X in
      !>                            [X2S(1)-X2S(2), X2S(1)+X2S(2)]
      !>
      !>        F( X2S(1) ) = 0
      !>
      !>     The value at X of the input expansion
      !>
      !>        f(X)
      !>
      !>     is returned as well.
      !>
      !>     Note that numerical problems may result from applying this
      !>     routine to abscissa values outside of the interval defined
      !>     by the input parameters X2S(*). See the $Restrictions section.
      !>
      !>     To evaluate Chebyshev expansions and their derivatives, use the
      !>     SPICELIB routines CHBINT or CHBDER.
      !>
      !>     This routine supports the SPICELIB SPK type 20 and PCK type 20
      !>     evaluators SPKE20 and PCKE20.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Let the domain of a polynomial to be evaluated be the
      !>        closed interval
      !>
      !>           [20, 30]
      !>
      !>        Let the input expansion represent the polynomial
      !>
      !>                             6
      !>           f(X)  = g(S) = 5*S
      !>
      !>        where
      !>
      !>           S     = (X - 20)/10
      !>
      !>        Let F(X) be an indefinite integral of f(X) such that
      !>
      !>           F(20) = 0
      !>
      !>        Evaluate
      !>
      !>           f(30) and F(30)
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CHBIGR_EX1
      !>      IMPLICIT NONE
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CP    ( 6 )
      !>      DOUBLE PRECISION      X
      !>      DOUBLE PRECISION      X2S   ( 2 )
      !>      DOUBLE PRECISION      P
      !>      DOUBLE PRECISION      ITGRLP
      !>
      !>      INTEGER               DEGP
      !>
      !>C
      !>C     Let our domain be the interval [10, 30].
      !>C
      !>      X2S(1) = 20.D0
      !>      X2S(2) = 10.D0
      !>
      !>C
      !>C     Assign the expansion coefficients.
      !>C
      !>      DEGP  = 5
      !>
      !>      CP(1) = 0.D0
      !>      CP(2) = 3.75D0
      !>      CP(3) = 0.D0
      !>      CP(4) = 1.875D0
      !>      CP(5) = 0.D0
      !>      CP(6) = 0.375D0
      !>
      !>C
      !>C     Evaluate the function and its integral at X = 30.
      !>C
      !>      X = 30.D0
      !>
      !>      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      !>
      !>C
      !>C     We make the change of variables
      !>C
      !>C        S(X) = (1/10) * ( X - 20 )
      !>C
      !>C     The expansion represents the polynomial
      !>C
      !>C                         5
      !>C        f(X) = g(S) = 6*S
      !>C
      !>C     An indefinite integral of the expansion is
      !>C
      !>C                                    6
      !>C        F(X) = G(S) * dX/dS = 10 * S
      !>C
      !>C     where G is defined on the interval [-1, 1]. The result
      !>C     should be (due to the change of variables)
      !>C
      !>C          (G(1)  - G(0) ) * dX/dS
      !>C
      !>C        = (F(30) - F(20)) * 10
      !>C
      !>C        = 10
      !>C
      !>C     The value of the expansion at X should be
      !>C
      !>C        f(30) = g(1) = 6
      !>C
      !>      WRITE (*,*) 'ITGRLP = ', ITGRLP
      !>      WRITE (*,*) 'P      = ', P
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         ITGRLP =    10.000000000000000
      !>         P      =    6.0000000000000000
      !>
      !>## Restrictions
      !>
      !>     1)  The value (X-X2S(1)) / X2S(2) normally should lie within the
      !>         interval -1:1 inclusive, that is, the closed interval
      !>         [-1, 1]. Chebyshev polynomials increase rapidly in magnitude
      !>         as a function of distance of abscissa values from this
      !>         interval.
      !>
      !>         In typical SPICE applications, where the input expansion
      !>         represents position, velocity, or orientation, abscissa
      !>         values that map to points outside of [-1, 1] due to round-off
      !>         error will not cause numeric exceptions.
      !>
      !>     2)  No checks for floating point overflow are performed.
      !>
      !>     3)  Significant accumulated round-off error can occur for input
      !>         expansions of excessively high degree. This routine imposes
      !>         no limits on the degree of the input expansion; users must
      !>         verify that the requested computation provides appropriate
      !>         accuracy.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence
      !>          Formula," p 161, Cambridge University Press, 1986.
      !>
      !>     [2]  "Chebyshev polynomials," Wikipedia, The Free Encyclopedia.
      !>          Retrieved 01:23, November 23, 2013, from
      !>          http://en.wikipedia.org/w/index.php?title=
      !>          Chebyshev_polynomials&oldid=574881046
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Corrected error in $Detailed_Input description of CP.
      !>
      !>        Fixed range of Chebyshev coefficients of input expansion in
      !>        the description of argument CP in $Detailed_Input.
      !>
      !>-    SPICELIB Version 1.0.0, 03-DEC-2013 (NJB)
      module subroutine chbigr ( ctx, degp, cp, x2s, x, p, itgrlp )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: degp  !! Degree of input Chebyshev expansion.
      real(wp), intent(in) :: cp     ( * )  !! Chebyshev coefficients of input expansion.
      real(wp), intent(in) :: x2s    ( 2 )  !! Transformation parameters.
      real(wp), intent(in) :: x  !! Abscissa value of evaluation.
      real(wp), intent(out) :: p  !! Input expansion evaluated at X.
      real(wp), intent(out) :: itgrlp  !! Integral evaluated at X.

!     Local variables
!
      real(wp) :: a2
      real(wp) :: adegp1
      real(wp) :: adegp2
      real(wp) :: ai
      real(wp) :: c0
      real(wp) :: f      ( 3 )
      real(wp) :: s
      real(wp) :: s2
      real(wp) :: w      ( 3 )
      real(wp) :: z      ( 3 )

      integer(ip) :: i
      integer(ip) :: nterms

!
!     Test RETURN but don't check in.
!
      if ( ctx%failed() ) return

!
!     Check the expansion degree.
!
      if ( degp < 0 )  then

         call ctx%setmsg ( 'Expansion degree must be non-negative ' &
         //            'but was #.'                            )
         call ctx%errint ( '#', degp                               )
         call ctx%sigerr ( 'SPICE(INVALIDDEGREE)'                  )
         return

      end if

!
!     Check the radius of the domain interval.
!
      if ( x2s(2) <= 0.0_wp )  then

         call ctx%setmsg ( 'Interval radius must be positive ' &
         //            'but was #.'                        )
         call ctx%errdp  ( '#', x2s(2)                         )
         call ctx%sigerr ( 'SPICE(INVALIDRADIUS)'              )
         return

      end if

      nterms = degp + 1

!
!     Background
!     ==========
!
!     Let
!
!        T ,  n = 0, ...
!         n
!
!     represent the nth Chebyshev polynomial of the first kind:
!
!        T (x) = cos( n*arccos(x) )
!         n
!
!     These polynomials satisfy the recurrence relationship
!
!        T   (x) = 2x T (x)  -  T   (x),  n = 2, ...
!         n+1          n         n-1
!
!     The Chebyshev polynomials of the second kind are denoted by
!
!        U ,  n = 0, ...
!         n
!
!     where
!
!        U (x) = 1
!         0
!
!        U (x) = 2x
!         1
!
!        U   (x) = 2x U (x)  -  U   (x),  n = 2, ...
!         n+1          n         n-1
!
!     The integration formula (1) below is based on several
!     identities:
!
!        T (x)        = (1/2) * ( U (x) - U   (x) ),  n = 2, ...    (B1)
!         n                        n       n-2
!
!        d(T (x))/dx  =  n U   (x),   n = 1, ...                    (B2)
!           n               n-1
!
!                                 d(T   (x))/dx   d(T   (x))/dx
!                                    n+1             n-1
!        T (x)        = (1/2) * ( ------------- - ------------- ),
!         n                             n+1             n-1
!
!                                     n = 2, ...                    (B3)
!
!     Identity (B1) can be proved via mathematical induction. Using
!     (B1) and the Chebyshev recurrence formulas for both kinds of
!     polynomials, identity (B2) can also be proved via mathematical
!     induction. Identity (B3) follows directly from the combination of
!     (B1) and (B2).
!
!     Formula (1) below follows from (B3).
!
!     Algorithm
!     =========
!
!     In the discussion below, all Chebyshev polynomials are of the
!     first kind.
!
!     Let the notation
!
!        I( f )
!
!     represent the indefinite integral of a function f.
!
!     The key formula we use below is
!
!                              T          T
!                               n+1        n-1
!        I ( T  ) = (1/2) * ( ------  -  ------ )  +  C,  for n > 1  (1)
!             n                 n+1        n-1
!
!     where C is a constant of integration. Applying (1) to a Chebyshev
!     expansion
!
!               N+1
!               __
!               \
!        f(x) = /  a  T   (x)                                        (2)
!               --  k  k-1
!               k=1
!
!     we have
!
!                               N+1
!                               __       T (x)    T   (x)
!                               \         k        k-2
!        I( f(x) ) =      (1/2) /  a (   ------ - -------  )
!                               --  k       k       k-2
!                               k=3
!
!                    + a T (x) + ( a  / 4 ) T (x) + C ,   for N > 1 (3a)
!                       1 1         2        2       0
!
!     or
!
!        I( f(x) ) = C  + a T (x),   for N = 1                      (3b)
!                     0    1 1
!
!     where
!
!        C
!         0
!
!     is a constant of integration. Then by grouping coefficients of
!     the Chebyshev polynomials, we have
!
!                           N+2
!                           __
!                           \
!        I( f(x) ) = C  +   /  A  T   (x),  for N >= 0               (4)
!                     0     --  k  k-1
!                           k=2
!
!     where
!
!        A    = a   -  (1/2)a           for N >= 2,    or
!         2      1           3
!
!        A    = a                       for N <= 1                  (5a)
!         2      1
!
!                 1
!        A    = ------ ( a  -  a    )   for k = 3, ... , N (N>=3)   (5b)
!         k     2(k-1)    k-1   k+1
!
!                1
!        A    = ---- a                  for N >= 2                  (5c)
!         N+1    2N   N
!
!                 1
!        A    = ------ a                for N >= 1                  (5d)
!         N+2   2(N+1)  N+1
!
!     Note that (5b) does not conflict with (5c) or (5d) for N < 3,
!     since (5b) is applicable only when N >= 3.
!
!     We'll compute the sums
!
!                           N+2
!                           __
!                           \
!        I( f(x) ) - C  =   /  A  T   (x)                            (6)
!                     0     --  k  k-1
!                           k=2
!
!               N+2
!               __
!               \
!        C  = - /  A  T   (0)                                        (7)
!         0     --  k  k-1
!               k=2
!
!     and
!
!               N+1
!               __
!               \
!        f(x) = /  a  T   (x)
!               --  k  k-1
!               k=1
!
!     using Clenshaw's recurrence formula. Note that in the above
!     equations, N is the degree of the input expansion, which is
!     given by the input argument DEGP.
!
!     Transform the independent variable X to the interval
!
!        [-1, 1]
!
!     Call the result S.
!
!     Note we've already checked that X2S(2) is positive.
!
      s     = (x-x2s(1)) / x2s(2)
      s2    = 2.0_wp * s

!
!     Pre-compute the coefficients of the integral expansion
!     that are known at this point. The terms A2, ADEGP1, and
!     ADEGP2 correspond to the variables
!
!        A , A   , A
!         2   N+1   N+2
!
!     above and are set according to equations 5a, 5c, and 5d.
!
      if ( nterms >= 3 ) then

         a2 = cp(1) - 0.5_wp*cp(3)
      else
         a2 = cp(1)
      end if

!
!     Initialize the two highest-indexed coefficients of
!     the integral expansion.
!
      adegp1 = 0.0_wp
      adegp2 = 0.0_wp

      if ( degp >= 2 ) then

         adegp1 = 0.5_wp * cp(degp) / degp

      end if

      if ( degp >= 1 ) then

         adegp2 = 0.5_wp * cp(degp+1) / (degp+1)

      end if

!
!     The three quantities we'll compute require different numbers of
!     loop iterations: the integrals at X and at 0 require that I be
!     initialized to DEGP+2, while the input expansion requires that I
!     be initialized to DEGP+1. Since we wish to save on loop overhead
!     by performing the respective loop body actions in parallel, we
!     perform the integral computations for I = DEGP+2 prior to the
!     start of the loop.
!
!     Since F(1) and F(2) would normally be initialized to 0 prior to
!     the start of the loop, after the first loop pass, F would have
!     the contents assigned below.
!
      f(3) = 0.0_wp
      f(2) = 0.0_wp
!
!     The initial value of F(1) is the highest-order coefficient of
!     the integral's expansion.
!
      if ( degp == 0 ) then
         f(1) = a2
      else
         f(1) = adegp2
      end if
!
!     We also want to evaluate [I(f)](0); we'll use this to
!     make the integral 0 at x = 0. We'll use the terms
!     Z(*) to evaluate the expansion of the integral at x = 0.
!
      z(3) = 0.0_wp
      z(2) = 0.0_wp
      z(1) = f(1)
!
!     We'll use the terms W(*) to evaluate the input expansion.
!
      w(1) = 0.0_wp
      w(2) = 0.0_wp

!
!     NTERMS is DEGP+1.
!
      i = nterms

      do while ( i > 1 )
!
!        The variable AI represents A(I), which is the Ith coefficient
!        of the Chebyshev expansion of the indefinite integral. AI is
!        set according to formulas 5a-c above.
!
         if ( i == 2 ) then

            ai = a2

         else if ( i < nterms ) then
!
!           AI is A ; I >= 3; DEGP >= 3.
!                  I
!
            ai = 0.5_wp * ( cp(i-1) - cp(i+1) ) / (i-1)

         else
!
!           AI is A      ; I = NTERMS.
!                  DEGP+1
!
            ai = adegp1

         end if

         f(3) = f(2)
         f(2) = f(1)
         f(1) = ai     + ( s2*f(2)  - f(3) )

         z(3) = z(2)
         z(2) = z(1)
         z(1) = ai     - z(3)

         w(3) = w(2)
         w(2) = w(1)
         w(1) = cp(i)  + ( s2*w(2)  - w(3) )

         i    = i - 1

      end do

!
!     C0 is the negative of the input expansion evaluated at x=0, and
!     that expansion value is
!
!        0*Z(1) - Z(2)
!
!     The other terms are computed as in CHBINT.
!
      c0      =                    z(2)
      itgrlp  = c0    +   s*f(1) - f(2)
      p       = cp(1) + ( s*w(1) - w(2) )

!
!     Scale the integral to account for the change of variables
!     (from the original domain to [-1,1]). The scale factor is
!
!        dX/dS = X2S(2)
!
      itgrlp = x2s(2) * itgrlp

      return
      end subroutine chbigr

      !> Find the intersection of an ellipse and a plane.
      !>
      !>## Detailed Input
      !>
      !>     ELLIPS   is a SPICE ellipse. The ellipse is allowed to
      !>              be degenerate: one or both semi-axes may have
      !>              zero length.
      !>
      !>     PLANE    is a SPICE plane.
      !>
      !>## Detailed Output
      !>
      !>     NXPTS    is the number of points of intersection of the
      !>              geometric plane and ellipse represented by PLANE
      !>              and ELLIPS. NXPTS may take the values 0, 1, 2 or
      !>              -1. The value -1 indicates that the ellipse
      !>              consists of more than one point and lies in the
      !>              plane, so the number of intersection points is
      !>              infinite.
      !>
      !>              When the ellipse consists of a single point and
      !>              lies in the plane, NXPTS is set to 1.
      !>
      !>     XPT1,
      !>     XPT2     are the points of intersection of the input plane
      !>              and ellipse. If there is only one intersection
      !>              point, both XPT1 and XPT2 contain that point. If
      !>              the number of intersection points is zero or
      !>              infinite, the contents of XPT1 and XPT2 are
      !>              undefined.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input plane is invalid, the error SPICE(INVALIDPLANE)
      !>         is signaled. The input plane must be a SPICE plane: the normal
      !>         vector must be non-zero and the constant must be non-negative.
      !>
      !>     2)  If the input ellipse has non-orthogonal axes, the error
      !>         SPICE(INVALIDELLIPSE) is signaled.
      !>
      !>     3)  The input ellipse is allowed to be a line segment or a point;
      !>         these cases are not considered to be errors. If the ellipse
      !>         consists of a single point and lies in the plane, the number
      !>         of intersection points is set to 1 (rather than -1) and
      !>         the output arguments XPT1 and XPT2 are assigned the value
      !>         of the ellipse's center.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the intersection set of a non-degenerate
      !>     plane with a possibly degenerate ellipse. The ellipse is allowed
      !>     to consist of a line segment or a point.
      !>
      !>     A plane may intersect an ellipse in 0, 1, 2, or infinitely many
      !>     points. For there to be an infinite set of intersection points,
      !>     the ellipse must lie in the plane and consist of more than one
      !>     point.
      !>
      !>## Examples
      !>
      !>     1)  If we want to find the angle of some ray above the limb of an
      !>         ellipsoid, where the angle is measured in a plane containing
      !>         the ray and a `down' vector, we can follow the procedure
      !>         given below. We assume the ray does not intersect the
      !>         ellipsoid. The result we seek is called ANGLE, imaginatively
      !>         enough.
      !>
      !>         We assume that all vectors are given in body-fixed
      !>         coordinates.
      !>
      !>```fortran
      !>            C
      !>            C     Find the limb of the ellipsoid as seen from the
      !>            C     point OBSERV. Here A, B, and C are the lengths of
      !>            C     the semi-axes of the ellipsoid.
      !>            C
      !>                  CALL EDLIMB ( A, B, C, OBSERV, LIMB )
      !>
      !>            C
      !>            C     The ray direction vector is RAYDIR, so the ray is the
      !>            C     set of points
      !>            C
      !>            C        OBSERV  +  t * RAYDIR
      !>            C
      !>            C     where t is any non-negative real number.
      !>            C
      !>            C     The `down' vector is just -OBSERV. The vectors
      !>            C     OBSERV and RAYDIR are spanning vectors for the plane
      !>            C     we're interested in. We can use PSV2PL to represent
      !>            C     this plane by a SPICE plane.
      !>            C
      !>                  CALL PSV2PL ( OBSERV, OBSERV, RAYDIR, PLANE )
      !>
      !>            C
      !>            C     Find the intersection of the plane defined by OBSERV
      !>            C     and RAYDIR with the limb.
      !>            C
      !>                  CALL INELPL ( LIMB, PLANE, NXPTS, XPT1, XPT2 )
      !>
      !>            C
      !>            C     We always expect two intersection points, if DOWN
      !>            C     is valid.
      !>            C
      !>                  IF ( NXPTS .LT. 2 ) THEN
      !>
      !>                     [ do something about the error ]
      !>
      !>                  ENDIF
      !>
      !>            C
      !>            C     Form the vectors from OBSERV to the intersection
      !>            C     points. Find the angular separation between the
      !>            C     boresight ray and each vector from OBSERV to the
      !>            C     intersection points.
      !>            C
      !>                  CALL VSUB   ( XPT1, OBSERV, VEC1 )
      !>                  CALL VSUB   ( XPT2, OBSERV, VEC2 )
      !>
      !>                  SEP1 = VSEP ( VEC1, RAYDIR )
      !>                  SEP2 = VSEP ( VEC2, RAYDIR )
      !>
      !>            C
      !>            C     The angular separation we're after is the minimum of
      !>            C     the two separations we've computed.
      !>            C
      !>                  ANGLE = MIN ( SEP1, SEP2 )
      !>```
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 07-OCT-2011 (NJB)
      !>
      !>        Relaxed ellipse semi-axes orthogonality test limit
      !>        SEPLIM from 1.D-12 TO 1.D-9 radians. The angular
      !>        separation of the axes of the input ellipse must not
      !>        differ from pi/2 radians by more than this limit.
      !>
      !>-    SPICELIB Version 2.0.0, 14-JAN-2008 (NJB)
      !>
      !>        Bug fix: the routine's specification and behavior have been
      !>        updated so the routine now returns a meaningful result for the
      !>        case of an ellipse consisting of a single point.
      !>
      !>        Bug fix: in the degenerate case where the input ellipse is a
      !>        line segment of positive length, and this segment intersects
      !>        the plane, the number of intersection points is set to 1
      !>        rather than 2.
      !>
      !>        Invalid input planes and ellipses are now diagnosed.
      !>
      !>-    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSUB call.
      !>
      !>-    SPICELIB Version 1.1.0, 24-MAR-1992 (NJB) (WLT)
      !>
      !>        Output arguments XPT1, XPT2 are now correctly declared
      !>        with length 3. Comment section for permuted index source
      !>        lines was added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 14-JAN-2008 (NJB)
      !>
      !>        Bug fix: the routine's specification and behavior have been
      !>        updated so the routine now returns a meaningful result for the
      !>        case of an ellipse consisting of a single point. In this case,
      !>        if an intersection is found, the number of intersection points
      !>        is set to 1 and both intersection arguments are set equal to
      !>        the ellipse's center.
      !>
      !>        Bug fix: in the degenerate case where the input ellipse is a
      !>        line segment of positive length, and this segment intersects
      !>        the plane, the number of intersection points is set to 1
      !>        rather than 2.
      !>
      !>        Invalid input planes and ellipses are now diagnosed.
      !>        Error handling code has been added to trap errors that had
      !>        been erroneously passed off to lower level routines for
      !>        diagnosis.
      module subroutine inelpl ( ctx, ellips, plane, nxpts, xpt1, xpt2 )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: halfpi
      use pepper_math, only: el2cgv, pl2nvc, pl2nvp, vdot, vnormg, vsep
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: ellips ( ubel )  !! A SPICE ellipse.
      real(wp), intent(in) :: plane  ( ubpl )  !! A SPICE plane.
      integer(ip), intent(out) :: nxpts  !! Number of intersection points of plane and ellipse.
      real(wp), intent(out) :: xpt1   ( 3 )  !! Intersection points.
      real(wp), intent(out) :: xpt2   ( 3 )  !! Intersection points.


      real(wp), parameter :: seplim = 1.0e-9_wp

      real(wp) :: alpha
      real(wp) :: angle1
      real(wp) :: angle2
      real(wp) :: beta
      real(wp) :: center (    3 )
      real(wp) :: const
      real(wp) :: inpcon
      real(wp) :: normal (    3 )
      real(wp) :: point  (    3 )
      real(wp) :: sep
      real(wp) :: smajor (    3 )
      real(wp) :: sminor (    3 )
      real(wp) :: tmpvec (    3 )
      real(wp) :: trans  ( ubpl )
      real(wp) :: v      (    2 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check the input plane.
!
      call pl2nvc ( plane, normal, inpcon )

      if ( all(normal == 0.0_wp) ) then

         call ctx%setmsg ( 'Input SPICE plane has zero normal vector.' )
         call ctx%sigerr ( 'SPICE(INVALIDPLANE)'                       )
         return

      else if ( inpcon < 0.0_wp ) then

         call ctx%setmsg ( 'Input SPICE plane has non-positive ' &
         //            'constant #. Properly constructed ' &
         //            'SPICE planes always have non-negative ' &
         //            'constants.'                                )
         call ctx%errdp  ( '#',  inpcon                                )
         call ctx%sigerr ( 'SPICE(INVALIDPLANE)'                       )
         return

      end if

!
!     Get the components of the input ellipse; check for
!     invalid semi-axes. The semi-axes may have zero length
!     but they must always be orthogonal. We require this
!     check only if both semi-axes have non-zero length.
!
      call el2cgv ( ellips, center, smajor, sminor )

      if ( .not. all(sminor == 0.0_wp) ) then

         sep = vsep( smajor, sminor )

         if (  abs( sep - halfpi )  >  seplim  ) then

            call ctx%setmsg ( 'Input SPICE ellipse has non-orthogonal ' &
            //            'semi-axes: (#,#,#) and (#,#,#). Angular ' &
            //            'separation of these vectors is # radians. ' &
            //            'Properly constructed SPICE ellipses ' &
            //            'always have orthogonal semi-axes.'         )
            call ctx%errdp  ( '#',  smajor(1)                             )
            call ctx%errdp  ( '#',  smajor(2)                             )
            call ctx%errdp  ( '#',  smajor(3)                             )
            call ctx%errdp  ( '#',  sminor(1)                             )
            call ctx%errdp  ( '#',  sminor(2)                             )
            call ctx%errdp  ( '#',  sminor(3)                             )
            call ctx%errdp  ( '#',  sep                                   )
            call ctx%sigerr ( 'SPICE(INVALIDELLIPSE)'                     )
            return

         end if

      end if

!
!     If the input ellipse is a single point, decide now
!     whether the ellipse lies in the plane.
!
      if ( all(smajor == 0.0_wp) ) then
!
!        The ellipse is a single point. If the ellipse's center
!        lies in the plane, the whole ellipse is the one
!        intersection point. Check the inner product of the
!        center and the plane's normal vector.
!
         if (  vdot( center, normal )  ==  inpcon  ) then
!
!           The center does in fact lie in the plane.
!
            nxpts = 1

            xpt1 = center
            xpt2 = center

         else
!
!           There's no intersection: the intersection arguments
!           are left undefined in this case.
!
            nxpts = 0

         end if

!
!        Return now; this simplifies the logic to follow.
!
         return

      end if

!
!     At this point the ellipse may still be degenerate: it can be a
!     line segment. We'll need to compute the intersection point or
!     points if we have a positive, finite intersection set.
!
!     The first thing we want to do is translate the plane and the
!     ellipse so as to center the ellipse at the origin.  To translate
!     the plane, just get a point and normal vector, and translate
!     the point.  Find the plane constant of the translated plane.
!
      call pl2nvp ( plane,   normal,  tmpvec )
      point = tmpvec - center
      call nvp2pl ( ctx, normal,  point,   trans  )

      call pl2nvc ( trans,   normal,  const  )

!
!     Ok, we can get to work.  The locus of the ellipse is
!
!        cos(theta) SMAJOR  +  sin(theta) SMINOR,
!
!     and any point X of the ellipse that intersects the input plane
!     satisfies
!
!        < X, NORMAL >  =  CONST.
!
!     Substituting our expression for points on the ellipse into the
!     second equation, we arrive at
!
!           cos(theta) < SMAJOR, NORMAL >
!        +  sin(theta) < SMINOR, NORMAL >   =  CONST.        (1)
!
!     This equation merits a little analysis. First, if NORMAL
!     is orthogonal to SMAJOR and SMINOR, the plane and ellipse must
!     be parallel. Also, the left side of the equation is zero in
!     this case. If CONST is non-zero, there are no solutions:
!     the ellipse and plane are parallel but do not intersect. If
!     CONST is zero, the ellipse lies in the plane: all values of
!     theta are solutions. Let's get this case out of the way
!     right now, shall we?
!
      v(1) = vdot ( smajor, normal )
      v(2) = vdot ( sminor, normal )

!
!     Test whether the plane and ellipse are parallel:
!
      if (  all(v == 0.0_wp)  ) then
!
!        The ellipse lies in the plane if and only if CONST is zero.
!        In any case, we don't modify XPT1 or XPT2.
!
         if ( const == 0.0_wp ) then
            nxpts = -1
         else
            nxpts = 0
         end if

         return

      end if

!
!     Now if NORMAL is not orthogonal to both SMAJOR and SMINOR,
!     the vector
!
!        V = (  < SMAJOR, NORMAL >,  < SMINOR, NORMAL >  )
!
!     is non-zero.  We can re-write (1) as
!
!        < U, V >  =  CONST,
!
!     where
!
!        U = ( cos(theta), sin(theta) ).
!
!     If alpha is the angle between U and V, we have
!
!        < U, V >  =  || U ||  *  || V ||  *  cos(alpha),
!
!     so
!
!        || V ||  *  cos(alpha)  =  CONST.                   (2)
!
!     CONST is positive, since PL2NVC returns the distance
!     of between its input plane and the origin as the output
!     plane constant.
!
!     Equation (2) has solutions if and only if
!
!        || V ||  >    CONST.                                (3)
!                 -
!
!     Let's return right now if there are no solutions.
!
      if (  vnormg( v, 2 )  <  const  )   then

         nxpts = 0
         return

      end if

!
!     Since (3) above is satisfied, the plane and ellipse intersect.
!     We can find alpha using the formula
!
!        alpha  =  +  arccos (  CONST  /  || V ||  )
!
!     Since alpha is the angular separation between U and V, we
!     can find U once we have the angular position of V; let's
!     call that beta.  The angular position of U (which we called
!     theta earlier) will be
!
!        theta   =   beta  +  alpha.
!                          -
!
!     The values of theta are the angles we seek.
!

      alpha   =  acos  (  const  /  vnormg ( v, 2 )  )

      beta    =  atan2 ( v(2), v(1) )

      angle1  =  beta - alpha
      angle2  =  beta + alpha

!
!     Determine the number of intersection points. We have a special
!     case if the semi-minor axis has length zero: in that case BETA is
!     zero or Pi, and although ANGLE1 and ANGLE2 may differ, the
!     cosines of these angles are identical. Since in this case
!     the solutions corresponding to ANGLE1 and ANGLE2 have the
!     form
!
!        CENTER + cos(ANGLE1)*SMAJOR
!        CENTER + cos(ANGLE2)*SMAJOR
!
!     the solutions are identical.
!
      if ( all(sminor == 0.0_wp) ) then

         nxpts = 1

      else

         if ( angle1 == angle2 ) then
!
!           This case occurs when ALPHA is zero.
!
            nxpts = 1
         else
            nxpts = 2
         end if

      end if

!
!     Compute the intersection points.
!
      xpt1 = 1.0_wp*center + dcos(angle1)*smajor + dsin(angle1)*sminor

      xpt2 = 1.0_wp*center + dcos(angle2)*smajor + dsin(angle2)*sminor

      return
      end subroutine inelpl

      !> Find the intersection of a ray and a plane.
      !>
      !>## Detailed Input
      !>
      !>     VERTEX,
      !>     DIR      are a point and direction vector that define a
      !>              ray in three-dimensional space.
      !>
      !>     PLANE    is a SPICE plane.
      !>
      !>## Detailed Output
      !>
      !>     NXPTS    is the number of points of intersection of the
      !>              input ray and plane. Values and meanings of
      !>              NXPTS are:
      !>
      !>                 0     No intersection.
      !>
      !>                 1     One point of intersection. Note that
      !>                       this case may occur when the ray's
      !>                       vertex is in the plane.
      !>
      !>                -1     An infinite number of points of
      !>                       intersection; the ray lies in the plane.
      !>
      !>     XPT      is the point of intersection of the input ray
      !>              and plane, when there is exactly one point of
      !>              intersection. Otherwise, XPT is the zero vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the ray's direction vector is the zero vector, the error
      !>         SPICE(ZEROVECTOR) is signaled. NXPTS and XPT are not
      !>         modified.
      !>
      !>     2)  If the ray's vertex is further than DPMAX / 3 from the
      !>         origin, the error SPICE(VECTORTOOBIG) is signaled.  NXPTS
      !>         and XPT are not modified.
      !>
      !>     3)  If the input plane is further than DPMAX / 3 from the
      !>         origin, the error SPICE(VECTORTOOBIG) is signaled.  NXPTS
      !>         and XPT are not modified.
      !>
      !>     4)  The input plane should be created by one of the SPICELIB
      !>         routines
      !>
      !>            NVC2PL
      !>            NVP2PL
      !>            PSV2PL
      !>
      !>         Invalid input planes will cause unpredictable results.
      !>
      !>     5)  In the interest of good numerical behavior, in the case
      !>         where the ray's vertex is not in the plane, this routine
      !>         considers that an intersection of the ray and plane occurs
      !>         only if the distance between the ray's vertex and the
      !>         intersection point is less than DPMAX / 3.
      !>
      !>         If VERTEX is not in the plane and this condition is not
      !>         met, then NXPTS is set to 0 and XPT is set to the zero
      !>         vector.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The intersection of a ray and plane in three-dimensional space
      !>     can be a the empty set, a single point, or the ray itself.
      !>
      !>## Examples
      !>
      !>     1)  Find the camera projection of the center of an extended
      !>         body. For simplicity, we assume:
      !>
      !>            -- The camera has no distortion;  the image of a point
      !>               is determined by the intersection of the focal plane
      !>               and the line determined by the point and the camera's
      !>               focal point.
      !>
      !>            -- The camera's pointing matrix (C-matrix) is available
      !>               in a C-kernel.
      !>
      !>            C
      !>            C     Load Leapseconds and SCLK kernels to support time
      !>            C     conversion.
      !>            C
      !>                  CALL FURNSH ( 'LEAP.KER' )
      !>                  CALL FURNSH ( 'SCLK.KER' )
      !>
      !>            C
      !>            C     Load an SPK file containing ephemeris data for
      !>            C     observer (a spacecraft, whose NAIF integer code
      !>            C     is SC) and target at the UTC epoch of observation.
      !>            C
      !>                  CALL FURNSH ( 'SPK.BSP' )
      !>
      !>            C
      !>            C     Load a C-kernel containing camera pointing for
      !>            C     the UTC epoch of observation.
      !>            C
      !>                  CALL FURNSH ( 'CK.BC' )
      !>
      !>            C
      !>            C     Find the ephemeris time (barycentric dynamical time)
      !>            C     and encoded spacecraft clock times corresponding to
      !>            C     the UTC epoch of observation.
      !>            C
      !>                  CALL UTC2ET ( UTC, ET          )
      !>                  CALL SCE2C  ( SC,  ET,  SCLKDP )
      !>
      !>            C
      !>            C     Encode the pointing lookup tolerance.
      !>            C
      !>                  CALL SCTIKS ( SC, TOLCH,  TOLDP  )
      !>
      !>            C
      !>            C     Find the observer-target vector at the observation
      !>            C     epoch. In this example, we'll use a light-time
      !>            C     corrected state vector.
      !>            C
      !>                  CALL SPKEZ ( TARGET,  ET,  'J2000',  'LT',  SC,
      !>                 .             STATE,   LT                        )
      !>
      !>            C
      !>            C     Look up camera pointing.
      !>            C
      !>                  CALL CKGP  ( CAMERA, SCLKDP, TOLDP, 'J2000', CMAT,
      !>                 .             CLKOUT, FOUND                        )
      !>
      !>                  IF ( .NOT. FOUND ) THEN
      !>
      !>                     [Handle this case...]
      !>
      !>                  END IF
      !>
      !>            C
      !>            C     Negate the spacecraft-to-target body vector and
      !>            C     convert it to camera coordinates.
      !>            C
      !>                  CALL VMINUS ( STATE, DIR       )
      !>                  CALL MXV    ( CMAT,  DIR,  DIR )
      !>
      !>            C
      !>            C     If FL is the camera's focal length, the effective
      !>            C     focal point is
      !>            C
      !>            C        FL * ( 0, 0, 1 )
      !>            C
      !>                  CALL VSCL ( FL, ZVEC, FOCUS )
      !>
      !>            C
      !>            C     The camera's focal plane contains the origin in
      !>            C     camera coordinates, and the z-vector is orthogonal
      !>            C     to the plane.  Make a SPICE plane representing
      !>            C     the focal plane.
      !>            C
      !>                  CALL NVC2PL ( ZVEC, 0.D0, FPLANE )
      !>
      !>            C
      !>            C     The image of the target body's center in the focal
      !>            C     plane is defined by the intersection with the focal
      !>            C     plane of the ray whose vertex is the focal point and
      !>            C     whose direction is DIR.
      !>            C
      !>                  CALL INRYPL ( FOCUS, DIR, FPLANE, NXPTS, IMAGE )
      !>
      !>                  IF ( NXPTS .EQ. 1 ) THEN
      !>            C
      !>            C        The body center does project to the focal plane.
      !>            C        Check whether the image is actually in the
      !>            C        camera's field of view...
      !>            C
      !>                               .
      !>                               .
      !>                               .
      !>                  ELSE
      !>
      !>            C
      !>            C        The body center does not map to the focal plane.
      !>            C        Handle this case...
      !>            C
      !>                               .
      !>                               .
      !>                               .
      !>                  END IF
      !>
      !>     2)  Find the Saturn ring plane intercept of a spacecraft-mounted
      !>         instrument's boresight vector.  We want the find the point
      !>         in the ring plane that will be observed by an instrument
      !>         with a give boresight direction at a specified time.  We
      !>         must account for light time and stellar aberration in order
      !>         to find this point.  The intercept point will be expressed
      !>         in Saturn body-fixed coordinates.
      !>
      !>         In this example, we assume
      !>
      !>            -- The ring plane is equatorial.
      !>
      !>            -- Light travels in a straight line.
      !>
      !>            -- The light time correction for the ring plane intercept
      !>               can be obtained by performing three light-time
      !>               correction iterations.  If this assumption does not
      !>               lead to a sufficiently accurate result, additional
      !>               iterations can be performed.
      !>
      !>            -- A Newtonian approximation of stellar aberration
      !>               suffices.
      !>
      !>            -- The boresight vector is given in J2000 coordinates.
      !>
      !>            -- The observation epoch is ET ephemeris seconds past
      !>               J2000.
      !>
      !>            -- The boresight vector, spacecraft and planetary
      !>               ephemerides, and ring plane orientation are all known
      !>               with sufficient accuracy for the application.
      !>
      !>            -- All necessary kernels are loaded by the caller of
      !>               this example routine.
      !>
      !>            SUBROUTINE RING_XPT ( SC, ET, BORVEC, SBFXPT, FOUND )
      !>            IMPLICIT NONE
      !>
      !>            CHARACTER*(*)         SC
      !>            DOUBLE PRECISION      ET
      !>            DOUBLE PRECISION      BORVEC ( 3 )
      !>            DOUBLE PRECISION      SBFXPT ( 3 )
      !>            LOGICAL               FOUND
      !>
      !>      C
      !>      C     SPICELIB functions
      !>      C
      !>            DOUBLE PRECISION      CLIGHT
      !>            DOUBLE PRECISION      VDIST
      !>
      !>      C
      !>      C     Local parameters
      !>      C
      !>            INTEGER               UBPL
      !>            PARAMETER           ( UBPL = 4 )
      !>
      !>            INTEGER               SATURN
      !>            PARAMETER           ( SATURN = 699 )
      !>
      !>      C
      !>      C     Local variables
      !>      C
      !>            DOUBLE PRECISION      BORV2  ( 3 )
      !>            DOUBLE PRECISION      CORVEC ( 3 )
      !>            DOUBLE PRECISION      LT
      !>            DOUBLE PRECISION      PLANE  ( UBPL )
      !>            DOUBLE PRECISION      SATSSB ( 6 )
      !>            DOUBLE PRECISION      SCPOS  ( 3 )
      !>            DOUBLE PRECISION      SCSSB  ( 6 )
      !>            DOUBLE PRECISION      STATE  ( 6 )
      !>            DOUBLE PRECISION      STCORR ( 3 )
      !>            DOUBLE PRECISION      TAU
      !>            DOUBLE PRECISION      TPMI   ( 3,  3 )
      !>            DOUBLE PRECISION      XPT    ( 3 )
      !>            DOUBLE PRECISION      ZVEC   ( 3 )
      !>
      !>            INTEGER               I
      !>            INTEGER               NXPTS
      !>            INTEGER               SCID
      !>
      !>            LOGICAL               FND
      !>
      !>      C
      !>      C     First step:  account for stellar aberration.  Since the
      !>      C     instrument pointing is given, we need to find the intercept
      !>      C     point such that, when the stellar aberration correction is
      !>      C     applied to the vector from the spacecraft to that point,
      !>      C     the resulting vector is parallel to BORVEC.  An easy
      !>      C     solution is to apply the inverse of the normal stellar
      !>      C     aberration correction to BORVEC, and then solve the
      !>      C     intercept problem with this corrected boresight vector.
      !>      C
      !>      C     Find the position of the observer relative
      !>      C     to the solar system barycenter at ET.
      !>      C
      !>            CALL BODN2C ( SC, SCID, FND )
      !>
      !>            IF ( .NOT. FND ) THEN
      !>
      !>               CALL SETMSG ( 'ID code for body # was not found.' )
      !>               CALL ERRCH  ( '#',  SC                            )
      !>               CALL SIGERR ( 'SPICE(NOTRANSLATION'               )
      !>               RETURN
      !>
      !>            END IF
      !>
      !>            CALL SPKSSB ( SCID, ET, 'J2000', SCSSB )
      !>
      !>      C
      !>      C     We now wish to find the vector CORVEC that, when
      !>      C     corrected for stellar aberration, yields BORVEC.
      !>      C     A good first approximation is obtained by applying
      !>      C     the stellar aberration correction for transmission
      !>      C     to BORVEC.
      !>      C
      !>            CALL STLABX ( BORVEC, SCSSB(4), CORVEC )
      !>
      !>      C
      !>      C     The inverse of the stellar aberration correction
      !>      C     applicable to CORVEC should be a very good estimate of
      !>      C     the correction we need to apply to BORVEC. Apply
      !>      C     this correction to BORVEC to obtain an improved estimate
      !>      C     of CORVEC.
      !>      C
      !>            CALL STELAB ( CORVEC, SCSSB(4), BORV2  )
      !>            CALL VSUB   ( BORV2,  CORVEC,   STCORR )
      !>            CALL VSUB   ( BORVEC, STCORR,   CORVEC )
      !>
      !>      C
      !>      C     Because the ring plane intercept may be quite far from
      !>      C     Saturn's center, we cannot assume light time from the
      !>      C     intercept to the observer is well approximated by
      !>      C     light time from Saturn's center to the observer.
      !>      C     We compute the light time explicitly using an iterative
      !>      C     approach.
      !>      C
      !>      C     We can however use the light time from Saturn's center to
      !>      C     the observer to obtain a first estimate of the actual light
      !>      C     time.
      !>      C
      !>            CALL SPKEZR ( 'SATURN', ET, 'J2000', 'LT', SC,
      !>           .               STATE,   LT                       )
      !>            TAU = LT
      !>
      !>      C
      !>      C     Find the ring plane intercept and calculate the
      !>      C     light time from it to the spacecraft.
      !>      C     Perform three iterations.
      !>      C
      !>            I     = 1
      !>            FOUND = .TRUE.
      !>
      !>            DO WHILE (  ( I .LE. 3 )  .AND.  ( FOUND )  )
      !>      C
      !>      C        Find the position of Saturn relative
      !>      C        to the solar system barycenter at ET-TAU.
      !>      C
      !>               CALL SPKSSB ( SATURN, ET-TAU, 'J2000', SATSSB )
      !>
      !>      C
      !>      C        Find the Saturn-to-observer vector defined by these
      !>      C        two position vectors.
      !>      C
      !>               CALL VSUB ( SCSSB, SATSSB, SCPOS )
      !>
      !>      C
      !>      C        Look up Saturn's pole at ET-TAU; this is the third
      !>      C        column of the matrix that transforms Saturn body-fixed
      !>      C        coordinates to J2000 coordinates.
      !>      C
      !>               CALL PXFORM ( 'IAU_SATURN', 'J2000', ET-TAU, TPMI )
      !>
      !>               CALL MOVED  ( TPMI(1,3), 3, ZVEC )
      !>
      !>      C
      !>      C        Make a SPICE plane representing the ring plane.
      !>      C        We're treating Saturn's center as the origin, so
      !>      C        the plane constant is 0.
      !>      C
      !>               CALL NVC2PL ( ZVEC, 0.D0, PLANE )
      !>
      !>      C
      !>      C        Find the intersection of the ring plane and the
      !>      C        ray having vertex SCPOS and direction vector
      !>      C        CORVEC.
      !>      C
      !>               CALL INRYPL ( SCPOS, CORVEC, PLANE, NXPTS, XPT )
      !>
      !>      C
      !>      C        If the number of intersection points is 1,
      !>      C        find the next light time estimate.
      !>      C
      !>               IF ( NXPTS .EQ. 1 ) THEN
      !>      C
      !>      C           Find the light time (zero-order) from the
      !>      C           intercept point to the spacecraft.
      !>      C
      !>                  TAU  =  VDIST ( SCPOS, XPT )  /  CLIGHT()
      !>                  I    =  I + 1
      !>
      !>               ELSE
      !>
      !>                  FOUND = .FALSE.
      !>
      !>               END IF
      !>
      !>            END DO
      !>
      !>      C
      !>      C     At this point, if FOUND is .TRUE., we iterated
      !>      C     3 times, and XPT is our estimate of the
      !>      C     position of the ring plane intercept point
      !>      C     relative to Saturn in the J2000 frame. This is the
      !>      C     point observed by an instrument pointed in direction
      !>      C     BORVEC at ET at mounted on the spacecraft SC.
      !>      C
      !>      C     If FOUND is .FALSE., the boresight ray does not
      !>      C     intersect the ring plane.
      !>      C
      !>      C     As a final step, transform XPT to Saturn body-fixed
      !>      C     coordinates.
      !>      C
      !>            IF ( FOUND ) THEN
      !>
      !>               CALL MTXV ( TPMI, XPT, SBFXPT )
      !>
      !>            END IF
      !>
      !>            END
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 29-SEP-2016 (NJB)
      !>
      !>        Changed from standard to discovery check-in. Fixed typo
      !>        in header.
      !>
      !>-    SPICELIB Version 1.1.1, 07-FEB-2008 (BVS)
      !>
      !>        Fixed a few typos in the header.
      !>
      !>-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      !>
      !>-    SPICELIB Version 1.0.3, 12-DEC-2002 (NJB)
      !>
      !>        Header fix: ring plane intercept algorithm was corrected.
      !>        Now light time is computed accurately, and stellar aberration
      !>        is accounted for. Example was turned into a complete
      !>        subroutine.
      !>
      !>-    SPICELIB Version 1.0.2, 09-MAR-1999 (NJB)
      !>
      !>        Reference to SCE2T replaced by reference to SCE2C. An
      !>        occurrence of ENDIF was replaced by END IF.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-APR-1991 (NJB) (WLT)
      module subroutine inrypl ( ctx, vertex, dir, plane, nxpts, xpt )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_math, only: pl2nvc, smsgnd, vdot, vhat, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: vertex (    3 )  !! Vertex and direction vector of ray.
      real(wp), intent(in) :: dir    (    3 )  !! Vertex and direction vector of ray.
      real(wp), intent(in) :: plane  ( ubpl )  !! A SPICE plane.
      integer(ip), intent(out) :: nxpts  !! Number of intersection points of ray and plane.
      real(wp), intent(out) :: xpt    (    3 )  !! Intersection point, if NXPTS = 1.


      real(wp), parameter :: margin = 3.0_wp

      real(wp) :: const
      real(wp) :: prjdif
      real(wp) :: prjdir
      real(wp) :: prjvn
      real(wp) :: mscale
      real(wp) :: normal ( 3 )
      real(wp) :: scale
      real(wp) :: sclcon
      real(wp) :: sclvtx ( 3 )
      real(wp) :: toobig
      real(wp) :: udir   ( 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     We'll give the name TOOBIG to the bound DPMAX() / MARGIN.
!     If we let VTXPRJ be the orthogonal projection of VERTEX onto
!     PLANE, and let DIFF be the vector VTXPRJ - VERTEX, then
!     we know that
!
!        ||  DIFF  ||    <     2 * TOOBIG
!
!     Check the distance of the ray's vertex from the origin.
!
      toobig = dpmax / margin

      if (  vnorm (vertex)  >=  toobig  )  then

         call ctx%setmsg ( 'Ray''s vertex is too far from the origin.' )
         call ctx%sigerr ( 'SPICE(VECTORTOOBIG)'                       )
         return

      end if

!
!     Check the distance of the plane from the origin.  (The returned
!     plane constant IS this distance.)
!
      call pl2nvc ( plane, normal, const )

      if (  const  >=  toobig  )  then

         call ctx%setmsg ( 'Plane is too far from the origin.' )
         call ctx%sigerr ( 'SPICE(VECTORTOOBIG)'               )
         return

      end if

!
!     Check the ray's direction vector.
!
      call vhat ( dir, udir )

      if ( all(udir == 0.0_wp) ) then

         call ctx%setmsg ( 'Ray''s direction vector is the zero vector.' )
         call ctx%sigerr ( 'SPICE(ZEROVECTOR)'                           )
         return

      end if

!
!     That takes care of the error cases.  Now scale the input vertex
!     and plane to improve numerical behavior.
!
      mscale = max ( const, vnorm(vertex) )

      if ( mscale /= 0.0_wp ) then

         sclvtx = (1.0_wp / mscale)*vertex
         sclcon  =  const / mscale

      else

         sclvtx = vertex
         sclcon   =  const

      end if

      if ( mscale > 1.0_wp ) then
         toobig = toobig / mscale
      end if

!     Find the projection (coefficient) of the ray's vertex along the
!     plane's normal direction.
!
      prjvn = vdot ( sclvtx, normal )

!
!     If this projection is the plane constant, the ray's vertex lies in
!     the plane.  We have one intersection or an infinite number of
!     intersections.  It all depends on whether the ray actually lies
!     in the plane.
!
!     The absolute value of PRJDIF is the distance of the ray's vertex
!     from the plane.
!
      prjdif  =  sclcon - prjvn

      if ( prjdif == 0.0_wp ) then
!
!        XPT is the original, unscaled vertex.
!
         xpt = vertex

         if (  vdot ( normal, udir )  ==  0.0_wp  )  then
!
!           The ray's in the plane.
!
            nxpts  =  -1
         else
            nxpts  =   1
         end if

         return

      end if

!
!     Ok, the ray's vertex is not in the plane.  The ray may still be
!     parallel to or may point away from the plane.  If the ray does
!     point towards the plane, mathematicians would say that the
!     ray does intersect the plane, but the computer may disagree.
!
!     For this routine to find an intersection, both of the following
!     conditions must be met:
!
!        -- The ray must point toward the plane; this happens when
!           PRJDIF has the same sign as < UDIR, NORMAL >.
!
!        -- The vector difference XPT - SCLVTX must not overflow.
!
!      Qualitatively, the case of interest looks something like the
!      picture below:
!
!                      * SCLVTX
!                      |\
!                      | \   <-- UDIR
!                      |  \
!    length of this    |   \|
!      segment is      |   -*
!                      |
!     | PRJDIF |   --> | ___________________________
!                      |/                          /
!                      |       *                  /   <-- PLANE
!                     /|        XPT              /
!                    / ^                        /
!                   /  | NORMAL                /
!                  /   | .                    /
!                 /    |/|                   /
!                / .---| /                  /
!               /  |   |/                  /
!              /   `---*                  /
!             /          Projection of SCLVTX onto the plane
!            /                          /
!           /                          /
!          ----------------------------
!

!
!     Find the projection of the direction vector along the plane's
!     normal vector.
!
      prjdir  =  vdot ( udir, normal )

!
!     We're done if the ray doesn't point toward the plane.  PRJDIF
!     has already been found to be non-zero at this point; PRJDIR is
!     zero if the ray and plane are parallel.  The SPICELIB routine
!     SMSGND will return a value of .FALSE. if PRJDIR is zero.
!
      if (  .not. smsgnd (prjdir, prjdif)  )  then
!
!        The ray is parallel to or points away from the plane.
!
         nxpts = 0
         xpt = 0.0_wp

         return

      end if

!
!     The difference XPT - SCLVTX is the hypotenuse of a right triangle
!     formed by SCLVTX, XPT, and the orthogonal projection of SCLVTX
!     onto the plane.  We'll obtain the hypotenuse by scaling UDIR.
!     We must make sure that this hypotenuse does not overflow.  The
!     scale factor has magnitude
!
!         | PRJDIF |
!       --------------
!         | PRJDIR |
!
!     and UDIR is a unit vector, so as long as
!
!         | PRJDIF |   <   | PRJDIR |  *  TOOBIG
!
!     the hypotenuse is no longer than TOOBIG.  The product can be
!     computed safely since PRJDIR has magnitude 1 or less.
!

      if (  abs(prjdif)  >=  abs(prjdir) * toobig  ) then
!
!        If the hypotenuse is too long, we say that no intersection
!        exists.
!
         nxpts = 0
         xpt = 0.0_wp

         return

      end if

!
!     We conclude that it's safe to compute XPT.  Scale UDIR and add
!     the result to SCLVTX.  The addition is safe because both addends
!     have magnitude no larger than TOOBIG.  The vector thus obtained
!     is the intersection point.
!
      nxpts    =   1
      scale    =   abs (prjdif)  /   abs (prjdir)

      xpt = 1.0_wp*sclvtx + scale*udir

!
!     Re-scale XPT.  This is safe, since TOOBIG has already been
!     scaled to allow for any growth of XPT at this step.
!
      xpt = mscale*xpt

      return
      end subroutine inrypl

      !> Construct the inverse of a 3x3 matrix with orthogonal columns and
      !> non-zero column norms using a numerically stable algorithm. The
      !> rows of the output matrix are the columns of the input matrix
      !> divided by the length squared of the corresponding columns.
      !>
      !>## Detailed Input
      !>
      !>     M        is a 3x3 matrix.
      !>
      !>## Detailed Output
      !>
      !>     MIT      is the matrix obtained by transposing M and dividing
      !>              the rows by squares of their norms.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the columns of M have zero length, the error
      !>         SPICE(ZEROLENGTHCOLUMN) is signaled.
      !>
      !>     2)  If any column is too short to allow computation of the
      !>         reciprocal of its length without causing a floating
      !>         point overflow, the error SPICE(COLUMNTOOSMALL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Suppose that M is the matrix
      !>
      !>            .-                      -.
      !>            |   A*u    B*v     C*w   |
      !>            |      1      1       1  |
      !>            |                        |
      !>            |   A*u    B*v     C*w   |
      !>            |      2      2       2  |
      !>            |                        |
      !>            |   A*u    B*v     C*w   |
      !>            |      3      3       3  |
      !>            `-                      -'
      !>
      !>     where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w )
      !>                         1   2   3      1   2   3          1   2   3
      !>     are unit vectors. This routine produces the matrix:
      !>
      !>            .-                      -.
      !>            |   a*u    a*u     a*u   |
      !>            |      1      2       3  |
      !>            |                        |
      !>            |   b*v    b*v     b*v   |
      !>            |      1      2       3  |
      !>            |                        |
      !>            |   c*w    c*w     c*w   |
      !>            |      1      2       3  |
      !>            `-                      -'
      !>
      !>     where a = 1/A, b = 1/B, and c = 1/C.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a double precision 3x3 matrix with mutually orthogonal
      !>        rows of arbitrary length, compute its inverse. Check that the
      !>        original matrix times the computed inverse produces the
      !>        identity matrix.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INVORT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      IMAT ( 3, 3 )
      !>      DOUBLE PRECISION      M    ( 3, 3 )
      !>      DOUBLE PRECISION      MOUT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define a matrix to invert.
      !>C
      !>      DATA                  M  /  0.D0,  0.5D0, 0.D0,
      !>     .                           -1.D0,  0.D0,  0.D0,
      !>     .                            0.D0,  0.D0,  1.D0 /
      !>
      !>      WRITE(*,*) 'Original Matrix:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( M(I,J), J=1,3 )
      !>
      !>      END DO
      !>C
      !>C     Invert the matrix, then output.
      !>C
      !>      CALL INVORT ( M, MOUT )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Inverse Matrix:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>C
      !>C     Check the M times MOUT produces the identity matrix.
      !>C
      !>      CALL MXM ( M, MOUT, IMAT )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original times inverse:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( IMAT(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original Matrix:
      !>               0.0000000      -1.0000000       0.0000000
      !>               0.5000000       0.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>         Inverse Matrix:
      !>               0.0000000       2.0000000       0.0000000
      !>              -1.0000000       0.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>         Original times inverse:
      !>               1.0000000       0.0000000       0.0000000
      !>               0.0000000       1.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of argument MIT in $Brief_I/O table. Extended $Abstract
      !>        section.
      !>
      !>        Added complete code example to $Examples section.
      !>
      !>-    SPICELIB Version 1.1.1, 14-NOV-2013 (EDW)
      !>
      !>        Edit to $Abstract. Eliminated unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      !>
      !>-    SPICELIB Version 1.0.0, 02-JAN-2002 (WLT)
      module subroutine invort ( ctx, m, mit )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_math, only: unorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: m   ( 3, 3 )  !! A 3x3 matrix.
      real(wp), intent(out) :: mit ( 3, 3 )  !! M after transposition and scaling of rows.

      real(wp), parameter :: bound = dpmax
      real(wp) :: length
      real(wp) :: scale
      real(wp) :: temp   ( 3, 3 )

      integer(ip) :: i

!
!     For each column, construct a scaled copy. However, make sure
!     everything is do-able before trying something.
!
      do i = 1, 3

         call unorm ( m(1,i), temp(1,i), length )

         if ( length == 0.0_wp ) then

            call ctx%setmsg ( 'Column # of the input matrix has a norm '  // &
                          'of zero. '                                  )
            call ctx%errint ( '#',        i                                )
            call ctx%sigerr ( 'SPICE(ZEROLENGTHCOLUMN)'                    )
            return

         end if

!
!        Make sure we can actually rescale the rows.
!
         if    ( length < 1.0_wp ) then

            if (  ( length * bound ) < 1.0_wp ) then

               call ctx%setmsg ( 'The length of column # is #. This '     // &
                             'number cannot be inverted.  For this '  // &
                             'reason, the scaled transpose of the '   // &
                             'input matrix cannot be formed. '        )
               call ctx%errint ( '#',   i                                 )
               call ctx%errdp  ( '#',   length                            )
               call ctx%sigerr ( 'SPICE(COLUMNTOOSMALL)'                  )
               return

            end if

         end if

         scale = 1.0_wp / length

         temp(:,i) = scale*temp(:,i)

      end do

!
!     If we make it this far, we just need to transpose TEMP into MIT.
!
      mit = transpose(temp)
      return
      end subroutine invort

      !> Return the inverse of a state transformation matrix.
      !>
      !>## Detailed Input
      !>
      !>     MAT      is a state transformation matrix for converting states
      !>              relative to one frame to states relative to another.
      !>              The state transformation of a state vector, S, is
      !>              performed by the matrix-vector product.
      !>
      !>                 MAT * S.
      !>
      !>              For MAT to be a "true" state transformation matrix
      !>              it must have the form
      !>
      !>                  .-            -.
      !>                  |       :      |
      !>                  |   R   :   0  |
      !>                  |.......:......|
      !>                  |       :      |
      !>                  |  W*R  :   R  |
      !>                  |       :      |
      !>                  `-            -'
      !>
      !>              where R is a 3x3 rotation matrix, 0 is the 3x3 zero
      !>              matrix and W is a 3x3 skew-symmetric matrix.
      !>
      !>              NOTE: no checks are performed on MAT to ensure that it
      !>                    does indeed have the form described above.
      !>
      !>## Detailed Output
      !>
      !>     INVMAT   is the inverse of MAT under the operation of matrix
      !>              multiplication.
      !>
      !>              If MAT has the form described above, then INVMAT has
      !>              the form shown below.
      !>
      !>                 .-             -.
      !>                 |     t  :      |
      !>                 |    R   :   0  |
      !>                 |........:......|
      !>                 |      t :    t |
      !>                 | (W*R)  :   R  |
      !>                 |        :      |
      !>                 `-             -'
      !>
      !>              (The superscript "t" denotes the matrix transpose
      !>              operation.)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No checks are performed to ensure that the input matrix is
      !>         indeed a state transformation matrix.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given a matrix for transforming states relative frame 1 to
      !>     states relative frame 2, the routine produces the inverse
      !>     matrix. That is, it returns the matrix for transforming states
      !>     relative to frame 2 to states relative to frame 1.
      !>
      !>     This special routine exists because unlike the inverse of a
      !>     rotation matrix, the inverse of a state transformation matrix,
      !>     is NOT simply the transpose of the matrix.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you have a geometric state of a spacecraft in Earth
      !>        body-fixed reference frame and wish to express this state
      !>        relative to an Earth centered J2000 frame. The following
      !>        example code illustrates how to carry out this computation.
      !>
      !>        Use the PCK kernel below to load the required high precision
      !>        orientation of the ITRF93 Earth body-fixed reference frame.
      !>        Note that the body ID code used in this file for the Earth is
      !>        3000.
      !>
      !>           earth_720101_070426.bpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INVSTM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      INVMAT ( 6, 6 )
      !>      DOUBLE PRECISION      ISTAT1 ( 6    )
      !>      DOUBLE PRECISION      ISTAT2 ( 6    )
      !>      DOUBLE PRECISION      MAT    ( 6, 6 )
      !>      DOUBLE PRECISION      STATE  ( 6    )
      !>      DOUBLE PRECISION      XMAT   ( 6, 6 )
      !>
      !>      INTEGER               EARTH
      !>
      !>C
      !>C     Define the state of the spacecraft, in km and
      !>C     km/s, and the ET epoch, in seconds past J2000.
      !>C
      !>      DATA                  ET    /  0.0D0 /
      !>      DATA                  STATE /  175625246.29100420D0,
      !>     .                               164189388.12540060D0,
      !>     .                               -62935198.26067264D0,
      !>     .                                   11946.73372264D0,
      !>     .                                  -12771.29732556D0,
      !>     .                                      13.84902914D0 /
      !>
      !>C
      !>C     Load the required high precision Earth PCK.
      !>C
      !>      CALL FURNSH ( 'earth_720101_070426.bpc' )
      !>
      !>C
      !>C     First get the state transformation from J2000 frame
      !>C     to Earth body-fixed frame at the time of interest ET.
      !>C     The body ID code used in high precision PCK files for
      !>C     the Earth is 3000; this number indicates that the
      !>C     terrestrial frame used is ITRF93.
      !>C
      !>      EARTH = 3000
      !>      CALL TISBOD ( 'J2000', EARTH, ET, MAT )
      !>
      !>C
      !>C     Get the inverse of MAT.
      !>C
      !>      CALL INVSTM ( MAT,  INVMAT          )
      !>
      !>C
      !>C     Transform from bodyfixed state to inertial state.
      !>C
      !>      CALL MXVG ( INVMAT, STATE, 6, 6, ISTAT1 )
      !>
      !>C
      !>C     Print the resulting state.
      !>C
      !>      WRITE(*,'(A)') 'Input state in Earth centered J2000 '
      !>     .            // 'frame, using INVSTM:'
      !>      WRITE(*,'(A,3F16.3)') '   Position:', ISTAT1(1:3)
      !>      WRITE(*,'(A,3F16.3)') '   Velocity:', ISTAT1(4:6)
      !>
      !>C
      !>C     Compute the same state using SXFORM.
      !>C
      !>      CALL SXFORM ( 'ITRF93', 'J2000', ET, XMAT )
      !>      CALL MXVG   ( XMAT, STATE, 6, 6, ISTAT2 )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Input state in Earth centered J2000 '
      !>     .            // 'frame, using SXFORM:'
      !>      WRITE(*,'(A,3F16.3)') '   Position:', ISTAT2(1:3)
      !>      WRITE(*,'(A,3F16.3)') '   Velocity:', ISTAT2(4:6)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input state in Earth centered J2000 frame, using INVSTM:
      !>           Position:   192681395.921  -143792821.383   -62934296.473
      !>           Velocity:          30.312          32.007          13.876
      !>
      !>        Input state in Earth centered J2000 frame, using SXFORM:
      !>           Position:   192681395.921  -143792821.383   -62934296.473
      !>           Velocity:          30.312          32.007          13.876
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Removed unnecessary Standard SPICE error handling calls to
      !>        register/unregister this routine in the error handling
      !>        subsystem; this routine is Error free.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing fragment.
      !>
      !>-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 29-OCT-1990 (WLT)
      module subroutine invstm ( ctx, mat, invmat )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: mat    ( 6, 6 )  !! A state transformation matrix.
      real(wp), intent(out) :: invmat ( 6, 6 )  !! The inverse of MAT.

      integer(ip), parameter :: nrows = 6

      integer(ip), parameter :: ncols = 6

      integer(ip), parameter :: block = 3

!
!     Not much to this. Just call the more general routine XPOSBL.
!
      call xposbl ( ctx, mat, nrows, ncols, block, invmat )

      return
      end subroutine invstm

      !> Indicate whether a 3x3 matrix is a rotation matrix.
      !>
      !> The function returns .TRUE. if and only if M is a rotation matrix.
      !>
      !>## Detailed Input
      !>
      !>     M        is a 3x3 matrix to be tested.
      !>
      !>     NTOL     is the tolerance for the norms of the columns
      !>              of M.
      !>
      !>     DTOL     is the tolerance for the determinant of a matrix
      !>              whose columns are the unitized columns of M.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if and only if M is found to be a
      !>     rotation matrix. The criteria that M must meet are:
      !>
      !>     1) The norm of each column of M must satisfy the relation
      !>
      !>           1.D0 - NTOL  <   || column ||   <  1.D0 + NTOL
      !>                        -                  -
      !>
      !>     2) The determinant of the matrix whose columns are the
      !>        unitized columns of M must satisfy
      !>
      !>           1.D0 - DTOL  <   determinant   <  1.D0 + DTOL
      !>                        -                 -
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If either of NTOL or DTOL is negative, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled. ISROT returns the
      !>         value .FALSE. in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is an error checking `filter'; its purpose is to
      !>     detect gross errors, such as uninitialized matrices. Matrices
      !>     that do not pass the tests used by this routine hardly qualify as
      !>     rotation matrices. The test criteria can be adjusted by varying
      !>     the parameters NTOL and DTOL.
      !>
      !>     A property of rotation matrices is that their columns form a
      !>     right-handed, orthonormal basis in 3-dimensional space. The
      !>     converse is true: all 3x3 matrices with this property are
      !>     rotation matrices.
      !>
      !>     An ordered set of three vectors V1, V2, V3 forms a right-handed,
      !>     orthonormal basis if and only if
      !>
      !>        1)   || V1 ||  =  || V2 ||  =  || V3 ||  =  1
      !>
      !>        2)   V3 = V1 x V2. Since V1, V2, and V3 are unit vectors,
      !>             we also have
      !>
      !>             < V3, V1 x V2 > = 1.
      !>
      !>             This quantity is the determinant of the matrix whose
      !>             columns are V1, V2 and V3.
      !>
      !>     When finite precision numbers are used, rotation matrices will
      !>     usually fail to satisfy these criteria exactly. We must use
      !>     criteria that indicate approximate conformance to the criteria
      !>     listed above. We choose
      !>
      !>        1)   |   || Vi ||  -  1   |   <   NTOL,  i = 1, 2, 3.
      !>                                      -
      !>
      !>        2)   Let
      !>
      !>                       Vi
      !>                Ui = ------ ,   i = 1, 2, 3.
      !>                     ||Vi||
      !>
      !>             Then we require
      !>
      !>                | < U3, U1 x U2 > - 1 |  <  DTOL;
      !>                                         -
      !>
      !>             equivalently, letting U be the matrix whose columns
      !>             are U1, U2, and U3, we insist on
      !>
      !>                | det(U) - 1 |  <  DTOL.
      !>                                _
      !>
      !>## Examples
      !>
      !>     1)  We have obtained an instrument pointing matrix C from a
      !>         C-kernel, and we wish to test whether it is in fact a
      !>         rotation matrix. We can use ISROT to check this:
      !>
      !>            C
      !>            C    Obtain pointing matrix:
      !>            C
      !>                 CALL CKGP ( INST, TIMEIN, TOL, REF, C, TIMOUT, FOUND )
      !>
      !>            C
      !>            C    Verify that C is a rotation:
      !>            C
      !>                 IF ( .NOT. ISROT ( C )  ) THEN
      !>
      !>                    [ perform exception handling ]
      !>
      !>                 ELSE
      !>
      !>                    [ code for the normal case goes here ]
      !>
      !>                 END IF
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of the
      !>        function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
      module logical function isrot ( ctx, m, ntol, dtol )
      use pepper_kinds, only: wp
      use pepper_math, only: det, unorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: m    ( 3, 3 )  !! A matrix to be tested.
      real(wp), intent(in) :: ntol  !! Tolerance for the norms of the columns of M.
      real(wp), intent(in) :: dtol  !! Tolerance for the determinant of a matrix whose columns
                                    !! are the unitized columns of M.

      real(wp) :: d
      real(wp) :: n1
      real(wp) :: n2
      real(wp) :: n3
      real(wp) :: unit ( 3, 3 )

      logical :: detok
      logical :: normok

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         isrot = .false.
         return
      end if

!
!     Tolerances must be non-negative.
!
      if ( ntol < 0.0_wp ) then

         isrot = .false.

         call ctx%setmsg ( 'NTOL should be non-negative; it is #.' )
         call ctx%errdp  ( '#',  ntol                              )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                )
         return

      else if ( dtol < 0.0_wp ) then

         isrot = .false.

         call ctx%setmsg ( 'DTOL should be non-negative; it is #.' )
         call ctx%errdp  ( '#',  dtol                              )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

!
!     The columns of M must resemble unit vectors.  If the norms are
!     outside of the allowed range, M is not a rotation matrix.
!
!     Also, the columns of M are required to be pretty nearly
!     orthogonal.  The discrepancy is gauged by taking the determinant
!     of the matrix UNIT, computed below, whose columns are the
!     unitized columns of M.
!
      call unorm (  m(1,1),  unit(1,1),  n1  )
      call unorm (  m(1,2),  unit(1,2),  n2  )
      call unorm (  m(1,3),  unit(1,3),  n3  )

      d      =  det ( unit )

      normok =       ( n1 == max(min(1.0_wp - ntol, 1.0_wp + ntol), min(max(1.0_wp - ntol, 1.0_wp + ntol), &
         n1)) )  .and. ( n2 == max(min(1.0_wp - ntol, 1.0_wp + ntol), min(max(1.0_wp - ntol, 1.0_wp + ntol), &
         n2)) )  .and. ( n3 == max(min(1.0_wp - ntol, 1.0_wp + ntol), min(max(1.0_wp - ntol, 1.0_wp + ntol), n3)) )

      detok  =         d  == max(min(1.0_wp - dtol, 1.0_wp + dtol), min(max(1.0_wp - dtol, 1.0_wp + dtol), d))

      if ( normok .and. detok ) then
         isrot = .true.
      else
         isrot = .false.
      end if

      return
      end function isrot

      !> Solve the equinoctial version of Kepler's equation.
      !>
      !> The function returns the solution to the equinoctial version of
      !> Kepler's equation, given the mean longitude and the h and k
      !> components of the equinoctial elements.
      !>
      !>## Detailed Input
      !>
      !>     ML       is the mean longitude of some body following two body
      !>              motion. (Mean longitude = Mean anomaly + argument of
      !>              periapse + longitude of ascending node.)
      !>
      !>     H        is the h component of the equinoctial element set
      !>              ( h = ECC*SIN( arg of periapse + long ascending node) )
      !>
      !>     K        is the k component of the equinoctial element set
      !>              ( k = ECC*COS( arg of periapse + long ascending node) )
      !>
      !>## Detailed Output
      !>
      !>     The function returns the solution to the equinoctial version of
      !>     Kepler's equation, given the mean longitude and the h and k
      !>     components of the equinoctial elements.
      !>
      !>     The solution is the value of F such that
      !>
      !>        ML = F + H * COS(F) - K * SIN(F)
      !>
      !>     Note that ECC = DSQRT ( K*K + H*H )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the sum of the squares of H and K is not less than .9,
      !>         the error SPICE(ECCOUTOFBOUNDS) is signaled.
      !>
      !>     2)  If the iteration for a solution to the equinoctial Kepler's
      !>         equation does not converge in 10 or fewer steps, the error
      !>         SPICE(NOCONVERGENCE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine solves the equinoctial element version of
      !>     Kepler's equation.
      !>
      !>        ML = F + H * COS(F) - K * SIN(F)
      !>
      !>     Here F is an offset from the eccentric anomaly E.
      !>
      !>        F = E - argument of periapse - longitude of ascending node.
      !>
      !>     where E is eccentric anomaly.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Owen and R. Vaughan, "Optical Navigation Program
      !>          Mathematical Models," JPL Engineering Memorandum 314-513,
      !>          August 9, 1991.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 26-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Procedure section for consistency with KPSOLV routine.
      !>
      !>-    SPICELIB Version 1.0.0, 11-DEC-1996 (WLT)
      module real(wp) function kepleq ( ctx, ml, h, k )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: ml  !! Mean longitude.
      real(wp), intent(in) :: h  !! h component of equinoctial elements.
      real(wp), intent(in) :: k  !! k component of equinoctial elements.

      real(wp) :: evec ( 2 )
      real(wp) :: e2

!
!     Make sure that H and K are in the expected range.
!
      e2 = h*h + k*k

      if ( e2 >= 0.81_wp ) then

         kepleq = 0.0_wp
         call ctx%setmsg ( 'The values of H and K supplied to KEPLEQ ' &
         //            'must satisfy the inequality H*H + K*K < ' &
         //            'ECC**2 where ECC is the eccentricity ' &
         //            'threshold of 0.9.  The values of H and K ' &
         //            'are: # and # respectively. H*H + K*K = ' &
         //            '#. ' )

         call ctx%errdp  ( '#', h  )
         call ctx%errdp  ( '#', k  )
         call ctx%errdp  ( '#', e2 )
         call ctx%sigerr ( 'SPICE(ECCOUTOFBOUNDS)' )
         return

      end if

!
!     Instead of solving the equation
!
!            ML  = F + H*DCOS(F) - K*DSIN(F)
!
!     We set X equal to F - ML and solve the equivalent equation
!
!            0   = X + H*DCOS(ML+X) - K*DSIN(ML+X)
!
!                = X + H*{DCOS(ML)*DCOS(X) - DSIN(ML)*DSIN(X)}
!                    - K*{DSIN(ML)*DCOS(X) + DCOS(ML)*DSIN(X)}
!
!                = X + { H*DCOS(ML) - K*DSIN(ML) }*DCOS(X)
!                    - { H*DSIN(ML) + K*DCOS(ML) }*DSIN(X)
!
!     We can rearrange this to:
!
!                                 -                    -     -       -
!                                |  DCOS(ML)  -DSIN(ML) |   | DCOS(X) |
!            0 = X + [ H  -K ] * |  DSIN(ML)   DCOS(ML) | * | DSIN(X) |
!                                 -                    -     -       -
!
!     Finally if we let
!
!                                        -                    -
!                                       |  DCOS(ML)  -DSIN(ML) |
!      EVEC =  [ EX  EY ] = [ -H  K ] * |  DSIN(ML)   DCOS(ML) |
!                                        -                    -
!
!     and
!
!              DCOS(X)
!      U(X) =  DSIN(X)
!
!     Then we can rewrite the equation as:
!
!        0  =  X - < EVEC, U(X) >
!
!     where <,> denotes the dot product operation.  Note that X
!     is necessarily in the range from -ECC to ECC where ECC = | EVEC |
!
!     Once we've computed X, F is just ML + X.
!
!     For those of you who are fans of the classical keplerian
!     elements:
!
!        x = F - ML = E - M
!
!     where E denotes eccentric anomaly and M denotes mean anomaly.
!
!     The routine KPEVEC returns the value of X that solves
!     the equation X - < EVEC, UVEC(X) >
!

      evec(1) = -h*dcos(ml) + k*dsin(ml)
      evec(2) =  h*dsin(ml) + k*dcos(ml)
      kepleq  =  ml         + kpsolv( ctx, evec )

      return
      end function kepleq

      !> Solve the equation X = < EVEC, U(X) > where U(X) is the unit
      !> vector [ COS(X), SIN(X) ] and  < , > denotes the two-dimensional
      !> dot product.
      !>
      !> The function returns the solution to X = < EVEC, U(X) >
      !>
      !>## Detailed Input
      !>
      !>     EVEC     is any two dimensional vector whose magnitude is
      !>              less than 1.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value X such that the equation
      !>
      !>        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the magnitude of EVEC is greater than or equal to 1,
      !>         the error SPICE(EVECOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses bisection and Newton's method to find
      !>     the root of the equation
      !>
      !>        X = EVEC(1)COS(X) + EVEC(2)SIN(X).
      !>
      !>     This equation is just a "vector form" of Kepler's equation.
      !>
      !>## Examples
      !>
      !>     Suppose you need to solve the equation
      !>
      !>         M = E - e SIN(E)                           [ 1 ]
      !>
      !>     for E. If we let X = E - M the equation is transformed to
      !>
      !>        0 = X - e SIN( X + M )
      !>
      !>          = X - e SIN( M ) COS(X) - e COS(M) SIN ( X )
      !>
      !>     Thus if we solve the equation
      !>
      !>        X = e SIN(M) COS(X) + e COS(M) SIN(X)
      !>
      !>     we can find the value of X we can compute E.
      !>
      !>     The code fragment below illustrates how this routine can
      !>     be used to solve equation [1].
      !>
      !>         EVEC(1) = ECC * DSIN(M)
      !>         EVEC(2) = ECC * DCOS(M)
      !>         E       = M   + KPSOLV( EVEC )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 26-AUG-1997 (WLT)
      !>
      !>        KPSOLV is now given an initial value of zero so that
      !>        if an error condition is detected, KPSOLV will have
      !>        a return value.
      !>
      !>-    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT)
      module real(wp) function kpsolv ( ctx, evec )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: evec ( 2 )  !! A 2-vector whose magnitude is less than 1.

!
!     MXNEWT is the number of iterations we will perform
!     in the Newtons method for finding the solution to
!     the vector form of Kepler's equation.  It has been
!     empirically determined that 5 iterations is always
!     sufficient on computers have 64 bit double precision
!     numbers.
!
      integer(ip), parameter :: mxnewt = 5

      real(wp) :: cosx
      real(wp) :: ecc
      real(wp) :: ecc2
      real(wp) :: h
      real(wp) :: k
      real(wp) :: sinx
      real(wp) :: x
      real(wp) :: xl
      real(wp) :: xm
      real(wp) :: xu
      real(wp) :: y0
      real(wp) :: ypx
      real(wp) :: yx
      real(wp) :: yxm

      integer(ip) :: i
      integer(ip) :: maxit

!
!     We give the function an initial value, just in case
!     we exit without solving Kepler's equation.
!
      kpsolv = 0.0_wp

      h      = evec(1)
      k      = evec(2)

      ecc2   = h*h + k*k

      if ( ecc2 >= 1.0_wp ) then

         call ctx%setmsg ( 'The magnitude of the vector EVEC = ( #, ' &
         //            '# ) must be less than 1.  However, the ' &
         //            'magnitude of this vector is #.' )

         call ctx%errdp  ( '#', h           )
         call ctx%errdp  ( '#', k           )
         call ctx%errdp  ( '#', dsqrt(ecc2) )

         call ctx%sigerr ( 'SPICE(EVECOUTOFRANGE)'  )
         return

      end if

!
!     We first approximate the equation 0 = X - H * COS(X) - K * SIN(X)
!     using bisection.  If we let Y(X) = X - H * COS(X) - K * SIN(X)
!
!        Y( ECC) =  ECC - <EVEC,U(X)>  =   ECC - ECC*COS(ANGLE_X) > 0
!        Y(-ECC) = -ECC - <EVEC,U(X)>  =  -ECC - ECC*COS(ANGLE_X) < 0
!
!     where ANGLE_X is the angle between U(X) and EVEC. Thus -ECC
!     and ECC necessarily bracket the root of the equation Y(X) = 0.
!
!     Also note that Y'(X) = 1 - < EVEC, V(X) > where V(X) is the
!     unit vector given by U'(X).  Thus Y is an increasing function
!     over the interval from -ECC to ECC.
!
!     The mid point of ECC and -ECC is 0 and Y(0) = -H.  Thus
!     we can do the first bisection step without doing
!     much in the way of computations.
!

      y0  = -h
      xm  =  0.0_wp
      ecc =  dsqrt(ecc2)

      if      ( y0 > 0.0_wp ) then

         xu  =  0.0_wp
         xl  = -ecc

      else if ( y0 < 0.0_wp ) then

         xu =  ecc
         xl =  0.0_wp

      else

         kpsolv = 0.0_wp
         return

      end if
!
!     Iterate until we are assured of being in a region where
!     Newton's method will converge quickly.  The formula
!     below was empirically determined to give good results.
!
      maxit = min(32, max( 1, nint(1.0_wp/(1.0_wp-ecc)) ) )

      do i = 1, maxit

!
!        Compute the next midpoint.  We bracket XM by XL and XU just in
!        case some kind of strange rounding occurs in the computation
!        of the midpoint.
!
         xm     = max( xl, min( xu, 0.5_wp*(xl + xu) ) )

!
!        Compute Y at the midpoint of XU and XL
!
         yxm = xm - h*cos(xm) - k*sin(xm)
!
!        Determine the new upper and lower bounds.
!
         if ( yxm > 0.0_wp ) then
            xu = xm
         else
            xl = xm
         end if

      end do

!
!     We've bisected into a region where we can now get rapid
!     convergence using Newton's method.
!
      x   = xm

      do i = 1, mxnewt

         cosx = dcos(x)
         sinx = dsin(x)
!
!        Compute Y and Y' at X.  Use these to get the next
!        iteration for X.
!
!        For those of you who might be wondering, "Why not put
!        in a check for YX .EQ. 0 and return early if we get
!        an exact solution?"  Here's why.  An empirical check
!        of those cases where you can actually escape from the
!        Do-loop  showed that the test YX .EQ. 0 is true
!        only about once in every 10000 case of random inputs
!        of EVEC.  Thus on average the check is a waste of
!        time and we don't bother with it.
!
         yx   = x     - h*cosx - k*sinx
         ypx  = 1.0_wp + h*sinx - k*cosx
         x    = x     - yx/ypx

      end do

      kpsolv = x

      return
      end function kpsolv

      !> Find a unit quaternion corresponding to a specified rotation
      !> matrix.
      !>
      !>## Detailed Input
      !>
      !>     R        is a rotation matrix.
      !>
      !>## Detailed Output
      !>
      !>     Q        is a unit-length SPICE-style quaternion
      !>              representing R. See the discussion of quaternion
      !>              styles in $Particulars below.
      !>
      !>              Q is a 4-dimensional vector. If R rotates vectors
      !>              in the counterclockwise sense by an angle of theta
      !>              radians about a unit vector A, where
      !>
      !>                 0 < theta < pi
      !>                   -       -
      !>
      !>              then letting h = theta/2,
      !>
      !>                 Q = ( cos(h), sin(h)A ,  sin(h)A ,  sin(h)A ).
      !>                                      1          2          3
      !>
      !>              The restriction that theta must be in the range
      !>              [0, pi] determines the output quaternion Q
      !>              uniquely except when theta = pi; in this special
      !>              case, both of the quaternions
      !>
      !>                 Q = ( 0,  A ,  A ,  A  )
      !>                            1    2    3
      !>              and
      !>
      !>                 Q = ( 0, -A , -A , -A  )
      !>                            1    2    3
      !>
      !>              are possible outputs.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If R is not a rotation matrix, the error SPICE(NOTAROTATION)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     A unit quaternion is a 4-dimensional vector for which the sum of
      !>     the squares of the components is 1. Unit quaternions can be used
      !>     to represent rotations in the following way: given a rotation
      !>     angle theta, where
      !>
      !>        0 < theta < pi
      !>          -       -
      !>
      !>     and a unit vector A, we can represent the transformation that
      !>     rotates vectors in the counterclockwise sense by theta radians
      !>     about A using the quaternion Q, where
      !>
      !>        Q =
      !>
      !>        ( cos(theta/2), sin(theta/2)a , sin(theta/2)a , sin(theta/2)a )
      !>                                     1               2               3
      !>
      !>     As mentioned in Detailed Output, our restriction on the range of
      !>     theta determines Q uniquely, except when theta = pi.
      !>
      !>     The SPICELIB routine Q2M is an one-sided inverse of this routine:
      !>     given any rotation matrix R, the calls
      !>
      !>        CALL M2Q ( R, Q )
      !>        CALL Q2M ( Q, R )
      !>
      !>     leave R unchanged, except for round-off error. However, the
      !>     calls
      !>
      !>        CALL Q2M ( Q, R )
      !>        CALL M2Q ( R, Q )
      !>
      !>     might preserve Q or convert Q to -Q.
      !>
      !>     Quaternion Styles
      !>     -----------------
      !>
      !>     There are different "styles" of quaternions used in
      !>     science and engineering applications. Quaternion styles
      !>     are characterized by
      !>
      !>     -  The order of quaternion elements
      !>
      !>     -  The quaternion multiplication formula
      !>
      !>     -  The convention for associating quaternions
      !>        with rotation matrices
      !>
      !>     Two of the commonly used styles are
      !>
      !>        - "SPICE"
      !>
      !>           > Invented by Sir William Rowan Hamilton
      !>           > Frequently used in mathematics and physics textbooks
      !>
      !>        - "Engineering"
      !>
      !>           > Widely used in aerospace engineering applications
      !>
      !>     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions.
      !>     Quaternions of any other style must be converted to SPICE
      !>     quaternions before they are passed to SPICELIB routines.
      !>
      !>     Relationship between SPICE and Engineering Quaternions
      !>     ------------------------------------------------------
      !>
      !>     Let M be a rotation matrix such that for any vector V,
      !>
      !>        M*V
      !>
      !>     is the result of rotating V by theta radians in the
      !>     counterclockwise direction about unit rotation axis vector A.
      !>     Then the SPICE quaternions representing M are
      !>
      !>        (+/-) (  cos(theta/2),
      !>                 sin(theta/2) A(1),
      !>                 sin(theta/2) A(2),
      !>                 sin(theta/2) A(3)  )
      !>
      !>     while the engineering quaternions representing M are
      !>
      !>        (+/-) ( -sin(theta/2) A(1),
      !>                -sin(theta/2) A(2),
      !>                -sin(theta/2) A(3),
      !>                 cos(theta/2)       )
      !>
      !>     For both styles of quaternions, if a quaternion q represents
      !>     a rotation matrix M, then -q represents M as well.
      !>
      !>     Given an engineering quaternion
      !>
      !>        QENG   = ( q0,  q1,  q2,  q3 )
      !>
      !>     the equivalent SPICE quaternion is
      !>
      !>        QSPICE = ( q3, -q0, -q1, -q2 )
      !>
      !>     Associating SPICE Quaternions with Rotation Matrices
      !>     ----------------------------------------------------
      !>
      !>     Let FROM and TO be two right-handed reference frames, for
      !>     example, an inertial frame and a spacecraft-fixed frame. Let the
      !>     symbols
      !>
      !>        V    ,   V
      !>         FROM     TO
      !>
      !>     denote, respectively, an arbitrary vector expressed relative to
      !>     the FROM and TO frames. Let M denote the transformation matrix
      !>     that transforms vectors from frame FROM to frame TO; then
      !>
      !>        V   =  M * V
      !>         TO         FROM
      !>
      !>     where the expression on the right hand side represents left
      !>     multiplication of the vector by the matrix.
      !>
      !>     Then if the unit-length SPICE quaternion q represents M, where
      !>
      !>        q = (q0, q1, q2, q3)
      !>
      !>     the elements of M are derived from the elements of q as follows:
      !>
      !>          +-                                                         -+
      !>          |           2    2                                          |
      !>          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
      !>          |                                                           |
      !>          |                                                           |
      !>          |                               2    2                      |
      !>      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
      !>          |                                                           |
      !>          |                                                           |
      !>          |                                                   2    2  |
      !>          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
      !>          |                                                           |
      !>          +-                                                         -+
      !>
      !>     Note that substituting the elements of -q for those of q in the
      !>     right hand side leaves each element of M unchanged; this shows
      !>     that if a quaternion q represents a matrix M, then so does the
      !>     quaternion -q.
      !>
      !>     To map the rotation matrix M to a unit quaternion, we start by
      !>     decomposing the rotation matrix as a sum of symmetric
      !>     and skew-symmetric parts:
      !>
      !>                                        2
      !>        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]
      !>
      !>                     symmetric                   skew-symmetric
      !>
      !>     OMEGA is a skew-symmetric matrix of the form
      !>
      !>                   +-             -+
      !>                   |  0   -n3   n2 |
      !>                   |               |
      !>         OMEGA  =  |  n3   0   -n1 |
      !>                   |               |
      !>                   | -n2   n1   0  |
      !>                   +-             -+
      !>
      !>     The vector N of matrix entries (n1, n2, n3) is the rotation axis
      !>     of M and theta is M's rotation angle. Note that N and theta
      !>     are not unique.
      !>
      !>     Let
      !>
      !>        C = cos(theta/2)
      !>        S = sin(theta/2)
      !>
      !>     Then the unit quaternions Q corresponding to M are
      !>
      !>        Q = +/- ( C, S*n1, S*n2, S*n3 )
      !>
      !>     The mappings between quaternions and the corresponding rotations
      !>     are carried out by the SPICELIB routines
      !>
      !>        Q2M {quaternion to matrix}
      !>        M2Q {matrix to quaternion}
      !>
      !>     M2Q always returns a quaternion with scalar part greater than
      !>     or equal to zero.
      !>
      !>     SPICE Quaternion Multiplication Formula
      !>     ---------------------------------------
      !>
      !>     Given a SPICE quaternion
      !>
      !>        Q = ( q0, q1, q2, q3 )
      !>
      !>     corresponding to rotation axis A and angle theta as above, we can
      !>     represent Q using "scalar + vector" notation as follows:
      !>
      !>        s =   q0           = cos(theta/2)
      !>
      !>        v = ( q1, q2, q3 ) = sin(theta/2) * A
      !>
      !>        Q = s + v
      !>
      !>     Let Q1 and Q2 be SPICE quaternions with respective scalar
      !>     and vector parts s1, s2 and v1, v2:
      !>
      !>        Q1 = s1 + v1
      !>        Q2 = s2 + v2
      !>
      !>     We represent the dot product of v1 and v2 by
      !>
      !>        <v1, v2>
      !>
      !>     and the cross product of v1 and v2 by
      !>
      !>        v1 x v2
      !>
      !>     Then the SPICE quaternion product is
      !>
      !>        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)
      !>
      !>     If Q1 and Q2 represent the rotation matrices M1 and M2
      !>     respectively, then the quaternion product
      !>
      !>        Q1*Q2
      !>
      !>     represents the matrix product
      !>
      !>        M1*M2
      !>
      !>## Examples
      !>
      !>     1)  A case amenable to checking by hand calculation:
      !>
      !>            To convert the rotation matrix
      !>
      !>                     +-              -+
      !>                     |  0     1    0  |
      !>                     |                |
      !>               R  =  | -1     0    0  |
      !>                     |                |
      !>                     |  0     0    1  |
      !>                     +-              -+
      !>
      !>            also represented as
      !>
      !>               [ pi/2 ]
      !>                       3
      !>
      !>            to a quaternion, we can use the code fragment
      !>
      !>               CALL ROTATE (  HALFPI(),  3,  R  )
      !>               CALL M2Q    (  R,             Q  )
      !>
      !>            M2Q will return Q as
      !>
      !>               ( sqrt(2)/2, 0, 0, -sqrt(2)/2 )
      !>
      !>            Why?  Well, R is a reference frame transformation that
      !>            rotates vectors by -pi/2 radians about the axis vector
      !>
      !>               A  = ( 0, 0, 1 )
      !>
      !>            Equivalently, R rotates vectors by pi/2 radians in
      !>            the counterclockwise sense about the axis vector
      !>
      !>               -A = ( 0, 0, -1 )
      !>
      !>            so our definition of Q,
      !>
      !>               h = theta/2
      !>
      !>               Q = ( cos(h), sin(h)A , sin(h)A , sin(h)A  )
      !>                                    1         2         3
      !>
      !>            implies that in this case,
      !>
      !>               Q =  ( cos(pi/4),  0,  0, -sin(pi/4)  )
      !>
      !>                 =  ( sqrt(2)/2,  0,  0, -sqrt(2)/2  )
      !>
      !>     2)  Finding a quaternion that represents a rotation specified by
      !>         a set of Euler angles:
      !>
      !>            Suppose our original rotation R is the product
      !>
      !>               [ TAU ]  [ pi/2 - DELTA ]  [ ALPHA ]
      !>                      3                 2          3
      !>
      !>            The code fragment
      !>
      !>               CALL EUL2M  ( TAU,   HALFPI() - DELTA,   ALPHA,
      !>              .              3,     2,                  3,      R )
      !>
      !>               CALL M2Q    ( R, Q )
      !>
      !>            yields a quaternion Q that represents R.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 27-FEB-2008 (NJB)
      !>
      !>        Updated header; added information about SPICE
      !>        quaternion conventions. Made various minor edits
      !>        throughout header.
      !>
      !>-    SPICELIB Version 2.0.0, 17-SEP-1999 (WLT)
      !>
      !>        The routine was re-implemented to sharpen the numerical
      !>        stability of the routine and eliminate calls to SIN
      !>        and COS functions.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
      module subroutine m2q ( ctx, r, q )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: r ( 3,  3 )  !! A rotation matrix.
      real(wp), intent(out) :: q ( 0 : 3 )  !! A unit quaternion representing R.

!
!     NTOL and DETOL are used to determine whether R is a rotation
!     matrix.
!
!     NTOL is the tolerance for the norms of the columns of R.
!
!     DTOL is the tolerance for the determinant of a matrix whose
!     columns are the unitized columns of R.
!
      real(wp), parameter :: ntol = 0.1_wp

      real(wp), parameter :: dtol = 0.1_wp

      real(wp) :: c
      real(wp) :: cc4
      real(wp) :: factor
      real(wp) :: l2
      real(wp) :: mtrace
      real(wp) :: polish
      real(wp) :: s(3)
      real(wp) :: s114
      real(wp) :: s224
      real(wp) :: s334
      real(wp) :: trace

!
!     If R is not a rotation matrix, we can't proceed.
!
      if (  .not. isrot ( ctx, r, ntol, dtol )  ) then
         call ctx%setmsg ( 'Input matrix was not a rotation.' )
         call ctx%sigerr ( 'SPICE(NOTAROTATION)'              )
         return

      end if
!
!     If our quaternion is C, S1, S2, S3 (the S's being the imaginary
!     part) and we let
!
!        CSi = C  * Si
!        Sij = Si * Sj
!
!     then the rotation matrix corresponding to our quaternion is:
!
!        R(1,1)      = 1.0D0 - 2*S22 - 2*S33
!        R(2,1)      =         2*S12 + 2*CS3
!        R(3,1)      =         2*S13 - 2*CS2
!
!        R(1,2)      =         2*S12 - 2*CS3
!        R(2,2)      = 1.0D0 - 2*S11 - 2*S33
!        R(3,2)      =         2*S23 + 2*CS1
!
!        R(1,3)      =         2*S13 + 2*CS2
!        R(2,3)      =         2*S23 - 2*CS1
!        R(3,3)      = 1.0D0 - 2*S11 - 2*S22
!
!        From the above we can see that
!
!           TRACE = 3 - 4*(S11 + S22 + S33)
!
!        so that
!
!           1.0D0 + TRACE = 4 - 4*(S11 + S22 + S33)
!                         = 4*(CC + S11 + S22 + S33)
!                         - 4*(S11 + S22 + S33)
!                         = 4*CC
!
!        Thus up to sign
!
!          C = 0.5D0 * DSQRT( 1.0D0 + TRACE )
!
!        But we also have
!
!          1.0D0 + TRACE - 2.0D0*R(i,i) = 4.0D0 - 4.0D0(Sii + Sjj + Skk)
!                                       - 2.0D0 + 4.0D0(Sjj + Skk )
!
!                                       = 2.0D0 - 4.0D0*Sii
!
!        So that
!
!           1.0D0 - TRACE + 2.0D0*R(i,i) = 4.0D0*Sii
!
!        and so up to sign
!
!           Si = 0.5D0*DSQRT( 1.0D0 - TRACE + 2.0D0*R(i,i) )
!
!        in addition to this observation, we note that all of the
!        product pairs can easily be computed
!
!         CS1 = (R(3,2) - R(2,3))/4.0D0
!         CS2 = (R(1,3) - R(3,1))/4.0D0
!         CS3 = (R(2,1) - R(1,2))/4.0D0
!         S12 = (R(2,1) + R(1,2))/4.0D0
!         S13 = (R(3,1) + R(1,3))/4.0D0
!         S23 = (R(2,3) + R(3,2))/4.0D0
!
!     But taking sums or differences of numbers that are nearly equal
!     or nearly opposite results in a loss of precision. As a result
!     we should take some care in which terms to select when computing
!     C, S1, S2, S3.  However, by simply starting with one of the
!     large quantities cc, S11, S22, or S33 we can make sure that we
!     use the best of the 6 quantities above when computing the
!     remaining components of the quaternion.
!

      trace  = r(1,1) + r(2,2) + r(3,3)
      mtrace = 1.0_wp - trace

      cc4    = 1.0_wp  + trace
      s114   = mtrace + 2.0_wp*r(1,1)
      s224   = mtrace + 2.0_wp*r(2,2)
      s334   = mtrace + 2.0_wp*r(3,3)

!
!     Note that if you simply add CC4 + S114 + S224 + S334
!     you get four. Thus at least one of the 4 terms is greater than 1.
!
      if ( 1.0_wp <= cc4 ) then
         c      =  dsqrt  ( cc4 * 0.25_wp )
         factor =  1.0_wp /( c   * 4.0_wp  )

         s(1)   = ( r(3,2) - r(2,3) )*factor
         s(2)   = ( r(1,3) - r(3,1) )*factor
         s(3)   = ( r(2,1) - r(1,2) )*factor

      else if ( 1.0_wp <= s114 ) then

         s(1)   = dsqrt  ( s114 * 0.25_wp )
         factor = 1.0_wp /( s(1) * 4.0_wp  )

         c      = ( r(3,2) - r(2,3) ) * factor
         s(2)   = ( r(1,2) + r(2,1) ) * factor
         s(3)   = ( r(1,3) + r(3,1) ) * factor

      else if ( 1.0_wp <= s224 ) then

         s(2)   = dsqrt  ( s224 * 0.25_wp )
         factor = 1.0_wp /( s(2) * 4.0_wp  )

         c      = ( r(1,3) - r(3,1) ) * factor
         s(1)   = ( r(1,2) + r(2,1) ) * factor
         s(3)   = ( r(2,3) + r(3,2) ) * factor

      else

         s(3)   = dsqrt  ( s334 * 0.25_wp )
         factor = 1.0_wp /( s(3) * 4.0_wp  )

         c      = ( r(2,1) - r(1,2) ) * factor
         s(1)   = ( r(1,3) + r(3,1) ) * factor
         s(2)   = ( r(2,3) + r(3,2) ) * factor

      end if
!
!     If the magnitude of this quaternion is not one, we polish it
!     up a bit.
!
      l2 = c*c + s(1)*s(1) + s(2)*s(2) + s(3)*s(3)

      if ( l2 /= 1.0_wp ) then
         polish = 1.0_wp/dsqrt(l2)
         c      =    c*polish
         s(1)   = s(1)*polish
         s(2)   = s(2)*polish
         s(3)   = s(3)*polish
      end if

      if ( c > 0.0_wp ) then
         q(0) = c
         q(1) = s(1)
         q(2) = s(2)
         q(3) = s(3)
      else
         q(0) = -c
         q(1) = -s(1)
         q(2) = -s(2)
         q(3) = -s(3)
      end if

      return
      end subroutine m2q

      !> Find the nearest point on a line to a specified point, and find
      !> the distance between the two points.
      !>
      !>## Detailed Input
      !>
      !>     LINPT,
      !>     LINDIR   are, respectively, a point and a direction vector
      !>              that define a line in 3-dimensional space. The
      !>              line is the set of points
      !>
      !>                 LINPT   +   t * LINDIR
      !>
      !>              where `t' is any real number.
      !>
      !>     POINT    is a point in 3-dimensional space.
      !>
      !>## Detailed Output
      !>
      !>     PNEAR    is the nearest point on the input line to the input
      !>              point.
      !>
      !>     DIST     is the distance between the input line and input
      !>              point.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the line direction vector LINDIR is the zero vector, the
      !>         error SPICE(ZEROVECTOR) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For every line L and point P, there is a unique closest point
      !>     on L to P. Call this closest point C. It is always true that
      !>     P - C  is perpendicular to L, and the length of P - C is called
      !>     the `distance' between P and L.
      !>
      !>## Examples
      !>
      !>     1)  Suppose a line passes through the point ( 1, 2, 3 ) and
      !>         has direction vector ( 0, 1, 1 ).  We wish to find the
      !>         closest point on the line to the point ( -6, 9, 10 ).  We
      !>         can use the code fragment
      !>
      !>            LINPT(1)   =  1.D0
      !>            LINPT(2)   =  2.D0
      !>            LINPT(3)   =  3.D0
      !>
      !>            LINDIR(1)  =  0.D0
      !>            LINDIR(2)  =  1.D0
      !>            LINDIR(3)  =  1.D0
      !>
      !>            POINT(1)   = -6.D0
      !>            POINT(2)   =  9.D0
      !>            POINT(3)   = 10.D0
      !>
      !>            CALL NPLNPT ( LINPT, LINDIR, POINT, PNEAR, DIST )
      !>
      !>         After the call, PNEAR will take the value
      !>
      !>            ( 1.D0, 9.D0, 10.D0 );
      !>
      !>         DIST will be 7.0.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 27-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 17-SEP-2014 (NJB)
      !>
      !>        Now uses discovery check-in.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VADD call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
      module subroutine nplnpt ( ctx, linpt, lindir, point, pnear, dist )
      use pepper_kinds, only: wp
      use pepper_math, only: vdist, vproj
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: linpt  ( 3 )  !! Point on a line and the line's direction vector.
      real(wp), intent(in) :: lindir ( 3 )  !! Point on a line and the line's direction vector.
      real(wp), intent(in) :: point  ( 3 )  !! A second point.
      real(wp), intent(out) :: pnear  ( 3 )  !! Nearest point on the line to POINT.
      real(wp), intent(out) :: dist  !! Distance between POINT and PNEAR.

      real(wp) :: proj  ( 3 )
      real(wp) :: trans ( 3 )

      if ( ctx%failed() ) return

!
!     We need a real direction vector to work with.
!
      if (  all(lindir == 0.0_wp)  )  then

         call ctx%setmsg ( 'Direction vector must be non-zero.' )
         call ctx%sigerr ( 'SPICE(ZEROVECTOR)'                  )
         return

      end if

!
!     We translate line and input point so as to put the line through
!     the origin.  Then the nearest point on the translated line to the
!     translated point TRANS is the projection of TRANS onto the line.
!
      trans = point - linpt
      call vproj ( trans,  lindir, proj  )
      pnear = proj + linpt

      dist = vdist ( pnear,  point )

      return
      end subroutine nplnpt

      !> Find the nearest point on a line segment to a given point.
      !>
      !>## Detailed Input
      !>
      !>     EP1,
      !>     EP2      are the endpoints of a line segment in 3-dimensional
      !>              space. EP1 and EP2 need not be distinct.
      !>
      !>     POINT    is an arbitrary point in 3-dimensional space.
      !>
      !>## Detailed Output
      !>
      !>     PNEAR    is the closest point on the line segment to POINT.
      !>
      !>     DIST     is the distance between POINT and PNEAR.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The input segment is allowed to be degenerate: it may be
      !>         a single point.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the nearest point on a line segment to a given
      !>        point in a simple case for which the results can easily be
      !>        checked.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM NPSGPT_EX1
      !>      IMPLICIT NONE
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,3F13.8)' )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DIST
      !>      DOUBLE PRECISION      ENDPT1 ( 3 )
      !>      DOUBLE PRECISION      ENDPT2 ( 3 )
      !>      DOUBLE PRECISION      PNEAR  ( 3 )
      !>      DOUBLE PRECISION      POINT  ( 3 )
      !>
      !>C
      !>C     Initialize the line segment's endpoints.
      !>C
      !>      CALL VPACK ( 1.D0, -2.D0, 3.D0, ENDPT1 )
      !>      CALL VPACK ( 1.D0,  2.D0, 3.D0, ENDPT2 )
      !>C
      !>C     Set the input point.
      !>C
      !>      CALL VPACK ( 1.D0,  0.D0, 0.D0, POINT )
      !>C
      !>C     Find the near point on the segment.
      !>C
      !>      CALL NPSGPT ( ENDPT1, ENDPT2, POINT, PNEAR, DIST )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,FMT1) 'Endpoint 1:  ', ENDPT1
      !>      WRITE (*,FMT1) 'Endpoint 2:  ', ENDPT2
      !>      WRITE (*,FMT1) 'Point:       ', POINT
      !>      WRITE (*,*) ' '
      !>      WRITE (*,FMT1) 'Near point:  ', PNEAR
      !>      WRITE (*,FMT1) 'Distance:    ', DIST
      !>      WRITE (*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Endpoint 1:     1.00000000  -2.00000000   3.00000000
      !>        Endpoint 2:     1.00000000   2.00000000   3.00000000
      !>        Point:          1.00000000   0.00000000   0.00000000
      !>
      !>        Near point:     1.00000000   0.00000000   3.00000000
      !>        Distance:       3.00000000
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 02-FEB-2016 (NJB)
      !>
      !>        Updated from DSKLIB Version 1.0.0, 20-MAR-2015 (NJB)
      module subroutine npsgpt ( ctx, ep1, ep2, point, pnear, dist )
      use pepper_kinds, only: wp
      use pepper_math, only: vdist, vdot
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: ep1   ( 3 )  !! Endpoints of a line segment.
      real(wp), intent(in) :: ep2   ( 3 )  !! Endpoints of a line segment.
      real(wp), intent(in) :: point ( 3 )  !! A point in 3-dimensional space.
      real(wp), intent(out) :: pnear ( 3 )  !! Nearest point on the line segment to POINT.
      real(wp), intent(out) :: dist  !! Distance between PNEAR and POINT.

      real(wp) :: seg    ( 3 )
      real(wp) :: segdot
      real(wp) :: lnear  ( 3 )
      real(wp) :: offset ( 3 )
      real(wp) :: offdot

      if ( ctx%failed() ) return

!
!     Find a direction vector defined by the endpoints.
!
      seg = ep2 - ep1

      if ( all(seg == 0.0_wp) ) then
!
!        The endpoints coincide, and both coincide with the
!        near point.
!
         pnear = ep1

         dist = vdist( ep1, point )

         return

      end if

!
!     Find the nearest point to POINT on the line defined by
!     EP1 and SEG.
!
      call nplnpt ( ctx, ep1, seg, point, lnear, dist )

      if ( ctx%failed() ) return

!
!     Determine whether LNEAR is on the segment, "before" EP1, or
!     "after" EP2, where SEG points in the "increasing" direction.
!
      offset = lnear - ep1

      offdot = vdot( offset, seg )

      if ( offdot < 0.0_wp ) then
!
!        The nearest point on the line precedes the first endpoint.
!        The closest point on the segment is the first endpoint.
!
         pnear = ep1

         dist = vdist ( ep1, point )

      else
!
!        See whether OFFSET is past the second endpoint. Compare
!        the dot product of OFFSET with SEG to that of SEG with
!        itself, since SEG is the offset of EP2 from EP1.
!
         segdot = vdot( seg, seg )

         if ( offdot > segdot ) then
!
!           The nearest point on the line follows the last endpoint.
!           The closest point on the segment is the last endpoint.
!
            pnear = ep2

            dist = vdist ( ep2, point )

         else
!
!           The near point is on the segment. LNEAR is actually the
!           solution.
!
            pnear = lnear

!
!           DIST was correctly set by the call to NPLNPT.
!
         end if

      end if

      return
      end subroutine npsgpt

      !> Make a SPICE plane from a normal vector and a constant.
      !>
      !>## Detailed Input
      !>
      !>     NORMAL,
      !>     KONST    are, respectively, a normal vector and constant
      !>              defining a plane. NORMAL need not be a unit
      !>              vector. Let the symbol < a, b > indicate the inner
      !>              product of vectors a and b; then the geometric
      !>              plane is the set of vectors X in three-dimensional
      !>              space that satisfy
      !>
      !>                 < X,  NORMAL >  =  KONST.
      !>
      !>## Detailed Output
      !>
      !>     PLANE    is a SPICE plane that represents the geometric
      !>              plane defined by NORMAL and KONST.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input vector NORMAL is the zero vector, the error
      !>         SPICE(ZEROVECTOR) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     SPICELIB geometry routines that deal with planes use the `plane'
      !>     data type to represent input and output planes. This data type
      !>     makes the subroutine interfaces simpler and more uniform.
      !>
      !>     The SPICELIB routines that produce SPICE planes from data that
      !>     define a plane are:
      !>
      !>        NVC2PL ( Normal vector and constant to plane )
      !>        NVP2PL ( Normal vector and point to plane    )
      !>        PSV2PL ( Point and spanning vectors to plane )
      !>
      !>     The SPICELIB routines that convert SPICE planes to data that
      !>     define a plane are:
      !>
      !>        PL2NVC ( Plane to normal vector and constant )
      !>        PL2NVP ( Plane to normal vector and point    )
      !>        PL2PSV ( Plane to point and spanning vectors )
      !>
      !>     Any of these last three routines may be used to convert this
      !>     routine's output, PLANE, to another representation of a
      !>     geometric plane.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Construct a SPICE plane from a normal vector and a constant.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM NVC2PL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 UBPL
      !>      PARAMETER             ( UBPL =   4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        KONST
      !>      DOUBLE PRECISION        PLANE  ( UBPL )
      !>      DOUBLE PRECISION        NORMAL ( 3    )
      !>      DOUBLE PRECISION        OKONST
      !>      DOUBLE PRECISION        ONORML ( 3    )
      !>
      !>C
      !>C     Set the normal vector and the constant defining the
      !>C     plane.
      !>C
      !>      DATA                    NORMAL /  1.D0, 1.D0, 1.D0 /
      !>
      !>      KONST = 23.D0
      !>
      !>      WRITE(*,'(A)') 'Inputs:'
      !>      WRITE(*,'(A,3F12.7)') '   Normal vector:', NORMAL
      !>      WRITE(*,'(A,F12.7)')  '   Constant     :', KONST
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Make a SPICE plane from NORMAL and KONST.
      !>C     NORMAL need not be a unit vector.
      !>C
      !>      CALL NVC2PL ( NORMAL, KONST,  PLANE         )
      !>
      !>C
      !>C     Print the results.
      !>C
      !>      CALL PL2NVC ( PLANE, ONORML, OKONST )
      !>      WRITE(*,'(A)') 'Generated plane:'
      !>      WRITE(*,'(A,3F12.7)') '   Normal vector:', ONORML
      !>      WRITE(*,'(A,F12.7)')  '   Constant     :', OKONST
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Inputs:
      !>           Normal vector:   1.0000000   1.0000000   1.0000000
      !>           Constant     :  23.0000000
      !>
      !>        Generated plane:
      !>           Normal vector:   0.5773503   0.5773503   0.5773503
      !>           Constant     :  13.2790562
      !>
      !>     2) Apply a linear transformation represented by a matrix to
      !>        a plane represented by a normal vector and a constant.
      !>
      !>        Find a normal vector and constant for the transformed plane.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM NVC2PL_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 UBPL
      !>      PARAMETER             ( UBPL =   4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        AXDEF  ( 3    )
      !>      DOUBLE PRECISION        KONST
      !>      DOUBLE PRECISION        PLANE  ( UBPL )
      !>      DOUBLE PRECISION        M      ( 3, 3 )
      !>      DOUBLE PRECISION        NORMAL ( 3    )
      !>      DOUBLE PRECISION        PLNDEF ( 3    )
      !>      DOUBLE PRECISION        POINT  ( 3    )
      !>      DOUBLE PRECISION        SPAN1  ( 3    )
      !>      DOUBLE PRECISION        SPAN2  ( 3    )
      !>      DOUBLE PRECISION        TKONST
      !>      DOUBLE PRECISION        TNORML ( 3    )
      !>      DOUBLE PRECISION        TPLANE ( UBPL )
      !>      DOUBLE PRECISION        TPOINT ( 3    )
      !>      DOUBLE PRECISION        TSPAN1 ( 3    )
      !>      DOUBLE PRECISION        TSPAN2 ( 3    )
      !>
      !>C
      !>C     Set the normal vector and the constant defining the
      !>C     initial plane.
      !>C
      !>      DATA                    NORMAL /
      !>     .               -0.1616904D0, 0.8084521D0, -0.5659165D0 /
      !>
      !>      DATA                    KONST  /  4.8102899D0 /
      !>
      !>C
      !>C     Define a transformation matrix to the right-handed
      !>C     reference frame having the +i unit vector as primary
      !>C     axis, aligned to the original frame's +X axis, and
      !>C     the -j unit vector as second axis, aligned to the +Y
      !>C     axis.
      !>C
      !>      DATA                    AXDEF  /  1.D0,  0.D0,  0.D0 /
      !>      DATA                    PLNDEF /  0.D0, -1.D0,  0.D0 /
      !>
      !>      CALL TWOVEC ( AXDEF, 1, PLNDEF, 2, M )
      !>
      !>C
      !>C     Make a SPICE plane from NORMAL and KONST, and then
      !>C     find a point in the plane and spanning vectors for the
      !>C     plane.  NORMAL need not be a unit vector.
      !>C
      !>      CALL NVC2PL ( NORMAL, KONST,  PLANE         )
      !>      CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 )
      !>
      !>C
      !>C     Apply the linear transformation to the point and
      !>C     spanning vectors.  All we need to do is multiply
      !>C     these vectors by M, since for any linear
      !>C     transformation T,
      !>C
      !>C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 )
      !>C
      !>C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2),
      !>C
      !>C     which means that T(POINT), T(SPAN1), and T(SPAN2)
      !>C     are a point and spanning vectors for the transformed
      !>C     plane.
      !>C
      !>      CALL MXV ( M, POINT, TPOINT )
      !>      CALL MXV ( M, SPAN1, TSPAN1 )
      !>      CALL MXV ( M, SPAN2, TSPAN2 )
      !>
      !>C
      !>C     Make a new SPICE plane TPLANE from the
      !>C     transformed point and spanning vectors, and find a
      !>C     unit normal and constant for this new plane.
      !>C
      !>      CALL PSV2PL ( TPOINT, TSPAN1, TSPAN2, TPLANE )
      !>      CALL PL2NVC ( TPLANE, TNORML, TKONST         )
      !>
      !>C
      !>C     Print the results.
      !>C
      !>      WRITE(*,'(A,3F12.7)') 'Unit normal vector:', TNORML
      !>      WRITE(*,'(A,F12.7)')  'Constant          :', TKONST
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Unit normal vector:  -0.1616904  -0.8084521   0.5659165
      !>        Constant          :   4.8102897
      !>
      !>## Restrictions
      !>
      !>     1)  No checking is done to prevent arithmetic overflow.
      !>
      !>## Literature References
      !>
      !>     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
      !>          7th Edition, Addison Wesley, 1988.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
      !>
      !>        Changed the input argument name CONST to KONST for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code examples.
      !>
      !>-    SPICELIB Version 1.1.1, 02-NOV-2009 (NJB)
      !>
      !>        Corrected header typo.
      !>
      !>-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VMINUS call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      module subroutine nvc2pl ( ctx, normal, konst, plane )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: unorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: normal (    3 )  !! A normal vector and constant defining a plane.
      real(wp), intent(in) :: konst  !! A normal vector and constant defining a plane.
      real(wp), intent(out) :: plane  ( ubpl )  !! An array representing the plane.


      real(wp) :: mag
      real(wp) :: tmpvec ( 3 )

!
!     This routine checks in only if an error is discovered.
!
      if ( ctx%failed() ) return

      call unorm ( normal, plane(nmlpos), mag )

!
!     The normal vector must be non-zero.
!
      if ( mag == 0 ) then

         call ctx%setmsg ( 'Plane''s normal must be non-zero.' )
         call ctx%sigerr ( 'SPICE(ZEROVECTOR)'                 )
         return

      end if

!
!     To find the plane constant corresponding to the unitized normal
!     vector, we observe that
!
!        < X, NORMAL > = KONST,
!
!     so
!
!        < X, NORMAL / || NORMAL || >   =   KONST / || NORMAL ||
!
      plane(conpos) =  konst / mag

!
!     The constant should be the distance of the plane from the
!     origin.  If the constant is negative, negate both it and the
!     normal vector.
!
      if ( plane(conpos) < 0.0_wp ) then

         plane(conpos) =   -plane(conpos)
         tmpvec = -plane(nmlpos:nmlpos+3-1)
         plane(nmlpos:nmlpos+3-1) = tmpvec

      end if

      return
      end subroutine nvc2pl

      !> Make a SPICE plane from a normal vector and a point.
      !>
      !>## Detailed Input
      !>
      !>     NORMAL,
      !>     POINT    are, respectively, a normal vector and point that
      !>              define a plane in three-dimensional space. NORMAL
      !>              need not be a unit vector. Let the symbol < a, b >
      !>              indicate the inner product of vectors a and b;
      !>              then the geometric plane is the set of vectors X
      !>              in three-dimensional space that satisfy
      !>
      !>                 < X - POINT, NORMAL >  =  0.
      !>
      !>## Detailed Output
      !>
      !>     PLANE    is a SPICE plane that represents the geometric
      !>              plane defined by POINT and NORMAL.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input vector NORMAL is the zero vector, the error
      !>         SPICE(ZEROVECTOR) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     SPICELIB geometry routines that deal with planes use the `plane'
      !>     data type to represent input and output planes. This data type
      !>     makes the subroutine interfaces simpler and more uniform.
      !>
      !>     The SPICELIB routines that produce SPICE planes from data that
      !>     define a plane are:
      !>
      !>        NVC2PL ( Normal vector and constant to plane )
      !>        NVP2PL ( Normal vector and point to plane    )
      !>        PSV2PL ( Point and spanning vectors to plane )
      !>
      !>     The SPICELIB routines that convert SPICE planes to data that
      !>     define a plane are:
      !>
      !>        PL2NVC ( Plane to normal vector and constant )
      !>        PL2NVP ( Plane to normal vector and point    )
      !>        PL2PSV ( Plane to point and spanning vectors )
      !>
      !>     Any of these last three routines may be used to convert this
      !>     routine's output, PLANE, to another representation of a
      !>     geometric plane.
      !>
      !>## Examples
      !>
      !>     1)  Project a vector V orthogonally onto a plane defined by POINT
      !>         and NORMAL. PROJ is the projection we want; it is the
      !>         closest vector in the plane to V.
      !>
      !>            CALL NVP2PL ( NORMAL, POINT,  PLANE )
      !>            CALL VPRJP  ( V,      PLANE,  PROJ  )
      !>
      !>     2)  Given a point in a plane and a normal vector, find the
      !>         distance of the plane from the origin. We make a
      !>         `plane' from the point and normal, then convert the
      !>         plane to a unit normal and constant. The constant CONST
      !>         is (according to the specification of PL2NVC) the distance of
      !>         the plane from the origin.
      !>
      !>            CALL NVP2PL ( NORMAL, POINT,  PLANE )
      !>            CALL PL2NVC ( PLANE,  NORMAL, CONST )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
      !>          7th Edition, Addison Wesley, 1988.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPILCIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VMINUS call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      module subroutine nvp2pl ( ctx, normal, point, plane )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: vdot, vhat
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: normal (    3 )  !! A normal vector and a point defining a plane.
      real(wp), intent(in) :: point  (    3 )  !! A normal vector and a point defining a plane.
      real(wp), intent(out) :: plane  ( ubpl )  !! An array representing the plane.


      real(wp) :: tmpvec ( 3 )

!
!     This routine checks in only if an error is discovered.
!
      if ( ctx%failed() ) return

!
!     The normal vector must be non-zero.
!
      if ( all(normal == 0.0_wp) ) then

         call ctx%setmsg ( 'Plane''s normal must be non-zero.' )
         call ctx%sigerr ( 'SPICE(ZEROVECTOR)'                 )
         return

      end if

      call vhat ( normal, plane(nmlpos) )

      plane(conpos)  =  vdot ( point, plane(nmlpos) )

!
!     The constant should be the distance of the plane from the
!     origin.  If the constant is negative, negate both it and the
!     normal vector.
!
      if ( plane(conpos) < 0.0_wp ) then

         plane(conpos) =   -plane(conpos)
         tmpvec = -plane(nmlpos:nmlpos+3-1)
         plane(nmlpos:nmlpos+3-1) = tmpvec

      end if

      return
      end subroutine nvp2pl

      !> Make a SPICE plane from a point and two spanning vectors.
      !>
      !>## Detailed Input
      !>
      !>     POINT,
      !>     SPAN1,
      !>     SPAN2    are, respectively, a point and two spanning vectors that
      !>              define a geometric plane in three-dimensional space. The
      !>              plane is the set of vectors
      !>
      !>                 POINT   +   s * SPAN1   +   t * SPAN2
      !>
      !>              where `s' and `t' are real numbers. The spanning vectors
      !>              SPAN1 and SPAN2 must be linearly independent, but they
      !>              need not be orthogonal or unitized.
      !>
      !>## Detailed Output
      !>
      !>     PLANE    is a SPICE plane that represents the geometric plane
      !>              defined by POINT, SPAN1, and SPAN2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If SPAN1 and SPAN2 are linearly dependent, i.e. the vectors
      !>         POINT, SPAN1, and SPAN2 do not define a plane, the error
      !>         SPICE(DEGENERATECASE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     SPICELIB geometry routines that deal with planes use the `plane'
      !>     data type to represent input and output planes. This data type
      !>     makes the subroutine interfaces simpler and more uniform.
      !>
      !>     The SPICELIB routines that produce SPICE planes from data that
      !>     define a plane are:
      !>
      !>        NVC2PL ( Normal vector and constant to plane )
      !>        NVP2PL ( Normal vector and point to plane    )
      !>        PSV2PL ( Point and spanning vectors to plane )
      !>
      !>     The SPICELIB routines that convert SPICE planes to data that
      !>     define a plane are:
      !>
      !>        PL2NVC ( Plane to normal vector and constant )
      !>        PL2NVP ( Plane to normal vector and point    )
      !>        PL2PSV ( Plane to point and spanning vectors )
      !>
      !>     Any of these last three routines may be used to convert this
      !>     routine's output, PLANE, to another representation of a
      !>     geometric plane.
      !>
      !>## Examples
      !>
      !>     1)  Project a vector V orthogonally onto a plane defined by
      !>         POINT, SPAN1, and SPAN2. PROJ is the projection we want; it
      !>         is the closest vector in the plane to V.
      !>
      !>            CALL PSV2PL ( POINT,   SPAN1,   SPAN2,   PLANE )
      !>            CALL VPRJP  ( V,       PLANE,   PROJ           )
      !>
      !>     2)  Find the plane determined by a spacecraft's position vector
      !>         relative to a central body and the spacecraft's velocity
      !>         vector. We assume that all vectors are given in the same
      !>         coordinate system.
      !>
      !>            C
      !>            C     POS is the spacecraft's position, relative to
      !>            C     the central body. VEL is the spacecraft's velocity
      !>            C     vector. POS is a point (vector, if you like) in
      !>            C     the orbit plane, and it is also one of the spanning
      !>            C     vectors of the plane.
      !>            C
      !>                  CALL PSV2PL ( POS, POS, VEL, PLANE )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
      !>          7th Edition, Addison Wesley, 1988.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VMINUS call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      module subroutine psv2pl ( ctx, point, span1, span2, plane )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: ucrss, vdot
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: point (    3 )  !! A point and two spanning vectors defining a
                                              !! plane.
      real(wp), intent(in) :: span1 (    3 )  !! A point and two spanning vectors defining a
                                              !! plane.
      real(wp), intent(in) :: span2 (    3 )  !! A point and two spanning vectors defining a
                                              !! plane.
      real(wp), intent(out) :: plane ( ubpl )  !! An array representing the plane.


      real(wp) :: tmpvec ( 3 )

!
!     This routine checks in only if an error is discovered.
!
      if ( ctx%failed() ) return

!
!     Find the unitized cross product of SPAN1 and SPAN2; this is our
!     unit normal vector, or possibly its inverse.
!
      call ucrss (  span1,  span2,  plane(nmlpos)  )

      if (  all(plane(nmlpos:nmlpos+3-1) == 0.0_wp)  ) then
         call ctx%setmsg ( 'Spanning vectors are parallel.' )
         call ctx%sigerr ( 'SPICE(DEGENERATECASE)'          )
         return
      end if

!
!     Find the plane constant corresponding to the unit normal
!     vector we've found.
!
      plane(conpos)  =  vdot ( plane(nmlpos), point )

!
!     The constant should be the distance of the plane from the
!     origin.  If the constant is negative, negate both it and the
!     normal vector.
!
      if ( plane(conpos) < 0.0_wp ) then

         plane(conpos) =   -plane(conpos)
         tmpvec = -plane(nmlpos:nmlpos+3-1)
         plane(nmlpos:nmlpos+3-1) = tmpvec

      end if

      return
      end subroutine psv2pl

      !> Compute the total area of a collection of triangular plates.
      !>
      !> The function returns the total area of the set of plates.
      !>
      !>## Detailed Input
      !>
      !>     NV       is the number of vertices comprising the plate
      !>              set.
      !>
      !>     VRTCES   is an array containing the plate model's vertices.
      !>              Elements
      !>
      !>                 VRTCES( 1, I )
      !>                 VRTCES( 2, I )
      !>                 VRTCES( 3, I )
      !>
      !>              are, respectively, the X, Y, and Z components of
      !>              the Ith vertex.
      !>
      !>              This routine doesn't associate units with the
      !>              vertices.
      !>
      !>     NP       is the number of triangular plates comprising the
      !>              plate set.
      !>
      !>     PLATES   is an array containing 3-tuples of integers
      !>              representing the set of plates. The elements of
      !>              PLATES are vertex indices. The vertex indices are
      !>              1-based: vertices have indices ranging from 1 to
      !>              NV. The elements
      !>
      !>                 PLATES( 1, I )
      !>                 PLATES( 2, I )
      !>                 PLATES( 3, I )
      !>
      !>              are, respectively, the indices of the vertices
      !>              comprising the Ith plate.
      !>
      !>              Note that the order of the vertices of a plate is
      !>              significant: the vertices must be ordered in the
      !>              positive (counterclockwise) sense with respect to
      !>              the outward normal direction associated with the
      !>              plate. In other words, if V1, V2, V3 are the
      !>              vertices of a plate, then
      !>
      !>                 ( V2 - V1 )  x  ( V3 - V2 )
      !>
      !>              points in the outward normal direction. Here
      !>              "x" denotes the vector cross product operator.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the total area of the input set of plates.
      !>     Each plate contributes the area of the triangle defined by the
      !>     plate's vertices.
      !>
      !>     If the components of the vertex array have length unit L, then the
      !>     output area has units
      !>
      !>         2
      !>        L
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the number of plates is less than 0, the error
      !>         SPICE(BADPLATECOUNT) is signaled.
      !>
      !>     2)  If the number of plates is positive and the number of vertices
      !>         is less than 3, the error SPICE(TOOFEWVERTICES) is signaled.
      !>
      !>     3)  If any plate contains a vertex index outside of the range
      !>
      !>            [1, NV]
      !>
      !>         the error SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the total area of a set of triangular
      !>     plates. The plates need not define a closed surface.
      !>
      !>     Examples of valid plate sets:
      !>
      !>        Tetrahedron
      !>        Box
      !>        Tiled ellipsoid
      !>        Tiled ellipsoid with one plate removed
      !>        Two disjoint boxes
      !>        Two boxes with intersection having positive volume
      !>        Single plate
      !>        Empty plate set
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the area of the pyramid defined by the four
      !>        triangular plates whose vertices are the 3-element
      !>        subsets of the set of vectors:
      !>
      !>           ( 0, 0, 0 )
      !>           ( 1, 0, 0 )
      !>           ( 0, 1, 0 )
      !>           ( 0, 0, 1 )
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PLTAR_EX1
      !>      IMPLICIT NONE
      !>C
      !>C     Compute the area of a plate model representing the
      !>C     pyramid with one vertex at the origin and the other
      !>C     vertices coinciding with the standard basis vectors.
      !>C
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      PLTAR
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               NVERT
      !>      PARAMETER           ( NVERT  = 4 )
      !>
      !>      INTEGER               NPLATE
      !>      PARAMETER           ( NPLATE = 4 )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      VRTCES ( 3, NVERT  )
      !>      DOUBLE PRECISION      AREA
      !>
      !>      INTEGER               PLATES ( 3, NPLATE )
      !>C
      !>C     Initial values
      !>C
      !>C     The plates defined below lie in the following planes,
      !>C     respectively:
      !>C
      !>C        Plate 1:    { P :  < P, (-1,  0,  0) > = 0 }
      !>C        Plate 2:    { P :  < P, ( 0, -1,  0) > = 0 }
      !>C        Plate 3:    { P :  < P, ( 0,  0, -1) > = 0 }
      !>C        Plate 4:    { P :  < P, ( 1,  1,  1) > = 1 }
      !>C
      !>      DATA                  PLATES /    1,     4,     3,
      !>     .                                  1,     2,     4,
      !>     .                                  1,     3,     2,
      !>     .                                  2,     3,     4 /
      !>
      !>      DATA                  VRTCES / 0.D0,  0.D0,  0.D0,
      !>     .                               1.D0,  0.D0,  0.D0,
      !>     .                               0.D0,  1.D0,  0.D0,
      !>     .                               0.D0,  0.D0,  1.D0 /
      !>
      !>      AREA = PLTAR ( NVERT, VRTCES, NPLATE, PLATES )
      !>
      !>      WRITE (*,*) 'Expected area   =    (3 + SQRT(3)) / 2'
      !>      WRITE (*,*) '                =    0.2366025403784438E+01'
      !>      WRITE (*,*) 'Computed area   = ', AREA
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Expected area   =    (3 + SQRT(3)) / 2
      !>                         =    0.2366025403784438E+01
      !>         Computed area   =    2.3660254037844384
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added DSK to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 21-OCT-2016 (NJB)
      !>
      !>        Original version 25-MAR-2016 (NJB)
      module real(wp) function pltar ( ctx, nv, vrtces, np, plates )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: vcrss, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nv  !! Number of vertices.
      real(wp), intent(in) :: vrtces ( 3, nv )  !! Array of vertices.
      integer(ip), intent(in) :: np  !! Number of triangular plates.
      integer(ip), intent(in) :: plates ( 3, np )  !! Array of plates.

      real(wp) :: cp    ( 3 )
      real(wp) :: edge1 ( 3 )
      real(wp) :: edge2 ( 3 )

      integer(ip) :: i
      integer(ip) :: j

!
!     The function must have an initial value.
!
      pltar  =  0.0_wp

      if ( ctx%failed() ) return

!
!     Check the vertex and plate counts.
!
      if ( np < 0 ) then

         call ctx%setmsg ( 'Plate count must be non-negative but NP = #.' )
         call ctx%errint ( '#',  np                                       )
         call ctx%sigerr ( 'SPICE(BADPLATECOUNT)'                         )
         return

      end if

      if ( np == 0 ) then
!
!        The area has already been set to zero.
!
         return

      end if

      if ( nv < 3 ) then

         call ctx%setmsg ( 'At least 3 vertices are needed, but NV = #.' )
         call ctx%errint ( '#',  nv                                      )
         call ctx%sigerr ( 'SPICE(TOOFEWVERTICES)'                       )
         return

      end if

!
!     Make sure the vertex indices are in the range [1, NV].
!
      do i = 1, np

         do j = 1, 3

            if (      ( plates(j,i) < 1  ) &
                 .or. ( plates(j,i) > nv )  ) then

               call ctx%setmsg ( 'Vertex indices must be in the range ' &
               //            '[1, NV] for all SPICE language versions. ' &
               //            'The input value of NV was #. Vertex ' &
               //            'index # in plate # was #. (The vertex ' &
               //            'and plate numbers in this message are ' &
               //            '1-based as well.)'                       )
               call ctx%errint ( '#',  nv                                  )
               call ctx%errint ( '#',  j                                   )
               call ctx%errint ( '#',  i                                   )
               call ctx%errint ( '#',  plates(j,i)                         )
               call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                  )
               return

            end if

         end do

      end do

      do i = 1, np
!
!        Take the cross product of two edges of the Ith plate.
!
         edge1 = vrtces(:,plates(2,i)) - vrtces(:,plates(1,i))

         edge2 = vrtces(:,plates(3,i)) - vrtces(:,plates(2,i))

         call vcrss ( edge1, edge2, cp )

!
!        The plate area is 1/2 of the magnitude of the
!        cross product.
!
         pltar = pltar  +  ( 0.5_wp * vnorm(cp) )

      end do

!
!     No check-out required, since the routine is not checked in
!     at this point.
!
      return
      end function pltar

      !> Find the nearest point on a triangular plate to a given point.
      !>
      !>## Detailed Input
      !>
      !>     POINT    is an arbitrary point in 3-dimensional space.
      !>
      !>     V1,
      !>     V2,
      !>     V3       are 3-vectors constituting the vertices of
      !>              a triangular plate.
      !>
      !>              The plate is allowed to be degenerate: it may
      !>              consist of a line segment or of a single point.
      !>
      !>## Detailed Output
      !>
      !>     PNEAR    is the closest point on the plate to POINT.
      !>              PNEAR is unique, since the plate is convex.
      !>
      !>     DIST     is the distance between POINT and PNEAR.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The input plate is allowed to be degenerate: it may be
      !>         a line segment or a single point.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the nearest point to the point (2,2,2) on a plate having
      !>        vertices at the unit basis vectors that lie along the positive
      !>        X, Y, and Z coordinate axes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PLTNP_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,3E15.7)' )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DIST
      !>      DOUBLE PRECISION      POINT  ( 3 )
      !>      DOUBLE PRECISION      PNEAR  ( 3 )
      !>      DOUBLE PRECISION      V1     ( 3 )
      !>      DOUBLE PRECISION      V2     ( 3 )
      !>      DOUBLE PRECISION      V3     ( 3 )
      !>
      !>C
      !>C     POINT is the input point.
      !>C
      !>      CALL VPACK ( 2.D0, 2.D0, 2.D0, POINT )
      !>C
      !>C     V1, V2, V3 are the vertices of a plate.
      !>C
      !>      CALL VPACK ( 1.D0, 0.D0, 0.D0, V1 )
      !>      CALL VPACK ( 0.D0, 1.D0, 0.D0, V2 )
      !>      CALL VPACK ( 0.D0, 0.D0, 1.D0, V3 )
      !>C
      !>C     Find the near point on the plate.
      !>C
      !>      CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST )
      !>
      !>      WRITE (*,*) ' '
      !>      WRITE (*,FMT1) 'Plate vertex 1 = ', V1
      !>      WRITE (*,FMT1) 'Plate vertex 2 = ', V2
      !>      WRITE (*,FMT1) 'Plate vertex 3 = ', V3
      !>      WRITE (*,FMT1) 'Input point    = ', POINT
      !>      WRITE (*,*)    ' '
      !>      WRITE (*,FMT1) 'Near point     = ', PNEAR
      !>      WRITE (*,FMT1) 'Distance       = ', DIST
      !>      WRITE (*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Plate vertex 1 =   0.1000000E+01  0.0000000E+00  0.0000000E+00
      !>        Plate vertex 2 =   0.0000000E+00  0.1000000E+01  0.0000000E+00
      !>        Plate vertex 3 =   0.0000000E+00  0.0000000E+00  0.1000000E+01
      !>        Input point    =   0.2000000E+01  0.2000000E+01  0.2000000E+01
      !>
      !>        Near point     =   0.3333333E+00  0.3333333E+00  0.3333333E+00
      !>        Distance       =   0.2886751E+01
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.3, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Edited code example output format for the solution to fit
      !>        within the $Examples section without modifications. Added
      !>        DSK to $Required_Readings.
      !>
      !>-    SPICELIB Version 1.1.2, 01-FEB-2016 (NJB)
      !>
      !>        Added code example to header.
      !>
      !>     DSKLIB Version 1.1.1, 19-MAR-2015 (NJB)
      !>
      !>        Fixed spelling error in header.
      !>
      !>     DSKLIB Version 1.1.0, 31-DEC-2014 (NJB)
      !>
      !>        Bug fix: vertex indices for outside case, near
      !>        point on 3rd edge were corrected.
      !>
      !>     DSKLIB Version 1.0.0, 29-SEP-2014 (NJB)
      module subroutine pltnp ( ctx, point, v1, v2, v3, pnear, dist )
      use pepper_kinds, only: wp
      use pepper_math, only: vcrss, vdist, vdot, vnorm, vperp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: point ( 3 )  !! A point in 3-dimensional space.
      real(wp), intent(in) :: v1    ( 3 )  !! Vertices of a triangular plate.
      real(wp), intent(in) :: v2    ( 3 )  !! Vertices of a triangular plate.
      real(wp), intent(in) :: v3    ( 3 )  !! Vertices of a triangular plate.
      real(wp), intent(out) :: pnear ( 3 )  !! Nearest point on the plate to POINT.
      real(wp), intent(out) :: dist  !! Distance between PNEAR and POINT.

      real(wp) :: d1
      real(wp) :: d2
      real(wp) :: d3
      real(wp) :: e1     ( 3 )
      real(wp) :: e2     ( 3 )
      real(wp) :: e3     ( 3 )
      real(wp) :: enorm1 ( 3 )
      real(wp) :: enorm2 ( 3 )
      real(wp) :: enorm3 ( 3 )
      real(wp) :: l1
      real(wp) :: l2
      real(wp) :: l3
      real(wp) :: normal ( 3 )
      real(wp) :: np1    ( 3 )
      real(wp) :: np2    ( 3 )
      real(wp) :: pdiff  ( 3 )
      real(wp) :: perp   ( 3 )

      logical :: degen
      logical :: in1
      logical :: in2
      logical :: in3

      if ( ctx%failed() ) return

!
!     Compute the plate's edges.
!
      e1 = v2 - v1
      e2 = v3 - v2
      e3 = v1 - v3

!
!     Compute a normal vector for the plate, if possible.
!     If the plate is degenerate, we'll find out at this point.
!
      call vcrss ( e1, e2, normal )

!
!     Compute the outward normals of the plate's edges in the
!     plate containing the plate.
!
      call vcrss ( e1, normal, enorm1 )
      call vcrss ( e2, normal, enorm2 )
      call vcrss ( e3, normal, enorm3 )

      degen =      all(normal == 0.0_wp) .or. all(enorm1 == 0.0_wp)  .or. all(enorm2 == 0.0_wp) .or. all(enorm3 == 0.0_wp)

      if ( degen ) then
!
!        The "plate" is a line segment or point. Determine
!        which case we have.
!
         l1 = vnorm(e1)
         l2 = vnorm(e2)
         l3 = vnorm(e3)

         if ( ( l1 == 0.0_wp ) .and. ( l2 == 0.0_wp ) ) then
!
!           Up to round-off error, the vertices coincide.
!           The vertex V1 for practical purposes is the plate.
!
            pnear = v1

            dist = vdist( pnear, point )

         else
!
!           The plate is a line segment having positive length.
!           One of the edges will coincide with the segment.
!           Determine which vertices are the endpoints.
!
            if ( l1 > max(l2, l3) ) then
!
!              The segment is bounded by V1 and V2.
!
               call npsgpt( ctx, v1, v2, point, pnear, dist )

            else if ( l2 > max(l3, l1) ) then
!
!              The segment is bounded by V2 and V3.
!
               call npsgpt( ctx, v2, v3, point, pnear, dist )

            else
!
!              The segment is bounded by V3 and V1.
!
               call npsgpt( ctx, v3, v1, point, pnear, dist )

            end if

         end if
!
!        The outputs are set for the degenerate cases.
!
         return

      end if

!
!     We have a non-degenerate plate. NORMAL has unit length.
!
!     We'll treat V1 as an origin in the plane containing
!     the plate. Find the offset of the POINT from V1, and
!     find the component of this offset orthogonal to NORMAL.
!
      pdiff = point - v1
      call vperp ( pdiff, normal, perp  )

!
!     Determine whether V1+PERP is inside the plate.
!
!     Note that the "line constants" for edges 1 and 3
!     are zero, since these edges contain V1. The line
!     constant for edge 2 is that of the offset of V2
!     from V1; this offset is edge 1.
!
      in1 = vdot( perp, enorm1 ) <= 0.0_wp
      in2 = vdot( perp, enorm2 ) <= vdot(e1,enorm2)
      in3 = vdot( perp, enorm3 ) <= 0.0_wp

      if ( in1 .and. in2 .and. in3 ) then
!
!        V1+PERP is inside the plate. It is the closest
!        point on the plate to POINT.
!
         pnear = v1 + perp
!
!        We have the near point; set the distance.
!
         dist = vdist( pnear, point )

      else
!
!        PERP is outside the plate. The nearest point
!        on the plate to POINT is on one of the edges.
!
!        We'll use the "in" flags to reduce the number
!        of point-edge distance computations.
!
         if ( ( .not. in1 ) .and. ( in2 .and. in3 ) ) then
!
!           The solution must be on the first edge.
!
            call npsgpt ( ctx, v1, v2, point, pnear, dist )

         else if ( ( .not. in2 ) .and. ( in3 .and. in1 ) ) then

!
!           The solution must be on the second edge.
!
            call npsgpt ( ctx, v2, v3, point, pnear, dist )

         else if ( ( .not. in3 ) .and. ( in1 .and. in2 ) ) then
!
!           The solution must be on the third edge.
!
            call npsgpt ( ctx, v3, v1, point, pnear, dist )

         else
!
!           Compute solutions on all three edges and pick
!           the best one.
!
            call npsgpt ( ctx, v1, v2, point, np1,   d1 )
            call npsgpt ( ctx, v2, v3, point, np2,   d2 )
            call npsgpt ( ctx, v3, v1, point, pnear, d3 )

            if ( d1 <= min(d2, d3) ) then

               pnear = np1
               dist = d1

            else if ( d2 <= min(d3, d1) ) then

               pnear = np2
               dist = d2

            else
!
!              PNEAR is already set.
!
               dist = d3

            end if

         end if

      end if

      return
      end subroutine pltnp

      !> Compute the volume of a three-dimensional region bounded by a
      !> collection of triangular plates.
      !>
      !> The function returns the volume of the spatial region bounded
      !> by the plates.
      !>
      !>## Detailed Input
      !>
      !>     NV       is the number of vertices comprising the plate
      !>              model.
      !>
      !>     VRTCES   is an array containing the plate model's vertices.
      !>              Elements
      !>
      !>                 VRTCES( 1, I )
      !>                 VRTCES( 2, I )
      !>                 VRTCES( 3, I )
      !>
      !>              are, respectively, the X, Y, and Z components of
      !>              the Ith vertex.
      !>
      !>              This routine doesn't associate units with the
      !>              vertices.
      !>
      !>     NP       is the number of triangular plates comprising the
      !>              plate model.
      !>
      !>     PLATES   is an array containing 3-tuples of integers
      !>              representing the model's plates. The elements of
      !>              PLATES are vertex indices. The vertex indices are
      !>              1-based: vertices have indices ranging from 1 to
      !>              NV. The elements
      !>
      !>                 PLATES( 1, I )
      !>                 PLATES( 2, I )
      !>                 PLATES( 3, I )
      !>
      !>              are, respectively, the indices of the vertices
      !>              comprising the Ith plate.
      !>
      !>              Note that the order of the vertices of a plate is
      !>              significant: the vertices must be ordered in the
      !>              positive (counterclockwise) sense with respect to
      !>              the outward normal direction associated with the
      !>              plate. In other words, if V1, V2, V3 are the
      !>              vertices of a plate, then
      !>
      !>                 ( V2 - V1 )  x  ( V3 - V2 )
      !>
      !>              points in the outward normal direction. Here
      !>              "x" denotes the vector cross product operator.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the volume of the spatial region bounded
      !>     by the plates.
      !>
      !>     If the components of the vertex array have length unit L, then the
      !>     output volume has units
      !>
      !>         3
      !>        L
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The input plate model must define a spatial region with
      !>         a boundary. This routine does not check the inputs to
      !>         verify this condition. See the $Restrictions section below.
      !>
      !>     2)  If the number of vertices is less than 4, the error
      !>         SPICE(TOOFEWVERTICES) is signaled.
      !>
      !>     3)  If the number of plates is less than 4, the error
      !>         SPICE(TOOFEWPLATES) is signaled.
      !>
      !>     4)  If any plate contains a vertex index outside of the range
      !>
      !>            [1, NV]
      !>
      !>         the error SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the volume of a spatial region bounded by
      !>     a set of triangular plates. If the plate set does not actually
      !>     form the boundary of a spatial region, the result of this routine
      !>     is invalid.
      !>
      !>     Examples:
      !>
      !>        Valid inputs
      !>        ------------
      !>        Tetrahedron
      !>        Box
      !>        Tiled ellipsoid
      !>        Two disjoint boxes
      !>
      !>        Invalid inputs
      !>        --------------
      !>        Single plate
      !>        Tiled ellipsoid with one plate removed
      !>        Two boxes with intersection having positive volume
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the volume of the pyramid defined by the four
      !>        triangular plates whose vertices are the 3-element
      !>        subsets of the set of vectors
      !>
      !>           ( 0, 0, 0 )
      !>           ( 1, 0, 0 )
      !>           ( 0, 1, 0 )
      !>           ( 0, 0, 1 )
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>C
      !>C     Compute the volume of a plate model representing the
      !>C     pyramid with one vertex at the origin and the other
      !>C     vertices coinciding with the standard basis vectors.
      !>C
      !>      PROGRAM PLTVOL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      PLTVOL
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               NVERT
      !>      PARAMETER           ( NVERT  = 4 )
      !>
      !>      INTEGER               NPLATE
      !>      PARAMETER           ( NPLATE = 4 )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      VRTCES ( 3, NVERT  )
      !>      DOUBLE PRECISION      VOL
      !>
      !>      INTEGER               PLATES ( 3, NPLATE )
      !>
      !>C
      !>C     Let the notation
      !>C
      !>C        < A, B >
      !>C
      !>C     denote the dot product of vectors A and B.
      !>C
      !>C     The plates defined below lie in the following planes,
      !>C     respectively:
      !>C
      !>C        Plate 1:    { P :  < P, (-1,  0,  0) > = 0 }
      !>C        Plate 2:    { P :  < P, ( 0, -1,  0) > = 0 }
      !>C        Plate 3:    { P :  < P, ( 0,  0, -1) > = 0 }
      !>C        Plate 4:    { P :  < P, ( 1,  1,  1) > = 1 }
      !>C
      !>      DATA                  PLATES /    1,     4,     3,
      !>     .                                  1,     2,     4,
      !>     .                                  1,     3,     2,
      !>     .                                  2,     3,     4 /
      !>
      !>      DATA                  VRTCES / 0.D0,  0.D0,  0.D0,
      !>     .                               1.D0,  0.D0,  0.D0,
      !>     .                               0.D0,  1.D0,  0.D0,
      !>     .                               0.D0,  0.D0,  1.D0 /
      !>
      !>      VOL = PLTVOL ( NVERT, VRTCES, NPLATE, PLATES )
      !>
      !>      WRITE (*,*) 'Expected volume =    1/6'
      !>      WRITE (*,*) 'Computed volume = ', VOL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Expected volume =    1/6
      !>         Computed volume =   0.16666666666666666
      !>
      !>## Restrictions
      !>
      !>     1)  The plate collection must describe a surface and enclose a
      !>         volume such that the divergence theorem (see [1]) is
      !>         applicable.
      !>
      !>## Literature References
      !>
      !>     [1]  T. Apostol, "Calculus, Vol. II," John Wiley & Sons, 1969.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added DSK to
      !>        $Required_Reading. Updated code example comments.
      !>
      !>-    SPICELIB Version 1.0.0, 24-OCT-2016 (NJB)
      !>
      !>        Based on original 11-FEB-2011
      module real(wp) function pltvol ( ctx, nv, vrtces, np, plates )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: det
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nv  !! Number of vertices.
      real(wp), intent(in) :: vrtces ( 3, nv )  !! Array of vertices.
      integer(ip), intent(in) :: np  !! Number of triangular plates.
      integer(ip), intent(in) :: plates ( 3, np )  !! Array of plates.

      real(wp) :: m ( 3, 3 )

      integer(ip) :: i
      integer(ip) :: j

!
!     The function must have an initial value.
!
      pltvol  =  0.0_wp

      if ( ctx%failed() ) return

!
!     Check the vertex and plate counts.
!
      if ( nv < 4 ) then

         call ctx%setmsg ( 'At least 4 vertices are needed, but NV = #.' )
         call ctx%errint ( '#',  nv                                      )
         call ctx%sigerr ( 'SPICE(TOOFEWVERTICES)'                       )
         return

      end if

      if ( np < 4 ) then

         call ctx%setmsg ( 'At least 4 plates are needed, but NP = #.' )
         call ctx%errint ( '#',  np                                    )
         call ctx%sigerr ( 'SPICE(TOOFEWPLATES)'                       )
         return

      end if

!
!     Make sure the vertex indices are in the range [1, NV].
!
      do i = 1, np

         do j = 1, 3

            if (      ( plates(j,i) < 1  ) &
                 .or. ( plates(j,i) > nv )  ) then

               call ctx%setmsg ( 'Vertex indices must be in the range ' &
               //            '[1, NV] for all SPICE language versions. ' &
               //            'The input value of NV was #. Vertex ' &
               //            'index # in plate # was #. (The vertex ' &
               //            'and plate numbers in this message are ' &
               //            '1-based as well.)'                       )
               call ctx%errint ( '#',  nv                                  )
               call ctx%errint ( '#',  j                                   )
               call ctx%errint ( '#',  i                                   )
               call ctx%errint ( '#',  plates(j,i)                         )
               call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                  )
               return

            end if

         end do

      end do

!
!     The volume computation below requires only a few lines of code,
!     but it might not be obvious that it works. An explanation
!     follows.
!
!        Notation
!        --------
!
!        The expression
!
!           A x B
!
!        denotes the cross product of vectors A and B. The notation
!
!           < A, B >
!
!        denotes the dot product of these vectors.
!
!        The expression
!
!           ||A||
!
!        denotes the Euclidean (L2) norm of A, and
!
!           ^
!           A
!
!        is the unit-length vector aligned with A, namely
!
!           A / ||A||
!
!        provided A is non-zero.
!
!        Geometric assumptions
!        ---------------------
!
!        The algorithm used here assumes, but does not attempt to
!        verify, that the input plate model represents the boundary of
!        a spatial region. The region has an "inside" and an "outside,"
!        so it makes sense to speak of surface normal vectors that are
!        either "inward pointing" or "outward pointing."

!        The input plates are assumed to have vertex indices ordered in
!        the positive sense about their respective outward normal
!        vectors. So if a plate's vertices are A, B, C, then the
!        plate's sides are
!
!           B - A,  C - B,  A - C
!
!        and all of
!
!           ( B - A ) x ( C - B )
!           ( C - B ) x ( A - C )
!           ( A - C ) x ( B - A )
!
!        are outward normal vectors.
!
!        Each plate lies in a plane, so all points on the plate satisfy
!        an equation of the form
!
!           < P, N > = constant
!
!        where N is an outward normal vector for that plate. If the
!        plane constant is positive, we say that the outward normal
!        vector "points away" from the origin; if the constant is
!        negative, we say that the outward normal "points toward" the
!        origin. This is a linguistic shortcut for the unwieldy phrases
!        "the component of the outward normal parallel to P points away
!        from the origin" or ... (the negative counterpart phrase).
!
!        Relationship of volume and surface
!        ----------------------------------
!
!        Given a suitable 3-D spatial region, the divergence theorem
!        (see [1]) says that the volume integral, over that region, of
!        the divergence of a vector field F (div F) is equal to the
!        surface integral of F over the boundary of the region. So if
!        one picks a vector field F having divergence identically equal
!        to 1, the surface integral of F equals the volume of the
!        region.
!
!        We can use this fact to set up a surface integral that yields
!        the volume enclosed by a set of triangular plates.
!
!        A faster algorithm
!        ------------------
!
!        However, there's a more efficient approach: we can compute the
!        volume of the model by summing the signed volumes of the
!        pyramids whose bases are the plates and whose apexes coincide
!        with the origin. Plates with outward normal vectors pointing
!        away from the origin contribute positive volume increments;
!        plates with outward normal vectors pointing toward the origin
!        contribute negative volume increments.
!
!        We'll show that these two methods are equivalent, and we'll
!        use the more efficient method in our code.
!
!        The surface integral
!        --------------------
!
!        The field
!
!           F( x, y, z )  =  (1/3)( x, y, z )
!
!        has divergence 1. Let the vectors
!
!           A, B, C
!
!        be the vertices of a plate, and let
!
!           N = ( B - A )  x  ( C - B )
!
!        be an outward normal of the plate. Then
!
!           < N, X >
!
!        is constant for all X in the plane containing the plate, so we
!        can pick the vertex A to stand in for X.
!
!        Then the surface integral of F over the plate is simply
!
!             ^
!           < N,  A/3 > * plate_area
!
!        Equivalence of the methods
!        --------------------------
!
!        If we show that the above integral equals the volume
!        contribution of the pyramid corresponding to the plate, the
!        validity of summing the signed pyramid volumes is established.
!
!        Below, let
!
!           S1, S2 be the plate sides (B-A), (C-B).
!
!           N be the outward normal vector S1 x S2.
!
!           plate_area be...the area of the plate.
!
!        Then
!
!           N                  =   S1 x S2
!                              =   ( B - A ) x ( C - B )
!                              =   ( B x C ) - ( A x C ) + ( A x B )
!
!        and since A is orthogonal to both
!
!           A x C
!           A x B
!
!        we have
!
!             ^
!           < N,  A >          =   <  ( B x C )/||N||,  A  >
!
!        Then
!
!           plate_area         =    (1/2) * ||( S1 x S2 )||
!                              =    ||N|| / 2
!
!        So the surface integral of F over the plate is
!
!                            ^
!           plate_area * ( < N, A/3 > )
!
!                              =  (1/6) * ||N||
!                                       * < (B x C)/||N||, A >
!
!                              =  < ( B x C ), A > / 6
!
!                              =  < ( A X B ), C > / 6
!
!        On the other hand, letting
!
!           base_area
!
!        denote the area of the triangle defined by A, B, and the
!        origin, the signed volume of the pyramid defined by A, B, and
!        C is
!
!           1/3 * base_area * height   =  1/3 *  < (A x B)/2,  C  >
!
!                                      =  < ( A X B ), C > / 6
!
!        This shows the signed pyramid volume is equal to the surface
!        integral of F over the plate.
!
!     We proceed to compute the signed volume of each pyramid whose
!     base is a plate and whose vertex is the origin.
!
!     Note that PLTVOL has already been initialized to 0.D0.
!
      do i = 1, np
!
!        Pack the vertices of the current plate into a 3x3 matrix.
!
         do j = 1, 3

            m(:,j) = vrtces(:,plates(j,i))

         end do

!
!        The determinant of M gives the volume of the parallelepiped
!        spanned by the origin-vertex vectors. The corresponding
!        pyramid has volume 1/3 * base_area * height, and
!        the area of the pyramid's base is half that of base of the
!        parallelepiped. So the determinant must be divided by 6.
!
         pltvol = pltvol  +  (  det( m ) / 6.0_wp  )

      end do

!
!     No check-out required, since the routine is not checked in
!     at this point.
!
      return
      end function pltvol

      !> Estimate the derivative of a function by finding the derivative
      !> of a quadratic approximating function. This derivative estimate
      !> is equivalent to that found by computing the average of forward
      !> and backward differences.
      !>
      !>## Detailed Input
      !>
      !>     NDIM     is the dimension of the function to be
      !>              differentiated. The derivative of each
      !>              function component will be found.
      !>
      !>     F0       is an array of NDIM function values at a point on
      !>              the real line; we'll refer to this point as X0.
      !>
      !>     F2       is an array of NDIM function values at a second
      !>              point on the real line; we'll refer to this point
      !>              as X2. The points X0 and X2 must satisfy
      !>
      !>                 X2 = X0 + 2 * DELTA
      !>
      !>     DELTA    is one half of the difference between X2 and X0:
      !>
      !>                 DELTA = ( X2 - X0 ) / 2
      !>
      !>              DELTA may be negative but must be non-zero.
      !>
      !>## Detailed Output
      !>
      !>     DFDT     is an N-dimensional vector representing an estimate
      !>              of the derivative of the input function at the
      !>              midpoint X1 of the interval between X0 and X2.
      !>
      !>              The Ith component of DFDT is
      !>
      !>                 ( 1 / (2*DELTA) ) * ( F2(I) - F0(I) )
      !>
      !>              We may regard this estimate as the derivative
      !>              at X1 of a parabola fitted to the points
      !>
      !>                  ( X0, F0(I) ),  ( X2, F2(I) )
      !>
      !>              We may also regard this derivative as the average
      !>              of the forward and backward first-order
      !>              differences of the input function defined by
      !>              F0(I), F2(I), and DELTA.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If DELTA is zero, the error SPICE(DIVIDEBYZERO) is signaled.
      !>
      !>     2)  If NDIM is less than 1, this routine will fail in a
      !>         system-dependent manner.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine estimates the derivative of a vector-valued function
      !>     using the average of forward and backward differences.
      !>
      !>     The derivative estimate computed by this routine is equivalent to
      !>     that obtained by fitting each component of the function with a
      !>     parabola at the points
      !>
      !>        (X0, f(X0)), (X1, f(X1)), (X2, f(X2))
      !>
      !>     where
      !>
      !>         X0  =  X1 - DELTA
      !>         X2  =  X1 + DELTA
      !>
      !>     and finding the derivative of the parabolas at X1.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Estimate the derivative of x**2 at x = 2.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>       PROGRAM QDERIV_EX1
      !>       IMPLICIT NONE
      !>
      !>       DOUBLE PRECISION     DELTA
      !>       DOUBLE PRECISION     DFDT  (1)
      !>       DOUBLE PRECISION     F0    (1)
      !>       DOUBLE PRECISION     F2    (1)
      !>       INTEGER              N
      !>
      !>       N     = 1
      !>       DELTA = 1.D-3
      !>       F0(1) = ( 2.D0 - DELTA ) ** 2.D0
      !>       F2(1) = ( 2.D0 + DELTA ) ** 2.D0
      !>
      !>       CALL QDERIV ( N, F0, F2, DELTA, DFDT )
      !>
      !>       WRITE ( *, '(1X,A,E25.16)'  ) '4 - DFDT(1) = ',
      !>      .                               4 - DFDT(1)
      !>       END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         4 - DFDT(1) =    0.4547473508864641E-12
      !>
      !>        Note that the difference displayed is platform-dependent, but
      !>        should be on the order of 1.E-12.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-AUG-2020 (JDR)
      !>
      !>        Changed input argument name "N" to "NDIM" for consistency with
      !>        other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
      module subroutine qderiv ( ctx, ndim, f0, f2, delta, dfdt )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ndim  !! Dimension of function to be differentiated.
      real(wp), intent(in) :: f0    ( ndim )  !! Function values at left endpoint.
      real(wp), intent(in) :: f2    ( ndim )  !! Function values at right endpoint.
      real(wp), intent(in) :: delta  !! Separation of abscissa points.
      real(wp), intent(out) :: dfdt  ( ndim )  !! Derivative vector.

!
!     Use discovery check-in.
!
      if ( delta == 0.0_wp ) then

         call ctx%setmsg ( 'Delta abscissa value is zero; a non-zero ' // &
                       'value is required.'                        )
         call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                       )
         return

      end if
!
!     Our derivative estimate is
!
!            1/2 * (   Backward_difference / DELTA
!                    + Forward_difference  / DELTA )
!
!        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X1) ) +  ( F(X1) - F(X0) )
!
!        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X0) )
!
!        =    (0.5/DELTA) * F(X2)  +  (-0.5/DELTA) * F(X0)
!
      dfdt = (0.5_wp/delta)*f2 + (-0.5_wp/delta)*f0

      return
      end subroutine qderiv

      !> Compute the axis of the rotation given by an input matrix
      !> and the angle of the rotation about that axis.
      !>
      !>## Detailed Input
      !>
      !>     MATRIX   is a 3x3 rotation matrix in double precision.
      !>
      !>## Detailed Output
      !>
      !>     AXIS     is a unit vector pointing along the axis of the
      !>              rotation. In other words, AXIS is a unit eigenvector
      !>              of the input matrix, corresponding to the eigenvalue
      !>              1. If the input matrix is the identity matrix, AXIS
      !>              will be the vector (0, 0, 1). If the input rotation is
      !>              a rotation by PI radians, both AXIS and -AXIS may be
      !>              regarded as the axis of the rotation.
      !>
      !>     ANGLE    is the angle between V and MATRIX*V for any non-zero
      !>              vector V orthogonal to AXIS. Angle is given in
      !>              radians. The angle returned will be in the range from
      !>              0 to PI.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input matrix is not a rotation matrix (where a fairly
      !>         loose tolerance is used to check this), an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     2)  If the input matrix is the identity matrix, this routine
      !>         returns an angle of 0.0, and an axis of ( 0.0, 0.0, 1.0 ).
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Every rotation matrix has an axis A such any vector, V, parallel
      !>     to that axis satisfies the equation
      !>
      !>        V = MATRIX * V
      !>
      !>     This routine returns a unit vector AXIS parallel to the axis of
      !>     the input rotation matrix. Moreover for any vector W orthogonal
      !>     to the axis of the rotation
      !>
      !>        AXIS  and  W x MATRIX*W
      !>
      !>        (where "x" denotes the cross product operation)
      !>
      !>     will be positive scalar multiples of one another (at least to
      !>     within the ability to make such computations with double
      !>     precision arithmetic, and under the assumption that the MATRIX
      !>     does not represent a rotation by zero or Pi radians).
      !>
      !>     The angle returned will be the angle between W and MATRIX*W for
      !>     any vector orthogonal to AXIS.
      !>
      !>     If the input matrix is a rotation by 0 or PI radians some choice
      !>     must be made for the AXIS returned. In the case of a rotation by
      !>     0 radians, AXIS is along the positive z-axis. In the case of a
      !>     rotation by 180 degrees, two choices are
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given an axis and an angle of rotation about that axis,
      !>        determine the rotation matrix. Using this matrix as input,
      !>        compute the axis and angle of rotation, and verify that
      !>        the later are equivalent by subtracting the original matrix
      !>        and the one resulting from using the computed axis and angle
      !>        of rotation on the AXISAR call.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RAXISA_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      TWOPI
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ANGLE
      !>      DOUBLE PRECISION      ANGOUT
      !>      DOUBLE PRECISION      AXIS   ( 3    )
      !>      DOUBLE PRECISION      AXOUT  ( 3    )
      !>      DOUBLE PRECISION      R      ( 3, 3 )
      !>      DOUBLE PRECISION      ROUT   ( 3, 3 )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define an axis and an angle for rotation.
      !>C
      !>      DATA                  AXIS  /  1.D0, 2.D0, 3.D0  /
      !>
      !>      ANGLE = 0.1D0 * TWOPI()
      !>
      !>C
      !>C     Determine the rotation matrix.
      !>C
      !>      CALL AXISAR ( AXIS, ANGLE, R )
      !>
      !>C
      !>C     Now calculate the rotation axis and angle based on the
      !>C     matrix as input.
      !>C
      !>      CALL RAXISA ( R, AXOUT, ANGOUT )
      !>
      !>      WRITE(*,'(A,3F12.8)') 'Axis :', AXOUT
      !>      WRITE(*,'(A,F12.8)')  'Angle:', ANGOUT
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Now input the AXOUT and ANGOUT to AXISAR to
      !>C     compare against the original rotation matrix R.
      !>C
      !>      WRITE(*,'(A)') 'Difference between input and output '
      !>     .           //  'matrices:'
      !>
      !>      CALL AXISAR ( AXOUT, ANGOUT, ROUT )
      !>
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(3F20.16)') ROUT(I,1) - R(I,1),
      !>     .                        ROUT(I,2) - R(I,2),
      !>     .                        ROUT(I,3) - R(I,3)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Axis :  0.26726124  0.53452248  0.80178373
      !>        Angle:  0.62831853
      !>
      !>        Difference between input and output matrices:
      !>         -0.0000000000000001  0.0000000000000000  0.0000000000000000
      !>          0.0000000000000001 -0.0000000000000001  0.0000000000000000
      !>          0.0000000000000000  0.0000000000000001  0.0000000000000000
      !>
      !>        Note, the zero matrix is accurate to round-off error. A
      !>        numerical demonstration of equality.
      !>
      !>     2) This routine can be used to numerically approximate the
      !>        instantaneous angular velocity vector of a rotating object.
      !>
      !>        Suppose that R(t) is the rotation matrix whose columns
      !>        represent the inertial pointing vectors of the body-fixed axes
      !>        of an object at time t.
      !>
      !>        Then the angular velocity vector points along the vector given
      !>        by:
      !>
      !>                                T
      !>            limit  AXIS( R(t+h)R )
      !>            h-->0
      !>
      !>        And the magnitude of the angular velocity at time t is given
      !>        by:
      !>
      !>                               T
      !>           d ANGLE ( R(t+h)R(t) )
      !>           ----------------------   at   h = 0
      !>                     dh
      !>
      !>        This code example computes the instantaneous angular velocity
      !>        vector of the Earth at 2000 Jan 01 12:00:00 TDB.
      !>
      !>        Use the PCK kernel below to load the required triaxial
      !>        ellipsoidal shape model and orientation data for the Earth.
      !>
      !>           pck00010.tpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RAXISA_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ANGLE
      !>      DOUBLE PRECISION      ANGVEL ( 3    )
      !>      DOUBLE PRECISION      AXIS   ( 3    )
      !>      DOUBLE PRECISION      INFROT ( 3, 3 )
      !>      DOUBLE PRECISION      H
      !>      DOUBLE PRECISION      RT     ( 3, 3 )
      !>      DOUBLE PRECISION      RTH    ( 3, 3 )
      !>      DOUBLE PRECISION      T
      !>
      !>C
      !>C     Load a PCK file containing a triaxial
      !>C     ellipsoidal shape model and orientation
      !>C     data for the Earth.
      !>C
      !>      CALL FURNSH ( 'pck00010.tpc' )
      !>
      !>C
      !>C     Load time into the double precision variable T
      !>C     and the delta time (1 ms) into the double precision
      !>C     variable H
      !>C
      !>      T = 0.D0
      !>      H = 1D-3
      !>
      !>C
      !>C     Get the rotation matrices from IAU_EARTH to J2000
      !>C     at T and T+H.
      !>C
      !>      CALL PXFORM ( 'IAU_EARTH', 'J2000', T,   RT  )
      !>      CALL PXFORM ( 'IAU_EARTH', 'J2000', T+H, RTH )
      !>
      !>C
      !>C     Compute the infinitesimal rotation R(t+h)R(t)**T
      !>C
      !>      CALL MXMT ( RTH, RT, INFROT )
      !>
      !>C
      !>C     Compute the AXIS and ANGLE of the infinitesimal rotation
      !>C
      !>      CALL RAXISA ( INFROT, AXIS, ANGLE )
      !>
      !>C
      !>C     Scale AXIS to get the angular velocity vector
      !>C
      !>      CALL VSCL ( ANGLE/H, AXIS, ANGVEL )
      !>
      !>C
      !>C     Output the results.
      !>C
      !>      WRITE(*,*) 'Instantaneous angular velocity vector:'
      !>      WRITE(*,'(3F15.10)') ANGVEL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Instantaneous angular velocity vector:
      !>           0.0000000000   0.0000000000   0.0000729212
      !>
      !>## Restrictions
      !>
      !>     1)  If the input matrix is not a rotation matrix but is close
      !>         enough to pass the tests this routine performs on it, no error
      !>         will be signaled, but the results may have poor accuracy.
      !>
      !>     2)  The input matrix is taken to be an object that acts on
      !>         (rotates) vectors---it is not regarded as a coordinate
      !>         transformation. To find the axis and angle of a coordinate
      !>         transformation, input the transpose of that matrix to this
      !>         routine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.2.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code examples.
      !>
      !>-    SPICELIB Version 2.1.2, 02-JAN-2008 (EDW)
      !>
      !>        Minor edit to the ANGLE declaration strictly
      !>        identifying the constant as a double.
      !>
      !>        From:
      !>
      !>           ANGLE = 2.0 * DATAN2( VNORM(Q(1)), Q(0) )
      !>
      !>        To:
      !>
      !>           ANGLE = 2.D0 * DATAN2( VNORM(Q(1)), Q(0) )
      !>
      !>-    SPICELIB Version 2.1.1, 05-JAN-2005 (NJB)
      !>
      !>        Minor edits and formatting changes were made.
      !>
      !>-    SPICELIB Version 2.1.0, 30-MAY-2002 (FST)
      !>
      !>        This routine now participates in error handling properly.
      !>
      !>-    SPICELIB Version 2.0.0, 19-SEP-1999 (WLT)
      !>
      !>        The routine was re-written so as to avoid the numerical
      !>        instabilities present in the previous implementation for
      !>        rotations very near zero or 180 degrees.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
      !>
      !>        The declaration for the SPICELIB function PI is now
      !>        preceded by an EXTERNAL statement declaring PI to be an
      !>        external function. This removes a conflict with any
      !>        compilers that have a PI intrinsic function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 30-MAY-2002 (FST)
      !>
      !>        Calls to CHKIN and CHKOUT in the standard SPICE error
      !>        handling style were added. Versions prior to 2.0.0
      !>        were error free, however the call to M2Q introduced in
      !>        version 2.0.0 signals an error if the input matrix is
      !>        not sufficiently close to a rotation.
      !>
      !>        Additionally, FAILED is now checked after the call to
      !>        M2Q. This prevents garbage from being placed into the
      !>        output arguments.
      !>
      !>-    Beta Version 1.1.0, 03-JAN-1989 (WLT)
      !>
      !>        Even though the routine stipulates that the input matrix
      !>        should be a rotation matrix, it might not be. As a result
      !>        we could have negative numbers showing up where we need
      !>        to take square roots. This fix simply bounds these values
      !>        so that Fortran intrinsics always get reasonable input values.
      !>
      !>        Add and example to the header.
      module subroutine raxisa ( ctx, matrix, axis, angle )
      use pepper_kinds, only: wp
      use pepper_constants, only: pi
      use pepper_math, only: vhat, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: matrix ( 3, 3 )  !! 3x3 rotation matrix in double precision.
      real(wp), intent(out) :: axis   (    3 )  !! Axis of the rotation.
      real(wp), intent(out) :: angle  !! Angle through which the rotation is performed.

      real(wp) :: q ( 0: 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Construct the quaternion corresponding to the input rotation
!     matrix
!
      call m2q ( ctx, matrix, q )

!
!     Check FAILED and return if an error has occurred.
!
      if ( ctx%failed() ) return

!
!     The quaternion we've just constructed is of the form:
!
!         cos(ANGLE/2) + sin(ANGLE/2) * AXIS
!
!     We take a few precautions to handle the case of an identity
!     rotation.
!
      if ( all(q(1:3) == 0.0_wp) ) then

         angle = 0
         axis(1) = 0.0_wp
         axis(2) = 0.0_wp
         axis(3) = 1.0_wp

      else if ( q(0) == 0.0_wp ) then

         angle   = pi
         axis(1) = q(1)
         axis(2) = q(2)
         axis(3) = q(3)

      else

         call vhat ( q(1), axis )
         angle = 2.0_wp * datan2 ( vnorm(q(1)), q(0) )

      end if

      return

      end subroutine raxisa

      !> Find the roots of a quadratic equation.
      !>
      !>## Detailed Input
      !>
      !>     A,
      !>     B,
      !>     C        are the coefficients of a quadratic polynomial
      !>
      !>                      2
      !>                 A * x   +  B * x  +  C.
      !>
      !>## Detailed Output
      !>
      !>     ROOT1,
      !>     ROOT2    are the roots of the equation
      !>
      !>                      2
      !>                 A * x   +  B * x  +  C = 0.
      !>
      !>              ROOT1 and ROOT2 are both arrays of length 2. The first
      !>              element of each array is the real part of a root; the
      !>              second element contains the complex part of the same
      !>              root.
      !>
      !>              When A is non-zero, ROOT1 represents the root
      !>
      !>                               _____________
      !>                              /  2
      !>                 - B   +    \/  B    -   4AC
      !>                 ---------------------------
      !>                               2A
      !>
      !>              and ROOT2 represents the root
      !>
      !>                               _____________
      !>                              /  2
      !>                 - B   -    \/  B    -   4AC
      !>                 --------------------------- .
      !>                               2A
      !>
      !>              When A is zero and B is non-zero, ROOT1 and ROOT2 both
      !>              represent the root
      !>
      !>                 - C / B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input coefficients A and B are both zero, the error
      !>         SPICE(DEGENERATECASE) is signaled. The output arguments
      !>         are not modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     1)   Humor us and suppose we want to compute the "golden ratio."
      !>
      !>          The quantity r is defined by the equation
      !>
      !>             1/r = r/(1-r),
      !>
      !>          which is equivalent to
      !>
      !>              2
      !>             r   +  r  -  1  =  0.
      !>
      !>          The following code fragment does the job.
      !>
      !>             C
      !>             C     Compute "golden ratio." The root we want,
      !>             C
      !>             C                ___
      !>             C               /
      !>             C        -1 + \/  5
      !>             C        -----------,
      !>             C             2
      !>             C
      !>             C
      !>             C     is contained in ROOT1.
      !>             C
      !>
      !>                   CALL RQUAD ( 1.D0, 1.D0, -1.D0, ROOT1, ROOT2 )
      !>
      !>                   PRINT *, 'The "golden ratio" is ', ROOT1(1)
      !>
      !>     2)   The equation,
      !>
      !>              2
      !>             x   +  1  =  0
      !>
      !>          can be solved by the code fragment
      !>
      !>             C
      !>             C     Let's do one with imaginary roots just for fun.
      !>             C
      !>
      !>                   CALL RQUAD ( 1.D0,  0.D0,  1.D0,  ROOT1,  ROOT2 )
      !>
      !>                   PRINT *, 'ROOT1 is ', ROOT1
      !>                   PRINT *, 'ROOT2 is ', ROOT2
      !>
      !>          The printed results will be something like:
      !>
      !>             ROOT1 is 0.000000000000000    1.000000000000000
      !>             ROOT2 is 0.000000000000000   -1.000000000000000
      !>
      !>## Restrictions
      !>
      !>     1)  No checks for overflow of the roots are performed.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 10-JUL-1990 (NJB)
      module subroutine rquad ( ctx, a, b, c, root1, root2 )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: a  !! Coefficient of quadratic term.
      real(wp), intent(in) :: b  !! Coefficient of linear term.
      real(wp), intent(in) :: c  !! Constant.
      real(wp), intent(out) :: root1 ( 2 )  !! Root built from positive discriminant term.
      real(wp), intent(out) :: root2 ( 2 )  !! Root built from negative discriminant term.

      real(wp) :: con
      real(wp) :: discrm
      real(wp) :: lin
      real(wp) :: scale
      real(wp) :: sqr

      logical :: zeroed

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The degree of the equation is zero unless at least one of the
!     second or first degree coefficients is non-zero.
!
      if (  ( a == 0.0_wp )  .and.  ( b == 0.0_wp )  )  then

         call ctx%setmsg ( 'Both 1st and 2nd degree coefficients are zero.')
         call ctx%sigerr ( 'SPICE(DEGENERATECASE)'                         )
         return

      end if

!
!     If we can scale the coefficients without zeroing any of them out,
!     we will do so, to help prevent overflow.
!
      scale  = max ( abs(a), abs(b), abs(c) )

      zeroed =      (  ( a /= 0.0_wp ) .and. ( a / scale == 0.0_wp )  ) &
               .or. (  ( b /= 0.0_wp ) .and. ( b / scale == 0.0_wp )  ) &
               .or. (  ( c /= 0.0_wp ) .and. ( c / scale == 0.0_wp )  )

      if ( .not. zeroed ) then

         sqr = a / scale
         lin = b / scale
         con = c / scale

      else

         sqr = a
         lin = b
         con = c

      end if

!
!     If the second-degree coefficient is non-zero, we have a bona fide
!     quadratic equation, as opposed to a linear equation.
!
      if ( sqr /= 0.0_wp ) then
!
!        Compute the discriminant.
!
         discrm = lin**2  -  4.0_wp * sqr * con

!
!        A non-negative discriminant indicates that the roots are
!        real.
!
         if ( discrm >= 0.0_wp ) then
!
!           The imaginary parts of both roots are zero.
!
            root1(2) = 0.0_wp
            root2(2) = 0.0_wp
!
!           We can take advantage of the fact that CON/SQR is the
!           product of the roots to improve the accuracy of the root
!           having the smaller magnitude.  We compute the larger root
!           first and then divide CON/SQR by it to obtain the smaller
!           root.
!
            if ( lin < 0.0_wp ) then
!
!              ROOT1 will contain the root of larger magnitude.
!
               root1(1) =  ( - lin + dsqrt(discrm) )  /  ( 2.0_wp * sqr )

               root2(1) =  ( con / sqr )  /   root1(1)

            else if ( lin > 0.0_wp ) then
!
!              ROOT2 will contain the root of larger magnitude.
!
               root2(1) =  ( - lin - dsqrt(discrm) )  /  ( 2.0_wp * sqr )

               root1(1) =  ( con / sqr )  /   root2(1)

            else
!
!              The roots have the same magnitude.
!
               root1(1)  =    dsqrt( discrm )  /  ( 2.0_wp * sqr )

               root2(1)  =  - root1(1)

            end if

!
!        The only other possibility is that the roots are complex.
!
         else
!
!           The roots are complex conjugates, so they have equal
!           magnitudes.
!
            root1(1)  =  -lin                /   ( 2.0_wp * sqr )
            root1(2)  =   dsqrt( -discrm )   /   ( 2.0_wp * sqr )

            root2(1)  =   root1(1)
            root2(2)  =  -root1(2)

         end if

!
!     If the second-degree coefficient is zero, we actually have a
!     linear equation.
!
      else if ( lin /= 0.0_wp ) then

         root1(1) =  - con / lin
         root1(2) =    0.0_wp

!
!        We set the second root equal to the first, rather than
!        leaving it undefined.
!
         root2 = root1

      end if

      return
      end subroutine rquad

      !> Compute the integer quotient and non-negative remainder
      !> of NUM and DENOM.
      !>
      !>## Detailed Input
      !>
      !>     NUM      is the numerator of a quotient
      !>
      !>     DENOM    is the denominator of a quotient
      !>
      !>## Detailed Output
      !>
      !>     Q        is the largest integer less than or equal to the
      !>              quotient NUM/DENOM
      !>
      !>     REM      is the remainder of the integer division NUM/DENOM
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If DENOM is zero, the error SPICE(DIVIDEBYZERO) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given the double precision inputs NUM and DENOM, this routine
      !>     finds double precision numbers Q and REM that satisfy the
      !>     following conditions:
      !>
      !>         1) NUM = DENOM * Q + REM
      !>
      !>         2) DENOM has integer value.
      !>
      !>         3) REM belongs to the half open interval [0, ABS(DENOM) )
      !>
      !>     This routine serves as a macro. In this way the code to perform
      !>     this task can be written and maintained in a single location.
      !>
      !>## Examples
      !>
      !>     One frequently needs to compute the  ``Two pi modulus'' of a
      !>     number. For positive numbers the FORTRAN intrinsic mod
      !>     function works well. However, for negative numbers the
      !>     intrinsic will return a negative modulus. This routine
      !>     can be used to compute the positive two pi modulus (MOD2PI) for
      !>     any number X by the call:
      !>
      !>         CALL RMAIND ( X, TWOPI(), I, MOD2PI )
      !>
      !>## Restrictions
      !>
      !>     1)  Arithmetic overflows are not trapped or detected by this
      !>         routine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      module subroutine rmaind ( ctx, num, denom, q, rem )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: num  !! Numerator used to compute quotient and remainder.
      real(wp), intent(in) :: denom  !! Denominator used to compute quotient and remainder.
      real(wp), intent(out) :: q  !! Integer portion of the quotient NUM/DENOM.
      real(wp), intent(out) :: rem  !! Remainder of the quotient NUM/DENOM.

      real(wp) :: mynum
      real(wp) :: mydnom

!
!     Take care of the zero-denominator case first
!
      if ( denom == 0.0_wp ) then
         call ctx%setmsg ( 'Attempting to compute a quotient with a '     // &
                       'divide by zero.'                              )
         call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                          )
         return
      end if

      mydnom = denom
      mynum  = num

      q   = dint ( mynum / mydnom )
      rem = mynum -    q * mydnom

      if ( rem < 0.0_wp ) then
         q   = q   - 1.0_wp
         rem = rem + mydnom
      end if

      return
      end subroutine rmaind

      !> Compute the integer quotient and non-negative remainder
      !> of NUM and DENOM.
      !>
      !>## Detailed Input
      !>
      !>     NUM      is the numerator of a quotient
      !>
      !>     DENOM    is the denominator of a quotient
      !>
      !>## Detailed Output
      !>
      !>     Q        is the largest integer less than or equal to the
      !>              quotient NUM/DENOM
      !>
      !>     REM      is the remainder of the integer division NUM/DENOM
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If DENOM is zero, the error SPICE(DIVIDEBYZERO) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given the integer inputs NUM and DENOM, this routine
      !>     finds integers Q and REM that satisfy the following conditions:
      !>
      !>         1) NUM = DENOM * Q + REM
      !>
      !>         2) REM is a non negative integer less than the absolute
      !>            value of DENOM.
      !>
      !>     This routine serves as a macro. In this way the code to perform
      !>     this task can be written and maintained in a single location.
      !>
      !>## Examples
      !>
      !>     One frequently needs to compute the  ``360 modulus'' of a
      !>     number. For positive numbers the FORTRAN intrinsic mod
      !>     function works well. However, for negative numbers the
      !>     intrinsic will return a negative modulus. This routine
      !>     can be used to compute the positive 360 pi modulus (MOD360) for
      !>     any integer I by the call:
      !>
      !>         CALL RMAINI ( I, 360, Q, MOD360 )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
      module subroutine rmaini ( ctx, num, denom, q, rem )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: num  !! Numerator used to compute quotient and remainder.
      integer(ip), intent(in) :: denom  !! Denominator used to compute quotient and remainder.
      integer(ip), intent(out) :: q  !! Integer portion of the quotient NUM/DENOM.
      integer(ip), intent(out) :: rem  !! Remainder of the quotient NUM/DENOM.

!
!     Take care of the zero-denominator case first
!
      if ( denom == 0.0_wp ) then
         call ctx%setmsg ( 'Attempting to compute a quotient with a '     // &
                       'divide by zero.'                              )
         call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                          )
         return
      end if

      q   = num / denom
      rem = num - denom*q

      if ( rem < 0 ) then
         q   = q   - 1
         rem = rem + denom
      end if

      return
      end subroutine rmaini

      !> Project a vector onto a specified plane, orthogonally.
      !>
      !>## Detailed Input
      !>
      !>     VIN      is a 3-vector that is to be orthogonally projected
      !>              onto a specified plane.
      !>
      !>     PLANE    is a SPICE plane that represents the geometric
      !>              plane onto which VIN is to be projected.
      !>
      !>              The normal vector component of a SPICE plane has
      !>              unit length.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the vector resulting from the orthogonal
      !>              projection of VIN onto PLANE. VOUT is the closest
      !>              point in the specified plane to VIN.
      !>
      !>## Parameters
      !>
      !>     UBPL     is the upper bound of a SPICE plane array.
      !>
      !>## Exceptions
      !>
      !>     1)  If the normal vector of the input plane does not have unit
      !>         length (allowing for round-off error), the error
      !>         SPICE(NONUNITNORMAL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Projecting a vector VIN orthogonally onto a plane can be thought
      !>     of as finding the closest vector in the plane to VIN. This
      !>     "closest vector" always exists; it may be coincident with the
      !>     original vector.
      !>
      !>     Two related routines are VPRJPI, which inverts an orthogonal
      !>     projection of a vector onto a plane, and VPROJ, which projects
      !>     a vector orthogonally onto another vector.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the closest point in the ring plane of a planet to a
      !>        spacecraft located at a point (in body-fixed coordinates).
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VPRJP_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>C     Upper bound of plane length.
      !>C
      !>      INTEGER                 UBPL
      !>      PARAMETER             ( UBPL = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      NORM   ( 3    )
      !>      DOUBLE PRECISION      ORIG   ( 3    )
      !>      DOUBLE PRECISION      PROJ   ( 3    )
      !>      DOUBLE PRECISION      RINGPL ( UBPL )
      !>      DOUBLE PRECISION      SCPOS  ( 3    )
      !>
      !>C
      !>C     Set the spacecraft location and define the normal
      !>C     vector as the normal to the equatorial plane, and
      !>C     the origin at the body/ring center.
      !>C
      !>      DATA                  SCPOS /  -29703.16955D0,
      !>     .                               879765.72163D0,
      !>     .                              -137280.21757D0   /
      !>
      !>      DATA                  NORM  /  0.D0, 0.D0, 1.D0 /
      !>
      !>      DATA                  ORIG  /  0.D0, 0.D0, 0.D0 /
      !>
      !>C
      !>C     Create the plane structure.
      !>C
      !>      CALL NVP2PL ( NORM, ORIG, RINGPL )
      !>
      !>C
      !>C     Project the position vector onto the ring plane.
      !>C
      !>      CALL VPRJP ( SCPOS, RINGPL, PROJ )
      !>
      !>      WRITE(*,'(A)') 'Projection of S/C position onto ring '
      !>     .            // 'plane:'
      !>      WRITE(*,'(3F17.5)') PROJ
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Projection of S/C position onto ring plane:
      !>             -29703.16955     879765.72163          0.00000
      !>
      !>## Restrictions
      !>
      !>     1)  It is recommended that the input plane be created by one of
      !>         the SPICELIB routines
      !>
      !>            NVC2PL ( Normal vector and constant to plane )
      !>            NVP2PL ( Normal vector and point to plane    )
      !>            PSV2PL ( Point and spanning vectors to plane )
      !>
      !>         In any case the input plane must have a unit length normal
      !>         vector and a plane constant consistent with the normal
      !>         vector.
      !>
      !>## Literature References
      !>
      !>     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
      !>          7th Edition, Addison Wesley, 1988.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR)
      !>
      !>        Added error check for non-unit plane normal vector.
      !>        Changed check-in style to discovery.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Added documentation of the parameter UBPL.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      module subroutine vprjp ( ctx, vin, plane, vout )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: approx, pl2nvc, vdot, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: vin   (    3 )  !! Vector to be projected.
      real(wp), intent(in) :: plane ( ubpl )  !! A SPICE plane onto which VIN is projected.
      real(wp), intent(out) :: vout  (    3 )  !! Vector resulting from projection.


      real(wp) :: const
      real(wp) :: normal ( 3 )

!
!     Check RETURN but
!
      if ( ctx%failed() ) return

!
!     Obtain a unit vector normal to the input plane, and a constant
!     for the plane.
!
      call pl2nvc ( plane, normal, const )

!
!     The normal vector returned by PL2NVC should be a unit vector.
!
      if (  .not.  approx( vnorm(normal), 1.0_wp, magtol )  ) then

         call ctx%setmsg ( 'Normal vector returned by PL2NVC does not ' &
         //            'have unit length; the difference of the ' &
         //            'length from 1 is #. The input plane is ' &
         //            'invalid. '                                )
         call ctx%errdp  ( '#',  vnorm(normal) - 1.0_wp                 )
         call ctx%sigerr ( 'SPICE(NONUNITNORMAL)'                     )
         return

      end if

!
!     Let the notation < a, b > indicate the inner product of vectors
!     a and b.
!
!     VIN differs from its projection onto PLANE by some multiple of
!     NORMAL. That multiple is
!
!               < VIN - VOUT, NORMAL >                 *  NORMAL
!
!        =   (  < VIN, NORMAL > - < VOUT, NORMAL >  )  *  NORMAL
!
!        =   (  < VIN, NORMAL > - CONST             )  *  NORMAL
!
!     Subtracting this multiple of NORMAL from VIN yields VOUT.
!
      vout = 1.0_wp*vin + (const - vdot ( vin, normal ))*normal

      return
      end subroutine vprjp

      !> Find the vector in a specified plane that maps to a specified
      !> vector in another plane under orthogonal projection.
      !>
      !>## Detailed Input
      !>
      !>     VIN,
      !>     PROJPL,
      !>     INVPL    are, respectively, a 3-vector, a SPICE plane
      !>              containing the vector, and a SPICE plane
      !>              containing the inverse image of the vector under
      !>              orthogonal projection onto PROJPL.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the inverse orthogonal projection of VIN. This
      !>              is the vector lying in the plane INVPL whose
      !>              orthogonal projection onto the plane PROJPL is
      !>              VIN. VOUT is valid only when FOUND (defined below)
      !>              is .TRUE. Otherwise, VOUT is undefined.
      !>
      !>     FOUND    indicates whether the inverse orthogonal projection
      !>              of VIN could be computed. FOUND is .TRUE. if so,
      !>              .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     UBPL     is the upper bound of a SPICE plane array.
      !>
      !>## Exceptions
      !>
      !>     1)  If the normal vector of either input plane does not have unit
      !>         length (allowing for round-off error), the error
      !>         SPICE(NONUNITNORMAL) is signaled.
      !>
      !>     2)  If the geometric planes defined by PROJPL and INVPL are
      !>         orthogonal, or nearly so, the inverse orthogonal projection
      !>         of VIN may be undefined or have magnitude too large to
      !>         represent with double precision numbers. In either such
      !>         case, FOUND will be set to .FALSE.
      !>
      !>     3)  Even when FOUND is .TRUE., VOUT may be a vector of extremely
      !>         large magnitude, perhaps so large that it is impractical to
      !>         compute with it. It's up to you to make sure that this
      !>         situation does not occur in your application of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Projecting a vector orthogonally onto a plane can be thought of
      !>     as finding the closest vector in the plane to the original vector.
      !>     This "closest vector" always exists; it may be coincident with the
      !>     original vector. Inverting an orthogonal projection means finding
      !>     the vector in a specified plane whose orthogonal projection onto
      !>     a second specified plane is a specified vector. The vector whose
      !>     projection is the specified vector is the inverse projection of
      !>     the specified vector, also called the "inverse image under
      !>     orthogonal projection" of the specified vector. This routine
      !>     finds the inverse orthogonal projection of a vector onto a plane.
      !>
      !>     Related routines are VPRJP, which projects a vector onto a plane
      !>     orthogonally, and VPROJ, which projects a vector onto another
      !>     vector orthogonally.
      !>
      !>## Examples
      !>
      !>     1)   Suppose
      !>
      !>             VIN    =  ( 0.0, 1.0, 0.0 ),
      !>
      !>          and that PROJPL has normal vector
      !>
      !>             PROJN  =  ( 0.0, 0.0, 1.0 ).
      !>
      !>          Also, let's suppose that INVPL has normal vector and constant
      !>
      !>             INVN   =  ( 0.0, 2.0, 2.0 )
      !>             INVC   =    4.0.
      !>
      !>          Then VIN lies on the y-axis in the x-y plane, and we want to
      !>          find the vector VOUT lying in INVPL such that the orthogonal
      !>          projection of VOUT the x-y plane is VIN. Let the notation
      !>          < a, b > indicate the inner product of vectors a and b.
      !>          Since every point X in INVPL satisfies the equation
      !>
      !>             <  X,  (0.0, 2.0, 2.0)  >  =  4.0,
      !>
      !>          we can verify by inspection that the vector
      !>
      !>             ( 0.0, 1.0, 1.0 )
      !>
      !>          is in INVPL and differs from VIN by a multiple of PROJN. So
      !>
      !>             ( 0.0, 1.0, 1.0 )
      !>
      !>          must be VOUT.
      !>
      !>          To find this result using SPICELIB, we can create the
      !>          SPICE planes PROJPL and INVPL using the code fragment
      !>
      !>             CALL NVP2PL  ( PROJN,  VIN,   PROJPL )
      !>             CALL NVC2PL  ( INVN,   INVC,  INVPL  )
      !>
      !>          and then perform the inverse projection using the call
      !>
      !>             CALL VPRJPI ( VIN, PROJPL, INVPL, VOUT )
      !>
      !>          VPRJPI will return the value
      !>
      !>             VOUT = ( 0.0, 1.0, 1.0 )
      !>
      !>## Restrictions
      !>
      !>     1)  It is recommended that the input planes be created by one of
      !>         the SPICELIB routines
      !>
      !>            NVC2PL ( Normal vector and constant to plane )
      !>            NVP2PL ( Normal vector and point to plane    )
      !>            PSV2PL ( Point and spanning vectors to plane )
      !>
      !>         In any case each input plane must have a unit length normal
      !>         vector and a plane constant consistent with the normal
      !>         vector.
      !>
      !>## Literature References
      !>
      !>     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
      !>          7th Edition, Addison Wesley, 1988.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 25-AUG-2021 (NJB) (JDR)
      !>
      !>        Added error checks for non-unit plane normal vectors.
      !>        Changed check-in style to discovery.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added documentation of the parameter UBPL.
      !>
      !>-    SPICELIB Version 2.0.0, 17-FEB-2004 (NJB)
      !>
      !>        Computation of LIMIT was re-structured to avoid
      !>        run-time underflow warnings on some platforms.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 17-FEB-2004 (NJB)
      !>
      !>        Computation of LIMIT was re-structured to avoid
      !>        run-time underflow warnings on some platforms.
      !>        In the revised code, BOUND/DPMAX() is never
      !>        scaled by a number having absolute value < 1.
      module subroutine vprjpi ( ctx, vin, projpl, invpl, vout, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_math, only: approx, pl2nvc, vdot, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: vin    (    3 )  !! The projected vector.
      real(wp), intent(in) :: projpl ( ubpl )  !! Plane containing VIN.
      real(wp), intent(in) :: invpl  ( ubpl )  !! Plane containing inverse image of VIN.
      real(wp), intent(out) :: vout   (    3 )  !! Inverse projection of VIN.
      logical, intent(out) :: found  !! Flag indicating whether VOUT could be calculated.


!
!     BOUND is used to bound the magnitudes of the numbers that we
!     try to take the reciprocal of, since we can't necessarily invert
!     any non-zero number.  We won't try to invert any numbers with
!     magnitude less than
!
!        BOUND / DPMAX().
!
!     BOUND is chosen somewhat arbitrarily....
!
      real(wp), parameter :: bound = 10.0_wp

      real(wp) :: denom
      real(wp) :: invc
      real(wp) :: invn  ( 3 )
      real(wp) :: limit
      real(wp) :: mult
      real(wp) :: numer
      real(wp) :: projc
      real(wp) :: projn ( 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the planes.
!
      call pl2nvc ( projpl, projn, projc )
      call pl2nvc ( invpl,  invn,  invc  )

!
!     Check the normal vectors obtained from the planes.
!
!     Each normal vector returned by PL2NVC should be a unit vector.
!
      if (  .not.  approx( vnorm(projn), 1.0_wp, magtol )  ) then

         call ctx%setmsg ( 'Normal vector of plane containing input ' &
         //            'point does not have unit length; the ' &
         //            'difference of the length from 1 is #. ' &
         //            'The input plane is invalid. '             )
         call ctx%errdp  ( '#',  vnorm(projn) - 1.0_wp                  )
         call ctx%sigerr ( 'SPICE(NONUNITNORMAL)'                     )
         return

      end if

      if (  .not.  approx( vnorm(invn), 1.0_wp, magtol )  ) then

         call ctx%setmsg ( 'Normal vector of plane containing output ' &
         //            'point does not have unit length; the ' &
         //            'difference of the length from 1 is #. ' &
         //            'The output plane is invalid. '            )
         call ctx%errdp  ( '#',  vnorm(invn) - 1.0_wp                   )
         call ctx%sigerr ( 'SPICE(NONUNITNORMAL)'                     )
         return

      end if

!
!     We'll first discuss the computation of VOUT in the nominal case,
!     and then deal with the exceptional cases.
!
!     When PROJPL and INVPL are not orthogonal to each other, the
!     inverse projection of VIN will differ from VIN by a multiple of
!     PROJN, the unit normal vector to PROJPL. We find this multiple
!     by using the fact that the inverse projection VOUT satisfies the
!     plane equation for the inverse projection plane INVPL.
!
!        We have
!
!           VOUT = VIN  +  MULT * PROJN;                           (1)
!
!        since VOUT satisfies
!
!           < VOUT, INVN >  =  INVC
!
!        we must have
!
!           <  VIN  +  MULT * PROJN,  INVN  > = INVC
!
!        which in turn implies
!
!                     INVC  -  < VIN, INVN >
!           MULT  =  ------------------------.                     (2)
!                        < PROJN, INVN >
!
!        Having MULT, we can compute VOUT according to equation (1).
!
!     Now, if the denominator in the above expression for MULT is zero
!     or just too small, performing the division would cause a
!     divide-by-zero error or an overflow of MULT.  In either case, we
!     will avoid carrying out the division, and we'll set FOUND to
!     .FALSE.
!
!     Compute the numerator and denominator of the right side of (2).
!
      numer  =  invc - vdot ( vin,   invn )
      denom  =         vdot ( projn, invn )

!
!     If the magnitude of the denominator is greater than the absolute
!     value of
!
!                    BOUND
!        LIMIT  =  --------- * NUMER,
!                   DPMAX()
!
!     we can safely divide the numerator by the denominator, and the
!     magnitude of the result will be no greater than
!
!         DPMAX()
!        --------- .
!          BOUND
!
!     Note that we have ruled out the case where NUMER and DENOM are
!     both zero by insisting on strict inequality in the comparison of
!     DENOM and LIMIT.
!
!     We never set LIMIT smaller than BOUND/DPMAX(), since
!     the computation using NUMER causes underflow to be signaled
!     on some systems.
!
      if ( abs(numer) < 1.0_wp ) then
         limit  =           bound / dpmax
      else
         limit  =  abs (  ( bound / dpmax ) * numer  )
      end if

      if (  abs (denom) > limit  ) then
!
!        We can find VOUT after all.
!
         mult = numer / denom

         vout = 1.0_wp*vin + mult*projn

         found = .true.

      else
!
!        No dice.
!
         found = .false.

      end if

      return
      end subroutine vprjpi

      !> Correct the apparent position of an object for stellar
      !> aberration.
      !>
      !>## Detailed Input
      !>
      !>     POBJ     is the position (x, y, z, km) of an object with
      !>              respect to the observer, possibly corrected for
      !>              light time.
      !>
      !>     VOBS     is the velocity (dx/dt, dy/dt, dz/dt, km/sec)
      !>              of the observer with respect to the Solar System
      !>              barycenter.
      !>
      !>## Detailed Output
      !>
      !>     APPOBJ   is the apparent position of the object relative
      !>              to the observer, corrected for stellar aberration.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the velocity of the observer is greater than or equal
      !>         to the speed of light, the error SPICE(VALUEOUTOFRANGE)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let r be the vector from the observer to the object, and v be
      !>         -                                                    -
      !>     the velocity of the observer with respect to the Solar System
      !>     barycenter. Let w be the angle between them. The aberration
      !>     angle phi is given by
      !>
      !>          sin(phi) = v sin(w) / c
      !>
      !>     Let h be the vector given by the cross product
      !>         -
      !>
      !>           h = r X v
      !>           -   -   -
      !>
      !>     Rotate r by phi radians about h to obtain the apparent position
      !>            -                      -
      !>     of the object.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the apparent position of the Moon relative to the
      !>        Earth, corrected for one light-time and stellar aberration,
      !>        given the geometric state of the Earth relative to the Solar
      !>        System Barycenter, and the difference between the stellar
      !>        aberration corrected and uncorrected position vectors, taking
      !>        several steps.
      !>
      !>        First, compute the light-time corrected state of the Moon body
      !>        as seen by the Earth, using its geometric state. Then apply
      !>        the correction for stellar aberration to the light-time
      !>        corrected state of the target body.
      !>
      !>        The code in this example could be replaced by a single call
      !>        to SPKPOS:
      !>
      !>            CALL SPKPOS ( 'MOON', ET, 'J2000', 'LT+S', 'EARTH',
      !>           .               POS,   LT                           )
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: stelab_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de418.bsp                     Planetary ephemeris
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de418.bsp',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM STELAB_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(6)         REFFRM
      !>      CHARACTER*(12)        UTCSTR
      !>
      !>      DOUBLE PRECISION      APPDIF ( 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      PCORR  ( 3 )
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      SOBS   ( 6 )
      !>
      !>      INTEGER               IDOBS
      !>      INTEGER               IDTARG
      !>
      !>C
      !>C     Assign an observer, Earth, target, Moon, time of interest
      !>C     and reference frame for returned vectors.
      !>C
      !>      IDOBS  = 399
      !>      IDTARG = 301
      !>      UTCSTR = 'July 4 2004'
      !>      REFFRM = 'J2000'
      !>
      !>C
      !>C     Load the needed kernels.
      !>C
      !>      CALL FURNSH ( 'stelab_ex1.tm' )
      !>
      !>C
      !>C     Convert the time string to ephemeris time, J2000.
      !>C
      !>      CALL STR2ET ( UTCSTR, ET )
      !>
      !>C
      !>C     Get the state of the observer with respect to the solar
      !>C     system barycenter.
      !>C
      !>      CALL SPKSSB ( IDOBS, ET, REFFRM, SOBS )
      !>
      !>C
      !>C     Get the light-time corrected position POS of the target
      !>C     body IDTARG as seen by the observer.
      !>C
      !>      CALL SPKAPO ( IDTARG, ET, REFFRM, SOBS, 'LT', POS, LT )
      !>
      !>C
      !>C     Output the uncorrected vector.
      !>C
      !>      WRITE(*,*) 'Uncorrected position vector'
      !>      WRITE(*,'(A,3F19.6)') '   ', POS(1), POS(2), POS(3)
      !>
      !>C
      !>C     Apply the correction for stellar aberration to the
      !>C     light-time corrected position of the target body.
      !>C
      !>      CALL STELAB ( POS, SOBS(4), PCORR )
      !>
      !>C
      !>C     Output the corrected position vector and the apparent
      !>C     difference from the uncorrected vector.
      !>C
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Corrected position vector'
      !>      WRITE(*,'(A,3F19.6)') '   ', PCORR(1), PCORR(2),
      !>     .                             PCORR(3)
      !>
      !>C
      !>C     Apparent difference.
      !>C
      !>      CALL VSUB ( POS, PCORR, APPDIF )
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Apparent difference'
      !>      WRITE(*,'(A,3F19.6)') '   ', APPDIF(1), APPDIF(2),
      !>     .                            APPDIF(3)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Uncorrected position vector
      !>                 201738.725087     -260893.141602     -147722.589056
      !>
      !>         Corrected position vector
      !>                 201765.929516     -260876.818077     -147714.262441
      !>
      !>         Apparent difference
      !>                    -27.204429         -16.323525          -8.326615
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Owen, "The Treatment of Aberration in Optical Navigation",
      !>          JPL IOM #314.8-524, 8 February 1985.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added example's
      !>        meta-kernel and problem statement. Created complete code
      !>        example from existing code fragments.
      !>
      !>-    SPICELIB Version 1.1.1, 08-JAN-2008 (NJB)
      !>
      !>        The header example was updated to remove references
      !>        to SPKAPP.
      !>
      !>-    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT)
      !>
      !>        The example was corrected so that SOBS(4) is passed
      !>        into STELAB instead of STARG(4).
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 08-AUG-1990 (HAN)
      !>
      !>        $Examples section of the header was updated to replace
      !>        calls to the GEF ephemeris readers by calls to the
      !>        new SPK ephemeris reader.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.1.0, 9-MAR-1989 (HAN)
      !>
      !>        Declaration of the variable LIGHT was removed from the code.
      !>        The variable was declared but never used.
      !>
      !>-    Beta Version 2.0.0, 28-DEC-1988 (HAN)
      !>
      !>        Error handling was added to check the velocity of the
      !>        observer. If the velocity of the observer is greater
      !>        than or equal to the speed of light, the error
      !>        SPICE(VALUEOUTOFRANGE) is signaled.
      module subroutine stelab ( ctx, pobj, vobs, appobj )
      use pepper_kinds, only: wp
      use pepper_constants, only: clight
      use pepper_math, only: vcrss, vdot, vhat, vnorm, vrotv
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: pobj   ( 3 )  !! Position of an object with respect to the
                                            !! observer.
      real(wp), intent(in) :: vobs   ( 3 )  !! Velocity of the observer with respect to the Solar
                                            !! System barycenter.
      real(wp), intent(out) :: appobj ( 3 )  !! Apparent position of the object with respect to
                                             !! the observer, corrected for stellar aberration.

      real(wp) :: onebyc
      real(wp) :: u      ( 3 )
      real(wp) :: vbyc   ( 3 )
      real(wp) :: lensqr
      real(wp) :: h      ( 3 )
      real(wp) :: sinphi
      real(wp) :: phi

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     We are not going to compute the aberrated vector in exactly the
!     way described in the particulars section.  We can combine some
!     steps and we take some precautions to prevent floating point
!     overflows.
!
!     Get a unit vector that points in the direction of the object
!     ( u_obj ).
!
      call vhat ( pobj, u )

!
!     Get the velocity vector scaled with respect to the speed of light
!     ( v/c ).
!
      onebyc = 1.0_wp / clight

      vbyc = onebyc*vobs

!
!     If the square of the length of the velocity vector is greater than
!     or equal to one, the speed of the observer is greater than or
!     equal to the speed of light. The observer speed is definitely out
!     of range. Signal an error.
!
      lensqr = vdot ( vbyc, vbyc )

      if ( lensqr >= 1.0_wp ) then

         call ctx%setmsg ( 'Velocity components of observer were:  '      // &
                       'dx/dt = *, dy/dt = *, dz/dt = *.'              )
         call ctx%errdp  ( '*', vobs (1)            )
         call ctx%errdp  ( '*', vobs (2)            )
         call ctx%errdp  ( '*', vobs (3)            )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

!
!     Compute u_obj x (v/c)
!
      call vcrss ( u, vbyc, h )

!
!     If the magnitude of the vector H is zero, the observer is moving
!     along the line of sight to the object, and no correction is
!     required. Otherwise, rotate the position of the object by phi
!     radians about H to obtain the apparent position.
!
      sinphi  = vnorm ( h )

      if ( sinphi /= 0.0_wp ) then

         phi = dasin ( sinphi )
         call  vrotv ( pobj, h, phi, appobj )

      else

         appobj = pobj

      end if

      return
      end subroutine stelab

      !> Correct the position of a target for the stellar aberration
      !> effect on radiation transmitted from a specified observer to
      !> the target.
      !>
      !>## Detailed Input
      !>
      !>     POBJ     is the cartesian position vector of an object with
      !>              respect to the observer, possibly corrected for
      !>              light time. Units are km.
      !>
      !>     VOBS     is the cartesian velocity vector of the observer
      !>              with respect to the Solar System barycenter. Units
      !>              are km/s.
      !>
      !>## Detailed Output
      !>
      !>     CORPOS   is the  position of the object relative to the
      !>              observer, corrected for the stellar aberration
      !>              effect on radiation directed toward the target. This
      !>              correction is the inverse of the usual stellar
      !>              aberration correction: the corrected vector
      !>              indicates the direction in which radiation must be
      !>              emitted from the observer, as seen in an inertial
      !>              reference frame having velocity equal to that of the
      !>              observer, in order to reach the position indicated by
      !>              the input vector POBJ.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the velocity of the observer is greater than or equal to
      !>         the speed of light, an error is signaled by a routine in the
      !>         call tree of this routine. The outputs are undefined.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     In order to transmit radiation from an observer to a specified
      !>     target, the emission direction must be corrected for one way
      !>     light time and for the motion of the observer relative to the
      !>     solar system barycenter. The correction for the observer's
      !>     motion when transmitting to a target is the inverse of the
      !>     usual stellar aberration correction applied to the light-time
      !>     corrected position of the target as seen by the observer.
      !>
      !>     Below is the description of the stellar aberration correction
      !>     used in the SPICELIB routine STELAB (with the notation changed
      !>     slightly):
      !>
      !>        Let R be the vector from the observer to the object, and V be
      !>        the velocity of the observer with respect to the Solar System
      !>        barycenter. Let W be the angle between them. The aberration
      !>        angle PHI is given by
      !>
      !>           sin(PHI) = V * sin(W) / C
      !>
      !>        Let H be the vector given by the cross product
      !>
      !>           H = R x V
      !>
      !>        Rotate R by PHI radians about H to obtain the apparent position
      !>        of the object.
      !>
      !>     This routine applies the inverse correction, so here the rotation
      !>     about H is by -PHI radians.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the apparent position of the Moon relative to the
      !>        Earth, corrected for one way light-time and stellar aberration
      !>        effect on radiation transmitted from the Earth to the Moon,
      !>        given the geometric state of the Earth relative to the Solar
      !>        System Barycenter, and the difference between the stellar
      !>        aberration corrected and uncorrected position vectors, taking
      !>        several steps.
      !>
      !>        First, compute the light-time corrected state of the Moon body
      !>        as seen by the Earth, using its geometric state. Then apply
      !>        the correction for stellar aberration to the light-time
      !>        corrected state of the target body, both for the transmission
      !>        case.
      !>
      !>        The code in this example could be replaced by a single call
      !>        to SPKPOS:
      !>
      !>            CALL SPKPOS ( 'MOON', ET, 'J2000', 'XLT+S', 'EARTH',
      !>           .               POS,   LT                            )
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: stlabx_ex1.tm
      !>
      !>           This meta-kernel is intended to support operation of SPICE
      !>           example programs. The kernels shown here should not be
      !>           assumed to contain adequate or correct versions of data
      !>           required by SPICE-based user applications.
      !>
      !>           In order for an application to use this meta-kernel, the
      !>           kernels referenced here must be present in the user's
      !>           current working directory.
      !>
      !>           The names and contents of the kernels referenced
      !>           by this meta-kernel are as follows:
      !>
      !>              File name                     Contents
      !>              ---------                     --------
      !>              de418.bsp                     Planetary ephemeris
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de418.bsp',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM STLABX_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(6)         REFFRM
      !>      CHARACTER*(12)        UTCSTR
      !>
      !>      DOUBLE PRECISION      APPDIF ( 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      PCORR  ( 3 )
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      SOBS   ( 6 )
      !>
      !>      INTEGER               IDOBS
      !>      INTEGER               IDTARG
      !>
      !>C
      !>C     Assign an observer, Earth, target, Moon, time of interest
      !>C     and reference frame for returned vectors.
      !>C
      !>      IDOBS  = 399
      !>      IDTARG = 301
      !>      UTCSTR = 'July 4 2004'
      !>      REFFRM = 'J2000'
      !>
      !>C
      !>C     Load the needed kernels.
      !>C
      !>      CALL FURNSH ( 'stlabx_ex1.tm' )
      !>
      !>C
      !>C     Convert the time string to ephemeris time.
      !>C
      !>      CALL STR2ET ( UTCSTR, ET )
      !>
      !>C
      !>C     Get the state of the observer with respect to the solar
      !>C     system barycenter.
      !>C
      !>      CALL SPKSSB ( IDOBS, ET, REFFRM, SOBS )
      !>
      !>C
      !>C     Get the light-time corrected position POS of the target
      !>C     body IDTARG as seen by the observer. Normally we would
      !>C     call SPKPOS to obtain this vector, but we already have
      !>C     the state of the observer relative to the solar system
      !>C     barycenter, so we can avoid looking up that state twice
      !>C     by calling SPKAPO.
      !>C
      !>      CALL SPKAPO ( IDTARG, ET, REFFRM, SOBS, 'XLT', POS, LT )
      !>
      !>C
      !>C     Output the uncorrected vector.
      !>C
      !>      WRITE(*,*) 'Uncorrected position vector'
      !>      WRITE(*,'(A,3F19.6)') '   ', POS(1), POS(2), POS(3)
      !>
      !>C
      !>C     Apply the correction for stellar aberration to the
      !>C     light-time corrected position of the target body.
      !>C
      !>      CALL STLABX ( POS, SOBS(4), PCORR )
      !>
      !>C
      !>C     Output the corrected position vector and the apparent
      !>C     difference from the uncorrected vector.
      !>C
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Corrected position vector'
      !>      WRITE(*,'(A,3F19.6)') '   ', PCORR(1), PCORR(2),
      !>     .                             PCORR(3)
      !>
      !>C
      !>C     Apparent difference.
      !>C
      !>      CALL VSUB ( POS, PCORR, APPDIF )
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Apparent difference'
      !>      WRITE(*,'(A,3F19.6)') '   ', APPDIF(1), APPDIF(2),
      !>     .                            APPDIF(3)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Uncorrected position vector
      !>                 201809.933536     -260878.049826     -147716.077987
      !>
      !>         Corrected position vector
      !>                 201782.730972     -260894.375627     -147724.405897
      !>
      !>         Apparent difference
      !>                     27.202563          16.325802           8.327911
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Owen, "The Treatment of Aberration in Optical Navigation",
      !>          JPL IOM #314.8-524, 8 February 1985.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 13-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added example's
      !>        meta-kernel and problem statement. Created complete code
      !>        example from existing code fragments.
      !>
      !>-    SPICELIB Version 1.0.1, 08-JAN-2008 (NJB)
      !>
      !>        The header example was updated to remove references
      !>        to SPKAPP.
      !>
      !>-    SPICELIB Version 1.0.0, 02-JAN-2002 (IMU) (WLT) (NJB)
      module subroutine stlabx ( ctx, pobj, vobs, corpos )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: pobj   ( 3 )  !! Position of an object with respect to the
                                            !! observer.
      real(wp), intent(in) :: vobs   ( 3 )  !! Velocity of the observer with respect to the Solar
                                            !! System barycenter.
      real(wp), intent(out) :: corpos ( 3 )  !! Corrected position of the object.

      real(wp) :: negvel ( 3 )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Obtain the negative of the observer's velocity.  This
!     velocity, combined with the target's position, will yield
!     the inverse of the usual stellar aberration correction,
!     which is exactly what we seek.
!
      negvel = -vobs

      call stelab ( ctx, pobj, negvel, corpos )

      return
      end subroutine stlabx

      !> Transpose the square blocks within a matrix.
      !>
      !>## Detailed Input
      !>
      !>     NROW     is the number of rows in the input matrix.
      !>
      !>     NCOL     is the number of columns in the input matrix.
      !>
      !>     BSIZE    is the number of rows and columns in each block
      !>              of the input matrix.
      !>
      !>     BMAT     is a block structured matrix. In other words
      !>              it looks like:
      !>
      !>                 -                                -
      !>                |      :      :       :     :      |
      !>                |  B   :  B   :   B   :     :  B   |
      !>                |   11 :   12 :    13 : ... :   1C |
      !>                |......:......:.......:     :......|
      !>                |      :      :       :     :      |
      !>                |  B   :  B   :   B   :     :  B   |
      !>                |   21 :   22 :    23 : ... :   2C |
      !>                |......:......:.......:     :......|
      !>                |      .                           |
      !>                |      .                           |
      !>                |      .                           |
      !>                |......................     .......|
      !>                |      :      :       :     :      |
      !>                |  B   :  B   :   B   :     :  B   |
      !>                |   R1 :   R2 :    R3 : ... :   RC |
      !>                |......:......:.......:     :......|
      !>                 -                                -
      !>
      !>              where each B   is a square matrix of BSIZE rows and
      !>                          ij
      !>              and columns.
      !>
      !>## Detailed Output
      !>
      !>     BTMAT    is the matrix obtained from BMAT when each of its
      !>              blocks is transposed. Given the description of
      !>              BMAT above, BTMAT looks like:
      !>
      !>                  -                                -
      !>                 |   t  :   t  :    t  :     :   t  |
      !>                 |  B   :  B   :   B   :     :  B   |
      !>                 |   11 :   12 :    13 : ... :   1C |
      !>                 |......:......:.......:     :......|
      !>                 |      :      :       :     :      |
      !>                 |   t  :   t  :    t  :     :   t  |
      !>                 |  B   :  B   :   B   :     :  B   |
      !>                 |   21 :   22 :    23 : ... :   2C |
      !>                 |......:......:.......:     :......|
      !>                 |      .                           |
      !>                 |      .                           |
      !>                 |      .                           |
      !>                 |......................     .......|
      !>                 |      :      :       :     :      |
      !>                 |   t  :   t  :    t  :     :   t  |
      !>                 |  B   :  B   :   B   :     :  B   |
      !>                 |   R1 :   R2 :    R3 : ... :   RC |
      !>                 |......:......:.......:     :......|
      !>                  -                                -
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the number of rows input is not positive, the error
      !>         SPICE(BADROWCOUNT) is signaled.
      !>
      !>     2)  If the number of columns input is not positive, the error
      !>         SPICE(BADCOLUMNCOUNT) is signaled.
      !>
      !>     3)  If the block size input is not positive, the error
      !>         SPICE(BADBLOCKSIZE) is signaled.
      !>
      !>     4)  If BMAT cannot be partitioned into an integer number of
      !>         blocks, the error SPICE(BLOCKSNOTEVEN) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine transposes the blocks of a block structured
      !>     matrix. This operation is valuable, as it is a means
      !>     for computing the inverse of a state transformation matrix
      !>     (see the example below).
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates how you would convert
      !>     a state relative to earth-fixed coordinates to J2000 coordinates.
      !>
      !>     C
      !>     C     We want to state earthfixed coordinates (399) to J2000
      !>     C     coordinates
      !>     C
      !>           BODY = 399
      !>           REF  = 'J2000'
      !>
      !>     C
      !>     C     Get the 6 by 6 state transformation matrix from J2000
      !>     C     coordinates to earthfixed coordinates.
      !>     C
      !>           CALL TISBOD ( REF, BODY, ET, TISPM )
      !>
      !>     C
      !>     C     The inverse of TISPM can be obtained by transposing the
      !>     C     3 by 3 blocks of the 6 by 6 matrix TISPM.
      !>     C
      !>           CALL XPOSBL ( TISPM, 6, 6, 3, TSPMI )
      !>
      !>     C
      !>     C     Now transform the earthfixed state (ESTATE) to the
      !>     C     inertial state (ISTATE).
      !>     C
      !>           CALL MXVG   ( TSPMI, ESTATE, 6, 6, ISTATE )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 05-NOV-1990 (WLT)
      module subroutine xposbl ( ctx, bmat, nrow, ncol, bsize, btmat )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nrow  !! the number of rows in the matrix BMAT
      integer(ip), intent(in) :: ncol  !! the number of columns in the matrix BMAT
      integer(ip), intent(in) :: bsize  !! the size of the square blocks in BMAT
      real(wp), intent(in) :: bmat  ( nrow, ncol )  !! a matrix composed of square block
                                                    !! submatrices
      real(wp), intent(out) :: btmat ( nrow, ncol )  !! the result of transposing the blocks of
                                                     !! BMAT

      real(wp) :: temp

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: cb
      integer(ip) :: rb

!
!     Ok. Here's what's going to happen.
!
!     The matrix has the form:
!
!       -                                -
!      |      :      :       :     :      |
!      |  B   :  B   :   B   :     :  B   |
!      |   11 :   12 :    13 : ... :   1C |
!      |......:......:.......:     :......|
!      |      :      :       :     :      |
!      |  B   :  B   :   B   :     :  B   |
!      |   21 :   22 :    23 : ... :   2C |
!      |......:......:.......:     :......|
!      |      .                           |
!      |      .                           |
!      |      .                           |
!      |......................     .......|
!      |      :      :       :     :      |
!      |  B   :  B   :   B   :     :  B   |
!      |   R1 :   R2 :    R3 : ... :   RC |
!      |......:......:.......:     :......|
!
!      Where each block B   is a square matrix.
!                        ij
!
!      All we really need to do is figure out how to transpose any
!      of the blocks.  Once that is done we can just cycle over
!      all of the blocks in the matrix.
!
!      So what does the ij block look like? Well, this is it.
!
!             a              a               ...    a
!              RB+1 CB+1      RB+1 CB+2              RB+1 CB+BSIZE
!
!             a              a               ...    a
!              RB+2 CB+1      RB+2 CB+2              RB+2 CB+BSIZE
!
!             a              a               ...    a
!              RB+3 CB+1      RB+3 CB+2              RB+3 CB+BSIZE
!
!                                 .
!                                 .
!                                 .
!
!              a              a                 ... a
!               RB+BSIZE CB+1  RB+BSIZE CB+2         RB+BSIZE CB+BSIZE
!
!     where RB = (i-1)*BSIZE, and CB = (j-1)*BSIZE.  But inspection of
!     this block shows that to transpose it we simply need to swap
!     the entries
!
!                a           and  a
!                 RB+m CB+n        RB+n CB+m
!
!     where m and n range over all integers from 1 to BSIZE.
!

!
!     Let's first check to make sure that the requested operation
!     makes sense.  Are all of the integers positive?
!
      if (  bsize < 1 ) then
         call ctx%setmsg ( 'The block size is not positive. '             // &
                       'The block size is #.'                         )
         call ctx%errint ( '#', bsize                                     )
         call ctx%sigerr ( 'SPICE(BADBLOCKSIZE)'                          )
         return
      end if

      if (  nrow < 1 ) then
         call ctx%setmsg ( 'The number of rows in the matrix is not '     // &
                       'positive. The number of rows is #.'           )
         call ctx%errint ( '#', nrow                                      )
         call ctx%sigerr ( 'SPICE(BADROWCOUNT)'                           )
         return
      end if

      if (  ncol < 1 ) then
         call ctx%setmsg ( 'The number of columns in the matrix is not '  // &
                       'positive. The number of columns is #.'        )
         call ctx%errint ( '#', ncol                                      )
         call ctx%sigerr ( 'SPICE(BADCOLUMNCOUNT)'                        )
         return
      end if

!
!     Is there a whole number of blocks in the matrix.
!
      if (      ( mod( ncol, bsize ) /= 0 ) &
           .or. ( mod( nrow, bsize ) /= 0 ) ) then

         call ctx%setmsg ( 'The block size does not evenly divide both '  // &
                       'the number of rows and the number of '        // &
                       'columns. The '                                // &
                       'block size is #; the number of rows is #; '   // &
                       'the number of columns is #. '                 )

         call ctx%errint ( '#', bsize                                     )
         call ctx%errint ( '#', nrow                                      )
         call ctx%errint ( '#', ncol                                      )
         call ctx%sigerr ( 'SPICE(BLOCKSNOTEVEN)'                         )
         return
      end if

!
!     If we get to this point we are ready to do the transposes.
!     Cycle over all of the blocks in the matrix.
!
      do cb = 0, ncol-1, bsize
         do rb = 0, nrow-1, bsize

!
!           OK. Transpose block ( RB, CB ).
!
            do i = 1, bsize
               do j = 1, i

                  if ( i == j ) then
                     btmat ( rb+i, cb+j ) = bmat ( rb+i, cb+j )
                  else
                     temp                 = bmat ( rb+i, cb+j )
                     btmat ( rb+i, cb+j ) = bmat ( rb+j, cb+i )
                     btmat ( rb+j, cb+i ) = temp
                  end if

               end do
            end do

         end do
      end do

      return
      end subroutine xposbl

end submodule pepper_numerics
