!> Vectors, matrices, rotations, quaternions, coordinate systems, planes, ellipses and other
!> numerical routines.
!>
!> The procedures keep SPICELIB's names, arguments and documentation. They
!> have no saved state and do not signal SPICE errors.
module pepper_math

   use pepper_kinds, only: ip, wp
   use pepper_bounds, only: ubpl, ubel

   implicit none

   private

   public :: approx, axisar, azlrec, brcktd, brckti, chbder, chbint, chbval,&
      cyllat, cylrec, cylsph, dcbrt, det, drdcyl, drdlat, drdsph, ducrss,&
      dvcrss, dvdot, dvhat, dvnorm, el2cgv, even, exact, frame, gcd, ident,&
      insang, invert, latcyl, latrec, latsph, mequ, mequg, mtxm, mtxmg,&
      mtxv, mtxvg, mxm, mxmg, mxmt, mxmtg, mxv, mxvg, odd, opsgnd, opsgni,&
      pl2nvc, pl2nvp, pl2psv, pltexp, pltnrm, polyds, q2m, qdq2av, qxq,&
      radrec, rav2xf, recazl, reccyl, reclat, recrad, recsph, sharpr,&
      smsgnd, smsgni, sphcyl, sphlat, sphrec, trace, traceg, ucrss, unorm,&
      unormg, vadd, vaddg, vcrss, vdist, vdistg, vdot, vdotg, vequ, vequg,&
      vhat, vhatg, vhatip, vlcom, vlcom3, vlcomg, vminug, vminus, vnorm,&
      vnormg, vpack, vperp, vproj, vprojg, vrel, vrelg, vrotv, vscl, vsclg,&
      vsclip, vsep, vsepg, vsub, vsubg, vtmv, vtmvg, vupack, vzero, vzerog,&
      xf2rav, xpose, xposeg, xpsgip, zzmsxf, zzrxr


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: dlon = 2  !! index of longitude in a coordinate vector
   integer(ip), parameter :: dr = 1  !! index of radius in a coordinate vector
   integer(ip), parameter :: dx = 1  !! index of x in a vector
   integer(ip), parameter :: dy = 2  !! index of y in a vector
   integer(ip), parameter :: dz = 3  !! index of z in a vector

contains

      !> Return .TRUE. if two double precision numbers are equal to
      !> within some tolerance.
      !>
      !> The function is .TRUE. whenever |X - Y| <= TOL.
      !>
      !>## Detailed Input
      !>
      !>     X,
      !>     Y        are arbitrary double precision numbers.
      !>
      !>     TOL      is a tolerance. X and Y are considered to be equal
      !>              if they differ by no more than this amount. If TOL
      !>              is negative, X and Y are never considered equal.
      !>
      !>## Detailed Output
      !>
      !>     The function is .TRUE. whenever |X - Y| <= TOL, and is .FALSE.
      !>     otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
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
      !>     C
      !>     C     If the eccentricity is near one, this a parabola.
      !>     C
      !>           IF ( APPROX ( ECC, 1.D0, 10.D-12 ) ) THEN
      !>              TYPE = 'PARABOLA'
      !>
      !>           ELSE IF ( ECC .LT. 1 ) THEN
      !>              TYPE = 'ELLIPSE'
      !>
      !>           ELSE
      !>              TYPE = 'HYPERBOLA'
      !>           END IF
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental logical function approx ( x, y, tol )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: x  !! Double precision numbers.
      real(wp), intent(in) :: y  !! Double precision numbers.
      real(wp), intent(in) :: tol  !! Tolerance.

!
!     Just shorthand, really.
!
      approx = ( abs ( x - y ) <= tol )

      return
      end function approx

      !> Construct a rotation matrix that rotates vectors by a specified
      !> angle about a specified axis.
      !>
      !>## Detailed Input
      !>
      !>     AXIS,
      !>     ANGLE    are, respectively, a rotation axis and a rotation
      !>              angle. AXIS and ANGLE determine a coordinate
      !>              transformation whose effect on any vector V is to
      !>              rotate V by ANGLE radians about the vector AXIS.
      !>
      !>## Detailed Output
      !>
      !>     R        is a rotation matrix representing the coordinate
      !>              transformation determined by AXIS and ANGLE: for
      !>              each vector V, R*V is the vector resulting from
      !>              rotating V by ANGLE radians about AXIS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If AXIS is the zero vector, the rotation generated is the
      !>         identity. This is consistent with the specification of VROTV.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     AXISAR can be thought of as a partial inverse of RAXISA.  AXISAR
      !>     really is a `left inverse': the code fragment
      !>
      !>        CALL RAXISA ( R,    AXIS,  ANGLE )
      !>        CALL AXISAR ( AXIS, ANGLE, R     )
      !>
      !>     preserves R, except for round-off error, as long as R is a
      !>     rotation matrix.
      !>
      !>     On the other hand, the code fragment
      !>
      !>        CALL AXISAR ( AXIS, ANGLE, R     )
      !>        CALL RAXISA ( R,    AXIS,  ANGLE )
      !>
      !>     preserves AXIS and ANGLE, except for round-off error, only if
      !>     ANGLE is in the range (0, pi). So AXISAR is a right inverse
      !>     of RAXISA only over a limited domain.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute a matrix that rotates vectors by pi/2 radians about
      !>        the Z-axis, and compute the rotation axis and angle based on
      !>        that matrix.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM AXISAR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      HALFPI
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ANGLE
      !>      DOUBLE PRECISION      ANGOUT
      !>      DOUBLE PRECISION      AXIS   ( 3    )
      !>      DOUBLE PRECISION      AXOUT  ( 3    )
      !>      DOUBLE PRECISION      ROTMAT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define an axis and an angle for rotation.
      !>C
      !>      AXIS(1) = 0.D0
      !>      AXIS(2) = 0.D0
      !>      AXIS(3) = 1.D0
      !>      ANGLE   = HALFPI()
      !>
      !>C
      !>C     Determine the rotation matrix.
      !>C
      !>      CALL AXISAR ( AXIS, ANGLE, ROTMAT )
      !>
      !>C
      !>C     Now calculate the rotation axis and angle based on
      !>C     ROTMAT as input.
      !>C
      !>      CALL RAXISA ( ROTMAT, AXOUT, ANGOUT )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A)') 'Rotation matrix:'
      !>      WRITE(*,*)
      !>      DO I = 1, 3
      !>         WRITE(*,'(3F10.5)') ( ROTMAT(I,J), J=1,3 )
      !>      END DO
      !>      WRITE(*,*)
      !>      WRITE(*,'(A,3F10.5)') 'Rotation axis       :', AXOUT
      !>      WRITE(*,'(A,F10.5)')  'Rotation angle (deg):',
      !>     .                                      ANGOUT * DPR()
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Rotation matrix:
      !>
      !>           0.00000  -1.00000   0.00000
      !>           1.00000   0.00000   0.00000
      !>           0.00000   0.00000   1.00000
      !>
      !>        Rotation axis       :   0.00000   0.00000   1.00000
      !>        Rotation angle (deg):  90.00000
      !>
      !>     2) Linear interpolation between two rotation matrices.
      !>
      !>        Let R(t) be a time-varying rotation matrix; R could be
      !>        a C-matrix describing the orientation of a spacecraft
      !>        structure. Given two points in time t1 and t2 at which
      !>        R(t) is known, and given a third time t3, where
      !>
      !>           t1  <  t3  <  t2,
      !>
      !>        we can estimate R(t3) by linear interpolation. In other
      !>        words, we approximate the motion of R by pretending that
      !>        R rotates about a fixed axis at a uniform angular rate
      !>        during the time interval [t1, t2]. More specifically, we
      !>        assume that each column vector of R rotates in this
      !>        fashion. This procedure will not work if R rotates through
      !>        an angle of pi radians or more during the time interval
      !>        [t1, t2]; an aliasing effect would occur in that case.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM AXISAR_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      HALFPI
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ANGLE
      !>      DOUBLE PRECISION      AXIS   ( 3    )
      !>      DOUBLE PRECISION      DELTA  ( 3, 3 )
      !>      DOUBLE PRECISION      FRAC
      !>      DOUBLE PRECISION      Q      ( 3, 3 )
      !>      DOUBLE PRECISION      R1     ( 3, 3 )
      !>      DOUBLE PRECISION      R2     ( 3, 3 )
      !>      DOUBLE PRECISION      R3     ( 3, 3 )
      !>      DOUBLE PRECISION      T1
      !>      DOUBLE PRECISION      T2
      !>      DOUBLE PRECISION      T3
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Lets assume that R(t) is the matrix that rotates
      !>C     vectors by pi/2 radians about the Z-axis every
      !>C     minute.
      !>C
      !>C     Let
      !>C
      !>C        R1 = R(t1), for t1 =  0", and
      !>C        R2 = R(t2), for t1 = 60".
      !>C
      !>C     Define both matrices and times.
      !>C
      !>      AXIS(1) = 0.D0
      !>      AXIS(2) = 0.D0
      !>      AXIS(3) = 1.D0
      !>
      !>      T1 =  0.D0
      !>      T2 = 60.D0
      !>      T3 = 30.D0
      !>
      !>      CALL IDENT  ( R1 )
      !>      CALL AXISAR ( AXIS, HALFPI(), R2 )
      !>
      !>C
      !>C     Lets compute
      !>C
      !>C                    -1
      !>C        Q  = R2 * R1  ,
      !>C
      !>C     The rotation axis and angle of Q define the rotation
      !>C     that each column of R(t) undergoes from time `t1' to
      !>C     time `t2'.  Since R(t) is orthogonal, we can find Q
      !>C     using the transpose of R1.  We find the rotation axis
      !>C     and angle via RAXISA.
      !>
      !>      CALL MXMT   ( R2,   R1,    Q      )
      !>      CALL RAXISA ( Q,    AXIS,  ANGLE  )
      !>
      !>C
      !>C     Find the fraction of the total rotation angle that R
      !>C     rotates through in the time interval [t1, t3].
      !>C
      !>      FRAC = ( T3 - T1 )  /  ( T2 - T1 )
      !>
      !>C
      !>C     Finally, find the rotation DELTA that R(t) undergoes
      !>C     during the time interval [t1, t3], and apply that
      !>C     rotation to R1, yielding R(t3), which we'll call R3.
      !>C
      !>      CALL AXISAR ( AXIS,   FRAC * ANGLE,  DELTA  )
      !>      CALL MXM    ( DELTA,  R1,            R3     )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A,F10.5)')  'Time (s)            :', T3
      !>      WRITE(*,'(A,3F10.5)') 'Rotation axis       :', AXIS
      !>      WRITE(*,'(A,F10.5)')  'Rotation angle (deg):',
      !>     .                               FRAC * ANGLE * DPR()
      !>      WRITE(*,'(A)')        'Rotation matrix     :'
      !>      WRITE(*,*)
      !>      DO I = 1, 3
      !>         WRITE(*,'(3F10.5)') ( R3(I,J), J=1,3 )
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Time (s)            :  30.00000
      !>        Rotation axis       :   0.00000   0.00000   1.00000
      !>        Rotation angle (deg):  45.00000
      !>        Rotation matrix     :
      !>
      !>           0.70711  -0.70711   0.00000
      !>           0.70711   0.70711   0.00000
      !>           0.00000   0.00000   1.00000
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
      !>-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code examples based on existing code fragments.
      !>
      !>-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VROTV call.
      !>
      !>        Identity matrix is now obtained from IDENT.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
      pure subroutine axisar ( axis, angle, r )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: axis  ( 3 )  !! Rotation axis.
      real(wp), intent(in) :: angle  !! Rotation angle, in radians.
      real(wp), intent(out) :: r     ( 3, 3 )  !! Rotation matrix corresponding to AXIS and ANGLE.

      real(wp) :: vtemp ( 3 )
      integer(ip) :: i

!
!     First, set R equal to the identity.
!
      call ident ( r )

!
!     The matrix we want rotates EVERY vector by ANGLE about AXIS.
!     In particular, it does so to our basis vectors.  The columns
!     of R are the images of the basis vectors under this rotation.
!
      do i = 1, 3
         call vrotv ( r(1,i), axis, angle, vtemp  )
         r(:,i) = vtemp
      end do

      return
      end subroutine axisar

      !> Convert from range, azimuth and elevation of a point to
      !> rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RANGE    is the distance of the point from the origin. The
      !>              input should be in terms of the same units in which
      !>              the output is desired.
      !>
      !>              Although negative values for RANGE are allowed, its
      !>              use may lead to undesired results. See the $Exceptions
      !>              section for a discussion on this topic.
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
      !>              counterclockwise direction; otherwise it increases
      !>              in the clockwise direction.
      !>
      !>     ELPLSZ   is a flag indicating how the elevation is measured.
      !>
      !>              If ELPLSZ is .TRUE., the elevation increases from
      !>              the XY plane toward +Z; otherwise toward -Z.
      !>
      !>## Detailed Output
      !>
      !>     RECTAN   is an array containing the rectangular coordinates of
      !>              the point.
      !>
      !>              The units associated with the point are those
      !>              associated with the input RANGE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the value of the input argument RANGE is negative
      !>         the output rectangular coordinates will be negated, i.e.
      !>         the resulting array will be of the same length
      !>         but opposite direction to the one that would be obtained
      !>         with a positive input argument RANGE of value ||RANGE||.
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
      !>     This routine converts the azimuth, elevation, and range
      !>     of a point into the associated rectangular coordinates.
      !>
      !>     The input is defined by the distance from the center of
      !>     the reference frame (range), the angle from a reference
      !>     vector (azimuth), and the angle above the XY plane of the
      !>     reference frame (elevation).
      !>
      !>     The way azimuth and elevation are measured depends on the
      !>     values given by the user to the AZCCW and ELPLSZ logical
      !>     flags. See the descriptions of these input arguments
      !>     for details.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create four tables showing a variety of azimuth/elevation
      !>        coordinates and the corresponding rectangular coordinates,
      !>        resulting from the different choices of the AZCCW and ELPLSZ
      !>        flags.
      !>
      !>        Corresponding azimuth/elevation and rectangular coordinates
      !>        are listed to three decimal places. Input angles are in
      !>        degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM AZLREC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(30)        MSG
      !>
      !>      DOUBLE PRECISION      AZ     ( NREC )
      !>      DOUBLE PRECISION      EL     ( NREC )
      !>      DOUBLE PRECISION      RANGE  ( NREC )
      !>      DOUBLE PRECISION      RAZ
      !>      DOUBLE PRECISION      REL
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>
      !>      LOGICAL               AZCCW  ( 2 )
      !>      LOGICAL               ELPLSZ ( 2 )
      !>
      !>C
      !>C     Define the input azimuth/elevation coordinates and the
      !>C     different choices of the AZCCW and ELPLSZ flags.
      !>C
      !>      DATA                  RANGE   /
      !>     .                            0.D0,    1.D0,    1.D0,
      !>     .                            1.D0,    1.D0,    1.D0,
      !>     .                            1.D0,    1.414D0, 1.414D0,
      !>     .                            1.414D0, 1.732D0           /
      !>
      !>      DATA                  AZ     /
      !>     .                            0.D0,    0.D0,   270.D0,
      !>     .                            0.D0,  180.D0,    90.D0,
      !>     .                            0.D0,  315.D0,     0.D0,
      !>     .                          270.D0,  315.D0            /
      !>
      !>      DATA                  EL     /
      !>     .                            0.D0,    0.D0,     0.D0,
      !>     .                          -90.D0,    0.D0,     0.D0,
      !>     .                           90.D0,    0.D0,   -45.D0,
      !>     .                          -45.D0,  -35.264D0         /
      !>
      !>      DATA                  AZCCW   /  .FALSE.,  .TRUE.  /
      !>      DATA                  ELPLSZ  /  .FALSE.,  .TRUE.  /
      !>
      !>C
      !>C     Create a table for each combination of AZCCW and ELPLSZ.
      !>C
      !>      DO I = 1, 2
      !>
      !>         DO J = 1, 2
      !>
      !>C
      !>C           Display the flag settings.
      !>C
      !>            MSG = 'AZCCW = #; ELPLSZ = #'
      !>            CALL REPML ( MSG, '#', AZCCW(I),  'C', MSG )
      !>            CALL REPML ( MSG, '#', ELPLSZ(J), 'C', MSG )
      !>
      !>            WRITE(*,*)
      !>            WRITE(*,'(A)') MSG
      !>
      !>C
      !>C           Print the banner.
      !>C
      !>            WRITE(*,*)
      !>            WRITE(*,'(A)') '   RANGE      AZ       EL   '
      !>     .       //            ' RECT(1)  RECT(2)  RECT(3)'
      !>            WRITE(*,'(A)') '  -------  -------  ------- '
      !>     .       //            ' -------  -------  -------'
      !>
      !>C
      !>C           Do the conversion. Input angles in degrees.
      !>C
      !>            DO N = 1, NREC
      !>
      !>               RAZ = AZ(N) * RPD()
      !>               REL = EL(N) * RPD()
      !>
      !>               CALL AZLREC ( RANGE(N), RAZ,       REL,
      !>     .                       AZCCW(I), ELPLSZ(J), RECTAN )
      !>
      !>               WRITE (*,'(6F9.3)')  RANGE(N), AZ(N), EL(N),
      !>     .                              RECTAN
      !>
      !>            END DO
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        AZCCW = False; ELPLSZ = False
      !>
      !>           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000  270.000    0.000   -0.000    1.000    0.000
      !>            1.000    0.000  -90.000    0.000    0.000    1.000
      !>            1.000  180.000    0.000   -1.000   -0.000    0.000
      !>            1.000   90.000    0.000    0.000   -1.000    0.000
      !>            1.000    0.000   90.000    0.000    0.000   -1.000
      !>            1.414  315.000    0.000    1.000    1.000    0.000
      !>            1.414    0.000  -45.000    1.000    0.000    1.000
      !>            1.414  270.000  -45.000   -0.000    1.000    1.000
      !>            1.732  315.000  -35.264    1.000    1.000    1.000
      !>
      !>        AZCCW = False; ELPLSZ = True
      !>
      !>           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000  270.000    0.000   -0.000    1.000    0.000
      !>            1.000    0.000  -90.000    0.000    0.000   -1.000
      !>            1.000  180.000    0.000   -1.000   -0.000    0.000
      !>            1.000   90.000    0.000    0.000   -1.000    0.000
      !>            1.000    0.000   90.000    0.000    0.000    1.000
      !>            1.414  315.000    0.000    1.000    1.000    0.000
      !>            1.414    0.000  -45.000    1.000    0.000   -1.000
      !>            1.414  270.000  -45.000   -0.000    1.000   -1.000
      !>            1.732  315.000  -35.264    1.000    1.000   -1.000
      !>
      !>        AZCCW = True; ELPLSZ = False
      !>
      !>           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000  270.000    0.000   -0.000   -1.000    0.000
      !>            1.000    0.000  -90.000    0.000    0.000    1.000
      !>            1.000  180.000    0.000   -1.000    0.000    0.000
      !>            1.000   90.000    0.000    0.000    1.000    0.000
      !>            1.000    0.000   90.000    0.000    0.000   -1.000
      !>            1.414  315.000    0.000    1.000   -1.000    0.000
      !>            1.414    0.000  -45.000    1.000    0.000    1.000
      !>            1.414  270.000  -45.000   -0.000   -1.000    1.000
      !>            1.732  315.000  -35.264    1.000   -1.000    1.000
      !>
      !>        AZCCW = True; ELPLSZ = True
      !>
      !>           RANGE      AZ       EL    RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000  270.000    0.000   -0.000   -1.000    0.000
      !>            1.000    0.000  -90.000    0.000    0.000   -1.000
      !>            1.000  180.000    0.000   -1.000    0.000    0.000
      !>            1.000   90.000    0.000    0.000    1.000    0.000
      !>            1.000    0.000   90.000    0.000    0.000    1.000
      !>            1.414  315.000    0.000    1.000   -1.000    0.000
      !>            1.414    0.000  -45.000    1.000    0.000   -1.000
      !>            1.414  270.000  -45.000   -0.000   -1.000   -1.000
      !>            1.732  315.000  -35.264    1.000   -1.000   -1.000
      !>
      !>     2) Compute the right ascension and declination of the pointing
      !>        direction of DSS-14 station at a given epoch.
      !>
      !>        Task Description
      !>        ================
      !>
      !>        In this example, we will obtain the right ascension and
      !>        declination of the pointing direction of the DSS-14 station at
      !>        a given epoch, by converting the station's pointing direction
      !>        given in azimuth and elevation to rectangular coordinates
      !>        in the station topocentric reference frame and applying a
      !>        frame transformation from DSS-14_TOPO to J2000, in order to
      !>        finally obtain the corresponding right ascension and
      !>        declination of the pointing vector.
      !>
      !>        In order to introduce the usage of the logical flags AZCCW
      !>        and ELPLSZ, we will assume that the azimuth is measured
      !>        counterclockwise and the elevation negative towards +Z
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
      !>           File name: azlrec_ex2.tm
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
      !>             File name                        Contents
      !>             ---------                        --------
      !>             naif0011.tls                     Leapseconds
      !>             earth_720101_070426.bpc          Earth historical
      !>                                              binary PCK
      !>             earth_topo_050714.tf             DSN station FK
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = ( 'naif0011.tls',
      !>                               'earth_720101_070426.bpc',
      !>                               'earth_topo_050714.tf'     )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM AZLREC_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT0
      !>      PARAMETER           ( FMT0   = '(A,3F15.8)' )
      !>
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1   = '(A,F15.8)' )
      !>
      !>      CHARACTER*(*)         META
      !>      PARAMETER           ( META   = 'azlrec_ex2.tm' )
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
      !>      CHARACTER*(40)        MSG
      !>      CHARACTER*(TIMLEN)    OBSTIM
      !>      CHARACTER*(FRNMLN)    REF
      !>
      !>      DOUBLE PRECISION      AZ
      !>      DOUBLE PRECISION      AZR
      !>      DOUBLE PRECISION      DEC
      !>      DOUBLE PRECISION      EL
      !>      DOUBLE PRECISION      ELR
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      JPOS   ( 3 )
      !>      DOUBLE PRECISION      PTARG  ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RA
      !>      DOUBLE PRECISION      RANGE
      !>      DOUBLE PRECISION      ROTATE ( 3, 3 )
      !>
      !>      INTEGER               I
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
      !>C     Set the local topocentric frame
      !>C
      !>      REF    = 'DSS-14_TOPO'
      !>
      !>C
      !>C     Set the station's pointing direction in azimuth and
      !>C     elevation. Set arbitrarily the range to 1.0. Azimuth
      !>C     and elevation shall be given in radians. Azimuth
      !>C     increases counterclockwise and elevation is negative
      !>C     towards +Z (above the local horizon)
      !>C
      !>      AZ     =   75.00
      !>      EL     =  -27.25
      !>      AZR    =   AZ * RPD()
      !>      ELR    =   EL * RPD()
      !>      R      =    1.00
      !>      AZCCW  = .TRUE.
      !>      ELPLSZ = .FALSE.
      !>
      !>C
      !>C     Obtain the rectangular coordinates of the station's
      !>C     pointing direction.
      !>C
      !>      CALL AZLREC ( R, AZR, ELR, AZCCW, ELPLSZ, PTARG )
      !>
      !>C
      !>C     Transform the station's pointing vector from the
      !>C     local topocentric frame to J2000.
      !>C
      !>      CALL PXFORM ( REF,   'J2000',   ET, ROTATE )
      !>      CALL MXV    ( ROTATE,  PTARG, JPOS         )
      !>
      !>C
      !>C     Compute the right ascension and declination.
      !>C     Express both angles in degrees.
      !>C
      !>      CALL RECRAD ( JPOS, RANGE, RA, DEC )
      !>      RA =   RA * DPR()
      !>      DEC =   DEC * DPR()
      !>
      !>C
      !>C     Display the computed pointing vector, the input
      !>C     data and resulting the angles.
      !>C
      !>      WRITE (*,*)
      !>      WRITE (*,FMT1) 'Pointing azimuth    (deg): ', AZ
      !>      WRITE (*,FMT1) 'Pointing elevation  (deg): ', EL
      !>
      !>      CALL REPML ( 'Azimuth counterclockwise?: #', '#',
      !>     .              AZCCW, 'C', MSG                    )
      !>      WRITE (*,'(A)') MSG
      !>
      !>      CALL REPML ( 'Elevation positive +Z?   : #', '#',
      !>     .              ELPLSZ, 'C', MSG                   )
      !>      WRITE (*,'(A)') MSG
      !>
      !>      WRITE (*,'(2A)') 'Observation epoch        : ', OBSTIM
      !>      WRITE (*,*)
      !>      WRITE (*,'(A)') 'Pointing direction (normalized):   '
      !>      WRITE (*,FMT0) '  ', ( PTARG(I), I = 1, 3 )
      !>      WRITE (*,*)
      !>      WRITE (*,FMT1) 'Pointing right ascension (deg): ', RA
      !>      WRITE (*,FMT1) 'Pointing declination (deg):     ', DEC
      !>      WRITE (*,*)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Pointing azimuth    (deg):     75.00000000
      !>        Pointing elevation  (deg):    -27.25000000
      !>        Azimuth counterclockwise?: True
      !>        Elevation positive +Z?   : False
      !>        Observation epoch        : 2003 OCT 13 06:00:00.000000 UTC
      !>
      !>        Pointing direction (normalized):
      !>               0.23009457     0.85872462     0.45787392
      !>
      !>        Pointing right ascension (deg):    280.06179939
      !>        Pointing declination (deg):         26.92826084
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
      pure subroutine azlrec ( range, az, el, azccw, elplsz, rectan )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: range  !! Distance of the point from the origin.
      real(wp), intent(in) :: az  !! Azimuth in radians.
      real(wp), intent(in) :: el  !! Elevation in radians.
      logical, intent(in) :: azccw  !! Flag indicating how azimuth is measured.
      logical, intent(in) :: elplsz  !! Flag indicating how elevation is measured.
      real(wp), intent(out) :: rectan ( 3 )  !! Rectangular coordinates of a point.

      real(wp) :: azimut
      real(wp) :: elevat

!
!     Error free routine. No check-in.
!
      azimut = az
      elevat = el

!
!     We are going to use RADREC to convert the azimuth and elevation
!     to rectangular coordinates. In RADREC, the right ascension
!     is measured counterclockwise from +X axis about the +Z axis, and
!     the declination is measured positive from the XY plane towards
!     +Z axis.
!
!     Check the AZCCW and ELPLSZ flags and convert AZ and EL to the
!     coordinate system used by RADREC.
!
!     If AZCCW is set to .FALSE. the azimuth is measured clockwise
!     from the +X axis about the +Z axis.
!
      if ( .not. azccw ) then
!
!        We can simply negate AZ; we don't need to map it into the
!        range [0, 2*pi]. LATREC will accept it as is.
!
!        Negate only non-zero values of AZ. Avoid creating
!        -0.D0 values, which affect printed outputs generated
!        by example programs.
!
         if ( az /= 0.0_wp ) then
            azimut = -az
         end if

      end if

!
!     If ELPLSZ is set to .FALSE. the elevation is measured positive
!     from the XY plane towards the -Z axis.
!
      if ( .not. elplsz ) then
!
!        Negate only non-zero values of EL. Avoid creating
!        -0.D0 values, which affect printed outputs generated
!        by example programs.
!
         if ( el /= 0.0_wp ) then
            elevat = -el
         end if

      end if

!
!     In principle, we could call the subroutine RADREC to convert AZ
!     and EL to rectangular coordinates. RADREC simply passes its
!     inputs to LATREC, so we bypass the middleman.
!
!     We rely on LATREC to handle the case of RANGE < 0.
!
      call latrec ( range, azimut, elevat, rectan )

      return
      end subroutine azlrec

      !> Bracket a double precision number. That is, given a number and an
      !> acceptable interval, make sure that the number is contained in the
      !> interval. (If the number is already in the interval, leave it
      !> alone. If not, set it to the nearest endpoint of the interval.)
      !>
      !> The function returns the bracketed number.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MIN and MAX instead).
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is the number to be bracketed. That is, the
      !>              value of NUMBER is constrained to lie in the
      !>              interval bounded by END1 and END2.
      !>
      !>     END1,
      !>     END2     are the lower and upper bounds for NUMBER. The
      !>              order is not important.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the bracketed number. That is NUMBER, if it
      !>     was already in the interval provided. Otherwise the returned
      !>     value is the nearest bound of the interval.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides a shorthand notation for code fragments
      !>     like the following:
      !>
      !>        IF ( END1 .LT. END2 ) THEN
      !>           IF      ( NUMBER .LT. END1 ) THEN
      !>              NUMBER = END1
      !>           ELSE IF ( NUMBER .GT. END2 ) THEN
      !>              NUMBER = END2
      !>           END IF
      !>        ELSE
      !>           IF      ( NUMBER .LT. END2 ) THEN
      !>              NUMBER = END2
      !>           ELSE IF ( NUMBER .GT. END1 ) THEN
      !>              NUMBER = END1
      !>           END IF
      !>        END IF
      !>
      !>     which occur frequently during the processing of program inputs.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following code example illustrates the operation of
      !>        BRCKTD.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BRCKTD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION        BRCKTD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ = 4  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        END1    ( LISTSZ )
      !>      DOUBLE PRECISION        END2    ( LISTSZ )
      !>      DOUBLE PRECISION        NUMBER  ( LISTSZ )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Set the values for the example.
      !>C
      !>      DATA                    END1   /  1.D0,   1.D0,
      !>     .                                 10.D0, -10.D0  /
      !>      DATA                    END2   / 10.D0,  10.D0,
      !>     .                                -10.D0,  -1.D0  /
      !>      DATA                    NUMBER / -1.D0,  29.D0,
      !>     .                                  3.D0,   3.D0  /
      !>
      !>      WRITE(*,'(A)') ' Number  End1   End2   Bracketed'
      !>      WRITE(*,'(A)') ' ------  -----  -----  ---------'
      !>
      !>      DO I = 1, LISTSZ
      !>
      !>         WRITE(*,'(3F7.1,F11.1)') NUMBER(I), END1(I), END2(I),
      !>     .                 BRCKTD ( NUMBER(I), END1(I), END2(I) )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Number  End1   End2   Bracketed
      !>         ------  -----  -----  ---------
      !>           -1.0    1.0   10.0        1.0
      !>           29.0    1.0   10.0       10.0
      !>            3.0   10.0  -10.0        3.0
      !>            3.0  -10.0   -1.0       -1.0
      !>
      !>     2) The following code example illustrates a typical use for
      !>        BRCKTD: force a star magnitude limit to be within a range.
      !>        Note that this code assumes that the user provided value
      !>        is a valid double precision number.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BRCKTD_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION        BRCKTD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER                 KWDSZ
      !>      PARAMETER             ( KWDSZ = 30   )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(KWDSZ)       USRIN
      !>
      !>      DOUBLE PRECISION        MAGLIN
      !>      DOUBLE PRECISION        MAGLOK
      !>
      !>C
      !>C     Prompt the user for the star magnitude.
      !>C
      !>      CALL PROMPT ( 'Enter star magnitude: ', USRIN )
      !>
      !>C
      !>C     Convert the user input to double precision.
      !>C
      !>      CALL PRSDP ( USRIN, MAGLIN )
      !>
      !>C
      !>C     Star magnitude must be in the range 0-10.
      !>C
      !>      MAGLOK = BRCKTD ( MAGLIN, 0.D0, 10.D0 )
      !>
      !>C
      !>C     Display confirmation message.
      !>C
      !>      IF ( MAGLIN .NE. MAGLOK ) THEN
      !>
      !>         WRITE(*,'(A,F4.1,A)') 'Provided star magnitude ',
      !>     .                 MAGLIN, ' is out of range (0-10).'
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(A,F4.1,A)') 'Provided star magnitude ',
      !>     .                 MAGLIN, ' is in range (0-10).'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using '10.1' as user provided input, the output was:
      !>
      !>        Enter star magnitude: 10.1
      !>        Provided star magnitude 10.1 is out of range (0-10).
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-AUG-2021 (JDR) (BVS)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples based on existing code fragments.
      !>
      !>        Updated code fragment in $Particulars to show that the
      !>        order of endpoints is not important.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 30-DEC-1988 (WLT)
      !>
      !>        The routine was modified so that the order of the endpoints
      !>        of the bracketing interval is not needed. The routine now
      !>        determines which is the left endpoint and which is the
      !>        right and acts appropriately.
      elemental real(wp) function brcktd ( number, end1, end2 )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: number  !! Number to be bracketed.
      real(wp), intent(in) :: end1  !! One of the bracketing endpoints for NUMBER.
      real(wp), intent(in) :: end2  !! The other bracketing endpoint for NUMBER.

!
!     What else is there to say?
!
      brcktd = max(min(end1, end2), min(max(end1, end2), number))
      end function brcktd

      !> Bracket an integer number. That is, given a number and an
      !> acceptable interval, make sure that the number is contained in the
      !> interval. (If the number is already in the interval, leave it
      !> alone. If not, set it to the nearest endpoint of the interval.)
      !>
      !> The function returns the bracketed number.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MIN and MAX instead).
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is the number to be bracketed. That is, the
      !>              value of NUMBER is constrained to lie in the
      !>              interval bounded by END1 and END2.
      !>
      !>     END1,
      !>     END2     are the lower and upper bounds for NUMBER. The
      !>              order is not important.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the bracketed number. That is NUMBER, if it
      !>     was already in the interval provided. Otherwise the returned
      !>     value is the nearest bound of the interval.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides a shorthand notation for code fragments
      !>     like the following:
      !>
      !>        IF ( END1 .LT. END2 ) THEN
      !>           IF      ( NUMBER .LT. END1 ) THEN
      !>              NUMBER = END1
      !>           ELSE IF ( NUMBER .GT. END2 ) THEN
      !>              NUMBER = END2
      !>           END IF
      !>        ELSE
      !>           IF      ( NUMBER .LT. END2 ) THEN
      !>              NUMBER = END2
      !>           ELSE IF ( NUMBER .GT. END1 ) THEN
      !>              NUMBER = END1
      !>           END IF
      !>        END IF
      !>
      !>     which occur frequently during the processing of program inputs.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following code example illustrates the operation of
      !>        BRCKTI.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BRCKTI_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 BRCKTI
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER                 LISTSZ
      !>      PARAMETER             ( LISTSZ = 4  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER                 END1    ( LISTSZ )
      !>      INTEGER                 END2    ( LISTSZ )
      !>      INTEGER                 I
      !>      INTEGER                 NUMBER  ( LISTSZ )
      !>
      !>C
      !>C     Set the values for the example.
      !>C
      !>      DATA                    END1   /  1,  1,  10, -10 /
      !>      DATA                    END2   / 10, 10, -10,  -1 /
      !>      DATA                    NUMBER / -1, 29,   3,   3 /
      !>
      !>      WRITE(*,'(A)') 'Number  End1  End2  Bracketed'
      !>      WRITE(*,'(A)') '------  ----  ----  ---------'
      !>
      !>      DO I = 1, LISTSZ
      !>
      !>         WRITE(*,'(3I6,I11)') NUMBER(I), END1(I), END2(I),
      !>     .             BRCKTI ( NUMBER(I), END1(I), END2(I) )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Number  End1  End2  Bracketed
      !>        ------  ----  ----  ---------
      !>            -1     1    10          1
      !>            29     1    10         10
      !>             3    10   -10          3
      !>             3   -10    -1         -1
      !>
      !>     2) The following code example illustrates a typical use for
      !>        BRCKTI: force an identifier to be within a range. Note that
      !>        this code assumes that the user provided value is a valid
      !>        integer number.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BRCKTI_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER                 BRCKTI
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER                 KWDSZ
      !>      PARAMETER             ( KWDSZ = 30   )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(KWDSZ)       USRIN
      !>
      !>      INTEGER                 CODEIN
      !>      INTEGER                 CODEOK
      !>
      !>C
      !>C     Prompt the user for the code identifier.
      !>C
      !>      CALL PROMPT ( 'Enter object code: ', USRIN )
      !>
      !>C
      !>C     Convert the user input to integer.
      !>C
      !>      CALL PRSINT ( USRIN, CODEIN )
      !>
      !>C
      !>C     Object code must be in the range 701-705.
      !>C
      !>      CODEOK = BRCKTI ( CODEIN, 701, 705 )
      !>
      !>C
      !>C     Display confirmation message.
      !>C
      !>      IF ( CODEIN .NE. CODEOK ) THEN
      !>
      !>         WRITE(*,'(A,I3,A)') 'Provided object code ', CODEIN,
      !>     .                       ' is out of range (701-705).'
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(A,I3,A)') 'Provided object code ', CODEIN,
      !>     .                       ' is in range (701-705).'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using '710' as user provided input, the output was:
      !>
      !>        Enter object code: 710
      !>        Provided object code 710 is out of range (701-705).
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-AUG-2021 (JDR) (BVS)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples based on existing code fragment.
      !>
      !>        Updated code fragment in $Particulars to show that the
      !>        order of endpoints is not important.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 30-DEC-1988 (WLT)
      !>
      !>        The routine was modified so that the order of the endpoints
      !>        of the bracketing interval is not needed. The routine now
      !>        determines which is the left endpoint and which is the
      !>        right and acts appropriately.
      elemental integer(ip)  function brckti ( number, end1, end2 )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: number  !! Number to be bracketed.
      integer(ip), intent(in) :: end1  !! One of the bracketing endpoints for NUMBER.
      integer(ip), intent(in) :: end2  !! The other bracketing endpoint for NUMBER.

!
!     What else is there to say?
!
      brckti = max(min(end1, end2), min(max(end1, end2), number))
      end function brckti

      !> Return the value of a polynomial and its first NDERIV
      !> derivatives, evaluated at the input X, using the coefficients of
      !> the Chebyshev expansion of the polynomial.
      !>
      !>## Detailed Input
      !>
      !>     CP       is an array of coefficients a polynomial with respect
      !>              to the Chebyshev basis. The polynomial to be
      !>              evaluated is assumed to be of the form:
      !>
      !>                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>              where T(I,S) is the I'th Chebyshev polynomial
      !>              evaluated at a number S whose double precision
      !>              value lies between -1 and 1. The value of S is
      !>              computed from the input variables X2S(1), X2S(2)
      !>              and X.
      !>
      !>     DEGP     is the degree of the Chebyshev polynomial to be
      !>              evaluated.
      !>
      !>     X2S      is an array of two parameters. These parameters are
      !>              used to transform the domain of the input variable X
      !>              into the standard domain of the Chebyshev polynomial.
      !>              X2S(1) should be a reference point in the domain of
      !>              X; X2S(2) should be the radius by which points are
      !>              allowed to deviate from the reference point and while
      !>              remaining within the domain of X. The value of
      !>              X is transformed into the value S given by
      !>
      !>                 S = ( X - X2S(1) ) / X2S(2)
      !>
      !>              Typically X2S(1) is the midpoint of the interval over
      !>              which X is allowed to vary and X2S(2) is the radius
      !>              of the interval.
      !>
      !>              The main reason for doing this is that a Chebyshev
      !>              expansion is usually fit to data over a span
      !>              from A to B where A and B are not -1 and 1
      !>              respectively. Thus to get the "best fit" the
      !>              data was transformed to the interval [-1,1] and
      !>              coefficients generated. These coefficients are
      !>              not rescaled to the interval of the data so that
      !>              the numerical "robustness" of the Chebyshev fit will
      !>              not be lost. Consequently, when the "best fitting"
      !>              polynomial needs to be evaluated at an intermediate
      !>              point, the point of evaluation must be transformed
      !>              in the same way that the generating points were
      !>              transformed.
      !>
      !>     X        is the value for which the polynomial is to be
      !>              evaluated.
      !>
      !>     NDERIV   is the number of derivatives to be computed by the
      !>              routine. NDERIV should be non-negative.
      !>
      !>     PARTDP   is a work space used by the program to compute
      !>              all of the desired derivatives. It should be declared
      !>              in the calling program as
      !>
      !>                 DOUBLE PRECISION    PARTDP(3, 0:NDERIV)
      !>
      !>## Detailed Output
      !>
      !>     DPDXS    is an array containing the value of the polynomial and
      !>              its derivatives evaluated at X.
      !>
      !>              DPDXS(0) is the value of the polynomial to be evaluated.
      !>              It is given by
      !>
      !>                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>              where T(I,S) is the I'th Chebyshev polynomial
      !>              evaluated  at a number S = ( X - X2S(1) )/X2S(2).
      !>
      !>              DPDXS(I) is the value of the I'th derivative of the
      !>              polynomial at X (I ranges from 1 to NDERIV). It is given
      !>              by
      !>
      !>                                             [i]
      !>                 (1/X2S(2)**I) ( CP(DEGP+1)*T   (DEGP,S)
      !>
      !>                                           [i]
      !>                               + CP(DEGP)*T   (DEGP-1,S)
      !>
      !>                               + ...
      !>
      !>                                        [i]
      !>                               + CP(2)*T   (1,S)
      !>
      !>                                        [i]
      !>                               + CP(1)*T   (0,S) )
      !>
      !>              where T(k,S) is the K'th Chebyshev polynomial and the
      !>              superscript [i] indicates its I'th derivative,
      !>              evaluated at the number S = ( X - X2S(1) )/X2S(2).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No tests are performed for exceptional values (NDERIV
      !>         negative, DEGP negative, etc.). This routine is expected to
      !>         be used at a low level in ephemeris evaluations. For that
      !>         reason it has been elected as a routine that will not
      !>         participate in error handling.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the value of a Chebyshev polynomial
      !>     expansion and the derivatives of the expansion with respect to X.
      !>     The polynomial is given by
      !>
      !>        CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                             + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>     where
      !>
      !>        S  =  ( X - X2S(1) ) / X2S(2)
      !>
      !>     and
      !>
      !>        T(I,S) is the I'th Chebyshev polynomial of the first kind
      !>        evaluated at S.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Depending upon the user's needs, there are 3 routines
      !>        available for evaluating Chebyshev polynomials.
      !>
      !>           CHBVAL   for evaluating a Chebyshev polynomial when no
      !>                    derivatives are desired.
      !>
      !>           CHBINT   for evaluating a Chebyshev polynomial and its
      !>                    first derivative.
      !>
      !>           CHBDER   for evaluating a Chebyshev polynomial and a user
      !>                    or application dependent number of derivatives.
      !>
      !>        Of these 3 the one most commonly employed by SPICE software
      !>        is CHBINT as it is used to interpolate ephemeris state
      !>        vectors; this requires the evaluation of a polynomial
      !>        and its derivative. When no derivatives are desired one
      !>        should use CHBVAL, or when more than one or an unknown
      !>        number of derivatives are desired one should use CHBDER.
      !>
      !>        The code example below illustrates how this routine might
      !>        be used to obtain points for plotting a polynomial
      !>        and its derivatives.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CHBDER_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CP     (7)
      !>
      !>C
      !>C     Dimension DPDXS as NDERIV + 1.
      !>C
      !>      DOUBLE PRECISION      DPDXS  (3+1)
      !>
      !>C
      !>C     Dimension partdp as 3 * (NDERIV + 1)
      !>C
      !>      DOUBLE PRECISION      PARTDP (3 * 4)
      !>      DOUBLE PRECISION      X
      !>      DOUBLE PRECISION      X2S    (2)
      !>
      !>      INTEGER               DEGP
      !>      INTEGER               I
      !>      INTEGER               NDERIV
      !>
      !>      DATA                  CP     / 1.D0,  3.D0,  0.5D0,
      !>     .                               1.D0,  0.5D0, -1.D0,
      !>     .                               1.D0               /
      !>      DATA                  X2S    / 0.5D0, 3.D0 /
      !>
      !>      DEGP   = 6
      !>      NDERIV = 3
      !>      X      = 1.D0
      !>
      !>      CALL CHBDER ( CP, DEGP, X2S, X, NDERIV, PARTDP, DPDXS )
      !>
      !>      WRITE(*,'(A,F10.6)') 'Value of the polynomial at X=1: ',
      !>     .                                               DPDXS(1)
      !>
      !>      DO I=2, NDERIV+1
      !>         WRITE(*,'(A,I1,A,F10.6)') '   Derivative ', I-1,
      !>     .                      ' at X=1        : ', DPDXS(I)
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Value of the polynomial at X=1:  -0.340878
      !>           Derivative 1 at X=1        :   0.382716
      !>           Derivative 2 at X=1        :   4.288066
      !>           Derivative 3 at X=1        :  -1.514403
      !>
      !>## Restrictions
      !>
      !>     1)  The user must be sure that the provided workspace is declared
      !>         properly in the calling routine. The proper declaration is:
      !>
      !>            INTEGER            NDERIV
      !>            PARAMETER        ( NDERIV = desired number of derivatives )
      !>
      !>            DOUBLE PRECISION   PARTDP (3, 0:NDERIV)
      !>
      !>         If for some reason a parameter is not passed to this routine
      !>         in NDERIV, the user should make sure that the value of NDERIV
      !>         is not so large that the work space provided is inadequate.
      !>
      !>     2)  One needs to be careful that the value
      !>
      !>            (X-X2S(1)) / X2S(2)
      !>
      !>         lies between -1 and 1. Otherwise, the routine may fail
      !>         spectacularly (for example with a floating point overflow).
      !>
      !>     3)  While this routine will compute derivatives of the input
      !>         polynomial, the user should consider how accurately the
      !>         derivatives of the Chebyshev fit, match the derivatives of the
      !>         function it approximates.
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence
      !>          Formula," p 161, Cambridge University Press, 1986.
      !>
      !>     [2]  T. Rivlin, "The Chebyshev Polynomials," Wiley, 1974.
      !>
      !>     [3]  R. Weast and S. Selby, "CRC Handbook of Tables for
      !>          Mathematics," 4th Edition, CRC Press, 1976.
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
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>        Fixed formulae in $Detailed_Output: replaced P(2) and P(1) by
      !>        X2S(2) and X2S(1) respectively.
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
      !>-    Beta Version 1.0.1, 16-FEB-1988 (WLT) (NJB)
      !>
      !>        The Error free specification was added to the routine as
      !>        well as an explanation for this designation. Examples added.
      !>        Declaration of unused variable RECIP removed.
      pure subroutine chbder ( cp, degp, x2s, x, nderiv, partdp, dpdxs )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: cp     (      * )  !! DEGP+1 Chebyshev polynomial coefficients.
      integer(ip), intent(in) :: degp  !! Degree of polynomial.
      real(wp), intent(in) :: x2s    (      2 )  !! Transformation parameters of polynomial.
      real(wp), intent(in) :: x  !! Value for which the polynomial is to be evaluated.
      integer(ip), intent(in) :: nderiv  !! The number of derivatives to compute.
      real(wp), intent(inout) :: partdp ( 3, 0:* )  !! Workspace provided for computing derivatives.
      real(wp), intent(out) :: dpdxs  (    0:* )  !! Array of the derivatives of the polynomial.

      real(wp) :: scale
      real(wp) :: s
      real(wp) :: s2

      integer(ip) :: i
      integer(ip) :: j

!
!     Transform X to S and initialize temporary variables.
!
      s     = (x-x2s(1)) / x2s(2)
      s2    = 2.0_wp * s
      j     = degp  + 1

      do i = 0,nderiv
         partdp (1,i) = 0.0_wp
         partdp (2,i) = 0.0_wp
      end do

!
!     Evaluate the polynomial ...
!
      do while ( j > 1 )

         partdp (3,0) = partdp (2,0)
         partdp (2,0) = partdp (1,0)
         partdp (1,0) = cp (j)   + ( s2*partdp(2,0)  - partdp(3,0) )

!
!        ... and its derivatives using recursion.
!
         scale = 2.0_wp

         do i = 1, nderiv

            partdp (3,i) =   partdp(2,i  )
            partdp (2,i) =   partdp(1,i  )

            partdp (1,i) =   partdp(2,i-1)*scale &
                           + partdp(2,i  )*s2 &
                           - partdp(3,i  )

            scale        =   scale + 2.0_wp
         end do

         j        = j - 1

      end do

      dpdxs(0) = cp(1) + ( s*partdp(1,0)  - partdp(2,0)  )

      scale    = 1.0_wp

      do i = 1, nderiv
         dpdxs(i)  =   partdp(1,i-1)*scale &
                     + partdp(1,i  )*s &
                     - partdp(2,i  )

         scale  =   scale + 1
      end do

!
!     Scale the k'th derivative w.r.t S by (1/X2S(2)**k) so that we have
!     the derivatives
!
!                    2          3          4          5
!        d P(S)     d P(S)     d P(S)     d P(S)     d P(S)
!        ------     ------     ------     ------     ------
!                       2          3          4          5
!          dX         dX         dX         dX         dX
!
!     NOTE: In the loop that follows we perform division instead of
!           multiplying by reciprocals so that the algorithm matches
!           CHBINT.  If multiplication by reciprocals is performed
!           CHBINT and CHBDER (although mathematically equivalent) will
!           not produce identical results for the first derivative.
!
      scale  = x2s(2)

      do i = 1, nderiv
         dpdxs(i) = dpdxs(i) / scale
         scale    = x2s(2)   * scale
      end do

      return
      end subroutine chbder

      !> Return the value of a polynomial and its derivative, evaluated at
      !> the input X, using the coefficients of the Chebyshev expansion of
      !> the polynomial.
      !>
      !>## Detailed Input
      !>
      !>     CP       is an array of coefficients of a polynomial with
      !>              respect to the Chebyshev basis. The polynomial to be
      !>              evaluated is assumed to be of the form:
      !>
      !>                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>              where T(I,S) is the I'th Chebyshev polynomial
      !>              evaluated at a number S whose double precision
      !>              value lies between -1 and 1. The value of S is
      !>              computed from the input variables X2S(1), X2S(2) and X.
      !>
      !>     DEGP     is the degree of the Chebyshev polynomial to be
      !>              evaluated.
      !>
      !>     X2S      is an array of two parameters. These parameters are
      !>              used to transform the domain of the input variable X
      !>              into the standard domain of the Chebyshev polynomial.
      !>              X2S(1) should be a reference point in the domain of X;
      !>              X2S(2) should be the radius by which points are
      !>              allowed to deviate from the reference point and while
      !>              remaining within the domain of X. The value of
      !>              X is transformed into the value S given by
      !>
      !>                 S = ( X - X2S(1) ) / X2S(2)
      !>
      !>              Typically X2S(1) is the midpoint of the interval over
      !>              which X is allowed to vary and X2S(2) is the radius of
      !>              the interval.
      !>
      !>              The main reason for doing this is that a Chebyshev
      !>              expansion is usually fit to data over a span
      !>              from A to B where A and B are not -1 and 1
      !>              respectively. Thus to get the "best fit" the
      !>              data was transformed to the interval [-1,1] and
      !>              coefficients generated. These coefficients are
      !>              not rescaled to the interval of the data so that
      !>              the numerical "robustness" of the Chebyshev fit will
      !>              not be lost. Consequently, when the "best fitting"
      !>              polynomial needs to be evaluated at an intermediate
      !>              point, the point of evaluation must be transformed
      !>              in the same way that the generating points were
      !>              transformed.
      !>
      !>     X        is the value for which the polynomial is to be
      !>              evaluated.
      !>
      !>## Detailed Output
      !>
      !>     P        is the value of the polynomial to be evaluated. It
      !>              is given by
      !>
      !>                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>              where T(I,S) is the I'th Chebyshev polynomial
      !>              evaluated  at a number S = ( X - X2S(1) )/X2S(2)
      !>
      !>     DPDX     is the value of the derivative of the polynomial at X.
      !>              It is given by
      !>
      !>                 1/X2S(2) [    CP(DEGP+1)*T'(DEGP,S)
      !>
      !>                             + CP(DEGP)*T'(DEGP-1,S)
      !>
      !>                             + ...
      !>
      !>                             + CP(2)*T'(1,S)
      !>
      !>                             + CP(1)*T'(0,S) ]
      !>
      !>              where T(I,S) and T'(I,S) are the I'th Chebyshev
      !>              polynomial and its derivative, respectively,
      !>              evaluated  at a number S = ( X - X2S(1) )/X2S(2)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No tests are performed for exceptional values (DEGP negative,
      !>         etc.). This routine is expected to be used at a low level in
      !>         ephemeris evaluations. For that reason it has been elected as
      !>         a routine that will not participate in error handling.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the value of a Chebyshev polynomial
      !>     expansion and the derivative of the expansion with respect to X.
      !>     The polynomial is given by
      !>
      !>        CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                             + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>     where
      !>
      !>        S  =  ( X - X2S(1) ) / X2S(2)
      !>
      !>     and
      !>
      !>        T(I,S) is the I'th Chebyshev polynomial of the first kind
      !>        evaluated at S.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Depending upon the user's needs, there are 3 routines
      !>        available for evaluating Chebyshev polynomials.
      !>
      !>           CHBVAL   for evaluating a Chebyshev polynomial when no
      !>                    derivatives are desired.
      !>
      !>           CHBINT   for evaluating a Chebyshev polynomial and its
      !>                    first derivative.
      !>
      !>           CHBDER   for evaluating a Chebyshev polynomial and a user
      !>                    or application dependent number of derivatives.
      !>
      !>        Of these 3 the one most commonly employed by SPICE software
      !>        is CHBINT as it is used to interpolate ephemeris state
      !>        vectors; this requires the evaluation of a polynomial
      !>        and its derivative. When no derivatives are desired one
      !>        should use CHBVAL, or when more than one or an unknown
      !>        number of derivatives are desired one should use CHBDER.
      !>
      !>        The code example below illustrates how this routine might
      !>        be used to obtain points for plotting a polynomial
      !>        and its derivative.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CHBINT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CP     (7)
      !>      DOUBLE PRECISION      DPDX
      !>      DOUBLE PRECISION      X
      !>      DOUBLE PRECISION      P
      !>      DOUBLE PRECISION      X2S    (2)
      !>
      !>      INTEGER               DEGP
      !>      INTEGER               I
      !>
      !>C
      !>C     Set the coefficients of the polynomial and its
      !>C     transformation parameters
      !>C
      !>      DATA                  CP     / 1.D0,  3.D0,  0.5D0,
      !>     .                               1.D0,  0.5D0, -1.D0,
      !>     .                               1.D0               /
      !>      DATA                  X2S    / 0.5D0, 3.D0 /
      !>
      !>      DEGP   = 6
      !>      X      = 1.D0
      !>
      !>      CALL CHBINT ( CP, DEGP, X2S, X, P, DPDX )
      !>
      !>      WRITE(*,'(A,F10.6)')
      !>     .        'Value of the polynomial at X=1: ', P
      !>      WRITE(*,'(A,F10.6)') '   First derivative'
      !>     .                 //  ' at X=1    : ', DPDX
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Value of the polynomial at X=1:  -0.340878
      !>           First derivative at X=1    :   0.382716
      !>
      !>## Restrictions
      !>
      !>     1)  One needs to be careful that the value
      !>
      !>            (X-X2S(1)) / X2S(2)
      !>
      !>         lies between -1 and 1. Otherwise, the routine may fail
      !>         spectacularly (for example with a floating point overflow).
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence
      !>          Formula," p 161, Cambridge University Press, 1986.
      !>
      !>     [2]  T. Rivlin, "The Chebyshev Polynomials," Wiley, 1974.
      !>
      !>     [3]  R. Weast and S. Selby, "CRC Handbook of Tables for
      !>          Mathematics," 4th Edition, CRC Press, 1976.
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
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
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
      !>-    Beta Version 1.0.1, 30-DEC-1988 (WLT)
      !>
      !>        The Error free specification was added to the routine as
      !>        well as an explanation for this designation. Examples added.
      pure subroutine chbint ( cp, degp, x2s, x, p, dpdx )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: cp  ( * )  !! DEGP+1 Chebyshev polynomial coefficients.
      integer(ip), intent(in) :: degp  !! Degree of polynomial.
      real(wp), intent(in) :: x2s ( 2 )  !! Transformation parameters of polynomial.
      real(wp), intent(in) :: x  !! Value for which the polynomial is to be evaluated
      real(wp), intent(out) :: p  !! Value of the polynomial at X
      real(wp), intent(out) :: dpdx  !! Value of the derivative of the polynomial at X

      integer(ip) :: j
      real(wp) :: w (3)
      real(wp) :: dw(3)
      real(wp) :: s
      real(wp) :: s2

!
!     Transform X to S and initialize temporary variables.
!
      s     = (x-x2s(1)) / x2s(2)
      s2    = 2.0_wp * s
      j     = degp  + 1
      w (1) = 0.0_wp
      w (2) = 0.0_wp
      dw(1) = 0.0_wp
      dw(2) = 0.0_wp

!
!     Evaluate the polynomial and its derivative using recursion.
!
      do while ( j > 1 )

         w (3) = w (2)
         w (2) = w (1)
         w (1) = cp(j)  + ( s2*w(2)  - w(3) )

         dw(3) = dw(2)
         dw(2) = dw(1)
         dw(1) = w(2)*2.0_wp + dw(2)*s2 - dw(3)

         j     = j - 1

      end do

      p    = cp(1) + ( s*w (1) - w (2) )
      dpdx =  w(1) + s*dw(1) - dw(2)

!
!     Scale the derivative by 1/X2S(2) so that we have the derivative
!
!                       d P(S)
!                       ------
!                         dX
!
      dpdx = dpdx / x2s(2)

      return
      end subroutine chbint

      !> Return the value of a polynomial evaluated at the input X using
      !> the coefficients for the Chebyshev expansion of the polynomial.
      !>
      !>## Detailed Input
      !>
      !>     CP       is an array of coefficients a polynomial with respect
      !>              to the Chebyshev basis. The polynomial to be
      !>              evaluated is assumed to be of the form:
      !>
      !>                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>              where T(I,S) is the I'th Chebyshev polynomial
      !>              evaluated at a number S whose double precision
      !>              value lies between -1 and 1. The value of S is
      !>              computed from the input variables X2S(1), X2S(2)
      !>              and X.
      !>
      !>     DEGP     is the degree of the Chebyshev polynomial to be
      !>              evaluated.
      !>
      !>     X2S      is an array of two parameters. These parameters are
      !>              used to transform the domain of the input variable X
      !>              into the standard domain of the Chebyshev polynomial.
      !>              X2S(1) should be a reference point in the domain of X;
      !>              X2S(2) should be the radius by which points are
      !>              allowed to deviate from the reference point and while
      !>              remaining within the domain of X. The value of
      !>              X is transformed into the value S given by
      !>
      !>                 S = ( X - X2S(1) ) / X2S(2)
      !>
      !>              Typically X2S(1) is the midpoint of the interval over
      !>              which X is allowed to vary and X2S(2) is the radius of
      !>              the interval.
      !>
      !>              The main reason for doing this is that a Chebyshev
      !>              expansion is usually fit to data over a span
      !>              from A to B where A and B are not -1 and 1
      !>              respectively. Thus to get the "best fit" the
      !>              data was transformed to the interval [-1,1] and
      !>              coefficients generated. These coefficients are
      !>              not rescaled to the interval of the data so that
      !>              the numerical "robustness" of the Chebyshev fit will
      !>              not be lost. Consequently, when the "best fitting"
      !>              polynomial needs to be evaluated at an intermediate
      !>              point, the point of evaluation must be transformed
      !>              in the same way that the generating points were
      !>              transformed.
      !>
      !>     X        is the value for which the polynomial is to be
      !>              evaluated.
      !>
      !>## Detailed Output
      !>
      !>     P        is the value of the polynomial to be evaluated. It
      !>              is given by
      !>
      !>                 CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                                      + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>              where T(I,S) is the I'th Chebyshev polynomial
      !>              evaluated  at a number S = ( X - X2S(1) )/X2S(2)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No tests are performed for exceptional values (DEGP negative,
      !>         etc.). This routine is expected to be used at a low level in
      !>         ephemeris evaluations. For that reason it has been elected as
      !>         a routine that will not participate in error handling.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the value P given by
      !>
      !>        CP(DEGP+1)*T(DEGP,S) + CP(DEGP)*T(DEGP-1,S) + ...
      !>
      !>                             + CP(2)*T(1,S) + CP(1)*T(0,S)
      !>
      !>     where
      !>
      !>        S  =  ( X - X2S(1) ) / X2S(2)
      !>
      !>     and
      !>
      !>        T(I,S) is the I'th Chebyshev polynomial of the first kind
      !>        evaluated at S.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Depending upon the user's needs, there are 3 routines
      !>        available for evaluating Chebyshev polynomials.
      !>
      !>           CHBVAL   for evaluating a Chebyshev polynomial when no
      !>                    derivatives are desired.
      !>
      !>           CHBINT   for evaluating a Chebyshev polynomial and its
      !>                    first derivative.
      !>
      !>           CHBDER   for evaluating a Chebyshev polynomial and a user
      !>                    or application dependent number of derivatives.
      !>
      !>        Of these 3 the one most commonly employed by SPICE software
      !>        is CHBINT as it is used to interpolate ephemeris state
      !>        vectors; this requires the evaluation of a polynomial
      !>        and its derivative. When no derivatives are desired one
      !>        should use CHBVAL, or when more than one or an unknown
      !>        number of derivatives are desired one should use CHBDER.
      !>
      !>        The code example below illustrates how this routine might
      !>        be used to obtain points for plotting a polynomial.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CHBVAL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CP     (7)
      !>      DOUBLE PRECISION      X
      !>      DOUBLE PRECISION      P
      !>      DOUBLE PRECISION      X2S    (2)
      !>
      !>      INTEGER               DEGP
      !>      INTEGER               I
      !>
      !>C
      !>C     Set the coefficients of the polynomial and its
      !>C     transformation parameters
      !>C
      !>      DATA                  CP     / 1.D0,  3.D0,  0.5D0,
      !>     .                               1.D0,  0.5D0, -1.D0,
      !>     .                               1.D0               /
      !>      DATA                  X2S    / 0.5D0, 3.D0 /
      !>
      !>      DEGP   = 6
      !>      X      = 1.D0
      !>
      !>      CALL CHBVAL ( CP, DEGP, X2S, X, P )
      !>
      !>      WRITE(*,'(A,F10.6)')
      !>     .        'Value of the polynomial at X=1: ', P
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Value of the polynomial at X=1:  -0.340878
      !>
      !>## Restrictions
      !>
      !>     1)  One needs to be careful that the value
      !>
      !>            (X-X2S(1)) / X2S(2)
      !>
      !>         lies between -1 and 1. Otherwise, the routine may fail
      !>         spectacularly (for example with a floating point overflow).
      !>
      !>## Literature References
      !>
      !>     [1]  W. Press, B. Flannery, S. Teukolsky and W. Vetterling,
      !>          "Numerical Recipes -- The Art of Scientific Computing,"
      !>          chapter 5.4, "Recurrence Relations and Clenshaw's Recurrence
      !>          Formula," p 161, Cambridge University Press, 1986.
      !>
      !>     [2]  T. Rivlin, "The Chebyshev Polynomials," Wisley, 1974.
      !>
      !>     [3]  R. Weast and S. Selby, "CRC Handbook of Tables for
      !>          Mathematics," 4th Edition, CRC Press, 1976.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 16-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.0.1, 30-DEC-1988 (WLT)
      !>
      !>        The Error free specification was added to the routine as
      !>        well as an explanation for this designation. Examples added.
      pure subroutine chbval ( cp, degp, x2s, x, p )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: cp  ( * )  !! DEGP+1 Chebyshev polynomial coefficients.
      integer(ip), intent(in) :: degp  !! Degree of polynomial.
      real(wp), intent(in) :: x2s ( 2 )  !! Transformation parameters of polynomial.
      real(wp), intent(in) :: x  !! Value for which the polynomial is to be evaluated.
      real(wp), intent(out) :: p  !! Value of the polynomial at X.

      integer(ip) :: j
      real(wp) :: w(3)
      real(wp) :: s
      real(wp) :: s2

!
!     Transform X to S and initialize temporary variables.
!
      s     = (x-x2s(1)) / x2s(2)
      s2    = 2.0_wp * s
      j     = degp  + 1
      w(1)  = 0.0_wp
      w(2)  = 0.0_wp

!
!     Evaluate the polynomial using recursion.
!
      do while ( j > 1 )
         w(3) = w (2)
         w(2) = w (1)
         w(1) = cp(j) + (s2*w(2) - w(3))
         j    = j - 1
      end do

      p     = (s*w(1)-w(2)) + cp(1)

      return
      end subroutine chbval

      !> Convert from cylindrical to latitudinal coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of the input point from Z axis.
      !>
      !>     CLON     is the cylindrical angle of the point from XZ plane
      !>              (radians).
      !>
      !>     Z        is the height of the point above XY plane.
      !>
      !>## Detailed Output
      !>
      !>     RADIUS   is the distance of the input point from origin.
      !>
      !>     LON      is the longitude (i.e. angle from the XZ plane) of
      !>              the input point (radians). LON is set equal to CLON.
      !>
      !>     LAT      is the latitude (i.e. angle above the XY plane) of the
      !>              input point (radians). The range of LAT is [-pi, pi].
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine converts coordinates given in cylindrical
      !>     coordinates to coordinates in latitudinal coordinates.
      !>
      !>     Latitudinal coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     above the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the cylindrical coordinates of the position of the
      !>        Moon as seen from the Earth, and convert them to latitudinal
      !>        and rectangular coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: cyllat_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CYLLAT_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'cyllat_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to cylindrical
      !>C     coordinates.
      !>C
      !>      CALL RECCYL ( POS, R, CLON, Z )
      !>
      !>C
      !>C     Convert the cylindrical coordinates to latitudinal.
      !>C
      !>      CALL CYLLAT ( R, CLON, Z, RADIUS, LON, LAT )
      !>
      !>C
      !>C     Convert the latitudinal coordinates to rectangular.
      !>C
      !>      CALL LATREC ( RADIUS, LON, LAT, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', R
      !>      WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z          (km): ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR()
      !>      WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from LATREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius     (km):      383289.01777726
      !>          Longitude (deg):         261.65040211
      !>          Z          (km):     -126505.93063865
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius     (km):      403626.33912495
      !>          Longitude (deg):         261.65040211
      !>          Latitude  (deg):         -18.26566077
      !>
      !>         Rectangular coordinates from LATREC:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of cylindrical coordinates
      !>        and the corresponding latitudinal coordinates.
      !>
      !>        Corresponding latitudinal and cylindrical coordinates are
      !>        listed to three decimal places. All input and output angles
      !>        are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CYLLAT_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CLON   ( NREC )
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      R      ( NREC )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RCLON
      !>      DOUBLE PRECISION      Z      ( NREC )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input cylindrical coordinates. Angles
      !>C     in degrees.
      !>C
      !>
      !>      DATA                 R    /  0.D0, 1.D0, 1.D0,
      !>     .                             0.D0, 1.D0, 1.D0,
      !>     .                             0.D0, 1.D0, 1.D0,
      !>     .                             0.D0, 0.D0           /
      !>
      !>      DATA                 CLON /  0.D0,   0.D0,  90.D0,
      !>     .                             0.D0, 180.D0, -90.D0,
      !>     .                             0.D0,  45.D0, 180.D0,
      !>     .                           180.D0,  33.D0         /
      !>
      !>      DATA                 Z    /  0.D0,   0.D0,  0.D0,
      !>     .                             1.D0,   1.D0,  0.D0,
      !>     .                            -1.D0,   0.D0, -1.D0,
      !>     .                             1.D0,   0.D0         /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '    R       CLON      Z    '
      !>     . //        '  RADIUS     LON     LAT   '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RCLON = CLON(I) * RPD()
      !>
      !>         CALL CYLLAT( R(I), RCLON, Z(I), RADIUS, LON, LAT )
      !>
      !>         WRITE (*,'(6F9.3)') R(I), CLON(I), Z(I),
      !>     .                       RADIUS, LON * DPR(), LAT * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>             R       CLON      Z      RADIUS     LON     LAT
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000    1.000    1.000    0.000   90.000
      !>            1.000  180.000    1.000    1.414  180.000   45.000
      !>            1.000  -90.000    0.000    1.000  -90.000    0.000
      !>            0.000    0.000   -1.000    1.000    0.000  -90.000
      !>            1.000   45.000    0.000    1.000   45.000    0.000
      !>            1.000  180.000   -1.000    1.414  180.000  -45.000
      !>            0.000  180.000    1.000    1.000  180.000   90.000
      !>            0.000   33.000    0.000    0.000   33.000    0.000
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the argument names LONGC and LONG to CLON and LON for
      !>        consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental subroutine cyllat ( r, clon, z,  radius, lon, lat )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of point from Z axis.
      real(wp), intent(in) :: clon  !! Cylindrical angle of point from XZ plane(radians).
      real(wp), intent(in) :: z  !! Height of point above XY plane.
      real(wp), intent(out) :: radius  !! Distance of point from origin.
      real(wp), intent(out) :: lon  !! Longitude of point (radians).
      real(wp), intent(out) :: lat  !! Latitude of point (radians).

      real(wp) :: big
      real(wp) :: x
      real(wp) :: y

      real(wp) :: rho
      real(wp) :: lattud

!
!     Convert the input cylindrical coordinates to latitudinal
!     coordinates, storing in temporary variables.
!
      big = max( dabs(r), dabs(z) )

      if ( big > 0 ) then
         x   = r/big
         y   = z/big
         rho = big * dsqrt( x*x + y*y )
      else
         rho = 0.0_wp
      end if

      if (rho==0.0_wp) then
         lattud = 0.0_wp
      else
         lattud = datan2 (z,r)
      end if
!
!     Move results to output variables
!
      lon    = clon
      radius = rho
      lat    = lattud
      return
      end subroutine cyllat

      !> Convert from cylindrical to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of the point of interest from Z-axis.
      !>
      !>     CLON     is the cylindrical angle (in radians) of the point of
      !>              interest from XZ plane.
      !>
      !>     Z        is the height of the point above XY plane.
      !>
      !>## Detailed Output
      !>
      !>     RECTAN   are the rectangular coordinates of the point of
      !>              interest.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine transforms the coordinates of a point from
      !>     cylindrical to rectangular coordinates.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the cylindrical coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to rectangular
      !>        coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: cylrec_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CYLREC_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'cylrec_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to cylindrical
      !>C     coordinates.
      !>C
      !>      CALL RECCYL ( POS, R, CLON, Z )
      !>
      !>C
      !>C     Convert the cylindrical to rectangular coordinates.
      !>C
      !>
      !>      CALL CYLREC ( R, CLON, Z, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', R
      !>      WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z          (km): ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from CYLREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius     (km):      383289.01777726
      !>          Longitude (deg):         261.65040211
      !>          Z          (km):     -126505.93063865
      !>
      !>         Rectangular coordinates from CYLREC:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of cylindrical coordinates
      !>        and the corresponding rectangular coordinates.
      !>
      !>        Corresponding rectangular and cylindrical coordinates are
      !>        listed to three decimal places. Input angles are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CYLREC_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      R      ( NREC )
      !>      DOUBLE PRECISION      CLON   ( NREC )
      !>      DOUBLE PRECISION      Z      ( NREC )
      !>      DOUBLE PRECISION      RCLON
      !>      DOUBLE PRECISION      RECTAN ( 3    )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input cylindrical coordinates. Angles
      !>C     in degrees.
      !>C
      !>      DATA                 R    /  0.D0, 1.D0,     1.D0,
      !>     .                             0.D0, 1.D0,     1.D0,
      !>     .                             0.D0, 1.4142D0, 1.D0,
      !>     .                             1.D0, 1.4142D0       /
      !>
      !>      DATA                 CLON /  0.D0,   0.D0,  90.D0,
      !>     .                             0.D0, 180.D0, 270.D0,
      !>     .                             0.D0,  45.D0,   0.D0,
      !>     .                            90.D0,  45.D0         /
      !>
      !>      DATA                 Z    /  0.D0,   0.D0,   0.D0,
      !>     .                             1.D0,   0.D0,   0.D0,
      !>     .                            -1.D0,   0.D0,   1.D0,
      !>     .                             1.D0,   1.D0         /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '    R       CLON      Z    '
      !>     . //        ' RECT(1)  RECT(2)  RECT(3) '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RCLON = CLON(I) * RPD()
      !>
      !>         CALL CYLREC( R(I), RCLON, Z(I), RECTAN )
      !>
      !>         WRITE (*,'(6F9.3)') R(I), CLON(I), Z(I),
      !>     .                       RECTAN
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>             R       CLON      Z     RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000    0.000    0.000    1.000    0.000
      !>            0.000    0.000    1.000    0.000    0.000    1.000
      !>            1.000  180.000    0.000   -1.000    0.000    0.000
      !>            1.000  270.000    0.000   -0.000   -1.000    0.000
      !>            0.000    0.000   -1.000    0.000    0.000   -1.000
      !>            1.414   45.000    0.000    1.000    1.000    0.000
      !>            1.000    0.000    1.000    1.000    0.000    1.000
      !>            1.000   90.000    1.000    0.000    1.000    1.000
      !>            1.414   45.000    1.000    1.000    1.000    1.000
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the input argument name LONG to CLON for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine cylrec ( r, clon, z,  rectan )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of a point from Z axis.
      real(wp), intent(in) :: clon  !! Angle (radians) of a point from XZ plane
      real(wp), intent(in) :: z  !! Height of a point above XY plane.
      real(wp), intent(out) :: rectan ( 3 )  !! Rectangular coordinates of the point.

      real(wp) :: x
      real(wp) :: y

!
!     Convert to rectangular coordinates, storing the results in
!     temporary variables.
!
      x         = r * dcos(clon)
      y         = r * dsin(clon)

!
!     Move the results to the output variables.
!
      rectan(1) = x
      rectan(2) = y
      rectan(3) = z

      return
      end subroutine cylrec

      !> Convert from cylindrical to spherical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of the point of interest from Z axis.
      !>
      !>     CLON     is the cylindrical angle (radians) of the point from
      !>              the XZ plane.
      !>
      !>     Z        is the height of the point above XY plane.
      !>
      !>## Detailed Output
      !>
      !>     RADIUS   is the distance of the point from origin.
      !>
      !>     COLAT    is the polar angle (co-latitude in radians) of the
      !>              point. The range of COLAT is [-pi, pi].
      !>
      !>     SLON     is the azimuthal angle (longitude) of the point
      !>              (radians). SLON is set equal to CLON.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This returns the spherical coordinates of a point whose position
      !>     is input through cylindrical coordinates.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the cylindrical coordinates of the position of the
      !>        Moon as seen from the Earth, and convert them to spherical
      !>        and rectangular coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: cylsph_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CYLSPH_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'cylsph_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to cylindrical
      !>C     coordinates.
      !>C
      !>      CALL RECCYL ( POS, R, CLON, Z )
      !>
      !>C
      !>C     Convert the cylindrical coordinates to spherical.
      !>C
      !>      CALL CYLSPH ( R, CLON, Z, RADIUS, COLAT, SLON )
      !>
      !>C
      !>C     Convert the spherical coordinates to rectangular.
      !>C
      !>      CALL SPHREC ( RADIUS, COLAT, SLON, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', R
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z           (km): ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from SPHREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius      (km):      383289.01777726
      !>          Longitude  (deg):         261.65040211
      !>          Z           (km):     -126505.93063865
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Colatitude (deg):         108.26566077
      !>          Longitude  (deg):         261.65040211
      !>
      !>         Rectangular coordinates from SPHREC:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of cylindrical coordinates
      !>        and the corresponding spherical coordinates.
      !>
      !>        Corresponding spherical and cylindrical coordinates are
      !>        listed to three decimal places. All input and output angles
      !>        are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CYLSPH_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CLON   ( NREC )
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      R      ( NREC )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RCLON
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z      ( NREC )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input cylindrical coordinates. Angles
      !>C     in degrees.
      !>C
      !>
      !>      DATA                 R    /  0.D0, 1.D0, 1.D0,
      !>     .                             0.D0, 1.D0, 1.D0,
      !>     .                             0.D0, 1.D0, 1.D0,
      !>     .                             0.D0, 0.D0           /
      !>
      !>      DATA                 CLON /  0.D0,   0.D0,  90.D0,
      !>     .                             0.D0, 180.D0, -90.D0,
      !>     .                             0.D0,  45.D0, 180.D0,
      !>     .                           180.D0,  33.D0         /
      !>
      !>      DATA                 Z    /  0.D0,   0.D0,  0.D0,
      !>     .                             1.D0,   1.D0,  0.D0,
      !>     .                            -1.D0,   0.D0, -1.D0,
      !>     .                             1.D0,   0.D0         /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '    R       CLON      Z    '
      !>     . //        '  RADIUS   COLAT     SLON  '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RCLON = CLON(I) * RPD()
      !>
      !>         CALL CYLSPH( R(I), RCLON, Z(I), RADIUS, COLAT, SLON )
      !>
      !>         WRITE (*,'(6F9.3)') R(I), CLON(I), Z(I), RADIUS,
      !>     .                       COLAT * DPR(), SLON  * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>             R       CLON      Z      RADIUS   COLAT     SLON
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000   90.000    0.000
      !>            1.000   90.000    0.000    1.000   90.000   90.000
      !>            0.000    0.000    1.000    1.000    0.000    0.000
      !>            1.000  180.000    1.000    1.414   45.000  180.000
      !>            1.000  -90.000    0.000    1.000   90.000  -90.000
      !>            0.000    0.000   -1.000    1.000  180.000    0.000
      !>            1.000   45.000    0.000    1.000   90.000   45.000
      !>            1.000  180.000   -1.000    1.414  135.000  180.000
      !>            0.000  180.000    1.000    1.000    0.000  180.000
      !>            0.000   33.000    0.000    0.000    0.000   33.000
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the argument names LONGC and LONG to CLON and SLON for
      !>        consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.1.1, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.1.0, 30-MAR-2016 (BVS)
      !>
      !>        A cosmetic change: replaced '0.0 D0's with '0.0D0's.
      !>        Re-arranged header sections.
      !>
      !>-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF. Obsolete $Revisions section
      !>        deleted.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental subroutine cylsph ( r, clon, z,  radius, colat, slon )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of point from Z axis.
      real(wp), intent(in) :: clon  !! Angle (radians) of point from XZ plane.
      real(wp), intent(in) :: z  !! Height of point above XY plane.
      real(wp), intent(out) :: radius  !! Distance of point from origin.
      real(wp), intent(out) :: colat  !! Polar angle (co-latitude in radians) of point.
      real(wp), intent(out) :: slon  !! Azimuthal angle (longitude) of point (radians).

      real(wp) :: big
      real(wp) :: x
      real(wp) :: y

      real(wp) :: rh
      real(wp) :: th

!
!     Convert to spherical, storing in temporary variables
!
      big = max( dabs(r), dabs(z) )

      if ( big == 0.0_wp ) then

         th = 0.0_wp
         rh = 0.0_wp

      else

         x  = r/big
         y  = z/big

         rh = big * dsqrt( x*x + y*y )
         th = datan2 (r,z)

      end if

!
!     Move the results to output variables
!
      slon   = clon
      radius = rh
      colat  = th

      return
      end subroutine cylsph

      !> Return the cube root of a double precision number.
      !>
      !> The function returns the cube root of the input value.
      !>
      !>## Detailed Input
      !>
      !>     X        is a double precision value.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the cube root of the input value.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DCBRT calculates the cube root of the input value by using the
      !>     exponentiation operator to raise the input value to the 1/3
      !>     power. This operation, however, is performed on the absolute
      !>     value of the input variable, and then the sign of the input
      !>     is transferred to the output value.
      !>
      !>     All values of the input variable X should be acceptable to the
      !>     DCBRT.
      !>
      !>## Examples
      !>
      !>     The following table gives sample values of the variable X and
      !>     DCBRT(X)
      !>
      !>      X                        DCBRT(X)
      !>     --------------------------------------------------------------
      !>      0.0D0                    0.0D0
      !>      8.0D0                    2.0D0
      !>     -1.0D3                   -1.0D1
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
      !>     W.M. Owen          (JPL)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      elemental real(wp) function  dcbrt ( x )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: x  !! Number whose cube root is desired.

      real(wp), parameter :: third = 1.0_wp/3.0_wp
      dcbrt = dsign (dabs(x)**third, x)
      return
      end function dcbrt

      !> Compute the determinant of a double precision 3x3 matrix.
      !>
      !> The function returns the value of the determinant found by direct
      !> application of the definition of the determinant.
      !>
      !>## Detailed Input
      !>
      !>     M1       is any double precision, 3x3 matrix.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value of the determinant found by direct
      !>     application of the definition of the determinant.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DET calculates the determinant of M1 in a single arithmetic
      !>     expression which is, effectively, the expansion of M1 about its
      !>     first row. Since the calculation of the determinant involves
      !>     the multiplication of numbers whose magnitudes are unrestricted,
      !>     there is the possibility of floating point overflow or underflow.
      !>     NO error checking or recovery is implemented in this routine.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x3 double precision matrix, compute its determinant.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DET_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      DET
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      M1     ( 3, 3 )
      !>      DOUBLE PRECISION      M2     ( 3, 3 )
      !>
      !>C
      !>C     Set M1 and M2.
      !>C
      !>      DATA                  M1 /  1.D0,  2.D0,  3.D0,
      !>     .                            4.D0,  5.D0,  6.D0,
      !>     .                            7.D0,  8.D0,  9.D0  /
      !>
      !>      DATA                  M2 /  1.D0,  2.D0,  3.D0,
      !>     .                            0.D0,  5.D0,  6.D0,
      !>     .                            0.D0,  0.D0,  9.D0  /
      !>
      !>C
      !>C     Display the determinant of M1 and M2.
      !>C
      !>      WRITE(*,'(A,F6.2)') 'Determinant of M1:', DET(M1)
      !>      WRITE(*,'(A,F6.2)') 'Determinant of M2:', DET(M2)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Determinant of M1:  0.00
      !>        Determinant of M2: 45.00
      !>
      !>## Restrictions
      !>
      !>     1)  No checking is implemented to determine whether M1 will cause
      !>         overflow or underflow in the process of calculating the
      !>         determinant. In most cases, this will not pose a problem.
      !>         The user is required to determine if M1 is suitable matrix
      !>         for DET to operate on.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 02-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing fragment.
      !>
      !>        Added missing IMPLICIT NONE statement.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function  det ( m1 )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: m1 ( 3,3 )  !! Matrix whose determinant is to be found.

      det =   m1(1,1) * ( m1(2,2)*m1(3,3) - m1(2,3)*m1(3,2) ) &
            - m1(1,2) * ( m1(2,1)*m1(3,3) - m1(2,3)*m1(3,1) ) &
            + m1(1,3) * ( m1(2,1)*m1(3,2) - m1(2,2)*m1(3,1) )

      return
      end function det

      !> Compute the Jacobian matrix of the transformation from
      !> cylindrical to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of the point of interest from Z-axis.
      !>
      !>     CLON     is the cylindrical angle (in radians) of the point of
      !>              interest from the XZ plane. The angle increases in
      !>              the counterclockwise sense about the +Z axis.
      !>
      !>     Z        is the height of the point above XY plane.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between cylindrical and rectangular coordinates. It
      !>              has the form
      !>
      !>                 .-                                  -.
      !>                 |  DX/DR     DX/DCLON       DX/DZ    |
      !>                 |                                    |
      !>                 |  DY/DR     DY/DCLON       DY/DZ    |
      !>                 |                                    |
      !>                 |  DZ/DR     DZ/DCLON       DZ/DZ    |
      !>                 `-                                  -'
      !>
      !>              evaluated at the input values of R, CLON and Z.
      !>              Here X, Y, and Z are given by the familiar formulae
      !>
      !>                 X = R*COS(CLON)
      !>                 Y = R*SIN(CLON)
      !>                 Z = Z
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is often convenient to describe the motion of an object in
      !>     the cylindrical coordinate system. However, when performing
      !>     vector computations its hard to beat rectangular coordinates.
      !>
      !>     To transform states given with respect to cylindrical coordinates
      !>     to states with respect to rectangular coordinates, one uses
      !>     the Jacobian of the transformation between the two systems.
      !>
      !>     Given a state in cylindrical coordinates
      !>
      !>        ( r, clon, z, dr, dclon, dz )
      !>
      !>     the velocity in rectangular coordinates is given by the matrix
      !>     equation:
      !>                    t          |                           t
      !>        (dx, dy, dz)   = JACOBI|          * (dr, dclon, dz)
      !>                               |(r,clon,z)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(r,clon,z)
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
      !>           File name: drdcyl_ex1.tm
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
      !>      PROGRAM DRDCYL_EX1
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
      !>      CALL FURNSH ( 'drdcyl_ex1.tm' )
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
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>        Changed the input argument name LONG to CLON for consistency
      !>        with other routines.
      !>
      !>-    SPICELIB Version 1.0.1, 12-NOV-2013 (EDW)
      !>
      !>        Trivial edit to header, deleted trailing whitespace
      !>        on lines.
      !>
      !>-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT) (IMU)
      pure subroutine drdcyl ( r, clon, z, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of a point from the origin.
      real(wp), intent(in) :: clon  !! Angle of the point from the XZ plane in radians.
      real(wp), intent(in) :: z  !! Height of the point above the XY plane.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.






      jacobi (dx,dr)     =  dcos( clon )
      jacobi (dy,dr)     =  dsin( clon )
      jacobi (dz,dr)     =  0.0_wp

      jacobi (dx,dlon)   = -dsin( clon ) * r
      jacobi (dy,dlon)   =  dcos( clon ) * r
      jacobi (dz,dlon)   =  0.0_wp

      jacobi (dx,dz)     =  0.0_wp
      jacobi (dy,dz)     =  0.0_wp
      jacobi (dz,dz)     =  1.0_wp

      return
      end subroutine drdcyl

      !> Compute the Jacobian matrix of the transformation from
      !> latitudinal to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of a point from the origin.
      !>
      !>     LON      is the angle of the point from the XZ plane in
      !>              radians. The angle increases in the counterclockwise
      !>              sense about the +Z axis.
      !>
      !>     LAT      is the angle of the point from the XY plane in
      !>              radians. The angle increases in the direction of the
      !>              +Z axis.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between latitudinal and rectangular coordinates. It has
      !>              the form
      !>
      !>                  .-                               -.
      !>                  |  DX/DR     DX/DLON     DX/DLAT  |
      !>                  |                                 |
      !>                  |  DY/DR     DY/DLON     DY/DLAT  |
      !>                  |                                 |
      !>                  |  DZ/DR     DZ/DLON     DZ/DLAT  |
      !>                  `-                               -'
      !>
      !>              evaluated at the input values of R, LON and LAT.
      !>              Here X, Y, and Z are given by the familiar formulae
      !>
      !>                 X = R * COS(LON) * COS(LAT)
      !>                 Y = R * SIN(LON) * COS(LAT)
      !>                 Z = R *            SIN(LAT)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is often convenient to describe the motion of an object
      !>     in latitudinal coordinates. It is also convenient to manipulate
      !>     vectors associated with the object in rectangular coordinates.
      !>
      !>     The transformation of a latitudinal state into an equivalent
      !>     rectangular state makes use of the Jacobian of the
      !>     transformation between the two systems.
      !>
      !>     Given a state in latitudinal coordinates,
      !>
      !>          ( r, lon, lat, dr, dlon, dlat )
      !>
      !>     the velocity in rectangular coordinates is given by the matrix
      !>     equation
      !>                    t          |                               t
      !>        (dx, dy, dz)   = JACOBI|             * (dr, dlon, dlat)
      !>                               |(r,lon,lat)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(r,lon,lat)
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
      !>           File name: drdlat_ex1.tm
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
      !>      PROGRAM DRDLAT_EX1
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
      !>      CALL FURNSH ( 'drdlat_ex1.tm' )
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2020 (JDR)
      !>
      !>        Changed the input argument name LONG to LON for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Updated $Brief_I/O and $Detailed_Input sections to correct R
      !>        argument name, which in previous version was RADIUS.
      !>
      !>-    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT)
      pure subroutine drdlat ( r, lon, lat, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of a point from the origin.
      real(wp), intent(in) :: lon  !! Angle of the point from the XZ plane in radians.
      real(wp), intent(in) :: lat  !! Angle of the point from the XY plane in radians.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.






      integer(ip), parameter :: dlat = 3

!
!     Construct the matrix directly.
!
      jacobi (dx,dr)   =      dcos( lon ) * dcos( lat )
      jacobi (dy,dr)   =      dsin( lon ) * dcos( lat )
      jacobi (dz,dr)   =                    dsin( lat )

      jacobi (dx,dlon) = -r * dsin( lon ) * dcos( lat )
      jacobi (dy,dlon) =  r * dcos( lon ) * dcos( lat )
      jacobi (dz,dlon) =  0.0_wp

      jacobi (dx,dlat) = -r * dcos( lon ) * dsin( lat )
      jacobi (dy,dlat) = -r * dsin( lon ) * dsin( lat )
      jacobi (dz,dlat) =  r *               dcos( lat )

      return
      end subroutine drdlat

      !> Compute the Jacobian matrix of the transformation from spherical
      !> to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of a point from the origin.
      !>
      !>     COLAT    is the angle between the point and the positive
      !>              Z-axis, in radians.
      !>
      !>     SLON     is the angle of the point from the XZ plane in
      !>              radians. The angle increases in the counterclockwise
      !>              sense about the +Z axis.
      !>
      !>## Detailed Output
      !>
      !>     JACOBI   is the matrix of partial derivatives of the conversion
      !>              between spherical and rectangular coordinates,
      !>              evaluated at the input coordinates. This matrix has
      !>              the form
      !>
      !>                  .-                                   -.
      !>                  |  DX/DR     DX/DCOLAT     DX/DSLON   |
      !>                  |                                     |
      !>                  |  DY/DR     DY/DCOLAT     DY/DSLON   |
      !>                  |                                     |
      !>                  |  DZ/DR     DZ/DCOLAT     DZ/DSLON   |
      !>                  `-                                   -'
      !>
      !>               evaluated at the input values of R, SLON and LAT.
      !>               Here X, Y, and Z are given by the familiar formulae
      !>
      !>                 X = R*COS(SLON)*SIN(COLAT)
      !>                 Y = R*SIN(SLON)*SIN(COLAT)
      !>                 Z = R*COS(COLAT)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is often convenient to describe the motion of an object in
      !>     the spherical coordinate system. However, when performing
      !>     vector computations its hard to beat rectangular coordinates.
      !>
      !>     To transform states given with respect to spherical coordinates
      !>     to states with respect to rectangular coordinates, one makes use
      !>     of the Jacobian of the transformation between the two systems.
      !>
      !>     Given a state in spherical coordinates
      !>
      !>          ( r, colat, slon, dr, dcolat, dslon )
      !>
      !>     the velocity in rectangular coordinates is given by the matrix
      !>     equation:
      !>                    t          |                                   t
      !>        (dx, dy, dz)   = JACOBI|              * (dr, dcolat, dslon)
      !>                               |(r,colat,slon)
      !>
      !>     This routine computes the matrix
      !>
      !>              |
      !>        JACOBI|
      !>              |(r,colat,slon)
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
      !>           File name: drdsph_ex1.tm
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
      !>      PROGRAM DRDSPH_EX1
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
      !>      CALL FURNSH ( 'drdsph_ex1.tm' )
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>        Changed the argument name LONG to SLON for consistency with
      !>        other routines.
      !>
      !>-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT) (IMU)
      pure subroutine drdsph ( r, colat, slon, jacobi )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of a point from the origin.
      real(wp), intent(in) :: colat  !! Angle of the point from the positive Z-axis.
      real(wp), intent(in) :: slon  !! Angle of the point from the XY plane.
      real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.





      integer(ip), parameter :: dcolat = 2

      integer(ip), parameter :: dlon = 3  !! column of the partial derivatives with respect to longitude

      real(wp) :: ccolat
      real(wp) :: clong
      real(wp) :: scolat
      real(wp) :: slong

      ccolat = dcos( colat )
      scolat = dsin( colat )

      clong  = dcos( slon  )
      slong  = dsin( slon  )

      jacobi (dx,dr)     =      clong * scolat
      jacobi (dy,dr)     =      slong * scolat
      jacobi (dz,dr)     =              ccolat

      jacobi (dx,dcolat) =  r * clong * ccolat
      jacobi (dy,dcolat) =  r * slong * ccolat
      jacobi (dz,dcolat) = -r *         scolat

      jacobi (dx,dlon)   = -r * slong * scolat
      jacobi (dy,dlon)   =  r * clong * scolat
      jacobi (dz,dlon)   =  0.0_wp

      return
      end subroutine drdsph

      !> Compute the unit vector parallel to the cross product of
      !> two 3-dimensional vectors and the derivative of this unit vector.
      !>
      !>## Detailed Input
      !>
      !>     S1       is any state vector. Typically, this might represent the
      !>              apparent state of a planet or the Sun, which defines the
      !>              orientation of axes of some coordinate system.
      !>
      !>     S2       is any state vector.
      !>
      !>## Detailed Output
      !>
      !>     SOUT     is the unit vector parallel to the cross product of the
      !>              position components of S1 and S2 and the derivative of
      !>              the unit vector.
      !>
      !>              If the cross product of the position components is
      !>              the zero vector, then the position component of the
      !>              output will be the zero vector. The velocity component
      !>              of the output will simply be the derivative of the
      !>              cross product of the position components of S1 and S2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the position components of S1 and S2 cross together to
      !>         give a zero vector, the position component of the output
      !>         will be the zero vector. The velocity component of the
      !>         output will simply be the derivative of the cross product
      !>         of the position vectors.
      !>
      !>     2)  If S1 and S2 are large in magnitude (taken together,
      !>         their magnitude surpasses the limit allowed by the
      !>         computer) then it may be possible to generate a
      !>         floating point overflow from an intermediate
      !>         computation even though the actual cross product and
      !>         derivative may be well within the range of double
      !>         precision numbers.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DUCRSS calculates the unit vector parallel to the cross product
      !>     of two vectors and the derivative of that unit vector.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) One can construct non-inertial coordinate frames from apparent
      !>        positions of objects or defined directions. However, if one
      !>        wants to convert states in this non-inertial frame to states
      !>        in an inertial reference frame, the derivatives of the axes of
      !>        the non-inertial frame are required.
      !>
      !>        Define a reference frame with the apparent direction of the
      !>        Sun as seen from Earth as the primary axis X. Use the Earth
      !>        pole vector to define with the primary axis the XY plane of
      !>        the frame, with the primary axis Y pointing in the direction
      !>        of the pole.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ducrss_ex1.tm
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
      !>              pck00008.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00008.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DUCRSS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      STATE  ( 6    )
      !>      DOUBLE PRECISION      TRANS  ( 6, 6 )
      !>      DOUBLE PRECISION      X_NEW  ( 6    )
      !>      DOUBLE PRECISION      Y_NEW  ( 6    )
      !>      DOUBLE PRECISION      Z      ( 6    )
      !>      DOUBLE PRECISION      Z_NEW  ( 6    )
      !>      DOUBLE PRECISION      ZINERT ( 6    )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the earth body-fixed pole vector (Z). The pole
      !>C     has no velocity in the Earth fixed frame IAU_EARTH.
      !>C
      !>      DATA                  Z  / 0.D0, 0.D0, 1.D0,
      !>     .                           0.D0, 0.D0, 0.D0  /
      !>
      !>C
      !>C     Load SPK, PCK, and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'ducrss_ex1.tm' )
      !>
      !>C
      !>C     Calculate the state transformation between IAU_EARTH and
      !>C     J2000 at an arbitrary epoch.
      !>C
      !>      CALL STR2ET ( 'Jan 1, 2009', ET )
      !>      CALL SXFORM ( 'IAU_EARTH', 'J2000', ET, TRANS )
      !>
      !>C
      !>C     Transform the earth pole vector from the IAU_EARTH frame
      !>C     to J2000.
      !>C
      !>      CALL MXVG ( TRANS, Z, 6, 6, ZINERT )
      !>
      !>C
      !>C     Calculate the apparent state of the Sun from Earth at
      !>C     the epoch ET in the J2000 frame.
      !>C
      !>      CALL SPKEZR ( 'Sun',   ET,   'J2000', 'LT+S',
      !>     .              'Earth', STATE, LT              )
      !>
      !>C
      !>C     Define the z axis of the new frame as the cross product
      !>C     between the apparent direction of the Sun and the Earth
      !>C     pole. Z_NEW cross X_NEW defines the Y axis of the
      !>C     derived frame.
      !>C
      !>      CALL DVHAT  ( STATE, X_NEW         )
      !>      CALL DUCRSS ( STATE, ZINERT, Z_NEW )
      !>      CALL DUCRSS ( Z_NEW, STATE,  Y_NEW )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A)') 'New X-axis:'
      !>      WRITE(*,'(A,3F16.12)') '   position:', (X_NEW(I), I=1,3)
      !>      WRITE(*,'(A,3F16.12)') '   velocity:', (X_NEW(I), I=4,6)
      !>      WRITE(*,'(A)') 'New Y-axis:'
      !>      WRITE(*,'(A,3F16.12)') '   position:', (Y_NEW(I), I=1,3)
      !>      WRITE(*,'(A,3F16.12)') '   velocity:', (Y_NEW(I), I=4,6)
      !>      WRITE(*,'(A)') 'New Z-axis:'
      !>      WRITE(*,'(A,3F16.12)') '   position:', (Z_NEW(I), I=1,3)
      !>      WRITE(*,'(A,3F16.12)') '   velocity:', (Z_NEW(I), I=4,6)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        New X-axis:
      !>           position:  0.183446637633 -0.901919663328 -0.391009273602
      !>           velocity:  0.000000202450  0.000000034660  0.000000015033
      !>        New Y-axis:
      !>           position:  0.078846540163 -0.382978080242  0.920386339077
      !>           velocity:  0.000000082384  0.000000032309  0.000000006387
      !>        New Z-axis:
      !>           position: -0.979862518033 -0.199671507623  0.000857203851
      !>           velocity:  0.000000044531 -0.000000218531 -0.000000000036
      !>
      !>        Note that these vectors define the transformation between the
      !>        new frame and J2000 at the given ET:
      !>
      !>               .-            -.
      !>               |       :      |
      !>               |   R   :  0   |
      !>           M = | ......:......|
      !>               |       :      |
      !>               | dRdt  :  R   |
      !>               |       :      |
      !>               `-            -'
      !>
      !>        with
      !>
      !>           DATA         R     / X_NEW(1:3), Y_NEW(1:3), Z_NEW(1:3)  /
      !>
      !>           DATA         dRdt  / X_NEW(4:6), Y_NEW(4:6), Z_NEW(4:6)  /
      !>
      !>## Restrictions
      !>
      !>     1)  No checking of S1 or S2 is done to prevent floating point
      !>         overflow. The user is required to determine that the magnitude
      !>         of each component of the states is within an appropriate range
      !>         so as not to cause floating point overflow. In almost every
      !>         case there will be no problem and no checking actually needs
      !>         to be done.
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
      !>-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.2.0, 08-APR-2014 (NJB)
      !>
      !>        Now scales inputs to reduce chance of numeric
      !>        overflow.
      !>
      !>-    SPICELIB Version 1.1.1, 22-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in DVHAT call.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
      pure subroutine ducrss ( s1, s2, sout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: s1   ( 6 )  !! Left hand state for cross product and derivative.
      real(wp), intent(in) :: s2   ( 6 )  !! Right hand state for cross product and derivative.
      real(wp), intent(out) :: sout ( 6 )  !! Unit vector and derivative of the cross product.

      real(wp) :: f1
      real(wp) :: f2
      real(wp) :: scls1  ( 6 )
      real(wp) :: scls2  ( 6 )
      real(wp) :: tmpsta ( 6 )

!
!     Scale the components of the input states so the states have the
!     same direction and angular rates, but their largest position
!     components have absolute value equal to 1. Do not modify states
!     that have all position components equal to zero.
!
      f1 = max( abs(s1(1)), abs(s1(2)), abs(s1(3)) )
      f2 = max( abs(s2(1)), abs(s2(2)), abs(s2(3)) )

      if ( f1 > 0.0_wp ) then

         scls1 = (1.0_wp/f1)*s1
      else
         scls1 = s1
      end if

      if ( f2 > 0.0_wp ) then

         scls2 = (1.0_wp/f2)*s2
      else
         scls2 = s2
      end if

!
!     Not much to this.  Just get the cross product and its derivative.
!     Using that, get the associated unit vector and its derivative.
!
      call dvcrss ( scls1,  scls2, tmpsta )
      call dvhat  ( tmpsta,        sout   )

      return
      end subroutine ducrss

      !> Compute the cross product of two 3-dimensional vectors
      !> and the derivative of this cross product.
      !>
      !>## Detailed Input
      !>
      !>     S1       is any state vector. Typically, this might represent the
      !>              apparent state of a planet or the Sun, which defines the
      !>              orientation of axes of some coordinate system.
      !>
      !>     S2       is any state vector.
      !>
      !>## Detailed Output
      !>
      !>     SOUT     is the state associated with the cross product of the
      !>              position components of S1 and S2. In other words, if
      !>              S1 = (P1,V1) and S2 = (P2,V2) then SOUT is
      !>              ( P1xP2, d/dt( P1xP2 ) ).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If S1 and S2 are large in magnitude (taken together,
      !>         their magnitude surpasses the limit allowed by the
      !>         computer) then it may be possible to generate a
      !>         floating point overflow from an intermediate
      !>         computation even though the actual cross product and
      !>         derivative may be well within the range of double
      !>         precision numbers.
      !>
      !>         DVCRSS does NOT check the magnitude of S1 or S2 to
      !>         insure that overflow will not occur.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DVCRSS calculates the three-dimensional cross product of two
      !>     vectors and the derivative of that cross product according to
      !>     the definition.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the cross product of two 3-dimensional vectors
      !>        and the derivative of this cross product.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DVCRSS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      S1     ( 6, 2 )
      !>      DOUBLE PRECISION      S2     ( 6, 2 )
      !>      DOUBLE PRECISION      SOUT   ( 6    )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Set S1 and S2 vectors.
      !>C
      !>      DATA                  S1 /
      !>     .                   0.D0, 1.D0, 0.D0, 1.D0, 0.D0, 0.D0,
      !>     .                   5.D0, 5.D0, 5.D0, 1.D0, 0.D0, 0.D0  /
      !>      DATA                  S2 /
      !>     .                 1.D0,  0.D0,  0.D0, 1.D0, 0.D0, 0.D0,
      !>     .                -1.D0, -1.D0, -1.D0, 2.D0, 0.D0, 0.D0  /
      !>
      !>C
      !>C     For each vector S1 and S2, compute their cross product
      !>C     and its derivative.
      !>C
      !>      DO I = 1, 2
      !>
      !>         CALL DVCRSS ( S1(1,I), S2(1,I), SOUT)
      !>
      !>         WRITE(*,'(A,6F7.1)') 'S1  :', ( S1(J,I), J=1,6 )
      !>         WRITE(*,'(A,6F7.1)') 'S2  :', ( S2(J,I), J=1,6 )
      !>         WRITE(*,'(A,6F7.1)') 'SOUT:', ( SOUT(J), J=1,6 )
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        S1  :    0.0    1.0    0.0    1.0    0.0    0.0
      !>        S2  :    1.0    0.0    0.0    1.0    0.0    0.0
      !>        SOUT:    0.0    0.0   -1.0    0.0    0.0   -1.0
      !>
      !>        S1  :    5.0    5.0    5.0    1.0    0.0    0.0
      !>        S2  :   -1.0   -1.0   -1.0    2.0    0.0    0.0
      !>        SOUT:    0.0    0.0    0.0    0.0   11.0  -11.0
      !>
      !>     2) One can construct non-inertial coordinate frames from apparent
      !>        positions of objects or defined directions. However, if one
      !>        wants to convert states in this non-inertial frame to states
      !>        in an inertial reference frame, the derivatives of the axes of
      !>        the non-inertial frame are required.
      !>
      !>        Define a reference frame with the apparent direction of the
      !>        Sun as seen from Earth as the primary axis X. Use the Earth
      !>        pole vector to define with the primary axis the XY plane of
      !>        the frame, with the primary axis Y pointing in the direction
      !>        of the pole.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: dvcrss_ex2.tm
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
      !>              pck00008.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00008.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DVCRSS_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      STATE  ( 6    )
      !>      DOUBLE PRECISION      TMPSTA ( 6    )
      !>      DOUBLE PRECISION      TRANS  ( 6, 6 )
      !>      DOUBLE PRECISION      X_NEW  ( 6    )
      !>      DOUBLE PRECISION      Y_NEW  ( 6    )
      !>      DOUBLE PRECISION      Z      ( 6    )
      !>      DOUBLE PRECISION      Z_NEW  ( 6    )
      !>      DOUBLE PRECISION      ZINERT ( 6    )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the earth body-fixed pole vector (Z). The pole
      !>C     has no velocity in the Earth fixed frame IAU_EARTH.
      !>C
      !>      DATA                  Z  / 0.D0, 0.D0, 1.D0,
      !>     .                           0.D0, 0.D0, 0.D0  /
      !>
      !>C
      !>C     Load SPK, PCK, and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'dvcrss_ex2.tm' )
      !>
      !>C
      !>C     Calculate the state transformation between IAU_EARTH and
      !>C     J2000 at an arbitrary epoch.
      !>C
      !>      CALL STR2ET ( 'Jan 1, 2009', ET )
      !>      CALL SXFORM ( 'IAU_EARTH', 'J2000', ET, TRANS )
      !>
      !>C
      !>C     Transform the earth pole vector from the IAU_EARTH frame
      !>C     to J2000.
      !>C
      !>      CALL MXVG ( TRANS, Z, 6, 6, ZINERT )
      !>
      !>C
      !>C     Calculate the apparent state of the Sun from Earth at
      !>C     the epoch ET in the J2000 frame.
      !>C
      !>      CALL SPKEZR ( 'Sun',   ET,   'J2000', 'LT+S',
      !>     .              'Earth', STATE, LT              )
      !>
      !>C
      !>C     Define the X axis of the new frame to aligned with
      !>C     the computed state. Calculate the state's unit vector
      !>C     and its derivative to get the X axis and its
      !>C     derivative.
      !>C
      !>      CALL DVHAT  ( STATE, X_NEW         )
      !>
      !>C
      !>C     Define the Z axis of the new frame as the cross product
      !>C     between the computed state and the Earth pole.
      !>C     Calculate the Z direction in the new reference frame,
      !>C     then calculate the this direction's unit vector and its
      !>C     derivative to get the Z axis and its derivative.
      !>C
      !>      CALL DVCRSS ( STATE,  ZINERT, TMPSTA )
      !>      CALL DVHAT  ( TMPSTA, Z_NEW          )
      !>
      !>C
      !>C     As for Z_NEW, calculate the Y direction in the new
      !>C     reference frame, then calculate this direction's unit
      !>C     vector and its derivative to get the Y axis and its
      !>C     derivative.
      !>C
      !>      CALL DUCRSS ( Z_NEW,  STATE, TMPSTA )
      !>      CALL DVHAT  ( TMPSTA, Y_NEW         )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A)') 'New X-axis:'
      !>      WRITE(*,'(A,3F16.12)') '   position:', (X_NEW(I), I=1,3)
      !>      WRITE(*,'(A,3F16.12)') '   velocity:', (X_NEW(I), I=4,6)
      !>      WRITE(*,'(A)') 'New Y-axis:'
      !>      WRITE(*,'(A,3F16.12)') '   position:', (Y_NEW(I), I=1,3)
      !>      WRITE(*,'(A,3F16.12)') '   velocity:', (Y_NEW(I), I=4,6)
      !>      WRITE(*,'(A)') 'New Z-axis:'
      !>      WRITE(*,'(A,3F16.12)') '   position:', (Z_NEW(I), I=1,3)
      !>      WRITE(*,'(A,3F16.12)') '   velocity:', (Z_NEW(I), I=4,6)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        New X-axis:
      !>           position:  0.183446637633 -0.901919663328 -0.391009273602
      !>           velocity:  0.000000202450  0.000000034660  0.000000015033
      !>        New Y-axis:
      !>           position:  0.078846540163 -0.382978080242  0.920386339077
      !>           velocity:  0.000000082384  0.000000032309  0.000000006387
      !>        New Z-axis:
      !>           position: -0.979862518033 -0.199671507623  0.000857203851
      !>           velocity:  0.000000044531 -0.000000218531 -0.000000000036
      !>
      !>        Note that these vectors define the transformation between the
      !>        new frame and J2000 at the given ET:
      !>
      !>               .-            -.
      !>               |       :      |
      !>               |   R   :  0   |
      !>           M = | ......:......|
      !>               |       :      |
      !>               | dRdt  :  R   |
      !>               |       :      |
      !>               `-            -'
      !>
      !>        with
      !>
      !>           DATA         R     / X_NEW(1:3), Y_NEW(1:3), Z_NEW(1:3)  /
      !>
      !>           DATA         dRdt  / X_NEW(4:6), Y_NEW(4:6), Z_NEW(4:6)  /
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples.
      !>
      !>-    SPICELIB Version 1.0.1, 22-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
      pure subroutine dvcrss ( s1, s2, sout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: s1   ( 6 )  !! Left hand state for cross product and derivative.
      real(wp), intent(in) :: s2   ( 6 )  !! Right hand state for cross product and derivative.
      real(wp), intent(out) :: sout ( 6 )  !! State associated with cross product of positions.

      real(wp) :: vtemp  ( 3 )
      real(wp) :: dvtmp1 ( 3 )
      real(wp) :: dvtmp2 ( 3 )

!
!     Calculate the cross product of S1 and S2, store it in VTEMP.
!
      call vcrss ( s1(1), s2(1), vtemp  )

!
!     Calculate the two components of the derivative of S1 x S2.
!
      call vcrss ( s1(4), s2(1), dvtmp1 )
      call vcrss ( s1(1), s2(4), dvtmp2 )

!
!     Put all of the pieces into SOUT.
!
      sout(1:3) = vtemp
      sout(4:6) = dvtmp1 + dvtmp2

      return
      end subroutine dvcrss

      !> Compute the derivative of the dot product of two double
      !> precision position vectors.
      !>
      !> The function returns the derivative of the dot product <S1,S2>
      !>
      !>## Detailed Input
      !>
      !>     S1       is any state vector. The components are in order
      !>              (x, y, z, dx/dt, dy/dt, dz/dt )
      !>
      !>     S2       is any state vector.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the derivative of the dot product of the
      !>     position portions of the two state vectors S1 and S2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given two state vectors S1 and S2 made up of position and
      !>     velocity components (P1,V1) and (P2,V2) respectively,
      !>     DVDOT calculates the derivative of the dot product of P1 and P2,
      !>     i.e. the time derivative
      !>
      !>           d
      !>           -- < P1, P2 > = < V1, P2 > + < P1, V2 >
      !>           dt
      !>
      !>     where <,> denotes the dot product operation.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that given two state vectors whose position components
      !>        are unit vectors, and that we need to compute the rate of
      !>        change of the angle between the two vectors.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DVDOT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DVDOT
      !>      DOUBLE PRECISION      VDOT
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      DTHETA
      !>      DOUBLE PRECISION      S1     (6)
      !>      DOUBLE PRECISION      S2     (6)
      !>
      !>C
      !>C     Define the two state vectors whose position
      !>C     components are unit vectors.
      !>C
      !>      DATA                  S1 /
      !>     .         7.2459D-01,  6.6274D-01, 1.8910D-01,
      !>     .        -1.5990D-06,  1.6551D-06, 7.4873D-07 /
      !>
      !>      DATA                  S2 /
      !>     .         8.4841D-01, -4.7790D-01, -2.2764D-01,
      !>     .         1.0951D-07,  1.0695D-07,  4.8468D-08 /
      !>
      !>C
      !>C     We know that the Cosine of the angle THETA between them
      !>C     is given by
      !>C
      !>C        cos(THETA) = VDOT(S1,S2)
      !>C
      !>C     Thus by the chain rule, the derivative of the angle is
      !>C     given by:
      !>C
      !>C        sin(THETA) dTHETA/dt = DVDOT(S1,S2)
      !>C
      !>C     Thus for values of THETA away from zero we can compute
      !>C     dTHETA/dt as:
      !>C
      !>      DTHETA = DVDOT(S1,S2) / SQRT( 1 - VDOT(S1,S2)**2 )
      !>
      !>      WRITE(*,'(A,F18.12)') 'Rate of change of angle '
      !>     . //                   'between S1 and S2:', DTHETA
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Rate of change of angle between S1 and S2:   -0.000002232415
      !>
      !>        Note that if the position components of S1 and S2 are parallel,
      !>        the derivative of the  angle between the positions does not
      !>        exist. Any code that computes the derivative of the angle
      !>        between two position vectors should account for the case
      !>        when the position components are parallel.
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for determining that the states S1 and
      !>         S2 are not so large as to cause numeric overflow. In most
      !>         cases this won't present a problem.
      !>
      !>     2)  An implicit assumption exists that S1 and S2 are specified in
      !>         the same reference frame. If this is not the case, the
      !>         numerical result has no meaning.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples. Added entry #2 to $Restrictions.
      !>
      !>-    SPICELIB Version 1.0.0, 18-MAY-1995 (WLT)
      pure real(wp) function dvdot ( s1, s2 )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: s1 ( 6 )  !! First state vector in the dot product.
      real(wp), intent(in) :: s2 ( 6 )  !! Second state vector in the dot product.

      dvdot = s1(1)*s2(4) + s1(2)*s2(5) + s1(3)*s2(6) &
            + s1(4)*s2(1) + s1(5)*s2(2) + s1(6)*s2(3)

      return
      end function dvdot

      !> Find the unit vector corresponding to a state vector and the
      !> derivative of the unit vector.
      !>
      !>## Detailed Input
      !>
      !>     S1       is any double precision state. If the position
      !>              component of the state is the zero vector, this
      !>              routine will detect it and will not attempt to divide
      !>              by zero.
      !>
      !>## Detailed Output
      !>
      !>     SOUT     is a state containing the unit vector pointing in
      !>              the direction of position component of S1 and the
      !>              derivative of the unit vector with respect to time.
      !>
      !>              SOUT may overwrite S1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If S1 represents the zero vector, then the position
      !>         component of SOUT will also be the zero vector. The
      !>         velocity component will be the velocity component
      !>         of S1.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let S1 be a state vector with position and velocity components P
      !>     and V respectively. From these components one can compute the
      !>     unit vector parallel to P, call it U and the derivative of U
      !>     with respect to time, DU. This pair (U,DU) is the state returned
      !>     by this routine in SOUT.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that STATE gives the apparent state of a body with
      !>        respect to an observer. This routine can be used to compute
      !>        the instantaneous angular rate of the object across the sky as
      !>        seen from the observers vantage.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: dvhat_ex1.tm
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
      !>              pck00008.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0009.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'pck00008.tpc',
      !>                                  'naif0009.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DVHAT_EX1
      !>      IMPLICIT NONE
      !>
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      OMEGA
      !>      DOUBLE PRECISION      STATE  (6)
      !>      DOUBLE PRECISION      USTATE (6)
      !>
      !>      DOUBLE PRECISION      VNORM
      !>
      !>      CHARACTER*(32)        EPOCH
      !>      CHARACTER*(32)        TARGET
      !>      CHARACTER*(32)        FRAME
      !>      CHARACTER*(32)        ABCORR
      !>      CHARACTER*(32)        OBSRVR
      !>
      !>C
      !>C     Load SPK, PCK, and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'dvhat_ex1.tm' )
      !>
      !>C
      !>C     Define an arbitrary epoch, convert the epoch to
      !>C     ephemeris time.
      !>C
      !>      EPOCH = 'Jan 1 2009'
      !>      CALL STR2ET ( EPOCH, ET )
      !>
      !>C
      !>C     Calculate the state of the moon with respect to the
      !>C     earth-moon barycenter in J2000, corrected for light time
      !>C     and stellar aberration at ET.
      !>C
      !>      TARGET   = 'MOON'
      !>      FRAME    = 'J2000'
      !>      ABCORR   = 'LT+S'
      !>      OBSRVR   = 'EARTH BARYCENTER'
      !>
      !>      CALL SPKEZR ( TARGET, ET,    FRAME, ABCORR,
      !>     .              OBSRVR, STATE, LT            )
      !>
      !>C
      !>C     Calculate the unit vector of STATE and the derivative
      !>C     of the unit vector.
      !>C
      !>      CALL DVHAT ( STATE, USTATE )
      !>
      !>C
      !>C     Calculate the instantaneous angular velocity from the
      !>C     magnitude of the derivative of the unit vector.
      !>C
      !>C          v = r x omega
      !>C
      !>C          ||omega|| = ||v||  for  r . v = 0
      !>C                      -----
      !>C                      ||r||
      !>C
      !>C          ||omega|| = ||v||  for  ||r|| = 1
      !>C
      !>      OMEGA = VNORM( USTATE(4) )
      !>
      !>      WRITE(*,'(A,E18.12)') 'Instantaneous angular velocity'
      !>     . //                   ' (rad/s): ', OMEGA
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Instantaneous angular velocity (rad/s): 0.248106659269E-05
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
      !>-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header comments to comply with NAIF standard. Added
      !>        meta-kernel to the example.
      !>
      !>-    SPICELIB Version 1.1.1, 06-MAY-2010 (EDW)
      !>
      !>        Expanded the code example into a complete program.
      !>
      !>        Reordered header sections to proper NAIF convention.
      !>        Removed Revision section, it listed a duplication of a
      !>        $Version section entry.
      !>
      !>-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VPERP and VSCL calls.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
      pure subroutine dvhat ( s1, sout )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: s1   ( 6 )  !! State to be normalized.
      real(wp), intent(out) :: sout ( 6 )  !! Unit vector S1 / |S1|, and its time derivative.

!
!     Local Constants.
!
      integer(ip), parameter :: pos = 1

      integer(ip), parameter :: vel = 4

!
!     Local variables.
!
      real(wp) :: length

!
!     Get the position portion of the output state and the length of
!     the input position.
!
      call unorm ( s1(pos), sout(pos), length )

      if ( length == 0.0_wp ) then
!
!        If the length of the input position is zero, just copy
!        the input velocity to the output velocity.
!
         sout(vel:vel+3-1) = s1(vel:vel+3-1)

      else
!
!        Otherwise the derivative of the unit vector is just the
!        component of the input velocity perpendicular to the input
!        position, scaled by the reciprocal of the length of the
!        input position.
!
         call vperp  ( s1(vel),      sout(pos), sout(vel) )
         sout(vel:vel+3-1) = (1.0_wp/length)*sout(vel:vel+3-1)

      end if

      return
      end subroutine dvhat

       !> Calculate the derivative of the norm of a 3-vector.
       !>
       !> The function returns the derivative of the norm of the position
       !> component of the input STATE vector.
       !>
       !>## Detailed Input
       !>
       !>     STATE    is a double precision 6-vector, the second three
       !>              components being the derivatives of the first three
       !>              with respect to some scalar.
       !>
       !>                               dx
       !>                 STATE =  ( x, -- )
       !>                               ds
       !>
       !>              A common form for STATE would contain position and
       !>              velocity.
       !>
       !>## Detailed Output
       !>
       !>     The function returns the derivative of the norm of the position
       !>     component of the input STATE vector:
       !>
       !>                  d ||x||
       !>        DVNORM = --------
       !>                    ds
       !>
       !>     where the norm of x is given by:
       !>
       !>                                     .----------------
       !>                   .---------       /    2    2    2
       !>        ||x|| =  \/ < x, x >  = \  / ( x1 + x2 + x3  )
       !>                                 \/
       !>
       !>     If the velocity component of STATE is:
       !>
       !>                  dx1   dx2   dx3
       !>           v = ( ----, ----, ---- )
       !>                  ds    ds    ds
       !>
       !>     then
       !>
       !>           d||x||      < x, v >
       !>           ------ =  ------------  =  < xhat, v >
       !>             ds        .---------
       !>                     \/ < x, x >
       !>
       !>## Parameters
       !>
       !>     None.
       !>
       !>## Exceptions
       !>
       !>     Error free.
       !>
       !>     1)  If the first three components of STATE ("x") describe the
       !>         origin (zero vector) the routine returns zero as the
       !>         derivative of the vector norm.
       !>
       !>## Files
       !>
       !>     None.
       !>
       !>## Particulars
       !>
       !>     A common use for this routine is to calculate the time derivative
       !>     of the radius corresponding to a state vector.
       !>
       !>## Examples
       !>
       !>     The numerical results shown for this example may differ across
       !>     platforms. The results depend on the SPICE kernels used as
       !>     input, the compiler and supporting libraries, and the machine
       !>     specific arithmetic implementation.
       !>
       !>     1) Compute the derivative of the norm of three vectors of
       !>        different magnitudes. Use the first two vectors to define
       !>        the derivatives as parallel and anti-parallel, and let
       !>        the third be the zero vector.
       !>
       !>        Example code begins here.
       !>
       !>```fortran
       !>      PROGRAM DVNORM_EX1
       !>      IMPLICIT NONE
       !>
       !>      DOUBLE PRECISION      X     (3)
       !>      DOUBLE PRECISION      MAG   (3)
       !>      DOUBLE PRECISION      DVMAG (3)
       !>      DOUBLE PRECISION      Y     (6)
       !>
       !>      DOUBLE PRECISION      DVNORM
       !>C
       !>C     Create several 6-vectors (6x1 arrays) with the structure
       !>C
       !>C        s = |  x  |
       !>C            |     |
       !>C            |  dx |
       !>C            |  -- |
       !>C            |  ds |
       !>C
       !>C      where 'x' is a 3-vector (3x1 array).
       !>C
       !>
       !>C
       !>C      Create 's' with 'x' of varying magnitudes. Use 'x'
       !>C      and '-x' to define the derivative as parallel and
       !>C      anti-parallel.
       !>C
       !>      MAG(1) =  -4.D0
       !>      MAG(2) =   4.D0
       !>      MAG(3) =  12.D0
       !>
       !>      X(1)   = 1.D0
       !>      X(2)   = DSQRT( 2.D0 )
       !>      X(3)   = DSQRT( 3.D0 )
       !>
       !>C
       !>C     Parallel...
       !>C
       !>      Y(1)   = X(1) * 10.D0**MAG(1)
       !>      Y(2)   = X(2) * 10.D0**MAG(1)
       !>      Y(3)   = X(3) * 10.D0**MAG(1)
       !>      Y(4)   = X(1)
       !>      Y(5)   = X(2)
       !>      Y(6)   = X(3)
       !>
       !>      WRITE(*,*) 'Parallel x, dx/ds         : ', DVNORM( Y )
       !>
       !>C
       !>C     ... anti-parallel...
       !>C
       !>      Y(1)   = X(1) * 10.D0**MAG(2)
       !>      Y(2)   = X(2) * 10.D0**MAG(2)
       !>      Y(3)   = X(3) * 10.D0**MAG(2)
       !>      Y(4)   = -X(1)
       !>      Y(5)   = -X(2)
       !>      Y(6)   = -X(3)
       !>
       !>      WRITE(*,*) 'Anti-parallel x, dx/ds    : ', DVNORM( Y )
       !>
       !>C
       !>C     ... 'x' zero vector
       !>C
       !>      Y(1)   = 0.D0
       !>      Y(2)   = 0.D0
       !>      Y(3)   = 0.D0
       !>      Y(4)   = X(1) * 10.D0**MAG(3)
       !>      Y(5)   = X(2) * 10.D0**MAG(3)
       !>      Y(6)   = X(3) * 10.D0**MAG(3)
       !>
       !>      WRITE(*,*) 'Zero vector x, large dx/ds: ', DVNORM( Y )
       !>      END
       !>```
       !>
       !>        When this program was executed on a Mac/Intel/gfortran/64-bit
       !>        platform, the output was:
       !>
       !>         Parallel x, dx/ds         :    2.4494897427831779
       !>         Anti-parallel x, dx/ds    :   -2.4494897427831779
       !>         Zero vector x, large dx/ds:    0.0000000000000000
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
       !>     E.D. Wright        (JPL)
       !>
       !>## Version
       !>
       !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
       !>
       !>        Added IMPLICIT NONE statement.
       !>
       !>        Edited the header to comply with NAIF standard. Added problem
       !>        statement to the example. Moved the contents of the
       !>        $Restrictions section to $Exceptions.
       !>
       !>-    SPICELIB Version 1.0.0, 03-MAY-2010 (EDW)
       pure real(wp) function dvnorm( state )
       use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: state ( 6 )  !! A 6-vector composed of three coordinates and their
                                           !! derivatives.

!
!     SPICELIB functions.
!

!
!     Local Variables.
!
      real(wp) :: xhat ( 3 )

!
!     If "x" describes the zero vector, return zero as the derivative
!     of the vector norm.
!
      if ( vnorm( state(1) ) == 0.0_wp ) then

         dvnorm = 0.0_wp
         return

      end if

!
!     Construct a unit vector from the x vector data
!     in STATE.
!
      call vhat( state(1), xhat )

!
!     Project the velocity components onto the XHAT vector.
!
!      d ||x||          x
!      -------  = v . -----
!        ds           ||x||
!

      dvnorm = vdot( state(4), xhat )

      return
      end function dvnorm

      !> Convert a SPICE ellipse to a center vector and two generating
      !> vectors. The selected generating vectors are semi-axes of the
      !> ellipse.
      !>
      !>## Detailed Input
      !>
      !>     ELLIPS   is a SPICE ellipse.
      !>
      !>## Detailed Output
      !>
      !>     CENTER,
      !>     SMAJOR,
      !>     SMINOR   are, respectively, a center vector, a semi-major
      !>              axis vector, and a semi-minor axis vector that
      !>              generate the input ellipse. This ellipse is the
      !>              set of points
      !>
      !>                 CENTER + cos(theta) SMAJOR + sin(theta) SMINOR
      !>
      !>              where theta ranges over the interval (-pi, pi].
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     SPICE ellipses serve to simplify calling sequences and reduce
      !>     the chance for error in declaring and describing argument lists
      !>     involving ellipses.
      !>
      !>     The set of ellipse conversion routines is
      !>
      !>        CGV2EL ( Center and generating vectors to ellipse )
      !>        EL2CGV ( Ellipse to center and generating vectors )
      !>
      !>     A word about the output of this routine: the semi-major axis of
      !>     an ellipse is a vector of largest possible magnitude in the set
      !>
      !>        cos(theta) VEC1  +  sin(theta) VEC2,
      !>
      !>     where theta is in the interval (-pi, pi].  There are two such
      !>     vectors; they are additive inverses of each other. The semi-minor
      !>     axis is an analogous vector of smallest possible magnitude. The
      !>     semi-major and semi-minor axes are orthogonal to each other. If
      !>     SMAJOR and SMINOR are choices of semi-major and semi-minor axes,
      !>     then the input ellipse can also be represented as the set of
      !>     points
      !>
      !>        CENTER + cos(theta) SMAJOR + sin(theta) SMINOR
      !>
      !>     where theta ranges over the interval (-pi, pi].
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a SPICE ellipse structure, extract its components into
      !>        independent variables.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EL2CGV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 UBEL
      !>      PARAMETER             ( UBEL =   9 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        CENTER ( 3    )
      !>      DOUBLE PRECISION        ECENTR ( 3    )
      !>      DOUBLE PRECISION        ELLIPS ( UBEL )
      !>      DOUBLE PRECISION        SMAJOR ( 3    )
      !>      DOUBLE PRECISION        SMINOR ( 3    )
      !>      DOUBLE PRECISION        VEC1   ( 3    )
      !>      DOUBLE PRECISION        VEC2   ( 3    )
      !>
      !>      INTEGER                 I
      !>
      !>C
      !>C     Define the center and two linearly independent
      !>C     generating vectors of an ellipse (the vectors need not
      !>C     be linearly independent).
      !>C
      !>      DATA                    CENTER / -1.D0,  1.D0, -1.D0 /
      !>
      !>      DATA                    VEC1   /  1.D0,  1.D0,  1.D0 /
      !>
      !>      DATA                    VEC2   /  1.D0, -1.D0,  1.D0 /
      !>
      !>C
      !>C     Create the ELLIPS.
      !>C
      !>      CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
      !>
      !>C
      !>C     In a real application, please use SPICELIB API EL2CGV
      !>C     to retrieve the center and generating vectors from the
      !>C     ellipse structure (see next block).
      !>C
      !>      WRITE(*,'(A)') 'SPICE ellipse:'
      !>      WRITE(*,'(A,3F10.6)') '   Semi-minor axis:',
      !>     .                                    ( ELLIPS(I), I=7,9 )
      !>      WRITE(*,'(A,3F10.6)') '   Semi-major axis:',
      !>     .                                    ( ELLIPS(I), I=4,6 )
      !>      WRITE(*,'(A,3F10.6)') '   Center         :',
      !>     .                                    ( ELLIPS(I), I=1,3 )
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Obtain the center and generating vectors from the
      !>C     ELLIPS.
      !>C
      !>      CALL EL2CGV ( ELLIPS, ECENTR, SMAJOR, SMINOR )
      !>
      !>      WRITE(*,'(A)') 'SPICE ellipse (using EL2CGV):'
      !>      WRITE(*,'(A,3F10.6)') '   Semi-minor axis:', SMINOR
      !>      WRITE(*,'(A,3F10.6)') '   Semi-major axis:', SMAJOR
      !>      WRITE(*,'(A,3F10.6)') '   Center         :', ECENTR
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        SPICE ellipse:
      !>           Semi-minor axis:  0.000000  1.414214  0.000000
      !>           Semi-major axis:  1.414214 -0.000000  1.414214
      !>           Center         : -1.000000  1.000000 -1.000000
      !>
      !>        SPICE ellipse (using EL2CGV):
      !>           Semi-minor axis:  0.000000  1.414214  0.000000
      !>           Semi-major axis:  1.414214 -0.000000  1.414214
      !>           Center         : -1.000000  1.000000 -1.000000
      !>
      !>     2) Given an ellipsoid and a viewpoint exterior to it, calculate
      !>        the limb ellipse as seen from that viewpoint.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EL2CGV_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER                 UBEL
      !>      PARAMETER             ( UBEL =   9 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION        A
      !>      DOUBLE PRECISION        B
      !>      DOUBLE PRECISION        C
      !>      DOUBLE PRECISION        ECENTR ( 3    )
      !>      DOUBLE PRECISION        LIMB   ( UBEL )
      !>      DOUBLE PRECISION        SMAJOR ( 3    )
      !>      DOUBLE PRECISION        SMINOR ( 3    )
      !>      DOUBLE PRECISION        VIEWPT ( 3    )
      !>
      !>C
      !>C     Define a viewpoint exterior to the ellipsoid.
      !>C
      !>      DATA                    VIEWPT /  2.D0,  0.D0,  0.D0 /
      !>
      !>C
      !>C     Define an ellipsoid.
      !>C
      !>      A = SQRT( 2.D0 )
      !>      B = 2.D0 * SQRT( 2.D0 )
      !>      C = SQRT( 2.D0 )
      !>
      !>C
      !>C     Calculate the limb ellipse as seen by from the
      !>C     viewpoint.
      !>C
      !>      CALL EDLIMB ( A, B, C, VIEWPT, LIMB )
      !>
      !>C
      !>C     Output the structure components.
      !>C
      !>      CALL EL2CGV ( LIMB, ECENTR, SMAJOR, SMINOR )
      !>
      !>      WRITE(*,'(A)') 'Limb ellipse as seen from viewpoint:'
      !>      WRITE(*,'(A,3F11.6)') '   Semi-minor axis:', SMINOR
      !>      WRITE(*,'(A,3F11.6)') '   Semi-major axis:', SMAJOR
      !>      WRITE(*,'(A,3F11.6)') '   Center         :', ECENTR
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Limb ellipse as seen from viewpoint:
      !>           Semi-minor axis:   0.000000   0.000000  -1.000000
      !>           Semi-major axis:   0.000000   2.000000  -0.000000
      !>           Center         :   1.000000   0.000000   0.000000
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
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
      pure subroutine el2cgv ( ellips, center, smajor, sminor )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: ellips ( ubel )  !! A SPICE ellipse.
      real(wp), intent(out) :: center (    3 )  !! Center and semi-axes of ELLIPS.
      real(wp), intent(out) :: smajor (    3 )  !! Center and semi-axes of ELLIPS.
      real(wp), intent(out) :: sminor (    3 )  !! Center and semi-axes of ELLIPS.

!
!     SPICE ellipses contain a center vector, a semi-major
!     axis vector, and a semi-minor axis vector.  These are
!     located, respectively, in elements
!
!        CTRPOS through CTRPOS + 1
!
!        MAJPOS through MAJPOS + 1
!
!        MINPOS through MINPOS + 1
!
      integer(ip), parameter :: ctrpos = 1

      integer(ip), parameter :: majpos = 4

      integer(ip), parameter :: minpos = 7

!
!     The center of the ellipse is held in the first three elements.
!     The semi-major and semi-minor axes come next.
!
      center = ellips(ctrpos:ctrpos+3-1)
      smajor = ellips(majpos:majpos+3-1)
      sminor = ellips(minpos:minpos+3-1)

      return
      end subroutine el2cgv

      !> Determine whether an integer is even.
      !>
      !> The function returns .TRUE. if IVAL is even, otherwise .FALSE.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (MOD instead).
      !>
      !>## Detailed Input
      !>
      !>     IVAL     is the integer to be tested for evenness.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if IVAL is even, .FALSE. if IVAL is
      !>     odd.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
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
      !>     Let ENDPTS contain a series of endpoints,
      !>
      !>        a , b , ..., a , b
      !>         1   1        n   n
      !>
      !>     representing an ordered collection of disjoint intervals,
      !>
      !>        a   <  b   < a
      !>         i  -   i     i+1
      !>
      !>     The following code fragment uses EVEN to determine whether
      !>     an arbitrary value X is contained in any of the intervals.
      !>
      !>        CONTAINED = .FALSE.
      !>
      !>        DO I = 1, N-1
      !>           IF ( X .GE. ENDPTS(I)  .AND.  X .LE. ENDPTS(I+1) ) THEN
      !>              CONTAINED = ( .NOT. EVEN ( I ) )
      !>           END IF
      !>        END DO
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Changed the input argument name "I" to "IVAL" for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental logical function even ( ival )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ival  !! The integer in question.

!
!     Self-explanatory.
!
      even = mod(ival, 2) == 0
      end function even

      !> Round an input double precision number to a specified exact value
      !> if the number and the value are equal to within some tolerance.
      !>
      !> The function returns VALUE whenever |NUMBER - VALUE| < TOL.
      !> -
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is an arbitrary double precision number.
      !>
      !>     VALUE    is a target value.
      !>
      !>     TOL      is a tolerance. NUMBER and VALUE are considered to
      !>              be equal if they differ by no more than this amount.
      !>              If TOL is negative, they are never considered equal.
      !>
      !>## Detailed Output
      !>
      !>     The function returns VALUE whenever |NUMBER - VALUE| < TOL, and
      !>     otherwise returns NUMBER.                            -
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
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
      !>     C
      !>     C     If the eccentricity is near one, make this a parabola.
      !>     C
      !>           ECC = EXACT ( ECC, 1.D0, 10.D-12 )
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental real(wp) function exact ( number, value, tol )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: number  !! Double precision number.
      real(wp), intent(in) :: value  !! Target value.
      real(wp), intent(in) :: tol  !! Tolerance.

!
!     Just shorthand, really.
!
      if ( abs ( number - value ) <= tol ) then
         exact = value

      else
         exact = number
      end if

      return
      end function exact

      !> Build a right handed orthonormal frame (x,y,z) from a
      !> 3-dimensional input vector, where the X-axis of the resulting
      !> frame is parallel to the original input vector.
      !>
      !>## Detailed Input
      !>
      !>     X        is a 3-dimensional vector used to form the first vector
      !>              of a right-handed orthonormal triple.
      !>
      !>## Detailed Output
      !>
      !>     X,
      !>     Y,
      !>     Z        are the 3-dimensional unit vectors that form a right
      !>              handed orthonormal frame, where X is now a unit vector
      !>              parallel to the original input vector in X. There are no
      !>              special geometric properties connected to Y and Z (other
      !>              than that they complete the right handed frame).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If X on input is the zero vector the "standard" frame (ijk)
      !>         is returned.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given an input vector X, this routine returns unit vectors X,
      !>     Y, and Z such that XYZ forms a right-handed orthonormal frame
      !>     where the output X is parallel to the input X.
      !>
      !>     This routine is intended primarily to provide a basis for
      !>     the plane orthogonal to X. There are no special properties
      !>     associated with Y and Z other than that the resulting XYZ frame
      !>     is right handed and orthonormal. There are an infinite
      !>     collection of pairs (Y,Z) that could be used to this end.
      !>     Even though for a given X, Y and Z are uniquely
      !>     determined, users
      !>     should regard the pair (Y,Z) as a random selection from this
      !>     infinite collection.
      !>
      !>     For instance, when attempting to determine the locus of points
      !>     that make up the limb of a triaxial body, it is a straightforward
      !>     matter to determine the normal to the limb plane. To find
      !>     the actual parametric equation of the limb one needs to have
      !>     a basis of the plane. This routine can be used to get a basis
      !>     in which one can describe the curve and from which one can
      !>     then determine the principal axes of the limb ellipse.
      !>
      !>## Examples
      !>
      !>     In addition to using a vector to construct a right handed frame
      !>     with the x-axis aligned with the input vector, one can construct
      !>     right handed frames with any of the axes aligned with the input
      !>     vector.
      !>
      !>     For example suppose we want a right hand frame XYZ with the
      !>     Z-axis aligned with some vector V. Assign V to Z
      !>
      !>           Z(1) = V(1)
      !>           Z(2) = V(2)
      !>           Z(3) = V(3)
      !>
      !>     Then call FRAME with the arguments X,Y,Z cycled so that Z
      !>     appears first.
      !>
      !>           CALL FRAME (Z, X, Y)
      !>
      !>     The resulting XYZ frame will be orthonormal with Z parallel
      !>     to the vector V.
      !>
      !>     To get an XYZ frame with Y parallel to V perform the following
      !>
      !>           Y(1) = V(1)
      !>           Y(2) = V(2)
      !>           Y(3) = V(3)
      !>
      !>           CALL FRAME (Y, Z, X)
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 03-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Improved
      !>        argument descriptions.
      !>
      !>-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VHAT call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 29-DEC-1988 (WLT) (IMU)
      !>
      !>        The routine was modified so that it now accepts any input
      !>        vector in the X slot (it originally was assumed to be a unit
      !>        vector).  Moreover, the original algorithm has been streamlined
      !>        a great deal to take advantage of our knowledge of the
      !>        internal structure of the orthonormal triple.
      pure subroutine frame ( x, y, z )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(inout) :: x ( 3 )  !! Input vector. A parallel unit vector on output.
      real(wp), intent(out) :: y ( 3 )  !! Unit vector in the plane orthogonal to X.
      real(wp), intent(out) :: z ( 3 )  !! Unit vector given by the cross product <X,Y>.

      real(wp) :: a
      real(wp) :: b
      real(wp) :: c
      real(wp) :: f

      integer(ip) :: s1
      integer(ip) :: s2
      integer(ip) :: s3

!
!     First make X into a unit vector.
!
      call vhatip ( x )

!
!     We'll need the squares of the components of X in a bit.
!
      a = x(1)*x(1)
      b = x(2)*x(2)
      c = x(3)*x(3)

!
!     If X is zero, then just return the ijk frame.
!
      if ( a+b+c == 0.0_wp ) then

         x(1) = 1.0_wp
         x(2) = 0.0_wp
         x(3) = 0.0_wp

         y(1) = 0.0_wp
         y(2) = 1.0_wp
         y(3) = 0.0_wp

         z(1) = 0.0_wp
         z(2) = 0.0_wp
         z(3) = 1.0_wp

         return

      end if

!
!     If we make it this far, determine which component of X has the
!     smallest magnitude.  This component will be zero in Y. The other
!     two components of X will put into Y swapped with the sign of
!     the first changed.  From there, Z can have only one possible
!     set of values which it gets from the smallest component
!     of X, the non-zero components of Y and the length of Y.
!
      if      (       ( a <= b ) &
                .and. ( a <= c ) ) then

         f  = dsqrt ( b + c )
         s1 = 1
         s2 = 2
         s3 = 3

      else if (       ( b <= a ) &
                .and. ( b <= c ) ) then

         f  = dsqrt ( a + c )
         s1 = 2
         s2 = 3
         s3 = 1

      else

         f  = dsqrt ( a + b )
         s1 = 3
         s2 = 1
         s3 = 2

      end if

!
!     Note: by construction, F is the magnitude of the large components
!     of X.  With this in mind, one can verify by inspection that X, Y
!     and Z yield an orthonormal frame.  The right handedness follows
!     from the assignment of values to S1, S2 and S3 (they are merely
!     cycled from one case to the next).
!
      y(s1)  =   0.0_wp
      y(s2)  = - x(s3) / f
      y(s3)  =   x(s2) / f

      z(s1)  =   f
      z(s2)  = - x(s1) * y(s3)
      z(s3)  =   x(s1) * y(s2)

      return
      end subroutine frame

      !> Return the greatest common divisor of two integers.
      !>
      !> The function returns the greatest common divisor of A and B.
      !>
      !>## Detailed Input
      !>
      !>     A        is any integer.
      !>
      !>     B        is any integer.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the greatest common divisor of A and B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If both A and B are zero, then GCD returns zero.
      !>
      !>     2)  If exactly one of A and B is zero, then GCD is by definition
      !>         the maximum of the absolute values of A and B.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses Euclid's Algorithm to find the greatest common
      !>     divisor (GCD) of the integers A and B. In other words the
      !>     largest integer, G, such that A = k*G for some k and B = j*G for
      !>     some G. Note if either A or B is zero, then we return the
      !>     maximum of the two integers ABS(A) and ABS(B).  If one is
      !>     non-zero we have just what the definition says. If both are zero
      !>     the definition above does not give us a GCD, so we take the GCD
      !>     of 0 and 0 to be 0.
      !>
      !>## Examples
      !>
      !>     A      B            GCD
      !>     -----  -----         -----
      !>       8      4             4
      !>      120    44             4
      !>      15    135            15
      !>     101     97             1
      !>     119    221            17
      !>     144     81             9
      !>       0    111           111
      !>       0      0             0
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  D. Knuth, "The Art of Computer Programming Vol 1. --
      !>          Fundamental Algorithms," 2nd Ed., Addison-Wesley, 1973
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental integer(ip) function gcd ( a, b )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: a  !! Any integer
      integer(ip), intent(in) :: b  !! Any integer

      integer(ip) :: remndr
      integer(ip) :: absa
      integer(ip) :: absb
      integer(ip) :: p
      integer(ip) :: q

      absa = abs(a)
      absb = abs(b)

      if ( absa > absb ) then
         p = absa
         q = absb
      else
         p = absb
         q = absa
      end if

      remndr = 1

      if ( q /= 0 ) then

         do while (remndr /= 0 )
            gcd    = q
            remndr = p - (p/q)*q
            p      = q
            q      = remndr
         end do

      else

         gcd = p

      end if

      return

      end function gcd

      !> Return the 3x3 identity matrix.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     MATRIX   is the 3x3 Identity matrix. That MATRIX is
      !>              the following
      !>
      !>                 .-                       -.
      !>                 |  1.0D0   0.0D0   0.0D0  |
      !>                 |  0.0D0   1.0D0   0.0D0  |
      !>                 |  0.0D0   0.0D0   1.0D0  |
      !>                 `-                       -'
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine for obtaining the 3x3 identity matrix
      !>     so that you may avoid having to write the loop or assignments
      !>     needed to get the matrix.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a 3x3 matrix and compute its inverse using the SPICELIB
      !>        routine INVERT. Verify the accuracy of the computed inverse
      !>        using the mathematical identity
      !>
      !>                -1
      !>           M x M   - I = 0
      !>
      !>        where I is the 3x3 identity matrix.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM IDENT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      IDMAT  ( 3, 3 )
      !>      DOUBLE PRECISION      IMAT   ( 3, 3 )
      !>      DOUBLE PRECISION      M      ( 3, 3 )
      !>      DOUBLE PRECISION      MOUT   ( 3, 3 )
      !>      DOUBLE PRECISION      MZERO  ( 3, 3 )
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
      !>      CALL INVERT ( M, MOUT )
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
      !>      CALL IDENT ( IDMAT )
      !>      CALL MXM   ( M, MOUT, IMAT )
      !>
      !>      CALL VSUBG ( IMAT, IDMAT, 9, MZERO )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original times inverse minus identity:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( MZERO(I,J), J=1,3 )
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
      !>               0.0000000       2.0000000      -0.0000000
      !>              -1.0000000       0.0000000      -0.0000000
      !>               0.0000000      -0.0000000       1.0000000
      !>
      !>         Original times inverse minus identity:
      !>               0.0000000       0.0000000       0.0000000
      !>               0.0000000       0.0000000       0.0000000
      !>               0.0000000       0.0000000       0.0000000
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
      !>-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.0, 05-FEB-1996 (WLT)
      pure subroutine ident ( matrix )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(out) :: matrix ( 3, 3 )  !! The 3x3 identity matrix.

      matrix ( 1, 1 ) = 1.0_wp
      matrix ( 2, 1 ) = 0.0_wp
      matrix ( 3, 1 ) = 0.0_wp

      matrix ( 1, 2 ) = 0.0_wp
      matrix ( 2, 2 ) = 1.0_wp
      matrix ( 3, 2 ) = 0.0_wp

      matrix ( 1, 3 ) = 0.0_wp
      matrix ( 2, 3 ) = 0.0_wp
      matrix ( 3, 3 ) = 1.0_wp

      return
      end subroutine ident

      !> Determine if a given vector lies inside the solid tetrahedral
      !> angle determined by 3 vectors. If it does, return the
      !> point where the scale factor such that SCALE*V lies in the
      !> plane spanned by E1, E2, and E3.
      !>
      !>## Detailed Input
      !>
      !>     V        is a 3-vector. This is the vector to test to see
      !>              if it lies between the 3 vectors E1, E2 and E3
      !>
      !>     E1,
      !>     E2,
      !>     E3       are the three edges of a solid tetrahedral angle. (See
      !>              particulars for a discussion of the solid angle).
      !>
      !>## Detailed Output
      !>
      !>     FOUND    indicates that V lies inside the solid tetrahedral
      !>              angle determined by E1, E2 and E3.
      !>
      !>     SCALE    if V lies inside the solid tetrahedral angle given
      !>              by E1, E2 and E3, SCALE*V is the point is the positive
      !>              scalar multiple of V that pierces the triangle
      !>              determined by the points E1, E2, E3.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If E1, E2 and E3 are not linearly independent, the routine
      !>         returns .FALSE. SCALE will be set to 0.
      !>
      !>     2)  If V is the zero vector, the routine returns .FALSE.
      !>         SCALE will be set to 0.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given 3 linearly independent vectors E1, E2, and E3 the
      !>     set of vectors a*E1 + b*E2 + c*E3  where a, b, and c
      !>     are non-negative form a region of space that is a tetrahedral
      !>     solid angle. If you cut this solid angle with a plane
      !>     that intersects all three rays from the origin determined
      !>     by E1, E2 and E3 you will get a tetrahedron (a 4-sided
      !>     solid with each face a triangle).
      !>
      !>     This routine determines whether the ray associated with
      !>     a vector V lies inside the tetrahedral angle E1,E2,E3.
      !>     Moreover, if V lies inside this angle, this routine returns
      !>     the scale factor SCALE such that the point SCALE*V
      !>     lies in the plane containing the points E1, E2 and E3.
      !>     This is necessarily a point in the triangle determined by
      !>     E1, E2 and E3.
      !>
      !>## Examples
      !>
      !>     Suppose you have a triangle in space specified by three
      !>     vertices P1, P2 and P3 and that an observer at location
      !>     OBS is looking along the ray emanating from OBS with
      !>     direction V. Does this ray intersect the triangle
      !>     P1, P2, P3?  Using this routine, you can answer this
      !>     question and give the point of intersection if there is
      !>     one. Here's how.
      !>
      !>     First construct the vectors from OBS to the corners of
      !>     the triangle.
      !>
      !>     CALL VSUB ( P1, OBS, E1 )
      !>     CALL VSUB ( P2, OBS, E2 )
      !>     CALL VSUB ( P3, OBS, E3 )
      !>
      !>     Now see if V lies between the vectors E1, E2, E3 and return
      !>     the intersection point if it does.
      !>
      !>     CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      !>
      !>     If there was an intersection, add SCALE*V to OBS to get the
      !>     point of intersection. Otherwise say there was no intersection.
      !>
      !>     IF ( FOUND ) THEN
      !>
      !>        CALL VLCOM ( 1.0D0, OBS, SCALE, V, POINT )
      !>
      !>        WRITE (*,*) 'The ray intersects the triangle at:
      !>        WRITE (*,*) POINT(1)
      !>        WRITE (*,*) POINT(2)
      !>        WRITE (*,*) POINT(3)
      !>
      !>     ELSE
      !>
      !>        WRITE (*,*) 'There is no intersection.'
      !>
      !>     END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This routine can suffer from extreme loss of precision if the
      !>         vectors E1, E2, E3 are too long compared to the lengths of the
      !>         line segments formed by their pairwise differences.
      !>
      !>         The user of this routine must ensure that the inputs are
      !>         suitable.
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
      !>-    SPICELIB Version 1.0.3, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 02-FEB-2016 (NJB)
      !>
      !>        Fixed comment typos. Updated $Restrictions.
      !>
      !>-    SPICELIB Version 1.0.1, 08-OCT-2009 (NJB)
      !>
      !>        Updated header.
      !>
      !>-    SPICELIB Version 1.0.0, 09-JUN-1996 (WLT)
      pure subroutine insang ( v, e1, e2, e3, found, scale )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v     ( 3 )  !! Vector to test for "betweenness"
      real(wp), intent(in) :: e1    ( 3 )  !! First edge of the tetrahedral angle
      real(wp), intent(in) :: e2    ( 3 )  !! Second edge of the tetrahedral angle
      real(wp), intent(in) :: e3    ( 3 )  !! Third edge of the tetrahedral angle
      logical, intent(out) :: found  !! Indicates whether V lies in the solid angle
      real(wp), intent(out) :: scale  !! Scale times V is in the triangle E1,E2,E3

      real(wp) :: denom
      real(wp) :: en
      real(wp) :: norm12 ( 3 )
      real(wp) :: norm23 ( 3 )
      real(wp) :: norm31 ( 3 )
      real(wp) :: vn12
      real(wp) :: vn23
      real(wp) :: vn31
!
!     Our initial value for SCALE is zero.  When we have better
!     information, we'll change this.
!
      scale = 0.0_wp

!
!     First we construct a normal to the plane spanned by E1 and E2
!     and make sure that we don't get a zero vector.  If we
!     get the zero vector, E1 and E2 are linearly dependent so we
!     set the value of FOUND to FALSE and return.
!
      call vcrss ( e1, e2, norm12 )

!
!     First make sure V and E3 are in the same half space
!     bounded by E1 and E2.  If they are not, we can return.
!
      vn12 = vdot( v,  norm12 )
      en   = vdot( e3, norm12 )
!
!     Determine whether NORML and E3 are perpendicular.  If they
!     are perpendicular, E3 is a linear combination of E1 and E2.
!     In this case set FOUND to FALSE and return.
!
      if ( en == 0.0_wp ) then
         found = .false.
         return
      end if
!
!     Now check to see if V and E3 are in the same half space.  If
!     not, we can stop and return the value FALSE.
!
      if (       en   > 0.0_wp &
           .and. vn12 < 0.0_wp ) then

         found = .false.
         return

      else if (       en   < 0.0_wp &
                .and. vn12 > 0.0_wp ) then

         found = .false.
         return

      end if

!
!     Now check that V and E1 are on the same side of the plane
!     spanned by E2 and E3.  Note we don't have to compute EN
!     again <( E2 x E3 ), E1 >  because of the vector identity
!
!       < (E1 x E2), E3 > =  < (E2 x E3), E1 > = < (E3 x E1), E2 >
!
      call vcrss ( e2, e3, norm23 )
      vn23 = vdot  ( v,      norm23 )

!
!     The following tests are the same as in the previous case.
!
      if (       en   > 0.0_wp &
           .and. vn23 < 0.0_wp ) then

         found = .false.
         return

      else if (       en   < 0.0_wp &
                .and. vn23 > 0.0_wp ) then

         found = .false.
         return

      end if

!
!     Finally check to see if V and E2 are in the same half space
!     bounded by E3 and E2
!
      call vcrss ( e3, e1, norm31 )
      vn31 = vdot  ( v,      norm31 )

      if (       en   > 0.0_wp &
           .and. vn31 < 0.0_wp ) then

         found = .false.
         return

      else if (       en   < 0.0_wp &
                .and. vn31 > 0.0_wp ) then

         found = .false.
         return

      end if

!
!     If you get this far, we know that V is lies in the intersection
!     of the half spaces determined by the various combinations of
!     E1, E2 and E3.
!
      found = .true.

!
!     Now find the intersection. First get a normal to the triangle.
!     One way to get the normal is to find the vector cross
!     product
!
!       NORML = ( E2 - E1 ) x ( E3 - E1 )
!
!     However, this can be rewritten as:
!
!        NORML = E2 x E3 - E1 x E3 - E2 x E1 + E1 x E1
!
!              = E2 x E3 + E3 x E1 + E1 x E2
!
!     But we already have the three components E2 x E3, ... etc.
!     in the vectors NORM12, NORM23, NORM31
!
!     Now we need to find the scalar multiple t*V such that
!
!        < tV - E1, NORML > = 0
!
!     But this can be rewritten as:
!
!        t < V, NORML > = < E1, NORML >
!
!     Solving for t yields
!
!      t = < E1, NORML > / < V, NORML >
!
!        = < E1, E1xE2 + E2xE3 + E3xE1 > / <  V, E1xE2 + E2xE3 + E3xE1 >
!
!        = ( 0 + <E1, E2xE3> + 0 ) / (<V,E1xE2> + <V,E2xE3> + <V,E3xE1>)
!
!        =  EN / ( VN12 + VN23 + VN31 )
!
      denom = vn12 + vn23 + vn31

      if ( denom == 0.0_wp ) then
         found = .false.
      else
         found = .true.
         scale =  en/denom
      end if

      return
      end subroutine insang

      !> Generate the inverse of a 3x3 matrix.
      !>
      !>## Detailed Input
      !>
      !>     M        is an arbitrary 3x3 matrix. The limits on the size of
      !>              elements of M are determined by the process of
      !>              calculating the cofactors of each element of the matrix.
      !>              For a 3x3 matrix this amounts to the differencing of two
      !>              terms, each of which consists of the multiplication of
      !>              two matrix elements. This multiplication must not exceed
      !>              the range of double precision numbers or else an overflow
      !>              error will occur.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is the inverse of M and is calculated explicitly using
      !>              the matrix of cofactors. MOUT is set to be the zero
      !>              matrix if M is singular.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If M is singular, MOUT is set to be the zero matrix.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     First the determinant is explicitly calculated using the
      !>     fundamental definition of the determinant. If this value is less
      !>     that 10**-16 then the matrix is deemed to be singular and the
      !>     output value is filled with zeros. Otherwise, the output matrix
      !>     is calculated an element at a time by generating the cofactor of
      !>     each element. Finally, each element in the matrix of cofactors
      !>     is multiplied by the reciprocal of the determinant and the result
      !>     is the inverse of the original matrix.
      !>
      !>     NO INTERNAL CHECKING ON THE INPUT MATRIX M IS PERFORMED EXCEPT
      !>     ON THE SIZE OF ITS DETERMINANT.  THUS IT IS POSSIBLE TO GENERATE
      !>     A FLOATING POINT OVERFLOW OR UNDERFLOW IN THE PROCESS OF
      !>     CALCULATING THE MATRIX OF COFACTORS.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a double precision 3x3 matrix, compute its inverse. Check
      !>        that the original matrix times the computed inverse produces
      !>        the identity matrix.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM INVERT_EX1
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
      !>      CALL INVERT ( M, MOUT )
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
      !>               0.0000000       2.0000000      -0.0000000
      !>              -1.0000000       0.0000000      -0.0000000
      !>               0.0000000      -0.0000000       1.0000000
      !>
      !>         Original times inverse:
      !>               1.0000000       0.0000000       0.0000000
      !>               0.0000000       1.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>## Restrictions
      !>
      !>     1)  The input matrix must be such that generating the cofactors
      !>         will not cause a floating point overflow or underflow. The
      !>         strictness of this condition depends, of course, on the
      !>         computer installation and the resultant maximum and minimum
      !>         values of double precision numbers.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed input argument name M1 to M for consistency with other
      !>        routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        complete code example to $Examples section.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine invert ( m, mout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: m    ( 3,3 )  !! Matrix to be inverted.
      real(wp), intent(out) :: mout ( 3,3 )  !! Inverted matrix (M)**-1. If M is singular, then MOUT
                                             !! will be the zero matrix.

      real(wp) :: mtemp(3,3)
      real(wp) :: mdet
      real(wp) :: invdet

!
!  Find the determinant of M and check for singularity
!
      mdet = det(m)
      if ( dabs(mdet) < 1.0e-16_wp ) then
         mout = 0.0_wp
         return
      end if

!
!  Get the cofactors of each element of M
!
      mtemp(1,1) =  (m(2,2)*m(3,3) - m(3,2)*m(2,3))
      mtemp(1,2) = -(m(1,2)*m(3,3) - m(3,2)*m(1,3))
      mtemp(1,3) =  (m(1,2)*m(2,3) - m(2,2)*m(1,3))
      mtemp(2,1) = -(m(2,1)*m(3,3) - m(3,1)*m(2,3))
      mtemp(2,2) =  (m(1,1)*m(3,3) - m(3,1)*m(1,3))
      mtemp(2,3) = -(m(1,1)*m(2,3) - m(2,1)*m(1,3))
      mtemp(3,1) =  (m(2,1)*m(3,2) - m(3,1)*m(2,2))
      mtemp(3,2) = -(m(1,1)*m(3,2) - m(3,1)*m(1,2))
      mtemp(3,3) =  (m(1,1)*m(2,2) - m(2,1)*m(1,2))

!
!  Multiply the cofactor matrix by 1/MDET to obtain the inverse
!
      invdet = 1.0_wp / mdet
      mout = invdet*mtemp
      return
      end subroutine invert

      !> Convert from latitudinal coordinates to cylindrical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RADIUS   is the distance of a point from the origin.
      !>
      !>     LON      is the angle of the point from the XZ plane in
      !>              radians.
      !>
      !>     LAT      is the angle of the point from the XY plane in
      !>              radians.
      !>
      !>## Detailed Output
      !>
      !>     R        is the distance of the point from the Z-axis.
      !>
      !>     CLON     is the angle of the point from the XZ plane in
      !>              radians. CLON is set equal to LON.
      !>
      !>     Z        is the height of the point above the XY plane.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the cylindrical coordinates of a point
      !>     whose position is input in latitudinal coordinates.
      !>
      !>     Latitudinal coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     above the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the latitudinal coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to cylindrical and
      !>        rectangular coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: latcyl_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATCYL_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'latcyl_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to latitudinal
      !>C     coordinates.
      !>C
      !>      CALL RECLAT ( POS, RADIUS, LON, LAT )
      !>
      !>C
      !>C     Convert the latitudinal coordinates to cylindrical.
      !>C
      !>      CALL LATCYL ( RADIUS, LON, LAT, R, CLON, Z )
      !>
      !>C
      !>C     Convert the cylindrical coordinates to rectangular.
      !>C
      !>      CALL CYLREC ( R, CLON, Z, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR()
      !>      WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', R
      !>      WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z          (km): ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from CYLREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius     (km):      403626.33912495
      !>          Longitude (deg):         -98.34959789
      !>          Latitude  (deg):         -18.26566077
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius     (km):      383289.01777726
      !>          Longitude (deg):         -98.34959789
      !>          Z          (km):     -126505.93063865
      !>
      !>         Rectangular coordinates from CYLREC:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of latitudinal coordinates
      !>        and the corresponding cylindrical coordinates.
      !>
      !>        Corresponding latitudinal and cylindrical coordinates are
      !>        listed to three decimal places. Input and output angles are
      !>        in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATCYL_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      LAT    ( NREC )
      !>      DOUBLE PRECISION      LON    ( NREC )
      !>      DOUBLE PRECISION      RADIUS ( NREC )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RLAT
      !>      DOUBLE PRECISION      RLON
      !>      DOUBLE PRECISION      Z
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input latitudinal coordinates. Angles in
      !>C     degrees.
      !>C
      !>
      !>      DATA                 RADIUS / 0.D0, 1.D0,     1.D0,
      !>     .                              1.D0, 1.4142D0, 1.D0,
      !>     .                              1.D0, 1.D0,     1.4142D0,
      !>     .                              1.D0, 0.D0               /
      !>
      !>      DATA                 LON   /  0.D0,    0.D0,  90.D0,
      !>     .                              0.D0,  180.D0, -90.D0,
      !>     .                              0.D0,   45.D0, 180.D0,
      !>     .                             180.D0,  33.D0            /
      !>
      !>      DATA                 LAT   / 90.D0,    0.D0,   0.D0,
      !>     .                             90.D0,   45.D0,   0.D0,
      !>     .                            -90.D0,    0.D0, -45.D0,
      !>     .                             90.D0,    0.D0            /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '  RADIUS    LON      LAT   '
      !>     . //        '    R       CLON      Z    '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RLON = LON(I) * RPD()
      !>         RLAT = LAT(I) * RPD()
      !>
      !>         CALL LATCYL( RADIUS(I), RLON, RLAT, R, CLON, Z )
      !>
      !>         WRITE (*,'(6F9.3)') RADIUS(I), LON(I), LAT(I),
      !>     .                       R, CLON * DPR(), Z
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>           RADIUS    LON      LAT       R       CLON      Z
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000   90.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000    0.000    1.000   90.000    0.000
      !>            1.000    0.000   90.000    0.000    0.000    1.000
      !>            1.414  180.000   45.000    1.000  180.000    1.000
      !>            1.000  -90.000    0.000    1.000  -90.000    0.000
      !>            1.000    0.000  -90.000    0.000    0.000   -1.000
      !>            1.000   45.000    0.000    1.000   45.000    0.000
      !>            1.414  180.000  -45.000    1.000  180.000   -1.000
      !>            1.000  180.000   90.000    0.000  180.000    1.000
      !>            0.000   33.000    0.000    0.000   33.000    0.000
      !>
      !>     3) Other than the obvious conversion between coordinate systems
      !>        this routine could be used to obtain the axial projection
      !>        from a sphere to a cylinder about the z-axis that contains
      !>        the equator of the sphere.
      !>
      !>        Such a projection is valuable because it preserves the
      !>        areas between regions on the sphere and their projections to
      !>        the cylinder.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATCYL_EX3
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,F23.11)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Define the point whose projection is to be
      !>C     computed.
      !>C
      !>      RADIUS =  100.D0
      !>      LON    =   45.D0  * RPD()
      !>      LAT    =  -12.5D0 * RPD()
      !>
      !>C
      !>C     Convert the latitudinal coordinates to cylindrical.
      !>C
      !>      CALL LATCYL ( RADIUS, LON, LAT, R, CLON, Z )
      !>
      !>      WRITE(*,*) 'Coordinates of the projected point on '
      !>     .        // 'cylinder:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', R
      !>      WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z          (km): ', Z
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Coordinates of the projected point on cylinder:
      !>
      !>          Radius     (km):          97.62960071199
      !>          Longitude (deg):          45.00000000000
      !>          Z          (km):         -21.64396139381
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the argument names LONG and LONGC to LON and CLON for
      !>        consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.2, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental subroutine latcyl ( radius, lon, lat, r, clon, z )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: radius  !! Distance of a point from the origin.
      real(wp), intent(in) :: lon  !! Angle of the point from the XZ plane in radians.
      real(wp), intent(in) :: lat  !! Angle of the point from the XY plane in radians.
      real(wp), intent(out) :: r  !! Distance of the point from the Z axis.
      real(wp), intent(out) :: clon  !! Angle of the point from the XZ plane in radians.
      real(wp), intent(out) :: z  !! Height of the point above the XY plane.

      real(wp) :: rh
      real(wp) :: zz

!
!     Convert to cylindrical, storing in temporary variables
!
      rh = radius * dcos(lat)
      zz = radius * dsin(lat)

!
!     Move the results to output variables.
!
      clon  = lon
      r     = rh
      z     = zz

      return
      end subroutine latcyl

      !> Convert from latitudinal coordinates to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RADIUS   is the distance of a point from the origin.
      !>
      !>     LON      is the Longitude of the input point. This is the
      !>              angle between the prime meridian and the meridian
      !>              containing the point. The direction of increasing
      !>              longitude is from the +X axis towards the +Y axis.
      !>
      !>              Longitude is measured in radians. On input, the
      !>              range of longitude is unrestricted.
      !>
      !>     LAT      is the latitude of the input point. This is the angle
      !>              from the XY plane of the ray from the origin through
      !>              the point.
      !>
      !>              Latitude is measured in radians. On input, the range
      !>              of latitude is unrestricted.
      !>
      !>## Detailed Output
      !>
      !>     RECTAN   are the rectangular coordinates of the input point.
      !>              RECTAN is a 3-vector.
      !>
      !>              The units associated with RECTAN are those
      !>              associated with the input RADIUS.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the rectangular coordinates of a point
      !>     whose position is input in latitudinal coordinates.
      !>
      !>     Latitudinal coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     above the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the latitudinal coordinates of the position of the
      !>        Moon as seen from the Earth, and convert them to rectangular
      !>        coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: latrec_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATREC_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'latrec_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to latitudinal
      !>C     coordinates.
      !>C
      !>      CALL RECLAT ( POS, RADIUS, LON, LAT )
      !>
      !>C
      !>C     Convert the latitudinal to rectangular coordinates.
      !>C
      !>
      !>      CALL LATREC ( RADIUS, LON, LAT, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR()
      !>      WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from LATREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius     (km):      403626.33912495
      !>          Longitude (deg):         -98.34959789
      !>          Latitude  (deg):         -18.26566077
      !>
      !>         Rectangular coordinates from LATREC:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of latitudinal coordinates
      !>        and the corresponding rectangular coordinates.
      !>
      !>        Corresponding latitudinal and rectangular coordinates are
      !>        listed to three decimal places. Input angles are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATREC_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      LAT    ( NREC )
      !>      DOUBLE PRECISION      LON    ( NREC )
      !>      DOUBLE PRECISION      RADIUS ( NREC )
      !>      DOUBLE PRECISION      RECTAN ( 3    )
      !>      DOUBLE PRECISION      RLAT
      !>      DOUBLE PRECISION      RLON
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input latitudinal coordinates. Angles in
      !>C     degrees.
      !>C
      !>
      !>      DATA                 RADIUS / 0.D0,    1.D0,      1.D0,
      !>     .                              1.D0,     1.D0,     1.D0,
      !>     .                              1.D0, 1.4142D0, 1.4142D0,
      !>     .                          1.4142D0, 1.732D0            /
      !>
      !>      DATA                 LON    / 0.D0,    0.D0,  90.D0,
      !>     .                              0.D0,  180.D0, -90.D0,
      !>     .                              0.D0,   45.D0,   0.D0,
      !>     .                             90.D0,   45.D0            /
      !>
      !>      DATA                 LAT    / 0.D0,     0.D0,   0.D0,
      !>     .                             90.D0,     0.D0,   0.D0,
      !>     .                            -90.D0,     0.D0,  45.D0,
      !>     .                             45.D0, 35.264D0           /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '  RADIUS    LON      LAT   '
      !>     . //        ' RECT(1)  RECT(2)  RECT(3) '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RLON = LON(I) * RPD()
      !>         RLAT = LAT(I) * RPD()
      !>
      !>         CALL LATREC( RADIUS(I), RLON, RLAT, RECTAN )
      !>
      !>         WRITE (*,'(6F9.3)') RADIUS(I), LON(I), LAT(I),
      !>     .                       RECTAN
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>           RADIUS    LON      LAT    RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000    0.000    0.000    1.000    0.000
      !>            1.000    0.000   90.000    0.000    0.000    1.000
      !>            1.000  180.000    0.000   -1.000    0.000    0.000
      !>            1.000  -90.000    0.000    0.000   -1.000    0.000
      !>            1.000    0.000  -90.000    0.000    0.000   -1.000
      !>            1.414   45.000    0.000    1.000    1.000    0.000
      !>            1.414    0.000   45.000    1.000    0.000    1.000
      !>            1.414   90.000   45.000    0.000    1.000    1.000
      !>            1.732   45.000   35.264    1.000    1.000    1.000
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
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the input argument name LONG to LON for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header changes were made to improve clarity. Some
      !>        minor header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine latrec ( radius, lon, lat, rectan )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: radius  !! Distance of a point from the origin.
      real(wp), intent(in) :: lon  !! Longitude of point in radians.
      real(wp), intent(in) :: lat  !! Latitude of point in radians.
      real(wp), intent(out) :: rectan ( 3 )  !! Rectangular coordinates of the point.

      real(wp) :: x
      real(wp) :: y
      real(wp) :: z

!
!     Convert to rectangular coordinates, storing the results in
!     temporary variables.
!
      x = radius * dcos(lon) * dcos(lat)
      y = radius * dsin(lon) * dcos(lat)
      z = radius * dsin(lat)

!
!  Move the results to the output variables.
!
      rectan(1) = x
      rectan(2) = y
      rectan(3) = z

      return
      end subroutine latrec

      !> Convert from latitudinal coordinates to spherical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RADIUS   is the distance of a point from the origin.
      !>
      !>     LON      is the angle of the point from the XZ plane in
      !>              radians.
      !>
      !>     LAT      is the angle of the point from the XY plane in
      !>              radians.
      !>
      !>## Detailed Output
      !>
      !>     RHO      is the distance of the point from the origin.
      !>
      !>     COLAT    is the angle between the vector from the origin to the
      !>              point and the positive Z axis in radians. COLAT is
      !>              computed as PI/2 - LAT.
      !>
      !>     SLON     is the angle of the point from the XZ plane (radians).
      !>              SLON is set equal to LON.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the spherical coordinates of a point
      !>     whose position is input in latitudinal coordinates.
      !>
      !>     Latitudinal coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     above the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>     Spherical coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     from the z-axis.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Co-latitude is obtained by subtracting latitude from HALFPI()
      !>        Radius and longitude mean the same thing in both latitudinal
      !>        and spherical coordinates. The table below lists LAT and
      !>        corresponding COLAT in terms of degrees.
      !>
      !>             LAT     COLAT
      !>            -----    -----
      !>               0        90
      !>              20        70
      !>              45        45
      !>             -30       120
      !>              90         0
      !>             -45       135
      !>
      !>     2) Compute the latitudinal coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to spherical and
      !>        rectangular coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: latsph_ex2.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATSPH_EX2
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      SLON
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'latsph_ex2.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to latitudinal
      !>C     coordinates.
      !>C
      !>      CALL RECLAT ( POS, RADIUS, LON, LAT )
      !>
      !>C
      !>C     Convert the latitudinal coordinates to spherical.
      !>C
      !>      CALL LATSPH ( RADIUS, LON, LAT, R, COLAT, SLON )
      !>
      !>C
      !>C     Convert the spherical coordinates to rectangular.
      !>C
      !>      CALL SPHREC ( R, COLAT, SLON, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', LON*DPR()
      !>      WRITE(*,FMT1) '  Latitude   (deg): ', LAT*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', R
      !>      WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from SPHREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Longitude  (deg):         -98.34959789
      !>          Latitude   (deg):         -18.26566077
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Colatitude (deg):         108.26566077
      !>          Longitude  (deg):         -98.34959789
      !>
      !>         Rectangular coordinates from SPHREC:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>     3) Create a table showing a variety of latitudinal coordinates
      !>        and the corresponding spherical coordinates.
      !>
      !>        Corresponding latitudinal and spherical coordinates are
      !>        listed to three decimal places. Input and output angles are
      !>        in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LATSPH_EX3
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      LAT    ( NREC )
      !>      DOUBLE PRECISION      LON    ( NREC )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS ( NREC )
      !>      DOUBLE PRECISION      RLAT
      !>      DOUBLE PRECISION      RLON
      !>      DOUBLE PRECISION      SLON
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input latitudinal coordinates. Angles in
      !>C     degrees.
      !>C
      !>
      !>      DATA                 RADIUS / 0.D0, 1.D0,     1.D0,
      !>     .                              1.D0, 1.4142D0, 1.D0,
      !>     .                              1.D0, 1.D0,     1.4142D0,
      !>     .                              1.D0, 0.D0               /
      !>
      !>      DATA                 LON   /  0.D0,    0.D0,  90.D0,
      !>     .                              0.D0,  180.D0, -90.D0,
      !>     .                              0.D0,   45.D0, 180.D0,
      !>     .                             180.D0,  33.D0            /
      !>
      !>      DATA                 LAT   / 90.D0,    0.D0,   0.D0,
      !>     .                             90.D0,   45.D0,   0.D0,
      !>     .                            -90.D0,    0.D0, -45.D0,
      !>     .                             90.D0,    0.D0            /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '  RADIUS    LON      LAT   '
      !>     . //        '     R     COLAT     SLON  '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RLON = LON(I) * RPD()
      !>         RLAT = LAT(I) * RPD()
      !>
      !>         CALL LATSPH( RADIUS(I), RLON, RLAT, R, COLAT, SLON )
      !>
      !>         WRITE (*,'(6F9.3)') RADIUS(I), LON(I), LAT(I),
      !>     .                       R, COLAT * DPR(), SLON * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>           RADIUS    LON      LAT        R     COLAT     SLON
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000   90.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000   90.000    0.000
      !>            1.000   90.000    0.000    1.000   90.000   90.000
      !>            1.000    0.000   90.000    1.000    0.000    0.000
      !>            1.414  180.000   45.000    1.414   45.000  180.000
      !>            1.000  -90.000    0.000    1.000   90.000  -90.000
      !>            1.000    0.000  -90.000    1.000  180.000    0.000
      !>            1.000   45.000    0.000    1.000   90.000   45.000
      !>            1.414  180.000  -45.000    1.414  135.000  180.000
      !>            1.000  180.000   90.000    1.000    0.000  180.000
      !>            0.000   33.000    0.000    0.000   90.000   33.000
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Changed the argument names LONG and LONGS to LON and SLON for
      !>        consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.2, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental subroutine latsph ( radius, lon, lat,  rho, colat, slon )
      use pepper_kinds, only: wp
      use pepper_constants, only: halfpi
      implicit none

      real(wp), intent(in) :: radius  !! Distance of a point from the origin.
      real(wp), intent(in) :: lon  !! Angle of the point from the XZ plane in radians.
      real(wp), intent(in) :: lat  !! Angle of the point from the XY plane in radians.
      real(wp), intent(out) :: rho  !! Distance of the point from the origin.
      real(wp), intent(out) :: colat  !! Angle of the point from positive Z axis (radians).
      real(wp), intent(out) :: slon  !! Angle of the point from the XZ plane (radians).

      real(wp) :: th
      real(wp) :: ph

!
!     Convert to spherical coordinates, storing the results in
!     temporary variables
!
      th = halfpi - lat
      ph = lon

!
!     Move results to output variables
!
      rho   = radius
      colat = th
      slon  = ph

      return
      end subroutine latsph

      !> Set one double precision 3x3 matrix equal to another.
      !>
      !>## Detailed Input
      !>
      !>     M1       is an arbitrary input 3x3 matrix. There are no
      !>              restrictions on what it may contain.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a 3x3 matrix set to be equal to M1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
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
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) This trivial example demonstrates how to use MEQU to assign
      !>        one matrix to another.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MEQU_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 3, 3 )
      !>      DOUBLE PRECISION      MOUT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1.
      !>C
      !>      DATA                  M1   /  0.0D0,  1.0D0,  0.0D0,
      !>     .                             -1.0D0,  0.0D0,  0.0D0,
      !>     .                              0.0D0,  0.0D0,  1.0D0  /
      !>
      !>C
      !>C     Assign M1 to MOUT and print MOUT.
      !>C
      !>      CALL MEQU ( M1, MOUT )
      !>
      !>      WRITE(*,'(A)') 'MOUT:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        MOUT:
      !>               0.0000000      -1.0000000       0.0000000
      !>               1.0000000       0.0000000       0.0000000
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mequ ( m1, mout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: m1   ( 3, 3 )  !! Input matrix.
      real(wp), intent(out) :: mout ( 3, 3 )  !! Output matrix equal to M1.

      mout = m1

      return
      end subroutine mequ

      !> Set one double precision matrix of arbitrary size equal to
      !> another.
      !>
      !>## Detailed Input
      !>
      !>     M1       is an arbitrary-sized double precision matrix.
      !>              There are no restrictions on what it may contain.
      !>
      !>     NR       is the number of rows in the input matrix.
      !>
      !>     NC       is the number of columns in the input matrix.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a NRxNC matrix set to be equal to M1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NR < 1 or NC < 1, the elements of the matrix MOUT are not
      !>         assigned any values.
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
      !>     If  M1 = | 1.0D0   2.0D0 |
      !>              |               |
      !>              | 2.0D0   4.0D0 |
      !>              |               |
      !>              | 4.0D0   6.0D0 |
      !>
      !>     the call
      !>
      !>     CALL MEQUG ( M1, 3, 2, MOUT )
      !>
      !>     produces the matrix
      !>
      !>       MOUT = | 1.0D0   2.0D0 |
      !>              |               |
      !>              | 2.0D0   4.0D0 |
      !>              |               |
      !>              | 4.0D0   6.0D0 |
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mequg ( m1, nr, nc, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nr  !! Row dimension of M1 (and also MOUT).
      integer(ip), intent(in) :: nc  !! Column dimension of M1 (and also MOUT).
      real(wp), intent(in) :: m1   ( nr,nc )  !! Input matrix.
      real(wp), intent(out) :: mout ( nr,nc )  !! Output matrix equal to M1.

      mout = m1
      return
      end subroutine mequg

      !> Multiply the transpose of a 3x3 matrix and a 3x3 matrix.
      !>
      !>## Detailed Input
      !>
      !>     M1       is any 3x3 double precision matrix. Typically,
      !>              M1 will be a rotation matrix since then its
      !>              transpose is its inverse (but this is NOT a
      !>              requirement).
      !>
      !>     M2       is any 3x3 double precision matrix.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is s 3x3 double precision matrix. MOUT is the
      !>              product MOUT = (M1**T) x M2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the subscripts I and J from 1 to 3:
      !>
      !>        MOUT(I,J) = Summation from K=1 to 3 of  ( M1(K,I) * M2(K,J) )
      !>
      !>     Note that the reversal of the K and I subscripts in the left-hand
      !>     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
      !>     and not simply of M1 itself.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given two 3x3 matrices, multiply the transpose of the first
      !>        matrix by the second one.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MTXM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 3, 3 )
      !>      DOUBLE PRECISION      M2   ( 3, 3 )
      !>      DOUBLE PRECISION      MOUT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1 and M2.
      !>C
      !>      DATA                  M1 /  1.0D0,  4.0D0,  7.0D0,
      !>     .                            2.0D0,  5.0D0,  8.0D0,
      !>     .                            3.0D0,  6.0D0,  9.0D0  /
      !>
      !>      DATA                  M2 /  1.0D0, -1.0D0,  0.0D0,
      !>     .                            1.0D0,  1.0D0,  0.0D0,
      !>     .                            0.0D0,  0.0D0,  1.0D0  /
      !>
      !>C
      !>C     Multiply the transpose of M1 by M2.
      !>C
      !>      CALL MTXM ( M1, M2, MOUT )
      !>
      !>      WRITE(*,'(A)') 'Transpose of M1 times M2:'
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Transpose of M1 times M2:
      !>            -3.000     5.000     7.000
      !>            -3.000     7.000     8.000
      !>            -3.000     9.000     9.000
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M1 and M2 so that a floating point overflow does
      !>         not occur. (In the typical use where M1 and M2 are rotation
      !>         matrices, this not a risk at all.)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mtxm ( m1, m2, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: m1   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(in) :: m2   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(out) :: mout ( 3,3 )  !! 3x3 double precision matrix which is the product
                                             !! (M1**T) * M2.

      real(wp) :: prodm(3,3)

      integer(ip) :: i
      integer(ip) :: j

!
!  Perform the matrix multiplication
!
      do   i=1,3
         do   j=1,3
            prodm(i,j) = m1(1,i)*m2(1,j) &
                       + m1(2,i)*m2(2,j) &
                       + m1(3,i)*m2(3,j)
         end do
      end do

!
!  Move the result into MOUT
!
      mout = prodm

      return
      end subroutine mtxm

      !> Multiply the transpose of a matrix with another matrix,
      !> both of arbitrary size. (The dimensions of the matrices must be
      !> compatible with this multiplication.)
      !>
      !>## Detailed Input
      !>
      !>     M1       is an double precision matrix of arbitrary dimension
      !>              whose transpose is the left hand multiplier of a
      !>              matrix multiplication.
      !>
      !>     M2       is an double precision matrix of arbitrary dimension
      !>              whose transpose is the left hand multiplier of a
      !>              matrix multiplication.
      !>
      !>     NC1      is the column dimension of M1 and row dimension of
      !>              MOUT.
      !>
      !>     NR1R2    is the row dimension of both M1 and M2.
      !>
      !>     NC2      is the column dimension of both M2 and MOUT.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a double precision matrix containing the product
      !>
      !>                           T
      !>                 MOUT =  M1  x  M2
      !>
      !>              where the superscript T denotes the transpose of M1.
      !>
      !>              MOUT must NOT overwrite either M1 or M2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NR1R2 < 1, the elements of the matrix MOUT are set equal to
      !>         zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>     For each value of the subscript I from 1 to NC1, and J from 1
      !>     to NC2:
      !>
      !>     MOUT(I,J) = Summation from K=1 to NR1R2 of  ( M1(K,I) * M2(K,J) )
      !>
      !>     Note that the reversal of the K and I subscripts in the left-hand
      !>     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
      !>     and not simply of M1 itself.
      !>
      !>     Since this subroutine operates on matrices of arbitrary size, it
      !>     is not possible to buffer intermediate results. Thus, MOUT
      !>     should NOT overwrite either M1 or M2.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 2x4 and a 2x3 matrices, multiply the transpose of the
      !>        first matrix by the second one.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MTXMG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 4, 2 )
      !>      DOUBLE PRECISION      M2   ( 2, 3 )
      !>      DOUBLE PRECISION      MOUT ( 4, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1 and M2.
      !>C
      !>      DATA                  M1 /  1.0D0,  1.0D0,
      !>     .                            2.0D0,  1.0D0,
      !>     .                            3.0D0,  1.0D0,
      !>     .                            0.0D0,  1.0D0  /
      !>
      !>      DATA                  M2 /  1.0D0,  0.0D0,
      !>     .                            2.0D0,  0.0D0,
      !>     .                            3.0D0,  0.0D0  /
      !>
      !>C
      !>C     Multiply the transpose of M1 by M2.
      !>C
      !>      CALL MTXMG ( M1, M2, 4, 2, 3, MOUT )
      !>
      !>      WRITE(*,'(A)') 'Transpose of M1 times M2:'
      !>      DO I = 1, 4
      !>
      !>         WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Transpose of M1 times M2:
      !>             1.000     2.000     3.000
      !>             2.000     4.000     6.000
      !>             3.000     6.000     9.000
      !>             0.000     0.000     0.000
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M1 and M2 so that a floating point overflow does
      !>         not occur.
      !>
      !>     2)  MOUT must not overwrite M1 or M2 or else the intermediate
      !>         will affect the final result.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example based on the existing example.
      !>
      !>        Added entry #1 to $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mtxmg ( m1, m2, nc1, nr1r2, nc2, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nc1  !! Column dimension of M1 and row dimension of MOUT.
      integer(ip), intent(in) :: nr1r2  !! Row dimension of both M1 and M2.
      integer(ip), intent(in) :: nc2  !! Column dimension of both M2 and MOUT.
      real(wp), intent(in) :: m1   ( nr1r2,nc1 )  !! Left-hand matrix whose transpose is to be
                                                  !! multiplied.
      real(wp), intent(in) :: m2   ( nr1r2,nc2 )  !! Right-hand matrix to be multiplied.
      real(wp), intent(out) :: mout ( nc1,  nc2 )  !! Product matrix M1**T * M2.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k

!
!  Perform the matrix multiplication
!
      do i = 1, nc1
         do j = 1, nc2

            mout(i,j) = 0.0_wp
            do k = 1, nr1r2
               mout(i,j) = mout(i,j) + m1(k,i)*m2(k,j)
            end do

         end do
      end do
      return
      end subroutine mtxmg

      !> Multiply the transpose of a 3x3 matrix on the left with a vector
      !> on the right.
      !>
      !>## Detailed Input
      !>
      !>     M        is an arbitrary 3x3 double precision matrix.
      !>              Typically, M will be a rotation matrix since
      !>              then its transpose is its inverse (but this is NOT
      !>              a requirement).
      !>
      !>     VIN      is an arbitrary 3-dimensional double precision
      !>              vector.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a 3-dimensional double precision vector. VOUT is
      !>              the product VOUT = (M**T)  x (VIN).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the subscript I from 1 to 3:
      !>
      !>                        3
      !>                     .-----
      !>                      \
      !>           VOUT(I) =   )  M(K,I) * VIN(K)
      !>                      /
      !>                     '-----
      !>                       K=1
      !>
      !>     Note that the reversal of the K and I subscripts in the left-hand
      !>     matrix M is what makes VOUT the product of the TRANSPOSE of
      !>     and not simply of M itself.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x3 matrix and a 3-vector, multiply the transpose of
      !>        the matrix by the vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MTXV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M    ( 3, 3 )
      !>      DOUBLE PRECISION      VIN  ( 3    )
      !>      DOUBLE PRECISION      VOUT ( 3    )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M and VIN.
      !>C
      !>      DATA                  M    /  1.0D0, -1.0D0,  0.0D0,
      !>     .                              1.0D0,  1.0D0,  0.0D0,
      !>     .                              0.0D0,  0.0D0,  1.0D0  /
      !>
      !>      DATA                  VIN  /  5.0D0, 10.0D0, 15.0D0  /
      !>
      !>C
      !>C     Multiply the transpose of M by VIN.
      !>C
      !>      CALL MTXV ( M, VIN, VOUT )
      !>
      !>      WRITE(*,'(A)') 'Transpose of M times VIN:'
      !>      WRITE(*,'(3F10.3)') VOUT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Transpose of M times VIN:
      !>            -5.000    15.000    15.000
      !>
      !>        Note that typically the matrix M will be a rotation matrix.
      !>        Because the transpose of an orthogonal matrix is equivalent to
      !>        its inverse, applying the rotation to the vector is
      !>        accomplished by multiplying the vector by the transpose of the
      !>        matrix.
      !>
      !>        Let
      !>
      !>               -1
      !>              M   * VIN = VOUT
      !>
      !>        If M is an orthogonal matrix, then (M**T) * VIN = VOUT.
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M and VIN so that a floating point overflow does
      !>         not occur.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 25-AUG-2021 (JDR)
      !>
      !>        Changed input argument name MATRIX to M for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mtxv ( m, vin, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: m      ( 3,3 )  !! 3X3 double precision matrix.
      real(wp), intent(in) :: vin    (   3 )  !! 3-dimensional double precision vector.
      real(wp), intent(out) :: vout   (   3 )  !! 3-dimensional double precision vector. VOUT is the
                                               !! product M**T * VIN.

      real(wp) :: prodv  ( 3 )
      integer(ip) :: i

!
!  Perform the matrix-vector multiplication
!
      do i = 1, 3
        prodv(i) = m(1,i)*vin(1) + &
                   m(2,i)*vin(2) + &
                   m(3,i)*vin(3)
      end do

!
!  Move the result into VOUT
!
      vout(1) = prodv(1)
      vout(2) = prodv(2)
      vout(3) = prodv(3)

      return
      end subroutine mtxv

      !> Multiply the transpose of a matrix and a vector of
      !> arbitrary size.
      !>
      !>## Detailed Input
      !>
      !>     M1       is a double precision matrix of arbitrary size whose
      !>              transpose forms the left-hand matrix of the
      !>              multiplication.
      !>
      !>     V2       is a double precision vector on the right of the
      !>              multiplication.
      !>
      !>     NC1      is the column dimension of M1 and length of VOUT.
      !>
      !>     NR1R2    is the row dimension of M1 and length of V2.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the double precision vector which results from
      !>              the expression
      !>
      !>                            T
      !>                 VOUT = (M1)  x V2
      !>
      !>              where the T denotes the transpose of M1.
      !>
      !>              VOUT must NOT overwrite either M1 or V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>     For each value of the subscript I from 1 to NC1,
      !>
      !>     VOUT(I) = Summation from K=1 to NR1R2 of  ( M1(K,I) * V2(K) )
      !>
      !>     Note that the reversal of the K and I subscripts in the left-hand
      !>     matrix M1 is what makes VOUT the product of the TRANSPOSE of M1
      !>     and not simply of M1 itself.
      !>
      !>     Since this subroutine operates on matrices of arbitrary size, it
      !>     is not feasible to buffer intermediate results. Thus, VOUT
      !>     should NOT overwrite either M1 or V2.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x2 matrix and a 3-vector, multiply the transpose of
      !>        the matrix by the vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MTXVG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M    ( 3, 2 )
      !>      DOUBLE PRECISION      VIN  ( 3    )
      !>      DOUBLE PRECISION      VOUT ( 2    )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M and VIN.
      !>C
      !>      DATA                  M    /  1.0D0,  1.0D0,  1.0D0,
      !>     .                              2.0D0,  3.0D0,  4.0D0  /
      !>
      !>      DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
      !>
      !>C
      !>C     Multiply the transpose of M by VIN.
      !>C
      !>      CALL MTXVG ( M, VIN, 2, 3, VOUT )
      !>
      !>      WRITE(*,'(A)') 'Transpose of M times VIN:'
      !>      WRITE(*,'(2F10.3)') VOUT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Transpose of M times VIN:
      !>             6.000    20.000
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M1 and V2 so that a floating point overflow does
      !>         not occur.
      !>
      !>     2)  VOUT not overwrite M1 or V2 or else the intermediate
      !>         will affect the final result.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mtxvg ( m1, v2, nc1, nr1r2, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nc1  !! Column dimension of M1 and length of VOUT.
      integer(ip), intent(in) :: nr1r2  !! Row dimension of M1 and length of V2.
      real(wp), intent(in) :: m1   ( nr1r2,nc1 )  !! Left-hand matrix whose transpose is to be
                                                  !! multiplied.
      real(wp), intent(in) :: v2   ( nr1r2     )  !! Right-hand vector to be multiplied.
      real(wp), intent(out) :: vout (       nc1 )  !! Product vector M1**T * V2.

      real(wp) :: sum
      integer(ip) :: i,k

!
!  Perform the matrix-vector multiplication
!
      do i = 1, nc1

         sum = 0.0_wp

         do k = 1, nr1r2
            sum = sum + m1(k,i)*v2(k)
         end do

         vout(i) = sum

      end do

      return
      end subroutine mtxvg

      !> Multiply two 3x3 matrices.
      !>
      !>## Detailed Input
      !>
      !>     M1       is an arbitrary 3x3 double precision matrix.
      !>
      !>     M2       is an arbitrary 3x3 double precision matrix.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a 3x3 double precision matrix. MOUT is the product
      !>              M1*M2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the subscripts I and J from 1 to 3:
      !>
      !>                          3
      !>                       .-----
      !>                        \
      !>           MOUT(I,J) =   )  M1(I,K) * M2(K,J)
      !>                        /
      !>                       '-----
      !>                         K=1
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given two 3x3 double precision matrices, compute their
      !>        product.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MXM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 3, 3 )
      !>      DOUBLE PRECISION      M2   ( 3, 3 )
      !>      DOUBLE PRECISION      MOUT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1 and M2.
      !>C
      !>      DATA                  M1   /  1.0D0, -1.0D0,  0.0D0,
      !>     .                              1.0D0,  1.0D0,  0.0D0,
      !>     .                              0.0D0,  0.0D0,  1.0D0  /
      !>
      !>      DATA                  M2   /  1.0D0,  0.0D0,  0.0D0,
      !>     .                              0.0D0,  1.0D0, -1.0D0,
      !>     .                              0.0D0,  1.0D0,  1.0D0  /
      !>
      !>C
      !>C     Compute M1 times M2.
      !>C
      !>      CALL MXM ( M1, M2, MOUT )
      !>
      !>      WRITE(*,'(A)') 'M1:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( M1(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'M2:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( M2(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'M1 times M2:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        M1:
      !>               1.0000000       1.0000000       0.0000000
      !>              -1.0000000       1.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>        M2:
      !>               1.0000000       0.0000000       0.0000000
      !>               0.0000000       1.0000000       1.0000000
      !>               0.0000000      -1.0000000       1.0000000
      !>
      !>        M1 times M2:
      !>               1.0000000       1.0000000       1.0000000
      !>              -1.0000000       1.0000000       1.0000000
      !>               0.0000000      -1.0000000       1.0000000
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mxm ( m1, m2, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: m1   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(in) :: m2   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(out) :: mout ( 3,3 )  !! The 3x3 double precision matrix product M1*M2.

      real(wp) :: prodm( 3, 3 )

      integer(ip) :: i
      integer(ip) :: j

!
!  Perform the matrix multiplication
!
      do i=1,3
         do j=1,3
            prodm(i,j) = m1(i,1)*m2(1,j) &
                       + m1(i,2)*m2(2,j) &
                       + m1(i,3)*m2(3,j)
         end do
      end do
!
!  Move the result into MOUT
!
      mout = prodm

      return
      end subroutine mxm

      !> Multiply two double precision matrices of arbitrary size.
      !>
      !>## Detailed Input
      !>
      !>     M1       is any double precision matrix of arbitrary size.
      !>
      !>     M2       is any double precision matrix of arbitrary size.
      !>              The number of rows in M2 must match the number of
      !>              columns in M1.
      !>
      !>     NR1      is the number of rows in both M1 and MOUT.
      !>
      !>     NC1R2    is the number of columns in M1 and (by necessity)
      !>              the number of rows of M2.
      !>
      !>     NC2      is the number of columns in both M2 and MOUT.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a a double precision matrix of dimension
      !>              NR1 x NC2. MOUT is the product matrix given
      !>              by MOUT = (M1) x (M2). MOUT must not overwrite
      !>              M1 or M2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NC1R2 < 1, the elements of the matrix MOUT are set equal to
      !>         zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>     For each value of the subscript I from 1 to NC1, and J from 1
      !>     to NC2:
      !>
      !>        MOUT(I,J) = Summation from K=1 to NC1R2 of ( M1(I,K) * M2(K,J)
      !>
      !>     Since this subroutine operates on matrices of arbitrary size, it
      !>     is not feasible to buffer intermediate results. Thus, MOUT
      !>     should NOT overwrite either M1 or M2.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x2 and a 2x3 matrices, multiply the first matrix by
      !>        the second one.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MXMG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 3, 2 )
      !>      DOUBLE PRECISION      M2   ( 2, 3 )
      !>      DOUBLE PRECISION      MOUT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1 and M2.
      !>C
      !>      DATA                  M1 /  1.0D0,  2.0D0, 3.0D0,
      !>     .                            4.0D0,  5.0D0, 6.0D0  /
      !>
      !>      DATA                  M2 /  1.0D0,  2.0D0,
      !>     .                            3.0D0,  4.0D0,
      !>     .                            5.0D0,  6.0D0  /
      !>
      !>C
      !>C     Multiply M1 by M2.
      !>C
      !>      CALL MXMG ( M1, M2, 3, 2, 3, MOUT )
      !>
      !>      WRITE(*,'(A)') 'M1 times M2:'
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        M1 times M2:
      !>             9.000    19.000    29.000
      !>            12.000    26.000    40.000
      !>            15.000    33.000    51.000
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to prevent numeric overflow or
      !>         underflow.
      !>
      !>     2)  No error checking performed to determine if the input and
      !>         output matrices have, in fact, been correctly dimensioned.
      !>
      !>     3)  MOUT should not overwrite M1 or M2.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Changed input argument names ROW1, COL1 and COL2 to NR1, NC1R2
      !>        and NC2 for consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mxmg ( m1, m2, nr1, nc1r2, nc2, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nr1  !! Row dimension of M1 (and also MOUT).
      integer(ip), intent(in) :: nc1r2  !! Column dimension of M1 and row dimension of M2.
      integer(ip), intent(in) :: nc2  !! Column dimension of M2 (and also MOUT).
      real(wp), intent(in) :: m1   ( nr1,   nc1r2 )  !! NR1   x NC1R2 double precision matrix.
      real(wp), intent(in) :: m2   ( nc1r2, nc2   )  !! NC1R2 x NC2   double precision matrix.
      real(wp), intent(out) :: mout ( nr1,   nc2   )  !! NR1 x NC2 double precision matrix.

      real(wp) :: sum

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k

!
!  Perform the matrix multiplication
!
      do i = 1, nr1
         do j = 1, nc2
            sum = 0.0_wp
            do k = 1, nc1r2
               sum = sum + m1(i,k)*m2(k,j)
            end do
            mout(i,j) = sum
         end do
      end do
      return
      end subroutine mxmg

      !> Multiply a 3x3 matrix and the transpose of another 3x3 matrix.
      !>
      !>## Detailed Input
      !>
      !>     M1       is an arbitrary 3x3 double precision matrix.
      !>
      !>     M2       is an arbitrary 3x3 double precision matrix.
      !>              Typically, M2 will be a rotation matrix since
      !>              then its transpose is its inverse (but this is
      !>              NOT a requirement).
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a 3x3 double precision matrix. MOUT is the product
      !>
      !>                               T
      !>                 MOUT = M1 x M2
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the subscripts I and J from 1 to 3:
      !>
      !>                          3
      !>                       .-----
      !>                        \
      !>           MOUT(I,J) =   )  M1(I,K) * M2(J,K)
      !>                        /
      !>                       '-----
      !>                         K=1
      !>
      !>     Note that the reversal of the K and J subscripts in the right-
      !>     hand matrix M2 is what makes MOUT the product of the TRANSPOSE of
      !>     M2 and not simply of M2 itself.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given two 3x3 double precision matrices, multiply the first
      !>        matrix by the transpose of the second one.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MXMT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 3, 3 )
      !>      DOUBLE PRECISION      M2   ( 3, 3 )
      !>      DOUBLE PRECISION      MOUT ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1.
      !>C
      !>      DATA                  M1   /  0.0D0, -1.0D0,  0.0D0,
      !>     .                              1.0D0,  0.0D0,  0.0D0,
      !>     .                              0.0D0,  0.0D0,  1.0D0  /
      !>
      !>C
      !>C     Make M2 equal to M1.
      !>C
      !>      CALL MEQU ( M1, M2 )
      !>
      !>C
      !>C     Multiply M1 by the transpose of M2.
      !>C
      !>      CALL MXMT ( M1, M2, MOUT )
      !>
      !>      WRITE(*,'(A)') 'M1:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( M1(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'M2:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( M2(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'M1 times transpose of M2:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        M1:
      !>               0.0000000       1.0000000       0.0000000
      !>              -1.0000000       0.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>        M2:
      !>               0.0000000       1.0000000       0.0000000
      !>              -1.0000000       0.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>        M1 times transpose of M2:
      !>               1.0000000       0.0000000       0.0000000
      !>               0.0000000       1.0000000       0.0000000
      !>               0.0000000       0.0000000       1.0000000
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M1 and M2 so that a floating point overflow does
      !>         not occur. (In the typical use where M1 and M2 are rotation
      !>         matrices, this not a risk at all.)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples based on existing code fragments.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mxmt ( m1, m2, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: m1   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(in) :: m2   ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(out) :: mout ( 3,3 )  !! The product M1 times transpose of M2.

      real(wp) :: prodm ( 3, 3 )

      integer(ip) :: i
      integer(ip) :: j

!
!  Perform the matrix multiplication
!
      do i=1,3
         do j=1,3
            prodm(i,j) = m1(i,1)*m2(j,1) &
                       + m1(i,2)*m2(j,2) &
                       + m1(i,3)*m2(j,3)
         end do
      end do
!
!  Move the result into MOUT
!
      mout = prodm

      return
      end subroutine mxmt

      !> Multiply a matrix and the transpose of a matrix, both of
      !> arbitrary size.
      !>
      !>## Detailed Input
      !>
      !>     M1       is any double precision matrix of arbitrary size.
      !>
      !>     M2       is any double precision matrix of arbitrary size.
      !>
      !>              The number of columns in M2 must match the number of
      !>              columns in M1.
      !>
      !>     NR1      is the number of rows in both M1 and MOUT.
      !>
      !>     NC1C2    is the number of columns in M1 and (by necessity)
      !>              the number of columns of M2.
      !>
      !>     NR2      is the number of rows in both M2 and the number of
      !>              columns in MOUT.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a double precision matrix of dimension NR1 x NR2.
      !>
      !>              MOUT is the product matrix given by
      !>
      !>                               T
      !>                 MOUT = M1 x M2
      !>
      !>              where the superscript "T" denotes the transpose
      !>              matrix.
      !>
      !>              MOUT must not overwrite M1 or M2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>     For each value of the subscript I from 1 to NR1, and J from 1
      !>     to NR2:
      !>
      !>     MOUT(I,J) = Summation from K=1 to NC1C2 of  ( M1(I,K) * M2(J,K) )
      !>
      !>     Notice that the order of the subscripts of M2 are reversed from
      !>     what they would be if this routine merely multiplied M1 and M2.
      !>     It is this transposition of subscripts that makes this routine
      !>     multiply M1 and the TRANPOSE of M2.
      !>
      !>     Since this subroutine operates on matrices of arbitrary size, it
      !>     is not feasible to buffer intermediate results. Thus, MOUT
      !>     should NOT overwrite either M1 or M2.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 2x3 and a 3x4 matrices, multiply the first matrix by
      !>        the transpose of the second one.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MXMTG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1   ( 2, 3 )
      !>      DOUBLE PRECISION      M2   ( 4, 3 )
      !>      DOUBLE PRECISION      MOUT ( 2, 4 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M1 and M2.
      !>C
      !>      DATA                  M1 /  1.0D0, 3.0D0,
      !>     .                            2.0D0, 2.0D0,
      !>     .                            3.0D0, 1.0D0  /
      !>
      !>      DATA                  M2 /  1.0D0, 2.0D0, 1.0D0, 2.0D0,
      !>     .                            2.0D0, 1.0D0, 2.0D0, 1.0D0,
      !>     .                            0.0D0, 2.0D0, 0.0D0, 2.0D0 /
      !>
      !>C
      !>C     Multiply M1 by the transpose of M2.
      !>C
      !>      CALL MXMTG ( M1, M2, 2, 3, 4, MOUT )
      !>
      !>      WRITE(*,'(A)') 'M1 times transpose of M2:'
      !>      DO I = 1, 2
      !>
      !>         WRITE(*,'(4F10.3)') ( MOUT(I,J), J=1,4)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        M1 times transpose of M2:
      !>             5.000    10.000     5.000    10.000
      !>             7.000    10.000     7.000    10.000
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to prevent numeric overflow or
      !>         underflow.
      !>
      !>         The user is responsible for checking the magnitudes of the
      !>         elements of M1 and M2 so that a floating point overflow does
      !>         not occur.
      !>
      !>     2)  No error checking is performed to determine if the input and
      !>         output matrices have, in fact, been correctly dimensioned.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mxmtg ( m1, m2, nr1, nc1c2, nr2, mout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nr1  !! Row dimension of M1 and row dimension of MOUT.
      integer(ip), intent(in) :: nc1c2  !! Column dimension of M1 and column dimension of M2.
      integer(ip), intent(in) :: nr2  !! Row dimension of M2 and column dimension of MOUT.
      real(wp), intent(in) :: m1   ( nr1,nc1c2 )  !! Left-hand matrix to be multiplied.
      real(wp), intent(in) :: m2   ( nr2,nc1c2 )  !! Right-hand matrix whose transpose is to be
                                                  !! multiplied.
      real(wp), intent(out) :: mout ( nr1,nr2   )  !! Product matrix M1 * M2**T.

      real(wp) :: sum
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k

!
!  Perform the matrix multiplication
!
      do i = 1, nr1

         do j = 1, nr2
            sum = 0.0_wp

            do k = 1, nc1c2
               sum = sum + m1(i,k)*m2(j,k)
            end do

            mout(i,j) = sum

         end do

      end do

      return
      end subroutine mxmtg

      !> Multiply a 3x3 double precision matrix with a 3-dimensional
      !> double precision vector.
      !>
      !>## Detailed Input
      !>
      !>     M        is an arbitrary 3x3 double precision matrix.
      !>
      !>     VIN      is an arbitrary 3-dimensional double precision vector.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a 3-dimensional double precision vector. VOUT is
      !>              the product M * V.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the subscript I from 1 to 3:
      !>
      !>                        3
      !>                     .-----
      !>                      \
      !>           VOUT(I) =   )  M(I,K) * VIN(K)
      !>                      /
      !>                     '-----
      !>                       K=1
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x3 matrix and a 3-vector, multiply the matrix by
      !>        the vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MXV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M    ( 3, 3 )
      !>      DOUBLE PRECISION      VIN  ( 3    )
      !>      DOUBLE PRECISION      VOUT ( 3    )
      !>
      !>C
      !>C     Define M and VIN.
      !>C
      !>      DATA                  M    /  0.0D0, -1.0D0,  0.0D0,
      !>     .                              1.0D0,  0.0D0,  0.0D0,
      !>     .                              0.0D0,  0.0D0,  1.0D0  /
      !>
      !>      DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
      !>
      !>C
      !>C     Multiply M by VIN.
      !>C
      !>      CALL MXV ( M, VIN, VOUT )
      !>
      !>      WRITE(*,'(A)') 'M times VIN:'
      !>      WRITE(*,'(3F10.3)') VOUT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        M times VIN:
      !>             2.000    -1.000     3.000
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M and VIN so that a floating point overflow does
      !>         not occur.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on the existing example.
      !>
      !>        Changed input argument name MATRIX to M for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mxv ( m, vin, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: m      ( 3, 3 )  !! 3x3 double precision matrix.
      real(wp), intent(in) :: vin    (    3 )  !! 3-dimensional double precision vector.
      real(wp), intent(out) :: vout   (    3 )  !! 3-dimensional double precision vector. VOUT is
                                                !! the product M*VIN.

      real(wp) :: prodv  ( 3 )
      integer(ip) :: i

!
!  Perform the matrix-vector multiplication
!
      do i = 1, 3
         prodv(i) = m(i,1)*vin(1) + &
                    m(i,2)*vin(2) + &
                    m(i,3)*vin(3)
      end do
!
!  Move the buffered vector into the output vector VOUT.
!
      vout(1) = prodv(1)
      vout(2) = prodv(2)
      vout(3) = prodv(3)

      return
      end subroutine mxv

      !> Multiply a matrix and a vector of arbitrary size.
      !>
      !>## Detailed Input
      !>
      !>     M1       is a double precision matrix of arbitrary size which
      !>              forms the left-hand matrix of the multiplication.
      !>
      !>     V2       is a double precision vector on the right of the
      !>              multiplication.
      !>
      !>     NR1      is the row dimension of M1 and length of VOUT.
      !>
      !>     NC1R2    is the column dimension of M1 and length of V2.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the double precision vector which results from
      !>              the expression VOUT = (M1) x V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the subscript I from 1 to NR1,
      !>
      !>        VOUT(I) = Summation from K=1 to NC1R2 of  ( M1(I,K) * V2(K) )
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 2x3 matrix and a 3-vector, multiply the matrix by
      !>        the vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM MXVG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M    ( 2, 3 )
      !>      DOUBLE PRECISION      VIN  ( 3    )
      !>      DOUBLE PRECISION      VOUT ( 2    )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define M and VIN.
      !>C
      !>      DATA                  M    /  1.0D0,  2.0D0,
      !>     .                              1.0D0,  3.0D0,
      !>     .                              1.0D0,  4.0D0  /
      !>
      !>      DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
      !>
      !>C
      !>C     Multiply M by VIN.
      !>C
      !>      CALL MXVG ( M, VIN, 2, 3, VOUT )
      !>
      !>      WRITE(*,'(A)') 'M times VIN:'
      !>      WRITE(*,'(2F10.3)') VOUT
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        M times VIN:
      !>             6.000    20.000
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for checking the magnitudes of the
      !>         elements of M1 and V2 so that a floating point overflow does
      !>         not occur.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example based on the existing example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Re-ordered header sections and made minor formatting
      !>        changes.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine mxvg ( m1, v2, nr1, nc1r2, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nr1  !! Row dimension of M1 and length of VOUT.
      integer(ip), intent(in) :: nc1r2  !! Column dimension of M1 and length of V2.
      real(wp), intent(in) :: m1   ( nr1,nc1r2 )  !! Left-hand matrix to be multiplied.
      real(wp), intent(in) :: v2   (     nc1r2 )  !! Right-hand vector to be multiplied.
      real(wp), intent(out) :: vout ( nr1       )  !! Product vector M1*V2.

      real(wp) :: sum

      integer(ip) :: i
      integer(ip) :: k

!
!  Perform the matrix-vector multiplication
!
      do i = 1, nr1
         sum = 0.0_wp
         do k = 1, nc1r2
            sum = sum + m1(i,k)*v2(k)
         end do
         vout(i) = sum
      end do

      return
      end subroutine mxvg

      !> Determine whether an integer is odd.
      !>
      !> The function returns .TRUE. if IVAL is odd, otherwise .FALSE.
      !>
      !>## Detailed Input
      !>
      !>     IVAL     is the integer to be tested for oddness.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if IVAL is odd, .FALSE. if IVAL is
      !>     even.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Divide IVAL by two. If the remainder is one, IVAL is odd.
      !>
      !>## Examples
      !>
      !>     Let ENDPTS contain a series of endpoints,
      !>
      !>        a , b , ..., a , b
      !>         1   1        n   n
      !>
      !>     representing an ordered collection of disjoint intervals,
      !>
      !>        a   <  b   < a
      !>         i  -   i     i+1
      !>
      !>     The following code fragment uses ODD to determine whether
      !>     an arbitrary value X is contained in any of the intervals.
      !>
      !>        CONTAINED = .FALSE.
      !>
      !>        DO I = 1, N-1
      !>           IF ( X .GE. ENDPTS(I)  .AND.  X .LE. ENDPTS(I+1) ) THEN
      !>              CONTAINED = ( ODD ( I ) )
      !>           END IF
      !>        END DO
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Changed the input argument name "I" to "IVAL" for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS)
      !>
      !>        Fixed a few typos in the header.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      elemental logical function  odd ( ival )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: ival  !! The integer in question.

!
!     Self-explanatory.
!
      odd = ( mod (ival,2) /= 0 )

      return
      end function odd

      !> Return .TRUE. if two given double precision numbers have opposite
      !> signs.
      !>
      !> The function returns .TRUE. when the double precision numbers X
      !> and Y have opposite signs.
      !>
      !>## Detailed Input
      !>
      !>     X        is any double precision number.
      !>
      !>     Y        is any double precision number.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if one of the pair X,Y is positive and
      !>     the other is negative. If either of the two values is zero, OPSGND
      !>     will be .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the value:
      !>
      !>           (      (( X .GT. 0) .AND. (Y .LT. 0))
      !>             .OR. (( X .LT. 0) .AND. (Y .GT. 0)) )
      !>
      !>     This is a more stable value than
      !>
      !>           ( X*Y .LT. 0 )
      !>
      !>     Note that if either of the two values is zero, OPSGND will be
      !>     false.
      !>
      !>## Examples
      !>
      !>     This routine can be used whenever a decision depends upon two
      !>     Double Precision values having opposite signs.
      !>
      !>     IF ( OPSGND ( F(X1), F(X2) ) ) THEN
      !>           .
      !>           .
      !>        find a root of F lying between X1 and X2
      !>           .
      !>           .
      !>     ELSE
      !>           .
      !>           .
      !>        do something
      !>           .
      !>           .
      !>     END IF
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
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Extended
      !>        $Detailed_Output section to indicate the output value for
      !>        the case of either input being zero.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental logical function opsgnd  ( x, y )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: x  !! A double precision number
      real(wp), intent(in) :: y  !! A double precision number

      opsgnd = (      (( x > 0 ) .and. ( y < 0 )) &
                 .or. (( x < 0 ) .and. ( y > 0 )) &
               )

      return
      end function opsgnd

      !> Return .TRUE. if two given integer numbers have opposite signs.
      !>
      !> The function returns .TRUE. when the integer numbers X and Y have
      !> opposite signs.
      !>
      !>## Detailed Input
      !>
      !>     X        is any integer number.
      !>
      !>     Y        is any integer number.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if one of the pair X,Y is positive and
      !>     the other is negative. If either of the two values is zero, OPSGNI
      !>     will be .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the value:
      !>
      !>           (      (( X .GT. 0) .AND. (Y .LT. 0))
      !>             .OR. (( X .LT. 0) .AND. (Y .GT. 0)) )
      !>
      !>     This is a more stable value than
      !>
      !>           ( X*Y .LT. 0 )
      !>
      !>     Note that if either of the two values is zero, OPSGNI will be
      !>     false.
      !>
      !>## Examples
      !>
      !>     This routine can be used whenever a decision depends upon two
      !>     integer values having opposite signs.
      !>
      !>     IF ( OPSGNI ( F(X1), F(X2) ) ) THEN
      !>           .
      !>           .
      !>        find the value of F closest to zero.
      !>           .
      !>           .
      !>     ELSE
      !>           .
      !>           .
      !>        do something
      !>           .
      !>           .
      !>     END IF
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Extended
      !>        $Detailed_Output section to indicate the output value for
      !>        the case of either input being zero.
      !>
      !>-    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS)
      !>
      !>        Fixed cut-and-paste errors in the header.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental logical function opsgni  ( x, y )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: x  !! An integer.
      integer(ip), intent(in) :: y  !! An integer.

      opsgni = (      (( x > 0 ) .and. ( y < 0 )) &
                 .or. (( x < 0 ) .and. ( y > 0 )) &
               )

      return
      end function opsgni

      !> Return a unit normal vector and constant that define a specified
      !> plane.
      !>
      !> UBPL       P   SPICE plane upper bound.
      !>
      !>## Detailed Input
      !>
      !>     PLANE    is a SPICE plane.
      !>
      !>## Detailed Output
      !>
      !>     NORMAL,
      !>     KONST    are, respectively, a unit normal vector and
      !>              constant that define the geometric plane
      !>              represented by PLANE. Let the symbol < A, B >
      !>              indicate the inner product of vectors A and B;
      !>              then the geometric plane is the set of vectors X
      !>              in three-dimensional space that satisfy
      !>
      !>                 < X,  NORMAL >  =  KONST.
      !>
      !>              NORMAL is a unit vector. KONST is the distance of
      !>              the plane from the origin;
      !>
      !>                 KONST * NORMAL
      !>
      !>              is the closest point in the plane to the origin.
      !>
      !>## Parameters
      !>
      !>     UBPL     is the upper bound of a SPICE plane array.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  The input plane MUST have been created by one of the SPICELIB
      !>         routines
      !>
      !>            NVC2PL ( Normal vector and constant to plane )
      !>            NVP2PL ( Normal vector and point to plane    )
      !>            PSV2PL ( Point and spanning vectors to plane )
      !>
      !>         Otherwise, the results of this routine are unpredictable.
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
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Determine the distance of a plane from the origin, and
      !>        confirm the result by calculating the dot product (inner
      !>        product) of a vector from the origin to the plane and a
      !>        vector in that plane.
      !>
      !>        The dot product between these two vectors should be zero,
      !>        to double precision round-off, so orthogonal to that
      !>        precision.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PL2NVC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION        VDOT
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
      !>      DOUBLE PRECISION        DOTP
      !>      DOUBLE PRECISION        KONST
      !>      DOUBLE PRECISION        PLANE  ( UBPL )
      !>      DOUBLE PRECISION        NORMAL ( 3    )
      !>      DOUBLE PRECISION        PLNVEC ( 3    )
      !>      DOUBLE PRECISION        POINT  ( 3    )
      !>      DOUBLE PRECISION        VEC    ( 3    )
      !>
      !>C
      !>C     Define the plane with a vector normal to the plan
      !>C     and a point in the plane.
      !>C
      !>      DATA                    NORMAL / -1.D0,  5.D0,   -3.5D0 /
      !>      DATA                    POINT  /  9.D0, -0.65D0, -12.D0 /
      !>
      !>C
      !>C     Create the SPICE plane from the normal and point.
      !>C
      !>      CALL NVP2PL ( NORMAL, POINT, PLANE )
      !>
      !>C
      !>C     Calculate the normal vector and constant defining
      !>C     the plane. The constant value is the distance from
      !>C     the origin to the plane.
      !>C
      !>      CALL PL2NVC ( PLANE, NORMAL, KONST )
      !>      WRITE(*,'(A,F12.7)') 'Distance to the plane:', KONST
      !>
      !>C
      !>C     Confirm the results. Calculate a vector
      !>C     from the origin to the plane.
      !>C
      !>      CALL VSCL ( KONST, NORMAL, VEC )
      !>      WRITE(*,'(A,3F12.7)') 'Vector from origin   :', VEC
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Now calculate a vector in the plane from the
      !>C     location in the plane defined by VEC.
      !>C
      !>      CALL VSUB ( VEC, POINT, PLNVEC )
      !>
      !>C
      !>C     These vectors should be orthogonal.
      !>C
      !>      WRITE(*,'(A,F12.7)') 'dot product          :',
      !>     .                     VDOT( PLNVEC, VEC )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Distance to the plane:   4.8102899
      !>        Vector from origin   :  -0.7777778   3.8888889  -2.7222222
      !>
      !>        dot product          :  -0.0000000
      !>
      !>     2) Apply a linear transformation represented by a matrix to
      !>        a plane represented by a normal vector and a constant.
      !>
      !>        Find a normal vector and constant for the transformed plane.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PL2NVC_EX2
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
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR)
      !>
      !>        Changed the output argument name CONST to KONST for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples. Added documentation of the parameter UBPL.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      pure subroutine pl2nvc ( plane, normal, konst )
      use pepper_kinds, only: ip, wp
      implicit none


      real(wp), intent(in) :: plane  ( ubpl )  !! A SPICE plane.
      real(wp), intent(out) :: normal (    3 )  !! A normal vector and constant defining the
                                                !! geometric plane represented by PLANE.
      real(wp), intent(out) :: konst  !! A normal vector and constant defining the geometric plane
                                      !! represented by PLANE.

!
!     The contents of SPICE planes are as follows:
!
!        Elements NMLPOS through NMLPOS + 2 contain a unit normal
!        vector for the plane.
!
!        Element CONPOS contains a constant for the plane;  every point
!        X in the plane satisfies
!
!           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS).
!
!        The plane constant is the distance of the plane from the
!        origin; the normal vector, scaled by the constant, is the
!        closest point in the plane to the origin.
!
      integer(ip), parameter :: nmlpos = 1

      integer(ip), parameter :: conpos = 4

!
!     Note for programmers: validity of the input plane is not
!     checked in the interest of efficiency. The input plane will be
!     valid if it was created by one of the SPICE plane construction
!     routines.
!
!     Unpack the plane.
!
      normal = plane(nmlpos:nmlpos+3-1)
      konst   =   plane(conpos)

      return
      end subroutine pl2nvc

      !> Return a unit normal vector and point that define a specified
      !> plane.
      !>
      !>## Detailed Input
      !>
      !>     PLANE    is a SPICE plane.
      !>
      !>## Detailed Output
      !>
      !>     NORMAL,
      !>     POINT    are, respectively, a unit normal vector and point
      !>              that define the geometric plane represented by
      !>              PLANE. Let the symbol < A, B > indicate the inner
      !>              product of vectors A and B; then the geometric
      !>              plane is the set of vectors X in three-dimensional
      !>              space that satisfy
      !>
      !>                 < X - POINT, NORMAL >  =  0.
      !>
      !>              POINT is always the closest point in the input
      !>              plane to the origin. POINT is always a
      !>              non-negative scalar multiple of NORMAL.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  The input plane MUST have been created by one of the SPICELIB
      !>         routines
      !>
      !>            NVC2PL ( Normal vector and constant to plane )
      !>            NVP2PL ( Normal vector and point to plane    )
      !>            PSV2PL ( Point and spanning vectors to plane )
      !>
      !>         Otherwise, the results of this routine are unpredictable.
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
      !>## Examples
      !>
      !>     1)  Given a plane normal and constant, find a point in
      !>         the plane. POINT is the point we seek.
      !>
      !>            CALL NVC2PL ( NORMAL, CONST,  PLANE )
      !>            CALL PL2NVP ( PLANE,  NORMAL, POINT )
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
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR)
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
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      pure subroutine pl2nvp ( plane, normal, point )
      use pepper_kinds, only: ip, wp
      implicit none


      real(wp), intent(in) :: plane  ( ubpl )  !! A SPICE plane.
      real(wp), intent(out) :: normal (    3 )  !! A unit normal vector and point that define PLANE.
      real(wp), intent(out) :: point  (    3 )  !! A unit normal vector and point that define PLANE.

      real(wp) :: const

!
!     Note for programmers: validity of the input plane is not
!     checked in the interest of efficiency. The input plane will be
!     valid if it was created by one of the SPICE plane construction
!     routines.
!
!     Extract the unit normal and constant from the plane. Scaling the
!     unit normal by the constant gives us the closest point in the
!     plane to the origin.

      call pl2nvc ( plane, normal, const )
      point = const*normal

      return
      end subroutine pl2nvp

      !> Return a point and two orthogonal spanning vectors that generate
      !> a specified plane.
      !>
      !>## Detailed Input
      !>
      !>     PLANE    is a SPICE plane.
      !>
      !>## Detailed Output
      !>
      !>     POINT,
      !>     SPAN1,
      !>     SPAN2    are, respectively, a point and two orthogonal spanning
      !>              vectors that generate the geometric plane represented by
      !>              PLANE. The geometric plane is the set of vectors
      !>
      !>                 POINT   +   s * SPAN1   +   t * SPAN2
      !>
      !>              where `s' and `t' are real numbers. POINT is the closest
      !>              point in the plane to the origin; this point is always a
      !>              multiple of the plane's normal vector. SPAN1 and SPAN2
      !>              are an orthonormal pair of vectors. POINT, SPAN1, and
      !>              SPAN2 are mutually orthogonal.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  The input plane MUST have been created by one of the SPICELIB
      !>         routines
      !>
      !>            NVC2PL ( Normal vector and constant to plane )
      !>            NVP2PL ( Normal vector and point to plane    )
      !>            PSV2PL ( Point and spanning vectors to plane )
      !>
      !>         Otherwise, the results of this routine are unpredictable.
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
      !>## Examples
      !>
      !>     1)  Project a vector V orthogonally onto a plane defined by
      !>         POINT, SPAN1, and SPAN2. PROJ is the projection we want; it
      !>         is the closest vector in the plane to V.
      !>
      !>            CALL PSV2PL ( POINT,   SPAN1,    SPAN2,   PLANE )
      !>            CALL VPRJP  ( V,       PLANE,    PROJ           )
      !>
      !>     2)  Find the intersection of a plane and the unit sphere. This
      !>         is a geometry problem that arises in computing the
      !>         intersection of a plane and a triaxial ellipsoid. The
      !>         SPICELIB routine INEDPL computes this intersection, but this
      !>         example does illustrate how to use this routine.
      !>
      !>            C
      !>            C     The geometric plane of interest will be represented
      !>            C     by the SPICE plane PLANE in this example.
      !>            C
      !>            C     The intersection circle will be represented by the
      !>            C     vectors CENTER, V1, and V2; the circle is the set
      !>            C     of points
      !>            C
      !>            C        CENTER  +  cos(theta) V1  +  sin(theta) V2,
      !>            C
      !>            C     where theta is in the interval (-pi, pi].
      !>            C
      !>            C     The logical variable FOUND indicates whether the
      !>            C     intersection is non-empty.
      !>            C
      !>
      !>            C
      !>            C     The center of the intersection circle will be the
      !>            C     closest point in the plane to the origin. This
      !>            C     point is returned by PL2PSV. The distance of the
      !>            C     center from the origin is the norm of CENTER.
      !>            C
      !>                  CALL PL2PSV  ( PLANE, CENTER, SPAN1, SPAN2 )
      !>
      !>                  DIST = VNORM ( CENTER )
      !>
      !>            C
      !>            C     The radius of the intersection circle will be
      !>            C
      !>            C             ____________
      !>            C         _  /          2
      !>            C          \/  1 - DIST
      !>            C
      !>            C     since the radius of the circle, the distance of the
      !>            C     plane from the origin, and the radius of the sphere
      !>            C     (1) are the lengths of the sides of a right triangle.
      !>            C
      !>                  RADIUS = SQRT ( 1.0D0 - DIST**2 )
      !>
      !>                  CALL VSCL  ( RADIUS, SPAN1, V1 )
      !>                  CALL VSCL  ( RADIUS, SPAN2, V2 )
      !>
      !>                  FOUND = .TRUE.
      !>
      !>     3)  Apply a linear transformation represented by the matrix M to
      !>         a plane represented by the normal vector N and the constant C.
      !>         Find a normal vector and constant for the transformed plane.
      !>
      !>            C
      !>            C     Make a SPICE plane from N and C, and then find a
      !>            C     point in the plane and spanning vectors for the
      !>            C     plane. N need not be a unit vector.
      !>            C
      !>                  CALL NVC2PL ( N,      C,      PLANE         )
      !>                  CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 )
      !>
      !>            C
      !>            C     Apply the linear transformation to the point and
      !>            C     spanning vectors. All we need to do is multiply
      !>            C     these vectors by M, since for any linear
      !>            C     transformation T,
      !>            C
      !>            C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 )
      !>            C
      !>            C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2),
      !>            C
      !>            C     which means that T(POINT), T(SPAN1), and T(SPAN2)
      !>            C     are a point and spanning vectors for the transformed
      !>            C     plane.
      !>            C
      !>                  CALL MXV ( M, POINT, TPOINT )
      !>                  CALL MXV ( M, SPAN1, TSPAN1 )
      !>                  CALL MXV ( M, SPAN2, TSPAN2 )
      !>
      !>            C
      !>            C     Make a new SPICE plane TPLANE from the
      !>            C     transformed point and spanning vectors, and find a
      !>            C     unit normal and constant for this new plane.
      !>            C
      !>                  CALL PSV2PL ( TPOINT,  TSPAN1,  TSPAN2,  TPLANE )
      !>                  CALL PL2NVC ( TPLANE,  TN,      TC              )
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
      !>-    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR)
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
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
      pure subroutine pl2psv ( plane, point, span1, span2 )
      use pepper_kinds, only: ip, wp
      implicit none


      real(wp), intent(in) :: plane ( ubpl )  !! A SPICE plane.
      real(wp), intent(out) :: point (    3 )  !! A point in the input plane and two vectors
                                               !! spanning the input plane.
      real(wp), intent(out) :: span1 (    3 )  !! A point in the input plane and two vectors
                                               !! spanning the input plane.
      real(wp), intent(out) :: span2 (    3 )  !! A point in the input plane and two vectors
                                               !! spanning the input plane.

      real(wp) :: normal ( 3 )

!
!     Note for programmers: validity of the input plane is not
!     checked in the interest of efficiency. The input plane will be
!     valid if it was created by one of the SPICE plane construction
!     routines.
!
!     Find a unit normal vector for the plane, and find the closest
!     point in the plane to the origin.
!
      call pl2nvp ( plane, normal, point )

!
!     Next, find an orthogonal pair of vectors that are also
!     orthogonal to the PLANE's normal vector.  The SPICELIB routine
!     FRAME does this for us.  NORMAL, SPAN1, and SPAN2 form a
!     right-handed orthonormal system upon output from FRAME.
!
      call frame  ( normal, span1, span2 )

      return
      end subroutine pl2psv

      !> Expand a triangular plate by a specified amount. The expanded
      !> plate is co-planar with, and has the same orientation as, the
      !> original. The centroids of the two plates coincide.
      !>
      !>## Detailed Input
      !>
      !>     IVERTS   is an array containing three vertices of a triangular
      !>              plate. Each vertex is a three-dimensional vector. The
      !>              elements
      !>
      !>                 IVERTS(J,I), J = 1, 3
      !>
      !>              are, respectively, the X, Y, and Z components of the
      !>              Ith vertex.
      !>
      !>     DELTA    is a fraction by which the plate is to be scaled.
      !>              Scaling is done so that the scaled plate has the
      !>              following properties:
      !>
      !>                 -  it is co-planar with the input plate
      !>
      !>                 -  its centroid coincides with that of the input
      !>                    plate
      !>
      !>                 -  its sides remain parallel to the corresponding
      !>                    sides of the input plate
      !>
      !>                 -  the distance of each vertex from the centroid is
      !>                    (1+DELTA) times the corresponding distance for
      !>                    the input plate
      !>
      !>## Detailed Output
      !>
      !>     OVERTS   is an array containing three vertices of the triangular
      !>              plate resulting from scaling the input plate.
      !>
      !>              If CTROID is the centroid (the average of the vertices)
      !>              of the input plate, then the Ith vertex of OVERTS
      !>
      !>                 OVERTS(J,I), J = 1, 3
      !>
      !>              is equal to
      !>
      !>                 CTROID(J) + (1+DELTA)*( IVERTS(J,I) - CTROID(J) ),
      !>
      !>                 J = 1, 3
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine supports "greedy" ray-plate intercept algorithms.
      !>     Such algorithms attempt to ensure that false negatives---in which
      !>     an intersection is not found due to round-off error---do not
      !>     occur. In such an algorithm, the plate of interest is expanded
      !>     slightly before the intersection test is performed.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Expand an equilateral triangle that lies in the plane
      !>
      !>           { (x,y,z) : z = 7 }
      !>
      !>        Use an expansion fraction of 1.D0; this doubles the size of
      !>        the plate.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>       PROGRAM PLTEXP_EX1
      !>       IMPLICIT NONE
      !>
      !>       DOUBLE PRECISION      DELTA
      !>       DOUBLE PRECISION      IVERTS ( 3, 3 )
      !>       DOUBLE PRECISION      OVERTS ( 3, 3 )
      !>       DOUBLE PRECISION      S
      !>
      !>       INTEGER               I
      !>
      !>       S = SQRT(3.D0)/2
      !>
      !>       CALL VPACK (    S,  -0.5D0,  7.D0, IVERTS(1,1) )
      !>       CALL VPACK ( 0.D0,    1.D0,  7.D0, IVERTS(1,2) )
      !>       CALL VPACK (   -S,  -0.5D0,  7.D0, IVERTS(1,3) )
      !>
      !>       DELTA = 1.D0
      !>
      !>       CALL PLTEXP ( IVERTS, DELTA, OVERTS )
      !>
      !>       WRITE (*,*) ' '
      !>       WRITE (*,*) 'Vertices of input plate: '
      !>
      !>       WRITE (*, '(1X,A,3E18.10)' ) ' I1 = ',
      !>      .          (IVERTS(I,1), I = 1, 3)
      !>       WRITE (*, '(1X,A,3E18.10)' ) ' I2 = ',
      !>      .          (IVERTS(I,2), I = 1, 3)
      !>       WRITE (*, '(1X,A,3E18.10)' ) ' I3 = ',
      !>      .          (IVERTS(I,3), I = 1, 3)
      !>
      !>       WRITE (*,*) ' '
      !>       WRITE (*,*) 'Vertices of output plate: '
      !>
      !>       WRITE (*, '(1X,A,3E18.10)' ) ' O1 = ',
      !>      .          (OVERTS(I,1), I = 1, 3)
      !>       WRITE (*, '(1X,A,3E18.10)' ) ' O2 = ',
      !>      .          (OVERTS(I,2), I = 1, 3)
      !>       WRITE (*, '(1X,A,3E18.10)' ) ' O3 = ',
      !>      .          (OVERTS(I,3), I = 1, 3)
      !>       WRITE (*,*) ' '
      !>       END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Vertices of input plate:
      !>          I1 =   0.8660254038E+00 -0.5000000000E+00  0.7000000000E+01
      !>          I2 =   0.0000000000E+00  0.1000000000E+01  0.7000000000E+01
      !>          I3 =  -0.8660254038E+00 -0.5000000000E+00  0.7000000000E+01
      !>
      !>         Vertices of output plate:
      !>          O1 =   0.1732050808E+01 -0.1000000000E+01  0.7000000000E+01
      !>          O2 =   0.0000000000E+00  0.2000000000E+01  0.7000000000E+01
      !>          O3 =  -0.1732050808E+01 -0.1000000000E+01  0.7000000000E+01
      !>
      !>        Note that the height of the plate is unchanged, but the vectors
      !>        from the centroid to the vertices have doubled in length.
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
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Edits to code example output format for the solution to fit
      !>        within the $Examples section without modifications.
      !>
      !>-    SPICELIB Version 1.0.0, 29-FEB-2016 (NJB)
      !>
      !>        Based on original version 28-MAY-2014 (NJB)
      pure subroutine pltexp ( iverts, delta, overts )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: iverts ( 3, 3 )  !! Vertices of the plate to be expanded.
      real(wp), intent(in) :: delta  !! Fraction by which the plate is to be expanded.
      real(wp), intent(out) :: overts ( 3, 3 )  !! Vertices of the expanded plate.

      real(wp) :: sclctr ( 3 )
      real(wp) :: d
      real(wp) :: s

!
!     Compute the centroid of the input vertices. Scale the centroid
!     by DELTA, since we'll only use the scaled form.
!
!     Unroll all loops to avoid loop overhead.
!
      d = delta / 3.0_wp

      sclctr(1) = d * ( iverts(1,1) + iverts(1,2) + iverts(1,3) )
      sclctr(2) = d * ( iverts(2,1) + iverts(2,2) + iverts(2,3) )
      sclctr(3) = d * ( iverts(3,1) + iverts(3,2) + iverts(3,3) )

!
!     Compute the offsets of the vertices from the centroid CTROID;
!     scale each offset by (1+DELTA). The Ith expanded vertex is
!
!        CTROID + (1+DELTA) * ( IVERTS(*,I) - CTROID )
!
!     which can be re-written as
!
!        ( (1+DELTA) * IVERTS(*,I) )  -  ( DELTA * CTROID )
!
!     or
!
!        ( (1+DELTA) * IVERTS(*,I) )  -  SCLCTR
!
      s = 1.0_wp + delta

      overts(1,1) = ( s * iverts(1,1) ) - sclctr(1)
      overts(2,1) = ( s * iverts(2,1) ) - sclctr(2)
      overts(3,1) = ( s * iverts(3,1) ) - sclctr(3)

      overts(1,2) = ( s * iverts(1,2) ) - sclctr(1)
      overts(2,2) = ( s * iverts(2,2) ) - sclctr(2)
      overts(3,2) = ( s * iverts(3,2) ) - sclctr(3)

      overts(1,3) = ( s * iverts(1,3) ) - sclctr(1)
      overts(2,3) = ( s * iverts(2,3) ) - sclctr(2)
      overts(3,3) = ( s * iverts(3,3) ) - sclctr(3)

      end subroutine pltexp

      !> Compute an outward normal vector of a triangular plate.
      !> The vector does not necessarily have unit length.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2,
      !>     V3       are vertices of a triangular plate.
      !>
      !>## Detailed Output
      !>
      !>     NORMAL   is an outward normal vector of the plate defined by
      !>              the input vertices. The order of the vertices is
      !>              used to determine the choice of normal direction:
      !>              the normal vector is
      !>
      !>                 ( V2 - V1 ) x ( V3 - V2 )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The input plate may be degenerate: it may be a line segment
      !>         or a point. These are not considered to be erroneous inputs.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine saves computation time by not scaling the output
      !>     vector to unit length. The caller can scale the vector using
      !>     the routine VHAT.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute an upward normal of an equilateral triangle lying
      !>        in the X-Y plane and centered at the origin.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PLTNRM_EX1
      !>      IMPLICIT NONE
      !>
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      S
      !>      DOUBLE PRECISION      V1     ( 3 )
      !>      DOUBLE PRECISION      V2     ( 3 )
      !>      DOUBLE PRECISION      V3     ( 3 )
      !>
      !>      S = SQRT(3.D0)/2
      !>
      !>      CALL VPACK (    S,  -0.5D0,  0.D0, V1 )
      !>      CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 )
      !>      CALL VPACK (   -S,  -0.5D0,  0.D0, V3 )
      !>
      !>      CALL PLTNRM ( V1, V2, V3, NORMAL )
      !>
      !>      WRITE (*, '(A,3F18.14)' ) 'NORMAL = ', NORMAL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        NORMAL =   0.00000000000000  0.00000000000000  2.59807621135332
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
      !>        Edited the header to comply with NAIF standard..
      !>        Changed code example output format for the solution to fit
      !>        within the $Examples section without modifications.
      !>
      !>-    SPICELIB Version 1.0.0, 26-JAN-2016 (NJB)
      pure subroutine pltnrm ( v1, v2, v3, normal )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1     ( 3 )  !! Vertices of a plate.
      real(wp), intent(in) :: v2     ( 3 )  !! Vertices of a plate.
      real(wp), intent(in) :: v3     ( 3 )  !! Vertices of a plate.
      real(wp), intent(out) :: normal ( 3 )  !! Plate's outward normal vector.

      real(wp) :: edge1  ( 3 )
      real(wp) :: edge2  ( 3 )

!
!     This routine is error-free.
!

!
!     Type 2 plate vertices are ordered in the positive
!     (right-handed) sense about the outward normal.
!
      edge1 = v2 - v1
      edge2 = v3 - v2

      call vcrss ( edge1, edge2, normal )

      end subroutine pltnrm

      !> Compute the value of a polynomial and its first
      !> NDERIV derivatives at the value T.
      !>
      !>## Detailed Input
      !>
      !>     COEFFS   are the coefficients of the polynomial that is
      !>              to be evaluated. The first element of this array
      !>              should be the constant term, the second element the
      !>              linear coefficient, the third term the quadratic
      !>              coefficient, and so on. The number of coefficients
      !>              supplied should be one more than DEG.
      !>
      !>                 F(X) =   COEFFS(1) + COEFFS(2)*X + COEFFS(3)*X^2
      !>
      !>                        + COEFFS(4)*X^4 + ... + COEFFS(DEG+1)*X^DEG
      !>
      !>     DEG      is the degree of the polynomial to be evaluated. DEG
      !>              should be one less than the number of coefficients
      !>              supplied.
      !>
      !>     NDERIV   is the number of derivatives to compute. If NDERIV
      !>              is zero, only the polynomial will be evaluated. If
      !>              NDERIV = 1, then the polynomial and its first
      !>              derivative will be evaluated, and so on. If the value
      !>              of NDERIV is negative, the routine returns
      !>              immediately.
      !>
      !>     T        is the point at which the polynomial and its
      !>              derivatives should be evaluated.
      !>
      !>## Detailed Output
      !>
      !>     P        is an array containing the value of the polynomial and
      !>              its derivatives evaluated at T. The first element of
      !>              the array contains the value of P at T. The second
      !>              element of the array contains the value of the first
      !>              derivative of P at T and so on. The NDERIV + 1'st
      !>              element of the array contains the NDERIV'th derivative
      !>              of P evaluated at T.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If NDERIV is less than zero, the routine simply returns.
      !>
      !>     2)  If the degree of the polynomial is less than 0, the routine
      !>         returns the first NDERIV+1 elements of P set to 0.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses the user supplied coefficients (COEFFS)
      !>     to evaluate a polynomial (having these coefficients) and its
      !>     derivatives at the point T. The zero'th derivative of the
      !>     polynomial is regarded as the polynomial itself.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) For the polynomial
      !>
      !>           F(x) = 1 + 3*x + 0.5*x^2 + x^3 + 0.5*x^4 - x^5 + x^6
      !>
      !>        the coefficient set
      !>
      !>           Degree  coeffs
      !>           ------  ------
      !>           0       1
      !>           1       3
      !>           2       0.5
      !>           3       1
      !>           4       0.5
      !>           5      -1
      !>           6       1
      !>
      !>        Compute the value of the polynomial and it's first
      !>        3 derivatives at the value T = 1.0. We expect:
      !>
      !>           Derivative Number     T = 1
      !>           ------------------    -----
      !>           F(x)         0        6
      !>           F'(x)        1        10
      !>           F''(x)       2        23
      !>           F'''(x)      3        78
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM POLYDS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               NDERIV
      !>      PARAMETER           ( NDERIV = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      COEFFS (7)
      !>      DOUBLE PRECISION      P      ( NDERIV + 1 )
      !>      DOUBLE PRECISION      T
      !>
      !>      INTEGER               DEG
      !>      INTEGER               I
      !>
      !>      DATA                  COEFFS / 1.D0,   3.D0,
      !>     .                               0.5D0,  1.D0,
      !>     .                               0.5D0, -1.D0,
      !>     .                               1.D0          /
      !>
      !>      T = 1.D0
      !>      DEG = 6
      !>
      !>      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      !>
      !>      DO I= 1, NDERIV + 1
      !>         WRITE(*,*) 'P = ', P(I)
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         P =    6.0000000000000000
      !>         P =    10.000000000000000
      !>         P =    23.000000000000000
      !>         P =    78.000000000000000
      !>
      !>## Restrictions
      !>
      !>     1)  Depending on the coefficients the user should be careful when
      !>         taking high order derivatives. As the example shows, these
      !>         can get big in a hurry. In general the coefficients of the
      !>         derivatives of a polynomial grow at a rate greater
      !>         than N! (N factorial).
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 16-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Added
      !>        full code example. Updated Exception #2 to properly describe
      !>        the routine's behavior.
      !>
      !>-    SPICELIB Version 1.1.0, 11-JUL-1995 (KRG)
      !>
      !>        Replaced the function calls to DFLOAT with standard conforming
      !>        calls to DBLE.
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
      !>-    Beta Version 1.0.1, 30-DEC-1988 (WLT)
      !>
      !>        The error free specification was added as well as notes
      !>        on exceptional degree or derivative requests.
      pure subroutine polyds ( coeffs, deg, nderiv, t, p )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: coeffs ( 0:* )  !! Coefficients of the polynomial to be evaluated.
      integer(ip), intent(in) :: deg  !! Degree of the polynomial to be evaluated.
      integer(ip), intent(in) :: nderiv  !! Number of derivatives to compute.
      real(wp), intent(in) :: t  !! Point to evaluate the polynomial and derivatives
      real(wp), intent(out) :: p      ( 0:* )  !! Value of polynomial and derivatives.

      integer(ip) :: i
      integer(ip) :: k
      real(wp) :: scale

      if ( nderiv < 0 ) then
         return
      end if

!
!     The following loops may not look like much, but they compute
!     P(T), P'(T), P''(T), ... etc.
!
!     To see why, recall that if A_0 through A_N are the coefficients
!     of a polynomial, then P(t) can be computed from the sequence
!     of polynomials given by:
!
!        P_0(t) = 0
!        P_1(t) = t*P_0(t)     +   A_N
!        P_2(t) = t*P_1(t)     +   A_[N-1]
!               .
!               .
!               .
!        P_n(t) = t*P_[n-1](t) +   A_0
!
!     The final polynomial in this list is in fact P(t).  From this
!     it follows that P'(t) is given by P_n'(t).  But
!
!        P_n'(t)     = t*P_[n-1]'(t) +   P_[n-1](t)
!
!     and
!
!        P_[n-1]'(t) = t*P_[n-2]'(t) +   P_[n-2](t)
!                    .
!                    .
!                    .
!        P_2'(t)     = t*P_1'(t)     +   P_1(t)
!        P_1'(t)     = t*P_0'(t)     +   P_0(t)
!        P_0'(t)     = 0
!
!     Rearranging the sequence we have a recursive method
!     for computing P'(t).  At the i'th stage we require only the i-1st
!     polynomials P_[i-1] and P_[i-1]' .
!
!        P_0'(t)     = 0
!        P_1'(t)     = t*P_0'(t)     +   P_0(t)
!        P_2'(t)     = t*P_1'(t)     +   P_1(t)
!                    .
!                    .
!                    .
!        P_[n-1]'(t) = t*P_[n-2]'(t) +   P_[n-2](t)
!        P_n'(t)     = t*P_[n-1]'(t) +   P_[n-1](t)
!
!     Similarly,
!
!        P_0''(t)     = 0
!        P_1''(t)     = t*P_0''(t)     +   2*P_0'(t)
!        P_2''(t)     = t*P_1''(t)     +   2*P_1'(t)
!                     .
!                     .
!                     .
!        P_[n-1]''(t) = t*P_[n-2]''(t) +   2*P_[n-2]'(t)
!
!        P_0'''(t)     = 0
!        P_1'''(t)     = t*P_0'''(t)     +   3*P_0''(t)
!        P_2'''(t)     = t*P_1'''(t)     +   3*P_1''(t)
!                      .
!                      .
!                      .
!        P_[n-1]'''(t) = t*P_[n-2]'''(t) +   3*P_[n-2]''(t)
!
!     Thus if P(I) contains the k'th iterations of the i'th derivative
!     computation of P and P(I-1) contains the k'th iteration of the
!     i-1st derivative of P then, t*P(I) + I*P(I-1) is the value of the
!     k+1st iteration of the computation of the i'th derivative of
!     P. This can then be stored in P(I).
!
!     If in a loop we compute in-place k'th iteration of the
!     I'th derivative before we perform the in-place k'th iteration
!     of the I-1st and I-2cnd derivative, then the k-1'th values
!     of the I-1st and I-2cnd will not be altered and will be available
!     for the computation of the k'th iteration of the I-1st
!     derivative.  This observation gives us an economical way to
!     compute all of the derivatives (including the zero'th derivative)
!     in place.  We simply compute the iterates of the high order
!     derivatives first.

!
!     Initialize the polynomial value (and all of its derivatives) to be
!     zero.
!
      do i = 0, nderiv
         p(i) = 0.0_wp
      end do

!
!     Set up the loop "counters" (they count backwards) for the first
!     pass through the loop.
!
      k     = deg
      i     = nderiv
      scale = real( nderiv, wp )

      do while ( k >= 0 )

         do while ( i > 0 )
            p(i)  = t*p(i) + scale*p(i-1)
            scale = scale - 1
            i     = i - 1
         end do

         p(0)  = t*p(0) + coeffs(k)
         k     = k - 1
         i     = nderiv
         scale = real( nderiv, wp )
      end do

      return
      end subroutine polyds

      !> Find the rotation matrix corresponding to a specified unit
      !> quaternion.
      !>
      !>## Detailed Input
      !>
      !>     Q        is a unit-length SPICE-style quaternion. Q has the
      !>              property that
      !>
      !>                 || Q ||  =  1
      !>
      !>              See the discussion of quaternion styles in
      !>              $Particulars below.
      !>
      !>## Detailed Output
      !>
      !>     R        is a 3 by 3 rotation matrix representing the same
      !>              rotation as does Q. See the discussion titled
      !>              "Associating SPICE Quaternions with Rotation
      !>              Matrices" in $Particulars below.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If Q is not a unit quaternion, the output matrix R is
      !>         the rotation matrix that is the result of converting
      !>         normalized Q to a rotation matrix.
      !>
      !>     2)  If Q is the zero quaternion, the output matrix R is
      !>         the identity matrix.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If a 4-dimensional vector Q satisfies the equality
      !>
      !>        || Q ||   =  1
      !>
      !>     or equivalently
      !>
      !>            2          2          2          2
      !>        Q(0)   +   Q(1)   +   Q(2)   +   Q(3)   =  1,
      !>
      !>     then we can always find a unit vector A and a scalar r such that
      !>
      !>        Q = ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
      !>
      !>     We can interpret A and r as the axis and rotation angle of a
      !>     rotation in 3-space. If we restrict r to the range [0, pi],
      !>     then r and A are uniquely determined, except if r = pi. In this
      !>     special case, A and -A are both valid rotation axes.
      !>
      !>     Every rotation is represented by a unique orthogonal matrix; this
      !>     routine returns that unique rotation matrix corresponding to Q.
      !>
      !>     The SPICELIB routine M2Q is a one-sided inverse of this routine:
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
      !>          .-                                                         -.
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
      !>          `-                                                         -.
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
      !>                   .-             -.
      !>                   |  0   -n3   n2 |
      !>                   |               |
      !>         OMEGA  =  |  n3   0   -n1 |
      !>                   |               |
      !>                   | -n2   n1   0  |
      !>                   `-             -'
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
      !>            To convert the quaternion
      !>
      !>               Q = ( sqrt(2)/2, 0, 0, -sqrt(2)/2 )
      !>
      !>            to a rotation matrix, we can use the code fragment
      !>
      !>               Q(0) =  DSQRT(2)/2.D0
      !>               Q(1) =  0.D0
      !>               Q(2) =  0.D0
      !>               Q(3) = -DSQRT(2)/2.D0
      !>
      !>               CALL Q2M ( Q, R )
      !>
      !>            The matrix R will be set equal to
      !>
      !>               +-              -+
      !>               |  0     1    0  |
      !>               |                |
      !>               | -1     0    0  |.
      !>               |                |
      !>               |  0     0    1  |
      !>               +-              -+
      !>
      !>            Why?  Well, Q represents a rotation by some angle r about
      !>            some axis vector A, where r and A satisfy
      !>
      !>               Q =
      !>
      !>               ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
      !>
      !>            In this example,
      !>
      !>               Q = ( sqrt(2)/2, 0, 0, -sqrt(2)/2 ),
      !>
      !>            so
      !>
      !>               cos(r/2) = sqrt(2)/2.
      !>
      !>            Assuming that r is in the interval [0, pi], we must have
      !>
      !>               r = pi/2,
      !>
      !>            so
      !>
      !>               sin(r/2) = sqrt(2)/2.
      !>
      !>            Since the second through fourth components of Q represent
      !>
      !>               sin(r/2) * A,
      !>
      !>            it follows that
      !>
      !>               A = ( 0, 0, -1 ).
      !>
      !>            So Q represents a transformation that rotates vectors by
      !>            pi/2 about the negative z-axis. This is equivalent to a
      !>            coordinate system rotation of pi/2 about the positive
      !>            z-axis; and we recognize R as the matrix
      !>
      !>               [ pi/2 ] .
      !>                       3
      !>
      !>     2)  Finding a set of Euler angles that represent a rotation
      !>         specified by a quaternion:
      !>
      !>            Suppose our rotation R is represented by the quaternion
      !>            Q. To find angles TAU, ALPHA, DELTA such that
      !>
      !>               R  =  [ TAU ]  [ pi/2 - DELTA ]  [ ALPHA ] ,
      !>                            3                 2          3
      !>
      !>            we can use the code fragment
      !>
      !>               CALL Q2M    ( Q, R )
      !>
      !>               CALL M2EUL  ( R,   3,      2,       3,
      !>              .                   TAU,    DELTA,   ALPHA  )
      !>
      !>               DELTA = HALFPI() - DELTA
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
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Corrected the
      !>        output argument name in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.2, 26-FEB-2008 (NJB)
      !>
      !>        Updated header; added information about SPICE
      !>        quaternion conventions.
      !>
      !>-    SPICELIB Version 1.1.1, 13-JUN-2002 (FST)
      !>
      !>        Updated the $Exceptions section to clarify exceptions that
      !>        are the result of changes made in the previous version of
      !>        the routine.
      !>
      !>-    SPICELIB Version 1.1.0, 04-MAR-1999 (WLT)
      !>
      !>        Added code to handle the case in which the input quaternion
      !>        is not of length 1.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
      pure subroutine q2m ( q, r )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: q ( 0 : 3 )  !! A unit quaternion.
      real(wp), intent(out) :: r ( 3,  3 )  !! A rotation matrix corresponding to Q.

      real(wp) :: q01
      real(wp) :: q02
      real(wp) :: q03
      real(wp) :: q12
      real(wp) :: q13
      real(wp) :: q23
      real(wp) :: q1s
      real(wp) :: q2s
      real(wp) :: q3s

      real(wp) :: l2
      real(wp) :: sharpn

!
!     If a matrix R represents a rotation of r radians about the unit
!     vector n, we know that R can be represented as
!
!                                           2
!        I  +  sin(r) N  +  [ 1 - cos(r) ] N ,
!
!     where N is the matrix that satisfies
!
!        Nv = n x v
!
!     for all vectors v, namely
!
!             +-                -+
!             |  0    -n     n   |
!             |         3     2  |
!             |                  |
!        N =  |  n     0    -n   |.
!             |   3           1  |
!             |                  |
!             | -n     n     0   |
!             |   2     1        |
!             +-                -+
!
!      Define S as
!
!         sin(r/2) N,
!
!      and let our input quaternion Q be
!
!         ( q ,  q ,  q ,  q ).
!            0    1    2    3
!
!      Using the facts that
!
!                             2
!         1 - cos(r)  =  2 sin (r/2)
!
!      and
!
!         sin(r)      =  2 cos(r/2) sin(r/2),
!
!      we can express R as
!
!                                      2
!         I  +  2 cos(r/2) S    +   2 S,
!
!      or
!                                2
!         I  +  2 q  S    +   2 S.
!                  0
!
!      Since S is just
!
!         +-                -+
!         |  0    -q     q   |
!         |         3     2  |
!         |                  |
!         |  q     0    -q   |,
!         |   3           1  |
!         |                  |
!         | -q     q     0   |
!         |   2     1        |
!         +-                -+
!
!      our expression for R comes out to
!
!         +-                                                         -+
!         |          2   2                                            |
!         | 1 - 2 ( q + q  )    2( q q  -  q q )     2 ( q q  + q q ) |
!         |          2   3          1 2     0 3           1 3    0 2  |
!         |                                                           |
!         |                              2   2                        |
!         | 2( q q  +  q q )    1 - 2 ( q + q  )     2 ( q q  - q q ) |.
!         |     1 2     0 3              1   3            2 3    0 1  |
!         |                                                           |
!         |                                                   2   2   |
!         | 2( q q  -  q q )    2 ( q q  + q q )     1 - 2 ( q + q  ) |
!         |     1 3     0 2          2 3    0 1               1   2   |
!         +-                                                         -+
!
!      For efficiency, we avoid duplicating calculations where possible.
!

      q01  =  q(0) * q(1)
      q02  =  q(0) * q(2)
      q03  =  q(0) * q(3)

      q12  =  q(1) * q(2)
      q13  =  q(1) * q(3)

      q23  =  q(2) * q(3)

      q1s  =  q(1) * q(1)
      q2s  =  q(2) * q(2)
      q3s  =  q(3) * q(3)

!
!     We sharpen the computation by effectively converting Q to
!     a unit quaternion if it isn't one already.
!
      l2   =  q(0) * q(0) + q1s + q2s + q3s

      if ( l2 /= 1.0_wp .and. l2 /= 0.0_wp ) then

         sharpn = 1.0_wp / l2

         q01    = q01 * sharpn
         q02    = q02 * sharpn
         q03    = q03 * sharpn

         q12    = q12 * sharpn
         q13    = q13 * sharpn

         q23    = q23 * sharpn

         q1s    = q1s * sharpn
         q2s    = q2s * sharpn
         q3s    = q3s * sharpn

      end if

      r(1,1) =  1.0_wp  -  2.0_wp * ( q2s + q3s )
      r(2,1) =           2.0_wp * ( q12 + q03 )
      r(3,1) =           2.0_wp * ( q13 - q02 )

      r(1,2) =           2.0_wp * ( q12 - q03 )
      r(2,2) =  1.0_wp  -  2.0_wp * ( q1s + q3s )
      r(3,2) =           2.0_wp * ( q23 + q01 )

      r(1,3) =           2.0_wp * ( q13 + q02 )
      r(2,3) =           2.0_wp * ( q23 - q01 )
      r(3,3) =  1.0_wp  -  2.0_wp * ( q1s + q2s )

      return
      end subroutine q2m

      !> Derive angular velocity from a unit quaternion and its derivative
      !> with respect to time.
      !>
      !>## Detailed Input
      !>
      !>     Q        is a unit length 4-vector representing a
      !>              SPICE-style quaternion. See the discussion of
      !>              quaternion styles in $Particulars below.
      !>
      !>     DQ       is a 4-vector representing the derivative of
      !>              Q with respect to time.
      !>
      !>## Detailed Output
      !>
      !>     AV       is 3-vector representing the angular velocity
      !>              defined by Q and DQ, that is, the angular velocity
      !>              of the frame defined by the rotation matrix
      !>              associated with Q. This rotation matrix can be
      !>              obtained via the SPICELIB routine Q2M; see the
      !>              $Particulars section for the explicit matrix
      !>              entries.
      !>
      !>              AV is the vector (imaginary) part of the
      !>              quaternion product
      !>
      !>                       *
      !>                 -2 * Q  * DQ
      !>
      !>              This angular velocity is the same vector that
      !>              could be obtained (much less efficiently ) by
      !>              mapping Q and DQ to the corresponding C-matrix R
      !>              and its derivative DR, then calling the SPICELIB
      !>              routine XF2RAV.
      !>
      !>              AV has units of
      !>
      !>                 radians / T
      !>
      !>              where
      !>
      !>                 1 / T
      !>
      !>              is the unit associated with DQ.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  A unitized version of input quaternion is used in the
      !>         computation. No attempt is made to diagnose an invalid
      !>         input quaternion.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
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
      !>     About this routine
      !>     ==================
      !>
      !>     Given a time-dependent SPICE quaternion representing the
      !>     attitude of an object, we can obtain the object's angular
      !>     velocity AV in terms of the quaternion Q and its derivative
      !>     with respect to time DQ:
      !>
      !>                          *
      !>        AV  =  Im ( -2 * Q  * DQ )                                  (1)
      !>
      !>     That is, AV is the vector (imaginary) part of the product
      !>     on the right hand side (RHS) of equation (1).  The scalar part
      !>     of the RHS is zero.
      !>
      !>     We'll now provide an explanation of formula (1). For any
      !>     time-dependent rotation, the associated angular velocity at a
      !>     given time is a function of the rotation and its derivative at
      !>     that time. This fact enables us to extend a proof for a limited
      !>     subset of rotations to *all* rotations: if we find a formula
      !>     that, for any rotation in our subset, gives us the angular
      !>     velocity as a function of the rotation and its derivative, then
      !>     that formula must be true for all rotations.
      !>
      !>     We start out by considering the set of rotation matrices
      !>
      !>        R(t) = M(t)C                                                (2)
      !>
      !>     where C is a constant rotation matrix and M(t) represents a
      !>     matrix that "rotates" with constant, unit magnitude angular
      !>     velocity and that is equal to the identity matrix at t = 0.
      !>
      !>     For future reference, we'll consider C to represent a coordinate
      !>     transformation from frame F1 to frame F2. We'll call F1 the
      !>     "base frame" of C. We'll let AVF2 be the angular velocity of
      !>     M(t) relative to F2 and AVF1 be the same angular velocity
      !>     relative to F1.
      !>
      !>     Referring to the axis-and-angle decomposition of M(t)
      !>
      !>                                                2
      !>        M(t) = I + sin(t)OMEGA + (1-cos(t))OMEGA                    (3)
      !>
      !>     (see the Rotation Required Reading for a derivation) we
      !>     have
      !>
      !>        d(M(t))|
      !>        -------|     = OMEGA                                        (4)
      !>          dt   |t=0
      !>
      !>     Then the derivative of R(t) at t = 0 is given by
      !>
      !>        d(R(t))|
      !>        -------|     = OMEGA  * C                                   (5)
      !>          dt   |t=0
      !>
      !>     The rotation axis A associated with OMEGA is defined by        (6)
      !>
      !>        A(1) =  - OMEGA(2,3)
      !>        A(2) =    OMEGA(1,3)
      !>        A(3) =  - OMEGA(1,2)
      !>
      !>     Since the coordinate system rotation M(t) rotates vectors about A
      !>     through angle t radians at time t, the angular velocity AVF2 of
      !>     M(t) is actually given by
      !>
      !>        AVF2  =  - A                                                (7)
      !>
      !>     This angular velocity is represented relative to the image
      !>     frame F2 associated with the coordinate transformation C.
      !>
      !>     Now, let's proceed to the angular velocity formula for
      !>     quaternions.
      !>
      !>     To avoid some verbiage, we'll freely use 3-vectors to represent
      !>     the corresponding pure imaginary quaternions.
      !>
      !>     Letting QR(t), QM(t), and QC be quaternions representing the
      !>     time-dependent matrices R(t), M(t) and C respectively, where
      !>     QM(t) is selected to be a differentiable function of t in a
      !>     neighborhood of t = 0, the quaternion representing R(t) is
      !>
      !>        QR(t) = QM(t) * QC                                          (8)
      !>
      !>     Differentiating with respect to t, then evaluating derivatives
      !>     at t = 0, we have
      !>
      !>        d(QR(t))|         d(QM(t))|
      !>        --------|     =   --------|     * QC                        (9)
      !>           dt   |t=0         dt   |t=0
      !>
      !>     Since QM(t) represents a rotation having axis A and rotation
      !>     angle t, then (according to the relationship between SPICE
      !>     quaternions and rotations set out in the Rotation Required
      !>     Reading), we see QM(t) must be the quaternion (represented as the
      !>     sum of scalar and vector parts):
      !>
      !>        cos(t/2)  +  sin(t/2) * A                                  (10)
      !>
      !>     where A is the rotation axis corresponding to the matrix
      !>     OMEGA introduced in equation (3).  By inspection
      !>
      !>        d(QM(t))|
      !>        --------|     =   1/2 * A                                  (11)
      !>           dt   |t=0
      !>
      !>     which is a quaternion with scalar part zero. This allows us to
      !>     rewrite the quaternion derivative
      !>
      !>        d(QR(t))|
      !>        --------|     =   1/2  *  A  *  QC                         (12)
      !>           dt   |t=0
      !>
      !>     or for short,
      !>
      !>        DQ = 1/2 * A * QC                                          (13)
      !>
      !>     Since from (7) we know the angular velocity AVF2 of the frame
      !>     associated with QM(t) is the negative of the rotation axis
      !>     defined by (3), we have
      !>
      !>        DQ = - 1/2 * AVF2 * QC                                     (14)
      !>
      !>     Since
      !>
      !>        AVF2 = C * AVF1                                            (15)
      !>
      !>     we can apply the quaternion transformation formula
      !>     (from the Rotation Required Reading)
      !>
      !>                                 *
      !>        AVF2 =  QC  *  AVF1  * QC                                  (16)
      !>
      !>     Now we re-write (15) as
      !>
      !>                                     *
      !>        DQ = - 1/2 * ( QC * AVF1 * QC ) * QC
      !>
      !>           = - 1/2 *   QC * AVF1                                   (17)
      !>
      !>     Then the angular velocity vector AVF1 is given by
      !>
      !>                       *
      !>        AVF1  = -2 * QC  * DQ                                      (18)
      !>
      !>     The relation (18) has now been demonstrated for quaternions
      !>     having constant, unit magnitude angular velocity. But since
      !>     all time-dependent quaternions having value QC and derivative
      !>     DQ at a given time t have the same angular velocity at time t,
      !>     that angular velocity must be AVF1.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following test program creates a quaternion and quaternion
      !>        derivative from a known rotation matrix and angular velocity
      !>        vector. The angular velocity is recovered from the quaternion
      !>        and quaternion derivative by calling QDQ2AV and by an
      !>        alternate method; the results are displayed for comparison.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM QDQ2AV_EX1
      !>      IMPLICIT NONE
      !>C
      !>C     Start with a known rotation and angular velocity.  Find
      !>C     the quaternion and quaternion derivative.  The latter is
      !>C     computed from
      !>C
      !>C                       *
      !>C        AV  =   -2  * Q  * DQ
      !>C
      !>C        DQ  =  -1/2 * Q  * AV
      !>C
      !>C
      !>C     SPICELIB Functions
      !>C
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ANGLE  ( 3 )
      !>      DOUBLE PRECISION      AV     ( 3 )
      !>      DOUBLE PRECISION      AVX    ( 3 )
      !>      DOUBLE PRECISION      DM     ( 3,  3 )
      !>      DOUBLE PRECISION      DQ     ( 0 : 3 )
      !>      DOUBLE PRECISION      EXPAV  ( 3 )
      !>      DOUBLE PRECISION      M      ( 3,  3 )
      !>      DOUBLE PRECISION      MOUT   ( 3,  3 )
      !>      DOUBLE PRECISION      Q      ( 0 : 3 )
      !>      DOUBLE PRECISION      QAV    ( 0 : 3 )
      !>      DOUBLE PRECISION      XTRANS ( 6,  6 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Pick some Euler angles and form a rotation matrix.
      !>C
      !>      ANGLE(1) = -20.0 * RPD()
      !>      ANGLE(2) =  50.0 * RPD()
      !>      ANGLE(3) = -60.0 * RPD()
      !>
      !>      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 1, 3, M )
      !>
      !>      CALL M2Q   ( M, Q )
      !>
      !>C
      !>C     Choose an angular velocity vector.
      !>C
      !>      EXPAV(1) =  1.0D0
      !>      EXPAV(2) =  2.0D0
      !>      EXPAV(3) =  3.0D0
      !>
      !>C
      !>C     Form the quaternion derivative.
      !>C
      !>      QAV(0)    =  0.D0
      !>      CALL VEQU ( EXPAV, QAV(1) )
      !>
      !>      CALL QXQ ( Q, QAV, DQ )
      !>
      !>      CALL VSCLG ( -0.5D0, DQ, 4, DQ )
      !>
      !>C
      !>C     Recover angular velocity from Q and DQ using QDQ2AV.
      !>C
      !>      CALL QDQ2AV ( Q, DQ, AV )
      !>
      !>C
      !>C     Now we'll obtain the angular velocity from Q and
      !>C     DQ by an alternate method.
      !>C
      !>C     Convert Q back to a rotation matrix.
      !>C
      !>      CALL Q2M ( Q, M )
      !>
      !>C
      !>C     Convert Q and DQ to a rotation derivative matrix.  This
      !>C     somewhat messy procedure is based on differentiating the
      !>C     formula for deriving a rotation from a quaternion, then
      !>C     substituting components of Q and DQ into the derivative
      !>C     formula.
      !>C
      !>
      !>      DM(1,1)  =  -4.D0 * (   Q(2)*DQ(2)  +  Q(3)*DQ(3)  )
      !>
      !>      DM(1,2)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1)
      !>     .                      - Q(0)*DQ(3)  -  Q(3)*DQ(0)  )
      !>
      !>      DM(1,3)  =   2.D0 * (   Q(1)*DQ(3)  +  Q(3)*DQ(1)
      !>     .                      + Q(0)*DQ(2)  +  Q(2)*DQ(0)  )
      !>
      !>      DM(2,1)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1)
      !>     .                      + Q(0)*DQ(3)  +  Q(3)*DQ(0)  )
      !>
      !>      DM(2,2)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(3)*DQ(3)  )
      !>
      !>      DM(2,3)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2)
      !>     .                      - Q(0)*DQ(1)  -  Q(1)*DQ(0)  )
      !>
      !>      DM(3,1)  =   2.D0 * (   Q(3)*DQ(1)  +  Q(1)*DQ(3)
      !>     .                      - Q(0)*DQ(2)  -  Q(2)*DQ(0)  )
      !>
      !>      DM(3,2)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2)
      !>     .                      + Q(0)*DQ(1)  +  Q(1)*DQ(0)  )
      !>
      !>      DM(3,3)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(2)*DQ(2)  )
      !>
      !>C
      !>C     Form the state transformation matrix corresponding to M
      !>C     and DM.
      !>
      !>      CALL CLEARD ( 36, XTRANS )
      !>
      !>C
      !>C     Upper left block:
      !>C
      !>      DO I = 1, 3
      !>
      !>         DO J = 1, 3
      !>            XTRANS(I,J) = M(I,J)
      !>         END DO
      !>
      !>      END DO
      !>
      !>C
      !>C     Lower right block:
      !>C
      !>      DO I = 1, 3
      !>
      !>         DO J = 1, 3
      !>            XTRANS(3+I,3+J) = M(I,J)
      !>         END DO
      !>
      !>      END DO
      !>
      !>C
      !>C     Lower left block:
      !>C
      !>      DO I = 1, 3
      !>
      !>         DO J = 1, 3
      !>            XTRANS(3+I,J) = DM(I,J)
      !>         END DO
      !>
      !>      END DO
      !>
      !>C
      !>C     Now use XF2RAV to produce the expected angular velocity.
      !>C
      !>      CALL XF2RAV ( XTRANS, MOUT, AVX )
      !>
      !>C
      !>C     The results should match to nearly full double
      !>C     precision.
      !>C
      !>      WRITE(*,*) 'Original angular velocity:'
      !>      WRITE(*,'(1X,3F20.16)') EXPAV
      !>      WRITE(*,*) 'QDQ2AV''s angular velocity:'
      !>      WRITE(*,'(1X,3F20.16)') AV
      !>      WRITE(*,*) 'XF2RAV''s angular velocity:'
      !>      WRITE(*,'(1X,3F20.16)') AVX
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original angular velocity:
      !>           1.0000000000000000  2.0000000000000000  3.0000000000000000
      !>         QDQ2AV's angular velocity:
      !>           0.9999999999999998  1.9999999999999996  2.9999999999999991
      !>         XF2RAV's angular velocity:
      !>           1.0000000000000002  2.0000000000000000  3.0000000000000000
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
      !>-    SPICELIB Version 1.1.2, 04-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard..
      !>        Changed code example output format to fit within the $Examples
      !>        section without modifications.
      !>
      !>-    SPICELIB Version 1.1.1, 26-FEB-2008 (NJB)
      !>
      !>        Updated header; added information about SPICE
      !>        quaternion conventions.
      !>
      !>-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      !>
      !>-    SPICELIB Version 1.0.1, 24-FEB-2004 (NJB)
      !>
      !>        Made minor edits to the $Particulars header section.
      !>
      !>-    SPICELIB Version 1.0.0, 26-AUG-2002 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      pure subroutine qdq2av ( q, dq, av )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: q    ( 0 : 3 )  !! Unit SPICE quaternion.
      real(wp), intent(in) :: dq   ( 0 : 3 )  !! Derivative of Q with respect to time.
      real(wp), intent(out) :: av   ( 3 )  !! Angular velocity defined by Q and DQ.

      real(wp) :: qhat  ( 0 : 3 )
      real(wp) :: qstar ( 0 : 3 )
      real(wp) :: qtemp ( 0 : 3 )

!
!     Get a unitized copy of the input quaternion.
!
      call vhatg  ( q,    4,  qhat     )

!
!     Get the conjugate QSTAR of QHAT.
!
      qstar(0) = qhat(0)
      qstar(1:3) = -qhat(1:3)

!
!     Compute the angular velocity via the relationship
!
!                       *
!           AV  = -2 * Q  * DQ
!

      call qxq    ( qstar,  dq,        qtemp )
      av = (-2.0_wp)*qtemp(1:3)

      return
      end subroutine qdq2av

      !> Multiply two quaternions.
      !>
      !>## Detailed Input
      !>
      !>     Q1       is a 4-vector representing a SPICE-style
      !>              quaternion. See the discussion of quaternion
      !>              styles in $Particulars below.
      !>
      !>              Note that multiple styles of quaternions
      !>              are in use. This routine will not work properly
      !>              if the input quaternions do not conform to
      !>              the SPICE convention. See the $Particulars
      !>              section for details.
      !>
      !>     Q2       is a second SPICE-style quaternion.
      !>
      !>## Detailed Output
      !>
      !>     QOUT     is 4-vector representing the quaternion product
      !>
      !>                 Q1 * Q2
      !>
      !>              Representing Q(i) as the sums of scalar (real)
      !>              part s(i) and vector (imaginary) part v(i)
      !>              respectively,
      !>
      !>                 Q1 = s1 + v1
      !>                 Q2 = s2 + v2
      !>
      !>              QOUT has scalar part s3 defined by
      !>
      !>                 s3 = s1 * s2 - <v1, v2>
      !>
      !>              and vector part v3 defined by
      !>
      !>                 v3 = s1 * v2  +  s2 * v1  +  v1 x v2
      !>
      !>              where the notation < , > denotes the inner
      !>              product operator and x indicates the cross
      !>              product operator.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
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
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given the "basis" quaternions:
      !>
      !>           QID:  ( 1.0, 0.0, 0.0, 0.0 )
      !>           QI :  ( 0.0, 1.0, 0.0, 0.0 )
      !>           QJ :  ( 0.0, 0.0, 1.0, 0.0 )
      !>           QK :  ( 0.0, 0.0, 0.0, 1.0 )
      !>
      !>        the following quaternion products give these results:
      !>
      !>            Product       Expected result
      !>           -----------   ----------------------
      !>            QI  * QJ     ( 0.0, 0.0, 0.0, 1.0 )
      !>            QJ  * QK     ( 0.0, 1.0, 0.0, 0.0 )
      !>            QK  * QI     ( 0.0, 0.0, 1.0, 0.0 )
      !>            QI  * QI     (-1.0, 0.0, 0.0, 0.0 )
      !>            QJ  * QJ     (-1.0, 0.0, 0.0, 0.0 )
      !>            QK  * QK     (-1.0, 0.0, 0.0, 0.0 )
      !>            QID * QI     ( 0.0, 1.0, 0.0, 0.0 )
      !>            QI  * QID    ( 0.0, 1.0, 0.0, 0.0 )
      !>            QID * QJ     ( 0.0, 0.0, 1.0, 0.0 )
      !>
      !>        The following code example uses QXQ to produce these results.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM QXQ_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      QID    ( 0 : 3 )
      !>      DOUBLE PRECISION      QI     ( 0 : 3 )
      !>      DOUBLE PRECISION      QJ     ( 0 : 3 )
      !>      DOUBLE PRECISION      QK     ( 0 : 3 )
      !>      DOUBLE PRECISION      QOUT   ( 0 : 3 )
      !>
      !>C
      !>C     Let QID, QI, QJ, QK be the "basis"
      !>C     quaternions.
      !>C
      !>      DATA                  QID  / 1.D0,  0.D0,  0.D0,  0.D0 /
      !>      DATA                  QI   / 0.D0,  1.D0,  0.D0,  0.D0 /
      !>      DATA                  QJ   / 0.D0,  0.D0,  1.D0,  0.D0 /
      !>      DATA                  QK   / 0.D0,  0.D0,  0.D0,  1.D0 /
      !>
      !>C
      !>C     Compute:
      !>C
      !>C        QI x QJ = QK
      !>C        QJ x QK = QI
      !>C        QK x QI = QJ
      !>C
      !>      CALL QXQ ( QI, QJ, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QI x QJ  =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '     QK  =', QK
      !>      WRITE(*,*) ' '
      !>
      !>      CALL QXQ ( QJ, QK, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QJ x QK  =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '     QI  =', QI
      !>      WRITE(*,*) ' '
      !>
      !>      CALL QXQ ( QK, QI, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QK x QI  =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '     QJ  =', QJ
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Compute:
      !>C
      !>C        QI x QI  ==  -QID
      !>C        QJ x QJ  ==  -QID
      !>C        QK x QK  ==  -QID
      !>C
      !>      CALL QXQ ( QI, QI, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QI x QI  =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '     QID =', QID
      !>      WRITE(*,*) ' '
      !>
      !>      CALL QXQ ( QJ, QJ, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QJ x QJ  =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '     QID =', QID
      !>      WRITE(*,*) ' '
      !>
      !>      CALL QXQ ( QK, QK, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QK x QK  =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '     QID =', QID
      !>      WRITE(*,*) ' '
      !>
      !>C
      !>C     Compute:
      !>C
      !>C        QID x QI  = QI
      !>C        QI  x QID = QI
      !>C        QID x QJ  = QJ
      !>C
      !>      CALL QXQ ( QID, QI, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QID x QI =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '      QI =', QI
      !>      WRITE(*,*) ' '
      !>
      !>      CALL QXQ ( QI, QID, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QI x QID =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '      QI =', QI
      !>      WRITE(*,*) ' '
      !>
      !>      CALL QXQ ( QID, QJ, QOUT )
      !>      WRITE(*,'(A,4F8.2)') 'QID x QJ =', QOUT
      !>      WRITE(*,'(A,4F8.2)') '      QJ =', QJ
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        QI x QJ  =    0.00    0.00    0.00    1.00
      !>             QK  =    0.00    0.00    0.00    1.00
      !>
      !>        QJ x QK  =    0.00    1.00    0.00    0.00
      !>             QI  =    0.00    1.00    0.00    0.00
      !>
      !>        QK x QI  =    0.00    0.00    1.00    0.00
      !>             QJ  =    0.00    0.00    1.00    0.00
      !>
      !>        QI x QI  =   -1.00    0.00    0.00    0.00
      !>             QID =    1.00    0.00    0.00    0.00
      !>
      !>        QJ x QJ  =   -1.00    0.00    0.00    0.00
      !>             QID =    1.00    0.00    0.00    0.00
      !>
      !>        QK x QK  =   -1.00    0.00    0.00    0.00
      !>             QID =    1.00    0.00    0.00    0.00
      !>
      !>        QID x QI =    0.00    1.00    0.00    0.00
      !>              QI =    0.00    1.00    0.00    0.00
      !>
      !>        QI x QID =    0.00    1.00    0.00    0.00
      !>              QI =    0.00    1.00    0.00    0.00
      !>
      !>        QID x QJ =    0.00    0.00    1.00    0.00
      !>              QJ =    0.00    0.00    1.00    0.00
      !>
      !>     2) Compute the composition of two rotation matrices by
      !>        converting them to quaternions and computing their
      !>        product, and by directly multiplying the matrices.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM QXQ_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CMAT1  ( 3,  3 )
      !>      DOUBLE PRECISION      CMAT2  ( 3,  3 )
      !>      DOUBLE PRECISION      CMOUT  ( 3,  3 )
      !>      DOUBLE PRECISION      Q1     ( 0 : 3 )
      !>      DOUBLE PRECISION      Q2     ( 0 : 3 )
      !>      DOUBLE PRECISION      QOUT   ( 0 : 3 )
      !>
      !>      INTEGER               I
      !>
      !>      DATA                  CMAT1  /  1.D0,  0.D0,  0.D0,
      !>     .                                0.D0, -1.D0,  0.D0,
      !>     .                                0.D0,  0.D0, -1.D0  /
      !>
      !>      DATA                  CMAT2  /  0.D0,  1.D0,  0.D0,
      !>     .                                1.D0,  0.D0,  0.D0,
      !>     .                                0.D0,  0.D0, -1.D0  /
      !>
      !>C
      !>C     Convert the C-matrices to quaternions.
      !>C
      !>      CALL M2Q ( CMAT1, Q1 )
      !>      CALL M2Q ( CMAT2, Q2 )
      !>
      !>C
      !>C     Find the product.
      !>C
      !>      CALL QXQ ( Q1, Q2, QOUT )
      !>
      !>C
      !>C     Convert the result to a C-matrix.
      !>C
      !>      CALL Q2M ( QOUT, CMOUT )
      !>
      !>      WRITE(*,'(A)') 'Using quaternion product:'
      !>      WRITE(*,'(3F10.4)') (CMOUT(1,I), I = 1, 3)
      !>      WRITE(*,'(3F10.4)') (CMOUT(2,I), I = 1, 3)
      !>      WRITE(*,'(3F10.4)') (CMOUT(3,I), I = 1, 3)
      !>
      !>C
      !>C     Multiply CMAT1 and CMAT2 directly.
      !>C
      !>      CALL MXM ( CMAT1, CMAT2, CMOUT )
      !>
      !>      WRITE(*,'(A)') 'Using matrix product:'
      !>      WRITE(*,'(3F10.4)') (CMOUT(1,I), I = 1, 3)
      !>      WRITE(*,'(3F10.4)') (CMOUT(2,I), I = 1, 3)
      !>      WRITE(*,'(3F10.4)') (CMOUT(3,I), I = 1, 3)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Using quaternion product:
      !>            0.0000    1.0000    0.0000
      !>           -1.0000    0.0000    0.0000
      !>            0.0000    0.0000    1.0000
      !>        Using matrix product:
      !>            0.0000    1.0000    0.0000
      !>           -1.0000    0.0000    0.0000
      !>            0.0000    0.0000    1.0000
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
      !>-    SPICELIB Version 1.0.2, 06-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Created complete code examples from existing example and
      !>        code fragments.
      !>
      !>-    SPICELIB Version 1.0.1, 26-FEB-2008 (NJB)
      !>
      !>        Updated header; added information about SPICE
      !>        quaternion conventions.
      !>
      !>-    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB)
      pure subroutine qxq ( q1, q2, qout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: q1   ( 0 : 3 )  !! First SPICE quaternion factor.
      real(wp), intent(in) :: q2   ( 0 : 3 )  !! Second SPICE quaternion factor.
      real(wp), intent(out) :: qout ( 0 : 3 )  !! Product of Q1 and Q2.

      real(wp) :: cross ( 3 )

!
!     Compute the scalar part of the product.
!
      qout(0)  =  q1(0) * q2(0)  -  vdot( q1(1), q2(1) )

!
!     And now the vector part.  The SPICELIB routine VLCOM3 computes
!     a linear combination of three 3-vectors.
!
      call vcrss  ( q1(1),  q2(1),  cross )

      qout(1:3) = q1(0)*q2(1:3) + q2(0)*q1(1:3) + 1.0_wp*cross

      return
      end subroutine qxq

      !> Convert from range, right ascension, and declination to
      !> rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RANGE    is the distance of the point from the origin. Input
      !>              should be in terms of the same units in which the
      !>              output is desired.
      !>
      !>     RA       is the right ascension of the point. This is the angular
      !>              distance measured toward the east from the prime
      !>              meridian to the meridian containing the input point.
      !>              The direction of increasing right ascension is from
      !>              the +X axis towards the +Y axis.
      !>
      !>              The range (i.e., the set of allowed values) of
      !>              RA is unrestricted. Units are radians.
      !>
      !>     DEC      is the declination of the point. This is the angle from
      !>              the XY plane of the ray from the origin through the
      !>              point.
      !>
      !>              The range (i.e., the set of allowed values) of
      !>              DEC is unrestricted. Units are radians.
      !>
      !>## Detailed Output
      !>
      !>     RECTAN   is the array containing the rectangular coordinates of
      !>              the point.
      !>
      !>              The units associated with RECTAN are those
      !>              associated with the input RANGE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine converts the right ascension, declination, and range
      !>     of a point into the associated rectangular coordinates.
      !>
      !>     The input is defined by a distance from a central reference point,
      !>     an angle from a reference meridian, and an angle above the equator
      !>     of a sphere centered at the central reference point.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Convert to the J2000 frame the right ascension and declination
      !>        of an object initially expressed with respect to the B1950
      !>        reference frame.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RADREC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DECB
      !>      DOUBLE PRECISION      DECJ
      !>      DOUBLE PRECISION      MTRANS ( 3, 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RAB
      !>      DOUBLE PRECISION      RAJ
      !>      DOUBLE PRECISION      V1950  ( 3    )
      !>      DOUBLE PRECISION      V2000  ( 3    )
      !>
      !>C
      !>C     Set the initial right ascension and declination
      !>C     coordinates of the object, given with respect
      !>C     to the B1950 reference frame.
      !>C
      !>      RAB  = 135.88680896D0
      !>      DECB =  17.50151037D0
      !>
      !>C
      !>C     Convert RAB and DECB to a 3-vector expressed in
      !>C     the B1950 frame.
      !>C
      !>      CALL RADREC ( 1.D0, RAB * RPD(), DECB * RPD(), V1950 )
      !>
      !>C
      !>C     We use the SPICELIB routine PXFORM to obtain the
      !>C     transformation  matrix for converting vectors between
      !>C     the B1950 and J2000 reference frames.  Since
      !>C     both frames are inertial, the input time value we
      !>C     supply to PXFORM is arbitrary.  We choose zero
      !>C     seconds past the J2000 epoch.
      !>C
      !>      CALL PXFORM ( 'B1950', 'J2000', 0.D0, MTRANS )
      !>
      !>C
      !>C     Transform the vector to the J2000 frame.
      !>C
      !>      CALL MXV ( MTRANS, V1950, V2000 )
      !>
      !>C
      !>C     Find the right ascension and declination of the
      !>C     J2000-relative vector.
      !>C
      !>      CALL RECRAD ( V2000, R, RAJ, DECJ )
      !>
      !>C
      !>C     Output the results.
      !>C
      !>      WRITE(*,*) 'Right ascension (B1950 frame): ', RAB
      !>      WRITE(*,*) 'Declination (B1950 frame)    : ', DECB
      !>
      !>      WRITE(*,*) 'Right ascension (J2000 frame): ',
      !>     .                                       RAJ * DPR()
      !>      WRITE(*,*) 'Declination (J2000 frame)    : ',
      !>     .                                      DECJ * DPR()
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Right ascension (B1950 frame):    135.88680896000000
      !>         Declination (B1950 frame)    :    17.501510369999998
      !>         Right ascension (J2000 frame):    136.58768235448090
      !>         Declination (J2000 frame)    :    17.300442748830637
      !>
      !>     2) Define a set of 15 right ascension-declination data pairs for
      !>        the Earth's pole at different ephemeris epochs, convert them
      !>        to rectangular coordinates and compute the angular separation
      !>        between these coordinates and the IAU_EARTH pole given by a
      !>        PCK kernel.
      !>
      !>        Use the PCK kernel below to load the required triaxial
      !>        ellipsoidal shape model and orientation data for the Earth.
      !>
      !>           pck00010.tpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RADREC_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>      DOUBLE PRECISION      VSEP
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NCOORD
      !>      PARAMETER           ( NCOORD = 15 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DEC    ( NCOORD )
      !>      DOUBLE PRECISION      ET     ( NCOORD )
      !>      DOUBLE PRECISION      POLE   ( 3      )
      !>      DOUBLE PRECISION      MTRANS ( 3, 3   )
      !>      DOUBLE PRECISION      RA     ( NCOORD )
      !>      DOUBLE PRECISION      V2000  ( 3      )
      !>      DOUBLE PRECISION      Z      ( 3      )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define a set of 15 right ascension-declination
      !>C     coordinate pairs (in degrees) for the Earth's pole
      !>C     and the array of corresponding ephemeris times in
      !>C     J2000 TDB seconds.
      !>C
      !>      DATA                  RA   /
      !>     .           180.003739D0, 180.003205D0, 180.002671D0,
      !>     .           180.002137D0, 180.001602D0, 180.001068D0,
      !>     .           180.000534D0, 360.000000D0, 359.999466D0,
      !>     .           359.998932D0, 359.998397D0, 359.997863D0,
      !>     .           359.997329D0, 359.996795D0, 359.996261D0  /
      !>
      !>      DATA                  DEC  /
      !>     .            89.996751D0,  89.997215D0,  89.997679D0,
      !>     .            89.998143D0,  89.998608D0,  89.999072D0,
      !>     .            89.999536D0,  90.000000D0,  89.999536D0,
      !>     .            89.999072D0,  89.998607D0,  89.998143D0,
      !>     .            89.997679D0,  89.997215D0,  89.996751D0  /
      !>
      !>      DATA                  ET   /   -18408539.52023917D0,
      !>     .         -15778739.49107254D0, -13148939.46190590D0,
      !>     .         -10519139.43273926D0,  -7889339.40357262D0,
      !>     .          -5259539.37440598D0,  -2629739.34523934D0,
      !>     .                60.68392730D0,   2629860.71309394D0,
      !>     .           5259660.74226063D0,   7889460.77142727D0,
      !>     .          10519260.80059391D0,  13149060.82976055D0,
      !>     .          15778860.85892719D0,  18408660.88809383D0  /
      !>
      !>      DATA                  Z    /  0.D0, 0.D0, 1.D0  /
      !>
      !>C
      !>C     Load a PCK kernel.
      !>C
      !>      CALL FURNSH ( 'pck00010.tpc' )
      !>
      !>C
      !>C     Print the banner out.
      !>C
      !>      WRITE(*,'(A)') '       ET           Angular difference'
      !>      WRITE(*,'(A)') '------------------  ------------------'
      !>
      !>      DO I = 1, NCOORD
      !>
      !>C
      !>C        Convert the right ascension and declination
      !>C        coordinates (in degrees) to rectangular.
      !>C
      !>         CALL RADREC ( 1.D0, RA(I) * RPD(), DEC(I) * RPD(),
      !>     .                 V2000                               )
      !>
      !>C
      !>C        Retrieve the transformation matrix from the J2000
      !>C        frame to the IAU_EARTH frame.
      !>C
      !>         CALL PXFORM ( 'J2000', 'IAU_EARTH', ET(I), MTRANS )
      !>
      !>C
      !>C        Rotate the V2000 vector into IAU_EARTH. This vector
      !>C        should equal (round-off) the Z direction unit vector.
      !>C
      !>         CALL MXV ( MTRANS, V2000, POLE )
      !>
      !>C
      !>C        Output the ephemeris time and the angular separation
      !>C        between the rotated vector and the Z direction unit
      !>C        vector.
      !>C
      !>         WRITE(*,'(F18.8,2X,F18.16)') ET(I),
      !>     .                                VSEP( POLE, Z ) * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>               ET           Angular difference
      !>        ------------------  ------------------
      !>        -18408539.52023917  0.0000001559918278
      !>        -15778739.49107254  0.0000000106799881
      !>        -13148939.46190590  0.0000001773517911
      !>        -10519139.43273926  0.0000003440236194
      !>         -7889339.40357262  0.0000004893045693
      !>         -5259539.37440598  0.0000003226327536
      !>         -2629739.34523934  0.0000001559609507
      !>               60.68392730  0.0000000107108706
      !>          2629860.71309394  0.0000001773826862
      !>          5259660.74226063  0.0000003440544891
      !>          7889460.77142727  0.0000004892736740
      !>         10519260.80059391  0.0000003226018712
      !>         13149060.82976055  0.0000001559300556
      !>         15778860.85892719  0.0000000107417474
      !>         18408660.88809383  0.0000001774135760
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  L. Taff, "Celestial Mechanics, A Computational Guide for the
      !>          Practitioner," Wiley, 1985
      !>
      !>## Author and Institution
      !>
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONTE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete  code example based on existing code fragment
      !>        and a second example.
      !>
      !>-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header changes were made to improve clarity. Some
      !>        minor header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
      pure subroutine radrec ( range, ra, dec, rectan )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: range  !! Distance of a point from the origin.
      real(wp), intent(in) :: ra  !! Right ascension of point in radians.
      real(wp), intent(in) :: dec  !! Declination of point in radians.
      real(wp), intent(out) :: rectan ( 3 )  !! Rectangular coordinates of the point.

!
!     Convert from range, right ascension, and declination to
!     rectangular coordinates by calling the routine LATREC.
!
      call latrec ( range, ra, dec, rectan )

      return
      end subroutine radrec

      !> Determine a state transformation matrix from a rotation matrix
      !> and the angular velocity of the rotation.
      !>
      !>## Detailed Input
      !>
      !>     ROT      is a rotation matrix that gives the transformation from
      !>              some frame FRAME1 to another frame FRAME2.
      !>
      !>     AV       is the angular velocity of the transformation.
      !>              In other words, if P is the position of a fixed
      !>              point in FRAME2, then from the point of view of
      !>              FRAME1, P rotates (in a right handed sense) about
      !>              an axis parallel to AV. Moreover the rate of rotation
      !>              in radians per unit time is given by the length of
      !>              AV.
      !>
      !>              More formally, the velocity V of P in FRAME1 is
      !>              given by
      !>                                 T
      !>                 V  =  AV x ( ROT  * P )
      !>
      !>## Detailed Output
      !>
      !>     XFORM    is a state transformation matrix associated
      !>              with ROT and AV. If S1 is the state of an object
      !>              with respect to FRAME1, then the state S2 of the
      !>              object with respect to FRAME2 is given by
      !>
      !>                 S2  =  XFORM * S1
      !>
      !>              where "*" denotes Matrix-Vector multiplication.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No checks are performed on ROT to ensure that it is indeed
      !>         a rotation matrix.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is essentially a macro routine for converting
      !>     a rotation and angular velocity of the rotation to the
      !>     equivalent state transformation matrix.
      !>
      !>     This routine is an inverse of XF2RAV.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following example program uses CKGPAV to get C-matrix
      !>        and associated angular velocity vector for an image whose
      !>        SCLK count (un-encoded character string version) is known.
      !>
      !>        From that matrix and angular velocity vector, the associated
      !>        state transformation matrix is obtained.
      !>
      !>        Note that we need to load a SCLK kernel to convert from clock
      !>        string to "ticks." Although not required for older spacecraft
      !>        clocks, most modern spacecraft ones require a leapseconds
      !>        kernel to be loaded in addition to a SCLK kernel.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: rav2xf_ex1.tm
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
      !>              File name              Contents
      !>              --------------------   -----------------------
      !>              cas00071.tsc           CASSINI SCLK
      !>              04161_04164ra.bc       CASSINI spacecraft
      !>                                     reconstructed CK
      !>
      !>           \begindata
      !>
      !>             KERNELS_TO_LOAD = ( 'cas00071.tsc'
      !>                                 '04161_04164ra.bc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RAV2XF_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Constants for this program.
      !>C
      !>C     -- The code for the CASSINI spacecraft clock is -82.
      !>C
      !>C     -- The code for CASSINI spacecraft reference frame is
      !>C        -82000.
      !>C
      !>C    --  Spacecraft clock tolerance is 1.0 seconds. This may
      !>C        not be an acceptable tolerance for some applications.
      !>C        It must be converted to "ticks" (units of encoded
      !>C        SCLK) for input to CKGPAV.
      !>C
      !>C     -- The reference frame we want is J2000.
      !>C
      !>      CHARACTER*(*)         META
      !>      PARAMETER           ( META   = 'rav2xf_ex1.tm' )
      !>
      !>      CHARACTER*(*)         REFFRM
      !>      PARAMETER           ( REFFRM = 'J2000' )
      !>
      !>      CHARACTER*(*)         SCLKCH
      !>      PARAMETER           ( SCLKCH = '1/1465476046.160' )
      !>
      !>      CHARACTER*(*)         SCLTOL
      !>      PARAMETER           ( SCLTOL = '1.0' )
      !>
      !>      INTEGER               SCID
      !>      PARAMETER           ( SCID   = -82    )
      !>
      !>      INTEGER               INSTID
      !>      PARAMETER           ( INSTID = -82000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AV     ( 3 )
      !>      DOUBLE PRECISION      CLKOUT
      !>      DOUBLE PRECISION      CMAT   ( 3, 3 )
      !>      DOUBLE PRECISION      FXMAT  ( 6, 6 )
      !>      DOUBLE PRECISION      SCLKDP
      !>      DOUBLE PRECISION      TOLTIK
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load kernels.
      !>C
      !>      CALL FURNSH ( META )
      !>
      !>C
      !>C     Convert tolerance from CASSINI formatted character
      !>C     string SCLK to ticks which are units of encoded SCLK.
      !>C
      !>      CALL SCTIKS ( SCID, SCLTOL, TOLTIK )
      !>
      !>C
      !>C     CKGPAV requires encoded spacecraft clock.
      !>C
      !>      CALL SCENCD ( SCID, SCLKCH, SCLKDP )
      !>
      !>      CALL CKGPAV ( INSTID, SCLKDP, TOLTIK, REFFRM,
      !>     .              CMAT,   AV,     CLKOUT, FOUND )
      !>
      !>C
      !>C     Recall that CMAT and AV are the rotation and angular
      !>C     velocity of the transformation from J2000 to the
      !>C     spacecraft frame.
      !>C
      !>      IF ( FOUND ) THEN
      !>
      !>C
      !>C        Display CMAT and AV.
      !>C
      !>         WRITE(*,'(A)') 'Rotation matrix:'
      !>         DO I = 1, 3
      !>
      !>            WRITE(*,'(3F10.6)') (CMAT(I,J), J=1,3 )
      !>
      !>         END DO
      !>
      !>         WRITE(*,'(A)') 'Angular velocity:'
      !>         WRITE(*,'(3F20.16)') AV
      !>
      !>C
      !>C        Get state transformation from J2000 to the spacecraft
      !>C        frame.
      !>C
      !>         CALL RAV2XF ( CMAT,  AV, FXMAT )
      !>
      !>C
      !>C        Display the results.
      !>C
      !>         WRITE(*,*)
      !>         WRITE(*,'(A)') 'State transformation matrix:'
      !>         DO I = 1, 6
      !>
      !>            WRITE(*,'(6F10.6)') (FXMAT(I,J), J=1,6 )
      !>
      !>         END DO
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) 'No rotation matrix/angular velocity '
      !>     .          //  'found for ', SCLKCH
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Rotation matrix:
      !>         -0.604984  0.796222 -0.005028
      !>         -0.784160 -0.596891 -0.169748
      !>         -0.138158 -0.098752  0.985475
      !>        Angular velocity:
      !>          0.0000032866819065 -0.0000099372638338  0.0000197597699770
      !>
      !>        State transformation matrix:
      !>         -0.604984  0.796222 -0.005028  0.000000  0.000000  0.000000
      !>         -0.784160 -0.596891 -0.169748  0.000000  0.000000  0.000000
      !>         -0.138158 -0.098752  0.985475  0.000000  0.000000  0.000000
      !>         -0.000016 -0.000012 -0.000003 -0.604984  0.796222 -0.005028
      !>          0.000013 -0.000015 -0.000010 -0.784160 -0.596891 -0.169748
      !>         -0.000008 -0.000006 -0.000002 -0.138158 -0.098752  0.985475
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
      !>-    SPICELIB Version 1.1.1, 04-JUL-2021 (JDR)
      !>
      !>        Corrected $Abstract section, which described XF2RAV instead of
      !>        this routine.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based existing fragment.
      !>
      !>         Added ROTATION to the required readings.
      !>
      !>-    SPICELIB Version 1.1.0, 28-JUL-1997 (WLT)
      !>
      !>        The example in version 1.0.0 was incorrect. The example
      !>        in version 1.1.0 fixes the previous problem.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
      pure subroutine rav2xf ( rot, av, xform )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: rot    ( 3, 3 )  !! Rotation matrix.
      real(wp), intent(in) :: av     ( 3    )  !! Angular velocity vector.
      real(wp), intent(out) :: xform  ( 6, 6 )  !! State transformation associated with ROT and AV.

      integer(ip) :: i
      integer(ip) :: j

      real(wp) :: omegat ( 3, 3 )
      real(wp) :: drotdt ( 3, 3 )

!
!     A state transformation matrix XFORM has the following form
!
!         [      |     ]
!         |  R   |  0  |
!         |      |     |
!         | -----+-----|
!         |  dR  |     |
!         |  --  |  R  |
!         [  dt  |     ]
!
!     where R is a rotation and dR/dt is the time derivative of that
!     rotation.  From this we can immediately fill in most of the
!     state transformation matrix.
!
      do i = 1, 3
         do j = 1,3
            xform(i,  j  ) = rot(i,j)
            xform(i+3,j+3) = rot(i,j)
            xform(i,  j+3) = 0.0_wp
         end do
      end do

!
!     Now for the rest.
!
!     Recall that ROT is a transformation that converts positions
!     in some frame FRAME1 to positions in a second frame FRAME2.
!
!     The angular velocity matrix OMEGA (the cross product matrix
!     corresponding to AV) has the following property.
!
!     If P is the position of an object that is stationary with
!     respect to FRAME2 then the velocity V of that object in FRAME1
!     is given by:
!                          t
!         V  =  OMEGA * ROT  *  P
!
!     But V is also given by
!
!                    t
!               d ROT
!         V =   -----  * P
!                 dt
!
!     So that
!                                  t
!                    t        d ROT
!         OMEGA * ROT    =   -------
!                               dt
!
!     Hence
!
!          d ROT                 t
!          -----   =  ROT * OMEGA
!            dt
!
!     From this discussion we can see that we need OMEGA transpose.
!     Here it is.
!
      omegat(1,1) =  0.0_wp
      omegat(2,1) = -av(3)
      omegat(3,1) =  av(2)

      omegat(1,2) =  av(3)
      omegat(2,2) =  0.0_wp
      omegat(3,2) = -av(1)

      omegat(1,3) = -av(2)
      omegat(2,3) =  av(1)
      omegat(3,3) =  0.0_wp

      call mxm ( rot, omegat, drotdt )

      do i = 1, 3
         do j = 1,3
            xform(i+3,j) = drotdt(i,j)
         end do
      end do

      return
      end subroutine rav2xf

      !> Convert rectangular coordinates of a point to range, azimuth and
      !> elevation.
      !>
      !>## Detailed Input
      !>
      !>     RECTAN   are the rectangular coordinates of a point.
      !>
      !>     AZCCW    is a flag indicating how azimuth is measured.
      !>
      !>              If AZCCW is .TRUE., azimuth increases in the
      !>              counterclockwise direction; otherwise it increases in
      !>              the clockwise direction.
      !>
      !>     ELPLSZ   is a flag indicating how elevation is measured.
      !>
      !>              If ELPLSZ is .TRUE., elevation increases from
      !>              the XY plane toward +Z; otherwise toward -Z.
      !>
      !>## Detailed Output
      !>
      !>     RANGE    is the distance of the point from the origin.
      !>
      !>              The units associated with RANGE are those associated
      !>              with the input point.
      !>
      !>     AZ       is the azimuth of the point. This is the angle between
      !>              the projection onto the XY plane of the vector from the
      !>              origin to the point and the +X axis of the reference
      !>              frame. AZ is zero at the +X axis.
      !>
      !>              The way azimuth is measured depends on the value of the
      !>              logical flag AZCCW. See the description of the argument
      !>              AZCCW for details.
      !>
      !>              AZ is output in radians. The range of AZ is [0, 2*pi].
      !>
      !>     EL       is the elevation of the point. This is the angle between
      !>              the vector from the origin to the point and the XY
      !>              plane. EL is zero at the XY plane.
      !>
      !>              The way elevation is measured depends on the value of
      !>              the logical flag ELPLSZ. See the description of the
      !>              argument ELPLSZ for details.
      !>
      !>              EL is output in radians. The range of EL is [-pi/2,
      !>              pi/2].
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the X and Y components of RECTAN are both zero, the
      !>         azimuth is set to zero.
      !>
      !>     2)  If RECTAN is the zero vector, azimuth and elevation
      !>         are both set to zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the range, azimuth, and elevation of a point
      !>     specified in rectangular coordinates.
      !>
      !>     The output is defined by the distance from the center of the
      !>     reference frame (range), the angle from a reference vector
      !>     (azimuth), and the angle above the XY plane of the reference
      !>     frame (elevation).
      !>
      !>     The way azimuth and elevation are measured depends on the values
      !>     given by the user to the AZCCW and ELPLSZ logical flags. See the
      !>     descriptions of these input arguments for details.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Create four tables showing a variety of rectangular
      !>        coordinates and the corresponding range, azimuth and
      !>        elevation, resulting from the different choices of the AZCCW
      !>        and ELPLSZ flags.
      !>
      !>        Corresponding rectangular coordinates and azimuth, elevation
      !>        and range are listed to three decimal places. Output angles
      !>        are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECAZL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(30)        MSG
      !>
      !>      DOUBLE PRECISION      AZ
      !>      DOUBLE PRECISION      EL
      !>      DOUBLE PRECISION      RANGE
      !>      DOUBLE PRECISION      RECTAN ( 3, NREC )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               K
      !>      INTEGER               N
      !>
      !>      LOGICAL               AZCCW  ( 2 )
      !>      LOGICAL               ELPLSZ ( 2 )
      !>
      !>C
      !>C     Define the input rectangular coordinates and the
      !>C     different choices of the AZCCW and ELPLSZ flags.
      !>C
      !>      DATA                  RECTAN /
      !>     .                  0.D0,         0.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,         1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,         1.D0,
      !>     .                 -1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,        -1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,        -1.D0,
      !>     .                  1.D0,         1.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         1.D0,
      !>     .                  0.D0,         1.D0,         1.D0,
      !>     .                  1.D0,         1.D0,         1.D0  /
      !>
      !>      DATA                  AZCCW   /  .FALSE.,  .TRUE.  /
      !>      DATA                  ELPLSZ  /  .FALSE.,  .TRUE.  /
      !>
      !>C
      !>C     Create a table for each combination of AZCCW and ELPLSZ.
      !>C
      !>      DO I = 1, 2
      !>
      !>         DO J = 1, 2
      !>
      !>C
      !>C           Display the flag settings.
      !>C
      !>            MSG = 'AZCCW = #; ELPLSZ = #'
      !>            CALL REPML ( MSG, '#', AZCCW(I),  'C', MSG )
      !>            CALL REPML ( MSG, '#', ELPLSZ(J), 'C', MSG )
      !>
      !>            WRITE(*,*)
      !>            WRITE(*,'(A)') MSG
      !>
      !>C
      !>C           Print the banner.
      !>C
      !>            WRITE(*,*)
      !>            WRITE(*,'(A)') '  RECT(1)  RECT(2)  RECT(3) '
      !>     .       //            '  RANGE      AZ       EL'
      !>            WRITE(*,'(A)') '  -------  -------  ------- '
      !>     .       //            ' -------  -------  -------'
      !>
      !>C
      !>C           Do the conversion. Output angles in degrees.
      !>C
      !>            DO N = 1, NREC
      !>
      !>               CALL RECAZL( RECTAN(1,N), AZCCW(I), ELPLSZ(J),
      !>     .                      RANGE,       AZ,       EL        )
      !>
      !>               WRITE (*,'(6F9.3)') ( RECTAN(K,N), K=1,3 ),
      !>     .                           RANGE, AZ * DPR(), EL * DPR()
      !>
      !>            END DO
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        AZCCW = False; ELPLSZ = False
      !>
      !>          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            0.000    1.000    0.000    1.000  270.000    0.000
      !>            0.000    0.000    1.000    1.000    0.000  -90.000
      !>           -1.000    0.000    0.000    1.000  180.000    0.000
      !>            0.000   -1.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000   -1.000    1.000    0.000   90.000
      !>            1.000    1.000    0.000    1.414  315.000    0.000
      !>            1.000    0.000    1.000    1.414    0.000  -45.000
      !>            0.000    1.000    1.000    1.414  270.000  -45.000
      !>            1.000    1.000    1.000    1.732  315.000  -35.264
      !>
      !>        AZCCW = False; ELPLSZ = True
      !>
      !>          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            0.000    1.000    0.000    1.000  270.000    0.000
      !>            0.000    0.000    1.000    1.000    0.000   90.000
      !>           -1.000    0.000    0.000    1.000  180.000    0.000
      !>            0.000   -1.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000   -1.000    1.000    0.000  -90.000
      !>            1.000    1.000    0.000    1.414  315.000    0.000
      !>            1.000    0.000    1.000    1.414    0.000   45.000
      !>            0.000    1.000    1.000    1.414  270.000   45.000
      !>            1.000    1.000    1.000    1.732  315.000   35.264
      !>
      !>        AZCCW = True; ELPLSZ = False
      !>
      !>          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            0.000    1.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000    1.000    1.000    0.000  -90.000
      !>           -1.000    0.000    0.000    1.000  180.000    0.000
      !>            0.000   -1.000    0.000    1.000  270.000    0.000
      !>            0.000    0.000   -1.000    1.000    0.000   90.000
      !>            1.000    1.000    0.000    1.414   45.000    0.000
      !>            1.000    0.000    1.000    1.414    0.000  -45.000
      !>            0.000    1.000    1.000    1.414   90.000  -45.000
      !>            1.000    1.000    1.000    1.732   45.000  -35.264
      !>
      !>        AZCCW = True; ELPLSZ = True
      !>
      !>          RECT(1)  RECT(2)  RECT(3)   RANGE      AZ       EL
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            0.000    1.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000    1.000    1.000    0.000   90.000
      !>           -1.000    0.000    0.000    1.000  180.000    0.000
      !>            0.000   -1.000    0.000    1.000  270.000    0.000
      !>            0.000    0.000   -1.000    1.000    0.000  -90.000
      !>            1.000    1.000    0.000    1.414   45.000    0.000
      !>            1.000    0.000    1.000    1.414    0.000   45.000
      !>            0.000    1.000    1.000    1.414   90.000   45.000
      !>            1.000    1.000    1.000    1.732   45.000   35.264
      !>
      !>     2) Compute the apparent azimuth and elevation of Venus as seen
      !>        from the DSS-14 station.
      !>
      !>        Task Description
      !>        ================
      !>
      !>        In this example, we will obtain the apparent position of
      !>        Venus as seen from the DSS-14 station in the DSS-14 topocentric
      !>        reference frame. We will use a station frames kernel and
      !>        transform the resulting rectangular coordinates to azimuth,
      !>        elevation and range using AZLREC.
      !>
      !>        In order to introduce the usage of the logical flags AZCCW
      !>        and ELPLSZ, we will request the azimuth to be measured
      !>        clockwise and the elevation positive towards the +Z
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
      !>           File name: recazl_ex2.tm
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
      !>                                               binary PCK
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
      !>      PROGRAM RECAZL_EX2
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
      !>      CHARACTER*(*)         FMT0
      !>      PARAMETER           ( FMT0   = '(3F21.8)'  )
      !>
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1   = '(A,F20.8)' )
      !>
      !>      CHARACTER*(*)         META
      !>      PARAMETER           ( META   = 'recazl_ex2.tm' )
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
      !>      DOUBLE PRECISION      EL
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      PTARG  ( 3 )
      !>      DOUBLE PRECISION      R
      !>
      !>      INTEGER               I
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
      !>C     Compute the observer-target position.
      !>C
      !>      CALL SPKPOS ( TARGET, ET, REF, ABCORR, OBS, PTARG, LT )
      !>
      !>C
      !>C     Compute azimuth, elevation and range of Venus
      !>C     as seen from DSS-14, with azimuth increasing
      !>C     clockwise and elevation positive towards +Z
      !>C     axis of the DSS-14_TOPO reference frame
      !>C
      !>      AZCCW  = .FALSE.
      !>      ELPLSZ = .TRUE.
      !>
      !>      CALL RECAZL ( PTARG, AZCCW, ELPLSZ, R, AZ, EL )
      !>
      !>C
      !>C     Express both angles in degrees.
      !>C
      !>      EL =   EL * DPR()
      !>      AZ =   AZ * DPR()
      !>
      !>C
      !>C     Display the computed position, the range and
      !>C     the angles.
      !>C
      !>      WRITE (*,*)
      !>      WRITE (*,'(2A)') 'Target:                ', TARGET
      !>      WRITE (*,'(2A)') 'Observation time:      ', OBSTIM
      !>      WRITE (*,'(2A)') 'Observer center:       ', OBS
      !>      WRITE (*,'(2A)') 'Observer frame:        ', REF
      !>      WRITE (*,'(2A)') 'Aberration correction: ', ABCORR
      !>      WRITE (*,*)
      !>      WRITE (*,'(A)')  'Observer-target position (km):'
      !>      WRITE (*,FMT0)  PTARG
      !>      WRITE (*,FMT1)  'Light time (s):       ', LT
      !>      WRITE (*,*)
      !>      WRITE (*,FMT1) 'Target azimuth          (deg): ', AZ
      !>      WRITE (*,FMT1) 'Target elevation        (deg): ', EL
      !>      WRITE (*,FMT1) 'Observer-target distance (km): ', R
      !>      WRITE (*,*)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Target:                VENUS
      !>        Observation time:      2003 OCT 13 06:00:00.000000 UTC
      !>        Observer center:       DSS-14
      !>        Observer frame:        DSS-14_TOPO
      !>        Aberration correction: CN+S
      !>
      !>        Observer-target position (km):
      !>            66886767.37916669   146868551.77222887  -185296611.10841593
      !>        Light time (s):               819.63862811
      !>
      !>        Target azimuth          (deg):         294.48543372
      !>        Target elevation        (deg):         -48.94609726
      !>        Observer-target distance (km):   245721478.99272084
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
      !>     S.C. Krening       (JPL)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-2021 (JDR) (NJB) (SCK) (BVS)
      pure subroutine recazl ( rectan, azccw, elplsz, range, az, el )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none

      real(wp), intent(in) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      logical, intent(in) :: azccw  !! Flag indicating how Azimuth is measured.
      logical, intent(in) :: elplsz  !! Flag indicating how Elevation is measured.
      real(wp), intent(out) :: range  !! Distance of the point from the origin.
      real(wp), intent(out) :: az  !! Azimuth in radians.
      real(wp), intent(out) :: el  !! Elevation in radians.

!
!     Call the subroutine RECRAD to convert the rectangular coordinates
!     into right ascension and declination.  In RECRAD, the right
!     ascension is measured counterclockwise from +X axis about the +Z
!     axis, and the declination is measured positive from the XY plane
!     towards +Z axis.
!
!     The header of RECRAD says in part:
!
!        The range of RA is [0, 2*pi].
!        The range of DEC is [-pi/2, pi/2].
!
!     The range of AZ in the following call is that of RA.
!     The range of EL is that of DEC.
!
      call recrad ( rectan, range, az, el )

!
!     If AZCCW is set to .FALSE. the azimuth is measured clockwise from
!     the +X axis about the +Z axis.
!
      if ( .not. azccw ) then
!
!        Azimuth increases in the clockwise direction.
!
!        Map AZ to its 2*pi complement if AZ is non-zero. Don't map
!        zero to 2*pi. The value of AZ returned from RECRAD is never
!        negative, but it may be zero.
!
         if ( az > 0.0_wp ) then
!
!            Replace AZ with its 2*pi complement.
!
!            This assignment requires that ACOS( -1.D0 ) be exactly
!            equal to DATAN2( 0.D0, <non-zero value> ). This should be
!            true for any correct arithmetic implementation.
!
!            Although we expect the result to always be non-negative,
!            we take no chances.
!
             az = max( twopi - az, 0.0_wp )

         end if

      end if

!
!     If ELPLSZ is set to .FALSE. the elevation is measured positive
!     from the XY plane toward the -Z axis.
!
      if ( .not. elplsz ) then
!
!        Negate only non-zero values of EL. Avoid creating
!        -0.D0 values, which affect printed outputs generated
!        by example programs.
!
         if ( el /= 0.0_wp ) then
            el = -el
         end if

      end if

      return
      end subroutine recazl

      !> Convert from rectangular to cylindrical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RECTAN   are the rectangular coordinates of the point of
      !>              interest.
      !>
      !>## Detailed Output
      !>
      !>     R        is the distance of the point of interest from Z-axis.
      !>
      !>     CLON     is the cylindrical angle (in radians) of the point of
      !>              interest from XZ plane. The CLON range is [0, 2pi].
      !>
      !>     Z        is the height of the point above XY plane.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine transforms the coordinates of a point from
      !>     rectangular to cylindrical coordinates.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the cylindrical coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to rectangular
      !>        coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: reccyl_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECCYL_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'reccyl_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to cylindrical
      !>C     coordinates.
      !>C
      !>      CALL RECCYL ( POS, R, CLON, Z )
      !>
      !>C
      !>C     Convert the cylindrical to rectangular coordinates.
      !>C
      !>
      !>      CALL CYLREC ( R, CLON, Z, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', R
      !>      WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z          (km): ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from CYLREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius     (km):      383289.01777726
      !>          Longitude (deg):         261.65040211
      !>          Z          (km):     -126505.93063865
      !>
      !>         Rectangular coordinates from CYLREC:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of rectangular coordinates
      !>        and the corresponding cylindrical coordinates.
      !>
      !>        Corresponding rectangular and cylindrical coordinates are
      !>        listed to three decimal places. Output angles are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECCYL_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RECTAN ( 3, NREC )
      !>      DOUBLE PRECISION      Z
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the input rectangular coordinates.
      !>C
      !>      DATA                 RECTAN /
      !>     .                  0.D0,         0.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,         1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,         1.D0,
      !>     .                 -1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,        -1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,        -1.D0,
      !>     .                  1.D0,         1.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         1.D0,
      !>     .                  0.D0,         1.D0,         1.D0,
      !>     .                  1.D0,         1.D0,         1.D0  /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) ' RECT(1)  RECT(2)  RECT(3) '
      !>     . //        '    R       CLON      Z'
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         CALL RECCYL( RECTAN(1,I), R, CLON, Z )
      !>
      !>         WRITE (*,'(6F9.3)') ( RECTAN(J,I), J=1,3 ),
      !>     .                         R, CLON * DPR(), Z
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>          RECT(1)  RECT(2)  RECT(3)     R       CLON      Z
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            0.000    1.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000    1.000    0.000    0.000    1.000
      !>           -1.000    0.000    0.000    1.000  180.000    0.000
      !>            0.000   -1.000    0.000    1.000  270.000    0.000
      !>            0.000    0.000   -1.000    0.000    0.000   -1.000
      !>            1.000    1.000    0.000    1.414   45.000    0.000
      !>            1.000    0.000    1.000    1.000    0.000    1.000
      !>            0.000    1.000    1.000    1.000   90.000    1.000
      !>            1.000    1.000    1.000    1.414   45.000    1.000
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the output argument name LONG to CLON for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF. Obsolete $Revisions section
      !>        deleted.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine reccyl ( rectan, r, clon, z )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none

      real(wp), intent(in) :: rectan(3)  !! Rectangular coordinates of a point.
      real(wp), intent(out) :: r  !! Distance of the point from Z axis.
      real(wp), intent(out) :: clon  !! Angle (radians) of the point from XZ plane
      real(wp), intent(out) :: z  !! Height of the point above XY plane.

      real(wp) :: x
      real(wp) :: y
      real(wp) :: big

!
!     Use temporary variables for computing R.
!
      big = max( dabs(rectan(1)), dabs(rectan(2)) )

!
!     Convert to cylindrical coordinates
!
      z = rectan ( 3 )

      if ( big == 0 ) then
         r      = 0.0_wp
         clon   = 0.0_wp
      else
         x      = rectan(1) / big
         y      = rectan(2) / big

         r      = big * dsqrt (x*x + y*y)

         clon   = datan2 ( y, x )
      end if

      if ( clon < 0.0_wp) then
         clon = clon + twopi
      end if

      return
      end subroutine reccyl

      !> Convert from rectangular coordinates to latitudinal coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RECTAN   are the rectangular coordinates of a point.
      !>
      !>## Detailed Output
      !>
      !>     RADIUS   is the distance of a point from the origin.
      !>
      !>              The units associated with RADIUS are those
      !>              associated with the input RECTAN.
      !>
      !>     LON      is the longitude of the input point. This is the
      !>              angle between the prime meridian and the meridian
      !>              containing the point. The direction of increasing
      !>              longitude is from the +X axis towards the +Y axis.
      !>
      !>              LON is output in radians. The range of LON is
      !>              [ -pi, pi].
      !>
      !>     LAT      is the latitude of the input point. This is the angle
      !>              from the XY plane of the ray from the origin through
      !>              the point.
      !>
      !>              LAT is output in radians. The range of LAT is
      !>              [-pi/2, pi/2].
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the X and Y components of RECTAN are both zero, the
      !>         longitude is set to zero.
      !>
      !>     2)  If RECTAN is the zero vector, longitude and latitude are
      !>         both set to zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the latitudinal coordinates of a point
      !>     whose position is input in rectangular coordinates.
      !>
      !>     Latitudinal coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     above the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the latitudinal coordinates of the position of the
      !>        Moon as seen from the Earth, and convert them to rectangular
      !>        coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: reclat_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECLAT_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'reclat_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to latitudinal
      !>C     coordinates.
      !>C
      !>      CALL RECLAT ( POS, RADIUS, LON, LAT )
      !>
      !>C
      !>C     Convert the latitudinal to rectangular coordinates.
      !>C
      !>
      !>      CALL LATREC ( RADIUS, LON, LAT, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR()
      !>      WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from LATREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius     (km):      403626.33912495
      !>          Longitude (deg):         -98.34959789
      !>          Latitude  (deg):         -18.26566077
      !>
      !>         Rectangular coordinates from LATREC:
      !>
      !>          X          (km):      -55658.44323296
      !>          Y          (km):     -379226.32931475
      !>          Z          (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of rectangular coordinates
      !>        and the corresponding latitudinal coordinates.
      !>
      !>        Corresponding rectangular and latitudinal coordinates are
      !>        listed to three decimal places. Output angles are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECLAT_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3, NREC )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the input rectangular coordinates.
      !>C
      !>      DATA                 RECTAN /
      !>     .                  0.D0,         0.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,         1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,         1.D0,
      !>     .                 -1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,        -1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,        -1.D0,
      !>     .                  1.D0,         1.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         1.D0,
      !>     .                  0.D0,         1.D0,         1.D0,
      !>     .                  1.D0,         1.D0,         1.D0  /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) ' RECT(1)  RECT(2)  RECT(3) '
      !>     . //        '  RADIUS    LON      LAT   '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         CALL RECLAT( RECTAN(1,I), RADIUS, LON, LAT )
      !>
      !>         WRITE (*,'(6F9.3)') ( RECTAN(J,I), J=1,3 ),
      !>     .              RADIUS, LON * DPR(), LAT * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>          RECT(1)  RECT(2)  RECT(3)   RADIUS    LON      LAT
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000    0.000
      !>            0.000    1.000    0.000    1.000   90.000    0.000
      !>            0.000    0.000    1.000    1.000    0.000   90.000
      !>           -1.000    0.000    0.000    1.000  180.000    0.000
      !>            0.000   -1.000    0.000    1.000  -90.000    0.000
      !>            0.000    0.000   -1.000    1.000    0.000  -90.000
      !>            1.000    1.000    0.000    1.414   45.000    0.000
      !>            1.000    0.000    1.000    1.414    0.000   45.000
      !>            0.000    1.000    1.000    1.414   90.000   45.000
      !>            1.000    1.000    1.000    1.732   45.000   35.264
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
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Changed the output argument name LONG to LON for consistency
      !>        with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header changes were made to improve clarity. Some
      !>        minor header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine reclat ( rectan, radius, lon, lat )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      real(wp), intent(out) :: radius  !! Distance of a point from the origin.
      real(wp), intent(out) :: lon  !! Longitude of point in radians.
      real(wp), intent(out) :: lat  !! Latitude of point in radians.

!
!     Local variables.
!
      real(wp) :: x
      real(wp) :: y
      real(wp) :: z
      real(wp) :: big

!
!     Store rectangular coordinates in temporary variables
!
      big = max ( dabs(rectan(1)), dabs(rectan(2)), dabs(rectan(3)) )

      if ( big > 0 ) then

         x      = rectan(1) / big
         y      = rectan(2) / big
         z      = rectan(3) / big

         radius = big * dsqrt (x*x + y*y + z*z)

         lat    = datan2 ( z, dsqrt(x*x + y*y) )

         x      = rectan(1)
         y      = rectan(2)

         if (x==0.0_wp .and. y==0.0_wp) then
            lon = 0.0_wp
         else
            lon = datan2 (y,x)
         end if

      else
         radius = 0.0_wp
         lat    = 0.0_wp
         lon   = 0.0_wp
      end if

      return
      end subroutine reclat

      !> Convert rectangular coordinates to range, right ascension,
      !> and declination.
      !>
      !>## Detailed Input
      !>
      !>     RECTAN   are the rectangular coordinates of a point.
      !>
      !>## Detailed Output
      !>
      !>     RANGE    is the distance of the point from the origin.
      !>
      !>              The units associated with RANGE are those
      !>              associated with the input RECTAN.
      !>
      !>     RA       is the right ascension of RECTAN. This is the angular
      !>              distance measured toward the east from the prime
      !>              meridian to the meridian containing the input point.
      !>              The direction of increasing right ascension is from
      !>              the +X axis towards the +Y axis.
      !>
      !>              RA is output in radians. The range of RA is [0, 2*pi].
      !>
      !>     DEC      is the declination of RECTAN. This is the angle from
      !>              the XY plane of the ray from the origin through the
      !>              point.
      !>
      !>              DEC is output in radians. The range of DEC is
      !>              [-pi/2, pi/2].
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the X and Y components of RECTAN are both zero, the
      !>         right ascension is set to zero.
      !>
      !>     2)  If RECTAN is the zero vector, right ascension and declination
      !>         are both set to zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the range, right ascension, and declination
      !>     of a point specified in rectangular coordinates.
      !>
      !>     The output is defined by a distance from a central reference
      !>     point, an angle from a reference meridian, and an angle above
      !>     the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>## Examples
      !>
      !>     The following code fragment converts right ascension and
      !>     declination from the B1950 reference frame to the J2000 frame.
      !>
      !>        C
      !>        C     Convert RA and DEC to a 3-vector expressed in
      !>        C     the B1950 frame.
      !>        C
      !>              CALL RADREC ( 1.D0, RA, DEC, V1950 )
      !>        C
      !>        C     We use the SPICELIB routine PXFORM to obtain the
      !>        C     transformation  matrix for converting vectors between
      !>        C     the B1950 and J2000 reference frames. Since
      !>        C     both frames are inertial, the input time value we
      !>        C     supply to PXFORM is arbitrary. We choose zero
      !>        C     seconds past the J2000 epoch.
      !>        C
      !>              CALL PXFORM ( 'B1950', 'J2000', 0.D0, MTRANS )
      !>        C
      !>        C     Transform the vector to the J2000 frame.
      !>        C
      !>              CALL MXV ( MTRANS, V1950, V2000 )
      !>        C
      !>        C     Find the RA and DEC of the J2000-relative vector.
      !>        C
      !>              CALL RECRAD ( V2000, R, RA, DEC )
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
      !>     C.H. Acton         (JPL)
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
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
      !>-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header changes were made to improve clarity. Some
      !>        minor header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
      pure subroutine recrad ( rectan, range, ra, dec )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none

      real(wp), intent(in) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp), intent(out) :: range  !! Distance of the point from the origin.
      real(wp), intent(out) :: ra  !! Right ascension in radians.
      real(wp), intent(out) :: dec  !! Declination in radians.

!
!     Call the subroutine RECLAT to convert the rectangular coordinates
!     into latitudinal coordinates.  In RECLAT, the longitude ( which
!     is returned to this subroutine as RA ) ranges from - pi to pi
!     radians.   Because the right ascension ranges from zero to
!     two pi radians, whenever RA is negative two pi must be added to
!     it.
!
      call reclat ( rectan, range, ra, dec )

      if ( ra < 0 ) then
         ra = ra + twopi
      end if

      return
      end subroutine recrad

      !> Convert from rectangular coordinates to spherical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RECTAN   are the rectangular coordinates of a point.
      !>
      !>## Detailed Output
      !>
      !>     R        is the distance of the point from the origin.
      !>
      !>     COLAT    is the angle between the point and the positive Z-axis in
      !>              radians. The range of COLAT is [0, pi].
      !>
      !>     SLON     is the longitude of the point in radians. This is the
      !>              angle between the positive X-axis and the orthogonal
      !>              projection of the point onto the XY plane. SLON increases
      !>              in the counterclockwise sense about the positive Z-axis.
      !>              The range of SLON is [-pi, pi].
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the spherical coordinates of a point
      !>     whose position is input in rectangular coordinates.
      !>
      !>     Spherical coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     from the z-axis.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the spherical coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to rectangular
      !>        coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: recsph_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECSPH_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'recsph_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to spherical
      !>C     coordinates.
      !>C
      !>      CALL RECSPH ( POS, RADIUS, COLAT, SLON )
      !>
      !>C
      !>C     Convert the spherical coordinates to rectangular.
      !>C
      !>      CALL SPHREC ( RADIUS, COLAT, SLON, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from SPHREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Colatitude (deg):         108.26566077
      !>          Longitude  (deg):         -98.34959789
      !>
      !>         Rectangular coordinates from SPHREC:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of rectangular coordinates
      !>        and the corresponding spherical coordinates.
      !>
      !>        Corresponding rectangular and spherical coordinates are
      !>        listed to three decimal places. Output angles in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM RECSPH_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3, NREC )
      !>      DOUBLE PRECISION      SLON
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the input rectangular coordinates.
      !>C
      !>      DATA                 RECTAN /
      !>     .                  0.D0,         0.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,         1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,         1.D0,
      !>     .                 -1.D0,         0.D0,         0.D0,
      !>     .                  0.D0,        -1.D0,         0.D0,
      !>     .                  0.D0,         0.D0,        -1.D0,
      !>     .                  1.D0,         1.D0,         0.D0,
      !>     .                  1.D0,         0.D0,         1.D0,
      !>     .                  0.D0,         1.D0,         1.D0,
      !>     .                  1.D0,         1.D0,         1.D0  /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) ' RECT(1)  RECT(2)  RECT(3) '
      !>     . //        '  RADIUS   COLAT     SLON  '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         CALL RECSPH( RECTAN(1,I), RADIUS, COLAT, SLON )
      !>
      !>         WRITE (*,'(6F9.3)') ( RECTAN(J,I), J=1,3 ),
      !>     .                        RADIUS, COLAT * DPR(),
      !>     .                                SLON  * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>          RECT(1)  RECT(2)  RECT(3)   RADIUS   COLAT     SLON
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000    0.000    0.000    1.000   90.000    0.000
      !>            0.000    1.000    0.000    1.000   90.000   90.000
      !>            0.000    0.000    1.000    1.000    0.000    0.000
      !>           -1.000    0.000    0.000    1.000   90.000  180.000
      !>            0.000   -1.000    0.000    1.000   90.000  -90.000
      !>            0.000    0.000   -1.000    1.000  180.000    0.000
      !>            1.000    1.000    0.000    1.414   90.000   45.000
      !>            1.000    0.000    1.000    1.414   45.000    0.000
      !>            0.000    1.000    1.000    1.414   45.000   90.000
      !>            1.000    1.000    1.000    1.732   54.736   45.000
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the argument name LONG to SLON for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples. Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.2, 07-JAN-2002 (NJB)
      !>
      !>        Fixed description of SLON in $Brief_I/O and Detailed_I/O
      !>        header sections.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine recsph ( rectan, r, colat, slon  )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp), intent(out) :: r  !! Distance of the point from the origin.
      real(wp), intent(out) :: colat  !! Angle of the point from the Z-axis (radians)
      real(wp), intent(out) :: slon  !! Longitude of the point (radians).

      real(wp) :: x
      real(wp) :: y
      real(wp) :: z
      real(wp) :: big

!
!     Store rectangular coordinates in temporary variables
!
      big = max ( dabs(rectan(1)), dabs(rectan(2)), dabs(rectan(3)) )

      if ( big > 0 ) then

         x      = rectan(1) / big
         y      = rectan(2) / big
         z      = rectan(3) / big

         r      = big * dsqrt (x*x + y*y + z*z)

         colat  = datan2 ( dsqrt(x*x + y*y), z )

         x      = rectan(1)
         y      = rectan(2)

         if (x==0.0_wp .and. y==0.0_wp) then
            slon  = 0.0_wp
         else
            slon  = datan2 (y,x)
         end if

      else
         r     = 0.0_wp
         colat = 0.0_wp
         slon  = 0.0_wp
      end if

      return
      end subroutine recsph

      !> Adjust the columns of a matrix that is "nearly" a rotation
      !> so that they are numerically unit length and orthogonal,
      !> going from left to right in the usual printed presentation
      !> of a matrix.
      !>
      !>## Detailed Input
      !>
      !>     ROT      is a 3x3 matrix that is nearly a rotation matrix.
      !>
      !>## Detailed Output
      !>
      !>     ROT      is the input after sharpening the columns.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  This routine is not meant to be used on singular or near-
      !>         singular matrices (in other words, matrices with determinant
      !>         close to zero).
      !>
      !>         If the input matrix is singular, the output matrix may not
      !>         be a rotation matrix. In any case, the results should be
      !>         considered unreliable in this case.
      !>
      !>         No error handling is done for invalid input matrices.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine "sharpens" the orthogonality of a potential
      !>     rotation matrix. It is intended for use in those situations
      !>     in which you have a rotation matrix that may be derived
      !>     from single precision inputs or that may have experienced
      !>     round off errors in its construction.
      !>
      !>## Examples
      !>
      !>     Suppose that you have a rotation matrix that needs to be
      !>     converted to a quaternion. The SPICE matrix to quaternion
      !>     conversion routine M2Q performs error checks on the input
      !>     matrix and signals an error if it does not meet the checks
      !>     for a quaternion. By calling this routine you can ensure that
      !>     your rotation matrix (provided it's non-singular) will pass
      !>     the restrictions imposed by M2Q.
      !>
      !>         CALL SHARPR ( ROT )
      !>         CALL M2Q    ( ROT, Q )
      !>
      !>## Restrictions
      !>
      !>     1)  See the $Exceptions section above.
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
      !>-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 13-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VHAT call. Some header updates were made.
      !>
      !>-    SPICELIB Version 1.0.0, 16-SEP-1999 (WLT)
      pure subroutine sharpr ( rot )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(inout) :: rot ( 3, 3 )  !! The rotation matrix to be sharpened.

!
!     Unitize the first column of the rotation.
!
      call vhatip ( rot(1,1) )

!
!     Unitize the third column of the rotation and make it
!     orthogonal to the first two columns.
!
      call ucrss ( rot(1,1),  rot(1,2),  rot(1,3) )

!
!     Unitize the second column of the rotation and make it
!     orthogonal to the first and third columns.
!
      call ucrss ( rot(1,3),  rot(1,1),  rot(1,2) )

      return
      end subroutine sharpr

      !> Return .TRUE. if two given double precision numbers have the same
      !> sign.
      !>
      !> The function returns .TRUE. if the input arguments have the same
      !> sign.
      !>
      !>## Detailed Input
      !>
      !>     X        is any double precision number.
      !>
      !>     Y        is any double precision number.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if X and Y are both positive or both
      !>     negative. Otherwise, it returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the value:
      !>
      !>           (      (( X .GT. 0) .AND. (Y .GT. 0))
      !>             .OR. (( X .LT. 0) .AND. (Y .LT. 0)) )
      !>
      !>     This is a more stable value than
      !>
      !>           ( X*Y .GT. 0 )
      !>
      !>     Note: If either of the to inputs is zero. The result returned
      !>     will be .FALSE.
      !>
      !>## Examples
      !>
      !>     This routine can be used whenever a decision depends upon two
      !>     Double Precision values having the same sign.
      !>
      !>     IF ( SMSGND ( F(X1), F(X2) ) ) THEN
      !>           .
      !>           .
      !>        do something
      !>           .
      !>           .
      !>     ELSE
      !>           .
      !>           .
      !>        find a root of F lying between X1 and X2
      !>           .
      !>           .
      !>     END IF
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
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental logical function smsgnd ( x, y )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: x  !! A double precision number
      real(wp), intent(in) :: y  !! A double precision number

      smsgnd = (      (( x > 0 ) .and. ( y > 0 )) &
                 .or. (( x < 0 ) .and. ( y < 0 )) &
               )

      return
      end function smsgnd

      !> Return .TRUE. if two given integer numbers have the same sign.
      !>
      !> The function returns .TRUE. if the input arguments have the same
      !> sign.
      !>
      !>## Detailed Input
      !>
      !>     X        is any integer.
      !>
      !>     Y        is any integer.
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if X and Y are both positive or both
      !>     negative. Otherwise, it returns .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the value:
      !>
      !>           (      (( X .GT. 0) .AND. (Y .GT. 0))
      !>             .OR. (( X .LT. 0) .AND. (Y .LT. 0)) )
      !>
      !>     This is a more stable value than
      !>
      !>           ( X*Y .GT. 0 )
      !>
      !>     Note: If either of the to inputs is zero. The result returned
      !>     will be .FALSE.
      !>
      !>## Examples
      !>
      !>     This routine can be used whenever a decision depends upon two
      !>     integer values having the same sign.
      !>
      !>     IF ( SMSGNI ( F(X1), F(X2) ) ) THEN
      !>           .
      !>           .
      !>        do something
      !>           .
      !>           .
      !>     ELSE
      !>           .
      !>           .
      !>        find a root of F lying between X1 and X2
      !>           .
      !>           .
      !>     END IF
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
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental logical function smsgni ( x, y )
      use pepper_kinds, only: ip
      implicit none

      integer(ip), intent(in) :: x  !! An integer.
      integer(ip), intent(in) :: y  !! An integer.

      smsgni = (      (( x > 0 ) .and. ( y > 0 )) &
                 .or. (( x < 0 ) .and. ( y < 0 )) &
               )

      return
      end function smsgni

      !> Convert from spherical coordinates to cylindrical coordinates.
      !>
      !>## Detailed Input
      !>
      !>     RADIUS   is the distance of the point from origin.
      !>
      !>     COLAT    is the polar angle (co-latitude in radians) of the
      !>              point.
      !>
      !>     SLON     is the azimuthal angle (longitude) of the point
      !>              (radians).
      !>
      !>## Detailed Output
      !>
      !>     R        is the distance of the point of interest from Z-axis.
      !>
      !>     CLON     is the cylindrical angle (radians) of the point from
      !>              the XZ plane. CLON is set equal to SLON.
      !>
      !>     Z        is the height of the point above XY plane.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This returns the cylindrical coordinates of a point whose
      !>     position is input through spherical coordinates.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the spherical coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to cylindrical and
      !>        rectangular coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: sphcyl_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHCYL_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'sphcyl_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to spherical
      !>C     coordinates.
      !>C
      !>      CALL RECSPH ( POS, RADIUS, COLAT, SLON )
      !>
      !>C
      !>C     Convert the spherical coordinates to cylindrical.
      !>C
      !>      CALL SPHCYL ( RADIUS, COLAT, SLON, R, CLON, Z )
      !>
      !>C
      !>C     Convert the cylindrical coordinates to rectangular.
      !>C
      !>      CALL CYLREC ( R, CLON, Z, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Cylindrical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', R
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z           (km): ', Z
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from CYLREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Colatitude (deg):         108.26566077
      !>          Longitude  (deg):         -98.34959789
      !>
      !>         Cylindrical coordinates:
      !>
      !>          Radius      (km):      383289.01777726
      !>          Longitude  (deg):         -98.34959789
      !>          Z           (km):     -126505.93063865
      !>
      !>         Rectangular coordinates from CYLREC:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of spherical coordinates
      !>        and the corresponding cylindrical coordinates.
      !>
      !>        Corresponding spherical and cylindrical coordinates are
      !>        listed to three decimal places. Input and output angles are
      !>        in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHCYL_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      COLAT  ( NREC )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS ( NREC )
      !>      DOUBLE PRECISION      RCOLAT
      !>      DOUBLE PRECISION      RSLON
      !>      DOUBLE PRECISION      SLON   ( NREC )
      !>      DOUBLE PRECISION      Z
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input spherical coordinates. Input angles
      !>C     in degrees.
      !>C
      !>      DATA                 RADIUS / 0.D0, 1.D0,     1.D0,
      !>     .                              1.D0, 1.4142D0, 1.D0,
      !>     .                              1.D0, 1.D0,     1.4142D0,
      !>     .                              1.D0, 0.D0               /
      !>
      !>      DATA                 COLAT /  0.D0,   90.D0,  90.D0,
      !>     .                              0.D0,   45.D0,  90.D0,
      !>     .                            180.D0,   90.D0, 135.D0,
      !>     .                              0.D0,   90.D0            /
      !>
      !>      DATA                 SLON  /  0.D0,    0.D0,  90.D0,
      !>     .                              0.D0,  180.D0, -90.D0,
      !>     .                              0.D0,   45.D0, 180.D0,
      !>     .                             180.D0,  33.D0            /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '  RADIUS   COLAT     SLON  '
      !>     . //        '    R       CLON      Z    '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RCOLAT = COLAT(I) * RPD()
      !>         RSLON  = SLON(I)  * RPD()
      !>
      !>         CALL SPHCYL( RADIUS(I), RCOLAT, RSLON, R, CLON, Z )
      !>
      !>         WRITE (*,'(6F9.3)') RADIUS(I), COLAT(I), SLON(I),
      !>     .                       R, CLON * DPR(), Z
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>           RADIUS   COLAT     SLON      R       CLON      Z
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000   90.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000   90.000    1.000   90.000    0.000
      !>            1.000    0.000    0.000    0.000    0.000    1.000
      !>            1.414   45.000  180.000    1.000  180.000    1.000
      !>            1.000   90.000  -90.000    1.000  -90.000    0.000
      !>            1.000  180.000    0.000    0.000    0.000   -1.000
      !>            1.000   90.000   45.000    1.000   45.000    0.000
      !>            1.414  135.000  180.000    1.000  180.000   -1.000
      !>            1.000    0.000  180.000    0.000  180.000    1.000
      !>            0.000   90.000   33.000    0.000   33.000    0.000
      !>
      !>     3) Other than the obvious conversion between coordinate systems
      !>        this routine could be used to obtain the axial projection
      !>        from a sphere to a cylinder about the z-axis that contains
      !>        the equator of the sphere.
      !>
      !>        Such a projection is valuable because it preserves the
      !>        areas between regions on the sphere and their projections to
      !>        the cylinder.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHCYL_EX3
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         FMT1
      !>      PARAMETER           ( FMT1 = '(A,F23.11)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Define the point whose projection is to be
      !>C     computed.
      !>C
      !>      RADIUS =   100.D0
      !>      SLON   =    45.D0  * RPD()
      !>      COLAT  =   102.5D0 * RPD()
      !>
      !>C
      !>C     Convert the spherical coordinates to cylindrical.
      !>C
      !>      CALL SPHCYL ( RADIUS, COLAT, SLON, R, CLON, Z )
      !>
      !>      WRITE(*,*) 'Coordinates of the projected point on '
      !>     .        // 'cylinder:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius     (km): ', R
      !>      WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR()
      !>      WRITE(*,FMT1) '  Z          (km): ', Z
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Coordinates of the projected point on cylinder:
      !>
      !>          Radius     (km):          97.62960071199
      !>          Longitude (deg):          45.00000000000
      !>          Z          (km):         -21.64396139381
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Changed the argument names LONGS and LONG to SLON and CLON for
      !>        consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.2, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental subroutine sphcyl ( radius, colat, slon, r, clon, z )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: radius  !! Distance of point from origin.
      real(wp), intent(in) :: colat  !! Polar angle (co-latitude in radians) of point.
      real(wp), intent(in) :: slon  !! Azimuthal angle (longitude) of point (radians).
      real(wp), intent(out) :: r  !! Distance of point from Z axis.
      real(wp), intent(out) :: clon  !! Angle (radians) of point from XZ plane.
      real(wp), intent(out) :: z  !! Height of point above XY plane.

      real(wp) :: rr
      real(wp) :: zz

!
!     Convert to cylindrical coordinates, storing the results in
!     temporary variables.
!
      rr = radius*dsin(colat)
      zz = radius*dcos(colat)

!
!     Move the results to the output variables.
!
      clon   = slon
      r      = rr
      z      = zz

      return
      end subroutine sphcyl

      !> Convert from spherical coordinates to latitudinal coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of the point from the origin.
      !>
      !>     COLAT    is the angle between the vector from the origin to the
      !>              point and the positive Z-axis in radians.
      !>
      !>     SLON     is the angle of the point from the XZ plane (radians).
      !>
      !>## Detailed Output
      !>
      !>     RADIUS   is the distance of a point from the origin.
      !>
      !>     LON      is the angle of the point from the XZ plane in
      !>              radians. LON is set equal to SLON.
      !>
      !>     LAT      is the angle of the point from the XY plane in
      !>              radians. LAT is computed as pi/2 - COLAT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the latitudinal coordinates of a point
      !>     whose position is input in spherical coordinates.
      !>
      !>     Latitudinal coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     above the equator of a sphere centered at the central reference
      !>     point.
      !>
      !>     Spherical coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     from the Z-axis.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Latitude is obtained by subtracting co-latitude from HALFPI
      !>        Radius and longitude mean the same thing in both latitudinal
      !>        and spherical coordinates. The table below lists LAT and
      !>        corresponding COLAT in terms of degrees.
      !>
      !>             LAT     COLAT
      !>            -----    -----
      !>               0        90
      !>              20        70
      !>              45        45
      !>             -30       120
      !>              90         0
      !>             -45       135
      !>
      !>     2) Compute the spherical coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to latitudinal and
      !>        rectangular coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: sphlat_ex2.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHLAT_EX2
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'sphlat_ex2.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to spherical
      !>C     coordinates.
      !>C
      !>      CALL RECSPH ( POS, R, COLAT, SLON )
      !>
      !>C
      !>C     Convert the spherical coordinates to latitudinal.
      !>C
      !>      CALL SPHLAT ( R, COLAT, SLON, RADIUS, LON, LAT )
      !>
      !>C
      !>C     Convert the latitudinal coordinates to rectangular.
      !>C
      !>      CALL LATREC ( RADIUS, LON, LAT, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', R
      !>      WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Latitudinal coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', LON*DPR()
      !>      WRITE(*,FMT1) '  Latitude   (deg): ', LAT*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from LATREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Colatitude (deg):         108.26566077
      !>          Longitude  (deg):         -98.34959789
      !>
      !>         Latitudinal coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Longitude  (deg):         -98.34959789
      !>          Latitude   (deg):         -18.26566077
      !>
      !>         Rectangular coordinates from LATREC:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>     3) Create a table showing a variety of spherical coordinates
      !>        and the corresponding cylindrical coordinates.
      !>
      !>        Corresponding spherical and cylindrical coordinates are
      !>        listed to three decimal places. Input and output angles
      !>        are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHLAT_EX3
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      COLAT  ( NREC )
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      R      ( NREC )
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RCOLAT
      !>      DOUBLE PRECISION      RSLON
      !>      DOUBLE PRECISION      SLON   ( NREC )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input spherical coordinates. Angles in
      !>C     degrees.
      !>C
      !>      DATA                 R      / 0.D0, 1.D0,     1.D0,
      !>     .                              1.D0, 1.4142D0, 1.D0,
      !>     .                              1.D0, 1.D0,     1.4142D0,
      !>     .                              1.D0, 0.D0               /
      !>
      !>      DATA                 COLAT /  0.D0,   90.D0,  90.D0,
      !>     .                              0.D0,   45.D0,  90.D0,
      !>     .                            180.D0,   90.D0, 135.D0,
      !>     .                              0.D0,   90.D0            /
      !>
      !>      DATA                 SLON  /  0.D0,    0.D0,  90.D0,
      !>     .                              0.D0,  180.D0, -90.D0,
      !>     .                              0.D0,   45.D0, 180.D0,
      !>     .                             180.D0,  33.D0            /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '     R     COLAT     SLON  '
      !>     . //        '  RADIUS    LON      LAT   '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion. Output angles in degrees.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RCOLAT = COLAT(I) * RPD()
      !>         RSLON  = SLON(I)  * RPD()
      !>
      !>         CALL SPHLAT( R(I), RCOLAT, RSLON, RADIUS, LON, LAT )
      !>
      !>         WRITE (*,'(6F9.3)') R(I), COLAT(I), SLON(I),
      !>     .                       RADIUS, LON * DPR(), LAT * DPR()
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>              R     COLAT     SLON    RADIUS    LON      LAT
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000   90.000
      !>            1.000   90.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000   90.000    1.000   90.000    0.000
      !>            1.000    0.000    0.000    1.000    0.000   90.000
      !>            1.414   45.000  180.000    1.414  180.000   45.000
      !>            1.000   90.000  -90.000    1.000  -90.000    0.000
      !>            1.000  180.000    0.000    1.000    0.000  -90.000
      !>            1.000   90.000   45.000    1.000   45.000    0.000
      !>            1.414  135.000  180.000    1.414  180.000  -45.000
      !>            1.000    0.000  180.000    1.000  180.000   90.000
      !>            0.000   90.000   33.000    0.000   33.000    0.000
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Changed the argument names LONGS and LONG to SLON and LON for
      !>        consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.2, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      elemental subroutine sphlat ( r, colat, slon, radius, lon, lat )
      use pepper_kinds, only: wp
      use pepper_constants, only: halfpi
      implicit none

      real(wp), intent(in) :: r  !! Distance of the point from the origin.
      real(wp), intent(in) :: colat  !! Angle of the point from positive Z axis (radians).
      real(wp), intent(in) :: slon  !! Angle of the point from the XZ plane (radians).
      real(wp), intent(out) :: radius  !! Distance of a point from the origin
      real(wp), intent(out) :: lon  !! Angle of the point from the XZ plane in radians
      real(wp), intent(out) :: lat  !! Angle of the point from the XY plane in radians

      real(wp) :: rr
      real(wp) :: lattud

!
!     Convert to latitudinal coordinates, storing the results in
!     temporary variables
!
      rr     = r
      lattud = halfpi - colat

!
!     Move the results to the output variables.
!
      lon    = slon
      radius = rr
      lat    = lattud

      return
      end subroutine sphlat

      !> Convert from spherical coordinates to rectangular coordinates.
      !>
      !>## Detailed Input
      !>
      !>     R        is the distance of the point from the origin.
      !>
      !>     COLAT    is the angle between the point and the positive
      !>              Z-axis in radians.
      !>
      !>     SLON     is the angle of the projection of the point to the
      !>              XY plane from the positive X-axis in radians. The
      !>              positive Y-axis is at longitude PI/2 radians.
      !>
      !>## Detailed Output
      !>
      !>     RECTAN   are the rectangular coordinates of a point.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine returns the rectangular coordinates of a point
      !>     whose position is input in spherical coordinates.
      !>
      !>     Spherical coordinates are defined by a distance from a central
      !>     reference point, an angle from a reference meridian, and an angle
      !>     from the Z-axis. The co-latitude of the positive Z-axis is
      !>     zero. The longitude of the positive Y-axis is PI/2 radians.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the spherical coordinates of the position of the Moon
      !>        as seen from the Earth, and convert them to rectangular
      !>        coordinates.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: sphrec_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'de421.bsp',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHREC_EX1
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
      !>      PARAMETER           ( FMT1 = '(A,F20.8)' )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      CLON
      !>      DOUBLE PRECISION      COLAT
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      RADIUS
      !>      DOUBLE PRECISION      RECTAN ( 3 )
      !>      DOUBLE PRECISION      SLON
      !>      DOUBLE PRECISION      Z
      !>
      !>C
      !>C     Load SPK and LSK kernels, use a meta kernel for
      !>C     convenience.
      !>C
      !>      CALL FURNSH ( 'sphrec_ex1.tm' )
      !>
      !>C
      !>C     Look up the geometric state of the Moon as seen from
      !>C     the Earth at 2017 Mar 20, relative to the J2000
      !>C     reference frame.
      !>C
      !>      CALL STR2ET ( '2017 Mar 20', ET )
      !>
      !>      CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
      !>     .              'Earth', POS, LT               )
      !>
      !>C
      !>C     Convert the position vector POS to spherical
      !>C     coordinates.
      !>C
      !>      CALL RECSPH ( POS, RADIUS, COLAT, SLON )
      !>
      !>C
      !>C     Convert the spherical coordinates to rectangular.
      !>C
      !>      CALL SPHREC ( RADIUS, COLAT, SLON, RECTAN )
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Original rectangular coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', POS(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', POS(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', POS(3)
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Spherical coordinates:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  Radius      (km): ', RADIUS
      !>      WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR()
      !>      WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR()
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Rectangular coordinates from SPHREC:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,FMT1) '  X           (km): ', RECTAN(1)
      !>      WRITE(*,FMT1) '  Y           (km): ', RECTAN(2)
      !>      WRITE(*,FMT1) '  Z           (km): ', RECTAN(3)
      !>      WRITE(*,*) ' '
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Original rectangular coordinates:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>         Spherical coordinates:
      !>
      !>          Radius      (km):      403626.33912495
      !>          Colatitude (deg):         108.26566077
      !>          Longitude  (deg):         -98.34959789
      !>
      !>         Rectangular coordinates from SPHREC:
      !>
      !>          X           (km):      -55658.44323296
      !>          Y           (km):     -379226.32931475
      !>          Z           (km):     -126505.93063865
      !>
      !>     2) Create a table showing a variety of spherical coordinates
      !>        and the corresponding rectangular coordinates.
      !>
      !>        Corresponding spherical and rectangular coordinates are
      !>        listed to three decimal places. Input angles are in degrees.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SPHREC_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      DPR
      !>      DOUBLE PRECISION      RPD
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NREC
      !>      PARAMETER           ( NREC = 11 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      COLAT  ( NREC )
      !>      DOUBLE PRECISION      RADIUS ( NREC )
      !>      DOUBLE PRECISION      RCOLAT
      !>      DOUBLE PRECISION      RSLON
      !>      DOUBLE PRECISION      SLON   ( NREC )
      !>      DOUBLE PRECISION      RECTAN ( 3    )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the input spherical coordinates. Angles in
      !>C     degrees.
      !>C
      !>      DATA                 RADIUS / 0.D0, 1.D0,         1.D0,
      !>     .                              1.D0, 1.D0,         1.D0,
      !>     .                              1.D0, 1.4142D0, 1.4142D0,
      !>     .                          1.4142D0, 1.7320D0           /
      !>
      !>      DATA                 COLAT /  0.D0,   90.D0,  90.D0,
      !>     .                              0.D0,   90.D0,  90.D0,
      !>     .                            180.D0,   90.D0,  45.D0,
      !>     .                             45.D0,   54.7356D0        /
      !>
      !>      DATA                 SLON  /  0.D0,    0.D0,  90.D0,
      !>     .                              0.D0,  180.D0, -90.D0,
      !>     .                              0.D0,   45.D0,   0.D0,
      !>     .                              90.D0,  45.D0            /
      !>
      !>C
      !>C     Print the banner.
      !>C
      !>      WRITE(*,*) '  RADIUS   COLAT     SLON  '
      !>     . //        ' RECT(1)  RECT(2)  RECT(3) '
      !>      WRITE(*,*) ' -------  -------  ------- '
      !>     . //        ' -------  -------  ------- '
      !>
      !>C
      !>C     Do the conversion.
      !>C
      !>      DO I = 1, NREC
      !>
      !>         RCOLAT = COLAT(I) * RPD()
      !>         RSLON  = SLON(I)  * RPD()
      !>
      !>         CALL SPHREC( RADIUS(I), RCOLAT, RSLON, RECTAN )
      !>
      !>         WRITE (*,'(6F9.3)') RADIUS(I), COLAT(I), SLON(I),
      !>     .                       RECTAN
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>           RADIUS   COLAT     SLON   RECT(1)  RECT(2)  RECT(3)
      !>          -------  -------  -------  -------  -------  -------
      !>            0.000    0.000    0.000    0.000    0.000    0.000
      !>            1.000   90.000    0.000    1.000    0.000    0.000
      !>            1.000   90.000   90.000    0.000    1.000    0.000
      !>            1.000    0.000    0.000    0.000    0.000    1.000
      !>            1.000   90.000  180.000   -1.000    0.000    0.000
      !>            1.000   90.000  -90.000    0.000   -1.000    0.000
      !>            1.000  180.000    0.000    0.000    0.000   -1.000
      !>            1.414   90.000   45.000    1.000    1.000    0.000
      !>            1.414   45.000    0.000    1.000    0.000    1.000
      !>            1.414   45.000   90.000    0.000    1.000    1.000
      !>            1.732   54.736   45.000    1.000    1.000    1.000
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Changed the argument name LONG to SLON for consistency with
      !>        other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete code examples.
      !>
      !>-    SPICELIB Version 1.0.4, 26-JUL-2016 (BVS)
      !>
      !>        Minor headers edits.
      !>
      !>-    SPICELIB Version 1.0.3, 24-SEP-1997 (WLT)
      !>
      !>        The BRIEF I/O section was corrected so that it
      !>        correctly reflects the inputs and outputs.
      !>
      !>-    SPICELIB Version 1.0.2, 12-JUL-1995 (WLT)
      !>
      !>        The header documentation was corrected so that longitude
      !>        now is correctly described as the angle from the
      !>        XZ plane instead of XY.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine sphrec ( r, colat, slon, rectan  )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: r  !! Distance of a point from the origin.
      real(wp), intent(in) :: colat  !! Angle of the point from the Z-axis in radians.
      real(wp), intent(in) :: slon  !! Angle of the point from the XZ plane in radians.
      real(wp), intent(out) :: rectan ( 3 )  !! Rectangular coordinates of the point.

      real(wp) :: x
      real(wp) :: y
      real(wp) :: z

!
!     Convert to rectangular coordinates, storing in the results in
!     temporary variables
!
      x = r * dcos(slon) * dsin(colat)
      y = r * dsin(slon) * dsin(colat)
      z = r * dcos(colat)

!
!     Move the results to the output variables
!
      rectan(1) = x
      rectan(2) = y
      rectan(3) = z

      return
      end subroutine sphrec

      !> Return the trace of a 3x3 matrix.
      !>
      !> The function returns the trace of MATRIX.
      !>
      !>## Detailed Input
      !>
      !>     MATRIX   is a double precision 3x3 matrix.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the trace of MATRIX, i.e. it is the sum of
      !>     the diagonal elements of MATRIX.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical
      !>     expression:
      !>
      !>                  3
      !>                .----
      !>                 \
      !>        TRACE =   )  MATRIX(I,I)
      !>                 /
      !>                '----
      !>                 I=1
      !>
      !>     No error detection or correction is implemented within this
      !>     function.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x3 double precision matrix, compute its trace.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TRACE_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      TRACE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      MATRIX ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define MATRIX.
      !>C
      !>      DATA                  MATRIX  /  3.D0,  0.D0,  4.D0,
      !>     .                                 5.D0, -2.D0,  0.D0,
      !>     .                                 7.D0,  8.D0, -1.D0  /
      !>
      !>      WRITE(*,'(A)') 'MATRIX:'
      !>      DO I=1, 3
      !>
      !>         WRITE(*,'(3F6.1)') ( MATRIX(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>C
      !>C     Compute the trace of MATRIX and display the result.
      !>C
      !>      WRITE(*,*)
      !>      WRITE(*,'(A,F4.1)') 'Trace: ', TRACE ( MATRIX )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        MATRIX:
      !>           3.0   5.0   7.0
      !>           0.0  -2.0   8.0
      !>           4.0   0.0  -1.0
      !>
      !>        Trace:  0.0
      !>
      !>## Restrictions
      !>
      !>     1)  No checking is performed to guard against floating point
      !>         overflow or underflow. This routine should probably not be
      !>         used if the input matrix is expected to have large double
      !>         precision numbers along the diagonal.
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
      !>-    SPICELIB Version 1.0.2, 03-JUL-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure real(wp) function  trace ( matrix )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: matrix ( 3, 3 )  !! 3x3 matrix of double precision numbers.

      trace = matrix(1,1) + matrix(2,2) + matrix(3,3)

      return
      end function trace

      !> Return the trace of a square matrix of arbitrary dimension.
      !>
      !> The function returns the trace of the square matrix of arbitrary
      !> dimension MATRIX.
      !>
      !>## Detailed Input
      !>
      !>     MATRIX   is a double precision square matrix of arbitrary
      !>              dimension. The input matrix must be square or else the
      !>              concept is meaningless.
      !>
      !>     NDIM     is the dimension of MATRIX.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the trace of MATRIX, i.e. it is the sum of
      !>     the diagonal elements of MATRIX.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical
      !>     expression:
      !>
      !>                   NDIM
      !>                 .------
      !>                  \
      !>        TRACEG =   ) MATRIX(I,I)
      !>                  /
      !>                 '------
      !>                    I=1
      !>
      !>     No error detection or correction is implemented within this
      !>     function.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 4x4 double precision matrix, compute its trace.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TRACEG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      TRACEG
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      MATRIX ( NDIM, NDIM )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define MATRIX.
      !>C
      !>      DATA                  MATRIX  /
      !>     .                          3.D0,  0.D0,  4.D0,  0.D0,
      !>     .                          5.D0, -2.D0,  0.D0,  0.D0,
      !>     .                          7.D0,  8.D0, -1.D0,  1.D0,
      !>     .                          3.D0,  1.D0,  0.D0,  0.D0  /
      !>
      !>      WRITE(*,'(A)') 'Matrix:'
      !>      DO I=1, NDIM
      !>
      !>         WRITE(*,'(4F6.1)') ( MATRIX(I,J), J=1,NDIM )
      !>
      !>      END DO
      !>
      !>C
      !>C     Compute the trace of MATRIX and display the result.
      !>C
      !>      WRITE(*,*)
      !>      WRITE(*,'(A,F4.1)') 'Trace: ', TRACEG ( MATRIX, NDIM )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Matrix:
      !>           3.0   5.0   7.0   3.0
      !>           0.0  -2.0   8.0   1.0
      !>           4.0   0.0  -1.0   0.0
      !>           0.0   0.0   1.0   0.0
      !>
      !>        Trace:  0.0
      !>
      !>## Restrictions
      !>
      !>     1)  No checking is performed to guard against floating point
      !>         overflow or underflow. This routine should probably not be
      !>         used if the input matrix is expected to have large double
      !>         precision numbers along the diagonal.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Created
      !>        complete code example based on existing fragment. Updated
      !>        $Particulars to provide mathematical representation of the
      !>        implemented algorithm.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure real(wp) function  traceg ( matrix, ndim )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of the matrix.
      real(wp), intent(in) :: matrix ( ndim,ndim )  !! NDIM x NDIM matrix of double precision
                                                    !! numbers.

      integer(ip) :: i

      traceg = 0.0_wp
      do i = 1,ndim
         traceg = traceg + matrix(i,i)
      end do

      return
      end function traceg

      !> Compute the normalized cross product of two 3-vectors.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two double precision 3-dimensional vectors.
      !>              Typically, these might represent the (possibly unit)
      !>              vector to a planet, Sun, or a star which defines the
      !>              orientation of axes of some reference frame.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the double precision 3-dimensional normalized cross
      !>              product of V1 and V2. VOUT is the result of the
      !>              computation
      !>
      !>                     V1 x V2
      !>                 ---------------
      !>                  || V1 x V2 ||
      !>
      !>              where "x" denotes the cross product and ||X||| the norm
      !>              of a vector X.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the cross product of V1 and V2 yields the zero-vector,
      !>         then the zero-vector is returned instead of a vector of
      !>         unit length.
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
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of vectors and compute the normalized cross
      !>        product of each vector in first set and the corresponding
      !>        vector in the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM UCRSS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 3 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  V1   / 0.D0,  1.D0,  0.D0,
      !>     .                             5.D0,  5.D0,  5.D0  /
      !>
      !>      DATA                  V2   / 3.D0,  0.D0,  0.D0,
      !>     .                            -2.D0, -2.D0, -2.D0  /
      !>
      !>C
      !>C     Calculate the cross product of each pair of vectors
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL UCRSS ( V1(1,I), V2(1,I), VOUT )
      !>
      !>         WRITE(*,'(A,3F5.1)') 'Vector A                : ',
      !>     .                                  ( V1(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F5.1)') 'Vector B                : ',
      !>     .                                  ( V2(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F5.1)') 'Normalized cross product: ',
      !>     .                                                VOUT
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector A                :   0.0  1.0  0.0
      !>        Vector B                :   3.0  0.0  0.0
      !>        Normalized cross product:   0.0  0.0 -1.0
      !>
      !>        Vector A                :   5.0  5.0  5.0
      !>        Vector B                :  -2.0 -2.0 -2.0
      !>        Normalized cross product:   0.0  0.0  0.0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 10-JAN-1989 (WLT)
      !>
      !>        Error free specification added. In addition the algorithm was
      !>        made more robust in the sense that floating point overflows
      !>        cannot occur.
      pure subroutine ucrss ( v1, v2, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! Left vector for cross product.
      real(wp), intent(in) :: v2   ( 3 )  !! Right vector for cross product.
      real(wp), intent(out) :: vout ( 3 )  !! Normalized cross product of V1 and V2.

      real(wp) :: vcross(3)
      real(wp) :: vmag

      real(wp) :: maxv1
      real(wp) :: maxv2

      real(wp) :: tv1 ( 3 )
      real(wp) :: tv2 ( 3 )

!
!     Get the biggest component of each of the two vectors.
!
      maxv1 = max ( dabs(v1(1)), dabs(v1(2)), dabs(v1(3)) )
      maxv2 = max ( dabs(v2(1)), dabs(v2(2)), dabs(v2(3)) )

!
!     Scale V1 and V2 by 1/MAXV1 and 1/MAXV2 respectively
!
      if ( maxv1 /= 0 ) then
         tv1(1) = v1(1)/maxv1
         tv1(2) = v1(2)/maxv1
         tv1(3) = v1(3)/maxv1
      else
         tv1(1) = 0.0_wp
         tv1(2) = 0.0_wp
         tv1(3) = 0.0_wp
      end if

      if ( maxv2 /= 0 ) then
         tv2(1) = v2(1)/maxv2
         tv2(2) = v2(2)/maxv2
         tv2(3) = v2(3)/maxv2
      else
         tv2(1) = 0.0_wp
         tv2(2) = 0.0_wp
         tv2(3) = 0.0_wp
      end if

!
!  Calculate the cross product of V1 and V2
!
      vcross(1) = tv1(2)*tv2(3) - tv1(3)*tv2(2)
      vcross(2) = tv1(3)*tv2(1) - tv1(1)*tv2(3)
      vcross(3) = tv1(1)*tv2(2) - tv1(2)*tv2(1)

!
!  Get the magnitude of VCROSS and normalize it
!
      vmag = vnorm(vcross)

      if (vmag>0.0_wp) then
         vout(1) = vcross(1) / vmag
         vout(2) = vcross(2) / vmag
         vout(3) = vcross(3) / vmag
      else
         vout(1) = 0.0_wp
         vout(2) = 0.0_wp
         vout(3) = 0.0_wp
      end if

      return
      end subroutine ucrss

      !> Normalize a double precision 3-vector and return its magnitude.
      !>
      !>## Detailed Input
      !>
      !>     V1       is an arbitrary 3-vector, including the
      !>              zero vector.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the unit vector in the direction of V1. If V1 is
      !>              the zero vector, then VOUT will also be the zero
      !>              vector.
      !>
      !>     VMAG     is the magnitude of V1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     UNORM references a function called VNORM (which itself is
      !>     numerically stable) to calculate the norm of the input vector V1.
      !>     If the norm is equal to zero, then each component of the output
      !>     vector VOUT is set to zero. Otherwise, VOUT is calculated by
      !>     dividing V1 by the norm.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of vectors and compute their corresponding unit
      !>        vector and magnitudes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM UNORM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      VMAG
      !>      DOUBLE PRECISION      SETA ( 3, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  SETA / 5.D0,  12.D0,  0.D0,
      !>     .                             1.D-7,  2.D-7, 2.D-7 /
      !>
      !>C
      !>C     Calculate the unit vectors and magnitudes.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL UNORM ( SETA(1,I), VOUT, VMAG )
      !>
      !>         WRITE(*,'(A,3F13.8)') 'Vector     : ',
      !>     .                                   ( SETA(J,I), J=1, 3 )
      !>         WRITE(*,'(A,3F13.8)') 'Unit vector: ', VOUT
      !>         WRITE(*,'(A,F13.8)')  'Magnitude:   ', VMAG
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector     :    5.00000000  12.00000000   0.00000000
      !>        Unit vector:    0.38461538   0.92307692   0.00000000
      !>        Magnitude:     13.00000000
      !>
      !>        Vector     :    0.00000010   0.00000020   0.00000020
      !>        Unit vector:    0.33333333   0.66666667   0.66666667
      !>        Magnitude:      0.00000030
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Added complete example code.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
      pure subroutine unorm ( v1, vout, vmag )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! Vector to be normalized.
      real(wp), intent(out) :: vout ( 3 )  !! Unit vector V1 / |V1|.
      real(wp), intent(out) :: vmag  !! Magnitude of V1, i.e. |V1|.

!
!  Obtain the magnitude of V1
!
      vmag = vnorm(v1)

!
!   If VMAG is nonzero, then normalize.  Note that this process is
!   numerically stable: overflow could only happen if VMAG were small,
!   but this could only happen if each component of V1 were small.
!   In fact, the magnitude of any vector is never less than the
!   magnitude of any component.
!
      if ( vmag > 0.0_wp ) then
         vout(1) = v1(1) / vmag
         vout(2) = v1(2) / vmag
         vout(3) = v1(3) / vmag
      else
         vout(1) = 0.0_wp
         vout(2) = 0.0_wp
         vout(3) = 0.0_wp
      end if

      return
      end subroutine unorm

      !> Normalize a double precision vector of arbitrary dimension and
      !> return its magnitude.
      !>
      !>## Detailed Input
      !>
      !>     V1       is an arbitrary double precision n-dimensional vector,
      !>              including the zero vector.
      !>
      !>     NDIM     is the dimension of V1 and VOUT.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the double precision n-dimensional unit vector in the
      !>              direction of V1. If V1 is the zero vector, then VOUT
      !>              will also be the zero vector.
      !>
      !>     VMAG     is the magnitude of V1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     UNORMG references a function called VNORMG (which itself is
      !>     numerically stable) to calculate the norm of the input vector V1.
      !>     If the norm is equal to zero, then each component of the output
      !>     vector VOUT is set to zero. Otherwise, VOUT is calculated by
      !>     dividing V1 by the norm. No error detection or correction is
      !>     implemented.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of n-dimensional vectors and compute their
      !>        corresponding unit vectors and magnitudes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM UNORMG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      VMAG
      !>      DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  V1    /
      !>     .                         5.D0,  12.D0,  0.D0,  4.D0,
      !>     .                         1.D-6,  2.D-6, 2.D-6, 0.D0  /
      !>
      !>C
      !>C     Calculate the unit vectors and magnitudes.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL UNORMG ( V1(1,I), NDIM, VOUT, VMAG )
      !>
      !>         WRITE(*,'(A,4F12.7)') 'Vector     :',
      !>     .                         ( V1(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4F12.7)') 'Unit vector:', VOUT
      !>         WRITE(*,'(A,F12.7)')  'Magnitude  :', VMAG
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector     :   5.0000000  12.0000000   0.0000000   4.0000000
      !>        Unit vector:   0.3676073   0.8822575   0.0000000   0.2940858
      !>        Magnitude  :  13.6014705
      !>
      !>        Vector     :   0.0000010   0.0000020   0.0000020   0.0000000
      !>        Unit vector:   0.3333333   0.6666667   0.6666667   0.0000000
      !>        Magnitude  :   0.0000030
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is implemented in this subroutine to guard
      !>         against numeric overflow.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Updated code comments.
      !>
      !>        Added complete code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine unormg ( v1, ndim, vout, vmag )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of V1 (and also VOUT).
      real(wp), intent(in) :: v1   ( ndim )  !! Vector to be normalized.
      real(wp), intent(out) :: vout ( ndim )  !! Unit vector V1 / ||V1||.
      real(wp), intent(out) :: vmag  !! Magnitude of V1, i.e. ||V1||.

      integer(ip) :: i

!
!  Obtain the magnitude of V1
!
      vmag = vnormg (v1,ndim)

!
!   If VMAG is nonzero, then normalize. Note that this process is
!   numerically stable: overflow could only happen if VMAG were small,
!   but this could only happen if each component of V1 were also small.
!   In fact, the magnitude of any vector is never less than the
!   magnitude of any component.
!
      if (vmag>0.0_wp) then
         do i = 1, ndim
            vout(i) = v1(i) / vmag
         end do
      else
         do i = 1, ndim
            vout(i) = 0.0_wp
         end do
      end if
      return
      end subroutine unormg

      !> Add two double precision 3-dimensional vectors.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two arbitrary double precision 3-dimensional
      !>              vectors.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the double precision 3-dimensional vector sum of V1
      !>              and V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine simply performs addition between components of V1
      !>     and V2. No checking is performed to determine whether floating
      !>     point overflow has occurred.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of 3-dimensional vectors and compute the sum
      !>        of each vector in first set with the corresponding vector in
      !>        the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VADD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      SETA ( 3, SETSIZ )
      !>      DOUBLE PRECISION      SETB ( 3, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  SETA / 1.D0,  2.D0,  3.D0,
      !>     .                             1.D-7, 1.D23, 1.D-9  /
      !>
      !>      DATA                  SETB / 4.D0,  5.D0,   6.D0,
      !>     .                             1.D24, 1.D23,  0.D0  /
      !>
      !>C
      !>C     Calculate the sum of each pair of vectors
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VADD ( SETA(1,I), SETB(1,I), VOUT )
      !>
      !>         WRITE(*,'(A,3E11.2)') 'Vector A  : ',
      !>     .                        ( SETA(J,I), J=1,3 )
      !>         WRITE(*,'(A,3E11.2)') 'Vector B  : ',
      !>     .                        ( SETB(J,I), J=1,3 )
      !>         WRITE(*,'(A,3E11.2)') 'Sum vector: ', VOUT
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector A  :    0.10E+01   0.20E+01   0.30E+01
      !>        Vector B  :    0.40E+01   0.50E+01   0.60E+01
      !>        Sum vector:    0.50E+01   0.70E+01   0.90E+01
      !>
      !>        Vector A  :    0.10E-06   0.10E+24   0.10E-08
      !>        Vector B  :    0.10E+25   0.10E+24   0.00E+00
      !>        Sum vector:    0.10E+25   0.20E+24   0.10E-08
      !>
      !>## Restrictions
      !>
      !>     1)  The user is required to determine that the magnitude each
      !>         component of the vectors is within the appropriate range so as
      !>         not to cause floating point overflow.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vadd ( v1, v2, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! First vector to be added.
      real(wp), intent(in) :: v2   ( 3 )  !! Second vector to be added.
      real(wp), intent(out) :: vout ( 3 )  !! Sum vector, V1 + V2.

      vout = v1 + v2
      end subroutine vadd

      !> Add two vectors of arbitrary dimension.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two arbitrary double precision n-dimensional
      !>              vectors.
      !>
      !>     NDIM     is the dimension of V1, V2 and VOUT.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the double precision n-dimensional vector sum of V1
      !>              and V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine simply performs addition between components of V1
      !>     and V2. No checking is performed to determine whether floating
      !>     point overflow has occurred.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of n-dimensional vectors and compute the sum
      !>        of each vector in first set with the corresponding vector in
      !>        the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VADDG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  SETA /
      !>     .                      1.D0,  2.D0,   3.D0,  4.D0,
      !>     .                      1.D-7, 1.D23, 1.D-9,  0.D0   /
      !>
      !>      DATA                  SETB /
      !>     .                      4.D0,  5.D0,   6.D0,  7.D0,
      !>     .                      1.D24, 1.D23,  0.D0,  3.D-23  /
      !>
      !>C
      !>C     Calculate the sum of each pair of vectors
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VADDG ( SETA(1,I), SETB(1,I), NDIM, VOUT )
      !>
      !>         WRITE(*,'(A,4E11.2)') 'Vector A  : ',
      !>     .                        ( SETA(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4E11.2)') 'Vector B  : ',
      !>     .                        ( SETB(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4E11.2)') 'Sum vector: ', VOUT
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector A  :    0.10E+01   0.20E+01   0.30E+01   0.40E+01
      !>        Vector B  :    0.40E+01   0.50E+01   0.60E+01   0.70E+01
      !>        Sum vector:    0.50E+01   0.70E+01   0.90E+01   0.11E+02
      !>
      !>        Vector A  :    0.10E-06   0.10E+24   0.10E-08   0.00E+00
      !>        Vector B  :    0.10E+25   0.10E+24   0.00E+00   0.30E-22
      !>        Sum vector:    0.10E+25   0.20E+24   0.10E-08   0.30E-22
      !>
      !>## Restrictions
      !>
      !>     1)  The user is required to determine that the magnitude each
      !>         component of the vectors is within the appropriate range so as
      !>         not to cause floating point overflow.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.2, 07-NOV-2003 (EDW)
      !>
      !>        Corrected a mistake in the second example's value
      !>        for VOUT, i.e. replaced (1D24, 2D23, 0.0) with
      !>        (1D24, 2D23).
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vaddg ( v1, v2, ndim, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of V1, V2, and VOUT.
      real(wp), intent(in) :: v1   ( ndim )  !! First vector to be added.
      real(wp), intent(in) :: v2   ( ndim )  !! Second vector to be added.
      real(wp), intent(out) :: vout ( ndim )  !! Sum vector, V1 + V2.

      vout(1:ndim) = v1(1:ndim) + v2(1:ndim)
      end subroutine vaddg

      !> Compute the cross product of two 3-dimensional vectors.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two 3-dimensional vectors. Typically, these might
      !>              represent the (possibly unit) vector to a planet, Sun,
      !>              or a star which defines the orientation of axes of some
      !>              reference frame.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the cross product of V1 and V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VCRSS calculates the three dimensional cross product of two
      !>     vectors according to the definition.
      !>
      !>     If V1 and V2 are large in magnitude (taken together, their
      !>     magnitude surpasses the limit allowed by the computer) then it
      !>     may be possible to generate a floating point overflow from an
      !>     intermediate computation even though the actual cross product may
      !>     be well within the range of double precision numbers. VCRSS does
      !>     NOT check the magnitude of V1 or V2 to insure that overflow will
      !>     not occur.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of vectors and compute the cross product of
      !>        each vector in first set and the corresponding vector in
      !>        the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VCRSS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 3 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  SETA / 0.D0,  1.D0,  0.D0,
      !>     .                             5.D0,  5.D0,  5.D0  /
      !>
      !>      DATA                  SETB / 1.D0,  0.D0,  0.D0,
      !>     .                            -1.D0, -1.D0, -1.D0  /
      !>
      !>C
      !>C     Calculate the cross product of each pair of vectors
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VCRSS ( SETA(1,I), SETB(1,I), VOUT )
      !>
      !>         WRITE(*,'(A,3F5.1)') 'Vector A     : ',
      !>     .                        ( SETA(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F5.1)') 'Vector B     : ',
      !>     .                        ( SETB(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F5.1)') 'Cross product: ', VOUT
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector A     :   0.0  1.0  0.0
      !>        Vector B     :   1.0  0.0  0.0
      !>        Cross product:   0.0  0.0 -1.0
      !>
      !>        Vector A     :   5.0  5.0  5.0
      !>        Vector B     :  -1.0 -1.0 -1.0
      !>        Cross product:   0.0  0.0  0.0
      !>
      !>## Restrictions
      !>
      !>     1)  No checking of V1 or V2 is done to prevent floating point
      !>         overflow. The user is required to determine that the
      !>         magnitude of each component of the vectors is within an
      !>         appropriate range so as not to cause floating point overflow.
      !>         In almost every case there will be no problem and no checking
      !>         actually needs to be done.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vcrss ( v1, v2, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! Left hand vector for cross product.
      real(wp), intent(in) :: v2   ( 3 )  !! Right hand vector for cross product.
      real(wp), intent(out) :: vout ( 3 )  !! Cross product V1 x V2.

      real(wp) :: vtemp(3)

!
!  Calculate the cross product of V1 and V2, store in VTEMP
!
      vtemp(1) = v1(2)*v2(3) - v1(3)*v2(2)
      vtemp(2) = v1(3)*v2(1) - v1(1)*v2(3)
      vtemp(3) = v1(1)*v2(2) - v1(2)*v2(1)
!
!  Now move the result into VOUT
!
      vout(1) = vtemp(1)
      vout(2) = vtemp(2)
      vout(3) = vtemp(3)
      return
      end subroutine vcrss

      !> Return the distance between two three-dimensional vectors.
      !>
      !> The function returns the distance between V1 and V2.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two vectors in three-dimensional space, the
      !>              distance between which is desired.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the distance between V1 and V2. This is
      !>     defined as
      !>
      !>        ||  V1 - V2  ||,
      !>
      !>     where || x || indicates the Euclidean norm of the vector x.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function is simply shorthand for the code
      !>
      !>        CALL VSUB ( V1, V2, DIFF )
      !>
      !>        DIST = VNORM ( DIFF )
      !>
      !>     Using this function saves you the annoyance of declaring local
      !>     storage for the difference vector DIFF.
      !>
      !>     The Euclidean norm of a three-dimensional vector (x, y, z) is
      !>     defined as
      !>
      !>                                     1/2
      !>             2        2        2
      !>        (   x    +   y    +   z    ).
      !>
      !>     This number is the distance of the point (x, y, z) from the
      !>     origin. If A and B are two vectors whose components are
      !>
      !>        ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),
      !>
      !>     then the distance between A and B is the norm of the difference
      !>     A - B, which has components
      !>
      !>        (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).
      !>
      !>     A related routine is VDISTG, which computes the distance between
      !>     two vectors of general dimension.
      !>
      !>## Examples
      !>
      !>     1)  If V1 is
      !>
      !>            ( 2.0D0,  3.0D0,  0.D0 )
      !>
      !>         and V2 is
      !>
      !>            ( 5.0D0,  7.0D0,  12.D0 ),
      !>
      !>         VDIST (V1, V2) will be 13.D0.
      !>
      !>     2)  If VGR2 and NEP are states of the Voyager 2 spacecraft and
      !>         Neptune with respect to some common center at a given time
      !>         ET, then
      !>
      !>            VDIST ( VGR2, NEP )
      !>
      !>         yields the distance between the spacecraft and Neptune at time
      !>         ET.
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
      !>-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 08-JUL-1990 (NJB)
      pure real(wp) function  vdist ( v1, v2 )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1 ( 3 )  !! Two 3-vectors.
      real(wp), intent(in) :: v2 ( 3 )  !! Two 3-vectors.

      real(wp) :: diff ( 3 )

!
!     No surprises.
!
      diff = v1 - v2

      vdist = vnorm (diff)

      return
      end function vdist

      !> Return the distance between two vectors of arbitrary dimension.
      !>
      !> The function returns the distance between V1 and V2.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two vectors of arbitrary dimension, the
      !>              distance between which is desired.
      !>
      !>     NDIM     is the common dimension of V1 and V2. NDIM must be
      !>              non-negative and must not exceed the minimum of the
      !>              declared sizes of the actual arguments corresponding
      !>              to V1 and V2.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the distance between V1 and V2. This is
      !>     defined as
      !>
      !>        ||  V1 - V2  ||,
      !>
      !>     where || x || indicates the Euclidean norm of the vector x.
      !>
      !>     If NDIM is less than 1, the function value is set to 0.D0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The Euclidean norm of an n-dimensional vector
      !>
      !>        (x ,  x , ... , x )
      !>          1    2         n
      !>
      !>     is defined as
      !>
      !>                                                1/2
      !>              2        2                  2
      !>        (   x    +   x    +  . . .  +   x     ).
      !>             1        2                  n
      !>
      !>     This number is the distance of the point (x, y, z) from the
      !>     origin. If n = 3, and A and B are two vectors whose components
      !>     are
      !>
      !>        ( A(1), A(2), A(3) )    and    ( B(1), B(2), B(3) ),
      !>
      !>     then the distance between A and B is the norm of the difference
      !>     A - B, which has components
      !>
      !>        (  A(1) - B(1),  A(2) - B(2),  A(3) - B(3)  ).
      !>
      !>     A related routine is VDIST, which computes the distance between
      !>     two 3-vectors.
      !>
      !>## Examples
      !>
      !>     1)  If V1 is
      !>
      !>            ( 2.0D0,  3.0D0 )
      !>
      !>         and V2 is
      !>
      !>            ( 5.0D0,  7.0D0 ),
      !>
      !>         and NDIM is 2, then
      !>
      !>            VDISTG (V1, V2, NDIM )
      !>
      !>         will be 5.D0.
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
      !>-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 17-JUL-1990 (NJB)
      pure real(wp) function vdistg ( v1, v2, ndim )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: v1   ( * )  !! Two vectors of arbitrary dimension.
      real(wp), intent(in) :: v2   ( * )  !! Two vectors of arbitrary dimension.
      integer(ip), intent(in) :: ndim  !! The common dimension of V1 and V2

      real(wp) :: scale
      integer(ip) :: i

!
!     We find the norm of a scaled version of the difference vector,
!     and then rescale this norm.  This method helps prevent overflow
!     due to squaring the components of the difference vector.
!
!     The code here is almost identical to that of VNORMG.  We'd love
!     to just call VNORMG, but that would require storage for the
!     difference vector.  So we do the job ourselves.
!

!
!     Find the scale factor.
!
      scale = 0.0_wp

      do i = 1, ndim
         scale = max (  scale,  dabs ( v1(i) - v2(i) )  )
      end do

      if ( scale == 0.0_wp ) then

         vdistg = 0.0_wp
         return

      else

         vdistg = 0.0_wp

         do i = 1, ndim
            vdistg = vdistg  +  (  ( v1(i) - v2(i) ) / scale  ) ** 2
         end do

         vdistg = scale  *  sqrt ( vdistg )

      end if

      return
      end function vdistg

      !> Compute the dot product of two double precision, 3-dimensional
      !> vectors.
      !>
      !> The function returns the value of the dot product of V1 and V2.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two arbitrary double precision 3-dimensional
      !>              vectors.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value of the dot product (inner product)
      !>     of V1 and V2:
      !>
      !>        < V1, V2 >
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VDOT calculates the dot product of V1 and V2 by a simple
      !>     application of the definition:
      !>
      !>                   3
      !>                .-----
      !>                 \
      !>        VDOT  =   )  V1(I) * V2(I)
      !>                 /
      !>                '-----
      !>                  I=1
      !>
      !>     No error checking is performed to prevent or recover from numeric
      !>     overflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that you have a set of double precision 3-dimensional
      !>        vectors. Check if they are orthogonal to the Z-axis.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VDOT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VDOT
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( 3, SETSIZ )
      !>      DOUBLE PRECISION      Z    ( 3         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  V1  / 1.D0,  0.D0,  0.D0,
      !>     .                            0.D0, -6.D0,  0.D0,
      !>     .                           10.D0,  0.D0, -1.D0,
      !>     .                            0.D0,  0.D0,  1.D0  /
      !>
      !>      DATA                  Z   / 0.D0,  0.D0,  1.D0  /
      !>
      !>C
      !>C     Check the orthogonality with respect to Z of each
      !>C     vector in V1.
      !>C
      !>      DO I = 1, SETSIZ
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,'(A,3F6.1)') 'Input vector (V1): ',
      !>     .                         ( V1(J,I), J=1,3 )
      !>
      !>         IF ( VDOT( V1(1,I), Z ) .EQ. 0.D0 ) THEN
      !>
      !>            WRITE(*,'(A)') 'V1 and Z are orthogonal.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A)') 'V1 and Z are NOT orthogonal.'
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector (V1):    1.0   0.0   0.0
      !>        V1 and Z are orthogonal.
      !>
      !>        Input vector (V1):    0.0  -6.0   0.0
      !>        V1 and Z are orthogonal.
      !>
      !>        Input vector (V1):   10.0   0.0  -1.0
      !>        V1 and Z are NOT orthogonal.
      !>
      !>        Input vector (V1):    0.0   0.0   1.0
      !>        V1 and Z are NOT orthogonal.
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for determining that the vectors V1
      !>         and V2 are not so large as to cause numeric overflow. In
      !>         most cases this will not present a problem.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 28-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Improved $Particulars section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function vdot ( v1, v2 )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1 ( 3 )  !! First vector in the dot product.
      real(wp), intent(in) :: v2 ( 3 )  !! Second vector in the dot product.

      vdot = v1(1)*v2(1) + v1(2)*v2(2) + v1(3)*v2(3)
      return
      end function vdot

      !> Compute the dot product of two vectors of arbitrary dimension.
      !>
      !> The function returns the value of the dot product of V1 and V2.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two arbitrary double precision n-dimensional
      !>              vectors.
      !>
      !>     NDIM     is the dimension of V1 and V2.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value of the dot product (inner product)
      !>     of V1 and V2:
      !>
      !>        < V1, V2 >
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VDOTG calculates the dot product of V1 and V2 by a simple
      !>     application of the definition:
      !>
      !>                   NDIM
      !>                 .------
      !>                  \
      !>        VDOTG  =   )  V1(I) * V2(I)
      !>                  /
      !>                 '------
      !>                    I=1
      !>
      !>     No error checking is performed to prevent or recover from numeric
      !>     overflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that you have a set of double precision n-dimensional
      !>        vectors. Check if they are orthogonal to the Z-axis in
      !>        n-dimensional space.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VDOTG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VDOTG
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 5 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      Z    ( NDIM         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  V1  / 1.D0,  0.D0,  0.D0, 0.D0,
      !>     .                            0.D0,  1.D0,  0.D0, 3.D0,
      !>     .                            0.D0,  0.D0, -6.D0, 0.D0,
      !>     .                           10.D0,  0.D0, -1.D0, 0.D0,
      !>     .                            0.D0,  0.D0,  0.D0, 1.D0  /
      !>
      !>      DATA                  Z   / 0.D0,  0.D0,  1.D0, 0.D0  /
      !>
      !>C
      !>C     Check the orthogonality with respect to Z of each
      !>C     vector in V1.
      !>C
      !>      DO I = 1, SETSIZ
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,'(A,4F6.1)') 'Input vector (V1): ',
      !>     .                         ( V1(J,I), J=1,NDIM )
      !>
      !>         IF ( VDOTG( V1(1,I), Z, NDIM ) .EQ. 0.D0 ) THEN
      !>
      !>            WRITE(*,'(A)') 'V1 and Z are orthogonal.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A)') 'V1 and Z are NOT orthogonal.'
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector (V1):    1.0   0.0   0.0   0.0
      !>        V1 and Z are orthogonal.
      !>
      !>        Input vector (V1):    0.0   1.0   0.0   3.0
      !>        V1 and Z are orthogonal.
      !>
      !>        Input vector (V1):    0.0   0.0  -6.0   0.0
      !>        V1 and Z are NOT orthogonal.
      !>
      !>        Input vector (V1):   10.0   0.0  -1.0   0.0
      !>        V1 and Z are NOT orthogonal.
      !>
      !>        Input vector (V1):    0.0   0.0   0.0   1.0
      !>        V1 and Z are orthogonal.
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for determining that the vectors V1
      !>         and V2 are not so large as to cause numeric overflow. In
      !>         most cases this will not present a problem.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 28-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Improved $Particulars section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function vdotg ( v1, v2, ndim )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: v1  ( * )  !! First vector in the dot product.
      real(wp), intent(in) :: v2  ( * )  !! Second vector in the dot product.
      integer(ip), intent(in) :: ndim  !! Dimension of V1 and V2.

      integer(ip) :: i
      vdotg = 0.0_wp
      do i = 1, ndim
         vdotg = vdotg + v1(i)*v2(i)
      end do

      return
      end function vdotg

      !> Make one double precision 3-dimensional vector equal to
      !> another.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     VIN      is an arbitrary, double precision 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a double precision 3-dimensional vector set equal
      !>              to VIN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VEQU simply sets each component of VOUT in turn equal to VIN. No
      !>     error checking is performed because none is needed.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Lets assume we have a pointing record that contains the
      !>        start time of an interpolation interval, the components of
      !>        the quaternion that represents the C-matrix associated with
      !>        the start time of the interval, and the angular velocity vector
      !>        of the interval. The following example demonstrates how to
      !>        extract the time, the quaternion and the angular velocity
      !>        vector into separate variables for their processing.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VEQU_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AV     ( 3 )
      !>      DOUBLE PRECISION      QUAT   ( 4 )
      !>      DOUBLE PRECISION      RECORD ( 8 )
      !>      DOUBLE PRECISION      TIME
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the pointing record. We would normally obtain it
      !>C     from, e.g. CK readers or other non SPICE data files.
      !>C
      !>      DATA                  RECORD  /
      !>     .      283480.753D0,   0.99999622D0,  0.0D0,  0.0D0,
      !>     .     -0.0027499965D0, 0.0D0,         0.0D0,  0.01D0 /
      !>
      !>C
      !>C     Get the time, quaternion and angular velocity vector
      !>C     into separate variables.
      !>C
      !>      TIME = RECORD(1)
      !>
      !>      CALL VEQUG  ( RECORD(2), 4, QUAT )
      !>      CALL VEQU   ( RECORD(6),    AV   )
      !>
      !>C
      !>C     Display the contents of the variables.
      !>C
      !>      WRITE(*,'(A,F11.3)') 'Time            :', TIME
      !>
      !>      WRITE(*,'(A)')       'Quaternion      :'
      !>      WRITE(*,'(4F15.10)')  QUAT
      !>      WRITE(*,'(A)')       'Angular velocity:'
      !>      WRITE(*,'(3F15.10)')  AV
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Time            : 283480.753
      !>        Quaternion      :
      !>           0.9999962200   0.0000000000   0.0000000000  -0.0027499965
      !>        Angular velocity:
      !>           0.0000000000   0.0000000000   0.0100000000
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vequ ( vin, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: vin   ( 3 )  !! Double precision 3-dimensional vector.
      real(wp), intent(out) :: vout  ( 3 )  !! Double precision 3-dimensional vector set equal to
                                            !! VIN.

      vout = vin
      end subroutine vequ

      !> Make one double precision vector of arbitrary dimension equal
      !> to another.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array assignment instead).
      !>
      !>## Detailed Input
      !>
      !>     VIN      is an arbitrary, double precision n-dimensional vector.
      !>
      !>     NDIM     is the dimension of VIN and VOUT.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a double precision n-dimensional vector set equal
      !>              to VIN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code simply sets each component of VOUT equal to the
      !>     corresponding component of VIN.
      !>
      !>     Note that this routine may be used in place of MOVED, which
      !>     sets each output array element equal to the corresponding
      !>     input array element.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Lets assume we have a pointing record that contains the
      !>        start time of an interpolation interval, the components of
      !>        the quaternion that represents the C-matrix associated with
      !>        the start time of the interval, and the angular velocity vector
      !>        of the interval. The following example demonstrates how to
      !>        extract the time, the quaternion and the angular velocity
      !>        vector into separate variables for their processing.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VEQUG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AV     ( 3 )
      !>      DOUBLE PRECISION      QUAT   ( 4 )
      !>      DOUBLE PRECISION      RECORD ( 8 )
      !>      DOUBLE PRECISION      TIME
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the pointing record. We would normally obtain it
      !>C     from, e.g. CK readers or other non SPICE data files.
      !>C
      !>      DATA                  RECORD  /
      !>     .      283480.753D0,   0.99999622D0,  0.0D0,  0.0D0,
      !>     .     -0.0027499965D0, 0.0D0,         0.0D0,  0.01D0 /
      !>
      !>C
      !>C     Get the time, quaternion and angular velocity vector
      !>C     into separate variables.
      !>C
      !>      TIME = RECORD(1)
      !>
      !>      CALL VEQUG  ( RECORD(2), 4, QUAT )
      !>      CALL VEQU   ( RECORD(6),    AV   )
      !>
      !>C
      !>C     Display the contents of the variables.
      !>C
      !>      WRITE(*,'(A,F11.3)') 'Time            :', TIME
      !>
      !>      WRITE(*,'(A)')       'Quaternion      :'
      !>      WRITE(*,'(4F15.10)')  QUAT
      !>      WRITE(*,'(A)')       'Angular velocity:'
      !>      WRITE(*,'(3F15.10)')  AV
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Time            : 283480.753
      !>        Quaternion      :
      !>           0.9999962200   0.0000000000   0.0000000000  -0.0027499965
      !>        Angular velocity:
      !>           0.0000000000   0.0000000000   0.0100000000
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vequg ( vin, ndim, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of VIN (and also VOUT).
      real(wp), intent(in) :: vin   ( ndim )  !! Double precision n-dimensional vector.
      real(wp), intent(out) :: vout  ( ndim )  !! Double precision n-dimensional vector set equal to
                                               !! VIN.

      vout(1:ndim) = vin(1:ndim)
      end subroutine vequg

      !> Find the unit vector along a double precision 3-dimensional
      !> vector.
      !>
      !>## Detailed Input
      !>
      !>     V1       is any double precision, 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the unit vector in the direction of V1.
      !>
      !>              If V1 represents the zero vector, then VOUT will also be
      !>              the zero vector. VOUT may overwrite V1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If V1 represents the zero vector, then VOUT will also be the
      !>         zero vector.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VHAT determines the magnitude of V1 and then divides each
      !>     component of V1 by the magnitude. This process is highly stable
      !>     over the whole range of 3-dimensional vectors.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of vectors and compute their corresponding unit
      !>        vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VHAT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      SETA ( 3, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  SETA / 5.D0,  12.D0,  0.D0,
      !>     .                             1.D-7,  2.D-7, 2.D-7 /
      !>
      !>C
      !>C     Calculate the unit vectors.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VHAT ( SETA(1,I), VOUT )
      !>
      !>         WRITE(*,'(A,3F13.8)') 'Vector     : ',
      !>     .                                   ( SETA(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F13.8)') 'Unit vector: ', VOUT
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector     :    5.00000000  12.00000000   0.00000000
      !>        Unit vector:    0.38461538   0.92307692   0.00000000
      !>
      !>        Vector     :    0.00000010   0.00000020   0.00000020
      !>        Unit vector:    0.33333333   0.66666667   0.66666667
      !>
      !>## Restrictions
      !>
      !>     1)  There is no known case whereby floating point overflow may
      !>         occur. Thus, no error recovery or reporting scheme is
      !>         incorporated into this subroutine.
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example to $Examples section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (HAN) (NJB)
      pure subroutine vhat ( v1, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! Vector to be normalized.
      real(wp), intent(out) :: vout ( 3 )  !! Unit vector V1 / |V1|.

      real(wp) :: vmag

!
!  Obtain the magnitude of V1
!
      vmag = vnorm(v1)

!
!   If VMAG is nonzero, then normalize.  Note that this process is
!   numerically stable: overflow could only happen if VMAG were small,
!   but this could only happen if each component of V1 were small.
!   In fact, the magnitude of any vector is never less than the
!   magnitude of any component.
!
      if (vmag>0.0_wp) then
         vout(1) = v1(1) / vmag
         vout(2) = v1(2) / vmag
         vout(3) = v1(3) / vmag
      else
         vout(1) = 0.0_wp
         vout(2) = 0.0_wp
         vout(3) = 0.0_wp
      end if

      return
      end subroutine vhat

      !> Find the unit vector along a double precision vector of
      !> arbitrary dimension.
      !>
      !>## Detailed Input
      !>
      !>     V1       is any double precision vector of arbitrary dimension.
      !>
      !>     NDIM     is the dimension of V1 (and also VOUT).
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the unit vector in the direction of V1:
      !>
      !>                           V1
      !>                 VOUT = --------
      !>                         ||V1||
      !>
      !>              If V1 represents the zero vector, then VOUT will also be
      !>              the zero vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VHATG determines the magnitude of V1 and then divides each
      !>     component of V1 by the magnitude. This process is highly stable
      !>     over the whole range of multi-dimensional vectors.
      !>
      !>     This routine will detect if V1 the zero vector, and will not
      !>     attempt to divide by zero.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of n-dimensional vectors and find the unit vector
      !>        along each of them.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VHATG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  V1  /
      !>     .                          5.D0,  12.D0,  0.D0,  0.D0,
      !>     .                          1.D-7,  2.D-7, 2.D-7, 0.D0  /
      !>
      !>C
      !>C     Calculate the unit vectors.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VHATG ( V1(1,I), NDIM, VOUT )
      !>
      !>         WRITE(*,'(A,4F12.7)') 'Input vector: ',
      !>     .                         ( V1(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4F12.7)') 'Unit vector : ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector:    5.0000000  12.0000000   0.0000000   0.0000000
      !>        Unit vector :    0.3846154   0.9230769   0.0000000   0.0000000
      !>
      !>        Input vector:    0.0000001   0.0000002   0.0000002   0.0000000
      !>        Unit vector :    0.3333333   0.6666667   0.6666667   0.0000000
      !>
      !>## Restrictions
      !>
      !>     1)  The relative number of cases whereby floating point overflow
      !>         may occur is negligible. Thus, no error recovery or reporting
      !>         scheme is incorporated into this subroutine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vhatg ( v1, ndim, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of V1 (and also VOUT).
      real(wp), intent(in) :: v1   ( ndim )  !! Vector to be normalized.
      real(wp), intent(out) :: vout ( ndim )  !! Unit vector along V1.

      real(wp) :: vmag

      integer(ip) :: i

!
!  Obtain the magnitude of V1
!
      vmag = vnormg (v1,ndim)

!
!   If VMAG is nonzero, then normalize.  Note that this process is
!   numerically stable: overflow could only happen if VMAG were small,
!   but this could only happen if each component of V1 were small.
!   In fact, the magnitude of any vector is never less than the
!   magnitude of any component.
!
      if (vmag>0.0_wp) then
         do i = 1, ndim
            vout(i) = v1(i) / vmag
         end do
      else
         do i = 1, ndim
            vout(i) = 0.0_wp
         end do
      end if
      return
      end subroutine vhatg

      !> Scale a three-dimensional vector to unit length.
      !>
      !>## Detailed Input
      !>
      !>     V        is any double precision, 3-dimensional vector. If this
      !>              vector is the zero vector, this routine will detect it,
      !>              and will not attempt to divide by zero.
      !>
      !>## Detailed Output
      !>
      !>     V        is the unit vector in the direction of the input vector.
      !>              If on input V represents the zero vector, then V will be
      !>              returned as the zero vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  The zero vector is returned if the input value of V is the
      !>         zero vector.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is provided for situation where it is convenient
      !>     to scale a vector to unit length in place rather than store
      !>     the result in a separate variable. Note that the call
      !>
      !>        CALL VHAT ( V, V )
      !>
      !>     is not permitted by the ANSI Fortran 77 standard; this routine
      !>     can be called instead to achieve the same result.
      !>
      !>     VHATIP determines the magnitude of V and then, if the magnitude
      !>     is non-zero, divides each component of V by the magnitude. This
      !>     process is highly stable over the whole range of 3-dimensional
      !>     vectors.
      !>
      !>## Examples
      !>
      !>     The following table shows how selected vectors are mapped to
      !>     unit vectors
      !>
      !>     V on input            V on output
      !>     ------------------    ------------------
      !>     (5, 12, 0)            (5/13, 12/13, 0)
      !>     (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)
      !>
      !>## Restrictions
      !>
      !>     1)  There is no known case whereby floating point overflow may
      !>         occur. Thus, no error recovery or reporting scheme is
      !>         incorporated into this subroutine.
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) (HAN) (WMO) (WLT)
      pure subroutine vhatip ( v )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(inout) :: v ( 3 )  !! Vector to be normalized/unit vector.

      real(wp) :: vmag

!
!     Obtain the magnitude of V.
!
      vmag = vnorm ( v )

!
!     If VMAG is nonzero, then normalize. Note that this process is
!     numerically stable: overflow could only happen if VMAG were
!     small, but this could only happen if each component of V1 were
!     small. In fact, the magnitude of any vector is never less than
!     the magnitude of any component.
!
      if ( vmag > 0.0_wp ) then

         v(1) = v(1) / vmag
         v(2) = v(2) / vmag
         v(3) = v(3) / vmag

      else

         v(1) = 0.0_wp
         v(2) = 0.0_wp
         v(3) = 0.0_wp

      end if

      return
      end subroutine vhatip

      !> Compute a vector linear combination of two double precision,
      !> 3-dimensional vectors.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     A        is the double precision scalar variable that multiplies
      !>              V1.
      !>
      !>     V1       is an arbitrary, double precision 3-dimensional vector.
      !>
      !>     B        is the double precision scalar variable that multiplies
      !>              V2.
      !>
      !>     V2       is an arbitrary, double precision 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     SUM      is the double precision 3-dimensional vector which
      !>              contains the linear combination
      !>
      !>                 A * V1 + B * V2
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the index I, from 1 to 3:
      !>
      !>           SUM(I) = A * V1(I) + B * V2(I)
      !>
      !>     No error checking is performed to guard against numeric overflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you want to generate a sequence of points representing
      !>        an elliptical footprint, from the known semi-major
      !>        and semi-minor axes.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VLCOM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      TWOPI
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      STEP
      !>      DOUBLE PRECISION      THETA
      !>      DOUBLE PRECISION      SMAJOR ( 3 )
      !>      DOUBLE PRECISION      SMINOR ( 3 )
      !>      DOUBLE PRECISION      VECTOR ( 3 )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Let SMAJOR and SMINOR be the two known semi-major and
      !>C     semi-minor axes of our elliptical footprint.
      !>C
      !>      DATA                  SMAJOR /
      !>     .                    0.070115D0, 0.D0,        0.D0 /
      !>
      !>      DATA                  SMINOR /
      !>     .                    0.D0,       0.035014D0,  0.D0 /
      !>
      !>C
      !>C     Compute the vectors of interest and display them
      !>C
      !>      THETA = 0.D0
      !>      STEP  = TWOPI() / 16
      !>
      !>      DO I = 1, 16
      !>
      !>         CALL VLCOM (  COS(THETA), SMAJOR,
      !>     .                 SIN(THETA), SMINOR, VECTOR )
      !>
      !>         WRITE(*,'(I2,A,3F10.6)') I, ':', VECTOR
      !>
      !>         THETA = THETA + STEP
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         1:  0.070115  0.000000  0.000000
      !>         2:  0.064778  0.013399  0.000000
      !>         3:  0.049579  0.024759  0.000000
      !>         4:  0.026832  0.032349  0.000000
      !>         5:  0.000000  0.035014  0.000000
      !>         6: -0.026832  0.032349  0.000000
      !>         7: -0.049579  0.024759  0.000000
      !>         8: -0.064778  0.013399  0.000000
      !>         9: -0.070115  0.000000  0.000000
      !>        10: -0.064778 -0.013399 -0.000000
      !>        11: -0.049579 -0.024759 -0.000000
      !>        12: -0.026832 -0.032349 -0.000000
      !>        13: -0.000000 -0.035014 -0.000000
      !>        14:  0.026832 -0.032349  0.000000
      !>        15:  0.049579 -0.024759  0.000000
      !>        16:  0.064778 -0.013399  0.000000
      !>
      !>     2) As a second example, suppose that U and V are orthonormal
      !>        vectors that form a basis of a plane. Moreover suppose that we
      !>        wish to project a vector X onto this plane.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VLCOM_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VDOT
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      PUV    ( 3 )
      !>      DOUBLE PRECISION      X      ( 3 )
      !>      DOUBLE PRECISION      U      ( 3 )
      !>      DOUBLE PRECISION      V      ( 3 )
      !>
      !>C
      !>C     Let X be an arbitrary 3-vector
      !>C
      !>      DATA                  X  /  4.D0, 35.D0, -5.D0  /
      !>
      !>C
      !>C     Let U and V be orthonormal 3-vectors spanning the
      !>C     plane of interest.
      !>C
      !>      DATA                  U  /  0.D0,  0.D0,  1.D0 /
      !>
      !>      V(1) =  SQRT(2.D0)/2.D0
      !>      V(2) = -SQRT(2.D0)/2.D0
      !>      V(3) =  0.D0
      !>
      !>C
      !>C     Compute the projection of X onto this 2-dimensional
      !>C     plane in 3-space.
      !>C
      !>      CALL VLCOM ( VDOT ( X, U ), U, VDOT ( X, V ), V, PUV )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A,3F6.1)') 'Input vector             : ', X
      !>      WRITE(*,'(A,3F6.1)') 'Projection into 2-d plane: ', PUV
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector             :    4.0  35.0  -5.0
      !>        Projection into 2-d plane:  -15.5  15.5  -5.0
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to guard against numeric
      !>         overflow or underflow. The user is responsible for insuring
      !>         that the input values are reasonable.
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
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine vlcom ( a, v1, b, v2, sum )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: a  !! Coefficient of V1.
      real(wp), intent(in) :: v1  ( 3 )  !! Vector in 3-space.
      real(wp), intent(in) :: b  !! Coefficient of V2.
      real(wp), intent(in) :: v2  ( 3 )  !! Vector in 3-space.
      real(wp), intent(out) :: sum ( 3 )  !! Linear vector combination A*V1 + B*V2.

      sum = a*v1 + b*v2
      end subroutine vlcom

      !> Compute the vector linear combination of three double precision
      !> 3-dimensional vectors.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     A        is the double precision scalar variable that multiplies
      !>              V1.
      !>
      !>     V1       is an arbitrary, double precision 3-dimensional vector.
      !>
      !>     B        is the double precision scalar variable that multiplies
      !>              V2.
      !>
      !>     V2       is an arbitrary, double precision 3-dimensional vector.
      !>
      !>     C        is the double precision scalar variable that multiplies
      !>              V3.
      !>
      !>     V3       is a double precision 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     SUM      is the double precision 3-dimensional vector which
      !>              contains the linear combination
      !>
      !>                 A * V1 + B * V2 + C * V3
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the index I, from 1 to 3:
      !>
      !>           SUM(I) = A * V1(I) + B * V2(I) + C * V3(I)
      !>
      !>     No error checking is performed to guard against numeric overflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you have an instrument with an elliptical field
      !>        of view described by its angular extent along the semi-minor
      !>        and semi-major axes.
      !>
      !>        The following code example demonstrates how to create
      !>        16 vectors aiming at visualizing the field-of-view in
      !>        three dimensional space.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VLCOM3_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      TWOPI
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>C     Define the two angular extends, along the semi-major
      !>C     (U) and semi-minor (V) axes of the elliptical field
      !>C     of view, in radians.
      !>C
      !>      DOUBLE PRECISION      MAXANG
      !>      PARAMETER           ( MAXANG = 0.07D0 )
      !>
      !>      DOUBLE PRECISION      MINANG
      !>      PARAMETER           ( MINANG = 0.035D0 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      A
      !>      DOUBLE PRECISION      B
      !>      DOUBLE PRECISION      STEP
      !>      DOUBLE PRECISION      THETA
      !>      DOUBLE PRECISION      U      ( 3 )
      !>      DOUBLE PRECISION      V      ( 3 )
      !>      DOUBLE PRECISION      VECTOR ( 3 )
      !>      DOUBLE PRECISION      Z      ( 3 )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Let U and V be orthonormal 3-vectors spanning the
      !>C     focal plane of the instrument, and Z its
      !>C     boresight.
      !>C
      !>      DATA                  U  /  1.D0,  0.D0,  0.D0 /
      !>      DATA                  V  /  0.D0,  1.D0,  0.D0 /
      !>      DATA                  Z  /  0.D0,  0.D0,  1.D0 /
      !>
      !>C
      !>C     Find the length of the ellipse's axes. Note that
      !>C     we are dealing with unitary vectors.
      !>C
      !>      A = TAN ( MAXANG )
      !>      B = TAN ( MINANG )
      !>
      !>C
      !>C     Compute the vectors of interest and display them
      !>C
      !>      THETA = 0.D0
      !>      STEP  = TWOPI() / 16
      !>
      !>      DO I = 1, 16
      !>
      !>         CALL VLCOM3 ( 1.D0,           Z, A * COS(THETA), U,
      !>     .                 B * SIN(THETA), V, VECTOR            )
      !>
      !>         WRITE(*,'(I2,A,3F10.6)') I, ':', VECTOR
      !>
      !>         THETA = THETA + STEP
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         1:  0.070115  0.000000  1.000000
      !>         2:  0.064777  0.013399  1.000000
      !>         3:  0.049578  0.024759  1.000000
      !>         4:  0.026832  0.032349  1.000000
      !>         5:  0.000000  0.035014  1.000000
      !>         6: -0.026832  0.032349  1.000000
      !>         7: -0.049578  0.024759  1.000000
      !>         8: -0.064777  0.013399  1.000000
      !>         9: -0.070115  0.000000  1.000000
      !>        10: -0.064777 -0.013399  1.000000
      !>        11: -0.049578 -0.024759  1.000000
      !>        12: -0.026832 -0.032349  1.000000
      !>        13: -0.000000 -0.035014  1.000000
      !>        14:  0.026832 -0.032349  1.000000
      !>        15:  0.049578 -0.024759  1.000000
      !>        16:  0.064777 -0.013399  1.000000
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to guard against numeric
      !>         overflow or underflow. The user is responsible for insuring
      !>         that the input values are reasonable.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added restriction #1.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 01-NOV-1990 (WLT)
      pure subroutine vlcom3 ( a, v1, b, v2, c, v3, sum )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: a  !! Coefficient of V1.
      real(wp), intent(in) :: v1  ( 3 )  !! Vector in 3-space.
      real(wp), intent(in) :: b  !! Coefficient of V2.
      real(wp), intent(in) :: v2  ( 3 )  !! Vector in 3-space.
      real(wp), intent(in) :: c  !! Coefficient of V3.
      real(wp), intent(in) :: v3  ( 3 )  !! Vector in 3-space.
      real(wp), intent(out) :: sum ( 3 )  !! Linear vector combination A*V1 + B*V2 + C*V3.

      sum = a*v1 + b*v2 + c*v3
      end subroutine vlcom3

      !> Compute a vector linear combination of two double precision
      !> vectors of arbitrary dimension.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     N        is the dimension of V1, V2 and SUM.
      !>
      !>     A        is the double precision scalar variable that multiplies
      !>              V1.
      !>
      !>     V1       is an arbitrary, double precision n-dimensional vector.
      !>
      !>     B        is the double precision scalar variable that multiplies
      !>              V2.
      !>
      !>     V2       is an arbitrary, double precision n-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     SUM      is the double precision n-dimensional vector which
      !>              contains the linear combination
      !>
      !>                 A * V1 + B * V2
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The code reflects precisely the following mathematical expression
      !>
      !>        For each value of the index I, from 1 to N:
      !>
      !>           SUM(I) = A * V1(I) + B * V2(I)
      !>
      !>     No error checking is performed to guard against numeric overflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Perform the projection of a 4-dimensional vector into a
      !>        2-dimensional plane in 4-space.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VLCOMG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VDOTG
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      PUV    ( NDIM )
      !>      DOUBLE PRECISION      X      ( NDIM )
      !>      DOUBLE PRECISION      U      ( NDIM )
      !>      DOUBLE PRECISION      V      ( NDIM )
      !>
      !>C
      !>C     Let X be an arbitrary NDIM-vector
      !>C
      !>      DATA                  X  /  4.D0, 35.D0, -5.D0, 7.D0  /
      !>
      !>C
      !>C     Let U and V be orthonormal NDIM-vectors spanning the
      !>C     plane of interest.
      !>C
      !>      DATA                  U  /  0.D0,  0.D0,  1.D0, 0.D0 /
      !>
      !>      V(1) =  SQRT(3.D0)/3.D0
      !>      V(2) = -SQRT(3.D0)/3.D0
      !>      V(3) =  0.D0
      !>      V(4) =  SQRT(3.D0)/3.D0
      !>
      !>C
      !>C     Compute the projection of X onto this 2-dimensional
      !>C     plane in NDIM-space.
      !>C
      !>      CALL VLCOMG ( NDIM, VDOTG ( X, U, NDIM), U,
      !>     .                    VDOTG ( X, V, NDIM), V, PUV )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A,4F6.1)') 'Input vector             : ', X
      !>      WRITE(*,'(A,4F6.1)') 'Projection into 2-d plane: ', PUV
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector             :    4.0  35.0  -5.0   7.0
      !>        Projection into 2-d plane:   -8.0   8.0  -5.0  -8.0
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to guard against numeric
      !>         overflow or underflow. The user is responsible for insuring
      !>         that the input values are reasonable.
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
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine vlcomg ( n, a, v1, b, v2, sum )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: n  !! Dimension of vector space.
      real(wp), intent(in) :: a  !! Coefficient of V1.
      real(wp), intent(in) :: v1  ( n )  !! Vector in N-space.
      real(wp), intent(in) :: b  !! Coefficient of V2.
      real(wp), intent(in) :: v2  ( n )  !! Vector in N-space.
      real(wp), intent(out) :: sum ( n )  !! Linear vector combination A*V1 + B*V2.

      sum(1:n) = a*v1(1:n) + b*v2(1:n)
      end subroutine vlcomg

      !> Negate a double precision vector of arbitrary dimension.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     VIN      is any double precision vector of arbitrary size.
      !>
      !>     NDIM     is the dimension of VIN and VOUT.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a n-dimensional double precision vector which
      !>              contains the negation of VIN.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For each value of the index I from 1 to NDIM, VMINUG negates VIN
      !>     by the expression:
      !>
      !>        VOUT(I) = - VIN(I)
      !>
      !>     No error checking is performed since overflow can occur ONLY if
      !>     the dynamic range of positive floating point numbers is not the
      !>     same size as the dynamic range of negative floating point numbers
      !>     AND at least one component of VIN falls outside the common range.
      !>     The likelihood of this occurring is so small as to be of no
      !>     concern.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of n-dimensional vectors and negate each of them.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VMINUG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      VIN  ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define a set of n-dimensional vectors.
      !>C
      !>      DATA                  VIN  /
      !>     .                    -10.D0, 15.D0, -5.D0, 20.D0,
      !>     .                      0.D0,  0.D0,  0.D0,  0.D0  /
      !>
      !>C
      !>C     Negate each vector
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VMINUG ( VIN(1,I), NDIM, VOUT )
      !>
      !>         WRITE(*,'(A,4F7.1)') 'Input vector  : ',
      !>     .                        ( VIN(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4F7.1)') 'Negated vector: ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector  :   -10.0   15.0   -5.0   20.0
      !>        Negated vector:    10.0  -15.0    5.0  -20.0
      !>
      !>        Input vector  :     0.0    0.0    0.0    0.0
      !>        Negated vector:    -0.0   -0.0   -0.0   -0.0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.3, 02-OCT-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>        Extended $Particulars section.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vminug ( vin, ndim, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of VIN (and also VOUT).
      real(wp), intent(in) :: vin   ( ndim )  !! n-dimensional vector to be negated.
      real(wp), intent(out) :: vout  ( ndim )  !! Negated vector -V1.

      vout(1:ndim) = -vin(1:ndim)
      end subroutine vminug

      !> Negate a double precision 3-dimensional vector.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     V1       is any double precision 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is the negation (additive inverse) of V1. It is a
      !>              double precision 3-dimensional vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For each value of the index I from 1 to 3, VMINUS negates V1
      !>     by the expression:
      !>
      !>        VOUT(I) = - V1(I)
      !>
      !>     No error checking is performed since overflow can occur ONLY if
      !>     the dynamic range of positive floating point numbers is not the
      !>     same size as the dynamic range of negative floating point numbers
      !>     AND at least one component of V1 falls outside the common range.
      !>     The likelihood of this occurring is so small as to be of no
      !>     concern.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of 3-dimensional vectors and negate each of them.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VMINUS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( 3, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( 3         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define a set of 3-dimensional vectors.
      !>C
      !>      DATA                  V1  /  1.D0, -2.D0, 0.D0,
      !>     .                             0.D0,  0.D0, 0.D0  /
      !>
      !>C
      !>C     Negate each vector
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VMINUS ( V1(1,I), VOUT )
      !>
      !>         WRITE(*,'(A,3F6.1)') 'Input vector  : ',
      !>     .                        ( V1(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F6.1)') 'Negated vector: ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector  :    1.0  -2.0   0.0
      !>        Negated vector:   -1.0   2.0  -0.0
      !>
      !>        Input vector  :    0.0   0.0   0.0
      !>        Negated vector:   -0.0  -0.0  -0.0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.3, 02-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vminus ( v1, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! Vector to be negated.
      real(wp), intent(out) :: vout ( 3 )  !! Negated vector -V1.

      vout = -v1
      end subroutine vminus

      !> Compute the magnitude of a double precision 3-dimensional
      !> vector.
      !>
      !> The function returns the magnitude of V1.
      !>
      !>## Detailed Input
      !>
      !>     V1       is any double precision 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the magnitude of V1 calculated in a
      !>     numerically stable way.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VNORM takes care to avoid overflow while computing the norm of the
      !>     input vector V1. VNORM finds the component of V1 whose magnitude
      !>     is the largest. Calling this magnitude V1MAX, the norm is computed
      !>     using the formula:
      !>
      !>                        ||    1         ||
      !>        VNORM = V1MAX * || ------- * V1 ||
      !>                        ||  V1MAX       ||
      !>
      !>     where the notation ||X|| indicates the norm of the vector X.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a set of 3-dimensional vectors and compute the
      !>        magnitude of each vector within.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VNORM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VNORM
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( 3, SETSIZ )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define a set of 3-dimensional vectors.
      !>C
      !>      DATA                  V1  /  1.D0,   2.D0,   2.D0,
      !>     .                             5.D0,  12.D0,   0.D0,
      !>     .                            -5.D-17, 0.0D0, 12.D-17  /
      !>
      !>C
      !>C     Calculate the magnitude of each vector
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         WRITE(*,'(A,3E10.2)') 'Input vector: ',
      !>     .                         ( V1(J,I), J=1,3 )
      !>         WRITE(*,'(A,F24.20)') 'Magnitude   : ',
      !>     .                         VNORM ( V1(1,I) )
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector:   0.10E+01  0.20E+01  0.20E+01
      !>        Magnitude   :   3.00000000000000000000
      !>
      !>        Input vector:   0.50E+01  0.12E+02  0.00E+00
      !>        Magnitude   :  13.00000000000000000000
      !>
      !>        Input vector:  -0.50E-16  0.00E+00  0.12E-15
      !>        Magnitude   :   0.00000000000000013000
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 06-JUL-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function vnorm ( v1 )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1 ( 3 )  !! Vector whose magnitude is to be found.

      real(wp) :: v1max

!
!  Determine the maximum component of the vector.
!
      v1max = max (dabs(v1(1)), dabs(v1(2)), dabs(v1(3)))

!
!  If the vector is zero, return zero; otherwise normalize first.
!  Normalizing helps in the cases where squaring would cause overflow
!  or underflow.  In the cases where such is not a problem it not worth
!  it to optimize further.
!
      if (v1max==0.0_wp) then
         vnorm = 0.0_wp
      else
         vnorm = v1max * dsqrt (  (v1(1)/v1max)**2 &
                                + (v1(2)/v1max)**2 &
                                + (v1(3)/v1max)**2)
      end if
      return
      end function vnorm

      !> Compute the magnitude of a double precision vector of arbitrary
      !> dimension.
      !>
      !> The function returns the magnitude of V1.
      !>
      !>## Detailed Input
      !>
      !>     V1       is any double precision vector of arbitrary dimension.
      !>
      !>     NDIM     is the dimension of the input vector V1.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the magnitude of V1 calculated in a
      !>     numerically stable way.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     VNORMG finds the component of V1 whose magnitude is the largest.
      !>     If the absolute magnitude of that component indicates that a
      !>     numeric overflow would occur when it is squared, or if it
      !>     indicates that an underflow would occur when squared (falsely
      !>     giving a magnitude of zero) then the following expression is
      !>     used:
      !>
      !>        VNORMG = V1MAX * MAGNITUDE OF [ (1/V1MAX)*V1 ]
      !>
      !>     Otherwise a simpler expression is used:
      !>
      !>        VNORMG = MAGNITUDE OF [ V1 ]
      !>
      !>     Beyond the logic described above, no further checking of the
      !>     validity of the input is performed.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a four-dimensional vector and calculate its magnitude.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VNORMG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      VNORMG
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1 ( NDIM )
      !>
      !>      DATA                  V1 / 12.3D0, -4.32D0,
      !>     .                           76.0D0,  1.87D0 /
      !>
      !>C
      !>C     Compute the magnitude of V1
      !>C
      !>      WRITE(*,*) 'Magnitude of v1: ', VNORMG ( V1, NDIM )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Magnitude of v1:    77.132673362201047
      !>
      !>     2) The following table show the correlation between various input
      !>        vectors V1 and VNORMG:
      !>
      !>           NDIM    V1(NDIM)                          VNORMG
      !>           -------------------------------------------------
      !>            1      (-7.0D20)                          7.D20
      !>            3      (1.D0, 2.D0, 2.D0)                 3.D0
      !>            4      (3.D0, 3.D0, 3.D0, 3.D0)           6.D0
      !>            5      (5.D0, 12.D0, 0.D0, 0.D0, 0.D0)   13.D0
      !>            3      (-5.D-17, 0.D0, 12.D-17)          13.D-17
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Edited the header comments to comply with NAIF standard. Added
      !>        complete code example.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function vnormg ( v1, ndim )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of V1.
      real(wp), intent(in) :: v1   ( ndim )  !! Vector whose magnitude is to be found.

!
!     Local variables.
!
      integer(ip) :: i
      real(wp) :: v1max
      real(wp) :: a

!
!  Determine the maximum component of the vector.
!
      v1max = 0.0_wp

      do i=1,ndim

         if ( dabs(v1(i)) > v1max ) &
            v1max = dabs(v1(i))

      end do
!
!  If the vector is zero, return zero; otherwise normalize first.
!  Normalizing helps in the cases where squaring would cause overflow
!  or underflow.  In the cases where such is not a problem it not worth
!  it to optimize further.
!
      if ( v1max == 0.0_wp ) then

         vnormg = 0.0_wp

      else

         vnormg = 0.0_wp

         do i = 1, ndim
            a      = v1(i)  / v1max
            vnormg = vnormg + a*a
         end do

         vnormg = v1max * dsqrt (vnormg)

      end if
      return
      end function vnormg

      !> Pack three scalar components into a vector.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (an array constructor instead).
      !>
      !>## Detailed Input
      !>
      !>     X,
      !>     Y,
      !>     Z        are the scalar components of a 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     V        is the equivalent vector, such that
      !>
      !>                 V(1) = X
      !>                 V(2) = Y
      !>                 V(3) = Z
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Basically, this is just shorthand notation for the common
      !>     sequence
      !>
      !>        V(1) = X
      !>        V(2) = Y
      !>        V(3) = Z
      !>
      !>     The routine is useful largely for two reasons. First, it
      !>     reduces the chance that the programmer will make a "cut and
      !>     paste" mistake, like
      !>
      !>        V(1) = X
      !>        V(1) = Y
      !>        V(1) = Z
      !>
      !>     Second, it makes conversions between equivalent units simpler,
      !>     and clearer. For instance, the sequence
      !>
      !>        V(1) = X * RPD
      !>        V(2) = Y * RPD
      !>        V(3) = Z * RPD
      !>
      !>     can be replaced by the (nearly) equivalent sequence
      !>
      !>        CALL VPACK  ( X,   Y, Z, V )
      !>        CALL VSCLIP ( RPD, V       )
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input
      !>     (if any), the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute an upward normal of an equilateral triangle lying
      !>        in the X-Y plane and centered at the origin.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VPACK_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      NORMAL ( 3 )
      !>      DOUBLE PRECISION      S
      !>      DOUBLE PRECISION      V1     ( 3 )
      !>      DOUBLE PRECISION      V2     ( 3 )
      !>      DOUBLE PRECISION      V3     ( 3 )
      !>
      !>      S = SQRT(3.D0)/2
      !>
      !>C
      !>C     Define the three corners of the triangle.
      !>C
      !>      CALL VPACK (    S,  -0.5D0,  0.D0, V1 )
      !>      CALL VPACK ( 0.D0,    1.D0,  0.D0, V2 )
      !>      CALL VPACK (   -S,  -0.5D0,  0.D0, V3 )
      !>
      !>C
      !>C     Compute an upward normal of the triangle.
      !>C
      !>      CALL PLTNRM ( V1, V2, V3, NORMAL )
      !>
      !>      WRITE (*, '(A,3F17.13)' ) 'NORMAL = ', NORMAL
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        NORMAL =   0.0000000000000  0.0000000000000  2.5980762113533
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 16-JUL-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine vpack ( x, y, z, v )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: x  !! Scalar components of a vector.
      real(wp), intent(in) :: y  !! Scalar components of a vector.
      real(wp), intent(in) :: z  !! Scalar components of a vector.
      real(wp), intent(out) :: v ( 3 )  !! Equivalent vector.

!
!     Just shorthand, like it says above.
!
      v = [x, y, z]
      end subroutine vpack

      !> Find the component of a vector that is perpendicular to a second
      !> vector. All vectors are 3-dimensional.
      !>
      !>## Detailed Input
      !>
      !>     A        is a double precision, 3-dimensional vector. It the
      !>              vector whose component orthogonal to B is sought. (There
      !>              is a unique decomposition of A into a sum V + P, where V
      !>              is parallel to B and P is orthogonal to B. We want the
      !>              component P.)
      !>
      !>     B        is a double precision, 3-dimensional vector. This
      !>              vector is the vector used as a reference for the
      !>              decomposition of A.
      !>
      !>## Detailed Output
      !>
      !>     P        is a double precision, 3-dimensional vector containing
      !>              the component of A that is orthogonal to B.
      !>              P may overwrite either A or B.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given and non-zero vector B and a vector A, there is a unique
      !>     decomposition of A as a sum V + P such that P is orthogonal
      !>     to B and V is parallel to B. This routine finds the vector P.
      !>
      !>     If B is a zero vector, P will be identical to A.
      !>
      !>## Examples
      !>
      !>     The following table gives sample inputs and results from calling
      !>     VPERP.
      !>
      !>        A                  B                 P
      !>        ------------------------------------------
      !>        (6, 6, 6)      ( 2, 0, 0)        (0, 6, 6)
      !>        (6, 6, 6)      (-3, 0, 0)        (0, 6, 6)
      !>        (6, 6, 0)      ( 0, 7, 0)        (6, 0, 0)
      !>        (6, 0, 0)      ( 0, 0, 9)        (6, 0, 0)
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 16-SEP-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 11-MAY-2010 (EDW)
      !>
      !>        Minor edit to code comments eliminating typo.
      !>
      !>        Reordered header sections to proper NAIF convention.
      !>        Removed Revision section, it listed a duplication of a
      !>        $Version section entry.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
      pure subroutine vperp ( a, b, p )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: a ( 3 )  !! The vector whose orthogonal component is sought.
      real(wp), intent(in) :: b ( 3 )  !! The vector used as the orthogonal reference.
      real(wp), intent(out) :: p ( 3 )  !! The component of A orthogonal to B.

      real(wp) :: biga
      real(wp) :: bigb
      real(wp) :: r(3)
      real(wp) :: t(3)
      real(wp) :: v(3)

!
!     Error free routine:  no check-in.
!
      biga = max ( dabs(a(1)), dabs(a(2)), dabs(a(3)) )
      bigb = max ( dabs(b(1)), dabs(b(2)), dabs(b(3)) )

!
!     If A is the zero vector, just set P to zero and return.
!
      if ( biga == 0.0_wp ) then

         p(1) = 0.0_wp
         p(2) = 0.0_wp
         p(3) = 0.0_wp
         return

      end if

!
!     If B is the zero vector, then set P equal to A.
!
      if ( bigb == 0.0_wp ) then

         p(1) = a(1)
         p(2) = a(2)
         p(3) = a(3)
         return

      end if

      t(1) = a(1) / biga
      t(2) = a(2) / biga
      t(3) = a(3) / biga

      r(1) = b(1) / bigb
      r(2) = b(2) / bigb
      r(3) = b(3) / bigb

      call vproj  ( t,    r, v )
      p = t - v
      p = biga*p

      return
      end subroutine vperp

      !> Compute the projection of one 3-dimensional vector onto another
      !> 3-dimensional vector.
      !>
      !>## Detailed Input
      !>
      !>     A        is a double precision, 3-dimensional vector. This
      !>              vector is to be projected onto the vector B.
      !>
      !>     B        is a double precision, 3-dimensional vector. This
      !>              vector is the vector which receives the projection.
      !>
      !>## Detailed Output
      !>
      !>     P        is a double precision, 3-dimensional vector containing
      !>              the projection of A onto B. (P is necessarily parallel
      !>              to B.) If B is the zero vector then P will be returned
      !>              as the zero vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Given any vectors A and B, there is a unique decomposition of
      !>     A as a sum V + P such that V, the dot product of V and B, is zero,
      !>     and the dot product of P with B is equal the product of the
      !>     lengths of P and B. P is called the projection of A onto B. It
      !>     can be expressed mathematically as
      !>
      !>        DOT(A,B)
      !>        -------- * B
      !>        DOT(B,B)
      !>
      !>     (This is not necessarily the prescription used to compute the
      !>     projection. It is intended only for descriptive purposes.)
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of vectors and compute the projection of
      !>        each vector of the first set on the corresponding vector of
      !>        the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VPROJ_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 3 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      PVEC ( NDIM )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  SETA / 6.D0,  6.D0,  6.D0,
      !>     .                             6.D0,  6.D0,  6.D0,
      !>     .                             6.D0,  6.D0,  0.D0,
      !>     .                             6.D0,  0.D0,  0.D0  /
      !>
      !>      DATA                  SETB / 2.D0,  0.D0,  0.D0,
      !>     .                            -3.D0,  0.D0,  0.D0,
      !>     .                             0.D0,  7.D0,  0.D0,
      !>     .                             0.D0,  0.D0,  9.D0  /
      !>
      !>C
      !>C     Calculate the projection
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VPROJ ( SETA(1,I), SETB(1,I), PVEC )
      !>
      !>         WRITE(*,'(A,3F5.1)') 'Vector A  : ',
      !>     .                        ( SETA(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F5.1)') 'Vector B  : ',
      !>     .                        ( SETB(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F5.1)') 'Projection: ', PVEC
      !>         WRITE(*,*) ' '
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector A  :   6.0  6.0  6.0
      !>        Vector B  :   2.0  0.0  0.0
      !>        Projection:   6.0  0.0  0.0
      !>
      !>        Vector A  :   6.0  6.0  6.0
      !>        Vector B  :  -3.0  0.0  0.0
      !>        Projection:   6.0 -0.0 -0.0
      !>
      !>        Vector A  :   6.0  6.0  0.0
      !>        Vector B  :   0.0  7.0  0.0
      !>        Projection:   0.0  6.0  0.0
      !>
      !>        Vector A  :   6.0  0.0  0.0
      !>        Vector B  :   0.0  0.0  9.0
      !>        Projection:   0.0  0.0  0.0
      !>
      !>## Restrictions
      !>
      !>     1)  An implicit assumption exists that A and B are specified in
      !>         the same reference frame. If this is not the case, the
      !>         numerical result has no meaning.
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
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Added entry in $Restrictions section.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
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
      !>-    Beta Version 1.1.0, 4-JAN-1989 (WLT)
      !>
      !>        Upgrade the routine to work with negative axis indexes. Also
      !>        take care of the funky way the indices (other than the input)
      !>        were obtained via the MOD function. It works but isn't as
      !>        clear (or fast) as just reading the axes from data.
      pure subroutine vproj ( a, b, p )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: a ( 3 )  !! The vector to be projected.
      real(wp), intent(in) :: b ( 3 )  !! The vector onto which A is to be projected.
      real(wp), intent(out) :: p ( 3 )  !! The projection of A onto B.

      real(wp) :: biga
      real(wp) :: bigb
      real(wp) :: r(3)
      real(wp) :: t(3)
      real(wp) :: scale

      biga = max ( dabs(a(1)), dabs(a(2)), dabs(a(3)) )
      bigb = max ( dabs(b(1)), dabs(b(2)), dabs(b(3)) )

      if ( biga == 0 ) then
         p(1) = 0.0_wp
         p(2) = 0.0_wp
         p(3) = 0.0_wp
         return
      end if

      if (  bigb == 0 ) then
         p(1) = 0.0_wp
         p(2) = 0.0_wp
         p(3) = 0.0_wp
         return
      end if

      r(1)  = b(1) / bigb
      r(2)  = b(2) / bigb
      r(3)  = b(3) / bigb

      t(1)  = a(1) / biga
      t(2)  = a(2) / biga
      t(3)  = a(3) / biga

      scale = vdot (t,r) * biga  / vdot (r,r)

      p = scale*r

      return
      end subroutine vproj

      !> Compute the projection of one vector onto another vector. All
      !> vectors are of arbitrary dimension.
      !>
      !>## Detailed Input
      !>
      !>     A        is a double precision vector of arbitrary dimension.
      !>              This vector is to be projected onto the vector B.
      !>
      !>     B        is a double precision vector of arbitrary dimension.
      !>              This vector is the vector which receives the
      !>              projection.
      !>
      !>     NDIM     is the dimension of A, B and P.
      !>
      !>## Detailed Output
      !>
      !>     P        is a double precision vector of arbitrary dimension
      !>              containing the projection of A onto B. (P is
      !>              necessarily parallel to B.)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The projection of a vector A onto a vector B is, by definition,
      !>     that component of A which is parallel to B. To find this
      !>     component it is enough to find the scalar ratio of the length of
      !>     B to the projection of A onto B, and then use this number to
      !>     scale the length of B. This ratio is given by
      !>
      !>         RATIO = < A, B > / < B, B >
      !>
      !>     where <,> denotes the general vector dot product. This routine
      !>     does not attempt to divide by zero in the event that B is the
      !>     zero vector.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of vectors and compute the projection of
      !>        each vector of the first set on the corresponding vector of
      !>        the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VPROJG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 4 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      SETA ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      SETB ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      PVEC ( NDIM )
      !>
      !>      INTEGER               I
      !>      INTEGER               M
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  SETA / 6.D0,  6.D0,  6.D0,  0.D0,
      !>     .                             6.D0,  6.D0,  6.D0,  0.D0,
      !>     .                             6.D0,  6.D0,  0.D0,  0.D0,
      !>     .                             6.D0,  0.D0,  0.D0,  0.D0 /
      !>
      !>      DATA                  SETB / 2.D0,  0.D0,  0.D0,  0.D0,
      !>     .                            -3.D0,  0.D0,  0.D0,  0.D0,
      !>     .                             0.D0,  7.D0,  0.D0,  0.D0,
      !>     .                             0.D0,  0.D0,  9.D0,  0.D0 /
      !>
      !>C
      !>C     Calculate the projection
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VPROJG ( SETA(1,I), SETB(1,I), NDIM, PVEC )
      !>         WRITE(*,'(A,4F5.1)') 'Vector A  : ',
      !>     .                     ( SETA(M,I), M = 1, NDIM )
      !>         WRITE(*,'(A,4F5.1)') 'Vector B  : ',
      !>     .                     ( SETB(M,I), M = 1, NDIM )
      !>         WRITE(*,'(A,4F5.1)') 'Projection: ', PVEC
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Vector A  :   6.0  6.0  6.0  0.0
      !>        Vector B  :   2.0  0.0  0.0  0.0
      !>        Projection:   6.0  0.0  0.0  0.0
      !>
      !>        Vector A  :   6.0  6.0  6.0  0.0
      !>        Vector B  :  -3.0  0.0  0.0  0.0
      !>        Projection:   6.0 -0.0 -0.0 -0.0
      !>
      !>        Vector A  :   6.0  6.0  0.0  0.0
      !>        Vector B  :   0.0  7.0  0.0  0.0
      !>        Projection:   0.0  6.0  0.0  0.0
      !>
      !>        Vector A  :   6.0  0.0  0.0  0.0
      !>        Vector B  :   0.0  0.0  9.0  0.0
      !>        Projection:   0.0  0.0  0.0  0.0
      !>
      !>## Restrictions
      !>
      !>     1)  No error detection or recovery schemes are incorporated into
      !>         this routine except to insure that no attempt is made to
      !>         divide by zero. Thus, the user is required to make sure that
      !>         the vectors A and B are such that no floating point overflow
      !>         will occur when the dot products are calculated.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Corrected math
      !>        expression in $Particulars section. Removed unnecessary
      !>        $Revisions section.
      !>
      !>        Added complete code example to $Examples section based on
      !>        existing example.
      !>
      !>-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
      pure subroutine vprojg ( a, b, ndim, p )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of A, B, and P.
      real(wp), intent(in) :: a ( ndim )  !! The vector to be projected.
      real(wp), intent(in) :: b ( ndim )  !! The vector onto which A is to be projected.
      real(wp), intent(out) :: p ( ndim )  !! The projection of A onto B.

      real(wp) :: adotb
      real(wp) :: bdotb
      real(wp) :: scale

      adotb  = vdotg (a,b,ndim)
      bdotb  = vdotg (b,b,ndim)

      if ( bdotb == 0.0_wp ) then
         scale = 0.0_wp
      else
         scale = adotb/bdotb
      end if

      p = scale*b

      return
      end subroutine vprojg

      !> Return the relative difference between two 3-dimensional vectors.
      !>
      !> The function returns the relative difference between two
      !> 3-dimensional vectors.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two 3-dimensional vectors for which the relative
      !>              difference is to be computed.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the relative difference between the two input
      !>     3-dimensional vectors V1 and V2.
      !>
      !>     It is defined as:
      !>
      !>                          || V1 - V2 ||
      !>        VREL   =   ------------------------
      !>                    MAX ( ||V1||, ||V2|| )
      !>
      !>     where ||X|| indicates the Euclidean norm of the vector X.
      !>
      !>     VREL assumes values in the range [0,2]. If both V1 and V2 are zero
      !>     vectors then VREL is defined to be zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If both V1 and V2 are zero vectors, then VREL is defined
      !>         to be zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function computes the relative difference between two
      !>     3-dimensional vectors as defined above.
      !>
      !>     The function VRELG may be used to find the relative difference
      !>     for two vectors of general dimension.
      !>
      !>## Examples
      !>
      !>     This example code fragment computes the relative difference
      !>     between the geometric and light time corrected state of Io
      !>     with respect to Voyager 2 at a given UTC time.
      !>
      !>     C
      !>     C     The NAIF integer code for Io is 501 and the code for
      !>     C     Voyager 2 is -32.
      !>     C
      !>
      !>           INTEGER               IO
      !>           PARAMETER           ( IO  = 501 )
      !>
      !>           INTEGER               VG2
      !>           PARAMETER           ( VG2 = -32 )
      !>
      !>     C
      !>     C     SPICELIB function
      !>     C
      !>           DOUBLE PRECISION      VREL
      !>     C
      !>     C     Local variables
      !>     C
      !>           DOUBLE PRECISION      STATE ( 6 )
      !>           DOUBLE PRECISION      POS1  ( 3 )
      !>           DOUBLE PRECISION      POS2  ( 3 )
      !>           DOUBLE PRECISION      DIFF
      !>           DOUBLE PRECISION      LT
      !>           DOUBLE PRECISION      ET
      !>
      !>           INTEGER               HANDLE
      !>
      !>           CHARACTER*(20)        UTC
      !>
      !>           DATA                  UTC / '1979 JUN 25 12:00:00' /
      !>
      !>     C
      !>     C     Load the sample SPK ephemeris file.
      !>     C
      !>           CALL SPKLEF ( 'VG2_JUP.BSP', HANDLE )
      !>     C
      !>     C     Convert the UTC time string to ephemeris time.
      !>     C
      !>           CALL UTC2ET ( UTC, ET )
      !>     C
      !>     C     First calculate the geometric state and then the light
      !>     C     time corrected state.
      !>     C
      !>           CALL SPKEZ ( IO, ET, 'J2000', 'NONE', VG2, STATE, LT )
      !>
      !>           CALL VEQU  ( STATE, POS1 )
      !>
      !>           CALL SPKEZ ( IO, ET, 'J2000', 'LT', VG2, STATE, LT )
      !>
      !>           CALL VEQU  ( STATE, POS2 )
      !>     C
      !>     C     Call VREL to find the relative difference between the
      !>     C     two states.
      !>     C
      !>           DIFF = VREL ( POS1, POS2 )
      !>
      !>           .
      !>           .
      !>           .
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
      !>     J.M. Lynch         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUN-1992 (JML)
      pure real(wp) function vrel ( v1, v2  )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1 ( 3 )  !! Input vectors.
      real(wp), intent(in) :: v2 ( 3 )  !! Input vectors.

      real(wp) :: nunorm
      real(wp) :: denorm

!
!     If the numerator is zero then set VREL equal to zero. Otherwise,
!     perform the rest of the calculation.
!
!     This handles the case where both vectors are zero vectors since
!     the distance between them will be zero.
!
      nunorm = vdist ( v1, v2 )

      if ( nunorm == 0.0_wp ) then

         vrel = 0.0_wp

      else

         denorm = max (  vnorm(v1), vnorm(v2)  )

         vrel = nunorm / denorm

       end if

      return
      end function vrel

      !> Return the relative difference between two vectors of general
      !> dimension.
      !>
      !> The function returns the relative difference between two vectors
      !> of general dimension.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two vectors for which the relative difference is to
      !>              be computed.
      !>
      !>     NDIM     is the dimension of V1 and V2.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the relative difference between the two input
      !>     n-dimensional vectors V1 and V2.
      !>
      !>     It is defined as:
      !>
      !>                          || V1 - V2 ||
      !>        VRELG   =   ------------------------
      !>                     MAX ( ||V1||, ||V2|| )
      !>
      !>     where ||X|| indicates the Euclidean norm of the vector X.
      !>
      !>     VRELG assumes values in the range [0,2]. If both V1 and V2 are
      !>     zero vectors then VRELG is defined to be zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If both V1 and V2 are zero vectors, then VRELG is defined to
      !>         be zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function computes the relative difference between two vectors
      !>     of general dimension as defined above.
      !>
      !>     The function VREL may be used to find the relative difference
      !>     for two 3-dimensional vectors.
      !>
      !>## Examples
      !>
      !>     This example determines if the state of Jupiter, with respect
      !>     to Voyager 2, for a set of times is the same for two different
      !>     ephemeris files. Instead of insisting on absolute equality
      !>     between the state vectors, the program will check if the relative
      !>     difference between the vectors is greater than a fixed tolerance.
      !>
      !>     C
      !>     C     The NAIF code for Jupiter is 599 and for Voyager 2 is -32.
      !>     C     Set the tolerance to be 0.0005.
      !>     C
      !>           INTEGER               JUP
      !>           PARAMETER           ( JUP = 599 )
      !>
      !>           INTEGER               VG2
      !>           PARAMETER           ( VG2 = -32 )
      !>
      !>           INTEGER               NUM
      !>           PARAMETER           ( NUM = 500 )
      !>
      !>           DOUBLE PRECISION      TOL
      !>           PARAMETER           ( TOL = 5.D-04 )
      !>
      !>     C
      !>     C     SPICELIB function
      !>     C
      !>           DOUBLE PRECISION      VRELG
      !>     C
      !>     C     Local variables
      !>     C
      !>           DOUBLE PRECISION      STATE1 ( 6, NUM )
      !>           DOUBLE PRECISION      STATE2 ( 6, NUM )
      !>           DOUBLE PRECISION      ET     (    NUM )
      !>           DOUBLE PRECISION      LT
      !>           DOUBLE PRECISION      DIFF
      !>
      !>           INTEGER               HANDLE
      !>           INTEGER               I
      !>
      !>           .
      !>           .
      !>           .
      !>
      !>           C
      !>           C     Load  the first SPK file.
      !>           C
      !>                 CALL SPKLEF ( 'VG2_SOURCE_1.BSP', HANDLE )
      !>           C
      !>           C     Find the states for each time in the array ET.
      !>           C     This example assumes that the SPK file can
      !>           C     provide states for all of the times in the array.
      !>           C
      !>                 DO I = 1, NUM
      !>
      !>                    CALL SPKEZ ( JUP, ET(I),      'J2000', 'LT',
      !>                .                VG2, STATE1(1,I), LT           )
      !>
      !>                 END DO
      !>           C
      !>           C     Unload the first file and load the second one.
      !>           C
      !>                 CALL SPKUEF ( HANDLE )
      !>
      !>                 CALL SPKLEF ( 'VG2_SOURCE_2.BSP', HANDLE )
      !>           C
      !>           C     Find the states from the new file.
      !>           C
      !>                 DO I = 1, NUM
      !>
      !>                    CALL SPKEZ ( JUP, ET(I),      'J2000', 'LT',
      !>                .                VG2, STATE2(1,I), LT           )
      !>
      !>                 END DO
      !>           C
      !>           C     Now compare the two state vectors for each time.
      !>           C
      !>                 DO I = 1, NUM
      !>
      !>                    DIFF = VRELG ( STATE1(1,I), STATE2(1,I), 6 )
      !>
      !>                    IF ( DIFF .GT. TOL ) THEN
      !>
      !>                       .
      !>                       .
      !>                       .
      !>
      !>                    END IF
      !>
      !>                 END DO
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
      !>     J.M. Lynch         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 15-JUN-1992 (JML)
      pure real(wp) function vrelg ( v1, v2, ndim )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: v1 ( * )  !! Input vectors.
      real(wp), intent(in) :: v2 ( * )  !! Input vectors.
      integer(ip), intent(in) :: ndim  !! Dimension of V1 and V2.

      real(wp) :: nunorm
      real(wp) :: denorm

!
!     If the numerator is zero then set VRELG equal to zero. Otherwise,
!     perform the rest of the calculation.
!
!     This handles the case where both vectors are zero vectors since
!     the distance between them will be zero.
!
      nunorm = vdistg ( v1, v2, ndim )

      if ( nunorm == 0.0_wp ) then

         vrelg = 0.0_wp

      else

         denorm = max (  vnormg(v1,ndim), vnormg(v2,ndim)  )

         vrelg = nunorm / denorm

       end if

      return
      end function vrelg

      !> Rotate a vector about a specified axis vector by a specified
      !> angle and return the rotated vector.
      !>
      !>## Detailed Input
      !>
      !>     V        is a 3-dimensional vector to be rotated.
      !>
      !>     AXIS     is the axis about which the rotation is to be
      !>              performed.
      !>
      !>     THETA    is the angle through which V is to be rotated about
      !>              AXIS.
      !>
      !>## Detailed Output
      !>
      !>     R        is the result of rotating V about AXIS by THETA.
      !>              If AXIS is the zero vector, R = V.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the input axis is the zero vector, R will be returned
      !>         as V.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the result of rotating (in a right handed
      !>     sense) the vector V about the axis represented by AXIS through
      !>     an angle of THETA radians.
      !>
      !>     If W is a unit vector parallel to AXIS, then R is given by:
      !>
      !>         R = V + ( 1 - cos(THETA) ) Wx(WxV) + sin(THETA) (WxV)
      !>
      !>     where "x" above denotes the vector cross product.
      !>
      !>## Examples
      !>
      !>     If AXIS = ( 0, 0, 1 ) and THETA = PI/2 then the following results
      !>     for R will be obtained
      !>
      !>             V                           R
      !>        -------------             ----------------
      !>        ( 1, 2, 3 )                ( -2, 1, 3 )
      !>        ( 1, 0, 0 )                (  0, 1, 0 )
      !>        ( 0, 1, 0 )                ( -1, 0, 0 )
      !>
      !>     If AXIS = ( 0, 1, 0 ) and THETA = PI/2 then the following results
      !>     for R will be obtained
      !>
      !>             V                           R
      !>        -------------             ----------------
      !>        ( 1, 2, 3 )                (  3, 2, -1 )
      !>        ( 1, 0, 0 )                (  0, 0, -1 )
      !>        ( 0, 1, 0 )                (  0, 1,  0 )
      !>
      !>     If AXIS = ( 1, 1, 1 ) and THETA = PI/2 then the following results
      !>     for R will be obtained
      !>
      !>             V                                     R
      !>        -----------------------------   -----------------------------
      !>        ( 1.0,     2.0,     3.0     )   ( 2.577.., 0.845.., 2.577.. )
      !>        ( 2.577.., 0.845.., 2.577.. )   ( 3.0      2.0,     1.0     )
      !>        ( 3.0      2.0,     1.0     )   ( 1.422.., 3.154.., 1.422.. )
      !>        ( 1.422.., 3.154.., 1.422.. )   ( 1.0      2.0,     3.0     )
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.2, 05-FEB-2003 (NJB)
      !>
      !>        Header examples were corrected.  $Exceptions section
      !>        filled in. Miscellaneous header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB) (HAN)
      pure subroutine vrotv ( v, axis, theta, r )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v    ( 3 )  !! Vector to be rotated.
      real(wp), intent(in) :: axis ( 3 )  !! Axis of the rotation.
      real(wp), intent(in) :: theta  !! Angle of rotation (radians).
      real(wp), intent(out) :: r    ( 3 )  !! Result of rotating V about AXIS by THETA.

      real(wp) :: c
      real(wp) :: s
      real(wp) :: rplane (3)
      real(wp) :: p      (3)
      real(wp) :: v1     (3)
      real(wp) :: v2     (3)
      real(wp) :: x      (3)

!
!     Just in case the user tries to rotate about the zero vector -
!     check, and if so return the input vector
!
      if ( vnorm( axis ) == 0.0_wp ) then
         r = v
         return
      end if

!
!     Compute the unit vector that lies in the direction of the
!     AXIS.  Call it X.
!
      call vhat ( axis, x )

!
!     Compute the projection of V onto AXIS.  Call it P.
!
      call vproj ( v, x, p )

!
!     Compute the component of V orthogonal to the AXIS.  Call it V1.
!
      v1 = v - p

!
!     Rotate V1 by 90 degrees about the AXIS and call the result V2.
!
      call vcrss  ( x, v1, v2)

!
!     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about
!     the AXIS in the plane normal to the axis, call the result RPLANE
!
      c = dcos (theta)
      s = dsin (theta)

      rplane = c*v1 + s*v2

!
!     Add the rotated component in the normal plane to AXIS to the
!     projection of V onto AXIS (P) to obtain R.
!
      r = rplane + p

      return
      end subroutine vrotv

      !> Multiply a scalar and a double precision 3-dimensional vector.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     S        is a double precision scalar used to multiply the vector
      !>              V1.
      !>
      !>     V1       is a double precision 3-dimensional vector, which is to
      !>              be scaled by S.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a double precision 3-dimensional vector containing
      !>              the product of the scalar with the vector V1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For each value of the index I from 1 to 3, this subroutine
      !>     performs the following multiplication
      !>
      !>        VOUT(I) = S * V1(I)
      !>
      !>     No error checking is performed to guard against numeric overflow
      !>     or underflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a sets of scalar double precision values and use them
      !>        to scale a given 3-dimensional vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VSCL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      S    ( SETSIZ )
      !>      DOUBLE PRECISION      V1   ( 3      )
      !>      DOUBLE PRECISION      VOUT ( 3      )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the set of scalars and the input vector.
      !>C
      !>      DATA                  S    / 3.D0, 0.D0, -1.D0 /
      !>
      !>      DATA                  V1   / 1.D0, 2.D0, -3.D0 /
      !>
      !>      WRITE(*,'(A,3F6.1)') 'Input vector : ', V1
      !>      WRITE(*,*)
      !>
      !>C
      !>C     Calculate product of each scalar and V1.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VSCL ( S(I), V1, VOUT )
      !>
      !>         WRITE(*,'(A,F6.1)')  'Scale factor : ', S(I)
      !>         WRITE(*,'(A,3F6.1)') 'Output vector: ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector :    1.0   2.0  -3.0
      !>
      !>        Scale factor :    3.0
      !>        Output vector:    3.0   6.0  -9.0
      !>
      !>        Scale factor :    0.0
      !>        Output vector:    0.0   0.0  -0.0
      !>
      !>        Scale factor :   -1.0
      !>        Output vector:   -1.0  -2.0   3.0
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for insuring that no floating point
      !>         overflow occurs from multiplying S by any component of V1. No
      !>         error recovery or reporting scheme is incorporated in this
      !>         subroutine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vscl ( s, v1, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: s  !! Scalar to multiply a vector.
      real(wp), intent(in) :: v1   ( 3 )  !! Vector to be multiplied.
      real(wp), intent(out) :: vout ( 3 )  !! Product vector, S * V1.

      vout = s*v1
      end subroutine vscl

      !> Multiply a scalar and a double precision vector of arbitrary
      !> dimension.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     S        is a double precision scalar.
      !>
      !>     V1       is a double precision n-dimensional vector.
      !>
      !>     NDIM     is the dimension of V1 (and VOUT).
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a double precision n-dimensional vector containing
      !>              the product of the scalar with the vector V1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For each value of the index I from 1 to NDIM, this subroutine
      !>     performs the following multiplication
      !>
      !>        VOUT(I) = S * V1(I)
      !>
      !>     No error checking is performed to guard against numeric overflow
      !>     or underflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define a sets of scalar double precision values and use them
      !>        to scale a given n-dimensional vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VSCLG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      S    ( SETSIZ )
      !>      DOUBLE PRECISION      V1   ( NDIM   )
      !>      DOUBLE PRECISION      VOUT ( NDIM   )
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Define the set of scalars and the input vector.
      !>C
      !>      DATA                  S    / 3.D0, 0.D0, -1.D0 /
      !>
      !>      DATA                  V1   / 1.D0, 2.D0, -3.D0, 4.D0  /
      !>
      !>      WRITE(*,'(A,4F6.1)') 'Input vector : ', V1
      !>      WRITE(*,*)
      !>
      !>C
      !>C     Calculate product of each scalar and V1.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VSCLG ( S(I), V1, NDIM, VOUT )
      !>
      !>         WRITE(*,'(A,F6.1)')  'Scale factor : ', S(I)
      !>         WRITE(*,'(A,4F6.1)') 'Output vector: ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector :    1.0   2.0  -3.0   4.0
      !>
      !>        Scale factor :    3.0
      !>        Output vector:    3.0   6.0  -9.0  12.0
      !>
      !>        Scale factor :    0.0
      !>        Output vector:    0.0   0.0  -0.0   0.0
      !>
      !>        Scale factor :   -1.0
      !>        Output vector:   -1.0  -2.0   3.0  -4.0
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to guard against numeric
      !>         overflow. The programmer is thus required to insure that the
      !>         values in V1 and S are reasonable and will not cause overflow.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vsclg ( s, v1, ndim, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of V1 (and also VOUT).
      real(wp), intent(in) :: s  !! Scalar to multiply a vector.
      real(wp), intent(in) :: v1   ( ndim )  !! Vector to be multiplied.
      real(wp), intent(out) :: vout ( ndim )  !! Product vector, S * V1.

      vout(1:ndim) = s*v1(1:ndim)
      end subroutine vsclg

      !> Multiply a scalar and a 3-dimensional double precision vector,
      !> replacing the input vector with the result.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     S        is a double precision scalar used to multiply the vector
      !>              V.
      !>
      !>     V        is a 3-dimensional, double precision vector which is to
      !>              be scaled by S.
      !>
      !>## Detailed Output
      !>
      !>     V        is the 3-dimensional, double precision vector resulting
      !>              from the scalar multiplication
      !>
      !>                 S * V
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is provided for situation where it is convenient to
      !>     scale a vector in place rather than store the result in a
      !>     separate variable. Note that the call
      !>
      !>        CALL VSCL ( S, V, V )
      !>
      !>     is not permitted by the ANSI Fortran 77 standard; this routine
      !>     can be called instead to achieve the same result.
      !>
      !>     VSCLIP multiplies each component of V by S to form the respective
      !>     components of the output vector. No error checking is performed.
      !>
      !>## Examples
      !>
      !>     The following table shows the output V as a function of the
      !>     the inputs V and S.
      !>
      !>        V on input         S          V on output
      !>        -------------------------------------------------------
      !>        (1D0, -2D0, 0D0)   -1D0       (-1D0, 2D0, 0D0)
      !>        (0D0, 0D0, 0D0)     5D0       (0D0, 0D0, 0D0)
      !>
      !>## Restrictions
      !>
      !>     1)  The user is responsible for insuring that no floating point
      !>         overflow occurs from multiplying S by any component of V. No
      !>         error recovery or reporting scheme is incorporated in this
      !>         subroutine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB) (WMO)
      pure subroutine vsclip ( s, v )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: s  !! Scalar by which to multiply a vector.
      real(wp), intent(inout) :: v ( 3 )  !! Vector to be multiplied/result of multiplication.

      v = s*v
      end subroutine vsclip

      !> Find the separation angle in radians between two double
      !> precision, 3-dimensional vectors. This angle is defined as zero
      !> if either vector is zero.
      !>
      !> The function returns the angle between V1 and V2 expressed in
      !> radians.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two double precision 3-dimensional vectors. Either
      !>              V1 or V2, or both, may be the zero vector.
      !>
      !>              An implicit assumption exists that V1 and V2 are
      !>              specified in the same reference frame. If this is not
      !>              the case, the numerical result of this routine has no
      !>              meaning.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the angle between V1 and V2 expressed in
      !>     radians.
      !>
      !>     VSEP is strictly non-negative. If either V1 or V2 is the zero
      !>     vector, then VSEP is defined to be 0 radians.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     In the plane, it is a simple matter to calculate the angle
      !>     between two vectors once the two vectors have been made to be
      !>     unit length. Then, since the two vectors form the two equal
      !>     sides of an isosceles triangle, the length of the third side
      !>     is given by the expression
      !>
      !>           LENGTH = 2.0 * SIN ( VSEP/2.0 )
      !>
      !>     The length is given by the magnitude of the difference of the
      !>     two unit vectors
      !>
      !>           LENGTH = NORM ( U1 - U2 )
      !>
      !>     Once the length is found, the value of VSEP may be calculated
      !>     by inverting the first expression given above as
      !>
      !>           VSEP = 2.0 * ARCSIN ( LENGTH/2.0 )
      !>
      !>     This expression becomes increasingly unstable when VSEP gets
      !>     larger than PI/2 radians or 90 degrees. In this situation (which
      !>     is easily detected by determining the sign of the dot product of
      !>     V1 and V2) the supplementary angle is calculated first and
      !>     then VSEP is given by
      !>
      !>           VSEP = PI - SUPPLEMENTARY_ANGLE
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of 3-dimensional vectors and compute the
      !>        angular separation between each vector in first set and the
      !>        corresponding vector in the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VSEP_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VSEP
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( 3, SETSIZ )
      !>      DOUBLE PRECISION      V2   ( 3, SETSIZ )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  V1 / 1.D0,  0.D0,  0.D0,
      !>     .                           1.D0,  0.D0,  0.D0,
      !>     .                           3.D0,  0.D0,  0.D0  /
      !>
      !>      DATA                  V2 / 1.D0,  0.D0,  0.D0,
      !>     .                           0.D0,  1.D0,  0.D0,
      !>     .                          -5.D0,  0.D0,  0.D0  /
      !>
      !>C
      !>C     Calculate the angular separation between each pair
      !>C     of vectors.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         WRITE(*,'(A,3F6.1)')  'First vector            : ',
      !>     .                        ( V1(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F6.1)')  'Second vector           : ',
      !>     .                        ( V2(J,I), J=1,3 )
      !>         WRITE(*,'(A,F15.10)') 'Angular separation (rad): ',
      !>     .                             VSEP ( V1(1,I), V2(1,I) )
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        First vector            :    1.0   0.0   0.0
      !>        Second vector           :    1.0   0.0   0.0
      !>        Angular separation (rad):    0.0000000000
      !>
      !>        First vector            :    1.0   0.0   0.0
      !>        Second vector           :    0.0   1.0   0.0
      !>        Angular separation (rad):    1.5707963268
      !>
      !>        First vector            :    3.0   0.0   0.0
      !>        Second vector           :   -5.0   0.0   0.0
      !>        Angular separation (rad):    3.1415926536
      !>
      !>## Restrictions
      !>
      !>     1)  The user is required to insure that the input vectors will not
      !>         cause floating point overflow upon calculation of the vector
      !>         dot product since no error detection or correction code is
      !>         implemented. In practice, this is not a significant
      !>         restriction.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example based on existing example.
      !>
      !>-    SPICELIB Version 1.1.1, 17-APR-2006 (EDW)
      !>
      !>        Typo correction to the value of PI/2 in the $Examples
      !>        section, 1.571 instead of 1.71.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
      pure real(wp) function vsep ( v1, v2 )
      use pepper_kinds, only: wp
      use pepper_constants, only: pi
      implicit none

      real(wp), intent(in) :: v1 ( 3 )  !! First vector.
      real(wp), intent(in) :: v2 ( 3 )  !! Second vector.

!     The following declarations represent, respectively:
!        Magnitudes of V1, V2
!        Either of the difference vectors: V1-V2 or V1-(-V2)
!        Unit vectors parallel to V1 and V2
!
      real(wp) :: dmag1
      real(wp) :: dmag2
      real(wp) :: vtemp(3)
      real(wp) :: u1(3)
      real(wp) :: u2(3)

!
!  Calculate the magnitudes of V1 and V2; if either is 0, VSEP = 0
!
      call unorm ( v1, u1, dmag1 )
      if ( dmag1 == 0.0_wp ) then
         vsep = 0.0_wp
         return
      end if

      call unorm ( v2, u2, dmag2 )
      if ( dmag2 == 0.0_wp ) then
         vsep = 0.0_wp
         return
      end if

      if      ( vdot(u1,u2) > 0 ) then
         vtemp(1) = u1(1) - u2(1)
         vtemp(2) = u1(2) - u2(2)
         vtemp(3) = u1(3) - u2(3)

         vsep = 2.0_wp * asin (0.5_wp * vnorm(vtemp))

      else if ( vdot(u1,u2) < 0 ) then
         vtemp(1) = u1(1) + u2(1)
         vtemp(2) = u1(2) + u2(2)
         vtemp(3) = u1(3) + u2(3)

         vsep = pi - 2.0_wp * asin (0.5_wp * vnorm(vtemp))

      else
         vsep = pi / 2.0_wp
      end if

      return
      end function vsep

      !> Find the separation angle in radians between two double precision
      !> vectors of arbitrary dimension. This angle is defined as zero if
      !> either vector is zero.
      !>
      !> The function returns the angle between V1 and V2 expressed in
      !> radians.
      !>
      !>## Detailed Input
      !>
      !>     V1,
      !>     V2       are two double precision vectors of arbitrary dimension.
      !>              Either V1 or V2, or both, may be the zero vector.
      !>
      !>              An implicit assumption exists that V1 and V2 are
      !>              specified in the same reference space. If this is not
      !>              the case, the numerical result of this routine has no
      !>              meaning.
      !>
      !>     NDIM     is the dimension of both V1 and V2.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the angle between V1 and V2 expressed in
      !>     radians.
      !>
      !>     VSEPG is strictly non-negative. For input vectors of four or more
      !>     dimensions, the angle is defined as the generalization of the
      !>     definition for three dimensions. If either V1 or V2 is the zero
      !>     vector, then VSEPG is defined to be 0 radians.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     In four or more dimensions this angle does not have a physically
      !>     realizable interpretation. However, the angle is defined as
      !>     the generalization of the following definition which is valid in
      !>     three or two dimensions:
      !>
      !>        In the plane, it is a simple matter to calculate the angle
      !>        between two vectors once the two vectors have been made to be
      !>        unit length. Then, since the two vectors form the two equal
      !>        sides of an isosceles triangle, the length of the third side
      !>        is given by the expression
      !>
      !>           LENGTH = 2.0 * SIN ( VSEPG/2.0 )
      !>
      !>        The length is given by the magnitude of the difference of the
      !>        two unit vectors
      !>
      !>           LENGTH = NORM ( U1 - U2 )
      !>
      !>        Once the length is found, the value of VSEPG may be calculated
      !>        by inverting the first expression given above as
      !>
      !>           VSEPG = 2.0 * ARCSIN ( LENGTH/2.0 )
      !>
      !>        This expression becomes increasingly unstable when VSEPG gets
      !>        larger than PI/2 radians or 90 degrees. In this situation
      !>        (which is easily detected by determining the sign of the dot
      !>        product of V1 and V2) the supplementary angle is calculated
      !>        first and then VSEPG is given by
      !>
      !>           VSEPG = PI - SUPPLEMENTARY_ANGLE
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of n-dimensional vectors and compute the
      !>        angular separation between each vector in first set and the
      !>        corresponding vector in the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VSEPG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VSEPG
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  V1 /
      !>     .                      1.D0,  0.D0,  0.D0,  0.D0,
      !>     .                      1.D0,  0.D0,  0.D0,  0.D0,
      !>     .                      3.D0,  0.D0,  0.D0,  0.D0   /
      !>
      !>      DATA                  V2 /
      !>     .                      1.D0,  0.D0,  0.D0,  0.D0,
      !>     .                      0.D0,  1.D0,  0.D0,  0.D0,
      !>     .                     -5.D0,  0.D0,  0.D0,  0.D0  /
      !>
      !>C
      !>C     Calculate the angular separation between each pair
      !>C     of vectors.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         WRITE(*,'(A,4F6.1)')  'First vector            : ',
      !>     .                        ( V1(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4F6.1)')  'Second vector           : ',
      !>     .                        ( V2(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,F15.10)') 'Angular separation (rad): ',
      !>     .                VSEPG ( V1(1,I), V2(1,I), NDIM )
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        First vector            :    1.0   0.0   0.0   0.0
      !>        Second vector           :    1.0   0.0   0.0   0.0
      !>        Angular separation (rad):    0.0000000000
      !>
      !>        First vector            :    1.0   0.0   0.0   0.0
      !>        Second vector           :    0.0   1.0   0.0   0.0
      !>        Angular separation (rad):    1.5707963268
      !>
      !>        First vector            :    3.0   0.0   0.0   0.0
      !>        Second vector           :   -5.0   0.0   0.0   0.0
      !>        Angular separation (rad):    3.1415926536
      !>
      !>## Restrictions
      !>
      !>     1)  The user is required to insure that the input vectors will not
      !>         cause floating point overflow upon calculation of the vector
      !>         dot product since no error detection or correction code is
      !>         implemented. In practice, this is not a significant
      !>         restriction.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     C.A. Curzon        (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code example based on existing example.
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (HAN)
      pure real(wp) function vsepg ( v1, v2, ndim )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi
      implicit none

      real(wp), intent(in) :: v1 ( * )  !! First vector.
      real(wp), intent(in) :: v2 ( * )  !! Second vector.
      integer(ip), intent(in) :: ndim  !! The number of elements in V1 and V2.

!     The following declarations represent, respectively:
!        Magnitudes of V1, V2
!        Reciprocals of the magnitudes of V1, V2
!        Magnitude of either of the difference vectors: V1-V2 or
!           V1-(-V2)
!
      real(wp) :: dmag1
      real(wp) :: dmag2
      real(wp) :: r1
      real(wp) :: r2
      real(wp) :: magdif

      integer(ip) :: i

!
!     Calculate the magnitudes of V1 and V2; if either is 0, VSEPG = 0
!
      dmag1 = vnormg ( v1, ndim )
      if ( dmag1 == 0.0_wp ) then
         vsepg = 0.0_wp
         return
      end if

      dmag2 = vnormg ( v2, ndim )
      if ( dmag2 == 0.0_wp ) then
         vsepg = 0.0_wp
         return
      end if

      if      ( vdotg (v1, v2, ndim) > 0 ) then

         r1 = 1.0_wp/dmag1
         r2 = 1.0_wp/dmag2

         magdif = 0.0_wp
         do i = 1, ndim
            magdif = magdif + ( v1(i)*r1 - v2(i)*r2 )**2
         end do
         magdif = dsqrt(magdif)

         vsepg = 2.0_wp * asin (0.5_wp * magdif)

      else if ( vdotg (v1, v2, ndim) < 0 ) then

         r1 = 1.0_wp/dmag1
         r2 = 1.0_wp/dmag2

         magdif = 0.0_wp
         do i = 1, ndim
            magdif = magdif + ( v1(i)*r1 + v2(i)*r2 )**2
         end do
         magdif = dsqrt(magdif)

         vsepg = pi - 2.0_wp * asin (0.5_wp * magdif)

      else
         vsepg = pi / 2.0_wp
      end if

      return
      end function vsepg

      !> Compute the difference between two double precision 3-dimensional
      !> vectors.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     V1       is a double precision 3-dimensional vector which is the
      !>              minuend (i.e. first or left-hand member) in the vector
      !>              subtraction.
      !>
      !>     V2       is a double precision 3-dimensional vector which is the
      !>              subtrahend (i.e. second or right-hand member) in the
      !>              vector subtraction.
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a double precision 3-dimensional vector which
      !>              represents the vector difference, V1 - V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For each value of the index I from 1 to 3, this routine performs
      !>     the following subtraction:
      !>
      !>        VOUT(I) = V1(I) - V2(I)
      !>
      !>     No error checking is performed to guard against numeric overflow
      !>     or underflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of 3-dimensional vectors and compute the
      !>        difference from each vector in first set with the
      !>        corresponding vector in the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VSUB_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( 3, SETSIZ )
      !>      DOUBLE PRECISION      V2   ( 3, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( 3         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  V1 / 1.D0,  2.D0,  3.D0,
      !>     .                           1.D0,  2.D0,  3.D0,
      !>     .                           1.D0,  2.D0,  3.D0  /
      !>
      !>      DATA                  V2 / 1.D0,  1.D0,  1.D0,
      !>     .                          -1.D0, -2.D0, -3.D0,
      !>     .                          -1.D0,  2.D0, -3.D0  /
      !>
      !>C
      !>C     Calculate the difference between each pair of vectors
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VSUB ( V1(1,I), V2(1,I), VOUT )
      !>
      !>         WRITE(*,'(A,3F6.1)') 'First vector : ',
      !>     .                        ( V1(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F6.1)') 'Second vector: ',
      !>     .                        ( V2(J,I), J=1,3 )
      !>         WRITE(*,'(A,3F6.1)') 'Difference   : ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        First vector :    1.0   2.0   3.0
      !>        Second vector:    1.0   1.0   1.0
      !>        Difference   :    0.0   1.0   2.0
      !>
      !>        First vector :    1.0   2.0   3.0
      !>        Second vector:   -1.0  -2.0  -3.0
      !>        Difference   :    2.0   4.0   6.0
      !>
      !>        First vector :    1.0   2.0   3.0
      !>        Second vector:   -1.0   2.0  -3.0
      !>        Difference   :    2.0   0.0   6.0
      !>
      !>## Restrictions
      !>
      !>     1)  The user is required to determine that the magnitude each
      !>         component of the vectors is within the appropriate range so as
      !>         not to cause floating point overflow. No error recovery or
      !>         reporting scheme is incorporated in this routine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.4, 03-JUL-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.3, 22-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.2, 07-NOV-2003 (EDW)
      !>
      !>        Corrected a mistake in the second example's value
      !>        for VOUT, i.e. replaced (1D24, 2D23, 0.0) with
      !>        (-1D24, 0.0, 0.0).
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vsub ( v1, v2, vout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v1   ( 3 )  !! First vector (minuend).
      real(wp), intent(in) :: v2   ( 3 )  !! Second vector (subtrahend).
      real(wp), intent(out) :: vout ( 3 )  !! Difference vector, V1 - V2.

      vout = v1 - v2
      end subroutine vsub

      !> Compute the difference between two double precision vectors of
      !> arbitrary dimension.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (array expressions instead).
      !>
      !>## Detailed Input
      !>
      !>     V1       is a double precision vector of arbitrary dimension which
      !>              is the minuend (i.e. first or left-hand member) in the
      !>              vector subtraction.
      !>
      !>     V2       is a double precision vector of arbitrary dimension which
      !>              is the subtrahend (i.e. second or right-hand member) in
      !>              the vector subtraction.
      !>
      !>     NDIM     is the dimension of V1 and V2 (and VOUT).
      !>
      !>## Detailed Output
      !>
      !>     VOUT     is a double precision n-dimensional vector which
      !>              represents the vector difference, V1 - V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For each value of the index I from 1 to NDIM, this routine
      !>     performs the following subtraction:
      !>
      !>        VOUT(I) = V1(I) - V2(I)
      !>
      !>     No error checking is performed to guard against numeric overflow
      !>     or underflow.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Define two sets of n-dimensional vectors and compute the
      !>        difference from each vector in first set with the
      !>        corresponding vector in the second set.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VSUBG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
      !>      DOUBLE PRECISION      VOUT ( NDIM         )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the two vector sets.
      !>C
      !>      DATA                  V1 /
      !>     .                      1.D0,  2.D0,  3.D0,  4.D0,
      !>     .                      1.D0,  2.D0,  3.D0,  4.D0,
      !>     .                      1.D0,  2.D0,  3.D0,  4.D0   /
      !>
      !>      DATA                  V2 /
      !>     .                      1.D0,  1.D0,  1.D0,  1.D0,
      !>     .                     -1.D0, -2.D0, -3.D0, -4.D0,
      !>     .                     -1.D0,  2.D0, -3.D0,  4.D0  /
      !>
      !>C
      !>C     Calculate the difference between each pair of vectors
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>         CALL VSUBG ( V1(1,I), V2(1,I), NDIM, VOUT )
      !>
      !>         WRITE(*,'(A,4F6.1)') 'First vector : ',
      !>     .                        ( V1(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4F6.1)') 'Second vector: ',
      !>     .                        ( V2(J,I), J=1,NDIM )
      !>         WRITE(*,'(A,4F6.1)') 'Difference   : ', VOUT
      !>         WRITE(*,*)
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        First vector :    1.0   2.0   3.0   4.0
      !>        Second vector:    1.0   1.0   1.0   1.0
      !>        Difference   :    0.0   1.0   2.0   3.0
      !>
      !>        First vector :    1.0   2.0   3.0   4.0
      !>        Second vector:   -1.0  -2.0  -3.0  -4.0
      !>        Difference   :    2.0   4.0   6.0   8.0
      !>
      !>        First vector :    1.0   2.0   3.0   4.0
      !>        Second vector:   -1.0   2.0  -3.0   4.0
      !>        Difference   :    2.0   0.0   6.0   0.0
      !>
      !>## Restrictions
      !>
      !>     1)  No error checking is performed to guard against numeric
      !>         overflow. The programmer is thus required to insure that the
      !>         values in V1 and V2 are reasonable and will not cause
      !>         overflow. No error recovery or reporting scheme is
      !>         incorporated in this routine.
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 09-MAY-1990 (HAN)
      !>
      !>        Several errors in the header documentation were corrected.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine vsubg ( v1, v2, ndim, vout )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: ndim  !! Dimension of V1, V2, and VOUT.
      real(wp), intent(in) :: v1   ( ndim )  !! First vector (minuend).
      real(wp), intent(in) :: v2   ( ndim )  !! Second vector (subtrahend).
      real(wp), intent(out) :: vout ( ndim )  !! Difference vector, V1 - V2.

      vout(1:ndim) = v1(1:ndim) - v2(1:ndim)
      end subroutine vsubg

      !> Multiply the transpose of a 3-dimensional column vector,
      !> a 3x3 matrix, and a 3-dimensional column vector.
      !>
      !> The function returns the result of multiplying the transpose of
      !> V1 by MATRIX by V2.
      !>
      !>## Detailed Input
      !>
      !>     V1       is any double precision 3-dimensional column vector.
      !>
      !>     MATRIX   is any double precision 3x3 matrix.
      !>
      !>     V2       is any double precision 3-dimensional column vector.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the double precision value of the equation
      !>
      !>          T
      !>        V1  *  MATRIX * V2
      !>
      !>     Notice that VTMV is actually the dot product of the vector
      !>     resulting from multiplying the transpose of V1 and MATRIX and the
      !>     vector V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine implements the following vector/matrix/vector
      !>     multiplication:
      !>
      !>                 T
      !>        VTMV = V1  * MATRIX * V2
      !>
      !>     V1 is a column vector which becomes a row vector when transposed.
      !>     V2 is a column vector.
      !>
      !>     No checking is performed to determine whether floating point
      !>     overflow has occurred.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the multiplication of the transpose of a 3-dimensional
      !>        column vector, a 3x3 matrix, and a second 3-dimensional column
      !>        vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VTMV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VTMV
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      MATRIX ( 3, 3 )
      !>      DOUBLE PRECISION      V1     (    3 )
      !>      DOUBLE PRECISION      V2     (    3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define V1, MATRIX and V2.
      !>C
      !>      DATA                  V1      /  2.D0,  4.D0, 6.D0  /
      !>      DATA                  MATRIX  /  0.D0, -1.D0, 0.D0,
      !>     .                                 1.D0,  0.D0, 0.D0,
      !>     .                                 0.D0,  0.D0, 1.D0  /
      !>      DATA                  V2      /  1.D0,  1.D0, 1.D0  /
      !>
      !>      WRITE(*,'(A)') 'V1:'
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(F6.1)') V1(I)
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'MATRIX:'
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(3F6.1)') ( MATRIX(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'V2:'
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(F6.1)') V2(I)
      !>
      !>      END DO
      !>
      !>C
      !>C     Compute the transpose of V1 times MATRIX times V2.
      !>C
      !>      WRITE(*,*)
      !>      WRITE(*,'(A,F6.1)') 'Transpose of V1 times MATRIX '
      !>     .                 // 'times V2:', VTMV ( V1, MATRIX, V2 )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        V1:
      !>           2.0
      !>           4.0
      !>           6.0
      !>
      !>        MATRIX:
      !>           0.0   1.0   0.0
      !>          -1.0   0.0   0.0
      !>           0.0   0.0   1.0
      !>
      !>        V2:
      !>           1.0
      !>           1.0
      !>           1.0
      !>
      !>        Transpose of V1 times MATRIX times V2:   4.0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header and code to comply with NAIF standard. Added
      !>        complete code example based on existing example.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function vtmv ( v1, matrix, v2 )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: v1     (   3 )  !! 3-dimensional double precision column vector.
      real(wp), intent(in) :: matrix ( 3,3 )  !! 3x3 double precision matrix.
      real(wp), intent(in) :: v2     (   3 )  !! 3-dimensional double precision column vector.

      integer(ip) :: k
      integer(ip) :: l

      vtmv = 0.0_wp
      do k=1,3
         do l=1,3
            vtmv = vtmv + v1(k) * matrix(k,l) * v2(l)
         end do
      end do

      return
      end function vtmv

      !> Multiply the transpose of a n-dimensional column vector,
      !> a nxm matrix, and a m-dimensional column vector.
      !>
      !> The function returns the result of (V1**T * MATRIX * V2 ).
      !>
      !>## Detailed Input
      !>
      !>     V1       is an n-dimensional double precision vector.
      !>
      !>     MATRIX   is an n x m double precision matrix.
      !>
      !>     V2       is an m-dimensional double precision vector.
      !>
      !>     NROW     is the number of rows in MATRIX. This is also
      !>              equivalent to the number of rows in the vector V1.
      !>
      !>     NCOL     is the number of columns in MATRIX. This is also
      !>              equivalent to the number of rows in the vector V2.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the double precision value of the equation
      !>     (V1**T * MATRIX * V2 ).
      !>
      !>     Notice that VTMVG is actually the dot product of the vector
      !>     resulting from multiplying the transpose of V1 and MATRIX and the
      !>     vector V2.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine implements the following vector/matrix/vector
      !>     multiplication:
      !>
      !>                      T    |          |  |  |
      !>        VTMVG = [   V1   ] |  MATRIX  |  |V2|
      !>                           |          |  |  |
      !>
      !>     by calculating over all values of the indices K and L from 1 to
      !>     NROW and 1 to NCOL, respectively, the expression
      !>
      !>        VTMVG = Summation of ( V1(K)*MATRIX(K,L)*V2(L) ) .
      !>
      !>     V1 is a column vector which becomes a row vector when transposed.
      !>     V2 is a column vector.
      !>
      !>     No checking is performed to determine whether floating point
      !>     overflow has occurred.
      !>
      !>## Examples
      !>
      !>     If  V1 = | 1.0D0 |  MATRIX = | 2.0D0  0.0D0 |  V2 = | 1.0D0 |
      !>              |       |           |              |       |       |
      !>              | 2.0D0 |           | 1.0D0  2.0D0 |       | 2.0D0 |
      !>              |       |           |              |
      !>              | 3.0D0 |           | 1.0D0  1.0D0 |
      !>
      !>     NROW = 3
      !>     NCOL = 2
      !>
      !>     then the value of the function is  21.0D0.
      !>
      !>## Restrictions
      !>
      !>     1)  Since no error detection or recovery is implemented, the
      !>         programmer is required to insure that the inputs to this
      !>         routine are both valid and within the proper range.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added comments
      !>        to the code and moved the declaration of each local variable to
      !>        a separate line.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure real(wp) function vtmvg ( v1, matrix, v2, nrow, ncol )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nrow  !! Number of rows in MATRIX (number of rows in V1.)
      integer(ip), intent(in) :: ncol  !! Number of columns in MATRIX (number of rows in V2.)
      real(wp), intent(in) :: v1      ( nrow       )  !! N-dimensional double precision column
                                                      !! vector.
      real(wp), intent(in) :: matrix  ( nrow, ncol )  !! NxM double precision matrix.
      real(wp), intent(in) :: v2      (       ncol )  !! M-dimensional double precision column
                                                      !! vector.

      integer(ip) :: k
      integer(ip) :: l

!
!  Perform the multiplication
!
      vtmvg = 0.0_wp
      do k = 1, nrow
         do l = 1, ncol
            vtmvg = vtmvg + v1(k)*matrix(k,l)*v2(l)
         end do
      end do

      return
      end function vtmvg

      !> Unpack three scalar components from a vector.
      !>
      !>## Detailed Input
      !>
      !>     V        is a double precision 3-dimensional vector.
      !>
      !>## Detailed Output
      !>
      !>     X,
      !>     Y,
      !>     Z        are the double precision scalar components of the
      !>              vector V. The following equalities hold:
      !>
      !>                 X = V(1)
      !>                 Y = V(2)
      !>                 Z = V(3)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Basically, this is just shorthand notation for the common
      !>     sequence
      !>
      !>        X = V(1)
      !>        Y = V(2)
      !>        Z = V(3)
      !>
      !>     The routine is useful largely for two reasons. First, it
      !>     reduces the chance that the programmer will make a "cut and
      !>     paste" mistake, like
      !>
      !>        X = V(1)
      !>        Y = V(1)
      !>        Z = V(1)
      !>
      !>     Second, it makes conversions between equivalent units simpler,
      !>     and clearer. For instance, the sequence
      !>
      !>        X = V(1) * RPD()
      !>        Y = V(2) * RPD()
      !>        Z = V(3) * RPD()
      !>
      !>     can be replaced by the (nearly) equivalent sequence
      !>
      !>        CALL VSCLIP ( RPD(),  V    )
      !>        CALL VUPACK ( V,   X, Y, Z )
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that you have an instrument kernel that provides,
      !>        within a single keyword, the three frequencies used by the
      !>        instrument, and that you want to use these frequencies
      !>        independently within your code.
      !>
      !>        The following code example demonstrates how to use VUPACK
      !>        to get these frequencies into independent scalar variables.
      !>
      !>        Use the kernel shown below, an IK defining the three
      !>        frequencies used by an instrument with NAIF ID -999001.
      !>
      !>           KPL/IK
      !>
      !>           File name: vupack_ex1.ti
      !>
      !>           The keyword below define the three frequencies used by a
      !>           hypothetical instrument (NAIF ID -999001). They correspond
      !>           to three filters: red, green and blue. Frequencies are
      !>           given in micrometers.
      !>
      !>           \begindata
      !>
      !>              INS-999001_FREQ_RGB   = (  0.65,  0.55, 0.475 )
      !>              INS-999001_FREQ_UNITS = ( 'MICROMETERS'       )
      !>
      !>           \begintext
      !>
      !>           End of IK
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VUPACK_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         IKNAME
      !>      PARAMETER           ( IKNAME = 'vupack_ex1.ti' )
      !>
      !>      CHARACTER*(*)         KEYWRD
      !>      PARAMETER           ( KEYWRD = 'INS-999001_FREQ_RGB' )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      DDATA  ( 3 )
      !>      DOUBLE PRECISION      RED
      !>      DOUBLE PRECISION      GREEN
      !>      DOUBLE PRECISION      BLUE
      !>
      !>      INTEGER               N
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load the instrument kernel.
      !>C
      !>      CALL FURNSH ( IKNAME )
      !>
      !>C
      !>C     Get the frequency data from the kernel pool.
      !>C
      !>      CALL GDPOOL ( KEYWRD, 1, 3, N, DDATA, FOUND )
      !>
      !>      IF ( FOUND ) THEN
      !>
      !>         CALL VUPACK ( DDATA, RED, GREEN, BLUE )
      !>         WRITE(*,'(A,F6.2)') 'Blue  (nm): ', BLUE  * 1000.D0
      !>         WRITE(*,'(A,F6.2)') 'Green (nm): ', GREEN * 1000.D0
      !>         WRITE(*,'(A,F6.2)') 'Red   (nm): ', RED   * 1000.D0
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) 'No data found in the kernel pool for ',
      !>     .              KEYWRD
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Blue  (nm): 475.00
      !>        Green (nm): 550.00
      !>        Red   (nm): 650.00
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 07-SEP-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Fixed order of operands in equalities presented in
      !>        $Detailed_Output. Updated code fragments in $Particulars to
      !>        use in-place vector-scaling API.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      pure subroutine vupack ( v, x, y, z )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v ( 3 )  !! Input 3-dimensional vector.
      real(wp), intent(out) :: x  !! Scalar components of the vector.
      real(wp), intent(out) :: y  !! Scalar components of the vector.
      real(wp), intent(out) :: z  !! Scalar components of the vector.

!
!     Just shorthand, like it says above.
!
      x = v(1)
      y = v(2)
      z = v(3)

      return
      end subroutine vupack

      !> Indicate whether a 3-vector is the zero vector.
      !>
      !> The function returns the value .TRUE. if and only if V is the
      !> zero vector.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (ALL instead).
      !>
      !>## Detailed Input
      !>
      !>     V        is a vector in 3-space.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value .TRUE. if and only if V is the
      !>     zero vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function has the same truth value as the logical expression
      !>
      !>        VNORM ( V )  .EQ.  0.D0
      !>
      !>     Replacing the above expression by
      !>
      !>        VZERO ( V )
      !>
      !>     has several advantages: the latter expresses the test more
      !>     clearly, looks better, and doesn't go through the work of scaling,
      !>     squaring, taking a square root, and re-scaling (all of which
      !>     VNORM must do) just to find out that a vector is non-zero.
      !>
      !>     A related function is VZEROG, which accepts vectors of arbitrary
      !>     dimension.
      !>
      !>## Examples
      !>
      !>     1)  When testing whether a vector is the zero vector, one
      !>         normally constructs tests like
      !>
      !>            IF (  VNORM ( V )  .EQ.  0.D0  ) THEN
      !>                        .
      !>                        .
      !>                        .
      !>
      !>         These can be replaced with the code
      !>
      !>            IF (  VZERO ( V )  ) THEN
      !>                        .
      !>                        .
      !>                        .
      !>
      !>      2)  Check that a normal vector is non-zero before creating
      !>         a plane with PNV2PL:
      !>
      !>         IF (  VZERO ( NORMAL )  )  THEN
      !>
      !>            [ handle error ]
      !>
      !>         ELSE
      !>
      !>            CALL PNV2PL ( POINT, NORMAL, PLANE )
      !>                          .
      !>                          .
      !>                          .
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 17-JUL-1990 (NJB) (IMU)
      pure logical function vzero ( v )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: v ( 3 )  !! Vector to be tested.

!
!     `Just do it'.
!
      vzero = all(v == 0.0_wp)
      end function vzero

      !> Indicate whether an n-dimensional vector is the zero vector.
      !>
      !> The function returns the value .TRUE. if and only if V is the
      !> zero vector.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (ALL instead).
      !>
      !>## Detailed Input
      !>
      !>     V,
      !>     NDIM     are, respectively, an n-dimensional vector and its
      !>              dimension.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the value .TRUE. if and only if V is the
      !>     zero vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  When NDIM is non-positive, this function returns the value
      !>         .FALSE. (A vector of non-positive dimension cannot be the
      !>         zero vector.)
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function has the same truth value as the logical expression
      !>
      !>        VNORMG ( V, NDIM )  .EQ.  0.D0
      !>
      !>     Replacing the above expression by
      !>
      !>        VZEROG ( V, NDIM )
      !>
      !>     has several advantages: the latter expresses the test more
      !>     clearly, looks better, and doesn't go through the work of scaling,
      !>     squaring, taking a square root, and re-scaling (all of which
      !>     VNORMG must do) just to find out that a vector is non-zero.
      !>
      !>     A related function is VZERO, which accepts three-dimensional
      !>     vectors.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a set of n-dimensional vectors, check which ones are
      !>        the zero vector.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VZEROG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      LOGICAL               VZEROG
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NDIM
      !>      PARAMETER           ( NDIM   = 4 )
      !>
      !>      INTEGER               SETSIZ
      !>      PARAMETER           ( SETSIZ = 2 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      V    ( NDIM, SETSIZ )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the vector set.
      !>C
      !>      DATA                  V   /
      !>     .                          0.D0,  0.D0,  0.D0,  2.D-7,
      !>     .                          0.D0,  0.D0,  0.D0,  0.D0  /
      !>
      !>C
      !>C     Check each n-dimensional vector within the set.
      !>C
      !>      DO I=1, SETSIZ
      !>
      !>C
      !>C        Check if the I'th vector is the zero vector.
      !>C
      !>         WRITE(*,*)
      !>         WRITE(*,'(A,4F11.7)') 'Input vector: ',
      !>     .                         ( V(J,I), J=1,NDIM )
      !>
      !>         IF ( VZEROG ( V(1,I), NDIM ) ) THEN
      !>
      !>            WRITE(*,'(A)') '   The zero vector.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A)') '   Not all elements of the '
      !>     .                  // 'vector are zero.'
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input vector:   0.0000000  0.0000000  0.0000000  0.0000002
      !>           Not all elements of the vector are zero.
      !>
      !>        Input vector:   0.0000000  0.0000000  0.0000000  0.0000000
      !>           The zero vector.
      !>
      !>     2) Define a unit quaternion and confirm that it is non-zero
      !>        before converting it to a rotation matrix.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM VZEROG_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      VNORMG
      !>      LOGICAL               VZEROG
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      Q    ( 0 : 3 )
      !>      DOUBLE PRECISION      M    ( 3,  3 )
      !>      DOUBLE PRECISION      S
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define a unit quaternion.
      !>C
      !>      S = SQRT( 2.D0 ) / 2.D0
      !>
      !>      Q(0) = S
      !>      Q(1) = 0.D0
      !>      Q(2) = 0.D0
      !>      Q(3) = -S
      !>
      !>      WRITE(*,'(A,4F12.7)') 'Quaternion :', Q
      !>
      !>C
      !>C     Confirm that it is non-zero and
      !>C
      !>      IF ( VZEROG ( Q, 4 ) ) THEN
      !>
      !>         WRITE(*,*) '   Quaternion is the zero vector.'
      !>
      !>      ELSE
      !>
      !>C
      !>C        Confirm q satisfies ||Q|| = 1.
      !>C
      !>         WRITE(*,'(A,F12.7)') 'Norm       :', VNORMG ( Q, 4 )
      !>
      !>C
      !>C        Convert the quaternion to a matrix form.
      !>C
      !>         CALL Q2M ( Q, M )
      !>
      !>         WRITE(*,'(A)') 'Matrix form:'
      !>         DO I = 1, 3
      !>
      !>            WRITE(*,'(3F12.7)') ( M(I,J), J=1,3 )
      !>
      !>         END DO
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Quaternion :   0.7071068   0.0000000   0.0000000  -0.7071068
      !>        Norm       :   1.0000000
      !>        Matrix form:
      !>           0.0000000   1.0000000   0.0000000
      !>          -1.0000000   0.0000000  -0.0000000
      !>          -0.0000000   0.0000000   1.0000000
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code examples.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 18-JUL-1990 (NJB) (IMU)
      pure logical function vzerog ( v, ndim )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: v    ( * )  !! Vector to be tested.
      integer(ip), intent(in) :: ndim  !! Dimension of V.

!
!     Leave as soon as we find a non-zero component.  If we get through
!     the loop, we have a zero vector, as long as the vector's dimension
!     is valid.
!
      vzerog = ndim >= 1 .and. all(v(1:ndim) == 0.0_wp)
      end function vzerog

      !> Determine the rotation matrix and angular velocity of the
      !> rotation from a state transformation matrix.
      !>
      !>## Detailed Input
      !>
      !>     XFORM    is a state transformation matrix from one frame
      !>              FRAME1 to some other frame FRAME2.
      !>
      !>## Detailed Output
      !>
      !>     ROT      is a rotation that gives the transformation from
      !>              some frame FRAME1 to another frame FRAME2.
      !>
      !>     AV       is the angular velocity of the transformation.
      !>              In other words, if P is the position of a fixed
      !>              point in FRAME2, then from the point of view of
      !>              FRAME1, P rotates (in a right handed sense) about
      !>              an axis parallel to AV. Moreover the rate of rotation
      !>              in radians per unit time is given by the length of
      !>              AV.
      !>
      !>              More formally, the velocity V of P in FRAME1 is
      !>              given by
      !>                                 T
      !>                  V  = AV x ( ROT  * P )
      !>
      !>              The components of AV are given relative to FRAME1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No checks are performed on XFORM to ensure that it is indeed
      !>         a state transformation matrix.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is essentially a macro routine for converting
      !>     state transformation matrices into the equivalent representation
      !>     in terms of a rotation and angular velocity.
      !>
      !>     This routine is an inverse of the routine RAV2XF.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that you wanted to determine the angular velocity
      !>        of the Earth body-fixed reference frame with respect to
      !>        J2000 at a particular epoch ET. The following code example
      !>        illustrates a procedure for computing the angular velocity.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: xf2rav_ex1.tm
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
      !>              earth_720101_070426.bpc       Earth historical
      !>                                            binary PCK
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'earth_720101_070426.bpc',
      !>                                  'naif0012.tls'            )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM XF2RAV_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         META
      !>      PARAMETER           ( META   = 'xf2rav_ex1.tm' )
      !>
      !>      CHARACTER*(*)         UTCSTR
      !>      PARAMETER           ( UTCSTR = '2005-OCT-10 16:00:00' )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AV     ( 3    )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      FTMTRX ( 6, 6 )
      !>      DOUBLE PRECISION      ROT    ( 3, 3 )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Load SPICE kernels.
      !>C
      !>      CALL FURNSH ( META )
      !>
      !>C
      !>C     Convert the input time to seconds past J2000 TDB.
      !>C
      !>      CALL STR2ET ( UTCSTR, ET )
      !>
      !>C
      !>C     Get the transformation matrix from J2000 frame to
      !>C     ITRF93.
      !>C
      !>      CALL SXFORM ( 'J2000', 'ITRF93', ET, FTMTRX )
      !>
      !>C
      !>C     Now get the angular velocity by calling XF2RAV
      !>C
      !>      CALL XF2RAV ( FTMTRX, ROT, AV )
      !>
      !>C
      !>C      Display the results.
      !>C
      !>      WRITE(*,'(A)') 'Rotation matrix:'
      !>      DO I = 1, 3
      !>
      !>         WRITE(*,'(3F16.11)') ( ROT(I,J), J=1,3 )
      !>
      !>      END DO
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Angular velocity:'
      !>      WRITE(*,'(3F16.11)') AV
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Rotation matrix:
      !>          -0.18603277688  -0.98254352801   0.00014659080
      !>           0.98254338275  -0.18603282936  -0.00053610915
      !>           0.00055402128   0.00004429795   0.99999984555
      !>
      !>        Angular velocity:
      !>           0.00000004025   0.00000000324   0.00007292114
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
      !>-    SPICELIB Version 1.1.1, 19-MAY-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>        Added ROTATION to the required readings.
      !>
      !>-    SPICELIB Version 1.1.0, 28-JUL-1997 (WLT)
      !>
      !>        The example in version 1.0.0 was incorrect. The example
      !>        in version 1.1.0 fixes the previous problem.
      !>
      !>-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
      pure subroutine xf2rav ( xform, rot, av )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: xform  ( 6, 6 )  !! is a state transformation matrix.
      real(wp), intent(out) :: rot    ( 3, 3 )  !! is the rotation associated with XFORM.
      real(wp), intent(out) :: av     ( 3    )  !! is the angular velocity associated with XFORM.

      integer(ip) :: i
      integer(ip) :: j

      real(wp) :: drdt  ( 3, 3 )
      real(wp) :: omega ( 3, 3 )

!
!     A state transformation matrix XFORM has the following form
!
!         [      |     ]
!         |  R   |  0  |
!         |      |     |
!         | -----+-----|
!         |  dR  |     |
!         |  --  |  R  |
!         [  dt  |     ]
!
!     where R is a rotation and dR/dt is the time derivative of that
!     rotation.  From this we can immediately read the rotation and
!     its derivative.
!
      do i = 1, 3
         do j = 1,3
            rot (i,j) = xform(i,  j)
            drdt(i,j) = xform(i+3,j)
         end do
      end do

!
!     Recall that ROT is a transformation that converts positions
!     in some frame FRAME1 to positions in a second frame FRAME2.
!
!     The angular velocity matrix OMEGA (the cross product matrix
!     corresponding to AV) has the following property.
!
!     If P is the position of an object that is stationary with
!     respect to FRAME2 then the velocity V of that object in FRAME1
!     is given by:
!                          t
!         V  =  OMEGA * ROT  *  P
!
!     But V is also given by
!
!                    t
!               d ROT
!         V =   -----  * P
!                 dt
!
!     So that
!                                  t
!                    t        d ROT
!         OMEGA * ROT    =   -------
!                               dt
!
!     Hence
!                             t
!                       d ROT
!         OMEGA    =   -------  *  ROT
!                         dt
!
      call mtxm ( drdt, rot, omega )

!
!     Recall that OMEGA has the form
!
!         _                     _
!        |                       |
!        |   0    -AV(3)  AV(2)  |
!        |                       |
!        |  AV(3)    0   -AV(1)  |
!        |                       |
!        | -AV(2)   AV(1)   0    |
!        |_                     _|
!
      av(1) = omega(3,2)
      av(2) = omega(1,3)
      av(3) = omega(2,1)

      return
      end subroutine xf2rav

      !> Transpose a 3x3 matrix.
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (TRANSPOSE instead).
      !>
      !>## Detailed Input
      !>
      !>     M1       is any double precision 3x3 matrix.
      !>
      !>## Detailed Output
      !>
      !>     MOUT     is a double precision, 3x3 matrix which contains the
      !>              transpose of M1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     XPOSE first copies the diagonal elements of M1 to MOUT. Then
      !>     the off-diagonal elements are transposed using a temporary
      !>     variable in the following order:
      !>
      !>        (1,2) <---> (2,1)
      !>        (1,3) <---> (3,1)
      !>        (2,3) <---> (3,2)
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a 3x3 double precision matrix, find its transpose.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM XPOSE_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      M1     (3,3)
      !>      DOUBLE PRECISION      MOUT   (3,3)
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>
      !>C
      !>C     Define the input matrix.
      !>C
      !>      DATA                  M1     /  1.0D0,  0.0D0,  0.0D0,
      !>     .                                2.0D0,  4.0D0,  6.0D0,
      !>     .                                3.0D0,  5.0D0,  0.0D0 /
      !>
      !>C
      !>C     Compute the transpose of M1.
      !>C
      !>      CALL XPOSE ( M1, MOUT )
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,*) 'Input matrix (M1):'
      !>      WRITE(*,*)
      !>      DO I= 1, 3
      !>         WRITE(*,'(3F6.1)') ( M1(I,J), J= 1, 3 )
      !>      END DO
      !>      WRITE(*,*)
      !>      WRITE(*,*) 'Transpose of M1:'
      !>      WRITE(*,*)
      !>      DO I= 1, 3
      !>         WRITE(*,'(3F6.1)') ( MOUT(I,J), J= 1, 3 )
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Input matrix (M1):
      !>
      !>           1.0   2.0   3.0
      !>           0.0   4.0   5.0
      !>           0.0   6.0   0.0
      !>
      !>         Transpose of M1:
      !>
      !>           1.0   0.0   0.0
      !>           2.0   4.0   6.0
      !>           3.0   5.0   0.0
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
      !>     W.M. Owen          (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
      pure subroutine xpose ( m1, mout )
      use pepper_kinds, only: wp
      implicit none

      real(wp), intent(in) :: m1   ( 3,3 )  !! 3x3 matrix to be transposed.
      real(wp), intent(out) :: mout ( 3,3 )  !! Transpose of M1.

!
!  Move the three diagonal elements from M1 to MOUT
!
      mout = transpose(m1)
      end subroutine xpose

      !> Transpose a matrix of arbitrary size (the matrix
      !> need not be square).
      !>
      !> PEPPER: kept for backward compatibility (SPICELIB's interface); the
      !> library does not use it (TRANSPOSE and RESHAPE instead).
      !>
      !>## Detailed Input
      !>
      !>     MATRIX   is a matrix to be transposed.
      !>
      !>     NROW     is the number of rows of input matrix MATRIX.
      !>
      !>     NCOL     is the number of columns of input matrix MATRIX.
      !>
      !>## Detailed Output
      !>
      !>     XPOSEM   is the transposed matrix.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If either NROW or NCOL is less than or equal to zero, no
      !>         action is taken. The routine simply returns.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine transposes the input matrix MATRIX and writes the
      !>     result to the matrix XPOSEM. This algorithm is performed in
      !>     such a way that the transpose can be performed in place. That
      !>     is, MATRIX and XPOSEM can use the same storage area in memory.
      !>
      !>     NOTE:  The matrices MATRIX and XPOSEM are declared
      !>            one-dimensional for computational purposes only. The
      !>            calling program should declare them as MATRIX(NROW,NCOL)
      !>            and XPOSEM(NCOL,NROW).
      !>
      !>            This routine works on the assumption that the input and
      !>            output matrices are defined as described above. More
      !>            specifically it assures that the elements of the matrix
      !>            to be transformed are stored in contiguous memory locations
      !>            as shown here. On output these elements will be
      !>            rearranged in consecutive memory locations as shown.
      !>
      !>               MATRIX              XPOSEM
      !>
      !>                m_11                m_11
      !>                m_21                m_12
      !>                m_31                m_13
      !>                 .                   .
      !>                 .                   .
      !>                 .                  m_1ncol
      !>                m_nrow1             m_21
      !>                m_12                m_22
      !>                m_22                m_23
      !>                m_32                 .
      !>                 .                   .
      !>                 .                  m_2ncol
      !>                 .                   .
      !>                m_nrow2
      !>                 .                   .
      !>
      !>                 .                   .
      !>
      !>                 .                   .
      !>                m_1ncol
      !>                m_2ncol             m_nrow1
      !>                m_3ncol             m_nrow2
      !>                 .                  m_nrow3
      !>                 .                   .
      !>                 .                   .
      !>                m_nrowncol          m_nrowncol
      !>
      !>     For those familiar with permutations, this algorithm relies
      !>     upon the fact that the transposition of a matrix, which has
      !>     been stored as a string, is simply the action of a
      !>     permutation applied to that string. Since any permutation
      !>     can be decomposed as a product of disjoint cycles, it is
      !>     possible to transpose the matrix with only one additional
      !>     storage register. However, once a cycle has been computed
      !>     it is necessary to find the next entry in the string that
      !>     has not been moved by the permutation. For this reason the
      !>     algorithm is slower than would be necessary if the numbers
      !>     of rows and columns were known in advance.
      !>
      !>## Examples
      !>
      !>     This routine is primarily useful when attempting to transpose
      !>     large matrices, where inplace transposition is important. For
      !>     example suppose you have the following declarations
      !>
      !>         DOUBLE PRECISION      MATRIX ( 1003, 800  )
      !>
      !>     If the transpose of the matrix is needed, it may not be
      !>     possible to fit a second matrix requiring the same storage
      !>     into memory. Instead declare XPOSEM as below and use
      !>     an equivalence so that the same area of memory is allocated.
      !>
      !>         DOUBLE PRECISION      XPOSEM (  800, 1003 )
      !>         EQUIVALENCE         ( MATRIX (1,1), XPOSEM(1,1) )
      !>
      !>     To obtain the transpose simply execute
      !>
      !>         CALL XPOSEG ( MATRIX, 1003, 800, XPOSEM )
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
      !>-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.2.3, 22-APR-2010 (NJB)
      !>
      !>        Header correction: assertions that the output
      !>        can overwrite the input have been removed.
      !>
      !>-    SPICELIB Version 1.2.2, 04-MAY-1993 (HAN)
      !>
      !>        The example listed arguments in the call to XPOSEG incorrectly.
      !>        The number of rows was listed as the number of columns, and
      !>        the number of columns was listed as the number of rows.
      !>
      !>-    SPICELIB Version 1.2.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.2.0, 06-AUG-1990 (WLT)
      !>
      !>        The original version of the routine had a bug. It worked
      !>        in place, but the fixed points (1,1) and (n,m) were not
      !>        moved so that the routine did not work if input and output
      !>        matrices were different.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB)
      pure subroutine xposeg ( matrix, nrow, ncol, xposem )
      use pepper_kinds, only: ip, wp
      implicit none

      real(wp), intent(in) :: matrix ( 0:* )  !! Matrix to be transposed.
      integer(ip), intent(in) :: nrow  !! Number of rows of input matrix MATRIX.
      integer(ip), intent(in) :: ncol  !! Number of columns of input matrix MATRIX.
      real(wp), intent(out) :: xposem ( 0:* )  !! Transposed matrix.

!
!     Take care of dumb cases first.
!
      if ( nrow <= 0 .or. ncol <= 0 ) return
      xposem(0:nrow*ncol-1) = reshape(transpose(reshape(matrix(0:nrow*ncol-1), [nrow, ncol])), [nrow*ncol])
      end subroutine xposeg

      !> Transpose a matrix of arbitrary size and shape in place.
      !>
      !>## Detailed Input
      !>
      !>     NROW     is the number of rows of input matrix MATRIX.
      !>
      !>     NCOL     is the number of columns of input matrix MATRIX.
      !>
      !>     MATRIX   is a matrix to be transposed.
      !>
      !>## Detailed Output
      !>
      !>     MATRIX   is the transposed matrix: element (i,j) of the input
      !>              matrix is element (j,i) of the output matrix.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If either NROW or NCOL is less than or equal to zero, no
      !>         action is taken. The routine simply returns.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine replaces the input matrix MATRIX with its transpose.
      !>
      !>     NOTE:  The matrix MATRIX is declared one-dimensional for
      !>            computational purposes only. The calling program may
      !>            declare it as MATRIX(NROW,NCOL) or MATRIX(NCOL,NROW).
      !>
      !>            This routine assumes that the elements of the matrix to be
      !>            transformed are stored in contiguous memory locations as
      !>            shown here. On output these elements will be rearranged
      !>            in consecutive memory locations as shown.
      !>
      !>               MATRIX on input      MATRIX on output
      !>
      !>                m_11                m_11
      !>                m_21                m_12
      !>                m_31                m_13
      !>                 .                   .
      !>                 .                   .
      !>                 .                  m_1ncol
      !>                m_nrow1             m_21
      !>                m_12                m_22
      !>                m_22                m_23
      !>                m_32                 .
      !>                 .                   .
      !>                 .                  m_2ncol
      !>                 .                   .
      !>                m_nrow2
      !>                 .                   .
      !>
      !>                 .                   .
      !>
      !>                 .                   .
      !>                m_1ncol
      !>                m_2ncol             m_nrow1
      !>                m_3ncol             m_nrow2
      !>                 .                  m_nrow3
      !>                 .                   .
      !>                 .                   .
      !>                m_nrowncol          m_nrowncol
      !>
      !>     For those familiar with permutations, this algorithm relies
      !>     upon the fact that the transposition of a matrix, which has
      !>     been stored as a 1-dimensional array, is simply the action of a
      !>     permutation applied to that array. Since any permutation
      !>     can be decomposed as a product of disjoint cycles, it is
      !>     possible to transpose the matrix with only one additional
      !>     storage register. However, once a cycle has been computed
      !>     it is necessary to find the next entry in the array that
      !>     has not been moved by the permutation. For this reason the
      !>     algorithm is slower than would be necessary if the numbers
      !>     of rows and columns were known in advance.
      !>
      !>## Examples
      !>
      !>     This routine is provided for situation where it is convenient to
      !>     transpose a general two-dimensional matrix
      !>     in place rather than store the result in a
      !>     separate array. Note that the call
      !>
      !>        CALL XPOSEG ( MATRIX, NROW, NCOL, MATRIX )
      !>
      !>     is not permitted by the ANSI Fortran 77 standard; this routine
      !>     can be called instead to achieve the same result.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-SEP-2006 (EDW)
      !>
      !>        Initial version date unknown. Version data entry
      !>        added this date.
      pure subroutine xpsgip ( nrow, ncol, matrix )
      use pepper_kinds, only: ip, wp
      implicit none

      integer(ip), intent(in) :: nrow  !! Number of rows of input matrix.
      integer(ip), intent(in) :: ncol  !! Number of columns of input matrix.
      real(wp), intent(inout) :: matrix ( 0 : * )  !! Matrix to be transposed/transposed matrix.

      real(wp) :: source
      real(wp) :: temp

      integer(ip) :: dest
      integer(ip) :: k
      integer(ip) :: m
      integer(ip) :: moved
      integer(ip) :: n
      integer(ip) :: nmoves
      integer(ip) :: r
      integer(ip) :: start

!
!     Take care of dumb cases first.
!
      if ( ( nrow <= 0 ) .or. ( ncol <= 0 ) ) then
         return
      end if

!
!     Use the abbreviations N and M for NROW and NCOL.
!
      n      = nrow
      m      = ncol

!
!     Set up the upper bound for the number of objects to be moved and
!     initialize the counters.
!
      nmoves = n*m - 2
      moved  = 0
      start  = 1

!
!     Until MOVED is equal to NMOVES, there is some matrix element that
!     has not been moved to its proper location in the transpose matrix.
!
      do while ( moved < nmoves )

         source  =  matrix( start )
         k       =  start / n
         r       =  start - n*k
         dest    =  r*m   + k

!
!        Perform this cycle of the permutation.  We will be done when
!        the destination of the next element is equal to the starting
!        position of the first element to be moved in this cycle.
!
         do while ( dest /= start )

            temp           = matrix( dest )
            matrix( dest ) = source
            source         = temp

            moved          = moved + 1
            k              = dest  / n
            r              = dest  - k*n
            dest           = m*r   + k

         end do

         matrix( dest ) = source
         dest           = 0
         moved          = moved + 1

!
!        Find the next element of the matrix that has not already been
!        moved by the transposition operation.
!
         if ( moved < nmoves ) then

            do while ( dest /= start )

               start = start + 1
               k     = start / n
               r     = start - k*n
               dest  = r*m   + k

               do while ( dest > start )

                  k     = dest / n
                  r     = dest - k*n
                  dest  = m*r  + k

               end do

            end do

         end if

      end do

      return
      end subroutine xpsgip

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     This routine multiplies together a sequence of state
      !>     transformation matrices.
      !>
      !>## Detailed Input
      !>
      !>     MATRIX      is an array of 6x6 state transformation matrices.
      !>                 It is essential that all these matrices have the form
      !>
      !>                    -            -
      !>                   |      |       |
      !>                   |   R  |   0   |
      !>                   |      |       |
      !>                   | -----+------ |
      !>                   |      |       |
      !>                   |   D  |   R   |
      !>                   |      |       |
      !>                    -            -
      !>
      !>                 The routine does not compute the product of a sequence
      !>                 that does not satisfy this condition.
      !>
      !>     N           is an integer giving the number of matrices in the
      !>                 sequence.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT      is the product of the matrices stored in MATRIX.
      !>                 Specifically, it is the result of the product
      !>
      !>                       M_N * M_(N-1) * ... * M_2 * M_1
      !>
      !>                 where the K'th matrix M_K is define by the
      !>                 relationship
      !>
      !>                    M_K( I, J )  = MATRIX ( I, J, K )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1) If N is 0 or smaller OUTPUT will be returned as the
      !>        6x6 identity matrix.
      !>
      !>     2) IF N is 1 OUTPUT will be returned as M_1 where M_1 is
      !>        the matrix defined above in the description of OUTPUT.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a private SPICE routine that computes the product
      !>     of a sequence of state transformation matrices.
      !>
      !>     This routine takes special advantage of the structure of
      !>     state transformation matrices so that the number of
      !>     actual multiplies and additions is reduced to 3/8 of that
      !>     which would be needed by a general matrix multiplication
      !>     routine.
      !>
      !>     The key to this computation saving is the structure of the
      !>     state transformation matrix.  Suppose that M2 and M1 are
      !>     two such matrices.  Then the product
      !>
      !>         -            -    -            -
      !>        |      |       |  |      |       |
      !>        |   R2 |   0   |  |   R1 |   0   |
      !>        |      |       |  |      |       |
      !>        | -----+------ |  | -----+------ |  =
      !>        |      |       |  |      |       |
      !>        |   D2 |   R2  |  |   D1 |   R1  |
      !>        |      |       |  |      |       |
      !>         -            -    -            -
      !>
      !>         -                              -
      !>        |                  |             |
      !>        |   R2*R1          |     0       |
      !>        |                  |             |
      !>        | -----------------+------------ |
      !>        |                  |             |
      !>        |   D2*R1 + R2*D1  |   R2*R1     |
      !>        |                  |             |
      !>         -                              -
      !>
      !>     As can be seen this can be computed with 3 3x3 matrix multiplies
      !>     and one 3x3 matrix addition.
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Reordered header sections. Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
      pure subroutine zzmsxf ( matrix, n, output )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp), intent(in) :: matrix ( 6, 6, * )  !! A sequence of state transformation matrices
      integer(ip), intent(in) :: n  !! The number of state transformation matrices
      real(wp), intent(out) :: output ( 6, 6    )  !! The product of the state transformations.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: l
      integer(ip) :: m

      integer(ip) :: get
      integer(ip) :: put
      integer(ip) :: incr

      real(wp) :: temp ( 6, 6, 2 )

!
!    If we have more than 2 matrices to deal with we will need to
!    set up the PUT location
!
      put = 1

!
!     We perform tests in the order they seem most likely to
!     occur.
!
      if ( n == 2 ) then
!
!        If there are exactly two inputs, then the output takes
!        only a single matrix multiply.
!
         do j = 1,3
            do k = 1,3

               output(j,k) = matrix(j,1,2)*matrix(1,k,1) &
                           + matrix(j,2,2)*matrix(2,k,1) &
                           + matrix(j,3,2)*matrix(3,k,1)
            end do
         end do

         do j = 4,6
            do k = 1,3

               output(j,k) = matrix(j,1,2)*matrix(1,k,1) &
                           + matrix(j,2,2)*matrix(2,k,1) &
                           + matrix(j,3,2)*matrix(3,k,1) &
                           + matrix(j,4,2)*matrix(4,k,1) &
                           + matrix(j,5,2)*matrix(5,k,1) &
                           + matrix(j,6,2)*matrix(6,k,1)
            end do
         end do

         do j = 1,3
            do k = 4,6
               output(j,k) = 0.0_wp
            end do
         end do

         do j = 4,6
            l = j - 3
            do k = 4,6
               m = k - 3
               output(j,k) = output(l,m)
            end do
         end do

      else if ( n > 2 ) then

!
!        We need to compute the product
!
!           MATRIX( , ,N) * MATRIX( , ,N-1) * ... * MATRIX( , , 1 )
!
!        Compute the first product.  MATRIX( , ,2) * MATRIX( , ,1)
!
!        First compute the upper left hand 3x3 portion of the product...
!
         do j = 1,3
            do k = 1,3

               temp(j,k,put) =  matrix(j,1,2)*matrix(1,k,1) &
                              + matrix(j,2,2)*matrix(2,k,1) &
                              + matrix(j,3,2)*matrix(3,k,1)
            end do
         end do

!
!        Next compute the lower left hand 3x3 portion of the product.
!
         do j = 4,6
            do k = 1,3
               temp(j,k,put) =  matrix(j,1,2)*matrix(1,k,1) &
                              + matrix(j,2,2)*matrix(2,k,1) &
                              + matrix(j,3,2)*matrix(3,k,1) &
                              + matrix(j,4,2)*matrix(4,k,1) &
                              + matrix(j,5,2)*matrix(5,k,1) &
                              + matrix(j,6,2)*matrix(6,k,1)
            end do
         end do
!
!        We don't bother to compute the upper right hand 3x3 portion
!        of the matrix since it is always zero.
!
!        Finally we could copy the lower right hand 3x3 portion of the
!        product from the upper left hand portion.  But as you can
!        see below we never actually have to reference TEMP(I,K,GET)
!        for K = 4 to 6.  So we can just skip that part of the
!        computation.
!
!        Now continue building the product.  Note we will toggle
!        back and forth from TEMP(,,1) to TEMP(,,2) for storing
!        (PUTting) the results of our computations.  This way we
!        don't have to spend time moving any of the our computation
!        results to get ready for the next product.  See the end
!        of the loop below (keeping mind the next three values) to
!        see the little trick that's used to toggle back and forth.
!
         incr = -1
         put  =  2
         get  =  1

         do i = 3, n-1
!
!           First the upper left hand portion of the product.
!
            do j = 1,3
               do k = 1,3

                  temp(j,k,put) =  matrix(j,1,i)*temp(1,k,get) &
                                 + matrix(j,2,i)*temp(2,k,get) &
                                 + matrix(j,3,i)*temp(3,k,get)

               end do
            end do
!
!           Next the lower left hand portion of the product.
!
            do j = 4,6
               do k = 1,3

                  temp(j,k,put) = matrix(j,1,i)*temp(1,k,get) &
                                + matrix(j,2,i)*temp(2,k,get) &
                                + matrix(j,3,i)*temp(3,k,get) &
                                + matrix(j,4,i)*temp(4,k,get) &
                                + matrix(j,5,i)*temp(5,k,get) &
                                + matrix(j,6,i)*temp(6,k,get)

               end do
            end do
!
!           And as before, we don't need to compute the upper right
!           or lower right hand 3x3 portions of the matrix. So
!           we just skip them.  Toggle GET and PUT so we will
!           be ready for the next pass.
!
            get  =  put
            put  =  put + incr
            incr =      - incr

         end do

!
!        Finally compute the last product.  First the upper
!        left hand portion of the product.
!
         do j = 1,3
            do k = 1,3

               output(j,k) = matrix(j,1,n)*temp(1,k,get) &
                           + matrix(j,2,n)*temp(2,k,get) &
                           + matrix(j,3,n)*temp(3,k,get)
            end do
         end do
!
!        The lower left hand portion of the product.
!
         do j = 4,6
            do k = 1,3

               output(j,k) = matrix(j,1,n)*temp(1,k,get) &
                           + matrix(j,2,n)*temp(2,k,get) &
                           + matrix(j,3,n)*temp(3,k,get) &
                           + matrix(j,4,n)*temp(4,k,get) &
                           + matrix(j,5,n)*temp(5,k,get) &
                           + matrix(j,6,n)*temp(6,k,get)
            end do
         end do
!
!        The upper right hand portion of the product is zero.
!
         do j = 1,3
            do k = 4,6
               output(j,k) = 0.0_wp
            end do
         end do

!
!        The lower right hand portion of the product is a copy of
!        the upper left hand portion of the product.
!
         do j = 4,6
            l = j - 3
            do k = 4,6
               m = k - 3

               output(j,k) = output(l,m)

            end do
         end do

      else if ( n == 1 ) then
!
!        If there is only one matrix in the list the output is
!        simply the input.
!
         do i = 1, 6
            do j = 1, 6
               output(j,i) = matrix(j,i,1)
            end do
         end do

      else if ( n <= 0 ) then

         do j = 1, 6

            output(j,j) = 1.0_wp

            do k = j+1,6
               output(j,k) = 0.0_wp
               output(k,j) = 0.0_wp
            end do
         end do

      end if

      return
      end subroutine zzmsxf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     This routine multiplies together a sequence of state
      !>     transformation matrices.
      !>
      !>## Detailed Input
      !>
      !>     MATRIX      is an array of 3x3  matrices.
      !>
      !>     N           is an integer giving the number of matrices in the
      !>                 sequence.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT      is the product of the matrices stored in MATRIX.
      !>                 Specifically, it is the result of the product
      !>
      !>                       M_N * M_(N-1) * ... * M_2 * M_1
      !>
      !>                 where the K'th matrix M_K is define by the
      !>                 relationship
      !>
      !>                    M_K( I, J )  = MATRIX ( I, J, K )
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1) If N is 0 or smaller OUTPUT will be returned as the
      !>        3x36 identity matrix.
      !>
      !>     2) IF N is 1 OUTPUT will be returned as M_1 where M_1 is
      !>        the matrix defined above in the description of OUTPUT.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a private SPICE routine that computes the product
      !>     of a sequence of 3x3  matrices.
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Reordered header sections. Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
      pure subroutine zzrxr ( matrix, n, output )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp), intent(in) :: matrix ( 3, 3, * )  !! A sequence of state transformation matrices
      integer(ip), intent(in) :: n  !! The number of 3x3  matrices
      real(wp), intent(out) :: output ( 3, 3    )  !! The product of the 3x3 matrices.

      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k

      integer(ip) :: get
      integer(ip) :: put
      integer(ip) :: incr

      real(wp) :: temp ( 3, 3, 2 )

!
!    If we have more than 2 matrices to deal with we will need to
!    set up the PUT location
!
      put = 1

!
!     We perform tests in the order they seem most likely to
!     occur.
!
      if ( n == 2 ) then
!
!        If there are exactly two inputs, then the output takes
!        only a single matrix multiply.
!
         do j = 1,3
            do k = 1,3

               output(j,k) = matrix(j,1,2)*matrix(1,k,1) &
                           + matrix(j,2,2)*matrix(2,k,1) &
                           + matrix(j,3,2)*matrix(3,k,1)
            end do
         end do

      else if ( n > 2 ) then

!
!        We need to compute the product
!
!           MATRIX( , ,N) * MATRIX( , ,N-1) * ... * MATRIX( , , 1 )
!
!        Compute the first product.  MATRIX( , ,2) * MATRIX( , ,1)
!
!        First compute the upper left hand 3x3 portion of the product...
!
         do j = 1,3
            do k = 1,3

               temp(j,k,put) =  matrix(j,1,2)*matrix(1,k,1) &
                              + matrix(j,2,2)*matrix(2,k,1) &
                              + matrix(j,3,2)*matrix(3,k,1)
            end do
         end do

!
!        Now continue building the product.  Note we will toggle
!        back and forth from TEMP(,,1) to TEMP(,,2) for storing
!        (PUTting) the results of our computations.  This way we
!        don't have to spend time moving any of the our computation
!        results to get ready for the next product.  See the end
!        of the loop below (keeping mind the next three values) to
!        see the little trick that's used to toggle back and forth.
!
         incr = -1
         put  =  2
         get  =  1

         do i = 3, n-1
!
!           First the upper left hand portion of the product.
!
            do j = 1,3
               do k = 1,3

                  temp(j,k,put) =  matrix(j,1,i)*temp(1,k,get) &
                                 + matrix(j,2,i)*temp(2,k,get) &
                                 + matrix(j,3,i)*temp(3,k,get)

               end do
            end do

!
!           And as before, we don't need to compute the upper right
!           or lower right hand 3x3 portions of the matrix. So
!           we just skip them.  Toggle GET and PUT so we will
!           be ready for the next pass.
!
            get  =  put
            put  =  put + incr
            incr =      - incr

         end do

!
!        Finally compute the last product.  First the upper
!        left hand portion of the product.
!
         do j = 1,3
            do k = 1,3

               output(j,k) = matrix(j,1,n)*temp(1,k,get) &
                           + matrix(j,2,n)*temp(2,k,get) &
                           + matrix(j,3,n)*temp(3,k,get)
            end do
         end do

      else if ( n == 1 ) then
!
!        If there is only one matrix in the list the output is
!        simply the input.
!
         do i = 1, 3
            do j = 1, 3
               output(j,i) = matrix(j,i,1)
            end do
         end do

      else if ( n <= 0 ) then

         call ident ( output )

      end if

      return
      end subroutine zzrxr

end module pepper_math
