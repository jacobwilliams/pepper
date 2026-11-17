!> Orientation data: body orientation from PCK files and text kernels ([[tisbod]], [[tipbod]]), C-kernel and text-kernel frames ([[ckfrot]], [[ckfxfm]], [[tkfram]]), switch frames ([[zzswfxfm]]), the TEME frame, and frame and SCLK lookups.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_orientation

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: ndc = 2  !! number of double precision components of a segment descriptor
   integer(ip), parameter :: nic = 6  !! number of integer components of a segment descriptor
   integer(ip), parameter :: nc = ndc + ( nic + 1 )/2  !! number of characters (double precision words) a packed descriptor takes
   integer(ip), parameter :: idlen = nc * 8  !! length of a segment identifier (NC double precision words of 8 characters)
   integer(ip), parameter :: timlen = 35  !! length of a time string

contains

      !> Return the spacecraft clock type for a specified spacecraft.
      !>
      !> The function returns the spacecraft clock type associated with
      !> the spacecraft specified by SC.
      !>
      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft, whose
      !>              spacecraft clock `type' is desired.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the spacecraft clock type associated with
      !>     the spacecraft specified by SC.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the kernel variable that assigns a SCLK type to the
      !>         spacecraft specified by SC is not found in the kernel pool, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. SCTYPE returns the value 0 in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The raison d'etre of this routine is that it consolidates the code
      !>     that maps spacecraft ID's to clock types. While any routine may
      !>     call SCTYPE, it is unlikely that there will be a need for
      !>     non-SPICELIB routines to call this routine directly.
      !>
      !>## Examples
      !>
      !>     1)  Find the SCLK type for Galileo.
      !>
      !>            During program initialization, we load a SCLK kernel file
      !>            into the kernel pool. We will pretend that the name of
      !>            this file is GLLSCLK.KER. You must use the actual name of
      !>            an SCLK kernel that is accessible by your program to try
      !>            this example.
      !>
      !>                C
      !>                C     Load the SCLK kernel.
      !>                C
      !>                      CALL FURNSH ( 'GLLSCLK.KER' )
      !>                                 .
      !>                                 .
      !>                                 .
      !>                C
      !>                C     Print out the clock type for Galileo.
      !>                C
      !>                      TYPE = SCTYPE ( -77 )
      !>
      !>                      PRINT *, 'Galileo clock type is ', TYPE
      !>
      !>     2)  Find the SCLK type for Mars Observer.
      !>
      !>                C
      !>                C     Load the SCLK kernel.
      !>                C
      !>                      CALL FURNSH ( 'MOSCLK.KER' )
      !>                                 .
      !>                                 .
      !>                                 .
      !>                C
      !>                C     Print out the clock type for Mars Observer.
      !>                C
      !>                      TYPE = SCTYPE ( -94 )
      !>
      !>                      PRINT *, 'Mars Observer clock type is ', TYPE
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that an SCLK kernel appropriate to the
      !>         spacecraft specified by SC has been loaded into the kernel
      !>         pool.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.0, 12-AUG-2021 (NJB) (JDR)
      !>
      !>        Re-implemented using the type 1 SCLK database in order to
      !>        improve performance. The routine still works as before for
      !>        clock types other than 1.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.3.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 1.2.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.1.0, 22-MAR-1993 (JML)
      !>
      !>        1) The routine now uses the kernel pool watch capability.
      !>
      !>        2) The routine now returns a value of zero if RETURN is
      !>           .TRUE. on entry.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB)
      module integer(ip) function sctype ( ctx, sc )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then

         sctype = 0

         return

      end if

!
!     Get the type from the type 1 subsystem.
!
      call scty01 ( ctx, sc, sctype )

      return
      end function sctype

      !> Return a 6x6 matrix that transforms states in inertial
      !> coordinates to states in body-equator-and-prime-meridian
      !> coordinates.
      !>
      !>## Detailed Input
      !>
      !>     REF      is the NAIF name for an inertial reference frame.
      !>              Acceptable names include:
      !>
      !>                 Name       Description
      !>                 --------   --------------------------------
      !>                 'J2000'    Earth mean equator, dynamical
      !>                            equinox of J2000
      !>
      !>                 'B1950'    Earth mean equator, dynamical
      !>                            equinox of B1950
      !>
      !>                 'FK4'      Fundamental Catalog (4)
      !>
      !>                 'DE-118'   JPL Developmental Ephemeris (118)
      !>
      !>                 'DE-96'    JPL Developmental Ephemeris ( 96)
      !>
      !>                 'DE-102'   JPL Developmental Ephemeris (102)
      !>
      !>                 'DE-108'   JPL Developmental Ephemeris (108)
      !>
      !>                 'DE-111'   JPL Developmental Ephemeris (111)
      !>
      !>                 'DE-114'   JPL Developmental Ephemeris (114)
      !>
      !>                 'DE-122'   JPL Developmental Ephemeris (122)
      !>
      !>                 'DE-125'   JPL Developmental Ephemeris (125)
      !>
      !>                 'DE-130'   JPL Developmental Ephemeris (130)
      !>
      !>                 'GALACTIC' Galactic System II
      !>
      !>                 'DE-200'   JPL Developmental Ephemeris (200)
      !>
      !>                 'DE-202'   JPL Developmental Ephemeris (202)
      !>
      !>              See the Frames Required Reading frames.req for a full
      !>              list of inertial reference frame names built into
      !>              SPICE.
      !>
      !>              The output TSIPM will give the transformation
      !>              from this frame to the bodyfixed frame specified by
      !>              BODY at the epoch specified by ET.
      !>
      !>     BODY     is the integer ID code of the body for which the
      !>              state transformation matrix is requested. Bodies
      !>              are numbered according to the standard NAIF numbering
      !>              scheme. The numbering scheme is explained in the NAIF
      !>              IDs Required Reading naif_ids.req.
      !>
      !>     ET       is the epoch at which the state transformation
      !>              matrix is requested. (This is typically the
      !>              epoch of observation minus the one-way light time
      !>              from the observer to the body at the epoch of
      !>              observation.)
      !>
      !>## Detailed Output
      !>
      !>     TSIPM    is a 6x6 transformation matrix. It is used to
      !>              transform states from inertial coordinates to body
      !>              fixed (also called equator and prime meridian ---
      !>              PM) coordinates.
      !>
      !>              Given a state S in the inertial reference frame
      !>              specified by REF, the corresponding bodyfixed state
      !>              is given by the matrix vector product:
      !>
      !>                 TSIPM * S
      !>
      !>              The X axis of the PM system is directed  to the
      !>              intersection of the equator and prime meridian.
      !>              The Z axis points along  the spin axis and points
      !>              towards the same side of the invariable plane of
      !>              the solar system as does earth's north pole.
      !>
      !>              NOTE: The inverse of TSIPM is NOT its transpose.
      !>              The matrix, TSIPM, has a structure as shown below:
      !>
      !>                 .-            -.
      !>                 |       :      |
      !>                 |   R   :  0   |
      !>                 | ......:......|
      !>                 |       :      |
      !>                 | dR/dt :  R   |
      !>                 |       :      |
      !>                 `-            -'
      !>
      !>              where R is a time varying rotation matrix and dR/dt is
      !>              its derivative. The inverse of this matrix is:
      !>
      !>                 .-              -.
      !>                 |     T  :       |
      !>                 |    R   :  0    |
      !>                 | .......:.......|
      !>                 |        :       |
      !>                 |      T :   T   |
      !>                 | dR/dt  :  R    |
      !>                 |        :       |
      !>                 `-              -'
      !>
      !>              The SPICELIB routine INVSTM is available for producing
      !>              this inverse.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If data required to define the body-fixed frame associated
      !>         with BODY are not found in the binary PCK system or the kernel
      !>         pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In
      !>         the case of IAU style body-fixed frames, the absence of
      !>         prime meridian polynomial data (which are required) is used
      !>         as an indicator of missing data.
      !>
      !>     2)  If the test for exception (1) passes, but in fact requested
      !>         data are not available in the kernel pool, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the kernel pool does not contain all of the data required
      !>         to define the number of nutation precession angles
      !>         corresponding to the available nutation precession
      !>         coefficients, the error SPICE(INSUFFICIENTANGLES) is
      !>         signaled.
      !>
      !>     4)  If the reference frame REF is not recognized, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     5)  If the specified body code BODY is not recognized, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     6)  If, for a given body, both forms of the kernel variable names
      !>
      !>            BODY<body ID>_CONSTANTS_JED_EPOCH
      !>            BODY<body ID>_CONSTS_JED_EPOCH
      !>
      !>         are found in the kernel pool, the error
      !>         SPICE(COMPETINGEPOCHSPEC) is signaled. This is done
      !>         regardless of whether the values assigned to the kernel
      !>         variable names match.
      !>
      !>     7)  If, for a given body, both forms of the kernel variable names
      !>
      !>            BODY<body ID>_CONSTANTS_REF_FRAME
      !>            BODY<body ID>_CONSTS_REF_FRAME
      !>
      !>         are found in the kernel pool, the error
      !>         SPICE(COMPETINGFRAMESPEC) is signaled. This is done
      !>         regardless of whether the values assigned to the kernel
      !>         variable names match.
      !>
      !>     8)  If the central body associated with the input BODY, whether
      !>         a system barycenter or BODY itself, has associated phase
      !>         angles (aka nutation precession angles), and the kernel
      !>         variable BODY<body ID>_MAX_PHASE_DEGREE for the central
      !>         body is present but has a value outside the range 1:3,
      !>         the error SPICE(DEGREEOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Note: NAIF recommends the use of SPKEZR with the appropriate
      !>     frames kernels when possible over TISBOD.
      !>
      !>     The matrix for transforming inertial states to bodyfixed
      !>     states is the 6x6 matrix shown below as a block structured
      !>     matrix.
      !>
      !>        .-            -.
      !>        |       :      |
      !>        | TIPM  :  0   |
      !>        | ......:......|
      !>        |       :      |
      !>        | DTIPM : TIPM |
      !>        |       :      |
      !>        `-            -'
      !>
      !>     This can also be expressed in terms of Euler angles
      !>     PHI, DELTA and W. The transformation from inertial to
      !>     bodyfixed coordinates is represented in the SPICE kernel
      !>     pool as:
      !>
      !>        TIPM   = [W]  [DELTA]  [PHI]
      !>                    3        1      3
      !>     Thus
      !>
      !>        DTIPM  = d[W] /dt [DELTA]  [PHI]
      !>                     3           1      3
      !>
      !>               + [W]  d[DELTA] /dt  [PHI]
      !>                    3             1      3
      !>
      !>               + [W]  [DELTA]  d[PHI] /dt
      !>                    3        1           3
      !>
      !>     If a binary PCK file record can be used for the time and
      !>     body requested, it will be used. The most recently loaded
      !>     binary PCK file has first priority, followed by previously
      !>     loaded binary PCK files in backward time order. If no
      !>     binary PCK file has been loaded, the text P_constants
      !>     kernel file is used.
      !>
      !>     If there is only text PCK kernel information, it is
      !>     expressed in terms of RA, DEC and W, where
      !>
      !>        RA  = PHI - HALFPI
      !>        DEC = HALFPI - DELTA
      !>        W   = W
      !>
      !>     The angles RA, DEC, and W are defined as follows in the
      !>     text PCK file:
      !>
      !>                                      2    .-----
      !>                      RA1*t      RA2*t      \
      !>        RA  = RA0  + -------  + -------   +  )  a(i) * sin( theta(i) )
      !>                        T          2        /
      !>                                  T        '-----
      !>                                              i
      !>
      !>                                       2   .-----
      !>                      DEC1*t     DEC2*t     \
      !>        DEC = DEC0 + -------- + --------  +  )  d(i) * cos( theta(i) )
      !>                        T           2       /
      !>                                   T       '-----
      !>                                              i
      !>
      !>                                     2     .-----
      !>                       W1*t      W2*t       \
      !>        W   = W0   +  ------  + -------   +  )  w(i) * sin( theta(i) )
      !>                        d          2        /
      !>                                  d        '-----
      !>                                              i
      !>
      !>     where `d' is in seconds/day; T in seconds/Julian century;
      !>     a(i), d(i), and w(i) arrays apply to satellites only; and
      !>     theta(i), defined as
      !>
      !>                                THETA1(i)*t
      !>        theta(i) = THETA0(i) + -------------
      !>                                     T
      !>
      !>     are specific to each planet.
      !>
      !>     These angles ---typically nodal rates--- vary in number and
      !>     definition from one planetary system to the next.
      !>
      !>     Thus
      !>
      !>                                   .-----
      !>                 RA1     2*RA2*t    \   a(i)*THETA1(i)*cos(theta(i))
      !>     dRA/dt   = ----- + --------- +  ) ------------------------------
      !>                  T          2      /                 T
      !>                            T      '-----
      !>                                      i
      !>
      !>                                     .-----
      !>                 DEC1     2*DEC2*t    \   d(i)*THETA1(i)*sin(theta(i))
      !>      dDEC/dt = ------ + ---------- -  ) ------------------------------
      !>                   T          2       /                 T
      !>                             T       '-----
      !>                                        i
      !>
      !>                                 .-----
      !>                 W1     2*W2*t    \   w(i)*THETA1(i)*cos(theta(i))
      !>      dW/dt   = ---- + -------- +  ) ------------------------------
      !>                 d         2      /                 T
      !>                          d      '-----
      !>                                    i
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Calculate the matrix to transform a state vector from the
      !>        J2000 frame to the Saturn fixed frame at a specified
      !>        time, and use it to compute the geometric position and
      !>        velocity of Titan in Saturn's body-fixed frame.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: tisbod_ex1.tm
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
      !>              sat375.bsp                    Saturn satellite ephemeris
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'sat375.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TISBOD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      STATE  ( 6    )
      !>      DOUBLE PRECISION      SATVEC ( 6    )
      !>      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      !>
      !>      INTEGER               I
      !>      INTEGER               SATID
      !>
      !>C
      !>C     Load the kernels.
      !>C
      !>      CALL FURNSH ( 'tisbod_ex1.tm' )
      !>
      !>C
      !>C     The body ID for Saturn.
      !>C
      !>      SATID = 699
      !>
      !>C
      !>C     Retrieve the transformation matrix at some time.
      !>C
      !>      CALL STR2ET ( 'Jan 1 2005',   ET        )
      !>      CALL TISBOD ( 'J2000', SATID, ET, TSIPM )
      !>
      !>C
      !>C     Retrieve the state of Titan as seen from Saturn
      !>C     in the J2000 frame at ET.
      !>C
      !>      CALL SPKEZR ( 'TITAN',  ET,    'J2000', 'NONE',
      !>     .              'SATURN', STATE, LT              )
      !>
      !>      WRITE(*,'(A)') 'Titan as seen from Saturn '
      !>     .            // '(J2000 frame):'
      !>      WRITE(*,'(A,3F13.3)') '   position   (km):',
      !>     .               ( STATE(I), I=1,3 )
      !>      WRITE(*,'(A,3F13.3)') '   velocity (km/s):',
      !>     .               ( STATE(I), I=4,6 )
      !>
      !>C
      !>C     Rotate the 6-vector STATE into the
      !>C     Saturn body-fixed reference frame.
      !>C
      !>      CALL MXVG ( TSIPM, STATE, 6, 6, SATVEC )
      !>
      !>      WRITE(*,'(A)') 'Titan as seen from Saturn '
      !>     .            // '(IAU_SATURN frame):'
      !>      WRITE(*,'(A,3F13.3)') '   position   (km):',
      !>     .               ( SATVEC(I), I=1,3 )
      !>      WRITE(*,'(A,3F13.3)') '   velocity (km/s):',
      !>     .               ( SATVEC(I), I=4,6 )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Titan as seen from Saturn (J2000 frame):
      !>           position   (km):  1071928.661  -505781.970   -60383.976
      !>           velocity (km/s):        2.404        5.176       -0.560
      !>        Titan as seen from Saturn (IAU_SATURN frame):
      !>           position   (km):   401063.338 -1116965.364    -5408.806
      !>           velocity (km/s):     -177.547      -63.745        0.028
      !>
      !>        Note that the complete example could be replaced by a single
      !>        SPKEZR call:
      !>
      !>           CALL SPKEZR ( 'TITAN',  ET,    'IAU_SATURN', 'NONE',
      !>          .              'SATURN', STATE, LT                   )
      !>
      !>     2) Use TISBOD is used to compute the angular velocity vector (with
      !>        respect to the J2000 inertial frame) of the specified body at
      !>        given time.
      !>
      !>        Use the meta-kernel from Example 1 above.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TISBOD_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      AV     ( 3    )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      DTIPM  ( 3, 3 )
      !>      DOUBLE PRECISION      OMEGA  ( 3, 3 )
      !>      DOUBLE PRECISION      ROT    ( 3, 3 )
      !>      DOUBLE PRECISION      TIPM   ( 3, 3 )
      !>      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      !>      DOUBLE PRECISION      V      ( 3    )
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               SATID
      !>
      !>C
      !>C     Load the kernels.
      !>C
      !>      CALL FURNSH ( 'tisbod_ex1.tm' )
      !>
      !>C
      !>C     The body ID for Saturn.
      !>C
      !>      SATID = 699
      !>
      !>C
      !>C     First get the state transformation matrix.
      !>C
      !>      CALL STR2ET ( 'Jan 1 2005',   ET        )
      !>      CALL TISBOD ( 'J2000', SATID, ET, TSIPM )
      !>
      !>C
      !>C     This matrix has the form:
      !>C
      !>C          .-            -.
      !>C          |       :      |
      !>C          | TIPM  :  0   |
      !>C          | ......:......|
      !>C          |       :      |
      !>C          | DTIPM : TIPM |
      !>C          |       :      |
      !>C          `-            -'
      !>C
      !>C     We extract TIPM and DTIPM
      !>C
      !>      DO  I = 1,3
      !>         DO  J = 1,3
      !>
      !>            TIPM  ( I, J ) = TSIPM ( I,   J )
      !>            DTIPM ( I, J ) = TSIPM ( I+3, J )
      !>
      !>         END DO
      !>      END DO
      !>
      !>C
      !>C     The transpose of TIPM and DTIPM, (TPMI and DTPMI), gives
      !>C     the transformation from bodyfixed coordinates to inertial
      !>C     coordinates.
      !>C
      !>C     Here is a fact about the relationship between angular
      !>C     velocity associated with a time varying rotation matrix
      !>C     that gives the orientation of a body with respect to
      !>C     an inertial frame.
      !>C
      !>C        The angular velocity vector can be read from the off
      !>C        diagonal components of the matrix product:
      !>C
      !>C                                t
      !>C        OMEGA =     DTPMI * TPMI
      !>C
      !>C                         t
      !>C              =     DTIPM * TIPM
      !>C
      !>C        the components of the angular velocity V will appear
      !>C        in this matrix as:
      !>C
      !>C            .-                   -.
      !>C            |                     |
      !>C            |   0    -V(3)  V(2)  |
      !>C            |                     |
      !>C            |  V(3)    0   -V(1)  |
      !>C            |                     |
      !>C            | -V(2)   V(1)   0    |
      !>C            |                     |
      !>C            `-                   -'
      !>C
      !>C
      !>      CALL MTXM ( DTIPM, TIPM, OMEGA )
      !>
      !>      V(1) = OMEGA (3,2)
      !>      V(2) = OMEGA (1,3)
      !>      V(3) = OMEGA (2,1)
      !>
      !>C
      !>C     Display the results.
      !>C
      !>      WRITE(*,'(A)') 'Angular velocity (km/s):'
      !>      WRITE(*,'(3F16.9)') V
      !>
      !>C
      !>C     It is possible to compute the angular velocity using
      !>C     a single call to XF2RAV.
      !>C
      !>      CALL XF2RAV ( TSIPM, ROT, AV )
      !>
      !>      WRITE(*,'(A)') 'Angular velocity using XF2RAV (km/s):'
      !>      WRITE(*,'(3F16.9)') AV
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Angular velocity (km/s):
      !>             0.000014001     0.000011995     0.000162744
      !>        Angular velocity using XF2RAV (km/s):
      !>             0.000014001     0.000011995     0.000162744
      !>
      !>## Restrictions
      !>
      !>     1)  The kernel pool must be loaded with the appropriate
      !>         coefficients (from a text or binary PCK file) prior to calling
      !>         this routine.
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
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.5.1, 16-DEC-2021 (NJB) (JDR)
      !>
      !>        The routine was updated to support user-defined maximum phase
      !>        angle degrees. The additional text kernel kernel variable name
      !>        BODYnnn_MAX_PHASE_DEGREE must be used when the phase angle
      !>        polynomials have degree higher than 1. The maximum allowed
      !>        degree is 3.
      !>
      !>        The kernel variable names
      !>
      !>           BODY#_CONSTS_REF_FRAME
      !>           BODY#_CONSTS_JED_EPOCH
      !>
      !>        are now recognized.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added note to $Particulars section.
      !>
      !>-    SPICELIB Version 4.5.0, 26-JUL-2016 (BVS)
      !>
      !>        The routine was updated to be more efficient by using a hash
      !>        and buffers so save text PCK data instead of doing kernel POOL
      !>        look-ups over an over again. The routine now checks the POOL
      !>        state counter and dumps all buffered data if it changes.
      !>
      !>        BUG FIX: changed available room in the BODVCD call
      !>        fetching 'NUT_PREC_ANGLES' from MAXANG to MAXANG*2.
      !>
      !>-    SPICELIB Version 4.4.0, 01-FEB-2008 (NJB)
      !>
      !>        The routine was updated to improve the error messages created
      !>        when required PCK data are not found. Now in most cases the
      !>        messages are created locally rather than by the kernel pool
      !>        access routines. In particular missing binary PCK data will
      !>        be indicated with a reasonable error message.
      !>
      !>-    SPICELIB Version 4.3.0, 13-DEC-2005 (NJB)
      !>
      !>        Bug fix: previous update introduced bug in state
      !>        transformation when REF was unequal to PCK native frame.
      !>
      !>-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
      !>
      !>        Re-wrote portions of algorithm to simplify source code.
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MXM and VADDG calls.
      !>
      !>        Replaced calls to ZZBODVCD with calls to BODVCD.
      !>
      !>-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
      !>
      !>        Tests of routine FAILED() were added.
      !>
      !>-    SPICELIB Version 4.0.0, 12-FEB-2004 (NJB)
      !>
      !>        Code has been updated to support satellite ID codes in the
      !>        range 10000 to 99999 and to allow nutation precession angles
      !>        to be associated with any object.
      !>
      !>        Implementation changes were made to improve robustness
      !>        of the code.
      !>
      !>-    SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
      !>
      !>        Properly initialized the variable NPAIRS.
      !>
      !>-    SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
      !>
      !>        Changed to call PCKMAT rather than PCKEUL.
      !>
      !>-    SPICELIB Version 3.1.0, 18-OCT-1994 (KSZ)
      !>
      !>        Fixed bug which incorrectly modded DW by two pi.
      !>
      !>-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
      !>
      !>        Changed to look for binary PCK file, and used this
      !>        to find Euler angles, if such data has been loaded.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
      !>
      !>        Updated to handle P_constants referenced to different epochs
      !>        and inertial reference frames.
      !>
      !>        $Required_Reading and $Literature_References sections were
      !>        updated.
      !>
      !>-    SPICELIB Version 1.0.0, 05-NOV-1990 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.5.0, 26-JUL-2016 (BVS)
      !>
      !>        The routine was updated to be more efficient by using a hash
      !>        and buffers so save text PCK data instead of doing kernel POOL
      !>        look-ups over an over again. The routine now checks the POOL
      !>        state counter and dumps all buffered data if it changes.
      !>
      !>-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
      !>
      !>        Re-wrote portions of algorithm to simplify source code.
      !>        The routine now takes advantage of EUL2XF, which wasn't
      !>        available when the first version of this routine was written.
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MXM and VADDG calls.
      !>
      !>        Replaced calls to ZZBODVCD with calls to BODVCD.
      !>
      !>-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
      !>
      !>        Tests of routine FAILED() were added. The new checks
      !>        are intended to prevent arithmetic operations from
      !>        being performed with uninitialized or invalid data.
      !>
      !>-     SPICELIB Version 4.0.0, 27-JAN-2004 (NJB)
      !>
      !>         Code has been updated to support satellite ID codes in the
      !>         range 10000 to 99999 and to allow nutation precession angles
      !>         to be associated with any object.
      !>
      !>         Calls to deprecated kernel pool access routine RTPOOL
      !>         were replaced by calls to GDPOOL.
      !>
      !>         Calls to BODVAR have been replaced with calls to
      !>         ZZBODVCD.
      !>
      !>-     SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
      !>
      !>        The variable NPAIRS is now initialized
      !>        at the same point as NA, NTHETA, ND, and NW to be
      !>        zero. This prevents the routine from performing
      !>        needless calculations for planets and avoids possible
      !>        floating point exceptions.
      !>
      !>-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
      !>
      !>        TISBOD now gets the TSIPM matrix from PCKMAT.
      !>        Reference frame calculation moved to end.
      !>
      !>-     SPICELIB Version 3.0.1, 07-OCT-1994 (KSZ)
      !>
      !>        TISBOD bug which mistakenly moded DW by 2PI
      !>        was removed.
      !>
      !>-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
      !>
      !>        TISBOD now uses new software to check for the
      !>        existence of binary PCK files, search the for
      !>        data corresponding to the requested body and time,
      !>        and return the appropriate Euler angles. Otherwise
      !>        the code calculates the Euler angles from the
      !>        P_constants kernel file.
      !>
      !>-     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
      !>
      !>         Updated to handle P_constants referenced to different epochs
      !>         and inertial reference frames.
      !>
      !>         TISBOD now checks the kernel pool for presence of the
      !>         variables
      !>
      !>            BODY#_CONSTANTS_REF_FRAME
      !>
      !>         and
      !>
      !>            BODY#_CONSTANTS_JED_EPOCH
      !>
      !>         where # is the NAIF integer code of the barycenter of a
      !>         planetary system or of a body other than a planet or
      !>         satellite. If either or both of these variables are
      !>         present, the P_constants for BODY are presumed to be
      !>         referenced to the specified inertial frame or epoch.
      !>         If the epoch of the constants is not J2000, the input
      !>         time ET is converted to seconds past the reference epoch.
      !>         If the frame of the constants is not the frame specified
      !>         by REF, the rotation from the P_constants' frame to
      !>         body-fixed coordinates is transformed to the rotation from
      !>         the requested frame to body-fixed coordinates. The same
      !>         transformation is applied to the derivative of this
      !>         rotation.
      !>
      !>         Due to the prescience of the original author, the code
      !>         was already prepared to handle the possibility of
      !>         specification of a P_constants inertial reference frame via
      !>         kernel pool variables.
      !>
      !>         Also, the $Required_Reading and $Literature_References
      !>         sections were updated. The SPK required reading has been
      !>         deleted from the $Literature_References section, and the
      !>         NAIF_IDS, KERNEL, and TIME Required Reading files have
      !>         been added in the $Required_Reading section.
      !>
      !>     Local parameters
      !>
      !>     Maximum number of coefficients per phase angle polynomial.
      !>
      !>     Local variables
      !>
      !>     POOL state counter.
      !>
      !>     ID-based hash for text PCK data. KIDLST, KIDPOL, and KIDIDS
      !>     provide the index in the body PCK data arrays at which the data
      !>     of the body with a given ID are stored.
      module subroutine tisbod ( ctx, ref, body, et, tsipm )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: halfpi, j2000, rpd, spd, twopi
      use pepper_math, only: mxm, vdotg
      use pepper_strings, only: suffix, repmi_inplace
      use pepper_inc_errhnd
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: ref  !! ID of inertial reference frame to transform from
      integer(ip), intent(in) :: body  !! ID code of body
      real(wp), intent(in) :: et  !! Epoch of transformation
      real(wp), intent(out) :: tsipm   ( 6,6 )  !! Transformation (state), inertial to prime meridian
      real(wp), allocatable :: sinth(:), costh(:), dsinth(:), dcosth(:)
      real(wp) :: dpval_1(1)  !! one-element copy of dpval (array argument)
      integer(ip) :: pcref_1(1)  !! one-element copy of pcref (array argument)
      real(wp) :: pckepc_1(1)  !! one-element copy of pckepc (array argument)
      integer(ip), parameter :: mxphas = 4
      integer(ip), parameter :: mxpoly = 3
      integer(ip), parameter :: namlen = 32
      integer(ip), parameter :: maxbod = 157
      character(:), allocatable :: fixfrm
      character(timlen) :: timstr
      integer(ip) :: nphsco
      integer(ip) :: reqref
      logical :: found
      real(wp) :: tc
      real(wp) :: w
      real(wp) :: wcoef  ( 0:mxpoly-1 )
      integer(ip) :: m
      logical :: found2
      integer(ip) :: nphase
      real(wp) :: ddelta
      real(wp) :: pckepc
      integer(ip) :: at
      integer(ip) :: deg
      real(wp) :: tmpepc
      character(lmsgln) :: errmsg
      real(wp) :: req2pc (     3, 3  )
      integer(ip) :: k
      integer(ip) :: avail
      logical :: new
      character(1) :: dtype
      real(wp) :: dec
      integer(ip) :: dim
      real(wp) :: dphi
      real(wp) :: epoch
      real(wp) :: xtipm  (     3, 3  )
      real(wp) :: dra
      real(wp) :: eulsta (     6     )
      real(wp) :: dtheta
      real(wp) :: xdtipm (     3, 3  )
      integer(ip) :: cent
      integer(ip) :: l
      real(wp) :: td
      real(wp) :: ddec
      integer(ip) :: pcref
      real(wp) :: phi
      real(wp) :: dw
      integer(ip) :: nw
      integer(ip) :: nd
      real(wp) :: sintmp
      real(wp) :: dpval
      character(namlen) :: item2
      integer(ip) :: frcode
      real(wp) :: delta
      real(wp) :: dcoef  ( 0:mxpoly-1 )
      real(wp) :: rcoef  ( 0:mxpoly-1 )
      integer(ip) :: i
      integer(ip) :: ntheta
      integer(ip) :: na
      real(wp) :: ra
      integer(ip) :: j
      logical :: update
      real(wp) :: costmp
      integer(ip) :: refid
      character(namlen) :: item
      real(wp) :: theta
      real(wp), allocatable :: tcoef(:)
      real(wp) :: tipm   (     3, 3  )
      real(wp), allocatable :: dc(:)
      real(wp), allocatable :: wc(:)
      real(wp) :: dtipm  (     3,  3 )
      real(wp), allocatable :: ac(:)
      if ( .not. allocated ( ctx%tisbod_state ) ) then
         allocate ( ctx%tisbod_state )
         call init_tisbod_state ( ctx%tisbod_state )
      end if
      associate ( d => ctx%tisbod_state%d, &
                  t => ctx%tisbod_state%t, &
                  j2code => ctx%tisbod_state%j2code, &
                  first => ctx%tisbod_state%first, &
                  pulctr => ctx%tisbod_state%pulctr, &
                  bidlst => ctx%tisbod_state%bidlst, &
                  bidpol => ctx%tisbod_state%bidpol, &
                  bidids => ctx%tisbod_state%bidids, &
                  bdcoef => ctx%tisbod_state%bdcoef, &
                  bpckep => ctx%tisbod_state%bpckep, &
                  brcoef => ctx%tisbod_state%brcoef, &
                  bwcoef => ctx%tisbod_state%bwcoef, &
                  bna => ctx%tisbod_state%bna, &
                  bnd => ctx%tisbod_state%bnd, &
                  bnphas => ctx%tisbod_state%bnphas, &
                  bnphco => ctx%tisbod_state%bnphco, &
                  bnw => ctx%tisbod_state%bnw, &
                  bpcref => ctx%tisbod_state%bpcref, &
                  bang => ctx%tisbod_state%bang )
      if ( ctx%failed() ) return

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( ctx, pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( ctx, maxbod, bidlst, bidpol )

!
!        Get the code for the J2000 frame.
!
         call irfnum ( ctx, 'J2000', j2code )

!
!        Set seconds per day and per century.
!
         d = spd
         t = d  *  36525.0_wp

         first = .false.

      end if

      call irfnum  ( ctx, ref, reqref )

!
!     Get state transformation matrix from high precision PCK file, if
!     available.
!
      call pckmat ( ctx, body, et, pcref, tsipm, found )

      if ( .not. found ) then
!
!        The data for the frame of interest are not available in a
!        loaded binary PCK file. This is not an error: the data may be
!        present in the kernel pool.
!
!        Check the POOL counter. If it changed, dump all buffered
!        constants data.
!
         call zzpctrck ( ctx, pulctr, update )

         if ( update ) then
            call zzhsiini ( ctx, maxbod, bidlst, bidpol )
         end if

!
!        Check if we have data for this body in our buffers.
!
         call zzhsichk ( ctx, bidlst, bidpol, bidids, body, at )

         if ( at /= 0 ) then
!
!           Set PCREF as it is used after the text PCK "IF".
!
            pcref  = bpcref ( at )

         else
!
!           Conduct a non-error-signaling check for the presence of a
!           kernel variable that is required to implement an IAU style
!           body-fixed reference frame. If the data aren't available,
!           we don't want BODVCD to signal a SPICE(KERNELVARNOTFOUND)
!           error; we want to issue the error signal locally, with a
!           better error message.
!
            item = 'BODY#_PM'
            call repmi_inplace ( item, '#',   body )
            call dtpool ( ctx, item, found, nw,   dtype )

            if ( .not. found ) then
!
!              Now we do have an error.
!
!              We don't have the data we'll need to produced the
!              requested state transformation matrix. In order to
!              create an error message understandable to the user,
!              find, if possible, the name of the reference frame
!              associated with the input body. Note that the body is
!              really identified by a PCK frame class ID code, though
!              most of the documentation just calls it a body ID code.
!
               call frame_of_class ( ctx, pck, body,  frcode, fixfrm, cent, found )
               call etcal  ( ctx, et,  timstr )

               errmsg = 'PCK data required to compute the orientation ' &
            //       'of the # # for epoch # TDB were not found. ' &
            //       'If these data were to be provided by a binary ' &
            //       'PCK file, then it is possible that the PCK ' &
            //       'file does not have coverage for the specified ' &
            //       'body-fixed frame at the time of interest. If the ' &
            //       'data were to be provided by a text PCK file, ' &
            //       'then possibly the file does not contain data ' &
            //       'for the specified body-fixed frame. In ' &
            //       'either case it is possible that a required ' &
            //       'PCK file was not loaded at all.'

!
!              Fill in the variable data in the error message.
!
               if ( found ) then
!
!                 The frame system knows the name of the body-fixed
!                 frame.
!
                  call ctx%setmsg ( errmsg                  )
                  call ctx%errch  ( '#', 'body-fixed frame' )
                  call ctx%errch  ( '#', fixfrm             )
                  call ctx%errch  ( '#', timstr             )

               else
!
!                 The frame system doesn't know the name of the
!                 body-fixed frame, most likely due to a missing
!                 frame kernel.
!
                  call suffix ( '#', 1, errmsg                      )
                  call ctx%setmsg ( errmsg                              )
                  call ctx%errch  ( '#', &
                             'body-fixed frame associated with ' &
               //            'the ID code'                       )
                  call ctx%errint ( '#', body                           )
                  call ctx%errch  ( '#', timstr                         )
                  call ctx%errch  ( '#', &
                             'Also, a frame kernel defining the ' &
               //            'body-fixed frame associated with ' &
               //            'body # may need to be loaded.'     )
                  call ctx%errint ( '#', body                           )

               end if

               call ctx%sigerr ( 'SPICE(FRAMEDATANOTFOUND)' )
               return

            end if

!
!           Find the body code used to label the reference frame and
!           epoch specifiers for the orientation constants for BODY.
!
!           For planetary systems, the reference frame and epoch for
!           the orientation constants is associated with the system
!           barycenter, not with individual bodies in the system. For
!           any other bodies, (the Sun or asteroids, for example) the
!           body's own code is used as the label.
!
            refid = zzbodbry ( ctx, body )

!
!           Look up the epoch of the constants. The epoch is specified
!           as a Julian ephemeris date. The epoch defaults to J2000.
!
!           Look for both forms of the JED epoch kernel variable. At
!           most one is allowed to be present.
!
            item   =  'BODY#_CONSTANTS_JED_EPOCH'
            call repmi_inplace ( item, '#', refid )

            item2  =  'BODY#_CONSTS_JED_EPOCH'
            call repmi_inplace ( item2, '#', refid )

            call gdpool ( ctx, item, 1, 1, dim, pckepc_1, found )
            pckepc = pckepc_1(1)

            if ( .not. found ) then

               call gdpool ( ctx, item2, 1, 1, dim, pckepc_1, found2 )
               pckepc = pckepc_1(1)

               if ( .not. found2 ) then
                  pckepc = j2000
               end if

            else
!
!              Check for presence of both forms of the variable names.
!
               call dtpool ( ctx, item2, found2, dim, dtype )

               if ( found2 ) then

                  call ctx%setmsg ( 'Both kernel variables # and # are ' &
                  //            'present in the kernel pool. At most ' &
                  //            'one form of the kernel variable name ' &
                  //            'may be present.'           )
                  call ctx%errch  ( '#', item                   )
                  call ctx%errch  ( '#', item2                  )
                  call ctx%sigerr ( 'SPICE(COMPETINGEPOCHSPEC)' )
                  return

               end if

            end if

!
!           Look up the reference frame of the constants. The reference
!           frame is specified by a code recognized by CHGIRF. The
!           default frame is J2000, symbolized by the code J2CODE.
!
            item   =  'BODY#_CONSTANTS_REF_FRAME'
            call repmi_inplace ( item, '#', refid )

            item2  =  'BODY#_CONSTS_REF_FRAME'
            call repmi_inplace ( item2, '#', refid )

            call repmi_inplace ( item, '#', refid )
            call gipool ( ctx, item, 1, 1, dim, pcref_1, found )
            pcref = pcref_1(1)

            if ( .not. found ) then

               call gipool ( ctx, item2, 1, 1, dim, pcref_1, found )
               pcref = pcref_1(1)

               if ( .not. found ) then
                  pcref  =  j2code
               end if

            else
!
!              Check for presence of both forms of the variable names.
!
               call dtpool ( ctx, item2, found, dim, dtype )

               if ( found ) then

                  call ctx%setmsg ( 'Both kernel variables # and # are ' &
                  //            'present in the kernel pool. At most ' &
                  //            'one form of the kernel variable name ' &
                  //            'may be present.'           )
                  call ctx%errch  ( '#', item                   )
                  call ctx%errch  ( '#', item2                  )
                  call ctx%sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
                  return

               end if

            end if

!
!           Whatever the body, it has quadratic time polynomials for
!           the RA and Dec of the pole, and for the rotation of the
!           Prime Meridian.
!
            item = 'POLE_RA'
            rcoef(0:0+mxpoly-1) = 0.0_wp
            call bodvcd ( ctx, body, item, mxpoly, na, rcoef )

            if ( ctx%failed() ) return

            item = 'POLE_DEC'
            dcoef(0:0+mxpoly-1) = 0.0_wp
            call bodvcd ( ctx, body, item, mxpoly, nd, dcoef )

            if ( ctx%failed() ) return

            item = 'PM'
            wcoef(0:0+mxpoly-1) = 0.0_wp
            call bodvcd ( ctx, body, item, mxpoly, nw, wcoef )

            if ( ctx%failed() ) return

!
!           Whether or not the body is a satellite, there may be
!           additional nutation and libration (THETA) terms.
!
            ntheta = 0
            nphase = 0
            nphsco = 0
            na     = 0
            nd     = 0
            nw     = 0

!
!           PEPPER: the arrays have the sizes of the kernel variables
!           (SPICELIB: MAXANG, 200, values per item).
!
            allocate ( tcoef(max(1_ip, body_item_size(ctx, refid, 'NUT_PREC_ANGLES'))), source=0.0_wp )
            allocate ( ac(max(1_ip, body_item_size(ctx, body, 'NUT_PREC_RA'))), source=0.0_wp )
            allocate ( dc(max(1_ip, body_item_size(ctx, body, 'NUT_PREC_DEC'))), source=0.0_wp )
            allocate ( wc(max(1_ip, body_item_size(ctx, body, 'NUT_PREC_PM'))), source=0.0_wp )
            item = 'NUT_PREC_ANGLES'

            if ( bodfnd ( ctx, refid, item ) ) then

!              Find out whether the maximum phase angle degree
!              has been explicitly set.
!
               item2 = 'MAX_PHASE_DEGREE'

               if ( bodfnd( ctx,refid, item2) ) then

                  call bodvcd ( ctx, refid, item2, 1, dim, dpval_1 )
                  dpval = dpval_1(1)

                  deg = nint( dpval )

                  if ( ( deg < 1 ) .or. ( deg > mxphas-1 ) ) then

                     call ctx%setmsg ( 'Maximum phase angle degree for ' &
                     //            'body # must be in the range 1:# ' &
                     //            'but was #.'                      )
                     call ctx%errint ( '#', refid                        )
                     call ctx%errint ( '#', mxphas-1                     )
                     call ctx%errint ( '#', deg                          )
                     call ctx%sigerr ( 'SPICE(DEGREEOUTOFRANGE)'         )
                     return

                  end if

                  nphsco = deg + 1

               else
!
!                 The default degree is 1, yielding two coefficients.
!
                  nphsco = 2

               end if
!
!              There is something a bit obscure going on below. BODVCD
!              loads the array TCOEF in the following order
!
!                 TCOEF(1,1), TCOEF(2,1), ... TCOEF(NPHSCO),
!                 TCOEF(1,2), TCOEF(2,2), ...
!
!              The NTHETA that comes back is the total number of items
!              loaded, but we will need the actual limit on the second
!              dimension. That is --- NTHETA / NPHSCO.
!
               call bodvcd ( ctx, refid, item, size(tcoef, kind=ip), ntheta, tcoef )

               if ( ctx%failed() ) return

!
!              NPHSCO is at least 1; this division is safe.
!
               nphase = ntheta / nphsco

            end if

!
!           Look up the right ascension nutations in the precession of
!           the pole. NA is the number of Ascension coefficients. AC
!           are the Ascension coefficients.
!
            item = 'NUT_PREC_RA'

            if ( bodfnd ( ctx, body, item ) ) then

               call bodvcd ( ctx, body, item, size(ac, kind=ip), na, ac )

               if ( ctx%failed() ) return

            end if

!
!           Look up the declination nutations in the precession of the
!           pole. ND is the number of Declination coefficients. DC are
!           the Declination coefficients.
!
            item = 'NUT_PREC_DEC'

            if ( bodfnd ( ctx, body, item ) ) then

               call bodvcd ( ctx, body, item, size(dc, kind=ip), nd, dc )

               if ( ctx%failed() ) return

            end if

!
!           Finally look up the prime meridian nutations. NW is the
!           number of coefficients. WC is the array of coefficients.
!
            item = 'NUT_PREC_PM'

            if ( bodfnd ( ctx, body, item ) ) then

               call bodvcd ( ctx, body, item, size(wc, kind=ip), nw, wc )

               if ( ctx%failed() ) return

            end if

!
!           The number of coefficients returned had better not be
!           bigger than the number of angles we are going to compute.
!           If it is we simply signal an error and bag it, for sure.
!
            if ( max( na, nd, nw ) > nphase ) then
               call ctx%setmsg ( 'Insufficient number of nutation/' &
               //            'precession angles for body * at time #. ' &
               //            'Number of angles is #; number required ' &
               //            'is #.'                )
               call ctx%errint ( '*', body              )
               call ctx%errdp  ( '#', et                )
               call ctx%errint ( '#', nphase            )
               call ctx%errint ( '#', max( na, nd, nw ) )
               call ctx%sigerr ( 'SPICE(INSUFFICIENTANGLES)' )
               return
            end if

!
!           We succeeded to fetch all data for this body. Save it in
!           the buffers. First check if there is room. If not, dump the
!           buffers to make room.
!
            call zzhsiavl( ctx, bidpol, avail )
            if ( avail <= 0 ) then
               call zzhsiini ( ctx, maxbod, bidlst, bidpol )
            end if

!
!           Add this body to the hash and save its data in the buffers.
!
            call zzhsiadd ( ctx, bidlst, bidpol, bidids, body, at, new )

            bpckep ( at ) = pckepc
            bpcref ( at ) = pcref
            bnphas ( at ) = nphase
            bnphco ( at ) = nphsco
            bna    ( at ) = na
            bnd    ( at ) = nd
            bnw    ( at ) = nw

            brcoef(0:0+mxpoly-1,at) = rcoef(0:0+mxpoly-1)
            bdcoef(0:0+mxpoly-1,at) = dcoef(0:0+mxpoly-1)
            bwcoef(0:0+mxpoly-1,at) = wcoef(0:0+mxpoly-1)
            bang(at)%t = tcoef(1:ntheta)
            bang(at)%a = ac(1:na)
            bang(at)%d = dc(1:nd)
            bang(at)%w = wc(1:nw)

         end if

!
!        The reference epoch in the PCK is given as JED. Convert to
!        ephemeris seconds past J2000. Then convert the input ET to
!        seconds past the reference epoch.
!
         tmpepc  =  spd * ( bpckep(at) - j2000 )
         epoch   =  et    -   tmpepc
         td      =  epoch / d
         tc      =  epoch / t

!
!        Evaluate the time polynomials and their derivatives w.r.t.
!        EPOCH at EPOCH.
!
!        Evaluate the time polynomials at EPOCH.
!
         ra  = brcoef(0,at) + tc * ( brcoef(1,at) + tc * brcoef(2,at) )
         dec = bdcoef(0,at) + tc * ( bdcoef(1,at) + tc * bdcoef(2,at) )
         w   = bwcoef(0,at) + td * ( bwcoef(1,at) + td * bwcoef(2,at) )

         dra  = ( brcoef(1,at)  + 2.0_wp*tc*brcoef(2,at) ) / t
         ddec = ( bdcoef(1,at)  + 2.0_wp*tc*bdcoef(2,at) ) / t
         dw   = ( bwcoef(1,at)  + 2.0_wp*td*bwcoef(2,at) ) / d

!
!        Compute the nutations and librations (and their derivatives)
!        as appropriate.
!
         allocate ( sinth(bnphas(at)), costh(bnphas(at)), dsinth(bnphas(at)), dcosth(bnphas(at)) )
         do i = 1, bnphas(at)

            if ( bnphco(at) == 2 ) then
!
!              This case is so common that we'll deal with it
!              separately. We'll avoid unnecessary arithmetic
!              operations.
!
               k = 1 + 2*(i-1)
               m = 1 + k

               theta  = ( bang(at)%t(k) + tc * bang(at)%t(m) ) * rpd
               dtheta = ( bang(at)%t(m) / t ) * rpd

            else
!
!              THETA and DTHETA have higher-order terms; there are
!              BNPHCO(AT) coefficients for each angle.
!
               theta = 0.0_wp

               do j = 1, bnphco(at)
!
!                 K is the start index for the coefficients of the
!                 Ith angle.
!
                  k     = j + bnphco(at)*(i-1)

                  theta = theta  + ( tc**(j-1) * bang(at)%t(k) )

               end do

               theta  = theta * rpd

               k = 1 + bnphco(at)*(i-1)
               m = 1 + k

               dtheta = bang(at)%t(m) / t

               do j = 3, bnphco(at)

                  k      = j + bnphco(at)*(i-1)
                  l      = j - 1
!
!                 Differentiate with respect to EPOCH the Jth
!                 term of the expression for the Ith angle; add
!                 the result to the current sum.
!
                  dtheta = dtheta + &
                           ( l * tc**(l-1) * bang(at)%t(k) ) / t**(l-1)
               end do

               dtheta = dtheta * rpd

            end if

            sintmp    =  sin ( theta )
            costmp    =  cos ( theta )

            sinth(i)  =  sintmp
            costh(i)  =  costmp
            dsinth(i) =  costmp * dtheta
            dcosth(i) = -sintmp * dtheta

         end do

!
!        Adjust RA, DEC, W and their derivatives by the librations
!        and nutations.
!
         ra  = ra    + vdotg ( bang(at)%a, sinth,  bna(at) )
         dec = dec   + vdotg ( bang(at)%d, costh,  bnd(at) )
         w   = w     + vdotg ( bang(at)%w, sinth,  bnw(at) )

         dra  = dra  + vdotg ( bang(at)%a, dsinth, bna(at) )
         ddec = ddec + vdotg ( bang(at)%d, dcosth, bnd(at) )
         dw   = dw   + vdotg ( bang(at)%w, dsinth, bnw(at) )

!
!        Convert from degrees to radians
!
         ra      = ra   * rpd
         dec     = dec  * rpd
         w       = w    * rpd

         dra     = dra  * rpd
         ddec    = ddec * rpd
         dw      = dw   * rpd
!
!        Convert to Euler angles.
!
         w       = mod ( w,  twopi )
         phi     = ra + halfpi
         delta   = halfpi - dec

         dphi    =   dra
         ddelta  = - ddec

         if ( ctx%failed() ) return

!
!        Pack the Euler angles and their derivatives into
!        a state vector.
!
         eulsta(1:3) = [w, delta, phi]
         eulsta(4:6) = [dw, ddelta, dphi]

!
!        Find the state transformation defined by the Euler angle
!        state vector. The transformation matrix TSIPM has the
!        following structure:
!
!            -            -
!           |       :      |
!           | TIPM  :  0   |
!           | ......:......|
!           |       :      |
!           | DTIPM : TIPM |
!           |       :      |
!            -            -
!
         call eul2xf ( ctx, eulsta, 3, 1, 3, tsipm )

      end if

!
!     At this point the base frame PCREF has been determined.
!
!     If the requested base frame is not base frame associated with the
!     PCK data, adjust the transformation matrix TSIPM to map from the
!     requested frame to the body-fixed frame.
!
      if ( reqref /= pcref ) then
!
!        Next get the position transformation from the user specified
!        inertial frame to the native PCK inertial frame.
!
         call irfrot ( ctx, reqref, pcref, req2pc )

         if ( ctx%failed() ) return

!
!        Since we're applying an inertial transformation to TSIPM,
!        we can rotate the non-zero blocks of TSIPM. This saves
!        a bunch of double precision multiplications.
!
!        Extract the upper and lower left blocks of TSIPM.
!
         do i = 1, 3

            do j = 1, 3

               tipm (i,j) =  tsipm( i,   j )
               dtipm(i,j) =  tsipm( i+3, j )

            end do

         end do

!
!        Rotate the blocks. Note this is a right multiplication.
!
         call mxm ( tipm,  req2pc, xtipm  )
         call mxm ( dtipm, req2pc, xdtipm )

!
!        Replace the non-zero blocks of TSIPM. This gives us the
!        transformation from the requested frame to the
!        bodyfixed frame.
!
         do i = 1, 3

            do j = 1, 3

               tsipm( i,   j   ) = xtipm (i,j)
               tsipm( i+3, j+3 ) = xtipm (i,j)
               tsipm( i+3, j   ) = xdtipm(i,j)

            end do

         end do

      end if
!
!     That's all folks. Check out and get out.
!
      return
      end associate
      end subroutine tisbod

      !> Find the position rotation matrix from a Text Kernel (TK) frame
      !> with the specified frame class ID to its base frame.
      !>
      !>## Detailed Input
      !>
      !>     FRCODE   is the unique frame class ID of the TK frame for which
      !>              data is being requested. For TK frames the frame class
      !>              ID is always equal to the frame ID.
      !>
      !>## Detailed Output
      !>
      !>     ROT      is a position rotation matrix that converts positions
      !>              relative to the TK frame given by its frame class ID,
      !>              FRCODE, to positions relative to the base frame given by
      !>              its frame ID, FRAME.
      !>
      !>              Thus, if a position S has components x,y,z in the TK
      !>              frame, then S has components x', y', z' in the base
      !>              frame.
      !>
      !>                 .-  -.     .-     -. .- -.
      !>                 | x' |     |       | | x |
      !>                 | y' |  =  |  ROT  | | y |
      !>                 | z' |     |       | | z |
      !>                 `-  -'     `-     -' `- -'
      !>
      !>     FRAME    is the ID code of the base reference frame to which ROT
      !>              will transform positions.
      !>
      !>     FOUND    is a logical indicating whether or not a frame definition
      !>              for the TK frame with the frame class ID, FRCODE, was
      !>              constructed from kernel pool data. If ROT and FRAME were
      !>              constructed, FOUND will be returned with the value .TRUE.
      !>              Otherwise it will be returned with the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     BUFSIZ   is the number of rotation, frame class ID pairs that can
      !>              have their instance data buffered for the sake of
      !>              improving run-time performance. This value MUST be
      !>              positive and should probably be at least 10.
      !>
      !>## Exceptions
      !>
      !>     1)  If some kernel variable associated with this frame is not
      !>         present in the kernel pool, or does not have the proper type
      !>         or dimension, an error is signaled by a routine in the call
      !>         tree of this routine. In such a case FOUND will be set to
      !>         .FALSE.
      !>
      !>     2)  If the input FRCODE has the value 0, the error
      !>         SPICE(ZEROFRAMEID) is signaled. FOUND will be set to .FALSE.
      !>
      !>     3)  If the name of the frame corresponding to FRCODE cannot be
      !>         determined, the error SPICE(INCOMPLETEFRAME) is signaled.
      !>
      !>     4)  If the frame given by FRCODE is defined relative to a frame
      !>         that is unrecognized, the error SPICE(BADFRAMESPEC) is
      !>         signaled. FOUND will be set to .FALSE.
      !>
      !>     5)  If the kernel pool specification for the frame given by
      !>         FRCODE is not one of 'MATRIX', 'ANGLES' or 'QUATERNION',
      !>         the error SPICE(UNKNOWNFRAMESPEC) is signaled. FOUND will be
      !>         set to .FALSE.
      !>
      !>     6)  If the frame FRCODE is equal to the relative frame ID (i.e.
      !>         the frame is defined relative to itself), the error
      !>         SPICE(BADFRAMESPEC2) is signaled. FOUND will be set to .FALSE.
      !>
      !>     7)  If name-based and ID-based forms of any TKFRAME_ keyword
      !>         are detected in the kernel pool at the same time, the error
      !>         SPICE(COMPETINGFRAMESPEC) is signaled. FOUND will be set to
      !>         .FALSE.
      !>
      !>## Files
      !>
      !>     This routine makes use of the loaded text kernels to determine
      !>     the rotation from a constant offset TK frame to its base frame.
      !>
      !>## Particulars
      !>
      !>     This routine is used to construct the rotation from some frame
      !>     that is a constant rotation offset from some other reference
      !>     frame. This rotation is derived from data stored in the kernel
      !>     pool.
      !>
      !>     This routine is intended to be used as a low level routine by the
      !>     frame system software. However, you could use this routine to
      !>     directly retrieve the rotation from an fixed offset TK frame to
      !>     its base frame.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Compute the rotation from the DSS-34 topocentric frame to
      !>        its base Earth body-fixed frame and use it to determine the
      !>        geodetic latitude and longitude of the DSS-34 site.
      !>
      !>        Use the FK kernel below to load the required topocentric
      !>        reference frame definition for the DSS-34 site.
      !>
      !>           earth_topo_050714.tf
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TKFRAM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      DOUBLE PRECISION      DPR
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         MYTOPO
      !>      PARAMETER           ( MYTOPO = 'DSS-34_TOPO' )
      !>
      !>      INTEGER               MXFRLN
      !>      PARAMETER           ( MXFRLN = 26 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(MXFRLN)    FRNAME
      !>
      !>      DOUBLE PRECISION      LAT
      !>      DOUBLE PRECISION      LON
      !>      DOUBLE PRECISION      RAD
      !>      DOUBLE PRECISION      ROT   ( 3, 3 )
      !>      DOUBLE PRECISION      Z     ( 3    )
      !>
      !>      INTEGER               FRAME
      !>      INTEGER               FRCODE
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load the FK that contains the topocentric reference
      !>C     frame definition for DSS-34.
      !>C
      !>      CALL FURNSH ( 'earth_topo_050714.tf' )
      !>
      !>C
      !>C     The name of the topocentric frame is MYTOPO.
      !>C     First we get the ID code of the topocentric frame.
      !>C
      !>      CALL NAMFRM ( MYTOPO, FRCODE )
      !>
      !>C
      !>C     Next get the rotation from the topocentric frame to
      !>C     the body-fixed frame. We can use the TK frame ID in
      !>C     place of the TK frame class ID in this call because
      !>C     for TK frames these IDs are identical.
      !>C
      !>      CALL TKFRAM ( FRCODE, ROT, FRAME, FOUND )
      !>
      !>C
      !>C     Make sure the topocentric frame is relative to one of
      !>C     the Earth fixed frames.
      !>C
      !>      CALL FRMNAM( FRAME, FRNAME )
      !>
      !>      IF (       FRNAME .NE. 'IAU_EARTH'
      !>     .     .AND. FRNAME .NE. 'EARTH_FIXED'
      !>     .     .AND. FRNAME .NE. 'ITRF93'  ) THEN
      !>
      !>         WRITE (*,*) 'The frame ', MYTOPO,
      !>     .               ' does not appear to be '
      !>         WRITE (*,*) 'defined relative to an '
      !>     .            // 'Earth fixed frame.'
      !>         STOP
      !>
      !>      END IF
      !>
      !>C
      !>C     Things look ok. Get the location of the Z-axis in the
      !>C     topocentric frame.
      !>C
      !>      Z(1) = ROT(1,3)
      !>      Z(2) = ROT(2,3)
      !>      Z(3) = ROT(3,3)
      !>
      !>C
      !>C     Convert the Z vector to latitude, longitude and radius.
      !>C
      !>      CALL RECLAT ( Z, RAD, LAT, LON )
      !>
      !>      WRITE (*,'(A)') 'The geodetic coordinates of the center'
      !>      WRITE (*,'(A)') 'of the topographic frame are:'
      !>      WRITE (*,*)
      !>      WRITE (*,'(A,F20.13)') '   Latitude  (deg): ', LAT*DPR()
      !>      WRITE (*,'(A,F20.13)') '   Longitude (deg): ', LON*DPR()
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        The geodetic coordinates of the center
      !>        of the topographic frame are:
      !>
      !>           Latitude  (deg):    148.9819650021110
      !>           Longitude (deg):    -35.3984778756552
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
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.3.0, 20-AUG-2021 (JDR) (BVS) (NJB)
      !>
      !>        BUG FIX: the routine now signals an error if it detects
      !>        name-based and ID-based forms of any TKFRAME_ keyword present
      !>        in the POOL at the same time. This prevents name-based
      !>        keywords from frame definitions loaded with lower priority
      !>        from being used instead of ID-based keywords from frame
      !>        definitions loaded with higher priority.
      !>
      !>        BUG FIX: when failing to fetch any frame keywords from the
      !>        POOL or for any other reason, the routine now always returns
      !>        FOUND = .FALSE. Previously FOUND could be set to .TRUE. by a
      !>        DTPOOL call preceding the failure.
      !>
      !>        BUG FIX: when failing due to a frame defined relative to
      !>        itself or due to an unrecognized _SPEC, the routine now always
      !>        returns FRAME = 0. Previously FRAME was set to the _RELATIVE
      !>        keyword.
      !>
      !>        BUG FIX: the misspelled short error message
      !>        SPICE(INCOMPLETEFRAME) was corrected. The message had been
      !>        spelled correctly in header comments but not in the code.
      !>
      !>        Changed to return ROT as identity for all failures; previously
      !>        it was returned this way only for some failures.
      !>
      !>        Changed the input argument name ID to FRCODE for consistency
      !>        with other routines.
      !>
      !>        Fixed minor typo on the UNKNOWNFRAMESPEC long error message.
      !>
      !>        Edited the header to comply with NAIF standard and modern
      !>        SPICE CK and frames terminology.
      !>
      !>        Added complete code example based on existing fragments.
      !>
      !>        Construction of kernel variable names now uses trimmed
      !>        strings in order to suppress gfortran compile warnings.
      !>
      !>        Added DATA statements to initialize BUFFI, BUFFD, and IDENTS.
      !>        This change suppresses ftnchek warnings for variables possibly
      !>        not initialized before use. It is not a bug fix.
      !>
      !>        Minor inline comment typos were corrected.
      !>
      !>-    SPICELIB Version 2.2.0, 08-JAN-2014 (BVS)
      !>
      !>        Added an error check for frames defined relative to
      !>        themselves.
      !>
      !>        Increased BUFSIZ from 20 to 200.
      !>
      !>-    SPICELIB Version 2.1.0, 23-APR-2009 (NJB)
      !>
      !>        Bug fix: watch is deleted only for frames
      !>        that are deleted from the buffer.
      !>
      !>-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now deletes watches set on
      !>        kernel variables of frames that are discarded from
      !>        the local buffering system.
      !>
      !>-    SPICELIB Version 1.2.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in CONVRT, UCRSS, VHATG and VSCL calls.
      !>
      !>-    SPICELIB Version 1.1.0, 21-NOV-2001 (FST)
      !>
      !>        Updated this routine to dump the buffer of frame ID codes
      !>        it saves when it or one of the modules in its call tree
      !>        signals an error. This fixes a bug where a frame's ID code is
      !>        buffered, but the matrix and kernel pool watcher were not set
      !>        properly.
      !>
      !>-    SPICELIB Version 1.0.0, 18-NOV-1996 (WLT)
      module subroutine tkfram ( ctx, frcode, rot, frame, found )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: ident, q2m, sharpr, vdot, vhatg
      use pepper_strings, only: intstr, rtrim, repmi_inplace, ucase_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frcode  !! Frame class ID of a TK frame.
      real(wp), intent(out) :: rot   ( 3, 3 )  !! Rotation matrix from TK frame to frame FRAME.
      integer(ip), intent(out) :: frame  !! Frame ID of the base reference.
      logical, intent(out) :: found  !! .TRUE. if the rotation could be determined.
      integer(ip), parameter :: bufsiz = 200
      integer(ip), parameter :: wdsize = 32
      integer(ip), parameter :: nitems = 14
      integer(ip), parameter :: nvars = 14
      logical :: full
      integer(ip) :: tail
      character(wdsize) :: units
      integer(ip) :: ar
      character(:), allocatable :: name
      real(wp) :: tempd
      real(wp) :: quatrn ( 0:3 )
      character(wdsize) :: agent
      logical :: found2
      logical :: found1
      logical :: fnd
      logical :: buffrd
      real(wp) :: qtmp   ( 0:3 )
      character(wdsize) :: idstr
      character(:), allocatable :: altnat
      character(:), allocatable :: item ( : )
      character(:), allocatable :: alt ( : )
      real(wp) :: matrix ( 3,    3      )
      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: oldid
      character(wdsize) :: spec
      character(1) :: type
      integer(ip) :: r
      logical :: update
      real(wp) :: angles ( 3 )
      integer(ip) :: axes   ( 3 )
      character(:), allocatable :: frname
      if ( .not. allocated ( ctx%tkfram_state ) ) then
         allocate ( ctx%tkfram_state )
         call init_tkfram_state ( ctx%tkfram_state )
      end if
      associate ( oldagt => ctx%tkfram_state%oldagt, &
                  buffd => ctx%tkfram_state%buffd, &
                  at => ctx%tkfram_state%at, &
                  buffi => ctx%tkfram_state%buffi, &
                  idents => ctx%tkfram_state%idents, &
                  idnt => ctx%tkfram_state%idnt, &
                  first => ctx%tkfram_state%first, &
                  pool => ctx%tkfram_state%pool )
      if ( first ) then

         first = .false.

         call lnkini ( ctx, bufsiz, pool )

      end if

!
!     Now do the standard SPICE error handling.  Sure this is
!     a bit unconventional, but nothing will be hurt by doing
!     the stuff above first.
!
      if ( ctx%failed() ) return

!
!     So far, we've not FOUND the rotation to the specified frame.
!
      found = .false.

!
!     Check the ID to make sure it is non-zero.
!
      if (  frcode == 0 ) then

         call lnkini ( ctx, bufsiz, pool )

         call ctx%setmsg ( 'Frame identification codes are ' &
         //            'required to be non-zero.  You''ve ' &
         //            'specified a frame with ID value ' &
         //            'zero. '                            )
         call ctx%sigerr ( 'SPICE(ZEROFRAMEID)'                )
         return

      end if

!
!     Find out whether our linked list pool is already full.
!     We'll use this information later to decide whether we're
!     going to have to delete a watcher.
!
      full = lnknfn( ctx,pool) == 0

      if ( full ) then
!
!        If the input frame ID is not buffered, we'll need to
!        overwrite an existing buffer entry. In this case
!        the call to LOCATI we're about to make will overwrite
!        the ID code in the slot we're about to use. We need
!        this ID code, so extract it now while we have the
!        opportunity. The old ID sits at the tail of the list
!        whose head node is AT.
!
         tail  = lnktl  ( ctx, at, pool )

         oldid = idents ( 1,  tail )

!
!        Create the name of the agent associated with the old
!        frame.
!
         oldagt = 'TKFRAME_#'
         call repmi_inplace ( oldagt, '#', oldid )

      end if

!
!     Look up the address of the instance data.
!
      idnt(1) = frcode
      call locati ( ctx, idnt, 1, idents, pool, at, buffrd )

      if (  full  .and. ( .not. buffrd )  ) then
!
!        Since the buffer is already full, we'll delete the watcher for
!        the kernel variables associated with OLDID, since there's no
!        longer a need for that watcher.
!
!        First clear the update status of the old agent; DWPOOL won't
!        delete an agent with a unchecked update.
!
         call cvpool ( ctx, oldagt, update )
         call dwpool ( ctx, oldagt )

      end if

!
!     Until we have better information we put the identity matrix
!     into the output rotation and set FRAME to zero.
!
      call ident ( rot )
      frame = 0

!
!     If we have to look up the data for our frame, we do
!     it now and perform any conversions and computations that
!     will be needed when it's time to convert coordinates to
!     directions.
!
!     Construct the name of the agent associated with the
!     requested frame.  (Each frame has its own agent).
!
      call intstr ( frcode, idstr  )
      call frame_name_of ( ctx, frcode, frname, fnd )

      if ( frname == ' ' ) then

         call lnkini ( ctx, bufsiz, pool )

         call ctx%setmsg ( 'The Text Kernel (TK) frame with ID code ' &
         //            '# does not have a recognized name. '      )
         call ctx%errint ( '#', frcode                                )
         call ctx%sigerr ( 'SPICE(INCOMPLETEFRAME)'                   )
         return

      end if

      agent  = 'TKFRAME_' // idstr( :rtrim(idstr) )
      r      =  rtrim(agent)

      altnat = 'TKFRAME_' // frname
      ar     =  rtrim(altnat)

!
!     If the frame is buffered, we check the kernel pool to
!     see if there has been an update to this frame.
!
      if ( buffrd ) then

         call cvpool ( ctx, agent(1:r), update )

      else
!
!        If the frame is not buffered we definitely need to update
!        things.

         update = .true.

      end if

      if ( .not. update ) then
!
!        Just look up the rotation matrix and relative-to
!        information from the local buffer.
!
         rot(1,1)  = buffd ( 1, at )
         rot(2,1)  = buffd ( 2, at )
         rot(3,1)  = buffd ( 3, at )
         rot(1,2)  = buffd ( 4, at )
         rot(2,2)  = buffd ( 5, at )
         rot(3,2)  = buffd ( 6, at )
         rot(1,3)  = buffd ( 7, at )
         rot(2,3)  = buffd ( 8, at )
         rot(3,3)  = buffd ( 9, at )

         frame     = buffi ( 1, at )

      else
!
!        Determine how the frame is specified and what it
!        is relative to.  The variables that specify
!        how the frame is represented and what it is relative to
!        are TKFRAME_#_SPEC and TKFRAME_#_RELATIVE where # is
!        replaced by the text value of ID or the frame name.
!
!        PEPPER: the kernel variable names are as long as the frame
!        name needs (SPICELIB: 32 characters, saved work arrays).
!
         allocate ( character(max(r, ar) + 10) :: item(nvars), alt(nitems) )
         item(1) = agent(1:r) // '_SPEC'
         item(2) = agent(1:r) // '_RELATIVE'

         alt (1) = altnat(1:ar) // '_SPEC'
         alt (2) = altnat(1:ar) // '_RELATIVE'
!
!        See if the friendlier version of the kernel pool variables
!        are available.
!
!        If both forms are present, we signal an error.
!
         do i = 1, 2

            call dtpool ( ctx, item(i), found1, n, type )
            call dtpool ( ctx, alt(i),  found2, n, type )

            if ( found1 .and. found2 ) then
               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               call ctx%setmsg ( 'Frame name-based and frame '    // &
                             'ID-based text kernel (fixed-'   // &
                             'offset) frame definition '      // &
                             'keywords ''#'' and ''#'' are '  // &
                             'both present in the '           // &
                             'POOL. Most likely this is '     // &
                             'because loaded text kernels '   // &
                             'contain competing definitions ' // &
                             'of the ''#'' frame using '      // &
                             'different keyword styles, '     // &
                             'which is not allowed. '         )
               call ctx%errch  ( '#', item(i) )
               call ctx%errch  ( '#', alt(i) )
               call ctx%errch  ( '#', frname )
               call ctx%sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return
            end if

            if ( found2 ) then
               item(i) = alt(i)
            end if

         end do

!
!        If either the SPEC or RELATIVE frame are missing from
!        the kernel pool, we simply return.
!
         if (     badkpv ( ctx, 'TKFRAM', item(1), '=', 1, 1, 'C' ) &
             .or. badkpv ( ctx, 'TKFRAM', item(2), '=', 1, 1, 'C' ) ) then

            call lnkini ( ctx, bufsiz, pool )
            frame =  0
            call ident  (  rot     )
            return

         end if

!
!        If we make it this far, look up the SPEC and RELATIVE frame.
!
         call gcpool ( ctx, item(1),   1, 1, n, spec,  fnd )
         call ctx%pool_string ( item(2), 1_ip, name, fnd )

!
!        Look up the ID code for this frame.
!
         call namfrm ( ctx, name, frame )

         if ( frame == 0 ) then

            call lnkini ( ctx, bufsiz, pool )
            call ident  (  rot     )
            call ctx%setmsg ( 'The frame to which frame # is ' &
            //            'relatively defined is not ' &
            //            'recognized. The kernel pool ' &
            //            'specification of the relative ' &
            //            'frame is ''#''.  This is not a ' &
            //            'recognized frame. '              )
            call ctx%errint ( '#', frcode                       )
            call ctx%errch  ( '#', name                         )
            call ctx%sigerr ( 'SPICE(BADFRAMESPEC)'             )
            return

         end if

!
!        Make sure that the RELATIVE frame ID is distinct from the
!        frame ID. If they are the same, SPICE will go into an
!        indefinite loop.
!
         if ( frame == frcode ) then

            call lnkini ( ctx, bufsiz, pool )
            frame =  0
            call ident  (  rot     )
            call ctx%setmsg ( 'Bad fixed offset frame specification: ' &
            //            'the frame ''#'' (frame ID #) is ' &
            //            'defined relative to itself. SPICE ' &
            //            'cannot work with such frames. '         )
            call ctx%errch  ( '#', name                                )
            call ctx%errint ( '#', frcode                              )
            call ctx%sigerr ( 'SPICE(BADFRAMESPEC2)'                   )
            return

         end if

!
!        Convert SPEC to upper case so that we can easily check
!        to see if this is one of the expected specification types.
!
         call ucase_inplace ( spec )

         if ( spec == 'MATRIX' ) then
!
!           This is the easiest case.  Just grab the matrix
!           from the kernel pool (and polish it up a bit just
!           to make sure we have a rotation matrix).
!
!           We give preference to the kernel pool variable
!           TKFRAME_<name>_MATRIX if it is available.
!
!           If both forms are present, we signal an error.
!
            item(3) = agent (1:r)  // '_MATRIX'
            alt (3) = altnat(1:ar) // '_MATRIX'

            call dtpool ( ctx, item(3), found1, n, type )
            call dtpool ( ctx, alt(3),  found2, n, type )

            if ( found1 .and. found2 ) then
               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               call ctx%setmsg ( 'Frame name-based and frame '    // &
                             'ID-based text kernel (fixed-'   // &
                             'offset) frame definition '      // &
                             'keywords ''#'' and ''#'' are '  // &
                             'both present in the '           // &
                             'POOL. Most likely this is '     // &
                             'because loaded text kernels '   // &
                             'contain competing definitions ' // &
                             'of the ''#'' frame using '      // &
                             'different keyword styles, '     // &
                             'which is not allowed. '         )
               call ctx%errch  ( '#', item(3) )
               call ctx%errch  ( '#', alt(3) )
               call ctx%errch  ( '#', frname )
               call ctx%sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return
            end if

            if ( found2 ) then
               item(3) = alt(3)
            end if

            if ( badkpv ( ctx, 'TKFRAM', item(3), '=', 9, 1, 'N' ) ) then

               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return

            end if

!
!           The variable meets current expectations, look it up
!           from the kernel pool.
!
            call gdpool ( ctx, item(3),   1, 9, n, matrix,  fnd )

!
!           In this case the full transformation matrix has been
!           specified.  We simply polish it up a bit.
!
            rot = matrix
            call sharpr (            rot )

!
!           The matrix might not be right-handed, so correct
!           the sense of the second and third columns if necessary.
!
            if ( vdot( rot(1,2), matrix(1,2) ) < 0.0_wp ) then
               rot(:,2) = (-1.0_wp)*rot(:,2)
            end if

            if ( vdot( rot(1,3), matrix(1,3) ) < 0.0_wp ) then
               rot(:,3) = (-1.0_wp)*rot(:,3)
            end if

         else if ( spec == 'ANGLES' ) then
!
!           Look up the angles, their units and axes for the
!           frame specified by ID. (Note that UNITS are optional).
!           As in the previous case we give preference to the
!           form TKFRAME_<name>_<item> over TKFRAME_<id>_<item>.
!
            item(3) = agent(1:r)   // '_ANGLES'
            item(4) = agent(1:r)   // '_AXES'
            item(5) = agent(1:r)   // '_UNITS'

            alt (3) = altnat(1:ar) // '_ANGLES'
            alt (4) = altnat(1:ar) // '_AXES'
            alt (5) = altnat(1:ar) // '_UNITS'
!
!           Again, we give preference to the more friendly form
!           of TKFRAME specification.
!
!           If both forms are present, we signal an error.
!
            do i = 3, 5

               call dtpool ( ctx, item(i), found1, n, type )
               call dtpool ( ctx, alt(i),  found2, n, type )

               if ( found1 .and. found2 ) then
                  call lnkini ( ctx, bufsiz, pool )
                  frame =  0
                  call ident  ( rot      )
                  call ctx%setmsg ( 'Frame name-based and frame '    // &
                                'ID-based text kernel (fixed-'   // &
                                'offset) frame definition '      // &
                                'keywords ''#'' and ''#'' are '  // &
                                'both present in the '           // &
                                'POOL. Most likely this is '     // &
                                'because loaded text kernels '   // &
                                'contain competing definitions ' // &
                                'of the ''#'' frame using '      // &
                                'different keyword styles, '     // &
                                'which is not allowed. '         )
                  call ctx%errch  ( '#', item(i) )
                  call ctx%errch  ( '#', alt(i) )
                  call ctx%errch  ( '#', frname )
                  call ctx%sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
                  return
               end if

               if ( found2 ) then
                  item(i) = alt(i)
               end if

            end do

            if (    badkpv( ctx, 'TKFRAM', item(3), '=', 3, 1, 'N' ) &
               .or. badkpv( ctx, 'TKFRAM', item(4), '=', 3, 1, 'N' ) ) &
            then

               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return

            end if

            call gdpool ( ctx, item(3),   1, 3, n, angles, fnd )
            call gipool ( ctx, item(4),   1, 3, n, axes,   fnd )
            call gcpool ( ctx, item(5),   1, 1, n, units,  fnd )

!           (SPICELIB sets the default before the call and relies on
!           GCPOOL leaving it unchanged; PEPPER's GCPOOL output is
!           INTENT(OUT).)
            if ( .not. fnd ) units = 'RADIANS'

!
!           Convert angles to radians.
!
            do i = 1, 3

               call convrt ( ctx, angles(i), units, 'RADIANS', tempd )
               angles(i) = tempd

            end do

!
!           Compute the rotation from instrument frame to CK frame.
!
            call eul2m ( ctx,  angles(1),   angles(2),   angles(3), &
                          axes  (1),   axes  (2),   axes  (3), rot )

            if ( ctx%failed() ) then
               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return
            end if

         else if ( spec == 'QUATERNION' ) then
!
!           Look up the quaternion and convert it to a rotation
!           matrix. Again there are two possible variables that
!           may point to the quaternion. We give preference to
!           the form TKFRAME_<name>_Q over the form TKFRAME_<id>_Q.
!
!           If both forms are present, we signal an error.
!
            item(3) = agent(1:r)   // '_Q'
            alt (3) = altnat(1:ar) // '_Q'

            call dtpool ( ctx, item(3), found1, n, type )
            call dtpool ( ctx, alt(3),  found2, n, type )

            if ( found1 .and. found2 ) then
               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               call ctx%setmsg ( 'Frame name-based and frame '    // &
                             'ID-based text kernel (fixed-'   // &
                             'offset) frame definition '      // &
                             'keywords ''#'' and ''#'' are '  // &
                             'both present in the '           // &
                             'POOL. Most likely this is '     // &
                             'because loaded text kernels '   // &
                             'contain competing definitions ' // &
                             'of the ''#'' frame using '      // &
                             'different keyword styles, '     // &
                             'which is not allowed. '         )
               call ctx%errch  ( '#', item(3) )
               call ctx%errch  ( '#', alt(3) )
               call ctx%errch  ( '#', frname )
               call ctx%sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               return
            end if

            if ( found2 ) then
               item(3) = alt(3)
            end if

            if ( badkpv ( ctx, 'TKFRAM', item(3), '=', 4, 1, 'N' ) ) then
               call lnkini ( ctx, bufsiz, pool )
               frame =  0
               call ident  ( rot      )
               return
            end if
!
!           In this case we have the quaternion representation.
!           Again, we do a small amount of polishing of the input.
!
            call gdpool ( ctx, item(3), 1, 4, n, quatrn, fnd )
            call vhatg  ( quatrn,  4,       qtmp   )
            call q2m    ( qtmp,             rot    )

         else
!
!           We don't recognize the SPEC for this frame.  Say
!           so.  Also note that perhaps the user needs to upgrade
!           the toolkit.
!
            call lnkini ( ctx, bufsiz, pool )
            frame =  0
            call ident  ( rot      )
            call ctx%setmsg ( 'The frame specification "# = ' &
            //            '''#''" is not one of the recognized ' &
            //            'means of specifying a text-kernel ' &
            //            'constant offset frame. ' &
            //            'This may reflect a typographical ' &
            //            'error or may indicate that you ' &
            //            'need to consider updating your ' &
            //            'version of the SPICE toolkit. ' )

            call ctx%errch  ( '#', item(1) )
            call ctx%errch  ( '#', spec    )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAMESPEC)' )
            return

         end if

!
!        Buffer the identifier, relative frame and rotation matrix.
!
         buffd ( 1, at ) = rot(1,1)
         buffd ( 2, at ) = rot(2,1)
         buffd ( 3, at ) = rot(3,1)
         buffd ( 4, at ) = rot(1,2)
         buffd ( 5, at ) = rot(2,2)
         buffd ( 6, at ) = rot(3,2)
         buffd ( 7, at ) = rot(1,3)
         buffd ( 8, at ) = rot(2,3)
         buffd ( 9, at ) = rot(3,3)

         buffi ( 1, at ) = frame

!
!        If these were not previously buffered, we need to set
!        a watch on the various items that might be used to define
!        this frame.
!
         if ( .not. buffrd ) then
!
!           Immediately check for an update so that we will
!           not redundantly look for this item the next time this
!           routine is called.
!
            item( 1) = agent(1:r)   // '_RELATIVE'
            item( 2) = agent(1:r)   // '_SPEC'
            item( 3) = agent(1:r)   // '_AXES'
            item( 4) = agent(1:r)   // '_MATRIX'
            item( 5) = agent(1:r)   // '_Q'
            item( 6) = agent(1:r)   // '_ANGLES'
            item( 7) = agent(1:r)   // '_UNITS'

            item( 8) = altnat(1:ar) // '_RELATIVE'
            item( 9) = altnat(1:ar) // '_SPEC'
            item(10) = altnat(1:ar) // '_AXES'
            item(11) = altnat(1:ar) // '_MATRIX'
            item(12) = altnat(1:ar) // '_Q'
            item(13) = altnat(1:ar) // '_ANGLES'
            item(14) = altnat(1:ar) // '_UNITS'

            call swpool ( ctx, agent, 14, item )
            call cvpool ( ctx, agent, update   )

         end if

      end if

      if ( ctx%failed() ) then
         call lnkini ( ctx, bufsiz, pool )
         frame =  0
         call ident  ( rot      )
         return
      end if

!
!     All errors cause the routine to exit before we get to this
!     point.  If we reach this point we didn't have an error and
!     hence did find the rotation from ID to FRAME.
!
      found = .true.

!
!     That's it
!
      return
      end associate
      end subroutine tkfram

      !> Look up a frame definition kernel variable whose associated
      !> value is a frame name or frame ID code.  The returned value is
      !> always an ID code.  The kernel variable name can refer to
      !> the frame being defined by either name or ID code.
      !>
      !> If the kernel variable is not present, or if the variable
      !> is not a frame name or a numeric value, signal an error.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.  This frame
      !>                    may be thought of as the frame associated with
      !>                    "left hand side" of the kernel variable
      !>                    assignment.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.  This
      !>                    is the ID code corresponding to FRNAME.
      !>
      !>     ITEM           is a string identifying the specific datum
      !>                    to be fetched.  The kernel variable name
      !>                    has the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    or
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    The former of the two names takes precedence:
      !>                    this routine will look for a numeric variable
      !>                    of that name first.
      !>
      !>                    The value associated with the kernel variable
      !>                    must be one of
      !>
      !>                       - a reference frame ID code
      !>
      !>                       - a string representation of an integer,
      !>                         for example '5'
      !>
      !>                       - a reference frame name
      !>
      !>## Detailed Output
      !>
      !>     IDCODE         is the frame ID code associated with the value of
      !>                    the requested kernel variable. This frame may be
      !>                    regarded as being associated with the "right hand
      !>                    side." of the kernel variable assignment. The
      !>                    kernel variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has numeric type, the associated
      !>                    value will be returned.  If this variable is
      !>                    found and has character type, the value will
      !>                    be converted to a frame ID code, and that
      !>                    code will be returned.
      !>
      !>                    If this variable is not found, the variable
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    will be looked up.  If this variable is found and
      !>                    has numeric type, the associated value will be
      !>                    returned.  If this variable is found and has
      !>                    character type, the value will be converted to a
      !>                    frame ID code, and that code will be returned.
      !>
      !>                    If a numeric value associated with the selected
      !>                    kernel variable is not integral, it will be
      !>                    rounded to the closest integer.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If neither the frame-ID-based or frame-name-based form of the
      !>        requested kernel variable name matches a kernel variable
      !>        present in the kernel pool, the error SPICE(KERNELVARNOTFOUND)
      !>        will be signaled.
      !>
      !>     2) If either the frame-ID-based or frame-name-based form of the
      !>        requested kernel variable name has length greater than KVNMLN,
      !>        the excessively long name will not be searched for. A search
      !>        will still be done using the alternative form of the name if
      !>        that form has length less than or equal to KVNMLN.
      !>
      !>     3) If both the frame-ID-based and frame-name-based forms of the
      !>        requested kernel variable name have length greater than KVNMLN,
      !>        the error SPICE(VARNAMETOOLONG) will be signaled.
      !>
      !>     4) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than one
      !>        associated value, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
      !>
      !>     5) If a name match is found for a character kernel variable, but
      !>        the value associated with the variable cannot be mapped to a
      !>        frame ID code or an integer, the error SPICE(NOTRANSLATION)
      !>        is signaled.
      !>
      !>     6) If a name match is found for a numeric kernel variable,
      !>        but that variable has a value that cannot be rounded to an
      !>        integer representable on the host platform, an error will
      !>        be signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     1) Kernel variables fetched by this routine are normally
      !>        introduced into the kernel pool by loading one or more
      !>        frame kernels.  See the Frames Required Reading for
      !>        details.
      !>
      !>## Particulars
      !>
      !>     This routine centralizes logic for kernel variable lookups that
      !>     must be performed by the SPICELIB frame subsystem. Part of the
      !>     functionality of this routine consists of handling error
      !>     conditions such as the unavailability of required kernel
      !>     variables; hence no "found" flag is returned to the caller.
      !>
      !>     As indicated above, the requested kernel variable may have a name
      !>     of the form
      !>
      !>        FRAME_<frame ID code>_<ITEM>
      !>
      !>     or
      !>
      !>        FRAME_<frame name>_<ITEM>
      !>
      !>     Because most frame definition keywords have the first form, this
      !>     routine looks for a name of that form first.
      !>
      !>     Note that although this routine considers the two forms of the
      !>     names to be synonymous, from the point of view of the kernel pool
      !>     data structure, these names are distinct.  Hence kernel variables
      !>     having names of both forms, but having possibly different
      !>     attributes, can be simultaneously present in the kernel pool.
      !>     Intentional use of this kernel pool feature is discouraged.
      !>
      !>## Examples
      !>
      !>     1) See ZZDYNFRM.
      !>
      !>     2) Applications of this routine include finding ID codes of
      !>        frames associated with velocity vectors or constant vectors
      !>        serving to define two-vector dynamic frames.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) An array-valued kernel variable matching the "ID code form"
      !>        of the requested kernel variable name could potentially
      !>        mask a scalar-valued kernel variable matching the "name
      !>        form" of the requested name.  This problem can be prevented
      !>        by sensible frame kernel design.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
      module subroutine zzdynfid ( ctx, frname, frcode, item, idcode )
      use pepper_kinds, only: ip
      use pepper_strings, only: beint, intstr, repmc, rtrim
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(out) :: idcode  !! Output kernel variable.
      integer(ip) :: idcode_1(1)  !! one-element copy of idcode (array argument)

      character(:), allocatable :: outnam
      character(1) :: dtype
      character(:), allocatable :: kvname

      integer(ip) :: n

      logical :: found

      if ( ctx%failed() ) return

!
!     Prepare to check the name of the kernel variable we're about
!     to look up.
!
!     Convert the frame code to a string.
!
!     PEPPER: the kernel variable names have any length (SPICELIB:
!     32 characters, VARNAMETOOLONG); the lookup is shared by the
!     ZZDYNxxx routines.
!
      call dynamic_frame_variable ( ctx, frname, frcode, item, .true., kvname, found, n, dtype )
      if ( ctx%failed() ) return

!
!     Getting to this point means we found a kernel variable. The name
!     of the variable is KVNAME.  The data type is DTYPE and the
!     cardinality is N.
!
      if ( dtype == 'C' ) then
!
!        Rather than using BADKPV, we check the cardinality of the
!        kernel variable in-line so we can create a more detailed error
!        message if need be.
!
         if ( n > 1 ) then

            call ctx%setmsg ( 'The kernel variable # has used to define ' // &
                          'frame # was expected to have size not '    // &
                          'exceeding 1 but in fact has size #. '      // &
                          'Usually this type of '                     // &
                          'problem is due to an error in a frame '    // &
                          'definition provided in a frame kernel.'    )
            call ctx%errch  ( '#',  kvname                                )
            call ctx%errch  ( '#',  frname                                )
            call ctx%errint ( '#',  n                                     )
            call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                    )
            return

         end if

!
!        Look up the kernel variable.
!
         call ctx%pool_string ( kvname, 1_ip, outnam, found )

         if ( .not. found ) then

            call ctx%setmsg ( 'The kernel variable # has used to define ' // &
                          'frame # was not found after DTPOOL '       // &
                          'indicated it was present in pool.'         )
            call ctx%errch  ( '#',  kvname                                )
            call ctx%errch  ( '#',  frname                                )
            call ctx%sigerr ( 'SPICE(BUG)'                                )
            return

         end if

!
!        Convert the output frame name to a frame code.
!
         call namfrm ( ctx, outnam, idcode )

         if ( idcode == 0 ) then
!
!           If IDCODE is zero, that means NAMFRM couldn't translate
!           the name.  Perhaps the name is an integer?
!
            if ( beint(outnam) ) then

               call prsint ( ctx, outnam, idcode )

            else
!
!              We're outta aces.
!
               call ctx%setmsg ( 'The kernel variable # used to define '  // &
                             'frame # is assigned the character '     // &
                             'value #.  This value was expected to '  // &
                             'be a reference frame name, but NAMFRM ' // &
                             'cannot translate this name to a frame ' // &
                             'ID code.'                               )
               call ctx%errch  ( '#',  kvname                             )
               call ctx%errch  ( '#',  frname                             )
               call ctx%errch  ( '#',  outnam                             )
               call ctx%sigerr ( 'SPICE(NOTRANSLATION)'                   )
               return

            end if

         end if
!
!        IDCODE has been assigned a value at this point.
!

      else
!
!        The variable has numeric type.
!
         if ( n > 1 ) then

            call ctx%setmsg ( 'The kernel variable # has used to define ' // &
                          'frame # was expected to have size not '    // &
                          'exceeding 1 but in fact has size #. '      // &
                          'Usually this type of '                     // &
                          'problem is due to an error in a frame '    // &
                          'definition provided in a frame kernel.'    )
            call ctx%errch  ( '#',  kvname                                )
            call ctx%errch  ( '#',  frname                                )
            call ctx%errint ( '#',  n                                     )
            call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                    )
            return

         end if

!
!        Look up the kernel variable.
!
         call gipool ( ctx, kvname, 1, 1, n, idcode_1, found )
         idcode = idcode_1(1)

         if ( .not. found ) then

            call ctx%setmsg ( 'The kernel variable # has used to define ' // &
                          'frame # was not found after DTPOOL '       // &
                          'indicated it was present in pool.'         )
            call ctx%errch  ( '#',  kvname                                )
            call ctx%errch  ( '#',  frname                                )
            call ctx%sigerr ( 'SPICE(BUG)'                                )
            return

         end if

      end if

      return
      end subroutine zzdynfid

      !> Translate a string containing a frame name to its ID code, but
      !> bypass calling NAMFRM and return saved value provided by the
      !> caller if the name is the same as the saved name and the POOL
      !> state did not change.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     USRCTR      is the value of the POOL state counter tracked by
      !>                 (saved in) the caller (user) routine specifically for
      !>                 this frame name/ID pair of variables.
      !>
      !>     SAVNAM      is the frame name saved in the caller routine
      !>                 specifically for this frame name/ID pair of
      !>                 variables. For detailed description of allowed values
      !>                 see description of the FRNAME argument in NAMFRM.
      !>
      !>     SAVCDE      is the frame ID code saved in the caller routine
      !>                 specifically for this frame name/ID pair of
      !>                 variables. For detailed description of allowed values
      !>                 see description of the FRCODE argument in NAMFRM.
      !>
      !>     FRNAME      is the input frame name. For detailed description of
      !>                 allowed values see description of the FRNAME argument
      !>                 in NAMFRM.
      !>
      !>## Detailed Output
      !>
      !>     USRCTR      is the current POOL state counter.
      !>
      !>     SAVNAM      is the frame name saved in the caller routine
      !>                 specifically for this frame name/ID pair of
      !>                 variables. On the output SAVNAM always equals FRNAME.
      !>
      !>     SAVCDE      is the frame ID code saved in the caller routine
      !>                 specifically for this frame name/ID pair of
      !>                 variables. If the frame name cannot be mapped to an
      !>                 ID, FRCODE is returned as 0. On the output SAVCDE
      !>                 always equals FRCODE.
      !>
      !>     FRCODE      is the output frame ID code. For detailed description
      !>                 of possible values see description of the FRCODE
      !>                 argument in NAMFRM. If the frame name cannot be
      !>                 mapped to an ID, FRCODE is returned as 0. On the
      !>                 output FRCODE always equals SAVCDE.
      !>
      !>## Parameters
      !>
      !>     CTRSIZ      is the dimension of the counter array used by
      !>                 various SPICE subsystems to uniquely identify
      !>                 changes in their states. This parameter is
      !>                 defined in the private include file 'zzctr.inc'.
      !>
      !>## Exceptions
      !>
      !>     1) Errors may be signaled by routines in the call tree of this
      !>        routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine translates a string containing a frame name to an
      !>     integer code but bypasses calling NAMFRM to do the translation
      !>     and simply returns the saved ID value provided by the caller if
      !>     the input name is the same as the saved name provided by the
      !>     caller, the saved ID provided by the caller is not 0, and the
      !>     POOL state counter tracked by the caller is the same as the
      !>     current POOL state counter.
      !>
      !>     The POOL state counter and name/ID pair of saved variables
      !>     tracked by the caller must be specific for each frame of
      !>     interest. I.e. if the caller routine needs to call this routine
      !>     to do translations for two distinct frames, the caller routine
      !>     mush use must have its own set of saved POOL state counter and
      !>     name/ID variables for each of the two frames.
      !>
      !>## Examples
      !>
      !>     This example shows how a routine that needs to do frame name-ID
      !>     conversion for two distinct frames (FRA and FRB) can do it using
      !>     ZZNAMFRM.
      !>
      !>           SUBROUTINE <name> ( FRA, FRB, ... )
      !>           ...
      !>           INCLUDE               'zzctr.inc'
      !>           ...
      !>     C
      !>     C     Saved frame name length.
      !>     C
      !>           INTEGER               FRNMLN
      !>           PARAMETER           ( FRNMLN = 32 )
      !>           ....
      !>     C
      !>     C
      !>     C     Saved name/ID item declarations.
      !>     C
      !>           INTEGER               SVCTR1 ( CTRSIZ )
      !>           CHARACTER*(FRNMLN)    SVFRA
      !>           INTEGER               SVFRAI
      !>           INTEGER               SVCTR2 ( CTRSIZ )
      !>           CHARACTER*(FRNMLN)    SVFRB
      !>           INTEGER               SVFRBI
      !>           LOGICAL               FIRST
      !>     C
      !>     C     Saved name/ID items.
      !>     C
      !>           SAVE                  SVCTR1
      !>           SAVE                  SVFRA
      !>           SAVE                  SVFRAI
      !>           SAVE                  SVCTR2
      !>           SAVE                  SVFRB
      !>           SAVE                  SVFRBI
      !>           SAVE                  FIRST
      !>     C
      !>     C     Initial values.
      !>     C
      !>           DATA                  FIRST   / .TRUE. /
      !>           ...
      !>     C
      !>     C     Initialization.
      !>     C
      !>           IF ( FIRST ) THEN
      !>     C
      !>     C        Initialize POOL counters.
      !>     C
      !>              CALL ZZCTRUIN( SVCTR1 )
      !>              CALL ZZCTRUIN( SVCTR2 )
      !>              FIRST = .FALSE.
      !>           END IF
      !>     C
      !>     C     Starting from translation of FRA name to ID.
      !>     C
      !>           CALL ZZNAMFRM ( SVCTR1, SVFRA, SVFRAI, FRA, FRAID )
      !>
      !>           IF ( FRAID .EQ. 0 ) THEN
      !>              CALL SETMSG ( '...' )
      !>              CALL SIGERR ( 'SPICE(FRAMEAIDCODENOTFOUND)' )
      !>              CALL CHKOUT ( '<name>' )
      !>              RETURN
      !>           END IF
      !>     C
      !>     C     Now do the same for FRB.
      !>     C
      !>           CALL ZZNAMFRM ( SVCTR2, SVFRB, SVFRBI, FRB, FRBID )
      !>
      !>           IF ( FRBID .EQ. 0 ) THEN
      !>              CALL SETMSG ( '...' )
      !>              CALL SIGERR ( 'SPICE(FRAMEBIDCODENOTFOUND)' )
      !>              CALL CHKOUT ( '<name>' )
      !>              RETURN
      !>           END IF
      !>           ...
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
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 23-SEP-2013 (BVS)
      module subroutine zznamfrm ( ctx, usrctr, savnam, savcde, frname, frcode )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! POOL state counter saved by the caller.
      character(*), intent(inout) :: savnam  !! Frame name saved by the caller.
      integer(ip), intent(inout) :: savcde  !! Frame ID code saved by the caller.
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(out) :: frcode  !! Frame ID code.

!
!     Local variables.
!
      logical :: update

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check/update POOL state counter.
!
      call zzpctrck ( ctx, usrctr, update )

!
!     Check update flag, saved ID, and saved name against the input.
!
      if ( .not. update       .and. &
           savcde /= 0      .and. &
           len_trim(frname) <= len(savnam) .and. &
           savnam == frname       ) then

!
!        No change in the POOL state, the saved name was successfully
!        resolved earlier, and input and saved names are the same.
!        Return saved ID.
!
         frcode = savcde

      else

!
!        Check in because NAMFRM may fail.
!

!
!        POOL state changed, or the saved name was never successfully
!        resolved earlier, or input and saved names are different. Call
!        NAMFRM to look up ID and reset saved values.
!
         call namfrm( ctx, frname, frcode )

!
!        PEPPER: a name longer than the caller's saved name is not saved
!        (its truncation could match another, shorter name); it is
!        looked up on every call.
!
         if ( len_trim(frname) <= len(savnam) ) then
            savnam = frname
            savcde = frcode
         else
            savnam = ' '
            savcde = 0
         end if

!
!        Check out.
!

      end if

      return

      end subroutine zznamfrm

      !> Create an informative long error message for cases where the
      !> frame system signals a SPICE(NOFRAMECONNECT) error.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ET             Epoch of frame transformation, expressed as
      !>                    seconds past J2000 TDB.
      !>
      !>     FRAME1         Frame ID code of frame at start of first path.
      !>
      !>     ENDP1          Frame ID code of frame at end of first path;
      !>                    this frame is the last node that could be
      !>                    reached from the frame designated by FRAME1.
      !>
      !>     FRAME2         Frame ID code of frame at start of second path.
      !>
      !>     ENDP2          Frame ID code of frame at end of second path;
      !>                    this frame is the last node that could be
      !>                    reached from the frame designated by FRAME2.
      !>
      !>## Detailed Output
      !>
      !>     ERRMSG         Long error message specifying computable
      !>                    frame paths, indications of missing SCLK
      !>                    or CK data, and optionally, debugging hints.
      !>
      !>                    The rules for formation of this message are:
      !>
      !>                       1) State the epoch.
      !>
      !>                       2) State the names of the frames for which
      !>                          a connection was attempted, if these
      !>                          names are available.
      !>
      !>                       3) State the names of the frames at the
      !>                          endpoints of both paths, if these
      !>                          names are available.
      !>
      !>                          Omit this portion of the message for any
      !>                          path of length one: in other words, if a
      !>                          frame and path endpoint coincide, omit the
      !>                          clause stating the frame can be connected to
      !>                          itself.
      !>
      !>                       4) For any path endpoint frame, if that
      !>                          frame is of CK type, indicate that
      !>                          CK and SCLK data must be loaded for
      !>                          that frame.
      !>
      !>                       5) For any path endpoint frame, if that
      !>                          frame is of CK type and SCLK data for the
      !>                          SCLK associated with that frame are not
      !>                          available, indicate this problem, along with
      !>                          the CK and SCLK ID codes associated with
      !>                          this frame.
      !>
      !>                       6) If at least one path endpoint frame
      !>                          is of CK type, and all required SCLK data
      !>                          are present, include a closing message
      !>                          explaining how CK coverage may be inadequate
      !>                          and recommending use of CKBRIEF.
      !>
      !>                       7) If both path endpoint frames are of CK type,
      !>                          and required SCLK data are present for only
      !>                          one of these frames, include a closing
      !>                          message explaining how CK coverage may be
      !>                          inadequate for a frame for which SCLK data
      !>                          are available, and recommending use of
      !>                          CKBRIEF.
      !>
      !>                       8) Omit the closing message if no path
      !>                          endpoint CK frame has associated SCLK
      !>                          data.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If a call to FRINFO or NAMFRM signals an error, this routine
      !>        will not be able to create a long error message. The
      !>        caller will not be able to diagnose the frame connection
      !>        failure, since an error condition will already exist.
      !>
      !>## Files
      !>
      !>     1) Each input frame ID argument will be mapped, if possible,
      !>        to a frame name. Any input frame ID that's not built in
      !>        must
      !>
      !>## Particulars
      !>
      !>     This routine centralizes creation of a long error message for
      !>     frame connection failures. This routine should be called
      !>     from:
      !>
      !>        FRMCHG
      !>        REFCHG
      !>        ZZFRMCH0
      !>        ZZFRMCH1
      !>        ZZREFCH0
      !>        ZZREFCH1
      !>
      !>## Examples
      !>
      !>     Below are some examples of messages created by this routine.
      !>
      !>    1)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
      !>         TDB), there is insufficient information available to
      !>         transform from reference frame -82000 (CASSINI_SC_COORD) to
      !>         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
      !>         a CK frame; a CK file containing data for instrument or
      !>         structure -82000 at the epoch shown above, as well as a
      !>         corresponding SCLK kernel, must be loaded in order to use
      !>         this frame. Frame MEX_SPACECRAFT could be transformed to
      !>         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK
      !>         file containing data for instrument or structure -41001 at
      !>         the epoch shown above, as well as a corresponding SCLK
      !>         kernel, must be loaded in order to use this frame. Failure to
      !>         find required CK data could be due to one or more CK files
      !>         not having been loaded, or to the epoch shown above lying
      !>         within a coverage gap or beyond the coverage bounds of the
      !>         loaded CK files. It is also possible that no loaded CK file
      !>         has required angular velocity data for the input epoch, even
      !>         if a loaded CK does have attitude data for that epoch. You
      !>         can use CKBRIEF with the -dump option to display coverage
      !>         intervals of a CK file.
      !>
      !>    2)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
      !>         TDB), there is insufficient information available to
      !>         transform from reference frame -82360 (CASSINI_ISS_NAC) to
      !>         reference frame 1 (J2000). Frame CASSINI_ISS_NAC could be
      !>         transformed to frame -82000 (CASSINI_SC_COORD). The latter is
      !>         a CK frame; a CK file containing data for instrument or
      !>         structure -82000 at the epoch shown above, as well as a
      !>         corresponding SCLK kernel, must be loaded in order to use
      !>         this frame. Failure to find required CK data could be due to
      !>         one or more CK files not having been loaded, or to the epoch
      !>         shown above lying within a coverage gap or beyond the
      !>         coverage bounds of the loaded CK files. It is also possible
      !>         that no loaded CK file has required angular velocity data for
      !>         the input epoch, even if a loaded CK does have attitude data
      !>         for that epoch. You can use CKBRIEF with the -dump option to
      !>
      !>     3)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
      !>         TDB), there is insufficient information available to
      !>         transform from reference frame -82000 (CASSINI_SC_COORD) to
      !>         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
      !>         a CK frame; a CK file containing data for instrument or
      !>         structure -82000 at the epoch shown above, as well as a
      !>         corresponding SCLK kernel, must be loaded in order to use
      !>         this frame. No SCLK kernel for instrument or structure
      !>         -82000, with corresponding SCLK ID -82, is currently loaded.
      !>         Frame MEX_SPACECRAFT could be transformed to frame -41001
      !>         (MEX_SC_REF). The latter is a CK frame; a CK file containing
      !>         data for instrument or structure -41001 at the epoch shown
      !>         above, as well as a corresponding SCLK kernel, must be loaded
      !>         in order to use this frame. No SCLK kernel for instrument or
      !>         structure -41001, with corresponding SCLK ID -41, is
      !>         currently loaded.
      !>
      !>     4)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
      !>         TDB), there is insufficient information available to
      !>         transform from reference frame -82000 (CASSINI_SC_COORD) to
      !>         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
      !>         a CK frame; a CK file containing data for instrument or
      !>         structure -82000 at the epoch shown above, as well as a
      !>         corresponding SCLK kernel, must be loaded in order to use
      !>         this frame. Frame MEX_SPACECRAFT could be transformed to
      !>         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK
      !>         file containing data for instrument or structure -41001 at
      !>         the epoch shown above, as well as a corresponding SCLK
      !>         kernel, must be loaded in order to use this frame. No SCLK
      !>         kernel for instrument or structure -41001, with corresponding
      !>         SCLK ID -41, is currently loaded. For a CK frame for which
      !>         the corresponding SCLK kernel has been loaded, failure to
      !>         find required CK data could be due to one or more CK files
      !>         not having been loaded, or to the epoch shown above lying
      !>         within a coverage gap or beyond the coverage bounds of the
      !>         loaded CK files. It is also possible that no loaded CK file
      !>         has required angular velocity data for the input epoch, even
      !>         if a loaded CK does have attitude data for that epoch. You
      !>         can use CKBRIEF with the -dump option to display coverage
      !>         intervals of a CK file.
      !>
      !>## Restrictions
      !>
      !>     1) This is a private routine. SPICE user applications should not
      !>        call this routine.
      !>
      !>     2) See exception (1) above.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 09-SEP-2013 (NJB)
      !>
      !>        Long error message for missing CK data now
      !>        mentions the possibility of missing angular
      !>        velocity data.
      !>
      !>-    SPICELIB Version 1.0.0, 14-DEC-2008 (NJB)
      module subroutine zznofcon ( ctx, et, frame1, endp1, frame2, endp2, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: suffix, repmc_inplace, repmi_inplace
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch.
      integer(ip), intent(in) :: frame1  !! "From" frame ID code.
      integer(ip), intent(in) :: endp1  !! "From" path endpoint frame ID code.
      integer(ip), intent(in) :: frame2  !! "To" frame ID code.
      integer(ip), intent(in) :: endp2  !! "To" path endpoint frame ID code.
      character(*), intent(out) :: errmsg  !! Long error message.


      integer(ip), parameter :: phrsln = 600

      type(string) :: bname ( 2 )
      logical :: namfnd
      type(string) :: name  ( 2 )
      character(phrsln) :: phrase
      character(timlen) :: timstr

      integer(ip) :: endps  ( 2 )
      integer(ip) :: center
      integer(ip) :: class
      integer(ip) :: clssid
      integer(ip) :: frames ( 2 )
      integer(ip) :: i
      integer(ip) :: sclkid

      logical :: ckmiss
      logical :: found
      logical :: havnam ( 2 )
      logical :: scmiss

!
!     Because this routine might cause a SPICE error to be
!     signaled, we have to check in.
!
      if ( ctx%failed() ) return

!
!     Capture input IDs in arrays.
!
      frames(1) = frame1
      frames(2) = frame2
      endps(1)  = endp1
      endps(2)  = endp2

!
!     The flags CKMISS and SCMISS are used, respectively, to
!     record whether any CK lookup failed due to missing CK
!     data or missing SCLK data. Each of these flags is turned
!     on if at least one lookup failed due to the indicated
!     cause.
!
      ckmiss = .false.
      scmiss = .false.

!
!     Get a string representation of the transformation epoch.
!
      call etcal ( ctx, et, timstr )

!
!     Get the names of the participating frames, if available.
!
      call frame_name_of ( ctx, frames(1), name(1)%s, namfnd )
      call frame_name_of ( ctx, frames(2), name(2)%s, namfnd )
      call frame_name_of ( ctx, endps(1), bname(1)%s, namfnd )
      call frame_name_of ( ctx, endps(2), bname(2)%s, namfnd )

      if ( ctx%failed() ) return

      do i = 1, 2

         if ( name(i)%s == ' ' ) then

            name(i)%s   = 'Name not available'
            havnam(i) = .false.
         else
            havnam(i) = .true.
         end if

         if ( bname(i)%s == ' ' ) then
            bname(i)%s = 'Name not available'
         end if

      end do

      call repmf ( ctx, 'At epoch # TDB (# TDB), there is ' &
                   //   'insufficient information available to ' &
                   //   'transform from reference frame # (@) to ' &
                   //   'reference frame # (@).', '#', et, 14, 'E', errmsg )
      call repmc_inplace ( errmsg, '#', timstr )

      do i = 1, 2

         call repmi_inplace ( errmsg, '#', frames(i) )
         call repmc_inplace ( errmsg, '@', name(i)%s )

      end do

!
!     For any frame graph longer than a single point, tell the user
!     the endpoint of the frame connection graph originating
!     at that frame.
!
      do i = 1, 2

         if ( frames(i) /= endps(i)  ) then

            phrase = 'Frame # could be transformed to frame # (@).'

            if ( havnam(i) ) then
               call repmc_inplace ( phrase, '#', name(i)%s )
            else
               call repmi_inplace ( phrase, '#', frames(i) )
            end if

            call repmi_inplace ( phrase, '#', endps(i) )
            call repmc_inplace ( phrase, '@', bname(i)%s )

            call suffix ( phrase, 1, errmsg )

!
!           The error messages below are appended only if they're not
!           redundant.
!
            if (  ( i == 1 )  .or.  ( endps(2) /= endps(1) )  ) then
!
!              For each endpoint frame, if that frame is of CK type,
!              indicate the instrument ID for which CK data are needed.
!
               call frinfo ( ctx, endps(i), center, class, clssid, found )

               if ( ctx%failed() ) return

               if ( found ) then

                  if ( class == ck ) then

                     phrase = 'The latter is a CK frame; a CK file ' &
                     //       'containing data for instrument or ' &
                     //       'structure # at the epoch shown above, ' &
                     //       'as well as a corresponding SCLK kernel, ' &
                     //       'must be loaded in order to use this ' &
                     //       'frame.'

                     call repmi_inplace ( phrase, '#', clssid )

                     call suffix ( phrase, 1, errmsg )

!
!                    Find out whether we have SCLK data for this
!                    CK ID.
!
                     call ckmeta ( ctx, clssid, 'SCLK', sclkid )

                     if (  .not. zzsclk ( ctx, clssid, sclkid )  ) then

                        scmiss = .true.

                        phrase = 'No SCLK kernel for instrument or ' &
                        //       'structure #, with corresponding ' &
                        //       'SCLK ID #, is currently loaded.'

                        call repmi_inplace ( phrase, '#', clssid )
                        call repmi_inplace ( phrase, '#', sclkid )

                        call suffix ( phrase, 1, errmsg )

                     else
!
!                       If we got here and have the SCLK data, then
!                       we don't have CK data.
!
                        ckmiss = .true.

                     end if

                  end if
!
!                 End of CK frame case.
!
               end if
!
!              End of "info found" case.
!
            end if
!
!           End of distinct frame case.
!

         else if (        ( i        == 1        ) &
                    .or.  ( endps(2) /= endps(1) )  ) then
!
!           The error messages below are appended only if they're not
!           redundant.
!
!           This graph has length one. If the frame comprising
!           this graph is a CK frame, generate a phrase
!           indicating the needed CK data.
!
            call frinfo ( ctx, frames(i), center, class, clssid, found )

            if ( ctx%failed() ) return

            if ( found ) then

               if ( class == ck ) then

                  phrase = '# is a CK frame; a CK file ' &
                  //       'containing data for instrument or ' &
                  //       'structure # at the epoch shown above, ' &
                  //       'as well as a corresponding SCLK kernel, ' &
                  //       'must be loaded in order to use this ' &
                  //       'frame.'

                  if ( havnam(i) ) then
                     call repmc_inplace ( phrase, '#', name(i)%s )
                  else
                     call repmi_inplace ( phrase, '#', frames(i) )
                  end if

                  call repmi_inplace ( phrase, '#', clssid )

                  call suffix ( phrase, 1, errmsg )

!
!                 Find out whether we have SCLK data for this
!                 CK ID.
!
                  call ckmeta ( ctx, clssid, 'SCLK', sclkid )

                  if (  .not. zzsclk ( ctx, clssid, sclkid )  ) then

                     scmiss = .true.

                     phrase = 'No SCLK kernel for instrument or ' &
                     //       'structure #, with corresponding ' &
                     //       'SCLK ID #, is currently loaded.'

                     call repmi_inplace ( phrase, '#', clssid )
                     call repmi_inplace ( phrase, '#', sclkid )

                     call suffix ( phrase, 1, errmsg )

                  else
!
!                    If we got here and have the SCLK data, then
!                    we don't have CK data.
!
                     ckmiss = .true.

                  end if

               end if
!
!              End of CK frame case.
!
            end if
!
!           End of "info found" case.
!
         end if
!
!        End of path length case.
!
      end do
!
!     End of path loop.
!

      if ( ckmiss ) then
!
!        At least one lookup failed due to missing CK data.
!
!        The informational message we include depends on whether we
!        also lack SCLK data.
!
         if ( scmiss ) then
!
!           We lack SCLK data for one frame and CK data for another.
!
            phrase = 'For a CK frame for which the corresponding SCLK ' &
            //       'kernel has been loaded, ' &
            //       'failure to find required CK data ' &
            //       'could be due to one or more CK files ' &
            //       'not having been loaded, or to the epoch ' &
            //       'shown above lying within a coverage gap or ' &
            //       'beyond the coverage bounds of the loaded CK ' &
            //       'files. It is also possible that no loaded CK ' &
            //       'file has required angular velocity data for ' &
            //       'the input epoch, even if a loaded CK ' &
            //       'does have attitude data for that epoch. ' &
            //       'You can use CKBRIEF with the -dump option ' &
            //       'to display coverage intervals of a CK file.'

         else
!
!           We have SCLK data but lack CK data.
!
            phrase = 'Failure to find required CK data ' &
            //       'could be due to one or more CK files ' &
            //       'not having been loaded, or to the epoch ' &
            //       'shown above lying within a coverage gap or ' &
            //       'beyond the coverage bounds of the loaded CK ' &
            //       'files. It is also possible that no loaded CK ' &
            //       'file has required angular velocity data for ' &
            //       'the input epoch, even if a loaded CK ' &
            //       'does have attitude data for that epoch. ' &
            //       'You can use CKBRIEF with the -dump option ' &
            //       'to display coverage intervals of a CK file.'

         end if

         call suffix ( phrase, 1, errmsg )

      end if

      return
      end subroutine zznofcon

      !> Fetch kernel variables for a specified switch frame. Data
      !> for the new frame are added to the switch frame database.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRAMID      is the integer ID code for a switch reference frame
      !>                 for which kernel pool data are to be fetched.
      !>
      !>     HDFRAM      is an array containing head nodes of hash collision
      !>                 lists in FRPOOL.
      !>
      !>     FRPOOL      is a singly linked list pool containing collision
      !>                 lists corresponding to hash values.
      !>
      !>     FIDLST      is an array of switch frame ID codes. The Ith element
      !>                 of FRPOOL corresponds to the ID code at index I in
      !>                 FIDLST.
      !>
      !>     BASBEG      is an array of start indices of base frame lists and
      !>                 base frame attributes. The Ith element of BASBEG is
      !>                 the start index in BASLST of the base frame list of
      !>                 the switch frame having ID at index I in FIDLST. The
      !>                 attributes of the base frame at a given index in
      !>                 BASLST are stored at the same index in parallel
      !>                 arrays.
      !>
      !>     FREE        is the index of the first free element in the array
      !>                 BASLST. This is also the index of the first free
      !>                 elements in the parallel arrays CLSSES, CLSIDS,
      !>                 STARTS and STOPS.
      !>
      !>     BASCNT      is an array of counts of base frames associated with
      !>                 switch frames. The Ith element of BASCNT is the base
      !>                 frame count of the switch frame having ID at index I
      !>                 in FIDLST.
      !>
      !>     USETIM      is an array of logical flags indicating the existence
      !>                 of time intervals associated with the base frames of
      !>                 a given switch frame. The flag at index I of USETIM
      !>                 is .TRUE. if and only if the switch frame having ID
      !>                 at index I of FIDLST has time intervals associated
      !>                 with its base frames.
      !>
      !>     BINARY      is an array of logical flags indicating the
      !>                 eligibility of a given switch frame for use of a
      !>                 binary search to locate the base frame time interval
      !>                 containing a given request time. Eligibility means
      !>                 that time intervals exist and that the stop time of
      !>                 the interval at index J is less than or equal to the
      !>                 start time at index J+1. The flag at index I of
      !>                 BINARY is .TRUE. if and only if the switch frame at
      !>                 index I of FIDLST is eligible for binary search.
      !>
      !>     CLSSES      is an array of classes of base frames. The element at
      !>                 index I is the class of the base frame at index I of
      !>                 BASLST, if that element of BASLST is defined.
      !>
      !>     CLSIDS      is of frame class IDs of base frames. The element
      !>                 at index I is the frame class ID of the base frame
      !>                 at index I of BASLST, if that element of BASLST is
      !>                 defined.
      !>
      !>     BASLST      is an array of ID codes of base frames associated
      !>                 with switch frames. The base frame having ID at index
      !>                 BASBEG(I)-1+J of BASLST is the Jth base frame of the
      !>                 switch frame having frame ID at index I of FIDLST.
      !>
      !>     STARTS      is an array of base frame applicability interval
      !>                 start times. The Ith element of STARTS is the start
      !>                 time of the base frame at index I of BASLST, if that
      !>                 element is defined and that frame has associated time
      !>                 intervals. For a given switch frame, all bases have
      !>                 associated time intervals or none do.
      !>
      !>     STOPS       is an array of base frame applicability interval
      !>                 start times. The Ith element of STARTS is the start
      !>                 time of the base frame at index I of BASLST, if that
      !>                 element is defined and that frame has associated time
      !>                 intervals. For a given switch frame, all bases have
      !>                 associated time intervals or none do.
      !>
      !>## Detailed Output
      !>
      !>     HDFRAM      is the input collision list head node array, updated
      !>                 if necessary to reflect the addition of a node for
      !>                 the switch frame designated by the input frame ID.
      !>
      !>     FRPOOL      is the hash collision list pool, updated to reflect
      !>                 the addition of a node for the switch frame
      !>                 designated by the input frame ID.
      !>
      !>     FIDLST      is the switch frame ID list, updated to reflect the
      !>                 addition of the input frame ID.
      !>
      !>     BASBEG      is the input base frame head list, updated to reflect
      !>                 the addition of an entry for the switch frame
      !>                 designated by the input frame ID.
      !>
      !>     FREE        is the common index in BASLST and its parallel arrays
      !>                 of their first free elements, following addition of
      !>                 data associated with the input frame ID.
      !>
      !>     BASCNT      is the input base frame count list, updated to
      !>                 reflect the addition of the base frames associated
      !>                 with the input frame ID.
      !>
      !>     USETIM      is the input time interval presence flag array,
      !>                 updated to reflect the addition of the flag for the
      !>                 the switch frame designated by the input frame ID.
      !>
      !>     BINARY      is the input binary search eligibility array, updated
      !>                 to reflect the addition of the flag for the switch
      !>                 frame designated by the input frame ID.
      !>
      !>     CLSSES      is the input class array, updated to reflect the
      !>                 addition of base frame classes for the switch frame
      !>                 designated by the input frame ID.
      !>
      !>     CLSIDS      is the input frame class ID array, updated to reflect
      !>                 the addition of base frame frame class IDs for the
      !>                 switch frame designated by the input frame ID.
      !>
      !>     BASLST      is the input base frame array, updated to reflect
      !>                 the addition of base frames associated with the
      !>                 switch frame designated by the input frame ID.
      !>
      !>     STARTS      is the input base frame interval start time array,
      !>                 updated to reflect the addition of base frames
      !>                 associated with the switch frame designated by the
      !>                 input frame ID.
      !>
      !>     STOPS       is the input base frame interval stop time array,
      !>                 updated to reflect the addition of base frames
      !>                 associated with the switch frame designated by the
      !>                 input frame ID.
      !>
      !>     FRAMAT      is the common index in the arrays
      !>
      !>                    FIDLST, BASBEG, BASCNT, USETIM, BINARY
      !>
      !>                 of information associated with the switch frame
      !>                 designated by the input frame ID.
      !>
      !>## Parameters
      !>
      !>     LBSNGL      is the lower bound of a singly linked list pool
      !>                 managed by the integer hash subsystem.
      !>
      !>     See the include file zzswtchf.inc for SPICE-private parameters
      !>     defining sizes of buffers used by the switch frame subsystem.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input frame ID maps to a frame specification for which
      !>         the ID doesn't match, the error SPICE(BADFRAMESPEC) is
      !>         signaled.
      !>
      !>     2)  If kernel variables for any of the universal frame
      !>         specification items
      !>
      !>            frame ID
      !>            frame name
      !>            frame center
      !>            frame class
      !>            frame class ID
      !>
      !>         or the switch frame item
      !>
      !>            base frame list
      !>
      !>        are not found, the error SPICE(MISSINGFRAMEVAR) is signaled.
      !>
      !>     3) If the ID of a base frame cannot be mapped to a name,
      !>        the error SPICE(FRAMENAMENOTFOUND) is signaled.
      !>
      !>     4) If the stop time for a given interval is not strictly greater
      !>        than the corresponding start time, the error
      !>        SPICE(BADTIMEBOUNDS) is signaled.
      !>
      !>     5) If only one of the start time or stop time kernel variables
      !>        is supplied, the error SPICE(PARTIALFRAMESPEC) is signaled.
      !>
      !>     6) If the count of start times doesn't match the count of
      !>        stop times, or if either count doesn't match the count of
      !>        base frames, the error SPICE(COUNTMISMATCH) is signaled.
      !>
      !>     7) If an error occurs during kernel variable lookup, the error
      !>        is signaled by a routine in the call tree of this routine.
      !>
      !>     8) If an error occurs during time conversion, the error
      !>        is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     Appropriate kernels must be loaded by the calling program before
      !>     this routine is called.
      !>
      !>     The following data are required:
      !>
      !>         - A switch frame specification, normally provided by a
      !>           frame kernel.
      !>
      !>              > Base frames may be specified by name or frame ID.
      !>
      !>              > Time interval bounds may be specified by singly
      !>                quoted strings or by data using the text kernel "@"
      !>                syntax.
      !>
      !>         - Frame specifications for base frames. Normally these
      !>           specifications are provided by frame kernels.
      !>
      !>           Even though specifications of frames identified by ID code
      !>           are not explicitly required by this routine, they are
      !>           required by the intended caller of this routine ZZSWFXFM.
      !>
      !>         - A leapseconds kernel, if time interval bounds are
      !>           specified by time strings.
      !>
      !>     In all cases, kernel data are normally loaded once per program
      !>     run, NOT every time this routine is called.
      !>
      !>## Particulars
      !>
      !>     This routine obtains switch frame specifications from the kernel
      !>     pool and adds them to the switch frame database.
      !>
      !>     If, upon entry, the database already contains the maximum number
      !>     of switch frames, the database will be cleared in order to make
      !>     room for the new switch frame specification.
      !>
      !>     If, upon entry, the database doesn't have enough room to buffer
      !>     the base frame data associated with a new switch frame, the
      !>     database will be cleared in order to make room for the new switch
      !>     frame specification.
      !>
      !>     Complete error checking of the new switch frame specification is
      !>     performed on each call.
      !>
      !>## Examples
      !>
      !>     None. See usage in ZZSWFXFM.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine is SPICE-private. User applications must not
      !>         call it.
      !>
      !>     2)  To improve efficiency, this routine buffers switch frame
      !>         specifications. The buffers are emptied after any update
      !>         to the kernel pool. Since the process of parsing, and
      !>         buffering parameters provided by, switch frame specifications
      !>         is rather slow, applications using switch frames should
      !>         avoid high-frequency kernel pool updates.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     B.V. Semenov   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS)
      module subroutine zzswffet ( ctx, framid, hdfram, frpool, fidlst, basbeg, free, bascnt, usetim, binary, clsses, clsids, baslst, starts, stops, framat, nroom )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc
      use pepper_inc_zzswtchf
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: framid  !! The frame ID of a switch frame.
      integer(ip), intent(inout) :: hdfram ( * )  !! Array of hash collision list head nodes.
      integer(ip), intent(inout) :: frpool ( lbsngl : * )  !! Hash collision list pool.
      integer(ip), intent(inout) :: fidlst ( * )  !! Frame IDs corresponding to list nodes.
      integer(ip), intent(inout) :: basbeg ( * )  !! Start indices of base lists of switch frames.
      integer(ip), intent(inout) :: free  !! First free index in base frame arrays.
      integer(ip), intent(inout) :: bascnt ( * )  !! Counts of base frames for each switch frame.
      logical, intent(inout) :: usetim ( * )  !! Logical flags indicating presence of time bounds.
      logical, intent(inout) :: binary ( * )  !! Logical flags indicating binary search usage.
      integer(ip), intent(inout) :: clsses ( * )  !! Classes of base frames.
      integer(ip), intent(inout) :: clsids ( * )  !! Frame class IDs of base frames.
      integer(ip), intent(inout) :: baslst ( * )  !! Array containing lists of base frame IDs.
      real(wp), intent(inout) :: starts ( * )  !! Lists of interval start times.
      real(wp), intent(inout) :: stops ( * )  !! Lists of interval stop times.
      integer(ip), intent(out) :: framat  !! Index in FIDLST of input switch frame ID.
      integer(ip), intent(in), optional :: nroom  !! PEPPER: size of the base frame arrays (default MAXBAS).
      integer(ip) :: cap
      integer(ip) :: fid_1(1)  !! one-element copy of fid (array argument)
      integer(ip) :: frclid_1(1)  !! one-element copy of frclid (array argument)
      integer(ip) :: frclas_1(1)  !! one-element copy of frclas (array argument)

!
!     Kernel variable name templates:
!
      character(*), parameter :: tmalgn = 'FRAME_#_ALIGNED_WITH'

      character(*), parameter :: tmcent = 'FRAME_#_CENTER'

      character(*), parameter :: tmclas = 'FRAME_#_CLASS'

      character(*), parameter :: tmclid = 'FRAME_#_CLASS_ID'


      character(*), parameter :: tmfnam = 'FRAME_#_NAME'

      character(*), parameter :: tmstrt = 'FRAME_#_START'

      character(*), parameter :: tmstop = 'FRAME_#_STOP'

!
!     Other parameters
!

      integer(ip), parameter :: timlen = 80

!
!     Indices of items in the arrays of kernel variables:
!
      integer(ip), parameter :: ixfnam = 1

      integer(ip), parameter :: ixid = ixfnam + 1

      integer(ip), parameter :: ixcent = ixid    + 1

      integer(ip), parameter :: ixclas = ixcent + 1

      integer(ip), parameter :: ixclid = ixclas + 1

      integer(ip), parameter :: ixalgn = ixclid + 1

      integer(ip), parameter :: ixstrt = ixalgn + 1

      integer(ip), parameter :: ixstop = ixstrt + 1

      integer(ip), parameter :: kvnmln = 32

      integer(ip), parameter :: nkv = 8

      character(:), allocatable :: basnam
      character(1) :: bastyp
      character(:), allocatable :: frname
      character(kvnmln) :: kvalgn
      character(kvnmln) :: kvclid
      character(kvnmln) :: kvcent
      character(kvnmln) :: kvclas
      character(:), allocatable :: kvid
      character(kvnmln) :: kvfnam
      type(string) :: kvnams ( nkv )
      character(kvnmln) :: kvstrt
      character(kvnmln) :: kvstop
      character(1) :: stptyp
      character(1) :: strtyp
      character(timlen) :: timstr

      integer(ip) :: center
      integer(ip) :: fid
      integer(ip) :: frcent
      integer(ip) :: frclas
      integer(ip) :: frclid
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: nbases
      integer(ip) :: nfravl
      integer(ip) :: nstart
      integer(ip) :: nstop
      integer(ip) :: room

!
!     KVFND(*) indicates whether a specified kernel variable was found.
!
      logical :: havtim
      logical :: inffnd
      logical :: kvfnd ( nkv )
      logical :: new

      logical :: strxst
      logical :: stpxst

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     No result found yet.
!
      framat = 0

!
!     Create names of kernel variables, using the input frame ID.
!
!     Basic frame specification variables, excluding that for the
!     frame ID. We'll need the frame name before we can create the
!     name of the kernel variable for the frame ID.
!
      call repmi( ctx, tmcent, '#', framid, kvcent )
      call repmi( ctx, tmclas, '#', framid, kvclas )
      call repmi( ctx, tmclid, '#', framid, kvclid )
      call repmi( ctx, tmfnam, '#', framid, kvfnam )

!
!     Switch frame specification variables:
!
      call repmi( ctx, tmalgn, '#', framid, kvalgn )
      call repmi( ctx, tmstrt, '#', framid, kvstrt )
      call repmi( ctx, tmstop, '#', framid, kvstop )

!
!     Below, we use logical flags to describe the status
!     of kernel variable availability:
!
!         KVFND(*) indicates that a variable exists.
!
!     Look up the basic variables.
!
      call gipool ( ctx, kvclas, 1, 1, n, frclas_1, kvfnd(ixclas) )
      frclas = frclas_1(1)
      call gipool ( ctx, kvclid, 1, 1, n, frclid_1, kvfnd(ixclid) )
      frclid = frclid_1(1)
      call ctx%pool_string ( kvfnam, 1_ip, frname, kvfnd(ixfnam) )
      kvid = ' '

      if ( ctx%failed() ) then

         call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

         return

      end if

!
!     Fetch the switch frame ID variable.
!
      if ( kvfnd(ixfnam) ) then
!
!        Use the frame name to fetch the switch frame ID variable.
!
         kvid = 'FRAME_' // trim ( adjustl ( frname ) )

         call gipool ( ctx, kvid, 1, 1, n, fid_1, kvfnd(ixid) )
         fid = fid_1(1)

         if ( ctx%failed() ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            return

         end if

!
!        The frame ID of the frame specification had better match
!        the input frame ID.
!
         if ( kvfnd(ixid) .and. ( fid /= framid ) ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'Input frame ID was #, but ID in frame ' &
            //            'specification from kernel pool was #. ' )
            call ctx%errint ( '#', framid           )
            call ctx%errint ( '#', fid              )
            call ctx%sigerr ( 'SPICE(BADFRAMESPEC)' )
            return

         end if
!
!        The frame name must always be found. If not, the error
!        will be diagnosed in the block below.
!
      else
!
!        We couldn't find the frame name. Indicate the frame ID
!        variable [sic] wasn't found, since its found flag won't be set
!        by GIPOOL.
!
         kvfnd(ixid) = .false.

      end if

!
!     Look up the central body of the frame. The name of the kernel
!     variable for the body could refer to the frame by name or frame
!     ID; the body itself could be specified by name or body ID.
!
      if ( kvfnd(ixfnam) ) then

         call zzdynbid ( ctx, frname, framid, 'CENTER', frcent )

         if ( ctx%failed() ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            return

         end if

         kvfnd(ixcent) = .true.
      else
         kvfnd(ixcent) = .false.
      end if

!
!     Look up the type and count of the base frame list. It
!     may have either string or numeric type.
!
      call dtpool( ctx, kvalgn, kvfnd(ixalgn), nbases, bastyp )

      if ( ctx%failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

         return

      end if

!
!     Store the kernel variable names in order to prepare for
!     checking availability of required variables.
!
      kvnams(ixid)%s = kvid
      kvnams(ixfnam)%s = kvfnam
      kvnams(ixcent)%s = kvcent
      kvnams(ixclas)%s = kvclas
      kvnams(ixclid)%s = kvclid
      kvnams(ixalgn)%s = kvalgn

!
!     Check for required variables that haven't been supplied.
!
      do i = 1, ixalgn
!
!        The first 6 items are needed; start and stop times are needed
!        if at least one is present.
!
         if ( .not. kvfnd(i) ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'Kernel variable #, needed for ' &
            //            'specification of switch frame having ' &
            //            'frame ID #, was not ' &
            //            'found in the kernel pool. This can occur ' &
            //            'when a frame kernel providing the required ' &
            //            'switch frame specification has not been ' &
            //            'loaded, or if the specification is present ' &
            //            'but is incorrect.'                         )
            call ctx%errch  ( '#', kvnams(i)%s                            )
            call ctx%errint ( '#', framid                                 )
            call ctx%sigerr ( 'SPICE(MISSINGFRAMEVAR)'                    )
            return

         end if

      end do

!
!     Find out whether time bounds are supplied, and if so, which data
!     type is used to represent them.
!
      kvnams(ixstrt)%s = kvstrt
      kvnams(ixstop)%s = kvstop

      call dtpool( ctx, kvstrt, kvfnd(ixstrt), nstart, strtyp )
      call dtpool( ctx, kvstop, kvfnd(ixstop), nstop,  stptyp )

      if ( ctx%failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

         return

      end if

!
!     Availability of starts and stops is optional...however both
!     must be provided if either is, and the counts must match
!     those of the base frames.
!
!     Note that it's necessary to perform these checks before buffering
!     base frame data, since that process assumes that the number of
!     start and stop times is either zero or matches the number of
!     base frames.
!
      strxst = kvfnd(ixstrt)
      stpxst = kvfnd(ixstop)

      havtim = strxst .and. stpxst

      if ( havtim ) then
!
!        Make sure that counts of base frames, start times, and
!        stop times are equal.
!
         if ( ( nstart /= nstop ) .or. ( nstart /= nbases ) ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'Kernel variables for the switch frame ' &
            //            'having frame ID # have mismatched sizes: ' &
            //            'number of base frames = #; number of start ' &
            //            'times = #; number of stop times = #.'      )
            call ctx%errint ( '#', framid            )
            call ctx%errint ( '#', nbases            )
            call ctx%errint ( '#', nstart            )
            call ctx%errint ( '#', nstop             )
            call ctx%sigerr ( 'SPICE(COUNTMISMATCH)' )
            return

         end if

      else
!
!        Check for inconsistent presence of start and stop times.
!
         if ( strxst .or. stpxst ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

!
!           Create long message template to be filled in.
!
            call ctx%setmsg ( 'Kernel variable #, which specifies base ' &
            //            'frame applicability # times, was not ' &
            //            'provided for the switch frame having ' &
            //            'frame ID #, while the kernel variable # ' &
            //            'specifying base frame applicability ' &
            //            '# times was provided. Switch frame ' &
            //            'applicability start and stop times are ' &
            //            'optional, but both must be provided if ' &
            //            'either is.'  )

            if ( strxst ) then
!
!              Stop times are missing.
!
               call ctx%errch  ( '#', kvstop  )
               call ctx%errch  ( '#', 'stop'  )
               call ctx%errint ( '#', framid  )
               call ctx%errch  ( '#', kvstrt  )
               call ctx%errch  ( '#', 'start' )

            else
!
!              Start times are missing.
!
               call ctx%errch  ( '#', kvstrt  )
               call ctx%errch  ( '#', 'start' )
               call ctx%errint ( '#', framid  )
               call ctx%errch  ( '#', kvstop  )
               call ctx%errch  ( '#', 'stop'  )

            end if

            call ctx%sigerr ( 'SPICE(PARTIALFRAMESPEC)' )
            return

         end if

      end if

!
!     Compute room for the output data.
!
!     PEPPER: the size of the base frame arrays is NROOM if given (the
!     library's arrays grow); MAXBAS otherwise.
!
      cap = maxbas
      if ( present ( nroom ) ) cap = nroom
      room = cap + 1 - free

!
!     Check the available room in the frame ID pool and in the frame
!     base arrays.
!
      call zzhsiavl ( ctx, frpool, nfravl )

      if ( ( nfravl == 0 ) .or. ( room < nbases ) ) then
!
!        There's no room for another frame in the frame pool, or
!        there's no room for the new frame's data in the base
!        frame arrays.
!
!        If we can make enough room by re-initializing the whole
!        local frame database, do so. If the new frame has too
!        much data to fit in the empty base frame arrays, that's
!        an error.
!
         if ( nbases <= cap ) then
!
!           We can fit the frame data in by clearing out the
!           database.
!
            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

!
!           The database is now initialized.
!
            free = 1
            room = cap

         else
!
!           We can't make enough room.
!
!           Initialize local data structures, for safety.
!
            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'The requested frame # has # associated ' &
            //            'base frames. The maximum number that ' &
            //            'can be supported is #.'   )
            call ctx%errint ( '#', framid                )
            call ctx%errint ( '#', nbases                )
            call ctx%errint ( '#', cap                   )
            call ctx%sigerr ( 'SPICE(TOOMANYBASEFRAMES)' )
            return

         end if

      end if

!
!     Add the frame and its data to the local database.
!
!     Start by adding the frame ID to the frame hash structure.
!     The output argument NEW indicates whether the item is new;
!     we don't need to check this argument. We also don't need to
!     check for failure of ZZHSIADD, since we've already ensured
!     there's room in the hash for a new clock.
!
      call zzhsiadd ( ctx, hdfram, frpool, fidlst, framid, framat, new )

!
!     At this point, FRAMAT is set.
!
!     Store the frame ID, base start index, base count, and time
!     interval availability attributes for this frame.
!
      fidlst( framat ) = framid
      basbeg( framat ) = free
      bascnt( framat ) = nbases
      usetim( framat ) = havtim

!
!     Look up the base frame variables. The base frame list may have
!     either string or numeric type.
!
      kvfnd(ixalgn) = .false.

      if ( bastyp == 'N' ) then
!
!        The frames are specified by ID code. We can buffer the frame
!        ID codes immediately.
!
         call gipool ( ctx, kvalgn,       1,            room, &
                       bascnt(free), baslst(free), kvfnd(ixalgn) )

         if ( ctx%failed() ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            return

         end if

      else if ( bastyp == 'C' ) then
!
!        The frames are specified by name. We must convert the
!        names to frame ID codes before we can can buffer the IDs.
!
         kvfnd(ixalgn) = .false.

         do i = 1, nbases

            call ctx%pool_string ( kvalgn, i, basnam, kvfnd(ixalgn) )

            if ( ctx%failed() ) then
!
!              This code should be unreachable but is provided for
!              safety.
!
               call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

               return

            end if

            call namfrm( ctx, basnam, baslst(free-1+i) )

            if ( baslst(free-1+i) == 0 ) then

               call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

               call ctx%setmsg( 'Base frame name # of switch frame ' &
               //           '# could not be translated to a ' &
               //           'frame ID code '           )
               call ctx%errch ( '#', basnam                )
               call ctx%errint( '#', framid                )
               call ctx%sigerr( 'SPICE(FRAMENAMENOTFOUND)' )
               return

            end if

         end do

      else
!
!        Backstop: this code should be unreachable.
!
         call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

         call ctx%setmsg ( 'Base frame kernel variable # exists but ' &
         //            'DTPOOL returned data type # rather than ' &
         //            'one of the expected values: ''C'' or ''N''.' )
         call ctx%errch  ( '#', kvalgn  )
         call ctx%errch  ( '#', bastyp  )
         call ctx%sigerr ( 'SPICE(BUG)' )
         return

      end if

!
!     Get attributes of each base frame.
!
      do i = 1, nbases
!
!        Store the frame class and frame class ID for this base frame.
!
         j = free - 1 + i

         call frinfo ( ctx, baslst(j), center, &
                       clsses(j), clsids(j), inffnd )

         if ( ctx%failed() ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            return

         end if

         if ( .not. inffnd ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'No specification was found for ' &
            //            'base frame # of switch frame #.' )
            call ctx%errint ( '#', baslst(j)                    )
            call ctx%errint ( '#', framid                       )
            call ctx%sigerr ( 'SPICE(FRAMEINFONOTFOUND)'        )
            return

         end if

      end do

!
!     Fetch interval bounds if they're present.
!
      if ( usetim(framat) ) then
!
!        Fetch start times.
!
!        Note that DTPOOL sets the type to 'X' if the target
!        kernel variable is not found.
!
         if ( strtyp == 'N' ) then
!
!           The start times are represented by double precision
!           numbers.
!
            call gdpool( ctx, kvstrt, 1,            room, &
                         nstart, starts(free), kvfnd(ixstrt) )

         else if ( strtyp == 'C' ) then
!
!           The start times are represented by strings.
!
!           We must convert each string to a numeric value and store
!           the value in the start time buffer.
!
            kvfnd(ixstrt) = .false.

            do i = 1, nbases

               call gcpool( ctx, kvstrt,  i,      1, &
                            n,       timstr, kvfnd(ixstrt) )

               call str2et( ctx, timstr, starts(free-1+i) )

            end do

         else
!
!           Backstop: this code should be unreachable.
!
            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'Start time kernel variable # exists but ' &
            //            'DTPOOL returned data type # rather than ' &
            //            'one of the expected values: ''C'' ' &
            //            'or ''N''.' )
            call ctx%errch  ( '#', kvstrt  )
            call ctx%errch  ( '#', strtyp  )
            call ctx%sigerr ( 'SPICE(BUG)' )
            return

         end if

         if ( ctx%failed() ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            return

         end if

!
!        Fetch stop times.
!
         if ( stptyp == 'N' ) then
!
!           The stop times are represented by double precision numbers.
!
            call gdpool( ctx, kvstop, 1,           room, &
                         nstop,  stops(free), kvfnd(ixstop) )

         else if ( stptyp == 'C' ) then
!
!           The stop times are represented by strings.
!
!           We must convert each string to a numeric value and store
!           the value in the stop time buffer.
!
            kvfnd(ixstop) = .false.

            do i = 1, nstop

               call gcpool( ctx, kvstop,  i,      1, &
                            n,       timstr, kvfnd(ixstop) )

               call str2et( ctx, timstr, stops(free-1+i) )

            end do

         else
!
!           Backstop: this code should be unreachable.
!
            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            call ctx%setmsg ( 'Stop time kernel variable # exists but ' &
            //            'DTPOOL returned data type # rather than ' &
            //            'one of the expected values: ''C'' ' &
            //            'or ''N''.' )
            call ctx%errch  ( '#', kvstop  )
            call ctx%errch  ( '#', stptyp  )
            call ctx%sigerr ( 'SPICE(BUG)' )
            return

         end if

         if ( ctx%failed() ) then

            call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

            return
         end if

!
!        Singleton intervals and out-of-order intervals are not
!        allowed.
!
         do i = 1, nstart

            j = free - 1 + i

            if ( starts(j) >= stops(j) ) then

               call zzswfcln ( ctx, hdfram, frpool, basbeg, framat )

               call ctx%setmsg ( 'Interval time bounds are not strictly ' &
               //            'increasing at interval index # for ' &
               //            'switch frame #. Time bounds are #:# TDB ' &
               //            '(# TDB : # TDB)'      )
               call ctx%errint ( '#', i                 )
               call ctx%errint ( '#', framid            )
               call ctx%errdp  ( '#', starts(j)         )
               call ctx%errdp  ( '#', stops(j)          )
               call etcal  ( ctx, starts(j), timstr      )
               call ctx%errch  ( '#',       timstr      )
               call etcal  ( ctx, stops(j),  timstr      )
               call ctx%errch  ( '#',       timstr      )
               call ctx%sigerr ( 'SPICE(BADTIMEBOUNDS)' )
               return

            end if

         end do

      end if

!
!     Determine whether binary search on the time intervals is
!     possible.
!
      if ( usetim(framat) ) then

         i = 2
         binary( framat ) = .true.

         do while (  ( i <= nbases ) .and.  binary(framat)  )
!
!           Note that proper ordering of start and stop times for
!           each interval has already been verified.
!
            j = basbeg(framat) - 1 + i

            if ( stops(j-1) > starts(j) ) then

               binary( framat ) = .false.

            end if

            i = i + 1

         end do

      else
         binary( framat ) = .false.
      end if

!
!     We've found the data. The switch frame database has been
!     updated. FRAMAT is already set.
!
!     Account for the base frame storage used.
!
      free = free + nbases

      return
      end subroutine zzswffet

      !> J2000 to TEME, and TEME to J2000, probably.
      !>
      !>## Detailed Input
      !>
      !>     ET         time expressed in terms of TDB seconds past epoch J2000
      !>                for which to to calculate the J2000 frame to TEME frame
      !>                transformation operator.
      !>
      !>## Detailed Output
      !>
      !>     J2TM       the 6x6 operator that transforms cartesian states from
      !>                the J2000 to TEME reference frame.
      !>
      !>     TM2J       the 6x6 operator that transforms cartesian states from
      !>                the TEME to J2000 reference frame.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     TETE    True equator true equinox
      !>     MEME    Mean equator mean equinox
      !>     TEME    True equator mean equinox
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
      !>     N.J. Bachman   (JPL)
      !>     S.C. Krening   (JPL)
      !>     E.D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 10-OCT-2021 (EDW)
      !>
      !>        Routine now returns TEME to J2000 mapping operator.
      !>
      !>-    SPICELIB Version 1.0.0, 06-MAR-2012 (NJB) (SCK) (EDW)
      module subroutine zzteme ( ctx, et, j2tm, tm2j )
      use pepper_kinds, only: wp
      use pepper_math, only: mxvg
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch of the state transformation operator.
      real(wp), intent(out) :: j2tm    ( 6, 6 )  !! The state transformation operator J2000 to TEME.
      real(wp), intent(out) :: tm2j    ( 6, 6 )  !! The state transformation operator TEME to J2000.

      real(wp) :: m1     ( 6, 6 )
      real(wp) :: m1inv  ( 6, 6 )
      real(wp) :: m2     ( 6, 6 )
      real(wp) :: m2inv  ( 6, 6 )
      real(wp) :: xj2000 ( 6 )
      real(wp) :: z      ( 6 )
      real(wp) :: zj2000 ( 6 )

      if ( ctx%failed() ) return

!
!     Extract the MEME +X vector and its derivative, both
!     expressed relative to the J2000 frame, into X.
!
      call zzeprc76( ctx, et, m1    )
      call invstm  ( ctx, m1, m1inv )

      xj2000 = m1inv(:,1)

!
!     Extract the TETE +Z vector and its derivative, both
!     expressed relative to the MEME frame, into Z.
!
      call zzenut80( ctx, et, m2    )
      call invstm  ( ctx, m2, m2inv )

      z = m2inv(:,3)

!
!     Transform Z to the J2000 frame.
!
      call mxvg ( m1inv, z, 6, 6, zj2000 )

!
!     Compute the TEME to J2000 state transformation, and the
!     inverse.
!
      call zztwovxf ( ctx, zj2000, 3, xj2000, 1, tm2j )
      call invstm   ( ctx, tm2j,  j2tm )

      return
      end subroutine zzteme

      !> Convert ephemeris seconds past J2000 (ET) to continuous encoded
      !> spacecraft clock (`ticks').  Non-integral tick values may be
      !> returned.
      !>
      !>## Detailed Input
      !>
      !>     SC       is a NAIF integer code for a spacecraft whose
      !>              encoded SCLK value at the epoch specified by ET is
      !>              desired.
      !>
      !>     ET       is an epoch, specified as ephemeris seconds past
      !>              J2000.
      !>
      !>## Detailed Output
      !>
      !>     SCLKDP   is an encoded spacecraft clock value. SCLKDP is
      !>              an encoded representation of the total number
      !>              of spacecraft clock ticks measured from the time
      !>              the spacecraft clock started to the epoch ET:
      !>              partition information IS reflected in the encoded
      !>              value.
      !>
      !>              SCLKDP may be non-integral:  SCLKDP is NOT
      !>              rounded to the nearest whole tick.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an SCLK kernel has not been loaded, does not contain all of
      !>         the required data, or contains invalid data, an error is
      !>         signaled by a routine in the call tree of this routine. The
      !>         output argument SCLKDP will not be modified. This routine
      !>         assumes that that an SCLK kernel appropriate to the spacecraft
      !>         clock identified by the input argument SC has been loaded.
      !>
      !>     2)  If a leapseconds kernel is required for conversion between
      !>         SCLK and ET but is not loaded, an error is signaled by a
      !>         routine in the call tree of this routine. The output argument
      !>         SCLKDP will not be modified. When using SCLK kernels that map
      !>         SCLK to a time system other than ET (also called barycentric
      !>         dynamical time---`TDB'), it is necessary to have a leapseconds
      !>         kernel loaded at the time this routine is called.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If the clock type for the spacecraft clock identified by
      !>         SC is not supported by this routine, the error
      !>         SPICE(NOTSUPPORTED) is signaled. The output argument SCLKDP
      !>         will not be modified.
      !>
      !>     4)  If the input ET value is not representable as an encoded
      !>         spacecraft clock value for the spacecraft clock identified by
      !>         SC, an error is signaled by a routine in the call tree of this
      !>         routine. The output argument SCLKDP will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine outputs continuous encoded SCLK values; unlike the
      !>     routine SCE2T, the values output by this routine need not be
      !>     integral.
      !>
      !>     This routine supports use of non-integral encoded clock values in
      !>     C-kernels:  non-integral clock values may be stored as pointing
      !>     time tags when a C-kernel is created, and they may be supplied
      !>     as request times to the C-kernel readers.
      !>
      !>     The advantage of encoded SCLK, as opposed to character string
      !>     representations of SCLK, is that encoded SCLK values are easy to
      !>     perform arithmetic operations on. Also, working with encoded SCLK
      !>     reduces the overhead of repeated conversion of  character strings
      !>     to integers or double precision numbers.
      !>
      !>     To convert ET to a string representation of an SCLK value, use
      !>     the SPICELIB routine SCE2S.
      !>
      !>     See the SCLK Required Reading for a list of the entire set of
      !>     SCLK conversion routines.
      !>
      !>## Examples
      !>
      !>     1)  Convert ET directly to an encoded SCLK value; use both of
      !>         these time values to look up both C-kernel (pointing) and
      !>         SPK (position and velocity) data for an epoch specified by an
      !>         ephemeris time.
      !>
      !>            During program initialization, load the leapseconds and
      !>            SCLK kernels. We will pretend that these files are named
      !>            "LEAPSECONDS.KER" and "GLLSCLK.KER".  To use this code
      !>            fragment, you must substitute the actual names of these
      !>            kernel files for the names used here.
      !>
      !>               C
      !>               C     Load leapseconds and SCLK kernels:
      !>               C
      !>                     CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>                     CALL FURNSH ( 'GLLSCLK.KER'     )
      !>
      !>            The mission is Galileo, which has spacecraft ID -77.
      !>            Let ET be the epoch, specified in ephemeris seconds
      !>            past J2000, at which both position and pointing data
      !>            are desired.
      !>
      !>            Find the continuous encoded SCLK value corresponding to ET.
      !>
      !>                     CALL SCE2C  ( -77,  ET,  SCLKDP )
      !>
      !>            Now you're ready to call both CKGP, which expects the input
      !>            epoch to be specified by an encoded SCLK string, and
      !>            SPKEZ, which expects the epoch to be specified as an
      !>            ephemeris time.
      !>
      !>               C
      !>               C     Find scan platform pointing CMAT and s/c--target
      !>               C     vector (first 3 components of STATE) at epoch.
      !>               C     We assume that CK and SPK kernels have been loaded
      !>               C     already, via CKLPF and SPKLEF respectively.
      !>               C
      !>                     CALL CKGP  ( SCANPL,
      !>                    .             SCLKDP,
      !>                    .             TOL,
      !>                    .             REFSYS,
      !>                    .             CMAT,
      !>                    .             CLKOUT,
      !>                    .             FOUND   )
      !>
      !>                     CALL SPKEZ ( TARGET,
      !>                    .             ET,
      !>                    .             REFSYS,
      !>                    .             CORR,
      !>                    .             -77,
      !>                    .             STATE,
      !>                    .             LT      )
      !>
      !>     2)  Convert UTC to a continuous encoded Voyager 2 SCLK value.
      !>
      !>            Again, your initialization code must load the leapseconds
      !>            and SCLK kernels.
      !>
      !>               C
      !>               C     Load leapseconds and SCLK kernels:
      !>               C
      !>                     CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>                     CALL FURNSH ( 'VGR2SCLK.KER'    )
      !>
      !>            To find the encoded Voyager 2 SCLK value SCLKDP
      !>            corresponding to a UTC time, you can use the code fragment
      !>
      !>                     CALL UTC2ET ( UTC,  ET          )
      !>                     CALL SCE2C  ( -32,  ET,  SCLKDP )
      !>
      !>## Restrictions
      !>
      !>     1)  An SCLK kernel appropriate to the spacecraft clock identified
      !>         by SC must be loaded at the time this routine is called.
      !>
      !>     2)  If the SCLK kernel used with this routine does not map SCLK
      !>         directly to barycentric dynamical time, a leapseconds kernel
      !>         must be loaded at the time this routine is called.
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
      !>-    SPICELIB Version 1.1.0, 13-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved the
      !>        required readings present in $Literature_References section to
      !>        $Required_Reading.
      !>
      !>-    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.0, 09-MAR-1999 (NJB)
      module subroutine sce2c ( ctx, sc, et, sclkdp )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      real(wp), intent(in) :: et  !! Ephemeris time, seconds past J2000.
      real(wp), intent(out) :: sclkdp  !! SCLK, encoded as ticks since spacecraft clock start. SCLKDP need not be integral.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Just hand off the conversion to the appropriate routine.
!
      if (  sctype ( ctx, sc )  ==  1  ) then

         call scec01 ( ctx, sc, et, sclkdp )

      else

         call ctx%setmsg ( 'Clock type # is not supported.' )
         call ctx%errint ( '#', sctype ( ctx, sc )               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'            )
         return

      end if

      return
      end subroutine sce2c

      !> Evaluate a single SPK data record from a segment of type 10
      !> (NORAD two-line element sets.). This evaluator uses algorithms
      !> as described in Vallado 2006 [4].
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, specified as ephemeris seconds past
      !>              J2000, at which a state vector is to be computed.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of some
      !>              body, relative to some center, in some inertial
      !>              reference frame.
      !>
      !>              The structure of RECORD is:
      !>
      !>                  RECORD(1)
      !>                     .            Geophysical Constants such as
      !>                     .            GM, J2, J3, J4, etc.
      !>                     .
      !>                  RECORD(NGEO)
      !>
      !>                  RECORD(NGEO + 1)
      !>                     .
      !>                     .            elements and epoch for the body
      !>                     .            at epoch 1.
      !>                     .
      !>                  RECORD(NGEO + NELEMS )
      !>
      !>                  RECORD(NGEO + NELEMS + 1)
      !>                     .
      !>                     .            elements and epoch for the body
      !>                     .            at epoch 2.
      !>                     .
      !>                  RECORD(NGEO + 2*NELEMS )
      !>
      !>              Epoch 1 and epoch 2 are the times in the segment that
      !>              bracket ET. If ET is less than the first time in the
      !>              segment then both epochs 1 and 2 are equal to the
      !>              first time. And if ET is greater than the last time
      !>              then, epochs 1 and 2 are set equal to this last time.
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state produced by evaluating RECORD at ET.
      !>              Units are km and km/sec relative to the J2000
      !>              reference frame.
      !>
      !>## Parameters
      !>
      !>     NGEO     is the number of geophysical constants for SGP4 SPK
      !>              records.
      !>
      !>     AFSPC    set the SGP4 propagator to use the original
      !>              Space Track #3 GST algorithm as described in Hoots [1];
      !>              value defined in zzsgp4.inc.
      !>
      !>     IMPRVD   set the SGP4 propagator to use the improved GST
      !>              algorithm as defined in Vallado [4]; value defined in
      !>              zzsgp4.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  If a problem occurs when evaluating the two-line elements, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine interpolates a state from the two reference sets
      !>     of two-line element sets contained in RECORD.
      !>
      !>     It is assumed that this routine is used in conjunction with
      !>     the routine SPKR10 as shown here:
      !>
      !>        CALL SPKR10   ( HANDLE, DESCR, ET, RECORD         )
      !>        CALL SPKE10   (                ET, RECORD, STATE  )
      !>
      !>     Where it is known in advance that the HANDLE, DESCR pair points
      !>     to a type 10 data segment.
      !>
      !>## Examples
      !>
      !>     The SPKEnn routines are almost always used in conjunction with
      !>     the corresponding SPKRnn routines, which read the records from
      !>     SPK files.
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRnn
      !>     routines might be used to examine raw segment data before
      !>     evaluating it with the SPKEnn routines.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>     C
      !>     C     Look at parts of the descriptor.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 10 ) THEN
      !>
      !>              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE10 ( ET, RECORD, STATE )
      !>                  .
      !>                  .  Check out the evaluated state.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
      !>          Propagation of the NORAD Element Sets," U.S. Air Force
      !>          Aerospace Defense Command, Colorado Springs, CO, 1980.
      !>
      !>     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
      !>          Space Command Element Sets,"  U.S. Air Force Aerospace
      !>          Defense Command, Colorado Springs, CO, 1986.
      !>
      !>     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
      !>          Orbit Modeling in the U. S. Space Surveillance System,"
      !>          Journal of Guidance, Control, and Dynamics. 27(2):174-185,
      !>          2004.
      !>
      !>     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
      !>          Spacetrack Report #3," paper AIAA 2006-6753 presented at the
      !>          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
      !>          August 21-24, 2006.
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
      !>-    SPICELIB Version 3.1.0, 10-OCT-2021 (JDR) (EDW)
      !>
      !>        Use of modified ZZTEME to eliminate a matrix inversion.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 18-FEB-2015 (EDW)
      !>
      !>        Evaluator now uses Vallado derived propagator as described
      !>        in Vallado 2006 [4].
      !>
      !>-    SPICELIB Version 2.0.0, 01-JAN-2011 (EDW)
      !>
      !>        Correction of state transformation calculation. Algorithm
      !>        now computes state transformation as from TEME to J2000.
      !>        The previous version of this routine calculated TETE to
      !>        J2000.
      !>
      !>-    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MTXV and VADD calls.
      !>
      !>-    SPICELIB Version 1.0.0, 18-JUL-1997 (WLT)
      module subroutine spke10 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi
      use pepper_math, only: mxvg
      use pepper_inc_zzsgp4
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity).
      integer(ip), parameter :: nangs = 4
      integer(ip), parameter :: time1 = ngeo + kepoch
      integer(ip), parameter :: time2 = nelems + nangs + time1
      integer(ip), parameter :: model1 = ngeo + 1
      integer(ip), parameter :: angs1 = model1 + nelems
      integer(ip), parameter :: model2 = angs1 + nangs
      real(wp), parameter :: mypi = pi
      real(wp) :: tm2j     ( 6, 6 )
      real(wp) :: j2tm     ( 6, 6 )
      real(wp) :: dargdt
      real(wp) :: vcomp    ( 3 )
      real(wp) :: t1
      real(wp) :: w
      real(wp) :: denom
      real(wp) :: arg
      real(wp) :: s1       ( 6 )
      real(wp) :: numer
      real(wp) :: s2       ( 6 )
      real(wp) :: t2
      real(wp) :: dwdt
      real(wp) :: tmpsta   ( 6 )
      if ( ctx%failed() ) return

!
!     Fetch the two epochs stored in the record.
!
      t1 = record(time1)
      t2 = record(time2)

!
!     Evaluate the two states. Call them s_1(t) and s_2(t).
!     Let the position and velocity components be: p_1, v_1, p_2, v_2.
!
!     The final position is a weighted average.
!
!     Let
!
!        W(t) =  0.5 + 0.5*COS( PI*(t-t1)/(t2-t1) )
!
!     then
!
!        p  = W(t)*p_1(t) + (1 - W(t))*p_2(t)
!        v  = W(t)*v_1(t) + (1 - W(t))*v_2(t) + W'(t)*(p_1(t) - p_2(t))
!
!     If t1 = t2, the state is just s(t1).
!
!     Note: there are a number of weighting schemes we could have
!     used.  This one has the nice property that
!
!     The graph of W is symmetric about the point
!
!        ( (t1+t2)/2,  W( (t1+t2)/2 ) )
!
!     The range of W is from 1 to 0. The derivative of W is
!     symmetric and zero at both t1 and t2.
!

      if ( t1 /= t2 ) then

!
!        Initialize then propagate.
!
!        XXSGP4E returns on entry if XXSGP4I signals an error.
!

!
!        Evaluate first TLE.
!
         call xxsgp4i ( ctx, record(1), record(model1), afspc )

         if ( ctx%failed() ) return

!
!        Time from epoch of set 1 in minutes.
!
         call xxsgp4e ( ctx, (et - t1)/60.0_wp, s1 )

         if ( ctx%failed() ) return

!
!        Evaluate second TLE.
!
         call xxsgp4i ( ctx, record(1), record(model2), afspc )

         if ( ctx%failed() ) return

!
!        Time from epoch of set 2 in minutes.
!
         call xxsgp4e ( ctx, (et - t2)/60.0_wp, s2 )

         if ( ctx%failed() ) return

!
!        Compute the weighting function that we'll need later
!        when we combine states 1 and 2.
!
         numer  = et - t1
         denom  = t2 - t1
         arg    = numer*mypi/denom
         dargdt =       mypi/denom

         w      =  0.5_wp + 0.5_wp * dcos( arg )
         dwdt   =        - 0.5_wp * dsin( arg ) * dargdt

!
!        Now compute the weighted average of the two states.
!
         state = w*s1 + (1.0_wp-w)*s2
         vcomp = dwdt*s1(1:3) + (-dwdt)*s2(1:3)
         tmpsta(4:6) = state(4:6) + vcomp
         state(4:6) = tmpsta(4:6)

      else

!
!        Evaluate the TLE.
!
         call xxsgp4i ( ctx, record(1), record(model1), afspc )

         if ( ctx%failed() ) return

!
!        Time from epoch of set 1 in minutes.
!
         call xxsgp4e ( ctx, (et - t1)/60.0_wp, state )

         if ( ctx%failed() ) return

      end if

!
!     Finally, convert the TEME state to J2000.  First
!     calculate the mapping from J2000 to TEME (J2TM), and from
!     TEME to J2000 (TM2J) at time ET...
!
!                         -1
!     Note that J2TM = TM2J
!
      call zzteme( ctx, et, j2tm, tm2j )

!
!     ...now convert the TEME state to a J2000 state.
!
      call mxvg   ( tm2j, state, 6, 6, tmpsta )
      state = tmpsta

      return
      end subroutine spke10

      !> Return a 3x3 matrix that transforms positions in inertial
      !> coordinates to positions in body-equator-and-prime-meridian
      !> coordinates.
      !>
      !>## Detailed Input
      !>
      !>     REF      is the NAIF name for an inertial reference frame.
      !>              Acceptable names include:
      !>
      !>                 Name       Description
      !>                 --------   --------------------------------
      !>                 'J2000'    Earth mean equator, dynamical
      !>                            equinox of J2000
      !>
      !>                 'B1950'    Earth mean equator, dynamical
      !>                            equinox of B1950
      !>
      !>                 'FK4'      Fundamental Catalog (4)
      !>
      !>                 'DE-118'   JPL Developmental Ephemeris (118)
      !>
      !>                 'DE-96'    JPL Developmental Ephemeris ( 96)
      !>
      !>                 'DE-102'   JPL Developmental Ephemeris (102)
      !>
      !>                 'DE-108'   JPL Developmental Ephemeris (108)
      !>
      !>                 'DE-111'   JPL Developmental Ephemeris (111)
      !>
      !>                 'DE-114'   JPL Developmental Ephemeris (114)
      !>
      !>                 'DE-122'   JPL Developmental Ephemeris (122)
      !>
      !>                 'DE-125'   JPL Developmental Ephemeris (125)
      !>
      !>                 'DE-130'   JPL Developmental Ephemeris (130)
      !>
      !>                 'GALACTIC' Galactic System II
      !>
      !>                 'DE-200'   JPL Developmental Ephemeris (200)
      !>
      !>                 'DE-202'   JPL Developmental Ephemeris (202)
      !>
      !>              See the Frames Required Reading frames.req for a full
      !>              list of inertial reference frame names built into
      !>              SPICE.
      !>
      !>              The output TIPM will give the transformation
      !>              from this frame to the bodyfixed frame specified by
      !>              BODY at the epoch specified by ET.
      !>
      !>     BODY     is the integer ID code of the body for which the
      !>              position transformation matrix is requested. Bodies
      !>              are numbered according to the standard NAIF numbering
      !>              scheme. The numbering scheme is explained in the NAIF
      !>              IDs Required Reading naif_ids.req.
      !>
      !>     ET       is the epoch at which the position transformation
      !>              matrix is requested. (This is typically the
      !>              epoch of observation minus the one-way light time
      !>              from the observer to the body at the epoch of
      !>              observation.)
      !>
      !>## Detailed Output
      !>
      !>     TIPM     is a 3x3 coordinate transformation matrix. It is
      !>              used to transform positions from inertial
      !>              coordinates to body fixed (also called equator and
      !>              prime meridian --- PM) coordinates.
      !>
      !>              Given a position P in the inertial reference frame
      !>              specified by REF, the corresponding bodyfixed
      !>              position is given by the matrix vector product:
      !>
      !>                 TIPM * S
      !>
      !>              The X axis of the PM system is directed to the
      !>              intersection of the equator and prime meridian.
      !>              The Z axis points along  the spin axis and points
      !>              towards the same side of the invariable plane of
      !>              the solar system as does earth's north pole.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the kernel pool does not contain all of the data required
      !>         for computing the transformation matrix, TIPM, the error
      !>         SPICE(INSUFFICIENTANGLES) is signaled.
      !>
      !>     2)  If the reference frame, REF, is not recognized, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the specified body code, BODY, is not recognized, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     TIPBOD takes PCK information as input, either in the
      !>     form of a binary or text PCK file. High precision
      !>     binary files are searched for first (the last loaded
      !>     file takes precedence); then it defaults to the text
      !>     PCK file. If binary information is found for the
      !>     requested body and time, the Euler angles are
      !>     evaluated and the transformation matrix is calculated
      !>     from them. Using the Euler angles PHI, DELTA and W
      !>     we compute
      !>
      !>        TIPM = [W]  [DELTA]  [PHI]
      !>                  3        1      3
      !>
      !>     If no appropriate binary PCK files have been loaded,
      !>     the text PCK file is used. Here information is found
      !>     as RA, DEC and W (with the possible addition of nutation
      !>     and libration terms for satellites). Again, the Euler
      !>     angles are found, and the transformation matrix is
      !>     calculated from them. The transformation from inertial to
      !>     body-fixed coordinates is represented as:
      !>
      !>        TIPM = [W]  [HALFPI-DEC]  [RA+HALFPI]
      !>                  3             1            3
      !>
      !>     These are basically the Euler angles, PHI, DELTA and W:
      !>
      !>        RA  = PHI - HALFPI
      !>        DEC = HALFPI - DELTA
      !>        W   = W
      !>
      !>     The angles RA, DEC, and W are defined as follows in the
      !>     text PCK file:
      !>
      !>                                      2    .-----
      !>                      RA1*t      RA2*t      \
      !>        RA  = RA0  + -------  + -------   +  )  a(i) * sin( theta(i) )
      !>                        T          2        /
      !>                                  T        '-----
      !>                                              i
      !>
      !>                                       2   .-----
      !>                      DEC1*t     DEC2*t     \
      !>        DEC = DEC0 + -------- + --------  +  )  d(i) * cos( theta(i) )
      !>                        T           2       /
      !>                                   T       '-----
      !>                                              i
      !>
      !>                                     2     .-----
      !>                       W1*t      W2*t       \
      !>        W   = W0   +  ------  + -------   +  )  w(i) * sin( theta(i) )
      !>                        d          2        /
      !>                                  d        '-----
      !>                                              i
      !>
      !>     where `d' is in seconds/day; T in seconds/Julian century;
      !>     a(i), d(i), and w(i) arrays apply to satellites only; and
      !>     theta(i), defined as
      !>
      !>                                THETA1(i)*t
      !>        theta(i) = THETA0(i) + -------------
      !>                                     T
      !>
      !>     are specific to each planet.
      !>
      !>     These angles ---typically nodal rates--- vary in number and
      !>     definition from one planetary system to the next.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Calculate the matrix to rotate a position vector from the
      !>        J2000 frame to the Saturn fixed frame at a specified
      !>        time, and use it to compute the position of Titan in
      !>        Saturn's body-fixed frame.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: tipbod_ex1.tm
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
      !>              sat375.bsp                    Saturn satellite ephemeris
      !>              pck00010.tpc                  Planet orientation and
      !>                                            radii
      !>              naif0012.tls                  Leapseconds
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'sat375.bsp',
      !>                                  'pck00010.tpc',
      !>                                  'naif0012.tls'  )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TIPBOD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      LT
      !>      DOUBLE PRECISION      POS    ( 3    )
      !>      DOUBLE PRECISION      SATVEC ( 3    )
      !>      DOUBLE PRECISION      TIPM   ( 3, 3 )
      !>
      !>      INTEGER               SATID
      !>
      !>C
      !>C     Load the kernels.
      !>C
      !>      CALL FURNSH ( 'tipbod_ex1.tm' )
      !>
      !>C
      !>C     The body ID for Saturn.
      !>C
      !>      SATID = 699
      !>
      !>C
      !>C     Retrieve the transformation matrix at some time.
      !>C
      !>      CALL STR2ET ( 'Jan 1 2005',   ET       )
      !>      CALL TIPBOD ( 'J2000', SATID, ET, TIPM )
      !>
      !>C
      !>C     Retrieve the position of Titan as seen from Saturn
      !>C     in the J2000 frame at ET.
      !>C
      !>      CALL SPKPOS ( 'TITAN',  ET, 'J2000', 'NONE',
      !>     .              'SATURN', POS, LT            )
      !>
      !>      WRITE(*,'(A)')        'Titan as seen from Saturn:'
      !>      WRITE(*,'(A,3F13.3)') '   in J2000 frame     :', POS
      !>
      !>C
      !>C     Rotate the position 3-vector POS into the
      !>C     Saturn body-fixed reference frame.
      !>C
      !>      CALL MXV ( TIPM, POS, SATVEC )
      !>
      !>      WRITE(*,'(A,3F13.3)') '   in IAU_SATURN frame:', SATVEC
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Titan as seen from Saturn:
      !>           in J2000 frame     :  1071928.661  -505781.970   -60383.976
      !>           in IAU_SATURN frame:   401063.338 -1116965.364    -5408.806
      !>
      !>        Note that the complete example could be replaced by a single
      !>        SPKPOS call:
      !>
      !>           CALL SPKPOS ( 'TITAN',  ET, 'IAU_SATURN', 'NONE',
      !>          .              'SATURN', POS, LT                   )
      !>
      !>## Restrictions
      !>
      !>     1)  The kernel pool must be loaded with the appropriate
      !>         coefficients (from a text or binary PCK file) prior to
      !>         calling this routine.
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries in $Revisions section.
      !>
      !>        Added complete code example.
      !>
      !>        Added frames.req to $Required_Reading.
      !>
      !>-    SPICELIB Version 1.3.0, 02-MAR-2016 (BVS)
      !>
      !>        Updated to use the 3x3 top-left corner of the 6x6 matrix
      !>        returned by TISBOD instead of fetching kernel data and doing
      !>        computations in-line.
      !>
      !>        Fixed indentation of some header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 23-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in MXM call. Replaced header references to LDPOOL with
      !>        references to FURNSH.
      !>
      !>-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
      !>
      !>        Tests of routine FAILED() were added.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1994 (KSZ)
      !>
      !>        Underlying BODMAT code changed to look for binary PCK
      !>        data files, and use them to get orientation information if
      !>        they are available. Only the comments to TIPBOD changed.
      !>
      !>-    SPICELIB Version 1.0.2, 06-JUL-1993 (HAN)
      !>
      !>        Example in header was corrected. Previous version had
      !>        incorrect matrix dimension specifications passed to MXVG.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 05-AUG-1991 (NJB) (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 05-JAN-2005 (NJB)
      !>
      !>        Tests of routine FAILED() were added. The new checks
      !>        are intended to prevent arithmetic operations from
      !>        being performed with uninitialized or invalid data.
      module subroutine tipbod ( ctx, ref, body, et, tipm )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: ref  !! ID of inertial reference frame to transform from.
      integer(ip), intent(in) :: body  !! ID code of body.
      real(wp), intent(in) :: et  !! Epoch of transformation.
      real(wp), intent(out) :: tipm   ( 3, 3 )  !! Position transformation matrix, inertial to prime meridian.

      real(wp) :: tsipm   ( 6, 6 )

      integer(ip) :: i
      integer(ip) :: j

!
!     Standard SPICE Error handling.
!
      if ( ctx%failed() ) return

!
!     Get 6x6 state transformation from TISBOD. If succeeded, pull out
!     left-top 3x3 matrix.
!
      call tisbod ( ctx, ref, body, et, tsipm )

      if ( ctx%failed() ) return

      do  i = 1, 3
         do   j  = 1, 3
            tipm  ( i, j ) = tsipm  ( i, j )
         end do
      end do

      return
      end subroutine tipbod

      !> Find the position rotation matrix from a C-kernel (CK) frame with
      !> the specified frame class ID (CK ID) to the base frame of the
      !> highest priority CK segment containing orientation data for this
      !> CK frame at the time requested.
      !>
      !>## Detailed Input
      !>
      !>     INST     is the unique frame class ID (CK ID) of the CK frame for
      !>              which data is being requested.
      !>
      !>     ET       is the epoch for which the position rotation is desired.
      !>              ET should be given in seconds past the epoch of J2000
      !>              TDB.
      !>
      !>## Detailed Output
      !>
      !>     ROTATE   is a position rotation matrix that converts positions
      !>              relative to the CK frame given by its frame class ID,
      !>              INST, to positions relative to the base frame given by
      !>              its frame ID, REF.
      !>
      !>              Thus, if a position S has components x,y,z in the CK
      !>              frame, then S has components x', y', z' in the base
      !>              frame.
      !>
      !>                 .-  -.     .-        -. .- -.
      !>                 | x' |     |          | | x |
      !>                 | y' |  =  |  ROTATE  | | y |
      !>                 | z' |     |          | | z |
      !>                 `-  -'     `-        -' `- -'
      !>
      !>     REF      is the ID code of the base reference frame to which
      !>              ROTATE will transform positions.
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. FOUND will be .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If no CK files were loaded prior to calling this routine, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If no SCLK correlation data needed to read CK files were
      !>         loaded prior to calling this routine, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>     3)  If the input time ET cannot be converted to an encoded SCLK
      !>         time, using SCLK data associated with INST, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     CKFROT searches through loaded CK files to locate a segment that
      !>     can satisfy the request for position rotation data for the CK
      !>     frame with the specified frame class ID at time ET. You must load
      !>     a CK file containing such data before calling this routine. You
      !>     must also load SCLK and possibly LSK files needed to convert the
      !>     input ET time to the encoded SCLK time with which the orientation
      !>     data stored inside that CK is tagged.
      !>
      !>## Particulars
      !>
      !>     CKFROT searches through loaded CK files to satisfy a pointing
      !>     request. Last-loaded files are searched first, and individual
      !>     files are searched in backwards order, giving priority to
      !>     segments that were added to a file later than the others.
      !>
      !>     The search ends when a segment is found that can give pointing
      !>     for the specified CK frame at the request time.
      !>
      !>     Segments with and without angular velocities are considered by
      !>     this routine.
      !>
      !>     This routine uses the CKMETA routine to determine the SCLK ID
      !>     used to convert the input ET time to the encoded SCLK time used
      !>     to look up pointing data in loaded CK files.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Use CKFROT to compute the instantaneous angular velocity
      !>        vector for the Mars Global Surveyor (MGS) spacecraft frame,
      !>        'MGS_SPACECRAFT', relative to the inertial frame used as the
      !>        base frame in CK files containing MGS spacecraft orientation
      !>        at 2003-JUL-25 13:00:00. The frame class ID (CK ID) for the
      !>        'MGS_SPACECRAFT' frame is -94000.
      !>
      !>        Suppose that R(t) is the rotation matrix whose columns
      !>        represent the inertial pointing vectors of the MGS spacecraft
      !>        axes at time `t'.
      !>
      !>        Then the angular velocity vector points along the vector given
      !>        by:
      !>
      !>                                T
      !>            limit  AXIS( R(t+h)R )
      !>            h-->0
      !>
      !>        And the magnitude of the angular velocity at time `t' is given
      !>        by:
      !>
      !>                                T
      !>            d ANGLE ( R(t+h)R(t) )
      !>           ------------------------   at   h = 0
      !>                      dh
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ckfrot_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>              mgs_sclkscet_00061.tsc        MGS SCLK coefficients
      !>              mgs_sc_ext12.bc               MGS s/c bus attitude
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = ( 'naif0012.tls',
      !>                               'mgs_sclkscet_00061.tsc',
      !>                               'mgs_sc_ext12.bc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKFROT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         EPOCH
      !>      PARAMETER           ( EPOCH  = '2003-JUL-25 13:00:00' )
      !>
      !>      INTEGER               INST
      !>      PARAMETER           ( INST   = -94000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      ANGLE
      !>      DOUBLE PRECISION      ANGVEL ( 3    )
      !>      DOUBLE PRECISION      AXIS   ( 3    )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      INFROT ( 3, 3 )
      !>      DOUBLE PRECISION      H
      !>      DOUBLE PRECISION      RET    ( 3, 3 )
      !>      DOUBLE PRECISION      RETH   ( 3, 3 )
      !>
      !>      INTEGER               REF
      !>      INTEGER               REFH
      !>
      !>      LOGICAL               FOUND
      !>      LOGICAL               FOUNDH
      !>
      !>C
      !>C     Load the required LSK, SCLK and CK. Use a
      !>C     meta-kernel for convenience.
      !>C
      !>      CALL FURNSH ( 'ckfrot_ex1.tm' )
      !>
      !>C
      !>C     First convert the time to seconds past J2000. Set the
      !>C     delta time (1 ms).
      !>C
      !>      CALL STR2ET ( EPOCH, ET )
      !>      H = 1.D-3
      !>
      !>C
      !>C     Now, look up the rotation from the MGS spacecraft
      !>C     frame specified by its frame class ID (CK ID) to a
      !>C     base reference frame (returned by CKFROT), at ET
      !>C     and ET+H.
      !>C
      !>      CALL CKFROT ( INST, ET,   RET,  REF,  FOUND  )
      !>      CALL CKFROT ( INST, ET+H, RETH, REFH, FOUNDH )
      !>
      !>C
      !>C     If both rotations were computed and if the base
      !>C     reference frames are the same, compute the
      !>C     instantaneous angular velocity vector.
      !>C
      !>      IF ( FOUND .AND. FOUNDH .AND. REF .EQ. REFH ) THEN
      !>
      !>C
      !>C        Compute the infinitesimal rotation R(t+h)R(t)**T.
      !>C
      !>         CALL MXMT ( RETH, RET, INFROT )
      !>
      !>C
      !>C        Compute the AXIS and ANGLE of the infinitesimal
      !>C        rotation.
      !>C
      !>         CALL RAXISA ( INFROT, AXIS, ANGLE )
      !>
      !>C
      !>C        Scale AXIS to get the angular velocity vector.
      !>C
      !>         CALL VSCL ( ANGLE/H, AXIS, ANGVEL )
      !>
      !>C
      !>C        Output the results.
      !>C
      !>         WRITE(*,'(A)')
      !>     .           'Instantaneous angular velocity vector:'
      !>         WRITE(*,'(3F15.10)') ANGVEL
      !>         WRITE(*,'(A,I5)') 'Reference frame ID:', REF
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) 'ERROR: data not found or frame mismatch.'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Instantaneous angular velocity vector:
      !>           0.0001244121   0.0008314866   0.0003028634
      !>        Reference frame ID:    1
      !>
      !>## Restrictions
      !>
      !>     1)  A CK file must be loaded prior to calling this routine.
      !>
      !>     2)  LSK and SCLK files needed for time conversions must be loaded
      !>         prior to calling this routine.
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
      !>-    SPICELIB Version 1.3.0, 13-DEC-2021 (JDR) (BVS) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard and modern
      !>        SPICE CK and frames terminology. Added initialization of local
      !>        variable SFND.
      !>
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.2.0, 17-FEB-2000 (WLT)
      !>
      !>        The routine now checks to make sure convert ET to TICKS
      !>        and that at least one C-kernel is loaded before trying
      !>        to look up the transformation. Also the routine now calls
      !>        SCE2C instead of SCE2T.
      !>
      !>-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
      module subroutine ckfrot ( ctx, inst, et, rotate, ref, found )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inst  !! Frame class ID (CK ID) of a CK frame.
      real(wp), intent(in) :: et  !! Epoch measured in seconds past J2000 TDB.
      real(wp), intent(out) :: rotate  ( 3, 3 )  !! Rotation matrix from CK frame to frame REF.
      integer(ip), intent(out) :: ref  !! Frame ID of the base reference.
      logical, intent(out) :: found  !! .TRUE. when requested pointing is available.

      integer(ip) :: handle
      integer(ip) :: icd      ( nic  )
      integer(ip) :: sclkid

      real(wp) :: av       ( 3    )
      real(wp) :: clkout
      real(wp) :: descr    ( nc   )
      real(wp) :: dcd      ( ndc  )
      real(wp) :: rot      ( 3, 3 )
      real(wp) :: time
      real(wp) :: tol

      character(idlen) :: segid

      logical :: needav
      logical :: sfnd
      logical :: pfnd
      logical :: have

!
!     Set FOUND to .FALSE. right now in case we end up
!     returning before doing any work.
!
      found = .false.
      ref   =  0

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     We don't need angular velocity data.
!     Assume the segment won't be found until it really is.
!
      needav = .false.
      tol    =  0.0_wp

!
!     Begin a search for this instrument and time, and get the first
!     applicable segment.
!
      call ckhave ( ctx, have )
      call ckmeta ( ctx, inst,  'SCLK', sclkid )

      if ( .not. have ) then
         return
      else if ( .not. zzsclk ( ctx, inst, sclkid ) ) then
         return
      end if

!
!     Initialize SFND here in case an error occurs before CKSNS can
!     set its value.
!
      sfnd = .false.

      call sce2c  ( ctx, sclkid, et,            time   )
      call ckbss  ( ctx, inst,   time,   tol,   needav )
      call cksns  ( ctx, handle, descr,  segid, sfnd   )

!
!     Keep trying candidate segments until a segment can produce a
!     pointing instance within the specified time tolerance of the
!     input time.
!
!     Check FAILED to prevent an infinite loop if an error is detected
!     by a SPICELIB routine and the error handling is not set to abort.
!
      do while ( sfnd .and. ( .not. ctx%failed() ) )

         call ckpfs ( ctx, handle, descr, time,   tol, needav, &
                      rot,    av,    clkout, pfnd          )

         if ( pfnd ) then
!
!           Found one. Fetch the ID code of the reference frame
!           from the descriptor.
!
            call dafus  ( ctx, descr, ndc, nic, dcd, icd )
            ref   =  icd( 2 )
            found = .true.
!
!           We now have the rotation matrix from
!           REF to INS. We invert ROT to get the rotation
!           from INST to REF.
!
            rotate = transpose(rot)

            return

         end if

         call cksns ( ctx, handle, descr, segid, sfnd )

      end do

      return
      end subroutine ckfrot

      !> Find the state transformation matrix from a C-kernel (CK) frame
      !> with the specified frame class ID (CK ID) to the base frame of
      !> the highest priority CK segment containing orientation and
      !> angular velocity data for this CK frame at the time requested.
      !>
      !>## Detailed Input
      !>
      !>     INST     is the unique frame class ID (CK ID) of the CK frame for
      !>              which data is being requested.
      !>
      !>     ET       is the epoch for which the state transformation is
      !>              desired. ET should be given in seconds past the epoch of
      !>              J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     XFORM    is a state transformation matrix that converts states
      !>              relative to the CK frame given by its frame class ID,
      !>              INST, to states relative to the base frame given by its
      !>              frame ID, REF.
      !>
      !>              Thus, if a state S has components x, y, z, dx, dy, dz in
      !>              the CK frame, then S has components x', y', z', dx',
      !>              dy', dz' in the base frame REF.
      !>
      !>                 .-   -.     .-         -. .-  -.
      !>                 |  x' |     |           | |  x |
      !>                 |  y' |     |           | |  y |
      !>                 |  z' |     |           | |  z |
      !>                 | dx' |  =  |   XFORM   | | dx |
      !>                 | dy' |     |           | | dy |
      !>                 | dz' |     |           | | dz |
      !>                 `-   -'     `-         -' `-  -'
      !>
      !>     REF      is the ID code of the base reference frame to which
      !>              XFORM will transform states.
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. FOUND will be .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If no CK files were loaded prior to calling this routine, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If no SCLK correlation data needed to read CK files were
      !>         loaded prior to calling this routine, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>     3)  If the input time ET cannot be converted to an encoded SCLK
      !>         time, using SCLK data associated with INST, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     CKFXFM searches through loaded CK files to locate a segment that
      !>     can satisfy the request for state transformation data for the CK
      !>     frame with the specified frame class ID at time ET. You must load
      !>     a CK file containing such data before calling this routine. You
      !>     must also load SCLK and possibly LSK files needed to convert the
      !>     input ET time to the encoded SCLK time with which the orientation
      !>     data stored inside that CK is tagged.
      !>
      !>## Particulars
      !>
      !>     CKFXFM searches through loaded CK files to satisfy a pointing
      !>     request. Last-loaded files are searched first, and individual
      !>     files are searched in backwards order, giving priority to
      !>     segments that were added to a file later than the others.
      !>
      !>     The search ends when a segment is found that can give pointing
      !>     for the specified CK frame at the request time.
      !>
      !>     Only segments with angular velocities are considered by this
      !>     routine.
      !>
      !>     This routine uses the CKMETA routine to determine the SCLK ID
      !>     used to convert the input ET time to the encoded SCLK time used
      !>     to look up pointing data in loaded CK files.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Use CKFXFM to compute the angular rate of rotation for the Mars
      !>        Global Surveyor (MGS) spacecraft frame, 'MGS_SPACECRAFT',
      !>        relative to the inertial frame used as the base frame in CK
      !>        files containing MGS spacecraft orientation at 2003-JUL-25
      !>        13:00:00. The frame class ID (CK ID) for the 'MGS_SPACECRAFT'
      !>        frame is -94000.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ckfxfm_ex1.tm
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
      !>              naif0012.tls                  Leapseconds
      !>              mgs_sclkscet_00061.tsc        MGS SCLK coefficients
      !>              mgs_sc_ext12.bc               MGS s/c bus attitude
      !>
      !>           \begindata
      !>
      !>           KERNELS_TO_LOAD = ( 'naif0012.tls',
      !>                               'mgs_sclkscet_00061.tsc',
      !>                               'mgs_sc_ext12.bc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKFXFM_EX1
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
      !>      CHARACTER*(*)         EPOCH
      !>      PARAMETER           ( EPOCH  = '2003-JUL-25 13:00:00' )
      !>
      !>      INTEGER               INST
      !>      PARAMETER           ( INST   = -94000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AV     ( 3    )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      ROT    ( 3, 3 )
      !>      DOUBLE PRECISION      XFORM  ( 6, 6 )
      !>
      !>      INTEGER               REF
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load the required LSK, SCLK and CK. Use a
      !>C     meta-kernel for convenience.
      !>C
      !>      CALL FURNSH ( 'ckfxfm_ex1.tm' )
      !>
      !>C
      !>C     First convert the time to seconds past J2000.
      !>C
      !>      CALL STR2ET ( EPOCH, ET )
      !>
      !>C
      !>C     Now, look up the state transformation from the MGS
      !>C     spacecraft frame specified by its frame class ID
      !>C     (CK ID) to a base reference frame (returned by
      !>C     CKFXFM), at ET.
      !>C
      !>      CALL CKFXFM ( INST, ET, XFORM, REF, FOUND )
      !>
      !>C
      !>C     Next determine the angular velocity of the
      !>C     transformation.
      !>C
      !>      CALL XF2RAV ( XFORM, ROT, AV )
      !>
      !>C
      !>C     The angular rate of change (in radians/second) is just
      !>C     the magnitude of AV.
      !>C
      !>      WRITE(*,'(A,F20.16)') 'Angular rate of change (rad/s):',
      !>     .                       VNORM ( AV )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Angular rate of change (rad/s):  0.0008907319999591
      !>
      !>## Restrictions
      !>
      !>     1)  A CK file must be loaded prior to calling this routine.
      !>
      !>     2)  LSK and SCLK files needed for time conversions must be loaded
      !>         prior to calling this routine.
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
      !>-    SPICELIB Version 2.3.0, 13-DEC-2021 (JDR) (BVS) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard and modern
      !>        SPICE CK and frames terminology. Added complete code example
      !>        based on existing fragments. Added initialization of local
      !>        variable SFND.
      !>
      !>-    SPICELIB Version 2.2.0, 17-FEB-2000 (WLT)
      !>
      !>        The routine now checks to make sure convert ET to TICKS
      !>        and that at least one C-kernel is loaded before trying
      !>        to look up the transformation.
      !>
      !>-    SPICELIB Version 2.1.0, 09-MAR-1999 (NJB)
      !>
      !>        A call to SCE2T has been replaced by a call to SCE2C.
      !>
      !>-    SPICELIB Version 2.0.0, 28-JUL-1997 (WLT)
      !>
      !>        The previous edition did not correctly compute the derivative
      !>        block of the state transformation matrix.
      !>
      !>        The routine incorrectly computed the state transformation
      !>        matrix using the rotation from INST to REF together with
      !>        the angular velocity from REF to INST. Now it computes
      !>        the state transformation matrix from REF to INST and then
      !>        inverts the result to get the correct matrix.
      !>
      !>        Moved the assignment of FOUND to just before the check
      !>        of the SPICELIB function RETURN. That way if the routine
      !>        exits immediately via a check of the function RETURN(),
      !>        FOUND will have an appropriate value.
      !>
      !>-    SPICELIB Version 1.0.0, 03-OCT-1994 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 09-MAR-1999 (NJB)
      !>
      !>        A call to SCE2T has been replaced by a call to SCE2C. This
      !>        routine performs conversion of ET to continuous ticks,
      !>        reducing truncation error in the representation of the input
      !>        time value.
      module subroutine ckfxfm ( ctx, inst, et, xform, ref, found )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: rav2xf
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inst  !! Frame class ID (CK ID) of a CK frame.
      real(wp), intent(in) :: et  !! Epoch measured in seconds past J2000 TDB.
      real(wp), intent(out) :: xform  ( 6, 6 )  !! Transformation from CK frame to frame REF.
      integer(ip), intent(out) :: ref  !! Frame ID of the base reference.
      logical, intent(out) :: found  !! .TRUE. when requested pointing is available.

      integer(ip) :: handle
      integer(ip) :: icd      ( nic  )
      integer(ip) :: sclkid

      real(wp) :: av       ( 3    )
      real(wp) :: clkout
      real(wp) :: descr    ( nc   )
      real(wp) :: dcd      ( ndc  )
      real(wp) :: rot      ( 3, 3 )
      real(wp) :: ref2in   ( 6, 6 )
      real(wp) :: time
      real(wp) :: tol

      character(idlen) :: segid

      logical :: needav
      logical :: sfnd
      logical :: pfnd
      logical :: have

!
!     Set FOUND to .FALSE. right now in case we end up
!     returning before doing any work.
!
      found = .false.
      ref   =  0

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Need angular velocity data.
!     Assume the segment won't be found until it really is.
!
      needav = .true.
      tol    =  0.0_wp

!
!     Begin a search for this instrument and time, and get the first
!     applicable segment.
!
      call ckmeta ( ctx, inst,  'SCLK', sclkid )
      call ckhave ( ctx, have )

      if ( .not. have ) then
         return
      else if ( .not. zzsclk ( ctx, inst, sclkid ) ) then
         return
      end if

!
!     Initialize SFND here in case an error occurs before CKSNS can
!     set its value.
!
      sfnd = .false.

      call sce2c  ( ctx, sclkid, et,            time   )
      call ckbss  ( ctx, inst,   time,   tol,   needav )
      call cksns  ( ctx, handle, descr,  segid, sfnd   )

!
!     Keep trying candidate segments until a segment can produce a
!     pointing instance within the specified time tolerance of the
!     input time.
!
!     Check FAILED to prevent an infinite loop if an error is detected
!     by a SPICELIB routine and the error handling is not set to abort.
!
      do while ( sfnd .and. ( .not. ctx%failed() ) )

         call ckpfs ( ctx, handle, descr, time,   tol, needav, &
                      rot,    av,    clkout, pfnd          )

         if ( pfnd ) then
!
!           Found one. Fetch the ID code of the reference frame
!           from the descriptor.
!
            call dafus  ( ctx, descr, ndc, nic, dcd, icd )
            ref   =  icd( 2 )
            found = .true.
!
!           We now have the transformation matrix from
!           REF to INST immediately. Using the angular velocity
!           we compute the state transformation matrix from REF to INST
!
            call rav2xf ( rot, av, ref2in )

!
!           Finally, we invert REF2IN to get the state transformation
!           from INST to REF.
!
            call invstm ( ctx, ref2in, xform )

            return

         end if

         call cksns ( ctx, handle, descr, segid, sfnd )

      end do

      return
      end subroutine ckfxfm

      !> Return, for a specified SPK segment and time, the state (position
      !> and velocity) of the segment's target body relative to its center
      !> of motion.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle assigned to a SPK file, and the
      !>              descriptor for a segment within the file. Together
      !>              they determine the ephemeris data from which the
      !>              state of the body is to be computed.
      !>
      !>     ET       is the epoch (ephemeris time) at which the state
      !>              is to be computed.
      !>
      !>## Detailed Output
      !>
      !>     REF      is the id-code of the reference frame to
      !>              which the vectors returned by the routine belong.
      !>
      !>     STATE    is a 6-dimensional vector that contains the position and
      !>              velocity, at epoch ET, for whatever body is covered by
      !>              the specified segment. STATE has six elements: the first
      !>              three contain the body's position; the last three contain
      !>              the body's velocity. These vectors are rotated into the
      !>              specified  reference frame, the origin of which is
      !>              located at the center of motion for the body (see CENTER,
      !>              below). Units are always km and km/sec.
      !>
      !>     CENTER   is the integer ID code of the center of motion for
      !>              the state.
      !>
      !>## Parameters
      !>
      !>     MAXREC   is the maximum length of a record returned by any of
      !>              data type-specific routines SPKRnn, which are called
      !>              by SPKPVN (see $Particulars).
      !>
      !>## Exceptions
      !>
      !>     1)  If the segment type is not supported by the current
      !>         version of SPKPVN, the error SPICE(SPKTYPENOTSUPP)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     SPKPVN is the most basic of the SPK readers, the reader upon
      !>     which SPKPV and SPKGEO, etc. are built. It should not normally
      !>     be called directly except in cases where some optimization is
      !>     required. (That is, where the calling program has prior knowledge
      !>     of the center-barycenter shifts to be performed, or a non-standard
      !>     method of determining the files and segments to be used when
      !>     computing states.)
      !>
      !>     This is the only reader which makes distinctions between the
      !>     various segment types in the SPK format. The complete list
      !>     of types currently supported is shown below.
      !>
      !>        Type   Description
      !>        ----   -----------------------
      !>           1   Difference Lines
      !>           2   Chebyshev (P)
      !>           3   Chebyshev (P,V)
      !>           5   Two body propagation between discrete states
      !>           8   Lagrange interpolation, equally spaced discrete states
      !>           9   Lagrange interpolation, unequally spaced discrete states
      !>          12   Hermite interpolation, equally spaced discrete states
      !>          13   Hermite interpolation, unequally spaced discrete states
      !>          14   Chebyshev Unequally spaced
      !>          15   Precessing Ellipse
      !>          17   Equinoctial Elements
      !>          18   ESOC/DDID Hermite/Lagrange Interpolation
      !>          19   ESOC/DDID Piecewise Interpolation
      !>          20   Chebyshev (V)
      !>          21   Extended Modified Difference Array
      !>
      !>     SPKPVN is the only reader that needs to be changed in order to
      !>     add a new segment type to the SPK format. If a new data type is
      !>     added, the following steps should be taken:
      !>
      !>     1) Write two new routines, SPKRnn and SPKEnn, to read and
      !>        evaluate, respectively, a record from a data type nn segment.
      !>
      !>     2) Insert a new case into the body of SPKPVN to accommodate the
      !>        new type.
      !>
      !>     3) If necessary, adjust the parameter MAXREC, above, so that it
      !>        is large enough to encompass the maximum size of a record
      !>        returned by SPKRnn and passed to SPKEnn.
      !>
      !>        The maximum record lengths for each data type currently
      !>        supported are as follows:
      !>
      !>                  Data type       Maximum record length
      !>                  ---------       ---------------------
      !>                      1                    71
      !>                      2                    87
      !>                      3                   171
      !>                      5                    15
      !>                      8                   171
      !>                      9                   197
      !>                     12                    87
      !>                     13                    99
      !>                     14                 Variable
      !>                     15                    16
      !>                     17                    12
      !>                     18                   198
      !>                     19                   198
      !>                     20                   159
      !>                     21                   112
      !>
      !>## Examples
      !>
      !>     In the following code fragment, an entire SPK file is searched
      !>     for segments containing a particular epoch. For each one found,
      !>     the body, center, segment identifier, and range at the epoch
      !>     are printed out.
      !>
      !>        CALL DAFOPR ( 'TEST.SPK', HANDLE )
      !>        CALL DAFBFS (             HANDLE )
      !>
      !>        CALL DAFFNA ( FOUND  )
      !>
      !>        DO WHILE ( FOUND )
      !>
      !>           CALL DAFGS ( DESCR )
      !>           CALL DAFUS ( DESCR, 2, 6, DC, IC )
      !>
      !>           IF ( DC(1) .LE. ET  .AND.  ET .LE. DC(2) ) THEN
      !>              CALL SPKPVN ( HANDLE, DESCR, ET, REF, STATE, CENTER )
      !>              CALL DAFGN  ( IDENT )
      !>              CALL FRMNAM ( REF, FRAME )
      !>              WRITE (*,*)
      !>              WRITE (*,*) 'Body   = ', IC(1)
      !>              WRITE (*,*) 'Center = ', CENTER,
      !>              WRITE (*,*) 'ID     = ', IDENT
      !>              WRITE (*,*) 'Frame  = ', FRAME
      !>              WRITE (*,*) 'Range  = ', VNORM ( STATE )
      !>           END IF
      !>
      !>           CALL DAFFNA ( FOUND )
      !>
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.0.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section. Improved $Abstract section.
      !>
      !>-    SPICELIB Version 4.0.0, 23-DEC-2013 (NJB)
      !>
      !>        Added support for types 19, 20 and 21. Added header
      !>        comments giving description for types 18, 19,
      !>        and 21. Removed header reference to type 4.
      !>
      !>-    SPICELIB Version 3.0.0, 16-AUG-2002 (NJB)
      !>
      !>        Added support for type 18. This routine now uses the
      !>        include file spkrec.inc to declare the record size.
      !>
      !>        Corrected header comments giving record sizes for types
      !>        8, 9, 12, 13.
      !>
      !>-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
      !>
      !>        Added support for types 12 and 13.
      !>
      !>-    SPICELIB Version 1.1.0, 07-JAN-1997 (WLT)
      !>
      !>        Added support for type 17.
      !>
      !>-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
      recursive module subroutine spkpvn ( ctx, handle, descr, et, ref, state, center )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spkrec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      integer(ip), intent(out) :: ref  !! Target reference frame.
      real(wp), intent(out) :: state    ( 6 )  !! Position, velocity.
      integer(ip), intent(out) :: center  !! Center of state.

!
!     Some local space is needed in which to return records, and
!     into which to unpack the segment descriptor.
!

      real(wp) :: record   ( maxrec )

      real(wp) :: dc       (      2 )
      integer(ip) :: ic       (      6 )
      integer(ip) :: recsiz
      integer(ip) :: type

      integer(ip), parameter :: nstate = 6

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpacking the segment descriptor will tell us the center,
!     reference frame, and data type for this segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      center = ic(2)
      ref    = ic(3)
      type   = ic(4)

!
!     Each data type has a pair of routines to read and evaluate
!     records for that data type. These routines are the only ones
!     that actually look inside the segments.
!
!     By the time we have more than 100 data types, we should be
!     allowed to use longer variable names.
!
      if ( type == 01 ) then
         call spkr01 ( ctx, handle, descr, et, record         )
         call spke01 ( ctx,                et, record, state  )

      else if ( type == 02 ) then
         call spkr02 ( ctx, handle, descr, et, record         )
         call spke02 ( ctx,                et, record, state  )

      else if ( type == 03 ) then
         call spkr03 ( ctx, handle, descr, et, record         )
         call spke03 ( ctx,                et, record, state  )

!
!     Type 04 is not officially part of the library.
!
!     ELSE IF ( TYPE .EQ. 04 ) THEN
!         CALL SPKR04 ( HANDLE, DESCR, ET, RECORD         )
!         CALL SPKE04 (                ET, RECORD, STATE  )

      else if ( type == 05 ) then
         call spkr05 ( ctx, handle, descr, et, record         )
         call spke05 ( ctx,                et, record, state  )

      else if ( type == 08 ) then
         call spkr08 ( ctx, handle, descr, et, record         )
         call spke08 ( ctx,                et, record, state  )

      else if ( type == 09 ) then
         call spkr09 ( ctx, handle, descr, et, record         )
         call spke09 ( ctx,                et, record, state  )

      else if ( type == 10 ) then
         call spkr10 ( ctx, handle, descr, et, record         )
         call spke10 ( ctx,                et, record, state  )

      else if ( type == 12 ) then
         call spkr12 ( ctx, handle, descr, et, record         )
         call spke12 ( ctx,                et, record, state  )

      else if ( type == 13 ) then
         call spkr13 ( ctx, handle, descr, et, record         )
         call spke13 ( ctx,                et, record, state  )

      else if ( type == 14 ) then
!
!        Fetch the number of Chebyshev coefficients, compute the record
!        size needed, and signal an error if there is not enough storage
!        in RECORD. The number of coefficients is the first constant
!        value in the generic segment.
!
         call sgfcon ( ctx, handle, descr, 1, 1, record(1) )

         if ( ctx%failed() ) return

         recsiz = nstate * int(record(1)) + 3

         if ( recsiz > maxrec ) then

            call ctx%setmsg ( 'Storage for # double precision numbers' &
            //            ' is needed for an SPK data record and' &
            //            ' only # locations were available. Update' &
            //            ' the parameter MAXREC in the subroutine' &
            //            ' SPKPVN and notify the NAIF group of this' &
            //            ' problem.'                                )
            call ctx%errint ( '#', recsiz                                )
            call ctx%errint ( '#', maxrec                                )
            call ctx%sigerr ( 'SPICE(SPKRECTOOLARGE)'                    )
            return

         end if

         call spkr14 ( ctx, handle, descr, et, record         )
         call spke14 ( ctx,                et, record, state  )

      else if ( type == 15 ) then
         call spkr15 ( ctx, handle, descr, et, record         )
         call spke15 ( ctx,                et, record, state  )

      else if ( type == 17 ) then
         call spkr17 ( ctx, handle, descr, et, record         )
         call spke17 ( ctx,                et, record, state  )

      else if ( type == 18 ) then
         call spkr18 ( ctx, handle, descr, et, record         )
         call spke18 ( ctx,                et, record, state  )

      else if ( type == 19) then
         call spkr19 ( ctx, handle, descr, et, record         )
         call spke19 ( ctx,                et, record, state  )

      else if ( type == 20) then
         call spkr20 ( ctx, handle, descr, et, record         )
         call spke20 ( ctx,                et, record, state  )

      else if ( type == 21) then
         call spkr21 ( ctx, handle, descr, et, record         )
         call spke21 ( ctx,                et, record, state  )

      else if ( type == 100 ) then
!
!        PEPPER: a programmatic ephemeris (ADD_EPHEMERIS): the virtual
!        segment's provider.
!
         call provided_state ( ctx, handle, et, state )

      else
         call ctx%setmsg ( 'SPK type # is not supported in your ' &
         //            'version of the SPICE library.  You will ' &
         //            'need to upgrade your version of the ' &
         //            'library to make use of ephemerides that ' &
         //            'contain this SPK data type. ' )
         call ctx%errint ( '#', type               )
         call ctx%sigerr ( 'SPICE(SPKTYPENOTSUPP)' )
         return
      end if

      return
      end subroutine spkpvn

      !> Return switch frame state or position transformation matrix.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     INFRM       is the frame ID code of switch frame for which
      !>                 the transformation to a base frame at time ET is
      !>                 sought.
      !>
      !>     ET          is a frame orientation evaluation epoch, expressed
      !>                 as seconds past J2000 TDB.
      !>
      !>     NDIM        is the common row and column dimension of the output
      !>                 matrix: 6 or 3. When NDIM is set to 6, a 6x6 state
      !>                 transformation matrix is returned; when NDIM is set
      !>                 to 3, a 3x3 position transformation matrix is
      !>                 returned.
      !>
      !>## Detailed Output
      !>
      !>     XFORM       is a matrix that transforms vectors of dimension NDIM
      !>                 from the switch frame designated by INFRM to the
      !>                 frame designated by OUTFRM, at time ET.
      !>
      !>                 By definition, a switch frame is aligned with a
      !>                 selected base frame at time ET. However, depending on
      !>                 the class of the selected base frame, OUTFRM may
      !>                 be set to that base frame, the base frame at ET of
      !>                 that base, or the frame J2000. See the description of
      !>                 OUTFRM below.
      !>
      !>                 XFORM is defined if and only if FOUND is .TRUE.
      !>
      !>     OUTFRM      is the frame ID code of a frame selected by this
      !>                 routine, which may be a base frame of the switch
      !>                 frame designated by INFRM, or which may be another
      !>                 frame, selected as described below. The output matrix
      !>                 XFORM transforms vectors of dimension NDIM from the
      !>                 switch frame designated by INFRM to the frame
      !>                 designated by OUTFRM.
      !>
      !>                 OUTFRM is defined if and only if FOUND is .TRUE.
      !>
      !>                 OUTFRM is selected as follows:
      !>
      !>                    - Base frames are obtained from the kernel
      !>                      variable
      !>
      !>                         FRAME_< INFRM >_ALIGNED_WITH
      !>
      !>                      Base frames are listed in order of ascending
      !>                      priority. A given frame may occur at multiple
      !>                      indices of the base frame array.
      !>
      !>                    - If time intervals are included in the switch
      !>                      frame specification, the highest priority
      !>                      base frame for which the associated time
      !>                      interval contains ET, and in which data are
      !>                      available at ET, is chosen.
      !>
      !>                      If time intervals are not provided, the highest
      !>                      priority base frame for which data are available
      !>                      at ET is chosen.
      !>
      !>                      Data availability may depend on NDIM. This is
      !>                      because 6x6 state transformations that rely on
      !>                      CK data require angular velocity to be present;
      !>                      3x3 position transformations don't.
      !>
      !>                      If no base frame providing data at ET is found
      !>                      by the process described above, FOUND is set to
      !>                      .FALSE. and OUTFRM is undefined.
      !>
      !>                    - If the process succeeds in finding a base frame
      !>                      providing data at ET, OUTFRM is set according to
      !>                      the class of the base frame:
      !>
      !>                         > Base is class INERTL: OUTFRM is set to
      !>                           the ID code of J2000.
      !>
      !>                         > Base is class PCK: OUTFRM is set to the ID
      !>                           code of J2000.
      !>
      !>                         > Base is class CK: OUTFRM is set to the ID
      !>                           code of the CK frame's base frame at time
      !>                           ET. This is the base returned by FRMGET for
      !>                           ET, if NDIM is 6, or the base returned by
      !>                           ROTGET at ET if NDIM is 3.
      !>
      !>                         > Base is class TK: OUTFRM is set to the ID
      !>                           code of the TK frame's base frame. This is
      !>                           the frame specified by the
      !>                           TKFRAME_<ID>_RELATIVE keyword of the TK
      !>                           frame's specification.
      !>
      !>                         > Base is class DYN: OUTFRM is set to the ID
      !>                           code of the base frame.
      !>
      !>                         > Base is class SWTCH: OUTFRM is set to the
      !>                           ID code of the base frame.
      !>
      !>     FOUND       is a logical flag indicating whether a frame
      !>                 transformation was found at time ET. XFORM and OUTFRM
      !>                 are valid if and only if FOUND is .TRUE.
      !>
      !>## Parameters
      !>
      !>     See the include file zzswtchf.inc for SPICE-private parameters
      !>     defining sizes of buffers used by the switch frame subsystem.
      !>
      !>## Exceptions
      !>
      !>     1)  If NDIM is a value other than 6 or 3, the error
      !>         SPICE(BADDIMENSION) will be signaled.
      !>
      !>     2)  If the switch frame designated by INFRM has more than MAXBAS
      !>         associated base frames, the error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     3)  If a frame specification cannot be found for a base frame of
      !>         the frame designated by INFRM, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     4)  If an error is found in the specification of the switch frame
      !>         designated by INFRM, the error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     5)  If an error occurs while computing the transformation from
      !>         the frame designated by INFRM to that designated by OUTFRM,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     Appropriate kernels must be loaded by the calling program before
      !>     this routine is called.
      !>
      !>     The following data are required:
      !>
      !>         - A switch frame specification, normally provided by a
      !>           frame kernel.
      !>
      !>         - Frame specifications for all base frames. Normally these
      !>           specifications are provided by frame kernels.
      !>
      !>     The following data may be needed:
      !>
      !>         - A leapseconds kernel
      !>
      !>         - SCLK kernel parameters and data for each CK base frame.
      !>           This information is normally provided by SCLK kernels.
      !>
      !>         - CK data for each CK base frame. These data are provided
      !>           by C-kernels.
      !>
      !>         - PCK data for each PCK base frame.
      !>
      !>         - Any kernels required to specify any dynamic base frame.
      !>
      !>     In all cases, kernel data are normally loaded once per program
      !>     run, NOT every time this routine is called.
      !>
      !>## Particulars
      !>
      !>     The switch frame subsystem does not participate in fake
      !>     recursion, hence there are no shadow routines corresponding to
      !>     ZZSWFXFM and ZZSWFFET.
      !>
      !>     The only frame orientation evaluations performed in the process
      !>     of computing the output transformation XFORM are those that
      !>     don't require recursive use of the frame subsystem. The frame
      !>     classes in this category are
      !>
      !>        Inertial
      !>        PCK
      !>        CK
      !>        TK
      !>
      !>     For dynamic and switch base frames, no orientation is computed.
      !>     Instead, the identity matrix is returned.
      !>
      !>## Examples
      !>
      !>     None. See the routines FRMGET and ROTGET for examples of usage.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine is SPICE-private. User applications must not
      !>         call it.
      !>
      !>     2)  To improve efficiency, this routine buffers switch frame
      !>         specifications. The buffers are emptied after any update
      !>         to the kernel pool. Since the process of parsing, and
      !>         buffering parameters provided by, switch frame specifications
      !>         is rather slow, applications using switch frames should
      !>         avoid high-frequency kernel pool updates.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>     B.V. Semenov   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB) (BVS)
      module subroutine zzswfxfm ( ctx, infrm, et, ndim, xform, outfrm, found )
      use pepper_containers, only: grow
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstled
      use pepper_math, only: ident
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzswtchf
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: infrm  !! Frame ID code of switch frame.
      real(wp), intent(in) :: et  !! Time, expressed as seconds past J2000 TDB.
      integer(ip), intent(in) :: ndim  !! Dimension: 3 or 6.
      real(wp), intent(out) :: xform ( ndim, ndim )  !! Transformation from switch to selected base frame.
      integer(ip), intent(out) :: outfrm  !! ID code of selected base frame.
      logical, intent(out) :: found  !! Flag indicating whether transformation was found.
      integer(ip), parameter :: j2code = 1
      integer(ip), parameter :: d3 = 3
      integer(ip), parameter :: d6 = 6
      real(wp) :: rmat   ( 3, 3 )
      real(wp) :: tsipm  ( 6, 6 )
      integer(ip) :: basidx
      integer(ip) :: framat
      integer(ip) :: head
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: lb
      integer(ip) :: lstbeg
      logical :: samfrm
      logical :: timeok
      logical :: update
      if ( .not. allocated ( ctx%zzswfxfm_state ) ) then
         allocate ( ctx%zzswfxfm_state )
         call init_zzswfxfm_state ( ctx%zzswfxfm_state )
      end if
!
!     PEPPER: make room in the base frame arrays for the base frames of
!     INFRM after those buffered (before the ASSOCIATE block: the arrays
!     may be reallocated).
!
      block
         character(40) :: kvalgn
         character(1) :: bastyp
         integer(ip) :: nbases
         logical :: found
         found = .false.
         if ( infrm /= ctx%zzswfxfm_state%prvfrm ) then
            write ( kvalgn, '(a,i0,a)' ) 'FRAME_', infrm, '_ALIGNED_WITH'
            call ctx%dtpool ( kvalgn, found, nbases, bastyp )
         end if
         if ( found ) then
            associate ( st => ctx%zzswfxfm_state )
            call grow ( st%baslst, st%free - 1 + nbases )
            call grow ( st%clsids, size ( st%baslst, kind=ip ) )
            call grow ( st%clsses, size ( st%baslst, kind=ip ) )
            call grow ( st%starts, size ( st%baslst, kind=ip ) )
            call grow ( st%stops,  size ( st%baslst, kind=ip ) )
            end associate
         end if
      end block
      associate ( basbeg => ctx%zzswfxfm_state%basbeg, &
                  bascnt => ctx%zzswfxfm_state%bascnt, &
                  baslst => ctx%zzswfxfm_state%baslst, &
                  clsids => ctx%zzswfxfm_state%clsids, &
                  clsses => ctx%zzswfxfm_state%clsses, &
                  binary => ctx%zzswfxfm_state%binary, &
                  fidlst => ctx%zzswfxfm_state%fidlst, &
                  free => ctx%zzswfxfm_state%free, &
                  frpool => ctx%zzswfxfm_state%frpool, &
                  hdfram => ctx%zzswfxfm_state%hdfram, &
                  pass1 => ctx%zzswfxfm_state%pass1, &
                  polctr => ctx%zzswfxfm_state%polctr, &
                  prvat => ctx%zzswfxfm_state%prvat, &
                  prvfrm => ctx%zzswfxfm_state%prvfrm, &
                  starts => ctx%zzswfxfm_state%starts, &
                  stops => ctx%zzswfxfm_state%stops, &
                  usetim => ctx%zzswfxfm_state%usetim, &
                  xident => ctx%zzswfxfm_state%xident )
      if ( ctx%failed() ) return

!
!     No frame found yet.
!
      found = .false.

      if ( pass1 ) then
!
!        Initialize pool data structures.
!
         call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                         free,   prvat,  prvfrm, samfrm  )
!
!        Initialize the local pool counter.
!
         call zzctruin ( ctx, polctr )
!
!        Note: no FAILED() check is needed after the two calls above.
!
!        Create a 6x6 identity matrix.
!
         xident = 0.0_wp

         do i = 1, 6
            xident(i,i) = 1.0_wp
         end do

         pass1 = .false.

      end if

!
!     Check the transformation dimension.
!
      if (  ( ndim /= d3 ) .and. ( ndim /= d6 )  ) then

         call ctx%setmsg ( 'Transformation dimension must ' &
         //            'be 3 or 6 but was #.'           )
         call ctx%errint ( '#', ndim                        )
         call ctx%sigerr ( 'SPICE(BADDIMENSION)'            )
         return

      end if

!
!     Reinitialize local database if the kernel pool has been
!     updated. The call below syncs POLCTR.
!
      call zzpctrck ( ctx, polctr, update )

      if ( update ) then
!
!        Initialize local data structures.
!
         call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                         free,   prvat,  prvfrm, samfrm  )

!
!        SAMFRM is set to .FALSE. and PRVFRM is set to 0 at this point.
!
      else
!
!        SAMFRM indicates that INFRM is the same switch frame we saw on
!        the previous call, no kernel pool update has occurred since
!        that call, and that the call was successful. We don't save the
!        previous switch frame information for unsuccessful calls: for
!        those, we set PRVFRM to zero so that it won't match any valid
!        frame ID.
!
         samfrm = ( infrm /= 0 ) .and. ( infrm == prvfrm )

      end if

      if ( samfrm ) then
!
!        We can use information saved from the previous call. FRAMAT is
!        the index of the switch frame ID in the switch frame ID array.
!
         framat = prvat

      else
!
!        Look up the input frame.
!
         call zzhsichk ( ctx, hdfram, frpool, fidlst, infrm, framat )

      end if

      if ( framat == 0 ) then
!
!        This frame is unknown to the switch frame subsystem. Try to
!        look up the frame's specification from the kernel pool.
!        ZZSWFFET checks and returns the frame specification.
!
         call zzswffet ( ctx, infrm,  hdfram, frpool, fidlst, basbeg, &
                         free,   bascnt, usetim, binary, clsses, &
                         clsids, baslst, starts, stops,  framat , size ( baslst, kind=ip ) )

         if (  ctx%failed()  .or.  ( framat == 0 )  ) then
!
!           We were unable to obtain information for the requested
!           frame. Initialize all information about the previous frame.
!           For safety, initialize the local database.
!
            call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                            free,   prvat,  prvfrm, samfrm  )

            return

         end if

      end if

!
!     At this point, FRAMAT is set.
!
!     Below, HEAD is the common index in each parallel array of base
!     frame data of the first item for a given switch frame. BASIDX is
!     the current index. The index obtained by adding a switch frame's
!     base frame count to HEAD is the successor of the last index of
!     base frame data for that switch frame.
!
      head = basbeg( framat )

!
!     At this point, the frame's information is present in the
!     local database. FRAMAT is the index of the frame in the hash
!     data structure. HEAD is the start index of the base frame data
!     for the input switch frame.
!
!     Search for the highest-priority frame that provides data
!     for the request time.
!
!     For safety, reset FOUND.
!
      found = .false.

      if ( binary(framat) ) then
!
!        We'll do a binary search. Suppress follow-on linear searching
!        and frame data lookup by default: when BASIDX is zero, the
!        linear search and data fetch loop won't execute. We'll update
!        BASIDX if an interval is found.
!
         basidx = 0

!
!        Find the last interval, if any, that contains ET. Start by
!        finding the last start time that's less than or equal to ET.
!
!        Here we rely on the assumption that start and stop times for
!        each frame are stored in increasing time order.
!
         lstbeg = lstled( et, bascnt(framat), starts(head) )

!
!        If LSTBEG is zero, then ET is strictly less than all of the
!        start times, in which case there's no match.
!
         if ( lstbeg > 0 ) then
!
!           ET could belong to the interval having start time stored at
!           relative index LSTBEG, which is equivalent to the base
!           frame array index HEAD+LSTBEG-1.
!
            if (  et <= stops( head+lstbeg-1 )  ) then
!
!              This interval contains ET. Prepare to fetch data for
!              the base frame corresponding to this interval.
!
               basidx = head + lstbeg - 1

            end if

         end if

      else
!
!        Prepare for a linear search and fetching base frame data.
!
         basidx = head + bascnt(framat) - 1

      end if

!
!     Search the base list in order of decreasing priority. The search
!     will terminate once we've examined the base frame at index LB,
!     if not before.
!
      lb = head

      do while (  ( basidx >= lb ) .and. ( .not. found )  )

         if (  usetim( framat )  ) then

            timeok =       ( et >= starts(basidx) ) &
                     .and. ( et <= stops (basidx) )
         else
            timeok = .false.
         end if

         if (  timeok .or. ( .not. usetim(framat) )  ) then
!
!           This base frame is applicable.
!
            if ( clsses(basidx) == ck ) then
!
!              We can't know whether there are CK data for the request
!              time without looking up the data, so that's what we do.
!              If we find data, we return the transformation to the CK
!              frame's base frame and the frame ID of the CK frame's
!              base frame.
!
               if ( ndim == d6 ) then

                  call ckfxfm ( ctx, clsids(basidx), et, &
                                xform, outfrm,  found )
               else

                  call ckfrot ( ctx, clsids(basidx), et, &
                                xform, outfrm,  found )
               end if

               if ( ctx%failed() ) then

                  call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm  )

                  return

               end if

!
!              If we got this far and we're in binary search mode, we
!              won't find a result, except possibly when ET is equal to
!              the interval start time.
!
               if ( binary(framat) .and. ( .not. found ) ) then

                  if ( et > starts(basidx) ) then
!
!                    The next lowest-indexed interval doesn't cover ET,
!                    so no data can be found to satisfy this request.
!
                     prvfrm = 0
                     prvat  = 0

                     return

                  end if

               end if
!
!              FOUND is set. If we did find data, XFORM and OUTFRM
!              are set. If FOUND is .FALSE., we'll look at the
!              remaining lower-priority bases, if any, if we're not in
!              binary search mode. If we're in binary search mode,
!              we'll look at the next lower-priority base only if ET is
!              equal to the interval start time.
!

            else if ( clsses(basidx) == tk ) then

               call tkfram ( ctx, clsids(basidx), rmat, outfrm, found )

               if ( ctx%failed() ) then

                  found  = .false.

                  call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm  )

                  return

               end if

               if ( ndim == d6 ) then

                  do i = 1,3
                     do j = 1,3
                        xform(i,  j  ) = rmat(i,j)
                        xform(i+3,j+3) = rmat(i,j)
                        xform(i+3,j  ) = 0.0_wp
                        xform(i,  j+3) = 0.0_wp
                     end do
                  end do

               else
                  xform = rmat
               end if
!
!              OUTFRM and FOUND have been set by TKFRAM.
!

            else if ( clsses(basidx) == pck ) then

               if ( ndim == d6 ) then

                  call tisbod ( ctx, 'J2000', clsids(basidx), et, tsipm )
                  call invstm ( ctx, tsipm, xform )
               else
                  call tipbod ( ctx, 'J2000', clsids(basidx), et, rmat  )
                  xform = transpose(rmat)
               end if

               if ( ctx%failed() ) then

                  found  = .false.

                  call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm  )

                  return

               end if

               outfrm = j2code
               found  = .true.

            else if ( clsses(basidx) == inertl ) then

               call irfrot ( ctx, baslst(basidx), j2code, rmat )

               if ( ctx%failed() ) then

                  found  = .false.

                  call zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, &
                                  free,   prvat,  prvfrm, samfrm )

                  return

               end if

               if ( ndim == d6 ) then

                  do i = 1,3
                     do j = 1,3
                        xform(i,  j  ) = rmat(i,j)
                        xform(i+3,j+3) = rmat(i,j)
                        xform(i+3,j  ) = 0.0_wp
                        xform(i,  j+3) = 0.0_wp
                     end do
                  end do

               else
                  xform = rmat
               end if

               outfrm = j2code
               found  = .true.

            else
!
!              For other non-CK frames---switch and dynamic---we just
!              return the base frame and the identity matrix.
!
               if ( ndim == d6 ) then

                  xform = xident
               else
                  call ident ( xform )
               end if

               outfrm = baslst( basidx )
               found  = .true.

            end if

         end if

!
!        If we got this far and we're in binary search mode,
!        we've already found a result. We'll exit the loop at
!        the next loop condition test.
!
!        Check the next lower priority base frame and interval.
!
         basidx = basidx - 1

      end do

!
!     FOUND is set.
!
!     We either found data, or we ran out of base frames to try.
!     If we found data, the outputs XFORM and OUTFRM are set.
!
      if ( found ) then
!
!        Save values for re-use.
!
         prvfrm = infrm
         prvat  = framat

      else
!
!        Ensure the saved values won't be re-used.
!
         prvfrm = 0

      end if

      return
      end associate
      end subroutine zzswfxfm

end submodule pepper_orientation
