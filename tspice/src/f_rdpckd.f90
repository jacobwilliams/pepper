!$Procedure      F_RDPCKD ( Text PCK reader tests )

      SUBROUTINE F_RDPCKD ( OK )
      use pepper_legacy

! $ Abstract
!
!     This family runs tests on a text PCK based the draft IAU WGCCRE
!     report dated 2017 September 14, and runs tests on the PCK
!     reader software that makes use of it.
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OK         O   Test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK             is a boolean flag indicating test status. OK is
!                    returned .TRUE. only if all tests conducted by
!                    this family have passed.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Not applicable.
!
! $ Files
!
!     The data provided by this routine are meant to be consistent with
!     the current IAU report [1] and should be consistent with the NAIF
!     text PCK
!
!        pck00011.tpc
!
!     This routine must be updated along with each NAIF text PCK update.
!
! $ Particulars
!
!     This routine tests a version of the current NAIF text PCK file
!     which is generated at run time by the test utility T_PCKnn.
!
!     Orientation data are checked by means of calls to the SPICELIB
!     routines
!
!        BODEUL
!        BODMAT
!        TIPBOD
!        TISBOD
!
!     Results of computations done by these routines are compared
!     with results of computations implemented in-line in the
!     test utility routine T_PCKEQ.  This testing approach attempts
!     to validate orientation computations done using text PCK data
!     by comparing their results to those obtained via an alternate
!     computational approach.
!
!     This routine also tests radii data provided by the PCK, and
!     checks that the sets of bodies for which orientation and radii
!     data are provided by the PCK match the corresponding sets for
!     the IAU report [1].
!
!     These tests also serve to minimize the chance of transcription
!     error when a new PCK version is created. Entering the PCK
!     constants both in the new PCK file and in T_PCKEQ enables
!     double-checking of the constants.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) This routine must be kept consistent with the both current IAU
!        report [1] and the current NAIF text PCK. Currency is achieved
!        by calling a current version of T_PCKEQ and the current PCK
!        generation utility T_PCKnn.
!
!
! $ Literature_References
!
!     [1]   Seidelmann, P.K., Archinal, B.A., A'Hearn, M.F.,
!           Conrad, A., Consolmagno, G.J., Hestroffer, D.,
!           Hilton, J.L., Krasinsky, G.A., Neumann, G.,
!           Oberst, J., Stooke, P., Tedesco, E.F., Tholen, D.J.,
!           and Thomas, P.C. "Report of the IAU/IAG Working Group
!           on cartographic coordinates and rotational elements: 2006."
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    TSPICE Version 5.0.0, 16-DEC-2021 (NJB)
!
!-&

!
!     SPICELIB functions
!



!
!     Local parameters
!
      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 2.D-10 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               MAXSET
      PARAMETER           ( MAXSET = 1000 )

      INTEGER               NUMLEN
      PARAMETER           ( NUMLEN = KVNMLN )

!
!     Maximum number of phase angles
!
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 200 )

!
!     Maximum phase angle degree
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 3 )

!
!     Size of phase angle coefficient buffer
!
      INTEGER               MAXBUF
      PARAMETER           ( MAXBUF = MAXANG * (MAXDEG+1) )

!
!     Local variables
!
      CHARACTER*(1)         DTYPE
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(NUMLEN)    IDSTR
      CHARACTER*(KVNMLN)    KVDEG
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(KVNMLN)    KVPHAS
      CHARACTER*(KVNMLN)    KVNAMS ( LBCELL : MAXSET )
      CHARACTER*(FILSIZ)    PCK
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(KVNMLN)    XNAME

      DOUBLE PRECISION      PHSBUF ( MAXBUF )
      DOUBLE PRECISION      B2J    ( 3, 3 )
      DOUBLE PRECISION      CMAT   ( 2, 2 )
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DETM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EUL0   ( 3 )
      DOUBLE PRECISION      EUL2   ( 3 )
      DOUBLE PRECISION      EULSTA ( 6 )
      DOUBLE PRECISION      J2B    ( 3, 3 )
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      MI     ( 2, 2 )
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RJ2E   ( 3, 3 )
      DOUBLE PRECISION      RVEC   ( 2 )
      DOUBLE PRECISION      SAVBUF ( MAXBUF )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TC
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TIPM2  ( 3, 3 )
      DOUBLE PRECISION      TJ2E   ( 6, 6 )
      DOUBLE PRECISION      TMPMAT ( 3, 3 )
      DOUBLE PRECISION      TMPXF  ( 6, 6 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      TSIPM2 ( 6, 6 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      XDEC
      DOUBLE PRECISION      XDTIPM ( 3, 3 )
      DOUBLE PRECISION      XLMBDA
      DOUBLE PRECISION      XRA
      DOUBLE PRECISION      XRAD   ( 3 )
      DOUBLE PRECISION      XTIPM  ( 3, 3 )
      DOUBLE PRECISION      XTSIPM ( 6, 6 )
      DOUBLE PRECISION      XW
      DOUBLE PRECISION      ZMAT   ( 3, 3 )

      INTEGER               ANGSET ( LBCELL : MAXSET )
      INTEGER               CODE
      INTEGER               H
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               XT
      INTEGER               RADSET ( LBCELL : MAXSET )
      INTEGER               REFID
      INTEGER               XSET   ( LBCELL : MAXSET )

      LOGICAL               FOUND
      LOGICAL               MODOK

!
!     Save all variables.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_RDPCKD' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create full text PCK file.' )


      PCK = 'test_0011d.tpc'

      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL  ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the PCK file, load it, and delete it.
!
      CALL T_PCK11D ( PCK, .TRUE., .TRUE. )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL  ( PCK )


! ***************************************************************
!
!     Rotation tests
!
! ***************************************************************


!
!     Check the set of bodies for which rotation data are provided.
!     Find the set of kernel variables for prime meridian coefficients;
!     check the corresponding set of ID codes against the set
!     from the test utility T_PCKASTD.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check the set of bodies for which ' &
      //           'rotation data are provided.'        )


      CALL SSIZEI ( MAXSET, XSET   )
      CALL SSIZEI ( MAXSET, ANGSET )

      CALL T_PCKASTD ( XSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GNPOOL ( 'BODY*_PM', 1, MAXSET, N, KVNAMS(1), FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VALIDC ( MAXSET, N, KVNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Remove all of the nutation precession PM names.
!
      I = 1

      DO WHILE ( I .LE. CARDC(KVNAMS) )

         IF (  INDEX( KVNAMS(I), 'NUT_PREC' )  .GT. 0  ) THEN

            CALL REMOVC( KVNAMS(I), KVNAMS )

         ELSE
            I = I + 1
         END IF

      END DO

      CALL CHCKSL ( 'PM names found', FOUND, .TRUE., OK )

      N = CARDC(KVNAMS)

      CALL CHCKSI ( 'Number of PM names', N,          '=', &
                                          CARDI(XSET), 0,   OK )

!
!     Check each name in the expected set for presence in the
!     actual name set.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bodies in set expected to have ' &
      //           'rotation data against those present ' &
      //           'in the kernel.'                         )


      DO I = 1, CARDI(XSET)

         XNAME = 'BODY#_PM'

         CALL REPMI ( XNAME, '#', XSET(I), XNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the expected name is in the kernel
!           variable list.
!
         J = BSRCHC( XNAME, N, KVNAMS(1) )

         QNAME = '# is present in kernel?'
         CALL REPMC ( QNAME, '#', XNAME, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( QNAME, (J .GT. 0), .TRUE., OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bodies having rotation data in ' &
      //           'kernel against those in the expected ' &
      //           'set.'                                 )

!
!     Check each name in the actual set for presence in the
!     expected name set.
!
      DO I = 1, CARDC(KVNAMS)
!
!        Get the ID code of the current name.
!
         IDSTR = KVNAMS(I)(5:)

         J = INDEX( IDSTR, '_' )

         CALL PRSINT ( IDSTR(:J-1), CODE )

         QNAME = '# is present in expected set?'
         CALL REPMI ( QNAME, '#', CODE, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, ELEMI(CODE,XSET), .TRUE., OK )

      END DO

!
!     Since we've checked the set of bodies, let the actual
!     set be the expected set.
!
      CALL COPYI ( XSET, ANGSET )

!
!     Delete body -10 from the set used for the normal test cases.
!
      CALL REMOVI ( -10, ANGSET )


!
!     Perform BODEUL tests.
!
!     A note concerning the looser tolerance value MEDIUM used
!     in these tests:  converting Euler angles to rotation matrices
!     involves applying the SIN and COS functions to the angles.
!     In the case of the prime meridian angle W, applying trig
!     functions increases the relative error, since W may start
!     out with magnitude much larger than 2*pi.  The error in W
!     is the largest source of error in the matrix TIPM.
!
      DO I = 1, CARDI(ANGSET)

         DO XT = -5, 5
!
! --- Case: ------------------------------------------------------
!
            ET = XT * 10 * JYEAR()

            LINE =  'BODEUL test: Check Euler angles for # at ET #.'

            CALL REPMI ( LINE, '#', ANGSET(I), LINE )
            CALL REPMD ( LINE, '#', ET,  14,   LINE )
            CALL CHCKXC  ( .FALSE., ' ', OK )

            CALL TCASE ( LINE )


            CALL T_PCKANGD ( ANGSET(I), ET,  XRA, XDEC, XW )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            XW = MOD ( XW, TWOPI() )

            IF ( XW .LT. 0 ) THEN
               XW = XW + TWOPI()
            END IF

            XRA = MOD ( XRA, TWOPI() )

            IF ( XRA .LT. 0 ) THEN
               XRA = XRA + TWOPI()
            END IF


            CALL BODEUL  ( ANGSET(I), ET,  RA,  DEC,  W, LAMBDA )
            CALL CHCKXC  ( .FALSE., ' ', OK )

            CALL CHCKSD ( 'RA',   RA,  '~', XRA,  TIGHT , OK )
            CALL CHCKSD ( 'DEC',  DEC, '~', XDEC, TIGHT , OK )
            CALL CHCKSD ( 'W',    W,   '~', XW,   MEDIUM, OK )

         END DO

      END DO


!
!     Perform analogous tests on both BODMAT and TIPBOD. In order to
!     compare results against those from T_PCKANGD, we represent results
!     from the latter routine as rotation matrices.
!
!     Obtain the position and state transformations from the J2000
!     to the ECLIPJ2000 frame.  These will be used later.
!
      CALL PXFORM ( 'J2000', 'ECLIPJ2000', 0.D0, RJ2E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'J2000', 'ECLIPJ2000', 0.D0, TJ2E )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, CARDI(ANGSET)

            DO XT = -5, 5
!
! --- Case: ------------------------------------------------------
!
               ET = XT * 10 * JYEAR()

               LINE =  'BODMAT test: Check attitude matrix for # at '// &
                       'ET #.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL TCASE ( LINE )

               CALL T_PCKANGD ( ANGSET(I), ET,  XRA, XDEC, XW )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL EUL2M ( XW, HALFPI()-XDEC, HALFPI()+XRA, &
                            3,  1,             3,            XTIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

!
!              Test BODMAT.
!
               CALL BODMAT  (         ANGSET(I), ET, TIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )


!
! --- Case: ------------------------------------------------------
!
!
!              Test TIPBOD.
!
               LINE =  'TIPBOD test: Check attitude matrix for # ' // &
                       'at ET #.'
               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL TCASE ( LINE )

               CALL TIPBOD  ( 'J2000', ANGSET(I), ET, TIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
!
!              Repeat the TIPBOD test using a non-native inertial
!              frame. Right-multiply the matrix obtained from TIPBOD by
!              the mapping from J2000 to the non-native frame; this
!              should give us the same transformation we'd have had if
!              we had looked up the mapping from J2000 to the
!              body-fixed frame directly.
!
               CALL TIPBOD  ( 'ECLIPJ2000', ANGSET(I), ET, TIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL MXM   ( TIPM,   RJ2E, TMPMAT )
               CALL MOVED ( TMPMAT, 9,    TIPM   )

               CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

            END DO

      END DO


!
!     Perform analogous tests on TISBOD.  In order to compare
!     results against those from T_PCKANGD, we represent results
!     from the latter routine as rotation matrices.
!
!     We use discrete derivatives of the Euler angles to examine the
!     derivative portion of the state transformation matrix.
!
      CALL CLEARD ( 9, ZMAT )


      DO I = 1, CARDI(ANGSET)

            DO XT = -5, 5
!
! --- Case: ------------------------------------------------------
!
               ET = XT * 10 * JYEAR()

               LINE =  'TISBOD: check state transformation matrix ' // &
                       'for # at ET #.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )


               CALL T_PCKANGD ( ANGSET(I), ET,  XRA, XDEC, XW )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL EUL2M ( XW, HALFPI()-XDEC, HALFPI()+XRA, &
                            3,  1,             3,            XTIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TISBOD  ( 'J2000', ANGSET(I), ET, TSIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

!
!              We test the blocks of the state transformation matrix
!              separately, since we don't use identical tolerances for
!              all blocks.
!
               DO J = 1, 3
                  DO K = 1, 3
                     TIPM(J,K) = TSIPM(J,K)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM (upper left block)', &
                              TIPM, '~', XTIPM, 9, MEDIUM, OK )

               DO J = 1, 3
                  DO K = 1, 3
                     TIPM(J,K) = TSIPM(J+3,K+3)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM (lower right block)', &
                              TIPM, '~', XTIPM, 9, MEDIUM, OK )

               DO J = 1, 3
                  DO K = 1, 3
                     TMPMAT(J,K) = TSIPM(J,K+3)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM (upper right block)', &
                              TMPMAT, '~', ZMAT, 9, MEDIUM, OK )

!
!              To test the derivative block, we'll first estimate the
!              derivatives of the Euler angles using a quadratic
!              approximation.  We'll sample the angles at +/- 1 second
!              from ET.
!
               DELTA = 1.D0
!
!              Get the left side angles...
!
               CALL T_PCKANGD ( ANGSET(I), ET-DELTA, XRA, XDEC, XW )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               EUL0(1) = XW
               EUL0(2) = HALFPI() - XDEC
               EUL0(3) = HALFPI() + XRA

!
!              The right side angles...
!
               CALL T_PCKANGD ( ANGSET(I), ET+DELTA, XRA, XDEC, XW )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               EUL2(1) = XW
               EUL2(2) = HALFPI() - XDEC
               EUL2(3) = HALFPI() + XRA

!
!              Find the derivatives.
!
               CALL QDERIV ( 3, EUL0, EUL2, DELTA, EULSTA(4) )

!
!              Complete the Euler angle state vector by filling in the
!              first three components with the angles at ET.
!
               CALL T_PCKANGD ( ANGSET(I), ET, XRA, XDEC, XW )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL VPACK ( XW, HALFPI()-XDEC, HALFPI()+XRA, EULSTA )

!
!              Now find the expected state transformation at ET.
!
               CALL EUL2XF ( EULSTA, 3, 1, 3, XTSIPM )

!
!              Extract the derivative block from the expected state
!              transformation.
!
               DO J = 1, 3
                  DO K = 1, 3
                     XDTIPM(J,K) = XTSIPM(J+3,K)
                  END DO
               END DO

!
!              Do the same for the transformation returned from TISBOD.
!
               DO J = 1, 3
                  DO K = 1, 3
                     TMPMAT(J,K) = TSIPM(J+3,K)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM derivative (lower left block)', &
                              TMPMAT, '~', XDTIPM, 9, MEDIUM, OK )


!
! --- Case: ------------------------------------------------------
!
!
!              Repeat the TISBOD test using a non-native input frame.
!
               LINE =  'TISBOD: check state transformation matrix '  // &
                       'for # at ET #.  Use ECLIPJ2000 as input frame.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )
               CALL TISBOD ( 'ECLIPJ2000', ANGSET(I), ET, TSIPM2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXMG   ( TSIPM2,  TJ2E, 6, 6, 6, TMPXF  )
               CALL MOVED  ( TMPXF,   36,            TSIPM2 )

!
!              Compare against TSIPM from the previous test.
!
               CALL CHCKAD ( 'TSIPM2', &
                             TSIPM2, '~', TSIPM, 36, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
!
!              Repeat the TISBOD test using the max phase angle
!              keyword. This test exercises the logic for recognition
!              of the keyword, but not that for higher-order phase
!              angle expressions.
!
               LINE =  'TISBOD: check state transformation matrix ' &
               //      'for # at ET #.  Use ECLIPJ2000 as input frame.' &
               //      ' Set max phase angle degree via keyword.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )

!
!              Insert the keyword for BODY#_MAX_PHASE_DEGREE for this
!              body into the kernel pool.
!
               CALL REPMI ( 'BODY#_MAX_PHASE_DEGREE', '#', &
                            ANGSET(I),             KVNAME )
               CALL CHCKXC  ( .FALSE., ' ', OK )

!
!              Set the maximum degree to the default, which is 1.
!
               CALL PIPOOL ( KVNAME, 1, [1] )
               CALL CHCKXC  ( .FALSE., ' ', OK )


               CALL TISBOD ( 'ECLIPJ2000', ANGSET(I), ET, TSIPM2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXMG   ( TSIPM2,  TJ2E, 6, 6, 6, TMPXF  )
               CALL MOVED  ( TMPXF,   36,            TSIPM2 )

!
!              Compare against TSIPM from the previous test.
!
               CALL CHCKAD ( 'TSIPM2', &
                             TSIPM2, '~', TSIPM, 36, MEDIUM, OK )

!
!              Delete the phase angle degree keyword.
!
               CALL DVPOOL ( KVNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
!
!              Repeat the TISBOD test using the max phase angle
!              keyword. In this test, we'll modify the phase angle
!              coefficients so as to produce cubic polynomials that
!              yield the same angles and angular rates as the originals
!              for the specific, known input time.
!
!              We'll do this test only for bodies for which the phase
!              angle degree is 1.
!
!              We're going to go to some trouble to create the
!              polynomial coefficients, so in this test case we'll
!              test BODEUL using them as well.
!
               LINE =  'TISBOD and BODEUL: check state transformation ' &
               //      'matrix and Euler angles for # at ET #. Use ' &
               //      'J2000 as input frame. Generate cubic phase ' &
               //      'angle polynomials and use those instead of ' &
               //      'the originals.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )

!
!              Get the expected results.
!
               CALL TISBOD ( 'J2000', ANGSET(I), ET, TSIPM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TIPBOD ( 'J2000', ANGSET(I), ET, TIPM  )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODEUL ( ANGSET(I), ET, XRA, XDEC, XW, XLMBDA )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Perform this modification only for angle sets that
!              don't have the degree keyword.
!
!              Get the body ID associated with the phase angles, if
!              there is one.
!
               REFID = ZZBODBRY( ANGSET(I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

               MODOK =  BODFND( REFID, 'NUT_PREC_ANGLES'  ) &
                           .AND. .NOT. &
                        BODFND( REFID, 'MAX_PHASE_DEGREE' )

               IF ( MODOK ) THEN
!
!                 Fetch the original phase angle coefficients.
!
                  KVPHAS = 'BODY#_NUT_PREC_ANGLES'
                  CALL REPMI ( KVPHAS, '#', REFID, KVPHAS )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

                  CALL GDPOOL ( KVPHAS, 1, MAXBUF/2, N, SAVBUF, FOUND )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
!
!                 Insert the keyword for BODY#_MAX_PHASE_DEGREE for
!                 this body into the kernel pool, setting the phase
!                 angle polynomial degree to 3.

                  CALL REPMI ( 'BODY#_MAX_PHASE_DEGREE', '#', &
                               REFID,                    KVDEG )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

                  CALL PIPOOL ( KVDEG, 1, [3] )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

!
!                 Each original phase angle polynomial has the form
!
!                    A  +  TC*B
!
!                 where TC is ET in units of Julian centuries.
!                 Given TC, we can create coefficients that define
!                 a cubic polynomial that has the same value and
!                 derivative with respect to ET, at TC. We'll choose
!                 the coefficients
!
!                    A2, B2, C2, D2
!
!                 where if TC is 0,
!
!                    A2 = A
!                    B2 = B
!                    C2 = 0
!                    D2 = 0
!
!                 and otherwise
!
!                    A2 = A
!                    B2 = B/2
!
!                    C2, D2 are chosen so that the polynomial value
!                    and derivative wrt. ET match, i.e.
!
!                       C2 * TC**2    + D2 * TC**3         =  TC * B/2
!                       C2 * 2 * TC/T + D2 * 3 * TC**2/T   = (B/2) / T
!
!                   where TC = ET / T.
!
!                   We solve for C2 and D2 by treating this as a matrix
!                   equation:
!
!                     ._         _.
!                     |  M11  M12 |       | C2 |     | R1 |
!                     |           |   *   |    |  =  |    |
!                     |  M21  M22 |       | D2 |     | R2 |
!                     `-         -'
!
!                   where
!
!                      M11 = TC**2
!                      M12 = TC**3
!                      M21 = 2 * TC/T
!                      M22 = 3 * TC**2 / T
!
!                      R1  = TC * B/2
!                      R2  = (B/2) / T
!
!                   Then multiplying both sides of the equation by
!                   the matrix inverse (provided it exists) yields
!                   the solution.
!
!                 We'll store the new coefficients in PHSBUF.
!
                  T      = 100 * JYEAR()
                  TC     = ET / T
                  NCOEFF = N/2

                  DO J = 1, NCOEFF

                     H = 2*(J-1) + 1
                     K = 4*(J-1) + 1
!
!                    Set A2.
!
                     PHSBUF(K) = SAVBUF(H)
!
!                    Set B2, C2 and D2.
!
                     IF ( TC .EQ. 0.D0 ) THEN

                        PHSBUF(K+1) = SAVBUF(H+1)
                        PHSBUF(K+2) = 0.D0
                        PHSBUF(K+3) = 0.D0

                     ELSE

                        PHSBUF(K+1) = SAVBUF(H+1) * 0.5D0

                        CMAT(1,1) = TC**2
                        CMAT(1,2) = TC**3
                        CMAT(2,1) = 2 *  TC/T
!
!                       d(TC**3)/dET = 3 * TC**2 / T. N0067 had
!                       3 * (TC/T)**2, the same mistake as TISBOD's
!                       (see spicelib_bugs.md), so the two agreed.
!
                        CMAT(2,2) = 3 * TC**2 / T

                        RVEC(1) = TC * SAVBUF(H+1) / 2
                        RVEC(2) = ( SAVBUF(H+1) / 2 ) / T

                        DETM =   CMAT(1,1)*CMAT(2,2) &
                               - CMAT(1,2)*CMAT(2,1)

                        IF ( DETM .EQ. 0.D0 ) THEN
!
!                          It's very unlikely we arrive here, but we
!                          can't continue if we do. Force the test
!                          system to catch the error.
!
                           CALL SETMSG ( 'Cubic polynomial solution ' &
                           //            'matrix is singular.' )
                           CALL SIGERR ( 'SPICE(TESTERROR)'    )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                        ELSE
!
!                          Let MI be the inverse of the matrix M above.
!                          Use the 2x2 matrix inverse formula.
!
                           MI(1,1) =  CMAT(2,2) / DETM
                           MI(1,2) = -CMAT(1,2) / DETM
                           MI(2,1) = -CMAT(2,1) / DETM
                           MI(2,2) =  CMAT(1,1) / DETM

                           CALL MXVG ( MI, RVEC, 2, 2, PHSBUF(K+2) )

                        END IF

                     END IF

                  END DO

!
!                 Insert the new coefficients into the kernel pool.
!
                  CALL PDPOOL ( KVPHAS, 4*NCOEFF, PHSBUF )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TISBOD ( 'J2000', ANGSET(I), ET, TSIPM2 )
                  CALL CHCKXC ( .FALSE., ' ', OK )


                  CALL TIPBOD ( 'J2000', ANGSET(I), ET, TIPM2 )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Compare against TIPM and TSIPM computed using the
!                 original phase angle coefficients.
!
!
!                 With the corrected cubic (C2 = B/TC, D2 = -B/(2*TC**2)),
!                 the angle is a sum of terms of size B*TC and B*TC/2,
!                 a few hundred radians for fast angles (Saturn), so it
!                 rounds to about 1e-13: VTIGHT is too tight here.
!
                  CALL CHCKAD ( 'TIPM2', &
                                 TIPM2, '~',  TIPM,   9, 1.D-13, OK )

                  CALL CHCKAD ( 'TSIPM2', &
                                 TSIPM2, '~', TSIPM, 36, TIGHT,  OK )

!
!                 Check angles from BODEUL against expected values.
!
                  CALL BODEUL ( ANGSET(I), ET, RA, DEC, W, LAMBDA )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSD ( 'RA',  RA,  '~', XRA,  1.D-13, OK )
                  CALL CHCKSD ( 'DEC', DEC, '~', XDEC, 1.D-13, OK )
                  CALL CHCKSD ( 'W',   W,   '~', XW,   1.D-13, OK )

!
!                 LAMBDA is not affected by the phase angles. We
!                 should get an exact match.
!
                  CALL CHCKSD ( 'LAMBDA', LAMBDA, '~', &
                                          XLMBDA, 0.D0, OK )

!
!                 Restore the original phase angle coefficients.
!
                  CALL PDPOOL ( KVPHAS, 2*NCOEFF, SAVBUF )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Delete the phase angle degree keyword. Note that
!                 this step is valid only if the keyword wasn't
!                 present to begin with.
!
                  CALL DVPOOL ( KVDEG )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

            END DO
!
!           We've tested TISBOD for the current time value.
!
!
!        We've tested TISBOD for the current body.
!
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TISBOD error case: invalid phase degree' )

!
!     Insert the keyword for BODY#_MAX_PHASE_DEG for this
!     body into the kernel pool.
!
!     For this test to work, the barycenter must be one for which
!     there are associated phase angles.
!
      REFID = 3

      CALL REPMI ( 'BODY#_MAX_PHASE_DEGREE', '#', REFID, KVDEG )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( KVDEG, 1, [4] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'J2000', 399, 0.D0, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(DEGREEOUTOFRANGE)', OK )

      CALL PIPOOL ( KVDEG, 1, [0] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'J2000', 399, 0.D0, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(DEGREEOUTOFRANGE)', OK )

      CALL DVPOOL ( KVDEG )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
!
!     Now check TIPBOD'S ability to deal with non-standard epochs and
!     frames.  Look up data for body -10 (these data are created by the
!     test utility T_PCK10) at the B1950 epoch in the B1950 frame.
!     Object -10 uses the same constants as those for the sun, but for
!     body -10, the native frame is B1950 and the reference epoch is
!     B1950.  The output should match the standard numbers for the sun.
!
!     NOTE: the current implementation of TIPBOD (as of N0067) relies
!     on TISBOD, so these checks are not strictly necessary at this
!     time. They may become necessary later.
!
!
      CALL TCASE ( 'TIPBOD lookup of matrix for body -10' )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT (           10, 0.D0, XTIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TIPBOD error case: lookup of matrix for body ' &
      //           '-10 when both *CONSTANTS_JED_EPOCH and ' &
      //           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TIPBOD error case: lookup of matrix for body ' &
      //           '-10 when both *_CONSTANTS_REF_FRAME and ' &
      //           '*CONSTS_REF_FRAME keywords are present.' )

!
!     Delete competing epoch spec first.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
!     Perform a normal TIPBOD computation using the _CONSTS_ forms of
!     the epoch and base frame keywords.
!
      CALL TCASE ( 'TIPBOD: lookup B1950 to body-fixed transformation ' &
      //           'matrix for body -10 using both *CONSTS_JED_EPOCH ' &
      //           'and *CONSTS_REF_FRAME keywords.' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the new keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
!     Look up the transformation from J2000 to IAU_SUN for body 10
!     at the J2000 epoch. This should match the transformation from
!     the B1950 frame to IAU_SUN for body -10 at the B1950 epoch,
!     since in both cases the input time is the epoch of the constants,
!     and both bodies have identical rotational elements.
!
      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT (           10, 0.D0, XTIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, TIGHT, OK )

!
! --- Case: ------------------------------------------------------
!
!
!     Repeat the above sequence of tests for TISBOD.
!
      CALL TCASE ( 'TISBOD lookup of matrix for body -10' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore the old keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'J2000', 10, 0.D0, XTSIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL TISBOD ( 'B1950', -10, ET, TSIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TSIPM',  TSIPM, '~', XTSIPM, 36, TIGHT, OK )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TISBOD error case: lookup of matrix for body ' &
      //           '-10 when both *CONSTANTS_JED_EPOCH and ' &
      //           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'B1950', -10, ET, TSIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TISBOD error case: lookup of matrix for body ' &
      //           '-10 when both *_CONSTANTS_REF_FRAME and ' &
      //           '*CONSTS_REF_FRAME keywords are present.' )

!
!     Delete competing epoch spec first.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'B1950', -10, ET, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
!     Perform a normal TISBOD computation using the _CONSTS_ forms of
!     the epoch and base frame keywords.
!
      CALL TCASE ( 'TISBOD: lookup B1950 to body-fixed transformation ' &
      //           'matrix for body -10 using both *CONSTS_JED_EPOCH ' &
      //           'and *CONSTS_REF_FRAME keywords.' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the new keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
!     Look up the transformation from J2000 to IAU_SUN for body 10
!     at the J2000 epoch. This should match the transformation from
!     the B1950 frame to IAU_SUN for body -10 at the B1950 epoch,
!     since in both cases the input time is the epoch of the constants,
!     and both bodies have identical rotational elements.
!
      ET  =  ( B1950() - J2000() ) * SPD()

      CALL TISBOD ( 'J2000', 10, 0.D0, XTSIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'B1950', -10, ET,  TSIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TSIPM',  TSIPM, '~', XTSIPM, 36, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
!
!     Repeat the above sequence of tests for BODMAT.
!
      CALL TCASE ( 'BODMAT lookup of matrix for body -10' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore the old keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( 10, 0.D0, XTIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )
!
!     The matrix we'll get for body -10 will be multiplied on the
!     right by the transformation from J2000 to B1950, so do the same
!     to the expected matrix.
!
      CALL PXFORM ( 'J2000', 'B1950', 0.D0, J2B )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL MXM  ( XTIPM, J2B, TMPMAT )
      CALL MEQU ( TMPMAT,     XTIPM  )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT ( -10, ET, TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, TIGHT, OK )
      CALL CHCKXC  ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'BODMAT error case: lookup of matrix for body ' &
      //           '-10 when both *CONSTANTS_JED_EPOCH and ' &
      //           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( -10, ET, TSIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'BODMAT error case: lookup of matrix for body ' &
      //           '-10 when both *_CONSTANTS_REF_FRAME and ' &
      //           '*CONSTS_REF_FRAME keywords are present.' )

!
!     Delete competing epoch spec first.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( -10, ET, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
!     Perform a normal BODMAT computation using the _CONSTS_ forms of
!     the epoch and base frame keywords.
!
      CALL TCASE ( 'BODMAT: lookup B1950 to body-fixed transformation ' &
      //           'matrix for body -10 using both *CONSTS_JED_EPOCH ' &
      //           'and *CONSTS_REF_FRAME keywords.' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the new keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
!     Look up the transformation from J2000 to IAU_SUN for body 10
!     at the J2000 epoch. This should match the transformation from
!     the B1950 frame to IAU_SUN for body -10 at the B1950 epoch,
!     since in both cases the input time is the epoch of the constants,
!     and both bodies have identical rotational elements.
!
!     The matrix we'll get for body -10 will be multiplied on the
!     right by the transformation from J2000 to B1950, so do the same
!     to the expected matrix.
!
      CALL PXFORM ( 'J2000', 'B1950', 0.D0, J2B )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( 10, 0.D0, XTIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL MXM  ( XTIPM, J2B, TMPMAT )
      CALL MEQU ( TMPMAT,     XTIPM  )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT ( -10, ET,  TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
!
!     Now check BODEUL's ability to deal with non-standard epochs and
!     frames.  Look up data for body -10 at the B1950 epoch in the
!     B1950 frame.  Convert the resulting Euler angles to a matrix.
!
!     Since the numbers in the PCK match the standard
!     numbers for the sun, applying the B1950-to-J2000 transformation
!     to the output via right multiplication
!     should yield the standard matrix for the sun.
!
      CALL TCASE ( 'BODEUL lookup of angles for body -10, epoch B9150' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore the old keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )


      CALL BODMAT (  10, 0.D0, XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODEUL ( -10, ET,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The Euler angles we used to produce the matrix TIPM will have
!     been adjusted to produce the transformation from J2000 to the
!     body-fixed frame for body -10. Remove this factor from the
!     matrix so we obtain the matrix defined by the angles derived
!     directly from the PCK constants.
!
      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

!
!     Compare to the J2000-to-bodyfixed matrix for body 10 at ET 0.
!
      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
!     Repeat test for epoch J2000.
!
      CALL TCASE ( 'BODEUL lookup of angles for body -10, epoch J2000' )

      ET  =  ( J2000() - B1950() ) * SPD()

      CALL BODMAT (  10,  ET,  XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

!
!     Check the LAMBDA value associated with body -10.  We
!     expect the value to be 99 degrees *RPD().  (The value
!     459 degrees is set by T_PCK09, which writes the PCK file
!     we're using.  We expect BODEUL to mod this value by 2*pi.)
!
      XLMBDA = 99.D0 * RPD()
      CALL CHCKSD ( 'LAMBDA',  LAMBDA, '~', XLMBDA, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'BODEUL error case: lookup of angles for body ' &
      //           '-10 when both *CONSTANTS_JED_EPOCH and ' &
      //           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'BODEUL error case: lookup of angles for body ' &
      //           '-10 when both *_CONSTANTS_REF_FRAME and ' &
      //           '*CONSTS_REF_FRAME keywords are present.' )

!
!     Delete competing epoch spec first.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

!
! --- Case: ------------------------------------------------------
!
!     Perform a normal BODEUL computation using the _CONSTS_ forms of
!     the epoch and base frame keywords.
!
      CALL TCASE ( 'BODEUL: lookup B1950 to body-fixed transformation ' &
      //           'matrix for body -10 using both *CONSTS_JED_EPOCH ' &
      //           'and *CONSTS_REF_FRAME keywords. Epoch is B1950.' )

!
!     Delete the competing keywords.
!

      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the new keywords.
!

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )


      CALL BODMAT (  10, 0.D0, XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODEUL ( -10, ET,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The Euler angles we used to produce the matrix TIPM will have
!     been adjusted to produce the transformation from J2000 to the
!     body-fixed frame for body -10. Remove this factor from the
!     matrix so we obtain the matrix defined by the angles derived
!     directly from the PCK constants.
!
      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

!
!     Compare to the J2000-to-bodyfixed matrix for body 10 at ET 0.
!
      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

!
! --- Case: ------------------------------------------------------
!
!     Perform a normal BODEUL computation using the _CONSTS_ forms of
!     the epoch and base frame keywords.
!
      CALL TCASE ( 'BODEUL: lookup B1950 to body-fixed transformation ' &
      //           'matrix for body -10 using both *CONSTS_JED_EPOCH ' &
      //           'and *CONSTS_REF_FRAME keywords. Epoch is J2000.' )

!
!     Delete the competing keywords.
!
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the new keywords.
!
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, [B1950()] )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, [2]  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      ET  =  ( J2000() - B1950() ) * SPD()

      CALL BODMAT (  10, ET, XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The Euler angles we used to produce the matrix TIPM will have
!     been adjusted to produce the transformation from J2000 to the
!     body-fixed frame for body -10. Remove this factor from the
!     matrix so we obtain the matrix defined by the angles derived
!     directly from the PCK constants.
!
      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

!
!     Compare to the J2000-to-bodyfixed matrix for body 10 at ET 0.
!
      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )


! ***************************************************************
!
!     Radius tests
!
! ***************************************************************

!
!     Check the set of bodies for which radius data are provided.
!     Find the set of kernel variables for triaxial radii;
!     check the corresponding set of ID codes against the set
!     from the test utility T_PCKRSTD.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check the set of bodies for which ' &
      //           'radius data are provided.'        )


      CALL SSIZEI ( MAXSET, XSET   )
      CALL SSIZEI ( MAXSET, RADSET )

      CALL T_PCKRSTD ( XSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GNPOOL ( 'BODY*_RADII', 1, MAXSET, N, KVNAMS(1), FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VALIDC ( MAXSET, N, KVNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'RADII names found', FOUND, .TRUE., OK )

      N = CARDC(KVNAMS)

      CALL CHCKSI ( 'Number of RADII names', N,          '=', &
                                             CARDI(XSET), 0,   OK )

!
!     Check each name in the expected set for presence in the
!     actual name set.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bodies in set expected to have ' &
      //           'radius data against those present ' &
      //           'in the kernel.'                         )

      DO I = 1, CARDI(XSET)

         XNAME = 'BODY#_RADII'

         CALL REPMI ( XNAME, '#', XSET(I), XNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the expected name is in the kernel
!           variable list.
!
         J = BSRCHC( XNAME, N, KVNAMS(1) )

         QNAME = '# is present in kernel?'
         CALL REPMC ( QNAME, '#', XNAME, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( QNAME, (J .GT. 0), .TRUE., OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check bodies having radius data in ' &
      //           'kernel against those in the expected ' &
      //           'set.'                                 )

!
!     Check each name in the actual set for presence in the
!     expected name set.
!
      DO I = 1, CARDC(KVNAMS)
!
!        Get the ID code of the current name.
!
         IDSTR = KVNAMS(I)(5:)

         J = INDEX( IDSTR, '_' )

         CALL PRSINT ( IDSTR(:J-1), CODE )

         QNAME = '# is present in expected set?'
         CALL REPMI ( QNAME, '#', CODE, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, ELEMI(CODE,XSET), .TRUE., OK )

      END DO

!
!     Since we've checked the set of bodies, let the actual
!     set be the expected set.
!
      CALL COPYI ( XSET, ANGSET )

!
! --- Case: ------------------------------------------------------
!
!
!     Check radii of each body in the expected set.
!
      DO I = 1, CARDI(XSET)

         TITLE = 'Check radii of body #'
         CALL REPMI ( TITLE, '#', XSET(I), TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
         CALL TCASE ( TITLE )

         CALL T_PCKRADD( XSET(I), XRAD )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL BODVCD ( XSET(I), 'RADII', 3, J, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = 'Number of radii of body #'
         CALL REPMI ( QNAME, '#', XSET(I), QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( QNAME, J, '=', 3, 0, OK )


         QNAME = 'Radii of body #'
         CALL REPMI ( QNAME, '#', XSET(I), QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( QNAME, RADII, '=', XRAD, 3, TIGHT, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL  ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_SUCCESS ( OK )
      RETURN
      END
