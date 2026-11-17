!> The segments of SPK, PCK and CK files: reading their records and evaluating them (all supported types), the generic segment routines, two-body propagation and the SGP4 model.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## ZZSGP4 (SGP4 wrapper)
!>
!> PEPPER: SPICELIB's umbrella routine ZZSGP4 is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzsgp4. Its documentation follows.
!>
!>### Abstract
!>
!>     Umbrella for the SGP4 initializer and evaluator routines.
!>
!>### Detailed Input
!>
!>     See Individual Entry points.
!>
!>### Detailed Output
!>
!>     See Individual Entry points.
!>
!>### Parameters
!>
!>     None.
!>
!>### Exceptions
!>
!>     1)  If ZZSGP4 is called, the error SPICE(BOGUSENTRY) is signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     This routine wraps XXSGP4E and XXSGP4I. As entry points to
!>     this routine, they share the local memory space.
!>
!>### Examples
!>
!>     The numerical results shown for these examples may differ across
!>     platforms. The results depend on the SPICE kernels used as
!>     input, the compiler and supporting libraries, and the machine
!>     specific arithmetic implementation.
!>
!>     Use a set of TLEs to calculate a collection of states for a
!>     time interval centered at the TLE set epoch.
!>
!>           PROGRAM ZZSGP4_T
!>
!>     C
!>     C     Read a data file containing sets of TLEs, then calculate
!>     C     states at -1440 to 1440 minutes from the epoch of each
!>     C     TLE set in steps of 10 minutes.
!>     C
!>     C     Example cases listed in sgp4-ver.tle.
!>     C
!>     C     1 00005U 58002B   00179.78495062  .00000023
!>     C       00000-0  28098-4 0  4753
!>     C     2 00005  34.2682 348.7242 1859667 331.7664
!>     C       19.3264 10.82419157413667
!>     C
!>     C     1 04632U 70093B   04031.91070959 -.00000084
!>     C       00000-0  10000-3 0  9955
!>     C     2 04632  11.4628 273.1101 1450506 207.6000
!>     C       143.9350  1.20231981 44145
!>     C
!>     C     1 06251U 62025E   06176.82412014  .00008885
!>     C       00000-0  12808-3 0  3985
!>     C     2 06251  58.0579  54.0425 0030035 139.1568
!>     C       221.1854 15.56387291  6774
!>     C
!>     C     1 08195U 75081A   06176.33215444  .00000099
!>     C       00000-0  11873-3 0   813
!>     C     2 08195  64.1586 279.0717 6877146 264.7651
!>     C       20.2257  2.00491383225656
!>     C
!>     C     1 09880U 77021A   06176.56157475  .00000421
!>     C       00000-0  10000-3 0  9814
!>     C     2 09880  64.5968 349.3786 7069051 270.0229
!>     C       16.3320  2.00813614112380
!>     C
!>     C     1 09998U 74033F   05148.79417928 -.00000112
!>     C       00000-0  00000+0 0  4480
!>     C     2 09998   9.4958 313.1750 0270971 327.5225
!>     C       30.8097  1.16186785 45878
!>     C
!>     C     1 11801U          80230.29629788  .01431103
!>     C       00000-0  14311-1      13
!>     C     2 11801  46.7916 230.4354 7318036  47.4722
!>     C       10.4117  2.28537848    13
!>     C
!>     C     1 14128U 83058A   06176.02844893 -.00000158
!>     C       00000-0  10000-3 0  9627
!>     C     2 14128  11.4384  35.2134 0011562  26.4582
!>     C       333.5652  0.98870114 46093
!>     C
!>     C     1 16925U 86065D   06151.67415771  .02550794
!>     C       -30915-6  18784-3 0  4486
!>     C     2 16925  62.0906 295.0239 5596327 245.1593
!>     C       47.9690  4.88511875148616
!>     C
!>     C     1 20413U 83020D   05363.79166667  .00000000
!>     C       00000-0  00000+0 0  7041
!>     C     2 20413  12.3514 187.4253 7864447 196.3027
!>     C       356.5478  0.24690082  7978
!>     C
!>     C     1 21897U 92011A   06176.02341244 -.00001273
!>     C       00000-0 -13525-3 0  3044
!>     C     2 21897  62.1749 198.0096 7421690 253.0462
!>     C       20.1561  2.01269994104880
!>     C
!>     C     1 22312U 93002D   06094.46235912  .99999999
!>     C       81888-5  49949-3 0  3953
!>     C     2 22312  62.1486  77.4698 0308723 267.9229
!>     C       88.7392 15.95744531 98783
!>     C
!>     C     1 22674U 93035D   06176.55909107  .00002121
!>     C       00000-0  29868-3 0  6569
!>     C     2 22674  63.5035 354.4452 7541712 253.3264
!>     C       18.7754  1.96679808 93877
!>     C
!>     C     1 23177U 94040C   06175.45752052  .00000386
!>     C       00000-0  76590-3 0    95
!>     C     2 23177   7.0496 179.8238 7258491 296.0482
!>     C       8.3061  2.25906668 97438
!>     C
!>     C     1 23333U 94071A   94305.49999999 -.00172956
!>     C       26967-3  10000-3 0    15
!>     C     2 23333  28.7490   2.3720 9728298  30.4360
!>     C       1.3500  0.07309491    70
!>     C
!>     C     1 23599U 95029B   06171.76535463  .00085586
!>     C       12891-6  12956-2 0  2905
!>     C     2 23599   6.9327   0.2849 5782022 274.4436
!>     C       25.2425  4.47796565123555
!>     C
!>     C     1 24208U 96044A   06177.04061740 -.00000094
!>     C       00000-0  10000-3 0  1600
!>     C     2 24208   3.8536  80.0121 0026640 311.0977
!>     C       48.3000  1.00778054 36119
!>     C
!>     C     1 25954U 99060A   04039.68057285 -.00000108
!>     C       00000-0  00000-0 0  6847
!>     C     2 25954   0.0004 243.8136 0001765  15.5294
!>     C       22.7134  1.00271289 15615
!>     C
!>     C     1 26900U 01039A   06106.74503247  .00000045
!>     C       00000-0  10000-3 0  8290
!>     C     2 26900   0.0164 266.5378 0003319  86.1794
!>     C       182.2590  1.00273847 16981
!>     C
!>     C     1 26975U 78066F   06174.85818871  .00000620
!>     C       00000-0  10000-3 0  6809
!>     C     2 26975  68.4714 236.1303 5602877 123.7484
!>     C       302.5767  2.05657553 67521
!>     C
!>     C     1 28057U 03049A   06177.78615833  .00000060
!>     C       00000-0  35940-4 0  1836
!>     C     2 28057  98.4283 247.6961 0000884  88.1964
!>     C       271.9322 14.35478080140550
!>     C
!>     C     1 28129U 03058A   06175.57071136 -.00000104
!>     C       00000-0  10000-3 0   459
!>     C     2 28129  54.7298 324.8098 0048506 266.2640
!>     C       93.1663  2.00562768 18443
!>     C
!>     C     1 28350U 04020A   06167.21788666  .16154492
!>     C       76267-5  18678-3 0  8894
!>     C     2 28350  64.9977 345.6130 0024870 260.7578
!>     C       99.9590 16.47856722116490
!>     C
!>     C     1 28623U 05006B   06177.81079184  .00637644
!>     C       69054-6  96390-3 0  6000
!>     C     2 28623  28.5200 114.9834 6249053 170.2550
!>     C       212.8965  3.79477162 12753
!>     C
!>     C     1 28626U 05008A   06176.46683397 -.00000205
!>     C       00000-0  10000-3 0  2190
!>     C     2 28626   0.0019 286.9433 0000335  13.7918
!>     C       55.6504  1.00270176  4891
!>     C
!>     C     1 28872U 05037B   05333.02012661  .25992681
!>     C       00000-0  24476-3 0  1534
!>     C     2 28872  96.4736 157.9986 0303955 244.0492
!>     C       110.6523 16.46015938 10708
!>     C
!>     C     1 29141U 85108AA  06170.26783845  .99999999
!>     C       00000-0  13519-0 0   718
!>     C     2 29141  82.4288 273.4882 0015848 277.2124
!>     C       83.9133 15.93343074  6828
!>     C
!>     C     1 29238U 06022G   06177.28732010  .00766286
!>     C       10823-4  13334-2 0   101
!>     C     2 29238  51.5595 213.7903 0202579  95.2503
!>     C       267.9010 15.73823839  1061
!>     C
!>     C     1 88888U          80275.98708465  .00073094
!>     C       13844-3  66816-4 0    87
!>     C     2 88888  72.8435 115.9689 0086731  52.6988
!>     C       110.5714 16.05824518  1058
!>     C
!>
!>           IMPLICIT NONE
!>
!>           INCLUDE 'zzsgp4.inc'
!>
!>           CHARACTER*(72)           LINES  ( 2 )
!>           CHARACTER*(72)           TLEDAT
!>
!>           INTEGER                  FRSTYR
!>           INTEGER                  I
!>           INTEGER                  OPMODE
!>
!>           DOUBLE PRECISION         DELT
!>           DOUBLE PRECISION         ELEMS  ( 10 )
!>           DOUBLE PRECISION         EPOCH
!>           DOUBLE PRECISION         GEOPHS ( 8 )
!>           DOUBLE PRECISION         STATE  ( 6 )
!>           DOUBLE PRECISION         TF
!>           DOUBLE PRECISION         TIME
!>           DOUBLE PRECISION         TS
!>
!>           LOGICAL                  EOF
!>
!>     C
!>     C     SPICELIB routines.
!>     C
!>           LOGICAL                  FAILED
!>
!>     C
!>     C     Load a leapseconds kernel for time conversion. Required
!>     C     by the SPK 10 evaluator.
!>     C
!>           CALL FURNSH ( '/kernels/gen/lsk/naif0011.tls' )
!>
!>     C
!>     C     Define the geophysical quantities using the values
!>     C     from geophysical.ker.
!>     C
!>           GEOPHS( K_J2 ) =    1.082616D-3
!>           GEOPHS( K_J3 ) =   -2.53881D-6
!>           GEOPHS( K_J4 ) =   -1.65597D-6
!>           GEOPHS( K_KE ) =    7.43669161D-2
!>           GEOPHS( K_QO ) =  120.0D0
!>           GEOPHS( K_SO ) =   78.0D0
!>           GEOPHS( K_ER ) = 6378.135D0
!>           GEOPHS( K_AE ) =    1.0D0
!>
!>           TLEDAT = 'sgp4-ver1.tle'
!>
!>     C
!>     C     Error subsystem to report to ensure execution continues
!>     C     if an error signals.
!>     C
!>           CALL ERRACT( 'SET', 'REPORT')
!>
!>     C
!>     C     Use Spacetrack #3 algorithm to calculate sidereal time.
!>     C
!>           OPMODE = AFSPC
!>
!>     C
!>     C     Identify the earliest year for the elements.
!>     C
!>           FRSTYR = 1958
!>
!>     C
!>     C     Start and final offsets from TLE epochs. [-1440, 1400]
!>     C     minutes.
!>     C
!>           TS     =  -1440.0D0
!>           TF     =   1440.0D0
!>
!>     C
!>     C     Step size for elements output 10 minutes.
!>     C
!>           DELT   = 10.D0
!>
!>     C
!>     C     Read the TLE data file.
!>     C
!>           CALL RDTEXT ( TLEDAT, LINES(1), EOF )
!>           CALL RDTEXT ( TLEDAT, LINES(2), EOF )
!>
!>     C
!>     C     Loop over data file until end-of-file.
!>     C
!>           DO WHILE ( .NOT. EOF )
!>
!>     C
!>     C        Parse the elements to something SPICE can use.
!>     C
!>              CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
!>
!>              WRITE(*, FMT='(A72)') LINES(1)
!>              WRITE(*, FMT='(A72)') LINES(2)
!>              WRITE(*,*) ' '
!>
!>     C
!>     C        Initialize SGP4 calculations based on values in
!>     C        GEOPHS, ELEMS, and AFSPC.
!>     C
!>              CALL XXSGP4I ( GEOPHS, ELEMS, OPMODE )
!>
!>     C
!>     C        Start time keyed in minutes from TLE epoch.
!>     C
!>              TIME   = TS
!>
!>              DO WHILE ( TIME .LE. DABS(TF) .AND. (.NOT. FAILED()) )
!>
!>     C
!>     C           Calculate the STATE at TIME.
!>     C
!>                 CALL XXSGP4E ( TIME, STATE )
!>
!>     C
!>     C           If the propagation succeeded, output the STATE.
!>     C
!>                 IF ( .NOT. FAILED() ) THEN
!>
!>                    WRITE(*, FMT ='(7F17.8)' ) TIME,
!>          .                                    (STATE(I),I=1,6)
!>
!>                 END IF
!>
!>     C
!>     C           Increment the evaluation time by one step.
!>     C
!>                 TIME = TIME + DELT
!>
!>              END DO
!>
!>              WRITE(*,*) ' '
!>
!>     C
!>     C        reset the error subsystem for the next loop.
!>     C
!>              CALL RESET()
!>
!>     C
!>     C        Read the next two lines (if any) from the TLE
!>     C        data file.
!>     C
!>              CALL RDTEXT ( TLEDAT, LINES(1), EOF )
!>              CALL RDTEXT ( TLEDAT, LINES(2), EOF )
!>
!>           END DO
!>
!>           END
!>
!>### Restrictions
!>
!>     None.
!>
!>### Literature References
!>
!>     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!>         Propagation of the NORAD Element Sets." Spacetrack Report #3.
!>         U.S. Air Force: Aerospace Defense Command.
!>
!>     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
!>         of Space Command Element Sets." Space Command,
!>         U. S. Air Force, CO.
!>
!>     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
!>         History of Analytical Orbit Modeling in the U. S. Space
!>         Surveillance System. Journal of Guidance, Control, and
!>         Dynamics. 27(2):174-185.
!>
!>     [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
!>         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
!>         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
!>         Specialist Conference, August 21-24, 2006. Keystone, CO.
!>
!>### Author and Institution
!>
!>     J. Diaz del Rio    (ODC Space)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR)
!>
!>        Correction of documentation error in listing of GEOPHS
!>        constants. The definition of index 5, "High altitude bound
!>        for atmospheric model in km.", and index 6, "Low altitude
!>        bound for atmospheric model in km." were switched.
!>
!>-    SPICELIB Version 1.0.0, 15-SEP-2014 (EDW)
!>
!>        Based on routine SGP4, 28-JUN-2005, Vallado 2006 [4].
!>
!>### Implementation notes
!>
!>     DS values
!>
!>     SPICELIB routines.
submodule (pepper_instance_module) pepper_records

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: nconst = 1  !! number of a generic segment's constants (SGFCON), at the start of the record
   integer(ip), parameter :: begel1 = nconst + 1  !! index in the record of the first packet, after the constants
   integer(ip), parameter :: dirsiz = 100  !! size of a segment's directory of epochs (every 100th epoch)
   integer(ip), parameter :: bufsiz = dirsiz + 1  !! size of a buffer of epochs (a directory and one more)
   integer(ip), parameter :: cntidx = 2  !! index in a record of the number of packets
   integer(ip), parameter :: ctrlsz = 4  !! number of control words at the end of a segment
   integer(ip), parameter :: maxrec = 129  !! maximum size of a record
   integer(ip), parameter :: nd = 2  !! number of double precision components of a DAF array summary (SPK, CK, PCK: 2)
   integer(ip), parameter :: ndc = 2  !! number of double precision components of a segment descriptor
   integer(ip), parameter :: ni = 6  !! number of integer components of a DAF array summary (SPK, CK: 6)
   integer(ip), parameter :: nic = 6  !! number of integer components of a segment descriptor
   integer(ip), parameter :: pktidx = 3  !! index in a record of the first packet
   integer(ip), parameter :: qavsiz = 7  !! number of components of a quaternion and angular velocity
   integer(ip), parameter :: qsiz = 4  !! number of components of a quaternion
   integer(ip), parameter :: sbtidx = 1  !! index in a record of the subtype
   integer(ip), parameter :: sizidx = 1  !! index in a record of its size
   integer(ip), parameter :: staidx = 4  !! index in a record of the first state
   integer(ip), parameter :: statsz = 6  !! number of components of a state (position and velocity)

contains

      !> Evaluate a pointing record returned by [[ckr01]] from a CK data type 1
      !> segment. Return the C-matrix and optionally the angular velocity
      !> vector associated with the time CLKOUT.
      !>
      !>## Detailed Input
      !>
      !>     NEEDAV   is .TRUE. when angular velocity data is requested.
      !>
      !>     RECORD   is a set of double precision numbers returned by CKR01
      !>              that contain sufficient information from a data type
      !>              1 pointing segment to evaluate the C-matrix and
      !>              possibly the angular velocity vector (if NEEDAV is
      !>              .TRUE.) for a particular instance.
      !>
      !>              The contents of RECORD are as follows:
      !>
      !>                 RECORD( 1 ) = CLKOUT
      !>
      !>                 RECORD( 2 ) = q0
      !>                 RECORD( 3 ) = q1
      !>                 RECORD( 4 ) = q2
      !>                 RECORD( 5 ) = q3
      !>
      !>                 RECORD( 6 ) = Av1  ]
      !>                 RECORD( 7 ) = Av2  |-- Optional
      !>                 RECORD( 8 ) = Av3  ]
      !>
      !>              The quantities q0 - q3 represent a quaternion.
      !>              The quantities Av1, Av2, and Av3 represent the angular
      !>              velocity vector.
      !>
      !>              CLKOUT is the encoded spacecraft clock time
      !>              associated with the quaternion and, optionally, the
      !>              angular velocity vector.
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components of
      !>              of a vector expressed in the reference frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at time CLKOUT.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              reference frame, then v has components x', y', z' in
      !>              the instrument fixed frame at time CLKOUT:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                            (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector. This is returned only
      !>              if it has been requested, as indicated by NEEDAV. In
      !>              other words, if NEEDAV is .TRUE., the angular velocity
      !>              portion of RECORD must be present.
      !>
      !>              The angular velocity vector is the vector whose
      !>              direction gives the right-handed axis about which
      !>              the reference frame tied to the instrument is
      !>              instantaneously rotating at time CLKOUT.
      !>
      !>              The angular velocity vector is returned in component
      !>              form
      !>
      !>                       AV = [ AV1  , AV2  , AV3  ]
      !>
      !>              which is in terms of the reference coordinate frame
      !>              specified in the segment descriptor.
      !>
      !>              The magnitude of AV is the magnitude of the instantane-
      !>              ous velocity of the rotation, in radians per second.
      !>
      !>     CLKOUT   is the encoded spacecraft clock time associated with the
      !>              returned C-matrix and, optionally, the returned angular
      !>              velocity vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  No checking is done to determine whether RECORD is a valid
      !>         record.
      !>
      !>     2)  If NEEDAV is .TRUE., then RECORD is assumed to contain angular
      !>         velocity data. No checking is performed to verify this
      !>         assumption.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For a detailed description of the structure of a type 1 pointing
      !>     segment, see the CK Required Reading file.
      !>
      !>     The only real work done by CKE01 is to convert the pointing
      !>     portion of the record from quaternion form to C-matrix form.
      !>
      !>     The angular velocity vector will only be returned if it has been
      !>     requested. In other words, if NEEDAV is .TRUE., the routine will
      !>     expect the angular velocity component of the record to be present.
      !>
      !>## Examples
      !>
      !>     A call to a CKEnn routine is almost always preceded by a call to
      !>     the corresponding CKRnn routine, which gets the logical record
      !>     that CKEnn evaluates.
      !>
      !>     The following code fragment searches through a file represented
      !>     by HANDLE for all segments applicable to the Voyager 2 wide angle
      !>     camera, for a particular spacecraft clock time, which have data
      !>     type 1. It then evaluates the pointing for that epoch and prints
      !>     the result.
      !>
      !>     C
      !>     C     - Get the spacecraft clock time. Must encode it for use
      !>     C       in the C-kernel.
      !>     C
      !>     C     - Set the time tolerance high to catch anything close to
      !>     C       the input time.
      !>     C
      !>     C     - We don't need angular velocity data.
      !>     C
      !>
      !>           SC     = -32
      !>           INST   = -32002
      !>           TOL    =  1000.D0
      !>           NEEDAV = .FALSE.
      !>           DTYPE  =  1
      !>     C
      !>     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'VGR_SCLK.TSC'        )
      !>           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
      !>     C
      !>     C     Convert the input request time to ticks.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>
      !>     C
      !>     C     Search from the beginning through all segments.
      !>     C
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( SFND   )
      !>
      !>           DO WHILE ( SFND )
      !>
      !>              CALL DAFGN ( IDENT                 )
      !>              CALL DAFGS ( DESCR                 )
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>              IF (        INST          .EQ. ICD( 1 )
      !>          .               DTYPE         .EQ. ICD( 3 )
      !>          .        .AND.  SCLKDP + TOL  .GE. DCD( 1 )
      !>          .        .AND.  SCLKDP - TOL  .LE. DCD( 2 )  ) THEN
      !>
      !>                 CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                   RECORD, FOUND )
      !>
      !>                 IF ( FOUND ) THEN
      !>
      !>                    CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
      !>
      !>                    WRITE (*,*) 'Segment descriptor and identifier:'
      !>                    WRITE (*,*) DCD, ICD
      !>                    WRITE (*,*) IDENT
      !>
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*) CMAT
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( SFND )
      !>
      !>           END DO
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
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced header references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.2.0, 14-NOV-1995 (WLT)
      !>
      !>        Changed "inertial frame" to simply reference frame to
      !>        reflect new capabilities of the SPICE system.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 30-AUG-1991 (MJS) (JML)
      !>
      !>        1) Previously, in the standard SPICE error handling, the
      !>           logical function RETURN was not written as a function;
      !>           it is now written as a function.
      !>
      !>        2) The example program was changed so that the tolerance
      !>           and data type are used in selecting which segments to read.
      !>
      !>        3) It was specified that the angular velocity vector
      !>           gives the right-handed axis about which the instrument
      !>           frame rotates.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        The example program was corrected so that the input
      !>        instrument code was tested against ICD(1) instead of
      !>        ICD(3).
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 14-NOV-1995 (WLT)
      !>
      !>        Changed "inertial frame" to simply reference frame to
      !>        reflect new capabilities of the SPICE system.
      !>
      !>        This change affects only documentation not code.
      !>
      !>-    SPICELIB Version 1.1.0, 30-AUG-1991 (MJS) (JML)
      !>
      !>        1) In the standard SPICE error handling, the line:
      !>
      !>              IF ( RETURN ) THEN
      !>
      !>           was changed to
      !>
      !>              IF ( RETURN() ) THEN
      !>
      !>        2) The example program was changed so that the tolerance
      !>           and data type are used in selecting  which segments to read.
      !>
      !>        3) It was specified that the angular velocity vector
      !>           gives the right-handed axis about which the instrument
      !>           frame rotates.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        1) The example program was corrected so that the input
      !>           instrument code was tested against ICD(1) instead of
      !>           ICD(3).
      !>        2) SCLK was removed from the Required Reading section.
      !>
      !>-    Beta Version 1.1.0, 29-AUG-1990 (MJS) (JEM)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>        1) The argument SCLK was removed from the calling sequence.
      !>        2) Header was updated.
      !>        3) The call to the routine QUAT2M_3 was replaced by a call to
      !>           the routine Q2M.
      !>
      !>-    Beta Version 1.0.0, 18-MAY-1990 (RET) (IMU)
      module subroutine cke01 ( ctx, needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      use pepper_math, only: q2m
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: needav  !! .TRUE. if angular velocity vector is required.
      real(wp), intent(in) :: record ( *    )  !! Data type 1 pointing record.
      real(wp), intent(out) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp), intent(out) :: av     ( 3    )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! Output spacecraft clock time.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Dissect the record.
!
      clkout = record( 1 )

      call q2m ( record( 2 ), cmat )

      if ( needav ) then

         av( 1 ) = record( 6 )
         av( 2 ) = record( 7 )
         av( 3 ) = record( 8 )

      end if

      return
      end subroutine cke01

      !> Evaluate a pointing record returned by [[ckr02]] from a CK data type 2
      !> segment. Return the C-matrix and angular velocity vector
      !> associated with the time CLKOUT.
      !>
      !>## Detailed Input
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>     RECORD   is a set of double precision numbers returned by CKR02
      !>              that contain sufficient information from a data type
      !>              2 pointing segment to evaluate the C-matrix and the
      !>              angular velocity vector for a particular instance.
      !>
      !>              The contents of RECORD are as follows:
      !>
      !>                 RECORD( 1  ) = start SCLKDP of interval
      !>
      !>                 RECORD( 2  ) = SCLK for which pointing was found
      !>
      !>                 RECORD( 3  ) = seconds / tick rate
      !>
      !>                 RECORD( 4  ) = q0
      !>                 RECORD( 5  ) = q1
      !>                 RECORD( 6  ) = q2
      !>                 RECORD( 7  ) = q3
      !>
      !>                 RECORD( 8  ) = av1
      !>                 RECORD( 9  ) = av2
      !>                 RECORD( 10 ) = av3
      !>
      !>              The quantities q0 - q3 are the components of the
      !>              quaternion that represents the C - matrix associated
      !>              with the start of the interval. The quantities av1,
      !>              av2, and av3 are the components of the angular velocity
      !>              vector.
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components
      !>              of a vector expressed in the inertial frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at the returned time.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              inertial frame, then v has components x', y', z' in the
      !>              instrument fixed frame where:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                            (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector. The angular velocity
      !>              contained in RECORD is returned only if NEEDAV is .TRUE.
      !>
      !>              The direction of the angular velocity vector gives
      !>              the right-handed axis about which the instrument fixed
      !>              reference frame is rotating. The magnitude of AV is
      !>              the magnitude of the instantaneous velocity of the
      !>              rotation, in radians per second.
      !>
      !>              The angular velocity vector is returned in component
      !>              form
      !>
      !>                       AV = [ AV1  , AV2  , AV3  ]
      !>
      !>              which is in terms of the inertial coordinate frame
      !>              specified in the segment descriptor.
      !>
      !>     CLKOUT   is the encoded SCLK associated with the returned
      !>              C-matrix and angular velocity vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  No checking is done to determine whether RECORD is valid.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For a detailed description of the structure of a type 2 pointing
      !>     segment, see the CK Required Reading.
      !>
      !>     Pointing data in a type 2 segment consists of intervals during
      !>     which the orientation of the spacecraft structure can be described
      !>     by an initial C-matrix and a constant angular velocity vector.
      !>     From the information contained in the pointing record returned by
      !>     CKR02, this subroutine calculates and returns the C-matrix
      !>     associated with the time returned by CKR02. It also returns the
      !>     angular velocity vector contained in the pointing record.
      !>
      !>## Examples
      !>
      !>     A call to a CKEnn routine is almost always preceded by a call to
      !>     the corresponding CKRnn routine, which gets the logical record
      !>     that CKEnn evaluates.
      !>
      !>     The following code fragment searches through a file (represented
      !>     by HANDLE) for all segments applicable to the Voyager 2 wide angle
      !>     camera, for a particular spacecraft clock time, that are of data
      !>     types 1 or 2. It then evaluates the pointing for that epoch and
      !>     prints the result.
      !>
      !>           SC     = -32
      !>           INST   = -32002
      !>     C
      !>     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'VGR_SCLK.TSC'        )
      !>           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
      !>
      !>     C
      !>     C     Get the spacecraft clock time. Must encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>
      !>     C
      !>     C     Search from the beginning through all segments.
      !>     C
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( SFND   )
      !>
      !>           DO WHILE ( SFND )
      !>
      !>              CALL DAFGN ( IDENT                 )
      !>              CALL DAFGS ( DESCR                 )
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>              IF ( INST          .EQ. ICD( 1 )  .AND.
      !>          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
      !>          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
      !>
      !>                 DTYPE = ICD ( 3 )
      !>
      !>                 IF ( DTYPE .EQ. 1 ) THEN
      !>
      !>                    CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                      RECORD, FOUND                       )
      !>
      !>                    IF ( FOUND ) THEN
      !>                       CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
      !>                    END IF
      !>
      !>                 ELSE  IF ( DTYPE .EQ. 2 ) THEN
      !>
      !>                    CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL,
      !>          .                      RECORD, FOUND )
      !>
      !>                    IF ( FOUND ) THEN
      !>                       CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
      !>                    END IF
      !>
      !>                 END IF
      !>
      !>                 IF ( FOUND ) THEN
      !>
      !>                    WRITE (*,*) 'Segment descriptor and identifier:'
      !>                    WRITE (*,*) DCD, ICD
      !>                    WRITE (*,*) IDENT
      !>
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*) CMAT
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( SFND )
      !>
      !>           END DO
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
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
      !>-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
      !>
      !>        Removed non-standard end-of-declarations marker
      !>        'C%&END_DECLARATIONS' from comments.
      !>
      !>-    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
      module subroutine cke02 ( ctx, needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      use pepper_math, only: axisar, mxmt, q2m, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: needav  !! .TRUE. if angular velocity is requested.
      real(wp), intent(in) :: record ( *    )  !! Data type 2 pointing record.
      real(wp), intent(out) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp), intent(out) :: av     ( 3    )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! SCLK associated with C-matrix.

      real(wp) :: quat   ( 4    )
      real(wp) :: cbase  ( 3, 3 )
      real(wp) :: rot    ( 3, 3 )
      real(wp) :: avtemp (    3 )
      real(wp) :: angle
      real(wp) :: time

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Copy the returned encoded SCLK time into CLKOUT.
!
      clkout = record(2)

!     The quaternion stored in RECORD represents the C - matrix
!     corresponding to the start time of the interval.  The angular
!     velocity vector is constant throughout the interval and gives
!     the axis and rate by which the spacecraft is rotating.
!
!     Copy the quaternion and the angular velocity from RECORD.
!
!        RECORD ( 4 ) = q0
!        RECORD ( 5 ) = q1
!        RECORD ( 6 ) = q2
!        RECORD ( 7 ) = q3
!
!        RECORD ( 8  ) = av1
!        RECORD ( 9  ) = av2
!        RECORD ( 10 ) = av3
!
      quat = record(4:7)
      avtemp = record(8:10)

!
!     Calculate the angle of the rotation.
!
!        RECORD ( 1 ) = The start time of the interval.
!        RECORD ( 2 ) = The time that pointing was returned for.
!        RECORD ( 3 ) = The number of seconds per SCLK tick.
!
      time  =  ( record(2) - record(1) ) * record(3)

      angle =    time * vnorm ( avtemp )

!
!     Construct a matrix which rotates vectors by ANGLE radians about
!     AVTEMP.
!
      call axisar ( avtemp, angle, rot )

!
!     Convert the quaternion to a C - matrix.
!
      call q2m ( quat, cbase )
!
!     Rotate each of the axis vectors of the spacecraft instrument frame
!     by ANGLE radians about AVTEMP. (AVTEMP is given in the same
!     inertial frame as the C - matrix.)  The resulting matrix is the
!     transpose of the requested C - matrix.
!
!        [       ]       [       ] T         [        ] T
!        [  ROT  ]   *   [ CBASE ]     =     [  CMAT  ]
!        [       ]       [       ]           [        ]
!
!     OR
!
!        [       ]       [       ] T         [        ]
!        [ CBASE ]   *   [  ROT  ]     =     [  CMAT  ]
!        [       ]       [       ]           [        ]
!

      call mxmt ( cbase, rot, cmat )

!
!     Return the angular velocity only if it is requested.
!
      if ( needav ) then
         av = avtemp
      end if

      return
      end subroutine cke02

      !> Evaluate a pointing record returned by [[ckr03]] from a CK type 3
      !> segment. Return the C-matrix and angular velocity vector
      !> associated with the time CLKOUT.
      !>
      !>## Detailed Input
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>     RECORD   is a set of double precision numbers returned by CKR03
      !>              that contain sufficient information from a type 3 CK
      !>              segment to evaluate the C-matrix and the angular
      !>              velocity vector at a particular time. Depending on
      !>              the contents of RECORD, this routine will either
      !>              interpolate between two pointing instances that
      !>              bracket a request time, or it will simply return the
      !>              pointing given by a single pointing instance.
      !>
      !>              When pointing at the request time can be determined
      !>              by linearly interpolating between the two pointing
      !>              instances that bracket that time, the bracketing
      !>              pointing instances are returned in RECORD as follows:
      !>
      !>                 RECORD( 1  ) = Left bracketing SCLK time.
      !>
      !>                 RECORD( 2  ) = lq0  \
      !>                 RECORD( 3  ) = lq1   \    Left bracketing
      !>                 RECORD( 4  ) = lq2   /      quaternion.
      !>                 RECORD( 5  ) = lq3  /
      !>
      !>                 RECORD( 6  ) = lav1 \     Left bracketing
      !>                 RECORD( 7  ) = lav2  |    angular velocity
      !>                 RECORD( 8  ) = lav3 /       ( optional )
      !>
      !>                 RECORD( 9  ) = Right bracketing SCLK time.
      !>
      !>                 RECORD( 10 ) = rq0  \
      !>                 RECORD( 11 ) = rq1   \    Right bracketing
      !>                 RECORD( 12 ) = rq2   /       quaternion.
      !>                 RECORD( 13 ) = rq3  /
      !>
      !>                 RECORD( 14 ) = rav1 \     Right bracketing
      !>                 RECORD( 15 ) = rav2  |    angular velocity
      !>                 RECORD( 16 ) = rav3 /       ( optional )
      !>
      !>                 RECORD( 17 ) = pointing request time
      !>
      !>              The quantities lq0 - lq3 and rq0 - rq3 are the
      !>              components of the quaternions that represent the
      !>              C-matrices associated with the times that bracket
      !>              the requested time.
      !>
      !>              The quantities lav1, lav2, lav3 and rav1, rav2, rav3
      !>              are the components of the angular velocity vectors at
      !>              the respective bracketing times. The components of the
      !>              angular velocity vectors are specified relative to the
      !>              inertial reference frame of the segment.
      !>
      !>              When the routine is to simply return the pointing
      !>              given by a particular pointing instance, then the
      !>              values of that pointing instance are returned in both
      !>              parts of RECORD ( i.e. RECORD(1-9) and RECORD(10-16) ).
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components
      !>              of a vector expressed in the inertial frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at the returned time.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              inertial frame, then v has components x', y', z' in the
      !>              instrument fixed frame where:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                            (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector of the instrument fixed
      !>              frame defined by CMAT. The angular velocity is
      !>              returned only if NEEDAV is .TRUE.
      !>
      !>              The direction of the angular velocity vector gives
      !>              the right-handed axis about which the instrument fixed
      !>              reference frame is rotating. The magnitude of AV is
      !>              the magnitude of the instantaneous velocity of the
      !>              rotation, in radians per second.
      !>
      !>              The angular velocity vector is returned in component
      !>              form
      !>
      !>                       AV = [ AV1  , AV2  , AV3  ]
      !>
      !>              which is in terms of the inertial coordinate frame
      !>              specified in the segment descriptor.
      !>
      !>     CLKOUT   is the encoded SCLK associated with the returned
      !>              C-matrix and angular velocity vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If inputs are invalid or otherwise in appropriate, such that
      !>         the computed matrix is not a rotation matrix, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the array RECORD contains pointing instances that bracket the
      !>     request time then CKE03 will linearly interpolate between those
      !>     two values to obtain pointing at the request time. If the
      !>     pointing instances in RECORD are for the same time, then this
      !>     routine will simply unpack the record and convert the quaternion
      !>     to a C-matrix.
      !>
      !>     The linear interpolation performed by this routine is defined
      !>     as follows:
      !>
      !>     1)  Let t be the time for which pointing is requested and
      !>         let CMAT1 and CMAT2 be C-matrices associated with times
      !>         t1 and t2 where:
      !>
      !>                t1 < t2,  and  t1 <= t,  and  t <= t2.
      !>
      !>     2)  Assume that the spacecraft frame rotates about a fixed
      !>         axis at a constant angular rate from time t1 to time t2.
      !>         The angle and rotation axis can be obtained from the
      !>         rotation matrix ROT12 where:
      !>
      !>                            T                       T
      !>                       CMAT2   =  ROT12    *   CMAT1
      !>
      !>            or
      !>                                       T
      !>                       ROT12   =  CMAT2    *   CMAT1
      !>
      !>                       ROT12   ==> ( ANGLE, AXIS )
      !>
      !>     3)  To obtain pointing at time t, rotate the spacecraft frame
      !>         about the vector AXIS from its orientation at time t1 by the
      !>         angle THETA where:
      !>
      !>                                            ( t  - t1 )
      !>                       THETA  =  ANGLE  *   -----------
      !>                                            ( t2 - t1 )
      !>
      !>     4)  Thus if ROT1t is the matrix that rotates vectors by the
      !>         angle THETA about the vector AXIS, then the output C-matrix
      !>         is given by:
      !>
      !>                           T                     T
      !>                       CMAT  =  ROT1t   *   CMAT1
      !>
      !>                                                 T
      !>                       CMAT  =  CMAT1   *   ROT1t
      !>
      !>     5)  The angular velocity is treated independently of the
      !>         C-matrix. If it is requested, then the AV at time t is
      !>         the weighted average of the angular velocity vectors at
      !>         the times t1 and t2:
      !>
      !>                          ( t  - t1 )
      !>                    W  =  -----------
      !>                          ( t2 - t1 )
      !>
      !>                    AV  = ( 1 - W ) * AV1   +   W * AV2
      !>
      !>## Examples
      !>
      !>     The CKRnn routines are usually used in tandem with the CKEnn
      !>     routines, which evaluate the record returned by CKRnn to give
      !>     the pointing information and output time.
      !>
      !>     The following code fragment searches through all of the segments
      !>     in a file applicable to the Mars Observer spacecraft bus that
      !>     are of data type 3, for a particular spacecraft clock time.
      !>     It then evaluates the pointing for that epoch and prints the
      !>     result.
      !>
      !>           CHARACTER*(20)        SCLKCH
      !>           CHARACTER*(20)        SCTIME
      !>           CHARACTER*(40)        IDENT
      !>
      !>           INTEGER               I
      !>           INTEGER               SC
      !>           INTEGER               INST
      !>           INTEGER               HANDLE
      !>           INTEGER               DTYPE
      !>           INTEGER               ICD      (    6 )
      !>
      !>           DOUBLE PRECISION      SCLKDP
      !>           DOUBLE PRECISION      TOL
      !>           DOUBLE PRECISION      CLKOUT
      !>           DOUBLE PRECISION      DESCR    (    5 )
      !>           DOUBLE PRECISION      DCD      (    2 )
      !>           DOUBLE PRECISION      RECORD   (   17 )
      !>           DOUBLE PRECISION      CMAT     ( 3, 3 )
      !>           DOUBLE PRECISION      AV       (    3 )
      !>
      !>           LOGICAL               NEEDAV
      !>           LOGICAL               FND
      !>           LOGICAL               SFND
      !>
      !>           SC     = -94
      !>           INST   = -94000
      !>           DTYPE  =  3
      !>           NEEDAV = .FALSE.
      !>
      !>     C
      !>     C     Load the MO SCLK kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'MO_SCLK.TSC'       )
      !>           CALL DAFOPR ( 'MO_CK.BC',  HANDLE )
      !>     C
      !>     C     Get the spacecraft clock time. Then encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>     C
      !>     C     Use a tolerance of 2 seconds ( half of the nominal
      !>     C     separation between MO pointing instances ).
      !>     C
      !>           CALL SCTIKS ( SC, '0000000002:000', TOL )
      !>
      !>     C
      !>     C     Search from the beginning of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( SFND   )
      !>
      !>           FND    = .FALSE.
      !>
      !>           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
      !>
      !>     C
      !>     C        Get the segment identifier and descriptor.
      !>     C
      !>
      !>              CALL DAFGN ( IDENT                 )
      !>              CALL DAFGS ( DESCR                 )
      !>     C
      !>     C        Unpack the segment descriptor into its integer and
      !>     C        double precision components.
      !>     C
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>     C
      !>     C        Determine if this segment should be processed.
      !>     C
      !>              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
      !>          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
      !>          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
      !>          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
      !>
      !>                 CALL CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                   RECORD, FND )
      !>
      !>                 IF ( FND ) THEN
      !>
      !>                    CALL CKE03 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
      !>
      !>                    CALL SCDECD ( SC, CLKOUT, SCTIME )
      !>
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Segment identifier: ', IDENT
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Pointing returned for time: ',
      !>          .                      SCTIME
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
      !>                    WRITE (*,*)
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  No explicit checking is done on the input RECORD.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 2.0.0, 13-JUN-2002 (FST)
      !>
      !>        This routine now participates in error handling properly.
      !>
      !>-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 13-JUN-2002 (FST)
      !>
      !>        Calls to CHKIN and CHKOUT in the standard SPICE error
      !>        handling style were added. Versions prior to 2.0.0
      !>        were error free, however changes to RAXISA from error
      !>        free to error signaling forced this update.
      !>
      !>        Additionally, FAILED is now checked after the call to
      !>        RAXISA. This prevents garbage from being placed into
      !>        the output arguments.
      module subroutine cke03 ( ctx, needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      use pepper_math, only: axisar, mtxm, mxmt, q2m
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: needav  !! .TRUE. if angular velocity is requested.
      real(wp), intent(in) :: record ( *     )  !! Data type 3 pointing record.
      real(wp), intent(out) :: cmat   ( 3, 3  )  !! C-matrix.
      real(wp), intent(out) :: av     ( 3     )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! SCLK associated with C-matrix.

      real(wp) :: t
      real(wp) :: t1
      real(wp) :: t2
      real(wp) :: q1    (    4 )
      real(wp) :: q2    (    4 )
      real(wp) :: cmat1 ( 3, 3 )
      real(wp) :: cmat2 ( 3, 3 )
      real(wp) :: av1   (    3 )
      real(wp) :: av2   (    3 )
      real(wp) :: axis  (    3 )
      real(wp) :: rot   ( 3, 3 )
      real(wp) :: delta ( 3, 3 )
      real(wp) :: frac
      real(wp) :: angle

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the record, for easier reading.
!
      t    = record(17)
      t1   = record(1 )
      t2   = record(9 )

      q1 = record(2:5)
      av1 = record(6:8)
      q2 = record(10:13)
      av2 = record(14:16)

!
!     If T1 and T2 are the same then no interpolation or extrapolation
!     is performed.  Simply convert the quaternion to a C-matrix and
!     return.
!
      if ( t1 == t2 ) then

         call q2m ( q1, cmat )

         clkout = t1

         if ( needav ) then
            av = av1
         end if

         return

      end if

!
!     Interpolate between the two pointing instances to obtain pointing
!     at the request time.
!

!
!     Calculate what fraction of the interval the request time
!     represents.
!
      frac = ( t - t1 ) / ( t2 - t1 )

!
!     Convert the left and right quaternions to C-matrices.
!
      call q2m ( q1, cmat1 )
      call q2m ( q2, cmat2 )

!
!     Find the matrix that rotates the spacecraft instrument frame from
!     the orientation specified by CMAT1 to that specified by CMAT2.
!     Then find the axis and angle of that rotation matrix.
!
!             T                      T
!        CMAT2   =    ROT    *  CMAT1
!
!                          T
!        ROT     =    CMAT2  *  CMAT1
!

      call mtxm   ( cmat2, cmat1, rot   )

      call raxisa ( ctx, rot,   axis,  angle )

      if ( ctx%failed() ) return

!
!     Calculate the matrix that rotates vectors about the vector AXIS
!     by the angle ANGLE * FRAC.
!
      call axisar ( axis,  angle * frac, delta )

!
!     The interpolated pointing at the request time is given by CMAT
!     where:
!
!              T                    T
!          CMAT   =  DELTA  *  CMAT1
!
!     and
!                                   T
!          CMAT   =  CMAT1  *  DELTA
!

      call mxmt   ( cmat1, delta, cmat )

!
!     Set CLKOUT equal to the time that pointing is being returned.
!
      clkout = t

!
!     If angular velocity is requested then take a weighted average
!     of the angular velocities at the left and right endpoints.
!

      if ( needav ) then

         av = (1.0_wp - frac)*av1 + frac*av2

      end if

      return
      end subroutine cke03

      !> Evaluate a pointing record returned by [[ckr04]] from a CK type 4
      !> segment. Return the C-matrix and angular velocity vector
      !> associated with the time CLKOUT.
      !>
      !>## Detailed Input
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>     RECORD   is a set of double precision numbers returned by
      !>              CKR04. RECORD must have the following structure:
      !>
      !>              ---------------------------------------------------
      !>              |    Encoded onboard time which is the closest    |
      !>              |  to SCLKDP and belongs to one of approximation  |
      !>              |                   intervals                     |
      !>              ---------------------------------------------------
      !>              |       encoded SCLK time of the midpoint of      |
      !>              |             interpolation interval              |
      !>              ---------------------------------------------------
      !>              |          radii of interpolation interval        |
      !>              |    expressed as double precision SCLK ticks     |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q0           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q1           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q2           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q3           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for AV1          |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for AV2          |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for AV3          |
      !>              ---------------------------------------------------
      !>              |               q0 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |               q1 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |               q2 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |               q3 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |         AV1 Cheby coefficients (optional)       |
      !>              ---------------------------------------------------
      !>              |         AV2 Cheby coefficients (optional)       |
      !>              ---------------------------------------------------
      !>              |         AV3 Cheby coefficients (optional)       |
      !>              ---------------------------------------------------
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components
      !>              of a vector expressed in the inertial frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at the returned time.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              inertial frame, then v has components x', y', z' in
      !>              the instrument fixed frame where:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                            (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector of the instrument fixed
      !>              frame defined by CMAT. The angular velocity is
      !>              returned only if NEEDAV is .TRUE.
      !>
      !>              The direction of the angular velocity vector gives
      !>              the right-handed axis about which the instrument fixed
      !>              reference frame is rotating. The magnitude of AV is
      !>              the magnitude of the instantaneous velocity of the
      !>              rotation, in radians per second.
      !>
      !>              The angular velocity vector is returned in component
      !>              form
      !>
      !>                       AV = [ AV1  , AV2  , AV3  ]
      !>
      !>              which is in terms of the inertial coordinate frame
      !>              specified in the segment descriptor.
      !>
      !>     CLKOUT   is the encoded SCLK associated with the returned
      !>              C-matrix and angular velocity vector.
      !>
      !>## Parameters
      !>
      !>     See 'ckparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  No checking is done to determine whether RECORD is valid.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     For a detailed description of the structure of a type 4 pointing
      !>     segment, see the CK Required Reading file.
      !>
      !>     The work done by CKE04 is to calculate quaternion and angular
      !>     velocity components using Chebyshev polynomial approximation
      !>     parameters. The second step of evaluation is to convert the
      !>     pointing portion of the record from quaternion form to C-matrix
      !>     form.
      !>
      !>     The angular velocity vector will only be returned if it has been
      !>     requested. In other words, if NEEDAV is .TRUE., the routine will
      !>     expect the angular velocity component of the record to be
      !>     present.
      !>
      !>## Examples
      !>
      !>     The CKRnn routines are usually used in tandem with the CKEnn
      !>     routines, which evaluate the record returned by CKRnn to give
      !>     the pointing information and output time.
      !>
      !>     The following code fragment searches through all of the segments
      !>     in a file applicable to the Mars Global Surveyor spacecraft bus
      !>     that are of data type 4, for a particular spacecraft clock time.
      !>     It then evaluates the pointing for that epoch and prints the
      !>     result.
      !>
      !>     C
      !>     C     CK parameters include file.
      !>     C
      !>           INCLUDE               'ckparam.inc'
      !>     C
      !>     C     Local variables
      !>     C
      !>           CHARACTER*(20)        SCLKCH
      !>           CHARACTER*(20)        SCTIME
      !>           CHARACTER*(40)        IDENT
      !>
      !>           DOUBLE PRECISION      AV     ( 3 )
      !>           DOUBLE PRECISION      CLKOUT
      !>           DOUBLE PRECISION      CMAT   ( 3, 3 )
      !>           DOUBLE PRECISION      DCD    ( 2 )
      !>           DOUBLE PRECISION      DESCR  ( 5 )
      !>           DOUBLE PRECISION      RECORD ( CK4RSZ )
      !>           DOUBLE PRECISION      SCLKDP
      !>           DOUBLE PRECISION      TOL
      !>
      !>           INTEGER               HANDLE
      !>           INTEGER               I
      !>           INTEGER               ICD    ( 6 )
      !>           INTEGER               INST
      !>           INTEGER               SC
      !>
      !>           LOGICAL               FND
      !>           LOGICAL               NEEDAV
      !>           LOGICAL               SFND
      !>     C
      !>     C     Initial values.
      !>     C
      !>           SC     = -94
      !>           INST   = -94000
      !>           NEEDAV = .FALSE.
      !>     C
      !>     C     Load the MGS SCLK kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH( 'MGS_SCLK.TSC' )
      !>           CALL DAFOPR( 'MGS_CK4.BC', HANDLE )
      !>     C
      !>     C     Get the spacecraft clock time. Then encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           CALL PROMPT( 'Enter SCLK string: ', SCLKCH )
      !>           CALL SCENCD( SC, SCLKCH, SCLKDP )
      !>     C
      !>     C     Use a tolerance of 2 seconds (half of the nominal
      !>     C     separation between MGS pointing instances ).
      !>     C
      !>           CALL SCTIKS ( SC, '0000000002:000', TOL )
      !>     C
      !>     C     Search from the beginning of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBFS( HANDLE )
      !>           CALL DAFFNA( SFND   )
      !>
      !>           FND = .FALSE.
      !>
      !>           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
      !>     C
      !>     C        Get the segment identifier and descriptor.
      !>     C
      !>              CALL DAFGN( IDENT )
      !>              CALL DAFGS( DESCR )
      !>     C
      !>     C        Unpack the segment descriptor into its integer and
      !>     C        double precision components.
      !>     C
      !>              CALL DAFUS( DESCR, 2, 6, DCD, ICD )
      !>     C
      !>     C        Determine if this segment should be processed.
      !>     C
      !>              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
      !>          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
      !>          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
      !>          .        ( CK4DTP        .EQ. ICD( 3 ) )      ) THEN
      !>     C
      !>     C           Find CK 4 record covering requested time.
      !>     C
      !>                 CALL CKR04( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                  RECORD, FND )
      !>
      !>                 IF ( FND ) THEN
      !>     C
      !>     C              Compute pointing using found CK 4 record.
      !>     C
      !>                    CALL CKE04( NEEDAV, RECORD, CMAT, AV, CLKOUT)
      !>
      !>                    CALL SCDECD( SC, CLKOUT, SCTIME )
      !>
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Segment identifier: ', IDENT
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Pointing returned for time: ',
      !>          .                      SCTIME
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
      !>                    WRITE (*,*)
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  No checking is done on the input RECORD.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     E.D. Wright        (JPL)
      !>     Y.K. Zaiko         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.3, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
      !>
      !>        Minor header edits.
      !>
      !>-    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
      module subroutine cke04 ( ctx, needav, record, cmat, av, clkout )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: chbval, q2m, vhatg
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: needav  !! .TRUE. if angular velocity is requested.
      real(wp), intent(in) :: record ( * )  !! Data type 4 pointing record.
      real(wp), intent(out) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp), intent(out) :: av     ( 3 )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! SCLK associated with C-matrix.

      real(wp) :: q      ( 4 )
      real(wp) :: qout   ( 4 )

      integer(ip) :: basadd
      integer(ip) :: i
      integer(ip) :: ideg   ( qavsiz )

!
!     Initial values.
!
      av( 1 ) = 0.0_wp
      av( 2 ) = 0.0_wp
      av( 3 ) = 0.0_wp

!
!     Read numbers of polynomial coefficients from input record to
!     local integer array.
!
      do i = 1, qavsiz
         ideg( i ) = int( record( 3 + i ) )
      end do

!
!     Evaluate polynomial function for quaternion components at time
!     RECORD( 1 ).
!
      basadd = ck4sft + 1

      do i = 1, qsiz

         call chbval( record( basadd ), ideg( i ) - 1, record( 2 ), &
                                               record( 1 ), q( i ) )
         basadd = basadd + ideg ( i )

      end do

!
!     Normalize quaternion.
!
      call vhatg ( q, qsiz, qout )

!
!     Convert the quaternion to a C-matrix.
!
      call q2m ( qout, cmat )

      clkout = record( 1 )

!
!     Check if angular velocities have to be evaluated, then
!     evaluate them.
!
      if ( needav ) then

         do i = qsiz + 1, qavsiz

            call chbval ( record( basadd ), ideg( i ) - 1, record( 2 ), &
                                          record( 1 ), av( i - qsiz ) )
            basadd = basadd + ideg( i )

         end do

      end if

!
!     All done.
!
      return

      end subroutine cke04

      !> Evaluate a single data record from a type 5 CK segment.
      !>
      !>## Detailed Input
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>     RECORD   is a record from a type 5 CK segment which, when
      !>              evaluated at the epoch contained in its first
      !>              element, will give the attitude and angular velocity
      !>              of a spacecraft structure or instrument relative to a
      !>              base reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | evaluation epoch     |
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | nominal SCLK rate    |
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                          .
      !>                          .
      !>                          .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>                See the CK Required Reading or the include file
      !>                ck05.inc for details on CK type 5 packet contents.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   has been modified due to its use as a workspace array.
      !>              The contents are undefined.
      !>
      !>     CMAT     is a rotation matrix that transforms the components
      !>              of a vector expressed in the base frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at the returned time.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              base frame, then v has components x', y', z' in the
      !>              instrument fixed frame where:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                            (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector of the instrument fixed
      !>              frame defined by CMAT. The angular velocity is
      !>              returned only if NEEDAV is .TRUE.
      !>
      !>              The direction of the angular velocity vector gives
      !>              the right-handed axis about which the instrument fixed
      !>              reference frame is rotating. The magnitude of AV is
      !>              the magnitude of the instantaneous velocity of the
      !>              rotation, in radians per second.
      !>
      !>              The angular velocity vector is returned in component
      !>              form
      !>
      !>                       AV = [ AV1  , AV2  , AV3  ]
      !>
      !>              which is in terms of the base coordinate frame
      !>              specified in the segment descriptor.
      !>
      !>     CLKOUT   is the encoded SCLK associated with the returned
      !>              C-matrix and angular velocity vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input record contains an unrecognized subtype code,
      !>         the error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     2)  If the record subtype is one for which quaternion derivatives
      !>         are stored (subtypes 0 and 2), and if the Ith quaternion in
      !>         the input record is farther than its negative from the (I-1)st
      !>         quaternion in the record, the error SPICE(BADQUATSIGN) is
      !>         signaled.
      !>
      !>         For subtypes 1 and 3, this condition is not considered an
      !>         error: the closer to the preceding quaternion of the two
      !>         quaternion representations is used for interpolation.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of CK type 5 (MEX/Rosetta Attitude
      !>     file interpolation) CK segments is described in the CK Required
      !>     Reading.
      !>
      !>## Examples
      !>
      !>     The CKEnn routines are almost always used in conjunction with
      !>     the corresponding CKRnn routines, which read the records from
      !>     CK files.
      !>
      !>     The following code fragment searches through all of the segments
      !>     in a file applicable to the Mars Express spacecraft bus that
      !>     are of data type 5, for a particular spacecraft clock time.
      !>     It then evaluates the pointing for that epoch and prints the
      !>     result.
      !>
      !>           CHARACTER*(20)        SCLKCH
      !>           CHARACTER*(20)        SCTIME
      !>           CHARACTER*(40)        IDENT
      !>
      !>           INTEGER               I
      !>           INTEGER               SC
      !>           INTEGER               INST
      !>           INTEGER               HANDLE
      !>           INTEGER               DTYPE
      !>           INTEGER               ICD      (    6 )
      !>
      !>           DOUBLE PRECISION      SCLKDP
      !>           DOUBLE PRECISION      TOL
      !>           DOUBLE PRECISION      CLKOUT
      !>           DOUBLE PRECISION      DESCR    (    5 )
      !>           DOUBLE PRECISION      DCD      (    2 )
      !>           DOUBLE PRECISION      RECORD   (   17 )
      !>           DOUBLE PRECISION      CMAT     ( 3, 3 )
      !>           DOUBLE PRECISION      AV       (    3 )
      !>
      !>           LOGICAL               NEEDAV
      !>           LOGICAL               FND
      !>           LOGICAL               SFND
      !>
      !>           SC     = -41
      !>           INST   = -41000
      !>           DTYPE  =  5
      !>           NEEDAV = .FALSE.
      !>
      !>     C
      !>     C     Load the MEX SCLK kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'MEX_SCLK.TSC'       )
      !>           CALL DAFOPR ( 'MEX_CK.BC',  HANDLE )
      !>     C
      !>     C     Get the spacecraft clock time. Then encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>     C
      !>     C     Use a tolerance of 2 seconds ( half of the nominal
      !>     C     separation between MEX pointing instances ).
      !>     C
      !>           CALL SCTIKS ( SC, '0000000002:000', TOL )
      !>
      !>     C
      !>     C     Search from the beginning of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( SFND   )
      !>
      !>           FND    = .FALSE.
      !>
      !>           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
      !>
      !>     C
      !>     C        Get the segment identifier and descriptor.
      !>     C
      !>              CALL DAFGN ( IDENT )
      !>              CALL DAFGS ( DESCR )
      !>     C
      !>     C        Unpack the segment descriptor into its integer and
      !>     C        double precision components.
      !>     C
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>     C
      !>     C        Determine if this segment should be processed.
      !>     C
      !>              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
      !>          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
      !>          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
      !>          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
      !>
      !>                 CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                   RECORD, FND )
      !>
      !>                 IF ( FND ) THEN
      !>
      !>                    CALL CKE05 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
      !>
      !>                    CALL SCDECD ( SC, CLKOUT, SCTIME )
      !>
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Segment identifier: ', IDENT
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Pointing returned for time: ',
      !>          .                      SCTIME
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
      !>                    WRITE (*,*)
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that the input record is valid. Any
      !>         checking of the input data is assumed to have been performed
      !>         when the source CK file was created.
      !>
      !>     2)  This routine assumes that the input data are suitable for the
      !>         interpolation method indicated by the subtype code in the
      !>         input record. Since the mapping of rotations to quaternions
      !>         is multiple-valued, this routine assumes that whichever sign
      !>         minimizes the Euclidean distance between one quaternion and
      !>         the next is the correct sign.
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
      !>-    SPICELIB Version 3.1.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.1.0, 11-AUG-2015 (NJB)
      !>
      !>        Bug fix: PRVPTR is now updated at the end of the quaternion
      !>        sequence check for Hermite subtypes.
      !>
      !>-    SPICELIB Version 3.0.0, 06-FEB-2014 (NJB)
      !>
      !>        Bug fix and functional change: quaternion sign adjustment
      !>        is no longer performed for the Hermite subtypes (0 and 2).
      !>        If a sign adjustment is needed for quaternions belonging to
      !>        a record of Hermite subtype, an error is signaled. Sign
      !>        adjustment is still performed for the Lagrange subtypes.
      !>
      !>        Corrected in-line comments concerning change of AV units.
      !>
      !>-    SPICELIB Version 2.0.0, 20-NOV-2006 (NJB)
      !>
      !>        Bug fix: this routine now assumes that angular velocity
      !>        and quaternion derivative values stored in the input
      !>        record have units of radians/second.
      !>
      !>        Bug fix: this routine no longer attempts to determine
      !>        the correct sign of quaternion derivatives. The caller
      !>        must supply quaternion derivatives that are suitable
      !>        for interpolation.
      !>
      !>-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments in
      !>        XPOSEG and VSCL calls. Replaced header reference to LDPOOL
      !>        with reference to FURNSH.
      !>
      !>-    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB)
      !>
      !>        Bug fix: angular velocity computation was modified to
      !>        match that used in the corresponding algorithm employed
      !>        by the MEX/Rosetta attitude file reader. The quaternion
      !>        derivative used to derive angular velocity now is the
      !>        derivative of the *unit* quaternion.
      !>
      !>-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments in
      !>        XPOSEG and VSCL calls. Replaced header reference to LDPOOL
      !>        with reference to FURNSH.
      !>
      !>-    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB)
      !>
      !>        Bug fix: angular velocity computation was modified to
      !>        match that used in the corresponding algorithm employed
      !>        by the MEX/Rosetta attitude file reader. The quaternion
      !>        derivative used to derive angular velocity now is the
      !>        derivative of the *unit* quaternion.
      !>
      !>        Letting Q(t) be the quaternion derived by polynomial
      !>        interpolation, and letting UQ(t) be Q(t)/||Q(t)||,
      !>        the quaternion derivative d(UQ)/dt is now used.
      module subroutine cke05 ( ctx, needav, record, cmat, av, clkout )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: q2m, qdq2av, vdistg, vdotg, vhatg, vnormg, xpsgip
      use pepper_inc_ck05
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: needav  !! .TRUE. if angular velocity is requested.
      real(wp), intent(inout) :: record   ( * )  !! Data type 5 record.
      real(wp), intent(out) :: cmat     ( 3, 3 )  !! C-matrix.
      real(wp), intent(out) :: av       ( 3 )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! SCLK associated with C-matrix.

!
!     Index of evaluation epoch in record:
!
      integer(ip), parameter :: epcidx = 1

!
!     Index of subtype code in record:
!
      integer(ip), parameter :: sbtidx = 2

!
!     Index of packet count in record:
!
      integer(ip), parameter :: cntidx = 3

!
!     Index at which packets start; packet base:
!
      integer(ip), parameter :: pktidx = 5

      integer(ip), parameter :: pktbas = pktidx - 1

      real(wp) :: dq     ( 0 : 3 )
      real(wp) :: ds     ( 0 : 3 )
      real(wp) :: locrec ( ckmrsz )
      real(wp) :: mags
      real(wp) :: q      ( 0 : 3 )
      real(wp) :: qneg   ( 0 : 3 )
      real(wp) :: radtrm ( 0 : 3 )
      real(wp) :: rate
      real(wp) :: sclddq ( 0 : 3 )
      real(wp) :: sclkdp
      real(wp) :: state  ( 8 )
      real(wp) :: vbuff  ( 6 )
      real(wp) :: work   ( ckmrsz * 2,  2 )

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: newptr
      integer(ip) :: packsz
      integer(ip) :: prvptr
      integer(ip) :: subtyp
      integer(ip) :: to
      integer(ip) :: ub
      integer(ip) :: xstart
      integer(ip) :: ystart

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      subtyp =  nint( record(sbtidx) )

      if ( subtyp == c05tp0 ) then

         packsz = c05ps0

      else if ( subtyp == c05tp1 ) then

         packsz = c05ps1

      else if ( subtyp == c05tp2 ) then

         packsz = c05ps2

      else if ( subtyp == c05tp3 ) then

         packsz = c05ps3

      else

         call ctx%setmsg ( 'Unexpected CK type 5 subtype # found in ' // &
                       'type 5 segment.'                          )
         call ctx%errint ( '#',  subtyp                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

!
!     Get the packet count and epoch.
!
      n       =  nint( record(cntidx) )
      sclkdp  =        record(epcidx)

!
!     Get the nominal clock rate.
!
      rate = record(4)

!
!     Adjust quaternion "signs" as necessary to minimize distance
!     between successive quaternions. This adjustment is performed
!     only for subtypes that don't store quaternion derivatives
!     (these are the Lagrange subtypes).
!
      if (  ( subtyp == c05tp1 ) .or. ( subtyp == c05tp3 )  ) then
!
!        For these subtypes, only the quaternions themselves need be
!        adjusted.
!
!        PRVPTR is the index of the "previous" quaternion---the one to
!        which the successor and its negative will be compared.
!
         prvptr = pktidx

         do i = 2, n
!
!           NEWPTR points to the quaternion ahead of the one
!           pointed to by PRVPTR.
!
            newptr = pktidx + packsz*(i-1)

            qneg = -record(newptr:newptr+4-1)

!
!           Replace the Ith quaternion with QNEG if QNEG is closer
!           than the current quaternion to the previous quaternion.
!
            if (     vdistg( record(prvptr), qneg,           4 ) &
                < vdistg( record(prvptr), record(newptr), 4 ) ) then

               record(newptr:newptr+4-1) = qneg

            end if

            prvptr = newptr

         end do

      else
!
!        For the Hermite types, if the quaternions need to be adjusted,
!        we have an error condition.
!
!        PRVPTR is the index of the "previous" quaternion---the one to
!        which the successor and its negative will be compared.
!
         prvptr = pktidx

         do i = 2, n
!
!           NEWPTR points to the quaternion ahead of the one
!           pointed to by PRVPTR.
!
            newptr = pktidx + packsz*(i-1)

            qneg = -record(newptr:newptr+4-1)
!
!           Replace the Ith quaternion with QNEG if QNEG is closer
!           than the current quaternion to the previous quaternion.
!
            if (     vdistg( record(prvptr), qneg,           4 ) &
                < vdistg( record(prvptr), record(newptr), 4 ) ) then

               call ctx%setmsg ( 'Quaternion sign error: quaternion at ' &
               //            'index # in the input record is farther ' &
               //            'than its negative from the preceding ' &
               //            'quaternion in the record. Quaternion ' &
               //            'is (#, #, #, #); predecessor is ' &
               //            '(#, #, #, #). This makes the quaternion ' &
               //            'sequence unsuitable for Hermite ' &
               //            'interpolation. The quaternions, and ' &
               //            'if applicable, their derivatives, ' &
               //            'must be adjusted before they are ' &
               //            'passed to this routine.'                 )
               call ctx%errint ( '#', i                                    )
               call ctx%errdp  ( '#', record(newptr  )                     )
               call ctx%errdp  ( '#', record(newptr+1)                     )
               call ctx%errdp  ( '#', record(newptr+2)                     )
               call ctx%errdp  ( '#', record(newptr+3)                     )
               call ctx%errdp  ( '#', record(prvptr  )                     )
               call ctx%errdp  ( '#', record(prvptr+1)                     )
               call ctx%errdp  ( '#', record(prvptr+2)                     )
               call ctx%errdp  ( '#', record(prvptr+3)                     )
               call ctx%sigerr ( 'SPICE(BADQUATSIGN)'                      )
               return

            end if

            prvptr = newptr

         end do

      end if

      if ( subtyp == c05tp1 ) then
!
!        We perform Lagrange interpolation on each quaternion
!        component, and obtain quaternion derivatives from the
!        interpolating polynomials.  The quaternion and derivative
!        gives us angular velocity.
!
!        We'll transpose the pointing information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.  We allow LGRINT to overwrite
!        the state values in the input record, since this saves local
!        storage and does no harm.  (See the header of LGRINT for a
!        description of its work space usage.)
!
         n   =  nint( record(cntidx) )

         call xpsgip ( packsz, n, record(pktidx) )

!
!        We interpolate each state component in turn.
!
         xstart   =   pktidx   +  n * packsz

         do i = 1, packsz

            ystart    =   pktidx  +  n * (i-1)

            call lgrind ( ctx, n, &
                          record(xstart), &
                          record(ystart), &
                          work, &
                          sclkdp, &
                          state(i), &
                          state(i+4)  )

         end do

!
!        The output quaternion is a unitized version of the
!        interpolated state.
!
         mags = vnormg( state, 4 )

         if ( mags == 0.0_wp ) then

            call ctx%setmsg ( 'Quaternion magnitude at SCLK # was zero.' )
            call ctx%errdp  ( '#',  sclkdp                               )
            call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                      )
            return

         end if

         q = (1.0_wp/mags)*state(1:4)

         if ( needav ) then
!
!           Find the time derivative of the unit quaternion:
!           Letting S represent the quaternion portion of STATE, we
!           have
!
!              Q = S/||S||
!
!           Then letting < , > denote the 4-dimensional inner product
!           operator, we have
!
!                         d(S)/dt      < Q, d(S)/dt >
!              d(Q)/dt =  -------  -   -------------- * Q
!                          ||S||            ||S||
!
            ds = state(5:8)

            sclddq = (1.0_wp          / mags)*ds
            radtrm = (vdotg(q,ds,4) / mags)*q

            dq = sclddq - radtrm
!
!           Derive angular velocity from Q and dQ/dt:
!
            call qdq2av ( q, dq, av )

!
!           Scale the AV from radians/tick to radians/second.
!
            av = (1.0_wp/rate)*av

         end if

!
!        Q and if required AV have been assigned.
!

      else if ( subtyp == c05tp3 ) then
!
!        This is the easiest case:  we perform Lagrange interpolation
!        on each quaternion or angular velocity component.
!
!        We'll transpose the pointing information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.  We allow LGRINT to overwrite
!        the state values in the input record, since this saves local
!        storage and does no harm.  (See the header of LGRINT for a
!        description of its work space usage.)
!
         n   =  nint( record(cntidx) )

         call xpsgip ( packsz, n, record(pktidx) )

!
!        We interpolate each state component in turn.
!
         xstart  =  pktidx   +  n * packsz

         if ( needav ) then
            ub  =  packsz
         else
            ub  =  4
         end if

         do i = 1, ub

            ystart    =   pktidx  +  n * (i-1)

            state(i)  =   lgrint ( ctx, n, &
                                   record(xstart), &
                                   record(ystart), &
                                   locrec, &
                                   sclkdp          )
         end do

!
!        The output quaternion is a unitized version of the
!        interpolated state.
!
         call vhatg ( state, 4, q )

         if ( needav ) then
!
!           The angular velocity already is in units of radians/second.
!
            av = state(5:7)

         end if

!
!        Q and if required AV have been assigned.
!

      else
!
!        We have a Hermite-style subtype.  Whether it's subtype 0
!        or 2, we perform Hermite interpolation on the quaternions.
!
!        We interpolate each quaternion component in turn.  Attitude and
!        angular velocity are interpolated separately.
!
         xstart   =   pktidx  +  packsz * n

         do i = 1, 4

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer RECORD.
!
!              In order to perform Hermite interpolation, the
!              quaternions and quaternion derivatives must have a
!              common time scale. So prior to interpolation, we scale
!              the units of the quaternion derivatives from radians/sec
!              to radians/tick.
!
               from         = pktbas + packsz*(j-1) + i
               to           =              2 * j    - 1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 4 ) * rate

            end do

!
!           Interpolate the Ith quaternion and quaternion derivative
!           components.
!
            call hrmint ( ctx, n, &
                          record(xstart), &
                          locrec, &
                          sclkdp, &
                          work, &
                          state(i  ), &
                          state(i+4)      )

         end do

!
!        The output quaternion is a unitized version of the
!        interpolated state.
!
         mags = vnormg( state, 4 )

         if ( mags == 0.0_wp ) then

            call ctx%setmsg ( 'Quaternion magnitude at SCLK # was zero.' )
            call ctx%errdp  ( '#',  sclkdp                               )
            call ctx%sigerr ( 'SPICE(DIVIDEBYZERO)'                      )
            return

         end if

         q = (1.0_wp/mags)*state(1:4)

         if ( needav ) then

            if ( subtyp == c05tp0 ) then
!
!              Find the time derivative of the unit quaternion:
!              Letting S represent the quaternion portion of STATE, we
!              have
!
!                 Q = S/||S||
!
!              Then letting < , > denote the 4-dimensional inner product
!              operator, we have
!
!                            d(S)/dt      < Q, d(S)/dt >
!                 d(Q)/dt =  -------  -   -------------- * Q
!                             ||S||            ||S||
!
               ds = state(5:8)

               sclddq = (1.0_wp          / mags)*ds
               radtrm = (vdotg(q,ds,4) / mags)*q

               dq = sclddq - radtrm
!
!              Derive angular velocity from Q and dQ/dt:
!
               call qdq2av ( q, dq, av )

!
!              Scale the AV from radians/tick to radians/second.
!
               av = (1.0_wp/rate)*av

            else
!
!              This is subtype 2; we perform Hermite interpolation on
!              the angular velocity and its derivative.
!
!              Now interpolate angular velocity, using separate angular
!              velocity data and angular acceleration.
!
               do i = 1, 3

                  do j = 1, n
!
!                    For the Jth input packet, copy the Ith position
!                    and velocity components into the local record
!                    buffer LOCREC.  Note that, as with quaternion
!                    derivatives, we must scale angular acceleration
!                    from radians/sec**2 to radians/(sec*tick) before
!                    interpolating.
!
                     from         = pktbas + packsz*(j-1) +  8  +  i
                     to           =              2 * j    -  1

                     locrec(to  ) = record ( from     )
                     locrec(to+1) = record ( from + 3 ) * rate

                  end do

!
!                 Interpolate the Ith angular velocity and angular
!                 acceleration components of the attitude. We'll
!                 capture the result in a temporary buffer, then
!                 transfer the velocity to the output argument AV.
!
                  call hrmint ( ctx, n, &
                                record(xstart), &
                                locrec, &
                                sclkdp, &
                                work, &
                                vbuff(i  ), &
                                vbuff(i+3)     )
               end do

!
!              Fill in the angular velocity in the output angular
!              velocity vector using the results of interpolating
!              velocity and acceleration.
!
!              The angular velocity is already in units of
!              radians/second.
!
               av = vbuff(1:3)

            end if
!
!           We've handled the type 0 and type 2 cases.
!
         end if
!
!        We've computed the angular velocity AV for the Hermite
!        subtypes, if a.v. was requested.
!
      end if
!
!     We've handled all four subtypes.
!

!
!     Produce a C-matrix from the interpolated quaternion. Set CLKOUT.
!
      call q2m ( q, cmat )

      clkout = record(epcidx)

      return
      end subroutine cke05

      !> Read a pointing record from a CK segment, data type 1.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle of the CK file containing the
      !>              segment.
      !>
      !>     DESCR    is the descriptor of the segment.
      !>
      !>     SCLKDP   is an encoded spacecraft clock time for which
      !>              pointing is being requested. The SPICELIB routines
      !>              SCENCD and SCDECD are used to encode and decode SCLK
      !>              times.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>              The record returned by CKR01 is the one whose time is
      !>              closest to SCLKDP and within TOL units of SCLKDP.
      !>
      !>     NEEDAV   is .TRUE. when angular velocity data is requested.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the pointing record. Contents are as follows:
      !>
      !>                 RECORD( 1 ) = CLKOUT
      !>
      !>                 RECORD( 2 ) = q0
      !>                 RECORD( 3 ) = q1
      !>                 RECORD( 4 ) = q2
      !>                 RECORD( 5 ) = q3
      !>
      !>                 RECORD( 6 ) = Av1  ]
      !>                 RECORD( 7 ) = Av2  |-- Returned optionally
      !>                 RECORD( 8 ) = Av3  ]
      !>
      !>              CLKOUT is the encoded spacecraft clock time for the
      !>              returned pointing values. CLKOUT will be the closest
      !>              time in the segment to the input time as long as it is
      !>              within the input tolerance (see FOUND below). If SCLKDP
      !>              falls at the exact midpoint of two times, the record
      !>              for the greater of the two will be returned.
      !>
      !>              The quantities q0 - q3 represent a quaternion.
      !>              The quantities Av1, Av2, and Av3 represent the angular
      !>              velocity vector, and are returned if the segment
      !>              contains angular velocity data and NEEDAV is .TRUE.
      !>              The components of the angular velocity vector are
      !>              specified relative to the inertial reference frame
      !>              for the segment.
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. FOUND will be .FALSE. when there is no pointing
      !>              instance within the segment whose time falls within
      !>              the requested time tolerance on either side of the
      !>              input time.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified handle does not belong to any file that is
      !>         currently known to be open, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If DESCR is not a valid, packed descriptor of a segment in
      !>         the CK file specified by HANDLE, the results of this routine
      !>         are unpredictable.
      !>
      !>     3)  If the segment is not of data type 1, as specified in the
      !>         third integer component of the segment descriptor,
      !>         the error SPICE(WRONGDATATYPE) is signaled.
      !>
      !>     4)  If there is a need for angular velocity data and the segment
      !>         contains no such data, the error SPICE(NOAVDATA) is signaled.
      !>
      !>## Files
      !>
      !>     The file containing the segment is specified by its handle, and
      !>     should be opened for read, either by CKLPF or DAFOPR.
      !>
      !>## Particulars
      !>
      !>     See the CK Required Reading file for a detailed description of
      !>     the structure of a type 1 pointing segment.
      !>
      !>     This routine searches a type 1 segment for the pointing instance
      !>     whose associated time is closest to the time that pointing was
      !>     requested for. If this time is within the tolerance specified by
      !>     the user, it sets FOUND equal to true and returns information in
      !>     the array RECORD that CKE01 uses to evaluate the pointing at the
      !>     time CLKOUT.
      !>
      !>## Examples
      !>
      !>     The CKRnn routines are usually used in tandem with the CKEnn
      !>     routines, which evaluate the record returned by CKRnn to give
      !>     the pointing information and output time.
      !>
      !>     The following code fragment searches backwards through a file
      !>     (represented by HANDLE) for all segments applicable to the
      !>     Voyager 2 wide angle camera, for a particular spacecraft clock
      !>     time, which have data type 1. It then evaluates the pointing for
      !>     that epoch and prints the result.
      !>
      !>     The search performed here does not mimic the behavior of the CK
      !>     reader APIs CKGP and CKGPAV, which consider data from multiple CK
      !>     files, when available. See the CK Required reading for details.
      !>
      !>     C
      !>     C     - Get the spacecraft clock time. Must encode it for use
      !>     C       in the C-kernel.
      !>     C
      !>     C     - Set the time tolerance high to catch anything close to
      !>     C       the input time.
      !>     C
      !>     C     - We don't need angular velocity data.
      !>     C
      !>           SC     = -32
      !>           INST   = -32002
      !>           TOL    =  1000.D0
      !>           NEEDAV = .FALSE.
      !>           DTYPE  =  1
      !>     C
      !>     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'VGR_SCLK.TSC'        )
      !>           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
      !>     C
      !>     C     Convert the input request time to ticks.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>
      !>     C
      !>     C     Search backwards from the end of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBBS ( HANDLE )
      !>           CALL DAFFPA ( SFND   )
      !>
      !>           DO WHILE ( SFND )
      !>
      !>              CALL DAFGN ( IDENT                 )
      !>              CALL DAFGS ( DESCR                 )
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>              IF ( INST          .EQ. ICD( 1 )  .AND.
      !>          .        DTYPE         .EQ. ICD( 3 )  .AND.
      !>          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
      !>          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
      !>
      !>                 CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                   RECORD, FOUND )
      !>
      !>                 IF ( FOUND ) THEN
      !>
      !>                    CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
      !>
      !>                    WRITE (*,*) 'Segment descriptor and identifier:'
      !>                    WRITE (*,*) DCD, ICD
      !>                    WRITE (*,*) IDENT
      !>
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*) CMAT
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFPA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  The file containing the segment should be opened for read,
      !>         either by CKLPF or DAFOPR.
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
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.2, 12-AUG-2021 (NJB) (JDR)
      !>
      !>        Updated code example to use backwards search. Added
      !>        note regarding difference between this search and those
      !>        performed by the CK reader APIs CKGP and CKGPAV.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 30-AUG-1991 (JML)
      !>
      !>        This routine now checks the segment descriptor to
      !>        determine if it has been given a type 1 segment.
      !>
      !>        The FOUND flag is set to .FALSE. at the beginning of
      !>        the routine.
      !>
      !>        The $Particulars section was changed to provide a more
      !>        general description of the function of this routine. The
      !>        information that was originally in $Particulars was moved
      !>        to the body of the code.
      !>
      !>        The example program was changed so that the tolerance
      !>        and data type are used in selecting which segments to read.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        The example program was corrected so that the input
      !>        instrument code was tested against ICD(1) instead of
      !>        ICD(3).
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
      !>
      !>        1) This routine now checks the segment descriptor, ICD(3),
      !>           to determine if it has been given a type 1 segment.
      !>
      !>        2) The FOUND flag is set to .FALSE. at the beginning of
      !>           the routine. This is done so that if a SPICE error
      !>           is signaled, the FOUND flag will definitely be false.
      !>
      !>        3) The $Particulars section was changed to provide a more
      !>           general description of the function of this routine. The
      !>           information that was originally in $Particulars was moved
      !>           to the body of the code.
      !>
      !>        4) The example program was changed so that the tolerance
      !>           and data type are used in selecting which segments to read.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        1) The example program was corrected so that the input
      !>           instrument code was tested against ICD(1) instead of
      !>           ICD(3).
      !>        2) ROTATIONS was removed from the Required Reading section.
      !>
      !>-    Beta Version 1.1.0, 29-AUG-1990 (MJS) (JEM)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>        1) The variable SCLK was changed to SCLKDP.
      !>        2) The declarations for the parameters QSIZ, QAVSIZ, NDC, and
      !>           NIC were moved from the "Declarations" section of the
      !>           header to the "Local parameters" section of the code below
      !>           the header. These parameters are not meant to modified by
      !>           users.
      !>        3) The variable DIRSIZ has been parameterized in the code
      !>           following the header. DIRSIZ is still 100.
      !>        5) The header was improved and updated to reflect the changes.
      !>        6) The in-code comments were improved.
      !>
      !>-    Beta Version 1.0.0, 17-MAY-1990 (RET) (IMU)
      module subroutine ckr01 ( ctx, handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstcld, lstled
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr  ( * )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Spacecraft clock time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      logical, intent(in) :: needav  !! .TRUE. when angular velocity data is requested.
      real(wp), intent(out) :: record ( * )  !! Pointing data record.
      logical, intent(out) :: found  !! .TRUE. when data is found.

!        DIRSIZ     is the directory size.
!
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
!        QSIZ       is the number of double precision numbers making up
!                   the quaternion portion of a pointing record.
!
!        QAVSIZ     is the number of double precision numbers making up
!                   the quaternion and angular velocity portion of a
!                   pointing record.
!
!        DTYPE      is the data type of the segment that this routine
!                   operates on.
!






      integer(ip), parameter :: dtype = 1

      integer(ip) :: icd    ( nic )
      integer(ip) :: beg
      integer(ip) :: end
      integer(ip) :: nrec
      integer(ip) :: ndir
      integer(ip) :: psiz
      integer(ip) :: group
      integer(ip) :: dirloc
      integer(ip) :: remain
      integer(ip) :: skip
      integer(ip) :: grpndx
      integer(ip) :: i
      integer(ip) :: n

      real(wp) :: dcd    ( ndc )
      real(wp) :: buffer ( dirsiz )

      logical :: fnd

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     To minimize the number of file reads performed during the search,
!     a buffer of 100 double precision numbers is used to read the SCLK
!     times from the C-kernel.  If there are 10,001 or fewer pointing
!     records, at most four reads will be needed to satisfy the request:
!     one to read NREC, one to read in 100 or fewer directory times,
!     one to read 100 or fewer actual times, and then after the
!     appropriate record has been located, one to read the quaternion
!     and angular velocity data.
!
!     One more read would be required for every other group of 10,000
!     records in the segment.
!

!
!     Start off with FOUND set to FALSE.
!
      found = .false.
!
!     We need to look at a few of the descriptor components.
!
!     The unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!
      call dafus ( ctx, descr, ndc, nic, dcd, icd )
!
!     Check to make sure that the segment is type 1.
!
      if ( icd( 3 ) /= dtype ) then

         call ctx%setmsg ( 'The segment is not a type 1 segment.  ' // &
                       'Type is #'                                 )
         call ctx%errint ( '#', icd(3)                                 )
         call ctx%sigerr ( 'SPICE(WRONGDATATYPE)'                      )
         return

      end if

!
!     The size of the record returned depends on whether or not the
!     segment contains angular velocity data.
!
!     This is a convenient place to check if the need for angular
!     velocity data matches the availability.
!
      if ( icd( 4 ) == 1 ) then

         psiz = qavsiz

      else

         psiz = qsiz

         if ( needav ) then

            call ctx%setmsg ( 'Segment does not contain angular velocity '// &
                          'data.'           )
            call ctx%sigerr ( 'SPICE(NOAVDATA)' )
            return

         end if

      end if

!
!     The beginning and ending addresses of the segment are in the
!     descriptor.
!
      beg = icd( 5 )
      end = icd( 6 )

!
!     Get the number of records in this segment, and from that determine
!     the number of directory epochs.
!
      call dafgda ( ctx, handle, end, end, buffer )
      nrec = int( buffer(1) )
      ndir = ( nrec - 1 )/dirsiz

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records. The way the directory is constructed guarantees
!     that we will definitely find the closest time in the segment to
!     SCLKDP in the indicated group.
!
!     There is only one group if there are no directory epochs.
!
      if ( ndir == 0 ) then
         group = 1

      else
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, need to go through all of the
!        pointing numbers (PSIZ*NREC of them), then through all of
!        the SCLKDP times (NREC more) to get to the first SCLK
!        directory.
!
         dirloc = beg + ( psiz + 1 ) * nrec

!
!        Locate the first directory epoch greater than SCLKDP. Read in
!        as many as DIRSIZ directory epochs at a time for comparison.
!
         fnd    = .false.
         remain =  ndir
         group  =  0

         do while ( .not. fnd )
!
!           The number of records to read in the buffer.
!
            n = min( remain, dirsiz )

            call dafgda ( ctx, handle, dirloc, dirloc + n - 1, buffer )

            remain = remain - n

!
!           If we find the first directory time greater than or equal
!           to the epoch, we're done.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
            i = lstled( sclkdp, n, buffer )

            if ( i < n ) then
               group =  group + i + 1
               fnd   = .true.

            else if ( remain == 0 ) then
               group =  ndir + 1
               fnd   = .true.

            else
               dirloc = dirloc + n
               group  = group  + n

            end if

         end do

      end if

!
!     Now we know which group of DIRSIZ (or less) times to look at.
!     Out of the NREC SCLKDP times, the number that we should skip over
!     to get to the proper group is DIRSIZ*( GROUP - 1 ).
!
      skip = dirsiz*( group - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, need to pass through
!     PSIZ*NREC pointing numbers to get to the first SCLKDP time.
!     Then we skip over the number just computed above.
!
      grpndx = beg + (nrec * psiz) + skip

!
!     The number of times that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
      n = min( dirsiz, nrec - skip )

      call dafgda ( ctx, handle, grpndx, grpndx + n - 1, buffer )

!
!     Find the time in the group closest to the input time, and see
!     if it's within tolerance.
!
      i = lstcld( sclkdp, n, buffer )

      if ( abs( sclkdp - buffer( i ) ) > tol ) return

!
!     Now we know the exact record that we want.
!
!     RECORD( 1 ) holds CLKOUT.
!
      found       = .true.
      record( 1 ) =  buffer( i )

!
!     We need the Ith pointing record out of this group of DIRSIZ.
!     This group of DIRSIZ is SKIP records into the beginning
!     of the segment. And each record is PSIZ big.
!
      n = beg + psiz*( skip + i - 1 )

      call dafgda ( ctx, handle, n, n + psiz - 1, record( 2 ) )

!
!     That is all.
!
      return
      end subroutine ckr01

      !> Read a pointing record from a CK segment, data type 2.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle of the CK file containing the
      !>              segment.
      !>
      !>     DESCR    is the descriptor of the segment.
      !>
      !>     SCLKDP   is the encoded spacecraft clock time for which
      !>              pointing is being requested.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>              When SCLKDP falls within the bounds of one of the
      !>              intervals then the tolerance has no effect. However,
      !>              if the request time is not in one of the intervals
      !>              then the tolerance is used to determine if pointing
      !>              at one of the interval endpoints should be returned.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the pointing record. Contents are as follows:
      !>
      !>                 RECORD( 1  ) = Start time of interval.
      !>                 RECORD( 2  ) = Time for which pointing was found.
      !>                 RECORD( 3  ) = Seconds per tick rate.
      !>
      !>                 RECORD( 4  ) = q0
      !>                 RECORD( 5  ) = q1
      !>                 RECORD( 6  ) = q2
      !>                 RECORD( 7  ) = q3
      !>
      !>                 RECORD( 8  ) = av1
      !>                 RECORD( 9  ) = av2
      !>                 RECORD( 10 ) = av3
      !>
      !>              The quantities q0 - q3 are the components of the
      !>              quaternion that represents the C-matrix associated with
      !>              the start time of the interval. The quantities av1,
      !>              av2, and av3 represent the angular velocity vector of
      !>              the interval. The components of the angular velocity
      !>              vector are specified relative to the inertial reference
      !>              frame of the segment.
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified handle does not belong to any file that is
      !>         currently known to be open, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If DESCR is not a valid, packed descriptor of a segment in
      !>         the CK file specified by HANDLE, the results of this routine
      !>         are unpredictable.
      !>
      !>     3)  If the segment is not of data type 2, as specified in the
      !>         third integer component of the segment descriptor,
      !>         the error SPICE(WRONGDATATYPE) is signaled.
      !>
      !>## Files
      !>
      !>     The file containing the segment is specified by its handle, and
      !>     should be opened for read, either by CKLPF or DAFOPR.
      !>
      !>## Particulars
      !>
      !>     See the CK Required Reading file for a detailed description of
      !>     the structure of a type 2 pointing segment.
      !>
      !>     This routine searches a type 2 segment and determines if the
      !>     request for pointing can be satisfied by the segment. If so,
      !>     then it returns information in the array RECORD that CKE02 uses
      !>     to evaluate the pointing at the time for which pointing was found.
      !>
      !>     When the time for which pointing was requested falls within one
      !>     of the intervals then the returned time is the same as the
      !>     requested time. However, when the request time is not within any
      !>     of the intervals then the returned time will be the interval
      !>     endpoint closest to the request time, provided that endpoint is
      !>     within the tolerance specified by the user.
      !>
      !>## Examples
      !>
      !>     The CKRnn routines are usually used in tandem with the CKEnn
      !>     routines, which evaluate the record returned by CKRnn to give
      !>     the pointing information and output time.
      !>
      !>     The following code fragment searches backwards through a file
      !>     (attached to HANDLE) for all segments applicable to the Voyager 2
      !>     wide angle camera, for a particular spacecraft clock time, that
      !>     are of data types 1 or 2. It then evaluates the pointing for that
      !>     epoch and prints the result.
      !>
      !>     The search performed here does not mimic the behavior of the CK
      !>     reader APIs CKGP and CKGPAV, which consider data from multiple CK
      !>     files, when available. See the CK Required reading for details.
      !>
      !>           SC     = -32
      !>           INST   = -32002
      !>     C
      !>     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'VGR_SCLK.TSC'        )
      !>           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE )
      !>     C
      !>     C     Get the spacecraft clock time. Must encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>
      !>     C
      !>     C     Search backwards from the end of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBBS ( HANDLE )
      !>           CALL DAFFPA ( SFND   )
      !>
      !>           DO WHILE ( SFND )
      !>
      !>              CALL DAFGN ( IDENT                 )
      !>              CALL DAFGS ( DESCR                 )
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>              IF ( INST          .EQ. ICD( 1 )  .AND.
      !>          .        SCLKDP + TOL  .GE. DCD( 1 )  .AND.
      !>          .        SCLKDP - TOL  .LE. DCD( 2 ) ) THEN
      !>
      !>                 DTYPE = ICD ( 3 )
      !>
      !>                 IF ( DTYPE .EQ. 1 ) THEN
      !>
      !>                    CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                      RECORD, FOUND                       )
      !>
      !>                    IF ( FOUND ) THEN
      !>                       CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
      !>                    END IF
      !>
      !>                 ELSE  IF ( DTYPE .EQ. 2 ) THEN
      !>
      !>                    CALL CKR02 ( HANDLE, DESCR, SCLKDP, TOL,
      !>          .                      RECORD, FOUND )
      !>
      !>                    IF ( FOUND ) THEN
      !>                       CALL CKE02 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )
      !>                    END IF
      !>
      !>                 END IF
      !>
      !>                 IF ( FOUND ) THEN
      !>
      !>                    WRITE (*,*) 'Segment descriptor and identifier:'
      !>                    WRITE (*,*) DCD, ICD
      !>                    WRITE (*,*) IDENT
      !>
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*) CMAT
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFPA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  The file containing the segment should be opened for read,
      !>         either by CKLPF or DAFOPR.
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
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.2, 06-JUL-2021 (NJB) (JDR)
      !>
      !>        Updated code example to use backwards search. Added
      !>        note regarding difference between this search and those
      !>        performed by the CK reader APIs CKGP and CKGPAV.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
      module subroutine ckr02 ( ctx, handle, descr, sclkdp, tol, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstled
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr  ( * )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Spacecraft clock time.
      real(wp), intent(in) :: tol  !! Time tolerance
      real(wp), intent(out) :: record ( * )  !! Pointing data record.
      logical, intent(out) :: found  !! .TRUE. when data is found.
      real(wp) :: stopi_1(1)  !! one-element copy of stopi (array argument)

!        DIRSIZ     is the directory size.
!
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
!        PSIZ       is the number of double precision numbers making up
!                   the record containing the quaternion, angular
!                   velocity vector, and seconds per tick rate.
!
!        DTYPE      is the data type of the segment that this routine
!                   operates on.
!



      integer(ip), parameter :: psiz = 8

      integer(ip), parameter :: dtype = 2

      integer(ip) :: icd    ( nic )
      integer(ip) :: beg
      integer(ip) :: end
      integer(ip) :: nrec
      integer(ip) :: ndir
      integer(ip) :: group
      integer(ip) :: dirloc
      integer(ip) :: remain
      integer(ip) :: skip
      integer(ip) :: grpndx
      integer(ip) :: index
      integer(ip) :: stploc
      integer(ip) :: arrsiz
      integer(ip) :: i
      integer(ip) :: n

      real(wp) :: dcd    ( ndc    )
      real(wp) :: buffer ( dirsiz )
      real(wp) :: prec   ( psiz   )
      real(wp) :: start
      real(wp) :: clkout
      real(wp) :: stopi
      real(wp) :: diff1
      real(wp) :: diff2

      logical :: fnd

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     To minimize the number of file reads performed during the search,
!     a buffer of 100 double precision numbers is used to read the SCLK
!     times from the C-kernel.  If there are 10,001 or fewer pointing
!     records, at most four reads will be needed to satisfy the request:
!     one to read in 100 or fewer directory times, one to read 100 or
!     fewer interval start times, one to read from the stop times, and
!     then, after the appropriate record has been located, one to read
!     the pointing record.
!
!     One more read would be required for every other group of 10,000
!     records in the segment.
!

!
!     Start off with FOUND equal to false.
!
      found = .false.
!
!     We need to look at a few of the descriptor components.
!
!     The unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!

      call dafus ( ctx, descr, ndc, nic, dcd, icd )

!
!     Check to make sure that the segment is type 2.
!
      if ( icd( 3 ) /= dtype ) then

         call ctx%setmsg ( 'The segment is not a type 2 segment.  ' // &
                       'Type is #'                                 )
         call ctx%errint ( '#', icd(3)                                 )
         call ctx%sigerr ( 'SPICE(WRONGDATATYPE)'                      )
         return

      end if

!
!     The beginning and ending addresses of the segment are in the
!     descriptor.
!
      beg = icd( 5 )
      end = icd( 6 )

!
!     Get the number of records in this segment, and from that determine
!     the number of directory epochs.
!
!        Based on the structure of a type 2 segment, the size of a
!        segment with N pointing intervals is given as follows:
!
!           ARRSIZ  =  PSIZ * N  +  2 * N  +  ( N-1 ) / 100       (1)
!
!        In the above equation PSIZ is eight and integer arithmetic is
!        used.  This equation is equivalent to:
!
!           100 * ARRSIZ  =  1000 * N  + ( N-1 ) * 100            (2)
!                                        -------
!                                          100
!
!        If we can eliminate the integer division then, since all of
!        the other values represent whole numbers, we can solve the
!        equation for N in terms of ARRSIZ by using double precision
!        arithmetic and then rounding the result to the nearest integer.
!
!        This next equation uses double precision arithmetic and is
!        equivalent to (2):
!
!           100 * ARRSIZ  = 1000 * N + ( N-1 ) - ( N-1 ) MOD 100  (3)
!
!        Which means:
!
!           100 * ARRSIZ + 1     ( N-1 ) MOD 100
!           ----------------  +  ---------------   =   N          (4)
!                1001                 1001
!
!         Since the second term on the left side of (4) is always less
!         than 0.1, the first term will always round to the correct
!         value of N.
!
      arrsiz = end - beg + 1

      nrec   = nint (  ( 100.0_wp * (real(arrsiz, wp)) + 1.0_wp ) / 1001.0_wp  )

      ndir   = ( nrec - 1 ) / dirsiz

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records.
!
!     There is only one group if there are no directory epochs.
!
      if ( ndir == 0 ) then
         group = 1

      else
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, we need to go through all of the
!        pointing numbers (PSIZ*NREC of them), then through all of
!        the SCLK start and stop times (2*NREC more) to get to the
!        first SCLK directory.
!
         dirloc = beg + ( psiz + 2 ) * nrec

!
!        Locate the last directory epoch less than or equal to SCLKDP.
!
!        Read in as many as DIRSIZ directory epochs at a time for
!        comparison.
!
         fnd    = .false.
         remain =  ndir
         group  =  0

         do while ( .not. fnd )
!
!           The number of records to read in the buffer.
!
            n = min( remain, dirsiz )

            call dafgda ( ctx, handle, dirloc, dirloc + n - 1, buffer )

            remain = remain - n

!
!           Determine the last directory element in BUFFER that's less
!           than or equal to SCLKDP.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
            i = lstled( sclkdp, n, buffer )

            if ( i < n ) then
               group =  group + i + 1
               fnd   = .true.

            else if ( remain == 0 ) then
               group =  ndir + 1
               fnd   = .true.

            else
               dirloc = dirloc + n
               group  = group  + n

            end if

         end do

      end if

!
!     Now we know which group of DIRSIZ (or less) times to look at.
!     Out of the NREC START times, the number that we should skip over
!     to get to the proper group is DIRSIZ*( GROUP - 1 ).
!
      skip = dirsiz * ( group - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, we need to pass through
!     PSIZ*NREC pointing numbers to get to the first START time.
!     Then we skip over the number just computed above.
!
      grpndx = beg + nrec * ( psiz ) + skip

!
!     The number of times that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
      n = min( dirsiz, nrec - skip )

      call dafgda ( ctx, handle, grpndx, grpndx + n - 1, buffer )

!
!     Find the largest time in the group less than or equal to the input
!     time.
!
      i = lstled ( sclkdp, n, buffer )

!
!     If the request time does not fall into one of the intervals, then
!     there are several cases in which this routine can return an
!     endpoint of an interval.
!
!        1)  If I = 0 then the request time falls before the first START
!            time in the group.  Because of the way that the directory
!            is constructed we already know that the preceding STOP
!            time is not the right one so all we have to check is if
!            SCLKDP + TOL is greater than or equal to the first START
!            time of the group.
!
!        2)  If I = N and the request time is not in the Nth interval
!            then we know that the request time is after the last STOP
!            time in the group.  Because of the way that the directory
!            is constructed we already know that the following START
!            time is not the right one so all we have to check is if
!            SCLKDP - TOL is less than or equal to the last STOP time
!            of the group.
!
!        3)  Finally, if I is between 1 and N-1 and the request time
!            does not fall in any of the intervals then we need to
!            return the closer of STOP(I) or START(I+1) if it is
!            within TOL of SCLKDP.
!
!     If SCLKDP is less than the first time in BUFFER then check to see
!     if we want the first START time in the group.
!
      if ( i == 0 ) then

         if ( ( sclkdp + tol ) >= buffer(1) ) then

            found  = .true.
            start  =  buffer(1)
            clkout =  buffer(1)
            index  =  1

         else

            return

         end if

      else

!
!        I is not equal to zero. Determine if the request time falls
!        within the Ith interval.
!
         stploc = beg + nrec*( psiz + 1 ) + skip + i - 1

         call dafgda ( ctx, handle, stploc, stploc, stopi_1 )
         stopi = stopi_1(1)

         if ( sclkdp <= stopi )  then

            found  = .true.
            start  =  buffer(i)
            clkout =  sclkdp
            index  =  i

         else

!
!           The request time does not fall within the interval. Check
!           to see if the Ith STOP time or the (I+1)th START time
!           satisfy the request.
!
!           If I = N then we need to consider only the STOP time
!           because of the way that the directory is constructed.
!
            if ( i == n ) then

               if ( ( sclkdp - tol ) <= stopi ) then

                  found  = .true.
                  start  =  buffer(i)
                  clkout =  stopi
                  index  =  i

               else

                  return

               end if

            else

!
!              Find which time SCLKDP is closest to and then see if
!              it is within the tolerance.
!
               diff1 = sclkdp      - stopi
               diff2 = buffer(i+1) - sclkdp

               if ( min ( diff1, diff2 ) <= tol ) then

                  found = .true.
!
!                 Notice that if the request time is equidistant from
!                 the STOP and START time the START time will be chosen.
!
                  if ( diff2 <= diff1 ) then

                     start  = buffer(i+1)
                     clkout = buffer(i+1)
                     index  = i+1

                  else

                     start  = buffer(i)
                     clkout = stopi
                     index  = i

                  end if

               else

                  return

               end if

            end if

         end if

      end if
!
!     Now we know the exact record that we want and can begin
!     constructing the output record.
!
!     RECORD( 1 ) holds the interval start time.
!     RECORD( 2 ) holds the time for which pointing was found (CLKOUT).
!
      record( 1 ) =  start
      record( 2 ) =  clkout

!
!     We need the pointing record out of GROUP indexed by INDEX.
!     This group of size DIRSIZ is SKIP records into the beginning
!     of the segment. And each record is PSIZ big.
!
      n = beg + psiz * ( skip + index - 1 )

      call dafgda ( ctx, handle, n, n + psiz - 1, prec )

      record ( 3 ) = prec ( psiz )

      record(4:10) = prec(1:7)

!
!     That is all.
!
      return
      end subroutine ckr02

      !> Read a pointing record from a CK segment, data type 3.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle of the CK file containing the
      !>              segment.
      !>
      !>     DESCR    is the descriptor of the segment.
      !>
      !>     SCLKDP   is the encoded spacecraft clock time for which
      !>              pointing is being requested.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>              When SCLKDP falls within the bounds of one of the
      !>              interpolation intervals then the tolerance has no
      !>              effect because pointing will be returned at the
      !>              request time.
      !>
      !>              However, if the request time is not in one of the
      !>              intervals, then the tolerance is used to determine
      !>              if pointing at one of the interval endpoints should
      !>              be returned.
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record that CKE03 will evaluate to determine
      !>              the pointing.
      !>
      !>              When the request time falls within an interval for
      !>              which linear interpolation is valid, the values of
      !>              the two pointing instances that bracket the request
      !>              time are returned in RECORD as follows:
      !>
      !>                 RECORD( 1  ) = Left bracketing SCLK time.
      !>
      !>                 RECORD( 2  ) = lq0  \
      !>                 RECORD( 3  ) = lq1   \    Left bracketing
      !>                 RECORD( 4  ) = lq2   /      quaternion.
      !>                 RECORD( 5  ) = lq3  /
      !>
      !>                 RECORD( 6  ) = lav1 \     Left bracketing
      !>                 RECORD( 7  ) = lav2       angular velocity
      !>                 RECORD( 8  ) = lav3 /       ( optional )
      !>
      !>                 RECORD( 9  ) = Right bracketing SCLK time.
      !>
      !>                 RECORD( 10 ) = rq0  \
      !>                 RECORD( 11 ) = rq1   \    Right bracketing
      !>                 RECORD( 12 ) = rq2   /       quaternion.
      !>                 RECORD( 13 ) = rq3  /
      !>
      !>                 RECORD( 14 ) = rav1 \     Right bracketing
      !>                 RECORD( 15 ) = rav2       angular velocity
      !>                 RECORD( 16 ) = rav3 /       ( optional )
      !>
      !>                 RECORD( 17 ) = pointing request time, SCLKDP.
      !>
      !>              The quantities lq0 - lq3 and rq0 - rq3 are the
      !>              components of the quaternions that represent the
      !>              C-matrices associated with the times that bracket
      !>              the requested time.
      !>
      !>              The quantities lav1, lav2, lav3 and rav1, rav2, rav3
      !>              are the components of the angular velocity vectors at
      !>              the respective bracketing times. The components of the
      !>              angular velocity vectors are specified relative to
      !>              the inertial reference frame of the segment.
      !>
      !>              If the request time does not fall within an
      !>              interpolation interval, but is within TOL of an
      !>              interval endpoint, the values of that pointing
      !>              instance are returned in both parts of RECORD
      !>              ( i.e. RECORD(1-9) and RECORD(10-16) ).
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. This occurs when the time for which pointing
      !>              is requested falls inside one of the interpolation
      !>              intervals, or when the request time is within the
      !>              tolerance of an interval endpoint.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified handle does not belong to an open DAF file,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If DESCR is not a valid descriptor of a segment in the CK
      !>         file specified by HANDLE, the results of this routine are
      !>         unpredictable.
      !>
      !>     3)  If the segment is not of data type 3, as specified in the
      !>         third integer component of the segment descriptor,
      !>         the error SPICE(WRONGDATATYPE) is signaled.
      !>
      !>     4)  If angular velocity data was requested but the segment
      !>         contains no such data, the error SPICE(NOAVDATA) is signaled.
      !>
      !>## Files
      !>
      !>     The file containing the segment is specified by its handle and
      !>     should be opened for read or write access, either by CKLPF,
      !>     DAFOPR, or DAFOPW.
      !>
      !>## Particulars
      !>
      !>     See the CK Required Reading file for a detailed description of
      !>     the structure of a type 3 pointing segment.
      !>
      !>     When the time for which pointing was requested falls within an
      !>     interpolation interval, then FOUND will be true and RECORD will
      !>     contain the pointing instances in the segment that bracket the
      !>     request time.  CKE03 will evaluate RECORD to give pointing at
      !>     the request time.
      !>
      !>     However, when the request time is not within any of the
      !>     interpolation intervals, then FOUND will be true only if the
      !>     interval endpoint closest to the request time is within the
      !>     tolerance specified by the user. In this case both parts of
      !>     RECORD will contain this closest pointing instance, and CKE03
      !>     will evaluate RECORD to give pointing at the time associated
      !>     with the returned pointing instance.
      !>
      !>## Examples
      !>
      !>     The CKRnn routines are usually used in tandem with the CKEnn
      !>     routines, which evaluate the record returned by CKRnn to give
      !>     the pointing information and output time.
      !>
      !>     The following code fragment searches backwards through all of the
      !>     segments in a file applicable to the Mars Observer spacecraft bus
      !>     that are of data type 3, for a particular spacecraft clock time.
      !>     It then evaluates the pointing for that epoch and prints the
      !>     result.
      !>
      !>     The search performed here does not mimic the behavior of the CK
      !>     reader APIs CKGP and CKGPAV, which consider data from multiple CK
      !>     files, when available. See the CK Required reading for details.
      !>
      !>           CHARACTER*(20)        SCLKCH
      !>           CHARACTER*(20)        SCTIME
      !>           CHARACTER*(40)        IDENT
      !>
      !>           INTEGER               I
      !>           INTEGER               SC
      !>           INTEGER               INST
      !>           INTEGER               HANDLE
      !>           INTEGER               DTYPE
      !>           INTEGER               ICD      (    6 )
      !>
      !>           DOUBLE PRECISION      SCLKDP
      !>           DOUBLE PRECISION      TOL
      !>           DOUBLE PRECISION      CLKOUT
      !>           DOUBLE PRECISION      DESCR    (    5 )
      !>           DOUBLE PRECISION      DCD      (    2 )
      !>           DOUBLE PRECISION      RECORD   (   17 )
      !>           DOUBLE PRECISION      CMAT     ( 3, 3 )
      !>           DOUBLE PRECISION      AV       (    3 )
      !>
      !>           LOGICAL               NEEDAV
      !>           LOGICAL               FND
      !>           LOGICAL               SFND
      !>
      !>           SC     = -94
      !>           INST   = -94000
      !>           DTYPE  =  3
      !>           NEEDAV = .FALSE.
      !>
      !>     C
      !>     C     Load the MO SCLK kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'MO_SCLK.TSC'       )
      !>           CALL DAFOPR ( 'MO_CK.BC',  HANDLE )
      !>     C
      !>     C     Get the spacecraft clock time. Then encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>     C
      !>     C     Use a tolerance of 2 seconds ( half of the nominal
      !>     C     separation between MO pointing instances ).
      !>     C
      !>           CALL SCTIKS ( SC, '0000000002:000', TOL )
      !>
      !>     C
      !>     C     Search backwards from the end of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBBS ( HANDLE )
      !>           CALL DAFFPA ( SFND   )
      !>
      !>           FND    = .FALSE.
      !>
      !>           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
      !>
      !>     C
      !>     C        Get the segment identifier and descriptor.
      !>     C
      !>
      !>              CALL DAFGN ( IDENT                 )
      !>              CALL DAFGS ( DESCR                 )
      !>     C
      !>     C        Unpack the segment descriptor into its integer and
      !>     C        double precision components.
      !>     C
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>     C
      !>     C        Determine if this segment should be processed.
      !>     C
      !>              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
      !>          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
      !>          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
      !>          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
      !>
      !>                 CALL CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                   RECORD, FND )
      !>
      !>                 IF ( FND ) THEN
      !>
      !>                    CALL CKE03 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
      !>
      !>                    CALL SCDECD ( SC, CLKOUT, SCTIME )
      !>
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Segment identifier: ', IDENT
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Pointing returned for time: ',
      !>          .                      SCTIME
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
      !>                    WRITE (*,*)
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFPA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  The file containing the segment should be opened for read
      !>         or write access either by CKLPF, DAFOPR, or DAFOPW.
      !>
      !>     2)  The record returned by this routine is intended to be
      !>         evaluated by CKE03.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.2, 12-AUG-2021 (NJB) (JDR)
      !>
      !>        Updated code example to use backwards search. Added
      !>        note regarding difference between this search and those
      !>        performed by the CK reader APIs CKGP and CKGPAV.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
      module subroutine ckr03 ( ctx, handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_arrays, only: lstled, lstltd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr  ( * )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Pointing request time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      logical, intent(in) :: needav  !! Angular velocity request flag.
      real(wp), intent(out) :: record ( * )  !! Pointing data record.
      logical, intent(out) :: found  !! .TRUE. when data is found.
      real(wp) :: nstart_1(1)  !! one-element copy of nstart (array argument)
      real(wp) :: start_1(1)  !! one-element copy of start (array argument)
      real(wp) :: lsclk_1(1)  !! one-element copy of lsclk (array argument)
      integer(ip), parameter :: bufsiz = 100
      integer(ip), parameter :: dtype = 3
      integer(ip) :: icd    ( ni )
      integer(ip) :: beg
      integer(ip) :: end
      integer(ip) :: numrec
      integer(ip) :: numint
      integer(ip) :: nrdir
      integer(ip) :: nidir
      integer(ip) :: group
      integer(ip) :: dirloc
      integer(ip) :: remain
      integer(ip) :: skip
      integer(ip) :: grpadd
      integer(ip) :: addr
      integer(ip) :: laddr
      integer(ip) :: raddr
      integer(ip) :: psiz
      integer(ip) :: i
      integer(ip) :: n
      real(wp) :: dcd    ( nd     )
      real(wp) :: buffer ( bufsiz )
      real(wp) :: lsclk
      real(wp) :: rsclk
      real(wp) :: rdiff
      real(wp) :: ldiff
      real(wp) :: start
      real(wp) :: nstart
      logical :: fnd
      if ( .not. allocated ( ctx%ckr03_state ) ) allocate ( ctx%ckr03_state )
      associate ( prevs => ctx%ckr03_state%prevs, &
                  prevn => ctx%ckr03_state%prevn, &
                  lhand => ctx%ckr03_state%lhand, &
                  lbeg => ctx%ckr03_state%lbeg, &
                  lend => ctx%ckr03_state%lend )
      if ( ctx%failed() ) return

!
!     Start off with FOUND equal to false just in case a SPICELIB error
!     is signaled and the return mode is not set to ABORT.
!
      found = .false.

!
!     We need to look at a few of the descriptor components.
!
!     The unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!

      call dafus ( ctx, descr, nd, ni, dcd, icd )

!
!     Check to make sure that the segment is type 3.
!
      if ( icd( 3 ) /= dtype ) then

         call ctx%setmsg ( 'The segment is not a type 3 segment.  '   // &
                       'Type is #'                                 )
         call ctx%errint ( '#', icd(3)                                 )
         call ctx%sigerr ( 'SPICE(WRONGDATATYPE)'                      )
         return

      end if

!
!     Does this segment contain angular velocity?
!
      if ( icd ( 4 ) == 1 ) then

         psiz = qavsiz

      else

         psiz = qsiz

         if ( needav ) then

            call ctx%setmsg ( 'Segment does not contain angular velocity '// &
                          'data.'                                      )
            call ctx%sigerr ( 'SPICE(NOAVDATA)'                            )
            return

         end if

      end if

!
!     The beginning and ending addresses of the segment are in the
!     descriptor.
!
      beg = icd( 5 )
      end = icd( 6 )

!
!     The procedure used in finding a record to satisfy the request
!     for pointing is as follows:
!
!        1) Find the two pointing instances in the segment that bracket
!           the request time.
!
!           The pointing instance that brackets the request time on the
!           left is defined to be the one associated with the largest
!           time in the segment that is less than or equal to SCLKDP.
!
!           The pointing instance that brackets the request time on the
!           right is defined to be the one associated with the first
!           time in the segment greater than SCLKDP.
!
!           Since the times in the segment are strictly increasing the
!           left and right bracketing pointing instances are always
!           adjacent.
!
!        2) Determine if the bracketing times are in the same
!           interpolation interval.
!
!        3) If they are, then pointing at the request time may be
!           linearly interpolated from the bracketing times.
!
!        4) If the times that bracket the request time are not in the
!           same interval then, since they are adjacent in the segment
!           and since intervals begin and end at actual times, they must
!           both be interval endpoints.  Return the pointing instance
!           associated with the endpoint closest to the request time,
!           provided that it is within the tolerance.
!

!
!     Get the number of intervals and pointing instances ( records )
!     in this segment, and from that determine the number of respective
!     directory epochs.
!
      call dafgda ( ctx, handle, end - 1, end, buffer )

      numint  = nint ( buffer ( 1 ) )
      numrec  = nint ( buffer ( 2 ) )

      nidir   = ( numint - 1 ) / dirsiz
      nrdir   = ( numrec - 1 ) / dirsiz

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT. You need
!     need to do this only after the first call to DAFGDA.
!
      if ( ctx%failed() ) return

!
!     To find the times that bracket the request time we will first
!     find the greatest directory time less than the request time.
!     This will narrow down the search to a group of DIRSIZ or fewer
!     times where the Jth group is defined to contain SCLK times
!     ((J-1)*DIRSIZ + 1) through (J*DIRSIZ).
!
!     For example if DIRSIZ = 100 then:
!
!                         group   first time #     last time #
!                         -----  ---------------   ------------
!                           1            1             100
!                           2          101             200
!                           .            .               .
!                           .            .               .
!                          10          901            1000
!                           .            .               .
!                           .            .               .
!                     NRDIR+1     (NRDIR)*100+1     NUMREC
!
!     Thus if the Ith directory time is the largest one less than
!     our request time SCLKDP, then we know that:
!
!       SCLKS ( DIRSIZ * I ) <  SCLKDP  <= SCLKS ( DIRSIZ * (I+1) )
!
!     where SCLKS is taken to be the array of NUMREC times associated
!     with the pointing instances.
!
!     Therefore, at least one of the bracketing times will come from
!     the (I+1)th group.
!

!
!     There is only one group if there are no directory epochs.
!
      if ( nrdir == 0 ) then

         group = 1

      else
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, we need to go through all of the
!        pointing numbers (PSIZ*NUMREC of them) and then through all of
!        the NUMREC SCLK times.
!
         dirloc = beg + ( psiz + 1 ) * numrec
!
!        Search through the directory times.  Read in as many as BUFSIZ
!        directory epochs at a time for comparison.
!
         fnd    = .false.
         remain =  nrdir
         group  =  0

         do while ( .not. fnd )
!
!           The number of records to read into the buffer.
!
            n = min( remain, bufsiz )

            call dafgda ( ctx, handle, dirloc, dirloc + n - 1, buffer )

            remain = remain - n
!
!           Determine the last directory element in BUFFER that's less
!           than SCLKDP.
!
            i = lstltd( sclkdp, n, buffer )

            if ( i < n ) then

               group =  group + i + 1
               fnd   = .true.

            else if ( remain == 0 ) then
!
!              The request time is greater than the last directory time
!              so we want the last group in the segment.
!
               group =  nrdir + 1
               fnd   = .true.

            else
!
!              Need to read another block of directory times.
!
               dirloc = dirloc + n
               group  = group  + n

            end if

         end do

      end if

!
!     Now we know which group of DIRSIZ (or less) times to look at.
!     Out of the NUMREC SCLK times, the number that we should skip over
!     to get to the proper group is DIRSIZ * ( GROUP - 1 ).
!
      skip = dirsiz * ( group - 1 )

!
!     From this we can compute the address in the segment of the group
!     of times we want.  From the beginning, we need to pass through
!     PSIZ * NUMREC pointing numbers to get to the first SCLK time.
!     Then we skip over the number just computed above.
!
      grpadd = beg  +  ( numrec * psiz )  +  skip

!
!     The number of times that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
      n = min ( dirsiz, numrec - skip )

      call dafgda ( ctx, handle, grpadd, grpadd + n - 1, buffer )

!
!     Find the largest time in the group less than or equal to the input
!     time.
!
      i = lstled ( sclkdp, n, buffer )

!
!     Find the pointing instances in the segment that bracket the
!     request time and calculate the addresses for the pointing data
!     associated with these times. For cases in which the request time
!     is equal to one of the times in the segment, that time will be
!     the left bracketing time of the returned pair.
!
!     Need to handle the cases when the request time is greater than
!     the last or less than the first time in the segment separately.
!

      if ( i == 0 ) then

         if ( group == 1 ) then
!
!           The time occurs before the first time in the segment. Since
!           this time cannot possibly be in any of the intervals, the
!           first time can satisfy the request for pointing only if it
!           is within the tolerance of the request time.
!
            if ( ( buffer(1) - sclkdp ) <= tol ) then

               record ( 1 ) = buffer(1)
               record ( 9 ) = buffer(1)
!
!              Calculate the address of the quaternion and angular
!              velocity data.  Then read it from the file.
!
               call dafgda ( ctx, handle, beg, beg + psiz - 1, buffer )

               record(2:2+psiz-1) = buffer(1:psiz)
               record(10:10+psiz-1) = buffer(1:psiz)

               record ( 17 ) = sclkdp

               found = .true.

            end if

            return

         else

!
!           The first time in the current group brackets the request
!           time on the right and the last time from the preceding
!           group brackets on the left.
!
            rsclk = buffer ( 1 )

            raddr = beg  +  skip * psiz

            call dafgda ( ctx, handle, grpadd - 1, grpadd - 1, lsclk_1 )
            lsclk = lsclk_1(1)

            laddr = raddr - psiz

         end if

      else  if ( i == n ) then

!
!        There are two possible cases, but the same action can handle
!        both.
!
!        1) If this is the last group ( NRDIR + 1 ) then the request
!           time occurs on or after the last time in the segment.
!           In either case this last time can satisfy the request for
!           pointing only if it is within the tolerance of the request
!           time.
!
!        2) The request time is greater than or equal to the last time
!           in this group. Since this time is the same as the (I+1)th
!           directory time, and since the search on the directory times
!           used a strictly less than test, we know that the request
!           time must be equal to this time.  Just return the pointing
!           instance associated with the request time.  ( Note that
!           SCLKDP - BUFFER(N) will be zero in this case. )
!

         if ( ( sclkdp - buffer(n) ) <= tol ) then

            record ( 1 ) = buffer(n)
            record ( 9 ) = buffer(n)
!
!           Calculate the address of the quaternion and angular
!           velocity data.  Then read it from the file.
!
            addr = beg + psiz * ( skip + n - 1 )

            call dafgda ( ctx, handle, addr, addr + psiz - 1, buffer )

            record(2:2+psiz-1) = buffer(1:psiz)
            record(10:10+psiz-1) = buffer(1:psiz)

            record ( 17 ) = sclkdp

            found = .true.

         end if

         return

      else

!
!        The bracketing times are contained in this group.
!
         lsclk = buffer ( i     )
         rsclk = buffer ( i + 1 )

         laddr = beg  +  ( skip + i - 1 ) * psiz

         raddr = laddr + psiz

      end if

!
!     At this point we have the two times in the segment that bracket
!     the request time.  We also have the addresses of the pointing
!     data associated with those times. The task now is to determine
!     if the bracketing times fall in the same interval.  If so then
!     we can interpolate between them.  If they don't then return
!     pointing for whichever of the two times is closest to the
!     request time, provided that it is within the tolerance.
!

!
!     Find the interpolation interval that the request time is in and
!     determine if the bracketing SCLK's are both in it.
!
!     First check if the request time falls in the same interval as
!     it did last time.  We need to make sure that we are dealing
!     with the same segment as well as the same time range.
!
!     PREVS      is the start time of the interval that satisfied
!                the previous request for pointing.
!
!     PREVN      is the start time of the interval that followed
!                the interval specified above.
!
!     LHAND      is the handle of the file that PREVS and PREVN
!                were found in.
!
!     LBEG,      are the beginning and ending addresses of the
!     LEND       segment in the file LHAND that PREVS and PREVN
!                were found in.
!
      if (  ( handle == lhand ) .and. &
            ( beg    == lbeg  ) .and. &
            ( end    == lend  ) .and. &
            ( sclkdp >= prevs ) .and. &
            ( sclkdp < prevn )      ) then

         start  = prevs
         nstart = prevn

      else

!
!        The START times of all of the intervals are stored in the
!        segment and a directory of every hundredth START is also
!        stored. The procedure to find the bracketing interval start
!        times is identical to the one used above for finding the
!        bracketing times.
!
!        The directory epochs narrow down the search for the times that
!        bracket the request time to a group of DIRSIZ or fewer records.
!

!
!        There is only one group if there are no directory epochs.
!
         if ( nidir == 0 ) then

            group = 1

         else
!
!           Compute the location of the first directory epoch.  From the
!           beginning of the segment, we need to go through all of the
!           pointing numbers (PSIZ*NUMREC of them), then through all of
!           the NUMREC SCLK times and NRDIR directory times, and then
!           finally through the NUMINT interval start times.
!
            dirloc = beg + ( psiz + 1 ) * numrec  +  nrdir  +  numint

!
!           Locate the largest directory time less than the
!           request time SCLKDP.
!
!           Read in as many as BUFSIZ directory epochs at a time for
!           comparison.
!
            fnd    = .false.
            remain =  nidir
            group  =  0

            do while ( .not. fnd )
!
!              The number of records to read into the buffer.
!
               n = min( remain, bufsiz )

               call dafgda ( ctx, handle, dirloc, dirloc + n - 1, buffer )

               remain = remain - n
!
!              Determine the last directory element in BUFFER that's
!              less than SCLKDP.
!
               i = lstltd ( sclkdp, n, buffer )

               if ( i < n ) then

                  group =  group + i + 1
                  fnd   = .true.

               else if ( remain == 0 ) then
!
!                 The request time is greater than the last directory
!                 time so we want the last group in the segment.
!
                  group =  nidir + 1
                  fnd   = .true.

               else
!
!                 Need to read another block of directory times.
!
                  dirloc = dirloc + n
                  group  = group  + n

               end if

            end do

         end if

!
!        Now we know which group of DIRSIZ (or less) times to look at.
!        Out of the NUMINT SCLK START times, the number that we should
!        skip over to get to the proper group is DIRSIZ * ( GROUP - 1 ).
!
         skip = dirsiz * ( group - 1 )

!
!        From this we can compute the address in the segment of the
!        group of times we want.  To get to the first interval start
!        time we must pass over PSIZ * NUMREC pointing numbers, NUMREC
!        SCLK times, and NRDIR SCLK directory times.  Then we skip
!        over the number just computed above.
!

         grpadd = beg  +  ( psiz + 1 ) * numrec  +  nrdir  +  skip

!
!        The number of times that we have to look at may be less than
!        DIRSIZ.  However many there are, go ahead and read them into
!        the buffer.
!
         n = min ( dirsiz, numint - skip )

         call dafgda ( ctx, handle, grpadd, grpadd + n - 1, buffer )
!
!        Find the index of the largest time in the group that is less
!        than or equal to the input time.
!
         i = lstled ( sclkdp, n, buffer )

         if ( i == 0 ) then
!
!           The first start time in the buffer is the start of the
!           interval following the one containing the request time.
!
!           We don't need to check if GROUP = 1 because the case of
!           the request time occurring before the first time in the
!           segment has already been handled.
!
            nstart = buffer (1)

            addr = grpadd - 1

            call dafgda ( ctx, handle, addr, addr, start_1 )
            start = start_1(1)

         else  if ( i == n ) then

            if ( group == ( nidir + 1 ) ) then
!
!              This is the last interval in the segment.
!
               start = buffer ( n )

               nstart = dpmax

            else
!
!              The last START time in this group is equal to the
!              request time.
!
               start = buffer ( n )

               addr = grpadd + n

               call dafgda ( ctx, handle, addr, addr, nstart_1 )
               nstart = nstart_1(1)

            end if

         else
!
!           The bracketing START times are contained in this group.
!
            start  = buffer ( i     )
            nstart = buffer ( i + 1 )

         end if

!
!        Save the information about the interval and segment.
!
         lhand = handle
         lbeg  = beg
         lend  = end
         prevs = start
         prevn = nstart

      end if

!
!     Check and see if the bracketing pointing instances belong
!     to the same interval.  If they do then we can interpolate
!     between them, if not then check to see if the closer of
!     the two to the request time lies within the tolerance.
!
!     The left bracketing time will always belong to the same
!     interval as the request time, therefore we need to check
!     only that the right bracketing time is less than the start
!     time of the next interval.
!

      if ( rsclk < nstart ) then

         record ( 1 ) = lsclk

         call dafgda ( ctx, handle, laddr, laddr + psiz - 1, record ( 2 ) )

         record ( 9 ) = rsclk

         call dafgda ( ctx, handle, raddr, raddr + psiz - 1, record ( 10 ) )

         record ( 17 ) = sclkdp

         found = .true.

      else

         ldiff = sclkdp - lsclk
         rdiff = rsclk  - sclkdp

         if ( ( ldiff <= tol ) .or. ( rdiff <= tol ) ) then
!
!           Return the pointing instance closest to the request time.
!
!           If the request time is midway between LSCLK and RSCLK then
!           grab the pointing instance associated with the greater time.
!
            if ( ldiff < rdiff ) then

               record ( 1 ) = lsclk
               record ( 9 ) = lsclk

               call dafgda ( ctx, handle, laddr, laddr+psiz-1, buffer )

               record(2:2+psiz-1) = buffer(1:psiz)
               record(10:10+psiz-1) = buffer(1:psiz)

            else

               record ( 1 ) = rsclk
               record ( 9 ) = rsclk

               call dafgda ( ctx, handle, raddr, raddr+psiz-1, buffer )

               record(2:2+psiz-1) = buffer(1:psiz)
               record(10:10+psiz-1) = buffer(1:psiz)

            end if

            record ( 17 ) = sclkdp

            found = .true.

         end if

      end if

      return
      end associate
      end subroutine ckr03

      !> Read a single CK data record from a segment of type 05
      !> (MEX/Rosetta Attitude file interpolation).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a CK segment of type 05.
      !>
      !>     SCLKDP   is an encoded spacecraft clock time indicating
      !>              the epoch for which pointing is desired.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>              When SCLKDP falls within the bounds of one of the
      !>              interpolation intervals then the tolerance has no
      !>              effect because pointing will be returned at the
      !>              request time.
      !>
      !>              However, if the request time is not in one of the
      !>              intervals, then the tolerance is used to determine
      !>              if pointing at one of the interval endpoints should
      !>              be returned.
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a set of data from the specified segment which,
      !>              when evaluated at epoch SCLKDP, will give the
      !>              attitude and angular velocity of some body, relative
      !>              to the reference frame indicated by DESCR.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | evaluation epoch     |
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | nominal SCLK rate    |
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>              The packet size is a function of the subtype code.
      !>              All packets in a record have the same size.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     This routine follows the pattern established in the lower-numbered
      !>     CK data type readers of not explicitly performing error
      !>     diagnoses. Exceptions are listed below nonetheless.
      !>
      !>     1)  If the input HANDLE does not designate a loaded CK file, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the segment specified by DESCR is not of data type 05,
      !>         the error SPICE(WRONGCKTYPE) is signaled.
      !>
      !>     3)  If the input SCLK value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>     4)  If the window size is non-positive or greater than the
      !>         maximum allowed value, the error SPICE(INVALIDVALUE) is
      !>         signaled.
      !>
      !>     5)  If the window size is not compatible with the segment
      !>         subtype, the error SPICE(INVALIDVALUE) is signaled.
      !>
      !>     6)  If the segment subtype is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     7)  If the tolerance is negative, the error SPICE(VALUEOUTOFRANGE)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the CK Required Reading file for a description of the
      !>     structure of a data type 05 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the CKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the CKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
      !>
      !>     The search performed here does not mimic the behavior of the CK
      !>     reader APIs CKGP and CKGPAV, which continue searching when an
      !>     applicable segment doesn't satisfy a pointing request. See the CK
      !>     Required reading for details.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV )
      !>           CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   )
      !>
      !>           IF ( .NOT. SFND ) THEN
      !>              [Handle case of pointing not being found]
      !>           END IF
      !>
      !>     C
      !>     C     Look at parts of the descriptor.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 05 ) THEN
      !>
      !>              CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                RECORD, FOUND                       )
      !>
      !>              IF ( .NOT. FOUND ) THEN
      !>                 [Handle case of pointing not being found]
      !>              END IF
      !>
      !>              [Look at the RECORD data]
      !>                  .
      !>                  .
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  Correctness of inputs must be ensured by the caller of
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 06-JUL-2021 (NJB) (JDR)
      !>
      !>        Corrected code example: removed comment character preceding
      !>        CKBSS call. Added note regarding difference between this
      !>        search and those performed by the CK reader APIs CKGP and
      !>        CKGPAV.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 27-JAN-2014 (NJB)
      !>
      !>        Increased MAXDEG to 23 for compatibility with CK type 6.
      !>
      !>-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
      module subroutine ckr05 ( ctx, handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_arrays, only: lstled, lstltd
      use pepper_math, only: odd
      use pepper_inc_ck05
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Pointing request time.
      real(wp), intent(in) :: tol  !! Lookup tolerance.
      logical, intent(in) :: needav  !! Angular velocity flag.
      real(wp), intent(out) :: record   ( * )  !! Data record.
      logical, intent(out) :: found  !! Flag indicating whether record was found.
      integer(ip), parameter :: pbufsz = dirsiz + 1
      integer(ip), parameter :: ctrlsz = 5
      integer(ip), parameter :: parsiz = 4
      integer(ip), parameter :: sbufsz = dirsiz + 3
      integer(ip), parameter :: maxdeg = 23
      real(wp) :: contrl ( ctrlsz )
      real(wp) :: dc     ( nd )
      real(wp) :: hepoch
      real(wp) :: lepoch
      real(wp) :: nstart
      real(wp) :: nnstrt
      real(wp) :: pbuffr ( pbufsz )
      real(wp) :: rate
      real(wp) :: sbuffr ( sbufsz )
      real(wp) :: start
      real(wp) :: t
      integer(ip) :: pbegix
      integer(ip) :: begin
      integer(ip) :: bufbas
      integer(ip) :: dirbas
      integer(ip) :: end
      integer(ip) :: pendix
      integer(ip) :: first
      integer(ip) :: high
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: j
      integer(ip) :: last
      integer(ip) :: lsize
      integer(ip) :: low
      integer(ip) :: maxwnd
      integer(ip) :: n
      integer(ip) :: nidir
      integer(ip) :: npdir
      integer(ip) :: nints
      integer(ip) :: npread
      integer(ip) :: nsrch
      integer(ip) :: nsread
      integer(ip) :: packsz
      integer(ip) :: pgroup
      integer(ip) :: remain
      integer(ip) :: rsize
      integer(ip) :: sbegix
      integer(ip) :: sendix
      integer(ip) :: sgroup
      integer(ip) :: subtyp
      integer(ip) :: timbas
      integer(ip) :: type
      integer(ip) :: wndsiz
      integer(ip) :: wstart
      if ( .not. allocated ( ctx%ckr05_state ) ) allocate ( ctx%ckr05_state )
      associate ( prevs => ctx%ckr05_state%prevs, &
                  prevn => ctx%ckr05_state%prevn, &
                  prevnn => ctx%ckr05_state%prevnn, &
                  lhand => ctx%ckr05_state%lhand, &
                  lbeg => ctx%ckr05_state%lbeg, &
                  lend => ctx%ckr05_state%lend )
      if ( ctx%failed() ) return

!
!     No pointing found so far.
!
      found = .false.

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      type  = ic( 3 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 05 data segment.
!
      if ( type /= 5 ) then
         call ctx%setmsg( 'You are attempting to locate type * ' // &
                      'data in a type 5 data segment.'       )
         call ctx%errint( '*',  type                             )
         call ctx%sigerr( 'SPICE(WRONGCKTYPE)'                   )
         return
      end if

!
!     Check the tolerance value.
!
      if ( tol < 0.0_wp ) then
         call ctx%setmsg( 'Tolerance must be non-negative but '  // &
                      'was actually *.'                      )
         call ctx%errdp ( '*',  tol                              )
         call ctx%sigerr( 'SPICE(VALUEOUTOFRANGE)'               )
         return
      end if

!
!     Check the request time and tolerance against the bounds in
!     the segment descriptor.
!
      if (       ( (sclkdp + tol) < dc(1) ) &
           .or.  ( (sclkdp - tol) > dc(2) )  )  then
!
!        The request time is too far outside the segment's coverage
!        interval for any pointing to satisfy the request.
!
         return

      end if

!
!     Set the request time to use for searching.
!
      t = max(min(dc(1), dc(2)), min(max(dc(1), dc(2)), sclkdp))

!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        2)  The epochs in the segment are in strictly increasing
!            order.
!
!        3)  The interpolation interval start times in the segment are
!            in strictly increasing order.
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than MAXDEG.
!
      call dafgda ( ctx, handle, end-ctrlsz+1, end, contrl )

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT.  We
!     do this only after the first call to DAFGDA, as in CKR03.
!
      if ( ctx%failed() ) return

      rate    =         contrl(1)
      subtyp  =  nint ( contrl(2) )
      wndsiz  =  nint ( contrl(3) )
      nints   =  nint ( contrl(4) )
      n       =  nint ( contrl(5) )

!
!     Set the packet size, which is a function of the subtype.
!
      if ( subtyp == c05tp0 ) then

         packsz = c05ps0

      else if ( subtyp == c05tp1 ) then

         packsz = c05ps1

      else if ( subtyp == c05tp2 ) then

         packsz = c05ps2

      else if ( subtyp == c05tp3 ) then

         packsz = c05ps3

      else

         call ctx%setmsg ( 'Unexpected CK type 5 subtype # found in ' // &
                       'type 5 segment.'                          )
         call ctx%errint ( '#',  subtyp                               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

!
!     Check the window size.
!
      if ( wndsiz <= 0 ) then

         call ctx%setmsg ( 'Window size in type 05 segment was #; must ' // &
                       'be positive.'                                )
         call ctx%errint ( '#',  wndsiz                                  )
         call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                         )
         return

      end if

      if (      ( subtyp == c05tp0 ) &
           .or. ( subtyp == c05tp2 )  ) then
!
!        These are the Hermite subtypes.
!
         maxwnd  =  (maxdeg+1)/2

         if ( wndsiz > maxwnd ) then

            call ctx%setmsg ( 'Window size in type 05 segment was #; max '// &
                          'allowed value is # for subtypes 0 and 2 '  // &
                          '(Hermite, 8 or 14-element packets).'       )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%errint ( '#',  maxwnd                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

         if ( odd(wndsiz) ) then

            call ctx%setmsg ( 'Window size in type 05 segment was #; '    // &
                          'must be even for subtypes 0 and 2 '        // &
                          '(Hermite, 8 or 14-element packets).'       )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

      else if (      ( subtyp == c05tp1 ) &
                .or. ( subtyp == c05tp3 )  ) then
!
!        These are the Lagrange subtypes.
!
         maxwnd  =  maxdeg + 1

         if ( wndsiz > maxwnd ) then

            call ctx%setmsg ( 'Window size in type 05 segment was #; max '// &
                          'allowed value is # for subtypes 1 and 3 '  // &
                          '(Lagrange, 4 or 7-element packets).'       )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%errint ( '#',  maxwnd                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

         if ( odd(wndsiz) ) then

            call ctx%setmsg ( 'Window size in type 05 segment was #; '    // &
                          'must be even for subtypes 1 and 3 '        // &
                          '(Lagrange, 4 or 7-element packets).'       )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

      else

         call ctx%setmsg ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle the new subtype #'        )
         call ctx%errint ( '#', subtyp                                  )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         return

      end if

!
!     We now need to select the pointing values to interpolate
!     in order to satisfy the pointing request.  The first step
!     is to use the pointing directories (if any) to locate a set of
!     epochs bracketing the request time.  Note that the request
!     time might not be bracketed:  it could precede the first
!     epoch or follow the last epoch.
!
!     We'll use the variable PGROUP to refer to the set of epochs
!     to search.  The first group consists of the epochs prior to
!     and including the first pointing directory entry.  The last
!     group consists of the epochs following the last pointing
!     directory entry.  Other groups consist of epochs following
!     one pointing directory entry up to and including the next
!     pointing directory entry.
!

      npdir   =  (n-1) / dirsiz
      dirbas  =  begin +  n*packsz  +  n  -  1

      if ( npdir == 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         pgroup  = 1

      else
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  dirbas
         npread  =  min ( npdir, dirsiz )

         call dafgda ( ctx, handle, bufbas+1, bufbas+npread, pbuffr )

         remain  =  npdir - npread

         do while (       ( pbuffr(npread) < t ) &
                    .and. ( remain         > 0 )   )

            bufbas  =  bufbas + npread
            npread  =  min ( remain, dirsiz )
!
!           Note:  NPREAD is always > 0 here.
!
            call dafgda ( ctx, handle, bufbas+1, bufbas+npread, pbuffr )

            remain  =  remain - npread

         end do

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in PBUFFR(1).
!
!        PGROUP is one more than the number of directories we've
!        passed by.
!
         pgroup  =     ( bufbas - dirbas ) &
                    +   lstltd ( t, npread, pbuffr ) &
                    +   1

      end if

!
!     PGROUP now indicates the set of epochs in which to search for the
!     request epoch.  The following cases can occur:
!
!        PGROUP = 1
!        ==========
!
!           NPDIR = 0
!           --------
!           The request time may precede the first time tag
!           of the segment, exceed the last time tag, or lie
!           in the closed interval bounded by these time tags.
!
!           NPDIR >= 1
!           ---------
!           The request time may precede the first time tag
!           of the group but does not exceed the last epoch
!           of the group.
!
!        1 < PGROUP <= NPDIR
!        ===================
!
!           The request time follows the last time of the
!           previous group and is less than or equal to
!           the pointing directory entry at index PGROUP.
!
!        1 < PGROUP = NPDIR + 1
!        ======================
!
!           The request time follows the last time of the
!           last pointing directory entry.  The request time
!           may exceed the last time tag.
!
!     Now we'll look up the time tags in the group of epochs
!     we've identified.
!
!     We'll use the variable names PBEGIX and PENDIX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( pgroup == 1 ) then

         pbegix  =  1
         pendix  =  min ( n, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is strictly bracketed on the
!        low side by the time tag set we look up.
!
         pbegix  =      (  pgroup  - 1 ) * dirsiz
         pendix  =  min (  pbegix + dirsiz,  n  )

      end if

      timbas  =  dirbas - n

      call dafgda ( ctx, handle, timbas+pbegix, timbas+pendix, pbuffr )

      npread  =  pendix - pbegix + 1

!
!     At this point, we'll deal with the cases where T lies outside
!     of the range of epochs we've buffered.
!
      if ( t < pbuffr(1) ) then
!
!        This can happen only if PGROUP = 1 and T precedes all epochs.
!        If the input request time is too far from PBUFFR(1) on
!        the low side, we're done.
!
         if ( sclkdp + tol < pbuffr(1) ) return

!
!        Bracket T to move it within the range of buffered epochs.
!
         t = pbuffr(1)

      else if ( t > pbuffr(npread) ) then
!
!        This can happen only if T follows all epochs.
!
         if ( sclkdp - tol > pbuffr(npread) ) return

!
!        Bracket T to move it within the range of buffered epochs.
!
         t = pbuffr(npread)

      end if

!
!     At this point,
!
!        | T - SCLKDP |  <=  TOL
!
!     Also, one of the following is true:
!
!        T is the first time of the segment
!
!        T is the last time of the segment
!
!        T equals SCLKDP
!
!     Find two adjacent time tags bounding the request epoch.  The
!     request time cannot be greater than all of time tags in the
!     group, and it cannot precede the first element of the group.
!
      i  =  lstltd (  t,  npread,  pbuffr  )

!
!     The variables LOW and HIGH are the indices of a pair of time
!     tags that bracket the request time.  Remember that NPREAD could
!     be equal to 1, in which case we would have LOW = HIGH.
!
      if ( i == 0 ) then
!
!        This can happen only if PGROUP = 1 and T = PBUFFR(1).
!
         low    =  1
         lepoch =  pbuffr(1)

         if ( n == 1 ) then
            high = 1
         else
            high = 2
         end if

         hepoch = pbuffr(high)

      else

         low    =  pbegix + i - 1
         lepoch =  pbuffr(i)

         high   =  low + 1
         hepoch =  pbuffr(i+1)

      end if

!
!     We now need to find the interpolation interval containing
!     T, if any.  We may be able to use the interpolation
!     interval found on the previous call to this routine.  If
!     this is the first call or if the previous interval is not
!     applicable, we'll search for the interval.
!
!     First check if the request time falls in the same interval as
!     it did last time.  We need to make sure that we are dealing
!     with the same segment as well as the same time range.
!
!        PREVS      is the start time of the interval that satisfied
!                   the previous request for pointing.
!
!        PREVN      is the start time of the interval that followed
!                   the interval specified above.
!
!        PREVNN     is the start time of the interval that followed
!                   the interval starting at PREVN.
!
!        LHAND      is the handle of the file that PREVS and PREVN
!                   were found in.
!
!        LBEG,      are the beginning and ending addresses of the
!        LEND       segment in the file LHAND that PREVS and PREVN
!                   were found in.
!
      if (  ( handle == lhand ) .and. &
            ( begin  == lbeg  ) .and. &
            ( end    == lend  ) .and. &
            ( t      >= prevs ) .and. &
            ( t      < prevn )      ) then

         start  = prevs
         nstart = prevn
         nnstrt = prevnn

      else
!
!        Search for the interpolation interval.
!
         nidir   =  (nints-1) / dirsiz
         dirbas  =   end - ctrlsz - nidir

         if ( nidir == 0 ) then
!
!           There's no mystery about which group of epochs to search.
!
            sgroup  = 1

         else
!
!           There's at least one directory.  Find the first directory
!           whose time is greater than or equal to the request time, if
!           there is such a directory.  We'll search linearly through
!           the directory entries, reading up to DIRSIZ of them at a
!           time. Having found the correct set of directory entries,
!           we'll perform a binary search within that set for the
!           desired entry.
!
            bufbas  =  dirbas
            nsread  =  min ( nidir, dirsiz )
            remain  =  nidir - nsread

            call dafgda ( ctx, handle, bufbas+1, bufbas+nsread, sbuffr )

            do while (       ( sbuffr(nsread) < t ) &
                       .and. ( remain         > 0 )   )

               bufbas  =  bufbas + nsread
               nsread  =  min ( remain, dirsiz )
               remain  =  remain - nsread
!
!              Note:  NSREAD is always > 0 here.
!
               call dafgda ( ctx, handle, bufbas+1, bufbas+nsread, sbuffr )

            end do

!
!           At this point, BUFBAS - DIRBAS is the number of directory
!           entries preceding the one contained in SBUFFR(1).
!
!           SGROUP is one more than the number of directories we've
!           passed by.
!
            sgroup  =     ( bufbas - dirbas ) &
                       +   lstltd ( t, nsread, sbuffr ) &
                       +   1

         end if

!
!        SGROUP now indicates the set of interval start times in which
!        to search for the request epoch.
!
!        Now we'll look up the time tags in the group of epochs we've
!        identified.
!
!        We'll use the variable names SBEGIX and SENDIX to refer to the
!        indices, relative to the set of start times, of the first and
!        last start times in the set we're going to look up.
!
         if ( sgroup == 1 ) then

            sbegix  =  1
            sendix  =  min ( nints, dirsiz + 2 )

         else
!
!           Look up the start times for the group of interest. Also
!           buffer last start time from the previous group. Also, it
!           turns out to be useful to pick up two extra start
!           times---the first two start times of the next group---if
!           they exist.
!
            sbegix  =      (  sgroup  - 1 ) * dirsiz

            sendix  =  min (  sbegix + dirsiz + 2,  nints  )

         end if

         timbas  =  dirbas - nints

         call dafgda ( ctx, handle, timbas+sbegix, timbas+sendix, sbuffr )

         nsread  =  sendix - sbegix + 1

!
!        Find the last interval start time less than or equal to the
!        request time.  We know T is greater than or equal to the
!        first start time, so I will be > 0.
!
         nsrch  =  min ( dirsiz + 1, nsread )

         i      =  lstled (  t,  nsrch, sbuffr  )

         start  =  sbuffr(i)

!
!        Let NSTART ("next start") be the start time that follows
!        START, if START is not the last start time.  If NSTART
!        has a successor, let NNSTRT be that start time.
!
         if ( i < nsread ) then

            nstart = sbuffr(i+1)

            if ( i+1 < nsread ) then

               nnstrt = sbuffr(i+2)
            else
               nnstrt = dpmax
            end if

         else

            nstart = dpmax
            nnstrt = dpmax

         end if

      end if

!
!     If T does not lie within the interpolation interval starting
!     at time START, we'll determine whether T is closer to this
!     interval or the next.  If the distance between T and the
!     closer interval is less than or equal to TOL, we'll map T
!     to the closer endpoint of the closer interval.  Otherwise,
!     we return without finding pointing.
!
      if ( hepoch == nstart ) then
!
!        The first time tag greater than or equal to T is the start
!        time of the next interpolation interval.
!
!        The request time lies between interpolation intervals.
!        LEPOCH is the last time tag of the first interval; HEPOCH
!        is the first time tag of the next interval.
!
         if (  abs( t - lepoch ) <= abs( hepoch - t )  ) then
!
!           T is closer to the first interval...
!
            if (  abs( t - lepoch )  >  tol  ) then
!
!              ...But T is too far from the interval.
!
               return

            end if

!
!           Map T to the right endpoint of the preceding interval.
!
            t      = lepoch
            high   = low
            hepoch = lepoch

         else
!
!           T is closer to the second interval...
!
            if (  abs( hepoch - t )  >  tol  ) then
!
!              ...But T is too far from the interval.
!
               return

            end if

!
!           Map T to the left endpoint of the next interval.
!
            t      = hepoch
            low    = high
            lepoch = hepoch

!
!           Since we're going to be picking time tags from the next
!           interval, we'll need to adjust START and NSTART.
!
            start  = nstart
            nstart = nnstrt

         end if

      end if

!
!     We now have
!
!        LEPOCH < T <  HEPOCH
!                -   -
!
!     where LEPOCH and HEPOCH are the time tags at indices
!     LOW and HIGH, respectively.
!
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike CK types 8, 9, 12, and 13, for type 05 we adjust
!     the window size to keep the request time within the central
!     interval of the window.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the request
!     time is too close to one end of the interpolation interval, we
!     reduce the window size, after which one endpoint of the window
!     will coincide with an endpoint of the interpolation interval.
!
!     We start out by looking up the set of time tags we'd use
!     if there were no gaps in the coverage.  We then trim our
!     time tag set to ensure all tags are in the interpolation
!     interval.  It's possible that the interpolation window will
!     collapse to a single point as a result of this last step.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size of the set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
!     available, the set ranges from index 1 to index LOW.
!
      lsize =  min ( wndsiz/2, low )

!
!     RSIZE is defined analogously for the right half of the window.
!
      rsize =  min (  wndsiz/2,  ( n - high + 1 )  )

!
!     The window size is simply the sum of LSIZE and RSIZE.
!
      wndsiz =  lsize + rsize

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      first =  low    -  lsize   +  1

      last  =  first  +  wndsiz  -  1

!
!     Buffer the epochs.
!
      wstart  =  begin  +  n * packsz  +  first  -  1

      call dafgda ( ctx,  handle, wstart, wstart+wndsiz-1, pbuffr )

!
!     Discard any epochs less than START or greater than or equal
!     to NSTART.  The set of epochs we want ranges from indices
!     I+1 to J.  This range is non-empty unless START and NSTART
!     are both DPMAX().
!
      i = lstltd ( start,  wndsiz, pbuffr )
      j = lstltd ( nstart, wndsiz, pbuffr )

      if ( i == j ) then
!
!        Fuggedaboudit.
!
         return

      end if

!
!     Update FIRST, LAST, and WNDSIZ.
!
      wndsiz = j     - i
      first  = first + i
      last   = first + wndsiz - 1

!
!     Put the subtype into the output record.  The size of the group
!     of packets is derived from the subtype, so we need not include
!     the size.
!
      record(1) =  t
      record(2) =  subtyp
      record(3) =  wndsiz
      record(4) =  rate

!
!     Read the packets.
!
      call dafgda ( ctx, handle, &
                    begin + (first-1)*packsz, &
                    begin +      last*packsz  -  1, &
                    record(parsiz+1)                )

!
!     Finally, add the epochs to the output record.
!
      record(parsiz + 1 + wndsiz*packsz:parsiz + 1 + wndsiz*packsz+j-i-1) = pbuffr(i+1:i+1+j-i-1)

!
!     Save the information about the interval and segment.
!
      lhand  = handle
      lbeg   = begin
      lend   = end
      prevs  = start
      prevn  = nstart
      prevnn = nnstrt

!
!     Indicate pointing was found.
!
      found = .true.

      return
      end associate
      end subroutine ckr05

      !> Read a single CK data record from a segment of type 6
      !> (ESOC/DDID Piecewise Interpolation).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for a CK
      !>              segment of type 6.
      !>
      !>     SCLKDP   is an encoded spacecraft clock time indicating the
      !>              epoch for which pointing is desired.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>              When SCLKDP falls between the start time of one of
      !>              the mini-segment intervals and the last time tag of
      !>              that interval, the tolerance has no effect because
      !>              pointing will be returned at the request time.
      !>
      !>              However, if the request time falls within a coverage
      !>              gap in one of the intervals, or outside of any
      !>              interval, then the tolerance is used to determine if
      !>              pointing should be returned at the closest epoch for
      !>              which pointing is available. This epoch is either an
      !>              interval's start time or the smaller of an interval's
      !>              end time and its last time tag.
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested. If the
      !>              input segment descriptor indicates angular velocity
      !>              is absent, the error SPICE(NOAVDATA) is signaled.
      !>
      !>              Note: although all subtypes of type 6 records either
      !>              contain or compute angular velocity, a CK creator may
      !>              choose to indicate that the provided angular velocity
      !>              data are not valid; this can be done by setting the
      !>              segment descriptor angular velocity flag to .FALSE.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a set of data from the specified segment which,
      !>              when evaluated at epoch SCLKDP, will give the
      !>              attitude and angular velocity of some body, relative
      !>              to the reference frame indicated by DESCR.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | evaluation epoch     |
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | nominal SCLK rate    |
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>              The packet size is a function of the subtype code.
      !>              All packets in a record have the same size.
      !>
      !>     FOUND    is a logical flag indicating whether data were found.
      !>              If NEEDAV is .FALSE., data will be found if the
      !>              request time is within TOL ticks of a time for which
      !>              the segment provides data. If NEEDAV is .TRUE., the
      !>              segment's angular velocity flag must also be set in
      !>              order for data to be found.
      !>
      !>              A type 6 segment provides data for times that are
      !>              between its descriptor time bounds and that are
      !>              within the coverage region of a mini-segment
      !>              interval. The coverage region of a mini-segment
      !>              interval extends from its start time to the lesser of
      !>              its stop time and its last time tag.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     This routine roughly follows the pattern established in the
      !>     lower-numbered CK data type readers of not explicitly performing
      !>     error diagnoses. In particular, the C-kernel from which data are
      !>     read is assumed to be valid in most respects. The few exceptions
      !>     that are handled here are listed below.
      !>
      !>     1)  If the input HANDLE does not designate a loaded CK file, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the segment specified by DESCR is not of data type 6, the
      !>         error SPICE(WRONGCKTYPE) is signaled.
      !>
      !>     3)  If the input SCLK value is not within TOL ticks of a time
      !>         for which the segment provides data, FOUND is set to .FALSE.
      !>         and the output record is undefined.
      !>
      !>     4)  If the window size is non-positive or greater than the
      !>         maximum allowed value, the error SPICE(INVALIDVALUE) is
      !>         signaled.
      !>
      !>     5)  If the window size is not compatible with the segment
      !>         subtype, the error SPICE(INVALIDVALUE) is signaled.
      !>
      !>     6)  If the segment subtype is not recognized, the error
      !>         SPICE(INVALIDSUBTYPE) is signaled.
      !>
      !>     7)  If the tolerance is negative, the error SPICE(NEGATIVETOL) is
      !>         signaled.
      !>
      !>     8)  If an error occurs while trying to read data from a C-kernel,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     9)  If the input segment descriptor indicates that angular
      !>         velocity data are not present, and if the input flag NEEDAV
      !>         is set to .TRUE., the error SPICE(NOAVDATA) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the CK Required Reading file for a description of the
      !>     structure of a data type 6 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the CKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the CKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
      !>
      !>     The search performed here does not mimic the behavior of the CK
      !>     reader APIs CKGP and CKGPAV, which continue searching when an
      !>     applicable segment doesn't satisfy a pointing request. See the CK
      !>     Required reading for details.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV )
      !>           CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   )
      !>
      !>           IF ( .NOT. SFND ) THEN
      !>              [Handle case of pointing not being found]
      !>           END IF
      !>
      !>     C
      !>     C     Look at parts of the descriptor.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 6 ) THEN
      !>
      !>              CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                RECORD, FOUND                       )
      !>
      !>              IF ( .NOT. FOUND ) THEN
      !>                 [Handle case of pointing not being found]
      !>              END IF
      !>
      !>              [Look at the RECORD data]
      !>                  .
      !>                  .
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  Correctness of the C-kernel read by this routine is
      !>         assumed.
      !>
      !>     2)  Correctness of inputs must be ensured by the caller of
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 06-JUL-2021 (NJB) (JDR)
      !>
      !>        Corrected code example: removed comment character preceding
      !>        CKBSS call. Added note regarding difference between this
      !>        search and those performed by the CK reader APIs CKGP and
      !>        CKGPAV.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
      module subroutine ckr06 ( ctx, handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstled, lstltd
      use pepper_math, only: odd
      use pepper_inc_ckparam
      use pepper_inc_ck06
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Pointing request time.
      real(wp), intent(in) :: tol  !! Lookup tolerance.
      logical, intent(in) :: needav  !! Angular velocity flag.
      real(wp), intent(out) :: record   ( * )  !! Data record.
      logical, intent(out) :: found  !! Flag indicating whether record was found.
      real(wp) :: lstepc_1(1)  !! one-element copy of lstepc (array argument)
      integer(ip), parameter :: nsgpar = 2
      integer(ip), parameter :: c06mw0 = ( maxdeg + 1 )/2
      integer(ip), parameter :: c06mw1 = ( maxdeg + 1 )
      integer(ip), parameter :: c06mw2 = ( maxdeg + 1 )/2
      integer(ip), parameter :: c06mw3 = ( maxdeg + 1 )
      real(wp) :: buffer ( bufsiz )
      real(wp) :: contrl ( ctrlsz )
      real(wp) :: dc     ( nd )
      real(wp) :: gap
      real(wp) :: lstepc
      real(wp) :: mintim ( 2 )
      real(wp) :: rate
      real(wp) :: t
      integer(ip) :: baddr
      integer(ip) :: begidx
      integer(ip) :: bufbas
      integer(ip) :: dirbas
      integer(ip) :: eaddr
      integer(ip) :: endidx
      integer(ip) :: first
      integer(ip) :: group
      integer(ip) :: high
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: isel
      integer(ip) :: ivbas
      integer(ip) :: ivbix
      integer(ip) :: iveix
      integer(ip) :: last
      integer(ip) :: low
      integer(ip) :: lsize
      integer(ip) :: maxwnd
      integer(ip) :: minib
      integer(ip) :: minie
      integer(ip) :: miniix
      integer(ip), parameter :: mxwnsz ( 0 : c06nst-1 ) = &
         [ c06mw0, c06mw1, c06mw2, c06mw3 ]
      integer(ip) :: n
      integer(ip) :: ndir
      integer(ip) :: npkdir
      integer(ip) :: npkt
      integer(ip) :: nrcpkt
      integer(ip) :: nread
      integer(ip) :: pkdbas
      integer(ip) :: pktsiz
      integer(ip), parameter :: pktszs ( 0 : c06nst-1 ) = &
         [ c06ps0, c06ps1, c06ps2, c06ps3 ]
      integer(ip) :: remain
      integer(ip) :: rsize
      integer(ip) :: subtyp
      integer(ip) :: timbas
      integer(ip) :: type
      integer(ip) :: wndsiz
      logical :: avflag
      logical :: ivlsel
      logical :: lval
      logical :: prvfnd
      logical :: samivl
      logical :: samseg
      if ( .not. allocated ( ctx%ckr06_state ) ) allocate ( ctx%ckr06_state )
      associate ( pass1 => ctx%ckr06_state%pass1, &
                  svbeg => ctx%ckr06_state%svbeg, &
                  svbtim => ctx%ckr06_state%svbtim, &
                  svetim => ctx%ckr06_state%svetim, &
                  svfnd => ctx%ckr06_state%svfnd, &
                  svhan => ctx%ckr06_state%svhan, &
                  svlast => ctx%ckr06_state%svlast, &
                  svmiix => ctx%ckr06_state%svmiix, &
                  svminb => ctx%ckr06_state%svminb, &
                  svn => ctx%ckr06_state%svn, &
                  svnpkt => ctx%ckr06_state%svnpkt, &
                  svpkdb => ctx%ckr06_state%svpkdb, &
                  svpknd => ctx%ckr06_state%svpknd, &
                  svpksz => ctx%ckr06_state%svpksz, &
                  svrate => ctx%ckr06_state%svrate, &
                  svstyp => ctx%ckr06_state%svstyp, &
                  svwnsz => ctx%ckr06_state%svwnsz )
      if ( ctx%failed() ) return

!
!     Start with a parameter compatibility check on the first
!     pass.
!
      if ( pass1 ) then

         if ( ckmrsz < maxrsz ) then

            call ctx%setmsg ( 'CK type 6 record size may be as ' &
            //            'large as #, but CKPFS record size ' &
            //            '(defined in ckparam.inc) is #.'     )
            call ctx%errint ( '#',  maxrsz                         )
            call ctx%errint ( '#',  ckmrsz                         )
            call ctx%sigerr ( 'SPICE(BUG)'                         )

         end if
!
!        Indicate the first pass was completed.
!
         pass1 = .false.

      end if

!
!     No pointing found so far.
!
      found  = .false.

!
!     Let PRVFND indicate the last value of FOUND we returned. PRVFND
!     allows us to reset SVFND to .FALSE. at the start of this routine,
!     so we don't have to do this prior to every return (of which there
!     are more than 35).
!
      prvfnd = svfnd

!
!     Set the saved value of FOUND so as to reflect the value
!     of FOUND we'll return next.
!
      svfnd  = .false.

!
!     "Touch" the input argument NEEDAV to suppress compiler warnings.
!
      lval = (needav)
      lval = (lval)

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      type   = ic( 3 )
      avflag = ic( 4 ) == 1
      baddr  = ic( 5 )
      eaddr  = ic( 6 )

!
!     Check whether angular velocity data are requested but not
!     available.
!
      if (  needav  .and. ( .not. avflag ) ) then

         call ctx%setmsg ( 'Segment descriptor indicates angular velocity ' &
         //            'data are not available, but such data were ' &
         //            'requested.'                                    )
         call ctx%sigerr ( 'SPICE(NOAVDATA)'                               )
         return

      end if

!
!     Check the tolerance value.
!
      if ( tol < 0.0_wp ) then

         call ctx%setmsg( 'Tolerance must be non-negative but ' &
         //           'was actually *.'                      )
         call ctx%errdp ( '*',  tol                              )
         call ctx%sigerr( 'SPICE(NEGATIVETOL)'                   )
         return

      end if

!
!     Check the request time and tolerance against the bounds in
!     the segment descriptor.
!
      if (       ( (sclkdp + tol) < dc(1) ) &
           .or.  ( (sclkdp - tol) > dc(2) )  )  then
!
!        The request time is too far outside the segment's coverage
!        interval for any pointing to satisfy the request.
!
         return

      end if

!
!     Set the request time to use for searching.
!
      t = max(min(dc(1), dc(2)), min(max(dc(1), dc(2)), sclkdp))

!
!     From this point onward, we assume the segment was constructed
!     correctly.
!
!     Terminology: below, the phrase "base address of 'X'" refers to
!     the DAF address immediately preceding X. Base addresses simplify
!     mapping DAF array (here "array" means an array stored in
!     consecutive DAF addresses, not "segment") indices to DAF
!     addresses, since the DAF address of the Ith array element is
!     obtained by adding I to the DAF array's base address.
!
!     Key variables:
!
!        Name      Meaning
!        ----      -------
!        BADDR     Segment begin DAF address.
!
!        DIRBAS    Base address of mini-segment interval directory.
!
!        EADDR     Segment end DAF address.
!
!        FIRST     Index (mini-segment-relative) of first time tag in
!                  sequence transferred to the output record.
!
!        HIGH      Index (mini-segment-relative) of time tag following
!                  the tag at index LOW (see description below).
!
!        IVBIX     Index in the mini-segment interval bounds array of
!                  the start time of the applicable interval.
!
!        IVLBAS    Base address of mini-segment interval time bounds.
!
!        IVLSEL    Interval selection flag: this routine selects the
!                  last applicable interval if true; otherwise it
!                  selects the first applicable interval.
!
!        LAST      Index (mini-segment-relative) of last time tag in
!                  sequence transferred to the output record.
!
!        LOW       Index (mini-segment-relative) of last time tag less
!                  than the request time, or of the first time tag if
!                  this tag equals the request time.
!
!        MINIB,
!        MINIE     Mini-segment begin and end DAF addresses. These
!                  addresses are absolute, not segment-relative.
!
!        MINIIX    Mini-segment/mini-segment interval index.
!
!        N         Count of mini-segments.
!
!        NDIR      Number of mini-segment interval time bounds
!                  directories.
!
!        NPKDIR    Number of packet directory entries for current
!                  mini-segment.
!
!        NPKT      Packet count for current mini-segment.
!
!        NRCPKT    Output record packet count. Note that this count,
!                  due to reduction of order at mini-segment
!                  boundaries, may be smaller than the window size
!                  stored in the current mini-segment.
!
!        PKDBAS    Base address of packet directory for current
!                  mini-segment.
!
!        PKTSIZ    Size of packets of current mini-segment.
!
!        SUBTYP    Subtype of current mini-segment.
!
!        TIMBAS    Base address of time tags of current mini-segment.
!
!        WNDSIZ    Interpolation window size of current mini-segment.
!
!     Re-used variables: the variables shown in the list below
!     are used as short-duration variables, much like loop index
!     variables; they are re-used as needed.
!
!        BUFBAS
!        BUFFER
!        GROUP
!        NREAD
!        REMAIN
!
!     Decide whether we're looking at the same segment we saw on the
!     previous call, and whether the mini-segment interval used on
!     that call is still applicable.
!
!     Re-use of data from a previous call requires that the saved
!     data were set on a successful call. Note that PRVFND can not
!     be true on the first pass.
!
      samseg  =       ( handle ==  svhan ) &
                .and. ( baddr  ==  svbeg ) &
                .and.   prvfnd

!
!     Give SAMIVL an initial value. If we do have the
!     same interval, update SAMIVL to indicate this.
!
      samivl = .false.

      if ( samseg ) then
!
!        We now assume that all data saved from the last
!        read of this segment are valid.
!
         if ( svlast ) then
!
!           We pick the last interval containing T. For all intervals
!           but the last, T must be less than the interval end time.
!           For the last interval, T may equal the end time.
!
!           Note that we don't bother to test for the special case
!           where the interval is not the last, there's a gap at the
!           end of the interval and T equals the last epoch of the
!           interval. In this rare case, we do not reuse the old
!           interval data, even though it would be possible to
!           add code to do so.
!
            if ( svmiix < svn ) then

               samivl =       ( t >= svbtim ) &
                        .and. ( t < svetim )
            else

               samivl =         t >= svbtim &
                        .and. ( t <= svetim )
            end if

         else
!
!           We pick the first interval containing T. For all intervals
!           but the first, T must be greater than the interval start
!           time. For the first interval, T may equal the start time.
!
            if ( svmiix > 1 ) then

               samivl =       ( t  > svbtim ) &
                        .and. ( t  <= svetim )

            else

               samivl =       ( t  >= svbtim ) &
                        .and. ( t  <= svetim )
            end if

         end if

      end if

      if ( samseg .and. samivl ) then
!
!        We're looking at the same segment as last time, and the
!        mini-segment interval we looked up last time is applicable
!        for the input time T.
!
!        Simply restore the segment and interval parameters we
!        saved from the previous lookup.
!
!        We don't need to restore the segment start DAF address
!        BADDR, since we've already extracted it from DESCR.
!
!        Restore
!
!           - The mini-segment's packet directory count
!           - The mini-segment's packet directory base address
!
         npkdir = svpknd
         pkdbas = svpkdb

!
!        Restore
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         n      = svn
         miniix = svmiix
         minib  = svminb

!
!        Restore
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment interpolation window size
!           - The mini-segment clock rate
!
         subtyp = svstyp
         pktsiz = svpksz
         npkt   = svnpkt
         wndsiz = svwnsz
         rate   = svrate

      else
!
!        The segment and interval information for the current segment
!        must be looked up.
!
!        Perform checks on this segment.
!
!        Make sure that this really is a type 06 data segment.
!
         if ( type /= 6 ) then

            call ctx%setmsg( 'You are attempting to locate type * ' &
            //           'data in a type 6 data segment.'       )
            call ctx%errint( '*',  type                             )
            call ctx%sigerr( 'SPICE(WRONGCKTYPE)'                   )
            return

         end if

!
!        Locate the mini-segment interval that contains the request
!        time. If the request time lies a common boundary of two
!        intervals, the choice of interval is determined by the
!        interval selection flag.
!
!        Before getting started, we need to determine which interval to
!        use if the request time lies on a boundary between two
!        intervals. The segment's interval selection flag tells us how
!        to resolve this.
!
         call dafgda ( ctx, handle, eaddr-1, eaddr, contrl )

         if ( ctx%failed() ) return

         isel   = nint ( contrl(1) )
         n      = nint ( contrl(2) )

         ivlsel = isel == itrue

!
!        Determine the number of interval directory entries in the
!        segment. Note that for most CK types, this computation is
!        performed by computing
!
!           ( N - 1 ) / DIRSIZ
!
!        where N is the segment's epoch count.
!
!        However the set of items in this case is a sequence
!        of N start times followed by a final stop time, so
!        the epoch count is
!
!           N + 1
!
!        and the numerator in the ratio above is incremented by 1.
!
         ndir = n / dirsiz

!
!        Note that the directory placement scheme always leaves
!        a non-empty group of epochs following the last directory
!        entry.
!
!        Let DIRBAS be the base address of the interval directory.
!        We'll compute DIRBAS whether or not the interval directory
!        is non-empty.
!
!        If the interval directory is non-empty, it spans the address
!        range
!
!           DIRBAS+1 : DIRBAS+NDIR
!
!        We compute DIRBAS by starting at the end of the segment
!        and skipping over the control area, the mini-segment
!        start/stop pointers, and the interval directory itself.
!
         dirbas = eaddr - nsgpar - ( n + 1 ) - ndir

!
!        The way we search the directory depends on the treatment
!        of request times that lie on interval boundaries.
!
         if ( ivlsel ) then
!
!           We must pick the latest interval containing the request
!           time.
!
!           The stop time of the interval we seek is the first interval
!           boundary strictly greater than T, unless T is the stop time
!           of the final interval.
!
!           We want to find the group of interval boundaries containing
!           the stop time of the interval containing T. There are
!           NDIR+1 such groups; all but the last have a directory entry
!           that coincides with the final epoch of the group. We'll use
!           the variable GROUP as the group index.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir == 0 ) then
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            else
!
!              The index of the group we seek is the index of the first
!              directory entry that is greater than T, if such an entry
!              exists. If there's no such entry, the group we seek is
!              the final one.
!
!              Find the last directory entry less than or equal to
!              the request time. The directory entry after that one,
!              if such exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas
!
!              Fetch the current batch of directory entries.
!
               call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

               if ( ctx%failed() ) return

               remain = ndir - nread
!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               do while (       ( remain        > 0 ) &
                          .and. ( buffer(nread) <= t )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

                  if ( ctx%failed() ) return

                  remain = remain - nread

               end do
!
!              Count the directory entries that are less than or equal
!              to T. The number we skipped over before the final loop
!              iteration is BUFBAS-DIRBAS; the number of buffered
!              entries we're skipping is the number of entries that are
!              less than or equal to T. The index of the group of
!              epochs containing T exceeds the skipped directory count
!              by 1.
!
               group =    bufbas - dirbas &
                       +  lstled ( t, nread, buffer ) &
                       +  1
!
!              GROUP is in the range 1 : NDIR+1.
!
            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )
!
!           Now find the index of the last interval boundary less than
!           or equal to T. We'll need to read the current group of
!           epochs first, so compute the base of the range of addresses
!           containing this group.

            bufbas = ivbas + ( ( group - 1 ) * dirsiz )
!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            remain = ( n + 1 ) - ( ( group - 1 ) * dirsiz )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            nread  = min ( dirsiz, remain )

            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

            if ( ctx%failed() ) return

!
!           Find the index of the first epoch greater than T; this is
!           the number of epochs that are less than or equal to T, plus
!           1. The case where T matches the final epoch must be handled
!           here, since in this case no epoch exceeds T.
!
            iveix =    bufbas - ivbas &
                    +  lstled ( t, nread, buffer ) &
                    +  1

            iveix = min ( iveix,  n+1 )

!
!           Backstop test:
!
            if ( iveix < 2 ) then

               call ctx%setmsg ( 'IVEIX = #.' )
               call ctx%errint ( '#', iveix   )
               call ctx%sigerr ( 'SPICE(BUG)' )
               return

            end if

!
!           The epoch at index IVEIX is the end time of the
!           mini-segment interval we'll use. The index of
!           the interval itself is IVEIX - 1.
!
            miniix = iveix - 1

         else
!
!           IVLSEL is .FALSE., meaning we must pick the first interval
!           containing the request time.

!           The start time of the interval we seek is the last interval
!           boundary strictly less than T, unless T is the start time
!           of the first interval. The stop time of this interval is
!           the first boundary greater than or equal to T.
!
!           We want to find the group of interval boundaries containing
!           the stop time of the interval containing T. There are
!           NDIR+1 such groups; all but the last have a directory entry
!           that coincides with the final epoch of the group. We'll use
!           the variable GROUP as the group index.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir == 0 ) then
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            else
!
!              Find the last directory entry strictly less than the
!              request time. The directory entry after that one, if
!              such exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas
               remain = ndir - nread
!
!              Fetch the current batch of directory entries.
!
               call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

               if ( ctx%failed() ) return
!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               do while (      ( remain        > 0 ) &
                         .and. ( buffer(nread) < t )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

                  if ( ctx%failed() ) return

                  remain = remain - nread

               end do
!
!              Count the directory entries that are less than T. The
!              number we skipped over before the final loop iteration
!              is BUFBAS-DIRBAS; the number of buffered entries we're
!              skipping is the number of entries that are less than T.
!              The index of the group of epochs containing T exceeds
!              the skipped directory count by 1.
!
               group =    bufbas - dirbas &
                       +  lstltd ( t, nread, buffer ) &
                       +  1
!
!              GROUP is in the range 1 : NDIR+1.
!
            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )

!
!           Now find the index of the last interval boundary epoch less
!           than T. We'll need to read the current group of epochs
!           first, so compute the base of the range of addresses
!           containing this group.
!
            bufbas = ivbas + ( ( group - 1 ) * dirsiz )

!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            remain = ( n + 1 ) - ( ( group - 1 ) * dirsiz )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            nread  = min ( dirsiz, remain )

            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

            if ( ctx%failed() ) return

!
!           Find the index of the last interval boundary less than T.
!           The case where T matches the first boundary must be handled
!           here, since in this case no boundary precedes T.
!
            ivbix =    bufbas - ivbas &
                    +  lstltd ( t, nread, buffer )

            ivbix = max ( ivbix,  1 )

!
!           Backstop test:
!
            if ( ivbix > n ) then

               call ctx%setmsg ( 'IVBIX = #.' )
               call ctx%errint ( '#', ivbix   )
               call ctx%sigerr ( 'SPICE(BUG)' )
               return

            end if

!
!           The epoch at index IVBIX is the begin time of the
!           mini-segment interval we'll use.
!
            miniix = ivbix

         end if
!
!        This is the end of the IF block that handles mini-segment
!        selection for the two possible values of IVLSEL.
!
!        If the mini-segment we just found has a gap, and if TOL is
!        positive, it's possible that the mini-segment we want actually
!        is the successor of the one at index MINIIX. We'll check this
!        by finding the last epoch of the mini-segment we just
!        identified.
!
!        Look up the begin and end pointers of the mini-segment at index
!        MINIIX. For the first N-1 mini-segments, the "end pointer"
!        of one mini-segment is the "begin" pointer of the next.
!
         bufbas = eaddr - nsgpar - ( n + 1 ) + ( miniix - 1 )

         call dafgda ( ctx, handle, bufbas+1, bufbas+2, buffer )

         if ( ctx%failed() ) return

         minib = nint ( buffer(1) ) + baddr - 1
!
!        Note that the end of the current mini-segment
!        precedes the start of the next mini-segment by
!        one address.
!
         minie = nint ( buffer(2) ) + baddr - 2

!
!        Look up the control area of the mini-segment.
!
         call dafgda ( ctx, handle, minie-ctrlsz+1, minie, contrl )

         if ( ctx%failed() ) return

!
!        Fetch the control area parameters for the mini-segment.
!
         rate    =         contrl(1)
         subtyp  =  nint ( contrl(2) )
         wndsiz  =  nint ( contrl(3) )
         npkt    =  nint ( contrl(4) )

!
!        Compute the directory count for the mini-segment.
!
         npkdir  =  ( npkt - 1 ) / dirsiz

!
!        The last epoch of the mini-segment precedes the epoch
!        directories and the control area. Look up this epoch.
!
         bufbas  =  minie - ctrlsz - npkdir - 1

         call dafgda ( ctx, handle, bufbas+1, bufbas+1, lstepc_1 )
         lstepc = lstepc_1(1)

         if ( ctx%failed() ) return

!
!        Determine whether the request time is in a gap.
!
         if ( t > lstepc ) then
!
!           Yep, T lies in a gap. But we may still be able to
!           find data for this request, if the lookup tolerance
!           is positive.
!
            if ( tol == 0.0_wp ) then
!
!              We're out of luck. We can't find pointing for this
!              request. FOUND is already .FALSE., so just return.
!
               return

            else
!
!              Determine the distance of T from the nearest epochs.
!
!              Look up the time bounds of the mini-segment at index
!              MINIIX.
!
               call dafgda ( ctx, handle, ivbas+miniix, &
                                     ivbas+miniix+1, mintim )

               if ( ctx%failed() ) return

!
!              See whether T is close enough to a stored epoch for
!              us to find pointing. If not, return now.
!
               if (      (  ( t         - lstepc )  >  tol  ) &
                   .and. (  ( mintim(2) - t      )  >  tol  )  ) then
!
!                 We can't find pointing for T. FOUND is already
!                 .FALSE., so just return.
!
                  return

               end if

!
!              Continue to look for pointing usable for time T.
!
               if ( miniix == n ) then
!
!                 We're looking at the final mini-segment. If
!                 T is close enough to LSTEPC, we can find
!                 pointing.
!
                  if (  ( t - lstepc )  <=  tol  ) then
!
!                    We're going to carry on using the current
!                    mini-segment. We'll update T to be the last epoch
!                    of this mini-segment.
!
                     t = lstepc

                  else
!
!                    T is too far from LSTEPC. We're done. FOUND is
!                    already .FALSE., so just return.
!
                     return

                  end if

               else
!
!                 There's a successor to the current interval. Determine
!                 which interval contains an epoch closest to T.
!
!                 Compute the size of the gap at the right end of the
!                 interior of the current interval.
!
                  gap  =  mintim(2) - lstepc

                  if (  ( t - lstepc )  <=  ( gap/2 )  ) then
!
!                    T is closer to LSTEPC than the start time of the
!                    next interval. We're going to carry on using the
!                    current mini-segment. We'll update T to be the
!                    last epoch of this mini-segment.
!
                     t = lstepc

                  else
!
!                    T is closer to the start time of the next interval
!                    than to LSTEPC. than the start time of the next
!                    interval. We're going to use the next
!                    mini-segment.
!
                     miniix  =  miniix + 1

!
!                    Update the mini-segment parameters we already
!                    found, since these have been superseded.
!
!                    The mini-segment pointers:
!
                     bufbas = eaddr - nsgpar - (n + 1) + ( miniix - 1 )

                     call dafgda ( ctx, handle, bufbas+1, bufbas+2, buffer )

                     if ( ctx%failed() ) return

                     minib = nint ( buffer(1) ) + baddr - 1
!
!                    Note that the end of the current mini-segment
!                    precedes the start of the next mini-segment by one
!                    address.
!
                     minie = nint ( buffer(2) ) + baddr - 2

!
!                    Look up the control area of the mini-segment.
!
                     call dafgda ( ctx, handle, minie-ctrlsz+1, &
                                           minie,          contrl )

                     if ( ctx%failed() ) return

!
!                    Fetch the control area parameters for the
!                    mini-segment.
!
                     rate    =  nint ( contrl(1) )
                     subtyp  =  nint ( contrl(2) )
                     wndsiz  =  nint ( contrl(3) )
                     npkt    =  nint ( contrl(4) )
!
!                    Since we have new mini-segment parameters, we need
!                    to check them. We'll defer these checks until
!                    later, so we can perform one set of checks,
!                    regardless of which logic path we followed to
!                    select a mini-segment.
!
!                    Compute the directory count for the mini-segment.
!
                     npkdir  =  ( npkt - 1 ) / dirsiz
!
!                    We're going to set T to the start time of the
!                    current mini-segment interval, which is the stop
!                    time of the previous one.
!
                     t = mintim(2)
!
!                    We still need to look up the last epoch of the
!                    current mini-segment. We'll use this when we save
!                    the time bounds of the mini-segment.
!
                     bufbas = minie - ctrlsz - npkdir - 1

                     call dafgda ( ctx, handle, bufbas+1, bufbas+1, lstepc_1 )
                     lstepc = lstepc_1(1)

                     if ( ctx%failed() ) return

                  end if

               end if
!
!              At this point T is set. If we had to update the
!              mini-segment index and its parameters, we did so.
!
            end if
!
!           We've handled the case where T lies in a gap and the
!           tolerance is non-zero.

         end if
!
!        This is the end of the block that handles the case where T lies
!        in a gap. At this point, the following items are set:
!
!           T
!           MINIIX
!           MINIB
!           MINIE
!           SUBTYP
!           WNDSIZ
!           NPKT
!           NPKDIR
!           RATE
!
!        Look up the time bounds of the mini-segment at index MINIIX.
!        These bounds are used quite a bit farther on, when we save
!        them for future use.
!
         call dafgda ( ctx, handle, ivbas+miniix, ivbas+miniix+1, mintim )

         if ( ctx%failed() ) return

!
!        From this point onward, we'll work with the mini-segment
!        that occupies the address range MINIB : MINIE.
!
!        Set the packet size, which is a function of the subtype.
!        Also set the maximum window size. First check the subtype,
!        which will be used as an array index.
!
         if (  ( subtyp < 0 ) .or. ( subtyp >= c06nst ) ) then

            call ctx%setmsg ( 'Unexpected CK type 6 subtype # found in ' &
            //            'type 06 segment within mini-segment #.'     )
            call ctx%errint ( '#',  subtyp                                 )
            call ctx%errint ( '#',  miniix                                 )
            call ctx%sigerr ( 'SPICE(INVALIDSUBTYPE)'                      )
            return

         end if

         pktsiz = pktszs ( subtyp )
         maxwnd = mxwnsz ( subtyp )

!
!        Check the window size.
!
         if ( ( wndsiz < 2 ) .or. ( wndsiz > maxwnd ) ) then

            call ctx%setmsg ( 'Window size in type 6 segment was #; must ' &
            //            'be in the range 2:# for subtype #. ' &
            //            'Mini-segment index is #.'                   )
            call ctx%errint ( '#',  wndsiz                                 )
            call ctx%errint ( '#',  maxwnd                                 )
            call ctx%errint ( '#',  subtyp                                 )
            call ctx%errint ( '#',  miniix                                 )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                        )
            return

         end if

         if ( odd(wndsiz) ) then

            call ctx%setmsg ( 'Window size in type 06 segment was #; ' &
            //            'must be even for subtype #. ' &
            //            'Mini-segment index is #.'               )
            call ctx%errint ( '#',  wndsiz                             )
            call ctx%errint ( '#',  subtyp                             )
            call ctx%errint ( '#',  miniix                             )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                    )
            return

         end if

!
!        Compute the base address of the sequence of packet
!        directory entries for the current mini-segment/interval.
!
         pkdbas  =  minib - 1 + ( npkt * ( pktsiz + 1 ) )

!
!        The test below is done for safety. No SPICE errors
!        should ever be detected at this point.
!
         if ( ctx%failed() ) return

!
!        If we made it this far, we did so without a SPICE error. We
!        have valid segment parameters which can be saved for the next
!        call.
!
!        Save
!
!           - The DAF handle
!           - The segment begin DAF address
!           - The segment's "select last/first interval" flag
!
         svhan  = handle
         svbeg  = baddr
         svlast = ivlsel

!
!        Save the time bounds of the applicable mini-segment/interval.
!        We don't want to indicate data availability within a gap, since
!        the re-use logic assumes data availability.
!
         svbtim =       mintim(1)
         svetim = min ( mintim(2), lstepc )

!
!        Save
!
!           - The mini-segment/interval directory count
!           - The mini-segment/interval directory base address
!
         svpknd = npkdir
         svpkdb = pkdbas

!
!        Save
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         svn    = n
         svmiix = miniix
         svminb = minib

!
!        Save
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment window size
!           - The mini-segment clock rate
!
         svstyp = subtyp
         svpksz = pktsiz
         svnpkt = npkt
         svwnsz = wndsiz
         svrate = rate

      end if

!
!     We're ready to construct the output record. The first step is to
!     identify the indices of the packets and epochs corresponding to
!     the request.
!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      if ( npkdir == 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         group  = 1

      else
!
!        There's at least one directory entry. Find the first directory
!        entry whose time is greater than or equal to the request time,
!        if there is such an entry.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  pkdbas
         nread   =  min ( npkdir, dirsiz )
         remain  =  npkdir - nread

         call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

         if ( ctx%failed() ) return

         do while (       ( buffer(nread) < t ) &
                    .and. ( remain        > 0 )  )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, dirsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

            if ( ctx%failed() ) return

         end do

!
!        At this point, BUFBAS - PKDBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - pkdbas ) &
                    +   lstltd ( t, nread, buffer ) &
                    +   1
      end if

!
!     GROUP now indicates the set of epochs in which to search for the
!     request epoch.  If GROUP is 1, the request time lies within the
!     inclusive time interval bounded by the first and last epochs of
!     the first group.  Otherwise, the request time lies in the time
!     interval bounded by the last element of the preceding group and
!     the last element of the current group.
!
!     We'll use the variable names BEGIDX and ENDIDX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( group == 1 ) then

         begidx  =  1
         endidx  =  min ( npkt, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         begidx  =      (  group  - 1 ) * dirsiz
         endidx  =  min (  begidx + dirsiz,  npkt  )

      end if

      timbas  =  pkdbas - npkt

      call dafgda ( ctx, handle, timbas+begidx, timbas+endidx, buffer )

      if ( ctx%failed() ) return

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      i  =  lstltd (  t,  endidx-begidx+1,  buffer  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      if ( i == 0 ) then
         low  =  1
      else
         low  =  begidx + i - 1
      end if

      high  =  low  +  1

!
!     Now select the set of packets used for interpolation. Note
!     that the window size is known to be even.
!
!     For CK type 6 we allow the window size to shrink when the window
!     must be truncated due to proximity to an interval boundary.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window: the size
!     set of window epochs to the left of the request time. We want
!     this size to be WNDSIZ/2, but if not enough packets are
!     available, the set ranges from index 1 to index LOW.
!

      lsize =  min ( wndsiz/2, low )

!
!     RSIZE is defined analogously for the right half of the window.
!
      rsize =  min (  wndsiz/2,  ( npkt - high + 1 )  )

!
!     The actual window size is simply the sum of LSIZE and RSIZE.
!
      nrcpkt =  lsize + rsize

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      first =  low    -  lsize   +  1

      last  =  first  +  nrcpkt  -  1

!
!     We're ready to construct the output record.
!
!     Put the subtype and window size into the output record.
!     The fourth element is the nominal SCLK rate.
!
      record(1) =  t
      record(2) =  subtyp
      record(3) =  nrcpkt
      record(4) =  rate

!
!     Read the packets.
!
      call dafgda ( ctx, handle, &
                    minib + (first-1)*pktsiz, &
                    minib +      last*pktsiz  -  1, &
                    record( ctrlsz + 1 )           )
!
!     Finally, add the epochs to the output record.
!     Read the sequence of time tags.
!
      bufbas  =  ( minib - 1 )  +  ( npkt * pktsiz )  +  ( first - 1 )

      call dafgda ( ctx, handle, &
                    bufbas+1, &
                    bufbas+nrcpkt, &
                    record( ctrlsz + nrcpkt*pktsiz + 1 )  )

      if ( ctx%failed() ) return
!
!     Indicate pointing was found.
!
      found = .true.
      svfnd = .true.

      return
      end associate
      end subroutine ckr06

      !> Read a single PCK data record from a segment of type 2
      !> (Chebyshev, 3-vector only).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a PCK segment of type 2.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the Euler
      !>              angles (orientation) of some body.
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
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the PCK Required Reading file for a description of the
      !>     structure of a data type 2 (Chebyshev polynomials, Euler
      !>     angles only) segment.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Dump the record of a type 2 PCK which, when evaluated at
      !>        a given epoch, will give the Euler angles (orientation) of
      !>        the Moon body-fixed frame with class ID 31004 with respect
      !>        to J2000.
      !>
      !>        Note that the data returned is in its rawest form, taken
      !>        directly from the segment. As such, it will be meaningless to
      !>        a user unless he/she understands the structure of the data
      !>        type completely. Given that understanding, however, the PCKR02
      !>        routine might be used to "dump" and check segment data for a
      !>        particular epoch.
      !>
      !>        Use the PCK kernel below to obtain the record.
      !>
      !>           moon_pa_de418_1950-2050.bpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PCKR02_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               BODY
      !>      PARAMETER           ( BODY   = 31004 )
      !>
      !>      INTEGER               DESCSZ
      !>      PARAMETER           ( DESCSZ = 5     )
      !>
      !>      INTEGER               IDSIZE
      !>      PARAMETER           ( IDSIZE = 40    )
      !>
      !>C
      !>C     Set the maximum record size:
      !>C
      !>C        RSIZE = 2 + 3 * (PDEG +1)
      !>C
      !>C     Assume a maximum polynomial degree of 25, and
      !>C     knowing that PCKR02 returns RSIZE as first element
      !>C     of the output record...
      !>C
      !>      INTEGER               RECRSZ
      !>      PARAMETER           ( RECRSZ = 81    )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(IDSIZE)    SEGID
      !>
      !>      DOUBLE PRECISION      BEGET
      !>      DOUBLE PRECISION      DESCR  ( DESCSZ )
      !>      DOUBLE PRECISION      ENDET
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      RECORD ( RECRSZ )
      !>
      !>      INTEGER               BADDR
      !>      INTEGER               BODYID
      !>      INTEGER               EADDR
      !>      INTEGER               FRAMID
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               INDEX
      !>      INTEGER               PCKHDL
      !>      INTEGER               PCKTYP
      !>      INTEGER               PDEG
      !>      INTEGER               RSIZE
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load the PCK file.
      !>C
      !>      CALL PCKLOF ( 'moon_pa_de418_1950-2050.bpc', PCKHDL )
      !>
      !>C
      !>C     Set the epoch. Use ephemeris time of J2000 epoch.
      !>C
      !>      ET = 0.D0
      !>
      !>C
      !>C     Get a segment applicable to a specified body and epoch.
      !>C
      !>      CALL PCKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )
      !>
      !>      IF ( FOUND ) THEN
      !>
      !>C
      !>C        Unpack the segment.
      !>C
      !>         CALL PCKUDS ( DESCR, BODYID, FRAMID, PCKTYP,
      !>     .                 BEGET, ENDET,  BADDR,  EADDR  )
      !>
      !>         IF ( PCKTYP .EQ. 2 ) THEN
      !>
      !>            CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
      !>
      !>            RSIZE = RECORD(1)
      !>            PDEG  = ( RSIZE - 2 ) / 3 - 1
      !>
      !>C
      !>C           Output the data.
      !>C
      !>            WRITE(*,*) 'Record size      : ', RSIZE
      !>            WRITE(*,*) 'Polynomial degree: ', PDEG
      !>
      !>            WRITE(*,*) 'Record data      :'
      !>            WRITE(*,*) '   Interval midpoint: ', RECORD(2)
      !>            WRITE(*,*) '   Interval radius  : ', RECORD(3)
      !>
      !>            INDEX = 4
      !>            WRITE(*,*) '   RA coefficients  : '
      !>            DO I = 0, PDEG
      !>               WRITE(*,*) '      ', RECORD(INDEX+I)
      !>            END DO
      !>
      !>            INDEX = 4 + ( PDEG + 1 )
      !>            WRITE(*,*) '   DEC coefficients : '
      !>            DO I = 0, PDEG
      !>               WRITE(*,*) '      ', RECORD(INDEX+I)
      !>            END DO
      !>
      !>            INDEX = 4 + 2 * ( PDEG + 1 )
      !>            WRITE(*,*) '   W coefficients   : '
      !>            DO I = 0, PDEG
      !>               WRITE(*,*) '      ', RECORD(INDEX+I)
      !>            END DO
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'PCK is not type 2'
      !>
      !>         END IF
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) '   ***** SEGMENT NOT FOUND *****'
      !>
      !>      END IF
      !>
      !>C
      !>C     Unload the PCK file.
      !>C
      !>      CALL PCKUOF ( PCKHDL )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Record size      :           32
      !>         Polynomial degree:            9
      !>         Record data      :
      !>            Interval midpoint:    302400.00000000000
      !>            Interval radius  :    345600.00000000000
      !>            RA coefficients  :
      !>                 -5.4242086033301107E-002
      !>                 -5.2241405162792561E-005
      !>                  8.9751456289930307E-005
      !>                 -1.5288696963234620E-005
      !>                  1.3218870864581395E-006
      !>                  5.9822156790328180E-007
      !>                 -6.5967702052551211E-008
      !>                 -9.9084309118396298E-009
      !>                  4.9276055963541578E-010
      !>                  1.1612267413829385E-010
      !>            DEC coefficients :
      !>                 0.42498898565916610
      !>                  1.3999219324235620E-004
      !>                 -1.8855140511098865E-005
      !>                 -2.1964684808526649E-006
      !>                  1.4229817868138752E-006
      !>                 -1.6991716166847001E-007
      !>                 -3.4824688140649506E-008
      !>                  2.9208428745895990E-009
      !>                  4.4217757657060300E-010
      !>                 -3.9211207055305402E-012
      !>            W coefficients   :
      !>                  2565.0633504619473
      !>                 0.92003769451305328
      !>                 -8.0503797901914501E-005
      !>                  1.1960860244433900E-005
      !>                 -1.2237900518372542E-006
      !>                 -5.3651349407824562E-007
      !>                  6.0843372260403005E-008
      !>                  9.0211287487688797E-009
      !>                 -4.6460429330339309E-010
      !>                 -1.0446918704281774E-010
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
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.2, 06-JUL-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example from existing fragment.
      !>
      !>-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.0, 11-MAR-1993 (KSZ)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      module subroutine pckr02 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

!
!     Parameters
!

      integer(ip), parameter :: ni = 5

      real(wp) :: dc       (   nd )
      real(wp) :: init
      real(wp) :: intlen

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: ic       (   ni )
      integer(ip) :: nrec
      integer(ip) :: recadr
      integer(ip) :: recno
      integer(ip) :: recsiz

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, nd, ni, dc, ic )

      begin = ic( ni-1 )
      end   = ic( ni )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return by comparing the input
!     epoch with the initial time of the segment and the length of the
!     interval covered by each record.  These final two constants are
!     located at the end of the segment, along with the size of each
!     logical record and the total number of records.
!
      call dafgda ( ctx, handle, end-3, end, record )

      init   = record( 1 )
      intlen = record( 2 )
      recsiz = int( record( 3 ) )
      nrec   = int( record( 4 ) )

      recno = int( (et - init) / intlen ) + 1
      recno = min( recno, nrec )

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size of the record.
!
      record( 1 ) = record( 3 )
      call dafgda ( ctx, handle, recadr, recadr + recsiz - 1, record( 2 ) )

      return
      end subroutine pckr02

      !> Read a single PCK data record from a segment of type 20
      !> (Chebyshev, derivative coefficients only).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a PCK segment of type 20.
      !>
      !>     ET       is an epoch for which a data record from a specific
      !>              segment is required. ET is expressed as seconds past
      !>              J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which, when
      !>              evaluated at epoch ET, will give Euler angles and
      !>              Euler angle rates representing the orientation and
      !>              angular velocity of the body-fixed reference frame
      !>              associated with the segment.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for ANGLE_1 rate              |
      !>                 +--------------------------------------+
      !>                 | Coeffs for ANGLE_2 rate              |
      !>                 +--------------------------------------+
      !>                 | Coeffs for ANGLE_3 rate              |
      !>                 +--------------------------------------+
      !>                 | ANGLE_1 at interval midpoint         |
      !>                 +--------------------------------------+
      !>                 | ANGLE_2 at interval midpoint         |
      !>                 +--------------------------------------+
      !>                 | ANGLE_3 at interval midpoint         |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Angular components have units of radians.
      !>                 - Rate coefficients have units of radians/s.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an issue is detected while looking up PCK data, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the PCK Required Reading file for a description of the
      !>     structure of a data type 20 (Chebyshev polynomials,
      !>     derivative coefficients only) segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the PCKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the PCKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified frame class ID
      !>     C     and epoch.
      !>     C
      !>           CALL PCKSFS ( CLSSID, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>     C
      !>     C     Look at parts of the descriptor.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>           REF    = ICD( 2 )
      !>           TYPE   = ICD( 3 )
      !>
      !>           IF ( TYPE .EQ. 20 ) THEN
      !>              CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (IMU)
      module subroutine pckr20 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: j2000, spd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      real(wp) :: dc       ( 2 )
      real(wp) :: dscale
      real(wp) :: init
      real(wp) :: initfr
      real(wp) :: initjd
      real(wp) :: intlen
      real(wp) :: intrvl
      real(wp) :: mid
      real(wp) :: pos      ( 3 )
      real(wp) :: radius
      real(wp) :: recbeg
      real(wp) :: tscale

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: i
      integer(ip) :: ic       ( 6 )
      integer(ip) :: loc
      integer(ip) :: nrec
      integer(ip) :: nterms
      integer(ip) :: recadr
      integer(ip) :: recno
      integer(ip) :: recsiz
      integer(ip) :: size

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      begin = ic( 4 )
      end   = ic( 5 )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the integer and fractional parts of the initial time of the first
!     record's coverage interval, and the length of the interval
!     covered by each record. These constants are located at the end of
!     the segment, along with the size of each logical record and the
!     total number of records.
!
!     For convenience, we'll fetch the segment's distance and time
!     scales in the same call.
!
      call dafgda ( ctx, handle, end-6, end, record )

      dscale = record( 1 )
      tscale = record( 2 )
      initjd = record( 3 )
      initfr = record( 4 )
      intlen = record( 5 )
      recsiz = int( record( 6 ) )
      nrec   = int( record( 7 ) )

!
!     NTERMS is the number of rate coefficients per component,
!     plus 1 (for the angle component).
!
      nterms = recsiz / 3

!
!     Convert the initial epoch and interval length to
!     seconds past J2000 TDB.
!
      init   = (  ( initjd - j2000 )  +  initfr  ) * spd
      intrvl = intlen * spd

!
!     Locate the record containing the coefficients to use.
!
      recno  = int( (et - init) / intrvl ) + 1
      recno  = max( 1,  min( recno, nrec )  )

!
!     Compute the midpoint and radius of the record at
!     index RECNO. We want to compute the midpoint in such
!     a way that we take advantage of interval lengths that
!     are exactly representable, when we have them.
!
!     RECBEG is the record start time, minus the fractional
!     part of the segment start time, expressed as seconds
!     past J2000. We'll account for the fractional part of the
!     start time below when we compute MID.
!
      recbeg = (  ( initjd - j2000 )  +  (recno-1)*intlen  ) * spd

      radius = intrvl / 2.0_wp

      mid    = recbeg + ( initfr * spd ) + radius

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size, midpoint, and
!     radius of the record.
!
      record( 1 ) = record( 6 ) + 2
      record( 2 ) = mid
      record( 3 ) = radius
      call dafgda ( ctx, handle, recadr, recadr + recsiz - 1, record( 4 ) )

!
!     We're going to re-arrange the record: the angle components
!     will be transferred to the end of the record, and the record
!     contents will be left-shifted to fill in the free elements.
!
      do i = 1, 3
         pos( i ) = record ( 3  +  i*nterms )
      end do

      size = recsiz + 3
!
!     Remove the angle elements from the record.
!
      do i = 1, 3
!
!        LOC is the index of the element to delete. After the first
!        removal, we must account for the resulting left shift when
!        calculating the indices of subsequent elements to be removed.
!
         loc = 3 + i*nterms - (i-1)

         call remlad ( ctx, 1, loc, record, size )
!
!        Note that SIZE is an in-out argument; on output it
!        indicates the size of the array after removal of
!        the indicated element(s).
!
      end do

!
!     Convert the angles to radians.
!
      pos = dscale*pos

!
!     Append the angles to the record. Since we inserted three
!     elements at the start of the record and deleted three angle
!     elements, the target index is the same as if we had copied the
!     record directly to the output array.
!
      record(recsiz+1:recsiz+1+3-1) = pos

!
!     Convert the angular rate Chebyshev coefficients to units of
!     radians/s.
!
      do i = 4, recsiz
         record(i) = record(i) * (dscale/tscale)
      end do

      return
      end subroutine pckr20

      !> Obtain the value of a specified generic segment meta data item.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF opened for reading that
      !>              contains the generic segment described by DESCR.
      !>
      !>     DESCR    is the descriptor of a generic segment. This must
      !>              be the descriptor for a generic segment in the DAF
      !>              associated with HANDLE.
      !>
      !>     MNEMON   is the mnemonic used to represent the desired piece of
      !>              meta data. See the file 'sgparam.inc' for details, the
      !>              mnemonics, and their values.
      !>
      !>## Detailed Output
      !>
      !>     VALUE    is the value of the meta data item associated with
      !>              the mnemonic MNEMON that is in the generic segment
      !>              specified by HANDLE and DESCR.
      !>
      !>## Parameters
      !>
      !>     This subroutine makes use of parameters defined in the file
      !>     'sgparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If the mnemonic for the meta data item is not valid, the error
      !>         SPICE(UNKNOWNMETAITEM) is signaled.
      !>
      !>     2)  If the last address in the DAF segment that reports the number
      !>         of meta data items that exist in the segment is less than
      !>         MNMETA, the error SPICE(INVALIDMETADATA) is signaled.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE above.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility for fetching the meta data associated
      !>     with a DAF generic segment.
      !>
      !>     A DAF generic segment contains several logical data partitions:
      !>
      !>        1) A partition for constant values to be associated with each
      !>           data packet in the segment.
      !>
      !>        2) A partition for the data packets.
      !>
      !>        3) A partition for reference values.
      !>
      !>        4) A partition for a packet directory, if the segment contains
      !>           variable sized packets.
      !>
      !>        5) A partition for a reference value directory.
      !>
      !>        6) A reserved partition that is not currently used. This
      !>           partition is only for the use of the NAIF group at the Jet
      !>           Propulsion Laboratory (JPL).
      !>
      !>        7) A partition for the meta data which describes the locations
      !>           and sizes of other partitions as well as providing some
      !>           additional descriptive information about the generic
      !>           segment.
      !>
      !>                 +============================+
      !>                 |         Constants          |
      !>                 +============================+
      !>                 |          Packet 1          |
      !>                 |----------------------------|
      !>                 |          Packet 2          |
      !>                 |----------------------------|
      !>                 |              .             |
      !>                 |              .             |
      !>                 |              .             |
      !>                 |----------------------------|
      !>                 |          Packet N          |
      !>                 +============================+
      !>                 |      Reference Values      |
      !>                 +============================+
      !>                 |      Packet Directory      |
      !>                 +============================+
      !>                 |    Reference  Directory    |
      !>                 +============================+
      !>                 |       Reserved  Area       |
      !>                 +============================+
      !>                 |     Segment Meta Data      |
      !>                 +----------------------------+
      !>
      !>     Only the placement of the meta data at the end of a segment is
      !>     required. The other data partitions may occur in any order in the
      !>     segment because the meta data will contain pointers to the
      !>     appropriate locations of the other data partitions within the
      !>     segment.
      !>
      !>     The meta data for the segment should be obtained only through
      !>     use of this routine, SGMETA.
      !>
      !>## Examples
      !>
      !>     Suppose that we would like to know how many constants, data
      !>     packets, and reference values are in the generic segment that we
      !>     have located in the DAF file associated with HANDLE.
      !>
      !>     C
      !>     C     Get the number of constants.
      !>     C
      !>           CALL SGMETA ( HANDLE, DESCR, NCON, NCONST )
      !>     C
      !>     C     Get the number of data packets.
      !>     C
      !>           CALL SGMETA ( HANDLE, DESCR, NPKT, NPKTS )
      !>     C
      !>     C     Get the number of constants.
      !>     C
      !>           CALL SGMETA ( HANDLE, DESCR, NREF, NREFS )
      !>
      !>     C
      !>     C     Print the values.
      !>     C
      !>           WRITE (*, *) 'Number of Constants       : ', NCONST
      !>           WRITE (*, *) 'Number of Data Packets    : ', NPKTS
      !>           WRITE (*, *) 'Number of Reference Values: ', NREFS
      !>
      !>## Restrictions
      !>
      !>     1)  The segment described by DESCR MUST be a generic segment,
      !>         otherwise the results of this routine are not predictable.
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
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.4.1, 20-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.4.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>
      !>-    SPICELIB Version 1.3.0, 14-JUN-1999 (FST)
      !>
      !>        Altered the check in/out structure to be more reasonable.
      !>        This introduced redundant code, but only to increase the
      !>        efficiency of the normal mode of operation.
      !>
      !>-    SPICELIB Version 1.2.0, 24-SEP-1998 (FST)
      !>
      !>        Modified the code that handles reading the meta data from the
      !>        DAF to handle the case when the number of meta data items in
      !>        the file exceeds the current maximum defined in sgparam.inc.
      !>        In the event that this situation occurs, the routine loads
      !>        what meta data it can interpret and ignores the rest. In
      !>        this event if NMETA is requested, it is returned as MXMETA in
      !>        sgparam.inc.
      !>
      !>        An additional exception is now trapped by the routine. If
      !>        a generic segment in a DAF reports less than the known minimum
      !>        number of meta data items, then the routine signals the
      !>        error SPICE(INVALIDMETADATA).
      !>
      !>        The conditions that cause the SPICE(UNKNOWNMETAITEM) to be
      !>        signaled have been altered. Now if the integer mnemonic
      !>        is not between 1 and METASZ inclusive, or NMETA the error
      !>        is signaled. In the versions preceding this change, for
      !>        segments that reported less than NMETA items of meta data
      !>        could not use this routine to request the number of meta
      !>        data items without signaling SPICE(UNKNOWNMETAITEM).
      !>
      !>-    SPICELIB Version 1.1.0, 11-APR-1995 (KRG)
      !>
      !>        Modified the code that deals with the EQUIVALENCEd part
      !>        descriptor. We now call MOVED rather than using a direct
      !>        assignment.
      !>
      !>-    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT)
      module subroutine sgmeta ( ctx, handle, descr, mnemon, value )
      use pepper_kinds, only: ip, wp
      use pepper_packing, only: unpack_integers
      use pepper_inc_sgparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF open for reading.
      real(wp), intent(in) :: descr  ( * )  !! Descriptor for a generic segment in the DAF.
      integer(ip), intent(in) :: mnemon  !! An integer mnemonic for the desired meta data.
      integer(ip), intent(out) :: value  !! The value of the meta data item requested.
      real(wp) :: dmtasz_1(1)  !! one-element copy of dmtasz (array argument)
      integer(ip) :: ametas
      integer(ip) :: begin
      integer(ip) :: begmta
      integer(ip) :: begm1
      integer(ip) :: end
      integer(ip) :: endmta
      integer(ip) :: i
      integer(ip) :: nd
      integer(ip) :: ni
      integer(ip) :: niovr2
      real(wp) :: dmtasz
      real(wp) :: xmeta  ( mxmeta )
      integer(ip) :: itemp  (      4 )
      if ( .not. allocated ( ctx%sgmeta_state ) ) allocate ( ctx%sgmeta_state )
      associate ( lstbeg => ctx%sgmeta_state%lstbeg, &
                  lsthan => ctx%sgmeta_state%lsthan, &
                  meta => ctx%sgmeta_state%meta, &
                  metasz => ctx%sgmeta_state%metasz, &
                  nieven => ctx%sgmeta_state%nieven, &
                  ioffst => ctx%sgmeta_state%ioffst )
      if ( ctx%failed() ) return

!
!     Handle the case when we are looking at the same file and segment
!     descriptor first.  This will result in duplicated code, but will
!     increase efficiency for the usual execution case. We need not
!     worry about the first time through, since LSTHAN and LSTBEG are
!     set to values that are bogus for actual DAF files.
!
      if ( handle == lsthan ) then

!
!        Get the begin and end values from the descriptor. They are
!        located in the last two "integer" positions of the descriptor.
!
         if ( nieven ) then
            call unpack_integers ( descr(ioffst:ioffst), 2, itemp(1:2) )
            begin = itemp(1)
            end   = itemp(2)
         else
            call unpack_integers ( descr(ioffst:ioffst+2-1), 4, itemp )
            begin = itemp(2)
            end   = itemp(3)
         end if

!
!        Check the segment start address. This will tell us whether we
!        are looking at the same segment.
!
         if ( lstbeg == begin ) then

!
!        The only acceptable integer mnemonics at this point are 1
!        through METASZ inclusive, and NMETA.  All other requests
!        should signal the SPICE(UNKNOWNMETAITEM) error, since the
!        current segment has no knowledge of these values.
!
            if (   ( mnemon <= 0      )       .or. &
                 ( ( mnemon > metasz ) .and. &
                   ( mnemon /= nmeta  ) )           ) then

               value = -1
               call ctx%setmsg ( 'The item requested, #, is not one of the' &
               //            ' recognized meta data items associated' &
               //            ' with this generic segment.'             )
               call ctx%errint ( '#',   mnemon                             )
               call ctx%sigerr ( 'SPICE(UNKNOWNMETAITEM)'                  )
               return

            end if

!
!           Set the value for the desired meta data item and return.
!
            value = meta ( mnemon )

            return

         end if

      end if

!
!     At this point we are going to have to load the meta data.  If
!     the new handle and the old handle are the same, then the above
!     code has already retrieved the relevant segment addresses. If not
!     we need to fetch them.  First check in.
!

      if ( handle /= lsthan ) then

         call dafhsf ( ctx, handle, nd,  ni )

         if ( ctx%failed() ) return

         niovr2 = ni / 2

         nieven = ( 2 * niovr2 ) == ni

         ioffst = nd + niovr2
         lsthan = handle

!
!        Get the begin and end values from the descriptor. They are
!        located in the last two "integer" positions of the descriptor.
!
         if ( nieven ) then
            call unpack_integers ( descr(ioffst:ioffst), 2, itemp(1:2) )
            begin = itemp(1)
            end   = itemp(2)
         else
            call unpack_integers ( descr(ioffst:ioffst+2-1), 4, itemp )
            begin = itemp(2)
            end   = itemp(3)
         end if

      end if

!
!     Save the new begin address. Remember we have either just computed
!     this from the IF block above, or we computed it in the very
!     first IF block.
!
      lstbeg = begin

!
!     Compute the begin address of the meta data and compute the
!     end address of the number we will be collecting.
!
      call dafgda ( ctx, handle, end, end, dmtasz_1 )
      dmtasz = dmtasz_1(1)

      if ( ctx%failed() ) return

      metasz = nint ( dmtasz )
!
!     Store the actual meta size in AMETAS, in case METASZ ends up
!     being modified to conform to our current understanding of
!     meta data items.
!
      ametas = metasz

!
!     Check to see if METASZ is an unacceptable value.
!
      if ( metasz < mnmeta ) then

         value = -1
         call ctx%setmsg ( 'This segment reports that it has # meta ' &
         //            'data items. Every generic segment must ' &
         //            'have at least #.'                          )
         call ctx%errint ( '#',   metasz                               )
         call ctx%errint ( '#',   mnmeta                               )
         call ctx%sigerr ( 'SPICE(INVALIDMETADATA)'                    )
         return

!
!     If it is not, we may need to fix a few things to work around some
!     older files that have been delivered. We perform these kludges
!     here. Originally, the number of meta data items was not
!     considered to be part of the meta data. It now is, so if we
!     encounter an older version of the file, we need to increment the
!     meta data size by 1. The number of meta data items is always
!     after all of the meta data items, so we can do this.
!
      else if ( metasz == 15 ) then

         metasz = metasz + 1
         ametas = metasz

!
!     If not check to see if METASZ is greater than the known MXMETA.
!     If it is then this segment most likely was constructed from
!     some newer version of the toolkit.  Load what meta data we
!     currently know about as laid out in sgparam.inc.
!
      else if ( metasz > mxmeta ) then

!
!        Leave AMETAS alone, since we need to know how far back
!        into the DAF file to begin reading.
!
          metasz = mxmeta

      end if

!
!     The address computations that follow are precisely the same
!     as the previous version of the file, except when AMETAS is not
!     METASZ.  This only happens when METASZ is greater than MXMETA.
!
      begmta = end - ametas + 1
      endmta = begmta + metasz - 1

      call dafgda ( ctx, handle, begmta, endmta, xmeta )

      if ( ctx%failed() ) return

!
!     Convert all of the meta data values into integers.
!
      do i = 1, metasz
         meta(i) = nint ( xmeta(i) )
      end do

!
!     The kludge continues... NMETA and MXMETA are ALWAYS the same
!     value, and any missing values must appear between the last known
!     value, META(METASZ-1), and the end value, META(NMETA), so we zero
!     them out.
!
      meta(nmeta) = metasz

      do i = metasz, mxmeta-1
         meta(i) = 0
      end do

!
!     Adjust the bases so that the N'th item of a partition is at
!     address META(PARTITION_BASE) + N
!
      begm1          = begin - 1
      meta( conbas ) = meta( conbas ) + begm1
      meta( refbas ) = meta( refbas ) + begm1
      meta( rdrbas ) = meta( rdrbas ) + begm1
      meta( pdrbas ) = meta( pdrbas ) + begm1
      meta( pktbas ) = meta( pktbas ) + begm1
      meta( rsvbas ) = meta( rsvbas ) + begm1

!
!     The only acceptable integer mnemonics at this point are 1 through
!     METASZ inclusive, and NMETA.  All other requests should signal
!     the SPICE(UNKNOWNMETAITEM) error, since the current segment has
!     no knowledge of these values.
!
      if (   ( mnemon <= 0      )       .or. &
           ( ( mnemon > metasz ) .and. &
             ( mnemon /= nmeta  ) )           ) then

         value = -1
         call ctx%setmsg ( 'The item requested, #, is not one of the' &
         //            ' recognized meta data items associated' &
         //            ' with this generic segment.'               )
         call ctx%errint ( '#',   mnemon                               )
         call ctx%sigerr ( 'SPICE(UNKNOWNMETAITEM)'                    )
         return

      end if

!
!     Set the value for the desired meta data item, check out if we
!     need to, and return.
!
      value = meta ( mnemon )

      return
      end associate
      end subroutine sgmeta

      !> Evaluate a single SPK data record from a segment of type 1
      !> (Difference Lines).
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, at which a state vector is to
      !>              be computed.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of some
      !>              body, relative to some center, in some inertial
      !>              reference frame.
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state. Units are km and km/sec.
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
      !>     The exact format and structure of type 1 (difference lines)
      !>     segments are described in the SPK Required Reading file.
      !>
      !>     Difference lines (DL's) are generated by JPL navigation
      !>     system programs P and PV. Each data record is equivalent
      !>     to the (slightly rearranged) 'P' portion of a NAVIO PV file
      !>     data record.
      !>
      !>     SPKE01 is a specialized version of Fred Krogh's subroutine DAINT.
      !>     Only the calling sequence has been changed.
      !>
      !>     Because the original version was undocumented, only Fred
      !>     knows how this really works.
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     F.T. Krogh         (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.1.0, 14-FEB-1997 (WLT)
      !>
      !>        The goto's were removed and loop and if structures
      !>        revealed. We still don't know exactly what's going
      !>        on, but at least the bones of this routine have been
      !>        cleaned off and are ready for assembly. (WLT)
      !>
      !>-    SPICELIB Version 1.0.4, 30-OCT-1996 (WLT)
      !>
      !>        Removed redundant SAVE statements from the declaration
      !>        section. Thanks to Steve Schlaifer for finding this
      !>        error.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
      !>
      !>        SPK01 was removed from the $Required_Reading section of the
      !>        header. The information in the SPK01 Required Reading file
      !>        is now part of the SPK Required Reading file.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (FTK)
      module subroutine spke01 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity).
      real(wp) :: tl_1(1)  !! one-element copy of tl (array argument)
      integer(ip) :: j
      real(wp) :: delta
      integer(ip) :: ks
      integer(ip) :: i
      integer(ip) :: kqmax1
      integer(ip) :: ks1
      integer(ip) :: mq2
      real(wp) :: tp
      real(wp) :: tl
      real(wp) :: sum
      real(wp) :: dt       ( 15,3 )
      real(wp) :: g        (   15 )
      integer(ip) :: kqq
      integer(ip) :: jx
      real(wp) :: fc       (   14 )
      real(wp) :: refpos   (    3 )
      integer(ip) :: kq       (    3 )
      real(wp) :: wc       (   13 )
      real(wp) :: refvel   (    3 )
      real(wp) :: w        (   17 )
      if ( ctx%failed() ) return

!     PEPPER: FC(1) is always 1 (SPICELIB set it with DATA); the other
!     elements are computed below.
      fc(1) = 1.0_wp

!
!     Unpack the contents of the MDA array.
!
!        Name    Dimension  Description
!        ------  ---------  -------------------------------
!        TL              1  Final epoch of record
!        G              15  Stepsize function vector
!        REFPOS          3  Reference position vector
!        REFVEL          3  Reference velocity vector
!        DT         15,NTE  Modified divided difference arrays
!        KQMAX1          1  Maximum integration order plus 1
!        KQ            NTE  Integration order array
!
!     For our purposes, NTE is always 3.
!
      tl_1 = record(1:1)
      tl = tl_1(1)
      g = record(2:16)
!
!     Collect the reference position and velocity.
!
      refpos(1) =  record(17)
      refvel(1) =  record(18)

      refpos(2) =  record(19)
      refvel(2) =  record(20)

      refpos(3) =  record(21)
      refvel(3) =  record(22)

      dt = reshape(record(23:67), shape(dt))

      kqmax1    = int ( record(68) )
      kq(1)     = int ( record(69) )
      kq(2)     = int ( record(70) )
      kq(3)     = int ( record(71) )

!
!     Next we set up for the computation of the various differences
!
      delta     = et     - tl
      tp        = delta
      mq2       = kqmax1 - 2
      ks        = kqmax1 - 1

!
!     This is clearly collecting some kind of coefficients.
!     The problem is that we have no idea what they are...
!
!     The G coefficients are supposed to be some kind of step size
!     vector.
!
!     TP starts out as the delta t between the request time
!     and the time for which we last had a state in the MDL file.
!     We then change it from DELTA  by the components of the stepsize
!     vector G.
!
      do j = 1, mq2
         fc(j+1) = tp    / g(j)
         wc(j)   = delta / g(j)
         tp      = delta + g(j)
      end do

!
!     Collect KQMAX1 reciprocals.
!
      do j = 1, kqmax1
         w(j) = 1.0_wp/real(j, wp)
      end do

!
!     Compute the W(K) terms needed for the position interpolation
!     (Note,  it is assumed throughout this routine that KS, which
!     starts out as KQMAX1-1 (the ``maximum integration'')
!     is at least 2.
!
      jx  = 0
      ks1 = ks - 1

      do while ( ks >= 2 )

         jx  = jx+1

         do  j = 1, jx
            w(j+ks) = fc(j+1)*w(j+ks1) - wc(j)*w(j+ks)
         end do

         ks  = ks1
         ks1 = ks1 - 1

      end do

!
!     Perform position interpolation: (Note that KS = 1 right now.
!     We don't know much more than that.)
!
      do  i = 1, 3

         kqq = kq(i)
         sum = 0.0_wp

         do j = kqq, 1, -1
            sum = sum + dt(j,i)*w(j+ks)
         end do

         state(i) = refpos(i)  +  delta * ( refvel(i) + delta*sum )

      end do

!
!     Again we need to compute the W(K) coefficients that are
!     going to be used in the velocity interpolation.
!     (Note, at this point, KS = 1, KS1 = 0.)
!
      do  j = 1, jx
         w(j+ks) = fc(j+1)*w(j+ks1) - wc(j)*w(j+ks)
      end do

      ks = ks - 1

!
!     Perform velocity interpolation:
!
      do  i = 1, 3

         kqq = kq(i)
         sum = 0.0_wp

         do j = kqq, 1, -1
            sum = sum + dt(j,i)*w(j+ks)
         end do

         state(i+3) = refvel(i) + delta*sum

      end do

!
!     That's all folks.  We don't know why we did anything, but
!     at least we can tell structurally what we did.
!

      return
      end subroutine spke01

      !> Evaluate a single data record from an PCK or SPK segment of type
      !> 2 (Chebyshev Polynomials, 3 components).
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch at which a state vector or Euler angle
      !>              state is to be computed. The epoch is represented as
      !>              seconds past J2000 TDB.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will yield three function components and their
      !>              derivatives with respect to time. The record
      !>              structure for SPK type 2 data is:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z position component      |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Position components have units of km.
      !>
      !>              See PCKE02 for a description of PCK type 2 records.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by this routine. See the INCLUDE file
      !>              spkrec.inc for the correct record length.
      !>
      !>## Detailed Output
      !>
      !>     XYZDOT   is a 6-vector. In order, the components of XYZDOT are
      !>              X, Y, Z, X', Y', and Z'. Units for state evaluations
      !>              will be km and km/sec. Units for angles will be
      !>              radians and radians/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input record contains an invalid coefficient count,
      !>         the error SPICE(INVALIDCOUNT) is signaled.
      !>
      !>     2)  If the input record contains invalid domain transformation
      !>         parameters, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 2 (Chebyshev polynomials,
      !>     position only) segments are described in the SPK and PCK Required
      !>     Reading files.
      !>
      !>     A type 2 segment contains three sets of Chebyshev coefficients,
      !>     one set each for components X, Y, and Z. SPKE02 calls the routine
      !>     CHBINT for each set to evaluate the polynomial AND its first
      !>     derivative (which it computes internally) at the input epoch,
      !>     thereby arriving at the complete state.
      !>
      !>## Examples
      !>
      !>     The data returned by the routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely.
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
      !>           IF ( TYPE .EQ. 2 ) THEN
      !>
      !>              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE02 ( ET, RECORD, XYZDOT )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 2.0.0, 18-JAN-2014 (NJB)
      !>
      !>        Added error checks for invalid coefficient counts
      !>        and invalid interval radius. Changed error handling
      !>        style to "discovery." Enhanced header documentation.
      !>
      !>-    SPICELIB Version 1.0.4, 22-MAR-1994 (KSZ)
      !>
      !>     Comments changed so this can be used as
      !>     a generic Chebyshev evaluator, rather than just for
      !>     SPK type 2 files.  (KSZ)
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
      !>
      !>        SPK02 was removed from the $Required_Reading section of the
      !>        header. The information in the SPK02 Required Reading file
      !>        is now part of the SPK Required Reading file.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
      module subroutine spke02 ( ctx, et, record, xyzdot )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: chbint
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: xyzdot   ( 6 )  !! Three function components and their derivatives.
      integer(ip) :: cofloc
      integer(ip) :: degp
      integer(ip) :: i
      integer(ip) :: ncof

      if ( ctx%failed() ) return

!
!     The first number in the record is the record size.  Following it
!     are two numbers that will be used later, then the three sets of
!     coefficients.  The number of coefficients for each variable can
!     be determined from the record size, since there are the same
!     number of coefficients for each variable.
!
      ncof = ( int( record( 1 ) ) - 2 ) / 3

      if ( ncof < 1 ) then

         call ctx%setmsg ( 'The input record''s coefficient count NCOF ' &
         //            'should be positive but was #.'              )
         call ctx%errint ( '#', ncof                                    )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     Check the radius of the domain interval.
!
      if ( record(3) <= 0.0_wp )  then

         call ctx%setmsg ( 'Interval radius must be positive ' &
         //            'but was #.'                        )
         call ctx%errdp  ( '#', record(3)                      )
         call ctx%sigerr ( 'SPICE(INVALIDRADIUS)'              )
         return

      end if

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degp = ncof - 1

!
!     Call CHBINT once for each variable to evaluate the position
!     and velocity values.
!
       do i = 1, 3
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         cofloc = ncof*( i - 1 ) + 4
!
!        CHBINT needs as input the coefficients, the degree of the
!        polynomial, the epoch, and also two variable transformation
!        parameters, which are located, in our case, in the second and
!        third slots of the record.
!
!        Note that CHBINT is "error free."
!
         call chbint ( record( cofloc ),  degp,       record( 2 ), &
                       et,                xyzdot(i),  xyzdot(i + 3)  )
      end do

      return
      end subroutine spke02

      !> Evaluate a single SPK data record from a segment of type 3
      !> (Chebyshev Polynomials, position and velocity).
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch at which a state vector or Euler angle
      !>              state is to be computed. The epoch is represented as
      !>              seconds past J2000 TDB.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will yield three function components and their
      !>              derivatives with respect to time. The record
      !>              structure for SPK type 3 data is:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z velocity component      |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Position components have units of km.
      !>                 - Velocity components have units of km/s.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by this routine. See the INCLUDE file
      !>              spkrec.inc for the correct record length.
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state. In order, X, Y, Z, X', Y', and Z'.
      !>              Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input record contains an invalid coefficient count,
      !>         the error SPICE(INVALIDCOUNT) is signaled.
      !>
      !>     2)  If the input record contains invalid domain transformation
      !>         parameters, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 3 (Chebyshev polynomials,
      !>     position and velocity) segments are described in the SPK
      !>     Required Reading file.
      !>
      !>     A type 3 segment contains six sets of Chebyshev coefficients,
      !>     one set each for the position coordinates X, Y, and Z, and one
      !>     set each for the velocity coordinates X', Y', and Z'.  SPKE03
      !>     calls the routine CHBVAL to evaluate each polynomial, and arrive
      !>     at the complete state.
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
      !>           IF ( TYPE .EQ. 3 ) THEN
      !>
      !>              CALL SPKR03 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE03 ( ET, RECORD, STATE )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 2.0.0, 31-DEC-2013 (NJB)
      !>
      !>        Added error checks for invalid coefficient counts
      !>        and invalid interval radius. Changed error handling
      !>        style to "discovery." Enhanced header documentation.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
      !>
      !>        SPK03 was removed from the $Required_Reading section of the
      !>        header. The information in the SPK03 Required Reading file
      !>        is now part of the SPK Required Reading file.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
      module subroutine spke03 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: chbval
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity).

      integer(ip) :: ncof
      integer(ip) :: degp
      integer(ip) :: cofloc
      integer(ip) :: i

      if ( ctx%failed() ) return

!
!     The first number in the record is the record size.  Following it
!     are two numbers that will be used later, then the six sets of
!     coefficients.  The number of coefficients for each quantity can
!     be determined from the record size, since there are the same
!     number of coefficients for each quantity.
!
      ncof = ( int( record( 1 ) ) - 2 ) / 6

      if ( ncof < 1 ) then

         call ctx%setmsg ( 'The input record''s coefficient count NCOF ' &
         //            'should be positive but was #.'              )
         call ctx%errint ( '#', ncof                                    )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     Check the radius of the domain interval.
!
      if ( record(3) <= 0.0_wp )  then

         call ctx%setmsg ( 'Interval radius must be positive ' &
         //            'but was #.'                        )
         call ctx%errdp  ( '#', record(3)                      )
         call ctx%sigerr ( 'SPICE(INVALIDRADIUS)'              )
         return

      end if

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degp = ncof - 1

!
!     Call CHBVAL once for each quantity to evaluate the position
!     and velocity values.
!
      do i = 1, 6
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         cofloc = ncof*( i - 1 ) + 4

!
!        CHBVAL needs as input the coefficients, the degree of the
!        polynomial, the epoch, and also two variable transformation
!        parameters, which are located, in our case, in the second and
!        third slots of the record.
!
         call chbval ( record( cofloc ), degp, record( 2 ), et, &
                       state( i )                               )
      end do

      return
      end subroutine spke03

      !> Evaluate a single SPK data record from a segment of type 8
      !> (equally spaced discrete states, interpolated by Lagrange
      !> polynomials).
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, at which a state vector is to
      !>              be computed.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of some
      !>              body, relative to some center, in some inertial
      !>              reference frame. Normally, the caller of this routine
      !>              will obtain RECORD by calling SPKR08.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | start epoch          |
      !>                 +----------------------+
      !>                 | step size            |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state. In order, the elements are
      !>
      !>                 X, Y, Z, X', Y', and Z'
      !>
      !>              Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The caller of this routine must ensure that the input record
      !>         is appropriate for the supplied ET value. Otherwise,
      !>         arithmetic overflow may result.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 8 (equally spaced discrete
      !>     states, interpolated by Lagrange polynomials) segments are
      !>     described in the SPK Required Reading file.
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
      !>           IF ( TYPE .EQ. 8 ) THEN
      !>
      !>              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE08 ( ET, RECORD, STATE )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 10-DEC-2013 (NJB)
      !>
      !>        RECORD is now strictly an input; it is not overwritten by this
      !>        routine. Formerly RECORD was used as a workspace array.
      !>
      !>-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in XPOSEG and LGRESP calls.
      !>
      !>-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in XPOSEG and LGRESP calls.
      module subroutine spke08 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spkrec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(in) :: record ( * )  !! Data record.
      real(wp), intent(out) :: state  ( 6 )  !! State (position and velocity).
!
!     Indices of input record elements:
!
!        -- size
!        -- start epoch
!        -- step size
!        -- start of state information
!
      integer(ip), parameter :: begidx = 2
      integer(ip), parameter :: stpidx = 3
!
!     Local variables
!
      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: ystart
!
!     Save arrays to prevent stack overflow problems on some
!     platforms.
!
      real(wp) :: locrec ( maxrec )
!
!     Initialize the workspace array to suppress compiler warnings.
!
      real(wp) :: work   ( maxrec )
      if ( ctx%failed() ) return

!
!     We'll transpose the state information in the input record
!     so that contiguous pieces of it can be shoved directly into the
!     interpolation routine LGRESP.
!
      n   =  nint( record(sizidx) )

      locrec(1:n*statsz) = reshape(transpose(reshape(record(staidx:staidx+statsz*n-1), [statsz, n])), [n*statsz])

!
!     We interpolate each state component in turn.
!
      do i = 1, statsz

         ystart    =   1  +  ( (i-1) * n )

         state(i)  =   lgresp ( ctx, n, &
                                record(begidx), &
                                record(stpidx), &
                                locrec(ystart), &
                                work, &
                                et              )
      end do

      return
      end subroutine spke08

      !> Evaluate a single SPK data record from a segment of type 9
      !> (discrete states, evaluated by Lagrange interpolation).
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, at which a state vector is to
      !>              be computed.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of some
      !>              body, relative to some center, in some inertial
      !>              reference frame. Normally, the caller of this routine
      !>              will obtain RECORD by calling SPKR09.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the input record, modified by use as a work area.
      !>              On output, RECORD no longer contains useful
      !>              information.
      !>
      !>     STATE    is the state. In order, the elements are
      !>
      !>                 X, Y, Z, X', Y', and Z'
      !>
      !>              Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  The caller of this routine must ensure that the input record
      !>         is appropriate for the supplied ET value. Otherwise,
      !>         arithmetic overflow may result.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 9 (unequally spaced
      !>     discrete states, evaluated by Lagrange interpolation) segments are
      !>     described in the SPK Required Reading file.
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
      !>           IF ( TYPE .EQ. 9 ) THEN
      !>
      !>              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE09 ( ET, RECORD, STATE )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in XPOSEG and LGRINT calls.
      !>
      !>-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB) (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in XPOSEG and LGRINT calls.
      module subroutine spke09 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spkrec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(inout) :: record ( * )  !! Data record.
      real(wp), intent(out) :: state  ( 6 )  !! State (position and velocity).


!
!     Indices of input record elements:
!
!        -- size
!        -- start of state information
!

      integer(ip), parameter :: staidx = 2

      real(wp) :: locrec ( maxrec )

      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: xstart
      integer(ip) :: ystart

      if ( ctx%failed() ) return

!
!     We'll transpose the state information in the input record
!     so that contiguous pieces of it can be shoved directly into the
!     interpolation routine LGRINT.  We allow LGRINT to overwrite the
!     state values in the input record, since this saves local storage
!     and does no harm.  (See the header of LGRINT for a description of
!     its work space usage.)
!
      n   =  nint( record(sizidx) )

      locrec(1:n*statsz) = reshape(transpose(reshape(record(staidx:staidx+statsz*n-1), [statsz, n])), [n*statsz])
      record(staidx:staidx+statsz*n-1) = locrec(1:statsz*n)

!
!     We interpolate each state component in turn.
!
      xstart   =   2   +  n * statsz

      do i = 1, statsz

         ystart    =   2   +  n * (i-1)

         state(i)  =   lgrint ( ctx, n, &
                                record(xstart), &
                                record(ystart), &
                                locrec, &
                                et              )

      end do

      return
      end subroutine spke09

      !> Evaluate a single data record from a type 12 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch for which a state vector is desired.
      !>
      !>     RECORD   is a record from a type 12 SPK segment which, when
      !>              evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative to
      !>              some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | start epoch          |
      !>                 +----------------------+
      !>                 | step size            |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state vector at epoch ET. Its contents are, in
      !>              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     MAXREC   is the maximum size of SPK record. See the SPICELIB
      !>              routine SPKPVN for details.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while interpolating the SPK data, the
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 12 (unequally spaced
      !>     discrete states, evaluated by Hermite interpolation) SPK segments
      !>     is described in the SPK Required Reading.
      !>
      !>## Examples
      !>
      !>     The SPKEnn routines are almost always used in conjunction with
      !>     the corresponding SPKRnn routines, which read the records from
      !>     SPK files.
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch before evaluating the record to obtain a
      !>     state vector, as in the example which follows.
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
      !>
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 12 ) THEN
      !>
      !>              CALL SPKR12 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE12 ( ET, RECORD, STATE )
      !>                  .
      !>                  .  Check out the evaluated state.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that the input record is valid. Any
      !>         checking of the input data is assumed to have been performed
      !>         when the source SPK file was created.
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
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Exceptions section to describe possible issues detected by
      !>        this routine. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-1999 (NJB)
      module subroutine spke12 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch for which a state is desired.
      real(wp), intent(in) :: record   ( * )  !! Record from a type 12 SPK segment valid for ET.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity) at epoch ET.



      integer(ip), parameter :: stridx = sizidx + 1

      integer(ip), parameter :: stpidx = stridx + 1

      integer(ip), parameter :: staidx = stpidx + 1

      real(wp) :: locrec ( maxrec )
      real(wp) :: work   ( maxrec * 2,  2 )

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: to
      integer(ip) :: xstart

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      n =  nint( record(sizidx) )

!
!     We interpolate each state component in turn.
!
      xstart   =   2  +  6 * n

      do i = 1, 3

         do j = 1, n
!
!           For the Jth input state vector, copy the Ith position and
!           velocity components into the local record buffer LOCREC.
!
            from         = staidx - 1 + 6*(j-1) + i
            to           = 2*j    - 1

            locrec(to  ) = record ( from     )
            locrec(to+1) = record ( from + 3 )

         end do

!
!        Interpolate the Ith position and velocity components of the
!        state.
!
         call hrmesp ( ctx, n, &
                       record(stridx), &
                       record(stpidx), &
                       locrec, &
                       et, &
                       work, &
                       state(i  ), &
                       state(i+3)      )

      end do

      return
      end subroutine spke12

      !> Evaluate a single data record from a type 13 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch for which a state vector is desired.
      !>
      !>     RECORD   is a record from a type 13 SPK segment which, when
      !>              evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative to
      !>              some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state vector at epoch ET. Its contents are, in
      !>              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     MAXREC   is the maximum size of SPK record. See the SPICELIB
      !>              routine SPKPVN for details.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while interpolating the SPK data, the
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 13 (unequally spaced
      !>     discrete states, evaluated by Hermite interpolation) SPK segments
      !>     is described in the SPK Required Reading.
      !>
      !>## Examples
      !>
      !>     The SPKEnn routines are almost always used in conjunction with
      !>     the corresponding SPKRnn routines, which read the records from
      !>     SPK files.
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch before evaluating the record to obtain a
      !>     state vector, as in the example which follows.
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
      !>
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 13 ) THEN
      !>
      !>              CALL SPKR13 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE13 ( ET, RECORD, STATE )
      !>                  .
      !>                  .  Check out the evaluated state.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that the input record is valid. Any
      !>         checking of the input data is assumed to have been performed
      !>         when the source SPK file was created.
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
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Exceptions section to describe possible issues detected by
      !>        this routine. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
      module subroutine spke13 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch for which a state is desired.
      real(wp), intent(in) :: record   ( * )  !! Record from a type 13 SPK segment valid for ET.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity) at epoch ET.



      real(wp) :: locrec ( maxrec )
      real(wp) :: work   ( maxrec * 2,  2 )

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: to
      integer(ip) :: xstart

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      n =  nint( record(sizidx) )

!
!     We interpolate each state component in turn.
!
      xstart   =   2  +  6 * n

      do i = 1, 3

         do j = 1, n
!
!           For the Jth input state vector, copy the Ith position and
!           velocity components into the local record buffer LOCREC.
!
            from         = 1 + 6*(j-1) + i
            to           =     2* j    - 1

            locrec(to  ) = record ( from     )
            locrec(to+1) = record ( from + 3 )

         end do

!
!        Interpolate the Ith position and velocity components of the
!        state.
!
         call hrmint ( ctx, n, &
                       record(xstart), &
                       locrec, &
                       et, &
                       work, &
                       state(i  ), &
                       state(i+3)      )

      end do

      return
      end subroutine spke13

      !> Evaluate a single data record from a type 14 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch for which a state vector is desired.
      !>
      !>     RECORD   is a record from a type 14 SPK segment which, when
      !>              evaluated at epoch ET, will give the state (position
      !>              and velocity) of some body, relative to some center, in
      !>              some inertial reference frame.
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state vector at epoch ET. Its contents are, in
      !>              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
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
      !>     The exact format and structure of a type 14 SPK segment is
      !>     described in the SPK Required Reading.
      !>
      !>     A type 14 record contains six sets of Chebyshev coefficients,
      !>     one set each for the position coordinates X, Y, and Z, and one
      !>     set each for the velocity coordinates X', Y', and Z' of a state
      !>     vector.  SPKE14 calls the routine CHBVAL to evaluate each
      !>     Chebyshev polynomial, and arrive at the complete state.
      !>
      !>## Examples
      !>
      !>     The SPKEnn routines are almost always used in conjunction with
      !>     the corresponding SPKRnn routines, which read the records from
      !>     SPK files.
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch before evaluating the record to obtain a
      !>     state vector, as in the example which follows.
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
      !>
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 14 ) THEN
      !>
      !>              CALL SPKR14 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE14 ( ET, RECORD, STATE )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 10-MAR-1995 (KRG)
      module subroutine spke14 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: chbval
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch for which a state is desired.
      real(wp), intent(in) :: record   ( * )  !! Record from a type 14 SPK segment valid for ET.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity) at epoch ET.

      integer(ip) :: cofloc
      integer(ip) :: degree
      integer(ip) :: i
      integer(ip) :: ncoeff

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The first number in the record is the number of Chebyshev
!     Polynomial coefficients used to represent each component of the
!     state vector. Following it are two numbers that will be used
!     when evaluating the sets of coefficients, and finally the six sets
!     of coefficients.
!
      ncoeff = int( record( 1 ) )

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degree = ncoeff - 1

!
!     Call CHBVAL once for each quantity to evaluate the position
!     and velocity values.
!
      do i = 1, 6
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         cofloc = ncoeff * ( i - 1 ) + 4
!
!        CHBVAL needs as input the coefficients, the degree of the
!        polynomial, also two variable transformation parameters, which
!        are located in the second and third slots of the record, and
!        the epoch. We get back the appropriate element of a state
!        vector.
!
         call chbval ( record(cofloc), degree, record(2), et, state(i) )

      end do

      return

      end subroutine spke14

      !> Evaluate a single SPK data record from a segment of type 17
      !> (Equinoctial Elements).
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, specified as ephemeris seconds past
      !>              J2000, at which a state vector is to be computed.
      !>
      !>     RECIN    is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of some
      !>              body, relative to some center, in some inertial
      !>              reference frame.
      !>
      !>              The structure of RECIN is:
      !>
      !>                 RECIN (1)  epoch of the elements in ephemeris seconds
      !>                            past J2000.
      !>
      !>                 RECIN (2)-RECIN (10) Equinoctial Elements:
      !>
      !>                    RECIN (2)  is the semi-major axis (A) of the orbit.
      !>
      !>                    RECIN (3)  is the value of H at the specified
      !>                               epoch. ( E*SIN(ARGP+NODE) ).
      !>
      !>                    RECIN (4)  is the value of K at the specified epoch
      !>                               ( E*COS(ARGP+NODE) ).
      !>
      !>                    RECIN (5)  is the mean longitude (MEAN0+ARGP+NODE)
      !>                               at the epoch of the elements.
      !>
      !>                    RECIN (6)  is the value of P
      !>                               (TAN(INC/2)*SIN(NODE)) at the specified
      !>                               epoch.
      !>
      !>                    RECIN (7)  is the value of Q
      !>                               (TAN(INC/2)*COS(NODE)) at the specified
      !>                               epoch.
      !>
      !>                    RECIN (8)  is the rate of the longitude of periapse
      !>                               (dARGP/dt + dNODE/dt ) at the epoch of
      !>                               the elements. This rate is assumed to
      !>                               hold for all time.
      !>
      !>                    RECIN (9)  is the derivative of the mean longitude
      !>                               ( dM/dt + dARGP/dt + dNODE/dt ).  This
      !>                               rate is assumed to be constant.
      !>
      !>                    RECIN (10)  is the rate of the longitude of the
      !>                                ascending node ( dNODE/dt).
      !>
      !>                 RECIN (11) Right Ascension of the pole of the
      !>                            orbital reference system relative to the
      !>                            reference frame of the associated SPK
      !>                            segment.
      !>
      !>                 RECIN (12) Declination of the pole of the
      !>                            orbital reference system relative to
      !>                            the reference frame of the associated
      !>                            SPK segment.
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state produced by evaluating RECIN at ET.
      !>              Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the eccentricity is greater than 0.9, the error
      !>         SPICE(BADECCENTRICITY) is signaled.
      !>
      !>     2)  If the semi-major axis is non-positive, the error
      !>         SPICE(BADSEMIAXIS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine performs a cursory examination of the elements
      !>     of a type 17 SPK data record and then passes the equinoctial
      !>     elements contained in that record on to the SPICE routine
      !>     EQNCPV for evaluation.
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
      !>           IF ( TYPE .EQ. 17 ) THEN
      !>
      !>              CALL SPKR17 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE17 ( ET, RECORD, STATE )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT)
      module subroutine spke17 ( ctx, et, recin, state )
      use pepper_kinds, only: wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(in) :: recin  ( * )  !! Data record.
      real(wp), intent(out) :: state  ( 6 )  !! State (position and velocity).

      real(wp) :: a
      real(wp) :: rapole
      real(wp) :: decpol
      real(wp) :: h
      real(wp) :: k
      real(wp) :: ecc
      real(wp) :: epoch

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Fetch the various entities from the input record, first the epoch.
!
      epoch  = recin(1)
      a      = recin(2)
      h      = recin(3)
      k      = recin(4)
      ecc    = dsqrt( h*h + k*k )

      rapole = recin(11)
      decpol = recin(12)

!
!     Check all the inputs here for obvious failures.  Yes, perhaps
!     this is overkill.  However, there is a lot more computation
!     going on in this routine so that the small amount of overhead
!     here should not be significant.
!
      if ( a <= 0 ) then

         call ctx%setmsg ( 'The semi-major axis supplied to the ' &
         //            'SPK type 17 evaluator was non-positive.  This ' &
         //            'value must be positive. The value supplied was ' &
         //            '#.'                    )
         call ctx%errdp  ( '#', a                  )
         call ctx%sigerr ( 'SPICE(BADSEMIAXIS)'    )
         return

      else if ( ecc > 0.9_wp ) then

         call ctx%setmsg ( 'The eccentricity supplied for a type 17 ' &
         //            'segment is greater than 0.9.  It must be ' &
         //            'less than 0.9.' &
         //            'The value supplied ' &
         //            'to the type 17 evaluator was #. ' )
         call ctx%errdp  ( '#',   ecc                )
         call ctx%sigerr ( 'SPICE(BADECCENTRICITY)'  )
         return

      end if

!
!     That's all for here, just plug the elements into the routine
!     knows how to evaluate the equinoctial elements.
!
      call eqncpv ( ctx, et, epoch, recin(2), rapole, decpol, state )

!
!     That's all folks.  return.
!
      return
      end subroutine spke17

      !> Evaluate a single data record from a type 18 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch for which a state vector is desired.
      !>
      !>     RECORD   is a record from a type 18 SPK segment which, when
      !>              evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative to
      !>              some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                          .
      !>                          .
      !>                          .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state vector at epoch ET. Its contents are, in
      !>              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     MAXREC   is the maximum size of SPK record. See the SPICELIB
      !>              routine SPKPVN for details.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that the input record is valid.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 18 (MEX/Rosetta Orbit
      !>     file interpolation) SPK segments is described in the SPK
      !>     Required Reading.
      !>
      !>## Examples
      !>
      !>     The SPKEnn routines are almost always used in conjunction with
      !>     the corresponding SPKRnn routines, which read the records from
      !>     SPK files.
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch before evaluating the record to obtain a
      !>     state vector, as in the example which follows.
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
      !>
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 18 ) THEN
      !>
      !>              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE18 ( ET, RECORD, STATE )
      !>                  .
      !>                  .  Check out the evaluated state.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that the input record is valid. Any
      !>         checking of the input data is assumed to have been performed
      !>         when the source SPK file was created.
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
      !>-    SPICELIB Version 1.1.1, 16-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 05-NOV-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in XPOSEG and LGRINT calls.
      !>
      !>-    SPICELIB Version 1.0.0, 17-AUG-2002 (NJB)
      module subroutine spke18 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: xpsgip
      use pepper_inc_spk18
      use pepper_inc_spkrec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch for which a state is desired.
      real(wp), intent(inout) :: record   ( * )  !! Record from a type 18 SPK segment valid for ET. (PEPPER: its packets are transposed in place.)
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity) at epoch ET.

      real(wp) :: locrec ( maxrec )
      real(wp) :: work   ( maxrec * 2,  2 )
      real(wp) :: vbuff  ( 6 )

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: packsz
      integer(ip) :: subtyp
      integer(ip) :: to
      integer(ip) :: xstart
      integer(ip) :: ystart

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      subtyp =  nint( record(sbtidx) )

      if ( subtyp == s18tp0 ) then

         packsz = s18ps0

      else if ( subtyp == s18tp1 ) then

         packsz = s18ps1

      else

         call ctx%setmsg ( 'Unexpected SPK type 18 subtype found in ' // &
                       'type 18 record.'                          )
         call ctx%errint ( '#',  subtyp                               )
         call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                      )
         return

      end if
!
!     Get the packet count.
!
      n = nint( record(cntidx) )

      if ( subtyp == s18tp1 ) then
!
!        This is the easy case:  we perform Lagrange interpolation
!        on each state component.
!
!        We'll transpose the state information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.
!
         n   =  nint( record(cntidx) )

         call xpsgip ( packsz, n, record(pktidx) )

!
!        We interpolate each state component in turn.
!
         xstart   =   3   +  n * packsz

         do i = 1, packsz

            ystart    =   3   +  n * (i-1)

            state(i)  =   lgrint ( ctx, n, &
                                   record(xstart), &
                                   record(ystart), &
                                   locrec, &
                                   et              )
         end do

      else

!
!        We interpolate each state component in turn.  Position and
!        velocity are interpolated separately.
!
         xstart   =   3  +  packsz * n

         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2 + packsz*(j-1) + i
               to           =     2 * j        - 1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith position and velocity components of the
!           state.  We'll keep the position and overwrite the velocity.
!
            call hrmint ( ctx, n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          state(i  ), &
                          state(i+3)      )

         end do

!
!        Now interpolate velocity, using separate velocity data and
!        acceleration.
!
         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2  +  packsz*(j-1)  +  packsz/2  +  i
               to           =       2 * j         -  1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith velocity and acceleration components of
!           the state.  We'll capture the result in a temporary buffer,
!           then transfer the velocity to the output state array.
!
            call hrmint ( ctx, n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          vbuff(i  ), &
                          vbuff(i+3)     )

         end do

!
!        Fill in the velocity in the output state using the results of
!        interpolating velocity and acceleration.
!
         state(4:6) = vbuff(1:3)

      end if

      return
      end subroutine spke18

      !> Evaluate a single data record from a type 19 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch for which a state vector is desired,
      !>              expressed as seconds past J2000 TDB.
      !>
      !>     RECORD   is a record from a type 19 SPK segment which, when
      !>              evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative to
      !>              some center, in the reference frame associated
      !>              with the data packets. Usually the body, center
      !>              and frame are those of the SPK segment from which
      !>              the packets were read.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                          .
      !>                          .
      !>                          .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epoch 1              |
      !>                 +----------------------+
      !>                          .
      !>                          .
      !>                          .
      !>                 +----------------------+
      !>                 | epoch n              |
      !>                 +----------------------+
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state vector at epoch ET. Its contents are, in
      !>              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     MAXREC   is the maximum size of SPK record. See the SPICELIB
      !>              routine SPKPVN for details.
      !>
      !>## Exceptions
      !>
      !>     1)  Most types of errors in the input record cannot be diagnosed
      !>         by this routine. This routine assumes that the input record
      !>         is valid.
      !>
      !>     2)  If the subtype code in the input record is invalid, an error
      !>         is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 19 (ESOC/DDID piecewise
      !>     interpolation) SPK segments is described in the SPK Required
      !>     Reading.
      !>
      !>## Examples
      !>
      !>     The SPKEnn routines are almost always used in conjunction with
      !>     the corresponding SPKRnn routines, which read the records from
      !>     SPK files.
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch before evaluating the record to obtain a
      !>     state vector, as in the example that follows.
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
      !>
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 19 ) THEN
      !>
      !>              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE19 ( ET, RECORD, STATE )
      !>                  .
      !>                  .  Check out the evaluated state.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that the input record is valid. Any
      !>         checking of the input data is assumed to have been performed
      !>         when the source SPK file was created.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
      !>
      !>        Updated to support subtype 2. Now performs
      !>        computations in-line, rather than calling
      !>        SPKE18.
      !>
      !>-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
      module subroutine spke19 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: xpsgip
      use pepper_inc_spk19
      use pepper_inc_spkrec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch for which a state is desired.
      real(wp), intent(inout) :: record   ( * )  !! Record from a type 19 SPK segment valid for ET. (PEPPER: its packets are transposed in place.)
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity) at epoch ET.

      real(wp) :: locrec ( maxrec )
      real(wp) :: work   ( maxrec * 2,  2 )
      real(wp) :: vbuff  ( 6 )

      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: packsz
      integer(ip) :: subtyp
      integer(ip) :: to
      integer(ip) :: xstart
      integer(ip) :: ystart

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      subtyp =  nint( record(sbtidx) )

      if ( subtyp == s19tp0 ) then

         packsz = s19ps0

      else if ( subtyp == s19tp1 ) then

         packsz = s19ps1

      else if ( subtyp == s19tp2 ) then

         packsz = s19ps2

      else

         call ctx%setmsg ( 'Unexpected SPK type 19 subtype found in ' &
         //            'type 19 record.'                          )
         call ctx%errint ( '#',  subtyp                               )
         call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                      )
         return

      end if

!
!     Get the packet count.
!
      n = nint( record(cntidx) )

      if ( subtyp == s19tp0 ) then
!
!        We interpolate each state component in turn.  Position and
!        velocity are interpolated separately.
!
         xstart = 3  +  packsz * n

         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2 + packsz*(j-1) + i
               to           =     2 * j        - 1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith position and velocity components of the
!           state.  We'll keep the position and overwrite the velocity.
!
            call hrmint ( ctx, n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          state(i  ), &
                          state(i+3)      )

         end do

!
!        Now interpolate velocity, using separate velocity data and
!        acceleration.
!
         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2  +  packsz*(j-1)  +  packsz/2  +  i
               to           =       2 * j         -  1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith velocity and acceleration components of
!           the state.  We'll capture the result in a temporary buffer,
!           then transfer the velocity to the output state array.
!
            call hrmint ( ctx, n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          vbuff(i  ), &
                          vbuff(i+3)     )
         end do

!
!        Fill in the velocity in the output state using the results of
!        interpolating velocity and acceleration.
!
         state(4:6) = vbuff(1:3)

      else if ( subtyp == s19tp1 ) then
!
!        We perform Lagrange interpolation on each state component.
!
!        We'll transpose the state information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.
!
         call xpsgip ( packsz, n, record(pktidx) )

!
!        We interpolate each state component in turn.
!
         xstart   =   3   +  n * packsz

         do i = 1, packsz

            ystart    =   3   +  n * (i-1)

            state(i)  =   lgrint ( ctx, n, &
                                   record(xstart), &
                                   record(ystart), &
                                   locrec, &
                                   et              )
         end do

      else if ( subtyp == s19tp2 ) then
!
!        We perform Hermite interpolation on each position component
!        and corresponding velocity component.
!
         xstart   =   3   +  n * packsz

         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2 + packsz*(j-1) + i
               to           =     2 * j        - 1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith position and velocity components of the
!           state.
!
            call hrmint ( ctx, n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          state(i), &
                          state(i+3)      )
         end do

      else
!
!         This is a backstop case.
!
         call ctx%setmsg ( 'Unexpected SPK type 19 subtype found in ' &
         //            'type 19 record.'                          )
         call ctx%errint ( '#',  subtyp                               )
         call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                      )
         return

      end if

      return
      end subroutine spke19

      !> Evaluate a single data record from an SPK or PCK segment of type
      !> 20 (Chebyshev polynomials, velocity only).
      !>
      !>## Detailed Input
      !>
      !>     ET       is the epoch at which a state vector or Euler angle
      !>              state is to be computed. The epoch is represented as
      !>              seconds past J2000 TDB.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will yield three function components and their
      !>              derivatives with respect to time. The record
      !>              structure for SPK type 20 data is:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z velocity component      |
      !>                 +--------------------------------------+
      !>                 | X position component                 |
      !>                 +--------------------------------------+
      !>                 | Y position component                 |
      !>                 +--------------------------------------+
      !>                 | Z position component                 |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Position components have units of km.
      !>                 - Velocity coefficients have units of km/s.
      !>
      !>              See PCKE20 for a description of PCK type 20 records.
      !>
      !>              PCK type 20 records contain coefficients for Euler
      !>              angle rates and Euler angles corresponding to the
      !>              interval midpoint. See PCKE20 for a more detailed
      !>              description of the contents of PCK type 20 records.
      !>
      !>## Detailed Output
      !>
      !>     XYZDOT   is a 6-vector. In order, the components of XYZDOT are
      !>              X, Y, Z, X', Y', and Z'. Units for state evaluations
      !>              will be km and km/sec. Units for angles will be
      !>              radians and radians/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input record contains an invalid coefficient count,
      !>         the error SPICE(INVALIDCOUNT) is signaled.
      !>
      !>     2)  If the input record contains invalid domain transformation
      !>         parameters, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine evaluates both type 20 SPK and PCK records.
      !>
      !>     The exact format and structure of type 20 (Chebyshev polynomials,
      !>     position only) segments are described in the SPK and PCK Required
      !>     Reading files.
      !>
      !>     A type 20 record contains three sets of Chebyshev coefficients---
      !>     one set each for velocity components dX/dt, dY/dt, and dZ/dt. It
      !>     also contains a position vector (or for a PCK record, Euler
      !>     angles) associated with the midpoint of the record's coverage
      !>     interval. The position (or orientation) is obtained from the
      !>     indefinite integral of the velocity and the given vector.
      !>
      !>## Examples
      !>
      !>     The data returned by the routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely.
      !>
      !>     The code fragment below demonstrates reading and evaluating
      !>     a type 20 SPK record.
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
      !>           IF ( TYPE .EQ. 20 ) THEN
      !>
      !>              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE20 ( ET, RECORD, XYZDOT )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     R.E. Thurman       (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (RET) (KSZ)
      module subroutine spke20 ( ctx, et, record, xyzdot )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: xyzdot   ( 6 )  !! Three function components and their derivatives.

      real(wp) :: intgrl ( 3 )

      integer(ip) :: degp
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: ncof
      integer(ip) :: posloc

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The first number in the record is the record size. This is the
!     number of elements in the record, excluding the size itself.
!     Following it are the record's midpoint and radius, then the three
!     sets of coefficients. The record ends with three values which
!     represent either a position or orientation at the record coverage
!     interval's midpoint. The number of coefficients for each variable
!     can be determined from the record size, since there are the same
!     number of coefficients for each variable.
!
!     The number of items counted by RECORD(1), other than the
!     Chebyshev coefficients, is 5.
!
      ncof = ( int( record( 1 ) ) - 5 ) / 3

      if ( ncof < 1 ) then

         call ctx%setmsg ( 'The input record''s coefficient count NCOF ' &
         //            'should be positive but was #.'              )
         call ctx%errint ( '#', ncof                                    )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degp = ncof - 1

!
!     Pass the Chebyshev coefficient portion of the record into CHBIGR,
!     which will evaluate the expansion and its integral for each
!     component. The constants of integration are selected so that the
!     integrals are zero when the input time is the interval midpoint.
!
      do i = 1, 3

         j = 4 + (i-1)*ncof

         call chbigr ( ctx, degp, record(j),   record(2), &
                       et,   xyzdot(3+i), intgrl(i)  )
      end do

!
!     Add the position vector or Euler angles at the interval midpoint
!     to the integral.
!
      posloc = 4  +  ( 3 * ncof )

      xyzdot(1:3) = record(posloc:posloc+3-1) + intgrl

      return
      end subroutine spke20

      !> Evaluate a single SPK data record from a segment of type 21
      !> (Extended Difference Lines).
      !>
      !>## Detailed Input
      !>
      !>     ET       is an epoch at which a state vector is to be
      !>              computed. The epoch is represented as seconds past
      !>              J2000 TDB.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of an
      !>              ephemeris object, relative to its center of motion,
      !>              in an inertial reference frame.
      !>
      !>              The contents of RECORD are as follows:
      !>
      !>                 RECORD(1):         The difference table size per
      !>                                    Cartesian component. Call this
      !>                                    size MAXDIM; then the difference
      !>                                    line (MDA) size DLSIZE is
      !>
      !>                                      ( 4 * MAXDIM ) + 11
      !>
      !>                 RECORD(2)
      !>                    ...
      !>                 RECORD(1+DLSIZE):  An extended difference line.
      !>                                    The contents are:
      !>
      !>                    Dimension  Description
      !>                    ---------  ----------------------------------
      !>                    1          Reference epoch of difference line
      !>                    MAXDIM     Stepsize function vector
      !>                    1          Reference position vector,  x
      !>                    1          Reference velocity vector,  x
      !>                    1          Reference position vector,  y
      !>                    1          Reference velocity vector,  y
      !>                    1          Reference position vector,  z
      !>                    1          Reference velocity vector,  z
      !>                    MAXDIM,3   Modified divided difference
      !>                               arrays (MDAs)
      !>                    1          Maximum integration order plus 1
      !>                    3          Integration order array
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state resulting from evaluation of the input
      !>              record at ET. Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     MAXTRM   is the maximum number of terms allowed in
      !>              each component of the difference table
      !>              contained in the input argument RECORD.
      !>              See the INCLUDE file spk21.inc for the value
      !>              of MAXTRM.
      !>
      !>## Exceptions
      !>
      !>     1)  If the maximum table size of the input record exceeds
      !>         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 21 (difference lines)
      !>     segments are described in the SPK Required Reading file.
      !>
      !>     SPKE21 is a modified version of SPKE01. The routine has been
      !>     generalized to support variable size difference lines.
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     F.T. Krogh         (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB) (FTK) (WLT) (IMU)
      module subroutine spke21 ( ctx, et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk21
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity).
      real(wp) :: tl_1(1)  !! one-element copy of tl (array argument)
      integer(ip) :: maxdim
      integer(ip) :: j
      real(wp) :: delta
      integer(ip) :: ks
      integer(ip) :: i
      integer(ip) :: kqmax1
      integer(ip) :: ks1
      integer(ip) :: mq2
      real(wp) :: tp
      real(wp) :: tl
      real(wp) :: sum
      real(wp) :: g        ( maxtrm )
      integer(ip) :: kqq
      integer(ip) :: jx
      real(wp) :: fc       ( maxtrm )
      real(wp) :: refpos   (    3 )
      integer(ip) :: kq       (    3 )
      real(wp) :: wc       ( maxtrm-1 )
      real(wp) :: refvel   (    3 )
      real(wp) :: w        ( maxtrm+2 )
      real(wp) :: dt       ( maxtrm, 3 )
      if ( ctx%failed() ) return

!     PEPPER: FC(1) is always 1 (SPICELIB set it with DATA); the other
!     elements are computed below.
      fc(1) = 1.0_wp
!
!     The first element of the input record is the dimension
!     of the difference table MAXDIM.
!
      maxdim = nint ( record(1) )

      if ( maxdim > maxtrm ) then

         call ctx%setmsg ( 'The input record has a maximum table ' &
         //            'dimension of #, while the maximum ' &
         //            'supported by this routine is #. It is ' &
         //            'possible that this problem is due to ' &
         //            'your SPICE Toolkit being out of date.' )
         call ctx%errint ( '#',  maxdim                            )
         call ctx%errint ( '#',  maxtrm                            )
         call ctx%sigerr ( 'SPICE(DIFFLINETOOLARGE)'               )
         return

      end if

!
!     Unpack the contents of the MDA array.
!
!        Name     Dimension  Description
!        ------   ---------  -------------------------------
!        TL               1  Reference epoch of record
!        G           MAXDIM  Stepsize function vector
!        REFPOS           3  Reference position vector
!        REFVEL           3  Reference velocity vector
!        DT      MAXDIM,NTE  Modified divided difference arrays
!        KQMAX1           1  Maximum integration order plus 1
!        KQ             NTE  Integration order array
!
!     For our purposes, NTE is always 3.
!

      tl_1 = record(2:2)
      tl = tl_1(1)
      g(1:maxdim) = record(3:3+maxdim-1)

!
!     Collect the reference position and velocity.
!
      refpos(1) =  record(maxdim+3)
      refvel(1) =  record(maxdim+4)

      refpos(2) =  record(maxdim+5)
      refvel(2) =  record(maxdim+6)

      refpos(3) =  record(maxdim+7)
      refvel(3) =  record(maxdim+8)

!
!     Initializing the difference table is one aspect of this routine
!     that's a bit different from SPKE01. Here the first dimension of
!     the table in the input record can be smaller than MAXTRM. So, we
!     must transfer separately the portions of the table corresponding
!     to each component.
!
      do i = 1, 3
         dt(1:maxdim,i) = record(i*maxdim+9:i*maxdim+9+maxdim-1)
      end do

      kqmax1    = int ( record(4*maxdim + 9 ) )
      kq(1)     = int ( record(4*maxdim + 10) )
      kq(2)     = int ( record(4*maxdim + 11) )
      kq(3)     = int ( record(4*maxdim + 12) )

!
!     Next we set up for the computation of the various differences
!
      delta     = et     - tl
      tp        = delta
      mq2       = kqmax1 - 2
      ks        = kqmax1 - 1

!
!     This is clearly collecting some kind of coefficients.
!     The problem is that we have no idea what they are...
!
!     The G coefficients are supposed to be some kind of step size
!     vector.
!
!     TP starts out as the delta t between the request time and the
!     difference line's reference epoch. We then change it from DELTA
!     by the components of the stepsize vector G.
!
      do j = 1, mq2
!
!        Make sure we're not about to attempt division by zero.
!
         if ( g(j) == 0.0_wp ) then

            call ctx%setmsg ( 'A  value of zero was found at ' &
            //            'index # of the step size vector.'   )
            call ctx%errint ( '#',  j                              )
            call ctx%sigerr ( 'SPICE(ZEROSTEP)'                    )
            return

         end if

         fc(j+1) = tp    / g(j)
         wc(j)   = delta / g(j)
         tp      = delta + g(j)

      end do

!
!     Collect KQMAX1 reciprocals.
!
      do j = 1, kqmax1
         w(j) = 1.0_wp/real(j, wp)
      end do

!
!     Compute the W(K) terms needed for the position interpolation
!     (Note,  it is assumed throughout this routine that KS, which
!     starts out as KQMAX1-1 (the ``maximum integration'')
!     is at least 2.
!
      jx  = 0
      ks1 = ks - 1

      do while ( ks >= 2 )

         jx  = jx+1

         do  j = 1, jx
            w(j+ks) = fc(j+1)*w(j+ks1) - wc(j)*w(j+ks)
         end do

         ks  = ks1
         ks1 = ks1 - 1

      end do

!
!     Perform position interpolation: (Note that KS = 1 right now.
!     We don't know much more than that.)
!
      do  i = 1, 3

         kqq = kq(i)
         sum = 0.0_wp

         do j = kqq, 1, -1
            sum = sum + dt(j,i)*w(j+ks)
         end do

         state(i) = refpos(i)  +  delta * ( refvel(i) + delta*sum )

      end do

!
!     Again we need to compute the W(K) coefficients that are
!     going to be used in the velocity interpolation.
!     (Note, at this point, KS = 1, KS1 = 0.)
!
      do  j = 1, jx
         w(j+ks) = fc(j+1)*w(j+ks1) - wc(j)*w(j+ks)
      end do

      ks = ks - 1

!
!     Perform velocity interpolation:
!
      do  i = 1, 3

         kqq = kq(i)
         sum = 0.0_wp

         do j = kqq, 1, -1
            sum = sum + dt(j,i)*w(j+ks)
         end do

         state(i+3) = refvel(i) + delta*sum

      end do

      return
      end subroutine spke21

      !> Read a single SPK data record from a segment of type 1
      !> (Difference Lines).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 1.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 1 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 1 ) THEN
      !>              CALL SPKR01 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        to $Exceptions section. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
      !>
      !>        SPK01 was removed from the $Required_Reading section of the
      !>        header. The information in the SPK01 Required Reading file
      !>        is now part of the SPK Required Reading file.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine spkr01 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstltd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      real(wp) :: dc       (   2 )
      integer(ip) :: ic       (   6 )
      integer(ip) :: begin
      integer(ip) :: end

      real(wp) :: data     ( 100 )
      integer(ip) :: nrec
      integer(ip) :: ndir
      integer(ip) :: offd
      integer(ip) :: offe
      integer(ip) :: off

      integer(ip) :: recno
      integer(ip) :: offr
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      begin = ic(5)
      end   = ic(6)

!
!     Get the number of records in the segment. From that, we can
!     compute
!
!        NDIR      The number of directory epochs.
!
!        OFFD      The offset of the first directory epoch.
!
!        OFFE      The offset of the first epoch.
!
!     the number of directory epochs.
!
      call dafgda ( ctx, handle, end, end, data )
      nrec = int ( data(1) )

      ndir  = nrec / 100
      offd  = end - ndir - 1
      offe  = end - ndir - nrec - 1

!
!     What we want is the record number: once we have that, we can
!     compute the offset of the record from the beginning of the
!     segment, grab it, and go. But how to find it?
!
!     Ultimately, we want the first record whose epoch is greater
!     than or equal to ET. If there are 100 or fewer records, all
!     the record epochs can be examined in a single group.
!
      if ( nrec <= 100 ) then
         call dafgda ( ctx, handle, offe+1, offe+nrec, data )
         recno = lstltd ( et, nrec, data ) + 1

         offr = (begin - 1) + (recno - 1) * 71
         call dafgda ( ctx, handle, offr+1, offr+71, record )

         return
      end if

!
!     Searching directories is a little more difficult.
!
!     The directory contains epochs 100, 200, and so on. Once we
!     find the first directory epoch greater than or equal to ET,
!     we can grab the corresponding set of 100 record epochs, and
!     search them.
!
      do i = 1, ndir
         call dafgda ( ctx, handle, offd+i, offd+i, data )

         if ( data(1) >= et ) then
            off = offe + (i - 1) * 100
            call dafgda ( ctx, handle, off+1, off+100, data )

            recno = ((i - 1) * 100) + lstltd ( et, 100, data ) + 1

            offr = (begin - 1) + (recno - 1) * 71
            call dafgda ( ctx, handle, offr+1, offr+71, record )

            return
         end if
      end do

!
!     If ET is greater than the final directory epoch, we want one
!     of the final records.
!
      i = mod ( nrec, 100 )

      call dafgda ( ctx, handle, end-ndir-i, end-ndir-1, data )
      recno = (ndir * 100) + lstltd ( et, i, data ) + 1

      offr = (begin - 1) + (recno - 1) * 71
      call dafgda ( ctx, handle, offr+1, offr+71, record )

      return
      end subroutine spkr01

      !> Read a single SPK data record from a segment of type 2
      !> (Chebyshev, position only).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              an SPK segment of type 2.
      !>
      !>     ET       is an epoch for which a data record from the
      !>              specified segment is required. ET is expressed as
      !>              seconds past J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is an array of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of the target body identified
      !>              by the input segment descriptor. The descriptor
      !>              specifies the center of motion and reference frame of
      !>              the state.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z position component      |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Position components have units of km.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by this routine. See the INCLUDE file
      !>              spkrec.inc for the correct record length.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 2 (Chebyshev polynomials, position
      !>     only) segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 2 ) THEN
      !>              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.2, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.1.1, 18-JAN-2014 (NJB)
      !>
      !>        Enhanced header and in-line documentation.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
      !>
      !>        SPK02 was removed from the $Required_Reading section of the
      !>        header. The information in the SPK02 Required Reading file
      !>        is now part of the SPK Required Reading file.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine spkr02 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      real(wp) :: dc       (   2 )
      real(wp) :: init
      real(wp) :: intlen

      integer(ip) :: ic       (   6 )
      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: recsiz
      integer(ip) :: nrec
      integer(ip) :: recno
      integer(ip) :: recadr

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      begin = ic( 5 )
      end   = ic( 6 )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the initial time of the first record's coverage interval, and the
!     length of the interval covered by each record. These constants
!     are located at the end of the segment, along with the size of
!     each logical record and the total number of records.
!
      call dafgda ( ctx, handle, end-3, end, record )

      init   = record( 1 )
      intlen = record( 2 )
      recsiz = int( record( 3 ) )
      nrec   = int( record( 4 ) )

      recno = int( (et - init) / intlen ) + 1
      recno = min( recno, nrec )

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size of the record.
!
      record( 1 ) = record( 3 )
      call dafgda ( ctx, handle, recadr, recadr + recsiz - 1, record( 2 ) )

      return
      end subroutine spkr02

      !> Read a single SPK data record from a segment of type 3
      !> (Chebyshev coefficients, position and velocity).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 3.
      !>
      !>     ET       is an epoch for which a data record from the
      !>              specified segment is required. ET is expressed as
      !>              seconds past J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is an array of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of the target body identified
      !>              by the input segment descriptor. The descriptor
      !>              specifies the center of motion and reference frame of
      !>              the state.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z position component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z velocity component      |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Position components have units of km.
      !>                 - Velocity components have units of km/s.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by this routine. See the INCLUDE file
      !>              spkrec.inc for the correct record length.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 3 (Chebyshev polynomials, position
      !>     and velocity) segment.
      !>
      !>     On not so close inspection, you will see that the implementation
      !>     of this routine is identical to SPKR02, which reads a type 2
      !>     (Chebyshev polynomials, position only) segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 3 ) THEN
      !>              CALL SPKR03 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.2, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.1.1, 18-JAN-2014 (NJB)
      !>
      !>        Enhanced header and in-line documentation.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
      !>
      !>        SPK03 was removed from the $Required_Reading section of the
      !>        header. The information in the SPK03 Required Reading file
      !>        is now part of the SPK Required Reading file.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
      module subroutine spkr03 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      real(wp) :: dc       (   2 )
      real(wp) :: init
      real(wp) :: intlen

      integer(ip) :: ic       (   6 )
      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: recsiz
      integer(ip) :: nrec
      integer(ip) :: recno
      integer(ip) :: recadr

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      begin = ic( 5 )
      end   = ic( 6 )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the initial time of the first record's coverage interval, and the
!     length of the interval covered by each record. These constants
!     are located at the end of the segment, along with the size of
!     each logical record and the total number of records.
!
      call dafgda ( ctx, handle, end-3, end, record )

      init   = record( 1 )
      intlen = record( 2 )
      recsiz = int( record( 3 ) )
      nrec   = int( record( 4 ) )

      recno = int( (et - init) / intlen ) + 1
      recno = min( recno, nrec )

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size of the record.
!
      record( 1 ) = record( 3 )
      call dafgda ( ctx, handle, recadr, recadr + recsiz - 1, record( 2 ) )

      return
      end subroutine spkr03

      !> Read a single SPK data record from a segment of type 5
      !> ( two body propagation between discrete state vectors ).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              the type 05 SPK segment to be read.
      !>
      !>     ET       is a target epoch, specified as ephemeris seconds past
      !>              J2000, for which a data record from the segment is
      !>              required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a logical record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>              The structure of RECORD is:
      !>
      !>                  RECORD(1)
      !>                     .            state of the body at epoch 1.
      !>                     .
      !>                     .
      !>                  RECORD(6)
      !>
      !>                  RECORD(7)
      !>                     .
      !>                     .            state of the body at epoch 2.
      !>                     .
      !>                  RECORD(12)
      !>                  RECORD(13)      epoch 1 in seconds past 2000.
      !>                  RECORD(14)      epoch 2 in seconds past 2000.
      !>                  RECORD(15)      GM for the center of motion.
      !>
      !>              Epoch 1 and epoch 2 are the times in the segment that
      !>              bracket ET. If ET is less than the first time in the
      !>              segment then both epochs 1 and 2 are equal to the
      !>              first time. And if ET is greater than the last time
      !>              then, epochs 1 and 2 are set equal to this last time.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the segment specified by DESCR is not of data type 05,
      !>         the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     2)  No error is signaled if ET is outside the time bounds of
      !>         the segment. The output RECORD will contain epochs and the
      !>         associated states which satisfy the rules stated above.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine reads the segment specified by DESCR from the SPK
      !>     file attached to HANDLE to locate the two epochs in the segment
      !>     that bracket the input ET. It then returns a logical record which
      !>     contains these times and their associated states, and also the
      !>     mass of the center of motion. The routine makes explicit use of
      !>     the structure of the type 05 data segment to locate this data.
      !>
      !>     See the section of the SPK Required Reading on data type 05 for
      !>     a description of the structure of a type 05 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRnn
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>           IF ( FOUND ) THEN
      !>
      !>     C
      !>     C        Look at parts of the descriptor.
      !>     C
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>              CENTER = ICD( 2 )
      !>              REF    = ICD( 3 )
      !>              TYPE   = ICD( 4 )
      !>
      !>              IF ( TYPE .EQ. 05 ) THEN
      !>
      !>                 CALL SPKR05 ( HANDLE, DESCR, ET, RECORD )
      !>                     .
      !>                     .  Look at the RECORD data.
      !>                     .
      !>              END IF
      !>
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
      !>     J.M. Lynch         (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
      module subroutine spkr05 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstltd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      integer(ip), parameter :: size = 100

      integer(ip), parameter :: dirsiz = size

      integer(ip), parameter :: bufsiz = size

      integer(ip), parameter :: stasiz = 6



      real(wp) :: dc       (   nd )
      real(wp) :: data     ( size )

      integer(ip) :: ic       (   ni )
      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: type
      integer(ip) :: nrec
      integer(ip) :: ndir
      integer(ip) :: group
      integer(ip) :: dirloc
      integer(ip) :: remain
      integer(ip) :: skip
      integer(ip) :: grpadd
      integer(ip) :: addrss
      integer(ip) :: i
      integer(ip) :: n

      logical :: fnd

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, nd, ni, dc, ic )

      type  = ic(4)
      begin = ic(5)
      end   = ic(6)

!
!     Make sure that this really is a type 5 data segment.
!
      if ( type /= 05 ) then
         call ctx%setmsg( 'You are attempting to locate type 5 '      // &
                      'data in a type # data segment.'              )
         call ctx%errint( '#',  type                                    )
         call ctx%sigerr( 'SPICE(WRONGSPKTYPE)'                         )
         return
      end if

!
!     Get the number of records in the segment. While we're at it,
!     get the GM of the central body (it's adjacent to NREC)
!     since we'll need it anyway. Put it where it belongs, at the
!     end of the output record.
!
      call dafgda ( ctx, handle, end-1, end, data )

      nrec       = nint ( data(2) )
      record(15) =        data(1)

!
!     From the number of records, we can compute the number of
!     directory epochs.
!
      ndir = nrec / dirsiz

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records. Because the Ith directory epoch is the I*100th
!     epoch, the Ith group will contain epochs ((I-1)*100 + 1) through
!     (I*100).  For example:
!                            group   first epoch #   last epoch #
!                            -----   -------------   ------------
!                              1               1          100
!                              2             101          200
!                              .               .            .
!                              .               .            .
!                             10             901         1000
!                              .               .            .
!                              .               .            .
!                              N     (N-1)*100+1        N*100

      if ( ndir == 0 ) then
!
!        There is only one group if there are no directory epochs.
!
         group = 1

      else
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, we need to go through all of the
!        NREC states and epochs.
!
         dirloc = begin + ( stasiz + 1 ) * nrec

!
!        Determine which group of DIRSIZ times to search, by finding
!        the last directory epoch that is less than ET.
!
         fnd    = .false.
         remain =  ndir
         group  =  0

         do while ( .not. fnd )
!
!           Read in as many as BUFSIZ directory epochs at a time
!           for comparison.
!
            n = min( remain, bufsiz )

            call dafgda ( ctx, handle, dirloc, dirloc + n - 1, data )

            remain = remain - n

!
!           Determine the last directory element in DATA that's less
!           than ET.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
            i = lstltd( et, n, data )

            if ( i < n ) then

               group =  group + i + 1
               fnd   = .true.

            else if ( remain == 0 ) then

               group =  ndir + 1
               fnd   = .true.

            else

               dirloc = dirloc + n
               group  = group  + n

            end if

         end do

      end if

!
!     Now we know which group of DIRSIZ (or less) epochs to look at.
!     Out of the NREC epochs, the number that we should skip over
!     to get to the proper group is DIRSIZ * ( GROUP - 1 ).
!
      skip = dirsiz * ( group - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, we need to pass through
!     STASIZ * NREC state numbers to get to the first epoch. Then we
!     skip over the number just computed above.
!
      grpadd = begin + nrec * ( stasiz ) + skip

!
!     The number of epochs that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
!     If there are no times in the last group then the time that we
!     are looking for is the same as the last directory epoch.
!     We should not try to read in this instance.
!
      n = min ( dirsiz, nrec - skip )

      if ( n /= 0 ) then

         call dafgda ( ctx, handle, grpadd, grpadd + n - 1, data )
!
!        Find the index of the largest time in the group that is less
!        than the input time.
!
         i = lstltd ( et, n, data )

      else
!
!        If we are here it means that ET is greater then the last
!        time in the segment and there are no elements in the last
!        group.  This can occur when the number of epochs is a multiple
!        DIRSIZ.
!
!        By setting N equal to I we can handle this case in the
!        same branch as when there are elements in the last group.
!        This is because the DATA array still contains the directory
!        epochs and I is pointing at the last element which is also the
!        last time in the segment.
!
         n = i

      end if

!
!     At this point N is the number of epochs in this GROUP which is
!     also the size of the array DATA which contains the epochs. I is
!     the index of the largest time in DATA which is less than ET.
!
!     We need to take different actions depending on whether ET is less
!     than the first time or greater than the last one in the GROUP.
!

      if ( i == 0 ) then

         if ( group == 1 ) then

!
!           ET is less than or equal to the first time in the segment.
!           Return the state at the first time twice.
!
            record(13) = data(1)
            record(14) = data(1)

            call dafgda ( ctx, handle, begin, begin + 5, data )

            record(1:6) = data(1:6)
            record(7:12) = data(1:6)

            return

         else
!
!           ET is less than or equal to the first time in this group
!           but not the first time in the segment. Get the last time
!           from the preceding group. The states for this case will
!           be read outside of the IF block.
!
            call dafgda ( ctx, handle, grpadd - 1, grpadd, data )

            record(13) = data(1)
            record(14) = data(2)

         end if

      else if ( i == n ) then

         if ( group == (ndir + 1) ) then
!
!           ET is greater than all of the times in the segment. Return
!           the state for the last time twice.
!
            record(13) = data(n)
            record(14) = data(n)

            addrss = begin + ( nrec - 1 ) * stasiz

            call dafgda ( ctx, handle, addrss, addrss + 5, data )

            record(1:6) = data(1:6)
            record(7:12) = data(1:6)

            return

         else
!
!           ET is greater than the last time in this group but this is
!           not the last time in the segment.  Need the first time from
!           the following group. The states for this case will be read
!           outside of the IF block.
!
            call dafgda ( ctx, handle, grpadd + n - 1, grpadd + n, data )

            record(13) = data(1)
            record(14) = data(2)

         end if

      else
!
!        There are two times in the group that bracket ET. The states
!        for this case will be read outside of the IF block.
!
         record(13) = data( i )
         record(14) = data(i+1)

      end if

!
!     Read the consecutive states for the two epochs found above.
!     ET is greater than the (SKIP + I)th time but less than or
!     equal to the time (SKIP + I + 1).
!

      addrss = begin + ( skip + i - 1 ) * stasiz

      call dafgda ( ctx, handle, addrss, addrss + 11, data )

      record(1:12) = data(1:12)

      return
      end subroutine spkr05

      !> Read a single SPK data record from a segment of type 8
      !> (equally spaced discrete states, interpolated by Lagrange
      !> polynomials).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 8.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a set of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | start epoch          |
      !>                 +----------------------+
      !>                 | step size            |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     This routine follows the pattern established in the lower-numbered
      !>     SPK data type readers of not explicitly performing error
      !>     diagnoses. Exceptions are listed below nonetheless.
      !>
      !>     1)  If the input HANDLE does not designate a loaded SPK file, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the segment specified by DESCR is not of data type 08,
      !>         the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     3)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 8 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 8 ) THEN
      !>              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  Correctness of inputs must be ensured by the caller of
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
      !>
      !>        Data type check was relaxed to enable reading type 12
      !>        segments.
      !>
      !>-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
      !>
      !>        In-line comment concerning transpose of state data was
      !>        removed.
      !>
      !>-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
      module subroutine spkr08 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_math, only: odd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.






      real(wp) :: contrl ( ctrlsz  )
      real(wp) :: dc     ( nd )
      real(wp) :: start
      real(wp) :: step

      integer(ip) :: begin
      integer(ip) :: degree
      integer(ip) :: end
      integer(ip) :: first
      integer(ip) :: grpsiz
      integer(ip) :: ic     ( ni )
      integer(ip) :: last
      integer(ip) :: low
      integer(ip) :: n
      integer(ip) :: near
      integer(ip) :: type

      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      type  = ic( 4 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 8 or type 12 data segment.
!
      if (  ( type /= 08 ) .and. ( type /= 12 )  ) then
         call ctx%setmsg( 'You are attempting to locate type 8 or type ' // &
                      '12 data in a type # data segment.'             )
         call ctx%errint( '#',  type                                      )
         call ctx%sigerr( 'SPICE(WRONGSPKTYPE)'                           )
         return
      end if

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et < dc(1) )  .or.  ( et > dc(2) )  )  then
         call ctx%setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call ctx%errdp ( '#',  et                                      )
         call ctx%errdp ( '#',  dc(1)                                   )
         call ctx%errdp ( '#',  dc(2)                                   )
         call ctx%sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if

!
!     The type 8 segment structure is described by this diagram from
!     the SPK Required Reading:
!
!        +-----------------------+
!        | State 1               |
!        +-----------------------+
!        | State 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | State N               |
!        +-----------------------+
!        | Epoch of state 1 (ET) |
!        +-----------------------+
!        | Step size             |
!        +-----------------------+
!        | Polynomial degree     |
!        +-----------------------+
!        | Number of states      |
!        +-----------------------+
!
!     We'll need the last four items before we can determine which
!     states make up our output record.
!
      call dafgda ( ctx, handle, end-3, end, contrl )

      start   =  contrl(1)
      step    =  contrl(2)
      degree  =  nint ( contrl(3) )
      n       =  nint ( contrl(4) )

      grpsiz  =  degree + 1

!
!     We'll now select the set of states that define the interpolating
!     polynomials.  The cases of odd and even GRPSIZ are handled
!     separately.
!
      if ( odd(grpsiz) ) then
!
!        Find the index of the state whose epoch is closest to the
!        input epoch.  Find the first and last indices in the record
!        of the (GRPSIZ-1)/2 states on either side of this central
!        state.
!
         near   =  nint ( ( et - start ) / step )  +  1

         first  =  min (  max( 1,  near - (degree/2) ),  n-degree )
         last   =  first  +  degree

      else
!
!        Find the index of the last state whose epoch is less than or
!        equal to that of the input epoch.  Find the first and last
!        indices in the record of the set of GRPSIZ consecutive states
!        having this state as the (GRPSIZ/2)th one.
!
         low    =  int ( ( et - start ) / step )  +  1

         first  =  min (  max( 1,  low - degree/2 ),  n-degree )
         last   =  first  +  degree

      end if

!
!     Put the size of the group of states, the epoch of the first
!     state in the record, and the step size into the output record.
!
      record(1) =  grpsiz
      record(2) =  start  +  (first-1)*step
      record(3) =  step

!
!     Read the states.
!
      call dafgda ( ctx, handle, &
                    begin + (first-1)*statsz, &
                    begin +      last*statsz  -  1, &
                    record(staidx)                  )

      return
      end subroutine spkr08

      !> Read a single SPK data record from a segment of type 9
      !> (Unequally spaced discrete states, interpolated by Lagrange
      !> polynomials).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 9.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a set of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     This routine follows the pattern established in the lower-numbered
      !>     SPK data type readers of not explicitly performing error
      !>     diagnoses. Exceptions are listed below nonetheless.
      !>
      !>     1)  If the input HANDLE does not designate a loaded SPK file, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the segment specified by DESCR is not of data types 9 or
      !>         13, the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     3)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 9 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 9 ) THEN
      !>              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  Correctness of inputs must be ensured by the caller of
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
      !>
      !>        Data type check was relaxed to enable reading type 13
      !>        segments.
      !>
      !>-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
      !>
      !>        In-line comment concerning transpose of state data was
      !>        removed.
      !>
      !>-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
      module subroutine spkr09 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstltd
      use pepper_math, only: odd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.






      real(wp) :: buffer ( bufsiz )
      real(wp) :: contrl ( 2  )
      real(wp) :: dc     ( nd )

      integer(ip) :: begidx
      integer(ip) :: begin
      integer(ip) :: bufbas
      integer(ip) :: degree
      integer(ip) :: dirbas
      integer(ip) :: end
      integer(ip) :: endidx
      integer(ip) :: first
      integer(ip) :: group
      integer(ip) :: high
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: last
      integer(ip) :: low
      integer(ip) :: n
      integer(ip) :: ndir
      integer(ip) :: near
      integer(ip) :: nread
      integer(ip) :: remain
      integer(ip) :: start
      integer(ip) :: timbas
      integer(ip) :: type
      integer(ip) :: wndsiz

      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      type  = ic( 4 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 9 or type 13 data segment.
!
      if (  ( type /= 09 ) .and. ( type /= 13 )  ) then
         call ctx%setmsg( 'You are attempting to locate type 9 or type ' // &
                      '13 data in a type # data segment.'             )
         call ctx%errint( '#',  type                                      )
         call ctx%sigerr( 'SPICE(WRONGSPKTYPE)'                           )
         return
      end if

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et < dc(1) )  .or.  ( et > dc(2) )  )  then
         call ctx%setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call ctx%errdp ( '#',  et                                      )
         call ctx%errdp ( '#',  dc(1)                                   )
         call ctx%errdp ( '#',  dc(2)                                   )
         call ctx%sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if
!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The first and last epochs in the segment define a time
!            interval that contains the interval defined by the segment
!            descriptor's time bounds.
!
!        2)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        3)  The epochs in the segment are in strictly increasing
!            order.
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than
!
!               ( L - 1 ) / 7         [integer division]
!
!            where L is the declared length of the argument RECORD.
!
!        5)  There are at least as many epochs in the segment as the
!            the number of points required to define an interpolating
!            polynomial of the specified degree.
!
!     We'll need the last two items before we can determine which
!     states make up our output record.
!
      call dafgda ( ctx, handle, end-1, end, contrl )

      degree  =  nint ( contrl(1) )
      n       =  nint ( contrl(2) )

      wndsiz  =  degree + 1

!
!     We'll now select the set of states that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      ndir    =  (n-1) / dirsiz
      dirbas  =  end  -  ndir  -  2

      if ( ndir == 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         group  = 1

      else
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to BUFSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  dirbas
         nread   =  min ( ndir, bufsiz )
         remain  =  ndir - nread

         call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

         do while (       ( buffer(nread) < et ) &
                    .and. ( remain        > 0  )   )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, bufsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

         end do

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - dirbas ) &
                    +   lstltd ( et, nread, buffer ) &
                    +   1

      end if

!
!     GROUP now indicates the set of epochs in which to search for the
!     request epoch.  If GROUP is 1, the request time lies within the
!     inclusive time interval bounded by the first and last epochs of
!     the first group.  Otherwise, the request time lies in the time
!     interval bounded by the last element of the preceding group and
!     the last element of the current group.
!
!     We'll use the variable names BEGIDX and ENDIDX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( group == 1 ) then

         begidx  =  1
         endidx  =  min ( n, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         begidx  =      (  group  - 1 ) * dirsiz
         endidx  =  min (  begidx + dirsiz,  n  )

      end if

      timbas  =  dirbas - n

      call dafgda ( ctx, handle, timbas+begidx, timbas+endidx, buffer )

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      i     =  lstltd (  et,  endidx-begidx+1,  buffer  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      if ( i == 0 ) then
         low  =  1
      else
         low  =  begidx + i - 1
      end if

      high  =  low  +  1

!
!     Now select the set of states used for interpolation.
!
      if ( odd(wndsiz) ) then
!
!        Find the index of the state whose epoch is closest to the
!        input epoch.  The index I is in the range [0, DIRSIZ],
!        since ENDIDX - BEGIDX never exceeds DIRSIZ, and ET is
!        never larger than the (ENDIDX-BEGIDX+1)th element of the
!        buffer.
!
         if ( i == 0 ) then
!
!           This can happen only if the request time matches the
!           first time tag of the segment.
!
            near = low

         else if (       dabs(  et  -  buffer(i)    ) &
                   <  dabs(  et  -  buffer(i+1)  )    )  then

            near = low

         else
            near = high
         end if

!
!        The epochs whose index is NEAR is the (WNDSIZ/2 + 1)th
!        of the interpolating set, unless the request time is too close
!        to the end of the coverage interval, in which case one endpoint
!        of the window will coincide with an endpoint of the coverage
!        interval.
!
         first =  min (  max ( near - degree/2,  1),   n-degree  )

         last  =  first  +  degree

      else
!
!        The group size is even.
!
!        The bracketing epochs we've found are the (WNDSIZ/2)th
!        and (WNDSIZ/2 + 1)th of the interpolating set, unless the
!        request time is too close to the end of the coverage interval,
!        in which case one endpoint of the window will coincide with
!        an endpoint of the coverage interval.
!
         first =  min (  max ( low - degree/2,  1 ),   n-degree  )

         last  =  first  +  degree

      end if

!
!     Put the size of the group of states into the output record.
!
      record(1) =  wndsiz

!
!     Read the states.
!
      call dafgda ( ctx, handle, &
                    begin + (first-1)*statsz, &
                    begin +      last*statsz  -  1, &
                    record(2)                       )

!
!     Finally, add the epochs to the output record.
!
      start  =  begin   +  n * statsz  +  first - 2

      call dafgda ( ctx,  handle, &
                     start + 1, &
                     start + wndsiz, &
                     record( 2 + wndsiz*statsz )   )

      return
      end subroutine spkr09

      !> Read a single SPK data record from a segment of type 15
      !> (Precessing Conic Propagation).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 15.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the segment specified by DESCR is not a type 15 segment,
      !>         the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     2)  A type 15 segment should have exactly 16 values. If this
      !>         is not the case, the error SPICE(MALFORMEDSEGMENT) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine reads all of the data from a type 15 SPK segment.
      !>
      !>     The structure of the data retrieved in RECORD is:
      !>
      !>         RECORD(1)             epoch of the orbit elements at periapse
      !>                               in ephemeris seconds past J2000.
      !>         RECORD(2)-RECORD(4)   unit trajectory pole vector
      !>         RECORD(5)-RECORD(7)   unit periapsis vector
      !>         RECORD(8)             semi-latus rectum---p in the
      !>                               equation:
      !>
      !>                               r = p/(1 + ECC*COS(Nu))
      !>
      !>         RECORD(9)             eccentricity
      !>         RECORD(10)            J2 processing flag describing
      !>                               what J2 corrections are to be
      !>                               applied when the orbit is
      !>                               propagated.
      !>
      !>                                Value       Meaning
      !>                                -----  -----------------------------
      !>                                1      Regress line of nodes only.
      !>                                2      Precess line of apsides only.
      !>                                3      Don't use J2 corrections.
      !>                                Other  Regress line of nodes
      !>                                       and precess line of apsides.
      !>
      !>         RECORD(11)-RECORD(13) unit central body pole vector
      !>         RECORD(14)            central body GM
      !>         RECORD(15)            central body J2
      !>         RECORD(16)            central body radius
      !>
      !>     Except for J2, units are radians, km, seconds.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRnn
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 15 ) THEN
      !>              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     S. Schlaifer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
      module subroutine spkr15 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr (5)  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record(*)  !! Data record.



!
!     The difference between the first and last address of a type 15
!     segment should be 15.
!
      integer(ip), parameter :: diff = 15

      real(wp) :: dc  ( nd )
      integer(ip) :: ic  ( ni )

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: type

!
!     Standard Spice Error Handling.
!
      if (ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus( ctx, descr, nd, ni, dc, ic )

      type  = ic(4)
      begin = ic(5)
      end   = ic(6)
!
!     Make sure that this really is a type 15 data segment.
!
      if (type /= 15) then

         call ctx%setmsg ('You are attempting to locate type 15 ' // &
                      'data in a type # data segment.')
         call ctx%errint ('#', type                       )
         call ctx%sigerr ('SPICE(WRONGSPKTYPE)'           )
         return

      end if

!
!     Since it doesn't cost much we make sure that the segment has
!     the correct amount of data.
!
      if ( end - begin /= diff ) then

         call ctx%setmsg ( 'A type 15 segment should contain exactly ' &
         //            '16 double precision values.  The segment ' &
         //            'supplied had #.  The segment is badly ' &
         //            'formed. ' )

         call ctx%errint ( '#',   end-begin+1         )
         call ctx%sigerr ( 'SPICE(MALFORMEDSEGMENT)'  )
         return

      end if

!
!     Read the data for the record.
!
      call dafgda ( ctx, handle, begin, end, record )
      return
      end subroutine spkr15

      !> Read a single SPK data record from a segment of type 17
      !> (Equinoctial Elements).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 17.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the segment specified by DESCR is not a type 17 segment,
      !>         the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     2)  A type 17 segment should have exactly 16 values. If this
      !>         is not the case, the error SPICE(MALFORMEDSEGMENT) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine reads all of the data from a type 17 SPK segment.
      !>
      !>     The structure of the data retrieved in RECORD is:
      !>
      !>         RECORD(1) is the epoch of the orbit elements at
      !>                   in ephemeris seconds past J2000.
      !>
      !>         RECORD(2) is the semi-major axis (A) of the orbit.
      !>
      !>         RECORD(3) is the value of H at the specified epoch.
      !>                   ( E*SIN(ARGP+NODE) ).
      !>
      !>         RECORD(4) is the value of K at the specified epoch
      !>                   ( E*COS(ARGP+NODE) ).
      !>
      !>         RECORD(5) is the mean longitude (MEAN0+ARGP+NODE)at
      !>                   the epoch of the elements.
      !>
      !>         RECORD(6) is the value of P (TAN(INC/2)*SIN(NODE))at
      !>                   the specified epoch.
      !>
      !>         RECORD(7) is the value of Q (TAN(INC/2)*COS(NODE))at
      !>                        the specified epoch.
      !>
      !>         RECORD(8) is the rate of the longitude of periapse
      !>                   (dARGP/dt + dNODE/dt ) at the epoch of
      !>                   the elements. This rate is assumed to hold
      !>                   for all time.
      !>
      !>         RECORD(9) is the derivative of the mean longitude
      !>                   ( dM/dt + dARGP/dt + dNODE/dt ).  This
      !>                   rate is assumed to be constant.
      !>
      !>         RECORD(10) is the rate of the longitude of the ascending
      !>                    node ( dNODE/dt).
      !>
      !>         RECORD(11) Right Ascension of the pole of the
      !>                    orbital reference system relative to the
      !>                    reference frame of the associated SPK segment.
      !>
      !>         RECORD(12) Declination of the pole of the
      !>                    orbital reference system relative to the
      !>                    reference frame of the associated SPK segment.
      !>
      !>     Units are km, radians and radians/second.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRnn
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 17 ) THEN
      !>              CALL SPKR17 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     S. Schlaifer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Fixed textual
      !>        description of SPK type in $Abstract.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>        Added IMPLICIT NONE.
      !>
      !>-    SPICELIB Version 1.0.0, 03-JAN-1997 (WLT) (SS)
      module subroutine spkr17 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr (5)  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record(*)  !! Data record.



!
!     The difference between the first and last address of a type 17
!     segment should be 11.
!
      integer(ip), parameter :: diff = 11

      real(wp) :: dc  ( nd )
      integer(ip) :: ic  ( ni )

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: type

!
!     Standard Spice Error Handling.
!
      if (ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus( ctx, descr, nd, ni, dc, ic )

      type  = ic(4)
      begin = ic(5)
      end   = ic(6)
!
!     Make sure that this really is a type 17 data segment.
!
      if (type /= 17) then

         call ctx%setmsg ('You are attempting to locate type 17 ' // &
                      'data in a type # data segment.')
         call ctx%errint ('#', type                       )
         call ctx%sigerr ('SPICE(WRONGSPKTYPE)'           )
         return

      end if

!
!     Since it doesn't cost much we make sure that the segment has
!     the correct amount of data.
!
      if ( end - begin /= diff ) then

         call ctx%setmsg ( 'A type 17 segment should contain exactly ' &
         //            '# double precision values.  The segment ' &
         //            'supplied had #.  The segment is badly ' &
         //            'formed. ' )

         call ctx%errint ( '#',   end-begin+1         )
         call ctx%errint ( '#',   diff+1              )
         call ctx%sigerr ( 'SPICE(MALFORMEDSEGMENT)'  )
         return

      end if

!
!     Read the data for the record.
!
      call dafgda ( ctx, handle, begin, end, record )
      return
      end subroutine spkr17

      !> Read a single SPK data record from a segment of type 18
      !> (MEX/Rosetta Orbit file interpolation).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 18.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a set of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>              The packet size is a function of the subtype code.
      !>              All packets in a record have the same size.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input HANDLE does not designate a loaded SPK file, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the segment specified by DESCR is not of data type 18,
      !>         the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     3)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>     4)  If the window size is non-positive or greater than the
      !>         maximum allowed value, the error SPICE(INVALIDVALUE) is
      !>         signaled.
      !>
      !>     5)  If the window size is not compatible with the segment
      !>         subtype, the error SPICE(INVALIDVALUE) is signaled.
      !>
      !>     6)  If the segment subtype is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     7)  If the input segment contains fewer than 2 packets, the
      !>         error SPICE(TOOFEWSTATES) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 18 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 18 ) THEN
      !>              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  Correctness of inputs must be ensured by the caller of
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
      !>
      !>        An error check was added for segment packet counts
      !>        less than 2.
      !>
      !>        An in-line comment regarding deducibility of record size from
      !>        segment subtype was removed. The comment now says the actual
      !>        count of packets in the output record is inserted into the
      !>        record.
      !>
      !>-    SPICELIB Version 1.0.0, 04-SEP-2002 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
      !>
      !>        An error check was added for segment packet counts
      !>        less than 2.
      module subroutine spkr18 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstltd
      use pepper_math, only: odd
      use pepper_inc_spk18
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.





      integer(ip), parameter :: ctrlsz = 3

!
!     Maximum polynomial degree:
!
      integer(ip), parameter :: maxdeg = 15

      real(wp) :: buffer ( bufsiz )
      real(wp) :: contrl ( ctrlsz )
      real(wp) :: dc     ( nd )

      integer(ip) :: begidx
      integer(ip) :: begin
      integer(ip) :: bufbas
      integer(ip) :: dirbas
      integer(ip) :: end
      integer(ip) :: endidx
      integer(ip) :: first
      integer(ip) :: group
      integer(ip) :: high
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: last
      integer(ip) :: lsize
      integer(ip) :: low
      integer(ip) :: maxwnd
      integer(ip) :: n
      integer(ip) :: ndir
      integer(ip) :: nread
      integer(ip) :: packsz
      integer(ip) :: remain
      integer(ip) :: rsize
      integer(ip) :: start
      integer(ip) :: subtyp
      integer(ip) :: timbas
      integer(ip) :: type
      integer(ip) :: wndsiz

      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      type  = ic( 4 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 18 data segment.
!
      if ( type /= 18 ) then
         call ctx%setmsg( 'You are attempting to locate type * ' // &
                      'data in a type 18 data segment.'      )
         call ctx%errint( '*',  type                             )
         call ctx%sigerr( 'SPICE(WRONGSPKTYPE)'                  )
         return
      end if

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et < dc(1) )  .or.  ( et > dc(2) )  )  then
         call ctx%setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call ctx%errdp ( '#',  et                                      )
         call ctx%errdp ( '#',  dc(1)                                   )
         call ctx%errdp ( '#',  dc(2)                                   )
         call ctx%sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if

!     We'll need the last two items before we can determine which
!     packets make up our output record.
!
      call dafgda ( ctx, handle, end-ctrlsz+1, end, contrl )

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT. You need
!     need to do this only after the first call to DAFGDA.
!
      if ( ctx%failed() ) return

      subtyp  =  nint ( contrl(1) )
      wndsiz  =  nint ( contrl(2) )
      n       =  nint ( contrl(3) )

      if ( n < 2 )  then

         call ctx%setmsg( 'Packet count # is less than the minimum '   // &
                      'valid value, which is 2.'                   )
         call ctx%errint( '#',  n                                      )
         call ctx%sigerr( 'SPICE(TOOFEWSTATES)'                        )
         return

      end if

!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The first and last epochs in the segment define a time
!            interval that contains the interval defined by the segment
!            descriptor's time bounds.
!
!        2)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        3)  The epochs in the segment are in strictly increasing
!            order.
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than
!
!               MAXDEG
!

!
!     Set the packet size, which is a function of the subtype.
!
      if ( subtyp == s18tp0 ) then

         packsz = s18ps0

      else if ( subtyp == s18tp1 ) then

         packsz = s18ps1

      else

         call ctx%setmsg ( 'Unexpected SPK type 18 subtype # found in ' // &
                       'type 18 segment.'                           )
         call ctx%errint ( '#',  subtyp                                 )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         return

      end if

!
!     Check the window size.
!
      if ( wndsiz <= 0 ) then

         call ctx%setmsg ( 'Window size in type 18 segment was #; must ' // &
                       'be positive.'                                )
         call ctx%errint ( '#',  subtyp                                  )
         call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                         )
         return

      end if

      if ( subtyp == s18tp0 ) then

         maxwnd  =  (maxdeg+1)/2

         if ( wndsiz > maxwnd ) then

            call ctx%setmsg ( 'Window size in type 18 segment was #; max '// &
                          'allowed value is # for subtype 0 (Hermite,'// &
                          ' 12-element packets).'                     )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%errint ( '#',  maxwnd                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

         if ( odd(wndsiz) ) then

            call ctx%setmsg ( 'Window size in type 18 segment was #; '    // &
                          'must be even for subtype 0 (Hermite,'      // &
                          ' 12-element packets).'                     )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

      else if ( subtyp == s18tp1 ) then

         maxwnd  =  maxdeg + 1

         if ( wndsiz > maxwnd ) then

            call ctx%setmsg ( 'Window size in type 18 segment was #; max '// &
                          'allowed value is # for subtype 1 '         // &
                          '(Lagrange, 6-element packets).'            )
            call ctx%errint ( '#',  wndsiz                                )
            call ctx%errint ( '#',  maxwnd                                )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                       )
            return

         end if

         if ( odd(wndsiz) ) then

            call ctx%setmsg ( 'Window size in type 18 segment was #; '// &
                          'must be even for subtype 1 '           // &
                          '(Lagrange, 6-element packets).'        )
            call ctx%errint ( '#',  wndsiz                            )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                   )
            return

         end if

      else

         call ctx%setmsg ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle new subtypes.'            )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         return

      end if

!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      ndir    =  (n-1) / dirsiz
      dirbas  =  end  -  ndir  -  ctrlsz

      if ( ndir == 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         group  = 1

      else
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to BUFSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  dirbas
         nread   =  min ( ndir, dirsiz )
         remain  =  ndir - nread

         call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

         do while (       ( buffer(nread) < et ) &
                    .and. ( remain        > 0  )   )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, dirsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

         end do

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - dirbas ) &
                    +   lstltd ( et, nread, buffer ) &
                    +   1

      end if

!
!     GROUP now indicates the set of epochs in which to search for the
!     request epoch.  If GROUP is 1, the request time lies within the
!     inclusive time interval bounded by the first and last epochs of
!     the first group.  Otherwise, the request time lies in the time
!     interval bounded by the last element of the preceding group and
!     the last element of the current group.
!
!     We'll use the variable names BEGIDX and ENDIDX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( group == 1 ) then

         begidx  =  1
         endidx  =  min ( n, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         begidx  =      (  group  - 1 ) * dirsiz
         endidx  =  min (  begidx + dirsiz,  n  )

      end if

      timbas  =  dirbas - n

      call dafgda ( ctx, handle, timbas+begidx, timbas+endidx, buffer )

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      i  =  lstltd (  et,  endidx-begidx+1,  buffer  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      if ( i == 0 ) then
         low  =  1
      else
         low  =  begidx + i - 1
      end if

      high  =  low  +  1

!
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike SPK types 8, 9, 12, and 13, for type 18 we adjust
!     the window size to keep the request time within the central
!     interval of the window.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
!     available, the set ranges from index 1 to index LOW.
!

      lsize =  min ( wndsiz/2, low )

!
!     RSIZE is defined analogously for the right half of the window.
!
      rsize =  min (  wndsiz/2,  ( n - high + 1 )  )

!
!     The window size is simply the sum of LSIZE and RSIZE.
!
      wndsiz =  lsize + rsize

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      first =  low    -  lsize   +  1

      last  =  first  +  wndsiz  -  1

!
!     Put the subtype and actual window size, which is the number of
!     packets in the record, into the output record.
!
      record(1) =  subtyp

      record(2) =  wndsiz

!
!     Read the packets.
!
      call dafgda ( ctx, handle, &
                    begin + (first-1)*packsz, &
                    begin +      last*packsz  -  1, &
                    record(3)                       )

!
!     Finally, add the epochs to the output record.
!
      start  =  begin   +  n * packsz  +  first - 2

      call dafgda ( ctx,  handle, &
                     start + 1, &
                     start + wndsiz, &
                     record( 3 + wndsiz*packsz )   )

      return
      end subroutine spkr18

      !> Read a single SPK data record from a segment of type 19
      !> (ESOC/DDID Piecewise Interpolation).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for an SPK
      !>              segment of type 19. The SPK file designated by HANDLE
      !>              must be open for read access.
      !>
      !>     ET       is an epoch for which a data record from a specific
      !>              segment is required. ET is expressed as seconds past
      !>              J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is an array of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of the target body identified
      !>              by the input segment descriptor. The descriptor
      !>              specifies the center of motion and reference frame of
      !>              the state.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>              The packet size is a function of the type 19 subtype.
      !>              All packets in a record have the same size.
      !>
      !>## Parameters
      !>
      !>     See the Fortran INCLUDE file spk19.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input HANDLE does not designate a loaded SPK file, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the segment specified by DESCR is not of data type 19,
      !>         the error SPICE(WRONGSPKTYPE) is signaled.
      !>
      !>     3)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>     4)  If the window size is non-positive or greater than the
      !>         maximum allowed value, the error SPICE(INVALIDVALUE) is
      !>         signaled.
      !>
      !>     5)  If the window size is not compatible with the segment
      !>         subtype, the error SPICE(INVALIDVALUE) is signaled.
      !>
      !>     6)  If the segment subtype is not recognized, the error
      !>         SPICE(NOTSUPPORTED) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     SPICE user applications normally will have no need to call this
      !>     routine directly. For further information, see the headers of the
      !>     SPICE SPK APIs
      !>
      !>        SPKEZR
      !>        SPKPOS
      !>
      !>     the SPK Required Reading file spk.req, and the SPICE SPK
      !>     tutorial.
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 19 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 19 ) THEN
      !>              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  Correctness of inputs must be ensured by the caller of
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
      !>
      !>        Updated to support subtype 2.
      !>
      !>-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
      module subroutine spkr19 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstled, lstltd
      use pepper_math, only: odd
      use pepper_inc_spk19
      use pepper_inc_spkrec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.
      integer(ip), parameter :: ctrlsz = 3
      integer(ip), parameter :: s19mw0 = ( maxdeg + 1 )/2
      integer(ip), parameter :: s19mw1 = ( maxdeg + 1 )
      integer(ip), parameter :: s19mw2 = ( maxdeg + 1 )
      real(wp) :: buffer ( bufsiz )
      real(wp) :: contrl ( ctrlsz )
      real(wp) :: dc     ( nd )
      real(wp) :: mintim ( nd )
      integer(ip) :: baddr
      integer(ip) :: begidx
      integer(ip) :: bufbas
      integer(ip) :: dirbas
      integer(ip) :: eaddr
      integer(ip) :: endidx
      integer(ip) :: first
      integer(ip) :: group
      integer(ip) :: high
      integer(ip) :: i
      integer(ip) :: ic     ( ni )
      integer(ip) :: isel
      integer(ip) :: ivbas
      integer(ip) :: ivbix
      integer(ip) :: iveix
      integer(ip) :: last
      integer(ip) :: low
      integer(ip) :: lsize
      integer(ip) :: maxwnd
      integer(ip) :: minib
      integer(ip) :: minie
      integer(ip) :: miniix
      integer(ip), parameter :: mxwnsz ( 0 : s19nst-1 ) = &
         [ s19mw0, s19mw1, s19mw2 ]
      integer(ip) :: n
      integer(ip) :: ndir
      integer(ip) :: npkdir
      integer(ip) :: npkt
      integer(ip) :: nrcpkt
      integer(ip) :: nread
      integer(ip) :: pkdbas
      integer(ip) :: pktsiz
      integer(ip), parameter :: pktszs ( 0 : s19nst-1 ) = &
         [ s19ps0, s19ps1, s19ps2 ]
      integer(ip) :: remain
      integer(ip) :: rsize
      integer(ip) :: subtyp
      integer(ip) :: timbas
      integer(ip) :: type
      integer(ip) :: wndsiz
      logical :: ivlsel
      logical :: prvok
      logical :: samivl
      logical :: samseg
      if ( .not. allocated ( ctx%spkr19_state ) ) allocate ( ctx%spkr19_state )
      associate ( pass1 => ctx%spkr19_state%pass1, &
                  svbeg => ctx%spkr19_state%svbeg, &
                  svbtim => ctx%spkr19_state%svbtim, &
                  svetim => ctx%spkr19_state%svetim, &
                  svhan => ctx%spkr19_state%svhan, &
                  svlast => ctx%spkr19_state%svlast, &
                  svmiix => ctx%spkr19_state%svmiix, &
                  svminb => ctx%spkr19_state%svminb, &
                  svn => ctx%spkr19_state%svn, &
                  svnpkt => ctx%spkr19_state%svnpkt, &
                  svok => ctx%spkr19_state%svok, &
                  svpkdb => ctx%spkr19_state%svpkdb, &
                  svpknd => ctx%spkr19_state%svpknd, &
                  svpksz => ctx%spkr19_state%svpksz, &
                  svstyp => ctx%spkr19_state%svstyp, &
                  svwnsz => ctx%spkr19_state%svwnsz )
      if ( ctx%failed() ) return

!
!     Before any error checks are done, copy the status from
!     the previous call. Set the saved status variable to
!     .FALSE. here so it will be .FALSE. on exit unless this
!     call is successful.
!
      prvok = svok
      svok  = .false.

!
!     Terminology: below, the phrase "base address of 'X'" refers to
!     the DAF address immediately preceding X. Base addresses simplify
!     mapping DAF array (here "array" means an array stored in
!     consecutive DAF addresses, not "segment") indices to DAF
!     addresses, since the DAF address of the Ith array element is
!     obtained by adding I to the DAF array's base address.
!
!     Key variables:
!
!        Name      Meaning
!        ----      -------
!        BADDR     Segment begin DAF address.
!
!        DIRBAS    Base address of interpolation interval directory.
!
!        EADDR     Segment end DAF address.
!
!        FIRST     Index (mini-segment-relative) of first time tag in
!                  sequence transferred to to output record.
!
!        HIGH      Index (mini-segment-relative) of time tag following
!                  the tag at index LOW (see description below).
!
!        IVBIX     Index in the interpolation interval bounds array of
!                  the start time of the applicable interval.
!
!        IVLBAS    Base address of interpolation interval time bounds.
!
!        IVLSEL    Interval selection flag: this routine selects the
!                  last applicable interval if true; otherwise it
!                  selects the first applicable interval.
!
!        LAST      Index (mini-segment-relative) of last time tag in
!                  sequence transferred to the output record.
!
!        LOW       Index (mini-segment-relative) of last time tag less
!                  than the request time, or of the first time tag if
!                  this tag equals the request time.
!
!        MINIB,
!        MINIE     Mini-segment begin and end DAF addresses. These
!                  addresses are absolute, not segment-relative.
!
!        MINIIX    Interpolation interval/mini-segment index.
!
!        N         Count of interpolation intervals/mini-segments.
!
!        NDIR      Number of interpolation interval time bounds
!                  directories.
!
!        NPKDIR    Number of packet directory entries for current
!                  mini-segment.
!
!        NPKT      Packet count for current mini-segment.
!
!        NRCPKT    Output record packet count. Note that this count,
!                  due to reduction of order at mini-segment
!                  boundaries, may be smaller than the window size
!                  stored in the current mini-segment.
!
!        PKDBAS    Base address of packet directory for current
!                  mini-segment.
!
!        PKTSIZ    Size of packets of current mini-segment.
!
!        SUBTYP    Subtype of current mini-segment.
!
!        TIMBAS    Base address of time tags of current mini-segment.
!
!        WNDSIZ    Interpolation window size of current mini-segment.
!
!     Re-used variables: the variables shown in the list below
!     are used as short-duration variables, much like loop index
!     variables; they are re-used as needed.
!
!        BUFBAS
!        BUFFER
!        GROUP
!        NREAD
!        REMAIN
!
!     Start with a parameter compatibility check on the first
!     pass.
!
      if ( pass1 ) then

         if ( maxrec < maxrsz ) then

            call ctx%setmsg ( 'SPK type 19 record size may be as ' &
            //            'large as #, but SPKPVN record size ' &
            //            '(defined in spkrec.inc) is #.'      )
            call ctx%errint ( '#',  maxrsz                         )
            call ctx%errint ( '#',  maxrec                         )
            call ctx%sigerr ( 'SPICE(BUG)'                         )

         end if

!
!        Indicate the first pass was completed.
!
         pass1 = .false.

      end if

!
!     Unpack the segment descriptor, and get the start and end
!     addresses of the segment.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      type  = ic( 4 )
      baddr = ic( 5 )
      eaddr = ic( 6 )

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et < dc(1) )  .or.  ( et > dc(2) )  )  then

         call ctx%setmsg( 'Request time # is outside of descriptor ' &
         //           'bounds # : #.'                           )
         call ctx%errdp ( '#',  et                                  )
         call ctx%errdp ( '#',  dc(1)                               )
         call ctx%errdp ( '#',  dc(2)                               )
         call ctx%sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                  )
         return

      end if

!
!     Decide whether we're looking at the same segment we saw on the
!     previous call, and whether the interpolation interval used on
!     that call is still applicable.
!
!     Re-use of data from a previous call requires that the saved
!     data were set on a successful call.
!
      samseg  =       ( handle ==  svhan ) &
                .and. ( baddr  ==  svbeg ) &
                .and.                prvok

!
!     Give SAMIVL an initial value. If we do have the
!     same interval, update SAMIVL to indicate this.
!
      samivl = .false.

      if ( samseg ) then
!
!        We now assume that all data saved from the last
!        read of this segment are valid.
!
         if ( svlast ) then
!
!           We pick the last interval containing ET. For
!           all intervals but the last, ET must be
!           less than the interval end time.
!
            if ( svmiix < svn ) then

               samivl =       ( et >= svbtim ) &
                        .and. ( et < svetim )
            else

               samivl =         et >= svbtim &
                        .and. ( et <= svetim )

            end if

         else
!
!           We pick the first interval containing ET. For
!           all intervals but the first, ET must be
!           greater than the interval start time.
!
            if ( svmiix > 1 ) then

               samivl =       ( et  > svbtim ) &
                        .and. ( et  <= svetim )

            else

               samivl =         et  >= svbtim &
                        .and. ( et  <= svetim )

            end if

         end if

      end if

      if ( samseg .and. samivl ) then
!
!        We're looking at the same segment as last time, and the
!        interpolation interval we looked up last time is applicable
!        for the input time ET.
!
!        Simply restore the segment and interval parameters we
!        saved from the previous lookup.
!
!        We don't need to restore the segment start DAF address
!        BADDR, since we've already extracted it from DESCR.
!
!        Restore
!
!           - The mini-segment's packet directory count
!           - The mini-segment's packet directory base address
!
         npkdir = svpknd
         pkdbas = svpkdb

!
!        Restore
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         n      = svn
         miniix = svmiix
         minib  = svminb

!
!        Restore
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment window size
!
         subtyp = svstyp
         pktsiz = svpksz
         npkt   = svnpkt
         wndsiz = svwnsz

      else
!
!        The segment and interval information for the current segment
!        must be looked up.
!
!        Perform checks on this segment.
!
!        Make sure that this really is a type 19 data segment.
!
         if ( type /= 19 ) then

            call ctx%setmsg( 'You are attempting to locate type * ' &
            //           'data in a type 19 data segment.'      )
            call ctx%errint( '*',  type                             )
            call ctx%sigerr( 'SPICE(WRONGSPKTYPE)'                  )
            return
         end if

!
!        Locate the interpolation interval that contains the request
!        time.
!
!        Before getting started, we need to determine which interval to
!        use if the request time lies on a boundary between two
!        intervals. The segment's interval selection flag tells us how
!        to resolve this.
!
         call dafgda ( ctx, handle, eaddr-1, eaddr, contrl )

         if ( ctx%failed() ) return

         isel   = nint ( contrl(1) )
         n      = nint ( contrl(2) )

         ivlsel = isel == itrue

!
!        Determine the number of interval directory entries in the
!        segment. Note that for most SPK types, this computation is
!        performed by computing
!
!           ( N - 1 ) / DIRSIZ
!
!        where N is the segment's epoch count.
!
!        However the set of items in this case is a sequence
!        of N start times followed by a final stop time, so
!        the epoch count is
!
!           N + 1
!
!        and the numerator in the ratio above is incremented by 1.
!
         ndir = n / dirsiz

!
!        Note that the directory placement scheme always leaves
!        a non-empty set of epochs following the last directory
!        entry.
!
!        Let DIRBAS be the base address of the interval directory.
!        We'll compute DIRBAS whether or not the interval directory
!        is non-empty.
!
!        If the interval directory is non-empty, it spans the address
!        range
!
!           DIRBAS+1 : DIRBAS+NDIR
!
!        We compute DIRBAS by starting at the end of the segment
!        and skipping over the control area, the mini-segment
!        start/stop pointers, and the interval directory itself.
!
         dirbas = eaddr - 2 - ( n + 1 ) - ndir

!
!        The way we search the directory depends on the treatment
!        of request times that lie on interval boundaries.
!
         if ( ivlsel ) then
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir > 0 ) then
!
!              Find the last directory entry *less than or equal to*
!              the request time. The directory entry *after* that one,
!              if such exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas

!
!              Fetch the current batch of directory entries.
!
               call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

               if ( ctx%failed() ) return

               remain = ndir - nread

!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               do while (       ( remain        > 0  ) &
                          .and. ( buffer(nread) <= et )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

                  if ( ctx%failed() ) return

                  remain = remain - nread

               end do

!
!              Count the directory entries that are less than or equal
!              to ET. The number we skipped over before the final loop
!              iteration is BUFBAS-DIRBAS. The index of the group of
!              epochs containing ET exceeds the skipped directory count
!              by 1.
!
               group =    bufbas - dirbas &
                       +  lstled ( et, nread, buffer ) &
                       +  1

            else
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )

!
!           Now find the index of the last interval boundary less than
!           or equal to ET. We'll need to read the current group of
!           epochs first, so compute the base of the range of addresses
!           containing this group.

            bufbas = ivbas + ( ( group - 1 ) * dirsiz )

!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            remain = ( n + 1 ) - ( ( group - 1 ) * dirsiz )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            nread  = min ( dirsiz, remain )

            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

            if ( ctx%failed() ) return

!
!           Find the index of the first epoch greater than ET. The case
!           where ET matches the final epoch must be handled here,
!           since in this case no epoch exceeds ET.
!
            iveix =    bufbas - ivbas &
                    +  lstled ( et, nread, buffer ) &
                    +  1

            iveix = min ( iveix,  n+1 )

!
!           Backstop test:
!
            if ( iveix < 2 ) then

               call ctx%setmsg ( 'IVEIX = #.' )
               call ctx%errint ( '#', iveix   )
               call ctx%sigerr ( 'SPICE(BUG)' )
               return

            end if

!
!           The epoch at index IVEIX is the end time of the
!           interpolation interval we'll use. The index of
!           the interval itself is IVEIX - 1.
!
            miniix = iveix - 1

         else
!
!           IVLSEL is .FALSE., meaning we must pick the first interval
!           containing the request time.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir > 0 ) then
!
!              Find the last directory entry *less than* the request
!              time. The directory entry *after* that one, if such
!              exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas
               remain = ndir - nread

!
!              Fetch the current batch of directory entries.
!
               call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

               if ( ctx%failed() ) return

!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
                do while (      ( remain        > 0  ) &
                          .and. ( buffer(nread) < et )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

                  if ( ctx%failed() ) return

                  remain = remain - nread

               end do

!
!              Count the directory entries that are less than ET. The
!              number we skipped over before the final loop iteration
!              is BUFBAS-DIRBAS. The index of the group of epochs
!              containing ET exceeds the skipped directory count by 1.
!
               group =    bufbas - dirbas &
                       +  lstltd ( et, nread, buffer ) &
                       +  1

            else
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )

!
!           Now find the index of the last interval epoch less than ET.
!           We'll need to read the current group of epochs first, so
!           compute the base of the range of addresses containing this
!           group.

            bufbas = ivbas + ( ( group - 1 ) * dirsiz )

!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            remain = ( n + 1 ) - ( ( group - 1 ) * dirsiz )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            nread  = min ( dirsiz, remain )

            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

            if ( ctx%failed() ) return

!
!           Find the index of the last epoch less than ET. The case
!           where ET matches the first epoch must be handled here,
!           since in this case no epoch precedes ET.
!
            ivbix =    bufbas - ivbas &
                    +  lstltd ( et, nread, buffer )

            ivbix = max ( ivbix,  1 )

!
!           Backstop test:
!
            if ( ivbix > n ) then

               call ctx%setmsg ( 'IVBIX = #.' )
               call ctx%errint ( '#', ivbix   )
               call ctx%sigerr ( 'SPICE(BUG)' )
               return

            end if

!
!           The epoch at index IVBIX is the begin time of the
!           interpolation interval we'll use. The index of the interval
!           itself is also IVBIX.
!
            miniix = ivbix

         end if
!
!        This is the end of the IF block that handles mini-segment
!        selection for the two possible values of IVLSEL.
!
!        Look up the begin and end pointers of the mini-segment at index
!        MINIIX. For the first N-1 mini-segments, the "end pointer"
!        of one mini-segment is the "begin" pointer of the next.
!
         bufbas = eaddr - 2 - ( n + 1 ) + ( miniix - 1 )

         call dafgda ( ctx, handle, bufbas+1, bufbas+2, buffer )

         if ( ctx%failed() ) return

         minib = nint ( buffer(1) ) + baddr - 1
!
!        Note that the end of the current mini-segment
!        precedes the start of the next mini-segment by
!        one address.
!
         minie = nint ( buffer(2) ) + baddr - 2

!
!        Look up the time bounds of the mini-segment at index MINIIX.
!        These bounds are used quite a bit farther on, when we save
!        them for future use.
!
         call dafgda ( ctx, handle, ivbas+miniix, ivbas+miniix+1, mintim )

         if ( ctx%failed() ) return

!
!        From this point onward, we'll work with the mini-segment
!        that occupies the address range MINIB : MINIE.
!
!        Look up the control area of the mini-segment.
!
         call dafgda ( ctx, handle, minie-ctrlsz+1, minie, contrl )

         if ( ctx%failed() ) return

!
!        Fetch the control area parameters for the mini-segment.
!
         subtyp  =  nint ( contrl(1) )
         wndsiz  =  nint ( contrl(2) )
         npkt    =  nint ( contrl(3) )

         if (  ( subtyp < 0 ) .or. ( subtyp >= s19nst ) ) then

            call ctx%setmsg ( 'Unexpected SPK type 19 subtype # found in ' &
            //            'type 19 segment within mini-segment #.'     )
            call ctx%errint ( '#',  subtyp                                 )
            call ctx%errint ( '#',  miniix                                 )
            call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'                        )
            return

         end if

         pktsiz = pktszs ( subtyp )
         maxwnd = mxwnsz ( subtyp )

!
!        Check the window size.
!
         if ( ( wndsiz < 2 ) .or. ( wndsiz > maxwnd ) ) then

            call ctx%setmsg ( 'Window size in type 19 segment was #; must ' &
            //            'be in the range 2:# for subtype #. ' &
            //            'Mini-segment index is #.'                   )
            call ctx%errint ( '#',  wndsiz                                 )
            call ctx%errint ( '#',  maxwnd                                 )
            call ctx%errint ( '#',  subtyp                                 )
            call ctx%errint ( '#',  miniix                                 )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                        )
            return

         end if

         if ( odd(wndsiz) ) then

            call ctx%setmsg ( 'Window size in type 19 segment was #; ' &
            //            'must be even for subtype #. ' &
            //            'Mini-segment index is #.'               )
            call ctx%errint ( '#',  wndsiz                             )
            call ctx%errint ( '#',  subtyp                             )
            call ctx%errint ( '#',  miniix                             )
            call ctx%sigerr ( 'SPICE(INVALIDVALUE)'                    )
            return

         end if

!
!        Compute the number of packet directory entries for
!        the current mini-segment/interval.
!
         npkdir  =  (npkt-1) / dirsiz
!
!        Compute the base address of the sequence of packet
!        directory entries for the current mini-segment/interval.
!
         pkdbas  =  minib - 1 + ( npkt * ( pktsiz + 1 ) )

!
!        The test below is done for safety. No SPICE error s
!        should ever be detected at this point.
!
         if ( ctx%failed() ) return

!
!        If we made it this far, we did so without a SPICE error. We
!        have valid segment parameters which can be saved for the next
!        call.
!
!        Save
!
!           - The DAF handle
!           - The segment begin DAF address
!           - The segment's "select last/first interval" flag
!
         svhan  = handle
         svbeg  = baddr
         svlast = ivlsel

!
!        Save the time bounds of the applicable mini-segment/interval.
!
         svbtim = mintim(1)
         svetim = mintim(2)

!
!        Save
!
!           - The mini-segment/interval directory count
!           - The mini-segment/interval directory base address
!
         svpknd = npkdir
         svpkdb = pkdbas

!
!        Save
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         svn    = n
         svmiix = miniix
         svminb = minib

!
!        Save
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment window size
!
         svstyp = subtyp
         svpksz = pktsiz
         svnpkt = npkt
         svwnsz = wndsiz

      end if

!
!     We're ready to construct the output record. The first step is to
!     identify the indices of the packets and epochs corresponding to
!     the request.
!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      if ( npkdir == 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         group  = 1

      else
!
!        There's at least one directory entry. Find the first directory
!        entry whose time is greater than or equal to the request time,
!        if there is such an entry.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  pkdbas
         nread   =  min ( npkdir, dirsiz )
         remain  =  npkdir - nread

         call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

         if ( ctx%failed() ) return

         do while (       ( buffer(nread) < et ) &
                    .and. ( remain        > 0  )   )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, dirsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( ctx, handle, bufbas+1, bufbas+nread, buffer )

            if ( ctx%failed() ) return

         end do

!
!        At this point, BUFBAS - PKDBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - pkdbas ) &
                    +   lstltd ( et, nread, buffer ) &
                    +   1
      end if

!
!     GROUP now indicates the set of epochs in which to search for the
!     request epoch.  If GROUP is 1, the request time lies within the
!     inclusive time interval bounded by the first and last epochs of
!     the first group.  Otherwise, the request time lies in the time
!     interval bounded by the last element of the preceding group and
!     the last element of the current group.
!
!     We'll use the variable names BEGIDX and ENDIDX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( group == 1 ) then

         begidx  =  1
         endidx  =  min ( npkt, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         begidx  =      (  group  - 1 ) * dirsiz
         endidx  =  min (  begidx + dirsiz,  npkt  )

      end if

      timbas  =  pkdbas - npkt

      call dafgda ( ctx, handle, timbas+begidx, timbas+endidx, buffer )

      if ( ctx%failed() ) return

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      i  =  lstltd (  et,  endidx-begidx+1,  buffer  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      if ( i == 0 ) then
         low  =  1
      else
         low  =  begidx + i - 1
      end if

      high  =  low  +  1

!
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike SPK types 8, 9, 12, and 13, for type 19 we allow the
!     window size to shrink when the window must be truncated due to
!     proximity to an interval boundary.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
!     available, the set ranges from index 1 to index LOW.
!

      lsize =  min ( wndsiz/2, low )

!
!     RSIZE is defined analogously for the right half of the window.
!
      rsize =  min (  wndsiz/2,  ( npkt - high + 1 )  )

!
!     The actual window size is simply the sum of LSIZE and RSIZE.
!
      nrcpkt =  lsize + rsize

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      first =  low    -  lsize   +  1

      last  =  first  +  nrcpkt  -  1

!
!     We're ready to construct the output record.
!
!     Put the subtype and window size into the output record.
!
      record(1) =  subtyp

      record(2) =  nrcpkt

!
!     Read the packets.
!
      call dafgda ( ctx, handle, &
                    minib + (first-1)*pktsiz, &
                    minib +      last*pktsiz  -  1, &
                    record(3)                       )

!
!     Finally, add the epochs to the output record.
!     Read the sequence of time tags.
!
      bufbas  =  ( minib - 1 )  +  ( npkt * pktsiz )  +  ( first - 1 )

      call dafgda ( ctx, handle, &
                    bufbas+1, &
                    bufbas+nrcpkt, &
                    record( 3 + nrcpkt*pktsiz )  )

      if ( ctx%failed() ) return

!
!     The call was successful. Record this fact so that saved
!     interval data are available for re-use.
!
      svok = .true.

      return
      end associate
      end subroutine spkr19

      !> Read a single SPK data record from a segment of type 20
      !> (Chebyshev, velocity coefficients only).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for an SPK
      !>              segment of type 20.
      !>
      !>     ET       is an epoch for which a data record from the specified
      !>              segment is required. ET is expressed as seconds past
      !>              J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is an array of data from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of the target body identified
      !>              by the input segment descriptor. The descriptor
      !>              specifies the center of motion and reference frame of
      !>              the state.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for X velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Y velocity component      |
      !>                 +--------------------------------------+
      !>                 | Coeffs for Z velocity component      |
      !>                 +--------------------------------------+
      !>                 | X position component                 |
      !>                 +--------------------------------------+
      !>                 | Y position component                 |
      !>                 +--------------------------------------+
      !>                 | Z position component                 |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Position components have units of km.
      !>                 - Velocity coefficients have units of km/s.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by this routine. See the INCLUDE file
      !>              spkrec.inc for the correct record length.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 20 (Chebyshev polynomials, velocity
      !>     only) segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 20 ) THEN
      !>              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (IMU)
      module subroutine spkr20 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: j2000, spd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      real(wp) :: dc       ( 2 )
      real(wp) :: dscale
      real(wp) :: init
      real(wp) :: initfr
      real(wp) :: initjd
      real(wp) :: intlen
      real(wp) :: intrvl
      real(wp) :: mid
      real(wp) :: pos      ( 3 )
      real(wp) :: radius
      real(wp) :: recbeg
      real(wp) :: tscale

      integer(ip) :: begin
      integer(ip) :: end
      integer(ip) :: i
      integer(ip) :: ic       ( 6 )
      integer(ip) :: loc
      integer(ip) :: nrec
      integer(ip) :: nterms
      integer(ip) :: recadr
      integer(ip) :: recno
      integer(ip) :: recsiz
      integer(ip) :: size

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      begin = ic( 5 )
      end   = ic( 6 )

!
!     The segment is made up of a number of logical records, each
!     having the same size, and covering the same length of time.
!
!     We can determine which record to return using the input epoch,
!     the integer and fractional parts of the initial time of the first
!     record's coverage interval, and the length of the interval
!     covered by each record. These constants are located at the end of
!     the segment, along with the size of each logical record and the
!     total number of records.
!
!     For convenience, we'll fetch the segment's distance and time
!     scales in the same call.
!
      call dafgda ( ctx, handle, end-6, end, record )

      dscale = record( 1 )
      tscale = record( 2 )
      initjd = record( 3 )
      initfr = record( 4 )
      intlen = record( 5 )
      recsiz = int( record( 6 ) )
      nrec   = int( record( 7 ) )

!
!     NTERMS is the number of velocity coefficients per component,
!     plus 1 (for the position component).
!
      nterms = recsiz / 3

!
!     Convert the initial epoch and interval length to
!     seconds past J2000 TDB.
!
      init   = (  ( initjd - j2000 )  +  initfr  ) * spd
      intrvl = intlen * spd

!
!     Locate the record containing the coefficients to use.
!
      recno  = int( (et - init) / intrvl ) + 1
      recno  = max( 1,  min( recno, nrec )  )

!
!     Compute the midpoint and radius of the record at
!     index RECNO. We want to compute the midpoint in such
!     a way that we take advantage of interval lengths that
!     are exactly representable, when we have them.
!
!     RECBEG is the record start time, minus the fractional
!     part of the segment start time, expressed as seconds
!     past J2000. We'll account for the fractional part of the
!     start time below when we compute MID.
!
      recbeg = (  ( initjd - j2000 )  +  (recno-1)*intlen  ) * spd

      radius = intrvl / 2.0_wp

      mid    = recbeg + ( initfr * spd ) + radius

!
!     Compute the address of the desired record.
!
      recadr = ( recno - 1 )*recsiz + begin

!
!     Along with the record, return the size, midpoint, and
!     radius of the record.
!
      record( 1 ) = real( recsiz + 2, wp )
      record( 2 ) = mid
      record( 3 ) = radius
      call dafgda ( ctx, handle, recadr, recadr + recsiz - 1, record( 4 ) )

!
!     We're going to re-arrange the record: the position components
!     will be transferred to the end of the record, and the record
!     contents will be left-shifted to fill in the free elements.
!
      do i = 1, 3
         pos( i ) = record ( 3  +  i*nterms )
      end do

      size = recsiz + 3
!
!     Remove the position elements from the record.
!
      do i = 1, 3
!
!        LOC is the index of the element to delete. After the first
!        removal, we must account for the resulting left shift when
!        calculating the indices of subsequent elements to be removed.
!
         loc = 3 + i*nterms - (i-1)

         call remlad ( ctx, 1, loc, record, size )
!
!        Note that SIZE is an in-out argument; on output it indicates
!        the size of the array after removal of the indicated
!        element(s).
!
      end do

!
!     Convert the position vector to km.
!
      pos = dscale*pos

!
!     Append the position to the record. Since we inserted three
!     elements at the start of the record and deleted three position
!     elements, the target index is the same as if we had copied the
!     record directly to the output array.
!
      record(recsiz+1:recsiz+1+3-1) = pos

!
!     Convert the velocity Chebyshev coefficients to units of km/s.
!
      do i = 4, recsiz
         record(i) = record(i) * (dscale/tscale)
      end do

      return
      end subroutine spkr20

      !> Read a single SPK data record from a segment of type 21
      !> (Extended Difference Lines).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 21.
      !>
      !>     ET       is an epoch for which a data record from a specific
      !>              segment is required. The epoch is represented as
      !>              seconds past J2000 TDB.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of an
      !>              ephemeris object, relative to its center of motion,
      !>              in an inertial reference frame.
      !>
      !>              The contents of RECORD are as follows:
      !>
      !>                 RECORD(1):         The difference table size per
      !>                                    Cartesian component. Call this
      !>                                    size MAXDIM; then the difference
      !>                                    line (MDA) size DLSIZE is
      !>
      !>                                      ( 4 * MAXDIM ) + 11
      !>
      !>                 RECORD(2)
      !>                    ...
      !>                 RECORD(1+DLSIZE):  An extended difference line.
      !>                                    The contents are:
      !>
      !>                    Dimension  Description
      !>                    ---------  ----------------------------------
      !>                    1          Reference epoch of difference line
      !>                    MAXDIM     Stepsize function vector
      !>                    1          Reference position vector,  x
      !>                    1          Reference velocity vector,  x
      !>                    1          Reference position vector,  y
      !>                    1          Reference velocity vector,  y
      !>                    1          Reference position vector,  z
      !>                    1          Reference velocity vector,  z
      !>                    MAXDIM,3   Modified divided difference
      !>                               arrays (MDAs)
      !>                    1          Maximum integration order plus 1
      !>                    3          Integration order array
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the maximum table size of the input record exceeds
      !>         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled.
      !>
      !>     2)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 21 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 1 ) THEN
      !>              CALL SPKR21 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     F.T. Krogh         (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
      module subroutine spkr21 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstltd
      use pepper_inc_spk21
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.

      integer(ip), parameter :: bufsiz = 100

      real(wp) :: data   ( bufsiz )
      real(wp) :: dc     ( 2 )

      integer(ip) :: begin
      integer(ip) :: dflsiz
      integer(ip) :: end
      integer(ip) :: i
      integer(ip) :: ic     ( 6 )
      integer(ip) :: maxdim
      integer(ip) :: ndir
      integer(ip) :: nrec
      integer(ip) :: off
      integer(ip) :: offd
      integer(ip) :: offe
      integer(ip) :: offr
      integer(ip) :: recno

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the segment descriptor.
!
      call dafus ( ctx, descr, 2, 6, dc, ic )

      begin = ic(5)
      end   = ic(6)

!
!     Get the number of records in the segment. From that, we can
!     compute
!
!        NDIR      The number of directory epochs.
!
!        OFFD      The offset of the first directory epoch.
!
!        OFFE      The offset of the first epoch.
!
!     the number of directory epochs.
!
!     We'll fetch the difference table dimension as well.
!
      call dafgda ( ctx, handle, end-1, end, data )

      nrec   = nint ( data(2) )
      ndir   = nrec / bufsiz
      offd   = end  - ndir - 2
      offe   = offd - nrec

      maxdim = nint ( data(1) )

      if ( maxdim > maxtrm ) then

         call ctx%setmsg ( 'The input record has a maximum table ' &
         //            'dimension of #, while the maximum ' &
         //            'supported by this routine is #. It is ' &
         //            'possible that this problem is due to ' &
         //            'your SPICE Toolkit being out of date.' )
         call ctx%errint ( '#',  maxdim                            )
         call ctx%errint ( '#',  maxtrm                            )
         call ctx%sigerr ( 'SPICE(DIFFLINETOOLARGE)'               )
         return

      end if

!
!     The difference line dimension per component is the
!     first element of the output record.
!
      record( 1 ) = maxdim

!
!     Set the difference line size.
!
      dflsiz = ( 4 * maxdim ) + 11

!
!     What we want is the record number: once we have that, we can
!     compute the offset of the record from the beginning of the
!     segment, grab it, and go. But how to find it?
!
!     Ultimately, we want the first record whose epoch is greater
!     than or equal to ET. If there are BUFSIZ or fewer records, all
!     the record epochs can be examined in a single group.
!
      if ( nrec <= bufsiz ) then

         call dafgda ( ctx, handle, offe+1, offe+nrec, data )
         recno = lstltd ( et, nrec, data ) + 1

         offr = (begin - 1) + (recno - 1) * dflsiz
         call dafgda ( ctx, handle, offr+1, offr+dflsiz, record(2) )

         return

      end if

!
!     Searching directories is a little more difficult.
!
!     The directory contains epochs BUFSIZ, 2*BUFSIZ, and so on. Once
!     we find the first directory epoch greater than or equal to ET, we
!     can grab the corresponding set of BUFSIZ record epochs, and
!     search them.
!
      do i = 1, ndir

         call dafgda ( ctx, handle, offd+i, offd+i, data )

         if ( data(1) >= et ) then

            off = offe + (i - 1) * bufsiz
            call dafgda ( ctx, handle, off+1, off+bufsiz, data )

            recno = ((i - 1) * bufsiz) + lstltd ( et, bufsiz, data ) + 1

            offr = (begin - 1) + (recno - 1) * dflsiz
            call dafgda ( ctx, handle, offr+1, offr+dflsiz, record(2) )

            return

         end if

      end do

!
!     If ET is greater than the final directory epoch, we want one
!     of the final records.
!
      i = mod ( nrec, bufsiz )

      call dafgda ( ctx, handle, end-ndir-i-1, end-ndir-2, data )

      recno = (ndir * bufsiz) + lstltd ( et, i, data ) + 1

      offr = (begin - 1) + (recno - 1) * dflsiz

      call dafgda ( ctx, handle, offr+1, offr+dflsiz, record(2) )

      return
      end subroutine spkr21

      !> Compute the values of the Stumpff functions C_0 through C_3 at
      !> a specified point.
      !>
      !>## Detailed Input
      !>
      !>     X        is the argument to use in each of the Stumpff functions
      !>              C_0, C_1, C_2, and C_3.
      !>
      !>## Detailed Output
      !>
      !>     C0,
      !>     C1,
      !>     C2,
      !>     C3       are the values of the Stumpff functions C_0(X), C_1(X),
      !>              C_2(X), and C_3(X).
      !>
      !>## Parameters
      !>
      !>     TRUNC    is the Maclaurin series for C_3 and C_2 respectively are:
      !>
      !>                                     2     3                 k
      !>                        1     X     X     X              (-X)
      !>              C_3(X) =  --- - --- + --- - --- + . . . + ----------. . .
      !>                        3!    5!    7!    9!           (3 + 2*K)!
      !>
      !>              and
      !>
      !>                                     2     3                 k
      !>                        1     X     X     X              (-X)
      !>              C_2(X) =  --- - --- + --- - --- + . . . + ----------. . .
      !>                        2!    4!    6!    8!           (2 + 2*K)!
      !>
      !>              These series are used in the evaluation of C_3 and C_2.
      !>              Thus, it is necessary to make a decision about where to
      !>              truncate the series in our evaluation of C_3 and C_2.
      !>
      !>              TRUNC is used to tell this routine where to truncate
      !>              the Maclaurin series for C_3 and C_2.
      !>
      !>              The value of TRUNC for your machine  is the smallest
      !>              integer such that
      !>
      !>                                1
      !>                  1.0D0  +  ----------        =  1.0D0
      !>                            (2*TRUNC)!
      !>
      !>              The following program will (if compiled and linked)
      !>              will produce the values of TRUNC for your machine.
      !>
      !>              INTEGER               TRUNC
      !>
      !>              DOUBLE PRECISION      DENOM
      !>              DOUBLE PRECISION      FACTR
      !>
      !>              DOUBLE PRECISION      X
      !>
      !>              DENOM = 2.0D0
      !>              FACTR = 2.0D0
      !>              TRUNC = 1
      !>
      !>              X      = 1.0D0 / DENOM
      !>
      !>              DO WHILE ( 1.0D0 + X .GT. 1.0D0 )
      !>                 DENOM = DENOM * (2.0D0+FACTR) * (1.0D0+FACTR)
      !>                 FACTR = FACTR +  2.0D0
      !>                 TRUNC = TRUNC +  1
      !>                 X     = 1.0D0 /  DENOM
      !>              END DO
      !>
      !>              WRITE (*,*) 'The value of TRUNC is: ', TRUNC
      !>
      !>              END
      !>
      !>## Exceptions
      !>
      !>     1)  If the input value of X is not in the domain of values
      !>         for which the Stumpff functions can be computed, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>         The range of valid inputs is from  -[ln(2) + ln(DPMAX)]**2
      !>         to DPMAX.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the values of the Stumpff functions C_0,
      !>     C_1, C_2, and C_3 at the input X.
      !>
      !>     The Stumpff function C_k(X) for k = 0, 1, ... is given by the
      !>     series:
      !>
      !>                                 2        3                  m
      !>                1      X        X        X               (-X)
      !>     C_k(X) =  --- - ------ + ------ - ------ + . . . + ------- + . . .
      !>                k!   (k+2)!   (k+4)!   (k+6)!           (k+2m)!
      !>
      !>     These series converge for all real values of X.
      !>
      !>## Examples
      !>
      !>     For positive X,
      !>
      !>        C_0(X)   =  COS ( DSQRT(X) )
      !>
      !>                    SIN ( DSQRT(X) )
      !>        C_1(X)   =  ---------------
      !>                          DSQRT(X)
      !>
      !>                    1 - COS ( DSQRT(X) )
      !>        C_2(X)   = ---------------------
      !>                           X
      !>
      !>                    1  -  SIN ( DSQRT(X) ) / DSQRT(X)
      !>        C_3(X)   =  ----------------------------------
      !>                              X
      !>
      !>     Thus the following block of code can be used to check this
      !>     routine for reasonableness:
      !>
      !>     INTEGER               I
      !>
      !>     DOUBLE PRECISION      X
      !>     DOUBLE PRECISION      ROOTX
      !>
      !>     DOUBLE PRECISION      TC0
      !>     DOUBLE PRECISION      TC1
      !>     DOUBLE PRECISION      TC2
      !>     DOUBLE PRECISION      TC3
      !>
      !>     DOUBLE PRECISION      C0
      !>     DOUBLE PRECISION      C1
      !>     DOUBLE PRECISION      C2
      !>     DOUBLE PRECISION      C3
      !>
      !>     DO I = 1, 10
      !>
      !>        X     = DBLE (I)
      !>        ROOTX = DSQRT(X)
      !>
      !>        TC0   = COS ( ROOTX )
      !>        TC1   = SIN ( ROOTX ) / ROOTX
      !>
      !>        TC2   = ( 1.0D0 - COS( ROOTX )         ) / X
      !>        TC3   = ( 1.0D0 - SIN( ROOTX ) / ROOTX ) / X
      !>
      !>        CALL STMP03 ( X, C0, C1, C2, C3 )
      !>
      !>        WRITE (*,*)
      !>        WRITE (*,*) 'Expected - Computed for X = ', X
      !>        WRITE (*,*)
      !>        WRITE (*,*) 'Delta C0 :', TC0 - C0
      !>        WRITE (*,*) 'Delta C1 :', TC1 - C1
      !>        WRITE (*,*) 'Delta C2 :', TC2 - C2
      !>        WRITE (*,*) 'Delta C3 :', TC3 - C3
      !>
      !>     END DO
      !>
      !>     END
      !>
      !>     You should expect all of the differences to be on the order of
      !>     the precision of the machine on which this program is executed.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  J. Danby, "Fundamentals of Celestial Mechanics," 2nd Edition,
      !>          pp.345-347, Willman-Bell, 1989.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.29.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 3.28.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.27.0, 08-APR-2014 (NJB)
      !>
      !>        Updated in-line documentation and cleaned up
      !>        code following changes made in version 3.21.0.
      !>
      !>-    SPICELIB Version 3.26.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 3.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 3.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 3.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 3.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 3.21.0, 09-APR-2012 (WLT)
      !>
      !>        Code was updated to correct excessive round-off
      !>        errors in the case where |X| > 1.
      !>
      !>-    SPICELIB Version 3.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 3.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 3.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 3.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 3.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 3.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 3.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 3.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 3.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 3.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 3.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 3.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 3.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 3.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 3.0.3, 24-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 3.0.0, 08-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
      !>
      !>        The file was modified to include values for other platforms.
      !>        Also, the file was formatted for use by the program that
      !>        creates the environment specific source files.
      !>
      !>-    SPICELIB Version 1.0.0, 17-FEB-1992 (WLT)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 3.27.0, 08-APR-2014 (NJB) (WLT)
      !>
      !>        In version 3.21.0, the routine was re-written to use the
      !>        standard trigonometric and hyperbolic trigonometric formulas
      !>        for the Stumpff functions for input arguments having magnitude
      !>        greater than or equal to 1. This was done to prevent loss of
      !>        accuracy for some input values.
      !>
      !>        In version 3.27.0, the code was cleaned up: unreachable code
      !>        was deleted, and comments were changed to match the updated
      !>        code.
      !>
      !>        The derivation of the argument mapping formulas has been
      !>        retained as an appendix in a comment section at the end of
      !>        this source file. These formulas may be used a future revision
      !>        of this routine.
      !>
      !>-    SPICELIB Version 3.0.0, 08-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN)
      !>
      !>        The file was modified to include values for other platforms.
      !>        Also, the file was formatted for use by the program that
      !>        creates the environment specific source files.
      module subroutine stmp03 ( ctx, x, c0, c1, c2, c3 )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: x  !! Argument to each Stumpff function C_0 to C_3.
      real(wp), intent(out) :: c0  !! Value of C_0(X)
      real(wp), intent(out) :: c1  !! Value of C_1(X)
      real(wp), intent(out) :: c2  !! Value of C_2(X)
      real(wp), intent(out) :: c3  !! Value of C_3(X)
!     PEPPER: SPICELIB truncated the series at 10 terms for Intel Fortran
!     on Windows (#if) and at 11 everywhere else; 11 is used everywhere.
      integer(ip), parameter :: trunc = 11
!
!     The integers NPAIRS, LPAIR2, and LPAIR3 are used to declare
!     space for Maclaurin series coefficients and for determining how
!     many terms of these series to use in the computation of
!     C_2 and C_3.
!
!     Here's what is supposed to be true.
!
!        1/(TRUNC*2)!  + 1.0D0 = 1.0D0
!
!     using this machine's double precision arithmetic.
!
!     We will map the input X to a value y between -1 and 1 and then
!     construct the values of the functions at X from their values at y.
!     Since we will only evaluate the series expansion for C_2 and C_3
!     for values of y between -1 and 1, its easy to show that we don't
!     need to consider terms in the series whose coefficients have
!     magnitudes less than or equal 1/(2*TRUNC)! .
!
!     If the value of TRUNC is 10, then the series expansions for
!     C_2(y) and C_3(y) are can be truncated as shown here:
!
!                                   2             7       8
!              .    1      y       y             y       y
!       C_3(y) =   --- -  ---  +  ---  +  ... - ---  +  ---
!                   3!     5!      7!           17!     19!
!
!                 1        y         y            y           y
!              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
!                2*3      4*5       6*7          16*17       18*19
!
!              .    1      y       y             y       y
!       C_2(y) =   --- -  ---  +  ---  +  ... + ---  -  ---
!                   2!     4!      6!           16!     18!
!
!                 1        y         y            y           y
!              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
!                1*2      3*4       5*6          15*16       17*18
!
!     As is evident from the above, we are going to need the
!     "reciprocal pairs"
!
!       1/(1*2),  1/(2*3),  1/(3*4), 1/(4*5), ...
!
!     The number of such fractions be computed directly from
!     TRUNC. LPAIR3 and LPAIR2 indicate which of these pairs
!     (counting 1/(1*2) as the first) will be the last one needed in
!     the evaluation of C_2 and C_3.
!
      integer(ip), parameter :: npairs = 2*trunc - 2
      integer(ip), parameter :: lpair3 = npairs
      integer(ip), parameter :: lpair2 = npairs - 1
!
!     Local variables
!
      real(wp) :: y
      real(wp) :: z
      integer(ip) :: i
      if ( .not. allocated ( ctx%stmp03_state ) ) allocate ( ctx%stmp03_state )
      associate ( first => ctx%stmp03_state%first, &
                  lbound => ctx%stmp03_state%lbound, &
                  pairs => ctx%stmp03_state%pairs )
!
!     We are going to need the numbers
!
!        1/(2*3), 1/(3*4), 1/(4*5), ...
!
!     but we don't want to compute them every time this routine is
!     called.  So the first time this routine is called we compute
!     them and put them in the array PAIRS for use on subsequent
!     calls. (This could be done via parameters, but computing them
!     at run time seems to have a better chance of being
!     easily maintained.)
!
!     In addition we will need to compute the lower bound for which
!     C_0,...,C_3 can be computed.  This lower bound is computed by
!     noting that C_0 has the largest magnitude of all the Stumpff
!     functions over the domain from -infinity to -1.  Moreover, in this
!     range
!
!        C_0(X) = Cosh( SQRT(-X) )
!
!     Thus the range of X for which the Stumpff functions can be
!     computed is bounded below by the value of X for which
!
!        Cosh ( SQRT(-X) ) = DPMAX
!
!     Which implies the lower bound for valid inputs is at
!
!        X = - ( DLOG ( 2.0 ) + DLOG( DPMAX ) ) ** 2
!
!          = - ( DLOG ( 2*N ) + DLOG ( DPMAX/N  ) ) ** 2
!
!     We point out the second formulation of the bound just in case
!     your compiler can't handle the computation of DLOG ( DPMAX ).
!     If this unfortunate situation should arise, complain to the
!     company that produces your compiler and in the code below
!     compute LBOUND using the second form above with N equal to
!     some large power of 2 (say 2**20).
!
      if ( first ) then

         first  = .false.

         do i = 1, npairs
            pairs(i) = 1.0_wp / (real(i, wp)*real(i+1, wp))
         end do

         y      =   dlog(2.0_wp) + dlog( dpmax )
         lbound = - y*y

      end if

!
!     First we make sure that the input value of X is within the
!     range that we are confident we can use to compute the Stumpff
!     functions.
!
      if ( x <= lbound ) then

         call ctx%setmsg( 'The input value of X must be greater than ' &
         //           '#.  The input value was #'                  )
         call ctx%errdp ( '#', lbound                                  )
         call ctx%errdp ( '#', x                                       )
         call ctx%sigerr( 'SPICE(VALUEOUTOFRANGE)'                     )
         return

      end if

!
!     From the definition of the Stumpff functions it can be seen that
!     C_0(X), C_1(X) are given by
!
!          COS ( DSQRT(X) )   and   SIN ( DSQRT(X) ) / DSQRT(X)
!
!     for positive X. Moreover, the series used to define them converges
!     for all real X.
!
!     These functions have a number of simple relationships that make
!     their computations practical.  Among these are:
!
!                         1
!          x*C_k+2(x) =  ---  -  C_k(x)
!                         k!
!
!                                 2
!           C_0(4x)   =  2*[ C_0(x) ]  -  1
!
!           C_1(4x)   =    C_1(x)*C_0(x)
!
!                                 2
!           C_2(4x)   =   [C_1(x)]  / 2
!
!           C_3(4x)   = [ C_2(x) + C_0(x)*C_3(x) ] / 4
!
!      These can be used to derive formulae for C_0(16x) ... C_3(16x)
!      that involve only C_0(x) ... C_3(x).  If we let
!
!                                      2
!                     Z        = C_0(x)  - 0.5
!
!      and
!
!                     W        = 2*C_0(x)*C_1(x)
!
!      then
!
!                                   2
!                     C_0(16x) = 8*Z  -  1
!
!                     C_1(16x) = W*Z
!
!                                 2
!                     C_2(16x) = W  / 8
!
!                                       2
!                                 C_1(x)  + Z*[C_2(x) + C_0(x)*C_3(x)]
!                     C_3(16x) =  ----------------------------------
!                                                  8
!
       if ( x < -1.0_wp ) then

         z  = sqrt(-x)
         c0 = cosh(z)
         c1 = sinh(z)/z
         c2 = (1-c0)/x
         c3 = (1-c1)/x
         return

      end if

      if ( x > 1.0_wp ) then

         z  = sqrt(x)
         c0 = cos(z)
         c1 = sin(z)/z
         c2 = (1-c0)/x
         c3 = (1-c1)/x
         return

      end if

!
!     If the magnitude of X is less than or equal to 1, we compute
!     the function values directly from their power series
!     representations.
!
!     Compute C_3 of x :
!
!                                   2             7       8
!              .    1      x       x             x       x
!       C_3(x) =   --- -  ---  +  ---  +  ... - ---  +  ---
!                   3!     5!      7!           17!     19!
!
!                 1        x         x            x           x
!              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
!                2*3      4*5       6*7          16*17       18*19
!
!                 ^        ^         ^             ^           ^
!                 |        |         |             |           |
!                 |        |         |             |           |
!              PAIR(2)  PAIR(4)   PAIR(6)  ...  PAIR(16)    PAIR(18)
!
!     Assuming that we don't need to go beyond the term with 1/19!,
!     LPAIR3 will be 18.
!

      c3    = 1.0_wp

      do i  = lpair3, 4, -2
         c3 = 1.0_wp - x*pairs(i)*c3
      end do

      c3    =           pairs(2)*c3

!
!     Compute C_2 of x  :
!
!        Here's how we do it.
!                                   2             7       8
!              .    1      x       x             x       x
!       C_2(x) =   --- -  ---  +  ---  +  ... + ---  -  ---
!                   2!     4!      6!           16!     18!
!
!                 1        x         x            x           x
!              = ---( 1 - --- ( 1 - --- (...( 1- ----- ( 1 - ----- )...)
!                1*2      3*4       5*6          15*16       17*18
!
!                 ^        ^         ^             ^           ^
!                 |        |         |             |           |
!                 |        |         |             |           |
!              PAIR(1)  PAIR(3)   PAIR(5)  ...  PAIR(15)    PAIR(17)
!
!     Assuming that we don't need to go beyond  the term with 1/18!,
!     LPAIR2 will be 17.
!
      c2    = 1.0_wp

      do i  = lpair2, 3, -2
         c2 = 1.0_wp - x*pairs(i)*c2
      end do

      c2    =           pairs(1)*c2

!
!     Get C1 and C0 via the recursion formula:
!
!                         1
!          x*C_k+2(y) =  ---  -  C_k(x)
!                         k!
!
      c1 =  1.0_wp - x*c3
      c0 =  1.0_wp - x*c2

      return
      end associate
      end subroutine stmp03

      !> The function returns the NAIF integer ID code of the barycenter,
      !> if any, associated with BODY.
      !>
      !> Return the barycenter code associated with a body belonging to
      !> a planetary system.  For other bodies, simply return the
      !> input ID code.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>      BODY        is the integer ID code of the body for which the
      !>                  barycenter ID code is requested.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the NAIF integer ID code of the barycenter,
      !>     if any, associated with BODY.  If BODY is not the NAIF integer
      !>     ID code of a planet or satellite, the value BODY is returned.
      !>
      !>     Planetary barycenter codes are the integers 1, ..., 9.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1) If BODY is not the NAIF integer ID code of a planet or
      !>        satellite, the value BODY is returned.  This case is
      !>        not considered to be an error.
      !>
      !>     2) Codes of the form
      !>
      !>           PXNNN, where
      !>
      !>                  P   is    1, ...,  9,
      !>                  X   is    1, 2, 3, 4, 6, 7, 8, 9
      !>              and NNN is  001, ... 999
      !>
      !>        are mapped to the integer P.  These codes are not
      !>        considered to be erroneous, though they were not
      !>        part of the planned satellite numbering scheme at
      !>        the date this routine was released.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine enables the caller to determine to which
      !>     planetary system, if any, a planet or natural satellite belongs.
      !>     This capability is used by the SPICELIB PCK subsystem.
      !>
      !>     Planets have ID codes of the form P99, where P is 1, ..., 9.
      !>
      !>     Natural satellites have ID codes of the form
      !>
      !>        PNN, where
      !>
      !>               P  is  1, ..., 9
      !>           and NN is 01, ... 98
      !>
      !>     or
      !>
      !>        PXNNN, where
      !>
      !>               P   is    1, ...,  9,
      !>               X   is    0  or    5,
      !>           and NNN is  001, ... 999
      !>
      !>     Codes with X = 5 are provisional.
      !>
      !>## Examples
      !>
      !>     1) Find the planetary system (indicated by a barycenter ID
      !>        code) associated with the ID code 65001 (a provisional code
      !>        for a Saturnian satellite):
      !>
      !>            BODY = 65001
      !>            BARY = ZZBODBRY ( BODY )
      !>
      !>        BARY is assigned the value 6.
      !>
      !>     2) Find the planetary system associated with the ID code
      !>        60001 (an "extended" code for a Saturnian satellite):
      !>
      !>            BODY = 60001
      !>            BARY = ZZBODBRY ( BODY )
      !>
      !>        BARY is assigned the value 6.
      !>
      !>     3) Find the planetary system associated with the ID code
      !>        606 (Titan):
      !>
      !>            BODY = 606
      !>            BARY = ZZBODBRY ( BODY )
      !>
      !>        BARY is assigned the value 6.
      !>
      !>     4) Find the planetary system associated with the ID code
      !>        699 (Saturn):
      !>
      !>            BODY = 699
      !>            BARY = ZZBODBRY ( BODY )
      !>
      !>        BARY is assigned the value 6.
      !>
      !>     5) Find the planetary system associated with the ID code 6
      !>        (Saturn system barycenter):
      !>
      !>            BODY = 6
      !>            BARY = ZZBODBRY ( BODY )
      !>
      !>        BARY is assigned the value 6.
      !>
      !>     6) Find the planetary system associated with the ID code
      !>        9511010 (asteroid Gaspra):
      !>
      !>            BODY = 9511010
      !>            BARY = ZZBODBRY ( BODY )
      !>
      !>        BARY is assigned the value 9511010.
      !>
      !>## Restrictions
      !>
      !>     1) This routine should not be called from routines outside
      !>        of SPICELIB.  The interface and functionality may change
      !>        without notice.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 01-FEB-2004 (NJB)
      module integer(ip) function zzbodbry ( ctx, body )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! ID code of body.
      if (  ( body >= 100 ) .and. ( body <= 999 )  ) then
!
!        BODY is a "traditional" NAIF planet or natural satellite
!        ID code.
!
         zzbodbry = body / 100

      else if (  ( body >= 10000 ) .and. ( body <= 99999 )  ) then
!
!        BODY is an "extended" NAIF natural satellite ID code.
!
         zzbodbry = body / 10000

      else
!
!        BODY is a barycenter code or is not associated with a
!        planetary system.  In either case, we simply return
!        the input value BODY.
!
         zzbodbry = body

      end if

      return
      end function zzbodbry

      !> This is the routine that unpacks a set integer numbers stored in
      !> a single double precision number.
      !>
      !> Its current specific use is to "uncompress" seven integer numbers
      !> representing numbers of polynomial coefficients in a logical
      !> type 4 CK record from a single DP number stored in a physical
      !> type 4 CK record in a file.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     DPCOEF     is a DP number containing NSETS integers packed
      !>                together.
      !>
      !>     NSETS      is the number of integers packed in the DPCOEF.
      !>
      !>     PARCOD     is the packing base.
      !>
      !>## Detailed Output
      !>
      !>     I          is an array containing unpacked integers.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Particulars
      !>
      !>     This routine unpacks NSETS integers packed in a single double
      !>     precision number using base specified by PARCOD and stored them
      !>     in the array I. The integers are packed in the DP using the
      !>     following algorithm:
      !>
      !>        [DPCOEF]= PARCOD ** ( NSETS - 1 ) * I( 1 )        +
      !>                  PARCOD ** ( NSETS - 2 ) * I( 2 )        +
      !>                  ...
      !>                  PARCOD ** 1             * I( NSETS - 1 )+
      !>                  PARCOD ** 0             * I( NSETS )
      !>     where:
      !>
      !>        I(1:NSETS) - is an array of integer numbers with values
      !>                     in the range [0:PARCOD-1].
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     Output integer array I must have enough space to hold NSETS
      !>     numbers.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     Y.K. Zaiko     (JPL)
      !>     B.V. Semenov   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
      module subroutine zzck4d2i ( ctx, dpcoef, nsets, parcod, i )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(inout) :: dpcoef  !! DP number containing packed integer numbers. (PEPPER: it is overwritten while unpacking)
      integer(ip), intent(in) :: nsets  !! Number of integer components packed in DPCOEF.
      real(wp), intent(in) :: parcod  !! Packing base.
      integer(ip), intent(out) :: i      ( * )  !! Array of NSETS integer components.

!
!     Local variables.
!
      real(wp) :: x
      integer(ip) :: k

!
!     Let's unpack it!
!
      x = parcod ** ( nsets - 1 )

      do k = 0, nsets - 1

         i( nsets - k ) = dpcoef / x
         dpcoef = dpcoef - i( nsets - k ) * x
         x = x / parcod

      end do

!
!     All done.
!
      return

      end subroutine zzck4d2i

      !> This subroutine provides deep space common items used by both the
      !> secular and periodics subroutines.
      !>
      !> See Detailed_input and Detailed_Output.
      !>
      !> This routine previously had the name DPPER, but the functions
      !> inside weren't well organized.
      !>
      !>## Detailed Input
      !>
      !>    EPOCH       Epoch of TLE set as Julian day value.
      !>
      !>    EP          Eccentricity
      !>
      !>    ARGPP       Argument of perigee
      !>
      !>    TC          Minutes past EPOCH, nominally zero.
      !>
      !>    INCLP       Inclination
      !>
      !>    NODEP       Right ascension of ascending node
      !>
      !>    NP          Mean motion
      !>
      !>## Detailed Output
      !>
      !>    SINIM       SIN of mean inclination.
      !>
      !>    COSIM       COS of mean inclination.
      !>
      !>    SINOMM      Internal SGD4 parameter.
      !>
      !>    COSOMM      Internal SGD4 parameter.
      !>
      !>    SNODM       Internal SGD4 parameter.
      !>
      !>    CNODM       Internal SGD4 parameter.
      !>
      !>    DAY         Internal SGD4 parameter.
      !>
      !>    E3          Internal SGD4 parameter.
      !>
      !>    EE2         Internal SGD4 parameter.
      !>
      !>    EM          Eccentricity.
      !>
      !>    EMSQ        Eccentricity squared.
      !>
      !>    GAM         Internal SGD4 parameter.
      !>
      !>    PEO         Internal SGD4 parameter.
      !>
      !>    PGHO        Internal SGD4 parameter.
      !>
      !>    PHO         Internal SGD4 parameter.
      !>
      !>    PINCO       Internal SGD4 parameter.
      !>
      !>    PLO         Internal SGD4 parameter.
      !>
      !>    RTEMSQ      Internal SGD4 parameter.
      !>
      !>    SE2         Internal SGD4 parameter.
      !>
      !>    SE3         Internal SGD4 parameter.
      !>
      !>    SGH2        Internal SGD4 parameter.
      !>
      !>    SGH3        Internal SGD4 parameter.
      !>
      !>    SGH4        Internal SGD4 parameter.
      !>
      !>    SH2         Internal SGD4 parameter.
      !>
      !>    SH3         Internal SGD4 parameter.
      !>
      !>    SI2         Internal SGD4 parameter.
      !>
      !>    SI3         Internal SGD4 parameter.
      !>
      !>    SL2         Internal SGD4 parameter.
      !>
      !>    SL3         Internal SGD4 parameter.
      !>
      !>    SL4         Internal SGD4 parameter.
      !>
      !>    S1          S coefficients
      !>
      !>    S2             ...
      !>
      !>    S3             ...
      !>
      !>    S4             ...
      !>
      !>    S5             ...
      !>
      !>    S6             ...
      !>
      !>    S7             ...
      !>
      !>    SS1            ...
      !>
      !>    SS2            ...
      !>
      !>    SS3            ...
      !>
      !>    SS4            ...
      !>
      !>    SS5            ...
      !>
      !>    SS6            ...
      !>
      !>    SS7            ...
      !>
      !>    SZ1            ...
      !>
      !>    SZ2            ...
      !>
      !>    SZ3            ...
      !>
      !>    SZ11           ...
      !>
      !>    SZ12           ...
      !>
      !>    SZ13           ...
      !>
      !>    SZ21           ...
      !>
      !>    SZ22           ...
      !>
      !>    SZ23           ...
      !>
      !>    SZ31           ...
      !>
      !>    SZ32           ...
      !>
      !>    SZ33           ...
      !>
      !>    XGH2        Internal SGD4 parameter.
      !>
      !>    XGH3        Internal SGD4 parameter.
      !>
      !>    XGH4        Internal SGD4 parameter.
      !>
      !>    XH2         Internal SGD4 parameter.
      !>
      !>    XH3         Internal SGD4 parameter.
      !>
      !>    XI2         Internal SGD4 parameter.
      !>
      !>    XI3         Internal SGD4 parameter.
      !>
      !>    XL2         Internal SGD4 parameter.
      !>
      !>    XL3         Internal SGD4 parameter.
      !>
      !>    XL4         Internal SGD4 parameter.
      !>
      !>    NM          Mean motion
      !>
      !>    Z1          Z coefficients
      !>
      !>    Z2             ...
      !>
      !>    Z3             ...
      !>
      !>    Z11            ...
      !>
      !>    Z12            ...
      !>
      !>    Z13            ...
      !>
      !>    Z21            ...
      !>
      !>    Z22            ...
      !>
      !>    Z23            ...
      !>
      !>    Z31            ...
      !>
      !>    Z32            ...
      !>
      !>    Z33            ...
      !>
      !>    ZMOL        Internal SGD4 parameter.
      !>
      !>    ZMOS        Internal SGD4 parameter.
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
      !>     This routine is based on the DSCOM code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3" [4].
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        Capitalize all variables.
      !>
      !>        ENDIF replaced with END IF.
      !>
      !>        ENDDO replaced with END DO.
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
      !>   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>       Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>       U.S. Air Force: Aerospace Defense Command.
      !>
      !>   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>       of Space Command Element Sets." Space Command,
      !>       U. S. Air Force, CO.
      !>
      !>   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>       History of Analytical Orbit Modeling in the U. S. Space
      !>       Surveillance System. Journal of Guidance, Control, and
      !>       Dynamics. 27(2):174-185.
      !>
      !>   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>       Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     David Vallado   (AGI)
      !>     E. D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 11-NOV-2014 (EDW)
      !>
      !>        Based on routine DSCOM, 28-JUN-2005, Vallado 2006 [4].
      module subroutine zzdscm ( ctx, epoch, eccp, argpp, tc, inclp, nodep, np, snodm, cnodm, sinim, cosim, sinomm, cosomm, day, e3, ee2, eccm, emsq, gam, peo, pgho, pho, pinco, plo, rtemsq, se2, se3, sgh2, sgh3, sgh4, sh2, sh3, si2, si3, sl2, sl3, sl4, s1, s2, s3, s4, s5, s6, s7, ss1, ss2, ss3, ss4, ss5, ss6, ss7, sz1, sz2, sz3, sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33, xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4, xn, z1, z2, z3, z11, z12, z13, z21, z22, z23, z31, z32, z33, zmol, zmos )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: epoch  !! Epoch of TLE set as Julian day value.
      real(wp), intent(inout) :: eccp
      real(wp), intent(in) :: argpp  !! Argument of perigee
      real(wp), intent(in) :: tc  !! Minutes past EPOCH, nominally zero.
      real(wp), intent(in) :: inclp  !! Inclination
      real(wp), intent(in) :: nodep  !! Right ascension of ascending node
      real(wp), intent(in) :: np  !! Mean motion
      real(wp), intent(out) :: snodm  !! Internal SGD4 parameter.
      real(wp), intent(out) :: cnodm  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sinim  !! SIN of mean inclination.
      real(wp), intent(out) :: cosim  !! COS of mean inclination.
      real(wp), intent(out) :: sinomm  !! Internal SGD4 parameter.
      real(wp), intent(out) :: cosomm  !! Internal SGD4 parameter.
      real(wp), intent(out) :: day  !! Internal SGD4 parameter.
      real(wp), intent(out) :: e3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: ee2  !! Internal SGD4 parameter.
      real(wp), intent(inout) :: eccm
      real(wp), intent(out) :: emsq  !! Eccentricity squared.
      real(wp), intent(out) :: gam  !! Internal SGD4 parameter.
      real(wp), intent(out) :: rtemsq  !! Internal SGD4 parameter.
      real(wp), intent(out) :: se2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: peo  !! Internal SGD4 parameter.
      real(wp), intent(out) :: pgho  !! Internal SGD4 parameter.
      real(wp), intent(out) :: pho  !! Internal SGD4 parameter.
      real(wp), intent(out) :: pinco  !! Internal SGD4 parameter.
      real(wp), intent(out) :: plo  !! Internal SGD4 parameter.
      real(wp), intent(out) :: se3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sgh2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sgh3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sgh4  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sh2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sh3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: si2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: si3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sl2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sl3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: sl4  !! Internal SGD4 parameter.
      real(wp), intent(out) :: s1  !! S coefficients
      real(wp), intent(out) :: s2  !! ...
      real(wp), intent(out) :: s3  !! ...
      real(wp), intent(out) :: s4  !! ...
      real(wp), intent(out) :: s5  !! ...
      real(wp), intent(out) :: s6  !! ...
      real(wp), intent(out) :: s7  !! ...
      real(wp), intent(out) :: ss1  !! ...
      real(wp), intent(out) :: ss2  !! ...
      real(wp), intent(out) :: ss3  !! ...
      real(wp), intent(out) :: ss4  !! ...
      real(wp), intent(out) :: ss5  !! ...
      real(wp), intent(out) :: ss6  !! ...
      real(wp), intent(out) :: ss7  !! ...
      real(wp), intent(out) :: sz1  !! ...
      real(wp), intent(out) :: sz2  !! ...
      real(wp), intent(out) :: sz3  !! ...
      real(wp), intent(out) :: sz11  !! ...
      real(wp), intent(out) :: sz12  !! ...
      real(wp), intent(out) :: sz13  !! ...
      real(wp), intent(out) :: sz21  !! ...
      real(wp), intent(out) :: sz22  !! ...
      real(wp), intent(out) :: sz23  !! ...
      real(wp), intent(out) :: sz31  !! ...
      real(wp), intent(out) :: sz32  !! ...
      real(wp), intent(out) :: sz33  !! ...
      real(wp), intent(out) :: xgh2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xgh3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xgh4  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xh2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xh3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xi2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xi3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xl2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xl3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xl4  !! Internal SGD4 parameter.
      real(wp), intent(inout) :: xn
      real(wp), intent(out) :: z1  !! Z coefficients
      real(wp), intent(out) :: z2  !! ...
      real(wp), intent(out) :: z3  !! ...
      real(wp), intent(out) :: z11  !! ...
      real(wp), intent(out) :: z12  !! ...
      real(wp), intent(out) :: z13  !! ...
      real(wp), intent(out) :: z21  !! ...
      real(wp), intent(out) :: z22  !! ...
      real(wp), intent(out) :: z23  !! ...
      real(wp), intent(out) :: z31  !! ...
      real(wp), intent(out) :: z32  !! ...
      real(wp), intent(out) :: z33  !! ...
      real(wp), intent(out) :: zmol  !! Internal SGD4 parameter.
      real(wp), intent(out) :: zmos  !! Internal SGD4 parameter.

      real(wp) :: zes
      real(wp) :: a1
      real(wp) :: a10
      real(wp) :: a2
      real(wp) :: a3
      real(wp) :: a4
      real(wp) :: a5
      real(wp) :: a6
      real(wp) :: a7
      real(wp) :: a8
      real(wp) :: a9
      real(wp) :: betasq
      real(wp) :: c1l
      real(wp) :: c1ss
      real(wp) :: cc
      real(wp) :: ctem
      real(wp) :: stem
      real(wp) :: x1
      real(wp) :: x2
      real(wp) :: x3
      real(wp) :: x4
      real(wp) :: x5
      real(wp) :: x6
      real(wp) :: x7
      real(wp) :: x8
      real(wp) :: xnodce
      real(wp) :: xnoi
      real(wp) :: zcosg
      real(wp) :: zcosgl
      real(wp) :: zcosgs
      real(wp) :: zcosh
      real(wp) :: zcoshl
      real(wp) :: zcosi
      real(wp) :: zcosil
      real(wp) :: zcosis
      real(wp) :: zel
      real(wp) :: zsing
      real(wp) :: zsingl
      real(wp) :: zsings
      real(wp) :: zsinh
      real(wp) :: zsinhl
      real(wp) :: zsini
      real(wp) :: zsinil
      real(wp) :: zsinis
      real(wp) :: zx
      real(wp) :: zy

      integer(ip) :: lsflg

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Constants
!
      zes    =  0.01675_wp
      zel    =  0.05490_wp
      c1ss   =  2.9864797e-6_wp
      c1l    =  4.7968065e-7_wp
      zsinis =  0.39785416_wp
      zcosis =  0.91744867_wp
      zcosgs =  0.1945905_wp
      zsings = -0.98088458_wp

!
!     DEEP SPACE PERIODICS INITIALIZATION
!
      xn     = np
      eccm   = eccp
      snodm  = dsin(nodep)
      cnodm  = dcos(nodep)
      sinomm = dsin(argpp)
      cosomm = dcos(argpp)
      sinim  = dsin(inclp)
      cosim  = dcos(inclp)
      emsq   = eccm*eccm
      betasq = 1.0_wp-emsq
      rtemsq = dsqrt(betasq)

!
!     INITIALIZE LUNAR SOLAR TERMS
!
!     Note, EPOCH + 18261.5D0 corresponds to JD days since
!     1899-12-31 12:00:00.
!
      peo    = 0.0_wp
      pinco  = 0.0_wp
      plo    = 0.0_wp
      pgho   = 0.0_wp
      pho    = 0.0_wp
      day    = epoch + 18261.5_wp + tc/1440.0_wp
      xnodce = dmod(4.5236020_wp - 9.2422029e-4_wp*day,twopi)
      stem   = dsin(xnodce)
      ctem   = dcos(xnodce)
      zcosil = 0.91375164_wp - 0.03568096_wp*ctem
      zsinil = dsqrt(1.0_wp - zcosil*zcosil)
      zsinhl = 0.089683511_wp*stem / zsinil
      zcoshl = dsqrt(1.0_wp - zsinhl*zsinhl)
      gam    = 5.8351514_wp + 0.0019443680_wp*day
      zx     = 0.39785416_wp*stem/zsinil
      zy     = zcoshl*ctem + 0.91744867_wp*zsinhl*stem
      zx     = datan2(zx,zy)
      zx     = gam + zx - xnodce
      zcosgl = dcos(zx)
      zsingl = dsin(zx)

!
!     DO SOLAR TERMS
!
      zcosg = zcosgs
      zsing = zsings
      zcosi = zcosis
      zsini = zsinis
      zcosh = cnodm
      zsinh = snodm
      cc    = c1ss
      xnoi  = 1.0_wp / xn

!
!     Loop over the lunar and solar term flags.
!
      do lsflg = 1,2

         a1 =   zcosg*zcosh + zsing*zcosi*zsinh
         a3 =  -zsing*zcosh + zcosg*zcosi*zsinh
         a7 =  -zcosg*zsinh + zsing*zcosi*zcosh
         a8 =   zsing*zsini
         a9 =   zsing*zsinh + zcosg*zcosi*zcosh
         a10=   zcosg*zsini
         a2 =   cosim*a7 + sinim*a8
         a4 =   cosim*a9 + sinim*a10
         a5 =  -sinim*a7 + cosim*a8
         a6 =  -sinim*a9 + cosim*a10

         x1 =  a1*cosomm + a2*sinomm
         x2 =  a3*cosomm + a4*sinomm
         x3 = -a1*sinomm + a2*cosomm
         x4 = -a3*sinomm + a4*cosomm
         x5 =  a5*sinomm
         x6 =  a6*sinomm
         x7 =  a5*cosomm
         x8 =  a6*cosomm

         z31= 12.0_wp*x1*x1 - 3.0_wp*x3*x3
         z32= 24.0_wp*x1*x2 - 6.0_wp*x3*x4
         z33= 12.0_wp*x2*x2 - 3.0_wp*x4*x4
         z1 =  3.0_wp* (a1*a1 + a2*a2) + z31*emsq
         z2 =  6.0_wp* (a1*a3 + a2*a4) + z32*emsq
         z3 =  3.0_wp* (a3*a3 + a4*a4) + z33*emsq
         z11= -6.0_wp*a1*a5 + emsq* (-24.0_wp*x1*x7-6.0_wp*x3*x5)
         z12= -6.0_wp* (a1*a6 + a3*a5) + emsq* &
                 ( -24.0_wp*(x2*x7+x1*x8) - 6.0_wp*(x3*x6+x4*x5) )
         z13= -6.0_wp*a3*a6 + emsq*(-24.0_wp*x2*x8 - 6.0_wp*x4*x6)
         z21=  6.0_wp*a2*a5 + emsq*(24.0_wp*x1*x5-6.0_wp*x3*x7)
         z22=  6.0_wp* (a4*a5 + a2*a6) + emsq* &
                 (  24.0_wp*(x2*x5+x1*x6) - 6.0_wp*(x4*x7+x3*x8) )
         z23=  6.0_wp*a4*a6 + emsq*(24.0_wp*x2*x6 - 6.0_wp*x4*x8)
         z1 = z1 + z1 + betasq*z31
         z2 = z2 + z2 + betasq*z32
         z3 = z3 + z3 + betasq*z33
         s3 = cc*xnoi
         s2 = -0.5_wp*s3 / rtemsq
         s4 = s3*rtemsq
         s1 = -15.0_wp*eccm*s4
         s5 = x1*x3 + x2*x4
         s6 = x2*x3 + x1*x4
         s7 = x2*x4 - x1*x3

!
!        DO LUNAR TERMS
!
         if (lsflg==1) then
            ss1   = s1
            ss2   = s2
            ss3   = s3
            ss4   = s4
            ss5   = s5
            ss6   = s6
            ss7   = s7
            sz1   = z1
            sz2   = z2
            sz3   = z3
            sz11  = z11
            sz12  = z12
            sz13  = z13
            sz21  = z21
            sz22  = z22
            sz23  = z23
            sz31  = z31
            sz32  = z32
            sz33  = z33
            zcosg = zcosgl
            zsing = zsingl
            zcosi = zcosil
            zsini = zsinil
            zcosh = zcoshl*cnodm+zsinhl*snodm
            zsinh = snodm*zcoshl-cnodm*zsinhl
            cc    = c1l
         end if

      end do

      zmol  = dmod( 4.7199672_wp + 0.22997150_wp*day-gam, twopi )
      zmos  = dmod( 6.2565837_wp + 0.017201977_wp*day,    twopi )

!
!     DO SOLAR TERMS
!
      se2 =   2.0_wp*ss1*ss6
      se3 =   2.0_wp*ss1*ss7
      si2 =   2.0_wp*ss2*sz12
      si3 =   2.0_wp*ss2*(sz13-sz11)
      sl2 =  -2.0_wp*ss3*sz2
      sl3 =  -2.0_wp*ss3*(sz3-sz1)
      sl4 =  -2.0_wp*ss3*(-21.0_wp-9.0_wp*emsq)*zes
      sgh2=   2.0_wp*ss4*sz32
      sgh3=   2.0_wp*ss4*(sz33-sz31)
      sgh4= -18.0_wp*ss4*zes
      sh2 =  -2.0_wp*ss2*sz22
      sh3 =  -2.0_wp*ss2*(sz23-sz21)

!
!     DO LUNAR TERMS
!
      ee2 =   2.0_wp*s1*s6
      e3  =   2.0_wp*s1*s7
      xi2 =   2.0_wp*s2*z12
      xi3 =   2.0_wp*s2*(z13-z11)
      xl2 =  -2.0_wp*s3*z2
      xl3 =  -2.0_wp*s3*(z3-z1)
      xl4 =  -2.0_wp*s3*(-21.0_wp-9.0_wp*emsq)*zel
      xgh2=   2.0_wp*s4*z32
      xgh3=   2.0_wp*s4*(z33-z31)
      xgh4= -18.0_wp*s4*zel
      xh2 =  -2.0_wp*s2*z22
      xh3 =  -2.0_wp*s2*(z23-z21)

      return
      end subroutine zzdscm

      !> This Subroutine provides Deep Space contributions to Mean
      !> Motion Dot due to geopotential resonance with half day and one
      !> day orbits.
      !>
      !> See Detailed_input and Detailed_Output.
      !>
      !>## Detailed Input
      !>
      !>    COSIM       COS of mean inclination.
      !>
      !>    SINIM       SIN of mean inclination.
      !>
      !>    EMSQ        Eccentricity squared
      !>
      !>    ARGPO       Argument of Perigee
      !>
      !>    S1          S coefficients
      !>
      !>    S2              ...
      !>
      !>    S3              ...
      !>
      !>    S4              ...
      !>
      !>    S5              ...
      !>
      !>    SS1             ...
      !>
      !>    SS2             ...
      !>
      !>    SS3             ...
      !>
      !>    SS4             ...
      !>
      !>    SS5             ...
      !>
      !>    SZ1             ...
      !>
      !>    SZ3             ...
      !>
      !>    SZ11            ...
      !>
      !>    SZ13            ...
      !>
      !>    SZ21            ...
      !>
      !>    SZ23            ...
      !>
      !>    SZ31            ...
      !>
      !>    SZ33            ...
      !>
      !>    T           Time
      !>
      !>    TC
      !>
      !>    GSTO        Greenwich sidereal time in radians
      !>
      !>    MO          Mean anomaly
      !>
      !>    MDOT        Mean anomaly rate
      !>
      !>    NO          Mean motion
      !>
      !>    NODEO       Right ascension of ascending node
      !>
      !>    NODEDOT     Right ascension of ascending node rate
      !>
      !>    XPIDOT
      !>
      !>    Z1          Z coefficients
      !>
      !>    Z3               ...
      !>
      !>    Z11              ...
      !>
      !>    Z13              ...
      !>
      !>    Z21              ...
      !>
      !>    Z23              ...
      !>
      !>    Z31              ...
      !>
      !>    Z33              ...
      !>
      !>    ECCM        Mean eccentricity
      !>
      !>    ARGPM       Mean argument of perigee
      !>
      !>    INCLM       Mean inclination
      !>
      !>    MM          Mean anomaly
      !>
      !>    XN          Mean motion
      !>
      !>    NODEM       Mean right ascension of ascending node
      !>
      !>## Detailed Output
      !>
      !>    ECCM        Mean eccentricity
      !>
      !>    ARGPM       Mean argument of perigee
      !>
      !>    INCLM       Mean inclination
      !>
      !>    MM          Mean anomaly
      !>
      !>    XN          Mean motion
      !>
      !>    NODEM       Right ascension of ascending node
      !>
      !>    IREZ        Resonance flags: 0-none, 1-one day, 2-half day
      !>
      !>    ATIME       Internal SGD4 parameter.
      !>
      !>    D2201       D COEFFCIENTS
      !>
      !>    D2211           ...
      !>
      !>    D3210           ...
      !>
      !>    D3222           ...
      !>
      !>    D4410           ...
      !>
      !>    D4422           ...
      !>
      !>    D5220           ...
      !>
      !>    D5232           ...
      !>
      !>    D5421           ...
      !>
      !>    D5433           ...
      !>
      !>    DEDT        Internal SGD4 parameter.
      !>
      !>    DIDT        Internal SGD4 parameter.
      !>
      !>    DMDT        Internal SGD4 parameter.
      !>
      !>    DNDT        Internal SGD4 parameter.
      !>
      !>    DNODT       Internal SGD4 parameter.
      !>
      !>    DOMDT       Internal SGD4 parameter.
      !>
      !>    DEL1        Internal SGD4 parameter.
      !>
      !>    DEL2        Internal SGD4 parameter.
      !>
      !>    DEL3        Internal SGD4 parameter.
      !>
      !>    SES         Internal SGD4 parameter.
      !>
      !>    SGHL        Internal SGD4 parameter.
      !>
      !>    SGHS        Internal SGD4 parameter.
      !>
      !>    SGS         Internal SGD4 parameter.
      !>
      !>    SHL         Internal SGD4 parameter.
      !>
      !>    SHS         Internal SGD4 parameter.
      !>
      !>    SIS         Internal SGD4 parameter.
      !>
      !>    SLS         Internal SGD4 parameter.
      !>
      !>    THETA       Internal SGD4 parameter.
      !>
      !>    XFACT       Internal SGD4 parameter.
      !>
      !>    XLAMO       Internal SGD4 parameter.
      !>
      !>    XLI         Internal SGD4 parameter.
      !>
      !>    XNI         Internal SGD4 parameter.
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
      !>     This routine is based on the DSINIT code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3" [4].
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        Removed getgravconst call, replaced with GEOPHS array.
      !>
      !>        Capitalize all variables.
      !>
      !>        ENDIF replaced with END IF.
      !>
      !>        whichconst    eliminated, function provided by GEOPHS
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
      !>   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>       Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>       U.S. Air Force: Aerospace Defense Command.
      !>
      !>   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>       of Space Command Element Sets." Space Command,
      !>       U. S. Air Force, CO.
      !>
      !>   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>       History of Analytical Orbit Modeling in the U. S. Space
      !>       Surveillance System. Journal of Guidance, Control, and
      !>       Dynamics. 27(2):174-185.
      !>
      !>   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>       Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     David Vallado   (AGI)
      !>     E. D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 03-NOV-2014 (EDW)
      !>
      !>        Based on routine DSINIT, 28-JUN-2005, Vallado 2006 [4].
      module subroutine zzdsin ( ctx, geophs, cosim, emsq, argpo, s1, s2, s3, s4, s5, sinim, ss1, ss2, ss3, ss4, ss5, sz1, sz3, sz11, sz13, sz21, sz23, sz31, sz33, t, tc, gsto, mo, mdot, no, nodeo, nodedot, xpidot, z1, z3, z11, z13, z21, z23, z31, z33, ecco, eccsq, eccm, argpm, inclm, mm, xn, nodem, irez, atime, d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433, dedt, didt, dmdt, dndt, dnodt, domdt, del1, del2, del3, xfact, xlamo, xli, xni )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi, twopi
      use pepper_inc_zzsgp4
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(inout) :: argpm  !! Mean argument of perigee
      real(wp), intent(in) :: argpo  !! Argument of Perigee
      real(wp), intent(out) :: atime  !! Internal SGD4 parameter.
      real(wp), intent(in) :: cosim  !! COS of mean inclination.
      real(wp), intent(out) :: d2201  !! D COEFFCIENTS
      real(wp), intent(out) :: d2211  !! ...
      real(wp), intent(out) :: d3210  !! ...
      real(wp), intent(out) :: d3222  !! ...
      real(wp), intent(out) :: d4410  !! ...
      real(wp), intent(out) :: d4422  !! ...
      real(wp), intent(out) :: d5220  !! ...
      real(wp), intent(out) :: d5232  !! ...
      real(wp), intent(out) :: d5421  !! ...
      real(wp), intent(out) :: d5433  !! ...
      real(wp), intent(out) :: dedt  !! Internal SGD4 parameter.
      real(wp), intent(out) :: del1  !! Internal SGD4 parameter.
      real(wp), intent(out) :: del2  !! Internal SGD4 parameter.
      real(wp), intent(out) :: del3  !! Internal SGD4 parameter.
      real(wp), intent(out) :: didt  !! Internal SGD4 parameter.
      real(wp), intent(out) :: dmdt  !! Internal SGD4 parameter.
      real(wp), intent(out) :: dndt  !! Internal SGD4 parameter.
      real(wp), intent(out) :: dnodt  !! Internal SGD4 parameter.
      real(wp), intent(out) :: domdt  !! Internal SGD4 parameter.
      real(wp), intent(inout) :: eccm  !! Mean eccentricity
      real(wp), intent(inout) :: ecco
      real(wp), intent(inout) :: eccsq
      real(wp), intent(inout) :: emsq  !! Eccentricity squared
      real(wp), intent(in) :: geophs    ( 8 )
      real(wp), intent(in) :: gsto  !! Greenwich sidereal time in radians
      real(wp), intent(inout) :: inclm  !! Mean inclination
      real(wp), intent(in) :: mdot  !! Mean anomaly rate
      real(wp), intent(inout) :: mm  !! Mean anomaly
      real(wp), intent(in) :: mo  !! Mean anomaly
      real(wp), intent(in) :: no  !! Mean motion
      real(wp), intent(in) :: nodedot  !! Right ascension of ascending node rate
      real(wp), intent(inout) :: nodem  !! Mean right ascension of ascending node
      real(wp), intent(in) :: nodeo  !! Right ascension of ascending node
      real(wp), intent(in) :: s1  !! S coefficients
      real(wp), intent(in) :: s2  !! ...
      real(wp), intent(in) :: s3  !! ...
      real(wp), intent(in) :: s4  !! ...
      real(wp), intent(in) :: s5  !! ...
      real(wp), intent(in) :: sinim  !! SIN of mean inclination.
      real(wp), intent(in) :: ss1  !! ...
      real(wp), intent(in) :: ss2  !! ...
      real(wp), intent(in) :: ss3  !! ...
      real(wp), intent(in) :: ss4  !! ...
      real(wp), intent(in) :: ss5  !! ...
      real(wp), intent(in) :: sz1  !! ...
      real(wp), intent(in) :: sz11  !! ...
      real(wp), intent(in) :: sz13  !! ...
      real(wp), intent(in) :: sz21  !! ...
      real(wp), intent(in) :: sz23  !! ...
      real(wp), intent(in) :: sz3  !! ...
      real(wp), intent(in) :: sz31  !! ...
      real(wp), intent(in) :: sz33  !! ...
      real(wp), intent(in) :: t  !! Time
      real(wp), intent(in) :: tc
      real(wp), intent(out) :: xfact  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xlamo  !! Internal SGD4 parameter.
      real(wp), intent(out) :: xli  !! Internal SGD4 parameter.
      real(wp), intent(inout) :: xn  !! Mean motion
      real(wp), intent(out) :: xni  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xpidot
      real(wp), intent(in) :: z1  !! Z coefficients
      real(wp), intent(in) :: z11  !! ...
      real(wp), intent(in) :: z13  !! ...
      real(wp), intent(in) :: z21  !! ...
      real(wp), intent(in) :: z23  !! ...
      real(wp), intent(in) :: z3  !! ...
      real(wp), intent(in) :: z31  !! ...
      real(wp), intent(in) :: z33  !! ...
      integer(ip), intent(out) :: irez  !! Resonance flags: 0-none, 1-one day, 2-half day

      real(wp) :: ainv2
      real(wp) :: aonv
      real(wp) :: cosisq
      real(wp) :: emo
      real(wp) :: emsqo
      real(wp) :: eoc
      real(wp) :: f220
      real(wp) :: f221
      real(wp) :: f311
      real(wp) :: f321
      real(wp) :: f322
      real(wp) :: f330
      real(wp) :: f441
      real(wp) :: f442
      real(wp) :: f522
      real(wp) :: f523
      real(wp) :: f542
      real(wp) :: f543
      real(wp) :: g200
      real(wp) :: g201
      real(wp) :: g211
      real(wp) :: g300
      real(wp) :: g310
      real(wp) :: g322
      real(wp) :: g410
      real(wp) :: g422
      real(wp) :: g520
      real(wp) :: g521
      real(wp) :: g532
      real(wp) :: g533
      real(wp) :: q22
      real(wp) :: q31
      real(wp) :: q33
      real(wp) :: root22
      real(wp) :: root32
      real(wp) :: root44
      real(wp) :: root52
      real(wp) :: root54
      real(wp) :: rptim
      real(wp) :: ses
      real(wp) :: sghl
      real(wp) :: sghs
      real(wp) :: sgs
      real(wp) :: shl
      real(wp) :: shs
      real(wp) :: sini2
      real(wp) :: sis
      real(wp) :: sls
      real(wp) :: temp
      real(wp) :: temp1
      real(wp) :: theta
      real(wp) :: x2o3
      real(wp) :: xke
      real(wp) :: xno2
      real(wp) :: znl
      real(wp) :: zns

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Constants
!
      q22    = 1.7891679e-6_wp
      q31    = 2.1460748e-6_wp
      q33    = 2.2123015e-7_wp
      root22 = 1.7891679e-6_wp
      root44 = 7.3636953e-9_wp
      root54 = 2.1765803e-9_wp

!
!     This equates to 7.29211514668855e-5 rad/sec
!
      rptim  = 4.37526908801129966e-3_wp

      root32 = 3.7393792e-7_wp
      root52 = 1.1428639e-7_wp
      x2o3   = 2.0_wp / 3.0_wp
      znl    = 1.5835218e-4_wp
      zns    = 1.19459e-5_wp

!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      xke   = geophs(k_ke)

!
!     DEEP SPACE INITIALIZATION
!
      irez = 0
      if ( (xn<0.0052359877_wp)  .and. &
           (xn>0.0034906585_wp) ) then
         irez = 1
      end if

      if ( (xn   >= 8.26e-3_wp) .and. &
           (xn   <= 9.24e-3_wp) .and. &
           (eccm >= 0.5_wp)  )then
         irez = 2
      end if

!
!     DO SOLAR TERMS
!
      ses  =  ss1*zns*ss5
      sis  =  ss2*zns*(sz11 + sz13)
      sls  = -zns*ss3*(sz1 + sz3 - 14.0_wp - 6.0_wp*emsq)
      sghs =  ss4*zns*(sz31 + sz33 - 6.0_wp)
      shs  = -zns*ss2*(sz21 + sz23)

!
!       sgp4fix for 180 deg incl
!
        if ( (inclm < 5.2359877e-2_wp) .or. &
             (inclm > pi-5.2359877e-2_wp) ) then
            shs = 0.0_wp
        end if

        if (sinim/=0.0_wp) then
            shs = shs/sinim
        end if

        sgs  = sghs - cosim*shs

!
!       DO LUNAR TERMS
!
        dedt = ses + s1*znl*s5
        didt = sis + s2*znl*(z11 + z13)
        dmdt = sls - znl*s3*(z1 + z3 - 14.0_wp - 6.0_wp*emsq)
        sghl = s4*znl*(z31 + z33 - 6.0_wp)
        shl  = -znl*s2*(z21 + z23)

!
!       sgp4fix for 180 deg incl
!

        if ( (inclm < 5.2359877e-2_wp ) .or. &
             (inclm > pi-5.2359877e-2_wp)) then
            shl = 0.0_wp
        end if

        domdt= sgs+sghl
        dnodt= shs

        if (sinim /= 0.0_wp) then
            domdt = domdt-cosim/sinim*shl
            dnodt = dnodt+shl/sinim
        end if

!
!       CALCULATE DEEP SPACE RESONANCE EFFECTS
!
        dndt  = 0.0_wp
        theta = dmod(gsto + tc*rptim, twopi )
        eccm  = eccm + dedt*t
        emsq  = eccm**2
        inclm = inclm + didt*t
        argpm = argpm + domdt*t
        nodem = nodem + dnodt*t
        mm    = mm + dmdt*t

!
!   sgp4fix for negative inclinations
!   the following if statement should be commented out
!
!           IF(Inclm .lt. 0.0D0) THEN
!             Inclm  = -Inclm
!             Argpm  = Argpm-PI
!             nodem = nodem+PI
!           END IF
!

!
!       Initialize the resonance terms
!
         if (irez /= 0) then

            aonv = (xn/xke)**x2o3

!
!           GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS
!
            if (irez == 2) then

               cosisq = cosim*cosim
               emo    = eccm
               emsqo  = emsq
               eccm   = ecco
               emsq   = eccsq
               eoc    = eccm*emsq
               g201   = -0.306_wp-(eccm-0.64_wp)*0.440_wp

               if (eccm<=0.65_wp) then

                  g211 =   3.616_wp -  13.2470_wp*eccm + &
                              16.2900_wp*emsq
                  g310 = -19.302_wp + 117.3900_wp*eccm - &
                              228.4190_wp*emsq + 156.591_wp*eoc
                  g322 = -18.9068_wp+ 109.7927_wp*eccm - &
                              214.6334_wp*emsq + 146.5816_wp*eoc
                  g410 = -41.122_wp + 242.6940_wp*eccm - &
                              471.0940_wp*emsq + 313.953_wp*eoc
                  g422 =-146.407_wp + 841.8800_wp*eccm - &
                              1629.014_wp*emsq + 1083.435_wp*eoc
                  g520 =-532.114_wp + 3017.977_wp*eccm - &
                              5740.032_wp*emsq + 3708.276_wp*eoc

               else

                  g211 =  -72.099_wp +  331.819_wp*eccm - &
                              508.738_wp*emsq + 266.724_wp*eoc
                  g310 = -346.844_wp + 1582.851_wp*eccm - &
                              2415.925_wp*emsq + 1246.113_wp*eoc
                  g322 = -342.585_wp + 1554.908_wp*eccm - &
                              2366.899_wp*emsq + 1215.972_wp*eoc
                  g410 =-1052.797_wp + 4758.686_wp*eccm - &
                              7193.992_wp*emsq + 3651.957_wp*eoc
                  g422 =-3581.690_wp + 16178.11_wp*eccm - &
                              24462.77_wp*emsq + 12422.52_wp*eoc

                  if (eccm>0.715_wp) then
                     g520 =-5149.66_wp + 29936.92_wp*eccm - &
                              54087.36_wp*emsq + 31324.56_wp*eoc
                  else
                     g520 = 1464.74_wp -  4664.75_wp*eccm + &
                              3763.64_wp*emsq
                  end if

               end if

            if (eccm<0.7_wp) then

                g533 = -919.22770_wp + 4988.6100_wp*eccm-9064.7700_wp*emsq &
                     + 5542.21_wp*eoc
                g521 = -822.71072_wp + 4568.6173_wp*eccm-8491.4146_wp*emsq &
                     + 5337.524_wp*eoc
                g532 = -853.66600_wp + 4690.2500_wp*eccm-8624.7700_wp*emsq &
                     + 5341.4_wp*eoc

            else

                g533 =-37995.780_wp + 161616.52_wp*eccm-229838.20_wp*emsq+ &
                    109377.94_wp*eoc
                g521 =-51752.104_wp + 218913.95_wp*eccm-309468.16_wp*emsq+ &
                    146349.42_wp*eoc
                g532 =-40023.880_wp + 170470.89_wp*eccm-242699.48_wp*emsq+ &
                    115605.82_wp*eoc

            end if

            sini2 =  sinim*sinim
            f220  =  0.75_wp* (1.0_wp+2.0_wp*cosim+cosisq)
            f221  =  1.5_wp*sini2
            f321  =  1.875_wp*sinim * (1.0_wp-2.0_wp*cosim-3.0_wp*cosisq)
            f322  = -1.875_wp*sinim * (1.0_wp+2.0_wp*cosim-3.0_wp*cosisq)
            f441  = 35.0_wp*sini2*f220
            f442  = 39.3750_wp*sini2*sini2
            f522  =  9.84375_wp*sinim * (sini2* (1.0_wp-2.0_wp*cosim- &
                     5.0_wp*cosisq)+0.33333333_wp * (-2.0_wp+4.0_wp*cosim+ &
                     6.0_wp*cosisq) )
            f523  =  sinim * (4.92187512_wp*sini2 * (-2.0_wp-4.0_wp*cosim+ &
                     10.0_wp*cosisq) + 6.56250012_wp* &
                     (1.0_wp+2.0_wp*cosim-3.0_wp*cosisq))
            f542  =  29.53125_wp*sinim * (2.0_wp-8.0_wp*cosim+cosisq* &
                     (-12.0_wp+8.0_wp*cosim+10.0_wp*cosisq) )
            f543  = 29.53125_wp*sinim * (-2.0_wp-8.0_wp*cosim+cosisq* &
                     (12.0_wp+8.0_wp*cosim-10.0_wp*cosisq) )

            xno2   =  xn * xn
            ainv2  =  aonv * aonv
            temp1  =  3.0_wp*xno2*ainv2
            temp   =  temp1*root22
            d2201  =  temp*f220*g201
            d2211  =  temp*f221*g211
            temp1  =  temp1*aonv
            temp   =  temp1*root32
            d3210  =  temp*f321*g310
            d3222  =  temp*f322*g322
            temp1  =  temp1*aonv
            temp   =  2.0_wp*temp1*root44
            d4410  =  temp*f441*g410
            d4422  =  temp*f442*g422
            temp1  =  temp1*aonv
            temp   =  temp1*root52
            d5220  =  temp*f522*g520
            d5232  =  temp*f523*g532
            temp   =  2.0_wp*temp1*root54
            d5421  =  temp*f542*g521
            d5433  =  temp*f543*g533
            xlamo  =  dmod(mo+nodeo+nodeo-theta-theta, twopi)
            xfact  = mdot + dmdt + 2.0_wp * (nodedot+dnodt-rptim) - no

            eccm = emo
            emsq = emsqo

         end if

         if (irez == 1) then

!
!           SYNCHRONOUS RESONANCE TERMS
!
            g200  = 1.0_wp + emsq * (-2.5_wp+0.8125_wp*emsq)
            g310  = 1.0_wp + 2.0_wp*emsq
            g300  = 1.0_wp + emsq * (-6.0_wp+6.60937_wp*emsq)
            f220  = 0.75_wp * (1.0_wp+cosim) * (1.0_wp+cosim)
            f311  = 0.9375_wp*sinim*sinim* &
                     (1.0_wp+3.0_wp*cosim) - 0.75_wp*(1.0_wp+cosim)
            f330  = 1.0_wp+cosim
            f330  = 1.875_wp*f330*f330*f330
            del1  = 3.0_wp*xn*xn*aonv*aonv
            del2  = 2.0_wp*del1*f220*g200*q22
            del3  = 3.0_wp*del1*f330*g300*q33*aonv
            del1  = del1*f311*g310*q31*aonv
            xlamo = dmod(mo+nodeo+argpo-theta, twopi)
            xfact = mdot + xpidot - rptim + dmdt + domdt + dnodt - no

         end if

!
!        FOR SGP4, INITIALIZE THE INTEGRATOR
!
         xli   = xlamo
         xni   = no
         atime = 0.0_wp
         xn    = no + dndt

      end if

      return
      end subroutine zzdsin

      !> This subroutine provides deep space contributions to mean
      !> elements for perturbing third body. These effects have been
      !> averaged over one revolution of the sun and moon. For earth
      !> resonance effects, the effects have been averaged over NO
      !> revolutions of the satellite.
      !>
      !> See Detailed_input and Detailed_Output.
      !>
      !>## Detailed Input
      !>
      !>     D2201      D coefficients
      !>
      !>     D2211         ...
      !>
      !>     D3210         ...
      !>
      !>     D3222         ...
      !>
      !>     D4410         ...
      !>
      !>     D4422         ...
      !>
      !>     D5220         ...
      !>
      !>     D5232         ...
      !>
      !>     D5421         ...
      !>
      !>     D5433         ...
      !>
      !>     DEDT       Internal SGD4 parameter.
      !>
      !>     DEL1       Internal SGD4 parameter.
      !>
      !>     DEL2       Internal SGD4 parameter.
      !>
      !>     DEL3       Internal SGD4 parameter.
      !>
      !>     DIDT       Internal SGD4 parameter.
      !>
      !>     DMDT       Internal SGD4 parameter.
      !>
      !>     DNODT      Internal SGD4 parameter.
      !>
      !>     DOMDT      Internal SGD4 parameter.
      !>
      !>     IREZ       Flag for resonance: 0-none, 1-one day, 2-half day
      !>.
      !>     ARGPO      Argument of perigee
      !>
      !>     ARGPDOT    Argument of perigee dot (rate)
      !>
      !>     T          Time of evaluation.
      !>
      !>     TC         Internal SGD4 parameter.
      !>
      !>     GSTO       Greenwich Sidereal Time
      !>
      !>     XFACT      Internal SGD4 parameter.
      !>
      !>     XLAMO      Internal SGD4 parameter.
      !>
      !>     NO         Mean motion.
      !>
      !>     ATIME      Internal SGD4 parameter.
      !>
      !>     EM         Eccentricity.
      !>
      !>     FT         Internal SGD4 parameter.
      !>
      !>     ARGPM      Argument of perigee
      !>
      !>     INCLM      Inclination.
      !>
      !>     XLI        Internal SGD4 parameter.
      !>
      !>     MM         Mean anomaly.
      !>
      !>     XNI        Mean motion.
      !>
      !>     NODEM      Right ascension of ascending node
      !>
      !>## Detailed Output
      !>
      !>     ATIME      Internal SGD4 parameter.
      !>
      !>     EM         Calculated mean eccentricity.
      !>
      !>     ARGPM      Calculated mean argument of perigee.
      !>
      !>     INCLM      Calculated mean inclination.
      !>
      !>     XLI        Internal SGD4 parameter.
      !>
      !>     MM         Calculated mean anomaly.
      !>
      !>     XNI        Calculated  mean motion.
      !>
      !>     NODEM      Calculated mean right ascension of
      !>                ascending node.
      !>
      !>     DNDT       Value XN-NO.
      !>
      !>     NM         Calculated mean motion.
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
      !>     This routine is based on the DSPACE code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3" [4].
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        Capitalize all variables.
      !>
      !>        ENDIF replaced with END IF.
      !>
      !>        ENDDO replaced with END DO.
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
      !>   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>       Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>       U.S. Air Force: Aerospace Defense Command.
      !>
      !>   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>       of Space Command Element Sets." Space Command,
      !>       U. S. Air Force, CO.
      !>
      !>   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>       History of Analytical Orbit Modeling in the U. S. Space
      !>       Surveillance System. Journal of Guidance, Control, and
      !>       Dynamics. 27(2):174-185.
      !>
      !>   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>       Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     David Vallado   (AGI)
      !>     E. D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    SPICELIB Version 1.0.0, OCT-14-2014 (EDW)
      !>
      !>        Based on routine DPSPACE, 28-JUN-2005, Vallado 2006 [4].
      module subroutine zzdspc ( ctx, irez, d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433, dedt, del1, del2, del3, didt, dmdt, dnodt, domdt, argpo, argpdot, t, tc, gsto, xfact, xlamo, no, atime, eccm, argpm, inclm, xli, mm, xni, nodem, dndt, xn )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: irez  !! Flag for resonance: 0-none, 1-one day, 2-half day .
      real(wp), intent(in) :: d2201  !! D coefficients
      real(wp), intent(in) :: d2211  !! ...
      real(wp), intent(in) :: d3210  !! ...
      real(wp), intent(in) :: d3222  !! ...
      real(wp), intent(in) :: d4410  !! ...
      real(wp), intent(in) :: d4422  !! ...
      real(wp), intent(in) :: d5220  !! ...
      real(wp), intent(in) :: d5232  !! ...
      real(wp), intent(in) :: d5421  !! ...
      real(wp), intent(in) :: d5433  !! ...
      real(wp), intent(in) :: dedt  !! Internal SGD4 parameter.
      real(wp), intent(in) :: del1  !! Internal SGD4 parameter.
      real(wp), intent(in) :: del2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: del3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: didt  !! Internal SGD4 parameter.
      real(wp), intent(in) :: dmdt  !! Internal SGD4 parameter.
      real(wp), intent(in) :: dnodt  !! Internal SGD4 parameter.
      real(wp), intent(in) :: domdt  !! Internal SGD4 parameter.
      real(wp), intent(in) :: argpo  !! Argument of perigee
      real(wp), intent(in) :: argpdot  !! Argument of perigee dot (rate)
      real(wp), intent(in) :: t  !! Time of evaluation.
      real(wp), intent(in) :: tc  !! Internal SGD4 parameter.
      real(wp), intent(in) :: gsto  !! Greenwich Sidereal Time
      real(wp), intent(in) :: xfact  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xlamo  !! Internal SGD4 parameter.
      real(wp), intent(in) :: no  !! Mean motion.
      real(wp), intent(inout) :: atime  !! Internal SGD4 parameter.
      real(wp), intent(inout) :: eccm
      real(wp), intent(inout) :: argpm  !! Argument of perigee
      real(wp), intent(inout) :: inclm  !! Inclination.
      real(wp), intent(inout) :: xli  !! Internal SGD4 parameter.
      real(wp), intent(inout) :: mm  !! Mean anomaly.
      real(wp), intent(inout) :: xni  !! Mean motion.
      real(wp), intent(inout) :: nodem  !! Right ascension of ascending node ascending node.
      real(wp), intent(out) :: dndt  !! Value XN-NO.
      real(wp), intent(inout) :: xn

      real(wp) :: delt
      real(wp) :: fasx2
      real(wp) :: fasx4
      real(wp) :: fasx6
      real(wp) :: ft
      real(wp) :: g22
      real(wp) :: g32
      real(wp) :: g44
      real(wp) :: g52
      real(wp) :: g54
      real(wp) :: rptim
      real(wp) :: step2
      real(wp) :: stepn
      real(wp) :: stepp
      real(wp) :: theta
      real(wp) :: x2li
      real(wp) :: x2omi
      real(wp) :: xl
      real(wp) :: xldot
      real(wp) :: xnddt
      real(wp) :: xndt
      real(wp) :: xomi

      integer(ip) :: iret
      integer(ip) :: iretn

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Constants
!
      fasx2 = 0.13130908_wp
      fasx4 = 2.8843198_wp
      fasx6 = 0.37448087_wp
      g22   = 5.7686396_wp
      g32   = 0.95240898_wp
      g44   = 1.8014998_wp
      g52   = 1.0508330_wp
      g54   = 4.4108898_wp
      rptim = 4.37526908801129966e-3_wp
      stepp =    720.0_wp
      stepn =   -720.0_wp
      step2 = 259200.0_wp

!
!     Calculate deep space resonance effects.
!
      dndt  = 0.0_wp
      theta = dmod(gsto + tc*rptim,twopi)
      eccm  = eccm + dedt*t

      inclm = inclm + didt*t
      argpm = argpm + domdt*t
      nodem = nodem + dnodt*t
      mm    = mm + dmdt*t

!
!   sgp4fix for negative inclinations
!   the following if statement should be commented out
!
!        IF( INCLM .LT. 0.0D0) THEN
!            INCLM  = -INCLM
!            ARGPM  = ARGPM-PI
!            NODEM  = NODEM+PI
!        END IF
!

!
!     sgp4fix for propagator problems
!
!     The following integration works for negative time steps and
!     periods. The specific changes are unknown because the original
!     code was so convoluted
!
!     sgp4fix Take out atime = 0.0 and fix for faster operation
!

!
!     Just in case - should be set in loops if used.
!
      ft    = 0.0_wp

      if (irez /= 0) then

!
!     UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION
!
!     EPOCH RESTART
!

!
!        sgp4fix streamline check
!
         if ( (    atime == 0.0_wp) .or. &
              (t * atime <= 0.0_wp) .or. &
              (  dabs(t) < dabs(atime))  ) then
            atime  = 0.0_wp
            xni    = no
            xli    = xlamo
         end if

!
!        sgp4fix move check outside loop
!
         if (t > 0.0_wp) then
            delt = stepp
         else
            delt = stepn
         end if

!
!        ADDED FOR DO LOOP
!
         iretn = 381

!
!        ADDED FOR LOOP
!
         iret  =   0

         do while (iretn==381)

!
!           DOT TERMS CALCULATED
!
!           NEAR - SYNCHRONOUS RESONANCE TERMS
!
            if (irez /= 2) then

               xndt  = del1*dsin(xli-fasx2) + &
                        del2*dsin(2.0_wp*(xli-fasx4)) + &
                        del3*dsin(3.0_wp*(xli-fasx6))
               xldot = xni + xfact
               xnddt = del1*dcos(xli-fasx2) + &
                  2.0_wp*del2*dcos(2.0_wp*(xli-fasx4)) + &
                  3.0_wp*del3*dcos(3.0_wp*(xli-fasx6))
               xnddt = xnddt*xldot

            else

!
!              NEAR - HALF-DAY RESONANCE TERMS
!

               xomi = argpo + argpdot*atime
               x2omi= xomi + xomi
               x2li = xli + xli
               xndt = d2201*dsin(x2omi+xli-g22) + &
                       d2211*dsin(xli-g22) + &
                       d3210*dsin( xomi+xli-g32) + &
                       d3222*dsin(-xomi+xli-g32) + &
                       d4410*dsin(x2omi+x2li-g44)+ &
                       d4422*dsin(x2li-g44)+ &
                       d5220*dsin( xomi+xli-g52) + &
                       d5232*dsin(-xomi+xli-g52) + &
                       d5421*dsin( xomi+x2li-g54)+ &
                       d5433*dsin(-xomi+x2li-g54)
               xldot = xni+xfact
               xnddt = d2201*dcos(x2omi+xli-g22) + &
                        d2211*dcos(xli-g22)+ &
                        d3210*dcos( xomi+xli-g32) + &
                        d3222*dcos(-xomi+xli-g32) + &
                        d5220*dcos( xomi+xli-g52) + &
                        d5232*dcos(-xomi+xli-g52) + &
                        2.0_wp*(d4410*dcos(x2omi+x2li-g44) + &
                        d4422*dcos(x2li-g44) + &
                        d5421*dcos( xomi+x2li-g54) + &
                        d5433*dcos(-xomi+x2li-g54))
               xnddt = xnddt*xldot

            end if

!
!           INTEGRATOR
!
!           sgp4fix move end checks to end of routine
!

            if (dabs(t-atime)>=stepp) then
               iret  = 0
               iretn = 381
            else
               ft    = t-atime
               iretn = 0
            end if

            if (iretn==381) then
               xli   = xli + xldot*delt + xndt*step2
               xni   = xni + xndt*delt + xnddt*step2
               atime = atime + delt
            end if

         end do

         xn = xni + xndt*ft  + xnddt*ft*ft*0.5_wp
         xl = xli + xldot*ft + xndt*ft*ft*0.5_wp

         if(irez /= 1) then
            mm   = xl-2.0_wp*nodem+2.0_wp*theta
            dndt = xn-no
         else
            mm   = xl-nodem-argpm+theta
            dndt = xn-no
         end if

         xn = no + dndt

      end if

      return
      end subroutine zzdspc

      !> Provide deep space long period periodic contributions to the mean
      !> elements. By design, these periodics are zero at epoch. This used
      !> to be dscom which included initialization, but it's really a
      !> recurring function.
      !>
      !> See Detailed_input and Detailed_Output.
      !>
      !>## Detailed Input
      !>
      !>     OPMODE     Flag indicating which technique
      !>                to use to calculate sidereal time.
      !>
      !>     E3         Internal SGD4 parameter.
      !>
      !>     EE2        Internal SGD4 parameter.
      !>
      !>     PEO        Internal SGD4 parameter.
      !>
      !>     PGHO       Internal SGD4 parameter.
      !>
      !>     PHO        Internal SGD4 parameter.
      !>
      !>     PINCO      Internal SGD4 parameter.
      !>
      !>     PLO        Internal SGD4 parameter.
      !>
      !>     SE2        Internal SGD4 parameter.
      !>
      !>     SE3        Internal SGD4 parameter.
      !>
      !>     SGH2       Internal SGD4 parameter.
      !>
      !>     SGH3       Internal SGD4 parameter.
      !>
      !>     SGH4       Internal SGD4 parameter.
      !>
      !>     SH2        Internal SGD4 parameter.
      !>
      !>     SH3        Internal SGD4 parameter.
      !>
      !>     SI2        Internal SGD4 parameter.
      !>
      !>     SI3        Internal SGD4 parameter.
      !>
      !>     SL2        Internal SGD4 parameter.
      !>
      !>     SL3        Internal SGD4 parameter.
      !>
      !>     SL4        Internal SGD4 parameter.
      !>
      !>     T          Time for state evaluation.
      !>
      !>     XGH2       Internal SGD4 parameter.
      !>
      !>     XGH3       Internal SGD4 parameter.
      !>
      !>     XGH4       Internal SGD4 parameter.
      !>
      !>     XH2        Internal SGD4 parameter.
      !>
      !>     XH3        Internal SGD4 parameter.
      !>
      !>     XI2        Internal SGD4 parameter.
      !>
      !>     XI3        Internal SGD4 parameter.
      !>
      !>     XL2        Internal SGD4 parameter.
      !>
      !>     XL3        Internal SGD4 parameter.
      !>
      !>     XL4        Internal SGD4 parameter.
      !>
      !>     ZMOL       Internal SGD4 parameter.
      !>
      !>     ZMOS       Internal SGD4 parameter.
      !>
      !>     INCLO      Unused argument. Maintained in call for historical
      !>                reference.
      !>
      !>     DOINIT     Flag indicating initialization state. True to
      !>                initialize, false otherwise.
      !>
      !>     ECCP       Eccentricity.
      !>
      !>     INCLP      Inclination.
      !>
      !>     NODEP      Right ascension of  ascending node.
      !>
      !>     ARGPP      Argument of periapsis.
      !>
      !>     MP         Mean anomaly.
      !>
      !>## Detailed Output
      !>
      !>     ECCP       Calculated perturbed eccentricity.
      !>
      !>     INCLP      Calculated perturbed inclination.
      !>
      !>     NODEP      Calculated perturbed right ascension of
      !>                ascending node.
      !>
      !>     ARGPP      Calculated perturbed argument of periapsis.
      !>
      !>     MP         Calculated perturbed mean anomaly.
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
      !>     This routine is based on the DPPER code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3" [4].
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        Capitalize all variables.
      !>
      !>        ENDIF replaced with END IF.
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
      !>   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>       Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>       U.S. Air Force: Aerospace Defense Command.
      !>
      !>   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>       of Space Command Element Sets." Space Command,
      !>       U. S. Air Force, CO.
      !>
      !>   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>       History of Analytical Orbit Modeling in the U. S. Space
      !>       Surveillance System. Journal of Guidance, Control, and
      !>       Dynamics. 27(2):174-185.
      !>
      !>   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>       Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     David Vallado   (AGI)
      !>     E. D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Tweaked wording of header Abstract section.
      !>        Fixed typos in comments.
      !>
      !>-    SPICELIB Version 1.0.0, SEP-15-2014 (EDW)
      !>
      !>        Based on routine DDPER, 28-JUN-2005, Vallado 2006 [4].
      module subroutine zzdspr ( ctx, opmode, e3, ee2, peo, pgho, pho, pinco, plo, se2, se3, sgh2, sgh3, sgh4, sh2, sh3, si2, si3, sl2, sl3, sl4, t, xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4, zmol, zmos, inclo, doinit, eccp, inclp, nodep, argpp, mp )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi, twopi
      use pepper_inc_zzsgp4
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: opmode  !! Flag indicating which technique to use to calculate sidereal time.
      real(wp), intent(in) :: e3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: ee2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: peo  !! Internal SGD4 parameter.
      real(wp), intent(in) :: pgho  !! Internal SGD4 parameter.
      real(wp), intent(in) :: pho  !! Internal SGD4 parameter.
      real(wp), intent(in) :: pinco  !! Internal SGD4 parameter.
      real(wp), intent(in) :: plo  !! Internal SGD4 parameter.
      real(wp), intent(in) :: se2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: se3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sgh2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sgh3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sgh4  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sh2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sh3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: si2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: si3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sl2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sl3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: sl4  !! Internal SGD4 parameter.
      real(wp), intent(in) :: t  !! Time for state evaluation.
      real(wp), intent(in) :: xgh2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xgh3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xgh4  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xh2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xh3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xi2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xi3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xl2  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xl3  !! Internal SGD4 parameter.
      real(wp), intent(in) :: xl4  !! Internal SGD4 parameter.
      real(wp), intent(in) :: zmol  !! Internal SGD4 parameter.
      real(wp), intent(in) :: zmos  !! Internal SGD4 parameter.
      real(wp), intent(in) :: inclo  !! Unused argument. Maintained in call for historical reference.
      logical, intent(in) :: doinit  !! Flag indicating initialization state. True to initialize, false otherwise.
      real(wp), intent(inout) :: eccp  !! Eccentricity.
      real(wp), intent(inout) :: inclp  !! Inclination.
      real(wp), intent(inout) :: nodep  !! Right ascension of  ascending node. ascending node.
      real(wp), intent(inout) :: argpp  !! Argument of periapsis.
      real(wp), intent(inout) :: mp  !! Mean anomaly.

      real(wp) :: alfdp
      real(wp) :: betdp
      real(wp) :: cosip
      real(wp) :: cosop
      real(wp) :: dalf
      real(wp) :: dbet
      real(wp) :: dls
      real(wp) :: f2
      real(wp) :: f3
      real(wp) :: pe
      real(wp) :: pgh
      real(wp) :: ph
      real(wp) :: pinc
      real(wp) :: pl
      real(wp) :: sel
      real(wp) :: ses
      real(wp) :: sghl
      real(wp) :: sghs
      real(wp) :: shl
      real(wp) :: shs
      real(wp) :: sil
      real(wp) :: sinip
      real(wp) :: sinop
      real(wp) :: sinzf
      real(wp) :: sis
      real(wp) :: sll
      real(wp) :: sls
      real(wp) :: xls
      real(wp) :: xnoh
      real(wp) :: zel
      real(wp) :: zes
      real(wp) :: zf
      real(wp) :: zm
      real(wp) :: znl
      real(wp) :: zns

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Local constants.
!
      zes  = 0.01675_wp
      zel  = 0.05490_wp
      zns  = 1.19459e-5_wp
      znl  = 1.5835218e-4_wp

!
!     Calculate time varying periodics.
!
      zm   = zmos + zns*t

      if (doinit) zm = zmos

      zf   = zm + 2.0_wp*zes*dsin(zm)
      sinzf= dsin(zf)
      f2   =  0.5_wp*sinzf*sinzf - 0.25_wp
      f3   = -0.5_wp*sinzf*dcos(zf)
      ses  = se2*f2 + se3*f3
      sis  = si2*f2 + si3*f3
      sls  = sl2*f2 + sl3*f3 + sl4*sinzf
      sghs = sgh2*f2 + sgh3*f3 + sgh4*sinzf
      shs  = sh2*f2 + sh3*f3
      zm   = zmol + znl*t

      if (doinit) zm = zmol

      zf   = zm + 2.0_wp*zel*dsin(zm)
      sinzf= dsin(zf)
      f2   =  0.5_wp*sinzf*sinzf - 0.25_wp
      f3   = -0.5_wp*sinzf*dcos(zf)
      sel  = ee2*f2 + e3*f3
      sil  = xi2*f2 + xi3*f3
      sll  = xl2*f2 + xl3*f3 + xl4*sinzf
      sghl = xgh2*f2 + xgh3*f3 + xgh4*sinzf
      shl  = xh2*f2 + xh3*f3
      pe   = ses + sel
      pinc = sis + sil
      pl   = sls + sll
      pgh  = sghs + sghl
      ph   = shs + shl

      if ( .not. doinit) then

         pe    = pe   - peo
         pinc  = pinc - pinco
         pl    = pl   - plo
         pgh   = pgh  - pgho
         ph    = ph   - pho
         inclp = inclp  + pinc
         eccp  = eccp   + pe
         sinip = dsin(inclp)
         cosip = dcos(inclp)

!
!        Apply periodics directly.
!
!        sgp4fix for lyddane choice
!
!        strn3 used original inclination - this is technically
!        feasible
!
!        gsfc used perturbed inclination - also technically feasible
!        probably best to readjust the 0.2 limit value and limit
!        discontinuity
!
!        0.2 rad = 11.45916 deg
!
!        use next line for original strn3 approach and original
!        inclination
!
!            IF (INCLO.GE.0.2D0) THEN
!
!        use next line for gsfc version and perturbed inclination
!
         if (inclp>=0.2_wp) then

            ph     = ph/sinip
            pgh    = pgh - cosip*ph
            argpp  = argpp + pgh
            nodep  = nodep + ph
            mp     = mp + pl

         else

!
!           Apply periodics with Lyddane modification.
!
            sinop  = dsin(nodep)
            cosop  = dcos(nodep)
            alfdp  = sinip*sinop
            betdp  = sinip*cosop
            dalf   =  ph*cosop + pinc*cosip*sinop
            dbet   = -ph*sinop + pinc*cosip*cosop
            alfdp  = alfdp + dalf
            betdp  = betdp + dbet
            nodep  = dmod(nodep,twopi)

!
!           sgp4fix for afspc written intrinsic functions
!           NODEP used without a trigonometric function ahead
!

            if ( (nodep  < 0.0_wp)      .and. &
                 (opmode == afspc)  )   then
               nodep = nodep + twopi
            end if

            xls    = mp + argpp + cosip*nodep
            dls    = pl + pgh - pinc*nodep*sinip
            xls    = xls + dls
            xnoh   = nodep
            nodep  = datan2(alfdp,betdp)

!
!           sgp4fix for afspc written intrinsic functions
!           NODEP used without a trigonometric function ahead
!

            if ( (nodep < 0.0_wp)  .and. &
                 (opmode == afspc)  )  then
               nodep = nodep + twopi
            end if

            if (dabs(xnoh-nodep) > pi) then

               if (nodep < xnoh) then
                  nodep = nodep+twopi
               else
                  nodep = nodep-twopi
               end if

            end if

            mp   = mp + pl
            argpp=  xls - mp - cosip*nodep

         end if

      end if

      return
      end subroutine zzdspr

      !> This subroutine initializes the SGP4 propagator. All the
      !> initialization is consolidated here instead of having multiple
      !> loops inside other routines.
      !>
      !> See Detailed_input and Detailed_Output.
      !>
      !>## Detailed Input
      !>
      !>     GEOPYS         Gephysical constants array.
      !>
      !>     ECCO           Eccentricity.
      !>
      !>     EPOCH          TLE epoch time in days from
      !>                    1950-00-00 00:00:00.000000 TDB, i.e.
      !>                    1949-12-31 00:00:00.000000 TDB.
      !>
      !>     INCLO          Inclination
      !>
      !>     OPMODE         Flag indicating which technique
      !>                    to use to calculate sidereal time.
      !>
      !>     NO             Mean motion.
      !>
      !>## Detailed Output
      !>
      !>     NO             Mean motion.
      !>
      !>     AINV           1.0/A0
      !>
      !>     AO             Semi major axis.
      !>
      !>     CON41          Value -CON42-COSIO2-COSIO2.
      !>
      !>     CON42          1.0 - 5.0*cos(inclination).
      !>
      !>     COSIO          Cosine of inclination.
      !>
      !>     COSIO2         COSIO squared.
      !>
      !>     ECCSQ          Eccentricity squared.
      !>
      !>     OMEOSQ         1.0 - ECCO * ECCO.
      !>
      !>     POSQ           Semi-parameter squared.
      !>
      !>     RP             Radius of perigee.
      !>
      !>     RTEOSQ         Square root of (1.0 - ECCO*ECCO).
      !>
      !>     SINIO          Sine of inclination.
      !>
      !>     GSTO           GST at time of observation.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) SPICE(UNKNOWNMODE) signals when the value of OPMODE
      !>        does not equal an assigned OPMODE value listed in
      !>        zzsgp4.inc.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is based on the INITL code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3" [4].
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        Removed getgravconst call, replaced with GEOPHS array.
      !>
      !>        Capitalize all variables.
      !>
      !>        ENDIF replaced with END IF.
      !>
      !>        RadPerDay     replaced with RADDAY
      !>
      !>        Opsmode       replaced with OPMODE
      !>
      !>        whichconst    eliminated, function provided by GEOPHS
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
      !>   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>       Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>       U.S. Air Force: Aerospace Defense Command.
      !>
      !>   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>       of Space Command Element Sets." Space Command,
      !>       U. S. Air Force, CO.
      !>
      !>   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>       History of Analytical Orbit Modeling in the U. S. Space
      !>       Surveillance System. Journal of Guidance, Control, and
      !>       Dynamics. 27(2):174-185.
      !>
      !>   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>       Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     David Vallado   (AGI)
      !>     E. D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, OCT-09-2014 (EDW)
      !>
      !>        Based on routine INITL, 28-JUN-2005, Vallado 2006 [4].
      module subroutine zzinil ( ctx, geophs, opmode, ecco, epoch, inclo, no, ainv, ao, con41, con42, cosio, cosio2, eccsq, omeosq, posq, rp, rteosq, sinio, gsto )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_zzsgp4
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: geophs    ( 8 )
      integer(ip), intent(in) :: opmode  !! Flag indicating which technique to use to calculate sidereal time.
      real(wp), intent(in) :: ecco  !! Eccentricity.
      real(wp), intent(in) :: epoch  !! TLE epoch time in days from 1950-00-00 00:00:00.000000 TDB, i.e. 1949-12-31 00:00:00.000000 TDB.
      real(wp), intent(in) :: inclo  !! Inclination
      real(wp), intent(inout) :: no  !! Mean motion.
      real(wp), intent(out) :: ainv  !! 1.0/A0
      real(wp), intent(out) :: ao  !! Semi major axis.
      real(wp), intent(out) :: con41  !! Value -CON42-COSIO2-COSIO2.
      real(wp), intent(out) :: con42  !! 1.0 - 5.0*cos(inclination).
      real(wp), intent(out) :: cosio  !! Cosine of inclination.
      real(wp), intent(out) :: cosio2  !! COSIO squared.
      real(wp), intent(out) :: eccsq  !! Eccentricity squared.
      real(wp), intent(out) :: omeosq  !! 1.0 - ECCO * ECCO.
      real(wp), intent(out) :: posq  !! Semi-parameter squared.
      real(wp), intent(out) :: rp  !! Radius of perigee.
      real(wp), intent(out) :: rteosq  !! Square root of (1.0 - ECCO*ECCO).
      real(wp), intent(out) :: sinio  !! Sine of inclination.
      real(wp), intent(out) :: gsto  !! GST at time of observation.

      real(wp) :: adel
      real(wp) :: ak
      real(wp) :: c1
      real(wp) :: c1p2p
      real(wp) :: d1
      real(wp) :: del
      real(wp) :: fk5r
      real(wp) :: j2
      real(wp) :: po
      real(wp) :: radday
      real(wp) :: temp
      real(wp) :: tfrac
      real(wp) :: thgr70
      real(wp) :: ts70
      real(wp) :: tut1
      real(wp) :: x2o3
      real(wp) :: xke

      integer(ip) :: ids70

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      j2    = geophs(k_j2)
      xke   = geophs(k_ke)
      x2o3  = 2.0_wp/3.0_wp

!
!     Calculate auxillary epoch quantities
!
      eccsq  = ecco*ecco
      omeosq = 1.0_wp - eccsq
      rteosq = dsqrt(omeosq)
      cosio  = dcos(inclo)
      cosio2 = cosio*cosio

!
!     Un-KOZAI the mean motion
!
      ak   =  (xke/no)**x2o3
      d1   =  0.75_wp*j2* (3.0_wp*cosio2-1.0_wp) / (rteosq*omeosq)
      del  =  d1/(ak*ak)
      adel =  ak * ( 1.0_wp - del*del - del* &
                       (1.0_wp/3.0_wp + 134.0_wp*del*del / 81.0_wp) )
      del  =  d1/(adel*adel)
      no   =  no/(1.0_wp + del)

      ao   =  (xke/no)**x2o3
      sinio=  dsin(inclo)
      po   =  ao*omeosq
      con42=  1.0_wp-5.0_wp*cosio2
      con41=  -con42-cosio2-cosio2
      ainv =  1.0_wp/ao
      posq =  po*po
      rp   =  ao*(1.0_wp-ecco)

!
!     Calculate greenwich location at epoch
!

!
!     sgp4fix Modern approach to finding sidereal time
!

      if (opmode == imprvd ) then

!
!        Radians per day, earth rotation, 6.30038809866574D0.
!
         radday = twopi * 1.002737909350795_wp
         temp   = epoch + 2433281.5_wp
         tut1= ( dint(temp-0.5_wp) + 0.5_wp - 2451545.0_wp ) / 36525.0_wp
         gsto= 1.75336855923327_wp + 628.331970688841_wp*tut1 &
                   + 6.77071394490334e-06_wp*tut1*tut1 &
                   - 4.50876723431868e-10_wp*tut1*tut1*tut1 &
                   + radday*( temp-0.5_wp-dint(temp-0.5_wp) )

      else if (opmode == afspc ) then

!
!        sgp4fix Use old way of finding GST
!
!        Count integer number of days from 0 jan 1970
!
         ts70  = epoch-7305.0_wp
         ids70 = int(ts70 + 1.0e-8_wp)
         tfrac = ts70-ids70

!
!        Find greenwich location at epoch
!
         c1     = 1.72027916940703639e-2_wp
         thgr70 = 1.7321343856509374_wp
         fk5r   = 5.07551419432269442e-15_wp
         c1p2p  = c1+twopi
         gsto   = thgr70+c1*ids70+c1p2p*tfrac+ts70*ts70*fk5r

      else

         call ctx%setmsg ( 'Unknown value for OPMODE. Value # not ' &
          //           'coded in zzsgp4.inc.'                         )
         call ctx%errint ( '#', opmode                                    )
         call ctx%sigerr ( 'SPICE(UNKNOWNMODE)'                           )
         return

      end if

!
!     Check quadrants
!
      gsto = dmod( gsto,twopi )

      if ( gsto < 0.0_wp ) then
         gsto= gsto + twopi
      end if

      return
      end subroutine zzinil

      !> Evaluate a single data record from a type 6 CK segment.
      !>
      !>## Detailed Input
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>     RECORD   is a record from a type 6 CK segment which, when
      !>              evaluated at the epoch contained in its first
      !>              element, will give the attitude and angular velocity
      !>              of a spacecraft structure or instrument relative to a
      !>              base reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | evaluation epoch     |
      !>                 +----------------------+
      !>                 | subtype code         |
      !>                 +----------------------+
      !>                 | number of packets (n)|
      !>                 +----------------------+
      !>                 | nominal SCLK rate    |
      !>                 +----------------------+
      !>                 | packet 1             |
      !>                 +----------------------+
      !>                 | packet 2             |
      !>                 +----------------------+
      !>                          .
      !>                          .
      !>                          .
      !>                 +----------------------+
      !>                 | packet n             |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>                See the CK Required Reading or the include file
      !>                ck06.inc for details on CK type 6 packet contents.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   has been modified due to its use as a workspace array.
      !>              The contents are undefined.
      !>
      !>     CMAT     is a rotation matrix that transforms the components
      !>              of a vector expressed in the base frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at the returned time.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              base frame, then v has components x', y', z' in the
      !>              instrument fixed frame where:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                            (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector of the instrument fixed
      !>              frame defined by CMAT. The angular velocity is
      !>              returned only if NEEDAV is .TRUE.
      !>
      !>              The direction of the angular velocity vector gives
      !>              the right-handed axis about which the instrument fixed
      !>              reference frame is rotating. The magnitude of AV is
      !>              the magnitude of the instantaneous velocity of the
      !>              rotation, in radians per second.
      !>
      !>              The angular velocity vector is returned in component
      !>              form
      !>
      !>                       AV = [ AV1  , AV2  , AV3  ]
      !>
      !>              which is in terms of the base coordinate frame
      !>              specified in the segment descriptor.
      !>
      !>     CLKOUT   is the encoded SCLK associated with the returned
      !>              C-matrix and angular velocity vector.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input record contains an unrecognized subtype code, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the record subtype is one for which quaternion derivatives
      !>         are stored (subtypes 0 and 2), and if the Ith quaternion in
      !>         the input record is farther than its negative from the (I-1)st
      !>         quaternion in the record, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>         For subtypes 1 and 3, this condition is not considered an
      !>         error: the closer to the preceding quaternion of the two
      !>         quaternion representations is used for interpolation.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of CK type 6 (MEX/Rosetta Attitude
      !>     file interpolation) CK segments is described in the CK Required
      !>     Reading.
      !>
      !>## Examples
      !>
      !>     The CKEnn routines are almost always used in conjunction with
      !>     the corresponding CKRnn routines, which read the records from
      !>     CK files.
      !>
      !>     The following code fragment searches through all of the segments
      !>     in a file applicable to the Mars Express spacecraft bus that
      !>     are of data type 6, for a particular spacecraft clock time.
      !>     It then evaluates the pointing for that epoch and prints the
      !>     result.
      !>
      !>           CHARACTER*(20)        SCLKCH
      !>           CHARACTER*(20)        SCTIME
      !>           CHARACTER*(40)        IDENT
      !>
      !>           INTEGER               I
      !>           INTEGER               SC
      !>           INTEGER               INST
      !>           INTEGER               HANDLE
      !>           INTEGER               DTYPE
      !>           INTEGER               ICD      (    6 )
      !>
      !>           DOUBLE PRECISION      SCLKDP
      !>           DOUBLE PRECISION      TOL
      !>           DOUBLE PRECISION      CLKOUT
      !>           DOUBLE PRECISION      DESCR    (    5 )
      !>           DOUBLE PRECISION      DCD      (    2 )
      !>           DOUBLE PRECISION      RECORD   (   17 )
      !>           DOUBLE PRECISION      CMAT     ( 3, 3 )
      !>           DOUBLE PRECISION      AV       (    3 )
      !>
      !>           LOGICAL               NEEDAV
      !>           LOGICAL               FND
      !>           LOGICAL               SFND
      !>
      !>           SC     = -41
      !>           INST   = -41000
      !>           DTYPE  =  6
      !>           NEEDAV = .FALSE.
      !>
      !>     C
      !>     C     Load the MEX SCLK kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH ( 'MEX_SCLK.TSC'       )
      !>           CALL DAFOPR ( 'MEX_CK.BC',  HANDLE )
      !>     C
      !>     C     Get the spacecraft clock time. Then encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           WRITE (*,*) 'Enter spacecraft clock time string:'
      !>           READ (*,FMT='(A)') SCLKCH
      !>
      !>           CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>     C
      !>     C     Use a tolerance of 2 seconds ( half of the nominal
      !>     C     separation between MEX pointing instances ).
      !>     C
      !>           CALL SCTIKS ( SC, '0000000002:000', TOL )
      !>
      !>     C
      !>     C     Search from the beginning of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( SFND   )
      !>
      !>           FND    = .FALSE.
      !>
      !>           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
      !>
      !>     C
      !>     C        Get the segment identifier and descriptor.
      !>     C
      !>              CALL DAFGN ( IDENT )
      !>              CALL DAFGS ( DESCR )
      !>     C
      !>     C        Unpack the segment descriptor into its integer and
      !>     C        double precision components.
      !>     C
      !>              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>     C
      !>     C        Determine if this segment should be processed.
      !>     C
      !>              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
      !>          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
      !>          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
      !>          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
      !>
      !>                 CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                   RECORD, FND )
      !>
      !>                 IF ( FND ) THEN
      !>
      !>                    CALL CKE06 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
      !>
      !>                    CALL SCDECD ( SC, CLKOUT, SCTIME )
      !>
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Segment identifier: ', IDENT
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Pointing returned for time: ',
      !>          .                      SCTIME
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
      !>                    WRITE (*,*)
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  This routine performs minimal error checking. The input data
      !>         are assumed to have been checked when the source CK file was
      !>         created.
      !>
      !>     2)  With the exception of the check described in item 2 of
      !>         the $Exceptions section above, the input data are assumed to
      !>         be suitable for the interpolation method specified by the
      !>         input record's subtype and packet count (which implies an
      !>         interpolating polynomial degree).
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
      module subroutine cke06 ( ctx, needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      use pepper_inc_ck06
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: needav  !! .TRUE. if angular velocity is requested.
      real(wp), intent(inout) :: record   ( * )  !! Data type 6 record.
      real(wp), intent(out) :: cmat     ( 3, 3 )  !! C-matrix.
      real(wp), intent(out) :: av       ( 3 )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! SCLK associated with C-matrix.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Given that our nominally type 6 input record is actually a
!     valid type 5 record, we let the type 5 evaluator do the
!     work.
!
      call cke05 ( ctx, needav, record, cmat, av, clkout )

      return
      end subroutine cke06

      !> Return (depending upon the user's request) the ID code of either
      !> the spacecraft or spacecraft clock associated with a C-Kernel ID
      !> code.
      !>
      !>## Detailed Input
      !>
      !>     CKID     is the ID code for some object whose attitude
      !>              and possibly angular velocity are stored in
      !>              some C-kernel.
      !>
      !>     META     is a character string that indicates which piece
      !>              of meta data to fetch. Acceptable values are
      !>              'SCLK' and 'SPK'. The routine is case insensitive.
      !>              Leading and trailing blanks are insignificant.
      !>              However, blanks between characters are regarded
      !>              as being significant.
      !>
      !>## Detailed Output
      !>
      !>     IDCODE   if META is 'SCLK' then the value returned in IDCODE
      !>              is the ID code of the spacecraft clock used for
      !>              converting ET to TICKS and TICKS to ET for the
      !>              C-kernel used to represent the attitude of the
      !>              object with ID code CKID.
      !>
      !>              If META is 'SPK' then the value returned in IDCODE is the
      !>              ID code of the spacecraft on which the platform indicated
      !>              by CKID is mounted.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the variable META is not recognized to be one of the
      !>         inputs 'SPK' or 'SCLK', the error SPICE(UNKNOWNCKMETA)
      !>         is signaled.
      !>
      !>     2)  If CKID is greater than -1000, the associated SCLK and SPK
      !>         IDs must be in the kernel pool. If they are not present
      !>         a value of zero is returned for the requested item. Zero
      !>         is never the valid ID of a spacecraft clock.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine for mapping C-kernels to associated
      !>     spacecraft clocks.
      !>
      !>     An association of an SCLK ID and spacecraft ID with a CK frame
      !>     class ID may be made by placing in a text kernel the kernel
      !>     variable assignments
      !>
      !>        CK_<ck_frame_class_ID_code>_SCLK = <ID code of SCLK>
      !>        CK_<ck_frame_class_ID_code>_SPK  = <SPK ID code>
      !>
      !>     See the Frames Required Reading section on CK frames.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose you would like to look up the attitude of an object
      !>        in a C-kernel but have ET and seconds as your input time and
      !>        tolerance.
      !>
      !>        Use the meta-kernel shown below to load the required SPICE
      !>        kernels.
      !>
      !>           KPL/MK
      !>
      !>           File name: ckmeta_ex1.tm
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
      !>              naif0012.tls           Leapseconds
      !>              04153_04182ca_ISS.bc   CASSINI image navigated
      !>                                     spacecraft CK
      !>
      !>           \begindata
      !>
      !>              KERNELS_TO_LOAD = ( 'naif0012.tls',
      !>                                  'cas00071.tsc'
      !>                                  '04153_04182ca_ISS.bc' )
      !>
      !>           \begintext
      !>
      !>           End of meta-kernel
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CKMETA_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>C     -- The code for CASSINI spacecraft reference frame is
      !>C        -82000.
      !>C
      !>C     -- The reference frame we want is J2000.
      !>C
      !>      CHARACTER*(*)         REF
      !>      PARAMETER           ( REF = 'J2000' )
      !>
      !>      INTEGER               CKID
      !>      PARAMETER           ( CKID = -82000  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      AV     ( 3 )
      !>      DOUBLE PRECISION      CLKOUT
      !>      DOUBLE PRECISION      CMAT   ( 3, 3 )
      !>      DOUBLE PRECISION      ET
      !>      DOUBLE PRECISION      ETOUT
      !>      DOUBLE PRECISION      SECTOL
      !>      DOUBLE PRECISION      TICKS
      !>      DOUBLE PRECISION      TICK2
      !>      DOUBLE PRECISION      TOL
      !>
      !>      INTEGER               IDCODE
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Initial values.
      !>C
      !>      DATA                  ET      / 141162208.034340D0 /
      !>      DATA                  SECTOL  / 0.5D0 /
      !>
      !>C
      !>C     First load the CK, LSK and SCLK files.
      !>C
      !>      CALL FURNSH ( 'ckmeta_ex1.tm' )
      !>
      !>C
      !>C     Get the SCLK identifier of the spacecraft clock required
      !>C     to convert from ET to TICKS.
      !>C
      !>      CALL CKMETA ( CKID, 'SCLK', IDCODE )
      !>
      !>C
      !>C     Convert ET and ET+SECTOL to spacecraft clock ticks.
      !>C
      !>      CALL SCE2C  ( IDCODE, ET,        TICKS  )
      !>      CALL SCE2C  ( IDCODE, ET+SECTOL, TICK2  )
      !>
      !>C
      !>C     Compute the tolerance in spacecraft clock ticks.
      !>C
      !>      TOL = TICK2 - TICKS
      !>
      !>C
      !>C     Look the attitude up.
      !>C
      !>      CALL CKGPAV ( CKID, TICKS, TOL,    REF,
      !>     .              CMAT, AV,    CLKOUT, FOUND )
      !>
      !>      WRITE(*,'(A,F20.6)')    'Input ET:            ', ET
      !>
      !>      IF ( FOUND ) THEN
      !>
      !>         CALL SCT2E ( IDCODE, CLKOUT, ETOUT )
      !>         WRITE(*,'(A,F20.6)') 'Attitude found at ET:', ETOUT
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(A)') 'No attitude found at ET.'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Input ET:                141162208.034340
      !>        Attitude found at ET:    141162208.034586
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
      !>-    SPICELIB Version 1.2.1, 07-JUN-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing fragment.
      !>
      !>-    SPICELIB Version 1.2.0, 06-SEP-2013 (BVS)
      !>
      !>        BUG FIX: the POOL agents now watch both variables --
      !>        CK_<ID>_SCLK and CK_<ID>_SPK. Before they watched only
      !>        CK_<ID>_SCLK.
      !>
      !>        BUG FIX: if a previously available CK_<ID>_SCLK or CK_<ID>_SPK
      !>        variable that was used to populate a saved value disappears,
      !>        the routine now resets and returns the value based on the
      !>        default rule rather than keeping and returning the stale
      !>        POOL-based saved value.
      !>
      !>        BUG FIX: the routine now deletes watchers for the CK IDs that
      !>        were bumped from the local buffer.
      !>
      !>        Updated to keep track of agent-specific POOL counters and call
      !>        ZZCVPOOL to make use of them.
      !>
      !>-    SPICELIB Version 1.1.0, 05-MAR-2009 (NJB)
      !>
      !>        This routine now keeps track of whether its kernel pool
      !>        look-up failed. If so, a kernel pool lookup is attempted on
      !>        the next call to this routine. This change is an enhancement,
      !>        not a bug fix (unlike similar modifications in SCLK routines).
      !>
      !>        Header sections were put in correct order.
      !>
      !>-    SPICELIB Version 1.0.1, 09-MAR-1999 (NJB)
      !>
      !>        Comments referring to SCE2T have been updated to refer to
      !>        SCE2C. Occurrences of "id" replaced by "ID."
      !>
      !>-    SPICELIB Version 1.0.0, 04-OCT-1994 (WLT)
      module subroutine ckmeta ( ctx, ckid, meta, idcode )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bschoi, orderi
      use pepper_strings, only: intstr, ljucrs, prefix, suffix
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ckid  !! The ID code for some C kernel object.
      character(*), intent(in) :: meta  !! The kind of meta data requested 'SPK' or 'SCLK'
      integer(ip), intent(out) :: idcode  !! The requested SCLK or spacecraft ID code.
      integer(ip), parameter :: nck = 30
      integer(ip), parameter :: sclk = 1
      integer(ip), parameter :: spk = sclk   + 1
      character(7), parameter :: base = 'CKMETA.'
      integer(ip) :: this
      integer(ip) :: n
      logical :: update
      character(7) :: mymeta
      if ( .not. allocated ( ctx%ckmeta_state ) ) allocate ( ctx%ckmeta_state )
      associate ( lookup => ctx%ckmeta_state%lookup, &
                  agent => ctx%ckmeta_state%agent, &
                  cks => ctx%ckmeta_state%cks, &
                  cksord => ctx%ckmeta_state%cksord, &
                  currnt => ctx%ckmeta_state%currnt, &
                  last => ctx%ckmeta_state%last, &
                  sclks => ctx%ckmeta_state%sclks, &
                  spks => ctx%ckmeta_state%spks, &
                  usrctr => ctx%ckmeta_state%usrctr, &
                  first => ctx%ckmeta_state%first, &
                  found => ctx%ckmeta_state%found, &
                  nodata => ctx%ckmeta_state%nodata )
      if ( ctx%failed() ) return

      if ( first ) then

!
!        Initialize all agent-specific POOL counters to user value.
!
         do n = 1, nck
            call zzctruin( ctx, usrctr( 1, n ) )
         end do

!
!        Clear AGENTS array. We will use a non-blank AGENT value as the
!        flag to delete previously set watchers.
!
         agent = ' '

         first = .false.

      end if

!
!     Get an upper-case, left-justified copy of the metadata
!     type ('SCLK' or 'SPK').
!
      call ljucrs ( 1, meta, mymeta )

!
!     See if we already have this CK ID in hand.
!
      this = bschoi ( ckid, currnt, cks, cksord )

      if ( this > 0 ) then
!
!        We've got it.  Check to see if its value has been updated.
!        (Note that every CK ID  has its own agent and saved POOL
!        counter.)
!
         call zzcvpool ( ctx, agent( this ), usrctr( 1, this ), update )

         if ( update .or. nodata ) then

            call gipool( ctx, lookup(sclk,this), 1, 1, n, &
                         sclks(this),       found(sclk) )

            call gipool( ctx, lookup(spk, this), 1, 1, n, &
                         spks (this),       found(spk) )

            if ( ctx%failed() ) then

               nodata = .true.

               return

            end if

!
!           Note that failure to find data is not an error in this
!           routine; it's just SPICE errors that are a problem.
!
            nodata = .false.

         else

!
!           The POOL variables did not change since the last check and
!           we have already buffered IDs for this CK ID. Set found
!           flags to make use of saved values.
!
            found(sclk) = .true.
            found(spk)  = .true.

         end if

      else

!
!        We don't have this on our handy list. Find a place to put it.
!
         if ( currnt < nck ) then

            currnt = currnt + 1
            last   = currnt

         else

            last   = last + 1

            if ( last > nck ) then
               last = 1
            end if

         end if

         this = last

!
!        If we already have a watcher at this index, delete it. Note
!        we may have an update pending for this watcher, so we will
!        check it first to clear it.
!
         if ( agent( this ) /= ' ' ) then
            call cvpool ( ctx, agent( this ), update )
            call dwpool ( ctx, agent( this ) )
         end if

!
!        Recompute the order vector for the CKS; construct the
!        kernel pool variable names and the agent name.
!
         cks(this) = ckid

         call orderi (  cks,    currnt, cksord            )

         call intstr (  ckid,           lookup(sclk,this) )
         call prefix ( 'CK_',   0,      lookup(sclk,this) )

         agent (     this) =  base   // lookup(sclk,this)
         lookup(spk, this) =            lookup(sclk,this)

         call suffix ( '_SCLK',  0,     lookup(sclk,this) )
         call suffix ( '_SPK',   0,     lookup(spk, this) )

!
!        Set a watch for this item and fetch the current value
!        from the kernel pool (if there is a value there).
!
         call swpool ( ctx, agent( this ), 2, lookup(1,this) )

         call cvpool ( ctx, agent( this ), update )

         call gipool ( ctx,lookup(sclk,this), 1, 1, n, &
                      sclks(this),       found(sclk) )

         call gipool ( ctx,lookup(spk, this), 1, 1, n, &
                      spks (this),       found(spk ) )

         if ( ctx%failed() ) then

            nodata = .true.

            return

         end if

!
!        Note that failure to find data is not an error in this
!        routine; it's just SPICE errors that are a problem.
!
!        At this point, kernel data checks are done.
!
         nodata = .false.

      end if

!
!     If we didn't find either _SCLK or _SPK variable, we manufacture
!     an ID code based upon the "convention" used for all CKS so far.
!     However, the convention assumes that the CK ID will be less than
!     -1000 if it's not there is no sensible ID to return.  We return
!     zero in that case.
!
      if ( .not. found(sclk) ) then

         if ( cks(this) <= -1000 ) then

            sclks(this) = cks(this)/1000

         else

            sclks(this) = 0

         end if

      end if

      if ( .not. found(spk) ) then

         if ( cks(this) <= -1000 ) then

            spks(this) = cks(this)/1000

         else

            spks(this) = 0

         end if

      end if

!
!     Set output ID.
!
      if ( mymeta == 'SPK' ) then

         idcode = spks (this)

      else if ( mymeta == 'SCLK' ) then

         idcode = sclks(this)

      else

         idcode = 0

         call ctx%setmsg ( 'The CK meta data item "#" is not a ' &
         //            'recognized meta data item for the ' &
         //            'routine CKMETA. The recognized value ' &
         //            'are "SPK" and "SCLK". ' )

         call ctx%errch  ( '#', meta )
         call ctx%sigerr ( 'SPICE(UNKNOWNCKMETA)'  )
         return

      end if

      return
      end associate
      end subroutine ckmeta

      !> Return the number of pointing records in a CK type 04 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of the binary CK file containing the
      !>              segment. The file should have been opened for read
      !>              or write access, either by CKLPF, DAFOPR, or DAFOPW.
      !>
      !>     DESCR    is the packed descriptor of a data type 4 segment.
      !>
      !>## Detailed Output
      !>
      !>     NREC     is the number of pointing records in the type 4
      !>              segment.
      !>
      !>## Parameters
      !>
      !>     See 'ckparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If the segment indicated by DESCR is not a type 4 segment,
      !>         the error SPICE(CKWRONGDATATYPE) is signaled.
      !>
      !>     2)  If the specified handle does not belong to any DAF file that
      !>         is currently known to be open, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If DESCR is not a valid descriptor of a segment in the CK
      !>         file specified by HANDLE, the results of this routine are
      !>         unpredictable.
      !>
      !>## Files
      !>
      !>     The file specified by HANDLE should be open for read or
      !>     write access.
      !>
      !>## Particulars
      !>
      !>     For a complete description of the internal structure of a type 4
      !>     segment, see the CK required reading.
      !>
      !>     This routine returns the number of pointing records contained
      !>     in the specified segment. It is normally used in conjunction
      !>     with CKGR04 which returns the Ith pointing record in the
      !>     segment.
      !>
      !>## Examples
      !>
      !>     Suppose that DATA.BC is a CK file that contains segments of
      !>     data type 4. Then the following code fragment extracts the
      !>     data packets contained in the segment.
      !>
      !>     C
      !>     C     CK parameters include file.
      !>     C
      !>           INCLUDE               'ckparam.inc'
      !>     C
      !>     C     $Declarations.
      !>     C
      !>           DOUBLE PRECISION      DCD    ( 2 )
      !>           DOUBLE PRECISION      DESCR  ( 5 )
      !>           DOUBLE PRECISION      PKTDAT ( CK4RSZ )
      !>
      !>           INTEGER               AVFLAG
      !>           INTEGER               HANDLE
      !>           INTEGER               I
      !>           INTEGER               ICD    ( 6 )
      !>           INTEGER               K
      !>           INTEGER               LASTAD
      !>           INTEGER               NCOEF  ( QAVSIZ )
      !>           INTEGER               NREC
      !>
      !>           LOGICAL               FOUND
      !>     C
      !>     C     First load the file. (The file may also be opened by using
      !>     C     CKLPF.)
      !>     C
      !>           CALL DAFOPR ( 'DATA.BC', HANDLE )
      !>     C
      !>     C     Begin forward search. Find the first array.
      !>     C
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( FOUND  )
      !>     C
      !>     C     Get segment descriptor.
      !>     C
      !>           CALL DAFGS ( DESCR )
      !>     C
      !>     C     Unpack the segment descriptor into its double precision
      !>     C     and integer components.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>
      !>           IF ( ICD( 3 ) .EQ. 4 ) THEN
      !>     C
      !>     C        How many records does this segment contain?
      !>     C
      !>              CALL CKNR04 ( HANDLE, DESCR, NREC )
      !>
      !>              DO I = 1, NREC
      !>     C
      !>     C           Get the data records stored in the segment.
      !>     C
      !>                 CALL CKGR04 ( HANDLE, DESCR, I, PKTDAT )
      !>     C
      !>     C           Print data packet contents. Print coverage interval
      !>     C           midpoint & radii first.
      !>     C
      !>                 WRITE (2,*) PKTDAT (1)
      !>                 WRITE (2,*) PKTDAT (2)
      !>     C
      !>     C           Decode numbers of coefficients.
      !>     C
      !>                 CALL ZZCK4D2I ( PKTDAT(3), QAVSIZ, CK4PCD, NCOEF )
      !>     C
      !>     C           Print number of coefficients for Q0, Q1, Q2 and Q3.
      !>     C
      !>                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 1 ), NCOEF( 2 )
      !>                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 3 ), NCOEF( 4 )
      !>     C
      !>     C           Print number coefficients for AV1, AV2 and AV3.
      !>     C
      !>                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 5 ), NCOEF( 6 )
      !>                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 7 )
      !>     C
      !>     C           Print Cheby coefficients.
      !>     C
      !>                 LASTAD = 0
      !>
      !>                 DO K = 1, QAVSIZ
      !>                    LASTAD = LASTAD + NCOEF( K )
      !>                 END DO
      !>
      !>                 DO K = 4, LASTAD + 4
      !>                    WRITE (2,*) PKTDAT (K)
      !>                 END DO
      !>
      !>              END DO
      !>
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  The binary CK file containing the segment whose descriptor
      !>         was passed to this routine must be opened for read or write
      !>         access by either CKLPF, DAFOPR, or DAFOPW.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     Y.K. Zaiko         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
      !>
      !>        Minor header edits.
      !>
      !>-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
      module subroutine cknr04 ( ctx, handle, descr, nrec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! The handle of the file containing the segment.
      real(wp), intent(in) :: descr   ( * )  !! The descriptor of the type 4 segment.
      integer(ip), intent(out) :: nrec  !! The number of pointing records in the segment.



      real(wp) :: dcd    ( ndc )

      integer(ip) :: icd    ( nic )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check whether our segment is of the type 4 by unpacking
!     descriptor and checking value of its third integer component.
!
      call dafus ( ctx, descr, ndc, nic, dcd, icd )

      if ( icd( 3 ) /= ck4dtp ) then
         call ctx%setmsg ( 'Data type of the segment should be 4: '      // &
                       'Passed descriptor shows type = #.'           )
         call ctx%errint ( '#', icd ( 3 )                                )
         call ctx%sigerr ( 'SPICE(CKWRONGDATATYPE)'                      )
         return
      end if

!
!     The number of records (packets) can be obtained by a call to
!     SGMETA. This number is a meta item 12 (see sgparam.inc for
!     details.)
!
      call sgmeta ( ctx, handle, descr, 12, nrec )

!
!     All done.
!

      return

      end subroutine cknr04

      !> Evaluate a single PCK data record from a segment of type 2
      !> (Chebyshev Polynomials, position only).
      !>
      !>## Detailed Input
      !>
      !>     ET       is an epoch, at which the Euler angles are to
      !>              be computed.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give the Euler angles of some body.
      !>
      !>## Detailed Output
      !>
      !>     EULANG   is the Euler angles and their derivatives at
      !>              time ET.
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
      !>     The exact format and structure of type 2 (Chebyshev polynomials,
      !>     position only) segments are described in the PCK Required Reading
      !>     file.
      !>
      !>     A type 2 segment contains three sets of Chebyshev coefficients,
      !>     one set each for the Euler angles phi, delta and psi.  PCKE02
      !>     calls the routine SPKE02 for each set to evaluate the polynomial
      !>     AND its first derivative.
      !>
      !>## Examples
      !>
      !>     The PCKEnn routines are almost always used in conjunction with
      !>        the corresponding PCKRnn routines, which read the records from
      !>        binary PCK files.
      !>
      !>        The data returned by the PCKRnn routine is in its rawest form,
      !>        taken directly from the segment. As such, it will be
      !>        meaningless to a user unless he/she understands the structure
      !>        of the data type completely. Given that understanding, however,
      !>        the PCKRnn routines might be used to examine raw segment data
      !>        before evaluating it with the PCKEnn routines.
      !>
      !>     Here we load a binary PCK files and use PCKE02 to get the
      !>     Euler angles.
      !>
      !>     C
      !>     C  Load binary PCK file.
      !>     C
      !>        CALL PCKLOF ('example.pck', HANDLE)
      !>
      !>     C  Get a segment applicable to a specified body and epoch.
      !>
      !>        CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>        IF ( FOUND ) THEN
      !>
      !>           Look at parts of the descriptor.
      !>
      !>           CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
      !>           TYPE   = ICD( NT )
      !>           REF    = ICD( NR )
      !>
      !>           IF ( TYPE .EQ. 2 ) THEN
      !>
      !>              Read in Chebyshev coefficients from segment.
      !>
      !>              CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
      !>
      !>              Call evaluation routine to get Euler angles
      !>              phi, delta, w.
      !>
      !>              CALL PCKE02 ( ET, RECORD, EULANG )
      !>
      !>     The Euler angles and their derivatives are returned
      !>     in EULANG.
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
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 1.1.0, 13-MAR-1995 (KSZ)
      !>
      !>         Added error handling.
      !>
      !>-    SPICELIB Version 1.0.0, 30-SEP-1994 (KSZ)
      module subroutine pcke02 ( ctx, et, record, eulang )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: eulang   ( 6 )  !! Euler angles and their derivatives.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Call evaluation routine to get Euler angles
!     phi, delta, w.
!
      call spke02 ( ctx, et, record, eulang )

!
!     Mod the 3rd element of the state by TWOPI.
!     We do this because we've always done this.
!
      eulang(3) = mod ( eulang(3), twopi )

      return
      end subroutine pcke02

      !> Evaluate a single PCK data record from a segment of type 20
      !> (Chebyshev Polynomials, rotation derivative only).
      !>
      !>## Detailed Input
      !>
      !>     ET       is an epoch at which the Euler angles state is to be
      !>              computed. The epoch is represented as seconds past
      !>              J2000 TDB.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will yield Euler angles and Euler angle rates
      !>              representing the orientation and angular velocity,
      !>              with respect to its base frame, of the reference
      !>              frame associated with the input record.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +--------------------------------------+
      !>                 | record size (excluding this element) |
      !>                 +--------------------------------------+
      !>                 | Coverage interval midpoint           |
      !>                 +--------------------------------------+
      !>                 | Coverage interval radius             |
      !>                 +--------------------------------------+
      !>                 | Coeffs for ANGLE_1 rate              |
      !>                 +--------------------------------------+
      !>                 | Coeffs for ANGLE_2 rate              |
      !>                 +--------------------------------------+
      !>                 | Coeffs for ANGLE_3 rate              |
      !>                 +--------------------------------------+
      !>                 | ANGLE_1 at interval midpoint         |
      !>                 +--------------------------------------+
      !>                 | ANGLE_2 at interval midpoint         |
      !>                 +--------------------------------------+
      !>                 | ANGLE_3 at interval midpoint         |
      !>                 +--------------------------------------+
      !>
      !>              In the above record
      !>
      !>                 - Times are expressed as seconds past J2000 TDB.
      !>                 - Angular components have units of radians.
      !>                 - Rate coefficients have units of radians/s.
      !>
      !>              RECORD must be declared by the caller with size large
      !>              enough to accommodate the largest record that can be
      !>              returned by PCKR20.
      !>
      !>## Detailed Output
      !>
      !>     EULANG   is a 6-vector containing Euler angles and their
      !>              derivatives at time ET. The angles occupy the first
      !>              three elements of EULANG; the rates follow. The order
      !>              of the components is
      !>
      !>                 ( ANGLE_1, ANGLE_2, ANGLE_3,
      !>                   rate_1,  rate_2,  rate_3  )
      !>
      !>              The angular units are radians; the rate units are
      !>              radians/second.
      !>
      !>              The Euler angles represent the orientation, relative
      !>              to its base frame, of the PCK frame associated with
      !>              the input record. The angles, which are numbered
      !>              according to their ordinal position in the logical
      !>              records, define a transformation matrix R as follows:
      !>
      !>                 R = [ ANGLE_3 ]  [ ANGLE_2 ]  [ ANGLE_1 ]
      !>                                3            1            3
      !>
      !>              Here the notation
      !>
      !>                 [ THETA ]
      !>                          i
      !>
      !>              denotes a reference frame rotation of THETA radians
      !>              in the right-hand sense about the ith coordinate
      !>              axis. See the Rotation Required Reading for further
      !>              discussion of this notation.
      !>
      !>              The matrix R transforms vectors expressed in the base
      !>              frame to vectors expressed in the PCK frame associated
      !>              with RECORD by left multiplication:
      !>
      !>                 V    = R * V
      !>                  PCK        FRAME
      !>
      !>              In cases where the PCK frame is a body-fixed,
      !>              right-handed frame with its +Z axis aligned with a
      !>              body's north pole, the orientation angles are related
      !>              to right ascension (RA) and declination (DEC) of the
      !>              PCK frame's north pole, and prime meridian
      !>              orientation (W), by the equations
      !>
      !>                 ANGLE_1 = RA   + pi/2 radians
      !>                 ANGLE_2 = pi/2 - DEC  radians
      !>                 ANGLE_3 = W           radians
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input record contains an invalid coefficient count, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the input record contains invalid domain transformation
      !>         parameters, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The exact format and structure of type 20 (Chebyshev polynomials,
      !>     position only) segments are described in the PCK Required Reading
      !>     file.
      !>
      !>     A type 20 segment contains three sets of Chebyshev coefficients,
      !>     one set each for the derivatives with respect to time of the
      !>     Euler angles phi, delta and psi. PCKE20 calls the routine SPKE20
      !>     for each set to evaluate the polynomial and its first derivative.
      !>
      !>## Examples
      !>
      !>     The PCKEnn routines are almost always used in conjunction with
      !>        the corresponding PCKRnn routines, which read the records from
      !>        binary PCK files.
      !>
      !>        The data returned by the PCKRnn routine are in their rawest
      !>        form, taken directly from the segment. As such, they will be
      !>        meaningless to a user unless he/she understands the structure
      !>        of the data type completely. Given that understanding, however,
      !>        the PCKRnn routines might be used to examine raw segment data
      !>        before evaluating it with the PCKEnn routines.
      !>
      !>        Here we load a binary PCK files and use PCKE20 to get the
      !>        Euler angles.
      !>
      !>     C
      !>     C     Load binary PCK file.
      !>     C
      !>           CALL PCKLOF ('example.pck', HANDLE)
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>           IF ( FOUND ) THEN
      !>     C
      !>     C        Look at parts of the descriptor.
      !>     C
      !>              CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
      !>              TYPE   = ICD( NT )
      !>              REF    = ICD( NR )
      !>
      !>              IF ( TYPE .EQ. 20 ) THEN
      !>     C
      !>     C           Read in Chebyshev coefficients from segment.
      !>     C
      !>                 CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
      !>     C
      !>     C           Call evaluation routine to get Euler angles
      !>     C           phi, delta, w.
      !>     C
      !>                 CALL PCKE20 ( ET, RECORD, EULANG )
      !>
      !>        The Euler angles and their derivatives are returned
      !>        in EULANG.
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
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (KSZ)
      module subroutine pcke20 ( ctx, et, record, eulang )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Evaluation epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: eulang   ( 6 )  !! Euler angles and their derivatives.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Call evaluation routine to get Euler angles
!     phi, delta, w.
!
      call spke20 ( ctx, et, record, eulang )

!
!     Map the third angle into the range (-2pi, 2pi).
!
      eulang(3) = mod ( eulang(3), twopi )

      return
      end subroutine pcke20

      !> Compute the state of a massless body at time t_0 + DT by applying
      !> the two-body force model to a given central mass and a given body
      !> state at time t_0.
      !>
      !>## Detailed Input
      !>
      !>     GM       is the gravitational constant G times the mass M of the
      !>              central body.
      !>
      !>     PVINIT   is the state at some specified time relative to the
      !>              central mass. The mass of the object is assumed to
      !>              be negligible when compared to the central mass.
      !>
      !>     DT       is a offset in time from the time of the initial
      !>              state to which the two-body state should be
      !>              propagated. (The units of time and distance must be
      !>              the same in GM, PVINIT, and DT).
      !>
      !>## Detailed Output
      !>
      !>     PVPROP   is the two-body propagation of the initial state
      !>              DT units of time past the epoch of the initial state.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If GM is not positive, the error SPICE(NONPOSITIVEMASS) is
      !>         signaled.
      !>
      !>     2)  If the position of the initial state is the zero vector, the
      !>         error SPICE(ZEROPOSITION) is signaled.
      !>
      !>     3)  If the velocity of the initial state is the zero vector, the
      !>         error SPICE(ZEROVELOCITY) is signaled.
      !>
      !>     4)  If the cross product of the position and velocity of PVINIT
      !>         has squared length of zero, the error SPICE(NONCONICMOTION)
      !>         is signaled.
      !>
      !>     5)  If DT is so large that there is a danger of floating point
      !>         overflow during computation, the error SPICE(DTOUTOFRANGE) is
      !>         signaled and a message is generated describing the problem.
      !>         The value of DT must be "reasonable". In other words, DT
      !>         should be less than 10**20 seconds for realistic solar system
      !>         orbits specified in the MKS system. (The actual bounds on DT
      !>         are much greater but require substantial computation.) The
      !>         "reasonableness" of DT is checked at run-time.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine uses a universal variables formulation for the
      !>     two-body motion of an object in orbit about a central mass. It
      !>     propagates an initial state to an epoch offset from the
      !>     epoch of the initial state by time DT.
      !>
      !>     This routine does not suffer from the finite precision
      !>     problems of the machine that are inherent to classical
      !>     formulations based on the solutions to Kepler's equation:
      !>
      !>           n( t - T ) = E - e Sin(E)         elliptic case
      !>           n( t - T ) = e sinh(F) - F        hyperbolic case
      !>
      !>     The derivation used to determine the propagated state is a
      !>     slight variation of the derivation in Danby's book
      !>     "Fundamentals of Celestial Mechanics" [1].
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) When the eccentricity of an orbit is near 1, and the epoch
      !>        of classical elements is near the epoch of periapse, classical
      !>        formulations that propagate a state from elements tend to
      !>        lack robustness due to the finite precision of floating point
      !>        machines. In those situations it is better to use a universal
      !>        variables formulation to propagate the state.
      !>
      !>        By using this routine, you need not go from a state to
      !>        elements and back to a state. Instead, you can get the state
      !>        from an initial state.
      !>
      !>        If PVINIT is your initial state and you want the state 3600
      !>        seconds later, the following call will suffice.
      !>
      !>           Look up GM somewhere
      !>
      !>           DT = 3600.0D0
      !>
      !>           CALL PROP2B ( GM, PVINIT, DT, PVPROP )
      !>
      !>        After the call, PVPROP will contain the state of the
      !>        object 3600 seconds after the time it had state PVINIT.
      !>
      !>     2) Use the two-body force model to propagate the state of a
      !>        massless body orbiting the Earth at 100,000,000 km after half
      !>        a period.
      !>
      !>        In circular two-body motion, the orbital speed is
      !>
      !>           s     = sqrt(mu/r)
      !>
      !>        where mu is the central mass. After tau/2 = pi*r/s seconds
      !>        (half period), the state should equal the negative of the
      !>        original state.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM PROP2B_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      DOUBLE PRECISION      PI
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      MU
      !>      DOUBLE PRECISION      PVINIT ( 6 )
      !>      DOUBLE PRECISION      R
      !>      DOUBLE PRECISION      SPEED
      !>      DOUBLE PRECISION      STATE  ( 6 )
      !>      DOUBLE PRECISION      T
      !>
      !>C
      !>C     Initial values.
      !>C
      !>      MU    =  3.9860043543609598D+05
      !>      R     =  1.0D+08
      !>      SPEED =  SQRT( MU / R )
      !>      T     =  PI( )*R/SPEED
      !>
      !>      PVINIT(1) =  0.0D0
      !>      PVINIT(2) =  R/SQRT(2.0D0)
      !>      PVINIT(3) =  R/SQRT(2.0D0)
      !>      PVINIT(4) =  0.0D0
      !>      PVINIT(5) = -SPEED/SQRT(2.0D0)
      !>      PVINIT(6) =  SPEED/SQRT(2.0D0)
      !>
      !>C
      !>C     Calculate the state of the body at 0.5 period
      !>C     after the epoch.
      !>C
      !>      CALL PROP2B ( MU, PVINIT, T, STATE )
      !>
      !>C
      !>C     The `state' vector should equal -pvinit
      !>C
      !>      WRITE(*,*) 'State at t0:'
      !>      WRITE(*,'(A,3F17.5)') '   R   (km):',
      !>     .                 PVINIT(1), PVINIT(2), PVINIT(3)
      !>      WRITE(*,'(A,3F17.5)') '   V (km/s):',
      !>     .                 PVINIT(4), PVINIT(5), PVINIT(6)
      !>
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'State at tau/2:'
      !>      WRITE(*,'(A,3F17.5)') '   R   (km):',
      !>     .                    STATE(1), STATE(2), STATE(3)
      !>      WRITE(*,'(A,3F17.5)') '   V (km/s):',
      !>     .                    STATE(4), STATE(5), STATE(6)
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         State at t0:
      !>           R   (km):          0.00000   70710678.11865   70710678.11865
      !>           V (km/s):          0.00000         -0.04464          0.04464
      !>
      !>         State at tau/2:
      !>           R   (km):         -0.00000  -70710678.11865  -70710678.11865
      !>           V (km/s):          0.00000          0.04464         -0.04464
      !>
      !>## Restrictions
      !>
      !>     1)  Users should be sure that GM, PVINIT and DT are all in the
      !>         same system of units ( for example MKS ).
      !>
      !>## Literature References
      !>
      !>     [1]  J. Danby, "Fundamentals of Celestial Mechanics," 2nd Edition,
      !>          pp 168-180, Willman-Bell, 1988.
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
      !>-    SPICELIB Version 2.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 2.1.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VSCL call.
      !>
      !>-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF.
      !>
      !>-    SPICELIB Version 2.0.0, 16-MAY-1995 (WLT)
      !>
      !>        The initial guess at a solution to Kepler's equation was
      !>        modified slightly and a loop counter was added to the
      !>        bisection loop together with logic that will force termination
      !>        of the bisection loop.
      !>
      !>-    SPICELIB Version 1.0.0, 10-MAR-1992 (WLT)
      module subroutine prop2b ( ctx, gm, pvinit, dt, pvprop )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax
      use pepper_math, only: vcrss, vdot, vnorm
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: gm  !! Gravity of the central mass.
      real(wp), intent(in) :: pvinit ( 6 )  !! Initial state from which to propagate a state.
      real(wp), intent(in) :: dt  !! Time offset from initial state to propagate to.
      real(wp), intent(out) :: pvprop ( 6 )  !! The propagated state.
      integer(ip), parameter :: bufsiz = 3
      integer(ip), parameter :: maxbit = 64
      real(wp) :: pos    ( 3 )
      real(wp) :: kfun
      real(wp) :: f
      real(wp) :: bq
      integer(ip) :: lcount
      real(wp) :: logmxc
      real(wp) :: maxc
      real(wp) :: c0
      integer(ip) :: k
      real(wp) :: b2rv
      real(wp) :: fixed
      logical :: new
      real(wp) :: pc
      real(wp) :: logdpm
      real(wp) :: x3
      integer(ip) :: bumped
      real(wp) :: hvec   ( 3 )
      real(wp) :: fx2
      real(wp) :: pcdot
      real(wp) :: tmpvec ( 3 )
      real(wp) :: x2
      real(wp) :: qovr0
      real(wp) :: h2
      real(wp) :: b
      real(wp) :: oldx
      real(wp) :: x
      real(wp) :: kfunl
      real(wp) :: vc
      real(wp) :: r0
      real(wp) :: rv
      real(wp) :: e
      real(wp) :: logbnd
      real(wp) :: logf
      real(wp) :: rootf
      real(wp) :: c1
      real(wp) :: br
      real(wp) :: eqvec  ( 3 )
      real(wp) :: kfunu
      integer(ip) :: mostc
      integer(ip) :: i
      real(wp) :: c3
      real(wp) :: c2
      real(wp) :: q
      real(wp) :: br0
      real(wp) :: bound
      real(wp) :: vcdot
      if ( .not. allocated ( ctx%prop2b_state ) ) allocate ( ctx%prop2b_state )
      associate ( lower => ctx%prop2b_state%lower, &
                  upper => ctx%prop2b_state%upper, &
                  vel => ctx%prop2b_state%vel, &
                  savepv => ctx%prop2b_state%savepv, &
                  savegm => ctx%prop2b_state%savegm, &
                  sbound => ctx%prop2b_state%sbound, &
                  sb2rv => ctx%prop2b_state%sb2rv, &
                  sbq => ctx%prop2b_state%sbq, &
                  sbr0 => ctx%prop2b_state%sbr0, &
                  sf => ctx%prop2b_state%sf, &
                  sqovr0 => ctx%prop2b_state%sqovr0, &
                  newest => ctx%prop2b_state%newest, &
                  nsaved => ctx%prop2b_state%nsaved )
      if ( ctx%failed() ) return

!
!     Life will be easier if we use POS and VEL to hold the state.
!
      pos(1) = pvinit(1)
      pos(2) = pvinit(2)
      pos(3) = pvinit(3)

      vel(1) = pvinit(4)
      vel(2) = pvinit(5)
      vel(3) = pvinit(6)

!
!     If we propagate many states from the same initial state,
!     most of the variables used to propagate the state will
!     not change in value.
!
!     To save time needed to compute these variables, we recompute
!     variables that depend upon the initial state only when the
!     initial state is not one of those already buffered by this
!     routine.
!
!     Determine whether or not this GM and state are the same as the
!     one of those already buffered.  Note that we look through the
!     saved states and GM from the most recently input values of PVINIT
!     and GM to the oldest saved state and GM.
!
!     NEWEST(1)  contains the most recently input initial conditions
!     NEWEST(2)  contains the next most recently input initial
!                conditions etc.
!
!     Also note that when this routine starts up there will be no
!     buffered states or GMs.  Every time we encounter a new state, we
!     will increment the number of saved states NSAVED until we have
!     BUFSIZ states buffered.  From that point on, when a new state is
!     encountered we will overwrite the oldest buffered state.
!
      i   =  0
      new = .true.

      do while ( ( i < nsaved ) .and. new )

         i   = i + 1
         k   = newest(i)

         new =     ( pvinit(1) /= savepv(1,k) ) &
              .or. ( pvinit(2) /= savepv(2,k) ) &
              .or. ( pvinit(3) /= savepv(3,k) ) &
              .or. ( pvinit(4) /= savepv(4,k) ) &
              .or. ( pvinit(5) /= savepv(5,k) ) &
              .or. ( pvinit(6) /= savepv(6,k) ) &
              .or. ( gm        /= savegm(  k) )

      end do

      if ( .not. new ) then
!
!        We update the order vector NEWEST so that the state being
!        used this time becomes the "youngest" state.
!
         k      = i
         bumped = newest(k)

         do i = k, 2, -1
            newest(i) = newest(i-1)
         end do

         newest(1) = bumped
         k         = bumped

!
!        Now look up all of the other saved quantities.
!
         b2rv   = sb2rv (k)
         bound  = sbound(k)
         bq     = sbq   (k)
         br0    = sbr0  (k)
         f      = sf    (k)
         qovr0  = sqovr0(k)

      else

!
!        We have a new state, new GM or both.  First let's make sure
!        there is nothing obviously wrong with them.  (We buffer
!        only states, GMs and intermediate values that are "good.")
!        First check for nonpositive mass.
!
         if ( gm <= 0.0_wp ) then
            call ctx%sigerr ( 'SPICE(NONPOSITIVEMASS)'   )
            return
         end if

!
!        Next for a zero position vector
!
         if ( all(pos == 0.0_wp) ) then
            call ctx%sigerr ( 'SPICE(ZEROPOSITION)'   )
            return
         end if

!
!        Finally for a zero velocity vector
!
         if ( all(vel == 0.0_wp) ) then
            call ctx%sigerr ( 'SPICE(ZEROVELOCITY)'   )
            return
         end if

!
!        Obvious problems have been checked. Here are the relevant
!        equations. Let ...
!
!           GM        be the gravitational attraction of the central
!                     mass.
!
!           POS and   be the initial position and velocity respectively
!           VEL       of the orbiting object.
!
!           R0       be the magnitude of the position vector POS
!
!           RV       be the value of the dot product  POS * VEL
!
         r0 = vnorm ( pos      )
         rv = vdot  ( pos, vel )

!
!        Let HVEC be the specific angular momentum vector and let Q be
!        the distance at periapse.
!
!                   1)    HVEC  =   POS  x  VEL
!
!                                       2
!                   2)    H2    = |HVEC|  =  GM*(1+E)*Q
!
         call vcrss ( pos,  vel,     hvec )
         h2 = vdot  ( hvec, hvec          )

!
!        Let's make sure we are not in the pathological case of
!        rectilinear motion.
!
         if ( h2 == 0 ) then
            call ctx%sigerr ( 'SPICE(NONCONICMOTION)'   )
            return
         end if

!
!        Let E be the eccentricity of the orbit.
!
!        Let QVEC be the unit vector that points toward perihelion, and
!        let EQVEC be QVEC scaled by E.
!
!                                   VEL X HVEC      POS
!                    1)  E*QVEC  =  ----------  -   ---
!                                       GM           R0
!
!                                         VEL X HVEC      POS
!                    2)  E       = NORM ( ----------  -   --- )
!                                            GM            R0
!
         call vcrss ( vel,       hvec,                      tmpvec  )
         eqvec = (1.0_wp/gm)*tmpvec + (-1.0_wp/r0)*pos

         e  = vnorm ( eqvec )

!
!        Solve the equation H2 = GM*Q*(1+E) for Q.
!
         q  = h2 / ( gm * (1+e) )

!
!        From the discussion of the universal variables formulation in
!        Danby's book on pages 174 and 175 (see the reference listed
!        above) you can show that by making the substitutions
!
!              F  =  1 - E
!
!        and
!
!                       _____
!                      /  Q
!              S  =   / -----    X   = B * X
!                   \/   GM
!
!        that DT satisfies the universal variables Kepler's equation:
!
!                                   2     2     2        2
!              DT =  B*R0*X*C_1( F*X ) + B *RV*X C_2( F*X )
!
!                                               3        2
!                                      +   B*Q*X C_3( F*X )
!
!                 =  KFUN( X )
!
!        (where C_k is used to denote the Stumpff functions. This is
!        the universal variables formulation of Kepler's equation.
!        KFUN is our abbreviation for "Kepler function.")
!
!        (One might wonder, "Why make such a change of variables?"
!        By making this substitution early in the derivation supplied
!        in Danby's book, you can always deal with physically
!        meaningful quantities --- the pure numeric value of F and the
!        distance of periapse.  Thus one does not need to be concerned
!        about infinite or negative semi-major axes or with discussing
!        how to interpret these somewhat artificial artifacts of the
!        classical derivations for two body motion.)
!
!        Given the unique X for which this Kepler's equation is
!        satisfied, we can compute the state of the orbiting object
!        at a time DT past the epoch of the state POS and VEL.
!        Evidently we will need the constants:
!
         f      = 1.0_wp - e
         b      = dsqrt ( q / gm )
         br0    = b*r0
         b2rv   = b*b*rv
         bq     = b*q

!
!        The state corresponding to the value of X that solves this
!        equation is given by
!
!              PC * POS + VC * VEL              ( position )
!
!        and
!
!              PCDOT * POS + VCDOT * VEL        ( velocity )
!
!        where
!                                            2        2
!           ( 1 )    PC    =  1  -  ( Q/R0 )X C_2( F*X )
!
!                                            3        2
!           ( 2 )    VC    =  DT -  ( B*Q  )X C_3( F*X )
!
!                                       Q               2
!           ( 3 )    PCDOT =     -  ( ------ ) X C_1( F*X )
!                                     B*R*R0
!
!                                      B*Q     2        2
!           ( 4 )    VCDOT =  1  -  (  ---  ) X C_2( F*X )
!                                      B*R
!
!        Here R denotes the distance from the center of CP*POS + CV*VEL
!        It turns out that R can be computed as:
!
!                                        2     2             2
!           ( 5 )   B*R    = B*R0 C_0(F*X ) + B *RV X C_1(F*X )
!
!                                                 2       2
!                                        +   B*Q X C_2(F*X )
!
!        Therefore we will also need the constant
!
         qovr0   = q / r0

!
!        We will have to find the unique value of X such that
!
!             DT = KFUN ( X )
!
!        where KFUN stands for the "Kepler function" defined by the
!        equation below:
!
!                                   2
!        KFUN(X) =   B*R0*X * C_1(FX )
!
!                   2     2        2
!                + B *RV*X * C_2(FX )
!
!                         3        2
!                +   B*Q*X * C_3(FX )
!
!        (There is a unique solution to this equation. KFUN(X) is
!        unbounded above and below and is an increasing function
!        over all real X for all non-rectilinear orbits. To see this
!        we note that the variable X is a function of DT and is given
!        by the integral from 0 to DT of the differential:
!
!                   dt
!                 ------
!                 B*R(t)
!
!        where R(t) is the range of the body as a function of time.
!        Therefore X is an increasing function of DT, and DT must
!        also be an increasing function of X.
!
!        Thus, there is a unique value of X  that solves this
!        equation).
!
!        If F is less than zero, we can have the computation of C0,...
!        overflow.  This is because for X < 0
!
!               C_0(X) = COSH( DSQRT(-X) )
!
!               C_1(X) = SINH( DSQRT(-X) )
!                        -----------------
!                              DSQRT(-X)
!
!        and from the recursion relationship we know that
!
!               C_2(X) =  ( 1/0! - C_0(X) ) / X
!
!               C_3(X) =  ( 1/1! - C_1(X) ) / X
!
!                         1 - COSH( DSQRT(-X) )
!               C_2(X) = ------------------------
!                                  X
!
!                         1  - SINH( DSQRT(-X) ) / DSQRT(-X)
!               C_3(X) = -----------------------------------
!                                    X
!
!        Clearly for negative values of F*X*X having large magnitude,
!        it is easy to get an overflow.
!
!        In the case when F is less than 0 we choose X so that we can
!        compute all of the following:
!
!               | COEF_0 * X**0 * C_0(FX**2) |
!
!               | COEF_1 * X**1 * C_1(FX**2) |
!
!               | COEF_2 * X**2 * C_2(FX**2) |
!
!               | COEF_3 * X**3 * C_3(FX**2) |
!
!         where COEF_n are coefficients that will be used in forming
!         linear combinations of X**n C_n(FX**2) terms.
!
!         The variable portion of the last 3 terms above can be
!         rewritten as:
!
!                                   SINH ( DSQRT(-F)*|X| )
!        | X**1 * C_1(FX**2) |  =   ----------------------
!                                          DSQRT(-F)
!
!                                   1 - COSH( DSQRT(-F)*|X| )
!        | X**2 * C_2(FX**2) |  =  ----------------------------
!                                             -F
!
!                                  DSQRT(-F)*|X|   - SINH(DSQRT(-F)*|X|)
!        | X**3 * C_3(FX**2) |  =  -------------------------------------
!                                              F*DSQRT(-F)
!
!        For large |X| the absolute values of these expressions are well
!        approximated by
!
!                                         0.0
!               COSH( DSQRT(-F)|X| ) * |F|
!
!                                         -0.5
!               SINH( DSQRT(-F)|X| ) * |F|
!
!                                         -1.0
!               COSH( DSQRT(-F)|X| ) * |F|
!
!                                         -1.5
!               SINH( DSQRT(-F)|X| ) * |F|
!
!        For large |X| the logarithms of these expressions are well
!        approximated by:
!
!               DSQRT(-F)|X| - LOG(2) - 0.0*LOG(-F)
!
!               DSQRT(-F)|X| - LOG(2) - 0.5*LOG(-F)
!
!               DSQRT(-F)|X| - LOG(2) - 1.0*LOG(-F)
!
!               DSQRT(-F)|X| - LOG(2) - 1.5*LOG(-F)
!
!        respectively.
!
!        To ensure that we can form a linear combination of these terms
!        we will require that:
!
!           |COEF_N*X**N * C_N(FX**2)| < DPMAX / 4
!
!        for N=0,1,2,3.  This is equivalent to
!
!              LOG ( X**N * C_N(FX**2) )   <      LOG ( DPMAX )
!            + LOG (|COEF_N|)                   - 2 LOG ( 2     )
!
!        or
!
!              LOG ( X**N * C_N(FX**2) )   <      LOG ( DPMAX    )
!                                             -   LOG ( |COEF_N| )
!                                             - 2*LOG ( 2        ).
!
!        Replacing the left hand side with the magnitude expressions
!        computed above we have:
!
!            DSQRT(-F)|X| - LOG(2) - N*0.5*LOG( -F )  <   LOG ( DPMAX  )
!                                                      -  LOG (|COEF_N|)
!                                                      -2*LOG ( 2      )
!
!         So that:
!
!            |X|  <    {   LOG ( DPMAX  )
!                        - LOG (|COEF_N|)
!                        - LOG (  2     )
!                        + LOG ( -F     )*N*0.5 } / DSQRT(-F)
!
!         Let MAXC be the maximum of 1.0D0 and the various coefficients
!         of the Stumpff functions.  We can then set our absolute value
!         bound on X to be:
!
!             MIN        LOG(DPMAX/2) - LOG(MAXC) + (n/2)LOG(-F)
!            n = 0,3  {  -----------------------------------------  }
!                               DSQRT(-F)
!
!        (Actually we know that the minimum must occur for n = 0 or
!        for n = 3).
!
         maxc   = max   ( 1.0_wp, dabs(br0), &
                                 dabs(b2rv), &
                                 dabs(bq), &
                                 dabs(qovr0/bq) )

         if ( f < 0 ) then

            logmxc = log   ( maxc            )
            logdpm = log   ( dpmax / 2.0_wp )

            fixed  = logdpm  - logmxc

            rootf  = dsqrt ( - f             )
            logf   = log   ( - f             )

            bound  = min (  (fixed              )/rootf, &
                            (fixed + 1.5_wp*logf )/rootf )

!
!           Note that in the above, we can always perform the division
!           by ROOTF.  To see this we note that -F is at least the
!           machine precision (we got it by subtracting E from 1.)
!           Thus its square root is a reasonably large number (if F is
!           10**-N then ROOTF is 10**(-N/2) )  The value of FIXED is
!           about 3*M where M is the largest exponent such that 2**M
!           is representable on the host machine.  Thus BOUND is at
!           worst M*10**(N/2)  This will always be computable.
!
        else

!
!           In the case when F is non-negative we must be sure we
!           can compute all of the following.
!
!               | COEF_0 * X**0 * C_0(FX**2) | < | COEF_0          |
!
!               | COEF_1 * X**1 * C_1(FX**2) | < | COEF_1*|X|      |
!
!               | COEF_2 * X**2 * C_2(FX**2) | < | COEF_2*X**2 / 2 |
!
!               | COEF_3 * X**3 * C_3(FX**2) | < | COEF_3*X**3 / 6 |
!
!           If we assume that COEF_0 is computable, all of these are
!           bounded above by:
!
!                       | MAX(COEF_1,...COEF_3) * X**3 / 6 |
!
!           We want to make sure we can add these terms so we need to
!           make sure that
!
!              | MAX(COEF_1,...,COEF_3) * X**3 / 6 | < DPMAX() / 4.
!
!           Thus we need:
!
!              |X**3| <          1.5*DPMAX / MAX(COEF_1,...,COEF_3)
!              |X|    <  DCBRT ( 1.5*DPMAX / MAX(COEF_1,...,COEF_3) )
!
!           (We'll use logarithms to compute the upper bound for |X|.)
!
            logbnd =      ( dlog(1.5_wp) + dlog(dpmax) - dlog(maxc) ) &
                     /      3.0_wp

            bound  = dexp ( logbnd )

         end if
!
!        All the obvious problems have been checked, move everybody
!        on the list down and put the new guy on top of the list.
!

         nsaved = max(min(1, bufsiz), min(max(1, bufsiz), nsaved+1))
         bumped = newest ( nsaved              )

         do i = nsaved, 2, -1
            newest(i) = newest(i-1)
         end do

         newest(1) = bumped
         k         = bumped

         savepv(1,k) = pvinit(1)
         savepv(2,k) = pvinit(2)
         savepv(3,k) = pvinit(3)
         savepv(4,k) = pvinit(4)
         savepv(5,k) = pvinit(5)
         savepv(6,k) = pvinit(6)
         savegm(  k) = gm

!
!        Finally we save the results of all of the above
!        computations so that we won't have to do them again,
!        if this initial state and GM are entered again.
!
         sb2rv (k) = b2rv
         sbound(k) = bound
         sbq   (k) = bq
         sbr0  (k) = br0
         sf    (k) = f
         sqovr0(k) = qovr0

      end if
!
!     We are now ready to find the unique value of X such that
!
!             DT = KFUN ( X )
!
!     First we must bracket the root. The basic idea is this:
!
!     1) KFUN(0) = 0 so we will let one endpoint of our initial
!        guess of a bracketing interval be 0.
!
!     2) We get our initial guess at the other endpoint of the
!        bracketing interval by recalling that
!
!                   dt
!         dX  =   ------
!                 B*R(t)
!
!        From this observation it follows that
!
!                   DT
!          X  <  -------
!                   B*Q
!
!        Thus the solution to
!
!             DT = KFUN ( X )
!
!        Satisfies
!
!                     DT
!         0 < X  <  -------
!                    B*Q
!
!        We now have a guess at a bracketing interval. In the case
!        DT is positive it looks like
!
!                0        X
!         -------[--------]-----------------------------
!
!        This is ok mathematically, but due to rounding etc it is
!        conceivable that we might not have bracketed the root.
!        We check and if not we will double the
!        endpoint farthest from zero and call this X, and make
!        the other endpoint the old value of X.
!
!                0
!         -------+--------[--------]--------------------
!
!        We continue this process ...
!
!                0
!         -------+-----------------[-----------------]--
!
!        ...until the root is bracketed. (One shift is certain
!        to do the job).
!
!        If we perform this interval shift, we will have to take
!        care that X does not run out of the domain for which
!        we can safely compute KFUN.  Thus we will make sure that
!        the endpoints of these shifted intervals always stay safely
!        inside the domain for which KFUN can be computed.
!
      x    = dt / bq
      x    = max(min(-bound, bound), min(max(-bound, bound), x))
      fx2  = f*x*x

      call stmp03 ( ctx, fx2, c0, c1, c2, c3 )

      kfun =   x*(br0*c1 + x*( b2rv*c2 + x*(bq*c3) ) )

      if ( dt < 0 ) then

         upper = 0
         lower = x

         do while ( kfun > dt )

            upper = lower
            lower = lower * 2.0_wp
            oldx  = x
            x     = max(min(-bound, bound), min(max(-bound, bound), lower))

!
!           Make sure we are making progress. (In other words make sure
!           we don't run into the boundary of values that X can assume.
!           If we do run into the boundary, X will be unchanged and
!           there's nothing further we can do.  We'll have to call it
!           quits and tell the user what happened.)
!
            if ( x == oldx ) then

               fx2 = f*bound*bound

               call stmp03 ( ctx, fx2, c0, c1, c2, c3 )

               kfunl = -bound*(br0*c1 - bound*(b2rv*c2 - bound*bq*c3))
               kfunu =  bound*(br0*c1 + bound*(b2rv*c2 + bound*bq*c3))

               call ctx%setmsg ( 'The input delta time (DT) has a '       // &
                             'value of #.  This is beyond the '       // &
                             'range of DT for which we can '          // &
                             'reliably propagate states. The limits ' // &
                             'for this GM and initial state are '     // &
                             'from # to #. '                          )
               call ctx%errdp  ( '#', dt                                  )
               call ctx%errdp  ( '#', kfunl                               )
               call ctx%errdp  ( '#', kfunu                               )
               call ctx%sigerr ( 'SPICE(DTOUTOFRANGE)'                    )
               return
            end if

            fx2   = f*x*x

            call stmp03 ( ctx, fx2, c0, c1, c2, c3 )

            kfun = x*(br0*c1 + x*( b2rv*c2 + x*(bq*c3) ) )

         end do

      else if ( dt > 0 ) then

         lower = 0
         upper = x

         do while ( kfun < dt )

            lower = upper
            upper = upper * 2.0_wp
            oldx  = x
            x     = max(min(-bound, bound), min(max(-bound, bound), upper))
!
!           Make sure we are making progress.
!
            if ( x == oldx ) then

               fx2 = f*bound*bound

               call stmp03 ( ctx, fx2, c0, c1, c2, c3 )

               kfunl = -bound*(br0*c1 - bound*(b2rv*c2 - bound*bq*c3))
               kfunu =  bound*(br0*c1 + bound*(b2rv*c2 + bound*bq*c3))

               call ctx%setmsg ( 'The input delta time (DT) has a '       // &
                             'value of #.  This is beyond the '       // &
                             'range of DT for which we can '          // &
                             'reliably propagate states. The limits ' // &
                             'for this GM and initial state are '     // &
                             'from # to #. '                          )
               call ctx%errdp  ( '#', dt                                  )
               call ctx%errdp  ( '#', kfunl                               )
               call ctx%errdp  ( '#', kfunu                               )
               call ctx%sigerr ( 'SPICE(DTOUTOFRANGE)'                    )
               return
            end if

            fx2  = f*x*x

            call stmp03 ( ctx, fx2, c0, c1, c2, c3 )

            kfun = x*(br0*c1 + x*( b2rv*c2 + x*bq*c3 ) )

         end do

      else

         pvprop = pvinit
         return

      end if

!
!     Ok. We've bracketed the root.  Now for lack of anything more
!     clever, we just bisect to find the solution.
!
!     We add a loop counter so that we can ensure termination of the
!     loop below.
!
!     On some systems the computed midpoint is stored in an extended
!     precision register.  Thus the midpoint is always different from
!     UPPER and LOWER.  Yet when the new value of LOWER and UPPER
!     are assigned UPPER and LOWER do not change and hence the
!     loop fails to terminate.  With the loop counter we force
!     termination of the loop.
!
      x    = min ( upper, max ( lower, (lower + upper)/2.0_wp) )
      fx2  = f*x*x

      call stmp03 ( ctx, fx2, c0, c1, c2, c3 )
      lcount = 0
      mostc  = 1000

      do while (         x      > lower &
                 .and.   x      < upper &
                 .and.   lcount < mostc )

         kfun = x*(br0*c1 + x*( b2rv*c2 + x*bq*c3 ) )

         if ( kfun > dt ) then
            upper = x
         else if ( kfun < dt ) then
            lower = x
         else
            upper = x
            lower = x
         end if
!
!        As soon as the bracketing values move away from
!        zero we can modify the count limit.
!
         if ( mostc > maxbit ) then

            if ( upper /= 0.0_wp .and. lower /= 0.0_wp ) then
               mostc   = maxbit
               lcount = 0
            end if

         end if

         x    = min ( upper, max ( lower, (lower + upper)/2.0_wp ) )
         fx2  = f*x*x

         call stmp03 ( ctx, fx2, c0, c1, c2, c3 )

         lcount = lcount + 1

      end do

!
!     With X in hand we simply compute BR, PC, VC, PCDOT and VCDOT
!     described in equations (1) --- (5) above. (Note, by our choice
!     of BOUND above, one can show that none of the computations
!     below can cause an overflow).
!
      x2    = x *x
      x3    = x2*x
      br    = br0*c0 + x*( b2rv*c1  + x*(bq*c2) )

      pc    = 1.0_wp     -      qovr0        * x2 * c2
      vc    = dt        -      bq           * x3 * c3
      pcdot =           -     (qovr0 / br ) * x  * c1
      vcdot = 1.0_wp     -     (bq    / br ) * x2 * c2

!
!     ... and compute the linear combinations needed to get PVPROP
!
      pvprop(1:3) = pc*pos + vc*vel
      pvprop(4:6) = pcdot*pos + vcdot*vel

      return
      end associate
      end subroutine prop2b

      !> Fetch from the constants partition of a generic segment the
      !> double precision numbers from FIRST to LAST. The segment is
      !> identified by a DAF file handle and segment descriptor.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF opened for reading that
      !>              contains the segment described by DESCR.
      !>
      !>     DESCR    is the descriptor of the segment with the desired
      !>              constant values. This must be the descriptor for a
      !>              generic segment in the DAF associated with HANDLE.
      !>
      !>     FIRST    is the index of the first value to fetch from the
      !>              constants section of the generic segment associated
      !>              with HANDLE and DESCR.
      !>
      !>     LAST     is the index of the last value to fetch from the
      !>              constants section of the generic segment associated
      !>              with HANDLE and DESCR.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is the array of constant values obtained from the
      !>              constants section of the generic segment associated
      !>              with HANDLE and DESCR.
      !>
      !>## Parameters
      !>
      !>     This subroutine makes use of parameters defined in the file
      !>     'sgparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If FIRST is less than 1 or LAST is greater than the number of
      !>         constants, the error SPICE(REQUESTOUTOFBOUNDS) is signaled.
      !>
      !>     2)  If LAST is less than FIRST, the error SPICE(REQUESTOUTOFORDER)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE above.
      !>
      !>## Particulars
      !>
      !>     This routine allows easy access to values from the constants
      !>     partition of a generic segment in a DAF file. Please see the DAF
      !>     Required Reading or the include file 'sgparam.inc' for a more
      !>     detailed description of a generic segment.
      !>
      !>## Examples
      !>
      !>     Suppose that you have located a DAF generic segment. The
      !>     fragment of code below shows how to fetch all of the
      !>     constants from that segment.
      !>
      !>        Declarations:
      !>
      !>        DOUBLE PRECISION      CONSTS(<enough room to hold constants>)
      !>
      !>        INTEGER               MYNCON
      !>
      !>        Get the number of items in the constants section.
      !>
      !>        CALL SGMETA ( HANDLE, DESCR, NCON, MYNCON )
      !>
      !>        Fetch the constants from the segment.
      !>
      !>        CALL SGFCON ( HANDLE, DESCR, 1, MYNCON, CONSTS )
      !>
      !>## Restrictions
      !>
      !>     1)  The segment described by DESCR must be a generic segment,
      !>         otherwise the results of this routine are not predictable.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>
      !>-    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT)
      module subroutine sgfcon ( ctx, handle, descr, first, last, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF open for reading.
      real(wp), intent(in) :: descr  ( * )  !! Descriptor for a generic segment in the DAF.
      integer(ip), intent(in) :: first  !! The index of the first constant value to fetch.
      integer(ip), intent(in) :: last  !! The index of the last constant value to fetch.
      real(wp), intent(out) :: values ( * )  !! The constant values that were requested.

      integer(ip) :: b
      integer(ip) :: base
      integer(ip) :: e
      integer(ip) :: myncon
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the value for the base of the constants and the number of
!     constants in the generic segment.
!
      call sgmeta ( ctx, handle, descr, conbas, base   )
      call sgmeta ( ctx, handle, descr, ncon,   myncon )

      if ( ctx%failed() ) return

!
!     Perform checks on the inputs for reasonableness.
!
      if ( ( first < 1 ) .or. ( last > myncon ) ) then

         call ctx%setmsg ( 'The range of constants requested' &
         //            ' extends beyond the available constant' &
         //            ' data.  Constants are available for' &
         //            ' indices 1 to #.  You have requested' &
         //            ' data from # to #. '                     )
         call ctx%errint ( '#', myncon                               )
         call ctx%errint ( '#', first                                )
         call ctx%errint ( '#', last                                 )
         call ctx%sigerr ( 'SPICE(REQUESTOUTOFBOUNDS)'               )
         return

      end if

      if ( last < first ) then

         call ctx%setmsg ( 'The last constant item requested, #, is' &
         //            ' before the first constant item' &
         //            ' requested, #.'                            )
         call ctx%errint ( '#', last                                   )
         call ctx%errint ( '#', first                                  )
         call ctx%sigerr ( 'SPICE(REQUESTOUTOFORDER)'                  )
         return

      end if

!
!     Compute the addresses of the data within the file and then fetch
!     the data.
!
      b = base + first
      e = base + last

      call dafgda ( ctx, handle, b, e, values )

      return

      end subroutine sgfcon

      !> Fetch the data packets indexed from FIRST to LAST from the
      !> packet partition of a generic segment. The segment is
      !> identified by a DAF file handle and segment descriptor.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF opened for reading that
      !>              contains the segment described by DESCR.
      !>
      !>     DESCR    is the descriptor of the segment with the desired
      !>              constant values. This must be the descriptor for a
      !>              generic segment in the DAF associated with HANDLE.
      !>
      !>     FIRST    is the index of the first value to fetch from the
      !>              constants section of the DAF segment described
      !>              by DESCR.
      !>
      !>     LAST     is the index of the last value to fetch from the
      !>              constants section of the DAF segment described
      !>              by DESCR
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is the array of values constructed by concatenating
      !>              requested packets one after the other into
      !>              an array. Pictorially we can represent VALUES
      !>              as:
      !>
      !>                 +--------------------------+
      !>                 | first requested packet   |
      !>                 +--------------------------+
      !>                 | second requested packet  |
      !>                 +--------------------------+
      !>                            .
      !>                            .
      !>                            .
      !>                 +--------------------------+
      !>                 | first requested packet   |
      !>                 +--------------------------+
      !>
      !>     ENDS     is an array of pointers to the ends of the
      !>              fetched packets.  ENDS(1) gives the index
      !>              of the last item of the first packet fetched.
      !>              ENDS(2) gives the index of the last item of
      !>              the second packet fetched, etc.
      !>
      !>## Parameters
      !>
      !>     This subroutine makes use of parameters defined in the file
      !>     'sgparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If FIRST is less than 1 or LAST is greater than the number of
      !>         packets, the error SPICE(REQUESTOUTOFBOUNDS) is signaled.
      !>
      !>     2)  If LAST is less than FIRST, the error SPICE(REQUESTOUTOFORDER)
      !>         is signaled.
      !>
      !>     3)  If the packet directory structure is unrecognized, the error
      !>         SPICE(UNKNOWNPACKETDIR) is signaled. The most likely cause of
      !>         this error is that an upgrade to your version of the SPICE
      !>         toolkit is needed.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE above.
      !>
      !>## Particulars
      !>
      !>     This routine fetches requested packets from a generic
      !>     DAF segment. The two arrays returned have the following
      !>     relationship to one another. The first packet returned
      !>     resides in VALUES between indexes 1 and ENDS(1).  If a
      !>     second packet is returned it resides in VALUES between
      !>     indices ENDS(1)+1 and ENDS(2).  This relations ship is
      !>     repeated so that if I is greater than 1 and at least I
      !>     packets were returned then the I'th packet resides in
      !>     VALUES between index ENDS(I-1) + 1 and ENDS(I).
      !>
      !>## Examples
      !>
      !>     Suppose that you have located a generic DAF segment (as
      !>     identified by the contents of a segment descriptor).  The
      !>     fragment of code below shows how you could fetch packets
      !>     3 through 7 (assuming that many packets are present).
      !>     from the segment.
      !>
      !>        Declarations:
      !>
      !>        DOUBLE PRECISION   MYPKSZ (<enough room to hold all packets>)
      !>
      !>        INTEGER               ENDS  ( 5 )
      !>        INTEGER               MYNPKT
      !>
      !>        get the number of packets
      !>
      !>        CALL SGMETA ( HANDLE, DESCR, NPKT, MYNPKT )
      !>
      !>        finally, fetch the packets from the segment.
      !>
      !>        IF ( 7 .LE. MYNPKT ) THEN
      !>           CALL SGFPKT ( HANDLE, DESCR, 3, 7,  MYPKSZ, ENDS )
      !>        END IF
      !>
      !>## Restrictions
      !>
      !>     1)  The segment described by DESCR must be a generic segment,
      !>         otherwise the results of this routine are not predictable.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA calls with DAFGDA.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUL-1996 (KRG) (NJB)
      !>
      !>        Found and fixed a bug in the calculation of the beginning
      !>        address for variable length packet fetching. The base address
      !>        for the packet directory was not added into the value. This
      !>        bug went unnoticed because of a bug in SGSEQW, entry SGWES,
      !>        that put absolute addresses into the packet directory rather
      !>        than addresses that were relative to the start of the DAF
      !>        array. The bug in SGSEQW has also been fixed.
      !>
      !>-    SPICELIB Version 1.0.0, 06-JAN-1994 (KRG) (WLT)
      module subroutine sgfpkt ( ctx, handle, descr, first, last, values, ends )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! The file handle attached to an open DAF.
      real(wp), intent(in) :: descr   ( * )  !! The descriptor associated with a generic segment.
      integer(ip), intent(in) :: first  !! The index of the first data packet to fetch.
      integer(ip), intent(in) :: last  !! The index of the last data packet to fetch.
      real(wp), intent(out) :: values  ( * )  !! The data packets that have been fetched.
      integer(ip), intent(out) :: ends    ( * )  !! An array of pointers to the ends of the packets.

      real(wp) :: dtemp(2)

      integer(ip) :: b
      integer(ip) :: begin1
      integer(ip) :: begin2
      integer(ip) :: e
      integer(ip) :: i
      integer(ip) :: mynpdr
      integer(ip) :: mynpkt
      integer(ip) :: mypdrb
      integer(ip) :: mypdrt
      integer(ip) :: mypksz
      integer(ip) :: mypktb
      integer(ip) :: mypkto
      integer(ip) :: size
      integer(ip) :: soffst
      integer(ip) :: voffst
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform the needed initialization
!
      call sgmeta ( ctx, handle, descr, npkt,   mynpkt )
      call sgmeta ( ctx, handle, descr, pdrtyp, mypdrt )
      call sgmeta ( ctx, handle, descr, pktoff, mypkto )
      call sgmeta ( ctx, handle, descr, pktsz,  mypksz )
      call sgmeta ( ctx, handle, descr, pktbas, mypktb )

      if ( ctx%failed() ) return

!
!     Perform checks on the inputs for reasonableness.
!
      if ( ( first < 1 ) .or. ( last > mynpkt ) ) then

         call ctx%setmsg ( 'The range of packets requested ' &
         //            'extends beyond the available packet ' &
         //            'data.  The packet data is available ' &
         //            'for indexes 1 to #.  You''ve requested ' &
         //            'data from # to #. '                       )
         call ctx%errint ( '#', mynpkt                                )
         call ctx%errint ( '#', first                                 )
         call ctx%errint ( '#', last                                  )
         call ctx%sigerr ( 'SPICE(REQUESTOUTOFBOUNDS)'                )
         return

      end if

      if ( last < first ) then

         call ctx%setmsg ( 'The last packet requested, #, is ' &
         //            'before the first packet ' &
         //            'requested, #. '                     )
         call ctx%errint ( '#', last                            )
         call ctx%errint ( '#', first                           )
         call ctx%sigerr ( 'SPICE(REQUESTOUTOFORDER)'           )
         return

      end if
!
!     We've passed the sanity tests, if the packet directory structure
!     is recognized fetch the values and ends. We assume that we are
!     reading data from a correctly constructed generic segment, so we
!     do not need to worry about the type of reference index, as this is
!     not needed to fetch a data packet.
!     Currently, only two packet directory types are supported, and this
!     subroutine is the only place that this is documented. The types
!     have values zero (0) and one (1) for, respectively, fixed size
!     packets and variable size packets.
!
      if ( mypdrt == 0 ) then
!
!        All packets have the same size MYPKSZ so the address of the
!        start of the first packet and end of the last packet are easily
!        computed.
!
         if ( mypkto == 0 ) then
!
!           Compute tha addresses for the packet data in the generic
!           segment.
!
            b    =  mypktb   + ( first - 1 ) * mypksz + 1
            e    =  mypktb   +  last         * mypksz
!
!           Get the packet data all in one shot since we know it's
!           contiguous.
!
            call dafgda ( ctx, handle, b, e, values )

         else
!
!           Compute the addresses for the packet data in the generic
!           segment. Remember that we need to account for an offset
!           here to get to the start of the actual data packet.
!
            size = mypksz + mypkto
!
!           Get the packet data. Because there is an offset from the
!           address to the start of the packet data, we need to get
!           the data one packet at a time rather than all at once.
!
            do i = first, last

               soffst = ( i -     1 ) * size   + 1
               voffst = ( i - first ) * mypksz + 1

               b =  mypktb + soffst + mypkto
               e =  mypktb + soffst + mypksz

               call dafgda ( ctx, handle, b, e, values(voffst) )

               if ( ctx%failed() ) return

            end do

         end if
!
!        Compute the ends for each of the data packets. This is the
!        same for both of the cases above because we have fixed size
!        data packets.
!
         do i = 1, last-first+1
            ends(i) = i * mypksz
         end do

      else
!
!        In addition to the other meta data items already retrieved, we
!        will also need a few others.
!
         call sgmeta ( ctx, handle, descr, pdrbas, mypdrb )
         call sgmeta ( ctx, handle, descr, npdr,   mynpdr )

         if ( ctx%failed() ) return
!
!        Each packet has a different size, so we need to fetch each one
!        individually, keeping track of the ends and things. We assume
!        that there is enough room in the array of values to hold all of
!        the packets. For the variable packet case, however, we do not
!        need to treat the implicit indexing and explicit indexing cases
!        separately.
!
         voffst = 1

         do i = 1, last-first+1
!
!           Compute the addresses in the generic segment for the
!           beginning of data packets I and I+1. We need these to
!           compute the size of the packet.
!
            b = mypdrb + first + i - 1
            e = b + 1
!
!           Get the beginning addresses for the two data packets and
!           convert them into integers.
!
            call dafgda( ctx, handle, b, e, dtemp )

            if ( ctx%failed() ) return

            begin1 = int ( dtemp(1) )
            begin2 = int ( dtemp(2) )
!
!           Compute the size of data packet I, remembering to deal with
!           the packet offset that might be present, and the beginning
!           and ending addresses for the packet data.
!
            size = begin2 - begin1 - mypkto
            b    = mypktb + begin1
            e    = b      + size - 1
!
!           Get the data for packet I.
!
            call dafgda ( ctx, handle, b, e, values(voffst) )

            if ( ctx%failed() ) return
!
!           Compute the end for packet I and store it.
!
            voffst  = voffst + size
            ends(i) = voffst - 1

         end do

      end if

      return

      end subroutine sgfpkt

      !> Find the reference value associated with the value X and its
      !> index in a generic segment. The segment is identified by a DAF
      !> file handle and segment descriptor.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF open for reading
      !>
      !>     DESCR    is the descriptor of the generic segment that we are
      !>              going to search for a reference value to associate with
      !>              X.
      !>
      !>     X        is a value for which the associated reference value
      !>              and reference index is requested.
      !>
      !>## Detailed Output
      !>
      !>     VALUE    is the reference value associated with the input value
      !>              X.
      !>
      !>     INDX     is the index of VALUE within the set of reference
      !>              values for the generic segment. This value may be used
      !>              to obtain a particular packet of data from the generic
      !>              segment.
      !>
      !>     FOUND    is a logical flag indicating whether a reference value
      !>              associated with X was found. If a reference value was
      !>              found, FOUND will have a value of .TRUE.; otherwise it
      !>              will have a value of .FALSE.
      !>
      !>## Parameters
      !>
      !>     This subroutine makes use of parameters defined in the file
      !>     'sgparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If the reference directory structure is unrecognized, the
      !>         error SPICE(UNKNOWNREFDIR) is signaled. The most likely cause
      !>         of this error is that an upgrade to your version of the SPICE
      !>         toolkit is needed.
      !>
      !>     2)  If a value computed for the index of an implicitly indexed
      !>         generic segment is too large to be represented as an integer,
      !>         the error SPICE(INDEXTOOLARGE) is signaled.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE above.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to easily find the index and value
      !>     of the reference item that should be associated with a
      !>     value X. Given this information you can then easily retrieve
      !>     the packet that should be associated with X.
      !>
      !>## Examples
      !>
      !>     Suppose that you have a generic segment that contains the
      !>     following items.
      !>
      !>         1)  Packets that model the motion of a body as a function
      !>             of time over some interval of time.
      !>
      !>         2)  Reference values that are the epochs corresponding
      !>             to the beginning of the intervals for the packets.
      !>
      !>     To retrieve the correct packet to use to compute the position
      !>     and velocity of the body at a particular epoch,  ET, you could
      !>     use the following code. (Note this block of code assumes that
      !>     you aren't going to run into any exceptional cases such as ET
      !>     falling outside the range of times for which the packets can
      !>     provide ephemeris data.)
      !>
      !>        Find out the index of the time that should be associated
      !>        with the ET we've been given
      !>
      !>        CALL SGFRVI ( HANDLE, DESCR, ET,  ETFND, INDX, FOUND )
      !>
      !>        Fetch the INDX'th ephemeris packet from the segment.
      !>
      !>        CALL SGFPKT ( HANDLE, DESCR, INDX, EPHEM )
      !>
      !>## Restrictions
      !>
      !>     1)  The segment described by DESCR MUST be a generic segment,
      !>         otherwise the results of this routine are not predictable.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.1, 26-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
      !>
      !>        Replaced DAFRDA call with DAFGDA.
      !>
      !>-    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT)
      !>
      !>        A bug was found in the EXPCLS index case when the
      !>        trying to retrieve the last value in a generic segment.
      !>        This bug was discovered by the HP compiler complaining
      !>        that an index used was not initialized.
      !>
      !>        The offending line was
      !>
      !>                 MYVALU = BUFFER(I)
      !>
      !>        The corrected line is:
      !>
      !>                 MYVALU = BUFFER(BFINDX)
      !>
      !>-    SPICELIB Version 1.0.0, 28-MAR-1994 (KRG) (WLT)
      module subroutine sgfrvi ( ctx, handle, descr, x, value, indx, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      use pepper_arrays, only: lstled
      use pepper_inc_sgparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! The handle of a DAF open for reading.
      real(wp), intent(in) :: descr ( * )  !! The descriptor for a DAF generic segment.
      real(wp), intent(in) :: x  !! The key value used to find a reference and index.
      real(wp), intent(out) :: value  !! The reference value associated with X.
      integer(ip), intent(out) :: indx  !! The index of VALUE within the reference values.
      logical, intent(out) :: found  !! A flag indicating whether values for X were found.
      real(wp) :: myvalu_1(1)  !! one-element copy of myvalu (array argument)

      real(wp) :: buffer ( bufsiz )
      real(wp) :: dptemp
      real(wp) :: endref
      real(wp) :: myvalu

      integer(ip) :: begin
      integer(ip) :: bfindx
      integer(ip) :: fullrd
      integer(ip) :: end
      integer(ip) :: i
      integer(ip) :: myindx
      integer(ip) :: nfetch
      integer(ip) :: mynpkt
      integer(ip) :: mynrdr
      integer(ip) :: mynref
      integer(ip) :: myrdrb
      integer(ip) :: myrdrt
      integer(ip) :: myrefb
      integer(ip) :: rdridx
      integer(ip) :: remain

      logical :: done
      logical :: isdirv
      logical :: myfnd
!
!     Constants (SPICELIB computed them on the first call).
!
      real(wp), parameter :: dpimax = real( intmax, wp )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Set the value for the maximum index as a double precision number,
!     but only do it the first time into the subroutine.
!

!
!     Collect the necessary meta data values common to all cases.
!
      call sgmeta ( ctx, handle, descr, npkt,   mynpkt )
      call sgmeta ( ctx, handle, descr, nref,   mynref )
      call sgmeta ( ctx, handle, descr, rdrtyp, myrdrt )
      call sgmeta ( ctx, handle, descr, refbas, myrefb )

      if ( ctx%failed() ) return
!
!     Check to be sure that we know how to deal with the type of index
!     in the segment. The index type should be between the minimum
!     allowed index type, MNIDXT, and the maximum allowed index type,
!     MXIDXT, as specified in the file 'sgparam.inc'.
!
      if ( ( myrdrt < mnidxt) .or. ( myrdrt > mxidxt ) ) then

         call ctx%setmsg ( 'The generic DAF segment you attempted to' &
         //            ' read has an unsupported reference directory' &
         //            ' structure. The integer code given for this' &
         //            ' structure is #, and allowed codes are within' &
         //            ' the range # to #. The likely cause of this' &
         //            ' anomaly is your version of SPICELIB needs' &
         //            ' updating. Contact your system administrator' &
         //            ' or NAIF for a toolkit update.'               )
         call ctx%errint ( '#', myrdrt                                    )
         call ctx%errint ( '#', mnidxt                                    )
         call ctx%errint ( '#', mxidxt                                    )
         call ctx%sigerr ( 'SPICE(UNKNOWNREFDIR)'                         )
         return

      end if
!
!     We don't have an index yet and we initialize things to zero.
!
      myfnd  = .false.
      myindx = 0
      myvalu = 0.0_wp
!
!     We pass the idiot checks, so lets proceed. We have a IF block for
!     each allowed reference directory type code.
!
!        For implicitly indexed data packets, the interval
!
!           [ BUFFER(1), BUFFER(1) + (N - 1) * BUFFER(2) )
!
!        is divided into subintervals as follows:
!
!           (-infinity, r1), [r_1,r_2) [r_2, r_3), ..., [r_i, r_(i+1)),
!            ..., [r_N, +infinity),
!
!        where N = the number of packets in the segment, MYNPKT, and
!        r_i = BUFFER(1) + (i-1) * BUFFER(2).
!
!        If X is in [r_i, r_(i+1)), i = 1, N-1, then we found a value
!        and the index returned will be i with the reference value
!        returned will be r_i.
!
!        If X is in [r_N, +infinity), then we found a value and the
!        index returned will be N and the reference value returned will
!        be r_N.
!
!        If X is in (-infinity, r1), we have two possibilities:
!
!           1) If the index type is implicit closest, we found a value,
!              the index returned will be 1 and the reference value
!              returned will be r_1.
!
!           2) If the index type is implicit less than or equal, we do
!              not find a value.
!
!        For explicitly indexed packets we simply search the reference
!        directory for an appropriate reference value.
!
      if ( ( myrdrt /= imple ) .and. ( myrdrt /= impcls ) ) then
!
!        In addition to the meta data items we already have, we also
!        need these.
!
         call sgmeta ( ctx, handle, descr, nrdr,   mynrdr )
         call sgmeta ( ctx, handle, descr, rdrbas, myrdrb )

         if ( ctx%failed() ) return
!
!        We need to scan the reference directory (if there is one) to
!        determine the appropriate block of reference values to read
!        from the generic segment. Then we compute the number of
!        reference values to fetch and examine. Finally, based on the
!        index type we figure out whether we have found a reference
!        value or not. It will take a little while to get there, so
!        let's get going.
!
!        We have not started yet, so we're not done and we cannot have a
!        reference directory value yet.
!
         done   = .false.
         isdirv = .false.
!
!        We have not read any full buffers of reference directory values
!        yet, all of the reference directory values remain to be read,
!        and we have no index for a reference directory value.
!
         fullrd = 0
         remain = mynrdr
         rdridx = 0
!
!        Search the reference directory values to select the appropriate
!        block of reference values to read.
!
         do while ( ( .not. done ) .and. ( remain > 0 ) )
!
!           Read a buffer of reference directory items.
!
            nfetch = min ( dirsiz, remain )
            begin  = myrdrb + fullrd * dirsiz + 1
            end    = begin  + nfetch - 1

            call dafgda ( ctx, handle, begin, end, buffer )

            if ( ctx%failed() ) return
!
!           See if X is in the current buffer.
!
            rdridx = lstled ( x, nfetch, buffer )

            if ( rdridx == 0 ) then
!
!              If not, then X < BUFFER(1) and we're done. This indicates
!              that the desired reference value is before, or in, the
!              previous block of reference values.
!
               done = .true.

            else if ( rdridx == nfetch ) then
!
!              If we get the last value of the buffer, then either we
!              are done, X = BUFFER(NFETCH), or X > BUFFER(NFETCH).
!
               if ( x == buffer(nfetch) ) then
!
!                 If X = BUFFER(NFETCH) we are done, we have a directory
!                 value, and it might be a value we want to return.
!
                  done   = .true.
                  isdirv = .true.

               else
!
!                 Otherwise, we might have more stuff to read, so update
!                 the remainder and the current number of full buffer
!                 reads and try the loop again.
!
                  remain = remain - nfetch

                  if ( remain > 0 ) then
!
!                    We don't want to increment FULLRD for a partial
!                    buffer read. The arithmetic for the index
!                    calculations below will use RDRIDX to deal with
!                    this.
!
                     fullrd = fullrd + 1

                  end if

               end if

            else
!
!              BUFFER(1) <= X < BUFFER(NFETCH), i.e., we have something
!              in the buffer. Check to see if X = BUFFER(RDRIDX). If so,
!              we are done, we have a directory value, and it might be a
!              value we want to return. Otherwise, we are just done.
!
               done = .true.

               if ( x == buffer(rdridx) ) then

                  isdirv = .true.

               end if

            end if

         end do

         rdridx = fullrd * dirsiz + rdridx
!
!        There are three cases that we need to consider when X is not a
!        reference directory value:
!
!           Case 1: 0 < RDRIDX < MYNRDR (most common first)
!           Case 2: RDRIDX = 0
!           Case 3: RDRIDX = MYNRDR
!
         if ( .not. isdirv ) then

            if ( ( rdridx > 0 ) .and. ( rdridx < mynrdr ) ) then
!
!              If we were able to bracket X before reaching the end of
!              the reference directory, then we KNOW that we have a
!              candidate for a reference value in the reference data.
!              All we need to do is read the reference data and find it
!              in the buffer. We also read the reference directory
!              values that bracket the desired reference value into
!              BUFFER, so that they are there if we need them.
!
               nfetch = min( bufsiz, mynref - rdridx*dirsiz + 1 )

               begin  = myrefb + dirsiz * rdridx
               end    = begin  + nfetch - 1

               call dafgda ( ctx, handle, begin, end, buffer )

               if ( ctx%failed() ) return

               bfindx = lstled ( x, nfetch, buffer )
               myindx = dirsiz * rdridx + bfindx - 1

            else if ( rdridx == 0 ) then
!
!              The reference value may be one of the reference values
!              less than the first reference directory item. So we
!              compute the beginning and ending addresses for the data,
!              read it in, and try to find a reference value.
!
               nfetch = min ( bufsiz, mynref )

               begin  = myrefb + 1
               end    = begin  + nfetch - 1

               call dafgda ( ctx, handle, begin, end, buffer )

               if ( ctx%failed() ) return

               bfindx = lstled ( x, nfetch, buffer )
               myindx = bfindx

            else if ( rdridx == mynrdr ) then
!
!              If we were not able to bracket X before reaching the end
!              of the reference directory, then we might have a
!              candidate for a reference value in the reference data
!              after the last reference directory value. All we need to
!              do is read the reference data and look.
!
!              NOTE: NFETCH can never be zero or negative, so we can
!              glibly use it. The reason for this is the NFETCH can only
!              be zero if the desired reference value is a reference
!              directory value, and we already know that the reference
!              value we want is not a reference directory value, because
!              we are here. For similar reasons, NFETCH can never be
!              negative.
!
               begin  = myrefb + dirsiz * rdridx
               end    = myrefb + mynref
               nfetch = end - begin + 1

               call dafgda ( ctx, handle, begin, end, buffer )

               if ( ctx%failed() ) return

               bfindx = lstled ( x, nfetch, buffer )
               myindx = dirsiz * rdridx + bfindx - 1

            end if

         else
!
!           We have a reference directory value, whose index is easy to
!           compute.
!
            myindx = dirsiz * rdridx

         end if
!
!        Now, if we have a candidate for a reference value, lets make
!        sure, based on the type of index we have.
!
         if ( myrdrt == explt ) then
!
!           We have a reference value only if X > some reference
!           value.
!
            if ( .not. isdirv ) then
!
!              If the value is not a reference directory value, then
!              we have two cases:
!
!                 Case 1: 0 < MYINDX <= MYNREF
!                 Case 2: MYINDX = 0
!
               if (       ( myindx > 0      ) &
                    .and. ( myindx <= mynref ) ) then
!
!                 We found a reference value. The reference value we
!                 want is either the value indicated by MYINDX or
!                 the reference value immediately preceding MYINDX,
!                 if there is such a value. To deal with this we
!                 split the test up into two cases.
!
                  if ( myindx > 1 ) then
!
!                    If X > BUFFER(BFINDX) then we are done, so set the
!                    value. If not, then we want the reference value
!                    that is immediately before the current one.
!
                     if ( x > buffer(bfindx) ) then

                        myfnd  = .true.
                        myvalu = buffer(bfindx)

                     else

                        myfnd = .true.
                        myvalu = buffer(bfindx-1)
                        myindx = myindx - 1

                     end if

                  else
!
!                    Remember, MYINDX is 1 here. If we are greater
!                    than the first reference value in the segment,
!                    we are done. Otherwise there is no reference
!                    value to be associated with X.
!
                     if ( x > buffer(myindx) ) then

                        myfnd  = .true.
                        myvalu = buffer(myindx)

                     else
!
!                       We did not find a reference value. X was
!                       equal to the first reference value of the
!                       generic segment.
!
                        myfnd = .false.

                     end if

                  end if

               else if ( myindx == 0 ) then
!
!                 We did not find a reference value. X was < the
!                 first reference value for the generic segment.
!
                  myfnd = .false.

               end if

            else
!
!              We have a reference directory value, and we are done.
!              Either the reference directory value is the one we
!              want or the reference value immediately preceding it
!              is the one we want.
!
               myfnd  = .true.

               myindx = myindx - 1

               begin  = myrefb + myindx
               end    = begin

               call dafgda ( ctx, handle, begin, end, myvalu_1 )
               myvalu = myvalu_1(1)

               if ( ctx%failed() ) return

            end if

         else if ( myrdrt == exple ) then
!
!           We have a reference value only if X >= some reference
!           value. At this point, either we have the value and index
!           we want or X is before the first reference value of the
!           generic segment. We consider two cases, the first when X
!           is not a reference directory value, and the second when
!           it is.
!
            if ( .not. isdirv ) then
!
!              If X is not a directory value, then MYINDX is either
!              equal to zero, implying that X is before the first
!              reference value in the generic segment, or MYINDX > 0,
!              implying that we have found a reference value.
!
               if (       ( myindx > 0      ) &
                    .and. ( myindx <= mynref ) ) then

                  myfnd  = .true.
                  myvalu = buffer(bfindx)

               else if ( myindx == 0 ) then
!
!                 We did not find a reference value. X was < the
!                 first reference value for the generic segment.
!
                  myfnd = .false.

               end if

            else
!
!              We have a reference directory value, and it is the one
!              we want.
!
               myfnd  = .true.
               myvalu = x

            end if

         else if ( myrdrt == expcls ) then
!
!           We have a reference value for every value of X. If X <
!           the first reference value of the generic segment, the
!           closest value is the first reference value. If X > the
!           last reference value of the generic segment, the closest
!           value is the last reference value. For X between the
!           first and last reference values we simple take the
!           closest reference value to X, resolving a tie by
!           accepting the larger reference value.
!
            if ( .not. isdirv ) then
!
!              If X is not a directory value, then MYINDX is either
!              equal to zero, implying that X is before the first
!              reference value in the generic segment,
!              0 < MYINDX < MYNPKT, implying X is between the first
!              and last reference values in the generic segment, or
!              MYINDX = MYNPKT implying that X is greater than or
!              equal to the last reference value.
!
               if (       ( myindx > 0      ) &
                    .and. ( myindx < mynref ) ) then

                  i = bfindx
!
!                 Find the closest value to X, choosing the larger in
!                 the event of a tie.
!
                  if ( (buffer(i+1) - x) <= (x - buffer(i)) ) then

                     i      = i + 1
                     myindx = myindx + 1

                  end if

                  myfnd  = .true.
                  myvalu = buffer(i)

               else if ( myindx == 0 ) then
!
!                 X is before the first reference value for the
!                 generic segment, so the closest reference value is
!                 the first one.
!
                  myfnd  = .true.
                  myindx = 1
                  myvalu = buffer(1)

               else if ( myindx == mynref ) then
!
!                 X is at of after the last reference value for the
!                 generic segment, so the closest reference value is
!                 the last reference value, which will be in BUFFER.
!

                  myfnd  = .true.
                  myvalu = buffer(bfindx)

               end if

            else
!
!              We have a reference directory value, and it is the one
!              we want.
!
               myfnd  = .true.
               myvalu = x

            end if

         end if

      else if ( myrdrt == imple ) then
!
!        Get the begin and end addresses from which to read the
!        reference values and get the reference values.
!
         begin = myrefb + 1
         end   = myrefb + 2

         call dafgda ( ctx, handle, begin, end, buffer )

         if ( ctx%failed() ) return

         endref = buffer(1) + real(mynpkt-1, wp) * buffer(2)
!
!        Compute the index if we can.
!
         if ( x < buffer(1) ) then
!
!           If X is less than BUFFER(1), we do not have a reference
!           value.
!
            myfnd = .false.

         else if ( x > endref ) then
!
!           If X is greater than ENDREF, then we have a reference
!           value, ENDREF.
!
            myfnd  = .true.
            myindx = mynpkt
            myvalu = endref

         else
!
!           r_1 < X < r_N, i.e., we found a value. Compute the index
!           and the reference value.
!
            if ( mynpkt > 1 ) then

               myfnd = .true.
!
!              Compute the index.
!
               dptemp = 1.0_wp + (x - buffer(1)) / buffer(2)
!
!              Test to see if we can safely convert the index to an
!              integer.
!
               if ( dptemp > dpimax ) then

                  call ctx%setmsg ( 'The computed index is too large' &
                  //            ' to be represented as an integer.' &
                  //            ' The most likely problem is that' &
                  //            ' an incorrect value was stored' &
                  //            ' for the step size. The value' &
                  //            ' found for the step was: #'        )
                  call ctx%errdp ( '#', buffer(2)                       )
                  call ctx%sigerr ( 'SPICE(INDEXTOOLARGE)'              )
                  return

               end if

               myindx = idint( dptemp         )
               myindx = min  ( myindx, mynpkt )

            else
!
!              There is only one packet.
!
               myindx = 1

            end if
!
!           Compute the reference value.
!
            myvalu = buffer(1) + real(myindx-1, wp) * buffer(2)

         end if

      else if ( myrdrt == impcls ) then
!
!        Get the begin and end addresses from which to read the
!        reference values and get the reference values.
!
         begin = myrefb + 1
         end   = myrefb + 2

         call dafgda ( ctx, handle, begin, end, buffer )

         if ( ctx%failed() ) return

         endref = buffer(1) + real(mynpkt-1, wp) * buffer(2)
!
!        Compute the index if we can.
!
         if ( x < buffer(1) ) then
!
!           If X < BUFFER(1), then we found a value, the index
!           returned will be 1 and the reference value returned will
!           be BUFFER(1).
!
            myfnd  = .true.
            myindx = 1
            myvalu = buffer(1)

         else if (  x > endref ) then
!
!           If X > ENDREF, then we found a value, the index returned
!           will be MYNPKT and the reference value returned will be
!           ENDREF.
!
            myfnd  = .true.
            myindx = mynpkt
            myvalu = endref

         else
!
!           r_1 < X < r_N, i.e., we found a value. Compute the index
!           and the reference value. If X is closer to r_I, the index
!           returned will be I with a reference value of r_I. If X is
!           closer to r_(I+1), the index returned will be I+1 with a
!           reference value of r_(I+1).
!
            if ( mynpkt > 1 ) then

               myfnd = .true.
!
!              Compute the index.
!
               dptemp = 1.5_wp + (x - buffer(1)) / buffer(2)

               if ( dptemp > dpimax + 0.5_wp ) then

                  call ctx%setmsg ( 'The computed index is too large' &
                  //            ' to be represented as an integer.' &
                  //            ' The most likely problem is that' &
                  //            ' an incorrect value was stored' &
                  //            ' for the step size. The value' &
                  //            ' found for the step was: #'        )
                  call ctx%errdp ( '#', buffer(2)                       )
                  call ctx%sigerr ( 'SPICE(INDEXTOOLARGE)'              )
                  return

               end if

               myindx = idint( dptemp )

            else
!
!              There is only one packet.
!
               myindx = 1

            end if
!
!           Compute the reference value.
!
            myvalu = buffer(1) + real(myindx-1, wp) * buffer(2)

         end if

      end if

!
!     At this point, we have either found a value or not. If so, then we
!     need to set the index, value, and found flag for output.
!     Otherwise, we simply set the found flag.
!
      if ( myfnd ) then

         indx  = myindx
         value = myvalu

      end if

      found = myfnd

      return

      end subroutine sgfrvi

      !> Read a single data record from a type 12 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of the open SPK file which contains
      !>              the segment of interest.
      !>
      !>     DESCR    is the descriptor for a type 12 SPK segment that
      !>              contains the record of interest.
      !>
      !>     ET       is the target epoch used to determine the
      !>              particular record to be obtained from the SPK
      !>              segment.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | start epoch          |
      !>                 +----------------------+
      !>                 | step size            |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This subroutine will read a single record from a type 12 SPK
      !>     segment. The record read will provide the data necessary to
      !>     compute the state for the body designated by DESCR at epoch
      !>     ET.
      !>
      !>     The exact format and structure of a type 12 SPK segment is
      !>     described in the SPK Required Reading.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch, as in the example which follows.
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
      !>           IF ( TYPE .EQ. 12 ) THEN
      !>              CALL SPKR12 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine should not be called directly by a casual
      !>         user. It is intended for use by the subroutine SPKPVN, and
      !>         certain tests for error conditions are not performed here, as
      !>         SPKPVN will have already performed them.
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
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Exceptions section. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
      module subroutine spkr12 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of the open SPK file.
      real(wp), intent(in) :: descr    ( * )  !! Descriptor of the segment with the desired record.
      real(wp), intent(in) :: et  !! Epoch used to identify the desired record.
      real(wp), intent(out) :: record   ( * )  !! The desired type 12 SPK record.


!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The type 8 reader knows how to obtain a type 12 record.
!
      call spkr08 ( ctx, handle, descr, et, record )

      return
      end subroutine spkr12

      !> Read a single data record from a type 13 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of the open SPK file which contains
      !>              the segment of interest.
      !>
      !>     DESCR    is the descriptor for a type 13 SPK segment that
      !>              contains the record of interest.
      !>
      !>     ET       is the target epoch used to determine the
      !>              particular record to be obtained from the SPK
      !>              segment.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>              The structure of the record is as follows:
      !>
      !>                 +----------------------+
      !>                 | number of states (n) |
      !>                 +----------------------+
      !>                 | state 1 (6 elts.)    |
      !>                 +----------------------+
      !>                 | state 2 (6 elts.)    |
      !>                 +----------------------+
      !>                             .
      !>                             .
      !>                             .
      !>                 +----------------------+
      !>                 | state n (6 elts.)    |
      !>                 +----------------------+
      !>                 | epochs 1--n          |
      !>                 +----------------------+
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  It is assumed that the descriptor and handle supplied are
      !>         for a properly constructed type 13 segment. No checks are
      !>         performed to ensure this.
      !>
      !>     2)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>     3)  If any other error occurs while looking up SPK data, the error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This subroutine will read a single record from a type 13 SPK
      !>     segment. The record read will provide the data necessary to
      !>     compute the state for the body designated by DESCR at epoch
      !>     ET.
      !>
      !>     The exact format and structure of a type 13 SPK segment is
      !>     described in the SPK Required Reading.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch, as in the example which follows.
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
      !>           IF ( TYPE .EQ. 13 ) THEN
      !>              CALL SPKR13 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine should not be called directly by a casual
      !>         user. It is intended for use by the subroutine SPKPVN, and
      !>         certain tests for error conditions are not performed here, as
      !>         SPKPVN will have already performed them.
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
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated entry
      !>        #3 in $Exceptions section. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
      module subroutine spkr13 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of the open SPK file.
      real(wp), intent(in) :: descr    ( * )  !! Descriptor of the segment with the desired record.
      real(wp), intent(in) :: et  !! Epoch used to identify the desired record.
      real(wp), intent(out) :: record   ( * )  !! The desired type 13 SPK record.


!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The type 9 reader knows how to obtain a type 13 record.
!
      call spkr09 ( ctx, handle, descr, et, record )

      return
      end subroutine spkr13

      !> Read a single data record from a type 4 CK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the integer handle of the CK file containing the
      !>              segment.
      !>
      !>     DESCR    is the descriptor of the segment.
      !>
      !>     SCLKDP   is the encoded spacecraft clock time for which
      !>              pointing is being requested.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock.
      !>
      !>              When SCLKDP falls within the bounds of one of the
      !>              interpolation intervals then the tolerance has no
      !>              effect because pointing will be returned at the
      !>              request time.
      !>
      !>              However, if the request time is not in one of the
      !>              intervals, then the tolerance is used to determine
      !>              if pointing at one of the interval endpoints should
      !>              be returned.
      !>
      !>     NEEDAV   is .TRUE. if angular velocity is requested.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record that CKE04 will evaluate to determine
      !>              the pointing and it includes parameters:
      !>
      !>              ---------------------------------------------------
      !>              |    Encoded onboard time which is the closest    |
      !>              |  to SCLKDP and belongs to one of approximation  |
      !>              |                   intervals                     |
      !>              ---------------------------------------------------
      !>              |       encoded SCLK time of the midpoint of      |
      !>              |             interpolation interval              |
      !>              ---------------------------------------------------
      !>              |          radii of interpolation interval        |
      !>              |    expressed as double precision SCLK ticks     |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q0           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q1           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q2           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for q3           |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for AV1          |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for AV2          |
      !>              ---------------------------------------------------
      !>              |         Number of coefficients for AV3          |
      !>              ---------------------------------------------------
      !>              |               q0 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |               q1 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |               q2 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |               q3 Cheby coefficients             |
      !>              ---------------------------------------------------
      !>              |         AV1 Cheby coefficients (optional)       |
      !>              ---------------------------------------------------
      !>              |         AV2 Cheby coefficients (optional)       |
      !>              ---------------------------------------------------
      !>              |         AV3 Cheby coefficients (optional)       |
      !>              ---------------------------------------------------
      !>
      !>     FOUND    is .TRUE. if a record was found to satisfy the pointing
      !>              request. This occurs when the time for which pointing
      !>              is requested falls inside one of the interpolation
      !>              intervals, or when the request time is within the
      !>              tolerance of an interval endpoint.
      !>
      !>## Parameters
      !>
      !>     See 'ckparam.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified handle does not belong to an open DAF file,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If the specified descriptor does not belong to a segment data
      !>         format organized in accordance with generic segment
      !>         architecture, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     3)  If DESCR is not a valid descriptor of a segment in the CK
      !>         file specified by HANDLE, the results of this routine are
      !>         unpredictable.
      !>
      !>     4)  If the segment is not of data type 4, as specified in the
      !>         third integer component of the segment descriptor,
      !>         the error SPICE(WRONGDATATYPE) is signaled.
      !>
      !>     5)  If angular velocity data was requested but the segment
      !>         contains no such data, the error SPICE(NOAVDATA) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the CK Required Reading file for a detailed description of
      !>     the structure of a type 4 pointing segment.
      !>
      !>     When the time for which pointing was requested falls within an
      !>     interpolation interval, then FOUND will be true and RECORD will
      !>     contain the set of Chebyshev polynomial coefficients for the
      !>     time interval that brackets the request time. CKE04 will
      !>     evaluate RECORD to give pointing at the request time.
      !>
      !>     However, when the request time is not within any of the
      !>     interpolation intervals, then FOUND will be true only if the
      !>     interval endpoint closest to the request time is within the
      !>     tolerance specified by the user. In this case RECORD will
      !>     contain the set of Chebyshev polynomial coefficients for the
      !>     time interval one of the ends of which was within tolerance
      !>     from the request time, and CKE04 will evaluate RECORD to give
      !>     pointing at the time associated with that interval end time.
      !>
      !>## Examples
      !>
      !>     The CKRnn routines are usually used in tandem with the CKEnn
      !>     routines, which evaluate the record returned by CKRnn to give
      !>     the pointing information and output time.
      !>
      !>     The following code fragment searches backwards through all of the
      !>     segments in a file applicable to the Mars Global Surveyor
      !>     spacecraft bus that are of data type 4, for a particular
      !>     spacecraft clock time. It then evaluates the pointing for that
      !>     epoch and prints the result.
      !>
      !>     The search performed here does not mimic the behavior of the CK
      !>     reader APIs CKGP and CKGPAV, which consider data from multiple CK
      !>     files, when available. See the CK Required reading for details.
      !>
      !>     C
      !>     C     CK parameters include file.
      !>     C
      !>           INCLUDE               'ckparam.inc'
      !>
      !>     C
      !>     C     Local variables
      !>     C
      !>           CHARACTER*(20)        SCLKCH
      !>           CHARACTER*(20)        SCTIME
      !>           CHARACTER*(40)        IDENT
      !>
      !>           DOUBLE PRECISION      AV     ( 3 )
      !>           DOUBLE PRECISION      CLKOUT
      !>           DOUBLE PRECISION      CMAT   ( 3, 3 )
      !>           DOUBLE PRECISION      DCD    ( 2 )
      !>           DOUBLE PRECISION      DESCR  ( 5 )
      !>           DOUBLE PRECISION      RECORD ( CK4RSZ )
      !>           DOUBLE PRECISION      SCLKDP
      !>           DOUBLE PRECISION      TOL
      !>
      !>           INTEGER               HANDLE
      !>           INTEGER               I
      !>           INTEGER               ICD    ( 6 )
      !>           INTEGER               INST
      !>           INTEGER               SC
      !>
      !>           LOGICAL               FND
      !>           LOGICAL               NEEDAV
      !>           LOGICAL               SFND
      !>
      !>     C
      !>     C     Initial values.
      !>     C
      !>           SC     = -94
      !>           INST   = -94000
      !>           NEEDAV = .FALSE.
      !>
      !>     C
      !>     C     Load the MGS SCLK kernel and the C-kernel.
      !>     C
      !>           CALL FURNSH( 'MGS_SCLK.TSC' )
      !>           CALL DAFOPR( 'MGS_CK4.BC', HANDLE )
      !>
      !>     C
      !>     C     Get the spacecraft clock time. Then encode it for use
      !>     C     in the C-kernel.
      !>     C
      !>           CALL PROMPT( 'Enter SCLK string: ', SCLKCH )
      !>           CALL SCENCD( SC, SCLKCH, SCLKDP )
      !>
      !>     C
      !>     C     Use a tolerance of 2 seconds (half of the nominal
      !>     C     separation between MGS pointing instances ).
      !>     C
      !>           CALL SCTIKS ( SC, '0000000002:000', TOL )
      !>
      !>     C
      !>     C     Search backwards from the end of the CK file through all
      !>     C     of the segments.
      !>     C
      !>           CALL DAFBBS( HANDLE )
      !>           CALL DAFFPA( SFND   )
      !>
      !>           FND = .FALSE.
      !>
      !>           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
      !>
      !>     C
      !>     C        Get the segment identifier and descriptor.
      !>     C
      !>              CALL DAFGN( IDENT )
      !>              CALL DAFGS( DESCR )
      !>
      !>     C
      !>     C        Unpack the segment descriptor into its integer and
      !>     C        double precision components.
      !>     C
      !>              CALL DAFUS( DESCR, 2, 6, DCD, ICD )
      !>
      !>     C
      !>     C        Determine if this segment should be processed.
      !>     C
      !>              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
      !>          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
      !>          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
      !>          .        ( CK4DTP        .EQ. ICD( 3 ) )      ) THEN
      !>
      !>     C
      !>     C           Find CK 4 record covering requested time.
      !>     C
      !>                 CALL CKR04( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
      !>          .                  RECORD, FND )
      !>
      !>                 IF ( FND ) THEN
      !>
      !>     C
      !>     C              Compute pointing using found CK 4 record.
      !>     C
      !>                    CALL CKE04( NEEDAV, RECORD, CMAT, AV, CLKOUT)
      !>
      !>                    CALL SCDECD( SC, CLKOUT, SCTIME )
      !>
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Segment identifier: ', IDENT
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'Pointing returned for time: ',
      !>          .                      SCTIME
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) 'C-matrix:'
      !>                    WRITE (*,*)
      !>                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
      !>                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
      !>                    WRITE (*,*)
      !>
      !>                 END IF
      !>
      !>              END IF
      !>
      !>              CALL DAFFPA ( SFND )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  The file containing the segment should be opened for read
      !>         or write access either by CKLPF, DAFOPR, or DAFOPW.
      !>
      !>     2)  The record returned by this routine is intended to be
      !>         evaluated by CKE04.
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
      !>     E.D. Wright        (JPL)
      !>     Y.K. Zaiko         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.3, 17-AUG-2021 (NJB) (JDR)
      !>
      !>        Updated code example to use backwards search. Added
      !>        note regarding difference between this search and those
      !>        performed by the CK reader APIs CKGP and CKGPAV.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
      !>
      !>        Minor header edits.
      !>
      !>-    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
      module subroutine ckr04 ( ctx, handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr  ( * )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Pointing request time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      logical, intent(in) :: needav  !! Angular velocity request flag.
      real(wp), intent(out) :: record ( * )  !! Pointing data record.
      logical, intent(out) :: found  !! .TRUE. when a record covering SCLKDP is found.
      integer(ip) :: ends_1(1)  !! one-element copy of ends (array argument)

      real(wp) :: value
      real(wp) :: midpt1
      real(wp) :: midpt2
      real(wp) :: rad1
      real(wp) :: rad2
      real(wp) :: lbnd1
      real(wp) :: lbnd2
      real(wp) :: rbnd1
      real(wp) :: dcd    ( 2 )
      real(wp) :: clkout

      integer(ip) :: numall
      integer(ip) :: numcft ( qavsiz )
      integer(ip) :: icd    ( 6 )
      integer(ip) :: ends
      integer(ip) :: indx
      integer(ip) :: k
      integer(ip) :: nrec

      logical :: exist

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Set initial value of the found flag to "NOT FOUND".
!
      found = .false.

!
!     We need to unpack and analyze descriptor components. The
!     unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!
      call dafus  ( ctx, descr, 2, 6, dcd, icd )

!
!     Check if the segment is type 4. Signal an error if it's not.
!
      if ( icd( 3 ) /= ck4dtp ) then

         call ctx%setmsg ( 'The segment is not a type 4 segment.  '   // &
                       'Type is #'                                 )
         call ctx%errint ( '#', icd(3)                                 )
         call ctx%sigerr ( 'SPICE(WRONGDATATYPE)'                      )
         return

      end if

      if ( needav ) then

!
!        Signal an error if angular velocities are required but
!        they are not present in the segment.
!
         if ( icd( 4 ) /= 1 ) then

            call ctx%setmsg ( 'Segment does not contain angular '     // &
                          'velocity data.'                        )
            call ctx%sigerr ( 'SPICE(NOAVDATA)'                       )
            return

         end if

      end if

!
!     Get number of records (packets) in the segment.
!
      call cknr04 ( ctx, handle, descr, nrec )

!
!     Locate the last time in the set of reference epochs less than or
!     equal to the input SCLKDP.
!
      call sgfrvi ( ctx, handle, descr, sclkdp, value, indx, exist )

      if ( ctx%failed() ) return

      if ( .not. exist ) then

!
!        We didn't find reference value with means that SCLKDP is
!        less than the left bound of the first interpolation interval.
!        Fetch the first record.
!
         indx = 1
         call sgfpkt ( ctx, handle, descr, indx, indx, record, ends_1 )
         ends = ends_1(1)

         if ( ctx%failed() ) return

         midpt1 = record( 1 )
         rad1   = record( 2 )

!
!        Check whether SCLKDP is within TOL of the left bound of the
!        first interval.
!
         lbnd1 = midpt1 - rad1 - tol

         if ( sclkdp >= lbnd1  ) then

            found = .true.
            clkout = midpt1 - rad1

         end if

      else

!
!        We found reference value.
!
         if ( indx >= nrec ) then

!
!           The SCLKDP is greater than the left bound of the last
!           interpolation interval. Fetch the last record.
!
            indx = nrec

            call sgfpkt ( ctx, handle, descr, indx, indx, record, ends_1 )
            ends = ends_1(1)

            if ( ctx%failed() ) return

            midpt1 = record( 1 )
            rad1   = record( 2 )

!
!           Check whether SCLKDP is within TOL of the right bound of
!           the last interval.
!
            rbnd1 = midpt1 + rad1 + tol

            if ( sclkdp <= rbnd1 ) then

               found = .true.

!
!              Check whether SCLKDP falls between right bound of the
!              last interval and right bound + TOL.
!
               rbnd1 = midpt1 + rad1

               if ( sclkdp >= rbnd1 ) then

                  clkout = midpt1 + rad1

               else

!
!                 SCLKDP belongs to the last interval
!
                  clkout = sclkdp

               end if

            end if

         else if ( ( indx >= 1 ) .and. ( indx < nrec ) ) then

!
!           The SCLKDP lies between left bound of the first interval
!           and the right bound of the interval before the last
!           interval. Fetch the found record.
!
            call sgfpkt ( ctx, handle, descr, indx, indx, record, ends_1 )
            ends = ends_1(1)

            if ( ctx%failed() ) return

            midpt1 = record( 1 )
            rad1   = record( 2 )

!
!           Check whether SCLKDP belongs to current interval.
!
            rbnd1 = midpt1 + rad1

            if ( sclkdp <= rbnd1 ) then

               found = .true.
               clkout = sclkdp

            else

!
!              SCLKDP doesn't belong to current interval. Fetch the
!              next packet.
!
               call sgfpkt ( ctx, handle, descr, indx + 1, indx + 1, record, ends_1 )
               ends = ends_1(1)

               if ( ctx%failed() ) return

               midpt2 = record( 1 )
               rad2   = record( 2 )

!
!              Find the closest interval bound for SCLKDP.
!
               rbnd1 = midpt1 + rad1
               lbnd2 = midpt2 - rad2

               if ( ( sclkdp - rbnd1 ) <= ( lbnd2 - sclkdp ) ) then

!
!                 SCLKDP is closer to the right bound of current
!                 interval. Check whether it's within TOL of it.
!
                  rbnd1 = midpt1 + rad1 + tol

                  if ( sclkdp <= rbnd1 ) then

                     found = .true.
                     clkout = midpt1 + rad1

!
!                    At this point we need to re-read our current
!                    record because it was overwritten by the next
!                    record. No FAILED() check here -- we already
!                    fetched this packet successfully one call to
!                    SGFPKT ago.
!
                     call sgfpkt ( ctx, handle, descr, indx, indx, record, ends_1 )
                     ends = ends_1(1)

                  end if

               else

!
!                 SCLKDP is closer to the left bound of the next
!                 interval. Check whether it's within TOL of it.
!
                  lbnd2 = midpt2 - rad2 - tol

                  if ( sclkdp >= lbnd2 ) then

                     found = .true.
                     indx = indx + 1
                     clkout = midpt2 - rad2

                  end if

               end if

            end if

         end if

      end if

!
!     If we found the interval on segment the SCLKDP belongs to, then
!
      if ( found ) then

!
!        Decode numbers of polynomial coefficients.
!
         call zzck4d2i ( ctx, record( 3 ), qavsiz, ck4pcd, numcft )

!
!        Count total number of coefficients.
!
         numall = 0

         do k = 1, qavsiz
            numall = numall + numcft ( k )
         end do

!
!        Move coefficients to the right and insert numbers of
!        coefficients into output RECORD.
!
         do k = numall, 1, -1
            record ( k + ck4sft ) = record( k + 3 )
         end do

         do k = 1, qavsiz
            record( k + 3 ) = numcft( k )
         end do

         record( 3 ) = record( 2 )
         record( 2 ) = record( 1 )

!
!        Insert CLKOUT into output RECORD
!
         record( 1 ) = clkout

      end if

!
!     All done.
!

      return

      end subroutine ckr04

      !> Evaluate a single PCK data record from a segment of type 03
      !> (Variable width Chebyshev Polynomials for RA, DEC, and W) to
      !> obtain a state transformation matrix.
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, at which a state transformation
      !>              matrix is to be calculated.
      !>
      !>     RECORD   is a data record which, when evaluated at epoch ET,
      !>              will give RA, DEC, and W and angular velocity
      !>              for a body. The RA, DEC and W are relative to
      !>              some inertial frame. The angular velocity is
      !>              expressed relative to the body fixed coordinate frame.
      !>
      !>## Detailed Output
      !>
      !>     ROTMAT   is the state transformation matrix at epoch ET.
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
      !>     The exact format and structure of type 03 PCK segments are
      !>     described in the PCK Required Reading file.
      !>
      !>     A type 03 segment contains six sets of Chebyshev coefficients,
      !>     one set each for RA, DEC, and W and one set each for the
      !>     components of the angular velocity of the body. The coefficients
      !>     for RA, DEC, and W are relative to some inertial reference
      !>     frame. The coefficients for the components of angular velocity
      !>     are relative to the body fixed frame and must be transformed
      !>     via the position transformation corresponding to RA, DEC and W.
      !>
      !>     PCKE03 calls the routine CHBVAL to evaluate each polynomial,
      !>     to obtain a complete set of values. These values are then
      !>     used to determine a state transformation matrix that will
      !>     rotate an inertially referenced state into the bodyfixed
      !>     coordinate system.
      !>
      !>## Examples
      !>
      !>     The PCKEnn routines are almost always used in conjunction with
      !>     the corresponding PCKRnn routines, which read the records from
      !>     binary PCK files.
      !>
      !>     The data returned by the PCKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the PCKRnn
      !>     routines might be used to examine raw segment data before
      !>     evaluating it with the PCKEnn routines.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>     C
      !>     C     Look at parts of the descriptor.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>           TYPE   = ICD( 3 )
      !>
      !>           IF ( TYPE .EQ. 03 ) THEN
      !>
      !>              CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL PCKE03 ( ET, RECORD, ROTMAT )
      !>                  .
      !>                  .  Apply the rotation and check out the state.
      !>                  .
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
      !>     K.R. Gehringer     (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.2, 20-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 3.0.0, 06-OCT-1995 (WLT)
      !>
      !>        Brian Carcich at Cornell discovered that the Euler
      !>        angles were being re-arranged unnecessarily. As a
      !>        result the state transformation matrix computed was
      !>        not the one we expected. (The re-arrangement was
      !>        a left-over from  implementation 1.0.0. This problem
      !>        has now been corrected.
      !>
      !>-    SPICELIB Version 2.0.0, 28-JUL-1995 (WLT)
      !>
      !>        Version 1.0.0 was written under the assumption that
      !>        RA, DEC, W and dRA/dt, dDEC/dt and dW/dt were supplied
      !>        in the input RECORD. This version repairs the
      !>        previous misinterpretation.
      !>
      !>-    SPICELIB Version 1.0.0, 14-MAR-1995 (KRG)
      module subroutine pcke03 ( ctx, et, record, rotmat )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: halfpi, rpd
      use pepper_math, only: chbval, vcrss
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch state transformation.
      real(wp), intent(in) :: record   ( *    )  !! Data record valid for epoch ET.
      real(wp), intent(out) :: rotmat   ( 6, 6 )  !! State transformation matrix at epoch ET.

      real(wp) :: eulang ( 6    )
      real(wp) :: mav    ( 3    )
      real(wp) :: rot    ( 3, 3 )
      real(wp) :: drotdt ( 3, 3 )

      integer(ip) :: ncoeff
      integer(ip) :: degree
      integer(ip) :: cofloc
      integer(ip) :: i
      integer(ip) :: j
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The first number in the record is the number of Chebyshev
!     Polynomial coefficients used to represent each component of the
!     state vector.  Following it are two numbers that will be used
!     later, then the six sets of coefficients.
!
      ncoeff = int( record( 1 ) )
!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degree = ncoeff - 1
!
!     Call CHBVAL once for each quantity to obtain RA, DEC, and W values
!     as well as values for the angular velocity.
!
!     Note that we stick the angular velocity in the components 4 thru 6
!     of the array EULANG even though they are not derivatives of
!     components 1 thru 3.  It's just simpler to do it this way.
!
!     Editorial Comment:
!
!        Unlike every other SPICE routine, the units for the type 03
!        PCK segment are degrees.  This inconsistency exists solely
!        to support the NEAR project and the intransigence of one of the
!        participants of that project.
!
!        It's a bad design and we know it.
!
!        ---W.L. Taber
!
      do i = 1, 6
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         cofloc = ncoeff * ( i - 1 ) + 4
!
!        CHBVAL needs as input the coefficients, the degree of the
!        polynomial, the epoch, and also two variable transformation
!        parameters, which are located, in our case, in the second and
!        third slots of the record.
!
         call chbval ( record( cofloc ), degree, record( 2 ), et, &
                       eulang(i)                                  )
!
!        Convert to radians.
!
         eulang(i) = rpd * eulang(i)

      end do
!
!     EULANG(1) is RA make it PHI
!     EULANG(2) is DEC make it DELTA
!     EULANG(3) is W
!
      eulang(1) = halfpi + eulang(1)
      eulang(2) = halfpi - eulang(2)

!
!     Before we obtain the state transformation matrix, we need to
!     compute the rotation components of the transformation..
!     The rotation we want to perform is:
!
!        [W]  [DELTA]  [PHI]
!           3        1      3
!
!     The array of Euler angles is now:
!
!        EULANG(1) = PHI
!        EULANG(2) = DELTA
!        EULANG(3) = W
!        EULANG(4) = AV_1 (bodyfixed)
!        EULANG(5) = AV_2 (bodyfixed)
!        EULANG(6) = AV_3 (bodyfixed)
!
!     Compute the rotation associated with the Euler angles.
!
      call eul2m ( ctx, eulang(3), eulang(2), eulang(1), 3, 1, 3, rot  )

!
!     This rotation transforms positions relative to the inertial
!     frame to positions relative to the bodyfixed frame.
!
!     We next need to get dROT/dt.
!
!     For this discussion let P be the bodyfixed coordinates of
!     a point that is fixed with respect to the bodyfixed frame.
!
!     The velocity of P with respect to the inertial frame is
!     given by
!                 t             t
!        V   = ROT ( AV ) x  ROT ( P )
!
!                  t
!              dROT
!            = ----  ( P )
!               dt
!
!     But
!            t            t            t
!         ROT ( AV ) x ROT ( P ) = ROT  ( AV x P )
!
!     Let OMEGA be the cross product matrix corresponding to AV.
!     Then
!           t                   t
!        ROT  ( AV x P )  =  ROT * OMEGA * P
!
!     where * denotes matrix multiplication.
!
!     From these observations it follows that
!
!                                  t
!           t                  dROT
!        ROT  * OMEGA * P   =  ---- * P
!                                dt
!
!     Consequently, it follows that
!
!        dROT         t
!        ----  = OMEGA  * ROT
!         dt
!
!              = -OMEGA * ROT
!
!     We compute dROT/dt now.  Note that we can get the columns
!     of  -OMEGA*ROT by computing the cross products -AV x COL
!     for each column COL of ROT.
!
      mav(1) = -eulang(4)
      mav(2) = -eulang(5)
      mav(3) = -eulang(6)

      call vcrss ( mav, rot(1,1), drotdt(1,1) )
      call vcrss ( mav, rot(1,2), drotdt(1,2) )
      call vcrss ( mav, rot(1,3), drotdt(1,3) )

!
!     Now we simply fill in the blanks.
!
      do i = 1, 3
         do j = 1, 3
            rotmat(i,  j  ) = rot   (i,j)
            rotmat(i+3,j  ) = drotdt(i,j)
            rotmat(i,  j+3) = 0.0_wp
            rotmat(i+3,j+3) = rot   (i,j)
         end do
      end do

      return
      end subroutine pcke03

      !> Read a single PCK data record from a segment of type 03.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the file handle for a type 03 PCK segment.
      !>
      !>     DESCR    is the segment descriptor for a type 03 PCK segment.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              the specified segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the RA, DEC,
      !>              W and body fixed angular rates for the body associated
      !>              with the segment.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  It is assumed that the descriptor and handle supplied are
      !>         for a properly constructed type 03 segment. No checks are
      !>         performed to ensure this.
      !>
      !>     2)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>     3)  If any issue is detected while reading the PCK data, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This subroutine reads a type 03 PCK record from the segment
      !>     specified by HANDLE and DESCR. The record read will contain
      !>     sufficient information to to compute RA, DEC, W and body fixed
      !>     angular rates for the body associated with the segment for epoch
      !>     ET.
      !>
      !>     See the PCK Required Reading file for a description of the
      !>     structure of a type 03 PCK segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the PCKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the PCKRnn
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
      !>
      !>     C
      !>     C     Get a segment applicable to a specified body and epoch.
      !>     C
      !>           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
      !>
      !>     C
      !>     C     Look at parts of the descriptor.
      !>     C
      !>           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
      !>           CENTER = ICD( 2 )
      !>           REF    = ICD( 3 )
      !>           TYPE   = ICD( 4 )
      !>
      !>           IF ( TYPE .EQ. 03 ) THEN
      !>              CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  It is assumed that the descriptor and handle supplied are
      !>         for a properly constructed type 03 segment. No checks are
      !>         performed to ensure this.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 12-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>
      !>-    SPICELIB Version 1.0.0, 20-SEP-1995 (KRG)
      module subroutine pckr03 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle for a PCK file.
      real(wp), intent(in) :: descr    ( 5 )  !! Descriptor for a type 03 PCK segment.
      real(wp), intent(in) :: et  !! Target epoch for orientation information.
      real(wp), intent(out) :: record   ( * )  !! Data record associated with epoch ET.
      integer(ip) :: ends_1(1)  !! one-element copy of ends (array argument)

      real(wp) :: value

      integer(ip) :: ends
      integer(ip) :: indx

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Check the request time against the time bounds in the segment
!     descriptor. In order to get the right data back from the generic
!     segment calls below, we need to be sure that the desired epoch
!     falls within the bounds of the segment, as specified by the
!     descriptor. The first two elements of the descriptor are the start
!     time for the segment and the stop time for the segment,
!     respectively.
!
      if ( ( et < descr(1) )  .or.  ( et > descr(2) ) )  then

         call ctx%setmsg( 'Request time # is outside of descriptor'    // &
                      ' bounds # : #.'                              )
         call ctx%errdp ( '#',  et                                      )
         call ctx%errdp ( '#',  descr(1)                                )
         call ctx%errdp ( '#',  descr(2)                                )
         call ctx%sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if
!
!     Fetch the constants and store them in the first part of
!     the output RECORD.
!
      call sgfcon ( ctx, handle, descr, 1,  nconst, record(1) )
!
!     Locate the time in the file less than or equal to the input ET.
!
      call sgfrvi ( ctx, handle, descr, et, value, indx, found )
!
!     Fetch the data record.
!
      call sgfpkt ( ctx, handle, descr, indx, indx, record(begel1), ends_1 )
      ends = ends_1(1)

      return

      end subroutine pckr03

      !> Evaluate a single SPK data record from a segment of type 5
      !> (two body propagation between discrete state vectors).
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
      !>                     .            state of the body at epoch 1.
      !>                     .
      !>                     .
      !>                  RECORD(6)
      !>
      !>                  RECORD(7)
      !>                     .
      !>                     .            state of the body at epoch 2.
      !>                     .
      !>                  RECORD(12)
      !>                  RECORD(13)      epoch 1 in seconds past 2000.
      !>                  RECORD(14)      epoch 2 in seconds past 2000.
      !>                  RECORD(15)      GM for the center of motion.
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
      !>              Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is a problem propagating, subject to the laws of two
      !>         body motion, either of the states from RECORD to the requested
      !>         time ET, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine interpolates a state from the two reference states
      !>     contained in RECORD.
      !>
      !>     It is assumed that this routine is used in conjunction with
      !>     the routine SPKR05 as shown here:
      !>
      !>        CALL SPKR05 ( HANDLE, DESCR, ET, RECORD         )
      !>        CALL SPKE05 (                ET, RECORD, STATE  )
      !>
      !>     Where it is known in advance that the HANDLE, DESCR pair points
      !>     to a type 05 data segment.
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
      !>           IF ( TYPE .EQ. 5 ) THEN
      !>
      !>              CALL SPKR05 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE05 ( ET, RECORD, STATE )
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
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     J.M. Lynch         (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VADD call.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
      !>
      !>        The declaration for the SPICELIB function PI is now
      !>        preceded by an EXTERNAL statement declaring PI to be an
      !>        external function. This removes a conflict with any
      !>        compilers that have a PI intrinsic function.
      !>
      !>-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 31-AUG-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VADD call.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
      !>
      !>        The declaration for the SPICELIB function PI is now
      !>        preceded by an EXTERNAL statement declaring PI to be an
      !>        external function. This removes a conflict with any
      !>        compilers that have a PI intrinsic function.
      !>
      !>-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
      module subroutine spke05 ( ctx, et, record, state )
      use pepper_kinds, only: wp
      use pepper_constants, only: pi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(in) :: record   ( * )  !! Data record.
      real(wp), intent(out) :: state    ( 6 )  !! State (position and velocity).

      real(wp) :: arg
      real(wp) :: dargdt
      real(wp) :: denom
      real(wp) :: dwdt
      real(wp) :: gm
      real(wp) :: numer
      real(wp) :: pv       ( 6, 2 )
      real(wp) :: s1       ( 6 )
      real(wp) :: s2       ( 6 )
      real(wp) :: t1
      real(wp) :: t2
      real(wp) :: vcomp    ( 3 )
      real(wp) :: vel      ( 3 )
      real(wp) :: w

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Unpack the record, for easier reading.
!
      pv = reshape(record(1:12), shape(pv))

      t1 = record(13)
      t2 = record(14)
      gm = record(15)

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
!        ( (t1+t2)/2,  W( (t1+t2)/2 )
!
!     The range of W is from 1 to 0.  And the derivative of W is
!     symmetric and zero at both t1 and t2.
!
      if ( t1 /= t2 ) then

         call prop2b ( ctx, gm, pv(1,1), et - t1, s1 )
         call prop2b ( ctx, gm, pv(1,2), et - t2, s2 )

         numer  = et - t1
         denom  = t2 - t1
         arg    = numer*pi/denom
         dargdt =       pi/denom

         w      =  0.5_wp + 0.5_wp * cos( arg )
         dwdt   =        - 0.5_wp * sin( arg ) * dargdt

         state = w*s1 + (1.0_wp-w)*s2
         vcomp = dwdt*s1(1:3) + (-dwdt)*s2(1:3)
         vel = state(4:6) + vcomp
         state(4:6) = vel

      else

         call prop2b ( ctx, gm, pv(1,1), et - t1, state )

      end if

      return
      end subroutine spke05

      !> Evaluate a single SPK data record from a segment of type 15
      !> (Precessing Conic Propagation).
      !>
      !>## Detailed Input
      !>
      !>     ET       is a target epoch, specified as ephemeris seconds past
      !>              J2000, at which a state vector is to be computed.
      !>
      !>     RECIN    is a data record which, when evaluated at epoch ET,
      !>              will give the state (position and velocity) of some
      !>              body, relative to some center, in some inertial
      !>              reference frame.
      !>
      !>              The structure of RECIN is:
      !>
      !>              RECIN(1)             epoch of periapsis
      !>                                   in ephemeris seconds past J2000.
      !>              RECIN(2)-RECIN(4)    unit trajectory pole vector
      !>              RECIN(5)-RECIN(7)    unit periapsis vector
      !>              RECIN(8)             semi-latus rectum---p in the
      !>                                   equation:
      !>
      !>                                   r = p/(1 + ECC*COS(Nu))
      !>
      !>              RECIN(9)             eccentricity
      !>              RECIN(10)            J2 processing flag describing
      !>                                   what J2 corrections are to be
      !>                                   applied when the orbit is
      !>                                   propagated.
      !>
      !>                                   All J2 corrections are applied
      !>                                   if this flag has a value that
      !>                                   is not 1,2 or 3.
      !>
      !>                                   If the value of the flag is 3
      !>                                   no corrections are done.
      !>
      !>                                   If the value of the flag is 1
      !>                                   no corrections are computed for
      !>                                   the precession of the line
      !>                                   of apsides. However, regression
      !>                                   of the line of nodes is
      !>                                   performed.
      !>
      !>                                   If the value of the flag is 2
      !>                                   no corrections are done for
      !>                                   the regression of the line of
      !>                                   nodes. However, precession of the
      !>                                   line of apsides is performed.
      !>
      !>                                   Note that J2 effects are computed
      !>                                   only if the orbit is elliptic and
      !>                                   does not intersect the central
      !>                                   body.
      !>
      !>              RECIN(11)-RECIN(13)  unit central body pole vector
      !>              RECIN(14)            central body GM
      !>              RECIN(15)            central body J2
      !>              RECIN(16)            central body radius
      !>
      !>              Units are radians, km, seconds
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state produced by evaluating RECIN at ET.
      !>              Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the eccentricity is less than zero, the error
      !>         SPICE(BADECCENTRICITY) is signaled.
      !>
      !>     2)  If the semi-latus rectum is non-positive, the error
      !>         SPICE(BADLATUSRECTUM) is signaled.
      !>
      !>     3)  If the pole vector, trajectory pole vector or periapsis vector
      !>         has zero length, the error SPICE(BADVECTOR) is signaled.
      !>
      !>     4)  If the trajectory pole vector and the periapsis vector are not
      !>         orthogonal, the error SPICE(BADINITSTATE) is signaled. The
      !>         test for orthogonality is very crude. The routine simply
      !>         checks that the absolute value of the dot product of the unit
      !>         vectors parallel to the trajectory pole and periapse vectors
      !>         is less than 0.00001. This check is intended to catch
      !>         blunders, not to enforce orthogonality to double precision
      !>         tolerance.
      !>
      !>     5)  If the mass of the central body is non-positive, the error
      !>         SPICE(NONPOSITIVEMASS) is signaled.
      !>
      !>     6)  If the radius of the central body is negative, the error
      !>         SPICE(BADRADIUS) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This algorithm applies J2 corrections for precessing the
      !>     node and argument of periapse for an object orbiting an
      !>     oblate spheroid.
      !>
      !>     Note the effects of J2 are incorporated only for elliptic
      !>     orbits that do not intersect the central body.
      !>
      !>     While the derivation of the effect of the various harmonics
      !>     of gravitational field are beyond the scope of this header
      !>     the effect of the J2 term of the gravity model are as follows
      !>
      !>        The line of node precesses. Over one orbit average rate of
      !>        precession,  DNode/dNu,  is given by
      !>
      !>                                3 J2
      !>              dNode/dNu =  -  -----------------  DCOS( inc )
      !>                                2 (P/RPL)**2
      !>
      !>        (Since this is always less than zero for oblate spheroids, this
      !>           should be called regression of nodes.)
      !>
      !>        The line of apsides precesses. The average rate of precession
      !>        DPeri/dNu is given by
      !>                                   3 J2
      !>              dPeri/dNu =     ----------------- ( 5*DCOS ( inc ) - 1 )
      !>                                2 (P/RPL)**2
      !>
      !>        Details of these formulae are given in the Battin's book (see
      !>        literature references below).
      !>
      !>     It is assumed that this routine is used in conjunction with
      !>     the routine SPKR15 as shown here:
      !>
      !>        CALL SPKR15 ( HANDLE, DESCR, ET, RECIN         )
      !>        CALL SPKE15 (                ET, RECIN, STATE  )
      !>
      !>     where it is known in advance that the HANDLE, DESCR pair points
      !>     to a type 15 data segment.
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
      !>           IF ( TYPE .EQ. 15 ) THEN
      !>
      !>              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>              CALL SPKE15 ( ET, RECORD, STATE )
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
      !>     [1]  J. Danby, "Fundamentals of Celestial Mechanics," 2nd Edition,
      !>          pp.345-347, Willman-Bell, 1989.
      !>
      !>     [2]  R. H. Battin, "Astronautical Guidance," pp.199, McGraw-Hill
      !>          Book Company, 1964.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     S. Schlaifer       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 14-APR-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
      !>
      !>        Updated to remove non-standard use of duplicate arguments
      !>        in VHAT, VROTV, and VSCL calls.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
      !>
      !>        The declaration for the SPICELIB function PI is now
      !>        preceded by an EXTERNAL statement declaring PI to be an
      !>        external function. This removes a conflict with any
      !>        compilers that have a PI intrinsic function.
      !>
      !>-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
      module subroutine spke15 ( ctx, et, recin, state )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpr, pi, twopi
      use pepper_math, only: vcrss, vdot, vhatip, vrotv, vsep
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(in) :: recin  ( * )  !! Data record.
      real(wp), intent(out) :: state  ( 6 )  !! State (position and velocity).

      real(wp) :: angle
      real(wp) :: cosinc
      real(wp) :: dmdt
      real(wp) :: dnode
      real(wp) :: dot
      real(wp) :: dperi
      real(wp) :: dt
      real(wp) :: ecc
      real(wp) :: epoch
      real(wp) :: gm
      real(wp) :: k2pi
      real(wp) :: manom
      real(wp) :: near
      real(wp) :: oj2
      real(wp) :: oneme2
      real(wp) :: p
      real(wp) :: pa     ( 3 )
      real(wp) :: pv     ( 3 )
      real(wp) :: rpl
      real(wp) :: speed
      real(wp) :: state0 ( 6 )
      real(wp) :: ta
      real(wp) :: theta
      real(wp) :: tmpsta ( 6 )
      real(wp) :: tp     ( 3 )
      real(wp) :: z

      integer(ip) :: j2flg

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Fetch the various entities from the input record, first the epoch.
!
      epoch  = recin(1)

!
!     The trajectory pole vector.
!
      tp = recin(2:4)

!
!     The periapsis vector.
!
      pa = recin(5:7)

!
!     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
!     and eccentricity.
!
      p      = recin(8)
      ecc    = recin(9)

!
!     J2 processing flag.
!
      j2flg  = int( recin(10) )

!
!     Central body pole vector.
!
      pv = recin(11:13)

!
!     The central mass, J2 and radius of the central body.
!
      gm     = recin(14)
      oj2    = recin(15)
      rpl    = recin(16)

!
!     Check all the inputs here for obvious failures.  Yes, perhaps
!     this is overkill.  However, there is a lot more computation
!     going on in this routine so that the small amount of overhead
!     here should not be significant.
!

      if ( p <= 0 ) then

         call ctx%setmsg ( 'The semi-latus rectum supplied to the ' &
         //            'SPK type 15 evaluator was non-positive.  This ' &
         //            'value must be positive. The value supplied was ' &
         //            '#.'                    )
         call ctx%errdp  ( '#', p                  )
         call ctx%sigerr ( 'SPICE(BADLATUSRECTUM)' )
         return

      else if ( ecc < 0.0_wp ) then

         call ctx%setmsg ( 'The eccentricity supplied for a type 15 ' &
         //            'segment is negative.  It must be non-negative. ' &
         //            'The value supplied ' &
         //            'to the type 15 evaluator was #. ' )
         call ctx%errdp  ( '#',   ecc                )
         call ctx%sigerr ( 'SPICE(BADECCENTRICITY)'  )
         return

      else if ( gm  <= 0.0_wp ) then

         call ctx%setmsg ( 'The mass supplied for the central body ' &
         //            'of a type 15 segment was non-positive. ' &
         //            'Masses must be positive.  The value ' &
         //            'supplied was #. '        )
         call ctx%errdp  ( '#', gm                   )
         call ctx%sigerr ( 'SPICE(NONPOSITIVEMASS)'  )
         return

      else if ( all(tp == 0.0_wp) ) then

         call ctx%setmsg ( 'The trajectory pole vector supplied to ' &
         //            'SPKE15 had length zero. The most likely ' &
         //            'cause of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         call ctx%sigerr ( 'SPICE(BADVECTOR)'   )
         return

      else if ( all(pa == 0.0_wp) ) then

         call ctx%setmsg ( 'The periapse vector supplied to SPKE15 ' &
         //            'had length zero. The most likely cause ' &
         //            'of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         call ctx%sigerr ( 'SPICE(BADVECTOR)'   )
         return

      else if ( all(pv == 0.0_wp) ) then

         call ctx%setmsg ( 'The central pole vector supplied to ' &
         //            'SPKE15 had length zero. The most likely ' &
         //            'cause of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         call ctx%sigerr ( 'SPICE(BADVECTOR)'   )
         return

      else if ( rpl < 0.0_wp ) then

         call ctx%setmsg ( 'The central body radius was negative. ' &
         //            'It must be zero or positive.  The value ' &
         //            'supplied was #. '  )
         call ctx%errdp  ( '#', rpl            )
         call ctx%sigerr ( 'SPICE(BADRADIUS)'  )
         return

      end if

!
!     Convert TP, PV and PA to unit vectors.
!     (It won't hurt to polish them up a bit here if they are already
!      unit vectors.)
!
      call vhatip ( pa )
      call vhatip ( tp )
      call vhatip ( pv )

!
!     One final check.  Make sure the pole and periapse vectors are
!     orthogonal. (We will use a very crude check but this should
!     rule out any obvious errors.)
!
      dot = vdot ( pa, tp )

      if ( abs(dot) > 1.0e-5_wp ) then

         angle = vsep ( pa, tp ) * dpr

         call ctx%setmsg ( 'The periapsis and trajectory pole ' &
         //            'vectors are not orthogonal. The angle' &
         //            'between them is # degrees. '         )
         call ctx%errdp  ( '#',     angle        )
         call ctx%sigerr ( 'SPICE(BADINITSTATE)' )
         return

      end if

!
!     Compute the distance and speed at periapse.
!
      near  = p           / ( 1.0_wp + ecc)
      speed = dsqrt(gm/p) * ( 1.0_wp + ecc )

!
!     Next get the position at periapse ...
!
      state0(1:3) = near*pa

!
!     ... and the velocity at periapsis.
!
      call vcrss  ( tp,    pa, state0(4) )
      state0(4:6) = speed*state0(4:6)

!
!     Determine the elapsed time from periapse to the requested
!     epoch and propagate the state at periapsis to the epoch of
!     interest.
!
!     Note that we are making use of the following fact.
!
!        If R is a rotation, then the states obtained by
!        the following blocks of code are mathematically the
!        same. (In reality they may differ slightly due to
!        roundoff.)
!
!        Code block 1.
!
!           CALL MXV   ( R,  STATE0,     STATE0    )
!           CALL MXV   ( R,  STATE0(4),  STATE0(4) )
!           CALL PROP2B( GM, STATE0, DT, STATE     )
!
!        Code block 2.
!
!           CALL PROP2B( GM, STATE0, DT, STATE    )
!           CALL MXV   ( R,  STATE,      STATE    )
!           CALL MXV   ( R,  STATE(4),   STATE(4) )
!
!     This allows us to first compute the propagation of our initial
!     state and then if needed perform the precession of the line
!     of nodes and apsides by simply precessing the resulting state.
!
      dt = et - epoch
      call prop2b ( ctx, gm, state0, dt, state )

!
!     If called for, handle precession needed due to the J2 term.  Note
!     that the motion of the lines of nodes and apsides is formulated
!     in terms of the true anomaly.  This means we need the accumulated
!     true anomaly in order to properly transform the state.
!
      if (       ( j2flg /= 3     ) &
           .and. ( oj2   /= 0.0_wp ) &
           .and. ( ecc   < 1.0_wp ) &
           .and. ( near  > rpl   )  ) then

!
!        First compute the change in mean anomaly since periapsis.
!
         oneme2 = 1.0_wp - ecc**2
         dmdt   = ( oneme2 / p ) * dsqrt( gm * oneme2 / p )
         manom  = dmdt*dt
!
!        Next compute the angle THETA such that THETA is between
!        -pi and pi and such than MANOM = THETA + K*2*pi for
!        some integer K.
!
         theta  = mod( manom, twopi )

         if ( abs(theta)> pi ) then
            theta = theta - sign(twopi,theta)
         end if

         k2pi   = manom - theta
!
!        We can get the accumulated true anomaly from the propagated
!        state theta and the accumulated mean anomaly prior to this
!        orbit.
!
         ta     = vsep ( pa, state )
         ta     = sign ( ta, theta )
         ta     = ta  +  k2pi

!
!        Determine how far the line of nodes and periapsis have moved.
!
         cosinc = vdot(pv,tp)

         z      = ta * 1.5_wp * oj2 *(rpl/p)**2
         dnode  = -z * cosinc
         dperi  =  z * ( 2.5_wp*cosinc**2 - 0.5_wp )

!
!        Precess the periapsis by rotating the state vector about the
!        trajectory pole
!
         if ( j2flg /= 1 ) then

            call vrotv ( state(1), tp, dperi, tmpsta(1) )
            call vrotv ( state(4), tp, dperi, tmpsta(4) )
            state = tmpsta

         end if

!
!        Regress the line of nodes by rotating the state
!        about the pole of the central body.
!
         if ( j2flg /= 2 ) then

            call vrotv ( state,    pv, dnode, tmpsta(1) )
            call vrotv ( state(4), pv, dnode, tmpsta(4) )
            state = tmpsta

         end if

!
!        We could perform the rotations above in the other order,
!        but we would also have to rotate the pole before precessing
!        the line of apsides.
!

      end if

!
!     That's all folks.  return.
!
      return
      end subroutine spke15

      !> Read a single SPK data record from a segment of type 10
      !> (NORAD two line element sets).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE,
      !>     DESCR    are the file handle and segment descriptor for
      !>              a SPK segment of type 10.
      !>
      !>     ET       is a target epoch, for which a data record from
      !>              a specific segment is required.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  It is assumed that the descriptor and handle supplied are
      !>         for a properly constructed type 10 segment. No checks are
      !>         performed to ensure this.
      !>
      !>     2)  If an error occurs while looking up SPK data, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a data type 10 segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in its rawest form,
      !>     taken directly from the segment. As such, it will be meaningless
      !>     to a user unless he/she understands the structure of the data type
      !>     completely. Given that understanding, however, the SPKRxx
      !>     routines might be used to "dump" and check segment data for a
      !>     particular epoch.
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
      !>           IF ( TYPE .EQ. 1 ) THEN
      !>              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated entry
      !>        #2 in $Exceptions section. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.1.0, 09-MAR-2009 (EDW)
      !>
      !>        Removed declaration of unused variable DOINT.
      !>
      !>-    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT)
      module subroutine spkr10 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      real(wp), intent(in) :: descr    ( 5 )  !! Segment descriptor.
      real(wp), intent(in) :: et  !! Target epoch.
      real(wp), intent(out) :: record   ( * )  !! Data record.
      integer(ip), parameter :: nconst = 8
      integer(ip), parameter :: nelems = 10
      integer(ip), parameter :: nangs = 4
      integer(ip), parameter :: begel1 = nconst + 1
      integer(ip), parameter :: begel2 = begel1 + nelems + nangs
      integer(ip), parameter :: enset1 = begel1 + nelems - 1
      integer(ip), parameter :: enset2 = begel2 + nelems - 1
      integer(ip) :: from
      logical :: found
      integer(ip) :: getelm
      integer(ip) :: set1
      integer(ip) :: set2
      integer(ip) :: putelm
      integer(ip) :: i
      integer(ip) :: ends    ( 2 )
      integer(ip) :: indx
      real(wp) :: value
      integer(ip) :: to
      integer(ip) :: nepoch
      if ( ctx%failed() ) return

!
!     Fetch the constants and store them in the first part of
!     the output RECORD.
!
      call sgfcon ( ctx, handle, descr, 1,  nconst, record(1) )

!
!     Locate the time in the file closest to the input ET.
!
      call sgfrvi ( ctx, handle, descr, et, value, indx, found )

!
!     Determine which pair of element sets to choose so that
!     they will bracket ET.
!
      if ( et <= value ) then

         from = max( indx-1, 1 )
         to   = indx

      else

         call sgmeta ( ctx, handle, descr, nref, nepoch )
         from = indx
         to   = min( indx+1, nepoch  )

      end if

!
!     Fetch the element sets
!
      call sgfpkt ( ctx, handle, descr, from, to, record(begel1), ends )
!
!     If the size of the packets is not 14, this is an old style
!     two-line element set without nutation information.  We simply
!     set all of the angles to zero.
!
      if ( ends(1) == nelems ) then
!
!        First shift the elements to their proper locations in RECORD
!        so there will be room to fill in the zeros.
!
         putelm = enset2
         getelm = enset1 + nelems

         do while ( getelm > enset1 )

            record(putelm) = record(getelm)
            putelm         = putelm - 1
            getelm         = getelm - 1

         end do

         set1 = enset1 + 1
         set2 = enset2 + 1

         do i = 1, nangs
            record(set1) = 0.0_wp
            record(set2) = 0.0_wp
            set1         = set1 + 1
            set2         = set2 + 1
         end do

      end if

!
!     If we only got one element set, ET  was either before the
!     first one in the segment or after the last one in the
!     segment.  We simply copy the one fetched a second time so
!     that the record is properly constructed.
!
      if ( from == to ) then
         record(begel2:begel2+nelems + nangs-1) = record(begel1:begel1+nelems + nangs-1)
      end if

      return
      end subroutine spkr10

      !> Read a single data record from a type 14 SPK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of the open SPK file which contains the
      !>              segment of interest.
      !>
      !>     DESCR    is the descriptor for a type 14 SPK segment that contains
      !>              the record of interest.
      !>
      !>     ET       is the target epoch used to determine the particular
      !>              record to be obtained from the SPK segment.
      !>
      !>## Detailed Output
      !>
      !>     RECORD   is the record from the specified segment which,
      !>              when evaluated at epoch ET, will give the state
      !>              (position and velocity) of some body, relative
      !>              to some center, in some inertial reference frame.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  It is assumed that the descriptor and handle supplied are
      !>         for a properly constructed type 14 segment. No checks are
      !>         performed to ensure this.
      !>
      !>     2)  If the input ET value is not within the range specified
      !>         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
      !>         is signaled.
      !>
      !>     3)  If any other error occurs while looking up SPK data, the error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     This subroutine will read a single record from a type 14 SPK
      !>     segment. The record read will provide the data necessary to
      !>     compute the state for a some body in some inertial frame at epoch
      !>     ET.
      !>
      !>     See the SPK Required Reading file for a description of the
      !>     structure of a type 14 SPK segment.
      !>
      !>## Examples
      !>
      !>     The data returned by the SPKRnn routine is in a raw form, taken
      !>     directly from the segment. As such, it will be not be directly
      !>     useful to a user unless they have a complete understanding of the
      !>     structure of the data type. Given that understanding, however,
      !>     the SPKRnn routines could be used to "dump" and check segment data
      !>     for a particular epoch, as in the example which follows.
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
      !>           IF ( TYPE .EQ. 14 ) THEN
      !>              CALL SPKR14 ( HANDLE, DESCR, ET, RECORD )
      !>                  .
      !>                  .  Look at the RECORD data.
      !>                  .
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine should not be called directly by a casual
      !>         user. It is intended for use by the subroutine SPKPV, and
      !>         certain tests for error conditions are not performed here, as
      !>         SPKPV will have already performed them.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated entry
      !>        #3 in $Exceptions section. Moved SPK required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 10-MAR-1995 (KRG)
      module subroutine spkr14 ( ctx, handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of the open SPK file.
      real(wp), intent(in) :: descr    ( * )  !! Descriptor of the segment with the desired record.
      real(wp), intent(in) :: et  !! Epoch used to identify the desired record.
      real(wp), intent(out) :: record   ( * )  !! The desired type 14 SPK record.
      integer(ip) :: ends_1(1)  !! one-element copy of ends (array argument)

!
!     The beginning location in the output record for the non constant
!     segment data.
!

      real(wp) :: value

      integer(ip) :: ends
      integer(ip) :: indx

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Check the request time against the time bounds in the segment
!     descriptor. In order to get the right data back from the generic
!     segment calls below, we need to be sure that the desired epoch
!     falls within the bounds of the segment, as specified by the
!     descriptor. The first two elements of the descriptor are the start
!     time for the segment and the stop time for the segment,
!     respectively.
!
      if ( ( et < descr(1) )  .or.  ( et > descr(2) ) )  then

         call ctx%setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call ctx%errdp ( '#',  et                                      )
         call ctx%errdp ( '#',  descr(1)                                )
         call ctx%errdp ( '#',  descr(2)                                )
         call ctx%sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if
!
!     Fetch the constants and store them in the first part of
!     the output RECORD.
!
      call sgfcon ( ctx, handle, descr, 1, nconst, record(1) )
!
!     Locate the time in the file less than or equal to the input ET,
!     obtaining its index. This will allow us to retrieve the proper
!     record.
!
      call sgfrvi ( ctx, handle, descr, et, value, indx, found )
!
!     Fetch the appropriate record from the segment.
!
      call sgfpkt ( ctx, handle, descr, indx, indx, record(begel1), ends_1 )
      ends = ends_1(1)

      return

      end subroutine spkr14

      !> Evaluate pointing data from a segment for a given time.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of the binary CK file containing the
      !>              desired segment. The file should have been opened
      !>              for read access, either by CKLPF or DAFOPR.
      !>
      !>     DESCR    is the packed descriptor of the segment.
      !>
      !>     SCLKDP   is the encoded spacecraft clock time for which
      !>              pointing is desired.
      !>
      !>     TOL      is a time tolerance, measured in the same units as
      !>              encoded spacecraft clock. The C-matrix returned by
      !>              CKPFS is the one whose time is closest to SCLKDP and
      !>              within TOL units of SCLKDP.
      !>
      !>     NEEDAV   is .TRUE. when angular velocity data is requested.
      !>
      !>## Detailed Output
      !>
      !>     CMAT     is a rotation matrix that transforms the components of
      !>              of a vector expressed in the reference frame given in
      !>              the segment to components expressed in the instrument
      !>              fixed frame at time CLKOUT.
      !>
      !>              Thus, if a vector v has components x, y, z in the
      !>              CK base frame, then v has components x', y', z' in
      !>              the instrument fixed frame at time CLKOUT:
      !>
      !>                   [ x' ]     [          ] [ x ]
      !>                   | y' |  =  |   CMAT   | | y |
      !>                   [ z' ]     [          ] [ z ]
      !>
      !>              If the x', y', z' components are known, use the
      !>              transpose of the C-matrix to determine x, y, z as
      !>              follows.
      !>
      !>                   [ x ]      [          ]T    [ x' ]
      !>                   | y |  =   |   CMAT   |     | y' |
      !>                   [ z ]      [          ]     [ z' ]
      !>                           (Transpose of CMAT)
      !>
      !>     AV       is the angular velocity vector. This is returned only
      !>              if it has been requested, as indicated by NEEDAV. In
      !>              other words, if NEEDAV is .TRUE., then the pointing
      !>              records in the segment must contain AV data.
      !>
      !>              The angular velocity vector is the right-handed axis
      !>              about which the reference frame tied to the instrument
      !>              is instantaneously rotating at time CLKOUT. The
      !>              magnitude of AV is the magnitude of the instantaneous
      !>              velocity of the rotation, in radians per second.
      !>
      !>              The components of AV are given relative to the
      !>              reference frame specified in the segment descriptor.
      !>
      !>     CLKOUT   is the encoded spacecraft clock time associated with
      !>              the returned C-matrix and, optionally, the returned
      !>              angular velocity vector.
      !>
      !>     FOUND    is .TRUE. if a C-matrix and an angular velocity vector
      !>              (if requested) were found to satisfy the pointing
      !>              request. FOUND will be false otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the data type of the segment is not one of those supported
      !>         by this routine, the error SPICE(CKUNKNOWNDATATYPE) is
      !>         signaled.
      !>
      !>     2)  If the specified handle does not belong to any file that is
      !>         currently known to be open, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     3)  If DESCR is not a valid, packed descriptor of a segment in
      !>         the CK file specified by HANDLE, the results of this routine
      !>         are unpredictable.
      !>
      !>     4)  If TOL is negative, FOUND is .FALSE.
      !>
      !>     5)  If NEEDAV is .TRUE., but the segment doesn't contain AV data,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The structure of this routine is just a big case statement. Each
      !>     segment data type is supported by two routines:
      !>
      !>        CKRnn   which reads a single logical pointing record from a
      !>                segment of type nn. (A logical record is defined as
      !>                a collection of numbers sufficient to determine the
      !>                C-matrix, and optionally the angular velocity vector,
      !>                at the input time.)
      !>
      !>        CKEnn   which evaluates the pointing record returned by CKRnn
      !>                to give the C-matrix and optionally the angular
      !>                velocity vector at the input time.
      !>
      !>     The data type is determined from the segment descriptor, and the
      !>     appropriate routines are called.
      !>
      !>## Examples
      !>
      !>     CKPFS allows you to be more selective than CKGP or CKGPAV about
      !>     choosing segments to satisfy CK pointing requests.
      !>
      !>     Suppose MOC.BC is a CK file consisting of several segments
      !>     containing Mars Observer Camera pointing data. Each segment
      !>     covers the same time period, but produces different pointing
      !>     values (one segment may contain predict values, another may
      !>     contain telemetry-based values, and others may contain different
      !>     corrected versions).
      !>
      !>     The following code fragment shows how different the results are
      !>     for each segment. The program steps through the file segment by
      !>     segment and requests pointing for the same time from each
      !>     segment. The results are printed to the screen.
      !>
      !>     GETIME is an imaginary routine used to get an encoded SCLK time
      !>     (SCLKDP) and time tolerance from the user.
      !>
      !>           SC     = -94
      !>           INST   = -94001
      !>           NEEDAV = .TRUE.
      !>
      !>           CALL CKLPF ( 'MOC.BC', HANDLE )
      !>
      !>           CALL GETIME ( SCLKDP, TOL, QUIT )
      !>
      !>     C
      !>     C     For each time, begin a forward search through the file, and
      !>     C     for each segment found, get its descriptor, identifier, and
      !>     C     evaluate the pointing.
      !>     C
      !>           DO WHILE ( .NOT. QUIT )
      !>
      !>              CALL DAFBFS ( HANDLE )
      !>              CALL DAFFNA ( FOUND  )
      !>
      !>              DO WHILE ( FOUND )
      !>
      !>                 CALL DAFGS ( DESCR )
      !>                 CALL DAFGN ( IDENT )
      !>
      !>                 CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL,   NEEDAV,
      !>          .                   CMAT,   AV,    CLKOUT, PFOUND         )
      !>
      !>                 IF ( PFOUND ) THEN
      !>                    WRITE (*,*) 'Segment:          ', IDENT
      !>                    WRITE (*,*) 'C-Matrix:         ', CMAT
      !>                    WRITE (*,*) 'Angular velocity: ', AV
      !>
      !>                 ELSE
      !>                    CALL SCDECD ( SC, SCLKDP, SCLKCH )
      !>                    WRITE (*,*) 'Data not found at time ', SCLKCH
      !>
      !>                 END IF
      !>
      !>                 CALL DAFFNA ( FOUND )
      !>
      !>              END DO
      !>
      !>              CALL GETIME ( SCLKDP, TOL, QUIT )
      !>
      !>           END DO
      !>
      !>## Restrictions
      !>
      !>     1)  A C-kernel file should have been loaded by either CKLPF
      !>         or DAFOPR.
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
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 6.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB)
      !>
      !>        Bug fix: this routine now sets the output FOUND to
      !>        .FALSE. if a SPICE error is detected.
      !>
      !>        The routine was updated to handle data type 6 segments.
      !>        Several comment typos were corrected.
      !>
      !>-    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB)
      !>
      !>        The routine was updated to handle data type 5 segments.
      !>
      !>-    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS)
      !>
      !>        The routine was updated to handle data type 4 segments.
      !>        The RECSIZ size parameter was eliminated. The dimension
      !>        of the RECORD buffer is now defined by the CKMRSZ parameter
      !>        specified in the 'ckparam.inc' include file.
      !>
      !>-    SPICELIB Version 3.0.0, 11-SEP-1992 (JML)
      !>
      !>        The routine was updated to handle data type 3 segments.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
      !>
      !>         The routine was updated to handle data type 2 segments.
      !>
      !>         FOUND is now initialized to false.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>         The restriction that a C-kernel file must be loaded
      !>         was explicitly stated.
      !>
      !>-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB)
      !>
      !>        The routine was updated to handle data type 6 segments.
      !>
      !>-    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB)
      !>
      !>        The routine was updated to handle data type 5 segments.
      !>
      !>-    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS)
      !>
      !>        The routine was updated to handle data type 4 segments.
      !>
      !>           a) 'ckparam.inc' include file was included.
      !>
      !>           b) RECSIZ size parameter was eliminated.
      !>
      !>           c) Size of the RECORD was reset to CKMRSZ, parameter
      !>              defined in the 'ckparam.inc' include file.
      !>
      !>           d) Calls to CKR04 and CKE04 were added to the case
      !>              statement.
      !>
      !>-    SPICELIB Version 3.0.0, 11-SEP-1992 (JML)
      !>
      !>        The routine was updated to handle data type 3 segments.
      !>
      !>           a) RECSIZ was increased to 17.
      !>
      !>           b) Calls to CKR03 and CKE03 were added to the case
      !>              statement.
      !>
      !>-    SPICELIB Version 2.0.0, 30-AUG-1991 (JML)
      !>
      !>        1) The routine was updated to handle data type 2 segments.
      !>
      !>        2) FOUND is initialized to false to guard against it being
      !>           left unchanged from its previous value when an error is
      !>           detected.
      !>
      !>-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
      !>
      !>        1) The restriction that a C-kernel file must be loaded
      !>           was explicitly stated.
      !>
      !>-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
      !>
      !>        The following changes were made as a result of the
      !>        NAIF CK Code and Documentation Review:
      !>
      !>        1) The variable SCLK was changed to SCLKDP.
      !>        2) The declarations for the parameters RECSIZ, NDC, and NIC
      !>           were moved from the "Declarations" section of the header
      !>           to the "Local parameters" section of the code below the
      !>           header. These parameters are not meant to modified by
      !>           users.
      !>        3) The header was updated.
      !>        4) The comments in the code were improved.
      !>
      !>-    Beta Version 1.0.0, 07-MAY-1990 (RET) (IMU)
      module subroutine ckpfs ( ctx, handle, descr, sclkdp, tol, needav, cmat, av, clkout, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! CK file handle.
      real(wp), intent(in) :: descr  ( *    )  !! Segment descriptor.
      real(wp), intent(in) :: sclkdp  !! Spacecraft clock time.
      real(wp), intent(in) :: tol  !! Time tolerance.
      logical, intent(in) :: needav  !! .TRUE. when angular velocity data is requested.
      real(wp), intent(out) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp), intent(out) :: av     ( 3    )  !! Angular velocity vector.
      real(wp), intent(out) :: clkout  !! Output spacecraft clock time.
      logical, intent(out) :: found  !! .TRUE. when requested pointing is available.

      integer(ip) :: icd    ( nic    )
      integer(ip) :: type

      real(wp) :: dcd    ( ndc    )
      real(wp) :: record ( ckmrsz )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Start off with FOUND set to false.
!
      found  = .false.

!
!     Upgrading CKPFS to accommodate new data types involves following
!     these steps:
!
!     1)  Write the two new routines CKRnn and CKEnn. (You may need to
!         add or subtract from the arguments used in the existing CKRnn
!         and CKEnn calling sequences, but should not have to change
!         the inputs or outputs to CKPFS.)
!
!     2)  Insert a new case into the code of CKPFS.
!
!     3)  Depending on the size of RECORD returned from CKRnn, modify
!         the parameter RECSIZ.  (You will only need to change it if
!         RECSIZ is not large enough for the new CKRnn's RECORD.)
!

!
!     Unpack the descriptor to see what the data type of the segment is,
!     and call the appropriate read-and-evaluate routines.
!
      call dafus ( ctx, descr, ndc, nic, dcd, icd )

      type = icd( 3 )

      if ( type == 1 ) then

         call ckr01 ( ctx, handle, descr, sclkdp, tol, needav, record, &
                      found )

         if ( found ) then
            call cke01 ( ctx, needav, record, cmat, av, clkout )
         end if

      else  if ( type == 2 ) then

         call ckr02 ( ctx, handle, descr, sclkdp, tol, record, found )

         if ( found ) then
            call cke02 ( ctx, needav, record, cmat, av, clkout )
         end if

      else  if ( type == 3 ) then

         call ckr03 ( ctx,handle, descr, sclkdp, tol, needav, record, found)

         if ( found ) then
            call cke03 ( ctx, needav, record, cmat, av, clkout )
         end if

      else  if ( type == 4 ) then

         call ckr04 ( ctx,handle, descr, sclkdp, tol, needav, record, found)

         if ( found ) then
            call cke04 ( ctx, needav, record, cmat, av, clkout )
         end if

      else  if ( type == 5 ) then

         call ckr05 ( ctx,handle, descr, sclkdp, tol, needav, record, found)

         if ( found ) then
            call cke05 ( ctx, needav, record, cmat, av, clkout )
         end if

      else  if ( type == 6 ) then

         call ckr06 ( ctx,handle, descr, sclkdp, tol, needav, record, found)

         if ( found ) then
            call cke06 ( ctx, needav, record, cmat, av, clkout )
         end if

      else

         call ctx%setmsg ( 'The data type # is not currently supported.' )
         call ctx%errint ( '#', type                                     )
         call ctx%sigerr ( 'SPICE(CKUNKNOWNDATATYPE)'                    )

      end if

!
!     In case an evaluator signaled an error, we check the SPICE
!     error status here. If a SPICE error occurred, indicate no
!     data were found.
!
      if ( ctx%failed() ) then
         found = .false.
      end if

      return
      end subroutine ckpfs

      !> Return the name of an inertial reference frame and the 6 x 6
      !> state transformation matrix from that frame to the body fixed
      !> frame of a given body at a specified epoch.
      !>
      !>## Detailed Input
      !>
      !>     BODY     is the integer ID code of the body for which the
      !>              state transformation matrix is requested. Bodies
      !>              are numbered according to the standard NAIF
      !>              numbering scheme. The numbering scheme is
      !>              explained in the NAIF_IDS required reading file.
      !>
      !>     ET       is the epoch at which the state transformation
      !>              matrix is requested.
      !>
      !>## Detailed Output
      !>
      !>     REF      is the integer code for the inertial reference frame
      !>              of the state transformation matrix TSIPM. (See the
      !>              routine CHGIRF for a full list of inertial reference
      !>              frame names.)
      !>
      !>     TSIPM    is a 6x6 transformation matrix. It is used to
      !>              transform states from inertial coordinates to body
      !>              fixed (also called equator and prime meridian --- PM)
      !>              coordinates.
      !>
      !>              Given a state S in the inertial reference frame
      !>              specified by REF, the corresponding state in the body
      !>              fixed reference frame is given by the matrix vector
      !>              product:
      !>
      !>                 TSIPM * S
      !>
      !>              See the PCK required reading for further details
      !>              concerning PCK reference frames.
      !>
      !>              NOTE: The inverse of TSIPM is NOT its transpose. The
      !>              matrix, TSIPM, has the structure shown below:
      !>
      !>                          -            -
      !>                         |       :      |
      !>                         |   R   :  0   |
      !>                         | ......:......|
      !>                         |       :      |
      !>                         | dR_dt :  R   |
      !>                         |       :      |
      !>                          -            -
      !>
      !>              where R is a time varying rotation matrix and dR_dt
      !>              is its derivative. The inverse of this matrix is:
      !>
      !>                          -              -
      !>                         |     T  :       |
      !>                         |    R   :  0    |
      !>                         | .......:.......|
      !>                         |        :       |
      !>                         |      T :   T   |
      !>                         | dR_dt  :  R    |
      !>                         |        :       |
      !>                          -              -
      !>
      !>              The SPICE routine INVSTM is available for producing
      !>              this inverse.
      !>
      !>      FOUND      if the data allowing the computation of a state
      !>              transformation matrix for the requested time and body
      !>              are found in a binary PCK file, FOUND will have the
      !>              value .TRUE., otherwise it will have the value
      !>              .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the size of the type 20 PCK record to be  retrieved is too
      !>         large to fit into RECORD, the error SPICE(PCKRECTOOLARGE)
      !>         is signaled.
      !>
      !>     2)  If any issue is detected while reading PCK data, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the requested transformation matrix cannot be computed
      !>         using data from loaded binary PCK files, FOUND is returned
      !>         with the value .FALSE. This is not a SPICE error.
      !>
      !>## Files
      !>
      !>     This routine computes transformation matrices using data
      !>     provided by a loaded binary PCK kernel.
      !>
      !>## Particulars
      !>
      !>     The matrix for transforming an inertial state into a body fixed
      !>     states is the 6x6 matrix shown below as a block structured
      !>     matrix.
      !>
      !>                 -            -
      !>                |       :      |
      !>                | TIPM  :  0   |
      !>                | ......:......|
      !>                |       :      |
      !>                | DTIPM : TIPM |
      !>                |       :      |
      !>                 -            -
      !>
      !>     If a binary PCK file record can be found for the time and body
      !>     requested, it will be used. The most recently loaded binary PCK
      !>     file has first priority, followed by previously loaded binary PCK
      !>     files in backward time order. If no binary PCK file has been
      !>     loaded, the text P_constants kernel file is used.
      !>
      !>## Examples
      !>
      !>     Here we load a binary PCK files and use PCKEUL to get the
      !>     Euler angles.
      !>
      !>     C
      !>     C  Load binary PCK file.
      !>     C
      !>        CALL PCKLOF ('example.pck', HANDLE)
      !>
      !>     C  Call routine to get transformation matrix.
      !>
      !>        CALL PCKMAT ( BODY, ET, REF, TIPM, FOUND )
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
      !>     K.R. Gehringer     (JPL)
      !>     E.D. Wright        (JPL)
      !>     K.S. Zukor         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 03-JAN-2014 (NJB) (EDW)
      !>
      !>        Minor edits to $Procedure; clean trailing whitespace.
      !>        Removed unneeded $Revisions section.
      !>
      !>        Updated to support type 20. Changed long error message
      !>        for the case of RECORD having insufficient room: the
      !>        user is no longer advised to modify the record size.
      !>
      !>-    SPICELIB Version 2.0.0, 22-MAR-1995 (KRG) (KSZ)
      !>
      !>        Added PCK type 03. Added a new exception. Made some minor
      !>        comment changes.
      !>
      !>-    SPICELIB Version 1.0.0, 21-MAR-1995 (KSZ)
      !>
      !>        Replaces PCKEUL and returns the transformation
      !>        matrix rather than the Euler angles.
      module subroutine pckmat ( ctx, body, et, ref, tsipm, found )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! ID code of some body.
      real(wp), intent(in) :: et  !! Epoch of transformation.
      integer(ip), intent(out) :: ref  !! Integer code for inertial reference frame.
      real(wp), intent(out) :: tsipm ( 6, 6 )  !! Transformation from Inertial to PM for BODY at ET.
      logical, intent(out) :: found  !! .TRUE. if data for BODY and ET are found.

!     ND and NI values for a PCK file.
!

      integer(ip), parameter :: ni = 5
!
!     Index for the reference frame code in the integer summary.
!
      integer(ip), parameter :: nr = 2
!
!     Length of the descriptor for a PCK file.
!
      integer(ip), parameter :: ns = nd + (ni+1)/2
!
!     Index for the data type code in the integer summary.
!
      integer(ip), parameter :: nt = 3
!
!     Maximum size allowed for a record in a segment of a binary PCK
!     file.
!
      integer(ip), parameter :: maxrec = 130
!
!     Number of components in a state vector.
!
      integer(ip), parameter :: nstate = 6

      character(40) :: ident

      real(wp) :: dcd    (     nd )
      real(wp) :: descr  (     ns )
      real(wp) :: estate ( nstate )
      real(wp) :: eulang ( nstate )
      real(wp) :: record ( maxrec )

      integer(ip) :: handle
      integer(ip) :: icd    (     ni )
      integer(ip) :: recsiz
      integer(ip) :: type

!
!     Standard SPICE Error handling.
!
      if ( ctx%failed() ) return

!
!     Get a segment applicable to a specified body and epoch.
!
      call pcksfs ( ctx, body, et, handle, descr, ident, found )

      if ( ctx%failed() ) then
         found = .false.
         return
      end if

      if ( found ) then

!
!        Look at parts of the descriptor.
!
         call dafus ( ctx, descr, nd, ni, dcd, icd )

         type = icd( nt )
         ref  = icd( nr )

         if ( type == 02 ) then
!
!           Read in Chebyshev coefficients from segment.
!
            call pckr02 ( ctx, handle, descr, et, record )
!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            call pcke02 ( ctx, et, record, eulang )

            if ( ctx%failed() ) then
               found = .false.
               return
            end if
!
!           From the PCK type two file the Euler angles are
!           retrieved in a particular order.  The routine to
!           get the TSIPM matrix from expects them in another
!           order.  Here we change from EULANG to ESTATE, which
!           has this proper order.
!
            estate (1) = eulang (3)
            estate (2) = eulang (2)
            estate (3) = eulang (1)
            estate (4) = eulang (6)
            estate (5) = eulang (5)
            estate (6) = eulang (4)

!
!           Call routine which takes Euler angles to transformation
!           matrix.
!
            call eul2xf ( ctx, estate, 3, 1, 3, tsipm )

            if ( ctx%failed() ) then
               found = .false.
               return
            end if

         else if ( type == 03 ) then
!
!           Fetch the number of Chebyshev coefficients, compute the
!           record size needed, and signal an error if there is not
!           enough storage in RECORD. The number of coefficients is the
!           first constant value in the generic segment.
!
            call sgfcon ( ctx, handle, descr, 1, 1, record(1) )

            if ( ctx%failed() ) then
               found = .false.
               return
            end if

            recsiz = nstate * int(record(1)) + 2

            if ( recsiz > maxrec ) then

               call ctx%setmsg ( 'Storage for # double precision numbers' &
               //            ' is needed for a PCK data record and' &
               //            ' only # locations were available.' &
               //            ' Notify the NAIF' &
               //            ' group of this problem.'                 )
               call ctx%errint ( '#', recsiz                               )
               call ctx%errint ( '#', maxrec                               )
               call ctx%sigerr ( 'SPICE(PCKKRECTOOLARGE)'                  )
               return

            end if

            call pckr03 ( ctx, handle, descr, et, record )
            call pcke03 ( ctx, et, record, tsipm         )

            if ( ctx%failed() ) then
               found = .false.
               return
            end if

         else if ( type == 20 ) then
!
!           Read in Chebyshev coefficients from segment.
!
            call pckr20 ( ctx, handle, descr, et, record )
!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            call pcke20 ( ctx, et, record, eulang )

            if ( ctx%failed() ) then
               found = .false.
               return
            end if
!
!           From the PCK type 20 file the Euler angles are
!           retrieved in a particular order. The routine to
!           get the TSIPM matrix from expects them in another
!           order. Here we change from EULANG to ESTATE, which
!           has this proper order.
!
            estate (1) = eulang (3)
            estate (2) = eulang (2)
            estate (3) = eulang (1)
            estate (4) = eulang (6)
            estate (5) = eulang (5)
            estate (6) = eulang (4)

!
!           Call routine which takes Euler angles to transformation
!           matrix.
!
            call eul2xf ( ctx, estate, 3, 1, 3, tsipm )

            if ( ctx%failed() ) then
               found = .false.
               return
            end if

         else
!
!           If data matching the requested body and time was not
!           found, FOUND is false.
!
            found = .false.

         end if

      end if

      return
      end subroutine pckmat

      !> This subroutine initializes variables for SGP4.
      !>
      !>## Detailed Input
      !>
      !>     GEOPHS   is a collection of 8 geophysical constants needed
      !>              for computing a state. The order of these
      !>              constants must be:
      !>
      !>              GEOPHS(1) = J2 gravitational harmonic for earth
      !>              GEOPHS(2) = J3 gravitational harmonic for earth
      !>              GEOPHS(3) = J4 gravitational harmonic for earth
      !>
      !>              These first three constants are dimensionless.
      !>
      !>              GEOPHS(4) = KE: Square root of the GM for earth where
      !>                          GM is expressed in earth radii cubed per
      !>                          minutes squared.
      !>
      !>              GEOPHS(5) = QO: High altitude bound for atmospheric
      !>                          model in km.
      !>
      !>              GEOPHS(6) = SO: Low altitude bound for atmospheric
      !>                          model in km.
      !>
      !>              GEOPHS(7) = RE: Equatorial radius of the earth in km.
      !>
      !>              GEOPHS(8) = AE: Distance units/earth radius
      !>                          (normally 1)
      !>
      !>              Below are currently recommended values for these
      !>              items:
      !>
      !>                J2 =    1.082616D-3
      !>                J3 =   -2.53881D-6
      !>                J4 =   -1.65597D-6
      !>
      !>              The next item is the square root of GM for the
      !>              earth given in units of earth-radii**1.5/Minute
      !>
      !>                KE =    7.43669161D-2
      !>
      !>              The next two items give the top and
      !>              bottom of the atmospheric drag model
      !>              used by the type 10 ephemeris type.
      !>              Don't adjust these unless you understand
      !>              the full implications of such changes.
      !>
      !>                QO =  120.0D0
      !>                SO =   78.0D0
      !>
      !>              The following is the equatorial radius
      !>              of the earth as used by NORAD in km.
      !>
      !>                ER = 6378.135D0
      !>
      !>              The value of AE is the number of
      !>              distance units per earth radii used by
      !>              the NORAD state propagation software.
      !>              The value should be 1 unless you've got
      !>              a very good understanding of the NORAD
      !>              routine SGP4 and the affect of changing
      !>              this value..
      !>
      !>                AE =    1.0D0
      !>
      !>     ELEMS    is an array containing two-line element data
      !>              as prescribed below. The elements XNDD6O and BSTAR
      !>              must already be scaled by the proper exponent stored
      !>              in the two line elements set. Moreover, the
      !>              various items must be converted to the units shown
      !>              here.
      !>
      !>                 ELEMS (  1 ) = XNDT2O in radians/minute**2
      !>                 ELEMS (  2 ) = XNDD6O in radians/minute**3
      !>                 ELEMS (  3 ) = BSTAR
      !>                 ELEMS (  4 ) = XINCL  in radians
      !>                 ELEMS (  5 ) = XNODEO in radians
      !>                 ELEMS (  6 ) = EO
      !>                 ELEMS (  7 ) = OMEGAO in radians
      !>                 ELEMS (  8 ) = XMO    in radians
      !>                 ELEMS (  9 ) = XNO    in radians/minute
      !>                 ELEMS ( 10 ) = EPOCH of the elements in seconds
      !>                                past ephemeris epoch J2000.
      !>
      !>     OPMODE   Flag indicating which technique
      !>              to use to calculate sidereal time.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If radius of perigee has value less-than 1, the error
      !>         SPICE(SUBORBITAL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is based on the SGP4INIT code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3".
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        1) Implemented error checks using SPICE error subsystem.
      !>           On detecting an error, control returns to the calling
      !>           routine. This behavior differs from the original
      !>           version.
      !>
      !>        2) Comments prefixed with "SGP4FIX" indicate a comment
      !>           from the code by Vallado et. al concerning a correction
      !>           to the STR#3 code.
      !>
      !>        3) Eliminated the use of COMMON blocks.
      !>
      !>        Removed getgravconst call, replaced with GEOPHS array.
      !>
      !>        xBStar,
      !>        xEcco,
      !>        Epoch,
      !>        xArgpo,
      !>        xInclo,
      !>        xMo,
      !>        xNo,
      !>        xnodeo replaced with ELEMS array.
      !>        radiusearthkm replaced with ER
      !>
      !>## Examples
      !>
      !>     Refer to $Examples section in ZZSGP4.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>         Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>         U.S. Air Force: Aerospace Defense Command.
      !>
      !>     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>         of Space Command Element Sets." Space Command,
      !>         U. S. Air Force, CO.
      !>
      !>     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>         History of Analytical Orbit Modeling in the U. S. Space
      !>         Surveillance System. Journal of Guidance, Control, and
      !>         Dynamics. 27(2):174-185.
      !>
      !>     [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>         Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR)
      !>
      !>        Correction of documentation error in listing of GEOPHS
      !>        constants. The definition of index 5, "High altitude bound
      !>        for atmospheric model in km.", and index 6, "Low altitude
      !>        bound for atmospheric model in km." were switched.
      !>
      !>-    SPICELIB Version 1.0.0, 11-DEC-2014 (EDW)
      !>
      !>        Based on routine SGP4INIT, 28-JUN-2005, Vallado 2006 [4].
      module subroutine xxsgp4i ( ctx, geophs, elems, opmode )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_zzsgp4
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: geophs    ( 8 )  !! Geophysical constants.
      real(wp), intent(in) :: elems     ( 10 )  !! Two-line element data.
      integer(ip), intent(in) :: opmode  !! Flag to indicate operation mode for GMST.
      real(wp) :: ainv
      real(wp) :: am
      real(wp) :: ao
      real(wp) :: argpm
      real(wp) :: argpp
      real(wp) :: axnl
      real(wp) :: aynl
      real(wp) :: betal
      real(wp) :: cc1sq
      real(wp) :: cc2
      real(wp) :: cc3
      real(wp) :: cnod
      real(wp) :: cnodm
      real(wp) :: coef
      real(wp) :: coef1
      real(wp) :: con42
      real(wp) :: cos2u
      real(wp) :: coseo1
      real(wp) :: cosi
      real(wp) :: cosim
      real(wp) :: cosio
      real(wp) :: cosio2
      real(wp) :: cosio4
      real(wp) :: cosip
      real(wp) :: cosisq
      real(wp) :: cosomm
      real(wp) :: cossu
      real(wp) :: cosu
      real(wp) :: day
      real(wp) :: delm
      real(wp) :: delomg
      real(wp) :: dndt
      real(wp) :: eccm
      real(wp) :: eccp
      real(wp) :: eccsq
      real(wp) :: ecose
      real(wp) :: eeta
      real(wp) :: el2
      real(wp) :: emsq
      real(wp) :: eo1
      real(wp) :: epoch
      real(wp) :: esine
      real(wp) :: etasq
      real(wp) :: gam
      real(wp) :: inclm
      real(wp) :: j3
      real(wp) :: j4
      real(wp) :: kps
      real(wp) :: mm
      real(wp) :: mp
      real(wp) :: mr
      real(wp) :: mv
      real(wp) :: nodem
      real(wp) :: nodep
      real(wp) :: omeosq
      real(wp) :: omgadf
      real(wp) :: perige
      real(wp) :: pinvsq
      real(wp) :: pl
      real(wp) :: posq
      real(wp) :: psisq
      real(wp) :: qzms24
      real(wp) :: qzms2t
      real(wp) :: rdotl
      real(wp) :: rl
      real(wp) :: rp
      real(wp) :: rtemsq
      real(wp) :: rteosq
      real(wp) :: rvdot
      real(wp) :: rvdotl
      real(wp) :: s1
      real(wp) :: s2
      real(wp) :: s3
      real(wp) :: s4
      real(wp) :: s5
      real(wp) :: s6
      real(wp) :: s7
      real(wp) :: sfour
      real(wp) :: sin2u
      real(wp) :: sineo1
      real(wp) :: sini
      real(wp) :: sinim
      real(wp) :: sinio
      real(wp) :: sinip
      real(wp) :: sinomm
      real(wp) :: sinsu
      real(wp) :: sinu
      real(wp) :: snod
      real(wp) :: snodm
      real(wp) :: ss
      real(wp) :: ss1
      real(wp) :: ss2
      real(wp) :: ss3
      real(wp) :: ss4
      real(wp) :: ss5
      real(wp) :: ss6
      real(wp) :: ss7
      real(wp) :: su
      real(wp) :: sz1
      real(wp) :: sz11
      real(wp) :: sz12
      real(wp) :: sz13
      real(wp) :: sz2
      real(wp) :: sz21
      real(wp) :: sz22
      real(wp) :: sz23
      real(wp) :: sz3
      real(wp) :: sz31
      real(wp) :: sz32
      real(wp) :: sz33
      real(wp) :: t2
      real(wp) :: t3
      real(wp) :: t4
      real(wp) :: tc
      real(wp) :: tem5
      real(wp) :: temp
      real(wp) :: temp1
      real(wp) :: temp2
      real(wp) :: temp3
      real(wp) :: temp4
      real(wp) :: tempa
      real(wp) :: tempe
      real(wp) :: templ
      real(wp) :: tsi
      real(wp) :: tumin
!     PEPPER: TTRANS declares its time vector with 10 elements.
      real(wp) :: tvec   ( 10 )
      real(wp) :: tzero
      real(wp) :: u
      real(wp) :: ux
      real(wp) :: uy
      real(wp) :: uz
      real(wp) :: vx
      real(wp) :: vy
      real(wp) :: vz
      real(wp) :: x2o3
      real(wp) :: xhdot1
      real(wp) :: xinc
      real(wp) :: xincp
      real(wp) :: xl
      real(wp) :: xlm
      real(wp) :: xmdf
      real(wp) :: xmx
      real(wp) :: xmy
      real(wp) :: xn
      real(wp) :: xnoddf
      real(wp) :: xnode
      real(wp) :: xpidot
      real(wp) :: z1
      real(wp) :: z11
      real(wp) :: z12
      real(wp) :: z13
      real(wp) :: z2
      real(wp) :: z21
      real(wp) :: z22
      real(wp) :: z23
      real(wp) :: z3
      real(wp) :: z31
      real(wp) :: z32
      real(wp) :: z33
      integer(ip) :: iter
      logical :: doinit
      if ( .not. allocated ( ctx%zzsgp4_state ) ) allocate ( ctx%zzsgp4_state )
      associate ( a => ctx%zzsgp4_state%a, &
                  alta => ctx%zzsgp4_state%alta, &
                  altp => ctx%zzsgp4_state%altp, &
                  argpdot => ctx%zzsgp4_state%argpdot, &
                  argpo => ctx%zzsgp4_state%argpo, &
                  atime => ctx%zzsgp4_state%atime, &
                  aycof => ctx%zzsgp4_state%aycof, &
                  bstar => ctx%zzsgp4_state%bstar, &
                  cc1 => ctx%zzsgp4_state%cc1, &
                  cc4 => ctx%zzsgp4_state%cc4, &
                  cc5 => ctx%zzsgp4_state%cc5, &
                  con41 => ctx%zzsgp4_state%con41, &
                  d2 => ctx%zzsgp4_state%d2, &
                  d2201 => ctx%zzsgp4_state%d2201, &
                  d2211 => ctx%zzsgp4_state%d2211, &
                  d3 => ctx%zzsgp4_state%d3, &
                  d3210 => ctx%zzsgp4_state%d3210, &
                  d3222 => ctx%zzsgp4_state%d3222, &
                  d4 => ctx%zzsgp4_state%d4, &
                  d4410 => ctx%zzsgp4_state%d4410, &
                  d4422 => ctx%zzsgp4_state%d4422, &
                  d5220 => ctx%zzsgp4_state%d5220, &
                  d5232 => ctx%zzsgp4_state%d5232, &
                  d5421 => ctx%zzsgp4_state%d5421, &
                  d5433 => ctx%zzsgp4_state%d5433, &
                  dedt => ctx%zzsgp4_state%dedt, &
                  del1 => ctx%zzsgp4_state%del1, &
                  del2 => ctx%zzsgp4_state%del2, &
                  del3 => ctx%zzsgp4_state%del3, &
                  delmo => ctx%zzsgp4_state%delmo, &
                  didt => ctx%zzsgp4_state%didt, &
                  dmdt => ctx%zzsgp4_state%dmdt, &
                  dnodt => ctx%zzsgp4_state%dnodt, &
                  dodeep => ctx%zzsgp4_state%dodeep, &
                  domdt => ctx%zzsgp4_state%domdt, &
                  dosimp => ctx%zzsgp4_state%dosimp, &
                  e3 => ctx%zzsgp4_state%e3, &
                  ecco => ctx%zzsgp4_state%ecco, &
                  ee2 => ctx%zzsgp4_state%ee2, &
                  er => ctx%zzsgp4_state%er, &
                  eta => ctx%zzsgp4_state%eta, &
                  gsto => ctx%zzsgp4_state%gsto, &
                  inclo => ctx%zzsgp4_state%inclo, &
                  irez => ctx%zzsgp4_state%irez, &
                  j2 => ctx%zzsgp4_state%j2, &
                  j3oj2 => ctx%zzsgp4_state%j3oj2, &
                  mdot => ctx%zzsgp4_state%mdot, &
                  mo => ctx%zzsgp4_state%mo, &
                  no => ctx%zzsgp4_state%no, &
                  nodedot => ctx%zzsgp4_state%nodedot, &
                  nodeo => ctx%zzsgp4_state%nodeo, &
                  omgcof => ctx%zzsgp4_state%omgcof, &
                  peo => ctx%zzsgp4_state%peo, &
                  pgho => ctx%zzsgp4_state%pgho, &
                  pho => ctx%zzsgp4_state%pho, &
                  pinco => ctx%zzsgp4_state%pinco, &
                  plo => ctx%zzsgp4_state%plo, &
                  se2 => ctx%zzsgp4_state%se2, &
                  se3 => ctx%zzsgp4_state%se3, &
                  sgh2 => ctx%zzsgp4_state%sgh2, &
                  sgh3 => ctx%zzsgp4_state%sgh3, &
                  sgh4 => ctx%zzsgp4_state%sgh4, &
                  sh2 => ctx%zzsgp4_state%sh2, &
                  sh3 => ctx%zzsgp4_state%sh3, &
                  si2 => ctx%zzsgp4_state%si2, &
                  si3 => ctx%zzsgp4_state%si3, &
                  sinmao => ctx%zzsgp4_state%sinmao, &
                  sl2 => ctx%zzsgp4_state%sl2, &
                  sl3 => ctx%zzsgp4_state%sl3, &
                  sl4 => ctx%zzsgp4_state%sl4, &
                  svmode => ctx%zzsgp4_state%svmode, &
                  t2cof => ctx%zzsgp4_state%t2cof, &
                  t3cof => ctx%zzsgp4_state%t3cof, &
                  t4cof => ctx%zzsgp4_state%t4cof, &
                  t5cof => ctx%zzsgp4_state%t5cof, &
                  x1mth2 => ctx%zzsgp4_state%x1mth2, &
                  x7thm1 => ctx%zzsgp4_state%x7thm1, &
                  xfact => ctx%zzsgp4_state%xfact, &
                  xgh2 => ctx%zzsgp4_state%xgh2, &
                  xgh3 => ctx%zzsgp4_state%xgh3, &
                  xgh4 => ctx%zzsgp4_state%xgh4, &
                  xh2 => ctx%zzsgp4_state%xh2, &
                  xh3 => ctx%zzsgp4_state%xh3, &
                  xi2 => ctx%zzsgp4_state%xi2, &
                  xi3 => ctx%zzsgp4_state%xi3, &
                  xke => ctx%zzsgp4_state%xke, &
                  xl2 => ctx%zzsgp4_state%xl2, &
                  xl3 => ctx%zzsgp4_state%xl3, &
                  xl4 => ctx%zzsgp4_state%xl4, &
                  xlamo => ctx%zzsgp4_state%xlamo, &
                  xlcof => ctx%zzsgp4_state%xlcof, &
                  xli => ctx%zzsgp4_state%xli, &
                  xmcof => ctx%zzsgp4_state%xmcof, &
                  xni => ctx%zzsgp4_state%xni, &
                  xnodcf => ctx%zzsgp4_state%xnodcf, &
                  zmol => ctx%zzsgp4_state%zmol, &
                  zmos => ctx%zzsgp4_state%zmos )
      if ( ctx%failed() ) return

!
!     Initialize.
!
      dodeep = .false.
      dosimp = .false.
      svmode = opmode

!
!     This code block replaces the call:
!
!     sgp4fix - note the following variables are also passed directly
!     via sgp4 common. It is possible to streamline the XXSGP4I call
!     by deleting the "x" variables, but the user would need to set
!     the common values first. we include the additional assignment
!     in case twoline2rv is not used.
!
!        bstar  = xbstar
!        ecco   = xecco
!        argpo  = xargpo
!        inclo  = xinclo
!        mo     = xmo
!        no     = xno
!        nodeo  = xnodeo

      bstar = elems ( kbstar )
      inclo = elems ( kincl  )
      nodeo = elems ( knode0 )
      ecco  = elems ( kecc   )
      argpo = elems ( komega )
      mo    = elems ( kmo    )
      no    = elems ( kno    )

!
!       Remember that sgp4 uses units of days from 0 jan 1950
!       (sgp4epoch) and minutes from the epoch (time)
!
!       2433281.5 JD TDB = 1949-12-31 00:00:00.000000 TDB
!       2400000.5 JD TDB = 1858-11-17 00:00:00.000000 TDB
!
!       2433281.5 - 2400000.5 = 33281.0
!

!
!     Convert the J2000 TDB representation of the epoch to
!     JD UTC then calculate the offset from the JD 2433281.5 UTC
!     reference.
!

      tvec(1) = elems ( kepoch )
      call ttrans ( ctx, 'TDB', 'JDUTC', tvec )
      epoch = tvec(1) - 2433281.5_wp

      if ( ctx%failed() ) return

!
!     This code block replaces the call:
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      j2    = geophs(k_j2)
      j3    = geophs(k_j3)
      j4    = geophs(k_j4)
      er    = geophs(k_er)
      xke   = geophs(k_ke)
      tumin = 1.0_wp/geophs(k_ke)
      j3oj2 = j3/j2

!
!     The following assignment and IF block is taken
!     from TWOLINE2RVSGP4.
!
      a = (no*tumin)**(-2.0_wp/3.0_wp)

      if (dabs(ecco-1.0_wp) > 0.000001_wp) then
         altp= (a*(1.0_wp-ecco))-1.0_wp
         alta= (a*(1.0_wp+ecco))-1.0_wp
      else
         alta= 999999.9_wp
         altp= 2.0_wp* (4.0_wp/(no*no)**(1.0_wp/3.0_wp))
      end if

      ss     = 78.0_wp/er + 1.0_wp
      qzms2t = ((120.0_wp-78.0_wp)/er) ** 4
      x2o3   =  2.0_wp / 3.0_wp

!
!     sgp4fix divisor for divide by zero check on inclination
!     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared
!     it to 1.5D-12, so the threshold was changed to 1.5D-12 for
!     consistency.
!
        temp4 =   1.5e-12_wp
        tzero = 0.0_wp
        doinit= .true.

        call zzinil( ctx, geophs, opmode, ecco,  epoch, inclo,  no, &
                     ainv,   ao,     con41, con42, cosio,  cosio2, &
                     eccsq,  omeosq, posq,  rp,    rteosq, sinio, &
                     gsto )

        if ( ctx%failed() ) return

!
!       Check RP for a reasonable value. The propagator may not
!       calculate correct state values for RP < 1.
!
        if (rp < 1.0_wp) then

           call ctx%setmsg ( 'TLE elements suborbital.'     )
           call ctx%sigerr ( 'SPICE(SUBORBITAL)'            )
           return

        end if

!
!       If nodeo and No are gtr 0
!
        if( (omeosq >= 0.0_wp) .or. (no >= 0.0_wp) ) then

            dosimp = .false.

            if (rp < (220.0_wp/er+1.0_wp)) then
                dosimp = .true.
            end if

            sfour  = ss
            qzms24 = qzms2t
            perige = (rp-1.0_wp)*er

!
!           For perigees below 156 km, S and Qoms2t are altered.
!
            if(perige < 156.0_wp) then
                sfour = perige-78.0_wp

                if(perige <= 98.0_wp) then
                    sfour = 20.0_wp
                end if

                qzms24 = ( (120.0_wp-sfour)/er )**4
                sfour  = sfour/er + 1.0_wp

            end if

            pinvsq = 1.0_wp/posq

            tsi    = 1.0_wp/(ao-sfour)
            eta    = ao*ecco*tsi
            etasq  = eta*eta
            eeta   = ecco*eta
            psisq  = dabs(1.0_wp-etasq)
            coef   = qzms24*tsi**4
            coef1  = coef/psisq**3.5_wp
            cc2    = coef1*no* (ao* (1.0_wp+1.5_wp*etasq+eeta* &
                     (4.0_wp+etasq) )+0.375_wp* &
               j2*tsi/psisq*con41*(8.0_wp+3.0_wp*etasq*(8.0_wp+etasq)))
            cc1    = bstar*cc2
            cc3    = 0.0_wp

            if(ecco > 1.0e-4_wp) then
                cc3 = -2.0_wp*coef*tsi*j3oj2*no*sinio/ecco
            end if

            x1mth2 = 1.0_wp-cosio2
            cc4    = 2.0_wp*no*coef1*ao*omeosq*(eta*(2.0_wp+0.5_wp*etasq) &
                    +ecco*(0.5_wp + 2.0_wp*etasq) - j2*tsi / (ao*psisq)* &
                    (-3.0_wp*con41*(1.0_wp-2.0_wp* &
             eeta+etasq*(1.5_wp-0.5_wp*eeta))+0.75_wp*x1mth2*(2.0_wp*etasq &
             -eeta*(1.0_wp+etasq))*dcos(2.0_wp*argpo)))
            cc5    = 2.0_wp*coef1*ao*omeosq* (1.0_wp + 2.75_wp* &
                     (etasq + eeta) + eeta*etasq )
            cosio4 = cosio2*cosio2
            temp1  = 1.5_wp*j2*pinvsq*no
            temp2  = 0.5_wp*temp1*j2*pinvsq
            temp3  = -0.46875_wp*j4*pinvsq*pinvsq*no
            mdot   = no + 0.5_wp*temp1*rteosq*con41 + 0.0625_wp*temp2* &
                     rteosq*(13.0_wp - 78.0_wp*cosio2 + 137.0_wp*cosio4)
            argpdot= -0.5_wp*temp1*con42 + 0.0625_wp*temp2* &
                     (7.0_wp - 114.0_wp*cosio2 + &
              395.0_wp*cosio4)+temp3*(3.0_wp-36.0_wp*cosio2+49.0_wp*cosio4)
            xhdot1 = -temp1*cosio
            nodedot = xhdot1+(0.5_wp*temp2*(4.0_wp-19.0_wp*cosio2)+ &
                       2.0_wp*temp3*(3.0_wp - 7.0_wp*cosio2))*cosio
            xpidot = argpdot+nodedot
            omgcof = bstar*cc3*dcos(argpo)
            xmcof  = 0.0_wp

            if(ecco > 1.0e-4_wp) then
                xmcof = -x2o3*coef*bstar/eeta
            end if

            xnodcf = 3.5_wp*omeosq*xhdot1*cc1
            t2cof  = 1.5_wp*cc1

!
!           sgp4fix for divide by zero with xinco = 180 deg.
!
            if (dabs(cosio+1.0_wp)> 1.5e-12_wp) then
                xlcof  = -0.25_wp*j3oj2*sinio* &
                         (3.0_wp+5.0_wp*cosio)/(1.0_wp+cosio)
            else
                xlcof  = -0.25_wp*j3oj2*sinio* &
                         (3.0_wp+5.0_wp*cosio)/temp4
            end if

            aycof  = -0.5_wp*j3oj2*sinio
            delmo  = (1.0_wp+eta*dcos(mo))**3
            sinmao = dsin(mo)
            x7thm1 = 7.0_wp*cosio2-1.0_wp

!
!           Deep Space Initialization
!
            if ((twopi/no) >= 225.0_wp) then

                dodeep = .true.
                dosimp = .true.
                tc     = 0.0_wp
                inclm  = inclo

!
!               Common.
!
                call zzdscm ( ctx, epoch, ecco, argpo, tc, inclo, &
                        nodeo,  no, &
                        snodm,  cnodm, sinim, cosim, sinomm, cosomm, &
                        day,    e3,    ee2,   eccm,  emsq,   gam, &
                        peo,    pgho,  pho,   pinco, plo, &
                        rtemsq, se2,   se3,   sgh2,  sgh3,   sgh4, &
                        sh2,    sh3,   si2,   si3,   sl2,    sl3, &
                        sl4,    s1,    s2,    s3,    s4,     s5, &
                        s6,     s7,    ss1,   ss2,   ss3,    ss4, &
                        ss5,    ss6,   ss7,   sz1,   sz2,    sz3, &
                        sz11,   sz12,  sz13,  sz21,  sz22,   sz23, &
                        sz31,   sz32,  sz33,  xgh2,  xgh3,   xgh4, &
                        xh2,    xh3,   xi2,   xi3,   xl2,    xl3, &
                        xl4,    xn,    z1,    z2,    z3,     z11, &
                        z12,    z13,   z21,   z22,   z23,    z31, &
                        z32,    z33,   zmol,  zmos )

!
!               Long period perturbations.
!
                call zzdspr( ctx, opmode, e3, ee2, peo, pgho, pho, &
                        pinco, &
                        plo,  se2,   se3,   sgh2, sgh3, sgh4, &
                        sh2,  sh3,   si2,   si3,  sl2,  sl3, &
                        sl4,  tzero, xgh2,  xgh3, xgh4, xh2, &
                        xh3,  xi2,   xi3,   xl2,  xl3,  xl4, &
                        zmol, zmos,  inclm, doinit, &
                        ecco, inclo, nodeo, argpo, mo )

                argpm  = 0.0_wp
                nodem  = 0.0_wp
                mm     = 0.0_wp

!
!               Initialization
!
                call zzdsin ( ctx, geophs, &
                         cosim,  emsq,  argpo, s1,    s2,    s3, &
                         s4,     s5,    sinim, ss1,   ss2,   ss3, &
                         ss4,    ss5,   sz1,   sz3,   sz11,  sz13, &
                         sz21,   sz23,  sz31,  sz33,  tzero, tc, &
                         gsto,   mo,    mdot,  no,    nodeo, nodedot, &
                         xpidot, z1,    z3,    z11,   z13,   z21, &
                         z23,    z31,   z33,   ecco,  eccsq, &
                         eccm,   argpm, inclm, mm,    xn,    nodem, &
                         irez,   atime, d2201, d2211, d3210, d3222, &
                         d4410,  d4422, d5220, d5232, d5421, d5433, &
                         dedt,   didt,  dmdt,  dndt,  dnodt, domdt, &
                         del1,   del2,  del3,  xfact, xlamo, xli, &
                         xni )

            end if

!
!           Set variables if not deep space or rp < 220
!
            if ( .not. dosimp ) then
               cc1sq = cc1*cc1
               d2    = 4.0_wp*ao*tsi*cc1sq
               temp  = d2*tsi*cc1 / 3.0_wp
               d3    = (17.0_wp*ao + sfour) * temp
               d4    = 0.5_wp*temp*ao*tsi* &
                        (221.0_wp*ao + 31.0_wp*sfour)*cc1
               t3cof = d2 + 2.0_wp*cc1sq
               t4cof = 0.25_wp* (3.0_wp*d3+cc1*(12.0_wp*d2+10.0_wp*cc1sq) )
               t5cof = 0.2_wp* (3.0_wp*d4 + 12.0_wp*cc1*d3 + 6.0_wp*d2*d2 + &
                        15.0_wp*cc1sq* (2.0_wp*d2 + cc1sq) )

            end if

         end if

      doinit = .false.

      return
      end associate
      end subroutine xxsgp4i

      !> This procedure is the SGP4 prediction model from Space Command.
      !> This is an updated and combined version of SGP4 and SDP4
      !> originally published separately in Spacetrack report #3 [1].
      !> This version follows the methodology from the 2006 AIAA
      !> Vallado paper [4] describing the history and development of
      !> the code.
      !>
      !>## Detailed Input
      !>
      !>     T        is the time in minutes from the elements epoch
      !>              at which to calculate a state from the
      !>              TLE set.
      !>
      !>## Detailed Output
      !>
      !>     STATE    is the state produced by evaluating the input elements
      !>              at the input epoch T. Units are km and km/sec.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the mean motion is less than 0, the error
      !>         SPICE(BADMEANMOTION) is signaled.0D0.
      !>
      !>     2)  If the mean eccentricity is not bounded by [-0, the error
      !>         SPICE(BADMECCENTRICITY) is signaled.001, 1].
      !>
      !>     3)  If the mean semimajor axis has value less-than ,, the error
      !>         SPICE(BADMSEMIMAJOR) is signaled.95.
      !>
      !>     4)  If the perturbed eccentricity is not bounded by [0, 1], the
      !>         error SPICE(BADPECCENTRICITY) is signaled.
      !>
      !>     5)  If the semi-latus rectum has value less-than 0, the error
      !>         SPICE(BADSEMILATUS) is signaled.
      !>
      !>     6)  If the scaled orbit radial distance has value less than 1, the
      !>         error SPICE(ORBITDECAY) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is based on the SGP4 code by David Vallado
      !>     corresponding to "Revisiting Spacetrack Report #3."
      !>     The intent is to maintain the original Vallado algorithm,
      !>     changing code only to meet NAIF format standards and to
      !>     integrate with SPICELIB.
      !>
      !>        1) Implemented error checks using SPICE error subsystem.
      !>           On detecting an error, control returns to the calling
      !>           routine. This behavior differs from the original
      !>           version.
      !>
      !>        2) Comments prefixed with "SGP4FIX" indicate a comment
      !>           from the code by Vallado et. al concerning a correction
      !>           to the STR#3 code.
      !>
      !>        3) Eliminated the use of COMMON blocks.
      !>
      !>        Removed getgravconst call, replaced with GEOPHS array.
      !>
      !>        Capitalize all variables.
      !>
      !>        radiusearthkm replaced with ER
      !>        VKmPerSec     replaced with KPS
      !>        r, v          replaced with STATE
      !>        method        replaced with DODEEP
      !>        isimp         replaced with DOSIMP
      !>        Error         eliminated
      !>        whichconst    eliminated, function provided by GEOPHS
      !>
      !>## Examples
      !>
      !>     Refer to $Examples section in ZZSGP4.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
      !>         Propagation of the NORAD Element Sets." Spacetrack Report #3.
      !>         U.S. Air Force: Aerospace Defense Command.
      !>
      !>     [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
      !>         of Space Command Element Sets." Space Command,
      !>         U. S. Air Force, CO.
      !>
      !>     [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
      !>         History of Analytical Orbit Modeling in the U. S. Space
      !>         Surveillance System. Journal of Guidance, Control, and
      !>         Dynamics. 27(2):174-185.
      !>
      !>     [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
      !>         and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
      !>         AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
      !>         Specialist Conference, August 21-24, 2006. Keystone, CO.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 30-MAY-2021 (BVS) (JDR)
      !>
      !>        Added missing 'D' to literal DP constants.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 15-SEP-2014 (EDW)
      !>
      !>        Based on routine SGP4, 28-JUN-2005 [4].
      module subroutine xxsgp4e ( ctx, t, state )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: pi, twopi
      use pepper_inc_zzsgp4
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: t  !! Time to evaluate state in minutes from epoch.
      real(wp), intent(out) :: state     ( 6 )  !! Evaluated state.
      real(wp) :: ainv
      real(wp) :: am
      real(wp) :: ao
      real(wp) :: argpm
      real(wp) :: argpp
      real(wp) :: axnl
      real(wp) :: aynl
      real(wp) :: betal
      real(wp) :: cc1sq
      real(wp) :: cc2
      real(wp) :: cc3
      real(wp) :: cnod
      real(wp) :: cnodm
      real(wp) :: coef
      real(wp) :: coef1
      real(wp) :: con42
      real(wp) :: cos2u
      real(wp) :: coseo1
      real(wp) :: cosi
      real(wp) :: cosim
      real(wp) :: cosio
      real(wp) :: cosio2
      real(wp) :: cosio4
      real(wp) :: cosip
      real(wp) :: cosisq
      real(wp) :: cosomm
      real(wp) :: cossu
      real(wp) :: cosu
      real(wp) :: day
      real(wp) :: delm
      real(wp) :: delomg
      real(wp) :: dndt
      real(wp) :: eccm
      real(wp) :: eccp
      real(wp) :: eccsq
      real(wp) :: ecose
      real(wp) :: eeta
      real(wp) :: el2
      real(wp) :: emsq
      real(wp) :: eo1
      real(wp) :: epoch
      real(wp) :: esine
      real(wp) :: etasq
      real(wp) :: gam
      real(wp) :: inclm
      real(wp) :: j3
      real(wp) :: j4
      real(wp) :: kps
      real(wp) :: mm
      real(wp) :: mp
      real(wp) :: mr
      real(wp) :: mv
      real(wp) :: nodem
      real(wp) :: nodep
      real(wp) :: omeosq
      real(wp) :: omgadf
      real(wp) :: perige
      real(wp) :: pinvsq
      real(wp) :: pl
      real(wp) :: posq
      real(wp) :: psisq
      real(wp) :: qzms24
      real(wp) :: qzms2t
      real(wp) :: rdotl
      real(wp) :: rl
      real(wp) :: rp
      real(wp) :: rtemsq
      real(wp) :: rteosq
      real(wp) :: rvdot
      real(wp) :: rvdotl
      real(wp) :: s1
      real(wp) :: s2
      real(wp) :: s3
      real(wp) :: s4
      real(wp) :: s5
      real(wp) :: s6
      real(wp) :: s7
      real(wp) :: sfour
      real(wp) :: sin2u
      real(wp) :: sineo1
      real(wp) :: sini
      real(wp) :: sinim
      real(wp) :: sinio
      real(wp) :: sinip
      real(wp) :: sinomm
      real(wp) :: sinsu
      real(wp) :: sinu
      real(wp) :: snod
      real(wp) :: snodm
      real(wp) :: ss
      real(wp) :: ss1
      real(wp) :: ss2
      real(wp) :: ss3
      real(wp) :: ss4
      real(wp) :: ss5
      real(wp) :: ss6
      real(wp) :: ss7
      real(wp) :: su
      real(wp) :: sz1
      real(wp) :: sz11
      real(wp) :: sz12
      real(wp) :: sz13
      real(wp) :: sz2
      real(wp) :: sz21
      real(wp) :: sz22
      real(wp) :: sz23
      real(wp) :: sz3
      real(wp) :: sz31
      real(wp) :: sz32
      real(wp) :: sz33
      real(wp) :: t2
      real(wp) :: t3
      real(wp) :: t4
      real(wp) :: tc
      real(wp) :: tem5
      real(wp) :: temp
      real(wp) :: temp1
      real(wp) :: temp2
      real(wp) :: temp3
      real(wp) :: temp4
      real(wp) :: tempa
      real(wp) :: tempe
      real(wp) :: templ
      real(wp) :: tsi
      real(wp) :: tumin
!     PEPPER: TTRANS declares its time vector with 10 elements.
      real(wp) :: tvec   ( 10 )
      real(wp) :: tzero
      real(wp) :: u
      real(wp) :: ux
      real(wp) :: uy
      real(wp) :: uz
      real(wp) :: vx
      real(wp) :: vy
      real(wp) :: vz
      real(wp) :: x2o3
      real(wp) :: xhdot1
      real(wp) :: xinc
      real(wp) :: xincp
      real(wp) :: xl
      real(wp) :: xlm
      real(wp) :: xmdf
      real(wp) :: xmx
      real(wp) :: xmy
      real(wp) :: xn
      real(wp) :: xnoddf
      real(wp) :: xnode
      real(wp) :: xpidot
      real(wp) :: z1
      real(wp) :: z11
      real(wp) :: z12
      real(wp) :: z13
      real(wp) :: z2
      real(wp) :: z21
      real(wp) :: z22
      real(wp) :: z23
      real(wp) :: z3
      real(wp) :: z31
      real(wp) :: z32
      real(wp) :: z33
      integer(ip) :: iter
      logical :: doinit
      if ( .not. allocated ( ctx%zzsgp4_state ) ) allocate ( ctx%zzsgp4_state )
      associate ( argpdot => ctx%zzsgp4_state%argpdot, &
                  argpo => ctx%zzsgp4_state%argpo, &
                  atime => ctx%zzsgp4_state%atime, &
                  aycof => ctx%zzsgp4_state%aycof, &
                  bstar => ctx%zzsgp4_state%bstar, &
                  cc1 => ctx%zzsgp4_state%cc1, &
                  cc4 => ctx%zzsgp4_state%cc4, &
                  cc5 => ctx%zzsgp4_state%cc5, &
                  con41 => ctx%zzsgp4_state%con41, &
                  d2 => ctx%zzsgp4_state%d2, &
                  d2201 => ctx%zzsgp4_state%d2201, &
                  d2211 => ctx%zzsgp4_state%d2211, &
                  d3 => ctx%zzsgp4_state%d3, &
                  d3210 => ctx%zzsgp4_state%d3210, &
                  d3222 => ctx%zzsgp4_state%d3222, &
                  d4 => ctx%zzsgp4_state%d4, &
                  d4410 => ctx%zzsgp4_state%d4410, &
                  d4422 => ctx%zzsgp4_state%d4422, &
                  d5220 => ctx%zzsgp4_state%d5220, &
                  d5232 => ctx%zzsgp4_state%d5232, &
                  d5421 => ctx%zzsgp4_state%d5421, &
                  d5433 => ctx%zzsgp4_state%d5433, &
                  dedt => ctx%zzsgp4_state%dedt, &
                  del1 => ctx%zzsgp4_state%del1, &
                  del2 => ctx%zzsgp4_state%del2, &
                  del3 => ctx%zzsgp4_state%del3, &
                  delmo => ctx%zzsgp4_state%delmo, &
                  didt => ctx%zzsgp4_state%didt, &
                  dmdt => ctx%zzsgp4_state%dmdt, &
                  dnodt => ctx%zzsgp4_state%dnodt, &
                  dodeep => ctx%zzsgp4_state%dodeep, &
                  domdt => ctx%zzsgp4_state%domdt, &
                  dosimp => ctx%zzsgp4_state%dosimp, &
                  e3 => ctx%zzsgp4_state%e3, &
                  ecco => ctx%zzsgp4_state%ecco, &
                  ee2 => ctx%zzsgp4_state%ee2, &
                  er => ctx%zzsgp4_state%er, &
                  eta => ctx%zzsgp4_state%eta, &
                  gsto => ctx%zzsgp4_state%gsto, &
                  inclo => ctx%zzsgp4_state%inclo, &
                  irez => ctx%zzsgp4_state%irez, &
                  j2 => ctx%zzsgp4_state%j2, &
                  j3oj2 => ctx%zzsgp4_state%j3oj2, &
                  mdot => ctx%zzsgp4_state%mdot, &
                  mo => ctx%zzsgp4_state%mo, &
                  no => ctx%zzsgp4_state%no, &
                  nodedot => ctx%zzsgp4_state%nodedot, &
                  nodeo => ctx%zzsgp4_state%nodeo, &
                  omgcof => ctx%zzsgp4_state%omgcof, &
                  peo => ctx%zzsgp4_state%peo, &
                  pgho => ctx%zzsgp4_state%pgho, &
                  pho => ctx%zzsgp4_state%pho, &
                  pinco => ctx%zzsgp4_state%pinco, &
                  plo => ctx%zzsgp4_state%plo, &
                  se2 => ctx%zzsgp4_state%se2, &
                  se3 => ctx%zzsgp4_state%se3, &
                  sgh2 => ctx%zzsgp4_state%sgh2, &
                  sgh3 => ctx%zzsgp4_state%sgh3, &
                  sgh4 => ctx%zzsgp4_state%sgh4, &
                  sh2 => ctx%zzsgp4_state%sh2, &
                  sh3 => ctx%zzsgp4_state%sh3, &
                  si2 => ctx%zzsgp4_state%si2, &
                  si3 => ctx%zzsgp4_state%si3, &
                  sinmao => ctx%zzsgp4_state%sinmao, &
                  sl2 => ctx%zzsgp4_state%sl2, &
                  sl3 => ctx%zzsgp4_state%sl3, &
                  sl4 => ctx%zzsgp4_state%sl4, &
                  svmode => ctx%zzsgp4_state%svmode, &
                  t2cof => ctx%zzsgp4_state%t2cof, &
                  t3cof => ctx%zzsgp4_state%t3cof, &
                  t4cof => ctx%zzsgp4_state%t4cof, &
                  t5cof => ctx%zzsgp4_state%t5cof, &
                  x1mth2 => ctx%zzsgp4_state%x1mth2, &
                  x7thm1 => ctx%zzsgp4_state%x7thm1, &
                  xfact => ctx%zzsgp4_state%xfact, &
                  xgh2 => ctx%zzsgp4_state%xgh2, &
                  xgh3 => ctx%zzsgp4_state%xgh3, &
                  xgh4 => ctx%zzsgp4_state%xgh4, &
                  xh2 => ctx%zzsgp4_state%xh2, &
                  xh3 => ctx%zzsgp4_state%xh3, &
                  xi2 => ctx%zzsgp4_state%xi2, &
                  xi3 => ctx%zzsgp4_state%xi3, &
                  xke => ctx%zzsgp4_state%xke, &
                  xl2 => ctx%zzsgp4_state%xl2, &
                  xl3 => ctx%zzsgp4_state%xl3, &
                  xl4 => ctx%zzsgp4_state%xl4, &
                  xlamo => ctx%zzsgp4_state%xlamo, &
                  xlcof => ctx%zzsgp4_state%xlcof, &
                  xli => ctx%zzsgp4_state%xli, &
                  xmcof => ctx%zzsgp4_state%xmcof, &
                  xni => ctx%zzsgp4_state%xni, &
                  xnodcf => ctx%zzsgp4_state%xnodcf, &
                  zmol => ctx%zzsgp4_state%zmol, &
                  zmos => ctx%zzsgp4_state%zmos )
      if ( ctx%failed() ) return

!
!     Local constants. Keep compiler ok for warnings on
!     uninitialized variables
!
      x2o3   = 2.0_wp/3.0_wp
      mr     = 0.0_wp
      coseo1 = 1.0_wp
      sineo1 = 0.0_wp

!
!     Set mathematical constants.
!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!

!
!     sgp4fix divisor for divide by zero check on inclination
!     the old check used 1.0D0 + cos(pi-1.0D-9), but then compared it to
!     1.5D-12, so the threshold was changed to 1.5D-12 for consistency.
!
      temp4 =  1.5e-12_wp
      kps   =  er * xke/60.0_wp

!
!     UPDATE FOR SECULAR GRAVITY AND ATMOSPHERIC DRAG
!
      xmdf   = mo + mdot*t
      omgadf = argpo + argpdot*t
      xnoddf = nodeo + nodedot*t
      argpm  = omgadf
      mm     = xmdf
      t2     = t*t
      nodem  = xnoddf + xnodcf*t2
      tempa  = 1.0_wp - cc1*t
      tempe  = bstar*cc4*t
      templ  = t2cof*t2

      if ( .not. dosimp ) then

         delomg = omgcof*t
         delm   = xmcof*(( 1.0_wp+eta*dcos(xmdf) )**3-delmo)
         temp   = delomg + delm
         mm     = xmdf + temp
         argpm  = omgadf - temp
         t3     = t2*t
         t4     = t3*t
         tempa  = tempa - d2*t2 - d3*t3 - d4*t4
         tempe  = tempe + bstar*cc5*(dsin(mm) - sinmao)
         templ  = templ + t3cof*t3 + t4*(t4cof + t*t5cof)

      end if

      xn    = no
      eccm  = ecco
      inclm = inclo

      if (dodeep) then
         tc     = t

         call zzdspc ( ctx,  irez,  d2201, d2211,   d3210, d3222, d4410, &
                        d4422, d5220, d5232,   d5421, d5433, dedt, &
                        del1,  del2,  del3,    didt,  dmdt,  dnodt, &
                        domdt, argpo, argpdot, t,     tc,    gsto, &
                        xfact, xlamo, no,      atime, eccm, &
                        argpm, inclm, xli,     mm,    xni, &
                        nodem, dndt,  xn  )

      end if

!
!     Mean motion less than 0.0.
!
      if(xn <= 0.0_wp) then

         call ctx%setmsg ( 'Mean motion less-than zero. ' &
          //           'This error may indicate a bad TLE set.'     )
         call ctx%sigerr ( 'SPICE(BADMEANMOTION)'                       )
         return

      end if

      am   = (xke/xn)**x2o3*tempa**2
      xn   = xke/am**1.5_wp
      eccm = eccm-tempe

!
!     Fix tolerance for error recognition. Vallado code used
!     a lower limit of -0.001. This value apparently prevents
!     an error signal due to roundoff error.
!
      if (eccm >= 1.0_wp .or. eccm < -0.001_wp) then

         call ctx%setmsg ( 'Mean eccentricity value, #, beyond ' &
          //           'allowed bounds [-0.001,1.0). This ' &
          //           'error may indicate a bad TLE set.'          )
         call ctx%errdp  ( '#', eccm                                    )
         call ctx%sigerr ( 'SPICE(BADMECCENTRICITY)'                     )
         return

      end if

      if ( am < 0.95_wp ) then

         call ctx%setmsg ( 'Mean semi-major axis value, #, below allowed ' &
          //           'minimum of 0.95. This error may indicate ' &
          //           'a bad TLE set or a decayed orbit.'          )
         call ctx%errdp  ( '#', eccm                                    )
         call ctx%sigerr ( 'SPICE(BADMSEMIMAJOR)'                        )
         return

      end if

!
!     sgp4fix change test condition for eccentricity
!
      if (eccm < 1.0e-6_wp) eccm = 1.0e-6_wp

      mm     = mm+no*templ
      xlm    = mm+argpm+nodem
      emsq   = eccm*eccm
      temp   = 1.0_wp - emsq
      nodem  = dmod( nodem, twopi)
      argpm  = dmod( argpm, twopi)
      xlm    = dmod( xlm,   twopi)
      mm     = dmod( xlm - argpm - nodem, twopi)

!
!     Compute extra mean quantities
!
      sinim  = dsin(inclm)
      cosim  = dcos(inclm)

!
!     Add lunar-solar periodics
!
      eccp   = eccm
      xincp  = inclm
      argpp  = argpm
      nodep  = nodem
      mp     = mm
      sinip  = sinim
      cosip  = cosim

!
!     Use deep space perturbation if indicated.
!

      if (dodeep) then

         call zzdspr ( ctx, svmode, e3,  ee2,   peo,    pgho, pho, pinco, &
                       plo,  se2,   se3,   sgh2,   sgh3, sgh4, &
                       sh2,  sh3,   si2,   si3,    sl2,  sl3, &
                       sl4,  t,     xgh2,  xgh3,   xgh4, xh2, &
                       xh3,  xi2,   xi3,   xl2,    xl3,  xl4, &
                       zmol, zmos,  inclo, .false., &
                       eccp, xincp, nodep, argpp, mp )

         if(xincp < 0.0_wp) then
            xincp  = -xincp
            nodep  = nodep + pi
            argpp  = argpp - pi
         end if

         if ( (eccp < 0.0_wp) .or. &
              (eccp > 1.0_wp) ) then

            call ctx%setmsg ( 'Perturbed eccentricity value, #, beyond ' &
             //           'allowed bounds [0,1]. This error may ' &
             //           'indicate a bad TLE set.'                )
            call ctx%errdp  ( '#', eccp                                )
            call ctx%sigerr ( 'SPICE(BADPECCENTRICITY)'                )
            return

         end if

      end if

!
!     Update for long period periodics if a deep space trajectory.
!

      if (dodeep) then

          sinip =  dsin(xincp)
          cosip =  dcos(xincp)
          aycof = -0.5_wp*j3oj2*sinip

!
!         sgp4fix for divide by zero with xincp = 180 deg
!

          if (dabs(cosip+1.0_wp)> 1.5e-12_wp) then
             xlcof  = -0.25_wp*j3oj2*sinip* &
                       (3.0_wp+5.0_wp*cosip)/(1.0_wp+cosip)
          else
             xlcof  = -0.25_wp*j3oj2*sinip* &
                       (3.0_wp+5.0_wp*cosip)/temp4
          end if

      end if

      axnl = eccp*dcos(argpp)
      temp = 1.0_wp / (am*(1.0_wp-eccp*eccp))
      aynl = eccp*dsin(argpp) + temp*aycof
      xl   = mp + argpp + nodep + temp*xlcof*axnl

!
!     Solve Kepler's equation.
!
      u    = dmod(xl-nodep,twopi)
      eo1  = u
      iter = 0

!
!     sgp4fix for Kepler iteration the following iteration needs
!     better limits on corrections
!

      temp = 9999.9_wp

      do while ((temp>=1.0e-12_wp).and.(iter<10))

         iter  = iter+1
         sineo1= dsin(eo1)
         coseo1= dcos(eo1)
         tem5  = 1.0_wp - coseo1*axnl - sineo1*aynl
         tem5  = (u - aynl*coseo1 + axnl*sineo1 - eo1) / tem5
         temp  = dabs(tem5)

!
!        Stop excessive correction.
!
         if(temp>1.0_wp) tem5=tem5/temp

         eo1   = eo1+tem5

      end do

!
!     Short period preliminary quantities.
!
      ecose = axnl*coseo1+aynl*sineo1
      esine = axnl*sineo1-aynl*coseo1
      el2   = axnl*axnl+aynl*aynl
      pl    = am*(1.0_wp-el2)

!
!     Error check for semi-latus rectum < 0.0
!
      if ( pl < 0.0_wp ) then

         call ctx%setmsg ( 'Semi-latus rectum less-than zero.'          )
         call ctx%sigerr ( 'SPICE(BADSEMILATUS)'                        )
         return

      end if

      rl    = am*(1.0_wp-ecose)
      rdotl = dsqrt(am)*esine/rl
      rvdotl= dsqrt(pl)/rl
      betal = dsqrt(1.0_wp-el2)
      temp  = esine/(1.0_wp+betal)
      sinu  = am/rl*(sineo1-aynl-axnl*temp)
      cosu  = am/rl*(coseo1-axnl+aynl*temp)
      su    = datan2(sinu,cosu)
      sin2u = (cosu+cosu)*sinu
      cos2u = 1.0_wp-2.0_wp*sinu*sinu
      temp  = 1.0_wp/pl
      temp1 = 0.5_wp*j2*temp
      temp2 = temp1*temp

!
!     Update for short period periodics if a deep space trajectory.
!
      if (dodeep) then
          cosisq = cosip*cosip
          con41  = 3.0_wp*cosisq - 1.0_wp
          x1mth2 = 1.0_wp - cosisq
          x7thm1 = 7.0_wp*cosisq - 1.0_wp
      end if

      mr    = rl*(1.0_wp - 1.5_wp*temp2*betal*con41) + &
                 0.5_wp*temp1*x1mth2*cos2u
      su    = su - 0.25_wp*temp2*x7thm1*sin2u
      xnode = nodep + 1.5_wp*temp2*cosip*sin2u
      xinc  = xincp + 1.5_wp*temp2*cosip*sinip*cos2u
      mv    = rdotl - xn*temp1*x1mth2*sin2u / xke
      rvdot = rvdotl + xn*temp1* (x1mth2*cos2u+1.5_wp*con41) / xke

!
!     Orientation vectors.
!

      sinsu=  dsin(su)
      cossu=  dcos(su)
      snod =  dsin(xnode)
      cnod =  dcos(xnode)
      sini =  dsin(xinc)
      cosi =  dcos(xinc)
      xmx  = -snod*cosi
      xmy  =  cnod*cosi
      ux   =  xmx*sinsu + cnod*cossu
      uy   =  xmy*sinsu + snod*cossu
      uz   =  sini*sinsu
      vx   =  xmx*cossu - cnod*sinsu
      vy   =  xmy*cossu - snod*sinsu
      vz   =  sini*cossu

!
!     Position and velocity.
!

      state(1) = mr*ux * er
      state(2) = mr*uy * er
      state(3) = mr*uz * er
      state(4) = (mv*ux + rvdot*vx) * kps
      state(5) = (mv*uy + rvdot*vy) * kps
      state(6) = (mv*uz + rvdot*vz) * kps

!
!     sgp4fix for decaying satellites
!
!     Place this test here to ensure evaluation of STATE.
!     The result may be physically invalid.
!
      if (mr < 1.0_wp) then

         call ctx%setmsg ( 'Satellite has decayed.' )
         call ctx%sigerr ( 'SPICE(ORBITDECAY)'      )
         return

      end if

      return
      end associate
      end subroutine xxsgp4e

end submodule pepper_records
