!> Spacecraft clock conversions: [[scs2e]], [[sce2s]], [[sct2e]], [[sce2t]], [[scencd]], [[scdecd]], [[sctiks]], [[scfmt]], [[scpart]].
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_sclk_api

   implicit none

contains

      !> Convert ephemeris seconds past J2000 (ET) to integral
      !> encoded spacecraft clock (`ticks'). For conversion to
      !> fractional ticks, (required for C-kernel production), see
      !> the routine [[sce2c]].
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
      !>     SCLKDP   is an encoded integral spacecraft clock value.
      !>              SCLKDP is an encoded representation of the total
      !>              count of spacecraft clock ticks measured from the
      !>              time the spacecraft clock started to the epoch ET:
      !>              partition information IS reflected in the encoded
      !>              value. SCLKDP is rounded to the nearest integral
      !>              double precision number.
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
      !>     This routine outputs discrete, encoded SCLK values. Since
      !>     continuous SCLK values are generally more useful, the newer
      !>     routine SCE2C (ET to continuous ticks) should normally be used
      !>     in place of this one. However, the functionality of this
      !>     routine is needed for converting ET to SCLK strings, and the
      !>     routine SCE2S calls this routine for that purpose.
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
      !>            is desired.
      !>
      !>            Find the encoded SCLK value corresponding to ET.
      !>
      !>                     CALL SCE2T  ( -77,  ET,  SCLKDP )
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
      !>     2)  Convert UTC to an encoded Voyager 2 SCLK value.
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
      !>                     CALL SCE2T  ( -32,  ET,  SCLKDP )
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
      !>        Moved the required readings present in $Literature_References
      !>        section to $Required_Reading.
      !>
      !>-    SPICELIB Version 1.0.4, 27-JAN-2004 (NJB)
      !>
      !>        Header was updated to remove comments indicating this routine
      !>        was deprecated. Minor changes were made to clarify both the
      !>        functionality of this routine and the difference between
      !>        this routine and SCE2C.  $Examples were updated to use FURNSH.
      !>
      !>-    SPICELIB Version 1.0.3, 09-MAR-1999 (NJB)
      !>
      !>        Updated to reflect the introduction of continuous ticks and
      !>        the routine SCE2C.
      !>
      !>-    SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)
      !>
      !>        Header was updated to reflect possibility of needing to load
      !>        a leapseconds kernel before calling this routine. Comment
      !>        section for permuted index source lines was added following the
      !>        header.
      !>
      !>-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
      !>
      !>        $Restrictions section no longer states that you must load the
      !>        leapseconds kernel prior to calling this routine.
      !>
      !>        The examples have been slightly re-written. In particular,
      !>        they no longer use calls to CLPOOL.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
      module subroutine sce2t ( ctx, sc, et, sclkdp )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      real(wp), intent(in) :: et  !! Ephemeris time, seconds past J2000.
      real(wp), intent(out) :: sclkdp  !! SCLK, encoded as ticks since spacecraft clock start.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Just hand off the conversion to the appropriate routine.
!
      if (  sctype ( ctx, sc )  ==  1  ) then

         call scet01 ( ctx, sc, et, sclkdp )

      else

         call ctx%setmsg ( 'Clock type # is not supported.' )
         call ctx%errint ( '#', sctype ( ctx, sc )               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'            )
         return

      end if

      return
      end subroutine sce2t

      !> Convert encoded spacecraft clock ticks to character clock format.
      !>
      !>## Detailed Input
      !>
      !>     SC       is the NAIF ID number for the spacecraft whose clock's
      !>              time is being decoded.
      !>
      !>     TICKS    is the double precision encoding of a clock time in
      !>              units of ticks. Partition information is not reflected
      !>              in this value.
      !>
      !>              An analogy may be drawn between a spacecraft clock and
      !>              a standard wall clock. The number of ticks
      !>              corresponding to the wall clock string
      !>
      !>                 hh:mm:ss
      !>
      !>              would be the number of seconds represented by that
      !>              time.
      !>
      !>              For example,
      !>
      !>                 Clock string    Number of ticks
      !>                 ------------    ---------------
      !>                   00:00:10             10
      !>                   00:01:00             60
      !>                   00:10:00            600
      !>                   01:00:00           3600
      !>                   01:01:00           3660
      !>
      !>              If TICKS contains a fractional part the result is the
      !>              same as if TICKS had been rounded to the nearest whole
      !>              number.
      !>
      !>              See the $Examples section below for examples of
      !>              actual spacecraft clock conversions.
      !>
      !>## Detailed Output
      !>
      !>     CLKSTR   is the spacecraft clock character string
      !>              corresponding to TICKS. Partition information is
      !>              not included in CLKSTR.
      !>
      !>              Using Galileo as an example, the full format clock
      !>              string is
      !>
      !>                 wwwwwwww:xx:y:z
      !>
      !>              where z is a mod-8 counter (values 0-7) which
      !>              increments approximately once every 8 1/3 ms., y is a
      !>              mod-10 counter (values 0-9) which increments once
      !>              every time z turns over, i.e., approximately once every
      !>              66 2/3 ms., xx is a mod-91 (values 0-90) counter
      !>              which increments once every time y turns over, i.e.,
      !>              once every 2/3 seconds. wwwwwwww is the Real-Time Image
      !>              Count (RIM), which increments once every time xx turns
      !>              over, i.e., once every 60 2/3 seconds. The roll-over
      !>              expression for the RIM is 16777215, which corresponds
      !>              to approximately 32 years.
      !>
      !>              wwwwwwww, xx, y, and z are referred to interchangeably
      !>              as the fields or components of the spacecraft clock.
      !>              SCLK components may be separated by any of these five
      !>              characters: ' '  ':'  ','  '-'  '.'
      !>              The delimiter used is determined by a kernel pool
      !>              variable and can be adjusted by the user.
      !>
      !>              Some spacecraft clock components have offset, or
      !>              starting, values different from zero. For example,
      !>              with an offset value of 1, a mod 20 counter would
      !>              cycle from 1 to 20 instead of from 0 to 19.
      !>
      !>              See the SCLK required reading for a detailed
      !>              description of the Voyager and Mars Observer clock
      !>              formats.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the data type for the spacecraft is not supported,
      !>         the error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     2)  If the value for TICKS is negative, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     3)  If the SCLK kernel file does not contain data for the
      !>         spacecraft specified by SC, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     4)  If the declared length of CLKSTR is not large enough to
      !>         contain the output clock string, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The routine SCTIKS performs the inverse operation to SCFMT,
      !>     converting from clock format to number of ticks.
      !>
      !>     Note the important difference between SCFMT and SCDECD. SCDECD
      !>     converts some number of ticks since the spacecraft clock start
      !>     time to a character string which includes a partition number.
      !>     SCFMT, which is called by SCDECD, does not make use of partition
      !>     information.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) The following code example finds partition start and stop
      !>        times for the Stardust spacecraft from a spacecraft clock
      !>        kernel file. Since those times are always returned in units
      !>        of ticks, the program uses SCFMT to print the times in
      !>        Stardust clock format.
      !>
      !>        Use the SCLK kernel below to load the Stardust time
      !>        correlation data and spacecraft clock partition information.
      !>
      !>           sdu_sclkscet_00074.tsc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SCFMT_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the parameters that define the sizes and limits
      !>C     of the SCLK system.
      !>C
      !>      INCLUDE 'sclk.inc'
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               CLKLEN
      !>      PARAMETER           ( CLKLEN = 30 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(CLKLEN)    START
      !>      CHARACTER*(CLKLEN)    STOP
      !>
      !>      DOUBLE PRECISION      PSTART (MXPART)
      !>      DOUBLE PRECISION      PSTOP  (MXPART)
      !>
      !>      INTEGER               I
      !>      INTEGER               NPARTS
      !>      INTEGER               SC
      !>
      !>C
      !>C     Assign the value for the Stardust spacecraft ID.
      !>C
      !>      SC = -29
      !>
      !>C
      !>C     Load the SCLK file.
      !>C
      !>      CALL FURNSH ( 'sdu_sclkscet_00074.tsc' )
      !>
      !>C
      !>C     Retrieve the arrays for PSTART and PSTOP and the
      !>C     number of partitions within the SCLK.
      !>C
      !>      CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
      !>
      !>C
      !>C     Loop over each array value.
      !>C
      !>      DO I= 1, NPARTS
      !>
      !>         CALL SCFMT ( SC, PSTART( I ), START )
      !>         CALL SCFMT ( SC, PSTOP ( I ), STOP )
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,*) 'Partition: ', I
      !>         WRITE(*,*) '   Start : ', START
      !>         WRITE(*,*) '   Stop  : ', STOP
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Partition:            1
      !>            Start : 0000000000.000
      !>            Stop  : 0602741011.080
      !>
      !>         Partition:            2
      !>            Start : 0602741014.217
      !>            Stop  : 0605660648.173
      !>
      !>         Partition:            3
      !>            Start : 0605660649.000
      !>            Stop  : 0631375256.224
      !>
      !>         Partition:            4
      !>            Start : 0631375257.000
      !>            Stop  : 0633545577.218
      !>
      !>         Partition:            5
      !>            Start : 0633545578.000
      !>            Stop  : 0644853954.043
      !>
      !>         Partition:            6
      !>            Start : 0644853954.000
      !>            Stop  : 0655316480.089
      !>
      !>         Partition:            7
      !>            Start : 0655316480.000
      !>            Stop  : 0660405279.066
      !>
      !>         Partition:            8
      !>            Start : 0660405279.000
      !>            Stop  : 0670256568.229
      !>
      !>         Partition:            9
      !>            Start : 0670256569.000
      !>            Stop  : 0674564039.091
      !>
      !>         Partition:           10
      !>            Start : 0674564040.000
      !>            Stop  : 4294537252.255
      !>
      !>     2) Below are some examples illustrating various input numbers of
      !>        ticks and the resulting clock string outputs for the Galileo
      !>        spacecraft.
      !>
      !>           TICKS                CLKSTR
      !>           ----------------     --------------------
      !>           -1                   Error: Ticks must be a positive number
      !>           0                    '00000000:00:0:0'
      !>           1                    '00000000:00:0:1'
      !>           1.3                  '00000000:00:0:1'
      !>           1.5                  '00000000:00:0:2'
      !>           2                    '00000000:00:0:2'
      !>           7                    '00000000:00:0:7'
      !>           8                    '00000000:00:1:0'
      !>           80                   '00000000:01:0:0'
      !>           88                   '00000000:01:1:0'
      !>           7279                 '00000000:90:9:7'
      !>           7280                 '00000001:00:0:0'
      !>           1234567890           '00169583:45:6:2'
      !>
      !>        The following examples are for the Voyager 2 spacecraft.
      !>        Note that the third component of the Voyager clock has an
      !>        offset value of one.
      !>
      !>           TICKS                CLKSTR
      !>           ----------------     --------------------
      !>           -1                   Error: Ticks must be a positive number
      !>           0                    '00000:00:001'
      !>           1                    '00000:00:002'
      !>           1.3                  '00000:00:002'
      !>           1.5                  '00000:00:003'
      !>           2                    '00000:00:003'
      !>           799                  '00000:00:800'
      !>           800                  '00000:01:001'
      !>           47999                '00000:59:800'
      !>           48000                '00001:00:001'
      !>           3145727999           '65535:59:800'
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
      !>     R.E. Thurman       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing code fragment.
      !>
      !>        Moved implementation details from $Particulars section to a
      !>        comment in the appropriate part of the code where it applies.
      !>
      !>-    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.1, 17-APR-1992 (JML) (WLT)
      !>
      !>        The $Exceptions section was updated to state that an error
      !>        is signaled if SCLKCH is not declared big enough to
      !>        contain the output spacecraft clock string.
      !>
      !>        The wording to exception number three was changed.
      !>
      !>        Miscellaneous minor updates to the header were performed.
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
      module subroutine scfmt ( ctx, sc, ticks, clkstr )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft identification code.
      real(wp), intent(in) :: ticks  !! Spacecraft clock count encoded representation.
      character(*), intent(out) :: clkstr  !! Character representation of a clock count.

      integer(ip) :: type

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Determine which data type the spacecraft clock belongs to and
!     calls SCFMnn, where nn corresponds to the data type code. SCFMTnn
!     handles the actual conversion from ticks to clock string format.
!
      type  =  sctype( ctx, sc )

      if  ( type == 1 )  then
         call scfm01( ctx, sc, ticks, clkstr )
      else
         call ctx%setmsg ( 'Clock type # is not supported. ' )
         call ctx%errint ( '#', type                         )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'             )
         return
      end if

      return
      end subroutine scfmt

      !> Get spacecraft clock partition information from a spacecraft
      !> clock kernel file.
      !>
      !>## Detailed Input
      !>
      !>     SC       is the NAIF ID for the spacecraft whose clock partition
      !>              information is being requested.
      !>
      !>## Detailed Output
      !>
      !>     NPARTS   is the number of spacecraft clock time partitions
      !>              described in the kernel file for spacecraft SC.
      !>
      !>     PSTART   is an array containing NPARTS partition start times
      !>              represented as double precision, encoded SCLK
      !>              ("ticks"). The values contained in PSTART are whole
      !>              numbers.
      !>
      !>     PSTOP    is an array containing NPARTS partition end times
      !>              represented as double precision, encoded SCLK
      !>              ("ticks"). The values contained in PSTOP are whole
      !>              numbers.
      !>
      !>## Parameters
      !>
      !>     See the include file
      !>
      !>        sclk.inc
      !>
      !>     for sizes and limits used by the SCLK system.
      !>
      !>     MXPART   is the maximum number of partitions for any spacecraft
      !>              clock. SCLK kernels contain start and stop times for
      !>              each partition. See the include file sclk.inc for this
      !>              parameter's value.
      !>
      !>## Exceptions
      !>
      !>     1)  If the kernel variables containing the spacecraft clock
      !>         partition start and stop times have not been loaded in the
      !>         kernel pool, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     2)  If the number of start and stop times are different,
      !>         the error SPICE(NUMPARTSUNEQUAL) is signaled.
      !>
      !>## Files
      !>
      !>     An SCLK kernel containing spacecraft clock partition start
      !>     and stop times for the spacecraft clock indicated by SC must
      !>     be loaded into the kernel pool.
      !>
      !>## Particulars
      !>
      !>     SCPART looks for two variables in the kernel pool for each
      !>     spacecraft's partition information. If SC = -nn, then the names of
      !>     the variables are
      !>
      !>        SCLK_PARTITION_START_nn
      !>        SCLK_PARTITION_END_nn
      !>
      !>     The start and stop times returned are in units of "ticks".
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) The following code example finds partition start and stop
      !>        times for the Stardust spacecraft from a spacecraft clock
      !>        kernel file. Since those times are always returned in units
      !>        of ticks, the program uses SCFMT to print the times in
      !>        Stardust clock format.
      !>
      !>        Use the SCLK kernel below to load the Stardust time
      !>        correlation data and spacecraft clock partition information.
      !>
      !>           sdu_sclkscet_00074.tsc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SCPART_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the parameters that define the sizes and limits
      !>C     of the SCLK system.
      !>C
      !>      INCLUDE 'sclk.inc'
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               CLKLEN
      !>      PARAMETER           ( CLKLEN = 30 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(CLKLEN)    START
      !>      CHARACTER*(CLKLEN)    STOP
      !>
      !>      DOUBLE PRECISION      PSTART (MXPART)
      !>      DOUBLE PRECISION      PSTOP  (MXPART)
      !>
      !>      INTEGER               I
      !>      INTEGER               NPARTS
      !>      INTEGER               SC
      !>
      !>C
      !>C     Assign the value for the Stardust spacecraft ID.
      !>C
      !>      SC = -29
      !>
      !>C
      !>C     Load the SCLK file.
      !>C
      !>      CALL FURNSH ( 'sdu_sclkscet_00074.tsc' )
      !>
      !>C
      !>C     Retrieve the arrays for PSTART and PSTOP and the
      !>C     number of partitions within the SCLK.
      !>C
      !>      CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
      !>
      !>C
      !>C     Loop over each array value.
      !>C
      !>      DO I= 1, NPARTS
      !>
      !>         CALL SCFMT ( SC, PSTART( I ), START )
      !>         CALL SCFMT ( SC, PSTOP ( I ), STOP )
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,*) 'Partition: ', I
      !>         WRITE(*,*) '   Start : ', START
      !>         WRITE(*,*) '   Stop  : ', STOP
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Partition:            1
      !>            Start : 0000000000.000
      !>            Stop  : 0602741011.080
      !>
      !>         Partition:            2
      !>            Start : 0602741014.217
      !>            Stop  : 0605660648.173
      !>
      !>         Partition:            3
      !>            Start : 0605660649.000
      !>            Stop  : 0631375256.224
      !>
      !>         Partition:            4
      !>            Start : 0631375257.000
      !>            Stop  : 0633545577.218
      !>
      !>         Partition:            5
      !>            Start : 0633545578.000
      !>            Stop  : 0644853954.043
      !>
      !>         Partition:            6
      !>            Start : 0644853954.000
      !>            Stop  : 0655316480.089
      !>
      !>         Partition:            7
      !>            Start : 0655316480.000
      !>            Stop  : 0660405279.066
      !>
      !>         Partition:            8
      !>            Start : 0660405279.000
      !>            Stop  : 0670256568.229
      !>
      !>         Partition:            9
      !>            Start : 0670256569.000
      !>            Stop  : 0674564039.091
      !>
      !>         Partition:           10
      !>            Start : 0674564040.000
      !>            Stop  : 4294537252.255
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that an SCLK kernel appropriate to the
      !>         spacecraft identified by SC has been loaded into the kernel
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
      !>     R.E. Thurman       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.4.0, 06-JUL-2021 (JDR) (NJB)
      !>
      !>        Code was re-written to fetch partition data from the SC01
      !>        subsystem. This routine no longer sets watches on kernel
      !>        variables.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries from $Revisions section.
      !>
      !>        Added complete code example based on existing code fragment.
      !>
      !>-    SPICELIB Version 2.3.1, 19-MAR-2014 (NJB)
      !>
      !>        Minor header comment updates were made.
      !>
      !>-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB)
      !>
      !>        The values of the parameter MXPART is now
      !>        provided by the INCLUDE file sclk.inc.
      !>
      !>-    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.1.0, 22-MAR-1993 (JML)
      !>
      !>        The routine now uses the kernel pool watch capability.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) (JML) (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.4.0, 04-NOV-2020 (JDR) (NJB)
      !>
      !>        Code was re-written to fetch partition data from the SC01
      !>        subsystem. This routine no longer sets watches on kernel
      !>        variables. Setting watches "touches" the kernel pool, which
      !>        thwarts optimizations of many SPICE subsystems.
      module subroutine scpart ( ctx, sc, nparts, pstart, pstop )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sclk
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft identification code.
      integer(ip), intent(out) :: nparts  !! The number of spacecraft clock partitions.
      real(wp), intent(out) :: pstart  ( * )  !! Array of partition start times.
      real(wp), intent(out) :: pstop   ( * )  !! Array of partition stop times.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the partition values from the type 1 subsystem. If the
!     clock is not type 1, an error will be signaled.
!
      call scpr01 ( ctx, sc, nparts, pstart, pstop )

      return
      end subroutine scpart

      !> Convert encoded spacecraft clock (`ticks') to ephemeris
      !> seconds past J2000 (ET).
      !>
      !>## Detailed Input
      !>
      !>     SC       is a NAIF integer code for a spacecraft, one of
      !>              whose encoded clock values is represented by
      !>              SCLKDP.
      !>
      !>     SCLKDP   is an encoded spacecraft clock value. SCLKDP
      !>              represents time measured from spacecraft clock
      !>              start: partition information IS reflected in the
      !>              encoded value.
      !>
      !>## Detailed Output
      !>
      !>     ET       is the epoch, specified as ephemeris seconds past
      !>              J2000, that corresponds to SCLKDP.
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
      !>         output argument ET will not be modified. This routine assumes
      !>         that that an SCLK kernel appropriate to the spacecraft clock
      !>         identified by the input argument SC has been loaded.
      !>
      !>     2)  If a leapseconds kernel is required for conversion between
      !>         SCLK and ET but is not loaded, an error is signaled by a
      !>         routine in the call tree of this routine. The output argument
      !>         ET will not be modified. When using SCLK kernels that map SCLK
      !>         to a time system other than ET (also called barycentric
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
      !>         SPICE(NOTSUPPORTED) is signaled. The output argument ET
      !>         will not be modified.
      !>
      !>     4)  If the input argument SCLKDP is invalid, an error is signaled
      !>         by a routine in the call tree of this routine. The output
      !>         argument ET will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine operates on encoded SCLK values. These values
      !>     are obtained by calling the SPICELIB routine SCENCD or other
      !>     SCLK conversion routines. The advantage of encoded SCLK, as
      !>     opposed to character string representations of SCLK is that
      !>     encoded SCLK values are easy to perform arithmetic operations on.
      !>     Additionally, working with encoded SCLK reduces the overhead of
      !>     repeated conversion of character strings to integers or double
      !>     precision numbers.
      !>
      !>     To convert a string representation of an SCLK value to ET, use
      !>     the SPICELIB routine SCS2E.
      !>
      !>     See the SCLK Required Reading for a list of the entire set of
      !>     SCLK conversion routines.
      !>
      !>## Examples
      !>
      !>     1)  Encode a Galileo SCLK string, and convert the encoded value
      !>         to ET; use these time values to look up both GLL orbiter
      !>         scan platform's pointing and the GLL--Earth state vector
      !>         for an epoch specified by an SCLK string.
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
      !>                     CALL FURNSH ( 'SCLK.KER'        )
      !>
      !>            The mission is Galileo, which has spacecraft ID -77.
      !>            Let's assume that the SCLK string is
      !>
      !>                     1 / 1900000:00:00
      !>
      !>            The number 1, followed by a slash, indicates that the
      !>            epoch is in the first partition.
      !>
      !>            The next step is to encode this SCLK string, and also
      !>            find the corresponding ET value:
      !>
      !>                     CALL SCENCD ( -77, '1/ 1900000:00:00', SCLKDP )
      !>                     CALL SCT2E  ( -77,  SCLKDP,            ET     )
      !>
      !>            We'll assume that you've already loaded SPK and CK files
      !>            containing ephemeris data for the GLL orbiter and the
      !>            Earth, as well as scan platform pointing.  Now you're
      !>            ready to call both CKGP, which expects the input epoch to
      !>            be specified by an encoded SCLK string, and SPKEZ, which
      !>            expects the epoch to be specified as an ephemeris time.
      !>
      !>               C
      !>               C     Find scan platform pointing CMAT and s/c--target
      !>               C     vector (first 3 components of STATE) at epoch.
      !>               C     We assume that CK and SPK kernels have been loaded
      !>               C     already, via CKLPF and SPKLEF respectively.
      !>               C
      !>                     SCANPL = -77001
      !>                     EARTH  =    399
      !>
      !>                     CALL CKGP  ( SCANPL,
      !>                    .             SCLKDP,
      !>                    .             TOL,
      !>                    .             REFSYS,
      !>                    .             CMAT,
      !>                    .             CLKOUT,
      !>                    .             FOUND   )
      !>
      !>                     IF ( .NOT. FOUND ) THEN
      !>
      !>                        [ Indicate to user that pointing was not
      !>                          available ]
      !>
      !>                     END IF
      !>
      !>                     CALL SPKEZ ( EARTH,
      !>                    .             ET,
      !>                    .             REFSYS,
      !>                    .             CORR,
      !>                    .             -77,
      !>                    .             STATE,
      !>                    .             LT      )
      !>
      !>     2)  Convert an encoded Voyager 2 SCLK value to UTC, using calendar
      !>         format, with 3 digits of precision.
      !>
      !>            Again, your initialization code must load the leapseconds
      !>            and SCLK kernels:
      !>
      !>               C
      !>               C     Load leapseconds and SCLK kernels:
      !>               C
      !>                     CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>                     CALL FURNSH ( 'VGR2SCLK.KER'    )
      !>
      !>            To find the UTC value corresponding to the encoded
      !>            Voyager 2 SCLK value SCLKDP, you can use the code fragment
      !>
      !>                     CALL SCT2E  ( -32,  SCLKDP,     ET  )
      !>                     CALL ET2UTC (  ET,  'C',    3,  UTC )
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
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
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
      !>        Moved the required readings present in $Literature_References
      !>        section to $Required_Reading.
      !>
      !>-    SPICELIB Version 1.0.4, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.3, 09-MAR-1999 (NJB)
      !>
      !>        Explicit list of SCLK conversion routines in $Particulars
      !>        section has been replaced by a pointer to the SCLK Required
      !>        Reading.
      !>
      !>-    SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)
      !>
      !>        The $Brief_I/O section now lists ET correctly as an output
      !>        from this routine. Header was updated to reflect possibility
      !>        of needing to load a leapseconds kernel before calling this
      !>        routine. Comment section for permuted index source lines was
      !>        added following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
      !>
      !>        $Restrictions section no longer states that you must load the
      !>        leapseconds kernel prior to calling this routine.
      !>
      !>        The examples have been slightly re-written. In particular,
      !>        they no longer use calls to CLPOOL.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
      module subroutine sct2e ( ctx, sc, sclkdp, et )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      real(wp), intent(in) :: sclkdp  !! SCLK, encoded as ticks since spacecraft clock start.
      real(wp), intent(out) :: et  !! Ephemeris time, seconds past J2000.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Just hand off the conversion to the appropriate routine.
!
      if (  sctype ( ctx, sc )  ==  1  ) then

         call scte01 ( ctx, sc, sclkdp, et )

      else

         call ctx%setmsg ( 'Clock type # is not supported.' )
         call ctx%errint ( '#', sctype ( ctx, sc )               )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'            )
         return

      end if

      return
      end subroutine sct2e

      !> Convert a spacecraft clock format string to number of "ticks".
      !>
      !>## Detailed Input
      !>
      !>     SC       is the NAIF ID number for the spacecraft whose clock
      !>              string is being converted.
      !>
      !>     CLKSTR   is a character string representing a spacecraft clock
      !>              time, WITHOUT PARTITION NUMBER.
      !>
      !>              Using Galileo as an example, the full format is
      !>
      !>                             wwwwwwww:xx:y:z
      !>
      !>              where z is a mod-8 counter (values 0-7) which
      !>              increments approximately once every 8 1/3 ms., y is a
      !>              mod-10 counter (values 0-9) which increments once
      !>              every time z turns over, i.e., approximately once every
      !>              66 2/3 ms., xx is a mod-91 (values 0-90) counter
      !>              which increments once every time y turns over, i.e.,
      !>              once every 2/3 seconds. wwwwwwww is the Real-Time Image
      !>              Count (RIM), which increments once every time xx turns
      !>              over, i.e., once every 60 2/3 seconds. The roll-over
      !>              expression for the RIM is 16777215, which corresponds
      !>              to approximately 32 years.
      !>
      !>              wwwwwwww, xx, y, and z are referred to interchangeably
      !>              as the fields or components of the spacecraft clock.
      !>              SCLK components may be separated by any of the
      !>              following characters: ' '  '.'  ':'  ','  '-'
      !>              Any number of spaces may separate the components and
      !>              the delimiters. The presence of the RIM component
      !>              is required. Successive components may be omitted, and
      !>              in such cases are assumed to represent zero values.
      !>
      !>              Values for the individual components may exceed the
      !>              maximum expected values. For instance, '0:0:0:9' is
      !>              an acceptable Galileo clock string, and will convert
      !>              to the same number of ticks as '0:0:1:1'.
      !>
      !>              Consecutive delimiters containing no intervening digits
      !>              are treated as if they delimit zero components.
      !>
      !>              Trailing zeros should always be included to match the
      !>              length of the counter.  For example, a Galileo clock
      !>              count of '25684.90' should not be represented as
      !>              '25684.9'.
      !>
      !>              Some spacecraft clock components have offset, or
      !>              starting, values different from zero. For example,
      !>              with an offset value of 1, a mod 20 counter would
      !>              cycle from 1 to 20 instead of from 0 to 19.
      !>
      !>              See the SCLK required reading for a detailed
      !>              description of the Voyager and Mars Observer clock
      !>              formats.
      !>
      !>## Detailed Output
      !>
      !>     TICKS    is the number of ticks represented by the spacecraft
      !>              clock string. A tick is defined to be the smallest
      !>              time increment expressible by the spacecraft clock.
      !>
      !>              An analogy may be drawn between a spacecraft clock
      !>              and a standard wall clock, measuring hours, minutes
      !>              and seconds. The number of ticks represented by the
      !>              wall clock string
      !>                                   hh:mm:ss
      !>
      !>              would be the number of seconds represented by that
      !>              time.
      !>
      !>              For example:
      !>
      !>                       00:00:10  would convert to 10
      !>                       00:01:00  would convert to 60
      !>                       00:10:00  would convert to 600
      !>                       01:00:00  would convert to 3600
      !>                       01:01:00  would convert to 3660
      !>
      !>              See the $Examples section below for examples for
      !>              actual spacecraft clocks.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the spacecraft clock type is not supported, the
      !>         error SPICE(NOTSUPPORTED) is signaled.
      !>
      !>     2)  If any of the extracted clock components cannot be parsed as
      !>         integers, or the string has too many components, or the value
      !>         of one of the components is less than the offset value, then,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     3)  Invalid spacecraft ID's are not diagnosed.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Each spacecraft is assigned a clock type code in the kernel file.
      !>     SCTIKS calls the function SCTYPE to determine this value. If the
      !>     clock type is supported by SPICE, then the SPICELIB routine TIKSnn
      !>     is called to handle the actual conversion from clock format to
      !>     number of ticks. The nn in TIKSnn refers to the spacecraft clock
      !>     type code. Different spacecraft have distinct clock formats but
      !>     can still be of the same clock type.
      !>
      !>     The TIKSnn routines are entry points to the routines SCLKnn, which
      !>     also contain the ticks-to-clock format conversion routines FMTnn.
      !>     FMTnn is called by the subroutine SCFMT, which performs the
      !>     inverse operation to SCTIKS.
      !>
      !>     Note the important difference between SCENCD and SCTIKS. SCENCD
      !>     converts a clock string to the number of ticks it represents
      !>     since the beginning of the mission, and so uses partition
      !>     information. SCTIKS just converts to absolute ticks.
      !>
      !>## Examples
      !>
      !>     SCTIKS is used as part of the process of encoding spacecraft clock
      !>     by SCENCD, though SCTIKS does not process any partition informa-
      !>     tion.
      !>
      !>     Another use of SCTIKS, however, is to convert a clock measurement
      !>     to ticks for use as a tolerance for the CK reader CKGP.
      !>
      !>     C
      !>     C      Get the pointing from a CK file of the VGR 1 narrow angle
      !>     C      image corresponding to a particular SCLK count.
      !>     C
      !>     C      Load the CK file and the kernel file containing SCLK
      !>     C      partition information for SCENCD.
      !>     C
      !>            CALL CKLPF  ( 'VGR1NA.CK', HANDLE )
      !>            CALL FURNSH ( 'SCLK.KER' )
      !>
      !>     C
      !>     C      Get the right ID numbers.
      !>     C
      !>            SC    = -31
      !>            INSTR = -31001
      !>
      !>     C
      !>     C      The SCLK string includes a partition number. Pictures are
      !>     C      never shuttered at intervals smaller than 1 MOD60 count
      !>     C      from each other. So use 1 MOD60 count as the time
      !>     C      tolerance.
      !>     C
      !>            CLKSTR = '1/20556:14:768'
      !>            TOLSTR = '      0:01:000'
      !>
      !>     C
      !>     C      Encode the clock string and the tolerance.
      !>     C
      !>            CALL SCENCD ( SC, CLKSTR, SCLK )
      !>            CALL SCTIKS ( SC, TOLSTR, TOL  )
      !>
      !>     C
      !>     C      Get the pointing from the C-kernel.
      !>     C
      !>            CALL CKGP ( INSTR, SCLK, TOL, REF, CMAT, CLKOUT, FOUND )
      !>
      !>      Below are some examples illustrating various clock string inputs
      !>      and the resulting outputs for the Galileo spacecraft. See the
      !>      SCLK required reading for a detailed description of the Galileo
      !>      clock format.
      !>
      !>         CLKSTR                TICKS
      !>         ----------------      --------------------
      !>         '0:0:0:1'             1
      !>         '0:0:1'               8
      !>         '0:1'                 80
      !>         '1'                   7280
      !>         '1 0 0 0'             7280
      !>         '1,0,0,0'             7280
      !>         '1:90'                14480
      !>         '1:9'                 8000
      !>         '1:09'                8000
      !>         '0-0-10'              80   |--  Third component is supposed
      !>         '0-1-0'               80   |    to be a mod-10 count.
      !>         '0/1/0'               Error: '/' is not an accepted delimiter.
      !>         '1: 00 : 0 : 1'       7281
      !>         '1:::1'               7281
      !>         '1.1.1.1.1'           Error: Too many components
      !>         '1.1.1.1.'            Error: The last delimiter signals that
      !>                                      a fifth component will follow.
      !>
      !>         The following examples are for the Voyager 2 spacecraft. Note
      !>         that the last component of the Voyager clock has an offset
      !>         value of 1.
      !>
      !>         CLKSTR                TICKS
      !>         ----------------      --------------------
      !>          '0.0.001'              0
      !>          '0:0:002'              1
      !>          '0:01'                 800
      !>          '1'                    48000
      !>          '1.0'                  48000
      !>          '1.0.0'                Error: The 3rd component is never 0.
      !>          '0.0:100'              99
      !>          '0-60-1'               48000
      !>          '1-1-1'                48800
      !>          '1-1-2'                48801
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
      !>     R.E. Thurman       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 01-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
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
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
      module subroutine sctiks ( ctx, sc, clkstr, ticks )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft identification code.
      character(*), intent(in) :: clkstr  !! Character representation of a spacecraft clock.
      real(wp), intent(out) :: ticks  !! Number of ticks represented by the clock string.

      integer(ip) :: type

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If the spacecraft clock type is supported by NAIF then
!     call TIKSnn to perform the conversion.
!

      type = sctype ( ctx, sc )

      if ( type == 1 )  then
         call sctk01 ( ctx, sc, clkstr, ticks )
      else
         call ctx%setmsg ( 'Clock type # is not supported.' )
         call ctx%errint ( '#', type                        )
         call ctx%sigerr ( 'SPICE(NOTSUPPORTED)'            )
         return

      end if

      return
      end subroutine sctiks

      !> Convert a double precision encoding of spacecraft clock time into
      !> a character representation.
      !>
      !>## Detailed Input
      !>
      !>     SC       is the NAIF integer code of the spacecraft whose
      !>              clock's time is being decoded.
      !>
      !>     SCLKDP   is the double precision encoding of a clock time in
      !>              units of ticks since the spacecraft clock start time.
      !>              This value does reflect partition information.
      !>
      !>              An analogy may be drawn between a spacecraft clock
      !>              and a standard wall clock. The number of ticks
      !>              corresponding to the wall clock string
      !>
      !>                 hh:mm:ss
      !>
      !>              would be the number of seconds represented by that
      !>              time.
      !>
      !>              For example:
      !>
      !>                 Clock string      Number of ticks
      !>                 ------------      ---------------
      !>                   00:00:10              10
      !>                   00:01:00              60
      !>                   00:10:00             600
      !>                   01:00:00            3600
      !>
      !>              If SCLKDP contains a fractional part the result
      !>              is the same as if SCLKDP had been rounded to the
      !>              nearest whole number.
      !>
      !>## Detailed Output
      !>
      !>     SCLKCH   is the character representation of the clock count.
      !>              The exact form that SCLKCH takes depends on the
      !>              spacecraft.
      !>
      !>              Nevertheless, SCLKCH will have the following general
      !>              format:
      !>
      !>                 'pp/sclk_string'
      !>
      !>              'pp' is an integer greater than or equal to one and
      !>              represents a "partition number".
      !>
      !>              Each mission is divided into some number of partitions.
      !>              A new partition starts when the spacecraft clock
      !>              resets, either to zero, or to some other
      !>              value. Thus, the first partition for any mission
      !>              starts with launch, and ends with the first clock
      !>              reset. The second partition starts immediately when
      !>              the first stopped, and so on.
      !>
      !>              In order to be completely unambiguous about a
      !>              particular time, you need to specify a partition number
      !>              along with the standard clock string.
      !>
      !>              Information about when partitions occur for different
      !>              missions is contained in a spacecraft clock kernel
      !>              file which needs to be loaded into the kernel pool
      !>              before calling SCDECD.
      !>
      !>              The routine SCPART may be used to read the partition
      !>              start and stop times, in encoded units of ticks, from
      !>              the kernel file.
      !>
      !>              Since the end time of one partition is coincident with
      !>              the begin time of the next, two different time strings
      !>              with different partition numbers can encode into the
      !>              same value.
      !>
      !>              For example, if partition 1 ends at time t1, and
      !>              partition 2 starts at time t2, then
      !>
      !>                 '1/t1' and '2/t2'
      !>
      !>              will be encoded into the same value, say X. SCDECD
      !>              always decodes such values into the latter of the
      !>              two partitions. In this example,
      !>
      !>                 CALL SCDECD ( X, SC, CLKSTR )
      !>
      !>              will result in
      !>
      !>                 CLKSTR = '2/t2'.
      !>
      !>              'sclk_string' is a spacecraft specific clock string,
      !>              typically consisting of a number of components
      !>              separated by delimiters.
      !>
      !>              Using Galileo as an example, the full format is
      !>
      !>                 wwwwwwww:xx:y:z
      !>
      !>              where z is a mod-8 counter (values 0-7) which
      !>              increments approximately once every 8 1/3 ms., y is a
      !>              mod-10 counter (values 0-9) which increments once
      !>              every time z turns over, i.e., approximately once every
      !>              66 2/3 ms., xx is a mod-91 (values 0-90) counter
      !>              which increments once every time y turns over, i.e.,
      !>              once every 2/3 seconds. wwwwwwww is the Real-Time Image
      !>              Count (RIM), which increments once every time xx turns
      !>              over, i.e., once every 60 2/3 seconds. The roll-over
      !>              expression for the RIM is 16777215, which corresponds
      !>              to approximately 32 years.
      !>
      !>              wwwwwwww, xx, y, and z are referred to interchangeably
      !>              as the fields or components of the spacecraft clock.
      !>              SCLK components may be separated by any of these five
      !>              characters: ' '  ':'  ','  '-'  '.'
      !>              The delimiter used is determined by a kernel pool
      !>              variable and can be adjusted by the user.
      !>
      !>              Some spacecraft clock components have offset, or
      !>              starting, values different from zero. For example,
      !>              with an offset value of 1, a mod 20 counter would
      !>              cycle from 1 to 20 instead of from 0 to 19.
      !>
      !>              See the SCLK required reading for a detailed
      !>              description of the Voyager and Mars Observer clock
      !>              formats.
      !>
      !>## Parameters
      !>
      !>     MXPART   is the maximum number of spacecraft clock partitions
      !>              expected in the kernel file for any one spacecraft.
      !>              See the INCLUDE file sclk.inc for this parameter's
      !>              value.
      !>
      !>## Exceptions
      !>
      !>     1)  If kernel variables required by this routine are unavailable,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine. SCLKCH will be returned as a blank string in this
      !>         case.
      !>
      !>     2)  If the number of partitions in the kernel file for spacecraft
      !>         SC exceeds the parameter MXPART, the error
      !>         SPICE(TOOMANYPARTS) is signaled. SCLKCH will be returned
      !>         as a blank string in this case.
      !>
      !>     3)  If the encoded value does not fall in the boundaries of the
      !>         mission, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>         SCLKCH will be returned as a blank string in this case.
      !>
      !>     4)  If the declared length of SCLKCH is not large enough to
      !>         contain the output clock string, the error
      !>         SPICE(SCLKTRUNCATED) is signaled by either this routine or a
      !>         routine in the call tree of this routine. On output SCLKCH
      !>         will contain a portion of the truncated clock string.
      !>
      !>## Files
      !>
      !>     A kernel file containing spacecraft clock partition information
      !>     for the desired spacecraft must be loaded, using the routine
      !>     FURNSH, before calling this routine.
      !>
      !>## Particulars
      !>
      !>     In general, it is difficult to compare spacecraft clock counts
      !>     numerically since there are too many clock components for a
      !>     single comparison. The routine SCENCD provides a method of
      !>     assigning a single double precision number to a spacecraft's
      !>     clock count, given one of its character representations.
      !>
      !>     This routine performs the inverse operation to SCENCD, converting
      !>     an encoded double precision number to character format.
      !>
      !>     To convert the number of ticks since the start of the mission to
      !>     a clock format character string, SCDECD:
      !>
      !>        1) Determines the spacecraft clock partition that TICKS falls
      !>           in.
      !>
      !>        2) Subtracts off the number of ticks occurring in previous
      !>           partitions, to get the number of ticks since the beginning
      !>           of the current partition.
      !>
      !>        3) Converts the resulting ticks to clock format and forms the
      !>           string
      !>
      !>              'partition_number/clock_string'
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Double precision encodings of spacecraft clock counts are used
      !>        to tag pointing data in the C-kernel.
      !>
      !>        In the following example, pointing for a sequence of images
      !>        from the CASSINI Imaging Science Subsystem (ISS) is requested
      !>        from the C-kernel using an array of character spacecraft clock
      !>        counts as input. The clock counts attached to the output are
      !>        then decoded to character and compared with the input strings.
      !>
      !>        Use the CK kernel below to load the CASSINI image navigated
      !>        spacecraft pointing and orientation data.
      !>
      !>           04153_04182ca_ISS.bc
      !>
      !>        Use the SCLK kernel below to load the CASSINI spacecraft clock
      !>        time correlation data required for the conversion between
      !>        spacecraft clock string representation and double precision
      !>        encoding of spacecraft clock counts.
      !>
      !>           cas00071.tsc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SCDECD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>C     The instrument we want pointing for is the CASSINI
      !>C     spacecraft. The reference frame we want is
      !>C     J2000. The spacecraft is CASSINI.
      !>C
      !>      INTEGER               SC
      !>      PARAMETER           ( SC     = -82 )
      !>
      !>      INTEGER               INST
      !>      PARAMETER           ( INST   = -82000 )
      !>
      !>      CHARACTER*(*)         REF
      !>      PARAMETER           ( REF    = 'J2000' )
      !>
      !>      CHARACTER*(*)         CK
      !>      PARAMETER           ( CK     = '04153_04182ca_ISS.bc' )
      !>
      !>      CHARACTER*(*)         SCLK
      !>      PARAMETER           ( SCLK   = 'cas00071.tsc' )
      !>
      !>      INTEGER               NPICS
      !>      PARAMETER           ( NPICS  = 4 )
      !>
      !>      CHARACTER*(*)         CLKTOL
      !>      PARAMETER           ( CLKTOL = '1.0' )
      !>
      !>      INTEGER               MAXLEN
      !>      PARAMETER           ( MAXLEN = 30 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(25)        SCLKIN (4)
      !>      CHARACTER*(25)        SCLKOUT
      !>
      !>      DOUBLE PRECISION      CMAT   (3,3)
      !>      DOUBLE PRECISION      TIMEIN
      !>      DOUBLE PRECISION      TIMEOUT
      !>      DOUBLE PRECISION      TOL
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               K
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Set the input SCLK strings.
      !>C
      !>      DATA                  SCLKIN /  '1/1465644279.0',
      !>     .                                '1/1465644281.0',
      !>     .                                '1/1465644351.0',
      !>     .                                '1/1465644361.0'  /
      !>
      !>C
      !>C     Load the appropriate files. We need
      !>C
      !>C        1. CK file containing pointing data.
      !>C        2. Spacecraft clock kernel file.
      !>C
      !>      CALL FURNSH ( CK   )
      !>      CALL FURNSH ( SCLK )
      !>
      !>C
      !>C     Convert the tolerance string to ticks.
      !>C
      !>      CALL SCTIKS ( SC, CLKTOL, TOL )
      !>
      !>      DO I= 1, NPICS
      !>
      !>         CALL SCENCD ( SC, SCLKIN(I), TIMEIN )
      !>
      !>         CALL CKGP ( INST, TIMEIN,  TOL,  REF,
      !>     .               CMAT, TIMEOUT, FOUND     )
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,'(2A)') 'Input s/c clock count : ', SCLKIN(I)
      !>
      !>         IF ( FOUND ) THEN
      !>
      !>            CALL SCDECD ( SC, TIMEOUT, SCLKOUT )
      !>
      !>            WRITE(*,'(2A)') 'Output s/c clock count: ',
      !>     .                                          SCLKOUT
      !>            WRITE(*,'(A)') 'Output C-Matrix:'
      !>
      !>            DO J = 1, 3
      !>
      !>               WRITE(*,'(3F21.15)') ( CMAT(J,K), K = 1, 3 )
      !>
      !>            END DO
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A)') 'No pointing found.'
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
      !>        Input s/c clock count : 1/1465644279.0
      !>        No pointing found.
      !>
      !>        Input s/c clock count : 1/1465644281.0
      !>        Output s/c clock count: 1/1465644281.171
      !>        Output C-Matrix:
      !>           -0.335351455948710    0.864374440205611    0.374694846658341
      !>           -0.937887426812980   -0.343851965210223   -0.046184419961653
      !>            0.088918927227039   -0.366909598048763    0.925997176691424
      !>
      !>        Input s/c clock count : 1/1465644351.0
      !>        Output s/c clock count: 1/1465644351.071
      !>        Output C-Matrix:
      !>           -0.335380929397586    0.864363638262230    0.374693385378623
      !>           -0.937874292008090   -0.343889838107825   -0.046169163264003
      !>            0.088946301703530   -0.366899550417080    0.925998528787713
      !>
      !>        Input s/c clock count : 1/1465644361.0
      !>        No pointing found.
      !>
      !>## Restrictions
      !>
      !>     1)  Assumes that an SCLK kernel file appropriate for the clock
      !>         designated by SC is loaded in the kernel pool at the time
      !>         this routine is called.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.2.0, 18-NOV-2021 (NJB) (JDR)
      !>
      !>        Now variables PSTART, PSTOP, and PTOTLS are saved. Made minor
      !>        changes to formatting of code.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example fragments using PDS
      !>        archived CASSINI data.
      !>
      !>        Added FAILED() call after SCFMT call.
      !>
      !>        Removed unnecessary entries in $Revisions section.
      !>
      !>-    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB)
      !>
      !>        Values of parameter MXPART and PARTLN are now
      !>        provided by the INCLUDE file sclk.inc.
      !>
      !>-    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT)
      !>
      !>        The routine was changed to signal an error when SCLKCH is
      !>        not long enough to contain the output spacecraft clock
      !>        string.
      !>
      !>        FAILED is now checked after calling SCPART.
      !>
      !>        References to CLPOOL were deleted.
      !>
      !>        Miscellaneous minor updates to the header were performed.
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT)
      !>
      !>        The routine was changed to signal an error when SCLKCH is
      !>        not long enough to contain the output spacecraft clock
      !>        string. Previously, the SCLK routines simply truncated
      !>        the clock string on the right. It was determined that
      !>        since this truncation could easily go undetected by the
      !>        user ( only the leftmost field of a clock string is
      !>        required when clock string is used as an input to a
      !>        SCLK routine ), it would be better to signal an error
      !>        when this happens.
      !>
      !>        FAILED is checked after calling SCPART in case an
      !>        error has occurred reading the kernel file and the
      !>        error action is not set to 'abort'.
      !>
      !>        References to CLPOOL were deleted.
      !>
      !>        Miscellaneous minor updates to the header were performed.
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      module subroutine scdecd ( ctx, sc, sclkdp, sclkch )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstled
      use pepper_strings, only: intstr, prefix, suffix
      use pepper_inc_sclk
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft identification code.
      real(wp), intent(in) :: sclkdp  !! Encoded representation of a spacecraft clock count.
      character(*), intent(out) :: sclkch  !! Character representation of a clock count.
!
!     Local variables
!
      real(wp), allocatable :: pstart(:), pstop(:), ptotls(:)
      integer(ip) :: npmax
      character(12) :: prtstr  ! PEPPER: any partition number and the slash (SPICELIB: PARTLN, 5)
      integer(ip) :: nparts
      integer(ip) :: part
      integer(ip) :: prelen
      integer(ip) :: suflen
      integer(ip) :: i
      real(wp) :: ticks
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Use a working copy of the input.
!
      ticks = anint ( sclkdp )

      sclkch = ' '

!
!     Read the partition start and stop times (in ticks) for this
!     mission. Error if there are too many of them.  Also need to
!     check FAILED in case error handling is not in ABORT or
!     DEFAULT mode.
!
!
!     PEPPER: the partitions go to arrays of the clock's size (SPICELIB
!     used arrays of MXPART, 9999, and SPICE(TOOMANYPARTS) for more).
!
      npmax = sclk_partition_count ( ctx, sc )
      allocate ( pstart(npmax), pstop(npmax), ptotls(npmax) )
      call scpart ( ctx, sc, nparts, pstart, pstop )

      if ( ctx%failed() ) return

!
!     For each partition, compute the total number of ticks in that
!     partition plus all preceding partitions.
!
      ptotls( 1 ) = anint ( pstop( 1 ) - pstart( 1 ) )

      do i = 2, nparts
         ptotls( i ) = anint ( ptotls( i-1 ) + pstop( i ) - pstart( i ))
      end do

!
!     The partition corresponding to the input ticks is the first one
!     whose tick total is greater than the input value.  The one
!     exception is when the input ticks is equal to the total number
!     of ticks represented by all the partitions.  In this case the
!     partition number is the last one, i.e. NPARTS.
!
!     Error if TICKS comes before the first partition (that is, if it's
!     negative), or after the last one.
!
      if ( ticks == ptotls (nparts) )  then
         part = nparts
      else
         part = lstled ( ticks, nparts, ptotls ) + 1
      end if

      if ( ( ticks < 0.0_wp ).or.( part > nparts ) ) then

         call ctx%setmsg ( 'Value for ticks, #, does not fall in any ' // &
                       'partition for spacecraft #.'  )
         call ctx%errdp  ( '#', ticks                     )
         call ctx%errint ( '#', sc                        )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'       )
         return

      end if

!
!     To get the count in this partition, subtract off the total of
!     the preceding partition counts and add the beginning count for
!     this partition.
!
      if ( part == 1 ) then
         ticks = ticks + pstart( part )
      else
         ticks = ticks + pstart( part ) - ptotls( part - 1 )
      end if

!
!     Now create the output SCLK clock string.
!
!     First convert from ticks to clock string format.
!
      call scfmt ( ctx, sc, ticks, sclkch )

      if ( ctx%failed() ) return

!
!     Now convert the partition number to a character string and prefix
!     it to the output string.
!
      call intstr ( part, prtstr )

      call suffix ( '/', 0, prtstr )

      prelen = len_trim(prtstr)
      suflen = len_trim(sclkch)

      if ( len ( sclkch ) - suflen < prelen ) then

         call ctx%setmsg ( 'Output string too short to contain clock '    // &
                       'string. Input tick value: #, requires string '// &
                       'of length #, but declared length is #.'       )
         call ctx%errdp  ( '#', sclkdp )
         call ctx%errint ( '#', prelen + suflen     )
         call ctx%errint ( '#', len ( sclkch )      )
         call ctx%sigerr ( 'SPICE(SCLKTRUNCATED)'   )
         return

      end if

      call prefix ( prtstr, 0, sclkch )

      return
      end subroutine scdecd

      !> Encode a character representation of spacecraft clock time into a
      !> double precision number.
      !>
      !>## Detailed Input
      !>
      !>     SC       is the standard NAIF ID of the spacecraft whose clock's
      !>              time is being encoded.
      !>
      !>     SCLKCH   is the character representation of some spacecraft's
      !>              clock count.
      !>
      !>              SCLKCH will have the following general format:
      !>
      !>                 'pp/sclk_string', or just
      !>                    'sclk_string'
      !>
      !>              'pp' is an integer greater than or equal to one
      !>              and is called the partition number.
      !>
      !>              Each mission is divided into some number of partitions.
      !>              A new partition starts when the spacecraft clock
      !>              resets, either to zero, or to some other
      !>              value. Thus, the first partition for any mission
      !>              starts with launch, and ends with the first clock
      !>              reset. The second partition starts immediately when
      !>              the first stopped, and so on.
      !>
      !>              In order to be completely unambiguous about a
      !>              particular time, you need to specify a partition number
      !>              along with the standard clock string.
      !>
      !>              Information about when partitions occur for different
      !>              missions is contained in a spacecraft clock kernel
      !>              file, which needs to be loaded into the kernel pool,
      !>              using the routine FURNSH.
      !>
      !>              The routine SCPART is used to read the partition
      !>              start and stop times, in encoded units of SCLK (called
      !>              "ticks" -- see SCLKDP below) from the kernel file.
      !>
      !>              If the partition number is included, it must be
      !>              separated from the rest of the string by a '/'.
      !>              Any number of spaces may separate the partition number,
      !>              the '/', and the rest of the clock string.
      !>
      !>              If the partition number is omitted, a default partition
      !>              will be assumed. The default partition is the lowest-
      !>              numbered partition that contains the given clock time.
      !>              If the clock time does not fall in any of the
      !>              partition boundaries then an error is signaled.
      !>
      !>              'sclk_string' is a spacecraft specific clock string.
      !>              Using Galileo as an example, the full format is
      !>
      !>                 wwwwwwww:xx:y:z
      !>
      !>              where z is a mod-8 counter (values 0-7) which
      !>              increments approximately once every 8 1/3 ms., y is a
      !>              mod-10 counter (values 0-9) which increments once
      !>              every time z turns over, i.e., approximately once every
      !>              66 2/3 ms., xx is a mod-91 (values 0-90) counter
      !>              which increments once every time y turns over, i.e.,
      !>              once every 2/3 seconds. wwwwwwww is the Real-Time Image
      !>              Count (RIM), which increments once every time xx turns
      !>              over, i.e., once every 60 2/3 seconds. The roll-over
      !>              expression for the RIM is 16777215, which corresponds
      !>              to approximately 32 years.
      !>
      !>              wwwwwwww, xx, y, and z are referred to interchangeably
      !>              as the fields or components of the spacecraft clock.
      !>              SCLK components may be separated by any of these
      !>              five characters: ' '  ':'  ','  '-'  '.'
      !>              Any number of spaces can separate the components and
      !>              the delimiters. The presence of the RIM component
      !>              is required. Successive components may be omitted, and
      !>              in such cases are assumed to represent zero values.
      !>
      !>              Values for the individual components may exceed the
      !>              maximum expected values. For instance, '0:0:0:9' is
      !>              an acceptable Galileo clock string, and will convert
      !>              to the same number of ticks as '0:0:1:1'.
      !>
      !>              Consecutive delimiters containing no intervening digits
      !>              are treated as if they delimit zero components.
      !>
      !>              Trailing zeros should always be included to match the
      !>              length of the counter. For example, a Galileo clock
      !>              count of '25684.90' should not be represented as
      !>              '25684.9'.
      !>
      !>              Some spacecraft clock components have offset, or
      !>              starting, values different from zero. For example,
      !>              with an offset value of 1, a mod 20 counter would
      !>              cycle from 1 to 20 instead of from 0 to 19.
      !>
      !>              See the SCLK required reading for a detailed
      !>              description of the Voyager and Mars Observer clock
      !>              formats.
      !>
      !>## Detailed Output
      !>
      !>     SCLKDP   is the double precision encoding of SCLKCH.
      !>
      !>              The encoding is such that order and proximity will be
      !>              preserved. That is, if t1, t2, and t3 are spacecraft
      !>              clock times, and t1*, t2*, and t3* are their encodings,
      !>              then if
      !>
      !>                 t1 < t2 < t3, and
      !>
      !>              t2 is closer to t1 than to t3, you will have the result
      !>              that
      !>
      !>                 t1* < t2* < t3*, and
      !>
      !>              t2* is closer to t1* than to t3*.
      !>
      !>              The units of encoded SCLK are "ticks since the start of
      !>              the mission", where a "tick" is defined to be the
      !>              shortest time increment expressible by a particular
      !>              spacecraft's clock.
      !>
      !>              Each clock string without partition number represents
      !>              a certain number of ticks, but you need to include
      !>              partition information to determine the relative
      !>              position of that time in relation to the start of the
      !>              mission.
      !>
      !>              Since the end time of one partition is coincident
      !>              with the begin time of the next, there are two
      !>              different representations for this instant, and they
      !>              will both yield the same encoding.
      !>
      !>              For example, if partition 1 has an end time of t1, and
      !>              partition 2 has a begin time of t2, then if we executed
      !>              the code fragment
      !>
      !>                 CALL SCENCD ( '1/t1', SC, X )
      !>                 CALL SCENCD ( '2/t2', SC, Y )
      !>
      !>              we would obtain X = Y.
      !>
      !>## Parameters
      !>
      !>     MXPART   is the maximum number of spacecraft clock partitions
      !>              expected in the kernel file for any one spacecraft.
      !>              See the INCLUDE file sclk.inc for this parameter's
      !>              value.
      !>
      !>## Exceptions
      !>
      !>     1)  If the number of partitions in the kernel file for spacecraft
      !>         SC exceeds the parameter MXPART, the error
      !>         SPICE(TOOMANYPARTS) is signaled.
      !>
      !>     2)  If any of the extracted clock components cannot be parsed as
      !>         integers, or the string has too many components, or the value
      !>         of one of the components is less than the offset value,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     If a partition number is included in the SCLK string, the
      !>     following exceptions may occur:
      !>
      !>     3)  If the partition number cannot be parsed as an integer, the
      !>         error SPICE(BADPARTNUMBER) is signaled.
      !>
      !>     4)  If the partition number is not in the range of the number of
      !>         partitions found in the kernel pool, the error
      !>         SPICE(BADPARTNUMBER) is signaled.
      !>
      !>     5)  If the clock count does not fall in the boundaries of the
      !>         specified partition, the error SPICE(NOTINPART) is
      !>         signaled.
      !>
      !>     If a partition number is not included in the SCLK string, the
      !>     following exception may occur.
      !>
      !>     6)  If the clock count does not fall in the boundaries of any
      !>         partition found in the kernel pool, the error
      !>         SPICE(NOPARTITION) is signaled.
      !>
      !>     7)  If the partition delimiter (slash) is first found in the last
      !>         position of SCLKCH, the error SPICE(INVALIDSCLKSTRING) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     A kernel file containing spacecraft clock partition information
      !>     for the desired spacecraft must be loaded, using the routine
      !>     FURNSH, before calling this routine.
      !>
      !>## Particulars
      !>
      !>     In general, it is difficult to compare spacecraft clock counts
      !>     numerically since there are too many clock components for a
      !>     single comparison. This routine provides a method of assigning a
      !>     single double precision number to a spacecraft's clock count,
      !>     given one of its character representations.
      !>
      !>     The routine SCDECD performs the inverse operation of SCENCD,
      !>     converting an encoded double precision number to character format.
      !>
      !>     To convert the string to ticks since the start of the mission,
      !>     SCENCD
      !>
      !>        1) Converts the non-partition portion of the string to
      !>           ticks, using the routine SCTIKS.
      !>
      !>        2) Determines the partition number for the clock time,
      !>           either by getting it directly from the input string, or
      !>           determining the default partition if none was specified.
      !>
      !>        3) Includes partition start and stop times, which are also
      !>           measured in ticks, to compute the number of ticks
      !>           from the beginning of the mission to the clock time.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as input,
      !>     the compiler and supporting libraries, and the machine specific
      !>     arithmetic implementation.
      !>
      !>     1) Double precision encodings of spacecraft clock counts are used
      !>        to tag pointing data in the C-kernel.
      !>
      !>        In the following example, pointing for a sequence of images
      !>        from the CASSINI Imaging Science Subsystem (ISS) is requested
      !>        from the C-kernel using an array of character spacecraft clock
      !>        counts as input. The clock counts attached to the output are
      !>        then decoded to character and compared with the input strings.
      !>
      !>        Use the CK kernel below to load the CASSINI image navigated
      !>        spacecraft pointing and orientation data.
      !>
      !>           04153_04182ca_ISS.bc
      !>
      !>        Use the SCLK kernel below to load the CASSINI spacecraft clock
      !>        time correlation data required for the conversion between
      !>        spacecraft clock string representation and double precision
      !>        encoding of spacecraft clock counts.
      !>
      !>           cas00071.tsc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM SCENCD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>C     The instrument we want pointing for is the CASSINI
      !>C     spacecraft. The reference frame we want is
      !>C     J2000. The spacecraft is CASSINI.
      !>C
      !>      INTEGER               SC
      !>      PARAMETER           ( SC     = -82 )
      !>
      !>      INTEGER               INST
      !>      PARAMETER           ( INST   = -82000 )
      !>
      !>      CHARACTER*(*)         REF
      !>      PARAMETER           ( REF    = 'J2000' )
      !>
      !>      CHARACTER*(*)         CK
      !>      PARAMETER           ( CK     = '04153_04182ca_ISS.bc' )
      !>
      !>      CHARACTER*(*)         SCLK
      !>      PARAMETER           ( SCLK   = 'cas00071.tsc' )
      !>
      !>      INTEGER               NPICS
      !>      PARAMETER           ( NPICS  = 4 )
      !>
      !>      CHARACTER*(*)         CLKTOL
      !>      PARAMETER           ( CLKTOL = '1.0' )
      !>
      !>      INTEGER               MAXLEN
      !>      PARAMETER           ( MAXLEN = 30 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(25)        SCLKIN (4)
      !>      CHARACTER*(25)        SCLKOUT
      !>
      !>      DOUBLE PRECISION      CMAT   (3,3)
      !>      DOUBLE PRECISION      TIMEIN
      !>      DOUBLE PRECISION      TIMEOUT
      !>      DOUBLE PRECISION      TOL
      !>
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               K
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Set the input SCLK strings.
      !>C
      !>      DATA                  SCLKIN /  '1/1465644279.0',
      !>     .                                '1/1465644281.0',
      !>     .                                '1/1465644351.0',
      !>     .                                '1/1465644361.0'  /
      !>
      !>C
      !>C     Load the appropriate files. We need
      !>C
      !>C        1. CK file containing pointing data.
      !>C        2. Spacecraft clock kernel file.
      !>C
      !>      CALL FURNSH ( CK   )
      !>      CALL FURNSH ( SCLK )
      !>
      !>C
      !>C     Convert the tolerance string to ticks.
      !>C
      !>      CALL SCTIKS ( SC, CLKTOL, TOL )
      !>
      !>      DO I= 1, NPICS
      !>
      !>         CALL SCENCD ( SC, SCLKIN(I), TIMEIN )
      !>
      !>         CALL CKGP ( INST, TIMEIN,  TOL,  REF,
      !>     .               CMAT, TIMEOUT, FOUND     )
      !>
      !>         WRITE(*,*)
      !>         WRITE(*,'(2A)') 'Input s/c clock count : ', SCLKIN(I)
      !>
      !>         IF ( FOUND ) THEN
      !>
      !>            CALL SCDECD ( SC, TIMEOUT, SCLKOUT )
      !>
      !>            WRITE(*,'(2A)') 'Output s/c clock count: ',
      !>     .                                          SCLKOUT
      !>            WRITE(*,'(A)') 'Output C-Matrix:'
      !>
      !>            DO J = 1, 3
      !>
      !>               WRITE(*,'(3F21.15)') ( CMAT(J,K), K = 1, 3 )
      !>
      !>            END DO
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A)') 'No pointing found.'
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
      !>        Input s/c clock count : 1/1465644279.0
      !>        No pointing found.
      !>
      !>        Input s/c clock count : 1/1465644281.0
      !>        Output s/c clock count: 1/1465644281.171
      !>        Output C-Matrix:
      !>           -0.335351455948710    0.864374440205611    0.374694846658341
      !>           -0.937887426812980   -0.343851965210223   -0.046184419961653
      !>            0.088918927227039   -0.366909598048763    0.925997176691424
      !>
      !>        Input s/c clock count : 1/1465644351.0
      !>        Output s/c clock count: 1/1465644351.071
      !>        Output C-Matrix:
      !>           -0.335380929397586    0.864363638262230    0.374693385378623
      !>           -0.937874292008090   -0.343889838107825   -0.046169163264003
      !>            0.088946301703530   -0.366899550417080    0.925998528787713
      !>
      !>        Input s/c clock count : 1/1465644361.0
      !>        No pointing found.
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
      !>     J.M. Lynch         (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     R.E. Thurman       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 21-NOV-2021 (NJB) (JDR)
      !>
      !>        Now variables PSTART, PSTOP, and PTOTLS are saved. Made minor
      !>        changes to formatting of code.
      !>
      !>        Bug fix: out-of-range character positions of SCLKCH are
      !>        no longer referenced.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example fragments using PDS
      !>        archived CASSINI data.
      !>
      !>        Removed wrong reference to routine CLPOOL from SCLKCH detailed
      !>        description and $Files section, and reference to nonexistent
      !>        TIKSnn routines.
      !>
      !>        Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.2.0, 28-FEB-2014 (BVS)
      !>
      !>        Added FAILED checks to prevent passing uninitialized values to
      !>        ANINT, which can causing numeric exceptions on some
      !>        environments.
      !>
      !>-    SPICELIB Version 1.1.0, 05-FEB-2008 (NJB)
      !>
      !>        The values of the parameter MXPART is now
      !>        provided by the INCLUDE file sclk.inc.
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
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (JML) (RET)
      module subroutine scencd ( ctx, sc, sclkch, sclkdp )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: cpos, nparsi
      use pepper_inc_sclk
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft identification code.
      character(*), intent(in) :: sclkch  !! Character representation of a spacecraft clock.
      real(wp), intent(out) :: sclkdp  !! Encoded representation of the clock count.
      real(wp), allocatable :: pstart(:), pstop(:), ptotls(:)
      integer(ip) :: npmax
      integer(ip) :: pos
      integer(ip) :: nparts
      integer(ip) :: pnter
      integer(ip) :: part
      integer(ip) :: i
      real(wp) :: ticks
      character(25) :: error
      if ( ctx%failed() ) return

!
!     Read the partition start and stop times (in ticks) for this
!     mission. Error if there are too many of them.
!
!
!     PEPPER: the partitions go to arrays of the clock's size (SPICELIB
!     used arrays of MXPART, 9999, and SPICE(TOOMANYPARTS) for more).
!
      npmax = sclk_partition_count ( ctx, sc )
      allocate ( pstart(npmax), pstop(npmax), ptotls(npmax) )
      call scpart ( ctx, sc, nparts, pstart, pstop )

      if ( ctx%failed() ) return

!
!     Convert the non-partition portion of the clock string to ticks.
!
      pos = cpos ( sclkch, '/', 1 )

      if ( pos == 1 ) then

         call ctx%setmsg ( 'Unable to parse the partition number '   // &
                       'from SCLK string #.'  )
         call ctx%errch  ( '#', sclkch            )
         call ctx%sigerr ( 'SPICE(BADPARTNUMBER)' )
         return

      end if

      if ( pos == len(sclkch) ) then

         call ctx%setmsg ( 'No SCLK components follow ' &
         //            'slash in SCLK string #.'  )
         call ctx%errch  ( '#', sclkch                )
         call ctx%sigerr ( 'SPICE(INVALIDSCLKSTRING)' )
         return

      end if

      call sctiks ( ctx,  sc, sclkch( pos+1 : ), ticks )

      if ( ctx%failed() ) return

      ticks = anint ( ticks )

!
!     PSTART and PSTOP represent integers but are read from the
!     kernel pool as double precision numbers. Make them whole
!     numbers so that logical tests may be performed with them.
!
      do i = 1, nparts
         pstop (i) = anint ( pstop(i)  )
         pstart(i) = anint ( pstart(i) )
      end do

!
!     For each partition, compute the total number of ticks in that
!     partition plus all preceding partitions.
!
      ptotls( 1 ) = anint ( pstop( 1 ) - pstart( 1 ) )

      do i = 2, nparts
         ptotls( i ) = anint ( ptotls( i-1 ) + pstop( i ) - pstart( i ))
      end do

!
!     Determine the partition number for the input clock string:
!
!        If it was included in the string make sure it's valid for
!        this mission.
!
!           Error if
!
!           1) The partition number can't be parsed.
!           2) The partition number is not in the range 1 to the number
!              of partitions.
!           3) The clock count does not fall in the boundaries of the
!              specified partition.
!
!        If it wasn't included, determine the default partition for
!        this clock count.
!
!           Error if
!
!           1) The clock count does not fall in the boundaries of any
!              of the partitions.
!
      if ( pos > 1 ) then

         part = 0

         call nparsi ( sclkch( :pos-1 ), part, error, pnter )

         if ( error /= ' ' ) then

            call ctx%setmsg ( 'Unable to parse the partition number '   // &
                          'from SCLK string #.'  )
            call ctx%errch  ( '#', sclkch            )
            call ctx%sigerr ( 'SPICE(BADPARTNUMBER)' )
            return

         else if ( part <= 0 .or. part > nparts ) then

            call ctx%setmsg ( 'Partition number # taken from SCLK string '// &
                          '# is not in acceptable range 1 to #.'     )
            call ctx%errint ( '#', part              )
            call ctx%errch  ( '#', sclkch            )
            call ctx%errint ( '#', nparts            )
            call ctx%sigerr ( 'SPICE(BADPARTNUMBER)' )
            return

         else if ( ticks < pstart( part )   .or. &
                   ticks > pstop ( part ) )  then

            call ctx%setmsg ( 'SCLK count # does not fall in the '        // &
                          'boundaries of partition number #.'  )
            call ctx%errch  ( '#', sclkch        )
            call ctx%errint ( '#', part          )
            call ctx%sigerr ( 'SPICE(NOTINPART)' )
            return

         end if

      else

!
!        PEPPER: a counted loop (Fortran's .AND. does not short-circuit:
!        SPICELIB's loop read PSTART(NPARTS+1), past the partitions).
!
         do part = 1, nparts
            if ( ticks >= pstart( part ) .and. ticks <= pstop( part ) ) exit
         end do

         if ( part > nparts ) then

            call ctx%setmsg ( 'SCLK count # does not fall in the '        // &
                          'boundaries of any of the partitions for '  // &
                          'spacecraft #.'      )
            call ctx%errch  ( '#', sclkch          )
            call ctx%errint ( '#', sc              )
            call ctx%sigerr ( 'SPICE(NOPARTITION)' )
            return

         end if

      end if

!
!     Now we have a valid partition number, and the number of ticks for
!     the clock string. To convert to ticks since the start of the
!     mission, add in the total number of ticks in preceding partitions
!     and subtract off the starting ticks value for this partition.
!
      if ( part > 1 ) then
         sclkdp = ticks - pstart( part ) + ptotls( part - 1 )
      else
         sclkdp = ticks - pstart( part )
      end if

      return
      end subroutine scencd

      !> Convert an epoch specified as ephemeris seconds past J2000 (ET) to
      !> a character string representation of a spacecraft clock value
      !> (SCLK).
      !>
      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft clock whose
      !>              reading at the epoch specified by ET is desired.
      !>
      !>     ET       is an epoch, specified as ephemeris seconds past
      !>              J2000.
      !>
      !>## Detailed Output
      !>
      !>     SCLKCH   is a character string representation of the
      !>              spacecraft clock value that corresponds to ET, for
      !>              the spacecraft clock specified by the input
      !>              argument SC. SCLKCH is an absolute spacecraft
      !>              clock value, so a partition number is included in
      !>              the string. The format of SCLKCH is specified in
      !>              the SCLK kernel for the clock SC. A general
      !>              discussion of spacecraft clock string formats is
      !>              available in the SCLK Required Reading.
      !>
      !>              In order to choose an appropriate length for
      !>              SCLKCH, you can examine an SCLK kernel for the
      !>              clock specified by SC. The format of string
      !>              representations of the clock's values is specified
      !>              by kernel variables associated with the clock. See
      !>              $Examples below for further information.
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
      !>         output argument SCLKCH will not be modified. This routine
      !>         assumes that an SCLK kernel appropriate to the spacecraft
      !>         clock identified by the input argument SC has been loaded.
      !>
      !>     2)  If a leapseconds kernel is required for conversion between
      !>         SCLK and ET but is not loaded, an error is signaled by a
      !>         routine in the call tree of this routine. The output argument
      !>         SCLKCH will not be modified. When using an SCLK kernel that
      !>         maps SCLK to a time system other than ET (also called
      !>         barycentric dynamical time---`TDB'), it is necessary to have a
      !>         leapseconds kernel loaded at the time this routine is called.
      !>
      !>         The time system to which an SCLK kernel maps SCLK epochs is
      !>         indicated by the variable SCLK_TIME_SYSTEM_nn in the kernel,
      !>         where nn is the negative of the NAIF integer code for the
      !>         spacecraft. The time system used in a kernel is TDB if and
      !>         only if the variable is assigned the value 1.
      !>
      !>     3)  If the input ET value is not representable in the spacecraft
      !>         clock string format for the spacecraft clock identified by SC,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine. The output argument SCLKCH will not be modified.
      !>
      !>     4)  If the output argument SCLKCH is too short to contain the
      !>         output spacecraft clock string produced by this routine, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. The output argument SCLKCH may contain a portion of
      !>         the truncated string.
      !>
      !>## Files
      !>
      !>     An SCLK kernel, appropriate to the spacecraft clock identified
      !>     by SC, must be loaded at the time this routine is called.
      !>
      !>     If the SCLK kernel used with this routine does not map SCLK
      !>     directly to barycentric dynamical time, a leapseconds kernel
      !>     must be loaded at the time this routine is called.
      !>
      !>## Particulars
      !>
      !>     This routine is provided as a convenience; it is simply shorthand
      !>     for the code fragment
      !>
      !>        CALL SCE2T  ( SC,  ET,      SCLKDP )
      !>        CALL SCDECD ( SC,  SCLKDP,  SCLKCH )
      !>
      !>     See the SCLK Required Reading for a list of the entire set of
      !>     SCLK conversion routines.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1)  Determine the length of Galileo spacecraft clock strings.
      !>
      !>         Examine a Galileo SCLK kernel. There you'll find the
      !>         kernel variable assignments
      !>
      !>            SCLK01_MODULI_77          = ( 16777215 91 10 8 )
      !>            SCLK01_OFFSETS_77         = (        0  0  0 0 )
      !>
      !>         Each field of the clock string contains values ranging
      !>         from the offset value to M-1, where M is the corresponding
      !>         modulus. So the Galileo clock fields have maximum values
      !>
      !>            16777214 90 9 7
      !>
      !>         representing the partition number by the symbol "pp" and
      !>         the field delimiter character by the symbol "D", we see
      !>         that the GLL SCLK format is
      !>
      !>            pp/xxxxxxxxDxxDxDx
      !>
      !>         This string has length 18 characters. Accounting for the
      !>         terminating null character, the value of `lenout' should
      !>         be set to at least 19.
      !>
      !>         Note:  the delimiter character is determined by the integer
      !>         code assignment
      !>
      !>            SCLK01_OUTPUT_DELIM_77    = (                2 )
      !>
      !>         The SCLK Required Reading indicates that 2 is the SCLK kernel
      !>         code for the colon character.
      !>
      !>     2)  Find the Galileo SCLK value corresponding to the ET
      !>
      !>            -322452420.5593641.
      !>
      !>            C
      !>            C     Start out by loading the SCLK kernel. In your own
      !>            C     program, you must use the name of a real SCLK kernel.
      !>            C     The name shown here is fictitious.
      !>            C
      !>                  CALL FURNSH ( 'GLLSCLK.KER' )
      !>
      !>            C
      !>            C     Load a leapseconds kernel in case it is needed for
      !>            C     SCLK-to-ET conversion. Depending on the SCLK kernel
      !>            C     used, it may not be necessary to load this file; it's
      !>            C     just a simple, reliable way of making sure that the
      !>            C     leapseconds kernel constants are available if we need
      !>            C     them. Again, a fictitious name is used.
      !>            C
      !>                  CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>
      !>            C
      !>            C     The spacecraft ID code for Galileo is -77.
      !>            C
      !>                  SC = -77
      !>                  ET = -322452420.5593641
      !>
      !>                  CALL SCE2S ( SC, ET, SCLKCH )
      !>
      !>         The returned value of SCLKCH will be
      !>
      !>            1/00010001:44:2:0.
      !>
      !>     3)  Convert the UTC time
      !>
      !>            August 25 1989 4:00:00
      !>
      !>         to a Voyager 2 SCLK value.
      !>
      !>         To enable you to perform UTC to ET conversion, your
      !>         initialization code must load the leapseconds and SCLK
      !>         kernels:
      !>
      !>            C
      !>            C     Load leapseconds and SCLK kernels:
      !>            C
      !>                  CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>                  CALL FURNSH ( 'VGR2SCLK.KER'    )
      !>
      !>         To find Voyager 2 SCLK string corresponding to the
      !>         specified UTC time, you can use the code fragment
      !>
      !>                  CALL UTC2ET ( 'Aug 25 1989 4:00:00',  ET     )
      !>                  CALL SCE2S  ( -32,        ET,         SCLKCH )
      !>
      !>         The result of the conversion is
      !>
      !>            SCLKCH  =  '4/11390:22:012'
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
      !>-    SPICELIB Version 1.3.0, 05-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries in $Revisions section.
      !>
      !>-    SPICELIB Version 1.2.2, 29-JUL-2003 (NJB)
      !>
      !>        Various header changes were made to improve clarity and
      !>        more fully explain the routine's functionality.
      !>
      !>-    SPICELIB Version 1.2.1, 09-MAR-1999 (NJB)
      !>
      !>        Explicit list of SCLK conversion routines in $Particulars
      !>        section has been replaced by a pointer to the SCLK Required
      !>        Reading.
      !>
      !>-    SPICELIB Version 1.2.0, 10-APR-1992 (NJB) (WLT)
      !>
      !>        Truncation of the output string is now treated as an error.
      !>        Header was updated to reflect possibility of needing to load
      !>        a leapseconds kernel before calling this routine. Comment
      !>        section for permuted index source lines was added following the
      !>        header.
      !>
      !>-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
      !>
      !>        Missing example added to the $Examples section.  $Restrictions
      !>        section no longer states that you must load the leapseconds
      !>        kernel prior to calling this routine.
      !>
      !>        The second example no longer uses a call to CLPOOL.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 10-APR-1992 (NJB) (WLT)
      !>
      !>        Truncation of the output string is now treated as an error.
      !>        The code changes made to implement the error checking were
      !>        in SCDECD and other lower-level routines.
      module subroutine sce2s ( ctx, sc, et, sclkch )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft clock ID code.
      real(wp), intent(in) :: et  !! Ephemeris time, specified as seconds past J2000.
      character(*), intent(out) :: sclkch  !! An SCLK string.

      real(wp) :: sclkdp

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Convert ET to encoded SCLK, and then to an SCLK string.
!
      call sce2t  ( ctx, sc, et,     sclkdp )
      call scdecd ( ctx, sc, sclkdp, sclkch )

      return
      end subroutine sce2s

      !> Convert a spacecraft clock string to ephemeris seconds past
      !> J2000 (ET).
      !>
      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft, one of whose
      !>              clock values is represented by SCLKCH. The set of
      !>              supported spacecraft clocks is listed in the SCLK
      !>              Required Reading.
      !>
      !>     SCLKCH   is a character string representation of the
      !>              spacecraft clock value that corresponds to ET, for
      !>              the spacecraft specified by the input argument SC.
      !>              SCLKCH is an absolute spacecraft clock time, so
      !>              partition information should be included in this
      !>              string. The precise format of SCLKCH is specified
      !>              in the SCLK Required Reading.
      !>
      !>## Detailed Output
      !>
      !>     ET       is the epoch, specified as ephemeris seconds past
      !>              J2000, that corresponds to SCLKCH.
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
      !>         output argument ET will not be modified. This routine assumes
      !>         that that an SCLK kernel appropriate to the spacecraft clock
      !>         identified by the input argument SC has been loaded.
      !>
      !>     2)  If a leapseconds kernel is required for conversion between
      !>         SCLK and ET but is not loaded, an error is signaled by a
      !>         routine in the call tree of this routine. The output argument
      !>         ET will not be modified. When using SCLK kernels that map SCLK
      !>         to a time system other than ET (also called barycentric
      !>         dynamical time---`TDB'), it is necessary to have a leapseconds
      !>         kernel loaded at the time this routine is called.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If the value of SCLKCH is invalid, an error is signaled by a
      !>         routine in the call tree of this routine. The output argument
      !>         ET will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is provided as a convenience; it is simply shorthand
      !>     for the code fragment
      !>
      !>        CALL SCENCD ( SC, SCLKCH, SCLKDP )
      !>        CALL SCT2E  ( SC, SCLKDP, ET     )
      !>
      !>     See the SCLK Required Reading for a list of the entire set of
      !>     SCLK conversion routines.
      !>
      !>## Examples
      !>
      !>     1)  Find the state (position and velocity) of Jupiter, as seen
      !>         from the Galileo spacecraft, at the epoch corresponding to
      !>         the SCLK value
      !>
      !>            2 / 3110578:89:09
      !>
      !>         The digit `2', followed by the forward slash, indicates that
      !>         the time value is in the second mission partition.
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
      !>               C
      !>               C     Load an SPK file (again, a fictitious file)
      !>               C     containing an ephemeris for Jupiter and the
      !>               C     GLL orbiter's trajectory.
      !>               C
      !>                     CALL SPKLEF ( 'GLLSPK.KER', HANDLE )
      !>
      !>            The Galileo spacecraft ID is -77.  Convert our SCLK
      !>            string to ephemeris seconds past J2000, which is the
      !>            time representation expected by SPKEZ.
      !>
      !>                     CALL SCS2E ( -77, '2 / 3110578:89:09', ET )
      !>
      !>            Find the state of Jupiter (body 599) as seen from Galileo
      !>            at time ET.  To use SPKEZ, you must first load an SPK
      !>            kernel, using the routine SPKLEF.
      !>
      !>                     CALL SPKEZ ( 599,
      !>                    .             ET,
      !>                    .             REFSYS,
      !>                    .             CORR,
      !>                    .             -77,
      !>                    .             STATE,
      !>                    .             LT      )
      !>
      !>     2)  Convert a Voyager 2 SCLK value to UTC, using calendar format,
      !>         with 3 digits of precision in the seconds component.
      !>
      !>            Again, your initialization code must load the leapseconds
      !>            and SCLK kernels:
      !>
      !>               C
      !>               C     Load leapseconds and SCLK kernels:
      !>               C
      !>                     CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>                     CALL FURNSH ( 'VGR2SCLK.KER'    )
      !>
      !>            To find the UTC value corresponding to Voyager 2 SCLK
      !>            string
      !>
      !>                     11389.20.768
      !>
      !>            you can use the code fragment
      !>
      !>                     CALL SCS2E  ( -32,  '11389.29.768',  ET  )
      !>                     CALL ET2UTC (  ET,  'C',      3,     UTC )
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
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
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
      !>        Moved the required readings present in $Literature_References
      !>        section to $Required_Reading and added CK to the list.
      !>
      !>-    SPICELIB Version 1.0.4, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 1.0.3, 09-MAR-1999 (NJB)
      !>
      !>        Explicit list of SCLK conversion routines in $Particulars
      !>        section has been replaced by a pointer to the SCLK Required
      !>        Reading.
      !>
      !>-    SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)
      !>
      !>        The $Brief_I/O section now lists ET correctly as an output
      !>        from this routine. Header was updated to reflect possibility
      !>        of needing to load a leapseconds kernel before calling this
      !>        routine. Comment section for permuted index source lines was
      !>        added following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
      !>
      !>        $Restrictions section no longer states that you must load the
      !>        leapseconds kernel prior to calling this routine.
      !>
      !>        The examples have been slightly re-written. In particular,
      !>        they no longer use calls to CLPOOL.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
      module subroutine scs2e ( ctx, sc, sclkch, et )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF integer code for a spacecraft.
      character(*), intent(in) :: sclkch  !! An SCLK string.
      real(wp), intent(out) :: et  !! Ephemeris time, seconds past J2000.

      real(wp) :: sclkdp

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Encode SCLKCH, and convert the result to ET.
!
      call scencd ( ctx, sc, sclkch, sclkdp )
      call sct2e  ( ctx, sc, sclkdp, et     )

      return
      end subroutine scs2e

end submodule pepper_sclk_api
