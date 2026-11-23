!> The kernel pool: variables loaded from text kernels or set by the program (ldpool, gdpool,
!> pdpool, ...), reading text kernels, and the watchers that tell subsystems when variables change.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## ZZCTR (Manipulate Counter Array)
!>
!> PEPPER: SPICELIB's umbrella routine ZZCTR is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzctr. Its documentation follows.
!>
!>### Abstract
!>
!>     Manipulate counter array.
!>
!>### Detailed Input
!>
!>     See the ENTRY points for a discussion of their arguments.
!>
!>### Detailed Output
!>
!>     See the ENTRY points for a discussion of their arguments.
!>
!>### Parameters
!>
!>     CTRSIZ      is the dimension of the counter array used by
!>                 various SPICE subsystems to identify
!>                 changes in their states. This parameter is
!>                 defined in the include file.
!>
!>### Exceptions
!>
!>     1) If ZZCTR is called directly, the error SPICE(BOGUSENTRY) is
!>        signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     ZZCTR should never be called directly, but should instead be
!>     accessed only through its entry points.
!>
!>     The purpose of this routine is to manipulate counter arrays used
!>     by various SPICE subsystems to identify changes in their states.
!>
!>           ZZCTRUIN       Initialize counter array kept by a user
!>                          routine.
!>
!>           ZZCTRSIN       Initialize counter array kept by a
!>                          subsystem routine.
!>
!>           ZZCTRINC       Increment counter array.
!>
!>           ZZCTRCHK       Check/update counter array.
!>
!>     A counter array consists of CTRSIZ elements representing
!>     cascading counters. The fastest counter is at index 1, the
!>     slowest counter is at index CTRSIZ. At the start of counting all
!>     counter array elements are set to INTMIN. In the process of
!>     counting the fastest element is incremented by one. As with any
!>     cascading counters when the fastest counter reaches INTMAX it
!>     rolls back to INTMIN and the next counter is incremented by 1.
!>     When all counters reach INTMAX, ZZCTRINC signals an error.
!>
!>### Examples
!>
!>     The counter array gives subsystems like POOL a way to provide to
!>     the user routines of that subsystem's services a simple indicator
!>     of whether the subsystem's state has changed, meaning that some
!>     saved values may have become stale and need to be updated or some
!>     other action needs to be taken in the user routines.
!>
!>     For that the subsystem initializes a counter array ("subsystem
!>     counter") using ZZCTRSIN, saves it, increments it using ZZCTRINC
!>     each time the subsystem's state changes, and resets a user
!>     counter to subsystem counter and indicates the need for update
!>     when counters are not the same, like this:
!>
!>        C
!>        C     Include zzctr.inc to access CTRSIZ.
!>        C
!>              INCLUDE              'zzctr.inc'
!>              ...
!>        C
!>        C     In input/output variable declarations declare user
!>        C     user counter (USRCTR) as I/O and the update flag
!>        C     (UPDATE) as O.
!>        C
!>              INTEGER               USRCTR ( CTRSIZ )
!>              LOGICAL               UPDATE
!>              ...
!>        C
!>        C     In local variable declarations declare and save the
!>        C     subsystem counter (SUBCTR) that will incremented
!>        C     with each change of state.
!>        C
!>              INTEGER               SUBCTR ( CTRSIZ )
!>              ...
!>              SAVE                  SUBCTR
!>              ...
!>        C
!>        C     In all places where initialization is done set
!>        C     subsystem counter using ZZCTRSIN.
!>        C
!>              IF ( FIRST ) THEN
!>                 ...
!>                 CALL ZZCTRSIN( SUBCTR )
!>                 FIRST = .FALSE.
!>              END IF
!>              ...
!>        C
!>        C     In all places where the subsystem state changes
!>        C     increment the subsystem counter using ZZCTRINC.
!>        C
!>              CALL ZZCTRINC( SUBCTR )
!>              ...
!>        C
!>        C     Make a special entry that checks and resets input
!>        C     user counter to the current subsystem counter and
!>        C     indicates whether it was updated using ZZCTRCHK.
!>        C
!>              ENTRY <entry_name> ( USRCTR, UPDATE )
!>
!>              CALL ZZCTRCHK( SUBCTR, USRCTR, UPDATE )
!>
!>              RETURN
!>              ...
!>
!>     The users of the subsystem services initialize a counter array
!>     ("user counter") using ZZCTRUIN, save it, and check against the
!>     subsystem's counter and update, if needed, using the subsystem's
!>     entry point, like this:
!>
!>        C
!>        C     Include zzctr.inc to access CTRSIZ.
!>        C
!>              INCLUDE              'zzctr.inc'
!>              ...
!>        C
!>        C     In local variable declarations declare and save
!>        C     user counter. Also declare an update flag.
!>        C
!>              INTEGER               USRCTR ( CTRSIZ )
!>              LOGICAL               UPDATE
!>              ...
!>              SAVE                  USRCTR
!>              ...
!>        C
!>        C     In all places where initialization is done initialize
!>        C     the user counter using ZZCTRUIN to ensure update on
!>        C     the first check.
!>        C
!>              IF ( FIRST ) THEN
!>                 ...
!>                 CALL ZZCTRUIN( USRCTR )
!>                 FIRST = .FALSE.
!>              END IF
!>              ...
!>        C
!>        C     In all places where there is a need to check for
!>        C     the subsystem state change call subsystem's entry
!>        C     that checks and updates the user counter based on
!>        C     value of the subsystem counter.
!>        C
!>              CALL <entry_name> ( USRCTR, UPDATE )
!>
!>              IF ( UPDATE ) THEN
!>
!>        C
!>        C        Do what needs to be done when the subsystem
!>        C        state has changed.
!>        C
!>                 ...
!>
!>              END IF
!>
!>### Restrictions
!>
!>     None.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     B.V. Semenov    (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
!>
!>### Implementation notes
!>
!>     Constants (SPICELIB computed them on the first call).
!>
!>## RDKER (Read a kernel file)
!>
!> PEPPER: SPICELIB's umbrella routine RDKER is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_rdker. Its documentation follows.
!>
!>### Abstract
!>
!>     Open and read the contents of a SPICE ASCII kernel file.
!>
!>### Detailed Input
!>
!>     All input is through entry RDKNEW.
!>
!>### Detailed Output
!>
!>     All output is through entry RDKDAT.
!>
!>### Parameters
!>
!>     None.
!>
!>### Exceptions
!>
!>     1)  If RDKER is called directly, the error SPICE(BOGUSENTRY) is
!>         signaled.
!>
!>### Files
!>
!>     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
!>     by RDKDAT. The entry point RDKLIN is available for reporting
!>     the name of the open file and the number of the last line that
!>     was read from that file.
!>
!>### Particulars
!>
!>     RDKER should never be called directly, but should instead be
!>     accessed only through its entry points, RDKNEW, RDKDAT and
!>     RDKLIN.
!>
!>### Examples
!>
!>     In the following example, RDKNEW and RDKDAT are used to read
!>     the contents of a kernel file.
!>
!>     Let the file KERNEL contain the following lines.
!>
!>        =============================================================
!>
!>        DELTA_T_A is defined to be 32.184 seconds, and should not
!>        be changed except under the most unusual circumstances.
!>
!>        \begindata
!>
!>        DELTA_T_A       =   32.184
!>
!>        \begintext
!>
!>        The next three items determine the relativistic correction
!>        in the difference ET - TAI. To turn the correction off,
!>        just set K to zero.
!>
!>        \begindata
!>
!>        K               =    1.657D-3
!>        ORBIT_ECC       =    1.671D-2
!>        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!>
!>        =============================================================
!>
!>     Then the code fragment
!>
!>        CALL RDKNEW ( KERNEL )
!>        CALL RDKDAT ( LINE, EOF )
!>
!>        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
!>           WRITE (6,*) LINE
!>           CALL RDKDAT ( LINE, EOF )
!>        END DO
!>
!>     prints the following lines.
!>
!>        =============================================================
!>        DELTA_T_A       =   32.184
!>        K               =    1.657D-3
!>        ORBIT_ECC       =    1.671D-2
!>        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!>        =============================================================
!>
!>### Restrictions
!>
!>     1)  The input file must be opened and initialized by RDKNEW prior
!>         to the first call to RDKDAT.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     H.A. Neilan        (JPL)
!>     B.V. Semenov       (JPL)
!>     M.J. Spencer       (JPL)
!>     W.L. Taber         (JPL)
!>     I.M. Underwood     (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 3.7.0, 28-NOV-2021 (BVS)
!>
!>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!>
!>-    SPICELIB Version 3.6.1, 17-JUN-2021 (JDR)
!>
!>        Edited the header of the RDKER umbrella routine and all its
!>        entry entry points to comply with NAIF standard.
!>
!>-    SPICELIB Version 3.6.0, 10-MAR-2014 (BVS)
!>
!>        Updated for PC-CYGWIN-64BIT-GCC_C.
!>
!>-    SPICELIB Version 3.5.0, 13-MAY-2010 (BVS)
!>
!>        Updated for SUN-SOLARIS-INTEL-CC_C.
!>
!>-    SPICELIB Version 3.4.0, 13-MAY-2010 (BVS)
!>
!>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!>
!>-    SPICELIB Version 3.3.0, 13-MAY-2010 (BVS)
!>
!>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!>
!>-    SPICELIB Version 3.2.0, 13-MAY-2010 (BVS)
!>
!>        Updated for PC-64BIT-MS_C.
!>
!>-    SPICELIB Version 3.1.0, 13-MAY-2010 (BVS)
!>
!>        Updated for MAC-OSX-64BIT-INTEL_C.
!>
!>-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
!>
!>        Entry points RDKNEW and RDKDAT have been updated so as to be
!>        able to parse text kernel lines containing tab characters.
!>
!>-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
!>
!>        Updated for PC-LINUX-64BIT-GCC_C.
!>
!>-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
!>
!>        Updated for MAC-OSX-INTEL_C.
!>
!>-    SPICELIB Version 2.3.0, 14-NOV-2005 (BVS)
!>
!>        Reinstated HP_C environment.
!>
!>-    SPICELIB Version 2.2.0, 26-OCT-2005 (BVS)
!>
!>        Updated for SUN-SOLARIS-64BIT-GCC_C.
!>
!>-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
!>
!>        File rdker.f made a master file so as to
!>        add the ZZSETNNREAD call. This call will exist
!>        only in FORTRAN source intended for conversion
!>        to C by the f2c utility.
!>
!>        The ZZSETNNREAD call activates and deactivates
!>        the non-native text line read capability for the
!>        CSPICE toolkit.
!>
!>-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
!>
!>        Corrected ENDIF to END IF.
!>
!>-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!>
!>         The entry point RDKLIN was added.
!>
!>-    SPICELIB Version 1.3.0, 22-SEP-1993 (NJB)
!>
!>         Updated for port to NeXT. The "previous kernel" is now closed
!>         only if there actually was a previous kernel.
!>
!>-    SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
!>
!>         RDKER now initializes the variables BEGDAT and BEGTXT
!>         in a portable way. On the first valid entry to this routine,
!>         the backslash character in the form CHAR(92) is concatenated
!>         individually to 'begindata' and 'begintext'.
!>
!>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!>
!>         Comment section for permuted index source lines was added
!>         following the header.
!>
!>-    SPICELIB Version 1.1.0, 07-DEC-1990 (HAN)
!>
!>         The declarations for BEGDAT and BEGTXT were changed from
!>         CHARACTER*10 to CHARACTER*(*).
!>
!>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!>
!>### Revisions
!>
!>-     SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
!>
!>         The entry point RDKLIN was added.
!>
!>-     SPICELIB Version 1.3.0, 22-SEP-1993 (NJB)
!>
!>         Updated for port to NeXT. The "previous kernel" is now closed
!>         only if there actually was a previous kernel.
!>
!>         In the last version of this routine, on the first entry into
!>         the routine, the variable FILE, which records the name of
!>         the last kernel accessed, was passed to CLTEXT.  CLTEXT
!>         executed an INQUIRE statement using this name, which was
!>         not initialized. On the NeXT, this caused the INQUIRE
!>         statement to fail.
!>
!>-     SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
!>
!>         RDKER now initializes the variables BEGDAT and BEGTXT
!>         in a portable way. On the first valid entry to this routine,
!>         the backslash character in the form CHAR(92) is concatenated
!>         individually to 'begindata' and 'begintext'. As a result of
!>         this change, this module is no longer considered environment
!>         specific. All references in the header to the previous method
!>         of initialization were removed.
!>
!>         FILE is now initialized to ' '. Before this modification, if
!>         a call to RDKDAT was performed prior to RDKNEW, RDTEXT
!>         would have printed out garbage (on some machines) in its
!>         error message when notifying the user that it couldn't read
!>         from FILE.
!>
!>-     SPICELIB Version 1.1.0, 7-DEC-1990 (HAN)
!>
!>         The declarations for BEGDAT and BEGTXT were changed from
!>         CHARACTER*10 to CHARACTER*(*). The fixed length of 10 was
!>         not long enough.
!>
!>-     Beta Version 1.1.0, 9-MAR-1989 (HAN)
!>
!>         Moved the declaration of the parameters BEGDAT and
!>         BEGTXT from the code to the $Declarations section.
!>         Filled out the Brief I/O and $Parameters sections.
!>
!>### Implementation notes
!>
!>     Because some environments (such as the SUN) treat the backslash
!>     character as a special character, some gyrations are needed to
!>     put it into a variable in a "portable" way. This is the reason
!>     for the following block of declarations. Admittedly this is
!>     bizarre, but it works.
!>
!>     The ASCII decimal code for the tab character is 9.
!>
!>## ZZTIME (Private, Time --- time parsing utilities)
!>
!> PEPPER: SPICELIB's umbrella routine ZZTIME is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zztime. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines. Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     This is an umbrella routine for a collection of entry points
!>     to the time parsing utility functions.
!>
!>### Detailed Input
!>
!>     See Individual Entry Points.
!>
!>### Detailed Output
!>
!>     See Individual Entry Points.
!>
!>### Parameters
!>
!>     None.
!>
!>### Exceptions
!>
!>     1)  If ZZTIME is called directly the error 'SPICE(BOGUSENTRY)'
!>         is signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     This routine serves as an umbrella for a collection of
!>     related entry points that are used to parse time strings.
!>
!>     Normal usage is to first call ZZTOKNS to create an internal
!>     representation for a time string. This internal representations
!>     maintains a list of identified substrings from the original
!>     input time string. For example the call to ZZTOKNS using
!>     the string
!>
!>       '1996 JAN 25 12:18:19.199'
!>        123456789012345678901234
!>
!>     yields the following internal representation:
!>
!>       'ibmbibi:i:i.i'
!>
!>     where the individual tokens correspond to the substrings
!>     indicated in the following table:
!>
!>       Identifier    Substring          meaning
!>       ----------    -------------      ----------------
!>         i           from 01 to 04      unsigned integer
!>         b           from 05 to 05      blanks or tab
!>         m           from 06 to 08      month
!>         b           from 09 to 09      blanks or tab
!>         i           from 10 to 11      unsigned integer
!>         b           from 12 to 12      blank or tab
!>         i           from 13 to 14      unsigned integer
!>         :           from 15 to 15      colon
!>         i           from 16 to 17      unsigned integer
!>         :           from 18 to 18      colon
!>         i           from 19 to 20      unsigned integer
!>         .           from 21 to 21      decimal point
!>         i           from 22 to 24      unsigned integer
!>
!>     These substrings may be combined and reidentified, removed
!>     or re-identified using the various entry points listed here:
!>
!>     ZZCMBT   combine several tokens into a single token
!>              for example you might scan right to left and replace
!>              the token sequence i.i by n (for number). In this
!>              case the substring boundaries of n would be from 19
!>              to 24.
!>
!>     ZZGREP   returns the current internal representation
!>              in the case above 'ibmbibi:i:i.i'
!>
!>     ZZISPT   returns TRUE if a pair of letters from a list are
!>              present in the internal representation. This is
!>              used primarily to detect erroneous substrings such
!>              as ',,' or ':,'
!>
!>     ZZIST    Return TRUE if a particular letter is present in the
!>              string.
!>
!>     ZZNOTE   Returns the substring boundaries associated with
!>              a letter and removes the letter from the internal
!>              representation. This is used primarily for calendar
!>              string modifiers such as 'B.C.', 'A.D.' etc.
!>
!>     ZZREMT   remove a letter from the internal representation.
!>              In the input example you might remove all white space
!>              markers.
!>
!>     ZZSUBT   substitute a different letter for one listed in the
!>              input one for one. For example after removing blanks
!>              you might substitute YmD for imi.
!>
!>     ZZVALT   replace an integer by a new marker if the integer
!>              lies withing a particular range. For example
!>              you might replace any integer between 1000 and 10000
!>              by Y (for year).
!>
!>     Once all substitutions and removals have been performed that
!>     can be made, the entry point ZZUNPCK allows you to extract
!>     year(Y), month(m), day or month(D), day of year (y), hours(H),
!>     minutes(M) and seconds(S) from the input string
!>
!>### Examples
!>
!>     See TPARTV.
!>
!>### Restrictions
!>
!>     None.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman    (JPL)
!>     B.V. Semenov    (JPL)
!>     W.L. Taber      (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.7.0, 08-NOV-2020 (EDW)
!>
!>        Added 'Z' to list of recognized tokens, marked as
!>        no op.
!>
!>-    SPICELIB Version 1.6.0, 05-FEB-2014 (EDW) (BVS)
!>
!>        BUG FIX: entry point ZZUNPCK: added error check on ITEM value.
!>        Failure to perform this check can cause BADSUBSCRIPT error
!>        signals from CSPICE code on invalid time strings.
!>
!>        BUG FIX: entry point ZZTOKNS: added checks for token indexes
!>        overflowing the maximum number of tokens and for the character
!>        positions in the time picture overflowing the time picture
!>        length. Both overflows previously resulted in segmentation
!>        faults for invalid input time strings that contained too many
!>        recognizable tokens or were too long and required too many
!>        characters in the picture representation.
!>
!>-    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB)
!>
!>        Bug fix: in entry point ZZTOKNS, changed upper
!>        bound used to detect non-printing characters from 128
!>        to 126.
!>
!>        Bug fix: added error handling to this routine. Header
!>        already referred to SPICE(BOGUSENTRY) error, but no
!>        such error was signaled.
!>
!>        Changed upper bound of arrays NAMES, F, and L from 128
!>        to 126.
!>
!>        Re-ordered header sections in various entry points.
!>
!>-    SPICELIB Version 1.4.0, 27-OCT-2006 (BVS)
!>
!>        Fixed the bug in the ZZTOKNS entry that in the case of a one
!>        character long blank input time string caused the TO variable
!>        be set to the value greater than the string length, triggering
!>        an OUT OF BOUNDS runtime error on HP. Added to ZZTOKNS a
!>        separate check for the blank input strings.
!>
!>-    SPICELIB Version 1.3.0, 13-Nov-2000 (WLT)
!>
!>        Changed the call to EQSTR to a call to SAMSBI so as to
!>        guard against overflowing strings.
!>
!>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!>
!>        The main routine (which should never be called) now returns
!>        the value .FALSE.
!>
!>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!>
!>        Added a RETURN statement at the end of the main routine.
!>        Enhanced error message for the case when the input string
!>        to ZZTOKNS has a non-printing character.
!>
!>-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
!>
!>### Implementation notes
!>
!>     Entry points
!>
!>     Standard Parameters
!>
!>     LOWER
!>     UPPER
!>     MIXED
!>
!>     FULL
!>     SHORT
!>
!>     Maximum number of tokens that a valid time string can contain.
!>
!>     Length of the string buffer containing the time string picture.
!>
!>     Representation Variables.
!>
!>     Token Recognition Variables.
!>
!>     At the moment there are 53 recognized substrings, we
!>     make room for 70 just so we won't have to increase
!>     the parameter NRECOG soon.
!>
!>## POOL (Maintain a pool of kernel variables)
!>
!> PEPPER: SPICELIB's umbrella routine POOL is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_pool. Its documentation follows.
!>
!>### Abstract
!>
!>     Maintain a pool of variables read from SPICE ASCII kernel files.
!>
!>### Detailed Input
!>
!>     See the ENTRY points for a discussion of their arguments.
!>
!>### Detailed Output
!>
!>     See the ENTRY points for a discussion of their arguments.
!>
!>### Parameters
!>
!>     MAXVAR   is the maximum number of variables that the
!>              kernel pool may contain at any one time.
!>              MAXVAR should be a prime number.
!>
!>              Here's a list of primes that should make
!>              it easy to upgrade MAXVAR when/if the need arises.
!>
!>                  103
!>                  199
!>                  307
!>                  401
!>                  503
!>                  601
!>                  701
!>                  751
!>                  811
!>                  911
!>                 1013
!>                 1213
!>                 1303
!>                 1511
!>                 1811
!>                 1913
!>                 2003
!>                 2203
!>                 2503
!>                 2803
!>                 3203
!>                 3607
!>                 4001
!>                 4507
!>                 4801
!>                 5003 Current Value
!>                 6007
!>                 6521
!>                 7001
!>                 7507
!>                 8009
!>                 8501
!>                 9001
!>                 9511
!>                10007
!>                10501
!>                11003
!>                11503
!>
!>     MAXLEN   is the maximum length of the variable names that
!>              can be stored in the kernel pool (also set in
!>              zzrvar.f).
!>
!>     MAXVAL   is the maximum number of distinct values that
!>              may belong to the variables in the kernel pool.
!>              Each variable must have at least one value, and
!>              may have any number, so long as the total number
!>              does not exceed MAXVAL. MAXVAL must be at least
!>              as large as MAXVAR.
!>
!>     MAXAGT   is the maximum number of agents that can be
!>              associated with a given kernel variable.
!>
!>     MAXCHR   is the maximum number of characters that can be
!>              stored in a component of a string valued kernel
!>              variable.
!>
!>     MXNOTE   is the maximum sum of the sizes of the sets of
!>              agents in the range of the mapping that associates
!>              with each watched kernel variable a set of agents
!>              that "watch" that variable.
!>
!>     MAXLIN   is the maximum number of character strings that
!>              can be stored as data for kernel pool variables.
!>
!>     CTRSIZ   is the dimension of the counter array used by
!>              various SPICE subsystems to uniquely identify
!>              changes in their states. This parameter is
!>              defined in the private include file 'zzctr.inc'.
!>
!>### Exceptions
!>
!>     1)  If POOL is called directly, the error SPICE(BOGUSENTRY) is
!>         signaled.
!>
!>### Files
!>
!>     See the ENTRY points for a discussion of their arguments.
!>
!>### Particulars
!>
!>     POOL should never be called directly, but should instead be
!>     accessed only through its entry points.
!>
!>     The purpose of this routine is to maintain a pool of variables
!>     read from ASCII kernel files. The following entry points may be
!>     used to access the pool.
!>
!>           CLPOOL         Clears the pool.
!>
!>           LDPOOL         Loads the variables from a kernel file into
!>                          the pool.
!>
!>           RTPOOL         Returns the value of a variable from
!>                          the pool. (Obsolete use GDPOOL)
!>
!>           EXPOOL         Confirms the existence of a numeric
!>                          variable in the pool.
!>
!>           WRPOOL         Writes the contents of the pool to an
!>                          ASCII kernel file.
!>
!>           SWPOOL         Sets up a "watcher" on a variable so that
!>                          various "agents" can be notified when a
!>                          variable has been updated.
!>
!>           CVPOOL         Indicates whether or not an agent's
!>                          variable has been updated since the last
!>                          time an agent checked with the pool.
!>
!>           GCPOOL         Returns the value of a string valued
!>                          variable in the pool.
!>
!>           GDPOOL         Returns the d.p. value of a numeric valued
!>                          variable in the pool.
!>
!>           GIPOOL         Returns the integer value of a numeric valued
!>                          variable in the pool.
!>
!>           DTPOOL         Returns the attributes of a variable in the
!>                          pool.
!>
!>           PCPOOL         Allows the insertion of a character variable
!>                          directly into the kernel pool without
!>                          supplying a text kernel.
!>
!>           PDPOOL         Allows the insertion of a double precision
!>                          variable directly into the kernel pool
!>                          without supplying a text kernel.
!>
!>           PIPOOL         Allows the insertion of an integer variable
!>                          directly into the kernel pool without
!>                          supplying a text kernel.
!>
!>           LMPOOL         Similar to LDPOOL, but the text kernel is
!>                          stored in an array of strings instead of an
!>                          external file.
!>
!>           SZPOOL         allows run time retrieval of kernel pool
!>                          memory parameters.
!>
!>           DVPOOL         allows deletion of a specific variable from
!>                          the kernel pool. (CLPOOL deletes all
!>                          variables from the kernel pool.)
!>
!>           GNPOOL         assists in determining which variables are
!>                          defined in the kernel pool via variable name
!>                          template matching.
!>
!>           DWPOOL         deletes a watch from the watcher system.
!>
!>     Nominally, the kernel pool contains up to MAXVAR separate
!>     variables, up to MAXVAL numeric values, and up to MAXLIN string
!>     values. The names of the individual variables may contain up to
!>     MAXLEN characters.
!>
!>### Examples
!>
!>     The following code fragment demonstrates how the data from
!>     several kernel files can be loaded into a kernel pool. After the
!>     pool is loaded, the values in the pool are written to a kernel
!>     file.
!>
!>     C
!>     C     Store in an array the names of the kernel files whose
!>     C     values will be loaded into the kernel pool.
!>     C
!>           FNAME (1) = 'AXES.KER'
!>           FNAME (2) = 'GM.KER'
!>           FNAME (3) = 'LEAP_SECONDS.KER'
!>
!>     C
!>     C     Clear the kernel pool. (This is optional.)
!>     C
!>           CALL CLPOOL
!>
!>     C
!>     C     Load the variables from the three kernel files into the
!>     C     the kernel pool.
!>     C
!>           DO I = 1, 3
!>             CALL LDPOOL ( FNAME (I) )
!>           END DO
!>
!>     C
!>     C     We can examine the values associated with any d.p. variable
!>     C     in the kernel pool using GDPOOL.
!>     C
!>           CALL GDPOOL ( VARIABLE, START, ROOM, NVALS, VALUES, FOUND )
!>
!>     C
!>     C     Open the text file 'NEWKERNEL.KER'.
!>     C
!>           CALL TXTOPN ( NEWKERNEL.KER', UNIT )
!>
!>     C
!>     C     Write the values in the kernel pool to the file.
!>     C
!>           CALL WRPOOL ( UNIT )
!>
!>### Restrictions
!>
!>     None.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     H.A. Neilan        (JPL)
!>     B.V. Semenov       (JPL)
!>     W.L. Taber         (JPL)
!>     R.E. Thurman       (JPL)
!>     F.S. Turner        (JPL)
!>     I.M. Underwood     (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 10.2.0, 27-AUG-2021 (JDR) (BVS) (NJB)
!>
!>        Changed input argument name KERNEL to FNAME in entry point
!>        LDPOOL for consistency with other routines.
!>
!>        Edited the header of the umbrella routine and all its entry
!>        points.
!>
!>        Updated SZPOOL $Detailed_Input section.
!>
!>-    SPICELIB Version 10.1.0, 14-JUL-2014 (NJB) (BVS)
!>
!>        Updated header of WRPOOL to improve accuracy of
!>        that routine's output description.
!>
!>        Updated header of CVPOOL to improve accuracy of
!>        the description of the output argument UPDATE.
!>
!>        Updated header of CLPOOL to improve the description
!>        of behavior of the watcher subsystem.
!>
!>        Last update was 17-JAN-2014 (BVS) (NJB)
!>
!>        Increased key POOL parameters as follows:
!>
!>           MAXVAR    5003 ->  26003
!>           MAXVAL  200000 -> 400000
!>           MAXLIN    4000 ->  15000
!>
!>        Decreased MXNOTE factor F (MXNOTE=MAXVAR*F) from 10 to 5.
!>
!>        Updated the main umbrella argument list to include the POOL
!>        state counter. Added the private entry point ZZPCTRCK allowing
!>        other routines to check their saved POOL state counter against
!>        the current POOL state counter to detect and act on the POOL
!>        state change.
!>
!>        Updated $Index_Entries sections of entry points PCPOOL, PDPOOL,
!>        and PIPOOL.
!>
!>-    SPICELIB Version 10.0.0, 24-MAY-2010 (EDW) (NJB)
!>
!>        Added an error check on the length of the kernel pool variable
!>        name argument in:
!>
!>           PCPOOL
!>           PDPOOL
!>           PIPOOL
!>
!>        to enforce the variable name length does not exceed MAXLEN.
!>
!>        Increased MAXVAL to 200000.
!>
!>-    SPICELIB Version 9.0.0, 19-MAR-2009 (NJB)
!>
!>        Added watch deletion entry point DWPOOL and private entry
!>        point ZZVUPOOL. Re-implemented watcher system to improve
!>        efficiency, particularly of watch deletion. Bug fix: corrected
!>        watcher overflow detection logic in SWPOOL. Updated header
!>        code examples to use TXTOPN instead of GETLUN and a Fortran
!>        OPEN statement; also to use GDPOOL instead of RTPOOL, except in
!>        the header of RTPOOL itself.
!>
!>        Code examples in SWPOOL and CVPOOL were updated to handle
!>        kernel pool fetch failures.
!>
!>        Existing entry points modified as part of this update were:
!>
!>           POOL
!>           CLPOOL
!>           CVPOOL
!>           DTPOOL
!>           DVPOOL
!>           EXPOOL
!>           GCPOOL
!>           GDPOOL
!>           GIPOOL
!>           GNPOOL
!>           LDPOOL
!>           LMPOOL
!>           PCPOOL
!>           PDPOOL
!>           PIPOOL
!>           RTPOOL
!>           SWPOOL
!>           WRPOOL
!>
!>        Code examples using RTPOOL were updated to use GDPOOL, except
!>        in the header of RTPOOL itself. Code examples using GETLUN and
!>        an in-line Fortran OPEN statement were updated to use TXTOPN.
!>
!>        Various typos in comments throughout this file were fixed.
!>
!>-    SPICELIB Version 8.3.0, 22-DEC-2004 (NJB)
!>
!>        Fixed bug in DVPOOL. Made corrections to comments in
!>        other entry points. The updated routines are DTPOOL,
!>        DVPOOL, EXPOOL, GCPOOL, GDPOOL, GIPOOL, RTPOOL.
!>
!>-    SPICELIB Version 8.2.0, 24-JAN-2003 (BVS)
!>
!>        Increased MAXVAL to 40000.
!>
!>-    SPICELIB Version 8.1.0, 13-MAR-2001 (FST) (NJB)
!>
!>        Increased kernel pool size and agent parameters. MAXVAR is now
!>        5003, MAXVAL is 10000, MAXLIN is 4000, MXNOTE is 2000, and
!>        MAXAGT is 1000.
!>
!>        Modified Fortran output formats used in entry point WRPOOL to
!>        remove list-directed formatting. This change was made to
!>        work around problems with the way f2c translates list-
!>        directed I/O.
!>
!>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!>
!>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!>        direct insertion of data into the kernel pool without having
!>        to read an external file.
!>
!>        Added the interface LMPOOL that allows SPICE
!>        programs to load text kernels directly from memory
!>        instead of requiring a text file.
!>
!>        Added the entry point SZPOOL to return kernel pool definition
!>        parameters.
!>
!>        Added the entry point DVPOOL to allow the removal of a variable
!>        from the kernel pool.
!>
!>        Added the entry point GNPOOL to allow users to determine
!>        variables that are present in the kernel pool
!>
!>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!>
!>        The implementation of the kernel pool was completely redone
!>        to improve performance in loading and fetching data. In
!>        addition the pool was upgraded so that variables may be
!>        either string or numeric valued.
!>
!>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!>        to the routine.
!>
!>        The entry point RTPOOL should now be regarded as obsolete
!>        and is maintained solely for backward compatibility with
!>        existing routines that make use of it.
!>
!>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!>
!>        The entry points SWPOOL and CVPOOL were added.
!>
!>-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!>
!>        Increased value of parameter MAXVAL to 5000 to accommodate
!>        storage of SCLK coefficients in the kernel pool.
!>
!>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!>
!>        All entry points except POOL and CLPOOL now initialize the
!>        pool if it has not been done yet.
!>
!>-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!>
!>        Added declaration of FAILED. FAILED is checked in the
!>        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!>
!>-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!>
!>       A FAILED test was inserted into the control of the DO-loop which
!>       reads in each kernel variable in LDPOOL.
!>
!>-    SPICELIB Version 1.2.0, 09-MAR-1989 (HAN)
!>
!>        Parameters BEGDAT and BEGTXT have been moved into the
!>        $Declarations section.
!>
!>-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!>
!>        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!>        (Actually, MAXLEN was implicitly 32 characters, and has only
!>        now been made an explicit---and changeable---limit.)
!>
!>        Declaration of unused function FAILED removed.
!>
!>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
!>
!>### Revisions
!>
!>-    SPICELIB Version 8.3.0, 22-DEC-2004 (NJB)
!>
!>        Fixed bug in DVPOOL. Made corrections to comments in
!>        other entry points. The updated routines are DTPOOL,
!>        DVPOOL, EXPOOL, GCPOOL, GDPOOL, GIPOOL, RTPOOL.
!>
!>-    SPICELIB Version 8.2.0, 24-JAN-2003 (BVS)
!>
!>        Increased MAXVAL to 40000.
!>
!>-    SPICELIB Version 8.1.0, 13-MAR-2001 (FST) (NJB)
!>
!>        Increased kernel pool size and agent parameters. MAXVAR is now
!>        5003, MAXVAL is 10000, MAXLIN is 4000, MXNOTE is 2000, and
!>        MAXAGT is 1000.
!>
!>        Modified Fortran output formats used in entry point WRPOOL to
!>        remove list-directed formatting. This change was made to
!>        work around problems with the way f2c translates list-
!>        directed I/O.
!>
!>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
!>
!>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
!>        direct insertion of data into the kernel pool without having
!>        to read an external file.
!>
!>        Added the interface LMPOOL that allows SPICE
!>        programs to load text kernels directly from memory
!>        instead of requiring a text file.
!>
!>        Added the entry point SZPOOL to return kernel pool definition
!>        parameters.
!>
!>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
!>
!>        The implementation of the kernel pool was completely redone
!>        to improve performance in loading and fetching data. In
!>        addition the pool was upgraded so that variables may be
!>        either string or numeric valued.
!>
!>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
!>        to the routine.
!>
!>        The entry point RTPOOL should now be regarded as obsolete
!>        and is maintained solely for backward compatibility with
!>        existing routines that make use of it.
!>
!>        The basic data structure used to maintain the list of
!>        variable names and values was replaced with a hash table
!>        implementation. Data and names are accessed by means
!>        of a hash function and linked lists of pointers to existing
!>        variable names and data values.
!>
!>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
!>
!>        The entry points SWPOOL (set watch on a pool variable)
!>        and CVPOOL (check variable for update) so that routines
!>        that buffer data stored in the kernel pool can fetch
!>        that data only when it is updated.
!>
!>        Also the control of initializations was modified to be
!>        consistent with other SPICELIB practices.
!>
!>        Finally, the revision history was upgraded so that the
!>        version number increases over time. This wasn't true
!>        before. In addition some early revision data that referred to
!>        pre-SPICELIB modifications were removed. This editing of
!>        the version numbers makes it unlikely that anyone can track
!>        down which previous version of this routine they have by
!>        looking at the version number. The best way to determine
!>        the routine you had previously is to compare the dates
!>        stored in the Version line of the routine.
!>
!>-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
!>
!>        Increased value of parameter MAXVAL to 5000 to accommodate
!>        storage of SCLK coefficients in the kernel pool.
!>
!>        Also, changed version number in previous revisions entry
!>        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0. The
!>        last version entry in the $Version section had been
!>        Version 1.0.0, dated later than the entry for `version 2'
!>        in the $Revisions section!
!>
!>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
!>
!>        All entry points except POOL and CLPOOL now initialize the
!>        pool if it has not been done yet.
!>
!>-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
!>
!>        Added declaration of FAILED. FAILED is checked in the
!>        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
!>
!>-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
!>
!>       A FAILED test was inserted into the control of the DO-loop which
!>       reads in each kernel variable.
!>
!>       Previously, if the error action 'RETURN' had been set by a
!>       calling program, and the call to RDKNEW by LDPOOL failed,
!>       then execution would continue through LDPOOL, with SPICELIB
!>       routines returning upon entry. This meant that the routine
!>       RDKVAR never got a chance to set the EOF flag, which was the
!>       only control of the DO-loop. An infinite loop resulted in such
!>       cases. The FAILED test resolves that situation.
!>
!>-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
!>
!>        Parameters BEGDAT and BEGTXT have been moved into the
!>        $Declarations section.
!>
!>-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!>
!>        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
!>        (Actually, MAXLEN was implicitly 32 characters, and has only
!>        now been made an explicit---and changeable---limit.)
!>
!>        Declaration of unused function FAILED removed.
!>
!>-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
!>
!>### Implementation notes
!>
!>     Private SPICELIB functions
!>
!>     The next two variables are for use in traversing linked lists.
!>
!>     Because some environments (such as the SUN) are too stupid to
!>     treat the backslash character correctly we have to go through
!>     some gyrations to put it into a variable in a "portable" way.
!>     This is the reason for the following block of declarations.
!>     Admittedly this is bizarre, but it works.
!>
!>     The following is the hash table used for holding kernel pool
!>     variables.  Here's the basic structure:
!>
!>     The function ZZHASH computes the address of the head of a linked
!>     list that contains the collisions for the range of ZZHASH.
!>
!>     The head node of the collision lists is stored in NAMLST.
!>
!>     If NAMLST has a value zero then
!>
!>        there is no name corresponding to that value of the
!>        hash function.
!>
!>     If NAMLST is non-zero then
!>
!>        it is the head node of the list of names that have been
!>        stored so far.
!>
!>        The list of addresses of names is stored in NMPOOL.
!>        The names that have been stored so far are in PNAMES.
!>
!>     The data associated with  PNAMES is pointed to by DATLST
!>     and CHPOOL or DPPOOL.  If a name of interest is stored in
!>     PNAMES(I) then the DATLST(I) points to the first data node
!>     associated with the name.
!>
!>     If DATLST(I) is less than zero then
!>
!>        its opposite is the address of the first node of
!>        character data associated with PNAMES(I).
!>
!>     If DATLST(I) is positive then
!>
!>        it points to the address of the first node of numeric
!>        data associated with PNAMES(I).
!>
!>     If DATLST(I) is zero
!>
!>        there is no data associated with PNAMES(I).
!>
!>     The arrays DPPOOL and CHPOOL are linked list pools that
!>     give the address lists of values associated with a name.
!>
!>     The actual data is stored in DPVALS and CHVALS.
!>
!>     Here's a picture of how this all works.
!>
!>```
!>                                             Linked list Pool
!>                                             of HASH collisions
!>                       NAMLST                  NMPOOL         PNAME
!>                     +------------+          +---------+    +--------+
!>                     |            |          |         |    |        |
!>                     +------------+ if not 0 +---------+    +--------+
!>  ZZHASH( NAME ) --->|  Head Node | ---.     |         |    |        |
!>                     +------------+    |     +---------+    +--------+
!>                                       |     |         |    |        |
!>                                       |     +---------+    +--------+
!>                                       `-->  |Head of  |    |Name    |
!>                                             |collision|    |corresp.|
!>                                             |list for | -. |to head |
!>                                             | NAME    |  | |of list |
!>                                             +---------+  | +--------+
!>                                             |         |  | |        |
!>                                             +---------+  | +--------+
!>                                             |         |  | |        |
!>                                             +---------+  | +--------+
!>                                             |Next Node|<-' |NextName|
!>                                             +---------+etc.+--------+
!>                                                  .              .
!>                                                  .              .
!>                                                  .              .
!>                                             +---------+    +--------+
!>                                             |         |    |        |
!>                                             +---------+    +--------+
!>```
!>
!>      Linked       Variable    Heads of
!>      List Pool     Names      Data lists
!>       NMPOOL       PNAME       DATLST
!>     +--------+   +--------+   +---------+          Head of linked list
!>     |        |   |        |   |         |     .--> in DPPOOL linked
!>     +--------+   +--------+   +---------+    |     list pool
!>     |        |   |        |   |         |    |
!>     +--------+   +--------+   +---------+    | Positive Value
!>     |        |<->|        |<->|         |---<
!>     +--------+   +--------+   +---------+    |
!>     |        |   |        |   |         |    | Negative Value
!>     +--------+   +--------+   +---------+    |
!>     |        |   |        |   |         |    `--> Opposite of head
!>     +--------+   +--------+   +---------+          of linked list
!>     |        |   |        |   |         |          in CHPOOL linked
!>     +--------+   +--------+   +---------+          list pool.
!>
!>      Linked                Values
!>      List Pool             of data
!>       DPPOOL (CHPOOL)      DPVALS (CHVALS)
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     | HEAD       |--. <--> | head value |
!>     +------------+  |      +------------+
!>     |            |  |      |            |
!>     +------------+  |      +------------+
!>     |            |  |      |            |
!>     +------------+  |      +------------+
!>     | Node 2     |<-' <--> | 2nd value  |
!>     +------------+ etc.    +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>     |            |         |            |
!>     +------------+         +------------+
!>
!>     The WT... variables make up the data structure that
!>     maps variables to their associated agents (WTAGNT).
!>     A diagram of the watcher data structure is shown below.
!>
!>      Watched     Heads of     Agent linked  Agent names
!>      variables   agent lists  list pool
!>       WTVARS       WTPTR        WTPOOL        WTAGNT
!>     +--------+   +--------+   +---------+   +---------+
!>     |        |   |        |   |         |   |         |
!>     +--------+   +--------+   +---------+   +---------+
!>     |        |   |        |   |         |   |         |
!>     +--------+   +--------+   +---------+   +---------+
!>     |        |<->|        |<->|         |<->|         |
!>     +--------+   +--------+   +---------+   +---------+
!>     |        |   |        |   |         |   |         |
!>     +--------+   +--------+   +---------+   +---------+
!>     |        |   |        |   |         |   |         |
!>     +--------+   +--------+   +---------+   +---------+
!>     |        |   |        |   |         |   |         |
!>     +--------+   +--------+   +---------+   +---------+
!>
!>     Agents contains the list of agents that need to be notified
!>     about updates to their variables.  NOTIFY and ACTIVE are both
!>     temporary sets.
!>
!>     These variables are declared with the size MXNOTE because
!>     they must be able to hold the largest possible number
!>     of agents that could be associated with a kernel variable.
!>
!>     First is our initialization flag.
!>
!>     POOL state counter.
!>
!>     The remaining local variables...
!>
!>     Set up the definition of our in-line functions.
submodule (pepper_instance_module) pepper_pool

   implicit none

   !> The time-string patterns [[tpartv]] recognizes ([[zztpats]]) and their
   !> meanings are stored in the order of SPICELIB's sort ([[orderc]]) of
   !> [[zztpats]]'s table, so that [[bsrchc]] finds the same entry as in SPICELIB
   !> (the table has duplicate patterns).
   !>
   !> PEPPER: SPICELIB sorted the table on the first call of [[tpartv]] and
   !> saved it; without the saved copy, [[tpartv]] built and sorted it on
   !> every call (90 percent of [[str2et]]'s time). test_time checks that
   !> [[orderc]] leaves this order unchanged.
   integer, parameter :: ntpats = 231  !! number of patterns
   !> the patterns, sorted
   character(32), parameter :: tpats_known ( 231 ) = [ character(32) :: &
      'Y-i-it', 'Y-i-iti', 'Y-i-iti:i', 'Y-i-iti:i:i', &
      'Y-i-iti:i:ix', 'Y-i-iti:i:n', 'Y-i-iti:i:nx', 'Y-i-iti:ix', &
      'Y-i-iti:n', 'Y-i-iti:nx', 'Y-i-itix', 'Y-i-itn', &
      'Y-i-itnx', 'Y-i-itx', 'Y-i/', 'Y-i/i:i', &
      'Y-i/i:i:i', 'Y-i/i:i:n', 'Y-i/i:n', 'Y-id', &
      'Y-idi:i', 'Y-idi:i:i', 'Y-idi:i:n', 'Y-idi:n', &
      'Y-it', 'Y-iti', 'Y-iti:i', 'Y-iti:i:i', &
      'Y-iti:i:ix', 'Y-iti:i:n', 'Y-iti:i:nx', 'Y-iti:ix', &
      'Y-iti:n', 'Y-iti:nx', 'Y-itix', 'Y-itn', &
      'Y-itnx', 'Y-itx', 'Yid', 'Yidi:i', &
      'Yidi:i:i', 'Yidi:i:n', 'Yidi:n', 'Yii', &
      'Yiii', 'Yiii:i', 'Yiii:i:i', 'Yiii:i:n', &
      'Yiii:n', 'Yiiii', 'Yiiiii', 'Yiiiin', &
      'Yiiin', 'Yiin', 'Yim', 'Yimi', &
      'Yimi:i', 'Yimi:i:i', 'Yimi:i:n', 'Yimi:n', &
      'Yimn', 'Yin', 'Ymi', 'Ymii', &
      'Ymii:i', 'Ymii:i:i', 'Ymii:i:n', 'Ymii:n', &
      'Ymin', 'Ymn', 'Ynm', 'i-Y/', &
      'i-Y/i:i', 'i-Y/i:i:i', 'i-Y/i:i:n', 'i-Y/i:n', &
      'i-Yd', 'i-Ydi:i', 'i-Ydi:i:i', 'i-Ydi:i:n', &
      'i-Ydi:n', 'i-i-Y', 'i-i-Yi:i', 'i-i-Yi:i:i', &
      'i-i-Yi:i:n', 'i-i-Yi:n', 'i-i-it', 'i-i-iti', &
      'i-i-iti:i', 'i-i-iti:i:i', 'i-i-iti:i:ix', 'i-i-iti:i:n', &
      'i-i-iti:i:nx', 'i-i-iti:ix', 'i-i-iti:n', 'i-i-iti:nx', &
      'i-i-itix', 'i-i-itn', 'i-i-itnx', 'i-i-itx', &
      'i-i/i:i', 'i-i/i:i:i', 'i-i/i:i:n', 'i-i/i:n', &
      'i-idi:i', 'i-idi:i:i', 'i-idi:i:n', 'i-idi:n', &
      'i-it', 'i-iti', 'i-iti:i', 'i-iti:i:i', &
      'i-iti:i:ix', 'i-iti:i:n', 'i-iti:i:nx', 'i-iti:ix', &
      'i-iti:n', 'i-iti:nx', 'i-itix', 'i-itn', &
      'i-itnx', 'i-itx', 'i/i/Y', 'i/i/Y/i:n', &
      'i/i/Yi:i', 'i/i/Yi:i:i', 'i/i/Yi:i:n', 'i/i/i', &
      'i/i/ii:i', 'i/i/ii:i:i', 'i/i/ii:i:n', 'i/i/ii:n', &
      'i/i/ii:n', 'i:i:ii-i-Y', 'i:i:ii/i/Y', 'i:i:ii/i/i', &
      'i:i:iimY', 'i:i:imiY', 'i:i:ni-i-Y', 'i:i:ni/i/Y', &
      'i:i:ni/i/i', 'i:i:nimY', 'i:i:nmiY', 'i:ii-i-Y', &
      'i:ii/i/Y', 'i:ii/i/i', 'i:iimY', 'i:imiY', &
      'i:ni-i-Y', 'i:ni/i/Y', 'i:ni/i/i', 'i:nimY', &
      'i:nmiY', 'iYd', 'iYdi:i', 'iYdi:i:i', &
      'iYdi:i:n', 'iYdi:n', 'iiY', 'iiYi', &
      'iiYi:i', 'iiYi:i:i', 'iiYi:i:n', 'iiYi:n', &
      'iiYn', 'iid', 'iidi:i', 'iidi:i:i', &
      'iidi:i:n', 'iidi:n', 'iim', 'iimi', &
      'iimi:i', 'iimi:i:i', 'iimi:i:n', 'iimi:n', &
      'iimii', 'iimiii', 'iimiin', 'iimin', &
      'iimn', 'imY', 'imYi', 'imYi:i', &
      'imYi:i:i', 'imYi:i:n', 'imYi:n', 'imYn', &
      'imi', 'imi:i:iY', 'imi:i:nY', 'imi:iY', &
      'imi:nY', 'imii', 'imii:i', 'imii:i:i', &
      'imii:i:n', 'imii:n', 'imiii', 'imiiii', &
      'imiiin', 'imiin', 'imin', 'imn', &
      'inY', 'inm', 'miY', 'miYi', &
      'miYi:i', 'miYi:i:i', 'miYi:i:n', 'miYi:n', &
      'miYn', 'mii', 'mii:i:iY', 'mii:i:nY', &
      'mii:iY', 'mii:nY', 'miii', 'miii:i', &
      'miii:i:i', 'miii:i:n', 'miii:n', 'miiii', &
      'miiiii', 'miiiin', 'miiin', 'miin', &
      'mnY', 'mni', 'nmY' ]
   !> the meaning of each pattern
   character(32), parameter :: tpats_meaning ( 231 ) = [ character(32) :: &
      'Y*m*D*', 'Y*m*D*H', 'Y*m*D*H*M', 'Y*m*D*H*M*S', &
      'Y*m*D*H*M*S*', 'Y*m*D*H*M*S', 'Y*m*D*H*M*S*', 'Y*m*D*H*M*', &
      'Y*m*D*H*M', 'Y*m*D*H*M*', 'Y*m*D*H*', 'Y*m*D*H', &
      'Y*m*D*H*', 'Y*m*D**', 'Y*y*', 'Y*y*H*M', &
      'Y*y*H*M*S', 'Y*y*H*M*S', 'Y*y*H*M', 'Y*y*', &
      'Y*y*H*M', 'Y*y*H*M*S', 'Y*y*H*M*S', 'Y*y*H*M', &
      'Y*y*', 'Y*y*H', 'Y*y*H*M', 'Y*y*H*M*S', &
      'Y*y*H*M*S*', 'Y*y*H*M*S', 'Y*y*H*M*S*', 'Y*y*H*M*', &
      'Y*y*H*M', 'Y*y*H*M*', 'Y*y*H*', 'Y*y*H', &
      'Y*y*H*', 'Y*y**', 'Yy*', 'Yy*H*M', &
      'Yy*H*M*S', 'Yy*H*M*S', 'Yy*H*M', 'YmD', &
      'YmDH', 'YmDH*M', 'YmDH*M*S', 'YmDH*M*S', &
      'YmDH*M', 'YmDHM', 'YmDHMS', 'YmDHMS', &
      'YmDHM', 'YmDH', 'YDm', 'YDmH', &
      'YDmH*M', 'YDmH*M*S', 'YDmH*M*S', 'YDmH*M', &
      'YDmH', 'YmD', 'YmD', 'YmDH', &
      'YmDH*M', 'YmDH*M*S', 'YmDH*M*S', 'YmDH*M', &
      'YmDH', 'YmD', 'YDm', 'y*Y*', &
      'y*Y*H*M', 'y*Y*H*M*S', 'y*Y*H*M*S', 'y*Y*H*M', &
      'y*Y*', 'y*Y*H*M', 'y*Y*H*M*S', 'y*Y*H*M*S', &
      'y*Y*H*M', 'm*D*Y', 'm*D*YH*M', 'm*D*YH*M*S', &
      'm*D*YH*M*S', 'm*D*YH*M', 'Y*m*D*', 'Y*m*D*H', &
      'Y*m*D*H*M', 'Y*m*D*H*M*S', 'Y*m*D*H*M*S*', 'Y*m*D*H*M*S', &
      'Y*m*D*H*M*S*', 'Y*m*D*H*M*', 'Y*m*D*H*M', 'Y*m*D*H*M*', &
      'Y*m*D*H*', 'Y*m*D*H', 'Y*m*D*H*', 'Y*m*D**', &
      'Y*y*H*M', 'Y*y*H*M*S', 'Y*y*H*M*S', 'Y*y*H*M', &
      'Y*y*H*M', 'Y*y*H*M*S', 'Y*y*H*M*S', 'Y*y*H*M', &
      'Y*y*', 'Y*y*H', 'Y*y*H*M', 'Y*y*H*M*S', &
      'Y*y*H*M*S*', 'Y*y*H*M*S', 'Y*y*H*M*S*', 'Y*y*H*M*', &
      'Y*y*H*M', 'Y*y*H*M*', 'Y*y*H*', 'Y*y*H', &
      'Y*y*H*', 'Y*y**', 'm*D*Y', 'm*D*Y*H*M', &
      'm*D*YH*M', 'm*D*YH*M*S', 'm*D*YH*M*S', 'm*D*Y', &
      'm*D*YH*M', 'm*D*YH*M*S', 'm*D*YH*M*S', 'm*D*YH*M', &
      'm*D*YH*M', 'H*M*Sm*D*Y', 'H*M*Sm*D*Y', 'H*M*Sm*D*Y', &
      'H*M*SDmY', 'H*M*SmDY', 'H*M*Sm*D*Y', 'H*M*Sm*D*Y', &
      'H*M*Sm*D*Y', 'H*M*SDmY', 'H*M*SmDY', 'H*Mm*D*Y', &
      'H*Mm*D*Y', 'H*Mm*D*Y', 'H*MDmY', 'H*MmDY', &
      'H*Mm*D*Y', 'H*Mm*D*Y', 'H*Mm*D*Y', 'H*MDmY', &
      'H*MmDY', 'yY*', 'yY*H*M', 'yY*H*M*S', &
      'yY*H*M*S', 'yY*H*M', 'mDY', 'mDYH', &
      'mDYH*M', 'mDYH*M*S', 'mDYH*M*S', 'mDYH*M', &
      'mDYH', 'Yy*', 'Yy*H*M', 'Yy*H*M*S', &
      'Yy*H*M*S', 'Yy*H*M', 'YDm', 'YDmH', &
      'YDmH*M', 'YDmH*M*S', 'YDmH*M*S', 'YDmH*M', &
      'YDmHM', 'YDmHMS', 'YDmHMS', 'YDmHM', &
      'YDmH', 'DmY', 'DmYH', 'DmYH*M', &
      'DmYH*M*S', 'DmYH*M*S', 'DmYH*M', 'DmYH', &
      'YmD', 'DmH*M*SY', 'DmH*M*SY', 'DmH*MY', &
      'DmH*MY', 'YmDH', 'YmDH*M', 'YmDH*M*S', &
      'YmDH*M*S', 'YmDH*M', 'YmDHM', 'YmDHMS', &
      'YmDHMS', 'YmDHM', 'YmDH', 'YmD', &
      'mDY', 'YDm', 'mDY', 'mDYH', &
      'mDYH*M', 'mDYH*M*S', 'mDYH*M*S', 'mDYH*M', &
      'mDYH', 'mDY', 'mDH*M*SY', 'mDH*M*SY', &
      'mDH*MY', 'mDH*MY', 'mDYH', 'mDYH*M', &
      'mDYH*M*S', 'mDYH*M*S', 'mDYH*M', 'mDYHM', &
      'mDYHMS', 'mDYHMS', 'mDYHM', 'mDYH', &
      'mDY', 'mDY', 'DmY' ]


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   !
   ! Below are a collection of enumerated lists that are used
   ! to discern what part of the processing we are in and what
   ! kind of entity we are dealing with.  First the overall
   ! processing flow of a variable assignment.
   !
   ! Next we have the various types of tokens that can be found
   ! in the parsing of an input line
   !
   ! Q   --- quoted (or protected tokens)
   ! NQ  --- unquoted tokens
   ! BV  --- beginning of a vector
   ! EV  --- ending of a vector
   ! EQ  --- equal sign
   ! EQP --- equal sign plus
   !
   ! A variable can have one of three types as we process
   ! it.  It can have an unknown type UNKNWN, STRTYP or NUMTYP.
   integer(ip), parameter :: begin = 1  !! reading state: start
   integer(ip), parameter :: bslash = 92  !! character code of the backslash
   integer(ip), parameter :: q = 1  !! token class: quoted string
   integer(ip), parameter :: nq = q      + 1  !! token class: not quoted
   integer(ip), parameter :: bv = nq     + 1  !! token class: beginning of values ("(")
   integer(ip), parameter :: done = begin  + 1  !! reading state: done
   integer(ip), parameter :: intext = 1  !! text kernel section: text (comments)
   integer(ip), parameter :: indata = intext + 1  !! text kernel section: data
   integer(ip), parameter :: endoff = indata + 1  !! text kernel section: past the end
   integer(ip), parameter :: ev = bv     + 1  !! token class: end of values (")")
   integer(ip), parameter :: eq = ev     + 1  !! token class: "="
   integer(ip), parameter :: eqp = eq     + 1  !! token class: "+="
   integer(ip), parameter :: era = 1  !! time string component: era
   integer(ip), parameter :: iblank = ichar( ' '   )  !! character code of a blank
   integer(ip), parameter :: icomma = ichar( ','   )  !! character code of a comma
   integer(ip), parameter :: iequal = ichar( '='   )  !! character code of "="
   integer(ip), parameter :: ilparn = ichar( '('   )  !! character code of "("
   integer(ip), parameter :: invar = done   + 1  !! reading state: in a variable's values
   integer(ip), parameter :: iplus = ichar( '+'   )  !! character code of "+"
   integer(ip), parameter :: iquote = ichar( ''''  )  !! character code of a quote
   integer(ip), parameter :: irparn = ichar( ')'   )  !! character code of ")"
   integer(ip), parameter :: itab = 9  !! character code of a tab
   integer(ip), parameter :: itmark = ichar( '@'   )  !! character code of "@" (an epoch)
   integer(ip), parameter :: linlen = 132  !! length of a text kernel line
   integer(ip), parameter :: lnsize = 80  !! length of a line of text (messages, kernel lines)
   integer(ip), parameter :: maxtkn = 64  !! maximum number of tokens of a time string
   logical, parameter :: no = .false.  !! false
   integer(ip), parameter :: strtyp = 1  !! kernel variable type: character
   integer(ip), parameter :: numtyp = strtyp + 1  !! kernel variable type: numeric
   integer(ip), parameter :: piclen = maxtkn * 5  !! length of a time picture
   integer(ip), parameter :: tabcde = 9  !! character code of a tab
   integer(ip), parameter :: unknwn = numtyp + 1  !! kernel variable type: not yet known
   integer(ip), parameter :: wday = era    + 1  !! time string component: weekday
   logical, parameter :: yes = .true.  !! true
   integer(ip), parameter :: zone = wday   + 1  !! time string component: time zone

contains

      !> Set counter array to the initial values for the user routines.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     OLDCTR      is the counter array set to the initial values for
      !>                 the user routines.
      !>
      !>## Parameters
      !>
      !>     CTRSIZ      is the dimension of the counter array used by
      !>                 various SPICE subsystems to identify
      !>                 changes in their states.
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
      !>     This type of initialization must be done by all user routines
      !>     to ensure update on the first check.
      !>
      !>## Examples
      !>
      !>     See umbrella Examples.
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
      !>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
      module subroutine zzctruin ( ctx, oldctr )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: oldctr ( ctrsiz )  !! Counter array to be set to initial user
                                                     !! values.
      integer(ip), parameter :: ctrhgh = intmax
      if ( ctx%failed() ) return

!
!     Initialize the high and low values.
!

!
!     Set counter.
!
      oldctr(1) = ctrhgh
      oldctr(2) = ctrhgh

      return
      end subroutine zzctruin

      !> Set counter array to the initial values for the subsystem
      !> routines.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     OLDCTR      is the counter array set to the initial values for
      !>                 the subsystem routines.
      !>
      !>## Parameters
      !>
      !>     CTRSIZ      is the dimension of the counter array used by
      !>                 various SPICE subsystems to identify
      !>                 changes in their states.
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
      !>     This type of initialization must be done by all subsystem
      !>     routines to set the counters to the lowest initial values.
      !>
      !>## Examples
      !>
      !>     See umbrella Examples.
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
      !>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
      module subroutine zzctrsin ( ctx, oldctr )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: oldctr ( ctrsiz )  !! Counter array to be set to initial
                                                     !! subsystem values
      integer(ip), parameter :: ctrlow = intmin
      if ( ctx%failed() ) return

!
!     Initialize the high and low values.
!

!
!     Set counter.
!
      oldctr(1) = ctrlow
      oldctr(2) = ctrlow

      return
      end subroutine zzctrsin

      !> Increment counter array.
      !>
      !>## Detailed Input
      !>
      !>     OLDCTR      is the counter array to be incremented.
      !>
      !>## Detailed Output
      !>
      !>     OLDCTR      is the counter array that was incremented.
      !>
      !>## Parameters
      !>
      !>     CTRSIZ      is the dimension of the counter array used by
      !>                 various SPICE subsystems to identify
      !>                 changes in their states.
      !>
      !>## Exceptions
      !>
      !>     1) If all elements of the counter array on the input are equal
      !>        to INTMAX, the error '(SPICEISTIRED)' is signaled.
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
      !>     See umbrella Examples.
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
      !>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
      module subroutine zzctrinc ( ctx, oldctr )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: oldctr ( ctrsiz )  !! Counter array to be incremented.
      integer(ip), parameter :: ctrhgh = intmax
      integer(ip), parameter :: ctrlow = intmin
      if ( ctx%failed() ) return

!
!     Initialize the high and low values.
!

!
!     Signal an error if both input counter array elements have high
!     values.
!
      if ( oldctr(1) == ctrhgh .and. oldctr(2) == ctrhgh ) then

         call ctx%setmsg ( 'A subsystem state counter overflowed. '    // &
                       'For this to happen there must be '         // &
                       'a SPICE bug or you must have been '        // &
                       'running your SPICE-based application for ' // &
                       'a very long time. Please contact NAIF.'    // &
                       'and report the circumstances under '       // &
                       'which this happened.'                      )

         call ctx%sigerr ( 'SPICE(SPICEISTIRED)' )

         return

      end if

!
!     Increment counters.
!
      if ( oldctr(1) == ctrhgh ) then

         oldctr(1) = ctrlow
         oldctr(2) = oldctr(2) + 1

      else

         oldctr(1) = oldctr(1) + 1

      end if

      return
      end subroutine zzctrinc

      !> Check and update, if needed, counter array.
      !>
      !>## Detailed Input
      !>
      !>     NEWCTR      is the new counter value against which the old
      !>                 value should be checked.
      !>
      !>     OLDCTR      is the old counter value.
      !>
      !>## Detailed Output
      !>
      !>     OLDCTR      is the updated the old counter value, set equal to
      !>                 the new counter value if counters were different or
      !>                 kept the same if counters were the same.
      !>
      !>     UPDATE      is the logical flag indicating whether the old
      !>                 counter was updated.
      !>
      !>## Parameters
      !>
      !>     CTRSIZ      is the dimension of the counter array used by
      !>                 various SPICE subsystems to identify
      !>                 changes in their states.
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
      !>     See umbrella Examples.
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
      !>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
      module subroutine zzctrchk ( ctx, newctr, oldctr, update )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: newctr ( ctrsiz )  !! New counter value.
      integer(ip), intent(inout) :: oldctr ( ctrsiz )  !! Old counter value.
      logical, intent(out) :: update  !! Update flag.
      if ( ctx%failed() ) return

!
!     Do counters differ?
!
      update = newctr(1) /= oldctr(1) .or. newctr(2) /= oldctr(2)

!
!     If they do, set old counter to new value.
!
      if ( update ) then

         oldctr(1) = newctr(1)
         oldctr(2) = newctr(2)

      end if

      return
      end subroutine zzctrchk

      !> Set the long error message equal to a standard I/O error message
      !> composed from an action, the name of a file, and a value of
      !> IOSTAT.
      !>
      !>## Detailed Input
      !>
      !>     ACTION   is the action which caused the error. This may
      !>              be the name of a basic operation, such as 'OPEN',
      !>              'READ', or 'WRITE', or may be more sophisticated,
      !>              for example, 'add an empty cluster header to'.
      !>
      !>     FILE     is the name of the file involved in the error.
      !>              This may be the system or logical name of a file
      !>              ('USER$DISK:[USER.SUB]TEMP.DAT', 'PLNEPH'), or one
      !>              of the standard files ('SYS$INPUT', 'SYS$OUTPUT').
      !>
      !>     IOSTAT   is the value of IOSTAT returned by ACTION. This
      !>              is appended to the end of the error message.
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
      !>     None.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The input arguments are inserted into the standard form shown
      !>     below. Spaces are inserted where needed. Leading and trailing
      !>     spaces are removed.
      !>
      !>     The long error message is set equal to a standard I/O error
      !>     message, of the form:
      !>
      !>                 An error occurred while --------1----------
      !>                 -------2-------.  The value of IOSTAT returned
      !>                 was --3--.
      !>
      !>               where the values of ACTION, FILE, and IOSTAT are
      !>               assigned to positions 1, 2 and 3
      !>               respectively.
      !>
      !>     If the length of the entire composed message exceeds 320
      !>     characters, it is truncated.
      !>
      !>     SIGERR must be called following a call to this routine to
      !>     actually output the resulting long error message to the error
      !>     output device.
      !>
      !>## Examples
      !>
      !>     The following example illustrates the use of IOERR.
      !>
      !>           CALL IOERR ( 'adding a new header to',
      !>                        EPHEM,
      !>                        24                      )
      !>
      !>     The resulting error message would be:
      !>
      !>           'An error occurred while adding a new header
      !>            to LIBDISK:[EPHEM.NESYS]VGR2_T860502.GEF.  The value
      !>            of IOSTAT returned was 24.'
      !>
      !>     Note that the user is not responsible for adding and eliminating
      !>     spaces to make the string readable. That is all done
      !>     automatically.
      !>
      !>     It is possible to omit the name of the file entirely, as in the
      !>     following (somewhat frivolous) example.
      !>
      !>           CALL IOERR ( 'cleaning a fish',
      !>                        ' ',
      !>                        -3                                )
      !>
      !>     The resulting error message would be:
      !>
      !>           'An error occurred while cleaning a fish.
      !>            The value of IOSTAT returned was -3.'
      !>
      !>     In fact, if the value of IOSTAT is zero, the last part of the
      !>     message is omitted entirely, as in the following example.
      !>
      !>           CALL IOERR ( 'writing the status line to',
      !>                        'SYS$OUTPUT',
      !>                        0                                 )
      !>
      !>     The resulting error message would be:
      !>
      !>           'An error occurred while writing the status
      !>            line to SYS$OUTPUT.'
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
      !>-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 20-DEC-1988 (NJB)
      !>
      !>        IOERR now sets the long error message equal to the
      !>        constructed message, rather than returning the constructed
      !>        message to the caller.  IOERR's argument list has been
      !>        changed accordingly, and a call to SETMSG has been added.
      !>        Also, the name of the calling routine no longer appears
      !>        in the constructed message.
      module subroutine ioerr ( ctx, action, file, iostat )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: action  !! Action which caused the error.
      character(*), intent(in) :: file  !! The name of the file involved.
      integer(ip), intent(in) :: iostat  !! The value of IOSTAT returned by ACTION.

      character(10) :: iochar

      integer(ip), parameter :: longln = 320
      character(longln) :: error

!
!     First comes some standard stuff.
!
      error =  'An error occurred while'

!
!     Next comes the action that caused the error, and the file name.
!     There should be at least one space between each of these pieces,
!     but not more than one.
!
      call suffix ( action, 1, error )
      call suffix ( file,   1, error )
      call suffix ( '.',    0, error )

!
!     More standard stuff. If IOSTAT is zero, there is no need for this
!     part of the message.
!
      if ( iostat /= 0 ) then

         call suffix ( 'The value of IOSTAT returned was', 2, error )

!
!        IOSTAT must be written to a character variable first.
!        Attempting to write it directly to ERROR could cause a
!        boo-boo if we have already overrun the length of ERROR.
!
         call intstr ( iostat, iochar )

         call suffix ( iochar, 1, error )
         call suffix ( '.',    0, error )

      end if

!
!     The message has been constructed.  Set the long error message
!     equal to the constructed message.
!

      call ctx%setmsg ( error )

      return
      end subroutine ioerr

      !> Read the next line of text from a text file.
      !>
      !>## Detailed Input
      !>
      !>     FILE     is the name of the text file from which the next
      !>              line is to be read. If the file is not currently
      !>              open, it is opened with a logical unit determined
      !>              at run time, and the first line of the file is
      !>              returned. Otherwise, the next line not yet read
      !>              from the file is read and returned.
      !>
      !>## Detailed Output
      !>
      !>     LINE     is next line of text in the specified file.
      !>              If the end of the file is reached, LINE is blank.
      !>
      !>     EOF      is .TRUE. when the end of the file is reached, and is
      !>              otherwise .FALSE.
      !>
      !>## Parameters
      !>
      !>     MAXOPN   is the maximum number of files that can be kept
      !>              open simultaneously by RDTEXT.
      !>
      !>              VAX:
      !>
      !>                 The default number of files that can be open at one
      !>                 time during a user's process is determined by the
      !>                 value of FILLM. This number is usually 20, but it
      !>                 may be changed by a user with sufficient privileges.
      !>
      !>              IBM PC / Microsoft FORTRAN 5.0:
      !>
      !>                 The default value for the maximum number of files
      !>                 open at one time is 20. This value may be changed
      !>                 by modifying the appropriate startup files as
      !>                 specified in the reference documentation.
      !>
      !>              IBM PC / Linux / Fort77:
      !>
      !>                 An experiment showed that a program can
      !>                 simultaneously open one file for each available
      !>                 logical unit; this amounts to 96 files.
      !>
      !>              Sun / Sun FORTRAN:
      !>
      !>                 "The maximum number of logical units that a program
      !>                 can have open at one time is the same as the SunOS
      !>                 system limit, currently 64."
      !>
      !>              HP-UX 9000/750, FORTRAN/9000 Series 700 computers and
      !>              Silicon Graphics:
      !>
      !>                 NAIF used a program to determine this value. Also,
      !>                 the values can be found by executing the command
      !>                 "man limits" and reading the value for OPEN_MAX.
      !>                 This value is listed as 60, but two units are used
      !>                 for standard output and standard error.
      !>
      !>              DEC Alpha-OSF/1:
      !>
      !>                 The comment in the output from the command
      !>                 "man limits" stated that the value of OPEN_MAX was
      !>                 64, but that it was "OBSOLETE, sysconf() interface
      !>                 should be used". Looking into sysconf did not produce
      !>                 any numbers, so the value is set at 20 because it
      !>                 works!
      !>
      !>              NeXT/Absoft Fortran:
      !>
      !>                 We couldn't find any documentation that addressed
      !>                 this value, so we set it to 20.
      !>
      !>     MAXLEN   is the maximum length of the file names that may
      !>              used to identify the files opened by RDTEXT.
      !>
      !>## Exceptions
      !>
      !>     1)  If too many files are open already, the error
      !>         SPICE(TOOMANYFILESOPEN) is signaled.
      !>
      !>     2)  If the attempt to open the file fails, the error
      !>         SPICE(FILEOPENFAILED) is signaled.
      !>
      !>     3)  If the attempt to read from the file fails, the error
      !>         SPICE(FILEREADFAILED) is signaled.
      !>
      !>     4)  If the attempt to "inquire" the status of the file fails,
      !>         the error SPICE(INQUIREFAILED) is signaled.
      !>
      !>## Files
      !>
      !>     See input FILE.
      !>
      !>## Particulars
      !>
      !>     RDTEXT reads the next line from a text file. If the file is
      !>     not currently open, it is opened with a logical unit determined
      !>     at run time, and the first line of the file is returned.
      !>     Otherwise, the next line not yet read from the file is returned.
      !>
      !>     If the end of the file is reached, a blank line is returned,
      !>     the end-of-file indicator is .TRUE., and the file is closed.
      !>
      !>     Several files may be opened and read simultaneously. Thus,
      !>     you may begin reading from one file before the end of another
      !>     file has been reached. RDTEXT maintains a separate file pointer
      !>     for each file.
      !>
      !>## Examples
      !>
      !>     Let FILE.1 contain the following lines.
      !>
      !>        Mary had a little lamb
      !>        Everywhere that Mary went
      !>
      !>     Let FILE.2 contain the following lines.
      !>
      !>        Its fleece was white as snow.
      !>        The lamb was sure to go.
      !>
      !>     Then the code fragment
      !>
      !>        DO I = 1, 2
      !>           CALL RDTEXT ( 'FILE.1', LINE, EOF )
      !>           WRITE (6,*) LINE
      !>
      !>           CALL RDTEXT ( 'FILE.2', LINE, EOF )
      !>           WRITE (6,*) LINE
      !>        END DO
      !>
      !>     produces the following output
      !>
      !>        Mary had a little lamb
      !>        Its fleece was white as snow.
      !>        Everywhere that Mary went
      !>        The lamb was sure to go.
      !>
      !>## Restrictions
      !>
      !>     1)  The values of MAXOPN and MAXLEN should not exceed any
      !>         corresponding limits imposed by the operating system.
      !>
      !>     2)  If the input file is a print file, the carriage control
      !>         character at the beginning of a given line will be considered
      !>         part of the line. (Text files have no carriage control
      !>         characters.)
      !>
      !>     PEPPER: the file is read as a stream of bytes and split into
      !>     lines here: a line ends at LF, CR-LF or a lone CR, whatever the
      !>     platform or the compiler (gfortran's run-time library takes a
      !>     lone CR as a line end, Intel's does not), and its terminator is
      !>     not part of it. A line longer than LINE is truncated, as by a
      !>     formatted read.
      !>
      !>     3)  In order to avoid access violations, the VAX/VMS version of
      !>         RDTEXT uses the VAX READONLY qualifier to open files. This
      !>         must be removed or replaced when the routine is ported to
      !>         non-VAX/VMS systems.
      !>
      !>     4)  On VAX systems, caution should be exercised when using
      !>         multiple logical names to point to the same file. Logical
      !>         name translation supporting execution of the Fortran
      !>         INQUIRE statement does not appear to work reliably in all
      !>         cases, which may lead this routine to believe that different
      !>         logical names indicate different files. The specific problem
      !>         that has been observed is that logical names that include
      !>         disk specifications are not always recognized as pointing
      !>         to the file they actually name.
      !>
      !>## Literature References
      !>
      !>     [1]  "VAX/VMS Guide to VAX/VMS System Management and Daily
      !>          Operations", Section 6.1.7, p 6-6, Digital Equipment
      !>          Corporation, September 1984.
      !>
      !>     [2]  "Microsoft FORTRAN Reference", Section C.3, p 404, Microsoft
      !>          Corporation, 1989.
      !>
      !>     [3]  "Sun FORTRAN Programmer's Guide", Section 7.2, p 73, Sun
      !>          Microsystems, Revision A, 6 May 1988.
      !>
      !>     [4]  "The Unix Man Pages," for limits on the HP and Silicon
      !>          Graphics. The value of OPEN_MAX refers to the number of files
      !>          a process can have open.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 6.27.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 6.26.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 6.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 6.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 6.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 6.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 6.21.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 6.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 6.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 6.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 6.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 6.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 6.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 6.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 6.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 6.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 6.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 6.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 6.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 6.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 6.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 6.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 6.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 6.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 6.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 6.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 6.0.6, 24-APR-2003 (EDW)
      !>
      !>        Added MAC-OSX-F77 to the list of platforms
      !>        that require READONLY to read write protected
      !>        kernels.
      !>
      !>-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 6.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 6.0.0, 07-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 5.0.0, 09-NOV-1993 (HAN)
      !>
      !>        Module was updated to include the values for MAXLEN and
      !>        MAXOPN and the appropriate OPEN statement for the Silicon
      !>        Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
      !>        value of 256 for Unix platforms was changed to 255.
      !>
      !>-    SPICELIB Version 4.1.0, 12-OCT-1992 (HAN)
      !>
      !>        Module was updated to include the parameters for the
      !>        Hewlett Packard UX 9000/750 environment.
      !>
      !>-    SPICELIB Version 4.0.0, 20-MAY-1992 (MJS)
      !>
      !>        INDEX saved.
      !>
      !>-    SPICELIB Version 3.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 3.0.0, 19-JUL-1991 (NJB)
      !>
      !>        Version 2.0.0 of RDTEXT produced a Fortran run-time error
      !>        if the input argument FILE was blank. This has been
      !>        repaired.
      !>
      !>-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
      !>
      !>        Value of N was initialized to zero. LINE is now filled
      !>        with blanks when an error occurs or when an end of file
      !>        is reached. Some small fix-ups in the header, including
      !>        re-ordering the sections correctly.
      !>
      !>-    SPICELIB Version 1.0.1, 20-MAR-1990 (HAN)
      !>
      !>        $Parameters section was updated to include the values
      !>        of MAXOPN for several machines. Sources of these values
      !>        are listed in the Literature References section.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 6.0.0, 07-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 5.0.0, 9-NOV-1993 (HAN)
      !>
      !>        Module was updated to include the values for MAXLEN and
      !>        MAXOPN and the appropriate OPEN statement for the Silicon
      !>        Graphics, DEC Alpha-OSF/1, and NeXT platforms. The previous
      !>        value of 256 for Unix platforms was changed to 255.
      !>
      !>-    SPICELIB Version 4.1.0, 12-OCT-1992 (HAN)
      !>
      !>        Module was updated to include the parameters for the
      !>        Hewlett Packard UX 9000/750 environment.
      !>
      !>        The code was also reformatted so that a utility program can
      !>        create the source file for a specific environment given a
      !>        master source file.
      !>
      !>-    SPICELIB Version 4.0.0, 20-MAY-1992 (MJS)
      !>
      !>        The variable INDEX was saved. Prior to this fix, when RDTEXT
      !>        closed a file, INDEX was used without being assigned a value.
      !>        Since INDEX always points to the current file (unit), saving
      !>        INDEX fixed this problem.
      !>
      !>-    SPICELIB Version 3.0.0, 19-JUL-1991 (NJB)
      !>
      !>        Version 2.0.0 of RDTEXT produced a Fortran run-time error
      !>        if the input argument FILE was blank. This has been
      !>        repaired.
      !>
      !>-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
      !>
      !>        In past versions when an end of file was reached or when error
      !>        occurred while reading the text file, LINE was returned with
      !>        its previous value. Now LINE is returned with blanks, in
      !>        accordance with the specifications given in the header.
      !>        The variable N, representing the number of files currently
      !>        open, was initialized to zero.
      !>
      !>        The method of checking whether the file to be read is one
      !>        already opened for reading by this routine has been improved.
      !>        Formerly, the input file name was compared against a list of
      !>        names of routines already opened by RDTEXT. If the input name
      !>        pointed to a file that had been opened using a different name,
      !>        RDTEXT would not recognize that the new name pointed to a file
      !>        that was already open. The technique used now greatly reduces
      !>        the chance of such an error. The input file name is compared
      !>        to the previous input file name, and if the names do not agree,
      !>        an INQUIRE is performed to test whether the file named by the
      !>        input file name is already open. Only if this INQUIRE
      !>        indicates that the file is not already open will RDTEXT attempt
      !>        to open the file.
      !>
      !>-    Beta Version 1.1.0, 17-FEB-1989 (IMU) (NJB)
      !>
      !>        The primary change was the addition of error handling.
      !>        At the same time, the parameters MAXOPN and MAXLEN were
      !>        moved into the calling sequence. The call to IOERR was
      !>        replaced by a call to SETMSG. The declaration of the unused
      !>        function FAILED was deleted. Finally, all internal references
      !>        to the entry point WRTEXT (which was dropped when the routine
      !>        left OPTLIB) were removed.
      module subroutine rdtext ( ctx, file, line, eof )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! Name of text file.
      character(*), intent(out) :: line  !! Next line from the text file.
      logical, intent(out) :: eof  !! End-of-file indicator.
      integer(ip), parameter :: maxopn = 96  !! maximum number of text files open at once
!
!     Local variables
!
      integer(ip) :: unit
      integer(ip) :: number
      integer(ip) :: iostat
      integer(ip) :: i
      logical :: same
      if ( .not. allocated ( ctx%spice%rdtext%lstfil ) ) ctx%spice%rdtext%lstfil = ' '
      associate ( units => ctx%spice%rdtext%units, &
                  n => ctx%spice%rdtext%n, &
                  lstunt => ctx%spice%rdtext%lstunt, &
                  index => ctx%spice%rdtext%index )
!
!     Set up the error processing.
!
      if ( ctx%failed() ) return

!
!     We will keep track of which files are open by storing the unit
!     numbers of those files. When a user requests a file to be read,
!     we first check if it is the same file as just previously read, if
!     not we use an INQUIRE statement to determine the open status and
!     unit number of the file. If the file is open we'll read it, if
!     not, well, we'll open it first. We could just skip the first
!     part, that is just use the INQUIRE statement, but that would
!     involve executing quite a few INQUIRE statements when just
!     reading one file and making this routine a much slower routine.
!

!
!     Are we reading the same file?
!
      same = ( ctx%spice%rdtext%lstfil == file )  .and. ( ctx%spice%rdtext%lstfil /= ' ' )

      if ( .not. same ) then

!
!        We still might have the same file. For example these three
!        names (on the VAX) are different but they represent the
!        same file:
!
!           1) MY$DISK:[MYDIR]MYFILE.DAT;
!
!           2) MYFILE.DAT;1
!
!           3) MYFILE.DAT
!
!        In other words, the user may have entered a different file
!        specification for the same file.
!
         number = 0

         inquire (  file    =  file, &
                    number  =  number, &
                    iostat  =  iostat  )

         if ( iostat /= 0 ) then
!
!           This is weird.  How can an INQUIRE statement fail,
!           if the syntax is correct?  But just in case...
!
            call ctx%setmsg ( 'INQUIRE error.  File = #, IOSTAT = #.' )
            call ctx%errch  ( '#', file                               )
            call ctx%errint ( '#', iostat                             )
            call ctx%sigerr ( 'SPICE(INQUIREFAILED)'                  )
            return

         end if

         index = findloc(units(1:n), number, dim=1)

         if ( index == 0 ) then
!
!           Well, we will treat it as a new file then.  We will
!           need a free logical unit. But only if we don't
!           have too many files open already.
!
            if ( n == maxopn ) then
               call ctx%setmsg ( 'Too many files open already.' )
               call ctx%sigerr ( 'SPICE(TOOMANYFILESOPEN)'      )
               return
            end if

!
!           Open the file (PEPPER: with a new unit from the runtime),
!           and hope nothing goes awry.
!

            open ( file            = file, &
                   newunit         = unit, &
                   status          = 'OLD', &
                   access          = 'STREAM', &
                   form            = 'UNFORMATTED', &
                   action          = 'READ', &
                   iostat          = iostat )

            if ( iostat /= 0 ) then
               call ctx%setmsg ( 'Could not open #.'     )
               call ctx%errch  ( '#', file               )
               call ctx%sigerr ( 'SPICE(FILEOPENFAILED)' )
               return
            end if

!
!           Whew! We're ready to read from this file. Save
!           the pertinent information:
!
!               - The number of files currently open.
!               - The logical unit connected to this file.
!               - The index of the file within the UNITS array.
!
            n         = n + 1
            units(n)  = unit
            index     = n
            ctx%spice%rdtext%pos(n) = 1
            inquire ( unit = unit, size = ctx%spice%rdtext%fsize(n) )

         end if

         ctx%spice%rdtext%lstfil = trim ( file )
         lstunt = units(index)

      end if

!
!     This is the easy part. Read the next line from the file.
!
!     PEPPER: the line is split from the bytes of the file here (LF,
!     CR-LF or a lone CR), not by a formatted read, whose run-time
!     libraries differ (Intel's does not take a lone CR as a line end).
!
      call next_line ( lstunt, ctx%spice%rdtext%pos(index), &
                       ctx%spice%rdtext%fsize(index), line, iostat )

!
!     Well, what happened? An end-of-file condition is indicated by
!     a negative value for IOSTAT. Any other non-zero value indicates
!     some other error. In any event, close the file immediately.
!     Repack the UNITS array, so that subsequent calls will not try to
!     read from the file without reopening it.
!
      eof = ( iostat < 0 )

      if ( iostat /= 0 ) then

         close ( units(index) )

         do i = index+1, n
            units(i-1)  = units(i)
            ctx%spice%rdtext%pos(i-1) = ctx%spice%rdtext%pos(i)
            ctx%spice%rdtext%fsize(i-1) = ctx%spice%rdtext%fsize(i)
         end do

         n = n - 1

!
!        Fill LINE with blanks.
!
         line = ' '

!
!        LSTFIL is no longer valid
!
         ctx%spice%rdtext%lstfil = ' '

!
!        If this is just the end of the file, don't report an error.
!        (All files have to end sometime.)
!
         if ( .not. eof ) then
            call ctx%setmsg ( 'Could not read from #.' )
            call ctx%errch  ( '#', file                )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'  )
            return
         end if

      end if

      return
      end associate

      contains

         !> PEPPER: the next line of a file read as a stream, from byte POS
         !> (updated): the bytes up to LF, CR-LF or a lone CR, or to the end
         !> of the file; truncated to the length of LINE, blank-padded.
         !> IOSTAT is negative at the end of the file, positive on an error.
         subroutine next_line ( unit, pos, size, line, iostat )
            use, intrinsic :: iso_fortran_env, only: int64
            integer(ip), intent(in) :: unit  !! the unit (stream access)
            integer(int64), intent(inout) :: pos  !! the position of the next byte
            integer(int64), intent(in) :: size  !! the size of the file (bytes)
            character(*), intent(out) :: line  !! the line
            integer(ip), intent(out) :: iostat  !! 0, negative at the end, positive on an error
            integer, parameter :: chunk = 512
            character(chunk) :: buf
            integer :: m, k, kept, ios, room
            character(1) :: next
            intrinsic :: scan  ! (not the scanner SCAN, a procedure of the context)
            line = ' '
            iostat = 0
            kept = 0
            if ( pos > size ) then
               iostat = -1
               return
            end if
            do
               m = int ( min ( int(chunk, int64), size - pos + 1 ) )
               read ( unit, pos = pos, iostat = ios ) buf(1:m)
               if ( ios /= 0 ) then
                  iostat = max ( ios, 1 )
                  return
               end if
               k = scan ( buf(1:m), achar(10) // achar(13) )
               if ( k == 0 ) then
                  ! no line end in this chunk: keep it all (as far as LINE
                  ! has room) and read on
                  room = min ( m, len(line) - kept )
                  if ( room > 0 ) line(kept+1:kept+room) = buf(1:room)
                  kept = kept + m
                  pos = pos + m
                  if ( pos > size ) return   ! the last line, without a terminator
                  cycle
               end if
               room = min ( k - 1, len(line) - kept )
               if ( room > 0 ) line(kept+1:kept+room) = buf(1:room)
               pos = pos + k
               if ( buf(k:k) == achar(13) ) then
                  ! CR-LF: one line end
                  if ( k < m ) then
                     if ( buf(k+1:k+1) == achar(10) ) pos = pos + 1
                  else if ( pos <= size ) then
                     read ( unit, pos = pos, iostat = ios ) next
                     if ( ios == 0 .and. next == achar(10) ) pos = pos + 1
                  end if
               end if
               return
            end do
         end subroutine next_line

      end subroutine rdtext

      !> Close a text file currently opened by [[rdtext]].
      !>
      !>## Detailed Input
      !>
      !>     FILE     is the name of a text file which is currently
      !>              opened for reading or writing by RDTEXT.
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
      !>     1)  If the attempt to "inquire" the status of the file fails,
      !>         the error SPICE(INQUIREFAILED) is signaled.
      !>
      !>## Files
      !>
      !>     The text file, FILE, was previously opened by RDTEXT.
      !>
      !>## Particulars
      !>
      !>     CLTEXT closes one of the files currently opened for reading or
      !>     writing by RDTEXT. If the specified file is not open, nothing
      !>     happens.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     1)  On VAX systems, caution should be exercised when using
      !>         multiple logical names to point to the same file. Logical
      !>         name translation supporting execution of the Fortran
      !>         INQUIRE statement does not appear to work reliably in all
      !>         cases, which may lead this routine to believe that different
      !>         logical names indicate different files. The specific problem
      !>         that has been observed is that logical names that include
      !>         disk specifications are not always recognized as pointing
      !>         to the file they actually name.
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
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 6.1.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 3.0.0, 27-SEP-1994 (WLT)
      !>
      !>        The check of RETURN was removed so that routines that need
      !>        to close a text file can do so even if an error has been
      !>        detected somewhere else in a user's program.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
      !>
      !>        Method of recognizing whether input file name points to
      !>        a file opened by RDTEXT has been improved. Header indentation
      !>        fixed.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 6.1.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>-    SPICELIB Version 3.0.0, 27-SEP-1994 (WLT)
      !>
      !>        The check of RETURN was removed so that routines that need
      !>        to close a text file can do so even if an error has been
      !>        detected somewhere else in a user's program.
      !>
      !>-    SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
      !>
      !>        Method of recognizing whether input file name points to
      !>        a file opened by RDTEXT has been improved. Formerly, CLTEXT
      !>        compared the input file name to a list of names of files
      !>        opened by RDTEXT. If the input name pointed to a file that
      !>        had been opened using a different name, CLTEXT would not
      !>        recognize that the new name pointed to a file that was already
      !>        open. The technique used now greatly reduces the chance of
      !>        such an error. Now, and INQUIRE is performed to obtain the
      !>        unit number attached to the file named by the input file name.
      !>        If this unit is attached to a file opened by RDTEXT, CLTEXT
      !>        will close that file.
      !>
      !>        Header indentation was fixed.
      !>
      !>-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
      !>
      !>        References to WRTEXT removed.
      module subroutine cltext ( ctx, file )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! Text file to be closed.
      logical :: return
      integer(ip) :: unit
      integer(ip) :: number
      integer(ip) :: iostat
      integer(ip) :: i
      logical :: same
      if ( .not. allocated ( ctx%spice%rdtext%lstfil ) ) ctx%spice%rdtext%lstfil = ' '
      associate ( units => ctx%spice%rdtext%units, &
                  n => ctx%spice%rdtext%n, &
                  lstunt => ctx%spice%rdtext%lstunt, &
                  index => ctx%spice%rdtext%index )
!
!     Which file?
!
      number = 0

      inquire (  file    =  file, &
                 number  =  number, &
                 iostat  =  iostat   )

      if ( iostat /= 0 ) then
!
!        This is weird.  How can an INQUIRE statement fail,
!        if the syntax is correct?  But just in case...
!
         call ctx%setmsg ( 'INQUIRE error.  File = #, IOSTAT = #.' )
         call ctx%errch  ( '#', file                               )
         call ctx%errint ( '#', iostat                             )
         call ctx%sigerr ( 'SPICE(INQUIREFAILED)'                  )
         return

      end if

      index = findloc(units(1:n), number, dim=1)

      if ( index > 0 ) then

         close ( units(index) )

         if ( units(index) == lstunt ) then
            ctx%spice%rdtext%lstfil = ' '
         end if

!
!        Remember all that salient information about the file?
!        Lose it.
!
         do i = index+1, n
            units(i-1)  = units(i)
            ctx%spice%rdtext%pos(i-1) = ctx%spice%rdtext%pos(i)
            ctx%spice%rdtext%fsize(i-1) = ctx%spice%rdtext%fsize(i)
         end do

         n = n - 1

      end if

      return
      end associate
      end subroutine cltext

      !> Open and initialize a SPICE ASCII kernel file.
      !>
      !>## Detailed Input
      !>
      !>     KERNEL   is the name of the SPICE ASCII kernel file to be opened
      !>              and initialized.
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
      !>     None.
      !>
      !>## Files
      !>
      !>     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
      !>     by RDKDAT.
      !>
      !>## Particulars
      !>
      !>     RDKNEW should be called prior to the first call to RDKDAT.
      !>     RDKNEW opens the kernel file and RDKDAT reads the lines of
      !>     data in the file.
      !>
      !>## Examples
      !>
      !>     In the following example, RDKNEW and RDKDAT are used to read
      !>     the contents of a kernel file.
      !>
      !>     Let the file KERNEL contain the following lines.
      !>
      !>        =============================================================
      !>
      !>        DELTA_T_A is defined to be 32.184 seconds, and should not
      !>        be changed except under the most unusual circumstances.
      !>
      !>        \begindata
      !>
      !>        DELTA_T_A       =   32.184
      !>
      !>        \begintext
      !>
      !>        The next three items determine the relativistic correction
      !>        in the difference ET - TAI. To turn the correction off,
      !>        just set K to zero.
      !>
      !>        \begindata
      !>
      !>        K               =    1.657D-3
      !>        ORBIT_ECC       =    1.671D-2
      !>        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
      !>
      !>        =============================================================
      !>
      !>     Then the code fragment
      !>
      !>        CALL RDKNEW ( KERNEL )
      !>        CALL RDKDAT ( LINE, EOF )
      !>
      !>        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
      !>           WRITE (6,*) LINE
      !>           CALL RDKDAT ( LINE, EOF )
      !>        END DO
      !>
      !>     prints the following lines.
      !>
      !>        =============================================================
      !>        DELTA_T_A       =   32.184
      !>        K               =    1.657D-3
      !>        ORBIT_ECC       =    1.671D-2
      !>        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
      !>        =============================================================
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
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.1, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
      !>
      !>        This entry point has been updated so as to be
      !>        able to parse text kernel lines containing tab
      !>        characters.
      !>
      !>-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
      !>
      !>        File rdker.f made a master file so as to
      !>        add the ZZSETNNREAD call. This call will exist
      !>        only in FORTRAN source intended for conversion
      !>        to C by the f2c utility.
      !>
      !>        The ZZSETNNREAD call activates and deactivates
      !>        the non-native text line read capability for the
      !>        CSPICE toolkit.
      !>
      !>-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
      !>
      !>         The entry point RDKLIN was added.
      !>
      !>-    SPICELIB Version 1.2.0, 01-JUN-1992 (MJS)
      !>
      !>         RDKER now initializes the variables BEGDAT and BEGTXT
      !>         in a portable way. On the first valid entry to this routine,
      !>         the backslash character in the form CHAR(92) is concatenated
      !>         individually to 'begindata' and 'begintext'.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine rdknew ( ctx, kernel )
      use pepper_kinds, only: ip
      use pepper_strings, only: rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: kernel  !! Kernel file.
      character(*), parameter :: dat = 'begindata'  !! the marker that begins a data section of a text kernel
      character(*), parameter :: txt = 'begintext'  !! the marker that begins a text section of a text kernel
      character(lnsize) :: first
      integer(ip) :: i
      integer(ip) :: r
      logical :: end
      if ( .not. allocated ( ctx%spice%rdker%file ) ) ctx%spice%rdker%file = ' '
      associate ( linnum => ctx%spice%rdker%linnum, &
                  status => ctx%spice%rdker%status, &
                  frstim => ctx%spice%rdker%frstim, &
                  begdat => ctx%spice%rdker%begdat, &
                  begtxt => ctx%spice%rdker%begtxt )
      if ( ctx%failed() ) return

!
!     Initialize the data delimiters if it hasn't been done already.
!
      if ( frstim ) then

         begdat = char(bslash) // dat
         begtxt = char(bslash) // txt

         frstim = .false.

      else
!
!        Close the previous file, if it hasn't been closed already.
!
         call cltext ( ctx, ctx%spice%rdker%file )

      end if

!
!     Close the new file, too, in case they are the same. No sense
!     burning up logical units.
!
      call cltext ( ctx, kernel )

!
!     Read the first line of the file. It can't possibly be a data
!     line, since data must be preceded by a \begindata marker, so
!     we needn't take any pains to save it.
!
!     We also initialize LINNUM to 1 so we know
!     the line number of the last line read and can return this
!     information from RDKLIN.
!

      call rdtext ( ctx, kernel, first, end )

!
!     Replace any tab characters with blanks.
!
      r = rtrim ( first )

      do i = 1, r

         if ( ichar(first(i:i)) == tabcde ) then

            first(i:i) = ' '

         end if

      end do

      first = adjustl(first)
      linnum =  1

!
!     The first line is enough to set the status for subsequent
!     calls to RDKDAT.
!
      if ( end ) then

         status = endoff
         call cltext ( ctx, kernel )

      else if ( first == begdat ) then
         status = indata

      else
         status = intext
      end if

!
!     Save the name of the file for future reference.
!
      ctx%spice%rdker%file = trim ( kernel )

      return
      end associate
      end subroutine rdknew

      !> Read the next line of data from a SPICE ASCII kernel file.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     LINE     is the next line of data from the kernel file most
      !>              recently opened by NEWKER. Data lines are non-blank lines
      !>              which lie between \begindata and \begintext markers.
      !>              Lines are returned left justified.
      !>
      !>     EOF      is .TRUE. when the end of the kernel file has been
      !>              reached, and is .FALSE. otherwise. The kernel file is
      !>              closed automatically when the end of the file is reached.
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
      !>     The SPICE ASCII kernel file KERNEL is opened by RDKNEW and read
      !>     by RDKDAT.
      !>
      !>## Particulars
      !>
      !>     RDKDAT is used internally by RDKVAR to retrieve successive lines
      !>     of data from the current kernel file. It exists primarily to
      !>     relieve RDKVAR of the responsibility of dealing with comment
      !>     blocks and blank lines.
      !>
      !>## Examples
      !>
      !>     In the following example, RDKNEW and RDKDAT are used to read
      !>     the contents of a kernel file.
      !>
      !>     Let the file KERNEL contain the following lines.
      !>
      !>        =============================================================
      !>
      !>        DELTA_T_A is defined to be 32.184 seconds, and should not
      !>        be changed except under the most unusual circumstances.
      !>
      !>        \begindata
      !>
      !>        DELTA_T_A       =   32.184
      !>
      !>        \begintext
      !>
      !>        The next three items determine the relativistic correction
      !>        in the difference ET - TAI. To turn the correction off,
      !>        just set K to zero.
      !>
      !>        \begindata
      !>
      !>        K               =    1.657D-3
      !>        ORBIT_ECC       =    1.671D-2
      !>        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
      !>
      !>        =============================================================
      !>
      !>     Then the code fragment
      !>
      !>        CALL RDKNEW ( KERNEL )
      !>        CALL RDKDAT ( LINE, EOF )
      !>
      !>        DO WHILE ( (.NOT. EOF) .AND. ( .NOT. FAILED () ) )
      !>           WRITE (6,*) LINE
      !>           CALL RDKDAT ( LINE, EOF )
      !>        END DO
      !>
      !>     prints the following lines.
      !>
      !>        =============================================================
      !>        DELTA_T_A       =   32.184
      !>        K               =    1.657D-3
      !>        ORBIT_ECC       =    1.671D-2
      !>        MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
      !>        =============================================================
      !>
      !>## Restrictions
      !>
      !>     1)  The input file must be opened and initialized by NEWKER prior
      !>         to the first call to RDKDAT.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.1, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 11-FEB-2008 (NJB)
      !>
      !>        This entry point has been updated so as to be
      !>        able to parse text kernel lines containing tab
      !>        characters.
      !>
      !>-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
      !>
      !>        File rdker.f made a master file so as to
      !>        add the ZZSETNNREAD call. This call will exist
      !>        only in FORTRAN source intended for conversion
      !>        to C by the f2c utility.
      !>
      !>        The ZZSETNNREAD call activates and deactivates
      !>        the non-native text line read capability for the
      !>        CSPICE toolkit.
      !>
      !>-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF.
      !>
      !>-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The entry point RDKLIN was added.
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
      !>-     Beta Version 2.0.0, 23-OCT-1989 (HAN)
      !>
      !>        A FAILED test was added to the DO-loop which reads
      !>        lines in the kernel file.
      !>
      !>        If the error action was set to 'RETURN' an infinite loop
      !>        could have resulted if RDTEXT failed and the loop conditions
      !>        were satisfied.
      module subroutine rdkdat ( ctx, line, eof )
      use pepper_kinds, only: ip
      use pepper_strings, only: rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: line  !! Next line of kernel data.
      logical, intent(out) :: eof  !! End of file indicator.
      character(*), parameter :: blank = ' '  !! a blank
      character(lnsize) :: first
      integer(ip) :: i
      integer(ip) :: r
      logical :: end
      if ( .not. allocated ( ctx%spice%rdker%file ) ) ctx%spice%rdker%file = ' '
      associate ( linnum => ctx%spice%rdker%linnum, &
                  status => ctx%spice%rdker%status, &
                  begdat => ctx%spice%rdker%begdat, &
                  begtxt => ctx%spice%rdker%begtxt )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If the previous call detected the end of the file,
!     this one should do the same.
!
      if ( status == endoff ) then
         eof = .true.
         return
      end if

!
!     Well, at least we can try to read a line. Adjust the status as
!     needed, return if appropriate, read another line if necessary.
!     Basically, we're looking for a non-blank line in a data segment.
!
!     Note that after every read, we increment LINNUM so we know
!     the line number of the last line read and can return this
!     information from RDKLIN.
!
      line = blank

      do while ( ( .not. ctx%failed() )          .and. &
                 ( status == intext  .or.  line == blank ) )

         call rdtext ( ctx, ctx%spice%rdker%file, line, eof )

!
!        Replace any tab characters with blanks.
!
         r = rtrim ( line )

         do i = 1, r

            if (  ichar(line(i:i)) == tabcde ) then

               line(i:i) = ' '

            end if

         end do

         line = adjustl(line)
         linnum = linnum + 1

         if ( eof ) then
            status = endoff
            call cltext ( ctx, ctx%spice%rdker%file )
            return

         else if ( line == begtxt ) then
            status = intext

         else if ( line == begdat ) then
            status = indata
            line   = blank
         end if

      end do

      return
      end associate
      end subroutine rdkdat

      !> Return the name of file and line number of the last line read by
      !> the entry point [[rdkdat]].
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     KERNEL   is the name of the last file supplied via a call to
      !>              RDKNEW. If no call to RDKNEW have been made KERNEL is
      !>              returned as a blank. If KERNEL is not sufficiently long
      !>              to hold th name of the file, the file name will be
      !>              truncated on the right.
      !>
      !>     NUMBER   is the number of the last line in KERNEL returned by a
      !>              call to RDKDAT. If no call to RDKNEW or RDKDAT have been
      !>              made NUMBER is returned with the value 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If no calls to RDKNEW have been made, KERNEL is returned as
      !>         a blank and NUMBER is returned with the value 0.
      !>
      !>     2)  If no calls to RDKDAT have been made but RDKNEW has been
      !>         called, NUMBER is returned with the value 1.
      !>
      !>     3)  If KERNEL is not sufficiently long to hold the name of the
      !>         file being read, the name will be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility to aid in determining the last
      !>     line read in a text file that is being read via RDKDAT.
      !>
      !>     It is particular useful in pointing out the location of
      !>     an error in an input file.
      !>
      !>## Examples
      !>
      !>     Suppose that you are processing a file and have detected an
      !>     error in the syntax in the file. The following code fragment
      !>     illustrates how you can use this routine to inform a user of
      !>     the location of the error in the file.
      !>
      !>        CALL RDKLIN ( FILE, NUMBER )
      !>        R =  RTRIM  ( FILE )
      !>
      !>        WRITE (*,*) 'An error occurred while reading line ', NUMBER
      !>        WRITE (*,*) 'of the file ''', FILE(1:R), ''''
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
      !>-    SPICELIB Version 2.1.1, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Corrected
      !>        output argument name in $Declarations section (changed FILE to
      !>        KERNEL)
      !>
      !>-    SPICELIB Version 2.1.0, 03-OCT-2005 (EDW)
      !>
      !>        File rdker.f made a master file so as to
      !>        add the ZZSETNNREAD call. This call will exist
      !>        only in FORTRAN source intended for conversion
      !>        to C by the f2c utility.
      !>
      !>        The ZZSETNNREAD call activates and deactivates
      !>        the non-native text line read capability for the
      !>        CSPICE toolkit.
      !>
      !>-    SPICELIB Version 2.0.0, 20-SEP-1995 (WLT)
      module subroutine rdklin ( ctx, kernel, number )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: kernel  !! The name of the current file that is being read.
      integer(ip), intent(out) :: number  !! The line number of the last line read in the file.
      logical :: return
      character(lnsize) :: first
      integer(ip) :: i
      integer(ip) :: r
      logical :: end
      if ( .not. allocated ( ctx%spice%rdker%file ) ) ctx%spice%rdker%file = ' '
      associate ( linnum => ctx%spice%rdker%linnum )
      kernel = ctx%spice%rdker%file
      number = linnum

      return
      end associate
      end subroutine rdklin

      !> Create an error message for ZZTOKNS token buffer and picture
      !> overflow exceptions.
      !>
      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     TEMPLT         is the input error message template. It should
      !>                    have two '#' characters in it.
      !>
      !>     STRING         is the string that should replace the first '#' in
      !>                    the template.
      !>
      !>     TOKEN          is the string that should replace the second '#'
      !>                    in the template.
      !>
      !>## Detailed Output
      !>
      !>     ERROR          is the complete output error message.
      !>
      !>     STATUS         is the output status flag always set .FALSE.
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
      !>     The sole purpose of this routine is to encapsulate in a single
      !>     routine the two REPMC calls and one ZZTOKNS = .FALSE. assignments
      !>     needed to make the error messages and returned status used in
      !>     numerous overflow check IF blocks in ZZTOKNS.
      !>
      !>## Examples
      !>
      !>     For these inputs
      !>
      !>        TEMPLT = 'This time string ''#'' has a bad token ''#''.'
      !>
      !>        STRING = '2001-01-01 boo 00:00:00'
      !>
      !>        TOKEN  = 'boo'
      !>
      !>     this routine returns
      !>
      !>        ERROR  =  'This time string ''2001-01-01 boo 00:00:00'' has '
      !>       .          'a bad token ''boo''.'
      !>
      !>        STATUS = .FALSE.
      !>
      !>## Restrictions
      !>
      !>     1) This is a private routine. SPICE user applications should not
      !>        call this routine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     B.V. Semenov   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 05-FEB-2014 (BVS)
      module subroutine zztknerr ( ctx, templt, string, token, error, status )
      use pepper_strings, only: repmc, repmc_inplace

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: templt  !! Error message template
      character(*), intent(in) :: string  !! First string to be substituted
      character(*), intent(in) :: token  !! Second string to be substituted
      character(*), intent(out) :: error  !! Complete error message
      logical, intent(out) :: status  !! Status flag
      call repmc ( templt, '#', string, error )
      call repmc_inplace ( error,  '#', token )
      status = .false.

      return

      end subroutine zztknerr

      !> Given a numeric pattern, construct the appropriate time format
      !> picture component.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     PICTUR     is a "TIMOUT" format picture that is under construction
      !>                The substring PICTUR(B:E) is supposed to be a sequence
      !>                of digits with possibly a decimal point in it.  The
      !>                digits before the decimal will be replaced by MARK.
      !>                The decimal point will be copied and digits after
      !>                the decimal point (up to 14 of them) will be replaced
      !>                by a the octothorpe character '#'.
      !>
      !>     B          are the beginning and ends of the substring mentioned
      !>     E          in PICTUR.
      !>
      !>     MARK       is a numeric time format component (DD, DOY, JULIAND,
      !>                HR, MN, SC )
      !>
      !>     PATTRN     a sequence of digits, possibly a leading minus sign
      !>                and possibly an embedded decimal point.
      !>
      !>## Detailed Output
      !>
      !>     PICTUR     is the input string with the appropriate time format
      !>                picture component inserted.
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
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that assists in the construction of
      !>     a format picture that corresponds to a particular instance
      !>     of a time string.
      !>
      !>## Examples
      !>
      !>     See ZZTIME.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 25-APR-1996 (WLT)
      module subroutine zzmkpc ( ctx, pictur, b, e, mark, pattrn )
      use pepper_kinds, only: ip
      use pepper_strings, only: zzrepsub_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(inout) :: pictur  !! A partially constructed time format picture
      integer(ip), intent(in) :: b  !! Beginning of substring to place a mark
      integer(ip), intent(in) :: e  !! End of substring to place a mark
      character(*), intent(in) :: mark  !! Initial portion of a mark
      character(*), intent(in) :: pattrn  !! Decimal pattern

      integer(ip), parameter :: maxplc = 14

      integer(ip), parameter :: maxsiz = maxplc + 12

      character(maxplc) :: places
      character(maxsiz) :: mymark

      integer(ip) :: lpat
      integer(ip) :: lmrk
      integer(ip) :: point
      integer(ip) :: use
      integer(ip) :: last

      places = '##############'
!
!     Construct the replacement marker.  First the unmodified
!     portion of the marker.  (We use LAST as the pointer to the
!     last valid character of the marker).
!
      lmrk   = len_trim(mark)
      lpat   = len   (pattrn)
      mymark = mark
      last   = lmrk
!
!     Is there a decimal point in the pattern?
!
      point  = index(pattrn, '.')

      if ( point > 0 ) then
!
!        We've got a decimal point.  We have to at least put this
!        into the marker.
!
         last              = last + 1
         mymark(last:last) = '.'
!
!        If the decimal point is not at the end of the pattern, we
!        will need to add some #'s to the marker (but not more than
!        MAXPLC of them).
!
         if ( point < lpat ) then

            use             = min ( maxplc, lpat - point )
            mymark(last+1:) = places(1:use)
            last            = last + use

         end if

      end if
!
!     We now let REPSUB do the work of replacing the substring
!     PICTUR(B:E) with the marker we've constructed.
!
      call zzrepsub_inplace ( pictur, b, e, mymark(1:last) )

      return

      end subroutine zzmkpc

      !> The function returns TRUE if a replacement is performed
      !>
      !> Replace matching tokens and remove the character "*"
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     SUB        is a substring of characters to located in the
      !>                current internal tokenized representation of a
      !>                time string that is maintained by ZZTIME.
      !>
      !>     REPLAC     is a string of characters that will replace 1 for 1
      !>                the characters in SUB.  Note that character * is
      !>                a special character in this substitution as it
      !>                will be removed (via ZZREMT) after substitution.
      !>
      !>     L2R        is a logical flag.  If L2R is TRUE, the search
      !>                for a substring matching SUB will be performed
      !>                in left to right order. If L2R is FALSE the
      !>                search for substring matching SUB will be performed
      !>                from right to left.
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE if a replacement is performed. Otherwise
      !>     it returns FALSE.
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
      !>     This routine is simply a macro for the combination of the
      !>     ZZTIME entry points ZZSUBT and ZZREMT
      !>
      !>## Examples
      !>
      !>     See TPARTV.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
      module logical function zzrept ( ctx, sub, replac, l2r )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: sub  !! is the substring to perform replacements on
      character(*), intent(in) :: replac  !! is the replacement string
      logical, intent(in) :: l2r  !! use left to right scanning if L2R is TRUE.

      logical :: ok

      zzrept = zzsubt ( ctx, sub, replac, l2r )
      ok     = zzremt ( ctx, '*' )

      return
      end function zzrept

      !> Determine whether the components of a time vector are in the
      !> "usual" range for the components, if component checking is
      !> enabled.
      !>
      !> If component checking is not enabled, this routine simply
      !> returns after setting the outputs.
      !>
      !>## Detailed Input
      !>
      !>     TVEC     is an array of double precision numbers that
      !>              represent the components of some calendar epoch.
      !>
      !>     TYPE     is kind of calendar epoch represented by TVEC
      !>              legitimate values are 'YMD' and 'YD'
      !>
      !>     MODS     is a logical flag indicating whether any of the
      !>              items in MODIFY are non-blank. If some item
      !>              in MODIFY is non-blank, MODS will be .TRUE. If
      !>              all items in MODIFY are blank, MODS will be .FALSE.
      !>
      !>     MODIFY   is an array of strings indicating how the
      !>              interpretation of the various components of TVEC
      !>              should be modified. Blank values indicate that
      !>              the default interpretation should be applied.
      !>              Non-blank components will have the following values
      !>              and meanings.
      !>
      !>               Component   Meaning   Possible Non-blank Modifier Values
      !>               ---------   -------   ----------------------------------
      !>               1           ERA       'A.D.', 'B.C.'
      !>               2           Weekday   'SUN',  'MON', ... etc.
      !>               3           AM/PM     'A.M.', 'P.M.'
      !>               4           System    'UTC',  'TDB', 'TDT'
      !>               5           Time Zone 'UTC+i:i', 'UTC-i:i'
      !>
      !>## Detailed Output
      !>
      !>     OK       is returned .TRUE. if all components of TVEC are within
      !>              the normal range of values. If some problem arises,
      !>              OK will be returned with the value .FALSE. Note that
      !>              component checking has not been enabled by a call
      !>              to TPARCH, the value of OK is automatically set to
      !>              .TRUE.
      !>
      !>     ERROR    if OK is returned with the value .TRUE., ERROR will be
      !>              returned as a blank. However, if OK is .FALSE., ERROR
      !>              will contain a diagnostic indicating what was wrong
      !>              with the components of TVEC. Note that
      !>              component checking has not been enabled by a call
      !>              to TPARCH, the value of ERROR is automatically set to
      !>              a blank.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  All problems with TVEC are reported via the logical OK
      !>         and the message ERROR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine works in conjunction with the entry point TPARCH.
      !>     If TPARCH has not been called with the input value 'YES' this
      !>     routine simply sets the outputs as indicated above and returns.
      !>
      !>     Usually strings such as February 32, 1997 are regarded as
      !>     erroneous. However, the SPICE time subsystem is capable
      !>     of attaching meaning to such strings. The routines TPARCH and
      !>     TCHECK allow you to treat such strings as erroneous throughout
      !>     the SPICE time sub-system.
      !>
      !>     This routine examines the components of a time vector and
      !>     determines whether or not all of the values in the vector
      !>     are within the normal bounds.
      !>
      !>     To pass inspection:
      !>
      !>        Years must be integers.
      !>
      !>        Months must be in the range from 1 to 12 and must be integers.
      !>
      !>        Days of the month must be in the normal ranges. For example
      !>             if the month specified is January, the day of the month
      !>             must be greater than or equal to 1.0D0 and strictly less
      !>             than 32.0D0 (The normal range for February is a function
      !>             of whether the year specified is a leap year. The
      !>             Gregorian calendar is used to determine leap years.)
      !>
      !>        Day of the year must be greater than or equal to 1.0D0
      !>             and strictly less than 366.0D0  (367.0D0 in a leap year.
      !>             The Gregorian calendar is used to determine leap years.)
      !>
      !>        Hours must be greater than or equal to 0.0D0 and strictly
      !>             less than 24.0D0. If the AMPM modifier is included
      !>             hours must be greater than or equal to 1.0D0 and strictly
      !>             less than 13.0D0.
      !>
      !>        Minutes must be greater than or equal to 0.0D0 and must
      !>             be strictly less than 60.0D0
      !>
      !>        Seconds must be greater than or equal to 0.0D0 and strictly
      !>             less than 60.0D0 (61.0D0 during the last minute of the
      !>             30th of June and the 31st of December).
      !>
      !>        If some component other than the seconds component is
      !>        not an integer, all components of lesser significance must
      !>        be zero.
      !>
      !>     This routine  is designed to work in conjunction
      !>     with the SPICE routine TPARTV and it is anticipated that
      !>     it will be called in the following fashion
      !>
      !>        CALL TPARTV ( STRING, TVEC, NTVEC,  TYPE,
      !>       .              MODIFY, MODS, YABBRV, SUCCES, ERROR )
      !>
      !>        IF ( .NOT. SUCCES ) THEN
      !>
      !>           communicate the diagnostic message and
      !>           take other actions as appropriate
      !>
      !>           RETURN
      !>
      !>        END IF
      !>
      !>        IF ( SUCCES .AND. CHECK ) THEN
      !>            CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
      !>        END IF
      !>
      !>        IF ( .NOT. OK ) THEN
      !>
      !>           communicate the diagnostic message and
      !>           take other actions as appropriate
      !>
      !>           RETURN
      !>
      !>        END IF
      !>
      !>## Examples
      !>
      !>     Suppose that you have parsed a string (via TPARTV) and want
      !>     to enforce normal ranges of the components. The following
      !>     sequence of calls will perform the checks on components.
      !>
      !>        get the current checking setting
      !>
      !>        CALL TCHCKD ( CURNT )
      !>
      !>        turn on component checking.
      !>
      !>        CALL TPARCH ( 'YES' )
      !>
      !>        Check the components.
      !>
      !>        CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
      !>
      !>        Reset the checking setting to the original value.
      !>
      !>        CALL TPARCH ( CURNT )
      !>
      !>        Now handle any problems that were diagnosed by TCHECK
      !>
      !>        IF ( .NOT. OK ) THEN
      !>
      !>           do something
      !>
      !>        END IF
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 01-NOV-2021 (JDR) (EDW)
      !>
      !>        Added logic to prevent evaluation of MODIFY when MODS false.
      !>
      !>        Added text listing routines affected and not affected by
      !>        explicit assignments to TPARCH.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 31-JAN-2017 (NJB)
      !>
      !>        Bug fix: updated logic so that B.C. leap years are recognized.
      !>
      !>-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
      !>
      !>        Fixed typo in the $Declarations section in the TPARCH header:
      !>        STRING -> TYPE.
      !>
      !>-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
      module subroutine tcheck ( ctx, tvec, type, mods, modify, ok, error )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc_inplace, repmd_inplace, repmi_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: tvec   ( * )  !! A vector of time components
      character(*), intent(in) :: type  !! The type of time vector.
      logical, intent(in) :: mods  !! A logical indicating the presence of modifiers
      character(*), intent(in) :: modify ( * )  !! The values of the modifiers
      logical, intent(out) :: ok  !! Indicates success or failure of component checks.
      character(*), intent(out) :: error  !! Diagnostic message if .NOT. OK.
      integer(ip) :: divble
      integer(ip), parameter :: ampm = zone + 1  !! time string component: AM/PM
      integer(ip), parameter :: lnsize = 200  !! length of the error message
      character(lnsize) :: messge
      character(10), parameter :: mnames ( 12 ) = &
         [ character(10) :: 'January', 'February', 'March', 'April', 'May', 'June', &
           'July', 'August', 'September', 'October', 'November', &
           'December' ]
      character(7), parameter :: cname  (  4 ) = &
         [ character(7) :: 'days', 'hours', 'minutes', 'seconds' ]
      real(wp) :: dinmon ( 12 )
      real(wp) :: doy
      real(wp) :: dinyr
      real(wp) :: jun30
      real(wp) :: hubnd
      real(wp) :: hlbnd
      integer(ip) :: comp
      integer(ip) :: day
      integer(ip) :: hour
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: leapdy
      integer(ip) :: minute
      integer(ip) :: month
      integer(ip) :: myear
      integer(ip) :: second
      integer(ip) :: year
      logical :: modtru
      divble(year,i) = max ( 0, 1 + ((iabs(year)/i)*i) - iabs(year) )
      associate ( dochck => ctx%spice%tcheck%dochck )
      if ( .not. dochck ) then
         ok    = .true.
         error = ' '
         return
      end if
!
!     Ok.  Checking has been enabled.  Proceed with the various
!     checks.
!
      year = nint( tvec(1) )

      modtru = .false.
      if ( mods ) then
         modtru = (modify(era) == 'B.C.')
      end if

      if ( modtru ) then
         myear = 1 - year
      else
         myear = year
      end if

      leapdy    =  divble( myear, 4   ) &
                -  divble( myear, 100 ) &
                +  divble( myear, 400 )

!     PEPPER: DINMON is a local variable now (SPICELIB saved it).
      dinmon    = [ 31.0_wp, 28.0_wp, 31.0_wp, 30.0_wp, 31.0_wp, 30.0_wp, 31.0_wp, 31.0_wp, 30.0_wp, 31.0_wp, 30.0_wp, 31.0_wp ]
      dinmon(2) = 28.0_wp  + real(leapdy, wp)
      dinyr     = 365.0_wp + real(leapdy, wp)
      jun30     = 181.0_wp + real(leapdy, wp)
!
!     The error message that will be attached to an out of range
!     problem for hours depends upon whether the AMPM modifier
!     was specified.  We set up valid range as well as the out
!     of range messages here.
!
      modtru = .false.
      if ( mods ) then
         modtru = (modify(ampm) /= ' ')
      end if

      if ( modtru ) then

         hubnd  = 13.0_wp
         hlbnd  =  1.0_wp
         messge = 'The hours component of the time specified was ' &
         //       '#. When either A.M. or P.M. is specified with ' &
         //       'the time the hours component must be at least ' &
         //       '1.0D0 and less than 13.0D0. '

      else

         hubnd  = 24.0_wp
         hlbnd  =  0.0_wp
         messge = 'The hours component of the time specified was ' &
         //       '#.  The hours component must be greater than ' &
         //       'or equal to 0.0D0 and less than 24.0D0. '

      end if
!
!     We only check YD and YMD anything else is out of the
!     province of this routine.
!
      if ( type /= 'YD' .and. type /= 'YMD' ) then

         ok    = .false.
         error = 'The type of the time vector specified was #, ' &
         //      'only ''YD'' and ''YMD'' are recognized. '
         call repmc_inplace ( error, '#', type )
         return

      end if

!
!     First check.  The year must be an integer.
!
      if ( tvec(1) /= real(year, wp) ) then
         ok    = .false.
         error = 'The year value was #.  This must be an ' &
         //      'integral value. '
         call repmd_inplace ( error,  '#', tvec(1), 8 )

         return
      end if

      if      ( type == 'YD' ) then

         day    = 2
         hour   = 3
         minute = 4
         second = 5
         doy    = tvec(2)

         if (      tvec(2) >= dinyr + 1.0_wp &
             .or.  tvec(2) < 1.0_wp       ) then

            ok    = .false.
            error = 'Day # has been specified for the year #. ' &
            //      'The correct range for the day of year for ' &
            //      'this year is from 1 to #. '

            call repmd_inplace ( error, '#', tvec(2), 8 )
            call repmi_inplace ( error, '#', year )
            call repmi_inplace ( error, '#', 365+leapdy )
            return

         end if

      else if ( type == 'YMD' ) then

         month  = nint( tvec(2) )
         day    = 3
         hour   = 4
         minute = 5
         second = 6
         doy    = 0.0_wp

         if ( tvec(2) /= real(month, wp) ) then

            ok    = .false.
            error = 'The month specified, #, was not an integer. ' &
            //      'The month must be an integer in the range ' &
            //      'from 1 to 12. '
            call repmd_inplace ( error,  '#', tvec(2), 3 )
            return

         else if (      tvec(2) <   1.0_wp &
                   .or. tvec(2) >  12.0_wp ) then

            ok    = .false.
            error = 'The month specified was #.  The month must ' &
            //      'be an integer in the range from 1 to 12 ' &
            //      '(inclusive). '
            call repmi_inplace ( error, '#', month )
            return

         else if (      tvec(3) < 1.0_wp &
                   .or. tvec(3) >= dinmon(month) + 1.0_wp ) then

            ok    = .false.
            error = 'The day of the month specified for the ' &
            //      'month of # was #.  For # the day must be at ' &
            //      'least 1.0D0 and less than #. '
            call repmc_inplace ( error, '#', mnames(month) )
            call repmd_inplace ( error, '#', tvec(3), 3 )
            call repmc_inplace ( error, '#', mnames(month) )
            call repmd_inplace ( error, '#', dinmon(month)+1.0_wp, 2 )
            return

         end if

         do i = 1, month -1
            doy = doy + dinmon(i)
         end do

         doy = doy + tvec(3)

      end if

!
!     Make sure the hours, minutes and seconds are all in range.
!
      if (     tvec(hour) >= hubnd &
          .or. tvec(hour) < hlbnd ) then

         ok    = .false.
         error = messge
         call repmd_inplace ( error,  '#', tvec(hour), 2 )
         return

      else if (     tvec(minute) >= 60.0_wp &
               .or. tvec(minute) <  0.0_wp ) then

         ok    = .false.
         error = 'The minutes component of the time specified ' &
         //      'was #. This value must be greater than or ' &
         //      'equal to 0.0 and less than 60.0. '
         call repmd_inplace ( error,  '#', tvec(minute), 2 )
         return

      end if

      if (     tvec(second) >= 60.0_wp &
          .or. tvec(second) <  0.0_wp ) then
!
!        We allow for the possibility that we might have a leapsecond.
!
         modtru = .false.
         if ( mods ) then
            modtru = ( modify(ampm) == 'P.M.')
         end if

         if (        tvec(second) < 61.0_wp &
              .and.  tvec(second) >  0.0_wp &
              .and.  tvec(minute) == 59.0_wp &
              .and.  tvec(hour)   == 23.0_wp &
              .and.( doy == dinyr .or. doy == jun30 ) ) then
!
!           Don't do anything.
!
         else if (        tvec(second) < 61.0_wp &
              .and.  tvec(second) >  0.0_wp &
              .and.  tvec(minute) == 59.0_wp &
              .and.  tvec(hour)   == 11.0_wp &
              .and.  modtru &
              .and.( doy == dinyr .or. doy == jun30 ) ) then
!
!           Don't do anything.
!
         else

            ok    = .false.
            error = 'The seconds component of time must be at least ' &
            //      '0.0D0 and less than 60.0D0 (61.0D0 during the ' &
            //      'last minute of June ' &
            //      '30 and December 31). The value supplied was #. '

            call repmd_inplace ( error,  '#', tvec(second), 8 )
            return

         end if
      end if

!
!     One final check.  If some component is not an integer
!     the remaining components must be zero.
!
      comp     = 0

      do i = day, minute

         comp = comp + 1
         k    = comp

         if ( tvec(i) /= nint(tvec(i)) ) then

            do j = i+1, second

               k = k+1

               if ( tvec(j) /= 0.0_wp ) then
                  ok    = .false.
                  error = 'The ''#'' component of the date has a ' &
                  //      'fractional component.  This is ' &
                  //      'allowed only if all components of ' &
                  //      'lesser significance have value 0.0D0. ' &
                  //      'However the ''#'' component has value ' &
                  //      '#. '

                  call repmc_inplace ( error, '#', cname(comp) )
                  call repmc_inplace ( error, '#', cname(k) )
                  call repmd_inplace ( error, '#', tvec(j), 2 )
                  return
               end if

            end do

         end if

      end do

!
!     If we make it this far, all components pass the reasonableness
!     tests.
!
      ok    = .true.
      error = ' '
      return
      end associate
      end subroutine tcheck

      !> Restrict the set of strings that are recognized by SPICE time
      !> parsing routines to those that have standard values for all time
      !> components.
      !>
      !>## Detailed Input
      !>
      !>     TYPE     is a character string that is used to adjust the set of
      !>              strings that will be regarded as valid time strings by
      !>              SPICE time parsing routines.
      !>
      !>              The default behavior of SPICE time software is to allow
      !>              an extended range of values for the various components
      !>              (tokens) of a time string. For example, using its default
      !>              behavior, TPARSE would regard 1993 JAN 367 as a valid
      !>              time string and return the UTC seconds past the J2000
      !>              epoch value that corresponds to Jan 2, 1994.
      !>
      !>              While this is a "reasonable" interpretation of such a
      !>              string, there may be occasions when such a string should
      !>              be regarded as an error.
      !>
      !>              By calling TPARCH with a value of 'YES', the action of
      !>              the time software will be modified. Strings that have
      !>              components that are out of the range of values used in
      !>              most English discourse will be regarded as errors. Thus
      !>              the numeric values of MONTH, DAY, HOUR, MINUTE, and
      !>              SECOND must satisfy the following conditions to be
      !>              regarded as legitimate calendar time strings.
      !>
      !>                 ITEM     Valid Range
      !>                 ------   -----------------------------------------
      !>                 MONTH    1 to 13
      !>                 DAY      1 to 365 (366 for leap years) when
      !>                          DAY is interpreted as the day of year
      !>                          i.e. the month token is empty.
      !>                          1 to 31  if month is January
      !>                          1 to 28  (29 in leap years) if month is
      !>                                   February
      !>                          1 to 31  if month is March
      !>                          1 to 30  if month is April
      !>                          1 to 31  if month is May
      !>                          1 to 31  if month is June
      !>                          1 to 30  if month is July
      !>                          1 to 31  if month is August
      !>                          1 to 30  if month is September
      !>                          1 to 31  if month is October
      !>                          1 to 30  if month is November
      !>                          1 to 31  if month is December
      !>                 HOUR     0 to 23
      !>                 MINUTE   0 to 59
      !>                 SECOND   0 up to but not including 60 on days that
      !>                          can not have a leapsecond.
      !>                          0 up to but not including 61 for times
      !>                          that are the last second of June or
      !>                          December. In other words,
      !>                               JUN 30, 23:59:60.xxxxxx...x
      !>                          and  DEC 31, 23:59:60.xxxxxx...x
      !>
      !>              To reset the action of time software to the default
      !>              action, set TYPE to a value that is not equivalent to
      !>              'YES' when case and spaces are ignored.
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
      !>     Error free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is used to alter the collections of strings
      !>     that SPICE software regards as legitimate calendar strings. The
      !>     default behavior of SPICE software is to accept strings such
      !>     as FEB 34, 1993 and to interpret these in a "natural way"
      !>     (FEB 34, 1993 is regarded as MARCH 6, 1993.) This behavior
      !>     is sometimes useful for "private" programs that you write.
      !>     However, such a string may be a typo (a finger accidentally hit
      !>     two keys for the day instead of one). Given that this string
      !>     does not appear in common usage, you may want to consider
      !>     that it is more likely the result of erroneous input. You
      !>     can alter the behavior of SPICE software so that it will
      !>     treat such a string as an error. To do this call this entry
      !>     point with TYPE having the value 'YES'.
      !>
      !>        CALL TPARCH ( 'YES' )
      !>
      !>     Until the behavior is reset by calling TPARCH with a value
      !>     other than 'YES' (such as 'NO'), SPICE software will treat all
      !>     out-of-bound components of time strings as errors.
      !>
      !>     If you are happy with the SPICE default interpretation of
      !>     strings, you do not need to make any calls to TPARCH.
      !>
      !>     All time parsing routines --including the top-level APIs TPARSE
      !>     and UTC2ET-- respect the setting assigned by TPARCH, except the
      !>     SPICELIB routine STR2ET.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) When accepting times as input interactively, you usually
      !>        read a string typed at a keyboard and then pass that string
      !>        to the SPICE time system to convert it to an ephemeris time.
      !>        The default behavior of SPICE software is to accept strings
      !>        such as FEB 34, 1993 and to interpret these in a "natural way"
      !>        (FEB 34, 1993 is regarded as MARCH 6, 1993.) The following
      !>        example code demonstrates how to modify this behavior.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TPARCH_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         TIMSTR
      !>      PARAMETER           ( TIMSTR = 'FEB 34, 1993' )
      !>
      !>      INTEGER               ERRMLN
      !>      PARAMETER           ( ERRMLN = 1000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(ERRMLN)    ERRMSG
      !>
      !>      DOUBLE PRECISION      SP2000
      !>
      !>C
      !>C     First, demonstrate the default behavior of SPICE.
      !>C     Let's get the number of UTC seconds past J2000 epoch.
      !>C
      !>      CALL TPARSE ( TIMSTR, SP2000, ERRMSG )
      !>
      !>      IF ( ERRMSG .EQ. ' ' ) THEN
      !>
      !>         WRITE(*,'(A,F18.6)') 'UTC (s): ', SP2000
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(2A)') 'Error  : ', ERRMSG
      !>
      !>      END IF
      !>
      !>C
      !>C     Now, turn error checking on and parse the time string
      !>C     again.
      !>C
      !>      CALL TPARCH ( 'YES' )
      !>      CALL TPARSE ( TIMSTR, SP2000, ERRMSG )
      !>
      !>      IF ( ERRMSG .EQ. ' ' ) THEN
      !>
      !>         WRITE(*,'(A,F18.6)') 'UTC (s): ', SP2000
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(2A)') 'Error  : ', ERRMSG
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        UTC (s):  -215352000.000000
      !>        Error  : The day of the month specified for the month of Feb***
      !>
      !>        Warning: incomplete output. 1 line extended past the right
      !>        margin of the header and has been truncated. This line is
      !>        marked by "***" at the end of the line.
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
      !>-    SPICELIB Version 1.0.2, 01-NOV-2021 (JDR) (EDW)
      !>
      !>        Added text listing routines affected and not affected by
      !>        explicit assignments to TPARCH.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added TIME to $Required_Reading list.
      !>
      !>-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
      !>
      !>        Fixed typo in the $Declarations section: STRING -> TYPE.
      !>
      !>-    SPICELIB Version 1.0.0, 07-APR-1996 (WLT)
      !>
      !>        The entry point TPARCH was moved from TPARSE to the routine
      !>        TCHECK so that all time parsing actions could be centralized.
      module subroutine tparch ( ctx, type )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: type  !! String: Use 'YES' to restrict time inputs.
      integer(ip) :: divble
      integer(ip), parameter :: lnsize = 200  !! length of the error message
      character(lnsize) :: messge
      character(10), parameter :: mnames ( 12 ) = &
         [ character(10) :: 'January', 'February', 'March', 'April', 'May', 'June', &
           'July', 'August', 'September', 'October', 'November', &
           'December' ]
      character(7), parameter :: cname  (  4 ) = &
         [ character(7) :: 'days', 'hours', 'minutes', 'seconds' ]
      real(wp) :: dinmon ( 12 )
      real(wp) :: doy
      real(wp) :: dinyr
      real(wp) :: jun30
      real(wp) :: hubnd
      real(wp) :: hlbnd
      integer(ip) :: comp
      integer(ip) :: day
      integer(ip) :: hour
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: leapdy
      integer(ip) :: minute
      integer(ip) :: month
      integer(ip) :: myear
      integer(ip) :: second
      integer(ip) :: year
      logical :: modtru
      divble(year,i) = max ( 0, 1 + ((iabs(year)/i)*i) - iabs(year) )
      associate ( dochck => ctx%spice%tcheck%dochck )
      dochck = eqstr( type, 'YES' )

      return
      end associate
      end subroutine tparch

      !> Determine whether component checking is enabled for time strings.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     TYPE     is a string that gives the answer to the question
      !>              "Is checking of components enabled?"  If checking
      !>              is enabled, the value returned will be "YES" if
      !>              checking is not enabled, the value returned will
      !>              be "NO".
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
      !>     This entry point allows you to "fetch" the current settings
      !>     regarding the checking of components of a time string. This
      !>     allows you to temporarily set the action to whatever is desired
      !>     in a particular piece of code and then reset the action to
      !>     the setting in effect prior to the routines activities.
      !>
      !>## Examples
      !>
      !>     Suppose you'd like to write a routine that always applies
      !>     component checking to the components of a time string.
      !>
      !>     Use this entry point together with TPARCH and TCHECK to
      !>     make use of the built-in SPICE capabilities
      !>
      !>        get the current setting.
      !>
      !>        CALL TCHCKD ( CURNT )
      !>        CALL TPARCH ( 'YES' )
      !>
      !>           perform some time
      !>           parsing activities.
      !>
      !>           check the components.
      !>
      !>        CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
      !>
      !>        Set the checking activity back to the value prior
      !>        to the work done here.
      !>
      !>        CALL TPARCH ( CURNT )
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
      !>-    SPICELIB Version 1.0.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 07-APR-1996 (WLT)
      module subroutine tchckd ( ctx, type )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: type  !! Answer to the question: "Is checking enabled?"
      integer(ip) :: divble
      integer(ip), parameter :: lnsize = 200  !! length of the error message
      character(lnsize) :: messge
      character(10), parameter :: mnames ( 12 ) = &
         [ character(10) :: 'January', 'February', 'March', 'April', 'May', 'June', &
           'July', 'August', 'September', 'October', 'November', &
           'December' ]
      character(7), parameter :: cname  (  4 ) = &
         [ character(7) :: 'days', 'hours', 'minutes', 'seconds' ]
      real(wp) :: dinmon ( 12 )
      real(wp) :: doy
      real(wp) :: dinyr
      real(wp) :: jun30
      real(wp) :: hubnd
      real(wp) :: hlbnd
      integer(ip) :: comp
      integer(ip) :: day
      integer(ip) :: hour
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: leapdy
      integer(ip) :: minute
      integer(ip) :: month
      integer(ip) :: myear
      integer(ip) :: second
      integer(ip) :: year
      logical :: modtru
      divble(year,i) = max ( 0, 1 + ((iabs(year)/i)*i) - iabs(year) )
      associate ( dochck => ctx%spice%tcheck%dochck )
      if ( dochck ) then
         type = 'YES'
      else
         type = 'NO'
      end if

      return
      end associate
      end subroutine tchckd

      !> Expand an abbreviated year to a full year specification.
      !>
      !>## Detailed Input
      !>
      !>     YEAR     is an "abbreviated year." In other words the 98 of
      !>              1998,  05 of 2005, etc.
      !>
      !>## Detailed Output
      !>
      !>     YEAR     is the expansion of the abbreviated year according
      !>              to the lower bound established in the entry point
      !>              TSETYR. By default if YEAR is 69 to 99, the output
      !>              is 1900 + the input value of YEAR. If YEAR is 0 to 68
      !>              the output value of YEAR is 2000 + the input value of
      !>              YEAR.
      !>
      !>              See the entry point TSETRY to modify this behavior.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If on input YEAR is not in the inclusive interval from
      !>         0 to 99, YEAR is returned unchanged.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows all of the SPICE time subsystem to handle
      !>     uniformly the expansion of "abbreviated" years.  (i.e. the
      !>     remainder after dividing the actual year by 100).
      !>
      !>     By using this routine together with the routine TSETYR you
      !>     can recover the actual year to associate with an abbreviation.
      !>
      !>     The default behavior is as follows
      !>
      !>        YEAR Input      YEAR Output
      !>        ----------      -----------
      !>            00              2000
      !>            01              2001
      !>             .                .
      !>             .                .
      !>             .                .
      !>            66              2066
      !>            67              2067
      !>            68              2068
      !>            69              1969
      !>             .                .
      !>             .                .
      !>             .                .
      !>            99              1999
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Demonstrate the default behavior of routine TEXPYR and then
      !>        modify it in order to set the lower bound of the expansion to
      !>        1980.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TEXPYR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NYEARS
      !>      PARAMETER           ( NYEARS = 10 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               I
      !>      INTEGER               EXYEAR
      !>      INTEGER               YEARS  ( NYEARS )
      !>
      !>C
      !>C     Set the input years.
      !>C
      !>      DATA                  YEARS /  0,  1, 68, 69, 70,
      !>     .                              78, 79, 80, 81, 99  /
      !>
      !>C
      !>C     Display the default behavior.
      !>C
      !>      WRITE(*,'(A)') 'Default behavior:'
      !>      WRITE(*,*)
      !>
      !>      WRITE(*,'(A)') 'In  Expansion'
      !>      WRITE(*,'(A)') '--  ---------'
      !>
      !>      DO I=1, NYEARS
      !>
      !>         EXYEAR = YEARS(I)
      !>         CALL TEXPYR ( EXYEAR )
      !>
      !>         WRITE(*,'(I2.2,5X,I4)') YEARS(I), EXYEAR
      !>
      !>      END DO
      !>
      !>C
      !>C     Set up the lower bound for the expansion of abbreviated
      !>C     years to 1980.
      !>C
      !>      CALL TSETYR ( 1980 )
      !>
      !>C
      !>C     Display the new behavior.
      !>C
      !>      WRITE(*,*)
      !>      WRITE(*,'(A)') 'Lower bound for expansion set to 1980:'
      !>      WRITE(*,*)
      !>
      !>      WRITE(*,'(A)') 'In  Expansion'
      !>      WRITE(*,'(A)') '--  ---------'
      !>
      !>      DO I=1, NYEARS
      !>
      !>         EXYEAR = YEARS(I)
      !>         CALL TEXPYR ( EXYEAR )
      !>
      !>         WRITE(*,'(I2.2,5X,I4)') YEARS(I), EXYEAR
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Default behavior:
      !>
      !>        In  Expansion
      !>        --  ---------
      !>        00     2000
      !>        01     2001
      !>        68     2068
      !>        69     1969
      !>        70     1970
      !>        78     1978
      !>        79     1979
      !>        80     1980
      !>        81     1981
      !>        99     1999
      !>
      !>        Lower bound for expansion set to 1980:
      !>
      !>        In  Expansion
      !>        --  ---------
      !>        00     2000
      !>        01     2001
      !>        68     2068
      !>        69     2069
      !>        70     2070
      !>        78     2078
      !>        79     2079
      !>        80     1980
      !>        81     1981
      !>        99     1999
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
      !>-    SPICELIB Version 2.0.1, 23-SEP-2020 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        example code.
      !>
      !>        Added TIME to the list of Required Readings.
      !>
      !>-    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT)
      !>
      !>        The default century was changed from 1950-2049 to 1969-2068
      !>
      !>-    SPICELIB Version 1.0.0, 08-APR-1996 (WLT)
      module subroutine texpyr ( ctx, year )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: year  !! The year of some epoch abbreviated/expanded.
      associate ( centry => ctx%spice%texpyr%centry, &
                  lbound => ctx%spice%texpyr%lbound )
      if ( year >= 100 .or. year < 0 ) return

      year = year + centry

      if ( year < lbound ) then
         year = year + 100
      end if

      return
      end associate
      end subroutine texpyr

      !> Set the lower bound on the 100 year range.
      !>
      !>## Detailed Input
      !>
      !>     YEAR     is the year associated with the lower bound on all year
      !>              expansions computed by the SPICELIB routine TEXPYR. For
      !>              example if YEAR is 1980, then the range of years that can
      !>              be abbreviated is from 1980 to 2079.
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
      !>     Error free.
      !>
      !>     1)  If YEAR is less than 1, no action is taken.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This entry point is used to set the range to which years
      !>     abbreviated to the last two digits will be expanded, allowing all
      !>     of the SPICE time subsystem routines to handle uniformly the
      !>     expansion those "abbreviated" years (i.e. the remainder after
      !>     dividing the actual year by 100.) The input supplied to this
      !>     routine represents the lower bound of the expansion interval. The
      !>     upper bound of the expansion interval is YEAR + 99.
      !>
      !>     The default expansion interval is from 1969 to 2068.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose that you need to manipulate time strings and that
      !>        you want to treat years components in the range from 0 to 99
      !>        as being abbreviations for years in the range from
      !>        1980 to 2079 (provided that the years are not modified by
      !>        an ERA substring). The example code below shows how you
      !>        could go about this.
      !>
      !>        Use the LSK kernel below to load the leap seconds and time
      !>        constants required for the conversions.
      !>
      !>           naif0012.tls
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TSETYR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               DATELN
      !>      PARAMETER           ( DATELN = 11 )
      !>
      !>      INTEGER               NTSTRS
      !>      PARAMETER           ( NTSTRS = 7 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(DATELN)    DATE   (NTSTRS)
      !>      CHARACTER*(DATELN)    TIMSTR
      !>
      !>      DOUBLE PRECISION      ET
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Assign an array of calendar dates.
      !>C
      !>      DATA                  DATE   / '00 JAN 21',
      !>     .                               '01 FEB 22',
      !>     .                               '48 MAR 23',
      !>     .                               '49 APR 24',
      !>     .                               '79 JUL 14',
      !>     .                               '80 FEB 02',
      !>     .                               '99 DEC 31' /
      !>
      !>C
      !>C     Load the required LSK.
      !>C
      !>      CALL FURNSH ( 'naif0012.tls' )
      !>
      !>C
      !>C     Set up the lower bound for the
      !>C     expansion of abbreviated years.
      !>C
      !>      CALL TSETYR ( 1980 )
      !>
      !>C
      !>C     Expand the years in input time strings.
      !>C
      !>      WRITE(*,*) 'Time string    Expansion'
      !>      WRITE(*,*) '-----------    -----------'
      !>
      !>      DO I = 1, NTSTRS
      !>
      !>         CALL STR2ET ( DATE(I), ET )
      !>         CALL TIMOUT ( ET, 'YYYY MON DD', TIMSTR )
      !>
      !>         WRITE(*,*) DATE(I), '    ', TIMSTR
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Time string    Expansion
      !>         -----------    -----------
      !>         00 JAN 21      2000 JAN 21
      !>         01 FEB 22      2001 FEB 22
      !>         48 MAR 23      2048 MAR 23
      !>         49 APR 24      2049 APR 24
      !>         79 JUL 14      2079 JUL 14
      !>         80 FEB 02      1980 FEB 02
      !>         99 DEC 31      1999 DEC 31
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
      !>-    SPICELIB Version 2.1.0, 23-SEP-2020 (JDR)
      !>
      !>        Fixed bug: Added check for "YEAR" to be positive in order to
      !>        update the lower bound for the expansion.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Added TIME to the list of Required Readings. Extended
      !>        description in $Particulars to further describe the intended
      !>        use of this routine.
      !>
      !>-    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT)
      !>
      !>        The default century was change from 1950-2049 to 1969-2068.
      !>
      !>-    SPICELIB Version 1.0.0, 08-APR-1996 (WLT)
      module subroutine tsetyr ( ctx, year )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: year  !! Lower bound on the 100 year interval of expansion
      associate ( centry => ctx%spice%texpyr%centry, &
                  lbound => ctx%spice%texpyr%lbound )
      if ( year > 0 ) then

         centry = ( year/100 ) * 100
         lbound =   year

      end if

      return
      end associate
      end subroutine tsetyr

      !> Initialize the built-in time patterns.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ROOM       an integer giving the room available for known patterns
      !>                and their meanings.
      !>
      !>                If ROOM does not equal the number of built-in patterns
      !>                the function returns only those patterns that will fit
      !>                and returns the value FALSE.
      !>
      !>## Detailed Output
      !>
      !>     NKNOWN     is the number of patterns/meanings returned in the
      !>                arrays KNOWN and MEANNG
      !>
      !>     KNOWN      is the array of automatically recognized calendar
      !>                date patterns.  KNOWN will be sorted according to
      !>                the FORTRAN collating sequence.
      !>
      !>     MEANNG     is the array of "meanings" associated with the built-in
      !>                patterns returned in the array KNOWN. MEANNG(I) is
      !>                the "meaning" associated with known pattern KNOWN(I).
      !>
      !>     The function returns TRUE if the arrays, KNOWN and MEANNG are
      !>     successfully initialized.  Otherwise it returns FALSE.
      !>
      !>## Parameters
      !>
      !>     COUNT      is the number of patterns/meanings that are
      !>                returned by this routine.
      !>
      !>## Exceptions
      !>
      !>     Error Free.
      !>
      !>     1) If ROOM is less than count, the function returns FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine that supports the SPICE routine
      !>     TPARTV that parses time strings.  This routine initializes
      !>     the set of built-in time patterns for use by TPARTV
      !>
      !>     The lengths of the strings assigned to an arbitrary MYKNWN
      !>     element must equal the string lengths of the corresponding
      !>     element MYMNNG. The relation, one-to-one and onto,
      !>     defining the meaning (or non meaning) and type of each
      !>     symbol in the time string abstract representation.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 3.2.0, 01-OCT-2020 (EDW) (NJB) (BVS)
      !>
      !>        Implemented a FIRST block to prevent reinitialization
      !>        of patterns on each entry to routine.
      !>
      !>        Added patterns to accommodate ISO formats with 'Z' suffix.
      !>
      !>-    SPICELIB Version 3.1.0, 11-DEC-2013 (EDW)
      !>
      !>        Corrected typo which showed MEANNG(203) assigned
      !>        the value 'm*D*YH*M' rather than the correct assignment
      !>        to MYMNNG( 203 ). The assignment error often prevented
      !>        the Time subsystem from parsing time strings with
      !>        the format 'i-i-Yi:i'.
      !>
      !>        Corrected header section ordering to meet SPICE requirements.
      !>
      !>-    SPICELIB Version 3.0.0, 16-AUG-2002 (WLT)
      !>
      !>        The interface of the routine was changed from
      !>
      !>           ZZTPATS( ROOM, KNOWN, MEANNG )
      !>
      !>        to
      !>
      !>           ZZTPATS( ROOM, NKNOWN, KNOWN, MEANNG )
      !>
      !>        and made error free.
      !>
      !>-    SPICELIB Version 2.0.0, 16-APR-1997 (WLT)
      !>
      !>        The collection of recognized built in patterns was
      !>        increased from 185 to 203 patterns.  The new patterns
      !>        begin at KNOWN(186) below.
      !>
      !>-    SPICELIB Version 1.0.0, 02-APR-1996 (WLT)
      module logical function zztpats ( ctx, room, nknown, known, meanng )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: room  !! The declared space available for patterns
      integer(ip), intent(out) :: nknown  !! The number of patterns returned.
      character(*), intent(out) :: known  ( * )  !! The patterns that are automatically
                                                 !! recognized
      character(*), intent(out) :: meanng ( * )  !! The meaning associated with the patterns.

      integer(ip), parameter :: count = ntpats  !! number of the known time patterns


      integer(ip) :: i
!
!     The time format patterns and their meanings (SPICELIB filled
!     these tables on the first call).
!

!
!     Copy as many patterns and meanings as the input arrays allow.
!
      nknown = min ( count, room )
!
!     PEPPER: the table (TPATS_KNOWN, TPATS_MEANING, in the submodule) is
!     stored in the order SPICELIB's sort gave it, so it is not sorted
!     here.
!
      do i = 1, nknown
         known (i) = tpats_known(i)
         meanng(i) = tpats_meaning(i)
      end do

!
!     If there wasn't sufficient room to get all of the patterns
!     and meanings, return FALSE.
!
      if ( count > room ) then
         zztpats = .false.
         return
      end if

      zztpats = .true.

      return
      end function zztpats

      !> Parse the components of a time string and return a vector of the
      !> components of that string. Also return an array of any modifiers
      !> present in the input string; these may alter the interpretation
      !> of the components.
      !>
      !> The function returns
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string that represents some
      !>              julian or calendar epoch.
      !>
      !>## Detailed Output
      !>
      !>     TVEC     is a vector of double precision numbers that represent
      !>              the input string. The number and meaning of the
      !>              components of TVEC depend upon the input string. This
      !>              meaning can be determined from the output variable
      !>              TYPE.
      !>
      !>                 TYPE     NTVEC     TVEC Components
      !>                 -----------------------------------------------------
      !>                 YMD      3 to 6    TVEC(1) is the calendar year
      !>                                    TVEC(2) is the numeric value of the
      !>                                            month (1-12)
      !>                                    TVEC(3) is the day of the month
      !>                                    TVEC(4) is the hour of the day
      !>                                    TVEC(5) is the minute of the hour
      !>                                    TVEC(6) is the second of the minute
      !>
      !>                 YD       2 to 5    TVEC(1) is the calendar year
      !>                                    TVEC(2) is the day of the year
      !>                                    TVEC(3) is the hour of the day
      !>                                    TVEC(4) is the minute of the hour
      !>                                    TVEC(5) is the second of the minute
      !>
      !>                 JD       1         TVEC(1) is the julian date
      !>
      !>              Note that the values of TVEC are not forced into the
      !>              normal ranges used in daily conversation.  TPARTV
      !>              simply reports what's found in the string and does
      !>              not pass judgement on the "correctness" of these
      !>              components.
      !>
      !>     NTVEC    is the actual number of components that were present
      !>              in the string. For example a user might have
      !>              supplied only year, month and day of an epoch.
      !>              In such a case NTVEC will be set to 3. The components
      !>              actually supplied will be 1 through NTVEC. Values
      !>              not supplied are set to zero.
      !>
      !>     TYPE     is the type of time string supplied. This is a function
      !>              of whether the string contains year, month and day,
      !>              day of year, or julian date.
      !>
      !>     MODIFY   is an array of character strings that indicate
      !>              whether a modifier to the calendar string was supplied.
      !>              If a particular modifier was not supplied, the
      !>              value of that component of MODIFY will be set to
      !>              a blank. Modifiers are used to change the meaning
      !>              of time strings.
      !>
      !>              For example 12:12:29 Jan 1, 1996  means 12 hours past
      !>              midnight on Jan 1, 1996 in the UTC time system. But
      !>              if we modify the string to be:
      !>
      !>                 12:12:29 A.M. Jan 1, Tuesday PDT 1996 B.C.
      !>
      !>              the string takes on an entirely different meaning.
      !>
      !>              Five different modifiers are recognized by TPARTV:
      !>              the era associated with the epoch, day of week of
      !>              the epoch, time zone of an epoch,  AM/PM used in
      !>              daily time usage, and the system (UTC, TDB, TT, or TDT).
      !>
      !>              Again whether or not modifiers are compatible with the
      !>              time and date components or with each other is not
      !>              determined by TPARTV. TPARTV simply reports what is
      !>              present in the string, leaving the task of deciding
      !>              the meaning of the string to the calling routine.
      !>
      !>              The components of MODIFY, their meaning and possible
      !>              values are given below.
      !>
      !>                                        Possible
      !>                 Component   Meaning    Non-blank Modifier Values
      !>                 ---------   ---------  -------------------------
      !>                 1           ERA        'A.D.', 'B.C.'
      !>                 2           Weekday    'SUN', 'MON', ... etc.
      !>                 3           Time Zone  'UTC+i:i', 'UTC-i:i'
      !>                 4           AM/PM      'A.M.', 'P.M.'
      !>                 5           System     'UTC', 'TDB', 'TT', 'TDT'
      !>
      !>              TPARTV recognizes the standard abbreviations of
      !>              all continental U.S. time zones.
      !>
      !>                 PDT --- Pacific  Daylight Time  (UTC-07:00)
      !>                 PST --- Pacific  Standard Time  (UTC-08:00)
      !>                 MDT --- Mountain Daylight Time  (UTC-06:00)
      !>                 MST --- Mountain Standard Time  (UTC-07:00)
      !>                 CDT --- Central  Daylight Time  (UTC-05:00)
      !>                 CST --- Central  Standard Time  (UTC-06:00)
      !>                 EDT --- Eastern  Daylight Time  (UTC-04:00)
      !>                 EST --- Eastern  Standard Time  (UTC-05:00)
      !>
      !>              In addition it recognizes offsets from UTC expressed
      !>              as UTC+/-HR:MN. Note that through out SPICELIB
      !>              the minutes component of the UTC offset are always
      !>              regarded as positive offsets from the hour offset.
      !>
      !>              All Time zones are returned in MODIFY as UTC offsets
      !>              as indicated in the table above.
      !>
      !>     MODS     is .TRUE. if some non-blank modifier was supplied.
      !>
      !>     YABBRV   is .TRUE. if a year was supplied in the abbreviated
      !>              form 'YR  where YR is a two digit integer.
      !>
      !>     SUCCES   is .TRUE. if the string was successfully parsed.
      !>              Otherwise it is set to .FALSE. and a diagnostic
      !>              is supplied in the argument ERROR.
      !>
      !>     PICTUR   is a string that gives a format picture that can
      !>              be used by the routine TIMOUT to construct a time
      !>              string of the same form as the input time string.
      !>
      !>              If some component of the input string could not be
      !>              identified, PICTUR is returned as a blank. However,
      !>              if all components of the input string could be
      !>              identified and the string is simply ambiguous, PICTUR
      !>              will contain a format picture that corresponds to
      !>              the ambiguous input. Consequently, you must check
      !>              the value of PICTUR to determine if TPARTV has
      !>              been able to construct a format picture.
      !>
      !>     ERROR    is blank if the string was successfully parsed.
      !>              Otherwise a human readable diagnostic is returned
      !>              in ERROR.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  All problems detected by this routine are reported via the
      !>         variables SUCCES and ERROR.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine parses in input string that represents some
      !>     epoch in some time system. In addition it constructs a
      !>     format picture that describes the position and meaning
      !>     of the various components of the string.
      !>
      !>     This routine is intended to be used in close conjunction with
      !>     the routines TTRANS and TIMOUT.
      !>
      !>     The string is parsed by first determining its recognizable
      !>     substrings (integers, punctuation marks, names of months,
      !>     names of weekdays, time systems, time zones, etc.) These
      !>     recognizable substrings are called the tokens of the input
      !>     string. The meaning of some tokens are immediately determined.
      !>     For example named months, weekdays, time systems have clear
      !>     meanings. However, the meanings of numeric components must
      !>     be deciphered from their magnitudes and location in
      !>     the string relative to the immediately recognized components
      !>     of the input string.
      !>
      !>     To determine the meaning of the numeric tokens in the input
      !>     string, a set of "productions rules" and transformations are
      !>     applied to the full set of tokens in the string. These
      !>     transformations are repeated until the meaning of every token
      !>     has been determined or until further transformations yield
      !>     no new clues into the meaning of the numeric tokens.
      !>
      !>     1)  Unless the substring 'JD' or 'jd' is present, the string is
      !>         assumed to be a calendar format (day-month-year or year and
      !>         day of year). If the substring JD or jd is present, the
      !>         string is assumed to represent a Julian date.
      !>
      !>     2)  If the Julian date specifier is not present, any integer
      !>         greater than 999 is regarded as being a year specification.
      !>
      !>     3)  A dash '-' can represent a minus sign only if it precedes
      !>         the first digit in the string and the string contains
      !>         the Julian date specifier (JD). (No negative years,
      !>         months, days, etc. are allowed).
      !>
      !>     4)  Numeric components of a time string must be separated
      !>         by a character that is not a digit or decimal point.
      !>         Only one decimal component is allowed. For example
      !>         1994219.12819 is sometimes interpreted as the
      !>         219th day of 1994 + 0.12819 days. TPARTV does not
      !>         support such strings.
      !>
      !>     5)  No exponential components are allowed. For example you
      !>         can't specify the Julian date of J2000 as 2.451545E6.
      !>         You also can't input 1993 Jun 23 23:00:01.202E-4 and have
      !>         to explicitly list all zeros that follow the decimal
      !>         point: i.e. 1993 Jun 23 23:00:00.0001202.
      !>
      !>     6)  The single colon (:) when used to separate numeric
      !>         components of a string is interpreted as separating
      !>         Hours, Minutes, and Seconds of time.
      !>
      !>     7)  If a double slash (//) or double colon (::) follows
      !>         a pair of integers, those integers are assumed  to
      !>         represent the year and day of year.
      !>
      !>     8)  A quote followed by an integer less than 100 is regarded
      !>         as an abbreviated year. For example: '93 would be regarded
      !>         as the 93rd year of the reference century. See the SPICELIB
      !>         routine TEXPYR for further discussion of abbreviated years.
      !>
      !>     9)  An integer followed by 'B.C.' or 'A.D.' is regarded as
      !>         a year in the era associated with that abbreviation.
      !>
      !>     10) All dates are regarded as belonging to the extended
      !>         Gregorian Calendar (the Gregorian calendar is the calendar
      !>         currently used by western society). See the SPICELIB routine
      !>         JUL2GR for converting from Julian Calendar to the Gregorian
      !>         Calendar.
      !>
      !>     11) If the ISO date-time separator (T) is present in the string
      !>         ISO allowed token patterns are examined for a match
      !>         with the current token list. If no match is found the
      !>         search is abandoned and appropriate diagnostic messages
      !>         are generated. Historically the interpretation of ISO
      !>         formatted time strings deviates from the ISO standard in
      !>         allowing two digit years and expanding years in the 0 to 99
      !>         range the same way as is done for non ISO formatted strings.
      !>         Due to this interpretation it is impossible to specify
      !>         times in years in the 0 A.D. to 99 A.D. range using ISO
      !>         formatted strings on the input.
      !>
      !>     12) If two delimiters are found in succession in the time
      !>         string, the time string is diagnosed as an erroneous string.
      !>         (Delimiters are comma, white space, dash, slash, period, or
      !>         day of year mark. The day of year mark is a pair of forward
      !>         slashes or a pair of colons.)
      !>
      !>         Note the delimiters do not have to be the same. The pair
      !>         of characters ",-" counts as two successive delimiters.
      !>
      !>     13) White space and commas serve only to delimit tokens in the
      !>         input string. They do not affect the meaning of any
      !>         of the tokens.
      !>
      !>     14) If an integer is greater than 1000 (and the 'JD' label
      !>         is not present, the integer is regarded as a year.
      !>
      !>     15) When the size of the integer components does not clearly
      !>         specify a year the following patterns are assumed
      !>
      !>         Calendar Format
      !>
      !>            Year Month Day
      !>            Month Day Year
      !>            Year Day Month
      !>
      !>            where Month is the name of a month, not its numeric
      !>            value.
      !>
      !>            When integer components are separated by slashes (/)
      !>            as in 3/4/5. Month, Day, Year is assumed (2005 March 4)
      !>
      !>         Day of Year Format.
      !>
      !>            If a day of year marker is present (// or ::) the
      !>            pattern
      !>
      !>              I-I// or I-I:: (where I stands for an integer)
      !>
      !>            is interpreted as Year Day-of-Year. However, I-I/ is
      !>            regarded as ambiguous.
      !>
      !>     The table below gives a list of abbreviations used to
      !>     classify tokens.
      !>
      !>                /   ---  slash punctuation mark
      !>                H   ---  hour
      !>                M   ---  Minute
      !>                S   ---  Second
      !>                Y   ---  year
      !>                d   ---  day of year marker
      !>                i   ---  unsigned integer
      !>                m   ---  month
      !>                n   ---  unsigned decimal number
      !>                y   ---  day of year
      !>                -   ---  dash punctuation mark
      !>                D   ---  day of month
      !>                :   ---  colon punctuation mark
      !>
      !>      Given these abbreviations the following (rather lengthy)
      !>      table gives the set of built in token patterns that
      !>      are recognized and the associated interpretation of that
      !>      pattern.
      !>
      !>         Pattern         Meaning         Pattern         Meaning
      !>         ------------------------        -------------------------
      !>         Y-i-it......... YmD             i/i/ii:i:n..... mDYHMS
      !>         Y-i-iti........ YmDH            i/i/ii:n....... mDYHM
      !>         Y-i-iti:i...... YmDHM           i/i/ii:n....... mDYHM
      !>         Y-i-iti:i:i.... YmDHMS          i:i:ii-i-Y..... HMSmDY
      !>         Y-i-iti:i:n.... YmDHMS          i:i:ii/i/Y..... HMSmDY
      !>         Y-i-iti:n...... YmDHM           i:i:ii/i/i..... HMSmDY
      !>         Y-i-itn........ YmDH            i:i:iimY....... HMSDmY
      !>         Y-i/........... Yy              i:i:imiY....... HMSmDY
      !>         Y-i/i:i........ YyHM            i:i:ni-i-Y..... HMSmDY
      !>         Y-i/i:i:i...... YyHMS           i:i:ni/i/Y..... HMSmDY
      !>         Y-i/i:i:n...... YyHMS           i:i:ni/i/i..... HMSmDY
      !>         Y-i/i:n........ YyHM            i:i:nimY....... HMSDmY
      !>         Y-id........... Yy              i:i:nmiY....... HMSmDY
      !>         Y-idi:i........ YyHM            i:ii-i-Y....... HMmDY
      !>         Y-idi:i:i...... YyHMS           i:ii/i/Y....... HMmDY
      !>         Y-idi:i:n...... YyHMS           i:ii/i/i....... HMmDY
      !>         Y-idi:n........ YyHM            i:iimY......... HMDmY
      !>         Y-it........... Yy              i:imiY......... HMmDY
      !>         Y-iti.......... YyH             i:ni-i-Y....... HMmDY
      !>         Y-iti:i........ YyHM            i:ni/i/Y....... HMmDY
      !>         Y-iti:i:i...... YyHMS           i:ni/i/i....... HMmDY
      !>         Y-iti:i:n...... YyHMS           i:nimY......... HMDmY
      !>         Y-iti:n........ YyHM            i:nmiY......... HMmDY
      !>         Y-itn.......... YyH             iYd............ yY
      !>         Yid............ Yy              iYdi:i......... yYHM
      !>         Yidi:i......... YyHM            iYdi:i:i....... yYHMS
      !>         Yidi:i:i....... YyHMS           iYdi:i:n....... yYHMS
      !>         Yidi:i:n....... YyHMS           iYdi:n......... yYHM
      !>         Yidi:n......... YyHM            iiY............ mDY
      !>         Yii............ YmD             iiYi........... mDYH
      !>         Yiii........... YmDH            iiYi:i......... mDYHM
      !>         Yiii:i......... YmDHM           iiYi:i:i....... mDYHMS
      !>         Yiii:i:i....... YmDHMS          iiYi:i:n....... mDYHMS
      !>         Yiii:i:n....... YmDHMS          iiYi:n......... mDYHM
      !>         Yiii:n......... YmDHM           iiYn........... mDYH
      !>         Yiiii.......... YmDHM           iid............ Yy
      !>         Yiiiii......... YmDHMS          iidi:i......... YyHM
      !>         Yiiiin......... YmDHMS          iidi:i:i....... YyHMS
      !>         Yiiin.......... YmDHM           iidi:i:n....... YyHMS
      !>         Yiin........... YmDH            iidi:n......... YyHM
      !>         Yim............ YDm             iim............ YDm
      !>         Yimi........... YDmH            iimi........... YDmH
      !>         Yimi:i......... YDmHM           iimi:i......... YDmHM
      !>         Yimi:i:i....... YDmHMS          iimi:i:i....... YDmHMS
      !>         Yimi:i:n....... YDmHMS          iimi:i:n....... YDmHMS
      !>         Yimi:n......... YDmHM           iimi:n......... YDmHM
      !>         Yimn........... YDmH            iimii.......... YDmHM
      !>         Yin............ YmD             iimiii......... YDmHMS
      !>         Ymi............ YmD             iimiin......... YDmHMS
      !>         Ymii........... YmDH            iimin.......... YDmHM
      !>         Ymii:i......... YmDHM           iimn........... YDmH
      !>         Ymii:i:i....... YmDHMS          imY............ DmY
      !>         Ymii:i:n....... YmDHMS          imYi........... DmYH
      !>         Ymii:n......... YmDHM           imYi:i......... DmYHM
      !>         Ymin........... YmDH            imYi:i:i....... DmYHMS
      !>         Ymn............ YmD             imYi:i:n....... DmYHMS
      !>         Ynm............ YDm             imYi:n......... DmYHM
      !>         i-Y/........... yY              imYn........... DmYH
      !>         i-Y/i:i........ yYHM            imi............ YmD
      !>         i-Y/i:i:i...... yYHMS           imi:i:iY....... DmHMSY
      !>         i-Y/i:i:n...... yYHMS           imi:i:nY....... DmHMSY
      !>         i-Y/i:n........ yYHM            imi:iY......... DmHMY
      !>         i-Yd........... yY              imi:nY......... DmHMY
      !>         i-Ydi:i........ yYHM            imii........... YmDH
      !>         i-Ydi:i:i...... yYHMS           imii:i......... YmDHM
      !>         i-Ydi:i:n...... yYHMS           imii:i:i....... YmDHMS
      !>         i-Ydi:n........ yYHM            imii:i:n....... YmDHMS
      !>         i-i-Y.......... mDY             imii:n......... YmDHM
      !>         i-i-Yi:i....... mDYHM           imiii.......... YmDHM
      !>         i-i-Yi:i:i..... mDYHMS          imiiii......... YmDHMS
      !>         i-i-Yi:i:n..... mDYHMS          imiiin......... YmDHMS
      !>         i-i-Yi:n....... mDYHM           imiin.......... YmDHM
      !>         i-i-it......... YmD             imin........... YmDH
      !>         i-i-iti........ YmDH            imn............ YmD
      !>         i-i-iti:i...... YmDHM           inY............ mDY
      !>         i-i-iti:i:i.... YmDHMS          inm............ YDm
      !>         i-i-iti:i:n.... YmDHMS          miY............ mDY
      !>         i-i-iti:n...... YmDHM           miYi........... mDYH
      !>         i-i-itn........ YmDH            miYi:i......... mDYHM
      !>         i-i/i:i........ YyHM            miYi:i:i....... mDYHMS
      !>         i-i/i:i:i...... YyHMS           miYi:i:n....... mDYHMS
      !>         i-i/i:i:n...... YyHMS           miYi:n......... mDYHM
      !>         i-i/i:n........ YyHM            miYn........... mDYH
      !>         i-idi:i........ YyHM            mii............ mDY
      !>         i-idi:i:i...... YyHMS           mii:i:iY....... mDHMSY
      !>         i-idi:i:n...... YyHMS           mii:i:nY....... mDHMSY
      !>         i-idi:n........ YyHM            mii:iY......... mDHMY
      !>         i-it........... Yy              mii:nY......... mDHMY
      !>         i-iti.......... YyH             miii........... mDYH
      !>         i-iti:i........ YyHM            miii:i......... mDYHM
      !>         i-iti:i:i...... YyHMS           miii:i:i....... mDYHMS
      !>         i-iti:i:n...... YyHMS           miii:i:n....... mDYHMS
      !>         i-iti:n........ YyHM            miii:n......... mDYHM
      !>         i-itn.......... YyH             miiii.......... mDYHM
      !>         i/i/Y.......... mDY             miiiii......... mDYHMS
      !>         i/i/Y/i:n...... mDYHM           miiiin......... mDYHMS
      !>         i/i/Yi:i....... mDYHM           miiin.......... mDYHM
      !>         i/i/Yi:i:i..... mDYHMS          miin........... mDYH
      !>         i/i/Yi:i:n..... mDYHMS          mnY............ mDY
      !>         i/i/i.......... mDY             mni............ mDY
      !>         i/i/ii:i....... mDYHM           nmY............ DmY
      !>         i/i/ii:i:i..... mDYHMS
      !>
      !>## Examples
      !>
      !>     Suppose you need to convert various time strings to ephemeris
      !>     seconds past J2000. The following pair of calls shows
      !>     how you would use this routine together with the routines
      !>     TCHECK and TTRANS to perform this task.
      !>
      !>         CALL TPARTV ( STRING,
      !>        .              TVEC,   NTVEC, TYPE,
      !>        .              MODIFY, MODS,  YABBRV, SUCCES,
      !>        .              PICTUR, ERROR )
      !>
      !>         IF ( .NOT. SUCCES ) THEN
      !>
      !>            Use the SPICE error handling facility to post an
      !>            error message and signal an error.
      !>
      !>            CALL SETMSG ( ERROR )
      !>            CALL SIGERR ( 'MYCHECK(BADTIME)' )
      !>            CALL CHKOUT ( 'MYROUTINE' )
      !>            RETURN
      !>         END IF
      !>
      !>         Check the components of TVEC to make sure everything
      !>         makes sense.
      !>
      !>         CALL TCHECK( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
      !>
      !>         IF ( .NOT. OK ) THEN
      !>
      !>            Use the SPICE error handling facility to post an
      !>            error message and signal an error.
      !>
      !>            CALL SETMSG ( ERROR )
      !>            CALL SIGERR ( 'MYCHECK(BADTIME)' )
      !>            CALL CHKOUT ( 'MYROUTINE' )
      !>            RETURN
      !>         END IF
      !>
      !>         CALL TTRANS ( TYPE, 'ET', TVEC )
      !>
      !>         ET = TVEC(1)
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
      !>-    SPICELIB Version 3.2.0, 23-DEC-2021 (EDW) (BVS) (JDR)
      !>
      !>        ISO format logic recognizes/evaluates ISO time strings
      !>        with or without trailing 'Z'.
      !>
      !>        Reordered header sections. Edited the header to comply with
      !>        NAIF standard.
      !>
      !>        Updated $Exceptions entry #1 wording.
      !>
      !>-    SPICELIB Version 3.1.0, 15-AUG-2002 (WLT)
      !>
      !>        Replaced the call to INSSUB with ZZINSSUB so that this
      !>        routine can legitimately be called error free.
      !>
      !>-    SPICELIB Version 3.0.0, 10-MAY-1999 (WLT)
      !>
      !>        The routine was modified so that weekday followed by a comma
      !>        is recognized as a legitimate pattern when parsing.
      !>
      !>-    SPICELIB Version 2.0.0, 16-APR-1997 (WLT)
      !>
      !>        The routine was modified so that last-chance removal of
      !>        delimiters ',', '-', and '/' are removed one at a time
      !>        (instead of all at once as in version 1.0.0) and the
      !>        resulting representation checked against
      !>        the built-in list.
      !>
      !>        In addition the set of built-in patterns was increased
      !>        from 185 to 203. See ZZTPATS for more details.
      !>
      !>-    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT)
      module subroutine tpartv ( ctx, string, tvec, ntvec, type, modify, mods, yabbrv, succes, pictur, error )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      use pepper_arrays, only: bsrchc
      use pepper_strings, only: prefix, rtrim, suffix, ucase, repmc_inplace, zzinssub_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! A string to be parsed as a time
      real(wp), intent(out) :: tvec   ( * )  !! A vector giving the components of the time.
      integer(ip), intent(out) :: ntvec  !! The number of components supplied for TVEC
      character(*), intent(out) :: type  !! The type of the "time vector" TVEC
      character(*), intent(out) :: modify ( * )  !! A list of modifiers present in STRING.
      logical, intent(out) :: mods  !! A logical indicating the presence of a modifier
      logical, intent(out) :: yabbrv  !! A logical indicating that a year was abbreviated
      logical, intent(out) :: succes  !! A logical indicating whether STRING was parsed.
      character(*), intent(out) :: pictur  !! A time format picture associated with STRING
      character(*), intent(out) :: error  !! A diagnostic message if STRING couldn't be parsed

!
!     Parameters
!
!     ERA
!     WDAY
!     ZONE
!     AMPM
!     SYSTEM
!




      integer(ip), parameter :: ampm = zone + 1  !! time string component: AM/PM

      integer(ip), parameter :: system = ampm + 1  !! time string component: time system

      integer(ip), parameter :: nmods = system  !! number of time string components (modifiers included)

      integer(ip) :: begs ( nmods )
      integer(ip) :: ends ( nmods )

      logical :: havera
      logical :: havwdy
      logical :: havzon
      logical :: havapm
      logical :: havsys
!
!     Local Variables.
!
!     The number of known time patterns NKNOWN comes from the include
!     file timepars.inc
!
      integer(ip), parameter :: wdsize = 12  !! length of the representation of a time string

      character(wdsize) :: rep

!
!     Time Zone Variables
!
      integer(ip), parameter :: zsize = 3

      integer(ip), parameter :: osize = 6

      integer(ip), parameter :: nzones = 8

      character(zsize), parameter :: zones ( nzones ) = &
         [ character(zsize) :: 'EST', 'EDT', 'CST', 'CDT', 'MST', 'MDT', 'PST', 'PDT' ]
      character(osize), parameter :: offset( nzones ) = &
         [ character(osize) :: 'UTC-5', 'UTC-4', 'UTC-6', 'UTC-5', 'UTC-7', 'UTC-6', &
           'UTC-8', 'UTC-7' ]
      character(1) :: delim ( 3 )

      integer(ip) :: b
      integer(ip) :: b1
      integer(ip) :: b2
      integer(ip) :: e
      integer(ip) :: e1
      integer(ip) :: e2
      integer(ip) :: from
      integer(ip) :: i
      integer(ip) :: mapto
      integer(ip) :: to
      integer(ip) :: use
      integer(ip) :: r

      logical :: resolv
      logical :: l2r
      logical :: r2l

!
!     Standard SPICE error handling.
!
!     So far there are no modifiers to the time string.
!
      mods   = .false.
      yabbrv = .false.

      do i = 1, nmods
         modify(i) = ' '
      end do

!
!     PEPPER: the built-in representation patterns are the constants
!     TPATS_KNOWN and TPATS_MEANING of this submodule, sorted (SPICELIB
!     loaded and sorted them with ZZTPATS on the first call; PEPPER, on
!     every call, before this change).
!

!
!     First step is to tokenize the string.  The new representation
!     is maintained in ZZTIME.  We'll get it later if we need it.
!
      resolv = zztokns ( ctx, string, error )

      if ( .not. resolv ) then
         succes = .false.
         ntvec  =  0
         type   = ' '
         pictur = ' '
         return
      end if

!
!     The result of tokenizing the string will be a representation
!     that contains the following letters.
!
!           '        The quote character
!           [        The left parenthesis
!           ]        The right parenthesis
!           ,        The comma
!           -        The dash
!           .        The decimal point
!           /        The slash---used to separate date components.
!           :        The colon (used to separate time components)
!           N  ---   stands for one of the symbols A.M. or P.M.
!           O        stands for the symbol UTC+
!           Z  ---   stands for a time zone such as PDT, PSD, CDT, etc.
!           b        stands for a block of white space
!           d        stands for the day of year marker (// or ::)
!           e  ---   stands for the era (B.C. or A.D.)
!           j        stands for julian date
!           m        stands for a month
!           o        stands for the symbol UTC-
!           s  ---   stands for a time system (UTC, TDB, TT, TDT)
!           t        stands the ISO date-T-time separator.
!           w  ---   stands for the day of the week.
!           i        stands for a sequence of digits
!
!     We will gradually remove many of these and replace the i, i.
!     and i.i with the following items
!
!           n       stands for a decimal number
!           Y       stands for a year
!           D       stands for the day in a month
!           y       stands for the day of the year
!           H       stands for hours
!           M       stands for minutes
!           S       stands for seconds
!
!     We will use the following logical functions to modify
!     the tokenized representation:
!
!        ZZTOKNS --- breaks the string down into a list of recognized
!                    tokens and stores an internal model for this
!                    list.  The begins and ends of the substrings
!                    associated with the tokenization are maintained
!                    inside the routine ZZTIME (which ZZTOKNS is an
!                    entry point to).  If some substring cannot be
!                    recognized, ZZTOKNS returns the value FALSE
!                    together with a diagnostic indicating what
!                    was wrong with the input string.
!
!        ZZCMBT  --- combines one or more tokens into a single token.
!                    this is performed only once and is done either
!                    scanning left to right or right to left.
!                    It returns TRUE if a combination is performed.
!
!        ZZREMT  --- removes all instances of a token from the tokenized
!                    representation.  It returns TRUE is an item
!                    is removed.
!
!        ZZSUBT  --- substitutes the first occurrence of a
!                    subpattern (scanning left to right or right to
!                    left) with another pattern of the same length.
!                    This is where we attach new meaning to the
!                    tokenized pattern.  It returns TRUE if a
!                    substitution is performed.
!
!        ZZREPT  --- is a combination of the ZZSUBT and ZZREMT
!                    This performs ZZSUBT on the string, but then
!                    remove all occurrences of the special character
!                    * from the tokenized list. It returns TRUE
!                    is a substitution is performed.
!
!        ZZNOTE  --- returns the begin and end of the first occurrence
!                    of some token, and then removes the token
!                    from the tokenized representation.  We use this
!                    primarily to extract modifiers from the tokenized
!                    string.  These should occur only once and once
!                    removed allow us to more easily attach meaning
!                    to the remaining tokens. The value of ZZNOTE
!                    is true if the requested item could be found,
!                    otherwise it is false and the begin and end
!                    of the requested substring are set to 0.
!
!        ZZIST   --- returns TRUE if the specified token is present
!                    in the tokenized substring.
!
!        ZZISPT  --- returns true is a pair of consecutive tokens
!                    from a list are located in the representation
!                    of the tokenized string.  This is used to
!                    locate consecutive pairs of delimiters in the
!                    input string. It returns TRUE if a pair of
!                    consecutive items is located.  Otherwise
!                    it returns FALSE.
!
!        ZZVALT  --- allows you to substitute a new token for any
!                    integer (i) that lies within a specified range
!                    of values.  This is primarily used to recognize
!                    years in the input string.
!
!        ZZGREP  --- is used to get the current representation of the
!                    tokenized string (with all processing resulting
!                    from use of the manipulation routines taken into
!                    account).
!
!        ZZTPATS --- is used to set up the large list of canned patterns
!                    that are recognized as legitimate tokenizations.
!                    Almost all legitimate time strings when tokenized
!                    will match one of these patterns.
!
!        ZZUNPCK --- uses STRING together with the current
!                    representation of it's tokens to return a
!                    time vector.  If a problem is encountered with
!                    the current tokens, it returns a diagnostic
!                    message that indicates why the string
!                    could not be parsed.  Note ZZUNPCK should be
!                    called only after all string modifiers have
!                    been retrieved via a call to ZZNOTE (or by
!                    manually removing them).
!
!     Next Step is to combine some tokens so that we won't run
!     into problems later on.  We may introduce some new components
!     in the process.
!
      l2r = .true.
      r2l = .not. l2r

      if ( zzcmbt ( ctx, 'Oi', 'z', l2r ) ) then

         resolv = zzcmbt( ctx, 'z:i', 'Z', l2r )
         resolv = zzsubt( ctx, 'z',   'Z', l2r )

      end if

      if ( zzcmbt ( ctx, 'oi', 'z', l2r ) ) then

         resolv = zzcmbt( ctx, 'z:i', 'Z', l2r )
         resolv = zzsubt( ctx, 'z',   'Z', l2r )

      end if

!
!     Next we resolve any months, or weekdays that are followed
!     by periods.
!
      resolv = zzrept ( ctx, 'm.', 'm*', l2r )
      resolv = zzrept ( ctx, 'w.', 'w*', l2r )
      resolv = zzrept ( ctx, 'w,', 'w*', l2r )

!
!     Now convert the right most integer-decimal-point pair to the
!     number representation.
!
      if      ( zzcmbt ( ctx, 'i.i', 'n', r2l ) ) then
!
!        We aren't going to do anything here.  We are simply
!        using the IF-THEN...ELSE IF ... ENDIF  to make sure
!        we only replace one decimal place.
!
      else if ( zzcmbt ( ctx, 'i.', 'n',  r2l  ) ) then
!
!        Same as the previous comment.
!
      end if
!
!     Remove any white space from the tokenization.
!
      resolv = zzremt ( ctx, 'b' )

!
!     User Custom Formats (this still needs a modicum of work).
!     ----------------------------------------------------------------
!     ================================================================
!
!     RESOLV = ZZGREP ( REP )
!     USE    = ISRCHC ( REP, NCUSTM, CUSTOM )
!
!     IF ( USE .GT. 0 ) THEN
!        RESOLV = ZZREPT ( CUSTM(USE), CMEANS(USE), L2R )
!     ELSE
!        RESOLV =  .FALSE.
!     END IF
!
!     IF ( RESOLV ) THEN
!
!        SUCCES = ZZUNPCK ( STRING, YABBRV, ...
!                           TVEC,   NTVEC, TYPE, PICTUR, ERROR )
!        ERROR  = ' '
!
!        RETURN
!     END IF
!

!
!     Julian Date
!     ----------------------------------------------------------------
!     ================================================================
!
      if ( zzist ( ctx, 'j' ) ) then
!
!        This is some form of Julian Date. Handle this case
!        right here and return.
!
         resolv = zzrept ( ctx, '[s]', '*s*', l2r )
         mods   = mods .or. zznote ( ctx, 's', b, e )

         if ( mods ) then
            call ucase ( string(b:e), modify(system) )
         end if

         resolv = zzrept ( ctx, '[j]', '*j*', l2r )
         resolv = zzremt ( ctx, 'j' )

         if ( .not. zzist ( ctx, 'n' ) ) then
            resolv = zzsubt( ctx, 'i', 'n', l2r )
         end if

         resolv = zzcmbt ( ctx, '-n',  'n', l2r )
         resolv = zzsubt ( ctx, 'n',   'J', l2r )

!
!        We let ZZUNPK handle the parsing or diagnosis of any problems.
!
         succes = zzunpck ( ctx, string, yabbrv, &
                            tvec,   ntvec, type, pictur, error )

         if ( index( pictur, 'JULIAND.' ) > 0 ) then
            call suffix ( '::RND', 1, pictur )
         end if

         if ( modify(system) /= ' ' ) then
            call suffix ( '::',           1, pictur  )
            call suffix ( modify(system), 0, pictur  )
         end if

         return

      end if

!
!     Calendar Date Formats.
!     ----------------------------------------------------------------
!     ================================================================
!
!     Replace any integers greater than 1000 by Y.
!
      b      = 1000
      e      = intmax
      resolv = zzvalt ( ctx, string, b, e, 'Y' )

!
!     If the ISO time delimiter 't' is present we don't perform
!     any further simplifications.
!
      if ( zzist ( ctx, 't' ) ) then

         resolv = zzgrep ( ctx, rep )
         use    = bsrchc ( rep, ntpats, tpats_known )

         if ( use /= 0 ) then

            resolv = zzrept  ( ctx, tpats_known(use), tpats_meaning(use), l2r )

            succes = zzunpck ( ctx, string, yabbrv, &
                               tvec,   ntvec, type, pictur, error)

!
!           If you tag an ISO as A.D., other logic in the Time
!           subsystem will not intepret the two digit year value as a
!           two digit abbreviation.
!
!            MODIFY(ERA) = 'A.D.'
!

            if ( index ( pictur, '.#' ) /=0 ) then
               call suffix ( '::RND', 1, pictur )
            end if

            if ( modify(zone) /= ' ' ) then
               call suffix ( '::',         1, pictur )
               call suffix ( modify(zone), 0, pictur )
            end if

            if ( modify(system) /= ' ' ) then
               call suffix ( '::',           1, pictur )
               call suffix ( modify(system), 0, pictur )
            end if

         else

            succes = .false.
            ntvec  =  0
            mods   = .false.
            type   = ' '
            pictur = ' '

            error  = 'The input string uses the ISO  "T" ' &
            //       'date/time delimiter but does not match any ' &
            //       'of the accepted ISO formats. '

         end if

         return

      end if

!
!     If we reach this point, either we didn't have any custom
!     formats supplied or we didn't match any of them.
!     Resolve any abbreviated years.  We've already set integers
!     that are 1000 or greater to 'Y'  Only 1 or 2 digit integers
!     can be year abbreviations.  We replace the 3 digit integers
!     with I temporarily; locate any abbreviated years; reset all
!     the 3-digit back to 'i'.  (Note 3-digit means value between
!     100 and 999.  003 is not regarded as a 3 digit number).
!
      b      = 100
      e      = 1000
      resolv = zzvalt ( ctx, string, b, e, 'I' )
      yabbrv = zzrept ( ctx, '''i', '*Y',  l2r )

      do while ( zzsubt ( ctx, 'I', 'i', l2r ) )
         b = b+1
      end do

!
!     Resolve the system, and other text components.
!
      resolv = zzrept ( ctx, '[e]', '*e*', l2r )
      resolv = zzrept ( ctx, '[w]', '*w*', l2r )
      resolv = zzrept ( ctx, '[N]', '*N*', l2r )
      resolv = zzrept ( ctx, '[Z]', '*Z*', l2r )
      resolv = zzrept ( ctx, '[s]', '*s*', l2r )
      resolv = zzsubt ( ctx, 'ie',  'Ye',  l2r )
!
!     Note the positions of ERA, WEEKDAY, TIME-ZONE, AMPM marker
!     and time SYSTEM.
!

      havera = zznote ( ctx, 'e', begs(era),    ends(era)    )
      havwdy = zznote ( ctx, 'w', begs(wday),   ends(wday)   )
      havzon = zznote ( ctx, 'Z', begs(zone),   ends(zone)   )
      havapm = zznote ( ctx, 'N', begs(ampm),   ends(ampm)   )
      havsys = zznote ( ctx, 's', begs(system), ends(system) )

      mods   = havera .or. havwdy .or. havzon .or. havapm .or. havsys

      if ( mods ) then

         do i = 1, nmods
            if ( begs(i) /= 0 ) then
               call ucase( string(begs(i):ends(i)),modify(i))
            end if
         end do

         if ( havera ) then
            if ( modify(era)(1:1) == 'A' ) then
               modify(era) = 'A.D.'
            else
               modify(era) = 'B.C.'
            end if
         end if

         if ( havapm ) then
            if ( modify(ampm)(1:1) == 'A' ) then
               modify(ampm) = 'A.M.'
            else
               modify(ampm) = 'P.M.'
            end if
         end if

         modify(wday)(4:) = ' '

         if ( havzon ) then

            mapto = findloc(zones, modify(zone), dim=1)

            if ( mapto /= 0 ) then
               modify(zone) = offset(mapto)
            end if

         end if

      end if

!
!     Try our built in formats without any further substitution.
!
      resolv = zzgrep ( ctx, rep )
      use    = bsrchc ( rep, ntpats, tpats_known )

      if ( use > 0 ) then

         resolv = zzrept  ( ctx, tpats_known(use),   tpats_meaning(use), l2r   )
         succes = zzunpck ( ctx, string, yabbrv, &
                            tvec,   ntvec, type, pictur, error )

         if ( index ( pictur, '.#' ) /=0 ) then
            call suffix ( '::RND', 1, pictur )
         end if

         if ( modify(zone) /= ' ' ) then
            call suffix ( '::',         1, pictur )
            call suffix ( modify(zone), 0, pictur )
         end if

         if ( modify(system) /= ' ' ) then
            call suffix ( '::',           1, pictur )
            call suffix ( modify(system), 0, pictur )
         end if

         return

      end if
!
!     Make sure we don't have a pair of successive delimiters
!     or a delimiter at either end of the input string.
!
      if (  zzispt ( ctx, ',/-:d.', from, to ) ) then

         succes = .false.
         ntvec  =  0
         type   = ' '

         error = string
         call zzinssub_inplace ( error, '>', to+1 )
         call zzinssub_inplace ( error, '<', from )
         call prefix ( 'There are two successive delimiters <#> ' &
         //            'in the input string.  This is an ' &
         //            'ambiguous input. '' ', &
                        0, error)
         call repmc_inplace ( error, '#', string(from:to) )
         call suffix ( '''', 0, error )
         pictur = ' '
         return

      end if

!
!     A delimiter hanging at either end of the string shall be
!     regarded as an error.
!
      resolv = zzgrep ( ctx, rep )
      r      = rtrim  ( rep )

      if ( index( ',/-:.', rep(1:1) ) > 0 ) then

         resolv = zzsubt ( ctx, rep(1:1), 'Q', l2r )
         resolv = .false.

      else if ( index( ',/-:.', rep(r:r) ) > 0 ) then

         resolv = zzsubt ( ctx, rep(r:r), 'Q', l2r )
         resolv = .false.

      end if

      if ( .not. resolv ) then

         resolv = zznote ( ctx, 'Q', from, to )
         error  = string
         call zzinssub_inplace ( error, '>', to+1 )
         call zzinssub_inplace ( error, '<', from )
         call prefix ( 'An unexpected delimiter (''#'') was ' &
         //            'encountered in the input string. '' ', &
                             0, error )
         call suffix ( '''', 0, error )
         call repmc_inplace ( error, '#', string(from:to) )
         pictur = ' '
         succes = .false.
         return
      end if
!
!     We probably made it unscathed through the check above.
!     Remove delimiters ',', '/', and '-' and retry the built-in
!     patterns.
!
      delim(1) = ','
      delim(2) = '-'
      delim(3) = '/'

      do i = 1, 3

         resolv = zzremt ( ctx, delim(i) )

         resolv = zzgrep ( ctx, rep )
         use    = bsrchc ( rep, ntpats, tpats_known )

         if ( use > 0 ) then

            resolv = zzrept  ( ctx, tpats_known(use),   tpats_meaning(use), l2r   )
            succes = zzunpck ( ctx, string, yabbrv, &
                               tvec,   ntvec, type, pictur, error )

            if ( index ( pictur, '.#' ) /=0 ) then
               call suffix ( '::RND', 1, pictur )
            end if

            if ( modify(zone) /= ' ' ) then
               call suffix ( '::',         1, pictur )
               call suffix ( modify(zone), 0, pictur )
            end if

            if ( modify(system) /= ' ' ) then
               call suffix ( '::',           1, pictur )
               call suffix ( modify(system), 0, pictur )
            end if

            return

         end if

      end do

!
!     If we make it to this point, we must have a pretty funky
!     time string.  There are some obvious incompatibilities. We
!     check them now
!

      if      ( zznote ( ctx, 'e', b, e ) ) then
      else if ( zznote ( ctx, 's', b, e ) ) then
      else if ( zznote ( ctx, 'Z', b, e ) ) then
      else if ( zznote ( ctx, 'w', b, e ) ) then
      else if ( zznote ( ctx, 'N', b, e ) ) then
      end if
!
!     If B is non-zero the item in question is a duplicate
!     modifier.
!
      if ( b > 0 ) then
         succes = .false.
         ntvec  =  0
         type   = ' '
         error  =  string

         call zzinssub_inplace ( error, '>', e+1 )
         call zzinssub_inplace ( error, '<', b )
         call prefix ( 'The substring "#" is a duplicate ' &
         //            'modifier of the input string: '' ', &
                               0,               error )
         call suffix ( '''',   0,               error )
         call repmc_inplace ( error, '#', string(b:e) )
         pictur = ' '
         return
      end if
!
!     Look for unresolved markers
!
      if      ( zznote ( ctx, '[', b, e ) ) then
      else if ( zznote ( ctx, ']', b, e ) ) then
      else if ( zznote ( ctx, 'O', b, e ) ) then
      else if ( zznote ( ctx, 'o', b, e ) ) then
      else if ( zznote ( ctx, 'z', b, e ) ) then
      end if

      if ( b > 0 ) then

         succes = .false.
         ntvec  =  0
         type   = ' '
         error  =  string

         call zzinssub_inplace ( error, '>', e+1 )
         call zzinssub_inplace ( error, '<', b )
         call prefix ( 'The substring "#" could not ' &
         //            'be resolved in the input string: '' ', &
                               0,               error )
         call suffix ( '''',   0,               error )
         call repmc_inplace ( error, '#', string(b:e) )
         pictur = ' '
         return

      end if

      if ( zzist ( ctx, 'm' ) .and. zzist ( ctx, 'd' ) ) then

         succes = .false.
         ntvec  =  0
         type   = ' '
         error  = string
         resolv = zznote ( ctx, 'm', b1, e1 )
         resolv = zznote ( ctx, 'd', b2, e2 )

         b     = max(b1, b2)
         e     = max(e1, e2)

         call zzinssub_inplace ( error, '>', e+1 )
         call zzinssub_inplace ( error, '<', b )

         b     = min(b1, b2)
         e     = min(e1, e2)

         call zzinssub_inplace ( error, '>', e+1 )
         call zzinssub_inplace ( error, '<', b )

         call prefix ( 'Both a month "#" and day of year ' &
         //            'delimiter "#" appear in the input ' &
         //            'string: '' ', 0, error )
         call suffix ( '''',          0, error )
         call repmc_inplace ( error, '#', string(b1:e1) )
         call repmc_inplace ( error, '#', string(b2:e2) )
         pictur = ' '
         return

      end if

!
!     Make the remaining obvious substitutions for hours,
!     minutes, and seconds
!
      if      ( zzrept ( ctx, 'i:i:i:n', 'D*H*M*S', r2l ) ) then
      else if ( zzrept ( ctx, 'i:i:i:i', 'D*H*M*S', r2l ) ) then
      else if ( zzrept ( ctx, 'i:i:n',   'H*M*S',   r2l ) ) then
      else if ( zzrept ( ctx, 'i:i:i',   'H*M*S',   r2l ) ) then
      else if ( zzrept ( ctx, 'i:n',     'H*M',     r2l ) ) then
      else if ( zzrept ( ctx, 'i:i',     'H*M',     r2l ) ) then
      end if

      resolv = zzremt ( ctx, ':' )
!
!     Handle the obvious substitutions of an integer next to
!     a Month.
!
      if      ( zzsubt ( ctx, '<miiH', 'mDY', l2r ) ) then
      else if ( zzsubt ( ctx, '<mi',   'mD',  l2r ) ) then
      else if ( zzsubt ( ctx, 'Siim>', 'SYDm',l2r ) ) then
      else if ( zzsubt ( ctx, 'im>',   'Dm',  l2r ) ) then
      else if ( zzsubt ( ctx, 'miY>',  'mDY', l2r ) ) then
      else if ( zzsubt ( ctx, 'Ymi',   'YmD', l2r ) ) then
      else if ( zzsubt ( ctx, 'Smi',   'SmD', l2r ) ) then
      else if ( zzsubt ( ctx, 'Mmi',   'MmD', l2r ) ) then
      else if ( zzsubt ( ctx, 'imY',   'DmY', l2r ) ) then
      else if ( zzsubt ( ctx, 'imH',   'DmH', l2r ) ) then
      else if ( zzrept ( ctx, 'Yid',   'Yy*', l2r ) ) then
      else if ( zzrept ( ctx, 'iYd',   'yY*', l2r ) ) then
      else if ( zzrept ( ctx, 'Ydi',   'Y*y', l2r ) ) then
      end if

!
!     That's it we let ZZUNPCK handle the problem of diagnosing
!     or decoding the current representation.
!

      succes = zzunpck ( ctx, string, yabbrv, &
                         tvec,   ntvec, type, pictur, error )

      if ( pictur /= ' ' ) then

         if ( index ( pictur, '.#' ) /=0 ) then
            call suffix ( '::RND', 1, pictur )
         end if

         if ( modify(zone) /= ' ' ) then
            call suffix ( '::',         1, pictur )
            call suffix ( modify(zone), 0, pictur )
         end if

         if ( modify(system) /= ' ' ) then
            call suffix ( '::',           1, pictur )
            call suffix ( modify(system), 0, pictur )
         end if

      end if

      return

      end subroutine tpartv

      !> The function returns TRUE is a combination was performed.
      !>
      !> Combine several token representatives into a single token.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a sequence of tokens to look for in the
      !>                stored internal representation.
      !>
      !>     LETTER     is the replacement token to insert for STRING.
      !>
      !>                If letter is a blank, the combination is simply
      !>                replaced by a blank.
      !>
      !>     L2R        is a logical. If TRUE, the internal representation
      !>                is scanned left to right. If FALSE, the internal
      !>                representation is scanned right to left.
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE if a combination is performed.
      !>     Otherwise it returns FALSE.
      !>
      !>     Note that the most important action of this function is a
      !>     side-effect. The internal representation of a time string
      !>     is modified to reflect the requested token combination.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error Free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This function allows you to alter the internal representation
      !>     of a time string by combining two or more tokens into a single
      !>     token.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzcmbt ( ctx, string, letter, l2r )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: pos, posr, zzrepsub_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! A sequence of tokens to be combined.
      character(1), intent(in) :: letter  !! The replacement token for the combination
      logical, intent(in) :: l2r  !! If TRUE scan left to right, else scan right to left
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: get
      integer(ip) :: next
      integer(ip) :: put
      logical :: did
      integer(ip) :: from
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends, &
                  pbegs => ctx%zztime%pbegs, &
                  pends => ctx%zztime%pends, &
                  to => ctx%zztime%to )
!
!     So far we haven't combined anything.
!
      did = no
!
!     Look for the substring either looking from the
!     left (L2R is YES) or from the right (L2R is NO).
!
      if ( l2r ) then
         from = pos  ( rep(1:size), string, 1    )
      else
         from = posr ( rep(1:size), string, size )
      end if

      to = from + len(string) - 1

      if ( from > 0 ) then

         did         = yes
         ends(from)  = ends(to)
         pends(from) = pends(to)
         put         = from + 1
         next        = to   + 1

!
!        Perform the substitution in the representation
!
         call zzrepsub_inplace ( rep, from, to, letter )
!
!        Now update the begins and ends of tokens in the original
!        string.
!
         do get = next, size

            begs (put) = begs (get)
            ends (put) = ends (get)
            pbegs(put) = pbegs(get)
            pends(put) = pends(get)

            put       = put + 1
         end do

         size = size - len(string) + 1

      end if

      zzcmbt = did
      return
      end associate
      end function zzcmbt

      !> Return the internal representation of the time string.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     STRING     is the current internal tokenized representation of
      !>                the time string that was last supplied to ZZTIME
      !>                via the entry point ZZTOKNS.
      !>
      !>     The function returns TRUE.
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
      !>     This returns the current internal representation of the
      !>     tokenized time string. The function always returns the
      !>     value TRUE.
      !>
      !>## Examples
      !>
      !>     See TPARTV.
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
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzgrep ( ctx, string )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: string  !! The current representation of tokenized time
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size )
      string = rep(1:max(1,size))
      zzgrep = .true.

      return
      end associate
      end function zzgrep

      !> The function returns TRUE if a pair is found.
      !>
      !> Determine if there is a pair of consecutive tokens from
      !> a user specified list of tokens.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a character string that gives a list of tokens
      !>                to search for in a string.
      !>
      !>## Detailed Output
      !>
      !>     B          is the location in the original time string supplied
      !>                to ZZTOKNS of the beginning a pair of consecutive
      !>                tokens from the list specified by STRING.
      !>
      !>     E          is the location in the original time string supplied
      !>                to ZZTOKENS of the end a pair of consecutive
      !>                tokens from the list specified by STRING.
      !>
      !>     The function returns the TRUE is a consecutive pair of tokens
      !>     from STRING is located. Otherwise it returns FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error Free.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine exists primarily to assist in the diagnosis
      !>     of consecutive delimiters in a time string.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzispt ( ctx, string, b, e )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: cpos
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! a list of tokens to search for.
      integer(ip), intent(out) :: b  !! the beginning of the first matching token
      integer(ip), intent(out) :: e  !! the ending of the last matching token.
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      logical :: did
      integer(ip) :: from
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends, &
                  to => ctx%zztime%to )
      did  = no
      from = cpos ( rep, string, 1 )

      do while ( from > 0 )

         if ( from < size ) then
            to   = from + 1
            did  = index( string, rep(to:to) ) > 0
         else
            b       = 0
            e       = 0
            zzispt  = .false.
            return
         end if

         if ( did ) then
            b      = begs(from)
            e      = ends(to)
            zzispt = .true.
            return
         end if

         from = cpos ( rep, string, to )

      end do

      b      =  0
      e      =  0
      zzispt = .false.

      return
      end associate
      end function zzispt

      !> LETTER     I
      !>
      !> The function returns
      !>
      !> Determine if a token is present in the internal representation
      !> of a tokenized time string.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     LETTER     is a token to look for in the tokenized representation
      !>                of a time string.
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE is LETTER is present in the internal
      !>     representation of the last time string passed to ZZTOKNS.
      !>     Otherwise it returns FALSE.
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
      !>     This routine determines whether or not a particular token
      !>     is present in a tokenized representation of a time.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.3.0, 03-SEP-2020 (EDW)
      !>
      !>        Added 'Z' to list of recognized tokens, marked as
      !>        no op.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzist ( ctx, letter )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(1), intent(in) :: letter
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size )
      zzist = index ( rep(1:size), letter ) > 0
      return
      end associate
      end function zzist

      !> The function returns TRUE if the token is located.
      !>
      !> Return the beginning and ending of a token in a time string
      !> and remove the token from the internal representation.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     LETTER     is a token to look for and remove from the
      !>                current tokenization of a time string.
      !>
      !>                If located the token is removed from the string.
      !>
      !>                Note that this simply finds the first matching
      !>                token. If others are present they are not
      !>                affected.
      !>
      !>## Detailed Output
      !>
      !>     B          is the beginning of the requested token if it
      !>                was found. Otherwise B is zero.
      !>
      !>     E          is the ending of the requested token if it was
      !>                found. Otherwise E is zero.
      !>
      !>     The function returns the value TRUE if the token is located.
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
      !>     Look up and remove a token from the internal representation
      !>     of a time string. This is useful in removing modifiers
      !>     from a string (such as the ERA of an epoch, AM/PM of a time
      !>     etc.)
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zznote ( ctx, letter, b, e )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(1), intent(in) :: letter  !! a token to look for in the internal representation
      integer(ip), intent(out) :: b  !! is the beginning of the token
      integer(ip), intent(out) :: e  !! is the end of the token.
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: get
      integer(ip) :: next
      integer(ip) :: put
      logical :: did
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends, &
                  pbegs => ctx%zztime%pbegs, &
                  pends => ctx%zztime%pends )
      put = index ( rep, letter )

      if ( put > 0 ) then
         b = begs(put)
         e = ends(put)

         next = put + 1

         do get = next, size

            begs (put)     = begs (get)
            ends (put)     = ends (get)
            pbegs(put)     = pbegs(get)
            pends(put)     = pends(get)
            rep  (put:put) = rep  (get:get)

            put           = put + 1
         end do

         rep(size:) = ' '
         size       = size - 1
         did        = yes

      else
         b          = 0
         e          = 0
         did        = no
      end if

      zznote = did
      return
      end associate
      end function zznote

      !> The function returns TRUE if any tokens are removed.
      !>
      !> Remove a specified token from the internal representation
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     LETTER     is a token to be removed from the internal
      !>                representation of a tokenized time string.
      !>                All instances of LETTER will be removed from
      !>                the internal representation.
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE if any instance of LETTER is removed
      !>     from the internal representation of a tokenized time string.
      !>     If no instances are removed the function returns FALSE.
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
      !>     This routine is used to remove various delimiters that
      !>     appear in a tokenized time string (although it could be
      !>     used to remove any token from a tokenized time string).
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzremt ( ctx, letter )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(1), intent(in) :: letter  !! token to remove from the internal representation.
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: put
      integer(ip) :: i
      logical :: did
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends, &
                  pbegs => ctx%zztime%pbegs, &
                  pends => ctx%zztime%pends )
      put = 0
      did = no

      do i = 1, size

         if ( ichar(rep(i:i) ) /= ichar(letter) ) then
            put            = put + 1
            rep  (put:put) = rep  (i:i)
            begs (put)     = begs (i)
            ends (put)     = ends (i)
            pbegs(put)     = pbegs(i)
            pends(put)     = pends(i)
         else
            did = yes
         end if

      end do

      size = put

      if ( put == 0 ) then
         rep = ' '
      else if ( put < len(rep) ) then
         rep(put+1:) = ' '
      end if

      zzremt = did
      return
      end associate
      end function zzremt

      !> The function returns TRUE is a substitution is performed.
      !>
      !> Substitute one token for another in the internal representation
      !> of a tokenized time string.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a string of tokens to look for in the internal
      !>                representation of a tokenized time string.
      !>
      !>                Only the first occurrence of STRING will be modified.
      !>
      !>                If the first character in STRING is '<', (and string
      !>                is more than 1 character in length) substitutions
      !>                will be performed in the4 tokenized string only if
      !>                STRING exactly matches the tokenized string
      !>                starting at the left most character.
      !>
      !>                If the last character in STRING is '>' (and string
      !>                is more than 1 character in length) substitutions
      !>                will be performed in the4 tokenized string only if
      !>                STRING exactly matches the tokenized string
      !>                ending at the right most character.
      !>
      !>                If first and last character of STRING are '<' and '>'
      !>                respectively, the first case above is applied and the
      !>                greater than character ('>') is regarded as just
      !>                another character.
      !>
      !>     TRANSL     is a sequence of replacement tokens to substitute
      !>                in place of STRING.
      !>
      !>     L2R        is a logical flag. If L2R is TRUE, the internal
      !>                representation is scanned from left to right. If
      !>                L2R is FALSE, the internal representation is scanned
      !>                from right to left.
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE if a substitution is performed.
      !>     Otherwise it returns FALSE.
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
      !>     This routine searches for the first instance of a specified
      !>     pattern in the internal representation of a tokenized
      !>     time string. If the pattern is found, it is replaced
      !>     by that value of TRANSL. Only one pattern substitution
      !>     is performed per call to this function.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzsubt ( ctx, string, transl, l2r )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: pos, posr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! token pattern to look for.
      character(*), intent(in) :: transl  !! token replacement pattern.
      logical, intent(in) :: l2r  !! direction to scan internal representation.
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: k
      logical :: did
      integer(ip) :: from
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  to => ctx%zztime%to )
!
!     So far we haven't combined anything.
!
      did = no
      k   = len ( string )

!
!     We have two special cases to deal with.
!
      if ( ichar(string(1:1)) == ichar('<') .and. k > 1 ) then

         to   = min( k-1, size )
         from = 1

         if ( string(2:k) == rep(from:to) ) then
            rep(from:to) = transl
            zzsubt       = yes
         else
            zzsubt       = no
         end if

         return

      else if ( ichar(string(k:k)) == ichar('>') .and. k > 1 ) then

         from = max(1,size - k + 2)
         to   = size

         if ( string(1:k-1) == rep(from:to) ) then
            rep(from:to) =  transl
            zzsubt       = yes
         else
            zzsubt       = no
         end if

         return

      end if

!
!     Look for the substring either looking from the
!     left (L2R is YES) or from the right (L2R is NO).
!
      if ( l2r ) then
         from = pos  ( rep, string, 1 )
      else
         from = posr ( rep, string, size )
      end if

      to   = from + len(transl) - 1

      if ( from > 0 ) then
         did          = yes
         rep(from:to) = transl
      end if

      zzsubt = did
      return
      end associate
      end function zzsubt

      !> The function returns TRUE is STRING can be tokenized.
      !>
      !> Construct an internal tokenized representation of STRING.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a string that is intended to represent some
      !>                epoch and that needs parsing.
      !>
      !>## Detailed Output
      !>
      !>     ERROR      is a diagnostic message that is returned if a
      !>                problem occurs while trying to tokenize the
      !>                input time string. If no problems arise, ERROR
      !>                will be returned as a blank.
      !>
      !>     The function returns TRUE if the input string can be successfully
      !>     tokenized. If a problem arises, the function returns FALSE
      !>     and diagnostic is returned in ERROR.
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
      !>     This is the first step in parsing a time string. The
      !>     string is examined for integers, month, weekdays, time systems
      !>     time zones, eras, am/pm and various separators. This
      !>     representation is maintained and manipulated by the
      !>     companion entry points in ZZTIME.
      !>
      !>     The various recognized tokens represented by this routine
      !>     are:
      !>
      !>        '    --- the quote character (year abbreviation)
      !>        ,    --- a comma  (delimiter)
      !>        -    --- a dash   (delimiter)
      !>        .    --- a period (delimiter)
      !>        /    --- a slash  (delimiter)
      !>        :    --- a colon  (delimiter)
      !>        N    --- AM/PM marker
      !>        O    --- UTC+  marker
      !>        Z    --- US Time Zone Marker
      !>        [    --- left parenthesis marker
      !>        ]    --- right parenthesis marker
      !>        b    --- stands for blanks, or tabs (delimiter)
      !>        d    --- day of year marker (delimiter)
      !>        e    --- era marker
      !>        j    --- Julian date system marker
      !>        m    --- month marker
      !>        o    --- UTC- marker
      !>        s    --- time system marker
      !>        t    --- the "T" marker used in ISO formats.
      !>        w    --- the weekday marker
      !>        i    --- unsigned integer marker
      !>
      !>     Using the other entry points in ZZTIME, these markers are
      !>     gradually removed and transformed to more meaningful markers.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>     N.J. Bachman    (JPL)
      !>     B.V. Semenov    (JPL)
      !>     W.L. Taber      (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.6.0, 05-JAN-2014 (NJB)
      !>
      !>        BUG FIX: added checks for token indexes overflowing the
      !>        maximum number of tokens and for the character positions in
      !>        the time picture overflowing the time picture length. Both
      !>        overflows previously resulted in segmentation faults for
      !>        invalid input time strings that contained too many
      !>        recognizable tokens or were too long and required too many
      !>        characters in the picture representation.
      !>
      !>-    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB)
      !>
      !>        Bug fix: changed upper bound used to detect
      !>        non-printing characters from 128 to 126.
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.3.0, 27-OCT-2006 (BVS)
      !>
      !>        Fixed the bug that in the case of a one character long blank
      !>        input time string caused the TO variable be set to the value
      !>        greater than the string length, triggering an OUT OF BOUNDS
      !>        runtime error on HP. Added a separate up-front check for the
      !>        blank input string.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zztokns ( ctx, string, error )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: lx4uns, prefix, rtrim, samchi, samsbi, suffix, ucase, &
         repmc_inplace, repmi_inplace, zzinssub_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! A time string to be tokenized and internalized.
      character(*), intent(out) :: error  !! A diagnostic message
      integer(ip), parameter :: lower = 1  !! case of month and weekday names: lower case
      integer(ip), parameter :: upper = lower + 1  !! case of month and weekday names: upper case
      integer(ip), parameter :: mixed = upper + 1  !! case of month and weekday names: mixed case
      integer(ip), parameter :: full = 1  !! form of month and weekday names: full
      integer(ip), parameter :: short = full + 1  !! form of month and weekday names: abbreviated
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip), parameter :: smwdsz = 12  !! length of a special token
      integer(ip) :: pfrom
      integer(ip) :: pnext
      character(smwdsz) :: spcial
      logical :: got
      integer(ip) :: r
      integer(ip) :: item
      logical :: check
      integer(ip) :: last
      integer(ip) :: next
      integer(ip) :: w
      integer(ip) :: put
      character(1) :: this
      integer(ip) :: nchar
      integer(ip) :: i
      logical :: did
      integer(ip) :: from
      integer(ip) :: kind
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends, &
                  pbegs => ctx%zztime%pbegs, &
                  pends => ctx%zztime%pends, &
                  pictur => ctx%zztime%pictur, &
                  names => ctx%zztime%names, &
                  picerr => ctx%zztime%picerr, &
                  tknerr => ctx%zztime%tknerr, &
                  blank => ctx%zztime%blank, &
                  to => ctx%zztime%to, &
                  pto => ctx%zztime%pto, &
                  case => ctx%zztime%case, &
                  mnsize => ctx%zztime%mnsize, &
                  wksize => ctx%zztime%wksize, &
                  width => ctx%zztime%width, &
                  recog => ctx%zztime%recog, &
                  mnmrk => ctx%zztime%mnmrk, &
                  wkday => ctx%zztime%wkday, &
                  class => ctx%zztime%class, &
                  f => ctx%zztime%f, &
                  l => ctx%zztime%l, &
                  ampm => ctx%zztime%ampm, &
                  first => ctx%zztime%first )
!
!     The first time in this routine we initialize our "tokenizing"
!     table.
!
      zztokns = no

      if ( first ) then

         first = no
         blank = ichar ( ' ' )

!
!        These are the error message templates for errors generated
!        for input time strings that have too many recognizable tokens
!        or are too long for their pictures to fit in the internal
!        picture buffer.
!
         tknerr = 'The input time string ''#'' cannot be processed ' // &
                  'because it contains more than @ recognizable ' // &
                  'tokens. The token that could not be processed ' // &
                  'was ''#''.'
         call repmi_inplace ( tknerr, '@', maxtkn )

         picerr = 'The input time string ''#'' cannot be processed '// &
                  'because the internal picture describing it ' // &
                  'requires more than @ characters. The token that ' // &
                  'could not be processed was ''#''.'
         call repmi_inplace ( picerr, '@', piclen )

!
!        Below is the list of recognized substrings. The basic
!        pattern here is to find the block of special tokens
!        that begin with a particular character. Insert into
!        that block the lines of code below
!
!        I              =  I + 1
!        F( ICHAR('letter')) =  I
!        RECOG(I)       = 'the full substring that's recognized '
!        WIDTH(I)       =  number of characters required to match
!        CLASS(I)       = 'the classification of this substring'
!        L( ICHAR('b')) =  I
!
!        Note matching is performed from the first string in the
!        group to the last.
!

         do i = 32, 126
            f(i)     =  0
            l(i)     = -1
            names(i) = 'substring'
         end do

         names( ichar('''') ) = '"Year Abbreviation Mark"'
         names( ichar(',' ) ) = 'comma'
         names( ichar('-' ) ) = 'dash'
         names( ichar('.' ) ) = 'period'
         names( ichar('/' ) ) = 'slash'
         names( ichar(':' ) ) = 'colon'
         names( ichar('D' ) ) = 'Day of Month'
         names( ichar('H' ) ) = 'Hour'
         names( ichar('M' ) ) = 'Minute'
         names( ichar('N' ) ) = 'AM/PM indicator'
         names( ichar('O' ) ) = 'UTC-Offset indicator'
         names( ichar('S' ) ) = 'Second'
         names( ichar('Y' ) ) = 'Year'
         names( ichar('Z' ) ) = 'Time-Zone indicator'
         names( ichar('[' ) ) = 'Left Parenthesis'
         names( ichar(']' ) ) = 'Right Parenthesis'
         names( ichar('b' ) ) = 'White Space'
         names( ichar('d' ) ) = 'Day-of-Year indicator'
         names( ichar('e' ) ) = 'Era'
         names( ichar('i' ) ) = 'Integer'
         names( ichar('j' ) ) = 'Julian Date indicator'
         names( ichar('m' ) ) = 'Month'
         names( ichar('n' ) ) = 'Decimal Number'
         names( ichar('o' ) ) = 'UTC-Offset indicator'
         names( ichar('s' ) ) = 'Time System specification'
         names( ichar('t' ) ) = 'ISO Time Separator'
         names( ichar('w' ) ) = 'Weekday'
         names( ichar('y' ) ) = 'Day of Year'

         mnmrk ( lower, full  ) = 'month'
         mnmrk ( upper, full  ) = 'MONTH'
         mnmrk ( mixed, full  ) = 'Month'
         mnmrk ( lower, short ) = 'mon'
         mnmrk ( upper, short ) = 'MON'
         mnmrk ( mixed, short ) = 'Mon'

         wkday ( lower, full  ) = 'weekday'
         wkday ( upper, full  ) = 'WEEKDAY'
         wkday ( mixed, full  ) = 'Weekday'
         wkday ( lower, short ) = 'wkd'
         wkday ( upper, short ) = 'WKD'
         wkday ( mixed, short ) = 'Wkd'
!
!        Length of the items Month, Mon, weekday, wkd
!
         wksize ( full  ) = 7
         wksize ( short ) = 3
         mnsize ( full  ) = 5
         mnsize ( short ) = 3

         i = 0
!
!        Tokens beginning with ' '
!
         i              =  i + 1
         f( ichar(' ')) =  i
         recog(i)       = ' '
         width(i)       =  1
         class(i)       = 'b'
         l( ichar(' ')) =  i

!
!        Tokens beginning with '('
!
         i              =  i + 1
         f( ichar('(')) =  i
         recog(i)       = '('
         width(i)       =  1
         class(i)       = '['
         l( ichar('(')) =  i
!
!        Tokens beginning with ')'
!
         i              =  i + 1
         f( ichar(')')) =  i
         recog(i)       = ')'
         width(i)       =  1
         class(i)       = ']'
         l( ichar(')')) =  i
!
!        Tokens beginning with ','
!
         i              =  i + 1
         f( ichar(',')) =  i
         recog(i)       = ','
         width(i)       =  1
         class(i)       = ','
         l( ichar(',')) =  i

!
!        Tokens beginning with '-'
!
         i              =  i + 1
         f( ichar('-')) =  i
         recog(i)       = '-'
         width(i)       =  1
         class(i)       = '-'
         l( ichar('-')) =  i

!
!        Tokens beginning with '.'
!
         i              =  i + 1
         f( ichar('.')) =  i
         recog(i)       = '.'
         width(i)       =  1
         class(i)       = '.'
         l( ichar('.')) =  i

!
!        Tokens beginning with '/'
!
         i              =  i + 1
         f( ichar('/')) =  i
         recog(i)       = '//'
         width(i)       =  2
         class(i)       = 'd'
         l( ichar('/')) =  i

         i              =  i + 1
         recog(i)       = '/'
         width(i)       =  1
         class(i)       = '/'
         l( ichar('/')) =  i
!
!        Tokens beginning with ':'
!
         i              =  i + 1
         f( ichar(':')) =  i
         recog(i)       = '::'
         width(i)       =  2
         class(i)       = 'd'
         l( ichar(':')) =  i

         i              =  i + 1
         recog(i)       = ':'
         width(i)       =  1
         class(i)       = ':'
         l( ichar(':')) =  i

!
!        Tokens beginning with 'A'
!
         i              =  i + 1
         f( ichar('A')) =  i
         recog(i)       = 'A.D.'
         width(i)       =  4
         class(i)       = 'e'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'AD'
         width(i)       =  2
         class(i)       = 'e'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'A.M.'
         width(i)       =  4
         class(i)       = 'N'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'AM'
         width(i)       =  2
         class(i)       = 'N'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'APRIL'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'AUGUST'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('A')) =  i

!
!        Tokens beginning with 'B'
!
         i              =  i + 1
         f( ichar('B')) =  i
         recog(i)       = 'B.C.'
         width(i)       =  4
         class(i)       = 'e'
         l( ichar('B')) =  i

         i              =  i + 1
         recog(i)       = 'BC'
         width(i)       =  2
         class(i)       = 'e'
         l( ichar('B')) =  i

!
!        Tokens beginning with 'C'
!
         i              =  i + 1
         f( ichar('C')) =  i
         recog(i)       = 'CDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('C')) =  i

         i              =  i + 1
         recog(i)       = 'CST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('C')) =  i

!
!        Tokens beginning with 'D'
!
         i              =  i + 1
         f( ichar('D')) =  i
         recog(i)       = 'DECEMBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('D')) =  i

         i              =  i + 1
         recog(i)       = 'D+'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('D')) =  i

         i              =  i + 1
         recog(i)       = 'D-'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('D')) =  i

         i              =  i + 1
         recog(i)       = 'D'
         width(i)       =  1
         class(i)       = 'E'
         l( ichar('D')) =  i

!
!        Tokens beginning with 'E'
!
         i              =  i + 1
         f( ichar('E')) =  i
         recog(i)       = 'EDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'EST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'E+'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'E-'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'E'
         width(i)       =  1
         class(i)       = 'E'
         l( ichar('E')) =  i

!
!        Tokens beginning with 'F'
!
         i              =  i + 1
         f( ichar('F')) =  i
         recog(i)       = 'FEBRUARY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('F')) =  i

         i              =  i + 1
         recog(i)       = 'FRIDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('F')) =  i

!
!        Tokens beginning with 'J'
!
         i              =  i + 1
         f( ichar('J')) =  i
         recog(i)       = 'JANUARY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('J')) =  i

         i              =  i + 1
         recog(i)       = 'JD'
         width(i)       =  2
         class(i)       = 'j'
         l( ichar('J')) =  i

         i              =  i + 1
         recog(i)       = 'JULY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('J')) =  i

         i              =  i + 1
         recog(i)       = 'JUNE'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('J')) =  i

!
!        Tokens beginning with 'M'
!
         i              =  i + 1
         f( ichar('M')) =  i
         recog(i)       = 'MARCH'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MAY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MONDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('M')) =  i

!
!        Tokens beginning with 'N'
!
         i              =  i + 1
         f( ichar('N')) =  i
         recog(i)       = 'NOVEMBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('N')) =  i

!
!        Tokens beginning with 'O'
!
         i              =  i + 1
         f( ichar('O')) =  i
         recog(i)       = 'OCTOBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('O')) =  i

!
!        Tokens beginning with 'P'
!
         i              =  i + 1
         f( ichar('P')) =  i
         recog(i)       = 'P.M.'
         width(i)       =  4
         class(i)       = 'N'
         l( ichar('P')) =  i

         i              =  i + 1
         recog(i)       = 'PDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('P')) =  i

         i              =  i + 1
         recog(i)       = 'PM'
         width(i)       =  2
         class(i)       = 'N'
         l( ichar('P')) =  i

         i              =  i + 1
         recog(i)       = 'PST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('P')) =  i

!
!        Tokens beginning with 'S'
!
         i              =  i + 1
         f( ichar('S')) =  i
         recog(i)       = 'SATURDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('S')) =  i

         i              =  i + 1
         recog(i)       = 'SEPTEMBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('S')) =  i

         i              =  i + 1
         recog(i)       = 'SUNDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('S')) =  i

!
!        Tokens beginning with 'T'
!
         i              =  i + 1
         f( ichar('T')) =  i
         recog(i)       = 'TDB'
         width(i)       =  3
         class(i)       = 's'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'TDT'
         width(i)       =  3
         class(i)       = 's'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'THURSDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'TUESDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'T'
         width(i)       =  1
         class(i)       = 't'
         l( ichar('T')) =  i

!
!        Tokens beginning with 'U'
!
         i              =  i + 1
         f( ichar('U')) =  i
         recog(i)       = 'UTC+'
         width(i)       =  4
         class(i)       = 'O'
         l( ichar('U')) =  i

         i              =  i + 1
         recog(i)       = 'UTC-'
         width(i)       =  4
         class(i)       = 'o'
         l( ichar('U')) =  i

         i              =  i + 1
         recog(i)       = 'UTC'
         width(i)       =  3
         class(i)       = 's'
         l( ichar('U')) =  i
!
!        Tokens beginning with ''''
!
         i              =  i + 1
         f( ichar('''')) =  i
         recog(i)       = ''''
         width(i)       =  1
         class(i)       = ''''
         l( ichar('''')) =  i
!
!        Tokens beginning with 'W'
!
         i              =  i + 1
         f( ichar('W')) =  i
         recog(i)       = 'WEDNESDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('W')) =  i
!
!        Tokens beginning with 'Z'
!
         i              =  i + 1
         f( ichar('Z')) =  i
         recog(i)       = 'Z'
         width(i)       =  1
         class(i)       = 'x'
         l( ichar('Z')) =  i

      end if

!
!     If the input string is blank, return with an error message.
!
      if ( string == ' ' ) then
         error = 'The input time string is blank.'
         zztokns = no
         return
      end if

!
!     OK. Initializations are out of the way. We now take
!     apart the string.
!
      did    = no
      error  = ' '
      rep    = ' '
      pictur = ' '
      size   = 0
      next   = 1
      pnext  = 1
      put    = 0
      ampm   = .false.
      last   = rtrim ( string )

      do while ( next <= last )
!
!        FROM and NEXT point to parts of the string, PFROM and PNEXT
!        point to parts of the picture we will construct.
!
         from  = next
         pfrom = pnext
         item  = ichar( string(next:next) )
!
!        First we try to find an unsigned integer in the string.
!
         call lx4uns ( string(1:last), from, to, nchar )

         if ( nchar > 0 ) then
!
!           We found an unsigned integer, add a letter to the
!           internal representation, note the begin and end
!           of the token and set NEXT to the first character
!           beyond this token.
!
            put               =  put + 1

            if ( put > maxtkn ) then
               call zztknerr ( ctx, tknerr, string, string(from:to), &
                               error, zztokns )
               return
            end if

            rep (put:put)     = 'i'
            begs(put)         =  from
            ends(put)         =  to
            next              =  to    + 1
            pto               =  pfrom + nchar - 1

            if ( pto > piclen ) then
               call zztknerr ( ctx, picerr, string, string(from:to), &
                               error, zztokns )
               return
            end if

            pnext             =  pto   + 1
            pictur(pfrom:pto) =  string(from:to)
            pbegs(put)        =  pfrom
            pends(put)        =  pto

         else if ( item == blank  ) then
!
!           We have a blank. We lump all consecutive
!           blanks together as one big fat blank.
!
            put           =  put + 1

            if ( put > maxtkn ) then
               call zztknerr ( ctx, tknerr, string, ' ', error, zztokns )
               return
            end if

            to            =  from
            begs(put)     =  from
            rep (put:put) = 'b'

            do while ( item == blank .and. to <= last )

               to   = to + 1
               if ( to <= last ) then
                  item = ichar( string(to:to) )
               end if

            end do

            next              = to
            to                = to - 1
            ends(put)         = to

            pto               = pfrom + to - from

            if ( pto > piclen ) then
               call zztknerr ( ctx, picerr, string, ' ', error, zztokns )
               return
            end if

            pnext             = pto   + 1
            pictur(pfrom:pto) = string(from:to)
            pbegs (put)       = pfrom
            pends (put)       = pto

         else if ( item == 9 ) then
!
!           We've got a tab character, we treat tabs as
!           blanks.
!
            put               = put + 1

            if ( put > maxtkn ) then
               call zztknerr ( ctx, tknerr, string, '<TAB>', &
                               error, zztokns )
               return
            end if

            rep(put:put)      = 'b'
            begs(put)         = from
            ends(put)         = from
            next              = next + 1

            pto               = pfrom

            if ( pto > piclen ) then
               call zztknerr ( ctx, picerr, string, '<TAB>', &
                               error, zztokns )
               return
            end if

            pnext             = pto   + 1
            pictur(pfrom:pto) = ' '
            pbegs (put)       = pfrom
            pends (put)       = pfrom

         else if (      item < 32 &
                   .or. item > 126 ) then
!
!           This is a non-printing character. This is
!           regarded as an error.
!
            error = string

            call zzinssub_inplace ( error, '<', next )

!
!           Overwrite the non-printing character with a
!           closing angle bracket.
!
            if ( next < len(error) ) then
               error( next+1 : next+1 ) = '>'
            end if

            call prefix ( 'There is a non-printing, non-tab ' &
            //            'character (ASCII #) at position # of the ' &
            //            'time string: ', 1, error )

            call repmi_inplace ( error, '#', item )
            call repmi_inplace ( error, '#', next )

            zztokns = no
            return

         else
!
!           This has to be one of the known types or we
!           have an unknown component in the string. We've constructed
!           a "parsing" table for handling these special cases.
!           This table uses the first letter of the string
!           to begin a search. We get that code and force it
!           into a suitable range.
!
            call     ucase ( string(next:next), this )
            item  =  ichar ( this )
            from  =  next
            check =  yes
            i     =  f(item)

            do while ( check .and. (i <= l(item)) )

               w    = width(i)
               to   = from + w - 1

               got  = samsbi( string, from, to, recog(i), 1, w )

               if ( got ) then
!
!                 We have a match. If it is the match of a month
!                 or day of the week, we keep looking for the
!                 end of the match.
!
                  if (      class(i) == 'm' &
                       .or. class(i) == 'w' ) then

                     spcial = recog(i)
                     r      = rtrim(spcial)
                     w      = w+1
                     to     = to + 1

                     do while ( samchi ( string, to, spcial(1:r), w ) )
                        w  = w  + 1
                        to = to + 1
                     end do

                     to        = to - 1

                     if ( w > r ) then
                        kind = full
                     else
                        kind = short
                     end if

                     if ( this /= string(next:next) ) then
                        case = lower
                     else if (string(next:next+2) == spcial(1:3)) &
                     then
                        case = upper
                     else
                        case = mixed
                     end if

                     if ( class(i) == 'm' ) then

                        pto               = pfrom + mnsize(kind) - 1

                        if ( pto > piclen ) then
                           call zztknerr ( ctx, picerr, string, &
                                    string(from:to), error, zztokns )
                           return
                        end if

                        pnext             = pto   + 1
                        pictur(pfrom:pto) = mnmrk(case,kind)

                     else

                        pto               = pfrom + wksize(kind) - 1

                        if ( pto > piclen ) then
                           call zztknerr ( ctx, picerr, string, &
                                    string(from:to), error, zztokns )
                           return
                        end if

                        pnext             = pto   + 1
                        pictur(pfrom:pto) = wkday(case,kind)

                     end if

                  else if ( class(i) == 'e' ) then

                     pto               =  pfrom + 2

                     if ( pto > piclen ) then
                        call zztknerr ( ctx, picerr, string, string(from:to), &
                                                        error, zztokns )
                        return
                     end if

                     pnext             =  pto   + 1

                     if ( string(from:from) == this ) then
                        pictur(pfrom:pto) = 'ERA'
                     else
                        pictur(pfrom:pto) = 'era'
                     end if

                  else if ( class(i) == 'N' ) then

                     pto               =  pfrom + 3

                     if ( pto > piclen ) then
                        call zztknerr ( ctx, picerr, string, string(from:to), &
                                                        error, zztokns )
                        return
                     end if

                     pnext             =  pto   + 1
                     if ( string(from:from) == this ) then
                        pictur(pfrom:pto) = 'AMPM'
                     else
                        pictur(pfrom:pto) = 'ampm'
                     end if

                     ampm = .true.

                  else if ( class(i) == 'x' ) then

!
!                    Recognized token, but ignored.
!

                  else

                     pto               =  pfrom + to - from

                     if ( pto > piclen ) then
                        call zztknerr ( ctx, picerr, string, string(from:to), &
                                                        error, zztokns )
                        return
                     end if

                     pnext             =  pto   + 1
                     pictur(pfrom:pto) =  string(from:to )

                  end if

                  put           = put + 1

                  if ( put > maxtkn ) then
                     call zztknerr ( ctx, tknerr, string, string(from:to), &
                                     error, zztokns )
                     return
                  end if

                  rep (put:put) = class(i)(1:1)
                  begs(put)     = from
                  ends(put)     = to
                  pbegs(put)    = pfrom
                  pends(put)    = pto
                  check         = no
                  next          = to + 1

               end if

               i = i + 1

            end do
!
!           If we reach the end of the loop and CHECK is still
!           set to TRUE, we have a bit of unrecognizable string.
!
            if ( check ) then

               error = string

               call zzinssub_inplace ( error, '>', from + 1 )
               call zzinssub_inplace ( error, '<', from )

               call prefix ( 'The input string contains an ' &
               //            'unrecognizable substring beginning ' &
               //            'at the character marked by <#>: "', &
                                  0, error )
               call suffix ( '"', 0, error )

               call repmc_inplace ( error, '#', string(from:from) )
               zztokns = no
               return

            end if

         end if

      end do

      size    = put
      zztokns = yes
      return
      end associate
      end function zztokns

      !> The function returns TRUE if the string was unpacked completely.
      !>
      !> Unpack the time string and parse its components using the
      !> stored internal representation.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is the original string from which the current
      !>                internal representation was derived.
      !>
      !>     YABBRV     is a logical that indicates whether or not an
      !>                abbreviated year was encountered in the string.
      !>                YABBRV is TRUE if such an abbreviation was present
      !>                otherwise it is FALSE.
      !>
      !>## Detailed Output
      !>
      !>     TVEC       is a double precision array of the parsed time
      !>                components. TVEC will have either 5 or 6 values
      !>                depending upon whether the string is Year, Month,
      !>                and Day of Month, or Year and Day of Year.
      !>
      !>     E          is the actual number of components that were
      !>                present in the internal representation.
      !>
      !>                If STRING cannot be fully resolved, E is returned
      !>                as a zero.
      !>
      !>     TRANSL     is the type of time vector. The value will be
      !>                'YD' (day of year) or 'YMD' (Year, Month, Day).
      !>
      !>                If STRING cannot be fully resolved, TRANSL is
      !>                returned as a blank.
      !>
      !>     PIC        is a picture of the time format corresponding the
      !>                the time string in the last call to ZZTOKNS.
      !>
      !>                If some part of the input string can't be identified
      !>                PIC is returned as a blank. Note that there is a
      !>                distinction between recognizable and parsable.
      !>                The input string must be unambiguous to be parsable,
      !>                However, even if a string is ambiguous it may
      !>                correspond to a legitimate format picture. Since
      !>                occasionally, that's what you want (an ambiguous
      !>                format), we allow it in PIC.
      !>
      !>     ERROR      is a diagnostic that indicates some problem in
      !>                resolving STRING. If no problems occur ERROR
      !>                is returned as a blank.
      !>
      !>     The function returns TRUE if STRING was successfully unpacked.
      !>     That is the string is parsed and is unambiguously recognized.
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
      !>     This routine is the last routine that will normally be
      !>     called by a time parsing routine. This call should be
      !>     made after all combinations, replacements and removals
      !>     that make sense to perform have been made.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.3.0, 29-APR-2013 (EDW)
      !>
      !>        Added error check on ITEM value. Failure to perform
      !>        this check can cause BADSUBSCRIPT error signals
      !>        from CSPICE code on invalid time strings.
      !>
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzunpck ( ctx, string, yabbrv, tvec, e, transl, pic, error )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: nparsd, pos, prefix, suffix, ucase, repmc_inplace, repmi_inplace, &
         zzinssub_inplace, zzrepsub_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! is a time string that has been tokenized.
      character(*), intent(out) :: transl  !! is the type TVEC ( YMD or YD )
      character(*), intent(out) :: error  !! a diagnostic of any problems
      character(*), intent(out) :: pic  !! is a picture of the format used for the time string
      real(wp), intent(out) :: tvec  ( * )  !! is a vector of time components
      integer(ip), intent(out) :: e  !! is the actual number of components present
      logical, intent(in) :: yabbrv  !! has the year been abbreviated.
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: nsec
      integer(ip) :: p2
      integer(ip) :: nhour
      character(piclen) :: messge
      integer(ip) :: p1
      integer(ip) :: item
      integer(ip) :: nmon
      integer(ip) :: ndoy
      integer(ip) :: nmin
      integer(ip) :: nyear
      integer(ip) :: nday
      integer(ip) :: njd
      integer(ip) :: ptr
      character(3) :: month
      integer(ip) :: value
      integer(ip) :: k
      integer(ip) :: i
      integer(ip) :: j
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends, &
                  pbegs => ctx%zztime%pbegs, &
                  pends => ctx%zztime%pends, &
                  pictur => ctx%zztime%pictur, &
                  names => ctx%zztime%names, &
                  hms => ctx%zztime%hms, &
                  ampm => ctx%zztime%ampm )
      nyear  = 0
      nmon   = 0
      nday   = 0
      nhour  = 0
      nmin   = 0
      nsec   = 0
      ndoy   = 0
      njd    = 0
      e      = 0
      transl = ' '

      hms(1) = 0.0_wp
      hms(2) = 0.0_wp
      hms(3) = 0.0_wp

      do i = size, 1, -1

         item = ichar( rep(i:i) )

!
!        Confirm ITEM range [32,126].
!
         if (      item < 32 &
              .or. item > 126 ) then

!
!           A non-printing character found in REP. This is
!           an error.
!
            error = 'A character at location #1 does not ' &
         //         'have ASCII value [32,126] for REP string.'

            call repmi_inplace ( error, '#1', i )

!
!           Error condition, return.
!
            zzunpck = no
            return

         end if

         j    = begs(i)
         k    = ends(i)

         if      ( item == ichar('Y') ) then

            nyear = nyear + 1
            e     = e     + 1
            call nparsd   ( string(j:k), tvec(1), error, ptr )

            if ( yabbrv ) then
               call zzrepsub_inplace ( pictur, pbegs(i), pends(i), &
                              'YR' )
            else
               call zzrepsub_inplace ( pictur, pbegs(i), pends(i), &
                              'YYYY' )
            end if

         else if ( item == ichar('m') ) then

            nmon  = nmon  + 1
            e     = e     + 1

            call ucase ( string(j:k), month )
            value = findloc(months, month, dim=1)

            if ( value == 0 ) then
               call nparsd  ( string(j:k), tvec(2), error, ptr )
               call zzrepsub_inplace ( pictur, pbegs(i), pends(i), 'MM' )
            else
               tvec(2) = real( value, wp )
            end if

         else if ( item == ichar('D') ) then

            nday  = nday  + 1
            e     = e     + 1

            call nparsd ( string(j:k), tvec(3), error, ptr  )

            call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'DD', &
                          string(j:k) )

         else if ( item == ichar('y') ) then

            ndoy  = ndoy  + 1
            e     = e     + 1

            call nparsd ( string(j:k), tvec(2), error, ptr  )

            call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'DOY', &
                          string(j:k) )

         else if ( item == ichar('H') ) then

            nhour = nhour + 1
            e     = e     + 1

            call nparsd ( string(j:k), hms(1), error, ptr  )
!
!           We have to handle the hour component based on the
!           presence of the AM/PM mark in the picture. We earlier
!           set up the logical AMPM to indicate its presence.
!
            if (  ampm ) then

               call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'AP', &
                             string(j:k) )

            else

               call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'HR', &
                             string(j:k) )

            end if

         else if ( item == ichar('M') ) then

            nmin  = nmin  + 1
            e     = e     + 1

            call nparsd ( string(j:k), hms(2), error, ptr  )

            call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'MN', &
                          string(j:k) )

         else if ( item == ichar('S') ) then

            nsec  = nsec  + 1
            e     = e     + 1

            call nparsd ( string(j:k), hms(3), error, ptr  )

            call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'SC', &
                          string(j:k) )

         else if ( item == ichar('J') ) then

            njd   = njd   + 1
            e     = e     + 1

            call nparsd ( string(j:k), tvec(1), error, ptr  )

            call zzmkpc ( ctx, pictur, pbegs(i), pends(i), 'JULIAND', &
                          string(j:k) )

         else if ( item == ichar('i') ) then

            error = string

            call zzinssub_inplace ( error, '>', k+1 )
            call zzinssub_inplace ( error, '<', j )

            call prefix ( 'The meaning of the integer <#> could ' &
            //            'not be determined: ''', 1, error )
            call suffix ( '''', 0, error )

            call repmc_inplace ( error, '#', string(j:k) )

            e       = 0
            pic     = ' '
            zzunpck = no
            return

         else if ( item == ichar('n') ) then

            error = string

            call zzinssub_inplace ( error, '>', k+1 )
            call zzinssub_inplace ( error, '<',  j )

            call prefix ( 'The meaning of the decimal number <#> ' &
            //            'could not be determined: ', 1, error )

            call repmc_inplace ( error, '#', string(j:k) )

            e       = 0
            pic     = ' '
            zzunpck = no
            return

         else

            error = string

            call zzinssub_inplace ( error, '>', k+1 )
            call zzinssub_inplace ( error, '<',  j )

            call prefix ( 'An unexpected # ("#") was encountered ' &
            //            'in the time string: ', 1, error )
            call repmc_inplace ( error, '#', names(item) )
            call repmc_inplace ( error, '#', string(j:k) )

            pic     = ' '
            e       = 0
            zzunpck = no
            return

         end if

      end do

!
!     Ok. Check the counts of substrings to make sure everything
!     looks ok. If so move the HMS into the appropriate slots
!     in TVEC, set the kind of TVEC, set the function value to YES,
!     and RETURN. Note regardless of the correctness of the parsing
!     we have a legitimate format picture at this point so we keep it.
!
      pic = pictur

      if (       nyear == 1 &
           .and. nmon  == 1 &
           .and. nday  == 1 &
           .and. ndoy  == 0 &
           .and. njd   == 0 &
           .and. nhour <= 1 &
           .and. nmin  <= nhour &
           .and. nsec  <= nmin ) then

          tvec(4) = hms(1)
          tvec(5) = hms(2)
          tvec(6) = hms(3)

          transl  = 'YMD'
          zzunpck =  yes
          return

      else if (      nyear == 1 &
               .and. nmon  == 0 &
               .and. nday  == 0 &
               .and. njd   == 0 &
               .and. ndoy  == 1 &
               .and. nhour <= 1 &
               .and. nmin  <= nhour &
               .and. nsec  <= nmin  ) then

          tvec(3) = hms(1)
          tvec(4) = hms(2)
          tvec(5) = hms(3)

          transl  = 'YD'
          zzunpck =  yes
          return

      else if (      nyear == 0 &
               .and. nmon  == 0 &
               .and. nday  == 0 &
               .and. njd   == 1 &
               .and. ndoy  == 0 &
               .and. nhour <= 0 &
               .and. nmin  <= 0 &
               .and. nsec  <= 0  ) then

          transl  = 'JD'
          zzunpck =  yes
          return

      end if
!
!     If we're still here, there is some kind of an error
!     in the input string. There are a lot of possible
!     problems.
!
      e = 0

      if (           nyear == 0 &
               .and. nday  == 0 &
               .and. njd   == 0 &
               .and. ndoy  == 0 &
               .and. nhour == 0 &
               .and. nmin  == 0 &
               .and. nsec  == 0  ) then

         error = 'No numeric components were supplied in the ' &
         //      'time string. '

      else if ( njd == 1 ) then

         error = 'The string possesses calendar components in ' &
         //      'addition to Julian Date specifier. '

      else if ( njd > 1 ) then

         error = 'There is more than one Julian Date specified ' &
         //      'in the epoch string. '

      else if      ( nyear == 0 ) then

         error = 'The year associated with the calendar string ' &
         //      '"#" could not be identified. '

         call repmc_inplace ( error, '#', string )

      else if ( nyear > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a calendar year ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'Y', 1    )
         p2 = pos ( rep, 'Y', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nmon > 0 .and. ndoy > 0 ) then

         error  =  string
         messge = 'Both a day of year and month were identified ' &
         //       'in the input string. "'

         p2 = max ( pos ( rep, 'm', 1 ), &
                    pos ( rep, 'y', 1 ) )

         p1 = min ( pos ( rep, 'm', 1 ), &
                    pos ( rep, 'y', 1 ) )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nmon > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a calendar month ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'm', 1    )
         p2 = pos ( rep, 'm', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( ndoy > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a day of year ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'y', 1    )
         p2 = pos ( rep, 'y', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nday > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a day of month ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'D', 1    )
         p2 = pos ( rep, 'D', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nhour > 1 ) then

         error  =  string
         messge = 'Two substrings representing an hour of the day ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'H', 1    )
         p2 = pos ( rep, 'H', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nmin  > 1 ) then

         error  =  string
         messge = 'Two substrings representing minutes of the hour ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'M', 1    )
         p2 = pos ( rep, 'M', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nsec  > 1 ) then

         error  =  string
         messge = 'Two substrings representing seconds ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'

         p1 = pos ( rep, 'S', 1    )
         p2 = pos ( rep, 'S', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         j = begs(p1)
         k = ends(p1)

         call zzinssub_inplace ( error, '>', k+1 )
         call zzinssub_inplace ( error, '<', j )
         call repmc_inplace ( messge, '#', string(j:k) )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( ndoy == 0 .and. nmon == 0 ) then

         error = 'Neither a month nor day of year could be ' &
         //      'identified in the input time string: "#" '

         call repmc_inplace ( error, '#', string )

      else if ( nmon == 1 .and. nday == 0 ) then

         error = 'A month was identified in the time string "#", ' &
         //      'but a day of month could not be identified. '

         call repmc_inplace ( error, '#', string )

      else if ( nmon == 0 .and. nday == 1 ) then

         error = 'A day of month was identified in the time ' &
         //      'string "#", but the month it belongs to could ' &
         //      'not be identified. '

         call repmc_inplace ( error, '#', string )

      else if ( nmin  > nhour ) then

         error = 'A minutes components of the time  was ' &
         //      'identified in the time string "#", but the ' &
         //      'hours component could not be identified. '

         call repmc_inplace ( error, '#', string )

      else if ( nsec  > nmin  ) then

         error = 'A seconds components of the time was ' &
         //      'identified in the time string "#", but the ' &
         //      'minutes component could not be identified. '

         call repmc_inplace ( error, '#', string )

      end if

      zzunpck = no
      return
      end associate
      end function zzunpck

      !> The function returns TRUE if any substitutions are performed.
      !>
      !> Examine the value of an integer token and if it is within the
      !> range from B to E replace the token with the new token
      !> specified by LETTER.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is an original time string as last supplied to ZZTOKNS.
      !>
      !>     B          is the lower bound of some test range of integers
      !>
      !>     E          is the upper bound of some test range of integers
      !>
      !>     LETTER     is the new token value to put in place of 'i' if
      !>                the value of the integer is between B and E
      !>                (inclusive).
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE if any substitutions are performed..
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
      !>     This function replaces every occurrence of 'i' in the internal
      !>     representation by the value LETTER if the numerical value
      !>     of the token corresponding to 'i' is between B and E.
      !>
      !>     This is used primarily to identify YEAR tokens in a time
      !>     string.
      !>
      !>## Examples
      !>
      !>     See TPARTV
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
      !>-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
      !>
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
      !>
      !>        The main routine (which should never be called) now returns
      !>        the value .FALSE.
      !>
      !>-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
      !>
      !>        Added a RETURN statement at the end of the main routine.
      !>        Enhanced error message for the case when the input string
      !>        to ZZTOKNS has a non-printing character.
      !>
      !>-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
      module logical function zzvalt ( ctx, string, b, e, letter )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: nparsi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! Original time string.
      character(1), intent(in) :: letter  !! New token if integer is within range.
      integer(ip), intent(in) :: b  !! Lower bound of value range
      integer(ip), intent(in) :: e  !! Upper bound of value range
      integer(ip), parameter :: wdsize = 32  !! length of a word of the error message
      character(3), parameter :: months ( 12 ) = &
         [ character(3) :: 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', &
           'SEP', 'OCT', 'NOV', 'DEC' ]
      integer(ip) :: item
      character(wdsize) :: myerr
      integer(ip) :: ptr
      integer(ip) :: value
      integer(ip) :: k
      integer(ip) :: i
      logical :: did
      integer(ip) :: j
      if ( .not. allocated ( ctx%zztime ) ) allocate ( ctx%zztime )
      associate ( rep => ctx%zztime%rep, &
                  size => ctx%zztime%size, &
                  begs => ctx%zztime%begs, &
                  ends => ctx%zztime%ends )
!
!     So far no translations have been performed.
!
      did = no
!
!     Examine each token to see if it is an integer.
!
      do i = 1, size

         item = ichar( rep(i:i) )

         if ( item == ichar('i') ) then
!
!           We've got an integer. Parse it to see if it
!           is in the specified range.
!
            j = begs(i)
            k = ends(i)

            call nparsi ( string(j:k), value, myerr, ptr )

            if (      ptr   == 0 &
                .and. value >= b &
                .and. value <= e ) then

               rep(i:i) = letter
               did      = yes

            end if

         end if

      end do

      zzvalt = did
      return
      end associate
      end function zzvalt

      !> Parse a time string and return seconds past the J2000 epoch
      !> on a formal calendar.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is an input time string, containing a Calendar or Julian
      !>              Date. It may be in several different formats and can make
      !>              use of abbreviations. Several example strings and the
      !>              times that they translate to are listed below in the
      !>              $Examples section.
      !>
      !>## Detailed Output
      !>
      !>     SP2000   is the equivalent of UTC, expressed in UTC seconds past
      !>              J2000. If an error occurs, or if the input time string is
      !>              ambiguous, SP2000 is not changed.
      !>
      !>     ERRMSG   is a descriptive error message, which is blank when no
      !>              error occurs.
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
      !>     The input string is examined and the various components of a date
      !>     are identified: julian date, year, month, day of year, day of
      !>     month, hour, minutes, seconds. These items are assumed to be
      !>     components on a calendar that contains no leapseconds (i.e. every
      !>     day is assumed to have exactly 86400 seconds).
      !>
      !>     TPARSE recognizes a wide range of standard time formats. The
      !>     $Examples section contains a list of several common strings that
      !>     are recognized and their interpretation. TPARSE relies on the
      !>     lower level SPICELIB routine TPARTV to interpret the input string.
      !>
      !>     Here is a brief summary of some of the basic rules used in the
      !>     interpretation of strings.
      !>
      !>     1)  Unless the substring 'JD' or 'jd' is present, the string is
      !>         assumed to be a calendar format (day-month-year or year and
      !>         day of year). If the substring JD or jd is present, the
      !>         string is assumed to represent a Julian date.
      !>
      !>     2)  If the Julian date specifier is not present, any integer
      !>         greater than 999 is regarded as being a year specification.
      !>
      !>     3)  A dash '-' can represent a minus sign only if it precedes
      !>         the first digit in the string and the string contains
      !>         the Julian date specifier (JD). (No negative years,
      !>         months, days, etc. are allowed).
      !>
      !>     4)  Numeric components of a time string must be separated
      !>         by a character that is not a digit or decimal point.
      !>         Only one decimal component is allowed. For example
      !>         1994219.12819 is sometimes interpreted as the
      !>         219th day of 1994 + 0.12819 days. TPARSE does not
      !>         support such strings.
      !>
      !>     5)  No exponential components are allowed. For example you
      !>         can't specify the Julian date of J2000 as 2.451545E6.
      !>         You also can't input 1993 Jun 23 23:00:01.202E-4 and have
      !>         to explicitly list all zeros that follow the decimal
      !>         point: i.e. 1993 Jun 23 23:00:00.0001202.
      !>
      !>     6)  The single colon (:) when used to separate numeric
      !>         components of a string is interpreted as separating
      !>         Hours, Minutes, and Seconds of time.
      !>
      !>     7)  If a double slash (//) or double colon (::) follows
      !>         a pair of integers, those integers are assumed  to
      !>         represent the year and day of year.
      !>
      !>     8)  A quote followed by an integer less than 100 is regarded
      !>         as an abbreviated year. For example: '93 would be regarded
      !>         as the 93rd year of the reference century. See the SPICELIB
      !>         routine TEXPYR for further discussion of abbreviated years.
      !>
      !>     9)  An integer followed by 'B.C.' or 'A.D.' is regarded as
      !>         a year in the era associated with that abbreviation.
      !>
      !>     10) All dates are regarded as belonging to the extended
      !>         Gregorian Calendar (the Gregorian calendar is the calendar
      !>         currently used by western society). See the SPICELIB routine
      !>         JUL2GR for converting from Julian Calendar to the Gregorian
      !>         Calendar.
      !>
      !>     11) If the ISO date-time separator (T) is present in the string
      !>         ISO allowed token patterns are examined for a match
      !>         with the current token list. If no match is found the
      !>         search is abandoned and appropriate diagnostic messages
      !>         are generated. Historically the interpretation of ISO
      !>         formatted time strings deviates from the ISO standard in
      !>         allowing two digit years and expanding years in the 0 to 99
      !>         range the same way as is done for non ISO formatted strings.
      !>         Due to this interpretation it is impossible to specify
      !>         times in years in the 0 A.D. to 99 A.D. range using ISO
      !>         formatted strings on the input.
      !>
      !>     12) If two delimiters are found in succession in the time
      !>         string, the time string is diagnosed as an erroneous string.
      !>         (Delimiters are comma, white space, dash, slash, period, or
      !>         day of year mark. The day of year mark is a pair of forward
      !>         slashes or a pair of colons.)
      !>
      !>         Note the delimiters do not have to be the same. The pair
      !>         of characters ",-" counts as two successive delimiters.
      !>
      !>     13) White space and commas serve only to delimit tokens in the
      !>         input string. They do not affect the meaning of any
      !>         of the tokens.
      !>
      !>     14) If an integer is greater than 1000 (and the 'JD' label
      !>         is not present, the integer is regarded as a year.
      !>
      !>     15) When the size of the integer components does not clearly
      !>         specify a year the following patterns are assumed
      !>
      !>         Calendar Format
      !>
      !>            Year Month Day
      !>            Month Day Year
      !>            Year Day Month
      !>
      !>            where Month is the name of a month, not its numeric
      !>            value.
      !>
      !>            When integer components are separated by slashes (/)
      !>            as in 3/4/5. Month, Day, Year is assumed (2005 March 4)
      !>
      !>         Day of Year Format.
      !>
      !>            If a day of year marker is present (// or ::) the
      !>            pattern
      !>
      !>              I-I// or I-I:: (where I stands for an integer)
      !>
      !>            is interpreted as Year Day-of-Year. However, I-I/ is
      !>            regarded as ambiguous.
      !>
      !>     To understand the complete list of strings that can be understood
      !>     by TPARSE you need to examine the SPICELIB routine TPARTV and read
      !>     the appendix to the TIME required reading entitled "Parsing Time
      !>     Strings."
      !>
      !>     TPARSE does not support the specification of time system
      !>     such as TDT or TDB; AM/PM specifications of time; or time
      !>     zones (such as PDT, UTC+7:20, etc.).
      !>
      !>     If some part of the time string is not recognized or if
      !>     the meaning of the components are not clear, an error string
      !>     is constructed that explains the problem with the string.
      !>
      !>     Since the routine works by breaking the input string into
      !>     a sequence of tokens whose meanings are determined by position
      !>     and magnitude, you can supply strings such as 1993 FEB 35 and
      !>     have this correctly interpreted as March 7, 1993. However,
      !>     this default action can be modified so that only "proper"
      !>     calendar dates and times are recognized. To do this call
      !>     the routine TPARCH as shown below:
      !>
      !>        CALL TPARCH ( 'YES' )
      !>
      !>     This will cause the routine to treat dates and times with
      !>     components outside the normal range as errors.
      !>
      !>     To return to the default behavior
      !>
      !>        CALL TPARCH ( 'NO' )
      !>
      !>     This routine returns information about parse errors in the output
      !>     string ERRMSG.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Parse a series of time strings representing calendar dates and
      !>        output number of seconds past J2000 epoch that corresponds to
      !>        each of them. Some of the input strings have an invalid format
      !>        which is reflected in their output.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM TPARSE_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants
      !>C
      !>      INTEGER               ERRMLN
      !>      PARAMETER           ( ERRMLN = 36 )
      !>
      !>      INTEGER               DATELN
      !>      PARAMETER           ( DATELN = 22 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(DATELN)    DATE   (7)
      !>      CHARACTER*(ERRMLN)    ERRMSG
      !>
      !>      DOUBLE PRECISION      SP2000
      !>
      !>      INTEGER               I
      !>
      !>C
      !>C     Assign an array of calendar dates. Not all of them are
      !>C     valid.
      !>C
      !>      DATA                  DATE   / '1986-01-18T12:19:52.18',
      !>     .                               '17JUN1982 18:28:28',
      !>     .                               '182-''92/ 12:28:29.182',
      !>     .                               '''67-271/ 12:28:30.291',
      !>     .                               '-467-14-25 26:00:75',
      !>     .                               '1993 FEB 35',
      !>     .                               '1993 MAR 7' /
      !>
      !>C
      !>C     Loop over the DATE array, call TPARSE for each element.
      !>C
      !>      WRITE(*,'(A)') '    Input string        '
      !>     .            // 'UTC sec past J2000'
      !>      WRITE(*,'(A)') '----------------------  '
      !>     .            // '------------------'
      !>
      !>      DO I= 1, 7
      !>
      !>         CALL TPARSE ( DATE(I), SP2000, ERRMSG )
      !>
      !>         IF ( ERRMSG .EQ. ' ' ) THEN
      !>
      !>            WRITE(*,'(A22,2X,F18.6)') DATE(I), SP2000
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A22,2X,A)') DATE(I), ERRMSG
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a PC/Linux/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>            Input string        UTC sec past J2000
      !>        ----------------------  ------------------
      !>        1986-01-18T12:19:52.18   -440293207.820000
      !>        17JUN1982 18:28:28       -553541492.000000
      !>        182-'92/ 12:28:29.182    -236820690.818000
      !>        '67-271/ 12:28:30.291    2137710510.291000
      !>        -467-14-25 26:00:75     An unexpected delimiter ('-') was en
      !>        1993 FEB 35              -215265600.000000
      !>        1993 MAR 7               -215265600.000000
      !>
      !>        Note that the "1993 FEB 35" string in converted to UTC seconds
      !>        past J2000, interpreted as "1993 MAR 7". Also note that the
      !>        error message resulting from parsing "-467-14-25 26:00:75"
      !>        is truncated to the provided ERRMLN length.
      !>
      !>     2) Below is a sampling of some of the time formats that are
      !>        acceptable as inputs to TPARSE. A complete discussion of
      !>        permissible formats is given in the reference document
      !>        time.req.
      !>
      !>        ISO (T) Formats.
      !>
      !>        String                        Year Mon  DOY DOM  HR Min Sec
      !>        ----------------------------  ---- ---  --- ---  -- --- -----
      !>        1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
      !>        1986-01-18T12                 1986 Jan   na  18  12  00 00
      !>        1986-01-18T12:19              1986 Jan   na  18  12  19 00
      !>        1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18
      !>        1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18
      !>        1995-08T18:28:12              1995  na  008  na  18  28 12
      !>        1995-08T18:28:12Z             1995  na  008  na  18  28 12
      !>        1995-18T                      1995  na  018  na  00  00 00
      !>        0000-01-01T                   1 BC Jan   na  01  00  00 00
      !>
      !>        Calendar Formats.
      !>
      !>        String                        Year   Mon DOM  HR Min  Sec
      !>        ----------------------------  ----   --- ---  -- ---  ------
      !>        Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57
      !>        1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192
      !>        2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002
      !>        Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287
      !>        1992 11:18:28  3 Jul          1992   Jul  03  11  18  28
      !>        June 12, 1989 01:21           1989   Jun  12  01  21  00
      !>        1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29
      !>        17JUN1982 18:28:28            1982   Jun  17  18  28  28
      !>        13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128
      !>        1972 27 jun 12:29             1972   Jun  27  12  29  00
      !>        '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289
      !>        27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182
      !>        23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29
      !>        18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291
      !>        29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298
      !>        29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298
      !>
      !>        Day of Year Formats.
      !>
      !>        String                        Year  DOY HR Min Sec
      !>        ----------------------------  ----  --- -- --- ------
      !>        1997-162::12:18:28.827        1997  162 12  18 28.827
      !>        162-1996/12:28:28.287         1996  162 12  28 28.287
      !>        1993-321/12:28:28.287         1993  231 12  28 28.287
      !>        1992 183// 12:18:19           1992  183 12  18 19
      !>        17:28:01.287 1992-272//       1992  272 17  28 01.287
      !>        17:28:01.282 272-1994//       1994  272 17  28 01.282
      !>        '92-271/ 12:28:30.291         1992* 271 12  28 30.291
      !>        92-182/ 18:28:28.281          1992* 182 18  28 28.281
      !>        182-92/ 12:29:29.192          0182+ 092 12  29 29.192
      !>        182-'92/ 12:28:29.182         1992  182 12  28 29.182
      !>
      !>        Julian Date Strings.
      !>
      !>        jd 28272.291                  Julian Date   28272.291
      !>        2451515.2981 (JD)             Julian Date 2451515.2981
      !>        2451515.2981 JD               Julian Date 2451515.2981
      !>
      !>                                     Abbreviations Used in Tables
      !>
      !>                                        na    --- Not Applicable
      !>                                        Mon   --- Month
      !>                                        DOY   --- Day of Year
      !>                                        DOM   --- Day of Month
      !>                                        Wkday --- Weekday
      !>                                        Hr    --- Hour
      !>                                        Min   --- Minutes
      !>                                        Sec   --- Sec
      !>
      !>        *  The default interpretation of a year that has been
      !>           abbreviated to two digits with or without a leading quote
      !>           as in 'xy or xy (such as '92 or 92) is to treat the year as
      !>           19xy if xy > 68 and to treat it as 20xy otherwise. Thus '70
      !>           is interpreted as 1970 and '67 is treated as 2067. However,
      !>           you may change the "split point" and centuries through use
      !>           of the SPICE routine TSETYR. See that routine for a
      !>           discussion of how you may reset the split point.
      !>
      !>        ** All epochs are regarded as belonging to the Gregorian
      !>           calendar. We formally extend the Gregorian calendar backward
      !>           and forward in time for all epochs. If you have epochs
      !>           belonging to the Julian Calendar, consult the SPICELIB
      !>           routines TPARTV and JUL2GR for a discussion concerning
      !>           conversions to the Gregorian calendar and ET. The routines
      !>           TIMDEF and STR2ET, used together, also support conversions
      !>           from Julian Calendar epochs to ET.
      !>
      !>        +  When a day of year format or calendar format string is
      !>           input and neither of the integer components of the date is
      !>           greater than 1000, the first integer is regarded as being
      !>           the year.
      !>
      !>        Any integer greater than 1000 is regarded as a year
      !>        specification. Thus 1001-1821//12:28:28 is interpreted as
      !>        specifying two years and will be rejected as ambiguous.
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
      !>     M. Costa Sitja     (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     W.M. Owen          (JPL)
      !>     B.V. Semenov       (JPL)
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.1.0, 23-DEC-2021 (JDR) (MCS)
      !>
      !>        Changed the output argument name ERROR to ERRMSG for
      !>        consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries in $Revisions section.
      !>
      !>        Added complete example code.
      !>
      !>        Updated $Examples to refer to STR2ET and TIMDEF as a
      !>        mechanism to convert from Julian Calendar to ephemeris time.
      !>
      !>        Added TIME to $Required_Reading list.
      !>
      !>-    SPICELIB Version 5.0.1, 18-MAY-2010 (BVS)
      !>
      !>        Removed "C$" marker from text in the header.
      !>
      !>-    SPICELIB Version 5.0.0, 30-DEC-1997 (WLT)
      !>
      !>        The routine was modified to compensate for the inability
      !>        of the Muller-Wimberly formula to handle negative years
      !>        (that is years prior to 1 AD).
      !>
      !>        Comments concerning the default century used for two
      !>        digit years were upgraded.
      !>
      !>-    SPICELIB Version 4.0.0, 08-APR-1996 (WLT)
      !>
      !>        All of the token recognition and parsing was moved
      !>        into the routine TPARTV. The entry point TPARCH
      !>        was moved to the routine TCHECK.
      !>
      !>        This routine now merely assembles the
      !>        parsed components to produce SP2000.
      !>
      !>        The number of strings now recognized has been greatly
      !>        increased. However, the interpretation given to
      !>        strings such as 31 Jan 32 has been changed.
      !>
      !>-    SPICELIB Version 3.0.0, 30-JUL-1993 (WLT)
      !>
      !>        The entry point TPARCH was added so that users may
      !>        restrict the set of input calendar strings to those
      !>        that are in proper form.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 18-NOV-1991 (MJS)
      !>
      !>        TPARSE no longer accepts a blank time string.
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (JML)
      !>
      !>        In the $Detailed_Input section of the header, the
      !>        description of how default values are assigned to
      !>        tokens in STRING was clarified.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 18-NOV-1991 (MJS)
      !>
      !>        TPARSE no longer accepts a blank time string. Prior to
      !>        this fix, TPARSE interpreted a blank time string to be
      !>        -1577880000.000 UTC seconds (1 JAN 1950 00:00:00).
      !>
      !>-    SPICELIB Version 1.0.1, 26-MAR-1991 (JML)
      !>
      !>        In the $Detailed_Input section of the header, the
      !>        description of how default values are assigned to
      !>        tokens in STRING was clarified.
      !>
      !>        NAIFers are accustomed to specifying day of year
      !>        formats of UTC strings in the following form:
      !>
      !>           1986-247 // 12:00:00
      !>
      !>        This revision to the header states explicitly that
      !>        the // is a blank token which results in the default
      !>        value being assigned to the month token. The previous
      !>        version of the header implied that tokens could be left
      !>        out or "missing" from the string, and that default values
      !>        would automatically be assigned. This works only for
      !>        tokens missing from the right end of the string. For
      !>        default values to be assigned to tokens missing from the
      !>        middle of a UTC string, consecutive delimiters such as
      !>        // or :: must be included.
      module subroutine tparse ( ctx, string, sp2000, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: j2000, spd
      use pepper_strings, only: repmc_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! Input time string, UTC.
      real(wp), intent(out) :: sp2000  !! Equivalent UTC seconds past J2000.
      character(*), intent(out) :: errmsg  !! Descriptive error message.

!
!     Parameters
!



      integer(ip), parameter :: ampm = zone + 1  !! time string component: AM/PM

      integer(ip), parameter :: system = ampm + 1  !! time string component: time system


      character(5) :: type
      character(8) :: modify   ( 5 )

      character(lnsize) :: pictur

      real(wp) :: tvec     ( 10 )

      integer(ip) :: day
      integer(ip) :: month
      integer(ip) :: ntvec
      integer(ip) :: q
      integer(ip) :: temp
      integer(ip) :: year

      logical :: adjust
      logical :: mods
      logical :: ok
      logical :: succes
      logical :: yabbrv

!
!     All the work of taking apart the string is handled
!     by TPARTV.
!
      errmsg  = ' '
      succes = .true.

      call tpartv ( ctx, string, &
                    tvec,   ntvec, type, &
                    modify, mods,  yabbrv, succes, &
                    pictur, errmsg )

      if ( .not. succes ) return

!
!     We are not going to support all of the various
!     time string modifiers that can be parsed.
!
      if ( mods ) then

         if ( modify(system) /= ' ' ) then

            errmsg = 'TPARSE does not support the specification ' &
            //      'of a time system in a string.  The time ' &
            //      'system # was specified. '
            call repmc_inplace ( errmsg, '#', modify(system) )
            return

         else if ( modify(zone) /= ' ' ) then

            errmsg = 'TPARSE does not support the specification ' &
            //      'of a time zone in a time string.  The time ' &
            //      'zone ''#'' was specified. '
            call repmc_inplace ( errmsg, '#', modify(zone) )
            return

         else if ( modify(ampm) /= ' ' ) then

            errmsg = 'TPARSE does not support the AM/PM ' &
            //      'conventions for time strings. '
            return

         end if

      end if

      if ( type == 'JD' ) then
!
!        Nothing to do but convert TVEC(1).
!

         sp2000 = (tvec(1)-j2000) * spd

      else if ( type == 'YMD' .or. type == 'YD' ) then

         call tcheck ( ctx, tvec, type, mods, modify, ok, errmsg )

         if ( .not. ok ) return
!
!        If we have day of year format, we move it into the
!        month-day of month format.
!
         if ( type == 'YD' ) then
            tvec(6) = tvec(5)
            tvec(5) = tvec(4)
            tvec(4) = tvec(3)
            tvec(3) = tvec(2)
            tvec(2) = 1.0_wp
         end if
!
!        Get the year month and day as integers.
!
         year   = nint( tvec(1) )
         month  = nint( tvec(2) )
         day    = nint( tvec(3) )
!
!        Fix up the year as needed.
!
         if ( modify(era) == 'B.C.' ) then

            year = 1 - year

         else if ( modify(era) == 'A.D.' ) then
!
!           Do nothing.
!
         else if ( year < 100 ) then

            call texpyr ( ctx, year )

         end if

!
!        Apply the Muller-Wimberly formula and then tack on
!        the seconds.
!
         if ( year < 1 ) then
!
!           The Muller-Wimberly formula doesn't work for years
!           less than 0.  So we boost the year by an appropriate
!           multiple of 400 and then subtract the appropriate
!           number of days later.
!
            adjust = .true.
            temp   =  year

            call rmaini ( ctx, temp, 400, q, year )

            year = year  + 400
            q    = q-1

         else

            adjust = .false.

         end if

         day    = 367*year - 7*(year+(month+9)/12)/4 &
                           - 3*((year+(month-9)/7)/100+1)/4 &
                           + 275*month/9 + day - 730516

         if ( adjust ) then
!
!           Adjust DAY by the appropriate multiple of 400 years.
!
            day = day + q*(400*365 + 97)

         end if

         sp2000 =  ( real(day, wp) - 0.5_wp ) * spd &
                  +  3600.0_wp * tvec(4) &
                  +    60.0_wp * tvec(5) &
                  +             tvec(6)

      else
!
!        We've already covered all the bases we are planning to
!        cover in this routine.  Any other case is regarded as an
!        error.
!

         errmsg = 'The only type of time strings that are handled ' &
         //      'by TPARSE are ''JD'', ''YMD'' and ''YD'' ' &
         //      '(year day-of-year).  You''ve entered a string ' &
         //      'of the type #. '

         call repmc_inplace ( errmsg, '#', type )

      end if

      return

      end subroutine tparse

      !> PEPPER: initializes the pool state PS (SPICELIB passed the arrays).
      !>
      !> This routine initializes the data structures needed for
      !> maintaining the kernel pool and initializes the hash function
      !> used for the name list in the kernel pool.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzpini ( ctx, ps )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(pool_state), intent(inout) :: ps  !! the kernel pool
      integer(ip), parameter :: shrtln = 9
      character(shrtln), parameter :: dat = 'begindata'
      character(shrtln), parameter :: txt = 'begintext'
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      if ( .not. ps%first ) return
      ps%begdat = char(bslash) // dat
      ps%begtxt = char(bslash) // txt
      call zzctrsin ( ctx, ps%subctr )
      if ( .not. ctx%failed() ) ps%first = .false.
      end subroutine zzpini

      !> PEPPER: stores into the pool state PS (SPICELIB passed the arrays).
      !>
      !> Read the next variable from a SPICE ASCII kernel file into
      !> the kernel pool.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NAMLST    this collection of arrays together with the hash
      !>     NMPOOL    function ZZHASH provide the mechanism for storing
      !>     NAMES     and retrieving kernel pool variables.
      !>     DATLST
      !>     DPPOOL    Given a potential variable name NAME the function
      !>     DPVALS    ZZHASH(NAME) gives the location in the array in
      !>     CHPOOL    NAMLST where one should begin looking for the
      !>     CHVALS    kernel pool variable NAME.
      !>
      !>               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel
      !>               pool variable corresponding to NAME.  If it is non-zero
      !>               then NAMLST is the head node of a linked list of names
      !>               that evaluate to the same integer under the function
      !>               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check
      !>               NAMES(NODE) for equality with NAME.  If there is
      !>               no match find the next node ( NMPOOL(NEXT,NODE) ) until
      !>               a match occurs or all nodes of the list have been
      !>               examined.  To insert a new NAME allocate a node NEW from
      !>               the free list of NMPOOL and append it to the tail of the
      !>               list pointed to by NAMLST ( ZZHASH(NAME) ).
      !>
      !>               Once a node for NAME is located (call it NAMEAT)
      !>               the values for NAME can be found by examining
      !>               DATLST(NAMEAT).  If zero, no values have yet been
      !>               given to NAME.  If less than zero, -DATLST(NAMEAT)
      !>               is the head node of a list in CHPOOL that gives the
      !>               indexes of the values of NAME in CHVALS.  If greater
      !>               than zero, DATLST(NAMEAT) is the head node of a list
      !>               in DPPOOL that gives the indexes of the values of NAME
      !>               in DPVALS.
      !>
      !>## Detailed Output
      !>
      !>     NAMLST     is the same structure as input but updated to
      !>     NMPOOL     include the next variable read from the current
      !>     NAMES      active text kernel in RDKER.
      !>     DATLST
      !>     DPPOOL
      !>     DPVALS
      !>     CHPOOL
      !>     CHVALS
      !>
      !>     VARNAM      is the name of the variable. VARNAM is blank if
      !>                 no variable is read.
      !>
      !>      EOF        is true when the end of the kernel file has been
      !>                 reached, and is false otherwise. The kernel file
      !>                 is closed automatically when the end of the file
      !>                 is reached.
      !>
      !>## Parameters
      !>
      !>      LINLEN      is the maximum length of a line in the kernel file.
      !>
      !>      MAXLEN      is the maximum length of the variable names that
      !>                  can be stored in the kernel pool (also set in
      !>                  pool.f).
      !>
      !>## Exceptions
      !>
      !>     1) The error 'SPICE(BADTIMESPEC)' is signaled if a value
      !>        beginning with '@' cannot be parsed as a time.
      !>
      !>     2) The error 'SPICE(BADVARASSIGN)' is signaled if variable
      !>        assignment does not have the form NAME = [(] value [ value ) ].
      !>
      !>     3) The error 'SPICE(KERNELPOOLFULL)' is signaled if there is
      !>        no room left in the kernel pool to store another variable
      !>        or value.
      !>
      !>     4) The error 'SPICE(NONPRINTINGCHAR)' is signaled if the name
      !>        in a variable assignment contains a non-printing character.
      !>
      !>     5) The error 'SPICE(NUMBEREXPECTED)' is signaled if a value
      !>        that is unquoted cannot be parsed as time or number.
      !>
      !>     6) The error 'SPICE(TYPEMISMATCH)' is signaled if a variable
      !>        has a first value of one type (numeric or character) and
      !>        a subsequent component has the other type.
      !>
      !>     7) The error 'SPICE(BADVARNAME)' signals if a kernel pool
      !>        variable name length exceeds MAXLEN.
      !>
      !>## Files
      !>
      !>     ZZRVAR reads from the file most recently opened by RDKNEW.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     See POOL (entry point LDPOOL).
      !>
      !>## Restrictions
      !>
      !>     The input file must be opened and initialized by RDKNEW prior
      !>     to the first call to ZZRVAR.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber   (JPL)
      !>     B.V. Semenov (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.7.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.7.0, 08-FEB-2010 (EDW)
      !>
      !>        Added an error check on the length of the kernel pool variable
      !>        name read from the kernel file.
      !>
      !>-    SPICELIB Version 1.6.0, 06-AUG-2002 (BVS)
      !>
      !>        Modified to make sure that DO WHILE loop that looks for the
      !>        end of string variable value always exits.
      !>
      !>-    SPICELIB Version 1.5.0, 07-APR-2000 (WLT)
      !>
      !>        Happy Birthday Alex. Added check to the assignment to CHVALS
      !>        so that we cannot store data past the end of the string.
      !>
      !>-    SPICELIB Version 1.4.0, 22-MAR-1999 (WLT)
      !>
      !>        Added code to detect and signal an error for empty
      !>        vector assignment.
      !>
      !>-    SPICELIB Version 1.3.0, 16-JAN-1997 (WLT)
      !>
      !>        The error message regarding the directives allowed
      !>        in a keyword =  value directive was updated.
      !>
      !>-    SPICELIB Version 1.1.0, 25-JUN-1996 (WLT)
      !>
      !>        The error message for unparsed numeric components
      !>        was corrected so that it now shows the line and
      !>        line number on which the error occurred.
      !>
      !>-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
      module subroutine zzrvar ( ctx, ps, varnam, eof )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_strings, only: lastpc, nparsd, rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(pool_state), intent(inout) :: ps  !! the kernel pool
      character(:), allocatable, intent(out) :: varnam  !! name of variable parsed.
      logical, intent(out) :: eof  !! if TRUE end of input file has been reached.



!     The size of the kernel pool's hash table (MAXVAR in POOL).
!

!
!     The next collection of variables are set up in first pass
!     through this routine.  They would be parameters if FORTRAN
!     allowed us to do this in a standard way.
!

      character(linlen) :: line
      character(linlen) :: cval  ! PEPPER: a string value is no longer than its line
      integer(ip) :: ncval
      character(linlen) :: name
      character(:), allocatable :: file  !! the kernel's name (PEPPER: any length)

      character(255) :: error

      real(wp) :: dvalue

      integer(ip) :: at
      integer(ip) :: b
      integer(ip) :: badat
      integer(ip) :: chnode
      integer(ip) :: code
      integer(ip) :: count
      integer(ip) :: datahd
      integer(ip) :: dirctv
      integer(ip) :: dpnode
      integer(ip) :: e
      integer(ip) :: free
      integer(ip) :: head
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: lookat
      integer(ip) :: lstnb
      integer(ip) :: nameat
      integer(ip) :: ncomp
      integer(ip) :: node
      integer(ip) :: number
      integer(ip) :: nxttok
      integer(ip) :: r1
      integer(ip) :: r2
      integer(ip) :: status
      integer(ip) :: tail
      integer(ip) :: varlen
      integer(ip) :: vartyp

      integer(ip) :: begs(linlen)
      integer(ip) :: ends(linlen)
      integer(ip) :: type(linlen)

!
!     The logicals below are used to take apart the tokens in an
!     input line.
!
      logical :: even
      logical :: intokn
      logical :: inquot
      logical :: insepf

      logical :: full
      logical :: found

!
!     The following logicals are in-line functions that are used
!     when processing the input strings.
!
      logical :: issep
      logical :: isquot
      logical :: isequ
      logical :: isrpar
      logical :: islpar
      logical :: isplus
      logical :: isbad
      logical :: istime

!
!     Constants (SPICELIB computed them on the first call).
!

!
!     Below are a collection of In-line function definitions that are
!     intended to make the code a bit easier to write and read.
!

      issep ( code ) =      code == iblank &
                       .or. code == icomma &
                       .or. code == ilparn &
                       .or. code == irparn &
                       .or. code == iequal &
                       .or. code == itab

      isquot( code ) =      code == iquote
      isequ ( code ) =      code == iequal
      isrpar( code ) =      code == irparn
      islpar( code ) =      code == ilparn
      isplus( code ) =      code == iplus
      istime( code ) =      code == itmark

      isbad ( dirctv ) =       dirctv /= eq &
                         .and. dirctv /= eqp

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Initializations.
!

!
!     No variable yet and no parsing errors so far.
!
      varnam = ' '
      error = ' '
      ncomp = 0

!
!     Get the next data line. Unless something is terribly wrong,
!     this will begin a new variable definition. We have to read
!     the whole variable, unless we get an error, in which case
!     we can quit.
!
      status = begin

      do while ( ( status /= done ) .and. ( .not. ctx%failed() ) )

         call rdkdat ( ctx, line, eof )

         if ( eof ) return

!
!        Find the "tokens" in the input line. As you scan from left
!        to right along the line, exactly one of the following
!        conditions is true.
!
!        1) You are in a separator field
!        4) You are in a quoted substring
!        5) You are in a non-quoted substring that isn't a separator
!           field.
!
!        Stuff between separator fields are regarded as tokens.  Note
!        this includes quoted strings.
!
!        In addition we keep track of 3 separators: '=', '(', ')'
!        Finally, whenever we encounters the separator '=', we back
!        up and see if it is preceded by a '+', if so we attach
!        it to the '=' and treat the pair of characters as a single
!        separator.
!

         even   = .true.
         intokn = .false.
         inquot = .false.
         insepf = .true.

         count  =  0
         i      =  0

         do while ( i < len(line) )
!
!           The current character is either a separator, quote or
!           some other character.
!
            i    = i + 1
            code = ichar(line(i:i))

            if ( issep(code) ) then
!
!              There are 3 possible states we could be in
!                 Separation Field
!                 A quoted substring with the last quote an odd one.
!                 A quoted substring with the last quote an even one.
!                 A non-quoted token.
!              In the first two cases nothing changes, but in the
!              next two cases we transition to a separation field.
!
               if ( intokn .or. ( inquot .and. even ) ) then
                  inquot = .false.
                  intokn = .false.
                  insepf = .true.
               end if

               if ( insepf ) then
!
!                 We need to see if this is one of the special
!                 separators
!
                  if      ( isequ(code)  ) then

                     count       = count + 1
                     begs(count) = i
                     type(count) = eq
                     ends(count) = i

                     if ( i > 1 ) then
!
!                       Look back at the previous character.
!                       See if it is a plus character.
!
                        code = ichar( line(i-1:i-1) )

                        if ( isplus(code) ) then
!
!                          This is the directive '+=' we need
!                          to set the beginning of this token
!                          to the one before this and adjust
!                          the end of the last token.
!
                           type(count) = eqp
                           begs(count) = i-1

                           if (      begs(count-1) &
                                == ends(count-1) ) then

                              count       = count - 1
                              begs(count) = i - 1
                              ends(count) = i
                              type(count) = eqp

                           else

                              ends(count-1) = ends(count-1) - 1

                           end if

                        end if

                     end if

                  else if ( isrpar(code) ) then

                     count       = count + 1
                     begs(count) = i
                     ends(count) = i
                     type(count) = ev

                  else if ( islpar(code) ) then

                     count       = count + 1
                     begs(count) = i
                     ends(count) = i
                     type(count) = bv

                  end if

               end if

            else if ( isquot(code) ) then
!
!              There are 3 cases of interest.
!                 We are in a quoted substring already
!                 We are in a separator field
!                 We are in a non-quoted token.
!              In the first case nothing changes.  In the second
!              two cases we change to being in a quoted substring.
!
               even = .not. even

               if ( .not. inquot ) then

                  insepf      = .false.
                  intokn      = .false.
                  inquot      = .true.
                  count       =  count + 1
                  begs(count) =  i
                  type(count) =  q

               end if

               ends(count) = i

            else
!
!              This is some character other than a quote, or
!              separator character.
!
!              We are in one of four situations.
!
!                 1) We are in a quoted substring with an odd number of
!                    quotes.
!                 2) We are in a quoted substring with an even number of
!                    quotes.
!                 2) We are in a separator field
!                 3) We are in a non-quoted token.
!
!              In cases 1 and 3 nothing changes. So we won't check
!              those cases.
!

               if ( insepf .or. ( inquot .and. even ) ) then
                  inquot      = .false.
                  insepf      = .false.
                  intokn      = .true.
                  count       =  count + 1
                  begs(count) =  i
                  type(count) =  nq
               end if

               ends(count) = i

            end if

         end do

!
!        The first word on the first line should be the name of a
!        variable. The second word should be a directive: = or +=.
!
         if ( status == begin ) then
!
!           There must be at least 3 contributing tokens on this line.
!
            if ( count < 3 ) then

               call kernel_location ( file, number )
               call ctx%setmsg ( 'A kernel variable was not properly ' &
               //            'formed on line # of the file #. ' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''. This line was ''#''. ' )

               r1 = rtrim(file)
               r2 = rtrim(line)

               call ctx%errint ( '#', number     )
               call ctx%errch  ( '#', file(1:r1) )
               call ctx%errch  ( '#', line(1:r2) )
               call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
               return

            end if
!
!           See if the variable name is legitimate:
!
            badat = lastpc(line(begs(1):ends(1)))

            if ( badat <= ends(1) - begs(1) ) then
!
!              There is a non-printing character in the variable
!              name.  This isn't allowed.
!
               at = begs(1) + badat

               call kernel_location ( file, number )
               r1 = rtrim  ( file )

               call ctx%setmsg ( 'There is a non-printing character ' &
               //            'embedded in line # of the text ' &
               //            'kernel file #.  Non-printing ' &
               //            'characters are not allowed in ' &
               //            'kernel variable assignments.  The ' &
               //            'non-printing character has ASCII ' &
               //            'code #. ' )

               call ctx%errint ( '#', number             )
               call ctx%errch  ( '#',        file(1 :r1) )
               call ctx%errint ( '#', ichar(line(at:at)) )
               call ctx%sigerr ( 'SPICE(NONPRINTINGCHAR)')
               return
            end if

!
!           Check the variable name length; signal an error
!           if longer than MAXLEN.
!
            varlen = len( line( begs(1): ends(1) ) )

            ! PEPPER: names have any length (SPICELIB signaled
            ! SPICE(BADVARNAME) for more than 32 characters).

!
!           The variable name is ok. How about the directive.
!

            varnam = line(begs(1):ends(1))

            dirctv = type(2)

!
!           If this is replacement (=) and not an addition (+=),
!           delete the values currently associated with the variable.
!           They will be replaced later.
!
            if ( isbad(dirctv) ) then

               call kernel_location ( file, number )
               call ctx%setmsg ( 'A kernel variable was not properly ' &
               //            'formed on line # of the file #. ' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''.  More specifically, ' &
               //            'the assignment operator did not have ' &
               //            'one of the expected forms: ''='' or ' &
               //            '''+=''. The line was ''#''. ' )

               r1 = rtrim(file)
               r2 = rtrim(line)

               call ctx%errint ( '#', number     )
               call ctx%errch  ( '#', file(1:r1) )
               call ctx%errch  ( '#', line(1:r2) )
               call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
               return

            end if
!
!           Locate this variable name in the name pool or insert it
!           if it isn't there.  The location will be NAMEAT and
!           we will use the variable FOUND to indicate whether or
!           not it was already present.
!
            call ps%create ( varnam, nameat, found )

!
!           Now depending upon the kind of directive, we will need
!           to remove data and allocate a new list or simply append
!           data to the existing list.
!
            if ( dirctv == eq ) then
!
!              We are going to dump whatever is associated with
!              this name and then we will need to allocate a new
!              linked list for the data.
!
               vartyp =  unknwn

               if ( found ) call ps%clear_values ( nameat )

            else if ( dirctv == eqp ) then
!
!              We need to append to the current variable.
!
               if ( found ) then

                  if ( ps%vars(nameat)%kind == pool_numeric ) then
                     vartyp = numtyp
                  else if ( ps%vars(nameat)%kind == pool_character ) then
                     vartyp = strtyp
                  else
                     vartyp = unknwn
                  end if

               else
                  vartyp = unknwn
               end if

            end if

            ! PEPPER: the source of the assignment (the kernel, and the line where
            ! the assignment begins)
            call kernel_location ( file, number )
            call ps%note_source ( nameat, file, number )

!
!           If this is a vector, the next thing on the line will be a
!           left parenthesis. Otherwise, assume that this is a scalar.
!           If it's a vector, get the first value. If it's a scalar,
!           plant a bogus right parenthesis, to make the following loop
!           terminate after one iteration.
!

            if ( type(3) == bv ) then
               nxttok        = 4
            else
               nxttok      = 3
               count       = count + 1
               type(count) = ev
            end if

!
!        For subsequent lines, treat everything as a new value.
!
         else

            nxttok = 1

         end if

!
!        We have a value anyway. Store it in the table.
!
!        Keep going until the other shoe (the right parenthesis)
!        drops, or until the end of the line is reached.
!
!        Dates begin with @; anything else is presumed to be a number.
!
         do while ( nxttok <= count )
            if ( type(nxttok) == ev ) exit
!
!           Get the begin and end of this token.
!
            b = begs(nxttok)
            e = ends(nxttok)

            if ( vartyp == unknwn ) then
!
!              We need to determine which category of variable we
!              have by looking at this token and deducing the
!              type.
!
               if      ( type(nxttok) == q  ) then

                  vartyp = strtyp

               else if ( type(nxttok) == nq ) then

                  vartyp = numtyp

               else
!
!                 This is an error. We should have had one of the
!                 two previous types.
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  call kernel_location ( file, number )
                  r1 = rtrim  ( file         )

                  call ctx%setmsg ( 'The first item following the ' &
                  //            'assignment operator should be ' &
                  //            'the value of a variable or a ' &
                  //            'left parenthesis ''('' followed ' &
                  //            'by a value for a variable. This ' &
                  //            'is not true on line # of the ' &
                  //            'text kernel file ''#''. ' )

                  call ctx%errint ( '#', number     )
                  call ctx%errch  ( '#', file(1:r1) )
                  call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
                  return

               end if

            end if

            if ( vartyp == strtyp ) then
!
!              First make sure that this token represents a string.
!
               if ( type(nxttok) /= q ) then
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  call kernel_location ( file, number )
                  r1 = rtrim  ( varnam        )
                  r2 = rtrim  ( file          )

                  call ctx%setmsg ( 'The kernel variable # has ' &
                  //            'been set up as a string ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the kernel file ''#'' is ' &
                  //            'not a string value. ' )

                  call ctx%errch  ( '#', varnam(1:r1) )
                  call ctx%errint ( '#', number       )
                  call ctx%errch  ( '#', file  (1:r2) )
                  call ctx%sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if
!
!              Still going? Make sure there is something between
!              the quotes.
!
               if ( b+1 >= e ) then
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  call kernel_location ( file, number )
                  r1 = rtrim  ( file          )

                  call ctx%setmsg ( 'There is a quoted string with ' &
                  //            'no characters on line # of the ' &
                  //            'text kernel file ''#''. ' )

                  call ctx%errint ( '#', number       )
                  call ctx%errch  ( '#', file  (1:r1) )
                  call ctx%sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if

!
!              We are ready to go.  Allocate a node for this data
!              item. First make sure there is room to do so.
!
               ncomp          = ncomp + 1

!
!              Adjust end-of-token position (E) if it happens to the
!              last, non-quote character of the truncated input line.
!              This has to be done to make sure that all meaningful
!              characters get moved to the value.
!
               code = ichar(line(e:e))
               if ( .not. isquot(code) ) then
                  e = e + 1
               end if

               ncval = 0
               j = b+1
               do while ( j < e )
                  code = ichar(line(j:j))
                  if ( isquot(code) ) then
                     j = j+1
                  end if
                  ncval = ncval + 1
                  cval(ncval:ncval) = line(j:j)
                  j = j + 1
               end do
               call ps%add_string ( nameat, cval(1:ncval) )
!
!              That's all for this value. It's now time to loop
!              back through and get the next value.
!
            else

               if ( type(nxttok) /= nq ) then
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  call kernel_location ( file, number )
                  r1 = rtrim  ( varnam        )
                  r2 = rtrim  ( file          )

                  call ctx%setmsg ( 'The kernel variable # has ' &
                  //            'been set up as a numeric or time ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the kernel file ''#'' is ' &
                  //            'not a numeric or time value. ' )

                  call ctx%errch  ( '#', varnam(1:r1) )
                  call ctx%errint ( '#', number       )
                  call ctx%errch  ( '#', file  (1:r2) )
                  call ctx%sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if
!
!              Look at the first character to see if we have a time
!              or a number.
!
               code = ichar( line(b:b) )

               if ( istime(code) ) then
!
!                 We need to have more than a single character.
!
                  if ( e == b ) then
!
!                    First perform the clean up function.
!
                     call ps%remove ( nameat )

                     call kernel_location ( file, number )
                     r1 = rtrim ( varnam        )
                     r2 = rtrim ( file          )

                     call ctx%setmsg ( 'At character # of  line # in ' &
                     //            'the text kernel file ''#'' the ' &
                     //            'character ''@'' appears.  This ' &
                     //            'character is reserved for ' &
                     //            'identifying time values in ' &
                     //            'assignments to kernel pool ' &
                     //            'variables.  However it is not ' &
                     //            'being used in this fashion for ' &
                     //            'the variable ''#''. ' )

                     call ctx%errint ( '#', b            )
                     call ctx%errint ( '#', number       )
                     call ctx%errch  ( '#', file  (1:r2) )
                     call ctx%errch  ( '#', varnam(1:r1) )
                     call ctx%sigerr ( 'SPICE(BADTIMESPEC)' )
                     return

                  end if

                  call tparse ( ctx, line(b+1:e), dvalue, error )

                  if ( error /= ' ' ) then
!
!                    First perform the clean up function.
!
                     call ps%remove ( nameat )

                     call kernel_location ( file, number )
                     r1 = rtrim  ( file )

                     lstnb = len_trim(error)

                     call ctx%setmsg ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a time ' &
                     //            'on line # of the text kernel ' &
                     //            'file ''#''.  Error message: ' &
                     //            '''#''' )

                     call ctx%errch  ( '#', line(b+1:e)     )
                     call ctx%errint ( '#', number          )
                     call ctx%errch  ( '#', file            )
                     call ctx%errch  ( '#', error(1:lstnb)  )
                     call ctx%sigerr ( 'SPICE(BADTIMESPEC)' )
                     return
                  end if

               else

                  call nparsd ( line(b:e), dvalue, error, i )

                  if ( error /= ' ' ) then

                     call ps%remove ( nameat )

                     call kernel_location ( file, number )

                     lstnb = len_trim(error)

                     call ctx%setmsg ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a number on ' &
                     //            'line # of the text kernel file ' &
                     //            '''#''.  Error message: ' &
                     //            '''#''' )
                     call ctx%errch  ( '#',  line(b:e) )
                     call ctx%errint ( '#',  number    )
                     call ctx%errch  ( '#',  file      )
                     call ctx%errch  ( '#', error(1:lstnb) )
                     call ctx%sigerr ( 'SPICE(NUMBEREXPECTED)' )
                     return
                  end if

               end if
!
!              OK. We have a parsed value.  See if there is room in
!              the numeric portion of the pool to store this value.
!
               call ps%add_number ( nameat, dvalue )
               ncomp          = ncomp + 1

            end if

!
!           Now process the next token in the list of tokens.
!
            nxttok = nxttok + 1

         end do
!
!        We could have ended the above loop in one of two ways.
!
!        1) NXTTOK now exceeds count.  This means we did not reach
!           an end of vector marker.
!        2) We hit an end of vector marker.
!
         if ( nxttok > count ) then
            status = invar
         else
            status = done
         end if

      end do

!
!     It is possible that we reached this point without actually
!     assigning a value to the kernel pool variable.  This can
!     happen if there is a vector input of the form NAME = ( )
!
      if ( ncomp < 1 ) then

         call ps%remove ( nameat )

         call kernel_location ( file, number )
         r1 = rtrim  ( file          )

         call ctx%setmsg ( 'The first item following the ' &
         //            'assignment operator should be ' &
         //            'the value of a variable or a ' &
         //            'left parenthesis ''('' followed ' &
         //            'by a value for a variable. This ' &
         //            'is not true on line # of the ' &
         //            'text kernel file ''#''. ' )

         call ctx%errint ( '#', number       )
         call ctx%errch  ( '#', file  (1:r1) )
         call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
         return

      end if

!
!     Return the name of the variable.
!

      return

      contains

      !> The name of the kernel being read, whole, and the number of the
      !> line just read (RDKLIN's, in a variable of the name's length;
      !> PEPPER: SPICELIB's was 255 characters).
      subroutine kernel_location ( kernel, line )
         character(:), allocatable, intent(out) :: kernel  !! the kernel's name
         integer(ip), intent(out) :: line  !! the line's number
         integer(ip) :: n
         n = 1
         if ( allocated ( ctx%spice%rdker%file ) ) n = max ( 1, len ( ctx%spice%rdker%file ) )
         allocate ( character(n) :: kernel )
         call rdklin ( ctx, kernel, line )
      end subroutine kernel_location
      end subroutine zzrvar

      !> PEPPER: stores into the pool state PS (SPICELIB passed the arrays).
      !>
      !> Read the next variable from a text buffer into the kernel pool.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     BUFFER    is a string array that contains the text that should
      !>               be parsed and placed into the kernel pool data
      !>               structure.
      !>
      !>     BSIZE     is the number of lines of text in BUFFER.
      !>
      !>     LINNUM    the line number (in BUFFER) at which to begin parsing
      !>               text.
      !>
      !>     NAMLST    this collection of arrays together with the hash
      !>     NMPOOL    function ZZHASH provide the mechanism for storing
      !>     NAMES     and retrieving kernel pool variables.
      !>     DATLST
      !>     DPPOOL    Given a potential variable name NAME the function
      !>     DPVALS    ZZHASH(NAME) gives the location in the array in
      !>     CHPOOL    NAMLST where one should begin looking for the
      !>     CHVALS    kernel pool variable NAME.
      !>
      !>               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel
      !>               pool variable corresponding to NAME.  If it is non-zero
      !>               then NAMLST is the head node of a linked list of names
      !>               that evaluate to the same integer under the function
      !>               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check
      !>               NAMES(NODE) for equality with NAME.  If there is
      !>               no match find the next node ( NMPOOL(NEXT,NODE) ) until
      !>               a match occurs or all nodes of the list have been
      !>               examined.  To insert a new NAME allocate a node NEW from
      !>               the free list of NMPOOL and append it to the tail of the
      !>               list pointed to by NAMLST ( ZZHASH(NAME) ).
      !>
      !>               Once a node for NAME is located (call it NAMEAT)
      !>               the values for NAME can be found by examining
      !>               DATLST(NAMEAT).  If zero, no values have yet been
      !>               given to NAME.  If less than zero, -DATLST(NAMEAT)
      !>               is the head node of a list in CHPOOL that gives the
      !>               indexes of the values of NAME in CHVALS.  If greater
      !>               than zero, DATLST(NAMEAT) is the head node of a list
      !>               in DPPOOL that gives the indexes of the values of NAME
      !>               in DPVALS.
      !>
      !>## Detailed Output
      !>
      !>     LINNUM     is the line number at which the "next" read should
      !>                begin.
      !>
      !>     NAMLST     is the same structure as input but updated to
      !>     NMPOOL     include the next variable read from the text buffer.
      !>     NAMES
      !>     DATLST
      !>     DPPOOL
      !>     DPVALS
      !>     CHPOOL
      !>     CHVALS
      !>
      !>     VARNAM     is the name of the variable. VARNAM is blank if
      !>                no variable is read.
      !>
      !>     EOF        is true when the end of the internal buffer has been
      !>                reached, and is false otherwise.
      !>
      !>## Parameters
      !>
      !>      LINLEN      is the maximum length of a line in the buffer.
      !>
      !>      MAXLEN      is the maximum length of the variable names that
      !>                  can be stored in the kernel pool (defined in pool.f).
      !>
      !>## Exceptions
      !>
      !>     1) The error 'SPICE(BADTIMESPEC)' is signaled if a value
      !>        beginning with '@' cannot be parsed as a time.
      !>
      !>     2) The error 'SPICE(BADVARASSIGN)' is signaled if variable
      !>        assignment does not have the form NAME = [(] value [ value ) ].
      !>
      !>     3) The error 'SPICE(KERNELPOOLFULL)' is signaled if there is
      !>        no room left in the kernel pool to store another variable
      !>        or value.
      !>
      !>     4) The error 'SPICE(NONPRINTINGCHAR)' is signaled if the name
      !>        in a variable assignment contains a non-printing character.
      !>
      !>     5) The error 'SPICE(NUMBEREXPECTED)' is signaled if a value
      !>        that is unquoted cannot be parsed as time or number.
      !>
      !>     6) The error 'SPICE(TYPEMISMATCH)' is signaled if a variable
      !>        has a first value of one type (numeric or character) and
      !>        a subsequent component has the other type.
      !>
      !>     7) The error 'SPICE(BADVARNAME)' signals if a kernel pool
      !>        variable name length exceeds MAXLEN.
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
      !>     See POOL (entry point LMPOOL).
      !>
      !>## Restrictions
      !>
      !>     The input buffer should be no more than 132 characters in width.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.1.0, 09-FEB-2010 (EDW)
      !>
      !>        Added an error check on the length of the kernel pool variable
      !>        names read from BUFFER.
      !>
      !>-    SPICELIB Version 1.0.0, 29-MAR-1999 (WLT)
      module subroutine zzrvbf ( ctx, buffer, bsize, linnum, ps, varnam, eof )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_strings, only: lastpc, nparsd, rtrim
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: buffer     (  * )  !! Array of text to be read and parsed.
      integer(ip), intent(in) :: bsize  !! Number of lines in text buffer.
      integer(ip), intent(inout) :: linnum  !! line number to begin reading buffer
      type(pool_state), intent(inout) :: ps  !! the kernel pool
      character(:), allocatable, intent(out) :: varnam  !! name of variable parsed
      logical, intent(out) :: eof  !! if TRUE end of input file has been reached.



!     The size of the kernel pool's hash table (MAXVAR in POOL).
!

!
!     The next collection of variables are set up in first pass
!     through this routine.  They would be parameters if FORTRAN
!     allowed us to do this in a standard way.
!

      character(linlen) :: line
      character(linlen) :: cval  ! PEPPER: a string value is no longer than its line
      integer(ip) :: ncval
      character(linlen) :: name

      character(256) :: error

      real(wp) :: dvalue

      integer(ip) :: at
      integer(ip) :: b
      integer(ip) :: badat
      integer(ip) :: begs(linlen)
      integer(ip) :: chnode
      integer(ip) :: code
      integer(ip) :: count
      integer(ip) :: datahd
      integer(ip) :: dirctv
      integer(ip) :: dpnode
      integer(ip) :: e
      integer(ip) :: ends(linlen)
      integer(ip) :: free
      integer(ip) :: head
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: lookat
      integer(ip) :: nameat
      integer(ip) :: ncomp
      integer(ip) :: node
      integer(ip) :: nxttok
      integer(ip) :: r1
      integer(ip) :: r2
      integer(ip) :: status
      integer(ip) :: tail
      integer(ip) :: type(linlen)
      integer(ip) :: varlen
      integer(ip) :: vartyp

!
!     The logicals below are used to take apart the tokens in an
!     input line.
!
      logical :: even
      logical :: intokn
      logical :: inquot
      logical :: insepf

      logical :: full
      logical :: found

!
!     The following logicals are in-line functions that are used
!     when processing the input strings.
!
      logical :: issep
      logical :: isquot
      logical :: isequ
      logical :: isrpar
      logical :: islpar
      logical :: isplus
      logical :: isbad
      logical :: istime

!
!     Constants (SPICELIB computed them on the first call).
!

!
!     Below are a collection of In-line function definitions that are
!     intended to make the code a bit easier to write and read.
!

      issep ( code ) =      code == iblank &
                       .or. code == icomma &
                       .or. code == ilparn &
                       .or. code == irparn &
                       .or. code == iequal &
                       .or. code == itab

      isquot( code ) =      code == iquote
      isequ ( code ) =      code == iequal
      isrpar( code ) =      code == irparn
      islpar( code ) =      code == ilparn
      isplus( code ) =      code == iplus
      istime( code ) =      code == itmark

      isbad ( dirctv ) =       dirctv /= eq &
                         .and. dirctv /= eqp

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Initializations.
!

!
!     No variable yet and no parsing errors so far.
!
      varnam = ' '
      error = ' '
      ncomp = 0

!
!     Get the next data line. Unless something is terribly wrong,
!     this will begin a new variable definition. We have to read
!     the whole variable, unless we get an error, in which case
!     we can quit.
!
      status = begin

      do while ( ( status /= done ) .and. ( .not. ctx%failed() ) )

         line = ' '
!
!        We need to skip blank lines...
!
         do while ( line == ' ' )
            eof    = linnum > bsize

            if ( eof ) return

            line   = buffer(linnum )
            linnum = linnum + 1
         end do

!
!        Find the "tokens" in the input line. As you scan from left
!        to right along the line, exactly one of the following
!        conditions is true.
!
!        1) You are in a separator field
!        4) You are in a quoted substring
!        5) You are in a non-quoted substring that isn't a separator
!           field.
!
!        Stuff between separator fields are regarded as tokens.  Note
!        this includes quoted strings.
!
!        In addition we keep track of 3 separators: '=', '(', ')'
!        Finally, whenever we encounters the separator '=', we back
!        up and see if it is preceded by a '+', if so we attach
!        it to the '=' and treat the pair of characters as a single
!        separator.
!

         even   = .true.
         intokn = .false.
         inquot = .false.
         insepf = .true.

         count  =  0
         i      =  0

         do while ( i < len(line) )
!
!           The current character is either a separator, quote or
!           some other character.
!
            i    = i + 1
            code = ichar(line(i:i))

            if ( issep(code) ) then
!
!              There are 3 possible states we could be in
!                 Separation Field
!                 A quoted substring with the last quote an odd one.
!                 A quoted substring with the last quote an even one.
!                 A non-quoted token.
!              In the first two cases nothing changes, but in the
!              next two cases we transition to a separation field.
!
               if ( intokn .or. ( inquot .and. even ) ) then
                  inquot = .false.
                  intokn = .false.
                  insepf = .true.
               end if

               if ( insepf ) then
!
!                 We need to see if this is one of the special
!                 separators
!
                  if      ( isequ(code)  ) then

                     count       = count + 1
                     begs(count) = i
                     type(count) = eq
                     ends(count) = i

                     if ( i > 1 ) then
!
!                       Look back at the previous character.
!                       See if it is a plus character.
!
                        code = ichar( line(i-1:i-1) )

                        if ( isplus(code) ) then
!
!                          This is the directive '+=' we need
!                          to set the beginning of this token
!                          to the one before this and adjust
!                          the end of the last token.
!
                           type(count) = eqp
                           begs(count) = i-1

                           if (      begs(count-1) &
                                == ends(count-1) ) then

                              count       = count - 1
                              begs(count) = i - 1
                              ends(count) = i
                              type(count) = eqp

                           else

                              ends(count-1) = ends(count-1) - 1

                           end if

                        end if

                     end if

                  else if ( isrpar(code) ) then

                     count       = count + 1
                     begs(count) = i
                     ends(count) = i
                     type(count) = ev

                  else if ( islpar(code) ) then

                     count       = count + 1
                     begs(count) = i
                     ends(count) = i
                     type(count) = bv

                  end if

               end if

            else if ( isquot(code) ) then
!
!              There are 3 cases of interest.
!                 We are in a quoted substring already
!                 We are in a separator field
!                 We are in a non-quoted token.
!              In the first case nothing changes.  In the second
!              two cases we change to being in a quoted substring.
!
               even = .not. even

               if ( .not. inquot ) then

                  insepf      = .false.
                  intokn      = .false.
                  inquot      = .true.
                  count       =  count + 1
                  begs(count) =  i
                  type(count) =  q

               end if

               ends(count) = i

            else
!
!              This is some character other than a quote, or
!              separator character.
!
!              We are in one of four situations.
!
!                 1) We are in a quoted substring with an odd number of
!                    quotes.
!                 2) We are in a quoted substring with an even number of
!                    quotes.
!                 2) We are in a separator field
!                 3) We are in a non-quoted token.
!
!              In cases 1 and 3 nothing changes. So we won't check
!              those cases.
!

               if ( insepf .or. ( inquot .and. even ) ) then
                  inquot      = .false.
                  insepf      = .false.
                  intokn      = .true.
                  count       =  count + 1
                  begs(count) =  i
                  type(count) =  nq
               end if

               ends(count) = i

            end if

         end do

!
!        The first word on the first line should be the name of a
!        variable. The second word should be a directive: = or +=.
!
         if ( status == begin ) then
!
!           There must be at least 3 contributing tokens on this line.
!
            if ( count < 3 ) then

               call ctx%setmsg ( 'A kernel variable was not properly ' &
               //            'formed on line # text buffer.' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''. This line was ''#''. ' )

               r2 = rtrim(line)

               call ctx%errint ( '#', linnum     )
               call ctx%errch  ( '#', line(1:r2) )
               call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
               return

            end if
!
!           See if the variable name is legitimate:
!
            badat = lastpc(line(begs(1):ends(1)))

            if ( badat <= ends(1) - begs(1) ) then
!
!              There is a non-printing character in the variable
!              name.  This isn't allowed.
!
               at = begs(1) + badat

               call ctx%setmsg ( 'There is a non-printing character ' &
               //            'embedded in line # of the text ' &
               //            'buffer.  Non-printing ' &
               //            'characters are not allowed in ' &
               //            'kernel variable assignments.  The ' &
               //            'non-printing character has ASCII ' &
               //            'code #. ' )

               call ctx%errint ( '#', linnum             )
               call ctx%errint ( '#', ichar(line(at:at)) )
               call ctx%sigerr ( 'SPICE(NONPRINTINGCHAR)')
               return
            end if

!
!           Check the variable name length; signal an error
!           if longer than MAXLEN.
!
            varlen = len( line( begs(1): ends(1) ) )

            ! PEPPER: names have any length (SPICELIB signaled
            ! SPICE(BADVARNAME) for more than 32 characters).

!
!           The variable name is ok. How about the directive.
!
            varnam = line(begs(1):ends(1))
            dirctv = type(2)

!
!           If this is replacement (=) and not an addition (+=),
!           delete the values currently associated with the variable.
!           They will be replaced later.
!
            if ( isbad(dirctv) ) then

               call ctx%setmsg ( 'A kernel variable was not properly ' &
               //            'formed on line # of the text buffer. ' &
               //            'Such an assignment should have the ' &
               //            'form: ''<variable name> [+]= ' &
               //            '<values>''.  More specifically, ' &
               //            'the assignment operator did not have ' &
               //            'one of the expected forms: ''='' or ' &
               //            '''+=''. The line was ''#''. ' )

               r2 = rtrim(line)

               call ctx%errint ( '#', linnum     )
               call ctx%errch  ( '#', line(1:r2) )
               call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
               return

            end if
!
!           Locate this variable name in the name pool or insert it
!           if it isn't there.  The location will be NAMEAT and
!           we will use the variable FOUND to indicate whether or
!           not it was already present.
!
            call ps%create ( varnam, nameat, found )

!
!           Now depending upon the kind of directive, we will need
!           to remove data and allocate a new list or simply append
!           data to the existing list.
!
            if ( dirctv == eq ) then
!
!              We are going to dump whatever is associated with
!              this name and then we will need to allocate a new
!              linked list for the data.
!
               vartyp =  unknwn

               if ( found ) call ps%clear_values ( nameat )

            else if ( dirctv == eqp ) then
!
!              We need to append to the current variable.
!
               if ( found ) then

                  if ( ps%vars(nameat)%kind == pool_numeric ) then
                     vartyp = numtyp
                  else if ( ps%vars(nameat)%kind == pool_character ) then
                     vartyp = strtyp
                  else
                     vartyp = unknwn
                  end if

               else
                  vartyp = unknwn
               end if

            end if

            ! PEPPER: the source of the assignment (the buffer of LMPOOL, and its line)
            if ( allocated ( ps%setter ) ) then
               call ps%note_source ( nameat, ps%setter, linnum - 1 )
            else
               call ps%note_source ( nameat, '(LMPOOL)', linnum - 1 )
            end if

!
!           If this is a vector, the next thing on the line will be a
!           left parenthesis. Otherwise, assume that this is a scalar.
!           If it's a vector, get the first value. If it's a scalar,
!           plant a bogus right parenthesis, to make the following loop
!           terminate after one iteration.
!

            if ( type(3) == bv ) then
               nxttok        = 4
            else
               nxttok      = 3
               count       = count + 1
               type(count) = ev
            end if

!
!        For subsequent lines, treat everything as a new value.
!
         else

            nxttok = 1

         end if

!
!        We have a value anyway. Store it in the table.
!
!        Keep going until the other shoe (the right parenthesis)
!        drops, or until the end of the line is reached.
!
!        Dates begin with @; anything else is presumed to be a number.
!
         do while ( nxttok <= count )
            if ( type(nxttok) == ev ) exit
!
!           Get the begin and end of this token.
!
            b = begs(nxttok)
            e = ends(nxttok)

            if ( vartyp == unknwn ) then
!
!              We need to determine which category of variable we
!              have by looking at this token and deducing the
!              type.
!
               if      ( type(nxttok) == q  ) then

                  vartyp = strtyp

               else if ( type(nxttok) == nq ) then

                  vartyp = numtyp

               else
!
!                 This is an error. We should have had one of the
!                 two previous types.
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  call ctx%setmsg ( 'The first item following the ' &
                  //            'assignment operator should be ' &
                  //            'the value of a variable or a ' &
                  //            'left parenthesis ''('' followed ' &
                  //            'by a value for a variable. This ' &
                  //            'is not true on line # of the ' &
                  //            'text buffer. ' )

                  call ctx%errint ( '#', linnum     )
                  call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
                  return

               end if

            end if

            if ( vartyp == strtyp ) then
!
!              First make sure that this token represents a string.
!
               if ( type(nxttok) /= q ) then
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  r1 = rtrim  ( varnam        )

                  call ctx%setmsg ( 'The kernel variable # has ' &
                  //            'been set up as a string ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the text buffer is ' &
                  //            'not a string value. ' )

                  call ctx%errch  ( '#', varnam(1:r1) )
                  call ctx%errint ( '#', linnum       )
                  call ctx%sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if
!
!              Still going? Make sure there is something between
!              the quotes.
!
               if ( b+1 >= e ) then
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  call ctx%setmsg ( 'There is a quoted string with ' &
                  //            'no characters on line # of the ' &
                  //            'text buffer. ' )

                  call ctx%errint ( '#', linnum       )
                  call ctx%sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if

!
!              We are ready to go.  Allocate a node for this data
!              item. First make sure there is room to do so.
!
               ncomp          = ncomp + 1

               ncval = 0
               j = b+1
               do while ( j < e )
                  code = ichar(line(j:j))
                  if ( isquot(code) ) then
                     j = j+1
                  end if
                  ncval = ncval + 1
                  cval(ncval:ncval) = line(j:j)
                  j = j + 1
               end do
               call ps%add_string ( nameat, cval(1:ncval) )
!
!              That's all for this value. It's now time to loop
!              back through and get the next value.
!
            else

               if ( type(nxttok) /= nq ) then
!
!                 First perform the clean up function.
!
                  call ps%remove ( nameat )

                  r1 = rtrim  ( varnam        )

                  call ctx%setmsg ( 'The kernel variable # has ' &
                  //            'been set up as a numeric or time ' &
                  //            'variable.  However, the value ' &
                  //            'that you are attempting to ' &
                  //            'assign to this variable on line ' &
                  //            '# of the kernel buffer is ' &
                  //            'not a numeric or time value. ' )

                  call ctx%errch  ( '#', varnam(1:r1) )
                  call ctx%errint ( '#', linnum       )
                  call ctx%sigerr ( 'SPICE(TYPEMISMATCH)' )
                  return

               end if
!
!              Look at the first character to see if we have a time
!              or a number.
!
               code = ichar( line(b:b) )

               if ( istime(code) ) then
!
!                 We need to have more than a single character.
!
                  if ( e == b ) then
!
!                    First perform the clean up function.
!
                     call ps%remove ( nameat )
                     r1 = rtrim (varnam        )

                     call ctx%setmsg ( 'At character # of  line # in ' &
                     //            'the text buffer the ' &
                     //            'character ''@'' appears.  This ' &
                     //            'character is reserved for ' &
                     //            'identifying time values in ' &
                     //            'assignments to kernel pool ' &
                     //            'variables.  However it is not ' &
                     //            'being used in this fashion for ' &
                     //            'the variable ''#''. ' )

                     call ctx%errint ( '#', b            )
                     call ctx%errint ( '#', linnum       )
                     call ctx%errch  ( '#', varnam(1:r1) )
                     call ctx%sigerr ( 'SPICE(BADTIMESPEC)' )
                     return

                  end if

                  call tparse ( ctx, line(b+1:e), dvalue, error )

                  if ( error /= ' ' ) then
!
!                    First perform the clean up function.
!
                     call ps%remove ( nameat )

                     call ctx%setmsg ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a time ' &
                     //            'on line # of the text buffer. ' )

                     call ctx%errch  ( '#', line(b+1:e) )
                     call ctx%errint ( '#', linnum      )
                     call ctx%sigerr ( 'SPICE(BADTIMESPEC)' )
                     return
                  end if

               else

                  call nparsd ( line(b:e), dvalue, error, i )

                  if ( error /= ' ' ) then

                     call ps%remove ( nameat )

                     call ctx%setmsg ( 'Encountered ''#'' while ' &
                     //            'attempting to parse a number on ' &
                     //            'line # of the text buffer' )
                     call ctx%errch  ( '#',  line(b:e) )
                     call ctx%errint ( '#',  linnum    )
                     call ctx%sigerr ( 'SPICE(NUMBEREXPECTED)' )
                     return
                  end if

               end if
!
!              OK. We have a parsed value.  See if there is room in
!              the numeric portion of the pool to store this value.
!
               call ps%add_number ( nameat, dvalue )
               ncomp          = ncomp + 1

            end if

!
!           Now process the next token in the list of tokens.
!
            nxttok = nxttok + 1

         end do
!
!        We could have ended the above loop in one of two ways.
!
!        1) NXTTOK now exceeds count.  This means we did not reach
!           an end of vector marker.
!        2) We hit an end of vector marker.
!
         if ( nxttok > count ) then
            status = invar
         else
            status = done
         end if

      end do

!
!     It is possible that we reached this point without actually
!     assigning a value to the kernel pool variable.  This can
!     happen if there is a vector input of the form NAME = ( )
!
      if ( ncomp < 1 ) then

         call ps%remove ( nameat )

         call ctx%setmsg ( 'The first item following the ' &
         //            'assignment operator should be ' &
         //            'the value of a variable or a ' &
         //            'left parenthesis ''('' followed ' &
         //            'by a value for a variable. This ' &
         //            'is not true on line # of the ' &
         //            'text buffer. ' )

         call ctx%errint ( '#', linnum-1   )
         call ctx%sigerr ( 'SPICE(BADVARASSIGN)' )
         return

      end if

!
!     Return the name of the variable.
!

      return
      end subroutine zzrvbf

      !> Remove all kernel variables from the kernel pool. Watches
      !> on kernel variables are retained.
      !>
      !>## Detailed Input
      !>
      !>     None.
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
      !>     Error free.
      !>
      !>     1)  All known agents (those established through the SPICELIB
      !>         routine SWPOOL) will be "notified" that their watched
      !>         variables have been updated whenever CLPOOL is called.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     CLPOOL clears the pool of kernel variables maintained by
      !>     the kernel POOL subsystem. All the variables in the pool are
      !>     deleted. However, all watcher information is retained.
      !>
      !>     Each watched variable will be regarded as having been updated.
      !>     Any agent associated with that variable will have a notice
      !>     posted for it indicating that its watched variable has been
      !>     updated.
      !>
      !>     Application programs can delete watches by calling the SPICELIB
      !>     routine DWPOOL. See the header of that routine for details.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) This code example demonstrates how to assign values to kernel
      !>        pool variables, how to check for the existence of kernel pool
      !>        variables and how to clear the kernel pool, i.e. how to delete
      !>        all variable assignments loaded into the kernel pool.
      !>
      !>        Place a value into the kernel pool and check for the variable
      !>        to which the value has been assigned. Clear the kernel pool
      !>        and check for that variable again.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CLPOOL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables
      !>C
      !>      DOUBLE PRECISION      DVALS ( 1 )
      !>
      !>      INTEGER               N
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Place a value into the kernel pool. Recall the routines
      !>C     for direct insertion of pool assignments have arrays for
      !>C     input.
      !>C
      !>      DVALS( 1 ) = -666.D0
      !>      CALL PDPOOL ( 'TEST_VAR', 1, DVALS )
      !>
      !>C
      !>C     Check for the variable assignment to TEST_VAR.
      !>C
      !>      CALL GDPOOL ( 'TEST_VAR', 1, 1, N, DVALS, FOUND )
      !>
      !>      WRITE(*,'(A)') 'First call to GDPOOL:'
      !>
      !>      IF ( FOUND ) THEN
      !>
      !>         WRITE(*,'(A,F8.2)') '   TEST_VAR value:', DVALS(1)
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(A)') '   TEST_VAR not in kernel pool.'
      !>
      !>      END IF
      !>
      !>C
      !>C     Now clear the kernel pool.
      !>C
      !>      CALL CLPOOL ()
      !>
      !>C
      !>C     Again, check for the TEST_VAR assignment.
      !>C
      !>      CALL GDPOOL ( 'TEST_VAR', 1, 1, N, DVALS, FOUND )
      !>
      !>      WRITE(*,'(A)') 'Second call to GDPOOL:'
      !>
      !>      IF ( FOUND ) THEN
      !>
      !>         WRITE(*,'(A,F6.2)') '   TEST_VAR value:', DVALS(1)
      !>
      !>      ELSE
      !>
      !>         WRITE(*,'(A)') '   TEST_VAR not in kernel pool.'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        First call to GDPOOL:
      !>           TEST_VAR value: -666.00
      !>        Second call to GDPOOL:
      !>           TEST_VAR not in kernel pool.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine should not be used to unload kernels that
      !>         have been loaded via FURNSH.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.2.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Routine declared error free.
      !>
      !>-    SPICELIB Version 8.2.0, 01-JUL-2014 (NJB) (BVS)
      !>
      !>        Description of behavior of watcher subsystem was expanded.
      !>
      !>        Last update was 30-JUL-2013 (BVS)
      !>
      !>           Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation. Updated $Restrictions
      !>        header section. Updated code example to use TXTOPN.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        This entry point clears the string valued variables as well as
      !>        the numeric valued variables.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL and CVPOOL were added.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.2.0, 01-JUL-2014 (NJB) (BVS)
      !>
      !>        Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>        ZZNWPOOL is called to update the list of agents
      !>        to notify of watched variable updates.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        This entry point clears the string valued variables as well as
      !>        the numeric valued variables.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL (set watch on a pool variable)
      !>        and CVPOOL (check variable for update) so that routines
      !>        that buffer data stored in the kernel pool can fetch
      !>        that data only when it is updated.
      !>
      !>        Also the control of initializations was modified to be
      !>        consistent with other SPICELIB practices.
      !>
      !>        Finally, the revision history was upgraded so that the
      !>        version number increases over time. This wasn't true
      !>        before. In addition some early revision data that referred to
      !>        pre-SPICELIB modifications were removed. This editing of
      !>        the version numbers makes it unlikely that anyone can track
      !>        down which previous version of this routine they have by
      !>        looking at the version number. The best way to determine
      !>        the routine you had previously is to compare the dates
      !>        stored in the Version line of the routine.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
      module subroutine clpool ( ctx )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      call ps%clear ( )
      call pool_changed ( ctx%spice, ' ' )
      end associate
!
!     PEPPER: the programmatic frames stay (they are registrations, as
!     loaded files are): their definitions are written into the emptied
!     pool.
!
      call write_provided_frames ( ctx )
      end subroutine clpool

      !> Load the variables contained in a NAIF ASCII kernel file into the
      !> kernel pool. Lines have at most 132 characters; a longer line is
      !> cut without an error (see Restrictions).
      !>
      !> PEPPER: a file named `*.json` (in any case) is a JSON kernel (see
      !> [[pepper_json]]), loaded atomically: on an error nothing in it is
      !> loaded.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of the text kernel file whose variables will
      !>              be loaded into the pool.
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
      !>     1)  If an I/O error occurs while opening or reading a text kernel,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If any text kernel parsing error occurs, the error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     3)  If a kernel pool overflow is detected, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See FNAME in $Detailed_Input.
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
      !>     1) The following program demonstrates how to load the variables
      !>        contained in a NAIF ASCII kernel file into the kernel pool
      !>        and how to determine the properties of a stored kernel
      !>        variable.
      !>
      !>        The program prompts for text kernel name and for the name of
      !>        a kernel variable. If the variable is present in the kernel
      !>        pool, the dimension and type of the variable are displayed.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM LDPOOL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local constants
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 256 )
      !>
      !>      INTEGER               KVNMLN
      !>      PARAMETER           ( KVNMLN = 33 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>      CHARACTER*(KVNMLN)    VARNAM
      !>      CHARACTER*(1)         VTYPE
      !>
      !>      INTEGER               N
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of a text-kernel file.
      !>C
      !>      CALL PROMPT ( 'Enter text-kernel name        > ', FNAME )
      !>
      !>C
      !>C     Load the kernel. The same operation could be done using
      !>C     a FURNSH call.
      !>C
      !>      CALL LDPOOL ( FNAME )
      !>
      !>      CALL PROMPT ( 'Enter name of kernel variable > ',
      !>     .               VARNAM )
      !>
      !>      CALL DTPOOL ( VARNAM, FOUND, N, VTYPE )
      !>
      !>      IF ( FOUND ) THEN
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) 'Properties of variable ',
      !>     .               VARNAM(:RTRIM(VARNAM)), ':'
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) '   Size:   ', N
      !>
      !>         IF ( VTYPE .EQ. 'C' ) THEN
      !>
      !>            WRITE(*,*) '   Type:   Character'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) '   Type:   Numeric'
      !>         END IF
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) VARNAM,
      !>     .              ' is not present in the kernel pool.'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the PCK file gm_de431.tpc to ask for the
      !>        variable 'BODY000_GMLIST', the output was:
      !>
      !>        Enter text-kernel name        > gm_de431.tpc
      !>        Enter name of kernel variable > BODY000_GMLIST
      !>
      !>         Properties of variable BODY000_GMLIST:
      !>
      !>            Size:             65
      !>            Type:   Numeric
      !>
      !>## Restrictions
      !>
      !>     PEPPER: a line of a text kernel or meta-kernel may have at most
      !>     132 characters (NAIF's text kernel rule). A longer line is cut to
      !>     132 characters without an error, as in SPICELIB: a value or name
      !>     that continues past column 132 is lost or misread. Split long
      !>     values over several lines.
      !>
      !>     1)  Normally SPICE applications should load kernels via the
      !>         FURNSH routine.
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
      !>     R.E. Thurman       (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.3.0, 17-AUG-2021 (JDR)
      !>
      !>        Changed input argument name KERNEL to FNAME for consistency
      !>        with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 8.2.0, 30-JUL-2013 (BVS)
      !>
      !>        Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>        Filled out $Exceptions section of header, which previously
      !>        contained only the word "None."
      !>
      !>        Updated code example to use TXTOPN.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        In addition much greater error checking is performed on
      !>        the input file to guarantee valid inputs.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL and CVPOOL were added.
      !>
      !>-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
      !>
      !>        Increased value of parameter MAXVAL to 5000 to accommodate
      !>        storage of SCLK coefficients in the kernel pool.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
      !>
      !>        Added declaration of FAILED. FAILED is checked in the
      !>        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
      !>
      !>-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
      !>
      !>        A FAILED test was inserted into the control of the DO-loop
      !>        which reads in each kernel variable in LDPOOL.
      !>
      !>-    SPICELIB Version 1.2.0, 09-MAR-1989 (HAN)
      !>
      !>        Parameters BEGDAT and BEGTXT have been moved into the
      !>        $Declarations section.
      !>
      !>-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
      !>
      !>        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
      !>        (Actually, MAXLEN was implicitly 32 characters, and has only
      !>        now been made an explicit---and changeable---limit.)
      !>
      !>        Declaration of unused function FAILED removed.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      !>
      !>        The entry point RTPOOL should now be regarded as obsolete
      !>        and is maintained solely for backward compatibility with
      !>        existing routines that make use of it.
      !>
      !>        The basic data structure used to maintain the list of
      !>        variable names and values was replaced with a hash table
      !>        implementation. Data and names are accessed by means
      !>        of a hash function and linked lists of pointers to existing
      !>        variable names and data values.
      !>
      !>        In addition much greater error checking is performed on
      !>        the input file to guarantee valid inputs.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL (set watch on a pool variable)
      !>        and CVPOOL (check variable for update) so that routines
      !>        that buffer data stored in the kernel pool can fetch
      !>        that data only when it is updated.
      !>
      !>        In addition, the revision history was upgraded so that the
      !>        version number increases over time. This wasn't true
      !>        before. In addition some early revision data that referred to
      !>        pre-SPICELIB modifications were removed. This editing of
      !>        the version numbers makes it unlikely that anyone can track
      !>        down which previous version of this routine they have by
      !>        looking at the version number. The best way to determine
      !>        the routine you had previously is to compare the dates
      !>        stored in the Version line of the routine.
      !>
      !>-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
      !>
      !>        Increased value of parameter MAXVAL to 5000 to accommodate
      !>        storage of SCLK coefficients in the kernel pool.
      !>
      !>        Also, changed version number in previous revisions entry
      !>        from SPICELIB Version 2.0.0 to SPICELIB Version 2.0.0. The
      !>        last version entry in the $Version section had been
      !>        Version 1.0.0, dated later than the entry for `version 2'
      !>        in the $Revisions section!
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
      !>
      !>        Added declaration of FAILED. FAILED is checked in the
      !>        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
      !>
      !>-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
      !>
      !>       A FAILED test was inserted into the control of the DO-loop which
      !>       reads in each kernel variable.
      !>
      !>       Previously, if the error action 'RETURN' had been set by a
      !>       calling program, and the call to RDKNEW by LDPOOL failed,
      !>       then execution would continue through LDPOOL, with SPICELIB
      !>       routines returning upon entry. This meant that the routine
      !>       RDKVAR never got a chance to set the EOF flag, which was the
      !>       only control of the DO-loop. An infinite loop resulted in such
      !>       cases. The FAILED test resolves that situation.
      !>
      !>-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
      !>
      !>        Parameters BEGDAT and BEGTXT have been moved into the
      !>        $Declarations section.
      !>
      !>-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
      !>
      !>        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
      !>        (Actually, MAXLEN was implicitly 32 characters, and has only
      !>        now been made an explicit---and changeable---limit.)
      !>
      !>        Declaration of unused function FAILED removed.
      !>
      !>-    SPICELIB Version 1.0.0, 8-JAN-1989 (IMU)
      module subroutine ldpool ( ctx, fname )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the text kernel file.
      character(:), allocatable :: varnam
      logical :: eof
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
!
!     PEPPER: a file named *.json is a JSON kernel (see pepper_json).
!
      if ( is_json_kernel ( fname ) ) then
         call ldjson ( ctx, ps, fname )
      else
         call rdknew ( ctx, fname )
         call zzrvar ( ctx, ps, varnam, eof )
         do while (  ( .not. eof ) .and. (.not. ctx%failed() )   )
            if ( varnam /= ' ' ) then
               call pool_changed ( ctx%spice, varnam )
            end if
            call zzrvar ( ctx, ps, varnam, eof )
         end do
         call cltext ( ctx,  fname  )
      end if
      end associate
      end subroutine ldpool

      !> Load an in-memory text or JSON kernel string into the kernel pool
      !> without touching the filesystem (PEPPER).
      module subroutine ldpool_text ( ctx, fname, text )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Virtual name of the text or JSON kernel.
      character(*), intent(in) :: text  !! Kernel contents.
      integer(ip), parameter :: pool_linlen = 132
      character(pool_linlen) :: line
      character(pool_linlen), allocatable :: cvals(:)
      integer :: ntext, pos_cur, pos_nl, pos_end, pass, idx
      integer(ip) :: nlines
      logical :: in_data
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      if ( is_json_kernel ( fname ) ) then
         call ldjson_text ( ctx, ps, fname, text )
         return
      end if
      ntext = len ( text )
      nlines = 0_ip
      do pass = 1, 2
         if ( pass == 2 ) then
            if ( nlines == 0_ip ) return
            allocate ( cvals(nlines) )
            nlines = 0_ip
         end if
         in_data = .false.
         pos_cur = 1
         do while ( pos_cur <= ntext )
            pos_nl = index ( text(pos_cur:ntext), achar(10) )
            if ( pos_nl == 0 ) then
               pos_end = ntext
               pos_nl  = ntext + 1
            else
               pos_end = pos_cur + pos_nl - 2
               pos_nl  = pos_cur + pos_nl
            end if
            if ( pos_end >= pos_cur ) then
               if ( text(pos_end:pos_end) == achar(13) ) pos_end = pos_end - 1
            end if
            line = ' '
            if ( pos_end >= pos_cur ) then
               line = text(pos_cur : min(pos_end, pos_cur + pool_linlen - 1))
            end if
            do idx = 1, len_trim ( line )
               if ( ichar(line(idx:idx)) == 9 ) line(idx:idx) = ' '
            end do
            line = adjustl ( line )
            if ( line == '\begintext' ) then
               in_data = .false.
            else if ( line == '\begindata' ) then
               in_data = .true.
            else if ( in_data .and. line /= ' ' ) then
               nlines = nlines + 1_ip
               if ( pass == 2 ) cvals(nlines) = line
            end if
            pos_cur = pos_nl
         end do
      end do
      end associate
      ctx%spice%pool%setter = trim ( fname )
      call lmpool ( ctx, cvals, nlines )
      if ( allocated ( ctx%spice%pool%setter ) ) deallocate ( ctx%spice%pool%setter )
      end subroutine ldpool_text

      !> Return the value of a kernel variable from the kernel pool.
      !>
      !> This routine is maintained only for backward compatibility.
      !> It should be regarded as obsolete. Use one of the entry points
      !> GDPOOL, GIPOOL or GCPOOL in its place.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the variable whose values are to be
      !>              returned. If the variable is not in the pool, FOUND
      !>              will be .FALSE.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of values associated with NAME.
      !>              If NAME is not in the pool, no value is given to
      !>              N.
      !>
      !>     VALUES   is the array of values associated with NAME.
      !>              If NAME is not in the pool, no values are given to
      !>              the elements of VALUES.
      !>
      !>     FOUND    is .TRUE. if the variable is in the pool, .FALSE. if it
      !>              is not.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the output argument VALUES is not large enough to hold all
      !>         of the values of the kernel variable designated by NAME,
      !>         then memory will be corrupted. RTPOOL cannot detect this
      !>         error.
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
      !>     The following code fragment demonstrates how the data from
      !>     several kernel files can be loaded into a kernel pool. After the
      !>     pool is loaded, the values in the pool are written to a kernel
      !>     file.
      !>
      !>     C
      !>     C     Store in an array the names of the kernel files whose
      !>     C     values will be loaded into the kernel pool.
      !>     C
      !>           FNAME (1) = 'AXES.KER'
      !>           FNAME (2) = 'GM.KER'
      !>           FNAME (3) = 'LEAP_SECONDS.KER'
      !>
      !>     C
      !>     C     Clear the kernel pool. (This is optional.)
      !>     C
      !>           CALL CLPOOL
      !>
      !>     C
      !>     C     Load the variables from the three kernel files into the
      !>     C     the kernel pool.
      !>     C
      !>           DO I = 1, 3
      !>             CALL LDPOOL ( FNAME (I) )
      !>           END DO
      !>
      !>     C
      !>     C     We can examine the values associated with any variable
      !>     C     in the kernel pool using RTPOOL.
      !>     C
      !>           CALL RTPOOL ( VARIABLE, NUMVAL, VALUES, FOUND )
      !>
      !>     C
      !>     C     Open the new text file 'NEWKERNEL.KER'.
      !>     C
      !>           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
      !>
      !>     C
      !>     C     Write the values in the kernel pool to the file.
      !>     C
      !>           CALL WRPOOL ( UNIT )
      !>
      !>## Restrictions
      !>
      !>     See $Exceptions section.
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
      !>-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>        Updated code example to use TXTOPN.
      !>
      !>-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      !>
      !>        The entry point RTPOOL should now be regarded as obsolete
      !>        and is maintained solely for backward compatibility with
      !>        existing routines that make use of it.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
      module subroutine rtpool ( ctx, name, n, values, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
      integer(ip), intent(out) :: n  !! Number of values associated with NAME.
      real(wp), intent(out) :: values    ( * )  !! Values associated with NAME.
      logical, intent(out) :: found  !! .TRUE. if variable is in pool.
      integer(ip) :: i
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      i     = ps%find ( name )
      found = .false.
      if ( i == 0 ) return
      if ( ps%vars(i)%kind /= pool_numeric ) return
      found = .true.
      n     = ps%vars(i)%n
      values(1:n) = ps%vars(i)%dvals(1:n)
      end associate
      end subroutine rtpool

      !> Confirm the existence of a numeric kernel variable in the kernel
      !> pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the numeric kernel variable whose
      !>              existence in the kernel pool is to be checked.
      !>
      !>## Detailed Output
      !>
      !>     FOUND    is .TRUE. whenever the specified variable is included
      !>              in the pool.
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
      !>     This routine determines whether or not a numeric kernel pool
      !>     variable exists. It does not detect the existence of
      !>     string valued kernel pool variables.
      !>
      !>     A better routine for determining the existence of numeric kernel
      !>     pool variables is the routine DTPOOL which determines the
      !>     existence, size and type of kernel pool variables.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following code example demonstrates how to use EXPOOL
      !>        to confirm the existence of numeric kernel pool variables.
      !>        In the example, we will look for different variables;
      !>        some of them numeric, some string valued and some not
      !>        present in the kernel pool.
      !>
      !>        Use the kernel shown below; an IK defining two keywords
      !>        used to provide data for an instrument with NAIF ID -999001.
      !>
      !>           KPL/IK
      !>
      !>           File name: expool_ex1.ti
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
      !>      PROGRAM EXPOOL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      CHARACTER*(*)         IKNAME
      !>      PARAMETER           ( IKNAME = 'expool_ex1.ti' )
      !>
      !>      INTEGER               KPVNLN
      !>      PARAMETER           ( KPVNLN = 32 )
      !>
      !>      INTEGER               NKPVNM
      !>      PARAMETER           ( NKPVNM = 3  )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(KPVNLN)    KEYWRD ( NKPVNM )
      !>
      !>      INTEGER               I
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Define the variable names
      !>C
      !>      DATA                  KEYWRD   /
      !>     .                             'INS-999001_FREQ_RGB',
      !>     .                             'NOT_IN_THE_POOL',
      !>     .                             'INS-999001_FREQ_UNITS' /
      !>
      !>C
      !>C     Load the instrument kernel.
      !>C
      !>      CALL FURNSH ( IKNAME )
      !>
      !>      DO I = 1, NKPVNM
      !>
      !>C
      !>C        Check if the variable is numeric and present
      !>C        in the kernel pool.
      !>C
      !>         CALL EXPOOL ( KEYWRD(I), FOUND )
      !>
      !>         WRITE(*,*) 'Variable name: ', KEYWRD(I)
      !>
      !>         IF ( FOUND ) THEN
      !>
      !>            WRITE(*,*) '   It is numeric and exists in the '
      !>     .              // 'kernel pool.'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) '   Either it is not numeric or it is '
      !>     .              // 'not in the kernel pool.'
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
      !>         Variable name: INS-999001_FREQ_RGB
      !>            It is numeric and exists in the kernel pool.
      !>         Variable name: NOT_IN_THE_POOL
      !>            Either it is not numeric or it is not in the kernel pool.
      !>         Variable name: INS-999001_FREQ_UNITS
      !>            Either it is not numeric or it is not in the kernel pool.
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
      !>-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Updated the header to reflect that only numeric variables
      !>        present in the kernel pool will cause the routine to return
      !>        .TRUE.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>        Fixed typos.
      !>
      !>-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      !>
      !>        The entry point RTPOOL should now be regarded as obsolete
      !>        and is maintained solely for backward compatibility with
      !>        existing routines that make use of it.
      !>
      !>        The basic data structure used to maintain the list of
      !>        variable names and values was replaced with a hash table
      !>        implementation. Data and names are accessed by means
      !>        of a hash function and linked lists of pointers to existing
      !>        variable names and data values.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      module subroutine expool ( ctx, name, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of a numeric kernel variable.
      logical, intent(out) :: found  !! .TRUE. when the variable is in the pool.
      integer(ip) :: i
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      i     = ps%find ( name )
      found = .false.
      if ( i > 0 ) found = ps%vars(i)%kind == pool_numeric
      end associate
      end subroutine expool

      !> Write to a specified unit a set of "keyword = value" assignments
      !> for all currently defined kernel variables. The assignments
      !> constitute a text kernel from which the current state of the
      !> kernel pool can be restored.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is the logical unit to which the variables in the pool
      !>              will be written.
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
      !>     1)  If writing a variable to the output file fails due to an I/O
      !>         error, the error SPICE(WRITEERROR) is signaled.
      !>
      !>## Files
      !>
      !>     This routine writes to the specified logical unit the kernel
      !>     variables present in the kernel pool. Each variable consists of a
      !>     name and a set of associated values. The variables are written in
      !>     the form of a series of "keyword = value" assignments. The
      !>     assignments are preceded by a SPICE text kernel "\begindata"
      !>     marker.
      !>
      !>     The output of this routine, if written to a text file, is a SPICE
      !>     text kernel. The current contents of the kernel pool can be
      !>     restored by clearing the pool and then loading this text kernel.
      !>
      !>     If the values are to be written to an output kernel file, the
      !>     file should be opened with a logical unit determined by the
      !>     calling program.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     The following code fragment demonstrates how the data from
      !>     several kernel files can be loaded into a kernel pool. After the
      !>     pool is loaded, the values in the pool are written to a kernel
      !>     file.
      !>
      !>     C
      !>     C     Store in an array the names of the kernel files whose
      !>     C     values will be loaded into the kernel pool.
      !>     C
      !>           FNAME (1) = 'AXES.KER'
      !>           FNAME (2) = 'GM.KER'
      !>           FNAME (3) = 'LEAP_SECONDS.KER'
      !>
      !>     C
      !>     C     Clear the kernel pool. (This is optional.)
      !>     C
      !>           CALL CLPOOL
      !>
      !>     C
      !>     C     Load the variables from the three kernel files into the
      !>     C     the kernel pool.
      !>     C
      !>           DO I = 1, 3
      !>             CALL LDPOOL ( FNAME (I) )
      !>           END DO
      !>
      !>     C
      !>     C     We can examine the values associated with any double
      !>     C     precision variable in the kernel pool using GDPOOL.
      !>     C
      !>           CALL GDPOOL ( VARIABLE, 1, NMAX, NUMVAL, VALUES, FOUND )
      !>
      !>     C
      !>     C     Open the new text file 'NEWKERNEL.KER'.
      !>     C
      !>           CALL TXTOPN ( 'NEWKERNEL.KER', UNIT )
      !>
      !>     C
      !>     C     Write the values in the kernel pool to the file.
      !>     C
      !>           CALL WRPOOL ( UNIT )
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 17-AUG-2021 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated the $Exceptions section to document the actual error
      !>        handling of the routine.
      !>
      !>-    SPICELIB Version 8.1.1, 30-JUN-2014 (NJB)
      !>
      !>        Updated header to more accurately describe the output
      !>        of this routine.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Updated code example to use TXTOPN.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued. Both types are supported
      !>        by WRPOOL.
      !>
      !>-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
      !>
      !>        Increased value of parameter MAXVAL to 5000 to accommodate
      !>        storage of SCLK coefficients in the kernel pool.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
      !>
      !>        Added declaration of FAILED. FAILED is checked in the
      !>        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
      !>
      !>-    SPICELIB Version 1.2.0, 09-MAR-1989 (HAN)
      !>
      !>        Parameters BEGDAT and BEGTXT have been moved into the
      !>        $Declarations section.
      !>
      !>-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
      !>
      !>        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
      !>        (Actually, MAXLEN was implicitly 32 characters, and has only
      !>        now been made an explicit---and changeable---limit.)
      !>
      !>        Declaration of unused function FAILED removed.
      !>
      !>-    SPICELIB Version 1.0.0, 08-JAN-1989 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The basic data structure used to maintain the list of
      !>        variable names and values was replaced with a hash table
      !>        implementation. Data and names are accessed by means
      !>        of a hash function and linked lists of pointers to existing
      !>        variable names and data values.
      !>
      !>-    SPICELIB Version 5.0.0, 22-AUG-1990 (NJB)
      !>
      !>        Increased value of parameter MAXVAL to 5000 to accommodate
      !>        storage of SCLK coefficients in the kernel pool.
      !>
      !>-    SPICELIB Version 4.0.0, 12-JUN-1990 (IMU)
      !>
      !>        All entry points except POOL and CLPOOL now initialize the
      !>        pool if it has not been done yet.
      !>
      !>-    SPICELIB Version 3.0.0, 23-OCT-1989 (HAN)
      !>
      !>        Added declaration of FAILED. FAILED is checked in the
      !>        DO-loops in LDPOOL and WRPOOL to prevent infinite looping.
      !>
      !>-    SPICELIB Version 2.0.0, 18-OCT-1989 (RET)
      !>
      !>       A FAILED test was inserted into the control of the DO-loop which
      !>       reads in each kernel variable.
      !>
      !>       Previously, if the error action 'RETURN' had been set by a
      !>       calling program, and the call to RDKNEW by LDPOOL failed,
      !>       then execution would continue through LDPOOL, with SPICELIB
      !>       routines returning upon entry. This meant that the routine
      !>       RDKVAR never got a chance to set the EOF flag, which was the
      !>       only control of the DO-loop. An infinite loop resulted in such
      !>       cases. The FAILED test resolves that situation.
      !>
      !>-    SPICELIB Version 1.2.0, 9-MAR-1989 (HAN)
      !>
      !>        Parameters BEGDAT and BEGTXT have been moved into the
      !>        $Declarations section.
      !>
      !>-    SPICELIB Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
      !>
      !>        Parameters MAXVAR, MAXVAL, MAXLEN moved into $Declarations.
      !>        (Actually, MAXLEN was implicitly 32 characters, and has only
      !>        now been made an explicit---and changeable---limit.)
      !>
      !>        Declaration of unused function FAILED removed.
      module subroutine wrpool ( ctx, unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Logical unit to which the variables in the pool will be
                                       !! written.
      integer(ip), parameter :: maxlen = 32
      character(1), parameter :: quote = char(39)
      character(2) :: finish
      character(:), allocatable :: line
      character(:), allocatable :: cvalue
      character(:), allocatable :: dq
      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: k
      integer(ip) :: margin
      integer(ip) :: nm
      logical :: vector
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
!     The variables are written in the order of their slots (the order
!     in which they were created). PEPPER: a name is padded to MAXLEN
!     (32) characters as by SPICELIB, or written whole if longer. A line
!     longer than 132 characters (a long name or string value) cannot be
!     read back whole: text kernel lines are limited to 132 characters.
!
      associate ( ps => ctx%spice%pool )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      write ( unit, '(1X,A)', iostat=iostat ) ps%begdat
      write ( unit, '(1X,A)', iostat=iostat )
      if ( iostat /= 0 ) then
         call ioerr ( ctx, 'writing a variable to the output ' &
         //           'kernel file ', &
                      ' ', &
                       iostat             )
         call ctx%sigerr ( 'SPICE(WRITEERROR)' )
         return
      end if
      do k = 1, ps%nslots
         if ( .not. allocated ( ps%vars(k)%name ) ) cycle
         associate ( v => ps%vars(k) )
         if ( v%n < 1 ) then
            call ctx%setmsg ( 'This error is never supposed to ' &
            //            'occur. No data was available for ' &
            //            'the variable ''#''. ' )
            call ctx%errch  ( '#', v%name    )
            call ctx%sigerr ( 'SPICE(BUG)'   )
            return
         end if
         nm     = max ( maxlen, len(v%name) )
         margin = nm + 6
         line   = v%name // repeat ( ' ', margin - len(v%name) )
         line(nm+2:) = '= '
         vector = v%n > 1
         if ( vector ) then
            line(nm+4:) = '( '
         end if
         do i = 1, v%n
            if ( i < v%n ) then
               finish = ', '
            else if ( vector ) then
               finish = ' )'
            else
               finish = ' '
            end if
            if ( v%kind == pool_numeric ) then
               write ( unit, '(1X,A,D25.17,A)', iostat=iostat ) &
               line, v%dvals(i), finish
            else
               call double_quotes ( v%cvals(i)%s, dq )
               cvalue = quote // dq // quote
               write ( unit, '(1X,3A)', iostat=iostat ) &
               line, cvalue, finish
            end if
            if ( iostat /= 0 ) then
               call ioerr ( ctx, 'writing a variable to the output ' &
               //           'kernel file ', &
                             ' ', &
                             iostat              )
               call ctx%sigerr ( 'SPICE(WRITEERROR)' )
               return
            end if
            line = repeat ( ' ', margin )
         end do
         end associate
      end do
      write ( unit, '(1X,A)', iostat=iostat )
      write ( unit, '(1X,A)', iostat=iostat ) ps%begtxt
      if ( iostat /= 0 ) then
         call ioerr ( ctx, 'writing a variable to the output ' &
         //           'kernel file ', &
                        ' ', &
                        iostat              )
         call ctx%sigerr ( 'SPICE(WRITEERROR)' )
         return
      end if
      end associate

      contains

!
!        The string with each quote doubled (as in a kernel's string
!        values).
!
!        (A subroutine, not a function with a deferred-length result:
!        gfortran keeps that length in a static variable at the call
!        site, which is not thread-safe.)
!
         pure subroutine double_quotes ( str, out )
            character(*), intent(in) :: str  !! the string
            character(:), allocatable, intent(out) :: out  !! the string with its quotes doubled
            integer :: j
            out = ''
            do j = 1, len ( str )
               out = out // str(j:j)
               if ( str(j:j) == quote ) out = out // quote
            end do
         end subroutine double_quotes
      end subroutine wrpool

      !> Add a name to the list of agents to notify whenever a member of
      !> a list of kernel variables is updated.
      !>
      !>## Detailed Input
      !>
      !>     AGENT    is the name of a routine or entry point (agency) that
      !>              will want to know when the kernel pool variables
      !>              designated by NAMES have been updated.
      !>
      !>     NNAMES   is the number of kernel pool variable names that will
      !>              be associated with AGENT.
      !>
      !>     NAMES    is an array of names of variables in the kernel pool.
      !>              Whenever any of these is updated, a notice will be
      !>              posted for AGENT so that one can quickly check
      !>              whether needed data has been modified.
      !>
      !>              Any kernel variable may be associated with multiple
      !>              agents; this call adds AGENT to each set of agents
      !>              associated with a member of NAMES.
      !>
      !>              The variables designated by NAMES need not exist in
      !>              the kernel pool at the time a watch is set.
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
      !>     1)  If sufficient room is not available to hold a new kernel
      !>         variable name, the error SPICE(KERVARSETOVERFLOW) is signaled.
      !>
      !>     2)  If sufficient room is not available to hold a new agent
      !>         name, the error SPICE(TOOMANYWATCHES) is signaled.
      !>
      !>     3)  If any kernel variable in the array NAMES is already watched
      !>         by MAXAGT agents, and AGENT is not already associated with
      !>         that kernel variable, the error SPICE(AGENTLISTOVERFLOW) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The kernel pool is a convenient place to store a wide
      !>     variety of data needed by routines in SPICELIB and routines
      !>     that interface with SPICELIB routines. However, when
      !>     a single name has a large quantity of data associated with
      !>     it, it becomes inefficient to constantly query the kernel
      !>     pool for values that are not updated on a frequent basis.
      !>
      !>     This entry point allows a routine to instruct the kernel pool
      !>     to post a message whenever a particular value gets updated.
      !>     In this way, a routine can quickly determine whether or not
      !>     data it requires has been updated since the last time the
      !>     data was accessed. This makes it reasonable to buffer
      !>     the data in local storage and update it only when
      !>     a variable in the kernel pool that affects this data has
      !>     been updated.
      !>
      !>     Note that SWPOOL has a side effect. Whenever a call to
      !>     SWPOOL is made, the agent specified in the calling sequence
      !>     is added to the list of agents that should be notified that
      !>     an update of its variables has occurred. In other words
      !>     the code
      !>
      !>         CALL SWPOOL ( AGENT, NNAMES, NAMES  )
      !>         CALL CVPOOL ( AGENT,         UPDATE )
      !>
      !>     will always return UPDATE as .TRUE.
      !>
      !>     This feature allows for a slightly cleaner use of SWPOOL and
      !>     CVPOOL as shown in the example below. Because SWPOOL
      !>     automatically loads AGENT into the list of agents to notify of
      !>     a kernel pool update, you do not have to include the code for
      !>     fetching the initial values of the kernel variables in the
      !>     initialization portion of a subroutine. Instead, the code for
      !>     the first fetch from the pool is the same as the code for
      !>     fetching when the pool is updated.
      !>
      !>## Examples
      !>
      !>     Suppose that you have an application subroutine, MYTASK, that
      !>     needs to access a large data set in the kernel pool. If this
      !>     data could be kept in local storage and kernel pool queries
      !>     performed only when the data in the kernel pool has been
      !>     updated, the routine can perform much more efficiently.
      !>
      !>     The code fragment below illustrates how you might make use of this
      !>     feature.
      !>
      !>     C
      !>     C     On the first call to this routine establish those variables
      !>     C     that we will want to read from the kernel pool only when
      !>     C     new values have been established.
      !>     C
      !>           IF ( FIRST ) THEN
      !>
      !>              FIRST = .FALSE.
      !>              HAVE  = .FALSE.
      !>
      !>              CALL SWPOOL ( 'MYTASK', NNAMES, NAMES )
      !>
      !>           END IF
      !>
      !>     C
      !>     C     If any of the variables has been updated, fetch
      !>     C     it from the kernel pool. (Note that this also
      !>     C     handles getting variables for the first time.)
      !>     C     We use HAVE to indicate the fetch succeeded. If it
      !>     C     didn't, we need to attempt the fetch on the next
      !>     C     pass into this routine.
      !>     C
      !>           CALL CVPOOL ( 'MYTASK', UPDATE )
      !>
      !>           IF (  UPDATE  .OR (.NOT. HAVE ) ) THEN
      !>
      !>              CALL GDPOOL ( 'MYTASK_VAR_1', 1, M, N1, VALS1, FOUND(1) )
      !>              CALL GDPOOL ( 'MYTASK_VAR_2', 1, M, N2, VALS2, FOUND(2) )
      !>                      .
      !>                      .
      !>                      .
      !>              CALL GDPOOL ( 'MYTASK_VAR_N', 1, M, NN, VALSN, FOUND(N) )
      !>
      !>           END IF
      !>
      !>           IF ( FAILED() ) THEN
      !>                 .
      !>                 .
      !>              do something about the failure
      !>                 .
      !>                 .
      !>
      !>           ELSE
      !>
      !>              HAVE = .TRUE.
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.2.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.2.0, 30-JUL-2013 (BVS)
      !>
      !>        Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        This routine was re-written to work with the new
      !>        watcher system implementation. Several bugs related
      !>        to watch system overflow were fixed.
      !>
      !>        The code example was updated to handle kernel pool
      !>        fetch failure.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL and CVPOOL were added.
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        This routine was re-written to work with the new
      !>        watcher system implementation.
      !>
      !>        Several bugs related to watch system overflow were fixed.
      !>        Now overflow error checks are performed *before* the
      !>        watcher system is updated, so a partial update won't
      !>        occur if there's not enough room for a full update.
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The basic data structure used to maintain the list of
      !>        variable names and values was replaced with a hash table
      !>        implementation. Data and names are accessed by means
      !>        of a hash function and linked lists of pointers to existing
      !>        variable names and data values.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL (set watch on a pool variable)
      !>        and CVPOOL (check variable for update) so that routines
      !>        that buffer data stored in the kernel pool can fetch
      !>        that data only when it is updated.
      !>
      !>        In addition, the revision history was upgraded so that the
      !>        version number increases over time. This wasn't true
      !>        before. In addition some early revision data that referred to
      !>        pre-SPICELIB modifications were removed. This editing of
      !>        the version numbers makes it unlikely that anyone can track
      !>        down which previous version of this routine they have by
      !>        looking at the version number. The best way to determine
      !>        the routine you had previously is to compare the dates
      !>        stored in the Version line of the routine.
      module subroutine swpool ( ctx, agent, nnames, names )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: names     ( * )  !! Variable names whose update causes the
                                                   !! notice.
      integer(ip), intent(in) :: nnames  !! The number of variables to associate with AGENT.
      character(*), intent(in) :: agent  !! The name of an agent to be notified after updates.
!
!     Standard SPICE error handling.
!
!     PEPPER: each context keeps its own watches,
!     checked against the instance's log of the pool's changes (see
!     swpool_context in pepper_instance_module): contexts do not take each
!     other's notices, and a watch set while other contexts read the
!     pool changes nothing they use. A notice is posted for any update
!     of a watched variable (loaded, set or deleted) since the last
!     check, as by SPICELIB, and for every variable after CLPOOL.
!
      if ( ctx%failed() ) return
      call swpool_context ( ctx, agent, nnames, names )

      return
      end subroutine swpool

      !> Indicate whether or not any watched kernel variables that have a
      !> specified agent on their notification list have been updated.
      !>
      !>## Detailed Input
      !>
      !>     AGENT    is the name of a subroutine, entry point, or significant
      !>              portion of code that needs to access variables in the
      !>              kernel pool. Generally this agent will buffer these
      !>              variables internally and fetch them from the kernel
      !>              pool only when they are updated.
      !>
      !>## Detailed Output
      !>
      !>     UPDATE   is a logical flag that will be set to .TRUE. if the
      !>              variables in the kernel pool that are associated with
      !>              AGENT have been updated since the last call to CVPOOL.
      !>
      !>              UPDATE will be set to .TRUE. on the first call made for
      !>              the specified agent, whether or not the associated
      !>              variables have been updated since the agent was placed
      !>              on their notification list, as long as the agent is
      !>              associated with any watched variables.
      !>
      !>## Parameters
      !>
      !>     See the umbrella subroutine POOL.
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
      !>     This entry point allows the calling program to determine
      !>     whether or not variables associated with with AGENT have
      !>     been updated. Making use of this entry point in conjunction
      !>     with the entry point SWPOOL (set watch on pool variables)
      !>     modules can buffer kernel pool variables they need and
      !>     fetch values from the kernel pool only when variables have
      !>     been updated.
      !>
      !>     Note that the call to CVPOOL has a side effect.
      !>     Two consecutive calls to CVPOOL with the same
      !>     AGENT will always result in the UPDATE being .FALSE.
      !>     on the second call. In other words, if you embed
      !>     the following two lines of code in a piece of code
      !>
      !>        CALL CVPOOL ( AGENT, UPDATE )
      !>        CALL CVPOOL ( AGENT, UPDATE )
      !>
      !>     and then test UPDATE, it will be .FALSE. The idea is
      !>     that once a call to CVPOOL has been made, the
      !>     kernel pool has performed its duty and notified the
      !>     calling routine that one of the AGENT's variables
      !>     has been updated. Consequently, on the second call
      !>     to CVPOOL above, the kernel pool will not have any
      !>     updates to report about any of AGENT's variables.
      !>
      !>     If, on the other hand, you have code such as
      !>
      !>        CALL CVPOOL ( AGENT, UPDATE )
      !>        CALL LDPOOL ( 'MYFILE.DAT'  )
      !>        CALL CVPOOL ( AGENT, UPDATE )
      !>
      !>     the value of UPDATE will be true if one of the variables
      !>     associated with AGENT was updated by the call to
      !>     LDPOOL (and that variable has been specified as one
      !>     to watch by call a call to SWPOOL).
      !>
      !>     It should also be noted that any call to CVPOOL that
      !>     occurs immediately after a call to SWPOOL will result in
      !>     UPDATE being returned as .TRUE. In other words, code
      !>     such as shown below, will always result in the value
      !>     of UPDATE as being returned .TRUE.
      !>
      !>        CALL SWPOOL ( AGENT, NNAMES, NAMES  )
      !>        CALL CVPOOL ( AGENT,         UPDATE )
      !>
      !>     See the header for SWPOOL for a full discussion of this
      !>     feature.
      !>
      !>## Examples
      !>
      !>     Suppose that you have an application subroutine, MYTASK, that
      !>     needs to access a large data set in the kernel pool. If this
      !>     data could be kept in local storage and kernel pool queries
      !>     performed only when the data in the kernel pool has been
      !>     updated, the routine can perform much more efficiently.
      !>
      !>     The code fragment below illustrates how you might make use of this
      !>     feature.
      !>
      !>     C
      !>     C     On the first call to this routine establish those variables
      !>     C     that we will want to read from the kernel pool only when
      !>     C     new values have been established.
      !>     C
      !>           IF ( FIRST ) THEN
      !>
      !>              FIRST = .FALSE.
      !>              HAVE  = .FALSE.
      !>
      !>              CALL SWPOOL ( 'MYTASK', NNAMES, NAMES )
      !>
      !>           END IF
      !>
      !>     C
      !>     C     If any of the variables has been updated, fetch
      !>     C     it from the kernel pool. (Note that this also
      !>     C     handles getting variables for the first time.)
      !>     C     We use HAVE to indicate the fetch succeeded. If it
      !>     C     didn't, we need to attempt the fetch on the next
      !>     C     pass into this routine.
      !>     C
      !>           CALL CVPOOL ( 'MYTASK', UPDATE )
      !>
      !>           IF (  UPDATE  .OR (.NOT. HAVE ) ) THEN
      !>
      !>              CALL GDPOOL ( 'MYTASK_VAR_1', 1, M, N1, VALS1, FOUND(1) )
      !>              CALL GDPOOL ( 'MYTASK_VAR_2', 1, M, N2, VALS2, FOUND(2) )
      !>                      .
      !>                      .
      !>                      .
      !>              CALL GDPOOL ( 'MYTASK_VAR_N', 1, M, NN, VALSN, FOUND(N) )
      !>
      !>           END IF
      !>
      !>           IF ( FAILED() ) THEN
      !>                 .
      !>                 .
      !>              do something about the failure
      !>                 .
      !>                 .
      !>
      !>           ELSE
      !>
      !>              HAVE = .TRUE.
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.1.1, 30-JUN-2014 (NJB)
      !>
      !>        Description of the output variable UPDATE now
      !>        mentions that the initial value of .TRUE. will
      !>        be returned after an agent is associated with
      !>        kernel variables.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>        The code example was updated to handle kernel pool
      !>        fetch failure.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL and CVPOOL were added.
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The basic data structure used to maintain the list of
      !>        variable names and values was replaced with a hash table
      !>        implementation. Data and names are accessed by means
      !>        of a hash function and linked lists of pointers to existing
      !>        variable names and data values.
      !>
      !>-    SPICELIB Version 6.0.0, 31-MAR-1992 (WLT)
      !>
      !>        The entry points SWPOOL (set watch on a pool variable)
      !>        and CVPOOL (check variable for update) so that routines
      !>        that buffer data stored in the kernel pool can fetch
      !>        that data only when it is updated.
      !>
      !>        In addition, the revision history was upgraded so that the
      !>        version number increases over time. This wasn't true
      !>        before. In addition some early revision data that referred to
      !>        pre-SPICELIB modifications were removed. This editing of
      !>        the version numbers makes it unlikely that anyone can track
      !>        down which previous version of this routine they have by
      !>        looking at the version number. The best way to determine
      !>        the routine you had previously is to compare the dates
      !>        stored in the Version line of the routine.
      module subroutine cvpool ( ctx, agent, update )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: agent  !! Name of the agent to check for notices.
      logical, intent(out) :: update  !! .TRUE. if variables for AGENT have been updated.
!
!     Standard SPICE error handling.
!
!     PEPPER: each context keeps its own watches,
!     checked against the instance's log of the pool's changes (see
!     swpool_context in pepper_instance_module): contexts do not take each
!     other's notices, and a watch set while other contexts read the
!     pool changes nothing they use. A notice is posted for any update
!     of a watched variable (loaded, set or deleted) since the last
!     check, as by SPICELIB, and for every variable after CLPOOL.
!
      if ( ctx%failed() ) return
      call cvpool_context ( ctx, agent, update )

      return
      end subroutine cvpool

      !> Return the character value of a kernel variable from the
      !> kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the variable whose values are to be
      !>              returned. If the variable is not in the pool with
      !>              character type, FOUND will be .FALSE.
      !>
      !>     START    is the index of the first component of NAME to return.
      !>              If START is less than 1, it will be treated as 1. If
      !>              START is greater than the total number of components
      !>              available for NAME, no values will be returned (N will
      !>              be set to zero). However, FOUND will still be set to
      !>              .TRUE.
      !>
      !>     ROOM     is the maximum number of components that should be
      !>              returned for this variable. (Usually it is the amount
      !>              of ROOM available in the array CVALS). If ROOM is
      !>              less than 1 the error SPICE(BADARRAYSIZE) will be
      !>              signaled.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of values associated with NAME that
      !>              are returned. It will always be less than or equal
      !>              to ROOM.
      !>
      !>              If NAME is not in the pool with character type, no
      !>              value is given to N.
      !>
      !>     CVALS    is the array of values associated with NAME.
      !>              If NAME is not in the pool with character type, no
      !>              values are given to the elements of CVALS.
      !>
      !>              If the length of CVALS is less than the length of
      !>              strings stored in the kernel pool (see MAXCHR) the
      !>              values returned will be truncated on the right.
      !>
      !>     FOUND    is .TRUE. if the variable is in the pool and has
      !>              character type, .FALSE. if it is not.
      !>
      !>## Parameters
      !>
      !>     MAXCHR   is the maximum number of characters that can be
      !>              stored in a component of a string valued kernel
      !>              variable. This value is currently 80.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of ROOM is less than one, the error
      !>         SPICE(BADARRAYSIZE) is signaled.
      !>
      !>     2)  If CVALS has declared length less than the size of a
      !>         string to be returned, the value will be truncated on
      !>         the right. See MAXCHR for the maximum stored size of
      !>         string variables.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the user interface to retrieving
      !>     character data stored in the kernel pool. This interface
      !>     allows you to retrieve the data associated with a variable
      !>     in multiple accesses. Under some circumstances this alleviates
      !>     the problem of having to know in advance the maximum amount
      !>     of space needed to accommodate all kernel variables.
      !>
      !>     However, this method of access does come with a price. It is
      !>     always more efficient to retrieve all of the data associated
      !>     with a kernel pool data in one call than it is to retrieve
      !>     it in sections.
      !>
      !>     See also the entry points GDPOOL and GIPOOL.
      !>
      !>## Examples
      !>
      !>     The following code fragment demonstrates how the data stored
      !>     in a kernel pool variable can be retrieved in pieces.
      !>
      !>     First we need some declarations.
      !>
      !>        INTEGER               ROOM
      !>        PARAMETER           ( ROOM = 3 )
      !>
      !>        CHARACTER*(8)         VARNAM
      !>        CHARACTER*(3)         INDENT
      !>        INTEGER               START
      !>        INTEGER               N
      !>        LOGICAL               FOUND
      !>        CHARACTER*(80)        CVALS(ROOM)
      !>
      !>     Next load the data in the file 'typical.ker' into the
      !>     kernel pool.
      !>
      !>        CALL LDPOOL ( 'typical.ker' )
      !>
      !>     Next we shall print the values stored for the kernel pool
      !>     variable 'MYDATA'
      !>
      !>        VARNAM = 'MYDATA'
      !>        INDENT = ' '
      !>        START  =  1
      !>
      !>        CALL GCPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
      !>
      !>        IF ( .NOT. FOUND )
      !>           WRITE (*,*) 'There is no string data available for MYDATA.'
      !>        ELSE
      !>
      !>           WRITE (*,*) 'Values for MYDATA.'
      !>           WRITE (*,*)
      !>
      !>           DO I = 1, N
      !>              WRITE (*,*) INDENT, CVALS(I)
      !>           END DO
      !>
      !>           DO WHILE ( N .EQ. ROOM )
      !>
      !>              START = START + N
      !>              CALL GCPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
      !>
      !>              DO I = 1, N
      !>                 WRITE (*,*) INDENT, CVALS(I)
      !>              END DO
      !>
      !>           END DO
      !>
      !>        END IF
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
      !>-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added MAXCHR
      !>        description in $Parameters.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      module subroutine gcpool ( ctx, name, start, room, n, cvals, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
      integer(ip), intent(out) :: n  !! Number of values returned for NAME.
      logical, intent(out) :: found  !! .TRUE. if variable is in pool.
      integer(ip), intent(in) :: start  !! Which component to start retrieving for NAME
      integer(ip), intent(in) :: room  !! The largest number of values to return.
      character(*), intent(out) :: cvals     ( * )  !! Values associated with NAME.
      integer(ip) :: i
      integer(ip) :: k
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      if ( room < 1 ) then
         call ctx%setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )
         call ctx%errint ( '#', room )
         call ctx%sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return
      end if
      i = ps%find ( name )
      if ( i == 0 ) then
         found = .false.
         return
      end if
      if ( ps%vars(i)%kind /= pool_character ) then
         n     =  0
         found = .false.
         return
      end if
      found = .true.
      n     =  0
      do k = max ( start, 1 ), ps%vars(i)%n
         n = n + 1
         cvals(n) = ps%vars(i)%cvals(k)%s
         if ( n == room ) exit
      end do
      end associate
      end subroutine gcpool

      !> Return one string value of a character kernel variable, of any
      !> length (PEPPER; [[gcpool]] truncates to the length of its output).
      module subroutine pool_string ( ctx, name, nth, value, found, n )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the name of the variable
      integer(ip), intent(in) :: nth  !! the index of the value
      character(:), allocatable, intent(out) :: value  !! the value (empty if not found)
      logical, intent(out) :: found  !! the variable is a character variable with an NTH value
      integer(ip), intent(out), optional :: n  !! the number of values of the variable (0 if it is not a character variable)
      integer(ip) :: i
      value = ''
      found = .false.
      if ( present ( n ) ) n = 0
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      i = ps%find ( name )
      if ( i == 0 ) return
      if ( ps%vars(i)%kind /= pool_character ) return
      if ( present ( n ) ) n = ps%vars(i)%n
      if ( nth < 1 .or. nth > ps%vars(i)%n ) return
      value = ps%vars(i)%cvals(nth)%s
      found = .true.
      end associate
      end subroutine pool_string

      !> The names of the kernel variables that match a template (wildcards
      !> * and %, case-insensitive, as [[gnpool]]), of any length, in the order
      !> of [[gnpool]] (PEPPER).
      module subroutine pool_names ( ctx, template, names )
      use pepper_kinds, only: ip
      use pepper_containers, only: grow
      use pepper_strings, only: matchi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: template  !! the template
      type(string), allocatable, intent(out) :: names(:)  !! the matching names
      integer(ip) :: k
      integer(ip) :: n
      allocate ( names(0) )
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      n = 0
      do k = 1, ps%nslots
         if ( .not. allocated ( ps%vars(k)%name ) ) cycle
         if ( matchi ( ps%vars(k)%name, template, '*', '%' ) ) then
            n = n + 1
            call grow ( names, n )
            names(n)%s = ps%vars(k)%name
         end if
      end do
      names = names(1:n)
      end associate
      end subroutine pool_names

      !> All the values of a numeric kernel variable (PEPPER; [[gdpool]] with
      !> the size taken from the pool).
      module subroutine pool_values ( ctx, name, values, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the name of the variable
      real(wp), allocatable, intent(out) :: values(:)  !! its values (size 0 if not found)
      logical, intent(out) :: found  !! the variable is in the pool and numeric
      integer(ip) :: i
      found = .false.
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) then
         allocate ( values(0) )
         return
      end if
      if ( ps%first ) call zzpini ( ctx, ps )
      i = ps%find ( name )
      if ( i == 0 ) then
         allocate ( values(0) )
      else if ( ps%vars(i)%kind /= pool_numeric ) then
         allocate ( values(0) )
      else
         values = ps%vars(i)%dvals(1:ps%vars(i)%n)
         found  = .true.
      end if
      end associate
      end subroutine pool_values

      !> All the values of a numeric kernel variable, rounded to integers
      !> (PEPPER; [[gipool]] with the size taken from the pool).
      !>
      !> A value outside the range of integers signals SPICE(INTOUTOFRANGE).
      module subroutine pool_integers ( ctx, name, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax, intmin
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the name of the variable
      integer(ip), allocatable, intent(out) :: values(:)  !! its values (size 0 if not found)
      logical, intent(out) :: found  !! the variable is in the pool and numeric
      integer(ip) :: i
      integer(ip) :: k
      found = .false.
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) then
         allocate ( values(0) )
         return
      end if
      if ( ps%first ) call zzpini ( ctx, ps )
      i = ps%find ( name )
      if ( i == 0 ) then
         allocate ( values(0) )
         return
      else if ( ps%vars(i)%kind /= pool_numeric ) then
         allocate ( values(0) )
         return
      end if
      allocate ( values(ps%vars(i)%n) )
      do k = 1, ps%vars(i)%n
         if (       ps%vars(i)%dvals(k) >= real( intmin, wp ) &
              .and. ps%vars(i)%dvals(k) <= real( intmax, wp )   ) then
            values(k) = nint ( ps%vars(i)%dvals(k), ip )
         else
            call ctx%setmsg ( 'The value associated with index # ' &
            //            'of the kernel variable # is ' &
            //            'outside the range of integers. ' &
            //            'The value stored was: # .'      )
            call ctx%errint ( '#', k                     )
            call ctx%errch  ( '#', trim(name)            )
            call ctx%errdp  ( '#', ps%vars(i)%dvals(k)   )
            call ctx%sigerr ( 'SPICE(INTOUTOFRANGE)'     )
            deallocate ( values )
            allocate ( values(0) )
            return
         end if
      end do
      found = .true.
      end associate
      end subroutine pool_integers

      !> All the values of a character kernel variable, each of any length
      !> (PEPPER; [[gcpool]] with the size taken from the pool).
      module subroutine pool_strings ( ctx, name, values, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the name of the variable
      type(string), allocatable, intent(out) :: values(:)  !! its values (size 0 if not found)
      logical, intent(out) :: found  !! the variable is in the pool and of character type
      integer(ip) :: i
      integer(ip) :: k
      found = .false.
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) then
         allocate ( values(0) )
         return
      end if
      if ( ps%first ) call zzpini ( ctx, ps )
      i = ps%find ( name )
      if ( i == 0 ) then
         allocate ( values(0) )
      else if ( ps%vars(i)%kind /= pool_character ) then
         allocate ( values(0) )
      else
         allocate ( values(ps%vars(i)%n) )
         do k = 1, ps%vars(i)%n
            values(k)%s = ps%vars(i)%cvals(k)%s
         end do
         found = .true.
      end if
      end associate
      end subroutine pool_strings

      !> Where the values of a kernel variable came from (PEPPER): the
      !> assignments in effect, in order, with the source of each, the line
      !> where it begins, and the index of its first value. An `=` assignment
      !> replaces the values, so it is the first one; each `+=` (or `"+NAME"`
      !> of a JSON kernel) that followed adds one.
      !>
      !> The source is the kernel file, named as it was loaded (with
      !> [[spice_furnsh]], or listed in a meta-kernel), or a tag for the values
      !> set by a program: `(PCPOOL)`, `(PDPOOL)`, `(PIPOOL)`, `(LMPOOL)` (its
      !> line is the line of the buffer), `(add_frame)` (the definition of a
      !> programmatic frame, [[spice_add_frame]]). [[pool_defined_by]] is the
      !> reverse.
      module subroutine pool_sources ( ctx, name, sources, lines, found, first )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the name of the variable
      type(string), allocatable, intent(out) :: sources(:)  !! the source of each assignment (size 0 if not found)
      integer(ip), allocatable, intent(out) :: lines(:)  !! the line where each begins (0 for PCPOOL, PDPOOL, PIPOOL, add_frame)
      logical, intent(out) :: found  !! the variable is in the pool
      integer(ip), allocatable, intent(out), optional :: first(:)  !! the index of the first value each gave
      integer(ip) :: i
      integer(ip) :: k
      integer(ip) :: n
      found = .false.
      n = 0
      associate ( ps => ctx%spice%pool )
      if ( .not. ctx%failed() ) then
         if ( ps%first ) call zzpini ( ctx, ps )
         i = ps%find ( name )
         if ( i > 0 ) then
            found = .true.
            n = ps%vars(i)%nfrom
         end if
      end if
      allocate ( sources(n), lines(n) )
      if ( present ( first ) ) allocate ( first(n) )
      do k = 1, n
         associate ( a => ps%vars(i)%from(k) )
            sources(k)%s = ps%sources(a%source)%s
            lines(k) = a%line
            if ( present ( first ) ) first(k) = a%first
         end associate
      end do
      end associate
      end subroutine pool_sources

      !> The kernel variables that a source assigned (PEPPER; the reverse of
      !> [[pool_sources]]): the variables with an assignment in effect from
      !> it, in the order of [[gnpool]]. A variable that a later `=` from
      !> another source replaced is not among them.
      module subroutine pool_defined_by ( ctx, source, names )
      use pepper_kinds, only: ip
      use pepper_containers, only: grow
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: source  !! the kernel file, named as it was loaded, or a tag such as (PDPOOL)
      type(string), allocatable, intent(out) :: names(:)  !! the variables it assigned
      integer(ip) :: id
      integer(ip) :: k
      integer(ip) :: n
      logical :: known
      allocate ( names(0) )
      if ( ctx%failed() ) return
      associate ( ps => ctx%spice%pool )
      if ( ps%first ) call zzpini ( ctx, ps )
      call ps%source_index%get ( trim(source), id, known )
      if ( .not. known ) return
      n = 0
      do k = 1, ps%nslots
         if ( .not. allocated ( ps%vars(k)%name ) ) cycle
         if ( ps%vars(k)%nfrom == 0 ) cycle
         if ( any ( ps%vars(k)%from(1:ps%vars(k)%nfrom)%source == id ) ) then
            n = n + 1
            call grow ( names, n )
            names(n)%s = ps%vars(k)%name
         end if
      end do
      names = names(1:n)
      end associate
      end subroutine pool_defined_by

      !> Write the kernel pool as a JSON kernel (PEPPER; the JSON counterpart
      !> of [[wrpool]]): one `"NAME": value` per variable, in the order of
      !> [[gnpool]], each with a `//` comment naming its sources and lines
      !> ([[pool_sources]]). Loading the file ([[spice_furnsh]], as a `*.json`
      !> kernel) gives the same variables and values: the numbers are written
      !> with as many digits as they need to read back exactly (integers
      !> without a fraction), and times as the numbers they became.
      module subroutine pool_write_json ( ctx, unit )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! the unit of the file: open for formatted sequential writing
      integer(ip), parameter :: wrap = 100  !! longest line of values before an array is written on several lines
      character(:), allocatable :: line
      character(:), allocatable :: comment
      character(:), allocatable :: item
      integer(ip) :: k
      integer(ip) :: j
      integer(ip) :: last
      integer(ip) :: iostat
      logical :: several
      if ( ctx%failed() ) return
      associate ( ps => ctx%spice%pool )
      if ( ps%first ) call zzpini ( ctx, ps )
      ! the last variable written (no comma after it)
      last = 0
      do k = 1, ps%nslots
         if ( allocated ( ps%vars(k)%name ) ) then
            if ( ps%vars(k)%n > 0 ) last = k
         end if
      end do
      write ( unit, '(a)', iostat=iostat ) '// The kernel pool (pool_write_json): each variable''s sources and lines'
      if ( iostat == 0 ) write ( unit, '(a)', iostat=iostat ) '{'
      do k = 1, ps%nslots
         if ( iostat /= 0 ) exit
         if ( .not. allocated ( ps%vars(k)%name ) ) cycle
         associate ( v => ps%vars(k) )
         ! the comment: the sources of the assignments in effect
         comment = ''
         do j = 1, v%nfrom
            if ( j > 1 ) comment = comment // '; '
            comment = comment // ps%sources(v%from(j)%source)%s
            if ( v%from(j)%line > 0 ) then
               call append_integer ( comment, ' line ', int ( v%from(j)%line, int64 ) )
            end if
         end do
         if ( v%n == 0 ) then
            ! a JSON kernel cannot give a variable no values
            write ( unit, '(a)', iostat=iostat ) '    // ' // v%name // ': no values (' // comment // ')'
            cycle
         end if
         call json_string ( v%name, item )
         line = '    ' // item // ': '
         ! the values, on the line if they fit
         several = .false.
         if ( v%n > 1 ) then
            line = line // '['
            do j = 1, v%n
               call value_text ( v, j, item )
               if ( j > 1 ) then
                  if ( len ( line ) + len ( item ) + 2 > wrap ) then
                     several = .true.
                     exit
                  end if
                  line = line // ', '
               end if
               line = line // item
            end do
         end if
         if ( several ) then
            ! one value per line, the comment after the opening bracket
            call json_string ( v%name, item )
            write ( unit, '(a)', iostat=iostat ) '    ' // item // ': [  // ' // comment
            do j = 1, v%n
               if ( iostat /= 0 ) exit
               call value_text ( v, j, item )
               if ( j < v%n ) item = item // ','
               write ( unit, '(a)', iostat=iostat ) '        ' // item
            end do
            line = '    ]'
            if ( k /= last ) line = line // ','
            if ( iostat == 0 ) write ( unit, '(a)', iostat=iostat ) line
         else
            if ( v%n > 1 ) then
               line = line // ']'
            else
               call value_text ( v, 1_ip, item )
               line = line // item
            end if
            if ( k /= last ) line = line // ','
            write ( unit, '(a)', iostat=iostat ) line // '  // ' // comment
         end if
         end associate
      end do
      if ( iostat == 0 ) write ( unit, '(a)', iostat=iostat ) '}'
      if ( iostat /= 0 ) then
         call ioerr ( ctx, 'writing the kernel pool as a JSON kernel', ' ', iostat )
         call ctx%sigerr ( 'SPICE(WRITEERROR)' )
      end if
      end associate

      contains

      !> Value j of a variable, as JSON: a number that reads back exactly, or a
      !> string.
      subroutine value_text ( v, j, text )
         type(pool_variable), intent(in) :: v  !! the variable
         integer(ip), intent(in) :: j  !! the value's index
         character(:), allocatable, intent(out) :: text  !! the value as JSON
         if ( v%kind == pool_numeric ) then
            call number_text ( v%dvals(j), text )
         else
            call json_string ( v%cvals(j)%s, text )
         end if
      end subroutine value_text

      !> A number as JSON, with the fewest significant digits (15 to 17) that
      !> read back to the same number, without trailing zeros (1.5E-3,
      !> 0.25 as 2.5E-1, 1.25 without an exponent); a whole number of up to 15
      !> digits without a fraction or exponent.
      subroutine number_text ( x, text )
         real(wp), intent(in) :: x  !! the number
         character(:), allocatable, intent(out) :: text  !! the number as JSON
         character(40) :: buffer
         character(16) :: form
         character(:), allocatable :: mantissa
         real(wp) :: back
         integer :: digits
         integer :: ios
         integer :: e
         integer :: exponent
         if ( x == aint ( x ) .and. abs ( x ) < 1.0e15_wp ) then
            write ( buffer, '(i0)' ) int ( x, int64 )
            text = trim ( buffer )
            return
         end if
         do digits = 15, 17
            write ( form, '(a,i0,a)' ) '(es30.', digits - 1, 'e3)'
            write ( buffer, form ) x
            read ( buffer, *, iostat=ios ) back
            if ( ios == 0 .and. back == x ) exit
         end do
         ! d.dddd000E+00n: the mantissa without its trailing zeros, the
         ! exponent without its leading zeros (none if it is 0)
         buffer = adjustl ( buffer )
         e = index ( buffer, 'E' )
         mantissa = buffer(1:e-1)
         do while ( mantissa(len(mantissa):) == '0' )
            mantissa = mantissa(1:len(mantissa)-1)
         end do
         if ( mantissa(len(mantissa):) == '.' ) mantissa = mantissa // '0'
         read ( buffer(e+1:), * ) exponent
         if ( exponent == 0 ) then
            text = mantissa
         else
            write ( buffer, '(a,i0)' ) 'E', exponent
            text = mantissa // trim ( buffer )
         end if
      end subroutine number_text

      !> A string as JSON: in quotes, with `"`, `\` and the control
      !> characters escaped.
      subroutine json_string ( s, text )
         character(*), intent(in) :: s  !! the string
         character(:), allocatable, intent(out) :: text  !! the string as JSON
         character(6) :: hex
         integer :: i
         integer :: c
         text = '"'
         do i = 1, len ( s )
            c = iachar ( s(i:i) )
            if ( s(i:i) == '"' .or. s(i:i) == '\' ) then
               text = text // '\' // s(i:i)
            else if ( c < 32 ) then
               write ( hex, '(a,z4.4)' ) '\u', c
               text = text // hex
            else
               text = text // s(i:i)
            end if
         end do
         text = text // '"'
      end subroutine json_string

      !> Append a label and an integer to a text.
      subroutine append_integer ( text, label, n )
         character(:), allocatable, intent(inout) :: text  !! the text
         character(*), intent(in) :: label  !! the label
         integer(int64), intent(in) :: n  !! the integer
         character(24) :: buffer
         write ( buffer, '(i0)' ) n
         text = text // label // trim ( buffer )
      end subroutine append_integer
      end subroutine pool_write_json

      !> Return the d.p. value of a kernel variable from the kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the variable whose values are to be
      !>              returned. If the variable is not in the pool with
      !>              numeric type, FOUND will be .FALSE.
      !>
      !>     START    is the index of the first component of NAME to return.
      !>              If START is less than 1, it will be treated as 1. If
      !>              START is greater than the total number of components
      !>              available for NAME, no values will be returned (N will
      !>              be set to zero). However, FOUND will still be set to
      !>              .TRUE.
      !>
      !>     ROOM     is the maximum number of components that should be
      !>              returned for this variable. (Usually it is the amount
      !>              of ROOM available in the array VALUES). If ROOM is
      !>              less than 1 the error SPICE(BADARRAYSIZE) will be
      !>              signaled.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of values associated with NAME that
      !>              are returned. It will always be less than or equal
      !>              to ROOM.
      !>
      !>              If NAME is not in the pool with numeric type, no value
      !>              is given to N.
      !>
      !>     VALUES   is the array of values associated with NAME.
      !>              If NAME is not in the pool with numeric type, no
      !>              values are given to the elements of VALUES.
      !>
      !>     FOUND    is .TRUE. if the variable is in the pool and has numeric
      !>              type, .FALSE. if it is not.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of ROOM is less than one, the error
      !>         SPICE(BADARRAYSIZE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the user interface to retrieving
      !>     numeric data stored in the kernel pool. This interface
      !>     allows you to retrieve the data associated with a variable
      !>     in multiple accesses. Under some circumstances this alleviates
      !>     the problem of having to know in advance the maximum amount
      !>     of space needed to accommodate all kernel variables.
      !>
      !>     However, this method of access does come with a price. It is
      !>     always more efficient to retrieve all of the data associated
      !>     with a kernel pool data in one call than it is to retrieve
      !>     it in sections.
      !>
      !>     This routine should be used in place of RTPOOL when possible
      !>     as it avoids errors associated with writing data past the
      !>     end of an array.
      !>
      !>     See also the entry points GIPOOL and GCPOOL.
      !>
      !>## Examples
      !>
      !>     The following code fragment demonstrates how the data stored
      !>     in a kernel pool variable can be retrieved in pieces.
      !>
      !>     First we need some declarations.
      !>
      !>        INTEGER               ROOM
      !>        PARAMETER           ( ROOM = 3 )
      !>
      !>        CHARACTER*(8)         VARNAM
      !>        CHARACTER*(3)         INDENT
      !>        INTEGER               START
      !>        INTEGER               N
      !>        LOGICAL               FOUND
      !>        DOUBLE PRECISION      VALUES(ROOM)
      !>
      !>     Next load the data in the file 'typical.ker' into the
      !>     kernel pool.
      !>
      !>        CALL LDPOOL ( 'typical.ker' )
      !>
      !>     Next we shall print the values stored for the kernel pool
      !>     variable 'MYDATA'
      !>
      !>        VARNAM = 'MYDATA'
      !>        INDENT = ' '
      !>        START  =  1
      !>
      !>        CALL GDPOOL ( VARNAM, START, ROOM, N, VALUES, FOUND )
      !>
      !>        IF ( .NOT. FOUND )
      !>           WRITE (*,*) 'There is no numeric data available for MYDATA.'
      !>        ELSE
      !>
      !>           WRITE (*,*) 'Values for MYDATA.'
      !>           WRITE (*,*)
      !>
      !>           DO I = 1, N
      !>              WRITE (*,*) INDENT, VALUES(I)
      !>           END DO
      !>
      !>           DO WHILE ( N .EQ. ROOM )
      !>
      !>              START = START + N
      !>              CALL GDPOOL ( VARNAM, START, ROOM, N, VALUES, FOUND )
      !>
      !>              DO I = 1, N
      !>                 WRITE (*,*) INDENT, VALUES(I)
      !>              END DO
      !>
      !>           END DO
      !>
      !>        END IF
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
      !>-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      module subroutine gdpool ( ctx, name, start, room, n, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
      integer(ip), intent(out) :: n  !! Number of values returned for NAME.
      real(wp), intent(out) :: values    ( * )  !! Values associated with NAME.
      logical, intent(out) :: found  !! .TRUE. if variable is in pool.
      integer(ip), intent(in) :: start  !! Which component to start retrieving for NAME
      integer(ip), intent(in) :: room  !! The largest number of values to return.
      integer(ip) :: i
      integer(ip) :: k
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      if ( room < 1 ) then
         call ctx%setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )
         call ctx%errint ( '#', room )
         call ctx%sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return
      end if
      i = ps%find ( name )
      if ( i == 0 ) then
         found = .false.
         return
      end if
      if ( ps%vars(i)%kind /= pool_numeric ) then
         n     =  0
         found = .false.
         return
      end if
      found = .true.
      n     =  0
      do k = max ( start, 1 ), ps%vars(i)%n
         n = n + 1
         values(n) = ps%vars(i)%dvals(k)
         if ( n == room ) exit
      end do
      end associate
      end subroutine gdpool

      !> Return the integer value of a kernel variable from the
      !> kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the variable whose values are to be
      !>              returned. If the variable is not in the pool with
      !>              numeric type, FOUND will be .FALSE.
      !>
      !>     START    is the index of the first component of NAME to return.
      !>              If START is less than 1, it will be treated as 1. If
      !>              START is greater than the total number of components
      !>              available for NAME, no values will be returned (N will
      !>              be set to zero). However, FOUND will still be set to
      !>              .TRUE.
      !>
      !>     ROOM     is the maximum number of components that should be
      !>              returned for this variable. (Usually it is the amount
      !>              of ROOM available in the array IVALS). If ROOM is
      !>              less than 1 the error SPICE(BADARRAYSIZE) will be
      !>              signaled.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of values associated with NAME that
      !>              are returned. It will always be less than or equal
      !>              to ROOM.
      !>
      !>              If NAME is not in the pool with numeric type, no value
      !>              is given to N.
      !>
      !>     IVALS    is the array of values associated with NAME. Any
      !>              numeric value having non-zero fractional part is
      !>              rounded to the closest integer. If NAME is not in the
      !>              pool or does not have numeric type, no values are
      !>              assigned to the elements of IVALS.
      !>
      !>     FOUND    is .TRUE. if the variable is in the pool and has numeric
      !>              type, .FALSE. if it is not.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of ROOM is less than one, the error
      !>         SPICE(BADARRAYSIZE) is signaled.
      !>
      !>     2)  If a value requested is outside the valid range
      !>         of integers, the error SPICE(INTOUTOFRANGE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the user interface for retrieving
      !>     integer data stored in the kernel pool. This interface
      !>     allows you to retrieve the data associated with a variable
      !>     in multiple accesses. Under some circumstances this alleviates
      !>     the problem of having to know in advance the maximum amount
      !>     of space needed to accommodate all kernel variables.
      !>
      !>     However, this method of access does come with a price. It is
      !>     always more efficient to retrieve all of the data associated
      !>     with a kernel pool data in one call than it is to retrieve
      !>     it in sections.
      !>
      !>     See also the entry points GDPOOL and GCPOOL.
      !>
      !>## Examples
      !>
      !>     The following code fragment demonstrates how the data stored
      !>     in a kernel pool variable can be retrieved in pieces.
      !>
      !>     First we need some declarations.
      !>
      !>        INTEGER               ROOM
      !>        PARAMETER           ( ROOM = 3 )
      !>
      !>        CHARACTER*(8)         VARNAM
      !>        CHARACTER*(3)         INDENT
      !>        INTEGER               START
      !>        INTEGER               N
      !>        LOGICAL               FOUND
      !>        INTEGER               IVALS(ROOM)
      !>
      !>     Next load the data in the file 'typical.ker' into the
      !>     kernel pool.
      !>
      !>        CALL LDPOOL ( 'typical.ker' )
      !>
      !>     Next we shall print the values stored for the kernel pool
      !>     variable 'MYDATA'
      !>
      !>        VARNAM = 'MYDATA'
      !>        INDENT = ' '
      !>        START  =  1
      !>
      !>        CALL GIPOOL ( VARNAM, START, ROOM, N, IVALS, FOUND )
      !>
      !>        IF ( .NOT. FOUND )
      !>           WRITE (*,*) 'There is no numeric data available for MYDATA.'
      !>        ELSE
      !>
      !>           WRITE (*,*) 'Values for MYDATA.'
      !>           WRITE (*,*)
      !>
      !>           DO I = 1, N
      !>              WRITE (*,*) INDENT, IVALS(I)
      !>           END DO
      !>
      !>           DO WHILE ( N .EQ. ROOM )
      !>
      !>              START = START + N
      !>              CALL GIPOOL ( VARNAM, START, ROOM, N, IVALS, FOUND )
      !>
      !>              DO I = 1, N
      !>                 WRITE (*,*) INDENT, IVALS(I)
      !>              END DO
      !>
      !>           END DO
      !>
      !>        END IF
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
      !>-    SPICELIB Version 8.1.2, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.1.1, 14-JUL-2014 (NJB)
      !>
      !>        Updated description of IVALS in $Detailed_Output
      !>        header section.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      module subroutine gipool ( ctx, name, start, room, n, ivals, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
      integer(ip), intent(out) :: n  !! Number of values returned for NAME.
      logical, intent(out) :: found  !! .TRUE. if variable is in pool.
      integer(ip), intent(in) :: start  !! Which component to start retrieving for NAME
      integer(ip), intent(in) :: room  !! The largest number of values to return.
      integer(ip), intent(out) :: ivals     ( * )  !! Values associated with NAME.
      integer(ip) :: i
      integer(ip) :: k
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      if ( room < 1 ) then
         call ctx%setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )
         call ctx%errint ( '#', room )
         call ctx%sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return
      end if
      i = ps%find ( name )
      if ( i == 0 ) then
         found = .false.
         return
      end if
      if ( ps%vars(i)%kind /= pool_numeric ) then
         n     =  0
         found = .false.
         return
      end if
      found = .true.
      n     =  0
      do k = max ( start, 1 ), ps%vars(i)%n
         n = n + 1
         if (       ps%vars(i)%dvals(k) >= real( intmin, wp ) &
              .and. ps%vars(i)%dvals(k) <= real( intmax, wp )   ) then
            ivals(n) = idnint( ps%vars(i)%dvals(k) )
         else
            call ctx%setmsg ( 'The value associated with index # ' &
            //            'of the kernel variable # is ' &
            //            'outside the range of integers. ' &
            //            'The value stored was: # .'      )
            call ctx%errint ( '#', k                     )
            call ctx%errch  ( '#', trim(name)            )
            call ctx%errdp  ( '#', ps%vars(i)%dvals(k)   )
            call ctx%sigerr ( 'SPICE(INTOUTOFRANGE)'     )
            return
         end if
         if ( n == room ) exit
      end do
      end associate
      end subroutine gipool

      !> Return the data about a kernel pool variable.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the variable whose values are to be
      !>              returned.
      !>
      !>## Detailed Output
      !>
      !>     FOUND    is .TRUE. if the variable is in the pool .FALSE. if it
      !>              is not.
      !>
      !>     N        is the number of values associated with NAME.
      !>              If NAME is not present in the pool N will be returned
      !>              with the value 0.
      !>
      !>     TYPE     is the type of the variable associated with NAME.
      !>
      !>                  'C' if the data is character data
      !>                  'N' if the data is numeric.
      !>                  'X' if there is no variable NAME in the pool.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the name requested is not in the kernel pool, FOUND
      !>         will be set to .FALSE., N to zero and TYPE to 'X'.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to determine whether or not a kernel
      !>     pool variable is present and to determine its size and type
      !>     if it is.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following program demonstrates how to determine the
      !>        properties of a stored kernel variable. The program prompts
      !>        for text kernel name and for the name of a kernel variable.
      !>        If the variable is present in the kernel pool, the dimension
      !>        and type of the variable are displayed.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DTPOOL_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local constants
      !>C
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 256 )
      !>
      !>      INTEGER               KVNMLN
      !>      PARAMETER           ( KVNMLN = 33 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>      CHARACTER*(KVNMLN)    VARNAM
      !>      CHARACTER*(1)         VTYPE
      !>
      !>      INTEGER               N
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Prompt for the name of a text-kernel file.
      !>C
      !>      CALL PROMPT ( 'Enter text-kernel name        > ', FNAME )
      !>
      !>C
      !>C     Load the kernel.
      !>C
      !>      CALL FURNSH ( FNAME )
      !>
      !>      CALL PROMPT ( 'Enter name of kernel variable > ',
      !>     .               VARNAM )
      !>
      !>      CALL DTPOOL ( VARNAM, FOUND, N, VTYPE )
      !>
      !>      IF ( FOUND ) THEN
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) 'Properties of variable ',
      !>     .               VARNAM(:RTRIM(VARNAM)), ':'
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) '   Size:   ', N
      !>
      !>         IF ( VTYPE .EQ. 'C' ) THEN
      !>
      !>            WRITE(*,*) '   Type:   Character'
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) '   Type:   Numeric'
      !>         END IF
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) VARNAM,
      !>     .              ' is not present in the kernel pool.'
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the FK file named cas_v40.tf to ask for the
      !>        variable 'FRAME_-82104_NAME', the output was:
      !>
      !>        Enter text-kernel name        > cas_v40.tf
      !>        Enter name of kernel variable > FRAME_-82104_NAME
      !>
      !>         Properties of variable FRAME_-82104_NAME:
      !>
      !>            Size:              1
      !>            Type:   Character
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
      !>-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.1, 22-DEC-2004 (NJB)
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>-    SPICELIB Version 7.0.0, 20-SEP-1995 (WLT)
      !>
      !>        The implementation of the kernel pool was completely redone
      !>        to improve performance in loading and fetching data. In
      !>        addition the pool was upgraded so that variables may be
      !>        either string or numeric valued.
      !>
      !>        The entry points GCPOOL, GDPOOL, GIPOOL and DTPOOL were added
      !>        to the routine.
      module subroutine dtpool ( ctx, name, found, n, type )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
      integer(ip), intent(out) :: n  !! Number of values returned for NAME.
      logical, intent(out) :: found  !! .TRUE. if variable is in pool.
      character(*), intent(out) :: type  !! Type of the variable 'C', 'N', 'X'
      integer(ip) :: i
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      found = .false.
      n     =  0
      type  = 'X'
      i = ps%find ( name )
      if ( i == 0 ) return
      if ( ps%vars(i)%kind == pool_character ) then
         type  = 'C'
      else if ( ps%vars(i)%kind == pool_numeric ) then
         type  = 'N'
      else
         return
      end if
      found = .true.
      n     = ps%vars(i)%n
      end associate
      end subroutine dtpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting character data into the kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the kernel pool variable to associate
      !>              with the values supplied in the array CVALS
      !>
      !>     N        is the number of values to insert into the kernel pool.
      !>
      !>     CVALS    is an array of strings to insert into the kernel
      !>              pool.
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
      !>     1)  If NAME is already present in the kernel pool and there
      !>         is sufficient room to hold all values supplied in CVALS,
      !>         the old values associated with NAME will be overwritten.
      !>
      !>     2)  If there is not sufficient room to insert a new variable into
      !>         the kernel pool and NAME is not already present in the kernel
      !>         pool, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     3)  If there is not sufficient room to insert the values
      !>         associated with NAME, the error SPICE(NOMOREROOM) is signaled.
      !>
      !>     4)  If the kernel pool variable name length exceeds its maximum
      !>         allowed length (see Kernel Required Reading, kernel.req), the
      !>         error SPICE(BADVARNAME) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This entry point provides a programmatic interface for inserting
      !>     character data into the SPICE kernel pool without reading an
      !>     external file.
      !>
      !>## Examples
      !>
      !>     Suppose that you wish to supply default values for a program
      !>     so that it may function even in the absence of the appropriate
      !>     text kernels. You can use the entry points PCPOOL, PDPOOL
      !>     and PIPOOL to initialize the kernel pool with suitable
      !>     values at program initialization. The example below shows
      !>     how you might set up various kernel pool variables that might
      !>     be required by a program.
      !>
      !>        Set up the relationship between the EARTH_BODYFIXED frame
      !>        and the IAU_EARTH frame.
      !>
      !>        CALL IDENT  ( MATRIX )
      !>        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX'    )
      !>        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1, 'IAU_EARTH' )
      !>        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX )
      !>
      !>        Load the IAU model for the earth's rotation and shape.
      !>
      !>        RA ( 1 ) =  0.0D0
      !>        RA ( 2 ) = -0.641D0
      !>        RA ( 3 ) =  0.0D0
      !>
      !>        DEC( 1 ) = 90.0D0
      !>        DEC( 2 ) = -0.557D0
      !>        DEC( 3 ) =  0.0D0
      !>
      !>        PM ( 1 ) = 190.16D0
      !>        PM ( 2 ) = 360.9856235D0
      !>        PM ( 3 ) =   0.0D0
      !>
      !>        R  ( 1 ) =  6378.140D0
      !>        R  ( 2 ) =  6378.140D0
      !>        R  ( 3 ) =  6356.75D0
      !>
      !>        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
      !>        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
      !>        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
      !>        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
      !>
      !>        Set up a preliminary set of leapsecond values.
      !>
      !>        CALL PDPOOL ( 'DELTET/DELTA_T_A/',1, 32.184D0  )
      !>        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
      !>        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
      !>
      !>        VALUES(1) = 6.23999600D0
      !>        VALUES(2) = 1.99096871D-7
      !>
      !>        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
      !>
      !>        VALUES(  1 ) = 10
      !>        VALUES(  3 ) = 11
      !>        VALUES(  5 ) = 12
      !>        VALUES(  7 ) = 13
      !>        VALUES(  9 ) = 14
      !>        VALUES( 11 ) = 15
      !>        VALUES( 13 ) = 16
      !>        VALUES( 15 ) = 17
      !>        VALUES( 17 ) = 18
      !>        VALUES( 19 ) = 19
      !>        VALUES( 21 ) = 20
      !>        VALUES( 23 ) = 21
      !>        VALUES( 25 ) = 22
      !>        VALUES( 27 ) = 23
      !>        VALUES( 29 ) = 24
      !>        VALUES( 31 ) = 25
      !>        VALUES( 33 ) = 26
      !>        VALUES( 35 ) = 27
      !>        VALUES( 37 ) = 28
      !>        VALUES( 39 ) = 29
      !>        VALUES( 41 ) = 30
      !>        VALUES( 43 ) = 31
      !>
      !>        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
      !>        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
      !>        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
      !>        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
      !>        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
      !>        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
      !>        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
      !>        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
      !>        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
      !>        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
      !>        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
      !>        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
      !>        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
      !>        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
      !>        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
      !>        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
      !>        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
      !>        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
      !>        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
      !>        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
      !>        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
      !>        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
      !>
      !>        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 9.1.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
      !>
      !>        Updated to increment POOL state counter.
      !>        Updated $Index_Entries section.
      !>
      !>-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
      !>
      !>        Added an error check on the length of the kernel pool variable
      !>        name argument to enforce the variable name length does not
      !>        exceed MAXLEN.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory instead
      !>        of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      module subroutine pcpool ( ctx, name, n, cvals )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! The kernel pool name to associate with CVALS.
      integer(ip), intent(in) :: n  !! The number of values to insert.
      character(*), intent(in) :: cvals     ( * )  !! An array of strings to insert into the
                                                   !! kernel pool.
      integer(ip) :: i
      integer(ip) :: nameat
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
!     PEPPER: names have any length (SPICELIB signaled SPICE(BADVARNAME)
!     for more than 32 characters), and there is no limit on the number
!     of values (SPICE(NOMOREROOM), SPICE(KERNELPOOLFULL)).
!
      if ( n <= 0 ) return
      if ( ctx%failed() ) return
      associate ( ps => ctx%spice%pool )
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      call ps%create ( name, nameat )
      call ps%clear_values ( nameat )
      ! PEPPER: the source of the assignment
      if ( allocated ( ps%setter ) ) then
         call ps%note_source ( nameat, ps%setter, 0_ip )
      else
         call ps%note_source ( nameat, '(PCPOOL)', 0_ip )
      end if
      do i = 1, n
         call ps%add_string ( nameat, cvals(i) )
      end do
      call pool_changed ( ctx%spice, name )
      end associate
      end subroutine pcpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting double precision data into the kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the kernel pool variable to associate
      !>              with the values supplied in the array VALUES
      !>
      !>     N        is the number of values to insert into the kernel pool.
      !>
      !>     VALUES   is an array of d.p. values to insert into the kernel
      !>              pool.
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
      !>     1)  If NAME is already present in the kernel pool and there
      !>         is sufficient room to hold all values supplied in VALUES,
      !>         the old values associated with NAME will be overwritten.
      !>
      !>     2)  If there is not sufficient room to insert a new variable into
      !>         the kernel pool and NAME is not already present in the kernel
      !>         pool, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     3)  If there is not sufficient room to insert the values
      !>         associated with NAME, the error SPICE(NOMOREROOM) is signaled.
      !>
      !>     4)  If the kernel pool variable name length exceeds its maximum
      !>         allowed length (see Kernel Required Reading, kernel.req), the
      !>         error SPICE(BADVARNAME) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This entry point provides a programmatic interface for inserting
      !>     data into the SPICE kernel pool without reading an external file.
      !>
      !>## Examples
      !>
      !>     Suppose that you wish to supply default values for a program
      !>     so that it may function even in the absence of the appropriate
      !>     text kernels. You can use the entry points PCPOOL, PDPOOL
      !>     and PIPOOL to initialize the kernel pool with suitable
      !>     values at program initialization. The example below shows
      !>     how you might set up various kernel pool variables that might
      !>     be required by a program.
      !>
      !>        Set up the relationship between the EARTH_BODYFIXED frame
      !>        and the IAU_EARTH frame.
      !>
      !>        CALL IDENT  ( MATRIX )
      !>        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX'    )
      !>        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1, 'IAU_EARTH' )
      !>        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX )
      !>
      !>        Load the IAU model for the earth's rotation and shape.
      !>
      !>        RA ( 1 ) =  0.0D0
      !>        RA ( 2 ) = -0.641D0
      !>        RA ( 3 ) =  0.0D0
      !>
      !>        DEC( 1 ) = 90.0D0
      !>        DEC( 2 ) = -0.557D0
      !>        DEC( 3 ) =  0.0D0
      !>
      !>        PM ( 1 ) = 190.16D0
      !>        PM ( 2 ) = 360.9856235D0
      !>        PM ( 3 ) =   0.0D0
      !>
      !>        R  ( 1 ) =  6378.140D0
      !>        R  ( 2 ) =  6378.140D0
      !>        R  ( 3 ) =  6356.75D0
      !>
      !>        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
      !>        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
      !>        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
      !>        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
      !>
      !>        Set up a preliminary set of leapsecond values.
      !>
      !>        CALL PDPOOL ( 'DELTET/DELTA_T_A', 1, 32.184D0  )
      !>        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
      !>        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
      !>
      !>        VALUES(1) = 6.23999600D0
      !>        VALUES(2) = 1.99096871D-7
      !>
      !>        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
      !>
      !>        VALUES(  1 ) = 10
      !>        VALUES(  3 ) = 11
      !>        VALUES(  5 ) = 12
      !>        VALUES(  7 ) = 13
      !>        VALUES(  9 ) = 14
      !>        VALUES( 11 ) = 15
      !>        VALUES( 13 ) = 16
      !>        VALUES( 15 ) = 17
      !>        VALUES( 17 ) = 18
      !>        VALUES( 19 ) = 19
      !>        VALUES( 21 ) = 20
      !>        VALUES( 23 ) = 21
      !>        VALUES( 25 ) = 22
      !>        VALUES( 27 ) = 23
      !>        VALUES( 29 ) = 24
      !>        VALUES( 31 ) = 25
      !>        VALUES( 33 ) = 26
      !>        VALUES( 35 ) = 27
      !>        VALUES( 37 ) = 28
      !>        VALUES( 39 ) = 29
      !>        VALUES( 41 ) = 30
      !>        VALUES( 43 ) = 31
      !>
      !>        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
      !>        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
      !>        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
      !>        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
      !>        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
      !>        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
      !>        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
      !>        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
      !>        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
      !>        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
      !>        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
      !>        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
      !>        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
      !>        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
      !>        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
      !>        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
      !>        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
      !>        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
      !>        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
      !>        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
      !>        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
      !>        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
      !>
      !>        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 9.1.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
      !>
      !>        Updated to increment POOL state counter.
      !>        Updated $Index_Entries section.
      !>
      !>-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
      !>
      !>        Added an error check on the length of the kernel pool variable
      !>        name argument to enforce the variable name length does not
      !>        exceed MAXLEN.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory instead
      !>        of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      module subroutine pdpool ( ctx, name, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! The kernel pool name to associate with VALUES.
      integer(ip), intent(in) :: n  !! The number of values to insert.
      real(wp), intent(in) :: values    ( * )  !! An array of values to insert into the kernel
                                               !! pool.
      integer(ip) :: i
      integer(ip) :: nameat
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
!     PEPPER: names have any length (SPICELIB signaled SPICE(BADVARNAME)
!     for more than 32 characters), and there is no limit on the number
!     of values (SPICE(NOMOREROOM), SPICE(KERNELPOOLFULL)).
!
      if ( n <= 0 ) return
      if ( ctx%failed() ) return
      associate ( ps => ctx%spice%pool )
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      call ps%create ( name, nameat )
      call ps%clear_values ( nameat )
      ! PEPPER: the source of the assignment
      if ( allocated ( ps%setter ) ) then
         call ps%note_source ( nameat, ps%setter, 0_ip )
      else
         call ps%note_source ( nameat, '(PDPOOL)', 0_ip )
      end if
      do i = 1, n
         call ps%add_number ( nameat, values(i) )
      end do
      call pool_changed ( ctx%spice, name )
      end associate
      end subroutine pdpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting integer data into the kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the kernel pool variable to associate
      !>              with the values supplied in the array IVALS
      !>
      !>     N        is the number of values to insert into the kernel pool.
      !>
      !>     IVALS    is an array of integers to insert into the kernel
      !>              pool.
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
      !>     1)  If NAME is already present in the kernel pool and there
      !>         is sufficient room to hold all values supplied in IVALS,
      !>         the old values associated with NAME will be overwritten.
      !>
      !>     2)  If there is not sufficient room to insert a new variable into
      !>         the kernel pool and NAME is not already present in the kernel
      !>         pool, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     3)  If there is not sufficient room to insert the values
      !>         associated with NAME, the error SPICE(NOMOREROOM) is signaled.
      !>
      !>     4)  If the kernel pool variable name length exceeds its maximum
      !>         allowed length (see Kernel Required Reading, kernel.req), the
      !>         error SPICE(BADVARNAME) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This entry point provides a programmatic interface for inserting
      !>     data into the SPICE kernel pool without reading an external file.
      !>
      !>## Examples
      !>
      !>     Suppose that you wish to supply default values for a program
      !>     so that it may function even in the absence of the appropriate
      !>     text kernels. You can use the entry points PCPOOL, PDPOOL
      !>     and PIPOOL to initialize the kernel pool with suitable
      !>     values at program initialization. The example below shows
      !>     how you might set up various kernel pool variables that might
      !>     be required by a program.
      !>
      !>        Set up the relationship between the EARTH_BODYFIXED frame
      !>        and the IAU_EARTH frame.
      !>
      !>        CALL IDENT ( MATRIX )
      !>        CALL PCPOOL ( 'TKFRAME_EARTH_FIXED_SPEC',     1, 'MATRIX' )
      !>        CALL PIPOOL ( 'TKFRAME_EARTH_FIXED_RELATIVE', 1,  10081   )
      !>        CALL PDPOOL ( 'TKFRAME_EARTH_FIXED_MATRIX',   9,  MATRIX  )
      !>
      !>        Load the IAU model for the earth's rotation and shape.
      !>
      !>        RA ( 1 ) =  0.0D0
      !>        RA ( 2 ) = -0.641D0
      !>        RA ( 3 ) =  0.0D0
      !>
      !>        DEC( 1 ) = 90.0D0
      !>        DEC( 2 ) = -0.557D0
      !>        DEC( 3 ) =  0.0D0
      !>
      !>        PM ( 1 ) = 190.16D0
      !>        PM ( 2 ) = 360.9856235D0
      !>        PM ( 3 ) =   0.0D0
      !>
      !>        R  ( 1 ) =  6378.140D0
      !>        R  ( 2 ) =  6378.140D0
      !>        R  ( 3 ) =  6356.75D0
      !>
      !>        CALL PDPOOL ( 'BODY399_POLE_RA',   3, RA  )
      !>        CALL PDPOOL ( 'BODY399_POLE_DEC',  3, DEC )
      !>        CALL PDPOOL ( 'BODY399_PM',        3, PM  )
      !>        CALL PDPOOL ( 'BODY399_RADII',     3, R   )
      !>
      !>        Set up a preliminary set of leapsecond values.
      !>
      !>        CALL PDPOOL ( 'DELTET/DELTA_T_A/',1, 32.184D0  )
      !>        CALL PDPOOL ( 'DELTET/K',         1,  1.657D-3 )
      !>        CALL PDPOOL ( 'DELTET/EB',        1,  1.671D-2 )
      !>
      !>        VALUES(1) = 6.23999600D0
      !>        VALUES(2) = 1.99096871D-7
      !>
      !>        CALL PDPOOL ( 'DELTET/M', 2, VALUES )
      !>
      !>        VALUES(  1 ) = 10
      !>        VALUES(  3 ) = 11
      !>        VALUES(  5 ) = 12
      !>        VALUES(  7 ) = 13
      !>        VALUES(  9 ) = 14
      !>        VALUES( 11 ) = 15
      !>        VALUES( 13 ) = 16
      !>        VALUES( 15 ) = 17
      !>        VALUES( 17 ) = 18
      !>        VALUES( 19 ) = 19
      !>        VALUES( 21 ) = 20
      !>        VALUES( 23 ) = 21
      !>        VALUES( 25 ) = 22
      !>        VALUES( 27 ) = 23
      !>        VALUES( 29 ) = 24
      !>        VALUES( 31 ) = 25
      !>        VALUES( 33 ) = 26
      !>        VALUES( 35 ) = 27
      !>        VALUES( 37 ) = 28
      !>        VALUES( 39 ) = 29
      !>        VALUES( 41 ) = 30
      !>        VALUES( 43 ) = 31
      !>
      !>        CALL TPARSE ( '1972-JAN-1', VALUES(2),  ERROR )
      !>        CALL TPARSE ( '1972-JUL-1', VALUES(4),  ERROR )
      !>        CALL TPARSE ( '1973-JAN-1', VALUES(6),  ERROR )
      !>        CALL TPARSE ( '1974-JAN-1', VALUES(8),  ERROR )
      !>        CALL TPARSE ( '1975-JAN-1', VALUES(10), ERROR )
      !>        CALL TPARSE ( '1976-JAN-1', VALUES(12), ERROR )
      !>        CALL TPARSE ( '1977-JAN-1', VALUES(14), ERROR )
      !>        CALL TPARSE ( '1978-JAN-1', VALUES(16), ERROR )
      !>        CALL TPARSE ( '1979-JAN-1', VALUES(18), ERROR )
      !>        CALL TPARSE ( '1980-JAN-1', VALUES(20), ERROR )
      !>        CALL TPARSE ( '1981-JUL-1', VALUES(22), ERROR )
      !>        CALL TPARSE ( '1982-JUL-1', VALUES(24), ERROR )
      !>        CALL TPARSE ( '1983-JUL-1', VALUES(26), ERROR )
      !>        CALL TPARSE ( '1985-JUL-1', VALUES(28), ERROR )
      !>        CALL TPARSE ( '1988-JAN-1', VALUES(30), ERROR )
      !>        CALL TPARSE ( '1990-JAN-1', VALUES(32), ERROR )
      !>        CALL TPARSE ( '1991-JAN-1', VALUES(34), ERROR )
      !>        CALL TPARSE ( '1992-JUL-1', VALUES(36), ERROR )
      !>        CALL TPARSE ( '1993-JUL-1', VALUES(38), ERROR )
      !>        CALL TPARSE ( '1994-JUL-1', VALUES(40), ERROR )
      !>        CALL TPARSE ( '1996-JAN-1', VALUES(42), ERROR )
      !>        CALL TPARSE ( '1997-JUL-1', VALUES(44), ERROR )
      !>
      !>        CALL PDPOOL ( 'DELTET/DELTA_AT',  44, VALUES )
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 9.1.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 9.1.0, 17-JAN-2014 (BVS) (NJB)
      !>
      !>        Updated to increment POOL state counter.
      !>        Updated $Index_Entries section.
      !>
      !>-    SPICELIB Version 9.0.0, 24-MAY-2010 (EDW)
      !>
      !>        Added an error check on the length of the kernel pool variable
      !>        name argument to enforce the variable name length does not
      !>        exceed MAXLEN.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory instead
      !>        of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      module subroutine pipool ( ctx, name, n, ivals )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! The kernel pool name to associate with IVALS.
      integer(ip), intent(in) :: n  !! The number of values to insert.
      integer(ip), intent(in) :: ivals     ( * )  !! An array of integers to insert into the
                                                  !! pool.
      integer(ip) :: i
      integer(ip) :: nameat
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
!     PEPPER: names have any length (SPICELIB signaled SPICE(BADVARNAME)
!     for more than 32 characters), and there is no limit on the number
!     of values (SPICE(NOMOREROOM), SPICE(KERNELPOOLFULL)).
!
      if ( n <= 0 ) return
      if ( ctx%failed() ) return
      associate ( ps => ctx%spice%pool )
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      call ps%create ( name, nameat )
      call ps%clear_values ( nameat )
      ! PEPPER: the source of the assignment
      if ( allocated ( ps%setter ) ) then
         call ps%note_source ( nameat, ps%setter, 0_ip )
      else
         call ps%note_source ( nameat, '(PIPOOL)', 0_ip )
      end if
      do i = 1, n
         call ps%add_number ( nameat, real( ivals(i), wp ) )
      end do
      call pool_changed ( ctx%spice, name )
      end associate
      end subroutine pipool

      !> Load the variables contained in an internal buffer into the
      !> kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     CVALS    is an array that contains lines of text that
      !>              could serve as a SPICE text kernel.
      !>
      !>     N        is the number of entries in CVALS.
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
      !>     1)  If any of the kernel pool variables names or their values, as
      !>         provided in the input CVALS array, cannot be parsed, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If there is no room left in the kernel pool to store all
      !>         variables present in the input CVALS array, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the length of any kernel pool variable name present in the
      !>         input CVALS array exceeds its maximum allowed length (see
      !>         Kernel Required Reading, kernel.req), an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows you to store a text kernel in an internal
      !>     array of your program and load this array into the kernel pool
      !>     without first storing its contents as a text kernel.
      !>
      !>## Examples
      !>
      !>     Suppose that your application is not particularly sensitive
      !>     to the current number of leapseconds but that you would
      !>     still like to use a relatively recent leapseconds kernel
      !>     without requiring users to load a leapseconds kernel into
      !>     the program. The example below shows how you might set up
      !>     the initialization portion of your program.
      !>
      !>        INTEGER               LNSIZE
      !>        PARAMETER           ( LNSIZE = 80 )
      !>
      !>        CHARACTER*(LNSIZE)    TEXT ( 27 )
      !>
      !>        TEXT(  1 ) = 'DELTET/DELTA_T_A =   32.184'
      !>        TEXT(  2 ) = 'DELTET/K         =    1.657D-3'
      !>        TEXT(  3 ) = 'DELTET/EB        =    1.671D-2'
      !>        TEXT(  4 ) = 'DELTET/M = (  6.239996D0   1.99096871D-7 )'
      !>        TEXT(  5 ) = 'DELTET/DELTA_AT  = ( 10,   @1972-JAN-1'
      !>        TEXT(  6 ) = '                     11,   @1972-JUL-1'
      !>        TEXT(  7 ) = '                     12,   @1973-JAN-1'
      !>        TEXT(  8 ) = '                     13,   @1974-JAN-1'
      !>        TEXT(  9 ) = '                     14,   @1975-JAN-1'
      !>        TEXT( 10 ) = '                     15,   @1976-JAN-1'
      !>        TEXT( 11 ) = '                     16,   @1977-JAN-1'
      !>        TEXT( 12 ) = '                     17,   @1978-JAN-1'
      !>        TEXT( 13 ) = '                     18,   @1979-JAN-1'
      !>        TEXT( 14 ) = '                     19,   @1980-JAN-1'
      !>        TEXT( 15 ) = '                     20,   @1981-JUL-1'
      !>        TEXT( 16 ) = '                     21,   @1982-JUL-1'
      !>        TEXT( 17 ) = '                     22,   @1983-JUL-1'
      !>        TEXT( 18 ) = '                     23,   @1985-JUL-1'
      !>        TEXT( 19 ) = '                     24,   @1988-JAN-1'
      !>        TEXT( 20 ) = '                     25,   @1990-JAN-1'
      !>        TEXT( 21 ) = '                     26,   @1991-JAN-1'
      !>        TEXT( 22 ) = '                     27,   @1992-JUL-1'
      !>        TEXT( 23 ) = '                     28,   @1993-JUL-1'
      !>        TEXT( 24 ) = '                     29,   @1994-JUL-1'
      !>        TEXT( 25 ) = '                     30,   @1996-JAN-1'
      !>        TEXT( 26 ) = '                     31,   @1997-JUL-1'
      !>        TEXT( 27 ) = '                     32,   @1999-JAN-1 )'
      !>
      !>        CALL LMPOOL ( TEXT, 27 )
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.3.1, 17-AUG-2021 (JDR) (BVS)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated $Exceptions section to better describe the error
      !>        handling of this routine.
      !>
      !>-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
      !>
      !>        Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.2.0, 10-FEB-2010 (EDW)
      !>
      !>        Added mention of the restriction on kernel pool variable
      !>        names to MAXLEN characters or less.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      module subroutine lmpool ( ctx, cvals, n )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! The number of entries in CVALS.
      character(*), intent(in) :: cvals     ( * )  !! An array that contains a SPICE text kernel
      character(:), allocatable :: varnam
      integer(ip) :: linnum
      logical :: eof
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      linnum = 1
      call zzrvbf ( ctx, cvals, n, linnum, ps, varnam, eof )
      do while ( .not. eof .and. .not. ctx%failed() )
         if ( varnam /= ' ' ) then
            call pool_changed ( ctx%spice, varnam )
         end if
         call zzrvbf ( ctx, cvals, n, linnum, ps, varnam, eof )
      end do
      end associate
      end subroutine lmpool

      !> Return the kernel pool size limitations.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of a kernel pool size parameter.
      !>              The following parameters may be specified.
      !>
      !>                 'MAXVAR'    is the maximum number of variables that
      !>                             the kernel pool may contain at any one
      !>                             time. MAXVAR should be a prime number.
      !>
      !>                 'MAXLEN'    is the maximum length of the variable
      !>                             names that can be stored in the kernel
      !>                             pool.
      !>
      !>                 'MAXVAL'    is the maximum number of distinct values
      !>                             that may belong to the variables in the
      !>                             kernel pool. Each variable must have at
      !>                             least one value, and may have any number,
      !>                             so long as the total number does not
      !>                             exceed MAXVAL. MAXVAL must be at least as
      !>                             large as MAXVAR.
      !>
      !>                 'MXNOTE'    is the maximum number of distinct
      !>                             variable-agents pairs that can be
      !>                             maintained by the kernel pool. (A variable
      !>                             is "paired" with an agent, if that agent
      !>                             is to be notified whenever the variable is
      !>                             updated.)
      !>
      !>                 'MAXAGT'    is the maximum number of agents that can
      !>                             be kept on the distribution list for
      !>                             notification of updates to kernel
      !>                             variables.
      !>
      !>                 'MAXCHR'    is the maximum number of characters that
      !>                             can be stored in a component of a string
      !>                             valued kernel variable.
      !>
      !>                 'MAXLIN'    is the maximum number of character strings
      !>                             that can be stored as data for kernel pool
      !>                             variables.
      !>
      !>              Note that the case of NAME is insignificant.
      !>
      !>## Detailed Output
      !>
      !>     N        is the value of the parameter specified by NAME. If
      !>              NAME is not one of the items specified above, N will
      !>              be returned with the value 0.
      !>
      !>     FOUND    is .TRUE. if the parameter is recognized .FALSE. if it
      !>              is not.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified parameter is not recognized, the value of N
      !>         returned will be zero and FOUND will be set to .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the a programmatic interface to the
      !>     parameters used to define the kernel pool. It is not
      !>     anticipated that most kernel pool users will need to use this
      !>     routine.
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
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated NAME
      !>        description to not refer to the main routine.
      !>
      !>-    SPICELIB Version 1.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      module subroutine szpool ( ctx, name, n, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the parameter to be returned.
      integer(ip), intent(out) :: n  !! Value of parameter specified by NAME.
      logical, intent(out) :: found  !! .TRUE. if NAME is recognized.
!
!     PEPPER: the pool has none of these limits (variables, values,
!     string values and their length, name length, agents, notices):
!     each is reported as HUGE(1), "unlimited".
!
      if ( ctx%failed() ) return
      found = .true.
      if (      eqstr( name, 'MAXVAR' ) .or. eqstr( name, 'MAXVAL' ) &
           .or. eqstr( name, 'MAXLIN' ) .or. eqstr( name, 'MAXCHR' ) &
           .or. eqstr( name, 'MXNOTE' ) .or. eqstr( name, 'MAXLEN' ) &
           .or. eqstr( name, 'MAXAGT' ) ) then
         n = huge ( 1_ip )
      else
         n     = 0
         found = .false.
      end if
      end subroutine szpool

      !> Delete a variable from the kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the kernel pool variable to delete.
      !>              The name and associated values are removed from the
      !>              kernel pool, freeing the occupied space.
      !>
      !>              If a watches are set on the variable designated by
      !>              NAME, the corresponding agents are placed on the list
      !>              of agents to be notified of a kernel variable update.
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
      !>     1)  If the specified variable is not present in the kernel pool,
      !>         this routine simply returns. No error is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine enables users to selectively remove variables from
      !>     the kernel pool, as opposed to having to clear the pool and
      !>     reload it.
      !>
      !>     Note that it is not necessary to remove kernel variables in order
      !>     to simply update them; this routine should be used only when
      !>     variables are to be removed.
      !>
      !>## Examples
      !>
      !>     1)  Remove triaxial radii of Jupiter from the kernel pool.
      !>
      !>            CALL DVPOOL ( 'BODY599_RADII' )
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
      !>-    SPICELIB Version 8.3.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Moved example from $Restrictions to $Examples section.
      !>
      !>-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
      !>
      !>        Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.2.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.1.0, 22-DEC-2004 (NJB)
      !>
      !>        Bug fix: corrected logic for determining when a
      !>        conflict resolution list is non-empty.
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (NJB) (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.3.0, 30-JUL-2013 (BVS)
      !>
      !>        Updated to increment POOL state counter.
      !>
      !>-    SPICELIB Version 8.2.0, 19-MAR-2009 (NJB)
      !>
      !>        Watcher update code was re-written for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.1.0, 22-DEC-2004 (NJB)
      !>
      !>        Bug fix: corrected logic for determining when a
      !>        conflict resolution list is non-empty. The test
      !>
      !>           IF ( NAMEAT .LT. 0 ) THEN
      !>
      !>        formerly tested the variable NODE instead of NAMEAT.
      !>
      !>        Corrected an in-line comment relating to finding the
      !>        head node of the conflict resolution list for NAME.
      module subroutine dvpool ( ctx, name )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the variable to be deleted.
      integer(ip) :: i
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
      associate ( ps => ctx%spice%pool )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      call zzctrinc ( ctx, ps%subctr )
      i = ps%find ( name )
      if ( i == 0 ) return
      call ps%remove ( i )
      call pool_changed ( ctx%spice, name )
      end associate
      end subroutine dvpool

      !> Return names of kernel variables matching a specified template.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is a MATCHI template which will be used when searching
      !>              for variable names in the kernel pool. The characters
      !>              '*' and '%' are used for the wild string and wild
      !>              characters respectively. For details of string
      !>              pattern matching see the header of the routine MATCHI.
      !>
      !>     START    is the index of the first variable name to return that
      !>              matches the NAME template. The matching names are
      !>              assigned indices ranging from 1 to NVAR, where NVAR is
      !>              the number of matching names. The index of a name does
      !>              not indicate how it compares alphabetically to another
      !>              name.
      !>
      !>              If START is less than 1, it will be treated as 1. If
      !>              START is greater than the total number of matching
      !>              variable names, no values will be returned and N will
      !>              be set to zero. However, FOUND will still be set to
      !>              .TRUE.
      !>
      !>     ROOM     is the maximum number of variable names that should
      !>              be returned for this template. If ROOM is less than 1
      !>              the error SPICE(BADARRAYSIZE) will be signaled.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of variable names matching NAME that are
      !>              returned. It will always be less than or equal to
      !>              ROOM.
      !>
      !>              If no variable names match NAME, N is set to zero.
      !>
      !>     CVALS    is an array of kernel pool variables whose names match
      !>              the template NAME and which have indices ranging from
      !>              START to START+N-1.
      !>
      !>              Note that in general the names returned in CVALS are
      !>              not sorted.
      !>
      !>              If no variables match NAME, no values are assigned to
      !>              the elements of CVALS.
      !>
      !>              If the length of CVALS is less than the length of the
      !>              variable names, the values returned will be truncated
      !>              on the right. To ensure that names are not truncated,
      !>              CVALS should be declared to be at least
      !>              CHARACTER*(32).
      !>
      !>     FOUND    is .TRUE. if the some variable name in the kernel pool
      !>              matches NAME, .FALSE. if it is not.
      !>
      !>## Parameters
      !>
      !>     MAXLEN   is the maximum length of the variable names that
      !>              can be stored in the kernel pool. This value is
      !>              currently 32.
      !>
      !>## Exceptions
      !>
      !>     1)  If the value of ROOM is less than one, the error
      !>         SPICE(BADARRAYSIZE) is signaled.
      !>
      !>     2)  If CVALS has declared length less than the size of a variable
      !>         name to be returned, the name will be truncated on the right.
      !>         See MAXLEN for the maximum size of variable names.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides the user interface for retrieving the names
      !>     of kernel pool variables. This interface allows you to retrieve
      !>     the names matching a template via multiple accesses. Under some
      !>     circumstances this alleviates the problem of having to know in
      !>     advance the maximum amount of space needed to accommodate all
      !>     matching names.
      !>
      !>     However, this method of access does come with a price. It is
      !>     always more efficient to retrieve all of the data associated with
      !>     a kernel pool variable in one call than it is to retrieve it in
      !>     sections. The parameter MAXVAR defines the upper bound on the
      !>     number of possible matching names.
      !>
      !>## Examples
      !>
      !>     The following code fragment demonstrates how the names of kernel
      !>     pool variables matching a template can be retrieved in pieces.
      !>
      !>     First we need some declarations.
      !>
      !>        INTEGER               ROOM
      !>        PARAMETER           ( ROOM = 3 )
      !>
      !>        CHARACTER*(3)         INDENT
      !>        CHARACTER*(80)        CVALS  (ROOM)
      !>        CHARACTER*(8)         VARNAM
      !>
      !>        INTEGER               START
      !>        INTEGER               N
      !>
      !>        LOGICAL               FOUND
      !>
      !>     Next load the data in the file 'typical.ker' into the
      !>     kernel pool.
      !>
      !>        CALL LDPOOL ( 'typical.ker' )
      !>
      !>     Next we shall print the names of kernel variables that match the
      !>     template 'BODY599*'.
      !>
      !>        VARNAM = 'BODY599*'
      !>        INDENT = ' '
      !>        START  =  1
      !>
      !>        CALL GNPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>
      !>           WRITE (*,*) 'There are no matching variables ' //
      !>       .               'in the kernel pool.'
      !>        ELSE
      !>
      !>           WRITE (*,*) 'Kernel pool variables:'
      !>           WRITE (*,*)
      !>
      !>           DO I = 1, N
      !>              WRITE (*,*) INDENT, CVALS(I)
      !>           END DO
      !>
      !>           DO WHILE ( N .EQ. ROOM )
      !>
      !>              START = START + N
      !>              CALL GNPOOL ( VARNAM, START, ROOM, N, CVALS, FOUND )
      !>
      !>              DO I = 1, N
      !>                 WRITE (*,*) INDENT, CVALS(I)
      !>              END DO
      !>
      !>           END DO
      !>
      !>        END IF
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
      !>-    SPICELIB Version 8.1.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added MAXLEN
      !>        description in $Parameters.
      !>
      !>-    SPICELIB Version 8.1.0, 19-MAR-2009 (NJB)
      !>
      !>        ZZPINI call was updated for compatibility
      !>        with new watcher system implementation.
      !>
      !>-    SPICELIB Version 8.0.0, 04-JUN-1999 (WLT)
      !>
      !>        Added the entry points PCPOOL, PDPOOL and PIPOOL to allow
      !>        direct insertion of data into the kernel pool without having
      !>        to read an external file.
      !>
      !>        Added the interface LMPOOL that allows SPICE
      !>        programs to load text kernels directly from memory
      !>        instead of requiring a text file.
      !>
      !>        Added the entry point SZPOOL to return kernel pool definition
      !>        parameters.
      !>
      !>        Added the entry point DVPOOL to allow the removal of a variable
      !>        from the kernel pool.
      !>
      !>        Added the entry point GNPOOL to allow users to determine
      !>        variables that are present in the kernel pool
      module subroutine gnpool ( ctx, name, start, room, n, cvals, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: matchi
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Template that names should match.
      integer(ip), intent(out) :: n  !! Number of values returned for NAME.
      logical, intent(out) :: found  !! .TRUE. if there is at least one match.
      integer(ip), intent(in) :: start  !! Index of first matching name to retrieve.
      integer(ip), intent(in) :: room  !! The largest number of values to return.
      character(*), intent(out) :: cvals     ( * )  !! Kernel pool variables whose names match
                                                    !! NAME.
      integer(ip) :: hits
      integer(ip) :: k
!
!     PEPPER: the pool is the records of POOL_STATE (pepper_states), with
!     names and values of any length and number; SPICELIB's hash table
!     and linked lists, and their limits, are gone.
!
!     The variables are searched in the order of their slots (the order
!     in which they were created; SPICELIB's was the order of its hash
!     table).
!
      associate ( ps => ctx%spice%pool )
      if ( ctx%failed() ) return
      if ( ps%first ) call zzpini ( ctx, ps )
      if ( room < 1 ) then
         call ctx%setmsg ( 'The amount of room specified as ' &
         //            'available for output in the output array ' &
         //            'was: #.  The amount of room must be ' &
         //            'positive. ' )
         call ctx%errint ( '#', room )
         call ctx%sigerr ( 'SPICE(BADARRAYSIZE)'  )
         return
      end if
      hits  = 0
      n     = 0
      do k = 1, ps%nslots
         if ( .not. allocated ( ps%vars(k)%name ) ) cycle
         if ( matchi( ps%vars(k)%name, name, '*', '%' ) ) then
            hits = hits + 1
            if ( hits >= start ) then
               if ( n < room ) then
                  n        = n + 1
                  cvals(n) = ps%vars(k)%name
               end if
               if ( n == room ) then
                  found = .true.
                  return
               end if
            end if
         end if
      end do
      found = hits > 0
      end associate
      end subroutine gnpool

      !> Delete a name from the list of agents to notify whenever a member
      !> of a list of kernel variables is updated.
      !>
      !>## Detailed Input
      !>
      !>     AGENT    is any agent name that has previously been associated
      !>              with a kernel pool watch via a call to SWPOOL. The
      !>              agent name will be deleted from the notification list
      !>              of every watched kernel variable.
      !>
      !>              Watched variables whose notification lists become
      !>              empty will be deleted.
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
      !>     1)  It's not an error to delete a non-existent agent---one
      !>         that is not present in the watcher system. A call to
      !>         delete a non-existent agent has no effect on the state
      !>         of the watcher system.
      !>
      !>     2)  If an attempt is made to delete an agent that
      !>         has an unchecked update, the error SPICE(UPDATEPENDING)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Kernel pool watches are a limited resource; the ability
      !>     to delete watches when they're no longer needed is essential
      !>     to allow programs that make heavy use of kernel pool watches
      !>     to run for extended periods.
      !>
      !>## Examples
      !>
      !>     Suppose that you have an application subroutine, MYTASK, that
      !>     needs to access a large data set in the kernel pool. If this
      !>     data could be kept in local storage and kernel pool queries
      !>     performed only when the data in the kernel pool has been
      !>     updated, the routine can perform much more efficiently.
      !>
      !>     If at some point the local stored data no longer need to be
      !>     watched---for example, if they're removed from the local
      !>     buffer to make room for other data---the watch set by the
      !>     agent 'MYTASK' on those data can be deleted via the call
      !>
      !>        CALL DWPOOL ( 'MYTASK' )
      !>
      !>## Restrictions
      !>
      !>     1)  It is recommended that watches be deleted only by
      !>         routines that established them.
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
      !>-    SPICELIB Version 1.1.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 11-SEP-2013 (BVS)
      !>
      !>        Updated to increment POOL state counter. Updated description
      !>        of the exception 1).
      !>
      !>-    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB)
      module subroutine dwpool ( ctx, agent )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: agent  !! The name of an agent to be notified after updates.
!
!     PEPPER: each context keeps its own watches,
!     checked against the instance's log of the pool's changes (see
!     swpool_context in pepper_instance_module): contexts do not take each
!     other's notices, and a watch set while other contexts read the
!     pool changes nothing they use. A notice is posted for any update
!     of a watched variable (loaded, set or deleted) since the last
!     check, as by SPICELIB, and for every variable after CLPOOL.
!
      if ( ctx%failed() ) return
      call dwpool_context ( ctx, agent )

      return
      end subroutine dwpool

      !> Check and update the POOL state counter tracked by a caller
      !> (user) routine.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     USRCTR   is the value of the POOL state counter tracked by
      !>              (saved in) the caller (user) routine.
      !>
      !>## Detailed Output
      !>
      !>     USRCTR   is the current POOL state counter.
      !>
      !>     UPDATE   is the logical flag indicating whether the input POOL
      !>              state counter was different from the current POOL
      !>              state counter and, therefore, had to be updated
      !>              (UPDATE = .TRUE.) or if it was the same (UPDATE =
      !>              .FALSE.).
      !>
      !>## Parameters
      !>
      !>     CTRSIZ   is the dimension of the counter array used by
      !>              various SPICE subsystems to uniquely identify
      !>              changes in their states. This parameter is
      !>              defined in the private include file 'zzctr.inc'.
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
      !>     This routine is not part of the SPICELIB API. This routine
      !>     may be removed in a later version of the SPICE Toolkit, or
      !>     its interface may change.
      !>
      !>     SPICE-based application code should not call this routine.
      !>
      !>     This routine allows other routines to be aware of POOL state
      !>     change due to addition or deletion variables or watchers. Such
      !>     awareness is needed to be able to locally save some POOL-based
      !>     data (e.g. body name/ID mappings or frame definitions) and only
      !>     update these locally saved values if the POOL has changed.
      !>
      !>     To make use of the POOL state counter to achieve this goal the
      !>     caller routines save the POOL state counter returned by the first
      !>     call to this routine and then check that saved value against the
      !>     current POOL state counter and update it by subsequent calls this
      !>     routine.
      !>
      !>## Examples
      !>
      !>     The routines that need to be aware of and act on the POOL state
      !>     change initialize a local POOL counter array using ZZCTRUIN, save
      !>     it, and check it against the current POOL state counter and
      !>     update it, if needed, using this entry point, as follows:
      !>
      !>        C
      !>        C     Include zzctr.inc to access CTRSIZ.
      !>        C
      !>              INCLUDE              'zzctr.inc'
      !>              ...
      !>
      !>        C
      !>        C     In local variable declarations declare and save
      !>        C     the local POOL state counter. Also declare the
      !>        C     update flag.
      !>        C
      !>              INTEGER               USRCTR ( CTRSIZ )
      !>              LOGICAL               UPDATE
      !>              ...
      !>              SAVE                  USRCTR
      !>              ...
      !>
      !>        C
      !>        C     In all places where initialization is done
      !>        C     initialize the local POOL state counter using
      !>        C     ZZCTRUIN to ensure an update on the first check.
      !>        C
      !>              IF ( FIRST ) THEN
      !>                 ...
      !>                 CALL ZZCTRUIN( USRCTR )
      !>                 FIRST = .FALSE.
      !>              END IF
      !>              ...
      !>
      !>        C
      !>        C     In all places where there is a need to check for
      !>        C     the POOL state change call this entry to
      !>        C     check and update the local POOL state counter.
      !>        C
      !>              CALL ZZPCTRCK ( USRCTR, UPDATE )
      !>
      !>              IF ( UPDATE ) THEN
      !>
      !>        C
      !>        C        It the POOL state changed, do what needs to
      !>        C        be done to deal with saved values based
      !>        C        on POOL data.
      !>        C
      !>                 ...
      !>
      !>              END IF
      !>
      !>## Restrictions
      !>
      !>     1)  This is a private routine. See $Particulars above.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 17-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 27-MAR-2014 (BVS)
      module subroutine zzpctrck ( ctx, usrctr, update )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(out) :: update  !! Flag indicating if input counter was updated
      integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! POOL state counter tracked by the
                                                          !! caller
      if ( ctx%failed() ) return
      call zzctrchk ( ctx, ctx%spice%pool%subctr, usrctr, update )
      end subroutine zzpctrck

   !> True if a file is a JSON kernel: its name ends in `.json` (in any
   !> case). PEPPER: no SPICELIB counterpart.
   pure logical function is_json_kernel(fname)
      character(*), intent(in) :: fname  !! the file name
      integer :: n, k
      character(5) :: ext
      is_json_kernel = .false.
      n = len_trim(fname)
      if (n < 5) return
      ext = fname(n - 4:n)
      do k = 2, 5
         if (ext(k:k) >= 'A' .and. ext(k:k) <= 'Z') ext(k:k) = achar(iachar(ext(k:k)) + 32)
      end do
      is_json_kernel = ext == '.json'
   end function is_json_kernel

   !> Load a JSON kernel (see [[pepper_json]]) into the kernel pool: the
   !> text kernel [[ldpool]] reads when the file is named `*.json`.
   !>
   !> The load is atomic: the file is read whole and parsed, the `@` times
   !> are converted and the types of the appends are checked before the
   !> pool changes, so that a kernel with an error loads nothing (a text
   !> kernel's variables before an error stay loaded). The errors:
   !>
   !>  - SPICE(FILEOPENFAILED), SPICE(FILEREADFAILED): the file;
   !>  - SPICE(JSONSYNTAXERROR): the JSON;
   !>  - SPICE(BADVARNAME): an empty name, or one with blanks or characters
   !>    that are not printable ASCII;
   !>  - SPICE(BADVARASSIGN): `null`, a nested object or array, an empty
   !>    array, an array that mixes strings with numbers or times;
   !>  - SPICE(BADTIMESPEC): a time (`"@..."`) TPARSE does not accept;
   !>  - SPICE(TYPEMISMATCH): an append (`"+NAME"`) of strings to a numeric
   !>    variable, or of numbers or times to a character variable.
   !>
   !> PEPPER: no SPICELIB counterpart.
   subroutine ldjson(ctx, ps, fname)
      use pepper_json, only: json_assignment, json_kernel_parse, json_numeric, json_character
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(pool_state), intent(inout) :: ps  !! the kernel pool
      character(*), intent(in) :: fname  !! the JSON kernel
      character(:), allocatable :: text, short, long
      type(json_assignment), allocatable :: items(:)
      type(string_map) :: kinds
      character(len=1024) :: errmsg
      integer(ip) :: nitems, line, k, j, at, kind
      integer(int64) :: size
      integer :: unit, ios
      logical :: ok, found

      ! the file, whole
      open (newunit=unit, file=trim(fname), access='stream', form='unformatted', action='read', &
            status='old', iostat=ios)
      if (ios /= 0) then
         call ctx%setmsg('The JSON kernel ''#'' could not be opened. IOSTAT was #.')
         call ctx%errch('#', trim(fname))
         call ctx%errint('#', ios)
         call ctx%sigerr('SPICE(FILEOPENFAILED)')
         return
      end if
      inquire (unit=unit, size=size)
      allocate (character(max(size, 0_int64)) :: text)
      ios = 0
      if (size > 0) read (unit, iostat=ios) text
      close (unit)
      if (ios /= 0) then
         call ctx%setmsg('The JSON kernel ''#'' could not be read. IOSTAT was #.')
         call ctx%errch('#', trim(fname))
         call ctx%errint('#', ios)
         call ctx%sigerr('SPICE(FILEREADFAILED)')
         return
      end if

      call ldjson_text(ctx, ps, fname, text)
      deallocate (text)
   end subroutine ldjson

   !> Parse and load a JSON kernel string already in memory into `ps`.
   subroutine ldjson_text(ctx, ps, fname, text)
      use pepper_json, only: json_assignment, json_kernel_parse, json_numeric, json_character
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(pool_state), intent(inout) :: ps  !! the kernel pool
      character(*), intent(in) :: fname  !! virtual or disk file name for error messages
      character(*), intent(in) :: text  !! JSON kernel source text
      character(:), allocatable :: short, long
      type(json_assignment), allocatable :: items(:)
      type(string_map) :: kinds
      character(len=1024) :: errmsg
      integer(ip) :: nitems, line, k, j, at, kind
      logical :: ok, found

      call json_kernel_parse(text, items, nitems, ok, short, long, line)
      if (.not. ok) then
         call ctx%setmsg('Line # of the JSON kernel ''#'': #.')
         call ctx%errint('#', line)
         call ctx%errch('#', trim(fname))
         call ctx%errch('#', long)
         call ctx%sigerr(short)
         return
      end if

      ! the times, and the types of the appends: before anything changes
      do k = 1, nitems
         associate (a => items(k))
            do j = 1, a%n
               if (.not. a%is_time(j)) cycle
               errmsg = ' '
               if (len_trim(a%texts(j)%s) == 0) then
                  errmsg = 'the time is blank'
               else
                  call tparse(ctx, a%texts(j)%s, a%numbers(j), errmsg)
                  if (ctx%failed()) return
               end if
               if (errmsg /= ' ') then
                  call ctx%setmsg('Encountered ''@#'' while attempting to parse a time for the variable ' &
                                  //'''#'' on line # of the JSON kernel ''#''. Error message: ''#''')
                  call ctx%errch('#', a%texts(j)%s)
                  call ctx%errch('#', a%name)
                  call ctx%errint('#', a%line)
                  call ctx%errch('#', trim(fname))
                  call ctx%errch('#', trim(errmsg))
                  call ctx%sigerr('SPICE(BADTIMESPEC)')
                  return
               end if
            end do
            if (a%append) then
               ! the variable's type: set before in this file, or in the pool
               call kinds%get(a%name, kind, found)
               if (.not. found) then
                  kind = 0
                  at = ps%find(a%name)
                  if (at > 0) then
                     if (ps%vars(at)%kind == pool_numeric) kind = json_numeric
                     if (ps%vars(at)%kind == pool_character) kind = json_character
                  end if
               end if
               if (kind /= 0 .and. kind /= a%kind) then
                  call ctx%setmsg('Line # of the JSON kernel ''#'' appends # to the variable ''#'', '// &
                                  'which has #.')
                  call ctx%errint('#', a%line)
                  call ctx%errch('#', trim(fname))
                  if (a%kind == json_numeric) then
                     call ctx%errch('#', 'numbers')
                     call ctx%errch('#', a%name)
                     call ctx%errch('#', 'strings')
                  else
                     call ctx%errch('#', 'strings')
                     call ctx%errch('#', a%name)
                     call ctx%errch('#', 'numbers')
                  end if
                  call ctx%sigerr('SPICE(TYPEMISMATCH)')
                  return
               end if
            end if
            call kinds%set(a%name, int(a%kind, ip))
         end associate
      end do

      ! the pool
      do k = 1, nitems
         associate (a => items(k))
            call ps%create(a%name, at, found)
            if (found .and. .not. a%append) call ps%clear_values(at)
            call ps%note_source(at, fname, a%line)
            if (a%kind == json_numeric) then
               do j = 1, a%n
                  call ps%add_number(at, a%numbers(j))
               end do
            else
               do j = 1, a%n
                  call ps%add_string(at, a%texts(j)%s)
               end do
            end if
            call pool_changed(ctx%spice, a%name)
         end associate
      end do
   end subroutine ldjson_text

end submodule pepper_pool
