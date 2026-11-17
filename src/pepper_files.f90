!> The handle manager of binary kernel files (DAF, DAS): the file table of the instance, the units
!> of each context, file formats, platform attributes and the FTP corruption check.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## ZZDDHMAN (Private --- DAF/DAS Handle Manager)
!>
!> PEPPER: SPICELIB's umbrella routine ZZDDHMAN is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzddhman. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines.  Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     This is an umbrella routine for a collection of entry points
!>     to the DAF/DAS handle manager.
!>
!>### Detailed Input
!>
!>     See the entry points for descriptions of their inputs.
!>
!>### Detailed Output
!>
!>     See the entry points for descriptions of their outputs.
!>
!>### Parameters
!>
!>     See the include file 'zzddhman.inc' for details of parameter
!>     definitions used within this module.
!>
!>### Exceptions
!>
!>     1) If ZZDDHMAN is called directly, the error SPICE(BOGUSENTRY)
!>        is signaled.
!>
!>     2) See entry points ZZDDHOPN, ZZDDHCLS, ZZDDHHLU, ZZDDHUNL,
!>        ZZDDHISN, ZZDDHNFO, ZZDDHFNH, and ZZDDHLUH for exceptions
!>        specific to those entry points.
!>
!>### Files
!>
!>     This set of routines is intended to provide low-level services
!>     for the creation, updating, and reading of Fortran direct access
!>     files utilized by the DAF and DAS systems within SPICE.
!>
!>### Particulars
!>
!>     ZZDDHMAN serves as an umbrella, allowing data to be shared by
!>     its entry points:
!>
!>        ZZDDHOPN       Open file.
!>        ZZDDHCLS       Close file.
!>        ZZDDHHLU       Handle to logical unit.
!>        ZZDDHUNL       Unlock handle from unit.
!>        ZZDDHISN       Is the file native architecture?
!>        ZZDDHNFO       Fetch information about a handle.
!>        ZZDDHFNH       Filename to handle.
!>        ZZDDFLUH       Logical unit to handle.
!>
!>     This umbrella serves a variety of functions to the DAS/DAF
!>     families of routines.
!>
!>        (1) DAF/DAS handle consolidation
!>        (2) Binary file format detection and tracking
!>        (3) FTP error detection services
!>        (4) Logical unit sharing
!>        (5) Filename and unit to handle mapping services
!>
!>### Examples
!>
!>     See individual entry points for pointers to modules that utilize
!>     their capabilities.
!>
!>### Restrictions
!>
!>     1) Changing the current working directory of a program when
!>        more than UTSIZE files are loaded into this interface requires
!>        that all filenames passed into ZZDDHOPN are specified with
!>        absolute pathnames.  Otherwise the OPEN/CLOSE switching
!>        logic will fail to OPEN files that are loaded.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     F.S. Turner     (JPL)
!>     E.D. Wright     (JPL)
!>     B.V. Semenov    (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 2.2.0, 01-OCT-2021 (NJB)
!>
!>        Corrected typos in a long error message and in comments.
!>
!>-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
!>
!>        Added the "magic number" column to the file table.
!>
!>-    SPICELIB Version 2.0.1, 24-APR-2003 (EDW)
!>
!>        Added MAC-OSX-F77 to the list of platforms
!>        that require READONLY to read write protected
!>        kernels.
!>
!>-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
!>
!>        The entry point ZZDDHOPN now invokes ZZPLTCHK, to verify
!>        that the runtime environment's binary file format matches
!>        the one for which the toolkit is configured.
!>
!>        The entry point ZZDDHCLS has had its argument list augmented
!>        to include a "KILL" flag.  Check the entry point header for
!>        details.
!>
!>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!>
!>### Revisions
!>
!>-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
!>
!>        The toolkit source code is far more sensitive to blind
!>        porting of source packaged for one environment to another.
!>        This sensitivity has already caused a few of our users
!>        some difficulty.  In an attempt to address these problems
!>        with future toolkits, ZZDDHOPN now invokes ZZPLTCHK on
!>        it's first pass.  This will perform any necessary checks
!>        on the runtime environment against the values recorded
!>        in ZZPLATFM and other environment specific components
!>        of the library.
!>
!>        As of this release, all that is verified is that the
!>        BFF ID listed in ZZPLATFM is compatible with the runtime
!>        environment.  See ZZPLTCHK's header for details.
!>
!>### Implementation notes
!>
!>     This logical allows initialization code to execute.
!>
!>     These strings store the labels for the parameters defined
!>     in the include file and retrieved by ZZDDHINI.
!>
!>     The file table consists of a set of arrays which serve as
!>     'columns' of the table.  The sets of elements having the same
!>     index in the arrays form the 'rows' of the table.  Each column
!>     contains a particular type of information; each row contains
!>     all of the information pertaining to a particular file.
!>
!>     All column names in the file table begin with 'FT'.  The columns
!>     are:
!>
!>        ABS      Absolute value of HAN
!>        AMH      File access method
!>        ARC      File architecture
!>        BFF      Binary file format
!>        HAN      Handle
!>        NAM      Filename
!>        RTM      RTRIM (right trimmed value for NAM)
!>        MNM      Unique DP number (the Magic NuMber ;)
!>
!>     New 'rows' are added to the end of the list; the list is repacked
!>     whenever a file is removed from the list.
!>
!>     NFT is the number of files currently loaded; this may not be
!>     greater than FTSIZE.  FINDEX refers to a file of interest within
!>     the table.  Since handles are always assigned in an increasing
!>     fashion, FTABS is guaranteed to be a sorted list.  We will use
!>     this fact to improve handle lookups in the file table.
!>
!>     NEXT stores the next handle to be used for file access.  This
!>     could be either for read or write based operations. NEXT is
!>     incremented just before entries in the file table are made.
!>     It begins as zero valued.
!>
!>     The unit table consists of a set of arrays which serve as
!>     'columns' of the table.  The sets of elements having the same
!>     index in the arrays form the 'rows' of the table.  Each column
!>     contains a particular type of information; each row contains
!>     all of the information pertaining to a particular logical unit.
!>
!>     All column names in the unit table begin with 'UT'.  The columns
!>     are:
!>
!>        CST      Cost to remove the file from the unit table
!>        HAN      Handle
!>        LCK      Is this logical unit locked to this handle?
!>        LUN      Logical unit
!>
!>     New 'rows' are added to the end of the list; the list is repacked
!>     whenever a logical unit is no longer needed.
!>
!>     NUT is the number of units currently stored in the table; this
!>     may not exceed UTSIZE.  UINDEX refers to a unit of interest
!>     within the table.
!>
!>     The following stores the native binary file format, a list of
!>     codes for supported binary formats, and the number of entries
!>     in SUPBFF.
!>
!>     Request counter used to determine cost.
submodule (pepper_instance_module) pepper_files

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: wdsize = 32  !! length of a word of text

contains

      !> Return platform ID and various attributes of the intended
      !> environment
      !>
      !>## Detailed Input
      !>
      !>     KEY         is a string value that indicates which platform
      !>                 specific information is desired.  Acceptable inputs
      !>                 are:
      !>
      !>                    'SYSTEM'      -  System Identification String
      !>                    'O/S'         -  Operating System or Environment
      !>                    'COMPILER'    -  NAIF Supported Compiler
      !>                    'FILE_FORMAT' -  Native Binary File Format
      !>                    'TEXT_FORMAT' -  Native Text File Format
      !>                    'READS_BFF'   -  List of supported binary file
      !>                                     formats.
      !>
      !>                 Note: The comparison is case-insensitive, and the
      !>                       supplied value must fit into a string buffer
      !>                       KYSIZE characters in length.
      !>
      !>## Detailed Output
      !>
      !>     VALUE       is the string that holds the information requested
      !>                 by the input string KEY. VALUE must be able to
      !>                 contain the maximum number of characters returned
      !>                 by this routine, WDSIZE, or truncation will occur.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If the KEY is invalid, then VALUE is set to the value
      !>        stored in the character string parameter DEFRPY defined
      !>        below.
      !>
      !>     2) If VALUE is not large enough to contain the requested
      !>        KEY's value, then truncation will occur.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine serves to identify the platform and compiler
      !>     used in creating SPICELIB.  It is provided so that routines
      !>     and programs can make run-time decisions based upon the
      !>     ambient Fortran environment.
      !>
      !>     Operating Systems:
      !>
      !>        This routine is now aware of the operating systems for which
      !>        the code is intended for compilation.  In some cases this may
      !>        be more than one operating system, particularly in the case
      !>        of the PC.
      !>
      !>     Binary File Format:
      !>
      !>        This routine now adds the capability to return at run time
      !>        the binary file architecture that is native to the system.
      !>
      !>     Text File Format:
      !>
      !>        This routine now has the capability to return at run time
      !>        the mechanism (or line terminator) used to delimit lines
      !>        in text files.  In most cases it will return common labels
      !>        for the special characters FORTRAN considers line break
      !>        indicators.
      !>
      !>     Binary File Formats Read:
      !>
      !>        This returns a space delimited list of all the binary file
      !>        formats this environment can read for DAF/DAS based files.
      !>
      !>## Examples
      !>
      !>     This routine could be used so that a single routine
      !>     could be written that translates the meaning of IOSTAT values
      !>     that depend upon the machine and compiler.  At run time
      !>     the routine could look up the appropriate message to associate
      !>     with an IOSTAT value.
      !>
      !>## Restrictions
      !>
      !>     1) VALUE must be large enough to contain the requested
      !>        information, otherwise truncation will occur.
      !>
      !>     2) The string passed in via the KEY input must be capable
      !>        of being properly copied into the KEYCPY buffer internal
      !>        to this routine.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>     F.S. Turner     (JPL)
      !>     E.D. Wright     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for:
      !>
      !>           MAC-OSX-M1-64BIT-CLANG_C
      !>
      !>        environment.
      !>
      !>-    SPICELIB Version 3.0.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for:
      !>
      !>           PC-CYGWIN-64BIT-GCC_C
      !>           PC-CYGWIN-64BIT-GFORTRAN
      !>           PC-CYGWIN-GFORTRAN
      !>           PC-LINUX-64BIT-IFORT
      !>           SUN-SOLARIS-64BIT-INTEL
      !>
      !>        environments.
      !>
      !>-    SPICELIB Version 2.9.0, 16-MAR-2010 (EDW)
      !>
      !>        Updated for:
      !>
      !>           MAC-OSX-64BIT-INTEL_C
      !>           PC-64BIT-MS_C
      !>           SUN-SOLARIS-64BIT-NATIVE_C
      !>           MAC-OSX-64BIT-GFORTRAN
      !>           MAC-OSX-64BIT-IFORT
      !>           PC-LINUX-64BIT-GFORTRAN
      !>           PC-WINDOWS-64BIT-IFORT
      !>           SUN-SOLARIS-INTEL-64BIT-CC_C
      !>           SUN-SOLARIS-INTEL-CC_C
      !>           SUN-SOLARIS-INTEL
      !>
      !>        environments.
      !>
      !>-    SPICELIB Version 2.8.0, 12-JAN-2009 (EDW)
      !>
      !>        Added MAC-OSX-GFORTRAN PC-LINUX-GFORTRAN environments.
      !>
      !>-    SPICELIB Version 2.7.0, 19-FEB-2008 (BVS)
      !>
      !>        Added PC-LINUX-IFORT environment.
      !>
      !>-    SPICELIB Version 2.6.0, 15-NOV-2006 (NJB)
      !>
      !>        Added PC-WINDOWS-IFORT, MAC-OSX-IFORT, and MAC-OSX-INTEL_C
      !>        environments.
      !>
      !>-    SPICELIB Version 2.5.0, 21-FEB-2006 (NJB)
      !>
      !>        Added PC-LINUX-64BIT-GCC_C environment.
      !>
      !>        Corrected error in in-line comments:  changed keyword
      !>        from FILE_ARCH to FILE_FORMAT.
      !>
      !>-    SPICELIB Version 2.4.0, 14-MAR-2005 (BVS)
      !>
      !>        Added SUN-SOLARIS-64BIT-GCC_C environment.
      !>
      !>-    SPICELIB Version 2.3.0, 31-DEC-2004 (BVS)
      !>
      !>        Added PC CYGWIN environments. Changed OS for PC-LAHEY,
      !>        PC-DIGITAL, and PC-MS_C to 'MICROSOFT WINDOWS'.
      !>
      !>-    SPICELIB Version 2.2.0, 07-JUL-2002 (EDW)
      !>
      !>        Added Mac OS X Unix environment.
      !>
      !>-    SPICELIB Version 2.1.0, 06-FEB-2002 (FST)
      !>
      !>        Updated the 'TEXT_FORMAT' key value for the PC-LINUX_C
      !>        environment.  Previous versions incorrectly indicated
      !>        'CR-LF' as line terminators.
      !>
      !>-    SPICELIB Version 2.0.0, 05-JUN-2001 (FST)
      !>
      !>        Added TEXT_FORMAT and READS_BFF key/value pairs.
      !>        Modified the header slightly to improve word choice;
      !>        specifically binary file format replaces file
      !>        architecture.
      !>
      !>        Updated the compiler entry for the PC-LINUX
      !>        environment to refer to g77 as opposed to f2c.
      !>
      !>        Updated the compiler entry for the MACPPC environment.
      !>        This environment is now officially tied to Absoft
      !>        Fortran.
      !>
      !>        Updated the compiler entry for the PC-LAHEY environment.
      !>        The compiler for this environment is LF95, the latest
      !>        offering from Lahey.
      !>
      !>-    SPICELIB Version 1.0.0, 22-FEB-1999 (FST)
      module subroutine zzplatfm ( ctx, key, value )
      use, intrinsic :: iso_fortran_env, only: compiler_version
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: key  !! String indicating what information to return.
      character(*), intent(out) :: value  !! String containing the requested information.
      integer(ip), parameter :: idxsys = 1
      integer(ip), parameter :: idxos = idxsys + 1
      integer(ip), parameter :: idxcmp = idxos  + 1
      integer(ip), parameter :: idxfmt = idxcmp + 1
      integer(ip), parameter :: idxtfm = idxfmt + 1
      integer(ip), parameter :: idxrbf = idxtfm + 1
      integer(ip), parameter :: kysize = 64
      integer(ip), parameter :: nattr = 6
      character(wdsize), parameter :: defrpy = '<UNAVAILABLE>'
      character(kysize) :: keycpy
      integer(ip) :: index
      if ( .not. allocated ( ctx%zzplatfm ) ) allocate ( ctx%zzplatfm )
      associate ( first => ctx%zzplatfm%first, &
                  attcpy => ctx%zzplatfm%attcpy, &
                  keyval => ctx%zzplatfm%keyval )
      if ( first ) then
!
!        Store the keys in the KEYVAL array.
!
         keyval (idxsys) = 'SYSTEM'
         keyval (idxos)  = 'O/S'
         keyval (idxcmp) = 'COMPILER'
         keyval (idxfmt) = 'FILE_FORMAT'
         keyval (idxtfm) = 'TEXT_FORMAT'
         keyval (idxrbf) = 'READS_BFF'

!
!        Set the default reply to be the zero'th component of ATTCPY.
!        This obviates IF-THEN-ELSE branching all together.
!
         attcpy (0) = defrpy

!
!        Platform/Environment specific assignments follow. PEPPER: the
!        system, the compiler and the text format are found at run time,
!        in standard Fortran (SPICELIB had them compiled in for each
!        platform): see PLATFORM_SYSTEM.
!
         call platform_system ( attcpy(idxsys), attcpy(idxos), attcpy(idxtfm) )
         attcpy (idxcmp) = compiler_version()
         attcpy (idxfmt) = 'LTL-IEEE'

         attcpy (idxrbf) = 'BIG-IEEE LTL-IEEE'

!
!        Don't execute these assignments again.
!
         first = .false.

      end if

!
!     Determine which KEY was passed in; do this by converting KEY
!     to the known member of the equivalence class of possible
!     values.
!
      call ucase ( key,    keycpy )
      keycpy = adjustl(keycpy)

!
!     Find out which key we were given.  In the event that one of the
!     KEYVALs (or some equivalent string) was not passed in, ISRCHC
!     returns a value of zero.
!
      index = findloc(keyval(1:nattr), keycpy, dim=1)
      value = attcpy ( index )

      return
      end associate
      end subroutine zzplatfm

      !> The system, the operating system and the line terminator of text
      !> files of the platform the program runs on (PEPPER; for
      !> [[zzplatfm]]).
      !>
      !> Standard Fortran has no inquiry for the operating system: Windows
      !> sets the environment variable OS to Windows_NT, and Cygwin, macOS and
      !> Linux are recognized by files that only they have. The Fortran
      !> run-time libraries end the lines of text files with CR-LF on
      !> Windows and with LF elsewhere (including Cygwin).
      subroutine platform_system ( system, os, text_format )
      implicit none
      character(*), intent(out) :: system  !! 'PC' or 'MAC'
      character(*), intent(out) :: os  !! the operating system ('<UNAVAILABLE>' if not recognized)
      character(*), intent(out) :: text_format  !! the line terminator of text files: 'LF' or 'CR-LF'
      character(16) :: osvar
      integer :: status
      system      = 'PC'
      os          = '<UNAVAILABLE>'
      text_format = 'LF'
      call get_environment_variable ( 'OS', osvar, status=status )
      if ( exists ( '/usr/bin/cygwin1.dll' ) ) then
         os = 'MICROSOFT WINDOWS/CYGWIN'
      else if ( status == 0 .and. osvar == 'Windows_NT' ) then
         os          = 'MICROSOFT WINDOWS'
         text_format = 'CR-LF'
      else if ( exists ( '/System/Library/CoreServices/SystemVersion.plist' ) ) then
         system = 'MAC'
         os     = 'MAC OS-X'
      else if ( exists ( '/proc/version' ) ) then
         os = 'LINUX'
      end if
      contains
         !> Whether a file exists.
         logical function exists ( file )
         character(*), intent(in) :: file  !! the file name
         integer :: ios
         inquire ( file=file, exist=exists, iostat=ios )
         if ( ios /= 0 ) exists = .false.
         end function exists
      end subroutine platform_system

      !> Validate the runtime environment against values assumed by the
      !> current toolkit source package.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     OK         is a logical when set to .TRUE. indicates that the
      !>                runtime environment passes any checks implemented
      !>                by this routine against the configured code.
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
      !>     1) This routine signals SPICE(BUG) if it determines the runtime
      !>        environment is incompatible with the configured binary file
      !>        format.
      !>
      !>## Particulars
      !>
      !>     This routine encapsulates a series of checks to diagnose the
      !>     runtime environment against assumptions configured in the
      !>     source code.  Configuration errors are reported via the error
      !>     SPICE(BUG).
      !>
      !>## Examples
      !>
      !>     See ZZDDHOPN for sample usage.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 07-AUG-2002 (FST)
      module subroutine zzpltchk ( ctx, ok )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(out) :: ok  !! Logical indicating the runtime environment is ok.


      character(wdsize) :: strbff
      character(wdsize) :: value
      character(wdsize) :: rtebff

      integer(ip) :: bff

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) return

!
!     Verify that the runtime environment's binary file format agrees
!     with the value listed in ZZPLATFM.
!
      call zzplatfm ( ctx, 'FILE_FORMAT', strbff )

!
!     Determine what the runtime environment binary file format appears
!     to be.
!
      call zzgetbff ( ctx, bff )
      call zzddhgsd ( ctx, 'BFF', bff, rtebff )

!
!     Check results, signal SPICE(BUG) if a discrepancy appears.
!
      if ( strbff /= rtebff ) then

         call ctx%setmsg   ( 'This version of SPICELIB was originally ' &
         //              'packaged by NAIF for # hardware using ' &
         //              '# with the # compiler.  This ' &
         //              'environment has a binary ' &
         //              'file format of #; however the software is ' &
         //              'running on an environment that has a binary ' &
         //              'file format of #.  This is a severe problem ' &
         //              'and may be because the software package was ' &
         //              'intended for use on a different computer ' &
         //              'system.  It also may be the result of an ' &
         //              'improper port; please contact NAIF.'         )

         call zzplatfm ( ctx, 'SYSTEM',   value                             )
         call ctx%errch    ( '#',        value                             )

         call zzplatfm ( ctx, 'O/S',      value                             )
         call ctx%errch    ( '#',        value                             )

         call zzplatfm ( ctx, 'COMPILER', value                             )
         call ctx%errch    ( '#',        value                             )

         call ctx%errch    ( '#',        strbff                            )

         if ( rtebff == ' ' ) then
            call ctx%errch ( '#', 'UNKNOWN' )
         else
            call ctx%errch ( '#',  rtebff   )
         end if

         call ctx%sigerr   ( 'SPICE(BUG)'                                  )
         return

      end if

      return

      end subroutine zzpltchk

      !> Retrieve the components of the FTP validation string.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     TSTCOM     is a string composed of clusters of characters that
      !>                are susceptible to FTP ASCII mode transfer corruption,
      !>                separated by the DELIM character.  For example:
      !>
      !>                    :<CLUSTR(1)>:<CLUSTR(2):...
      !>                                            ...<CLUSTR(N)>:
      !>
      !>                where <CLUSTR(I)> is one cluster of characters that
      !>                is subject to improper FTP corruption. The string
      !>                that is to receive this value should be SIZSTR
      !>                characters in length.
      !>
      !>     LEND,      are the two sequences of printing characters that
      !>     REND       bracket TSTCOM in the binary file.  Their purpose is
      !>                to permit proper detection of TSTCOM in the event
      !>                of compression or expansion, due to improper FTP
      !>                transfer.  The variables which are to receive these
      !>                values should be SIZEND characters in length.
      !>
      !>     DELIM      is the printing character delimiter that separates the
      !>                test character clusters from one another, as well as
      !>                LEND and REND.  Since it is often the case that pairs
      !>                or triples of non-printing characters will trigger
      !>                FTP corruption, this delimiter blocks any unintended
      !>                interaction.
      !>
      !>## Parameters
      !>
      !>     1) See include file zzftprms.inc
      !>
      !>     2) Since inserting non-printing characters into strings is a
      !>        somewhat arduous task requiring extensive use of the intrinsic
      !>        CHAR, integer parameters that map to the needed ASCII codes are
      !>        defined with variable names INT###, where ### is replaced with
      !>        the three digit ASCII integer code.  For each such integer
      !>        code, there is a corresponding character parameter whose name
      !>        is of the form ASC###.  For example:
      !>
      !>           INT010 = 10  -> ASC010 = <10> or <LF>
      !>           INT206 = 206 -> ASC206 = <206>
      !>
      !>        where <#> refers to CHAR(#) or CHAR(ICHAR('#')) in the case of
      !>        LF(line feed).
      !>
      !>        These naming conventions should be preserved when the FTP
      !>        validation string is updated.
      !>
      !>## Files
      !>
      !>     While this routine is designed to aid in the detection of
      !>     improper FTP transfers, it simply returns the candidate
      !>     string for validation and does not interact with any
      !>     files directly.
      !>
      !>## Exceptions
      !>
      !>     Error Free.
      !>
      !>## Particulars
      !>
      !>     To minimize code alterations in the event of a string update,
      !>     the calling routine that declares the variables to receive
      !>     the strings stored here should include zzftprms.inc and utilize
      !>     the size parameters defined there as recommended in the Detailed
      !>     I/O sections above.
      !>
      !>     This private SPICELIB routine is designed to centralize the
      !>     definition of the FTP validation string present in binary
      !>     SPICE kernels.  If in the process of FTP'ing a binary
      !>     file from one platform to another, the user neglects to
      !>     invoke the IMAGE (BINARY) transfer mode, an ASCII mode
      !>     transfer may occur.  As this at the very least may substitute
      !>     one set of line terminators for another, corruption of the
      !>     binary file is likely.  By placing a string that encapsulates
      !>     a representative set of these character sequences that are
      !>     susceptible to corruption in the file record, it is possible
      !>     to trap and report any problems to the user when corrupted
      !>     kernels are loaded at run time.
      !>
      !>     To that end, analysis of evidence obtained by moving test binary
      !>     files from one platform to another indicates the following
      !>     clusters of ASCII codes are likely candidates for corruption:
      !>
      !>        Test Clusters:
      !>
      !>        <13>      - Text line terminator on Macintosh-based platforms.
      !>        <10>      - Text line terminator on UNIX-based platforms.
      !>        <13><10>  - Text line terminator on Microsoft platforms.
      !>        <13><0>   - Sequence of characters that maps into <13> on some
      !>                    UNIX-based systems. (HP, SGI, NEXT)
      !>        <129>     - Macintosh based systems permute ASCII values whose
      !>                    parity bit is set.  Codes in excess of ASCII
      !>                    127 are altered.
      !>        <16><206> - Some ancient FTP servers on PC's convert this
      !>                    sequence of ASCII characters to <16><16><206>.
      !>
      !>     The examples above show that substitution of one set of line
      !>     terminators for another can result in expansion or compression of
      !>     certain sequences of bytes.  If the clusters were juxtaposed, new
      !>     sequences of adjacent bytes, themselves subject to transformation,
      !>     might be formed.  So the FTP test string present in the binary
      !>     file should have some mechanism for preventing interaction between
      !>     the clusters.  The test string should also be constructed so that
      !>     it can be easily located in the event compression or expansion,
      !>     either internally or elsewhere in the file record, shifts it away
      !>     from its default location.
      !>
      !>     So by separating these clusters with a printable delimiter, then
      !>     bracketing the entire test string with start and stop identifiers,
      !>     we have a reasonable mechanism for locating and analyzing any
      !>     potential FTP corruption. Then the sequence of characters to be
      !>     inserted into the file will appear as:
      !>
      !>        FTPSTR:<13>:<10>:<13><10>:<13><0>:<129>:<16><206>:ENDFTP
      !>
      !>     where 'FTPSTR' and 'ENDFTP' are the bracketing substrings and
      !>     ':' is the delimiting character.
      !>
      !>     By no means do we claim that these are the complete set of
      !>     clusters that are corruptible through an improper FTP transfer.
      !>     An update procedure is provided in the Revisions section just
      !>     after the routine header.  Following this procedure will require
      !>     the least amount of effort to prevent older files from falsely
      !>     indicating corruption under new Toolkits, as well as newer files
      !>     failing on old Toolkits.
      !>
      !>## Examples
      !>
      !>     This routine just fetches the components of the FTP validation
      !>     string.
      !>
      !>## Restrictions
      !>
      !>     1) TSTCOM, LEND, REND, and DELIM must be large enough to hold
      !>        the entire values returned by this routine, otherwise
      !>        truncation will occur.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 21-MAR-1999 (FST)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.0.0, 21-MAR-1999 (FST)
      !>
      !>        FTP validation string update procedure:
      !>
      !>           (1) Leave 'FTPSTR', 'ENDFTP', and ':' alone, as
      !>               their alteration will require special
      !>               consideration for older files.
      !>
      !>           (2) Leave the existing test clusters in the
      !>               existing order, and place any new clusters
      !>               between the last ':' and the E in 'ENDFTP'.
      !>               Make certain these are ':' delimited as well.
      !>
      !>           (3) Modify the contents of zzftprms.inc to
      !>               indicate the new sizes of the various string
      !>               components. Routines that include this must
      !>               then be recompiled.
      module subroutine zzftpstr ( ctx, tstcom, lend, rend, delim )
      use pepper_kinds, only: ip
      use pepper_strings, only: suffix
      use pepper_inc_zzftprms
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: tstcom  !! The FTP test component string.
      character(*), intent(out) :: lend  !! String that brackets TSTCOM on the left in a file.
      character(*), intent(out) :: rend  !! String that brackets TSTCOM on the right in a file.
      character(*), intent(out) :: delim  !! Delimiter that separates the pieces of TSTCOM.
!
!     Local Parameters
!
!     Maximum size of an individual test cluster component
!     including the ':'.
!
      integer(ip), parameter :: siztsq = 5
!
!     Integer codes of characters appearing in test clusters.
!
      integer(ip), parameter :: int000 = 0
      integer(ip), parameter :: int010 = 10
      integer(ip), parameter :: int013 = 13
      integer(ip), parameter :: int016 = 16
      integer(ip), parameter :: int129 = 129
      integer(ip), parameter :: int206 = 206
!
!     Local Variables
!
      character(siztsq) :: testsq ( numtst )
!
!     Non-printing character values.
!
      character(1) :: asc000
      character(1) :: asc010
      character(1) :: asc013
      character(1) :: asc016
      character(1) :: asc129
      character(1) :: asc206
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzftpstr ) ) allocate ( ctx%zzftpstr )
      associate ( first => ctx%zzftpstr%first, &
                  locdlm => ctx%zzftpstr%locdlm, &
                  loclnd => ctx%zzftpstr%loclnd, &
                  locrnd => ctx%zzftpstr%locrnd, &
                  locstr => ctx%zzftpstr%locstr )
!
!     On the first invocation initialize the string values.
!
      if ( first ) then
!
!        Convert the integer parameters to their non-printing ASCII
!        equivalents.
!
         asc000 = char( int000 )
         asc010 = char( int010 )
         asc013 = char( int013 )
         asc016 = char( int016 )
         asc129 = char( int129 )
         asc206 = char( int206 )

!
!        Now build the individual components of the test clusters.
!        Make certain the first component begins and ends with a ':',
!        and that the remaining pieces end in ':'. If you intend to
!        add some clusters, then append them to the end of the
!        sequence so as not to break the existing detection code.
!

!
!        Cluster #1 : <CR> - <13> - Macintosh Line Terminator
!
         testsq(1) = locdlm // asc013 // locdlm

!
!        Cluster #2 : <LF> - <10> - Unix Line Terminator
!
         testsq(2) = asc010 // locdlm

!
!        Cluster #3 : <CR><LF> - <10><13> - Microsoft Line Terminator
!
         testsq(3) = asc013 // asc010 // locdlm

!
!        Cluster #4 : <13><0>
!
         testsq(4) = asc013 // asc000 // locdlm

!
!        Cluster #5 : <129> - Macintosh Permutation of Parity Codes
!
         testsq(5) = asc129 // locdlm

!
!        Cluster #6 : <16><206>
!
         testsq(6) = asc016 // asc206 // locdlm

!
!        Sample cluster addition code follows
!
!        Cluster #7 : <xxx> - Description
!
!        TESTSQ(7) = ASCxxx // ... // LOCDLM
!

!
!        Now build the local copy of TSTCOM, LOCSTR. First clear the
!        uninitialized contents.
!
         locstr = ' '

         do i = 1, numtst

!
!           Append TESTSQ(I) to LOCSTR to properly construct the
!           test component of the FTP validation string.
!
            call suffix ( testsq(i), 0, locstr )

         end do

!
!        Prevent execution of this initialization code after first pass.
!
         first = .false.

      end if
!
!     Copy the local copies of the FTP string components to the
!     arguments passed in from the caller.
!
      tstcom = locstr
      lend   = loclnd
      rend   = locrnd
      delim  = locdlm

      return
      end associate
      end subroutine zzftpstr

      !> Check a character string that may contain the FTP validation
      !> string for FTP based errors.
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a string read in directly from a binary file.  This
      !>                string should, but does not have to, contain the FTP
      !>                validation string.  Typically this block of characters
      !>                is read in from the file record of the binary kernel.
      !>                If 'FTPSTR' or 'ENDFTP' occur anywhere in STRING, then
      !>                validation will be attempted.  Multiple occurrences of
      !>                these two special strings in STRING is also an issue.
      !>                See Restrictions for details.
      !>
      !>## Detailed Output
      !>
      !>     FTPERR     is a logical that indicates whether or not an FTP
      !>                error has occurred.  If an error is detected FTPERR
      !>                is set to TRUE, otherwise FTPERR is FALSE.  In the
      !>                event that STRING does not contain either of the FTP
      !>                bracketing strings, then the test will not be
      !>                performed.  Thus, FTPERR is set to FALSE.
      !>
      !>## Parameters
      !>
      !>     See include file zzftprms.inc.
      !>
      !>## Exceptions
      !>
      !>     1) In the event that both the left and right end markers of the
      !>        FTP validation string are not present in STRING, the routine
      !>        assumes that this information is from a pre-FTP test file.  As
      !>        such, the file can not be validated, so FTPERR remains FALSE.
      !>
      !>     2) If the FTP string brackets 'FTPSTR' and 'ENDFTP' are present in
      !>        multiple places in the text block, then this routine assumes
      !>        the last occurrence of the substring these strings bracket is
      !>        the FTP test sequence that requires validation.  So if this
      !>        routine encounters a block of text:
      !>
      !>        ...FTPSTR:<TESTSQ>:ENDFTP...FTPSTR[THISISNOTATEST]ENDFTP...
      !>
      !>        where <TESTSQ> is the actual test sequence, then it will
      !>        incorrectly compare [THISISNOTATEST] to the test component
      !>        returned from ZZFTPSTR.
      !>
      !>## Files
      !>
      !>     Although this routine validates information from a binary file,
      !>     it does not interact with the file directly, and relies upon
      !>     the caller to pass the appropriate string block.
      !>
      !>## Particulars
      !>
      !>     The purpose of this routine is to examine for FTP errors a
      !>     string brought in from a binary kernel.  This text may or may
      !>     not contain the FTP validation string defined in ZZFTPSTR.
      !>     However, if it contains at least one of the two bracketing
      !>     substrings ('FTPSTR' and 'ENDFTP'), then the routine assumes
      !>     that the text is subject to FTP validation. As a result of this,
      !>     the caller should avoid passing in user controlled chunks of
      !>     character data from the file.  If the user has decided to place
      !>     one of the FTP string bracket components in this portion of the
      !>     file, then ZZFTPCHK may be confused and incorrectly indicate an
      !>     error condition.
      !>
      !>## Examples
      !>
      !>     The following code fragment from DAFOPR reads in the DAF file
      !>     record and attempts to examine the contents for FTP errors.
      !>     (Note: this code fragment is from a 32 bit word length, 1
      !>     byte character environment.)
      !>
      !>     C
      !>     C     Check for FTP transfer errors to prevent the user from
      !>     C     inadvertently using a damaged kernel. First read the file
      !>     C     record into a string of 1000 characters.
      !>     C
      !>           READ ( UNIT = LUN, REC = 1, IOSTAT = IOSTAT ) FTPTST
      !>
      !>           IF ( IOSTAT .NE. 0 ) THEN
      !>
      !>              CLOSE       ( LUN                                     )
      !>              CALL SETMSG ( 'Error reading the file record from'   //
      !>          .                 ' the binary DAF file ''#''. IOSTAT'   //
      !>          .                 ' = #.'                                 )
      !>              CALL ERRCH  ( '#', FNAME                              )
      !>              CALL ERRINT ( '#', IOSTAT                             )
      !>              CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
      !>              CALL CHKOUT ( 'DAFOPR'                                )
      !>              RETURN
      !>
      !>           END IF
      !>
      !>     C
      !>     C     Since we are dealing with DAF files, only place the
      !>     C     last 500 characters of data from the file record into
      !>     C     ZZFTPCHK.  This ensures that the internal filename
      !>     C     and the ID word do not interfere with the FTP validation
      !>     C     process.
      !>     C
      !>           CALL ZZFTPCHK ( FTPTST(501:1000), FTPERR )
      !>
      !>           IF ( FTPERR ) THEN
      !>
      !>              CLOSE       ( LUN                                      )
      !>              CALL SETMSG ( 'FTP transfer error.  This binary DAF, '//
      !>          .                 '''#'', has most likely been corrupted '//
      !>          .                 'by an ASCII mode FTP transfer. '       //
      !>          .                 'Re-obtain the file using IMAGE or '    //
      !>          .                 'BINARY transfer mode.'                  )
      !>              CALL ERRCH  ( '#', FNAME                               )
      !>              CALL SIGERR ( 'SPICE(FTPXFERERROR)'                    )
      !>              CALL CHKOUT ( 'DAFOPR'                                 )
      !>              RETURN
      !>
      !>           END IF
      !>
      !>## Restrictions
      !>
      !>     1) STRING may contain multiple occurrences of the FTP bracketing
      !>        substrings ('FTPSTR' and 'ENDFTP'), but only if the last
      !>        occurrence of both brackets the actual data for validation.
      !>
      !>     2) This routine assumes the presence of either 'FTPSTR' or
      !>        'ENDFTP' in STRING indicates that validation is to be
      !>        attempted.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
      !>
      !>        Fixed typo in comment. Reordered header sections.
      !>
      !>-    SPICELIB Version 1.0.0, 22-MAR-1999 (FST)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.0.0, 22-MAR-1999 (FST)
      !>
      !>        This routine does not require modification if the FTP
      !>        validation string is updated according to the guidelines
      !>        laid out in ZZFTPSTR.  The reason for this is the
      !>        verification algorithm extracts the chunk of text between
      !>        'FTPSTR' and 'ENDFTP'.  It then checks to see whether or
      !>        not this chunk is a subset of the test component stored
      !>        in ZZFTPSTR. Two cases:
      !>
      !>           (1) It is.  Then this indicates that at the worst,
      !>               the chunk is from a valid file with an earlier
      !>               version of the FTP validation string.
      !>
      !>           (2) It is not.  While this is a fair indication that
      !>               the file may be corrupt, it's not a complete
      !>               treatment, since we may be examining a file
      !>               created with a newer version of the FTP validation
      !>               string.  So now check to see whether that test
      !>               component from ZZFTPSTR is a subset of the text
      !>               chunk from STRING.  If it is, then the file is
      !>               as valid as far as this version of the toolkit is
      !>               concerned.  Otherwise, the file is damaged.
      module subroutine zzftpchk ( ctx, string, ftperr )
      use pepper_kinds, only: ip
      use pepper_strings, only: pos, rtrim
      use pepper_inc_zzftprms
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! String that may contain the FTP validation string.
      logical, intent(out) :: ftperr  !! Logical indicating if FTP corruption occurred.
!
!     Local Variables
!
      character(sizdlm) :: delim
      character(sizexp) :: filstr
      integer(ip) :: fsmidx
      integer(ip) :: length
      integer(ip) :: msfidx
      logical :: isther
      if ( .not. allocated ( ctx%zzftpchk ) ) allocate ( ctx%zzftpchk )
      associate ( first => ctx%zzftpchk%first, &
                  lftbkt => ctx%zzftpchk%lftbkt, &
                  memstr => ctx%zzftpchk%memstr, &
                  rgtbkt => ctx%zzftpchk%rgtbkt )
!
!     On the first pass through, fetch a copy of the current FTP
!     validation string.
!
      if ( first ) then

         call zzftpstr ( ctx, memstr, lftbkt, rgtbkt, delim )

!
!        Don't fetch the string on subsequent calls to this routine.
!
         first = .false.

      end if

!
!     Extract the FTP validation string from the block of text that
!     was passed into the routine via the argument STRING. Note,
!     if the bracketed substring in the text block STRING is larger
!     than the FILSTR string size, ZZRBRKST will truncate the data
!     that does not fit.  This loss of data is not an issue, since in
!     this case we may only validate the part of the substring near
!     the head, for which we have enough room in FILSTR.
!
      call zzrbrkst ( ctx, string, &
                      lftbkt ( 1:rtrim(lftbkt) ), &
                      rgtbkt ( 1:rtrim(rgtbkt) ), &
                      filstr, &
                      length, &
                      isther                      )

!
!     Now check ISTHER to see if either LFTBKT or RGTBKT was present
!     in the block of text from the file. If both are absent, then
!     we must assume that this text is from a pre-FTP validation file,
!     and as such do not return any indication of an error.
!
      if ( .not. isther ) then

         ftperr = .false.

!
!     If one of the brackets is present, then we may proceed with
!     validation.  First check to see if the length is 0.  If it is,
!     then at least one of the brackets was present, but ZZRBRKST was
!     unable to extract a properly bracketed substring.  This is an
!     error.
!
      else if ( length <= 0 ) then

         ftperr = .true.

!
!     Now we make it to this ELSE statement only if ISTHER is TRUE, and
!     LENGTH is a positive number.  Compare the contents of FILSTR
!     and MEMSTR.
!
      else

!
!        First determine if the data from the file is a subset of
!        what is stored in memory.
!
         fsmidx = pos (  memstr,  filstr( 1:rtrim(filstr) ),  1  )

!
!        In the event that FSMIDX is non-zero, we know that FILSTR
!        is a substring of MEMSTR, and thus we have validated all the
!        test clusters from the file.
!
         if ( fsmidx /= 0 ) then

            ftperr = .false.

!
!        If FSMIDX is 0, then we do not yet know whether or not the
!        file is valid.  Now it may be the case that this file contains
!        a newer FTP validation string than this version of the
!        toolkit is aware.  Check to see whether what's in memory
!        is a substring of what's in FILSTR.
!
         else

            msfidx = pos (  filstr,  memstr( 1:rtrim(memstr) ),  1  )

!
!           If this comes back as zero, then we definitely have
!           an FTP error. Set FTPERR appropriately.
!
            ftperr = ( msfidx == 0 )

         end if

      end if

      return
      end associate
      end subroutine zzftpchk

      !> Extract from a string the last instance of a substring bracketed
      !> by specified left and right strings .
      !>
      !>## Detailed Input
      !>
      !>     STRING     is a string to be searched for a substring bracketed
      !>                by the strings LFTEND and RGTEND (see below).
      !>
      !>     LFTEND,    are respectively the left and right bracketing strings.
      !>     RGTEND     Trailing and leading white space is significant. LFTEND
      !>                may equal RGTEND.  See the Exceptions section for a
      !>                discussion of the case in which either of these strings
      !>                is absent.
      !>
      !>## Detailed Output
      !>
      !>     SUBSTR     is the substring of interest.  It consists of the
      !>                substring between the last instances of LFTEND
      !>                and RGTEND in STRING. Note: The argument passed into
      !>                the routine should be large enough to hold the entire
      !>                substring, or else truncation will occur. SUBSTR is
      !>                padded with trailing blanks.
      !>
      !>     LENGTH     is the number of characters placed into SUBSTR. This
      !>                value permits any significant trailing whitespace to be
      !>                dealt with appropriately. In the event that no
      !>                substring is assigned to SUBSTR, LENGTH will be 0.
      !>
      !>     BKPRES     is a logical that indicates whether or not at least
      !>                one of LFTEND or RGTEND is present in STRING.
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
      !>     1) If LFTEND or RGTEND are not present in STRING, then the routine
      !>        does not modify the contents of SUBSTR, LENGTH is returned as
      !>        0, and BKPRES is TRUE only if LFTEND or RGTEND is present.
      !>
      !>     2) If LFTEND and RGTEND are adjacent, then SUBSTR is not modified,
      !>        LENGTH is returned as 0, and BKPRES is TRUE.
      !>
      !>## Particulars
      !>
      !>     The purpose of this routine is to extract the last instance of
      !>     a substring bracketed by two specified strings.  The searching
      !>     is case sensitive, and all white space is significant.  The
      !>     characters between LFTEND and RGTEND are placed into SUBSTR,
      !>     and LENGTH is set to the number of characters copied into SUBSTR.
      !>     The assignment is not substring assignment, so the resultant
      !>     SUBSTR will be blank padded.  The logical BKPRES is a flag that
      !>     indicates whether or not either of the two brackets was found.
      !>     This is diagnostic information of some limited use in the event
      !>     that SUBSTR was not assigned a value.
      !>
      !>## Examples
      !>
      !>     The following table demonstrates the behavior of this routine:
      !>     ( If a row in the table has no entry for SUBSTR, then the
      !>       contents of SUBSTR are not modified by calling the routine
      !>       with these inputs. )
      !>
      !>     STRING               LFTEND   RGTEND   SUBSTR             LENGTH
      !>     ===================  =======  =======  =================  ======
      !>     'abc def ghi jkl'    'a'      'l'      'bc def ghi jk'      13
      !>     'abc def ghi jkl'    'abc'    'ghi'    ' def '              5
      !>     'abc def ghi jkl'    'abc'    '123'                         0
      !>     'abc def ghi jkl'    '123'    'def'                         0
      !>     'abc def ghi jkl'    'jkl'    'zzz'                         0
      !>     'abc def abc jkl'    'abc'    'abc'    ' def '              5
      !>     'ab cd ab ef ab '    'ab'     'ab'     ' ef '               4
      !>     'ab cd ab ef ab '    'ef'     'cd'                          0
      !>     'abc def-fed abc'    'def'    '-fed'                        0
      !>     'aaaaaaaaaaaaaaa'    'aa'     'aaaa'                        0
      !>     'aaaabbbaabababa'    'ba'     'a'      'b'                  1
      !>     'aaaabbbaababada'    'ba'     'a'      'd'                  1
      !>     'abcd efgh ijkl '    ' '      'l'      'ijk'                3
      !>     'abcd efgh ijkl '    '    '   'l'                           0
      !>     'ab  ef   ijklm '    '  '     'm'      'ijkl'               4
      !>     'ab   ef  ijklm '    '   '    'm'      'ef  ijkl'           8
      !>
      !>## Restrictions
      !>
      !>     1) The size of SUBSTR must be large enough to contain any
      !>        possible substring bracketed by LFTEND or RGTEND, otherwise
      !>        truncation will occur at assignment.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 22-MAR-1999 (FST)
      module subroutine zzrbrkst ( ctx, string, lftend, rgtend, substr, length, bkpres )
      use pepper_kinds, only: ip
      use pepper_strings, only: posr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! A string from which to extract SUBSTR.
      character(*), intent(in) :: lftend  !! A string that brackets SUBSTR on the left.
      character(*), intent(in) :: rgtend  !! A string that brackets SUBSTR on the right.
      character(*), intent(out) :: substr  !! The extracted substring.
      integer(ip), intent(out) :: length  !! The length of the extracted substring.
      logical, intent(out) :: bkpres  !! Logical indicating if either bracket is present.

      integer(ip) :: bsize
      integer(ip) :: lindex
      integer(ip) :: lsize
      integer(ip) :: rindex
      integer(ip) :: rsize

!
!     Compute the sizes of the bracketing substrings and the text
!     block.
!
      lsize = len ( lftend )
      rsize = len ( rgtend )
      bsize = len ( string )

!
!     Search from the right for RGTEND.
!
      rindex = posr ( string, rgtend, bsize )

!
!     Now continue the search from RINDEX to the right, this time
!     looking for LFTEND. If RINDEX comes back as 0, then the right
!     bracketing substring is not present, so search the entire string
!     for LFTEND. Otherwise, search from where the right bracket
!     search left off.
!
      if ( rindex == 0 ) then

         lindex = posr ( string, lftend, bsize )

      else

         lindex = posr ( string, lftend, rindex - lsize )

      end if

!
!     Interpret the results.  If RINDEX and LINDEX are both non-zero,
!     then return the substring they bracket, otherwise handle the
!     failed case.
!
      if ( ( rindex /= 0 ) .and. ( lindex /= 0 ) ) then

!
!        Check to see whether or not the brackets are adjacent, and
!        thus have no characters between them.
!
         if ( ( lindex + lsize ) > ( rindex - 1 ) ) then

            bkpres = .true.
            length = 0

!
!        If they aren't adjacent, then compute the length and prepare
!        SUBSTR.
!
         else

            length = rindex - ( lindex + lsize )
            bkpres = .true.
            substr = string ( ( lindex  + lsize ) : ( rindex - 1 ) )

         end if

      else

!
!        Set BKPRES to TRUE only if LINDEX or RINDEX is non-zero,
!        indicating one was found by POSR. Set LENGTH to 0, since we
!        will not be changing SUBSTR.
!
         bkpres = ( ( lindex + rindex ) > 0 )
         length = 0

      end if

      return
      end subroutine zzrbrkst

      !> Initialize ZZDDHMAN data structures.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     NATBFF     is an integer code for the binary file format native
      !>                to this system as described in the include file
      !>                'zzddhman.inc'.  Possible values are the parameters:
      !>
      !>                   BIGI3E
      !>                   LTLI3E
      !>                   VAXGFL
      !>                   VAXDFL
      !>
      !>     SUPBFF     is an array of integer codes for the binary file
      !>                formats supports for reading.  At the very minimum
      !>                it includes NATBFF, but potentially NUMBFF entries.
      !>                See the include file 'zzddhman.inc'.
      !>
      !>     NUMSUP     is the number of entries in the SUPBFF list.
      !>
      !>     STRAMH     is a list of strings containing the labels for the
      !>                access method ID codes defined in the include file
      !>                'zzddhman.inc'.  These values are retrieved from
      !>                ZZDDHGSD.  See it for details.
      !>
      !>     STRARC     is a list of strings containing the labels for the
      !>                file architecture ID codes defined in the include file
      !>                'zzddhman.inc'.  These values are retrieved from
      !>                ZZDDHGSD.  See it for details.
      !>
      !>     STRBFF     is a list of strings containing the labels for the
      !>                binary file format ID codes defined in the include
      !>                file 'zzddhman.inc'.  These values are retrieved from
      !>                ZZDDHGSD.  See it for details.
      !>
      !>## Parameters
      !>
      !>     See the include file 'zzddhman.inc'.
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
      !>     This routine populates a variety data structures that ZZDDHMAN
      !>     requires to perform its functions.
      !>
      !>## Examples
      !>
      !>     See ZZDDHMAN for sample usage.
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
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typo in long error message. Reordered header
      !>        sections.
      !>
      !>-    SPICELIB Version 1.0.0, 13-AUGUST-2001 (FST)
      module subroutine zzddhini ( ctx, natbff, supbff, numsup, stramh, strarc, strbff )
      use pepper_kinds, only: ip
      use pepper_strings, only: nextwd_inplace, ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: natbff  !! Native binary file format.
      integer(ip), intent(out) :: supbff ( * )  !! List of supported binary file formats for
                                                !! reading.
      integer(ip), intent(out) :: numsup  !! Number of entries returned in SUPBFF.
      character(*), intent(out) :: stramh ( * )  !! List of labels for METHOD ID codes
      character(*), intent(out) :: strarc ( * )  !! List of labels for ARCH ID codes
      character(*), intent(out) :: strbff ( * )  !! List of labels for BFF ID codes

!
!     Local Variables.
!
      character(strlen) :: linstr
      character(strsiz) :: tmpstr

      integer(ip) :: i

      logical :: done

!
!     Standard SPICE error handling with discovery check in/out.
!
      if ( ctx%failed() ) return

!
!     Populate the STR### arrays.
!
      do i = 1, numamh
         call zzddhgsd ( ctx, 'METHOD', i, stramh(i) )
      end do

      do i = 1, numarc
         call zzddhgsd ( ctx, 'ARCH', i, strarc(i) )
      end do

      do i = 1, numbff
         call zzddhgsd ( ctx, 'BFF', i, strbff(i) )
      end do

!
!     Get the native binary file format.
!
      call zzplatfm ( ctx, 'FILE_FORMAT', tmpstr )
      call ucase_inplace ( tmpstr )

      natbff = findloc(strbff(1:numbff), tmpstr, dim=1)

      if ( natbff == 0 ) then

         call ctx%setmsg ( 'The binary file format, ''#'', is not ' &
         //            'supported by this version of the toolkit. ' &
         //            'This is a serious problem, contact NAIF.'   )
         call ctx%errch  ( '#', tmpstr                                  )
         call ctx%sigerr ( 'SPICE(BUG)'                                 )
         return

      end if

!
!     Now fetch the list of supported binary file formats.
!
      call zzplatfm ( ctx, 'READS_BFF', linstr )

!
!     Parse the wordlist that is sitting in LINSTR.
!
      i = 0
      done = .false.

      do while ( .not. done )

!
!        Increment the counter and pop the next word
!        off.
!
         i = i + 1
         call nextwd_inplace ( linstr, tmpstr )

!
!        See if we're done.
!
         done = ( i > numbff ) .or. ( tmpstr == ' ' )

!
!        If we're not done, then convert this string to the
!        appropriate integer code.
!
         if ( .not. done ) then

            supbff(i) = findloc(strbff(1:numbff), tmpstr, dim=1)

!
!           Check to see if the binary file format listed
!           is properly supported.
!
            if ( supbff(i) == 0 ) then

               call ctx%setmsg ( 'The binary file format, ''#'', is ' &
               //            'not supported by this version of ' &
               //            'the toolkit. This is a serious ' &
               //            'problem, contact NAIF.            '   )
               call ctx%errch  ( '#', tmpstr                            )
               call ctx%sigerr ( 'SPICE(BUG)'                           )
               return

            end if

         end if

      end do

!
!     Now setup NUMSUP.  Given the way the WHILE loop above executes,
!     we need to subtract one from I to get the number of entries added
!     to SUPBFF.  This smacks of kludge... but it works.
!
      numsup = i - 1

      return
      end subroutine zzddhini

      !> Fetch binary file format.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     BFFID      is an integer code that indicates the binary file
      !>                format that is determined to be native to this
      !>                platform.  Possible values are:
      !>
      !>                   BIGI3E
      !>                   LTLI3E
      !>                   VAXGFL
      !>                   VAXDFL
      !>
      !>                as defined in the include file 'zzddhman.inc'.
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
      !>     1) If the platform on which this code is compiled does not
      !>        produce results that match any of the known binary file
      !>        formats, this routine sets BFFID to 0.
      !>
      !>## Particulars
      !>
      !>     This simple program:
      !>
      !>        PROGRAM DPTEST
      !>
      !>        DOUBLE PRECISION      DPNUM
      !>        INTEGER               INNUM ( 2 )
      !>
      !>        EQUIVALENCE         ( DPNUM, INNUM )
      !>
      !>        DPNUM = 7.0D0
      !>
      !>        WRITE (*,*) DPNUM
      !>        WRITE (*,*) INNUM ( 1 )
      !>        WRITE (*,*) INNUM ( 2 )
      !>
      !>        END
      !>
      !>     produces the following results on these representative platforms:
      !>
      !>        Sun-Solaris (BIGI3E):
      !>
      !>           7.000000000
      !>           1075576832
      !>           0
      !>
      !>        PC-Linux (LTLI3E):
      !>
      !>           7.000000000
      !>           0
      !>           1075576832
      !>
      !>        Alpha-Gfloat (VAXGFL):
      !>
      !>           7.000000000
      !>           16444
      !>           0
      !>
      !>        Alpha-Dfloat (VAXDFL):
      !>
      !>           7.000000000
      !>           16864
      !>           0
      !>
      !>     This routine performs exactly the same decomposition of the
      !>     double precision number 7.0D0 into two integers.  The results
      !>     are checked against those displayed here, and if a match is
      !>     found, returned.
      !>
      !>## Examples
      !>
      !>     See ZZDDHOPN for sample usage.
      !>
      !>## Restrictions
      !>
      !>     This routine derives the binary file format ID for a particular
      !>     platform, but for verification purposes only.  ZZPLATFM should
      !>     be used to obtain the binary file format for the current platform.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 06-AUG-2002 (FST)
      module subroutine zzgetbff ( ctx, bffid )
      use pepper_kinds, only: ip, wp
      use pepper_packing, only: unpack_integers
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: bffid  !! Binary file format code for this system.
!
!     Local Parameters
!
      real(wp), parameter :: dpvalu = 7.0_wp
!
!     Local Variables
!
      integer(ip) :: i
      integer(ip) :: iequiv ( 2 )
!
!     The two integers of 7.0's bits in each binary file format, indexed
!     by BIGI3E, LTLI3E, VAXGFL and VAXDFL (1 to 4). (PEPPER: parameters;
!     SPICELIB: saved arrays set by DATA statements.)
!
      integer(ip), parameter :: int1st ( numbff ) = &
         [ 1075576832, 0, 16444, 16864 ]  !! the first integer, per format
      integer(ip), parameter :: int2nd ( numbff ) = &
         [ 0, 1075576832, 0, 0 ]  !! the second integer, per format

!
!     The two integers of DPVALU's bits (PEPPER: UNPACK_INTEGERS;
!     SPICELIB: an EQUIVALENCE).
!
      call unpack_integers ( [ dpvalu ], 2, iequiv )

!
!     Examine the integer pairs, to identify the binary
!     file format.
!
      bffid = 0

      do i = 1, numbff

         if ( ( iequiv(1) == int1st(i) ) .and. &
              ( iequiv(2) == int2nd(i) )       ) then

            bffid = i

         end if

      end do

      return
      end subroutine zzgetbff

      !> Retrieve string definitions for various CLASS and ID pairs.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     CLASS      is the class of the input ID code.  Acceptable
      !>                values are:
      !>
      !>                   'METHOD' - File access method
      !>                   'ARCH'   - File architecture
      !>                   'BFF'    - Binary file format
      !>
      !>     ID         is a integer value that is defined in the include
      !>                file 'zzddhman.inc'.
      !>
      !>## Detailed Output
      !>
      !>     LABEL      is the 'decoded' ID value that describes a particular
      !>                CLASS/ID pair. In the event that CLASS and ID do not
      !>                correspond to parameters in the include file, the
      !>                routine simply returns a blank string.
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
      !>     1) If the CLASS/ID pair is not known, then this routine
      !>        simply returns a blank label.
      !>
      !>## Particulars
      !>
      !>     This routine is designed to consolidate label definitions for
      !>     parameters in the include file 'zzddhman.inc'.  These labels
      !>     are to be used in error messages and user level subroutines.
      !>
      !>## Examples
      !>
      !>     See ZZDDHMAN for sample usage.
      !>
      !>## Restrictions
      !>
      !>     This routine must be kept compatible with the contents of
      !>     'zzddhman.inc' as its functionality is heavily dependent
      !>     on its contents.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 11-OCT-2005 (NJB)
      !>
      !>        Removed embedded blanks from reference to parameter ARCH.
      !>        Previous spelling was 'ARC  H'.  This was not a problem
      !>        for Fortran compilers but was irritating for human readers.
      !>
      !>-    SPICELIB Version 1.0.0, 07-JUN-2001 (FST)
      module subroutine zzddhgsd ( ctx, class, id, label )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: class  !! String indicating the class of label requested.
      integer(ip), intent(in) :: id  !! Integer ID desired for the decoding.
      character(*), intent(out) :: label  !! Corresponding label for the CLASS/ID pair.
!
!     Local Parameters
!
      integer(ip), parameter :: locsiz = 8
!
!     Integer codes for the currently supported classes.
!
      integer(ip), parameter :: method = 1
      integer(ip), parameter :: arch = method + 1
      integer(ip), parameter :: bff = arch   + 1
!
!     Local Variables
!
      character(locsiz) :: tmpstr
      integer(ip) :: clsid
      if ( .not. allocated ( ctx%zzddhgsd ) ) allocate ( ctx%zzddhgsd )
      associate ( clslst => ctx%zzddhgsd%clslst, &
                  stramh => ctx%zzddhgsd%stramh, &
                  strarc => ctx%zzddhgsd%strarc, &
                  strbff => ctx%zzddhgsd%strbff )
!
!     Left justify and convert the input to upper case.
!
      tmpstr = adjustl(class)
      call ucase_inplace ( tmpstr )

      clsid = findloc(clslst, tmpstr, dim=1)

!
!     Initialize LABEL to the default response.
!
      label = ' '

!
!     Branch on CLSID and return the appropriate label as requested
!     by ID.
!
      if      (       ( clsid == method ) &
                .and. (    id >= 1      ) &
                .and. (    id <= numamh ) ) then

         label = stramh ( id )

      else if (       ( clsid == arch   ) &
                .and. (    id >= 1      ) &
                .and. (    id <= numarc ) ) then

         label = strarc ( id )

      else if (       ( clsid == bff    ) &
                .and. (    id >= 1      ) &
                .and. (    id <= numbff ) ) then

         label = strbff ( id )

      end if

      return
      end associate
      end subroutine zzddhgsd

      !> The function returns a unique enough DP number computed
      !> using the contents of the file attached to the UNIT.
      !>
      !> Return a unique enough DP number ("Magic NuMber") computed
      !> using the contents of the file attached to the specified unit.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     UNIT        is the logical unit attached to a file opened for
      !>                 direct access prior to calling this routine.
      !>
      !>## Detailed Output
      !>
      !>     The function returns a DP number, computed using a few integers
      !>     read from the file attached to the UNIT, that is unique enough
      !>     for the handle manager to check if two files opened for READ
      !>     access are not the same file.
      !>
      !>     If reading the first record of the file or any of the lower level
      !>     routines called by this function fail for any reason, the
      !>     returned value is set to 0.D0.
      !>
      !>## Parameters
      !>
      !>     IDLEN       is the length of the ID word.
      !>
      !>     NINTS       is the number of integers that will be read from
      !>                 a particular record of the file. NINTS must be
      !>                 big enough to make sure that unique pointers from
      !>                 the file record of DAF and DAS files are read and
      !>                 used to compute the output value.
      !>
      !>     The function also uses some DDH general parameters from
      !>     zzddhman.inc.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     This routine and routines in its call tree signal several
      !>     SPICE(BUG) exceptions. They are signaled if the module or modules
      !>     in its calling tree are improperly configured to run on this
      !>     platform.
      !>
      !>## Files
      !>
      !>     The input UNIT must be attached to a file opened for direct
      !>     access prior to calling this function.
      !>
      !>## Particulars
      !>
      !>     This function reads the first IDLEN characters (assumed to be the
      !>     ID word) followed by NINTS integers from the first record of the
      !>     direct access file attached to the input UNIT.
      !>
      !>     If successful, it examines the ID word to determine the file
      !>     architecture.
      !>
      !>     For DAF files it then tries to determine the binary format.
      !>
      !>     For DAF files in the native binary format, it adds up NINTS
      !>     integers read from the first record to get initial output value.
      !>     Then it reads additional NINTS integers from the first descriptor
      !>     record and, if the second read is successful, it adds these
      !>     additional integers to the output value.
      !>
      !>     For DAF files in a non-native binary format supported by run-time
      !>     translation, it re-read NINTS integers from the first record as
      !>     characters, converts them to NINTS integers using ZZXLATEI, and
      !>     adds them up to get initial output value. Then it reads as
      !>     characters NINTS integers from the first descriptor record of the
      !>     file, converts them to NINTS integers using ZZXLATEI and ,if the
      !>     second read is successful, it adds these additional integers to
      !>     the output value.
      !>
      !>     For DAS files, text kernels and unrecognized files, it simply
      !>     adds up NINTS integers to get the output value.
      !>
      !>     If the initial read is not successful, the output value is set to
      !>     zero.
      !>
      !>## Examples
      !>
      !>     See the caller routine, ZZDDHF2H.
      !>
      !>## Restrictions
      !>
      !>     The input UNIT must be attached to a file opened for direct
      !>     access prior to calling this function.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     B.B. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 26-APR-2012 (BVS)
      module real(wp) function zzddhmnm ( ctx, unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Logical unit attached to a file.
      integer(ip), parameter :: idlen = 8
      integer(ip), parameter :: nints = 20
!
!     Local parameters.
!
!     Character buffer size consistent with the number of integers
!     that will be read from the file.
!
      integer(ip), parameter :: nchars = nints * 4
!
!     Minimum and maximum values for the range of ASCII printing
!     characters.
!
      integer(ip), parameter :: minpch = 32
      integer(ip), parameter :: maxpch = 126
!
!     Local variables.
!
      character(idlen) :: arch
      character(idlen) :: idword
      character(idlen) :: type
      character(nchars) :: strbuf
      character(strsiz) :: stramh ( numamh )
      character(strsiz) :: strarc ( numarc )
      character(strsiz) :: strbff ( numbff )
      real(wp) :: mnm
      integer(ip) :: bff
      integer(ip) :: i
      integer(ip) :: intarr ( nints )
      integer(ip) :: iostat
      integer(ip) :: supidx
      if ( .not. allocated ( ctx%zzddhmnm ) ) allocate ( ctx%zzddhmnm )
      associate ( first => ctx%zzddhmnm%first, &
                  natbff => ctx%zzddhmnm%natbff, &
                  supbff => ctx%zzddhmnm%supbff, &
                  numsup => ctx%zzddhmnm%numsup )
!
!     Set default output value to zero.
!
      mnm      = 0.0_wp
      zzddhmnm = 0.0_wp

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform some initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() to handle the unlikely event that
!        ZZDDHINI signaled SPICE(BUG).
!
         if ( ctx%failed() ) return

!
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Read ID word string followed by NINTS integers from the first
!     record of the file.
!
      read ( unit=unit, rec=1, iostat=iostat ) idword, intarr

      if ( iostat == 0 ) then

!
!        Read succeeded. Try to determine the file architecture and
!        type from the ID word. To do this, mimic the part of GETFAT
!        that deals only with the ID word. First replace any non
!        printing ASCII characters in the ID word with blanks, then use
!        IDW2AT on the "cleaned" ID word to get architecture and type.
!
         do i = 1, idlen
            if (      ( ichar(idword(i:i)) < minpch ) &
                 .or. ( ichar(idword(i:i)) > maxpch ) ) then
               idword(i:i) = ' '
            end if
         end do

         call idw2at ( ctx, idword, arch, type )

!
!        Compute the output value based on the file architecture.
!
         if   ( arch == 'DAF' ) then

!
!           For DAF files, try to get the file's binary format.
!
            call zzddhppf ( ctx, unit, daf, bff )

            if ( ctx%failed() ) return

!
!           If the file is in a non-native format, we will need to read
!           the first record again, now directly as characters, and
!           translate these character to native integers.
!
            if ( bff /= natbff ) then

!
!              First, check if run-time translation is supported for
!              this BFF. This check, stolen from ZZDDHMAN, is needed
!              because ZZXLATEI accepts only BFFs for which translation
!              is guaranteed to be supported on this platform. If it
!              is not supported, simply get out (note that the default
!              return value was set to zero at the start.)
!
               supidx = findloc(supbff(1:numsup), bff, dim=1)

               if ( supidx == 0 ) return

!
!              Read the first record as characters and do translation.
!
               read ( unit=unit, rec=1, iostat=iostat ) idword, strbuf

               call zzxlatei ( ctx, bff, strbuf, nints, intarr )

               if ( ctx%failed() ) return

            end if

!
!           Add integers from the file record to the output value.
!
            do i = 1, nints
               mnm = mnm + intarr(i)
            end do

!
!           Read more integers from the start of the first descriptor
!           record without regard to the file's binary format and,
!           if successful, add them to the total.
!
            read ( unit=unit, rec=intarr(18), iostat=iostat ) intarr

            if ( iostat == 0 ) then

               do i = 1, nints
                  mnm = mnm + intarr(i)
               end do

            end if

         else if ( arch == 'DAS' ) then

!
!           For DAS files, for now, add up integers from the first
!           record to get the output value.
!
            do i = 1, nints
               mnm = mnm + intarr(i)
            end do

         else

!
!           For all other files, add up integers from the first record
!           to get the output value.
!
            do i = 1, nints
               mnm = mnm + intarr(i)
            end do

         end if

      else

!
!        The read of the file record failed. Do nothing as the output
!        value has already been set at the start of the function.
!

      end if

      zzddhmnm = mnm

      return
      end associate
      end function zzddhmnm

      !> Prepare preexisting binary file for entry into the handle
      !> table.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     UNIT       is a logical unit attached to the binary file to be
      !>                prepared for inclusion into the handle table.
      !>
      !>     ARCH       is an integer that indicates the architecture of
      !>                the file attached to UNIT.  Acceptable values are
      !>                the parameters:
      !>
      !>                   DAF
      !>                   DAS
      !>
      !>                defined in ZZDDHMAN.INC.
      !>
      !>## Detailed Output
      !>
      !>     BFF        is an integer that indicates the binary file format
      !>                of the DAF attached to UNIT.  Possible values are
      !>                the parameters:
      !>
      !>                   BIGI3E
      !>                   LTLI3E
      !>                   VAXGFL
      !>                   VAXDFL
      !>
      !>                defined in ZZDDHMAN.INC.
      !>
      !>## Parameters
      !>
      !>     See the include file ZZDDHMAN.INC.
      !>
      !>## Exceptions
      !>
      !>     1) SPICE(UNKNOWNFILARC) is signaled when ARCH is not in the
      !>        range of codes for known file architectures or the binary
      !>        file's ID word is unknown to IDW2AT.  BFF is set to 0 when
      !>        this error is signaled.  UNIT is not closed.
      !>
      !>     2) SPICE(FILEREADFAILED) is signaled when either of the two
      !>        READ statements in the module returns non-zero IOSTAT, thus
      !>        indicating read failure.  BFF is set to 0 in this case.  Unit
      !>        is not closed.
      !>
      !>     3) SPICE(FILARCMISMATCH) is signaled when the file attached to
      !>        UNIT is determined to utilize an architecture that is
      !>        different from the one to which the input argument ARCH
      !>        refers.  Unit is not closed.
      !>
      !>     4) SPICE(UNKNOWNBFF) is signaled whenever the binary file
      !>        format detection algorithm reaches a state of uncertainty
      !>        for DAFs.  This can be the result of several conditions,
      !>        an empty pre-N0052 DAF, reading a DAF with an unknown BFF
      !>        from a future toolkit, etc.  In all cases, BFF is set to 0.
      !>        Unit is not closed.
      !>
      !>     5) If a pre-FTP string binary is loaded, no FTP based
      !>        diagnostics are performed, and the file is assumed to be
      !>        in proper, working order.
      !>
      !>## Files
      !>
      !>     This routine reads at least one, and potentially, several records
      !>     from the file attached to UNIT.
      !>
      !>## Particulars
      !>
      !>     This routine exists to prepare a binary file for inclusion
      !>     in the handle table in ZZDDHMAN.  This includes verifying
      !>     that the file is suitable to load and determining the binary
      !>     file format where possible.
      !>
      !>     For DAF files:
      !>
      !>        The binary file format of old (pre-N0050) binaries is
      !>        detectable if the file is non-empty and undamaged.
      !>        New files contain the binary file format identification
      !>        string in the file record along with the FTP error
      !>        detection string.  They are correctly identified in most
      !>        cases, including damaged.
      !>
      !>     For DAS files:
      !>
      !>        The binary file format of old (pre-N0052) binaries is
      !>        not detectable.  This this module will assume that any
      !>        old DAS binaries are of the native format.  New binaries
      !>        include the binary file format identification string as
      !>        well as the FTP error detection string.  They are
      !>        correctly identified in most cases as well.
      !>
      !>     FTP Error Detection:
      !>
      !>        FTP error detection occurs when at least part of the
      !>        detection string is detected in the file record.  When
      !>        absent, no errors are signaled and the file is then
      !>        assumed to be an old binary.  In the event that the FTP
      !>        detection string is present, and additional unknown
      !>        sequences are present, diagnostics are only performed on
      !>        sequences known to this version of the toolkit.
      !>
      !>## Examples
      !>
      !>     See ZZDDHMAN for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) The file attached to UNIT was written on a platform whose
      !>        characters are of a single byte in length.
      !>
      !>     2) Numeric data when read as characters from the UNIT
      !>        preserves the bit patterns present in the file in
      !>        memory.
      !>
      !>     3) The intrinsic ICHAR preserves the bit pattern of the
      !>        character byte read from a file.  Namely if one examines
      !>        the integer created the 8 least significant bits will be
      !>        precisely those found in the character.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.26.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 2.0.1, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 2.0.0, 06-FEB-2002 (FST)
      !>
      !>        This routine was updated to load binaries created by
      !>        N0051 versions of Sun Solaris Native C Toolkits.  See
      !>        the Revisions section for details.
      !>
      !>-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 06-FEB-2002 (FST)
      !>
      !>        Shortly after releasing N0052, a few of our users
      !>        discovered that they were unable to load binary
      !>        DAFs created with the N0051 Sun Solaris Native C
      !>        (SUN-SOLARIS-NATIVE_C) Toolkits.  The reason for this
      !>        is the previous version of ZZDDHPPF released with N0052
      !>        assumed that if a DAF file record possessed a valid
      !>        FTP error detection string, then it must contain a
      !>        binary file format ID string as well.  Both were added
      !>        to the DAF file record in N0050.
      !>
      !>        However, a bug in the N0051 version of the ZZPLATFM
      !>        master file, the source of the binary file format ID
      !>        string for a given platform, neglected to assign a
      !>        value to the string.  Since it was a C environment
      !>        and the implementation of ZZPLATFM resulted in the
      !>        string being a static variable, it was initialized
      !>        to nulls and written into the file.
      !>
      !>        This version of ZZDDHPPF has been extended to recognize
      !>        a null binary file format ID, and apply the byte
      !>        examination algorithm used on pre-N0050 DAFs to determine
      !>        its format.
      module subroutine zzddhppf ( ctx, unit, arch, bff )
      use pepper_kinds, only: ip
      use pepper_strings, only: pos, ucase_inplace
      use pepper_inc_zzddhman
      use pepper_inc_zzftprms
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Logical unit attached to the binary file.
      integer(ip), intent(in) :: arch  !! Integer code indicating the file architecture.
      integer(ip), intent(out) :: bff  !! Integer code indicating the binary file format.
!
!     Local Parameters
!
!     Number of characters to be read in from a record.
!
      integer(ip), parameter :: numchr = 1000
!
!     Bounding indices for the window that brackets the FTP
!     error detection string in the file record.
!
      integer(ip), parameter :: ftpstr = 500
      integer(ip), parameter :: ftpend = 1000
!
!     Index of the start of the binary file format identification
!     string in DAF binaries.
!
      integer(ip), parameter :: dafbff = 89
!
!     Index of the start of the binary file format identification
!     string in DAS binaries.
!
      integer(ip), parameter :: dasbff = 85
!
!     Size of the binary format identification string.
!
      integer(ip), parameter :: bffsiz = 8
!
!     Index of the first byte of NI in the DAF file record.
!
      integer(ip), parameter :: nipos = 13
!
!     Index of the first byte of NSUM in the DAF descriptor record.
!
      integer(ip), parameter :: nsmpos = 17
!
!     Index of the first byte of FDREC in the DAF file record.
!
      integer(ip), parameter :: fdrpos = 77
!
!     Integer code such that CHAR(INTNUL) produces the NULL character.
!
      integer(ip), parameter :: intnul = 0
!
!     IDW2AT Output Argument Lengths.
!
      integer(ip), parameter :: idwlen = 4
!
!     NULLID is the index of the extended STRBFF "NULL" string ID.
!
      integer(ip), parameter :: nullid = numbff+1
!
!     Local Variables
!
      character(1) :: carg
      character(numchr) :: chrrec
      character(bffsiz) :: bffidw
      character(idwlen) :: filarc
      character(idwlen) :: filtyp
      integer(ip) :: fdrec
      integer(ip) :: ftppos
      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: tstarc
      logical :: found
      logical :: ftperr
!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
!     On some supported environments, ICHAR is not sufficient to
!     produce the desired results.  This however, is not the case
!     with this particular environment.
!     Some versions of the g77 implement ICHAR with a signed integer.
!     This function computes the value of ICHAR that this code requires
!     on any version of g77 for x86 Linux.
!
!     Statement Functions
!
      integer(ip) :: zzichr
!     PEPPER: one portable form (ICHAR is 0:255 with current compilers; the
!     MODULO also maps a signed ICHAR, as SPICELIB's g77 branch did).
      zzichr(carg) = modulo ( ichar(carg), 256 )
      if ( .not. allocated ( ctx%zzddhppf ) ) allocate ( ctx%zzddhppf )
      associate ( first => ctx%zzddhppf%first, &
                  ftpdlm => ctx%zzddhppf%ftpdlm, &
                  ftplft => ctx%zzddhppf%ftplft, &
                  ftpmem => ctx%zzddhppf%ftpmem, &
                  ftprgt => ctx%zzddhppf%ftprgt, &
                  null => ctx%zzddhppf%null, &
                  strarc => ctx%zzddhppf%strarc, &
                  strbff => ctx%zzddhppf%strbff )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If this is the first time into the routine, populate local
!     copies of reference values.  This includes the names of the
!     BFF parameters, the names of the ARCH parameters, and the
!     local copy of the FTP string.
!
      if ( first ) then

!
!        Construct and store the NULL valued byte.
!
         null = char(intnul)

!
!        Retrieve the BFF and ARCH names.
!
         do i = 1, numbff
            call zzddhgsd ( ctx, 'BFF', i, strbff(i) )
         end do

         do i = 1, numarc
            call zzddhgsd ( ctx, 'ARCH', i, strarc(i) )
         end do

!
!        Extend STRBFF to include the null BFFID.  This addresses
!        the N0051 Sun Solaris Native C toolkit binary files.
!
         do i = 1, bffsiz
            strbff(nullid) (i:i) = null
         end do

!
!        Fetch the FTP string.
!
         call zzftpstr ( ctx, ftpmem, ftplft, ftprgt, ftpdlm )

!
!        Set FIRST to FALSE so we will not reassign any of these values.
!
         first = .false.

      end if

!
!     Get the simple consistency checks out of the way first.  Is
!     the input ARCH value valid?
!
      if ( ( arch <= 0 ) .or. ( arch > numarc ) ) then

         bff = 0

         call ctx%setmsg ( 'The integer code, ''#'' indicating the file ' &
         //            'architecture to examine is out of range.'     )
         call ctx%errint ( '#', arch                                      )
         call ctx%sigerr ( 'SPICE(UNKNOWNFILARC)'                         )
         return

      end if

!
!     Read the first record from the file as a string of NUMCHR
!     characters.
!
      read ( unit, rec=1, iostat=iostat ) chrrec

!
!     Check for read failure.
!
      if ( iostat /= 0 ) then

         bff = 0

         call ctx%setmsg ( 'Error reading the file record from the ' &
         //            'binary DAF file ''#''.  IOSTAT = #.'     )
         call ctx%errfnm ( '#', unit                                 )
         call ctx%errint ( '#', iostat                               )
         call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                   )
         return

      end if

!
!     First check the ID word from the input file.
!
      call idw2at ( ctx, chrrec(1:8), filarc, filtyp )

!
!     Now locate FILARC in the STRARC array.
!
      tstarc = findloc(strarc, filarc, dim=1)

!
!     If FILARC was not found, signal an appropriate error.
!
      if ( tstarc == 0 ) then

         bff = 0

         call ctx%setmsg ( 'The file, #, has a unidentified file ' &
         //            'architecture.  Check that this file ' &
         //            'is a properly created binary SPICE ' &
         //            'kernel.'                               )
         call ctx%errfnm ( '#', unit                               )
         call ctx%sigerr ( 'SPICE(UNKNOWNFILARC)'                  )
         return

!
!     Otherwise we have an architecture mismatch error, if
!     FILARC does not agree with ARCH.
!
      else if ( tstarc /= arch ) then

         bff = 0

         call ctx%setmsg ( 'A request to load the # file, $, has ' &
         //            'been made by the % system.  This operation ' &
         //            'is not permitted.'                           )
         call ctx%errch  ( '#', strarc(tstarc)                           )
         call ctx%errfnm ( '$', unit                                     )
         call ctx%errch  ( '%', strarc(arch)                             )
         call ctx%sigerr ( 'SPICE(FILARCHMISMATCH)'                      )
         return

      end if

!
!     Now check for possible FTP transfer errors.
!
      call zzftpchk ( ctx, chrrec(ftpstr:ftpend), ftperr )

      if ( ftperr ) then

         bff = 0

         call ctx%setmsg ( 'FTP transfer error detected.  This ' &
         //            'binary $, ''#'', has most likely been ' &
         //            'corrupted by an ASCII mode FTP transfer. ' &
         //            'Obtain the file using IMAGE or BINARY ' &
         //            'transfer mode from the source.'            )
         call ctx%errch  ( '$', strarc(tstarc)                         )
         call ctx%errfnm ( '#', unit                                   )
         call ctx%sigerr ( 'SPICE(FTPXFERERROR)'                       )
         return

      end if

!
!     Now this search is redundant, but the presence of the
!     FTPLFT string in the latter half of the file record
!     is fairly conclusive evidence that this is a "new" binary,
!     and we can expect to locate the binary file format
!     identification string.
!
      ftppos = pos ( chrrec(500:1000), ftplft, 1 )

!
!     Check to see if we found FTPLFT.  If so extract the binary
!     file format ID word from the file record.
!
      if ( ftppos /= 0 ) then

!
!        Extract BFFIDW from CHRREC.
!
         if ( arch == daf ) then
            bffidw = chrrec ( dafbff : (dafbff+bffsiz-1) )
         else if ( arch == das ) then
            bffidw = chrrec ( dasbff : (dasbff+bffsiz-1) )
         end if

!
!        See if we can find BFFIDW in the STRBFF list.
!
         bff = findloc(strbff(1:numbff + 1), bffidw, dim=1)

!
!        Check to see if BFF is 0, if it is, signal an error since
!        this indicates an unrecognized BFF.
!
         if ( bff == 0 ) then

            call ctx%setmsg ( 'The file ''#'' utilizes the binary file ' &
            //            'format ''#''.  This format is currently ' &
            //            'unknown to this toolkit.  A toolkit ' &
            //            'update may be in order.'                  )
            call ctx%errfnm ( '#', unit                                  )
            call ctx%errch  ( '#', bffidw                                )
            call ctx%sigerr ( 'SPICE(UNKNOWNBFF)'                        )
            return

         end if

!
!        See if we have a NULLID situation, if not check out and
!        return as swe have identified the BFF.
!
         if ( bff /= nullid ) return

      end if

!
!     There is no FTP string, if the file is a DAS, we have to
!     assume it is of the native architecture.
!
      if ( arch == das ) then

         call zzplatfm ( ctx, 'FILE_FORMAT', bffidw )
         call ucase_inplace ( bffidw )

         bff = findloc(strbff(1:numbff), bffidw, dim=1)

         if ( bff == 0 ) then

            call ctx%setmsg ( 'The native architecture for this ' &
            //            'platform is unknown to this version of ' &
            //            'the toolkit. This is a severe problem ' &
            //            'that should never occur, please contact ' &
            //            'NAIF.'                                    )
            call ctx%sigerr ( 'SPICE(BUG)'                               )
            return

         end if

         return

      end if

!
!     If we reach this point, then we are either dealing with
!     an old DAF (created by a pre-N0050 toolkit) or one of the
!     DAFs created by the N0051 Sun Solaris Native C version of
!     the toolkit.  This requires an examination of the bits
!     and bytes in the file that works this way:
!
!        Since in a valid DAF, 2 <= NI <= 250, we can easily
!        determine whether the 4 bytes used to store NI in the
!        file record are little or big endian.  If we discover
!        that the integer is encoded as big-endian, then stop
!        as this file must be 'BIG-IEEE'.  If it is little
!        endian, then locate the first descriptor record
!        in the file.
!
!        Read the first descriptor record.  Extract NSUM, the
!        3rd DP from the record.  If it is 0.0D0, signal an error
!        as this is an empty DAF and we can not determine its
!        type.  If it's non-zero, then check to see if the first
!        4 bytes are "0s".  If they are it must be 'LTL-IEEE'.
!        Otherwise pass it off to ZZDDHIVF to discriminate between
!        'VAX-GFLT' and 'VAX-DFLT'.  We know the first 4 bytes must
!        be "0s" in the 'LTL-IEEE" case, since NSUM is subject to
!        the following inequality: 1 <= NSUM <= 125
!
!     Having laid out the scheme, let's get to it.  First take a
!     look at the four character bytes that hold NI.  These bytes
!     be one of the following:
!
!        Little Endian:  VAL, 0, 0, 0
!           Big Endian:    0, 0, 0, VAL
!
!     where VAL is some non-zero value.
!
      if ( ( chrrec(     nipos:nipos     ) == null ) .and. &
           ( chrrec( (nipos+1):(nipos+1) ) == null ) .and. &
           ( chrrec( (nipos+2):(nipos+2) ) == null ) .and. &
           ( chrrec( (nipos+3):(nipos+3) ) /= null )       ) then

         bff = bigi3e

      else if ( ( chrrec(     nipos:nipos     ) /= null ) .and. &
                ( chrrec( (nipos+1):(nipos+1) ) == null ) .and. &
                ( chrrec( (nipos+2):(nipos+2) ) == null ) .and. &
                ( chrrec( (nipos+3):(nipos+3) ) == null )       ) then

!
!        At this point we know we are dealing with a little endian
!        file.  Locate the first descriptor record.
!
         fdrec = zzichr( chrrec(     fdrpos:fdrpos     ) )
         fdrec = zzichr( chrrec( (fdrpos+1):(fdrpos+1) ) )*16   + fdrec
         fdrec = zzichr( chrrec( (fdrpos+2):(fdrpos+2) ) )*256  + fdrec
         fdrec = zzichr( chrrec( (fdrpos+3):(fdrpos+3) ) )*4096 + fdrec

!
!        Read the record into CHRREC.
!
         read ( unit, rec=fdrec, iostat=iostat ) chrrec

!
!        Check for read failure.
!
         if ( iostat /= 0 ) then

            bff = 0

            call ctx%setmsg ( 'Error reading a descriptor record from ' &
            //            'the binary DAF file ''#''.  IOSTAT = #.' )
            call ctx%errfnm ( '#', unit                                 )
            call ctx%errint ( '#', iostat                               )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                   )
            return

         end if

!
!        Now examine the NSUM DP in this record to determine the
!        architecture.
!
         if ( ( chrrec(     nsmpos:nsmpos )     == null ) .and. &
              ( chrrec( (nsmpos+1):(nsmpos+1) ) == null ) .and. &
              ( chrrec( (nsmpos+2):(nsmpos+2) ) == null ) .and. &
              ( chrrec( (nsmpos+3):(nsmpos+3) ) == null ) .and. &
              ( chrrec( (nsmpos+4):(nsmpos+4) ) == null ) .and. &
              ( chrrec( (nsmpos+5):(nsmpos+5) ) == null ) .and. &
              ( chrrec( (nsmpos+6):(nsmpos+6) ) == null ) .and. &
              ( chrrec( (nsmpos+7):(nsmpos+7) ) == null )       ) then

!
!           In this case we have an empty DAF, and can not distinguish
!           between little endian formats.  Signal an error and return.
!
            bff = 0

            call ctx%setmsg ( 'The DAF, ''#'', appears to contain no ' &
            //            'data.  As such, its binary file format ' &
            //            'can not be determined which prevents ' &
            //            'it from being loaded.'                   )
            call ctx%errfnm ( '#', unit                                 )
            call ctx%sigerr ( 'SPICE(UNKNOWNBFF)'                       )
            return

         else if ( ( chrrec(     nsmpos:nsmpos     ) == null ) .and. &
                   ( chrrec( (nsmpos+1):(nsmpos+1) ) == null ) .and. &
                   ( chrrec( (nsmpos+2):(nsmpos+2) ) == null ) .and. &
                   ( chrrec( (nsmpos+3):(nsmpos+3) ) == null )  ) then

!
!           In this case the file is little endian IEEE.  Set BFF.
!
            bff = ltli3e

         else

!
!           We are probably looking at a VAX file.  Find out which
!           format.
!
            call zzddhivf ( ctx, chrrec(nsmpos:(nsmpos+7)), bff, found )

            if ( .not. found ) then

               bff = 0

               call ctx%setmsg ( 'Unable to determine the binary file ' &
               //            'format of DAF ''#''.'                 )
               call ctx%errfnm ( '#', unit                              )
               call ctx%sigerr ( 'SPICE(UNKNOWNBFF)'                    )
               return

            end if

         end if

      else

         bff = 0

      end if

      return
      end associate
      end subroutine zzddhppf

      !> Extract the architecture and type of a SPICE binary kernel file
      !> from a file ID word.
      !>
      !>## Detailed Input
      !>
      !>     IDWORD   is the ID word from a SPICE binary kernel file or a
      !>              text version of a binary kernel file whose
      !>              architecture and type are to be extracted.
      !>
      !>## Detailed Output
      !>
      !>     ARCH     is the file architecture used to store the data in
      !>              a SPICE binary kernel file. If the architecture cannot
      !>              be extracted or is not recognized the value '?' is
      !>              returned.
      !>
      !>              The possible architectures are:
      !>
      !>                 ASC -- An ASCII text file.
      !>                 DAF -- A DAF based file.
      !>                 DAS -- A DAS based file.
      !>                 KPL -- Kernel Pool File (i.e., a text kernel)
      !>                 TXT -- An ASCII text file.
      !>                 TE1 -- Text E-Kernel type 1.
      !>
      !>     TYPE     is the type of the SPICE file. If the type can not be
      !>              extracted or if it is blank, the value '?' is
      !>              returned.
      !>
      !>              The type can only be extracted by this routine if
      !>              the ID word follows the convention
      !>
      !>                 <architecture>/<type>
      !>
      !>              where <architecture> is one of the file architectures
      !>              specified above, and
      !>
      !>                 <type> = 'xxxx'
      !>
      !>              where 'xxxx' represents a four character mnemonic or
      !>              code for the file type.
      !>
      !>              This subroutine does not do any checking of the file
      !>              types. If a valid architecture is found and the type
      !>              is non-blank, that is what will be returned. It is up
      !>              to a higher level authority to determine whether a type
      !>              is valid.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the variable ID word is blank, both the architecture and
      !>         type will be unknown, specified by '?'.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This subroutine is a support utility routine that attempts
      !>     to extract the architecture and type of a file from its ID word.
      !>     It may not be possible to determine the type of the file from the
      !>     ID word alone. Older files which contain the ID words 'NAIF/NIP',
      !>     or 'NAIF/DAF' do not have sufficient information in the ID word to
      !>     determine the type of the file. A type for the ID word 'NAIF/DAS'
      !>     is always 'PRE ', since files with this ID word were pre-release
      !>     DAS files.
      !>
      !>     A file architecture can always be extracted from a valid SPICE
      !>     ID word.
      !>
      !>     This subroutine and the subroutine GETFAT (get file architecture
      !>     and type) are intimately related. Whenever one of them is modified
      !>     the other should be checked to see if the modifications affect it.
      !>     Whenever a new architecture is added, both of the subroutines are
      !>     affected.
      !>
      !>## Examples
      !>
      !>     Suppose you wish to write a single routine for converting files
      !>     between text and binary formats. You can use this routine to
      !>     determine the architecture and type of the file and then pass the
      !>     file to the appropriate low level file conversion routine to
      !>     handle the actual conversion.
      !>
      !>        CALL IDW2AT ( IDWORD, ARCH, TYPE )
      !>
      !>        IF ( ARCH .EQ. 'DAF' ) THEN
      !>
      !>           convert a DAF file
      !>
      !>        ELSE IF ( ARCH .EQ. 'DAS' ) THEN
      !>
      !>           convert a DAS file
      !>
      !>        ELSE
      !>
      !>           WRITE(*,*) 'File architecture not supported.'
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
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.0, 26-OCT-1995 (KRG)
      !>
      !>        Changed the Version line from "Beta" to "SPICELIB" for the
      !>        current revisions. The subroutine was already in SPICELIB,
      !>        but the Version line said "Beta."
      !>
      !>        Added several new architectures:
      !>
      !>           KPL -- Kernel Pool File (i.e., a text kernel)
      !>           TXT -- An ASCII text file.
      !>           ASC -- An ASCII text file.
      !>           TE1 -- Text E-Kernel type 1.
      !>
      !>        Changed the response foe the ID word 'NAIF/DAS' to be
      !>        consistent with GETFAT. It now sets the architecture to 'DAS'
      !>        and the type to 'PRE', for pre-release version.
      !>
      !>-    SPICELIB Version 1.0.0, 30-SEP-1993 (KRG)
      !>
      !>## Revisions
      !>
      !>-     SPICELIB Version 2.0.0, 26-OCT-1995 (KRG)
      !>
      !>         Changed the Version line from "Beta" to "SPICELIB" for the
      !>         current revisions. The subroutine was already in SPICELIB,
      !>         but the Version line said "Beta."
      !>
      !>         Added several new architectures:
      !>
      !>            KPL -- Kernel Pool File (i.e., a text kernel)
      !>            TXT -- An ASCII text file.
      !>            ASC -- An ASCII text file.
      !>            TE1 -- Text E-Kernel type 1.
      !>
      !>         Changed the response foe the ID word 'NAIF/DAS' to be
      !>         consistent with GETFAT. It now sets the architecture to 'DAS'
      !>         and the type to 'PRE', for pre-release version.
      module subroutine idw2at ( ctx, idword, arch, type )
      use pepper_kinds, only: ip
      use pepper_strings, only: pos
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: idword  !! The IDWORD to be examined.
      character(*), intent(out) :: arch  !! The file architecture DAS or DAF.
      character(*), intent(out) :: type  !! The type of the file.

!
!     Set the length of a SPICE file ID word.
!
      integer(ip), parameter :: idwlen = 8
      character(idwlen) :: part1
      character(idwlen) :: part2

      integer(ip) :: slash
!
!     Standard obligatory error handling stuff.
!
      if ( ctx%failed() ) return
!
!     Check to see if we got a blank string for the ID word. If we did,
!     set the architecture and type to unknown.
!

      if ( idword == ' ' ) then

         arch = '?'
         type = '?'

         return

      end if
!
!     Initialize the temporary storage variables that we use.
!
      part1  = ' '
      part2  = ' '
!
!     See if we can get the architecture and type from the ID word.
!
!     Look for a '/' in the string. If we can't find it, we don't
!     recognize the architecture or the type, so set the architecture
!     and type to unknown.
!
      slash = pos( idword, '/', 1 )

      if ( slash == 0 ) then

         arch = '?'
         type = '?'

         return

      end if
!
!     The part before the slash is the architecture or the word 'NAIF'
!     in older files and the part after the slash is the type of file or
!     the architecture in older files.
!
      part1 = idword(1:slash-1)
      part2 = idword(slash+1:)
!
!     Let's now do some testing to try and figure out what's going on.
!
!     First we look for the information in the ID word format:
!
!        <architecture>/<type>,
!
!    then we look for the things that begin with the word 'NAIF'
!
      if ( part1 == 'DAF' ) then
!
!        We have a DAF file, so set the architecture and type.
!
         arch = 'DAF'

         if ( part2 /= ' ' ) then
            type = part2
         else
            type = '?'
         end if

      else if ( part1 == 'DAS' ) then
!
!        We have a DAS file, so set the architecture and type.
!
         arch = 'DAS'

         if ( part2 /= ' ' ) then
            type = part2
         else
            type = '?'
         end if

      else if ( part1 == 'TXT' ) then
!
!        We have an ASCII text file, so set the architecture and type.
!
         arch = 'TXT'

         if ( part2 /= ' ' ) then
            type = part2
         else
            type = '?'
         end if

      else if ( part1 == 'ASC' ) then
!
!        We have an ASCII text file, so set the architecture and type.
!
         arch = 'TXT'

         if ( part2 /= ' ' ) then
            type = part2
         else
            type = '?'
         end if

      else if ( part1 == 'KPL' ) then
!
!        We have a kernel pool file, so set the architecture and type.
!
         arch = 'KPL'

         if ( part2 /= ' ' ) then
            type = part2
         else
            type = '?'
         end if

      else if (  part1 == 'NAIF' )  then
!
!        We have a DAF (or NIP, these are equivalent) or DAS file,
!        identified by the value of PART2, but we have no idea what the
!        type is, unless the file is a DAS file, in which case it is a
!        pre-release EK file, since these are the only DAS files which
!        used the 'NAIF/DAS' ID word.
!
!        First, we determine the architecture from PART2, then if it is
!        DAF or NIP, we give up on the type. As mentioned above, if
!        PART2 contains DAS, we know a priori the type of the file.
!
         if ( ( part2 == 'DAF' ) .or. ( part2 == 'NIP' ) ) then

            arch = 'DAF'
            type = '?'

         else if ( part2 == 'DAS' ) then

            arch = 'DAS'
            type = 'PRE'

         else

            arch = '?'
            type = '?'

         end if

      else

         arch = '?'
         type = '?'

      end if

      return

      end subroutine idw2at

      !> Convert integers from one binary file format to another.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     INBFF      is an integer code that indicates the binary file
      !>                format of INPUT.  Acceptable values are the
      !>                parameters:
      !>
      !>                   BIGI3E
      !>                   LTLI3E
      !>                   VAXGFL
      !>                   VAXDFL
      !>
      !>                as defined in the include file 'zzddhman.inc'.
      !>
      !>     INPUT      is a string containing a group of integers read
      !>                from a file as a character string.  The length of
      !>                this string must be a multiple of the number of
      !>                bytes used to store an integer in a file utilizing
      !>                INBFF.
      !>
      !>     SPACE      is the number of integers that OUTPUT has room to
      !>                store.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT     is an array of integers containing the translated
      !>                values from INPUT into the native binary format.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     This routine signals several SPICE(BUG) exceptions.  They are
      !>     signaled when improperly specified inputs are passed into the
      !>     routine or if the module or modules in its calling tree are
      !>     improperly configured to run on this platform.  Callers that
      !>     prevent invalid inputs from being passed into this routine
      !>     need not check in.  See the $Restrictions section for a
      !>     discussion of input argument restrictions.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine translates integers from a non-native integer format
      !>     read from a file as a sequence of characters to the native format.
      !>
      !>## Examples
      !>
      !>     See ZZDAFGFR, ZZDAFGSR.
      !>
      !>## Restrictions
      !>
      !>     1) Numeric data when read as characters from a file preserve
      !>        the bit patterns present in the file in memory.
      !>
      !>     2) A byte is 8 bits, and a character is some multiple of
      !>        bytes.
      !>
      !>     3) The intrinsic ICHAR preserves the bit pattern of the character
      !>        byte read from a file.  Namely if one examines the integer
      !>        created the 8 least significant bits will be precisely those
      !>        found in the character.
      !>
      !>     4) The size of integers on the target environment are a multiple
      !>        of some number of bytes.
      !>
      !>     5) The length of the INPUT string is a multiple of the number
      !>        of bytes for an integer in the INBFF format.
      !>
      !>     6) INBFF is supported for reading on this platform, and not
      !>        equivalent to NATBFF on this platform.
      !>
      !>     7) This routine must support all of the non-native translations
      !>        required by the 'READS_BFF' key in ZZPLATFM.
      !>
      !>     8) The character label corresponding to INBFF must be one of the
      !>        non-native entries in the value of 'READS_BFF' returned by
      !>        ZZPLATFM for this environment.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.27.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 1.26.1, 01-OCT-2021 (NJB)
      !>
      !>        Fixed typo in comments.
      !>
      !>-    SPICELIB Version 1.26.0, 15-NOV-2015 (EDW)
      !>
      !>        Recast J = 4*(-1+I) + 1 as J = -3+4*I to accommodate
      !>        f2c processing.
      !>
      !>-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
      module subroutine zzxlatei ( ctx, inbff, input, space, output )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmin
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inbff  !! Binary file format code for integers in INPUT.
      character(*), intent(in) :: input  !! String containing integers read as characters.
      integer(ip), intent(in) :: space  !! Number of integers that can be placed in OUTPUT.
      integer(ip), intent(out) :: output ( * )  !! Translated integer values.
!
!     Local Parameters
!
!     These parameters are used for arithmetic shifting.
!
      integer(ip), parameter :: shft8 = 2**8
      integer(ip), parameter :: shft16 = 2**16
      integer(ip), parameter :: shft24 = 2**24
!
!     Local Variables
!
      character(1) :: carg
      character(strsiz) :: tmpstr
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: lenipt
      integer(ip) :: numint
      integer(ip) :: osign
      integer(ip) :: value
!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
!     On some supported environments, ICHAR is not sufficient to
!     produce the desired results.  This however, is not the case
!     with this particular environment.
!     Some versions of the g77 implement ICHAR with a signed integer.
!     This function computes the value of ICHAR that this code requires
!     on any version of g77 for x86 Linux.
!
!     Statement Functions
!
      integer(ip) :: zzichr
!     PEPPER: one portable form (ICHAR is 0:255 with current compilers; the
!     MODULO also maps a signed ICHAR, as SPICELIB's g77 branch did).
      zzichr(carg) = modulo ( ichar(carg), 256 )
      if ( .not. allocated ( ctx%zzxlatei ) ) allocate ( ctx%zzxlatei )
      associate ( first => ctx%zzxlatei%first, &
                  natbff => ctx%zzxlatei%natbff, &
                  strbff => ctx%zzxlatei%strbff, &
                  smlint => ctx%zzxlatei%smlint, &
                  bigint => ctx%zzxlatei%bigint )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform some initialization tasks.
!
      if ( first ) then

!
!        Populate STRBFF with the appropriate binary file
!        format labels.
!
         do i = 1, numbff
            call zzddhgsd ( ctx, 'BFF', i, strbff(i) )
         end do

!
!        Fetch the native binary file format.
!
         call zzplatfm ( ctx, 'FILE_FORMAT', tmpstr )
         call ucase_inplace ( tmpstr )

         natbff = findloc(strbff, tmpstr, dim=1)

         if ( natbff == 0 ) then

            call ctx%setmsg ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            call ctx%errch  ( '#', tmpstr                                  )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

!
!        Store the largest value a 32-bit integer can actually
!        hold.
!
         bigint = 2147483647

!
!        Prepare the smallest value a 32-bit integer can actually
!        store, regardless of what INTMIN returns.
!
         smlint = intmin

!
!        Set SMLINT to the appropriate value if INTMIN is too large.
!
         if ( smlint == -2147483647 ) then
            smlint = smlint - 1
         end if

!
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Check to see if INBFF is valid.  This should never occur if this
!     routine is called properly.
!
      if ( ( inbff < 1 ) .or. ( inbff > numbff ) ) then

         call ctx%setmsg ( 'The integer code used to indicate the ' &
         //            'binary file format of the input integers, ' &
         //            '#, is out of range.  This error should ' &
         //            'never occur.'                               )
         call ctx%errint ( '#', inbff                                   )
         call ctx%sigerr ( 'SPICE(BUG)'                                 )
         return

      end if

!
!     Retrieve the length of the input string.
!
      lenipt = len(input)

!
!     Now branch based on the value of NATBFF.
!
      if ( natbff == bigi3e ) then

         if ( inbff == ltli3e ) then

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing LTL-IEEE
!           integers and this is a BIG-IEEE machine, characters are
!           1-byte and integers are 4-bytes.  So the length of INPUT
!           must be a multiple of 4.
!
            numint = lenipt / 4

            if ( lenipt - ( numint*4 ) /= 0 ) then

               call ctx%setmsg ( 'The input string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 4 bytes.  This ' &
               //            'error should never occur.'           )
               call ctx%errch  ( '#', strbff(inbff)                    )
               call ctx%errch  ( '#', strbff(natbff)                   )
               call ctx%sigerr ( 'SPICE(BUG)'                          )
               return

            end if

!
!           Verify there is enough room to store the results of
!           the translation.
!
            if ( numint > space ) then

               call ctx%setmsg ( 'The caller specified that # integers ' &
               //            'are to be translated from binary ' &
               //            'format # to #.  However there is only ' &
               //            'room to hold # integers in the output ' &
               //            'array.  This error should never occur.' )
               call ctx%errint ( '#', numint                              )
               call ctx%errch  ( '#', strbff(inbff)                       )
               call ctx%errch  ( '#', strbff(natbff)                      )
               call ctx%errint ( '#', space                               )
               call ctx%sigerr ( 'SPICE(BUG)'                             )
               return

            end if

!
!           Start looping over each 4 character package in INPUT and
!           converting them to integers.
!
            do i = 1, numint

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               j = -3 + 4*i

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing LTL-IEEE, we know that
!              J is the least significant byte and that (J+3) is the
!              most significant.
!
!              INPUT:
!
!                      -------------------------------------
!                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
!                      -------------------------------------
!
!              From this we construct OUTPUT(I) using the following
!              relation:
!
!                      INPUT(J:J)
!                      INPUT(J+1:J+1) shifted 8 bits to the MSb
!                      INPUT(J+2:J+2) shifted 16 bits to the MSb
!                   +  INPUT(J+3:J+3) shifted 24 bits to the MSb
!                   -------------------------
!                      OUTPUT(I)
!
!              Perform the necessary computations.  What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               value     = zzichr( input(j:j) )
               output(i) = value

               value     = zzichr( input((j+1):(j+1)) )
               value     = value*shft8
               output(i) = output(i)+value

               value     = zzichr( input((j+2):(j+2)) )
               value     = value*shft16
               output(i) = output(i)+value

               value     = zzichr( input((j+3):(j+3)) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
               osign = value / 128

!
!              Strip the sign bit if necessary.
!
               if ( osign == 1 ) then
                  value = value - 128
               end if

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with OUTPUT(I).
!
               value     = value*shft24
               output(i) = output(i)+value

!
!              Install the sign bit.  At the moment in OUTPUT(I)
!              we have the bits precisely as they need to be
!              arranged.  Perform the following computations:
!
!                 OUTPUT(I) = (2**31-1) - OUTPUT(I) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First negate the result:
!
!                 OUTPUT(I) = -OUTPUT(I)
!
!              But this negation is effectively:
!
!                 OUTPUT(I) = 2**32 - OUTPUT(I)
!
!              Which yields:
!
!                 2**32 - (2**31) + OUTPUT(I)
!
!              or
!
!                 2**31 + OUTPUT(I)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               if ( osign == 1 ) then
                  if ( output(i) == 0 ) then
                     output(i) = smlint
                  else
                     output(i) = bigint-output(i)
                     output(i) = output(i) + 1
                     output(i) = -output(i)
                  end if
               end if

            end do

         else

            call ctx%setmsg ( 'Unable to translate integers from ' &
            //            'binary file format # to #.  This error ' &
            //            'should never occur and is indicative ' &
            //            'of a bug.  Contact NAIF.'                )
            call ctx%errch  ( '#', strbff(inbff)                        )
            call ctx%errch  ( '#', strbff(natbff)                       )
            call ctx%sigerr ( 'SPICE(BUG)'                              )
            return

         end if

      else if ( natbff == ltli3e ) then

         if ( inbff == bigi3e ) then

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing BIG-IEEE
!           integers and this is a LTL-IEEE machine, characters are
!           1-byte and integers are 4-bytes.  So the length of INPUT
!           must be a multiple of 4.
!
            numint = lenipt / 4

            if ( lenipt - ( numint*4 ) /= 0 ) then

               call ctx%setmsg ( 'The input string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 4 bytes.  This ' &
               //            'error should never occur.'           )
               call ctx%errch  ( '#', strbff(inbff)                    )
               call ctx%errch  ( '#', strbff(natbff)                   )
               call ctx%sigerr ( 'SPICE(BUG)'                          )
               return

            end if

!
!           Verify there is enough room to store the results of
!           the translation.
!
            if ( numint > space ) then

               call ctx%setmsg ( 'The caller specified that # integers ' &
               //            'are to be translated from binary ' &
               //            'format # to #.  However there is only ' &
               //            'room to hold # integers in the output ' &
               //            'array.  This error should never occur.' )
               call ctx%errint ( '#', numint                              )
               call ctx%errch  ( '#', strbff(inbff)                       )
               call ctx%errch  ( '#', strbff(natbff)                      )
               call ctx%errint ( '#', space                               )
               call ctx%sigerr ( 'SPICE(BUG)'                             )
               return

            end if

!
!           Start looping over each 4 character package in INPUT and
!           converting them to integers.
!
            do i = 1, numint

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               j = -3 + 4*i

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing BIG-IEEE, we know that
!              J is the most significant byte and that (J+3) is the
!              least significant.
!
!              INPUT:
!
!                      -------------------------------------
!                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
!                      -------------------------------------
!
!              From this we construct OUTPUT(I) using the following
!              relation:
!
!                      INPUT(J+3:J+3)
!                      INPUT(J+2:J+2)*SHFT8
!                      INPUT(J+1:J+1)*SHFT16
!                   +  INPUT(J:J)*SHFT24
!                   -------------------------
!                      OUTPUT(I)
!
!              Perform the necessary computations.  What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               value     = zzichr( input((j+3):(j+3)) )
               output(i) = value

               value     = zzichr( input((j+2):(j+2)) )
               value     = value*shft8
               output(i) = output(i)+value

               value     = zzichr( input((j+1):(j+1)) )
               value     = value*shft16
               output(i) = output(i)+value

               value     = zzichr( input(j:j) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
!
               osign = value / 128

!
!              Strip the sign bit if necessary.
!
               if ( osign == 1 ) then
                  value = value - 128
               end if

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with OUTPUT(I).
!
               value     = value*shft24
               output(i) = output(i)+value

!
!              Install the sign bit.  At the moment in OUTPUT(I)
!              we have the bits precisely as they need to be
!              arranged.  Perform the following computations:
!
!                 OUTPUT(I) = (2**31-1) - OUTPUT(I) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First, negate the result:
!
!                 OUTPUT(I) = -OUTPUT(I)
!
!              But this negation is effectively:
!
!                 OUTPUT(I) = 2**32 - OUTPUT(I)
!
!              Which yields:
!
!                 2**32 - (2**31) + OUTPUT(I)
!
!              or
!
!                 2**31 + OUTPUT(I)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               if ( osign == 1 ) then
                  if ( output(i) == 0 ) then
                     output(i) = smlint
                  else
                     output(i) = bigint-output(i)
                     output(i) = output(i) + 1
                     output(i) = -output(i)
                  end if
               end if

            end do

         else

            call ctx%setmsg ( 'Unable to translate integers from ' &
            //            'binary file format # to #.  This error ' &
            //            'should never occur and is indicative ' &
            //            'of a bug.  Contact NAIF.'                )
            call ctx%errch  ( '#', strbff(inbff)                        )
            call ctx%errch  ( '#', strbff(natbff)                       )
            call ctx%sigerr ( 'SPICE(BUG)'                              )
            return

         end if

!
!     The native binary file format on this platform is not supported
!     for the conversion of integers.  This is a bug, as this branch
!     of code should never be reached in normal operation.
!
      else

         call ctx%setmsg ( 'The native binary file format of this ' &
         //            'toolkit build, #, is not currently supported ' &
         //            'for translation of integers from non-native ' &
         //            'formats.'                                      )
         call ctx%errch  ( '#', strbff(natbff)                             )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

      return
      end associate
      end subroutine zzxlatei

      !> Identify VAX DAF file format.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NSUM       is a string whose first 8 bytes contain NSUM (the third
      !>                double precision number) from the first descriptor
      !>                record of a non-empty DAF suspected to be in one of
      !>                the VAX binary file formats.
      !>
      !>## Detailed Output
      !>
      !>     BFF        is an integer that signals whether NSUM indicates the
      !>                DAF is VAX-DFLT or VAX-GFLT.  Possible values are:
      !>
      !>                   VAXGFL
      !>                   VAXDFL
      !>
      !>                as defined in the include file 'zzddhman.inc'.  See it
      !>                for details.
      !>
      !>     FOUND      is a logical that indicates whether the ZZDDHVFF check
      !>                was successful.  If TRUE, BFF contains the code for
      !>                VAX-DFLT or VAX-GFLT binary file format.  If FALSE,
      !>                then BFF is untouched.
      !>
      !>## Parameters
      !>
      !>     See the include file 'zzddhman.inc'.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>## Files
      !>
      !>     This routine examines a series of bytes from a potential pre-N0050
      !>     DAF to determine its architecture, but does not access the file
      !>     itself.
      !>
      !>## Particulars
      !>
      !>     This routine examines the bit patterns stored in NSUM to determine
      !>     which of the two VAX binary file formats are used to store the
      !>     double precision values in the DAF file.
      !>
      !>## Examples
      !>
      !>     See ZZDDHPPF for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) The first 8 bytes of NSUM must contain the third double
      !>        precision value from the first descriptor record in a DAF
      !>        file not in BIG-IEEE binary file format.
      !>
      !>     2) The DAF file from which NSUM is extracted must be correct
      !>        or this routine may produce incorrect results.
      !>
      !>     3) Assumes CHARACTER*(1) is byte sized.
      !>
      !>     4) Assumes that ICHAR(CHAR(CHARACTER)) yields an integer with
      !>        the same bit pattern as the source character.
      !>
      !>## Literature References
      !>
      !>     1) Binary File Formats and Code Relying on Function Not Addressed
      !>        by the ANSI '77 Fortran Standard.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.26.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 1.25.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-INTEL.
      !>
      !>-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
      !>
      !>        Updated for PC-CYGWIN-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL.
      !>
      !>-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-CC_C.
      !>
      !>-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
      !>
      !>-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
      !>
      !>-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-WINDOWS-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for PC-64BIT-MS_C.
      !>
      !>-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-INTEL_C.
      !>
      !>-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-IFORT.
      !>
      !>-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
      !>
      !>        Updated for MAC-OSX-64BIT-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for PC-LINUX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
      !>
      !>        Updated for MAC-OSX-GFORTRAN.
      !>
      !>-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
      !>
      !>        Updated for PC-LINUX-IFORT.
      !>
      !>-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-LINUX-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-INTEL_C.
      !>
      !>-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for MAC-OSX-IFORT.
      !>
      !>-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
      !>
      !>        Updated for PC-WINDOWS-IFORT.
      !>
      !>-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
      !>
      !>        Updated for SUN-SOLARIS-64BIT-GCC_C.
      !>
      !>-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN_C.
      !>
      !>-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
      !>
      !>        Updated for PC-CYGWIN.
      !>
      !>-    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
      module subroutine zzddhivf ( ctx, nsum, bff, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: nsum  !! String storing the 8 bytes of the FDREC NSUM DP.
      integer(ip), intent(out) :: bff  !! Integer code indicating the binary file format.
      logical, intent(out) :: found  !! Logical indicating that BFF was determined.

      character(1) :: carg

      integer(ip) :: leader
      integer(ip) :: trailr

!
!     Statement Functions
!
      integer(ip) :: zzichr

!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
!     Some versions of the g77 implement ICHAR with a signed integer.
!     This function computes the value of ICHAR that this code requires
!     on any version of g77 for x86 Linux.
!     PEPPER: one portable form (ICHAR is 0:255 with current compilers; the
!     MODULO also maps a signed ICHAR, as SPICELIB's g77 branch did).
      zzichr(carg) = modulo ( ichar(carg), 256 )

!
!     Before diving right into the code that examines the bit patterns
!     stored in NSUM, review exactly what checks require completion and
!     why they function.
!
!     When this module is invoked, we already know that the DAF from
!     which NSUM was extracted is little endian, and that it is not
!     a LTL-IEEE file.  This leaves us with one of 3 sources for
!     NSUM:
!
!       (a) A VAX D-Floating file
!       (b) A VAX G-Floating file
!       (c) A damaged file
!
!     In the case of (c) the algorithm outlined below is not guaranteed
!     to produce correct results.  If the case is either (a) or (b),
!     then the routine will correctly determine the source binary file
!     format.  Here's why:
!
!        NSUM is the third double precision number from the first
!        descriptor record of a non-empty DAF file.  This number is
!        an integral valued DP bounded between 1 and 125 inclusive.
!
!        An examination of a binary file created with the following
!        code fragment:
!
!           INCLUDE              'zzddhman.inc'
!
!           DOUBLE PRECISION      DPDATA ( 125 )
!           INTEGER               I
!           INTEGER               LUN
!             .
!             .
!             .
!           CALL GETLUN( LUN )
!
!           DO I = 1, 125
!              DPDATA (I) = DBLE (I)
!           END DO
!
!           OPEN ( UNIT   = LUN,
!          .       FILE   = FNAME,
!          .       STATUS = 'NEW',
!          .       ACCESS = 'DIRECT',
!          .       RECL   = RECL      )
!
!           WRITE ( UNIT = LUN, REC = 1 ) ( DPDATA(I), I = 1, 125 )
!
!           END
!
!        This source file was compiled on a VMS VAX system both with
!        G-Floating and D-Floating options, and executed to produce
!        the binary file of interest.  The bit patterns for each of
!        the 125 entries were compared using the UNIX command 'od'.
!
!        This comparison yielded the fact that these two sets of 125
!        bit patterns did not intersect, and all that remained was to
!        uncover an efficient means of identifying which set a
!        particular member belonged to.
!
!        The following was observed:
!
!           With the exception of the first entry representing the
!           number 1.0D0 in the D-Floating case, all entries
!           appeared as: (hexadecimal byte dump from 'od' output)
!
!              0041 0000 0000 0000
!              4041 0000 0000 0000
!              8041 0000 0000 0000
!                       .
!                       .
!                       .
!              f643 0000 0000 0000
!              f843 0000 0000 0000
!              fa43 0000 0000 0000
!
!           While the G-Floating case:
!
!              1040 0000 0000 0000
!              2040 0000 0000 0000
!              2840 0000 0000 0000
!                       .
!                       .
!                       .
!              7e40 00c0 0000 0000
!              7f40 0000 0000 0000
!              7f40 0040 0000 0000
!
!           The important thing to note is that the fourth entry in
!           G-Floating bit patterns is always '0', and in the
!           D-Floating case (with the exception of the first entry)
!           is always non-zero.  The first entry in the D-Floating
!           table is:
!
!              8040 0000 0000 0000
!
!           It also happens to be the case that the leading value
!           of all G-Floating cases are numbers less than 8.
!           Constructing a series of tests around these observations
!           will produce correct results.  When the input file meets
!           the restrictions non-empty and correct.
!
!     So now all that remains is to lay out the specifics of the test.
!     First extract the leading 4 bits from NSUM(1:1) and the trailing
!     four bits from NSUM(2:2).  Then enter this IF/ELSE IF block:
!
!        If the value of the leading 4 bits from NSUM(1:1) is 8 and
!        the trailing 4 bits from NSUM(2:2) are 0, then the file is
!        of the D-Floating binary format.
!
!        Else if the value of the trailing 4 bits of NSUM(2:2) is
!        non-zero, then the file is also of the D-Floating binary
!        format.
!
!        Else if the value of the leading 4 bits of NSUM(1:1) is
!        strictly less than 8 and the trailing bits of NSUM(2:2)
!        are 0, then the file is of the G-Floating binary format.
!
!        Else the file is not of VAX type.
!
!     This routine could be reimplemented to examine all 8 bytes of
!     each double precision number and compare it to two tables of
!     values.  In the interest of simplicity the preceding option
!     was selected.
!
!     Convert the first and second characters in NSUM to integers.
!
      leader = zzichr( nsum(1:1) )
      trailr = zzichr( nsum(2:2) )

!
!     Shift the trailing 4 bits off LEADER.
!
      leader = leader / 16

!
!     Subtract the leading bits off TRAILR.
!
      trailr = trailr - ( 16 * (trailr / 16) )

!
!     Now determine what file we are looking at.
!
      if ( ( leader == 8 ) .and. ( trailr == 0 ) ) then

         found = .true.
         bff   = vaxdfl

      else if ( trailr /= 0 ) then

         found = .true.
         bff   = vaxdfl

      else if ( ( leader < 8 ) .and. ( trailr == 0 ) ) then

         found = .true.
         bff   = vaxgfl

      else

         found = .false.

      end if

      return
      end subroutine zzddhivf

      !> Convert filename to a handle.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FNAME      is the name of the file to locate in the file table.
      !>
      !>     FTABS,
      !>     FTAMH,
      !>     FTARC,
      !>     FTBFF,
      !>     FTHAN,
      !>     FTNAM,
      !>     FTRTM,
      !>     FTMNM      are the arrays respectively containing the absolute
      !>                value of the handle, access method, architecture,
      !>                binary file format, handle, name, RTRIM and
      !>                magic number columns of the file table.
      !>
      !>     NFT        is the number of entries in the file table.
      !>
      !>     PEPPER: the file table is FT (the records of zzddhman_state, with
      !>     NFT entries) instead of the arrays above.
      !>
      !>     UTCST,
      !>     UTHAN,
      !>     UTLCK,
      !>     UTLUN      are the arrays respectively containing the cost,
      !>                handle, locked, and logical unit columns of the unit
      !>                table.
      !>
      !>     NUT        is the number of entries in the unit table.
      !>
      !>## Detailed Output
      !>
      !>     UTCST,
      !>     UTHAN,
      !>     UTLCK,
      !>     UTLUN      are the arrays respectively containing the cost,
      !>                handle, locked, and logical unit columns of the unit
      !>                table.  If ZZDDHF2H requires a logical unit, then
      !>                it will borrow one from the unit table.  Depending
      !>                on the state of the table passed in from the caller
      !>                one of three possible scenarios may occur (Recall
      !>                that 'zero-cost' rows are ones whose units are
      !>                reserved with RESLUN and not currently connected
      !>                to any file.)
      !>
      !>                   A 'zero-cost' row exists in the table, in
      !>                   which case the row is used temporarily and
      !>                   may be removed depending on the number of entries
      !>                   in the file table (NFT).
      !>
      !>                   The unit table is full (NUT=UTSIZE), in which
      !>                   case the unit with the lowest cost that is not
      !>                   locked to its handle will be disconnected, used,
      !>                   and then returned to the table as a 'zero-cost'
      !>                   row before returning to the caller.
      !>
      !>                   The unit table is not full (NUT<UTSIZE) and there
      !>                   are no 'zero-cost' rows.  In this case NUT is
      !>                   temporarily increased by one, and the new row
      !>                   is used.  After this routine no longer requires
      !>                   the unit, depending on the number of entries in
      !>                   the file table (NFT) the row may be left in the
      !>                   table as a 'zero-handle' row or removed entirely.
      !>
      !>                In the event an error is signaled, the contents of the
      !>                unit table are placed into a usable state before
      !>                returning to the caller.
      !>
      !>     NUT        is the number of entries in the unit table. Since
      !>                this routine borrows a unit from the unit table, which
      !>                may involve allocation of a new unit, this value may
      !>                change.
      !>
      !>     EXISTS     is a logical if set to TRUE, indicates that FNAME
      !>                exists.  If FALSE, FNAME does not exist.  In the event
      !>                an exception is signaled the value is undefined.
      !>
      !>     OPENED     is a logical if set to TRUE, indicates that FNAME
      !>                is opened and attached to a logical unit.  If FALSE,
      !>                FNAME is not attached to a unit.  In the event an
      !>                exception is signaled the value is undefined.
      !>
      !>     HANDLE     is the handle in the file table associated with
      !>                FNAME.  If FOUND is FALSE, then HANDLE is returned as
      !>                0.
      !>
      !>     FOUND      is a logical if TRUE indicates that FNAME was found
      !>                in the file table.  If FALSE indicates that it was not
      !>                located.
      !>
      !>     MNM        is a unique (enough) DP number -- the Magic NuMber --
      !>                associated with FNAME computed by this examining the
      !>                file contents.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If any of the INQUIRE statements this routine performs fail,
      !>        the error SPICE(INQUIREFAILED) is signaled. FOUND is set to
      !>        FALSE and HANDLE to 0.
      !>
      !>     2) If the attempt to open FNAME fails, then SPICE(FILEOPENFAILED)
      !>        is signaled. FOUND is set to FALSE, and HANDLE to 0.
      !>
      !>     3) If FNAME is determined not to be loaded into the file table
      !>        then FOUND is set to FALSE and HANDLE is set to 0.
      !>
      !>## Files
      !>
      !>     If the file named by FNAME is not connected to a logical unit,
      !>     this routine will open it for direct access to complete its
      !>     examination.
      !>
      !>## Particulars
      !>
      !>     This routine encapsulates the logic necessary to determine if
      !>     a particular filename names a file already loaded into the
      !>     DAF/DAS handle manager.  If it discovers the file is loaded,
      !>     the routine returns the handle to the caller.
      !>
      !>## Examples
      !>
      !>     See ZZDDHFNH for sample usage.
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
      !>     F.S. Turner     (JPL)
      !>     E.D. Wright     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    SPICELIB Version 3.0.0, 26-APR-2012 (BVS)
      !>
      !>        Changed calling sequence to include FTMNM and MNM. Change
      !>        algorithm to compute MNM and use it to bypass n^2 INQUIREs
      !>        for files opened for READ access, if possible.
      !>
      !>-    SPICELIB Version 2.0.1, 24-APR-2003 (EDW)
      !>
      !>        Added MAC-OSX-F77 to the list of platforms
      !>        that require READONLY to read write protected
      !>        kernels.
      !>
      !>-    SPICELIB Version 2.0.0, 05-AUG-2002 (FST)
      !>
      !>        Bug fix: this module was updated to allow proper loading
      !>        of read-only files on VAX environments.
      !>
      !>-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 05-AUG-2002 (FST)
      !>
      !>        An OPEN statement that is exercised by this module under
      !>        certain circumstances, failed to pass the non-standard
      !>        READONLY option for the VAX environments.  This had the
      !>        undesirable side-effect of not permitting files available
      !>        only for READ access to be opened.
      !>
      !>        This file was promoted from a standard portable module
      !>        to a master file.
      module subroutine zzddhf2h ( ctx, fname, ft, utcst, uthan, utlck, utlun, nut, exists, opened, handle, found, mnm, tabsiz )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: rtrim
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of the file to convert to a handle.
      type(zzddhman_state), intent(inout) :: ft  !! File table. (PEPPER: inout, for the result kept during a load)
      integer(ip), intent(inout) :: utcst ( * )  !! Unit table.
      integer(ip), intent(inout) :: uthan ( * )  !! Unit table.
      logical, intent(inout) :: utlck ( * )  !! Unit table.
      integer(ip), intent(inout) :: utlun ( * )  !! Unit table.
      integer(ip), intent(inout) :: nut  !! Number of entries in the unit table.
      logical, intent(out) :: exists  !! Logical indicating if FNAME exists.
      logical, intent(out) :: opened  !! Logical indicating if FNAME is opened.
      integer(ip), intent(out) :: handle  !! Handle associated with FNAME.
      logical, intent(out) :: found  !! Logical indicating if FNAME's HANDLE was found.
      real(wp), intent(out) :: mnm  !! Unique DP (Magic NuMber) associated with FNAME.
      integer(ip), intent(in), optional :: tabsiz  !! PEPPER: size of the unit table (default UTSIZE, 23)

      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: rchar
      integer(ip) :: uindex
      integer(ip) :: unit

      logical :: locexs
      logical :: locopn

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     First check to see if FNAME is blank.  If so, set FOUND to .FALSE.
!     and return.  ZZDDHOPN prevents any blank filenames from being
!     loaded into the file table.
!
      if ( fname == ' ' ) then

         found  = .false.
         handle = 0
         opened = .false.
         exists = .false.
         return

      end if

!
!     Start by trimming the file name in preparation for the INQUIRE.
!
      rchar = rtrim ( fname )

!
!     Now INQUIRE on the input file FNAME.
!
      inquire ( file   = fname( 1:rchar ), &
                exist  = locexs, &
                opened = locopn, &
                number = unit, &
                iostat = iostat            )

!
!     Check IOSTAT for failure.
!
      if ( iostat /= 0 ) then
         found  = .false.
         handle = 0
         call ctx%setmsg ( 'INQUIRE failed. Value of IOSTAT was #.' )
         call ctx%errint ( '#', iostat                              )
         call ctx%sigerr ( 'SPICE(INQUIREFAILED)'                   )
         return
      end if

!
!     First, set some of the output arguments.  Remember, some
!     systems consider non-existent files as open.  Compensate for
!     this unusual behavior.
!
      exists = locexs
      opened = locopn .and. exists

!
!     Now check to see if the file exists.  If it does not, then
!     set FOUND to false and HANDLE to 0 as non-existent files
!     can not possibly be present in the file table.
!
      if ( .not. exists ) then

         found  = .false.
         handle = 0
         return

      end if

!
!     Now check to see if the file is opened.  If it is, we need to
!     determine whether or not the logical unit to which it is
!     attached is present in the unit table.
!
      if ( opened ) then

!
!        Since the file is opened, see if we can find its unit
!        in the unit table.
!
         uindex = findloc(utlun(1:nut), unit, dim=1)

!
!        When UINDEX is 0, the file is opened, but not by
!        the DAF/DAS handle manager.  Set FOUND to FALSE, HANDLE
!        to 0, and return to the caller.
!
         if ( uindex == 0 ) then
            handle = 0
            found  = .false.
            return
         end if

!
!        If we end up here, then we found UNIT in the unit table.
!        Set FOUND to TRUE if the handle associated with UNIT is
!        non-zero.
!
         handle = uthan(uindex)
         found  = handle /= 0
         return

      end if

!
!     At this point, we took action for all simple cases.  Now
!     we need to find out if FNAME is one of the files in the
!     file table that isn't open.  To determine this, we open FNAME,
!     and then INQUIRE on every file in the table.  To do this, we
!     need a unit. Get one.
!
!     PEPPER: during a load, the result for this file may have been kept
!     by the previous call (see zzddhman_state): it is not in the table.
!
      if ( ft%kept_f2h ( fname(1:rchar) ) ) then
         handle = 0
         found  = .false.
         mnm    = ft%f2h_mnm
         return
      end if

      call zzddhgtu ( ctx, utcst, uthan, utlck, utlun, nut, uindex, tabsiz )

      if ( ctx%failed() ) then
         handle = 0
         found  = .false.
         return
      end if

!
!     Now open the file (which we know exists and isn't open). Since
!     we effectively are just borrowing this unit, we are not going to
!     set UTHAN or UTCST from the defaults that ZZDDHGTU sets up.
!
      open ( newunit = utlun(uindex), &
             file   = fname( 1:rchar ), &
             access = 'DIRECT', &
             recl   = record_length(), &
             status = 'OLD', &
             iostat = iostat            )

!
!     Check IOSTAT.
!
      if ( iostat /= 0 ) then

!
!        Since an error has occurred, set FOUND to false and HANDLE
!        to 0.
!
         found  = .false.
         handle = 0

!
!        Close the unit and remove it from the unit table.
!
         if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex) )

         call zzddhrmu ( ctx, uindex, ft%nft, utcst, uthan, utlck, utlun, nut )

!
!        Signal the error and return.
!
         call ctx%setmsg ( 'Attempt to open file ''#'' failed. Value of ' &
         //            'IOSTAT was #.'                                )
         call ctx%errch  ( '#', fname                                     )
         call ctx%errint ( '#', iostat                                    )
         call ctx%sigerr ( 'SPICE(FILEOPENFAILED)'                        )
         return

      end if

!
!     Get a unique enough DP number -- the Magic NuMber (MNM) ;) -- for
!     this file.
!
      mnm = zzddhmnm ( ctx, utlun(uindex) )
!
!     PEPPER: keep the start of the first record for GETFAT (see below).
!
      if ( ft%reuse_f2h ) then
         read ( unit=utlun(uindex), rec=1, iostat=iostat ) ft%f2h_word
         ft%f2h_word_ok = iostat == 0
      end if

!
!     Now loop through all the files in the file table. Unfortunately
!     we have no other choice.
!
      i     = 1
      found = .false.

      do while ( ( i <= ft%nft ) .and. ( .not. found ) )

!
!        If this file's magic number is non-zero and is different from
!        the magic number of the currently checked, opened-for-READ
!        file, we will declare that these files are not the same file
!        and will skip INQUIRE. In all other cases we will do INQUIRE
!        and check UNITs.
!
!        PEPPER: a DAF built in memory has no file: it is not this one.
!
         if ( ft%files(i)%synthetic .or. &
              ( mnm /= 0.0_wp                               .and. &
              ( mnm /= ft%files(i)%mnm .and. ft%files(i)%method == read ) ) ) then

!
!           These files are not the same file. Clear IOSTAT and set
!           UNIT to not match the UNIT of the input file.
!
            iostat = 0
            unit   = utlun(uindex) + 1

         else

!
!           Do the INQUIRE. ;(
!
            inquire ( file   = ft%files(i)%name, &
                      exist  = locexs, &
                      opened = locopn, &
                      number = unit, &
                      iostat = iostat               )

         end if

!
!        Check IOSTAT.
!
         if ( iostat /= 0 ) then

!
!           Since we have an error condition, set FOUND to FALSE
!           and HANDLE to 0.
!
            found  = .false.
            handle = 0

!
!           Close the unit and clean up the unit table.
!
            if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex) )

            call zzddhrmu ( ctx, uindex, ft%nft,   utcst, uthan, &
                            utlck,  utlun, nut           )

!
!           Signal the error and return.
!
            call ctx%setmsg ( 'INQUIRE failed. Value of IOSTAT was #.' )
            call ctx%errint ( '#', iostat                              )
            call ctx%sigerr ( 'SPICE(INQUIREFAILED)'                   )
            return

         end if

!
!        Now check to see if FILE exists, is currently open. and
!        its UNIT matches UTLUN(UINDEX).
!
         if (       ( locexs .and. locopn ) &
              .and. (   unit ==  utlun(uindex) ) ) then

               handle = ft%files(i)%handle
               found  = .true.

!
!        Otherwise, continue searching.
!
         else
            i = i + 1
         end if

      end do

!
!     Check to see if we found the file in the file table.
!
      if ( .not. found ) then
         handle = 0
!
!        PEPPER: keep the result during a load (see zzddhman_state).
!
         if ( ft%reuse_f2h ) then
            ft%f2h_name    = fname(1:rchar)
            ft%f2h_changes = ft%changes
            ft%f2h_mnm     = mnm
         end if
      end if

!
!     Close the unit and clean up the unit table.
!
      if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex) )

      call zzddhrmu ( ctx, uindex, ft%nft, utcst, uthan, utlck,  utlun, nut )

      return

      end subroutine zzddhf2h

      !> Manage augmentation of the handle to logical unit request counter
      !> and the cost column in the unit table.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NUT        is the number of entries in the unit table.
      !>
      !>     UTCST      is the current cost column of the unit table.
      !>
      !>     REQCNT     is the current value of the HLU request counter to
      !>                adjust.
      !>
      !>## Detailed Output
      !>
      !>     UTCST      is the updated cost column of the unit table.  In
      !>                the nominal case, UTCST will not be adjusted, but
      !>                if REQCNT overflows, then adjustments will be made
      !>                to approximately preserve the priority.
      !>
      !>     REQCNT     is the updated value of the request counter.
      !>                Nominally this will be 1 more than the input
      !>                value.  However, in the case where REQCNT will
      !>                exceed INTMAX it will be assigned to INTMAX()/2 + 1.
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
      !>     1) If REQCNT on input is INTMAX(), then REQCNT on output will
      !>        be assigned INTMAX()/2 + 1 and the UTCST column will be
      !>        recomputed.
      !>
      !>## Particulars
      !>
      !>     This module manages the request counter and the cost column of
      !>     the unit table which is used to determine the expense of
      !>     disconnecting a handle from its logical unit.
      !>
      !>     In the nominal mode of operation, the request counter is simply
      !>     incremented by one and the cost column remains untouched.
      !>     However, when the request counter passed into the routine is
      !>     INTMAX, then REQCNT is not incremented.  In an attempt to preserve
      !>     the relationships between costs, all entries in the cost column
      !>     are halved and REQCNT is set to INTMAX()/2 + 1.  This has the
      !>     effect of preserving the cost relationships between rows, except
      !>     in half the cases where subsequent cost values are present.
      !>
      !>     The occurrence of rollover is rare, and thus the destruction of
      !>     relative cost relationships as well.
      !>
      !>## Examples
      !>
      !>     See ZZDDHHLU for sample usage.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
      module subroutine zzddhrcm ( ctx, nut, utcst, reqcnt )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nut  !! Number of entries in the unit table.
      integer(ip), intent(inout) :: utcst  ( * )  !! Cost column of the unit table.
      integer(ip), intent(inout) :: reqcnt  !! Value of the HLU request counter.

      integer(ip) :: i

!
!     Check to see if REQCNT is INTMAX, otherwise just increment
!     REQCNT.
!
      if ( reqcnt == intmax ) then

         reqcnt = intmax/2 + 1

         do i = 1,nut
            utcst(i) = max(1, utcst(i)/2 )
         end do

      else

         reqcnt = reqcnt + 1

      end if

      return
      end subroutine zzddhrcm

      !> Get or prepare an entry in the unit table to receive a new
      !> file.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     UTCST,
      !>     UTHAN,
      !>     UTLCK,
      !>     UTLUN,     are the arrays respectively containing the cost,
      !>                handle, locked, and logical unit columns of the
      !>                unit table.
      !>
      !>     NUT        is the number of entries in the unit table.
      !>
      !>## Detailed Output
      !>
      !>     UTCST,
      !>     UTHAN,
      !>     UTLCK,
      !>     UTLUN,     are the arrays respectively containing the cost,
      !>                handle, locked, and logical unit columns of the
      !>                unit table.  This may change as a new unit is
      !>                added or old ones are removed.
      !>
      !>     NUT        is the number of entries in the unit table.  This may
      !>                change as new entries are added.
      !>
      !>     UINDEX     is the index of the row where the new unit should
      !>                be attached.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     This routine may disconnect a file from its logical unit, to
      !>     successfully process the caller's request for a unit.
      !>
      !>## Exceptions
      !>
      !>     1) If GETLUN fails to assign a logical unit for any reason to
      !>        the row of interest, this routine sets the logical unit to -1,
      !>        since negative logical units in Fortran are not permitted.
      !>
      !>## Particulars
      !>
      !>      This routine only manipulates the contents of the unit table.
      !>      Any "zero" cost rows in the table indicate rows where the
      !>      listed logical unit has been reserved, but no file is currently
      !>      attached.
      !>
      !>      Callers of this routine should check FAILED since this
      !>      routine may invoke GETLUN.
      !>
      !>## Examples
      !>
      !>     See ZZDDHHLU for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) This routine must not be used to retrieve a unit for a
      !>        file that is already connected to a unit listed in the
      !>        unit table.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 29-MAY-2001 (FST)
      module subroutine zzddhgtu ( ctx, utcst, uthan, utlck, utlun, nut, uindex, tabsiz )
      use pepper_kinds, only: ip
      use pepper_arrays, only: orderi
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: utcst  ( * )  !! Unit table.
      integer(ip), intent(inout) :: uthan  ( * )  !! Unit table.
      logical, intent(inout) :: utlck  ( * )  !! Unit table.
      integer(ip), intent(inout) :: utlun  ( * )  !! Unit table.
      integer(ip), intent(inout) :: nut  !! Number of entries in the unit table.
      integer(ip), intent(out) :: uindex  !! Row in the unit table that can be replaced.
      integer(ip), intent(in), optional :: tabsiz  !! PEPPER: size of the unit table (default UTSIZE, 23)

      integer(ip) :: i
      integer(ip), allocatable :: orderv ( : )

      logical :: done

!
!     Standard SPICE discovery error handling.
!
      if ( ctx%failed() ) return

!
!     First check the case when the unit table is completely empty.
!
      if ( nut == 0 ) then

         nut    = 1
         uindex = 1

         utcst(uindex) = 0
         uthan(uindex) = 0
         utlck(uindex) = .false.

!        PEPPER: no unit yet; the file is opened with NEWUNIT=.
         utlun(uindex) = -1

!
!        Check FAILED to see if GETLUN signaled an error.  If so, then
!        return an invalid unit to the caller.
!
         if ( ctx%failed() ) then
            utlun(uindex) = -1
            return
         end if

!
!        If we end up here, then GETLUN succeeded and we have the new
!        unit.  Now return.
!
         return

      end if

!
!     If we reach here, then the table contains at least one entry.
!     Order the table rows by cost.
!
      allocate ( orderv ( max ( nut, 1_ip ) ) )
      call orderi ( utcst, nut, orderv )

!
!     Now check to for '0' cost rows as this indicates rows whose
!     logical units are reserved for this suite of routines usage,
!     but are not currently assigned a file.
!
      if ( utcst(orderv(1)) <= 0 ) then

         uindex = orderv(1)

!
!        '0' cost rows end up in the unit table as the result of a
!        row deletion, occurring when excess files are present.
!        When this process occurs, the logical unit listed in this
!        row is reserved for this module's usage only with RESLUN.
!        Free it, since we're about to reassign it.
!
!        PEPPER: units come from NEWUNIT=; nothing to free.

         return

      end if

!
!     Now if no '0' cost rows exist, check to see if we can
!     expand the table.
!
      if ( nut < size_of_table ( ) ) then

!
!        Now increment NUT and set UINDEX.
!
         nut    = nut + 1
         uindex = nut

!
!        Prepare the default values for the new row.
!
         utcst(uindex) = 0
         uthan(uindex) = 0
         utlck(uindex) = .false.

!        PEPPER: no unit yet; the file is opened with NEWUNIT=.
         utlun(uindex) = -1

!
!        Check FAILED to see if GETLUN signaled an error.  If so, then
!        return an invalid unit to the caller.
!
         if ( ctx%failed() ) then
            utlun(uindex) = -1
            return
         end if

!
!        If we end up here, then GETLUN worked properly.  Now return.
!
         return

      end if

!
!     If we reach here, then we have no zero-cost rows and a full unit
!     table.  Now it's time to determine which entry in the table to
!     bump.  We do this by stepping through the order vector until
!     we find the first 'non-locked' row.
!
      i    = 0
      done = .false.

      do while ( (.not. done) .and. (i /= nut) )

         i    = i + 1
         done = .not. utlck( orderv(i) )

      end do

!
!     Before going any further, signal an error if we discover
!     we have not found a row.
!
      if ( .not. done ) then

         uindex = 0
         call ctx%setmsg ( 'The unit table is full and all entries are ' &
         //            'locked.  This should never happen. Contact ' &
         //            'NAIF.'                                       )
         call ctx%sigerr ( 'SPICE(BUG)'                                  )
         return

      end if

!
!     Clear UTCST and UTHAN since we intend to disconnect
!     the unit upon return.
!
      utcst( orderv(i) ) = 0
      uthan( orderv(i) ) = 0

!
!     Set UINDEX and CLSLUN, then return.
!
      uindex = orderv(i)

!
!     At this point we need to close the unit from the row of interest.
!
      if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex) )

      return
      contains
         !> The size of the unit table: TABSIZ, or UTSIZE.
         pure integer(ip) function size_of_table ( ) result ( n )
            n = utsize
            if ( present ( tabsiz ) ) n = tabsiz
         end function size_of_table
      end subroutine zzddhgtu

      !> Remove an entry from the unit table.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the index of the row in the unit table for the
      !>                unit to remove.
      !>
      !>     NFT        is the number of entries in the file table after
      !>                the file whose unit is about to be disconnected
      !>                has been removed.
      !>
      !>     UTCST,
      !>     UTHAN,
      !>     UTLCK,
      !>     UTLUN,     are the cost, handle, locked, and logical unit columns
      !>                of the unit table respectively.
      !>
      !>     NUT        is the number of entries in the unit table.
      !>
      !>## Detailed Output
      !>
      !>     UTCST,
      !>     UTHAN,
      !>     UTLCK,
      !>     UTLUN,     are the cost, handle, locked, and logical unit columns
      !>                of the unit table respectively.  The contents will
      !>                change, for specifics see the Particulars section
      !>                below.
      !>
      !>     NUT        is the number of entries in the unit table.  Depending
      !>                on the state of the file table, this may or may not
      !>                change.  See the $Particulars section below for
      !>                details.
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
      !>     1) SPICE(INDEXOUTOFRANGE) is signaled when the input UINDEX is
      !>        either greater than NUT or less than 1.
      !>
      !>     2) If NUT is 0 on input, then this module simply returns.
      !>
      !>## Particulars
      !>
      !>     This routine only manipulates the contents of the unit table.
      !>     It is utilized to delete an entry in the unit table that is
      !>     the result of a file 'unload' or close operation.
      !>
      !>     If the number of files listed in the file table exceeds the
      !>     number of entries in the unit table, then this module will
      !>     reserve the logical unit listed in the row to remove, zero
      !>     out the cost and return.  In this event, NUT will remain
      !>     unchanged.
      !>
      !>     However, if there are less files in the file table than the
      !>     number of entries in the unit table, then this routine removes
      !>     the row and compresses the unit table, as one would expect.
      !>
      !>     The file attached to UNIT is not closed by this routine, the
      !>     closure should occur before invoking this module.
      !>
      !>## Examples
      !>
      !>     See ZZDDHHLU for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) This routine operates when an error condition introduced
      !>        by a prior call to SIGERR exists.  It calls no routines
      !>        that return on entry when proper inputs are provided.
      !>        Any updates to this routine must preserve this behavior.
      !>
      !>     2) The file attached to the unit that is to be removed should
      !>        already have been removed from the file table.  This is
      !>        necessary so the value of NFT reflects the number of files
      !>        available after the removal.
      !>
      !>     3) The logical unit in UTLUN(UINDEX) must be closed or buffered
      !>        externally prior to calling this routine.  Knowledge of its
      !>        value could be lost otherwise.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 05-NOV-2001 (FST)
      module subroutine zzddhrmu ( ctx, uindex, nft, utcst, uthan, utlck, utlun, nut )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: uindex  !! Row index to remove from the unit table.
      integer(ip), intent(in) :: nft  !! Number of entries in the file table.
      integer(ip), intent(inout) :: utcst ( * )  !! Unit table.
      integer(ip), intent(inout) :: uthan ( * )  !! Unit table.
      logical, intent(inout) :: utlck ( * )  !! Unit table.
      integer(ip), intent(inout) :: utlun ( * )  !! Unit table.
      integer(ip), intent(inout) :: nut  !! Number of entries in the unit table.

      integer(ip) :: i

!
!     First check to see if NUT is 0.  If so, just return, as there
!     are no rows to remove.
!
      if ( nut == 0 ) return

!
!     Check to see if we found the UINDEX in the unit table.
!     If not,, signal an error and return.
!
      if ( ( uindex > nut ) .or. ( uindex < 1 ) ) then

         call ctx%setmsg ( 'Attempt to remove row # from the unit table ' &
         //            'failed because valid row indices range from ' &
         //            '1 to NUT.'                                    )
         call ctx%errint ( '#', uindex                                    )
         call ctx%errint ( '#', nut                                       )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                       )
         return

      end if

!
!     We have found the row we need to remove from the table.
!     Check to see whether we are to remove this row or simply
!     mark it as zero cost and reserve the unit.  We know this
!     is the case when NFT is greater than or equal to NUT.
!
      if ( nft >= nut ) then

!
!        Zero the cost, clear the handle, and unlock the unit.
!
         utcst(uindex) = 0
         uthan(uindex) = 0
         utlck(uindex) = .false.

!
!        Reserve the unit for the handle manager's usage and
!        return.
!
!        PEPPER: units come from NEWUNIT=; nothing to reserve.
         return

      end if

!
!     If we reach here, then we have to remove the row from the
!     unit table and compress.
!
      do i = uindex+1, nut

         utcst(i-1) = utcst(i)
         uthan(i-1) = uthan(i)
         utlck(i-1) = utlck(i)
         utlun(i-1) = utlun(i)

      end do

!
!     Decrement NUT.
!
      nut = nut - 1

      return
      end subroutine zzddhrmu

      !> This function return the number of locked units in the unit table.
      !>
      !> Retrieve the number of locked units in the unit table.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     UTLCK      is the lock column of the unit table.  TRUE entries in
      !>                this column indicate a handle is locked to a particular
      !>                unit.
      !>
      !>     NUT        is the number of entries in the unit table.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of locked units in the unit table.
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
      !>     This routine simply encapsulates some logic used in several
      !>     places in ZZDDHMAN.
      !>
      !>## Examples
      !>
      !>     See ZZDDHMAN for sample usage.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
      module integer(ip) function zzddhclu ( ctx, utlck, nut )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: utlck ( * )  !! Lock column of the unit table.
      integer(ip), intent(in) :: nut  !! Number of entries in the unit table.

      integer(ip) :: i

!
!     Loop through UTLCK counting the number of TRUE values.
!
      zzddhclu = 0

      do i = 1, nut

         if ( utlck(i) ) then
            zzddhclu = zzddhclu + 1
         end if

      end do

      return
      end function zzddhclu

      !> Return the integer code for the native binary file format
      !> of the host system.
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
      !>     NATBFF         is the native binary file format code for the host
      !>                    system on which this routine is called.
      !>
      !>## Parameters
      !>
      !>     See the INCLUDE file zzddhman.inc for a description of
      !>     binary file format codes.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs while attempting to obtain the host
      !>         binary file format code, the error will be diagnosed by a
      !>         routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Routines outside of SPICELIB will normally have no need to call
      !>     this routine.
      !>
      !>     This routine centralizes the logic needed to obtain the host
      !>     system's binary file format code.
      !>
      !>## Examples
      !>
      !>     See usage in DASGRD.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB)
      module subroutine zzddhnfc ( ctx, natbff )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: natbff  !! Native binary file format code.
      character(strsiz) :: tmpstr
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzddhnfc ) ) allocate ( ctx%zzddhnfc )
      associate ( first => ctx%zzddhnfc%first, &
                  savbff => ctx%zzddhnfc%savbff, &
                  strbff => ctx%zzddhnfc%strbff )
      if ( ctx%failed() ) return

      if ( first ) then

!
!        Populate STRBFF, the buffer that contains the labels
!        for each binary file format.
!
         do i = 1, numbff
            call zzddhgsd ( ctx, 'BFF', i, strbff(i) )
         end do

!
!        Fetch the native binary file format and determine its
!        integer code.
!
         call zzplatfm ( ctx, 'FILE_FORMAT', tmpstr )
         call ucase_inplace ( tmpstr )

         savbff = findloc(strbff, tmpstr, dim=1)

         if ( savbff == 0 ) then

            call ctx%setmsg ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            call ctx%errch  ( '#', tmpstr                                  )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

!
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

      natbff = savbff

      return
      end associate
      end subroutine zzddhnfc

      !> Load a new direct access file.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FNAME      is the file name of the file to be loaded for direct
      !>                access.
      !>
      !>     METHOD     is the method by which to load the file. Acceptable
      !>                values are:
      !>
      !>                   'READ'    - Load existing file for read access.
      !>                   'WRITE'   - Load existing file for write access.
      !>                   'SCRATCH' - Load scratch file.
      !>                   'NEW'     - Load a new file for write access.
      !>
      !>                Note: The value of METHOD is case-insensitive.
      !>
      !>     ARCH       is the architecture of the file to be loaded.
      !>                Acceptable values are:
      !>
      !>                   'DAF'    - Load a DAF file
      !>                   'DAS'    - Load a DAS file
      !>
      !>                Note: The value of ARCH is case-insensitive.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE     is the file handle associated with the file.  This
      !>                handle is used to identify the file in subsequent
      !>                calls to other routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) The error SPICE(UNSUPPORTEDMETHOD) is signaled when the
      !>        METHOD input argument is improperly specified.  The value of
      !>        the output argument HANDLE is undefined when this error is
      !>        signaled.
      !>
      !>     2) The error SPICE(UNSUPPORTEDARCH) is signaled when the ARCH
      !>        input argument is improperly specified.  The value of the
      !>        output argument HANDLE is undefined when this error is
      !>        signaled.
      !>
      !>     3) The error SPICE(UTFULL) is signaled whenever METHOD is
      !>        set to 'SCRATCH' and no available units exist in the
      !>        unit table for locking.  The value of the output argument
      !>        HANDLE is undefined when this error is signaled.
      !>
      !>     4) The error SPICE(BLANKFILENAME) is signaled whenever METHOD
      !>        is set to 'READ', 'WRITE', or 'NEW' and the FNAME argument
      !>        is a blank string.  The value of the output argument HANDLE
      !>        is undefined when this error is signaled.
      !>
      !>     5) The error SPICE(FILENOTFOUND) is signaled whenever METHOD
      !>        is set to 'READ' or 'WRITE' and an INQUIRE performed on FNAME
      !>        indicates the file does not exist.  The value of the output
      !>        argument HANDLE is undefined when this error is signaled.
      !>
      !>     6) The error SPICE(IMPROPEROPEN) is signaled if the file
      !>        associated with FNAME is attached to a unit from some
      !>        source external to ZZDDHMAN's entry points.  The value of the
      !>        output argument HANDLE is undefined when this error is
      !>        signaled.
      !>
      !>     7) The error SPICE(FILARCMISMATCH) is signaled when a file is
      !>        loaded for 'READ' or 'WRITE' and the architecture of the
      !>        existing file disagrees with that of the input argument ARCH.
      !>        The value of the output argument HANDLE is undefined when
      !>        this error is signaled.
      !>
      !>     8) The error SPICE(FILEOPENCONFLICT) is signaled when an attempt
      !>        to load an already loaded file for any access other than READ.
      !>        The value of the output argument HANDLE is undefined when this
      !>        error is signaled.
      !>
      !>     9) The error SPICE(RWCONFLICT) is signaled when an attempt to
      !>        load a file for READ access that is already loaded into the
      !>        handle manager with a conflicting access method.  The value of
      !>        the output argument HANDLE is undefined when this error is
      !>        signaled.
      !>
      !>    10) The error SPICE(FTFULL) is signaled when an attempt to load
      !>        more than the maximum number of allowable files, FTSIZE,
      !>        is made.  The value of the output argument HANDLE is undefined
      !>        when this error is signaled.
      !>
      !>    11) The error SPICE(FILEOPENFAIL) is signaled whenever the
      !>        the file open fails with non-zero IOSTAT.  The value of the
      !>        output argument HANDLE is undefined when this error is
      !>        signaled.
      !>
      !>    12) The error SPICE(UNSUPPORTEDBFF) is signaled whenever the file
      !>        to be opened utilizes a binary file format that the platform
      !>        does not currently support.  The value of the output argument
      !>        HANDLE is undefined when this error is signaled.
      !>
      !>    13) When loading files with METHOD set to 'NEW', any errors
      !>        generated by this routine will cause the newly created file
      !>        to be deleted.
      !>
      !>    14) If the toolkit source is improperly configured for the
      !>        runtime environment, routines in the call tree of this
      !>        routine may signal errors.
      !>
      !>## Files
      !>
      !>     1) All direct access files loaded by this routine for
      !>        access methods other than 'SCRATCH' are specified by name.
      !>
      !>     2) Files opened with access method 'SCRATCH' are referenced
      !>        only by their logical unit.
      !>
      !>## Particulars
      !>
      !>     This private routine is designed to provide a common, unified
      !>     file load interface for DAF and DAS.
      !>
      !>## Examples
      !>
      !>     See DAFOPR, DAFONW, DAFOPW, DASOPR, DASOPS, DASONW, DASOPW for
      !>     sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) Files loaded through this interface should not be opened by
      !>        any other mechanism until the appropriate call to ZZDDHCLS
      !>        is made.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
      !>
      !>        Updated for the new "magic number" column in the file table.
      !>
      !>-    SPICELIB Version 2.0.0, 07-AUG-2002 (FST)
      !>
      !>        This entry point was updated to perform checks on the
      !>        runtime environment, to verify that the source is properly
      !>        configured for execution on this environment.  See the
      !>        Revisions section of ZZDDHMAN for details.
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhopn ( ctx, fname, method, arch, handle )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchi
      use pepper_strings, only: rtrim, ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: arch  !! Expected architecture of the file to load.
      character(*), intent(in) :: fname  !! Name of file to be loaded.
      character(*), intent(in) :: method  !! Access method used to load the file.
      integer(ip), intent(out) :: handle  !! Handle assigned to file.
      integer(ip), parameter :: scrlen = 4096  !! PEPPER: room for the OS's name of a scratch file
      character(:), allocatable :: locfnm
      character(scrlen) :: scrnam
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  next => ctx%spice%zzddhman%next, &
                  first => ctx%zzddhman_units%first, &
                  opnfst => ctx%zzddhman_units%opnfst, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup, &
                  nut => ctx%zzddhman_units%nut, &
                  utcst => ctx%zzddhman_units%utcst, &
                  uthan => ctx%zzddhman_units%uthan, &
                  utlck => ctx%zzddhman_units%utlck, &
                  utlun => ctx%zzddhman_units%utlun, &
                  reqcnt => ctx%zzddhman_units%reqcnt )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() to handle the unlikely event that
!        ZZDDHINI signaled SPICE(BUG).
!
         if ( ctx%failed() ) return

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     On first pass, perform any runtime environment checks.
!
      if ( opnfst ) then

         call zzpltchk ( ctx, platok )

         if ( ctx%failed() ) return

!
!        Clear OPNFST, since we've performed the diagnostics.
!
         opnfst = .false.

      end if

!
!     Initialize the value of HANDLE to 0.  In the event an error
!     is signaled this invalid value will be returned to the caller
!     for safety.
!
      handle = 0

!
!     Left justify FNAME to compress off any leading spaces.
!
      locfnm = trim ( adjustl ( fname ) )

!
!     Translate the value of the requested access method to the
!     corresponding integer code.
!
      tmpstr = method
      call ucase_inplace ( tmpstr )
      accmet = findloc(stramh, tmpstr, dim=1)

!
!     Check if the code was located.
!
      if ( accmet == 0 ) then

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         call ctx%setmsg ( 'The attempt to load file, ''#'', with access ' &
         //            'method, ''#'', failed because this access ' &
         //            'method is unsupported.'                        )
         call ctx%errch  ( '#', locfnm                                     )
         call ctx%errch  ( '#', method                                     )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDMETHOD)'                      )
         return

      end if

!
!     Translate the value of the requested file architecture to
!     the appropriate integer code.
!
      tmpstr = arch
      call ucase_inplace ( tmpstr )
      filarc = findloc(strarc, tmpstr, dim=1)

!
!     Check if the code was located.
!
      if ( filarc == 0 ) then

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         call ctx%setmsg ( 'The attempt to load file, ''#'', with ' &
         //            'architecture, ''#'', failed because this ' &
         //            'file architecture is unsupported.'             )
         call ctx%errch  ( '#', locfnm                                     )
         call ctx%errch  ( '#', arch                                       )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDARCH)'                        )
         return

      end if

!
!     Perform any preliminary checks that must be done before
!     fetching a logical unit from the unit table.  This requires
!     branching based on ACCMET's value.
!
      if ( accmet == scrtch ) then

!
!        Check to see if there are enough units available for locking
!        in the unit table.  If not, signal an error as all files
!        open with SCRTCH access must be locked to their units.
!
         locked = zzddhclu ( ctx, utlck, nut )

         if ( locked >= (size ( utlun, kind=ip ) - rsvunt) ) then

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            call ctx%setmsg ( 'The maximum number of units are locked ' &
            //            'to handles.  As such, there is no room ' &
            //            'to open the requested scratch file.'     )
            call ctx%sigerr ( 'SPICE(UTFULL)'                           )
            return

         end if

!
!     The NEW, READ, and WRITE access methods perform the same
!     checks on LOCFNM.
!
      else if ( ( accmet == new   ) .or. &
                ( accmet == read  ) .or. &
                ( accmet == write )      ) then

!
!        Check for a non-blank file name.
!
         if ( locfnm == ' ' ) then

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            call ctx%setmsg ( 'The attempt to load the file has failed, ' &
            //            'because the filename is blank.'            )
            call ctx%sigerr ( 'SPICE(BLANKFILENAME)'                      )
            return

         end if

      end if

!
!     PEPPER: a DAF built in memory (a kernel with no file behind it) is
!     found by its name (the most recently loaded, if several); it can
!     only be read.
!
      if ( (( accmet == read  ) .or. &
            ( accmet == write )) .and. .not. ft%files_only ) then
         do i = nft, 1, -1
            if ( .not. ft%files(i)%synthetic ) cycle
            if ( ft%files(i)%name /= locfnm ) cycle
            if ( accmet /= read ) then
               call ctx%setmsg ( 'The kernel ''#'' was built in memory: ' &
               //            'it has no file, and it cannot be opened ' &
               //            'for # access.' )
               call ctx%errch  ( '#', locfnm          )
               call ctx%errch  ( '#', stramh(accmet)  )
               call ctx%sigerr ( 'SPICE(FILEOPENCONFLICT)' )
               return
            end if
            if ( filarc /= ft%files(i)%arch ) then
               call ctx%setmsg ( 'The attempt to load file ''#'' as a # ' &
               //            'has failed because it is already loaded ' &
               //            'as a #.'                                 )
               call ctx%errch  ( '#', locfnm                               )
               call ctx%errch  ( '#', strarc(filarc)                       )
               call ctx%errch  ( '#', strarc(ft%files(i)%arch)             )
               call ctx%sigerr ( 'SPICE(FILARCMISMATCH)'                   )
               return
            end if
            handle = ft%files(i)%handle
            return
         end do
      end if

      mnm = 0.0_wp

!
!     In the READ or WRITE cases verify that LOCFNM is not already
!     in the file table.
!
      if ( ( accmet == read  ) .or. &
           ( accmet == write )      ) then

!
!        Check to see if the file associated with LOCFNM is already in
!        the file table.
!
         call zzddhf2h ( ctx, locfnm,  ft,     utcst, &
                         uthan,   utlck,  utlun,  nut,   inqext, &
                         inqopn,  inqhan, locfnd, mnm, size ( utlun, kind=ip ) )

!
!        First, check FAILED(), and return if anything has gone awry.
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         if ( ctx%failed() ) return

!
!        Now perform some simple sanity checks before preparing to
!        load the file.  First check to see if the file exists, it must
!        if we are going to open it with ACCMET set to READ or WRITE.
!
         if ( .not. inqext ) then

!
!           Recall HANDLE was initialized to 0, and this invalid
!           value is returned to the caller.
!
            call ctx%setmsg ( 'The file ''#'' does not exist.' )
            call ctx%errch  ( '#', locfnm                      )
            call ctx%sigerr ( 'SPICE(FILENOTFOUND)'            )
            return

         end if

!
!        Now if the file was not found in the file table, and it is
!        attached to a unit, this presents a problem.
!
!        PEPPER: a unit of another context may still be connected to a
!        file that was closed (it closes it when it next uses its unit
!        table); that is not an error. Nor, for reading, is a unit of
!        another instance (see opened_by_pepper).
!
         if ( ( .not. locfnd ) .and. ( inqopn ) .and. &
              ( .not. was_closed ( ctx%spice, locfnm ) ) .and. &
              ( .not. ( accmet == read .and. opened_by_pepper ( ctx%spice, locfnm ) ) ) ) then

!
!           Get the unit to include in the error message.
!
            inquire ( file   = locfnm, &
                      number = loclun, &
                      iostat = iostat  )

!
!           Since we performed a very similar INQUIRE statement in
!           ZZDDHF2H, a non-zero IOSTAT value indicates a severe error.
!
            if ( iostat /= 0 ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call ctx%setmsg ( 'INQUIRE failed.' )
               call ctx%sigerr ( 'SPICE(BUG)'      )
               return

            end if

!
!           Signal the error. Recall HANDLE was initialized to 0, and
!           this invalid value is returned to the caller.
!
            call ctx%setmsg ( 'The file ''#'' is already connected to ' &
            //            'unit #.'                                 )
            call ctx%errch  ( '#', locfnm                               )
            call ctx%errint ( '#', loclun                               )
            call ctx%sigerr ( 'SPICE(IMPROPEROPEN)'                     )
            return

         end if

!
!        Lastly check to see if the file in the file table, and
!        perform the appropriate sanity checks.
!
         if ( locfnd ) then

            findex = ft%find ( inqhan )

!
!           Check to see if the requested architecture does not match
!           that of the entry in the file table.
!
            if ( filarc /= ft%files(findex)%arch ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call ctx%setmsg ( 'The attempt to load file ''#'' as a # ' &
               //            'has failed because it is already loaded ' &
               //            'as a #.'                                 )
               call ctx%errch  ( '#', locfnm                               )
               call ctx%errch  ( '#', strarc(filarc)                       )
               call ctx%errch  ( '#', strarc(ft%files(findex)%arch)                )
               call ctx%sigerr ( 'SPICE(FILARCMISMATCH)'                   )
               return

            end if

!
!           Check to see if the access method is anything other
!           than READ.  If so, signal the appropriate error.
!           Note: this is only for READ.
!
            if ( accmet /= read ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call ctx%setmsg ( 'File ''#'' already loaded.' )
               call ctx%errch  ( '#', locfnm                  )
               call ctx%sigerr ( 'SPICE(FILEOPENCONFLICT)'    )
               return

            end if

!
!           If we reach here, then we have a file that exists
!           in the table, and the caller is attempting to load it
!           for READ access.  Check to make certain it is not
!           already loaded with another method.
!
            if ( accmet /= ft%files(findex)%method ) then

!
!              Recall HANDLE was initialized to 0, and this invalid
!              value is returned to the caller.
!
               call ctx%setmsg ( 'Unable to load file ''#'' for # ' &
               //            'access.  It is already loaded with ' &
               //            'the conflicting access #.'           )
               call ctx%errch  ( '#', locfnm                           )
               call ctx%errch  ( '#', stramh(accmet)                   )
               call ctx%errch  ( '#', stramh(ft%files(findex)%method)            )
               call ctx%sigerr ( 'SPICE(RWCONFLICT)'                   )
               return

            end if

!
!           If we make it this far, the file is in the file table
!           and all the sanity checks have passed. Return to the
!           caller as this is effectively a no-op.
!
            handle = ft%files(findex)%handle

            return

         end if

      end if

!
!     PEPPER: the file table grows as needed; SPICELIB signaled
!     SPICE(FTFULL) here when it had FTSIZE (5000) files.
!

!
!     We are about to attempt a HANDLE to LUN connection, increment
!     the request counter.
!
      call zzddhrcm ( ctx, nut, utcst, reqcnt )

!
!     Free up a logical unit in the UNIT table for our usage.
!
      call zzddhgtu ( ctx, utcst, uthan, utlck, utlun, nut, uindex, size ( utlun, kind=ip ) )

!
!     Check FAILED() since ZZDDHGTU may have invoked GETLUN.
!     Recall HANDLE was initialized to 0, and this invalid
!     value is returned to the caller.
!
      if ( ctx%failed() ) return

!
!     (PEPPER: the filename was trimmed when LOCFNM was set.)
!
!     If we have made it this far, then we're ready to perform the
!     appropriate open.  First get the handle ready.
!
      next = next + 1

!
!     Determine the sign of the new handle based on the requested
!     METHOD.
!
      if ( accmet == read ) then
         uthan ( uindex ) = next
      else
         uthan ( uindex ) = -next
      end if

!
!     The code that follows is structured a little strangely.  This
!     discussion is an attempt to clarify what the code does and
!     the motivation that led to its peculiar construction.
!
!     First, the file, scratch or otherwise, is opened with the
!     appropriate OPEN statement.  Then, the logical ERROR is set
!     to TRUE or FALSE depending on whether and IOSTAT error has
!     occurred as a result of the OPEN.  At this point, the code
!     enters into a IF block structured in the following manner:
!
!        IF ( ERROR ) THEN
!
!           Signal the IOSTAT related error from the OPEN statement.
!
!        ELSE IF ( ACCMET .EQ SCRTCH ) THEN
!
!           Attempt to INQUIRE on the UNIT assigned to the scratch
!           file to determine its name.  Store a default value,
!           in the event one is not returned.
!
!        ELSE IF ( ACCMET .EQ. READ ) .OR. ( ACCMET .EQ. WRITE ) THEN
!
!           Examine the preexisting file to determine if its FTP
!           detection string, file architecture, and binary
!           file format are acceptable.  If not, then signal the
!           error, set ERROR to TRUE, and do not check out or
!           return.
!
!        END IF
!
!        IF ( ERROR ) THEN
!
!           Remove the UNIT from the unit table. Decrement NEXT,
!           since the current value is not to be assigned as
!           a handle for this file. return.
!
!        END IF
!
!     The reason the code is structured in this unusual fashion
!     is to allow for a single treatment of the clean up on error
!     code to exist.
!

!
!     Perform the OPEN.  Branch on the appropriate access method.
!
      if ( accmet == scrtch ) then

         open ( newunit = utlun(uindex), &
                access = 'DIRECT', &
                recl   = record_length(), &
                status = 'SCRATCH', &
                iostat = iostat           )

         bff = natbff

      else if ( accmet == new ) then

         open ( newunit = utlun(uindex), &
                file   = locfnm, &
                access = 'DIRECT', &
                recl   = record_length(), &
                status = 'NEW', &
                iostat = iostat           )

         bff = natbff

      else if ( accmet == read ) then

         open ( newunit = utlun(uindex), &
                file   = locfnm, &
                access = 'DIRECT', &
                recl   = record_length(), &
                status = 'OLD', &
                iostat = iostat           )

      else if ( accmet == write ) then

         open ( newunit = utlun(uindex), &
                file   = locfnm, &
                access = 'DIRECT', &
                recl   = record_length(), &
                status = 'OLD', &
                iostat = iostat           )

      end if

!
!     Verify that IOSTAT is non-zero.
!
      error = ( iostat /= 0 )

!
!     Partially process the error.
!
      if ( error ) then

!
!        Now signal the error, but delay cleaning up and checking
!        out until leaving this IF block.
!
         if ( accmet == scrtch ) then
            call ctx%setmsg ( 'Attempt to open scratch file failed. ' &
            //            'IOSTAT was #.'                         )
         else if ( accmet == new ) then
            call ctx%setmsg ( 'Attempt to create new file, ''$'' ' &
            //            'failed. IOSTAT was #.'                 )
         else
            call ctx%setmsg ( 'Attempt to open file, ''$'' for % ' &
            //            'access failed. IOSTAT was #.'          )
         end if

         call ctx%errch  ( '$', locfnm                                )
         call ctx%errch  ( '%', stramh ( accmet )                     )
         call ctx%errint ( '#', iostat                                )
         call ctx%sigerr ( 'SPICE(FILEOPENFAIL)'                      )

!
!     If no IOSTAT based error has occurred as a result of the OPEN
!     statement, then perform any remaining checks or I/O operations
!     that are necessary to support loading the file.
!
      else if ( accmet == scrtch ) then

!
!        Inquire on the logical unit to produce the file name for
!        the scratch file.  Set the initial value of LOCFNM, in case
!        the INQUIRE does not replace it.
!
         locfnm = trim ( strarc(filarc) ) // ' SCRATCH FILE'
         scrnam = locfnm

         inquire ( unit   = utlun(uindex), &
                   name   = scrnam, &
                   iostat = iostat         )

!
!        In the event that this INQUIRE failed, keep the initial value
!        of LOCFNM. (PEPPER: the name the INQUIRE returns is read into
!        SCRNAM, of SCRLEN characters.)
!
         if ( iostat == 0 ) locfnm = trim ( scrnam )

      else if ( ( accmet == read  ) .or. &
                ( accmet == write )      ) then

!
!        Check for FTP errors, verify that FILARC is appropriate,
!        and determine the binary file format of the preexisting
!        file LOCFNM.
!
         call zzddhppf ( ctx, utlun(uindex), filarc, bff )

!
!        Set ERROR.
!
         error = ctx%failed()

!
!        If no error has occurred, verify that BFF is among the
!        list of supported format ID codes for the requested access
!        method.
!
         if ( .not. error ) then

!
!           This platform supports reading from files whose
!           format codes are listed in SUPBFF.
!
            if ( accmet == read ) then

               supidx = findloc(supbff(1:numsup), bff, dim=1)

               if ( supidx == 0 ) then

!
!                 Delay clean up.
!
                  error = .true.

                  if ( bff == 0 ) then

                     call ctx%setmsg ( 'Attempt to open file, ''#'', for ' &
                     //            'read access has failed.  This ' &
                     //            'file utilizes an unknown binary ' &
                     //            'file format.  This error may ' &
                     //            'result from attempting to open a ' &
                     //            'corrupt file or one of an ' &
                     //            'unknown type.'                     )
                     call ctx%errch  ( '#', locfnm                         )
                     call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)'             )

                  else

                     call ctx%setmsg ( 'Attempt to open file, ''#'', for ' &
                     //            'read access has failed.  The ' &
                     //            'non-native binary file format ' &
                     //            '''#'' is not currently supported ' &
                     //            'on this platform.  Obtain a ' &
                     //            'transfer format version, and ' &
                     //            'convert it to the native format. ' &
                     //            'See the Convert User''s Guide ' &
                     //            'for details.'                      )
                     call ctx%errch  ( '#', locfnm                         )
                     call ctx%errch  ( '#', strbff(bff)                    )
                     call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)'             )

                  end if

               end if

!
!           This platform only supports writing to files whose
!           binary formats are native.
!
            else

!
!              Delay clean up.
!
               if ( bff == 0 ) then

                  error = .true.

                  call ctx%setmsg ( 'Attempt to open file, ''#'', for ' &
                  //            'write access has failed.  This ' &
                  //            'file utilizes an unknown binary ' &
                  //            'file format.  This error may ' &
                  //            'result from attempting to open a ' &
                  //            'corrupt file or one of an unknown ' &
                  //            'type.'                              )
                  call ctx%errch  ( '#', locfnm                          )
                  call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)'              )

               else if ( bff /= natbff ) then

                  error = .true.

                  call ctx%setmsg ( 'Attempt to open file, ''#'', for ' &
                  //            'write access has failed.  This ' &
                  //            'file utilizes the non-native ' &
                  //            'binary file format ''#''.  At ' &
                  //            'this time only files of the ' &
                  //            'native format, ''#'', are ' &
                  //            'supported for write access.  See ' &
                  //            'the Convert User''s Guide for ' &
                  //            'details.'                          )
                  call ctx%errch  ( '#', locfnm                         )
                  call ctx%errch  ( '#', strbff(bff)                    )
                  call ctx%errch  ( '#', strbff(natbff)                 )
                  call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)'             )

               end if

            end if

         end if

      end if

!
!     If an error has occurred as a result of opening the file or
!     examining its contents, clean up.
!
      if ( error ) then

!
!        Close the unit we were using.  Remember to delete the file
!        if it was a 'new' one.
!
         if ( accmet == new ) then
            if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex), status = 'DELETE' )
         else
            if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex) )
         end if

!
!        Remove the unit from the unit table, since this UNIT
!        is no longer in use.
!
         call zzddhrmu ( ctx, uindex, nft, utcst, uthan, utlck, utlun, nut )

!
!        Decrement NEXT since this handle was never assigned to
!        a file.
!
         next = next - 1

!
!        Recall HANDLE was initialized to 0, and this invalid
!        value is returned to the caller.
!
         return

      end if

!
!     Finish filling out the unit table.
!
      utcst ( uindex ) = reqcnt

!
!     Only scratch files get the units locked to handles, this is
!     because they only exist as long as they have a unit.
!
      utlck ( uindex ) = accmet == scrtch

!
!     Now fill out the file table: the handle, access method, file
!     architecture, native binary file format, unique DP number and
!     filename. (PEPPER: one record, appended; the table grows.)
!
      call ft%add ( ddh_file ( handle = uthan ( uindex ), method = accmet, &
                               arch = filarc, bff = bff, mnm = mnm, name = locfnm ) )

!
!     Assign HANDLE the value of the new handle.
!
      handle = uthan ( uindex )

      return
      end associate
      end subroutine zzddhopn

      !> Close the file associated with HANDLE.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the file handle associated with the file that is
      !>                to be closed.
      !>
      !>     ARCH       is the expected architecture of the file associated
      !>                with HANDLE.
      !>
      !>     KILL       is a logical that indicates whether to kill the file
      !>                associated with HANDLE.  Essentially it performs:
      !>
      !>                   CLOSE ( UNIT, STATUS = 'DELETE')
      !>
      !>                on the file.  This only works if HANDLE is currently
      !>                assigned a UNIT in the UNIT table.
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
      !>     1) SPICE(FILARCMISMATCH) is signaled if the specified architecture
      !>        does not match the one listed in the file table.
      !>
      !>     2) SPICE(INVALIDACCESS) is signaled if KILL is set to .TRUE., and
      !>        HANDLE is associated with a file open for READ access.
      !>
      !>     3) SPICE(FILENOTCONNECTED) is signaled if KILL is set to .TRUE.,
      !>        and the file associated with handle is not currently in the
      !>        unit table.  The file is removed from the file table (closed)
      !>        as a result, even if this error is signaled.
      !>
      !>     4) If HANDLE is not found in the file table, this routine simply
      !>        returns to the caller.
      !>
      !>## Files
      !>
      !>     This routine will close the file associated with HANDLE if it
      !>     is currently utilizing a logical unit.
      !>
      !>## Particulars
      !>
      !>     This routine closes files in the file table and performs
      !>     any necessary operations to facilitate the proper disconnect
      !>     from any logical unit.
      !>
      !>     This routine may also be used to delete a file that is open
      !>     for write access if it currently has an entry in the UNIT table.
      !>
      !>## Examples
      !>
      !>     See DAFCLS or DASLLC.
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
      !>     F.S. Turner     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
      !>
      !>        Updated for the new "magic number" column in the file table.
      !>
      !>-    SPICELIB Version 2.0.0, 02-APR-2001 (FST)
      !>
      !>        Added a "KILL" argument to the argument list of the routine.
      !>        This will allow certain raw close statements to be replaced
      !>        with calls to ZZDDHCLS.
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 02-APR-2002 (FST)
      !>
      !>        Added the error SPICE(FILENOTCONNECTED) since the KILL
      !>        functionality is only required in the entry points:
      !>
      !>            DASFM - DASOPN, DASONW
      !>            DAFAH - DAFOPN, DAFONW
      !>
      !>        These routines open new files, so they reference newly
      !>        created handles that have entries in the UNIT table. Thus
      !>        the decision was made to signal the error
      !>        SPICE(FILENOTCONNECTED) rather than connect a file not
      !>        present in the unit table when KILL is set.
      module subroutine zzddhcls ( ctx, handle, arch, kill )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchi
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: arch  !! Expected architecture of the handle to close.
      integer(ip), intent(in) :: handle  !! File handle associated with the file to close.
      logical, intent(in) :: kill  !! Logical indicating whether to delete the file.
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup, &
                  nut => ctx%zzddhman_units%nut, &
                  utcst => ctx%zzddhman_units%utcst, &
                  uthan => ctx%zzddhman_units%uthan, &
                  utlck => ctx%zzddhman_units%utlck, &
                  utlun => ctx%zzddhman_units%utlun )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         if ( ctx%failed() ) return

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Find the file in the handle table.
!
      findex = ft%find ( handle )

!
!     Check to see whether we found the handle or not.
!
      if ( findex == 0 ) then
         return
      end if

!
!     Before actually closing the file, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architecture's code from stepping on another's.
!
      tmpstr = arch
      call ucase_inplace ( tmpstr )
      filarc = findloc(strarc, tmpstr, dim=1)

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      if ( filarc /= ft%files(findex)%arch ) then

         call ctx%setmsg ( 'Logical unit associated with # file $, ' &
         //            'is trying to be closed by routines in ' &
         //            'in the % system.'                         )
         call ctx%errch  ( '#', strarc( ft%files(findex)%arch )               )
         call ctx%errch  ( '%', tmpstr                                )
         call ctx%errch  ( '$', ft%files(findex)%name                         )
         call ctx%sigerr ( 'SPICE(FILARCMISMATCH)'                    )
         return

      end if

!
!     Now check that if KILL is set, the file is accessible for
!     WRITE.
!
      if ( ( kill ) .and. ( ft%files(findex)%method == read ) ) then

         call ctx%setmsg ( '# file $ is open for READ access.  Attempt ' &
         //            'to close and delete file has failed. '       )
         call ctx%errch  ( '#', strarc( ft%files(findex)%arch )                  )
         call ctx%errch  ( '#', ft%files(findex)%name                            )
         call ctx%sigerr ( 'SPICE(INVALIDACCESS)'                        )
         return

      end if

!
!     Buffer the access method for HANDLE, since we may need it
!     when deciding which close to perform.
!
      accmet = ft%files(findex)%method
!
!     PEPPER: note the close (other contexts' units for this handle).
!
      call file_closed ( ctx%spice, ft%files(findex)%name )

!
!     If we reach here, we need to remove the row FINDEX from
!     the file table.
!
      call ft%remove ( findex )

!
!     Locate HANDLE in the unit table.
!
      uindex = findloc(uthan(1:nut), handle, dim=1)

      if ( uindex /= 0 ) then

!
!        Close the unit.
!
         if ( ( kill ) .and. ( accmet /= scrtch ) ) then
            if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex), status = 'DELETE' )
         else
            if ( utlun(uindex) /= -1 ) close ( unit = utlun(uindex) )
         end if

!
!        Remove its entry from the unit table.
!
         call zzddhrmu ( ctx, uindex, nft, utcst, uthan, utlck, utlun, nut )

      else

!
!        First, check to see if KILL is set, if it is signal an error
!        since we are unable to delete the file.
!
         if ( ( kill ) .and. ( accmet /= scrtch ) ) then

            call ctx%setmsg ( 'File successfully closed.  Unable to ' &
            //            'delete file as requested.  File not ' &
            //            'currently present in the UNIT table. ' )
            call ctx%sigerr ( 'SPICE(FILENOTCONNECTED)'               )
            return

         end if

!
!        If we were unable to find the HANDLE in the unit table,
!        check to see if we have to clean up the UNIT table.
!
         if ( nft < nut ) then

            uindex = findloc(uthan(1:nut), 0, dim=1)

!
!           Now check to see if we located a zero valued handle.
!           If we did not manage to, then this is an error condition,
!           since we have more LUNs listed in the unit table than
!           files in the file table.
!
            if ( uindex == 0 ) then
               call ctx%setmsg ( 'There are less files in the file table ' &
               //            'than units in the unit table, and no ' &
               //            'row with a zero-valued handle can be ' &
               //            'found.  This should never occur.'        )
               call ctx%sigerr ( 'SPICE(BUG)'                              )
               return
            end if

!
!           Free the unit.
!
!           PEPPER: units come from NEWUNIT=; nothing to free.

!
!           Compress the table.
!
            do i = (uindex + 1), nut

               utcst(i-1) = utcst(i)
               uthan(i-1) = uthan(i)
               utlck(i-1) = utlck(i)
               utlun(i-1) = utlun(i)

            end do

!
!           Decrement NUT.
!
            nut = nut - 1

         end if
      end if

      return
      end associate
      end subroutine zzddhcls

      !> Return the logical unit associated with a handle, in the event
      !> the handle is not connected to a unit, connect it to one.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle associated with the file to retrieve a
      !>                logical unit.
      !>
      !>     ARCH       is the expected file architecture of the file
      !>                associated with HANDLE.
      !>
      !>     LOCK       is a logical that indicates whether the UNIT should be
      !>                locked to HANDLE.  Locked units will keep the files
      !>                open and assigned the same logical unit.  They may
      !>                only be unlocked by calling ZZDDHUNL.
      !>
      !>## Detailed Output
      !>
      !>     UNIT       is the logical unit that is currently assigned to
      !>                HANDLE.  This unit may be used to perform I/O
      !>                operations on the file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) The error SPICE(NOSUCHHANDLE) is signaled when HANDLE is not
      !>        found in the file table.  The value of UNIT is undefined when
      !>        this error occurs.
      !>
      !>     2) The error SPICE(FILARCMISMATCH) is signaled if the specified
      !>        architecture does not match the one listed for HANDLE in the
      !>        file table.  The value of UNIT is undefined when this error
      !>        occurs.
      !>
      !>     3) SPICE(FILEOPENFAIL) is signaled only when an attempt to
      !>        attach a logical unit to the file associated with HANDLE
      !>        fails.  The value of UNIT is undefined when this error
      !>        occurs.
      !>
      !>     4) The error SPICE(HLULOCKFAILED) is signaled when the input
      !>        LOCK logical has a value of TRUE, and there are no free
      !>        'lockable' units left in the unit table.  The value of UNIT
      !>        returned when this error is signaled is undefined.
      !>
      !>## Files
      !>
      !>     If HANDLE refers to a file not currently connected to a logical
      !>     unit, this routine will locate an entry in the unit table;
      !>     disconnect it if necessary; and connect the file associated with
      !>     HANDLE in its place.
      !>
      !>## Particulars
      !>
      !>     This routine is used to retrieve a logical unit for a desired
      !>     handle.  It also serves as a mechanism for locking the UNIT
      !>     to HANDLE relationship for a particular handle.  This routine
      !>     can not be used to unlock this relationship.  See ZZDDHUNL for
      !>     that functionality.
      !>
      !>## Examples
      !>
      !>     See DAFHLU or DASHLU for sample usage.
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
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 01-OCT-2021 (NJB)
      !>
      !>        Fixed typo in long error message and in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhhlu ( ctx, handle, arch, lock, unit )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchi
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(in) :: lock  !! Logical indicating to lock UNIT to HANDLE.
      character(*), intent(in) :: arch  !! Expected file architecture.
      integer(ip), intent(in) :: handle  !! Handle associated with the file of interest.
      integer(ip), intent(out) :: unit  !! Corresponding logical unit.
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup, &
                  nut => ctx%zzddhman_units%nut, &
                  utcst => ctx%zzddhman_units%utcst, &
                  uthan => ctx%zzddhman_units%uthan, &
                  utlck => ctx%zzddhman_units%utlck, &
                  utlun => ctx%zzddhman_units%utlun, &
                  reqcnt => ctx%zzddhman_units%reqcnt )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         if ( ctx%failed() ) then
            unit = 0
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Locate HANDLE in the file table.
!
      findex = ft%find ( handle )

      if ( findex == 0 ) then
         error = .true.
      else
         error = .false.
      end if

      if ( error ) then

         unit = 0

         call ctx%setmsg ( 'There is no file loaded with handle = #' )
         call ctx%errint ( '#', handle                               )
         call ctx%sigerr ( 'SPICE(NOSUCHHANDLE)'                     )
         return
      end if

!
!     PEPPER: a DAF built in memory has no file, so it has no unit.
!
      if ( ft%files(findex)%synthetic ) then
         unit = 0
         call ctx%setmsg ( 'The kernel ''#'' (handle #) was built in ' &
         //            'memory: it has no file, and this operation ' &
         //            'needs one.' )
         call ctx%errch  ( '#', ft%files(findex)%name )
         call ctx%errint ( '#', handle                )
         call ctx%sigerr ( 'SPICE(NOFILEFORKERNEL)'   )
         return
      end if

!
!     Before actually fetching the unit, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architectures code from stepping on another's.
!
      tmpstr = arch
      call ucase_inplace ( tmpstr )
      filarc = findloc(strarc, tmpstr, dim=1)

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      if ( filarc /= ft%files(findex)%arch ) then

         unit = 0

         call ctx%setmsg ( 'Logical unit associated with # file $, ' &
         //            'is trying to be unlocked by routines in ' &
         //            'in the % system.'                         )
         call ctx%errch  ( '#', strarc( ft%files(findex)%arch )               )
         call ctx%errch  ( '%', tmpstr                                )
         call ctx%errch  ( '$', ft%files(findex)%name                         )
         call ctx%sigerr ( 'SPICE(FILARCMISMATCH)'                    )
         return

      end if

!
!     If we make it this far, then we will be processing a handle
!     to logical unit request.  Increment REQCNT.
!
      call zzddhrcm ( ctx, nut, utcst, reqcnt )

!
!     Now check to see if the handle is already present in the
!     unit table.
!
      uindex = findloc(uthan(1:nut), handle, dim=1)

!
!     Check to see if we didn't locate the HANDLE in the table.
!     If we didn't, open the file associated with HANDLE again,
!     and get it into the unit table.
!
      if ( uindex == 0 ) then

!
!        We need a unit from the unit table, get one.
!
         call zzddhgtu ( ctx, utcst, uthan, utlck, utlun, nut, uindex, size ( utlun, kind=ip ) )

!
!        Check FAILED, since ZZDDHGTU may have invoked GETLUN.
!
         if ( ctx%failed() ) then

            unit = 0

            return
         end if

!
!        Re-attach the file to a logical unit.  Branch based on the
!        access method stored in the file table.
!
         if ( ( ft%files(findex)%method == new   ) .or. &
              ( ft%files(findex)%method == write )      ) then

            open ( newunit = utlun(uindex), &
                   file   = ft%files(findex)%name, &
                   access = 'DIRECT', &
                   recl   = record_length(), &
                   status = 'OLD', &
                   iostat = iostat           )

         else if ( ft%files(findex)%method == read ) then

            open ( newunit = utlun(uindex), &
                   file   = ft%files(findex)%name, &
                   access = 'DIRECT', &
                   recl   = record_length(), &
                   status = 'OLD', &
                   iostat = iostat           )

         else

            unit = 0

            call ctx%setmsg ( 'Invalid access method. This error ' &
            //            'should never be signaled.'          )
            call ctx%sigerr ( 'SPICE(BUG)'                         )
            return
         end if

!
!        Check IOSTAT for troubles.
!
         if ( iostat /= 0 ) then

!
!           The re-open was unsuccessful, leave the entry in the file
!           table and clean up the row in the unit table before
!           returning.  Normally when we call ZZDDHRMU it is to
!           remove a unit from the unit table.  In this case we
!           know the unit will remain since we have not decreased
!           the entries in the file table.
!
            call zzddhrmu ( ctx, uindex, nft,   utcst, uthan, &
                            utlck,  utlun, nut           )

!
!           Now signal the error.
!
            unit = 0

            call ctx%setmsg ( 'Attempt to reconnect logical unit to ' &
            //            'file ''#'' failed. IOSTAT was #.'      )
            call ctx%errch  ( '#', ft%files(findex)%name                      )
            call ctx%errint ( '#', iostat                             )
            call ctx%sigerr ( 'SPICE(FILEOPENFAIL)'                   )
            return

         end if

!
!        Lastly populate the unit table values.
!
         uthan(uindex) = ft%files(findex)%handle
         utlck(uindex) = .false.

      end if

!
!     At this point UINDEX points to the row in the unit table that
!     contains the connection information.  We need to update the cost
!     row with the new value of REQCNT, and then set the lock row to
!     TRUE if a lock request was made.
!
      utcst(uindex) = reqcnt

      if ( lock .and. (.not. utlck(uindex)) ) then

!
!        First check to see if we have enough lockable units
!        left in the unit table.
!
         locked = zzddhclu ( ctx, utlck, nut )

         if ( locked >= ( size ( utlun, kind=ip ) - rsvunt - scrunt ) ) then

            unit = 0

            call ctx%setmsg ( 'Unable to lock handle for file ''#'' ' &
            //            'to a logical unit.  There are no rows ' &
            //            'available for locking in the unit table.' )
            call ctx%errch  ( '#', ft%files(findex)%name                         )
            call ctx%sigerr ( 'SPICE(HLULOCKFAILED)'                     )
            return

         end if

         utlck(uindex) = .true.

      end if

!
!     Set the value of UNIT and return.
!
      unit = utlun(uindex)

      return
      end associate
      end subroutine zzddhhlu

      !> Unlock a logical unit from the specified handle.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the file handle associated with the file that
      !>                is to have its logical unit lock released.
      !>
      !>     ARCH       is the expected architecture of the file associated
      !>                with HANDLE.
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
      !>     1) If HANDLE is 0, not found in the unit table, or found and
      !>        not currently locked, this routine just returns to the
      !>        caller.
      !>
      !>     2) SPICE(FILARCMISMATCH) is signaled if the specified architecture
      !>        does not match the one listed in the file table.
      !>
      !>     3) If HANDLE is associated with a scratch file, this routine
      !>        simply returns, as scratch files may not be unlocked from
      !>        their logical units.
      !>
      !>## Files
      !>
      !>     This routine does not explicitly alter the open or closed
      !>     state of the file associated with HANDLE.
      !>
      !>## Particulars
      !>
      !>     This routine allows users to unlock a handle from it's logical
      !>     unit in the event a handle to logical unit request was made
      !>     with the LOCK argument set to true.  (DAFHLU and DASHLU both
      !>     lock units, and require a call to this routine to unlock them).
      !>
      !>## Examples
      !>
      !>     See some routine that calls this one (TBD).
      !>
      !>## Restrictions
      !>
      !>     This routine utilizes discovery check in and out.  However,
      !>     routines in the initialization loop may signal the error
      !>     SPICE(BUG) under the conditions of the existence of a bug.
      !>     Since this routine utilizes discovery check in and out,
      !>     no check in or out is performed around the initialization
      !>     block.  This is by design.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhunl ( ctx, handle, arch )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchi
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: arch  !! Expected architecture of the handle to unlock.
      integer(ip), intent(in) :: handle  !! File handle assigned to the file to unlock.
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup, &
                  nut => ctx%zzddhman_units%nut, &
                  uthan => ctx%zzddhman_units%uthan, &
                  utlck => ctx%zzddhman_units%utlck )
!
!     Standard SPICE discovery error handling.
!
      if ( ctx%failed() ) return

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).  No check out is performed, see the
!        $Restrictions section of the entry point header for details.
!
         if ( ctx%failed() ) return

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if
!
!     Prevent the user from locating zero handle rows.  This is not
!     really necessary since zero handle rows in the unit table are
!     empty and awaiting connections.  The state of the UTLCK is
!     not significant.
!
      if ( handle == 0 ) return

!
!     Look up the handle in the unit table.
!
      uindex = findloc(uthan(1:nut), handle, dim=1)

!
!     Now check the results of the lookup.  If HANDLE was not found
!     in the unit table or the unit was not locked, just return as
!     there is nothing to do.
!
      if ( uindex == 0 ) then
         return
      else if ( .not. utlck(uindex) ) then
         return
      end if

!
!     Now look up the handle in the table. Remember FTABS is a sorted
!     list in increasing order.
!
      findex = ft%find ( handle )

!
!     Check to see if HANDLE is in the file table.  We know it has
!     to be since it is in the unit table if we make it this far.
!     These checks are just for safety's sake.
!
      if ( findex == 0 ) then

         call ctx%setmsg ( 'HANDLE # was not found in the file table ' &
         //            'but was located in the unit table.  This ' &
         //            'error should never occur.'                 )
         call ctx%errint ( '#', handle                                 )
         call ctx%sigerr ( 'SPICE(BUG)'                                )
         return

      end if

!
!     Before actually unlocking the unit, check the input architecture
!     matches that listed in the file table for this handle.  This is
!     to prevent one architectures code from stepping on another's.
!
      tmpstr = arch
      call ucase_inplace ( tmpstr )
      filarc = findloc(strarc, tmpstr, dim=1)

!
!     Check to see if FILARC matches the code stored in the FTARC
!     column of the file table for this handle.  If it doesn't,
!     signal an error.
!
      if ( filarc /= ft%files(findex)%arch ) then

         call ctx%setmsg ( 'Logical unit associated with # file $, ' &
         //            'is trying to be unlocked by routines in ' &
         //            'in the % system.'                         )
         call ctx%errch  ( '#', strarc( ft%files(findex)%arch )               )
         call ctx%errch  ( '%', tmpstr                                )
         call ctx%errch  ( '$', ft%files(findex)%name                         )
         call ctx%sigerr ( 'SPICE(FILARCMISMATCH)'                    )
         return

      end if

!
!     Lastly, check to see if the access method for HANDLE indicates
!     scratch access.  If it is, just return, since scratch files
!     can not have their units unlocked.
!
      if ( ft%files(findex)%method == scrtch ) return

      utlck(uindex) = .false.

      return
      end associate
      end subroutine zzddhunl

      !> Get information about the file attached to HANDLE.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the file handle associated with the file for which
      !>                information is requested.
      !>
      !>## Detailed Output
      !>
      !>     FNAME      is the name of the file used associated with HANDLE.
      !>                This is the name used to load the file originally.
      !>
      !>     INTARC     is an integer code for FNAME's file architecture.
      !>                See the include file 'zzddhman.inc' for particulars.
      !>                The following are possible outputs:
      !>
      !>                   DAS - Direct Access, Segregated File Architecture
      !>                   DAF - Double Precision Array File Architecture
      !>
      !>     INTBFF     is an integer code that represents FNAME's binary
      !>                file format.  See the include file 'zzddhman.inc' for
      !>                particulars.  The following are the possible outputs:
      !>
      !>                   BIGI3E - Big Endian IEEE Floating Point Format
      !>                   LTLI3E - Little Endian IEEE Floating Point Format
      !>                   VAXGFL - VAX G-Float Format
      !>                   VAXDFL - VAX D-Float Format
      !>
      !>     INTAMH     is an integer code that represents FNAME's access
      !>                method.  See the include file 'zzddhman.inc' for
      !>                particulars.  The following are possible outputs:
      !>
      !>                   READ   - File was loaded for read access
      !>                   WRITE  - File was loaded for read/write access
      !>                   NEW    - New file was created for read/write access
      !>                   SCRTCH - Scratch file created for read/write access
      !>
      !>     FOUND      is a logical if set to TRUE indicates that HANDLE
      !>                was located in the file table.  Otherwise, it was
      !>                not found in the table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1) If FOUND is FALSE, then the other output arguments
      !>        are undefined.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine provides access to information necessary for
      !>     translation and other I/O based tasks to modules that are
      !>     not entry points to this handle manager.
      !>
      !>## Examples
      !>
      !>     See ZZDAFGFR, ZZDAFGSR, or ZZDAFGDR for sample usage.
      !>
      !>## Restrictions
      !>
      !>     Routines in the call tree of this routine may signal the error
      !>     SPICE(BUG) under the conditions of the existence of a bug
      !>     in routines the initialization loop invokes.  Since this
      !>     routine is error free with the exception of this bug condition,
      !>     it does not participate in tracing by design.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhnfo ( ctx, handle, fname, intarc, intbff, intamh, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchi
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: fname  !! Name of the file associated with HANDLE.
      integer(ip), intent(in) :: handle  !! File handle assigned to file of interest.
      integer(ip), intent(out) :: intamh  !! Integer code for FNAME's access method.
      integer(ip), intent(out) :: intarc  !! Integer code for FNAME's file architecture.
      integer(ip), intent(out) :: intbff  !! Integer code for FNAME's binary file format.
      logical, intent(out) :: found  !! Logical that indicates if HANDLE was found.
      logical :: return
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup )
!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         if ( ctx%failed() ) return

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Look up the handle in the table.  Remember FTABS is sorted
!     listed in increasing order.
!
      findex = ft%find ( handle )

!
!     Check to see if HANDLE is in the handle table.  Remember that
!     we are indexing the table using the absolute value of handle.
!     So include a check to see that HANDLE is FTHAN(FINDEX).
!
      if ( findex == 0 ) then
         fname  = ' '
         intarc = 0
         intbff = 0
         intamh = 0
         found  = .false.
         return
      end if

!
!     If we make it this far, then we have a handle that is in
!     the handle table at row FINDEX.
!
      found  = .true.
      fname  = ft%files(findex)%name
      intarc = ft%files(findex)%arch
      intbff = ft%files(findex)%bff
      intamh = ft%files(findex)%method

      return
      end associate
      end subroutine zzddhnfo

      !> Determine whether the file attached to HANDLE is uses the
      !> binary file format native to the system.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the file handle associated with the file that is
      !>                to be determined to be native or not.
      !>
      !>## Detailed Output
      !>
      !>     NATIVE     is a logical that when set to TRUE indicates that
      !>                the file associated with HANDLE is of the native
      !>                binary file format for the current platform. If
      !>                FALSE, then the file is of an alien file format.
      !>
      !>     FOUND      is a logical that when set to TRUE indicates that
      !>                HANDLE was found in the file table and is associated
      !>                with a file.  If FALSE, then NATIVE remains unchanged,
      !>                since the file was not found in the table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error Free.
      !>
      !>     1) In the event that HANDLE can not be found in the file table,
      !>        FOUND is set to FALSE and NATIVE is left unchanged.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine simply answers the question:  "Is the file attached
      !>     to this handle of the native binary file format?"
      !>
      !>## Examples
      !>
      !>     See DAFRDA for sample usage.
      !>
      !>## Restrictions
      !>
      !>     Routines in the call tree of this routine may signal the error
      !>     SPICE(BUG) under the conditions of the existence of a bug
      !>     in routines the initialization loop invokes.  Since this
      !>     routine is error free with the exception of this bug condition,
      !>     it does not participate in tracing by design.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhisn ( ctx, handle, native, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchi
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle assigned to file to determine format.
      logical, intent(out) :: native  !! Indicates if the file format is native.
      logical, intent(out) :: found  !! Indicates if HANDLE is currently attached to file.
      logical :: return
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup )
!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         if ( ctx%failed() ) return

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Look up the handle in the table. Remember FTABS is sorted
!     listed in increasing order.
!
      findex = ft%find ( handle )

!
!     Check to see if HANDLE is in the handle table.  Remember
!     that we are indexing the table using the absolute value of
!     handle.  So include a check to see that HANDLE is FTHAN(FINDEX).
!
      if ( findex == 0 ) then
         found = .false.
         return
      end if

!
!     If we make it this far, then we have found HANDLE in the file
!     table.  Set NATIVE appropriately and FOUND to TRUE.
!
      native = natbff == ft%files(findex)%bff
      found  = .true.

      return
      end associate
      end subroutine zzddhisn

      !> Retrieve handle associated with filename.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FNAME      is the name of a file previously loaded with ZZDDHOPN.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE     is the handle associated with the file.
      !>
      !>     FOUND      is a logical when TRUE indicates HANDLE was located
      !>                for FNAME.  If FALSE no handle was found associated
      !>                with FNAME.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) ZZDDHF2H in the call tree of this routine performs I/O
      !>        functions and may signal errors that are the result of
      !>        I/O failures.  See ZZDDHF2H header for details.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is provided for completeness and serves only to
      !>     support the DAFFNH and DASFNH interfaces.
      !>
      !>## Examples
      !>
      !>     See DAFFNH or DASFNH for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) On VAX environments, this routine may only be used when
      !>        FNAME refers to a DAF or DAS file.  An error may be
      !>        signaled when used with unopened files that utilize other
      !>        architectures.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 26-APR-2012 (BVS)
      !>
      !>        Updated for the new "magic number" column in the file table.
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhfnh ( ctx, fname, handle, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of a file previously loaded with [[zzddhopn]].
      integer(ip), intent(out) :: handle  !! Corresponding file handle.
      logical, intent(out) :: found  !! Logical indicating whether HANDLE was located.
      character(:), allocatable :: locfnm
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( ft => ctx%spice%zzddhman, &
                  nft => ctx%spice%zzddhman%nft, &
                  first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup, &
                  nut => ctx%zzddhman_units%nut, &
                  utcst => ctx%zzddhman_units%utcst, &
                  uthan => ctx%zzddhman_units%uthan, &
                  utlck => ctx%zzddhman_units%utlck, &
                  utlun => ctx%zzddhman_units%utlun )
      if ( ctx%failed() ) return

!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() only to trap the possibility of ZZDDHINI
!        signaling SPICE(BUG).
!
         if ( ctx%failed() ) then
            handle = 0
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Left justify FNAME to trim any leading white space.
!
      locfnm = trim ( adjustl ( fname ) )

!
!     Look up FNAME in the handle table.
!
      call zzddhf2h ( ctx, locfnm,  ft,    utcst, &
                      uthan,   utlck,  utlun, nut,   inqext, &
                      inqopn,  inqhan, found, mnm, size ( utlun, kind=ip ) )

!
!     Check found and set HANDLE if we have got one.  No need to
!     check FAILED() since ZZDDHF2H returns FOUND set to FALSE on
!     error.
!
      if ( found ) then
         handle = inqhan
      else
         handle = 0
      end if

      return
      end associate
      end subroutine zzddhfnh

      !> Return the handle associated with a logical unit.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     UNIT       is the logical unit to which a file managed by DDH is
      !>                currently connected.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE     is the handle associated with the logical unit of
      !>                interest.
      !>
      !>     FOUND      is a logical flag if TRUE indicates that a HANDLE
      !>                was found associated with UNIT.  If FALSE indicates
      !>                no handle was found for UNIT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1) If UNIT is not found in the unit table, HANDLE is undefined
      !>        and FOUND is set to .FALSE.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is provided for completeness and serves only to
      !>     support the DAFLUH and DASLUH interfaces.
      !>
      !>## Examples
      !>
      !>     See DAFLUH or DASLUH for usage.
      !>
      !>## Restrictions
      !>
      !>     Routines in the call tree of this routine may signal the error
      !>     SPICE(BUG) under the conditions of the existence of a bug
      !>     in routines the initialization loop invokes.  Since this
      !>     routine is error free with the exception of this bug condition,
      !>     it does not participate in tracing by design.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
      module subroutine zzddhluh ( ctx, unit, handle, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Corresponding handle.
      integer(ip), intent(in) :: unit  !! Logical unit connected to a file.
      logical, intent(out) :: found  !! Logical indicating the handle was located.
      logical :: return
      character(strsiz) :: tmpstr
      real(wp) :: mnm
      integer(ip) :: accmet
      integer(ip) :: bff
      integer(ip) :: filarc
      integer(ip) :: i
      integer(ip) :: inqhan
      integer(ip) :: iostat
      integer(ip) :: lchar
      integer(ip) :: locked
      integer(ip) :: loclun
      integer(ip) :: supidx
      logical :: error
      logical :: inqext
      logical :: inqopn
      logical :: locfnd
      logical :: platok
      integer(ip) :: findex
      integer(ip) :: uindex
      call units_ready ( ctx )
      call units_check ( ctx )
      associate ( first => ctx%zzddhman_units%first, &
                  stramh => ctx%zzddhman_units%stramh, &
                  strarc => ctx%zzddhman_units%strarc, &
                  strbff => ctx%zzddhman_units%strbff, &
                  natbff => ctx%zzddhman_units%natbff, &
                  supbff => ctx%zzddhman_units%supbff, &
                  numsup => ctx%zzddhman_units%numsup, &
                  nut => ctx%zzddhman_units%nut, &
                  uthan => ctx%zzddhman_units%uthan, &
                  utlun => ctx%zzddhman_units%utlun )
!
!     Do the initialization tasks.
!
      if ( first ) then

         call zzddhini ( ctx, natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED(), and return on failure.  We are not checking
!        out or in since this routine would be error free if not for
!        the possibility of ZZDDHINI signaling SPICE(BUG).  See
!        $Restrictions for details.
!
         if ( ctx%failed() ) then
            handle = 0
            return
         end if

!
!        Clear FIRST since we've done the initialization.
!
         first = .false.

      end if

!
!     Look up the unit in the table.
!
      uindex = findloc(utlun(1:nut), unit, dim=1)

      if ( uindex == 0 ) then
         handle = 0
         found  = .false.
         return
      else if ( uthan(uindex) == 0 ) then
         handle = 0
         found  = .false.
         return
      end if

!
!     We've got a handle, store the value and return.
!
      handle = uthan(uindex)
      found  = .true.

      return
      end associate
      end subroutine zzddhluh

   !> PEPPER: allocate the unit table of a context, with the instance's
   !> [[max_open_files]] entries (SPICELIB: UTSIZE).
   subroutine units_ready ( ctx )
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: n
      if ( allocated ( ctx%zzddhman_units ) ) return
      allocate ( ctx%zzddhman_units )
      n = ctx%spice%open_files_limit
      allocate ( ctx%zzddhman_units%utcst(n), source = 0_ip )
      allocate ( ctx%zzddhman_units%uthan(n), source = 0_ip )
      allocate ( ctx%zzddhman_units%utlck(n), source = .false. )
      allocate ( ctx%zzddhman_units%utlun(n), source = 0_ip )
   end subroutine units_ready

end submodule pepper_files
