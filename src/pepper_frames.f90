!> Reference frames: names, IDs and classes (FRAMEX, [[zzfdat]]), and the parameters of dynamic frames.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## FRAMEX (FRAMe EXpert)
!>
!> PEPPER: SPICELIB's umbrella routine FRAMEX is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_framex. Its documentation follows.
!>
!>### Abstract
!>
!>     This is an umbrella routine for the entry points available for
!>     manipulating different reference frames. It should not be called
!>     directly.
!>
!>### Detailed Input
!>
!>     See individual entry points for details concerning inputs.
!>
!>### Detailed Output
!>
!>     See individual entry points for details concerning inputs.
!>
!>### Parameters
!>
!>     None.
!>
!>### Exceptions
!>
!>     1)  If this routine is called directly, the error
!>         SPICE(BOGUSENTRY) is signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     This is an umbrella routine that comprises the SPICE
!>     interface to the reference frame transformation software.
!>
!>     There are 6 entry points.
!>
!>     NAMFRM  converts string to the ID codes used by low level
!>             SPICE software
!>
!>     FRMNAM  converts frame ID codes to the more familiar names
!>             used to describe various reference frames.
!>
!>     FRINFO  returns the center associated with a reference frame.
!>
!>     CIDFRM  given the ID code of an object, returns the bodyfixed
!>             frame associated with it.
!>
!>     CNMFRM  given the name of an object, returns the bodyfixed
!>             frame associated with it.
!>
!>     CCIFRM  given a frame's class and class ID, returns
!>             the frame's ID code, name, and center.
!>
!>### Examples
!>
!>     Suppose that you needed to transform between two reference
!>     frames on the basis of their names and that you wanted to
!>     correct for light time to the center of the second frame
!>     as seen from an observer with ID code OBS.
!>
!>     The code fragment below illustrates how you could use the
!>     entry points gathered in this routine to retrieve the
!>     state transformation matrix.
!>
!>        First convert names to frame ID codes.
!>
!>        CHARACTER*(32)        NAME1
!>        CHARACTER*(32)        NAME2
!>
!>        INTEGER               FRAME1
!>        INTEGER               FRAME2
!>        INTEGER               CENT
!>        INTEGER               OBS
!>
!>        DOUBLE PRECISION      ET
!>        DOUBLE PRECISION      LT
!>
!>        DOUBLE PRECISION      STATE ( 6 )
!>        DOUBLE PRECISION      XFORM ( 6, 6 )
!>
!>        First we use the entry points NAMFRM to convert the frame
!>        names to ID codes.
!>
!>        CALL NAMFRM ( NAME1, FRAME1 )
!>        CALL NAMFRM ( NAME2, FRAME2 )
!>
!>        Next we determine the center of the second frame
!>
!>        CALL FRINFO ( FRAME2, CENT, FRCLSS, CLSSID, FOUND )
!>
!>        Determine the light time to the center of the second frame.
!>
!>        CALL SPKGEO ( CENT,  ET, 'J2000',  OBS, STATE, LT )
!>
!>        Finally get the state transformation from FRAME1 to FRAME2
!>        at time ET - LT
!>
!>        CALL FRMCHG ( FRAME1, FRAME2, ET-LT, XFORM )
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
!>     B.V. Semenov       (JPL)
!>     W.L. Taber         (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 5.3.0, 26-AUG-2021 (JDR)
!>
!>        Changed the argument name CLASS to FRCLSS in FRINFO and CCIFRM
!>        entry points for consistency with other routines.
!>
!>        Edited the header of the umbrella routine and all its entry
!>        points to comply with NAIF standard. Updated the $Examples
!>        section of CCIFRM entry point.
!>
!>        Updated frame name length from 26 to 32 in example code.
!>
!>        Increased MAXBFR from 127 to 149 to accommodate additional
!>        built-in PCK based frames.
!>
!>-    SPICELIB Version 5.2.1, 02-FEB-2017 (BVS)
!>
!>        Shortened one of permuted index entries in CCIFRM.
!>
!>-    SPICELIB Version 5.2.0, 08-AUG-2012 (BVS)
!>
!>        The routine was updated to be more efficient by using hashes
!>        instead kernel POOL look-ups for kernel POOL frames and by
!>        using hashes instead of ordered array searches for built-in
!>        frames.
!>
!>        Bug fix: CCIFRM entry point logic was corrected to examine the
!>        built-in frames before looking at the kernel POOL frames.
!>
!>-    SPICELIB Version 5.1.1, 09-FEB-2011 (NJB)
!>
!>        Bug fix: corrected logic in entry point CIDFRM for
!>        object-frame association for case where the assigned frame
!>        value is denoted by a frame code.
!>
!>        Fixed typo in FRAMEX header.
!>
!>-    SPICELIB Version 5.0.1, 17-MAR-2009 (EDW)
!>
!>        Entry point NAMFRM: Typo correction in $Required_Reading,
!>        changed FRAME to FRAMES.
!>
!>-    SPICELIB Version 5.0.0, 05-NOV-2007 (NJB)
!>
!>        Entry point CCIFRM (map frame class and class ID
!>        to frame ID code, name, and center) has been added.
!>
!>-    SPICELIB Version 4.0.0, 13-SEP-2005 (NJB)
!>
!>        Entry point FRINFO is no longer error-free. Various frame
!>        definition errors that were previously ignored are now
!>        diagnosed.
!>
!>        Entry point FRINFO has been updated to support specification
!>        of frame center by name or ID code. Previously only ID codes
!>        could be used to identify frame centers.
!>
!>-    SPICELIB Version 3.2.0, 20-DEC-2004 (BVS)
!>
!>        Added parameter incorporating maximum body name length and set
!>        it to the same value as MAXL from zzbodtrn.inc. Used this
!>        parameter to declare local variable that holds frame center
!>        name (LCNAME).
!>
!>        In FRINFO entry: removed special handling of the frame IDs
!>        less than -999. If they cannot be ``resolved'' using kernel
!>        pool keywords, the frame is NOT declared CK-based with center
!>        ID derived by dividing frame ID by a 1000 and class ID
!>        assigned the frame ID anymore. In the current practice with
!>        multitude of TK frames with IDs set instrument IDs this
!>        default behavior is simply not valid.
!>
!>-    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB)
!>
!>        Bug fix: updated CNMFRM so a TK frame specified by name and
!>        designated as an object's preferred frame via kernel pool
!>        assignments is found, and so that the correct name of this
!>        frame is returned.
!>
!>-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!>
!>        Extended documentation of entry point CNMFRM and
!>        corrected example for that entry point.
!>
!>-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!>
!>        The entry points CIDFRM and CNMFRM were added so that
!>        user's may determine the frame-id and name to associated
!>        with a planetary object.
!>
!>-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!>
!>        The routine was upgraded to reflect that a block of
!>        frame ID codes have been reserved for use by the DSN.
!>        ID codes 13001 to 13999 have been set aside for DSN
!>        models for the orientation of the earth. These frames
!>        are all PCK frames. Moreover, the PCK ID code to
!>        use with these frames is simply the Frame-Code minus 10000.
!>        All of these frames are centered at the earth (body 399).
!>
!>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!>
!>        The values NINERT and NNINRT are included instead of
!>        being declared locally.
!>
!>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!>
!>### Implementation notes
!>
!>     Body name length. The value BDNMLN used here must be the
!>     same as the value of MAXL defined in the INCLUDE file
!>
!>        zzbodtrn.inc
!>
!>     Current value of MAXL = 36.
!>
!>     Frame name length.
!>
!>     Kernel variable name length.
!>
!>     Kernel variable buffer size.
!>
!>     POOL state counter.
!>
!>     Lower bound of collision lists in hashes.
!>
!>     The size of ID-based hash for kernel POOL frames.
!>
!>     Since defining a valid kernel POOL frame takes at least 5
!>     keywords and integer hash dimension must be a prime number, this
!>     size should be set to the first prime number greater than POOL's
!>     MAXVAR / 5 + 1.
!>
!>     For the current POOL MAXVAR set to 26003, such number is 5209.
!>
!>     Name-based hash for kernel pool frames. KNMLST, KNMPOL, and
!>     KNMNMS provide an index in the frame ID array KNMIDS at which the
!>     ID for the frame with a given name is stored.
!>
!>     ID-based hash for kernel pool frames. KIDLST, KIDPOL, and KIDIDS
!>     provide the index in the kernel frame attribute arrays KNAME,
!>     KCENT, KCLASS, and KCLSID at which the attributes of the frame
!>     with a given ID are stored.
!>
!>     The size of hashes for built-in frames.
!>
!>     Since integer hash dimension must be a prime number it cannot be
!>     computed in a parameter statement from the inertial and
!>     non-inertial frame counts provided in the include files. Instead
!>     it should be set manually to the first prime number greater than
!>     or equal to NCOUNT.
!>
!>     For the current N0067 NCOUNT equal to 145 (21 inertial + 124
!>     non-inertial), such number is 149.
!>
!>     Name-based hash for built-in frames. BNMLST, BNMPOL, and BNMNMS
!>     provide the index in BNMIDX which stores the index for the frame
!>     attributes in the built-in frame attributes arrays IDCODE, NAME,
!>     CENTER, TYPE, and TYPEID.
!>
!>     ID-based hash for built-in frames. BIDLST, BIDPOL, and BIDIDS
!>     provide an index in BIDIDX which stores the index for the frame
!>     attributes in the built-in frame attributes arrays IDCODE, NAME,
!>     CENTER, TYPE, and TYPEID.

submodule (pepper_instance_module) pepper_frames

   implicit none

contains

      !> Look up a frame definition kernel variable whose associated value
      !> is a body name or body ID code.  The returned value is always an
      !> ID code.  The frame name or frame ID may be used as part of the
      !> variable's name.
      !>
      !> If the kernel variable is not present, or if the variable
      !> is not a body name or a numeric value, signal an error.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.
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
      !>                       - a nbody ID code
      !>
      !>                       - a string representation of an integer,
      !>                         for example '5'
      !>
      !>                       - a body frame name
      !>
      !>## Detailed Output
      !>
      !>     IDCODE         is the requested body ID code.
      !>
      !>                    The kernel variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has numeric type, the associated
      !>                    value will be returned.  If this variable is
      !>                    found and has character type, the value will
      !>                    be converted to a body ID code, and that
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
      !>                    body ID code, and that code will be returned.
      !>
      !>                    If a numeric value associated with the selected
      !>                    kernel variable is not integral, it will be
      !>                    rounded to the closest integer.
      !>
      !>## Parameters
      !>
      !>     See zzdyn.inc for definition of KVNMLN.
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
      !>        that variable will not be searched for.
      !>
      !>     3) If both the frame-ID-based and frame-name-based forms of the
      !>        requested kernel variable name have length greater than KVNMLN,
      !>        the error SPICE(VARNAMETOOLONG) will be signaled.
      !>
      !>     4) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than 1
      !>        associated value, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
      !>
      !>     5) If a name match is found for a character kernel variable, but
      !>        the value associated with the variable cannot be mapped to a
      !>        body ID code, the error SPICE(NOTRANSLATION) will be
      !>        signaled.
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
      !>     access routines, these names are distinct.  Hence kernel
      !>     variables having names of both forms, but having possibly
      !>     different attributes, can be simultaneously present in the kernel
      !>     pool. Intentional use of this kernel pool feature is discouraged.
      !>
      !>## Examples
      !>
      !>     1) See ZZDYNFRM.
      !>
      !>     2) Applications of this routine include finding ID codes of
      !>        observer or target bodies serving to define two-vector dynamic
      !>        frames.
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
      !>-    SPICELIB Version 2.0.0, 05-AUG-2005 (NJB)
      !>
      !>        References to parameterized dynamic frames in long error
      !>        messages were changed to references to "reference frames."
      !>        This change was made to enable this utility to support
      !>        kernel variable look-ups for non-dynamic frames.
      !>
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 05-AUG-2005 (NJB)
      !>
      !>        References to parameterized dynamic frames in long error
      !>        messages were changed to references to "reference frames."
      !>        This change was made to enable this utility to support
      !>        kernel variable look-ups for non-dynamic frames.
      module subroutine zzdynbid ( ctx, frname, frcode, item, idcode )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, repmc, rtrim
      use pepper_inc_zzbodtrn
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(out) :: idcode  !! Body ID code.
      integer(ip) :: idcode_1(1)  !! one-element copy of idcode (array argument)

      character(:), allocatable :: bodnam
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
      call dynamic_frame_variable ( ctx, frname, frcode, item, .true., kvname, found, n, dtype, 'reference frame' )
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
         call ctx%pool_string ( kvname, 1_ip, bodnam, found )

         if ( .not. found ) then

            call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                          'indicated it was present in pool.'  )
            call ctx%errch  ( '#',  kvname                         )
            call ctx%sigerr ( 'SPICE(BUG)'                         )
            return

         end if

!
!        Convert the body name to a body code.
!
         call bods2c ( ctx, bodnam, idcode, found )

         if ( .not. found ) then

            call ctx%setmsg ( 'Body name # could not be translated ' // &
                          'to an ID code.'                       )
            call ctx%errch  ( '#',  bodnam                           )
            call ctx%sigerr ( 'SPICE(NOTRANSLATION)'                 )
            return

         end if

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

            call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                          'indicated it was present in pool.'  )
            call ctx%errch  ( '#',  kvname                         )
            call ctx%sigerr ( 'SPICE(BUG)'                         )
            return

         end if

      end if

      return
      end subroutine zzdynbid

      !> Look up array-valued integer frame kernel variable. The frame
      !> name or frame ID may be used as part of the variable's name.
      !>
      !> If the kernel variable is not present, or if the variable
      !> has the wrong data type, signal an error.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.
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
      !>## Detailed Output
      !>
      !>     N              is the number of values returned in the array
      !>                    VALUES.
      !>
      !>     VALUES         are the values associated with the requested
      !>                    array-valued, integer kernel variable. The kernel
      !>                    variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has numeric type, the associated
      !>                    values will be returned.  If this variable is
      !>                    not found, the variable
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    will be looked up.  If a numeric variable
      !>                    having that name is found, the associated
      !>                    values will be returned.
      !>
      !>## Parameters
      !>
      !>     See zzdyn.inc.
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
      !>        variable names is found, but that variable has character data
      !>        type, the error SPICE(BADVARIABLETYPE) will be signaled.
      !>
      !>     5) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than MAXN
      !>        associated values, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
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
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) A scalar-valued kernel variable matching the "ID code form"
      !>        of the requested kernel variable name could potentially
      !>        mask an array-valued kernel variable matching the "name
      !>        form" of the requested name.  This problem can be prevented
      !>        by sensible frame kernel design.
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
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
      module subroutine zzdynvai ( ctx, frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, repmc, rtrim
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(in) :: maxn  !! Maximum number of values to return.
      integer(ip), intent(out) :: n  !! Number of returned values.
      integer(ip), intent(out) :: values  ( * )  !! Output kernel variable.

      character(1) :: dtype
      character(:), allocatable :: kvname

      logical :: found

!
!     Standard SPICE error handling
!
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
!     Rather than using BADKPV, we check the data type and cardinality
!     of the kernel variable in-line so we can create a more detailed
!     error message if need be.
!
      if ( dtype == 'C' ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have integer '      // &
                       'data type but in fact has '                 // &
                       'character type.  Usually this type of '     // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)'                     )
         return

      end if

      if ( n > maxn ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%errint ( '#',  maxn                                   )
         call ctx%errint ( '#',  n                                      )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                     )
         return

      end if

!
!     Look up the kernel variable.
!
      call gipool ( ctx, kvname, 1, maxn, n, values, found )

      if ( .not. found ) then

         call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                       'indicated it was present in pool.'  )
         call ctx%errch  ( '#',  kvname                         )
         call ctx%sigerr ( 'SPICE(BUG)'                         )
         return

      end if

      return
      end subroutine zzdynvai

      !> Look up optional array-valued character frame kernel variable.
      !> The frame name or frame ID may be used as part of the variable's
      !> name.
      !>
      !> If the kernel variable is not present, or if the variable
      !> has the wrong data type or size, set the FOUND flag to .FALSE.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.
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
      !>                    this routine will look for a character variable
      !>                    of that name first.
      !>
      !>## Detailed Output
      !>
      !>     N              is the number of values returned in the array
      !>                    VALUES.
      !>
      !>     VALUES         are the values associated with the requested
      !>                    array-valued, character kernel variable. The
      !>                    kernel variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has character type, the associated
      !>                    values will be returned.  If this variable is
      !>                    not found, the variable
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    will be looked up.  If a character variable
      !>                    having that name is found, the associated
      !>                    values will be returned.
      !>
      !>                    VALUES is not defined if the requested kernel
      !>                    variable is not found.
      !>
      !>     FOUND          is a logical flag indicating whether the requested
      !>                    kernel variable was found.  If the search described
      !>                    above (in the detailed description of the output
      !>                    argument VALUES) is successful, FOUND is set to
      !>                    .TRUE.; otherwise FOUND is set to .FALSE.
      !>
      !>## Parameters
      !>
      !>     See zzdyn.inc.
      !>
      !>## Exceptions
      !>
      !>     1) If both the frame-ID-based and frame-name-based forms of the
      !>        requested kernel variable name have length greater than KVNMLN,
      !>        the error SPICE(VARNAMETOOLONG) will be signaled.
      !>
      !>     2) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has numeric data
      !>        type, the error SPICE(BADVARIABLETYPE) will be signaled.
      !>
      !>     3) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than MAXN
      !>        associated values, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
      !>
      !>     4) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than MAXN
      !>        associated values, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
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
      !>     must be performed by the SPICELIB frame subsystem.  This routine
      !>     is meant to look up array-valued character variables whose
      !>     presence is optional.  For required array character variables,
      !>     use ZZDYNVAC.
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
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) A scalar-valued kernel variable matching the "ID code form"
      !>        of the requested kernel variable name could potentially mask a
      !>        array-valued kernel variable matching the "name form" of the
      !>        requested name.  This problem can be prevented by sensible
      !>        frame kernel design.
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
      module subroutine zzdynoac ( ctx, frname, frcode, item, maxn, n, values, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, repmc, rtrim
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(in) :: maxn  !! Maximum number of values to return.
      integer(ip), intent(out) :: n  !! Number of returned values.
      character(*), intent(out) :: values  ( * )  !! Output kernel variable.
      logical, intent(out) :: found  !! "Found" flag.

      character(1) :: dtype
      character(:), allocatable :: kvname

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) return

!
!     Nothing found yet.
!
      found  = .false.

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
      call dynamic_frame_variable ( ctx, frname, frcode, item, .false., kvname, found, n, dtype )
      if ( ctx%failed() ) return
      if ( .not. found ) return

!
!     Getting to this point means we found a kernel variable. The name
!     of the variable is KVNAME.  The data type is DTYPE and the
!     cardinality is N.
!
!     Rather than using BADKPV, we check the data type and cardinality
!     of the kernel variable in-line so we can create a more detailed
!     error message if need be.
!
      if ( dtype == 'N' ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have character '    // &
                       'data type but in fact has numeric '         // &
                       'data type.  Usually this type of '          // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)'                     )
         return

      end if

      if ( n > maxn ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%errint ( '#',  maxn                                   )
         call ctx%errint ( '#',  n                                      )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                     )
         return

      end if

!
!     Look up the kernel variable.
!
      call gcpool ( ctx, kvname, 1, maxn, n, values, found )

      if ( .not. found ) then

         call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                       'indicated it was present in pool.'  )
         call ctx%errch  ( '#',  kvname                         )
         call ctx%sigerr ( 'SPICE(BUG)'                         )
         return

      end if

      return
      end subroutine zzdynoac

      !> Look up optional array-valued double precision frame kernel
      !> variable. The frame name or frame ID may be used as part of the
      !> variable's name.
      !>
      !> If the kernel variable is not present, or if the variable
      !> has the wrong data type, set the FOUND flag to .FALSE.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.
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
      !>## Detailed Output
      !>
      !>     N              is the number of values returned in the array
      !>                    VALUES.
      !>
      !>     VALUES         are the values associated with the requested
      !>                    array-valued, double precision kernel variable.
      !>                    The kernel variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has numeric type, the associated
      !>                    values will be returned.  If this variable is
      !>                    not found, the variable
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    will be looked up.  If a numeric variable
      !>                    having that name is found, the associated
      !>                    values will be returned.
      !>
      !>                    VALUES is not defined if the requested kernel
      !>                    variable is not found.
      !>
      !>     FOUND          is a logical flag indicating whether the requested
      !>                    kernel variable was found.  If the search described
      !>                    above (in the detailed description of the output
      !>                    argument VALUES) is successful, FOUND is set to
      !>                    .TRUE.; otherwise FOUND is set to .FALSE.
      !>
      !>## Parameters
      !>
      !>     See zzdyn.inc.
      !>
      !>## Exceptions
      !>
      !>     1) If both the frame-ID-based and frame-name-based forms of the
      !>        requested kernel variable name have length greater than KVNMLN,
      !>        the error SPICE(VARNAMETOOLONG) will be signaled.
      !>
      !>     2) If either the frame-ID-based or frame-name-based form of the
      !>        requested kernel variable name has length greater than KVNMLN,
      !>        the excessively long name will not be searched for. A search
      !>        will still be done using the alternative form of the name if
      !>        that form has length less than or equal to KVNMLN.
      !>
      !>     3) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has character data
      !>        type, the error SPICE(BADVARIABLETYPE) will be signaled.
      !>
      !>     4) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than MAXN
      !>        associated values, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
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
      !>     must be performed by the SPICELIB frame subsystem. This routine
      !>     is meant to look up array-valued double precision variables whose
      !>     presence is optional.
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
      !>     data structure, these names are distinct.  Hence kernel
      !>     variables having names of both forms, but having possibly
      !>     different attributes, can be simultaneously present in the kernel
      !>     pool. Intentional use of this kernel pool feature is discouraged.
      !>
      !>## Examples
      !>
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) A scalar-valued kernel variable matching the "ID code form"
      !>        of the requested kernel variable name could potentially
      !>        mask an array-valued kernel variable matching the "name
      !>        form" of the requested name.  This problem can be prevented
      !>        by sensible frame kernel design.
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
      !>-    SPICELIB Version 1.0.0, 16-DEC-2004 (NJB)
      module subroutine zzdynoad ( ctx, frname, frcode, item, maxn, n, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: intstr, repmc, rtrim
      use pepper_inc_zzbodtrn
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(in) :: maxn  !! Maximum number of values to return.
      integer(ip), intent(out) :: n  !! Number of returned values.
      real(wp), intent(out) :: values  ( * )  !! Output kernel variable.
      logical, intent(out) :: found  !! "Found" flag.

      character(1) :: dtype
      character(:), allocatable :: kvname

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) return

!
!     Nothing found yet.
!
      found  = .false.

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
      call dynamic_frame_variable ( ctx, frname, frcode, item, .false., kvname, found, n, dtype )
      if ( ctx%failed() ) return
      if ( .not. found ) return

!
!     Getting to this point means we found a kernel variable. The name
!     of the variable is KVNAME.  The data type is DTYPE and the
!     cardinality is N.
!
!     Rather than using BADKPV, we check the data type and cardinality
!     of the kernel variable in-line so we can create a more detailed
!     error message if need be.
!
      if ( dtype == 'C' ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have double '       // &
                       'precision data type but in fact has  '      // &
                       'character type.  Usually this type of '     // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)'                     )
         return

      end if

      if ( n > maxn ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%errint ( '#',  maxn                                   )
         call ctx%errint ( '#',  n                                      )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                     )
         return

      end if

!
!     Look up the kernel variable.
!
      call gdpool ( ctx, kvname, 1, maxn, n, values, found )

      if ( .not. found ) then

         call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                       'indicated it was present in pool.'  )
         call ctx%errch  ( '#',  kvname                         )
         call ctx%sigerr ( 'SPICE(BUG)'                         )
         return

      end if

      return
      end subroutine zzdynoad

      !> Look up array-valued character frame kernel variable.  The frame
      !> name or frame ID may be used as part of the variable's name.
      !>
      !> If the kernel variable is not present, or if the variable
      !> has the wrong data type, signal an error.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.
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
      !>                    this routine will look for a character variable
      !>                    of that name first.
      !>
      !>## Detailed Output
      !>
      !>     N              is the number of values returned in the array
      !>                    VALUES.
      !>
      !>     VALUES         are the values associated with the requested
      !>                    array-valued, character kernel variable. The
      !>                    kernel variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has character type, the associated
      !>                    values will be returned.  If this variable is
      !>                    not found, the variable
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    will be looked up.  If a character variable
      !>                    having that name is found, the associated
      !>                    values will be returned.
      !>
      !>## Parameters
      !>
      !>     See zzdyn.inc.
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
      !>        variable names is found, but that variable has numeric data
      !>        type, the error SPICE(BADVARIABLETYPE) will be signaled.
      !>
      !>     5) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than MAXN
      !>        associated values, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
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
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) A scalar-valued kernel variable matching the "ID code form"
      !>        of the requested kernel variable name could potentially
      !>        mask an array-valued kernel variable matching the "name
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
      module subroutine zzdynvac ( ctx, frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, repmc, rtrim
      use pepper_inc_zzdyn

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(in) :: maxn  !! Maximum number of values to return.
      integer(ip), intent(out) :: n  !! Number of returned values.
      character(*), intent(out) :: values  ( * )  !! Output kernel variable.
      type(string), allocatable :: strs(:)
      integer(ip) :: i
!
!     PEPPER: the body is DYNAMIC_FRAME_STRINGS, which returns the values
!     at full length; they are copied into VALUES.
!
      call dynamic_frame_strings ( ctx, frname, frcode, item, maxn, n, strs )
      if ( ctx%failed() ) return
      do i = 1, n
         values(i) = strs(i)%s
      end do
      return
      end subroutine zzdynvac

      !> Look up array-valued double precision frame kernel variable.
      !> The frame name or frame ID may be used as part of the variable's
      !> name.
      !>
      !> If the kernel variable is not present, or if the variable
      !> has the wrong data type, signal an error.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME         is the name of the reference frame with which
      !>                    the requested variable is associated.
      !>
      !>     FRCODE         is the frame ID code of the reference frame with
      !>                    which the requested variable is associated.
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
      !>## Detailed Output
      !>
      !>     N              is the number of values returned in the array
      !>                    VALUES.
      !>
      !>     VALUES         are the values associated with the requested
      !>                    array-valued, double precision kernel variable.
      !>                    The kernel variable name of the form
      !>
      !>                       FRAME_<frame ID code>_<ITEM>
      !>
      !>                    will be looked up first; if this variable
      !>                    is found and has numeric type, the associated
      !>                    values will be returned.  If this variable is
      !>                    not found, the variable
      !>
      !>                       FRAME_<frame name>_<ITEM>
      !>
      !>                    will be looked up.  If a numeric variable
      !>                    having that name is found, the associated
      !>                    values will be returned.
      !>
      !>## Parameters
      !>
      !>     See zzdyn.inc.
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
      !>        variable names is found, but that variable has character data
      !>        type, the error SPICE(BADVARIABLETYPE) will be signaled.
      !>
      !>     5) If kernel variable matching one form of the requested kernel
      !>        variable names is found, but that variable has more than MAXN
      !>        associated values, the error SPICE(BADVARIABLESIZE) will be
      !>        signaled.
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
      !>     See ZZDYNFRM.
      !>
      !>## Restrictions
      !>
      !>     1) This is a SPICE private routine; the routine is subject
      !>        to change without notice.  User applications should not
      !>        call this routine.
      !>
      !>     2) A scalar-valued kernel variable matching the "ID code form"
      !>        of the requested kernel variable name could potentially
      !>        mask an array-valued kernel variable matching the "name
      !>        form" of the requested name.  This problem can be prevented
      !>        by sensible frame kernel design.
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
      !>-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
      module subroutine zzdynvad ( ctx, frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: intstr, repmc, rtrim
      use pepper_inc_zzdyn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! Frame name.
      integer(ip), intent(in) :: frcode  !! Frame ID code.
      character(*), intent(in) :: item  !! Item associated with frame definition.
      integer(ip), intent(in) :: maxn  !! Maximum number of values to return.
      integer(ip), intent(out) :: n  !! Number of returned values.
      real(wp), intent(out) :: values  ( * )  !! Output kernel variable.

      character(1) :: dtype
      character(:), allocatable :: kvname

      logical :: found

!
!     Standard SPICE error handling
!
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
!     Rather than using BADKPV, we check the data type and cardinality
!     of the kernel variable in-line so we can create a more detailed
!     error message if need be.
!
      if ( dtype == 'C' ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have double '       // &
                       'precision data type but in fact has  '      // &
                       'character type.  Usually this type of '     // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)'                     )
         return

      end if

      if ( n > maxn ) then

         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%errint ( '#',  maxn                                   )
         call ctx%errint ( '#',  n                                      )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                     )
         return

      end if

!
!     Look up the kernel variable.
!
      call gdpool ( ctx, kvname, 1, maxn, n, values, found )

      if ( .not. found ) then

         call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                       'indicated it was present in pool.'  )
         call ctx%errch  ( '#',  kvname                         )
         call ctx%sigerr ( 'SPICE(BUG)'                         )
         return

      end if

      return
      end subroutine zzdynvad

      !> Perform partial data structure clean up in response to failures
      !> detected in the switch frame data fetch routine ZZSWFFET.
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
      !>     HDFRAM      is the initialized head node array.
      !>
      !>     FRPOOL      is the initialized hash collision list pool.
      !>
      !>     BASBEG      is the initialized base frame start index list.
      !>
      !>     FRAMAT      is the initialized frame ID index. This index is
      !>                 set to zero, which is never valid.
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
      !>     1)  If integer hash initialization fails, the error will be
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is essentially a macro that centralizes
      !>     initialization of bookkeeping data structures maintained by
      !>     ZZSWFFET. Only those initializations that are required by
      !>     ZZSWFFET are performed. See ZZSWFINI for the complete set of
      !>     initialization actions.
      !>
      !>## Examples
      !>
      !>     None. See usage in ZZSWFFET.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine is SPICE-private. User applications must not
      !>         call it.
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
      !>-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB)
      module subroutine zzswfcln ( ctx, hdfram, frpool, basbeg, framat )
      use pepper_kinds, only: ip
      use pepper_inc_zzswtchf
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: hdfram ( * )  !! Array of hash collision list head nodes.
      integer(ip), intent(out) :: frpool ( lbsngl : * )  !! Hash collision list pool.
      integer(ip), intent(out) :: basbeg ( * )  !! Start indices of base lists of switch frames.
      integer(ip), intent(out) :: framat  !! Switch frame index in switch frame ID array.

!
!     This routine must perform its function after an error has been
!     signaled, so it does not return upon entry after a SPICE error
!     occurs.
!

      framat = 0

!
!     Both of the following routines will execute even when a SPICE
!     error condition exists.
!
      basbeg(1:maxfrm) = 0
      call zzhsiini ( ctx, maxfrm, hdfram, frpool )

      return
      end subroutine zzswfcln

      !> Initialize switch frame bookkeeping data structures.
      !>
      !> Arrays of base frame attributes are not initialized by this
      !> routine.
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
      !>     HDFRAM      is the initialized head node array.
      !>
      !>     FRPOOL      is the initialized hash collision list pool.
      !>
      !>     FIDLST      is the initialized switch frame ID list.
      !>
      !>     BASBEG      is the initialized base frame start index list.
      !>
      !>     FREE        is the common index of the first free entries in the
      !>                 arrays of base frame attributes. The index is set to
      !>                 1.
      !>
      !>     PRVAT       is the index of the previous ZZSWFXFM input frame
      !>                 in the frame list.
      !>
      !>     PRVFRM      is the frame ID of the previous ZZSWFXFM input frame.
      !>
      !>     SAMFRM      is a logical flag indicating whether the current
      !>                 ZZSWFXFM input frame matches the previous one.
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
      !>     1)  If integer hash initialization fails, the error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine centralizes initialization of bookkeeping data
      !>     structures maintained by ZZSWFXFM.
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
      !>-    SPICELIB Version 1.0.0, 15-DEC-2021 (NJB)
      module subroutine zzswfini ( ctx, hdfram, frpool, fidlst, basbeg, free, prvat, prvfrm, samfrm )
      use pepper_kinds, only: ip
      use pepper_inc_zzswtchf
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: hdfram ( * )  !! Array of hash collision list head nodes.
      integer(ip), intent(out) :: frpool ( lbsngl : * )  !! Hash collision list pool.
      integer(ip), intent(out) :: fidlst ( * )  !! Frame IDs corresponding to list nodes.
      integer(ip), intent(out) :: basbeg ( * )  !! Start indices of base lists of switch frames.
      integer(ip), intent(out) :: free  !! First free index in base and time lists.
      integer(ip), intent(out) :: prvat  !! Index of previous input frame in frame list.
      integer(ip), intent(out) :: prvfrm  !! Previous input frame ID.
      logical, intent(out) :: samfrm  !! Flag indicating input frame matches previous one.

      if ( ctx%failed() ) return

!
!     Initialize the frame hash head node array, hash collision list
!     pool, and parallel frame ID list. Initialize the array of start
!     indices of associated base frame information. Indicate the base
!     frame data buffers are empty by setting FREE to 1.
!
      call zzhsiini ( ctx, maxfrm, hdfram, frpool )

      if ( ctx%failed() ) then
!
!        This code should be unreachable. It's provided for safety.
!
         return

      end if

      fidlst(1:maxfrm) = 0
      basbeg(1:maxfrm) = 0

      free = 1

!
!     Initialize all saved frame identity information.
!
      prvfrm = 0
      prvat  = 0
      samfrm = .false.

      return
      end subroutine zzswfini

      !> Return the values of the triaxial radii for any body in the
      !> kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     BODY     is the NAIF ID code of the body for which the axes are
      !>              requested. Bodies are numbered according to the
      !>              standard NAIF numbering scheme described in the
      !>              required reading (naif_ids.req) document.
      !>
      !>## Detailed Output
      !>
      !>     AXES     are the lengths of the axes of the body, in km.
      !>
      !>                    AXES(1)  is the longest equatorial radius of
      !>                             the body. For satellites, this axis is
      !>                             typically pointed toward the primary
      !>                             planet.
      !>
      !>                    AXES(2)  is the shortest equatorial radius of
      !>                             the body.
      !>
      !>                    AXES(3)  is the polar radius of the body.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the RADII kernel pool data for body specified does not have
      !>         exactly 3 axes defined, the error SPICE(INVALIDCOUNT) is
      !>         signaled.
      !>
      !>     2)  If the RADII kernel pool data for body specified contains any
      !>         zero or negative values, the error SPICE(BADAXISLENGTH) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     PCK data: triaxial radii for the target body must be loaded
      !>     into the kernel pool. Typically this is done by loading a
      !>     text PCK file via LDPOOL or a general kernel loader
      !>     such as FURNSH.
      !>
      !>## Particulars
      !>
      !>     ZZGFTREB returns the lengths of the axes of BODY. It serves
      !>     as a wrapper to the kernel data access call, the confirms
      !>     return of reasonable values.
      !>
      !>     Appropriate SPK and PCK data must be available to the calling
      !>     program before this routine is called.for missing kernel data
      !>     or non physical values.
      !>
      !>## Examples
      !>
      !>     The call
      !>
      !>         CALL ZZGFTREB ( 399,  VALUE )
      !>
      !>     returns the values associated with the variable 'BODY399_RADII',
      !>     for example,
      !>
      !>          VALUE(1) = 6378.140
      !>          VALUE(2) = 6378.140
      !>          VALUE(3) = 6356.755
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  "Report of the IAU/IAG/COSPAR Working Group on Cartographic
      !>          Coordinates and Rotational Elements of the Planets and
      !>          Satellites: 1991", March 3, 1992.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 11-SEP-2021 (EDW) (JDR)
      !>
      !>        Corrected short error messages token string. Axes length
      !>        error is signaled on less-than or equal-to zero.
      !>
      !>        Edited the header to comply with NAIF standard. Moved SPK
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 05-MAR-2003 (EDW)
      module subroutine zzgftreb ( ctx, body, axes )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! NAIF ID code of body.
      real(wp), intent(out) :: axes ( 3 )  !! Length of axes of body (1,2,3, as defined below).

      integer(ip) :: i
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Look up BODY radii in the kernel pool.
!
      call bodvcd ( ctx, body, 'RADII', 3, n, axes )

      if ( ctx%failed() ) return

      if ( n /= 3 ) then

         call ctx%setmsg ( 'Only # axes were found  ' &
              //       'for ID #. Three axes expected.'    )
         call ctx%errint ( '#',   n                            )
         call ctx%errint ( '#',   body                         )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'               )
         return

      else

         do i = 1, 3

           if ( axes(i) <= 0.0_wp ) then

               call ctx%setmsg ('Degenerate case. The # axis of body # ' &
               //           'is negative or zero.  Please check ' &
               //           'the text PCK file. You should fix the ' &
               //           '# component of the kernel pool ' &
               //           'variable  BODY#_RADII. '          )

               call ctx%errint ( '#', i                            )
               call ctx%errint ( '#', body                         )
               call ctx%errint ( '#', i                            )
               call ctx%errint ( '#', body                         )
               call ctx%sigerr ( 'SPICE(BADAXISLENGTH)'            )
               return

           end if

         end do

      end if

      return
      end subroutine zzgftreb

      !> Initialize the table of built-in frame names and their ID codes
      !> and hashes and order arrays providing efficient access to these
      !> tables.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NCOUNT   is the number of names that the calling routine
      !>              expects to receive. It should have the value of
      !>              NNAMES which is given below for NNAMES. If this is
      !>              not the case then the error SPICE(VERSIONMISMATCH1)
      !>              is signaled.
      !>
      !>     MAXBFR   is the build-in frame hash size. It should have a
      !>              value greater than or equal to the value of NNAMES.
      !>              If this is not the case then the error
      !>              SPICE(VERSIONMISMATCH2) is signaled.
      !>
      !>              If everything has been properly called, compiled and
      !>              linked these errors should never be signaled. If
      !>              either is signaled, it indicates that either a
      !>              calling sequence, or version mismatch has occurred.
      !>
      !>## Detailed Output
      !>
      !>     The six arrays described below (NAME...CORDER) should be declared
      !>     with the same dimensions --- NCOUNT.
      !>
      !>     NAME     is an array of the official SPICE names for the
      !>              recognized frames (both inertial and non-inertial)
      !>
      !>     IDCODE   is an array parallel to NAME of SPICE ID codes for
      !>              the various frames.
      !>
      !>     CENTER   is an array parallel to NAME of body ID codes for
      !>              the centers of frames.
      !>
      !>     TYPE     is an array parallel to NAME of inertial frame types
      !>              for the various frames. These include INERTL, PCK,
      !>              CK, etc.
      !>
      !>     TYPID    is an array parallel to NAME of the ID code for the
      !>              frame within the TYPE of the frame. Once the class
      !>              of the frame has been identified by TYPE, TYPID is
      !>              used to access the information specific about this
      !>              frame.
      !>
      !>     CENTRD   is an order vector for the array CENTER. The value
      !>              CENTER(CORDER(I)) is the I'th IDCODE when ordered
      !>              from smallest to largest.
      !>
      !>     The eight hash array below (BMM*, BID*) should be declared with
      !>     the same upper dimensions --- MAXBFR.
      !>
      !>     BNMLST,
      !>     BNMPOL,
      !>     BNMNMS   are the frame name-based hash head node pointer, node
      !>              collision, and item lists. Together they return the
      !>              index of the element in the BNMIDX index storage
      !>              array corresponding to a given built-in frame name.
      !>
      !>     BNMIDX   is the frame name-based hash index storage array
      !>              containing at the index determined by the hash for a
      !>              given name the index corresponding to the same name
      !>              in the NAME, IDCODE, CENTER, TYPE, and TYPID arrays.
      !>
      !>     BIDLST,
      !>     BIDPOL,
      !>     BIDIDS   are the frame ID-based hash head node pointer, node
      !>              collision, and item lists. Together they return the
      !>              index of the element in the BNMIDX index storage
      !>              array corresponding to a given built-in frame ID.
      !>
      !>     BIDIDX   is the frame ID-based hash index storage array
      !>              containing at the index determined by the hash for a
      !>              given ID the index corresponding to the same ID
      !>              in the NAME, IDCODE, CENTER, TYPE, and TYPID arrays.
      !>
      !>## Parameters
      !>
      !>     See include files.
      !>
      !>## Exceptions
      !>
      !>     If everything has been properly called, compiled and linked these
      !>     errors should never be signaled. If either is signaled, it
      !>     indicates that either a calling sequence, or version mismatch has
      !>     occurred.
      !>
      !>     1)  If the input number of frames NCOUNT is not equal to the
      !>         number of frames determined from include files, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>     2)  If the input hash size MAXBFR is not greater or equal to the
      !>         number of frames determined from include files, the error
      !>         SPICE(BUG2) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine establishes the built-in SPICE reference frames and
      !>     their ID-codes. In addition it returns the order vector for the
      !>     frame center IDs and hashes for the frame names and IDs.
      !>
      !>     This is a private routine intended solely as a support routine
      !>     for the SPICE routine FRAMEX.
      !>
      !>## Examples
      !>
      !>     This routine should typically be called as part of an
      !>     initialization portion of FRAMEX entry points.
      !>
      !>        LOGICAL               FIRST
      !>        SAVE                  FIRST
      !>
      !>        DATA                  FIRST / .TRUE. /
      !>
      !>        IF ( FIRST ) THEN
      !>
      !>           FIRST = .FALSE.
      !>           CALL ZZFDAT ( NCOUNT, MAXBFR, NAME,   IDCODE,
      !>       .                 CENTER, TYPE,   TYPID,  CENTRD,
      !>       .                 BNMLST, BNMPOL, BNMNMS, BNMIDX,
      !>       .                 BIDLST, BIDPOL, BIDIDS, BIDIDX )
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.2.0, 07-SEP-2021 (BVS) (JDR)
      !>
      !>        Added the following PCK frames:
      !>
      !>           IAU_52_EUROPA
      !>           IAU_NIX
      !>           IAU_HYDRA
      !>           IAU_RYUGU
      !>           IAU_ARROKOTH
      !>           IAU_DIDYMOS_BARYCENTER
      !>           IAU_DIDYMOS
      !>           IAU_DIMORPHOS
      !>           IAU_DONALDJOHANSON
      !>           IAU_EURYBATES
      !>           IAU_EURYBATES_BARYCENTER
      !>           IAU_QUETA
      !>           IAU_POLYMELE
      !>           IAU_LEUCUS
      !>           IAU_ORUS
      !>           IAU_PATROCLUS_BARYCENTER
      !>           IAU_PATROCLUS
      !>           IAU_MENOETIUS
      !>
      !>        Corrected frame name spelling:
      !>
      !>           IAU_MAGACLITE -> IAU_MEGACLITE
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        $Index_Entries.
      !>
      !>-    SPICELIB Version 5.1.0, 30-OCT-2014 (BVS)
      !>
      !>        Added the following PCK frame:
      !>
      !>           IAU_BENNU
      !>
      !>-    SPICELIB Version 5.0.0, 08-AUG-2013 (BVS)
      !>
      !>        Changed routine's calling sequence by dropping name and ID
      !>        order vectors and adding name- and ID-based hashes and
      !>        modified it to initialize hashes instead of the order arrays.
      !>
      !>-    SPICELIB Version 4.3.0, 11-OCT-2011 (BVS)
      !>
      !>        Added the following PCK frames:
      !>
      !>           IAU_CERES
      !>           IAU_PALLAS
      !>           IAU_LUTETIA
      !>           IAU_DAVIDA
      !>           IAU_STEINS
      !>
      !>-    SPICELIB Version 4.2.0, 11-MAY-2010 (BVS)
      !>
      !>        Added the following PCK frames:
      !>
      !>           IAU_BORRELLY
      !>           IAU_TEMPEL_1
      !>           IAU_VESTA
      !>           IAU_ITOKAWA
      !>
      !>-    SPICELIB Version 4.1.0, 12-DEC-2002 (BVS)
      !>
      !>        Added PCK frames for new Jovian satellites:
      !>
      !>           IAU_CALLIRRHOE
      !>           IAU_THEMISTO
      !>           IAU_MAGACLITE
      !>           IAU_TAYGETE
      !>           IAU_CHALDENE
      !>           IAU_HARPALYKE
      !>           IAU_KALYKE
      !>           IAU_IOCASTE
      !>           IAU_ERINOME
      !>           IAU_ISONOE
      !>           IAU_PRAXIDIKE
      !>
      !>-    SPICELIB Version 4.0.1, 18-OCT-2002 (EDW)
      !>
      !>        Corrected the erroneous frame values for IAU_PAN.
      !>        Minor edits to the header.
      !>
      !>-    SPICELIB Version 4.0.0, 02-AUG-2002 (FST)
      !>
      !>        The frames IAU_PAN, IAU_GASPRA, IAU_IDA, and IAU_EROS
      !>        were added to the list of recognized frames.
      !>
      !>-    SPICELIB Version 3.1.1, 20-APR-1999 (WLT)
      !>
      !>        Changed the variable name TYPEID to TYPID in the calling
      !>        sequence to avoid having to take special measures in the f2c
      !>        conversion process.
      !>
      !>-    SPICELIB Version 3.1.0, 11-SEP-1997 (WLT)
      !>
      !>        The error condition check early in the routine
      !>        did not use the exception handling subsystem correctly.
      !>        This has been fixed.
      !>
      !>-    SPICELIB Version 3.0.0, 02-JUN-1997 (WLT)
      !>
      !>        The calling sequence changed. ZZFDAT now also returns
      !>        an order vector for the CENTERs of the frames.
      !>
      !>-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
      !>
      !>        The frames ITRF93 and EARTH_FIXED were added to the
      !>        list of recognized frames.
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
      !>
      !>        Changed declarations so that the variables NINERT and
      !>        NNINRT are included instead of being declared locally.
      !>
      !>-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
      module subroutine zzfdat ( ctx, ncount, maxbfr, name, idcode, center, type, typid, centrd, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, bidids, bididx )
      use pepper_kinds, only: ip
      use pepper_arrays, only: orderi
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_ninert
      use pepper_inc_nninrt
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ncount  !! Input checking variable.
      integer(ip), intent(in) :: maxbfr  !! build-in frame hash size.
      character(*), intent(out) :: name   (          * )  !! array containing the names of all built-in frames
      integer(ip), intent(out) :: idcode (          * )  !! array containing the IDs of all built-in frames
      integer(ip), intent(out) :: center (          * )  !! array containing the centers of the built-in frames
      integer(ip), intent(out) :: type   (          * )  !! array containing the types of the built-in frames
      integer(ip), intent(out) :: typid  (          * )  !! array containing the subtype id
      integer(ip), intent(out) :: centrd (          * )  !! an order vector for CENTER
      integer(ip), intent(out) :: bnmlst (          * )  !! frame name-based hash head node pointer list
      integer(ip), intent(out) :: bnmpol ( lbpool : * )  !! frame name-based hash node collision list
      character(*), intent(out) :: bnmnms (          * )  !! frame name-based hash item list
      integer(ip), intent(out) :: bnmidx (          * )  !! frame name-based hash index storage array
      integer(ip), intent(out) :: bidlst (          * )  !! frame ID-based hash head node pointer list
      integer(ip), intent(out) :: bidpol ( lbpool : * )  !! frame ID-based hash node collision list
      integer(ip), intent(out) :: bidids (          * )  !! frame ID-based hash item list
      integer(ip), intent(out) :: bididx (          * )  !! frame ID-based hash index storage array


      integer(ip), parameter :: non = nninrt

      integer(ip), parameter :: nnames = ninert + non

!
!     To add to the list of recognized frames,
!
!     1. Determine whether or not the frame is inertial.
!
!        Inertial Case.
!
!        A. Be sure that the routine CHGIRF has been modified to
!           reflect the new frame and set NINERT (above) equal to
!           the number of recognized inertial frames give by CHGIRF.
!
!        Non Inertial Case.
!
!        A. Locate the last non-inertial frame in the lengthy list
!           below.
!
!        B. Add the frame name to the array NAME.  Add the IDCODE
!           to the array IDCODE. (Unless there is a compelling reason
!           to do otherwise this should just be the next integer in
!           the sequence of ID codes.  The mixture of old and new code
!           should look something like this:
!
!              Last bit of old assignments
!
!                 NAME   ( NINERT + NON ) = last name in the old routine
!                 IDCODE ( NINERT + NON ) = 10000 + NON
!
!              Your new assignment
!
!                 NAME   ( NINERT + NEXT ) = your name
!                 IDCODE ( NINERT + NEXT ) = 10000 + NEXT
!
!           where
!
!              NON  = the value of the parameter above
!              NEXT = NON + 1
!
!        C. Modify the value of the parameter NON above to reflect the
!           new number of non-inertial frames.
!
!     2. Update the version and date routine.
!
!     3. Update the routines that call this routine so that they
!        will be expecting the correct number of names and ID codes
!        to be returned.
!

!
!     Local variables.
!

      integer(ip) :: i
      integer(ip) :: item

      logical :: new

!
!     Perform the consistency checks first.
!
      if ( ncount /= nnames ) then
         call ctx%setmsg( 'There is an inconsistency between the ' &
         //           'version of the routine calling ZZFDAT and ' &
         //           'the current version of ZZFDAT. Check to ' &
         //           'make sure that you have the most current ' &
         //           'versions of ZZFDAT and the routines that ' &
         //           'make use of it.' )
         call ctx%sigerr ( 'SPICE(VERSIONMISMATCH1)'  )
         return
      end if

      if ( maxbfr < nnames ) then
         call ctx%setmsg( 'There is an inconsistency between the ' &
         //           'version of the routine calling ZZFDAT and ' &
         //           'the current version of ZZFDAT. Check to ' &
         //           'make sure that you have the most current ' &
         //           'versions of ZZFDAT and the routines that ' &
         //           'make use of it.' )
         call ctx%sigerr ( 'SPICE(VERSIONMISMATCH2)' )
         return
      end if

!
!     Inertial Frames Section
!
!     Fetch the names of the inertial frames from CHGIRF.
!
      do i = 1, ninert
         idcode( i ) = i
         center( i ) = 0
         type  ( i ) = inertl
         typid ( i ) = i
         call irfnam ( ctx, i, name(i) )
      end do

!
!     Non-Inertial Frames Section.
!
      name   ( ninert +   1 ) = 'IAU_MERCURY_BARYCENTER'
      idcode ( ninert +   1 ) =  10001
      center ( ninert +   1 ) =  1
      typid  ( ninert +   1 ) =  1
      type   ( ninert +   1 ) =  pck

      name   ( ninert +   2 ) = 'IAU_VENUS_BARYCENTER'
      idcode ( ninert +   2 ) =  10002
      center ( ninert +   2 ) =  2
      typid  ( ninert +   2 ) =  2
      type   ( ninert +   2 ) =  pck

      name   ( ninert +   3 ) = 'IAU_EARTH_BARYCENTER'
      idcode ( ninert +   3 ) =  10003
      center ( ninert +   3 ) =  3
      typid  ( ninert +   3 ) =  3
      type   ( ninert +   3 ) =  pck

      name   ( ninert +   4 ) = 'IAU_MARS_BARYCENTER'
      idcode ( ninert +   4 ) =  10004
      center ( ninert +   4 ) =  4
      typid  ( ninert +   4 ) =  4
      type   ( ninert +   4 ) =  pck

      name   ( ninert +   5 ) = 'IAU_JUPITER_BARYCENTER'
      idcode ( ninert +   5 ) =  10005
      center ( ninert +   5 ) =  5
      typid  ( ninert +   5 ) =  5
      type   ( ninert +   5 ) =  pck

      name   ( ninert +   6 ) = 'IAU_SATURN_BARYCENTER'
      idcode ( ninert +   6 ) =  10006
      center ( ninert +   6 ) =  6
      typid  ( ninert +   6 ) =  6
      type   ( ninert +   6 ) =  pck

      name   ( ninert +   7 ) = 'IAU_URANUS_BARYCENTER'
      idcode ( ninert +   7 ) =  10007
      center ( ninert +   7 ) =  7
      typid  ( ninert +   7 ) =  7
      type   ( ninert +   7 ) =  pck

      name   ( ninert +   8 ) = 'IAU_NEPTUNE_BARYCENTER'
      idcode ( ninert +   8 ) =  10008
      center ( ninert +   8 ) =  8
      typid  ( ninert +   8 ) =  8
      type   ( ninert +   8 ) =  pck

      name   ( ninert +   9 ) = 'IAU_PLUTO_BARYCENTER'
      idcode ( ninert +   9 ) =  10009
      center ( ninert +   9 ) =  9
      typid  ( ninert +   9 ) =  9
      type   ( ninert +   9 ) =  pck

      name   ( ninert +  10 ) = 'IAU_SUN'
      idcode ( ninert +  10 ) =  10010
      center ( ninert +  10 ) =  10
      typid  ( ninert +  10 ) =  10
      type   ( ninert +  10 ) =  pck

      name   ( ninert +  11 ) = 'IAU_MERCURY'
      idcode ( ninert +  11 ) =  10011
      center ( ninert +  11 ) =  199
      typid  ( ninert +  11 ) =  199
      type   ( ninert +  11 ) =  pck

      name   ( ninert +  12 ) = 'IAU_VENUS'
      idcode ( ninert +  12 ) =  10012
      center ( ninert +  12 ) =  299
      typid  ( ninert +  12 ) =  299
      type   ( ninert +  12 ) =  pck

      name   ( ninert +  13 ) = 'IAU_EARTH'
      idcode ( ninert +  13 ) =  10013
      center ( ninert +  13 ) =  399
      typid  ( ninert +  13 ) =  399
      type   ( ninert +  13 ) =  pck

      name   ( ninert +  14 ) = 'IAU_MARS'
      idcode ( ninert +  14 ) =  10014
      center ( ninert +  14 ) =  499
      typid  ( ninert +  14 ) =  499
      type   ( ninert +  14 ) =  pck

      name   ( ninert +  15 ) = 'IAU_JUPITER'
      idcode ( ninert +  15 ) =  10015
      center ( ninert +  15 ) =  599
      typid  ( ninert +  15 ) =  599
      type   ( ninert +  15 ) =  pck

      name   ( ninert +  16 ) = 'IAU_SATURN'
      idcode ( ninert +  16 ) =  10016
      center ( ninert +  16 ) =  699
      typid  ( ninert +  16 ) =  699
      type   ( ninert +  16 ) =  pck

      name   ( ninert +  17 ) = 'IAU_URANUS'
      idcode ( ninert +  17 ) =  10017
      center ( ninert +  17 ) =  799
      typid  ( ninert +  17 ) =  799
      type   ( ninert +  17 ) =  pck

      name   ( ninert +  18 ) = 'IAU_NEPTUNE'
      idcode ( ninert +  18 ) =  10018
      center ( ninert +  18 ) =  899
      typid  ( ninert +  18 ) =  899
      type   ( ninert +  18 ) =  pck

      name   ( ninert +  19 ) = 'IAU_PLUTO'
      idcode ( ninert +  19 ) =  10019
      center ( ninert +  19 ) =  999
      typid  ( ninert +  19 ) =  999
      type   ( ninert +  19 ) =  pck

      name   ( ninert +  20 ) = 'IAU_MOON'
      idcode ( ninert +  20 ) =  10020
      center ( ninert +  20 ) =  301
      typid  ( ninert +  20 ) =  301
      type   ( ninert +  20 ) =  pck

      name   ( ninert +  21 ) = 'IAU_PHOBOS'
      idcode ( ninert +  21 ) =  10021
      center ( ninert +  21 ) =  401
      typid  ( ninert +  21 ) =  401
      type   ( ninert +  21 ) =  pck

      name   ( ninert +  22 ) = 'IAU_DEIMOS'
      idcode ( ninert +  22 ) =  10022
      center ( ninert +  22 ) =  402
      typid  ( ninert +  22 ) =  402
      type   ( ninert +  22 ) =  pck

      name   ( ninert +  23 ) = 'IAU_IO'
      idcode ( ninert +  23 ) =  10023
      center ( ninert +  23 ) =  501
      typid  ( ninert +  23 ) =  501
      type   ( ninert +  23 ) =  pck

      name   ( ninert +  24 ) = 'IAU_EUROPA'
      idcode ( ninert +  24 ) =  10024
      center ( ninert +  24 ) =  502
      typid  ( ninert +  24 ) =  502
      type   ( ninert +  24 ) =  pck

      name   ( ninert +  25 ) = 'IAU_GANYMEDE'
      idcode ( ninert +  25 ) =  10025
      center ( ninert +  25 ) =  503
      typid  ( ninert +  25 ) =  503
      type   ( ninert +  25 ) =  pck

      name   ( ninert +  26 ) = 'IAU_CALLISTO'
      idcode ( ninert +  26 ) =  10026
      center ( ninert +  26 ) =  504
      typid  ( ninert +  26 ) =  504
      type   ( ninert +  26 ) =  pck

      name   ( ninert +  27 ) = 'IAU_AMALTHEA'
      idcode ( ninert +  27 ) =  10027
      center ( ninert +  27 ) =  505
      typid  ( ninert +  27 ) =  505
      type   ( ninert +  27 ) =  pck

      name   ( ninert +  28 ) = 'IAU_HIMALIA'
      idcode ( ninert +  28 ) =  10028
      center ( ninert +  28 ) =  506
      typid  ( ninert +  28 ) =  506
      type   ( ninert +  28 ) =  pck

      name   ( ninert +  29 ) = 'IAU_ELARA'
      idcode ( ninert +  29 ) =  10029
      center ( ninert +  29 ) =  507
      typid  ( ninert +  29 ) =  507
      type   ( ninert +  29 ) =  pck

      name   ( ninert +  30 ) = 'IAU_PASIPHAE'
      idcode ( ninert +  30 ) =  10030
      center ( ninert +  30 ) =  508
      typid  ( ninert +  30 ) =  508
      type   ( ninert +  30 ) =  pck

      name   ( ninert +  31 ) = 'IAU_SINOPE'
      idcode ( ninert +  31 ) =  10031
      center ( ninert +  31 ) =  509
      typid  ( ninert +  31 ) =  509
      type   ( ninert +  31 ) =  pck

      name   ( ninert +  32 ) = 'IAU_LYSITHEA'
      idcode ( ninert +  32 ) =  10032
      center ( ninert +  32 ) =  510
      typid  ( ninert +  32 ) =  510
      type   ( ninert +  32 ) =  pck

      name   ( ninert +  33 ) = 'IAU_CARME'
      idcode ( ninert +  33 ) =  10033
      center ( ninert +  33 ) =  511
      typid  ( ninert +  33 ) =  511
      type   ( ninert +  33 ) =  pck

      name   ( ninert +  34 ) = 'IAU_ANANKE'
      idcode ( ninert +  34 ) =  10034
      center ( ninert +  34 ) =  512
      typid  ( ninert +  34 ) =  512
      type   ( ninert +  34 ) =  pck

      name   ( ninert +  35 ) = 'IAU_LEDA'
      idcode ( ninert +  35 ) =  10035
      center ( ninert +  35 ) =  513
      typid  ( ninert +  35 ) =  513
      type   ( ninert +  35 ) =  pck

      name   ( ninert +  36 ) = 'IAU_THEBE'
      idcode ( ninert +  36 ) =  10036
      center ( ninert +  36 ) =  514
      typid  ( ninert +  36 ) =  514
      type   ( ninert +  36 ) =  pck

      name   ( ninert +  37 ) = 'IAU_ADRASTEA'
      idcode ( ninert +  37 ) =  10037
      center ( ninert +  37 ) =  515
      typid  ( ninert +  37 ) =  515
      type   ( ninert +  37 ) =  pck

      name   ( ninert +  38 ) = 'IAU_METIS'
      idcode ( ninert +  38 ) =  10038
      center ( ninert +  38 ) =  516
      typid  ( ninert +  38 ) =  516
      type   ( ninert +  38 ) =  pck

      name   ( ninert +  39 ) = 'IAU_MIMAS'
      idcode ( ninert +  39 ) =  10039
      center ( ninert +  39 ) =  601
      typid  ( ninert +  39 ) =  601
      type   ( ninert +  39 ) =  pck

      name   ( ninert +  40 ) = 'IAU_ENCELADUS'
      idcode ( ninert +  40 ) =  10040
      center ( ninert +  40 ) =  602
      typid  ( ninert +  40 ) =  602
      type   ( ninert +  40 ) =  pck

      name   ( ninert +  41 ) = 'IAU_TETHYS'
      idcode ( ninert +  41 ) =  10041
      center ( ninert +  41 ) =  603
      typid  ( ninert +  41 ) =  603
      type   ( ninert +  41 ) =  pck

      name   ( ninert +  42 ) = 'IAU_DIONE'
      idcode ( ninert +  42 ) =  10042
      center ( ninert +  42 ) =  604
      typid  ( ninert +  42 ) =  604
      type   ( ninert +  42 ) =  pck

      name   ( ninert +  43 ) = 'IAU_RHEA'
      idcode ( ninert +  43 ) =  10043
      center ( ninert +  43 ) =  605
      typid  ( ninert +  43 ) =  605
      type   ( ninert +  43 ) =  pck

      name   ( ninert +  44 ) = 'IAU_TITAN'
      idcode ( ninert +  44 ) =  10044
      center ( ninert +  44 ) =  606
      typid  ( ninert +  44 ) =  606
      type   ( ninert +  44 ) =  pck

      name   ( ninert +  45 ) = 'IAU_HYPERION'
      idcode ( ninert +  45 ) =  10045
      center ( ninert +  45 ) =  607
      typid  ( ninert +  45 ) =  607
      type   ( ninert +  45 ) =  pck

      name   ( ninert +  46 ) = 'IAU_IAPETUS'
      idcode ( ninert +  46 ) =  10046
      center ( ninert +  46 ) =  608
      typid  ( ninert +  46 ) =  608
      type   ( ninert +  46 ) =  pck

      name   ( ninert +  47 ) = 'IAU_PHOEBE'
      idcode ( ninert +  47 ) =  10047
      center ( ninert +  47 ) =  609
      typid  ( ninert +  47 ) =  609
      type   ( ninert +  47 ) =  pck

      name   ( ninert +  48 ) = 'IAU_JANUS'
      idcode ( ninert +  48 ) =  10048
      center ( ninert +  48 ) =  610
      typid  ( ninert +  48 ) =  610
      type   ( ninert +  48 ) =  pck

      name   ( ninert +  49 ) = 'IAU_EPIMETHEUS'
      idcode ( ninert +  49 ) =  10049
      center ( ninert +  49 ) =  611
      typid  ( ninert +  49 ) =  611
      type   ( ninert +  49 ) =  pck

      name   ( ninert +  50 ) = 'IAU_HELENE'
      idcode ( ninert +  50 ) =  10050
      center ( ninert +  50 ) =  612
      typid  ( ninert +  50 ) =  612
      type   ( ninert +  50 ) =  pck

      name   ( ninert +  51 ) = 'IAU_TELESTO'
      idcode ( ninert +  51 ) =  10051
      center ( ninert +  51 ) =  613
      typid  ( ninert +  51 ) =  613
      type   ( ninert +  51 ) =  pck

      name   ( ninert +  52 ) = 'IAU_CALYPSO'
      idcode ( ninert +  52 ) =  10052
      center ( ninert +  52 ) =  614
      typid  ( ninert +  52 ) =  614
      type   ( ninert +  52 ) =  pck

      name   ( ninert +  53 ) = 'IAU_ATLAS'
      idcode ( ninert +  53 ) =  10053
      center ( ninert +  53 ) =  615
      typid  ( ninert +  53 ) =  615
      type   ( ninert +  53 ) =  pck

      name   ( ninert +  54 ) = 'IAU_PROMETHEUS'
      idcode ( ninert +  54 ) =  10054
      center ( ninert +  54 ) =  616
      typid  ( ninert +  54 ) =  616
      type   ( ninert +  54 ) =  pck

      name   ( ninert +  55 ) = 'IAU_PANDORA'
      idcode ( ninert +  55 ) =  10055
      center ( ninert +  55 ) =  617
      typid  ( ninert +  55 ) =  617
      type   ( ninert +  55 ) =  pck

      name   ( ninert +  56 ) = 'IAU_ARIEL'
      idcode ( ninert +  56 ) =  10056
      center ( ninert +  56 ) =  701
      typid  ( ninert +  56 ) =  701
      type   ( ninert +  56 ) =  pck

      name   ( ninert +  57 ) = 'IAU_UMBRIEL'
      idcode ( ninert +  57 ) =  10057
      center ( ninert +  57 ) =  702
      typid  ( ninert +  57 ) =  702
      type   ( ninert +  57 ) =  pck

      name   ( ninert +  58 ) = 'IAU_TITANIA'
      idcode ( ninert +  58 ) =  10058
      center ( ninert +  58 ) =  703
      typid  ( ninert +  58 ) =  703
      type   ( ninert +  58 ) =  pck

      name   ( ninert +  59 ) = 'IAU_OBERON'
      idcode ( ninert +  59 ) =  10059
      center ( ninert +  59 ) =  704
      typid  ( ninert +  59 ) =  704
      type   ( ninert +  59 ) =  pck

      name   ( ninert +  60 ) = 'IAU_MIRANDA'
      idcode ( ninert +  60 ) =  10060
      center ( ninert +  60 ) =  705
      typid  ( ninert +  60 ) =  705
      type   ( ninert +  60 ) =  pck

      name   ( ninert +  61 ) = 'IAU_CORDELIA'
      idcode ( ninert +  61 ) =  10061
      center ( ninert +  61 ) =  706
      typid  ( ninert +  61 ) =  706
      type   ( ninert +  61 ) =  pck

      name   ( ninert +  62 ) = 'IAU_OPHELIA'
      idcode ( ninert +  62 ) =  10062
      center ( ninert +  62 ) =  707
      typid  ( ninert +  62 ) =  707
      type   ( ninert +  62 ) =  pck

      name   ( ninert +  63 ) = 'IAU_BIANCA'
      idcode ( ninert +  63 ) =  10063
      center ( ninert +  63 ) =  708
      typid  ( ninert +  63 ) =  708
      type   ( ninert +  63 ) =  pck

      name   ( ninert +  64 ) = 'IAU_CRESSIDA'
      idcode ( ninert +  64 ) =  10064
      center ( ninert +  64 ) =  709
      typid  ( ninert +  64 ) =  709
      type   ( ninert +  64 ) =  pck

      name   ( ninert +  65 ) = 'IAU_DESDEMONA'
      idcode ( ninert +  65 ) =  10065
      center ( ninert +  65 ) =  710
      typid  ( ninert +  65 ) =  710
      type   ( ninert +  65 ) =  pck

      name   ( ninert +  66 ) = 'IAU_JULIET'
      idcode ( ninert +  66 ) =  10066
      center ( ninert +  66 ) =  711
      typid  ( ninert +  66 ) =  711
      type   ( ninert +  66 ) =  pck

      name   ( ninert +  67 ) = 'IAU_PORTIA'
      idcode ( ninert +  67 ) =  10067
      center ( ninert +  67 ) =  712
      typid  ( ninert +  67 ) =  712
      type   ( ninert +  67 ) =  pck

      name   ( ninert +  68 ) = 'IAU_ROSALIND'
      idcode ( ninert +  68 ) =  10068
      center ( ninert +  68 ) =  713
      typid  ( ninert +  68 ) =  713
      type   ( ninert +  68 ) =  pck

      name   ( ninert +  69 ) = 'IAU_BELINDA'
      idcode ( ninert +  69 ) =  10069
      center ( ninert +  69 ) =  714
      typid  ( ninert +  69 ) =  714
      type   ( ninert +  69 ) =  pck

      name   ( ninert +  70 ) = 'IAU_PUCK'
      idcode ( ninert +  70 ) =  10070
      center ( ninert +  70 ) =  715
      typid  ( ninert +  70 ) =  715
      type   ( ninert +  70 ) =  pck

      name   ( ninert +  71 ) = 'IAU_TRITON'
      idcode ( ninert +  71 ) =  10071
      center ( ninert +  71 ) =  801
      typid  ( ninert +  71 ) =  801
      type   ( ninert +  71 ) =  pck

      name   ( ninert +  72 ) = 'IAU_NEREID'
      idcode ( ninert +  72 ) =  10072
      center ( ninert +  72 ) =  802
      typid  ( ninert +  72 ) =  802
      type   ( ninert +  72 ) =  pck

      name   ( ninert +  73 ) = 'IAU_NAIAD'
      idcode ( ninert +  73 ) =  10073
      center ( ninert +  73 ) =  803
      typid  ( ninert +  73 ) =  803
      type   ( ninert +  73 ) =  pck

      name   ( ninert +  74 ) = 'IAU_THALASSA'
      idcode ( ninert +  74 ) =  10074
      center ( ninert +  74 ) =  804
      typid  ( ninert +  74 ) =  804
      type   ( ninert +  74 ) =  pck

      name   ( ninert +  75 ) = 'IAU_DESPINA'
      idcode ( ninert +  75 ) =  10075
      center ( ninert +  75 ) =  805
      typid  ( ninert +  75 ) =  805
      type   ( ninert +  75 ) =  pck

      name   ( ninert +  76 ) = 'IAU_GALATEA'
      idcode ( ninert +  76 ) =  10076
      center ( ninert +  76 ) =  806
      typid  ( ninert +  76 ) =  806
      type   ( ninert +  76 ) =  pck

      name   ( ninert +  77 ) = 'IAU_LARISSA'
      idcode ( ninert +  77 ) =  10077
      center ( ninert +  77 ) =  807
      typid  ( ninert +  77 ) =  807
      type   ( ninert +  77 ) =  pck

      name   ( ninert +  78 ) = 'IAU_PROTEUS'
      idcode ( ninert +  78 ) =  10078
      center ( ninert +  78 ) =  808
      typid  ( ninert +  78 ) =  808
      type   ( ninert +  78 ) =  pck

      name   ( ninert +  79 ) = 'IAU_CHARON'
      idcode ( ninert +  79 ) =  10079
      center ( ninert +  79 ) =  901
      typid  ( ninert +  79 ) =  901
      type   ( ninert +  79 ) =  pck

!
!     This is for the first new PCK frame---the high precision earth
!     frame ITRF93.
!
      name   ( ninert +  80 ) = 'ITRF93'
      idcode ( ninert +  80 ) =  13000
      center ( ninert +  80 ) =  399
      typid  ( ninert +  80 ) =  3000
      type   ( ninert +  80 ) =  pck

!
!     This if for the alias frame EARTH BODYFIXED.  This is a TK
!     class frame.  To use it a FRAME kernel must be loaded via
!     FURNSH.
!
      name   ( ninert +  81 ) = 'EARTH_FIXED'
      idcode ( ninert +  81 ) =  10081
      center ( ninert +  81 ) =  399
      typid  ( ninert +  81 ) =  10081
      type   ( ninert +  81 ) =  tk

!
!     Frames introduced into the generic NAIF PCK
!     system as referenced from the 1997 IAU report.
!
      name   ( ninert +  82 ) =  'IAU_PAN'
      idcode ( ninert +  82 ) =  10082
      center ( ninert +  82 ) =  618
      typid  ( ninert +  82 ) =  618
      type   ( ninert +  82 ) =  pck

      name   ( ninert +  83 ) =  'IAU_GASPRA'
      idcode ( ninert +  83 ) =  10083
      center ( ninert +  83 ) =  9511010
      typid  ( ninert +  83 ) =  9511010
      type   ( ninert +  83 ) =  pck

      name   ( ninert +  84 ) =  'IAU_IDA'
      idcode ( ninert +  84 ) =  10084
      center ( ninert +  84 ) =  2431010
      typid  ( ninert +  84 ) =  2431010
      type   ( ninert +  84 ) =  pck

!
!     Frame referenced from the Eros orientation model in the 2000 IAU
!     report.
!
      name   ( ninert +  85 ) =  'IAU_EROS'
      idcode ( ninert +  85 ) =  10085
      center ( ninert +  85 ) =  2000433
      typid  ( ninert +  85 ) =  2000433
      type   ( ninert +  85 ) =  pck

!
!     Frames for Jovian satellites approved by IAU in late 2002.
!
      name   ( ninert +  86 ) =  'IAU_CALLIRRHOE'
      idcode ( ninert +  86 ) =  10086
      center ( ninert +  86 ) =  517
      typid  ( ninert +  86 ) =  517
      type   ( ninert +  86 ) =  pck

      name   ( ninert +  87 ) =  'IAU_THEMISTO'
      idcode ( ninert +  87 ) =  10087
      center ( ninert +  87 ) =  518
      typid  ( ninert +  87 ) =  518
      type   ( ninert +  87 ) =  pck

      name   ( ninert +  88 ) =  'IAU_MEGACLITE'
      idcode ( ninert +  88 ) =  10088
      center ( ninert +  88 ) =  519
      typid  ( ninert +  88 ) =  519
      type   ( ninert +  88 ) =  pck

      name   ( ninert +  89 ) =  'IAU_TAYGETE'
      idcode ( ninert +  89 ) =  10089
      center ( ninert +  89 ) =  520
      typid  ( ninert +  89 ) =  520
      type   ( ninert +  89 ) =  pck

      name   ( ninert +  90 ) =  'IAU_CHALDENE'
      idcode ( ninert +  90 ) =  10090
      center ( ninert +  90 ) =  521
      typid  ( ninert +  90 ) =  521
      type   ( ninert +  90 ) =  pck

      name   ( ninert +  91 ) =  'IAU_HARPALYKE'
      idcode ( ninert +  91 ) =  10091
      center ( ninert +  91 ) =  522
      typid  ( ninert +  91 ) =  522
      type   ( ninert +  91 ) =  pck

      name   ( ninert +  92 ) =  'IAU_KALYKE'
      idcode ( ninert +  92 ) =  10092
      center ( ninert +  92 ) =  523
      typid  ( ninert +  92 ) =  523
      type   ( ninert +  92 ) =  pck

      name   ( ninert +  93 ) =  'IAU_IOCASTE'
      idcode ( ninert +  93 ) =  10093
      center ( ninert +  93 ) =  524
      typid  ( ninert +  93 ) =  524
      type   ( ninert +  93 ) =  pck

      name   ( ninert +  94 ) =  'IAU_ERINOME'
      idcode ( ninert +  94 ) =  10094
      center ( ninert +  94 ) =  525
      typid  ( ninert +  94 ) =  525
      type   ( ninert +  94 ) =  pck

      name   ( ninert +  95 ) =  'IAU_ISONOE'
      idcode ( ninert +  95 ) =  10095
      center ( ninert +  95 ) =  526
      typid  ( ninert +  95 ) =  526
      type   ( ninert +  95 ) =  pck

      name   ( ninert +  96 ) =  'IAU_PRAXIDIKE'
      idcode ( ninert +  96 ) =  10096
      center ( ninert +  96 ) =  527
      typid  ( ninert +  96 ) =  527
      type   ( ninert +  96 ) =  pck

!
!     Frames for comets and asteroids, for which rotation constants
!     were added in 2006 IAU Report.
!
      name   ( ninert +  97 ) =  'IAU_BORRELLY'
      idcode ( ninert +  97 ) =  10097
      center ( ninert +  97 ) =  1000005
      typid  ( ninert +  97 ) =  1000005
      type   ( ninert +  97 ) =  pck

      name   ( ninert +  98 ) =  'IAU_TEMPEL_1'
      idcode ( ninert +  98 ) =  10098
      center ( ninert +  98 ) =  1000093
      typid  ( ninert +  98 ) =  1000093
      type   ( ninert +  98 ) =  pck

      name   ( ninert +  99 ) =  'IAU_VESTA'
      idcode ( ninert +  99 ) =  10099
      center ( ninert +  99 ) =  2000004
      typid  ( ninert +  99 ) =  2000004
      type   ( ninert +  99 ) =  pck

      name   ( ninert + 100 ) =  'IAU_ITOKAWA'
      idcode ( ninert + 100 ) =  10100
      center ( ninert + 100 ) =  2025143
      typid  ( ninert + 100 ) =  2025143
      type   ( ninert + 100 ) =  pck

!
!     Frames for asteroids, for which rotation constants were added in
!     2009 IAU Report.
!
      name   ( ninert + 101 ) =  'IAU_CERES'
      idcode ( ninert + 101 ) =  10101
      center ( ninert + 101 ) =  2000001
      typid  ( ninert + 101 ) =  2000001
      type   ( ninert + 101 ) =  pck

      name   ( ninert + 102 ) =  'IAU_PALLAS'
      idcode ( ninert + 102 ) =  10102
      center ( ninert + 102 ) =  2000002
      typid  ( ninert + 102 ) =  2000002
      type   ( ninert + 102 ) =  pck

      name   ( ninert + 103 ) =  'IAU_LUTETIA'
      idcode ( ninert + 103 ) =  10103
      center ( ninert + 103 ) =  2000021
      typid  ( ninert + 103 ) =  2000021
      type   ( ninert + 103 ) =  pck

      name   ( ninert + 104 ) =  'IAU_DAVIDA'
      idcode ( ninert + 104 ) =  10104
      center ( ninert + 104 ) =  2000511
      typid  ( ninert + 104 ) =  2000511
      type   ( ninert + 104 ) =  pck

      name   ( ninert + 105 ) =  'IAU_STEINS'
      idcode ( ninert + 105 ) =  10105
      center ( ninert + 105 ) =  2002867
      typid  ( ninert + 105 ) =  2002867
      type   ( ninert + 105 ) =  pck

!
!     Frame for Bennu asteroid, for OSIRIS-Rex project.
!
      name   ( ninert + 106 ) =  'IAU_BENNU'
      idcode ( ninert + 106 ) =  10106
      center ( ninert + 106 ) =  2101955
      typid  ( ninert + 106 ) =  2101955
      type   ( ninert + 106 ) =  pck

!
!     Frames for asteroid 52 Europa, Nix, Hydra, and Hayabusa 2,
!     New Horizons, DART, and Lucy mission targets.
!
      name   ( ninert + 107 ) =  'IAU_52_EUROPA'
      idcode ( ninert + 107 ) =  10107
      center ( ninert + 107 ) =  2000052
      typid  ( ninert + 107 ) =  2000052
      type   ( ninert + 107 ) =  pck

      name   ( ninert + 108 ) =  'IAU_NIX'
      idcode ( ninert + 108 ) =  10108
      center ( ninert + 108 ) =  902
      typid  ( ninert + 108 ) =  902
      type   ( ninert + 108 ) =  pck

      name   ( ninert + 109 ) =  'IAU_HYDRA'
      idcode ( ninert + 109 ) =  10109
      center ( ninert + 109 ) =  903
      typid  ( ninert + 109 ) =  903
      type   ( ninert + 109 ) =  pck

      name   ( ninert + 110 ) =  'IAU_RYUGU'
      idcode ( ninert + 110 ) =  10110
      center ( ninert + 110 ) =  2162173
      typid  ( ninert + 110 ) =  2162173
      type   ( ninert + 110 ) =  pck

      name   ( ninert + 111 ) =  'IAU_ARROKOTH'
      idcode ( ninert + 111 ) =  10111
      center ( ninert + 111 ) =  2486958
      typid  ( ninert + 111 ) =  2486958
      type   ( ninert + 111 ) =  pck

      name   ( ninert + 112 ) =  'IAU_DIDYMOS_BARYCENTER'
      idcode ( ninert + 112 ) =  10112
      center ( ninert + 112 ) =  20065803
      typid  ( ninert + 112 ) =  20065803
      type   ( ninert + 112 ) =  pck

      name   ( ninert + 113 ) =  'IAU_DIDYMOS'
      idcode ( ninert + 113 ) =  10113
      center ( ninert + 113 ) =  920065803
      typid  ( ninert + 113 ) =  920065803
      type   ( ninert + 113 ) =  pck

      name   ( ninert + 114 ) =  'IAU_DIMORPHOS'
      idcode ( ninert + 114 ) =  10114
      center ( ninert + 114 ) =  120065803
      typid  ( ninert + 114 ) =  120065803
      type   ( ninert + 114 ) =  pck

      name   ( ninert + 115 ) =  'IAU_DONALDJOHANSON'
      idcode ( ninert + 115 ) =  10115
      center ( ninert + 115 ) =  20052246
      typid  ( ninert + 115 ) =  20052246
      type   ( ninert + 115 ) =  pck

      name   ( ninert + 116 ) =  'IAU_EURYBATES'
      idcode ( ninert + 116 ) =  10116
      center ( ninert + 116 ) =  920003548
      typid  ( ninert + 116 ) =  920003548
      type   ( ninert + 116 ) =  pck

      name   ( ninert + 117 ) =  'IAU_EURYBATES_BARYCENTER'
      idcode ( ninert + 117 ) =  10117
      center ( ninert + 117 ) =  20003548
      typid  ( ninert + 117 ) =  20003548
      type   ( ninert + 117 ) =  pck

      name   ( ninert + 118 ) =  'IAU_QUETA'
      idcode ( ninert + 118 ) =  10118
      center ( ninert + 118 ) =  120003548
      typid  ( ninert + 118 ) =  120003548
      type   ( ninert + 118 ) =  pck

      name   ( ninert + 119 ) =  'IAU_POLYMELE'
      idcode ( ninert + 119 ) =  10119
      center ( ninert + 119 ) =  20015094
      typid  ( ninert + 119 ) =  20015094
      type   ( ninert + 119 ) =  pck

      name   ( ninert + 120 ) =  'IAU_LEUCUS'
      idcode ( ninert + 120 ) =  10120
      center ( ninert + 120 ) =  20011351
      typid  ( ninert + 120 ) =  20011351
      type   ( ninert + 120 ) =  pck

      name   ( ninert + 121 ) =  'IAU_ORUS'
      idcode ( ninert + 121 ) =  10121
      center ( ninert + 121 ) =  20021900
      typid  ( ninert + 121 ) =  20021900
      type   ( ninert + 121 ) =  pck

      name   ( ninert + 122 ) =  'IAU_PATROCLUS_BARYCENTER'
      idcode ( ninert + 122 ) =  10122
      center ( ninert + 122 ) =  20000617
      typid  ( ninert + 122 ) =  20000617
      type   ( ninert + 122 ) =  pck

      name   ( ninert + 123 ) =  'IAU_PATROCLUS'
      idcode ( ninert + 123 ) =  10123
      center ( ninert + 123 ) =  920000617
      typid  ( ninert + 123 ) =  920000617
      type   ( ninert + 123 ) =  pck

      name   ( ninert + 124 ) =  'IAU_MENOETIUS'
      idcode ( ninert + 124 ) =  10124
      center ( ninert + 124 ) =  120000617
      typid  ( ninert + 124 ) =  120000617
      type   ( ninert + 124 ) =  pck

!
!     Below is a template to use for adding another non-inertial
!     frame. Copy it, fill in the new values and then leave
!     a new template for the next person who needs to modify this
!     routine.
!
!     NAME   ( NINERT + 125 ) =  name
!     IDCODE ( NINERT + 125 ) =  10125
!     CENTER ( NINERT + 125 ) =  center
!     TYPID  ( NINERT + 125 ) =  type ID code
!     TYPE   ( NINERT + 125 ) =  type (INERTL, PCK, etc. )
!

!
!     Generate order vector for centers (used by CIDFRM).
!
      call orderi ( center, nnames, centrd )

!
!     Initialize build-in frame name- and ID-based hashes.
!
      call zzhscini ( ctx, maxbfr, bnmlst, bnmpol )
      call zzhsiini ( ctx, maxbfr, bidlst, bidpol )

!
!     Register all built-in frames in the frame name- and ID-based
!     hashes.
!
      do i = 1, nnames

         call zzhscadd ( ctx, bnmlst, bnmpol, bnmnms, name(i),   item, new )
         bnmidx( item ) = i

         call zzhsiadd ( ctx, bidlst, bidpol, bidids, idcode(i), item, new )
         bididx( item ) = i

      end do

      return
      end subroutine zzfdat

      !> Look up the frame ID code associated with a string.
      !>
      !>## Detailed Input
      !>
      !>     FRNAME   is a character string that stands for some
      !>              reference frame (either inertial or non-inertial).
      !>
      !>              Leading blanks in FRNAME are ignored. And the
      !>              case of the letters in FRNAME are insignificant.
      !>
      !>              Note that all legitimate frame names contain
      !>              26 or fewer characters.
      !>
      !>## Detailed Output
      !>
      !>     FRCODE   is the SPICE integer code used for internal
      !>              representation of the named reference frame.
      !>
      !>              If the name input through FRNAME is not recognized
      !>              FRCODE will be returned with a value of zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input name is not recognized, FRCODE will be
      !>         returned with a value of 0.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a low level interface routine intended primarily for
      !>     use within the SPK and CK systems to assist in the transformation
      !>     to user specified reference frames.
      !>
      !>     The routine first consults a stored list of reference frame
      !>     names in an attempt to determine the appropriate reference
      !>     frame code.
      !>
      !>     If this search is unsuccessful, the routine then examines the
      !>     kernel pool to determine whether or not a variable of the
      !>     form
      !>
      !>        'FRAME_' // FRNAME
      !>
      !>        (where leading blanks of FRNAME are ignored)
      !>
      !>     is present. If it is and the number of values associated with the
      !>     name is 1, this value is taken to be the frame ID code.
      !>
      !>     Note: It is NOT possible to override the default names and
      !>     ID codes stored locally in this routine by placing an
      !>     appropriately variable in the kernel pool with a different
      !>     ID code. The predefined values always take precedence.
      !>
      !>     Consult the FRAMES required reading document for more details
      !>     about constructing your own frame definitions.
      !>
      !>## Examples
      !>
      !>     Suppose that you needed to find the SPICE ID code for the
      !>     bodyfixed reference frame for Mars as modeled by the
      !>     IAU cartographic working group. Use the following code
      !>     to perform this task.
      !>
      !>        CALL NAMFRM ( 'IAU_MARS', FRCODE )
      !>
      !>        WRITE (*,*) 'The SPICE code for the Mars bodyfixed frame is: ',
      !>       .             FRCODE.
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
      !>-    SPICELIB Version 3.1.1, 02-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.1.0, 08-AUG-2012 (BVS)
      !>
      !>        The routine was updated to be more efficient by using hashes
      !>        instead kernel POOL look-ups for kernel POOL frames and by
      !>        using hashes instead of ordered array searches for built-in
      !>        frames.
      !>
      !>-    SPICELIB Version 3.0.2, 17-MAR-2009 (EDW)
      !>
      !>        Typo correction in $Required_Reading, changed FRAME to FRAMES.
      !>
      !>-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
      !>
      !>        Extended documentation of entry point CNMFRM and
      !>        corrected example for that entry point.
      !>
      !>-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
      !>
      !>        The entry points CIDFRM and CNMFRM were added so that
      !>        user's may determine the frame-id and name to associated
      !>        with a planetary object.
      !>
      !>-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
      !>
      !>        The routine was upgraded to reflect that a block of
      !>        frame ID codes have been reserved for use by the DSN.
      !>        ID codes 13001 to 13999 have been set aside for DSN
      !>        models for the orientation of the earth. These frames
      !>        are all PCK frames. Moreover, the PCK ID code to
      !>        use with these frames is simply the Frame-Code minus 10000.
      !>        All of these frames are centered at the earth (body 399).
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The values NINERT and NNINRT are included instead of
      !>        being declared locally.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
      module subroutine namfrm ( ctx, frname, frcode )
      use pepper_kinds, only: ip
      use pepper_strings, only: prefix, ucase_inplace
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! The name of some reference frame
      integer(ip), intent(out) :: frcode  !! The SPICE ID code of the frame.
      character(:), allocatable :: pname
      logical :: found
      frcode = 0
      if ( ctx%failed() ) return
!
!     Handle the most common case, J2000, first.
!
      if ( frname == 'J2000' .or. frname == 'j2000') then
         frcode = 1
         return
      end if
      call framex_ready ( ctx )
      if ( ctx%failed() ) return
!
!     PEPPER: the name is normalized (left-justified, uppercase) in a
!     copy of its own length; SPICELIB truncated it to 32 characters.
!
      allocate ( character(len(frname)) :: pname )
      pname = adjustl(frname)
      call ucase_inplace ( pname )
      call framex_code ( ctx, pname, frcode, found )
      return
      end subroutine namfrm

      !> Retrieve the name of a reference frame associated with a SPICE ID
      !> code.
      !>
      !>## Detailed Input
      !>
      !>     FRCODE   is an integer code for a reference frame.
      !>
      !>## Detailed Output
      !>
      !>     FRNAME   is the name associated with the reference frame. It will
      !>              be returned left justified.
      !>
      !>              If FRCODE is not recognized as the name of a known
      !>              reference frame, FRNAME will be returned as a blank.
      !>
      !>              If FRNAME is not sufficiently long to hold the name, it
      !>              will be truncated on the right.
      !>
      !>              All reference frame names are 26 or fewer characters in
      !>              length. Thus declaring FRNAME to be CHARACTER*(26) will
      !>              ensure that the returned name will not be truncated.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If FRCODE is not recognized as the name of a known reference
      !>         frame, FRNAME will be returned as a blank.
      !>
      !>     2)  If FRNAME is not sufficiently long to hold the name, it will
      !>         be truncated on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine retrieves the name of a reference frame associated
      !>     with a SPICE frame ID code.
      !>
      !>     The ID codes stored locally are scanned for a match with FRCODE.
      !>     If a match is found, the name stored locally will be returned
      !>     as the name for the frame.
      !>
      !>     If FRCODE is not a member of the list of internally stored
      !>     ID codes, the kernel pool will be examined to see if the
      !>     variable
      !>
      !>        FRAME_idcode_NAME
      !>
      !>     is present (where idcode is the decimal character equivalent
      !>     of FRCODE). If the variable is located and it has both
      !>     character type and dimension 1, the string value of the
      !>     kernel pool variable is returned as the name of the reference
      !>     frame.
      !>
      !>     Note that because the local information is always examined
      !>     first and searches of the kernel pool are performed only
      !>     after exhausting local information, it is not possible to
      !>     override the local name for any reference frame that is
      !>     known by this routine.
      !>
      !>## Examples
      !>
      !>     Suppose you needed to create a message concerning a reference
      !>     frame and wish to use the name of the frame in the message.
      !>     Suppose further that you have only the frame ID code at your
      !>     disposal. You can capture the frame name using this routine
      !>     as shown here.
      !>
      !>        CHARACTER*(32)        FRNAME
      !>
      !>        CALL FRMNAM ( FRCODE, FRNAME )
      !>
      !>        IF ( FRNAME .EQ. ' ' ) THEN
      !>           CALL INTSTR ( FRCODE, FRNAME )
      !>        END IF
      !>
      !>        WRITE (*,*) 'Concerning reference frame:', FRNAME
      !>
      !>        print the rest of your message.
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
      !>-    SPICELIB Version 3.1.1, 02-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Updated maximum
      !>        frame name length in detailed description of FRNAME argument
      !>        and changed frame name length from 26 to 32 in example code.
      !>
      !>-    SPICELIB Version 3.1.0, 08-AUG-2012 (BVS)
      !>
      !>        The routine was updated to be more efficient by using hashes
      !>        instead kernel POOL look-ups for kernel POOL frames and by
      !>        using hashes instead of ordered array searches for built-in
      !>        frames.
      !>
      !>-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
      !>
      !>        Extended documentation of entry point CNMFRM and
      !>        corrected example for that entry point.
      !>
      !>-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
      !>
      !>        The entry points CIDFRM and CNMFRM were added so that
      !>        user's may determine the frame-id and name to associated
      !>        with a planetary object.
      !>
      !>-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
      !>
      !>        The routine was upgraded to reflect that a block of
      !>        frame ID codes have been reserved for use by the DSN.
      !>        ID codes 13001 to 13999 have been set aside for DSN
      !>        models for the orientation of the earth. These frames
      !>        are all PCK frames. Moreover, the PCK ID code to
      !>        use with these frames is simply the Frame-Code minus 10000.
      !>        All of these frames are centered at the earth (body 399).
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The values NINERT and NNINRT are included instead of
      !>        being declared locally.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
      module subroutine frmnam ( ctx, frcode, frname )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: frname  !! the name associated with the reference frame.
      integer(ip), intent(in) :: frcode  !! an integer code for a reference frame
      character(:), allocatable :: name
      logical :: found
      call frame_name_of ( ctx, frcode, name, found )
      frname = name
      return
      end subroutine frmnam

      !> Retrieve the minimal attributes associated with a frame
      !> needed for converting transformations to and from it.
      !>
      !>## Detailed Input
      !>
      !>     FRCODE   is the ID code for some reference frame.
      !>
      !>## Detailed Output
      !>
      !>     CENT     is the body ID code for the center of the reference
      !>              frame (if such an ID code is appropriate).
      !>
      !>     FRCLSS   is the class or type of the frame. This identifies
      !>              which subsystem will be used to perform frame
      !>              transformations.
      !>
      !>     CLSSID   is the ID code used for the frame within its class.
      !>              This may be different from the frame ID code.
      !>
      !>     FOUND    is .TRUE. if CENT, FRCLSS and CCODE are available.
      !>              Otherwise, FOUND is returned with the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If a frame definition is encountered that does not define a
      !>         central body for the frame, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If a frame definition is encountered that does not define
      !>         a class for the frame, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     3)  If a frame definition is encountered that does not define a
      !>         class ID for the frame, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     4)  If a kernel variable defining a frame name is found, but
      !>         that variable has dimension greater than 1, the error
      !>         SPICE(INVALIDDIMENSION) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a low level routine needed by state transformation
      !>     software to transform states and attitudes between different
      !>     reference frames.
      !>
      !>     The routine first examines local "hard-coded" information about
      !>     reference frames to see if the requested frame belongs to this
      !>     set. If it does that information is returned.
      !>
      !>     If the requested information is not stored locally, the routine
      !>     then examines the kernel pool to see if the requested information
      !>     is stored there. If it is and has the expected format, the data
      !>     is retrieved and returned.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Given a frame ID, retrieve the SPICE body ID associated with
      !>        the frame's center, the frame class (or type of the frame),
      !>        and the ID used for the frame within its class.
      !>
      !>          Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM FRINFO_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               FRCODE
      !>      PARAMETER           ( FRCODE = 13000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               CENTER
      !>      INTEGER               CLSSID
      !>      INTEGER               FRCLSS
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Retrieve the information for frame ID 13000.
      !>C
      !>      CALL FRINFO ( FRCODE, CENTER, FRCLSS, CLSSID, FOUND )
      !>
      !>      IF ( FOUND ) THEN
      !>
      !>         WRITE(*,'(A,I6)') 'Frame center  : ', CENTER
      !>         WRITE(*,'(A,I6)') 'Frame class   : ', FRCLSS
      !>         WRITE(*,'(A,I6)') 'Frame class ID: ', CLSSID
      !>
      !>      ELSE
      !>
      !>         WRITE (*,'(A,I6)') 'There is insufficient data '
      !>     .                   // 'for frame ', FRCODE
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>          When this program was executed on a Mac/Intel/gfortran/64-bit
      !>          platform, the output was:
      !>
      !>        Frame center  :    399
      !>        Frame class   :      2
      !>        Frame class ID:   3000
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
      !>-    SPICELIB Version 4.2.0, 02-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example from existing fragments.
      !>
      !>        Changed the output argument name CLASS to FRCLSS for
      !>        consistency with other routines.
      !>
      !>-    SPICELIB Version 4.1.0, 08-AUG-2012 (BVS)
      !>
      !>        The routine was updated to be more efficient by using hashes
      !>        instead kernel POOL look-ups for kernel POOL frames and by
      !>        using hashes instead of ordered array searches for built-in
      !>        frames.
      !>
      !>-    SPICELIB Version 4.0.0, 12-SEP-2005 (NJB)
      !>
      !>        Entry point FRINFO is no longer error-free. The following
      !>        errors are now diagnosed:
      !>
      !>           - Invalid dimension of frame name variable
      !>
      !>           - If a valid frame name assignment is present:
      !>
      !>              + Missing frame ID code assignment
      !>              + Missing class assignment
      !>              + Missing class ID assignment
      !>
      !>        Specification of frame center by name or ID is now supported.
      !>        Previously only ID codes could be used to identify frame
      !>        centers. Various frame definition errors that were previously
      !>        ignored are now diagnosed.
      !>
      !>-    SPICELIB Version 3.1.0, 20-DEC-2004 (BVS)
      !>
      !>        Removed special handling of the frame IDs less than -999. If
      !>        they cannot be ``resolved'' using kernel pool keywords, the
      !>        frame is NOT declared CK-based with center ID derived by
      !>        dividing frame ID by a 1000 and class ID assigned the frame ID
      !>        anymore. In the current practice with multitude of TK frames
      !>        with IDs set instrument IDs this default behavior is simply
      !>        not valid.
      !>
      !>-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
      !>
      !>        Extended documentation of entry point CNMFRM and
      !>        corrected example for that entry point.
      !>
      !>-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
      !>
      !>        The entry points CIDFRM and CNMFRM were added so that
      !>        user's may determine the frame-id and name to associated
      !>        with a planetary object.
      !>
      !>-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
      !>
      !>        The routine was upgraded to reflect that a block of
      !>        frame ID codes have been reserved for use by the DSN.
      !>        ID codes 13001 to 13999 have been set aside for DSN
      !>        models for the orientation of the earth. These frames
      !>        are all PCK frames. Moreover, the PCK ID code to
      !>        use with these frames is simply the Frame-Code minus 10000.
      !>        All of these frames are centered at the earth (body 399).
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The values NINERT and NNINRT are included instead of
      !>        being declared locally.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
      module subroutine frinfo ( ctx, frcode, cent, frclss, clssid, found )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frcode  !! the ID code for some frame
      integer(ip), intent(out) :: cent  !! the center of the frame
      integer(ip), intent(out) :: frclss  !! the class (type) of the frame
      integer(ip), intent(out) :: clssid  !! the ID code for the frame within its class.
      logical, intent(out) :: found  !! .TRUE. if the requested information is available.
      integer(ip), parameter :: non = nninrt
      integer(ip), parameter :: ncount = ninert + non
      integer(ip), parameter :: room = 8
      integer(ip), parameter :: lnsize = 80
      integer(ip), parameter :: maxbfr = 149
      integer(ip) :: n
      integer(ip) :: values ( room )
      character(:), allocatable :: pname
      integer(ip) :: item
      character(lnsize) :: line   ( room )
      logical :: gotit
      logical :: lupdte
      character(:), allocatable :: lcfram
      logical :: lnew
      if ( .not. allocated ( ctx%framex_state ) ) then
         allocate ( ctx%framex_state )
         call init_framex_state ( ctx%framex_state )
      end if
      associate ( fx => ctx%framex_state, &
                  name => ctx%framex_state%name, &
                  center => ctx%framex_state%center, &
                  centrd => ctx%framex_state%centrd, &
                  idcode => ctx%framex_state%idcode, &
                  type => ctx%framex_state%type, &
                  typeid => ctx%framex_state%typeid, &
                  first => ctx%framex_state%first, &
                  pulctr => ctx%framex_state%pulctr, &
                  bnmlst => ctx%framex_state%bnmlst, &
                  bnmpol => ctx%framex_state%bnmpol, &
                  bnmnms => ctx%framex_state%bnmnms, &
                  bnmidx => ctx%framex_state%bnmidx, &
                  bidlst => ctx%framex_state%bidlst, &
                  bidpol => ctx%framex_state%bidpol, &
                  bidids => ctx%framex_state%bidids, &
                  bididx => ctx%framex_state%bididx )
      if ( ctx%failed() ) return

!
!     For efficiency, J2000 deserves special treatment.
!
      if ( frcode == 1 ) then
         cent   =  0
         frclss =  inertl
         clssid =  1
         found  = .true.
         return
      end if

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
         call fx%kreset ( )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ctx, ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( ctx%failed() ) return

         first = .false.

      end if

!
!     No frame information has been found yet.
!
      found = .false.

!
!     Determine the location of the requested item in the array
!     of ID codes.
!
      call zzhsichk ( ctx, bidlst, bidpol, bidids, frcode, item )
      if ( item /= 0 ) then
         item = bididx( item )
      end if

!
!     If the name is in our hash, we can just look up its ID code in
!     the parallel array.
!
      if ( item > 0 ) then

         cent   =  center(item)
         frclss =  type  (item)
         clssid =  typeid(item)
         found  = .true.

      else

!
!        See if this frame is in the kernel pool frame ID-based hash.
!        First reset the hash if POOL has changed.
!
         call zzpctrck ( ctx, pulctr, lupdte )

         if ( lupdte ) then
            call fx%kreset ( )
         end if

!
!        Check if this ID is in the hash.
!
         item = fx%kid_find ( frcode )

         if ( item /= 0 ) then

            cent   =  fx%kcent(item)
            frclss =  fx%kclass(item)
            clssid =  fx%kclsid(item)
            found  = .true.

         else

!
!           The ID wasn't in the hash, see if we can find this frame in
!           the kernel pool.
!
!           PEPPER: the name of any length.
!
            pname = frame_variable ( frcode, 'NAME' )
            call ctx%pool_string ( pname, 1_ip, lcfram, gotit, n )

            if ( gotit ) then

               if ( n > 1 ) then
!
!                 We have an array-valued variable that looks like
!                 a frame name. We consider this an error.
!
                  call ctx%setmsg ( 'Kernel variable # is array-valued; ' // &
                                'Frame name variables must be '       // &
                                'scalar-valued.'                      )
                  call ctx%errch  ( '#', pname                            )
                  call ctx%sigerr ( 'SPICE(INVALIDDIMENSION)'             )
                  return

               end if

               lcfram = trim ( adjustl ( lcfram ) )
!
!              Start by looking up the central body of the frame. The
!              name of the kernel variable for the body could refer to
!              the frame by name or frame ID; the body itself could be
!              specified by name or body ID.
!
               call zzdynbid ( ctx, lcfram, frcode, 'CENTER', cent )

               if ( ctx%failed() ) return

               found = .true.

!
!              FOUND has been set to indicate whether we found the
!              frame's center. If we did, CENT has been assigned.
!
!              Next look up the frame class and class ID.
!
               call zzdynvai ( ctx, lcfram, frcode, 'CLASS',    1, n, values)
               frclss  = values(1)

               call zzdynvai ( ctx, lcfram, frcode, 'CLASS_ID', 1, n, values)
               clssid = values(1)

               if ( ctx%failed() ) return

!              If we made it to this point, we successfully collected
!              all items for this frame. Add this frame to the
!              ID-based hash.
!
               call fx%kid_add ( frcode, item )

               if ( .not. ctx%failed() .and. item /= 0 ) then

                  fx%kname(item)%s = lcfram
                  fx%kcent(item) = cent
                  fx%kclass(item) = frclss
                  fx%kclsid(item) = clssid

!
!                 Also, try to add this frame to the name-based hash.
!
                  call fx%kname_add ( lcfram, item )

                  if ( .not. ctx%failed() .and. item /= 0 ) then

                     fx%knmids(item) = frcode

                  end if

               end if

            end if

         end if

!
!        In support of the DSN, NAIF has reserved a block of
!        ID codes for DSN specific frames  from 13000 to 13999.
!        These are always PCK based frames for the earth.
!        The PCK ID code is just FRCODE - 10000.
!
         if      ( .not. found &
                   .and. frcode >= 13000 &
                   .and. frcode < 14000 ) then

            cent   = 399
            frclss = pck
            clssid = frcode - 10000
            found  = .true.

         end if

      end if

      return
      end associate
      end subroutine frinfo

      !> Retrieve frame ID code and name to associate with a frame center.
      !>
      !>## Detailed Input
      !>
      !>     CENT     is the ID code for object for which there is a
      !>              preferred reference frame.
      !>
      !>## Detailed Output
      !>
      !>     FRCODE   is the frame ID code to associate with the object
      !>              specified by CENT.
      !>
      !>     FRNAME   is the name of the frame that should be associated
      !>              with the object specified by CENT. FRNAME should be
      !>              declared as CHARACTER*(32) to ensure that it can
      !>              contain the full name of the frame. If FRNAME does
      !>              not have enough room to hold the full name of the
      !>              frame, the name will be truncated on the right.
      !>
      !>     FOUND    is .TRUE. if the appropriate frame ID code and frame
      !>              name can be determined. Otherwise FOUND is returned
      !>              with the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If FRNAME does not have enough room to contain the frame name,
      !>         the name will be truncated on the right. (Declaring FRNAME to
      !>         be CHARACTER*(32) will ensure that the name will not be
      !>         truncated).
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows the user to determine the frame that should
      !>     be associated with a particular object. For example, if you
      !>     need the frame to associate with the Io, you can call CIDFRM
      !>     to determine the frame name and ID code for the bodyfixed frame
      !>     of Io.
      !>
      !>     The preferred frame to use with an object is specified via one
      !>     of the kernel pool variables:
      !>
      !>         OBJECT_<cent>_FRAME
      !>
      !>     where <cent> is the decimal representation of the integer CENT.
      !>
      !>     For those PCK objects that have "built-in" frame names this
      !>     routine returns the corresponding "IAU" frame and frame ID code.
      !>
      !>## Examples
      !>
      !>     Suppose that you want to determine the state of a target
      !>     in the preferred reference frame of some observer. This
      !>     routine can be used in conjunction with SPKEZ to compute
      !>     the state.
      !>
      !>        CALL CIDFRM ( OBS, FRCODE, FRNAME, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>
      !>           WRITE (*,*) 'The bodyfixed frame for object ', OBS
      !>           WRITE (*,*) 'could not be identified.'
      !>           STOP
      !>
      !>        END IF
      !>
      !>        CALL SPKEZ ( TARG, ET, FRNAME, ABCORR, OBS, STATE, LT )
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
      !>-    SPICELIB Version 3.2.1, 13-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated maximum frame name length from 26 to 32 in
      !>        $Detailed_Output and $Exceptions section.
      !>
      !>-    SPICELIB Version 3.2.0, 08-AUG-2012 (BVS)
      !>
      !>        The routine was updated to be more efficient by using hashes
      !>        instead kernel POOL look-ups for kernel POOL frames and by
      !>        using hashes instead of ordered array searches for built-in
      !>        frames.
      !>
      !>-    SPICELIB Version 3.1.1, 09-FEB-2011 (NJB)
      !>
      !>        Bug fix: corrected logic for object-frame association for case
      !>        where the assigned frame value is denoted by a frame code.
      !>
      !>-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
      !>
      !>        Extended documentation of entry point CNMFRM and
      !>        corrected example for that entry point.
      !>
      !>-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
      !>
      !>        The entry points CIDFRM and CNMFRM were added so that
      !>        user's may determine the frame-id and name to associated
      !>        with a planetary object.
      !>
      !>-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
      !>
      !>        The routine was upgraded to reflect that a block of
      !>        frame ID codes have been reserved for use by the DSN.
      !>        ID codes 13001 to 13999 have been set aside for DSN
      !>        models for the orientation of the earth. These frames
      !>        are all PCK frames. Moreover, the PCK ID code to
      !>        use with these frames is simply the Frame-Code minus 10000.
      !>        All of these frames are centered at the earth (body 399).
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The values NINERT and NNINRT are included instead of
      !>        being declared locally.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
      module subroutine cidfrm ( ctx, cent, frcode, frname, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bschoi
      use pepper_strings, only: prefix, repmc, ucase
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: frname  !! the name of the frame with ID FRCODE
      integer(ip), intent(out) :: frcode  !! the ID code of the frame associated with CENT
      integer(ip), intent(in) :: cent  !! an object to associate a frame with.
      logical, intent(out) :: found  !! .TRUE. if the requested information is available.
      character(:), allocatable :: name
      call frame_of_center ( ctx, cent, frcode, name, found )
      frname = name
      return
      end subroutine cidfrm

      !> Retrieve frame ID code and name to associate with an object.
      !>
      !>## Detailed Input
      !>
      !>     CNAME    is the name for object for which there is a
      !>              preferred reference frame
      !>
      !>## Detailed Output
      !>
      !>     FRCODE   is the frame ID code to associate with a the object
      !>              specified by CNAME.
      !>
      !>     FRNAME   is the name of the frame that should be associated
      !>              with the object specified by CNAME. FRNAME should be
      !>              declared as CHARACTER*(32) to ensure that it can
      !>              contain the full name of the frame. If FRNAME does
      !>              not have enough room to hold the full name of the
      !>              frame, the name will be truncated on the right.
      !>
      !>     FOUND    is .TRUE. if the appropriate frame ID code and frame
      !>              name can be determined. Otherwise FOUND is returned
      !>              with the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If FRNAME does not have enough room to contain the frame name,
      !>         the name will be truncated on the right. (Declaring FRNAME to
      !>         be CHARACTER*(32) will ensure that the name will not be
      !>         truncated).
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows the user to determine the frame that should
      !>     be associated with a particular object. For example, if you
      !>     need the frame to associate with the Io, you can call CNMFRM
      !>     to determine the frame name and ID code for the bodyfixed frame
      !>     of Io.
      !>
      !>     The preferred frame to use with an object is specified via one
      !>     of the kernel pool variables:
      !>
      !>         OBJECT_<cname>_FRAME
      !>
      !>     where <cname> is the non-blank portion of the string CNAME.
      !>
      !>     For those PCK objects that have "built-in" frame names this
      !>     routine returns the corresponding "IAU" frame and frame ID code.
      !>
      !>## Examples
      !>
      !>     Suppose that you want to determine the state of a target
      !>     in the preferred reference frame of some observer. This
      !>     routine can be used in conjunction with SPKEZR to compute
      !>     the state.
      !>
      !>        CALL CNMFRM ( OBSNAM, FRCODE, FRNAME, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>
      !>           WRITE (*,*) 'The bodyfixed frame for object ', OBSNAM
      !>           WRITE (*,*) 'could not be identified.'
      !>           STOP
      !>
      !>        END IF
      !>
      !>        CALL SPKEZR ( TARGET, ET, FRNAME, ABCORR, OBSNAM, STATE, LT )
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
      !>-    SPICELIB Version 3.2.1, 02-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated maximum frame name length from 26 to 32 in
      !>        $Detailed_Output and $Exceptions section.
      !>
      !>-    SPICELIB Version 3.2.0, 08-AUG-2012 (BVS)
      !>
      !>        The routine was updated to be more efficient by using hashes
      !>        instead kernel POOL look-ups for kernel POOL frames and by
      !>        using hashes instead of ordered array searches for built-in
      !>        frames.
      !>
      !>-    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB)
      !>
      !>        Bug fix: updated this routine so a TK frame specified by name
      !>        and designated as an object's preferred frame via kernel pool
      !>        assignments is found, and so that the correct name of this
      !>        frame is returned.
      !>
      !>-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
      !>
      !>        Extended documentation of entry point CNMFRM and
      !>        corrected example for that entry point.
      !>
      !>-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
      !>
      !>        The entry points CIDFRM and CNMFRM were added so that
      !>        user's may determine the frame-id and name to associated
      !>        with a planetary object.
      !>
      !>-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
      !>
      !>        The routine was upgraded to reflect that a block of
      !>        frame ID codes have been reserved for use by the DSN.
      !>        ID codes 13001 to 13999 have been set aside for DSN
      !>        models for the orientation of the earth. These frames
      !>        are all PCK frames. Moreover, the PCK ID code to
      !>        use with these frames is simply the Frame-Code minus 10000.
      !>        All of these frames are centered at the earth (body 399).
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
      !>
      !>        The values NINERT and NNINRT are included instead of
      !>        being declared locally.
      !>
      !>-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
      module subroutine cnmfrm ( ctx, cname, frcode, frname, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: prefix, repmc, ucase
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: cname  !! name of the object to find a frame for
      character(*), intent(out) :: frname  !! the name of the frame with ID FRCODE
      integer(ip), intent(out) :: frcode  !! the ID code of the frame associated with CNAME
      logical, intent(out) :: found  !! .TRUE. if the requested information is available.
      character(:), allocatable :: lookup
      character(:), allocatable :: name
      character(1) :: dattyp
      integer(ip) :: id
      integer(ip) :: item
      integer(ip) :: n
      logical :: gotit
      frcode = 0
      found  = .false.
      if ( ctx%failed() ) return
      call framex_ready ( ctx )
      if ( ctx%failed() ) return
!
!     A frame assigned to the object by OBJECT_<name>_FRAME, or else by
!     OBJECT_<ID>_FRAME (PEPPER: names of any length).
!
      name = 'OBJECT_' // marker_text ( cname ) // '_FRAME'
      lookup = name
      call ucase ( name, lookup )
      call dtpool ( ctx, lookup, gotit, n, dattyp )
      if ( .not. gotit ) then
         call bodn2c ( ctx, cname, id, gotit )
         if ( gotit ) then
            lookup = 'OBJECT_' // int_text ( id ) // '_FRAME'
            call dtpool ( ctx, lookup, gotit, n, dattyp )
         end if
      end if
      if ( gotit ) then
         call object_frame ( ctx, lookup, dattyp, frcode, name, found )
         frname = name
         return
      end if
!
!     Otherwise the built-in frame IAU_<name>, if there is one. As in
!     SPICELIB, FRNAME is IAU_<name> even if there is not.
!
      lookup = 'IAU_' // marker_text ( cname )
      name = lookup
      call ucase ( lookup, name )
      frname = name
      associate ( fx => ctx%framex_state )
      item = 0
      if ( len(name) <= len(fx%bnmnms) ) then
         call zzhscchk ( ctx, fx%bnmlst, fx%bnmpol, fx%bnmnms, name, item )
      end if
      if ( item /= 0 ) then
         item = fx%bnmidx( item )
      end if
      if ( item > 0 ) then
         frcode = fx%idcode(item)
         found  = .true.
      end if
      end associate
      return
      end subroutine cnmfrm

      !> Return the frame name, frame ID, and center associated with
      !> a given frame class and class ID.
      !>
      !>## Detailed Input
      !>
      !>     FRCLSS   is the class or type of the frame. This identifies
      !>              which subsystem will be used to perform frame
      !>              transformations.
      !>
      !>     CLSSID   is the ID code used for the frame within its class.
      !>              This may be different from the frame ID code.
      !>
      !>## Detailed Output
      !>
      !>     FRCODE   is the frame ID code for the reference frame
      !>              identified by FRCLSS and CLSSID.
      !>
      !>     FRNAME   is the name of the frame identified by FRCLSS and
      !>              CLSSID. FRNAME should be declared as CHARACTER*(32)
      !>              to ensure that it can contain the full name of the
      !>              frame. If FRNAME does not have enough room to hold
      !>              the full name of the frame, the name will be
      !>              truncated on the right.
      !>
      !>     CENT     is the body ID code for the center of the reference
      !>              frame identified by FRCLSS and CLSSID.
      !>
      !>     FOUND    is .TRUE. if FRCODE, FRNAME, and CENT are available.
      !>              Otherwise, FOUND is returned with the value .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that the first frame found with matching
      !>         class and class ID is the correct one. SPICE's frame system
      !>         does not diagnose the situation where there are multiple,
      !>         distinct frames with matching classes and class ID codes, but
      !>         this situation could occur if such conflicting frame
      !>         specifications are loaded via one or more frame kernels. The
      !>         user is responsible for avoiding such frame specification
      !>         conflicts.
      !>
      !>     2)  If FRNAME does not have room to contain the frame name, the
      !>         name will be truncated on the right. (Declaring FRNAME to be
      !>         CHARACTER*(32) will ensure that the name will not be
      !>         truncated).
      !>
      !>     3)  If a frame class assignment is found that associates a
      !>         string (as opposed to numeric) value with a frame class
      !>         keyword, the error SPICE(INVALIDFRAMEDEF) is signaled.
      !>
      !>     4)  If a frame class assignment is found that matches the input
      !>         class, but a corresponding class ID assignment is not
      !>         found in the kernel pool, the error SPICE(INVALIDFRAMEDEF)
      !>         is signaled.
      !>
      !>     5)  If a frame specification is found in the kernel pool with
      !>         matching frame class and class ID, but either the frame name
      !>         or frame ID code are not found, the error
      !>         SPICE(INVALIDFRAMEDEF) is signaled.
      !>
      !>     6)  If a frame specification is found in the kernel pool with
      !>         matching frame class and class ID, but the frame center
      !>         is not found, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows the user to determine the frame associated
      !>     with a given frame class and class ID code. The built-in frame
      !>     list is searched first for a matching frame; if no match is
      !>     found, then the kernel POOL is searched.
      !>
      !>     Since the neither the frame class nor the class ID are primary
      !>     keys, searching for matching frames is a linear (and therefore
      !>     typically slow) process.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following code example demonstrates how to find the frame
      !>        information about a frame by its ID using FRINFO and
      !>        by its class and class ID using CCIFRM.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM CCIFRM_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               FRNLEN
      !>      PARAMETER           ( FRNLEN = 32 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FRNLEN)    FRNAME
      !>
      !>      INTEGER               CENTR1
      !>      INTEGER               CENTR2
      !>      INTEGER               CLSS
      !>      INTEGER               CLSSID
      !>      INTEGER               FRCOD1
      !>      INTEGER               FRCOD2
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Find the frame code associated with ITRF93
      !>C
      !>      FRNAME = 'ITRF93'
      !>      CALL NAMFRM ( FRNAME, FRCOD1 )
      !>
      !>C
      !>C     Get the frame information.
      !>C
      !>      CALL FRINFO ( FRCOD1, CENTR1, CLSS, CLSSID, FOUND )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>
      !>            WRITE(*,*) 'No info found for frame ', FRCOD1
      !>            STOP
      !>
      !>      END IF
      !>
      !>      WRITE(*,'(A)')    'Frame ITRF93 info:'
      !>      WRITE(*,'(A,I6)') '   Frame Code: ', FRCOD1
      !>      WRITE(*,'(A,I6)') '   Center ID : ', CENTR1
      !>      WRITE(*,'(A,I6)') '   Class     : ', CLSS
      !>      WRITE(*,'(A,I6)') '   Class ID  : ', CLSSID
      !>
      !>C
      !>C     Return the frame name, frame ID, and center associated
      !>C     with the frame CLSS and CLSSID.
      !>C
      !>      CALL CCIFRM ( CLSS,   CLSSID, FRCOD2,
      !>     .              FRNAME, CENTR2, FOUND  )
      !>
      !>      IF ( .NOT. FOUND ) THEN
      !>
      !>            WRITE(*,*) 'No info found for type ', CLSS,
      !>     .                 ' frame ', CLSSID
      !>            STOP
      !>
      !>      END IF
      !>
      !>      WRITE(*,'(A,I3,A,I6,A)') 'Type', CLSS, ' frame', CLSSID,
      !>     .                         ' info:'
      !>      WRITE(*,'(2A)')   '   Frame name: ', FRNAME
      !>      WRITE(*,'(A,I6)') '   Frame Code: ', FRCOD2
      !>      WRITE(*,'(A,I6)') '   Center ID : ', CENTR2
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Frame ITRF93 info:
      !>           Frame Code:  13000
      !>           Center ID :    399
      !>           Class     :      2
      !>           Class ID  :   3000
      !>        Type  2 frame  3000 info:
      !>           Frame name: ITRF93
      !>           Frame Code:  13000
      !>           Center ID :    399
      !>
      !>## Restrictions
      !>
      !>     1)  See item (1) in the $Exceptions section above.
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
      !>-    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example from existing fragments.
      !>
      !>        Changed the input argument name CLASS to FRCLSS for
      !>        consistency with other routines.
      !>
      !>        Updated maximum frame name length from 26 to 32 in
      !>        $Detailed_Output and $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.1, 02-FEB-2017 (BVS)
      !>
      !>        Shortened one of permuted index entries.
      !>
      !>-    SPICELIB Version 1.1.0, 08-AUG-2012 (BVS)
      !>
      !>        The routine was updated to be more efficient by using hashes
      !>        instead kernel POOL look-ups for kernel POOL frames and by
      !>        using hashes instead of ordered array searches for built-in
      !>        frames.
      !>
      !>        Bug fix: CCIFRM logic was corrected to examine the built-in
      !>        frames before looking at the kernel POOL frames.
      !>
      !>-    SPICELIB Version 1.0.0, 05-NOV-2007 (NJB)
      module subroutine ccifrm ( ctx, frclss, clssid, frcode, frname, cent, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: prefix, repmc, suffix
      use pepper_inc_ninert
      use pepper_inc_nninrt
      use pepper_inc_frmtyp, frmtyp_all => all  ! PEPPER: ALL (unused) would hide the intrinsic
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: frname  !! Name of the frame identified by FRCLSS, CLSSID.
      integer(ip), intent(out) :: frcode  !! ID code of the frame identified by FRCLSS, CLSSID.
      integer(ip), intent(out) :: cent  !! Center of the frame identified by FRCLSS, CLSSID.
      integer(ip), intent(in) :: frclss  !! Class of frame.
      integer(ip), intent(in) :: clssid  !! Class ID of frame.
      logical, intent(out) :: found  !! .TRUE. if the requested information is available.
      character(:), allocatable :: name
      call frame_of_class ( ctx, frclss, clssid, frcode, name, cent, found )
      frname = name
      return
      end subroutine ccifrm

!> Find the kernel variable that defines an item of a frame:
!> FRAME_<ID code>_<item>, or else FRAME_<name>_<item>, names of any
!> length (PEPPER; the lookup shared by the ZZDYNxxx routines).
!>
!> PEPPER: find the kernel variable that defines an item of a frame:
!> FRAME_<ID code>_<item>, or else FRAME_<name>_<item>. The lookup shared
!> by the ZZDYNxxx routines; the names have any length (SPICELIB limited
!> both forms to 32 characters, SPICE(VARNAMETOOLONG)).
      module subroutine dynamic_frame_variable ( ctx, frname, frcode, item, required, kvname, found, n, dtype, what )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! the frame name
      integer(ip), intent(in) :: frcode  !! the frame ID code
      character(*), intent(in) :: item  !! the item, e.g. RELATIVE
      logical, intent(in) :: required  !! signal SPICE(KERNELVARNOTFOUND) if neither variable exists
      character(:), allocatable, intent(out) :: kvname  !! the variable found (the name form if neither)
      logical, intent(out) :: found  !! a variable was found
      integer(ip), intent(out) :: n  !! its number of values
      character(*), intent(out) :: dtype  !! its type, 'C' or 'N'
      character(*), intent(in), optional :: what  !! what the frame is, for the message (default: parameterized dynamic frame)
      found = .false.
      n     = 0
      dtype = ' '
      kvname = 'FRAME_' // int_text ( frcode ) // '_' // marker_text ( item )
      call dtpool ( ctx, kvname, found, n, dtype )
      if ( found .or. ctx%failed() ) return
      kvname = 'FRAME_' // marker_text ( frname ) // '_' // marker_text ( item )
      call dtpool ( ctx, kvname, found, n, dtype )
      if ( found .or. ctx%failed() .or. .not. required ) return
      call ctx%setmsg ( 'At least one of the kernel variables '   // &
                    'FRAME_#_# or FRAME_#_# was expected to ' // &
                    'be present in the kernel pool but '      // &
                    'neither was found. One of these '        // &
                    'variables is needed to define the '      // &
                    '# #.  Usually '                          // &
                    'this type of problem is due to a '       // &
                    'missing keyword assignment in a frame '  // &
                    'kernel.  Another, less likely, '         // &
                    'possibility is that other errors in a '  // &
                    'frame kernel have confused the '         // &
                    'frame subsystem into wrongly '           // &
                    'deciding these variables are needed.'    )
      call ctx%errint ( '#',  frcode )
      call ctx%errch  ( '#',  item   )
      call ctx%errch  ( '#',  frname )
      call ctx%errch  ( '#',  item   )
      if ( present ( what ) ) then
         call ctx%errch ( '#', what )
      else
         call ctx%errch ( '#', 'parameterized dynamic frame' )
      end if
      call ctx%errch  ( '#',  frname )
      call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
      end subroutine dynamic_frame_variable

!> The character values of the kernel variable that defines an item
!> of a dynamic frame, at full length (PEPPER; the body of [[zzdynvac]]).
!>
!> PEPPER: the character values of the kernel variable that defines an
!> item of a dynamic frame (FRAME_<ID code>_<item>, or else
!> FRAME_<name>_<item>), at full length: the body of [[zzdynvac]].
      module subroutine dynamic_frame_strings ( ctx, frname, frcode, item, maxn, n, values )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: frname  !! the frame name
      integer(ip), intent(in) :: frcode  !! the frame ID code
      character(*), intent(in) :: item  !! the item, e.g. FROM_FRAMES
      integer(ip), intent(in) :: maxn  !! the maximum number of values
      integer(ip), intent(out) :: n  !! the number of values
      type(string), allocatable, intent(out) :: values(:)  !! the values
      character(:), allocatable :: kvname
      character(1) :: dtype
      logical :: found
      integer(ip) :: i
      allocate ( values(0) )
      n = 0
      if ( ctx%failed() ) return
      call dynamic_frame_variable ( ctx, frname, frcode, item, .true., kvname, found, n, dtype )
      if ( ctx%failed() ) return
      if ( dtype == 'N' ) then
         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have character '    // &
                       'data type but in fact has numeric '         // &
                       'data type.  Usually this type of '          // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%sigerr ( 'SPICE(BADVARIABLETYPE)'                     )
         return
      end if
      if ( n > maxn ) then
         call ctx%setmsg ( 'The kernel variable # has used to define '  // &
                       'frame # was expected to have size not '     // &
                       'exceeding # but in fact has size #. '       // &
                       'Usually this type of '                      // &
                       'problem is due to an error in a frame '     // &
                       'definition provided in a frame kernel.'     )
         call ctx%errch  ( '#',  kvname                                 )
         call ctx%errch  ( '#',  frname                                 )
         call ctx%errint ( '#',  maxn                                   )
         call ctx%errint ( '#',  n                                      )
         call ctx%sigerr ( 'SPICE(BADVARIABLESIZE)'                     )
         return
      end if
      deallocate ( values )
      allocate ( values(n) )
      do i = 1, n
         call ctx%pool_string ( kvname, i, values(i)%s, found )
         if ( .not. found ) then
            call ctx%setmsg ( 'Variable # not found after DTPOOL ' // &
                          'indicated it was present in pool.'  )
            call ctx%errch  ( '#',  kvname                         )
            call ctx%sigerr ( 'SPICE(BUG)'                         )
            return
         end if
      end do
      end subroutine dynamic_frame_strings

!> The name of a frame from its ID code, of any length (PEPPER; the
!> body of [[frmnam]], which truncates to its argument's length).
      module subroutine frame_name_of ( ctx, frcode, frname, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frcode  !! the frame ID code
      character(:), allocatable, intent(out) :: frname  !! the frame name (empty if not found)
      logical, intent(out) :: found  !! the frame is known
      integer(ip) :: item
      integer(ip) :: n
      logical :: lupdte
      frname = ''
      found  = .false.
      if ( ctx%failed() ) return
      call framex_ready ( ctx )
      if ( ctx%failed() ) return
      associate ( fx => ctx%framex_state )
!
!     A built-in frame?
!
      call zzhsichk ( ctx, fx%bidlst, fx%bidpol, fx%bidids, frcode, item )
      if ( item /= 0 ) then
         frname = trim ( fx%name( fx%bididx(item) ) )
         found  = .true.
         return
      end if
!
!     A kernel frame looked up before (by FRINFO), unless the pool
!     changed since?
!
      call zzpctrck ( ctx, fx%pulctr, lupdte )
      if ( lupdte ) then
         call fx%kreset ( )
      end if
      item = fx%kid_find ( frcode )
      if ( item /= 0 ) then
         frname = fx%kname(item)%s
         found  = .true.
         return
      end if
!
!     The name assigned by FRAME_<frcode>_NAME, if it has one value.
!
      call ctx%pool_string ( frame_variable ( frcode, 'NAME' ), 1_ip, frname, found, n )
      if ( found .and. n == 1 ) then
         frname = trim ( adjustl ( frname ) )
      else
         frname = ''
         found  = .false.
      end if
      return
      end associate
      end subroutine frame_name_of

!> The frame associated with an object, its name of any length
!> (PEPPER; the body of [[cidfrm]]).
      module subroutine frame_of_center ( ctx, cent, frcode, frname, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bschoi
      use pepper_strings, only: ucase
      use pepper_inc_ninert
      use pepper_inc_nninrt
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: cent  !! the object's ID code
      integer(ip), intent(out) :: frcode  !! the frame ID code
      character(:), allocatable, intent(out) :: frname  !! the frame name (empty if not found)
      logical, intent(out) :: found  !! a frame was found
      integer(ip), parameter :: ncount = ninert + nninrt
      character(:), allocatable :: lookup
      character(:), allocatable :: bodnam
      character(1) :: dattyp
      integer(ip) :: item
      integer(ip) :: n
      logical :: gotit
      frcode = 0
      frname = ''
      found  = .false.
      if ( ctx%failed() ) return
      call framex_ready ( ctx )
      if ( ctx%failed() ) return
!
!     A frame assigned to the object by OBJECT_<ID>_FRAME, or else by
!     OBJECT_<name>_FRAME (PEPPER: names of any length).
!
      lookup = 'OBJECT_' // int_text ( cent ) // '_FRAME'
      call dtpool ( ctx, lookup, gotit, n, dattyp )
      if ( .not. gotit ) then
         call body_name_of ( ctx, cent, bodnam, gotit )
         if ( gotit ) then
            bodnam = 'OBJECT_' // marker_text ( bodnam ) // '_FRAME'
            lookup = bodnam
            call ucase ( bodnam, lookup )
            call dtpool ( ctx, lookup, gotit, n, dattyp )
         end if
      end if
      if ( gotit ) then
         call object_frame ( ctx, lookup, dattyp, frcode, frname, found )
         return
      end if
!
!     The default body-fixed frame of the Earth is IAU_EARTH.
!
      if ( cent == 399 ) then
         frcode = 10013
         frname = 'IAU_EARTH'
         found  = .true.
         return
      end if
!
!     The built-in frame centered on the object, if any.
!
      associate ( fx => ctx%framex_state )
      item = bschoi ( cent, ncount, fx%center, fx%centrd )
      if ( item > 0 ) then
         frcode = fx%idcode(item)
         frname = trim ( fx%name(item) )
         found  = .true.
      end if
      end associate
      return
      end subroutine frame_of_center

!> The frame of a class and class ID, its name of any length
!> (PEPPER; the body of [[ccifrm]]).
      module subroutine frame_of_class ( ctx, frclss, clssid, frcode, frname, cent, found )
      use pepper_kinds, only: ip
      use pepper_inc_ninert
      use pepper_inc_nninrt
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frclss  !! the frame class
      integer(ip), intent(in) :: clssid  !! the class ID
      integer(ip), intent(out) :: frcode  !! the frame ID code
      character(:), allocatable, intent(out) :: frname  !! the frame name (empty if not found)
      integer(ip), intent(out) :: cent  !! the frame's center
      logical, intent(out) :: found  !! a frame was found
      integer(ip), parameter :: ncount = ninert + nninrt
      type(string), allocatable :: kvars(:)
      character(:), allocatable :: look2
      integer(ip) :: values(1)
      integer(ip) :: i
      integer(ip) :: n
      logical :: fnd
      frcode = 0
      frname = ''
      cent   = 0
      found  = .false.
      if ( ctx%failed() ) return
      call framex_ready ( ctx )
      if ( ctx%failed() ) return
!
!     A built-in frame?
!
      associate ( fx => ctx%framex_state )
      do i = 1, ncount
         if (       ( fx%type(i)   == frclss ) &
              .and. ( fx%typeid(i) == clssid ) ) then
            frname = trim ( fx%name(i) )
            frcode = fx%idcode(i)
            cent   = fx%center(i)
            found  = .true.
            return
         end if
      end do
      end associate
!
!     Search the kernel frames: the variables FRAME_<ID or name>_CLASS.
!
!     PEPPER: the names are fetched once (SPICELIB fetched them 100 at a
!     time with GNPOOL, each call scanning the whole pool, and restarted
!     each batch after its first name because GIPOOL overwrote the count
!     of the batch, so each name was examined up to 100 times); names of
!     any length.
!
      call pool_names ( ctx, 'FRAME_*_CLASS', kvars )
      if ( ctx%failed() ) return
      do i = 1, size ( kvars, kind=ip )
         call gipool ( ctx, kvars(i)%s, 1_ip, 1_ip, n, values, fnd )
         if ( ctx%failed() ) return
         if ( .not. fnd ) then
            call ctx%setmsg ( 'Invalid frame specification found in ' &
            //            'kernel pool: frame class keyword ' &
            //            'is # but integer class was not ' &
            //            'associated with this keyword.'       )
            call ctx%errch  ( '#',  kvars(i)%s                      )
            call ctx%sigerr ( 'SPICE(INVALIDFRAMEDEF)'              )
            return
         end if
         if ( values(1) /= frclss ) cycle
         call gipool ( ctx, kvars(i)%s // '_ID', 1_ip, 1_ip, n, values, fnd )
         if ( ctx%failed() ) return
         if ( .not. fnd ) then
            call ctx%setmsg ( 'Invalid frame specification found in ' &
            //            'kernel pool: frame class keyword ' &
            //            'is # but associated integer ' &
            //            'class ID assignment was not found.' )
            call ctx%errch  ( '#',  kvars(i)%s                     )
            call ctx%sigerr ( 'SPICE(INVALIDFRAMEDEF)'             )
            return
         end if
         if ( values(1) /= clssid ) cycle
!
!        The frame's name is FRAME_<ID or name>_NAME (PEPPER: the
!        trailing _CLASS is replaced; SPICELIB replaced the first
!        occurrence of _CLASS, which may be part of a frame name).
!
         look2 = kvars(i)%s(:len(kvars(i)%s) - 6) // '_NAME'
         call ctx%pool_string ( look2, 1_ip, frname, fnd )
         if ( .not. fnd ) then
            call ctx%setmsg ( 'Invalid frame specification found ' &
            //            'in kernel pool: frame class keyword' &
            //            ' is # but associated frame name ' &
            //            'assignment was not found.'         )
            call ctx%errch  ( '#',  kvars(i)%s                    )
            call ctx%sigerr ( 'SPICE(INVALIDFRAMEDEF)'            )
            return
         end if
         call gipool ( ctx, 'FRAME_' // trim ( adjustl ( frname ) ), 1_ip, 1_ip, n, values, fnd )
         if ( ctx%failed() ) return
         if ( .not. fnd ) then
            call ctx%setmsg ( 'Invalid frame specification found ' &
            //            'in kernel pool: frame name is ' &
            //            'is # but associated frame ID ' &
            //            'assignment was not found.'         )
            call ctx%errch  ( '#',  frname                        )
            call ctx%sigerr ( 'SPICE(INVALIDFRAMEDEF)'            )
            return
         end if
         frcode = values(1)
         call zzdynbid ( ctx, frname, frcode, 'CENTER', cent )
         if ( .not. ctx%failed() ) then
            found = .true.
         end if
         return
      end do
      return
      end subroutine frame_of_class

   !> PEPPER: allocate and initialize the FRAMEX state of a context, and
   !> load the built-in frames on first use (the start of every FRAMEX
   !> entry point).
   subroutine framex_ready ( ctx )
      use pepper_inc_ninert
      use pepper_inc_nninrt
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: ncount = ninert + nninrt
      if ( .not. allocated ( ctx%framex_state ) ) then
         allocate ( ctx%framex_state )
         call init_framex_state ( ctx%framex_state )
      end if
      associate ( fx => ctx%framex_state )
      if ( fx%first ) then
         call zzctruin ( ctx, fx%pulctr )
         call fx%kreset ( )
         call zzfdat ( ctx, ncount, size(fx%bnmlst, kind=ip), fx%name, fx%idcode, &
                       fx%center, fx%type, fx%typeid, fx%centrd, &
                       fx%bnmlst, fx%bnmpol, fx%bnmnms, fx%bnmidx, &
                       fx%bidlst, fx%bidpol, fx%bidids, fx%bididx )
         if ( ctx%failed() ) return
         fx%first = .false.
      end if
      end associate
   end subroutine framex_ready

   !> PEPPER: the ID code of a frame from its normalized name
   !> (left-justified, uppercase, any length): a built-in frame, else a
   !> kernel frame (FRAME_<name>, a single value), cached by name.
   subroutine framex_code ( ctx, pname, frcode, found )
      class(pepper_context), intent(inout) :: ctx  !! the context (FRAMEX ready)
      character(*), intent(in) :: pname  !! the normalized frame name
      integer(ip), intent(out) :: frcode  !! the frame ID code (0 if not found)
      logical, intent(out) :: found  !! the frame is known
      integer(ip) :: item
      integer(ip) :: n
      integer(ip) :: values ( 8 )
      logical :: lupdte
      frcode = 0
      found  = .false.
      associate ( fx => ctx%framex_state )
      item = 0
      if ( len_trim(pname) <= len(fx%bnmnms) ) then
         call zzhscchk ( ctx, fx%bnmlst, fx%bnmpol, fx%bnmnms, pname, item )
      end if
      if ( item /= 0 ) then
         item = fx%bnmidx( item )
      end if
      if ( item > 0 ) then
         frcode = fx%idcode(item)
         found  = .true.
         return
      end if
      call zzpctrck ( ctx, fx%pulctr, lupdte )
      if ( lupdte ) then
         call fx%kreset ( )
      end if
      item = fx%kname_find ( pname )
      if ( item /= 0 ) then
         frcode = fx%knmids(item)
         found  = .true.
         return
      end if
      call gipool ( ctx, 'FRAME_' // trim(pname), 1_ip, 8_ip, n, values, found )
      if ( ctx%failed() ) return
      if ( n == 1 .and. found ) then
         frcode = values(1)
         call fx%kname_add ( pname, item )
         if ( .not. ctx%failed() .and. item /= 0 ) then
            fx%knmids(item) = frcode
         end if
      else
         found = .false.
      end if
      end associate
   end subroutine framex_code

   !> PEPPER: the frame assigned to an object by the kernel variable
   !> LOOKUP (OBJECT_<ID or name>_FRAME): a frame name ('C') or ID code
   !> ('N'). Shared by [[cidfrm]] and [[cnmfrm]].
   subroutine object_frame ( ctx, lookup, dattyp, frcode, frname, found )
      class(pepper_context), intent(inout) :: ctx  !! the context (FRAMEX ready)
      character(*), intent(in) :: lookup  !! the kernel variable
      character(*), intent(in) :: dattyp  !! its type, 'C' or 'N'
      integer(ip), intent(out) :: frcode  !! the frame ID code
      character(:), allocatable, intent(out) :: frname  !! the frame name (empty if not found)
      logical, intent(out) :: found  !! the frame is known
      integer(ip) :: n
      integer(ip) :: values ( 1 )
      logical :: gotit
      frcode = 0
      frname = ''
      found  = .false.
      if ( dattyp == 'C' ) then
         call ctx%pool_string ( lookup, 1_ip, frname, gotit )
         call framex_code ( ctx, frname, frcode, found )
         if ( .not. found ) then
            frname = ''
         end if
      else if ( dattyp == 'N' ) then
         call gipool ( ctx, lookup, 1_ip, 1_ip, n, values, gotit )
         if ( ctx%failed() ) return
         frcode = values(1)
         call frame_name_of ( ctx, frcode, frname, found )
      end if
   end subroutine object_frame

   !> PEPPER: the length of int_text's result.
   pure integer function int_text_length ( i ) result ( n )
      use pepper_strings, only: intstr
      integer(ip), intent(in) :: i  !! the integer
      character(24) :: buf
      call intstr ( i, buf )
      n = len_trim ( buf )
   end function int_text_length

   !> PEPPER: the frame kernel variable FRAME_<frcode>_<item>.
   pure function frame_variable ( frcode, item ) result ( name )
      integer(ip), intent(in) :: frcode  !! the frame ID code
      character(*), intent(in) :: item  !! the item, e.g. NAME
      character(7 + int_text_length(frcode) + len(item)) :: name
      name = 'FRAME_' // int_text ( frcode ) // '_' // item
   end function frame_variable

   !> PEPPER: an integer as text, without blanks.
   !>
   !> The string functions here have results of a length given by a
   !> specification expression, not a deferred length: gfortran keeps
   !> the length of a deferred-length result in a static variable at the
   !> call site, which is not thread-safe (see text_length in
   !> [[pepper_instance_module]]).
   pure function int_text ( i ) result ( text )
      use pepper_strings, only: intstr
      integer(ip), intent(in) :: i  !! the integer
      character(int_text_length(i)) :: text
      character(24) :: buf
      call intstr ( i, buf )
      text = buf
   end function int_text

   !> PEPPER: a string as [[repmc]] substitutes it for a marker: without
   !> leading and trailing blanks, or a single blank if it is blank.
   pure function marker_text ( value ) result ( text )
      character(*), intent(in) :: value  !! the string
      character(max(1, len_trim(adjustl(value)))) :: text
      text = adjustl ( value )
   end function marker_text

end submodule pepper_frames
